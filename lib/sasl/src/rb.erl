%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(rb).

-behaviour(gen_server).

%% External exports
-export([start/0, start/1, stop/0, rescan/0, rescan/1]).
-export([list/0, list/1, show/0, show/1, grep/1, start_log/1, stop_log/0]).
-export([h/0, help/0]).

%% Internal exports
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3,
	 handle_cast/2, handle_info/2, code_change/3]).

%%%-----------------------------------------------------------------
%%% Report Browser Tool.
%%% Formats Error reports written by log_mf_h
%%%-----------------------------------------------------------------

-record(state, {dir, data, device, max, type}).

%%-----------------------------------------------------------------
%% Interface functions.
%% For available options; see print_options().
%%-----------------------------------------------------------------
start() -> start([]).
start(Options) ->
    supervisor:start_child(sasl_sup, 
			   {rb_server, {rb, start_link, [Options]},
			    temporary, brutal_kill, worker, [rb]}).

start_link(Options) ->
    gen_server:start_link({local, rb_server}, rb, Options, []).

stop() -> 
    gen_server:call(rb_server, stop),
    supervisor:delete_child(sasl_sup, rb_server).

rescan() -> rescan([]).
rescan(Options) ->
    gen_server:call(rb_server, {rescan, Options}, infinity).

list() -> list(all).
list(Type) -> gen_server:call(rb_server, {list, Type}, infinity).

show() -> 
    gen_server:call(rb_server, show, infinity).

show(Number) when integer(Number) -> 
    gen_server:call(rb_server, {show_number, Number}, infinity);
show(Type) when atom(Type) ->
    gen_server:call(rb_server, {show_type, Type}, infinity).

grep(RegExp) -> gen_server:call(rb_server, {grep, RegExp}, infinity).

start_log(FileName) -> gen_server:call(rb_server, {start_log, FileName}).

stop_log() -> gen_server:call(rb_server, stop_log).

h() -> help().
help() ->
    io:format("~nReport Browser Tool - usage~n"),
    io:format("===========================~n"),
    io:format("rb:start()         - start the rb_server with default options~n"),
    io:format("rb:start(Options)  - where Options is a list of:~n"),
    print_options(),
    io:format("rb:h()             - print this help~n"),
    io:format("rb:help()          - print this help~n"),
    io:format("rb:list()          - list all reports~n"),
    io:format("rb:list(Type)      - list all reports of type Type~n"),
    io:format("      currently supported types are:~n"),
    print_types(),
    io:format("rb:grep(RegExp)    - print reports containing RegExp~n"),
    io:format("rb:rescan()        - rescans the report directory with same~n"),
    io:format("                     options.~n"),
    io:format("rb:rescan(Options) - rescans the report directory with new~n"),
    io:format("                     options. Options is same as in start/1.~n"),
    io:format("rb:show(Number)    - print report no Number~n"),
    io:format("rb:show(Type)      - print all reports of type Type~n"),
    io:format("rb:show()          - print all reports~n"),
    io:format("rb:start_log(File) - redirect all reports to file~n"),
    io:format("rb:stop_log()      - close the log file and redirect to~n"),
    io:format("                     standard_io~n"),
    io:format("rb:stop            - stop the rb_server~n").

%%-----------------------------------------------------------------
%% Internal functions.
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% MAKE SURE THESE TWO FUNCTIONS ARE UPDATED!
%%-----------------------------------------------------------------
print_options() ->
    io:format("      {start_log, FileName}~n"),
    io:format("         - default: standard_io~n"),
    io:format("      {max, MaxNoOfReports}~n"),
    io:format("         - MaxNoOfReports should be an integer or 'all'~n"),
    io:format("         - default: all~n"),
    io:format("      {report_dir, DirString}~n"),
    io:format("         - DirString should be a string without trailing~n"),
    io:format("         - directory delimiter.~n"),
    io:format("         - default: {sasl, error_logger_mf_dir}~n"),
    io:format("      {type, ReportType}~n"),
    io:format("         - ReportType should be a supported type, 'all'~n"),
    io:format("         - or a list of supported types~n"),
    io:format("         - default: all~n").

print_types() ->
    io:format("         - crash_report~n"),
    io:format("         - supervisor_report~n"),
    io:format("         - progress~n"),
    io:format("         - error~n").

	
init(Options) ->
    process_flag(priority, low),
    process_flag(trap_exit, true),
    Log = get_option(Options, start_log, standard_io),
    Device = open_log_file(Log),
    Dir = get_report_dir(Options),
    Max = get_option(Options, max, all),
    Type = get_option(Options, type, all),
    Data = scan_files(Dir ++ "/", Max, Type),
    {ok, #state{dir = Dir ++ "/", data = Data, device = Device,
		max = Max, type = Type}}.

%% (All those 'catch' are probably unnecessary now that we catch the
%% formatting in read_rep/4.)
handle_call({rescan, Options}, _From, State) ->
    Device = 
	case get_option(Options, start_log, {undefined}) of
	    {undefined} -> State#state.device;
	    Log ->
		close_device(State#state.device),
		open_log_file(Log)
	end,
    Max = get_option(Options, max, State#state.max),
    Type = get_option(Options, type, State#state.type),
    Data = scan_files(State#state.dir, Max, Type),
    NewState = State#state{data = Data, max = Max, type = Type,
			   device = Device},
    {reply, ok, NewState};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_, _From, #state{data = undefined}) ->
    {reply, {error, no_data}, #state{}};
handle_call({list, Type}, _From, State) ->
    print_list(State#state.data, Type),
    {reply, ok, State};
handle_call({start_log, FileName}, _From, State) ->
    NewDevice = open_log_file(FileName),
    {reply, ok, State#state{device = NewDevice}};
handle_call(stop_log, _From, State) ->
    close_device(State#state.device),
    {reply, ok, State#state{device = standard_io}};
handle_call({show_number, Number}, _From, State) ->
    #state{dir = Dir, data = Data, device = Device} = State,
    catch print_report(Dir, Data, Number, Device),
    {reply, ok, State};
handle_call({show_type, Type}, _From, State) ->
    #state{dir = Dir, data = Data, device = Device} = State,
    catch print_typed_reports(Dir, Data, Type, Device),
    {reply, ok, State};
handle_call(show, _From, State) ->
    #state{dir = Dir, data = Data, device = Device} = State,
    catch print_all_reports(Dir, Data, Device),
    {reply, ok, State};
handle_call({grep, RegExp}, _From, State) ->
    #state{dir = Dir, data = Data, device = Device} = State,
    catch print_grep_reports(Dir, Data, RegExp, Device),
    {reply, ok, State}.

terminate(_Reason, #state{device = Device}) ->
    close_device(Device).

handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------
%% Func: open_log_file/1
%% Args: FileName | standard_io
%% Returns: A Device for later use in call to io:format
%%-----------------------------------------------------------------
open_log_file(standard_io) -> standard_io;
open_log_file(FileName) ->
    case file:open(FileName, write) of
	{ok, Fd} -> Fd;
	Error -> 
	    io:format("rb: Cannot open file '~s' (~w).~n",
		      [FileName, Error]),
	    io:format("rb: Using standard_io~n"),
	    standard_io
    end.

close_device(Fd) when pid(Fd) ->
    catch file:close(Fd);
close_device(_) -> ok.

get_option(Options, Key, Default) ->
    case lists:keysearch(Key, 1, Options) of
	{value, {_Key, Value}} -> Value;
	_ -> Default
    end.

get_report_dir(Options) ->
    case lists:keysearch(report_dir, 1, Options) of
	{value, {_Key, RptDir}} -> RptDir;
	_ ->
	    case catch application:get_env(sasl, error_logger_mf_dir) of
		{ok, Dir} -> Dir;
		_ ->
		    exit("cannot locate report directory")
	    end
    end.

%%-----------------------------------------------------------------
%% Func: scan_files(RptDir, Max, Type)
%% Args: RptDir ::= string().
%%       Max ::= integer() | all, describing how many reports
%5               to read.
%%       Type ::= atom(), describing which reports to read.
%% Purpose: Scan all report files one time, and build a list of
%%          small elements 
%% Returns: Data, where Data is a list of
%%          {Number, Type, ShortDescr, Date, Fname, FilePosition}.
%%-----------------------------------------------------------------
scan_files(RptDir, Max, Type) ->
    case file:open(RptDir ++ "/index", [raw, read]) of
	{ok, Fd} ->
	    case catch file:read(Fd, 1) of
		{ok, [LastWritten]} -> 
		    Files = make_file_list(RptDir, LastWritten),
		    scan_files(RptDir, Files, Max, Type);		
		_ -> exit("cannot read the index file")
	    end;
	_ -> exit("cannot read the index file")
    end.

make_file_list(Dir, FirstFileNo) ->
    case file:list_dir(Dir) of
	{ok, FileNames} ->
	    FileNumbers = lists:zf(fun(Name) ->
					   case catch list_to_integer(Name) of
					       Int when integer(Int) ->
						   {true, Int};
					       _ ->
						   false
					   end
				   end,
				   FileNames),
	    shift(lists:sort(FileNumbers), FirstFileNo);
	_ -> exit({bad_directory, Dir})
    end.
					  
shift(List, First) -> 
    shift(List, First, []).

shift([H | T], H, Res) ->
    [H | Res] ++ lists:reverse(T);
shift([H | T], First, Res) ->
    shift(T, First, [H | Res]);
shift([], _, Res) ->
    Res.

%%-----------------------------------------------------------------
%% Func: scan_files(Dir, Files, Max, Type)
%% Args: Files is a list of FileName.
%% Purpose: Scan the report files in the index variable.
%% Returns: {Number, Type, ShortDescr, Date, FileName, FilePosition}
%%-----------------------------------------------------------------
scan_files(Dir, Files, Max, Type) ->
    scan_files(Dir, 1, Files, [], Max, Type).
scan_files(Dir, _, [], Res, Max, Type) -> Res;
scan_files(Dir, _, Files, Res, Max, Type) when Max =< 0 -> Res;
scan_files(Dir, No, [H|T], Res, Max, Type) ->
    Data = get_report_data_from_file(Dir, No, H, Max, Type),
    Len = length(Data),
    NewMax = dec_max(Max, Len),
    NewNo = No + Len,
    NewData = Data ++ Res,
    scan_files(Dir, NewNo, T, NewData, NewMax, Type).

dec_max(all, _) -> all;
dec_max(X,Y) -> X-Y.

get_report_data_from_file(Dir, No, FileNr, Max, Type) ->	
    Fname = integer_to_list(FileNr),
    FileName = lists:concat([Dir, Fname]),
    case file:open(FileName, read) of
	{ok, Fd} when pid(Fd) -> read_reports(No, Fd, Fname, Max, Type);
	_ -> [{No, unknown, "Can't open file " ++ Fname, "???", Fname, 0}]
    end.

%%-----------------------------------------------------------------
%% Func: read_reports(No, Fd, Fname, Max, Type)
%% Purpose: Read reports from one report file.
%% Returns: A list of {No, Type, ShortDescr, Date, FileName, FilePosition}
%% Note: We have to read all reports, and then check the max-
%%       variable, because the reports are reversed on the file, and
%%       we may need the last ones.
%%-----------------------------------------------------------------
read_reports(No, Fd, Fname, Max, Type) ->
    io:format("rb: reading report..."),
    case catch read_reports(Fd, [], Type) of
	{ok, Res} -> 
	    file:close(Fd),
	    io:format("done.~n"),
	    NewRes = 
		if
		    length(Res) > Max ->
			lists:sublist(Res, 1, Max);
		    true ->
			Res
		end,
	    add_report_data(NewRes, No, Fname);
	Else ->
	    io:format("err ~p~n", [Else]),
	    [{No, unknown, "Can't read reports from file " ++ Fname,
		  "???", Fname, 0}]
    end.

%%-----------------------------------------------------------------
%% Func: add_report_data(Res, No, FName)
%% Args: Res is a list of {Type, ShortDescr, Date, FilePos}
%% Purpose: Convert a list of {Type, ShortDescr, Date, FilePos} to
%%          a list of {No, Type, ShortDescr, Date, FileName, FilePos}
%% Returns: A list of {No, Type, ShortDescr, Date, FileName, FilePos}
%%-----------------------------------------------------------------
add_report_data(Res, No, FName) ->
    add_report_data(Res, No, FName, []).
add_report_data([{Type, ShortDescr, Date, FilePos}|T], No, FName, Res) ->
    add_report_data(T, No+1, FName,
		    [{No, Type, ShortDescr, Date, FName, FilePos}|Res]);
add_report_data([], No, FName, Res) -> Res.

read_reports(Fd, Res, Type) ->
    {ok, FilePos} = file:position(Fd, cur),
    case catch read_report(Fd) of
	{ok, Report} -> 
	    RealType = get_type(Report),
	    {ShortDescr, Date} = get_short_descr(Report),
	    Rep = {RealType, ShortDescr, Date, FilePos},
	    if
		Type == all->
		    read_reports(Fd, [Rep | Res], Type);
		RealType == Type ->
		    read_reports(Fd, [Rep | Res], Type);
		list(Type) ->
		    case lists:member(RealType, Type) of
			true ->
			    read_reports(Fd, [Rep | Res], Type);
			_ ->
			    read_reports(Fd, Res, Type)
		    end;
		true ->
		    read_reports(Fd, Res, Type)
	    end;
	{error, Error} ->
	    [{unknown, Error, [], FilePos} | Res];
	eof ->
	    {ok, Res};
	{'EXIT', Reason} ->
	    [{unknown, Reason, [], FilePos} | Res]
    end.

read_report(Fd) ->
    case io:get_chars(Fd,'',2) of
        [Hi,Lo] ->
            Size = get_int16(Hi,Lo),
            case io:get_chars(Fd,'',Size) of
                eof ->
                    {error,reading};
                List ->
                    Bin = list_to_binary(List),
                    {ok, binary_to_term(Bin)}
	    end;
        eof ->
            eof
    end.
 
get_int16(Hi,Lo) ->
    ((Hi bsl 8) band 16#ff00) bor (Lo band 16#ff).


%%-----------------------------------------------------------------
%% Update these functions with the reports that should be possible
%% to browse with rb.
%%-----------------------------------------------------------------
get_type({Time, {error_report, Pid, {_, crash_report, _}}}) ->
    crash_report;
get_type({Time, {error_report, Pid, {_, supervisor_report, _}}}) ->
    supervisor_report;
get_type({Time, {info_report, Pid, {_, progress, _}}}) ->
    progress;
get_type({Time, {Type, _, _}}) -> Type;
get_type(_) -> unknown.

get_short_descr({{Date, Time}, {error_report, Pid, {_, crash_report, Rep}}}) ->
    [OwnRep | _] = Rep,
    Name = 
	case lists:keysearch(registered_name, 1, OwnRep) of
	    {value, {_Key, []}} ->
		case lists:keysearch(initial_call, 1, OwnRep) of
		    {value, {_K, {M,F,A}}} -> M;
		    _ -> Pid
		end;
	    {value, {_Key, N}} -> N;
	    _ -> Pid
	end,
    NameStr = lists:flatten(io_lib:format("~w", [Name])),
    {NameStr, date_str(Date, Time)};
get_short_descr({{Date, Time}, {error_report, Pid, {_, supervisor_report,Rep}}}) ->
    Name =
	case lists:keysearch(supervisor, 1, Rep) of
	    {value, {_Key, N}} when atom(N) -> N;
	    _ -> Pid
	end,
    NameStr = lists:flatten(io_lib:format("~w", [Name])),
    {NameStr, date_str(Date,Time)};
get_short_descr({{Date, Time}, {Type, Pid, _}}) ->
    NameStr = lists:flatten(io_lib:format("~w", [Pid])),
    {NameStr, date_str(Date,Time)};
get_short_descr(_) ->
    {"???", "???"}.
    
date_str({Y,Mo,D},{H,Mi,S}) ->
    lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w", 
				[Y,Mo,D,H,Mi,S])).




print_list(Data, Type) ->
    Header = {"No", "Type", "Process", "Date     Time"},
    Width = find_width([Header | Data], 0)+1,
    Format = lists:concat(["~4s~20s ~", Width, "s~20s~n"]),
    io:format(Format, tuple_to_list(Header)),
    io:format(Format, ["==", "====", "=======", "====     ===="]),
    print_list(Data, Type, Width).
print_list([], _, _) -> true;
print_list([H|T], Type, Width) ->
    print_one_report(H, Type, Width),
    print_list(T, Type, Width).

find_width([], Width) -> Width;
find_width([H|T], Width) ->
    Try = length(element(3, H)),
    if
	Try > Width -> find_width(T, Try);
	true -> find_width(T, Width)
    end.

print_one_report({No, RealType, ShortDescr, Date, Fname, FilePosition},
		 WantedType,
		 Width) ->
    if
	WantedType == all ->
	    print_short_descr(No, RealType, ShortDescr, Date, Width);
	WantedType == RealType ->
	    print_short_descr(No, RealType, ShortDescr, Date, Width);
	true -> ok
    end.

print_short_descr(No, Type, ShortDescr, Date, Width) ->
    Format = lists:concat(["~4w~20w ~", Width, "s~20s~n"]),
    io:format(Format, [No,
		       Type, 
		       io_lib:format("~s", [ShortDescr]),
		       Date]).

print_typed_reports(Dir, [], Type, Device) ->
    ok;
print_typed_reports(Dir, Data, Type, Device) ->
    case element(2, hd(Data)) of
	Type -> print_report(Dir, Data, element(1, hd(Data)), Device);
	_ -> ok
    end,
    print_typed_reports(Dir, tl(Data), Type, Device).    

print_all_reports(Dir, [], Device) ->
    ok;
print_all_reports(Dir, Data, Device) ->
    print_report(Dir, Data, element(1, hd(Data)), Device),
    print_all_reports(Dir, tl(Data), Device).    

print_report(Dir, Data, Number, Device) ->
    {Fname, FilePosition, Type} = find_report(Data, Number),
    FileName = lists:concat([Dir, Fname]),
    case file:open(FileName, read) of
	{ok, Fd} -> read_rep(Fd, FilePosition, Type, Device);
	_ -> io:format("rb: can't open file ~p~n", [Fname])
    end.

find_report([{No, Type, ShortDescr, Date, Fname, FilePosition}|T], No) ->
    {Fname, FilePosition, Type};
find_report([H|T], No) -> find_report(T, No);
find_report([], No) ->
    io:format("There is no report with number ~p.~n", [No]).
    
print_grep_reports(Dir, [], RegExp, Device) ->
    ok;
print_grep_reports(Dir, Data, RegExp, Device) ->
    print_grep_report(Dir, Data, element(1, hd(Data)), Device, RegExp),
    print_grep_reports(Dir, tl(Data), RegExp, Device).    

print_grep_report(Dir, Data, Number, Device, RegExp) ->
    {Fname, FilePosition, Type} = find_report(Data, Number),
    FileName = lists:concat([Dir, Fname]),
    case file:open(FileName, read) of
	{ok, Fd} when pid(Fd) -> 
	    check_rep(Fd, FilePosition, Type, Device, RegExp, Number);
	_ -> 
	    io:format("rb: can't open file ~p~n", [Fname])
    end.

check_rep(Fd, FilePosition, Type, Device, RegExp, Number) ->
    case read_rep_msg(Fd, FilePosition, Type) of
	{Date, Msg} ->
	    MsgStr = lists:flatten(io_lib:format("~p",[Msg])),
	    case regexp:match(MsgStr, RegExp) of
		{match, _, _} ->
		    io:format("Found match in report number ~w~n", [Number]),
		    rb_format_supp:print(Date, Msg, Device);
		_ -> false
	    end;
	_ ->
	    io:format("rb: Cannot read from file~n")
    end.

read_rep(Fd, FilePosition, Type, Device) ->
    case read_rep_msg(Fd, FilePosition, Type) of
	{Date, Msg} ->
	    case catch rb_format_supp:print(Date, Msg, Device) of
		{'EXIT', _} ->
		    io:format(Device, "ERROR: ~p ~p~n", [Date, Msg]);
		_ ->
		    ok
	    end;
	_ -> 
	    io:format("rb: Cannot read from file~n")
    end.
    
read_rep_msg(Fd, FilePosition, Type) ->
    file:position(Fd, {bof, FilePosition}),
    Res = 
	case catch read_report(Fd) of
	    {ok, Report} ->
		{_ShortDescr, Date} = get_short_descr(Report),
		{Date, Report};
	    _ -> error
	end,
    file:close(Fd),
    Res.
