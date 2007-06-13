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
%% Portions created by Ericsson are Copyright 2002, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(escript).

%% Useful functionst that can be called from scripts.
-export([script_name/0]).

%% Internal API.
-export([start/0, start/1]).

-import(lists, [foldl/3,flatmap/2,member/2,reverse/1,foreach/2]).

script_name() ->
    [ScriptName|_] = init:get_plain_arguments(),
    ScriptName.

%%
%% Internal API.
%%

start() ->
    start([]).

start(EscriptOptions) ->
    try 
	[File|Args] = init:get_plain_arguments(),
	do_run(File, Args, EscriptOptions)
    catch
	throw:Str ->
	    io:format("escript: ~s\n", [Str]),
	    my_halt(127);
	_:Reason ->
	    io:format("escript: Internal error: ~p\n", [Reason]),
	    io:format("~p\n", [erlang:get_stacktrace()]),
	    my_halt(127)
    end.

%%%
%%% Internal functions follow.
%%%

do_run(File, Args, Options) ->
    {Parse,Mode} = parse_file(File),
    case Options of
	["s"] ->
	    %% Syntax check only.
	    case compile:forms(Parse, [report,strong_validation]) of
		{ok,_} ->
		    my_halt(0);
		_Other ->
		    fatal("There were compilation errors.")
	    end;
	_ ->
	    eval_module(Mode, Parse, File, Args)
    end.

eval_module(interpret, Parse, File, Args) ->
    interpret(File, Parse, Args);
eval_module(compile, Parse, _File, Args) ->
    compile(Parse, Args).

interpret(File, Parse0, Args) ->
    case erl_lint:module(Parse0) of
	{ok,Ws} ->
	    report_warnings(Ws);
	{error,Es,Ws} ->
	    report_errors(Es),
	    report_warnings(Ws),
	    fatal("There were compilation errors.")
    end,
    Parse = maybe_expand_records(Parse0),
    Dict  = parse_to_dict(Parse),
    ArgsA = erl_parse:abstract(Args, 0),
    Call = {call,0,{atom,0,main},[ArgsA]},
    try
	erl_eval:expr(Call,
		      erl_eval:new_bindings(),
		      {value,fun(I, J) ->
				     code_handler(I, J, Dict, File)
			     end}),
	my_halt(0)
    catch
	error:Reason ->
	    fatal("script failed with error reason ~P", [Reason,50]);
	throw:Reason ->
	    fatal("script failed: term '~P' was thrown, but not caught",
		  [Reason,50]);
	exit:Reason ->
	    fatal("script failed with EXIT reason '~P'", [Reason,50])
    end.

compile(Parse, Args) ->
    case compile:forms(Parse, [report]) of
	{ok,Module,BeamCode} -> 
	    erlang:load_module(Module, BeamCode),
	    run_code(Module, Args);
	_Other ->
	    fatal("There were compilation errors.")
    end.

run_code(Module, Args) ->
    try
	Module:main(Args),
	my_halt(0)
    catch
	error:Reason ->
	    fatal("script failed: ~P",
		  [{Reason,erlang:get_stacktrace()},50]);
	throw:Reason ->
	    fatal("script failed: ~P",
		  [{{nocatch,Reason},erlang:get_stacktrace()},50]);
	exit:Reason ->
	    fatal("script failed with EXIT reason '~P'", [Reason,50])
    end.

parse_to_dict(L) -> parse_to_dict(L, dict:new()).

parse_to_dict([{function,_,Name,Arity,Clauses}|T], Dict0) ->
    Dict = dict:store({local, Name,Arity}, Clauses, Dict0),
    parse_to_dict(T, Dict);
parse_to_dict([{attribute,_,import,{Mod,Funcs}}|T], Dict0) ->
    Dict = foldl(fun(I, D) ->
			 dict:store({remote,I}, Mod, D)
		 end, Dict0, Funcs),
    parse_to_dict(T, Dict);
parse_to_dict([_|T], Dict) ->
    parse_to_dict(T, Dict);
parse_to_dict([], Dict) ->
    Dict.

%% make a temporary module name

mk_mod() ->
    {I,J,K} = erlang:now(),
    Mod = list_to_atom("escript__" ++ integer_to_list(I) ++ integer_to_list(J) ++
		       integer_to_list(K)),
    {attribute,0,module, Mod}.

parse_file(File) ->
    parse_check_error(File, parse_file(File, 0, [], interpret)).

parse_file(File, Nerrs, L, Mode) ->
    {ok, P} = file:open(File, read),
    %% This is to skip the first line in the script
    io:get_line(P, ''),
    Ret = parse_loop(P, File, io:parse_erl_form(P, '', 2), Nerrs, L, Mode),
    file:close(P),
    Ret.

parse_include_lib(File, Nerrs, L0, Mode) ->
    case open_lib_dir(File) of
	{ok,P} ->
	    L = [{attribute,1,file,{File,1}}|L0],
	    Ret = parse_loop(P, File, io:parse_erl_form(P, '', 1), Nerrs, L, Mode),
	    file:close(P),
	    Ret;
	{error,bad_libdir} ->
	    io:format("Misformed -include_lib");
	{error,Reason} ->
	    io:format("Failed to open ~s: ~s\n", [File,file:format_error(Reason)]),
	    {Nerrs,L0,Mode}
    end.

open_lib_dir(File0) ->
    try
	[LibName|Rest] = filename:split(File0),
	File = filename:join([code:lib_dir(LibName)|Rest]),
	file:open(File, [read])
    catch
	_:_ ->
	    {error,bad_libdir}
    end.

parse_check_error(File, {0,L0,Mode}) ->
    L = reverse(L0),
    Code = [{attribute,0,file,{File,1}},
	    mk_mod()|case is_main_exported(L) of
			 false ->
			     [{attribute,0,export,[{main,1}]}|L];
			 true ->
			     L
		     end],
    {Code,Mode};
parse_check_error(_, _) -> fatal("There were compilation errors.").

maybe_expand_records(Code) ->
    case erase(there_are_records) of
	true -> erl_expand_records:module(Code, []);
	_ -> Code
    end.

parse_loop(_, _File, {eof,_}, Nerrs, L, Mode) ->
    {Nerrs,L,Mode};
parse_loop(P, File, {ok, Form, Ln}, Nerrs0, L0, Mode0) ->
    case Form of
	{attribute,_,mode,compile} ->
	    parse_loop(P, File, io:parse_erl_form(P,'',Ln), Nerrs0, L0, compile);
	{attribute,_,include_lib,Include} ->
	    {Nerrs,L1,Mode} = parse_include_lib(Include, Nerrs0, L0, Mode0),
	    L2 = [{attribute,Ln+1,file,{File,Ln+1}}|L1],
	    parse_loop(P, File, io:parse_erl_form(P,'',Ln), Nerrs, L2, Mode);
	{attribute,_,record,_} ->
	    put(there_are_records, true),
	    parse_loop(P, File, io:parse_erl_form(P,'',Ln), Nerrs0, [Form|L0], Mode0);
	Form ->
	    parse_loop(P, File, io:parse_erl_form(P,'',Ln), Nerrs0, [Form|L0], Mode0)
    end;
parse_loop(P, File, {error,{Ln,Mod,Args}, Ln1}, Nerrs, L, Mode) ->
    io:format("~s:~w: ~s\n",
	      [File,Ln,Mod:format_error(Args)]),
    parse_loop(P, File, io:parse_erl_form(P, '', Ln1), Nerrs+1, L, Mode).
    
code_handler(local, [file], _, File) ->
    File;
code_handler(Name, Args, Dict, File) ->
    %%io:format("code handler=~p~n",[{Name, Args}]),
    Arity = length(Args),
    case dict:find({local,Name,Arity}, Dict) of
	{ok, Cs} ->
	    LF = {value,fun(I, J) ->
				code_handler(I, J, Dict, File)
			end},
	    case erl_eval:match_clause(Cs, Args,erl_eval:new_bindings(),LF) of
		{Body, Bs} ->
		    {value, Val, _Bs1} = erl_eval:exprs(Body, Bs, LF),
		    Val;
		nomatch ->
		    erlang:error({function_clause,[{local,Name,Args}]})
	    end;
	error ->
	    case dict:find({remote,{Name,Arity}}, Dict) of
		{ok, Mod} ->
		    %% io:format("Calling:~p~n",[{Mod,Name,Args}]),
		    apply(Mod, Name, Args);
		error ->
		    io:format("Script does not export ~w/~w\n", [Name,Arity]),
		    my_halt(127)
	    end
    end.

is_main_exported([{attribute,_,export,Fs}|T]) ->
    case member({main,1}, Fs) of
	false -> is_main_exported(T);
	true -> true
    end;
is_main_exported([_|T]) -> is_main_exported(T);
is_main_exported([]) -> false.

fatal(Str) ->
    throw(Str).
				
fatal(Format, Args) ->
    throw(io_lib:format(Format, Args)).

report_errors(Errors) ->
    foreach(fun ({{F,_L},Eds}) -> list_errors(F, Eds);
		({F,Eds}) -> list_errors(F, Eds) end,
	    Errors).

list_errors(F, [{Line,Mod,E}|Es]) ->
    io:fwrite("~s:~w: ~s\n", [F,Line,Mod:format_error(E)]),
    list_errors(F, Es);
list_errors(F, [{Mod,E}|Es]) ->
    io:fwrite("~s: ~s\n", [F,Mod:format_error(E)]),
    list_errors(F, Es);
list_errors(_F, []) -> ok.

report_warnings(Ws0) ->
    Ws1 = flatmap(fun({{F,_L},Eds}) -> format_message(F, Eds);
		     ({F,Eds}) -> format_message(F, Eds) end,
		  Ws0),
    Ws = ordsets:from_list(Ws1),
    foreach(fun({_,Str}) -> io:put_chars(Str) end, Ws).

format_message(F, [{Line,Mod,E}|Es]) ->
    M = {{F,Line},io_lib:format("~s:~w: Warning: ~s\n", [F,Line,Mod:format_error(E)])},
    [M|format_message(F, Es)];
format_message(F, [{Mod,E}|Es]) ->
    M = {none,io_lib:format("~s: Warning: ~s\n", [F,Mod:format_error(E)])},
    [M|format_message(F, Es)];
format_message(_, []) -> [].

my_halt(Reason) ->
    case process_info(group_leader(), status) of
	{_,waiting} ->
	    %% Now all output data is down in the driver.
	    %% Give the driver some extra time before halting.
	    receive after 1 -> ok end,
	    halt(Reason);
	_ ->
	    %% Probably still processing I/O requests.
	    erlang:yield(),
	    my_halt(Reason)
    end.
