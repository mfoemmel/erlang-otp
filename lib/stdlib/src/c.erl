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
-module(c).

%% Utilities to use from shell.

-export([help/0,lc/1,c/1,c/2,nc/1,nc/2, nl/1,l/1,i/0,ni/0,
	 lc_batch/0, lc_batch/1,
	 i/3,pid/3,m/0,m/1,
	 zi/0, bt/1, q/0,
	 erlangrc/0,erlangrc/1,bi/1, flush/0, regs/0,
	 nregs/0,pwd/0,ls/0,ls/1,cd/1,memory/1,memory/0, xm/1]).

-import(lists, [reverse/1,flatten/1,sublist/3,sort/1,keysearch/3,keysort/2,
		concat/1,max/1,min/1,foreach/2,foldl/3,flatmap/2,map/2]).
-import(io, [format/1, format/2]).

help() ->
    format("bt(Pid)    -- stack backtrace for a process\n"
	   "c(File)    -- compile and load code in <File>\n"
	   "cd(Dir)    -- change working directory\n"
	   "flush()    -- flush any messages sent to the shell\n"
	   "help()     -- help info\n"
	   "i()        -- information about the system\n"
	   "ni()       -- information about the networked system\n"
	   "i(X,Y,Z)   -- information about pid <X,Y,Z>\n"
	   "l(Module)  -- load or reload module\n"
	   "lc([File]) -- compile a list of Erlang modules\n"
	   "ls()       -- list files in the current directory\n"
	   "ls(Dir)    -- list files in directory <Dir>\n"
	   "m()        -- which modules are loaded\n"
	   "m(Mod)     -- information about module <Mod>\n"
	   "memory()   -- memory allocation information\n"
	   "memory(T)  -- memory allocation information of type <T>\n"
	   "nc(File)   -- compile and load code in <File> on all nodes\n"
	   "nl(Module) -- load module on all nodes\n"
	   "pid(X,Y,Z) -- convert X,Y,Z to a Pid\n"
	   "pwd()      -- print working directory\n"
	   "q()        -- quit - shorthand for init:stop()\n"
	   "regs()     -- information about registered processes\n"
	   "nregs()    -- information about all registered processes\n"
	   "xm(M)      -- cross reference check a module\n"
	   "zi()       -- information about the system, including zombies\n").

%% c(FileName)
%%  Compile a file/module.

c(File) -> c(File, []).

c(File, Opts0) when list(Opts0) ->
    Opts = [report_errors,report_warnings|Opts0],
    case compile:file(File, Opts) of
	{ok,Mod} ->				%Listing file.
	    machine_load(Mod, File, Opts);
	{ok,Mod,_Ws} ->				%Warnings maybe turned on.
	    machine_load(Mod, File, Opts);
	Other ->				%Errors go here
	    Other
    end;
c(File, Opt) -> 
    c(File, [Opt]).

%%% Obtain the 'outdir' option from the argument. Return "." if no
%%% such option was given.
outdir([]) ->
    ".";
outdir([Opt|Rest]) ->
    case Opt of
	{outdir, D} ->
	    D;
	_ ->
	    outdir(Rest)
    end.

%%% We have compiled File with options Opts. Find out where the
%%% output file went to, and load it.
machine_load(Mod, File, Opts) ->
    Dir = outdir(Opts),
    File2 = filename:join(Dir, filename:basename(File, ".erl")),
    case compile:output_generated(Opts) of
	true ->
	    Base = packages:last(Mod),
	    case filename:basename(File, ".erl") of
		Base ->
		    code:purge(Mod),
		    check_load(code:load_abs(File2,Mod), Mod);
		_OtherMod ->
		    format("** Module name '~p' does not match file name '~p' **~n",
			   [Mod,File]),
		    {error, badfile}
	    end;
	false ->
	    format("** Warning: No object file created - nothing loaded **~n", []),
	    ok
    end.

%%% This function previously warned if the loaded module was
%%% loaded from some other place than current directory.
%%% Now, loading from other than current directory is supposed to work.
%%% so this function does nothing special.
check_load({error, R}, _) -> {error, R};
check_load(_, X) -> {ok, X}.

%% Compile a list of modules
%% enables the nice unix shell cmd
%% erl -s c lc f1 f2 f3 @d c1=v1 @c2 @i IDir @o ODir -s erlang halt
%% to compile files f1.erl , f2.erl ....... from a unix shell
%% with constant c2 defined, c1=v1 (v1 must be a term!), include dir
%% IDir, outdir ODir.

lc(Args) ->
    case catch split(Args, [], []) of
	error -> error;
	{Opts, Files} ->
	    COpts = [report_errors, report_warnings | reverse(Opts)],
	    foreach(fun(File) -> compile:file(File, COpts) end, reverse(Files))
    end.

%%% lc_batch/1 works like lc/1, but halts afterwards, with appropriate
%%% exit code. This is meant to be called by "erl -compile".
lc_batch() ->
    io:format("Error: no files to compile~n"),
    halt(1).

lc_batch(Args) ->
    case catch split(Args, [], []) of
	error ->
	    halt(1);
	{Opts, Files} ->
	    COpts = [report_errors, report_warnings | reverse(Opts)],
	    Res = map(fun(File) -> compile:file(File, COpts) end,
		      reverse(Files)),
	    case lists:member(error, Res) of
		true ->
		    halt(1);
		false ->
		    halt(0)
	    end
    end.

split(['@i', Dir | T], Opts, Files) ->
    split(T, [{i, atom_to_list(Dir)} | Opts], Files);
split(['@o', Dir | T], Opts, Files) ->
    split(T, [{outdir, atom_to_list(Dir)} | Opts], Files);
split(['@d', Def | T], Opts, Files) ->
    split(T, [split_def(atom_to_list(Def), []) | Opts], Files);
split([File | T], Opts, Files) ->
    split(T, Opts, [File | Files]);
split([], Opts, Files) ->
    {Opts, Files}.

split_def([$= | T], Res) -> {d, list_to_atom(reverse(Res)),make_term(T)};
split_def([H | T], Res) -> split_def(T, [H | Res]);
split_def([], Res) -> {d, list_to_atom(reverse(Res))}.

make_term(Str) ->
    case erl_scan:string(Str) of
	{ok, Tokens, _} ->
	    case erl_parse:parse_term(Tokens ++ [{dot, 1}]) of
		{ok, Term} -> Term;
		{error, {_,_,Reason}} ->
		    io:format("~s: ~s~n", [Reason, Str]),
		    throw(error)
	    end;
	{error, {_,_,Reason}, _} ->
	    io:format("~s: ~s~n", [Reason, Str]),
	    throw(error)
    end.

nc(File) -> nc(File, []).

nc(File, Opts0) when list(Opts0) ->
    Opts = Opts0 ++ [report_errors, report_warnings],
    case compile:file(File, Opts) of
	{ok,Mod} ->
	    Fname = concat([File, code:objfile_extension()]),
	    case file:read_file(Fname) of
		{ok,Bin} ->
		    rpc:eval_everywhere(code,load_binary,[Mod,Fname,Bin]),
		    {ok,Mod};
		Other ->
		    Other
	    end;
	Other ->                                %Errors go here
	    Other
    end;
nc(File, Opt) when atom(Opt) -> 
    nc(File, [Opt]).

%% l(Mod)
%%  Reload module Mod from file of same name

l(Mod) ->
    code:purge(Mod),
    code:load_file(Mod).

%% Network version of l/1
nl(Mod) ->
    case code:get_object_code(Mod) of
	{_Module, Bin, Fname} ->
            rpc:eval_everywhere(code,load_binary,[Mod,Fname,Bin]);
	Other ->
	    Other
    end.

%% All except zombies.
alive_processes() ->
    lists:filter(fun palive/1, processes()).

zi() -> i(processes()).
i() -> i(alive_processes()).
ni() -> i(all_procs()).

i(Ps) ->
    Alive = lists:filter(fun palive/1, Ps),
    i1(Alive),
    %% Zombies is not the same as Ps-Alive, since the remote process that
    %% fetched Ps is included among Alive, but has exited (for ni/0).
    Zombies = lists:filter(fun pzombie/1, Ps),
    case Zombies of
	[] ->
	    ok;
	Dead ->
	    io:format("~nDead processes:~n"),
	    i1(Dead)
    end.

i1(Ps) ->
    iformat("Pid", "Initial Call", "Heap", "Reds",
	    "Msgs"),
    iformat("Registered", "Current Function", "Stack", "",
	    ""),
    {R,M,H,S} = foldl(fun display_info/2, {0,0,0,0}, Ps),
    iformat("Total", "", w(H), w(R), w(M)),
    iformat("", "", w(S), "", "").

mfa_string({M, F, A}) ->
    io_lib:format("~w:~w/~w", [M, F, A]);
mfa_string(X) ->
    w(X).

display_info(Pid, {R,M,H,S}) ->
    case pinfo(Pid) of
	undefined -> {R,M,H,S};
	Info ->
	    Call = initial_call(Info),
	    Curr = case fetch(current_function, Info) of
		       {Mod,F,Args} when list(Args) ->
			   {Mod,F,length(Args)};
		       Other ->
			   Other
		   end,
	    Reds  = fetch(reductions, Info),
	    LM = length(fetch(messages, Info)),
	    HS = fetch(heap_size, Info),
	    SS = fetch(stack_size, Info),
	    iformat(w(Pid), mfa_string(Call),
		    w(HS),
		    w(Reds), w(LM)),
	    iformat(case fetch(registered_name, Info) of
			0 -> "";
			X -> w(X)
		    end,
		    mfa_string(Curr),
		    w(SS),
		    "",
		    ""),
	    {R+Reds, M+LM, H+HS, S+SS}
    end.

%% We have to do some assumptions about the initial call.
%% If the initial call is proc_lib:init_p/5 we can find more information
%% calling the function proc_lib:initial_call/1.

initial_call(Info)  ->
    case fetch(initial_call, Info) of
	{proc_lib, init_p, 5} ->
	    proc_lib:translate_initial_call(Info);
	ICall ->
	    ICall
    end.

iformat(A1, A2, A3, A4, A5) ->
    format("~-21s ~-33s ~8s ~8s ~4s~n", [A1,A2,A3,A4,A5]).

all_procs() ->
    case is_alive() of
	true -> flatmap(fun (N) -> rpc:call(N,erlang,processes,[]) end,
			[node()|nodes()]);
	false -> processes()
    end.

%% Like is_process_alive, but works also for R4 nodes, and also for
%% remote processes.
palive(Pid) ->
    S = case is_alive() of
	    true -> rpc:call(node(Pid), erlang, process_info, [Pid, status]);
	    false -> process_info(Pid, status)
	end,
    case S of
	undefined ->
	    false;
	{status, exiting} ->
	    false;
	_ ->
	    true
    end.

pzombie(Pid) ->
    S = case is_alive() of
	    true -> rpc:call(node(Pid), erlang, process_info, [Pid, status]);
	    false -> process_info(Pid, status)
	end,
    case S of
	undefined ->
	    false;
	{status, exiting} ->
	    true;
	_ ->
	    false
    end.

pinfo(Pid) ->
    case is_alive() of
	true -> rpc:call(node(Pid), erlang, process_info, [Pid]);
	false -> process_info(Pid)
    end.

fetch(Key, Info) ->
    case keysearch(Key, 1, Info) of
	{value, {_, Val}} -> Val;
	false -> 0
    end.

pid(X,Y,Z) ->
    list_to_pid("<" ++ integer_to_list(X) ++ "." ++
		integer_to_list(Y) ++ "." ++
		integer_to_list(Z) ++ ">").

i(X,Y,Z) -> pinfo(pid(X,Y,Z)).

q() ->
    init:stop().

bt(Pid) ->
    case catch erlang:process_display(Pid, backtrace) of
	{'EXIT', _} ->
	    undefined;
	_ ->
	    ok
    end.

m() ->
    mformat("Module", "File"),
    foreach(fun ({Mod,File}) -> mformat(Mod, File) end, sort(code:all_loaded())).

mformat(A1, A2) ->
    format("~-20s  ~s\n", [A1,A2]).

%% erlangrc(Home)
%%  Try to run a ".erlang" file, first in the current directory
%%  else in home directory.

erlangrc() ->
    case init:get_argument(home) of
	{ok,[[Home]]} ->
	    erlangrc([Home]);
	_ ->
	    f_p_e(["."], ".erlang")
    end.

erlangrc([Home]) ->
    f_p_e([".",Home], ".erlang").

error(Fmt, Args) ->
    error_logger:error_msg(Fmt, Args).

f_p_e(P, F) ->
    case file:path_eval(P, F) of
	{error, enoent} ->
	    {error, enoent};
	{error, E={Line, _Mod, _Term}} ->
	    error("file:path_eval(~p,~p): error on line ~p: ~s~n",
		  [P, F, Line, file:format_error(E)]),
	    ok;
	{error, E} ->
	    error("file:path_eval(~p,~p): ~s~n",
		  [P, F, file:format_error(E)]),
	    ok;
	Other ->
	    Other
    end.

bi(I) ->
    case erlang:system_info(I) of
	X when binary(X) -> io:put_chars(binary_to_list(X));
	X when list(X) -> io:put_chars(X);
	X -> format("~w", [X])
    end.

%%
%% Short and nice form of module info
%%

m(M) ->
    L = M:module_info(),
    {value,{exports,E}} = keysearch(exports, 1, L),
    Time = get_compile_time(L),
    COpts = get_compile_options(L),
    format("Module ~w compiled: ",[M]), print_time(Time),
    format("Compiler options:  ~p~n", [COpts]),
    print_object_file(M),
    format("Exports: ~n",[]), print_exports(keysort(1, E)).

print_object_file(Mod) ->
    case code:is_loaded(Mod) of
	{file,File} ->
	    format("Object file: ~s\n", [File]);
	_ ->
	    ignore
    end.

get_compile_time(L) ->
    case get_compile_info(L, time) of
	{ok,Val} -> Val;
	error -> notime
    end.

get_compile_options(L) ->
    case get_compile_info(L, options) of
	{ok,Val} -> Val;
	error -> []
    end.

get_compile_info(L, Tag) ->
    case keysearch(compile, 1, L) of
	{value, {compile, I}} ->
	    case keysearch(Tag, 1, I) of
		{value, {Tag, Val}} -> {ok,Val};
		false -> error
	    end;
	false -> error
    end.

print_exports(X) when length(X) > 16 ->
    split_print_exports(X);
print_exports([]) -> ok;
print_exports([{F, A} |Tail]) ->
    format("         ~w/~w~n",[F, A]),
    print_exports(Tail).

split_print_exports(L) ->
    Len = length(L),
    Mid = Len div 2,
    L1 = sublist(L, 1, Mid),
    L2 = sublist(L, Mid +1, Len - Mid + 1),
    split_print_exports(L1, L2).

split_print_exports([], [{F, A}|T]) ->
    Str = " ",
    format("~-30s~w/~w~n", [Str, F, A]),
    split_print_exports([], T);
split_print_exports([{F1, A1}|T1], [{F2, A2} | T2]) ->
    Str = flatten(io_lib:format("~w/~w", [F1, A1])),
    format("~-30s~w/~w~n", [Str, F2, A2]),
    split_print_exports(T1, T2);
split_print_exports([], []) -> ok.

print_time({Year,Month,Day,Hour,Min,_Secs}) ->
    format("Date: ~s ~w ~w, ", [month(Month),Day,Year]),
    format("Time: ~.2.0w.~.2.0w~n", [Hour,Min]);
print_time(notime) ->
    format("No compile time info available~n",[]).

month(1) -> "January";
month(2) -> "February";
month(3) -> "March";
month(4) -> "April";
month(5) -> "May";
month(6) -> "June";
month(7) -> "July";
month(8) -> "August";
month(9) -> "September";
month(10) -> "October";
month(11) -> "November";
month(12) -> "December".

%% Just because we can't eval receive statements...
flush() ->
    receive
	X ->
	    format("Shell got ~p~n",[X]),
	    flush()
    after 0 ->
	    ok
    end.

%% Print formated info about all registered processes in the system
nregs() ->
    foreach(fun (N) -> print_node_regs(N) end, all_regs()).

regs() ->
    print_node_regs({node(),registered()}).

all_regs() ->
    case is_alive() of
	true -> map(fun (N) -> {N,rpc:call(N, erlang, registered, [])} end,
		    [node()|nodes()]);
	false -> [{node(),registered()}]
    end.

print_node_regs({N, List}) when list(List) ->
    format("~n** Registered procs on node ~w **~n",[N]),
    rformat("Name", "Pid", "Initial Call", "Reds", "Msgs"),
    foreach(fun (Name) -> display_name_info(N, Name) end, sort(List)).

display_name_info(Node, Name) ->
    case pwhereis(Node, Name) of
	undefined ->
	    pline(Name, undefined, undefined);
	Pid ->
	    pline(Name, pinfo(Pid), Pid)
    end.

pwhereis(Node, Name) ->
    case is_alive() of
	true -> rpc:call(Node, erlang, whereis, [Name]);
	false -> whereis(Name)
    end.

pline(Name, undefined, Pid) ->		%Process has died
    rformat(Name, Pid, "dead", 0, 0);
pline(Name, Info, Pid) ->
    Call = initial_call(Info),
    Reds  = fetch(reductions, Info),
    LM = length(fetch(messages, Info)),
    rformat(io_lib:format("~w",[Name]),
	    io_lib:format("~w",[Pid]),
	    io_lib:format("~w",[Call]),
	    integer_to_list(Reds), integer_to_list(LM)).

rformat(Name, Pid, Call, Reds, LM) ->
    format("~-21s ~-12s ~-25s ~12s ~4s~n", [Name,Pid,Call,Reds,LM]).

%% pwd()
%% cd(Directory)
%%  These are just wrappers around the file:get/set_cwd functions.

pwd() ->
    case file:get_cwd() of
	{ok, Str} ->
	    ok = io:format("~s\n", [Str]);
	{error, _} ->
	    ok = io:format("Cannot determine current directory\n")
    end.

cd(Dir) ->
    file:set_cwd(Dir),
    pwd().

%% ls()
%% ls(Directory)
%%  The strategy is to print in fixed width files.

ls() ->
    ls(".").

ls(Dir) ->
    case file:list_dir(Dir) of
	{ok, Entries} ->
	    ls_print(sort(Entries));
	{error,_E} ->
	    format("Invalid directory\n")
    end.

ls_print([]) -> ok;
ls_print(L) ->
    Width = min([max(lengths(L, [])), 40]) + 5,
    ls_print(L, Width, 0).

ls_print(X, Width, Len) when Width + Len > 80 ->
    io:nl(),
    ls_print(X, Width, 0);
ls_print([H|T], Width, Len) ->
    io:format("~-*s",[Width,H]),
    ls_print(T, Width, Len+Width);
ls_print([], _, _) ->
    io:nl().

lengths([H|T], L) -> lengths(T, [length(H)|L]);
lengths([], L)    -> L.

w(X) ->
    io_lib:write(X).



%%
%% memory/[0,1] help functions
%%

get_proc_mem() ->
    lists:foldl(fun (P, Acc) ->
			case process_info(P, memory) of
			    {memory, M} ->
				Acc + M;
			    _ ->
				Acc
			end
		end,
		erlang:system_info(global_heaps_size),
		processes()).

get_non_proc_mem([{ports, Alloc}|Rest], Acc) ->
    get_non_proc_mem(Rest, Acc+Alloc);
get_non_proc_mem([{static, Alloc}|Rest], Acc) ->
    get_non_proc_mem(Rest, Acc+Alloc);
get_non_proc_mem([{atom_space, Alloc, _Used}|Rest], Acc) ->
    get_non_proc_mem(Rest, Acc+Alloc);
get_non_proc_mem([{binary, Alloc}|Rest], Acc) ->
    get_non_proc_mem(Rest, Acc+Alloc);
get_non_proc_mem([{atom_table, Alloc}|Rest], Acc) ->
    get_non_proc_mem(Rest, Acc+Alloc);
get_non_proc_mem([{module_table, Alloc}|Rest], Acc) ->
    get_non_proc_mem(Rest, Acc+Alloc);
get_non_proc_mem([{export_table, Alloc}|Rest], Acc) ->
    get_non_proc_mem(Rest, Acc+Alloc);
get_non_proc_mem([{register_table, Alloc}|Rest], Acc) ->
    get_non_proc_mem(Rest, Acc+Alloc);
get_non_proc_mem([{loaded_code, Alloc}|Rest], Acc) ->
    get_non_proc_mem(Rest, Acc+Alloc);
get_non_proc_mem([{bif_timer, Alloc}|Rest], Acc) ->
    get_non_proc_mem(Rest, Acc+Alloc);
get_non_proc_mem([{dist_table, Alloc}|Rest], Acc) ->
    get_non_proc_mem(Rest, Acc+Alloc);
get_non_proc_mem([{node_table, Alloc}|Rest], Acc) ->
    get_non_proc_mem(Rest, Acc+Alloc);
get_non_proc_mem([{link_lh, _Alloc}|Rest], Acc) ->
    get_non_proc_mem(Rest, Acc);
get_non_proc_mem([{process_desc, Alloc, Used}|Rest], Acc) ->
    get_non_proc_mem(Rest, Acc+Alloc-Used);
get_non_proc_mem([{proc_bin_desc, Alloc, Used}|Rest], Acc) ->
    get_non_proc_mem(Rest, Acc+Alloc-Used);
get_non_proc_mem([{link_desc, Alloc, Used}|Rest], Acc) ->
    get_non_proc_mem(Rest, Acc+Alloc-Used);
get_non_proc_mem([{link_sh_desc, Alloc, Used}|Rest], Acc) ->
    get_non_proc_mem(Rest, Acc+Alloc-Used);
get_non_proc_mem([{atom_desc, Alloc, _Used}|Rest], Acc) ->
    get_non_proc_mem(Rest, Acc+Alloc);
get_non_proc_mem([{export_desc, Alloc, _Used}|Rest], Acc) ->
    get_non_proc_mem(Rest, Acc+Alloc);
get_non_proc_mem([{module_desc, Alloc, _Used}|Rest], Acc) ->
    get_non_proc_mem(Rest, Acc+Alloc);
get_non_proc_mem([{preg_desc, Alloc, _Used}|Rest], Acc) ->
    get_non_proc_mem(Rest, Acc+Alloc);
get_non_proc_mem([{plist_desc, Alloc, _Used}|Rest], Acc) ->
    get_non_proc_mem(Rest, Acc+Alloc);
get_non_proc_mem([{_Mem, _Alloc, _Used}|Rest], Acc) ->
    get_non_proc_mem(Rest, Acc);
get_non_proc_mem([{_Mem, _Alloc}|Rest], Acc) ->
    get_non_proc_mem(Rest, Acc);
get_non_proc_mem([], Acc) ->
    Acc.

get_mem(MemType, allocated, [{MemType, Alloc, _Used}|_MemList]) ->
    Alloc;
get_mem(MemType, used, [{MemType, _Alloc, Used}|_MemList]) ->
    Used;
get_mem(MemType, _, [{MemType, Alloc}|_MemList]) ->
    Alloc;
get_mem(MemType, Type, [_|MemList]) ->
    get_mem(MemType, Type, MemList);
get_mem(_, _, []) ->
    throw(badarg).

get_ets_mem() ->
    get_ets_mem(ets:all(), 0).
get_ets_mem([T|Ts], Acc) ->
    case ets:info(T, memory) of
        M when integer(M) ->
            get_ets_mem(Ts, Acc+(M*4));
        _ ->
            get_ets_mem(Ts, Acc)
    end;
get_ets_mem([], Acc) ->
    Acc.


mem(total, Proc, MemList, Ets) ->
    case catch get_mem(total, allocated, MemList) of
	Tot when integer(Tot) ->
	    Tot;
	_ ->
	    mem(processes, Proc, MemList, Ets)
		+ mem(system, Proc, MemList, Ets)
    end;
mem(processes, Proc, _MemList, _Ets) ->
    Proc;
mem(system, Proc, MemList, Ets) ->
    case catch get_mem(total, allocated, MemList) of
	Tot when integer(Tot) ->
	    Tot - mem(processes, Proc, MemList, Ets);
	_ ->
	    get_non_proc_mem(MemList, 0) + mem(ets, Proc, MemList, Ets)
    end;
mem(atom, _Proc, MemList, _Ets) ->
    get_mem(atom_space, allocated, MemList)
        + get_mem(atom_table, allocated, MemList)
        + get_mem(atom_desc, allocated, MemList);
mem(atom_used, _Proc, MemList, _Ets) ->
    get_mem(atom_space, used, MemList)
        + get_mem(atom_table, used, MemList)
        + get_mem(atom_desc, used, MemList);
mem(binary, _Proc, MemList, _Ets) ->
    get_mem(binary, allocated, MemList);
mem(code, _Proc, MemList, _Ets) ->
    get_mem(module_table, allocated, MemList)
        + get_mem(export_table, allocated, MemList)
        + get_mem(loaded_code, allocated, MemList)
        + get_mem(export_desc, allocated, MemList)
        + get_mem(module_desc, allocated, MemList);
mem(ets, _Proc, _MemList, Ets) ->
    Ets;
mem(maximum, _Proc, MemList, _Ets) ->
    case catch get_mem(maximum, allocated, MemList) of
	Max when integer(Max) ->
	    Max;
	_ ->
	    unknown
    end;
mem(_, _, _, _) ->
    badarg.

non_proc_mem_list() ->
    Ports = lists:foldl(fun (P, Acc) ->
				case erlang:port_info(P, memory) of
				    {memory, M} ->
					Acc+M;
				    _ ->
					Acc
				end
			end,
			0,
			erlang:ports()),
    [{ports, Ports} | erlang:system_info(allocated_areas)].

get_proc_mem_if_needed(total) ->            get_proc_mem();
get_proc_mem_if_needed(processes) ->        get_proc_mem();
get_proc_mem_if_needed(_) ->                0.

get_non_proc_mem_if_needed(total) ->     non_proc_mem_list();
get_non_proc_mem_if_needed(processes) -> non_proc_mem_list();
get_non_proc_mem_if_needed(system) ->    non_proc_mem_list();
get_non_proc_mem_if_needed(atom) ->      non_proc_mem_list();
get_non_proc_mem_if_needed(atom_used) -> non_proc_mem_list();
get_non_proc_mem_if_needed(binary) ->    non_proc_mem_list();
get_non_proc_mem_if_needed(code) ->      non_proc_mem_list();
get_non_proc_mem_if_needed(maximum) ->   non_proc_mem_list();
get_non_proc_mem_if_needed(_) ->         [].

get_ets_mem_if_needed(total) ->             get_ets_mem();
get_ets_mem_if_needed(system) ->            get_ets_mem();
get_ets_mem_if_needed(ets) ->               get_ets_mem();
get_ets_mem_if_needed(_) ->                 0.

get_maximum_list(Proc, MemList, Ets) ->
    case mem(maximum, Proc, MemList, Ets) of
	unknown ->
	    [];
	Max ->
	    [{maximum, Max}]
    end.


%%
%% memory information.
%%
memory() ->
    MemList = non_proc_mem_list(),
    Ets = get_ets_mem(),
    Proc = get_proc_mem(),
    [{total,        mem(total,        Proc, MemList, Ets)},
     {processes,    mem(processes,    Proc, MemList, Ets)},
     {system,       mem(system,       Proc, MemList, Ets)},
     {atom,         mem(atom,         Proc, MemList, Ets)},
     {atom_used,    mem(atom_used,    Proc, MemList, Ets)},
     {binary,       mem(binary,       Proc, MemList, Ets)},
     {code,         mem(code,         Proc, MemList, Ets)},
     {ets,          mem(ets,          Proc, MemList, Ets)}|
     get_maximum_list(                Proc, MemList, Ets)].

memory(Type) ->
    case mem(Type,
	     get_proc_mem_if_needed(Type),
	     get_non_proc_mem_if_needed(Type),
	     get_ets_mem_if_needed(Type)) of
	Result when integer(Result) ->
	    Result;
	unknown when Type == maximum ->
	    erlang:fault(badarg, [Type]);
	Error ->
	    erlang:fault(Error, [Type])
    end.


%%
%% Cross Reference Check
%% 

xm(M) ->
    xref:m(M).
