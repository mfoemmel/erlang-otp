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
%% Purpose : Erlang meta evaluator

-module(int).

%% ------------------------------------------------
%% -- Exported user functions.
%% ------------------------------------------------

-export([version/0,m/0,i/1,i/2,ni/1,ni/2,a/1,a/2,na/1,
	 na/2,n/1,nn/1,snapshot/0,file/1,continue/1,continue/3,
	 auto_attach/1,auto_attach/2,interpreted/0,clear/0,
	 xattach/1,xattach/2,xattach/3,xattach/4,stack_trace/1]).

-export([break/2,delete_break/2,no_break/0,no_break/1,
	 break_in/3,break_in/4,del_break_in/3,disable_break/2,
	 enable_break/2,test_at_break/3,action_at_break/3,all_breaks/0,
	 all_breaks/1]).

-export([get_binding/2]).

%% ------------------------------------------------
%% -- Internal exports.
%% ------------------------------------------------

-export([auto_att/3,attached/1,eval/3,refresh/0,refresh/1,
	 clear/1,attach/2,attach/3,
	 snap_procs/0,start/0,int_module/1,
	 do_load/3,do_mark/1,do_delete/1]).


-define(ATTACH_FUNCTION, {dbg_ui_trace, a_start}).

%% ------------------------------------------------

version() ->
    '1.3.1'.

%% -------------------------------------------
%% Start a new graphical monitor.
%% A monitor displays status for all processes
%% running interpreted modules.
%% -------------------------------------------

m() ->
    debugger:start().

%% -------------------------------------------
%% Add Module(s) as being interpreted.
%% The actual paths will be searched for the
%% corresponding source file(s) (Module.erl).
%% Module(s) can be given with absolute path.
%% -------------------------------------------

i(Module) ->
    i(Module,[]).

i(Module,O) when atom(Module) ->
    case lists:member($/,atom_to_list(Module)) of
	false ->
	    int_mod(Module,local,O);
	_ ->
	    int_abs(Module,local,O)
    end;
i([X|Rest],O) when number(X) ->
    case lists:member($/,[X|Rest]) of
	false ->
	    int_mod(list_to_atom([X|Rest]),local,O);
	_ ->
	    int_abs([X|Rest],local,O)
    end;
i([Module],O) ->
    i(Module,O);
i([Module|Mods],O) ->
    i(Module,O),
    i(Mods,O);
i(_,_) ->
    error.

%% -------------------------------------------
%% The corresponding functions (i/1,i/2) for 
%% distributed erlang. The loading ... will be 
%% performed at all nodes using the broadcast 
%% facility.
%% -------------------------------------------

ni(Module) -> ni(Module,[]).

ni(Module,O) when atom(Module) ->
    case lists:member($/,atom_to_list(Module)) of
	false ->
	    int_mod(Module,dist,O);
	_ ->
	    int_abs(Module,dist,O)
    end;
ni([X|Rest],O) when number(X) ->
    case lists:member($/,[X|Rest]) of
	false ->
	    int_mod(list_to_atom([X|Rest]),dist,O);
	_ ->
	    int_abs([X|Rest],dist,O)
    end;
ni([Module],O) ->
    ni(Module,O);
ni([Module|Mods],O) ->
    ni(Module,O),
    ni(Mods,O);
ni(_,_) ->
    error.

%% -------------------------------------------
%% Add Module(s) as being interpreted.
%% The file(s) with absolute path is used.
%% -------------------------------------------

a(File) ->
    a(File,[]).

a(File,O) when atom(File) ->
    int_abs(File,local,O);
a([X|Rest],O) when number(X) ->
    int_abs([X|Rest],local,O);
a([File],O) ->
    a(File,O);
a([File|Files],O) ->
    a(File,O),
    a(Files,O);
a(_,_) ->
    error.

%% -------------------------------------------
%% The corresponding functions (a/1,a/2) for 
%% distributed erlang. The loading ... will be 
%% performed at all nodes using the broadcast 
%% facility.
%% -------------------------------------------

na(File) ->
    na(File,[]).

na(File,O) when atom(File) ->
    int_abs(File,dist,O);
na([X|Rest],O) when number(X) ->
    int_abs([X|Rest],dist,O);
na([File],O) ->
    na(File,O);
na([File|Files],O) ->
    na(File,O),
    na(Files,O);
na(_,_) ->
    error.

%% -------------------------------------------
%% Don't interpret module(s). The module will be
%% removed from the set of modules interpreted.
%% -------------------------------------------

n(Module) when atom(Module) ->
    del_mod(Module,local);
n([X|Rest]) when number(X) ->
    del_mod([X|Rest],local);
n([Module]) ->
    n(Module);
n([Module|Mods]) ->
    n(Module),
    n(Mods);
n(_) ->
    error.

%% -------------------------------------------
%% As above (n/1), but distributed.
%% -------------------------------------------

nn(Module) when atom(Module) ->
    del_mod(Module,dist);
nn([X|Rest]) when number(X) ->
    del_mod([X|Rest],dist);
nn([Module]) ->
    nn(Module);
nn([Module|Mods]) ->
    nn(Module),
    nn(Mods);
nn(_) ->
    error.

%% -------------------------------------------
%% Get a list of all interpreted modules.
%% -------------------------------------------

interpreted() -> code:interpreted().

%% -------------------------------------------
%% Attach to process.
%% -------------------------------------------

xattach(Pid) when pid(Pid) ->
    xattach(Pid,?ATTACH_FUNCTION).

%% -------------------------------------------
%% Order an interpreted process to continue
%% execution.
%% -------------------------------------------

continue(Pid) when pid(Pid) ->
    P = which_interpret(Pid,is_alive()),
    P ! {self(),which_meta,Pid},
    receive
	{P,which_meta_resp,Meta} when pid(Meta) ->
	    dbg_icmd:continue(Meta),
	    ok;
	{P,which_meta_resp,What} ->
	    What
    end.
    
continue(X,Y,Z) when integer(X),
                     integer(Y),
                     integer(Z) ->
    case catch c:pid(X,Y,Z) of
	Pid when pid(Pid) ->
	    continue(Pid);
	_ ->
	    {error,not_a_pid}
    end.

%% -------------------------------------------
%% Attach to process.
%% X,Y,Z is combind to a process identity.
%% -------------------------------------------

xattach(X,Y,Z) ->
    Pid = c:pid(X,Y,Z),
    xattach(Pid,?ATTACH_FUNCTION).

%% -------------------------------------------
%% Attach to process.
%% Use Fnk == {M,F} as the attaching interface.
%% -------------------------------------------

xattach(X,Y,Z,Fnk) ->
    Pid = c:pid(X,Y,Z),
    xattach(Pid,Fnk).

xattach(Pid,Fnk) when pid(Pid),
                      tuple(Fnk),
                      size(Fnk) == 2 ->
    P = which_interpret(Pid,is_alive()),
    P ! {self(),xattach,Pid,Fnk,node()},
    receive
	{P,xattach,Resp} ->
	    Resp
    end.

which_interpret(_,false) ->
    case whereis(interpret) of
	undefined ->
	    exit(no_int_serv);
	P ->
	    P
    end;
which_interpret(Pid,_) when node(Pid) == node() ->
    which_interpret(Pid,false);
which_interpret(Pid,_) ->
    case rpc:call(node(Pid),erlang,whereis,[interpret]) of
	undefined ->
	    exit(no_int_serv);
	P ->
	    P
    end.

%% -------------------------------------------
%% Set the stack trace flag.
%% Flags can be all (true), no_tail, false.
%% ist(Flag)
%% -------------------------------------------

stack_trace(Flag) ->
    case lists:member(Flag,[all,true,no_tail,false]) of
	true ->
	    stack_trace(Flag,is_alive());
	_ ->
	    exit(badarg)
    end.

stack_trace(Flag,false) ->
    dbg_icmd:stack_trace(Flag),
    true;
stack_trace(Flag,_) ->
    rpc:eval_everywhere(dbg_icmd,stack_trace,[Flag]),
    true.

%% -------------------------------------------
%% Set the automatic attachment flag.
%% Flags can be init, break and exit.
%% iaa(Flag) or ia([Flag,Flag,...])
%% -------------------------------------------

auto_attach(Auto) ->
    auto_attach(Auto,?ATTACH_FUNCTION).

%% -------------------------------------------
%% Set the automatic attachment flag.
%% Flags can be init, break and exit.
%% Use given function to start up an attachment
%% window.
%% ia(Flag,Fnk) or ia([Flag,Flag,...],Fnk)
%%   where Fnk == {M,F}
%% The given Fnk must have arity 3 or 4.
%% -------------------------------------------

auto_attach(Auto,Fnk) when atom(Auto) ->
    auto_attach([Auto],Fnk);
auto_attach(Auto,Fnk) when list(Auto),
                           tuple(Fnk),
                           size(Fnk) == 2 ->
    case is_alive() of
	true ->
	    rpc:eval_everywhere(?MODULE,auto_att,[Auto,Fnk,node()]),
	    true;
	_ ->
	    auto_att(Auto,Fnk,node())
    end.

auto_att(Auto,{Mod,Fnk},Node) ->
    IntPid = start(),
    IntPid ! {self(),auto_attach,auto(Auto,{Mod,Fnk,Node})},
    true.

auto(Auto,Fnk) ->
    case set_flags(Auto) of
	[] -> false;
	Flags -> {Flags,Fnk}
    end.

set_flags([init|F])  -> [init|set_flags(F)];
set_flags([break|F]) -> [break|set_flags(F)];
set_flags([exit|F])  -> [exit|set_flags(F)];
set_flags([_|F])     -> set_flags(F);
set_flags(_)         -> [].



%% ----------------------------------------------
%% Request information about all interpreted
%% processes. Returns a list of Pid tuples:
%% [{Pid,InitFunc,Status,Info},...]
%% ----------------------------------------------

snapshot() ->
    case is_alive() of
	false ->
	    snap_procs();
	_ ->
	    {Replies,_} = rpc:multicall(?MODULE,snap_procs,[]),
	    lists:flatten(Replies)
    end.

%% ----------------------------------------------
%% Manipulation of break points.
%% ----------------------------------------------

%% -- Set a new break point.

break(Mod, Line) when atom(Mod),integer(Line) ->
    ensure_started(),
    dbg_icmd:break(Mod, Line);
break(_,_) ->
    {error,badarg}.

%% -- Delete a break point.

delete_break(Mod, Line) when atom(Mod),integer(Line) ->
    ensure_started(),
    dbg_icmd:delete_break(Mod, Line);
delete_break(_,_) ->
    {error,badarg}.

%% -- Delete all existing break points.

no_break() ->
    ensure_started(),
    dbg_icmd:no_break().

%% -- Delete all existing break points in Mod.

no_break(Mod) when atom(Mod) ->
    ensure_started(),
    dbg_icmd:no_break(Mod);
no_break(_) ->
    {error,badarg}.

%% -- Get all current break points.
%% -- Returns: [{Break,Options},...] or []

all_breaks() ->
    ensure_started(),
    dbg_idb:get_all_breaks().

%% -- Get all current break points in Mod.
%% -- Returns: [{Break,Options},...] or []

all_breaks(Mod) when atom(Mod) ->
    ensure_started(),
    dbg_idb:get_all_breaks(Mod);
all_breaks(_) ->
    {error,badarg}.

%% -- Create break points at the first line
%% -- in every clause of Mod:Fnk/Arity.

break_in(Mod,Fnk,Arity) when atom(Mod),atom(Fnk),integer(Arity) ->
    ensure_started(),
    dbg_icmd:break_in(Mod,Fnk,Arity);
break_in(_,_,_) ->
    {error,badarg}.

%% -- Create break points at the first line
%% -- in every clause of Mod:Fnk/Arity.
%% -- And associate a conditional break at every line.

break_in(Mod,Fnk,Arity,Cond) when atom(Mod),atom(Fnk),integer(Arity) ->
    ensure_started(),
    dbg_icmd:break_in(Mod,Fnk,Arity,Cond);
break_in(_,_,_,_) ->
    {error,badarg}.

%% -- Delete break points at the first line
%% -- in every clause of Mod:Fnk/Arity.

del_break_in(Mod,Fnk,Arity) when atom(Mod),atom(Fnk),integer(Arity) ->
    ensure_started(),
    dbg_icmd:del_break_in(Mod,Fnk,Arity);
del_break_in(_,_,_) ->
    {error,badarg}.

%% -- Make an existing break point inactive.

disable_break(Mod,Line) when atom(Mod),integer(Line) ->
    ensure_started(),
    dbg_icmd:disable_break(Mod,Line);
disable_break(_,_) ->
    {error,badarg}.
    
%% -- Make an existing break point active.

enable_break(Mod,Line) when atom(Mod),integer(Line) ->
    ensure_started(),
    dbg_icmd:enable_break(Mod,Line);
enable_break(_,_) ->
    {error,badarg}.

%% -- Set which status a break point shall have
%% -- after it has been triggered the next time.
%% -- Action is: enable, disable or delete.

action_at_break(Mod,Line,Action) when atom(Mod),integer(Line) ->
    case lists:member(Action,[enable,disable,delete]) of
	true ->
	    ensure_started(),
	    dbg_icmd:action_at_break(Mod,Line,Action);
	_ ->
	    {error,badarg}
    end;
action_at_break(_,_,_) ->
    {error,badarg}.

%% -- Add a conditional function to a break point.
%% -- The given function shall have arity 1 and
%% -- return either true or false.
%% -- The argument of the given function is the
%% -- current variable bindings of the process at
%% -- the place of the break point, the bindings
%% -- can be inspected using int:get_binding/2.

%% -- Fnk == {Module,Function}
%% -- Fnk == {Module,Function,ExtraArgs}

test_at_break(Mod,Line,Fnk) when atom(Mod),integer(Line),
                                 tuple(Fnk),size(Fnk) == 2,
                                 atom(element(1,Fnk)),
                                 atom(element(2,Fnk)) ->
    ensure_started(),
    dbg_icmd:test_at_break(Mod,Line,Fnk);
test_at_break(Mod,Line,Fnk) when atom(Mod),integer(Line),
                                 tuple(Fnk),size(Fnk) == 3,
                                 atom(element(1,Fnk)),
                                 atom(element(2,Fnk)),
                                 list(element(3,Fnk)) ->
    ensure_started(),
    dbg_icmd:test_at_break(Mod,Line,Fnk);
test_at_break(_,_,_) ->
    {error,badarg}.

%% -- Get the binding of variable Var in the
%% -- binding structure.
%% -- This function shall be used from inside a
%% -- conditional break point function.
%% -- Returns: unbound or {value,Value}.

get_binding(Var, Bs) ->
    dbg_icmd:get_binding(Var, Bs).

%% ----------------------------------------------
%% ----------------------------------------------
%% Internal interface functions.
%% ----------------------------------------------
%% ----------------------------------------------

%% ----------------------------------------------
%% Inform the interpreter server that AttPid
%% has attached an interpreted process.
%% ----------------------------------------------

attached(AttPid) ->
    case whereis(interpret) of
	undefined ->
	    {error,not_interpreted};
	Pid when pid(Pid) ->
	    Pid ! {self(),{attached,AttPid}},
	    true
    end.

%% ----------------------------------------------
%% Interpret the function call Module:Func(Args).
%% If the process already is interpreted use the
%% old Meta process, otherwise start a new.
%% ----------------------------------------------

eval(Module, Func, Args) ->
    case int_p() of
	Pid when pid(Pid) ->
	    dbg_imeta:eval(Pid,Module,Func,Args);
	_ ->
	    dbg_imeta:start(Module,Func,Args)
    end.

int_p() ->
    case whereis(interpret) of
	Pid when pid(Pid) ->
	    Pid ! {self(),{interpreted,self()}},
	    receive
		{Pid,{interpreted,Res}} ->
		    Res
	    end;
	_ ->
	    start(),
	    false
    end.

%% ----------------------------------------------
%% Request information about all interpreted
%% prcesses. Our (self()) process is marked as 
%% a monitor and will continuesly receive info
%% about interpreted processes.
%% ----------------------------------------------

refresh() -> refresh1(is_alive()).

refresh1(false) -> refresh(self());
refresh1(true) -> rpc:eval_everywhere(?MODULE,refresh,[self()]).
    
refresh(Mon_Pid) ->
    case whereis(interpret) of
	Pid when pid(Pid) ->
	    Pid ! {Mon_Pid, refresh};
	_ ->
	    Pid = start(),
	    Pid ! {Mon_Pid, refresh}
    end.
    
%% ----------------------------------------------
%% Delete information about all terminated 
%% processes from the interpreter server.
%% ----------------------------------------------

clear() ->
    clear1(is_alive()),
    ok.

clear1(false) ->
    clear(self());
clear1(true) ->
    rpc:eval_everywhere(?MODULE,clear,[self()]).
    
clear(Mon_Pid) ->
    case whereis(interpret) of
	Pid when pid(Pid) ->
	    Pid ! {Mon_Pid, clear};
	_ ->
	    true
    end.
    
%% ----------------------------------------------
%% Fetch the corresponding file name of the
%% interpreted module Mod.
%% ----------------------------------------------

file(Mod) ->
    M = to_atom(Mod),
    case dbg_idb:lookup(Mod,mod_file) of
	{ok, File} ->
	    File;
	_ ->
	    {error,not_loaded}
    end.

%% ----------------------------------------------
%% Attach ourselves to Pid.
%% ----------------------------------------------

attach(Pid,Meta) ->
    Meta ! {attach,Pid,self()},
    Meta.

%% ----------------------------------------------
%% Attach ourselves to the terminated process Pid.
%% ----------------------------------------------

attach(Pid,Reason,Info) ->
    spawn_link(dbg_imeta,exit_info,[self(),Pid,Reason,Info]).

%% --------------------------------------------------
%% Fetch information about all interpreted processes.
%% --------------------------------------------------

snap_procs() ->
    case whereis(interpret) of
	undefined -> [];
	P ->
	    P ! {self(),snap},
	    receive
		{P,snap_resp,Snap} ->
		    lists:keysort(1,Snap)
	    end
    end.

%%% --------------------------------------------------------
%%% --------------------------------------------------------
%%% The interpreter server.
%%% --------------------------------------------------------
%%% --------------------------------------------------------

start() ->
    dbg_iserver_api:start().

ensure_started() ->
    start(),
    wait_for_db(100).

wait_for_db(0) ->
    exit(could_not_start_interpreter_db);
wait_for_db(N) ->
    case whereis(interpret) of
	undefined ->
	    exit(could_not_start_interpreter);
	_ ->
	    case whereis(int_db) of
		undefined ->
		    sleep(1),
		    wait_for_db(N-1);
		_ ->
		    true
	    end
    end.

sleep(T) -> receive after T -> true end.



to_atom(Mod) when atom(Mod) ->
    Mod;
to_atom(Mod) when list(Mod) ->
    list_to_atom(Mod).



%% --------------------------------------------------------
%% Miscellaneous functions.
%% --------------------------------------------------------

%% ----------------------------------------------
%% Misc. functions for the i,a,n,ni,na,nn funcs.
%% ----------------------------------------------

int_mod(Module,L,O) ->
    File = lists:concat(["./",Module]),
    int_mod(Module,File,L,O).

int_mod(Module,File,L,O) ->

    IsDistributed = is_alive(),

    maybe_load(Module,File,L,O,IsDistributed).






%% maybe_load/5 - "Compiles", and possibly loads a module into the interpreter.
%%
%% Returns: (???) Return values appear to be a bit undefined.
%%
%% {module, Module}	After an attempt to load the "compiled" binary to all
%%			nodes. The result is disregarded.
%%
%%				  Result of do_load
%%   ok					"
%%   {{module, Module}, List}        	"
%%   {error, sticky_directory}		"
%%   {error, no_interpreter}		"
%% 
%% error	Otherwise.
%%

maybe_load(Module,SrcFile,L,O,IsDistributed) ->

    case compile_module(Module,SrcFile,O) of
	
	%% Compilation succeeded, tell nodes to load result
	{ok,Module,File,Bin} when L == dist, IsDistributed == true ->
	    mark_int(Module, IsDistributed, L),
	    rpc:eval_everywhere(?MODULE,do_load,[Module,File,Bin]),
	    {module,Module};

	%% Compilation succeeded, load result
	{ok,Module,File,Bin} ->
	    mark_int(Module, IsDistributed, L),
	    do_load(Module,File,Bin);

	%% Error in "compilation"
	error ->
	    error;

	%% Unexpected results
	Other ->
	    Other
    end.

%% do_load/3 - Makes sure the interpreter server is started
%%             and then loads the "compiled" binary into it.
%%
%% Returns:
%%   ok
%%   {{module, Module}, List}
%%   {error, sticky_directory}
%%   {error, no_interpreter}


do_load(Module,File,Bin) ->
    start(),
    code:interpret_binary(Module,File,Bin).



%% mark_int/3 - Marks the specified module as interpreted in the code_server
%%              on all nodes in the distributed system.
%%
%% Returns:
%%  Whatever garbage the code:interpret/int:do_mark function returns.
%% {module, Module}		After "successful" addition of the module to the code_server.
%%  {error, W}
%% 

mark_int(Module,true,dist) ->
    case do_mark(Module) of
	{error,W} ->
	    {error,W};
	Res ->
	    rpc:eval_everywhere(?MODULE,do_mark,[Module]),
	    Res
    end;
mark_int(Module,_,_) ->
    do_mark(Module).



%% do_mark/1 - Starts the interpreter server, and notifies the code_server
%%             that the module shall be interpreted.
%%
%% Returns:
%%  Whatever garbage the code:interpret function returns.
%% {module, Module}		After "successful" addition of the module to the code_server.

do_mark(Module) ->
    start(),
    code:interpret(Module).

%% compile_module/3 - Sets up for "compilation" of the specified module.
%%
%% Returns:
%% {ok,Module, File, Binary}		If all went well
%% error		If the module couldn't be "compiled"

compile_module(Module,F,O) when atom(O) ->
    compile_module(Module,F,[O]);
compile_module(Module,F,O) ->
    Opts = lists:append(O,[report_errors,report_warnings]),
    case dbg_icompile:file(F,Opts) of
	{ok,Module,File,Bin} ->
	    {ok,Module,File,Bin};
	{ok,M,_,_} ->
	    io:format("** Modulename in objectcode differs from Modulename in argument **~n"),
	    error;
	_ ->
	    error
    end.

int_abs(File,L,O) ->
    F = to_list(File),
    File1 = case lists:suffix(".erl",F) of
		true ->
		    strip(lists:reverse(F));
		_ ->
		    F
	    end,
    Mod = to_mod(File1),
    int_mod(Mod,File1,L,O).

del_mod(Module,local) ->
    do_delete(Module);
del_mod(Module,dist) ->
    case is_alive() of
	true ->
	    rpc:eval_everywhere(?MODULE,do_delete,[Module]),
	    ok;
	_ ->
	    do_delete(Module)
    end.

do_delete(Module) ->
    start(),
    code:delete_interpret(Module).

strip([$l,$r,$e,$.|File]) ->
    lists:reverse(File);
strip(File) ->
    lists:reverse(File).

to_list(List) when list(List) -> List;
to_list(Atom) when atom(Atom) -> atom_to_list(Atom).

to_mod(File) ->
    N = string:words(File,$/),
    list_to_atom(string:sub_word(File,N,$/)).



int_module(X) ->
    dbg_iserver:int_module(X).

    
