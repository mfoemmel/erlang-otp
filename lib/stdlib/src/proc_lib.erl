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
-module(proc_lib).

%% This module is used to set some initial information
%% in each created process. 
%% Then a process terminates the Reason is checked and
%% an crash report is generated if the Reason was not 
%% expected.
%% 

-export([spawn/1,spawn_link/1, spawn/2, spawn_link/2,
         spawn/3,spawn_link/3,spawn/4,spawn_link/4,
         spawn_opt/2, spawn_opt/3, spawn_opt/4, spawn_opt/5,
	 start/3, start/4, start/5, start_link/3, start_link/4,start_link/5,
	 hibernate/3,
	 init_ack/1, init_ack/2,
	 init_p/3,init_p/5,format/1,initial_call/1,translate_initial_call/1]).

%% Internal exports.
-export([wake_up/3]).

spawn(F) when function(F) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn(proc_lib,init_p,[Parent,Ancestors,F]).

spawn(M,F,A) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn(proc_lib,init_p,[Parent,Ancestors,M,F,A]).

spawn_link(F) when function(F) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_link(proc_lib,init_p,[Parent,Ancestors,F]).

spawn_link(M,F,A) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_link(proc_lib,init_p,[Parent,Ancestors,M,F,A]).

spawn(Node, F) when function(F) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn(Node,proc_lib,init_p,[Parent,Ancestors,F]).

spawn(Node,M,F,A) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn(Node,proc_lib,init_p,[Parent,Ancestors,M,F,A]).

spawn_link(Node,F) when function(F) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_link(Node,proc_lib,init_p,[Parent,Ancestors,F]).

spawn_link(Node,M,F,A) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_link(Node,proc_lib,init_p,[Parent,Ancestors,M,F,A]).

spawn_opt(F, Opts) when function(F) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_opt(proc_lib,init_p,[Parent,Ancestors,F],Opts).

spawn_opt(Node, F, Opts) when function(F) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_opt(Node,proc_lib,init_p,[Parent,Ancestors,F],Opts).

spawn_opt(M,F,A,Opts) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_opt(proc_lib,init_p,[Parent,Ancestors,M,F,A],Opts).

spawn_opt(Node,M,F,A,Opts) ->
    Parent = get_my_name(),
    Ancestors = get_ancestors(),
    erlang:spawn_opt(Node,proc_lib,init_p,[Parent,Ancestors,M,F,A],Opts).


hibernate(M,F,A) ->
    erlang:hibernate(proc_lib, wake_up, [M,F,A]).

ensure_link(SpawnOpts) ->
    case lists:member(link, SpawnOpts) of
	true -> 
	    SpawnOpts;
	false ->
	    [link|SpawnOpts]
    end.


init_p(Parent,Ancestors,F) when function(F) ->
    put('$ancestors',[Parent|Ancestors]),
    put('$initial_call',F),
    Result = (catch F()),
    exit_p(Result,F).

init_p(Parent,Ancestors,M,F,A) ->
    put('$ancestors',[Parent|Ancestors]),
    put('$initial_call',{M,F,A}),
    Result = (catch apply(M,F,A)),
    exit_p(Result,{M,F,A}).

wake_up(M,F,A) ->
    Result = (catch apply(M,F,A)),
    exit_p(Result, {M,F,A}).

exit_p({'EXIT',Reason},StartF) ->
    crash_report(Reason,StartF),
    exit(Reason);
exit_p(Reason,_) ->
    Reason.

start(M,F,A) ->
    start(M,F,A,infinity).

start(M,F,A,Timeout) ->
    Pid = proc_lib:spawn(M,F,A),
    sync_wait(Pid, Timeout).

start(M,F,A,Timeout,SpawnOpts) ->
    Pid = proc_lib:spawn_opt(M, F, A, SpawnOpts),
    sync_wait(Pid, Timeout).

start_link(M,F,A) ->
    start_link(M,F,A,infinity).

start_link(M,F,A,Timeout) ->
    Pid = proc_lib:spawn_link(M,F,A),
    sync_wait(Pid, Timeout).

start_link(M,F,A,Timeout,SpawnOpts) ->
    Pid = proc_lib:spawn_opt(M, F, A, ensure_link(SpawnOpts)),
    sync_wait(Pid, Timeout).

sync_wait(Pid, Timeout) ->
    receive
	{ack, Pid, Return} ->
	    Return;
	{'EXIT', Pid, Reason} ->
	    {error, Reason}
    after Timeout ->
	    unlink(Pid),
	    exit(Pid, kill),
	    flush(Pid),
	    {error, timeout}
    end.

flush(Pid) ->
    receive
	{'EXIT', Pid, _} ->
	    true
    after 0 ->
	    true
    end.
    
init_ack(Parent, Return) ->
    Parent ! {ack, self(), Return}.

init_ack(Return) ->
    [Parent|_] = get('$ancestors'),
    init_ack(Parent, Return).

%% -----------------------------------------------------
%% Fetch the initial call of a proc_lib spawned process.
%% -----------------------------------------------------

initial_call({X,Y,Z})  when integer(X), integer(Y), integer(Z) ->
    initial_call(c:pid(X,Y,Z));
initial_call(Pid)  when pid(Pid) ->
    case get_process_info(Pid,dictionary) of
	{dictionary,Dict} ->
	    init_call(Dict);
	_ ->
	    false
    end;
initial_call(ProcInfo) when list(ProcInfo) ->
    case lists:keysearch(dictionary,1,ProcInfo) of
	{value,{dictionary,Dict}} ->
	    init_call(Dict);
	_ ->
	    false
    end.

init_call(Dict) ->
    case lists:keysearch('$initial_call',1,Dict) of
	{value,{_,{M,F,A}}} ->
	    {M,F,A};
	{value,{_,F}} when function(F) ->
	    F;
	_ ->
	    false
    end.

%% -----------------------------------------------------
%% Translate the '$initial_call' to some useful information.
%% However, the arguments are not returned here; only the
%% arity of the initial function.
%% This function is typically called from c:i() and c:regs().
%% -----------------------------------------------------

translate_initial_call(DictOrPid) ->
    case initial_call(DictOrPid) of
	{M,F,A} ->
	    trans_init(M,F,A);
	F when function(F) ->
	    F;
	false ->
	    {proc_lib,init_p,5}
    end.

trans_init(gen,init_it,[gen_server,_,_,supervisor,{_,Module,_},_]) ->
    {supervisor,Module,1};
trans_init(gen,init_it,[gen_server,_,_,_,supervisor,{_,Module,_},_]) ->
    {supervisor,Module,1};
trans_init(gen,init_it,[gen_server,_,_,supervisor_bridge,[Module|_],_]) ->
    {supervisor_bridge,Module,1};
trans_init(gen,init_it,[gen_server,_,_,_,supervisor_bridge,[Module|_],_]) ->
    {supervisor_bridge,Module,1};
trans_init(gen,init_it,[gen_server,_,_,Module,_,_]) ->
    {Module,init,1};
trans_init(gen,init_it,[gen_server,_,_,_,Module|_]) ->
    {Module,init,1};
trans_init(gen,init_it,[gen_fsm,_,_,Module,_,_]) ->
    {Module,init,1};
trans_init(gen,init_it,[gen_fsm,_,_,_,Module|_]) ->
    {Module,init,1};
trans_init(gen,init_it,[gen_event|_]) ->
    {gen_event,init_it,6};
trans_init(M,F,A) ->
    {M,F,length(A)}.

%% -----------------------------------------------------
%% Generate a crash report.
%% -----------------------------------------------------

crash_report(normal,_)      -> ok;
crash_report(shutdown,_)    -> ok;
crash_report(Reason,StartF) ->
    OwnReport = my_info(Reason,StartF),
    LinkReport = linked_info(self()),
    Rep = [OwnReport,LinkReport],
    error_logger:error_report(crash_report, Rep),
    Rep.

my_info(Reason,StartF) ->
    [{pid, self()},
     get_process_info(self(), registered_name),         
     {error_info, Reason}, 
     {initial_call, StartF},
     get_ancestors(self()),        
     get_process_info(self(), messages),
     get_process_info(self(), links),
     get_cleaned_dictionary(self()),
     get_process_info(self(), trap_exit),
     get_process_info(self(), status),
     get_process_info(self(), heap_size),
     get_process_info(self(), stack_size),
     get_process_info(self(), reductions)
    ].

get_ancestors(Pid) ->
    case get_dictionary(Pid,'$ancestors') of
	{'$ancestors',Ancestors} ->
	    {ancestors,Ancestors};
	_ ->
	    {ancestors,[]}
    end.

get_cleaned_dictionary(Pid) ->
    case get_process_info(Pid,dictionary) of
	{dictionary,Dict} -> {dictionary,clean_dict(Dict)};
	_                 -> {dictionary,[]}
    end.

clean_dict([E|Dict]) when element(1,E) == '$ancestors' ->
    clean_dict(Dict);
clean_dict([E|Dict]) when element(1,E) == '$initial_call' ->
    clean_dict(Dict);
clean_dict([E|Dict]) ->
    [E|clean_dict(Dict)];
clean_dict([]) ->
    [].

get_dictionary(Pid,Tag) ->
    case get_process_info(Pid,dictionary) of
	{dictionary,Dict} ->
	    case lists:keysearch(Tag,1,Dict) of
		{value,Value} -> Value;
		_             -> undefined
	    end;
	_ ->
	    undefined
    end.
	

linked_info(Pid) ->
  make_neighbour_reports1(neighbours(Pid)).
  
make_neighbour_reports1([P| Ps]) ->
  ReportBody = make_neighbour_report(P),
  %
  %  Process P might have been deleted.
  %
  case lists:member(undefined, ReportBody) of
    true ->
      make_neighbour_reports1(Ps);
    false ->
      [{neighbour, ReportBody}| make_neighbour_reports1(Ps)]
  end;
make_neighbour_reports1([]) ->
  [].
  
make_neighbour_report(Pid) ->
  [{pid, Pid},
   get_process_info(Pid, registered_name),          
   get_initial_call(Pid),
   get_process_info(Pid, current_function),
   get_ancestors(Pid),
   get_process_info(Pid, messages),
   get_process_info(Pid, links),
   get_cleaned_dictionary(Pid),
   get_process_info(Pid, trap_exit),
   get_process_info(Pid, status),
   get_process_info(Pid, heap_size),
   get_process_info(Pid, stack_size),
   get_process_info(Pid, reductions)
  ].
 
get_initial_call(Pid) ->
    case get_dictionary(Pid,'$initial_call') of
	{'$initial_call',Mfa} ->
	    {initial_call,Mfa};
	_ ->
	    get_process_info(Pid,initial_call)
    end.

%  neighbours(Pid) = list of Pids
%
%  Get the neighbours of Pid. A neighbour is a process which is 
%  linked to Pid and does not trap exit; or a neigbour of a 
%  neighbour etc.
% 
%  A breadth-first search is performed.
%

max_neighbours() -> 15.

neighbours(Pid) ->
    {_, Visited} = visit(adjacents(Pid), {max_neighbours(), [Pid]}),
    lists:delete(Pid,Visited).

%
% visit(Ps, {N, Vs}, Max) = {N0, V0s}
%
% A breadth-first search of neighbours.
%    Ps   processes,
%    Vs   visited processes,
%    N    max number to visit.
%   
visit([P| Ps], {N, Vs}) when N > 0 ->
  case lists:member(P, Vs) of
    false -> visit(adjacents(P), visit(Ps, {N-1, [P| Vs]}));
    true  -> visit(Ps, {N, Vs})
  end;
visit(_, {N, Vs}) ->
  {N, Vs}.
  
%
% adjacents(Pid) = AdjacencyList
% 
adjacents(Pid) ->
  case catch proc_info(Pid, links) of
    {links, Links} -> no_trap(Links);
    _              -> []
  end.
  
no_trap([P| Ps]) ->
  case catch proc_info(P, trap_exit) of
    {trap_exit, false} -> [P| no_trap(Ps)];
    _                  -> no_trap(Ps)
  end;
no_trap([]) ->
  [].
 
get_process_info(Pid, Tag) ->
 translate_process_info(Tag, catch proc_info(Pid, Tag)).

translate_process_info(registered_name, []) ->
  {registered_name, []};
translate_process_info(_ , {'EXIT', _}) ->
  undefined;
translate_process_info(_, Result) ->
  Result.

%%% -----------------------------------------------------------
%%% Misc. functions
%%% -----------------------------------------------------------

get_my_name() ->
    case proc_info(self(),registered_name) of
	{registered_name,Name} -> Name;
	_                      -> self()
    end.

get_ancestors() ->
    case get('$ancestors') of
	A when list(A) -> A;
	_              -> []
    end.

proc_info(Pid,Item) when node(Pid) == node() ->
    process_info(Pid,Item);
proc_info(Pid,Item) ->
    case lists:member(node(Pid),nodes()) of
	true ->
	    check(rpc:call(node(Pid), erlang, process_info, [Pid, Item]));
	_ ->
	    hidden
    end.

check({badrpc,nodedown}) -> undefined;
check({badrpc,Error})    -> Error;
check(Res)               -> Res.

%%% -----------------------------------------------------------
%%% Format (and write) a generated crash info structure.
%%% -----------------------------------------------------------

format([OwnReport,LinkReport]) ->
    OwnFormat = format_own(OwnReport),
    LinkFormat = format_link(LinkReport),
    io_lib:format("  crasher:~n~s  neighbours:~n~s",
		  [OwnFormat,LinkFormat]).

format_own(Report) ->
    format_report(Report).

format_link(Report) ->
    format_report(Report).

format_report(Rep) when list(Rep) ->
    format_rep(Rep);
format_report(Rep) ->
    io_lib:format("~p~n",[Rep]).

format_rep([{Tag,Data}|Rep]) ->
    io_lib:format("    ~p: ~80.18p~n",[Tag,Data]) ++ format_rep(Rep);
format_rep([Other|Rep]) ->
    io_lib:format("    ~p~n",[Other]) ++ format_rep(Rep);
format_rep(_) ->
    [].
