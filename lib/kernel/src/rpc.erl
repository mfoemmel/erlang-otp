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
-module(rpc).

%% General rpc, broadcast,multicall, promise and parallel evaluator
%% facility

%% This code used to reside in net.erl, but has now been moved to
%% a searate module.

-define(NAME, rex).

-behaviour(gen_server).

-export([start/0, start_link/0, stop/0,
	 call/4, call/5,
	 block_call/4, block_call/5,
	 server_call/4,
	 cast/4,
	 abcast/2,
	 abcast/3,
	 sbcast/2,
	 sbcast/3,
	 eval_everywhere/3,
	 eval_everywhere/4,
	 multi_server_call/2,
	 multi_server_call/3,
	 multicall/3,
	 multicall/4,
	 multicall/5,
	 safe_multi_server_call/2,
	 safe_multi_server_call/3,
	 async_call/4,
	 yield/1,
	 nb_yield/2,
	 nb_yield/1,
	 parallel_eval/1,
	 pmap/3, pinfo/1, pinfo/2]).

%% gen_server exports
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,
	 terminate/2, code_change/3]).

%% Remote execution and broadcasting facility

start() -> gen_server:start({local,?NAME},?MODULE,[],[]).
start_link() -> gen_server:start_link({local,?NAME},?MODULE,[],[]).

stop() -> stop(?NAME).
stop(Rpc) ->
    gen_server:call(Rpc, stop, infinity).

init([]) ->
    process_flag(trap_exit,true),
    {ok,gb_trees:empty()}.

handle_call({call, Mod, Fun, Args, Gleader}, To, S) ->
    RpcServer = self(),
    Caller =
	%% Spawn not to block the rpc server
	spawn(
	  fun () ->
		  set_group_leader(Gleader),
		  Reply = 
		      %% in case some sucker rex'es 
		      %% something that throws
		      case catch apply(Mod, Fun, Args) of
			  {'EXIT', _} = Exit ->
			      {badrpc, Exit};
			  Result ->
			      Result
		      end,
		  RpcServer ! {self(), {reply, Reply}}
	  end),
    erlang:monitor(process, Caller),
    {noreply, gb_trees:insert(Caller, To, S)};
handle_call({block_call, Mod, Fun, Args, Gleader}, _To, S) ->
    MyGL = group_leader(),
    set_group_leader(Gleader),
    Reply = 
	case catch apply(Mod,Fun,Args) of
	    {'EXIT', _} = Exit ->
		{badrpc, Exit};
	    Other ->
		Other
	end,
    group_leader(MyGL, self()), % restore
    {reply, Reply, S};
handle_call(stop, _To, S) ->
    {stop, normal, stopped, S};
handle_call(_, _To, S) ->
    {noreply, S}.  % Ignore !



handle_cast({cast, Mod, Fun, Args, Gleader}, S) ->
	    spawn(
	      fun() ->
		      set_group_leader(Gleader),
		      apply(Mod, Fun, Args)
	      end),
	    {noreply, S};
handle_cast(_, S) ->
    {noreply, S}.  % Ignore !

handle_info({'DOWN', _, process, Caller, Reason}, S) ->
    case gb_trees:lookup(Caller, S) of
	{value, To} ->
	    receive
		{Caller, {reply, Reply}} ->
		    gen_server:reply(To, Reply)
	    after 0 ->
		    gen_server:reply(To, {badrpc, {'EXIT', Reason}})
	    end,
	    {noreply, gb_trees:delete(Caller, S)};
	none ->
	    {noreply, S}
    end;
handle_info({Caller, {reply, Reply}}, S) ->
    case gb_trees:lookup(Caller, S) of
	{value, To} ->
	    receive
		{'DOWN', _, process, Caller, _} -> 
		    gen_server:reply(To, Reply),
		    {noreply, gb_trees:delete(Caller, S)}
	    end;
	none ->
	    {noreply, S}
    end;
handle_info({From, {sbcast, Name, Msg}}, S) ->
    case catch Name ! Msg of  %% use catch to get the printout
	{'EXIT', _} ->
	    From ! {?NAME, node(), {nonexisting_name, Name}};
	_ -> 
	    From ! {?NAME, node(), node()}
    end,
    {noreply,S};
handle_info({From, {send, Name, Msg}}, S) ->
    case catch Name ! {From, Msg} of %% use catch to get the printout
	{'EXIT', _} ->
	    From ! {?NAME, node(), {nonexisting_name, Name}};
	_ ->
	    ok    %% It's up to Name to respond !!!!!
    end,
    {noreply,S};
handle_info({From, {call,Mod,Fun,Args,Gleader}}, S) ->
    %% Special for hidden C node's, uugh ...
    handle_call({call,Mod,Fun,Args,Gleader}, {From,?NAME}, S);

handle_info(_, S) ->
    {noreply,S}.

terminate(_,_S) ->
    ok.

code_change(_, S, _) ->
    {ok, S}.

%% RPC aid functions ....

set_group_leader(Gleader) when pid(Gleader) -> 
    group_leader(Gleader,self());
set_group_leader(user) -> 
    %% For example, hidden C nodes doesn't want any I/O.
    case whereis(user) of
	Pid when pid(Pid) -> group_leader(Pid,self());
	_                 -> true
    end.



%% THE rpc client interface

call(N,M,F,A) when node() == N ->  %% Optimize local call
    local_call(M,F,A);
call(N,M,F,A) ->
    do_call(N, {call,M,F,A,group_leader()}, infinity).

call(N,M,F,A,_Timeout) when node() == N ->  %% Optimize local call
    local_call(M,F,A);
call(N,M,F,A,infinity) ->
    do_call(N, {call,M,F,A,group_leader()}, infinity);
call(N,M,F,A,Timeout) when integer(Timeout), Timeout >= 0 ->
    do_call(N, {call,M,F,A,group_leader()}, Timeout).

block_call(N,M,F,A) when node() == N -> %% Optimize local call
    local_call(M,F,A);
block_call(N,M,F,A) ->
    do_call(N, {block_call,M,F,A,group_leader()}, infinity).

block_call(N,M,F,A,_Timeout) when node() == N ->  %% Optimize local call
    local_call(M,F,A);
block_call(N,M,F,A,infinity) ->
    do_call(N, {block_call,M,F,A,group_leader()}, infinity);
block_call(N,M,F,A,Timeout) when integer(Timeout), Timeout >= 0 ->
    do_call(N, {block_call,M,F,A,group_leader()}, Timeout).

local_call(M,F,A) ->
    case catch apply(M,F,A) of
	{'EXIT',_}=V -> {badrpc, V};
	Other -> Other
    end.

do_call(Node, Request, infinity) ->
    rpc_check(catch gen_server:call({?NAME,Node}, Request, infinity));
do_call(Node, Request, Timeout) ->
    Tag = make_ref(),
    Caller = self(),
    Receiver =
	spawn(
	  fun() ->
		  %% Middleman process. Should be unsensitive to regular
		  %% exit signals. The sychronization is needed in case
		  %% the receiver would exit before the caller started
		  %% the monitor.
		  process_flag(trap_exit, true),
		  Mref = erlang:monitor(process, Caller),
		  receive
		      {Caller,Tag} ->
			  Result = gen_server:call({?NAME,Node}, 
						   Request, Timeout),
			  exit({self(),Tag,Result});
		      {'DOWN',Mref,_,_,_} ->
			  %% Caller died before sending us the go-ahead.
			  %% Give up silently.
			  exit(normal)
		  end
	  end),
    Mref = erlang:monitor(process, Receiver),
    Receiver ! {Caller,Tag},
    receive
	{'DOWN',Mref,_,_,{Receiver,Tag,Result}} ->
	    rpc_check(Result);
	{'DOWN',Mref,_,_,Reason} ->
	    %% The middleman code failed. Or someone did 
	    %% exit(_, kill) on the middleman process => Reason==killed
	    rpc_check_t({'EXIT',Reason})
    end.

rpc_check_t({'EXIT', {timeout,_}}) -> {badrpc, timeout};
rpc_check_t(X) -> rpc_check(X).
	    
rpc_check({'EXIT', {{nodedown,_},_}}) -> {badrpc, nodedown};
rpc_check({'EXIT', X}) -> exit(X);
rpc_check(X) -> X.


%% This is a real handy function to be used when interacting with
%% a  server called Name at node Node, It is assumed that the server
%% Receives messages on the form {From, Request} and replies on the
%% form From ! {ReplyWrapper, Node,Reply}
%% This function makes such a server call and ensures that that
%% The entire call is packed into an atomic transaction which 
%% either succeeds or fails, i.e never hangs (unless the server itself hangs)

server_call(Node, Name, ReplyWrapper, Msg) 
  when atom(Node), atom(Name) ->
    if node() == nonode@nohost, Node /= nonode@nohost ->
	    {error, nodedown};
       true ->
	    case catch erlang:monitor(process, {Name, Node}) of
		{'EXIT', _} ->
		    %% R6 node
		    erlang:monitor_node(Node, true),
		    {Name, Node} ! {self(), Msg},
		    R = receive
			    {nodedown, Node} -> 
				{error, nodedown};
			    {ReplyWrapper, Node, Reply} -> 
				receive
				    {nodedown, Node} ->
					Reply
				after 0 ->
					Reply
				end
			end,
		    erlang:monitor_node(Node, false),
		    R;
		Ref ->
		    {Name, Node} ! {self(), Msg},
		    receive
			{'DOWN', Ref, _, _, _} ->
			    {error, nodedown};
			{ReplyWrapper, Node, Reply} ->
			    erlang:demonitor(Ref),
			    receive
				{'DOWN', Ref, _, _, _} ->
				    Reply
			    after 0 ->
				    Reply
			    end
		    end
	    end
    end.
				    

cast(Node, Mod, Fun, Args) when Node == node() ->
    catch spawn(Mod, Fun, Args),
    true;
cast(Node,Mod,Fun,Args) ->
    gen_server:cast({?NAME,Node},{cast,Mod,Fun,Args,group_leader()}),
    true.


%% Asyncronous broadcast, returns nothing, it's just send'n prey
abcast(Name, Mess) ->
    abcast([node() | nodes()], Name, Mess).
abcast([Node|Tail], Name, Mess) ->
    Dest = {Name,Node},
    case catch erlang:send(Dest, Mess, [noconnect]) of
	noconnect -> spawn(erlang, send, [Dest,Mess]);
	_ -> ok
    end,
    abcast(Tail, Name, Mess);
abcast([], _,_) -> abcast.


%% Syncronous broadcast, returns a list of the nodes which had Name
%% as a registered server. Returns {Goodnodes, Badnodes},
%% Syncronous in the sence that we know that all servers have received the
%% message when we return from the call, we can't know that they have
%% processed the message though.

sbcast(Name, Mess) ->
    sbcast([node() | nodes()], Name, Mess).
sbcast(Nodes, Name, Mess) ->
    Monitors = send_nodes(Nodes, ?NAME, {sbcast, Name, Mess}, []),
    rec_nodes(?NAME, Monitors).

eval_everywhere(Mod, Fun, Args) ->
    eval_everywhere([node() | nodes()] , Mod, Fun, Args).
eval_everywhere(Nodes, Mod, Fun, Args) ->
    gen_server:abcast(Nodes, ?NAME,
		      {cast,Mod,Fun,Args,group_leader()}).


send_nodes([Node|Tail], Name, Msg, Monitors)
  when atom(Node) ->
    Monitor = start_monitor(Node, Name),
    %% Handle non-existing names in rec_nodes.
    catch {Name, Node} ! {self(), Msg},
    send_nodes(Tail, Name, Msg, [Monitor | Monitors]);
send_nodes([_Node|Tail], Name, Msg, Monitors) ->
    %% Skip non-atom _Node
    send_nodes(Tail, Name, Msg, Monitors);
send_nodes([], _Name,  _Req, Monitors) -> 
    Monitors.

%% Starts a monitor, either the new way, or the old.
%% Assumes that the arguments are atoms.
start_monitor(Node, Name) ->
    if node() == nonode@nohost, Node /= nonode@nohost ->
	    Ref = make_ref(),
	    self() ! {'DOWN', Ref, process, {Name, Node}, noconnection},
	    {Node, Ref};
       true ->
	    case catch erlang:monitor(process, {Name, Node}) of
		{'EXIT', _} ->
		    %% Remote node is R6
		    monitor_node(Node, true),
		    Node;
		Ref when reference(Ref) ->
		    {Node, Ref}
	    end
    end.

%% Cancels a monitor started with Ref=erlang:monitor(_, _),
%% i.e return value {Node, Ref} from start_monitor/2 above.
unmonitor(Ref) when reference(Ref) ->
    erlang:demonitor(Ref),
    receive
	{'DOWN', Ref, _, _, _} ->
	    true
    after 0 ->
	    true
    end.


%% Call apply(M,F,A) on all nodes in parallel
multicall(M, F, A) -> 
    multicall(M, F, A, infinity).

multicall(Nodes, M, F, A) when list(Nodes) ->
    multicall(Nodes, M, F, A, infinity);
multicall(M, F, A, Timeout) ->
    multicall([node() | nodes()], M, F, A, Timeout).

multicall(Nodes, M, F, A, infinity)
  when list(Nodes), atom(M), atom(F), list(A) ->
    do_multicall(Nodes, M, F, A, infinity);
multicall(Nodes, M, F, A, Timeout) 
  when list(Nodes), atom(M), atom(F), list(A), integer(Timeout), Timeout >= 0 ->
    do_multicall(Nodes, M, F, A, Timeout).

do_multicall(Nodes, M, F, A, Timeout) ->
    {Rep,Bad} = gen_server:multi_call(Nodes, ?NAME, 
				      {call, M,F,A, group_leader()}, 
				      Timeout),
    {lists:map(fun({_,R}) -> R end, Rep), Bad}.


%% Send Msg to Name on all nodes, and collect the answers.
%% Return {Replies, Badnodes} where Badnodes is a list of the nodes
%% that failed during the timespan of the call.
%% This function assumes that if we send a request to a server 
%% called Name, the server will reply with a reply
%% on the form {Name, Node, Reply}, otherwise this function will
%% hang forever. 
%% It also assumes that the server receives messages on the form
%% {From, Msg} and then replies as From ! {Name, node(), Reply}
%%
%% There is no aparent order among the replies


multi_server_call(Name, Msg) ->
    multi_server_call([node() | nodes()], Name, Msg).

multi_server_call(Nodes, Name, Msg) 
  when list(Nodes), atom(Name) ->
    Monitors = send_nodes(Nodes, Name, Msg, []),
    rec_nodes(Name, Monitors).

%% Returns {Replies, Badnodes} where Badnodes is a list of the nodes
%% that either terminated during the call or that didn't have a server
%% called Name running

safe_multi_server_call(Name, Msg) ->
    safe_multi_server_call([node() | nodes()], Name, Msg).

safe_multi_server_call(Nodes, Name, Msg)
  when list(Nodes), atom(Name) ->
    Monitors = send_nodes(Nodes, ?NAME, {send, Name, Msg}, []),
    rec_nodes(Name, Monitors).


rec_nodes(Name, Nodes) -> 
    rec_nodes(Name, Nodes, [],[]).


rec_nodes(_Name, [],  Badnodes, Replies) ->
    {Replies, Badnodes};

rec_nodes(Name, [{N,R} | Tail], Badnodes, Replies) ->
    receive
	{'DOWN', R, _, _, _} ->
	    rec_nodes(Name, Tail, [N|Badnodes], Replies);
	{?NAME, N, {nonexisting_name, _}} ->  
	    %% used by safe_multi_server_call(),sbcast()
	    unmonitor(R),
	    rec_nodes(Name, Tail, [N|Badnodes], Replies);
	{Name, N, Reply} ->  %% Name is bound !!!
	    unmonitor(R),
	    rec_nodes(Name, Tail, Badnodes, [Reply|Replies])
    end;
rec_nodes(Name, [N|Tail], Badnodes, Replies) ->
    receive
	{nodedown, N} ->
	    monitor_node(N, false),
	    rec_nodes(Name, Tail, [N|Badnodes], Replies);
	{?NAME, N, {nonexisting_name, _}} ->  
	    %% used by safe_multi_server_call(),sbcast()
	    monitor_node(N, false),
	    rec_nodes(Name, Tail, [N|Badnodes], Replies);
	{Name, N, Reply} ->  %% Name is bound !!!
	    monitor_node(N, false),
	    rec_nodes(Name, Tail, Badnodes, [Reply|Replies])
    end.

%% Now for an asynchronous rpc.
%% An asyncronous version of rpc that is faster for series of
%% rpc's towards the same node. I.e. it returns immediately and 
%% it returns a Key that can be used in a subsequent yield(Key).

async_call(Node, Mod, Fun, Args) ->
    ReplyTo = self(),
    spawn(
      fun() ->
	      R = call(Node, Mod, Fun, Args),         %% proper rpc
	      ReplyTo ! {self(), {promise_reply, R}}  %% self() is key
      end).

yield(Key) when is_pid(Key) ->
    {value,R} = do_yield(Key, infinity),
    R.

nb_yield(Key, infinity=Inf) when is_pid(Key) ->
    do_yield(Key, Inf);
nb_yield(Key, Timeout) when is_pid(Key), is_integer(Timeout), Timeout >= 0 ->
    do_yield(Key, Timeout).

nb_yield(Key) when is_pid(Key) ->
    do_yield(Key, 0).

do_yield(Key, Timeout) ->
    receive
        {Key,{promise_reply,R}} ->
            {value,R}
        after Timeout ->
            timeout
    end.



%% A parallel network evaluator
%% ArgL === [{M,F,Args},........]
%% Returns a lists of the evaluations in the same order as 
%% given to ArgL

parallel_eval(ArgL) ->
    Nodes = [node() | nodes()],
    Keys = map_nodes(ArgL,Nodes,Nodes),
    lists:map(fun yield/1,Keys).

map_nodes([],_,_) -> [];
map_nodes(ArgL,[],Original) ->  
    map_nodes(ArgL,Original,Original); 
map_nodes([{M,F,A}|Tail],[Node|MoreNodes], Original) ->
    [?MODULE:async_call(Node,M,F,A) | 
     map_nodes(Tail,MoreNodes,Original)].

%% Parallel version of lists:map/3 with exactly the same 
%% arguments and return value as lists:map/3
%% Except that it calls exit/1 if a network error occurs

pmap({M,F}, As, List) ->
    check(parallel_eval(build_args(M,F,As, List, [])), []).

%% By using an ackumulator twice we get the whole thing right
build_args(M,F, As, [Arg|Tail], Ack) ->
    build_args(M,F, As, Tail, [{M,F, [Arg|As]} |Ack]);
build_args(_,_, _, [], Ack) -> Ack.

%% If one single call fails, we fail the whole computation
check([{badrpc, _}|_], _) -> exit(badrpc);
check([X|T], Ack) -> check(T, [X|Ack]);
check([], Ack) -> Ack.


%% location transparent version of process_info
pinfo(Pid) when node(Pid) == node() ->
    process_info(Pid);
pinfo(Pid) ->
    call(node(Pid), erlang, process_info, [Pid]).

pinfo(Pid, Item) when node(Pid) == node() ->
    process_info(Pid, Item);
pinfo(Pid, Item) ->
    block_call(node(Pid), erlang, process_info, [Pid, Item]).

