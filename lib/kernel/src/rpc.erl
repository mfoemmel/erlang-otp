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


-behaviour(gen_server).

-export([start/0, start_link/0, stop/0,
	 call/4,         
	 block_call/4,   
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
	 safe_multi_server_call/2,
	 safe_multi_server_call/3,
	 async_call/4,
	 yield/1,
	 nb_yield/1,
	 parallel_eval/1, 
	 pmap/3, pinfo/1, pinfo/2]).

%% Internal exports ...
-export([reply/5, 
	 caller/4,
	 do_cs_call/5]).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,
	 terminate/2]).

%% Remote execution and broadcasting facility

start() -> gen_server:start({local,rex},rpc,[],[]).
start_link() -> gen_server:start_link({local,rex},rpc,[],[]).

stop() -> stop(rex).
stop(Rpc) ->
    gen_server:call(Rpc, stop, infinity).

init([]) ->
    process_flag(trap_exit,true),
    {ok,[]}.

handle_call(In,{From,Tag},S) ->
    case In of
	{call,Mod,Fun,Args,Gleader} ->
            %% in case some sucker rex'es something that hangs
            Id = spawn(rpc,reply,[{From,Tag},Mod,Fun,Args, Gleader]),
	    {noreply,S};
	{block_call, M,F,A,Gl} ->
	    MyGL = group_leader(),
	    reply({From,Tag},M,F,A,Gl),
	    group_leader(MyGL,self()), % reset
	    {noreply,S};
	stop ->
	    {stop,normal,stopped,S};
	_ ->
	    {noreply,S}  % Ignore !
    end.

handle_cast(In,S) ->
    case In of
	{cast,Mod,Fun,Args,Gleader} ->
            spawn(rpc,caller,[Gleader,Mod,Fun,Args]),
	    {noreply,S};
	_ ->
	    {noreply,S}  % Ignore !
    end.

handle_info({From, {sbcast, Name, Msg}}, S) ->
    case catch Name ! Msg of  %% use catch to get the printout
	{'EXIT', _} ->
	    From ! {rex, node(), {nonexisting_name, Name}};
	_ -> 
	    From ! {rex, node(), node()}
    end,
    {noreply,S};
handle_info({From, {send, Name, Msg}}, S) ->
    case catch Name ! {From, Msg} of %% use catch to get the printout
	{'EXIT', _} ->
	    From ! {rex, node(), {nonexisting_name, Name}};
	_ ->
	    ok    %% It's up to Name to respond !!!!!
    end,
    {noreply,S};
handle_info({From, {call,Mod,Fun,Args,Gleader}}, S) ->
    %% Special for hidden C node's, uugh ...
    handle_call({call,Mod,Fun,Args,Gleader}, {From,rex}, S);

handle_info(_, S) ->
    {noreply,S}.

terminate(_,_) ->
    ok.

%% RPC aid functions ....

reply(To,Mod,Fun,Args,Gleader) ->
    set_group_leader(Gleader),
    case catch apply(Mod,Fun,Args) of
	{'EXIT', R} ->
	    gen_server:reply(To,{badrpc, {'EXIT', R}});
	Other ->
	    gen_server:reply(To,Other)
    end.

set_group_leader(Gleader) when pid(Gleader) -> 
    group_leader(Gleader,self());
set_group_leader(user) -> 
    %% For example, hidden C nodes doesn't want any I/O.
    case whereis(user) of
	Pid when pid(Pid) -> group_leader(Pid,self());
	_                 -> true
    end.

caller(Gleader,M,F,A) -> 
    group_leader(Gleader,self()),
    apply(M,F,A).



%% THE rpc client interface

call(N,M,F,A) when node() == N ->  %% Optimize local call
    case V = (catch apply(M,F,A)) of
	{'EXIT', R} -> {badrpc, V};
	Other -> Other
    end;
call(N,M,F,A) ->
    rpc_check(catch gen_server:call({rex,N},
				    {call,M,F,A,group_leader()},
				    infinity)).

block_call(N,M,F,A) when node() == N -> %% Optimize local call
    case V = (catch apply(M,F,A)) of
	{'EXIT', R} -> {badrpc, V};
	Other -> Other
    end;
block_call(N,M,F,A) ->
    rpc_check(catch gen_server:call({rex,N},
				    {block_call,M,F,A,group_leader()},
				    infinity)).

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

server_call(Node, Name, ReplyWrapper, Msg) ->
    erlang:monitor_node(Node, true),
    {Name, Node} ! {self(), Msg},
    R = receive
	    {nodedown, Node} -> {error, nodedown};
	    {ReplyWrapper, Node, Reply} -> Reply
	end,
    erlang:monitor_node(Node, false),
    R.

cast(Node, Mod, Fun, Args) when Node == node() ->
    catch spawn(Mod, Fun, Args),
    true;
cast(Node,Mod,Fun,Args) ->
    gen_server:cast({rex,Node},{cast,Mod,Fun,Args,group_leader()}),
    true.


%% Asyncronous broadcast, returns nothing, it's just send'n prey
abcast(Name, Mess) ->
    abcast([node() | nodes()], Name, Mess).
abcast([Node|Tail], Name, Mess) ->
    {Name, Node} ! Mess,
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
    send_nodes(Nodes, rex, {sbcast, Name, Mess}),
    rec_nodes(rex, Nodes).

eval_everywhere(Mod, Fun, Args) ->
    eval_everywhere([node() | nodes()] , Mod, Fun, Args).
eval_everywhere(Nodes, Mod, Fun, Args) ->
    gen_server:abcast(Nodes, rex,
		      {cast,Mod,Fun,Args,group_leader()}).


send_nodes([Node|Tail], Name, Msg) ->
    monitor_node(Node, true),
    {Name, Node} ! {self(), Msg},
    send_nodes(Tail, Name, Msg);
send_nodes([], _, _) -> done.


%% Call apply(M,F,A) on all nodes in parallel
multicall(M,F,A) -> 
    multicall([node() | nodes()], M,F,A).
multicall(Nodes,M,F,A) ->
    {Rep,Bad} = gen_server:multi_call(Nodes, rex,
				      {call, M,F,A, group_leader()}),
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

multi_server_call(Nodes, Name, Msg) ->
    send_nodes(Nodes, Name, Msg),
    rec_nodes(Name, Nodes).

%% Returns {Replies, Badnodes} where Badnodes is a list of the nodes
%% that either terminated during the call or that didn't have a server
%% called Name running

safe_multi_server_call(Name, Msg) ->
    safe_multi_server_call([node() | nodes()], Name, Msg).

safe_multi_server_call(Nodes, Name, Msg) ->
    send_nodes(Nodes, rex, {send, Name, Msg}),
    rec_nodes(Name, Nodes).


rec_nodes(Name, Nodes) -> 
    rec_nodes(Name, Nodes, [],[]).


rec_nodes(Name, [],  Badnodes, Replies) ->
    {Replies, Badnodes};

rec_nodes(Name, [N|Tail], Badnodes, Replies) ->
    receive
	{nodedown, N} ->
	    monitor_node(N, false),
	    rec_nodes(Name, Tail, [N|Badnodes], Replies);
	{rex, N, {nonexisting_name, Name2}} ->  
	    %% used by safe_multi_server_call(),sbcast()
	    monitor_node(N, false),
	    rec_nodes(Name, Tail, [N|Badnodes], Replies);
	{Name, N, Reply} ->  %% Tag is bound !!!
	    monitor_node(N, false),
	    rec_nodes(Name, Tail, Badnodes, [Reply|Replies])
    end.

%% Now for an asynchronous rpc.
%%% Barbara Liskov like Call  Streams 
%% An asyncronous version of rpc that  is faster for series of
%% rpc's towards the same node. I.e it returns immideataly and 
%% it returns a Key that can be used in a subsequent yield(Key)

async_call(Node,Mod,Fun,Args) ->
    spawn(rpc,do_cs_call,[self(),Node,Mod,Fun,Args]).

yield(Key) ->
    receive
        {Key,{promise_reply,R}} ->
            R
    end.

nb_yield(Key) ->    %% Non blocking version
    receive
        {Key,{promise_reply,R}} ->
            {value,R}
        after 0 ->
            timeout
    end.

do_cs_call(ReplyTo,N,M,F,A) ->
    R = call(N,M,F,A),                      %% proper rpc
    ReplyTo ! {self(),{promise_reply,R}}.   %% self() is key
    

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
    [rpc:async_call(Node,M,F,A) | 
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
check([{badrpc, _} |Tail], _) -> exit(badrpc);
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

