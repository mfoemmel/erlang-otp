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
%%%----------------------------------------------------------------------
%%% Purpose   : Spawn a worker for each connection initiated from Java
%%%----------------------------------------------------------------------

-module(jive_worker).


-export([start/1,
         init/2,
         system_continue/3,
         system_terminate/4,
         system_code_change/4,
         write_debug/3]).

						% These are exported only for use by the test suite.
-export([pack/1,
	 parse_expr/1]).

-include("jive.hrl").

%%---------------------------------------------------------------------------
%%
%% NOTE: If you change the format of the State variable you have to change
%% the function system_code_change/4 to update the format.
%%
%% This module implements the workers that serve requests from a
%% specific connection from Java.
%% 
%%---------------------------------------------------------------------------

start(Socket) ->
    Pid = proc_lib:spawn_link(?MODULE, init, [self(), Socket]),
    {ok, Pid}.


init(Parent, Socket) ->
    Deb = sys:debug_options(?DBG_OPTS),		% XXX: Failed if sent Options
    Jpid = unregistered,			% No jpid for our pid
    State = {state,Socket,Jpid,[]},
    loop(Parent, Deb, State).



loop(Parent, Deb, State) ->
    {state,Socket,Jpid,OldData} = State,
    receive
	{send,Receiver,Msg} when Jpid /= unregistered ->
	    Deb1 = sys:handle_debug(Deb, {?MODULE, write_debug}, jive_worker,
				    {in, {send,Receiver,Msg}}),
	    %% We got data so we send it thru the socket to the client
	    Bytes = pack_message(Receiver,Msg),
	    Deb2 = sys:handle_debug(Deb1, {?MODULE, write_debug}, jive_worker,
				    {out, {tcp_send,Socket,Bytes}}),
	    gen_tcp:send(Socket, Bytes),
	    loop(Parent, Deb2, State);

	{tcp_closed, Socket} ->
	    %% Other side has closed. If we where registered with a Jpid
	    %% we don't need to unregister because the exit signal will
	    %% go to 'jive_server' and it will do that for us.

	    sys:handle_debug(Deb, {?MODULE, write_debug}, jive_worker,
			     {in, {tcp_closed, Socket}}),
	    gen_tcp:close(Socket);		% Close to free resources

	{tcp_error, Socket, Message} ->		% XXX: Actually an error report
	    Deb1 = sys:handle_debug(Deb, {?MODULE, write_debug}, jive_worker,
				    {in, {tcp_error, Socket, Message}}),
	    loop(Parent, Deb1, State);

	{tcp, Socket, Data} ->
	    Deb1 = sys:handle_debug(Deb, {?MODULE, write_debug}, jive_worker,
				    {in, {tcp, Socket, Data}}),
	    {ok,Deb2,NewJpid,DataLeft} = do_messages(OldData ++ Data,
						     Socket, Deb1, Jpid),
	    loop(Parent, Deb2, {state,Socket,NewJpid,DataLeft});
	{system, From, Request} ->
	    sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, State)
    end.


do_messages(Data, Socket, Deb, Jpid) ->
    case do_message(Data, Socket, Deb, Jpid) of
	{done,NewDeb,NewJpid,DataLeft} ->
	    {ok,NewDeb,NewJpid,DataLeft};
	{ok,NewDeb,NewJpid,DataLeft} ->
	    do_messages(DataLeft, Socket, NewDeb, NewJpid)
    end.

do_message(Data, Socket, Deb, Jpid) ->
    case parse_message(Data) of
	partial ->
	    {done,Deb,Jpid,Data};
	{ok,Message,DataLeft} ->
	    case Message of 
		{new_client, NewJpid} ->
		    Deb1 = sys:handle_debug(Deb, {?MODULE, write_debug},
					    jive_worker,
					    {parse_message_new_client,
					     NewJpid}),
		    Bytes = pack(NewJpid),
		    Deb2 = sys:handle_debug(Deb1, {?MODULE, write_debug},
					    jive_worker,
					    {out, {tcp_send,Socket,Bytes}}),
		    gen_tcp:send(Socket, [Bytes]),
		    {ok,Deb2,NewJpid,DataLeft};
		{value,Val} ->
		    Deb1 = sys:handle_debug(Deb, {?MODULE, write_debug},
					    jive_worker,
					    {parse_message_value,Val}),
		    Bytes = pack(Val),
		    Deb2 = sys:handle_debug(Deb1, {?MODULE, write_debug},
					    jive_worker,
					    {out, {tcp_send,Socket,Bytes}}),
		    gen_tcp:send(Socket, [Bytes]),
		    {ok,Deb2,Jpid,DataLeft};
		{error, Msg} ->
		    Deb1 = sys:handle_debug(Deb, {?MODULE, write_debug},
					    jive_worker,
					    {parse_message_result,
					     {error, Msg}}),
		    Bytes = [60,1,pack(Msg)],	% XXX: Just one errno...
		    Deb2 = sys:handle_debug(Deb1, {?MODULE, write_debug},
					    jive_worker,
					    {out, {tcp_send,Socket,Bytes}}),
		    gen_tcp:send(Socket, Bytes),
		    {ok,Deb2,Jpid,DataLeft};
		error ->
		    Deb1 = sys:handle_debug(Deb, {?MODULE, write_debug},
					    jive_worker,
					    parse_message_error),
		    {ok,Deb1,Jpid,DataLeft};
		ok ->
		    {ok,Deb,Jpid,DataLeft}
	    end
    end.


%%---------------------------------------------------------------------------
%%  Parse the different messages that can be sent to the server
%%---------------------------------------------------------------------------

%%
%% Parse a message sent to the server
%%
%% We return one of
%%
%%	{ok, Msg, Data}		successful returning the message
%%	partial			not enough data for this message
%%
%% The parse functions below for "apply", "spawn", "new client" and "spawn"
%% have side effects so we have to make sure we verify that we have all
%% data needed before the side effect is done.
%% 
%% 
%% 
%%
parse_message([]) ->
    partial;
parse_message(Msg) ->
    [Tag | Tail] = Msg,
    case Tag of 
	50 ->
	    parse_apply(Msg);
	51 ->
	    parse_spawn(Msg);
	52 ->
	    parse_send(Msg);
	53 ->
	    parse_new_client(Msg)
    end.

%%
%% Parse an apply message
%%
parse_apply([50 | Message]) ->
    case parse_atom(Message) of
	{ok,Module,Msg2} ->
	    case parse_atom(Msg2) of
		{ok,Name,Msg3} ->
		    case parse_list(Msg3) of
			{ok,List,Msg4} ->
			    case jive:check_allowed(Module, Name,
						    length(List)) of 
				ok ->
				    Result = apply(Module,Name,List),
				    {ok,{value,Result},Msg4};
				error ->
				    ErrMsg = lists:concat(["Function ",
							   Module,":",Name,"/",
							   length(List),
							   " is not allowed "
							   "to be used"]),
				    {ok,
				     {error,jive:list_to_string(ErrMsg)},Msg4}
			    end;
			partial ->
			    partial
		    end;
		partial ->
		    partial
	    end;
	partial ->
	    partial
    end.


%%
%% Parse a spawn message
%%
parse_spawn([51 | Message]) ->
    case parse_atom(Message) of
	{ok,Module,Msg2} ->
	    case parse_atom(Msg2) of
		{ok,Name,Msg3} ->
		    case parse_list(Msg3) of
			{ok,List,Msg4} ->
			    case jive:check_allowed(Module, Name,
						    length(List)) of 
				ok ->
				    %% Link, we don't want zombies
				    Pid =proc_lib:spawn_link(Module,Name,List),
				    PidId = jive:register_pid(Pid),
				    {ok,{value,PidId},Msg4};
				error ->
				    ErrMsg = lists:concat(["Process ",
							   Module,":",Name,"/",
							   length(List),
							   " is not allowed "
							   "to be spawned"]),
				    {ok,
				     {error,jive:list_to_string(ErrMsg)},Msg4}
			    end;
			partial ->
			    partial
		    end;
		partial ->
		    partial
	    end;
	partial ->
	    partial
    end.

%%
%% Parse a message sent when a client connects
%%
parse_new_client([53 | Message]) ->
    Jpid = jive:register_pid(self()),
    {ok,{new_client,Jpid},Message}.

%%
%% Parse a send message to a pid
%%
parse_send([52 | Message]) ->
    case parse_pid(Message) of
	{ok,Jpid,Expr} ->
	    case parse_expr(Expr) of
		{ok,Msg,Msg2} ->
		    case jive:get_pid(Jpid) of 
			error ->
			    {ok,error,Msg2};
			Pid when pid(Pid) ->
			    Pid ! Msg,
			    {ok,ok,Msg2}
		    end;
		partial ->
		    partial
	    end;
	partial ->
	    partial
    end.

%%---------------------------------------------------------------------------
%%  Parse an encoded Erlang expression
%%---------------------------------------------------------------------------

%%
%% Parse an encoded Erlang expression
%%
%% The parse functions below has no side effect. If we haven't got
%% enough data we return the atom 'partial' and the caller has to
%% wait for more data before he try again. So the return values are
%%
%%     {ok,Term,DataLeft}	We are done and there may be data left
%%     partial			We don't have enough data to proceed
%%
parse_expr([]) ->
    partial;
parse_expr(Expr) ->
    [Tag|Tail] = Expr,
    case Tag of 
	100 ->
	    parse_string(Expr);
	101 ->
	    parse_list(Expr);
	102 ->
	    parse_tuple(Expr);
	103 ->
	    parse_atom(Expr);
	104 ->
	    parse_integer(Expr);
	105 ->
	    parse_pid(Expr);
	106 ->
	    parse_float(Expr);
	107 ->
	    parse_binary(Expr)
%    108 ->
%      parse_ref(Expr);
%    109 ->
%      parse_port(Expr);
%    110 ->
%      parse_erlang_pid(Expr);
%    111 ->
%      parse_xlist(Expr)
    end.

%%
%% Parse an encoded string
%%
parse_string([100,L1,L2,L3,L4|Message]) ->
    Length = bytes_to_int(L1,L2,L3,L4),
    parse_string(Length, Message, [], string);
parse_string(Message) ->			% Not even 5 bytes
    partial.

parse_string(0, Message, String, string) ->
    {ok,lists:reverse(String),Message};
parse_string(0, Message, String, atom) ->
    Atom = list_to_atom(lists:reverse(String)),
    {ok,Atom,Message};
parse_string(N, [Char | Message], String, Type) when N > 0 ->
    parse_string(N-1, Message, [Char | String], Type);
parse_string(N, [], String, Type) when N > 0 ->	% No complete string
    partial.

%%
%% Parse an encoded atom
%%
parse_atom([103,L1,L2,L3,L4|Message]) ->
    Length = bytes_to_int(L1,L2,L3,L4),
    parse_string(Length, Message, [], atom);
parse_atom(Message) ->
    partial.

%%
%% Parse an encoded integer (sign + unsigned int)
%%
parse_integer([104,Sign,I1,I2,I3,I4 | Message]) ->
    Int = bytes_to_int(I1,I2,I3,I4),
    case Sign of 
	0 ->
	    {ok,Int,Message};
	1 ->
	    {ok,-Int,Message}
    end;
parse_integer(Message) ->
    partial.

%%
%% Parse an encoded pid-id
%%
parse_pid([105,I1,I2,I3,I4 | Message]) ->
    Pid = bytes_to_int(I1,I2,I3,I4),
    {ok,{pidid,Pid},Message};
parse_pid(Message) ->
    partial.

%%
%% Parse an encoded list
%%
parse_list([101,L1,L2,L3,L4 | Message]) ->
    Length = bytes_to_int(L1,L2,L3,L4),
    parse_list(Message,Length,[]);
parse_list(Message) ->				% Not even list length
    partial.

parse_list(Tail,0,List) ->
    {ok,lists:reverse(List),Tail};
parse_list(Message,Length,List) ->
    case parse_expr(Message) of
	{ok,Expr,Tail} ->
	    parse_list(Tail,Length-1,[Expr | List]);
	partial ->
	    partial
    end.

%%
%% Parse an encoded tuple
%%
parse_tuple([102,L1,L2,L3,L4 | Message]) ->
    Length = bytes_to_int(L1,L2,L3,L4),
    case parse_list(Message,Length,[]) of
	{ok,List,Tail} ->
	    Tuple = list_to_tuple(List),
	    {ok,Tuple,Tail};
	partial ->
	    partial
    end;
parse_tuple(Message) ->
    partial.

%%
%% Parse an encoded float
%%
parse_float([106 | Message]) ->
    case parse_string([100 | Message]) of
	{ok,Float,Tail} ->
	    {ok,list_to_float(Float),Tail};
	partial ->
	    partial
    end.

%%
%% Parse an encoded binary
%%
parse_binary([107 | Message]) ->
    case parse_string([100 | Message]) of
	{ok,Binary,Tail} ->
	    {ok,list_to_binary(Binary),Tail};
	partial ->
	    partial
    end.

%%
%% Parse an encoded ref
%%
%parse_ref([108 | Message]) ->
%  {Ref,Tail} = parse_string([100 | Message]),
%  {binary_to_term(list_to_binary(Ref)),Tail}.

%%
%% Parse an encoded port
%%
%parse_port([109 | Message]) ->
%  {Port,Tail} = parse_string([100 | Message]),
%  {binary_to_term(list_to_binary(Port)),Tail}.

%%
%% Parse an encoded pid
%%
%parse_erlang_pid([110 | Message]) ->
%  {Pid,Tail} = parse_string([100 | Message]),
%  {binary_to_term(list_to_binary(Pid)),Tail}.


%%
%% Parse an encoded list where tail /= []
%% It has as least one cons cell
%%
%parse_xlist([111,L1,L2,L3,L4 | Message]) ->
%    Length = bytes_to_int(L1, L2, L3, L4),
%    parse_xlist(Message, Length).

%parse_xlist(Message, 1) ->
%    parse_expr(Message);
%parse_xlist(Message, Length) ->
%    {Term, Tail0} = parse_expr(Message),
%    {Terms, Tail} = parse_xlist(Tail0, Length-1),
%    {[Term | Terms], Tail}.

%%
%% convert 4 bytes to an unsigned int
%%
bytes_to_int(I1,I2,I3,I4) ->
    (I1 bsl 24) bor (I2 bsl 16) bor (I3 bsl 8) bor I4.

%%
%% convert an int to 4 bytes
%%
int_to_bytes(I) ->
    [(I bsr 24) band 16#ff,
     (I bsr 16) band 16#ff,
     (I bsr  8) band 16#ff,
     I band 16#ff].

%%---------------------------------------------------------------------------
%% Pack messages sent to the java side
%%---------------------------------------------------------------------------

%%
%% Packs a message to a receiver
%%
pack_message(Receiver,Message) when integer(Receiver) ->
    [pack(Receiver),pack(Message)].

%%
%% Pack a string
%%
pack({string,S}) when list(S) ->
    check_string(S, S),
    [[100 | int_to_bytes(length(S))]|S];

%%
%% Pack a Jpid
%%
pack({pidid,I}) when integer(I) ->
    [105 | int_to_bytes(I)];

%%
%% Pack a list, will represent [], the empty list
%%
pack(L) when list(L) ->
    case proper_list(L) of
	yes ->
	    ByteList = lists:map(fun pack/1,[],L),
	    [[101 | int_to_bytes(length(L))] | ByteList];
	no ->
	    ?DBG_FORMAT("WARNING: not a proper list \"~w\"",[L]),
	    {ByteList, Length} = pack_xlist(L, [], 0),
	    [[111 | int_to_bytes(Length)] | ByteList]
    end;

%%
%% Pack a tuple
%%
pack(T) when tuple(T) ->
    L = tuple_to_list(T),
    ByteList = lists:map(fun pack/1,[],L),
    [[102 | int_to_bytes(length(L))] | ByteList];

%%
%% Pack an atom
%%
pack(A) when atom(A) ->
    S = atom_to_list(A),
    [[103 | int_to_bytes(length(S))] | S];

%%
%% Pack an Integer
%%
pack(I) when integer(I) ->
    [104 | int_to_bytes(I)];

%%
%% Pack a float
%%
pack(F) when float(F) ->
    S = float_to_list(F),
    [[106 | int_to_bytes(length(S))] | S];

%%
%% Pack a binary
%%
pack(B) when binary(B) ->
    L = binary_to_list(B),
    [[107 | int_to_bytes(length(L))] | L].

%%
%% Pack a ref
%%
%pack(B) when reference(B) ->
%    L = binary_to_list(term_to_binary(B)),
%    [[108 | int_to_bytes(length(L))] | L];

%%
%% Pack a port
%%
%pack(B) when port(B) ->
%    L = binary_to_list(term_to_binary(B)),
%    [[109 | int_to_bytes(length(L))] | L];

%%
%% Pack a pid
%%
%pack(B) when pid(B) ->
%    L = binary_to_list(term_to_binary(B)),
%    [[110 | int_to_bytes(length(L))] | L].


%
% Determine if list ends in []
%
proper_list([H | T]) ->
    proper_list(T);
proper_list([]) ->
    yes;
proper_list(_) ->
    no.

pack_xlist([H | T], ByteList, Len) ->
    pack_xlist(T, [pack(H) | ByteList], Len+1);
pack_xlist(T, ByteList, Len) ->
    {lists:reverse([pack(T) | ByteList]), Len+1}.


%%
%% In all cases we close the socket
%%
system_terminate(Reason, Parent, Deb, {state,Socket,Jpid,Data}) ->
    gen_tcp:close(Socket),			% Polite to clean up
    exit(Reason).

%%
%% Debugging output
%%
write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p event = ~p~n", [Name, Event]).

%%---------------------------------------------------------------------------

%%
%% system_continue/3
%%
system_continue(Parent, Deb, State) ->
    loop(Parent, Deb, State).

%%
%% This is where we update the state/loop variable if needed. Right now
%% we are the first version that support code update so we pass
%% the data on unmodified.
%%
system_code_change(Misc, Module, OldVsn, Extra) ->
    {ok, Misc}.



check_string([], S) ->
    true;
check_string([H | T], S) when integer(H), H >= 0, H < 256 ->
    check_string(T, S);
check_string(_, S) ->
    ?DBG_FORMAT("WARNING: not a proper string \"~w\"",[S]).
