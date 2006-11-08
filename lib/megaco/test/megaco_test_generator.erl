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
%%----------------------------------------------------------------------
%% Purpose: Message sequence generator for megaco
%%----------------------------------------------------------------------
-module(megaco_test_generator).

-export([start_link/1, start_link/2, stop/1, 
	 tcp/2, tcp/3, 
	 megaco/2, megaco/3, 
	 await_reply/2, await_reply/3, 
	 tcp_await_reply/1, tcp_await_reply/2, 
	 megaco_await_reply/1, megaco_await_reply/2
	]).

%% Internal exports
-export([start/2, init/3, tcp_handler_main/3, megaco_handler_main/3]).

%% Megaco callback api
-export([
         handle_connect/3,
         handle_disconnect/4,
         handle_syntax_error/4,
         handle_message_error/4,
         handle_trans_request/4,
         handle_trans_long_request/4,
         handle_trans_reply/5,
         handle_trans_ack/5,
	 handle_trans_request_abort/5,
	 handle_unexpected_trans/4
        ]).

-export([megaco_connect/5]).


-include_lib("megaco/include/megaco.hrl").


%%----------------------------------------------------------------------

-define(TIMEOUT, timer:minutes(5)).


%%----------------------------------------------------------------------

-record(state, {parent, handler, name, timer}).

%% A TCP sequence generator data record
-record(tcp, 
	{
	  listen,     % Listen socket
	  connection, % Connection socket
	  encode,     % Encode fun
	  decode,     % Decode fun
	  result = [] % Accumulated results from verification
	 }).

%% A MEGACO sequence generator data record
-record(megaco,
	{
	  mid,
	  recv_handle,
	  port,
	  send_handle,
	  conn_handle,

	  transport_sup,
	  ctrl_pid,

	  result = [] % Accumulated results from verification
	 }).


%%----------------------------------------------------------------------

start_link(Name) when is_list(Name) ->
    start(Name, self()).

start_link(Name, Node) when is_list(Name) and (Node /= node()) ->
    case rpc:call(Node, ?MODULE, start, [Name, self()]) of
	{ok, Pid} ->
	    link(Pid),
	    {ok, Pid};
	Error ->
	    Error
    end;
start_link(Name, Node) when is_list(Name) and (Node == node()) ->
    case start(Name, self()) of
	{ok, Pid} ->
	    link(Pid),
	    {ok, Pid};
	Error ->
	    Error
    end.	    

start(Name, Pid) when is_pid(Pid) ->
    proc_lib:start(?MODULE, init, [Name, self(), Pid]).


stop(Pid) ->
    request(Pid, stop).


tcp(Pid, Instructions) ->
    tcp(Pid, Instructions, ?TIMEOUT).

tcp(Pid, Instructions, Timeout) 
  when is_pid(Pid) and is_list(Instructions) and is_integer(Timeout) ->
    request(Pid, {tcp, Instructions, Timeout}),
    tcp_reply.

tcp_await_reply(Pid) ->
    await_reply(tcp_reply, Pid).

tcp_await_reply(Pid, Timeout) ->
    await_reply(tcp_reply, Pid, Timeout).


megaco(Pid, Instructions) ->
    megaco(Pid, Instructions, ?TIMEOUT).

megaco(Pid, Instructions, Timeout) 
  when is_pid(Pid) and is_list(Instructions) and is_integer(Timeout) ->
    request(Pid, {megaco, Instructions, Timeout}),
    megaco_reply.

megaco_await_reply(Pid) ->
    await_reply(megaco_reply, Pid).

megaco_await_reply(Pid, Timeout) ->
    await_reply(megaco_reply, Pid, Timeout).

request(Pid, Req) ->
    Pid ! {Req, self()}.
    

await_reply(Tag, Pid) ->
    await_reply(Tag, Pid, infinity).

await_reply(Tag, Pid, Timeout) ->
    receive
	{Tag, Pid, Reply} ->
	    Reply
    after Timeout ->
	    {error, timeout}
    end.


%%----------------------------------------------------------------------
    
init(Name, Starter, Parent) ->
    put(name, Name ++ "-GEN"),
    d("init -> entry with"
      "~n   Name:    ~s"
      "~n   Starter: ~p"
      "~n   Parent:  ~p", [Name, Starter, Parent]),
    proc_lib:init_ack(Starter, {ok, self()}),
    loop(#state{parent = Parent, name = Name}).

loop(#state{parent = Parent} = State) ->
    d("loop -> await request"),
    receive
	{stop, Parent} ->
	    abort(State),
	    exit(normal);
	
	{'EXIT', Parent, Reason} ->
	    exit({parent_exit, Reason});

	{abort, Parent} ->
	    abort(State),
	    loop(State);
	    
	{{tcp, Instructions, Timeout}, Parent} ->
	    d("loop -> received tcp request"),
	    Handler = start_tcp_handler(State#state.name, Instructions),
	    d("loop -> tcp handler: ~p", [Handler]),
	    Timer = erlang:send_after(Timeout, self(), tcp_timeout),
	    loop(State#state{handler = Handler, timer = Timer});

	{tcp_result, Pid, Res} when State#state.handler == Pid ->
	    d("loop -> received tcp result: ~n~p", [Res]),
	    Parent ! {tcp_reply, self(), Res},
	    Timer = State#state.timer,
	    Time = erlang:cancel_timer(Timer),
	    d("loop -> Time: ~w msec", [Time]),
	    loop(State#state{handler = undefined, timer = undefined});

	{{megaco, Instructions, Timeout}, Parent} ->
	    d("loop -> received megaco request"),
	    Handler = start_megaco_handler(State#state.name, Instructions),
	    d("loop -> megaco handler: ~p", [Handler]),
	    Timer = erlang:send_after(Timeout, self(), megaco_timeout),
	    loop(State#state{handler = Handler, timer = Timer});

	{megaco_result, Pid, Res} when State#state.handler == Pid ->
	    d("loop -> received megaco_reply"),
	    Parent ! {megaco_reply, self(), Res},
	    Timer = State#state.timer,
	    Time = erlang:cancel_timer(Timer),
	    d("loop -> Time: ~p", [Time]),
	    loop(State#state{handler = undefined, timer = undefined});

	{'EXIT', Handler, Reason} when State#state.handler == Handler ->
	    abort(State),
	    exit({handler_exit, Reason});

	tcp_timeout ->
	    d("loop -> received tcp-timeout"),
	    Handler = State#state.handler,
	    exit(Handler, kill),
	    Parent ! {tcp_reply, self(), {error, timeout}},
	    loop(State#state{handler = undefined, timer = undefined});

	megaco_timeout ->
	    d("loop -> received megaco-timeout"),
	    Handler = State#state.handler,
	    exit(Handler, kill),
	    Parent ! {megaco_reply, self(), {error, timeout}},
	    loop(State#state{handler = undefined, timer = undefined});

	Any ->
	    p("loop -> received "
	      "~n   Any: ~p", [Any]),
	    loop(State)
	    
    end.


%%% ----------------------------------------------------------------

abort(#state{handler = Pid}) when is_pid(Pid) ->    
    Pid ! {stop, self()};
abort(_) ->
    ok.


%%% ----------------------------------------------------------------

start_tcp_handler(Name, Instructions) ->
    spawn_link(?MODULE, tcp_handler_main, [Name, Instructions, self()]).

tcp_handler_main(Name, Instructions, Parent) ->
    put(name, Name),
    {Tcp, Reply} = handle_tcp(Instructions),
    close(Tcp), %% attempt to close just in case
    Parent ! {tcp_result, self(), Reply},
    receive
	{stop, Parent} ->
	    tcp_cleanup(Tcp),
	    exit(normal)
    end.


%% Instructions: [instruction()]
%% instruction() = {debug,  Debug}  |
%%                 {encode, Encode} |
%%                 {decode, Decode} |
%%                 {send,   Msg}    |
%%                 {expect_receive, TransactionType} |
%%                 {expect_receive, {TransactionType, To}} |
%%                 {listen, Port} |
%%                 {expect_accept, {{Host, Port}, To}} |
%%                 {accept, Port} |
%%                 {accept, {Addr, Port}} |
%%                 {sleep, To} |
%%                 
handle_tcp(Instructions0) when is_list(Instructions0) ->
    case (catch parse_tcp(Instructions0, [])) of
	{ok, Instructions} ->
	    (catch handle_tcp1(Instructions));
	Error ->
	    {invalid, Error}
    end.

handle_tcp1(Instructions) ->
    handle_tcp1(Instructions, #tcp{}).

handle_tcp1([], #tcp{result = Res} = Tcp) ->
    Reply = {ok, lists:reverse(Res)},
    {Tcp, Reply};
handle_tcp1([Instruction|Instructions], Tcp0) ->
    case (catch do_tcp(Instruction, Tcp0)) of
	Tcp when is_record(Tcp, tcp) ->
	    handle_tcp1(Instructions, Tcp);
	Error ->
	    {Tcp0, Error}
    end.

do_tcp({debug, Debug}, Tcp) ->
    p("debug: ~p", [Debug]),
    put(debug, Debug),
    Tcp;

do_tcp({encode, Encode}, Tcp) ->
    p("encode: ~p", [Encode]),
    Tcp#tcp{encode = Encode};

do_tcp({decode, Decode}, Tcp) ->
    p("decode: ~p", [Decode]),
    Tcp#tcp{decode = Decode};

do_tcp(disconnect, Tcp) ->
    p("disconnect"),
    tcp_cleanup(Tcp),
    Tcp#tcp{listen = undefined, connection = undefined};

do_tcp({listen, Port}, Tcp) ->
    p("listen to ~p", [Port]),
    Opts = [binary, {packet, tpkt}, {active, false}, {reuseaddr, true}],
    case (catch gen_tcp:listen(Port, Opts)) of
	{ok, Listen} ->
	    d("listen -> listen socket created"),
	    Tcp#tcp{listen = Listen};
	Error ->
	    e("failed creating listen socket: ~p", [Error]),
	    tcp_error(listen, Error)
    end;

do_tcp({expect_accept, {Addr, To}}, #tcp{listen = Listen} = Tcp) ->
    p("expect_accept from ~p (~p)", [Addr, To]),
    case (catch gen_tcp:accept(Listen, To)) of
	{ok, Sock} ->
	    d("expect_accept -> connection accepted"),
	    case (catch inet:peername(Sock)) of
		{ok, {Addr, _Port}} ->
		    d("expect_accept -> valid address"),
		    Tcp#tcp{connection = Sock};
		{ok, {_Addr, _Port}} when Addr == any ->
		    d("expect_accept -> valid"),
		    Tcp#tcp{connection = Sock};
		{ok, AddrPort} ->
		    tcp_error(expect_accept, {unknown_connect, AddrPort});
		Else ->
		    e("failed getting peername for socket: ~p", [Else]),
		    tcp_error(expect_accept, {peername, Else})
	    end;
	Error ->
	    e("failed accepting connection: ~p", [Error]),
	    close(Listen),
	    tcp_error(expect_accept, {accept_error, Error})
    end;

do_tcp({connect, {Addr, Port, To}}, Tcp) ->
    p("connect to ~p, ~p", [Addr, Port]),
    Opts = [binary, {packet, tpkt}, {active, once}],
    case (catch gen_tcp:connect(Addr, Port, Opts, To)) of
	{ok, Sock} ->
	    d("connect -> connected"),
	    Tcp#tcp{connection = Sock};
	Error ->
	    e("failed connecting: ~p", [Error]),
	    tcp_error(connect, Error)
    end;

%% Already encoded
do_tcp({send, Desc, Bin}, #tcp{connection = Sock} = Tcp) 
  when is_binary(Bin) ->
    p("send ~s message", [Desc]),
    NewBin = add_tpkt_header(Bin),
    d("send -> tpkt header added [~w], now send", [sz(NewBin)]),
    case (catch gen_tcp:send(Sock, NewBin)) of
	ok ->
	    d("send -> message sent"),
	    Tcp;
	Error ->
	    e("send -> send failed: ~n~p",[Error]),
	    close(Sock),
	    close(Tcp#tcp.listen),
	    tcp_error(send, {send_error, Error})
    end;

do_tcp({send, Desc, Msg}, #tcp{connection = Sock,
			       encode     = Encode} = Tcp) ->
    p("send ~s message", [Desc]),
    case (catch Encode(Msg)) of
	{ok, Bin} ->
	    d("send -> message encoded [~w], now add tpkt header: ~n~s", 
	      [sz(Bin), binary_to_list(Bin)]),
	    NewBin = add_tpkt_header(Bin),
	    d("send -> tpkt header added [~w], now send", [sz(NewBin)]),
	    case (catch gen_tcp:send(Sock, NewBin)) of
		ok ->
		    d("send -> message sent"),
		    Tcp;
		Error ->
		    e("send -> send failed: ~n~p",[Error]),
		    close(Sock),
		    close(Tcp#tcp.listen),
		    tcp_error(send, {send_error, Error})
	    end;
	Error ->
	    e("send -> encode failed: ~n~p",[Error]),
	    close(Sock),
	    close(Tcp#tcp.listen),
	    tcp_error(send, {endode_error, Error})
    end;

do_tcp({expect_receive, Desc, {Verify, To}}, 
       #tcp{connection = Sock,
	    decode     = Decode} = Tcp) ->
    p("expect_receive ~s message", [Desc]),
    inet:setopts(Sock, [{active, once}]),
    receive
	{tcp, Sock, <<3:8, _X:8, Length:16, Msg/binary>>} ->
	    d("expect_receive -> received message: Length = ~p", [Length]),
	    case (catch Decode(Msg)) of
		{ok, MegaMsg} when is_tuple(MegaMsg) ->
		    d("expect_receive -> decode successfull, now verify"),
		    case (catch Verify(MegaMsg)) of
			{ok, Res} ->
			    d("expect_receive -> verify successfull"),
			    Acc = Tcp#tcp.result,
			    Tcp#tcp{result = [Res|Acc]};
			Else ->
			    e("failed to verify message: ~n~p~n~p", 
			      [Else, MegaMsg]),
			    tcp_error(expect_receive, {verify_failed, Else})
		    end;
		Error ->
		    e("failed decoding message: ~p", [Error]),
		    tcp_error(expect_receive, Error)
	    end;
	Else ->
	    d("received unknown message: ~p", [Else]),
	    tcp_error(expect_receive, {unexpected_message, Else})
    after To ->
	    tcp_error(expect_receive, timeout)
    end;

do_tcp({expect_nothing, To}, #tcp{connection = Sock} = Tcp) ->
    p("expect_nothing ~w", [To]),
    inet:setopts(Sock, [{active, once}]),
    p("expect_nothing - await anything", []),
    receive
	Any ->
	    p("expect_nothing - received: ~p", [Any]),
	    tcp_error(expect_nothing, Any)
    after To ->
	    p("expect_nothing timeout after ~w", [To]),
	    Tcp
    end;

do_tcp({sleep, To}, Tcp) ->
    p("sleep ~p", [To]),
    sleep(To),
    Tcp.


tcp_error(Instr, Error) ->
    throw({error, {Instr, Error}}).


tcp_cleanup(#tcp{listen = Listen, connection = Conn}) ->
    close(Listen),
    close(Conn).


parse_tcp([], RevInstrs) ->
    {ok, lists:reverse(RevInstrs)};


parse_tcp([{debug, Debug}|Instrs], RevInstrs) 
  when Debug == true; Debug == false ->
    parse_tcp(Instrs, [{debug, Debug}|RevInstrs]);

%%
%% -- All the basic events, used when bypassing the megaco stack --
%% 

parse_tcp([{encode, Encode}|Instrs], RevInstrs) 
  when is_function(Encode) ->
    parse_tcp(Instrs, [{encode, Encode}|RevInstrs]);

parse_tcp([{decode, Decode}|Instrs], RevInstrs) 
  when is_function(Decode) ->
    parse_tcp(Instrs, [{decode, Decode}|RevInstrs]);

parse_tcp([disconnect|Instrs], RevInstrs) ->
    parse_tcp(Instrs, [disconnect|RevInstrs]);

parse_tcp([{listen, Port}|Instrs], RevInstrs) 
  when is_integer(Port), Port > 0 ->
    parse_tcp(Instrs, [{listen, Port}|RevInstrs]);

parse_tcp([{expect_accept, any}|Instrs], RevInstrs) ->
    parse_tcp(Instrs, [{expect_accept, {any, infinity}}|RevInstrs]);

parse_tcp([{expect_accept, {any, To}}|Instrs], RevInstrs) 
  when is_integer(To), To >= 0 ->
    parse_tcp(Instrs, [{expect_accept, {any, To}}|RevInstrs]);

parse_tcp([{expect_accept, {Host, infinity}}|Instrs], RevInstrs) ->
    {ok, Addr} = inet:getaddr(Host, inet),
    parse_tcp(Instrs, [{expect_accept, {Addr, infinity}}|RevInstrs]);

parse_tcp([{expect_accept, {Host, To}}|Instrs], RevInstrs) 
  when is_integer(To), To >= 0 ->
    {ok, Addr} = inet:getaddr(Host, inet),
    parse_tcp(Instrs, [{expect_accept, {Addr, To}}|RevInstrs]);

parse_tcp([{expect_accept, Host}|Instrs], RevInstrs) ->
    {ok, Addr} = inet:getaddr(Host, inet),
    parse_tcp(Instrs, [{expect_accept, {Addr, infinity}}|RevInstrs]);

parse_tcp([{connect, Port}|Instrs], RevInstrs) 
  when is_integer(Port), Port > 0 ->
    {ok, Host} = inet:gethostname(),
    {ok, Addr} = inet:getaddr(Host, inet),
    parse_tcp(Instrs, [{connect, {Addr, Port, infinity}}|RevInstrs]);

parse_tcp([{connect, {Port, infinity}}|Instrs], RevInstrs) 
  when is_integer(Port), Port > 0 ->
    {ok, Host} = inet:gethostname(),
    {ok, Addr} = inet:getaddr(Host, inet),
    parse_tcp(Instrs, [{connect, {Addr, Port, infinity}}|RevInstrs]);

parse_tcp([{connect, {Port, To}}|Instrs], RevInstrs) 
  when is_integer(Port) and (Port > 0) and 
       is_integer(To) and (To > 0) ->
    {ok, Host} = inet:gethostname(),
    {ok, Addr} = inet:getaddr(Host, inet),
    parse_tcp(Instrs, [{connect, {Addr, Port, To}}|RevInstrs]);

parse_tcp([{connect, {Host, Port}}|Instrs], RevInstrs) 
  when is_integer(Port) and (Port > 0) ->
    {ok, Addr} = inet:getaddr(Host, inet),
    parse_tcp(Instrs, [{connect, {Addr, Port, infinity}}|RevInstrs]);

parse_tcp([{connect, {Host, Port, infinity}}|Instrs], RevInstrs) 
  when is_integer(Port) and (Port > 0) ->
    {ok, Addr} = inet:getaddr(Host, inet),
    parse_tcp(Instrs, [{connect, {Addr, Port, infinity}}|RevInstrs]);

parse_tcp([{connect, {Host, Port, To}}|Instrs], RevInstrs) 
  when is_integer(Port) and (Port > 0) and
       is_integer(To) and (To > 0) ->
    {ok, Addr} = inet:getaddr(Host, inet),
    parse_tcp(Instrs, [{connect, {Addr, Port, To}}|RevInstrs]);

parse_tcp([{sleep, To}|Instrs], RevInstrs) 
  when is_integer(To) and (To > 0) ->
    parse_tcp(Instrs, [{sleep, To}|RevInstrs]);

parse_tcp([{expect_nothing, To}|Instrs], RevInstrs) 
  when is_integer(To) and (To > 0) ->
    parse_tcp(Instrs, [{expect_nothing, To}|RevInstrs]);

parse_tcp([{send, Desc, Msg}|Instrs], RevInstrs) 
  when is_list(Desc) and (is_tuple(Msg) or is_binary(Msg)) ->
    parse_tcp(Instrs, [{send, Desc, Msg}|RevInstrs]);

parse_tcp([{expect_receive, Desc, Verify}|Instrs], RevInstrs) 
  when is_list(Desc) and is_function(Verify) ->
    ExpRecv = {expect_receive, Desc, {Verify, infinity}},
    parse_tcp(Instrs, [ExpRecv|RevInstrs]);

parse_tcp([{expect_receive, Desc, {Verify, To}}|Instrs], RevInstrs) 
  when is_list(Desc) and is_function(Verify) and 
       is_integer(To) and (To > 0) ->
    ExpRecv = {expect_receive, Desc, {Verify, To}},
    parse_tcp(Instrs, [ExpRecv|RevInstrs]);

parse_tcp([Instr|_], _RevInstrs) ->
    throw({error, {invalid_instruction, Instr}}).


%%% ----------------------------------------------------------------

start_megaco_handler(Name, Instructions) ->
    spawn_link(?MODULE, megaco_handler_main, [Name, Instructions, self()]).

megaco_handler_main(Name, Instructions, Parent) ->
    put(name, Name),
    {Megaco, Reply} = handle_megaco(Instructions),
    Parent ! {megaco_result, self(), Reply},
    receive
	{stop, Parent} ->
	    megaco_cleanup(Megaco),
	    exit(normal)
    end.


%% Instructions: [instruction()]
%% instruction() = {debug,  Debug}  |
%%                 {sleep, To} |
%%                 
handle_megaco(Instructions0) when is_list(Instructions0) ->
    case (catch parse_megaco(Instructions0, [])) of
	{ok, Instructions} ->
	    (catch handle_megaco1(Instructions));
	Error ->
	    p("parsing arguments failed: ~n~p", [Error]),
	    {invalid, {error, Error}}
    end.

handle_megaco1(Instructions) ->
    handle_megaco1(Instructions, #megaco{}).

handle_megaco1([], #megaco{result = Res} = State) ->
    Reply = {ok, lists:reverse(Res)},
    {State, Reply};
handle_megaco1([Instruction|Instructions], State0) ->
    d("handle_megaco1 -> entry with"
      "~n   Instruction: ~p", [Instruction]),
    case (catch do_megaco(Instruction, State0)) of
	State when is_record(State, megaco) ->
	    handle_megaco1(Instructions, State);
	{error, State} when is_record(State, megaco) ->
	    p("handle_megaco1 -> error"), 
	    Reply = {error, {{instruction_failed, Instruction, Instructions},
			     lists:reverse(State#megaco.result)}},
	    {State, Reply};
	Error ->
	    p("handle_megaco1 -> Error: ~n~p", [Error]),
	    Reply = {error, {Error, lists:reverse(State0#megaco.result)}},
	    {State0, Reply}
    end.

do_megaco({debug, Debug}, State) ->
    p("debug: ~p", [Debug]),
    put(debug, Debug),
    State;

do_megaco({expect_nothing, To}, State) ->
    p("expect_nothing: ~p", [To]),
    receive
	Any ->
	    megaco_error(expect_nothing, Any)
    after To ->
        State
    end;

do_megaco({megaco_trace, disable}, State) ->
    p("megaco trace: disable"),
    megaco:disable_trace(),
    State;
do_megaco({megaco_trace, Level}, State) ->
    p("megaco trace: disable"),
    megaco:enable_trace(Level, io),
    State;

do_megaco(megaco_start, State) ->
    p("megaco_start"),
    ok = megaco:start(),
    State;

do_megaco(megaco_stop, State) ->
    p("megaco_stop"),
    ok = megaco:stop(),
    State;

do_megaco({megaco_start_user, Mid, RecvInfo, Conf}, State) ->
    p("megaco_start_user: ~p", [Mid]),

    d("megaco_start_user -> start user"),
    ok = megaco:start_user(Mid, Conf),
  
    d("megaco_start_user -> update user info: user_mod"),
    ok = megaco:update_user_info(Mid, user_mod,  ?MODULE),

    d("megaco_start_user -> update user info: user_args"),
    ok = megaco:update_user_info(Mid, user_args,  [self()]),
    
    Port = get_config(port, RecvInfo),
    EM   = get_config(encoding_module, RecvInfo),
    EC   = get_config(encoding_config, RecvInfo),
    TM   = get_config(transport_module, RecvInfo),
    RH0  = megaco:user_info(Mid, receive_handle),

    RH1  = RH0#megaco_receive_handle{send_mod        = TM,
				     encoding_mod    = EM,
				     encoding_config = EC},

    State#megaco{mid = Mid, recv_handle = RH1, port = Port};

do_megaco(megaco_stop_user, #megaco{mid = Mid} = State) 
  when Mid /= undefined ->
    megaco_cleanup(State),
    ok = megaco:stop_user(Mid),
    State#megaco{mid = undefined};

do_megaco(start_transport, #megaco{recv_handle = RH} = State) ->
    p("start_transport"),
    #megaco_receive_handle{send_mod = TM} = RH,
    {ok, Sup} = TM:start_transport(),
    d("start_transport -> Sup: ~p", [Sup]),
    State#megaco{transport_sup = Sup};

do_megaco({listen, Opts0}, 
	  #megaco{recv_handle = RH, port = Port, transport_sup = Pid} = State) 
  when RH#megaco_receive_handle.send_mod == megaco_tcp ->
    p("listen(tcp)"),
    Opts = [{port, Port}, {receive_handle, RH}|Opts0],
    case megaco_tcp:listen(Pid, Opts) of
	ok ->
	    State;
	Else ->
	    megaco_error({listen, Opts0}, {failed_starting_tcp_listen, Else})
    end;
do_megaco({listen, Opts0},
	  #megaco{recv_handle = RH, port = Port, transport_sup = Pid} = State) 
  when RH#megaco_receive_handle.send_mod == megaco_udp ->
    p("listen(udp) - open"),
    Opts = [{port, Port}, {receive_handle, RH}|Opts0],
    case megaco_udp:open(Pid, Opts) of
	{ok, _SH, _CtrlPid} ->
	    State;
	Else ->
	    megaco_error({listen, Opts0}, {failed_starting_udp_open, Else})
    end;

do_megaco({connect, Host, Opts0}, 
	  #megaco{transport_sup = Sup,
		  recv_handle   = RH, 
		  port          = Port} = State) ->
    #megaco_receive_handle{send_mod = TM} = RH,
    p("connect ~w ~p", [TM,Host]),
    PrelMid = preliminary_mid,
    case TM of
	megaco_tcp ->
	    Opts = [{host, Host}, {port, Port}, {receive_handle, RH}|Opts0],
	    case (catch megaco_tcp:connect(Sup, Opts)) of
		{ok, SH, ControlPid} ->
		    d("tcp connected: ~p, ~p", [SH, ControlPid]),
		    megaco_connector_start(RH, PrelMid, SH, ControlPid),
		    State#megaco{send_handle = SH,
				 ctrl_pid    = ControlPid};
		Error ->
		    megaco_error({connect, tcp, Host, Opts0}, Error)
	    end;
	megaco_udp ->
	    Opts = [{port, 0}, {receive_handle, RH}|Opts0],
            d("udp open", []),
	    case (catch megaco_udp:open(Sup, Opts)) of
		{ok, Handle, ControlPid} ->
		    d("udp opened: ~p, ~p", [Handle, ControlPid]),
		    SH = megaco_udp:create_send_handle(Handle, Host, Port),
		    megaco_connector_start(RH, PrelMid, SH, ControlPid),
		    State#megaco{send_handle = SH,
				 ctrl_pid    = ControlPid};
		Error ->
		    megaco_error({connect, udp, Host, Opts0}, Error)
	    end
    end;

do_megaco(megaco_connect, State) ->
    p("megaco_connect"),
    receive
	{megaco_connect_result, {ok, CH}} ->
	    p("megaco connect succeeded: ~p", [CH]),
	    State#megaco{conn_handle = CH};
	{megaco_connect_result, Error} ->
	    p("megaco connect failed: ~p", [Error]),
	    #megaco{result = Res} = State,
	    State#megaco{result = [Error|Res]}
    end;

do_megaco({megaco_user_info, Tag}, #megaco{mid = Mid, result = Res} = State) 
  when Mid /= undefined ->
    p("megaco_user_info: ~w", [Tag]),
    Val = (catch megaco:user_info(Mid, Tag)),
    d("megaco_user_info: ~p", [Val]),
    State#megaco{result = [Val|Res]};

do_megaco({megaco_update_user_info, Tag, Val}, #megaco{mid = Mid} = State) 
  when Mid /= undefined ->
    p("megaco_update_user_info: ~w -> ~p", [Tag, Val]),
    ok = megaco:update_user_info(Mid, Tag, Val),
    State;

do_megaco({megaco_conn_info, Tag}, 
	  #megaco{conn_handle = CH, result = Res} = State) 
  when CH /= undefined ->
    p("megaco_conn_info: ~w", [Tag]),
    Val = (catch megaco:conn_info(CH, Tag)),
    d("megaco_conn_info: ~p", [Val]),
    State#megaco{result = [Val|Res]};

do_megaco({megaco_update_conn_info, Tag, Val}, 
	  #megaco{conn_handle = CH} = State) 
  when CH /= undefined ->
    p("megaco_update_conn_info: ~w -> ~p", [Tag, Val]),
    ok = megaco:update_conn_info(CH, Tag, Val),
    State;

do_megaco({megaco_system_info, Tag}, #megaco{result = Res} = State) ->
    p("megaco_system_info: ~w", [Tag]),
    Val = (catch megaco:system_info(Tag)),
    d("megaco_system_info: ~p", [Val]),
    State#megaco{result = [Val|Res]};

%% This is either a MG or a MGC which is only connected to one MG
do_megaco({megaco_call, ARs, Opts}, #megaco{conn_handle = CH} = State)
  when CH /= undefined ->
    p("megaco_call"),
    {_PV, UserReply} = megaco:call(CH, ARs, Opts),
    d("megaco_cast -> UserReply: ~n~p", [UserReply]),
    State;

do_megaco({megaco_call, RemoteMid, ARs, Opts}, #megaco{mid = Mid} = State) ->
    p("megaco_call: ~p", [RemoteMid]),
    %% First we have to find the CH for this Mid
    Conns = megaco:user_info(Mid, connections),
    {value, {_, CH}} = 
	lists:keysearch(RemoteMid, #megaco_conn_handle.remote_mid, Conns),
    {_PV, UserReply} = megaco:call(CH, ARs, Opts),
    d("megaco_call -> UserReply: ~n~p", [UserReply]),
    State;

%% This is either a MG or a MGC which is only connected to one MG
do_megaco({megaco_cast, ARs, Opts}, #megaco{conn_handle = CH} = State)
  when CH /= undefined ->
    p("megaco_cast"),
    case megaco:cast(CH, ARs, Opts) of
	ok -> 
	    State;
	Error ->
	    d("failed sending (cast) message: ~n~p", [Error]), 
	    #megaco{result = Acc} = State,
	    {error, State#megaco{result = [Error|Acc]}}
    end;

do_megaco({megaco_cast, RemoteMid, ARs, Opts}, #megaco{mid = Mid} = State) ->
    p("megaco_cast: ~p", [RemoteMid]),
    %% First we have to find the CH for this Mid
    Conns = megaco:user_info(Mid, connections),
    {value, {_, CH}} = 
	lists:keysearch(RemoteMid, #megaco_conn_handle.remote_mid, Conns),
    case megaco:cast(CH, ARs, Opts) of
	ok ->
	    State;
	Error ->
	    d("failed sending (cast) message: ~n~p", [Error]), 
	    #megaco{result = Acc} = State,
	    {error, State#megaco{result = [Error|Acc]}}
    end;

%% Nothing shall happen for atleast Timeout time
do_megaco({megaco_callback, nocall, Timeout}, State) ->
    p("megaco_callback [~w]", [nocall]),
    receive
	{handle_megaco_callback, Type, Msg, Pid} ->
	    d("received unexpected megaco callback: ~n~p", [Msg]),
	    #megaco{result = Res} = State,
	    Err = {unexpected_callback, Type, Msg, Pid},
	    {error, State#megaco{result = [Err|Res]}}
    after Timeout ->
	    State
    end;

do_megaco({megaco_callback, Tag, Verify}, State) ->
    p("megaco_callback [~w]", [Tag]),
    receive
	{handle_megaco_callback, Type, Msg, Pid} ->
	    d("received megaco callback: ~n~p", [Msg]),
	    case Verify(Msg) of
		{VRes, Res, Reply} ->
		    d("megaco_callback [~w] ~w",[Tag,VRes]),
		    handle_megaco_callback_reply(Pid, Type, Reply),
		    validate(VRes, Tag, Res, State);
		{VRes, Delay, Res, Reply} ->
		    d("megaco_callback [~w] ~w, ~w",[Tag,Delay,VRes]),
		    handle_megaco_callback_reply(Pid, Type, Delay, Reply),
		    validate(VRes, Tag, Res, State)
	    end
    end;

do_megaco({megaco_callback, Tag, Verify, Timeout}, State) ->
    p("megaco_callback [~w]", [Tag]),
    receive
	{handle_megaco_callback, Type, Msg, Pid} ->
	    d("received megaco callback: ~n~p", [Msg]),
	    case Verify(Msg) of
		{VRes, Res, Reply} ->
		    d("megaco_callback [~w] ~w",[Tag,VRes]),
		    handle_megaco_callback_reply(Pid, Type, Reply),
		    validate(VRes, Tag, Res, State);
		{VRes, Delay, Res, Reply} ->
		    d("megaco_callback [~w] ~w, ~w",[Tag,Delay,VRes]),
		    handle_megaco_callback_reply(Pid, Type, Delay, Reply),
		    validate(VRes, Tag, Res, State)
	    end
    after Timeout ->
	    #megaco{result = Res} = State,
	    Err = {callback_timeout, Tag, Timeout},
	    {error, State#megaco{result = [Err|Res]}}
    end;

do_megaco({megaco_callback, Verifiers}, State) ->
    p("megaco_callback"),
    (catch megaco_callback_verify(Verifiers, State));

do_megaco({sleep, To}, State) ->
    p("sleep ~p", [To]),
    sleep(To),
    State;

do_megaco(Instr, _State) ->
    megaco_error(Instr, invalid_instruction).


%% This is used when a number of callback's is expected, but where
%% the specific order is unknown.
megaco_callback_verify([], State) ->
    d("megaco_callback_verify -> done"),
    State;
megaco_callback_verify(Verifiers0, State0) ->
    d("megaco_callback_verify -> entry when"
      "~n   length(Verifiers0): ~w", [length(Verifiers0)]),
    receive
	{handle_megaco_callback, Type, Msg, Pid} ->
	    d("received megaco callback: ~w~n~p", [Type,Msg]),
	    case megaco_callback_verify(Verifiers0, Type, Msg, Pid, State0) of
		{ok, Verifiers, State} ->
		    megaco_callback_verify(Verifiers, State);
		Error ->
		    Error
	    end
    end.
    
megaco_callback_verify(Verifiers0, Type, Msg, Pid, State0) ->
    d("megaco_callback_verify -> entry"),
    Tag = element(1,Msg),
    d("megaco_callback_verify -> Tag: ~w",[Tag]),
    case lists:keysearch(Tag, 1, Verifiers0) of
	{value, {Tag, N, Verify}} when (N > 0) and is_function(Verify) ->
	    d("megaco_callback_verify -> N: ~w",[N]),
	    case Verify(Msg) of
		{VRes, Res, Reply} ->
		    d("megaco_callback_verify -> VRes: ~w",[VRes]),
		    handle_megaco_callback_reply(Pid, Type, Reply),
		    case validate(VRes, Tag, Res, State0) of
			{error, _} = EState ->
			    d("megaco_callback_verify -> (1) error"),
			    throw(EState);
			State when N > 1 ->
			    d("megaco_callback_verify -> (1) validated"),
			    Rec = {Tag, N-1, Verify},
			    Verifiers = 
				lists:keyreplace(Tag, 1, Verifiers0, Rec),
			    {ok, Verifiers, State};
			State ->
			    d("megaco_callback_verify -> (2) validated"),
			    Verifiers = lists:keydelete(Tag, 1, Verifiers0),
			    {ok, Verifiers, State}
		    end;
		{VRes, Delay, Res, Reply} ->
		    d("megaco_callback_verify -> Delay: ~w, VRes: ~w",
		      [Delay,VRes]),
		    handle_megaco_callback_reply(Pid, Type, Delay, Reply),
		    case validate(VRes, Tag, Res, State0) of
			{error, _} = EState ->
			    d("megaco_callback_verify -> (2) error"),
			    throw(EState);
			State when N > 1 ->
			    d("megaco_callback_verify -> (3) validated"),
			    Rec = {Tag, N-1, Verify},
			    Verifiers = 
				lists:keyreplace(Tag, 1, Verifiers0, Rec),
			    {ok, Verifiers, State};
			State ->
			    d("megaco_callback_verify -> (4) validated"),
			    Verifiers = lists:keydelete(Tag, 1, Verifiers0),
			    {ok, Verifiers, State}
		    end
	    end;
	false ->
	    d("megaco_callback_verify -> no such tag ~w~n~p", 
	      [Tag, Verifiers0]),
	    #megaco{result = Res} = State0,
	    State = State0#megaco{result = [{Type, error, Msg}|Res]},
	    throw({error, State})
    end.

validate(ok, handle_connect = Tag, CH, #megaco{result = Acc} = S) ->
    S#megaco{conn_handle = CH, result = [{Tag, ok, CH}|Acc]};
validate(ok, Tag, Res, #megaco{result = Acc} = S) ->
    S#megaco{result = [{Tag, ok, Res}|Acc]};
validate(error, Tag, Res, #megaco{result = Acc} = S) ->
    {error, S#megaco{result = [{Tag, error, Res}|Acc]}}.


megaco_error(Instr, Error) ->
    throw({error, {Instr, Error}}).


megaco_connector_start(RH, PrelMid, SH, ControlPid) ->
    spawn_link(?MODULE, megaco_connect, [RH, PrelMid, SH, ControlPid, self()]).

megaco_connect(RH, PrelMid, SH, ControlPid, Parent) ->
    Result = megaco:connect(RH, PrelMid, SH, ControlPid),
    Parent ! {megaco_connect_result, Result},
    exit(normal).

megaco_cleanup(#megaco{mid = Mid}) ->
    Close = fun(CH) -> do_megaco_cleanup(CH) end,
    Conns = megaco:user_info(Mid, connections),
    lists:foreach(Close, Conns).

do_megaco_cleanup(CH) ->
    case (catch do_megaco_cleanup2(CH)) of
	ok ->
	    ok;
	{'EXIT', {no_such_connection, _}} ->
	    ok;
	{'EXIT', Reason} ->
	    exit(Reason)
    end.

do_megaco_cleanup2(CH) ->
    d("do_megaco_cleanup2 -> entry with"
      "~n   CH: ~p", [CH]),
    Reason     = {stopped_by_user,self()},
    Pid        = megaco:conn_info(CH, control_pid),
    SendMod    = megaco:conn_info(CH, send_mod),
    SendHandle = megaco:conn_info(CH, send_handle),
    d("do_megaco_cleanup2 -> disconnect"),
    megaco:disconnect(CH, Reason),
    d("do_megaco_cleanup2 -> disconnected, now cancel"),
    megaco:cancel(CH, Reason),
    d("do_megaco_cleanup2 -> canceled, now close"),
    case SendMod of
	megaco_tcp -> megaco_tcp:close(SendHandle);
	megaco_udp -> megaco_udp:close(SendHandle);
	SendMod    -> exit(Pid, Reason)
    end,
    ok.

parse_megaco([], RevInstrs) ->
    {ok, lists:reverse(RevInstrs)};


parse_megaco([{debug, Debug}|Instrs], RevInstrs) 
  when Debug == true; Debug == false ->
    parse_megaco(Instrs, [{debug, Debug}|RevInstrs]);

parse_megaco([{expect_nothing, To}|Instrs], RevInstrs) 
  when is_integer(To) and (To > 0) ->
    parse_megaco(Instrs, [{expect_nothing, To}|RevInstrs]);

parse_megaco([{megaco_trace, Level}|Instrs], RevInstrs) 
  when Level == disable; Level == max; Level == min ->
    Trace = {megaco_trace, Level},
    parse_megaco(Instrs, [Trace|RevInstrs]);

parse_megaco([{megaco_trace, Level}|Instrs], RevInstrs) 
  when is_integer(Level) ->
    Trace = {megaco_trace, Level},
    parse_megaco(Instrs, [Trace|RevInstrs]);

parse_megaco([{sleep, To}|Instrs], RevInstrs) 
  when is_integer(To) and (To > 0) ->
    parse_megaco(Instrs, [{sleep, To}|RevInstrs]);

parse_megaco([megaco_start|Instrs], RevInstrs) ->
    parse_megaco(Instrs, [megaco_start|RevInstrs]);

parse_megaco([megaco_stop|Instrs], RevInstrs) ->
    parse_megaco(Instrs, [megaco_stop|RevInstrs]);

parse_megaco([{megaco_start_user, Mid, RecvInfo, Conf}|Instrs], RevInstrs) 
  when is_list(Conf) ->
    Start = {megaco_start_user, Mid, RecvInfo, Conf},
    parse_megaco(Instrs, [Start|RevInstrs]);

parse_megaco([megaco_stop_user|Instrs], RevInstrs) ->
    parse_megaco(Instrs, [megaco_stop_user|RevInstrs]);

parse_megaco([{megaco_system_info, Tag}|Instrs], RevInstrs) 
  when is_atom(Tag) ->
    Info = {megaco_system_info, Tag},
    parse_megaco(Instrs, [Info|RevInstrs]);

parse_megaco([{megaco_user_info, Tag}|Instrs], RevInstrs) 
  when is_atom(Tag) ->
    Info = {megaco_user_info, Tag},
    parse_megaco(Instrs, [Info|RevInstrs]);

parse_megaco([{megaco_update_user_info, Tag, Val}|Instrs], RevInstrs) 
  when is_atom(Tag) ->
    Update = {megaco_update_user_info, Tag, Val},
    parse_megaco(Instrs, [Update|RevInstrs]);

parse_megaco([{megaco_conn_info, Tag}|Instrs], RevInstrs) 
  when is_atom(Tag) ->
    Info = {megaco_conn_info, Tag},
    parse_megaco(Instrs, [Info|RevInstrs]);

parse_megaco([{megaco_update_conn_info, Tag, Val}|Instrs], RevInstrs) 
  when is_atom(Tag) ->
    Update = {megaco_update_conn_info, Tag, Val},
    parse_megaco(Instrs, [Update|RevInstrs]);

parse_megaco([start_transport|Instrs], RevInstrs) ->
    parse_megaco(Instrs, [start_transport|RevInstrs]);

parse_megaco([listen|Instrs], RevInstrs) ->
    Listen = {listen, []},
    parse_megaco(Instrs, [Listen|RevInstrs]);

parse_megaco([{listen, Opts}|Instrs], RevInstrs) when is_list(Opts) ->
    Listen = {listen, Opts},
    parse_megaco(Instrs, [Listen|RevInstrs]);

parse_megaco([connect|Instrs], RevInstrs) ->
    {ok, LocalHost} = inet:gethostname(),
    Conn = {connect, LocalHost, []},
    parse_megaco(Instrs, [Conn|RevInstrs]);

parse_megaco([{connect, [{Key, _Val}|_] = Opts}|Instrs], RevInstrs) 
  when is_atom(Key) ->
    {ok, LocalHost} = inet:gethostname(),
    Conn = {connect, LocalHost, Opts},
    parse_megaco(Instrs, [Conn|RevInstrs]);

parse_megaco([{connect, Host}|Instrs], RevInstrs) ->
    Conn = {connect, Host, []},
    parse_megaco(Instrs, [Conn|RevInstrs]);

parse_megaco([{connect, Host, Opts}|Instrs], RevInstrs) ->
    Conn = {connect, Host, Opts},
    parse_megaco(Instrs, [Conn|RevInstrs]);

parse_megaco([disconnect|Instrs], RevInstrs) ->
    Disco = disconnect,
    parse_megaco(Instrs, [Disco|RevInstrs]);

parse_megaco([megaco_connect|Instrs], RevInstrs) ->
    Conn = megaco_connect,
    parse_megaco(Instrs, [Conn|RevInstrs]);

parse_megaco([megaco_disconnect|Instrs], RevInstrs) ->
    Disco = {megaco_disconnect, ignore},
    parse_megaco(Instrs, [Disco|RevInstrs]);

parse_megaco([{megaco_disconnect, Reason}|Instrs], RevInstrs) ->
    Disco = {megaco_disconnect, Reason},
    parse_megaco(Instrs, [Disco|RevInstrs]);

parse_megaco([{megaco_call, ARs, Opts}|Instrs], RevInstrs) 
  when is_list(ARs) and is_list(Opts) ->
    Call = {megaco_call, ARs, Opts},
    parse_megaco(Instrs, [Call|RevInstrs]);

parse_megaco([{megaco_call, Mid, ARs, Opts}|Instrs], RevInstrs) 
  when is_list(ARs) and is_list(Opts) ->
    Call = {megaco_call, Mid, ARs, Opts},
    parse_megaco(Instrs, [Call|RevInstrs]);

parse_megaco([{megaco_call, Mid, ARs, Opts}|Instrs], RevInstrs) 
  when is_binary(ARs) and is_list(Opts) ->
    Call = {megaco_call, Mid, ARs, Opts},
    parse_megaco(Instrs, [Call|RevInstrs]);

parse_megaco([{megaco_call, ARs, Opts}|Instrs], RevInstrs) 
  when is_binary(ARs) and is_list(Opts) ->
    Call = {megaco_call, ARs, Opts},
    parse_megaco(Instrs, [Call|RevInstrs]);

parse_megaco([{megaco_cast, ARs, Opts}|Instrs], RevInstrs) 
  when is_list(ARs) and is_list(Opts) ->
    Cast = {megaco_cast, ARs, Opts},
    parse_megaco(Instrs, [Cast|RevInstrs]);

parse_megaco([{megaco_cast, Mid, ARs, Opts}|Instrs], RevInstrs) 
  when is_list(ARs) and is_list(Opts) ->
    Cast = {megaco_cast, Mid, ARs, Opts},
    parse_megaco(Instrs, [Cast|RevInstrs]);

parse_megaco([{megaco_cast, ARs, Opts}|Instrs], RevInstrs) 
  when is_binary(ARs) and is_list(Opts) ->
    Cast = {megaco_cast, ARs, Opts},
    parse_megaco(Instrs, [Cast|RevInstrs]);

parse_megaco([{megaco_cast, Mid, ARs, Opts}|Instrs], RevInstrs) 
  when is_binary(ARs) and is_list(Opts) ->
    Cast = {megaco_cast, Mid, ARs, Opts},
    parse_megaco(Instrs, [Cast|RevInstrs]);

parse_megaco([{megaco_cancel, CH, Reason}|Instrs], RevInstrs) ->
    Cancel = {megaco_cancel, CH, Reason},
    parse_megaco(Instrs, [Cancel|RevInstrs]);

parse_megaco([{megaco_callback, nocall = Tag, Timeout}|Instrs], RevInstrs)  
  when is_integer(Timeout) ->
    C = {megaco_callback, Tag, Timeout},
    parse_megaco(Instrs, [C|RevInstrs]);

parse_megaco([{megaco_callback, Tag, Verify}|Instrs], RevInstrs)  
  when is_atom(Tag) and is_function(Verify) ->
    C = {megaco_callback, Tag, Verify},
    parse_megaco(Instrs, [C|RevInstrs]);

parse_megaco([{megaco_callback, Verifiers}|Instrs], RevInstrs)  
  when is_list(Verifiers) ->
    C = {megaco_callback, Verifiers},
    parse_megaco(Instrs, [C|RevInstrs]);

parse_megaco([Instr|_], _RevInstrs) ->
    throw({error, {invalid_instruction, Instr}}).



%%% ----------------------------------------------------------------

%% -- Megaco user callback interface --

handle_connect(CH, PV, P) ->
    Req = {handle_connect, CH, PV},
    handle_megaco_callback_call(P, Req).
	
handle_disconnect(CH, PV, R, P) ->
    Msg = {handle_disconnect, CH, PV, R},
    Reply = ok,
    handle_megaco_callback_cast(P, Msg, Reply).

handle_syntax_error(RH, PV, ED, P) ->
    Req = {handle_syntax_error, RH, PV, ED},
    handle_megaco_callback_call(P, Req).

handle_message_error(CH, PV, ED, P) ->
    Msg = {handle_message_error, CH, PV, ED},
    Reply = ok,
    handle_megaco_callback_cast(P, Msg, Reply).

handle_trans_request(CH, PV, AR, P) ->
    Req = {handle_trans_request, CH, PV, AR},
    handle_megaco_callback_call(P, Req).

handle_trans_long_request(CH, PV, RD, P) ->
    Req = {handle_trans_long_request, CH, PV, RD},
    handle_megaco_callback_call(P, Req).

handle_trans_reply(CH, PV, AR, RD, P) ->
    Msg = {handle_trans_reply, CH, PV, AR, RD},
    Reply = ok,
    handle_megaco_callback_cast(P, Msg, Reply).

handle_trans_ack(CH, PV, AS, AD, P) ->
    Msg = {handle_trans_ack, CH, PV, AS, AD},
    Reply = ok,
    handle_megaco_callback_cast(P, Msg, Reply).

handle_unexpected_trans(CH, PV, T, P) ->
    Msg = {handle_unexpected_trans, CH, PV, T},
    Reply = ok,
    handle_megaco_callback_cast(P, Msg, Reply).

handle_trans_request_abort(RH, PV, TransNo, Pid, P) ->
    Msg = {handle_trans_request_abort, RH, PV, TransNo, Pid},
    Reply = ok,
    handle_megaco_callback_cast(P, Msg, Reply).

handle_megaco_callback_cast(P, Msg, Reply) ->
    p("handle_megaco_callback_cast -> entry with Msg: ~n~p", [Msg]),
    P ! {handle_megaco_callback, cast, Msg, self()},
    Reply.

handle_megaco_callback_call(P, Msg) ->
    p("handle_megaco_callback_call -> entry with Msg: ~n~p", [Msg]),
    P ! {handle_megaco_callback, call, Msg, self()},
    receive
	{handle_megaco_callback_reply, Reply} ->
	    p("handle_megaco_callback_call -> received reply: ~n~p", [Reply]),
	    Reply;
	{handle_megaco_callback_reply, Delay, Reply} when is_integer(Delay) ->
	    p("handle_megaco_callback_call -> received reply [~w]: ~n~p", 
	      [Delay, Reply]),
	    sleep(Delay),
	    p("handle_megaco_callback_call -> deliver reply after delay [~w]", 
	      [Delay]),
	    Reply
    end.

handle_megaco_callback_reply(P, call, Reply) ->
    P ! {handle_megaco_callback_reply, Reply};
handle_megaco_callback_reply(_, _, _) ->
    ok.

handle_megaco_callback_reply(P, call, Delay, Reply) ->
    P ! {handle_megaco_callback_reply, Delay, Reply};
handle_megaco_callback_reply(_, _, _, _) ->
    ok.


    
%%% ----------------------------------------------------------------

close(#tcp{connection = Sock}) ->
    close(Sock);
close(undefined) ->	   
    ok;
close(Sock) ->
    (catch gen_tcp:close(Sock)).

add_tpkt_header(Bin) when is_binary(Bin) ->
    L = size(Bin) + 4,
    SZ1 = ((L) bsr 8) band 16#ff,
    SZ2 = (L) band 16#ff,
    <<3, 0, SZ1, SZ2, Bin/binary>>;
add_tpkt_header(IOList) when is_list(IOList) ->
    add_tpkt_header(list_to_binary(IOList)).

sleep(X) when is_integer(X), X =< 0 -> ok;
sleep(X) -> receive after X -> ok end.

sz(Bin) when is_binary(Bin) ->
    size(Bin);
sz(L) when is_list(L) ->
    lists:length(L);
sz(_) ->
    -1.

%%% ----------------------------------------------------------------

get_config(Key, Opts) ->
    {value, {Key, Val}} = lists:keysearch(Key, 1, Opts),
    Val.

%%% ----------------------------------------------------------------

d(F) ->
    d(F, []).

d(F, A) ->
    d(true, F, A).

d(true, F, A) ->
    p(" DBG", F, A);
d(_, _, _) ->
    ok.

e(F, A) ->
    p(" ERROR", F, A).

p(F) ->
    p("", F, []).

p(F, A) ->
    p("", F, A).

p(P, F, A) ->
    p(P, get(name), F, A).

p([], undefined, F, A) ->
    io:format("*** [~s] ~p *** " ++ 
	      "~n   " ++ F ++ "~n", 
	      [format_timestamp(now()),self()|A]);
p(P, undefined, F, A) ->
    io:format("*** [~s] ~p ~s *** " ++ 
	      "~n   " ++ F ++ "~n", 
	      [format_timestamp(now()),self(),P|A]);
p(P, N, F, A) ->
    io:format("*** [~s] ~p ~s~s *** " ++ 
	      "~n   " ++ F ++ "~n", 
	      [format_timestamp(now()),self(),N,P|A]).


format_timestamp({_N1, _N2, N3} = Now) ->
    {Date, Time}   = calendar:now_to_datetime(Now),
    {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    FormatDate = 
        io_lib:format("~.4w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w 4~w",
                      [YYYY,MM,DD,Hour,Min,Sec,round(N3/1000)]),  
    lists:flatten(FormatDate).
