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

%%% Purpose : SSL broker

-module(ssl_broker).
-behaviour(gen_server).

%% This module implements brokers for ssl. A broker is either a connector, 
%% an acceptor, or a listener. All brokers are children to ssl_broker_sup,
%% to which they are linked. Each broker is also linked to ssl_server, and
%% to its client.
%%
%% The purpose of the broker is to set up SSL connections through calls to
%% ssl_server and gen_tcp. All control information goes to the server,
%% while all data is exchanged directly between gen_tcp and the port program
%% of the ssl_server.
%%
%% A broker is created by a call to start_broker/3 (do *not* use start_link/4
%% - it is for ssl_broker_sup to call that one), and then call listen/3, 
%% accept/4, or connect/5. 
%%
%% The following table shows all functions dependency on status, version
%% of i/f, active mode etc.
%%
%% Permitted status transitions: 
%%
%%		nil	->	open 
%%		open	->	closing/closed
%%		closing	->	closed
%%
%% We are rather sloppy about nil, and consider open/closing == !closed,
%% open/closing/closed === any  etc.
%%
%%
%%	function/	valid		for	mode	output	new
%%	message		status		i/f		depends	state
%%							  on
%%==============================================================
%%	calls
%%	-----
%%	recv		open/closing	new	passive	-	ditto/closed
%%	send		open		new	any	-	ditto/closing
%%	accept		nil		any	any	-	open
%%	connect		nil		any	any	-	open
%%	listen		nil		any	any	-	open
%%	peername	open		any	any	-	ditto
%%	setopts		open		new	any	-	ditto
%%	getopts		open		new	any	-	ditto
%%	sockname	open		any	any	-	ditto
%%	inhibit		any		any	any	-	ditto
%%	release		any		any	any	-	ditto
%%
%%	casts
%%	-----
%%	close_client	any		new	any	-	closed	(1)
%%
%%	info
%%	----
%%	tcp		open/closing	any	active	i/f	ditto
%%	tcp_closed	open/closing	any	active	i/f	closed
%%	tcp_error	open/closing	any	active	i/f	closed
%%
%%	deliver		open		old	active	-	open/closing
%%
%%	close_server	open/cl'g/cl'd	any	any	-	closing/closed
%%
%%	client_close	open/cl'g/cl'd	old	any	-	closed	(1)
%%
%%
%%	(1) We terminate here.
%%
%% TODO
%%
%% XXX Timeouts are not checked (integer or infinity).
%%
%% XXX The collector thing is not gen_server compliant.
%%
%% XXX Remove everything that has to do with the old i/f.
%%
%% NOTE: There are two different "modes": (a) passive or active mode,
%% specified as {active, bool()}, and (b) list or binary mode, specified 
%% as {mode, list | binary}.
%%

-include("ssl_int.hrl").

%% External exports 

-export([start_broker/1, start_broker/2, start_broker/3, start_link/4,
	 accept/4, close/1, connect/5, controlling_process/2,
	 listen/3, recv/3, send/2, getopts/2, getopts/3, setopts/2,
	 sockname/1, peername/1]).

-export([listen_prim/5, connect_prim/8, accept_prim/8]).

%% Internal exports 

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2, collector_init/1]).

-include("ssl_broker_int.hrl").

%% start_broker(Type) -> {ok, Pid} | {error, Reason}
%% start_broker(Type, NewIf) -> {ok, Pid} | {error, Reason}
%% start_broker(Type, NewIf, GenOpts) -> {ok, Pid} | {error, Reason}
%%            Type = accept | connect | listen
%%            NewIf = true | false
%%            GenOpts = /standard gen_server options/
%%
%% This is the function to be called from interface modules (ssl.erl and 
%% ssl_socket.erl). The new interface (ssl.erl) is used by default. Links
%% to the caller.
%%
start_broker(Type) ->
    start_broker(Type, true).

start_broker(Type, NewIf) ->
    start_broker(Type, NewIf, []).

start_broker(Type, NewIf, GenOpts) ->
    case lists:member(Type, [listener, acceptor, connector]) of
	true ->
	    case supervisor:start_child(ssl_broker_sup, 
					[self(), Type, NewIf, GenOpts]) of
		{ok, Pid} ->
		    link(Pid),
		    {ok, Pid};
		{error, Reason} ->
		    {error, Reason}
	    end;
	false  ->
	    {error, ebrokertype}
    end.

%% start_link(Client, Type, NewIf, GenOpts) -> {ok, Pid} | {error, Reason}
%%	      
%%	Type = accept | connect | listen
%%	NewIf = true | false
%%	GenOpts = /standard gen_server options/
%%
%% This function is called by ssl_broker_sup and must *not* be called
%% from an interface module (ssl.erl or ssl_socket.erl).

start_link(Client, Type, NewIf, GenOpts) ->
    gen_server:start_link(?MODULE, [Client, Type, NewIf], GenOpts).


%% accept(Pid, ListenSocket, Opts, Timeout) -> {ok, Socket} | 
%% 						  {error, Reason}
%%  
%% Types:   Pid = pid() of acceptor
%%          ListenSocket = Socket = sslsocket()
%%          Opts = [{flags, Flags}] | []
%% 	    Timeout = integer() | infinity
%%
%% Opts should be empty for the new i/f. 
%%
accept(Pid, ListenSocket, Opts, Timeout) 
  when pid(Pid), record(ListenSocket, sslsocket), list(Opts) ->
    case are_accept_opts(Opts) of
	true ->
	    Req = {accept, self(), ListenSocket, Opts, Timeout},
	    gen_server:call(Pid, Req, infinity);
	false  ->
	    {error, eoptions}
    end.

%% close(Socket) -> ok | {error, Reason}
%%  
%% Types:   Socket = sslsocket() | pid()
%%
close(Socket) when record(Socket, sslsocket) ->
    close(Socket#sslsocket.pid);
close(Pid) when pid(Pid) ->
    gen_server:call(Pid, {close, self()}, infinity).

%% connect(Pid, Address, Port, Opts, Timeout) -> {ok, Socket} | 
%%						    {error, Reason}
%%  
%% Types:   Pid = pid() of connector
%%          Address  = string() | {byte(), byte(), byte(), byte()}
%%          Port = int()
%%          Opts = options()
%% 	    Timeout = integer() | infinity
%%          Socket = sslsocket()
%%
connect(Pid, Address, Port, Opts, Timeout) when pid(Pid), list(Opts) ->
    case are_connect_opts(Opts) of
	true ->
	    Req = {connect, self(), Address, Port, Opts, Timeout},
	    gen_server:call(Pid, Req, infinity);
	false  ->
	    {error, eoptions}
    end.

%% controlling_process(Socket, NewOwner) -> ok | {error, Reason}

controlling_process(Socket, NewOwner) when record(Socket, sslsocket) ->
    controlling_process(Socket#sslsocket.pid, NewOwner);
controlling_process(Pid, NewOwner) when pid(Pid), pid(NewOwner) ->
    case gen_server:call(Pid, {inhibit_msgs, self()}, infinity) of
	ok ->
	    transfer_messages(Pid, NewOwner),
	    gen_server:call(Pid, {release_msgs, self(), NewOwner}, infinity);
	Error ->
	    Error
    end.
    
%% listen(Pid, Port, Opts) -> {ok, ListenSocket} | {error, Reason}
%%  
%% Types:   Pid = pid() of listener
%%          Port = int()
%%          Opts = options()
%%          ListenSocket = sslsocket()
%%
listen(Pid, Port, Opts) when pid(Pid) ->
    case are_listen_opts(Opts) of
	true ->
	    Req = {listen, self(), Port, Opts}, 
	    gen_server:call(Pid, Req, infinity);
	false  ->
	    {error, eoptions}
    end.


%%
%% peername(Socket) -> {ok, {Address, Port}} | {error, Reason}
%%
peername(Socket) when record(Socket, sslsocket) ->
    peername(Socket#sslsocket.pid);
peername(Pid) when pid(Pid) ->			% old i/f
    Req = {peername, self()},
    gen_server:call(Pid, Req, infinity).


%% recv(Socket, Length, Timeout) -> {ok, Data} | {error, Reason}
%%
%% Types:   Socket = sslsocket()
%%          Length = Timeout = integer()
%%          Data = bytes() | binary()
%%
recv(Socket, Length, Timeout) when record(Socket, sslsocket) ->
    Req = {recv, self(), Length, Timeout}, 
    gen_server:call(Socket#sslsocket.pid, Req, infinity).


%% send(Socket, Data) -> ok | {error, Reason}
%%  
%% Types:   Socket = sslsocket()
%%
send(Socket, Data) when record(Socket, sslsocket) ->
    gen_server:call(Socket#sslsocket.pid, {send, self(), Data}, infinity).


%% getopts(Socket, OptTags) -> {ok, Opts} | {error, einval}
%%  
%% Types:	Pid = pid() of broker
%%  		Timeout = integer() >= 0 | infinity
%%		OptTags = option_tags()
%%		Opts = options()
%%
getopts(Socket, OptTags) ->
    getopts(Socket, OptTags, infinity).

getopts(Socket, OptTags, Timeout) when record(Socket, sslsocket), 
				       list(OptTags) ->
    Req = {getopts, self(), OptTags}, 
    gen_server:call(Socket#sslsocket.pid, Req, Timeout).


%%
%% setopts(Socket, Opts) -> ok | {error, Reason}
%%
setopts(Socket, Opts) when record(Socket, sslsocket) ->
    Req = {setopts, self(), Opts},
    gen_server:call(Socket#sslsocket.pid, Req, infinity).

%%
%% sockname(Socket) -> {ok, {Address, Port}} | {error, Reason}
%%
sockname(Socket) when record(Socket, sslsocket) ->
    sockname(Socket#sslsocket.pid);
sockname(Pid) when pid(Pid) ->			% old i/f
    Req = {sockname, self()},
    gen_server:call(Pid, Req, infinity).


%%
%%  INIT
%%

%% init
%%
init([Client, Type, NewIf]) ->
    process_flag(trap_exit, true),
    link(Client),
    Debug = case application:get_env(ssl, edebug) of
		{ok, true} -> 
		    true;
		_ ->
		    case application:get_env(ssl, debug) of
			{ok, true} ->
			    true;
			_  ->
			    false
		    end
	    end,
    Server = whereis(ssl_server),
    if 
	pid(Server) ->
	    link(Server),
	    debug1(Debug, Type, "in start, client = ~w, newif = ~w\n", 
		   [Client, NewIf]),
	    {ok, #st{brokertype = Type, server = Server, client = Client,
		     collector = Client, newif = NewIf, debug = Debug}};
	true  ->
	    {stop, no_ssl_server}
    end.


%%
%% HANDLE CALL
%%

%% recv - passive mode
%%
handle_call({recv, Client, Length, Timeout}, _From, St) 
  when St#st.status =/= closed, St#st.active == false ->
    debug(St, "recv: client = ~w~n", [Client]),
    Reply = gen_tcp:recv(St#st.proxysock, Length, Timeout),
    {reply, Reply, St};


%% send - new i/f
%% 
handle_call({send, Client, Data}, _From, St) ->
    debug(St, "send: client = ~w~n", [Client]),
    if 
	St#st.status =/= open ->
 	    {reply, {error, closed}, St};
	true ->
	    case gen_tcp:send(St#st.proxysock, Data) of
		ok ->
		    {reply, ok, St};
		{error, _Reason} ->
		    {reply, {error, closed}, St#st{status = closing}}
	    end
    end;


%% accept 
%% 
%% Client = pid of client 
%% ListenSocket = sslsocket()
%%
%% The new interface must have Opts = [], and the old interface 
%% Opts = [{flags, Flags}] only. The rest of the options is fetched from 
%% the process owning the ListenSocket.
%%
handle_call({accept, Client, ListenSocket, Opts, Timeout}, _From, St) ->
    debug(St, "accept: client = ~w, listensocket = ~w~n", 
	  [Client, ListenSocket]),
    case getopts(ListenSocket, tcp_listen_opt_tags(), ?DEF_TIMEOUT) of 
	{ok, LOpts} ->
	    case accept_prim(ssl_server, gen_tcp, Client, 
			     ListenSocket#sslsocket.fd, LOpts, Opts, 
			     Timeout, St) of
		{ok, ThisSocket, NSt} ->
		    {reply, {ok, ThisSocket}, NSt};
		{error, Reason, St} ->
		    What = what(Reason),
		    {stop, normal, {error, What}, St}
	    end;
	{error, Reason} ->
	    What = what(Reason),
	    {stop, normal, {error, What}, St}
    end;

%% connect
%%
%% Client = client pid
%% Address = hostname | ipstring | IP
%% Port = integer()
%% Opts = options()
%%
handle_call({connect, Client, Address, Port, Opts, Timeout}, _From, St) ->
    debug(St, "connect: client = ~w, address = ~p, port = ~w~n",
	  [Client, Address, Port]),
    case connect_prim(ssl_server, gen_tcp, Client, Address, Port, Opts, 
		      Timeout, St) of
	{ok, Res, NSt} ->
	    {reply, {ok, Res}, NSt};
	{error, Reason, NSt} ->
	    What = what(Reason),
	    {stop, normal, {error, What}, NSt}
    end;


%% close from client - new i/f
%%
handle_call({close, Client}, _From, St) ->
    debug(St, "close: client = ~w~n", [Client]),
    if
	integer(St#st.fd) ->
	    ssl_server:close(St#st.fd)
    end,
    {stop, normal, ok, St};

%%  listen
%% 
%%  Client = pid of client
%%  Port = int()
%%  Opts = options()
%%
handle_call({listen, Client, Port, Opts}, _From, St) ->
    debug(St, "listen: client = ~w, port = ~w~n",
	  [Client, Port]),
    case listen_prim(ssl_server, Client, Port, Opts, St) of
	{ok, Res, NSt} ->
	    {reply, {ok, Res}, NSt};
	{error, Reason, NSt} ->
	    What = what(Reason),
	    {stop, normal, {error, What}, NSt}
    end;

%% peername
%%
handle_call({peername, Client}, _From, St) ->
    debug(St, "peername: client = ~w~n", [Client]),
    if 
	St#st.status =/= open ->
	    {reply, {error, closed}, St};
	true ->
	    Reply = case ssl_server:peername(St#st.fd) of
			{ok, {Address, Port}} ->
			    {ok, At} = inet:getaddr(Address, inet),
			    {ok, {At, Port}};
			Error ->
			    Error
		    end,
	    {reply, Reply, St}
    end;


%% setopts
%%
handle_call({setopts, Client, _Opts}, _From, St) when St#st.status =/= open ->
    debug(St, "setopts: client = ~w~n", [Client]),
    {reply, {error, closed}, St};
handle_call({setopts, _Client, Opts0}, _From, St0) ->
    OptsOK = case St0#st.brokertype of
		 listener ->
		     are_opts(fun is_tcp_listen_opt/1, Opts0);
		 acceptor ->
		     are_opts(fun is_tcp_accept_opt/1, Opts0);
		 connector ->
		     are_opts(fun is_tcp_connect_opt/1, Opts0)
	     end,
    if 
	OptsOK == false ->
	    {reply, {error, eoptions}, St0};
	true ->
	    Opts1 = lists:keydelete(nodelay, 1, Opts0),
	    case inet:setopts(St0#st.proxysock, Opts1) of
		ok ->
		    Opts2 = replace_opts(Opts1, St0#st.opts),
		    Active = get_active(Opts2),
		    St2 = St0#st{opts = Opts2, 
				 active = Active},
		    case get_nodelay(Opts0) of
			empty ->
			    {reply, ok, St2};
			Bool ->
			    case setnodelay(ssl_server, St0, Bool) of
				ok ->
				    Opts3 = replace_opts([{nodelay, Bool}],
							 Opts2),
				    St3 = St0#st{opts = Opts3, 
						 active = Active},
				    {reply, ok, St3};
				{error, Reason} ->
				    {reply, {error, Reason}, St2}
			    end
		    end;
		{error, Reason} ->
		    {reply, {error, Reason}, St0}
	    end
    end;

%% sockname
%%
handle_call({sockname, Client}, _From, St) ->
    debug(St, "sockname: client = ~w~n", [Client]),
    if 
	St#st.status =/= open ->
	    {reply, {error, closed}, St};
	true ->
	    Reply = case ssl_server:sockname(St#st.fd) of
			{ok, {Address, Port}} ->
			    {ok, At} = inet:getaddr(Address, inet),
			    {ok, {At, Port}};
			Error ->
			    Error
		    end,
	    {reply, Reply, St}
    end;


%% inhibit msgs
%%
%%
handle_call({inhibit_msgs, Client}, _From, St) when St#st.client == Client ->
    debug(St, "inhibit_msgs: client = ~w~n", [Client]),
    {ok, Collector} = start_collector(),
    {reply, ok, St#st{collector = Collector}};


%% release msgs
%%
%%
handle_call({release_msgs, Client, NewClient}, _From, St) 
  when St#st.client == Client ->
    debug(St, "release_msgs: client = ~w~n", [Client]),
    unlink(Client),
    link(NewClient),
    release_collector(St#st.collector, NewClient),
    NSt = St#st{client = NewClient, collector = NewClient},
    {reply, ok, NSt};


%% getopts
%%
%%
handle_call({getopts, Client, OptTags}, _From, St) ->
    debug(St, "getopts: client = ~w~n", [Client]),
    Reply = case are_opt_tags(St#st.brokertype, St#st.newif, OptTags) of
		true ->
		    {ok, extract_opts(OptTags, St#st.opts)};
		_ ->
		    {error, einval}
	    end,
    {reply, Reply, St};

 
%% bad call
%% 
%%
handle_call(Request, _From, St) ->
    debug(St, "++++ ssl_broker: bad call: ~w~n", [Request]),
    {reply, {error, {badcall, Request}}, St}.

%%
%% HANDLE CAST
%%

handle_cast(Request, St) ->
    debug(St, "++++ ssl_broker: bad cast: ~w~n", [Request]),
    {stop, {error, {badcast, Request}}, St}.

%% 
%% HANDLE INFO
%%

%% tcp - active mode
%%
%% The collector is different from client only during change of
%% controlling process.
%%
handle_info({tcp, Socket, Data}, St) 
  when St#st.proxysock == Socket, St#st.status =/= closed, 
       St#st.active =/= false ->
    debug(St, "tcp: socket = ~w~n", [Socket]),
    Msg = if 
	    St#st.newif == true -> {ssl, St#st.thissock, Data};
	    true  -> {self(), {fromsocket, Data}}
	end,
    St#st.collector ! Msg,
    if
	St#st.active == once -> {noreply, St#st{active = false}};
	true -> {noreply, St}
    end;

%% tcp_closed - from proxy socket, active mode
%%
%%
handle_info({tcp_closed, Socket}, St) 
  when St#st.proxysock == Socket, St#st.status =/= closed, 
       St#st.active =/= false ->
    debug(St, "tcp_closed: socket = ~w~n", [Socket]),
    Msg = if 
	    St#st.newif == true -> {ssl_closed, St#st.thissock};
	    true -> {self(), {socket_closed, normal}}
	end,
    St#st.collector ! Msg,
    if
	St#st.active == once -> 
	    {noreply, St#st{status = closed, active = false}};
	true ->
	    {noreply, St#st{status = closed}}
    end;

%% tcp_error - from proxy socket, active mode
%%
%%
handle_info({tcp_error, Socket, Reason}, St) 
  when St#st.proxysock == Socket, St#st.status =/= closed, 
       St#st.active =/= false ->
    debug(St, "tcp_error: socket = ~w, reason = ~w~n", [Socket, Reason]),
    Msg = if 
	      St#st.newif == true -> {ssl_error, St#st.thissock, Reason};
	      true -> {self(), {socket_closed, Reason}}
	  end,
    St#st.collector ! Msg,
    if
	St#st.active == once -> 
	    {noreply, St#st{status = closed, active = false}};
	true ->
	    {noreply, St#st{status = closed}}
    end;


%% deliver - old interface (active)
%%
handle_info({Client, {deliver, Data}}, St) 
  when St#st.status == open, St#st.active == true ->
    debug(St, "deliver: client = ~w~n", [Client]),
    case gen_tcp:send(St#st.proxysock, Data) of
	ok ->
	    {noreply, St};
	{error, _Reason} ->
	    {noreply, St#st{status = closing}}
    end;


%% close - from server, active and passive mode
%% 
%%
handle_info({close, Server}, St) when St#st.server == Server ->
    Status = new_close_status(St#st.status),
    debug(St, "close_server: current status = ~w, new status = ~w~n",
	  [St#st.status, Status]),
    {noreply, St#st{status = Status}};


%% close from client - old i/f (active)
%% 
handle_info({Client, close}, St) ->
    debug(St, "client close: client = ~w, new status = closed~n",
	  [Client]),
    {stop, normal, St#st{status = closed}};


%% EXIT - from client
%% 
%%
handle_info({'EXIT', Client, Reason}, St) when St#st.client == Client ->
    debug(St, "exit client: client = ~w, reason = ~w~n", [Client, Reason]),
    {stop, normal, St#st{status = closed}};	% do not make noise


%% EXIT - from server
%%
%%
handle_info({'EXIT', Server, Reason}, St) when St#st.server == Server ->
    debug(St, "exit server: reason = ~w~n", [Reason]),
    {stop, Reason, St};

%% handle info catch all
%%
%%
handle_info(Info, St) ->
    debug(St, " bad info: ~w~n", [Info]),
    {stop, {error, {badinfo, Info}}, St}.


%% terminate
%%
%% 
terminate(Reason, St) ->
    debug(St, "in terminate reason: ~w, state: ~w~n", [Reason, St]),
    ok.

%% code_change
%%
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Primitive interface
%%
listen_prim(ServerName, Client, Port, Opts, St) ->
    LOpts = get_tcp_listen_opts(Opts),
    SSLOpts = get_ssl_opts(St#st.newif, Opts),
    FlagStr = if 
		  St#st.newif == true ->
		      mk_ssl_optstr(SSLOpts);
		  true -> 
		      ""			% No SSL Opts for old i/f
	      end,
    BackLog = get_backlog(LOpts),
    IP = get_ip(LOpts),
    case ssl_server:listen_prim(ServerName, IP, Port, FlagStr, BackLog) of
	{ok, ListenFd, _Port0} ->
	    ThisSocket = #sslsocket{fd = ListenFd, pid = self()},
	    StOpts = add_default_tcp_listen_opts(LOpts) ++
		add_default_ssl_opts(St#st.newif, SSLOpts),
	    NSt = St#st{fd = ListenFd, 
			active = get_active(LOpts), % irrelevant for listen
			opts = StOpts,
			thissock = ThisSocket, 
			status = open},
	    debug(St, "listen: ok: client = ~w, listenfd = ~w~n", 
		  [Client, ListenFd]),
	    {ok, ThisSocket, NSt};
	{error, Reason} ->
	    {error, Reason, St}
    end.
    
connect_prim(ServerName, TcpModule, Client, Address, Port, Opts, 
	     Timeout, St) ->
    COpts = get_tcp_connect_opts(Opts),
    SSLOpts = get_ssl_opts(St#st.newif, Opts),
    FlagStr = if
		  St#st.newif == true -> mk_ssl_optstr(SSLOpts);
		  true -> get_flags(SSLOpts)
	      end,
    case inet:getaddr(Address, inet) of
	{ok, IP} ->
	    %% Timeout is gen_server timeout - hence catch
	    case (catch ssl_server:connect_prim(ServerName, IP, Port, 
						FlagStr, Timeout)) of
		{ok, Fd, ProxyPort} ->
		    case connect_proxy(ServerName, TcpModule, Fd, 
				       ProxyPort, COpts, Timeout) of
			{ok, Socket} ->
			    ThisSocket = #sslsocket{fd = Fd, pid = self()}, 
			    StOpts = add_default_tcp_connect_opts(COpts) ++
				add_default_ssl_opts(St#st.newif, SSLOpts),
			    NSt = St#st{fd = Fd, 
					active = get_active(COpts),
					opts = StOpts,
					thissock = ThisSocket, 
					proxysock = Socket, 
					status = open},
			    case get_nodelay(COpts) of
				true -> setnodelay(ServerName, NSt, true);
				_ -> ok
			    end,
			    debug(St, "connect: ok: client = ~w, fd = ~w~n",
				  [Client, Fd]),

			    {ok, ThisSocket, NSt};
			{error, Reason} ->
			    {error, Reason, St}
		    end;
		{'EXIT', Reason} ->
		    {error, Reason, St};
		{error, Reason} ->
		    {error, Reason, St}
	    end;
	{error, Reason} ->
	    {error, Reason, St}
    end.
    
accept_prim(ServerName, TcpModule, Client, ListenFd, LOpts, Opts, 
	    Timeout, St) -> 
    AOpts = get_tcp_accept_opts(LOpts),
    {FlagStr, SSLOpts} = 
	if
	    St#st.newif == true -> 
		{[], []}; % No SSL options in new i/f
		  true -> 
		%% yes, original Opts: 
		{get_flags(Opts), get_ssl_opts(St#st.newif, Opts)}
	end,
    %% Timeout is gen_server timeout - hence catch.
    case (catch ssl_server:accept_prim(ServerName, ListenFd, FlagStr, 
				       Timeout)) of 
	{ok, Fd, ProxyPort} ->
	    case connect_proxy(ServerName, TcpModule, Fd, 
			       ProxyPort, AOpts, Timeout) of
		{ok, Socket} ->
		    ThisSocket = #sslsocket{fd = Fd, pid = self()}, 
		    StOpts = add_default_tcp_accept_opts(AOpts) ++
			add_default_ssl_opts(St#st.newif, SSLOpts),
		    NSt = St#st{fd = Fd, 
				active = get_active(AOpts),
				opts = StOpts,
				thissock = ThisSocket,
				proxysock = Socket, 
				status = open},
		    case get_nodelay(AOpts) of
			true -> setnodelay(ServerName, NSt, true);
			_ -> ok
		    end,
		    debug(St, "accept: ok: client = ~w, fd = ~w~n",
			  [Client, Fd]),
		    {ok, ThisSocket, NSt};
		{error, Reason} ->
		    {error, Reason, St}
	    end;
	{'EXIT', Reason} ->
	    {error, Reason, St};
	{error, Reason} ->
	    {error, Reason, St}
    end.
    

%%
%% LOCAL FUNCTIONS
%%

%% 
%% connect_proxy(Fd, ProxyPort, TOpts, Timeout) -> {ok, Socket} | 
%%						   {error, Reason}
%%
connect_proxy(ServerName, TcpModule, Fd, ProxyPort, TOpts, Timeout) ->
    case TcpModule:connect({127, 0, 0, 1}, ProxyPort, TOpts, Timeout) of
	{ok, Socket} ->
	    {ok, Port} = inet:port(Socket), 
	    case ssl_server:proxy_join_prim(ServerName, Fd, Port) of
		ok ->
		    {ok, Socket};
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.


setnodelay(ServerName, St, Bool) ->
    case ssl_server:setnodelay_prim(ServerName, St#st.fd, Bool) of
	ok ->
	    case inet:setopts(St#st.proxysock, [{nodelay, Bool}]) of
		ok ->
		    ok;
		{error, Reason} ->
		    {error, Reason}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

%%
%% start_collector()
%%
%% A collector is a little process that keeps messages during change of
%% controlling process. 
%% XXX This is not gen_server compliant :-(.
%%
start_collector() ->
    Pid = spawn_link(?MODULE, collector_init, [self()]),
    {ok, Pid}.

%%
%% release_collector(Collector, NewOwner)
%%
release_collector(Collector, NewOwner) ->
    Collector ! {release, self(), NewOwner},
    receive
	%% Reap collector
	{'EXIT', Collector, normal} ->
	    ok
    end.
	
%%
%% collector_init(Broker) -> void()
%%
collector_init(Broker) ->
    receive
	{release, Broker, NewOwner} ->
	    transfer_messages(Broker, NewOwner)
    end.

%% 
%% transfer_messages(Pid, NewOwner) -> void()
%%
transfer_messages(Pid, NewOwner) ->    
    receive
	{ssl, Sock, Data} ->
	    NewOwner ! {ssl, Sock, Data},
	    transfer_messages(Pid, NewOwner);
	{Pid, {fromsocket, Data}} ->
	    NewOwner ! {Pid, {fromsocket, Data}},
	    transfer_messages(Pid, NewOwner);
	{ssl_closed, Sock} ->
	    NewOwner ! {ssl_closed, Sock},
	    transfer_messages(Pid, NewOwner);
	{ssl_error, Sock, Reason} ->
	    NewOwner ! {ssl_error, Sock, Reason},
	    transfer_messages(Pid, NewOwner);
	{Pid, {socket_closed, Reason}} ->
	    NewOwner ! {Pid, {socket_closed, Reason}},
	    transfer_messages(Pid, NewOwner)
    after 0 ->
	    ok
    end.


%%
%% new_close_status(Status) -> NewStatus
%%
%% Implements: AnyStatus -> closing -> closed (absorbing)
%% 
new_close_status(closed) -> closed;
new_close_status(closing) -> closed;
new_close_status(_Other) -> closing.


%%
%% debug(St, Format, Args) -> void() - printouts
%%
debug(St, Format, Args) ->
    debug1(St#st.debug, St#st.brokertype, Format, Args).

debug1(true, Type, Format0, Args) ->
    {_MS, S, MiS} = erlang:now(),
    Secs = S rem 100, 
    MiSecs = MiS div 1000,
    Format = "++++ ~3..0w:~3..0w ssl_broker (~w)[~w]: " ++ Format0, 
    io:format(Format, [Secs, MiSecs, self(), Type| Args]);
debug1(_, _, _, _) ->
    ok.

%%
%% what(Reason) -> What
%% 
what(Reason) when atom(Reason) ->
    Reason;
what({'EXIT', Reason}) ->
    what(Reason);
what({What, _Where}) when atom(What) ->
    What;
what(Reason) ->
    Reason.


%%
%% OPTIONS
%%
%% XXX Should keep lists of tags and valid option values together. 

%% Option tags

%% XXX Must distinguish between old and new i/f (since we are using
%% getopts/3 from acceptor to get listener opts).
 
are_opt_tags(listener, NewIf, OptTags) ->
    is_subset(OptTags, listen_opt_tags(NewIf));
are_opt_tags(acceptor, NewIf, OptTags) ->
    is_subset(OptTags, accept_opt_tags(NewIf));
are_opt_tags(connector, NewIf, OptTags) ->
    is_subset(OptTags, connect_opt_tags(NewIf)).

tcp_gen_opt_tags() ->
    % All except `reuseaddr' and `deliver'.	
    [nodelay, active, packet, mode, header].	

tcp_listen_only_opt_tags() ->
    [ip, backlog].

ssl_opt_tags(true) ->				% New i/f
    [verify, depth, certfile, password, cacertfile, ciphers, cachetimeout];
ssl_opt_tags(false) ->				% Old i/f
    [].

ssl_old_opt_tags(true) ->
    [];
ssl_old_opt_tags(false) ->
    [flags].

accept_opt_tags(NewIf) ->
    tcp_gen_opt_tags() ++ ssl_old_opt_tags(NewIf).

connect_opt_tags(NewIf) ->
    tcp_gen_opt_tags() ++ ssl_opt_tags(NewIf) ++ ssl_old_opt_tags(NewIf).

tcp_listen_opt_tags() ->				
    tcp_gen_opt_tags() ++ tcp_listen_only_opt_tags().

listen_opt_tags(NewIf) ->				
    tcp_listen_opt_tags() ++ ssl_opt_tags(NewIf).

%% Options

%%
%% are_*_opts(Opts) -> true | false
%%
are_accept_opts(Opts) ->
    are_opts(fun is_accept_opt/1, Opts).

are_connect_opts(Opts) ->
    are_opts(fun is_connect_opt/1, Opts).

are_listen_opts(Opts) ->
    are_opts(fun is_listen_opt/1, Opts).

are_opts(F, Opts) ->
    lists:all(F, transform_opts(Opts)).
    
%%
%% get_*_opts(Opts) -> Value
%%
get_tcp_accept_opts(Opts) ->
    lists:filter(fun is_tcp_accept_opt/1, transform_opts(Opts)).

get_tcp_connect_opts(Opts) ->
    lists:filter(fun is_tcp_connect_opt/1, transform_opts(Opts)).

get_tcp_listen_opts(Opts) ->
    lists:filter(fun is_tcp_listen_opt/1, transform_opts(Opts)).

get_ssl_opts(true, Opts) ->			% New i/f
    lists:filter(fun is_ssl_opt/1, transform_opts(Opts));
get_ssl_opts(false, Opts) ->			% Old i/f
    lists:filter(fun is_ssl_old_opt/1, transform_opts(Opts)).

get_flags(Opts) ->
    get_tagged_opt(flags, Opts, "").

get_active(Opts) ->
    get_tagged_opt(active, Opts, true).
    
get_backlog(Opts) ->
    get_tagged_opt(backlog, Opts, ?DEF_BACKLOG).

get_ip(Opts) ->
    get_tagged_opt(ip, Opts, {0, 0, 0, 0}).

get_nodelay(Opts) ->
    get_tagged_opt(nodelay, Opts, empty).

%%
%% add_default_*_opts(Opts) -> NOpts
%%

add_default_tcp_accept_opts(Opts) ->
    add_default_opts(Opts, default_tcp_accept_opts()).

add_default_tcp_connect_opts(Opts) ->
    add_default_opts(Opts, default_tcp_connect_opts()).

add_default_tcp_listen_opts(Opts) ->
    add_default_opts(Opts, default_tcp_listen_opts()).

add_default_ssl_opts(NewIf, Opts) ->
    add_default_opts(Opts, default_ssl_opts(NewIf)).

add_default_opts(Opts, DefOpts) ->
    TOpts = transform_opts(Opts),
    TOpts ++ [{DTag, DVal} || 
		 {DTag, DVal} <- DefOpts, not lists:keymember(DTag, 1, TOpts)].

default_tcp_accept_opts() ->
    lists:filter(fun is_tcp_accept_opt/1, default_opts()).

default_tcp_connect_opts() ->
    lists:filter(fun is_tcp_connect_opt/1, default_opts()).

default_tcp_listen_opts() ->
    lists:filter(fun is_tcp_listen_opt/1, default_opts()).

default_ssl_opts(true) ->			% New i/f
    lists:filter(fun is_ssl_opt/1, default_opts());
default_ssl_opts(false) ->			% Old i/f
    [].						

default_opts() ->
    [{mode, list}, {packet, 0}, {nodelay, false}, {active, true},
     {backlog, ?DEF_BACKLOG}, {ip, {0, 0, 0, 0}},
     {verify, 0}, {depth, 1}].


%% Transform from old to new i/f, and also from old gen_tcp options to
%% new ones. All returned options are tagged options.
%%
transform_opts(Opts) ->
    lists:flatmap(fun transform_opt/1, Opts).

transform_opt(binary) -> 		[{mode, binary}];
transform_opt(list) -> 			[{mode, list}];
transform_opt({binary_packet, N}) -> 	[{mode, binary}, {packet, N}];
transform_opt(fourbytes) -> 		[{packet, 4}];
transform_opt(twobytes) -> 		[{packet, 2}];
transform_opt(onebyte) -> 		[{packet, 1}];
transform_opt(zerobyte) -> 		[{packet, 0}];
transform_opt({packet, raw}) -> 	[{packet, 0}];
transform_opt(raw) -> 			[];
transform_opt(Opt) -> 			[Opt].

%% NOTE: The is_*_opt/1 functions must be applied on transformed options
%% only.

is_accept_opt(Opt) ->
    is_tcp_accept_opt(Opt) or is_ssl_old_opt(Opt). % Old i/f.

is_connect_opt(Opt) ->
    is_tcp_connect_opt(Opt) or is_ssl_opt(Opt) or is_ssl_old_opt(Opt).

is_listen_opt(Opt) ->
    is_tcp_listen_opt(Opt) or is_ssl_opt(Opt).

is_tcp_accept_opt(Opt) ->
    is_tcp_gen_opt(Opt).

is_tcp_connect_opt(Opt) ->
    is_tcp_gen_opt(Opt).

is_tcp_listen_opt(Opt) ->
    is_tcp_gen_opt(Opt) or is_tcp_listen_only_opt(Opt).

%% General options supported by gen_tcp: All except `reuseaddr' and
%% `deliver'.
is_tcp_gen_opt({mode, list}) -> true;
is_tcp_gen_opt({mode, binary}) -> true;
is_tcp_gen_opt({header, Sz}) when integer(Sz), 0 =< Sz -> true; 
is_tcp_gen_opt({packet, Sz}) when integer(Sz), 0 =< Sz, Sz =< 4-> true;
is_tcp_gen_opt({packet, sunrm}) -> true;
is_tcp_gen_opt({packet, asn1}) -> true;
is_tcp_gen_opt({packet, cdr}) -> true;
is_tcp_gen_opt({packet, fcgi}) -> true;
is_tcp_gen_opt({packet, line}) -> true;
is_tcp_gen_opt({packet, tpkt}) -> true;
is_tcp_gen_opt({packet, http}) -> true;
is_tcp_gen_opt({packet, httph}) -> true;
is_tcp_gen_opt({nodelay, true}) -> true;
is_tcp_gen_opt({nodelay, false}) -> true;
is_tcp_gen_opt({active, true}) -> true;
is_tcp_gen_opt({active, false}) -> true;
is_tcp_gen_opt({active, once}) -> true;
is_tcp_gen_opt(_Opt) -> false.

is_tcp_listen_only_opt({backlog, Size}) when integer(Size), 0 =< Size -> true;
is_tcp_listen_only_opt({ip, Addr}) -> is_ip_address(Addr);
is_tcp_listen_only_opt(_Opt) -> false.

%% SSL options
is_ssl_old_opt({flags, String}) -> is_string(String); 
is_ssl_old_opt(_Opt) -> false.

is_ssl_opt({verify, Code}) when 0 =< Code, Code =< 2 -> true;
is_ssl_opt({depth, Depth}) when 0 =< Depth -> true;
is_ssl_opt({certfile, String}) -> is_string(String);
is_ssl_opt({keyfile, String}) -> is_string(String);
is_ssl_opt({password, String}) -> is_string(String);
is_ssl_opt({cacertfile, String}) -> is_string(String);
is_ssl_opt({ciphers, String}) -> is_string(String);
is_ssl_opt({cachetimeout, Timeout}) when Timeout >= 0 -> true;
is_ssl_opt(_Opt) -> false.

%% Various types
is_string(String) when list(String) ->
    lists:all(fun (C) when integer(C), 0 =< C, C =< 255 -> true; 
		  (_C) -> false end, 
	      String);
is_string(_) ->
    false.

is_ip_address(Addr) when tuple(Addr), size(Addr) == 4 ->
    is_string(tuple_to_list(Addr));
is_ip_address(Addr) when list(Addr) ->
    is_string(Addr);
is_ip_address(_) ->
    false.

get_tagged_opt(Tag, Opts, Default) ->
    case lists:keysearch(Tag, 1, Opts) of
	{value, {_, Value}} ->
	    Value;
	_Other ->
	    Default
    end.

%%
%%  mk_ssl_optstr(Opts) -> string()
%%
%%  Makes a "command line" string of SSL options
%%
mk_ssl_optstr(Opts) ->
    lists:flatten(lists:map(fun mk_one_ssl_optstr/1, Opts)).
    
mk_one_ssl_optstr({verify, Code}) ->
    [" -verify ", integer_to_list(Code)];
mk_one_ssl_optstr({depth, Depth}) ->
    [" -depth ", integer_to_list(Depth)];
mk_one_ssl_optstr({certfile, String}) -> 
    [" -cert ", String];
mk_one_ssl_optstr({keyfile, String}) -> 
    [" -key ", String];
mk_one_ssl_optstr({password, String}) -> 
    [" -passw ", String];
mk_one_ssl_optstr({cacertfile, String}) ->
    [" -cacert ", String];
mk_one_ssl_optstr({ciphers, String}) -> 
    [" -cipher ", String];
mk_one_ssl_optstr({cachetimeout, Timeout}) ->
    [" -cache ", integer_to_list(Timeout)];
mk_one_ssl_optstr(_) ->
    "".

extract_opts(OptTags, Opts) ->
    lists:filter(fun ({Tag, _}) -> lists:member(Tag, OptTags) end, Opts).

replace_opts(NOpts, Opts) ->
    lists:foldl(fun({Key, Val}, Acc) -> 
			lists:keyreplace(Key, 1, Acc, {Key, Val}) 
		end,
		Opts, NOpts).

%% Misc

is_subset(A, B) ->
    [] == A -- B.

