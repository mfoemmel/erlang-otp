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
-module(snmpm_net_if).

-behaviour(gen_server).
-behaviour(snmpm_network_interface).


%% Network Interface callback functions
-export([
	 start_link/2, 
	 stop/1, 
	 send_pdu/6,

	 note_store/2, 

 	 verbosity/2
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 code_change/3, terminate/2]).

-define(SNMP_USE_V3, true).
-include("snmp_types.hrl").
-include("snmpm_atl.hrl").
-include("snmp_debug.hrl").

%% -define(VMODULE,"NET_IF").
-include("snmp_verbosity.hrl").

%% -define(SERVER, ?MODULE).

-record(state, 
	{
	  server,
	  note_store,
	  sock, 
	  mpd_state,
	  log
	 }).


-ifdef(snmp_debug).
-define(GS_START_LINK(Args),
	gen_server:start_link(?MODULE, Args, [{debug,[trace]}])).
-else.
-define(GS_START_LINK(Args),
	gen_server:start_link(?MODULE, Args, [])).
-endif.


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
start_link(Server, NoteStore) ->
    ?d("start_link -> entry with"
       "~n   Server:    ~p"
       "~n   NoteStore: ~p", [Server, NoteStore]),
    Args = [Server, NoteStore], 
    ?GS_START_LINK(Args).

stop(Pid) ->
    call(Pid, stop).

send_pdu(Pid, Pdu, Vsn, MsgData, Addr, Port) when record(Pdu, pdu) ->
    ?d("send_pdu -> entry with"
       "~n   Pid:     ~p"
       "~n   Pdu:     ~p"
       "~n   Vsn:     ~p"
       "~n   MsgData: ~p"
       "~n   Addr:    ~p"
       "~n   Port:    ~p", [Pid, Pdu, Vsn, MsgData, Addr, Port]),
    cast(Pid, {send_pdu, Pdu, Vsn, MsgData, Addr, Port}).

note_store(Pid, NoteStore) ->
    call(Pid, {note_store, NoteStore}).

verbosity(Pid, V) ->
    call(Pid, {verbosity, V}).


%%%-------------------------------------------------------------------
%%% Callback functions from gen_server
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([Server, NoteStore]) -> 
    ?d("init -> entry with"
       "~n   Server:    ~p"
       "~n   NoteStore: ~p", [Server, NoteStore]),
    case (catch do_init(Server, NoteStore)) of
	{error, Reason} ->
	    {stop, Reason};
	{ok, State} ->
	    {ok, State}
    end.
	    
do_init(Server, NoteStore) ->
    process_flag(trap_exit, true),
    %% -- Prio --

    {ok, Prio} = snmpm_config:system_info(prio),
    process_flag(priority, Prio),

    %% -- Verbosity -- 
    {ok, Verbosity} = snmpm_config:system_info(net_if_verbosity),
    put(sname,mnif),
    put(verbosity,Verbosity),
    ?vlog("starting", []),

    %% -- MPD --
    {ok, Vsns} = snmpm_config:system_info(versions),
    MpdState = snmpm_mpd:init(Vsns),

    %% -- Module dependent options --
    {ok, Opts} = snmpm_config:system_info(net_if_options),

    %% -- Socket --
    RecBuf  = get_opt(Opts, recbuf,   1024),
    BindTo  = get_opt(Opts, bind_to,  false),
    NoReuse = get_opt(Opts, no_reuse, false),
    {ok, Port} = snmpm_config:system_info(port),
    {ok, Sock} = do_open_port(Port, RecBuf, BindTo, NoReuse),

    %% -- Audit trail log ---
    {ok, ATL} = snmpm_config:system_info(audit_trail_log),
    Log = do_init_log(ATL),

    %% -- We are done ---
    State = #state{server     = Server, 
		   note_store = NoteStore, 
		   mpd_state  = MpdState,
		   sock       = Sock, 
		   log        = Log},
    ?vdebug("started", []),
    {ok, State}.


%% Open port 
do_open_port(Port, RecvSz, BindTo, NoReuse) ->
    ?vtrace("do_open_port -> entry with"
	    "~n   Port:    ~p"
	    "~n   RecvSz:  ~p"
	    "~n   BindTo:  ~p"
	    "~n   NoReuse: ~p", [Port, RecvSz, BindTo, NoReuse]),
    IpOpts1 = bind_to(BindTo),
    IpOpts2 = no_reuse(NoReuse),
    IpOpts3 = recbuf(RecvSz),
    IpOpts  = [binary | IpOpts1 ++ IpOpts2 ++ IpOpts3],
    case init:get_argument(snmpm_fd) of
	{ok, [[FdStr]]} ->
	    Fd = list_to_integer(FdStr),
	    gen_udp:open(0, [{fd, Fd}|IpOpts]);
	error ->
	    gen_udp:open(Port, IpOpts)
    end.

bind_to(true) ->
    Addr = snmpm_config:system_info(address),
    [{ip, list_to_tuple(Addr)}];
bind_to(_) ->
    [].

no_reuse(false) ->
    [{reuseaddr, true}];
no_reuse(_) ->
    [].

recbuf(default) ->
    [];
recbuf(Sz) ->
    [{recbuf, Sz}].


%% Open log
do_init_log(false) ->
    ?vtrace("do_init_log(false) -> entry", []),
    undefined;
do_init_log(true) ->
    ?vtrace("do_init_log(true) -> entry", []),
    {ok, Type}   = snmpm_config:system_info(audit_trail_log_type),
    {ok, Dir}    = snmpm_config:system_info(audit_trail_log_dir),
    {ok, Size}   = snmpm_config:system_info(audit_trail_log_size),
    {ok, Repair} = snmpm_config:system_info(audit_trail_log_repair),
    Name = ?audit_trail_log_name, 
    File = filename:absname(?audit_trail_log_file, Dir),
    case snmp_log:create(Name, File, Size, Repair) of
	{ok, Log} ->
	    {Log, Type};
	{error, Reason} ->
	    throw({error, {failed_create_audit_log, Reason}})
    end.

    
%%--------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({verbosity, Verbosity}, _From, State) ->
    ?vlog("received verbosity request", []),
    put(verbosity, Verbosity),
    {reply, ok, State};

handle_call({note_store, Pid}, _From, State) ->
    ?vlog("received new note_store: ~w", [Pid]),
    {reply, ok, State#state{note_store = Pid}};

handle_call(stop, _From, State) ->
    ?vlog("received stop request", []),
    Reply = ok,
    {stop, normal, Reply, State};

handle_call(Req, From, State) ->
    error_msg("received unknown request (from ~p): ~n~p", [Req, From]),
    {reply, {error, {invalid_request, Req}}, State}.


%%--------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({send_pdu, Pdu, Vsn, MsgData, Addr, Port}, State) ->
    ?vlog("received send_pdu message with"
	  "~n   Pdu:     ~p"
	  "~n   Vsn:     ~p"
	  "~n   MsgData: ~p"
	  "~n   Addr:    ~p"
	  "~n   Port:    ~p", [Pdu, Vsn, MsgData, Addr, Port]),
    handle_send_pdu(Pdu, Vsn, MsgData, Addr, Port, State), 
    {noreply, State};

handle_cast(Msg, State) ->
    error_msg("received unknown message: ~n~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({udp, Sock, Ip, Port, Bytes}, #state{sock = Sock} = State) ->
    ?vlog("received ~w bytes from ~p:~p [~w]", [size(Bytes), Ip, Port, Sock]),
    handle_recv_msg(Ip, Port, Bytes, State),
    {noreply, State};

handle_info(Info, State) ->
    error_msg("received unknown info: ~n~p", [Info]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    %% Close logs
    ok.


%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
 
code_change(_Vsn, S, _Extra) ->
    {ok, S}.
 
 
%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

handle_recv_msg(Addr, Port, Bytes, 
		#state{server     = Pid, 
		       note_store = NoteStore, 
		       mpd_state  = MpdState, 
		       sock       = Sock,
		       log        = Log}) ->
    Logger = logger(Log, read, Addr, Port),
    case (catch snmpm_mpd:process_msg(Bytes, snmpUDPDomain, Addr, Port, 
				      MpdState, NoteStore, Logger)) of
	%% BMK BMK BMK
	%% Do we really need message size here??
	{ok, Vsn, #pdu{type = 'inform-request'} = Pdu, _MS, ACM} ->
	    ?vtrace("received inform-request", []),
	    Pid ! {snmp_inform, Pdu, Addr, Port},
	    RePdu = make_response_pdu(Pdu),
	    case snmpm_mpd:generate_response_msg(Vsn, RePdu, ACM, Logger) of
		{ok, Msg} ->
		    udp_send(Sock, Addr, Port, Msg);
		{discarded, Reason} ->
		    ?vlog("failed generating response message:"
			  "~n   Reason: ~p", [Reason]),
		    ok
	    end;

	{ok, _Vsn, #pdu{type = report} = Pdu, _MS, _ACM} ->
	    ?vtrace("received report", []),
	    Pid ! {snmp_report, Pdu, Addr, Port};

	{ok, _Vsn, #pdu{type = 'snmpv2-trap'} = Pdu, _MS, _ACM} ->
	    ?vtrace("received snmpv2-trap", []),
	    Pid ! {snmp_trap, Pdu, Addr, Port};

	{ok, _Vsn, Trap, _MS, _ACM} when record(Trap, trappdu) ->
	    ?vtrace("received trappdu", []),
	    Pid ! {snmp_trap, Trap, Addr, Port};

	{ok, _Vsn, Pdu, _MS, _ACM} when record(Pdu, pdu) ->
	    ?vtrace("received pdu", []),
	    Pid ! {snmp_pdu, Pdu, Addr, Port};

	{discarded, _Reason} ->
	    ?vtrace("discarded", []),
	    ok;
	Error ->
	    error_msg("processing of received message failed: "
		      "~n   ~p", [Error]),
	    ok
    end.


handle_send_pdu(Pdu, Vsn, MsgData, Addr, Port, 
		#state{note_store = NoteStore, 
		       sock       = Sock, 
		       log        = Log}) ->
    Logger = logger(Log, write, Addr, Port),
    case (catch snmpm_mpd:generate_msg(Vsn, NoteStore, 
				       Pdu, MsgData, Logger)) of
	{ok, Msg} ->
	    ?vtrace("handle_send_pdu -> message generated", []),
	    udp_send(Sock, Addr, Port, Msg);	    
	{discarded, Reason} ->
	    ?vlog("~n   PDU ~p not sent due to ~p", [Pdu, Reason]),
	    %% 
	    %% BMK BMK BMK BMK BMK BMK BMK BMK 
	    %% 
	    %% We have to inform the server since the user is 
	    %% propably waiting for a reply
	    %% 
	    %% BMK BMK BMK BMK BMK BMK BMK BMK 
	    %% 
	    ok
    end.


udp_send(Sock, Addr, Port, Msg) ->
    case (catch gen_udp:send(Sock, Addr, Port, Msg)) of
	ok ->
	    ?vdebug("sent ~w bytes to ~w:~w [~w]", 
		    [sz(Msg), Addr, Port, Sock]),
	    ok;
	{error, Reason} ->
	    error_msg("failed sending message to ~p:~p: "
		      "~n   ~p",[Addr, Port, Reason]);
	Error ->
	    error_msg("failed sending message to ~p:~p: "
		      "~n   ~p",[Addr, Port, Error])
    end.

sz(B) when binary(B) ->
    size(B);
sz(L) when list(L) ->
    length(L);
sz(_) ->
    undefined.


% mk_discovery_msg('version-3', Pdu, _VsnHdr, UserName) ->
%     ScopedPDU = #scopedPdu{contextEngineID = "",
% 			   contextName = "",
% 			   data = Pdu},
%     Bytes = snmp_pdus:enc_scoped_pdu(ScopedPDU),
%     MsgID = get(msg_id),
%     put(msg_id,MsgID+1),
%     UsmSecParams = 
% 	#usmSecurityParameters{msgAuthoritativeEngineID = "",
% 			       msgAuthoritativeEngineBoots = 0,
% 			       msgAuthoritativeEngineTime = 0,
% 			       msgUserName = UserName,
% 			       msgPrivacyParameters = "",
% 			       msgAuthenticationParameters = ""},
%     SecBytes = snmp_pdus:enc_usm_security_parameters(UsmSecParams),
%     PduType = Pdu#pdu.type,
%     Hdr = #v3_hdr{msgID = MsgID, 
% 		  msgMaxSize = 1000,
% 		  msgFlags = snmp_misc:mk_msg_flags(PduType, 0),
% 		  msgSecurityModel = ?SEC_USM,
% 		  msgSecurityParameters = SecBytes},
%     Msg = #message{version = 'version-3', vsn_hdr = Hdr, data = Bytes},
%     case (catch snmp_pdus:enc_message_only(Msg)) of
% 	{'EXIT', Reason} ->
% 	    error("Encoding error. Pdu: ~w. Reason: ~w",[Pdu, Reason]),
% 	    error;
% 	L when list(L) ->
% 	    {Msg, L}
%     end;
% mk_discovery_msg(Version, Pdu, {Com, _, _, _, _}, UserName) ->
%     Msg = #message{version = Version, vsn_hdr = Com, data = Pdu},
%     case catch snmp_pdus:enc_message(Msg) of
% 	{'EXIT', Reason} ->
% 	    error("Encoding error. Pdu: ~w. Reason: ~w",[Pdu, Reason]),
% 	    error;
% 	L when list(L) -> 
% 	    {Msg, L}
%     end.


% mk_msg('version-3', Pdu, {Context, User, EngineID, CtxEngineId, SecLevel}, 
%        MsgData) ->
%     %% Code copied from snmp_mpd.erl
%     {MsgId, SecName, SecData} =
% 	if
% 	    tuple(MsgData), Pdu#pdu.type == 'get-response' ->
% 		MsgData;
% 	    true -> 
% 		Md = get(msg_id),
% 		put(msg_id, Md + 1),
% 		{Md, User, []}
% 	end,
%     ScopedPDU = #scopedPdu{contextEngineID = CtxEngineId,
% 			   contextName = Context,
% 			   data = Pdu},
%     ScopedPDUBytes = snmp_pdus:enc_scoped_pdu(ScopedPDU),

%     PduType = Pdu#pdu.type,
%     V3Hdr = #v3_hdr{msgID      = MsgId,
% 		    msgMaxSize = 1000,
% 		    msgFlags   = snmp_misc:mk_msg_flags(PduType, SecLevel),
% 		    msgSecurityModel = ?SEC_USM},
%     Message = #message{version = 'version-3', vsn_hdr = V3Hdr,
% 		       data = ScopedPDUBytes},
%     SecEngineID = case PduType of
% 		      'get-response' -> snmp_framework_mib:get_engine_id();
% 		      _ -> EngineID
% 		  end,
%     case catch snmp_usm:generate_outgoing_msg(Message, SecEngineID,
% 					      SecName, SecData, SecLevel) of
% 	{'EXIT', Reason} ->
% 	    error("Encoding error. Pdu: ~w. Reason: ~w",[Pdu, Reason]),
% 	    error;
% 	{error, Reason} ->
% 	    error("Encoding error. Pdu: ~w. Reason: ~w",[Pdu, Reason]),
% 	    error;
% 	Packet ->
% 	    Packet
%     end;
% mk_msg(Version, Pdu, {Com, _User, _EngineID, _Ctx, _SecLevel}, _SecData) ->
%     Msg = #message{version = Version, vsn_hdr = Com, data = Pdu},
%     case catch snmp_pdus:enc_message(Msg) of
% 	{'EXIT', Reason} ->
% 	    error("Encoding error. Pdu: ~w. Reason: ~w",[Pdu, Reason]),
% 	    error;
% 	B when list(B) -> 
% 	    B
%     end.


%% -------------------------------------------------------------------

make_response_pdu(#pdu{request_id = ReqId}) ->
    #pdu{type         = 'get-response', 
	 request_id   = ReqId, 
	 error_status = noError,
	 error_index  = 0, 
	 varbinds     = []}.


%% -------------------------------------------------------------------

logger(undefined, _Type, _Addr, _Port) ->
    fun(_) ->
	    ok
    end;
logger({Log, Types}, Type, Addr, Port) ->
    case lists:member(Type, Types) of
	true ->
	    fun(Msg) ->
		    snmp_log:log(Log, Msg, Addr, Port)
	    end;
	false ->
	    fun(_) ->
		    ok
	    end
    end.


%% -------------------------------------------------------------------

error_msg(F, A) ->
    error_logger:error_msg("SNMPM: " ++ F ++ "~n", A).

% info_msg(F, A) ->
%     error_logger:info_msg("SNMPM: " ++ F ++ "~n", A).


%%%-------------------------------------------------------------------

% get_opt(Key, Opts) ->
%     ?vtrace("get option ~w", [Key]),
%     snmp_misc:get_option(Key, Opts).

get_opt(Opts, Key, Def) ->
    ?vtrace("get option ~w with default ~p", [Key, Def]),
    snmp_misc:get_option(Key, Opts, Def).


%% -------------------------------------------------------------------

call(Pid, Req) ->
    call(Pid, Req, infinity).

call(Pid, Req, Timeout) ->
    gen_server:call(Pid, Req, Timeout).

cast(Pid, Msg) ->
    gen_server:cast(Pid, Msg).

