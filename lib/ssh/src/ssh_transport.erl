%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1,  (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not,  it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis,  WITHOUT WARRANTY OF ANY KIND,  either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999,  Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id$
%%
-module(ssh_transport).

-behaviour(gen_server).

-include("ssh.hrl").

-export([
         open/1,
         open/2,
         dump_config/1,
         send_msg/2,
         set_active_once/1,
         set_controlling_process/2,
         exchange_keys/1,
         close/1
        ]).

-export([
         init/1,
         handle_info/2,
         handle_cast/2,
         handle_call/3,
         terminate/2,
         code_change/3
        ]).

-export([
         parse_packet/2,
         flatlength/1,
         split_data/2
        ]).

%% The AUX_CRYPTO module could become 'crypto' I guess.
-define(AUX_CRYPTO,             ssh_crypto).

-define(RECV_TIMEOUT,           60000).
-define(CONNECT_TIMEOUT,        60000).
-define(SEND_TIMEOUT,           60000).
-define(LANGUAGE,               "en").
-define(PAYLOAD_MAXLEN,         35000).
-define(MAX_BYTES_BEFORE_REKEY, 1073741824). % 1 bsl 30.
-define(RECV_BUF_SIZE,          8192).
-define(SEND_BUF_SIZE,          8192).
-define(NODELAY,                true).

-define(SUPP_EXCH_ALGS,         [?SSH_ALG_KEX_DH_GROUP1]).
-define(SUPP_PUB_KEY_ALGS,      [?SSH_ALG_PUB_KEY_RSA,
                                 ?SSH_ALG_PUB_KEY_DSS]).
-define(SUPP_MAC_ALGS,          [?SSH_ALG_HMAC_MD5,
                                 ?SSH_ALG_HMAC_SHA1]).
-define(SUPP_CIPH_ALGS,         [?SSH_ALG_AES128_CBC,
                                 ?SSH_ALG_3DES_CBC]).
-define(SUPP_COMPR_ALGS,        ["none"]).

-define(DB_DEFAULT,
        [
         {owner_pid,      '_'},
         {socket,         '_'},
         {cipher_cache,   {0, <<>>}},
         {plain_cache,    {0, <<>>}},
         {msgs,           []},
         {msgs_to_send,   []},
         {ignore_one,     false},
         {owner_window,   0},
         {kex,            '_'},
         {session_id,     "none"},
         {iv_c2s,         '_'},
         {iv_s2c,         '_'},
         {seq_no_c2s,     <<0, 0, 0, 0>>},
         {seq_no_s2c,     <<0, 0, 0, 0>>},
         {kex_alg,        '_'},
         {pub_key_alg,    '_'},
         {cipher_c2s,     "none"},
         {cipher_s2c,     "none"},
         {mac_c2s,        "none"},
         {mac_s2c,        "none"},
         {compr_c2s,      "none"},
         {compr_s2c,      "none"},
         {enc_key_c2s,    '_'},
         {enc_key_s2c,    '_'},
         {auth_key_c2s,   '_'},
         {auth_key_s2c,   '_'},
         {bytes_sent,     0},
         {bytes_recvd,    0},
         {vsn_c,          '_'},
         {vsn_s,          '_'},
         {server_key_fun, '_'},
         {error_code,     '_'},
         {options,        []},
         {next,           '_'}
        ]).

-record(kex,
        {
          init_c =      '_',
          init_s =      '_',
          phase =       '_',
          params =      '_',
          requester =   '_'
         }).

-record(dh_params,
        {
          e =   '_',
          x =   '_'
         }).

%% In case a record is preferred to carry the state variables, this has been
%% used during development.
%%-record(ssh,
%%      {
%%        owner_pid =           '_',
%%        socket =              '_',
%%        cipher_cache =        {0, <<>>},
%%        plain_cache =         {0, <<>>},
%%        msgs =                [],
%%        msgs_to_send =        [],
%%        ignore_one =          false,
%%        owner_window =        '_',
%%        kex =                 '_',
%%        session_id =          "none",
%%        iv_c2s =              '_',
%%        iv_s2c =              '_',
%%        seq_no_c2s =  <<0, 0, 0, 0>>,
%%        seq_no_s2c =  <<0, 0, 0, 0>>,
%%        kex_alg =             '_',
%%        pub_key_alg =         '_',
%%        cipher_c2s =          "none",
%%        cipher_s2c =          "none",
%%        mac_c2s =             "none",
%%        mac_s2c =             "none",
%%        compr_c2s =           "none",
%%        compr_s2c =           "none",
%%        enc_key_c2s =         '_',
%%        enc_key_s2c =         '_',
%%        auth_key_c2s =        '_',
%%        auth_key_s2c =        '_',
%%        bytes_sent =  0,
%%        bytes_recvd = 0,
%%        vsn_c =               '_',
%%        vsn_s =               '_',
%%        server_key_fun =      '_',
%%        error_code =          '_',
%%        options =             [],
%%        next =                '_'
%%       }).

%%%----------------------------------------------------------------------
%%% #             open/1
%%%    
%%% Input:        Host : ip_address()
%%% Output:       {error, Reason} | {ok, ssh_transport()}
%%%
%%% Description:  Connects to ssh service on Host.
%%%----------------------------------------------------------------------

open(Host) ->
    open(Host, [{port, ?SSH_PORT}, {verifun, fun(_, _) -> true end}]).

%%%----------------------------------------------------------------------
%%% #             open/2
%%%    
%%% Input:      Host    : ip_address()
%%%             Options : list() of option()
%%%
%%%             option() = {verifun, verifun()} |
%%%                        {port, integer()} |
%%%                        {exch_alg, exch_algs()} |
%%%                        {pub_key_alg, pub_key_algs()} |
%%%                        {cipher_c2s, ciph_algs()} |
%%%                        {cipher_s2c, ciph_algs()} |
%%%                        {mac_c2s, mac_algs()} |
%%%                        {mac_s2c, mac_algs()} |
%%%                        {compr_c2s, compr_algs()} |
%%%                        {compr_s2c, compr_algs()}
%%%
%%%             verifun() = fun(pub_key_alg(), Fingerprint) -> true | false
%%%
%%%             exch_algs() = [exch_alg() | exch_algs()] | []
%%%             exch_alg() = "diffie-hellman-group1-sha1"
%%%
%%%             pub_key_algs() = [pub_key_alg() | pub_key_algs()] | []
%%%             pub_key_alg() = "ssh-dss" | "ssh-rsa"
%%%
%%%             ciph_algs() = [ciph_alg() | ciph_algs()] | []
%%%             ciph_alg() = "3des-cbc" | "aes128-cbc"
%%%
%%%             mac_algs() = [mac_alg() | mac_algs()] | []
%%%             mac_alg() = "hmac-md5" | "hmac-sha1"
%%%
%%%             compr_algs() = [compr_alg() | compr_algs()] | []
%%%             compr_alg() = "none"
%%%
%%% Output:       {error, Reason} | {ok, ssh_transport()}
%%%
%%% Description:  Connects to ssh service on Host. Default options are
%%%               + Accept any host key
%%%                 {verifun, fun(_, _) -> true end}
%%%               + Connect to port 22.
%%%                 {port, 22}
%%%               + Accept Diffie-Hellman key exchange over hard-coded group.
%%%                 {exch_alg, ["diffie-hellman-group1-sha1"]}
%%%               + Prefer RSA host key, but accept DSS host key
%%%                 {pub_key_alg, ["ssh-rsa", "ssh-dss"]}
%%%               + Prefer MD5 HMAC, but also accept SHA1 HMAC.
%%%                 (xxx_c2s: client -> server, xxx_s2c: server -> client)
%%%                 {mac_c2s, ["hmac-md5", "hmac-sha1"]}
%%%                 {mac_s2c, ["hmac-md5", "hmac-sha1"]}
%%%               + Accept no compression.
%%%                 {compr_c2s, ["none"]}
%%%                 {compr_s2c, ["none"]}
%%%
%%%----------------------------------------------------------------------

open(Host, Options) ->
    {ok, Pid} = gen_server:start_link(?MODULE, ok, []),
    ServerKeyFun =
        case lists:keysearch(verifun, 1, Options) of
            {value, {verifun, Fun}} ->
                Fun;
            false ->
                fun(_, _) -> true end
        end,
    Port =
        case lists:keysearch(port, 1, Options) of
            {value, {port, P}} ->
                P;
            false ->
                ?SSH_PORT
        end,
    RestOpts =
        lists:filter(fun({OptionName, _})
                        when OptionName /= verifun,
			OptionName /= port ->
                             true;
                        (_) ->
                             false
                     end,
                     Options),
    PostOpts = {post_init, Host, Port, ServerKeyFun, RestOpts, self()},
    case gen_server:call(Pid, PostOpts, ?CONNECT_TIMEOUT) of
        ok ->
            case exchange_keys(Pid) of
                ok    -> {ok, Pid};
                Other -> Other
            end;
        Other ->
            Other
    end.

%%%----------------------------------------------------------------------
%%% #             send_msg/2
%%%    
%%% Input:      Ssh     : ssh_transport()
%%%             Message : binary() | list()
%%%
%%% Output:       {error, Reason} | ok
%%%
%%% Description:  Sends Message to the peer as an SSH Transport message.
%%%               Size of Message should be less than 33 kbytes.
%%%
%%%----------------------------------------------------------------------

send_msg(Ssh, {Size, Data}) ->
    gen_server:call(Ssh, {send_msg, {Size, Data}}, ?SEND_TIMEOUT);
send_msg(Ssh, Data) ->
    send_msg(Ssh, {flatlength(Data), Data}).

%%%----------------------------------------------------------------------
%%% #             set_active_once/1
%%%    
%%% Input:      Ssh     : ssh_transport()
%%%
%%% Output:       ok.
%%%
%%% Description:  Evaluating this will cause one message to be sent to
%%%               the caller.
%%%
%%%----------------------------------------------------------------------

set_active_once(Ssh) ->
    gen_server:cast(Ssh, {set_active_once, self()}).

%%%----------------------------------------------------------------------
%%% #             set_controlling_process/1
%%%    
%%% Input:      Ssh     : ssh_transport()
%%%             Pid     : pid()
%%%
%%% Output:       {error, not_owner} | ok.
%%%
%%% Description:  The controlling process will be able to receive messages
%%%               from the ssh transport.
%%%
%%%----------------------------------------------------------------------

set_controlling_process(Ssh, Pid) ->
    gen_server:call(Ssh, {set_controlling_process, self(), Pid}).

%%%----------------------------------------------------------------------
%%% #             close/1
%%%    
%%% Input:      Ssh     : ssh_transport()
%%%
%%% Output:       ok.
%%%
%%% Description:  Terminates the SSH transport with DISCONNECT_BY_APPLICATION.
%%%
%%%----------------------------------------------------------------------

close(Ssh) ->
    gen_server:cast(Ssh, close).

%%%----------------------------------------------------------------------
%%% #             exchange_keys/1
%%%    
%%% Input:      Ssh     : ssh_transport()
%%%
%%% Output:       ok.
%%%
%%% Description:  Forces a reexchange of transport keys.
%%%
%%%----------------------------------------------------------------------

exchange_keys(Ssh) ->
    gen_server:call(Ssh, exchange_keys, ?CONNECT_TIMEOUT).

dump_config(Ssh) ->
    gen_server:cast(Ssh, dump_config).

%%%----------------------------------------------------------------------
%%% #    init/1, handle_call/3, handle_cast/2, handle_info/2, ...
%%%    
%%% Input:
%%%
%%% Output:
%%%
%%% Description: gen_server callbacks.
%%%
%%%----------------------------------------------------------------------

init(_) ->
    process_flag(trap_exit, true),
    {ok, db_new()}.

handle_call({post_init, Host, Port, ServerKeyFun, Options, CtrlPid}, From, State) ->
    ConOpts = [binary, 
	       {packet, line}, 
	       {active, false},
	       {recbuf, ?RECV_BUF_SIZE}, 
	       {sndbuf, ?SEND_BUF_SIZE},
	       {nodelay, ?NODELAY}],
    case gen_tcp:connect(Host, Port, ConOpts, ?CONNECT_TIMEOUT) of
        {ok, Sock} ->
	    NewOpts = [{owner_pid, CtrlPid},
		       {socket, Sock},
		       {options, Options},
		       {server_key_fun, ServerKeyFun}],
	    NewState = db_insert(State, NewOpts),
            case send_version(NewState) of
                {ok, NewState1} ->
                    {reply, ok, NewState1};
                Other ->
                    {stop, Other, NewState}
            end;
        {error, Reason} ->
            {stop, normal, {error, {sockconnect, Reason}}, State}
    end;
handle_call({set_controlling_process, Old, New}, From, State) ->
    case db_lookup(State, owner_pid) of
        Old ->
            {reply, ok, db_insert(State, {owner_pid, New})};
        _ ->
            {reply, {error, not_owner}}
    end;
handle_call({send_msg, Data}, From, State) ->
    handle_send_msg(Data, From, State);
handle_call(exchange_keys, From, State) ->
    handle_exchange_keys(From, State).

handle_cast(close, State) ->
    handle_close(State);
handle_cast(more_data, State) ->
    handle_more(State);
handle_cast({set_active_once, Pid}, State) ->
    case db_lookup(State, owner_pid) of
        Pid ->
            handle_set_active_once(State);
        _ ->
            Pid ! {ssh_transport_error, self(), not_owner},
            {noreply, State}
    end;
handle_cast(dump_config, State) ->
    dump_config_imp(State),
    {noreply, State}.

handle_info({tcp, Sock, Data}, State) ->
    handle_data_from_socket(Data, State);
handle_info({tcp_closed, Sock}, State) ->
    handle_tcp_closed(State);
handle_info({tcp_error, Sock, Reason}, State) ->
    gen_tcp:close(Sock),
    {stop, {tcp_error, Reason}, State};
handle_info(timeout, State) ->
    handle_timeout(State);
handle_info(Other, State) ->
    {stop, Other, State}.

terminate(normal, State) ->
    case db_lookup(State, error_code) of
        '_' ->
            ok;
        {Code, Description} ->
            disconnect(State, Code, Description)
    end,
    ok;
terminate(tcp_closed, State) ->
    ok;
terminate({server_disconnected, How}, State) ->
    ok;
terminate({Type, Description}, State) ->
    disconnect(State, Type, Description),
    ok;
terminate(Reason, State) ->
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% #             send_version/1
%%%    
%%% Input:
%%% Output:
%%%
%%% Description:  Sends version string.
%%%----------------------------------------------------------------------

send_version(State) ->
    Sock = db_lookup(State, socket),
    case gen_tcp:send(Sock, [?SSH_VERSION_STRING, ?SSH_LINE_TERM]) of
        ok ->
            receive_version(State);
        {error, Reason} ->
            {error, {sendversion, Reason}}
    end.

%%%----------------------------------------------------------------------
%%% #           receive_version/1
%%%    
%%% Input:
%%% Output:     {ok, State} | {error, Reason}
%%%
%%% Description:  Receives and checks peer's version string for compatibility.
%%%----------------------------------------------------------------------

receive_version(State) ->
    Sock = db_lookup(State, socket),
    case get_version(Sock) of
        {ok, Vsn, VsnString} ->
            case lists:member(Vsn, ?SSH_COMPATIBLE_VERSIONS) of
                true ->
                    inet:setopts(Sock, [{packet, raw}, {active, once}]),
		    NewVersions = [{vsn_s, ?SSH_STRING(VsnString)},
				   {vsn_c, ?SSH_STRING(?SSH_VERSION_STRING)}],
                    {ok, db_insert(State, NewVersions)};
                false ->
                    {error, incompatible}
            end;
        {error, Reason} ->
            {error, {readversion, Reason}}
    end.

handle_send_msg(Msg, From, State) ->
    case db_lookup(State, kex) of
        '_' ->
            %% We are not currently performing key exchange, so simply send
            %% the message:
            case send(State, Msg) of
                {ok, NewState} ->
                    case db_lookup(NewState, bytes_sent) of
                        Sent when Sent >= ?MAX_BYTES_BEFORE_REKEY ->
                            %% Time to initiate key re-exchange. requester
                            %% is different from '_' if key re-exchange is
                            %% initiated by user.
                            gen_server:reply(From, ok),
			    NewKex = {kex, #kex{requester = '_'}},
			    kex_init(db_insert(NewState, NewKex));
                        _ ->
                            {reply, ok, NewState}
                    end;
                Other ->
		    NewError = {error_code,
				{?SSH_DISCONNECT_CONNECTION_LOST,
				 lists:flatten(io_lib:format("~p", [Other]))}},
		    Res = db_insert(State,NewError),
		    {stop, normal, Other, Res}
            end;
        _ ->
            %% We are currently exchanging keys, so put message in queue.
	    OldMsgs = db_lookup(State, msgs_to_send),
	    NewMsgs = {msgs_to_send, OldMsgs ++ [{Msg, From}]},
	    {noreply, db_insert(State, NewMsgs)}
    end.

send_msgs_to_send(State) ->
    case db_lookup(State, msgs_to_send) of
        [] ->
            {ok, State};
        [{Msg, From}|MsgsToSend] ->
            case send(State, Msg) of
                {ok, NewState} ->
                    gen_server:reply(From, ok),
		    Msgs = db_insert(NewState, {msgs_to_send, MsgsToSend}),
                    send_msgs_to_send(Msgs);
                Other ->
                    Other
            end
    end.

handle_set_active_once(State) ->
    case db_lookup(State, [owner_pid, msgs, owner_window]) of
        [Pid, [], Count] ->
            {noreply, db_insert(State, {owner_window, Count + 1})};
        [Pid, [{error, closed}], _] ->
            Pid ! {ssh_transport_error, self(), closed},
            {stop, normal, State};
        [Pid, Msgs, Count] -> %% Will this ever happen?
            {NewCount, NewMsgs} = deliver_msgs(Count + 1, Pid, Msgs),
            {noreply, db_insert(State, [{msgs, NewMsgs},
					{owner_window, NewCount}])}
    end.

handle_close(State) ->
    gen_tcp:close(db_lookup(State, socket)),
    %% At least one server implementation (Foxit WAC) reports a
    %% SSH_DISCONNECT_BY_APPLICATION as a fatal error.
						%    disconnect(State, ?SSH_DISCONNECT_BY_APPLICATION,
						%              "Stopped."),
    {stop, normal, State}.

handle_tcp_closed(State) ->
    case db_lookup(State, [owner_window, owner_pid]) of
        [0, _] ->
            case db_lookup(State, msgs) of
                [] ->
                    {stop, tcp_closed, State};
                Msgs ->
                    {noreply, db_insert(
                                State,
                                {msgs, Msgs ++ [{error, closed}]})}
            end;
        [Count, Pid] ->
            case db_lookup(State, msgs) of
                [] ->
                    Pid ! {ssh_transport_error, self(), closed},
                    {stop, normal, State};
                Msgs ->
                    {NewCount, NewMsgs} =
                        deliver_msgs(Count, Pid, Msgs),
                    {noreply, db_insert(
				State,
				[{owner_window, NewCount},
				 {msgs, NewMsgs ++ [{error, closed}]}])}
            end
    end.

handle_data_from_socket(Data, State) ->
    {OldSize, OldData} = db_lookup(State, cipher_cache),
    AddSize = flatlength(Data),
    incoming(db_insert(State, {cipher_cache,
			       {OldSize + AddSize, [OldData, Data]}})).

handle_more(State) ->
    incoming(State).

handle_timeout(State) ->
    {noreply, State}.

handle_disconnect_msg(Msg, State) ->
    case parse_packet(Msg) of
        {?SSH_MSG_DISCONNECT,
         [Code, Description, _]} ->
            {stop, {server_disconnected, Code, Description}, State};
        Other ->
            {stop, {server_disconnected, silently}, State}
    end.


handle_exchange_keys(From, State) ->
    case db_lookup(State, kex) of
        '_' ->
            %% Let's initiate a key exchange.
            kex_init(db_insert(State, {kex, #kex{requester = From}}));
        Kex ->
            %% Seems a key exchange is already in progress.
            {noreply, db_insert(State, {kex, Kex#kex{requester = From}})}
    end.

kex_init(State) ->
    [Kex, Options] = db_lookup(State, [kex, options]),
    Cookie = ?AUX_CRYPTO:rand_bytes(16),
    MyKexInit =
        [?SSH_MSG_KEXINIT, Cookie,
         algorithm_proposal(true, ?SUPP_EXCH_ALGS, exch_alg, Options),
         algorithm_proposal(true, ?SUPP_PUB_KEY_ALGS, pub_key_alg, Options),
         algorithm_proposal(true, ?SUPP_CIPH_ALGS, cipher_c2s, Options),
         algorithm_proposal(true, ?SUPP_CIPH_ALGS, cipher_s2c, Options),
         algorithm_proposal(true, ?SUPP_MAC_ALGS, mac_c2s, Options),
         algorithm_proposal(true, ?SUPP_MAC_ALGS, mac_s2c, Options),
         algorithm_proposal(true, ?SUPP_COMPR_ALGS, compr_c2s, Options),
         algorithm_proposal(true, ?SUPP_COMPR_ALGS, compr_s2c, Options),
         lists:duplicate(2, ?SSH_STRING("")),  % languages
         ?SSH_FALSE, ?SSH_UINT_32(0)],
    case send(State, MyKexInit) of
        {ok, NewState} ->
            %% Ok, do we have the server's kexinit packet?
            NewStateKex = db_lookup(NewState, kex),
            case NewStateKex#kex.init_s of
                '_' -> %% No, so we will wait for it.
                    {noreply,
                     db_insert(NewState,
                               {kex, NewStateKex#kex{
                                       init_c = pack(MyKexInit, false, State),
                                       phase = receive_kexinit}})};
                _ ->  %% Yes, so we will try to agree upon algorithms.
                    agree(
                      db_insert(NewState,
                                {kex, NewStateKex#kex{
                                        init_c = pack(MyKexInit, false,
                                                      State)
                                       }
                                }))
            end;
        {error, Reason} ->
            case Kex#kex.requester of
                '_' ->
                    ok;
                From ->
                    gen_server:reply(From, {error, {sendkexinit, Reason}})
            end,
            {stop, normal,
             db_insert(State, {error_code,
                               {?SSH_DISCONNECT_CONNECTION_LOST,
                                lists:flatten(
                                  io_lib:format("~p", [{sendkexinit, Reason}]))}
                              })}
    end.

agree(State) ->
    [Kex, Options] = db_lookup(State, [kex, options]),
    I_S = Kex#kex.init_s,
    %% Is the format of the server's packet ok?
    case parse_packet(I_S) of
        {?SSH_MSG_KEXINIT, ParsedServerKexInit} ->
            %% Yes, but what about the algorithms?
            [CtrlPid,
             Socket,
             SessionID,
             VsnC,
             VsnS,
             ServerKeyFun] =
                db_lookup(State,
                          [owner_pid,
                           socket,
                           session_id,
                           vsn_c,
                           vsn_s,
                           server_key_fun]),
            case agreed(db_insert(db_new(),
                                  [{owner_pid, CtrlPid},
                                   {socket, Socket},
                                   {session_id, SessionID},
                                   {vsn_c, VsnC},
                                   {vsn_s, VsnS},
                                   {server_key_fun, ServerKeyFun},
                                   {options, Options}]),
                        ParsedServerKexInit) of
                {ok, NextState, IgnoreOne} ->
                    %% Seems as if we can talk. Try to carry out the key exch.
                    start_kex(
                      db_insert(State, [{next, NextState},
                                        {kex, Kex#kex{
                                                init_s =
                                                [<<(size(I_S)):32/integer>>,
                                                 I_S]}},
                                        {ignore_one, IgnoreOne}]));
                {error, Reason} ->
                    %% We could not agree upon which algorithms to use.
                    case Kex#kex.requester of
                        '_' ->
                            ok;
                        From ->
                            gen_server:reply(From,
                                             {error, {agreealg, Reason}})
                    end,
                    {stop,
                     normal,
                     db_insert(State,
                               {error_code,
                                {?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
                                 lists:flatten(
                                   io_lib:format("~p",
                                                 [{agreealg, Reason}]))}})
                    }
            end;
        _ ->
            %% Seems as if the peer's kex init packet was malformed.
            case Kex#kex.requester of
                '_' ->
                    ok;
                From ->
                    gen_server:reply(From, {error, {parsekexinit, malformed_kex_init}})
            end,
            {stop, normal,
             db_insert(State,
                       {error_code,
                        {?SSH_DISCONNECT_PROTOCOL_ERROR,
                         lists:flatten(io_lib:format(
                                         "~p",
                                         [{parsekexinit, malformed_kex_init}]))}})}
    end.

start_kex(State) ->
    %% We have sent and received kex_init packets.
    %% It is now time to carry out the key exchange.
    [Sock, Kex, NextState] =
        db_lookup(State, [socket, kex, next]),
    #kex{init_c = I_C,
         init_s = I_S} = Kex,
    KexAlg = db_lookup(NextState, kex_alg),

    case KexAlg of
        ?SSH_ALG_KEX_DH_GROUP1 ->
            X = ?AUX_CRYPTO:rand_uniform(mpint(2), ?DH_14_Q),
            E = ?AUX_CRYPTO:mod_exp(?DH_14_G, X, ?DH_14_P),
            ClientKex = [?SSH_MSG_KEXDH_INIT, E],
            case send(State, ClientKex) of
                {ok, NewState} ->
                    KexData = #dh_params{x = X, e = E},
                    {noreply,
                     db_insert(NewState,
                               {kex, Kex#kex{params = KexData,
                                             phase = receive_kexpacket}})};
                {error, Reason} ->
                    case Kex#kex.requester of
                        '_' ->
                            ok;
                        From ->
                            gen_server:reply(From, {error, {sendkex, Reason}})
                    end,
                    {stop, normal,
                     db_insert(
                       State,
                       {error_code,
                        {?SSH_DISCONNECT_CONNECTION_LOST,
                         lists:flatten(
                           io_lib:format("~p", [{sendkex, Reason}]))}})}
            end
    end.

kex_continue(State) ->
    [Sock, Kex, NextState] =
        db_lookup(State, [socket, kex, next]),
    [KexAlg, PubKeyAlg] =
        db_lookup(NextState, [kex_alg, pub_key_alg]),

    case KexAlg of
        ?SSH_ALG_KEX_DH_GROUP1 ->
            %% This function is called when a kex packet is received, so
            %% the kex packet should be last.
            [Packet|ReversedMsgs] = lists:reverse(db_lookup(State, msgs)),
            NewState = db_insert(State, {msgs, lists:reverse(ReversedMsgs)}),
            #kex{init_c = I_C,
                 init_s = I_S,
                 params = Params} = Kex,
            #dh_params{x = X, e = E} = Params,
            case parse_packet(Packet) of
                {?SSH_MSG_KEXDH_REPLY, [K_S, F, Sig]} ->
                    case within_bounds(F, ?DH_14_P) of
                        true ->
                            Fingerprint = hex_string(crypto:md5([K_S])),
                            K = ?AUX_CRYPTO:mod_exp(F, X, ?DH_14_P),
                            [VsnC, VsnS] =
                                db_lookup(NewState, [vsn_c, vsn_s]),
                            Data = [VsnC,
                                    VsnS,
                                    I_C, I_S,
                                    <<(length(K_S)):32/integer>>,
                                    K_S, E, F, K],
                            H = crypto:sha(Data),
                            case verify_signature(PubKeyAlg, H, Sig, K_S) of
                                true ->
                                    kex_continue_verified(
                                      State, NextState, NewState, Kex,
                                      PubKeyAlg, Fingerprint, K_S, H, K);

                                false ->
                                    case Kex#kex.requester of
                                        '_' ->
                                            ok;
                                        Ref ->
                                            gen_server:reply(
                                              Ref,
                                              {error, {sigverif, failed}})
                                    end,
                                    {stop, normal,
                                     db_insert(
                                       NewState,
                                       {error_code,
                                        {?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
                                         "{sigverif, failed}"}})}
                            end;
                        false ->
                            case Kex#kex.requester of
                                '_' ->
                                    ok;
                                Ref ->
                                    gen_server:reply(Ref,
                                                     {error, {sigverif, failed}})
                            end,
                            {stop, normal,
                             db_insert(NewState,
                                       {error_code,
                                        {?SSH_DISCONNECT_KEY_EXCHANGE_FAILED,
                                         "{sigverif, failed}"}})}
                    end;
                _ ->
                    %% A malformed kex packet.
                    case Kex#kex.requester of
                        '_' ->
                            ok;
                        Ref ->
                            gen_server:reply(Ref,
                                             {error, {parsekex,
                                                      malformed_kex_packet}})
                    end,
                    {stop, normal,
                     db_insert(NewState,
                               {error_code,
                                {?SSH_DISCONNECT_PROTOCOL_ERROR,
                                 lists:flatten(
                                   io_lib:format("~p",
                                                 [{parsekex,
                                                   malformed_kex_packet}]))}})}
            end
    end.

kex_continue_verified(State, NextState, NewState, Kex, PubKeyAlg,
                      Fingerprint, K_S, H, K) ->
    %% The kex has been verified. Is the server key ok with the application?
    %% If yes, go on deriving the session keys.
    case (db_lookup(State, server_key_fun))(
           PubKeyAlg,
           Fingerprint) of
        true ->
            SessionID =
                case db_lookup(NewState, session_id) of
                    "none" ->
                        H;
                    OldID ->
                        OldID
                end,
            IV_C2S = derive_key(
                       K, H, "A", SessionID,
                       block_size(
                         c2s, NextState)),
            IV_S2C = derive_key(
                       K, H, "B", SessionID,
                       block_size(s2c, NextState)),
            IKey_C2S = derive_key(
                         K, H, "C", SessionID,
                         key_size(c2s, NextState)),
            IKey_S2C = derive_key(
                         K, H, "D", SessionID,
                         key_size(s2c, NextState)),
            AKey_C2S = derive_key(
                         K, H, "E", SessionID,
                         mac_size(c2s, NextState)),
            AKey_S2C = derive_key(
                         K, H, "F", SessionID,
                         mac_size(s2c, NextState)),
            send_new_keys(
              db_insert(
                NewState,
                [{kex, Kex#kex{params = '_',
			       init_s = '_',
                               init_c = '_'}},
                 {next,
                  db_insert(NextState,
                            [{session_id, SessionID},
                             {iv_c2s, IV_C2S},
                             {iv_s2c, IV_S2C},
                             {enc_key_c2s, IKey_C2S},
                             {enc_key_s2c, IKey_S2C},
                             {auth_key_c2s, AKey_C2S},
                             {auth_key_s2c, AKey_S2C}])
                 }]));
        Other ->
            case Kex#kex.requester of
                '_' ->
                    ok;
                Ref ->
                    gen_server:reply(
                      Ref,
                      {error, {hostkey,
                               PubKeyAlg,
                               Fingerprint,
                               b64_encode_pub_key(K_S)
                              }})
            end,
            {stop, normal,
             db_insert(NewState,
                       {error_code,
                        {?SSH_DISCONNECT_HOST_KEY_NOT_VERIFIABLE,
                         "Host key not accepted by user."}})}
    end.

send_new_keys(State) ->
    %% We have performed the key exchange and accepted the server's host key.
    %% Before using the new session keys. Both parties should send newkeys
    %% packets.
    case send(State, [?SSH_MSG_NEWKEYS]) of
        {ok, NewState} ->
            Kex = db_lookup(NewState, kex),
            {noreply,
             db_insert(NewState, {kex, Kex#kex{phase = receive_new_keys}})};
        {error, Reason} ->
            Kex = db_lookup(State, kex),
            case Kex#kex.requester of
                '_' ->
                    ok;
                Ref ->
                    gen_server:reply(Ref,
                                     {error, {sendnewkeys, Reason}})
            end,
            {stop, normal,
             db_insert(
               State,
               {error_code,
                {?SSH_DISCONNECT_CONNECTION_LOST,
                 lists:flatten(
                   io_lib:format("~p", [{sendnewkeys, Reason}]))}})}
    end.

receive_new_keys(State) ->
    [Msg | ReversedMsgs] = lists:reverse(db_lookup(State, msgs)),
    case Msg of
        <<?SSH_MSG_NEWKEYS>> ->
            %% The peer sent its newkeys packet, so lets start using the
            %% new session keys.
            %% We have already prepared a new state db in the 'next' field.
            Msgs = lists:reverse(ReversedMsgs),
            [SequenceNoC2S,
             SequenceNoS2C,
             CipherCache,
             PlainCache,
             WaitingForMsg,
             NextState,
             MsgsToSend,
             Kex,
             Pid] =
                db_lookup(State,
                          [seq_no_c2s,
                           seq_no_s2c,
                           cipher_cache,
                           plain_cache,
                           owner_window,
                           next,
                           msgs_to_send,
                           kex,
                           owner_pid]),
            case Kex#kex.requester of
                '_' ->
                    %% The key exchange was initiated by the peer or by
                    %% this process.
                    ok;
                Ref ->
                    %% The key exchange was initiated by the application.
                    gen_server:reply(Ref, ok)
            end,
            %% In case our db implementation is e.g. ETS:
            db_destroy(State),
            case send_msgs_to_send(
                   db_insert(NextState,
                             [{seq_no_c2s, SequenceNoC2S},
                              {seq_no_s2c, SequenceNoS2C},
                              {cipher_cache, CipherCache},
                              {plain_cache, PlainCache},
                              {owner_window, WaitingForMsg},
                              {msgs_to_send, MsgsToSend},
                              {owner_pid, Pid},
                              {msgs, Msgs}])) of
                {ok, NewState} ->
                    case WaitingForMsg of
                        0 ->
                            {noreply, NewState};
                        Count ->
                            {NewCount, NewMsgs} =
                                deliver_msgs(Count, Pid, Msgs),
                            {noreply,
                             db_insert(NewState,
                                       [{owner_window, NewCount},
                                        {msgs, NewMsgs}])}
                    end;
                _ ->
                    {stop, normal,
                     db_insert(State,
                               {error_code,
                                {?SSH_DISCONNECT_CONNECTION_LOST,
                                 "Connection lost."}})}
            end;
        _ ->
            Kex = db_lookup(State, kex),
            case Kex#kex.requester of
                '_' ->
                    ok;
                Ref ->
                    gen_server:reply(Ref,
                                     {error, {receivenewkeys, illegalpacket}})
            end,
            {stop, normal,
             db_insert(State,
                       {error_code,
                        {?SSH_DISCONNECT_PROTOCOL_ERROR,
                         "Expected new_keys packet."}})}
    end.

disconnect(State, Code, Reason) when integer(Code), list(Reason) ->
    send(State, [?SSH_MSG_DISCONNECT,
                 ?SSH_UINT_32(Code),
                 ?SSH_STRING(Reason),
                 ?SSH_STRING(?LANGUAGE)]),
    gen_tcp:close(db_lookup(State, socket));
disconnect(State, Code, Reason) ->
    send(State, [?SSH_MSG_DISCONNECT,
                 ?SSH_UINT_32(?SSH_DISCONNECT_BY_APPLICATION),
                 ?SSH_STRING("Unknown error"),
                 ?SSH_STRING(?LANGUAGE)]),
    gen_tcp:close(db_lookup(State, socket)).

incoming(State) ->
    case db_lookup(State, plain_cache) of
        {0, _} ->
            {CipherCacheSize, CipherCache} =
                db_lookup(State, cipher_cache),
            case {CipherCacheSize,
                  block_size(s2c, State)} of
                {DataSize, BlockSize} when DataSize >= BlockSize ->
                    [ToDecrypt, NewCipherCache] =
                        split_data([BlockSize],
                                   CipherCache),
                    {NewPlainCache, NewState} =
                        decrypt({BlockSize, ToDecrypt}, State),
                    incoming(
                      db_insert(NewState,
                                [{cipher_cache,
                                  {DataSize - BlockSize, NewCipherCache}},
                                 {plain_cache, {BlockSize, NewPlainCache}}]));
                _ ->
                    incomplete_msg(State)
            end;
        {PlainSize, <<PacketSize:32/integer, PadSize, MsgHead/binary>>}
        when PacketSize > ?PAYLOAD_MAXLEN ->
            {stop, normal,
             db_insert(State,
                       {error_code,
                        {?SSH_DISCONNECT_PROTOCOL_ERROR,
                         "Bad length: " ++ integer_to_list(PacketSize)}})};
        {PlainSize, PlainCache} ->
            <<PacketSize:32/integer, PadSize, MsgHead/binary>> = PlainCache,
            {CipherCacheSize, CipherCache} = db_lookup(State, cipher_cache),
            %% Needed = MacSize plus PacketSize - BlockSize + 4
            MacSize = mac_size(s2c, State),
            BlockSize = block_size(s2c, State),
            case PacketSize + MacSize - BlockSize + 4 of
                Needed when CipherCacheSize >= Needed ->
                    ToDecryptSize = PacketSize - BlockSize + 4,
                    [ToDecrypt, Mac, Rest] =
                        split_data([ToDecryptSize, MacSize],
                                   CipherCache),
                    MsgTailSize = ToDecryptSize - PadSize,
                    {Decrypted, NewState1} = decrypt({ToDecryptSize, ToDecrypt},
						     State),
                    {Msg, Pad} =
                        case MsgTailSize of
                            MsgTailSize when MsgTailSize >= 0 ->
                                <<MsgTail:MsgTailSize/binary, Pado/binary>> =
                                    Decrypted,
                                {[MsgHead, MsgTail], Pado};
                            _ ->
                                MsgSize = BlockSize - 5 + MsgTailSize,
                                <<RealMsg:MsgSize/binary, PadHead/binary>> =
                                    MsgHead,
                                {[RealMsg], [PadHead, Decrypted]}
                        end,

                    case verify_mac([PlainCache,
                                     Decrypted], Mac, NewState1) of
                        {ok, NewState2} ->
                            verified_msg(list_to_binary(Msg),
                                         db_insert(
                                           NewState2,
                                           {cipher_cache,
                                            {CipherCacheSize - ToDecryptSize -
                                             MacSize, Rest}}));
                        _ ->
                            {stop, normal,
                             db_insert(NewState1,
                                       {error_code,
                                        {?SSH_DISCONNECT_MAC_ERROR,
                                         "MAC verification error."}})}
                    end;
                _ ->
                    incomplete_msg(State)
            end
    end.

verified_msg(Msg, State) ->
    case {Msg, db_lookup(State, ignore_one)} of
        {<<?SSH_MSG_DISCONNECT, _/binary>>, _} ->
            handle_disconnect_msg(Msg, State);
        {<<Ignore, _/binary>>, _} when Ignore == ?SSH_MSG_DEBUG;
				       Ignore == ?SSH_MSG_IGNORE ->
            incomplete_msg(db_insert(State, {plain_cache, {0, <<>>}}));
        {_, true} ->
            incomplete_msg(db_insert(State,
                                     [{plain_cache, {0, <<>>}},
                                      {ignore_one, false}]));
        {<<?SSH_MSG_KEXINIT, _/binary>>, _} ->
            case db_lookup(State, kex) of
                '_' ->
                    set_rereceive(State),
                    kex_init(db_insert(State,
                                       [{plain_cache, {0, <<>>}},
                                        {kex, #kex{init_s = Msg}}]));
                Kex ->
                    new_msg(db_insert(State,
                                      [{plain_cache, {0, <<>>}},
                                       {kex, Kex#kex{init_s = Msg}}]))
            end;
        _ ->
            new_msg(db_insert(State,
                              [{msgs, db_lookup(State, msgs) ++ [Msg]},
                               {plain_cache, {0, <<>>}}]))
    end.

new_msg(State) ->
    case db_lookup(State, [kex, bytes_recvd]) of
        [Kex, ReceivedBytes]
        when Kex == '_',
	ReceivedBytes >= ?MAX_BYTES_BEFORE_REKEY ->
            set_rereceive(State),
            case db_lookup(State, [owner_window, owner_pid]) of
                [0, _] ->
                    kex_init(db_insert(State,
                                       {kex, #kex{requester = '_'}}));
                [Count, Pid] ->
                    Msgs = db_lookup(State, msgs),
                    {NewCount, NewMsgs} =
                        deliver_msgs(Count, Pid, Msgs),
                    kex_init(db_insert(State,
                                       [{owner_window, NewCount},
                                        {msgs, NewMsgs},
                                        {kex, #kex{requester = '_'}}]))
            end;
        ['_', _] ->
            set_rereceive(State),
            case db_lookup(State, [owner_window, owner_pid]) of
                [0, _] ->
                    {noreply, State};
                [Count, Pid] ->
                    Msgs = db_lookup(State, msgs),
                    {NewCount, NewMsgs} = deliver_msgs(Count, Pid, Msgs),
                    {noreply,
                     db_insert(State,
                               [{owner_window, NewCount},
                                {msgs, NewMsgs}])}
            end;
        _ ->
            set_rereceive(State),
            Kex = db_lookup(State, kex),
            case Kex#kex.phase of
                receive_kexinit ->
                    case Kex#kex.init_s of
                        '_' -> %%% The new message was not a kex_init message.
                            case db_lookup(State, [owner_window,
						   owner_pid]) of
                                [0, _] ->
                                    {noreply, State};
                                [Count, Pid] ->
                                    Msgs = db_lookup(State, msgs),
                                    {NewCount, NewMsgs} =
                                        deliver_msgs(Count, Pid, Msgs),
                                    {noreply,
                                     db_insert(State,
                                               [{owner_window, NewCount},
                                                {msgs, NewMsgs}])}
                            end;
                        _ ->
                            agree(State)
                    end;
                receive_kexpacket ->
                    kex_continue(State);
                receive_new_keys ->
                    receive_new_keys(State)
            end
    end.

incomplete_msg(State) ->
    set_rereceive(State),
    {noreply, State}.

decrypt({0, _}, State) ->
    {<<>>, State};
decrypt({Size, Data}, State) ->
    case db_lookup(State, cipher_s2c) of
        "none" ->
            {list_to_binary([Data]), State};
        ?SSH_ALG_3DES_CBC ->
            [IV, Key, BytesReceived] =
                db_lookup(State, [iv_s2c, enc_key_s2c, bytes_recvd]),
            <<Key1:8/binary, Key2:8/binary, Key3:8/binary>> =
                Key,
            DecryptedData =
                crypto:des_ede3_cbc_decrypt(Key1, Key2, Key3, IV,
                                            Data),
            NewIV = last(block_size(s2c, State), {Size, Data}),
            {DecryptedData,
             db_insert(State,
                       [{iv_s2c, NewIV},
                        {bytes_recvd,
                         BytesReceived + Size}])};
        ?SSH_ALG_AES128_CBC ->
            [IV, Key, BytesReceived] =
                db_lookup(State, [iv_s2c, enc_key_s2c, bytes_recvd]),
            DecryptedData =
                ?AUX_CRYPTO:aes128_cbc_decrypt(Key, IV, Data),
            NewIV = last(block_size(s2c, State), {Size, Data}),
            {DecryptedData,
             db_insert(State,
                       [{iv_s2c, NewIV},
                        {bytes_recvd,
                         BytesReceived + Size}])}
    end.

verify_mac(Data, MacToVerify, State) ->
    case db_lookup(State, [mac_s2c, seq_no_s2c]) of
        [MacAlg, SequenceNo]
        when MacAlg == "none" ->
            case flatlength(MacToVerify) of
                0 ->
                    {ok, db_insert(State,
				   {seq_no_s2c, uint_inc(SequenceNo)})};
                _ ->
                    {error, mac}
            end;
        [MacAlg, SequenceNo]
        when MacAlg == ?SSH_ALG_HMAC_SHA1 ->
            [AuthKey, BytesReceived] =
                db_lookup(State, [auth_key_s2c, bytes_recvd]),
            Mac = crypto:sha_mac(AuthKey, [SequenceNo, Data]),
            case compare_data(MacToVerify, Mac) of
                equal ->
                    {ok,
                     db_insert(State,
                               [{seq_no_s2c, uint_inc(SequenceNo)},
                                {bytes_recvd, BytesReceived + 20}])};
                _ ->
                    {error, mac}
            end;
        [MacAlg, SequenceNo]
        when MacAlg == ?SSH_ALG_HMAC_MD5 ->
            [AuthKey, BytesReceived] =
                db_lookup(State, [auth_key_s2c, bytes_recvd]),
            Mac = crypto:md5_mac(AuthKey, [SequenceNo, Data]),
            case compare_data(MacToVerify, Mac) of
                equal ->
                    {ok,
                     db_insert(State,
                               [{seq_no_s2c, uint_inc(SequenceNo)},
                                {bytes_recvd, BytesReceived + 20}])};
                _ ->
                    {error, mac}
            end
    end.


agreed(State, [_, SExchAlgs, SPubKeyAlgs, SCipherC2SAlgs, SCipherS2CAlgs,
               SMacC2SAlgs, SMacS2CAlgs, SComprC2SAlgs, SComprS2CAlgs,
               _, _, KexFollows, _]) ->
    Options = db_lookup(State, options),
    agreed([SExchAlgs, SPubKeyAlgs, SCipherC2SAlgs, SCipherS2CAlgs,
            SMacC2SAlgs, SMacS2CAlgs, SComprC2SAlgs, SComprS2CAlgs],
           [algorithm_proposal(false, ?SUPP_EXCH_ALGS, exch_alg, Options),
            algorithm_proposal(false, ?SUPP_PUB_KEY_ALGS, pub_key_alg, Options),
            algorithm_proposal(false, ?SUPP_CIPH_ALGS, cipher_c2s, Options),
            algorithm_proposal(false, ?SUPP_CIPH_ALGS, cipher_s2c, Options),
            algorithm_proposal(false, ?SUPP_MAC_ALGS, mac_c2s, Options),
            algorithm_proposal(false, ?SUPP_MAC_ALGS, mac_s2c, Options),
            algorithm_proposal(false, ?SUPP_COMPR_ALGS, compr_c2s, Options),
            algorithm_proposal(false, ?SUPP_COMPR_ALGS, compr_s2c, Options)],
           [kex_alg, pub_key_alg, cipher_c2s, cipher_s2c,
            mac_c2s, mac_s2c, compr_c2s, compr_s2c],      
           State, true, KexFollows).

agreed([], [], [], State, AllCorrect, KexFollows) ->
    {ok, State, (AllCorrect == false) and (KexFollows == true)};
agreed([SAlgs|SRest], [CAlgs|CRest], [Field|FRest],
       State, AllCorrect, KexFollows) ->
    case common(SAlgs, CAlgs) of
        {ok, Alg, Correct} ->
            agreed(SRest, CRest, FRest, db_insert(State, {Field, Alg}),
                   Correct and AllCorrect, KexFollows);
        Other ->
            {error, {none_in_common, Field}}
    end.

common(SString, CAlgs) ->
    {ok, SAlgs} = regexp:split(SString, ", "),
    case {SAlgs, CAlgs} of
        {[Common | _], [Common | _]} ->
            {ok, Common, true};
        _ ->
            common_1(SAlgs, CAlgs)
    end.

common_1(SAlgs, []) ->
    {error, none_in_common};
common_1(SAlgs, [CAlg | CRest]) ->
    case lists:member(CAlg, SAlgs) of
        true ->
            {ok, CAlg, false};
        false ->
            common_1(SAlgs, CRest)
    end.

get_version(Sock) ->
    case gen_tcp:recv(Sock, 0, ?RECV_TIMEOUT) of %%% Sock is in line mode.
        {ok, Line} ->
            get_version(Sock, binary_to_list(Line));
        Other ->
            Other
    end.

get_version(Sock, "SSH-" ++ VsnLine) ->
    case do_get_version(VsnLine, "") of
        {ok, Vsn} ->
            VsnString =
                strip_end_of_line(VsnLine),
            {ok, Vsn, "SSH-" ++ VsnString};
        Other ->
            Other
    end;
get_version(Sock, LastLine) ->
    case gen_tcp:recv(Sock, 0, ? RECV_TIMEOUT) of
        {ok, Line} ->
            get_version(Sock, binary_to_list(Line));
        Other ->
            Other
    end.

do_get_version([], Acc) ->
    {error, Acc};
do_get_version([$- | _], Acc) ->
    {ok, Acc};
do_get_version([Char | Rest], Acc) ->
    do_get_version(Rest, Acc ++ [Char]).

parse_packet(<<?SSH_MSG_KEXINIT, KexInit/binary>>) ->
    parse_packet([cookie, string, string, string, string, string,
                  string, string, string, string, string, boolean,
                  uint32], KexInit, {?SSH_MSG_KEXINIT, []});
parse_packet(<<?SSH_MSG_KEXDH_REPLY, KexReply/binary>>) ->
    parse_packet([string, mpint, string], KexReply, {?SSH_MSG_KEXDH_REPLY, []});
parse_packet(<<?SSH_MSG_IGNORE, Ignore/binary>>) ->
    parse_packet([string], Ignore, {?SSH_MSG_IGNORE, []});
parse_packet(<<?SSH_MSG_DISCONNECT, Disconnect/binary>>) ->
    parse_packet([uint32, string, string], Disconnect, {?SSH_MSG_DISCONNECT, []});
parse_packet(<<?SSH_MSG_UNIMPLEMENTED, Unimplemented/binary>>) ->
    {?SSH_MSG_UNIMPLEMENTED, Unimplemented};
parse_packet(<<?SSH_MSG_SERVICE_ACCEPT, Service/binary>>) ->
    parse_packet([string], Service, {?SSH_MSG_SERVICE_ACCEPT, []});
parse_packet(<<?SSH_MSG_DEBUG, Debug/binary>>) ->
    parse_packet([boolean, string, string], Debug, {?SSH_MSG_DEBUG, []});
parse_packet(<<?SSH_MSG_NEWKEYS, NewKeys/binary>>) ->
    {?SSH_MSG_NEWKEYS, NewKeys}.

parse_packet(Recipe, Packet) ->
    parse_packet(Recipe, Packet, {ok, []}).

parse_packet([], _, {Type, Acc}) ->
    {Type, lists:reverse(Acc)};
parse_packet([cookie | Pieces], <<Cookie:16/binary, Rest/binary>>, { Type, Acc}) ->
    parse_packet(Pieces, Rest, {Type, [Cookie | Acc]});
parse_packet([string|Pieces], <<StringLength:32/integer, Binary/binary>>,
             {Type, Acc}) ->
    case Binary of
        <<String:StringLength/binary, Rest/binary>> ->
            parse_packet(Pieces, Rest, {Type, [binary_to_list(String) | Acc]});
        _ ->
            {error, parse_string}
    end;
parse_packet([boolean | Pieces], <<Boolean, Rest/binary>>, {Type, Acc})
  when Boolean == ?SSH_FALSE ->
    parse_packet(Pieces, Rest, {Type, [false | Acc]});
parse_packet([boolean | Pieces], <<Boolean, Rest/binary>>, {Type, Acc }) ->
    parse_packet(Pieces, Rest, {Type, [true | Acc]});
parse_packet([uint32 | Pieces], <<UInt32:32/integer, Rest/binary>>, {Type, Acc}) ->
    parse_packet(Pieces, Rest, {Type, [UInt32|Acc]});
parse_packet([uint64 | Pieces], <<UInt64:64/integer, Rest/binary>>, {Type, Acc}) ->
    parse_packet(Pieces, Rest, {Type, [UInt64 | Acc]});
parse_packet([mpint | Pieces], <<MPIntLength:32/integer, Binary/binary>>,
             {Type, Acc}) ->
    case Binary of
        <<MPInt:MPIntLength/binary, Rest/binary>> ->
            parse_packet(Pieces, Rest,
                         {Type, [<<MPIntLength:32/integer, MPInt/binary>> |
				 Acc]});
        _ ->
            {error, parse_mpint}
    end;
parse_packet([dsskey | Pieces], <<StringLength:32/integer, Binary/binary>>,
             {Type, Acc}) ->
    case Binary of
        <<String:StringLength/binary, Rest/binary>> ->
            case
                parse_packet([string, mpint, mpint, mpint, mpint],
                             String, {dummy, []}) of
                {dummy, DssKey} ->
                    parse_packet(Pieces, Rest, {Type,
						[DssKey | Acc]});
                _ ->
                    {error, parse_dsskey}
            end;
        _ ->
            {error, parse_dsskey}
    end;
parse_packet([dsssig | Pieces], <<StringLength:32/integer, Binary/binary>>,
             {Type, Acc}) ->
    case Binary of
        <<String:StringLength/binary, Rest/binary>> ->
            case parse_packet([string, string], String, {dummy, []}) of
                {dummy, DssSig} ->
                    parse_packet(Pieces, Rest, {Type, [DssSig | Acc]});
                _ ->
                    {error, parse_dsssig}
            end;
        _ ->
            {error, parse_dsssig}
    end;
parse_packet([rsakey | Pieces], <<StringLength:32/integer, Binary/binary>>,
             {Type, Acc}) ->
    case Binary of
        <<String:StringLength/binary, Rest/binary>> ->
            case parse_packet([string, mpint, mpint], String, { dummy, []}) of
                {dummy, RsaKey} ->
                    parse_packet(Pieces, Rest, {Type, [RsaKey | Acc]});
                _ ->
                    {error, parse_rsakey}
            end;
        _ ->
            {error, parse_rsakey}
    end;
parse_packet([rsasig | Pieces], <<StringLength:32/integer, Binary/binary>>,
             {Type, Acc}) ->
    case Binary of
        <<String:StringLength/binary, Rest/binary>> ->
            case parse_packet([string, string], String, {dummy, []}) of
                {dummy, RsaSig} ->
                    parse_packet(Pieces, Rest, {Type, [RsaSig | Acc]});
                _ ->
                    {error, parse_rsasig}
            end;
        _ ->
            {error, parse_rsasig}
    end;
parse_packet([Type |_], _, _) ->
    %% There is a limited number of types, so the following is ok.
    Error = list_to_atom("parse_" ++ atom_to_list(Type)),
    {error, Error}.

pack({PaySize, PayLoad}, Padding, State) ->
    case Padding of
        false ->
            [<<PaySize:32/integer>>, PayLoad];
        Padding ->
            BlockSize = block_size(c2s, State),
            PadSize =
                case {Padding, 2 * BlockSize - 5 - (PaySize rem BlockSize)} of
                    {true, Whatever} ->
                        Whatever;
                    {random, Whatever} when Whatever < BlockSize ->
                        Whatever + BlockSize *
                            ?AUX_CRYPTO:rand_uniform(0, 256 div BlockSize - 1);
                    {random, Whatever} ->
                        Whatever + 8 *
                            ?AUX_CRYPTO:rand_uniform(0, 256 div BlockSize - 2)
                end,
            Pad = padding(PadSize, Padding),
            PacketSize = 1 + PaySize + PadSize,
            {PacketSize + 4,
             [<<PacketSize:32/integer, PadSize>>, PayLoad, Pad]}
    end;
pack(ListOfListsAndBinaries, Padding, State) when list(ListOfListsAndBinaries) ->
    pack({flatlength(ListOfListsAndBinaries), ListOfListsAndBinaries},
         Padding, State);
pack(OneTerm, Padding, State) ->
    pack([OneTerm], Padding, State).

send(State, Data) ->
    case db_lookup(State, cipher_c2s) of
        "none" ->
            {PacketSize, Packet} = pack(Data, true, State),
            add_mac_and_send(Packet, PacketSize, Packet, State);
        ?SSH_ALG_3DES_CBC ->
            [<<Key1:8/binary, Key2:8/binary, Key3:8/binary>>, IV] =
                db_lookup(State, [enc_key_c2s, iv_c2s]),
            {PacketSize, Packet} = pack(Data, true, State), %% random),
            EncPacket =
                crypto:des_ede3_cbc_encrypt(Key1, Key2, Key3, IV, Packet),
            AllButLastSize = PacketSize - 8,
            <<_:AllButLastSize/binary, NewIV:8/binary>> = EncPacket,
            add_mac_and_send(Packet, PacketSize, EncPacket,
                             db_insert(State, {iv_c2s, NewIV}));
        ?SSH_ALG_AES128_CBC ->
            [Key, IV] =
                db_lookup(State, [enc_key_c2s, iv_c2s]),
            {PacketSize, Packet} = pack(Data, true, State), %% random),
            EncPacket = ?AUX_CRYPTO:aes128_cbc_encrypt(Key, IV, Packet),
            AllButLastSize = PacketSize - block_size(c2s, State),
            <<_:AllButLastSize/binary, NewIV/binary>> = EncPacket,
            add_mac_and_send(Packet, PacketSize, EncPacket,
                             db_insert(State, {iv_c2s, NewIV}))
    end.

add_mac_and_send(Packet, PacketSize, EncPacket,
                 State) ->
    case db_lookup(State, [mac_c2s, seq_no_c2s, socket]) of
        ["none", SequenceNo, Sock] ->
            case gen_tcp:send(Sock, EncPacket) of
                ok ->
                    {ok, db_insert(State, {seq_no_c2s,
					   uint_inc(SequenceNo)})};
                Other ->
                    Other
            end;
        [?SSH_ALG_HMAC_SHA1, SequenceNo, Sock] ->
            [AuthKey, BytesSent] =
                db_lookup(State, [auth_key_c2s, bytes_sent]),
            Mac = crypto:sha_mac(AuthKey,
                                 [SequenceNo, Packet]),
            case gen_tcp:send(Sock, [EncPacket, Mac]) of
                ok ->
                    {ok, db_insert(State,
                                   [{seq_no_c2s, uint_inc(SequenceNo)},
                                    {bytes_sent,
                                     BytesSent + PacketSize}])};
                Other ->
                    Other
            end;
        [?SSH_ALG_HMAC_MD5, SequenceNo, Sock] ->
            [AuthKey, BytesSent] =
                db_lookup(State, [auth_key_c2s, bytes_sent]),
            Mac = crypto:md5_mac(AuthKey,
                                 [SequenceNo, Packet]),
            case gen_tcp:send(Sock, [EncPacket, Mac]) of
                ok ->
                    {ok, db_insert(State,
                                   [{seq_no_c2s, uint_inc(SequenceNo)},
                                    {bytes_sent,
                                     BytesSent + PacketSize}])};
                Other ->
                    Other
            end
    end.

padding(Size, random) ->
    ?AUX_CRYPTO:rand_bytes(Size);
padding(Size, true) ->
    lists:duplicate(Size, 0).

flatlength(Binary) when binary(Binary) ->
    size(Binary);
flatlength(List) ->
    flatlength(List, 0).

flatlength([H | T], L) when list(H) ->
    flatlength(H, flatlength(T, L));
flatlength([H | T], L) when binary(H) ->
    flatlength(T , L + size(H));
flatlength([H | T], L) ->
    flatlength(T, L+1);
flatlength([], L) ->
    L.

uint_inc(Binary) ->
    Size = size(Binary) * 8,
    <<Int:Size/integer>> = Binary,
    NewInt = Int + 1,
    <<NewInt:Size/integer>>.


%%% In the following operations, positive integers are assumed.

mpint_to_int(<<Size:32/integer, Bin/binary>>) ->
    BitSize = 8 * Size,
    <<Int:BitSize/integer>> = Bin,
    Int.

mpint(Integer) when integer(Integer), Integer >= 0 ->
    Binary = int_to_bin(Integer),
    mpint(Binary);
mpint(Binary) when binary(Binary) ->
    case Binary of
        <<MSB, _/binary>> when MSB > 127 ->
            %% A positive value should not have a 1 as most significant bit.
            <<(size(Binary) + 1):32/integer, 0, Binary/binary>>;
        _ ->
            <<(size(Binary)):32/integer, Binary/binary>>
		end.

int_to_bin(Integer) ->
    int_to_bin(Integer, []).

int_to_bin(0, List) ->
    list_to_binary(List);
int_to_bin(Integer, List) ->
    int_to_bin(Integer bsr 8, [Integer band 255|List]).

strip_end_of_line(String) ->
    do_strip_end_of_line(lists:reverse(String)).

do_strip_end_of_line("\n\r" ++ String) ->
    lists:reverse(String);
do_strip_end_of_line("\n" ++ String) ->
    lists:reverse(String);
do_strip_end_of_line(String) ->
    lists:reverse(String).

hex_string(Binary) ->
    hex_string(Binary, []).

hex_string(<<>>, Acc) ->
    lists:reverse(Acc);
hex_string(<<Byte, Rest/binary>>, Acc) ->
    hex_string(Rest, [hex_char(Byte band 15), hex_char(Byte bsr 4)|
		      case (length(Acc) + 1) rem 3 of
			  0 -> [$: | Acc];
			  _ -> Acc
		      end]).

hex_char(HB) when HB > 9 ->
    $a + HB - 10;
hex_char(HB) ->
    $0 + HB.

block_size(Dir, State) ->
    Cipher = case Dir of
                 s2c -> db_lookup(State, cipher_s2c);
                 c2s -> db_lookup(State, cipher_c2s)
             end,
    case Cipher of
        "none" ->               8;
        ?SSH_ALG_3DES_CBC ->    8;
        ?SSH_ALG_AES128_CBC -> 16
    end.

key_size(Dir, State) ->
    Cipher = case Dir of
                 s2c -> db_lookup(State, cipher_s2c);
                 c2s -> db_lookup(State, cipher_c2s)
             end,
    case Cipher of
        "none" ->               0;
        ?SSH_ALG_3DES_CBC ->   24;
        ?SSH_ALG_AES128_CBC -> 16
    end.

mac_size(Dir, State) ->
    Mac = case Dir of
              s2c -> db_lookup(State, mac_s2c);
              c2s -> db_lookup(State, mac_c2s)
          end,
    case Mac of
        "none" ->              0;
        ?SSH_ALG_HMAC_SHA1 -> 20;
        ?SSH_ALG_HMAC_MD5 ->  16
    end.

last(Size, {DataSize, Data}) ->
    [_, Wanted] = split_data([DataSize - Size], Data),
    Wanted.

split_data(SplitSizes, Data) when binary(Data) ->
    split_data(SplitSizes, Data, []);
split_data(SplitSizes, Data) ->
    split_data(SplitSizes, list_to_binary(Data), []).

split_data([], Rest, Acc) ->
    lists:reverse([Rest | Acc]);
split_data([FirstSize | RestSizes], Data, Acc) when size(Data) >= FirstSize ->
    <<First:FirstSize/binary, Rest/binary>> = Data,
    split_data(RestSizes, Rest, [First | Acc]).

%% This is a lot more complicated, but might give better performance
%% since list_to_binary/1 is not called.
%%split_data(SplitSizes, Data) ->
%%    split_data(SplitSizes, Data, []).
%%
%%split_data([], Rest, Acc) ->
%%    lists:reverse([Rest|Acc]);
%%split_data([SplitSize|SplitSizes], Data, Acc) ->
%%    {FirstPiece, Rest} = do_split_data(SplitSize, Data, []),
%%    split_data(SplitSizes, Rest, [FirstPiece|Acc]).
%%
%%do_split_data(0, Data, Acc) ->
%%    {lists:reverse(Acc), Data};
%%do_split_data(SizeofFirst, Data, Acc) when binary(Data) ->
%%    <<First:SizeofFirst/binary,
%%     Rest/binary>> = Data,
%%    {lists:reverse([First|Acc]), Rest};
%%do_split_data(SizeofFirst, [H|T], Acc) when binary(H), size(H) < SizeofFirst ->
%%    do_split_data(SizeofFirst - size(H), T, [H|Acc]);
%%do_split_data(SizeofFirst, [H|T], Acc) when integer(H) ->
%%    do_split_data(SizeofFirst - 1, T, [H|Acc]);
%%do_split_data(SizeofFirst, [H|T], Acc) ->
%%    case flatlength(H) of
%%       HLength when HLength >= SizeofFirst ->
%%           {First, Rest} = do_split_data(SizeofFirst, H, []),
%%           {lists:reverse([First|Acc]), [Rest|T]};
%%       HLength ->
%%           do_split_data(SizeofFirst - HLength, T, [H|Acc])
%%    end.

compare_data(Data1, Data2) when binary(Data1), binary(Data2) ->
    do_compare_data(Data1, Data2);
compare_data(Data1, Data2) when binary(Data1) ->
    do_compare_data(Data1, list_to_binary(Data2));
compare_data(Data1, Data2) when binary(Data2) ->
    do_compare_data(list_to_binary(Data1), Data2);
compare_data(Data1 , Data2) ->
    do_compare_data(list_to_binary(Data1), list_to_binary(Data2)).

do_compare_data(Binary1, Binary1) ->
    equal;
do_compare_data(_, _) ->
    not_equal.

set_rereceive(State) ->
    BlockSize = block_size(s2c, State),
    case db_lookup(State, cipher_cache) of
        {Size, _} when Size < BlockSize ->
            inet:setopts(db_lookup(State, socket), [{active, once}]);
        Size ->
            case db_lookup(State, plain_cache) of
                {0, _} ->
                    gen_server:cast(self(), more_data);
                _ ->
                    inet:setopts(db_lookup(State, socket), [{active, once}])
            end
    end.

derive_key(K, H, Salt, SessionID, Length) when Length =< 20 ->
    <<Key:Length/binary, _/binary>> =
        crypto:sha([K, H, Salt, SessionID]),
    Key;
derive_key(K, H, Salt, SessionID, Length) ->
    Key = crypto:sha([K, H, Salt, SessionID]),
    derive_key(K, H,  Key, Length).

derive_key(K, H, Sofar , Length) when size(Sofar) >= Length ->
    <<Key:Length/binary, _/binary>> = Sofar,
    Key;
derive_key(K, H, Sofar, Length) ->
    derive_key(K, H, <<Sofar/binary, (crypto:sha([K, H, Sofar]))/binary>>, Length).

algorithm_proposal(AsString, ListOfAlgs, OptionName, Options) ->
    FinalListOfAlgs =
        case lists:keysearch(OptionName, 1, Options) of
            false ->
                ListOfAlgs;
            {value, {OptionName, Algs}} ->
                Algs
        end,
    case AsString of
        true ->
            case FinalListOfAlgs of
                [] ->
                    ?SSH_STRING("");
                _ ->
                    sized_binary([hd(FinalListOfAlgs),
                                  [[", ", X] || X <- tl(FinalListOfAlgs)]])
            end;
        false ->
            FinalListOfAlgs
    end.

verify_signature(?SSH_ALG_PUB_KEY_DSS, H, Sig, K_S) ->
    {dummy, [["ssh-dss", K_S_P, K_S_Q, K_S_G, K_S_Y]]} =
        parse_packet([dsskey], sized_binary(K_S), {dummy, []}),
    {dummy, [["ssh-dss", SigBlob]]} =
        parse_packet([dsssig], sized_binary(Sig), {dummy, []}),   
    case ?AUX_CRYPTO:dss_verify([<<20:32/integer>>, H],
				SigBlob,
				[K_S_P, K_S_Q, K_S_G, K_S_Y]
			       ) of
        <<1>> ->
            true;
        _ ->
            false
    end;
verify_signature(?SSH_ALG_PUB_KEY_RSA, H, Sig, K_S) ->
    {dummy, [["ssh-rsa", K_S_E, K_S_N]]} =
        parse_packet([rsakey], sized_binary(K_S), {dummy, []}),
    {dummy, [["ssh-rsa", SigBlob]]} =
        parse_packet([rsasig], sized_binary(Sig), {dummy, []}),   
    case ?AUX_CRYPTO:rsa_verify([<<20:32/integer>>, H],
				sized_binary(SigBlob),
				[K_S_E, K_S_N]
			       ) of
        <<1>> ->
            true;
        _ ->
            false
    end.

within_bounds(MPInt, MPRoof) ->
    Roof = mpint_to_int(MPRoof),
    case mpint_to_int(MPInt) of
        Int when Int > 0, Int =< Roof ->
            true;
        _ ->
            false
    end.

b64_encode_pub_key(PubKeyBlob) ->
    httpd_util:encode_base64(binary_to_list(list_to_binary(PubKeyBlob))).


%% Settled for ETS in favour of record. Crash reports and such doesn't work
%% very well when you carry around big binaries in loop data.

db_new() ->
    Ets = ets:new(?MODULE, [private, set]),
    db_insert(Ets, ?DB_DEFAULT),
    Ets.

db_insert(Ets, KeysAndValues) ->
    true = ets:insert(Ets, KeysAndValues),
    Ets.

db_lookup(Ets, Key) when atom(Key) ->
    [{Key, Value}] = ets:lookup(Ets, Key),
    Value;
db_lookup(Ets, Keys) ->
    db_lookup(Ets, lists:reverse(Keys), []).

db_lookup(Ets, [], Acc) ->
    Acc;
db_lookup(Ets, [Key|Keys], Acc) ->
    db_lookup(Ets, Keys, [db_lookup(Ets, Key)|Acc]).

db_destroy(Ets) ->
    ets:delete(Ets).

%%db_new() ->
%%    #ssh{}.
%%
%%db_insert(Db, KeyAndValue) when tuple(KeyAndValue) ->
%%    db_insert(Db, [KeyAndValue]);
%%db_insert(Db, []) ->
%%    Db;
%%db_insert(Db, [{owner_pid, Value}|Values]) ->
%%    db_insert(Db#ssh{owner_pid = Value}, Values);
%%db_insert(Db, [{socket, Value}|Values]) ->
%%    db_insert(Db#ssh{socket = Value}, Values);
%%db_insert(Db, [{cipher_cache, Value}|Values]) ->
%%    db_insert(Db#ssh{cipher_cache = Value}, Values);
%%db_insert(Db, [{plain_cache, Value}|Values]) ->
%%    db_insert(Db#ssh{plain_cache = Value}, Values);
%%db_insert(Db, [{msgs, Value}|Values]) ->
%%    db_insert(Db#ssh{msgs = Value}, Values);
%%db_insert(Db, [{msgs_to_send, Value}|Values]) ->
%%    db_insert(Db#ssh{msgs_to_send = Value}, Values);
%%db_insert(Db, [{ignore_one, Value}|Values]) ->
%%    db_insert(Db#ssh{ignore_one = Value}, Values);
%%db_insert(Db, [{owner_window, Value}|Values]) ->
%%    db_insert(Db#ssh{owner_window = Value}, Values);
%%db_insert(Db, [{kex, Value}|Values]) ->
%%    db_insert(Db#ssh{kex = Value}, Values);
%%db_insert(Db, [{session_id, Value}|Values]) ->
%%    db_insert(Db#ssh{session_id = Value}, Values);
%%db_insert(Db, [{iv_c2s, Value}|Values]) ->
%%    db_insert(Db#ssh{iv_c2s = Value}, Values);
%%db_insert(Db, [{iv_s2c, Value}|Values]) ->
%%    db_insert(Db#ssh{iv_s2c = Value}, Values);
%%db_insert(Db, [{seq_no_c2s, Value}|Values]) ->
%%    db_insert(Db#ssh{seq_no_c2s = Value}, Values);
%%db_insert(Db, [{seq_no_s2c, Value}|Values]) ->
%%    db_insert(Db#ssh{seq_no_s2c = Value}, Values);
%%db_insert(Db, [{kex_alg, Value}|Values]) ->
%%    db_insert(Db#ssh{kex_alg = Value}, Values);
%%db_insert(Db, [{pub_key_alg, Value}|Values]) ->
%%    db_insert(Db#ssh{pub_key_alg = Value}, Values);
%%db_insert(Db, [{cipher_c2s, Value}|Values]) ->
%%    db_insert(Db#ssh{cipher_c2s = Value}, Values);
%%db_insert(Db, [{cipher_s2c, Value}|Values]) ->
%%    db_insert(Db#ssh{cipher_s2c = Value}, Values);
%%db_insert(Db, [{mac_c2s, Value}|Values]) ->
%%    db_insert(Db#ssh{mac_c2s = Value}, Values);
%%db_insert(Db, [{mac_s2c, Value}|Values]) ->
%%    db_insert(Db#ssh{mac_s2c = Value}, Values);
%%db_insert(Db, [{compr_c2s, Value}|Values]) ->
%%    db_insert(Db#ssh{compr_c2s = Value}, Values);
%%db_insert(Db, [{compr_s2c, Value}|Values]) ->
%%    db_insert(Db#ssh{compr_s2c = Value}, Values);
%%db_insert(Db, [{enc_key_c2s, Value}|Values]) ->
%%    db_insert(Db#ssh{enc_key_c2s = Value}, Values);
%%db_insert(Db, [{enc_key_s2c, Value}|Values]) ->
%%    db_insert(Db#ssh{enc_key_s2c = Value}, Values);
%%db_insert(Db, [{auth_key_c2s, Value}|Values]) ->
%%    db_insert(Db#ssh{auth_key_c2s = Value}, Values);
%%db_insert(Db, [{auth_key_s2c, Value}|Values]) ->
%%    db_insert(Db#ssh{auth_key_s2c = Value}, Values);
%%db_insert(Db, [{bytes_sent, Value}|Values]) ->
%%    db_insert(Db#ssh{bytes_sent = Value}, Values);
%%db_insert(Db, [{bytes_recvd, Value}|Values]) ->
%%    db_insert(Db#ssh{bytes_recvd = Value}, Values);
%%db_insert(Db, [{vsn_c, Value}|Values]) ->
%%    db_insert(Db#ssh{vsn_c = Value}, Values);
%%db_insert(Db, [{vsn_s, Value}|Values]) ->
%%    db_insert(Db#ssh{vsn_s = Value}, Values);
%%db_insert(Db, [{server_key_fun, Value}|Values]) ->
%%    db_insert(Db#ssh{server_key_fun = Value}, Values);
%%db_insert(Db, [{error_code, Value}|Values]) ->
%%    db_insert(Db#ssh{error_code = Value}, Values);
%%db_insert(Db, [{options, Value}|Values]) ->
%%    db_insert(Db#ssh{options = Value}, Values);
%%db_insert(Db, [{next, Value}|Values]) ->
%%    db_insert(Db#ssh{next = Value}, Values).
%%
%%db_lookup(Db, Key) when atom(Key) ->
%%    [Value] = db_lookup(Db, [Key]),
%%    Value;
%%db_lookup(Db, Keys) ->
%%    db_lookup(Db, lists:reverse(Keys), []).
%%
%%db_lookup(Db, [], Acc) ->
%%    Acc;
%%db_lookup(Db, [owner_pid|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.owner_pid|Acc]);
%%db_lookup(Db, [socket|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.socket|Acc]);
%%db_lookup(Db, [cipher_cache|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.cipher_cache|Acc]);
%%db_lookup(Db, [plain_cache|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.plain_cache|Acc]);
%%db_lookup(Db, [msgs|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.msgs|Acc]);
%%db_lookup(Db, [msgs_to_send|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.msgs_to_send|Acc]);
%%db_lookup(Db, [ignore_one|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.ignore_one|Acc]);
%%db_lookup(Db, [owner_window|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.owner_window|Acc]);
%%db_lookup(Db, [kex|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.kex|Acc]);
%%db_lookup(Db, [session_id|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.session_id|Acc]);
%%db_lookup(Db, [iv_c2s|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.iv_c2s|Acc]);
%%db_lookup(Db, [iv_s2c|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.iv_s2c|Acc]);
%%db_lookup(Db, [seq_no_c2s|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.seq_no_c2s|Acc]);
%%db_lookup(Db, [seq_no_s2c|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.seq_no_s2c|Acc]);
%%db_lookup(Db, [kex_alg|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.kex_alg|Acc]);
%%db_lookup(Db, [pub_key_alg|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.pub_key_alg|Acc]);
%%db_lookup(Db, [cipher_c2s|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.cipher_c2s|Acc]);
%%db_lookup(Db, [cipher_s2c|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.cipher_s2c|Acc]);
%%db_lookup(Db, [mac_c2s|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.mac_c2s|Acc]);
%%db_lookup(Db, [mac_s2c|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.mac_s2c|Acc]);
%%db_lookup(Db, [compr_c2s|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.compr_c2s|Acc]);
%%db_lookup(Db, [compr_s2c|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.compr_s2c|Acc]);
%%db_lookup(Db, [enc_key_c2s|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.enc_key_c2s|Acc]);
%%db_lookup(Db, [enc_key_s2c|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.enc_key_s2c|Acc]);
%%db_lookup(Db, [auth_key_c2s|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.auth_key_c2s|Acc]);
%%db_lookup(Db, [auth_key_s2c|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.auth_key_s2c|Acc]);
%%db_lookup(Db, [bytes_sent|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.bytes_sent|Acc]);
%%db_lookup(Db, [bytes_recvd|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.bytes_recvd|Acc]);
%%db_lookup(Db, [vsn_c|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.vsn_c|Acc]);
%%db_lookup(Db, [vsn_s|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.vsn_s|Acc]);
%%db_lookup(Db, [server_key_fun|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.server_key_fun|Acc]);
%%db_lookup(Db, [error_code|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.error_code|Acc]);
%%db_lookup(Db, [options|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.options|Acc]);
%%db_lookup(Db, [next|Keys], Acc) ->
%%    db_lookup(Db, Keys, [Db#ssh.next|Acc]).
%%
%%db_destroy(Db) ->
%%    ok.

deliver_msgs(0, Pid, Msgs) ->
    {0, Msgs};
deliver_msgs(Count, Pid, []) ->
    {Count, []};
deliver_msgs(Count, Pid, [Msg|Msgs]) ->
    Pid ! {ssh_transport, self(), Msg},
    deliver_msgs(Count - 1, Pid, Msgs).

sized_binary(Binary) when binary(Binary) ->
    <<(size(Binary)):32/integer, Binary/binary>>;
sized_binary(List) ->
    sized_binary(list_to_binary(List)).

%%%----------------------------------------------------------------------
%%% #4     CODE FOR TEMPORARY CORRECTIONS
%%%----------------------------------------------------------------------

dump_config_imp(State) ->
    ListOfInterestingConfigs =
        [owner_pid,
         socket,
         cipher_cache,
         plain_cache,
         msgs,
         msgs_to_send,
         ignore_one,
         owner_window,
         session_id,
         seq_no_c2s,
         seq_no_s2c,
         kex_alg,
         pub_key_alg,
         cipher_c2s,
         cipher_s2c,
         mac_c2s,
         mac_s2c,
         compr_c2s,
         compr_s2c,
         enc_key_c2s,
         enc_key_s2c,
         auth_key_c2s,
         auth_key_s2c,
         bytes_sent,
         bytes_recvd,
         vsn_s,
         error_code],
    io:format("Ssh Transport ~p:~n", [self()]),
    [dump_config_imp(X, State) || X <- ListOfInterestingConfigs],
    ok.

dump_config_imp(CacheName, State)
  when CacheName == cipher_cache; CacheName == plain_cache ->
    NiceString =
        case db_lookup(State, CacheName) of
            {Size, Cache} when Size > 100 ->
                [Hd, _] = split_data([100], Cache),
                lists:flatten(
                  io_lib:format("~p", [{Size, list_to_binary([Hd])}]));
            {Size, Cache} ->
                lists:flatten(
                  io_lib:format("~p", [{Size, list_to_binary([Cache])}]))
        end,
    ShortString = case length(NiceString) of
                      Length when Length < 144 ->
                          NiceString;
                      _ ->
                          NiceString1 = lists:sublist(NiceString, 144),
                          {ok, NiceString2, _} =
                              regexp:sub(NiceString1, ", [^, ]*...$$", ", ..."),
                          NiceString2
                  end,
    io:format(" ~-13w: ~s~n", [CacheName, ShortString]);
dump_config_imp(vsn_s, State) ->
    <<VsnSSize:32/integer, VsnS/binary>> =
        list_to_binary([(db_lookup(State, vsn_s))]),
    io:format(" ~-13w: ~p~n", [vsn_s, binary_to_list(VsnS)]);
dump_config_imp(SequenceNo, State)
  when SequenceNo == seq_no_c2s; SequenceNo == seq_no_s2c ->
    <<Seq:32/integer>> = db_lookup(State, SequenceNo),
    io:format(" ~-13w: ~p~n", [SequenceNo, Seq]);
dump_config_imp(Msgs, State)
  when Msgs == msgs; Msgs == msgs_to_send ->
    case db_lookup(State, Msgs) of
        [] ->
            io:format(" ~-13w: ~p~n", [Msgs, []]);
        MsgsData ->
            TruncText =
                lists:flatten(io_lib:format("...(~p)", [length(MsgsData)])),
            LongString = lists:flatten(io_lib:format("~w", [MsgsData])),
            ShortString =
                case length(LongString) of
                    LLength when LLength > 144 ->
                        Tmp =
                            lists:sublist(LongString, 144 + length(TruncText)),
                        element(2, regexp:sub(Tmp, ", [^, ]*$$", TruncText));
                    _ ->
                        LongString        
                end,   
            io:format(" ~-13w: ~s~n", [Msgs, ShortString])
    end;
dump_config_imp(BinConf, State)
  when BinConf == session_id; BinConf == enc_key_s2c; BinConf == enc_key_c2s;
       BinConf == auth_key_s2c; BinConf == auth_key_c2s ->
    BinConfValue = db_lookup(State, BinConf),
    io:format(" ~-13w: ~p~n", [BinConf, hex_string(BinConfValue)]);
dump_config_imp(ConfigName, State) ->
    io:format(" ~-13w: ~p~n", [ConfigName, db_lookup(State, ConfigName)]).

