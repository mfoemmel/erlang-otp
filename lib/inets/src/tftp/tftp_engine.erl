%%%-------------------------------------------------------------------
%%% File    : tftp_engine.erl
%%% Author  : Hakan Mattsson <hakan@erix.ericsson.se>
%%% Description : Protocol engine for trivial FTP
%%%
%%% Created : 18 May 2004 by Hakan Mattsson <hakan@erix.ericsson.se>
%%%-------------------------------------------------------------------

-module(tftp_engine).

%%%-------------------------------------------------------------------
%%% Interface
%%%-------------------------------------------------------------------

%% application internal functions
-export([
         daemon_start/1,
         daemon_loop/3,
         client_start/4,
         common_loop/6,
         info/1,
         change_config/2
        ]).

%% module internal
-export([
         daemon_init/1, 
         server_init/2, 
         client_init/2,
         wait_for_msg/3
        ]).

%% sys callback functions
-export([
         system_continue/3,
         system_terminate/4,
         system_code_change/4
        ]).

-include("tftp.hrl").

-record(error, {what, code, text}).
-record(prepared, {status, result, block_no, next_data, prev_data}).
-record(transfer_res, {status, decoded_msg, prepared}).
-define(ERROR(What, Code, Text), #error{what = What, code = Code, text = Text}).

%%%-------------------------------------------------------------------
%%% Info
%%%-------------------------------------------------------------------

info(ToPid) when is_pid(ToPid) ->
    call(info, ToPid, timer:seconds(10)).

change_config(ToPid, Options) when is_pid(ToPid) ->
    BadKeys = [host, port, udp],
    BadOptions = [{Key, Val} || {Key, Val} <- Options,
                                BadKey <- BadKeys,
                                Key =:= BadKey],
    case BadOptions of
        [] ->
            call({change_config, Options}, ToPid, timer:seconds(10));
        [{Key, Val} | _] ->
            {error, {badarg, {Key, Val}}}
    end.

call(Req, ToPid, Timeout) when is_pid(ToPid) ->
    Type = process,
    Ref = erlang:monitor(Type, ToPid),
    ToPid ! {Req, Ref, self()},
    receive
        {Reply, Ref, FromPid} when FromPid =:= ToPid ->
            erlang:demonitor(Ref),
            Reply;
        {'DOWN', Ref, Type, FromPid, _Reason} when FromPid =:= ToPid ->
            {error, timeout}
    after Timeout ->
            {error, timeout}
    end.

reply(Reply, Ref, ToPid) ->
    ToPid ! {Reply, Ref, self()}.

%%%-------------------------------------------------------------------
%%% Daemon
%%%-------------------------------------------------------------------

%% Returns {ok, Port}
daemon_start(Options) when is_list(Options) ->
    Config = tftp_lib:parse_config(Options),
    proc_lib:start_link(?MODULE, daemon_init, [Config], infinity).

daemon_init(Config) when is_record(Config, config), 
                         is_pid(Config#config.parent_pid) ->
    process_flag(trap_exit, true),
    UdpOptions = prepare_daemon_udp(Config),
    case catch gen_udp:open(Config#config.udp_port, UdpOptions) of
        {ok, Socket} ->
            {ok, ActualPort} = inet:port(Socket),
            proc_lib:init_ack({ok, self()}),
            Config2 = Config#config{udp_socket = Socket,
                                    udp_port   = ActualPort},
            print_debug_info(Config2, daemon, open, #tftp_msg_req{filename = ""}),
            daemon_loop(Config2, 0, []);
        {error, Reason} ->
            Text = lists:flatten(io_lib:format("UDP open ~p -> ~p", [UdpOptions, Reason])),
            print_debug_info(Config, daemon, open, ?ERROR(open, undef, Text)),
            exit({gen_udp_open, UdpOptions, Reason});
        Reason ->
            Text = lists:flatten(io_lib:format("UDP open ~p -> ~p", [UdpOptions, Reason])),
            print_debug_info(Config, daemon, open, ?ERROR(open, undef, Text)),
            exit({gen_udp_open, UdpOptions, Reason})
    end.

prepare_daemon_udp(#config{udp_port = Port, udp_options = UdpOptions}) ->
    case lists:keymember(fd, 1, UdpOptions) of
        true ->
            %% Use explicit fd
            UdpOptions;
        false ->
            %% Use fd from setuid_socket_wrap, such as -tftpd_69
            InitArg = list_to_atom("tftpd_" ++ integer_to_list(Port)),
            case init:get_argument(InitArg) of
                {ok, [[FdStr]] = Badarg} when is_list(FdStr) ->
                    case catch list_to_integer(FdStr) of
                        Fd when is_integer(Fd) ->
                            [{fd, Fd} | UdpOptions];
                        {'EXIT', _} ->
                            exit({badarg, {prebound_fd, InitArg, Badarg}})
                    end;
                {ok, Badarg} ->
                    exit({badarg, {prebound_fd, InitArg, Badarg}});
                error ->
                    UdpOptions
            end
    end.

daemon_loop(DaemonConfig, N, Servers) ->
    receive
        {info, Ref, FromPid} when is_pid(FromPid) ->
            ServerInfo = [{n_conn, N} | [{server, P} || P <- Servers]],
            Info = internal_info(DaemonConfig, daemon) ++ ServerInfo,
            reply({ok, Info}, Ref, FromPid),
            ?MODULE:daemon_loop(DaemonConfig, N, Servers);
        {{change_config, Options}, Ref, FromPid} when is_pid(FromPid) ->
            case catch tftp_lib:parse_config(Options, DaemonConfig) of
                {'EXIT', Reason} ->
                    reply({error, Reason}, Ref, FromPid),
                    ?MODULE:daemon_loop(DaemonConfig, N, Servers);
                DaemonConfig2 when is_record(DaemonConfig2, config) ->
                    reply(ok, Ref, FromPid),
                    ?MODULE:daemon_loop(DaemonConfig2, N, Servers)
            end;
        {udp, Socket, RemoteHost, RemotePort, Bin} when is_binary(Bin) ->
            inet:setopts(Socket, [{active, once}]),
            ServerConfig = DaemonConfig#config{parent_pid = self(),
                                               udp_host   = RemoteHost,
                                               udp_port   = RemotePort},
            Msg = (catch tftp_lib:decode_msg(Bin)),
            print_debug_info(ServerConfig, daemon, recv, Msg),
            case Msg of
                Req when is_record(Req, tftp_msg_req), 
                N < DaemonConfig#config.max_conn ->
                    Args = [ServerConfig, Req],
                    Pid = proc_lib:spawn_link(?MODULE, server_init, Args),
                    ?MODULE:daemon_loop(DaemonConfig, N + 1, [Pid | Servers]);
                Req when is_record(Req, tftp_msg_req) ->
                    Reply = #tftp_msg_error{code = enospc,
                                            text = "Too many connections"},
                    send_msg(ServerConfig, daemon, Reply),
                    ?MODULE:daemon_loop(DaemonConfig, N, Servers);
                {'EXIT', Reply} when is_record(Reply, tftp_msg_error) ->
                    send_msg(ServerConfig, daemon, Reply),
                    ?MODULE:daemon_loop(DaemonConfig, N, Servers);
                Req  ->
                    Reply = #tftp_msg_error{code = badop,
                                            text = "Illegal TFTP operation"},
                    warning(DaemonConfig, "Daemon received: ~p from ~p:~p", [Req, RemoteHost, RemotePort]),
                    send_msg(ServerConfig, daemon, Reply),
                    ?MODULE:daemon_loop(DaemonConfig, N, Servers)
            end;
        {system, From, Msg} ->
            Misc = {daemon_loop, [DaemonConfig, N, Servers]},
            sys:handle_system_msg(Msg, From, DaemonConfig#config.parent_pid, ?MODULE, [], Misc);
        {'EXIT', Pid, Reason} when DaemonConfig#config.parent_pid =:= Pid ->
            close_port(DaemonConfig, daemon, #tftp_msg_req{filename = ""}),
            exit(Reason);
        {'EXIT', Pid, _Reason} = Info ->
            case lists:member(Pid, Servers) of
                true ->
                    ?MODULE:daemon_loop(DaemonConfig, N - 1, Servers -- [Pid]);
                false ->
                    warning(DaemonConfig, "Daemon received: ~p", [Info]),
                    ?MODULE:daemon_loop(DaemonConfig, N, Servers)
            end;
        Info ->
            warning(DaemonConfig, "Daemon received: ~p", [Info]),
            ?MODULE:daemon_loop(DaemonConfig, N, Servers)
    end.

%%%-------------------------------------------------------------------
%%% Server
%%%-------------------------------------------------------------------

server_init(Config0, Req) when is_record(Config0, config),
                              is_pid(Config0#config.parent_pid),
                              is_record(Req, tftp_msg_req) ->
    process_flag(trap_exit, true),
    Config = 
        case os:getenv("TFTPDEBUG") of
            false ->
                Config0;
            DebugLevel ->
                Config0#config{debug_level = list_to_atom(DebugLevel)}
        end,
    SuggestedOptions = Req#tftp_msg_req.options,
    UdpOptions = Config#config.udp_options,
    UdpOptions2 = lists:keydelete(fd, 1, UdpOptions),
    Config1 = Config#config{udp_options = UdpOptions2},
    Config2 = tftp_lib:parse_config(SuggestedOptions, Config1),
    SuggestedOptions2 = Config2#config.user_options,
    Req2 = Req#tftp_msg_req{options = SuggestedOptions2},
    case open_free_port(Config2, server, Req2) of
        {ok, Config3} ->
            Filename = Req#tftp_msg_req.filename,
            case match_callback(Filename, Config3#config.callbacks) of
                {ok, Callback} ->
                    print_debug_info(Config3, server, match, Callback),
                    case pre_verify_options(Config3, Req2) of
                        ok ->
                            case callback({open, server_open}, Config3, Callback, Req2) of
                                {Callback2, {ok, AcceptedOptions}} ->
                                    {LocalAccess,  _} = local_file_access(Req2),
                                    OptText = "Internal error. Not allowed to add new options.",
                                    case post_verify_options(Config3, Req2, AcceptedOptions, OptText) of
                                        {ok, Config4, Req3} when AcceptedOptions =/= [] ->
                                            Reply = #tftp_msg_oack{options = AcceptedOptions},
                                            BlockNo =
                                                case LocalAccess of
                                                    read  -> 0;
                                                    write -> 1
                                                end,
                                            {Config5, Callback3, TransferRes} = 
                                                transfer(Config4, Callback2, Req3, Reply, LocalAccess, BlockNo, #prepared{}),
                                            common_loop(Config5, Callback3, Req3, TransferRes, LocalAccess, BlockNo);
                                        {ok, Config4, Req3} when LocalAccess =:= write ->
                                            BlockNo = 0,
                                            common_ack(Config4, Callback2, Req3, LocalAccess, BlockNo, #prepared{});
                                        {ok, Config4, Req3} when LocalAccess =:= read ->
                                            BlockNo = 0,
                                            common_read(Config4, Callback2, Req3, LocalAccess, BlockNo, BlockNo, #prepared{});
                                        {error, {Code, Text}} ->
                                            {undefined, Error} =
                                                callback({abort, {Code, Text}}, Config3, Callback2, Req2),
                                            send_msg(Config3, Req, Error),
                                            terminate(Config3, Req2, ?ERROR(post_verify_options, Code, Text))
                                    end;
                                {undefined, #tftp_msg_error{code = Code, text = Text} = Error} ->
                                    send_msg(Config3, Req, Error),
                                    terminate(Config3, Req, ?ERROR(server_open, Code, Text))
                            end;
                        {error, {Code, Text}} ->
                            {undefined, Error} =
                                callback({abort, {Code, Text}}, Config2, Callback, Req2),
                            send_msg(Config2, Req, Error),
                            terminate(Config2, Req2, ?ERROR(pre_verify_options, Code, Text))
                    end;
                {error, #tftp_msg_error{code = Code, text = Text} = Error} ->
                    send_msg(Config3, Req, Error),
                    terminate(Config3, Req, ?ERROR(match_callback, Code, Text))
            end;
        #error{} = Error ->
            terminate(Config2, Req, Error)
    end.

%%%-------------------------------------------------------------------
%%% Client
%%%-------------------------------------------------------------------

%% LocalFilename = filename() | 'binary' | binary()
%% Returns {ok, LastCallbackState} | {error, Reason}
client_start(Access, RemoteFilename, LocalFilename, Options) ->
    Config = tftp_lib:parse_config(Options),
    Config2 = Config#config{parent_pid      = self(),
                            udp_socket      = undefined},
    Req = #tftp_msg_req{access         = Access, 
                        filename       = RemoteFilename, 
                        mode           = lookup_mode(Config2#config.user_options),
                        options        = Config2#config.user_options,
                        local_filename = LocalFilename},
    Args = [Config2, Req],
    case proc_lib:start_link(?MODULE, client_init, Args, infinity) of
        {ok, LastCallbackState} ->
            {ok, LastCallbackState};
        {error, Error} ->
            {error, Error}
    end.

client_init(Config, Req) when is_record(Config, config),
                               is_pid(Config#config.parent_pid),
                               is_record(Req, tftp_msg_req) ->
    process_flag(trap_exit, true),
    %% Config = 
    %%  case os:getenv("TFTPDEBUG") of
    %%      false ->
    %%          Config0;
    %%      "none" ->
    %%          Config0;
    %%      DebugLevel ->
    %%          io:format("TFTPDEBUG: ~s\n", [DebugLevel]),
    %%          Config0#config{debug_level = list_to_atom(DebugLevel)}
    %%  end,
    case open_free_port(Config, client, Req) of
        {ok, Config2} ->
            Req2 =
                case Config2#config.use_tsize of
                    true ->
                        SuggestedOptions = Req#tftp_msg_req.options,
                        SuggestedOptions2 = tftp_lib:replace_val("tsize", "0", SuggestedOptions),
                        Req#tftp_msg_req{options = SuggestedOptions2};
                    false ->
                        Req
                end,
            LocalFilename = Req2#tftp_msg_req.local_filename,
            case match_callback(LocalFilename, Config2#config.callbacks) of
                {ok, Callback} ->
                    print_debug_info(Config2, client, match, Callback),
                    client_prepare(Config2, Callback, Req2);                
                {error, #tftp_msg_error{code = Code, text = Text}} ->
                    terminate(Config, Req, ?ERROR(match, Code, Text))
            end;
        #error{} = Error ->
            terminate(Config, Req, Error)
    end.

client_prepare(Config, Callback, Req) ->
    case pre_verify_options(Config, Req) of
        ok ->
            case callback({open, client_prepare}, Config, Callback, Req) of
                {Callback2, {ok, AcceptedOptions}} ->
                    OptText = "Internal error. Not allowed to add new options.",
                    case post_verify_options(Config, Req, AcceptedOptions, OptText) of
                        {ok, Config2, Req2} ->
                            {LocalAccess, _} = local_file_access(Req2),
			    BlockNo = 0,
                            {Config3, Callback3, TransferRes} =
                                transfer(Config2, Callback2, Req2, Req2, LocalAccess, BlockNo, #prepared{}),
                            client_open(Config3, Callback3, Req2, BlockNo, TransferRes);
                        {error, {Code, Text}} ->
                            callback({abort, {Code, Text}}, Config, Callback2, Req),
                            terminate(Config, Req, ?ERROR(post_verify_options, Code, Text))
                    end;
                {undefined, #tftp_msg_error{code = Code, text = Text}} ->
                    terminate(Config, Req, ?ERROR(client_prepare, Code, Text))
            end;
        {error, {Code, Text}} ->
            callback({abort, {Code, Text}}, Config, Callback, Req),
            terminate(Config, Req, ?ERROR(pre_verify_options, Code, Text))
    end.

client_open(Config, Callback, Req, BlockNo, #transfer_res{status = Status, decoded_msg = DecodedMsg, prepared = Prepared}) ->
    {LocalAccess, _} = local_file_access(Req),
    case Status of
        ok when is_record(Prepared, prepared) ->
            case DecodedMsg of
                Msg when record(Msg, tftp_msg_oack) ->
                    ServerOptions = Msg#tftp_msg_oack.options,
                    OptText = "Protocol violation. Server is not allowed new options",
                    case post_verify_options(Config, Req, ServerOptions, OptText) of
                        {ok, Config2, Req2} ->              
                            {Config3, Callback2, Req3} =
                                do_client_open(Config2, Callback, Req2),
                            case LocalAccess of
                                read ->
                                    common_read(Config3, Callback2, Req3, LocalAccess, BlockNo, BlockNo, Prepared);
                                write ->
                                    common_ack(Config3, Callback2, Req3, LocalAccess, BlockNo, Prepared)
                            end;
                        {error, {Code, Text}} ->
                            {undefined, Error} =
                                callback({abort, {Code, Text}}, Config, Callback, Req),
                            send_msg(Config, Req, Error),
                            terminate(Config, Req, ?ERROR(verify_server_options, Code, Text))
                    end;
                #tftp_msg_ack{block_no = ActualBlockNo} when LocalAccess =:= read ->
                    Req2 = Req#tftp_msg_req{options = []},
                    {Config2, Callback2, Req2} = do_client_open(Config, Callback, Req2),
                    ExpectedBlockNo = 0,
                    common_read(Config2, Callback2, Req2, LocalAccess, ExpectedBlockNo, ActualBlockNo, Prepared);
                #tftp_msg_data{block_no = ActualBlockNo, data = Data} when LocalAccess =:= write ->
                    Req2 = Req#tftp_msg_req{options = []},
                    {Config2, Callback2, Req2} = do_client_open(Config, Callback, Req2),
                    ExpectedBlockNo = 1,
                    common_write(Config2, Callback2, Req2, LocalAccess, ExpectedBlockNo, ActualBlockNo, Data, Prepared);
                %% #tftp_msg_error{code = Code, text = Text} when Req#tftp_msg_req.options =/= [] ->
                %%     %% Retry without options
                %%     callback({abort, {Code, Text}}, Config, Callback, Req),
                %%     Req2 = Req#tftp_msg_req{options = []},
                %%     client_prepare(Config, Callback, Req2);
                #tftp_msg_error{code = Code, text = Text} ->
                    callback({abort, {Code, Text}}, Config, Callback, Req),
                    terminate(Config, Req, ?ERROR(client_open, Code, Text));
                {'EXIT', #tftp_msg_error{code = Code, text = Text}} ->
                    callback({abort, {Code, Text}}, Config, Callback, Req),
                    terminate(Config, Req, ?ERROR(client_open, Code, Text));
                Msg when is_tuple(Msg) ->
                    Code = badop,
                    Text = "Illegal TFTP operation",
                    {undefined, Error} =
                        callback({abort, {Code, Text}}, Config, Callback, Req),
                    send_msg(Config, Req, Error),
                    Text2 = lists:flatten([Text, ". ", io_lib:format("~p", [element(1, Msg)])]),
                    terminate(Config, Req, ?ERROR(client_open, Code, Text2))
            end;
	error when is_record(Prepared, tftp_msg_error) ->
	    #tftp_msg_error{code = Code, text = Text} = Prepared,
            callback({abort, {Code, Text}}, Config, Callback, Req),
            terminate(Config, Req, ?ERROR(client_open, Code, Text))
    end.

do_client_open(Config, Callback, Req) ->
    case callback({open, client_open}, Config, Callback, Req) of
        {Callback2, {ok, FinalOptions}} ->
            OptText = "Internal error. Not allowed to change options.",
            case post_verify_options(Config, Req, FinalOptions, OptText) of
                {ok, Config2, Req2} ->
                    {Config2, Callback2, Req2};
                {error, {Code, Text}} ->
                    {undefined, Error} =
                        callback({abort, {Code, Text}}, Config, Callback2, Req),
                    send_msg(Config, Req, Error),
                    terminate(Config, Req, ?ERROR(post_verify_options, Code, Text))
            end;
        {undefined, #tftp_msg_error{code = Code, text = Text} = Error} ->
            send_msg(Config, Req, Error),
            terminate(Config, Req, ?ERROR(client_open, Code, Text))
    end.

%%%-------------------------------------------------------------------
%%% Common loop for both client and server
%%%-------------------------------------------------------------------

common_loop(Config, Callback, Req, #transfer_res{status = Status, decoded_msg = DecodedMsg, prepared = Prepared}, LocalAccess, ExpectedBlockNo) ->
    %%    Config =
    %%  case os:getenv("TFTPMAX") of
    %%      false -> 
    %%          Config0;
    %%      MaxBlockNoStr when Config0#config.debug_level =/= none ->
    %%          case list_to_integer(MaxBlockNoStr) of
    %%              MaxBlockNo when ExpectedBlockNo > MaxBlockNo ->
    %%                  io:format("TFTPMAX: ~p\n", [MaxBlockNo]),
    %%                  io:format("TFTPDEBUG: none\n", []),
    %%                  Config0#config{debug_level = none};
    %%              _ ->
    %%                  Config0
    %%          end;
    %%      _MaxBlockNoStr ->
    %%          Config0 
    %%  end,
    case Status of
        ok when is_record(Prepared, prepared) ->
            case DecodedMsg of
                #tftp_msg_ack{block_no = ActualBlockNo} when LocalAccess =:= read ->
                    common_read(Config, Callback, Req, LocalAccess, ExpectedBlockNo, ActualBlockNo, Prepared);
                #tftp_msg_data{block_no = ActualBlockNo, data = Data} when LocalAccess =:= write ->
                    common_write(Config, Callback, Req, LocalAccess, ExpectedBlockNo, ActualBlockNo, Data, Prepared);
                #tftp_msg_error{code = Code, text = Text} ->
                    callback({abort, {Code, Text}}, Config, Callback, Req),
                    terminate(Config, Req, ?ERROR(common_loop, Code, Text));
                {'EXIT', #tftp_msg_error{code = Code, text = Text} = Error} ->
                    callback({abort, {Code, Text}}, Config, Callback, Req),
                    send_msg(Config, Req, Error),
                    terminate(Config, Req, ?ERROR(common_loop, Code, Text));
                Msg when is_tuple(Msg) ->
                    Code = badop,
                    Text = "Illegal TFTP operation",
                    {undefined, Error} =
                        callback({abort, {Code, Text}}, Config, Callback, Req),
                    send_msg(Config, Req, Error),
                    Text2 = lists:flatten([Text, ". ", io_lib:format("~p", [element(1, Msg)])]),
                    terminate(Config, Req, ?ERROR(common_loop, Code, Text2))
            end;
	error when is_record(Prepared, tftp_msg_error) ->
	    #tftp_msg_error{code = Code, text = Text} = Prepared,
            send_msg(Config, Req, Prepared),
            terminate(Config, Req, ?ERROR(transfer, Code, Text))
    end.

common_read(Config, _, Req, _, _, _, #prepared{status = terminate, result = Result}) ->
    terminate(Config, Req, {ok, Result});
common_read(Config, Callback, Req, LocalAccess, ExpectedBlockNo, ActualBlockNo, Prepared)
  when ActualBlockNo =:= ExpectedBlockNo, is_record(Prepared, prepared) ->
    case early_read(Config, Callback, Req, LocalAccess, ActualBlockNo, Prepared) of
        {Callback2,  #prepared{status = more, next_data = Data} = Prepared2} when is_binary(Data) ->
	    Prepared3 = Prepared2#prepared{prev_data = Data, next_data = undefined},
            do_common_read(Config, Callback2, Req, LocalAccess, ActualBlockNo, Data, Prepared3);
        {undefined, #prepared{status = last, next_data = Data} = Prepared2} when is_binary(Data) ->
	    Prepared3 = Prepared2#prepared{status = terminate},
            do_common_read(Config, undefined, Req, LocalAccess, ActualBlockNo, Data, Prepared3);
        {undefined, #prepared{status = error, result = Error}} ->
	    #tftp_msg_error{code = Code, text = Text} = Error,
            send_msg(Config, Req, Error),
            terminate(Config, Req, ?ERROR(read, Code, Text))
    end;
common_read(Config, Callback, Req, LocalAccess, ExpectedBlockNo, ActualBlockNo, Prepared) 
  when ActualBlockNo =:= (ExpectedBlockNo - 1), is_record(Prepared, prepared) ->
    case Prepared of
        #prepared{status = more, prev_data = Data} when is_binary(Data) ->
            do_common_read(Config, Callback, Req, LocalAccess, ActualBlockNo, Data, Prepared);
        #prepared{status = last, prev_data = Data} when is_binary(Data) ->
            do_common_read(Config, Callback, Req, LocalAccess, ActualBlockNo, Data, Prepared);
        #prepared{status = error, result = Error} ->
	    #tftp_msg_error{code = Code, text = Text} = Error,
            send_msg(Config, Req, Error),
            terminate(Config, Req, ?ERROR(read, Code, Text))
    end;
common_read(Config, Callback, Req, LocalAccess, ExpectedBlockNo, ActualBlockNo, Prepared) 
  when ActualBlockNo =< ExpectedBlockNo , is_record(Prepared, prepared) ->
    %% error_logger:error_msg("TFTP READ ~s: Expected block ~p but got block ~p - IGNORED\n",
    %%			   [Req#tftp_msg_req.filename, ExpectedBlockNo, ActualBlockNo]),
    case Prepared of
        #prepared{status = more, prev_data = Data} when is_binary(Data) ->
	    Reply = #tftp_msg_data{block_no = ExpectedBlockNo, data = Data},
	    {Config2, Callback2, TransferRes} =
		wait_for_msg_and_handle_timeout(Config, Callback, Req, Reply, LocalAccess, ExpectedBlockNo, Prepared),
	    ?MODULE:common_loop(Config2, Callback2, Req, TransferRes, LocalAccess, ExpectedBlockNo);
        #prepared{status = last, prev_data = Data} when is_binary(Data) ->
	    Reply = #tftp_msg_data{block_no = ExpectedBlockNo, data = Data},
	    {Config2, Callback2, TransferRes} =
		wait_for_msg_and_handle_timeout(Config, Callback, Req, Reply, LocalAccess, ExpectedBlockNo, Prepared),
	    ?MODULE:common_loop(Config2, Callback2, Req, TransferRes, LocalAccess, ExpectedBlockNo);
        #prepared{status = error, result = Error} ->
	    #tftp_msg_error{code = Code, text = Text} = Error,
            send_msg(Config, Req, Error),
            terminate(Config, Req, ?ERROR(read, Code, Text))
    end;
common_read(Config, Callback, Req, _LocalAccess, ExpectedBlockNo, ActualBlockNo, Prepared)
  when is_record(Prepared, prepared) ->
    Code = badblk,
    Text = "Unknown transfer ID = " ++ 
        integer_to_list(ActualBlockNo) ++ " (" ++ integer_to_list(ExpectedBlockNo) ++ ")", 
    {undefined, Error} =
        callback({abort, {Code, Text}}, Config, Callback, Req),
    send_msg(Config, Req, Error),
    terminate(Config, Req, ?ERROR(read, Code, Text)).

do_common_read(Config, Callback, Req, LocalAccess, BlockNo, Data, Prepared)
  when is_binary(Data), is_record(Prepared, prepared) ->
    NextBlockNo = BlockNo + 1,
    case NextBlockNo =< 65535 of
        true ->
            Reply = #tftp_msg_data{block_no = NextBlockNo, data = Data},
            {Config2, Callback2, TransferRes} =
		transfer(Config, Callback, Req, Reply, LocalAccess, NextBlockNo, Prepared),
            ?MODULE:common_loop(Config2, Callback2, Req, TransferRes, LocalAccess, NextBlockNo);
        false ->
            Code = badblk,
            Text = "Too big transfer ID = " ++ 
                integer_to_list(NextBlockNo) ++ " > 65535", 
            {undefined, Error} =
                callback({abort, {Code, Text}}, Config, Callback, Req),
            send_msg(Config, Req, Error),
            terminate(Config, Req, ?ERROR(read, Code, Text))
    end.

common_write(Config, _, Req, _, _, _, _, #prepared{status = terminate, result = Result}) ->
    terminate(Config, Req, {ok, Result});
common_write(Config, Callback, Req, LocalAccess, ExpectedBlockNo, ActualBlockNo, Data, Prepared)
  when ActualBlockNo =:= ExpectedBlockNo, is_binary(Data), is_record(Prepared, prepared) ->
    case callback({write, Data}, Config, Callback, Req) of
        {Callback2, #prepared{status = more} = Prepared2} ->
            common_ack(Config, Callback2, Req, LocalAccess, ActualBlockNo, Prepared2);
        {undefined, #prepared{status = last, result = Result} = Prepared2} ->
            Config2 = pre_terminate(Config, Req, {ok, Result}),
	    Prepared3 = Prepared2#prepared{status = terminate},
            common_ack(Config2, undefined, Req, LocalAccess, ActualBlockNo, Prepared3);
        {undefined, #prepared{status = error, result = Error}} ->
	    #tftp_msg_error{code = Code, text = Text} = Error,
            send_msg(Config, Req, Error),
            terminate(Config, Req, ?ERROR(write, Code, Text))
    end;
common_write(Config, Callback, Req, LocalAccess, ExpectedBlockNo, ActualBlockNo, Data, Prepared)
  when ActualBlockNo =:= (ExpectedBlockNo - 1), is_binary(Data), is_record(Prepared, prepared) ->
    common_ack(Config, Callback, Req, LocalAccess, ExpectedBlockNo - 1, Prepared);
common_write(Config, Callback, Req, LocalAccess, ExpectedBlockNo, ActualBlockNo, Data, Prepared)
  when ActualBlockNo =< ExpectedBlockNo, is_binary(Data), is_record(Prepared, prepared) ->
    %% error_logger:error_msg("TFTP WRITE ~s: Expected block ~p but got block ~p - IGNORED\n",
    %% [Req#tftp_msg_req.filename, ExpectedBlockNo, ActualBlockNo]),
    Reply = #tftp_msg_ack{block_no = ExpectedBlockNo},
    {Config2, Callback2, TransferRes} = 
        wait_for_msg_and_handle_timeout(Config, Callback, Req, Reply, LocalAccess, ExpectedBlockNo, Prepared),
    ?MODULE:common_loop(Config2, Callback2, Req, TransferRes, LocalAccess, ExpectedBlockNo);
common_write(Config, Callback, Req, _, ExpectedBlockNo, ActualBlockNo, Data, Prepared)
  when is_binary(Data), is_record(Prepared, prepared) ->
    Code = badblk,
    Text = "Unknown transfer ID = " ++ 
        integer_to_list(ActualBlockNo) ++ " (" ++ integer_to_list(ExpectedBlockNo) ++ ")", 
    {undefined, Error} =
        callback({abort, {Code, Text}}, Config, Callback, Req),
    send_msg(Config, Req, Error),
    terminate(Config, Req, ?ERROR(write, Code, Text)).

common_ack(Config, Callback, Req, LocalAccess, BlockNo, Prepared) 
  when is_record(Prepared, prepared) ->
    Reply = #tftp_msg_ack{block_no = BlockNo},
    NextBlockNo = BlockNo + 1,
    {Config2, Callback2, TransferRes} = 
        transfer(Config, Callback, Req, Reply, LocalAccess, NextBlockNo, Prepared),
    case NextBlockNo =< 65535 of
        true ->   
            ?MODULE:common_loop(Config2, Callback2, Req, TransferRes, LocalAccess, NextBlockNo);
        false ->
            Code = badblk,
            Text = "Too big transfer ID = " ++ 
                integer_to_list(NextBlockNo) ++ " > 65535", 
            {undefined, Error} =
                callback({abort, {Code, Text}}, Config, Callback2, Req),
            send_msg(Config, Req, Error),
            terminate(Config, Req, ?ERROR(read, Code, Text))
    end.

pre_terminate(Config, Req, Result) ->
    if
        Req#tftp_msg_req.local_filename =/= undefined,
        Config#config.parent_pid =/= undefined ->
            proc_lib:init_ack(Result),
            unlink(Config#config.parent_pid),
            Config#config{parent_pid = undefined, polite_ack = true};
        true ->
            Config#config{polite_ack = true}
    end.

terminate(Config, Req, Result) ->
    Result2 =
        case Result of
            {ok, _} ->
                Result;
            #error{what = What, code = Code, text = Text} = Error ->
                print_debug_info(Config, Req, What, Error),
                {error, {What, Code, Text}}
	end,  
    if
        Config#config.parent_pid =:= undefined ->
            close_port(Config, client, Req),
            exit(normal);
        Req#tftp_msg_req.local_filename =/= undefined  ->
            %% Client
            close_port(Config, client, Req),
            proc_lib:init_ack(Result2),
            unlink(Config#config.parent_pid),
            exit(normal);
        true ->
            %% Server
            close_port(Config, server, Req),
            exit(shutdown)                  
    end.

close_port(Config, Who, Data) ->
    case Config#config.udp_socket of
        undefined -> 
            ignore;
        Socket    -> 
            print_debug_info(Config, Who, close, Data),
            gen_udp:close(Socket)
    end.

open_free_port(Config, Who, Data) when is_record(Config, config) ->
    UdpOptions = Config#config.udp_options,
    case Config#config.port_policy of
        random ->
            %% BUGBUG: Should be a random port
            case catch gen_udp:open(0, UdpOptions) of
                {ok, Socket} ->
                    Config2 = Config#config{udp_socket = Socket},
                    print_debug_info(Config2, Who, open, Data),
                    {ok, Config2};
                {error, Reason} ->
                    Text = lists:flatten(io_lib:format("UDP open ~p -> ~p", [[0 | UdpOptions], Reason])),
                    ?ERROR(open, undef, Text);
                {'EXIT', _} = Reason ->
                    Text = lists:flatten(io_lib:format("UDP open ~p -> ~p", [[0 | UdpOptions], Reason])),
                    ?ERROR(open, undef, Text)
            end;
        {range, Port, Max} when Port =< Max ->
            case catch gen_udp:open(Port, UdpOptions) of
                {ok, Socket} ->
                    Config2 = Config#config{udp_socket = Socket},
                    print_debug_info(Config2, Who, open, Data),
                    {ok, Config2};
                {error, eaddrinuse} ->
                    PortPolicy = {range, Port + 1, Max},
                    Config2 = Config#config{port_policy = PortPolicy},
                    open_free_port(Config2, Who, Data);
                {error, Reason} ->
                    Text = lists:flatten(io_lib:format("UDP open ~p -> ~p", [[Port | UdpOptions], Reason])),
                    ?ERROR(open, undef, Text);
                {'EXIT', _} = Reason->
                    Text = lists:flatten(io_lib:format("UDP open ~p -> ~p", [[Port | UdpOptions], Reason])),
                    ?ERROR(open, undef, Text)
            end;
        {range, Port, _Max} ->
            Reason = "Port range exhausted",
            Text = lists:flatten(io_lib:format("UDP open ~p -> ~p", [[Port | UdpOptions], Reason])),
            ?ERROR(Who, undef, Text)
    end.

%%-------------------------------------------------------------------
%% Transfer
%%-------------------------------------------------------------------

%% Returns {Config, Callback, #transfer_res{}}
transfer(Config, Callback, Req, Msg, LocalAccess, NextBlockNo, Prepared) 
  when is_record(Prepared, prepared) ->
    IoList = tftp_lib:encode_msg(Msg),
    Retries = 6, % BUGBUG: Add as configuration parameter
    do_transfer(Config, Callback, Req, Msg, IoList, LocalAccess, NextBlockNo, Prepared, Retries).

do_transfer(Config, Callback, Req, Msg, IoList, LocalAccess, NextBlockNo, Prepared, Retries)
  when is_record(Prepared, prepared), is_integer(Retries), Retries >= 0 ->
    case do_send_msg(Config, Req, Msg, IoList) of
        ok ->
            {Callback2, Prepared2} = 
                early_read(Config, Callback, Req, LocalAccess, NextBlockNo, Prepared),
	    do_wait_for_msg_and_handle_timeout(Config, Callback2, Req, Msg, IoList, LocalAccess, NextBlockNo, Prepared2, Retries);
	{error, _Reason} when Retries > 0 ->
	    Retries2 = Retries - 1,
            do_transfer(Config, Callback, Req, Msg, IoList, LocalAccess, NextBlockNo, Prepared, Retries2);
        {error, Reason} ->
            Code = undef,
            Text = lists:flatten(io_lib:format("Transfer failed - giving up -> ~p", [Reason])),
	    Error = #tftp_msg_error{code = Code, text = Text},
            {Config, Callback, #transfer_res{status = error, prepared = Error}}
    end.

wait_for_msg_and_handle_timeout(Config, Callback, Req, Msg, LocalAccess, NextBlockNo, Prepared) ->
    IoList = tftp_lib:encode_msg(Msg),
    Retries = 6, % BUGBUG: Add as configuration parameter
    do_wait_for_msg_and_handle_timeout(Config, Callback, Req, Msg, IoList, LocalAccess, NextBlockNo, Prepared, Retries).

do_wait_for_msg_and_handle_timeout(Config, Callback, Req, Msg, IoList, LocalAccess, NextBlockNo, Prepared, Retries) ->
    Code = undef,
    Text = "Transfer timed out.",
    case wait_for_msg(Config, Callback, Req) of
	timeout when Config#config.polite_ack =:= true ->
	    do_send_msg(Config, Req, Msg, IoList),
	    case Prepared of
		#prepared{status = terminate, result = Result} ->
		    terminate(Config, Req, {ok, Result});
		#prepared{} ->
		    terminate(Config, Req, ?ERROR(transfer, Code, Text))
	    end;
	timeout when Retries > 0 ->
	    Retries2 = Retries - 1,
	    do_transfer(Config, Callback, Req, Msg, IoList, LocalAccess, NextBlockNo, Prepared, Retries2);
	timeout ->
	    Error = #tftp_msg_error{code = Code, text = Text},
	    {Config, Callback, #transfer_res{status = error, prepared = Error}};
	{Config2, DecodedMsg} ->
	    {Config2, Callback, #transfer_res{status = ok, decoded_msg = DecodedMsg, prepared = Prepared}}
    end.

send_msg(Config, Req, Msg) ->
    case catch tftp_lib:encode_msg(Msg) of
        {'EXIT', Reason} ->
            Code = undef,
            Text = "Internal error. Encode failed",
            Msg2 = #tftp_msg_error{code = Code, text = Text, details = Reason},
            send_msg(Config, Req, Msg2);
        IoList ->
            do_send_msg(Config, Req, Msg, IoList)
    end.

do_send_msg(Config, Req, Msg, IoList) ->
    print_debug_info(Config, Req, send, Msg),
    %% case os:getenv("TFTPDUMP") of
    %%  false     ->
    %%      ignore;
    %%  DumpPath  ->
    %%      trace_udp_send(Req, Msg, IoList, DumpPath)
    %% end,
    Res = gen_udp:send(Config#config.udp_socket,
                       Config#config.udp_host,
                       Config#config.udp_port,
                       IoList),
    case Res of
        ok ->
            ok;
        {error, einval = Reason} ->
            error("Stacktrace; ~p\n gen_udp:send(~p, ~p, ~p, ~p) -> ~p\n", 
                  [erlang:get_stacktrace(),
                   Config#config.udp_socket,
                   Config#config.udp_host,
                   Config#config.udp_port,
                   IoList,
                   {error, Reason}]);
        {error, Reason} ->
            {error, Reason} 
    end.

%% trace_udp_send(#tftp_msg_req{filename = [$/ | RelFile]} = Req, Msg, IoList, DumpPath) ->
%%     trace_udp_send(Req#tftp_msg_req{filename = RelFile}, Msg, IoList, DumpPath);
%% trace_udp_send(#tftp_msg_req{filename = RelFile},
%%             #tftp_msg_data{block_no = BlockNo, data = Data},
%%             _IoList,
%%             DumpPath) ->
%%     File = filename:join([DumpPath, RelFile, "block" ++ string:right(integer_to_list(BlockNo), 5, $0)  ++ ".dump"]),
%%     if
%%      (BlockNo rem 1000) =:= 1 -> 
%%          io:format("TFTPDUMP: Data    ~s\n", [File]);
%%      true ->
%%          ignore
%%     end,
%%     ok = filelib:ensure_dir(File),
%%     ok = file:write_file(File, Data);
%% trace_udp_send(#tftp_msg_req{filename = RelFile}, Msg, _IoList, _DumpPath) ->
%%     io:format("TFTPDUMP: No data  ~s -> ~p\n", [RelFile, element(1, Msg)]).

wait_for_msg(Config, Callback, Req) ->
    receive
        {info, Ref, FromPid} when is_pid(FromPid) ->
            Type =
                case Req#tftp_msg_req.local_filename =/= undefined of
                    true  -> client;
                    false -> server
                end,
            Info = internal_info(Config, Type),
            reply({ok, Info}, Ref, FromPid),
            wait_for_msg(Config, Callback, Req);
        {{change_config, Options}, Ref, FromPid} when is_pid(FromPid) ->
            case catch tftp_lib:parse_config(Options, Config) of
                {'EXIT', Reason} ->
                    reply({error, Reason}, Ref, FromPid),
                    wait_for_msg(Config, Callback, Req);
                Config2 when is_record(Config2, config) ->
                    reply(ok, Ref, FromPid),
                    wait_for_msg(Config2, Callback, Req)
            end;
        {udp, Socket, RemoteHost, RemotePort, Bin} when is_binary(Bin),
                                             Callback#callback.block_no =:= undefined ->
            %% Client prepare
            inet:setopts(Socket, [{active, once}]),
            Config2 = Config#config{udp_host = RemoteHost,
                                    udp_port = RemotePort},
            DecodedMsg = (catch tftp_lib:decode_msg(Bin)),
            print_debug_info(Config2, Req, recv, DecodedMsg),
            {Config2, DecodedMsg};
        {udp, Socket, Host, Port, Bin} when is_binary(Bin),
                                            Config#config.udp_host =:= Host,
                                            Config#config.udp_port =:= Port ->
            inet:setopts(Socket, [{active, once}]),
            DecodedMsg = (catch tftp_lib:decode_msg(Bin)),
            print_debug_info(Config, Req, recv, DecodedMsg),
            {Config, DecodedMsg};
        {system, From, Msg} ->
            Misc = {wait_for_msg, [Config, Callback, Req]},
            sys:handle_system_msg(Msg, From, Config#config.parent_pid, ?MODULE, [], Misc);
        {'EXIT', Pid, _Reason} when Config#config.parent_pid =:= Pid ->
            Code = undef,
            Text = "Parent exited.",
            terminate(Config, Req, ?ERROR(wait_for_msg, Code, Text));
        Msg when Req#tftp_msg_req.local_filename =/= undefined ->
            warning(Config, "Client received : ~p", [Msg]),
            wait_for_msg(Config, Callback, Req);
        Msg when Req#tftp_msg_req.local_filename =:= undefined ->
            warning(Config, "Server received : ~p", [Msg]),
            wait_for_msg(Config, Callback, Req)
    after Config#config.timeout * 1000 ->
            print_debug_info(Config, Req, recv, timeout),
            timeout
    end.

early_read(Config, Callback, Req, LocalAccess, _NextBlockNo,
	   #prepared{status = Status, next_data = NextData, prev_data = PrevData} = Prepared) ->
    if
	Status =/= terminate,
	LocalAccess =:= read,
	Callback#callback.block_no =/= undefined,
	NextData =:= undefined ->
	    case callback(LocalAccess, Config, Callback, Req) of
		{undefined, Error} when is_record(Error, tftp_msg_error) ->
		    {undefined, Error};
		{Callback2, Prepared2} when is_record(Prepared2, prepared)->
		    {Callback2, Prepared2#prepared{prev_data = PrevData}}
	    end;
	true ->
	    {Callback, Prepared}
    end.

%%-------------------------------------------------------------------
%% Callback
%%-------------------------------------------------------------------

callback(Access, Config, Callback, Req) ->
    {Callback2, Result} =
        do_callback(Access, Config, Callback, Req),
    print_debug_info(Config, Req, call, {Callback2, Result}),
    {Callback2, Result}.

do_callback(read = Fun, Config, Callback, Req) 
  when is_record(Config, config),
       is_record(Callback, callback),
       is_record(Req, tftp_msg_req) ->
    Args =  [Callback#callback.state],
    NextBlockNo = Callback#callback.block_no + 1,
    case catch apply(Callback#callback.module, Fun, Args) of
        {more, Bin, NewState} when is_binary(Bin) ->
            Count   = Callback#callback.count + size(Bin),
            Callback2 = Callback#callback{state    = NewState, 
                                          block_no = NextBlockNo,
                                          count    = Count},
	    Prepared = #prepared{status    = more,
				 result    = undefined,
				 block_no  = NextBlockNo,
				 next_data = Bin},
	    verify_count(Config, Callback2, Req, Prepared);
        {last, Bin, Result} when is_binary(Bin) ->
	    Prepared = #prepared{status    = last,
				 result    = Result,
				 block_no  = NextBlockNo,
				 next_data = Bin},
	    {undefined, Prepared};
        {error, {Code, Text}} ->
	    Error = #tftp_msg_error{code = Code, text = Text},
	    Prepared = #prepared{status    = error,
				 result    = Error},
            {undefined, Prepared};
        Illegal ->
            Code = undef,
            Text = "Internal error. File handler error.",
            callback({abort, {Code, Text, Illegal}}, Config, Callback, Req)
    end;
do_callback({write = Fun, Bin}, Config, Callback, Req)
  when is_record(Config, config),
       is_record(Callback, callback),
       is_record(Req, tftp_msg_req),
       is_binary(Bin) ->
    Args =  [Bin, Callback#callback.state],
    NextBlockNo = Callback#callback.block_no + 1,
    case catch apply(Callback#callback.module, Fun, Args) of
        {more, NewState} ->
            Count   = Callback#callback.count + size(Bin),
            Callback2 = Callback#callback{state    = NewState, 
                                          block_no = NextBlockNo,
                                          count    = Count},
	    Prepared = #prepared{status    = more,
				 block_no  = NextBlockNo},	    
            verify_count(Config, Callback2, Req, Prepared);
        {last, Result} ->
	    Prepared = #prepared{status    = last,
				 result    = Result,
				 block_no  = NextBlockNo},	    
            {undefined, Prepared};
	{error, {Code, Text}} ->
	    Error = #tftp_msg_error{code = Code, text = Text},
	    Prepared = #prepared{status    = error,
				 result    = Error},
            {undefined, Prepared};	
        Illegal ->
            Code = undef,
            Text = "Internal error. File handler error.",
            callback({abort, {Code, Text, Illegal}}, Config, Callback, Req)
    end;
do_callback({open, Type}, Config, Callback, Req)
  when is_record(Config, config),
       is_record(Callback, callback),
       is_record(Req, tftp_msg_req) ->
    {Access, Filename} = local_file_access(Req),
    {Fun, BlockNo} =
        case Type of
            client_prepare -> {prepare, undefined};
            client_open    -> {open, 0};
            server_open    -> {open, 0}
        end,
    Mod = Callback#callback.module,
    Args = [Access,
            Filename,
            Req#tftp_msg_req.mode,
            Req#tftp_msg_req.options,
            Callback#callback.state],
    PeerInfo = peer_info(Config),
    code:ensure_loaded(Mod),
    Args2 =
        case erlang:function_exported(Mod, Fun, length(Args)) of
            true  -> Args;
            false -> [PeerInfo | Args]
        end,
    case catch apply(Mod, Fun, Args2) of
        {ok, AcceptedOptions, NewState} ->
            Callback2 = Callback#callback{state    = NewState, 
                                          block_no = BlockNo, 
                                          count    = 0}, 
            {Callback2, {ok, AcceptedOptions}};
        {error, {Code, Text}} ->
            {undefined, #tftp_msg_error{code = Code, text = Text}};
        Illegal ->
            Code = undef,
            Text = "Internal error. File handler error.",
            callback({abort, {Code, Text, Illegal}}, Config, Callback, Req)
    end;
do_callback({abort, {Code, Text}}, Config, Callback, Req) ->
    Error = #tftp_msg_error{code = Code, text = Text},
    do_callback({abort, Error}, Config, Callback, Req);
do_callback({abort, {Code, Text, Details}}, Config, Callback, Req) ->
    Error = #tftp_msg_error{code = Code, text = Text, details = Details},
    do_callback({abort, Error}, Config, Callback, Req);
do_callback({abort = Fun, #tftp_msg_error{code = Code, text = Text} = Error}, Config, Callback, Req)
  when is_record(Config, config),
       is_record(Callback, callback), 
       is_record(Req, tftp_msg_req) ->
    Args =  [Code, Text, Callback#callback.state],
    catch apply(Callback#callback.module, Fun, Args),
    {undefined, Error};
do_callback({abort, Error}, _Config, undefined, _Req) when is_record(Error, tftp_msg_error) ->
    {undefined, Error}.

peer_info(#config{udp_host = Host, udp_port = Port}) ->
    if
        is_tuple(Host), size(Host) =:= 4 ->
            {inet, tftp_lib:host_to_string(Host), Port};
        is_tuple(Host), size(Host) =:= 8 ->
            {inet6, tftp_lib:host_to_string(Host), Port};
        true ->
            {undefined, Host, Port}
    end.

match_callback(Filename, Callbacks) ->
    if
        Filename =:= binary ->
            {ok, #callback{regexp   = "", 
                           internal = "", 
                           module   = tftp_binary,
                           state    = []}};
        is_binary(Filename) ->
            {ok, #callback{regexp   = "", 
                           internal = "", 
                           module   = tftp_binary, 
                           state    = []}};  
        Callbacks =:= []  ->
            {ok, #callback{regexp   = "", 
                           internal = "",
                           module   = tftp_file, 
                           state    = []}};
        true ->
            do_match_callback(Filename, Callbacks)
    end.

do_match_callback(Filename, [C | Tail]) when is_record(C, callback) ->
    case catch regexp:match(Filename, C#callback.internal) of
        {match, _, _} ->
            {ok, C};
        nomatch ->
            do_match_callback(Filename, Tail);
        Details ->
            Code = baduser,
            Text = "Internal error. File handler not found",
            {error, #tftp_msg_error{code = Code, text = Text, details = Details}}
    end;
do_match_callback(Filename, []) ->
    Code = baduser,
    Text = "Internal error. File handler not found",
    {error, #tftp_msg_error{code = Code, text = Text, details = Filename}}.

verify_count(Config, Callback, Req, Result) ->
    case Config#config.max_tsize of
        infinity ->
            {Callback, Result};
        Max when Callback#callback.count =< Max ->
            {Callback, Result};
        _Max ->
            Code = enospc,
            Text = "Too large file.",
            callback({abort, {Code, Text}}, Config, Callback, Req)
    end.

%%-------------------------------------------------------------------
%% Miscellaneous
%%-------------------------------------------------------------------

internal_info(Config, Type) ->
    {ok, ActualPort} = inet:port(Config#config.udp_socket),
    [
     {type, Type},
     {host, tftp_lib:host_to_string(Config#config.udp_host)},
     {port, Config#config.udp_port},
     {local_port, ActualPort},
     {port_policy, Config#config.port_policy},
     {udp, Config#config.udp_options},
     {use_tsize, Config#config.use_tsize},
     {max_tsize, Config#config.max_tsize},
     {max_conn, Config#config.max_conn},
     {rejected, Config#config.rejected},
     {timeout, Config#config.timeout},
     {polite_ack, Config#config.polite_ack},
     {debug, Config#config.debug_level},
     {parent_pid, Config#config.parent_pid}
    ] ++ Config#config.user_options ++ Config#config.callbacks.

local_file_access(#tftp_msg_req{access = Access, 
                                local_filename = Local, 
                                filename = Filename}) ->
    case Local =:= undefined of
        true ->
            %% Server side
            {Access, Filename};
        false ->
            %% Client side
            case Access of
                read ->
                    {write, Local};
                write ->
                    {read, Local}
            end
    end.

pre_verify_options(Config, Req) ->
    Options = Req#tftp_msg_req.options,
    case catch verify_reject(Config, Req, Options) of
        ok ->
            case verify_integer("tsize", 0, Config#config.max_tsize, Options) of
                true ->
                    case verify_integer("blksize", 0, 65464, Options) of
                        true ->
                            ok;
                        false ->
                            {error, {badopt, "Too large blksize"}}
                    end;
                false ->
                    {error, {badopt, "Too large tsize"}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
    
post_verify_options(Config, Req, NewOptions, Text) ->
    OldOptions = Req#tftp_msg_req.options,
    BadOptions  = 
        [Key || {Key, _Val} <- NewOptions, 
                not lists:keymember(Key, 1, OldOptions)],
    case BadOptions =:= [] of
        true ->
            {ok,
             Config#config{timeout = lookup_timeout(NewOptions)},
             Req#tftp_msg_req{options = NewOptions}};
        false ->
            {error, {badopt, Text}}
    end.

verify_reject(Config, Req, Options) ->
    Access = Req#tftp_msg_req.access,
    Rejected = Config#config.rejected,
    case lists:member(Access, Rejected) of
        true ->
            {error, {eacces, atom_to_list(Access) ++ " mode not allowed"}};
        false ->
            [throw({error, {badopt, Key ++ " not allowed"}}) ||
                {Key, _} <- Options, lists:member(Key, Rejected)],
            ok
    end.

lookup_timeout(Options) ->
    case lists:keysearch("timeout", 1, Options) of
        {value, {_, Val}} ->
            list_to_integer(Val);
        false ->
            3
    end.

lookup_mode(Options) ->
    case lists:keysearch("mode", 1, Options) of
        {value, {_, Val}} ->
            Val;
        false ->
            "octet"
    end.

verify_integer(Key, Min, Max, Options) ->
    case lists:keysearch(Key, 1, Options) of
        {value, {_, Val}} when is_list(Val) ->
            case catch list_to_integer(Val) of
                {'EXIT', _} ->
                    false;
                Int when Int >= Min, is_integer(Min),
                         Max =:= infinity ->
                    true;
                Int when Int >= Min, is_integer(Min),
                         Int =< Max, is_integer(Max) ->
                    true;
                _ ->
                    false
            end;
        false ->
            true
    end.

warning(#config{debug_level = Level}, F, A) ->
    case Level of
	none ->
	    ok;
	error ->
	    ok;
	_ ->
	    ok = error_logger:warning_msg("~p(~p): " ++ F ++ "~n", [?MODULE, self() | A])
    end.

error(F, A) ->
    ok = error_logger:error_msg("~p(~p): " ++ F ++ "~n", [?MODULE, self() | A]).

print_debug_info(#config{debug_level = Level} = Config, Who, What, Data) ->
    if
        Level =:= none ->
            ok;
        Level =:= warning ->
            ok;
        is_record(Data, error) ->
            do_print_debug_info(Config, Who, What, Data);
        Level =:= error ->
            ok; 
        Level =:= all ->
            do_print_debug_info(Config, Who, What, Data);
        What =:= open ->
            do_print_debug_info(Config, Who, What, Data);
        What =:= close ->
            do_print_debug_info(Config, Who, What, Data);
        Level =:= brief ->
            ok; 
        What =/= recv, What =/= send ->
            ok;
        is_record(Data, tftp_msg_data), Level =:= normal ->
            ok;  
        is_record(Data, tftp_msg_ack), Level =:= normal ->
            ok;
        true ->
            do_print_debug_info(Config, Who, What, Data)
    end.

do_print_debug_info(Config, Who, What, #tftp_msg_data{data = Bin} = Msg) when is_binary(Bin) ->
    Msg2 = Msg#tftp_msg_data{data = {bytes, size(Bin)}},
    do_print_debug_info(Config, Who, What, Msg2);
do_print_debug_info(Config, Who, What, #tftp_msg_req{local_filename = Filename} = Msg) when is_binary(Filename) ->
    Msg2 = Msg#tftp_msg_req{local_filename = binary},
    do_print_debug_info(Config, Who, What, Msg2);
do_print_debug_info(Config, Who, What, Data) ->
    Local = 
        case catch inet:port(Config#config.udp_socket) of
            {'EXIT', _Reason} ->
                0;
            {ok, Port} ->
                Port
        end,
    %% Remote = Config#config.udp_port,
    PeerInfo = peer_info(Config),
    Side = 
        if
            is_record(Who, tftp_msg_req),
            Who#tftp_msg_req.local_filename =/= undefined ->
                client;
            is_record(Who, tftp_msg_req),
            Who#tftp_msg_req.local_filename =:= undefined ->
                server;
            is_atom(Who) ->
                Who
        end,
    case {What, Data} of
        {_, #error{what = What, code = Code, text = Text}} -> 
            io:format("~p(~p): ~p ~p -> ~p: ~s\n", [Side, Local, self(), What, Code, Text]);
        {open, #tftp_msg_req{filename = Filename}} ->
            io:format("~p(~p): open  ~p ->\n\t~p ~p\n", [Side, Local, PeerInfo, self(), Filename]);
        {close, #tftp_msg_req{filename = Filename}} ->
            io:format("~p(~p): close ~p ->\n\t~p ~p\n", [Side, Local, PeerInfo, self(), Filename]);
        {recv, _} ->
            io:format("~p(~p): recv  ~p <-\n\t~p\n", [Side, Local, PeerInfo, Data]);
        {send, _} ->
            io:format("~p(~p): send  ~p ->\n\t~p\n", [Side, Local, PeerInfo, Data]);
        {match, _} when is_record(Data, callback) ->
            Mod = Data#callback.module,
            State = Data#callback.state,
            io:format("~p(~p): match ~p ~p =>\n\t~p\n", [Side, Local, PeerInfo, Mod, State]);
        {call, _} ->
            case Data of
                {Callback, _Result} when is_record(Callback, callback) ->
                    Mod   = Callback#callback.module,
                    State = Callback#callback.state,
                    io:format("~p(~p): call ~p ~p =>\n\t~p\n", [Side, Local, PeerInfo, Mod, State]);
                {undefined, Result}  ->
                    io:format("~p(~p): call ~p result =>\n\t~p\n", [Side, Local, PeerInfo, Result])
            end
    end.


%%-------------------------------------------------------------------
%% System upgrade
%%-------------------------------------------------------------------

system_continue(_Parent, _Debug, {Fun, Args}) ->
    apply(?MODULE, Fun, Args).

system_terminate(Reason, _Parent, _Debug, {_Fun, _Args}) ->
    exit(Reason).

system_code_change({Fun, Args}, _Module, _OldVsn, _Extra) ->
    {ok, {Fun, Args}}.
