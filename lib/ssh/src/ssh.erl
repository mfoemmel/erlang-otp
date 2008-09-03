%%<copyright>
%% <year>2004-2007</year>
%% <holder>Ericsson AB, All Rights Reserved</holder>
%%</copyright>
%%<legalnotice>
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson AB.
%%</legalnotice>
%%

-module(ssh).

-include("ssh.hrl").
-include("ssh_connect.hrl").

-export([start/0, start/1, stop/0, attach/2, attach/3,
	 connect/3, close/1, daemon/1, daemon/2, daemon/3,
	 stop_listener/1, stop_listener/2, stop_daemon/1, stop_daemon/2,
	 shell/1, shell/2, shell/3]).

%%--------------------------------------------------------------------
%% Function: start([, Type]) -> ok
%%
%%  Type =  permanent | transient | temporary
%%
%% Description: Starts the inets application. Default type
%% is temporary. see application(3)
%%--------------------------------------------------------------------
start() ->
    application:start(ssh).

start(Type) ->
    application:start(ssh, Type).

%%--------------------------------------------------------------------
%% Function: stop() -> ok
%%
%% Description: Stops the inets application.
%%--------------------------------------------------------------------
stop() ->
    application:stop(ssh).

%%--------------------------------------------------------------------
%% Function: connect(Host, Port, Options) -> ConnectionRef
%% 
%%	Host - string()
%%	Port - integer()
%%	Options - [{Option, Value}]
%%
%% Description: Starts an ssh connection.
%%--------------------------------------------------------------------
connect(Host, Port, Options) ->
    {SocketOpts, Opts} = handle_options(Options),
    Timeout = proplists:get_value(timeout, Opts, ?DEFAULT_TIMEOUT),
    Address = ip_address(Host),
    try sshc_sup:start_child([[{address, Address}, {port, Port}, 
			      {role, client},
			      {channel_pid, self()},
			      {socket_opts, SocketOpts}, 
			      {ssh_opts, [{host, Host}| Opts]}]]) of 
	{ok, ConnectionSup} ->
	    {ok, Manager} = 
		ssh_connection_sup:connection_manager(ConnectionSup),
	    receive 
		{Manager, is_connected} ->
		    {ok, Manager};
		%% When the connection fails 
		%% ssh_connection_sup:connection_manager
		%% might return undefined as the connection manager
		%% could allready have terminated, so we will not
		%% match the Manager in this case
		{_, not_connected, {error, Reason}} ->
		    {error, Reason};
		{_, not_connected, Other} ->
		    {error, Other}
	    after 
		Timeout -> 
		    exit(ConnectionSup, shutdown),
		    {error, timeout}
	    end
    catch 
	exit:{noproc, _} ->
 	    {error, ssh_not_started}
    end.

%%--------------------------------------------------------------------
%% Function: close(ConnectionRef) -> ok
%%
%% Description: Closes an ssh connection.
%%--------------------------------------------------------------------	
close(ConnectionRef) ->
    ssh_connection_manager:stop(ConnectionRef).

%%--------------------------------------------------------------------
%% Function: daemon(Port) ->
%%           daemon(Port, Opts) ->
%%           daemon(Address, Port, Options) -> SshSystemRef
%%
%% Description: Starts a server listening for SSH connections 
%% on the given port.
%%--------------------------------------------------------------------	
daemon(Port) ->
    daemon(Port, []).

daemon(Port, Opts) ->
    daemon(any, Port, Opts).

daemon(HostAddr, Port, Opts) ->
    Shell = proplists:get_value(shell, Opts, {shell, start, []}),
    Address = case HostAddr of
		  any ->
		      ip_address("localhost");
		  Str when is_list(Str) ->
		      ip_address(Str);
		  _ ->
		      HostAddr
	      end,
    start_daemon(Address, Port, [{role, server}, 
				{shell, Shell} | Opts]).

%%--------------------------------------------------------------------
%% Function: stop_listener(SysRef) -> ok
%%           stop_listener(Address, Port) -> ok
%%
%%
%% Description: Stops the listener, but leaves 
%% existing connections started by the listener up and running.
%%--------------------------------------------------------------------	
stop_listener(SysSup) ->
    ssh_system_sup:stop_listener(SysSup).
stop_listener(Address, Port) ->
    ssh_system_sup:stop_listener(Address, Port).

%%--------------------------------------------------------------------
%% Function: stop_daemon(SysRef) -> ok
%%%          stop_daemon(Address, Port) -> ok
%%
%%
%% Description: Stops the listener and all connections started by 
%% the listener.
%%--------------------------------------------------------------------	
stop_daemon(SysSup) ->
    ssh_system_sup:stop_system(SysSup).
stop_daemon(Address, Port) ->
    ssh_system_sup:stop_system(Address, Port).

%%--------------------------------------------------------------------
%% Function: shell(Host [,Port,Options]) -> 
%%
%%   Host = string()
%%   Port = integer()
%%   Options = [{Option, Value}]
%%
%% Description: Starts an interactive shell to an SSH server on the
%% given <Host>. The function waits for user input,
%% and will not return until the remote shell is ended.(e.g. on
%% exit from the shell)
%%--------------------------------------------------------------------
shell(Host) ->
    shell(Host, ?SSH_DEFAULT_PORT, []).
shell(Host, Options) ->
    shell(Host, ?SSH_DEFAULT_PORT, Options).
shell(Host, Port, Options) ->
    ssh_ssh:connect(Host, Port, Options).

%% TODO: Should this be a supported API function, used by
%% sftp and ssh_ssh. Does it acctually work? Should be better tested
%% before made public!
attach(ConnectionRef, Timeout) ->
    ssh_connection_manager:attach(ConnectionRef, Timeout).

attach(ConnectionRef, ChannelPid, Timeout) ->
    ssh_connection_manager:attach(ConnectionRef, ChannelPid, Timeout).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
start_daemon(Address, Port, Options) ->
    {SocketOpts, Opts} = handle_options([{ip, Address} | Options]),
    case ssh_system_sup:system_supervisor(Address, Port) of
	undefined ->
	    try sshd_sup:start_child([{address, Address}, 
				      {port, Port}, {role, server},
				      {socket_opts, SocketOpts}, 
				      {ssh_opts, Opts}]) of
		{ok, SysSup} ->
		    {ok, SysSup};
		{error, {already_started, _}} ->
		    {error, eaddrinuse}
	    catch
		exit:{noproc, _} ->
		    {error, ssh_not_started}
	    end;
	Sup  ->
	    case ssh_system_sup:restart_acceptor(Address, Port) of
		ok ->
		    {ok, Sup};
		_  ->
		    {error, eaddrinuse}
	    end
    end.

handle_options(Opts) ->
    handle_options(proplists:unfold(Opts), [], []).
handle_options([], SockOpts, Opts) ->
    {SockOpts, Opts};
%% TODO: Could do some type checks here on plain ssh-opts
handle_options([{system_dir, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{user_dir, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{user_dir_fun, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{silently_accept_hosts, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{user_interaction, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{public_key_alg, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{connect_timeout, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{user, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{password, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{user_passwords, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{pwdfun, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{user_auth, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{key_cb, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{role, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{channel, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{compression, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{allow_user_interaction, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{passive_subsys, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{infofun, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{connectfun, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{disconnectfun , _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{failfun, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, SockOpts, [Opt | Opts]);
handle_options([{ip, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, [Opt |SockOpts], Opts);
handle_options([{ifaddr, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, [Opt |SockOpts], Opts);
handle_options([{fd, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, [Opt | SockOpts], Opts);
handle_options([{nodelay, _} = Opt | Rest], SockOpts, Opts) -> 
    handle_options(Rest, [Opt | SockOpts], Opts);
handle_options([Opt | Rest], SockOpts, Opts) ->
    handle_options(Rest, SockOpts, [Opt | Opts]).

ip_address(Host) ->
    {ok, Ip} =  case (catch inet:getaddr(Host,inet6)) of
		    {ok, {0, 0, 0, 0, 0, 16#ffff, _, _}} ->
			inet:getaddr(Host, inet);
		    {ok, IPAddr} ->
			{ok, IPAddr};
		    _ ->
			inet:getaddr(Host, inet) 
		end,
    Ip.
