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
-module(os_sup).

%% Purpose : Supervising the system log.


%% External exports
-export([start/0,start_link/0,stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {port, tag}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->  gen_server:start({local, os_sup_server}, os_sup, [], []).

start_link() ->  gen_server:start_link({local, os_sup_server}, os_sup, [], []).

stop() -> gen_server:call(os_sup_server, stop).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

init([]) ->
    process_flag(trap_exit, true),
    case os:cmd(cmd_str(enable)) of
	"0" ->
	    Port = open_port({spawn, cmd_str(port)}, [{packet, 2}]),
	    Tag = get_env(os_sup_errortag),
	    {ok, #state{port = Port, tag = Tag}};
	Error ->
	    {stop, {mod_syslog, Error}}
    end.

handle_call(stop, From, State) ->
    Port = State#state.port,
    Reason = stop_port(Port),
    {stop, Reason, ok, State#state{port = noport}}.

handle_cast(Msg, State) ->
    {noreply, State}.

handle_info({Port, {data, Msg}}, State) when Port == State#state.port ->
    error_logger:error_report(State#state.tag, Msg),
    {noreply, State};
handle_info({'EXIT', Port, Why}, State) when Port == State#state.port ->
    error_logger:error_msg("os_sup:server_body:port died"),	
    {stop, {port_died, Why}, State#state{port = noport}};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, State) when port(State#state.port) ->
    stop_port(State#state.port),
    ok;
terminate(_Reason, State) -> % The port is already stopped !!
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

stop_port(Port) ->
    Port ! {self(), {command, "only_stdin"}},
    case os:cmd(cmd_str(disable)) of
	"0" ->
	    Port ! {self(), {command, "die"}},
	    receive
		{'EXIT', Port, _} ->
		    normal
	    after 1000 ->
		    error_logger:error_msg("os_sup:server_stop:port not dead"),
		    {error,port_not_stopped}
	    end;
	Error ->
	    error_logger:error_msg("os_sup:server_stop:mod_syslog: ~p", [Error]),
	    {error, Error}
    end.

cmd_str(port)->
    %% portpgm ownpath
    PrivDir = code:priv_dir(os_mon),
    OwnPath = get_env(os_sup_own),
    PrivDir ++ "/bin/ferrule " ++ OwnPath;
cmd_str(Mode)->
    %% modpgm modesw ownpath syslogconf
    PrivDir = code:priv_dir(os_mon),
    OwnPath = get_env(os_sup_own),
    SyslogConf = get_env(os_sup_syslogconf),
    ModeSw =
	case Mode of
	    enable ->
		" otp ";
	    disable ->
		" nootp "
	end,
    PrivDir ++ "/bin/mod_syslog" ++ ModeSw ++ OwnPath ++ " " ++ SyslogConf.

get_env(Atom) ->
    case application:get_env(os_mon, Atom) of
	{ok, Value} ->
	    Value;
	undefined ->
	    error_logger:error_msg("os_sup:get_env:unknown config.param."),
	    exit({unknown_config_param, {os_sup, get_env, [Atom]}})
    end.
