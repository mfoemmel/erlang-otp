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
%% Purpose: The top supervisor for the http server (httpd) hangs under 
%%          inets_sup.
%%----------------------------------------------------------------------

-module(httpd_sup).

-behaviour(supervisor).

%% Internal application API
-export([start_link/1, start_link/2]).
-export([start_child/1, restart_child/2, stop_child/2]).

%% Supervisor callback
-export([init/1]).

-export([listen_init/4]).

-define(TIMEOUT, 15000).

%%%=========================================================================
%%%  API
%%%=========================================================================
start_link(HttpdServices) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [HttpdServices]).

start_link(HttpdServices, stand_alone) ->
    supervisor:start_link(?MODULE, [HttpdServices]).

start_child(Config) ->
    Spec =
	try httpd_config(Config) of
	    NewConfig ->
		httpd_child_spec(NewConfig, ?TIMEOUT, [])
	catch
	    %% When httpd is started at another time than
	    %% the inets application start it is allowed
	    %% to start from a property list.
	    {error, mandatory_config_file_missing} ->
		httpd_child_spec(Config, ?TIMEOUT, [])
	end,    	
    
    case supervisor:start_child(?MODULE, Spec) of
	{error,{invalid_child_spec, Error}} ->
	    Error;
	Other ->
	    Other
    end.

restart_child(Address, Port) ->
    Name = id(Address, Port),
    case supervisor:terminate_child(?MODULE, Name) of
        ok ->
            supervisor:restart_child(?MODULE, Name);
        Error ->
            Error
    end.
    
stop_child(Address, Port) ->
    Name = id(Address, Port),
    case supervisor:terminate_child(?MODULE, Name) of
        ok ->
            supervisor:delete_child(?MODULE, Name);
        Error ->
            Error
    end.
    
id(Address, Port) ->
    {httpd_instance_sup, Address, Port}.

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([HttpdServices]) ->
    RestartStrategy = one_for_one,
    MaxR = 10,
    MaxT = 3600,
    Children = child_specs(HttpdServices, []),
    {ok, {{RestartStrategy, MaxR, MaxT}, Children}}.

%%%=========================================================================
%%%  Internal functions
%%%=========================================================================

%% The format of the httpd service is:
%% httpd_service() -> {httpd,httpd()}
%% httpd()         -> [httpd_config()] | file()
%% httpd_config()  -> {file,file()} |
%%                    {debug,debug()} |
%%                    {accept_timeout,integer()}
%% debug()         -> disable | [debug_options()]
%% debug_options() -> {all_functions,modules()} | 
%%                    {exported_functions,modules()} |
%%                    {disable,modules()}
%% modules()       -> [atom()]


child_specs([], Acc) ->
    Acc;
child_specs([{httpd, HttpdService} | Rest], Acc) ->
    NewHttpdService = mk_tuple_list(HttpdService),
    case catch child_spec(NewHttpdService) of
	{error, Reason} ->
	    error_msg("Failed to start service: ~n~p ~n due to: ~p~n",
		      [HttpdService, Reason]),
	    child_specs(Rest, Acc);
	Spec ->
	    child_specs(Rest, [Spec | Acc])
    end.

child_spec(HttpdService) ->
    Debug = proplists:get_value(debug, HttpdService, []),
    AcceptTimeout = proplists:get_value(accept_timeout, HttpdService, 15000),
    Config = httpd_config(HttpdService),
    httpd_util:valid_options(Debug, AcceptTimeout, Config),
    httpd_child_spec(Config, AcceptTimeout, Debug).

httpd_config([Value| _] = HttpdService) when is_tuple(Value) ->
    case proplists:get_value(file, HttpdService) of
	undefined -> 
	    case proplists:get_value(proplist_file, HttpdService) of
		undefined ->
		    case mandatory_properties(HttpdService) of
			ok -> % stand alone start
			    HttpdService;
			_ ->
			    throw({error, 
				   mandatory_config_file_missing})
		    end;
		File ->
		   try file:consult(File) of
		       {ok, [PropList]} ->
			   PropList
		   catch 
		       exit:_ ->
			   throw({error, 
				  {could_not_consult_proplist_file, File}})  
		   end
	    end;
	File -> 
	    File
    end.

httpd_child_spec([Value| _] = Config, AcceptTimeout, Debug)  
  when is_tuple(Value)  ->
    case mandatory_properties(Config) of
	ok ->
	    case address(Config) of
		{ok, Address, NewConfig} ->  
		     Port =  
			proplists:get_value(port, NewConfig, 80),
		    httpd_child_spec(NewConfig, AcceptTimeout, 
				     Debug, Address, Port);
		 Error ->
		    Error
	    end;
	Error ->
	    Error
    end;

httpd_child_spec(ConfigFile, AcceptTimeout, Debug) ->
    case httpd_conf:load(ConfigFile) of
	{ok, ConfigList} ->
	    case mandatory_properties(ConfigList) of
		ok ->
		    Port = proplists:get_value(port, ConfigList, 80),
		    Address = proplists:get_value(bind_address, ConfigList,
						  any), 
		    httpd_child_spec([{file, ConfigFile} | ConfigList], 
				     AcceptTimeout, 
				     Debug, Address, Port);
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

httpd_child_spec(Config, AcceptTimeout, Debug, Addr, 0) ->
	case start_listen(Addr, 0, Config) of
	    {Pid, {NewPort, NewConfig, ListenSocket}} ->
		Name = {httpd_instance_sup, Addr, NewPort},
		StartFunc = {httpd_instance_sup, start_link,
			     [NewConfig, AcceptTimeout, 
			      {Pid, ListenSocket}, Debug]},
		Restart = permanent, 
		Shutdown = infinity,
		Modules = [httpd_instance_sup],
		Type = supervisor,
		{Name, StartFunc, Restart, Shutdown, Type, Modules};
	    {Pid, {error, Reason}}  ->
		exit(Pid, normal),
		{error, Reason}
	end;
		    
httpd_child_spec(Config, AcceptTimeout, Debug, Addr, Port) ->
    Name = {httpd_instance_sup, Addr, Port},
    StartFunc = {httpd_instance_sup, start_link,
		 [Config, AcceptTimeout, Debug]},
    Restart = permanent, 
    Shutdown = infinity,
    Modules = [httpd_instance_sup],
    Type = supervisor,
    {Name, StartFunc, Restart, Shutdown, Type, Modules}.


mk_tuple_list([]) ->
    [];
mk_tuple_list([H={_,_}|T]) ->
    [H|mk_tuple_list(T)];
mk_tuple_list(F) ->
    [{file,F}].

error_msg(F, A) ->
    error_logger:error_msg(F ++ "~n", A).

listen(Address, Port, Config)  ->
    SocketType = proplists:get_value(socket_type, Config, ip_comm), 
    case http_transport:start(SocketType) of
	ok ->
	    case http_transport:listen(SocketType, Address, Port) of
		{ok, ListenSocket} ->
		    NewConfig = proplists:delete(port, Config),
		    {ok, NewPort} = inet:port(ListenSocket),
		    {NewPort, [{port, NewPort} | NewConfig], ListenSocket};
		{error, Reason} ->
		    {error, {listen, Reason}}
	    end;
	{error, Reason} ->
	    {error, {socket_start_failed, Reason}}
    end.

address(Config) ->
    case proplists:get_value(bind_address, Config, any) of
	any ->
	    {ok, any, Config};
	Host ->
	    case httpd_util:ip_address(Host) of
		{ok, Address} ->
		    NewConfig = proplists:delete(bind_address, Config),
		    {ok, Address, [{bind_address, Address} | NewConfig]};
		{error, Reason} ->
		    {error, Reason}
	    end
    end.

start_listen(Address, Port, Config) ->
    Pid = listen_owner(Address, Port, Config),
    receive
	{Pid, Result} ->
	    {Pid, Result}
    end.

listen_owner(Address, Port, Config) ->
    spawn(?MODULE, listen_init, [self(), Address, Port, Config]).

listen_init(From, Address, Port, Config) ->			 
    process_flag(trap_exit, true),
    Result = listen(Address, Port, Config), 
    From ! {self(), Result},
    listen_loop().

listen_loop() ->
    receive
	{'EXIT', _, _} ->
	    ok
    end.
	    
mandatory_properties(ConfigList) ->
    a_must(ConfigList, [server_name,port,server_root,document_root]).

a_must(_ConfigList, []) ->
    ok;
a_must(ConfigList, [Prop | Rest]) ->
    case proplists:get_value(Prop, ConfigList) of
	undefined ->
	    {error, {missing_property, Prop}};
	_ ->
	    a_must(ConfigList, Rest)
    end.





