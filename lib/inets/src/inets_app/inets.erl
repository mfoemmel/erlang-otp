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
%% Purpose: The main interface module of the inets application
%%----------------------------------------------------------------------

-module(inets).

%% API
-export([start/0, start/1, start/2, start/3,  
	 stop/0, stop/2, 
	 services/0, services_info/0,
	 service_names/0]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start([, Type]) -> ok
%%
%%  Type =  permanent | transient | temporary
%%
%% Description: Starts the inets application. Default type
%% is temporary. see application(3)
%%--------------------------------------------------------------------
start() -> 
    application:start(inets).

start(Type) -> 
    application:start(inets, Type).

%%--------------------------------------------------------------------
%% Function: start(Service, ServiceConfig [, How]) -> {ok, Pid} | 
%%                                                {error, Reason}
%%
%% Service = - ftpc | tftpd | httpc | httpd
%% ServiceConfig = ConfPropList | ConfFile
%% ConfPropList = [{Property, Value}] according to service 
%% ConfFile = Path - when service is httpd
%% How = inets | stand_alone
%%
%% Description: Dynamically starts an inets service after the inets
%% application has been started. 
%%
%% Note: Dynamically started services will not be handled by
%% application takeover and failover behavior when inets is run as a
%% distributed application. Nor will they be automaticly restarted
%% when the inets application is restarted, but as long as the inets
%% application is up and running they will be supervised and may be
%% soft code upgraded. Services started with the option stand alone,
%% e.i. the service is not started as part of the inets application,
%% will lose all OTP application benefits such as soft upgrade. The
%% stand alone service will be linked to the process that started it.
%% In most cases some of the supervison functionallity will still be
%% in place and in some sense the calling process has now become the
%% top supervisor.
%% --------------------------------------------------------------------
start(Service, ServiceConfig) ->
    Module = service_module(Service),
    start_service(Module, ServiceConfig, inets).

start(Service, ServiceConfig, How) ->
    Module = service_module(Service),
    start_service(Module, ServiceConfig, How).

%%--------------------------------------------------------------------
%% Function: stop() -> ok
%%
%% Description: Stops the inets application.
%%--------------------------------------------------------------------
stop() -> 
    application:stop(inets).

%%--------------------------------------------------------------------
%% Function: stop(Service, Pid) -> ok
%%
%% Service - ftp | tftpd | http | httpd | stand_alone
%%
%% Description: Stops a started service of the inets application or takes
%% down a stand alone "service" gracefully.
%%--------------------------------------------------------------------
stop(stand_alone, Pid) ->
    true = exit(Pid, shutdown),
    ok;

stop(Service, Pid) ->
    Module = service_module(Service),
    call_service(Module, stop_service, Pid).

%%--------------------------------------------------------------------
%% Function: services() -> [{Service, Pid}]
%%
%% Description: Returns a list of currently running services. 
%% Note: Services started with the stand alone option will not be listed
%%--------------------------------------------------------------------
services() ->
    Modules = [service_module(Service) || Service <- 
					      service_names()],
    try lists:flatten(lists:map(fun(Module) ->
					Module:services()
				end, Modules)) of
	Result ->
	    Result
    catch 
	exit:{noproc, _} ->
            {error, inets_not_started}
    end.
					       
%%--------------------------------------------------------------------
%% Function: services_info() -> [{Service, Pid, Info}]
%%
%% Description: Returns a list of currently running services where
%% each service is described by a [{Property, Value}] list. 
%%--------------------------------------------------------------------
services_info() ->
    case services() of
	{error, inets_not_started} ->
	    {error, inets_not_started};
	Services ->
	    Fun =  fun({Service, Pid}) -> 
			   Module = service_module(Service),
			   Info =  
			       case Module:service_info(Pid) of
				   {ok, PropList} ->
				       PropList;
				   {error, Reason} ->
				       Reason
			       end,
			   {Service, Pid, Info}
		   end,
	    lists:flatten(lists:map(Fun, Services))
    end.
%%--------------------------------------------------------------------
%% Function: service_names() -> [ServiceName]
%%  
%% ServiceName = atom()
%%
%% Description: Returns a list of supported services
%%-------------------------------------------------------------------
service_names() ->
    [ftpc, tftpd, httpc, httpd].

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
start_service(Service, Args, stand_alone) -> 
    Service:start_standalone(Args);
start_service(Service, Args, inets) ->
    call_service(Service, start_service, Args).

call_service(Service, Call, Args) ->
    try Service:Call(Args) of
	Result ->
	    Result
    catch
        exit:{noproc, _} ->
            {error, inets_not_started}
    end.
	
service_module(tftpd) ->
    tftp;
service_module(httpc) ->
    http;
service_module(ftpc) ->
    ftp;
service_module(Service) ->
    Service.






