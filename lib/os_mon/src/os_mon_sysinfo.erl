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
-module(os_mon_sysinfo).

%%% Keeps the contact and communication with the win32sysinfo port program

-export([start_link/0, get_disk_info/0, get_disk_info/1, get_mem_info/0]).

-export([init/1, handle_call/3, handle_info/2, terminate/2]).

-define(DISK_INFO,$d).
-define(MEM_INFO,$m).
-define(OK,$o).
-record(state,{port}).
start_link() -> gen_server:start_link({local, os_mon_sysinfo}, os_mon_sysinfo, [], []).

get_disk_info() -> gen_server:call(os_mon_sysinfo,get_disk_info).

get_disk_info(DriveRoot) -> gen_server:call(os_mon_sysinfo,{get_disk_info,DriveRoot}).

get_mem_info() -> gen_server:call(os_mon_sysinfo, get_mem_info).

init([]) ->
    Port = open_port({spawn,
		      filename:join(code:priv_dir(os_mon),"bin/win32sysinfo.exe")},
		     [{packet,1}]),
    case wait_ack(Port) of
	ok ->
	    {ok,#state{port=Port}};
	Error ->
	    {stop, Error}
    end.

%%-----------------------------------------------------------------
%% Callback functions from gen_server
%%-----------------------------------------------------------------
handle_call(get_disk_info, _From, State) ->
    {reply, get_disk_info1(State#state.port), State};
handle_call({get_disk_info,RootList}, _From, State) ->
    {reply, get_disk_info1(State#state.port,RootList), State};
handle_call(get_mem_info, _From, State) ->
    {reply, get_mem_info1(State#state.port), State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.



get_disk_info1(Port) ->
    Port ! {self(),{command,[?DISK_INFO]}},
    get_data(Port,[]).

get_disk_info1(Port,PathList) ->
    Port ! {self(),{command,[?DISK_INFO|[P++[0]||P <- PathList]]}},
    get_data(Port,[]).

get_mem_info1(Port) ->
    Port ! {self(),{command,[?MEM_INFO]}},
    get_data(Port,[]).

get_data(Port, Sofar) ->
    receive
	{Port, {data, [?OK]}} ->
	    lists:reverse(Sofar);
	{Port, {data, Bytes}} ->
	    get_data(Port, [Bytes|Sofar])
    after 5000 ->
	    lists:reverse(Sofar)
    end.

wait_ack(Port) ->
    receive
	{Port, {data, [?OK]}} ->
	    ok;
	Reason ->
	    {error, Reason}
    after 5000 ->
	    {error, port_timeout}
    end.

