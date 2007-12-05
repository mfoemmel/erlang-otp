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
%%----------------------------------------------------------------------
%% Purpose: Utility functions for the (snmp manager) user test(s).
%%----------------------------------------------------------------------

-module(snmp_manager_user_test_lib).

-behaviour(snmpm_user).


%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include("test_server.hrl").
-include("snmp_test_lib.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([
         start_link/0, stop/1,
	 simulate_crash/2,
	 register/2, register_monitor/2, unregister/1, unregister/2
        ]).

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
-export([
         user/1
        ]).

-export([handle_error/3,
         handle_agent/4,
         handle_pdu/5,
         handle_trap/4,
         handle_inform/4,
         handle_report/4]).


-record(state, {parent, ids = []}).


%%----------------------------------------------------------------------
%% The user simulation API
%%----------------------------------------------------------------------

start_link() ->
    S = #state{parent = self()},
    proc_lib:start_link(?MODULE, user, [S]).

stop(Pid) ->
    cast(Pid, stop).

simulate_crash(Pid, Reason) ->
    call(Pid, {simulate_crash, Reason}).

register(Pid, Id) ->
    call(Pid, {register, Id}).

register_monitor(Pid, Id) ->
    call(Pid, {register_monitor, Id}).

unregister(Pid) ->
    call(Pid, unregister).

unregister(Pid, Id) ->
    call(Pid, {unregister, Id}).

user(#state{parent = Pid} = S) ->
    proc_lib:init_ack(Pid, {ok, self()}),
    user_loop(S).

user_loop(#state{parent = Parent} = S) ->
    receive
	{stop, Parent} ->
	    exit(normal);

	{{simulate_crash, Reason}, Parent, Ref} ->
	    reply(Parent, ok, Ref),
	    exit(Reason);
	
	{{register, Id}, Parent, Ref} ->
	    IDs = S#state.ids,
	    case lists:member(Id, IDs) of
		false ->
		    Res = snmpm:register_user(Id, ?MODULE, self()),
		    reply(Parent, Res, Ref),
		    user_loop(S#state{ids = [Id|IDs]});
		true ->
		    reply(Parent, {error, already_registered}, Ref),
		    user_loop(S)
	    end;
	
	{{register_monitor, Id}, Parent, Ref} ->
	    IDs = S#state.ids,
	    case lists:member(Id, IDs) of
		false ->
		    Res = snmpm:register_user_monitor(Id, ?MODULE, self()),
		    reply(Parent, Res, Ref),
		    user_loop(S#state{ids = [Id|IDs]});
		true ->
		    reply(Parent, {error, already_registered}, Ref),
		    user_loop(S)
	    end;
	
	{unregister, Parent, Ref} ->
	    Res = [snmpm:unregister_user(Id) || Id <- S#state.ids],
	    reply(Parent, {ok, Res}, Ref),
	    user_loop(S);
	
	{{unregister, Id}, Parent, Ref} ->
	    IDs = S#state.ids,
	    IDs2 = 
		case lists:member(Id, IDs) of
		    true ->
			Res = snmpm:unregister_user(Id),
			reply(Parent, Res, Ref),
			lists:delete(Id, IDs);
		    false ->
			reply(Parent, {error, not_registered}, Ref),
			IDs
		end,
	    user_loop(S#state{ids = IDs2});
	

	%% SNMP manager callback messages (from our callback API):

	{handle_error, Pid, ReqId, Reason} ->
	    do_handle_error(Pid, ReqId, Reason),
	    user_loop(S);

	{handle_agent, Pid, Addr, Port, SnmpInfo} ->
	    do_handle_agent(Pid, Addr, Port, SnmpInfo),
	    user_loop(S);

	{handle_pdu, Pid, Addr, Port, ReqId, SnmpResponse} ->
	    do_handle_pdu(Pid, Addr, Port, ReqId, SnmpResponse),
	    user_loop(S);

	{handle_trap, Pid, Addr, Port, SnmpTrap} ->
	    do_handle_trap(Pid, Addr, Port, SnmpTrap),
	    user_loop(S);

	{handle_inform, Pid, Addr, Port, SnmpInform} ->
	    do_handle_inform(Pid, Addr, Port, SnmpInform),
	    user_loop(S);

	{handle_report, Pid, Addr, Port, SnmpReport} ->
	    do_handle_report(Pid, Addr, Port, SnmpReport),
	    user_loop(S);

	Unknown ->
	    info("received unknown message: ~n~p", [Unknown]),
	    user_loop(S)
    end.
	    

%% -------------

do_handle_error(Pid, ReqId, Reason) ->
    info("received error callback:"
         "~n   ReqId:    ~p"
         "~n   Reason:   ~p", [ReqId, Reason]),
    Pid ! {ignore, self()},
    ok.


do_handle_agent(Pid, Addr, Port, SnmpInfo) ->
    info("received agent callback:"
         "~n   Addr:     ~p"
         "~n   Port:     ~p"
         "~n   SnmpInfo: ~p", [Addr, Port, SnmpInfo]),
    Pid ! {ignore, self()},
    ok.


do_handle_pdu(Pid, Addr, Port, ReqId, SnmpResponse) ->
    info("received pdu callback:"
         "~n   Addr:         ~p"
         "~n   Port:         ~p"
         "~n   ReqId:        ~p"
         "~n   SnmpResponse: ~p", [Addr, Port, ReqId, SnmpResponse]),
    Pid ! {ignore, self()},
    ok.


do_handle_trap(Pid, Addr, Port, SnmpTrap) ->
    info("received trap callback:"
         "~n   Addr:     ~p"
         "~n   Port:     ~p"
         "~n   SnmpTrap: ~p", [Addr, Port, SnmpTrap]),
    Pid ! {ignore, self()},
    ok.


do_handle_inform(Pid, Addr, Port, SnmpInform) ->
    info("received inform callback:"
         "~n   Addr:       ~p"
         "~n   Port:       ~p"
         "~n   SnmpInform: ~p", [Addr, Port, SnmpInform]),
    Pid ! {ignore, self()},
    ok.


do_handle_report(Pid, Addr, Port, SnmpReport) ->
    info("received report callback:"
         "~n   Addr:       ~p"
         "~n   Port:       ~p"
         "~n   SnmpReport: ~p", [Addr, Port, SnmpReport]),
    Pid ! {ignore, self()},
    ok.


info(F, A) ->
    error_logger:info_msg("USER SIMULATOR " ++ F ++ "~n", A).


%% -------------

call(UserPid, Req) ->
    call(UserPid, Req, 5000).

call(UserPid, Req, To) ->
    Ref = make_ref(),
    UserPid ! {Req, self(), Ref},
    receive
	{Reply, UserPid, Ref} ->
	    Reply
    after To ->
	    {error, timeout}
    end.

reply(Pid, Reply, Ref) ->    
    Pid ! {Reply, self(), Ref}.

cast(UserPid, Msg) ->
    UserPid ! {Msg, self()},
    ok.


%%----------------------------------------------------------------------
%% User callback functions:
%%----------------------------------------------------------------------

handle_error(ReqId, Reason, UserPid) ->
    UserPid ! {handle_error, self(), ReqId, Reason},
    ignore.
 
 
handle_agent(Addr, Port, SnmpInfo, UserPid) ->
    UserPid ! {handle_agent, self(), Addr, Port, SnmpInfo},
    ignore.
 
 
handle_pdu(Addr, Port, ReqId, SnmpResponse, UserPid) ->
    UserPid ! {handle_pdu, self(), Addr, Port, ReqId, SnmpResponse},
    ignore.
 
 
handle_trap(Addr, Port, SnmpTrap, UserPid) ->
    UserPid ! {handle_trap, self(), Addr, Port, SnmpTrap},
    ok.
 
 
handle_inform(Addr, Port, SnmpInform, UserPid) ->
    UserPid ! {handle_inform, self(), Addr, Port, SnmpInform},
    ok.


handle_report(Addr, Port, SnmpReport, UserPid) ->
    UserPid ! {handle_report, self(), Addr, Port, SnmpReport},
    ok.
