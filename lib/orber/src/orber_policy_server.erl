%%--------------------------------------------------------------------
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
%%--------------------------------------------------------------------
%% File: orber_policy_server.erl
%% Author: Niclas Eklund
%% 
%% Description:
%%    This file contains the CORBA::POLICY interfaces.
%%
%% Creation date: 990517
%%-----------------------------------------------------------------
-module(orber_policy_server).

-behaviour(gen_server).
 
%%--------------- INCLUDES -----------------------------------
-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_debug.hrl").
%%--------------- IMPORTS ------------------------------------
%%--------------- EXPORTS ------------------------------------
%% External
-export([start/1, stop/0, install/2, create/3, get_policy/2]).
-export([copy/1, destroy/1, '_get_policy_type'/1, '_get_value'/1]).
-export([associate_policies/2, unassociate_policies/1]).

%% Internal
-export([extract_data/1, get_tc/0]).

%%--------------- gen_server specific exports ----------------
-export([handle_call/3, handle_cast/2, handle_info/2, code_change/3]).
-export([init/1, terminate/2]).

%%--------------- misc, e.g., debugging functions ------------

%%--------------- LOCAL DEFINITIONS --------------------------
%% Mnesia Table definition record:
-record(corba_policy, {key, policy_id, type, value}).
-record(corba_policy_associations, {objref, policies}).

%% Macros
-define(dirty_query_context, true).

%% These macros returns a read fun suitable for evaluation in a transaction
-define(read_function(Objkey),
	fun() ->
		mnesia:dirty_read(Objkey)
	end).

-define(write_function(R),
	fun() ->
		mnesia:dirty_write(R)
	end).

-define(delete_function(R),
	fun() ->
		mnesia:delete(R)
	end).

-define(match_func(R),
	fun() ->
		mnesia:match_object({corba_policy_associations,R,'_'})
	end).

-ifdef(dirty_query_context).
-define(query_check(Q_res), Q_res).
-else.
-define(query_check(Q_res), {atomic, Q_res}).
-endif.


-define(CHECK_EXCEPTION(Res), case Res of
				  {'EXCEPTION', E} ->
				      corba:raise(E);
				   R ->
				      R
			      end).

%%------------------------------------------------------------
%% function : handle_cast, handle_info, code_change
%% Arguments: 
%% Returns  : 
%% Effect   : Functions demanded by the gen_server module. 
%%------------------------------------------------------------

code_change(OldVsn, State, Extra) ->
    {ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    {noreply, State}.

%%-----------------------------------------------------------
%% function : get_tc
%% Arguments: -
%% Returns  : TypeCode for Policy Object Reference.
%% Effect   : 
%% Exception: 
%%------------------------------------------------------------

get_tc() ->
    orber_tc:object_reference("IDL:omg.org/CORBA/Policy:1.0", "Policy").

%%-----------------------------------------------------------
%% function : init, terminate
%% Arguments: 
%% Returns  : 
%% Effect   : 
%% Exception: 
%%------------------------------------------------------------

init(Env) ->
    ?PRINTDEBUG("orber policy server"),
    case mnesia:wait_for_tables([corba_policy, corba_policy_associations], 
				infinity) of
	ok ->
	    process_flag(trap_exit, true),
	    {ok, []};
	StopReason ->
	    {stop, StopReason}
    end.

terminate(From, Reason) ->
    ok.

%%-----------------------------------------------------------
%% function : start, stop
%% Arguments: start(MnesiaOptions)
%% Returns  : 
%% Effect   : 
%% Exception: starts and stops the gen_server.
%%------------------------------------------------------------

start(Opts) ->
    gen_server:start_link({local, orber_policyserver}, ?MODULE, Opts, []).

stop() ->
    gen_server:call(orber_policyserver, stop, infinity).


%%-----------------------------------------------------------
%% function : extract_data
%% Arguments: PolicyRef
%% Returns  : 
%% Effect   : Used to extract Type and Value from a PolicyRef.
%%            Only usable if the value is simple, e.g., atom(V).
%% Exception: 
%%------------------------------------------------------------

extract_data(PolicyRef) ->
    Policy = iop_ior:get_privfield(PolicyRef),
    case binary_to_term(Policy) of
	{T, V} when atom(V), integer(T) ->
	    {T, V};
	_->
	    false
    end.

%%-----------------------------------------------------------
%% function : get_policy
%% Arguments: Obj - target object.
%%            PolicyType - integer
%% Returns  : PolicyRef - A Policy Refernce
%% Effect   : Returns Policy of given type associated with
%%            the target object.
%% Exception: 
%%------------------------------------------------------------

get_policy(Obj, PolicyType) when integer(PolicyType) ->
    ?CHECK_EXCEPTION(gen_server:call(orber_policyserver,
				     {get_policy, Obj, PolicyType},
				     infinity));
get_policy(_, _) ->
     corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).


%%-----------------------------------------------------------
%% function : associate_policies
%% Arguments: ObjectRef, PolicyList
%% Returns  : ok
%% Effect   : Associates given Policies with target object.
%%            Used by 'corba_object:get_policy(Obj)'
%% Exception: 
%%------------------------------------------------------------
associate_policies(Obj, PolicyList) when list(PolicyList) ->
    ?CHECK_EXCEPTION(gen_server:call(orber_policyserver,
				     {associate_policies, Obj, 
				      PolicyList},
				     infinity));
associate_policies(_, _)->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------
%% function : unassociate_policies
%% Arguments: ObjectRef, PolicyList
%% Returns  : ok
%% Effect   : Unassociates given Policies with target object.
%%            Used for cleaning up.
%% Exception: 
%%------------------------------------------------------------
unassociate_policies(Obj) ->
    ?CHECK_EXCEPTION(gen_server:call(orber_policyserver,
				     {unassociate_policies, Obj},
				     infinity));
unassociate_policies(_) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).
    

%%-----------------------------------------------------------
%% function : _get_value
%% Arguments: PolicyRef
%% Returns  : 
%% Effect   : 
%% Exception: 
%%------------------------------------------------------------

'_get_value'(PolicyRef) ->
    Policy = iop_ior:get_privfield(PolicyRef),
    case binary_to_term(Policy) of
	{_, V} when atom(V) ->
	    V;
	_->
	    ?CHECK_EXCEPTION(gen_server:call(orber_policyserver,
					     {PolicyRef, '_get_value', []}, 
					     infinity))
    end.

%%-----------------------------------------------------------
%% function : _get_policy_type
%% Arguments: PolicyRef
%% Returns  : 
%% Effect   : 
%% Exception: 
%%------------------------------------------------------------

'_get_policy_type'(PolicyRef) ->
    Policy = iop_ior:get_privfield(PolicyRef),
    case catch binary_to_term(Policy) of
	{T1,_} when integer(T1) ->
	    T1;
	{_,T2,_} when integer(T2) ->
	    T2;
	_->
	    ?CHECK_EXCEPTION(gen_server:call(orber_policyserver,
					     {PolicyRef, '_get_policy_type', []}, 
					     infinity))
    end.

%%-----------------------------------------------------------
%% function : create
%% Arguments: PolicyType
%% Returns  : A Policy object
%% Effect   : Creates a new policy object
%% Exception: 
%%------------------------------------------------------------

create(TypeId, PolicyType, PolicyValue) 
  when integer(PolicyType), atom(PolicyValue), list(TypeId) ->
    ObjRef = corba:mk_light_objkey(TypeId, orber_policyserver),
    Key = term_to_binary({PolicyType, PolicyValue}),
    iop_ior:set_privfield(ObjRef, Key);

create(TypeId, PolicyType, PolicyValue) 
  when integer(PolicyType), list(TypeId) ->
    ?CHECK_EXCEPTION(gen_server:call(orber_policyserver,
				     {create, TypeId, PolicyType, PolicyValue},
				     infinity)).

%%-----------------------------------------------------------
%% function : copy
%% Arguments: PolicyRef
%% Returns  : PolicyRef
%% Effect   : Copies a PolicyRef
%% Exception: 
%%------------------------------------------------------------

copy(Policy) ->
    ?CHECK_EXCEPTION(gen_server:call(orber_policyserver,
				     {Policy, copy, []}, infinity)).

%%-----------------------------------------------------------
%% function : destroy
%% Arguments: PolicyRef
%% Returns  : ok
%% Effect   : Destroys a PolicyRef
%% Exception: 
%%------------------------------------------------------------

destroy(Policy) ->
    ?CHECK_EXCEPTION(gen_server:call(orber_policyserver,
				     {Policy, destroy, []}, infinity)).


%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------

install(Timeout, Options) ->
    %% check if mnesia is running. If not, exit(..).
    mnesia_started(),
    %% Do we have a complete set of IFR tables? If not, create them.
    AllTabs = mnesia:system_info(tables),

    DB_Result1 = case lists:member(corba_policy, AllTabs) of
		    true ->
			case lists:member({local_content, true},
					  Options) of
			    true->
				mnesia:add_table_copy(corba_policy,
						       node(),
						       ram_copies);
			    _ ->
				mnesia:create_table(corba_policy,
						    [{attributes,
						      record_info(fields,
								  corba_policy)}
						     |Options])
			end;
		     _ ->
			mnesia:create_table(corba_policy,
					     [{attributes,
					      record_info(fields,
							  corba_policy)}
					     |Options])
		 end,
    DB_Result2 = case lists:member(corba_policy_associations, AllTabs) of
		    true ->
			case lists:member({local_content, true},
					  Options) of
			    true->
				mnesia:add_table_copy(corba_policy_associations,
						       node(),
						       ram_copies);
			    _ ->
				mnesia:create_table(corba_policy_associations,
						    [{attributes,
						      record_info(fields,
								  corba_policy_associations)}
						     |Options])
			end;
		     _ ->
			mnesia:create_table(corba_policy_associations,
					     [{attributes,
					      record_info(fields,
							  corba_policy_associations)}
					     |Options])
		 end,
    
    Wait = mnesia:wait_for_tables([corba_policy, corba_policy_associations],
				  Timeout),
    %% Check if any error has occured yet. 
    %% If there are errors, return them.
    if
	DB_Result1 == {atomic, ok},
	DB_Result2 == {atomic, ok},
	Wait == ok ->
	    ok;
	true ->
	    {error, {DB_Result1, DB_Result2, Wait}}
    end.

%%-----------------------------------------------------------------
%% Func: handle_call/3
%%
%% Comment:
%% In orber_policyserver gen_server all exceptions are tupples and corba:raise
%% may not be used. It is too time consuming to add catches in every 
%% function before returning. On the client side there is a case which 
%% maps every tupple on the format {'exception', E} to corba:raise(E).
%%-----------------------------------------------------------------

%%------ stop -----------------------------------------------------
handle_call(stop, From, State) ->
    {stop, normal, [], State};

%%------ _get_value -----------------------------------------------
handle_call({Ref, '_get_value', []}, From, State) ->
    Policy = iop_ior:get_privfield(Ref),
    case binary_to_term(Policy) of
	{_, V} when atom(V) ->
	    {reply, V, State};
	_->
	    case query_result(mnesia:dirty_read({corba_policy, Policy})) of
		Val when record(Val, corba_policy) ->
		    {reply, Val#corba_policy.value, State};
		R ->
		    {reply, R, State}
	    end
    end;

%%------ _get_policy_type -----------------------------------------
handle_call({Ref, '_get_policy_type', []}, From, State) ->
    Policy = iop_ior:get_privfield(Ref),
    case catch binary_to_term(Policy) of
	{T,_} when integer(T) ->
	    {reply, T, State};
	_->
	    {reply, 
	     {'EXCEPTION', #'BAD_PARAM'{completion_status=?COMPLETED_NO}},
	     State}
    end;

%%------ create ---------------------------------------------------
handle_call({create, TypeId, PolicyType, PolicyValue}, From, State) 
  when integer(PolicyType) ->
    ObjRef = corba:mk_light_objkey(TypeId, orber_policyserver),
    Key = term_to_binary({node(), PolicyType, now()}),
    Ref = iop_ior:set_privfield(ObjRef, Key),

    Policy = #corba_policy{key = Key,
			   policy_id = TypeId, 
			   type      = PolicyType,
			   value     = PolicyValue},
    _F = ?write_function(Policy),
    case write_result(mnesia:transaction(_F)) of
	ok ->
	    {reply, Ref, State};
	_ ->
	    {reply, {'EXCEPTION', #'INTERNAL'{completion_status=?COMPLETED_NO}},
	     State}
    end;

%%------ get_policy -----------------------------------------------
handle_call({get_policy, ObjectRef, PolicyType}, From, State) 
  when integer(PolicyType) ->
    _Fun = ?match_func(ObjectRef),
    case catch mnesia:transaction(_Fun) of
        {atomic, [{_,_,Policies}]} when list(Policies) ->
	    case extract_policy(Policies, PolicyType) of
		false ->
		    {reply,
		     {'EXCEPTION', #'BAD_PARAM'{completion_status=?COMPLETED_NO}}, 
		     State};
		{ok, Policy} -> 
		    {reply, Policy, State}
	    end;
	_ ->
	    {reply,
	     {'EXCEPTION', #'BAD_PARAM'{completion_status=?COMPLETED_NO}}, 
	     State}
    end;
%%------ associate_policies ---------------------------------------
handle_call({associate_policies, ObjectRef, PolicyList}, From, State)
  when list(PolicyList) ->
    Assoc = #corba_policy_associations{objref = ObjectRef,
				       policies = PolicyList}, 
    _F = ?write_function(Assoc),
    case write_result(mnesia:transaction(_F)) of
	ok ->
	    {reply, ok, State};
	_ ->
	    {reply, {'EXCEPTION', #'INTERNAL'{completion_status=?COMPLETED_NO}},
	     State}
    end;

%%------ unassociate_policies -------------------------------------
handle_call({unassociate_policies, ObjectRef}, From, State) ->
    ?query_check(Qres) = mnesia:dirty_read({corba_policy_associations, 
					    ObjectRef}),
    case Qres of
	[] ->
	    {reply, ok, State};
	[X]  ->
	    _F = ?delete_function({corba_policy_associations, ObjectRef}),
	    R = write_result(mnesia:transaction(_F)),
	    {reply, R, State};
	_ ->
	    {reply, 
	     {'EXCEPTION', #'INTERNAL'{completion_status=?COMPLETED_NO}},
	     State}
    end;

%%------ destroy --------------------------------------------------
handle_call({Ref, destroy, []}, From, State) ->
    Policy = iop_ior:get_privfield(Ref),
    ?query_check(Qres) = mnesia:dirty_read({corba_policy, Policy}),
    case Qres of
	[] ->
	    {reply, ok, State};
	[X]  ->
	    _F = ?delete_function({corba_policy, Policy}),
	    R = write_result(mnesia:transaction(_F)),
	    {reply, R, State};
	_ ->
	    {reply, 
	     {'EXCEPTION', #'INTERNAL'{completion_status=?COMPLETED_NO}},
	     State}
    end;

%%------ copy -----------------------------------------------------
handle_call({Policy, copy, []}, From, State) ->
    %% For now do nothing.
    {reply, Policy, State};

%%------ error ----------------------------------------------------
handle_call(_, _, State) ->
    {reply, 
     {'EXCEPTION', #'BAD_PARAM'{completion_status=?COMPLETED_NO}}, 
     State}.



%%--------------- MISC FUNCTIONS, E.G. DEBUGGING -------------
%%-----------------------------------------------------------
%% function : extract_policy
%% Arguments: PolicyList, PolicyType
%% Returns  : {ok, Policy} if found, otherwise 'false'.
%% Effect   : Get the Policy with matching PolicyType.
%% Exception: -
%%------------------------------------------------------------

extract_policy([], _) -> false;
extract_policy([Phead|Ptail], PolicyType) ->
    Key = iop_ior:get_privfield(Phead),
    case catch binary_to_term(Key) of
	{PolicyType,_} ->
	    {ok, Phead};
	_->
	    extract_policy(Ptail, PolicyType)
    end.
  

%%-----------------------------------------------------------
%% function : query_result
%% Arguments: -
%% Returns  : -
%% Effect   : Check a read transaction
%% Exception: Type or 'EXCEPTION' (if failure)
%%------------------------------------------------------------

query_result(?query_check(Qres)) -> 
    case Qres of
	[Hres] ->
	    Hres;
	[] ->
	    {'EXCEPTION', #'OBJECT_NOT_EXIST'{completion_status=?COMPLETED_NO}};
	Other ->
	    {'EXCEPTION', #'INTERNAL'{completion_status=?COMPLETED_NO}}
    end.

%%------------------------------------------------------------
%% function : write_result
%% Arguments: -
%% Returns  : -
%% Effect   : Check a write transaction
%% Exception: 'ok' or 'EXCEPTION' (if failure)
%%------------------------------------------------------------

write_result({atomic,ok}) -> ok;
write_result(Foo) ->
    {'EXCEPTION', #'INTERNAL'{completion_status=?COMPLETED_NO}}.

%%-----------------------------------------------------------
%% function : mnesia_started
%% Arguments: -
%% Returns  : -
%% Effect   : If mnesia is not running do exit(...)
%% Exception: 
%%------------------------------------------------------------

mnesia_started() ->
    case mnesia:system_info(is_running) of
	no ->
	    exit("Mnesia not running. Cannot start corba policy server");
	_ ->
	    ok
    end.

%%--------------- END OF MODULE ------------------------------
