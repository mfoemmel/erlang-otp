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
%% File: corba.erl
%% 
%% Description:
%%    This file contains the CORBA::ORB interface plus some 
%%    Orber specific functions.
%%
%% Creation date: 970115
%%-----------------------------------------------------------------
-module(corba).

-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").

%%-----------------------------------------------------------------
%% Standard interface CORBA
%%-----------------------------------------------------------------
-export([orb_init/1, orb_init/2]).
%%-----------------------------------------------------------------
%% Standard interface CORBA::ORB
%%-----------------------------------------------------------------
-export([%create_list/2,
	 %create_operation_list/2,
	 %% get_default_context/1,
	 %% 'BOA_init/2,
	 resolve_initial_references/1,
	 resolve_initial_references_local/1,
	 list_initial_services/0,
	 add_initial_service/2,
	 remove_initial_service/1,
	 resolve_initial_references_remote/2,
	 list_initial_services_remote/1,
	 object_to_string/1,
	 string_to_object/1]).

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([create/2,
	 create/3,
	 create/4,
	 create_link/2,
	 create_link/3,
	 create_link/4,
	 create_remote/3,
	 create_remote/5,
	 create_link_remote/3,
	 create_link_remote/5,
	 create_nil_objref/0,
	 dispose/1,
	 create_subobject_key/2,
	 get_subobject_key/1,
	 get_pid/1,
	 raise/1,
	 print_object/1,
	 print_object/2,
	 add_alternate_iiop_address/3]).

%%-----------------------------------------------------------------
%% Internal (inside orber implementation) exports
%%-----------------------------------------------------------------
-export([call/4, call/5, reply/2,
	 cast/4, cast/5, locate/1, locate/2,
	 request_from_iiop/6,
	 common_create/5,
	 mk_objkey/4,
	 mk_light_objkey/2,
	 objkey_to_string/1,
	 string_to_objkey/1,
	 string_to_objkey_local/1,
	 call_relay/3,
	 cast_relay/2, 
	 handle_init/2,
	 handle_terminate/3,
	 handle_info/3,
	 handle_code_change/4,
	 handle_call/7, 
	 handle_call/10, 
	 handle_cast/9,
	 handle_cast/6]).

%%-----------------------------------------------------------------
%% Internal definitions
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 5).

%% Defines possible configuration parameters a user can add when
%% creating new CORBA objects.
-record(options, {sup_child = false, 
		  persistent = false, 
		  regname = [], 
		  pseudo = false,
		  object_flags = ?ORB_INIT_FLAGS,
		  object_flags_set = ?ORB_INIT_FLAGS,
		  create_options = []}).

-record(extra, {timeout = infinity,
		context = []}).


%%------------------------------------------------------------
%%
%% Implementation of CORBA CORBA::ORB interfaces
%%
%%------------------------------------------------------------

%%create_list(Count) ->
%%    corba_nvlist:create_list(Count).

%%create_operation_list(OpDef) ->
%%    corba_nvlist:create_operation_list(OpDef).

orb_init(KeyValueList) ->
    orb_init(KeyValueList, "ORBER").

orb_init([], Name) ->
    ok;
orb_init(KeyValueList, Name) ->
    orber:multi_configure(KeyValueList);

orb_init(Args, Name) ->
    exit("Bad parameters, should be: [{Key, Value}, ...]").

%%-----------------------------------------------------------------
%% Initial reference handling
%%-----------------------------------------------------------------
resolve_initial_references(ObjectId) ->   
    case use_local_host(ObjectId) of
	true ->
	    orber_initial_references:get(ObjectId);
	Ref ->
	    corba:string_to_object(Ref)
    end.

resolve_initial_references_local(ObjectId) ->   
    orber_initial_references:get(ObjectId).

list_initial_services() ->  
    Local = orber_initial_references:list(),
    case orber:get_ORBInitRef() of
	undefined ->
	    Local;
	InitRef ->
	    Local ++ get_prefixes(InitRef, [])
    end.

get_prefixes([], Acc) ->
    Acc;
%% A list of ORBInitRef's
get_prefixes([H|T], Acc) when list(H) ->
    [Key|_] = string:tokens(H, "="),
    get_prefixes(T, [Key|Acc]);
%% A single ORBInitRef
get_prefixes(InitRef, Acc) when list(InitRef) ->
    [Key|_] = string:tokens(InitRef, "="),
    [Key];
get_prefixes(What, _) ->
    orber:dbg("[~p] corba:get_prefixes(~p); 
Malformed argument?", [?LINE, What], ?DEBUG_LEVEL),
    raise(#'BAD_PARAM'{completion_status = ?COMPLETED_NO}).


use_local_host(ObjectId) ->
    case orber:get_ORBInitRef() of
	undefined ->
	    case orber:get_ORBDefaultInitRef() of
		undefined ->
		    true;
		DefRef ->
		    DefRef++"/"++ObjectId
	    end;
	InitRef ->
	    case check_prefixes(InitRef, ObjectId) of
		false ->
		    case orber:get_ORBDefaultInitRef() of
			undefined ->
			    true;
			DefRef ->
			    DefRef++"/"++ObjectId
		    end;
		UseRef ->
		    strip_junk(UseRef)
	    end
    end.


check_prefixes([], _) ->
    false;
%% A list of ORBInitRef's
check_prefixes([H|T], ObjectId) when list(H) ->
    case prefix(ObjectId, H) of
	false ->
	    check_prefixes(T, ObjectId);
	UseRef ->
	    UseRef
    end;
%% A single ORBInitRef
check_prefixes(InitRef, ObjectId) when list(InitRef) ->
    case prefix(ObjectId, InitRef) of
	false ->
	    false;
	UseRef ->
	    UseRef
    end;
check_prefixes(What,_) -> 
    orber:dbg("[~p] corba:check_prefixes(~p); 
Malformed argument?", [?LINE, What], ?DEBUG_LEVEL),
    raise(#'BAD_PARAM'{completion_status = ?COMPLETED_NO}).


%% Valid is, for example, "NameService = corbaloc::host/NameService". 
%% Hence, we must remove ' ' and '='.
strip_junk([32|T]) ->
    strip_junk(T);
strip_junk([$=|T]) ->
    strip_junk(T);
strip_junk(Ref) ->
    Ref.

add_initial_service(ObjectId, ObjectRef) ->   
    orber_initial_references:add(ObjectId, ObjectRef).

remove_initial_service(ObjectId) ->  
    orber_initial_references:remove(ObjectId).

resolve_initial_references_remote(ObjectId, []) ->
    raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO});
resolve_initial_references_remote(ObjectId, [RemoteModifier| Rest]) 
  when list(RemoteModifier) ->
    case lists:prefix("iiop://", RemoteModifier) of
       true ->
	    [_, Host, Port] = string:tokens(RemoteModifier, ":/"),
	    IOR = iop_ior:create_external(orber:giop_version(), "", 
				 Host, list_to_integer(Port), "INIT"),
	    %% We know it's an external referens. Hence, no need to check.
	    {_, Key} = iop_ior:get_key(IOR),
	    orber_iiop:request(Key, 'get', [ObjectId], 
			       {{'tk_objref', 12, "object"},
				[{'tk_string', 0}],
				[]}, 'true', infinity, IOR, []);
       false ->
	    resolve_initial_references_remote(ObjectId, Rest)
    end;
resolve_initial_references_remote(ObjectId, []) ->
    raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

list_initial_services_remote([]) ->
    raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO});
list_initial_services_remote([RemoteModifier| Rest]) when list(RemoteModifier) ->
    case lists:prefix("iiop://", RemoteModifier) of
       true ->
	    [_, Host, Port] = string:tokens(RemoteModifier, ":/"),
	    IOR = iop_ior:create_external(orber:giop_version(), "", 
				 Host, list_to_integer(Port), "INIT"),
	    %% We know it's an external referens. Hence, no need to check.
	    {_, Key} = iop_ior:get_key(IOR),
	    orber_iiop:request(Key, 'list', [],
			       {{'tk_sequence', {'tk_string',0},0},
				[], []}, 'true', infinity, IOR, []);
	false -> 
	    list_initial_services_remote(Rest)
    end;
list_initial_services_remote(_) ->
    raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).



%%-----------------------------------------------------------------
%% Objectreference convertions
%%-----------------------------------------------------------------
object_to_string(Object) ->
    iop_ior:string_code(Object).

string_to_object(IORString) ->
    case lists:prefix("IOR", IORString) of
	true ->
	    {ObjRef, _, _} = iop_ior:string_decode(IORString),
	    ObjRef;
	_ ->
	    %% CORBA-2.4 allows both IOR and ior prefix.
	    case lists:prefix("ior", IORString) of
		true ->
		    {ObjRef, _, _} = iop_ior:string_decode(IORString),
		    ObjRef;
		_ ->
		    Data = orber_cosnaming_utils:select_type(IORString),
		    case orber_cosnaming_utils:lookup(Data) of
			String when list(String) ->
			    {Obj, _, _} = iop_ior:string_decode(String),
			    Obj;
			ObjRef ->
			    ObjRef
		    end
	    end
    end.


%%------------------------------------------------------------
%%
%% Implementation of NON-standard functions
%%
%%------------------------------------------------------------
create(Module, TypeID) ->
    create(Module, TypeID, []).

create(Module, TypeID, Env) ->
    common_create(Module, TypeID, Env, [], 'start').

create(Module, TypeID, Env, {Type, RegName}) ->
    common_create(Module, TypeID, Env, [{regname, {Type, RegName}}], 'start');
create(Module, TypeID, Env, Options) ->
    common_create(Module, TypeID, Env, Options, 'start').


create_link(Module, TypeID) ->
    create_link(Module, TypeID, []).

create_link(Module, TypeID, Env) ->
    common_create(Module, TypeID, Env, [], 'start_link').

create_link(Module, TypeID, Env, {Type, RegName}) ->
    common_create(Module, TypeID, Env, [{regname, {Type, RegName}}], 'start_link');
create_link(Module, TypeID, Env, Options) ->
    common_create(Module, TypeID, Env, Options, 'start_link').


create_remote(Node, Module, TypeID) ->
    create_remote(Node, Module, TypeID, []).

create_remote(Node, Module, TypeID, Env) ->
    common_create_remote(Node, Module, TypeID, Env, [], 'start').

create_remote(Node, Module, TypeID, Env, {Type, RegName}) ->
    common_create_remote(Node, Module, TypeID, Env, [{regname, {Type, RegName}}], 'start');
create_remote(Node, Module, TypeID, Env, Options) ->
    common_create_remote(Node, Module, TypeID, Env, Options, 'start').


create_link_remote(Node, Module, TypeID) ->
    create_link_remote(Node, Module, TypeID, []).

create_link_remote(Node, Module, TypeID, Env) ->
    common_create_remote(Node, Module, TypeID, Env, [], 'start_link').

create_link_remote(Node, Module, TypeID, Env, {Type, RegName}) ->
    common_create_remote(Node, Module, TypeID, Env, [{regname, {Type, RegName}}], 'start_link');
create_link_remote(Node, Module, TypeID, Env, Options) ->
    common_create_remote(Node, Module, TypeID, Env, Options, 'start_link').

common_create_remote(Node, Module, TypeID, Env, {Type, RegName}, StartMethod) ->
    common_create_remote(Node, Module, TypeID, Env, [{regname, {Type, RegName}}], StartMethod);
common_create_remote(Node, Module, TypeID, Env, Options, StartMethod) ->
    case node_check(Node) of
	true ->
	    rpc:call(Node, corba, common_create, [Module, TypeID, Env, Options, StartMethod]);
	_ ->
	    orber:dbg("[~p] corba:common_create_remote(~p); 
Node not in current domain.", [?LINE, Node], ?DEBUG_LEVEL),
	    raise(#'OBJ_ADAPTER'{completion_status=?COMPLETED_NO})
    end.

node_check(Node) ->
    lists:member(Node,orber:orber_nodes()).

common_create(Module, TypeID, Env, Options, StartMethod) when list(Options) ->
    Opt = evaluate_options(Options, #options{}),
    case Opt#options.regname of
	[] ->
	    ok;
	{'local', Atom} when atom(Atom), Opt#options.persistent == false ->
	    ok;
	{'global', _} ->
	    ok;
	Why ->
	    orber:dbg("[~p] corba:common_create(~p, ~p); 
bad name type or combination(~p).", [?LINE, Module, Options, Why], ?DEBUG_LEVEL),
	    raise(#'BAD_PARAM'{minor=(?ORBER_VMCID bor 1), 
				     completion_status=?COMPLETED_NO})   
    end,
    case Opt#options.pseudo of
	false ->
	    case gen_server:StartMethod(Module, {Opt, Env}, 
                                        Opt#options.create_options) of
		{ok, Pid} ->
		    case catch mk_objkey(Module, Pid, Opt#options.regname, 
                                         Opt#options.persistent, 
                                         Opt#options.object_flags) of
			{'EXCEPTION', E} ->
			    %% This branch is only used if we couldn't register 
			    %% our new objectkey due to an internal error in orber.
			    gen_server:call(Pid, stop),
			    raise(E);
			{'EXIT', R} ->
			    %% This branch takes care of exit values
			    %% which aren't expected (due to bug).
			    gen_server:call(Pid, stop),
	                    raise(#'BAD_PARAM'{minor=(?ORBER_VMCID bor 1), 
				                     completion_status=?COMPLETED_NO});
			Objkey when Opt#options.sup_child == true ->
		            {ok, Pid, Objkey};
                        Objkey ->
			    Objkey
		    end;
		X ->
		    X
	    end;
	true ->
	    ModuleImpl = list_to_atom(lists:concat([Module, '_impl'])),
	    case ModuleImpl:init(Env) of
		{ok, State} ->
		    create_subobject_key(mk_pseudo_objkey(Module, ModuleImpl, 
                                                          Opt#options.object_flags),
                                                          State);
		{ok, State,_} ->
		    create_subobject_key(mk_pseudo_objkey(Module, ModuleImpl,
                                                          Opt#options.object_flags),
                                                          State);
		Reason ->
		    orber:dbg("[~p] corba:common_create(~p);
'init' function incorrect(~p).", [?LINE, ModuleImpl, Reason], ?DEBUG_LEVEL),
	            raise(#'BAD_PARAM'{minor=(?ORBER_VMCID bor 1), 
				             completion_status=?COMPLETED_NO})   
	    end;
	What ->
	    orber:dbg("[~p] corba:common_create(~p, ~p); 
not a boolean(~p).", [?LINE, Module, Options, What], ?DEBUG_LEVEL),
	    raise(#'BAD_PARAM'{minor=(?ORBER_VMCID bor 1), 
				     completion_status=?COMPLETED_NO})   
    end.

%%----------------------------------------------------------------------
%% Function   : dispose
%% Arguments  : Object
%% Returns    : 
%% Description: Terminate the object represented by the supplied reference.
%%----------------------------------------------------------------------
dispose(?ORBER_NIL_OBJREF) ->
    ok;
dispose(Obj) ->
    corba_boa:dispose(Obj).

%%----------------------------------------------------------------------
%% Function   : create_nil_objref
%% Arguments  : -
%% Returns    : A NIL object reference
%% Description: 
%%----------------------------------------------------------------------
create_nil_objref() ->
    ?ORBER_NIL_OBJREF.

%%----------------------------------------------------------------------
%% Function   : create_subobject_key
%% Arguments  : A local object reference and an Erlang term().
%% Returns    : A new instance of the supplied reference with the 
%%              sub-object field changed to the given value.
%% Description: Initially, this field is set to 'undefined'
%%----------------------------------------------------------------------
create_subobject_key(Objkey, B) when binary(B) ->
    iop_ior:set_privfield(Objkey, B);
create_subobject_key(Objkey, T) ->
    create_subobject_key(Objkey, term_to_binary(T)).

%%----------------------------------------------------------------------
%% Function   : get_subobject_key
%% Arguments  : A local object reference
%% Returns    : Erlang term().
%% Description: Return the value set by using create_subobject_key/2
%%----------------------------------------------------------------------
get_subobject_key(Objkey) ->
    iop_ior:get_privfield(Objkey).

%%----------------------------------------------------------------------
%% Function   : get_pid
%% Arguments  : A local object reference
%% Returns    : If the object is local and is associated with a pid, this
%%              pid is returned. Otherwise, external- or pseudo-object,
%%              an exception is raised.
%% Description: 
%%----------------------------------------------------------------------
get_pid(Objkey) ->
    case iop_ior:get_key(Objkey) of
	{'internal', Key, _, _, _} ->
	    orber_objectkeys:get_pid(Key);
	{'internal_registered', Key, _, _, _} when atom(Key) ->
	    case whereis(Key) of
		undefined ->
		    raise(#'OBJECT_NOT_EXIST'{completion_status=?COMPLETED_NO});
		Pid ->
		    Pid
	    end;
	 R ->
	    orber:dbg("[~p] corba:get_pid(~p); 
Probably a pseudo- or external object(~p).", [?LINE, Objkey, R], ?DEBUG_LEVEL),
	    raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO})
    end.

%%----------------------------------------------------------------------
%% Function   : raise
%% Arguments  : Local exception representation.
%% Returns    : Throws the exception.
%% Description: 
%%----------------------------------------------------------------------
raise(E) ->
    throw({'EXCEPTION', E}).

%%----------------------------------------------------------------------
%% Function   : reply
%% Arguments  : To - pid
%%              Reply - Erlang term().
%% Returns    : 
%% Description: Used to reply to the invoker but still be able
%%              to do some more work in the callback module.
%%----------------------------------------------------------------------
reply(To, Reply) ->
    gen_server:reply(To, Reply).

%%----------------------------------------------------------------------
%% Function   : print_object
%% Arguments  : An object represented as one of the following:
%%               - local (tuple)
%%               - IOR
%%               - stringified IOR
%%               - corbaloc- or corbaname-schema
%%              IoDevice - the same as the io-module defines.
%% Returns    : 
%% Description: Prints the object's components and profiles.
%%----------------------------------------------------------------------
print_object(Object) ->
    iop_ior:print(Object).
print_object(Object, IoDevice) ->
    iop_ior:print(IoDevice, Object).

%%----------------------------------------------------------------------
%% Function   : add_alternate_iiop_address
%% Arguments  : Local object (tuple or IOR).
%%              IP - IP-string
%%              Port - integer().
%% Returns    : A local IOR with a TAG_ALTERNATE_IIOP_ADDRESS component.
%% Description: 
%%----------------------------------------------------------------------
add_alternate_iiop_address(Obj, Host, Port) when list(Host), integer(Port) ->
    TC = #'IOP_TaggedComponent'{tag = ?TAG_ALTERNATE_IIOP_ADDRESS, 
				component_data = #'ALTERNATE_IIOP_ADDRESS'{
				  'HostID' = Host, 
				  'Port' = Port}},
    iop_ior:add_component(Obj, TC);
add_alternate_iiop_address(Obj, Host, Port) ->
    orber:dbg("[~p] corba:add_alternate_iiop_address(~p, ~p); 
Incorrect argument(s). Host must be IP-string and Port an integer.", 
	      [?LINE, Host, Port], ?DEBUG_LEVEL),
    raise(#'BAD_PARAM'{completion_status = ?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Generic functions for accessing the call-back modules (i.e. X_impl.erl).
%% These functions are invoked by the generated stubs.
%%-----------------------------------------------------------------
handle_init(M, {Options, Env}) ->
    case M:init(Env) of
	{ok, State} ->
	    {ok, {0, State}};
	{ok,State,Timeout} ->
	    {ok, {0, State}, Timeout};
	Other ->
	    %% E.g. ignore | {stop, Reason}
	    Other
    end.


handle_terminate(M, Reason, {InternalState, State}) ->
    catch (M:terminate(Reason, State)).

handle_info(M, Info, {InternalState, State}) ->
    case M:handle_info(Info, State) of
	{noreply,NewState} ->
	    {noreply, {InternalState, NewState}};
	{noreply, NewState, Timeout} ->
	    {noreply, {InternalState, NewState}, Timeout};
	{stop, Reason, NewState} ->
	    {stop, Reason, {InternalState, NewState}}
    end.

handle_code_change(M, OldVsn, {InternalState, State}, Extra) ->
    {ok, NewState} = M:code_change(OldVsn, State, Extra),
    {ok, {InternalState, NewState}}.
	    

%% This function handles call Pre- & Post-conditions.
handle_call(M, F, A, {InternalState, State}, Ctx, This, From,
	    PreData, PostData, Stub) ->
    CArgs = call_state(A, State, This, From),
    case catch invoke_precond(PreData, Stub, F, CArgs) of
	{'EXIT', Why} ->
	    exit(Why);
	{'EXCEPTION', E} ->
	    {reply, {'EXCEPTION', E}, {InternalState, State}};
	ok ->
	    Result = handle_call2(M, F, CArgs, InternalState, State, Ctx),
	    case catch invoke_postcond(PostData, Stub, F, CArgs, Result) of
		{'EXIT', Why} ->
		    exit(Why);
		{'EXCEPTION', E} ->
		    {reply, {'EXCEPTION', E}, {InternalState, State}};
		ok ->
		    Result
	    end
    end.


invoke_precond(false, _, _, _) ->
    ok;
invoke_precond({CondM, CondF}, Stub, F, CArgs) ->
    CondM:CondF(Stub, F, CArgs).

%% We must remove the Internal State before invoking post-cond.
invoke_postcond(false, _, _, _, _) ->
    ok;
invoke_postcond({CondM, CondF}, Stub, F, CArgs,	{reply, Reply, {_, NS}}) ->
    CondM:CondF(Stub, F, CArgs, {reply, Reply, NS});
invoke_postcond({CondM, CondF}, Stub, F, CArgs, {reply, Reply, {_, NS}, Timeout}) ->
    CondM:CondF(Stub, F, CArgs, {reply, Reply, NS, Timeout});
invoke_postcond({CondM, CondF}, Stub, F, CArgs, {stop, Reason, Reply, {_, NS}}) ->
    CondM:CondF(Stub, F, CArgs, {stop, Reason, Reply, NS});
invoke_postcond({CondM, CondF}, Stub, F, CArgs, {stop, Reason, {_, NS}}) ->
    CondM:CondF(Stub, F, CArgs, {stop, Reason, NS});
invoke_postcond({CondM, CondF}, Stub, F, CArgs, {noreply,{_, NS}}) ->
    CondM:CondF(Stub, F, CArgs, {noreply,NS});
invoke_postcond({CondM, CondF}, Stub, F, CArgs, {noreply,{_, NS}, Timeout}) ->
    CondM:CondF(Stub, F, CArgs, {noreply, NS, Timeout});
invoke_postcond({CondM, CondF}, Stub, F, CArgs, Result) ->
    CondM:CondF(Stub, F, CArgs, Result).


handle_call(M, F, A, {InternalState, State}, Ctx, This, From) ->
    handle_call2(M, F, call_state(A, State, This, From), InternalState, State, Ctx).

handle_call2(M, F, A, InternalState, State, []) ->
    case catch apply(M, F, A) of
	{reply, Reply, NewState} ->
	    {reply, add_context(Reply), {InternalState, NewState}};
	{reply, Reply, NewState, Timeout} ->
	    {reply, add_context(Reply), {InternalState, NewState}, Timeout};
	{stop, Reason, Reply, NewState} ->
	    {stop, Reason, add_context(Reply), {InternalState, NewState}};
	{stop, Reason, NewState} ->
	    {stop, Reason, {InternalState, NewState}};
	{noreply,NewState} ->
	    {noreply,{InternalState, NewState}};
	{noreply,NewState,Timeout} ->
	    {noreply,{InternalState, NewState},Timeout};
	{'EXIT', Reason} ->
	    exit(Reason);
	{'EXCEPTION', E} ->
	    {reply, add_context({'EXCEPTION', E}), {InternalState, State}};
	{Reply, NewState} -> 
	    {reply, add_context(Reply), {InternalState, NewState}}
    end;
handle_call2(M, F, A, InternalState, State, Ctx) ->
    %% Set the new Context.
    put(oe_in_context, Ctx),
    case catch apply(M, F, A) of
	{reply, Reply, NewState} ->
	    put(oe_server_in_context, undefined),
	    {reply, add_context(Reply), {InternalState, NewState}};
	{reply, Reply, NewState, Timeout} ->
	    put(oe_server_in_context, undefined),
	    {reply, add_context(Reply), {InternalState, NewState}, Timeout};
	{stop, Reason, Reply, NewState} ->
	    {stop, Reason, add_context(Reply), {InternalState, NewState}};
	{stop, Reason, NewState} ->
	    {stop, Reason, {InternalState, NewState}};
	{noreply,NewState} ->
	    put(oe_server_in_context, undefined),
	    {noreply, {InternalState, NewState}};
	{noreply, {InternalState, NewState}, Timeout} ->
	    put(oe_server_in_context, undefined),
	    {noreply, {InternalState, NewState},Timeout};
	{'EXIT', Reason} ->
		exit(Reason);
	{'EXCEPTION', E} ->
	    put(oe_server_in_context, undefined),
	    {reply, add_context({'EXCEPTION', E}), {InternalState, State}};
	{Reply, NewState} -> 
	    put(oe_server_in_context, undefined),
	    {reply, add_context(Reply), {InternalState, NewState}}
    end.

call_state(A, State, false, false) ->
    [State|A];
call_state(A, State, false, From) ->
    [From, State|A];
call_state(A, State, This, false) ->
    [This, State|A];
call_state(A, State, This, From) ->
    [This, From, State|A].

cast_state(A, State, false) ->
    [State|A];
cast_state(A, State, This) ->
    [This, State|A].

add_context(Reply) ->
    case put(oe_server_out_context, undefined) of
	undefined ->
	    Reply;
	 OutCtx ->
	    Reply
    end.


%% This function handles call Pre- & Post-conditions.
handle_cast(M, F, A, {InternalState, State}, Ctx, This, PreData, PostData, Stub) ->
    CArgs = cast_state(A, State, This),
    case catch invoke_precond(PreData, Stub, F, CArgs) of
	{'EXIT', Why} ->
	    exit(Why);
	{'EXCEPTION', E} ->
	    {noreply, {InternalState, State}};
	ok ->
	    Result = handle_cast2(M, F, CArgs, InternalState, State, Ctx),
	    case catch invoke_postcond(PostData, Stub, F, CArgs, Result) of
		{'EXIT', Why} ->
		    exit(Why);
		{'EXCEPTION', E} ->
		    {noreply, {InternalState, State}};
		ok ->
		    Result
	    end
    end.


handle_cast(M, F, A, {InternalState, State}, Ctx, This) ->
    handle_cast2(M, F, cast_state(A, State, This), InternalState, State, Ctx).

handle_cast2(M, F, A, InternalState, State, []) ->
    case catch apply(M, F, A) of
	{noreply, NewState} ->
	    {noreply, {InternalState, NewState}};
	{noreply, NewState, Timeout} ->
	    {noreply, {InternalState, NewState}, Timeout};
	{stop, Reason, NewState} ->
	    {stop, Reason, {InternalState, NewState}};
	{'EXCEPTION', _} ->
	    {noreply, {InternalState, State}};
	{'EXIT', Reason} -> 
	    exit(Reason);
	NewState -> 
	    {noreply, {InternalState, NewState}}
    end;
handle_cast2(M, F, A, InternalState, State, Ctx) ->
    put(oe_server_in_context, Ctx),
    case catch apply(M, F, A) of
	{noreply, NewState} ->
	    put(oe_server_in_context, undefined),
	    {noreply, {InternalState, NewState}};
	{noreply, NewState, Timeout} ->
	    put(oe_server_in_context, undefined),
	    {noreply, {InternalState, NewState}, Timeout};
	{stop, Reason, NewState} ->
	    {stop, Reason, {InternalState, NewState}};
	{'EXCEPTION', _} ->
	    put(oe_server_in_context, undefined),
	    {noreply, {InternalState, State}};
	{'EXIT', Reason} -> 
	    exit(Reason);
	NewState -> 
	    put(oe_server_in_context, undefined),
	    {noreply, {InternalState, NewState}}
    end.


%%-----------------------------------------------------------------
%% Corba:call - the function for reqests
%%-----------------------------------------------------------------
call(Obj, Func, Args, TypesOrMod) ->
    call_helper(Obj, Func, Args, TypesOrMod, infinity, []).

call(Obj, Func, Args, TypesOrMod, [{context, Ctx}]) ->
    call_helper(Obj, Func, Args, TypesOrMod, infinity, Ctx);
call(Obj, Func, Args, TypesOrMod, [{timeout, Timeout}]) ->
    call_helper(Obj, Func, Args, TypesOrMod, Timeout, []);
call(Obj, Func, Args, TypesOrMod, Extra) when list(Extra) ->
    ExtraData = extract_extra_data(Extra, #extra{}),
    call_helper(Obj, Func, Args, TypesOrMod, ExtraData#extra.timeout, 
		ExtraData#extra.context);
call(Obj, Func, Args, TypesOrMod, Timeout) ->
    call_helper(Obj, Func, Args, TypesOrMod, Timeout, []).

call_helper(Obj, Func, Args, TypesOrMod, Timeout, Ctx) ->
    case iop_ior:get_key(Obj) of
	{'internal', Key, _, Flags, Mod} when atom(TypesOrMod) ->
	    Pid = orber_objectkeys:get_pid(Key),
	    call_internal(Pid, Obj, Func, Args, TypesOrMod, 
			  ?ORB_FLAG_TEST(Flags, ?ORB_TYPECHECK), Mod, 
			  Timeout, Ctx, true);
	{'internal_registered', Key, _, Flags, Mod} when atom(TypesOrMod) ->
	    call_internal(Key, Obj, Func, Args, TypesOrMod,
			  ?ORB_FLAG_TEST(Flags, ?ORB_TYPECHECK), Mod, 
			  Timeout, Ctx, true);
	{'internal', Key, _, Flags, Mod} ->
	    Pid = orber_objectkeys:get_pid(Key),
	    call_internal(Pid, Obj, Func, Args, TypesOrMod, 
			  ?ORB_FLAG_TEST(Flags, ?ORB_TYPECHECK), Mod, Timeout, Ctx, 
			  check_version(Mod:module_info(attributes)));
	{'internal_registered', Key, _, Flags, Mod} ->
	    call_internal(Key, Obj, Func, Args, TypesOrMod,
			  ?ORB_FLAG_TEST(Flags, ?ORB_TYPECHECK), Mod, Timeout, Ctx, 
			  check_version(Mod:module_info(attributes)));
	{'external', Key} when atom(TypesOrMod) ->		   
	    case catch TypesOrMod:oe_tc(Func) of
		{'EXIT', What} ->
		    orber:dbg("[~p] corba:call_helper(~p); 
The call-back module does not exist or incorrect IC-version used.
Reason: ~p", [?LINE, TypesOrMod, What], ?DEBUG_LEVEL),
		    raise(#'TRANSIENT'{minor=(?ORBER_VMCID bor 7),
				       completion_status=?COMPLETED_NO});
		undefined ->
		    raise(#'BAD_OPERATION'{minor = (?ORBER_VMCID bor 4),
					   completion_status=?COMPLETED_NO});
		Types ->
		    orber_iiop:request(Key, Func, Args, Types, 'true', Timeout, Obj, Ctx)
	    end;
	{'external', Key} ->		   
	    orber_iiop:request(Key, Func, Args, TypesOrMod, 'true', Timeout, Obj, Ctx)
    end.

extract_extra_data([], ED) ->
    ED;
extract_extra_data([{context, Ctx}|T], ED) ->
    extract_extra_data(T, ED#extra{context = Ctx});
extract_extra_data([{timeout, Timeout}|T], ED) ->
    extract_extra_data(T, ED#extra{timeout = Timeout}).

call_internal(Pid, Obj, Func, Args, Types, Check, Mod, Timeout, Ctx, ICV) 
  when pid(Pid), node(Pid) == node() ->
    typecheck_request(Check, Args, Types, Func),
    Data = case ICV of
	       true ->
		   {Obj, Ctx, Func, Args};
	       false ->
		   {Obj, Func, Args}
	   end,
    case catch gen_server:call(Pid, Data, Timeout) of
	{'EXCEPTION', E} ->
            typecheck_reply(Check, {'EXCEPTION', E}, Mod, Func),
	    raise(E);
	{'EXIT',{timeout, _}} ->
	    raise(#'TIMEOUT'{completion_status=?COMPLETED_MAYBE});
	{'EXIT',R} ->
	    orber:dbg("[~p] corba:call_internal(~p, ~p, ~p); 
call exit(~p).", [?LINE, Func, Args, Types, R], ?DEBUG_LEVEL),
	    raise(#'TRANSIENT'{minor=(?ORBER_VMCID bor 4), 
				     completion_status=?COMPLETED_NO});
	Res ->
            typecheck_reply(Check, Res, Types, Func),
	    Res
    end;
call_internal(Pid, Obj, Func, Args, Types, Check, Mod, Timeout, Ctx, ICV) 
  when pid(Pid) ->
    typecheck_request(Check, Args, Types, Func),
    Data = case ICV of
	       true ->
		   {Obj, Ctx, Func, Args};
	       false ->
		   {Obj, Func, Args}
	   end,
    case catch rpc:call(node(Pid), corba, call_relay, 
			[Pid, Data, Timeout]) of
	{'EXCEPTION', E} ->
            typecheck_reply(Check, {'EXCEPTION', E}, Types, Func),
	    raise(E);
	{badrpc, {'EXIT',R}} ->
	    orber:dbg("[~p] corba:call_internal(~p, ~p, ~p); 
call exit(~p).", [?LINE, Func, Args, Types, R], ?DEBUG_LEVEL),
	    raise(#'TRANSIENT'{minor=(?ORBER_VMCID bor 3), 
				     completion_status=?COMPLETED_MAYBE});
	{badrpc,nodedown} ->
	    orber:dbg("[~p] corba:call_internal(~p, ~p, ~p); 
Node ~p down.", [?LINE, Func, Args, Types, node(Pid)], ?DEBUG_LEVEL),
	    raise(#'TRANSIENT'{minor=(?ORBER_VMCID bor 2), 
				     completion_status=?COMPLETED_NO});
	{badrpc, Reason} ->
	    orber:dbg("[~p] corba:call_internal(~p, ~p, ~p); 
Unable to invoke operation due to: ~p", [?LINE, Func, Args, Types, Reason], ?DEBUG_LEVEL),
	    raise(#'TRANSIENT'{minor=(?ORBER_VMCID bor 5), 
				     completion_status=?COMPLETED_MAYBE});
	Res ->
            typecheck_reply(Check, Res, Types, Func),
	    Res
    end;

%% This case handles if the reference is created as a Pseudo object. Just call apply/3.
call_internal({pseudo, Module}, Obj, Func, Args, Types, Check, Mod, Timeout, Ctx, _) ->
    typecheck_request(Check, Args, Types, Func),
    State = binary_to_term(get_subobject_key(Obj)),
    case catch apply(Module, Func, [Obj, State|Args]) of
	{noreply, _} ->
	    ok;
	{noreply, _, _} ->
	    ok;
	{reply, Reply, _} ->
            typecheck_reply(Check, Reply, Types, Func),
	    Reply;
	{reply, Reply, _, _} ->
            typecheck_reply(Check, Reply, Types, Func),
	    Reply;
	{stop, _, Reply, _} ->
            typecheck_reply(Check, Reply, Types, Func),
	    Reply;
	{stop, _, _} ->
	    ok;
	{'EXCEPTION', E} ->
            typecheck_reply(Check, {'EXCEPTION', E}, Types, Func),
	    raise(E);
	{'EXIT', What} ->
	    orber:dbg("[~p] corba:call_internal(~p, ~p, ~p); 
Pseudo object exit(~p).", [?LINE, Func, Args, Types, What], ?DEBUG_LEVEL),
	    raise(#'TRANSIENT'{minor=(?ORBER_VMCID bor 4),
				     completion_status=?COMPLETED_MAYBE});
	Unknown ->
	    orber:dbg("[~p] corba:call_internal(~p, ~p, ~p); 
Pseudo object failed due to bad return value (~p).", 
				    [?LINE, Func, Args, Types, Unknown], ?DEBUG_LEVEL),
	    raise(#'TRANSIENT'{minor=(?ORBER_VMCID bor 6),
				     completion_status=?COMPLETED_MAYBE})
    end;
call_internal(Registered, Obj, Func, Args, Types, Check, Mod, Timeout, Ctx, ICV) 
  when atom(Registered)->
    typecheck_request(Check, Args, Types, Func),
    case whereis(Registered) of
	undefined ->
	    raise(#'OBJECT_NOT_EXIST'{completion_status=?COMPLETED_NO});
	P ->
	    Data = case ICV of
		       true ->
			   {Obj, Ctx, Func, Args};
		       false ->
			   {Obj, Func, Args}
		   end,
	    case catch gen_server:call(P, Data, Timeout) of
		{'EXCEPTION', E} ->
		    typecheck_reply(Check, {'EXCEPTION', E}, Types, Func),
		    raise(E);
		{'EXIT',{timeout, _}} ->
		    raise(#'TIMEOUT'{completion_status=?COMPLETED_MAYBE});	
		{'EXIT',R} ->
		    orber:dbg("[~p] corba:call_internal(~p, ~p, ~p).
call exit(~p).", [?LINE, Func, Args, Types, R], ?DEBUG_LEVEL),
		    raise(#'TRANSIENT'{minor=(?ORBER_VMCID bor 5), 
			  	             completion_status=?COMPLETED_MAYBE});
		Res ->
                    typecheck_reply(Check, Res, Types, Func),
		    Res
	    end
    end.


typecheck_request(true, Args, Mod, Func) when atom(Mod) ->
    case catch Mod:oe_tc(Func) of
	undefined ->
	    raise(#'BAD_OPERATION'{minor = (?ORBER_VMCID bor 4),
				   completion_status=?COMPLETED_NO});
	{'EXIT', What} ->
	    orber:dbg("[~p] corba:typecheck_request(~p, ~p, ~p); 
The call-back module does not exist or incorrect IC-version used.
Reason: ~p", [?LINE, Mod, Func, Args, What], ?DEBUG_LEVEL),
	    raise(#'TRANSIENT'{minor=(?ORBER_VMCID bor 7),
			       completion_status=?COMPLETED_NO});
	Types ->
	    typecheck_request_helper(Types, Args, Mod, Func)
    end;
typecheck_request(true, Args, Types, Func) ->
    typecheck_request_helper(Types, Args, Types, Func);
typecheck_request(_, _, _, _) ->
    ok.

typecheck_request_helper(Types, Args, Mod, Func) ->
    case catch cdr_encode:validate_request_body({1,2}, Types, Args) of
	{'EXCEPTION', E} ->
	    {_, TC, _} = Types, 
	    error_logger:error_msg("========= Orber Typecheck Request =========
Invoked......: ~p:~p/~p
Typecode.....: ~p
Arguments....: ~p
Result.......: ~p
===========================================~n", 
				   [Mod, Func, length(TC), TC, Args, {'EXCEPTION', E}]),
	    raise(E);
	{'EXIT',R} ->
	    {_, TC, _} = Types, 
	    error_logger:error_msg("========= Orber Typecheck Request =========
Invoked......: ~p:~p/~p
Typecode.....: ~p
Arguments....: ~p
Result.......: ~p
===========================================~n", [Mod, Func, length(TC), TC, Args, {'EXIT',R}]),
	    raise(#'MARSHAL'{completion_status=?COMPLETED_MAYBE});
	_ ->
	    ok
    end.

typecheck_reply(true, Args, Mod, Func) when atom(Mod) ->
    case catch Mod:oe_tc(Func) of
	undefined ->
	    raise(#'BAD_OPERATION'{minor = (?ORBER_VMCID bor 4),
				   completion_status=?COMPLETED_NO});
	{'EXIT', What} ->
	    orber:dbg("[~p] corba:typecheck_reply(~p, ~p, ~p); 
The call-back module does not exist or incorrect IC-version used.
Reason: ~p", [?LINE, Mod, Func, Args, What], ?DEBUG_LEVEL),
	    raise(#'TRANSIENT'{minor=(?ORBER_VMCID bor 7),
				       completion_status=?COMPLETED_NO});
	Types ->
	    typecheck_reply_helper(Types, Args, Mod, Func)
    end;
typecheck_reply(true, Args, Types, Func) ->
    typecheck_reply_helper(Types, Args, Types, Func);
typecheck_reply(_, _, _, _) ->
    ok.

typecheck_reply_helper(Types, Args, Mod, Func) ->
    case catch cdr_encode:validate_reply_body({1,2}, Types, Args) of
	{'tk_except', ExcType, ExcTC, {'EXCEPTION', E}} ->
	    {_, TC, _} = Types, 
	    error_logger:error_msg("========== Orber Typecheck Reply ==========
Invoked........: ~p:~p/~p
Exception Type.: ~p
Typecode.......: ~p
Raised.........: ~p
Result.........: ~p
===========================================~n", 
				   [Mod, Func, length(TC), ExcType, ExcTC, Args, {'EXCEPTION', E}]),
	    raise(E);
	{'EXCEPTION', E} ->
	    {RetType, TC, OutParams} = Types, 
	    error_logger:error_msg("========== Orber Typecheck Reply ==========
Invoked......: ~p:~p/~p
Typecode.....: ~p
Reply........: ~p
Result.......: ~p
===========================================~n", 
				   [Mod, Func, length(TC), [RetType | OutParams], Args, {'EXCEPTION', E}]),
	    raise(E);
	{'tk_except', ExcType, ExcTC, {'EXIT',R}} ->
	    {_, TC, _} = Types, 
	    error_logger:error_msg("========== Orber Typecheck Reply ==========
Invoked........: ~p:~p/~p
Exception Type.: ~p
Typecode.......: ~p
Raised.........: ~p
Result.........: ~p
===========================================~n", 
				   [Mod, Func, length(TC), ExcType, ExcTC, Args, {'EXIT',R}]),
	    raise(#'MARSHAL'{completion_status=?COMPLETED_MAYBE});
	{'EXIT',R} ->
	    {RetType, TC, OutParams} = Types, 
	    error_logger:error_msg("========== Orber Typecheck Reply ==========
Invoked........: ~p:~p/~p
Typecode.......: ~p
Reply..........: ~p
Result.........: ~p
===========================================~n", 
				   [Mod, Func, length(TC), [RetType | OutParams], Args, {'EXIT',R}]),
	    raise(#'MARSHAL'{completion_status=?COMPLETED_MAYBE});
	_ ->
	    ok
    end.

call_relay(Pid, Data, Timeout) ->
    case whereis(orber_objkeyserver) of
	undefined ->
	    raise(#'TRANSIENT'{minor=(?ORBER_VMCID bor 1), completion_status=?COMPLETED_MAYBE});
	_ ->
	    case catch gen_server:call(Pid, Data, Timeout) of
		{'EXCEPTION', E} ->
		    raise(E);
		{'EXIT',{timeout, _}} ->
		    raise(#'TIMEOUT'{completion_status=?COMPLETED_MAYBE});
		{'EXIT',R} ->
		    orber:dbg("[~p] corba:call_internal(~p); 
call exit(~p).", [?LINE, Data, R], ?DEBUG_LEVEL),
		    exit(R);
		Res ->
		    Res
	    end
    end.
	
%%-----------------------------------------------------------------
%% Corba:cast - the function for ONEWAY requests
%%-----------------------------------------------------------------
cast(Obj, Func, Args, TypesOrMod) ->
    cast_helper(Obj, Func, Args, TypesOrMod, []).

cast(Obj, Func, Args, TypesOrMod, [{context, Ctx}]) ->
    cast_helper(Obj, Func, Args, TypesOrMod, Ctx).

cast_helper(Obj, Func, Args, TypesOrMod, Ctx) ->
    case iop_ior:get_key(Obj) of
	{'internal', Key, _, Flags, Mod} when atom(TypesOrMod) ->
	    Pid = orber_objectkeys:get_pid(Key),
	    cast_internal(Pid, Obj, Func, Args, TypesOrMod, 
			  ?ORB_FLAG_TEST(Flags, ?ORB_TYPECHECK), Mod, Ctx, true);
	{'internal_registered', Key, _, Flags, Mod} when atom(TypesOrMod) ->
	    cast_internal(Key, Obj, Func, Args, TypesOrMod, 
			  ?ORB_FLAG_TEST(Flags, ?ORB_TYPECHECK), Mod, Ctx, true);
	{'internal', Key, _, Flags, Mod} ->
	    Pid = orber_objectkeys:get_pid(Key),
	    cast_internal(Pid, Obj, Func, Args, TypesOrMod, 
			  ?ORB_FLAG_TEST(Flags, ?ORB_TYPECHECK), Mod, Ctx,
			  check_version(Mod:module_info(attributes)));
	{'internal_registered', Key, _, Flags, Mod} ->
	    cast_internal(Key, Obj, Func, Args, TypesOrMod, 
			  ?ORB_FLAG_TEST(Flags, ?ORB_TYPECHECK), Mod, Ctx,
			  check_version(Mod:module_info(attributes)));
	{'external', Key} when atom(TypesOrMod) ->
	    case catch TypesOrMod:oe_tc(Func) of
		{'EXIT', What} ->
		    orber:dbg("[~p] corba:cast_helper(~p); 
The call-back module does not exist or incorrect IC-version used.
Reason: ~p", [?LINE, TypesOrMod, What], ?DEBUG_LEVEL),
		    raise(#'TRANSIENT'{minor=(?ORBER_VMCID bor 7),
				       completion_status=?COMPLETED_NO});
		undefined ->
		    raise(#'BAD_OPERATION'{minor = (?ORBER_VMCID bor 4),
					   completion_status=?COMPLETED_NO});
		Types ->
		    orber_iiop:request(Key, Func, Args, Types, 'false', infinity, 
				       Obj, Ctx)
	    end;
	{'external', Key} ->
	    orber_iiop:request(Key, Func, Args, TypesOrMod, 'false', infinity, 
			       Obj, Ctx)
    end.

cast_internal(Pid, Obj, Func, Args, Types, Check, Mod, Ctx, ICV) 
  when pid(Pid), node(Pid) == node() ->
    typecheck_request(Check, Args, Types, Func),
    Data = case ICV of
	       true ->
		   {Obj, Ctx, Func, Args};
	       false ->
		   {Obj, Func, Args}
	   end,
    catch gen_server:cast(Pid, Data),
    ok;
cast_internal(Pid, Obj, Func, Args, Types, Check, Mod, Ctx, ICV) when pid(Pid) ->
    typecheck_request(Check, Args, Types, Func),
    Data = case ICV of
	       true ->
		   {Obj, Ctx, Func, Args};
	       false ->
		   {Obj, Func, Args}
	   end,
    case catch rpc:call(node(Pid), corba, cast_relay, [Pid, Data]) of
	{'EXCEPTION', E} ->
            typecheck_reply(Check, {'EXCEPTION', E}, Mod, Func),
	    raise(E);
	{badrpc, {'EXIT', R}} ->
	    raise(#'TRANSIENT'{minor=(?ORBER_VMCID bor 3), 
				     completion_status=?COMPLETED_MAYBE});
	{badrpc,nodedown} ->
	    orber:dbg("[~p] corba:cast_internal(~p, ~p, ~p); 
Node ~p down.", [?LINE, Func, Args, Types, node(Pid)], ?DEBUG_LEVEL),
	    raise(#'TRANSIENT'{minor=(?ORBER_VMCID bor 2), 
				     completion_status=?COMPLETED_NO});
	Other ->
	    orber:dbg("[~p] corba:cast_internal(~p, ~p, ~p);
Communication with node: ~p failed with reason: ~p.", 
				    [?LINE, Func, Args, Types, node(Pid), Other], ?DEBUG_LEVEL),
	    raise(#'TRANSIENT'{minor=(?ORBER_VMCID bor 5), 
				     completion_status=?COMPLETED_MAYBE})
    end;

%% This case handles if the reference is created as a Pseudo object. Just call apply/3.
cast_internal({pseudo, Module}, Obj, Func, Args, Types, Check, Mod, Ctx, _) ->
    typecheck_request(Check, Args, Types, Func),
    State = binary_to_term(get_subobject_key(Obj)),
    catch apply(Module, Func, [Obj, State|Args]),
    ok;
cast_internal(Registered, Obj, Func, Args, Types, Check, Mod, Ctx, ICV) ->
    typecheck_request(Check, Args, Types, Func),
    case whereis(Registered) of
	undefined ->
	    raise(#'OBJECT_NOT_EXIST'{completion_status=?COMPLETED_NO});
	P ->
	    Data = case ICV of
		       true ->
			   {Obj, Ctx, Func, Args};
		       false ->
			   {Obj, Func, Args}
		   end,
	    gen_server:cast(P, Data)
    end.
	
cast_relay(Pid, Data) ->
    case whereis(orber_objkeyserver) of
	undefined ->
	    raise(#'TRANSIENT'{minor=(?ORBER_VMCID bor 1), 
				     completion_status=?COMPLETED_NO});
	_ ->
	    gen_server:cast(Pid, Data)
    end.

%%-----------------------------------------------------------------
%% Corba:locate - this function is for the moment just used for tests
%%-----------------------------------------------------------------
locate(Obj) ->
    locate(Obj, infinity).

locate(Obj, Timeout) ->
    case iop_ior:get_key(Obj) of
	{'external', Key} ->
	    orber_iiop:locate(Key, Timeout, Obj);
	_ ->
	    orber_objectkeys:check(iop_ior:get_objkey(Obj))
    end.

%%-----------------------------------------------------------------
%% Incomming request from iiop
%%-----------------------------------------------------------------
%% Operations which do not allow object invokation.
request_from_iiop(Obj, '_is_a', [Args], _, _, _) ->
    catch corba_object:is_a(Obj, Args);
%% First the OMG specified this operation to be '_not_existent' and then
%% changed it to '_non_existent' without suggesting that both must be supported.
%% See CORBA2.3.1 page 15-34, Minor revision 2.3.1: October 1999
request_from_iiop(Obj, '_not_existent', _, _, _, _) ->
    catch corba_object:non_existent(Obj);
request_from_iiop(Obj, '_non_existent', _, _, _, _) ->
    catch corba_object:non_existent(Obj);
request_from_iiop(Obj, 'get_policy', [Arg], _, _, _) ->
    catch corba_object:get_policy(Obj, Arg);

%% "Ordinary" operations.
request_from_iiop({Mod, _, _, _, _, _}, oe_get_interface, 
		  _, _, _, _ServiceCtx) when atom(Mod) ->
    case catch Mod:oe_get_interface() of
	{'EXIT', What} ->
	    case catch Mod:handle_call({false, oe_get_interface, []}, false, false) of
		{reply, OpDef, _} ->
		    OpDef;
		_->
		    []
	    end;	    
	undefined ->
	    {'EXCEPTION', #'BAD_OPERATION'{minor = (?ORBER_VMCID bor 4),
					   completion_status='COMPLETED_NO'}};
	Interface ->
	    Interface
    end;
%% This case will not be used as long as the previous case exists. When we choose
%% to no longer support the old style of stubs/skeletons generated by IC the case
%% above must be deleted.
%request_from_iiop({Mod, _, _, _, _, _}, oe_get_interface, 
%		  _, _, _, _ServiceCtx) when atom(Mod) ->
%    case catch Mod:oe_get_interface() of
%	{'EXIT', What} ->
%	    orber:dbg("[~p] corba:request_from_iiop(~p); 
%The call-back module does not exist or incorrect IC-version used.
%Reason: ~p", [?LINE, Mod, What], ?DEBUG_LEVEL),
%	    {'EXCEPTION', #'TRANSIENT'{minor=(?ORBER_VMCID bor 7),
%				       completion_status=?COMPLETED_NO}};
%	undefined ->
%	    {'EXCEPTION', #'BAD_OPERATION'{minor = (?ORBER_VMCID bor 4),
%					   completion_status='COMPLETED_NO'}};
%	Interface ->
%	    Interface
%    end;
request_from_iiop({Mod, pseudo, Module, UserDef, OrberDef, Flags} = ObjRef, 
		  Func, Args, Types, ResponseExpected, ServiceCtx) ->
    State = binary_to_term(get_subobject_key(ObjRef)),
    case ResponseExpected of
	true ->
	    case catch apply(Module, Func, [ObjRef, State|Args]) of
		{noreply, _} ->
		    ok;
		{noreply, _, _} ->
		    ok;
		{reply, Reply, _} ->
		    Reply;
		{reply, Reply, _, _} ->
		    Reply;
		{stop, _, Reply, _} ->
		    Reply;
		{stop, _, _} ->
		    ok;
		{'EXCEPTION', E} ->
		    {'EXCEPTION', E};
		{'EXIT', {undef, _}} ->
		    orber:dbg("[~p] corba:request_from_iiop(~p, ~p, ~p); 
The call-back module does not exist.", [?LINE, Func, Args, Types], ?DEBUG_LEVEL),
		    {'EXCEPTION', #'TRANSIENT'{minor=(?ORBER_VMCID bor 4),
					       completion_status=?COMPLETED_NO}};
		{'EXIT', What} ->
		    orber:dbg("[~p] corba:request_from_iiop(~p, ~p, ~p); 
Pseudo object exit(~p).
The call-back module probably contain an error.",
			      [?LINE, Func, Args, Types, What], ?DEBUG_LEVEL),
		    {'EXCEPTION', #'TRANSIENT'{minor=(?ORBER_VMCID bor 4),
					       completion_status=?COMPLETED_MAYBE}};
		Unknown ->
		    orber:dbg("[~p] corba:request_from_iiop(~p, ~p, ~p); 
Pseudo object failed(~p); 
confirm that the return value is correct (e.g. {reply, Reply, State})", 
			      [?LINE, Func, Args, Types, Unknown], ?DEBUG_LEVEL),
		    {'EXCEPTION', #'TRANSIENT'{minor=(?ORBER_VMCID bor 6),
					       completion_status=?COMPLETED_MAYBE}}
	    end;
	false ->
	    catch apply(Module, Func, [ObjRef, State|Args]),
	    ok;
	true_oneway ->
	    catch apply(Module, Func, [ObjRef, State|Args]),
	    ok
    end;

request_from_iiop({Mod, Type, Key, UserDef, OrberDef, Flags} = ObjRef, 
		  Func, Args, Types, true, ServiceCtx) ->
    case check_version(Mod:module_info(attributes)) of
	true ->
	    case catch gen_server:call(convert_key_to_pid(Key), 
				       {ObjRef, ServiceCtx, Func, Args}, infinity) of
		{'EXIT', What} ->
		    orber:dbg("[~p] corba:request_from_iiop(~p, ~p, ~p); gen_server:call exit: ~p", [?LINE, Func, Args, Types, What], ?DEBUG_LEVEL),
		    {'EXCEPTION', #'TRANSIENT'{minor=(?ORBER_VMCID bor 4),
					       completion_status=?COMPLETED_MAYBE}};
		Result ->
		    Result
	    end;
	false ->
	    case catch gen_server:call(convert_key_to_pid(Key), 
				       {ObjRef, Func, Args}, infinity) of
		{'EXIT', What} ->
		    orber:dbg("[~p] corba:request_from_iiop(~p, ~p, ~p); gen_server:call exit: ~p", [?LINE, Func, Args, Types, What], ?DEBUG_LEVEL),
		    {'EXCEPTION', #'TRANSIENT'{minor=(?ORBER_VMCID bor 4),
					       completion_status=?COMPLETED_MAYBE}};
		Result ->
		    Result
	    end
    end;
request_from_iiop({Mod, Type, Key, UserDef, OrberDef, Flags} = ObjRef, 
		  Func, Args, Types, _, ServiceCtx) ->
    case check_version(Mod:module_info(attributes)) of
	true ->
	    case catch gen_server:cast(convert_key_to_pid(Key), 
				       {ObjRef, ServiceCtx, Func, Args}) of
		{'EXIT', What} ->
		    orber:dbg("[~p] corba:request_from_iiop(~p, ~p, ~p); gen_server:cast exit: ~p", [?LINE, Func, Args, Types, What], ?DEBUG_LEVEL),
		    {'EXCEPTION', #'TRANSIENT'{minor=(?ORBER_VMCID bor 4),
					       completion_status=?COMPLETED_MAYBE}};
	        Result ->
	            Result
            end;
	false ->
	    case catch gen_server:cast(convert_key_to_pid(Key), 
				       {ObjRef, Func, Args}) of
		{'EXIT', What} ->
		    orber:dbg("[~p] corba:request_from_iiop(~p, ~p, ~p); gen_server:cast exit: ~p", [?LINE, Func, Args, Types, What], ?DEBUG_LEVEL),
		    {'EXCEPTION', #'TRANSIENT'{minor=(?ORBER_VMCID bor 4),
					       completion_status=?COMPLETED_MAYBE}};
	        Result ->
	            Result
            end
    end.

check_version([]) ->
    false;
check_version([{ic_compiled,"4_2"}|_])  ->
    true;
%% The ic_compiled attribute was introduced in 4.1.3.
check_version([{ic_compiled,[$4,$_,$1|_]}|_])  ->
    false;
check_version([{ic_compiled,_}|_])  ->
    true;
check_version([_|Rest])  ->
    check_version(Rest).

%%------------------------------------------------------------
%% Internal stuff
%%------------------------------------------------------------

convert_key_to_pid(Key) when binary(Key) ->
    orber_objectkeys:get_pid(Key);
convert_key_to_pid(Name) when atom(Name) ->
    Name.

mk_objkey(Mod, Pid, RegName, Persistent) ->
    mk_objkey(Mod, Pid, RegName, Persistent, 0).

mk_objkey(Mod, Pid, [], _, Flags) when pid(Pid) ->
    Key = make_objkey(),
    case orber_objectkeys:register(Key, Pid, false) of
	ok ->
	    {Mod, 'key', Key, term_to_binary(undefined), 0, Flags};
	R ->
	    orber:dbg("[~p] corba:mk_objkey(~p);
unable to store key(~p).", [?LINE, Mod, R], ?DEBUG_LEVEL),
	    raise(#'INTERNAL'{minor=(?ORBER_VMCID bor 2), completion_status=?COMPLETED_NO})
    end;
mk_objkey(Mod, Pid, {'global', RegName}, Persitent, Flags) when pid(Pid) ->
    Key = term_to_binary(RegName),
    case orber_objectkeys:register(Key, Pid, Persitent) of
	ok ->
	    {Mod, 'key', Key, term_to_binary(undefined), 0, Flags};
	R ->
	    orber:dbg("[~p] corba:mk_objkey(~p, ~p);
unable to store key(~p).", [?LINE, Mod, RegName, R], ?DEBUG_LEVEL),
	    raise(#'INTERNAL'{minor=(?ORBER_VMCID bor 2), completion_status=?COMPLETED_NO})
    end;
mk_objkey(Mod, Pid, {'local', RegName}, Persistent, Flags) when pid(Pid), atom(RegName) ->
    register(RegName, Pid),
    Key = make_objkey(),
    case orber_objectkeys:register(Key, Pid, Persistent) of
	ok ->
	    {Mod, 'registered', RegName, term_to_binary(undefined), 0, Flags};
	R ->
	    orber:dbg("[~p] corba:mk_objkey(~p, ~p);
unable to store key(~p).", [?LINE, Mod, RegName, R], ?DEBUG_LEVEL),
	    raise(#'INTERNAL'{minor=(?ORBER_VMCID bor 2), completion_status=?COMPLETED_NO})
    end.


mk_light_objkey(Mod, RegName) ->
    {Mod, 'registered', RegName, term_to_binary(undefined), 0, 0}.

mk_pseudo_objkey(Mod, Module, Flags) ->
    {Mod, 'pseudo', Module, term_to_binary(undefined), 0, Flags}.

make_objkey() ->
    term_to_binary({now(), node()}).

objkey_to_string({Mod, 'registered', 'orber_init', UserDef, OrberDef, Flags}) ->
    "INIT";
objkey_to_string({Mod, Type, Key, UserDef, OrberDef, Flags}) ->
    orber:domain() ++ [ 7 | binary_to_list(term_to_binary({Mod, Type, Key, UserDef, OrberDef, Flags}))];
objkey_to_string(External_object_key) ->
    External_object_key.

string_to_objkey("INIT") ->
    {orber_initial_references, 'registered', 'orber_init', term_to_binary(undefined), 0, 0};
string_to_objkey(String) -> 
    case prefix(orber:domain(), String) of
	[7 | Rest] ->
	    binary_to_term(list_to_binary(Rest));
	_ ->
	    String
    end.
%% This function may only be used when we know it's a local reference (i.e. target
%% key in a request; IOR's passed as argument or reply doesn't qualify)!
string_to_objkey_local("INIT") ->
    {orber_initial_references, 'registered', 'orber_init', term_to_binary(undefined), 0, 0};
string_to_objkey_local(String) -> 
    case prefix(orber:domain(), String) of
	[7 | Rest] ->
	    binary_to_term(list_to_binary(Rest));
	_ ->
	    case resolve_initial_references(String) of
		?ORBER_NIL_OBJREF ->
		    orber:dbg("[~p] corba:string_to_objkey_local(~p);
Invalid ObjektKey.", [?LINE, String], ?DEBUG_LEVEL),
		    ?ORBER_NIL_OBJREF;
		Object ->
		    {location_forward, Object}
	    end
    end.

prefix([], L2) ->
    L2;
prefix([E |L1], [E | L2]) ->
    prefix(L1, L2);
prefix(_, _) ->
    false.



evaluate_options([], #options{object_flags = Flags, 
			      object_flags_set = FlagsSet} = Options) ->
    case ?ORB_FLAG_TEST(FlagsSet, ?ORB_TYPECHECK) of
	true ->
	    Options;
	false ->
	    case orber:typechecking() of
		true ->
		    Options#options{object_flags = (?ORB_TYPECHECK bor Flags)};
		false ->
		    Options
	    end
    end;
evaluate_options([{sup_child, false}|Rest], Options) ->
    evaluate_options(Rest, Options);
evaluate_options([{create_options, COpt}|Rest], Options) when list(COpt) ->
    evaluate_options(Rest, Options#options{create_options = COpt});
evaluate_options([{sup_child, true}|Rest], Options) ->
    evaluate_options(Rest, Options#options{sup_child = true});
evaluate_options([{persistent, false}|Rest], Options) ->
    evaluate_options(Rest, Options);
evaluate_options([{persistent, true}|Rest], Options) ->
    evaluate_options(Rest, Options#options{persistent = true});
evaluate_options([{pseudo, false}|Rest], Options) ->
    evaluate_options(Rest, Options);
evaluate_options([{pseudo, true}|Rest], Options) ->
    evaluate_options(Rest, Options#options{pseudo = true});
evaluate_options([{regname, []}|Rest], Options) ->
    evaluate_options(Rest, Options);
evaluate_options([{regname, Name}|Rest], Options) ->
    evaluate_options(Rest, Options#options{regname = Name});
evaluate_options([{local_typecheck, false}|Rest], 
		 #options{object_flags_set = FlagsSet} = Options) ->
    %% This option overrides a global setting.
    evaluate_options(Rest, Options#options{object_flags_set = 
					   (?ORB_TYPECHECK bor FlagsSet)});
evaluate_options([{local_typecheck, true}|Rest], 
		 #options{object_flags = Flags,
			  object_flags_set = FlagsSet} = Options) ->
    evaluate_options(Rest, Options#options{object_flags = (?ORB_TYPECHECK bor Flags),
					   object_flags_set = 
					   (?ORB_TYPECHECK bor FlagsSet)});
evaluate_options([{no_security, true}|Rest], 
		 #options{object_flags = Flags} = Options) ->
    %% We do not allow this option to be set globally.
    evaluate_options(Rest, Options#options{object_flags = (?ORB_NO_SECURITY bor Flags)});
evaluate_options([{no_security, false}|Rest], Options) ->
    %% We do not allow this option to be set globally.
    evaluate_options(Rest, Options);
evaluate_options([{Key, Value}|_], _) ->
    orber:dbg("[~p] corba:evaluate_options(~p, ~p); 
Option not recognized or illegal value. Allowed settings:
sup_child.......: boolean()
persistent......: boolean()
pseudo..........: boolean()
local_typecheck.: boolean()
regname.........: {local, atom()} | {global, term()}", [?LINE, Key, Value], ?DEBUG_LEVEL),
    raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

