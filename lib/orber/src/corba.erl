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
	 cast/4, locate/1, locate/2,
	 request_from_iiop/5,
	 common_create/5,
	 mk_objkey/4,
	 mk_light_objkey/2,
	 objkey_to_string/1,
	 string_to_objkey/1,
	 string_to_objkey_local/1,
	 check_exception_type/1,
	 %%call_internal/2,
	 get_system_exception_typedef/1,
	 call_relay/3,
	 cast_relay/2]).

%%-----------------------------------------------------------------
%% Internal definitions
%%-----------------------------------------------------------------
%% Defines possible configuration parameters a user can add when
%% creating new CORBA objects.
-define(OE_CREATE_DEF_OPT, [{sup_child, false},
			    {persistent, false},
			    {regname, []},
			    {pseudo, false}]).

-define(DEBUG_LEVEL, 5).


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
    corba:raise(#'BAD_PARAM'{completion_status = ?COMPLETED_NO}).


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
    corba:raise(#'BAD_PARAM'{completion_status = ?COMPLETED_NO}).


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
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO});
resolve_initial_references_remote(ObjectId, [RemoteModifier| Rest]) 
  when list(RemoteModifier) ->
    case lists:prefix("iiop://", RemoteModifier) of
       true ->
	    [_, Host, Port] = string:tokens(RemoteModifier, ":/"),
	    IOR = iop_ior:create_external(orber:giop_version(), "", 
				 Host, list_to_integer(Port), "INIT"),
	    %% We know it's an external referens. Hence, nmo need to check.
	    {Location, Key} = iop_ior:get_key(IOR),
	    orber_iiop:request(Key, 'get', [ObjectId], 
			       {{'tk_objref', 12, "object"},
				[{'tk_string', 0}],
				[]}, 'true', infinity, IOR);
       false ->
	    resolve_initial_references_remote(ObjectId, Rest)
    end;
resolve_initial_references_remote(ObjectId, []) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

list_initial_services_remote([]) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO});
list_initial_services_remote([RemoteModifier| Rest]) when list(RemoteModifier) ->
    case lists:prefix("iiop://", RemoteModifier) of
       true ->
	    [_, Host, Port] = string:tokens(RemoteModifier, ":/"),
	    IOR = iop_ior:create_external(orber:giop_version(), "", 
				 Host, list_to_integer(Port), "INIT"),
	    %% We know it's an external referens. Hence, nmo need to check.
	    {Location, Key} = iop_ior:get_key(IOR),
	    orber_iiop:request(Key, 'list', [],
			       {{'tk_sequence', {'tk_string',0},0},
				[], []}, 'true', infinity, IOR);
	false -> 
	    list_initial_services_remote(Rest)
    end;
list_initial_services_remote(_) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).



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
	    corba:raise(#'OBJ_ADAPTER'{completion_status=?COMPLETED_NO})
    end.

node_check(Node) ->
    lists:member(Node,orber:orber_nodes()).

common_create(Module, TypeID, Env, Options, StartMethod) when list(Options) ->
    {regname, RegName}= get_option(regname, Options),
    {persistent, Persistent} = get_option(persistent, Options),
    case RegName of
	[] ->
	    ok;
	{'local', Atom} when atom(Atom), Persistent == false ->
	    ok;
	{'global', _} ->
	    ok;
	Why ->
	    orber:dbg("[~p] corba:common_create(~p, ~p); 
bad name type or combination(~p).", [?LINE, Module, Options, Why], ?DEBUG_LEVEL),
	    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})   
    end,
    {pseudo, Pseudo} = get_option(pseudo, Options),
    case Pseudo of
	false ->
	    {sup_child, SupChild} = get_option(sup_child, Options),
	    case apply(Module, StartMethod, [Env]) of
		{ok, Pid} ->
		    case catch mk_objkey(Module, Pid, RegName, Persistent) of
			{'EXCEPTION', E} ->
			    %% This branch is only used if we couldn't register 
			    %% our new objectkey due to an internal error in orber.
			    gen_server:call(Pid, stop),
			    corba:raise(E);
			{'EXIT', R} ->
			    %% This branch takes care of exit values
			    %% which aren't expected (due to bug).
			    gen_server:call(Pid, stop),
			    exit(R);
			Objkey ->
			    %% The normal case.
			    case SupChild of
				true ->
				    {ok, Pid, Objkey};
				false ->
				    Objkey
			    end
		    end;
		X ->
		    X
	    end;
	true ->
	    ModuleImpl = list_to_atom(lists:concat([Module, '_impl'])),
	    case apply(ModuleImpl, init, [Env]) of
		{ok, State} ->
		    create_subobject_key(mk_pseudo_objkey(Module, ModuleImpl),State);
		{ok, State,_} ->
		    create_subobject_key(mk_pseudo_objkey(Module, ModuleImpl),State);
		Reason ->
		    orber:dbg("[~p] corba:common_create(~p);
'init' function incorrect(~p).", [?LINE, ModuleImpl, Reason], ?DEBUG_LEVEL),
		    corba:raise(#'INTERNAL'{minor=1100, completion_status=?COMPLETED_NO})
	    end;
	What ->
	    orber:dbg("[~p] corba:common_create(~p, ~p); 
not a boolean(~p).", [?LINE, Module, Options, What], ?DEBUG_LEVEL),
	    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})   
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
    {Location, Key} = iop_ior:get_key(Objkey),
     case Location of
	 'internal' ->
	     orber_objectkeys:get_pid(Key);
	 'internal_registered' when atom(Key) -> 
	     case whereis(Key) of
		 undefined ->
		     corba:raise(#'OBJECT_NOT_EXIST'{minor=100, completion_status=?COMPLETED_NO});
		 Pid ->
		     Pid
	     end;
	 R ->
	     orber:dbg("[~p] corba:get_pid(~p); 
Probably a pseudo- or external object(~p).", [?LINE, Objkey, R], ?DEBUG_LEVEL),
	     corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO})
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
%% Corba:call - the function for requests
%%-----------------------------------------------------------------
call(Obj, Func, Args, Types) ->
    call(Obj, Func, Args, Types, infinity).

call(Obj, Func, Args, Types, Timeout) ->
    {Location, Key} = iop_ior:get_key(Obj),
    if
	Location == 'internal' ->
	    Pid = orber_objectkeys:get_pid(Key),
	    call_internal(Pid, {Obj, Func, Args, Types}, Timeout);
	Location == 'internal_registered' -> 
	    call_internal(Key, {Obj, Func, Args, Types}, Timeout);
	Location == 'external' -> 
	    orber_iiop:request(Key, Func, Args, Types, 'true', Timeout, Obj)
    end.

call_internal(Pid, {Obj, Func, Args, Types}, Timeout) when pid(Pid), 
							   node(Pid) == node() ->
    case catch gen_server:call(Pid, {Obj, Func, Args}, Timeout) of
	{'EXCEPTION', E} ->
	    corba:raise(E);
	{'EXIT',{timeout, _}} ->
	    corba:raise(#'COMM_FAILURE'{minor=108, completion_status=?COMPLETED_MAYBE});
	{'EXIT',R} ->
	    orber:dbg("[~p] corba:call_internal(~p, ~p, ~p); 
call exit(~p).", [?LINE, Func, Args, Types, R], ?DEBUG_LEVEL),
	    exit(R);
	Res ->
	    Res
    end;
call_internal(Pid, {Obj, Func, Args, Types}, Timeout) when pid(Pid) ->
    case catch rpc:call(node(Pid), corba, call_relay, 
			[Pid, {Obj, Func, Args}, Timeout]) of
	{'EXCEPTION', E} ->
	    corba:raise(E);
	{badrpc, {'EXIT', {undef, _}}} ->
	    %% Must be an older version; try the old-way.
	    case catch gen_server:call(Pid, {Obj, Func, Args}, Timeout) of
		{'EXCEPTION', E} ->
		    corba:raise(E);
		{'EXIT',{timeout, _}} ->
		    corba:raise(#'COMM_FAILURE'{minor=108, completion_status=?COMPLETED_MAYBE});
		{'EXIT',R} ->
		    orber:dbg("[~p] corba:call_internal(~p, ~p, ~p); 
call exit(~p).", [?LINE, Func, Args, Types, R], ?DEBUG_LEVEL),
		    exit(R);
		Res ->
		    Res
	    end;
	{badrpc, {'EXIT',R}} ->
	    orber:dbg("[~p] corba:call_internal(~p, ~p, ~p); 
call exit(~p).", [?LINE, Func, Args, Types, R], ?DEBUG_LEVEL),
	    exit(R);
	{badrpc,nodedown} ->
	    orber:dbg("[~p] corba:call_internal(~p, ~p, ~p); 
Node ~p down.", [?LINE, Func, Args, Types, node(Pid)], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{minor=108, completion_status=?COMPLETED_MAYBE});
	Res ->
	    Res
    end;

%% This case handles if the reference is created as a Pseudo object. Just call apply/3.
call_internal({pseudo, Module}, {Obj, Func, Args, Types}, Timeout) ->
    State = binary_to_term(get_subobject_key(Obj)),
    case catch apply(Module, Func, [Obj, State|Args]) of
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
	    corba:raise(E);
	{'EXIT', What} ->
	    orber:dbg("[~p] corba:call_internal(~p, ~p, ~p); 
Pseudo object exit(~p).", [?LINE, Func, Args, Types, What], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_MAYBE});
	Unknown ->
	    orber:dbg("[~p] corba:call_internal(~p, ~p, ~p); 
Pseudo object failed due to bad return value (~p).", 
				    [?LINE, Func, Args, Types, Unknown], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_MAYBE})
    end;
call_internal(Registered, {Obj, Func, Args, Types}, Timeout)  when atom(Registered)->
    case whereis(Registered) of
	undefined ->
	    corba:raise(#'OBJECT_NOT_EXIST'{minor=100, completion_status=?COMPLETED_NO});
%% To be used when we allow objects which are {'local', RegName} to be persistent.
%	    case orber_objectkeys:is_persistent(Registered) of
%		true ->
%		    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO});
%		_->
%		    corba:raise(#'OBJECT_NOT_EXIST'{completion_status=?COMPLETED_NO})
%	    end;
	P ->
	    case catch gen_server:call(P, {Obj, Func, Args}, Timeout) of
		{'EXCEPTION', E} ->
		    corba:raise(E);
		{'EXIT',{timeout, _}} ->
		    corba:raise(#'COMM_FAILURE'{minor=109, completion_status=?COMPLETED_MAYBE});	
		{'EXIT',R} ->
		    orber:dbg("[~p] corba:call_internal(~p, ~p, ~p); 
call exit(~p).", [?LINE, Func, Args, Types, R], ?DEBUG_LEVEL),
		    exit(R);
		Res ->
		    Res
	    end
    end.


call_relay(Pid, {Obj, Func, Args}, Timeout) ->
    case whereis(orber_objkeyserver) of
	undefined ->
	    corba:raise(#'COMM_FAILURE'{minor=110, completion_status=?COMPLETED_MAYBE});
	_ ->
	    case catch gen_server:call(Pid, {Obj, Func, Args}, Timeout) of
		{'EXCEPTION', E} ->
		    corba:raise(E);
		{'EXIT',{timeout, _}} ->
		    corba:raise(#'COMM_FAILURE'{minor=108, completion_status=?COMPLETED_MAYBE});
		{'EXIT',R} ->
		    orber:dbg("[~p] corba:call_internal(~p, ~p); 
call exit(~p).", [?LINE, Func, Args, R], ?DEBUG_LEVEL),
		    exit(R);
		Res ->
		    Res
	    end
    end.
	
%%-----------------------------------------------------------------
%% Corba:cast - the function for ONEWAY requests
%%-----------------------------------------------------------------
cast(Obj, Func, Args, Types) ->
    {Location, Key} = iop_ior:get_key(Obj),
    if
	Location == 'internal' ->
	    Pid = orber_objectkeys:get_pid(Key),
	    cast_internal(Pid, {Obj, Func, Args, Types});
	Location == 'internal_registered' -> 	
	    cast_internal(Key, {Obj, Func, Args, Types});
	Location == 'external' -> 
	    orber_iiop:request(Key, Func, Args, Types, 'false', infinity, Obj)
    end.

cast_internal(Pid, {Obj, Func, Args, Types}) when pid(Pid), node(Pid) == node() ->
    gen_server:cast(Pid, {Obj, Func, Args});
cast_internal(Pid, {Obj, Func, Args, Types}) when pid(Pid) ->
    case catch rpc:call(node(Pid), corba, call_relay, [Pid, {Obj, Func, Args}]) of
	{'EXCEPTION', E} ->
	    corba:raise(E);
	{badrpc, {'EXIT', {undef, _}}} ->
	    %% Must be an older version; try the old-way.
	    gen_server:cast(Pid, {Obj, Func, Args});
	{badrpc, {'EXIT', R}} ->
	    exit(R);
	{badrpc,nodedown} ->
	    orber:dbg("[~p] corba:cast_internal(~p, ~p, ~p); 
Node ~p down.", [?LINE, Func, Args, Types, node(Pid)], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{minor=108, completion_status=?COMPLETED_MAYBE});
	Other ->
	    orber:dbg("[~p] corba:cast_internal(~p, ~p, ~p);
Communication with node: ~p failed with reason: ~p.", 
				    [?LINE, Func, Args, Types, node(Pid), Other], ?DEBUG_LEVEL),
	    exit(Other)
    end;

%% This case handles if the reference is created as a Pseudo object. Just call apply/3.
cast_internal({pseudo, Module}, {Obj, Func, Args, Types}) ->
    State = binary_to_term(get_subobject_key(Obj)),
    catch apply(Module, Func, [Obj, State|Args]),
    ok;
cast_internal(Registered, {Obj, Func, Args, Types}) ->
    case whereis(Registered) of
	undefined ->
	    corba:raise(#'OBJECT_NOT_EXIST'{minor=100, completion_status=?COMPLETED_NO});
%% To be used when we allow objects which are {'local', RegName} to be persistent.
%	    case orber_objectkeys:is_persistent(Registered) of
%		true ->
%		    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO});
%		_->
%		    corba:raise(#'OBJECT_NOT_EXIST'{completion_status=?COMPLETED_NO})
%	    end;
	P ->
	    gen_server:cast(P, {Obj, Func, Args})
    end.
	
cast_relay(Pid, {Obj, Func, Args}) ->
    case whereis(orber_objkeyserver) of
	undefined ->
	    corba:raise(#'COMM_FAILURE'{minor=110, completion_status=?COMPLETED_MAYBE});
	_ ->
	    gen_server:cast(Pid, {Obj, Func, Args})
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
request_from_iiop(Obj, '_is_a', [Args], _Types, _ResponseExpected) ->
    catch corba_object:is_a(Obj, Args);
%% First the OMG specified this operation to be '_not_existent' and then
%% changed it to '_non_existent' without suggesting that both must be supported.
%% See CORBA2.3.1 page 15-34, Minor revision 2.3.1: October 1999
request_from_iiop(Obj, '_not_existent', _Args, _Types, _ResponseExpected) ->
    catch corba_object:non_existent(Obj);
request_from_iiop(Obj, '_non_existent', _Args, _Types, _ResponseExpected) ->
    catch corba_object:non_existent(Obj);
request_from_iiop(Obj, 'get_policy', [Arg], _, _) ->
    catch corba_object:get_policy(Obj, Arg);

%% "Ordinary" operations.
request_from_iiop({Id, ObjType, Module, UserDef, OrberDef, Flags}, oe_get_interface, 
		  Args, Types, _ResponseExpected) when atom(Id) ->
    case Id:handle_call({false, oe_get_interface, []}, false, false) of
	{reply, OpDef, _} ->
	    OpDef;
	_->
	    []
    end;
%% This case handles if the reference is created as a Pseudo object. Just call apply/3.
request_from_iiop({Id, pseudo, Module, UserDef, OrberDef, Flags}, oe_get_interface, 
		  Args, Types, _ResponseExpected) ->
    M=remove_impl(lists:reverse(atom_to_list(Module))),
    case M:handle_call({false, oe_get_interface, []}, false, false) of
	{reply, OpDef, _} ->
	    OpDef;
	_->
	    []
    end;
request_from_iiop({Id, pseudo, Module, UserDef, OrberDef, Flags}, Func, Args, 
		  Types, ResponseExpected) ->
    State = binary_to_term(get_subobject_key({Id, pseudo, Module, UserDef, OrberDef, 
					      Flags})),
    case ResponseExpected of
	true ->
	    case catch apply(Module, Func, [{Id, pseudo, Module, UserDef, OrberDef, 
					     Flags}, State|Args]) of
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
		{'EXIT', What} ->
		    orber:dbg("[~p] corba:request_from_iiop(~p, ~p, ~p); 
Pseudo object exit(~p). The call-back module probably contain an error.", 
					    [?LINE, Func, Args, Types, What], ?DEBUG_LEVEL),
		    {'EXCEPTION', #'COMM_FAILURE'{completion_status=?COMPLETED_MAYBE}};
		Unknown ->
		    orber:dbg("[~p] corba:request_from_iiop(~p, ~p, ~p); 
Pseudo object failed(~p); confirm that the return value is correct (e.g. {reply, Reply, State})", 
					    [?LINE, Func, Args, Types, Unknown], ?DEBUG_LEVEL),
		    {'EXCEPTION', #'COMM_FAILURE'{completion_status=?COMPLETED_MAYBE}}
	    end;
	false ->
	    catch apply(Module, Func, [{Id, pseudo, Module, UserDef, OrberDef, 
					     Flags}, State|Args]),
	    ok;
	true_oneway ->
	    catch apply(Module, Func, [{Id, pseudo, Module, UserDef, OrberDef, 
					     Flags}, State|Args]),
	    ok
    end;

request_from_iiop({Id, Type, Key, UserDef, OrberDef, Flags}, Func, Args, Types,
		  ResponseExpected) ->
    case ResponseExpected of
	'true' ->
	    gen_server:call(convert_key_to_pid(Key),
			    {{Id, Type, Key, UserDef, OrberDef, Flags}, Func, Args},
			    infinity);
	'false' ->
	    gen_server:cast(convert_key_to_pid(Key),
			    {{Id, Type, Key, UserDef, OrberDef, Flags}, Func, Args});
	'true_oneway' ->
	    gen_server:cast(convert_key_to_pid(Key),
			    {{Id, Type, Key, UserDef, OrberDef, Flags}, Func, Args})
    end.

remove_impl([$l,$p,$m,$i,$_|Rest]) -> list_to_atom(lists:reverse(Rest)).
    

%%------------------------------------------------------------
%% Internal stuff
%%------------------------------------------------------------

convert_key_to_pid(Key) when binary(Key) ->
    orber_objectkeys:get_pid(Key);
convert_key_to_pid(Name) when atom(Name) ->
    Name.

mk_objkey(Id, Pid, [], _) when pid(Pid) ->
    Key = make_objkey(),
    case orber_objectkeys:register(Key, Pid, false) of
	ok ->
	    {Id, 'key', Key, term_to_binary(undefined), 0, 0};
	R ->
	    orber:dbg("[~p] corba:mk_objkey(~p);
unable to store key(~p).", [?LINE, Id, R], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{minor=1101, completion_status=?COMPLETED_NO})
    end;
mk_objkey(Id, Pid, {'global', RegName}, Persitent) when pid(Pid) ->
    Key = term_to_binary(RegName),
    case orber_objectkeys:register(Key, Pid, Persitent) of
	ok ->
	    {Id, 'key', Key, term_to_binary(undefined), 0, 0};
	R ->
	    orber:dbg("[~p] corba:mk_objkey(~p, ~p);
unable to store key(~p).", [?LINE, Id, RegName, R], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{minor=1102, completion_status=?COMPLETED_NO})
    end;
mk_objkey(Id, Pid, {'local', RegName}, Persistent) when pid(Pid), atom(RegName) ->
    register(RegName, Pid),
    Key = make_objkey(),
    case orber_objectkeys:register(Key, Pid, Persistent) of
	ok ->
	    {Id, 'registered', RegName, term_to_binary(undefined), 0, 0};
	R ->
	    orber:dbg("[~p] corba:mk_objkey(~p, ~p);
unable to store key(~p).", [?LINE, Id, RegName, R], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{minor=1103, completion_status=?COMPLETED_NO})
    end.


mk_light_objkey(Id, RegName) ->
    {Id, 'registered', RegName, term_to_binary(undefined), 0, 0}.

mk_pseudo_objkey(Id, Module) ->
    {Id, 'pseudo', Module, term_to_binary(undefined), 0, 0}.

make_objkey() ->
    term_to_binary({now(), node()}).

objkey_to_string({Id, 'registered', 'orber_init', UserDef, OrberDef, Flags}) ->
    "INIT";
objkey_to_string({Id, Type, Key, UserDef, OrberDef, Flags}) ->
    orber:domain() ++ [ 7 | binary_to_list(term_to_binary({Id, Type, Key, UserDef, OrberDef, Flags}))];
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


%%-----------------------------------------------------------------
%% Check if CORBA system exception or user defined
%%-----------------------------------------------------------------
check_exception_type('UNKNOWN') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('BAD_PARAM') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('NO_MEMORY') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('IMP_LIMIT') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('COMM_FAILURE') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('INV_OBJREF') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('NO_PERMISSION') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('INTERNAL') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('MARSHAL') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('INITIALIZE') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('NO_IMPLEMENT') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('BAD_TYPECODE') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('BAD_OPERATION') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('NO_RESOURCES') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('NO_RESPONSE') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('PERSIST_STORE') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('BAD_INV_ORDER') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('TRANSIENT') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('FREE_MEM') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('INV_IDENT') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('INV_FLAG') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('INTF_REPOS') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('BAD_CONTEXT') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('OBJ_ADAPTER') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('DATA_CONVERSION') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('OBJECT_NOT_EXIST') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('TRANSACTION_REQUIRED') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('TRANSACTION_ROLLEDBACK') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('INVALID_TRANSACTION') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('INV_POLICY') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('CODESET_INCOMPATIBLE') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('REBIND') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('TIMEOUT') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('TRANSACTION_UNAVAILABLE') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('TRANSACTION_MODE') ->
    ?SYSTEM_EXCEPTION;
check_exception_type('BAD_QOS') ->
    ?SYSTEM_EXCEPTION;
check_exception_type(_) ->
    ?USER_EXCEPTION.


%%-----------------------------------------------------------------
%% Generate typedef of system exception
%%-----------------------------------------------------------------
get_system_exception_typedef(ExcName) ->
    Name = atom_to_list(ExcName),
    {'tk_except', "IDL:omg.org/CORBA/" ++ Name ++ ":1.0", Name,
     [
      {"minor",'tk_ulong'},
      {"completed",{'tk_enum', "", "completion_status",
	   ["COMPLETED_YES", "COMPLETED_NO",
	    "COMPLETED_MAYBE"]}}
     ]}.

get_option(Key, OptionList) ->
    case lists:keysearch(Key, 1, OptionList) of
	{value,{Key,Value}} ->
	    {Key,Value};
	_ ->
	    case lists:keysearch(Key, 1, ?OE_CREATE_DEF_OPT) of
		{value,{Key,Value}} ->
		    {Key,Value};
		_->
		    exit(io_lib:format("Option ~w not found in optionlist", 
				       [Key]))
	    end
    end.

