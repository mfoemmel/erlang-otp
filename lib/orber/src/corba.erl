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
%% Author: Lars Thorsen, Peter Lundell
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
%%-export(['ORB_init'/2]).
%%-----------------------------------------------------------------
%% Standard interface CORBA::ORB
%%-----------------------------------------------------------------
-export([%create_list/2,
	 %create_operation_list/2,
	 %% get_default_context/1,
	 %% 'ORB_init'/2,
	 %% 'BOA_init/2,
	 resolve_initial_references/1,
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
	 raise/1]).


%%-----------------------------------------------------------------
%% Internal (inside orber implementation) exports
%%-----------------------------------------------------------------
-export([call/4, call/5,
	 cast/4, locate/1,
	 request_from_iiop/5,
	 common_create/5,
	 mk_objkey/4,
	 mk_light_objkey/2,
	 objkey_to_string/1,
	 string_to_objkey/1,
	 check_exception_type/1,
	 %%call_internal/2,
	 get_system_exception_typedef/1]).

%%-----------------------------------------------------------------
%% Internal definitions
%%-----------------------------------------------------------------
%% Defines possible configuration parameters a user can add when
%% creating new CORBA objects.
-define(OE_CREATE_DEF_OPT, [{sup_child, false},
			    {persistent, false},
			    {regname, []}]).


%%------------------------------------------------------------
%%
%% Implementation of CORBA CORBA::ORB interfaces
%%
%%------------------------------------------------------------

%%create_list(Count) ->
%%    corba_nvlist:create_list(Count).

%%create_operation_list(OpDef) ->
%%    corba_nvlist:create_operation_list(OpDef).

%%'ORB_init'(Args, Name) ->
%%    ok.

%%-----------------------------------------------------------------
%% Initial reference handling
%%-----------------------------------------------------------------
resolve_initial_references(ObjectId) ->   
    orber_initial_references:get(string_to_objkey("INIT"), ObjectId).

list_initial_services() ->  
    orber_initial_references:list(string_to_objkey("INIT")).

add_initial_service(ObjectId, ObjectRef) ->   
    orber_initial_references:add(string_to_objkey("INIT"), ObjectId, ObjectRef).

remove_initial_service(ObjectId) ->  
    orber_initial_references:remove(string_to_objkey("INIT"), ObjectId).

resolve_initial_references_remote(ObjectId, []) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO});
resolve_initial_references_remote(ObjectId, [RemoteModifier| Rest]) ->
    case lists:prefix("iiop://", RemoteModifier) of
       true ->
	    [_, Host, Port] = string:tokens(RemoteModifier, ":/"),
	    orber_iiop:request({normal, Host, list_to_integer(Port), "INIT"}, 'get',
			       [ObjectId],
			       {{'tk_objref', 12, "object"},
				[{'tk_string', 0}],
				[]}, 'true');
       false ->
	    resolve_initial_references_remote(ObjectId, Rest)
    end.

list_initial_services_remote([]) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO});
list_initial_services_remote([RemoteModifier| Rest]) ->
    case lists:prefix("iiop://", RemoteModifier) of
       true ->
	    [_, Host, Port] = string:tokens(RemoteModifier, ":/"),
	    orber_iiop:request({normal, Host, list_to_integer(Port), "INIT"}, 'list', [],
			       {{'tk_sequence', {'tk_string',0},0},
				[], []}, 'true');
	false -> 
	    list_initial_services_remote(Rest)
    end.



%%-----------------------------------------------------------------
%% Objectreference convertions
%%-----------------------------------------------------------------
object_to_string(Object) ->
    iop_ior:string_code(Object).

string_to_object(IORString) ->
    {ObjRef, R, L} = iop_ior:string_decode(IORString),
    ObjRef.


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
	_ ->
	    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})   
    end,
    {sup_child, SupChild} = get_option(sup_child, Options),
    case apply(Module, StartMethod, [Env]) of
	{ok, Pid} ->
	    case catch mk_objkey(TypeID, Pid, RegName, Persistent) of
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
			_ ->
			    Objkey
		    end
	    end;
	X ->
	    X
    end.

dispose(Obj) ->
    corba_boa:dispose(Obj).

create_nil_objref() ->
    ?ORBER_NIL_OBJREF.

create_subobject_key(Objkey, B) when binary(B) ->
    iop_ior:set_privfield(Objkey, B);
create_subobject_key(Objkey, T) ->
    create_subobject_key(Objkey, term_to_binary(T)).

get_subobject_key(Objkey) ->
    iop_ior:get_privfield(Objkey).

get_pid(Objkey) ->
    {Location, Key} = iop_ior:get_key(Objkey),
     if
	Location == 'internal' ->
	     orber_objectkeys:get_pid(Key);
	Location == 'internal_registered' -> 
	     case whereis(Key) of
		 undefined ->
		     corba:raise(#'OBJECT_NOT_EXIST'{minor=100, completion_status=?COMPLETED_NO});
		 Pid ->
		     Pid
	     end;
	Location == 'external' -> 
	     corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO})
    end.

raise(E) ->
    throw({'EXCEPTION', E}).

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
	    orber_iiop:request(Key, Func, Args, Types, 'true')
    end.

call_internal(Pid, {Obj, Func, Args, Types}, Timeout) when pid(Pid) ->
    case gen_server:call(Pid, {Obj, Func, Args}, Timeout) of
	{'EXCEPTION', E} ->
	    corba:raise(E);
	Res ->
	    Res
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
	    case gen_server:call(P, {Obj, Func, Args}, Timeout) of
		{'EXCEPTION', E} ->
		    corba:raise(E);
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
	    orber_iiop:request(Key, Func, Args, Types, 'false')
    end.

cast_internal(Pid, {Obj, Func, Args, Types}) when pid(Pid) ->
    gen_server:cast(Pid, {Obj, Func, Args});
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
	

%%-----------------------------------------------------------------
%% Corba:locate - this function is for the moment just used for tests
%%-----------------------------------------------------------------
locate(Obj) ->
    {Location, Key} = iop_ior:get_key(Obj),
    if
	Location == 'internal' ->
	    orber_objectkeys:check(Key);
	Location == 'internal_registered' -> 	
	    orber_objectkeys:check(Key);
	Location == 'external' -> 
	    orber_iiop:locate(Key)
    end.


%%-----------------------------------------------------------------
%% Incomming request from iiop
%%-----------------------------------------------------------------
%% Operations which do not allow object invokation.
request_from_iiop(Obj, '_is_a', [Args], _Types, _ResponseExpected) ->
    catch corba_object:is_a(Obj, Args);
request_from_iiop(Obj, '_not_existent', _Args, _Types, _ResponseExpected) ->
    catch corba_object:non_existent(Obj);
request_from_iiop(Obj, 'get_policy', [Arg], _, _) ->
    catch corba_object:get_policy(Obj, Arg);

%% "Ordinary" operations.
%% Remove next case when we no longer wish to handle ObjRef/4 (only ObjRef/6).
request_from_iiop({Id, Type, Key, UserDef}, Func, Args, Types, ResponseExpected) ->
    Undef = term_to_binary(undefined),
    request_from_iiop({Id, Type, Key, UserDef, Undef, Undef}, Func, Args, Types, 
		      ResponseExpected);
request_from_iiop({Id, Type, Key, UserDef, OrberDef, Flags}, Func, Args, Types,
		  ResponseExpected) ->
    case ResponseExpected of
	'true' ->
	    gen_server:call(convert_key_to_pid(Key),
			    {{Id, Type, Key, UserDef, OrberDef, Flags}, Func, Args},
			    infinity);
	'false' ->
	    gen_server:cast(convert_key_to_pid(Key),
			    {{Id, Type, Key, UserDef, OrberDef, Flags}, Func, Args})
    end.

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
	    Undef = term_to_binary(undefined),
	    {list_to_binary(Id), 'key', Key, Undef, Undef, Undef};
	_ ->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end;
mk_objkey(Id, Pid, {'global', RegName}, Persitent) when pid(Pid) ->
    Key = term_to_binary(RegName),
    case orber_objectkeys:register(Key, Pid, Persitent) of
	ok ->
	    Undef = term_to_binary(undefined),
	    {list_to_binary(Id), 'key', Key, Undef, Undef, Undef};
	_ ->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end;
mk_objkey(Id, Pid, {'local', RegName}, Persistent) when pid(Pid), atom(RegName) ->
    register(RegName, Pid),
    Key = make_objkey(),
    case orber_objectkeys:register(Key, Pid, Persistent) of
	ok ->
	    Undef = term_to_binary(undefined),
	    {list_to_binary(Id), 'registered', RegName, Undef, Undef, Undef};
	R ->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.


mk_light_objkey(Id, RegName) ->
    Undef = term_to_binary(undefined),
    {list_to_binary(Id), 'registered', RegName, Undef, Undef, Undef}.

make_objkey() ->
    term_to_binary({now(), node()}).

%% Remove next case when we no longer wish to handle ObjRef/4 (only ObjRef/6).
objkey_to_string({Id, 'key', Key, UserDef}) ->
    Undef = term_to_binary(undefined),
    objkey_to_string({Id, 'key', Key, UserDef, Undef, Undef});
objkey_to_string({Id, 'key', Key, UserDef, OrberDef, Flags}) ->
    orber:domain() ++ ":" ++ binary_to_list(Id) ++ ":key:" ++
	binary_to_list(Key) ++ ":" ++ binary_to_list(UserDef) ++ ":" ++ 
	binary_to_list(OrberDef) ++ ":" ++ binary_to_list(Flags);
%% Remove next case when we no longer wish to handle ObjRef/4 (only ObjRef/6).
objkey_to_string({Id, 'registered', 'orber_init', UserDef}) ->
    "INIT";
objkey_to_string({Id, 'registered', 'orber_init', UserDef, OrberDef, Flags}) ->
    "INIT";
%% Remove next case when we no longer wish to handle ObjRef/4 (only ObjRef/6).
objkey_to_string({Id, 'registered', Atom, UserDef}) ->
    Undef = term_to_binary(undefined),
    objkey_to_string({Id, 'registered', Atom, UserDef, Undef, Undef});
objkey_to_string({Id, 'registered', Atom, UserDef, OrberDef, Flags}) ->
    orber:domain() ++ ":" ++ binary_to_list(Id) ++ ":registered:" ++
	atom_to_list(Atom) ++ ":" ++ binary_to_list(UserDef) ++ ":" ++ 
	binary_to_list(OrberDef) ++ ":" ++ binary_to_list(Flags); 
objkey_to_string(External_object_key) ->
    External_object_key.

string_to_objkey("INIT") ->
    Undef = term_to_binary(undefined),
    {list_to_binary("IDL:CORBA/InitialReferences:1.0"), 'registered', 
     'orber_init', Undef, Undef, Undef};
string_to_objkey(String) -> 
    case prefix(orber:domain(), String) of
	false ->
	    String;
	Rest ->
	    string_to_objkey_2(Rest)

    end.

prefix([], L2) ->
    L2;
prefix([E |L1], [E | L2]) ->
    prefix(L1, L2);
prefix(_, _) ->
    false.

string_to_objkey_2([$:| Rest]) ->
    {I, R1} = read_to_colon(Rest),
    {P, R2} = read_to_colon(R1),
    {V, R3} = read_to_colon(R2),
    {T, R4} = read_to_colon(R3),
    %% Next case replaced by the following cases. Remove later.
    %% {O, U} = read_to_colon(R4),
    {O, R5} = read_to_colon(R4),
    {U, R6} = read_to_colon(R5),
    {K, Fl} = read_to_colon(R6),
    Type = list_to_atom(T),
    Object = case Type of
	'key' ->
	    list_to_binary(O);
	'registered' -> 
	    list_to_atom(O)
    end,
    case K of
%% Remove next case when we no longer wish to handle ObjRef/4 (only ObjRef/6).
	[] ->
	    {list_to_binary(I ++ ":" ++ P ++ ":" ++ V), Type, Object, 
	     list_to_binary(U)};
	_ ->
	    {list_to_binary(I ++ ":" ++ P ++ ":" ++ V), Type, Object, 
	     list_to_binary(U), list_to_binary(K), list_to_binary(Fl)}
    end.

%% Remove next case when we no longer wish to handle ObjRef/4 (only ObjRef/6).
read_to_colon([]) ->
    {[], []};
read_to_colon([$: |R]) ->
    {[], R};
read_to_colon([C |R]) ->
    {Acc, R2} = read_to_colon(R),
    {[C|Acc], R2}.
    
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
check_exception_type('BAD_QOS') ->
    ?SYSTEM_EXCEPTION;
check_exception_type(_) ->
    ?USER_EXCEPTION.


%%-----------------------------------------------------------------
%% Generate typedef of system exception
%%-----------------------------------------------------------------
get_system_exception_typedef(ExcName) ->
    Name = atom_to_list(ExcName),
    {'tk_except', "IDL:CORBA/" ++ Name ++ ":1.0", Name,
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

