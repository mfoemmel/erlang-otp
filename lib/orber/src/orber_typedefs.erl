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
%%-----------------------------------------------------------------
%% File: orber_typedefs.erl
%% Author: Lars Thorsen
%% 
%% Description:
%%    This file contains some functions for internal typedef checking
%%
%% Creation date: 9700307
%%
%%-----------------------------------------------------------------
-module(orber_typedefs).

-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([get_op_def/2, get_Op_def/2, get_exception_def/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: get_op_def/2
%%
get_op_def(_Objkey, "_is_a") ->
    {orber_tc:boolean(),[orber_tc:string(0)],[]};
get_op_def(_Objkey, "_not_existent") ->
    {orber_tc:boolean(),[],[]};
get_op_def(_Objkey, "get_policy") ->
    {orber_policy_server:get_tc(),[orber_tc:unsigned_long()],[]};
get_op_def(Objkey, Op) ->
    List = corba:request_from_iiop(Objkey, oe_get_interface,
					 [], [], 'true'),
    get_op_def_1(List, Op).

get_op_def_1([], Op) ->
    exit(#'BAD_OPERATION'{completion_status='COMPLETED_NO'}); 
get_op_def_1([{Op, Types} | Rest], Op) ->
    Types;
get_op_def_1([_ | Rest], Op) ->
    get_op_def_1(Rest, Op).

%%-----------------------------------------------------------------
%% Func: get_exception_def/1
%%
get_exception_def(Exception) ->
    [Exc, TypeId | _] = tuple_to_list(Exception),
    case corba:check_exception_type(Exc) of
	?SYSTEM_EXCEPTION ->
	    {?SYSTEM_EXCEPTION, corba:get_system_exception_typedef(Exc)};
	?USER_EXCEPTION ->
	    Rep = orber_ifr:find_repository(),
	    ExceptionDef = orber_ifr:'Repository_lookup_id'(Rep, TypeId),
	    ContainedDescr = orber_ifr_exceptiondef:describe(ExceptionDef),
	    %% Borde det kollas om den här operationen kan ge ifrån sig
	    %% det här exceptionet ?
	    ExceptionDescr = ContainedDescr#contained_description.value,
	    {?USER_EXCEPTION, ExceptionDescr#exceptiondescription.type}
    end.

%%-----------------------------------------------------------------
%% Func: get_Op_def/2
%%
get_Op_def({TypeId, _, _}, Op) ->
    Rep = orber_ifr:find_repository(),
    %%io:format("Time before lookup: ~w~n", [erlang:now()]),
    Int = orber_ifr:'Repository_lookup_id'(Rep, TypeId),
    %%io:format("Time before descr inteface: ~w~n", [erlang:now()]),
    InterfaceDef = orber_ifr_interfacedef:describe_interface(Int),
    %%io:format("Time after: ~w~n", [erlang:now()]),
    Ops = InterfaceDef#fullinterfacedescription.operations,
    A = get_Op_def_1(Ops, Op),
    %%io:format("Time after all: ~w~n", [erlang:now()]),
    A.

get_Op_def_1([], _) ->
    exit(#'BAD_OPERATION'{completion_status='COMPLETED_NO'}); 
get_Op_def_1([#contained_description{kind=dk_Operation, value=#operationdescription{name=Op, result=Result, parameters=Parameters}}|_], Op) ->
    {In, Out} = get_parameters(Parameters,{[],[]}),
    {Result, lists:reverse(In), lists:reverse(Out)};
get_Op_def_1([_|Rest], Op) ->
    get_Op_def_1(Rest, Op).

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
get_parameters([], Par) ->
    Par;
get_parameters([#parameterdescription{type=Type, mode='PARAM_IN'} | Rest], {In, Out}) ->
    get_parameters(Rest, {[Type | In], Out});
get_parameters([#parameterdescription{type=Type, mode='PARAM_OUT'} | Rest], {In, Out}) ->
    get_parameters(Rest, {In, [Type |Out]});
get_parameters([#parameterdescription{type=Type, mode='PARAM_INOUT'} | Rest], {In, Out}) ->
    get_parameters(Rest, {[Type | In],[Type |Out]}).


