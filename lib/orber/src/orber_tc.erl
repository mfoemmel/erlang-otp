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
%% File: orber_tc.erl
%% Description:
%%    This file contains utility functions to create TypeCodes
%%
%% Creation date: 970407
%% Modified:
%%
%%-----------------------------------------------------------------
-module(orber_tc).

-include_lib("orber/src/orber_debug.hrl").
-include_lib("orber/include/ifr_types.hrl").
-include_lib("orber/include/corba.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([null/0, void/0, short/0, unsigned_short/0, 
	 long/0, unsigned_long/0, long_long/0,
	 unsigned_long_long/0, float/0, double/0,
	 boolean/0, char/0, octet/0, any/0,
	 typecode/0, principal/0,
	 object_reference/2, struct/3, 
	 union/5, enum/3,
	 string/1, sequence/2, array/2, alias/3,
	 exception/3, get_tc/1, check_tc/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% A number of function which can be used to create TypeCodes
null() ->
    tk_null.
void() ->
    tk_void.
short() ->
    tk_short.
unsigned_short() ->
    tk_ushort.
long() ->
    tk_long.
unsigned_long() ->
    tk_ulong.
long_long() ->
    tk_longlong.
unsigned_long_long() ->
    tk_ulonglong.
float() ->
    tk_float.
double() ->
    tk_double.
%%long_double() ->
%%    tk_longdouble.
boolean() ->
    tk_boolean.
char() ->
    tk_char.
octet() ->
    tk_octet.
any() ->
    tk_any.
typecode() ->
    tk_TypeCode.
principal() ->
    tk_principal.

object_reference(Id, Name) ->
    {tk_objref, Id, Name}.

struct(Id, Name, ElementList) ->
    {tk_struct, Id, Name, ElementList}.

union(Id, Name, DiscrTC, Default, ElementList) ->
    {tk_union, Id, Name, DiscrTC, Default, ElementList}.

enum(Id, Name, ElementList) ->
    {tk_enum, Id, Name, ElementList}.

string(Length) ->
    {tk_string, Length}.

sequence(ElemTC, Length) ->
    {tk_sequence, ElemTC, Length}.

array(ElemTC, Length) ->
    {tk_array, ElemTC, Length}.

alias(Id, Name, TC) ->
    {tk_alias, Id, Name, TC}.


exception(Id, Name, ElementList) ->
    {tk_exception, Id, Name, ElementList}.


%%-----------------------------------------------------------------
%% Get TypeCode (can be used for constructed types like structs, 
%% unions and exceptions)
%%
get_tc(T) when tuple(T) ->
    Type = element(1, T),
    case catch Type:tc() of
	{'EXIT', R} ->
	    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO});
	X ->
	    X
    end;
%% This call can be used if one have the IFR id and wants a typecode.
get_tc(IFRId) when list(IFRId) ->
    Rep = orber_ifr:find_repository(),
    Def = orber_ifr:lookup_id(Rep, IFRId),
    Descr = orber_ifr:describe(Def),
    TypeDescr = Descr#contained_description.value,
    TypeDescr#typedescription.type.
    

%%-----------------------------------------------------------------
%% Check TypeCode format
%%
check_tc('tk_null') -> true;
check_tc('tk_void') -> true;
check_tc('tk_short') -> true;
check_tc('tk_ushort') -> true;
check_tc('tk_long') -> true;
check_tc('tk_ulong') -> true;
check_tc('tk_longlong') -> true;
check_tc('tk_ulonglong') -> true;
check_tc('tk_float') -> true;
check_tc('tk_double') -> true;
%%check_tc('tk_longdouble') -> true;
check_tc('tk_boolean') -> true;
check_tc('tk_char') -> true;
check_tc('tk_octet') -> true;
check_tc('tk_any') -> true;
check_tc('tk_TypeCode') -> true;
check_tc('tk_Principal') -> true;
check_tc({'tk_objref', RepId, Name}) -> true;
check_tc({'tk_struct', RepId, Name, ElementList}) -> true;
check_tc({'tk_union', RepId, Name, DiscrTC, Default, ElementList}) -> true;
check_tc({'tk_enum', RepId, Name, ElementList}) -> true;
check_tc({'tk_string', MaxLength}) -> true;
check_tc({'tk_sequence', ElemTC, MaxLength}) -> true;
check_tc({'tk_array', ElemTC, Length}) -> true;
check_tc({'tk_alias', RepId, Name, TC}) -> true;
check_tc({'tk_except', RepId, Name, ElementList}) -> true;
check_tc({'none', Indirection}) -> true;
check_tc(_) -> false.
    
