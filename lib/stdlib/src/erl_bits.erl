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

-module(erl_bits).

-include("../include/erl_bits.hrl").

-export([system_bittypes/0, 
	 system_bitdefault/0,
	 set_bit_type/2,
	 as_list/1]).

%% Dummies.

system_bitdefault() -> no_system_bitdefault.
system_bittypes() -> no_system_types.

%% as_list(BitType) -> TypeList.

as_list(Bt) ->
    [Bt#bittype.type,{unit,Bt#bittype.unit},Bt#bittype.sign,Bt#bittype.endian].

%% set_bit_type(Size, TypeList) ->
%%              {ok,Size,BitType} |
%%              {error,{undefined_bittype,Name}} |
%%              {error,{bittype_mismatch,T1,T2,What}}

set_bit_type(Size, default) ->
    set_bit_type(Size, []);
set_bit_type(Size, TypeList) ->
    case catch set_bit(TypeList, #bittype{}) of
	{ok,#bittype{type=Type,unit=Unit,sign=Sign,endian=Endian}} ->
	    case catch apply_defaults(Type, Size, Unit, Sign, Endian) of
		{'EXIT',Reason} ->
		    exit(Reason);
		{ok,_,_}=OK -> OK;
		{error,_}=Error -> Error
	    end;
	{error,_}=Error -> Error
    end.

set_bit([T0|Ts], Bt) ->
    case update_type(T0, #bittype{}) of
	Type ->
	    case merge_bittype(Type, Bt) of
		{ok,Bt2} -> set_bit(Ts, Bt2);
		Error -> Error
	    end
    end;
set_bit([], Bt) -> {ok,Bt}.

update_type({unit,Sz}, Type) when integer(Sz), Sz > 0, Sz =< 256 ->
    Type#bittype { unit = Sz };
update_type(integer,      Type) -> Type#bittype { type   = integer};
update_type(float,        Type) -> Type#bittype { type   = float};
update_type(binary,       Type) -> Type#bittype { type   = binary};
update_type(big,          Type) -> Type#bittype { endian = big };
update_type(little,       Type) -> Type#bittype { endian = little };
update_type(native,       Type) -> Type#bittype { endian = native };
update_type(signed,       Type) -> Type#bittype { sign   = signed };
update_type(unsigned,     Type) -> Type#bittype { sign   = unsigned };
update_type(Name, _) -> throw({error,{undefined_bittype,Name}}).

%%
%% Merge two bit type specifications.
%%
merge_bittype(B1, B2) ->
    Endian = merge_field(B1#bittype.endian, B2#bittype.endian, "endianness"),
    Sign   = merge_field(B1#bittype.sign, B2#bittype.sign, "sign"),
    Type   = merge_field(B1#bittype.type, B2#bittype.type, "type"),
    Unit   = merge_field(B1#bittype.unit, B2#bittype.unit, "unit"),
    {ok,#bittype{type = Type,unit = Unit,endian = Endian,sign = Sign}}.

merge_field(undefined, B, _) -> B;
merge_field(A, undefined, _) -> A;
merge_field(A, A, _) -> A;
merge_field(X, Y, What) ->
    throw({error,{bittype_mismatch,X,Y,What}}).

%%
%% Defaults are as follows.
%% 
%% The default is integer.
%% The default size is 'all' for binaries, 8 for integers, 64 for floats.
%% No unit must be given if the size is not given.
%% The default unit size is 8 for binaries, and 1 for integers and floats.
%% The default sign is always unsigned.
%% The default endian is always big.
%%

apply_defaults(undefined, Size, Unit, Sign, Endian) -> %default type
    apply_defaults(integer, Size, Unit, Sign, Endian);

apply_defaults(binary, default, Unit, Sign, Endian) -> %default size
    check_unit(Unit),
    apply_defaults(binary, all, 8, Sign, Endian);
apply_defaults(integer, default, Unit, Sign, Endian) ->
    check_unit(Unit),
    apply_defaults(integer, 8, 1, Sign, Endian);
apply_defaults(float, default, Unit, Sign, Endian) ->
    check_unit(Unit),
    apply_defaults(float, 64, 1, Sign, Endian);

apply_defaults(binary, Size, undefined, Sign, Endian) -> %default unit
    apply_defaults(binary, Size, 8, Sign, Endian);
apply_defaults(integer, Size, undefined, Sign, Endian) ->
    apply_defaults(integer, Size, 1, Sign, Endian);
apply_defaults(float, Size, undefined, Sign, Endian) ->
    apply_defaults(float, Size, 1, Sign, Endian);

apply_defaults(Type, Size, Unit, undefined, Endian) -> %default sign
    apply_defaults(Type, Size, Unit, unsigned, Endian);

apply_defaults(Type, Size, Unit, Sign, undefined) -> %default endian
    apply_defaults(Type, Size, Unit, Sign, big);

apply_defaults(Type, Size, Unit, Sign, Endian) -> %done
    {ok,Size,#bittype{type=Type,unit=Unit,sign=Sign,endian=Endian}}.

check_unit(undefined) -> ok;
check_unit(_) -> throw({error,bittype_unit}).

