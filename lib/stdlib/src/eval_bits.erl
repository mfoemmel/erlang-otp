%% -*- erlang-indent-level: 4 -*-
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
%%     $Id $
%%
-module(eval_bits).

-export([expr_grp/3,expr_grp/5,match_bits/6, 
	 match_bits/7,bin_gen/6]).

%% Types used in this module:
%% @type bindings(). An abstract structure for bindings between
%% variables and values (the environment)
%%
%% @type evalfun(). A closure which evaluates an expression given an
%% environment
%%
%% @type matchfun(). A closure which performs a match given a value, a
%% pattern and an environment
%%
%% @type field() represents a field in a "bin"

%%% Part 1: expression evaluation (binary construction)

%% @spec expr_grp(Fields::[field()], Bindings::bindings(), 
%%                EvalFun::evalfun()) -> 
%%                  {value, binary(), bindings()}
%%
%% @doc Returns a tuple with {value,Bin,Bs} where Bin is the binary
%% constructed from form the Fields under the current Bindings. Bs
%% contains the present bindings. This function can also throw an
%% exception if the construction fails.

expr_grp(Fields, Bindings, EvalFun, [], _) ->
    expr_grp(Fields, Bindings, EvalFun, <<>>);
expr_grp(Fields, Bindings, EvalFun, ListOfBits, _) ->
    Bin = convert_list(ListOfBits),
    expr_grp(Fields, Bindings, EvalFun, Bin).

convert_list(List) ->
  << <<X:1>> || X <- List >>.

expr_grp(Fields, Bindings, EvalFun) ->
    catch expr_grp(Fields, Bindings, EvalFun, <<>>).

expr_grp([Field | FS], Bs0, Lf, Acc) ->
    {Bin,Bs} = eval_field(Field, Bs0, Lf),
    expr_grp(FS, Bs, Lf, <<Acc/binary-unit:1,Bin/binary-unit:1>>);
expr_grp([], Bs0, _Lf, Acc) ->
    {value,Acc,Bs0}.

eval_field({bin_element, _, {string, _, S}, default, default}, Bs0, _Fun) ->
    {list_to_binary(S),Bs0};
eval_field({bin_element,Line,E,Size0,Options0}, Bs0, Fun) ->
    {value,V,Bs1} = Fun(E, Bs0),
    {Size1,[Type,{unit,Unit},Sign,Endian]} = 
        make_bit_type(Line, Size0, Options0),
    {value,Size,Bs} = Fun(Size1, Bs1),
    {eval_exp_field1(V, Size, Unit, Type, Endian, Sign),Bs}.

eval_exp_field1(V, Size, Unit, Type, Endian, Sign) ->
    case catch eval_exp_field(V, Size, Unit, Type, Endian, Sign) of
        <<Val/bitstring>> -> Val;
        _ -> error(badarg)
    end.

eval_exp_field(Val, Size, Unit, integer, little, signed) ->
    <<Val:(Size*Unit)/little-signed>>;
eval_exp_field(Val, Size, Unit, integer, little, unsigned) ->
    <<Val:(Size*Unit)/little>>;
eval_exp_field(Val, Size, Unit, integer, native, signed) ->
    <<Val:(Size*Unit)/native-signed>>;
eval_exp_field(Val, Size, Unit, integer, native, unsigned) ->
    <<Val:(Size*Unit)/native>>;
eval_exp_field(Val, Size, Unit, integer, big, signed) ->
    <<Val:(Size*Unit)/signed>>;
eval_exp_field(Val, Size, Unit, integer, big, unsigned) ->
    <<Val:(Size*Unit)>>;
eval_exp_field(Val, _Size, _Unit, utf8, _, _) ->
    %% XXX Simplify code in the release following R12B-5.
    %% <<Val/utf8>>
    int_to_utf8(Val);
eval_exp_field(Val, _Size, _Unit, utf16, big, _) ->
    %% XXX Simplify code in the release following R12B-5.
    %% <<Val/big-utf16>>
    int_to_utf16_be(Val);
eval_exp_field(Val, _Size, _Unit, utf16, little, _) ->
    %% XXX Simplify code in the release following R12B-5.
    %% <<Val/little-utf16>>
    int_to_utf16_le(Val);
eval_exp_field(Val, _Size, _Unit, utf32, big, _) ->
    %% XXX Simplify code in the release following R12B-5.
    %%<<Val/big-utf32>>
    int_to_utf16_be(Val),			%Range check.
    <<Val:32/big>>;
eval_exp_field(Val, _Size, _Unit, utf32, little, _) ->
    %% XXX Simplify code in the release following R12B-5.
    %% <<Val/little-utf32>>
    int_to_utf16_le(Val),			%Range check.
    <<Val:32/little>>;
eval_exp_field(Val, Size, Unit, float, little, _) ->
    <<Val:(Size*Unit)/float-little>>;
eval_exp_field(Val, Size, Unit, float, native, _) ->
    <<Val:(Size*Unit)/float-native>>;
eval_exp_field(Val, Size, Unit, float, big, _) ->
    <<Val:(Size*Unit)/float>>;
eval_exp_field(Val, all, Unit, binary, _, _) ->
    case bit_size(Val) of
	Size when Size rem Unit =:= 0 ->
	    <<Val:Size/binary-unit:1>>;
	_ ->
	    error(badarg)
    end;
eval_exp_field(Val, Size, Unit, binary, _, _) ->
    <<Val:(Size*Unit)/binary-unit:1>>.

int_to_utf8(I) when 0 =< I, I =< 16#7F ->
    <<I>>;
int_to_utf8(I) when 0 =< I, I =< 16#7FF ->
    B2 = I,
    B1 = (I bsr 6),
    <<1:1,1:1,0:1,B1:5,1:1,0:1,B2:6>>;
int_to_utf8(I) when 0 =< I, I =< 16#FFFF ->
    if
	16#D800 =< I, I =< 16#DFFF;
	I =:= 16#FFFE; I =:= 16#FFFF -> error(badarg);
	true -> ok
    end,
    B3 = I,
    B2 = (I bsr 6),
    B1 = (I bsr 12),
    <<1:1,1:1,1:1,0:1,B1:4,1:1,0:1,B2:6,1:1,0:1,B3:6>>;
int_to_utf8(I) when 0 =< I, I =< 16#10FFFF ->
    B4 = I,
    B3 = (I bsr 6),
    B2 = (I bsr 12),
    B1 = (I bsr 18),
    <<1:1,1:1,1:1,1:1,0:1,B1:3,1:1,0:1,B2:6,1:1,0:1,B3:6,1:1,0:1,B4:6>>;
int_to_utf8(_) ->
    error(badarg).

int_to_utf16_be(I) when 0 =< I, I =< 16#10000 ->
    if
	16#D800 =< I, I =< 16#DFFF;
	I =:= 16#FFFE; I =:= 16#FFFF -> error(badarg);
	true -> ok
    end,
    <<I:16/big>>;
int_to_utf16_be(I) when 16#10000 =< I, I =< 16#10FFFF ->
    U = I - 16#10000,
    W1 = 16#D800 bor (U bsr 10),
    W2 = 16#DC00 bor (U band 16#3FF),
    <<W1:16/big,W2:16/big>>;
int_to_utf16_be(_) ->
    error(badarg).

int_to_utf16_le(I) when 0 =< I, I =< 16#10000 ->
    if
	16#D800 =< I, I =< 16#DFFF;
	I =:= 16#FFFE; I =:= 16#FFFF -> error(badarg);
	true -> ok
    end,
    <<I:16/little>>;
int_to_utf16_le(I) when 16#10000 =< I, I =< 16#10FFFF ->
    U = I - 16#10000,
    W1 = 16#D800 bor (U bsr 10),
    W2 = 16#DC00 bor (U band 16#3FF),
    <<W1:16/little,W2:16/little>>;
int_to_utf16_le(_) ->
    error(badarg).

%%% Part 2: matching in binary comprehensions
%% @spec bin_gen(BinPattern::{bin,integer(),[field()]}, Bin::binary(),
%%               GlobalEnv::bindings(), LocalEnv::bindings(),  
%%               MatchFun::matchfun(), EvalFun::evalfun()) -> 
%%                 {match, binary(), bindings()} | {nomatch, binary()} | done
%%
%% @doc Used to perform matching in a comprehension. If the match
%% succeeds a new environment and what remains of the binary is
%% returned. If the match fails what remains of the binary is returned.
%% If nothing remains of the binary the atom 'done' is returned.

bin_gen({bin,_,Fs}, Bin, Bs0, BBs0, Mfun, Efun) ->
    bin_gen(Fs, Bin, Bs0, BBs0, Mfun, Efun, true).

bin_gen([F|Fs], Bin, Bs0, BBs0, Mfun, Efun, Flag) ->
    case bin_gen_field(F, Bin, Bs0, BBs0, Mfun, Efun) of
        {match,Bs,BBs,Rest} ->
            bin_gen(Fs, Rest, Bs, BBs, Mfun, Efun, Flag);
        {nomatch,Rest} ->
            bin_gen(Fs, Rest, Bs0, BBs0, Mfun, Efun, false);
        done ->
            done
    end;
bin_gen([], Bin, Bs0, _BBs0, _Mfun, _Efun, true) ->
    {match, Bin, Bs0};
bin_gen([], Bin, _Bs0, _BBs0, _Mfun, _Efun, false) ->
    {nomatch, Bin}.
  
bin_gen_field({bin_element,_,{string,_,S},default,default},
              Bin, Bs, BBs, _Mfun, _Efun) ->
    Bits = list_to_binary(S),
    Size = byte_size(Bits),
    case Bin of
        <<Bits:Size/binary,Rest/bitstring>> ->
            {match,Bs,BBs,Rest};
        <<_:Size/binary,Rest/bitstring>> ->
            {nomatch,Rest};
        _ ->
            done
    end;
bin_gen_field({bin_element,Line,VE,Size0,Options0}, 
              Bin, Bs0, BBs0, Mfun, Efun) ->
    {Size1, [Type,{unit,Unit},Sign,Endian]} = 
        make_bit_type(Line, Size0, Options0),
    V = erl_eval:partial_eval(VE),
    match_check_size(Size1, BBs0),
    {value, Size, _BBs} = Efun(Size1, BBs0),
    case catch get_value(Bin, Type, Size, Unit, Sign, Endian) of
        {Val,<<_/bitstring>>=Rest} ->
            NewV = coerce_to_float(V, Type),
            case catch Mfun(NewV, Val, Bs0) of
                {match,Bs} ->
                    BBs = add_bin_binding(NewV, Bs, BBs0),
                    {match,Bs,BBs,Rest};
                _ ->
                    {nomatch,Rest}
            end;
        _ ->
            done
    end.

%%% Part 3: binary pattern matching 
%% @spec match_bits(Fields::[field()], Bin::binary()
%%                  GlobalEnv::bindings(), LocalEnv::bindings(),  
%%                  MatchFun::matchfun(),EvalFun::evalfun()) -> 
%%                  {match, bindings()} 
%% @doc Used to perform matching. If the match succeeds a new
%% environment is returned. If the match have some syntactic or
%% semantic problem which would have been caught at compile time this
%% function throws 'invalid', if the matching fails for other reasons
%% the function throws 'nomatch'

match_bits(Fs, Bin, Bs0, BBs, Mfun, Efun, _) ->
    match_bits(Fs, Bin, Bs0, BBs, Mfun, Efun).

match_bits(Fs, Bin, Bs0, BBs, Mfun, Efun) ->
    case catch match_bits_1(Fs, Bin, Bs0, BBs, Mfun, Efun) of
        {match,Bs} -> {match,Bs};
        invalid -> throw(invalid);
        _Error -> throw(nomatch)
    end.

match_bits_1([], <<>>,  Bs, _BBs, _Mfun, _Efun) -> 
    {match,Bs};
match_bits_1([F|Fs], Bits0, Bs0, BBs0, Mfun, Efun) ->
    {Bs,BBs,Bits} = match_field_1(F, Bits0, Bs0, BBs0, Mfun, Efun),
    match_bits_1(Fs, Bits, Bs, BBs, Mfun, Efun).

match_field_1({bin_element,_,{string,_,S},default,default},
              Bin, Bs, BBs, _Mfun, _Efun) ->
    Bits = list_to_binary(S),
    Size = byte_size(Bits),
    <<Bits:Size/binary,Rest/binary-unit:1>> = Bin,
    {Bs,BBs,Rest};
match_field_1({bin_element,Line,VE,Size0,Options0}, 
              Bin, Bs0, BBs0, Mfun, Efun) ->
    {Size1, [Type,{unit,Unit},Sign,Endian]} = 
        make_bit_type(Line, Size0, Options0),
    V = erl_eval:partial_eval(VE),
    Size2 = erl_eval:partial_eval(Size1),
    match_check_size(Size2, BBs0),
    {value, Size, _BBs} = Efun(Size2, BBs0),
    {Val,Rest} = get_value(Bin, Type, Size, Unit, Sign, Endian),
    NewV = coerce_to_float(V, Type),
    {match,Bs} = Mfun(NewV, Val, Bs0),
    BBs = add_bin_binding(NewV, Bs, BBs0),
    {Bs,BBs,Rest}.

%% Almost identical to the one in sys_pre_expand.
coerce_to_float({integer,L,I}=E, float) ->
    try
	{float,L,float(I)}
    catch
	error:badarg -> E;
	error:badarith -> E
    end;
coerce_to_float(E, _Type) -> 
    E.

add_bin_binding({var,_,'_'}, _Bs, BBs) ->
    BBs;
add_bin_binding({var,_,Name}, Bs, BBs) ->
    {value,Value} = erl_eval:binding(Name, Bs),
    erl_eval:add_binding(Name, Value, BBs);
add_bin_binding(_, _Bs, BBs) ->
    BBs.

get_value(Bin, integer, Size, Unit, Sign, Endian) ->
    get_integer(Bin, Size*Unit, Sign, Endian);
get_value(Bin, float, Size, Unit, _Sign, Endian) ->
    get_float(Bin, Size*Unit, Endian);
get_value(Bin0, utf8, undefined, _Unit, _Sign, _Endian) ->
    Size = utf8_size(Bin0),
    <<Bin:Size/bytes,Rest/bits>> = Bin0,
    {utf8_to_int(Bin),Rest};
get_value(Bin, utf16, undefined, _Unit, _Sign, big) ->
    get_value_utf16_be(Bin);
get_value(Bin, utf16, undefined, _Unit, _Sign, little) ->
    get_value_utf16_le(Bin);
get_value(Bin, utf32, undefined, _Unit, _Sign, big) ->
    <<Val:32/big,Rest/bits>> = Bin,
    int_to_utf16_be(Val),			%XXX Range check.
    {Val,Rest};
get_value(Bin, utf32, undefined, _Unit, _Sign, little) ->
    <<Val:32/little,Rest/bits>> = Bin,
    int_to_utf16_le(Val),			%XXX Range check.
    {Val,Rest};
get_value(Bin, binary, all, Unit, _Sign, _Endian) ->
    0 = (bit_size(Bin) rem Unit),
    {Bin,<<>>};
get_value(Bin, binary, Size, Unit, _Sign, _Endian) ->
    TotSize = Size*Unit,
    <<Val:TotSize/bitstring,Rest/bits>> = Bin,
    {Val,Rest}.

utf8_size(<<0:1,_:7,_/binary>>) -> 1;
utf8_size(<<1:1,1:1,0:1,_:5,_/binary>>) -> 2;
utf8_size(<<1:1,1:1,1:1,0:1,_:4,_/binary>>) -> 3;
utf8_size(<<1:1,1:1,1:1,1:1,0:1,_:3,_/binary>>) -> 4.

utf8_to_int(<<0:1,B:7>>) ->
    B;
utf8_to_int(<<1:1,1:1,0:1,B1:5,1:1,0:1,B2:6>>) ->
    (B1 bsl 6) bor B2;
utf8_to_int(<<1:1,1:1,1:1,0:1,B1:4,1:1,0:1,B2:6,1:1,0:1,B3:6>>) ->
    (B1 bsl 12) bor (B2 bsl 6) bor B3;
utf8_to_int(<<1:1,1:1,1:1,1:1,0:1,B1:3,1:1,0:1,
             B2:6,1:1,0:1,B3:6,1:1,0:1,B4:6>>) ->
    Res = (B1 bsl 18) bor (B2 bsl 12) bor (B3 bsl 6) bor B4,
    case Res of
        X when X > 16#10FFFF ->
            exit(unsupported_utf8);
        Other ->
            Other
    end.

get_value_utf16_be(<<I:16,Rest/bits>>) when I < 16#D800;
					    I > 16#DFFFF ->
    {I,Rest};
get_value_utf16_be(<<W1:16,W2:16,Rest/bits>>) ->
    {(((W1 band 16#3FF) bsl 10) bor (W2 band 16#3FF)) +
     16#10000,Rest}.

get_value_utf16_le(<<I:16/little,Rest/bits>>) when I < 16#D800;
					    I > 16#DFFFF ->
    {I,Rest};
get_value_utf16_le(<<W1:16/little,W2:16/little,Rest/bits>>) ->
    {((W1 band 16#3FF) bsl 10) bor (W2 band 16#3FF) +
     16#10000,Rest}.

get_integer(Bin, Size, signed, little) ->
    <<Val:Size/little-signed,Rest/binary-unit:1>> = Bin,
    {Val,Rest};
get_integer(Bin, Size, unsigned, little) ->
    <<Val:Size/little,Rest/binary-unit:1>> = Bin,
    {Val,Rest};
get_integer(Bin, Size, signed, native) ->
    <<Val:Size/native-signed,Rest/binary-unit:1>> = Bin,
    {Val,Rest};
get_integer(Bin, Size, unsigned, native) ->
    <<Val:Size/native,Rest/binary-unit:1>> = Bin,
    {Val,Rest};
get_integer(Bin, Size, signed, big) ->
    <<Val:Size/signed,Rest/binary-unit:1>> = Bin,
    {Val,Rest};
get_integer(Bin, Size, unsigned, big) ->
    <<Val:Size,Rest/binary-unit:1>> = Bin,
    {Val,Rest}.

get_float(Bin, Size, little) -> 
    <<Val:Size/float-little,Rest/binary-unit:1>> = Bin,
    {Val,Rest};
get_float(Bin, Size, native) -> 
    <<Val:Size/float-native,Rest/binary-unit:1>> = Bin,
    {Val,Rest};
get_float(Bin, Size, big) -> 
    <<Val:Size/float,Rest/binary-unit:1>> = Bin,
    {Val,Rest}.

%% Identical to the one in sys_pre_expand.
make_bit_type(Line, default, Type0) ->
    case erl_bits:set_bit_type(default, Type0) of
        {ok,all,Bt} -> {{atom,Line,all},erl_bits:as_list(Bt)};
	{ok,undefined,Bt} -> {{atom,Line,undefined},erl_bits:as_list(Bt)};
        {ok,Size,Bt} -> {{integer,Line,Size},erl_bits:as_list(Bt)};
        {error,Reason} -> error(Reason)
    end;
make_bit_type(_Line, Size, Type0) -> %Size evaluates to an integer or 'all'
    case erl_bits:set_bit_type(Size, Type0) of
        {ok,Size,Bt} -> {Size,erl_bits:as_list(Bt)};
        {error,Reason} -> error(Reason)
    end.

match_check_size({var,_,V}, Bs) -> 
    case erl_eval:binding(V, Bs) of
        {value,_} -> ok;
	unbound -> throw(invalid) % or, rather, error({unbound,V})
    end;
match_check_size({atom,_,all}, _Bs) ->
    ok;
match_check_size({atom,_,undefined}, _Bs) ->
    ok;
match_check_size({integer,_,_}, _Bs) ->
    ok;
match_check_size({value,_,_}, _Bs) ->
    ok;	%From the debugger.
match_check_size(_, _Bs) -> 
    throw(invalid).

%% error(Reason) -> exception thrown
%%  Throw a nice-looking exception, similar to exceptions from erl_eval.
error(Reason) ->
    erlang:raise(error, Reason, [{erl_eval,expr,3}]).

