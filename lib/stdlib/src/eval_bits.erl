-module(eval_bits).

-export([expr_grp/5,match_bits/6]).

%%% BITS help functions.

%%% The primary point here is not efficiency, but clarity.
%%% Bit sequences are represented as lists of 0 or 1.

%%% A note on floating-point number conversions: the Erlang code here
%%% is only used when NO_FLOAT_BIF is defined. When it is not defined,
%%% BIFs are used, which use the same code as the compiled code.
%%% Once we can use the binary syntax in this code itself (that is, in R8),
%%% we can remove both the Erlang code and the BIFs, and write exactly what
%%% we mean, using binary syntax.

%%% expr_grp/5 returns {value, Binary, New_bindings}.
%%% match_bits/6 returns either {match, New_bindings} or 'nomatch'.
%%% Their last argument should be 'true' if type defaulting should be
%%% done, 'false' otherwise (e.g., if sys_pre_expand has already done it).

%% error(Reason) -> exception thrown
%%  Throw a nice-looking exception, similar to exceptions from erl_eval.
error(Reason) ->
    exit({Reason,[{erl_eval,expr,3}]}).

%%% Part 1: expression evaluation (binary construction)

expr_grp([], Bs0, _Lf, Bits0, Call_maketype) ->
    Bits = lists:reverse(Bits0),
    Bits2 = lists:flatten(Bits),
    %% bits_to_bytes crashes if not multiple of 8.
    Bin = list_to_binary(bits_to_bytes(Bits2)),
    {value, Bin, Bs0};
expr_grp([Field | Fs], Bs0, Lf, Bits0, Call_maketype) ->
    {Bitl, Bs1} = expr_bit(Field, Bs0, Lf, Call_maketype),
    expr_grp(Fs, Bs1, Lf, [Bitl | Bits0], Call_maketype).

binary_to_bits(Bin, Size) ->
    lists:sublist(binary_to_bits(Bin), Size).

binary_to_bits(Bin) ->
    L = binary_to_list(Bin),
    bytes_to_bits(L).

bytes_to_bits([]) -> [];
bytes_to_bits([H|T]) -> byte_to_bits(H, 8, bytes_to_bits(T)).

byte_to_bits(_B, 0, Acc) -> Acc;
byte_to_bits(B, N, Acc) -> byte_to_bits(B bsr 1, N-1, [B band 1|Acc]).
    
maketype(E, Size0, Options0, true) ->
    make_bit_type(0, Size0, Options0);
maketype(E, Size0, Options0, false) ->
    {Size0, Options0}.

expr_bit({bin_element, _, {string, _, S}, default, default}, Bs0, _Fun,
	 _Call_maketype) ->
    {bytes_to_bits(S), Bs0};
expr_bit({bin_element, Line, E, Size0, Options0}, Bs0, Fun, Call_maketype) ->
    format("bit expr ~w~n", [{bin_element, Line, E, Size0, Options0}]),
    {value, V, Bs1} = Fun(E, Bs0),
    {Size1, Options} = maketype(E, Size0, Options0, Call_maketype),
    {value, Size, Bs} = Fun(Size1, Bs1),
    format("bit expr ~w~n", [{bin_element, x, V, Size, Options}]),
    Bitl = to_binary(V, Size, Options),
    format("bit list ~w~n", [Bitl]),
    {Bitl, Bs}.

size_or_all(all, All) -> All;
size_or_all(N, _All)  -> N.

to_binary(B0, Size0, [binary,{unit,Unit}|_]) when binary(B0) ->
    Size1 = size_or_all(Size0, size(B0)),
    binary_to_bits(B0, Size1*Unit);
to_binary(I, Size0, [integer,{unit,Unit}|Options]) when integer(I) ->
    Little = lists:member(little, Options),
    Size = Size0*Unit,
    L = i_to_bytes(I, Size),
    Bits = binary_to_bits(list_to_binary(L), Size),
    to_little_endian(Bits, Little);
to_binary(F, Size0, [float,{unit,Unit}|Options]) when float(F) ->
    Little = lists:member(little, Options),
    Size = Size0*Unit,
    Bits = float_to_ieee(F, Size),
    to_little_endian(Bits, Little);
to_binary(_, _, _) ->
    error(badarg).

type_and_unit([Type,{unit,Unit}|_]) -> {Type,Unit}.

mod(N, M) ->
    case N rem M of
	X when X < 0 ->
	    X+M;
	X ->
	    X
    end.

pick_bits(_I, 0, L) ->
    L;
pick_bits(I, Size, L) ->
    pick_bits(I bsr 1, Size-1, [I band 1 | L]).

i_to_bytes(I, Size) ->
    L = pick_bits(I, Size, lists:duplicate(mod(-Size, 8), 0)),
    bits_to_bytes(L).

bits_to_bytes([]) ->
    [];
bits_to_bytes(L0) ->
    {L, B} = pick8(L0),
    [B | bits_to_bytes(L)].

pick8(L) ->
    pick(L, 8, 0).

pick(L, 0, A) ->
    {L, A};
pick([], N, A) ->
    error(badarg);
pick([B|Rest], N, A) ->
    pick(Rest, N-1, 2*A+B).


%%% Big-endian serves as our native format here (regardless of what
%%% the endianness of the machine is). This is convenient, since the
%%% bit order within a byte is big-endian always.
%%% When a bit sequence consists of a number of 8-bit bytes, and a rest
%%% with less than 8 bits, the rest is at the start of the sequence for
%%% big-endian, at the end for little-endian.

%% to_little_endian(Bits, Little_p)
to_little_endian(B, false) -> B;
to_little_endian(B, true) ->
    %% an incomplete byte is at the start in the input
    L = length(B),
    P = L rem 8,
    {Piece, Rest} = split_list(P, B),
    R_big = bits_to_bytes(Rest),
    R_little = lists:reverse(R_big),
    bytes_to_bits(R_little) ++ Piece.

%% from_little_endian(Bits, Little_p)
from_little_endian(B, false) -> B;
%% an incomplete byte is at the end in the input
from_little_endian(B, true) ->
    L = length(B),
    P = L rem 8,
    {Rest, Piece} = split_list(L-P, B),
    R_little = bits_to_bytes(Rest),
    R_big = lists:reverse(R_little),
    Piece ++ bytes_to_bits(R_big).

float_to_ieee(F, Size) ->
    Bin = case catch <<F:Size/float>> of
	      {'EXIT',{badarg,_}} -> error(badarg);
	      {'EXIT',Reason}=Bad -> exit(Bad);
	      Other -> Other
	  end,
    binary_to_bits(Bin).

make_bit_type(Line, default, Type0) ->
    case erl_bits:set_bit_type(default, Type0) of
	{ok,all,Bt} -> {{atom,Line,all},erl_bits:as_list(Bt)};
	{ok,Size,Bt} -> {{integer,Line,Size},erl_bits:as_list(Bt)}
    end;
make_bit_type(Line, Size, Type0) ->		%Integer or 'all'
    {ok,Size,Bt} = erl_bits:set_bit_type(Size, Type0),
    {Size,erl_bits:as_list(Bt)}.

%%% Part 2: matching

match_bits(Fs, Bits, Bs0, Mfun, Efun, Call_maketype) ->
    case catch match_bits1(Fs, binary_to_bits(Bits), Bs0, Mfun, Efun, Call_maketype) of
	{match, Bs} ->
	    {match, Bs};
	_ ->
	    nomatch
    end.

match_bits1([], [], Bs, _Mfun, _Efun, _Call_maketype) ->
    {match, Bs};
match_bits1([Field | Rest], Bits0, Bs0, Mfun, Efun, Call_maketype) ->
%    format("matching ~w ~w~n", [Field, Bits0]),
    {Bs, Bits} = match_field(Field, Bits0, Bs0, Mfun, Efun, Call_maketype),
%    format("left ~w~n", [Bits]),
    match_bits1(Rest, Bits, Bs, Mfun, Efun, Call_maketype).

bits_to_int([1|_]=Bits, true) -> bits_to_int2(Bits, -1);
bits_to_int(Bits, Signed) -> bits_to_int2(Bits, 0).

bits_to_int2([], Acc) -> Acc;
bits_to_int2([Bit|Rest], Acc) ->
    bits_to_int2(Rest, Acc+Acc+Bit).

match_field({bin_element, _, String={string, _, S}, default, default},
	    Bits0, Bs0, Mfun, _Efun, _Call_maketype) ->
    Size = length(S),
    {Bits2, Bits} = split_list(Size*8, Bits0),
    Val = bits_to_bytes(Bits2),
    {match, Bs1} = Mfun(String, Val, Bs0),
    {Bs1, Bits};
match_field({bin_element, _, E, default, [binary|_]}, Bits0, Bs0, Mfun, Efun,
	    Call_maketype) ->
    B = list_to_binary(bits_to_bytes(Bits0)),
    {match, Bs} = Mfun(E, B, Bs0),
    {Bs, []};
match_field({bin_element, _, E, Size0, Options0}, Bits0, Bs0, Mfun, Efun,
	    Call_maketype) ->
%    format("match1 ~w~n", [{bin_element, Size0, Options0}]),
    {Size1, Options} = maketype(E, Size0, Options0, Call_maketype),
%    format("match2 ~w~n", [{bin_element, Size1, Options}]),
    case catch Efun(Size1, Bs0) of
	{value,all,Bs1} ->
	    B = list_to_binary(bits_to_bytes(Bits0)),
	    {match, Bs2} = Mfun(E, B, Bs1),
	    {Bs2,[]};
	{value,Size,Bs1} ->
	    {Type, Unit} = type_and_unit(Options),
%	    format("size ~w~n", [Size]),
	    {Bits2, Bits} = split_list(Size*Unit, Bits0),
	    Signed = lists:member(signed, Options),
	    Little = lists:member(little, Options),
	    Val = match_thing(Type, Bits2, Signed, Little),
	    {match,Bs2} = Mfun(E, Val, Bs1),
	    {Bs2,Bits};
	{'EXIT', Other} ->
	    %% Unbound variable, or something like it.
	    throw(Other)
    end.


split_list(N, List) ->
    {lists:sublist(List, N), lists:nthtail(N, List)}.

match_thing(Type, Bits, Signed, Little) ->
    case Type of
	integer ->
	    Bits1 = from_little_endian(Bits, Little),
	    bits_to_int(Bits1, Signed);
	float ->
	    Bits1 = from_little_endian(Bits, Little),
	    ieee_to_float(Bits1, length(Bits1));
	binary ->
	    list_to_binary(bits_to_bytes(Bits))
    end.

ieee_to_float(Bits, Size) ->
    Size = length(Bits),
    <<F:Size/float>> = list_to_binary(bits_to_bytes(Bits)),
    F.

%%% Trace output.
format(_Fmt, _Args) ->
%    io:format(_Fmt, _Args),
    ok.
