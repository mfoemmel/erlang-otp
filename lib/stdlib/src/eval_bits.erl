-module(eval_bits).

-export([expr_grp/5,match_bits/6]).

-import(lists, [member/2,foldl/3]).

%%% BITS help functions.

%%% The primary point here is not efficiency, but clarity.
%%% Bit sequences are represented as lists of 0 or 1.
%%% In matching we convert to bit lists only as much as we
%%% need, and keep the tail as a binary.

%%% expr_grp/5 returns {value, Binary, New_bindings}.
%%% match_bits/6 returns either {match, New_bindings} or 'nomatch',
%%% or throws 'invalid' (if a pattern is illegal - this can only happen
%%% if lint hasn't been run).
%%% Their last argument should be 'true' if type defaulting should be
%%% done, 'false' otherwise (e.g., if sys_pre_expand has already done it).


%% error(Reason) -> exception thrown
%%  Throw a nice-looking exception, similar to exceptions from erl_eval.
error(Reason) ->
    exit({Reason,[{erl_eval,expr,3}]}).

%%% Part 1: expression evaluation (binary construction)

expr_grp([], Bs0, _Lf, Bits0, _Call_maketype) ->
    Bits = lists:reverse(Bits0),
    Bits2 = lists:flatten(Bits),
    %% bits_to_bytes crashes if not multiple of 8.
    Bin = list_to_binary(bits_to_bytes(Bits2)),
    {value, Bin, Bs0};
expr_grp([Field | Fs], Bs0, Lf, Bits0, Call_maketype) ->
    {Bitl, Bs1} = expr_bit(Field, Bs0, Lf, Call_maketype),
    expr_grp(Fs, Bs1, Lf, [Bitl | Bits0], Call_maketype).

binary_to_bits(Bin, Size) ->
    sublist(binary_to_bits(Bin), Size).

binary_to_bits(Bin) ->
    bytes_to_bits(binary_to_list(Bin)).

-define(GET_BIT(Byte, Bit), (if
				 Byte band Bit =:= 0 -> 0;
				 true -> 1
			     end)).

bytes_to_bits(L) ->
    bytes_to_bits(lists:reverse(L), []).

bytes_to_bits([], Acc) -> Acc;
bytes_to_bits([H|T], Acc0) ->
    Acc = [?GET_BIT(H, 128),?GET_BIT(H, 64),?GET_BIT(H, 32),?GET_BIT(H, 16),
	   ?GET_BIT(H, 8),?GET_BIT(H, 4),?GET_BIT(H, 2),?GET_BIT(H, 1)|Acc0],
    bytes_to_bits(T, Acc).
				     
maketype(Size0, Options0, true) ->
    make_bit_type(0, Size0, Options0);
maketype(Size0, Options0, false) ->
    {Size0, Options0}.

expr_bit({bin_element, _, {string, _, S}, default, default}, Bs0, _Fun,
	 _Call_maketype) ->
    {bytes_to_bits(S), Bs0};
expr_bit({bin_element, Line, E, Size0, Options0}, Bs0, Fun, Call_maketype) ->
    format("bit expr ~w~n", [{bin_element, Line, E, Size0, Options0}]),
    {value, V, Bs1} = Fun(E, Bs0),
    {Size1, Options} = maketype(Size0, Options0, Call_maketype),
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

pick_bits(_I, 0, L) -> L;
pick_bits(I, Size, L) ->
    pick_bits(I bsr 1, Size-1, [I band 1 | L]).

i_to_bytes(I, Size) ->
    L = pick_bits(I, Size, lists:duplicate(mod(-Size, 8), 0)),
    bits_to_bytes(L).

bits_to_bytes(L) ->
    bits_to_bytes(L, []).

bits_to_bytes([B7,B6,B5,B4,B3,B2,B1,B0|T], Acc) ->
    Byte = (B7 bsl 7) bor (B6 bsl 6) bor
	(B5 bsl 5) bor (B4 bsl 4) bor (B3 bsl 3) bor
	(B2 bsl 2) bor (B1 bsl 1) bor B0,
    bits_to_bytes(T, [Byte|Acc]);
bits_to_bytes([], Acc) -> lists:reverse(Acc);
bits_to_bytes(_, _) -> exit(badarg).

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
	      {'EXIT',_}=Bad -> exit(Bad);
	      Other -> Other
	  end,
    binary_to_bits(Bin).

%% Identical to the one in sys_pre_expand.
make_bit_type(Line, default, Type0) ->
    case erl_bits:set_bit_type(default, Type0) of
	{ok,all,Bt} -> {{atom,Line,all},erl_bits:as_list(Bt)};
	{ok,Size,Bt} -> {{integer,Line,Size},erl_bits:as_list(Bt)}
    end;
make_bit_type(Line, Size, Type0) ->		%Integer or 'all'
    {ok,Size,Bt} = erl_bits:set_bit_type(Size, Type0),
    {Size,erl_bits:as_list(Bt)}.

%%% Part 2: matching

match_bits(Fs, Bin, Bs0, Mfun, Efun, Call_maketype) ->
    case catch match_bits1(Fs, Bin, Bs0, Mfun, Efun, Call_maketype) of
	{match,Bs} -> {match,Bs};
	invalid -> throw(invalid);
	_ -> nomatch
    end.

match_bits1([], <<>>, Bs, _Mfun, _Efun, _Call_maketype) -> {match,Bs};
match_bits1([F|Fs], Bits0, Bs0, Mfun, Efun, Call_maketype) ->
    %%format("matching ~w ~w~n", [Field, Bits0]),
    {Bs,Bits} = match_field(F, Bits0, Bs0, Mfun, Efun, Call_maketype),
    %%format("left ~w~n", [Bits]),
    match_bits1(Fs, Bits, Bs, Mfun, Efun, Call_maketype).

bits_to_int([1|_]=Bits, true) -> bits_to_int2(Bits, -1);
bits_to_int(Bits, _) -> bits_to_int2(Bits, 0).

bits_to_int2([], Acc) -> Acc;
bits_to_int2([Bit|Rest], Acc) ->
    bits_to_int2(Rest, Acc+Acc+Bit).

match_field({bin_element,_,{string,_,S},default,default},
	    Bin, Bs, Mfun, _Efun, _Call_maketype) ->
    Tail = foldl(fun(C, <<C:8,Tail/binary>>) -> Tail;
		    (C, Bits0) ->
			 {Bits,Tail} = get_bits(Bits0, 8),
			 C = bits_to_bytes(Bits),
			 Tail
		 end, Bin, S),
    {Bs,Tail};
match_field({bin_element,_,E,default,[binary|_]}, Bin, Bs0, Mfun, _Efun, _) ->
    {match,Bs} = Mfun(E, Bin, Bs0),
    {Bs,<<>>};
match_field({bin_element, _,E,Size0,Options0}, Bin, Bs0, Mfun, Efun,
	    Call_maketype) ->
    {Size1,Options} = maketype(Size0, Options0, Call_maketype),
    case Efun(Size1, Bs0) of
	{value,all,Bs1} when binary(Bin) ->
	    {match,Bs2} = Mfun(E, Bin, Bs1),
	    {Bs2,<<>>};
	{value,Size,Bs1} ->
	    {Type,Unit} = type_and_unit(Options),
	    {Val,Tail} = match_thing(Type, Options, Size*Unit, Bin),
	    {match,Bs2} = Mfun(E, Val, Bs1),
	    {Bs2,Tail}
    end.

match_thing(binary, Opts, Size, Bin) when Size rem 8 =:= 0, binary(Bin) ->
    split_binary(Bin, Size div 8);
match_thing(binary, Opts, Size, Bin) ->
    {Bits,Tail} = get_bits(Bin, Size),
    {list_to_binary(bits_to_bytes(Bits)),Tail};
match_thing(integer, Opts, Size, Bin) ->
    {Bits0,Tail} = get_bits(Bin, Size),
    Bits1 = from_little_endian(Bits0, member(little, Opts)),
    {bits_to_int(Bits1, member(signed, Opts)),Tail};
match_thing(float, Opts, Size, Bin) ->
    {Bits0,Tail} = get_bits(Bin, Size),
    Bits1 = from_little_endian(Bits0, member(little, Opts)),
    <<Float:Size/float>> = list_to_binary(bits_to_bytes(Bits1)),
    {Float,Tail};
match_thing(Type, Opts, Size, Bin) ->
    erlang:display({Type,Opts,Size,Bin}),
    exit(badarg).

get_bits(Bin0, N) when binary(Bin0), N rem 8 =:= 0 ->
    <<Bin:N/binary-unit:1,Tail/binary>> = Bin0,
    {bytes_to_bits(binary_to_list(Bin)),Tail};
get_bits(Bin, N) when binary(Bin) ->
    get_bits({[],0,Bin}, N);
get_bits({Bits,N,Bin}, N) -> {Bits,Bin};
get_bits({Bits,N,Bin}, Need) when Need < N ->
    {sublist(Bits, Need),{lists:nthtail(Need, Bits),N-Need,Bin}};
get_bits({Bits0,N,Bin0}, Need) ->
    BytesNeeded = (Need-N+7) div 8,
    <<Bin:BytesNeeded/binary,Tail/binary>> = Bin0,
    Bits = Bits0 ++ bytes_to_bits(binary_to_list(Bin)),
    case 8*size(Bin)+N of
	Need ->
	    {Bits,Tail};
	Have ->
	    {sublist(Bits, Need),{lists:nthtail(Need, Bits),Have-Need,Tail}}
    end.

split_list(N, List) ->
    {sublist(List, N), lists:nthtail(N, List)}.

%% sublist that doesn't allow longer N than the list.
sublist([E|Rest], N) when integer(N), N > 0 ->
    [E | sublist(Rest, N-1)];
sublist([], 0) ->
    [];
sublist([_|_], 0) ->
    [].


%%% Trace output.
format(_Fmt, _Args) ->
%    io:format(_Fmt, _Args),
    ok.
