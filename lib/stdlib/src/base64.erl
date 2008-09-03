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
%% Description: Implements base 64 encode and decode, see RFC6848.

-module(base64).

-export([encode/1, decode/1, mime_decode/1,
	 encode_to_string/1, decode_to_string/1, mime_decode_to_string/1]).

%%-------------------------------------------------------------------------
%% The following type is a subtype of string() for return values
%% of (some) functions of this module.
%%-------------------------------------------------------------------------

-type ascii_string() :: [1..255].

%%-------------------------------------------------------------------------
%% define a threshold for big binaries wich will be chopped off and
%% converted in chunks (must be a multiple both 3 and 4, i.e. 12)
-define(BIG, 24000).

%%-------------------------------------------------------------------------
%% encode_to_string(ASCII) -> Base64String
%%	ASCII - string() | binary()
%%	Base64String - string()
%%                                   
%% Description: Encodes a plain ASCII string (or binary) into base64.
%%-------------------------------------------------------------------------

-spec encode_to_string(string() | binary()) -> ascii_string().

encode_to_string(Bin) when is_binary(Bin) ->
    encode_to_string(binary_to_list(Bin));
encode_to_string(List) when is_list(List) ->
    encode_l(List).

%%-------------------------------------------------------------------------
%% encode(ASCII) -> Base64
%%	ASCII - string() | binary()
%%	Base64 - binary()
%%                                   
%% Description: Encodes a plain ASCII string (or binary) into base64.
%%-------------------------------------------------------------------------

-spec encode(string() | binary()) -> binary().

encode(Bin) when is_binary(Bin), byte_size(Bin) > ?BIG ->
    {A, B} = split_binary(Bin, ?BIG),
    L = encode_l(binary_to_list(A)),
    list_to_binary([L, encode(B)]);
encode(Bin) when is_binary(Bin) ->
    encode(binary_to_list(Bin));
encode(List) when is_list(List) ->
    list_to_binary(encode_l(List)).

-spec encode_l(string()) -> ascii_string().

encode_l(List) ->
    encode(List, encode_tuple()).
    
%% Base-64 encoding: take 6 bits at a time from the head of the binary
%% and emit it as 8 bit characters.
encode([], _T) ->
    [];
encode([A], T) ->
    [b64e(A bsr 2, T),
     b64e((A band 3) bsl 4, T), $=, $=];
encode([A,B], T) ->
    [b64e(A bsr 2, T),
     b64e(((A band 3) bsl 4) bor (B bsr 4), T), 
     b64e((B band 15) bsl 2, T), $=];
encode([A,B,C|Ls], T) ->
    BB = (A bsl 16) bor (B bsl 8) bor C,
    [b64e(BB bsr 18, T),
     b64e((BB bsr 12) band 63, T), 
     b64e((BB bsr 6) band 63, T),
     b64e(BB band 63, T) | encode(Ls, T)].

%%-------------------------------------------------------------------------
%% mime_decode(Base64) -> ASCII
%% decode(Base64) -> ASCII
%%	Base64 - string() | binary()
%%	ASCII - binary()
%%                                    
%% Description: Decodes an base64 encoded string to plain ASCII.
%% mime_decode strips away all characters not Base64 before converting,
%% whereas decode crashes if an illegal character is found
%%-------------------------------------------------------------------------

-spec decode(string() | binary()) -> binary().

decode(Bin) when is_binary(Bin), byte_size(Bin) > ?BIG ->
    {A, B} = split_binary(Bin, ?BIG),
    L = decode_l(binary_to_list(A)),
    list_to_binary([L, decode(B)]);
decode(Bin) when is_binary(Bin) ->
    decode(binary_to_list(Bin));
decode(List) when is_list(List) ->
    list_to_binary(decode_l(List)).

-spec mime_decode(string() | binary()) -> binary().

mime_decode(Bin) when is_binary(Bin), byte_size(Bin) > ?BIG ->
    {A, B} = split_binary(Bin, ?BIG),
    L = mime_decode_l(binary_to_list(A)),
    list_to_binary([L, mime_decode(B)]);
mime_decode(Bin) when is_binary(Bin) ->
    mime_decode(binary_to_list(Bin));
mime_decode(List) when is_list(List) ->
    list_to_binary(mime_decode_l(List)).

-spec decode_l(string()) -> string().

decode_l(List) ->
    L = strip_spaces(List, []),
    decode(L, decode_tuple(), []).

-spec mime_decode_l(string()) -> string().

mime_decode_l(List) ->
    L = strip_illegal(List, []),
    decode(L, decode_tuple(), []).

%%-------------------------------------------------------------------------
%% mime_decode_to_string(Base64) -> ASCII
%% decode_to_string(Base64) -> ASCII
%%	Base64 - string() | binary()
%%	ASCII - binary()
%%
%% Description: Decodes an base64 encoded string to plain ASCII.
%% mime_decode strips away all characters not Base64 before converting,
%% whereas decode crashes if an illegal character is found
%%-------------------------------------------------------------------------

-spec decode_to_string(string() | binary()) -> string().

decode_to_string(Bin) when is_binary(Bin) ->
    decode_to_string(binary_to_list(Bin));
decode_to_string(List) when is_list(List) ->
    decode_l(List).

-spec mime_decode_to_string(string() | binary()) -> string().

mime_decode_to_string(Bin) when is_binary(Bin) ->
    mime_decode_to_string(binary_to_list(Bin));
mime_decode_to_string(List) when is_list(List) ->
    mime_decode_l(List).


decode([], _T, A) ->
    A;
decode([$=,$=,C2,C1|Cs], T, A) ->
    Bits2x6 = (b64d(C1, T) bsl 18) bor (b64d(C2, T) bsl 12),
    Octet1 = Bits2x6 bsr 16,
    decode(Cs, T, [Octet1|A]);
decode([$=,C3,C2,C1|Cs], T, A) ->
    Bits3x6 = (b64d(C1, T) bsl 18) bor (b64d(C2, T) bsl 12)
	bor (b64d(C3, T) bsl 6),
    Octet1 = Bits3x6 bsr 16,
    Octet2 = (Bits3x6 bsr 8) band 16#ff,
    decode(Cs, T, [Octet1,Octet2|A]);
decode([C4,C3,C2,C1| Cs], T, A) ->
    Bits4x6 = (b64d(C1, T) bsl 18) bor (b64d(C2, T) bsl 12)
	bor (b64d(C3, T) bsl 6) bor b64d(C4, T),
    Octet1 = Bits4x6 bsr 16,
    Octet2 = (Bits4x6 bsr 8) band 16#ff,
    Octet3 = Bits4x6 band 16#ff,
    decode(Cs, T, [Octet1,Octet2,Octet3|A]).


%%%========================================================================
%%% Internal functions
%%%========================================================================

strip_spaces([], A) -> A;
strip_spaces([$=,C|_], A) when C =/= $= -> [$=, A];
strip_spaces([$\s|Cs], A) -> strip_spaces(Cs, A);
strip_spaces([$\t|Cs], A) -> strip_spaces(Cs, A);
strip_spaces([$\r|Cs], A) -> strip_spaces(Cs, A);
strip_spaces([$\n|Cs], A) -> strip_spaces(Cs, A);
strip_spaces([C|Cs], A) -> strip_spaces(Cs, [C | A]).

strip_illegal([], A) -> A;
strip_illegal([C|Cs], A) when C >= $A, C =< $Z -> strip_illegal(Cs, [C|A]);
strip_illegal([C|Cs], A) when C >= $a, C =< $z -> strip_illegal(Cs, [C|A]);
strip_illegal([C|Cs], A) when C >= $0, C =< $9 -> strip_illegal(Cs, [C|A]);
strip_illegal([$=,C|_], A) when C =/= $= -> [$= | A];
strip_illegal([C|Cs], A) when C=:=$+; C=:=$/; C=:=$= -> strip_illegal(Cs, [C|A]);
strip_illegal([_|Cs], A) -> strip_illegal(Cs, A).

%% arrays for character translation
%% value -> base64 (zero-based)
encode_tuple() ->
    {$A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M, $N,
     $O, $P, $Q, $R, $S, $T, $U, $V, $W, $X, $Y, $Z,
     $a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n,
     $o, $p, $q, $r, $s, $t, $u, $v, $w, $x, $y, $z,
     $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $+, $/}.	

%% base64 -> value (one-based)
decode_tuple() ->
    {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,62,-1,-1,-1,63,
     52,53,54,55,56,57,58,59,60,61,-1,-1,-1,-1,-1,-1,
     -1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,
     15,16,17,18,19,20,21,22,23,24,25,-1,-1,-1,-1,-1,
     -1,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
     41,42,43,44,45,46,47,48,49,50,51,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}.

%% accessors 
b64e(X, T) ->
    element(X+1, T).

b64d(X, T) ->
    b64d_ok(element(X, T)).

-spec b64d_ok(non_neg_integer()) -> non_neg_integer().
b64d_ok(N) when N >= 0 ->
    N.
