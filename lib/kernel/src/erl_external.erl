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
-module(erl_external).

-export([to_external/2, to_external/3, to_external/4,
	 from_external/3]).

-define(EXTERN_TAG, 131).
-define(SMALL_INTEGER, 97).
-define(INTEGER, 98).
-define(FLOAT, 99).
-define(ATOM, 100).
-define(REF, 101).
-define(PORT, 102).
-define(PID, 103).
-define(SMALL_TUPLE, 104).
-define(LARGE_TUPLE, 105).
-define(NIL, 106).
-define(STRING, 107).
-define(LIST, 108).
-define(BINARY, 109).
-define(SMALL_BIG, 110).
-define(LARGE_BIG, 111).
-define(NEW_CACHE, 78).
-define(CACHED_ATOM, 67).

-define(HASH_RANGE, 254).

-define(int16(X), [((X) bsr 8) band 16#ff, (X) band 16#ff]).

-define(i16(X1,X0),
        (?u16(X1,X0) - 
         (if (X1) > 127 -> 16#10000; true -> 0 end))).

-define(i32(X3,X2,X1,X0),
        (?u32(X3,X2,X1,X0) - 
         (if (X3) > 127 -> 16#100000000; true -> 0 end))).

-define(u16(X1,X0),
        (((X1) bsl 8) bor (X0))).

-define(u32(X3,X2,X1,X0), 
        (((X3) bsl 24) bor ((X2) bsl 16) bor ((X1) bsl 8) bor (X0))).

%% --------------------------------------------------------------
%% Convert a term encoded in external format into a local term.
%% If a new external is found, return the decoded external and
%% the new external undecoded, i.e. {Decoded, Rest}.
%% Type (= normal | hidden) does not make any difference here as
%% a hidden node never sends cached atoms.
%% --------------------------------------------------------------

from_external([?EXTERN_TAG|ExtForm], Node, _Type) ->
    {NewExt, RestExtForm} = decode(ExtForm, Node, []),
    {binary_to_term(list_to_binary([?EXTERN_TAG|NewExt])), RestExtForm}.

decode([?SMALL_INTEGER,Int|T], Node, Ext) ->
    decode(T, Node, Ext ++ [?SMALL_INTEGER,Int]);
decode([?INTEGER,I1,I2,I3,I4|T], Node, Ext) ->
    decode(T, Node, Ext ++ [?INTEGER,I1,I2,I3,I4]);
decode([?FLOAT|T0], Node, Ext) ->
    Float = lists:sublist(T0, 31),
    T = lists:sublist(T0, 32, length(T0) - 31),
    decode(T, Node, Ext ++ [?FLOAT|Float]);
decode([?REF|T0], Node, Ext) ->
    {NodeRef, T1} = decode_atom(T0, Node),
    [Id1,Id2,Id3,Id4,Creation|T] = T1,
    decode(T, Node, Ext ++ ([?REF|NodeRef] ++ [Id1,Id2,Id3,Id4,Creation]));
decode([?PORT|T0], Node, Ext) ->
    {NodeRef, T1} = decode_atom(T0, Node),
    [Id1,Id2,Id3,Id4,Creation|T] = T1,
    decode(T, Node, Ext ++ ([?PORT|NodeRef] ++ [Id1,Id2,Id3,Id4,Creation]));
decode([?PID|T0], Node, Ext) ->
    {NodeRef, T1} = decode_atom(T0, Node),
    [Id1,Id2,Id3,Id4,S1,S2,S3,S4,Creation|T] = T1,
    decode(T, Node,
	   Ext ++ ([?PID|NodeRef] ++ [Id1,Id2,Id3,Id4,S1,S2,S3,S4,Creation]));
decode([?SMALL_TUPLE,A|T], Node, Ext) ->
    decode(T, Node, Ext ++ [?SMALL_TUPLE,A]);
decode([?LARGE_TUPLE,A1,A2,A3,A4|T], Node, Ext) ->
    decode(T, Node, Ext ++ [?LARGE_TUPLE,A1,A2,A3,A4]);
decode([?NIL|T], Node, Ext) ->
    decode(T, Node, Ext ++ [?NIL]);
decode([?STRING, L1, L0|T0], Node, Ext) ->
    Len = ?i16(L1, L0),
    Str = lists:sublist(T0, Len),
    T = lists:sublist(T0, Len + 1, length(T0) - Len),
    decode(T, Node, Ext ++ [?STRING, L1, L0|Str]);
decode([?LIST,A1,A2,A3,A4|T], Node, Ext) ->
    decode(T, Node, Ext ++ [?LIST,A1,A2,A3,A4]);
decode([?BINARY, L3, L2, L1, L0|T0], Node, Ext) ->
    Len = ?i32(L3, L2, L1, L0),
    Str = lists:sublist(T0, Len),
    T = lists:sublist(T0, Len + 1, length(T0) - Len),
    decode(T, Node, Ext ++ [?BINARY, L3, L2, L1, L0|Str]);
decode([?SMALL_BIG, Len, Sign|T0], Node, Ext) ->
    Big = lists:sublist(T0, Len),
    T = lists:sublist(T0, Len + 1, length(T0) - Len),
    decode(T, Node, Ext ++ [?SMALL_BIG, Len, Sign|Big]);
decode([?LARGE_BIG, L3, L2, L1, L0, Sign|T0], Node, Ext) ->
    Len = ?i32(L3, L2, L1, L0),
    Big = lists:sublist(T0, Len),
    T = lists:sublist(T0, Len + 1, length(T0) - Len),
    decode(T, Node, Ext ++ [?LARGE_BIG, L3, L2, L1, L0, Sign|Big]);
decode([], _, Ext) ->
    {Ext, []};
decode([?EXTERN_TAG|Rest], _, Ext) ->
    {Ext, [?EXTERN_TAG|Rest]};
decode(T0, Node, Ext) ->
    {Atom, T} = decode_atom(T0, Node),
    decode(T, Node, Ext ++ Atom).

decode_atom([?ATOM, L1, L0|T0], _Node) ->
    Len = ?i16(L1, L0),
    Atom = lists:sublist(T0, Len),
    T = lists:sublist(T0, Len + 1, length(T0) - Len),
    {[?ATOM, L1, L0|Atom], T};
decode_atom([?NEW_CACHE, I, L1, L0|T0], Node) ->
    Len = ?i16(L1, L0),
    Atom = lists:sublist(T0, Len),
    T = lists:sublist(T0, Len + 1, length(T0) - Len),
    erl_atom_cache:insert(I, Node, Atom),
    {[?ATOM, L1, L0|Atom], T};
decode_atom([?CACHED_ATOM,I|T], Node) ->
    Atom = erl_atom_cache:fetch(I, Node),
    [L1, L0] = ?int16(length(Atom)),
    {[?ATOM, L1, L0|Atom], T}.

%% --------------------------------------------------------------
%% Convert a term encoded in external format into a local term.
%% --------------------------------------------------------------

to_external(Term, Node) ->
    to_external(Term, Node, node(), normal).

to_external(Term, Node, Type) ->
    to_external(Term, Node, node(), Type).

to_external(Term, Node, MyNode0, normal) ->
    MyNode = atom_to_list(MyNode0),
    to_external1(binary_to_list(term_to_binary(Term)), Node, MyNode);
to_external(Term, _, _, hidden) ->
    binary_to_list(term_to_binary(Term)).

to_external1([?EXTERN_TAG|ExtForm], Node, MyNode) ->
    NewExt = encode(ExtForm, Node, MyNode),
    [?EXTERN_TAG|NewExt].

encode([?SMALL_INTEGER,Int|T], Node, MyNode) ->
    [?SMALL_INTEGER,Int|encode(T, Node, MyNode)];
encode([?INTEGER,I1,I2,I3,I4|T], Node, MyNode) ->
    [?INTEGER,I1,I2,I3,I4|encode(T, Node, MyNode)];
encode([?FLOAT|T0], Node, MyNode) ->
    Float = lists:sublist(T0, 31),
    T = lists:sublist(T0, 32, length(T0) - 31),
    [?FLOAT|Float] ++ encode(T, Node, MyNode);
encode([?REF|T0], Node, MyNode) ->
    {NodeRef, T1} = encode_atom(T0, Node, MyNode),
    [Id1,Id2,Id3,Id4,Creation|T] = T1,
    [?REF|NodeRef] ++ [Id1,Id2,Id3,Id4,Creation|encode(T, Node, MyNode)];
encode([?PORT|T0], Node, MyNode) ->
    {NodeRef, T1} = encode_atom(T0, Node, MyNode),
    [Id1,Id2,Id3,Id4,Creation|T] = T1,
    [?PORT|NodeRef] ++ [Id1,Id2,Id3,Id4,Creation|encode(T, Node, MyNode)];
encode([?PID|T0], Node, MyNode) ->
    {NodeRef, T1} = encode_atom(T0, Node, MyNode),
    [Id1,Id2,Id3,Id4,S1,S2,S3,S4,Creation|T] = T1,
    [?PID|NodeRef] ++ [Id1,Id2,Id3,Id4,S1,S2,S3,S4,Creation|encode(T, Node, MyNode)];
encode([?SMALL_TUPLE,A|T], Node, MyNode) ->
    [?SMALL_TUPLE,A|encode(T, Node, MyNode)];
encode([?LARGE_TUPLE,A1,A2,A3,A4|T], Node, MyNode) ->
    [?LARGE_TUPLE,A1,A2,A3,A4|encode(T, Node, MyNode)];
encode([?NIL|T], Node, MyNode) ->
    [?NIL|encode(T, Node, MyNode)];
encode([?STRING, L1, L0|T0], Node, MyNode) ->
    Len = ?i16(L1, L0),
    Str = lists:sublist(T0, Len),
    T = lists:sublist(T0, Len + 1, length(T0) - Len),
    [?STRING, L1, L0|Str] ++ encode(T, Node, MyNode);
encode([?LIST,A1,A2,A3,A4|T], Node, MyNode) ->
    [?LIST,A1,A2,A3,A4|encode(T, Node, MyNode)];
encode([?BINARY, L3, L2, L1, L0|T0], Node, MyNode) ->
    Len = ?i32(L3, L2, L1, L0),
    Str = lists:sublist(T0, Len),
    T = lists:sublist(T0, Len + 1, length(T0) - Len),
    [?BINARY, L3, L2, L1, L0|Str] ++ encode(T, Node, MyNode);
encode([?SMALL_BIG, Len, Sign|T0], Node, MyNode) ->
    Big = lists:sublist(T0, Len),
    T = lists:sublist(T0, Len + 1, length(T0) - Len),
    [?SMALL_BIG, Len, Sign|Big] ++ encode(T, Node, MyNode);
encode([?LARGE_BIG, L3, L2, L1, L0, Sign|T0], Node, MyNode) ->
    Len = ?i32(L3, L2, L1, L0),
    Big = lists:sublist(T0, Len),
    T = lists:sublist(T0, Len + 1, length(T0) - Len),
    [?LARGE_BIG, L3, L2, L1, L0, Sign|Big] ++ encode(T, Node, MyNode);
encode([], _, _) ->
    [];
encode(T0, Node, MyNode) ->
    {Atom, T} = encode_atom(T0, Node, MyNode),
    Atom ++ encode(T, Node, MyNode).

encode_atom([?ATOM, L01, L00|T0], Node, MyNode) ->
    {L1, L0, Atom, T} = patch_nonode(L01, L00, T0, MyNode),
    case erl_atom_cache:lookup(Atom, Node) of
	{ok, Index} ->
	    {[?CACHED_ATOM,Index],T};
	_ ->
	    Index = create_index(Atom),
	    erl_atom_cache:insert(Atom, Node, Index),
	    {[?NEW_CACHE, Index, L1, L0|Atom], T}
    end.

patch_nonode(L01, L00, T0, MyNode) ->
    Len = ?i16(L01, L00),
    Atom = lists:sublist(T0, Len),
    T = lists:sublist(T0, Len + 1, length(T0) - Len),
    case Atom of
	"nonode@nohost" ->
	    [L1, L0] = ?int16(length(MyNode)),
	    {L1, L0, MyNode, T};
	_ ->
	    {L01, L00, Atom, T}
    end.

%%
%% If new index points to an occuppied slot, overwrite that slot !
%%
create_index(Atom) ->
    erlang:hash(Atom, ?HASH_RANGE).



