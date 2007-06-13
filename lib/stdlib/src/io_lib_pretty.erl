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
-module(io_lib_pretty).

%%% Pretty printing Erlang terms
%%%
%%% In this module "print" means the formatted printing while "write"
%%% means just writing out onto one line.

-export([print/1,print/2,print/3,print/4,print/5]).

%%%
%%% Exported functions
%%%

%% print(Term) -> [Chars]
%% print(Term, Column, LineLength, Depth) -> [Chars]
%% Depth = -1 gives unlimited print depth. Use io_lib:write for atomic terms.

print(Term) ->
    print(Term, 1, 80, -1).

%% print(Term, RecDefFun) -> [Chars]
%% print(Term, Depth, RecDefFun) -> [Chars]
%% RecDefFun = fun(Tag, NoFields) -> [FieldTag] | no
%% Used by the shell for printing records.
print(Term, RecDefFun) ->
    print(Term, -1, RecDefFun).

print(Term, Depth, RecDefFun) ->
    print(Term, 1, 80, Depth, RecDefFun).

print(Term, Col, Ll, D) ->
    print(Term, Col, Ll, D, no_fun).

print(_, _, _, 0, _RF) -> "...";
print(Term, Col, Ll, D, RecDefFun) when Col =< 0 ->
    print(Term, 1, Ll, D, RecDefFun);
print(Term, Col, Ll, D, RecDefFun) when is_tuple(Term); 
                                        is_list(Term) ->
    If = {_S, Len} = print_length(Term, D, RecDefFun),
    if
        Len < Ll - Col ->
            write(If);
        true ->
            TInd = while_fail([-1, 4], fun(I) -> cind(If, Col, Ll, I) end, 1),
            pp(If, Col, Ll, TInd, indent(Col), 0)
    end;
print(<<_/bitstr>>=Term, Col, Ll, D, RecDefFun) ->
    If = {_S, Len} = print_length(Term, D, RecDefFun),
    if
        Len < Ll - Col ->
            write(If);
        true ->
            TInd = while_fail([-1, 4], fun(I) -> cind(If, Col, Ll, I) end, 1),
            pp(If, Col, Ll, TInd, indent(Col), 0)
    end;
print(Term, _Col, _Ll, _D, _RF) ->
    io_lib:write(Term).

%%%
%%% Local functions
%%%

pp({_S, Len} = If, Col, Ll, _TInd, _Ind, LD) when Len < Ll - Col - LD ->
    write(If);
pp({{list,L}, _Len}, Col, Ll, TInd, Ind, LD) ->
    [$[, pp_list(L, Col + 1, Ll, TInd, indent(1, Ind), LD, $|), $]];
pp({{tuple,true,L}, _Len}, Col, Ll, TInd, Ind, LD) ->
    [${, pp_tag_tuple(L, Col, Ll, TInd, Ind, LD), $}];
pp({{tuple,false,L}, _Len}, Col, Ll, TInd, Ind, LD) ->
    [${, pp_list(L, Col + 1, Ll, TInd, indent(1, Ind), LD, $,), $}];
pp({{record,[{Name,NLen} | L]}, _Len}, Col, Ll, TInd, Ind, LD) ->
    [Name, ${, pp_record(L, NLen, Col, Ll, TInd, Ind, LD), $}];
pp({{bin,S}, _Len}, Col, Ll, _TInd, Ind, LD) ->
    pp_binary(S, Col + 2, Ll, indent(2, Ind), LD);
pp({S, _Len}, _Col, _Ll, _TInd, _Ind, _LD) ->
    S.

%%  Print a tagged tuple by indenting the rest of the elements
%%  differently to the tag. Tuple has size >= 2.
pp_tag_tuple([{Tag,Tlen} | L], Col, Ll, TInd, Ind, LD) ->
    TagInd = Tlen + 2,
    Tcol = Col + TagInd,
    S = $,,
    if
        TInd > 0, TagInd > TInd ->
            [Tag | pp_tail(L, Col+TInd, Ll, TInd, indent(TInd, Ind), LD, S)];
        true ->
            [Tag, S | pp_list(L, Tcol, Ll, TInd, indent(TagInd, Ind), LD, S)]
    end.

pp_record([], _Nlen, _Col, _Ll, _TInd, _Ind, _LD) ->
    "";
pp_record({dots, _}, _Nlen, _Col, _Ll, _TInd, _Ind, _LD) ->
    "...";
pp_record([F | Fs], Nlen, Col0, Ll, TInd, Ind0, LD) ->
    Nind = Nlen + 1,
    {Col, Ind, S} = rec_indent(Nind, TInd, Col0, Ind0),
    [S, pp_field(F, Col, Ll, TInd, Ind, last_depth(Fs, LD)) | 
     pp_fields_tail(Fs, Col, Ll, TInd, Ind, LD)].    

pp_fields_tail([], _Col, _Ll, _TInd, _Ind, _LD) ->
    "";
pp_fields_tail({dots, _}, _Col, _Ll, _TInd, _Ind, _LD) ->
    ",...";
pp_fields_tail([F | Fs], Col, Ll, TInd, Ind, LD) ->
    [$,, $\n, Ind, pp_field(F, Col, Ll, TInd, Ind, last_depth(Fs, LD)) |
     pp_fields_tail(Fs, Col, Ll, TInd, Ind, LD)].

pp_field({_,Len}=Fl, Col, Ll, _TInd, _Ind, LD) when Len < Ll - Col - LD ->
    write_field(Fl);
pp_field({{field, Name, NameL, F}, _Len}, Col0, Ll, TInd, Ind0, LD) ->
    {Col, Ind, S} = rec_indent(NameL, TInd, Col0, Ind0),
    [Name, " = ", S | pp(F, Col, Ll, TInd, Ind, LD)].

rec_indent(RInd, TInd, Col0, Ind0) ->
    Nl = (TInd > 0) and (RInd > TInd),
    DCol = case Nl of
               true -> TInd;
               false -> RInd
           end,
    Col = Col0 + DCol,
    Ind = indent(DCol, Ind0),
    S = case Nl of
            true -> [$\n | Ind];
            false -> ""
        end,
    {Col, Ind, S}.

pp_list({dots, _}, _Col, _Ll, _TInd, _Ind, _LD, _S) ->
    "...";
pp_list([E | Es], Col, Ll, TInd, Ind, LD, S) ->
    [pp(E, Col, Ll, TInd, Ind, last_depth(Es, LD)) | 
     pp_tail(Es, Col, Ll, TInd, Ind, LD, S)].

pp_tail([], _Col, _Ll, _TInd, _Ind, _LD, _S) ->
    "";
pp_tail([E | Es], Col, Ll, TInd, Ind, LD, S) ->
    [$,, $\n, Ind, pp(E, Col, Ll, TInd, Ind, last_depth(Es, LD)) | 
     pp_tail(Es, Col, Ll, TInd, Ind, LD, S)];
pp_tail({dots, _}, _Col, _Ll, _TInd, _Ind, _LD, S) ->
    [S | "..."];
pp_tail(E, Col, Ll, TInd, Ind, LD, S) ->
    [S, $\n, Ind | pp(E, Col, Ll, TInd, Ind, LD + 1)].

last_depth([_ | _], _LD) ->
    0;
last_depth(_, LD) ->
    LD + 1.

%% Reuse the list created by io_lib:write_binary()...
pp_binary([LT,LT,S,GT,GT], Col, Ll, Ind, LD) ->
    N = max(8, Ll - Col - LD),
    [LT,LT,pp_binary(S, N, N, Ind),GT,GT].

pp_binary([BS, $, | S], N, N0, Ind) ->
    Len = length(BS) + 1,
    case N - Len of
        N1 when N1 < 0 ->
            [$\n, Ind, BS, $, | pp_binary(S, N0 - Len, N0, Ind)];
        N1 ->
            [BS, $, | pp_binary(S, N1, N0, Ind)]
    end;
pp_binary([BS1, $:, BS2]=S, N, _N0, Ind) 
         when length(BS1) + length(BS2) + 1 > N ->
    [$\n, Ind, S];
pp_binary(S, N, _N0, Ind) when length(S) > N ->
    [$\n, Ind, S];
pp_binary(S, _N, _N0, _Ind) ->
    S.

write({{tuple, _IsTagged, L}, _}) ->
    [${, write_list(L, $,), $}];
write({{list, L}, _}) ->
    [$[, write_list(L, $|), $]];
write({{record, [{Name,_} | L]}, _}) ->
    [Name, ${, write_fields(L), $}];
write({{bin, S}, _}) ->
    S;
write({S, _}) ->
    S.

write_fields([]) ->
    "";
write_fields({dots, _}) ->
    "...";
write_fields([F | Fs]) ->
    [write_field(F) | write_fields_tail(Fs)].

write_fields_tail([]) ->
    "";
write_fields_tail({dots, _}) ->
    ",...";
write_fields_tail([F | Fs]) ->
    [$,, write_field(F) | write_fields_tail(Fs)].

write_field({{field, Name, _NameL, F}, _}) ->
    [Name, " = " | write(F)].

write_list({dots, _}, _S) ->
    "...";
write_list([E | Es], S) ->
    [write(E) | write_tail(Es, S)].

write_tail([], _S) ->
    [];
write_tail([E | Es], S) ->
    [$,, write(E) | write_tail(Es, S)];
write_tail({dots, _}, S) ->
    [S | "..."];
write_tail(E, S) ->
    [S | write(E)].

%% The depth (D) is used for extracting and counting the characters to
%% print. The structure is kept so that the returned intermediate
%% format can be formatted. The separators (list, tuple, record) are
%% counted but need to be added later.

%% D =/= 0
print_length([], _D, _RF) ->
    {"[]", 2};
print_length({}, _D, _RF) ->
    {"{}", 2};
print_length(List, D, RF) when is_list(List) ->
    case printable_list(List, D) of
        true ->
            S = io_lib:write_string(List, $"), %"
            {S, length(S)};
        %% Truncated lists could break some existing code.
        % {true, Prefix} ->
        %    S = io_lib:write_string(Prefix, $"), %"
        %    {[S | "..."], 3 + length(S)};
        false ->
            print_length_list(List, D, RF)
    end;
print_length(Fun, _D, _RF) when is_function(Fun) ->
    S = io_lib:write(Fun),
    {S, iolist_size(S)};
print_length(R, D, RF) when is_atom(element(1, R)), 
                            is_function(RF) ->
    case RF(element(1, R), size(R) - 1) of
        no -> 
            print_length_tuple(R, D, RF);
        RDefs ->
            print_length_record(R, D, RF, RDefs)
    end;
print_length(Tuple, D, RF) when is_tuple(Tuple) ->
    print_length_tuple(Tuple, D, RF);
print_length(<<>>, _D, _RF) ->
    {"<<>>", 4};
print_length(<<_/bitstr>>, 1, _RF) ->
    {"<<...>>", 7};
print_length(<<_/bitstr>>=Bin, D, _RF) ->
    case erlang:bitsize(Bin) rem 8 of
        0 ->
	    D1 = D - 1, 
	    case printable_bin(Bin, D1) of
	        List when is_list(List) ->
                    S = io_lib:write_string(List, $"),					
	            {[$<,$<,S,$>,$>], 4 + length(S)};
	        {true, Prefix} -> 
	            S = io_lib:write_string(Prefix, $"), 
	            {[$<,$<, S | "...>>"], 4 + length(S)};
	        false ->
	            S = io_lib:write(Bin, D),
	            {{bin,S}, iolist_size(S)}
	    end;
        _ ->
            S = io_lib:write(Bin, D),
	   {{bin,S}, iolist_size(S)}
    end;    
print_length(Term, _D, _RF) ->
    S = io_lib:write(Term),
    {S, iolist_size(S)}.

print_length_tuple(_Tuple, 1, _RF) ->
    {"{...}", 5};
print_length_tuple(Tuple, D, RF) ->
    L = print_length_list1(tuple_to_list(Tuple), D, RF),
    IsTagged = is_atom(element(1, Tuple)) and (size(Tuple) > 1),
    {{tuple,IsTagged,L}, list_length(L, 2)}.

print_length_record(_Tuple, 1, _RF, _RDefs) ->
    {"{...}", 5};
print_length_record(Tuple, D, RF, RDefs) ->
    Name = [$# | io_lib:write_atom(element(1, Tuple))],
    NameL = length(Name),
    L = print_length_fields(RDefs, D - 1, tl(tuple_to_list(Tuple)), RF),
    {{record, [{Name,NameL} | L]}, list_length(L, NameL + 2)}.

print_length_fields([], _D, [], _RF) ->
    [];
print_length_fields(_, 1, _, _RF) -> 
    {dots, 3};
print_length_fields([Def | Defs], D, [E | Es], RF) ->
    [print_length_field(Def, D - 1, E, RF) | 
     print_length_fields(Defs, D - 1, Es, RF)].

print_length_field(Def, D, E, RF) ->
    Name = io_lib:write_atom(Def),
    {S, L} = print_length(E, D, RF),
    NameL = length(Name) + 3,
    {{field, Name, NameL, {S, L}}, NameL + L}.

print_length_list(List, D, RF) -> 
    L = print_length_list1(List, D, RF),
    {{list, L}, list_length(L, 2)}.

print_length_list1([], _D, _RF) ->
    [];
print_length_list1(_, 1, _RF) ->
    {dots, 3};
print_length_list1([E | Es], D, RF) ->
    [print_length(E, D - 1, RF) | print_length_list1(Es, D - 1, RF)];
print_length_list1(E, D, RF) ->
    print_length(E, D - 1, RF).

list_length([], Acc) ->
    Acc;
list_length([{_, Len} | Es], Acc) ->
    list_length_tail(Es, Acc + Len);
list_length({_, Len}, Acc) ->
    Acc + Len.

list_length_tail([], Acc) ->
    Acc;
list_length_tail([{_,Len} | Es], Acc) ->
    list_length_tail(Es, Acc + 1 + Len);
list_length_tail({_, Len}, Acc) ->
    Acc + 1 + Len.

%% ?CHARS printable characters has depth 1.
-define(CHARS, 4).

printable_list(L, D) when D < 0 ->
    io_lib:printable_list(L);
printable_list(_L, 1) ->
    false;
printable_list(L, _D) ->
    io_lib:printable_list(L).
%% Truncated lists could break some existing code.
% printable_list(L, D) ->
%     Len = ?CHARS * (D - 1),
%     case printable_list1(L, Len) of
%         all ->
%             true;
%         N when is_integer(N), Len - N >= D - 1 ->
%             {L1, _} = lists:split(Len - N, L),
%             {true, L1};
%         N when is_integer(N) ->
%             false
%     end.

printable_bin(Bin, D) when D >= 0, ?CHARS * D =< size(Bin) ->
    printable_bin(Bin, min(?CHARS * D, size(Bin)), D);
printable_bin(Bin, D) ->
    printable_bin(Bin, size(Bin), D).

printable_bin(Bin, Len, D) ->
    N = min(20, Len),
    L = binary_to_list(Bin, 1, N),
    case printable_list1(L, N) of
        all when N =:= size(Bin)  ->
            L;
        all when N =:= Len -> % N < size(Bin)
            {true, L};
        all ->
            case printable_bin1(Bin, 1 + N, Len - N) of
                0 when size(Bin) =:= Len ->
                    binary_to_list(Bin);
                NC when D > 0, Len - NC >= D ->
                    {true, binary_to_list(Bin, 1, Len - NC)};
                NC when is_integer(NC) ->
                    false
            end;
        NC when is_integer(NC), D > 0, N - NC >= D ->
            {true, binary_to_list(Bin, 1, N - NC)};
        NC when is_integer(NC) ->
            false
    end.

printable_bin1(_Bin, _Start, 0) ->
    0;
printable_bin1(Bin, Start, Len) ->
    N = min(10000, Len),
    L = binary_to_list(Bin, Start, Start + N - 1),
    case printable_list1(L, N) of
        all ->
            printable_bin1(Bin, Start + N, Len - N);
        NC when is_integer(NC) ->
            Len - (N - NC)
    end.

%% -> all | integer() >=0. Adopted from io_lib.erl.
% printable_list1([_ | _], 0) -> 0;
printable_list1([C | Cs], N) when is_integer(C), C >= $\s, C =< $~ ->
    printable_list1(Cs, N - 1);
printable_list1([C | Cs], N) when is_integer(C), C >= $\240, C =< $\377 ->
    printable_list1(Cs, N - 1);
printable_list1([$\n | Cs], N) -> printable_list1(Cs, N - 1);
printable_list1([$\r | Cs], N) -> printable_list1(Cs, N - 1);
printable_list1([$\t | Cs], N) -> printable_list1(Cs, N - 1);
printable_list1([$\v | Cs], N) -> printable_list1(Cs, N - 1);
printable_list1([$\b | Cs], N) -> printable_list1(Cs, N - 1);
printable_list1([$\f | Cs], N) -> printable_list1(Cs, N - 1);
printable_list1([$\e | Cs], N) -> printable_list1(Cs, N - 1);
printable_list1([], _) -> all;
printable_list1(_, N) -> N.

%% Throw 'no_good' if the indentation exceeds half the line length.

cind({_S, Len}, Col, Ll, Ind) when Len < Ll - Col ->
    Ind;
cind({{list,L}, _Len}, Col, Ll, Ind) ->
    cind_list(L, Col + 1, Ll, Ind);
cind({{tuple,true,L}, _Len}, Col, Ll, Ind) ->
    cind_tag_tuple(L, Col, Ll, Ind);
cind({{tuple,false,L}, _Len}, Col, Ll, Ind) ->
    cind_list(L, Col + 1, Ll, Ind);
cind({{record,[{_Name,NLen} | L]}, _Len}, Col, Ll, Ind) ->
    cind_record(L, NLen, Col, Ll, Ind);
cind({{bin,_S}, _Len}, _Col, _Ll, Ind) ->
    Ind;
cind({_S, _Len}, _Col, _Ll, Ind) ->
    Ind.

cind_tag_tuple([{_Tag,Tlen} | L], Col, Ll, Ind) ->
    Tind = Tlen + 2,
    Tcol = Col + Tind,
    if
        Ind > 0, Tind > Ind ->
            Tcoli = Col + Ind,
            if
                Tcoli > Ll div 2 ->
                    throw(no_good);
                true ->  
                    cind_tail(L, Tcoli, Ll, Ind)
            end;
        Tcol >= Ll div 2 ->
            throw(no_good);
        true ->
            cind_list(L, Tcol, Ll, Ind)
    end.

cind_record([F | Fs], Nlen, Col0, Ll, Ind) ->
    Nind = Nlen + 1,
    Col = cind_tag(Nind, Col0, Ll, Ind),
    cind_field(F, Col, Ll, Ind),
    cind_fields_tail(Fs, Col, Ll, Ind);
cind_record(_, _Nlen, _Col, _Ll, Ind) ->
    Ind.

cind_fields_tail([F | Fs], Col, Ll, Ind) ->
    cind_field(F, Col, Ll, Ind),
    cind_fields_tail(Fs, Col, Ll, Ind);
cind_fields_tail(_, _Col, _Ll, Ind) ->
    Ind.

cind_field({{field, _N, _NL, _F}, Len}, Col, Ll, Ind) when Len < Ll - Col ->
    Ind;
cind_field({{field, _Name, NameL, F}, _Len}, Col0, Ll, Ind) ->
    Col = cind_tag(NameL, Col0, Ll, Ind),
    cind(F, Col, Ll, Ind).

cind_tag(TInd, Col0, Ll, Ind) ->
    DCol = if
               Ind > 0, TInd > Ind ->
                   Ind;
               true ->
                   TInd
           end,
    Col = Col0 + DCol,
    if
        Col > Ll div 2 ->
            throw(no_good);
        true ->
            Col
    end.

cind_list({dots, _}, _Col, _Ll, Ind) ->
    Ind;
cind_list([E | Es], Col, Ll, Ind) ->
    cind(E, Col, Ll, Ind),
    cind_tail(Es, Col, Ll, Ind).

cind_tail([], _Col, _Ll, Ind) ->
    Ind;
cind_tail([E | Es], Col, Ll, Ind) ->
    cind(E, Col, Ll, Ind),
    cind_tail(Es, Col, Ll, Ind);
cind_tail({dots, _}, _Col, _Ll, Ind) ->
    Ind;
cind_tail(E, Col, Ll, Ind) ->
    cind(E, Col, Ll, Ind).

while_fail([], _F, V) ->
    V;
while_fail([A | As], F, V) ->
    try F(A) catch _ -> while_fail(As, F, V) end.

min(X, Y) when X =< Y -> X;
min(_X, Y) -> Y.

max(X, Y) when X >= Y -> X;
max(_X, Y) -> Y.

indent(N) when is_integer(N), N > 0 ->
    chars($\s, N-1).

indent(1, Ind) -> % Optimization of common case
    [$\s | Ind];
indent(4, Ind) -> % Optimization of common case
    S2 = [$\s, $\s],
    [S2, S2 | Ind];
indent(N, Ind) when is_integer(N), N > 0 ->
    [chars($\s, N) | Ind].

%% A deep version of string:chars/2
chars(_C, 0) ->
    [];
chars(C, 2) ->
    [C, C];
chars(C, 3) ->
    [C, C, C];
chars(C, N) when (N band 1) =:= 0 ->
    S = chars(C, N bsr 1),
    [S | S];
chars(C, N) ->
    S = chars(C, N bsr 1),
    [C, S | S].
