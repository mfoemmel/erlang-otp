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
-module(string).

-export([len/1,equal/2,concat/2,chr/2,rchr/2,str/2,rstr/2,index/2,
	 span/2,cspan/2,substr/2,substr/3,tokens/2,chars/2,chars/3]).
-export([copies/2,words/1,words/2,strip/1,strip/2,strip/3,index/2,
	 sub_word/2,sub_word/3,left/2,left/3,right/2,right/3,
	 sub_string/2,sub_string/3,centre/2,centre/3]).
-export([re_sh_to_awk/1,re_parse/1,re_match/2,re_sub/3,re_gsub/3,re_split/2]).

-import(lists,[reverse/1,member/2]).

%% Robert's bit

%% len(String)
%%  Return the length of a string.

len(S) -> length(S).

%% equal(String1, String2)
%%  Test if 2 strings are equal.

equal(S, S) -> true;
equal(_, _) -> false.

%% concat(String1, String2)
%%  Concatenate 2 strings.

concat(S1, S2) -> S1 ++ S2.

%% chr(String, Char)
%% rchr(String, Char)
%%  Return the first/last index of the character in a string.

chr(S, C) -> chr(S, C, 1).

chr([C|Cs], C, I) -> I;
chr([_|Cs], C, I) -> chr(Cs, C, I+1);
chr([], C, I) -> 0.

rchr(S, C) -> rchr(S, C, 1, 0).

rchr([C|Cs], C, I, L) ->			%Found one, now find next!
    rchr(Cs, C, I+1, I);
rchr([_|Cs], C, I, L) ->
    rchr(Cs, C, I+1, L);
rchr([], C, I, L) -> L.

%% str(String, SubString)
%% rstr(String, SubString)
%% index(String, SubString)
%%  Return the first/last index of the sub-string in a string.
%%  index/2 is kept for backwards compatibility.

str(S, Sub) -> str(S, Sub, 1).

str([C|S], [C|Sub], I) ->
    case prefix(Sub, S) of
	true -> I;
	false -> str(S, [C|Sub], I+1)
    end;
str([_|S], Sub, I) -> str(S, Sub, I+1);
str([], Sub, I) -> 0.

rstr(S, Sub) -> rstr(S, Sub, 1, 0).

rstr([C|S], [C|Sub], I, L) ->
    case prefix(Sub, S) of
	true -> rstr(S, [C|Sub], I+1, I);
	false -> rstr(S, [C|Sub], I+1, L)
    end;
rstr([_|S], Sub, I, L) -> rstr(S, Sub, I+1, L);
rstr([], Sub, I, L) -> L.

prefix([C|Pre], [C|String]) -> prefix(Pre, String);
prefix([], String) -> true;
prefix(Pre, String) -> false.

index(S, Sub) -> str(S, Sub).

%% span(String, Chars) -> Length.
%% cspan(String, Chars) -> Length.

span(S, Cs) -> span(S, Cs, 0).

span([C|S], Cs, I) ->
    case member(C, Cs) of
	true -> span(S, Cs, I+1);
	false -> I
    end;
span([], Cs, I) -> I.

cspan(S, Cs) -> cspan(S, Cs, 0).

cspan([C|S], Cs, I) ->
    case member(C, Cs) of
	true -> I;
	false -> cspan(S, Cs, I+1)
    end;
cspan([], Cs, I) -> I.

%% substr(String, Start)
%% substr(String, Start, Length)
%%  Extract a sub-string from String.

substr([_|String], S) when S > 1 -> substr(String, S-1);
substr(String, 1) -> String.

substr(String, S, L) when L >= 0 ->
    substr1(substr(String, S), L).

substr1([C|String], L) when L > 0 -> [C|substr1(String, L-1)];
substr1(String, L) -> [].			%Be nice!

%% tokens(String, Seperators).
%%  Return a list of tokens seperated by characters in Seperators.

tokens(S, Seps) ->
    tokens1(S, Seps, []).

tokens1([C|S], Seps, Toks) ->
    case member(C, Seps) of
	true -> tokens1(S, Seps, Toks);
	false -> tokens2(S, Seps, Toks, [C])
    end;
tokens1([], Seps, Toks) ->
    reverse(Toks).

tokens2([C|S], Seps, Toks, Cs) ->
    case member(C, Seps) of
	true -> tokens1(S, Seps, [reverse(Cs)|Toks]);
	false -> tokens2(S, Seps, Toks, [C|Cs])
    end;
tokens2([], Seps, Toks, Cs) ->
    reverse([reverse(Cs)|Toks]).

chars(C, N) -> chars(C, N, []).

chars(C, N, Tail) when N > 0 ->
    [C|chars(C, N-1, Tail)];
chars(C, 0, Tail) ->
    Tail.

%% Torbjörn's bit.

%%% COPIES %%%

copies(_, 0) -> [];
copies(CharList, Num) ->
    CharList ++ copies(CharList, Num-1).

%%% WORDS %%%

words(String) -> words(String, $\s).

words(String, Char) when integer(Char) ->
    w_count(strip(String, both, Char), Char, 0).

w_count([], _, Num) -> Num+1;
w_count([H|T], H, Num) -> w_count(strip(T, left, H), H, Num+1);
w_count([H|T], Char, Num) -> w_count(T, Char, Num).

%%% SUB_WORDS %%%

sub_word(String, Index) -> sub_word(String, Index, $\s).

sub_word(String, Index, Char) when integer(Index), integer(Char) ->
    case words(String, Char) of
	Num when Num < Index ->
	    [];
	Num ->
	    s_word(strip(String, left, Char), Index, Char, 1, [])
    end.

s_word([], _, _, _,Res) -> reverse(Res);
s_word([Char|T],Index,Char,Index,Res) -> reverse(Res);
s_word([H|T],Index,Char,Index,Res) -> s_word(T,Index,Char,Index,[H|Res]);
s_word([Char|T],Stop,Char,Index,Res) when Index < Stop -> 
    s_word(strip(T,left,Char),Stop,Char,Index+1,Res);
s_word([H|T],Stop,Char,Index,Res) when Index < Stop -> 
    s_word(T,Stop,Char,Index,Res).

%%% STRIP %%%

strip(String) -> strip(String, both).

strip(String, left) -> strip_left(String, $\s);
strip(String, right) -> strip_right(String, $\s);
strip(String, both) ->
    strip_right(strip_left(String, $\s), $\s).

strip(String, right, Char) -> strip_right(String, Char);
strip(String, left, Char) -> strip_left(String, Char);
strip(String, both, Char) ->
    strip_right(strip_left(String, Char), Char).

strip_left([Sc|S], Sc) ->
    strip_left(S, Sc);
strip_left([C|S], Sc) -> [C|S];
strip_left([], Sc) -> [].

strip_right([Sc|S], Sc) ->
    strip_right_1(S, Sc, 1);
strip_right([C|S], Sc) ->
    [C|strip_right(S, Sc)];
strip_right([], Sc) ->
    [].

strip_right_1([Sc|S], Sc, Scn) ->
    strip_right_1(S, Sc, Scn+1);
strip_right_1([C|S], Sc, Scn) ->
    chars(Sc, Scn, [C|strip_right(S, Sc)]);
strip_right_1([], Sc, Scn) ->
    [].

%%% LEFT %%%

left(String, Len) -> left(String, Len, $\s).

left(String, Len, Char) when integer(Char) ->
    Slen = length(String),
    if
	Slen > Len -> substr(String, 1, Len);
	Slen < Len -> l_pad(String, Len-Slen, Char);
	Slen == Len -> String
    end.

l_pad(String, Num, Char) -> String ++ chars(Char, Num).

%%% RIGHT %%%

right(String, Len) -> right(String, Len, $\s).

right(String, Len, Char) when integer(Char) ->
    Slen = length(String),
    if
	Slen > Len -> substr(String, Slen-Len+1);
	Slen < Len -> r_pad(String, Len-Slen, Char);
	Slen == Len -> String
    end.

r_pad(String, Num, Char) -> chars(Char, Num, String).

%%% CENTRE %%%

centre(String, Len) -> centre(String ,Len, $\s).

centre(String, 0, _) -> [];			%Strange cases to centre string
centre(String, Len, Char) ->
    Slen = length(String),
    if
	Slen > Len -> substr(String, (Slen-Len) div 2 + 1, Len);
	Slen < Len ->
	    N = (Len-Slen) div 2,
	    r_pad(l_pad(String, Len-(Slen+N), Char), N, Char);
	Slen == Len -> String
    end.
	    

%%% SUB_STRING %%%

sub_string(String, Start) -> substr(String, Start).

sub_string(String, Start, Stop) -> substr(String, Start, Stop - Start + 1).

%% The Regular Expression Matching Functions.
%%
%%  These have been rewritten. As their interface has changed slightly
%%  (much to the better) I have moved them to a new module 'regexp' to
%%  avoid another "interface war" about something which doesn't
%%  serioulsy affect that many people. This interface is kept for
%%  backwards compatibility so I don't get shot for that as well.
%%
%%  /Robert Virding

re_sh_to_awk(ShellRegExp) -> regexp:sh_to_awk(ShellRegExp).

re_parse(RegExp) ->
    case regexp:parse(RegExp) of
	{ok,RE} -> {regexp,RE};
	{error,E} -> {error,E} 
    end.

re_match(String, RegExp) ->
    case regexp:match(String, RegExp) of
	{match,Start,Len} -> {match,substr(String, Start, Len),Start};
	nomatch -> nomatch;
	{error,E} -> {error,E}
    end.

re_sub(String, RegExp, New) ->
    case regexp:sub(String, RegExp, New) of
	{ok,Res,N} -> {ok,Res};
	{error,E} -> {error,E}
    end.

re_gsub(String, RegExp, New) ->
    case regexp:gsub(String, RegExp, New) of
	{ok,Res,N} -> {ok,Res};
	{error,E} -> {error,E}
    end.

re_split(String, RegExp) -> regexp:split(String, RegExp).
