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
%%
%% Tk library functions
%%
%% Some tcl rehack to make tk work & short cuts
%%

-module(tklib).

-compile(export_all).

%%
%% As index but return character in a string
%%
strindex(N, L) ->
    case index(N, L) of
	"" -> "";
	C  -> [C]
    end.
%%
%% Zero based list index
%% return the nth element of the list 
%% return's "" if bad index
%%
index(N, L) when N >= 0 ->
    ix(N, L);
index(_, _) -> "".

ix(0, [H|_]) ->  H;
ix(I, [_|L]) ->  ix(I-1, L);
ix(_, []) -> [].

%%
%% Search for Pattern in list and return it's index.
%% First element is zero. return -1 if not found
%%
search(List, Pattern) ->
    search(List, 0, Pattern).

search([H|_], I, H) -> I;
search([_|T], I, P) -> search(T, I+1, P);
search([], _, _) -> -1.

%%
%% Convert string to lower case
%%
tolower([C|Cs]) ->
    if C >= $A, C =< $Z -> [(C - $A) + $a | tolower(Cs)];
       true -> [C | tolower(Cs)]
    end;
tolower([]) -> [];

tolower(C) when C >= $A, C =< $Z -> (C - $A) + $a;
tolower(C) -> C.

%%
%% Given an index into a string
%% return the index to the first character in the word
%% (Zero based)
%%
wordstart(String, Ix) ->
    {N, B, A} = split(String, Ix),
    N - wscan(B, 0).

%%
%% Given an index into a string
%% return the index to the last character in the word
%% (Zero based)
%%
wordend(String, Ix) ->
    {N, B, A} = split(String, Ix),
    N + wscan(A, 0).

%% Search  string2  for  a sequence of characters that
%% exactly match the characters in string1.  If found,
%% return  the  index  of  the  first character in the
%% first such match within  string2.   If  not  found,
%% return -1.
%%
first(S1, S1) -> 
    0;
first(S1, S2) ->
    fst(S2, S1, length(S1), 0).

fst(S2, S1, N, I) when length(S2) < N -> 
    -1;
fst(S2, S1, N, I) ->
    case lists:sublist(S2, 1, N) of
	S1 -> I;
	_ ->
	    fst(tl(S2), S1, N, I+1)
    end.

%%
%% Split string return first part reverse
%%
split(String, Ix) ->
    split(String, Ix, 0, []).

split(Cs, 0, N, Acc) -> 
    {N, Acc, Cs};
split([C|Cs], I, N, Acc) ->
    split(Cs, I-1, N+1, [C|Acc]);
split([], _, N, Acc) ->
    {N, Acc, []}.

%%
%% Scan while alphanumeric characters
%%
wscan([C|Cs], N) ->
    if
	C >= $A, C =< $Z -> wscan(Cs, N+1);
	C >= $a, C =< $z -> wscan(Cs, N+1);
	C >= $0, C =< $9 -> wscan(Cs, N+1);
	C == $_ -> wscan(Cs, N+1);
	true -> N
    end;
wscan([], N) ->  N.

%%
%% While 
%%
while(Fun, [H|T]) ->
    case Fun(H) of
	true -> while(Fun, T);
	false -> H;
	{false,Value} -> Value
    end;
while(Fun, []) -> [].


%%
%% Convert x event numbers into names (debug)
%%
xeventname(2) -> "KeyPress";
xeventname(3) -> "KeyRelease";
xeventname(4) -> "ButtonPress";
xeventname(5) -> "ButtonRelease";
xeventname(6) -> "MotionNotify";
xeventname(7) -> "EnterNotify";
xeventname(8) -> "LeaveNotify";
xeventname(9) -> "FocusIn";
xeventname(10) -> "FocusOut";
xeventname(11) -> "KeymapNotify";
xeventname(12) -> "Expose";
xeventname(13) -> "GraphicsExpose";
xeventname(14) -> "NoExpose";
xeventname(15) -> "VisibilityNotify";
xeventname(16) -> "CreateNotify";
xeventname(17) -> "DestroyNotify";
xeventname(18) -> "UnmapNotify";
xeventname(19) -> "MapNotify";
xeventname(20) -> "MapRequest";
xeventname(21) -> "ReparentNotify";
xeventname(22) -> "ConfigureNotify";
xeventname(23) -> "ConfigureRequest";
xeventname(24) -> "GravityNotify";
xeventname(25) -> "ResizeRequest";
xeventname(26) -> "CirculateNotify";
xeventname(27) -> "CirculateRequest";
xeventname(28) -> "PropertyNotify";
xeventname(29) -> "SelectionClear";
xeventname(30) -> "SelectionRequest";
xeventname(31) -> "SelectionNotify";
xeventname(32) -> "ColormapNotify";
xeventname(33) -> "ClientMessage";
xeventname(34) -> "MappingNotify";
xeventname(_) -> "???".
