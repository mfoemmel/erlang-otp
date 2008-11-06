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

%% This module is a library of useful i/o functions. It is hoped that the
%% functions defined in it are basic enough to be used without modification
%% as components of more complex utilities.
%%
%% It is completely self-contained and uses no other modules. Its own
%% utilities are exported.
%%
%% Most of the code here is derived from the original prolog versions and
%% from similar code written by Joe Armstrong and myself.
%%
%% This module has been split into seperate modules:
%% io_lib        - basic write and utilities
%% io_lib_format - formatted output
%% io_lib_fread  - formatted input
%% io_lib_pretty - term prettyprinter

%% For handling ISO 8859-1 (Latin-1) we use the following type
%% information:
%%
%% 000 - 037	NUL - US	control
%% 040 - 057	SPC - /		punctuation
%% 060 - 071	0 - 9		digit
%% 072 - 100	: - @		punctuation
%% 101 - 132	A - Z		uppercase
%% 133 - 140	[ - `		punctuation
%% 141 - 172	a - z		lowercase
%% 173 - 176	{ - ~		punctuation
%% 177		DEL		control
%% 200 - 237			control
%% 240 - 277	NBSP - ¿	punctuation
%% 300 - 326	À - Ö		uppercase
%% 327		×		punctuation
%% 330 - 336	Ø - Þ		uppercase
%% 337 - 366	ß - ö		lowercase
%% 367		÷		punctuation
%% 370 - 377	ø - ÿ		lowercase
%%
%% Many punctuation characters region have special meaning.  Must
%% watch using × \327, very close to x \170

-module(io_lib).

-export([fwrite/2,fread/2,fread/3,format/2]).
-export([print/1,print/4,indentation/2]).

-export([write/1,write/2,write/3,nl/0,format_prompt/1]).
-export([write_atom/1,write_string/1,write_string/2,write_char/1]).

-export([quote_atom/2,char_list/1,deep_char_list/1,printable_list/1]).

%% Utilities for collecting characters.
-export([collect_chars/3,collect_line/2,collect_line/3,get_until/3]).

%% Interface calls to sub-modules.

fwrite(Format, Args) ->
    format(Format, Args).

fread(Chars, Format) ->
    io_lib_fread:fread(Chars, Format).

fread(Cont, Chars, Format) ->
    io_lib_fread:fread(Cont, Chars, Format).

format(Format, Args) ->
    case catch io_lib_format:fwrite(Format, Args) of
	{'EXIT',_} ->
	    erlang:error(badarg, [Format, Args]);
	Other ->
	    Other
    end.

print(Term) ->
    io_lib_pretty:print(Term).

print(Term, Column, LineLength, Depth) ->
    io_lib_pretty:print(Term, Column, LineLength, Depth).

indentation(Chars, Current) ->
    io_lib_format:indentation(Chars, Current).


%% Format an IO-request prompt (handles formatting errors safely).
%% Atoms, binaries, and iolists can be used as-is, and will be
%% printed without any additional quotes.
%% Note that the output is a deep string, and not an iolist (i.e.,
%% it may be deep, but never contains binaries, due to the "~s").
format_prompt({format,Format,Args}) ->
    format_prompt(Format,Args);
format_prompt(Prompt)
  when is_list(Prompt); is_atom(Prompt); is_binary(Prompt) ->
    format_prompt("~s", [Prompt]);
format_prompt(Prompt) ->
    format_prompt("~p", [Prompt]).

format_prompt(Format,Args) ->
    case catch io_lib:format(Format,Args) of
	{'EXIT',_} -> "???";
	List -> List
    end.


%% write(Term)
%% write(Term, Depth)
%% write(Term, Depth, Pretty)
%%  Return a (non-flattened) list of characters giving a printed
%%  representation of the term. write/3 is for backward compatibility.

write(Term) -> write(Term, -1).

write(Term, D, true) ->
    io_lib_pretty:print(Term, 1, 80, D);
write(Term, D, false) ->
    write(Term, D).

write(_Term, 0) -> "...";
write(Term, _D) when is_integer(Term) -> integer_to_list(Term);
write(Term, _D) when is_float(Term) -> io_lib_format:fwrite_g(Term);
write(Atom, _D) when is_atom(Atom) -> write_atom(Atom);
write(Term, _D) when is_port(Term) -> write_port(Term);
write(Term, _D) when is_pid(Term) -> pid_to_list(Term);
write(Term, _D) when is_reference(Term) -> write_ref(Term);
write(<<_/bitstring>>=Term, D) -> write_binary(Term, D);
write([], _D) -> "[]";
write({}, _D) -> "{}";
write([H|T], D) ->
    if
	D =:= 1 -> "[...]";
	true ->
	    [$[,[write(H, D-1)|write_tail(T, D-1, $|)],$]]
    end;
write(F, _D) when is_function(F) ->
    erlang:fun_to_list(F);
write(T, D) when is_tuple(T) ->
    if
	D =:= 1 -> "{...}";
	true ->
	    [${,
	     [write(element(1, T), D-1)|
              write_tail(tl(tuple_to_list(T)), D-1, $,)],
	     $}]
    end.

%% write_tail(List, Depth, CharacterBeforeDots)
%%  Test the terminating case first as this looks better with depth.

write_tail([], _D, _S) -> "";
write_tail(_, 1, S) -> [S | "..."];
write_tail([H|T], D, S) ->
    [$,,write(H, D-1)|write_tail(T, D-1, S)];
write_tail(Other, D, S) ->
    [S,write(Other, D-1)].

write_port(Port) ->
    erlang:port_to_list(Port).

write_ref(Ref) ->
    erlang:ref_to_list(Ref).

write_binary(B, D) when is_integer(D) ->
    [$<,$<,write_binary_body(B, D),$>,$>].

write_binary_body(_B, 1) ->
    "...";
write_binary_body(<<>>, _D) ->
    "";
write_binary_body(<<X:8>>, _D) ->
    [integer_to_list(X)];
write_binary_body(<<X:8,Rest/bitstring>>, D) ->
    [integer_to_list(X),$,|write_binary_body(Rest, D-1)];
write_binary_body(B, _D) ->
    L = bit_size(B),
    <<X:L>> = B,
    [integer_to_list(X),$:,integer_to_list(L)].

%% write_atom(Atom) -> [Char]
%%  Generate the list of characters needed to print an atom.

write_atom(Atom) ->
    Chars = atom_to_list(Atom),
    case quote_atom(Atom, Chars) of
	true ->
	    write_string(Chars, $');   %'
	false ->
	    Chars
    end.

%% quote_atom(Atom, CharList)
%%  Return 'true' if atom with chars in CharList needs to be quoted, else
%%  return 'false'.

quote_atom(Atom, Cs0) ->
    case erl_scan:reserved_word(Atom) of
	true -> true;
	false ->
	    case Cs0 of
		[C|Cs] when C >= $a, C =< $z ->
		    not name_chars(Cs);
		[C|Cs] when C >= $ß, C =< $ÿ, C =/= $÷ ->
		    not name_chars(Cs);
		_ -> true
	    end
    end.

name_chars([C|Cs]) ->
    case name_char(C) of
	true -> name_chars(Cs);
	false -> false
    end;
name_chars([]) -> true.

name_char(C) when C >= $a, C =< $z -> true;
name_char(C) when C >= $ß, C =< $ÿ, C =/= $÷ -> true;
name_char(C) when C >= $A, C =< $Z -> true;
name_char(C) when C >= $À, C =< $Þ, C =/= $× -> true;
name_char(C) when C >= $0, C =< $9 -> true;
name_char($_) -> true;
name_char($@) -> true;
name_char(_) -> false.

%% write_string([Char]) -> [Char]
%%  Generate the list of characters needed to print a string.

write_string(S) ->
    write_string(S, $").   %"

write_string(S, Q) ->
    [Q|write_string1(S, Q)].

write_string1([], Q) ->
    [Q];
write_string1([C|Cs], Q) ->
    string_char(C, Q, write_string1(Cs, Q)).

string_char(Q, Q, Tail) -> [$\\,Q|Tail];	%Must check these first!
string_char($\\, _, Tail) -> [$\\,$\\|Tail];
string_char(C, _, Tail) when C >= $\s, C =< $~ ->
    [C|Tail];
string_char(C, _, Tail) when C >= $\240, C =< $\377 ->
    [C|Tail];
string_char($\n, _, Tail) -> [$\\,$n|Tail];	%\n = LF
string_char($\r, _, Tail) -> [$\\,$r|Tail];	%\r = CR
string_char($\t, _, Tail) -> [$\\,$t|Tail];	%\t = TAB
string_char($\v, _, Tail) -> [$\\,$v|Tail];	%\v = VT
string_char($\b, _, Tail) -> [$\\,$b|Tail];	%\b = BS
string_char($\f, _, Tail) -> [$\\,$f|Tail];	%\f = FF
string_char($\e, _, Tail) -> [$\\,$e|Tail];	%\e = ESC
string_char($\d, _, Tail) -> [$\\,$d|Tail];	%\d = DEL
string_char(C, _, Tail) ->			%Other control characters.
    C1 = (C bsr 6) + $0,
    C2 = ((C bsr 3) band 7) + $0,
    C3 = (C band 7) + $0,
    [$\\,C1,C2,C3|Tail].

%% write_char(Char) -> [char()].
%%  Generate the list of characters needed to print a character constant.
%%  Must special case SPACE, $\s, here.

write_char($\s) -> "$\\s";			%Must special case this.
write_char(C) when is_integer(C), C >= $\000, C =< $\377 ->
    [$$|string_char(C, -1, [])].

%% char_list(CharList)
%% deep_char_list(CharList)
%%  Return true if CharList is a (possibly deep) list of characters, else
%%  false.

char_list([C|Cs]) when is_integer(C), C >= $\000, C =< $\377 ->
    char_list(Cs);
char_list([]) -> true;
char_list(_) -> false.			%Everything else is false

deep_char_list(Cs) ->
    deep_char_list(Cs, []).

deep_char_list([C|Cs], More) when is_list(C) ->
    deep_char_list(C, [Cs|More]);
deep_char_list([C|Cs], More) when is_integer(C), C >= $\000, C =< $\377 ->
    deep_char_list(Cs, More);
deep_char_list([], [Cs|More]) ->
    deep_char_list(Cs, More);
deep_char_list([], []) -> true;
deep_char_list(_, _More) ->			%Everything else is false
    false.

%% printable_list([Char]) -> bool()
%%  Return true if CharList is a list of printable characters, else
%%  false.

printable_list([C|Cs]) when is_integer(C), C >= $\040, C =< $\176 ->
    printable_list(Cs);
printable_list([C|Cs]) when is_integer(C), C >= $\240, C =< $\377 ->
    printable_list(Cs);
printable_list([$\n|Cs]) -> printable_list(Cs);
printable_list([$\r|Cs]) -> printable_list(Cs);
printable_list([$\t|Cs]) -> printable_list(Cs);
printable_list([$\v|Cs]) -> printable_list(Cs);
printable_list([$\b|Cs]) -> printable_list(Cs);
printable_list([$\f|Cs]) -> printable_list(Cs);
printable_list([$\e|Cs]) -> printable_list(Cs);
printable_list([]) -> true;
printable_list(_) -> false.			%Everything else is false

%% List = nl()
%%  Return a list of characters to generate a newline.

nl() ->
    "\n".

%%
%% Utilities for collecting characters in input files
%%


%% collect_chars(State, Data, Count). New in R9C.
%%  Returns:
%%      {stop,Result,RestData}
%%      NewState
collect_chars(start, Data, N) when is_binary(Data) ->
    Size = byte_size(Data),
    if Size > N ->
	    {B1,B2} = split_binary(Data, N),
	    {stop,B1,B2};
       Size < N ->
	    {binary,[Data],N-Size};
       true ->
	    {stop,Data,eof}
    end;
collect_chars(start,Data,N) when is_list(Data) ->
    collect_chars_list([], N, Data);
collect_chars(start, eof, _) ->
    {stop,eof,eof};
collect_chars({binary,Stack,_N}, eof, _) ->
    {stop,binrev(Stack),eof};
collect_chars({binary,Stack,N}, Data, _) ->
    Size = byte_size(Data),
    if Size > N ->
	    {B1,B2} = split_binary(Data, N),
	    {stop,binrev(Stack, [B1]),B2};
       Size < N ->
	    {binary,[Data|Stack],N-Size};
       true ->
	    {stop,binrev(Stack, [Data]),eof}
    end;
collect_chars({list,Stack,N}, Data, _) ->
    collect_chars_list(Stack, N, Data);
%% collect_chars(Continuation, MoreChars, Count)
%%  Returns:
%%	{done,Result,RestChars}
%%	{more,Continuation}

collect_chars([], Chars, N) ->
    collect_chars1(N, Chars, []);
collect_chars({Left,Sofar}, Chars, _N) ->
    collect_chars1(Left, Chars, Sofar).

collect_chars1(N, Chars, Stack) when N =< 0 ->
    {done,lists:reverse(Stack, []),Chars};
collect_chars1(N, [C|Rest], Stack) ->
    collect_chars1(N-1, Rest, [C|Stack]);
collect_chars1(_N, eof, []) ->
    {done,eof,[]};
collect_chars1(_N, eof, Stack) ->
    {done,lists:reverse(Stack, []),[]};
collect_chars1(N, [], Stack) ->
    {more,{N,Stack}}.

collect_chars_list(Stack, 0, Data) ->
    {stop,lists:reverse(Stack, []),Data};
collect_chars_list(Stack, _N, eof) ->
    {stop,lists:reverse(Stack, []),eof};
collect_chars_list(Stack, N, []) ->
    {list,Stack,N};
collect_chars_list(Stack,N, [H|T]) ->
    collect_chars_list([H|Stack], N-1, T).

%% collect_line(Continuation, MoreChars)
%%  Returns:
%%	{done,Result,RestChars}
%%	{more,Continuation}
%%
%%  XXX Can be removed when compatibility with pre-R12B-5 nodes
%%  is no longer required.
%% 
collect_line([], Chars) ->
    collect_line1(Chars, []);
collect_line({SoFar}, More) ->
    collect_line1(More, SoFar).

collect_line1([$\r, $\n|Rest], Stack) ->
    collect_line1([$\n|Rest], Stack);
collect_line1([$\n|Rest], Stack) ->
    {done,lists:reverse([$\n|Stack], []),Rest};
collect_line1([C|Rest], Stack) ->
    collect_line1(Rest, [C|Stack]);
collect_line1(eof, []) ->
    {done,eof,[]};
collect_line1(eof, Stack) ->
    {done,lists:reverse(Stack, []),[]};
collect_line1([], Stack) ->
    {more,{Stack}}.

%% collect_line(State, Data, _). New in R9C.
%%  Returns:
%%	{stop,Result,RestData}
%%	NewState

collect_line(start, Data, _) when is_binary(Data) ->
%    erlang:display({?MODULE,?LINE,[start,Data]}),
    collect_line_bin(Data, Data, []);
collect_line(start, Data, _) when is_list(Data) ->
%    erlang:display({?MODULE,?LINE,[start,Data]}),
    collect_line_list(Data, []);
collect_line(start, eof, _) ->
%    erlang:display({?MODULE,?LINE,[start,eof]}),
    {stop,eof,eof};
collect_line(Stack, Data, _) when is_binary(Data) ->
%    erlang:display({?MODULE,?LINE,[Stack,Data]}),
    collect_line_bin(Data, Data, Stack);
collect_line(Stack, Data, _) when is_list(Data) ->
%    erlang:display({?MODULE,?LINE,[Stack,Data]}),
    collect_line_list(Data, Stack);
collect_line([B|_]=Stack, eof, _) when is_binary(B) ->
%    erlang:display({?MODULE,?LINE,[Stack,eof]}),
    {stop,binrev(Stack),eof};
collect_line(Stack, eof, _) ->
%    erlang:display({?MODULE,?LINE,[Stack,eof]}),
    {stop,lists:reverse(Stack, []),eof}.

collect_line_bin(<<$\n,T/binary>>, Data, Stack0) ->
    N = byte_size(Data) - byte_size(T),
    <<Line:N/binary,_/binary>> = Data,
    case Stack0 of
	[] ->
	    {stop,Line,T};
	[<<$\r>>|Stack] when N =:= 1 ->
	    {stop,binrev(Stack, [$\n]),T};
	_ ->
	    {stop,binrev(Stack0, [Line]),T}
    end;
collect_line_bin(<<$\r,$\n,T/binary>>, Data, Stack) ->
    N = byte_size(Data) - byte_size(T) - 2,
    <<Line:N/binary,_/binary>> = Data,
    {stop,binrev(Stack, [Line,$\n]),T};
collect_line_bin(<<$\r>>, Data0, Stack) ->
    N = byte_size(Data0) - 1,
    <<Data:N/binary,_/binary>> = Data0,
    [<<$\r>>,Data|Stack];
collect_line_bin(<<_,T/binary>>, Data, Stack) ->
    collect_line_bin(T, Data, Stack);
collect_line_bin(<<>>, Data, Stack) ->
    %% Need more data here.
    [Data|Stack].

collect_line_list([$\n|T], [$\r|Stack]) ->
    {stop,lists:reverse(Stack, [$\n]),T};
collect_line_list([$\n|T], Stack) ->
    {stop,lists:reverse(Stack, [$\n]),T};
collect_line_list([H|T], Stack) ->
    collect_line_list(T, [H|Stack]);
collect_line_list([], Stack) ->
    Stack.

%% Translator function to emulate a new (R9C and later) 
%% I/O client when you have an old one.
%%
%% Implements a middleman that is get_until server and get_chars client.

get_until(start, Data, XtraArg) ->
    get_until([], Data, XtraArg);
get_until(Cont, Data, {Mod, Func, XtraArgs}) ->
    Chars = if is_binary(Data) ->
		    binary_to_list(Data);
	       true ->
		    Data
	    end,
    case apply(Mod, Func, [Cont,Chars|XtraArgs]) of
	{done,Result,Buf} ->
	    {stop,Result,Buf};
	{more,NewCont} ->
	    NewCont
    end.

binrev(L) ->
    list_to_binary(lists:reverse(L, [])).

binrev(L, T) ->
    list_to_binary(lists:reverse(L, T)).
