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
-module(io_lib_pretty).

%% Pretty printing Erlang terms
%%
%% In this module "print" means the formatted printing while "write" means
%% just writing out onto one line. Unfortunately, we can not use the
%% standard io_lib:write/2 as this does not handle printable lists
%% correctly. We also want to be sure that the length calcualtions are
%% the same.

-export([print/1,print/4]).

%% print(Term) -> [Chars]
%% print(Term, Column, LineLength, Depth) -> [Chars]
%% Depth = -1 gives unlimited print depth. Use io_lib:write for atomic terms.

print(Term) ->
    print(Term, 1, 80, -1).

print(List, Col, Ll, D) ->
    print(List, Col, Ll, D, indent(Col)).

print(_, _, _, 0, _) -> "...";
print([], _, _, _, _) -> "[]";
print({}, _, _, _, _) -> "{}";
print(List, Col, Ll, D, Ind) when list(List) ->
    case io_lib:printable_list(List) of
	true ->
	    io_lib:write_string(List, $");
	false ->
	    Len = write_length(List, D, 0, Ll - Col),
	    if
		D == 1 -> "[...]";
		Len + Col < Ll ->
		    write(List, D);
		true ->
		    [$[,
		     [print(hd(List), Col + 1, Ll, D - 1, indent(1, Ind))|
		      print_tail(tl(List), Col + 1, Ll, D - 1, indent(1, Ind))],
		     $]]
	    end
    end;
print(Fun, _Col, _Ll, _D, _Ind) when function(Fun) ->
    io_lib:write(Fun);
print(Tuple, Col, Ll, D, Ind) when tuple(Tuple) ->
    Len = write_length(Tuple, D, 0, Ll - Col),
    if
	D == 1 -> "{...}";
	Len + Col < Ll ->
	    write(Tuple, D);
	atom(element(1, Tuple)), size(Tuple) > 1 ->
	    print_tag_tuple(Tuple, Col, Ll, D, Ind);
	true ->
	    [${,
	     [print(element(1, Tuple), Col + 1, Ll, D - 1, indent(1, Ind))|
	      print_tail(tl(tuple_to_list(Tuple)), 
			 Col + 1, Ll, D - 1, indent(1, Ind))],
	     $}]
    end;
print(Binary, _Col, _Ll, D, _Ind) when binary(Binary) ->
    io_lib:write(Binary, D);
print(Vec, _Col, _Ll, _D, _Ind) when size(Vec) == 0 ->
    "#Vector<>";
print(Vec, Col, Ll, D, Ind) when size(Vec) >= 0 ->
    Len = write_length(Vec, D, 0, Ll - Col),
    if
	D == 1 -> "#Vector<...>";
	Len + Col < Ll ->
	    write(Vec, D);
	true ->
	    ["#Vector<",
	     [print(vector:get(1, Vec), Col + 1, Ll, D - 1, indent(1, Ind))|
	      print_tail(tl(vector:to_list(Vec)), 
			 Col + 1, Ll, D - 1, indent(1, Ind))],
	     $>]
    end;
print(Term, _Col, _Ll, D, _Ind) -> io_lib:write(Term, D).

%% print_tag_tuple(Tuple, Column, LineLength, Depth, Ind) -> [Char]
%%  Print a tagged tuple by indenting the rest of the elements differently
%%  to the tag. Start beside the tag if start column not too far to
%%  the right. Tuple has size >= 2.

print_tag_tuple(Tuple, Col, Ll, D, Ind) ->
    Tag = io_lib:write_atom(element(1, Tuple)),
    Tlen = length(Tag),
    Tind = Tlen + 2,
    Tcol = Col + Tind,
    if
	Tcol >= Ll div 2, Tlen > 2 ->
	    [${,Tag,
	     print_tail(tl(tuple_to_list(Tuple)), 
			Col + 4, Ll, D - 2, indent(4, Ind)),
	     $}];
	true ->
	    [${,Tag,$,,
	     [print(element(2, Tuple), Tcol, Ll, D - 2, indent(Tind, Ind))|
	      print_tail(tl(tl(tuple_to_list(Tuple))), 
			 Tcol, Ll, D - 3, indent(Tind, Ind))],
	     $}]
    end.

%% print_tail([Element], Column, LineLength, D, Ind) -> [Char]
%%  Pretty print the elements of a list or tuple.

print_tail([], _Col, _Ll, _D, _Ind) -> "";
print_tail(_, _Col, _Ll, 1, _Ind) -> "|...";
print_tail([E|Es], Col, Ll, D, Ind) ->
    [$,,$\n,Ind,print(E, Col, Ll, D-1)|
     print_tail(Es, Col, Ll, D-1, Ind)];
print_tail(E, Col, Ll, D, Ind) ->
    [$|,$\n,Ind,print(E, Col, Ll, D-1)].

%% write(Term, Depth) -> [Char]
%%  Write a term down to Depth on one line. Use io_lib:write/2 for
%%  atomic terms.

write(_, 0) -> "...";
write([], _) -> "[]";
write({}, _) -> "{}";
write(List, D) when list(List) ->
    case io_lib:printable_list(List) of
	true ->
	    io_lib:write_string(List, $");
	false ->
	    if
		D == 1 -> "[...]";
		true ->
		    [$[,
		     [write(hd(List), D-1)|write_tail(tl(List), D-1)],
		     $]]
	    end
    end;
write(Fun, _D) when function(Fun) -> io_lib:write(Fun); %Must catch this first
write(T, D) when tuple(T) ->
    if
	D == 1 -> "{...}";
	true ->
	    [${,
	     [write(element(1, T), D-1)|write_tail(tl(tuple_to_list(T)), D-1)],
	     $}]
    end;
write(Bin, D) when binary(Bin) -> io_lib:write(Bin, D);
write(T, D) when size(T) >= 0 ->
    if
	D == 1 -> "#Vector<...>";
	true ->
	    ["#Vector<",
	     [write(vector:get(1, T), D-1)|write_tail(tl(vector:to_list(T)), D-1)],
	     $>]
    end;
write(Term, D) -> io_lib:write(Term, D).

write_tail([], _D) -> "";
write_tail(_, 1) -> "|...";
write_tail([E|Es], D) ->
    [$,,write(E, D - 1)|write_tail(Es, D - 1)];
write_tail(E, D) ->
    [$|,write(E, D - 1)].
     
%% write_length(Term, Depth, Accumulator, MaxLength) -> integer()
%%  Calculate the print length of a term, but exit when length becomes
%%  greater than MaxLength.

write_length(_, _D, Acc, Max) when Acc > Max -> Acc;
write_length(_, 0, Acc, _Max) -> Acc + 3;
write_length([], _, Acc, _) -> Acc + 2;
write_length({}, _, Acc, _) -> Acc + 2;
write_length(List, D, Acc, Max) when list(List) ->
    case io_lib:printable_list(List) of
	true ->
	    Acc + length(io_lib:write_string(List, $"));
	false ->
	    write_length_list(List, D, Acc, Max)
    end;
write_length(Fun, _D, Acc, _Max) when function(Fun) ->
    Acc + length(io_lib:write(Fun));
write_length(Tuple, D, Acc, Max) when tuple(Tuple) ->
    write_length_list(tuple_to_list(Tuple), D, Acc, Max);
write_length(Bin, D, Acc, _Max) when binary(Bin) ->
    if  D < 0 ->                                %Used to print all
            Acc + 4 + 4*size(Bin);		%Acc + << 4*size >>
        true ->
            Acc + 4 + 4*(D+1)
    end;
write_length(Vec, _D, Acc, _Max) when size(Vec) == 0 -> Acc + 9;
write_length(Vec, D, Acc, Max) when size(Vec) >= 0 ->
    write_length_list(vector:to_list(Vec), D, Acc, Max);
write_length(Term, _D, Acc, _Max) ->
    Acc + length(io_lib:write(Term)).

write_length_list(_, _, Acc, Max) when Acc > Max -> Acc;
write_length_list([], _, Acc, _) -> Acc + 1;	%]
write_length_list(_, 1, Acc, _) -> Acc + 5;	%|...]
write_length_list([E|Es], D, Acc, Max) ->
    write_length_list(Es,
		      D - 1,
		      write_length(E, D - 1, Acc + 1, Max),
		      Max);
write_length_list(E, D, Acc, Max) ->
    write_length(E, D - 1, Acc + 2, Max).	%| ]

indent(N) when integer(N), N > 0 ->
    chars($\s, N-1);
indent(N) when integer(N) ->
    [[]]. % This is an ugly kludge not crash for column less than 1

indent(0, Ind) ->
    Ind;
indent(N, [[]]) when integer(N), N > 0 ->
    indent(N-1, []); % Same kludge as above
indent(1, Ind) -> % Optimization of common case
    [$\s|Ind];
indent(4, Ind) -> % Optimization of common case
    S2 = [$\s,$\s],
    [S2,S2|Ind];
indent(N, Ind) when integer(N), N > 0 ->
    [chars($\s, N)|Ind].

%% A deep version of string:chars/2
chars(_C, 0) ->
    [];
chars(C, 1) ->
    [C];
chars(C, 2) ->
    [C,C];
chars(C, 3) ->
    [C,C,C];
chars(C, N) when (N band 1) == 0 ->
    S = chars(C, N bsr 1),
    [S|S];
chars(C, N) ->
    S = chars(C, N bsr 1),
    [C,S|S].
