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

print(_, _, _, 0) -> "...";
print([], _, _, _) -> "[]";
print({}, _, _, _) -> "{}";
print(List, Col, Ll, D) when list(List) ->
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
		     [print(hd(List), Col + 1, Ll, D - 1)|
		      print_tail(tl(List), Col + 1, Ll, D - 1)],
		     $]]
	    end
    end;
print(Fun, Col, Ll, D) when function(Fun) ->
    io_lib:write(Fun);
print(Tuple, Col, Ll, D) when tuple(Tuple) ->
    Len = write_length(Tuple, D, 0, Ll - Col),
    if
	D == 1 -> "{...}";
	Len + Col < Ll ->
	    write(Tuple, D);
	atom(element(1, Tuple)), size(Tuple) > 1 ->
	    print_tag_tuple(Tuple, Col, Ll, D);
	true ->
	    [${,
	     [print(element(1, Tuple), Col + 1, Ll, D - 1)|
	      print_tail(tl(tuple_to_list(Tuple)), Col + 1, Ll, D - 1)],
	     $}]
    end;
print(Binary, Col, Ll, D) when binary(Binary) ->
    io_lib:write(Binary, D);
print(Vec, Col, Ll, D) when size(Vec) >= 0 ->
    Len = write_length(Vec, D, 0, Ll - Col),
    if
	D == 1 -> "#Vector<...>";
	Len + Col < Ll ->
	    write(Vec, D);
	true ->
	    ["#Vector<",
	     [print(vector:get(1, Vec), Col + 1, Ll, D - 1)|
	      print_tail(tl(vector:to_list(Vec)), Col + 1, Ll, D - 1)],
	     $>]
    end;
print(Term, Col, Ll, D) -> io_lib:write(Term, D).

%% print_tag_tuple(Tuple, Column, LineLength, Depth) -> [Char]
%%  Print a tagged tuple by indenting the rest of the elements differently
%%  to the tag. Start beside the tag if start column not too far to
%%  the right. Tuple has size >= 2.

print_tag_tuple(Tuple, Col, Ll, D) ->
    Tag = io_lib:write_atom(element(1, Tuple)),
    Tlen = length(Tag),
    Tcol = Col + Tlen + 2,
    if
	Tcol >= Ll div 2, Tlen > 2 ->
	    [${,Tag,
	     print_tail(tl(tuple_to_list(Tuple)), Col + 4, Ll, D - 2),
	     $}];
	true ->
	    [${,Tag,$,,
	     [print(element(2, Tuple), Col + Tlen + 2, Ll, D - 2)|
	      print_tail(tl(tl(tuple_to_list(Tuple))), Tcol, Ll, D - 3)],
	     $}]
    end.

%% print_tail([Element], Column, LineLength, D) -> [Char]
%%  Pretty print the elements of a list or tuple.

print_tail([], Col, Ll, D) -> "";
print_tail(Es, Col, Ll, 1) -> "|...";
print_tail([E|Es], Col, Ll, D) ->
    [$,,nl_indent(Col-1),
     print(E, Col, Ll, D-1)|
     print_tail(Es, Col, Ll, D-1)];
print_tail(E, Col, Ll, D) ->
    [$|,nl_indent(Col-1),print(E, Col, Ll, D-1)].

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
write(Fun, D) when function(Fun) -> io_lib:write(Fun); %Must catch this first
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

write_tail([], D) -> "";
write_tail(Es, 1) -> "|...";
write_tail([E|Es], D) ->
    [$,,write(E, D - 1)|write_tail(Es, D - 1)];
write_tail(E, D) ->
    [$|,write(E, D - 1)].
     
%% write_length(Term, Depth, Accumulator, MaxLength) -> integer()
%%  Calculate the print length of a term, but exit when length becomes
%%  greater than MaxLength.

write_length(T, D, Acc, Max) when Acc > Max -> Acc;
write_length(T, 0, Acc, Max) -> Acc + 3;
write_length([], _, Acc, _) -> Acc + 2;
write_length({}, _, Acc, _) -> Acc + 2;
write_length(List, D, Acc, Max) when list(List) ->
    case io_lib:printable_list(List) of
	true ->
	    Acc + length(io_lib:write_string(List, $"));
	false ->
	    write_length_list(List, D, Acc, Max)
    end;
write_length(Fun, D, Acc, Max) when function(Fun) ->
    Acc + length(io_lib:write(Fun));
write_length(Tuple, D, Acc, Max) when tuple(Tuple) ->
    write_length_list(tuple_to_list(Tuple), D, Acc, Max);
write_length(Bin, D, Acc, Max) when binary(Bin) ->
    Acc + length(io_lib:write(Bin));
write_length(Vec, D, Acc, Max) when size(Vec) >= 0 ->
    write_length_list(vector:to_list(Vec), D, Acc, Max);
write_length(Term, D, Acc, Max) ->
    Acc + length(io_lib:write(Term)).

write_length_list(_, _, Acc, Max) when Acc > Max -> Acc;
write_length_list([], _, Acc, _) -> Acc + 1;	%]
write_length_list(Es, 1, Acc, _) -> Acc + 5;	%|...]
write_length_list([E|Es], D, Acc, Max) ->
    write_length_list(Es,
		      D - 1,
		      write_length(E, D - 1, Acc + 1, Max),
		      Max);
write_length_list(E, D, Acc, Max) ->
    write_length(E, D - 1, Acc + 2, Max).	%| ]

nl_indent(N) when N >= 0 -> [$\n|string:chars($\s, N)];
nl_indent(N) -> "".
