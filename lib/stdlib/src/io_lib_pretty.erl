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

-export([print/1,print/2,print/3,print/4]).

%% print(Term) -> [Chars]
%% print(Term, Column, LineLength, Depth) -> [Chars]
%% Depth = -1 gives unlimited print depth. Use io_lib:write for atomic terms.

print(Term) ->
    print(Term, 1, 80, -1).

%% print(Term, RecDefFun) -> [Chars]
%% print(Term, Depth, RecDefFun) -> [Chars]
%% RecDefFun = fun(Tag, NoFields) -> [FieldTag] | no
%% Used by the shell to print records. 
print(Term, RecDefFun) ->
    print(Term, -1, RecDefFun).

print(Term, Depth, RecDefFun) ->
    print(Term, 1, 80, Depth, RecDefFun).

print(List, Col, Ll, D) ->
    print(List, Col, Ll, D, no_fun).

print(Term, Col, Ll, D, RecDefFun) ->
    print(Term, Col, Ll, D, indent(Col), RecDefFun).

print(_, _, _, 0, _, _RF) -> "...";
print([], _, _, _, _, _RF) -> "[]";
print({}, _, _, _, _, _RF) -> "{}";
print(List, Col, Ll, D, Ind, RF) when list(List) ->
    case io_lib:printable_list(List) of
	true ->
	    io_lib:write_string(List, $");
	false ->
	    Len = write_length(List, D, 0, Ll - Col, RF),
	    if
		D == 1 -> "[...]";
		Len + Col < Ll ->
		    write(List, D, RF);
		true ->
		    [$[,
		     [print(hd(List), Col + 1, Ll, D - 1, indent(1, Ind), RF)|
		      print_tail(tl(List), Col + 1, Ll, D - 1, indent(1, Ind), RF)],
		     $]]
	    end
    end;
print(Fun, _Col, _Ll, _D, _Ind, _RF) when function(Fun) ->
    io_lib:write(Fun);
print(Rec, Col, Ll, D, Ind, RF) when atom(element(1, Rec)), function(RF) ->
    case RF(element(1, Rec), size(Rec) - 1) of
       no ->
            print_tuple(Rec, Col, Ll, D, Ind, RF);
       RDefs ->
            print_record(Rec, RDefs, Col, Ll, D, Ind, RF)
    end;
print(Tuple, Col, Ll, D, Ind, RF) when tuple(Tuple) ->
    print_tuple(Tuple, Col, Ll, D, Ind, RF);
print(Binary, _Col, _Ll, D, _Ind, _RF) when binary(Binary) ->
    io_lib:write(Binary, D);
print(Term, _Col, _Ll, D, _Ind, _RF) -> io_lib:write(Term, D).

print_record(Rec, RDefs, Col, Ll, D, Ind, RF) ->
    Len = write_length(Rec, D, 0, Ll - Col, RF),
    if
        Len + Col < Ll ->
            write(Rec, D, RF);
	D == 1 -> "{...}";
        true ->
            Name = io_lib:write_atom(element(1, Rec)),
            Nlen = length(Name),
            Nind = Nlen + 2,
            Ncol = Col + Nind,
            {DCol,S} = if
                           Ncol >= Ll div 2, Nlen > 2 ->
                               {4,[$\n,indent(4, Ind)]};
                           true ->
                               {Nind,[]}
                       end,
            [$#,Name,${,
             print_fields(RDefs, tl(tuple_to_list(Rec)), 
                          Col + DCol, Ll, D - 2, indent(DCol, Ind), RF, S),
             $}]
    end.

print_fields([], [], _Col, _Ll, _D, _Ind, _RF, _S) ->
    "";
print_fields(_Defs, _Es, _Col, _Ll, 1, _Ind, _RF, _S) ->
    "|...";
print_fields([Def|Defs], [E|Es], Col, Ll, D, Ind, RF, S) ->
    [S,print_field(Def,E, Col, Ll, D-1, Ind, RF)|
     print_fields(Defs, Es, Col, Ll, D-1, Ind, RF, [$,,$\n,Ind])].

print_field(_Def, _E, _Col, _Ll, 0, _Ind, _RF) ->
    "...";
print_field(Def, E, Col, Ll, D, Ind, RF) ->
    [io_lib:write_atom(Def)," = ",print(E, Col, Ll, D, Ind, RF)].

print_tuple(Tuple, Col, Ll, D, Ind, RF) ->
    Len = write_length(Tuple, D, 0, Ll - Col, RF),
    if
	D == 1 -> "{...}";
	Len + Col < Ll ->
	    write(Tuple, D, RF);
	atom(element(1, Tuple)), size(Tuple) > 1 ->
	    print_tag_tuple(Tuple, Col, Ll, D, Ind, RF);
	true ->
	    [${,
	     [print(element(1, Tuple), Col + 1, Ll, D - 1, indent(1, Ind), RF)|
	      print_tail(tl(tuple_to_list(Tuple)), 
			 Col + 1, Ll, D - 1, indent(1, Ind), RF)],
	     $}]
    end.

%% print_tag_tuple(Tuple, Column, LineLength, Depth, Ind, RecDefFun) -> [Char]
%%  Print a tagged tuple by indenting the rest of the elements differently
%%  to the tag. Start beside the tag if start column not too far to
%%  the right. Tuple has size >= 2.

print_tag_tuple(Tuple, Col, Ll, D, Ind, RF) ->
    Tag = io_lib:write_atom(element(1, Tuple)),
    Tlen = length(Tag),
    Tind = Tlen + 2,
    Tcol = Col + Tind,
    if
	Tcol >= Ll div 2, Tlen > 2 ->
	    [${,Tag,
	     print_tail(tl(tuple_to_list(Tuple)), 
			Col + 4, Ll, D - 2, indent(4, Ind), RF),
	     $}];
	true ->
	    [${,Tag,$,,
	     [print(element(2, Tuple), Tcol, Ll, D - 2, indent(Tind,Ind), RF)|
	      print_tail(tl(tl(tuple_to_list(Tuple))), 
			 Tcol, Ll, D - 3, indent(Tind, Ind), RF)],
	     $}]
    end.

%% print_tail([Element], Column, LineLength, D, Ind, RecDefFun) -> [Char]
%%  Pretty print the elements of a list or tuple.

print_tail([], _Col, _Ll, _D, _Ind, _RF) -> "";
print_tail(_, _Col, _Ll, 1, _Ind, _RF) -> "|...";
print_tail([E|Es], Col, Ll, D, Ind, RF) ->
    [$,,$\n,Ind,print(E, Col, Ll, D-1, RF)|
     print_tail(Es, Col, Ll, D-1, Ind, RF)];
print_tail(E, Col, Ll, D, Ind, RF) ->
    [$|,$\n,Ind,print(E, Col, Ll, D-1, RF)].

%% write(Term, Depth, RecDefFun) -> [Char]
%%  Write a term down to Depth on one line. Use io_lib:write/2 for
%%  atomic terms.

write(_, 0, _RF) -> "...";
write([], _, _RF) -> "[]";
write({}, _, _RF) -> "{}";
write(List, D, RF) when list(List) ->
    case io_lib:printable_list(List) of
	true ->
	    io_lib:write_string(List, $");
	false ->
	    if
		D == 1 -> "[...]";
		true ->
		    [$[,
		     [write(hd(List), D-1, RF)|write_tail(tl(List), D-1, RF)],
		     $]]
	    end
    end;
write(Fun, _D, _RF) when function(Fun) -> 
    io_lib:write(Fun); %Must catch this first
write(R, D, RF) when atom(element(1, R)), function(RF) ->
    case RF(element(1, R), size(R) - 1) of
        no ->
            write_tuple(R, D, RF);
        RDefs ->
            [$#, io_lib:write_atom(element(1, R)), ${, 
             write_fields(RDefs, D - 1, tl(tuple_to_list(R)), RF, []),
             $}]
    end;
write(T, D, RF) when tuple(T) ->
    write_tuple(T, D, RF);
write(Bin, D, _RF) when binary(Bin) -> io_lib:write(Bin, D);
write(Term, D, _RF) -> io_lib:write(Term, D).

write_tuple(T, D, RF) ->
    if
	D == 1 -> "{...}";
	true ->
	    [${,
	     [write(element(1, T), D-1, RF) |
              write_tail(tl(tuple_to_list(T)), D-1, RF)],
	     $}]
    end.

write_tail([], _D, _RF) -> "";
write_tail(_, 1, _RF) -> "|...";
write_tail([E|Es], D, RF) ->
    [$,,write(E, D - 1, RF)|write_tail(Es, D - 1, RF)];
write_tail(E, D, RF) ->
    [$|,write(E, D - 1, RF)].
     
write_fields([], _D, [], _RF, _S) ->
    "";
write_fields(_, 1, _, _RF, _S) ->
    "|...";
write_fields([Def|Defs], D, [E|Es], RF, S) ->
    [S,write_field(Def, D - 1, E, RF)|write_fields(Defs, D - 1, Es, RF, $,)].

write_field(_Def, 0, _E, _RF) ->
    "...";
write_field(Def, D, E, RF) ->
    [io_lib:write_atom(Def)," = ",write(E, D, RF)].

%% write_length(Term, Depth, Accumulator, MaxLength, RecDefFun) -> integer()
%%  Calculate the print length of a term, but exit when length becomes
%%  greater than MaxLength.

write_length(_, _D, Acc, Max, _RF) when Acc > Max -> Acc;
write_length(_, 0, Acc, _Max, _RF) -> Acc + 3;
write_length([], _, Acc, _, _RF) -> Acc + 2;
write_length({}, _, Acc, _, _RF) -> Acc + 2;
write_length(List, D, Acc, Max, RF) when list(List) ->
    case io_lib:printable_list(List) of
	true ->
	    Acc + length(io_lib:write_string(List, $"));
	false ->
	    write_length_list(List, D, Acc, Max, RF)
    end;
write_length(Fun, _D, Acc, _Max, _RF) when function(Fun) ->
    Acc + length(io_lib:write(Fun));
write_length(R, D, Acc, Max, RF) when atom(element(1, R)), function(RF) ->
    case RF(element(1, R), size(R) - 1 ) of
        no -> 
            write_length_list(tuple_to_list(R), D, Acc, Max, RF);
        RDefs ->
            Acc1 = Acc + 2 + length(io_lib:write_atom(element(1, R))),
            write_length_fields(RDefs, D - 1, Acc1, Max, tl(tuple_to_list(R)), RF, 0)
    end;
write_length(Tuple, D, Acc, Max, RF) when tuple(Tuple) ->
    write_length_list(tuple_to_list(Tuple), D, Acc, Max, RF);
write_length(Bin, D, Acc, _Max, _RF) when binary(Bin) ->
    if  D < 0 ->                                %Used to print all
            Acc + 4 + 4*size(Bin);		%Acc + << 4*size >>
        true ->
            Acc + 4 + 4*(D+1)
    end;
write_length(Term, _D, Acc, _Max, _RF) ->
    Acc + length(io_lib:write(Term)).

write_length_list(_, _, Acc, Max, _RF) when Acc > Max -> Acc;
write_length_list([], _, Acc, _, _RF) -> Acc + 1; %]
write_length_list(_, 1, Acc, _, _RF) -> Acc + 5;  %|...]
write_length_list([E|Es], D, Acc, Max, RF) ->
    write_length_list(Es,
		      D - 1,
		      write_length(E, D - 1, Acc + 1, Max, RF),
		      Max,
                      RF);
write_length_list(E, D, Acc, Max, RF) ->
    write_length(E, D - 1, Acc + 2, Max, RF).	%| ]

write_length_fields(_, _D, Acc, Max, _, _RF, _Sl) when Acc > Max -> Acc;
write_length_fields([], _D, Acc, _Max, [], _RF, _Sl) -> Acc + 1; %}
write_length_fields(_, 1, Acc, _Max, _, _RF, Sl) -> Acc + Sl + 4; %|...}
write_length_fields([Def|Defs], D, Acc, Max, [E|Es], RF, Sl) ->
    Acc1 = write_length_field(Def, D - 1, Acc + Sl, Max, E, RF),
    write_length_fields(Defs, D-1, Acc1, Max, Es, RF, 1).

write_length_field(_Def, 0, Acc, _Max, _E, _RF) ->
    Acc + 3; %...
write_length_field(Def, D, Acc, Max, E, RF) ->
    write_length(E, D, Acc + length(io_lib:write_atom(Def)) + 3, Max, RF).

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
