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
%% Purpose : Core Erlang (naive) prettyprinter

-module(core_pp).

-export([format/1]).

-include("core_parse.hrl").

%% ====================================================================== %%
%% format(Node) -> Text
%%	Node = coreErlang()
%%	Text = string() | [Text]
%%
%%	Prettyprint-formats (naively) an abstract Core Erlang syntax
%%	tree.

-record(ctxt, {class = term,
	       indent = 0,
	       item_indent = 2,
	       body_indent = 4,
	       tab_width = 8}).

format(Node) -> case catch format(Node, #ctxt{}) of
		    {'EXIT',R} -> io_lib:format("~p",[Node]);
		    Other -> Other
		end.

format(Node, Ctxt) ->
    case canno(Node) of
	[] ->
	    format_1(Node, Ctxt);
	List ->
	    Ctxt1 = ctxt_bump_indent(Ctxt, Ctxt#ctxt.item_indent),
	    ["( ",
	     format_1(Node, Ctxt1),
	     nl_indent(Ctxt1),
	     "-| ",io_lib:write(List)," )"
	    ]
    end.

canno(Cthing) -> element(2, Cthing).

format_1(#c_atom{name=A}, Ctxt) -> core_atom(A);
format_1(#c_char{val=C}, Ctxt) -> io_lib:write_char(C);
format_1(#c_float{val=F}, Ctxt) -> float_to_list(F);
format_1(#c_int{val=I}, Ctxt) -> integer_to_list(I);
format_1(#c_nil{}, Ctxt) -> "[]";
format_1(#c_string{val=S}, Ctxt) -> io_lib:write_string(S);
format_1(#c_var{name=V}, Ctxt) ->
    case atom_to_list(V) of
	[C|Cs] when C == $_; C >= $A, C =< $Z -> [C|Cs];
	Cs -> [$_|Cs]
    end;
format_1(#c_bin{}=Type, Ctxt) ->
    ["<<",
     io_lib:write(Type),
     ">>"
    ];
format_1(#c_tuple{es=Es}, Ctxt) ->
    [${,
     format_hseq(Es, ",", ctxt_bump_indent(Ctxt, 1), fun format/2),
     $}
    ];
format_1(Cons, Ctxt) when record(Cons, c_cons) ->
    [$[,
     format_list_elements(Cons, ctxt_bump_indent(Ctxt, 1)),
     $]
    ];
format_1(#c_values{es=Es}, Ctxt) ->
    [$<,
     format_hseq(Es, ",", ctxt_bump_indent(Ctxt, 1), fun format/2),
     $>
    ];
format_1(#c_alias{var=V,pat=P}, Ctxt) ->
    Txt = [format(V)|" = "],
    [Txt|format(P, ctxt_bump_indent(Ctxt, width(Txt, Ctxt)))];
format_1(#c_let{vars=Vs,arg=A,body=B}, Ctxt) ->
    Ctxt1 = ctxt_bump_indent(Ctxt, Ctxt#ctxt.body_indent),
    ["let <",
     format_hseq(Vs, ",", ctxt_bump_indent(Ctxt, 5), fun format/2),
     "> =",
     nl_indent(Ctxt1),
     format(A, Ctxt1),
     nl_indent(Ctxt),
     "in  "
     | format(B, ctxt_bump_indent(Ctxt, 4))
    ];
format_1(#c_seq{arg=A,body=B}, Ctxt) ->
    Ctxt1 = ctxt_bump_indent(Ctxt, 4),
    ["do  ",
     format(A, Ctxt1),
     nl_indent(Ctxt),
     "then",
     nl_indent(Ctxt1)
     | format(B, Ctxt1)
    ];
format_1(#c_case{arg=A,clauses=Cs}, Ctxt) ->
    Ctxt1 = ctxt_bump_indent(Ctxt, Ctxt#ctxt.item_indent),
    ["case ",
     format(A, ctxt_bump_indent(Ctxt, 5)),
     " of",
     nl_indent(Ctxt1),
     format_vseq(Cs,
		 "", "",
		 ctxt_set_class(Ctxt1, clause),
		 fun format/2),
     nl_indent(Ctxt)
     | "end"
    ];
format_1(#c_clause{pats=Ps,guard=G,body=B}, Ctxt) ->
    %% Context class should be `clause'.
    Ptxt = ["<",
	    format_hseq(Ps, ", ", ctxt_bump_indent(Ctxt, 1), fun format/2),
	    ">"],
    Ctxt2 = ctxt_bump_indent(Ctxt, Ctxt#ctxt.body_indent),
    [Ptxt,
     " when ",
     format(G, ctxt_set_bump(Ctxt, expr, width(Ptxt, Ctxt) + 6)),
     " ->",
     nl_indent(Ctxt2)
     | format(B, ctxt_set_class(Ctxt2, expr))
    ];
format_1(#c_fun{vars=Vs,body=B}, Ctxt) ->
    Ctxt1 = ctxt_bump_indent(Ctxt, Ctxt#ctxt.body_indent),
    ["fun (",
     format_hseq(Vs, ",", ctxt_bump_indent(Ctxt, 5), fun format/2),
     ") ->",
     nl_indent(Ctxt1)
     | format(B, Ctxt1)
    ];
format_1(#c_call{op=Op,args=As}, Ctxt) ->
    Txt = [$(,format(Op, ctxt_bump_indent(Ctxt, 1)),$)],
    Ctxt1 = ctxt_bump_indent(Ctxt, 4),
    [Txt,
     nl_indent(Ctxt1),
     $(,format_hseq(As, ", ", Ctxt1, fun format/2),$)
    ];
format_1(#c_local{name=N,arity=A}, Ctxt) ->
    "local " ++ format_fa_pair({N,A}, Ctxt);
format_1(#c_remote{mod=M,name=N,arity=A}, Ctxt) when atom(M) ->
    %% This is for our internal translator.
    io_lib:format("remote ~s:~s/~w", [core_atom(M),core_atom(N),A]);
format_1(#c_internal{name=N,arity=A}, Ctxt) ->
    "internal " ++ format_fa_pair({N,A}, Ctxt);
format_1(#c_catch{body=B}, Ctxt) ->
    Ctxt1 = ctxt_bump_indent(Ctxt, Ctxt#ctxt.body_indent),
    ["catch",
     nl_indent(Ctxt1),
     format(B, Ctxt1)
    ];
format_1(#c_try{expr=E,vars=Vs,body=B}, Ctxt) ->
    Ctxt1 = ctxt_bump_indent(Ctxt, Ctxt#ctxt.body_indent),
    ["try",
     nl_indent(Ctxt1),
     format(E, Ctxt1),
     nl_indent(Ctxt),
     "catch (",
     format_hseq(Vs, ", ",
		 ctxt_bump_indent(Ctxt, 7),
		 fun format/2),
     ") ->",
     nl_indent(Ctxt1)
     | format(B, Ctxt1)
    ];
format_1(#c_receive{clauses=Cs,timeout=T,action=A}, Ctxt) ->
    Ctxt1 = ctxt_bump_indent(Ctxt, Ctxt#ctxt.item_indent),
    ["receive",
     nl_indent(Ctxt1),
     format_vseq(Cs, "", "", ctxt_set_class(Ctxt1, clause), fun format/2),
     nl_indent(Ctxt),
     "after ",
     format(T, ctxt_bump_indent(Ctxt, 6)),
     " ->",
     nl_indent(Ctxt1),
     format(A, Ctxt1),
     nl_indent(Ctxt)
     | "end"
    ];
format_1(#c_fdef{func=F,arity=A,body=B}, Ctxt) ->
    Ctxt1 = ctxt_set_bump(Ctxt, expr, Ctxt#ctxt.body_indent),
    ["fdef ",
     format_fa_pair({F,A}, ctxt_set_bump(Ctxt, term, 5)),
     " =",
     nl_indent(Ctxt1)
     | format(B, Ctxt1)
    ];
format_1(#c_mdef{name=N,exports=Es,attributes=As,body=B}, Ctxt) ->
    ["module ",
     format(#c_atom{name=N}, ctxt_set_bump(Ctxt, term, 7)),
     nl_indent(Ctxt),
     "export [",
     format_vseq(Es,
		 "", ",",
		 ctxt_set_bump(Ctxt, term, 8),
		 fun format_fa_pair/2),
     "]",
     nl_indent(Ctxt),
     "attributes [",
     format_vseq(As,
		 "", ",",
		 ctxt_set_bump(Ctxt, term, 12),
		 fun format_attribute/2),
     "]",
     nl_indent(Ctxt),
     format_vseq(B,
		 "", "",
		 ctxt_set_class(Ctxt, fdef),
		 fun format/2),
     nl_indent(Ctxt)
     | "end"
    ];
format_1(Type, Ctxt) ->
    ["** Unsupported type: ",
     io_lib:write(Type)
     | " **"
    ].

%% format_hseq([Thing], Separator, Context, Fun) -> Txt.

format_hseq([H], Sep, Ctxt, Fun) ->
    Fun(H, Ctxt);
format_hseq([H|T], Sep, Ctxt, Fun) ->
    Txt = [Fun(H, Ctxt)|Sep],
    Ctxt1 = ctxt_bump_indent(Ctxt, width(Txt, Ctxt)),
    [Txt|format_hseq(T, Sep, Ctxt1, Fun)];
format_hseq([], _, _, _) -> "".

%% format_vseq([Thing], LinePrefix, LineSuffix, Context, Fun) -> Txt.

format_vseq([H], Pre, Suf, Ctxt, Fun) ->
    Fun(H, Ctxt);
format_vseq([H|T], Pre, Suf, Ctxt, Fun) ->
    [Fun(H, Ctxt),Suf,nl_indent(Ctxt),Pre|
     format_vseq(T, Pre, Suf, Ctxt, Fun)];
format_vseq([], _, _, _, _) -> "".

format_fa_pair({F,A}, Ctxt) -> [core_atom(F),$/,integer_to_list(A)].

%% format_attribute({Name,Val}, Context) -> Txt.

format_attribute({Name,Val}, Ctxt) when list(Val) ->
    Txt = format(#c_atom{name=Name}, Ctxt),
    Ctxt1 = ctxt_bump_indent(Ctxt, width(Txt,Ctxt)+4),
    [Txt," = ",
     $[,format_vseq(Val, "", ",", Ctxt1,
		    fun (A, C) -> format(core_parse:abstract(A), C) end),$]
    ];
format_attribute({Name,Val}, Ctxt) ->
    Txt = format(#c_atom{name=Name}, Ctxt),
    [Txt," = ",format(core_parse:abstract(Val),
		      ctxt_bump_indent(Ctxt, width(Txt, Ctxt) + 3))].

format_list_elements(#c_cons{head=H,tail=T}, Ctxt) ->
    A = canno(T),
    case T of
	#c_nil{} when A == [] ->
	    format(H, Ctxt);
	#c_cons{} when A == [] ->
	    Txt = [format(H, Ctxt)|","],
	    Ctxt1 = ctxt_bump_indent(Ctxt, width(Txt, Ctxt)),
	    [Txt|format_list_elements(T, Ctxt1)];
	_ ->
	    Txt = [format(H, Ctxt)|"|"],
	    [Txt|format(T, ctxt_bump_indent(Ctxt, width(Txt, Ctxt)))]
    end.

indent(Ctxt) -> indent(Ctxt#ctxt.indent, Ctxt).

indent(N, Ctxt) when N =< 0 -> "";
indent(N, Ctxt) ->
    T = Ctxt#ctxt.tab_width,
    string:chars($\t, N div T, string:chars($\s, N rem T)).

nl_indent(Ctxt) -> [$\n|indent(Ctxt)].


unindent(T, Ctxt) ->
    unindent(T, Ctxt#ctxt.indent, Ctxt, []).

unindent(T, N, Ctxt, C) when N =< 0 ->
    [T|C];
unindent([$\s|T], N, Ctxt, C) ->
    unindent(T, N - 1, Ctxt, C);
unindent([$\t|T], N, Ctxt, C) ->
    Tab = Ctxt#ctxt.tab_width,
    if N >= Tab ->
	    unindent(T, N - Tab, Ctxt, C);
       true ->
	    unindent([string:chars($\s, Tab - N)|T], 0, Ctxt, C)
    end;
unindent([L|T], N, Ctxt, C) when list(L) ->
    unindent(L, N, Ctxt, [T|C]);
unindent([H|T], N, Ctxt, C) ->
    [H|[T|C]];
unindent([], N, Ctxt, [H|T]) ->
    unindent(H, N, Ctxt, T);
unindent([], N, Ctxt, []) -> [].


width(Txt, Ctxt) ->
    width(Txt, 0, Ctxt, []).

width([$\t|T], A, Ctxt, C) ->
    width(T, A + Ctxt#ctxt.tab_width, Ctxt, C);
width([$\n|T], A, Ctxt, C) ->
    width(unindent([T|C], Ctxt), Ctxt);
width([H|T], A, Ctxt, C) when list(H) ->
    width(H, A, Ctxt, [T|C]);
width([H|T], A, Ctxt, C) ->
    width(T, A + 1, Ctxt, C);
width([], A, Ctxt, [H|T]) ->
    width(H, A, Ctxt, T);
width([], A, Ctxt, []) -> A.

ctxt_bump_indent(Ctxt, Dx) ->
    Ctxt#ctxt{indent = Ctxt#ctxt.indent + Dx}.

ctxt_set_class(Ctxt, Class) ->
    Ctxt#ctxt{class = Class}.

ctxt_set_bump(Ctxt, Class, Dx) ->
    Ctxt#ctxt{class=Class,indent=Ctxt#ctxt.indent + Dx}.

core_atom(A) -> io_lib:write_string(atom_to_list(A), $').
