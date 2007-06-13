-module(erl_parse).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("erl_parse.yrl", 425).

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

-export([parse_form/1,parse_exprs/1,parse_term/1]).
-export([normalise/1,abstract/1,tokens/1,tokens/2]).
-export([abstract/2, package_segments/1]).
-export([inop_prec/1,preop_prec/1,func_prec/0,max_prec/0]).

%% The following directive is needed for (significantly) faster compilation
%% of the generated .erl file by the HiPE compiler.  Please do not remove.
-compile([inline,{hipe,[{regalloc,linear_scan}]}]).


%% mkop(Op, Arg) -> {op,Line,Op,Arg}.
%% mkop(Left, Op, Right) -> {op,Line,Op,Left,Right}.

mkop(L, {Op,Pos}, R) -> {op,Pos,Op,L,R}.

mkop({Op,Pos}, A) -> {op,Pos,Op,A}.

%% keep track of line info in tokens
line(Tup) -> element(2, Tup).

%% Entry points compatible to old erl_parse.
%% These really suck and are only here until Calle gets multiple
%% entry points working.

parse_form(Tokens) ->
    parse(Tokens).

parse_exprs(Tokens) ->
    case parse([{atom,0,f},{'(',0},{')',0},{'->',0}|Tokens]) of
	{ok,{function,_Lf,f,0,[{clause,_Lc,[],[],Exprs}]}} ->
	    {ok,Exprs};
	{error,E} -> {error,E}
    end.

parse_term(Tokens) ->
    case parse([{atom,0,f},{'(',0},{')',0},{'->',0}|Tokens]) of
	{ok,{function,_Lf,f,0,[{clause,_Lc,[],[],[Expr]}]}} ->
	    case catch normalise(Expr) of
		{'EXIT',_R} ->
		    {error,{line(Expr),?MODULE,"bad term"}};
		Term -> {ok,Term}
	    end;
	{ok,{function,_Lf,f,0,[{clause,_Lc,[],[],[_E1,E2|_Es]}]}} ->
	    {error,{line(E2),?MODULE,"bad term"}};
	{error,E} -> {error,E}
    end.

build_type({atom,_,any}, []) -> {type, any, []};
build_type({atom,_,atom}, []) -> {type, atom, []};
build_type({atom,_,binary}, []) -> {type, binary, []};
build_type({atom,_,bool}, []) -> {type, bool, []};
build_type({atom,_,byte}, []) -> {type, byte, []};
build_type({atom,_,char}, []) -> {type, char, []};
build_type({atom,_,float}, []) -> {type, float, []};
build_type({atom,_,function}, []) -> {type, 'fun', []};
build_type({atom,_,identifier}, []) -> {type, identifier, []};
build_type({atom,_,integer}, []) -> {type, integer, []};
build_type({atom,_,list}, []) -> {type, list, []};
build_type({atom,_,mfa}, []) -> {type, mfa, []};
build_type({atom,_,neg_integer}, []) -> {type, neg_integer, []};
build_type({atom,_,non_neg_integer}, []) -> {type, non_neg_integer, []};
build_type({atom,_,none}, []) -> {type, none, []};
build_type({atom,_,nonempty_list}, [C,T]) -> {type, cons, [C,T]};
build_type({atom,_,nonempty_possibly_improper_list}, []) -> 
    {type, cons, []};
build_type({atom,_,nonempty_posssibly_improper_list}, [C,T]) -> 
    {type, cons, [C, T]};
build_type({atom,_,number}, []) -> {type, number, []};
build_type({atom,_,pid}, []) -> {type, pid, []};
build_type({atom,_,port}, []) -> {type, port, []};
build_type({atom,_,pos_integer}, []) -> {type, pos_integer, []};
build_type({atom,_,possibly_improper_list}, [C,T]) -> 
    {type, pos_improper_list, [C,T]};
build_type({atom,_,possibly_improper_list}, []) -> 
    {type, pos_improper_list, []};
build_type({atom,_,ref}, []) -> {type, ref, []};
build_type({atom,_,string}, []) -> {type, string, []};
build_type({atom,_,tuple}, []) -> {type, tuple, []};
build_type({atom,_,Other}, []) -> {type, var, [Other]}.

build_typed_attribute({atom,La,record}, [{atom,_Ln,RecordName},RecTuple]) ->
    {attribute,La,record,{RecordName,record_tuple(RecTuple)}};
build_typed_attribute({atom,La,spec}, [{op,_Lo,'/',{atom,_La,FunName},
				                   {integer,_Li,FunArity}},
				       	TypeSpec])  ->
    {attribute,La,type_spec,{{FunName,FunArity},TypeSpec}};
build_typed_attribute({atom,La,_},_) ->
    error_bad_decl(La,spec).

lift_unions(T1, {type, union, List}) ->
    {type, union, [T1|List]};
lift_unions(T1, T2 = {type, _, _}) ->
    {type, union, [T1, T2]}.


%% build_attribute(AttrName, AttrValue) ->
%%	{attribute,Line,module,Module}
%%	{attribute,Line,export,Exports}
%%	{attribute,Line,import,Imports}
%%	{attribute,Line,record,{Name,Inits}}
%%	{attribute,Line,file,{Name,Line}}
%%	{attribute,Line,Name,Val}

build_attribute({atom,La,module}, Val) ->
    case Val of
	[{atom,_Lm,Module}] ->
	    {attribute,La,module,Module};
	[{atom,_Lm,Module},ExpList] ->
	    {attribute,La,module,{Module,var_list(ExpList)}};
	[Name] ->
	    case package_segments(Name) of
		error ->
		    error_bad_decl(La, module);
		Module ->
		    {attribute,La,module,Module}
	    end;
	[Name,ExpList] ->
	    case package_segments(Name) of
		error ->
		    error_bad_decl(La, module);
		Module ->
		    {attribute,La,module,{Module,var_list(ExpList)}}
	    end;
	_Other ->
	    error_bad_decl(La, module)
    end;
build_attribute({atom,La,export}, Val) ->
    case Val of
	[ExpList] ->
	    {attribute,La,export,farity_list(ExpList)};
	_Other -> error_bad_decl(La, export)
    end;
build_attribute({atom,La,import}, Val) ->
    case Val of
	[Name] ->
	    case package_segments(Name) of
		error ->
		    error_bad_decl(La, import);
		Module ->
		    {attribute,La,import,Module}
	    end;
	[{atom,_Lm,Mod},ImpList] ->
	    {attribute,La,import,{Mod,farity_list(ImpList)}};
	[Name, ImpList] ->
	    case package_segments(Name) of
		error ->
		    error_bad_decl(La, import);
		Module ->
		    {attribute,La,import,{Module,farity_list(ImpList)}}
	    end;
	_Other -> error_bad_decl(La, import)
    end;
build_attribute({atom,La,record}, Val) ->
    case Val of
	[{atom,_Ln,Record},RecTuple] ->
	    {attribute,La,record,{Record,record_tuple(RecTuple)}};
	_Other -> error_bad_decl(La, record)
    end;
build_attribute({atom,La,file}, Val) ->
    case Val of
	[{string,_Ln,Name},{integer,_Ll,Line}] ->
	    {attribute,La,file,{Name,Line}};
	_Other -> error_bad_decl(La, file)
    end;
build_attribute({atom,La,Attr}, Val) ->
    case Val of
	[Expr] ->
	    {attribute,La,Attr,term(Expr)};
	_Other -> return_error(La, "bad attribute")
    end.

var_list({cons,_Lc,{var,_,V},Tail}) ->
    [V|var_list(Tail)];
var_list({nil,_Ln}) -> [];
var_list(Other) ->
    return_error(line(Other), "bad variable list").

error_bad_decl(L, S) ->
    return_error(L, io_lib:format("bad ~w declaration", [S])).

farity_list({cons,_Lc,{op,_Lo,'/',{atom,_La,A},{integer,_Li,I}},Tail}) ->
    [{A,I}|farity_list(Tail)];
farity_list({nil,_Ln}) -> [];
farity_list(Other) ->
    return_error(line(Other), "bad function arity").

record_tuple({tuple,_Lt,Fields}) ->
    record_fields(Fields);
record_tuple(Other) ->
    return_error(line(Other), "bad record declaration").

%%record_fields([{atom,La,A}|Fields]) ->
%%    [{record_field,La,{atom,La,A},{atom,La,undefined},{type, any, []}}
%%     |record_fields(Fields)];
%%record_fields([{match,_Lm,{atom,La,A},Expr}|Fields]) ->
%%    [{record_field,La,{atom,La,A},Expr,{type, any, []}}
%%     |record_fields(Fields)];
%%record_fields([{typed,Expr,TypeInfo}|Fields]) ->
%%    [{record_field,La,FieldName,Init,_BogusTypeInfo}] = record_fields([Expr]),
%%    [{record_field,La,FieldName,Init,TypeInfo}|record_fields(Fields)];
%%record_fields([Other|_Fields]) ->
%%    return_error(line(Other), "bad record field");
%%record_fields([]) -> [].

record_fields([{atom,La,A}|Fields]) ->
    [{record_field,La,{atom,La,A}}|record_fields(Fields)];
record_fields([{match,_Lm,{atom,La,A},Expr}|Fields]) ->
    [{record_field,La,{atom,La,A},Expr}|record_fields(Fields)];
record_fields([{typed,Expr,TypeInfo}|Fields]) ->
    [Field] = record_fields([Expr]),
    TypeInfo1 = 
	case Expr of
	    {match, _, _, _} -> TypeInfo; %% If we have an initializer.
	    {atom, _, _} -> lift_unions({type, atom, ['undefined']}, TypeInfo)
	end, 
    [{typed_record_field,Field,TypeInfo1}|record_fields(Fields)];
record_fields([Other|_Fields]) ->
    return_error(line(Other), "bad record field");
record_fields([]) -> [].

term(Expr) ->
    case catch normalise(Expr) of
	{'EXIT',_R} -> return_error(line(Expr), "bad attribute");
	Term -> Term
    end.

package_segments(Name) ->
    package_segments(Name, [], []).

package_segments({record_field, _, F1, F2}, Fs, As) ->
    package_segments(F1, [F2 | Fs], As);
package_segments({atom, _, A}, [F | Fs], As) ->
    package_segments(F, Fs, [A | As]);
package_segments({atom, _, A}, [], As) ->
    lists:reverse([A | As]);
package_segments(_, _, _) ->
    error.

%% build_function([Clause]) -> {function,Line,Name,Arity,[Clause]}

build_function(Cs) ->
    Name = element(3, hd(Cs)),
    Arity = length(element(4, hd(Cs))),
    {function,line(hd(Cs)),Name,Arity,check_clauses(Cs, Name, Arity)}.

%% build_rule([Clause]) -> {rule,Line,Name,Arity,[Clause]'}

build_rule(Cs) ->
    Name = element(3, hd(Cs)),
    Arity = length(element(4, hd(Cs))),
    {rule,line(hd(Cs)),Name,Arity,check_clauses(Cs, Name, Arity)}.

%% build_fun(Line, [Clause]) -> {'fun',Line,{clauses,[Clause]}}.

build_fun(Line, Cs) ->
    Arity = length(element(4, hd(Cs))),
    {'fun',Line,{clauses,check_clauses(Cs, 'fun', Arity)}}.

check_clauses(Cs, Name, Arity) ->
     mapl(fun ({clause,L,N,As,G,B}) when N =:= Name, length(As) =:= Arity ->
		 {clause,L,As,G,B};
	     ({clause,L,_N,_As,_G,_B}) ->
		 return_error(L, "head mismatch") end, Cs).

build_try(L,Es,Scs,{Ccs,As}) ->
    {'try',L,Es,Scs,Ccs,As}.

%% mapl(F,List)
%% an alternative map which always maps from left to right
%% and makes it possible to interrupt the mapping with throw on
%% the first occurence from left as expected.
%% can be removed when the jam machine (and all other machines)
%% uses the standardized (Erlang 5.0) evaluation order (from left to right)
mapl(F, [H|T]) ->
	V = F(H),
	[V | mapl(F,T)];
mapl(_, []) ->
	[].

%% normalise(AbsTerm)
%% abstract(Term)
%%  Convert between the abstract form of a term and a term.

normalise({char,_,C}) -> C;
normalise({integer,_,I}) -> I;
normalise({float,_,F}) -> F;
normalise({atom,_,A}) -> A;
normalise({string,_,S}) -> S;
normalise({nil,_}) -> [];
normalise({bin,_,Fs}) ->
    {value, B, _} =
	eval_bits:expr_grp(Fs, [],
			   fun(E, _) ->
				   {value, normalise(E), []}
			   end, [], true),
    B;
normalise({cons,_,Head,Tail}) ->
    [normalise(Head)|normalise(Tail)];
normalise({tuple,_,Args}) ->
    list_to_tuple(normalise_list(Args));
%% Atom dot-notation, as in 'foo.bar.baz'
normalise({record_field,_,_,_}=A) ->
    case package_segments(A) of
	error -> erlang:fault({badarg, A});
	As -> list_to_atom(packages:concat(As))
    end;
%% Special case for unary +/-.
normalise({op,_,'+',{char,_,I}}) -> I;
normalise({op,_,'+',{integer,_,I}}) -> I;
normalise({op,_,'+',{float,_,F}}) -> F;
normalise({op,_,'-',{char,_,I}}) -> -I;		%Weird, but compatible!
normalise({op,_,'-',{integer,_,I}}) -> -I;
normalise({op,_,'-',{float,_,F}}) -> -F;
normalise(X) -> erlang:fault({badarg, X}).

normalise_list([H|T]) ->
    [normalise(H)|normalise_list(T)];
normalise_list([]) ->
    [].

abstract(T) when is_integer(T) -> {integer,0,T};
abstract(T) when is_float(T) -> {float,0,T};
abstract(T) when is_atom(T) -> {atom,0,T};
abstract([]) -> {nil,0};
abstract(B) when is_binary(B) ->
    {bin, 0, lists:map(fun(Byte) ->
			       {bin_element, 0,
				{integer, 0, Byte}, default, default}
		       end,
		       binary_to_list(B))};
abstract([C|T]) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C]);
abstract([H|T]) ->
    {cons,0,abstract(H),abstract(T)};
abstract(Tuple) when is_tuple(Tuple) ->
    {tuple,0,abstract_list(tuple_to_list(Tuple))}.

abstract_string([C|T], String) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C|String]);
abstract_string([], String) ->
    {string, 0, lists:reverse(String)};
abstract_string(T, String) ->
    not_string(String, abstract(T)).

not_string([C|T], Result) ->
    not_string(T, {cons, 0, {integer, 0, C}, Result});
not_string([], Result) ->
    Result.

abstract_list([H|T]) ->
    [abstract(H)|abstract_list(T)];
abstract_list([]) ->
    [].

%%% abstract/2 keeps the line number
abstract(T, Line) when is_integer(T) -> {integer,Line,T};
abstract(T, Line) when is_float(T) -> {float,Line,T};
abstract(T, Line) when is_atom(T) -> {atom,Line,T};
abstract([], Line) -> {nil,Line};
abstract(B, Line) when is_binary(B) ->
    {bin, Line, lists:map(fun(Byte) ->
			       {bin_element, Line,
				{integer, Line, Byte}, default, default}
		       end,
		       binary_to_list(B))};
abstract([C|T], Line) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C], Line);
abstract([H|T], Line) ->
    {cons,Line,abstract(H, Line),abstract(T, Line)};
abstract(Tuple, Line) when is_tuple(Tuple) ->
    {tuple,Line,abstract_list(tuple_to_list(Tuple), Line)}.

abstract_string([C|T], String, Line) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C|String], Line);
abstract_string([], String, Line) ->
    {string, Line, lists:reverse(String)};
abstract_string(T, String, Line) ->
    not_string(String, abstract(T, Line), Line).

not_string([C|T], Result, Line) ->
    not_string(T, {cons, Line, {integer, Line, C}, Result}, Line);
not_string([], Result, _Line) ->
    Result.

abstract_list([H|T], Line) ->
    [abstract(H, Line)|abstract_list(T, Line)];
abstract_list([], _Line) ->
    [].

%% tokens(AbsTerm) -> [Token]
%% tokens(AbsTerm, More) -> [Token]
%%  Generate a list of tokens representing the abstract term.

tokens(Abs) ->
    tokens(Abs, []).

tokens({char,L,C}, More) -> [{char,L,C}|More];
tokens({integer,L,N}, More) -> [{integer,L,N}|More];
tokens({float,L,F}, More) -> [{float,L,F}|More];
tokens({atom,L,A}, More) -> [{atom,L,A}|More];
tokens({var,L,V}, More) -> [{var,L,V}|More];
tokens({string,L,S}, More) -> [{string,L,S}|More];
tokens({nil,L}, More) -> [{'[',L},{']',L}|More];
tokens({cons,L,Head,Tail}, More) ->
    [{'[',L}|tokens(Head, tokens_tail(Tail, More))];
tokens({tuple,L,[]}, More) ->
    [{'{',L},{'}',L}|More];
tokens({tuple,L,[E|Es]}, More) ->
    [{'{',L}|tokens(E, tokens_tuple(Es, line(E), More))].

tokens_tail({cons,L,Head,Tail}, More) ->
    [{',',L}|tokens(Head, tokens_tail(Tail, More))];
tokens_tail({nil,L}, More) ->
    [{']',L}|More];
tokens_tail(Other, More) ->
    L = line(Other),
    [{'|',L}|tokens(Other, [{']',L}|More])].

tokens_tuple([E|Es], Line, More) ->
    [{',',Line}|tokens(E, tokens_tuple(Es, line(E), More))];
tokens_tuple([], Line, More) ->
    [{'}',Line}|More].

%% Give the relative precedences of operators.

inop_prec('=') -> {150,100,100};
inop_prec('!') -> {150,100,100};
inop_prec('orelse') -> {160,150,150};
inop_prec('andalso') -> {200,160,160};
inop_prec('==') -> {300,200,300};
inop_prec('/=') -> {300,200,300};
inop_prec('=<') -> {300,200,300};
inop_prec('<') -> {300,200,300};
inop_prec('>=') -> {300,200,300};
inop_prec('>') -> {300,200,300};
inop_prec('=:=') -> {300,200,300};
inop_prec('=/=') -> {300,200,300};
inop_prec('++') -> {400,300,300};
inop_prec('--') -> {400,300,300};
inop_prec('+') -> {400,400,500};
inop_prec('-') -> {400,400,500};
inop_prec('bor') -> {400,400,500};
inop_prec('bxor') -> {400,400,500};
inop_prec('bsl') -> {400,400,500};
inop_prec('bsr') -> {400,400,500};
inop_prec('or') -> {400,400,500};
inop_prec('xor') -> {400,400,500};
inop_prec('*') -> {500,500,600};
inop_prec('/') -> {500,500,600};
inop_prec('div') -> {500,500,600};
inop_prec('rem') -> {500,500,600};
inop_prec('band') -> {500,500,600};
inop_prec('and') -> {500,500,600};
inop_prec('#') -> {800,700,800};
inop_prec(':') -> {900,800,900};
inop_prec('.') -> {900,900,1000}.

preop_prec('catch') -> {0,100};
preop_prec('+') -> {600,700};
preop_prec('-') -> {600,700};
preop_prec('bnot') -> {600,700};
preop_prec('not') -> {600,700};
preop_prec('#') -> {700,800}.

func_prec() -> {800,700}.

max_prec() -> 1000.

-file("/ldisk/daily_build/otp_prebuild_r11b.2007-06-11_19/otp_src_R11B-5/bootstrap/lib/parsetools/include/yeccpre.hrl", 0).
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

parse(Tokens) ->
    yeccpars0(Tokens, false).

parse_and_scan({F, A}) -> % Fun or {M, F}
    yeccpars0([], {F, A});
parse_and_scan({M, F, A}) ->
    yeccpars0([], {{M, F}, A}).

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
	true ->
	    Message;
	_ ->
	    io_lib:write(Message)
    end.

% To be used in grammar files to throw an error message to the parser
% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function,{return_error,2}}).
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

yeccpars0(Tokens, MFA) ->
    try yeccpars1(Tokens, MFA, 0, [], [])
    catch 
        throw: {error, {_Line, ?MODULE, _M}} = Error -> 
                   Error % probably from return_error/1
    end.

% Don't change yeccpars1/6 too much, it is called recursively by yeccpars2/8!
yeccpars1([Token | Tokens], Tokenizer, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens,
	      Tokenizer);
yeccpars1([], {F, A}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, _Endline} ->
	    yeccpars1(Tokens, {F, A}, State, States, Vstack);
        {eof, _Endline} ->
            yeccpars1([], false, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], false, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, {'$end', 999999}, [], false).

% For internal use only.
yeccerror(Token) ->
    {error,
     {element(2, Token), ?MODULE,
      ["syntax error before: ", yecctoken2string(Token)]}}.

yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format('~s', [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:format('~w', [A]);
yecctoken2string({_Cat, _, Val}) -> io_lib:format('~w', [Val]);
yecctoken2string({'dot', _}) -> io_lib:format('~w', ['.']);
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:format('~w', [Other]);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("./erl_parse.erl", 584).

yeccpars2(0, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(1, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_1_(__Stack),
 yeccpars2(yeccgoto(rule, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(2, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 374, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_2_(__Stack),
 yeccpars2(yeccgoto(rule_clauses, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(3, dot, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 373, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(4, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_4_(__Stack),
 yeccpars2(yeccgoto(function, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(5, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 368, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_5_(__Stack),
 yeccpars2(yeccgoto(function_clauses, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(6, dot, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 367, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(7, '$end', _, __Stack, _, _, _) ->
 {ok, hd(__Stack)};
yeccpars2(7, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(8, dot, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 366, [8 | __Ss], [__T | __Stack]);
yeccpars2(8, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(9, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 287, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(10, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [10 | __Ss], [__T | __Stack]);
yeccpars2(10, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(11, 'when', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 84, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_11_(__Stack),
 yeccpars2(282, __Cat, [11 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(12, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_12_(__Stack),
 yeccpars2(yeccgoto(clause_args, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(13, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(14, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(15, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(16, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(atomic, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(17, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_700, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(18, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(19, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(20, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(21, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(22, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(23, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(24, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_700, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(25, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(26, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 280, [26 | __Ss], [__T | __Stack]);
yeccpars2(26, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(27, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 275, [27 | __Ss], [__T | __Stack]);
yeccpars2(27, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_900, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(28, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 271, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 272, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_800, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(29, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_700, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(30, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_600, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(31, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_500, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(32, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 262, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 263, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, 'and', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 264, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, 'band', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 265, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, 'div', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 266, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, 'rem', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 267, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_400, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(33, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 250, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, '++', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 251, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 252, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, '--', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 253, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'bor', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 254, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'bsl', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 255, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'bsr', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 256, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'bxor', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 257, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'or', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 258, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'xor', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 259, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_300, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(34, '/=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 239, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 240, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, '=/=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 241, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, '=:=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 242, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, '=<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 243, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, '==', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 244, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 245, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, '>=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 246, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_200, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(35, 'andalso', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 236, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_160, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(36, 'orelse', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 234, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_150, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(37, '!', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 230, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 231, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_100, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(38, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(39, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 228, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_39_(__Stack),
 yeccpars2(yeccgoto(exprs, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(40, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(41, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(42, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(43, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(44, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 211, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(45, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(46, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_46_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(argument_list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(47, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(prefix_op, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(48, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(prefix_op, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(49, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 208, [49 | __Ss], [__T | __Stack]);
yeccpars2(49, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(50, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, '>>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 187, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(51, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 172, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(52, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(atomic, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(53, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(54, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(prefix_op, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(55, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(56, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(57, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(atomic, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(58, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(atomic, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(59, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 152, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(60, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(61, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(atomic, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(62, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(prefix_op, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(63, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(64, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, 'after', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(65, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(strings, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(66, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(67, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(68, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(69, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [69 | __Ss], [__T | __Stack]);
yeccpars2(69, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(70, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_70_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(tuple, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(71, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_71_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tuple, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(72, 'after', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'of', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(73, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_73_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(try_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(74, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(75, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 96, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(76, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(77, 'when', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 84, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_77_(__Stack),
 yeccpars2(83, __Cat, [77 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(78, 'after', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(79, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 80, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_79_(__Stack),
 yeccpars2(yeccgoto(cr_clauses, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(80, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(81, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_81_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(cr_clauses, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(82, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_82_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(try_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(83, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 90, [83 | __Ss], [__T | __Stack]);
yeccpars2(83, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(84, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(85, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_85_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(clause_guard, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(86, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 87, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_86_(__Stack),
 yeccpars2(yeccgoto(guard, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(87, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(88, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_88_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(guard, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(89, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_89_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(cr_clause, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(90, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(91, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_91_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(clause_body, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(92, 'after', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 109, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, 'end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 110, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(93, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_93_(__Stack),
 yeccpars2(yeccgoto(try_clauses, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(94, 'when', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 84, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_94_(__Stack),
 yeccpars2(105, __Cat, [94 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(95, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [95 | __Ss], [__T | __Stack]);
yeccpars2(95, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(atomic, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(96, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 97, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(97, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(98, 'when', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 84, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_98_(__Stack),
 yeccpars2(99, __Cat, [98 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(99, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 90, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(100, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_100_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(try_clause, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(101, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(102, 'when', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 84, [102 | __Ss], [__T | __Stack]);
yeccpars2(102, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_102_(__Stack),
 yeccpars2(103, __Cat, [102 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(103, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 90, [103 | __Ss], [__T | __Stack]);
yeccpars2(103, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(104, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_104_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(try_clause, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(105, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 90, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(106, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_106_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(try_clause, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(107, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 96, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(108, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_108_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(try_clauses, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(109, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(110, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_110_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(try_catch, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(111, 'end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [111 | __Ss], [__T | __Stack]);
yeccpars2(111, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(112, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_112_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(try_catch, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(113, 'end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 114, [113 | __Ss], [__T | __Stack]);
yeccpars2(113, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(114, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_114_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(try_catch, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(115, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_115_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(strings, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(116, 'after', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 121, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, 'end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 122, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(117, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(118, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 90, [118 | __Ss], [__T | __Stack]);
yeccpars2(118, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(119, 'end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 120, [119 | __Ss], [__T | __Stack]);
yeccpars2(119, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(120, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_120_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(receive_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(121, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(122, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_122_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(receive_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(123, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 90, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(124, 'end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 125, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(125, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_125_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(receive_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(126, 'end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [126 | __Ss], [__T | __Stack]);
yeccpars2(126, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(127, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(128, '||', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [128 | __Ss], [__T | __Stack]);
yeccpars2(128, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(129, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(130, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(131, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [131 | __Ss], [__T | __Stack]);
yeccpars2(131, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_131_(__Stack),
 yeccpars2(yeccgoto(lc_exprs, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(132, '<-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [132 | __Ss], [__T | __Stack]);
yeccpars2(132, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lc_expr, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(133, '<=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(134, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(135, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_135_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(lc_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(136, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(137, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_137_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(lc_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(138, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(139, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_139_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(lc_exprs, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(140, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_140_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(list_comprehension, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(141, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_141_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(query_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(142, 'end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 148, [142 | __Ss], [__T | __Stack]);
yeccpars2(142, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(143, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 146, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_143_(__Stack),
 yeccpars2(yeccgoto(if_clauses, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(144, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 90, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(145, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_145_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(if_clause, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(146, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(147, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_147_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(if_clauses, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(148, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_148_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(if_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(149, 'end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 163, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(150, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 161, [150 | __Ss], [__T | __Stack]);
yeccpars2(150, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_150_(__Stack),
 yeccpars2(yeccgoto(fun_clauses, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(151, 'when', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 84, [151 | __Ss], [__T | __Stack]);
yeccpars2(151, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_151_(__Stack),
 yeccpars2(159, __Cat, [151 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(152, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 153, [152 | __Ss], [__T | __Stack]);
yeccpars2(152, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 154, [152 | __Ss], [__T | __Stack]);
yeccpars2(152, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(153, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 158, [153 | __Ss], [__T | __Stack]);
yeccpars2(153, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(154, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 155, [154 | __Ss], [__T | __Stack]);
yeccpars2(154, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(155, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 156, [155 | __Ss], [__T | __Stack]);
yeccpars2(155, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(156, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 157, [156 | __Ss], [__T | __Stack]);
yeccpars2(156, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(157, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_157_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(fun_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(158, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_158_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(fun_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(159, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 90, [159 | __Ss], [__T | __Stack]);
yeccpars2(159, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(160, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_160_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(fun_clause, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(161, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(162, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_162_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(fun_clauses, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(163, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_163_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(fun_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(164, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_164_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(165, 'of', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 166, [165 | __Ss], [__T | __Stack]);
yeccpars2(165, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(166, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(167, 'end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 168, [167 | __Ss], [__T | __Stack]);
yeccpars2(167, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(168, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_168_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(case_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(169, 'end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 170, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(170, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_170_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr_max, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(171, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 174, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 175, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 176, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, '||', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(172, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_172_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(173, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_173_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(174, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(175, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_175_(__Stack),
 yeccpars2(yeccgoto(tail, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(176, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(177, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 178, [177 | __Ss], [__T | __Stack]);
yeccpars2(177, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(178, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_178_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tail, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(179, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 174, [179 | __Ss], [__T | __Stack]);
yeccpars2(179, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 175, [179 | __Ss], [__T | __Stack]);
yeccpars2(179, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 176, [179 | __Ss], [__T | __Stack]);
yeccpars2(179, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(180, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_180_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tail, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(181, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(182, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(bit_expr, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(183, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 195, [183 | __Ss], [__T | __Stack]);
yeccpars2(183, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_183_(__Stack),
 yeccpars2(194, __Cat, [183 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(184, '||', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 191, [184 | __Ss], [__T | __Stack]);
yeccpars2(184, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr_max, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(185, '>>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 190, [185 | __Ss], [__T | __Stack]);
yeccpars2(185, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(186, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 188, [186 | __Ss], [__T | __Stack]);
yeccpars2(186, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_186_(__Stack),
 yeccpars2(yeccgoto(bin_elements, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(187, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_187_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(binary, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(188, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [188 | __Ss], [__T | __Stack]);
yeccpars2(188, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [188 | __Ss], [__T | __Stack]);
yeccpars2(188, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [188 | __Ss], [__T | __Stack]);
yeccpars2(188, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [188 | __Ss], [__T | __Stack]);
yeccpars2(188, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [188 | __Ss], [__T | __Stack]);
yeccpars2(188, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [188 | __Ss], [__T | __Stack]);
yeccpars2(188, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [188 | __Ss], [__T | __Stack]);
yeccpars2(188, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [188 | __Ss], [__T | __Stack]);
yeccpars2(188, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [188 | __Ss], [__T | __Stack]);
yeccpars2(188, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [188 | __Ss], [__T | __Stack]);
yeccpars2(188, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [188 | __Ss], [__T | __Stack]);
yeccpars2(188, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [188 | __Ss], [__T | __Stack]);
yeccpars2(188, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [188 | __Ss], [__T | __Stack]);
yeccpars2(188, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [188 | __Ss], [__T | __Stack]);
yeccpars2(188, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [188 | __Ss], [__T | __Stack]);
yeccpars2(188, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [188 | __Ss], [__T | __Stack]);
yeccpars2(188, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [188 | __Ss], [__T | __Stack]);
yeccpars2(188, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [188 | __Ss], [__T | __Stack]);
yeccpars2(188, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [188 | __Ss], [__T | __Stack]);
yeccpars2(188, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [188 | __Ss], [__T | __Stack]);
yeccpars2(188, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [188 | __Ss], [__T | __Stack]);
yeccpars2(188, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(189, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_189_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(bin_elements, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(190, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_190_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(binary, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(191, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(192, '>>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 193, [192 | __Ss], [__T | __Stack]);
yeccpars2(192, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(193, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_193_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(binary_comprehension, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(194, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 199, [194 | __Ss], [__T | __Stack]);
yeccpars2(194, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_194_(__Stack),
 yeccpars2(198, __Cat, [194 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(195, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(196, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(bit_size_expr, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(197, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_197_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(opt_bit_size_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(198, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_198_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(bin_element, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(199, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 202, [199 | __Ss], [__T | __Stack]);
yeccpars2(199, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(200, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_200_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(opt_bit_type_list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(201, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 205, [201 | __Ss], [__T | __Stack]);
yeccpars2(201, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_201_(__Stack),
 yeccpars2(yeccgoto(bit_type_list, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(202, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 203, [202 | __Ss], [__T | __Stack]);
yeccpars2(202, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_202_(__Stack),
 yeccpars2(yeccgoto(bit_type, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(203, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 204, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(204, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_204_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(bit_type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(205, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 202, [205 | __Ss], [__T | __Stack]);
yeccpars2(205, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(206, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_206_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(bit_type_list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(207, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_207_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(bit_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(208, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_208_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(expr_900, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(209, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 210, [209 | __Ss], [__T | __Stack]);
yeccpars2(209, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(210, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_210_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr_max, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(211, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 213, [211 | __Ss], [__T | __Stack]);
yeccpars2(211, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 214, [211 | __Ss], [__T | __Stack]);
yeccpars2(211, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(212, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_212_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(record_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(213, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 227, [213 | __Ss], [__T | __Stack]);
yeccpars2(213, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(214, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 217, [214 | __Ss], [__T | __Stack]);
yeccpars2(214, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 218, [214 | __Ss], [__T | __Stack]);
yeccpars2(214, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 219, [214 | __Ss], [__T | __Stack]);
yeccpars2(214, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(215, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 226, [215 | __Ss], [__T | __Stack]);
yeccpars2(215, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(216, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 224, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_216_(__Stack),
 yeccpars2(yeccgoto(record_fields, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(217, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 222, [217 | __Ss], [__T | __Stack]);
yeccpars2(217, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(218, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 220, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(219, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_219_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(record_tuple, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(220, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(221, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_221_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(record_field, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(222, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(223, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_223_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(record_field, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(224, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 217, [224 | __Ss], [__T | __Stack]);
yeccpars2(224, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 218, [224 | __Ss], [__T | __Stack]);
yeccpars2(224, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(225, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_225_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(record_fields, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(226, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_226_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(record_tuple, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(227, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_227_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(record_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(228, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(229, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_229_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(exprs, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(230, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(231, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(232, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_232_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr_100, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(233, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_233_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr_100, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(234, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [234 | __Ss], [__T | __Stack]);
yeccpars2(234, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [234 | __Ss], [__T | __Stack]);
yeccpars2(234, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [234 | __Ss], [__T | __Stack]);
yeccpars2(234, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [234 | __Ss], [__T | __Stack]);
yeccpars2(234, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [234 | __Ss], [__T | __Stack]);
yeccpars2(234, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [234 | __Ss], [__T | __Stack]);
yeccpars2(234, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [234 | __Ss], [__T | __Stack]);
yeccpars2(234, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [234 | __Ss], [__T | __Stack]);
yeccpars2(234, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [234 | __Ss], [__T | __Stack]);
yeccpars2(234, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [234 | __Ss], [__T | __Stack]);
yeccpars2(234, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [234 | __Ss], [__T | __Stack]);
yeccpars2(234, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [234 | __Ss], [__T | __Stack]);
yeccpars2(234, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [234 | __Ss], [__T | __Stack]);
yeccpars2(234, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [234 | __Ss], [__T | __Stack]);
yeccpars2(234, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [234 | __Ss], [__T | __Stack]);
yeccpars2(234, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [234 | __Ss], [__T | __Stack]);
yeccpars2(234, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [234 | __Ss], [__T | __Stack]);
yeccpars2(234, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [234 | __Ss], [__T | __Stack]);
yeccpars2(234, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [234 | __Ss], [__T | __Stack]);
yeccpars2(234, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [234 | __Ss], [__T | __Stack]);
yeccpars2(234, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [234 | __Ss], [__T | __Stack]);
yeccpars2(234, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [234 | __Ss], [__T | __Stack]);
yeccpars2(234, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [234 | __Ss], [__T | __Stack]);
yeccpars2(234, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(235, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_235_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr_150, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(236, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(237, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_237_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr_160, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(238, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [238 | __Ss], [__T | __Stack]);
yeccpars2(238, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [238 | __Ss], [__T | __Stack]);
yeccpars2(238, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [238 | __Ss], [__T | __Stack]);
yeccpars2(238, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [238 | __Ss], [__T | __Stack]);
yeccpars2(238, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [238 | __Ss], [__T | __Stack]);
yeccpars2(238, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [238 | __Ss], [__T | __Stack]);
yeccpars2(238, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [238 | __Ss], [__T | __Stack]);
yeccpars2(238, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [238 | __Ss], [__T | __Stack]);
yeccpars2(238, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [238 | __Ss], [__T | __Stack]);
yeccpars2(238, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [238 | __Ss], [__T | __Stack]);
yeccpars2(238, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [238 | __Ss], [__T | __Stack]);
yeccpars2(238, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [238 | __Ss], [__T | __Stack]);
yeccpars2(238, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [238 | __Ss], [__T | __Stack]);
yeccpars2(238, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [238 | __Ss], [__T | __Stack]);
yeccpars2(238, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [238 | __Ss], [__T | __Stack]);
yeccpars2(238, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [238 | __Ss], [__T | __Stack]);
yeccpars2(238, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [238 | __Ss], [__T | __Stack]);
yeccpars2(238, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [238 | __Ss], [__T | __Stack]);
yeccpars2(238, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [238 | __Ss], [__T | __Stack]);
yeccpars2(238, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [238 | __Ss], [__T | __Stack]);
yeccpars2(238, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [238 | __Ss], [__T | __Stack]);
yeccpars2(238, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [238 | __Ss], [__T | __Stack]);
yeccpars2(238, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [238 | __Ss], [__T | __Stack]);
yeccpars2(238, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(239, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(comp_op, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(240, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(comp_op, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(241, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(comp_op, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(242, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(comp_op, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(243, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(comp_op, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(244, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(comp_op, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(245, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(comp_op, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(246, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(comp_op, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(247, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_247_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr_200, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(248, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [248 | __Ss], [__T | __Stack]);
yeccpars2(248, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [248 | __Ss], [__T | __Stack]);
yeccpars2(248, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [248 | __Ss], [__T | __Stack]);
yeccpars2(248, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [248 | __Ss], [__T | __Stack]);
yeccpars2(248, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [248 | __Ss], [__T | __Stack]);
yeccpars2(248, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [248 | __Ss], [__T | __Stack]);
yeccpars2(248, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [248 | __Ss], [__T | __Stack]);
yeccpars2(248, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [248 | __Ss], [__T | __Stack]);
yeccpars2(248, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [248 | __Ss], [__T | __Stack]);
yeccpars2(248, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [248 | __Ss], [__T | __Stack]);
yeccpars2(248, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [248 | __Ss], [__T | __Stack]);
yeccpars2(248, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [248 | __Ss], [__T | __Stack]);
yeccpars2(248, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [248 | __Ss], [__T | __Stack]);
yeccpars2(248, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [248 | __Ss], [__T | __Stack]);
yeccpars2(248, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [248 | __Ss], [__T | __Stack]);
yeccpars2(248, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [248 | __Ss], [__T | __Stack]);
yeccpars2(248, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [248 | __Ss], [__T | __Stack]);
yeccpars2(248, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [248 | __Ss], [__T | __Stack]);
yeccpars2(248, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [248 | __Ss], [__T | __Stack]);
yeccpars2(248, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [248 | __Ss], [__T | __Stack]);
yeccpars2(248, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [248 | __Ss], [__T | __Stack]);
yeccpars2(248, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [248 | __Ss], [__T | __Stack]);
yeccpars2(248, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [248 | __Ss], [__T | __Stack]);
yeccpars2(248, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(249, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [249 | __Ss], [__T | __Stack]);
yeccpars2(249, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [249 | __Ss], [__T | __Stack]);
yeccpars2(249, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [249 | __Ss], [__T | __Stack]);
yeccpars2(249, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [249 | __Ss], [__T | __Stack]);
yeccpars2(249, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [249 | __Ss], [__T | __Stack]);
yeccpars2(249, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [249 | __Ss], [__T | __Stack]);
yeccpars2(249, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [249 | __Ss], [__T | __Stack]);
yeccpars2(249, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [249 | __Ss], [__T | __Stack]);
yeccpars2(249, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [249 | __Ss], [__T | __Stack]);
yeccpars2(249, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [249 | __Ss], [__T | __Stack]);
yeccpars2(249, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [249 | __Ss], [__T | __Stack]);
yeccpars2(249, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [249 | __Ss], [__T | __Stack]);
yeccpars2(249, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [249 | __Ss], [__T | __Stack]);
yeccpars2(249, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [249 | __Ss], [__T | __Stack]);
yeccpars2(249, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [249 | __Ss], [__T | __Stack]);
yeccpars2(249, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [249 | __Ss], [__T | __Stack]);
yeccpars2(249, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [249 | __Ss], [__T | __Stack]);
yeccpars2(249, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [249 | __Ss], [__T | __Stack]);
yeccpars2(249, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [249 | __Ss], [__T | __Stack]);
yeccpars2(249, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [249 | __Ss], [__T | __Stack]);
yeccpars2(249, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [249 | __Ss], [__T | __Stack]);
yeccpars2(249, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [249 | __Ss], [__T | __Stack]);
yeccpars2(249, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [249 | __Ss], [__T | __Stack]);
yeccpars2(249, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(250, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(add_op, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(251, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(list_op, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(252, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(add_op, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(253, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(list_op, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(254, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(add_op, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(255, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(add_op, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(256, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(add_op, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(257, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(add_op, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(258, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(add_op, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(259, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(add_op, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(260, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 262, [260 | __Ss], [__T | __Stack]);
yeccpars2(260, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 263, [260 | __Ss], [__T | __Stack]);
yeccpars2(260, 'and', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 264, [260 | __Ss], [__T | __Stack]);
yeccpars2(260, 'band', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 265, [260 | __Ss], [__T | __Stack]);
yeccpars2(260, 'div', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 266, [260 | __Ss], [__T | __Stack]);
yeccpars2(260, 'rem', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 267, [260 | __Ss], [__T | __Stack]);
yeccpars2(260, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_260_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr_400, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(261, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(262, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(mult_op, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(263, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(mult_op, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(264, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(mult_op, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(265, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(mult_op, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(266, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(mult_op, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(267, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(mult_op, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(268, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_268_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr_500, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(269, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_269_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr_300, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(270, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_270_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(function_call, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(271, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 274, [271 | __Ss], [__T | __Stack]);
yeccpars2(271, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(272, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [272 | __Ss], [__T | __Stack]);
yeccpars2(272, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [272 | __Ss], [__T | __Stack]);
yeccpars2(272, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [272 | __Ss], [__T | __Stack]);
yeccpars2(272, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [272 | __Ss], [__T | __Stack]);
yeccpars2(272, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [272 | __Ss], [__T | __Stack]);
yeccpars2(272, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [272 | __Ss], [__T | __Stack]);
yeccpars2(272, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [272 | __Ss], [__T | __Stack]);
yeccpars2(272, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [272 | __Ss], [__T | __Stack]);
yeccpars2(272, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [272 | __Ss], [__T | __Stack]);
yeccpars2(272, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [272 | __Ss], [__T | __Stack]);
yeccpars2(272, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [272 | __Ss], [__T | __Stack]);
yeccpars2(272, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [272 | __Ss], [__T | __Stack]);
yeccpars2(272, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [272 | __Ss], [__T | __Stack]);
yeccpars2(272, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [272 | __Ss], [__T | __Stack]);
yeccpars2(272, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [272 | __Ss], [__T | __Stack]);
yeccpars2(272, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [272 | __Ss], [__T | __Stack]);
yeccpars2(272, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [272 | __Ss], [__T | __Stack]);
yeccpars2(272, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(273, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_273_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr_800, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(274, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_274_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr_900, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(275, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 276, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(276, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 278, [276 | __Ss], [__T | __Stack]);
yeccpars2(276, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 214, [276 | __Ss], [__T | __Stack]);
yeccpars2(276, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(277, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_277_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(record_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(278, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 279, [278 | __Ss], [__T | __Stack]);
yeccpars2(278, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(279, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_279_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(record_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(280, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_280_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(argument_list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(281, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_281_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(expr_600, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(282, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 90, [282 | __Ss], [__T | __Stack]);
yeccpars2(282, ':-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 285, [282 | __Ss], [__T | __Stack]);
yeccpars2(282, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(283, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_283_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(rule_clause, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(284, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_284_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(function_clause, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(285, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(286, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_286_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(rule_body, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(287, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 288, [287 | __Ss], [__T | __Stack]);
yeccpars2(287, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(288, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [288 | __Ss], [__T | __Stack]);
yeccpars2(288, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [288 | __Ss], [__T | __Stack]);
yeccpars2(288, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [288 | __Ss], [__T | __Stack]);
yeccpars2(288, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [288 | __Ss], [__T | __Stack]);
yeccpars2(288, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [288 | __Ss], [__T | __Stack]);
yeccpars2(288, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [288 | __Ss], [__T | __Stack]);
yeccpars2(288, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [288 | __Ss], [__T | __Stack]);
yeccpars2(288, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [288 | __Ss], [__T | __Stack]);
yeccpars2(288, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [288 | __Ss], [__T | __Stack]);
yeccpars2(288, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [288 | __Ss], [__T | __Stack]);
yeccpars2(288, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [288 | __Ss], [__T | __Stack]);
yeccpars2(288, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [288 | __Ss], [__T | __Stack]);
yeccpars2(288, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [288 | __Ss], [__T | __Stack]);
yeccpars2(288, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [288 | __Ss], [__T | __Stack]);
yeccpars2(288, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [288 | __Ss], [__T | __Stack]);
yeccpars2(288, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [288 | __Ss], [__T | __Stack]);
yeccpars2(288, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [288 | __Ss], [__T | __Stack]);
yeccpars2(288, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [288 | __Ss], [__T | __Stack]);
yeccpars2(288, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [288 | __Ss], [__T | __Stack]);
yeccpars2(288, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [288 | __Ss], [__T | __Stack]);
yeccpars2(288, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [288 | __Ss], [__T | __Stack]);
yeccpars2(288, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [288 | __Ss], [__T | __Stack]);
yeccpars2(288, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [288 | __Ss], [__T | __Stack]);
yeccpars2(288, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [288 | __Ss], [__T | __Stack]);
yeccpars2(288, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(289, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 365, [289 | __Ss], [__T | __Stack]);
yeccpars2(289, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(290, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(attr_val, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(291, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 294, [291 | __Ss], [__T | __Stack]);
yeccpars2(291, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 295, [291 | __Ss], [__T | __Stack]);
yeccpars2(291, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_291_(__Stack),
 yeccpars2(yeccgoto(exprs, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(292, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 293, [292 | __Ss], [__T | __Stack]);
yeccpars2(292, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(293, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_293_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(attribute, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(294, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [294 | __Ss], [__T | __Stack]);
yeccpars2(294, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [294 | __Ss], [__T | __Stack]);
yeccpars2(294, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [294 | __Ss], [__T | __Stack]);
yeccpars2(294, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [294 | __Ss], [__T | __Stack]);
yeccpars2(294, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [294 | __Ss], [__T | __Stack]);
yeccpars2(294, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [294 | __Ss], [__T | __Stack]);
yeccpars2(294, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [294 | __Ss], [__T | __Stack]);
yeccpars2(294, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [294 | __Ss], [__T | __Stack]);
yeccpars2(294, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [294 | __Ss], [__T | __Stack]);
yeccpars2(294, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [294 | __Ss], [__T | __Stack]);
yeccpars2(294, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [294 | __Ss], [__T | __Stack]);
yeccpars2(294, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [294 | __Ss], [__T | __Stack]);
yeccpars2(294, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [294 | __Ss], [__T | __Stack]);
yeccpars2(294, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [294 | __Ss], [__T | __Stack]);
yeccpars2(294, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [294 | __Ss], [__T | __Stack]);
yeccpars2(294, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [294 | __Ss], [__T | __Stack]);
yeccpars2(294, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [294 | __Ss], [__T | __Stack]);
yeccpars2(294, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [294 | __Ss], [__T | __Stack]);
yeccpars2(294, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [294 | __Ss], [__T | __Stack]);
yeccpars2(294, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [294 | __Ss], [__T | __Stack]);
yeccpars2(294, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [294 | __Ss], [__T | __Stack]);
yeccpars2(294, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [294 | __Ss], [__T | __Stack]);
yeccpars2(294, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [294 | __Ss], [__T | __Stack]);
yeccpars2(294, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 353, [294 | __Ss], [__T | __Stack]);
yeccpars2(294, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(295, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 299, [295 | __Ss], [__T | __Stack]);
yeccpars2(295, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 300, [295 | __Ss], [__T | __Stack]);
yeccpars2(295, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 301, [295 | __Ss], [__T | __Stack]);
yeccpars2(295, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 302, [295 | __Ss], [__T | __Stack]);
yeccpars2(295, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 303, [295 | __Ss], [__T | __Stack]);
yeccpars2(295, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 304, [295 | __Ss], [__T | __Stack]);
yeccpars2(295, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(296, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 350, [296 | __Ss], [__T | __Stack]);
yeccpars2(296, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(top_type, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(297, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_297_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(typed_attr_val, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(298, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 348, [298 | __Ss], [__T | __Stack]);
yeccpars2(298, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_298_(__Stack),
 yeccpars2(yeccgoto(top_types, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(299, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 345, [299 | __Ss], [__T | __Stack]);
yeccpars2(299, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(300, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 323, [300 | __Ss], [__T | __Stack]);
yeccpars2(300, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 324, [300 | __Ss], [__T | __Stack]);
yeccpars2(300, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(301, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 299, [301 | __Ss], [__T | __Stack]);
yeccpars2(301, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 300, [301 | __Ss], [__T | __Stack]);
yeccpars2(301, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 301, [301 | __Ss], [__T | __Stack]);
yeccpars2(301, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 316, [301 | __Ss], [__T | __Stack]);
yeccpars2(301, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 302, [301 | __Ss], [__T | __Stack]);
yeccpars2(301, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 303, [301 | __Ss], [__T | __Stack]);
yeccpars2(301, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 304, [301 | __Ss], [__T | __Stack]);
yeccpars2(301, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(302, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 308, [302 | __Ss], [__T | __Stack]);
yeccpars2(302, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_302_(__Stack),
 yeccpars2(yeccgoto(type, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(303, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_303_(__Stack),
 yeccpars2(yeccgoto(type, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(304, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 299, [304 | __Ss], [__T | __Stack]);
yeccpars2(304, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 300, [304 | __Ss], [__T | __Stack]);
yeccpars2(304, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 301, [304 | __Ss], [__T | __Stack]);
yeccpars2(304, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 302, [304 | __Ss], [__T | __Stack]);
yeccpars2(304, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 303, [304 | __Ss], [__T | __Stack]);
yeccpars2(304, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 304, [304 | __Ss], [__T | __Stack]);
yeccpars2(304, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 306, [304 | __Ss], [__T | __Stack]);
yeccpars2(304, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(305, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 307, [305 | __Ss], [__T | __Stack]);
yeccpars2(305, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(306, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_306_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(307, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_307_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(308, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 299, [308 | __Ss], [__T | __Stack]);
yeccpars2(308, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 300, [308 | __Ss], [__T | __Stack]);
yeccpars2(308, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 310, [308 | __Ss], [__T | __Stack]);
yeccpars2(308, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 301, [308 | __Ss], [__T | __Stack]);
yeccpars2(308, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 302, [308 | __Ss], [__T | __Stack]);
yeccpars2(308, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 303, [308 | __Ss], [__T | __Stack]);
yeccpars2(308, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 304, [308 | __Ss], [__T | __Stack]);
yeccpars2(308, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(309, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 311, [309 | __Ss], [__T | __Stack]);
yeccpars2(309, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 312, [309 | __Ss], [__T | __Stack]);
yeccpars2(309, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(310, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_310_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(311, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_311_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(312, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 299, [312 | __Ss], [__T | __Stack]);
yeccpars2(312, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 300, [312 | __Ss], [__T | __Stack]);
yeccpars2(312, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 301, [312 | __Ss], [__T | __Stack]);
yeccpars2(312, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 302, [312 | __Ss], [__T | __Stack]);
yeccpars2(312, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 303, [312 | __Ss], [__T | __Stack]);
yeccpars2(312, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 304, [312 | __Ss], [__T | __Stack]);
yeccpars2(312, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(313, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 314, [313 | __Ss], [__T | __Stack]);
yeccpars2(313, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(314, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_314_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(315, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 317, [315 | __Ss], [__T | __Stack]);
yeccpars2(315, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 318, [315 | __Ss], [__T | __Stack]);
yeccpars2(315, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(316, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_316_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(317, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 319, [317 | __Ss], [__T | __Stack]);
yeccpars2(317, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(318, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_318_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(319, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 320, [319 | __Ss], [__T | __Stack]);
yeccpars2(319, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(320, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 321, [320 | __Ss], [__T | __Stack]);
yeccpars2(320, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(321, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 322, [321 | __Ss], [__T | __Stack]);
yeccpars2(321, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(322, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_322_(__Stack),
 __Nss = lists:nthtail(6, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(323, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 299, [323 | __Ss], [__T | __Stack]);
yeccpars2(323, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 300, [323 | __Ss], [__T | __Stack]);
yeccpars2(323, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 332, [323 | __Ss], [__T | __Stack]);
yeccpars2(323, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 301, [323 | __Ss], [__T | __Stack]);
yeccpars2(323, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 302, [323 | __Ss], [__T | __Stack]);
yeccpars2(323, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 303, [323 | __Ss], [__T | __Stack]);
yeccpars2(323, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 333, [323 | __Ss], [__T | __Stack]);
yeccpars2(323, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 304, [323 | __Ss], [__T | __Stack]);
yeccpars2(323, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(324, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 325, [324 | __Ss], [__T | __Stack]);
yeccpars2(324, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(325, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 326, [325 | __Ss], [__T | __Stack]);
yeccpars2(325, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(326, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 327, [326 | __Ss], [__T | __Stack]);
yeccpars2(326, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(327, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 328, [327 | __Ss], [__T | __Stack]);
yeccpars2(327, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(328, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_328_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(329, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(arg_type, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(330, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 341, [330 | __Ss], [__T | __Stack]);
yeccpars2(330, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(331, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 339, [331 | __Ss], [__T | __Stack]);
yeccpars2(331, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_331_(__Stack),
 yeccpars2(yeccgoto(arg_types, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(332, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 336, [332 | __Ss], [__T | __Stack]);
yeccpars2(332, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(333, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 334, [333 | __Ss], [__T | __Stack]);
yeccpars2(333, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(334, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 299, [334 | __Ss], [__T | __Stack]);
yeccpars2(334, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 300, [334 | __Ss], [__T | __Stack]);
yeccpars2(334, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 301, [334 | __Ss], [__T | __Stack]);
yeccpars2(334, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 302, [334 | __Ss], [__T | __Stack]);
yeccpars2(334, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 303, [334 | __Ss], [__T | __Stack]);
yeccpars2(334, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 304, [334 | __Ss], [__T | __Stack]);
yeccpars2(334, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(335, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_335_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(arg_type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(336, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 299, [336 | __Ss], [__T | __Stack]);
yeccpars2(336, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 300, [336 | __Ss], [__T | __Stack]);
yeccpars2(336, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 301, [336 | __Ss], [__T | __Stack]);
yeccpars2(336, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 302, [336 | __Ss], [__T | __Stack]);
yeccpars2(336, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 303, [336 | __Ss], [__T | __Stack]);
yeccpars2(336, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 304, [336 | __Ss], [__T | __Stack]);
yeccpars2(336, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(337, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 338, [337 | __Ss], [__T | __Stack]);
yeccpars2(337, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(338, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_338_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(339, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 299, [339 | __Ss], [__T | __Stack]);
yeccpars2(339, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 300, [339 | __Ss], [__T | __Stack]);
yeccpars2(339, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 301, [339 | __Ss], [__T | __Stack]);
yeccpars2(339, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 302, [339 | __Ss], [__T | __Stack]);
yeccpars2(339, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 303, [339 | __Ss], [__T | __Stack]);
yeccpars2(339, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 333, [339 | __Ss], [__T | __Stack]);
yeccpars2(339, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 304, [339 | __Ss], [__T | __Stack]);
yeccpars2(339, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(340, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_340_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(arg_types, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(341, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 342, [341 | __Ss], [__T | __Stack]);
yeccpars2(341, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(342, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 299, [342 | __Ss], [__T | __Stack]);
yeccpars2(342, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 300, [342 | __Ss], [__T | __Stack]);
yeccpars2(342, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 301, [342 | __Ss], [__T | __Stack]);
yeccpars2(342, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 302, [342 | __Ss], [__T | __Stack]);
yeccpars2(342, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 303, [342 | __Ss], [__T | __Stack]);
yeccpars2(342, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 304, [342 | __Ss], [__T | __Stack]);
yeccpars2(342, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(343, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 344, [343 | __Ss], [__T | __Stack]);
yeccpars2(343, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(344, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_344_(__Stack),
 __Nss = lists:nthtail(6, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(345, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 346, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(346, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 347, [346 | __Ss], [__T | __Stack]);
yeccpars2(346, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(347, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_347_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(348, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 299, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 300, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 301, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 302, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 303, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 304, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(349, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_349_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(top_types, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(350, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 299, [350 | __Ss], [__T | __Stack]);
yeccpars2(350, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 300, [350 | __Ss], [__T | __Stack]);
yeccpars2(350, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 301, [350 | __Ss], [__T | __Stack]);
yeccpars2(350, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 302, [350 | __Ss], [__T | __Stack]);
yeccpars2(350, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 303, [350 | __Ss], [__T | __Stack]);
yeccpars2(350, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 304, [350 | __Ss], [__T | __Stack]);
yeccpars2(350, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(351, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_351_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(top_type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(352, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_352_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(typed_attr_val, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(353, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [353 | __Ss], [__T | __Stack]);
yeccpars2(353, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [353 | __Ss], [__T | __Stack]);
yeccpars2(353, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [353 | __Ss], [__T | __Stack]);
yeccpars2(353, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [353 | __Ss], [__T | __Stack]);
yeccpars2(353, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [353 | __Ss], [__T | __Stack]);
yeccpars2(353, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [353 | __Ss], [__T | __Stack]);
yeccpars2(353, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [353 | __Ss], [__T | __Stack]);
yeccpars2(353, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [353 | __Ss], [__T | __Stack]);
yeccpars2(353, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [353 | __Ss], [__T | __Stack]);
yeccpars2(353, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [353 | __Ss], [__T | __Stack]);
yeccpars2(353, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [353 | __Ss], [__T | __Stack]);
yeccpars2(353, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [353 | __Ss], [__T | __Stack]);
yeccpars2(353, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [353 | __Ss], [__T | __Stack]);
yeccpars2(353, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [353 | __Ss], [__T | __Stack]);
yeccpars2(353, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [353 | __Ss], [__T | __Stack]);
yeccpars2(353, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [353 | __Ss], [__T | __Stack]);
yeccpars2(353, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [353 | __Ss], [__T | __Stack]);
yeccpars2(353, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [353 | __Ss], [__T | __Stack]);
yeccpars2(353, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [353 | __Ss], [__T | __Stack]);
yeccpars2(353, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [353 | __Ss], [__T | __Stack]);
yeccpars2(353, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [353 | __Ss], [__T | __Stack]);
yeccpars2(353, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [353 | __Ss], [__T | __Stack]);
yeccpars2(353, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [353 | __Ss], [__T | __Stack]);
yeccpars2(353, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [353 | __Ss], [__T | __Stack]);
yeccpars2(353, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [353 | __Ss], [__T | __Stack]);
yeccpars2(353, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(354, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 364, [354 | __Ss], [__T | __Stack]);
yeccpars2(354, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(355, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 361, [355 | __Ss], [__T | __Stack]);
yeccpars2(355, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_355_(__Stack),
 yeccpars2(yeccgoto(typed_exprs, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(356, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 357, [356 | __Ss], [__T | __Stack]);
yeccpars2(356, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 358, [356 | __Ss], [__T | __Stack]);
yeccpars2(356, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_356_(__Stack),
 yeccpars2(yeccgoto(exprs, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(357, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [357 | __Ss], [__T | __Stack]);
yeccpars2(357, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [357 | __Ss], [__T | __Stack]);
yeccpars2(357, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [357 | __Ss], [__T | __Stack]);
yeccpars2(357, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [357 | __Ss], [__T | __Stack]);
yeccpars2(357, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [357 | __Ss], [__T | __Stack]);
yeccpars2(357, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [357 | __Ss], [__T | __Stack]);
yeccpars2(357, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [357 | __Ss], [__T | __Stack]);
yeccpars2(357, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [357 | __Ss], [__T | __Stack]);
yeccpars2(357, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [357 | __Ss], [__T | __Stack]);
yeccpars2(357, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [357 | __Ss], [__T | __Stack]);
yeccpars2(357, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [357 | __Ss], [__T | __Stack]);
yeccpars2(357, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [357 | __Ss], [__T | __Stack]);
yeccpars2(357, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [357 | __Ss], [__T | __Stack]);
yeccpars2(357, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [357 | __Ss], [__T | __Stack]);
yeccpars2(357, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [357 | __Ss], [__T | __Stack]);
yeccpars2(357, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [357 | __Ss], [__T | __Stack]);
yeccpars2(357, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [357 | __Ss], [__T | __Stack]);
yeccpars2(357, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [357 | __Ss], [__T | __Stack]);
yeccpars2(357, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [357 | __Ss], [__T | __Stack]);
yeccpars2(357, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [357 | __Ss], [__T | __Stack]);
yeccpars2(357, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [357 | __Ss], [__T | __Stack]);
yeccpars2(357, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [357 | __Ss], [__T | __Stack]);
yeccpars2(357, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [357 | __Ss], [__T | __Stack]);
yeccpars2(357, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [357 | __Ss], [__T | __Stack]);
yeccpars2(357, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(358, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 299, [358 | __Ss], [__T | __Stack]);
yeccpars2(358, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 300, [358 | __Ss], [__T | __Stack]);
yeccpars2(358, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 301, [358 | __Ss], [__T | __Stack]);
yeccpars2(358, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 302, [358 | __Ss], [__T | __Stack]);
yeccpars2(358, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 303, [358 | __Ss], [__T | __Stack]);
yeccpars2(358, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 304, [358 | __Ss], [__T | __Stack]);
yeccpars2(358, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(359, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_359_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(typed_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(360, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_360_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(typed_exprs, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(361, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [361 | __Ss], [__T | __Stack]);
yeccpars2(361, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [361 | __Ss], [__T | __Stack]);
yeccpars2(361, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [361 | __Ss], [__T | __Stack]);
yeccpars2(361, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [361 | __Ss], [__T | __Stack]);
yeccpars2(361, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [361 | __Ss], [__T | __Stack]);
yeccpars2(361, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [361 | __Ss], [__T | __Stack]);
yeccpars2(361, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [361 | __Ss], [__T | __Stack]);
yeccpars2(361, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [361 | __Ss], [__T | __Stack]);
yeccpars2(361, 'begin', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [361 | __Ss], [__T | __Stack]);
yeccpars2(361, 'bnot', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [361 | __Ss], [__T | __Stack]);
yeccpars2(361, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [361 | __Ss], [__T | __Stack]);
yeccpars2(361, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [361 | __Ss], [__T | __Stack]);
yeccpars2(361, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [361 | __Ss], [__T | __Stack]);
yeccpars2(361, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [361 | __Ss], [__T | __Stack]);
yeccpars2(361, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [361 | __Ss], [__T | __Stack]);
yeccpars2(361, 'if', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [361 | __Ss], [__T | __Stack]);
yeccpars2(361, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [361 | __Ss], [__T | __Stack]);
yeccpars2(361, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [361 | __Ss], [__T | __Stack]);
yeccpars2(361, 'query', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [361 | __Ss], [__T | __Stack]);
yeccpars2(361, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [361 | __Ss], [__T | __Stack]);
yeccpars2(361, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [361 | __Ss], [__T | __Stack]);
yeccpars2(361, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [361 | __Ss], [__T | __Stack]);
yeccpars2(361, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [361 | __Ss], [__T | __Stack]);
yeccpars2(361, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [361 | __Ss], [__T | __Stack]);
yeccpars2(361, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(362, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_362_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(typed_exprs, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(363, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_363_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(typed_exprs, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(364, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_364_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(typed_record_fields, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(365, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_365_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(attribute, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(366, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_366_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(form, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(367, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_367_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(form, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(368, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 370, [368 | __Ss], [__T | __Stack]);
yeccpars2(368, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(369, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_369_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(function_clauses, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(370, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [370 | __Ss], [__T | __Stack]);
yeccpars2(370, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(371, 'when', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 84, [371 | __Ss], [__T | __Stack]);
yeccpars2(371, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_371_(__Stack),
 yeccpars2(372, __Cat, [371 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(372, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 90, [372 | __Ss], [__T | __Stack]);
yeccpars2(372, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(373, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_373_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(form, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(374, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 376, [374 | __Ss], [__T | __Stack]);
yeccpars2(374, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(375, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_375_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(rule_clauses, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(376, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(377, 'when', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 84, [377 | __Ss], [__T | __Stack]);
yeccpars2(377, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_377_(__Stack),
 yeccpars2(378, __Cat, [377 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(378, ':-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 285, [378 | __Ss], [__T | __Stack]);
yeccpars2(378, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(__Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.1",{missing_state_in_action_table, __Other}}).

yeccgoto(add_op, 33) ->
 249;
yeccgoto(arg_type, 323) ->
 331;
yeccgoto(arg_type, 339) ->
 331;
yeccgoto(arg_types, 323) ->
 330;
yeccgoto(arg_types, 339) ->
 340;
yeccgoto(argument_list, 10) ->
 12;
yeccgoto(argument_list, 29) ->
 270;
yeccgoto(argument_list, 59) ->
 151;
yeccgoto(argument_list, 161) ->
 151;
yeccgoto(argument_list, 370) ->
 12;
yeccgoto(argument_list, 376) ->
 12;
yeccgoto(atomic, 13) ->
 43;
yeccgoto(atomic, 20) ->
 43;
yeccgoto(atomic, 45) ->
 43;
yeccgoto(atomic, 50) ->
 43;
yeccgoto(atomic, 51) ->
 43;
yeccgoto(atomic, 53) ->
 43;
yeccgoto(atomic, 55) ->
 43;
yeccgoto(atomic, 56) ->
 43;
yeccgoto(atomic, 60) ->
 43;
yeccgoto(atomic, 64) ->
 43;
yeccgoto(atomic, 66) ->
 43;
yeccgoto(atomic, 68) ->
 43;
yeccgoto(atomic, 74) ->
 43;
yeccgoto(atomic, 75) ->
 43;
yeccgoto(atomic, 76) ->
 43;
yeccgoto(atomic, 80) ->
 43;
yeccgoto(atomic, 84) ->
 43;
yeccgoto(atomic, 87) ->
 43;
yeccgoto(atomic, 90) ->
 43;
yeccgoto(atomic, 97) ->
 43;
yeccgoto(atomic, 101) ->
 43;
yeccgoto(atomic, 107) ->
 43;
yeccgoto(atomic, 109) ->
 43;
yeccgoto(atomic, 117) ->
 43;
yeccgoto(atomic, 121) ->
 43;
yeccgoto(atomic, 127) ->
 43;
yeccgoto(atomic, 129) ->
 43;
yeccgoto(atomic, 134) ->
 43;
yeccgoto(atomic, 136) ->
 43;
yeccgoto(atomic, 138) ->
 43;
yeccgoto(atomic, 146) ->
 43;
yeccgoto(atomic, 166) ->
 43;
yeccgoto(atomic, 174) ->
 43;
yeccgoto(atomic, 176) ->
 43;
yeccgoto(atomic, 181) ->
 43;
yeccgoto(atomic, 188) ->
 43;
yeccgoto(atomic, 191) ->
 43;
yeccgoto(atomic, 195) ->
 43;
yeccgoto(atomic, 220) ->
 43;
yeccgoto(atomic, 222) ->
 43;
yeccgoto(atomic, 228) ->
 43;
yeccgoto(atomic, 230) ->
 43;
yeccgoto(atomic, 231) ->
 43;
yeccgoto(atomic, 234) ->
 43;
yeccgoto(atomic, 236) ->
 43;
yeccgoto(atomic, 238) ->
 43;
yeccgoto(atomic, 248) ->
 43;
yeccgoto(atomic, 249) ->
 43;
yeccgoto(atomic, 261) ->
 43;
yeccgoto(atomic, 272) ->
 43;
yeccgoto(atomic, 285) ->
 43;
yeccgoto(atomic, 288) ->
 43;
yeccgoto(atomic, 294) ->
 43;
yeccgoto(atomic, 353) ->
 43;
yeccgoto(atomic, 357) ->
 43;
yeccgoto(atomic, 361) ->
 43;
yeccgoto(attr_val, 288) ->
 292;
yeccgoto(attribute, 0) ->
 8;
yeccgoto(bin_element, 50) ->
 186;
yeccgoto(bin_element, 188) ->
 186;
yeccgoto(bin_elements, 50) ->
 185;
yeccgoto(bin_elements, 188) ->
 189;
yeccgoto(binary, 13) ->
 42;
yeccgoto(binary, 20) ->
 42;
yeccgoto(binary, 45) ->
 42;
yeccgoto(binary, 50) ->
 184;
yeccgoto(binary, 51) ->
 42;
yeccgoto(binary, 53) ->
 42;
yeccgoto(binary, 55) ->
 42;
yeccgoto(binary, 56) ->
 42;
yeccgoto(binary, 60) ->
 42;
yeccgoto(binary, 64) ->
 42;
yeccgoto(binary, 66) ->
 42;
yeccgoto(binary, 68) ->
 42;
yeccgoto(binary, 74) ->
 42;
yeccgoto(binary, 75) ->
 42;
yeccgoto(binary, 76) ->
 42;
yeccgoto(binary, 80) ->
 42;
yeccgoto(binary, 84) ->
 42;
yeccgoto(binary, 87) ->
 42;
yeccgoto(binary, 90) ->
 42;
yeccgoto(binary, 97) ->
 42;
yeccgoto(binary, 101) ->
 42;
yeccgoto(binary, 107) ->
 42;
yeccgoto(binary, 109) ->
 42;
yeccgoto(binary, 117) ->
 42;
yeccgoto(binary, 121) ->
 42;
yeccgoto(binary, 127) ->
 42;
yeccgoto(binary, 129) ->
 133;
yeccgoto(binary, 134) ->
 42;
yeccgoto(binary, 136) ->
 42;
yeccgoto(binary, 138) ->
 133;
yeccgoto(binary, 146) ->
 42;
yeccgoto(binary, 166) ->
 42;
yeccgoto(binary, 174) ->
 42;
yeccgoto(binary, 176) ->
 42;
yeccgoto(binary, 181) ->
 42;
yeccgoto(binary, 188) ->
 42;
yeccgoto(binary, 191) ->
 133;
yeccgoto(binary, 195) ->
 42;
yeccgoto(binary, 220) ->
 42;
yeccgoto(binary, 222) ->
 42;
yeccgoto(binary, 228) ->
 42;
yeccgoto(binary, 230) ->
 42;
yeccgoto(binary, 231) ->
 42;
yeccgoto(binary, 234) ->
 42;
yeccgoto(binary, 236) ->
 42;
yeccgoto(binary, 238) ->
 42;
yeccgoto(binary, 248) ->
 42;
yeccgoto(binary, 249) ->
 42;
yeccgoto(binary, 261) ->
 42;
yeccgoto(binary, 272) ->
 42;
yeccgoto(binary, 285) ->
 133;
yeccgoto(binary, 288) ->
 42;
yeccgoto(binary, 294) ->
 42;
yeccgoto(binary, 353) ->
 42;
yeccgoto(binary, 357) ->
 42;
yeccgoto(binary, 361) ->
 42;
yeccgoto(binary_comprehension, 13) ->
 41;
yeccgoto(binary_comprehension, 20) ->
 41;
yeccgoto(binary_comprehension, 45) ->
 41;
yeccgoto(binary_comprehension, 50) ->
 41;
yeccgoto(binary_comprehension, 51) ->
 41;
yeccgoto(binary_comprehension, 53) ->
 41;
yeccgoto(binary_comprehension, 55) ->
 41;
yeccgoto(binary_comprehension, 56) ->
 41;
yeccgoto(binary_comprehension, 60) ->
 41;
yeccgoto(binary_comprehension, 64) ->
 41;
yeccgoto(binary_comprehension, 66) ->
 41;
yeccgoto(binary_comprehension, 68) ->
 41;
yeccgoto(binary_comprehension, 74) ->
 41;
yeccgoto(binary_comprehension, 75) ->
 41;
yeccgoto(binary_comprehension, 76) ->
 41;
yeccgoto(binary_comprehension, 80) ->
 41;
yeccgoto(binary_comprehension, 84) ->
 41;
yeccgoto(binary_comprehension, 87) ->
 41;
yeccgoto(binary_comprehension, 90) ->
 41;
yeccgoto(binary_comprehension, 97) ->
 41;
yeccgoto(binary_comprehension, 101) ->
 41;
yeccgoto(binary_comprehension, 107) ->
 41;
yeccgoto(binary_comprehension, 109) ->
 41;
yeccgoto(binary_comprehension, 117) ->
 41;
yeccgoto(binary_comprehension, 121) ->
 41;
yeccgoto(binary_comprehension, 127) ->
 41;
yeccgoto(binary_comprehension, 129) ->
 41;
yeccgoto(binary_comprehension, 134) ->
 41;
yeccgoto(binary_comprehension, 136) ->
 41;
yeccgoto(binary_comprehension, 138) ->
 41;
yeccgoto(binary_comprehension, 146) ->
 41;
yeccgoto(binary_comprehension, 166) ->
 41;
yeccgoto(binary_comprehension, 174) ->
 41;
yeccgoto(binary_comprehension, 176) ->
 41;
yeccgoto(binary_comprehension, 181) ->
 41;
yeccgoto(binary_comprehension, 188) ->
 41;
yeccgoto(binary_comprehension, 191) ->
 41;
yeccgoto(binary_comprehension, 195) ->
 41;
yeccgoto(binary_comprehension, 220) ->
 41;
yeccgoto(binary_comprehension, 222) ->
 41;
yeccgoto(binary_comprehension, 228) ->
 41;
yeccgoto(binary_comprehension, 230) ->
 41;
yeccgoto(binary_comprehension, 231) ->
 41;
yeccgoto(binary_comprehension, 234) ->
 41;
yeccgoto(binary_comprehension, 236) ->
 41;
yeccgoto(binary_comprehension, 238) ->
 41;
yeccgoto(binary_comprehension, 248) ->
 41;
yeccgoto(binary_comprehension, 249) ->
 41;
yeccgoto(binary_comprehension, 261) ->
 41;
yeccgoto(binary_comprehension, 272) ->
 41;
yeccgoto(binary_comprehension, 285) ->
 41;
yeccgoto(binary_comprehension, 288) ->
 41;
yeccgoto(binary_comprehension, 294) ->
 41;
yeccgoto(binary_comprehension, 353) ->
 41;
yeccgoto(binary_comprehension, 357) ->
 41;
yeccgoto(binary_comprehension, 361) ->
 41;
yeccgoto(bit_expr, 50) ->
 183;
yeccgoto(bit_expr, 188) ->
 183;
yeccgoto(bit_size_expr, 195) ->
 197;
yeccgoto(bit_type, 199) ->
 201;
yeccgoto(bit_type, 205) ->
 201;
yeccgoto(bit_type_list, 199) ->
 200;
yeccgoto(bit_type_list, 205) ->
 206;
yeccgoto(case_expr, 13) ->
 40;
yeccgoto(case_expr, 20) ->
 40;
yeccgoto(case_expr, 45) ->
 40;
yeccgoto(case_expr, 50) ->
 40;
yeccgoto(case_expr, 51) ->
 40;
yeccgoto(case_expr, 53) ->
 40;
yeccgoto(case_expr, 55) ->
 40;
yeccgoto(case_expr, 56) ->
 40;
yeccgoto(case_expr, 60) ->
 40;
yeccgoto(case_expr, 64) ->
 40;
yeccgoto(case_expr, 66) ->
 40;
yeccgoto(case_expr, 68) ->
 40;
yeccgoto(case_expr, 74) ->
 40;
yeccgoto(case_expr, 75) ->
 40;
yeccgoto(case_expr, 76) ->
 40;
yeccgoto(case_expr, 80) ->
 40;
yeccgoto(case_expr, 84) ->
 40;
yeccgoto(case_expr, 87) ->
 40;
yeccgoto(case_expr, 90) ->
 40;
yeccgoto(case_expr, 97) ->
 40;
yeccgoto(case_expr, 101) ->
 40;
yeccgoto(case_expr, 107) ->
 40;
yeccgoto(case_expr, 109) ->
 40;
yeccgoto(case_expr, 117) ->
 40;
yeccgoto(case_expr, 121) ->
 40;
yeccgoto(case_expr, 127) ->
 40;
yeccgoto(case_expr, 129) ->
 40;
yeccgoto(case_expr, 134) ->
 40;
yeccgoto(case_expr, 136) ->
 40;
yeccgoto(case_expr, 138) ->
 40;
yeccgoto(case_expr, 146) ->
 40;
yeccgoto(case_expr, 166) ->
 40;
yeccgoto(case_expr, 174) ->
 40;
yeccgoto(case_expr, 176) ->
 40;
yeccgoto(case_expr, 181) ->
 40;
yeccgoto(case_expr, 188) ->
 40;
yeccgoto(case_expr, 191) ->
 40;
yeccgoto(case_expr, 195) ->
 40;
yeccgoto(case_expr, 220) ->
 40;
yeccgoto(case_expr, 222) ->
 40;
yeccgoto(case_expr, 228) ->
 40;
yeccgoto(case_expr, 230) ->
 40;
yeccgoto(case_expr, 231) ->
 40;
yeccgoto(case_expr, 234) ->
 40;
yeccgoto(case_expr, 236) ->
 40;
yeccgoto(case_expr, 238) ->
 40;
yeccgoto(case_expr, 248) ->
 40;
yeccgoto(case_expr, 249) ->
 40;
yeccgoto(case_expr, 261) ->
 40;
yeccgoto(case_expr, 272) ->
 40;
yeccgoto(case_expr, 285) ->
 40;
yeccgoto(case_expr, 288) ->
 40;
yeccgoto(case_expr, 294) ->
 40;
yeccgoto(case_expr, 353) ->
 40;
yeccgoto(case_expr, 357) ->
 40;
yeccgoto(case_expr, 361) ->
 40;
yeccgoto(clause_args, 10) ->
 11;
yeccgoto(clause_args, 370) ->
 371;
yeccgoto(clause_args, 376) ->
 377;
yeccgoto(clause_body, 83) ->
 89;
yeccgoto(clause_body, 99) ->
 100;
yeccgoto(clause_body, 103) ->
 104;
yeccgoto(clause_body, 105) ->
 106;
yeccgoto(clause_body, 118) ->
 119;
yeccgoto(clause_body, 123) ->
 124;
yeccgoto(clause_body, 144) ->
 145;
yeccgoto(clause_body, 159) ->
 160;
yeccgoto(clause_body, 282) ->
 284;
yeccgoto(clause_body, 372) ->
 284;
yeccgoto(clause_guard, 11) ->
 282;
yeccgoto(clause_guard, 77) ->
 83;
yeccgoto(clause_guard, 94) ->
 105;
yeccgoto(clause_guard, 98) ->
 99;
yeccgoto(clause_guard, 102) ->
 103;
yeccgoto(clause_guard, 151) ->
 159;
yeccgoto(clause_guard, 371) ->
 372;
yeccgoto(clause_guard, 377) ->
 378;
yeccgoto(comp_op, 34) ->
 238;
yeccgoto(cr_clause, 64) ->
 79;
yeccgoto(cr_clause, 76) ->
 79;
yeccgoto(cr_clause, 80) ->
 79;
yeccgoto(cr_clause, 166) ->
 79;
yeccgoto(cr_clauses, 64) ->
 116;
yeccgoto(cr_clauses, 76) ->
 78;
yeccgoto(cr_clauses, 80) ->
 81;
yeccgoto(cr_clauses, 166) ->
 167;
yeccgoto(expr, 13) ->
 39;
yeccgoto(expr, 45) ->
 209;
yeccgoto(expr, 51) ->
 171;
yeccgoto(expr, 53) ->
 39;
yeccgoto(expr, 55) ->
 165;
yeccgoto(expr, 56) ->
 164;
yeccgoto(expr, 60) ->
 39;
yeccgoto(expr, 64) ->
 77;
yeccgoto(expr, 66) ->
 39;
yeccgoto(expr, 68) ->
 39;
yeccgoto(expr, 74) ->
 39;
yeccgoto(expr, 75) ->
 94;
yeccgoto(expr, 76) ->
 77;
yeccgoto(expr, 80) ->
 77;
yeccgoto(expr, 84) ->
 39;
yeccgoto(expr, 87) ->
 39;
yeccgoto(expr, 90) ->
 39;
yeccgoto(expr, 97) ->
 98;
yeccgoto(expr, 101) ->
 102;
yeccgoto(expr, 107) ->
 94;
yeccgoto(expr, 109) ->
 39;
yeccgoto(expr, 117) ->
 118;
yeccgoto(expr, 121) ->
 123;
yeccgoto(expr, 127) ->
 128;
yeccgoto(expr, 129) ->
 132;
yeccgoto(expr, 136) ->
 137;
yeccgoto(expr, 138) ->
 132;
yeccgoto(expr, 146) ->
 39;
yeccgoto(expr, 166) ->
 77;
yeccgoto(expr, 174) ->
 179;
yeccgoto(expr, 176) ->
 177;
yeccgoto(expr, 191) ->
 132;
yeccgoto(expr, 220) ->
 221;
yeccgoto(expr, 222) ->
 223;
yeccgoto(expr, 228) ->
 39;
yeccgoto(expr, 285) ->
 132;
yeccgoto(expr, 288) ->
 291;
yeccgoto(expr, 294) ->
 39;
yeccgoto(expr, 353) ->
 356;
yeccgoto(expr, 357) ->
 356;
yeccgoto(expr, 361) ->
 356;
yeccgoto(expr_100, 13) ->
 38;
yeccgoto(expr_100, 45) ->
 38;
yeccgoto(expr_100, 51) ->
 38;
yeccgoto(expr_100, 53) ->
 38;
yeccgoto(expr_100, 55) ->
 38;
yeccgoto(expr_100, 56) ->
 38;
yeccgoto(expr_100, 60) ->
 38;
yeccgoto(expr_100, 64) ->
 38;
yeccgoto(expr_100, 66) ->
 38;
yeccgoto(expr_100, 68) ->
 38;
yeccgoto(expr_100, 74) ->
 38;
yeccgoto(expr_100, 75) ->
 38;
yeccgoto(expr_100, 76) ->
 38;
yeccgoto(expr_100, 80) ->
 38;
yeccgoto(expr_100, 84) ->
 38;
yeccgoto(expr_100, 87) ->
 38;
yeccgoto(expr_100, 90) ->
 38;
yeccgoto(expr_100, 97) ->
 38;
yeccgoto(expr_100, 101) ->
 38;
yeccgoto(expr_100, 107) ->
 38;
yeccgoto(expr_100, 109) ->
 38;
yeccgoto(expr_100, 117) ->
 38;
yeccgoto(expr_100, 121) ->
 38;
yeccgoto(expr_100, 127) ->
 38;
yeccgoto(expr_100, 129) ->
 38;
yeccgoto(expr_100, 136) ->
 38;
yeccgoto(expr_100, 138) ->
 38;
yeccgoto(expr_100, 146) ->
 38;
yeccgoto(expr_100, 166) ->
 38;
yeccgoto(expr_100, 174) ->
 38;
yeccgoto(expr_100, 176) ->
 38;
yeccgoto(expr_100, 191) ->
 38;
yeccgoto(expr_100, 220) ->
 38;
yeccgoto(expr_100, 222) ->
 38;
yeccgoto(expr_100, 228) ->
 38;
yeccgoto(expr_100, 230) ->
 233;
yeccgoto(expr_100, 231) ->
 232;
yeccgoto(expr_100, 285) ->
 38;
yeccgoto(expr_100, 288) ->
 38;
yeccgoto(expr_100, 294) ->
 38;
yeccgoto(expr_100, 353) ->
 38;
yeccgoto(expr_100, 357) ->
 38;
yeccgoto(expr_100, 361) ->
 38;
yeccgoto(expr_150, 13) ->
 37;
yeccgoto(expr_150, 45) ->
 37;
yeccgoto(expr_150, 51) ->
 37;
yeccgoto(expr_150, 53) ->
 37;
yeccgoto(expr_150, 55) ->
 37;
yeccgoto(expr_150, 56) ->
 37;
yeccgoto(expr_150, 60) ->
 37;
yeccgoto(expr_150, 64) ->
 37;
yeccgoto(expr_150, 66) ->
 37;
yeccgoto(expr_150, 68) ->
 37;
yeccgoto(expr_150, 74) ->
 37;
yeccgoto(expr_150, 75) ->
 37;
yeccgoto(expr_150, 76) ->
 37;
yeccgoto(expr_150, 80) ->
 37;
yeccgoto(expr_150, 84) ->
 37;
yeccgoto(expr_150, 87) ->
 37;
yeccgoto(expr_150, 90) ->
 37;
yeccgoto(expr_150, 97) ->
 37;
yeccgoto(expr_150, 101) ->
 37;
yeccgoto(expr_150, 107) ->
 37;
yeccgoto(expr_150, 109) ->
 37;
yeccgoto(expr_150, 117) ->
 37;
yeccgoto(expr_150, 121) ->
 37;
yeccgoto(expr_150, 127) ->
 37;
yeccgoto(expr_150, 129) ->
 37;
yeccgoto(expr_150, 136) ->
 37;
yeccgoto(expr_150, 138) ->
 37;
yeccgoto(expr_150, 146) ->
 37;
yeccgoto(expr_150, 166) ->
 37;
yeccgoto(expr_150, 174) ->
 37;
yeccgoto(expr_150, 176) ->
 37;
yeccgoto(expr_150, 191) ->
 37;
yeccgoto(expr_150, 220) ->
 37;
yeccgoto(expr_150, 222) ->
 37;
yeccgoto(expr_150, 228) ->
 37;
yeccgoto(expr_150, 230) ->
 37;
yeccgoto(expr_150, 231) ->
 37;
yeccgoto(expr_150, 234) ->
 235;
yeccgoto(expr_150, 285) ->
 37;
yeccgoto(expr_150, 288) ->
 37;
yeccgoto(expr_150, 294) ->
 37;
yeccgoto(expr_150, 353) ->
 37;
yeccgoto(expr_150, 357) ->
 37;
yeccgoto(expr_150, 361) ->
 37;
yeccgoto(expr_160, 13) ->
 36;
yeccgoto(expr_160, 45) ->
 36;
yeccgoto(expr_160, 51) ->
 36;
yeccgoto(expr_160, 53) ->
 36;
yeccgoto(expr_160, 55) ->
 36;
yeccgoto(expr_160, 56) ->
 36;
yeccgoto(expr_160, 60) ->
 36;
yeccgoto(expr_160, 64) ->
 36;
yeccgoto(expr_160, 66) ->
 36;
yeccgoto(expr_160, 68) ->
 36;
yeccgoto(expr_160, 74) ->
 36;
yeccgoto(expr_160, 75) ->
 36;
yeccgoto(expr_160, 76) ->
 36;
yeccgoto(expr_160, 80) ->
 36;
yeccgoto(expr_160, 84) ->
 36;
yeccgoto(expr_160, 87) ->
 36;
yeccgoto(expr_160, 90) ->
 36;
yeccgoto(expr_160, 97) ->
 36;
yeccgoto(expr_160, 101) ->
 36;
yeccgoto(expr_160, 107) ->
 36;
yeccgoto(expr_160, 109) ->
 36;
yeccgoto(expr_160, 117) ->
 36;
yeccgoto(expr_160, 121) ->
 36;
yeccgoto(expr_160, 127) ->
 36;
yeccgoto(expr_160, 129) ->
 36;
yeccgoto(expr_160, 136) ->
 36;
yeccgoto(expr_160, 138) ->
 36;
yeccgoto(expr_160, 146) ->
 36;
yeccgoto(expr_160, 166) ->
 36;
yeccgoto(expr_160, 174) ->
 36;
yeccgoto(expr_160, 176) ->
 36;
yeccgoto(expr_160, 191) ->
 36;
yeccgoto(expr_160, 220) ->
 36;
yeccgoto(expr_160, 222) ->
 36;
yeccgoto(expr_160, 228) ->
 36;
yeccgoto(expr_160, 230) ->
 36;
yeccgoto(expr_160, 231) ->
 36;
yeccgoto(expr_160, 234) ->
 36;
yeccgoto(expr_160, 236) ->
 237;
yeccgoto(expr_160, 285) ->
 36;
yeccgoto(expr_160, 288) ->
 36;
yeccgoto(expr_160, 294) ->
 36;
yeccgoto(expr_160, 353) ->
 36;
yeccgoto(expr_160, 357) ->
 36;
yeccgoto(expr_160, 361) ->
 36;
yeccgoto(expr_200, 13) ->
 35;
yeccgoto(expr_200, 45) ->
 35;
yeccgoto(expr_200, 51) ->
 35;
yeccgoto(expr_200, 53) ->
 35;
yeccgoto(expr_200, 55) ->
 35;
yeccgoto(expr_200, 56) ->
 35;
yeccgoto(expr_200, 60) ->
 35;
yeccgoto(expr_200, 64) ->
 35;
yeccgoto(expr_200, 66) ->
 35;
yeccgoto(expr_200, 68) ->
 35;
yeccgoto(expr_200, 74) ->
 35;
yeccgoto(expr_200, 75) ->
 35;
yeccgoto(expr_200, 76) ->
 35;
yeccgoto(expr_200, 80) ->
 35;
yeccgoto(expr_200, 84) ->
 35;
yeccgoto(expr_200, 87) ->
 35;
yeccgoto(expr_200, 90) ->
 35;
yeccgoto(expr_200, 97) ->
 35;
yeccgoto(expr_200, 101) ->
 35;
yeccgoto(expr_200, 107) ->
 35;
yeccgoto(expr_200, 109) ->
 35;
yeccgoto(expr_200, 117) ->
 35;
yeccgoto(expr_200, 121) ->
 35;
yeccgoto(expr_200, 127) ->
 35;
yeccgoto(expr_200, 129) ->
 35;
yeccgoto(expr_200, 136) ->
 35;
yeccgoto(expr_200, 138) ->
 35;
yeccgoto(expr_200, 146) ->
 35;
yeccgoto(expr_200, 166) ->
 35;
yeccgoto(expr_200, 174) ->
 35;
yeccgoto(expr_200, 176) ->
 35;
yeccgoto(expr_200, 191) ->
 35;
yeccgoto(expr_200, 220) ->
 35;
yeccgoto(expr_200, 222) ->
 35;
yeccgoto(expr_200, 228) ->
 35;
yeccgoto(expr_200, 230) ->
 35;
yeccgoto(expr_200, 231) ->
 35;
yeccgoto(expr_200, 234) ->
 35;
yeccgoto(expr_200, 236) ->
 35;
yeccgoto(expr_200, 285) ->
 35;
yeccgoto(expr_200, 288) ->
 35;
yeccgoto(expr_200, 294) ->
 35;
yeccgoto(expr_200, 353) ->
 35;
yeccgoto(expr_200, 357) ->
 35;
yeccgoto(expr_200, 361) ->
 35;
yeccgoto(expr_300, 13) ->
 34;
yeccgoto(expr_300, 45) ->
 34;
yeccgoto(expr_300, 51) ->
 34;
yeccgoto(expr_300, 53) ->
 34;
yeccgoto(expr_300, 55) ->
 34;
yeccgoto(expr_300, 56) ->
 34;
yeccgoto(expr_300, 60) ->
 34;
yeccgoto(expr_300, 64) ->
 34;
yeccgoto(expr_300, 66) ->
 34;
yeccgoto(expr_300, 68) ->
 34;
yeccgoto(expr_300, 74) ->
 34;
yeccgoto(expr_300, 75) ->
 34;
yeccgoto(expr_300, 76) ->
 34;
yeccgoto(expr_300, 80) ->
 34;
yeccgoto(expr_300, 84) ->
 34;
yeccgoto(expr_300, 87) ->
 34;
yeccgoto(expr_300, 90) ->
 34;
yeccgoto(expr_300, 97) ->
 34;
yeccgoto(expr_300, 101) ->
 34;
yeccgoto(expr_300, 107) ->
 34;
yeccgoto(expr_300, 109) ->
 34;
yeccgoto(expr_300, 117) ->
 34;
yeccgoto(expr_300, 121) ->
 34;
yeccgoto(expr_300, 127) ->
 34;
yeccgoto(expr_300, 129) ->
 34;
yeccgoto(expr_300, 136) ->
 34;
yeccgoto(expr_300, 138) ->
 34;
yeccgoto(expr_300, 146) ->
 34;
yeccgoto(expr_300, 166) ->
 34;
yeccgoto(expr_300, 174) ->
 34;
yeccgoto(expr_300, 176) ->
 34;
yeccgoto(expr_300, 191) ->
 34;
yeccgoto(expr_300, 220) ->
 34;
yeccgoto(expr_300, 222) ->
 34;
yeccgoto(expr_300, 228) ->
 34;
yeccgoto(expr_300, 230) ->
 34;
yeccgoto(expr_300, 231) ->
 34;
yeccgoto(expr_300, 234) ->
 34;
yeccgoto(expr_300, 236) ->
 34;
yeccgoto(expr_300, 238) ->
 247;
yeccgoto(expr_300, 248) ->
 269;
yeccgoto(expr_300, 285) ->
 34;
yeccgoto(expr_300, 288) ->
 34;
yeccgoto(expr_300, 294) ->
 34;
yeccgoto(expr_300, 353) ->
 34;
yeccgoto(expr_300, 357) ->
 34;
yeccgoto(expr_300, 361) ->
 34;
yeccgoto(expr_400, 13) ->
 33;
yeccgoto(expr_400, 45) ->
 33;
yeccgoto(expr_400, 51) ->
 33;
yeccgoto(expr_400, 53) ->
 33;
yeccgoto(expr_400, 55) ->
 33;
yeccgoto(expr_400, 56) ->
 33;
yeccgoto(expr_400, 60) ->
 33;
yeccgoto(expr_400, 64) ->
 33;
yeccgoto(expr_400, 66) ->
 33;
yeccgoto(expr_400, 68) ->
 33;
yeccgoto(expr_400, 74) ->
 33;
yeccgoto(expr_400, 75) ->
 33;
yeccgoto(expr_400, 76) ->
 33;
yeccgoto(expr_400, 80) ->
 33;
yeccgoto(expr_400, 84) ->
 33;
yeccgoto(expr_400, 87) ->
 33;
yeccgoto(expr_400, 90) ->
 33;
yeccgoto(expr_400, 97) ->
 33;
yeccgoto(expr_400, 101) ->
 33;
yeccgoto(expr_400, 107) ->
 33;
yeccgoto(expr_400, 109) ->
 33;
yeccgoto(expr_400, 117) ->
 33;
yeccgoto(expr_400, 121) ->
 33;
yeccgoto(expr_400, 127) ->
 33;
yeccgoto(expr_400, 129) ->
 33;
yeccgoto(expr_400, 136) ->
 33;
yeccgoto(expr_400, 138) ->
 33;
yeccgoto(expr_400, 146) ->
 33;
yeccgoto(expr_400, 166) ->
 33;
yeccgoto(expr_400, 174) ->
 33;
yeccgoto(expr_400, 176) ->
 33;
yeccgoto(expr_400, 191) ->
 33;
yeccgoto(expr_400, 220) ->
 33;
yeccgoto(expr_400, 222) ->
 33;
yeccgoto(expr_400, 228) ->
 33;
yeccgoto(expr_400, 230) ->
 33;
yeccgoto(expr_400, 231) ->
 33;
yeccgoto(expr_400, 234) ->
 33;
yeccgoto(expr_400, 236) ->
 33;
yeccgoto(expr_400, 238) ->
 33;
yeccgoto(expr_400, 248) ->
 33;
yeccgoto(expr_400, 285) ->
 33;
yeccgoto(expr_400, 288) ->
 33;
yeccgoto(expr_400, 294) ->
 33;
yeccgoto(expr_400, 353) ->
 33;
yeccgoto(expr_400, 357) ->
 33;
yeccgoto(expr_400, 361) ->
 33;
yeccgoto(expr_500, 13) ->
 32;
yeccgoto(expr_500, 45) ->
 32;
yeccgoto(expr_500, 51) ->
 32;
yeccgoto(expr_500, 53) ->
 32;
yeccgoto(expr_500, 55) ->
 32;
yeccgoto(expr_500, 56) ->
 32;
yeccgoto(expr_500, 60) ->
 32;
yeccgoto(expr_500, 64) ->
 32;
yeccgoto(expr_500, 66) ->
 32;
yeccgoto(expr_500, 68) ->
 32;
yeccgoto(expr_500, 74) ->
 32;
yeccgoto(expr_500, 75) ->
 32;
yeccgoto(expr_500, 76) ->
 32;
yeccgoto(expr_500, 80) ->
 32;
yeccgoto(expr_500, 84) ->
 32;
yeccgoto(expr_500, 87) ->
 32;
yeccgoto(expr_500, 90) ->
 32;
yeccgoto(expr_500, 97) ->
 32;
yeccgoto(expr_500, 101) ->
 32;
yeccgoto(expr_500, 107) ->
 32;
yeccgoto(expr_500, 109) ->
 32;
yeccgoto(expr_500, 117) ->
 32;
yeccgoto(expr_500, 121) ->
 32;
yeccgoto(expr_500, 127) ->
 32;
yeccgoto(expr_500, 129) ->
 32;
yeccgoto(expr_500, 136) ->
 32;
yeccgoto(expr_500, 138) ->
 32;
yeccgoto(expr_500, 146) ->
 32;
yeccgoto(expr_500, 166) ->
 32;
yeccgoto(expr_500, 174) ->
 32;
yeccgoto(expr_500, 176) ->
 32;
yeccgoto(expr_500, 191) ->
 32;
yeccgoto(expr_500, 220) ->
 32;
yeccgoto(expr_500, 222) ->
 32;
yeccgoto(expr_500, 228) ->
 32;
yeccgoto(expr_500, 230) ->
 32;
yeccgoto(expr_500, 231) ->
 32;
yeccgoto(expr_500, 234) ->
 32;
yeccgoto(expr_500, 236) ->
 32;
yeccgoto(expr_500, 238) ->
 32;
yeccgoto(expr_500, 248) ->
 32;
yeccgoto(expr_500, 249) ->
 260;
yeccgoto(expr_500, 285) ->
 32;
yeccgoto(expr_500, 288) ->
 32;
yeccgoto(expr_500, 294) ->
 32;
yeccgoto(expr_500, 353) ->
 32;
yeccgoto(expr_500, 357) ->
 32;
yeccgoto(expr_500, 361) ->
 32;
yeccgoto(expr_600, 13) ->
 31;
yeccgoto(expr_600, 45) ->
 31;
yeccgoto(expr_600, 51) ->
 31;
yeccgoto(expr_600, 53) ->
 31;
yeccgoto(expr_600, 55) ->
 31;
yeccgoto(expr_600, 56) ->
 31;
yeccgoto(expr_600, 60) ->
 31;
yeccgoto(expr_600, 64) ->
 31;
yeccgoto(expr_600, 66) ->
 31;
yeccgoto(expr_600, 68) ->
 31;
yeccgoto(expr_600, 74) ->
 31;
yeccgoto(expr_600, 75) ->
 31;
yeccgoto(expr_600, 76) ->
 31;
yeccgoto(expr_600, 80) ->
 31;
yeccgoto(expr_600, 84) ->
 31;
yeccgoto(expr_600, 87) ->
 31;
yeccgoto(expr_600, 90) ->
 31;
yeccgoto(expr_600, 97) ->
 31;
yeccgoto(expr_600, 101) ->
 31;
yeccgoto(expr_600, 107) ->
 31;
yeccgoto(expr_600, 109) ->
 31;
yeccgoto(expr_600, 117) ->
 31;
yeccgoto(expr_600, 121) ->
 31;
yeccgoto(expr_600, 127) ->
 31;
yeccgoto(expr_600, 129) ->
 31;
yeccgoto(expr_600, 136) ->
 31;
yeccgoto(expr_600, 138) ->
 31;
yeccgoto(expr_600, 146) ->
 31;
yeccgoto(expr_600, 166) ->
 31;
yeccgoto(expr_600, 174) ->
 31;
yeccgoto(expr_600, 176) ->
 31;
yeccgoto(expr_600, 191) ->
 31;
yeccgoto(expr_600, 220) ->
 31;
yeccgoto(expr_600, 222) ->
 31;
yeccgoto(expr_600, 228) ->
 31;
yeccgoto(expr_600, 230) ->
 31;
yeccgoto(expr_600, 231) ->
 31;
yeccgoto(expr_600, 234) ->
 31;
yeccgoto(expr_600, 236) ->
 31;
yeccgoto(expr_600, 238) ->
 31;
yeccgoto(expr_600, 248) ->
 31;
yeccgoto(expr_600, 249) ->
 31;
yeccgoto(expr_600, 261) ->
 268;
yeccgoto(expr_600, 285) ->
 31;
yeccgoto(expr_600, 288) ->
 31;
yeccgoto(expr_600, 294) ->
 31;
yeccgoto(expr_600, 353) ->
 31;
yeccgoto(expr_600, 357) ->
 31;
yeccgoto(expr_600, 361) ->
 31;
yeccgoto(expr_700, 13) ->
 30;
yeccgoto(expr_700, 20) ->
 281;
yeccgoto(expr_700, 45) ->
 30;
yeccgoto(expr_700, 51) ->
 30;
yeccgoto(expr_700, 53) ->
 30;
yeccgoto(expr_700, 55) ->
 30;
yeccgoto(expr_700, 56) ->
 30;
yeccgoto(expr_700, 60) ->
 30;
yeccgoto(expr_700, 64) ->
 30;
yeccgoto(expr_700, 66) ->
 30;
yeccgoto(expr_700, 68) ->
 30;
yeccgoto(expr_700, 74) ->
 30;
yeccgoto(expr_700, 75) ->
 30;
yeccgoto(expr_700, 76) ->
 30;
yeccgoto(expr_700, 80) ->
 30;
yeccgoto(expr_700, 84) ->
 30;
yeccgoto(expr_700, 87) ->
 30;
yeccgoto(expr_700, 90) ->
 30;
yeccgoto(expr_700, 97) ->
 30;
yeccgoto(expr_700, 101) ->
 30;
yeccgoto(expr_700, 107) ->
 30;
yeccgoto(expr_700, 109) ->
 30;
yeccgoto(expr_700, 117) ->
 30;
yeccgoto(expr_700, 121) ->
 30;
yeccgoto(expr_700, 127) ->
 30;
yeccgoto(expr_700, 129) ->
 30;
yeccgoto(expr_700, 136) ->
 30;
yeccgoto(expr_700, 138) ->
 30;
yeccgoto(expr_700, 146) ->
 30;
yeccgoto(expr_700, 166) ->
 30;
yeccgoto(expr_700, 174) ->
 30;
yeccgoto(expr_700, 176) ->
 30;
yeccgoto(expr_700, 191) ->
 30;
yeccgoto(expr_700, 220) ->
 30;
yeccgoto(expr_700, 222) ->
 30;
yeccgoto(expr_700, 228) ->
 30;
yeccgoto(expr_700, 230) ->
 30;
yeccgoto(expr_700, 231) ->
 30;
yeccgoto(expr_700, 234) ->
 30;
yeccgoto(expr_700, 236) ->
 30;
yeccgoto(expr_700, 238) ->
 30;
yeccgoto(expr_700, 248) ->
 30;
yeccgoto(expr_700, 249) ->
 30;
yeccgoto(expr_700, 261) ->
 30;
yeccgoto(expr_700, 285) ->
 30;
yeccgoto(expr_700, 288) ->
 30;
yeccgoto(expr_700, 294) ->
 30;
yeccgoto(expr_700, 353) ->
 30;
yeccgoto(expr_700, 357) ->
 30;
yeccgoto(expr_700, 361) ->
 30;
yeccgoto(expr_800, 13) ->
 29;
yeccgoto(expr_800, 20) ->
 29;
yeccgoto(expr_800, 45) ->
 29;
yeccgoto(expr_800, 51) ->
 29;
yeccgoto(expr_800, 53) ->
 29;
yeccgoto(expr_800, 55) ->
 29;
yeccgoto(expr_800, 56) ->
 29;
yeccgoto(expr_800, 60) ->
 29;
yeccgoto(expr_800, 64) ->
 29;
yeccgoto(expr_800, 66) ->
 29;
yeccgoto(expr_800, 68) ->
 29;
yeccgoto(expr_800, 74) ->
 29;
yeccgoto(expr_800, 75) ->
 29;
yeccgoto(expr_800, 76) ->
 29;
yeccgoto(expr_800, 80) ->
 29;
yeccgoto(expr_800, 84) ->
 29;
yeccgoto(expr_800, 87) ->
 29;
yeccgoto(expr_800, 90) ->
 29;
yeccgoto(expr_800, 97) ->
 29;
yeccgoto(expr_800, 101) ->
 29;
yeccgoto(expr_800, 107) ->
 29;
yeccgoto(expr_800, 109) ->
 29;
yeccgoto(expr_800, 117) ->
 29;
yeccgoto(expr_800, 121) ->
 29;
yeccgoto(expr_800, 127) ->
 29;
yeccgoto(expr_800, 129) ->
 29;
yeccgoto(expr_800, 136) ->
 29;
yeccgoto(expr_800, 138) ->
 29;
yeccgoto(expr_800, 146) ->
 29;
yeccgoto(expr_800, 166) ->
 29;
yeccgoto(expr_800, 174) ->
 29;
yeccgoto(expr_800, 176) ->
 29;
yeccgoto(expr_800, 191) ->
 29;
yeccgoto(expr_800, 220) ->
 29;
yeccgoto(expr_800, 222) ->
 29;
yeccgoto(expr_800, 228) ->
 29;
yeccgoto(expr_800, 230) ->
 29;
yeccgoto(expr_800, 231) ->
 29;
yeccgoto(expr_800, 234) ->
 29;
yeccgoto(expr_800, 236) ->
 29;
yeccgoto(expr_800, 238) ->
 29;
yeccgoto(expr_800, 248) ->
 29;
yeccgoto(expr_800, 249) ->
 29;
yeccgoto(expr_800, 261) ->
 29;
yeccgoto(expr_800, 285) ->
 29;
yeccgoto(expr_800, 288) ->
 29;
yeccgoto(expr_800, 294) ->
 29;
yeccgoto(expr_800, 353) ->
 29;
yeccgoto(expr_800, 357) ->
 29;
yeccgoto(expr_800, 361) ->
 29;
yeccgoto(expr_900, 13) ->
 28;
yeccgoto(expr_900, 20) ->
 28;
yeccgoto(expr_900, 45) ->
 28;
yeccgoto(expr_900, 51) ->
 28;
yeccgoto(expr_900, 53) ->
 28;
yeccgoto(expr_900, 55) ->
 28;
yeccgoto(expr_900, 56) ->
 28;
yeccgoto(expr_900, 60) ->
 28;
yeccgoto(expr_900, 64) ->
 28;
yeccgoto(expr_900, 66) ->
 28;
yeccgoto(expr_900, 68) ->
 28;
yeccgoto(expr_900, 74) ->
 28;
yeccgoto(expr_900, 75) ->
 28;
yeccgoto(expr_900, 76) ->
 28;
yeccgoto(expr_900, 80) ->
 28;
yeccgoto(expr_900, 84) ->
 28;
yeccgoto(expr_900, 87) ->
 28;
yeccgoto(expr_900, 90) ->
 28;
yeccgoto(expr_900, 97) ->
 28;
yeccgoto(expr_900, 101) ->
 28;
yeccgoto(expr_900, 107) ->
 28;
yeccgoto(expr_900, 109) ->
 28;
yeccgoto(expr_900, 117) ->
 28;
yeccgoto(expr_900, 121) ->
 28;
yeccgoto(expr_900, 127) ->
 28;
yeccgoto(expr_900, 129) ->
 28;
yeccgoto(expr_900, 136) ->
 28;
yeccgoto(expr_900, 138) ->
 28;
yeccgoto(expr_900, 146) ->
 28;
yeccgoto(expr_900, 166) ->
 28;
yeccgoto(expr_900, 174) ->
 28;
yeccgoto(expr_900, 176) ->
 28;
yeccgoto(expr_900, 191) ->
 28;
yeccgoto(expr_900, 220) ->
 28;
yeccgoto(expr_900, 222) ->
 28;
yeccgoto(expr_900, 228) ->
 28;
yeccgoto(expr_900, 230) ->
 28;
yeccgoto(expr_900, 231) ->
 28;
yeccgoto(expr_900, 234) ->
 28;
yeccgoto(expr_900, 236) ->
 28;
yeccgoto(expr_900, 238) ->
 28;
yeccgoto(expr_900, 248) ->
 28;
yeccgoto(expr_900, 249) ->
 28;
yeccgoto(expr_900, 261) ->
 28;
yeccgoto(expr_900, 285) ->
 28;
yeccgoto(expr_900, 288) ->
 28;
yeccgoto(expr_900, 294) ->
 28;
yeccgoto(expr_900, 353) ->
 28;
yeccgoto(expr_900, 357) ->
 28;
yeccgoto(expr_900, 361) ->
 28;
yeccgoto(expr_max, 13) ->
 27;
yeccgoto(expr_max, 20) ->
 27;
yeccgoto(expr_max, 45) ->
 27;
yeccgoto(expr_max, 50) ->
 182;
yeccgoto(expr_max, 51) ->
 27;
yeccgoto(expr_max, 53) ->
 27;
yeccgoto(expr_max, 55) ->
 27;
yeccgoto(expr_max, 56) ->
 27;
yeccgoto(expr_max, 60) ->
 27;
yeccgoto(expr_max, 64) ->
 27;
yeccgoto(expr_max, 66) ->
 27;
yeccgoto(expr_max, 68) ->
 27;
yeccgoto(expr_max, 74) ->
 27;
yeccgoto(expr_max, 75) ->
 27;
yeccgoto(expr_max, 76) ->
 27;
yeccgoto(expr_max, 80) ->
 27;
yeccgoto(expr_max, 84) ->
 27;
yeccgoto(expr_max, 87) ->
 27;
yeccgoto(expr_max, 90) ->
 27;
yeccgoto(expr_max, 97) ->
 27;
yeccgoto(expr_max, 101) ->
 27;
yeccgoto(expr_max, 107) ->
 27;
yeccgoto(expr_max, 109) ->
 27;
yeccgoto(expr_max, 117) ->
 27;
yeccgoto(expr_max, 121) ->
 27;
yeccgoto(expr_max, 127) ->
 27;
yeccgoto(expr_max, 129) ->
 27;
yeccgoto(expr_max, 134) ->
 135;
yeccgoto(expr_max, 136) ->
 27;
yeccgoto(expr_max, 138) ->
 27;
yeccgoto(expr_max, 146) ->
 27;
yeccgoto(expr_max, 166) ->
 27;
yeccgoto(expr_max, 174) ->
 27;
yeccgoto(expr_max, 176) ->
 27;
yeccgoto(expr_max, 181) ->
 207;
yeccgoto(expr_max, 188) ->
 182;
yeccgoto(expr_max, 191) ->
 27;
yeccgoto(expr_max, 195) ->
 196;
yeccgoto(expr_max, 220) ->
 27;
yeccgoto(expr_max, 222) ->
 27;
yeccgoto(expr_max, 228) ->
 27;
yeccgoto(expr_max, 230) ->
 27;
yeccgoto(expr_max, 231) ->
 27;
yeccgoto(expr_max, 234) ->
 27;
yeccgoto(expr_max, 236) ->
 27;
yeccgoto(expr_max, 238) ->
 27;
yeccgoto(expr_max, 248) ->
 27;
yeccgoto(expr_max, 249) ->
 27;
yeccgoto(expr_max, 261) ->
 27;
yeccgoto(expr_max, 272) ->
 273;
yeccgoto(expr_max, 285) ->
 27;
yeccgoto(expr_max, 288) ->
 27;
yeccgoto(expr_max, 294) ->
 27;
yeccgoto(expr_max, 353) ->
 27;
yeccgoto(expr_max, 357) ->
 27;
yeccgoto(expr_max, 361) ->
 27;
yeccgoto(exprs, 13) ->
 26;
yeccgoto(exprs, 53) ->
 169;
yeccgoto(exprs, 60) ->
 86;
yeccgoto(exprs, 66) ->
 72;
yeccgoto(exprs, 68) ->
 69;
yeccgoto(exprs, 74) ->
 113;
yeccgoto(exprs, 84) ->
 86;
yeccgoto(exprs, 87) ->
 86;
yeccgoto(exprs, 90) ->
 91;
yeccgoto(exprs, 109) ->
 111;
yeccgoto(exprs, 146) ->
 86;
yeccgoto(exprs, 228) ->
 229;
yeccgoto(exprs, 288) ->
 290;
yeccgoto(exprs, 294) ->
 229;
yeccgoto(exprs, 353) ->
 69;
yeccgoto(exprs, 357) ->
 229;
yeccgoto(exprs, 361) ->
 363;
yeccgoto(form, 0) ->
 7;
yeccgoto(fun_clause, 59) ->
 150;
yeccgoto(fun_clause, 161) ->
 150;
yeccgoto(fun_clauses, 59) ->
 149;
yeccgoto(fun_clauses, 161) ->
 162;
yeccgoto(fun_expr, 13) ->
 25;
yeccgoto(fun_expr, 20) ->
 25;
yeccgoto(fun_expr, 45) ->
 25;
yeccgoto(fun_expr, 50) ->
 25;
yeccgoto(fun_expr, 51) ->
 25;
yeccgoto(fun_expr, 53) ->
 25;
yeccgoto(fun_expr, 55) ->
 25;
yeccgoto(fun_expr, 56) ->
 25;
yeccgoto(fun_expr, 60) ->
 25;
yeccgoto(fun_expr, 64) ->
 25;
yeccgoto(fun_expr, 66) ->
 25;
yeccgoto(fun_expr, 68) ->
 25;
yeccgoto(fun_expr, 74) ->
 25;
yeccgoto(fun_expr, 75) ->
 25;
yeccgoto(fun_expr, 76) ->
 25;
yeccgoto(fun_expr, 80) ->
 25;
yeccgoto(fun_expr, 84) ->
 25;
yeccgoto(fun_expr, 87) ->
 25;
yeccgoto(fun_expr, 90) ->
 25;
yeccgoto(fun_expr, 97) ->
 25;
yeccgoto(fun_expr, 101) ->
 25;
yeccgoto(fun_expr, 107) ->
 25;
yeccgoto(fun_expr, 109) ->
 25;
yeccgoto(fun_expr, 117) ->
 25;
yeccgoto(fun_expr, 121) ->
 25;
yeccgoto(fun_expr, 127) ->
 25;
yeccgoto(fun_expr, 129) ->
 25;
yeccgoto(fun_expr, 134) ->
 25;
yeccgoto(fun_expr, 136) ->
 25;
yeccgoto(fun_expr, 138) ->
 25;
yeccgoto(fun_expr, 146) ->
 25;
yeccgoto(fun_expr, 166) ->
 25;
yeccgoto(fun_expr, 174) ->
 25;
yeccgoto(fun_expr, 176) ->
 25;
yeccgoto(fun_expr, 181) ->
 25;
yeccgoto(fun_expr, 188) ->
 25;
yeccgoto(fun_expr, 191) ->
 25;
yeccgoto(fun_expr, 195) ->
 25;
yeccgoto(fun_expr, 220) ->
 25;
yeccgoto(fun_expr, 222) ->
 25;
yeccgoto(fun_expr, 228) ->
 25;
yeccgoto(fun_expr, 230) ->
 25;
yeccgoto(fun_expr, 231) ->
 25;
yeccgoto(fun_expr, 234) ->
 25;
yeccgoto(fun_expr, 236) ->
 25;
yeccgoto(fun_expr, 238) ->
 25;
yeccgoto(fun_expr, 248) ->
 25;
yeccgoto(fun_expr, 249) ->
 25;
yeccgoto(fun_expr, 261) ->
 25;
yeccgoto(fun_expr, 272) ->
 25;
yeccgoto(fun_expr, 285) ->
 25;
yeccgoto(fun_expr, 288) ->
 25;
yeccgoto(fun_expr, 294) ->
 25;
yeccgoto(fun_expr, 353) ->
 25;
yeccgoto(fun_expr, 357) ->
 25;
yeccgoto(fun_expr, 361) ->
 25;
yeccgoto(function, 0) ->
 6;
yeccgoto(function_call, 13) ->
 24;
yeccgoto(function_call, 20) ->
 24;
yeccgoto(function_call, 45) ->
 24;
yeccgoto(function_call, 51) ->
 24;
yeccgoto(function_call, 53) ->
 24;
yeccgoto(function_call, 55) ->
 24;
yeccgoto(function_call, 56) ->
 24;
yeccgoto(function_call, 60) ->
 24;
yeccgoto(function_call, 64) ->
 24;
yeccgoto(function_call, 66) ->
 24;
yeccgoto(function_call, 68) ->
 24;
yeccgoto(function_call, 74) ->
 24;
yeccgoto(function_call, 75) ->
 24;
yeccgoto(function_call, 76) ->
 24;
yeccgoto(function_call, 80) ->
 24;
yeccgoto(function_call, 84) ->
 24;
yeccgoto(function_call, 87) ->
 24;
yeccgoto(function_call, 90) ->
 24;
yeccgoto(function_call, 97) ->
 24;
yeccgoto(function_call, 101) ->
 24;
yeccgoto(function_call, 107) ->
 24;
yeccgoto(function_call, 109) ->
 24;
yeccgoto(function_call, 117) ->
 24;
yeccgoto(function_call, 121) ->
 24;
yeccgoto(function_call, 127) ->
 24;
yeccgoto(function_call, 129) ->
 24;
yeccgoto(function_call, 136) ->
 24;
yeccgoto(function_call, 138) ->
 24;
yeccgoto(function_call, 146) ->
 24;
yeccgoto(function_call, 166) ->
 24;
yeccgoto(function_call, 174) ->
 24;
yeccgoto(function_call, 176) ->
 24;
yeccgoto(function_call, 191) ->
 24;
yeccgoto(function_call, 220) ->
 24;
yeccgoto(function_call, 222) ->
 24;
yeccgoto(function_call, 228) ->
 24;
yeccgoto(function_call, 230) ->
 24;
yeccgoto(function_call, 231) ->
 24;
yeccgoto(function_call, 234) ->
 24;
yeccgoto(function_call, 236) ->
 24;
yeccgoto(function_call, 238) ->
 24;
yeccgoto(function_call, 248) ->
 24;
yeccgoto(function_call, 249) ->
 24;
yeccgoto(function_call, 261) ->
 24;
yeccgoto(function_call, 285) ->
 24;
yeccgoto(function_call, 288) ->
 24;
yeccgoto(function_call, 294) ->
 24;
yeccgoto(function_call, 353) ->
 24;
yeccgoto(function_call, 357) ->
 24;
yeccgoto(function_call, 361) ->
 24;
yeccgoto(function_clause, 0) ->
 5;
yeccgoto(function_clause, 368) ->
 5;
yeccgoto(function_clauses, 0) ->
 4;
yeccgoto(function_clauses, 368) ->
 369;
yeccgoto(guard, 60) ->
 144;
yeccgoto(guard, 84) ->
 85;
yeccgoto(guard, 87) ->
 88;
yeccgoto(guard, 146) ->
 144;
yeccgoto(if_clause, 60) ->
 143;
yeccgoto(if_clause, 146) ->
 143;
yeccgoto(if_clauses, 60) ->
 142;
yeccgoto(if_clauses, 146) ->
 147;
yeccgoto(if_expr, 13) ->
 23;
yeccgoto(if_expr, 20) ->
 23;
yeccgoto(if_expr, 45) ->
 23;
yeccgoto(if_expr, 50) ->
 23;
yeccgoto(if_expr, 51) ->
 23;
yeccgoto(if_expr, 53) ->
 23;
yeccgoto(if_expr, 55) ->
 23;
yeccgoto(if_expr, 56) ->
 23;
yeccgoto(if_expr, 60) ->
 23;
yeccgoto(if_expr, 64) ->
 23;
yeccgoto(if_expr, 66) ->
 23;
yeccgoto(if_expr, 68) ->
 23;
yeccgoto(if_expr, 74) ->
 23;
yeccgoto(if_expr, 75) ->
 23;
yeccgoto(if_expr, 76) ->
 23;
yeccgoto(if_expr, 80) ->
 23;
yeccgoto(if_expr, 84) ->
 23;
yeccgoto(if_expr, 87) ->
 23;
yeccgoto(if_expr, 90) ->
 23;
yeccgoto(if_expr, 97) ->
 23;
yeccgoto(if_expr, 101) ->
 23;
yeccgoto(if_expr, 107) ->
 23;
yeccgoto(if_expr, 109) ->
 23;
yeccgoto(if_expr, 117) ->
 23;
yeccgoto(if_expr, 121) ->
 23;
yeccgoto(if_expr, 127) ->
 23;
yeccgoto(if_expr, 129) ->
 23;
yeccgoto(if_expr, 134) ->
 23;
yeccgoto(if_expr, 136) ->
 23;
yeccgoto(if_expr, 138) ->
 23;
yeccgoto(if_expr, 146) ->
 23;
yeccgoto(if_expr, 166) ->
 23;
yeccgoto(if_expr, 174) ->
 23;
yeccgoto(if_expr, 176) ->
 23;
yeccgoto(if_expr, 181) ->
 23;
yeccgoto(if_expr, 188) ->
 23;
yeccgoto(if_expr, 191) ->
 23;
yeccgoto(if_expr, 195) ->
 23;
yeccgoto(if_expr, 220) ->
 23;
yeccgoto(if_expr, 222) ->
 23;
yeccgoto(if_expr, 228) ->
 23;
yeccgoto(if_expr, 230) ->
 23;
yeccgoto(if_expr, 231) ->
 23;
yeccgoto(if_expr, 234) ->
 23;
yeccgoto(if_expr, 236) ->
 23;
yeccgoto(if_expr, 238) ->
 23;
yeccgoto(if_expr, 248) ->
 23;
yeccgoto(if_expr, 249) ->
 23;
yeccgoto(if_expr, 261) ->
 23;
yeccgoto(if_expr, 272) ->
 23;
yeccgoto(if_expr, 285) ->
 23;
yeccgoto(if_expr, 288) ->
 23;
yeccgoto(if_expr, 294) ->
 23;
yeccgoto(if_expr, 353) ->
 23;
yeccgoto(if_expr, 357) ->
 23;
yeccgoto(if_expr, 361) ->
 23;
yeccgoto(lc_expr, 129) ->
 131;
yeccgoto(lc_expr, 138) ->
 131;
yeccgoto(lc_expr, 191) ->
 131;
yeccgoto(lc_expr, 285) ->
 131;
yeccgoto(lc_exprs, 129) ->
 130;
yeccgoto(lc_exprs, 138) ->
 139;
yeccgoto(lc_exprs, 191) ->
 192;
yeccgoto(lc_exprs, 285) ->
 286;
yeccgoto(list, 13) ->
 22;
yeccgoto(list, 20) ->
 22;
yeccgoto(list, 45) ->
 22;
yeccgoto(list, 50) ->
 22;
yeccgoto(list, 51) ->
 22;
yeccgoto(list, 53) ->
 22;
yeccgoto(list, 55) ->
 22;
yeccgoto(list, 56) ->
 22;
yeccgoto(list, 60) ->
 22;
yeccgoto(list, 64) ->
 22;
yeccgoto(list, 66) ->
 22;
yeccgoto(list, 68) ->
 22;
yeccgoto(list, 74) ->
 22;
yeccgoto(list, 75) ->
 22;
yeccgoto(list, 76) ->
 22;
yeccgoto(list, 80) ->
 22;
yeccgoto(list, 84) ->
 22;
yeccgoto(list, 87) ->
 22;
yeccgoto(list, 90) ->
 22;
yeccgoto(list, 97) ->
 22;
yeccgoto(list, 101) ->
 22;
yeccgoto(list, 107) ->
 22;
yeccgoto(list, 109) ->
 22;
yeccgoto(list, 117) ->
 22;
yeccgoto(list, 121) ->
 22;
yeccgoto(list, 127) ->
 22;
yeccgoto(list, 129) ->
 22;
yeccgoto(list, 134) ->
 22;
yeccgoto(list, 136) ->
 22;
yeccgoto(list, 138) ->
 22;
yeccgoto(list, 146) ->
 22;
yeccgoto(list, 166) ->
 22;
yeccgoto(list, 174) ->
 22;
yeccgoto(list, 176) ->
 22;
yeccgoto(list, 181) ->
 22;
yeccgoto(list, 188) ->
 22;
yeccgoto(list, 191) ->
 22;
yeccgoto(list, 195) ->
 22;
yeccgoto(list, 220) ->
 22;
yeccgoto(list, 222) ->
 22;
yeccgoto(list, 228) ->
 22;
yeccgoto(list, 230) ->
 22;
yeccgoto(list, 231) ->
 22;
yeccgoto(list, 234) ->
 22;
yeccgoto(list, 236) ->
 22;
yeccgoto(list, 238) ->
 22;
yeccgoto(list, 248) ->
 22;
yeccgoto(list, 249) ->
 22;
yeccgoto(list, 261) ->
 22;
yeccgoto(list, 272) ->
 22;
yeccgoto(list, 285) ->
 22;
yeccgoto(list, 288) ->
 22;
yeccgoto(list, 294) ->
 22;
yeccgoto(list, 353) ->
 22;
yeccgoto(list, 357) ->
 22;
yeccgoto(list, 361) ->
 22;
yeccgoto(list_comprehension, 13) ->
 21;
yeccgoto(list_comprehension, 20) ->
 21;
yeccgoto(list_comprehension, 45) ->
 21;
yeccgoto(list_comprehension, 50) ->
 21;
yeccgoto(list_comprehension, 51) ->
 21;
yeccgoto(list_comprehension, 53) ->
 21;
yeccgoto(list_comprehension, 55) ->
 21;
yeccgoto(list_comprehension, 56) ->
 21;
yeccgoto(list_comprehension, 60) ->
 21;
yeccgoto(list_comprehension, 63) ->
 126;
yeccgoto(list_comprehension, 64) ->
 21;
yeccgoto(list_comprehension, 66) ->
 21;
yeccgoto(list_comprehension, 68) ->
 21;
yeccgoto(list_comprehension, 74) ->
 21;
yeccgoto(list_comprehension, 75) ->
 21;
yeccgoto(list_comprehension, 76) ->
 21;
yeccgoto(list_comprehension, 80) ->
 21;
yeccgoto(list_comprehension, 84) ->
 21;
yeccgoto(list_comprehension, 87) ->
 21;
yeccgoto(list_comprehension, 90) ->
 21;
yeccgoto(list_comprehension, 97) ->
 21;
yeccgoto(list_comprehension, 101) ->
 21;
yeccgoto(list_comprehension, 107) ->
 21;
yeccgoto(list_comprehension, 109) ->
 21;
yeccgoto(list_comprehension, 117) ->
 21;
yeccgoto(list_comprehension, 121) ->
 21;
yeccgoto(list_comprehension, 127) ->
 21;
yeccgoto(list_comprehension, 129) ->
 21;
yeccgoto(list_comprehension, 134) ->
 21;
yeccgoto(list_comprehension, 136) ->
 21;
yeccgoto(list_comprehension, 138) ->
 21;
yeccgoto(list_comprehension, 146) ->
 21;
yeccgoto(list_comprehension, 166) ->
 21;
yeccgoto(list_comprehension, 174) ->
 21;
yeccgoto(list_comprehension, 176) ->
 21;
yeccgoto(list_comprehension, 181) ->
 21;
yeccgoto(list_comprehension, 188) ->
 21;
yeccgoto(list_comprehension, 191) ->
 21;
yeccgoto(list_comprehension, 195) ->
 21;
yeccgoto(list_comprehension, 220) ->
 21;
yeccgoto(list_comprehension, 222) ->
 21;
yeccgoto(list_comprehension, 228) ->
 21;
yeccgoto(list_comprehension, 230) ->
 21;
yeccgoto(list_comprehension, 231) ->
 21;
yeccgoto(list_comprehension, 234) ->
 21;
yeccgoto(list_comprehension, 236) ->
 21;
yeccgoto(list_comprehension, 238) ->
 21;
yeccgoto(list_comprehension, 248) ->
 21;
yeccgoto(list_comprehension, 249) ->
 21;
yeccgoto(list_comprehension, 261) ->
 21;
yeccgoto(list_comprehension, 272) ->
 21;
yeccgoto(list_comprehension, 285) ->
 21;
yeccgoto(list_comprehension, 288) ->
 21;
yeccgoto(list_comprehension, 294) ->
 21;
yeccgoto(list_comprehension, 353) ->
 21;
yeccgoto(list_comprehension, 357) ->
 21;
yeccgoto(list_comprehension, 361) ->
 21;
yeccgoto(list_op, 33) ->
 248;
yeccgoto(mult_op, 32) ->
 261;
yeccgoto(mult_op, 260) ->
 261;
yeccgoto(opt_bit_size_expr, 183) ->
 194;
yeccgoto(opt_bit_type_list, 194) ->
 198;
yeccgoto(prefix_op, 13) ->
 20;
yeccgoto(prefix_op, 45) ->
 20;
yeccgoto(prefix_op, 50) ->
 181;
yeccgoto(prefix_op, 51) ->
 20;
yeccgoto(prefix_op, 53) ->
 20;
yeccgoto(prefix_op, 55) ->
 20;
yeccgoto(prefix_op, 56) ->
 20;
yeccgoto(prefix_op, 60) ->
 20;
yeccgoto(prefix_op, 64) ->
 20;
yeccgoto(prefix_op, 66) ->
 20;
yeccgoto(prefix_op, 68) ->
 20;
yeccgoto(prefix_op, 74) ->
 20;
yeccgoto(prefix_op, 75) ->
 20;
yeccgoto(prefix_op, 76) ->
 20;
yeccgoto(prefix_op, 80) ->
 20;
yeccgoto(prefix_op, 84) ->
 20;
yeccgoto(prefix_op, 87) ->
 20;
yeccgoto(prefix_op, 90) ->
 20;
yeccgoto(prefix_op, 97) ->
 20;
yeccgoto(prefix_op, 101) ->
 20;
yeccgoto(prefix_op, 107) ->
 20;
yeccgoto(prefix_op, 109) ->
 20;
yeccgoto(prefix_op, 117) ->
 20;
yeccgoto(prefix_op, 121) ->
 20;
yeccgoto(prefix_op, 127) ->
 20;
yeccgoto(prefix_op, 129) ->
 20;
yeccgoto(prefix_op, 136) ->
 20;
yeccgoto(prefix_op, 138) ->
 20;
yeccgoto(prefix_op, 146) ->
 20;
yeccgoto(prefix_op, 166) ->
 20;
yeccgoto(prefix_op, 174) ->
 20;
yeccgoto(prefix_op, 176) ->
 20;
yeccgoto(prefix_op, 188) ->
 181;
yeccgoto(prefix_op, 191) ->
 20;
yeccgoto(prefix_op, 220) ->
 20;
yeccgoto(prefix_op, 222) ->
 20;
yeccgoto(prefix_op, 228) ->
 20;
yeccgoto(prefix_op, 230) ->
 20;
yeccgoto(prefix_op, 231) ->
 20;
yeccgoto(prefix_op, 234) ->
 20;
yeccgoto(prefix_op, 236) ->
 20;
yeccgoto(prefix_op, 238) ->
 20;
yeccgoto(prefix_op, 248) ->
 20;
yeccgoto(prefix_op, 249) ->
 20;
yeccgoto(prefix_op, 261) ->
 20;
yeccgoto(prefix_op, 285) ->
 20;
yeccgoto(prefix_op, 288) ->
 20;
yeccgoto(prefix_op, 294) ->
 20;
yeccgoto(prefix_op, 353) ->
 20;
yeccgoto(prefix_op, 357) ->
 20;
yeccgoto(prefix_op, 361) ->
 20;
yeccgoto(query_expr, 13) ->
 19;
yeccgoto(query_expr, 20) ->
 19;
yeccgoto(query_expr, 45) ->
 19;
yeccgoto(query_expr, 50) ->
 19;
yeccgoto(query_expr, 51) ->
 19;
yeccgoto(query_expr, 53) ->
 19;
yeccgoto(query_expr, 55) ->
 19;
yeccgoto(query_expr, 56) ->
 19;
yeccgoto(query_expr, 60) ->
 19;
yeccgoto(query_expr, 64) ->
 19;
yeccgoto(query_expr, 66) ->
 19;
yeccgoto(query_expr, 68) ->
 19;
yeccgoto(query_expr, 74) ->
 19;
yeccgoto(query_expr, 75) ->
 19;
yeccgoto(query_expr, 76) ->
 19;
yeccgoto(query_expr, 80) ->
 19;
yeccgoto(query_expr, 84) ->
 19;
yeccgoto(query_expr, 87) ->
 19;
yeccgoto(query_expr, 90) ->
 19;
yeccgoto(query_expr, 97) ->
 19;
yeccgoto(query_expr, 101) ->
 19;
yeccgoto(query_expr, 107) ->
 19;
yeccgoto(query_expr, 109) ->
 19;
yeccgoto(query_expr, 117) ->
 19;
yeccgoto(query_expr, 121) ->
 19;
yeccgoto(query_expr, 127) ->
 19;
yeccgoto(query_expr, 129) ->
 19;
yeccgoto(query_expr, 134) ->
 19;
yeccgoto(query_expr, 136) ->
 19;
yeccgoto(query_expr, 138) ->
 19;
yeccgoto(query_expr, 146) ->
 19;
yeccgoto(query_expr, 166) ->
 19;
yeccgoto(query_expr, 174) ->
 19;
yeccgoto(query_expr, 176) ->
 19;
yeccgoto(query_expr, 181) ->
 19;
yeccgoto(query_expr, 188) ->
 19;
yeccgoto(query_expr, 191) ->
 19;
yeccgoto(query_expr, 195) ->
 19;
yeccgoto(query_expr, 220) ->
 19;
yeccgoto(query_expr, 222) ->
 19;
yeccgoto(query_expr, 228) ->
 19;
yeccgoto(query_expr, 230) ->
 19;
yeccgoto(query_expr, 231) ->
 19;
yeccgoto(query_expr, 234) ->
 19;
yeccgoto(query_expr, 236) ->
 19;
yeccgoto(query_expr, 238) ->
 19;
yeccgoto(query_expr, 248) ->
 19;
yeccgoto(query_expr, 249) ->
 19;
yeccgoto(query_expr, 261) ->
 19;
yeccgoto(query_expr, 272) ->
 19;
yeccgoto(query_expr, 285) ->
 19;
yeccgoto(query_expr, 288) ->
 19;
yeccgoto(query_expr, 294) ->
 19;
yeccgoto(query_expr, 353) ->
 19;
yeccgoto(query_expr, 357) ->
 19;
yeccgoto(query_expr, 361) ->
 19;
yeccgoto(receive_expr, 13) ->
 18;
yeccgoto(receive_expr, 20) ->
 18;
yeccgoto(receive_expr, 45) ->
 18;
yeccgoto(receive_expr, 50) ->
 18;
yeccgoto(receive_expr, 51) ->
 18;
yeccgoto(receive_expr, 53) ->
 18;
yeccgoto(receive_expr, 55) ->
 18;
yeccgoto(receive_expr, 56) ->
 18;
yeccgoto(receive_expr, 60) ->
 18;
yeccgoto(receive_expr, 64) ->
 18;
yeccgoto(receive_expr, 66) ->
 18;
yeccgoto(receive_expr, 68) ->
 18;
yeccgoto(receive_expr, 74) ->
 18;
yeccgoto(receive_expr, 75) ->
 18;
yeccgoto(receive_expr, 76) ->
 18;
yeccgoto(receive_expr, 80) ->
 18;
yeccgoto(receive_expr, 84) ->
 18;
yeccgoto(receive_expr, 87) ->
 18;
yeccgoto(receive_expr, 90) ->
 18;
yeccgoto(receive_expr, 97) ->
 18;
yeccgoto(receive_expr, 101) ->
 18;
yeccgoto(receive_expr, 107) ->
 18;
yeccgoto(receive_expr, 109) ->
 18;
yeccgoto(receive_expr, 117) ->
 18;
yeccgoto(receive_expr, 121) ->
 18;
yeccgoto(receive_expr, 127) ->
 18;
yeccgoto(receive_expr, 129) ->
 18;
yeccgoto(receive_expr, 134) ->
 18;
yeccgoto(receive_expr, 136) ->
 18;
yeccgoto(receive_expr, 138) ->
 18;
yeccgoto(receive_expr, 146) ->
 18;
yeccgoto(receive_expr, 166) ->
 18;
yeccgoto(receive_expr, 174) ->
 18;
yeccgoto(receive_expr, 176) ->
 18;
yeccgoto(receive_expr, 181) ->
 18;
yeccgoto(receive_expr, 188) ->
 18;
yeccgoto(receive_expr, 191) ->
 18;
yeccgoto(receive_expr, 195) ->
 18;
yeccgoto(receive_expr, 220) ->
 18;
yeccgoto(receive_expr, 222) ->
 18;
yeccgoto(receive_expr, 228) ->
 18;
yeccgoto(receive_expr, 230) ->
 18;
yeccgoto(receive_expr, 231) ->
 18;
yeccgoto(receive_expr, 234) ->
 18;
yeccgoto(receive_expr, 236) ->
 18;
yeccgoto(receive_expr, 238) ->
 18;
yeccgoto(receive_expr, 248) ->
 18;
yeccgoto(receive_expr, 249) ->
 18;
yeccgoto(receive_expr, 261) ->
 18;
yeccgoto(receive_expr, 272) ->
 18;
yeccgoto(receive_expr, 285) ->
 18;
yeccgoto(receive_expr, 288) ->
 18;
yeccgoto(receive_expr, 294) ->
 18;
yeccgoto(receive_expr, 353) ->
 18;
yeccgoto(receive_expr, 357) ->
 18;
yeccgoto(receive_expr, 361) ->
 18;
yeccgoto(record_expr, 13) ->
 17;
yeccgoto(record_expr, 20) ->
 17;
yeccgoto(record_expr, 45) ->
 17;
yeccgoto(record_expr, 51) ->
 17;
yeccgoto(record_expr, 53) ->
 17;
yeccgoto(record_expr, 55) ->
 17;
yeccgoto(record_expr, 56) ->
 17;
yeccgoto(record_expr, 60) ->
 17;
yeccgoto(record_expr, 64) ->
 17;
yeccgoto(record_expr, 66) ->
 17;
yeccgoto(record_expr, 68) ->
 17;
yeccgoto(record_expr, 74) ->
 17;
yeccgoto(record_expr, 75) ->
 17;
yeccgoto(record_expr, 76) ->
 17;
yeccgoto(record_expr, 80) ->
 17;
yeccgoto(record_expr, 84) ->
 17;
yeccgoto(record_expr, 87) ->
 17;
yeccgoto(record_expr, 90) ->
 17;
yeccgoto(record_expr, 97) ->
 17;
yeccgoto(record_expr, 101) ->
 17;
yeccgoto(record_expr, 107) ->
 17;
yeccgoto(record_expr, 109) ->
 17;
yeccgoto(record_expr, 117) ->
 17;
yeccgoto(record_expr, 121) ->
 17;
yeccgoto(record_expr, 127) ->
 17;
yeccgoto(record_expr, 129) ->
 17;
yeccgoto(record_expr, 136) ->
 17;
yeccgoto(record_expr, 138) ->
 17;
yeccgoto(record_expr, 146) ->
 17;
yeccgoto(record_expr, 166) ->
 17;
yeccgoto(record_expr, 174) ->
 17;
yeccgoto(record_expr, 176) ->
 17;
yeccgoto(record_expr, 191) ->
 17;
yeccgoto(record_expr, 220) ->
 17;
yeccgoto(record_expr, 222) ->
 17;
yeccgoto(record_expr, 228) ->
 17;
yeccgoto(record_expr, 230) ->
 17;
yeccgoto(record_expr, 231) ->
 17;
yeccgoto(record_expr, 234) ->
 17;
yeccgoto(record_expr, 236) ->
 17;
yeccgoto(record_expr, 238) ->
 17;
yeccgoto(record_expr, 248) ->
 17;
yeccgoto(record_expr, 249) ->
 17;
yeccgoto(record_expr, 261) ->
 17;
yeccgoto(record_expr, 285) ->
 17;
yeccgoto(record_expr, 288) ->
 17;
yeccgoto(record_expr, 294) ->
 17;
yeccgoto(record_expr, 353) ->
 17;
yeccgoto(record_expr, 357) ->
 17;
yeccgoto(record_expr, 361) ->
 17;
yeccgoto(record_field, 214) ->
 216;
yeccgoto(record_field, 224) ->
 216;
yeccgoto(record_fields, 214) ->
 215;
yeccgoto(record_fields, 224) ->
 225;
yeccgoto(record_tuple, 211) ->
 212;
yeccgoto(record_tuple, 276) ->
 277;
yeccgoto(rule, 0) ->
 3;
yeccgoto(rule_body, 282) ->
 283;
yeccgoto(rule_body, 378) ->
 283;
yeccgoto(rule_clause, 0) ->
 2;
yeccgoto(rule_clause, 374) ->
 2;
yeccgoto(rule_clauses, 0) ->
 1;
yeccgoto(rule_clauses, 374) ->
 375;
yeccgoto(strings, 13) ->
 16;
yeccgoto(strings, 20) ->
 16;
yeccgoto(strings, 45) ->
 16;
yeccgoto(strings, 50) ->
 16;
yeccgoto(strings, 51) ->
 16;
yeccgoto(strings, 53) ->
 16;
yeccgoto(strings, 55) ->
 16;
yeccgoto(strings, 56) ->
 16;
yeccgoto(strings, 60) ->
 16;
yeccgoto(strings, 64) ->
 16;
yeccgoto(strings, 65) ->
 115;
yeccgoto(strings, 66) ->
 16;
yeccgoto(strings, 68) ->
 16;
yeccgoto(strings, 74) ->
 16;
yeccgoto(strings, 75) ->
 16;
yeccgoto(strings, 76) ->
 16;
yeccgoto(strings, 80) ->
 16;
yeccgoto(strings, 84) ->
 16;
yeccgoto(strings, 87) ->
 16;
yeccgoto(strings, 90) ->
 16;
yeccgoto(strings, 97) ->
 16;
yeccgoto(strings, 101) ->
 16;
yeccgoto(strings, 107) ->
 16;
yeccgoto(strings, 109) ->
 16;
yeccgoto(strings, 117) ->
 16;
yeccgoto(strings, 121) ->
 16;
yeccgoto(strings, 127) ->
 16;
yeccgoto(strings, 129) ->
 16;
yeccgoto(strings, 134) ->
 16;
yeccgoto(strings, 136) ->
 16;
yeccgoto(strings, 138) ->
 16;
yeccgoto(strings, 146) ->
 16;
yeccgoto(strings, 166) ->
 16;
yeccgoto(strings, 174) ->
 16;
yeccgoto(strings, 176) ->
 16;
yeccgoto(strings, 181) ->
 16;
yeccgoto(strings, 188) ->
 16;
yeccgoto(strings, 191) ->
 16;
yeccgoto(strings, 195) ->
 16;
yeccgoto(strings, 220) ->
 16;
yeccgoto(strings, 222) ->
 16;
yeccgoto(strings, 228) ->
 16;
yeccgoto(strings, 230) ->
 16;
yeccgoto(strings, 231) ->
 16;
yeccgoto(strings, 234) ->
 16;
yeccgoto(strings, 236) ->
 16;
yeccgoto(strings, 238) ->
 16;
yeccgoto(strings, 248) ->
 16;
yeccgoto(strings, 249) ->
 16;
yeccgoto(strings, 261) ->
 16;
yeccgoto(strings, 272) ->
 16;
yeccgoto(strings, 285) ->
 16;
yeccgoto(strings, 288) ->
 16;
yeccgoto(strings, 294) ->
 16;
yeccgoto(strings, 353) ->
 16;
yeccgoto(strings, 357) ->
 16;
yeccgoto(strings, 361) ->
 16;
yeccgoto(tail, 171) ->
 173;
yeccgoto(tail, 179) ->
 180;
yeccgoto(top_type, 295) ->
 298;
yeccgoto(top_type, 301) ->
 315;
yeccgoto(top_type, 304) ->
 298;
yeccgoto(top_type, 308) ->
 309;
yeccgoto(top_type, 312) ->
 313;
yeccgoto(top_type, 323) ->
 329;
yeccgoto(top_type, 334) ->
 335;
yeccgoto(top_type, 336) ->
 337;
yeccgoto(top_type, 339) ->
 329;
yeccgoto(top_type, 342) ->
 343;
yeccgoto(top_type, 348) ->
 298;
yeccgoto(top_type, 350) ->
 351;
yeccgoto(top_type, 358) ->
 359;
yeccgoto(top_types, 295) ->
 297;
yeccgoto(top_types, 304) ->
 305;
yeccgoto(top_types, 348) ->
 349;
yeccgoto(try_catch, 72) ->
 73;
yeccgoto(try_catch, 78) ->
 82;
yeccgoto(try_clause, 75) ->
 93;
yeccgoto(try_clause, 107) ->
 93;
yeccgoto(try_clauses, 75) ->
 92;
yeccgoto(try_clauses, 107) ->
 108;
yeccgoto(try_expr, 13) ->
 15;
yeccgoto(try_expr, 20) ->
 15;
yeccgoto(try_expr, 45) ->
 15;
yeccgoto(try_expr, 50) ->
 15;
yeccgoto(try_expr, 51) ->
 15;
yeccgoto(try_expr, 53) ->
 15;
yeccgoto(try_expr, 55) ->
 15;
yeccgoto(try_expr, 56) ->
 15;
yeccgoto(try_expr, 60) ->
 15;
yeccgoto(try_expr, 64) ->
 15;
yeccgoto(try_expr, 66) ->
 15;
yeccgoto(try_expr, 68) ->
 15;
yeccgoto(try_expr, 74) ->
 15;
yeccgoto(try_expr, 75) ->
 15;
yeccgoto(try_expr, 76) ->
 15;
yeccgoto(try_expr, 80) ->
 15;
yeccgoto(try_expr, 84) ->
 15;
yeccgoto(try_expr, 87) ->
 15;
yeccgoto(try_expr, 90) ->
 15;
yeccgoto(try_expr, 97) ->
 15;
yeccgoto(try_expr, 101) ->
 15;
yeccgoto(try_expr, 107) ->
 15;
yeccgoto(try_expr, 109) ->
 15;
yeccgoto(try_expr, 117) ->
 15;
yeccgoto(try_expr, 121) ->
 15;
yeccgoto(try_expr, 127) ->
 15;
yeccgoto(try_expr, 129) ->
 15;
yeccgoto(try_expr, 134) ->
 15;
yeccgoto(try_expr, 136) ->
 15;
yeccgoto(try_expr, 138) ->
 15;
yeccgoto(try_expr, 146) ->
 15;
yeccgoto(try_expr, 166) ->
 15;
yeccgoto(try_expr, 174) ->
 15;
yeccgoto(try_expr, 176) ->
 15;
yeccgoto(try_expr, 181) ->
 15;
yeccgoto(try_expr, 188) ->
 15;
yeccgoto(try_expr, 191) ->
 15;
yeccgoto(try_expr, 195) ->
 15;
yeccgoto(try_expr, 220) ->
 15;
yeccgoto(try_expr, 222) ->
 15;
yeccgoto(try_expr, 228) ->
 15;
yeccgoto(try_expr, 230) ->
 15;
yeccgoto(try_expr, 231) ->
 15;
yeccgoto(try_expr, 234) ->
 15;
yeccgoto(try_expr, 236) ->
 15;
yeccgoto(try_expr, 238) ->
 15;
yeccgoto(try_expr, 248) ->
 15;
yeccgoto(try_expr, 249) ->
 15;
yeccgoto(try_expr, 261) ->
 15;
yeccgoto(try_expr, 272) ->
 15;
yeccgoto(try_expr, 285) ->
 15;
yeccgoto(try_expr, 288) ->
 15;
yeccgoto(try_expr, 294) ->
 15;
yeccgoto(try_expr, 353) ->
 15;
yeccgoto(try_expr, 357) ->
 15;
yeccgoto(try_expr, 361) ->
 15;
yeccgoto(tuple, 13) ->
 14;
yeccgoto(tuple, 20) ->
 14;
yeccgoto(tuple, 45) ->
 14;
yeccgoto(tuple, 50) ->
 14;
yeccgoto(tuple, 51) ->
 14;
yeccgoto(tuple, 53) ->
 14;
yeccgoto(tuple, 55) ->
 14;
yeccgoto(tuple, 56) ->
 14;
yeccgoto(tuple, 60) ->
 14;
yeccgoto(tuple, 64) ->
 14;
yeccgoto(tuple, 66) ->
 14;
yeccgoto(tuple, 68) ->
 14;
yeccgoto(tuple, 74) ->
 14;
yeccgoto(tuple, 75) ->
 14;
yeccgoto(tuple, 76) ->
 14;
yeccgoto(tuple, 80) ->
 14;
yeccgoto(tuple, 84) ->
 14;
yeccgoto(tuple, 87) ->
 14;
yeccgoto(tuple, 90) ->
 14;
yeccgoto(tuple, 97) ->
 14;
yeccgoto(tuple, 101) ->
 14;
yeccgoto(tuple, 107) ->
 14;
yeccgoto(tuple, 109) ->
 14;
yeccgoto(tuple, 117) ->
 14;
yeccgoto(tuple, 121) ->
 14;
yeccgoto(tuple, 127) ->
 14;
yeccgoto(tuple, 129) ->
 14;
yeccgoto(tuple, 134) ->
 14;
yeccgoto(tuple, 136) ->
 14;
yeccgoto(tuple, 138) ->
 14;
yeccgoto(tuple, 146) ->
 14;
yeccgoto(tuple, 166) ->
 14;
yeccgoto(tuple, 174) ->
 14;
yeccgoto(tuple, 176) ->
 14;
yeccgoto(tuple, 181) ->
 14;
yeccgoto(tuple, 188) ->
 14;
yeccgoto(tuple, 191) ->
 14;
yeccgoto(tuple, 195) ->
 14;
yeccgoto(tuple, 220) ->
 14;
yeccgoto(tuple, 222) ->
 14;
yeccgoto(tuple, 228) ->
 14;
yeccgoto(tuple, 230) ->
 14;
yeccgoto(tuple, 231) ->
 14;
yeccgoto(tuple, 234) ->
 14;
yeccgoto(tuple, 236) ->
 14;
yeccgoto(tuple, 238) ->
 14;
yeccgoto(tuple, 248) ->
 14;
yeccgoto(tuple, 249) ->
 14;
yeccgoto(tuple, 261) ->
 14;
yeccgoto(tuple, 272) ->
 14;
yeccgoto(tuple, 285) ->
 14;
yeccgoto(tuple, 288) ->
 14;
yeccgoto(tuple, 294) ->
 14;
yeccgoto(tuple, 353) ->
 14;
yeccgoto(tuple, 357) ->
 14;
yeccgoto(tuple, 361) ->
 14;
yeccgoto(type, 295) ->
 296;
yeccgoto(type, 301) ->
 296;
yeccgoto(type, 304) ->
 296;
yeccgoto(type, 308) ->
 296;
yeccgoto(type, 312) ->
 296;
yeccgoto(type, 323) ->
 296;
yeccgoto(type, 334) ->
 296;
yeccgoto(type, 336) ->
 296;
yeccgoto(type, 339) ->
 296;
yeccgoto(type, 342) ->
 296;
yeccgoto(type, 348) ->
 296;
yeccgoto(type, 350) ->
 296;
yeccgoto(type, 358) ->
 296;
yeccgoto(typed_attr_val, 288) ->
 289;
yeccgoto(typed_expr, 353) ->
 355;
yeccgoto(typed_expr, 357) ->
 355;
yeccgoto(typed_expr, 361) ->
 355;
yeccgoto(typed_exprs, 353) ->
 354;
yeccgoto(typed_exprs, 357) ->
 360;
yeccgoto(typed_exprs, 361) ->
 362;
yeccgoto(typed_record_fields, 294) ->
 352;
yeccgoto(__Symbol, __State) ->
 erlang:error({yecc_bug,"1.1",{__Symbol, __State, missing_in_goto_table}}).

-compile({inline,{yeccpars2_1_,1}}).
-file("erl_parse.yrl", 411).
yeccpars2_1_([__1 | __Stack]) ->
 [begin
   build_rule ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_2_,1}}).
-file("erl_parse.yrl", 413).
yeccpars2_2_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_4_,1}}).
-file("erl_parse.yrl", 116).
yeccpars2_4_([__1 | __Stack]) ->
 [begin
   build_function ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_5_,1}}).
-file("erl_parse.yrl", 118).
yeccpars2_5_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_11_,1}}).
-file("erl_parse.yrl", 128).
yeccpars2_11_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_12_,1}}).
-file("erl_parse.yrl", 125).
yeccpars2_12_([__1 | __Stack]) ->
 [begin
   element ( 1 , __1 )
  end | __Stack].

-compile({inline,{yeccpars2_39_,1}}).
-file("erl_parse.yrl", 362).
yeccpars2_39_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_46_,1}}).
-file("erl_parse.yrl", 358).
yeccpars2_46_([__2,__1 | __Stack]) ->
 [begin
   { [ ] , line ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_70_,1}}).
-file("erl_parse.yrl", 245).
yeccpars2_70_([__2,__1 | __Stack]) ->
 [begin
   { tuple , line ( __1 ) , [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_71_,1}}).
-file("erl_parse.yrl", 246).
yeccpars2_71_([__3,__2,__1 | __Stack]) ->
 [begin
   { tuple , line ( __1 ) , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_73_,1}}).
-file("erl_parse.yrl", 324).
yeccpars2_73_([__3,__2,__1 | __Stack]) ->
 [begin
   build_try ( line ( __1 ) , __2 , [ ] , __3 )
  end | __Stack].

-compile({inline,{yeccpars2_77_,1}}).
-file("erl_parse.yrl", 128).
yeccpars2_77_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_79_,1}}).
-file("erl_parse.yrl", 293).
yeccpars2_79_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_81_,1}}).
-file("erl_parse.yrl", 294).
yeccpars2_81_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_82_,1}}).
-file("erl_parse.yrl", 322).
yeccpars2_82_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   build_try ( line ( __1 ) , __2 , __4 , __5 )
  end | __Stack].

-compile({inline,{yeccpars2_85_,1}}).
-file("erl_parse.yrl", 127).
yeccpars2_85_([__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_86_,1}}).
-file("erl_parse.yrl", 365).
yeccpars2_86_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_88_,1}}).
-file("erl_parse.yrl", 366).
yeccpars2_88_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_89_,1}}).
-file("erl_parse.yrl", 297).
yeccpars2_89_([__3,__2,__1 | __Stack]) ->
 [begin
   { clause , line ( __1 ) , [ __1 ] , __2 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_91_,1}}).
-file("erl_parse.yrl", 130).
yeccpars2_91_([__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_93_,1}}).
-file("erl_parse.yrl", 333).
yeccpars2_93_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_94_,1}}).
-file("erl_parse.yrl", 128).
yeccpars2_94_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_98_,1}}).
-file("erl_parse.yrl", 128).
yeccpars2_98_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_100_,1}}).
-file("erl_parse.yrl", 343).
yeccpars2_100_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   L = line ( __1 ) ,
    { clause , L , [ { tuple , L , [ __1 , __3 , { var , L , '_' } ] } ] , __4 , __5 }
  end | __Stack].

-compile({inline,{yeccpars2_102_,1}}).
-file("erl_parse.yrl", 128).
yeccpars2_102_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_104_,1}}).
-file("erl_parse.yrl", 340).
yeccpars2_104_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   L = line ( __1 ) ,
    { clause , L , [ { tuple , L , [ __1 , __3 , { var , L , '_' } ] } ] , __4 , __5 }
  end | __Stack].

-compile({inline,{yeccpars2_106_,1}}).
-file("erl_parse.yrl", 337).
yeccpars2_106_([__3,__2,__1 | __Stack]) ->
 [begin
   L = line ( __1 ) ,
    { clause , L , [ { tuple , L , [ { atom , L , throw } , __1 , { var , L , '_' } ] } ] , __2 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_108_,1}}).
-file("erl_parse.yrl", 334).
yeccpars2_108_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_110_,1}}).
-file("erl_parse.yrl", 327).
yeccpars2_110_([__3,__2,__1 | __Stack]) ->
 [begin
   { __2 , [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_112_,1}}).
-file("erl_parse.yrl", 329).
yeccpars2_112_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { __2 , __4 }
  end | __Stack].

-compile({inline,{yeccpars2_114_,1}}).
-file("erl_parse.yrl", 331).
yeccpars2_114_([__3,__2,__1 | __Stack]) ->
 [begin
   { [ ] , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_115_,1}}).
-file("erl_parse.yrl", 376).
yeccpars2_115_([__2,__1 | __Stack]) ->
 [begin
   { string , line ( __1 ) , element ( 3 , __1 ) ++ element ( 3 , __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_120_,1}}).
-file("erl_parse.yrl", 302).
yeccpars2_120_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { 'receive' , line ( __1 ) , [ ] , __3 , __4 }
  end | __Stack].

-compile({inline,{yeccpars2_122_,1}}).
-file("erl_parse.yrl", 300).
yeccpars2_122_([__3,__2,__1 | __Stack]) ->
 [begin
   { 'receive' , line ( __1 ) , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_125_,1}}).
-file("erl_parse.yrl", 304).
yeccpars2_125_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { 'receive' , line ( __1 ) , __2 , __4 , __5 }
  end | __Stack].

-compile({inline,{yeccpars2_131_,1}}).
-file("erl_parse.yrl", 238).
yeccpars2_131_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_135_,1}}).
-file("erl_parse.yrl", 243).
yeccpars2_135_([__3,__2,__1 | __Stack]) ->
 [begin
   { b_generate , line ( __2 ) , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_137_,1}}).
-file("erl_parse.yrl", 242).
yeccpars2_137_([__3,__2,__1 | __Stack]) ->
 [begin
   { generate , line ( __2 ) , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_139_,1}}).
-file("erl_parse.yrl", 239).
yeccpars2_139_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_140_,1}}).
-file("erl_parse.yrl", 235).
yeccpars2_140_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { lc , line ( __1 ) , __2 , __4 }
  end | __Stack].

-compile({inline,{yeccpars2_141_,1}}).
-file("erl_parse.yrl", 355).
yeccpars2_141_([__3,__2,__1 | __Stack]) ->
 [begin
   { 'query' , line ( __1 ) , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_143_,1}}).
-file("erl_parse.yrl", 283).
yeccpars2_143_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_145_,1}}).
-file("erl_parse.yrl", 287).
yeccpars2_145_([__2,__1 | __Stack]) ->
 [begin
   { clause , line ( hd ( hd ( __1 ) ) ) , [ ] , __1 , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_147_,1}}).
-file("erl_parse.yrl", 284).
yeccpars2_147_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_148_,1}}).
-file("erl_parse.yrl", 281).
yeccpars2_148_([__3,__2,__1 | __Stack]) ->
 [begin
   { 'if' , line ( __1 ) , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_150_,1}}).
-file("erl_parse.yrl", 314).
yeccpars2_150_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_151_,1}}).
-file("erl_parse.yrl", 128).
yeccpars2_151_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_157_,1}}).
-file("erl_parse.yrl", 310).
yeccpars2_157_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { 'fun' , line ( __1 ) , { function , element ( 3 , __2 ) , element ( 3 , __4 ) , element ( 3 , __6 ) } }
  end | __Stack].

-compile({inline,{yeccpars2_158_,1}}).
-file("erl_parse.yrl", 308).
yeccpars2_158_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { 'fun' , line ( __1 ) , { function , element ( 3 , __2 ) , element ( 3 , __4 ) } }
  end | __Stack].

-compile({inline,{yeccpars2_160_,1}}).
-file("erl_parse.yrl", 318).
yeccpars2_160_([__3,__2,__1 | __Stack]) ->
 [begin
   { Args , Pos } = __1 ,
    { clause , Pos , 'fun' , Args , __2 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_162_,1}}).
-file("erl_parse.yrl", 315).
yeccpars2_162_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_163_,1}}).
-file("erl_parse.yrl", 312).
yeccpars2_163_([__3,__2,__1 | __Stack]) ->
 [begin
   build_fun ( line ( __1 ) , __2 )
  end | __Stack].

-compile({inline,{yeccpars2_164_,1}}).
-file("erl_parse.yrl", 133).
yeccpars2_164_([__2,__1 | __Stack]) ->
 [begin
   { 'catch' , line ( __1 ) , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_168_,1}}).
-file("erl_parse.yrl", 291).
yeccpars2_168_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { 'case' , line ( __1 ) , __2 , __4 }
  end | __Stack].

-compile({inline,{yeccpars2_170_,1}}).
-file("erl_parse.yrl", 189).
yeccpars2_170_([__3,__2,__1 | __Stack]) ->
 [begin
   { block , line ( __1 ) , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_172_,1}}).
-file("erl_parse.yrl", 199).
yeccpars2_172_([__2,__1 | __Stack]) ->
 [begin
   { nil , line ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_173_,1}}).
-file("erl_parse.yrl", 200).
yeccpars2_173_([__3,__2,__1 | __Stack]) ->
 [begin
   { cons , line ( __1 ) , __2 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_175_,1}}).
-file("erl_parse.yrl", 202).
yeccpars2_175_([__1 | __Stack]) ->
 [begin
   { nil , line ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_178_,1}}).
-file("erl_parse.yrl", 203).
yeccpars2_178_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_180_,1}}).
-file("erl_parse.yrl", 204).
yeccpars2_180_([__3,__2,__1 | __Stack]) ->
 [begin
   { cons , line ( __2 ) , __2 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_183_,1}}).
-file("erl_parse.yrl", 220).
yeccpars2_183_(__Stack) ->
 [begin
   default
  end | __Stack].

-compile({inline,{yeccpars2_186_,1}}).
-file("erl_parse.yrl", 210).
yeccpars2_186_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_187_,1}}).
-file("erl_parse.yrl", 207).
yeccpars2_187_([__2,__1 | __Stack]) ->
 [begin
   { bin , line ( __1 ) , [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_189_,1}}).
-file("erl_parse.yrl", 211).
yeccpars2_189_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_190_,1}}).
-file("erl_parse.yrl", 208).
yeccpars2_190_([__3,__2,__1 | __Stack]) ->
 [begin
   { bin , line ( __1 ) , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_193_,1}}).
-file("erl_parse.yrl", 237).
yeccpars2_193_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { bc , line ( __1 ) , __2 , __4 }
  end | __Stack].

-compile({inline,{yeccpars2_194_,1}}).
-file("erl_parse.yrl", 223).
yeccpars2_194_(__Stack) ->
 [begin
   default
  end | __Stack].

-compile({inline,{yeccpars2_197_,1}}).
-file("erl_parse.yrl", 219).
yeccpars2_197_([__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_198_,1}}).
-file("erl_parse.yrl", 214).
yeccpars2_198_([__3,__2,__1 | __Stack]) ->
 [begin
   { bin_element , line ( __1 ) , __1 , __2 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_200_,1}}).
-file("erl_parse.yrl", 222).
yeccpars2_200_([__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_201_,1}}).
-file("erl_parse.yrl", 226).
yeccpars2_201_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_202_,1}}).
-file("erl_parse.yrl", 228).
yeccpars2_202_([__1 | __Stack]) ->
 [begin
   element ( 3 , __1 )
  end | __Stack].

-compile({inline,{yeccpars2_204_,1}}).
-file("erl_parse.yrl", 229).
yeccpars2_204_([__3,__2,__1 | __Stack]) ->
 [begin
   { element ( 3 , __1 ) , element ( 3 , __3 ) }
  end | __Stack].

-compile({inline,{yeccpars2_206_,1}}).
-file("erl_parse.yrl", 225).
yeccpars2_206_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_207_,1}}).
-file("erl_parse.yrl", 216).
yeccpars2_207_([__2,__1 | __Stack]) ->
 [begin
   mkop ( __1 , __2 )
  end | __Stack].

-compile({inline,{yeccpars2_208_,1}}).
-file("erl_parse.yrl", 175).
yeccpars2_208_([__2,__1 | __Stack]) ->
 [begin
   { record_field , line ( __1 ) , { atom , line ( __1 ) , '' } , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_210_,1}}).
-file("erl_parse.yrl", 188).
yeccpars2_210_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_212_,1}}).
-file("erl_parse.yrl", 260).
yeccpars2_212_([__3,__2,__1 | __Stack]) ->
 [begin
   { record , line ( __1 ) , element ( 3 , __2 ) , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_216_,1}}).
-file("erl_parse.yrl", 269).
yeccpars2_216_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_219_,1}}).
-file("erl_parse.yrl", 266).
yeccpars2_219_([__2,__1 | __Stack]) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_221_,1}}).
-file("erl_parse.yrl", 272).
yeccpars2_221_([__3,__2,__1 | __Stack]) ->
 [begin
   { record_field , line ( __1 ) , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_223_,1}}).
-file("erl_parse.yrl", 273).
yeccpars2_223_([__3,__2,__1 | __Stack]) ->
 [begin
   { record_field , line ( __1 ) , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_225_,1}}).
-file("erl_parse.yrl", 270).
yeccpars2_225_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_226_,1}}).
-file("erl_parse.yrl", 267).
yeccpars2_226_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_227_,1}}).
-file("erl_parse.yrl", 258).
yeccpars2_227_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { record_index , line ( __1 ) , element ( 3 , __2 ) , __4 }
  end | __Stack].

-compile({inline,{yeccpars2_229_,1}}).
-file("erl_parse.yrl", 363).
yeccpars2_229_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_232_,1}}).
-file("erl_parse.yrl", 136).
yeccpars2_232_([__3,__2,__1 | __Stack]) ->
 [begin
   { match , line ( __2 ) , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_233_,1}}).
-file("erl_parse.yrl", 137).
yeccpars2_233_([__3,__2,__1 | __Stack]) ->
 [begin
   mkop ( __1 , __2 , __3 )
  end | __Stack].

-compile({inline,{yeccpars2_235_,1}}).
-file("erl_parse.yrl", 140).
yeccpars2_235_([__3,__2,__1 | __Stack]) ->
 [begin
   mkop ( __1 , __2 , __3 )
  end | __Stack].

-compile({inline,{yeccpars2_237_,1}}).
-file("erl_parse.yrl", 143).
yeccpars2_237_([__3,__2,__1 | __Stack]) ->
 [begin
   mkop ( __1 , __2 , __3 )
  end | __Stack].

-compile({inline,{yeccpars2_247_,1}}).
-file("erl_parse.yrl", 147).
yeccpars2_247_([__3,__2,__1 | __Stack]) ->
 [begin
   mkop ( __1 , __2 , __3 )
  end | __Stack].

-compile({inline,{yeccpars2_260_,1}}).
-file("erl_parse.yrl", 155).
yeccpars2_260_([__3,__2,__1 | __Stack]) ->
 [begin
   mkop ( __1 , __2 , __3 )
  end | __Stack].

-compile({inline,{yeccpars2_268_,1}}).
-file("erl_parse.yrl", 159).
yeccpars2_268_([__3,__2,__1 | __Stack]) ->
 [begin
   mkop ( __1 , __2 , __3 )
  end | __Stack].

-compile({inline,{yeccpars2_269_,1}}).
-file("erl_parse.yrl", 151).
yeccpars2_269_([__3,__2,__1 | __Stack]) ->
 [begin
   mkop ( __1 , __2 , __3 )
  end | __Stack].

-compile({inline,{yeccpars2_270_,1}}).
-file("erl_parse.yrl", 278).
yeccpars2_270_([__2,__1 | __Stack]) ->
 [begin
   { call , line ( __1 ) , __1 , element ( 1 , __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_273_,1}}).
-file("erl_parse.yrl", 171).
yeccpars2_273_([__3,__2,__1 | __Stack]) ->
 [begin
   { remote , line ( __2 ) , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_274_,1}}).
-file("erl_parse.yrl", 177).
yeccpars2_274_([__3,__2,__1 | __Stack]) ->
 [begin
   { record_field , line ( __2 ) , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_277_,1}}).
-file("erl_parse.yrl", 264).
yeccpars2_277_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { record , line ( __2 ) , __1 , element ( 3 , __3 ) , __4 }
  end | __Stack].

-compile({inline,{yeccpars2_279_,1}}).
-file("erl_parse.yrl", 262).
yeccpars2_279_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { record_field , line ( __2 ) , __1 , element ( 3 , __3 ) , __5 }
  end | __Stack].

-compile({inline,{yeccpars2_280_,1}}).
-file("erl_parse.yrl", 359).
yeccpars2_280_([__3,__2,__1 | __Stack]) ->
 [begin
   { __2 , line ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_281_,1}}).
-file("erl_parse.yrl", 163).
yeccpars2_281_([__2,__1 | __Stack]) ->
 [begin
   mkop ( __1 , __2 )
  end | __Stack].

-compile({inline,{yeccpars2_283_,1}}).
-file("erl_parse.yrl", 417).
yeccpars2_283_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { clause , line ( __1 ) , element ( 3 , __1 ) , __2 , __3 , __4 }
  end | __Stack].

-compile({inline,{yeccpars2_284_,1}}).
-file("erl_parse.yrl", 122).
yeccpars2_284_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { clause , line ( __1 ) , element ( 3 , __1 ) , __2 , __3 , __4 }
  end | __Stack].

-compile({inline,{yeccpars2_286_,1}}).
-file("erl_parse.yrl", 419).
yeccpars2_286_([__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_291_,1}}).
-file("erl_parse.yrl", 362).
yeccpars2_291_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_293_,1}}).
-file("erl_parse.yrl", 71).
yeccpars2_293_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   build_attribute ( __2 , __4 )
  end | __Stack].

-compile({inline,{yeccpars2_297_,1}}).
-file("erl_parse.yrl", 75).
yeccpars2_297_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 , __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_298_,1}}).
-file("erl_parse.yrl", 86).
yeccpars2_298_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_302_,1}}).
-file("erl_parse.yrl", 92).
yeccpars2_302_([__1 | __Stack]) ->
 [begin
   { type , atom , [ normalise ( __1 ) ] }
  end | __Stack].

-compile({inline,{yeccpars2_303_,1}}).
-file("erl_parse.yrl", 104).
yeccpars2_303_([__1 | __Stack]) ->
 [begin
   { type , integer , [ normalise ( __1 ) ] }
  end | __Stack].

-compile({inline,{yeccpars2_306_,1}}).
-file("erl_parse.yrl", 101).
yeccpars2_306_([__2,__1 | __Stack]) ->
 [begin
   { type , tuple , [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_307_,1}}).
-file("erl_parse.yrl", 102).
yeccpars2_307_([__3,__2,__1 | __Stack]) ->
 [begin
   { type , tuple , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_310_,1}}).
-file("erl_parse.yrl", 93).
yeccpars2_310_([__3,__2,__1 | __Stack]) ->
 [begin
   build_type ( __1 , [ ] )
  end | __Stack].

-compile({inline,{yeccpars2_311_,1}}).
-file("erl_parse.yrl", 94).
yeccpars2_311_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   build_type ( __1 , [ __3 ] )
  end | __Stack].

-compile({inline,{yeccpars2_314_,1}}).
-file("erl_parse.yrl", 95).
yeccpars2_314_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   build_type ( __1 , [ __3 , __5 ] )
  end | __Stack].

-compile({inline,{yeccpars2_316_,1}}).
-file("erl_parse.yrl", 96).
yeccpars2_316_([__2,__1 | __Stack]) ->
 [begin
   { type , nil , [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_318_,1}}).
-file("erl_parse.yrl", 97).
yeccpars2_318_([__3,__2,__1 | __Stack]) ->
 [begin
   { type , list , [ __2 ] }
  end | __Stack].

-compile({inline,{yeccpars2_322_,1}}).
-file("erl_parse.yrl", 98).
yeccpars2_322_([__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { type , nonempty_list , [ __2 ] }
  end | __Stack].

-compile({inline,{yeccpars2_328_,1}}).
-file("erl_parse.yrl", 106).
yeccpars2_328_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { type , range , [ normalise ( __2 ) , normalise ( __5 ) ] }
  end | __Stack].

-compile({inline,{yeccpars2_331_,1}}).
-file("erl_parse.yrl", 108).
yeccpars2_331_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_335_,1}}).
-file("erl_parse.yrl", 111).
yeccpars2_335_([__3,__2,__1 | __Stack]) ->
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_338_,1}}).
-file("erl_parse.yrl", 99).
yeccpars2_338_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { type , 'fun' , [ [ ] , __5 ] }
  end | __Stack].

-compile({inline,{yeccpars2_340_,1}}).
-file("erl_parse.yrl", 109).
yeccpars2_340_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_344_,1}}).
-file("erl_parse.yrl", 100).
yeccpars2_344_([__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { type , 'fun' , [ __3 , __6 ] }
  end | __Stack].

-compile({inline,{yeccpars2_347_,1}}).
-file("erl_parse.yrl", 103).
yeccpars2_347_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { type , record , [ normalise ( __2 ) ] }
  end | __Stack].

-compile({inline,{yeccpars2_349_,1}}).
-file("erl_parse.yrl", 87).
yeccpars2_349_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_351_,1}}).
-file("erl_parse.yrl", 90).
yeccpars2_351_([__3,__2,__1 | __Stack]) ->
 [begin
   lift_unions ( __1 , __3 )
  end | __Stack].

-compile({inline,{yeccpars2_352_,1}}).
-file("erl_parse.yrl", 74).
yeccpars2_352_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 , __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_355_,1}}).
-file("erl_parse.yrl", 79).
yeccpars2_355_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_356_,1}}).
-file("erl_parse.yrl", 362).
yeccpars2_356_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_359_,1}}).
-file("erl_parse.yrl", 84).
yeccpars2_359_([__3,__2,__1 | __Stack]) ->
 [begin
   { typed , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_360_,1}}).
-file("erl_parse.yrl", 81).
yeccpars2_360_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_362_,1}}).
-file("erl_parse.yrl", 80).
yeccpars2_362_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_363_,1}}).
-file("erl_parse.yrl", 82).
yeccpars2_363_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_364_,1}}).
-file("erl_parse.yrl", 77).
yeccpars2_364_([__3,__2,__1 | __Stack]) ->
 [begin
   { tuple , line ( __1 ) , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_365_,1}}).
-file("erl_parse.yrl", 72).
yeccpars2_365_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   build_typed_attribute ( __2 , __4 )
  end | __Stack].

-compile({inline,{yeccpars2_366_,1}}).
-file("erl_parse.yrl", 67).
yeccpars2_366_([__2,__1 | __Stack]) ->
 [begin
   __1
  end | __Stack].

-compile({inline,{yeccpars2_367_,1}}).
-file("erl_parse.yrl", 68).
yeccpars2_367_([__2,__1 | __Stack]) ->
 [begin
   __1
  end | __Stack].

-compile({inline,{yeccpars2_369_,1}}).
-file("erl_parse.yrl", 119).
yeccpars2_369_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_371_,1}}).
-file("erl_parse.yrl", 128).
yeccpars2_371_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_373_,1}}).
-file("erl_parse.yrl", 69).
yeccpars2_373_([__2,__1 | __Stack]) ->
 [begin
   __1
  end | __Stack].

-compile({inline,{yeccpars2_375_,1}}).
-file("erl_parse.yrl", 414).
yeccpars2_375_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_377_,1}}).
-file("erl_parse.yrl", 128).
yeccpars2_377_(__Stack) ->
 [begin
   [ ]
  end | __Stack].


-file("erl_parse.yrl", 912).
