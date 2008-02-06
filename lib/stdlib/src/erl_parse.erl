-module(erl_parse).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("erl_parse.yrl", 482).

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
-compile([{hipe,[{regalloc,linear_scan}]}]).


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

-type(attributes() :: 'export' | 'file' | 'import' | 'module'
		    | 'record' | 'spec' | 'type').

build_typed_attribute({atom,La,record}, [{atom,_Ln,RecordName},
                                         {typed_record, RecTuple}]) ->
    {attribute,La,record,{RecordName,record_tuple(RecTuple)}};
build_typed_attribute({atom,La,spec}, [{op,_Lo,'/',{atom,_La,FunName},
				                   {integer,_Li,FunArity}},
				       {type_sigs, TypeSpec}])  ->
    {attribute,La,spec,{{FunName,FunArity},TypeSpec}};
build_typed_attribute({atom,La,spec},
                      [{op,_,'/',{remote,_,{atom,_,ModName},
                                           {atom,_,FunName}},
                                 {integer,_,FunArity}},
                       {type_sigs, TypeSpec}]) ->
    {attribute,La,spec,{{ModName,FunName,FunArity},TypeSpec}};
build_typed_attribute({atom,La,type}, 
                      [{call,_,{atom,_,TypeName},Args},{type_def, Type}]) ->
    {attribute,La,type,{TypeName,Type,Args}};
build_typed_attribute({atom,La,Atom},_) ->
    case Atom of
        record -> error_bad_decl(La,record);
        spec   -> error_bad_decl(La,spec);
        type   -> error_bad_decl(La,type);
        _      -> return_error(La, "bad attribute")
    end.

lift_unions(T1, {type, _La, union, List}) ->
    {type, line(T1), union, [T1|List]};
lift_unions(T1, T2) ->
    {type, line(T1), union, [T1, T2]}.

build_gen_type({atom, La, tuple}) ->
    {type, La, tuple, any};
build_gen_type({atom, La, Name}) ->
    {type, La, Name, []}.

build_bin_type([{var, _, '_'}|Left], Int) ->
    build_bin_type(Left, Int);
build_bin_type([], Int) ->
    Int;
build_bin_type([{var, La, _}|_], _) ->
    return_error(La, "Bad binary type").

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

-spec(error_bad_decl/2 :: (integer(), attributes()) -> no_return()).

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

record_fields([{atom,La,A}|Fields]) ->
    [{record_field,La,{atom,La,A}}|record_fields(Fields)];
record_fields([{match,_Lm,{atom,La,A},Expr}|Fields]) ->
    [{record_field,La,{atom,La,A},Expr}|record_fields(Fields)];
record_fields([{typed,Expr,TypeInfo}|Fields]) ->
    [Field] = record_fields([Expr]),
    TypeInfo1 = 
	case Expr of
	    {match, _, _, _} -> TypeInfo; %% If we have an initializer.
	    {atom, La, _} -> 
                lift_unions(abstract(undefined, La), TypeInfo)
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
	error -> erlang:error({badarg, A});
	As -> list_to_atom(packages:concat(As))
    end;
%% Special case for unary +/-.
normalise({op,_,'+',{char,_,I}}) -> I;
normalise({op,_,'+',{integer,_,I}}) -> I;
normalise({op,_,'+',{float,_,F}}) -> F;
normalise({op,_,'-',{char,_,I}}) -> -I;		%Weird, but compatible!
normalise({op,_,'-',{integer,_,I}}) -> -I;
normalise({op,_,'-',{float,_,F}}) -> -F;
normalise(X) -> erlang:error({badarg, X}).

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

-file("/ldisk/daily_build/otp_prebuild_r12b.2008-02-05_20/otp_src_R12B-1/bootstrap/lib/parsetools/include/yeccpre.hrl", 0).
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
-spec(return_error/2 :: (integer(), any()) -> no_return()).
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.2").

yeccpars0(Tokens, MFA) ->
    try yeccpars1(Tokens, MFA, 0, [], [])
    catch 
        error: Error ->
            Stacktrace = erlang:get_stacktrace(),
            try yecc_error_type(Error, Stacktrace) of
                {syntax_error, Token} ->
                    yeccerror(Token);
                {missing_in_goto_table=Tag, State} ->
                    Desc = {State, Tag},
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                Stacktrace);
                {missing_in_goto_table=Tag, Symbol, State} ->
                    Desc = {Symbol, State, Tag},
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        throw: {error, {_Line, ?MODULE, _M}} = Error -> 
            Error % probably from return_error/2
    end.

yecc_error_type(function_clause, [{?MODULE,F,[_,_,_,_,Token,_,_]} | _]) ->
    "yeccpars2" ++ _ = atom_to_list(F),
    {syntax_error, Token};
yecc_error_type({case_clause,{State}}, [{?MODULE,yeccpars2,_}|_]) ->
    %% Inlined goto-function
    {missing_in_goto_table, State};
yecc_error_type(function_clause, [{?MODULE,F,[State]}|_]) ->
    "yeccgoto_" ++ SymbolL = atom_to_list(F),
    {ok,[{atom,_,Symbol}]} = erl_scan:string(SymbolL),
    {missing_in_goto_table, Symbol, State}.

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

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Stack1, [Token | Tokens], 
          Tokenizer) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Stack1 | Vstack], Token, Tokens, Tokenizer);
yeccpars1(State1, State, States, Vstack, Stack1, [], {F, A}) ->
    case apply(F, A) of
        {ok, Tokens, _Endline} ->
	    yeccpars1(State1, State, States, Vstack, Stack1, Tokens, {F, A});
        {eof, _Endline} ->
            yeccpars1(State1, State, States, Vstack, Stack1, [], false);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1(State1, State, States, Vstack, Stack1, [], false) ->
    yeccpars2(State, '$end', [State1 | States], [Stack1 | Vstack],
              {'$end', 999999}, [], false).

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



-file("./erl_parse.erl", 621).

yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(120=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(125=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(126=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(128=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(130=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(147=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(148=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(149=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(150=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(151=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(152=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(153=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(154=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(155=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(156=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(157=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(158=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(159=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(160=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(161=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(162=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(163=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(164=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_164(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(165=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(166=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(167=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(168=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(169=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(170=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(171=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_171(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(172=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(173=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(174=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(175=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(176=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(177=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(178=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(179=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(180=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(181=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(182=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_182(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(183=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(184=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(185=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_185(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(186=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_186(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(187=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_187(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(188=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(189=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(190=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_190(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(191=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(192=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(193=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_193(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(194=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_194(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(195=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(196=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_196(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(197=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_197(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(198=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(199=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(200=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(201=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(202=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(203=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(204=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(205=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(206=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(207=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_207(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(208=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(209=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_209(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(210=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_210(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(211=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_211(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(212=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(213=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(214=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(215=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_215(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(216=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_216(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(217=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(218=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_218(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(219=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_219(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(220=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(221=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(222=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(223=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_223(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(224=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_224(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(225=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_225(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(226=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_226(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(227=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(228=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(229=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(230=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(231=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(232=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_232(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(233=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_233(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(234=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(235=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_235(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(236=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(237=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_237(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(238=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(239=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_239(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(240=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_240(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(241=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_241(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(242=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_242(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(243=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(244=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(245=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_245(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(246=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_246(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(247=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_247(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(248=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(249=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(250=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_250(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(251=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_251(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(252=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_252(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(253=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_253(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(254=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(255=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_255(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(256=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_256(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(257=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_257(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(258=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(259=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_259(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(260=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_260(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(261=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(262=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_262(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(263=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_263(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(264=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_264(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(265=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_265(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(266=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_266(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(267=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_267(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(268=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_268(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(269=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_269(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(270=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_270(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(271=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_271(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(272=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(273=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_273(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(274=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_274(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(275=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_275(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(276=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_276(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(277=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_277(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(278=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_278(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(279=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_279(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(280=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_280(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(281=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_281(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(282=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_282(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(283=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_283(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(284=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_284(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(285=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(286=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_286(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(287=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_287(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(288=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(289=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_289(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(290=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_290(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(291=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_291(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(292=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_292(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(293=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_293(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(294=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_294(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(295=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_295(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(296=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_296(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(297=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_297(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(298=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_298(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(299=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_299(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(300=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_300(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(301=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_301(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(302=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_302(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(303=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_303(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(304=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_304(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(305=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_305(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(306=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_306(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(307=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_307(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(308=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_308(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(309=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_309(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(310=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_310(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(311=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_311(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(312=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_312(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(313=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_313(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(314=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_314(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(315=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_315(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(316=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_316(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(317=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_317(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(318=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_318(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(319=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_319(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(320=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_320(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(321=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_321(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(322=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_322(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(323=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_323(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(324=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_324(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(325=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_325(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(326=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_326(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(327=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_327(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(328=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_328(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(329=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_329(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(330=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_316(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(331=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_331(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(332=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_332(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(333=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_333(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(334=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_334(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(335=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_316(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(336=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_336(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(337=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_316(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(338=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_338(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(339=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_339(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(340=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_340(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(341=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(342=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_316(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(343=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(344=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(345=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(346=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_346(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(347=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_347(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(348=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_348(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(349=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(350=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(351=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_351(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(352=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_352(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(353=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_353(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(354=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_354(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(355=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_355(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(356=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_356(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(357=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_357(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(358=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_358(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(359=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_359(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(360=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_360(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(361=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_361(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(362=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_362(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(363=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_363(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(364=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_364(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(365=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_365(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(366=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_366(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(367=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_367(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(368=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_368(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(369=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_369(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(370=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_370(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(371=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_371(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(372=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_372(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(373=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_373(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(374=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_374(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(375=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_375(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(376=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_376(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(377=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_377(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(378=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_378(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(379=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_379(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(380=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_316(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(381=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_381(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(382=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_382(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(383=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_383(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(384=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_384(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(385=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_385(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(386=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_386(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(387=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_387(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(388=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_388(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(389=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_316(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(390=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_390(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(391=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_391(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(392=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_385(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(393=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_393(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(394=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_394(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(395=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_395(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(396=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_396(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(397=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_316(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(398=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_398(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(399=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_399(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(400=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_400(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(401=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_401(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(402=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(403=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_403(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(404=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_404(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(405=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_405(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(406=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(407=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_316(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(408=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_408(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(409=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_409(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(410=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(411=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_411(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(412=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_412(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(413=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_413(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(414=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_414(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(415=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_415(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(416=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_416(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(417=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_417(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(418=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_418(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(419=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(420=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_420(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(421=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(422=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_422(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(423=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_423(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(424=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_424(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(425=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(426=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_426(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(427=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_427(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.2",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr).

yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_1_(Stack),
 yeccpars2(yeccgoto_rule(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_2(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 423, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_2_(Stack),
 yeccpars2(yeccgoto_rule_clauses(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_3(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 422, Ss, Stack, T, Ts, Tzr).

yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_4_(Stack),
 yeccpars2(yeccgoto_function(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_5(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 417, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_5_(Stack),
 yeccpars2(yeccgoto_function_clauses(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_6(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 416, Ss, Stack, T, Ts, Tzr).

yeccpars2_7(_S, '$end', _Ss, Stack,  _T, _Ts, _Tzr) ->
 {ok, hd(Stack)}.

yeccpars2_8(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 415, Ss, Stack, T, Ts, Tzr).

yeccpars2_9(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 287, Ss, Stack, T, Ts, Tzr).

yeccpars2_10(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr).

yeccpars2_11(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_11_(Stack),
 yeccpars2(282, Cat, [11 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_12_(Stack),
 yeccpars2(yeccgoto_clause_args(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_13(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, 'case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, 'query', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr).

yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_max(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_max(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_atomic(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_700(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_max(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_max(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_20(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, 'case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, 'query', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr).

yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_max(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_max(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_max(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_700(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_max(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_26(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 280, Ss, Stack, T, Ts, Tzr).

yeccpars2_27(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 275, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_900(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_28(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 271, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 272, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_800(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_29(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_700(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_600(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_500(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_32(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 262, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 263, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 264, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, 'band', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 265, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, 'div', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 266, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 267, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_400(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_33(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 250, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 251, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 252, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 253, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 254, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 255, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 256, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 257, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 258, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 259, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_300(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_34(S, '/=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 239, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 240, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 241, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 242, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, '=<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 243, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 244, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 245, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 246, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_200(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_35(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 236, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_160(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_36(S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 234, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_150(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_37(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 230, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 231, Ss, Stack, T, Ts, Tzr);
yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_100(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_39(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 228, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_39_(Stack),
 yeccpars2(yeccgoto_exprs(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_max(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_max(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_max(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_max(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_44(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 211, Ss, Stack, T, Ts, Tzr).

yeccpars2_45(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, 'case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, 'query', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr).

yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_46_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_argument_list(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_prefix_op(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_prefix_op(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_49(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 208, Ss, Stack, T, Ts, Tzr).

yeccpars2_50(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, '>>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, 'case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, 'query', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr).

yeccpars2_51(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, 'case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, 'query', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr).

yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_atomic(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_53: see yeccpars2_45

yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_prefix_op(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_55: see yeccpars2_45

%% yeccpars2_56: see yeccpars2_45

yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_atomic(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_atomic(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_59(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 152, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_60: see yeccpars2_45

yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_atomic(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_prefix_op(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_63(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 127, Ss, Stack, T, Ts, Tzr).

yeccpars2_64(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, 'after', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, 'case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, 'query', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr).

yeccpars2_65(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_strings(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_66: see yeccpars2_45

yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_max(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_68(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, 'case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, 'query', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_68(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr).

yeccpars2_69(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr).

yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_70_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_tuple(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_71_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_tuple(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_72(S, 'after', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, 'of', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr).

yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_73_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_try_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_74: see yeccpars2_45

yeccpars2_75(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, 'case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, 'query', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_75(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_76: see yeccpars2_45

yeccpars2_77(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_77_(Stack),
 yeccpars2(83, Cat, [77 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_78(S, 'after', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr).

yeccpars2_79(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_79_(Stack),
 yeccpars2(yeccgoto_cr_clauses(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_80: see yeccpars2_45

yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_81_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_cr_clauses(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_82_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_try_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_83(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_84: see yeccpars2_45

yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_85_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_clause_guard(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_86(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_86_(Stack),
 yeccpars2(yeccgoto_guard(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_87: see yeccpars2_45

yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_88_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_guard(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_cr_clause(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_90: see yeccpars2_45

yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_91_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_clause_body(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_92(S, 'after', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 109, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr).

yeccpars2_93(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_93_(Stack),
 yeccpars2(yeccgoto_try_clauses(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_94(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_(Stack),
 yeccpars2(105, Cat, [94 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_95(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_atomic(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_96(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_max(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_97: see yeccpars2_45

yeccpars2_98(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_(Stack),
 yeccpars2(99, Cat, [98 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_99: see yeccpars2_83

yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_100_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_try_clause(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_101: see yeccpars2_45

yeccpars2_102(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_(Stack),
 yeccpars2(103, Cat, [102 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_103: see yeccpars2_83

yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_104_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_try_clause(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_105: see yeccpars2_83

yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_try_clause(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_107: see yeccpars2_75

yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_108_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_try_clauses(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_109: see yeccpars2_45

yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_110_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_try_catch(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_111(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr).

yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_112_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_try_catch(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_113(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr).

yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_114_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_try_catch(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_115_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_strings(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_116(S, 'after', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_117: see yeccpars2_45

%% yeccpars2_118: see yeccpars2_83

yeccpars2_119(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr).

yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_120_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_receive_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_121: see yeccpars2_45

yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_122_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_receive_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_123: see yeccpars2_83

yeccpars2_124(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr).

yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_receive_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_126(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_127: see yeccpars2_45

yeccpars2_128(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 129, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_129: see yeccpars2_45

yeccpars2_130(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr).

yeccpars2_131(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_131_(Stack),
 yeccpars2(yeccgoto_lc_exprs(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_132(S, '<-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_lc_expr(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_133(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_max(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_134: see yeccpars2_45

yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_135_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_lc_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_136: see yeccpars2_45

yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_137_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_lc_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_138: see yeccpars2_45

yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_139_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_lc_exprs(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_140(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_140_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_list_comprehension(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_141_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_query_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_142(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr).

yeccpars2_143(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 146, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_143_(Stack),
 yeccpars2(yeccgoto_if_clauses(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_144: see yeccpars2_83

yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_145_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_if_clause(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_146: see yeccpars2_45

yeccpars2_147(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_147_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_if_clauses(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_148(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_148_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_if_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_149(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr).

yeccpars2_150(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_(Stack),
 yeccpars2(yeccgoto_fun_clauses(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_151(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_151_(Stack),
 yeccpars2(159, Cat, [151 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_152(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 153, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 154, Ss, Stack, T, Ts, Tzr).

yeccpars2_153(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr).

yeccpars2_154(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 155, Ss, Stack, T, Ts, Tzr).

yeccpars2_155(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr).

yeccpars2_156(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 157, Ss, Stack, T, Ts, Tzr).

yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_157_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_fun_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_158_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_fun_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_159: see yeccpars2_83

yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_160_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_fun_clause(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_161: see yeccpars2_10

yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_162_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_fun_clauses(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_163(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_163_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_fun_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_164(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_164_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_165(S, 'of', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 166, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_166: see yeccpars2_45

yeccpars2_167(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 168, Ss, Stack, T, Ts, Tzr).

yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_168_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_case_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_169(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 170, Ss, Stack, T, Ts, Tzr).

yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_170_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_expr_max(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_171(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 175, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 129, Ss, Stack, T, Ts, Tzr).

yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_172_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_list(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_173_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_list(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_174: see yeccpars2_45

yeccpars2_175(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_175_(Stack),
 yeccpars2(yeccgoto_tail(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_176: see yeccpars2_45

yeccpars2_177(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr).

yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_178_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_tail(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_179(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 175, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr).

yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_180_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_tail(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_181(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'query', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr).

yeccpars2_182(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_bit_expr(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_183(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_183_(Stack),
 yeccpars2(194, Cat, [183 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_184(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr_max(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_185(S, '>>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr).

yeccpars2_186(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_186_(Stack),
 yeccpars2(yeccgoto_bin_elements(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_187(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_187_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_binary(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_188(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(S, 'case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(S, 'query', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr).

yeccpars2_189(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_189_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_bin_elements(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_190(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_190_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_binary(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_191: see yeccpars2_45

yeccpars2_192(S, '>>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr).

yeccpars2_193(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_193_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_binary_comprehension(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_194(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_194_(Stack),
 yeccpars2(198, Cat, [194 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_195: see yeccpars2_181

yeccpars2_196(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_bit_size_expr(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_197(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_197_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_opt_bit_size_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_198_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_bin_element(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_199(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr).

yeccpars2_200(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_200_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_opt_bit_type_list(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_201(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 205, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_201_(Stack),
 yeccpars2(yeccgoto_bit_type_list(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_202(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_202_(Stack),
 yeccpars2(yeccgoto_bit_type(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_203(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr).

yeccpars2_204(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_204_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_bit_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_205: see yeccpars2_199

yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_bit_type_list(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_207(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_207_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_bit_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_208(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_208_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_expr_900(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_209(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 210, Ss, Stack, T, Ts, Tzr).

yeccpars2_210(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_210_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_expr_max(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_211(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 213, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 214, Ss, Stack, T, Ts, Tzr).

yeccpars2_212(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_212_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_record_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_213(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 227, Ss, Stack, T, Ts, Tzr).

yeccpars2_214(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 217, Ss, Stack, T, Ts, Tzr);
yeccpars2_214(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 218, Ss, Stack, T, Ts, Tzr);
yeccpars2_214(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 219, Ss, Stack, T, Ts, Tzr).

yeccpars2_215(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 226, Ss, Stack, T, Ts, Tzr).

yeccpars2_216(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 224, Ss, Stack, T, Ts, Tzr);
yeccpars2_216(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_216_(Stack),
 yeccpars2(yeccgoto_record_fields(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_217(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 222, Ss, Stack, T, Ts, Tzr).

yeccpars2_218(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 220, Ss, Stack, T, Ts, Tzr).

yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_219_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_record_tuple(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_220: see yeccpars2_45

yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_221_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_record_field(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_222: see yeccpars2_45

yeccpars2_223(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_223_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_record_field(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_224(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 217, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 218, Ss, Stack, T, Ts, Tzr).

yeccpars2_225(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_225_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_record_fields(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_226(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_226_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_record_tuple(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_227_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_record_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_228: see yeccpars2_45

yeccpars2_229(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_229_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_exprs(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_230(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(S, 'case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(S, 'query', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_231: see yeccpars2_230

yeccpars2_232(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_232_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_expr_100(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_233(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_233_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_expr_100(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_234: see yeccpars2_230

yeccpars2_235(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_235_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_expr_150(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_236: see yeccpars2_230

yeccpars2_237(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_237_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_expr_160(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_238: see yeccpars2_230

yeccpars2_239(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_comp_op(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_240(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_comp_op(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_241(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_comp_op(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_242(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_comp_op(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_243(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_comp_op(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_244(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_comp_op(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_245(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_comp_op(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_246(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_comp_op(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_247(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_247_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_expr_200(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_248: see yeccpars2_230

%% yeccpars2_249: see yeccpars2_230

yeccpars2_250(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_add_op(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_251(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_list_op(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_252(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_add_op(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_253(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_list_op(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_254(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_add_op(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_255(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_add_op(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_256(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_add_op(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_257(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_add_op(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_258(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_add_op(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_259(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_add_op(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_260(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 262, Ss, Stack, T, Ts, Tzr);
yeccpars2_260(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 263, Ss, Stack, T, Ts, Tzr);
yeccpars2_260(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 264, Ss, Stack, T, Ts, Tzr);
yeccpars2_260(S, 'band', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 265, Ss, Stack, T, Ts, Tzr);
yeccpars2_260(S, 'div', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 266, Ss, Stack, T, Ts, Tzr);
yeccpars2_260(S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 267, Ss, Stack, T, Ts, Tzr);
yeccpars2_260(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_260_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_expr_400(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_261: see yeccpars2_230

yeccpars2_262(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_mult_op(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_263(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_mult_op(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_264(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_mult_op(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_265(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_mult_op(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_266(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_mult_op(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_267(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_mult_op(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_268(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_268_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_expr_500(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_269(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_269_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_expr_300(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_270(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_270_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_function_call(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_271(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 274, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_272: see yeccpars2_181

yeccpars2_273(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_273_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_expr_800(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_274(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_274_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_expr_900(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_275(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 276, Ss, Stack, T, Ts, Tzr).

yeccpars2_276(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 278, Ss, Stack, T, Ts, Tzr);
yeccpars2_276(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 214, Ss, Stack, T, Ts, Tzr).

yeccpars2_277(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_277_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_record_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_278(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 279, Ss, Stack, T, Ts, Tzr).

yeccpars2_279(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_279_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_record_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_280(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_280_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_argument_list(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_281(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_281_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_expr_600(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_282(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_282(S, ':-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 285, Ss, Stack, T, Ts, Tzr).

yeccpars2_283(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_283_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_rule_clause(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_284(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_284_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_function_clause(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_285: see yeccpars2_45

yeccpars2_286(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_286_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_rule_body(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_287(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 288, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_288: see yeccpars2_45

yeccpars2_289(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 414, Ss, Stack, T, Ts, Tzr).

yeccpars2_290(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_attr_val(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_291(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 294, Ss, Stack, T, Ts, Tzr);
yeccpars2_291(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 295, Ss, Stack, T, Ts, Tzr);
yeccpars2_291(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_291_(Stack),
 yeccpars2(yeccgoto_exprs(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_292(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 293, Ss, Stack, T, Ts, Tzr).

yeccpars2_293(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_293_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_attribute(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_294(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, 'case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, 'query', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 402, Ss, Stack, T, Ts, Tzr).

yeccpars2_295(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 303, Ss, Stack, T, Ts, Tzr);
yeccpars2_295(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 304, Ss, Stack, T, Ts, Tzr);
yeccpars2_295(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 305, Ss, Stack, T, Ts, Tzr);
yeccpars2_295(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 306, Ss, Stack, T, Ts, Tzr);
yeccpars2_295(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 307, Ss, Stack, T, Ts, Tzr);
yeccpars2_295(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 308, Ss, Stack, T, Ts, Tzr);
yeccpars2_295(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 309, Ss, Stack, T, Ts, Tzr);
yeccpars2_295(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 310, Ss, Stack, T, Ts, Tzr);
yeccpars2_295(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 311, Ss, Stack, T, Ts, Tzr);
yeccpars2_295(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 312, Ss, Stack, T, Ts, Tzr).

yeccpars2_296(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_296_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_typed_attr_val(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_297(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 399, Ss, Stack, T, Ts, Tzr);
yeccpars2_297(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_297_(Stack),
 yeccpars2(yeccgoto_type_sigs(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_298(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 397, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_top_type(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_299(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_299_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_typed_attr_val(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_300(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 394, Ss, Stack, T, Ts, Tzr);
yeccpars2_300(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_301(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 385, Ss, Stack, T, Ts, Tzr);
yeccpars2_301(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_type_sig(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_302(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_303(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 374, Ss, Stack, T, Ts, Tzr).

yeccpars2_304(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 303, Ss, Stack, T, Ts, Tzr);
yeccpars2_304(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 327, Ss, Stack, T, Ts, Tzr);
yeccpars2_304(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 305, Ss, Stack, T, Ts, Tzr);
yeccpars2_304(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 306, Ss, Stack, T, Ts, Tzr);
yeccpars2_304(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 307, Ss, Stack, T, Ts, Tzr);
yeccpars2_304(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 308, Ss, Stack, T, Ts, Tzr);
yeccpars2_304(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 309, Ss, Stack, T, Ts, Tzr);
yeccpars2_304(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 310, Ss, Stack, T, Ts, Tzr);
yeccpars2_304(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 329, Ss, Stack, T, Ts, Tzr);
yeccpars2_304(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 312, Ss, Stack, T, Ts, Tzr).

yeccpars2_305(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 373, Ss, Stack, T, Ts, Tzr).

yeccpars2_306(S, '>>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 359, Ss, Stack, T, Ts, Tzr);
yeccpars2_306(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 360, Ss, Stack, T, Ts, Tzr).

yeccpars2_307(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 303, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 305, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 306, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 307, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 350, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 308, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 309, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 310, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 311, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 312, Ss, Stack, T, Ts, Tzr).

yeccpars2_308(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 345, Ss, Stack, T, Ts, Tzr);
yeccpars2_308(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_309(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 319, Ss, Stack, T, Ts, Tzr).

yeccpars2_310(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_int_type(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_311(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_312(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 303, Ss, Stack, T, Ts, Tzr);
yeccpars2_312(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 305, Ss, Stack, T, Ts, Tzr);
yeccpars2_312(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 306, Ss, Stack, T, Ts, Tzr);
yeccpars2_312(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 307, Ss, Stack, T, Ts, Tzr);
yeccpars2_312(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 308, Ss, Stack, T, Ts, Tzr);
yeccpars2_312(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 309, Ss, Stack, T, Ts, Tzr);
yeccpars2_312(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 310, Ss, Stack, T, Ts, Tzr);
yeccpars2_312(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 311, Ss, Stack, T, Ts, Tzr);
yeccpars2_312(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 312, Ss, Stack, T, Ts, Tzr);
yeccpars2_312(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 315, Ss, Stack, T, Ts, Tzr).

yeccpars2_313(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 318, Ss, Stack, T, Ts, Tzr).

yeccpars2_314(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 316, Ss, Stack, T, Ts, Tzr);
yeccpars2_314(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_314_(Stack),
 yeccpars2(yeccgoto_top_types(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_315(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_315_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_316(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 303, Ss, Stack, T, Ts, Tzr);
yeccpars2_316(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 305, Ss, Stack, T, Ts, Tzr);
yeccpars2_316(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 306, Ss, Stack, T, Ts, Tzr);
yeccpars2_316(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 307, Ss, Stack, T, Ts, Tzr);
yeccpars2_316(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 308, Ss, Stack, T, Ts, Tzr);
yeccpars2_316(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 309, Ss, Stack, T, Ts, Tzr);
yeccpars2_316(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 310, Ss, Stack, T, Ts, Tzr);
yeccpars2_316(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 311, Ss, Stack, T, Ts, Tzr);
yeccpars2_316(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 312, Ss, Stack, T, Ts, Tzr).

yeccpars2_317(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_317_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_top_types(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_318(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_318_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_319(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 322, Ss, Stack, T, Ts, Tzr);
yeccpars2_319(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 323, Ss, Stack, T, Ts, Tzr).

yeccpars2_320(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 344, Ss, Stack, T, Ts, Tzr).

yeccpars2_321(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_fun_type_100(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_322(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 303, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 327, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 305, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 328, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 306, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 307, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 308, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 309, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 310, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 329, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 312, Ss, Stack, T, Ts, Tzr).

yeccpars2_323(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_323_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_324(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_arg_type(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_325(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 341, Ss, Stack, T, Ts, Tzr).

yeccpars2_326(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 339, Ss, Stack, T, Ts, Tzr);
yeccpars2_326(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_326_(Stack),
 yeccpars2(yeccgoto_arg_types(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_327(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 337, Ss, Stack, T, Ts, Tzr).

yeccpars2_328(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 332, Ss, Stack, T, Ts, Tzr).

yeccpars2_329(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 330, Ss, Stack, T, Ts, Tzr);
yeccpars2_329(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_330: see yeccpars2_316

yeccpars2_331(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_331_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_arg_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_332(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 333, Ss, Stack, T, Ts, Tzr).

yeccpars2_333(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 334, Ss, Stack, T, Ts, Tzr).

yeccpars2_334(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 335, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_335: see yeccpars2_316

yeccpars2_336(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_336_(Stack),
 Nss = lists:nthtail(6, Ss),
 yeccpars2(yeccgoto_fun_type_100(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_337: see yeccpars2_316

yeccpars2_338(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_338_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_fun_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_339(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 303, Ss, Stack, T, Ts, Tzr);
yeccpars2_339(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 305, Ss, Stack, T, Ts, Tzr);
yeccpars2_339(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 306, Ss, Stack, T, Ts, Tzr);
yeccpars2_339(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 307, Ss, Stack, T, Ts, Tzr);
yeccpars2_339(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 308, Ss, Stack, T, Ts, Tzr);
yeccpars2_339(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 309, Ss, Stack, T, Ts, Tzr);
yeccpars2_339(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 310, Ss, Stack, T, Ts, Tzr);
yeccpars2_339(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 329, Ss, Stack, T, Ts, Tzr);
yeccpars2_339(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 312, Ss, Stack, T, Ts, Tzr).

yeccpars2_340(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_340_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_arg_types(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_341(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 342, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_342: see yeccpars2_316

yeccpars2_343(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_343_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_fun_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_344(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_344_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_345(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 303, Ss, Stack, T, Ts, Tzr);
yeccpars2_345(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 347, Ss, Stack, T, Ts, Tzr);
yeccpars2_345(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 305, Ss, Stack, T, Ts, Tzr);
yeccpars2_345(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 306, Ss, Stack, T, Ts, Tzr);
yeccpars2_345(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 307, Ss, Stack, T, Ts, Tzr);
yeccpars2_345(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 308, Ss, Stack, T, Ts, Tzr);
yeccpars2_345(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 309, Ss, Stack, T, Ts, Tzr);
yeccpars2_345(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 310, Ss, Stack, T, Ts, Tzr);
yeccpars2_345(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 311, Ss, Stack, T, Ts, Tzr);
yeccpars2_345(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 312, Ss, Stack, T, Ts, Tzr).

yeccpars2_346(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 348, Ss, Stack, T, Ts, Tzr).

yeccpars2_347(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_347_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_348(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_348_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_349(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 351, Ss, Stack, T, Ts, Tzr);
yeccpars2_349(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 352, Ss, Stack, T, Ts, Tzr).

yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_350_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_351(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 353, Ss, Stack, T, Ts, Tzr).

yeccpars2_352(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_352_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_353(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 354, Ss, Stack, T, Ts, Tzr).

yeccpars2_354(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 355, Ss, Stack, T, Ts, Tzr).

yeccpars2_355(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 356, Ss, Stack, T, Ts, Tzr).

yeccpars2_356(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_356_(Stack),
 Nss = lists:nthtail(6, Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_357(S, '>>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 372, Ss, Stack, T, Ts, Tzr).

yeccpars2_358(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 366, Ss, Stack, T, Ts, Tzr);
yeccpars2_358(S, '>>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 367, Ss, Stack, T, Ts, Tzr).

yeccpars2_359(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_359_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_binary_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_360(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 361, Ss, Stack, T, Ts, Tzr).

yeccpars2_361(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 362, Ss, Stack, T, Ts, Tzr);
yeccpars2_361(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 363, Ss, Stack, T, Ts, Tzr).

yeccpars2_362(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_362_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_bin_base_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_363(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 364, Ss, Stack, T, Ts, Tzr).

yeccpars2_364(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 365, Ss, Stack, T, Ts, Tzr).

yeccpars2_365(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_365_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_bin_unit_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_366(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 369, Ss, Stack, T, Ts, Tzr).

yeccpars2_367(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_367_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_binary_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_368(S, '>>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 371, Ss, Stack, T, Ts, Tzr).

yeccpars2_369(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 370, Ss, Stack, T, Ts, Tzr).

yeccpars2_370(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 363, Ss, Stack, T, Ts, Tzr).

yeccpars2_371(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_371_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_binary_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_372(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_372_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_binary_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_373(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_373_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_int_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_374(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 375, Ss, Stack, T, Ts, Tzr).

yeccpars2_375(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 378, Ss, Stack, T, Ts, Tzr);
yeccpars2_375(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 379, Ss, Stack, T, Ts, Tzr).

yeccpars2_376(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 384, Ss, Stack, T, Ts, Tzr).

yeccpars2_377(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 382, Ss, Stack, T, Ts, Tzr);
yeccpars2_377(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_377_(Stack),
 yeccpars2(yeccgoto_field_types(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_378(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 380, Ss, Stack, T, Ts, Tzr).

yeccpars2_379(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_379_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_380: see yeccpars2_316

yeccpars2_381(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_381_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_field_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_382(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 378, Ss, Stack, T, Ts, Tzr).

yeccpars2_383(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_383_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_field_types(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_384(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_384_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_385(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 388, Ss, Stack, T, Ts, Tzr).

yeccpars2_386(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_386_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_type_sig(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_387(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 392, Ss, Stack, T, Ts, Tzr);
yeccpars2_387(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_387_(Stack),
 yeccpars2(yeccgoto_type_guards(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_388(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 389, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_389: see yeccpars2_316

yeccpars2_390(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 391, Ss, Stack, T, Ts, Tzr).

yeccpars2_391(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_391_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_type_guard(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_392: see yeccpars2_385

yeccpars2_393(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_393_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_type_guards(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_394(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 395, Ss, Stack, T, Ts, Tzr).

yeccpars2_395(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 305, Ss, Stack, T, Ts, Tzr);
yeccpars2_395(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 310, Ss, Stack, T, Ts, Tzr).

yeccpars2_396(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_396_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_397: see yeccpars2_316

yeccpars2_398(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_398_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_top_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_399(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 304, Ss, Stack, T, Ts, Tzr).

yeccpars2_400(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_400_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_type_sigs(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_401(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_401_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_typed_attr_val(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_402: see yeccpars2_68

yeccpars2_403(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 413, Ss, Stack, T, Ts, Tzr).

yeccpars2_404(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 410, Ss, Stack, T, Ts, Tzr);
yeccpars2_404(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_404_(Stack),
 yeccpars2(yeccgoto_typed_exprs(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_405(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 406, Ss, Stack, T, Ts, Tzr);
yeccpars2_405(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 407, Ss, Stack, T, Ts, Tzr);
yeccpars2_405(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_405_(Stack),
 yeccpars2(yeccgoto_exprs(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_406: see yeccpars2_45

%% yeccpars2_407: see yeccpars2_316

yeccpars2_408(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_408_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_typed_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_409(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_409_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_typed_exprs(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_410: see yeccpars2_45

yeccpars2_411(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_411_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_typed_exprs(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_412(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_412_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_typed_exprs(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_413(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_413_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_typed_record_fields(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_414(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_414_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_attribute(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_415(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_415_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_form(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_416(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_416_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_form(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_417(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 419, Ss, Stack, T, Ts, Tzr).

yeccpars2_418(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_418_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_function_clauses(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_419: see yeccpars2_10

yeccpars2_420(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_420_(Stack),
 yeccpars2(421, Cat, [420 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_421: see yeccpars2_83

yeccpars2_422(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_422_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_form(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_423(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 425, Ss, Stack, T, Ts, Tzr).

yeccpars2_424(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_424_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_rule_clauses(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_425: see yeccpars2_10

yeccpars2_426(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_426(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_426_(Stack),
 yeccpars2(427, Cat, [426 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_427(S, ':-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 285, Ss, Stack, T, Ts, Tzr).

yeccgoto_add_op(33) -> 249.

yeccgoto_arg_type(304) -> 326;
yeccgoto_arg_type(322) -> 326;
yeccgoto_arg_type(339) -> 326.

yeccgoto_arg_types(304) -> 325;
yeccgoto_arg_types(322) -> 325;
yeccgoto_arg_types(339) -> 340.

yeccgoto_argument_list(10) -> 12;
yeccgoto_argument_list(29) -> 270;
yeccgoto_argument_list(59) -> 151;
yeccgoto_argument_list(161) -> 151;
yeccgoto_argument_list(419) -> 12;
yeccgoto_argument_list(425) -> 12.

yeccgoto_atomic(13) -> 43;
yeccgoto_atomic(20) -> 43;
yeccgoto_atomic(45) -> 43;
yeccgoto_atomic(50) -> 43;
yeccgoto_atomic(51) -> 43;
yeccgoto_atomic(53) -> 43;
yeccgoto_atomic(55) -> 43;
yeccgoto_atomic(56) -> 43;
yeccgoto_atomic(60) -> 43;
yeccgoto_atomic(64) -> 43;
yeccgoto_atomic(66) -> 43;
yeccgoto_atomic(68) -> 43;
yeccgoto_atomic(74) -> 43;
yeccgoto_atomic(75) -> 43;
yeccgoto_atomic(76) -> 43;
yeccgoto_atomic(80) -> 43;
yeccgoto_atomic(84) -> 43;
yeccgoto_atomic(87) -> 43;
yeccgoto_atomic(90) -> 43;
yeccgoto_atomic(97) -> 43;
yeccgoto_atomic(101) -> 43;
yeccgoto_atomic(107) -> 43;
yeccgoto_atomic(109) -> 43;
yeccgoto_atomic(117) -> 43;
yeccgoto_atomic(121) -> 43;
yeccgoto_atomic(127) -> 43;
yeccgoto_atomic(129) -> 43;
yeccgoto_atomic(134) -> 43;
yeccgoto_atomic(136) -> 43;
yeccgoto_atomic(138) -> 43;
yeccgoto_atomic(146) -> 43;
yeccgoto_atomic(166) -> 43;
yeccgoto_atomic(174) -> 43;
yeccgoto_atomic(176) -> 43;
yeccgoto_atomic(181) -> 43;
yeccgoto_atomic(188) -> 43;
yeccgoto_atomic(191) -> 43;
yeccgoto_atomic(195) -> 43;
yeccgoto_atomic(220) -> 43;
yeccgoto_atomic(222) -> 43;
yeccgoto_atomic(228) -> 43;
yeccgoto_atomic(230) -> 43;
yeccgoto_atomic(231) -> 43;
yeccgoto_atomic(234) -> 43;
yeccgoto_atomic(236) -> 43;
yeccgoto_atomic(238) -> 43;
yeccgoto_atomic(248) -> 43;
yeccgoto_atomic(249) -> 43;
yeccgoto_atomic(261) -> 43;
yeccgoto_atomic(272) -> 43;
yeccgoto_atomic(285) -> 43;
yeccgoto_atomic(288) -> 43;
yeccgoto_atomic(294) -> 43;
yeccgoto_atomic(402) -> 43;
yeccgoto_atomic(406) -> 43;
yeccgoto_atomic(410) -> 43.

yeccgoto_attr_val(288) -> 292.

yeccgoto_attribute(0) -> 8.

yeccgoto_bin_base_type(306) -> 358.

yeccgoto_bin_element(50) -> 186;
yeccgoto_bin_element(188) -> 186.

yeccgoto_bin_elements(50) -> 185;
yeccgoto_bin_elements(188) -> 189.

yeccgoto_bin_unit_type(306) -> 357;
yeccgoto_bin_unit_type(366) -> 368.

yeccgoto_binary(13) -> 42;
yeccgoto_binary(20) -> 42;
yeccgoto_binary(45) -> 42;
yeccgoto_binary(50) -> 184;
yeccgoto_binary(51) -> 42;
yeccgoto_binary(53) -> 42;
yeccgoto_binary(55) -> 42;
yeccgoto_binary(56) -> 42;
yeccgoto_binary(60) -> 42;
yeccgoto_binary(64) -> 42;
yeccgoto_binary(66) -> 42;
yeccgoto_binary(68) -> 42;
yeccgoto_binary(74) -> 42;
yeccgoto_binary(75) -> 42;
yeccgoto_binary(76) -> 42;
yeccgoto_binary(80) -> 42;
yeccgoto_binary(84) -> 42;
yeccgoto_binary(87) -> 42;
yeccgoto_binary(90) -> 42;
yeccgoto_binary(97) -> 42;
yeccgoto_binary(101) -> 42;
yeccgoto_binary(107) -> 42;
yeccgoto_binary(109) -> 42;
yeccgoto_binary(117) -> 42;
yeccgoto_binary(121) -> 42;
yeccgoto_binary(127) -> 42;
yeccgoto_binary(129) -> 133;
yeccgoto_binary(134) -> 42;
yeccgoto_binary(136) -> 42;
yeccgoto_binary(138) -> 133;
yeccgoto_binary(146) -> 42;
yeccgoto_binary(166) -> 42;
yeccgoto_binary(174) -> 42;
yeccgoto_binary(176) -> 42;
yeccgoto_binary(181) -> 42;
yeccgoto_binary(188) -> 42;
yeccgoto_binary(191) -> 133;
yeccgoto_binary(195) -> 42;
yeccgoto_binary(220) -> 42;
yeccgoto_binary(222) -> 42;
yeccgoto_binary(228) -> 42;
yeccgoto_binary(230) -> 42;
yeccgoto_binary(231) -> 42;
yeccgoto_binary(234) -> 42;
yeccgoto_binary(236) -> 42;
yeccgoto_binary(238) -> 42;
yeccgoto_binary(248) -> 42;
yeccgoto_binary(249) -> 42;
yeccgoto_binary(261) -> 42;
yeccgoto_binary(272) -> 42;
yeccgoto_binary(285) -> 133;
yeccgoto_binary(288) -> 42;
yeccgoto_binary(294) -> 42;
yeccgoto_binary(402) -> 42;
yeccgoto_binary(406) -> 42;
yeccgoto_binary(410) -> 42.

yeccgoto_binary_comprehension(13) -> 41;
yeccgoto_binary_comprehension(20) -> 41;
yeccgoto_binary_comprehension(45) -> 41;
yeccgoto_binary_comprehension(50) -> 41;
yeccgoto_binary_comprehension(51) -> 41;
yeccgoto_binary_comprehension(53) -> 41;
yeccgoto_binary_comprehension(55) -> 41;
yeccgoto_binary_comprehension(56) -> 41;
yeccgoto_binary_comprehension(60) -> 41;
yeccgoto_binary_comprehension(64) -> 41;
yeccgoto_binary_comprehension(66) -> 41;
yeccgoto_binary_comprehension(68) -> 41;
yeccgoto_binary_comprehension(74) -> 41;
yeccgoto_binary_comprehension(75) -> 41;
yeccgoto_binary_comprehension(76) -> 41;
yeccgoto_binary_comprehension(80) -> 41;
yeccgoto_binary_comprehension(84) -> 41;
yeccgoto_binary_comprehension(87) -> 41;
yeccgoto_binary_comprehension(90) -> 41;
yeccgoto_binary_comprehension(97) -> 41;
yeccgoto_binary_comprehension(101) -> 41;
yeccgoto_binary_comprehension(107) -> 41;
yeccgoto_binary_comprehension(109) -> 41;
yeccgoto_binary_comprehension(117) -> 41;
yeccgoto_binary_comprehension(121) -> 41;
yeccgoto_binary_comprehension(127) -> 41;
yeccgoto_binary_comprehension(129) -> 41;
yeccgoto_binary_comprehension(134) -> 41;
yeccgoto_binary_comprehension(136) -> 41;
yeccgoto_binary_comprehension(138) -> 41;
yeccgoto_binary_comprehension(146) -> 41;
yeccgoto_binary_comprehension(166) -> 41;
yeccgoto_binary_comprehension(174) -> 41;
yeccgoto_binary_comprehension(176) -> 41;
yeccgoto_binary_comprehension(181) -> 41;
yeccgoto_binary_comprehension(188) -> 41;
yeccgoto_binary_comprehension(191) -> 41;
yeccgoto_binary_comprehension(195) -> 41;
yeccgoto_binary_comprehension(220) -> 41;
yeccgoto_binary_comprehension(222) -> 41;
yeccgoto_binary_comprehension(228) -> 41;
yeccgoto_binary_comprehension(230) -> 41;
yeccgoto_binary_comprehension(231) -> 41;
yeccgoto_binary_comprehension(234) -> 41;
yeccgoto_binary_comprehension(236) -> 41;
yeccgoto_binary_comprehension(238) -> 41;
yeccgoto_binary_comprehension(248) -> 41;
yeccgoto_binary_comprehension(249) -> 41;
yeccgoto_binary_comprehension(261) -> 41;
yeccgoto_binary_comprehension(272) -> 41;
yeccgoto_binary_comprehension(285) -> 41;
yeccgoto_binary_comprehension(288) -> 41;
yeccgoto_binary_comprehension(294) -> 41;
yeccgoto_binary_comprehension(402) -> 41;
yeccgoto_binary_comprehension(406) -> 41;
yeccgoto_binary_comprehension(410) -> 41.

yeccgoto_binary_type(295) -> 302;
yeccgoto_binary_type(304) -> 302;
yeccgoto_binary_type(307) -> 302;
yeccgoto_binary_type(312) -> 302;
yeccgoto_binary_type(316) -> 302;
yeccgoto_binary_type(322) -> 302;
yeccgoto_binary_type(330) -> 302;
yeccgoto_binary_type(335) -> 302;
yeccgoto_binary_type(337) -> 302;
yeccgoto_binary_type(339) -> 302;
yeccgoto_binary_type(342) -> 302;
yeccgoto_binary_type(345) -> 302;
yeccgoto_binary_type(380) -> 302;
yeccgoto_binary_type(389) -> 302;
yeccgoto_binary_type(397) -> 302;
yeccgoto_binary_type(407) -> 302.

yeccgoto_bit_expr(50) -> 183;
yeccgoto_bit_expr(188) -> 183.

yeccgoto_bit_size_expr(195) -> 197.

yeccgoto_bit_type(199) -> 201;
yeccgoto_bit_type(205) -> 201.

yeccgoto_bit_type_list(199) -> 200;
yeccgoto_bit_type_list(205) -> 206.

yeccgoto_case_expr(13) -> 40;
yeccgoto_case_expr(20) -> 40;
yeccgoto_case_expr(45) -> 40;
yeccgoto_case_expr(50) -> 40;
yeccgoto_case_expr(51) -> 40;
yeccgoto_case_expr(53) -> 40;
yeccgoto_case_expr(55) -> 40;
yeccgoto_case_expr(56) -> 40;
yeccgoto_case_expr(60) -> 40;
yeccgoto_case_expr(64) -> 40;
yeccgoto_case_expr(66) -> 40;
yeccgoto_case_expr(68) -> 40;
yeccgoto_case_expr(74) -> 40;
yeccgoto_case_expr(75) -> 40;
yeccgoto_case_expr(76) -> 40;
yeccgoto_case_expr(80) -> 40;
yeccgoto_case_expr(84) -> 40;
yeccgoto_case_expr(87) -> 40;
yeccgoto_case_expr(90) -> 40;
yeccgoto_case_expr(97) -> 40;
yeccgoto_case_expr(101) -> 40;
yeccgoto_case_expr(107) -> 40;
yeccgoto_case_expr(109) -> 40;
yeccgoto_case_expr(117) -> 40;
yeccgoto_case_expr(121) -> 40;
yeccgoto_case_expr(127) -> 40;
yeccgoto_case_expr(129) -> 40;
yeccgoto_case_expr(134) -> 40;
yeccgoto_case_expr(136) -> 40;
yeccgoto_case_expr(138) -> 40;
yeccgoto_case_expr(146) -> 40;
yeccgoto_case_expr(166) -> 40;
yeccgoto_case_expr(174) -> 40;
yeccgoto_case_expr(176) -> 40;
yeccgoto_case_expr(181) -> 40;
yeccgoto_case_expr(188) -> 40;
yeccgoto_case_expr(191) -> 40;
yeccgoto_case_expr(195) -> 40;
yeccgoto_case_expr(220) -> 40;
yeccgoto_case_expr(222) -> 40;
yeccgoto_case_expr(228) -> 40;
yeccgoto_case_expr(230) -> 40;
yeccgoto_case_expr(231) -> 40;
yeccgoto_case_expr(234) -> 40;
yeccgoto_case_expr(236) -> 40;
yeccgoto_case_expr(238) -> 40;
yeccgoto_case_expr(248) -> 40;
yeccgoto_case_expr(249) -> 40;
yeccgoto_case_expr(261) -> 40;
yeccgoto_case_expr(272) -> 40;
yeccgoto_case_expr(285) -> 40;
yeccgoto_case_expr(288) -> 40;
yeccgoto_case_expr(294) -> 40;
yeccgoto_case_expr(402) -> 40;
yeccgoto_case_expr(406) -> 40;
yeccgoto_case_expr(410) -> 40.

yeccgoto_clause_args(10) -> 11;
yeccgoto_clause_args(419) -> 420;
yeccgoto_clause_args(425) -> 426.

yeccgoto_clause_body(83) -> 89;
yeccgoto_clause_body(99) -> 100;
yeccgoto_clause_body(103) -> 104;
yeccgoto_clause_body(105) -> 106;
yeccgoto_clause_body(118) -> 119;
yeccgoto_clause_body(123) -> 124;
yeccgoto_clause_body(144) -> 145;
yeccgoto_clause_body(159) -> 160;
yeccgoto_clause_body(282) -> 284;
yeccgoto_clause_body(421) -> 284.

yeccgoto_clause_guard(11) -> 282;
yeccgoto_clause_guard(77) -> 83;
yeccgoto_clause_guard(94) -> 105;
yeccgoto_clause_guard(98) -> 99;
yeccgoto_clause_guard(102) -> 103;
yeccgoto_clause_guard(151) -> 159;
yeccgoto_clause_guard(420) -> 421;
yeccgoto_clause_guard(426) -> 427.

yeccgoto_comp_op(34) -> 238.

yeccgoto_cr_clause(64) -> 79;
yeccgoto_cr_clause(76) -> 79;
yeccgoto_cr_clause(80) -> 79;
yeccgoto_cr_clause(166) -> 79.

yeccgoto_cr_clauses(64) -> 116;
yeccgoto_cr_clauses(76) -> 78;
yeccgoto_cr_clauses(80) -> 81;
yeccgoto_cr_clauses(166) -> 167.

yeccgoto_expr(13) -> 39;
yeccgoto_expr(45) -> 209;
yeccgoto_expr(51) -> 171;
yeccgoto_expr(53) -> 39;
yeccgoto_expr(55) -> 165;
yeccgoto_expr(56) -> 164;
yeccgoto_expr(60) -> 39;
yeccgoto_expr(64) -> 77;
yeccgoto_expr(66) -> 39;
yeccgoto_expr(68) -> 39;
yeccgoto_expr(74) -> 39;
yeccgoto_expr(75) -> 94;
yeccgoto_expr(76) -> 77;
yeccgoto_expr(80) -> 77;
yeccgoto_expr(84) -> 39;
yeccgoto_expr(87) -> 39;
yeccgoto_expr(90) -> 39;
yeccgoto_expr(97) -> 98;
yeccgoto_expr(101) -> 102;
yeccgoto_expr(107) -> 94;
yeccgoto_expr(109) -> 39;
yeccgoto_expr(117) -> 118;
yeccgoto_expr(121) -> 123;
yeccgoto_expr(127) -> 128;
yeccgoto_expr(129) -> 132;
yeccgoto_expr(134) -> 135;
yeccgoto_expr(136) -> 137;
yeccgoto_expr(138) -> 132;
yeccgoto_expr(146) -> 39;
yeccgoto_expr(166) -> 77;
yeccgoto_expr(174) -> 179;
yeccgoto_expr(176) -> 177;
yeccgoto_expr(191) -> 132;
yeccgoto_expr(220) -> 221;
yeccgoto_expr(222) -> 223;
yeccgoto_expr(228) -> 39;
yeccgoto_expr(285) -> 132;
yeccgoto_expr(288) -> 291;
yeccgoto_expr(294) -> 39;
yeccgoto_expr(402) -> 405;
yeccgoto_expr(406) -> 405;
yeccgoto_expr(410) -> 405.

yeccgoto_expr_100(13) -> 38;
yeccgoto_expr_100(45) -> 38;
yeccgoto_expr_100(51) -> 38;
yeccgoto_expr_100(53) -> 38;
yeccgoto_expr_100(55) -> 38;
yeccgoto_expr_100(56) -> 38;
yeccgoto_expr_100(60) -> 38;
yeccgoto_expr_100(64) -> 38;
yeccgoto_expr_100(66) -> 38;
yeccgoto_expr_100(68) -> 38;
yeccgoto_expr_100(74) -> 38;
yeccgoto_expr_100(75) -> 38;
yeccgoto_expr_100(76) -> 38;
yeccgoto_expr_100(80) -> 38;
yeccgoto_expr_100(84) -> 38;
yeccgoto_expr_100(87) -> 38;
yeccgoto_expr_100(90) -> 38;
yeccgoto_expr_100(97) -> 38;
yeccgoto_expr_100(101) -> 38;
yeccgoto_expr_100(107) -> 38;
yeccgoto_expr_100(109) -> 38;
yeccgoto_expr_100(117) -> 38;
yeccgoto_expr_100(121) -> 38;
yeccgoto_expr_100(127) -> 38;
yeccgoto_expr_100(129) -> 38;
yeccgoto_expr_100(134) -> 38;
yeccgoto_expr_100(136) -> 38;
yeccgoto_expr_100(138) -> 38;
yeccgoto_expr_100(146) -> 38;
yeccgoto_expr_100(166) -> 38;
yeccgoto_expr_100(174) -> 38;
yeccgoto_expr_100(176) -> 38;
yeccgoto_expr_100(191) -> 38;
yeccgoto_expr_100(220) -> 38;
yeccgoto_expr_100(222) -> 38;
yeccgoto_expr_100(228) -> 38;
yeccgoto_expr_100(230) -> 233;
yeccgoto_expr_100(231) -> 232;
yeccgoto_expr_100(285) -> 38;
yeccgoto_expr_100(288) -> 38;
yeccgoto_expr_100(294) -> 38;
yeccgoto_expr_100(402) -> 38;
yeccgoto_expr_100(406) -> 38;
yeccgoto_expr_100(410) -> 38.

yeccgoto_expr_150(13) -> 37;
yeccgoto_expr_150(45) -> 37;
yeccgoto_expr_150(51) -> 37;
yeccgoto_expr_150(53) -> 37;
yeccgoto_expr_150(55) -> 37;
yeccgoto_expr_150(56) -> 37;
yeccgoto_expr_150(60) -> 37;
yeccgoto_expr_150(64) -> 37;
yeccgoto_expr_150(66) -> 37;
yeccgoto_expr_150(68) -> 37;
yeccgoto_expr_150(74) -> 37;
yeccgoto_expr_150(75) -> 37;
yeccgoto_expr_150(76) -> 37;
yeccgoto_expr_150(80) -> 37;
yeccgoto_expr_150(84) -> 37;
yeccgoto_expr_150(87) -> 37;
yeccgoto_expr_150(90) -> 37;
yeccgoto_expr_150(97) -> 37;
yeccgoto_expr_150(101) -> 37;
yeccgoto_expr_150(107) -> 37;
yeccgoto_expr_150(109) -> 37;
yeccgoto_expr_150(117) -> 37;
yeccgoto_expr_150(121) -> 37;
yeccgoto_expr_150(127) -> 37;
yeccgoto_expr_150(129) -> 37;
yeccgoto_expr_150(134) -> 37;
yeccgoto_expr_150(136) -> 37;
yeccgoto_expr_150(138) -> 37;
yeccgoto_expr_150(146) -> 37;
yeccgoto_expr_150(166) -> 37;
yeccgoto_expr_150(174) -> 37;
yeccgoto_expr_150(176) -> 37;
yeccgoto_expr_150(191) -> 37;
yeccgoto_expr_150(220) -> 37;
yeccgoto_expr_150(222) -> 37;
yeccgoto_expr_150(228) -> 37;
yeccgoto_expr_150(230) -> 37;
yeccgoto_expr_150(231) -> 37;
yeccgoto_expr_150(234) -> 235;
yeccgoto_expr_150(285) -> 37;
yeccgoto_expr_150(288) -> 37;
yeccgoto_expr_150(294) -> 37;
yeccgoto_expr_150(402) -> 37;
yeccgoto_expr_150(406) -> 37;
yeccgoto_expr_150(410) -> 37.

yeccgoto_expr_160(13) -> 36;
yeccgoto_expr_160(45) -> 36;
yeccgoto_expr_160(51) -> 36;
yeccgoto_expr_160(53) -> 36;
yeccgoto_expr_160(55) -> 36;
yeccgoto_expr_160(56) -> 36;
yeccgoto_expr_160(60) -> 36;
yeccgoto_expr_160(64) -> 36;
yeccgoto_expr_160(66) -> 36;
yeccgoto_expr_160(68) -> 36;
yeccgoto_expr_160(74) -> 36;
yeccgoto_expr_160(75) -> 36;
yeccgoto_expr_160(76) -> 36;
yeccgoto_expr_160(80) -> 36;
yeccgoto_expr_160(84) -> 36;
yeccgoto_expr_160(87) -> 36;
yeccgoto_expr_160(90) -> 36;
yeccgoto_expr_160(97) -> 36;
yeccgoto_expr_160(101) -> 36;
yeccgoto_expr_160(107) -> 36;
yeccgoto_expr_160(109) -> 36;
yeccgoto_expr_160(117) -> 36;
yeccgoto_expr_160(121) -> 36;
yeccgoto_expr_160(127) -> 36;
yeccgoto_expr_160(129) -> 36;
yeccgoto_expr_160(134) -> 36;
yeccgoto_expr_160(136) -> 36;
yeccgoto_expr_160(138) -> 36;
yeccgoto_expr_160(146) -> 36;
yeccgoto_expr_160(166) -> 36;
yeccgoto_expr_160(174) -> 36;
yeccgoto_expr_160(176) -> 36;
yeccgoto_expr_160(191) -> 36;
yeccgoto_expr_160(220) -> 36;
yeccgoto_expr_160(222) -> 36;
yeccgoto_expr_160(228) -> 36;
yeccgoto_expr_160(230) -> 36;
yeccgoto_expr_160(231) -> 36;
yeccgoto_expr_160(234) -> 36;
yeccgoto_expr_160(236) -> 237;
yeccgoto_expr_160(285) -> 36;
yeccgoto_expr_160(288) -> 36;
yeccgoto_expr_160(294) -> 36;
yeccgoto_expr_160(402) -> 36;
yeccgoto_expr_160(406) -> 36;
yeccgoto_expr_160(410) -> 36.

yeccgoto_expr_200(13) -> 35;
yeccgoto_expr_200(45) -> 35;
yeccgoto_expr_200(51) -> 35;
yeccgoto_expr_200(53) -> 35;
yeccgoto_expr_200(55) -> 35;
yeccgoto_expr_200(56) -> 35;
yeccgoto_expr_200(60) -> 35;
yeccgoto_expr_200(64) -> 35;
yeccgoto_expr_200(66) -> 35;
yeccgoto_expr_200(68) -> 35;
yeccgoto_expr_200(74) -> 35;
yeccgoto_expr_200(75) -> 35;
yeccgoto_expr_200(76) -> 35;
yeccgoto_expr_200(80) -> 35;
yeccgoto_expr_200(84) -> 35;
yeccgoto_expr_200(87) -> 35;
yeccgoto_expr_200(90) -> 35;
yeccgoto_expr_200(97) -> 35;
yeccgoto_expr_200(101) -> 35;
yeccgoto_expr_200(107) -> 35;
yeccgoto_expr_200(109) -> 35;
yeccgoto_expr_200(117) -> 35;
yeccgoto_expr_200(121) -> 35;
yeccgoto_expr_200(127) -> 35;
yeccgoto_expr_200(129) -> 35;
yeccgoto_expr_200(134) -> 35;
yeccgoto_expr_200(136) -> 35;
yeccgoto_expr_200(138) -> 35;
yeccgoto_expr_200(146) -> 35;
yeccgoto_expr_200(166) -> 35;
yeccgoto_expr_200(174) -> 35;
yeccgoto_expr_200(176) -> 35;
yeccgoto_expr_200(191) -> 35;
yeccgoto_expr_200(220) -> 35;
yeccgoto_expr_200(222) -> 35;
yeccgoto_expr_200(228) -> 35;
yeccgoto_expr_200(230) -> 35;
yeccgoto_expr_200(231) -> 35;
yeccgoto_expr_200(234) -> 35;
yeccgoto_expr_200(236) -> 35;
yeccgoto_expr_200(285) -> 35;
yeccgoto_expr_200(288) -> 35;
yeccgoto_expr_200(294) -> 35;
yeccgoto_expr_200(402) -> 35;
yeccgoto_expr_200(406) -> 35;
yeccgoto_expr_200(410) -> 35.

yeccgoto_expr_300(13) -> 34;
yeccgoto_expr_300(45) -> 34;
yeccgoto_expr_300(51) -> 34;
yeccgoto_expr_300(53) -> 34;
yeccgoto_expr_300(55) -> 34;
yeccgoto_expr_300(56) -> 34;
yeccgoto_expr_300(60) -> 34;
yeccgoto_expr_300(64) -> 34;
yeccgoto_expr_300(66) -> 34;
yeccgoto_expr_300(68) -> 34;
yeccgoto_expr_300(74) -> 34;
yeccgoto_expr_300(75) -> 34;
yeccgoto_expr_300(76) -> 34;
yeccgoto_expr_300(80) -> 34;
yeccgoto_expr_300(84) -> 34;
yeccgoto_expr_300(87) -> 34;
yeccgoto_expr_300(90) -> 34;
yeccgoto_expr_300(97) -> 34;
yeccgoto_expr_300(101) -> 34;
yeccgoto_expr_300(107) -> 34;
yeccgoto_expr_300(109) -> 34;
yeccgoto_expr_300(117) -> 34;
yeccgoto_expr_300(121) -> 34;
yeccgoto_expr_300(127) -> 34;
yeccgoto_expr_300(129) -> 34;
yeccgoto_expr_300(134) -> 34;
yeccgoto_expr_300(136) -> 34;
yeccgoto_expr_300(138) -> 34;
yeccgoto_expr_300(146) -> 34;
yeccgoto_expr_300(166) -> 34;
yeccgoto_expr_300(174) -> 34;
yeccgoto_expr_300(176) -> 34;
yeccgoto_expr_300(191) -> 34;
yeccgoto_expr_300(220) -> 34;
yeccgoto_expr_300(222) -> 34;
yeccgoto_expr_300(228) -> 34;
yeccgoto_expr_300(230) -> 34;
yeccgoto_expr_300(231) -> 34;
yeccgoto_expr_300(234) -> 34;
yeccgoto_expr_300(236) -> 34;
yeccgoto_expr_300(238) -> 247;
yeccgoto_expr_300(248) -> 269;
yeccgoto_expr_300(285) -> 34;
yeccgoto_expr_300(288) -> 34;
yeccgoto_expr_300(294) -> 34;
yeccgoto_expr_300(402) -> 34;
yeccgoto_expr_300(406) -> 34;
yeccgoto_expr_300(410) -> 34.

yeccgoto_expr_400(13) -> 33;
yeccgoto_expr_400(45) -> 33;
yeccgoto_expr_400(51) -> 33;
yeccgoto_expr_400(53) -> 33;
yeccgoto_expr_400(55) -> 33;
yeccgoto_expr_400(56) -> 33;
yeccgoto_expr_400(60) -> 33;
yeccgoto_expr_400(64) -> 33;
yeccgoto_expr_400(66) -> 33;
yeccgoto_expr_400(68) -> 33;
yeccgoto_expr_400(74) -> 33;
yeccgoto_expr_400(75) -> 33;
yeccgoto_expr_400(76) -> 33;
yeccgoto_expr_400(80) -> 33;
yeccgoto_expr_400(84) -> 33;
yeccgoto_expr_400(87) -> 33;
yeccgoto_expr_400(90) -> 33;
yeccgoto_expr_400(97) -> 33;
yeccgoto_expr_400(101) -> 33;
yeccgoto_expr_400(107) -> 33;
yeccgoto_expr_400(109) -> 33;
yeccgoto_expr_400(117) -> 33;
yeccgoto_expr_400(121) -> 33;
yeccgoto_expr_400(127) -> 33;
yeccgoto_expr_400(129) -> 33;
yeccgoto_expr_400(134) -> 33;
yeccgoto_expr_400(136) -> 33;
yeccgoto_expr_400(138) -> 33;
yeccgoto_expr_400(146) -> 33;
yeccgoto_expr_400(166) -> 33;
yeccgoto_expr_400(174) -> 33;
yeccgoto_expr_400(176) -> 33;
yeccgoto_expr_400(191) -> 33;
yeccgoto_expr_400(220) -> 33;
yeccgoto_expr_400(222) -> 33;
yeccgoto_expr_400(228) -> 33;
yeccgoto_expr_400(230) -> 33;
yeccgoto_expr_400(231) -> 33;
yeccgoto_expr_400(234) -> 33;
yeccgoto_expr_400(236) -> 33;
yeccgoto_expr_400(238) -> 33;
yeccgoto_expr_400(248) -> 33;
yeccgoto_expr_400(285) -> 33;
yeccgoto_expr_400(288) -> 33;
yeccgoto_expr_400(294) -> 33;
yeccgoto_expr_400(402) -> 33;
yeccgoto_expr_400(406) -> 33;
yeccgoto_expr_400(410) -> 33.

yeccgoto_expr_500(13) -> 32;
yeccgoto_expr_500(45) -> 32;
yeccgoto_expr_500(51) -> 32;
yeccgoto_expr_500(53) -> 32;
yeccgoto_expr_500(55) -> 32;
yeccgoto_expr_500(56) -> 32;
yeccgoto_expr_500(60) -> 32;
yeccgoto_expr_500(64) -> 32;
yeccgoto_expr_500(66) -> 32;
yeccgoto_expr_500(68) -> 32;
yeccgoto_expr_500(74) -> 32;
yeccgoto_expr_500(75) -> 32;
yeccgoto_expr_500(76) -> 32;
yeccgoto_expr_500(80) -> 32;
yeccgoto_expr_500(84) -> 32;
yeccgoto_expr_500(87) -> 32;
yeccgoto_expr_500(90) -> 32;
yeccgoto_expr_500(97) -> 32;
yeccgoto_expr_500(101) -> 32;
yeccgoto_expr_500(107) -> 32;
yeccgoto_expr_500(109) -> 32;
yeccgoto_expr_500(117) -> 32;
yeccgoto_expr_500(121) -> 32;
yeccgoto_expr_500(127) -> 32;
yeccgoto_expr_500(129) -> 32;
yeccgoto_expr_500(134) -> 32;
yeccgoto_expr_500(136) -> 32;
yeccgoto_expr_500(138) -> 32;
yeccgoto_expr_500(146) -> 32;
yeccgoto_expr_500(166) -> 32;
yeccgoto_expr_500(174) -> 32;
yeccgoto_expr_500(176) -> 32;
yeccgoto_expr_500(191) -> 32;
yeccgoto_expr_500(220) -> 32;
yeccgoto_expr_500(222) -> 32;
yeccgoto_expr_500(228) -> 32;
yeccgoto_expr_500(230) -> 32;
yeccgoto_expr_500(231) -> 32;
yeccgoto_expr_500(234) -> 32;
yeccgoto_expr_500(236) -> 32;
yeccgoto_expr_500(238) -> 32;
yeccgoto_expr_500(248) -> 32;
yeccgoto_expr_500(249) -> 260;
yeccgoto_expr_500(285) -> 32;
yeccgoto_expr_500(288) -> 32;
yeccgoto_expr_500(294) -> 32;
yeccgoto_expr_500(402) -> 32;
yeccgoto_expr_500(406) -> 32;
yeccgoto_expr_500(410) -> 32.

yeccgoto_expr_600(13) -> 31;
yeccgoto_expr_600(45) -> 31;
yeccgoto_expr_600(51) -> 31;
yeccgoto_expr_600(53) -> 31;
yeccgoto_expr_600(55) -> 31;
yeccgoto_expr_600(56) -> 31;
yeccgoto_expr_600(60) -> 31;
yeccgoto_expr_600(64) -> 31;
yeccgoto_expr_600(66) -> 31;
yeccgoto_expr_600(68) -> 31;
yeccgoto_expr_600(74) -> 31;
yeccgoto_expr_600(75) -> 31;
yeccgoto_expr_600(76) -> 31;
yeccgoto_expr_600(80) -> 31;
yeccgoto_expr_600(84) -> 31;
yeccgoto_expr_600(87) -> 31;
yeccgoto_expr_600(90) -> 31;
yeccgoto_expr_600(97) -> 31;
yeccgoto_expr_600(101) -> 31;
yeccgoto_expr_600(107) -> 31;
yeccgoto_expr_600(109) -> 31;
yeccgoto_expr_600(117) -> 31;
yeccgoto_expr_600(121) -> 31;
yeccgoto_expr_600(127) -> 31;
yeccgoto_expr_600(129) -> 31;
yeccgoto_expr_600(134) -> 31;
yeccgoto_expr_600(136) -> 31;
yeccgoto_expr_600(138) -> 31;
yeccgoto_expr_600(146) -> 31;
yeccgoto_expr_600(166) -> 31;
yeccgoto_expr_600(174) -> 31;
yeccgoto_expr_600(176) -> 31;
yeccgoto_expr_600(191) -> 31;
yeccgoto_expr_600(220) -> 31;
yeccgoto_expr_600(222) -> 31;
yeccgoto_expr_600(228) -> 31;
yeccgoto_expr_600(230) -> 31;
yeccgoto_expr_600(231) -> 31;
yeccgoto_expr_600(234) -> 31;
yeccgoto_expr_600(236) -> 31;
yeccgoto_expr_600(238) -> 31;
yeccgoto_expr_600(248) -> 31;
yeccgoto_expr_600(249) -> 31;
yeccgoto_expr_600(261) -> 268;
yeccgoto_expr_600(285) -> 31;
yeccgoto_expr_600(288) -> 31;
yeccgoto_expr_600(294) -> 31;
yeccgoto_expr_600(402) -> 31;
yeccgoto_expr_600(406) -> 31;
yeccgoto_expr_600(410) -> 31.

yeccgoto_expr_700(13) -> 30;
yeccgoto_expr_700(20) -> 281;
yeccgoto_expr_700(45) -> 30;
yeccgoto_expr_700(51) -> 30;
yeccgoto_expr_700(53) -> 30;
yeccgoto_expr_700(55) -> 30;
yeccgoto_expr_700(56) -> 30;
yeccgoto_expr_700(60) -> 30;
yeccgoto_expr_700(64) -> 30;
yeccgoto_expr_700(66) -> 30;
yeccgoto_expr_700(68) -> 30;
yeccgoto_expr_700(74) -> 30;
yeccgoto_expr_700(75) -> 30;
yeccgoto_expr_700(76) -> 30;
yeccgoto_expr_700(80) -> 30;
yeccgoto_expr_700(84) -> 30;
yeccgoto_expr_700(87) -> 30;
yeccgoto_expr_700(90) -> 30;
yeccgoto_expr_700(97) -> 30;
yeccgoto_expr_700(101) -> 30;
yeccgoto_expr_700(107) -> 30;
yeccgoto_expr_700(109) -> 30;
yeccgoto_expr_700(117) -> 30;
yeccgoto_expr_700(121) -> 30;
yeccgoto_expr_700(127) -> 30;
yeccgoto_expr_700(129) -> 30;
yeccgoto_expr_700(134) -> 30;
yeccgoto_expr_700(136) -> 30;
yeccgoto_expr_700(138) -> 30;
yeccgoto_expr_700(146) -> 30;
yeccgoto_expr_700(166) -> 30;
yeccgoto_expr_700(174) -> 30;
yeccgoto_expr_700(176) -> 30;
yeccgoto_expr_700(191) -> 30;
yeccgoto_expr_700(220) -> 30;
yeccgoto_expr_700(222) -> 30;
yeccgoto_expr_700(228) -> 30;
yeccgoto_expr_700(230) -> 30;
yeccgoto_expr_700(231) -> 30;
yeccgoto_expr_700(234) -> 30;
yeccgoto_expr_700(236) -> 30;
yeccgoto_expr_700(238) -> 30;
yeccgoto_expr_700(248) -> 30;
yeccgoto_expr_700(249) -> 30;
yeccgoto_expr_700(261) -> 30;
yeccgoto_expr_700(285) -> 30;
yeccgoto_expr_700(288) -> 30;
yeccgoto_expr_700(294) -> 30;
yeccgoto_expr_700(402) -> 30;
yeccgoto_expr_700(406) -> 30;
yeccgoto_expr_700(410) -> 30.

yeccgoto_expr_800(13) -> 29;
yeccgoto_expr_800(20) -> 29;
yeccgoto_expr_800(45) -> 29;
yeccgoto_expr_800(51) -> 29;
yeccgoto_expr_800(53) -> 29;
yeccgoto_expr_800(55) -> 29;
yeccgoto_expr_800(56) -> 29;
yeccgoto_expr_800(60) -> 29;
yeccgoto_expr_800(64) -> 29;
yeccgoto_expr_800(66) -> 29;
yeccgoto_expr_800(68) -> 29;
yeccgoto_expr_800(74) -> 29;
yeccgoto_expr_800(75) -> 29;
yeccgoto_expr_800(76) -> 29;
yeccgoto_expr_800(80) -> 29;
yeccgoto_expr_800(84) -> 29;
yeccgoto_expr_800(87) -> 29;
yeccgoto_expr_800(90) -> 29;
yeccgoto_expr_800(97) -> 29;
yeccgoto_expr_800(101) -> 29;
yeccgoto_expr_800(107) -> 29;
yeccgoto_expr_800(109) -> 29;
yeccgoto_expr_800(117) -> 29;
yeccgoto_expr_800(121) -> 29;
yeccgoto_expr_800(127) -> 29;
yeccgoto_expr_800(129) -> 29;
yeccgoto_expr_800(134) -> 29;
yeccgoto_expr_800(136) -> 29;
yeccgoto_expr_800(138) -> 29;
yeccgoto_expr_800(146) -> 29;
yeccgoto_expr_800(166) -> 29;
yeccgoto_expr_800(174) -> 29;
yeccgoto_expr_800(176) -> 29;
yeccgoto_expr_800(191) -> 29;
yeccgoto_expr_800(220) -> 29;
yeccgoto_expr_800(222) -> 29;
yeccgoto_expr_800(228) -> 29;
yeccgoto_expr_800(230) -> 29;
yeccgoto_expr_800(231) -> 29;
yeccgoto_expr_800(234) -> 29;
yeccgoto_expr_800(236) -> 29;
yeccgoto_expr_800(238) -> 29;
yeccgoto_expr_800(248) -> 29;
yeccgoto_expr_800(249) -> 29;
yeccgoto_expr_800(261) -> 29;
yeccgoto_expr_800(285) -> 29;
yeccgoto_expr_800(288) -> 29;
yeccgoto_expr_800(294) -> 29;
yeccgoto_expr_800(402) -> 29;
yeccgoto_expr_800(406) -> 29;
yeccgoto_expr_800(410) -> 29.

yeccgoto_expr_900(13) -> 28;
yeccgoto_expr_900(20) -> 28;
yeccgoto_expr_900(45) -> 28;
yeccgoto_expr_900(51) -> 28;
yeccgoto_expr_900(53) -> 28;
yeccgoto_expr_900(55) -> 28;
yeccgoto_expr_900(56) -> 28;
yeccgoto_expr_900(60) -> 28;
yeccgoto_expr_900(64) -> 28;
yeccgoto_expr_900(66) -> 28;
yeccgoto_expr_900(68) -> 28;
yeccgoto_expr_900(74) -> 28;
yeccgoto_expr_900(75) -> 28;
yeccgoto_expr_900(76) -> 28;
yeccgoto_expr_900(80) -> 28;
yeccgoto_expr_900(84) -> 28;
yeccgoto_expr_900(87) -> 28;
yeccgoto_expr_900(90) -> 28;
yeccgoto_expr_900(97) -> 28;
yeccgoto_expr_900(101) -> 28;
yeccgoto_expr_900(107) -> 28;
yeccgoto_expr_900(109) -> 28;
yeccgoto_expr_900(117) -> 28;
yeccgoto_expr_900(121) -> 28;
yeccgoto_expr_900(127) -> 28;
yeccgoto_expr_900(129) -> 28;
yeccgoto_expr_900(134) -> 28;
yeccgoto_expr_900(136) -> 28;
yeccgoto_expr_900(138) -> 28;
yeccgoto_expr_900(146) -> 28;
yeccgoto_expr_900(166) -> 28;
yeccgoto_expr_900(174) -> 28;
yeccgoto_expr_900(176) -> 28;
yeccgoto_expr_900(191) -> 28;
yeccgoto_expr_900(220) -> 28;
yeccgoto_expr_900(222) -> 28;
yeccgoto_expr_900(228) -> 28;
yeccgoto_expr_900(230) -> 28;
yeccgoto_expr_900(231) -> 28;
yeccgoto_expr_900(234) -> 28;
yeccgoto_expr_900(236) -> 28;
yeccgoto_expr_900(238) -> 28;
yeccgoto_expr_900(248) -> 28;
yeccgoto_expr_900(249) -> 28;
yeccgoto_expr_900(261) -> 28;
yeccgoto_expr_900(285) -> 28;
yeccgoto_expr_900(288) -> 28;
yeccgoto_expr_900(294) -> 28;
yeccgoto_expr_900(402) -> 28;
yeccgoto_expr_900(406) -> 28;
yeccgoto_expr_900(410) -> 28.

yeccgoto_expr_max(13) -> 27;
yeccgoto_expr_max(20) -> 27;
yeccgoto_expr_max(45) -> 27;
yeccgoto_expr_max(50) -> 182;
yeccgoto_expr_max(51) -> 27;
yeccgoto_expr_max(53) -> 27;
yeccgoto_expr_max(55) -> 27;
yeccgoto_expr_max(56) -> 27;
yeccgoto_expr_max(60) -> 27;
yeccgoto_expr_max(64) -> 27;
yeccgoto_expr_max(66) -> 27;
yeccgoto_expr_max(68) -> 27;
yeccgoto_expr_max(74) -> 27;
yeccgoto_expr_max(75) -> 27;
yeccgoto_expr_max(76) -> 27;
yeccgoto_expr_max(80) -> 27;
yeccgoto_expr_max(84) -> 27;
yeccgoto_expr_max(87) -> 27;
yeccgoto_expr_max(90) -> 27;
yeccgoto_expr_max(97) -> 27;
yeccgoto_expr_max(101) -> 27;
yeccgoto_expr_max(107) -> 27;
yeccgoto_expr_max(109) -> 27;
yeccgoto_expr_max(117) -> 27;
yeccgoto_expr_max(121) -> 27;
yeccgoto_expr_max(127) -> 27;
yeccgoto_expr_max(129) -> 27;
yeccgoto_expr_max(134) -> 27;
yeccgoto_expr_max(136) -> 27;
yeccgoto_expr_max(138) -> 27;
yeccgoto_expr_max(146) -> 27;
yeccgoto_expr_max(166) -> 27;
yeccgoto_expr_max(174) -> 27;
yeccgoto_expr_max(176) -> 27;
yeccgoto_expr_max(181) -> 207;
yeccgoto_expr_max(188) -> 182;
yeccgoto_expr_max(191) -> 27;
yeccgoto_expr_max(195) -> 196;
yeccgoto_expr_max(220) -> 27;
yeccgoto_expr_max(222) -> 27;
yeccgoto_expr_max(228) -> 27;
yeccgoto_expr_max(230) -> 27;
yeccgoto_expr_max(231) -> 27;
yeccgoto_expr_max(234) -> 27;
yeccgoto_expr_max(236) -> 27;
yeccgoto_expr_max(238) -> 27;
yeccgoto_expr_max(248) -> 27;
yeccgoto_expr_max(249) -> 27;
yeccgoto_expr_max(261) -> 27;
yeccgoto_expr_max(272) -> 273;
yeccgoto_expr_max(285) -> 27;
yeccgoto_expr_max(288) -> 27;
yeccgoto_expr_max(294) -> 27;
yeccgoto_expr_max(402) -> 27;
yeccgoto_expr_max(406) -> 27;
yeccgoto_expr_max(410) -> 27.

yeccgoto_exprs(13) -> 26;
yeccgoto_exprs(53) -> 169;
yeccgoto_exprs(60) -> 86;
yeccgoto_exprs(66) -> 72;
yeccgoto_exprs(68) -> 69;
yeccgoto_exprs(74) -> 113;
yeccgoto_exprs(84) -> 86;
yeccgoto_exprs(87) -> 86;
yeccgoto_exprs(90) -> 91;
yeccgoto_exprs(109) -> 111;
yeccgoto_exprs(146) -> 86;
yeccgoto_exprs(228) -> 229;
yeccgoto_exprs(288) -> 290;
yeccgoto_exprs(294) -> 229;
yeccgoto_exprs(402) -> 69;
yeccgoto_exprs(406) -> 229;
yeccgoto_exprs(410) -> 412.

yeccgoto_field_type(375) -> 377;
yeccgoto_field_type(382) -> 377.

yeccgoto_field_types(375) -> 376;
yeccgoto_field_types(382) -> 383.

yeccgoto_form(0) -> 7.

yeccgoto_fun_clause(59) -> 150;
yeccgoto_fun_clause(161) -> 150.

yeccgoto_fun_clauses(59) -> 149;
yeccgoto_fun_clauses(161) -> 162.

yeccgoto_fun_expr(13) -> 25;
yeccgoto_fun_expr(20) -> 25;
yeccgoto_fun_expr(45) -> 25;
yeccgoto_fun_expr(50) -> 25;
yeccgoto_fun_expr(51) -> 25;
yeccgoto_fun_expr(53) -> 25;
yeccgoto_fun_expr(55) -> 25;
yeccgoto_fun_expr(56) -> 25;
yeccgoto_fun_expr(60) -> 25;
yeccgoto_fun_expr(64) -> 25;
yeccgoto_fun_expr(66) -> 25;
yeccgoto_fun_expr(68) -> 25;
yeccgoto_fun_expr(74) -> 25;
yeccgoto_fun_expr(75) -> 25;
yeccgoto_fun_expr(76) -> 25;
yeccgoto_fun_expr(80) -> 25;
yeccgoto_fun_expr(84) -> 25;
yeccgoto_fun_expr(87) -> 25;
yeccgoto_fun_expr(90) -> 25;
yeccgoto_fun_expr(97) -> 25;
yeccgoto_fun_expr(101) -> 25;
yeccgoto_fun_expr(107) -> 25;
yeccgoto_fun_expr(109) -> 25;
yeccgoto_fun_expr(117) -> 25;
yeccgoto_fun_expr(121) -> 25;
yeccgoto_fun_expr(127) -> 25;
yeccgoto_fun_expr(129) -> 25;
yeccgoto_fun_expr(134) -> 25;
yeccgoto_fun_expr(136) -> 25;
yeccgoto_fun_expr(138) -> 25;
yeccgoto_fun_expr(146) -> 25;
yeccgoto_fun_expr(166) -> 25;
yeccgoto_fun_expr(174) -> 25;
yeccgoto_fun_expr(176) -> 25;
yeccgoto_fun_expr(181) -> 25;
yeccgoto_fun_expr(188) -> 25;
yeccgoto_fun_expr(191) -> 25;
yeccgoto_fun_expr(195) -> 25;
yeccgoto_fun_expr(220) -> 25;
yeccgoto_fun_expr(222) -> 25;
yeccgoto_fun_expr(228) -> 25;
yeccgoto_fun_expr(230) -> 25;
yeccgoto_fun_expr(231) -> 25;
yeccgoto_fun_expr(234) -> 25;
yeccgoto_fun_expr(236) -> 25;
yeccgoto_fun_expr(238) -> 25;
yeccgoto_fun_expr(248) -> 25;
yeccgoto_fun_expr(249) -> 25;
yeccgoto_fun_expr(261) -> 25;
yeccgoto_fun_expr(272) -> 25;
yeccgoto_fun_expr(285) -> 25;
yeccgoto_fun_expr(288) -> 25;
yeccgoto_fun_expr(294) -> 25;
yeccgoto_fun_expr(402) -> 25;
yeccgoto_fun_expr(406) -> 25;
yeccgoto_fun_expr(410) -> 25.

yeccgoto_fun_type(295) -> 301;
yeccgoto_fun_type(319) -> 321;
yeccgoto_fun_type(399) -> 301.

yeccgoto_fun_type_100(319) -> 320.

yeccgoto_function(0) -> 6.

yeccgoto_function_call(13) -> 24;
yeccgoto_function_call(20) -> 24;
yeccgoto_function_call(45) -> 24;
yeccgoto_function_call(51) -> 24;
yeccgoto_function_call(53) -> 24;
yeccgoto_function_call(55) -> 24;
yeccgoto_function_call(56) -> 24;
yeccgoto_function_call(60) -> 24;
yeccgoto_function_call(64) -> 24;
yeccgoto_function_call(66) -> 24;
yeccgoto_function_call(68) -> 24;
yeccgoto_function_call(74) -> 24;
yeccgoto_function_call(75) -> 24;
yeccgoto_function_call(76) -> 24;
yeccgoto_function_call(80) -> 24;
yeccgoto_function_call(84) -> 24;
yeccgoto_function_call(87) -> 24;
yeccgoto_function_call(90) -> 24;
yeccgoto_function_call(97) -> 24;
yeccgoto_function_call(101) -> 24;
yeccgoto_function_call(107) -> 24;
yeccgoto_function_call(109) -> 24;
yeccgoto_function_call(117) -> 24;
yeccgoto_function_call(121) -> 24;
yeccgoto_function_call(127) -> 24;
yeccgoto_function_call(129) -> 24;
yeccgoto_function_call(134) -> 24;
yeccgoto_function_call(136) -> 24;
yeccgoto_function_call(138) -> 24;
yeccgoto_function_call(146) -> 24;
yeccgoto_function_call(166) -> 24;
yeccgoto_function_call(174) -> 24;
yeccgoto_function_call(176) -> 24;
yeccgoto_function_call(191) -> 24;
yeccgoto_function_call(220) -> 24;
yeccgoto_function_call(222) -> 24;
yeccgoto_function_call(228) -> 24;
yeccgoto_function_call(230) -> 24;
yeccgoto_function_call(231) -> 24;
yeccgoto_function_call(234) -> 24;
yeccgoto_function_call(236) -> 24;
yeccgoto_function_call(238) -> 24;
yeccgoto_function_call(248) -> 24;
yeccgoto_function_call(249) -> 24;
yeccgoto_function_call(261) -> 24;
yeccgoto_function_call(285) -> 24;
yeccgoto_function_call(288) -> 24;
yeccgoto_function_call(294) -> 24;
yeccgoto_function_call(402) -> 24;
yeccgoto_function_call(406) -> 24;
yeccgoto_function_call(410) -> 24.

yeccgoto_function_clause(0) -> 5;
yeccgoto_function_clause(417) -> 5.

yeccgoto_function_clauses(0) -> 4;
yeccgoto_function_clauses(417) -> 418.

yeccgoto_guard(60) -> 144;
yeccgoto_guard(84) -> 85;
yeccgoto_guard(87) -> 88;
yeccgoto_guard(146) -> 144.

yeccgoto_if_clause(60) -> 143;
yeccgoto_if_clause(146) -> 143.

yeccgoto_if_clauses(60) -> 142;
yeccgoto_if_clauses(146) -> 147.

yeccgoto_if_expr(13) -> 23;
yeccgoto_if_expr(20) -> 23;
yeccgoto_if_expr(45) -> 23;
yeccgoto_if_expr(50) -> 23;
yeccgoto_if_expr(51) -> 23;
yeccgoto_if_expr(53) -> 23;
yeccgoto_if_expr(55) -> 23;
yeccgoto_if_expr(56) -> 23;
yeccgoto_if_expr(60) -> 23;
yeccgoto_if_expr(64) -> 23;
yeccgoto_if_expr(66) -> 23;
yeccgoto_if_expr(68) -> 23;
yeccgoto_if_expr(74) -> 23;
yeccgoto_if_expr(75) -> 23;
yeccgoto_if_expr(76) -> 23;
yeccgoto_if_expr(80) -> 23;
yeccgoto_if_expr(84) -> 23;
yeccgoto_if_expr(87) -> 23;
yeccgoto_if_expr(90) -> 23;
yeccgoto_if_expr(97) -> 23;
yeccgoto_if_expr(101) -> 23;
yeccgoto_if_expr(107) -> 23;
yeccgoto_if_expr(109) -> 23;
yeccgoto_if_expr(117) -> 23;
yeccgoto_if_expr(121) -> 23;
yeccgoto_if_expr(127) -> 23;
yeccgoto_if_expr(129) -> 23;
yeccgoto_if_expr(134) -> 23;
yeccgoto_if_expr(136) -> 23;
yeccgoto_if_expr(138) -> 23;
yeccgoto_if_expr(146) -> 23;
yeccgoto_if_expr(166) -> 23;
yeccgoto_if_expr(174) -> 23;
yeccgoto_if_expr(176) -> 23;
yeccgoto_if_expr(181) -> 23;
yeccgoto_if_expr(188) -> 23;
yeccgoto_if_expr(191) -> 23;
yeccgoto_if_expr(195) -> 23;
yeccgoto_if_expr(220) -> 23;
yeccgoto_if_expr(222) -> 23;
yeccgoto_if_expr(228) -> 23;
yeccgoto_if_expr(230) -> 23;
yeccgoto_if_expr(231) -> 23;
yeccgoto_if_expr(234) -> 23;
yeccgoto_if_expr(236) -> 23;
yeccgoto_if_expr(238) -> 23;
yeccgoto_if_expr(248) -> 23;
yeccgoto_if_expr(249) -> 23;
yeccgoto_if_expr(261) -> 23;
yeccgoto_if_expr(272) -> 23;
yeccgoto_if_expr(285) -> 23;
yeccgoto_if_expr(288) -> 23;
yeccgoto_if_expr(294) -> 23;
yeccgoto_if_expr(402) -> 23;
yeccgoto_if_expr(406) -> 23;
yeccgoto_if_expr(410) -> 23.

yeccgoto_int_type(295) -> 300;
yeccgoto_int_type(304) -> 300;
yeccgoto_int_type(307) -> 300;
yeccgoto_int_type(312) -> 300;
yeccgoto_int_type(316) -> 300;
yeccgoto_int_type(322) -> 300;
yeccgoto_int_type(330) -> 300;
yeccgoto_int_type(335) -> 300;
yeccgoto_int_type(337) -> 300;
yeccgoto_int_type(339) -> 300;
yeccgoto_int_type(342) -> 300;
yeccgoto_int_type(345) -> 300;
yeccgoto_int_type(380) -> 300;
yeccgoto_int_type(389) -> 300;
yeccgoto_int_type(395) -> 396;
yeccgoto_int_type(397) -> 300;
yeccgoto_int_type(407) -> 300.

yeccgoto_lc_expr(129) -> 131;
yeccgoto_lc_expr(138) -> 131;
yeccgoto_lc_expr(191) -> 131;
yeccgoto_lc_expr(285) -> 131.

yeccgoto_lc_exprs(129) -> 130;
yeccgoto_lc_exprs(138) -> 139;
yeccgoto_lc_exprs(191) -> 192;
yeccgoto_lc_exprs(285) -> 286.

yeccgoto_list(13) -> 22;
yeccgoto_list(20) -> 22;
yeccgoto_list(45) -> 22;
yeccgoto_list(50) -> 22;
yeccgoto_list(51) -> 22;
yeccgoto_list(53) -> 22;
yeccgoto_list(55) -> 22;
yeccgoto_list(56) -> 22;
yeccgoto_list(60) -> 22;
yeccgoto_list(64) -> 22;
yeccgoto_list(66) -> 22;
yeccgoto_list(68) -> 22;
yeccgoto_list(74) -> 22;
yeccgoto_list(75) -> 22;
yeccgoto_list(76) -> 22;
yeccgoto_list(80) -> 22;
yeccgoto_list(84) -> 22;
yeccgoto_list(87) -> 22;
yeccgoto_list(90) -> 22;
yeccgoto_list(97) -> 22;
yeccgoto_list(101) -> 22;
yeccgoto_list(107) -> 22;
yeccgoto_list(109) -> 22;
yeccgoto_list(117) -> 22;
yeccgoto_list(121) -> 22;
yeccgoto_list(127) -> 22;
yeccgoto_list(129) -> 22;
yeccgoto_list(134) -> 22;
yeccgoto_list(136) -> 22;
yeccgoto_list(138) -> 22;
yeccgoto_list(146) -> 22;
yeccgoto_list(166) -> 22;
yeccgoto_list(174) -> 22;
yeccgoto_list(176) -> 22;
yeccgoto_list(181) -> 22;
yeccgoto_list(188) -> 22;
yeccgoto_list(191) -> 22;
yeccgoto_list(195) -> 22;
yeccgoto_list(220) -> 22;
yeccgoto_list(222) -> 22;
yeccgoto_list(228) -> 22;
yeccgoto_list(230) -> 22;
yeccgoto_list(231) -> 22;
yeccgoto_list(234) -> 22;
yeccgoto_list(236) -> 22;
yeccgoto_list(238) -> 22;
yeccgoto_list(248) -> 22;
yeccgoto_list(249) -> 22;
yeccgoto_list(261) -> 22;
yeccgoto_list(272) -> 22;
yeccgoto_list(285) -> 22;
yeccgoto_list(288) -> 22;
yeccgoto_list(294) -> 22;
yeccgoto_list(402) -> 22;
yeccgoto_list(406) -> 22;
yeccgoto_list(410) -> 22.

yeccgoto_list_comprehension(13) -> 21;
yeccgoto_list_comprehension(20) -> 21;
yeccgoto_list_comprehension(45) -> 21;
yeccgoto_list_comprehension(50) -> 21;
yeccgoto_list_comprehension(51) -> 21;
yeccgoto_list_comprehension(53) -> 21;
yeccgoto_list_comprehension(55) -> 21;
yeccgoto_list_comprehension(56) -> 21;
yeccgoto_list_comprehension(60) -> 21;
yeccgoto_list_comprehension(63) -> 126;
yeccgoto_list_comprehension(64) -> 21;
yeccgoto_list_comprehension(66) -> 21;
yeccgoto_list_comprehension(68) -> 21;
yeccgoto_list_comprehension(74) -> 21;
yeccgoto_list_comprehension(75) -> 21;
yeccgoto_list_comprehension(76) -> 21;
yeccgoto_list_comprehension(80) -> 21;
yeccgoto_list_comprehension(84) -> 21;
yeccgoto_list_comprehension(87) -> 21;
yeccgoto_list_comprehension(90) -> 21;
yeccgoto_list_comprehension(97) -> 21;
yeccgoto_list_comprehension(101) -> 21;
yeccgoto_list_comprehension(107) -> 21;
yeccgoto_list_comprehension(109) -> 21;
yeccgoto_list_comprehension(117) -> 21;
yeccgoto_list_comprehension(121) -> 21;
yeccgoto_list_comprehension(127) -> 21;
yeccgoto_list_comprehension(129) -> 21;
yeccgoto_list_comprehension(134) -> 21;
yeccgoto_list_comprehension(136) -> 21;
yeccgoto_list_comprehension(138) -> 21;
yeccgoto_list_comprehension(146) -> 21;
yeccgoto_list_comprehension(166) -> 21;
yeccgoto_list_comprehension(174) -> 21;
yeccgoto_list_comprehension(176) -> 21;
yeccgoto_list_comprehension(181) -> 21;
yeccgoto_list_comprehension(188) -> 21;
yeccgoto_list_comprehension(191) -> 21;
yeccgoto_list_comprehension(195) -> 21;
yeccgoto_list_comprehension(220) -> 21;
yeccgoto_list_comprehension(222) -> 21;
yeccgoto_list_comprehension(228) -> 21;
yeccgoto_list_comprehension(230) -> 21;
yeccgoto_list_comprehension(231) -> 21;
yeccgoto_list_comprehension(234) -> 21;
yeccgoto_list_comprehension(236) -> 21;
yeccgoto_list_comprehension(238) -> 21;
yeccgoto_list_comprehension(248) -> 21;
yeccgoto_list_comprehension(249) -> 21;
yeccgoto_list_comprehension(261) -> 21;
yeccgoto_list_comprehension(272) -> 21;
yeccgoto_list_comprehension(285) -> 21;
yeccgoto_list_comprehension(288) -> 21;
yeccgoto_list_comprehension(294) -> 21;
yeccgoto_list_comprehension(402) -> 21;
yeccgoto_list_comprehension(406) -> 21;
yeccgoto_list_comprehension(410) -> 21.

yeccgoto_list_op(33) -> 248.

yeccgoto_mult_op(32) -> 261;
yeccgoto_mult_op(260) -> 261.

yeccgoto_opt_bit_size_expr(183) -> 194.

yeccgoto_opt_bit_type_list(194) -> 198.

yeccgoto_prefix_op(13) -> 20;
yeccgoto_prefix_op(45) -> 20;
yeccgoto_prefix_op(50) -> 181;
yeccgoto_prefix_op(51) -> 20;
yeccgoto_prefix_op(53) -> 20;
yeccgoto_prefix_op(55) -> 20;
yeccgoto_prefix_op(56) -> 20;
yeccgoto_prefix_op(60) -> 20;
yeccgoto_prefix_op(64) -> 20;
yeccgoto_prefix_op(66) -> 20;
yeccgoto_prefix_op(68) -> 20;
yeccgoto_prefix_op(74) -> 20;
yeccgoto_prefix_op(75) -> 20;
yeccgoto_prefix_op(76) -> 20;
yeccgoto_prefix_op(80) -> 20;
yeccgoto_prefix_op(84) -> 20;
yeccgoto_prefix_op(87) -> 20;
yeccgoto_prefix_op(90) -> 20;
yeccgoto_prefix_op(97) -> 20;
yeccgoto_prefix_op(101) -> 20;
yeccgoto_prefix_op(107) -> 20;
yeccgoto_prefix_op(109) -> 20;
yeccgoto_prefix_op(117) -> 20;
yeccgoto_prefix_op(121) -> 20;
yeccgoto_prefix_op(127) -> 20;
yeccgoto_prefix_op(129) -> 20;
yeccgoto_prefix_op(134) -> 20;
yeccgoto_prefix_op(136) -> 20;
yeccgoto_prefix_op(138) -> 20;
yeccgoto_prefix_op(146) -> 20;
yeccgoto_prefix_op(166) -> 20;
yeccgoto_prefix_op(174) -> 20;
yeccgoto_prefix_op(176) -> 20;
yeccgoto_prefix_op(188) -> 181;
yeccgoto_prefix_op(191) -> 20;
yeccgoto_prefix_op(220) -> 20;
yeccgoto_prefix_op(222) -> 20;
yeccgoto_prefix_op(228) -> 20;
yeccgoto_prefix_op(230) -> 20;
yeccgoto_prefix_op(231) -> 20;
yeccgoto_prefix_op(234) -> 20;
yeccgoto_prefix_op(236) -> 20;
yeccgoto_prefix_op(238) -> 20;
yeccgoto_prefix_op(248) -> 20;
yeccgoto_prefix_op(249) -> 20;
yeccgoto_prefix_op(261) -> 20;
yeccgoto_prefix_op(285) -> 20;
yeccgoto_prefix_op(288) -> 20;
yeccgoto_prefix_op(294) -> 20;
yeccgoto_prefix_op(402) -> 20;
yeccgoto_prefix_op(406) -> 20;
yeccgoto_prefix_op(410) -> 20.

yeccgoto_query_expr(13) -> 19;
yeccgoto_query_expr(20) -> 19;
yeccgoto_query_expr(45) -> 19;
yeccgoto_query_expr(50) -> 19;
yeccgoto_query_expr(51) -> 19;
yeccgoto_query_expr(53) -> 19;
yeccgoto_query_expr(55) -> 19;
yeccgoto_query_expr(56) -> 19;
yeccgoto_query_expr(60) -> 19;
yeccgoto_query_expr(64) -> 19;
yeccgoto_query_expr(66) -> 19;
yeccgoto_query_expr(68) -> 19;
yeccgoto_query_expr(74) -> 19;
yeccgoto_query_expr(75) -> 19;
yeccgoto_query_expr(76) -> 19;
yeccgoto_query_expr(80) -> 19;
yeccgoto_query_expr(84) -> 19;
yeccgoto_query_expr(87) -> 19;
yeccgoto_query_expr(90) -> 19;
yeccgoto_query_expr(97) -> 19;
yeccgoto_query_expr(101) -> 19;
yeccgoto_query_expr(107) -> 19;
yeccgoto_query_expr(109) -> 19;
yeccgoto_query_expr(117) -> 19;
yeccgoto_query_expr(121) -> 19;
yeccgoto_query_expr(127) -> 19;
yeccgoto_query_expr(129) -> 19;
yeccgoto_query_expr(134) -> 19;
yeccgoto_query_expr(136) -> 19;
yeccgoto_query_expr(138) -> 19;
yeccgoto_query_expr(146) -> 19;
yeccgoto_query_expr(166) -> 19;
yeccgoto_query_expr(174) -> 19;
yeccgoto_query_expr(176) -> 19;
yeccgoto_query_expr(181) -> 19;
yeccgoto_query_expr(188) -> 19;
yeccgoto_query_expr(191) -> 19;
yeccgoto_query_expr(195) -> 19;
yeccgoto_query_expr(220) -> 19;
yeccgoto_query_expr(222) -> 19;
yeccgoto_query_expr(228) -> 19;
yeccgoto_query_expr(230) -> 19;
yeccgoto_query_expr(231) -> 19;
yeccgoto_query_expr(234) -> 19;
yeccgoto_query_expr(236) -> 19;
yeccgoto_query_expr(238) -> 19;
yeccgoto_query_expr(248) -> 19;
yeccgoto_query_expr(249) -> 19;
yeccgoto_query_expr(261) -> 19;
yeccgoto_query_expr(272) -> 19;
yeccgoto_query_expr(285) -> 19;
yeccgoto_query_expr(288) -> 19;
yeccgoto_query_expr(294) -> 19;
yeccgoto_query_expr(402) -> 19;
yeccgoto_query_expr(406) -> 19;
yeccgoto_query_expr(410) -> 19.

yeccgoto_receive_expr(13) -> 18;
yeccgoto_receive_expr(20) -> 18;
yeccgoto_receive_expr(45) -> 18;
yeccgoto_receive_expr(50) -> 18;
yeccgoto_receive_expr(51) -> 18;
yeccgoto_receive_expr(53) -> 18;
yeccgoto_receive_expr(55) -> 18;
yeccgoto_receive_expr(56) -> 18;
yeccgoto_receive_expr(60) -> 18;
yeccgoto_receive_expr(64) -> 18;
yeccgoto_receive_expr(66) -> 18;
yeccgoto_receive_expr(68) -> 18;
yeccgoto_receive_expr(74) -> 18;
yeccgoto_receive_expr(75) -> 18;
yeccgoto_receive_expr(76) -> 18;
yeccgoto_receive_expr(80) -> 18;
yeccgoto_receive_expr(84) -> 18;
yeccgoto_receive_expr(87) -> 18;
yeccgoto_receive_expr(90) -> 18;
yeccgoto_receive_expr(97) -> 18;
yeccgoto_receive_expr(101) -> 18;
yeccgoto_receive_expr(107) -> 18;
yeccgoto_receive_expr(109) -> 18;
yeccgoto_receive_expr(117) -> 18;
yeccgoto_receive_expr(121) -> 18;
yeccgoto_receive_expr(127) -> 18;
yeccgoto_receive_expr(129) -> 18;
yeccgoto_receive_expr(134) -> 18;
yeccgoto_receive_expr(136) -> 18;
yeccgoto_receive_expr(138) -> 18;
yeccgoto_receive_expr(146) -> 18;
yeccgoto_receive_expr(166) -> 18;
yeccgoto_receive_expr(174) -> 18;
yeccgoto_receive_expr(176) -> 18;
yeccgoto_receive_expr(181) -> 18;
yeccgoto_receive_expr(188) -> 18;
yeccgoto_receive_expr(191) -> 18;
yeccgoto_receive_expr(195) -> 18;
yeccgoto_receive_expr(220) -> 18;
yeccgoto_receive_expr(222) -> 18;
yeccgoto_receive_expr(228) -> 18;
yeccgoto_receive_expr(230) -> 18;
yeccgoto_receive_expr(231) -> 18;
yeccgoto_receive_expr(234) -> 18;
yeccgoto_receive_expr(236) -> 18;
yeccgoto_receive_expr(238) -> 18;
yeccgoto_receive_expr(248) -> 18;
yeccgoto_receive_expr(249) -> 18;
yeccgoto_receive_expr(261) -> 18;
yeccgoto_receive_expr(272) -> 18;
yeccgoto_receive_expr(285) -> 18;
yeccgoto_receive_expr(288) -> 18;
yeccgoto_receive_expr(294) -> 18;
yeccgoto_receive_expr(402) -> 18;
yeccgoto_receive_expr(406) -> 18;
yeccgoto_receive_expr(410) -> 18.

yeccgoto_record_expr(13) -> 17;
yeccgoto_record_expr(20) -> 17;
yeccgoto_record_expr(45) -> 17;
yeccgoto_record_expr(51) -> 17;
yeccgoto_record_expr(53) -> 17;
yeccgoto_record_expr(55) -> 17;
yeccgoto_record_expr(56) -> 17;
yeccgoto_record_expr(60) -> 17;
yeccgoto_record_expr(64) -> 17;
yeccgoto_record_expr(66) -> 17;
yeccgoto_record_expr(68) -> 17;
yeccgoto_record_expr(74) -> 17;
yeccgoto_record_expr(75) -> 17;
yeccgoto_record_expr(76) -> 17;
yeccgoto_record_expr(80) -> 17;
yeccgoto_record_expr(84) -> 17;
yeccgoto_record_expr(87) -> 17;
yeccgoto_record_expr(90) -> 17;
yeccgoto_record_expr(97) -> 17;
yeccgoto_record_expr(101) -> 17;
yeccgoto_record_expr(107) -> 17;
yeccgoto_record_expr(109) -> 17;
yeccgoto_record_expr(117) -> 17;
yeccgoto_record_expr(121) -> 17;
yeccgoto_record_expr(127) -> 17;
yeccgoto_record_expr(129) -> 17;
yeccgoto_record_expr(134) -> 17;
yeccgoto_record_expr(136) -> 17;
yeccgoto_record_expr(138) -> 17;
yeccgoto_record_expr(146) -> 17;
yeccgoto_record_expr(166) -> 17;
yeccgoto_record_expr(174) -> 17;
yeccgoto_record_expr(176) -> 17;
yeccgoto_record_expr(191) -> 17;
yeccgoto_record_expr(220) -> 17;
yeccgoto_record_expr(222) -> 17;
yeccgoto_record_expr(228) -> 17;
yeccgoto_record_expr(230) -> 17;
yeccgoto_record_expr(231) -> 17;
yeccgoto_record_expr(234) -> 17;
yeccgoto_record_expr(236) -> 17;
yeccgoto_record_expr(238) -> 17;
yeccgoto_record_expr(248) -> 17;
yeccgoto_record_expr(249) -> 17;
yeccgoto_record_expr(261) -> 17;
yeccgoto_record_expr(285) -> 17;
yeccgoto_record_expr(288) -> 17;
yeccgoto_record_expr(294) -> 17;
yeccgoto_record_expr(402) -> 17;
yeccgoto_record_expr(406) -> 17;
yeccgoto_record_expr(410) -> 17.

yeccgoto_record_field(214) -> 216;
yeccgoto_record_field(224) -> 216.

yeccgoto_record_fields(214) -> 215;
yeccgoto_record_fields(224) -> 225.

yeccgoto_record_tuple(211) -> 212;
yeccgoto_record_tuple(276) -> 277.

yeccgoto_rule(0) -> 3.

yeccgoto_rule_body(282) -> 283;
yeccgoto_rule_body(427) -> 283.

yeccgoto_rule_clause(0) -> 2;
yeccgoto_rule_clause(423) -> 2.

yeccgoto_rule_clauses(0) -> 1;
yeccgoto_rule_clauses(423) -> 424.

yeccgoto_strings(13) -> 16;
yeccgoto_strings(20) -> 16;
yeccgoto_strings(45) -> 16;
yeccgoto_strings(50) -> 16;
yeccgoto_strings(51) -> 16;
yeccgoto_strings(53) -> 16;
yeccgoto_strings(55) -> 16;
yeccgoto_strings(56) -> 16;
yeccgoto_strings(60) -> 16;
yeccgoto_strings(64) -> 16;
yeccgoto_strings(65) -> 115;
yeccgoto_strings(66) -> 16;
yeccgoto_strings(68) -> 16;
yeccgoto_strings(74) -> 16;
yeccgoto_strings(75) -> 16;
yeccgoto_strings(76) -> 16;
yeccgoto_strings(80) -> 16;
yeccgoto_strings(84) -> 16;
yeccgoto_strings(87) -> 16;
yeccgoto_strings(90) -> 16;
yeccgoto_strings(97) -> 16;
yeccgoto_strings(101) -> 16;
yeccgoto_strings(107) -> 16;
yeccgoto_strings(109) -> 16;
yeccgoto_strings(117) -> 16;
yeccgoto_strings(121) -> 16;
yeccgoto_strings(127) -> 16;
yeccgoto_strings(129) -> 16;
yeccgoto_strings(134) -> 16;
yeccgoto_strings(136) -> 16;
yeccgoto_strings(138) -> 16;
yeccgoto_strings(146) -> 16;
yeccgoto_strings(166) -> 16;
yeccgoto_strings(174) -> 16;
yeccgoto_strings(176) -> 16;
yeccgoto_strings(181) -> 16;
yeccgoto_strings(188) -> 16;
yeccgoto_strings(191) -> 16;
yeccgoto_strings(195) -> 16;
yeccgoto_strings(220) -> 16;
yeccgoto_strings(222) -> 16;
yeccgoto_strings(228) -> 16;
yeccgoto_strings(230) -> 16;
yeccgoto_strings(231) -> 16;
yeccgoto_strings(234) -> 16;
yeccgoto_strings(236) -> 16;
yeccgoto_strings(238) -> 16;
yeccgoto_strings(248) -> 16;
yeccgoto_strings(249) -> 16;
yeccgoto_strings(261) -> 16;
yeccgoto_strings(272) -> 16;
yeccgoto_strings(285) -> 16;
yeccgoto_strings(288) -> 16;
yeccgoto_strings(294) -> 16;
yeccgoto_strings(402) -> 16;
yeccgoto_strings(406) -> 16;
yeccgoto_strings(410) -> 16.

yeccgoto_tail(171) -> 173;
yeccgoto_tail(179) -> 180.

yeccgoto_top_type(295) -> 299;
yeccgoto_top_type(304) -> 324;
yeccgoto_top_type(307) -> 349;
yeccgoto_top_type(312) -> 314;
yeccgoto_top_type(316) -> 314;
yeccgoto_top_type(322) -> 324;
yeccgoto_top_type(330) -> 331;
yeccgoto_top_type(335) -> 336;
yeccgoto_top_type(337) -> 338;
yeccgoto_top_type(339) -> 324;
yeccgoto_top_type(342) -> 343;
yeccgoto_top_type(345) -> 314;
yeccgoto_top_type(380) -> 381;
yeccgoto_top_type(389) -> 314;
yeccgoto_top_type(397) -> 398;
yeccgoto_top_type(407) -> 408.

yeccgoto_top_types(312) -> 313;
yeccgoto_top_types(316) -> 317;
yeccgoto_top_types(345) -> 346;
yeccgoto_top_types(389) -> 390.

yeccgoto_try_catch(72) -> 73;
yeccgoto_try_catch(78) -> 82.

yeccgoto_try_clause(75) -> 93;
yeccgoto_try_clause(107) -> 93.

yeccgoto_try_clauses(75) -> 92;
yeccgoto_try_clauses(107) -> 108.

yeccgoto_try_expr(13) -> 15;
yeccgoto_try_expr(20) -> 15;
yeccgoto_try_expr(45) -> 15;
yeccgoto_try_expr(50) -> 15;
yeccgoto_try_expr(51) -> 15;
yeccgoto_try_expr(53) -> 15;
yeccgoto_try_expr(55) -> 15;
yeccgoto_try_expr(56) -> 15;
yeccgoto_try_expr(60) -> 15;
yeccgoto_try_expr(64) -> 15;
yeccgoto_try_expr(66) -> 15;
yeccgoto_try_expr(68) -> 15;
yeccgoto_try_expr(74) -> 15;
yeccgoto_try_expr(75) -> 15;
yeccgoto_try_expr(76) -> 15;
yeccgoto_try_expr(80) -> 15;
yeccgoto_try_expr(84) -> 15;
yeccgoto_try_expr(87) -> 15;
yeccgoto_try_expr(90) -> 15;
yeccgoto_try_expr(97) -> 15;
yeccgoto_try_expr(101) -> 15;
yeccgoto_try_expr(107) -> 15;
yeccgoto_try_expr(109) -> 15;
yeccgoto_try_expr(117) -> 15;
yeccgoto_try_expr(121) -> 15;
yeccgoto_try_expr(127) -> 15;
yeccgoto_try_expr(129) -> 15;
yeccgoto_try_expr(134) -> 15;
yeccgoto_try_expr(136) -> 15;
yeccgoto_try_expr(138) -> 15;
yeccgoto_try_expr(146) -> 15;
yeccgoto_try_expr(166) -> 15;
yeccgoto_try_expr(174) -> 15;
yeccgoto_try_expr(176) -> 15;
yeccgoto_try_expr(181) -> 15;
yeccgoto_try_expr(188) -> 15;
yeccgoto_try_expr(191) -> 15;
yeccgoto_try_expr(195) -> 15;
yeccgoto_try_expr(220) -> 15;
yeccgoto_try_expr(222) -> 15;
yeccgoto_try_expr(228) -> 15;
yeccgoto_try_expr(230) -> 15;
yeccgoto_try_expr(231) -> 15;
yeccgoto_try_expr(234) -> 15;
yeccgoto_try_expr(236) -> 15;
yeccgoto_try_expr(238) -> 15;
yeccgoto_try_expr(248) -> 15;
yeccgoto_try_expr(249) -> 15;
yeccgoto_try_expr(261) -> 15;
yeccgoto_try_expr(272) -> 15;
yeccgoto_try_expr(285) -> 15;
yeccgoto_try_expr(288) -> 15;
yeccgoto_try_expr(294) -> 15;
yeccgoto_try_expr(402) -> 15;
yeccgoto_try_expr(406) -> 15;
yeccgoto_try_expr(410) -> 15.

yeccgoto_tuple(13) -> 14;
yeccgoto_tuple(20) -> 14;
yeccgoto_tuple(45) -> 14;
yeccgoto_tuple(50) -> 14;
yeccgoto_tuple(51) -> 14;
yeccgoto_tuple(53) -> 14;
yeccgoto_tuple(55) -> 14;
yeccgoto_tuple(56) -> 14;
yeccgoto_tuple(60) -> 14;
yeccgoto_tuple(64) -> 14;
yeccgoto_tuple(66) -> 14;
yeccgoto_tuple(68) -> 14;
yeccgoto_tuple(74) -> 14;
yeccgoto_tuple(75) -> 14;
yeccgoto_tuple(76) -> 14;
yeccgoto_tuple(80) -> 14;
yeccgoto_tuple(84) -> 14;
yeccgoto_tuple(87) -> 14;
yeccgoto_tuple(90) -> 14;
yeccgoto_tuple(97) -> 14;
yeccgoto_tuple(101) -> 14;
yeccgoto_tuple(107) -> 14;
yeccgoto_tuple(109) -> 14;
yeccgoto_tuple(117) -> 14;
yeccgoto_tuple(121) -> 14;
yeccgoto_tuple(127) -> 14;
yeccgoto_tuple(129) -> 14;
yeccgoto_tuple(134) -> 14;
yeccgoto_tuple(136) -> 14;
yeccgoto_tuple(138) -> 14;
yeccgoto_tuple(146) -> 14;
yeccgoto_tuple(166) -> 14;
yeccgoto_tuple(174) -> 14;
yeccgoto_tuple(176) -> 14;
yeccgoto_tuple(181) -> 14;
yeccgoto_tuple(188) -> 14;
yeccgoto_tuple(191) -> 14;
yeccgoto_tuple(195) -> 14;
yeccgoto_tuple(220) -> 14;
yeccgoto_tuple(222) -> 14;
yeccgoto_tuple(228) -> 14;
yeccgoto_tuple(230) -> 14;
yeccgoto_tuple(231) -> 14;
yeccgoto_tuple(234) -> 14;
yeccgoto_tuple(236) -> 14;
yeccgoto_tuple(238) -> 14;
yeccgoto_tuple(248) -> 14;
yeccgoto_tuple(249) -> 14;
yeccgoto_tuple(261) -> 14;
yeccgoto_tuple(272) -> 14;
yeccgoto_tuple(285) -> 14;
yeccgoto_tuple(288) -> 14;
yeccgoto_tuple(294) -> 14;
yeccgoto_tuple(402) -> 14;
yeccgoto_tuple(406) -> 14;
yeccgoto_tuple(410) -> 14.

yeccgoto_type(295) -> 298;
yeccgoto_type(304) -> 298;
yeccgoto_type(307) -> 298;
yeccgoto_type(312) -> 298;
yeccgoto_type(316) -> 298;
yeccgoto_type(322) -> 298;
yeccgoto_type(330) -> 298;
yeccgoto_type(335) -> 298;
yeccgoto_type(337) -> 298;
yeccgoto_type(339) -> 298;
yeccgoto_type(342) -> 298;
yeccgoto_type(345) -> 298;
yeccgoto_type(380) -> 298;
yeccgoto_type(389) -> 298;
yeccgoto_type(397) -> 298;
yeccgoto_type(407) -> 298.

yeccgoto_type_guard(385) -> 387;
yeccgoto_type_guard(392) -> 387.

yeccgoto_type_guards(385) -> 386;
yeccgoto_type_guards(392) -> 393.

yeccgoto_type_sig(295) -> 297;
yeccgoto_type_sig(399) -> 297.

yeccgoto_type_sigs(295) -> 296;
yeccgoto_type_sigs(399) -> 400.

yeccgoto_typed_attr_val(288) -> 289.

yeccgoto_typed_expr(402) -> 404;
yeccgoto_typed_expr(406) -> 404;
yeccgoto_typed_expr(410) -> 404.

yeccgoto_typed_exprs(402) -> 403;
yeccgoto_typed_exprs(406) -> 409;
yeccgoto_typed_exprs(410) -> 411.

yeccgoto_typed_record_fields(294) -> 401.

-compile({inline,{yeccpars2_1_,1}}).
-file("erl_parse.yrl", 468).
yeccpars2_1_([__1 | Stack]) ->
 [begin
   build_rule ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_2_,1}}).
-file("erl_parse.yrl", 470).
yeccpars2_2_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_4_,1}}).
-file("erl_parse.yrl", 173).
yeccpars2_4_([__1 | Stack]) ->
 [begin
   build_function ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_5_,1}}).
-file("erl_parse.yrl", 175).
yeccpars2_5_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_11_,1}}).
-file("erl_parse.yrl", 185).
yeccpars2_11_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_12_,1}}).
-file("erl_parse.yrl", 182).
yeccpars2_12_([__1 | Stack]) ->
 [begin
   element ( 1 , __1 )
  end | Stack].

-compile({inline,{yeccpars2_39_,1}}).
-file("erl_parse.yrl", 419).
yeccpars2_39_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_46_,1}}).
-file("erl_parse.yrl", 415).
yeccpars2_46_([__2,__1 | Stack]) ->
 [begin
   { [ ] , line ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_70_,1}}).
-file("erl_parse.yrl", 302).
yeccpars2_70_([__2,__1 | Stack]) ->
 [begin
   { tuple , line ( __1 ) , [ ] }
  end | Stack].

-compile({inline,{yeccpars2_71_,1}}).
-file("erl_parse.yrl", 303).
yeccpars2_71_([__3,__2,__1 | Stack]) ->
 [begin
   { tuple , line ( __1 ) , __2 }
  end | Stack].

-compile({inline,{yeccpars2_73_,1}}).
-file("erl_parse.yrl", 381).
yeccpars2_73_([__3,__2,__1 | Stack]) ->
 [begin
   build_try ( line ( __1 ) , __2 , [ ] , __3 )
  end | Stack].

-compile({inline,{yeccpars2_77_,1}}).
-file("erl_parse.yrl", 185).
yeccpars2_77_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_79_,1}}).
-file("erl_parse.yrl", 350).
yeccpars2_79_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_81_,1}}).
-file("erl_parse.yrl", 351).
yeccpars2_81_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_82_,1}}).
-file("erl_parse.yrl", 379).
yeccpars2_82_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   build_try ( line ( __1 ) , __2 , __4 , __5 )
  end | Stack].

-compile({inline,{yeccpars2_85_,1}}).
-file("erl_parse.yrl", 184).
yeccpars2_85_([__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_86_,1}}).
-file("erl_parse.yrl", 422).
yeccpars2_86_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_88_,1}}).
-file("erl_parse.yrl", 423).
yeccpars2_88_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_89_,1}}).
-file("erl_parse.yrl", 354).
yeccpars2_89_([__3,__2,__1 | Stack]) ->
 [begin
   { clause , line ( __1 ) , [ __1 ] , __2 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_91_,1}}).
-file("erl_parse.yrl", 187).
yeccpars2_91_([__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_93_,1}}).
-file("erl_parse.yrl", 390).
yeccpars2_93_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_94_,1}}).
-file("erl_parse.yrl", 185).
yeccpars2_94_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_98_,1}}).
-file("erl_parse.yrl", 185).
yeccpars2_98_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_100_,1}}).
-file("erl_parse.yrl", 400).
yeccpars2_100_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   L = line ( __1 ) ,
    { clause , L , [ { tuple , L , [ __1 , __3 , { var , L , '_' } ] } ] , __4 , __5 }
  end | Stack].

-compile({inline,{yeccpars2_102_,1}}).
-file("erl_parse.yrl", 185).
yeccpars2_102_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_104_,1}}).
-file("erl_parse.yrl", 397).
yeccpars2_104_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   L = line ( __1 ) ,
    { clause , L , [ { tuple , L , [ __1 , __3 , { var , L , '_' } ] } ] , __4 , __5 }
  end | Stack].

-compile({inline,{yeccpars2_106_,1}}).
-file("erl_parse.yrl", 394).
yeccpars2_106_([__3,__2,__1 | Stack]) ->
 [begin
   L = line ( __1 ) ,
    { clause , L , [ { tuple , L , [ { atom , L , throw } , __1 , { var , L , '_' } ] } ] , __2 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_108_,1}}).
-file("erl_parse.yrl", 391).
yeccpars2_108_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_110_,1}}).
-file("erl_parse.yrl", 384).
yeccpars2_110_([__3,__2,__1 | Stack]) ->
 [begin
   { __2 , [ ] }
  end | Stack].

-compile({inline,{yeccpars2_112_,1}}).
-file("erl_parse.yrl", 386).
yeccpars2_112_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { __2 , __4 }
  end | Stack].

-compile({inline,{yeccpars2_114_,1}}).
-file("erl_parse.yrl", 388).
yeccpars2_114_([__3,__2,__1 | Stack]) ->
 [begin
   { [ ] , __2 }
  end | Stack].

-compile({inline,{yeccpars2_115_,1}}).
-file("erl_parse.yrl", 433).
yeccpars2_115_([__2,__1 | Stack]) ->
 [begin
   { string , line ( __1 ) , element ( 3 , __1 ) ++ element ( 3 , __2 ) }
  end | Stack].

-compile({inline,{yeccpars2_120_,1}}).
-file("erl_parse.yrl", 359).
yeccpars2_120_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { 'receive' , line ( __1 ) , [ ] , __3 , __4 }
  end | Stack].

-compile({inline,{yeccpars2_122_,1}}).
-file("erl_parse.yrl", 357).
yeccpars2_122_([__3,__2,__1 | Stack]) ->
 [begin
   { 'receive' , line ( __1 ) , __2 }
  end | Stack].

-compile({inline,{yeccpars2_125_,1}}).
-file("erl_parse.yrl", 361).
yeccpars2_125_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { 'receive' , line ( __1 ) , __2 , __4 , __5 }
  end | Stack].

-compile({inline,{yeccpars2_131_,1}}).
-file("erl_parse.yrl", 295).
yeccpars2_131_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_135_,1}}).
-file("erl_parse.yrl", 300).
yeccpars2_135_([__3,__2,__1 | Stack]) ->
 [begin
   { b_generate , line ( __2 ) , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_137_,1}}).
-file("erl_parse.yrl", 299).
yeccpars2_137_([__3,__2,__1 | Stack]) ->
 [begin
   { generate , line ( __2 ) , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_139_,1}}).
-file("erl_parse.yrl", 296).
yeccpars2_139_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_140_,1}}).
-file("erl_parse.yrl", 292).
yeccpars2_140_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { lc , line ( __1 ) , __2 , __4 }
  end | Stack].

-compile({inline,{yeccpars2_141_,1}}).
-file("erl_parse.yrl", 412).
yeccpars2_141_([__3,__2,__1 | Stack]) ->
 [begin
   { 'query' , line ( __1 ) , __2 }
  end | Stack].

-compile({inline,{yeccpars2_143_,1}}).
-file("erl_parse.yrl", 340).
yeccpars2_143_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_145_,1}}).
-file("erl_parse.yrl", 344).
yeccpars2_145_([__2,__1 | Stack]) ->
 [begin
   { clause , line ( hd ( hd ( __1 ) ) ) , [ ] , __1 , __2 }
  end | Stack].

-compile({inline,{yeccpars2_147_,1}}).
-file("erl_parse.yrl", 341).
yeccpars2_147_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_148_,1}}).
-file("erl_parse.yrl", 338).
yeccpars2_148_([__3,__2,__1 | Stack]) ->
 [begin
   { 'if' , line ( __1 ) , __2 }
  end | Stack].

-compile({inline,{yeccpars2_150_,1}}).
-file("erl_parse.yrl", 371).
yeccpars2_150_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_151_,1}}).
-file("erl_parse.yrl", 185).
yeccpars2_151_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_157_,1}}).
-file("erl_parse.yrl", 367).
yeccpars2_157_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { 'fun' , line ( __1 ) , { function , element ( 3 , __2 ) , element ( 3 , __4 ) , element ( 3 , __6 ) } }
  end | Stack].

-compile({inline,{yeccpars2_158_,1}}).
-file("erl_parse.yrl", 365).
yeccpars2_158_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { 'fun' , line ( __1 ) , { function , element ( 3 , __2 ) , element ( 3 , __4 ) } }
  end | Stack].

-compile({inline,{yeccpars2_160_,1}}).
-file("erl_parse.yrl", 375).
yeccpars2_160_([__3,__2,__1 | Stack]) ->
 [begin
   { Args , Pos } = __1 ,
    { clause , Pos , 'fun' , Args , __2 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_162_,1}}).
-file("erl_parse.yrl", 372).
yeccpars2_162_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_163_,1}}).
-file("erl_parse.yrl", 369).
yeccpars2_163_([__3,__2,__1 | Stack]) ->
 [begin
   build_fun ( line ( __1 ) , __2 )
  end | Stack].

-compile({inline,{yeccpars2_164_,1}}).
-file("erl_parse.yrl", 190).
yeccpars2_164_([__2,__1 | Stack]) ->
 [begin
   { 'catch' , line ( __1 ) , __2 }
  end | Stack].

-compile({inline,{yeccpars2_168_,1}}).
-file("erl_parse.yrl", 348).
yeccpars2_168_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { 'case' , line ( __1 ) , __2 , __4 }
  end | Stack].

-compile({inline,{yeccpars2_170_,1}}).
-file("erl_parse.yrl", 246).
yeccpars2_170_([__3,__2,__1 | Stack]) ->
 [begin
   { block , line ( __1 ) , __2 }
  end | Stack].

-compile({inline,{yeccpars2_172_,1}}).
-file("erl_parse.yrl", 256).
yeccpars2_172_([__2,__1 | Stack]) ->
 [begin
   { nil , line ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_173_,1}}).
-file("erl_parse.yrl", 257).
yeccpars2_173_([__3,__2,__1 | Stack]) ->
 [begin
   { cons , line ( __1 ) , __2 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_175_,1}}).
-file("erl_parse.yrl", 259).
yeccpars2_175_([__1 | Stack]) ->
 [begin
   { nil , line ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_178_,1}}).
-file("erl_parse.yrl", 260).
yeccpars2_178_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_180_,1}}).
-file("erl_parse.yrl", 261).
yeccpars2_180_([__3,__2,__1 | Stack]) ->
 [begin
   { cons , line ( __2 ) , __2 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_183_,1}}).
-file("erl_parse.yrl", 277).
yeccpars2_183_(Stack) ->
 [begin
   default
  end | Stack].

-compile({inline,{yeccpars2_186_,1}}).
-file("erl_parse.yrl", 267).
yeccpars2_186_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_187_,1}}).
-file("erl_parse.yrl", 264).
yeccpars2_187_([__2,__1 | Stack]) ->
 [begin
   { bin , line ( __1 ) , [ ] }
  end | Stack].

-compile({inline,{yeccpars2_189_,1}}).
-file("erl_parse.yrl", 268).
yeccpars2_189_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_190_,1}}).
-file("erl_parse.yrl", 265).
yeccpars2_190_([__3,__2,__1 | Stack]) ->
 [begin
   { bin , line ( __1 ) , __2 }
  end | Stack].

-compile({inline,{yeccpars2_193_,1}}).
-file("erl_parse.yrl", 294).
yeccpars2_193_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { bc , line ( __1 ) , __2 , __4 }
  end | Stack].

-compile({inline,{yeccpars2_194_,1}}).
-file("erl_parse.yrl", 280).
yeccpars2_194_(Stack) ->
 [begin
   default
  end | Stack].

-compile({inline,{yeccpars2_197_,1}}).
-file("erl_parse.yrl", 276).
yeccpars2_197_([__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_198_,1}}).
-file("erl_parse.yrl", 271).
yeccpars2_198_([__3,__2,__1 | Stack]) ->
 [begin
   { bin_element , line ( __1 ) , __1 , __2 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_200_,1}}).
-file("erl_parse.yrl", 279).
yeccpars2_200_([__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_201_,1}}).
-file("erl_parse.yrl", 283).
yeccpars2_201_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_202_,1}}).
-file("erl_parse.yrl", 285).
yeccpars2_202_([__1 | Stack]) ->
 [begin
   element ( 3 , __1 )
  end | Stack].

-compile({inline,{yeccpars2_204_,1}}).
-file("erl_parse.yrl", 286).
yeccpars2_204_([__3,__2,__1 | Stack]) ->
 [begin
   { element ( 3 , __1 ) , element ( 3 , __3 ) }
  end | Stack].

-compile({inline,{yeccpars2_206_,1}}).
-file("erl_parse.yrl", 282).
yeccpars2_206_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_207_,1}}).
-file("erl_parse.yrl", 273).
yeccpars2_207_([__2,__1 | Stack]) ->
 [begin
   mkop ( __1 , __2 )
  end | Stack].

-compile({inline,{yeccpars2_208_,1}}).
-file("erl_parse.yrl", 232).
yeccpars2_208_([__2,__1 | Stack]) ->
 [begin
   { record_field , line ( __1 ) , { atom , line ( __1 ) , '' } , __2 }
  end | Stack].

-compile({inline,{yeccpars2_210_,1}}).
-file("erl_parse.yrl", 245).
yeccpars2_210_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_212_,1}}).
-file("erl_parse.yrl", 317).
yeccpars2_212_([__3,__2,__1 | Stack]) ->
 [begin
   { record , line ( __1 ) , element ( 3 , __2 ) , __3 }
  end | Stack].

-compile({inline,{yeccpars2_216_,1}}).
-file("erl_parse.yrl", 326).
yeccpars2_216_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_219_,1}}).
-file("erl_parse.yrl", 323).
yeccpars2_219_([__2,__1 | Stack]) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_221_,1}}).
-file("erl_parse.yrl", 329).
yeccpars2_221_([__3,__2,__1 | Stack]) ->
 [begin
   { record_field , line ( __1 ) , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_223_,1}}).
-file("erl_parse.yrl", 330).
yeccpars2_223_([__3,__2,__1 | Stack]) ->
 [begin
   { record_field , line ( __1 ) , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_225_,1}}).
-file("erl_parse.yrl", 327).
yeccpars2_225_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_226_,1}}).
-file("erl_parse.yrl", 324).
yeccpars2_226_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_227_,1}}).
-file("erl_parse.yrl", 315).
yeccpars2_227_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { record_index , line ( __1 ) , element ( 3 , __2 ) , __4 }
  end | Stack].

-compile({inline,{yeccpars2_229_,1}}).
-file("erl_parse.yrl", 420).
yeccpars2_229_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_232_,1}}).
-file("erl_parse.yrl", 193).
yeccpars2_232_([__3,__2,__1 | Stack]) ->
 [begin
   { match , line ( __2 ) , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_233_,1}}).
-file("erl_parse.yrl", 194).
yeccpars2_233_([__3,__2,__1 | Stack]) ->
 [begin
   mkop ( __1 , __2 , __3 )
  end | Stack].

-compile({inline,{yeccpars2_235_,1}}).
-file("erl_parse.yrl", 197).
yeccpars2_235_([__3,__2,__1 | Stack]) ->
 [begin
   mkop ( __1 , __2 , __3 )
  end | Stack].

-compile({inline,{yeccpars2_237_,1}}).
-file("erl_parse.yrl", 200).
yeccpars2_237_([__3,__2,__1 | Stack]) ->
 [begin
   mkop ( __1 , __2 , __3 )
  end | Stack].

-compile({inline,{yeccpars2_247_,1}}).
-file("erl_parse.yrl", 204).
yeccpars2_247_([__3,__2,__1 | Stack]) ->
 [begin
   mkop ( __1 , __2 , __3 )
  end | Stack].

-compile({inline,{yeccpars2_260_,1}}).
-file("erl_parse.yrl", 212).
yeccpars2_260_([__3,__2,__1 | Stack]) ->
 [begin
   mkop ( __1 , __2 , __3 )
  end | Stack].

-compile({inline,{yeccpars2_268_,1}}).
-file("erl_parse.yrl", 216).
yeccpars2_268_([__3,__2,__1 | Stack]) ->
 [begin
   mkop ( __1 , __2 , __3 )
  end | Stack].

-compile({inline,{yeccpars2_269_,1}}).
-file("erl_parse.yrl", 208).
yeccpars2_269_([__3,__2,__1 | Stack]) ->
 [begin
   mkop ( __1 , __2 , __3 )
  end | Stack].

-compile({inline,{yeccpars2_270_,1}}).
-file("erl_parse.yrl", 335).
yeccpars2_270_([__2,__1 | Stack]) ->
 [begin
   { call , line ( __1 ) , __1 , element ( 1 , __2 ) }
  end | Stack].

-compile({inline,{yeccpars2_273_,1}}).
-file("erl_parse.yrl", 228).
yeccpars2_273_([__3,__2,__1 | Stack]) ->
 [begin
   { remote , line ( __2 ) , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_274_,1}}).
-file("erl_parse.yrl", 234).
yeccpars2_274_([__3,__2,__1 | Stack]) ->
 [begin
   { record_field , line ( __2 ) , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_277_,1}}).
-file("erl_parse.yrl", 321).
yeccpars2_277_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { record , line ( __2 ) , __1 , element ( 3 , __3 ) , __4 }
  end | Stack].

-compile({inline,{yeccpars2_279_,1}}).
-file("erl_parse.yrl", 319).
yeccpars2_279_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { record_field , line ( __2 ) , __1 , element ( 3 , __3 ) , __5 }
  end | Stack].

-compile({inline,{yeccpars2_280_,1}}).
-file("erl_parse.yrl", 416).
yeccpars2_280_([__3,__2,__1 | Stack]) ->
 [begin
   { __2 , line ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_281_,1}}).
-file("erl_parse.yrl", 220).
yeccpars2_281_([__2,__1 | Stack]) ->
 [begin
   mkop ( __1 , __2 )
  end | Stack].

-compile({inline,{yeccpars2_283_,1}}).
-file("erl_parse.yrl", 474).
yeccpars2_283_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { clause , line ( __1 ) , element ( 3 , __1 ) , __2 , __3 , __4 }
  end | Stack].

-compile({inline,{yeccpars2_284_,1}}).
-file("erl_parse.yrl", 179).
yeccpars2_284_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { clause , line ( __1 ) , element ( 3 , __1 ) , __2 , __3 , __4 }
  end | Stack].

-compile({inline,{yeccpars2_286_,1}}).
-file("erl_parse.yrl", 476).
yeccpars2_286_([__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_291_,1}}).
-file("erl_parse.yrl", 419).
yeccpars2_291_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_293_,1}}).
-file("erl_parse.yrl", 73).
yeccpars2_293_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   build_attribute ( __2 , __4 )
  end | Stack].

-compile({inline,{yeccpars2_296_,1}}).
-file("erl_parse.yrl", 77).
yeccpars2_296_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 , { type_sigs , __3 } ]
  end | Stack].

-compile({inline,{yeccpars2_297_,1}}).
-file("erl_parse.yrl", 89).
yeccpars2_297_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_299_,1}}).
-file("erl_parse.yrl", 78).
yeccpars2_299_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 , { type_def , __3 } ]
  end | Stack].

-compile({inline,{yeccpars2_314_,1}}).
-file("erl_parse.yrl", 102).
yeccpars2_314_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_315_,1}}).
-file("erl_parse.yrl", 117).
yeccpars2_315_([__2,__1 | Stack]) ->
 [begin
   { type , line ( __1 ) , tuple , [ ] }
  end | Stack].

-compile({inline,{yeccpars2_317_,1}}).
-file("erl_parse.yrl", 103).
yeccpars2_317_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_318_,1}}).
-file("erl_parse.yrl", 118).
yeccpars2_318_([__3,__2,__1 | Stack]) ->
 [begin
   { type , line ( __1 ) , tuple , __2 }
  end | Stack].

-compile({inline,{yeccpars2_323_,1}}).
-file("erl_parse.yrl", 126).
yeccpars2_323_([__3,__2,__1 | Stack]) ->
 [begin
   { type , line ( __1 ) , 'fun' , [ ] }
  end | Stack].

-compile({inline,{yeccpars2_326_,1}}).
-file("erl_parse.yrl", 145).
yeccpars2_326_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_331_,1}}).
-file("erl_parse.yrl", 148).
yeccpars2_331_([__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_336_,1}}).
-file("erl_parse.yrl", 134).
yeccpars2_336_([__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { type , line ( __1 ) , 'fun' ,
    [ { type , line ( __1 ) , any } , __7 ] }
  end | Stack].

-compile({inline,{yeccpars2_338_,1}}).
-file("erl_parse.yrl", 138).
yeccpars2_338_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { type , line ( __1 ) , 'fun' ,
    [ { type , line ( __1 ) , product , [ ] } , __4 ] }
  end | Stack].

-compile({inline,{yeccpars2_340_,1}}).
-file("erl_parse.yrl", 146).
yeccpars2_340_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_343_,1}}).
-file("erl_parse.yrl", 141).
yeccpars2_343_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { type , line ( __1 ) , 'fun' ,
    [ { type , line ( __1 ) , product , __2 } , __5 ] }
  end | Stack].

-compile({inline,{yeccpars2_344_,1}}).
-file("erl_parse.yrl", 127).
yeccpars2_344_([__4,__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_347_,1}}).
-file("erl_parse.yrl", 110).
yeccpars2_347_([__3,__2,__1 | Stack]) ->
 [begin
   build_gen_type ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_348_,1}}).
-file("erl_parse.yrl", 111).
yeccpars2_348_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { type , line ( __1 ) ,
    normalise ( __1 ) , __3 }
  end | Stack].

-compile({inline,{yeccpars2_350_,1}}).
-file("erl_parse.yrl", 113).
yeccpars2_350_([__2,__1 | Stack]) ->
 [begin
   { type , line ( __1 ) , nil , [ ] }
  end | Stack].

-compile({inline,{yeccpars2_352_,1}}).
-file("erl_parse.yrl", 114).
yeccpars2_352_([__3,__2,__1 | Stack]) ->
 [begin
   { type , line ( __1 ) , list , [ __2 ] }
  end | Stack].

-compile({inline,{yeccpars2_356_,1}}).
-file("erl_parse.yrl", 115).
yeccpars2_356_([__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { type , line ( __1 ) ,
    nonempty_list , [ __2 ] }
  end | Stack].

-compile({inline,{yeccpars2_359_,1}}).
-file("erl_parse.yrl", 157).
yeccpars2_359_([__2,__1 | Stack]) ->
 [begin
   { type , line ( __1 ) , binary ,
    [ abstract ( 0 , line ( __1 ) ) ,
    abstract ( 0 , line ( __1 ) ) ] }
  end | Stack].

-compile({inline,{yeccpars2_362_,1}}).
-file("erl_parse.yrl", 167).
yeccpars2_362_([__3,__2,__1 | Stack]) ->
 [begin
   build_bin_type ( [ __1 ] , __3 )
  end | Stack].

-compile({inline,{yeccpars2_365_,1}}).
-file("erl_parse.yrl", 169).
yeccpars2_365_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   build_bin_type ( [ __1 , __3 ] , __5 )
  end | Stack].

-compile({inline,{yeccpars2_367_,1}}).
-file("erl_parse.yrl", 160).
yeccpars2_367_([__3,__2,__1 | Stack]) ->
 [begin
   { type , line ( __1 ) , binary ,
    [ __2 , abstract ( 0 , line ( __1 ) ) ] }
  end | Stack].

-compile({inline,{yeccpars2_371_,1}}).
-file("erl_parse.yrl", 165).
yeccpars2_371_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { type , line ( __1 ) , binary , [ __2 , __4 ] }
  end | Stack].

-compile({inline,{yeccpars2_372_,1}}).
-file("erl_parse.yrl", 162).
yeccpars2_372_([__3,__2,__1 | Stack]) ->
 [begin
   { type , line ( __1 ) , binary ,
    [ abstract ( 0 , line ( __1 ) ) , __2 ] }
  end | Stack].

-compile({inline,{yeccpars2_373_,1}}).
-file("erl_parse.yrl", 130).
yeccpars2_373_([__2,__1 | Stack]) ->
 [begin
   abstract ( - normalise ( __2 ) ,
    line ( __2 ) )
  end | Stack].

-compile({inline,{yeccpars2_377_,1}}).
-file("erl_parse.yrl", 151).
yeccpars2_377_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_379_,1}}).
-file("erl_parse.yrl", 119).
yeccpars2_379_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { type , line ( __1 ) , record , [ __2 ] }
  end | Stack].

-compile({inline,{yeccpars2_381_,1}}).
-file("erl_parse.yrl", 154).
yeccpars2_381_([__3,__2,__1 | Stack]) ->
 [begin
   { type , line ( __1 ) , field_type ,
    [ __1 , __3 ] }
  end | Stack].

-compile({inline,{yeccpars2_383_,1}}).
-file("erl_parse.yrl", 152).
yeccpars2_383_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_384_,1}}).
-file("erl_parse.yrl", 120).
yeccpars2_384_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { type , line ( __1 ) ,
    record , [ __2 | __4 ] }
  end | Stack].

-compile({inline,{yeccpars2_386_,1}}).
-file("erl_parse.yrl", 93).
yeccpars2_386_([__3,__2,__1 | Stack]) ->
 [begin
   { type , line ( __1 ) , bounded_fun ,
    [ __1 , __3 ] }
  end | Stack].

-compile({inline,{yeccpars2_387_,1}}).
-file("erl_parse.yrl", 96).
yeccpars2_387_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_391_,1}}).
-file("erl_parse.yrl", 99).
yeccpars2_391_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { type , line ( __1 ) , constraint ,
    [ __1 , __3 ] }
  end | Stack].

-compile({inline,{yeccpars2_393_,1}}).
-file("erl_parse.yrl", 97).
yeccpars2_393_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_396_,1}}).
-file("erl_parse.yrl", 124).
yeccpars2_396_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { type , line ( __1 ) , range ,
    [ __1 , __4 ] }
  end | Stack].

-compile({inline,{yeccpars2_398_,1}}).
-file("erl_parse.yrl", 105).
yeccpars2_398_([__3,__2,__1 | Stack]) ->
 [begin
   lift_unions ( __1 , __3 )
  end | Stack].

-compile({inline,{yeccpars2_400_,1}}).
-file("erl_parse.yrl", 90).
yeccpars2_400_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_401_,1}}).
-file("erl_parse.yrl", 76).
yeccpars2_401_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 , { typed_record , __3 } ]
  end | Stack].

-compile({inline,{yeccpars2_404_,1}}).
-file("erl_parse.yrl", 82).
yeccpars2_404_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_405_,1}}).
-file("erl_parse.yrl", 419).
yeccpars2_405_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_408_,1}}).
-file("erl_parse.yrl", 87).
yeccpars2_408_([__3,__2,__1 | Stack]) ->
 [begin
   { typed , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_409_,1}}).
-file("erl_parse.yrl", 84).
yeccpars2_409_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_411_,1}}).
-file("erl_parse.yrl", 83).
yeccpars2_411_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_412_,1}}).
-file("erl_parse.yrl", 85).
yeccpars2_412_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_413_,1}}).
-file("erl_parse.yrl", 80).
yeccpars2_413_([__3,__2,__1 | Stack]) ->
 [begin
   { tuple , line ( __1 ) , __2 }
  end | Stack].

-compile({inline,{yeccpars2_414_,1}}).
-file("erl_parse.yrl", 74).
yeccpars2_414_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   build_typed_attribute ( __2 , __4 )
  end | Stack].

-compile({inline,{yeccpars2_415_,1}}).
-file("erl_parse.yrl", 69).
yeccpars2_415_([__2,__1 | Stack]) ->
 [begin
   __1
  end | Stack].

-compile({inline,{yeccpars2_416_,1}}).
-file("erl_parse.yrl", 70).
yeccpars2_416_([__2,__1 | Stack]) ->
 [begin
   __1
  end | Stack].

-compile({inline,{yeccpars2_418_,1}}).
-file("erl_parse.yrl", 176).
yeccpars2_418_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_420_,1}}).
-file("erl_parse.yrl", 185).
yeccpars2_420_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_422_,1}}).
-file("erl_parse.yrl", 71).
yeccpars2_422_([__2,__1 | Stack]) ->
 [begin
   __1
  end | Stack].

-compile({inline,{yeccpars2_424_,1}}).
-file("erl_parse.yrl", 471).
yeccpars2_424_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_426_,1}}).
-file("erl_parse.yrl", 185).
yeccpars2_426_(Stack) ->
 [begin
   [ ]
  end | Stack].


-file("erl_parse.yrl", 955).
