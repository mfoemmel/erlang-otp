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
%% Portions created by Ericsson are Copyright 2000, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%

-module(xref_scanner).

-export([scan/1]).

scan(Chars) ->
    case erl_scan:string(Chars) of
	{ok, Tokens, _Line}  ->
	    {ok, lex(Tokens)};
	{error, {Line,Module,Info}, _EndLine} ->
	    {error, apply(Module, format_error, [Info]), Line}
    end.

-define(MFA(M,F,A,N), {atom,N,M}, {':',N}, {atom,N,F}, {'/',N}, {integer,N,A}).
-define(MFA2(M,F,A,N), 
	{'{',N},{atom,N,M},{',',N},{atom,N,F},{',',N},{integer,N,A},{'}',N}).
-define(DECL(N,T), {':',N},{var,N,T}).

lex([{atom,N,V1},{'->',N},{atom,N,V2} | L]) ->
    Constant = {constant, unknown, edge, {V1,V2}},
    [{edge,N,Constant} | lex(L)];
lex([{'{',N},{atom,N,V1},{',',N},{atom,N,V2},{'}',N} | L]) ->
    Constant = {constant, unknown, edge, {V1,V2}},
    [{edge,N,Constant} | lex(L)];
lex([?MFA(M,F,A,N),{'->',N},?MFA(M2,F2,A2,N) | L]) ->
    Constant = {constant, 'Fun', edge, {{M,F,A},{M2,F2,A2}}},
    [{edge,N,Constant} | lex(L)];
lex([?MFA(M,F,A,N) | L]) ->
    Constant = {constant, 'Fun', vertex, {M,F,A}},
    [{vertex,N,Constant} | lex(L)];
lex([{'{',N},?MFA2(M,F,A,N),{',',N},?MFA2(M2,F2,A2,N),{'}',N} | L]) ->
    Constant = {constant, 'Fun', edge, {{M,F,A},{M2,F2,A2}}},
    [{edge,N,Constant} | lex(L)];
lex([?MFA2(M,F,A,N) | L]) ->
    Constant = {constant, 'Fun', vertex, {M,F,A}},
    [{vertex,N,Constant} | lex(L)];
lex([?DECL(N,Decl) | L]) ->
    case is_type(Decl) of
	false -> [?DECL(N, Decl) | lex(L)];
	true -> [{decl,N,Decl} | lex(L)]
    end;
lex([{':',N},{'=',N} | L]) ->
    [{':=',N} | lex(L)];
lex([{'||',N},{'|',N} | L]) ->
    [{'|||',N} | lex(L)];
lex([V={var,N,Var} | L]) ->
    T = case is_type(Var) of
	    false -> V;
	    true -> {cast,N,Var}
	end,
    [T | lex(L)];
lex([T | Ts]) ->
    [T | lex(Ts)];
lex([]) ->
    [{'$end', at_end}].

is_type('Rel')         -> true;
is_type('App')         -> true;
is_type('Mod')         -> true;
is_type('Fun')         -> true;
is_type('Lin')         -> true;
is_type('LLin')        -> true;
is_type('XLin')        -> true;
is_type('ELin')        -> true;
is_type('XXL')         -> true;
is_type(_)             -> false.
