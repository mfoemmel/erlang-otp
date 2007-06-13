-module(edoc_parser).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("edoc_parser.yrl", 200).

%% ========================== -*-Erlang-*- =============================
%% EDoc function specification parser, generated from the file
%% "edoc_parser.yrl" by the Yecc parser generator.
%%
%% Copyright (C) 2002-2005 Richard Carlsson
%%
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%% ====================================================================

-export([parse_spec/2, parse_typedef/2, parse_throws/2, parse_ref/2,
	 parse_see/2, parse_param/2]).

-include("edoc_types.hrl").

%% Multiple entry point hack:

start_spec(Ts, L) -> run_parser(Ts, L, start_spec).

start_typedef(Ts, L) -> run_parser(Ts, L, start_typedef).

start_throws(Ts, L) -> run_parser(Ts, L, start_throws).

start_ref(Ts, L) -> run_parser(Ts, L, start_ref).

%% Error reporting fix

run_parser(Ts, L, Start) ->
    case parse([{Start,L} | Ts]) of
	{error, {999999,?MODULE,_}} ->
	    What = case Start of
		       start_spec -> "specification";
		       start_typedef -> "type definition";
		       start_throws -> "exception declaration";
		       start_ref -> "reference"
		   end,
	    {error, {L,?MODULE,["unexpected end of ", What]}};
	Other -> Other
    end.

%% Utility functions:

tok_val(T) -> element(3, T).

tok_line(T) -> element(2, T).

qname([A]) ->
    A;    % avoid unnecessary call to packages:concat/1.
qname(List) ->
    list_to_atom(packages:concat(lists:reverse(List))).

union(Ts) ->
    case Ts of
	[T] -> T;
	_ -> #t_union{types = lists:reverse(Ts)}
    end.

annotate(T, A) -> ?add_t_ann(T, A).
    
%% ---------------------------------------------------------------------

%% @doc EDoc type specification parsing. Parses the content of
%% <a href="overview-summary.html#ftag-spec">`@spec'</a> declarations.

parse_spec(S, L) ->
    case edoc_scanner:string(S, L) of
	{ok, Ts, _} ->
	    case start_spec(Ts, L) of
		{ok, Spec} ->
		    Spec;
		{error, E} ->
		    throw_error(E, L)
	    end;
	{error, E, _} ->
	    throw_error(E, L)
    end.

%% ---------------------------------------------------------------------

%% @doc EDoc type definition parsing. Parses the content of
%% <a href="overview-summary.html#gtag-type">`@type'</a> declarations.

parse_typedef(S, L) ->
    {S1, S2} = edoc_lib:split_at_stop(S),
    N = edoc_lib:count($\n, S1),
    L1 = L + N,
    Text = edoc_lib:strip_space(S2),
    {parse_typedef_1(S1, L), edoc_wiki:parse_xml(Text, L1)}.

parse_typedef_1(S, L) ->
    case edoc_scanner:string(S, L) of
	{ok, Ts, _} ->
	    case start_typedef(Ts, L) of
		{ok, T} ->
		    T;
		{error, E} ->
		    throw_error({parse_typedef, E}, L)
	    end;
	{error, E, _} ->
	    throw_error({parse_typedef, E}, L)
    end.

%% ---------------------------------------------------------------------

%% @doc Parses a <a
%% href="overview-summary.html#References">reference</a> to a module,
%% package, function, type, or application

parse_ref(S, L) ->
    case edoc_scanner:string(S, L) of
	{ok, Ts, _} ->
	    case start_ref(Ts, L) of
		{ok, T} ->
		    T;
		{error, E} ->
		    throw_error({parse_ref, E}, L)
	    end;
	{error, E, _} ->
	    throw_error({parse_ref, E}, L)
    end.

%% ---------------------------------------------------------------------

%% @doc Parses the content of
%% <a href="overview-summary.html#ftag-see">`@see'</a> references.
parse_see(S, L) ->
    {S1, S2} = edoc_lib:split_at_stop(S),
    N = edoc_lib:count($\n, S1),
    L1 = L + N,
    Text = edoc_lib:strip_space(S2),
    {parse_ref(S1, L), edoc_wiki:parse_xml(Text, L1)}.

%% ---------------------------------------------------------------------

%% @doc Parses the content of
%% <a href="overview-summary.html#ftag-param">`@param'</a> tags.
parse_param(S, L) ->
    {S1, S2} = edoc_lib:split_at_space(edoc_lib:strip_space(S)),
    case edoc_lib:strip_space(S1) of
	"" -> throw_error(parse_param, L);
	Name -> 
	    Text = edoc_lib:strip_space(S2),
	    {list_to_atom(Name), edoc_wiki:parse_xml(Text, L)}
    end.

%% ---------------------------------------------------------------------

%% @doc EDoc exception specification parsing. Parses the content of
%% <a href="overview-summary.html#ftag-throws">`@throws'</a> declarations.

parse_throws(S, L) ->
    case edoc_scanner:string(S, L) of
	{ok, Ts, _} ->
	    case start_throws(Ts, L) of
		{ok, Spec} ->
		    Spec;
		{error, E} ->
		    throw_error({parse_throws, E}, L)
	    end;
	{error, E, _} ->
	    throw_error({parse_throws, E}, L)
    end.

%% ---------------------------------------------------------------------

throw_error({L, M, D}, _L0) ->
    throw({error,L,{format_error,M,D}});
throw_error({parse_spec, E}, L) ->
    throw_error({"specification", E}, L);
throw_error({parse_typedef, E}, L) ->
    throw_error({"type definition", E}, L);
throw_error({parse_ref, E}, L) ->
    throw_error({"reference", E}, L);
throw_error({parse_throws, E}, L) ->
    throw_error({"throws-declaration", E}, L);
throw_error(parse_param, L) ->
    throw({error, L, "missing parameter name"});
throw_error({Where, E}, L) when is_list(Where) ->
    throw({error,L,{"unknown error parsing ~s: ~P.",[Where,E,15]}});
throw_error(E, L) ->
    %% Just in case.
    throw({error,L,{"unknown parse error: ~P.",[E,15]}}).

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



-file("./edoc_parser.erl", 293).

yeccpars2(0, start_ref, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, start_spec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, start_throws, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, start_typedef, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(1, '$end', _, __Stack, _, _, _) ->
 {ok, hd(__Stack)};
yeccpars2(1, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(2, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 114, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 115, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(3, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 102, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(4, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(5, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(6, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_6_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(start, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(7, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(8, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [8 | __Ss], [__T | __Stack]);
yeccpars2(8, where, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [8 | __Ss], [__T | __Stack]);
yeccpars2(8, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_8_(__Stack),
 yeccpars2(17, __Cat, [8 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(9, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(10, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [10 | __Ss], [__T | __Stack]);
yeccpars2(10, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [10 | __Ss], [__T | __Stack]);
yeccpars2(10, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(11, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_11_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(var_list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(12, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_12_(__Stack),
 yeccpars2(yeccgoto(vars, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(13, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_13_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(var_list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(14, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(15, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_15_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(vars, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(16, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_16_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(typedef, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(17, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(where_defs, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(18, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(19, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_19_(__Stack),
 yeccpars2(20, __Cat, [19 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(20, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_20_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(where_defs, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(21, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_21_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(defs, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(22, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [22 | __Ss], [__T | __Stack]);
yeccpars2(22, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [22 | __Ss], [__T | __Stack]);
yeccpars2(22, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(23, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(24, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(25, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(26, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_26_(__Stack),
 yeccpars2(yeccgoto(ptype, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(27, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 86, [27 | __Ss], [__T | __Stack]);
yeccpars2(27, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_27_(__Stack),
 yeccpars2(yeccgoto(ptype, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(28, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_28_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(def, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(29, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 83, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(30, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [30 | __Ss], [__T | __Stack]);
yeccpars2(30, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [30 | __Ss], [__T | __Stack]);
yeccpars2(30, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_30_(__Stack),
 yeccpars2(yeccgoto(nutype, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(31, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_31_(__Stack),
 yeccpars2(yeccgoto(ptypes, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(32, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 82, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(utype, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(33, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(34, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(35, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(36, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(37, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, '$end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_37_$end'(__Stack),
 yeccpars2(yeccgoto(ptype, hd(__Ss)), '$end', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(37, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_37_)'(__Stack),
 yeccpars2(yeccgoto(ptype, hd(__Ss)), ')', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(37, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_37_+'(__Stack),
 yeccpars2(yeccgoto(ptype, hd(__Ss)), '+', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(37, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_37_,'(__Stack),
 yeccpars2(yeccgoto(ptype, hd(__Ss)), ',', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(37, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_37_]'(__Stack),
 yeccpars2(yeccgoto(ptype, hd(__Ss)), ']', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(37, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_37_atom(__Stack),
 yeccpars2(yeccgoto(ptype, hd(__Ss)), atom, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(37, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_37_string(__Stack),
 yeccpars2(yeccgoto(ptype, hd(__Ss)), string, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(37, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_37_var(__Stack),
 yeccpars2(yeccgoto(ptype, hd(__Ss)), var, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(37, where, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_37_where(__Stack),
 yeccpars2(yeccgoto(ptype, hd(__Ss)), where, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(37, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_37_|'(__Stack),
 yeccpars2(yeccgoto(ptype, hd(__Ss)), '|', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(37, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_37_}'(__Stack),
 yeccpars2(yeccgoto(ptype, hd(__Ss)), '}', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(37, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_37_(__Stack),
 yeccpars2(yeccgoto(qname, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(38, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_38_(__Stack),
 yeccpars2(yeccgoto(ptype, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(39, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_39_(__Stack),
 yeccpars2(yeccgoto(ptype, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(40, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_40_(__Stack),
 yeccpars2(yeccgoto(ptype, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(41, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(42, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [42 | __Ss], [__T | __Stack]);
yeccpars2(42, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [42 | __Ss], [__T | __Stack]);
yeccpars2(42, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(43, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_43_(__Stack),
 yeccpars2(yeccgoto(utypes, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(44, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_44_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(utype_tuple, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(45, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(46, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_46_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(utype_tuple, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(47, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_47_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(utypes, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(48, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(49, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [49 | __Ss], [__T | __Stack]);
yeccpars2(49, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [49 | __Ss], [__T | __Stack]);
yeccpars2(49, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_49_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(nutype, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(50, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_50_(__Stack),
 yeccpars2(yeccgoto(ptype, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(51, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(52, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(53, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_53_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(ptypes, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(54, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_54_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(ptypes, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(55, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_55_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(ptype, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(56, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(57, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_57_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(ptype, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(58, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_58_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(ptype, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(59, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(60, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(61, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [61 | __Ss], [__T | __Stack]);
yeccpars2(61, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [61 | __Ss], [__T | __Stack]);
yeccpars2(61, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(62, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_62_(__Stack),
 yeccpars2(yeccgoto(qname, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(63, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(64, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(65, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(66, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_66_(__Stack),
 __Nss = lists:nthtail(6, __Ss),
 yeccpars2(yeccgoto(ptype, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(67, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_67_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(qname, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(68, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(69, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_69_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(utype_list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(70, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_70_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(utype_list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(71, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [71 | __Ss], [__T | __Stack]);
yeccpars2(71, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(72, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(73, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 79, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 80, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(74, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_74_(__Stack),
 yeccpars2(yeccgoto(fields, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(75, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(76, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_76_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(ptype, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(77, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(78, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_78_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(field, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(79, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(80, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_80_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(ptype, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(81, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_81_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(fields, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(82, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_82_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(utype, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(83, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 84, [83 | __Ss], [__T | __Stack]);
yeccpars2(83, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(84, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(85, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_85_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(ptype, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(86, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(87, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_87_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(ptype, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(88, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 89, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(89, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(90, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_90_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(def, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(91, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_91_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(defs, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(92, where, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_92_(__Stack),
 yeccpars2(17, __Cat, [92 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(93, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_93_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(typedef, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(94, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(etype, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(95, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_95_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(start, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(96, where, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_96_(__Stack),
 yeccpars2(17, __Cat, [96 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(97, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_97_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(throws, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(98, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 106, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(99, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_99_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(start, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(100, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(101, where, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_101_(__Stack),
 yeccpars2(17, __Cat, [101 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(102, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_102_(__Stack),
 yeccpars2(yeccgoto(function_name, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(103, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_103_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(spec, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(104, where, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_104_(__Stack),
 yeccpars2(17, __Cat, [104 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(105, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_105_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(spec, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(106, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(107, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_107_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(func_type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(108, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_108_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(start, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(109, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 124, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 125, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_109_(__Stack),
 yeccpars2(yeccgoto(mref, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(110, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(ref, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(111, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(ref, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(112, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(ref, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(113, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(ref, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(114, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 120, [114 | __Ss], [__T | __Stack]);
yeccpars2(114, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(115, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 116, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_115_(__Stack),
 yeccpars2(yeccgoto(qname, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(116, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 119, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(117, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(118, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_118_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(lref, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(119, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_119_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(lref, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(120, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 121, [120 | __Ss], [__T | __Stack]);
yeccpars2(120, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_120_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(aref, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(121, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(122, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_122_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(aref, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(123, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_123_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(aref, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(124, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(125, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(126, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [126 | __Ss], [__T | __Stack]);
yeccpars2(126, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [126 | __Ss], [__T | __Stack]);
yeccpars2(126, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(127, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(128, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [128 | __Ss], [__T | __Stack]);
yeccpars2(128, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(129, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_129_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(mref, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(130, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_130_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(mref, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(131, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_131_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(pref, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(__Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.1",{missing_state_in_action_table, __Other}}).

yeccgoto(aref, 2) ->
 113;
yeccgoto(def, 17) ->
 21;
yeccgoto(def, 20) ->
 21;
yeccgoto(def, 22) ->
 91;
yeccgoto(defs, 8) ->
 17;
yeccgoto(defs, 19) ->
 20;
yeccgoto(defs, 92) ->
 17;
yeccgoto(defs, 96) ->
 17;
yeccgoto(defs, 101) ->
 17;
yeccgoto(defs, 104) ->
 17;
yeccgoto(etype, 4) ->
 96;
yeccgoto(field, 72) ->
 74;
yeccgoto(field, 79) ->
 81;
yeccgoto(fields, 72) ->
 73;
yeccgoto(func_type, 3) ->
 101;
yeccgoto(func_type, 100) ->
 104;
yeccgoto(function_name, 3) ->
 100;
yeccgoto(lref, 2) ->
 112;
yeccgoto(mref, 2) ->
 111;
yeccgoto(mref, 121) ->
 123;
yeccgoto(nutype, 4) ->
 32;
yeccgoto(nutype, 18) ->
 32;
yeccgoto(nutype, 25) ->
 32;
yeccgoto(nutype, 34) ->
 32;
yeccgoto(nutype, 36) ->
 32;
yeccgoto(nutype, 41) ->
 32;
yeccgoto(nutype, 45) ->
 32;
yeccgoto(nutype, 77) ->
 32;
yeccgoto(nutype, 89) ->
 32;
yeccgoto(nutype, 106) ->
 32;
yeccgoto(pref, 2) ->
 110;
yeccgoto(pref, 121) ->
 122;
yeccgoto(ptype, 4) ->
 31;
yeccgoto(ptype, 18) ->
 31;
yeccgoto(ptype, 25) ->
 31;
yeccgoto(ptype, 34) ->
 31;
yeccgoto(ptype, 36) ->
 31;
yeccgoto(ptype, 41) ->
 31;
yeccgoto(ptype, 45) ->
 31;
yeccgoto(ptype, 48) ->
 31;
yeccgoto(ptype, 51) ->
 54;
yeccgoto(ptype, 52) ->
 53;
yeccgoto(ptype, 77) ->
 31;
yeccgoto(ptype, 86) ->
 87;
yeccgoto(ptype, 89) ->
 31;
yeccgoto(ptype, 106) ->
 31;
yeccgoto(ptypes, 4) ->
 30;
yeccgoto(ptypes, 18) ->
 30;
yeccgoto(ptypes, 25) ->
 30;
yeccgoto(ptypes, 34) ->
 30;
yeccgoto(ptypes, 36) ->
 30;
yeccgoto(ptypes, 41) ->
 30;
yeccgoto(ptypes, 45) ->
 30;
yeccgoto(ptypes, 48) ->
 49;
yeccgoto(ptypes, 77) ->
 30;
yeccgoto(ptypes, 89) ->
 30;
yeccgoto(ptypes, 106) ->
 30;
yeccgoto(qname, 2) ->
 109;
yeccgoto(qname, 4) ->
 29;
yeccgoto(qname, 18) ->
 29;
yeccgoto(qname, 25) ->
 29;
yeccgoto(qname, 34) ->
 29;
yeccgoto(qname, 36) ->
 29;
yeccgoto(qname, 41) ->
 29;
yeccgoto(qname, 45) ->
 29;
yeccgoto(qname, 48) ->
 29;
yeccgoto(qname, 51) ->
 29;
yeccgoto(qname, 52) ->
 29;
yeccgoto(qname, 60) ->
 61;
yeccgoto(qname, 77) ->
 29;
yeccgoto(qname, 86) ->
 29;
yeccgoto(qname, 89) ->
 29;
yeccgoto(qname, 106) ->
 29;
yeccgoto(qname, 121) ->
 109;
yeccgoto(ref, 2) ->
 108;
yeccgoto(spec, 3) ->
 99;
yeccgoto(start, 0) ->
 1;
yeccgoto(throws, 4) ->
 95;
yeccgoto(typedef, 5) ->
 6;
yeccgoto(utype, 4) ->
 94;
yeccgoto(utype, 18) ->
 92;
yeccgoto(utype, 25) ->
 28;
yeccgoto(utype, 34) ->
 43;
yeccgoto(utype, 36) ->
 56;
yeccgoto(utype, 41) ->
 43;
yeccgoto(utype, 45) ->
 47;
yeccgoto(utype, 77) ->
 78;
yeccgoto(utype, 89) ->
 90;
yeccgoto(utype, 106) ->
 107;
yeccgoto(utype_list, 3) ->
 98;
yeccgoto(utype_list, 4) ->
 27;
yeccgoto(utype_list, 18) ->
 27;
yeccgoto(utype_list, 25) ->
 27;
yeccgoto(utype_list, 34) ->
 27;
yeccgoto(utype_list, 36) ->
 27;
yeccgoto(utype_list, 37) ->
 55;
yeccgoto(utype_list, 41) ->
 27;
yeccgoto(utype_list, 45) ->
 27;
yeccgoto(utype_list, 48) ->
 27;
yeccgoto(utype_list, 51) ->
 27;
yeccgoto(utype_list, 52) ->
 27;
yeccgoto(utype_list, 65) ->
 66;
yeccgoto(utype_list, 77) ->
 27;
yeccgoto(utype_list, 84) ->
 85;
yeccgoto(utype_list, 86) ->
 27;
yeccgoto(utype_list, 89) ->
 27;
yeccgoto(utype_list, 100) ->
 98;
yeccgoto(utype_list, 106) ->
 27;
yeccgoto(utype_tuple, 4) ->
 26;
yeccgoto(utype_tuple, 18) ->
 26;
yeccgoto(utype_tuple, 25) ->
 26;
yeccgoto(utype_tuple, 34) ->
 26;
yeccgoto(utype_tuple, 36) ->
 26;
yeccgoto(utype_tuple, 41) ->
 26;
yeccgoto(utype_tuple, 45) ->
 26;
yeccgoto(utype_tuple, 48) ->
 26;
yeccgoto(utype_tuple, 51) ->
 26;
yeccgoto(utype_tuple, 52) ->
 26;
yeccgoto(utype_tuple, 77) ->
 26;
yeccgoto(utype_tuple, 86) ->
 26;
yeccgoto(utype_tuple, 89) ->
 26;
yeccgoto(utype_tuple, 106) ->
 26;
yeccgoto(utypes, 34) ->
 68;
yeccgoto(utypes, 41) ->
 42;
yeccgoto(var_list, 7) ->
 8;
yeccgoto(var_list, 23) ->
 88;
yeccgoto(vars, 9) ->
 10;
yeccgoto(where_defs, 8) ->
 16;
yeccgoto(where_defs, 92) ->
 93;
yeccgoto(where_defs, 96) ->
 97;
yeccgoto(where_defs, 101) ->
 103;
yeccgoto(where_defs, 104) ->
 105;
yeccgoto(__Symbol, __State) ->
 erlang:error({yecc_bug,"1.1",{__Symbol, __State, missing_in_goto_table}}).

-compile({inline,{yeccpars2_6_,1}}).
-file("edoc_parser.yrl", 44).
yeccpars2_6_([__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_8_,1}}).
-file("edoc_parser.yrl", 131).
yeccpars2_8_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_11_,1}}).
-file("edoc_parser.yrl", 143).
yeccpars2_11_([__2,__1 | __Stack]) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_12_,1}}).
-file("edoc_parser.yrl", 147).
yeccpars2_12_([__1 | __Stack]) ->
 [begin
   [ # t_var { name = tok_val ( __1 ) } ]
  end | __Stack].

-compile({inline,{yeccpars2_13_,1}}).
-file("edoc_parser.yrl", 144).
yeccpars2_13_([__3,__2,__1 | __Stack]) ->
 [begin
   lists : reverse ( __2 )
  end | __Stack].

-compile({inline,{yeccpars2_15_,1}}).
-file("edoc_parser.yrl", 148).
yeccpars2_15_([__3,__2,__1 | __Stack]) ->
 [begin
   [ # t_var { name = tok_val ( __3 ) } | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_16_,1}}).
-file("edoc_parser.yrl", 151).
yeccpars2_16_([__3,__2,__1 | __Stack]) ->
 [begin
   # t_typedef { name = # t_name { name = tok_val ( __1 ) } ,
    args = __2 ,
    defs = lists : reverse ( __3 ) }
  end | __Stack].

-compile({inline,{yeccpars2_19_,1}}).
-file("edoc_parser.yrl", 131).
yeccpars2_19_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_20_,1}}).
-file("edoc_parser.yrl", 56).
yeccpars2_20_([__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_21_,1}}).
-file("edoc_parser.yrl", 132).
yeccpars2_21_([__2,__1 | __Stack]) ->
 [begin
   [ __2 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_26_,1}}).
-file("edoc_parser.yrl", 91).
yeccpars2_26_([__1 | __Stack]) ->
 [begin
   # t_tuple { types = __1 }
  end | __Stack].

-compile({inline,{yeccpars2_27_,1}}).
-file("edoc_parser.yrl", 95).
yeccpars2_27_([__1 | __Stack]) ->
 [begin
   if length ( element ( 1 , __1 ) ) == 1 ->
   
    hd ( element ( 1 , __1 ) ) ;
    length ( element ( 1 , __1 ) ) == 0 ->
    return_error ( element ( 2 , __1 ) , "syntax error before: ')'" ) ;
    true ->
    return_error ( element ( 2 , __1 ) , "syntax error before: ','" )
    end
  end | __Stack].

-compile({inline,{yeccpars2_28_,1}}).
-file("edoc_parser.yrl", 136).
yeccpars2_28_([__3,__2,__1 | __Stack]) ->
 [begin
   # t_def { name = # t_var { name = tok_val ( __1 ) } ,
    type = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_30_,1}}).
-file("edoc_parser.yrl", 80).
yeccpars2_30_([__1 | __Stack]) ->
 [begin
   union ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_31_,1}}).
-file("edoc_parser.yrl", 83).
yeccpars2_31_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{'yeccpars2_37_$end',1}}).
-file("edoc_parser.yrl", 88).
'yeccpars2_37_$end'([__1 | __Stack]) ->
 [begin
   # t_atom { val = tok_val ( __1 ) }
  end | __Stack].

-compile({inline,{'yeccpars2_37_)',1}}).
-file("edoc_parser.yrl", 88).
'yeccpars2_37_)'([__1 | __Stack]) ->
 [begin
   # t_atom { val = tok_val ( __1 ) }
  end | __Stack].

-compile({inline,{'yeccpars2_37_+',1}}).
-file("edoc_parser.yrl", 88).
'yeccpars2_37_+'([__1 | __Stack]) ->
 [begin
   # t_atom { val = tok_val ( __1 ) }
  end | __Stack].

-compile({inline,{'yeccpars2_37_,',1}}).
-file("edoc_parser.yrl", 88).
'yeccpars2_37_,'([__1 | __Stack]) ->
 [begin
   # t_atom { val = tok_val ( __1 ) }
  end | __Stack].

-compile({inline,{'yeccpars2_37_]',1}}).
-file("edoc_parser.yrl", 88).
'yeccpars2_37_]'([__1 | __Stack]) ->
 [begin
   # t_atom { val = tok_val ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_37_atom,1}}).
-file("edoc_parser.yrl", 88).
yeccpars2_37_atom([__1 | __Stack]) ->
 [begin
   # t_atom { val = tok_val ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_37_string,1}}).
-file("edoc_parser.yrl", 88).
yeccpars2_37_string([__1 | __Stack]) ->
 [begin
   # t_atom { val = tok_val ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_37_var,1}}).
-file("edoc_parser.yrl", 88).
yeccpars2_37_var([__1 | __Stack]) ->
 [begin
   # t_atom { val = tok_val ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_37_where,1}}).
-file("edoc_parser.yrl", 88).
yeccpars2_37_where([__1 | __Stack]) ->
 [begin
   # t_atom { val = tok_val ( __1 ) }
  end | __Stack].

-compile({inline,{'yeccpars2_37_|',1}}).
-file("edoc_parser.yrl", 88).
'yeccpars2_37_|'([__1 | __Stack]) ->
 [begin
   # t_atom { val = tok_val ( __1 ) }
  end | __Stack].

-compile({inline,{'yeccpars2_37_}',1}}).
-file("edoc_parser.yrl", 88).
'yeccpars2_37_}'([__1 | __Stack]) ->
 [begin
   # t_atom { val = tok_val ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_37_,1}}).
-file("edoc_parser.yrl", 48).
yeccpars2_37_([__1 | __Stack]) ->
 [begin
   [ tok_val ( __1 ) ]
  end | __Stack].

-compile({inline,{yeccpars2_38_,1}}).
-file("edoc_parser.yrl", 90).
yeccpars2_38_([__1 | __Stack]) ->
 [begin
   # t_float { val = tok_val ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_39_,1}}).
-file("edoc_parser.yrl", 89).
yeccpars2_39_([__1 | __Stack]) ->
 [begin
   # t_integer { val = tok_val ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_40_,1}}).
-file("edoc_parser.yrl", 87).
yeccpars2_40_([__1 | __Stack]) ->
 [begin
   # t_var { name = tok_val ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_43_,1}}).
-file("edoc_parser.yrl", 73).
yeccpars2_43_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_44_,1}}).
-file("edoc_parser.yrl", 69).
yeccpars2_44_([__2,__1 | __Stack]) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_46_,1}}).
-file("edoc_parser.yrl", 70).
yeccpars2_46_([__3,__2,__1 | __Stack]) ->
 [begin
   lists : reverse ( __2 )
  end | __Stack].

-compile({inline,{yeccpars2_47_,1}}).
-file("edoc_parser.yrl", 74).
yeccpars2_47_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __3 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_49_,1}}).
-file("edoc_parser.yrl", 79).
yeccpars2_49_([__3,__2,__1 | __Stack]) ->
 [begin
   annotate ( union ( __3 ) , tok_val ( __1 ) )
  end | __Stack].

-compile({inline,{yeccpars2_50_,1}}).
-file("edoc_parser.yrl", 87).
yeccpars2_50_([__1 | __Stack]) ->
 [begin
   # t_var { name = tok_val ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_53_,1}}).
-file("edoc_parser.yrl", 85).
yeccpars2_53_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __3 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_54_,1}}).
-file("edoc_parser.yrl", 84).
yeccpars2_54_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __3 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_55_,1}}).
-file("edoc_parser.yrl", 111).
yeccpars2_55_([__2,__1 | __Stack]) ->
 [begin
   # t_type { name = # t_name { name = tok_val ( __1 ) } ,
    args = element ( 1 , __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_57_,1}}).
-file("edoc_parser.yrl", 92).
yeccpars2_57_([__2,__1 | __Stack]) ->
 [begin
   # t_nil { }
  end | __Stack].

-compile({inline,{yeccpars2_58_,1}}).
-file("edoc_parser.yrl", 93).
yeccpars2_58_([__3,__2,__1 | __Stack]) ->
 [begin
   # t_list { type = __2 }
  end | __Stack].

-compile({inline,{yeccpars2_62_,1}}).
-file("edoc_parser.yrl", 48).
yeccpars2_62_([__1 | __Stack]) ->
 [begin
   [ tok_val ( __1 ) ]
  end | __Stack].

-compile({inline,{yeccpars2_66_,1}}).
-file("edoc_parser.yrl", 118).
yeccpars2_66_([__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # t_type { name = # t_name { app = tok_val ( __2 ) ,
    module = qname ( __4 ) ,
    name = tok_val ( __6 ) } ,
    args = element ( 1 , __7 ) }
  end | __Stack].

-compile({inline,{yeccpars2_67_,1}}).
-file("edoc_parser.yrl", 49).
yeccpars2_67_([__3,__2,__1 | __Stack]) ->
 [begin
   [ tok_val ( __3 ) | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_69_,1}}).
-file("edoc_parser.yrl", 66).
yeccpars2_69_([__2,__1 | __Stack]) ->
 [begin
   { [ ] , tok_line ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_70_,1}}).
-file("edoc_parser.yrl", 67).
yeccpars2_70_([__3,__2,__1 | __Stack]) ->
 [begin
   { lists : reverse ( __2 ) , tok_line ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_74_,1}}).
-file("edoc_parser.yrl", 124).
yeccpars2_74_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_76_,1}}).
-file("edoc_parser.yrl", 106).
yeccpars2_76_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   # t_record { name = # t_atom { val = tok_val ( __2 ) } }
  end | __Stack].

-compile({inline,{yeccpars2_78_,1}}).
-file("edoc_parser.yrl", 128).
yeccpars2_78_([__3,__2,__1 | __Stack]) ->
 [begin
   # t_field { name = # t_atom { val = tok_val ( __1 ) } , type = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_80_,1}}).
-file("edoc_parser.yrl", 108).
yeccpars2_80_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # t_record { name = # t_atom { val = tok_val ( __2 ) } ,
    fields = lists : reverse ( __4 ) }
  end | __Stack].

-compile({inline,{yeccpars2_81_,1}}).
-file("edoc_parser.yrl", 125).
yeccpars2_81_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __3 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_82_,1}}).
-file("edoc_parser.yrl", 76).
yeccpars2_82_([__2,__1 | __Stack]) ->
 [begin
   annotate ( __1 , tok_val ( __2 ) )
  end | __Stack].

-compile({inline,{yeccpars2_85_,1}}).
-file("edoc_parser.yrl", 114).
yeccpars2_85_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   # t_type { name = # t_name { module = qname ( __1 ) ,
    name = tok_val ( __3 ) } ,
    args = element ( 1 , __4 ) }
  end | __Stack].

-compile({inline,{yeccpars2_87_,1}}).
-file("edoc_parser.yrl", 104).
yeccpars2_87_([__3,__2,__1 | __Stack]) ->
 [begin
   # t_fun { args = element ( 1 , __1 ) , range = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_90_,1}}).
-file("edoc_parser.yrl", 139).
yeccpars2_90_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   # t_def { name = # t_type { name = # t_name { name = tok_val ( __1 ) } ,
    args = __2 } ,
    type = __4 }
  end | __Stack].

-compile({inline,{yeccpars2_91_,1}}).
-file("edoc_parser.yrl", 133).
yeccpars2_91_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __3 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_92_,1}}).
-file("edoc_parser.yrl", 131).
yeccpars2_92_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_93_,1}}).
-file("edoc_parser.yrl", 155).
yeccpars2_93_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # t_typedef { name = # t_name { name = tok_val ( __1 ) } ,
    args = __2 ,
    type = __4 ,
    defs = lists : reverse ( __5 ) }
  end | __Stack].

-compile({inline,{yeccpars2_95_,1}}).
-file("edoc_parser.yrl", 43).
yeccpars2_95_([__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_96_,1}}).
-file("edoc_parser.yrl", 131).
yeccpars2_96_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_97_,1}}).
-file("edoc_parser.yrl", 194).
yeccpars2_97_([__2,__1 | __Stack]) ->
 [begin
   # t_throws { type = __1 ,
    defs = lists : reverse ( __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_99_,1}}).
-file("edoc_parser.yrl", 42).
yeccpars2_99_([__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_101_,1}}).
-file("edoc_parser.yrl", 131).
yeccpars2_101_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_102_,1}}).
-file("edoc_parser.yrl", 59).
yeccpars2_102_([__1 | __Stack]) ->
 [begin
   # t_name { name = tok_val ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_103_,1}}).
-file("edoc_parser.yrl", 52).
yeccpars2_103_([__2,__1 | __Stack]) ->
 [begin
   # t_spec { type = __1 , defs = lists : reverse ( __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_104_,1}}).
-file("edoc_parser.yrl", 131).
yeccpars2_104_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_105_,1}}).
-file("edoc_parser.yrl", 54).
yeccpars2_105_([__3,__2,__1 | __Stack]) ->
 [begin
   # t_spec { name = __1 , type = __2 , defs = lists : reverse ( __3 ) }
  end | __Stack].

-compile({inline,{yeccpars2_107_,1}}).
-file("edoc_parser.yrl", 62).
yeccpars2_107_([__3,__2,__1 | __Stack]) ->
 [begin
   # t_fun { args = element ( 1 , __1 ) , range = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_108_,1}}).
-file("edoc_parser.yrl", 45).
yeccpars2_108_([__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_109_,1}}).
-file("edoc_parser.yrl", 179).
yeccpars2_109_([__1 | __Stack]) ->
 [begin
   edoc_refs : module ( qname ( __1 ) )
  end | __Stack].

-compile({inline,{yeccpars2_115_,1}}).
-file("edoc_parser.yrl", 48).
yeccpars2_115_([__1 | __Stack]) ->
 [begin
   [ tok_val ( __1 ) ]
  end | __Stack].

-compile({inline,{yeccpars2_118_,1}}).
-file("edoc_parser.yrl", 185).
yeccpars2_118_([__3,__2,__1 | __Stack]) ->
 [begin
   edoc_refs : function ( tok_val ( __1 ) , tok_val ( __3 ) )
  end | __Stack].

-compile({inline,{yeccpars2_119_,1}}).
-file("edoc_parser.yrl", 187).
yeccpars2_119_([__3,__2,__1 | __Stack]) ->
 [begin
   edoc_refs : type ( tok_val ( __1 ) )
  end | __Stack].

-compile({inline,{yeccpars2_120_,1}}).
-file("edoc_parser.yrl", 168).
yeccpars2_120_([__2,__1 | __Stack]) ->
 [begin
   edoc_refs : app ( tok_val ( __2 ) )
  end | __Stack].

-compile({inline,{yeccpars2_122_,1}}).
-file("edoc_parser.yrl", 172).
yeccpars2_122_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   edoc_refs : app ( tok_val ( __2 ) , __4 )
  end | __Stack].

-compile({inline,{yeccpars2_123_,1}}).
-file("edoc_parser.yrl", 170).
yeccpars2_123_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   edoc_refs : app ( tok_val ( __2 ) , __4 )
  end | __Stack].

-compile({inline,{yeccpars2_129_,1}}).
-file("edoc_parser.yrl", 175).
yeccpars2_129_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   edoc_refs : function ( qname ( __1 ) , tok_val ( __3 ) , tok_val ( __5 ) )
  end | __Stack].

-compile({inline,{yeccpars2_130_,1}}).
-file("edoc_parser.yrl", 177).
yeccpars2_130_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   edoc_refs : type ( qname ( __1 ) , tok_val ( __3 ) )
  end | __Stack].

-compile({inline,{yeccpars2_131_,1}}).
-file("edoc_parser.yrl", 182).
yeccpars2_131_([__3,__2,__1 | __Stack]) ->
 [begin
   edoc_refs : package ( qname ( __1 ) )
  end | __Stack].


-file("edoc_parser.yrl", 396).
