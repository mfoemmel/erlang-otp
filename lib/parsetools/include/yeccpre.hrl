%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type(yecc_ret() :: {'error', _} | {'ok', _}).

-spec(parse/1 :: (_) -> yecc_ret()).
parse(Tokens) ->
    yeccpars0(Tokens, false).

-spec(parse_and_scan/1 ::
      ({function() | {atom(), atom()}, [_]} | {atom(), atom(), [_]}) ->
            yecc_ret()).
parse_and_scan({F, A}) -> % Fun or {M, F}
    yeccpars0([], {F, A});
parse_and_scan({M, F, A}) ->
    yeccpars0([], {{M, F}, A}).

-spec(format_error/1 :: (any()) -> [char() | list()]).
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

-define(CODE_VERSION, "1.3").

yeccpars0(Tokens, MFA) ->
    try yeccpars1(Tokens, MFA, 0, [], [])
    catch 
        error: Error ->
            Stacktrace = erlang:get_stacktrace(),
            try yecc_error_type(Error, Stacktrace) of
                {syntax_error, Token} ->
                    yeccerror(Token);
                {missing_in_goto_table=Tag, Symbol, State} ->
                    Desc = {Symbol, State, Tag},
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        throw: {error, {_Line, ?MODULE, _M}} = Error -> 
            Error % probably from return_error/2
    end.

yecc_error_type(function_clause, [{?MODULE,F,[State,_,_,_,Token,_,_]} | _]) ->
    case atom_to_list(F) of
        "yeccpars2" ++ _ ->
            {syntax_error, Token};
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            {missing_in_goto_table, Symbol, State}
    end.

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
    Text = case catch erl_scan:token_info(Token, text) of
               {text, Txt} -> Txt;
               _ -> yecctoken2string(Token)
           end,
    Location = case catch erl_scan:token_info(Token, location) of
                   {location, Loc} -> Loc;
                   _ -> element(2, Token)
               end,
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_unicode_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:format("~w", [A]);
yecctoken2string({_Cat, _, Val}) -> io_lib:format("~w", [Val]);
yecctoken2string({dot, _}) -> "'.'";
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:format("~w", [Other]);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


