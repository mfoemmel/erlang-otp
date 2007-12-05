%%<copyright>
%% <year>2003-2007</year>
%% <holder>Ericsson AB, All Rights Reserved</holder>
%%</copyright>
%%<legalnotice>
%% ``The contents of this file are subject to the Erlang Public License,
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
%% The Initial Developer of the Original Code is Ericsson AB.
%%</legalnotice>
%%

-module(rx).
-export([match/2, lmatch/2, match_pos/2]).
-export([t/0]).

-import(lists, [reverse/1]).

-include("ct_util.hrl").

%% @spec t() -> ok
%% @doc For debugging only.
t() ->
    tmatch("glurf", "glarf"),
    tmatch("kalle sover", "([^ ]*) so").

tmatch(Str, Pattern) ->
    io:format("~s =~~ /~s/", [Str,Pattern]),
    io:format("=> ~p\n", [match(Str, Pattern)]).

init() ->
    DriverPath = code:priv_dir(common_test) ++ "/lib",
    erl_ddll:load_driver(DriverPath, "erl_rx_driver"),
    P = open_port({spawn,erl_rx_driver}, []),
    register(erl_rx_driver, P),
    P.

port_call(Data) ->
    case whereis(erl_rx_driver) of
	undefined ->
	    init(); % a raise condition might be possible 
	_P ->
	    true
    end,
    port_control(erl_rx_driver, 0, Data).
	    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @spec match(Str, RegExp) -> nomatch | [string()]
%% Str = string() | [string|binary] | binary
%% RegExp = string() | binary
%% 
%% @doc This function tries to match the Posix regular expression
%% <code>RegExp</code> with the contents of <code>Str</code>. It returns
%% a list of matched strings if a match was found and
%% <code>nomatch</code> otherwise. The list of matched strings looks
%% like this: <code>[FullMatch, SubMatch1, SubMatch2, ...]</code>
%% where <code>FullMatch</code> is the string matched by the whole
%% regular expression and <code>SubMatchN</code> is the string that
%% matched subexpression no N. Subexpressions are denoted with '(' ')'
%% in the regular expression
%%
%% <p>Example:
%% <pre>
%% match("abc01xyz02rst23", "abc[0-9][0-9]"),
%% returns  ["abc01"]
%%
%% match("abc01xyz02rst23", "([a-z]+[0-9]+)([a-z]+[0-9]+)([a-z]+[0-9]+)"),
%% returns ["abc01xyz02rst23","abc01","xyz02","rst23"]
%% </pre></p>
%%
%% @end

match(Str, RegExp) ->
    match(Str, RegExp, strings).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @spec match_pos(Str, RegExp) -> nomatch | [{Start,End}]
%% Start = integer()
%% End = integer()
%% 
%% @doc This function is equivalent to <code>match/2</code>, but it
%% returns a list of positions instead for a list of strings.
match_pos(Str, RegExp) ->
    match(Str, RegExp, pos).

    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @spec lmatch(Lines, RegExp) -> nomatch | {[string()],Rest}
%% Lines = [Str]
%% Rest = [Str]
%% Str = string() | [string|binary] | binary
%% RegExp = string() | binary
%% 
%% @doc Performs match/2 on each string in <code>Lines</code>. The
%% first match found is returned along with the rest of the
%% <code>Lines</code>. If no match is found, <code>nomatch</code> is
%% returned.
%%
%% @see match/2
%%
%% @end
lmatch([Str|RestLines], RegExp) ->
    case match(Str, RegExp) of
	nomatch ->
	    lmatch(RestLines, RegExp);
	Match ->
	    {Match,RestLines}
    end;
lmatch([], _) ->
    nomatch.



%%%-----------------------------------------------------------------
%%% Internal Functions
match(Str, RegExp, ReturnType) when is_binary(RegExp) ->
    Data = [<<(size(RegExp)):32/native,RegExp/binary>>|Str],
    return(ReturnType, list_to_binary(Str),port_call(Data));
match(Str, RegExp, ReturnType) when is_list(RegExp) ->
    match(Str, list_to_binary(RegExp), ReturnType).


return(_Type,_,[]) -> nomatch;
return(Type, Str, Bin) ->
    return_1(Type, Str, Bin, []).

return_1(Type, Str, <<S:32/native,E:32/native,T/binary>>, Acc) ->
    Result = return_2(Type,Str,E,S),
    return_1(Type, Str, T, [Result|Acc]);
return_1(_, _, <<>>, Acc) -> reverse(Acc).


return_2(strings, Str, E, S) ->
    ResLen = E - S,
    <<_:S/binary,Result:ResLen/binary,_/binary>> = Str,
    binary_to_list(Result);
return_2(pos, _Str, E, S) ->
    {S+1,E}.
