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
-module(lib).

-export([flush_receive/0,error_message/2, progname/0, nonl/1, send/2,
	 sendw/2, eval_str/1]).

flush_receive() ->
    receive
	_Any ->
	    flush_receive()
    after
	0 ->
	    ok
    end.

%%
%% Functions for doing standard system format i/o.
%%

error_message(Format, Args) ->
    io:format("** ~s **\n", [io_lib:format(Format, Args)]).

%% Return the name of the script that starts (this) erlang 
%%

progname() ->
    case init:get_argument(progname) of
	{ok, [[Prog]]} ->
	    list_to_atom(Prog);
	_Other ->
	    no_prog_name
    end.

nonl([10]) -> [];
nonl([]) -> [];
nonl([H|T]) -> [H|nonl(T)].


send(To, Msg) -> To ! Msg.

sendw(To, Msg) ->
    To ! {self(), Msg},
    receive 
	Reply -> Reply
    end.



%% eval_str(InStr) -> {ok, OutStr} | {error, ErrStr'}
%%   InStr must represent a body

-define(result(F,D), lists:flatten(io_lib:format(F, D))).

eval_str(Str) when list(Str) ->
    case erl_scan:tokens([], Str, 0) of
	{more, {['"'], _, _}} ->
	    {error, "Unterminated (\") string"};
	{more, {['\''],_,_}} ->
	    {error, "Unterminated (\') string"};
	{more, _} ->
	    {error, "Incomplete form (missing .<cr>)??"};
	{done, {ok, Toks, _}, Rest} ->
	    case all_white(Rest) of
		true ->
		    case erl_parse:parse_exprs(Toks) of
			{ok, Exprs} ->
			    case catch erl_eval:exprs(Exprs, []) of
				{value, Val, _} ->
				    {ok, Val};
				Other ->
				    {error, ?result("*** eval: ~p", [Other])}
			    end;
			{error, {_Line, Mod, Args}} ->
			    Msg = ?result("*** ~s",[apply(Mod,format_error,
							     [Args])]),
			    {error, Msg}
		    end;
		false ->
		    {error, ?result("Non-white space found after "
				    "end-of-form :~s", [Rest])}
		end
    end;
eval_str(Bin) when binary(Bin) ->
    eval_str(binary_to_list(Bin)).

all_white([$\s|T]) -> all_white(T);
all_white([$\n|T]) -> all_white(T);
all_white([$\t|T]) -> all_white(T);
all_white([])      -> true;
all_white(_)       -> false.

		     
