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
-module(beam_mkops).

-export([cmdline/1]).

cmdline(Args) ->
    case catch cmdline1(Args) of
	{'EXIT', Reason} ->
	    io:format("Failed: ~p~n", [Reason]),
	    halt(1);
	ok ->
	    halt(0);
	{error, {Format, As}} ->
	    io:put_chars("beam_mkops: "),
	    case io:format(Format, As) of
		ok ->
		    ok;
		_ ->
		    io:format("~p, ~p", [Format, As])
	    end,
	    io:nl();
	Other ->
	    io:format("Internal error: Bad return value '~p'~n", [Other])
    end.

error(Format) ->
    error(Format, []).
error(Format, Args) ->
    throw({error, {Format, Args}}).

cmdline1([File]) ->
    In = lists:concat([File]),
    {ok, Fd} = file:open(In, [read]),
    List = process(Fd, []),
    file:close(Fd),
    Out = "beam_opcodes.hrl",
    {ok, OutFd} = file:open(Out, [write]),
    make(OutFd, List, filename:basename(Out)),
    file:close(OutFd),
    ok;
cmdline1(Other) ->
    error("Bad command line arguments: ~p", [Other]).

%% Processes input file.

process(Fd, Result) ->
    case scan(io:get_line(Fd, none), [], []) of
	eof ->
	    case Result of
		error ->
		    error;
		List ->
		    lists:reverse(Result)
	    end;
	[] ->
	    process(Fd, Result);
	List ->
	    case parse(List) of
		error ->
		    process(Fd, error);
		Tuple when tuple(Tuple) ->
		    process(Fd, [Tuple|Result])
	    end
    end.

%% Splits input line into fields.

scan(eof, Token, Result) ->
    eof;
scan([$#|_], Token, Result) ->
    lists:reverse(Result);
scan([$=|Rest], Token, []) ->
    Name = lists:reverse(Token),
    Value = lib:nonl(Rest),
    case catch list_to_integer(Value) of
	{'EXIT', _} ->
	    {define, {Name, Value}};
	Integer when integer(Integer) ->
	    {define, {Name, Integer}}
    end;
scan([C|Rest], [], Result) when C >= 0, C =< $\s ->
    scan(Rest, [], Result);
scan([C|Rest], Token, Result) when C >= 0, C =< $\s ->
    scan(Rest, [], [lists:reverse(Token)|Result]);
scan([C|Rest], Token, Result) ->
    scan(Rest, [C|Token], Result);
scan([], [C|Rest], Result) ->
    scan([], [], [[C|Rest]|Result]);
scan([], [], Result) ->
    lists:reverse(Result).

%% Parses field list.

parse([Name, Op, Size]) ->
    case catch {Name, list_to_integer(Op), list_to_integer(Size)} of
	{'EXIT', _} ->
	    error("bad opcode and/or size for '~s'", [Name]);
	Tuple ->
	    Tuple
    end;
parse(Tuple) when tuple(Tuple), size(Tuple) == 2 ->
    Tuple;
parse(_) ->
    error("wrong number of fields").

%% Makes output file.

make(Fd, List, Name) ->
    io:format(Fd, "% Automatically generated from ops.tab -- don't edit.~n", []),
    make_defs(Fd, List),
    MaxOp = lists:max([Op || {_, Op, _} <- List]),
    io:format(Fd, "-define(MAX_OPS, ~w).\n", [MaxOp+1]),
    io:nl(Fd),
    lists:foreach(fun({N, Op, _}) ->
			  ok = io:format(Fd, "-define(op_~s, ~w).~n", [N, Op]);
		     (_) ->
			  ok
		  end, List),
    io:nl(Fd),
    lists:foreach(fun({N, _, Sz}) ->
			  ok = io:format(Fd, "-define(sz_~s, ~w).~n", [N, Sz]);
		     (_) ->
			  ok
		  end, List),
    io:nl(Fd),
    lists:foreach(fun({N, Op, Sz}) ->
			  ok = io:format(Fd, "-define(opdef_~s, ~w).~n",
					 [N, {Op, Sz}]);
		     (_) ->
			  ok
		  end, List).

make_defs(Fd, List) ->
    Fun = fun({define, {Name, Value}}) ->
		  ok = io:format(Fd, "-define(~s, ~p).~n",
				 [Name, Value]);
	     (_) ->
		  ok
	  end,
    lists:foreach(Fun, List).
