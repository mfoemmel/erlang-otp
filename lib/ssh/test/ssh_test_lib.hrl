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
%%----------------------------------------------------------------------
%% Purpose: Define common macros for testing
%%----------------------------------------------------------------------

-define(APPLY(Proxy, Fun),
	Proxy ! {apply, Fun}).

-define(LOG(Format, Args),
	ssh_test_lib:log(Format, Args, ?MODULE, ?LINE)).

-define(ERROR(Reason),
	ssh_test_lib:error(Reason, ?MODULE, ?LINE)).

-define(SKIP(Reason),
	ssh_test_lib:skip(Reason, ?MODULE, ?LINE)).

-define(VERIFYL(Expected, Expr),
	fun(A,B) when list(A), list(B) ->
		A1 = lists:sort(A),
		B1 = lists:sort(B),
		case A1 of
		    B1 -> ?LOG("Ok, ~p~n", [B]);
		    _  -> ?ERROR(B)
		end,
		B;
	   (A,A) ->
		?LOG("Ok, ~p~n", [A]),
		A;
	   (A,B) ->
		?ERROR(B),
		B
	end(Expected, (catch Expr))).

-define(VERIFY(Expected, Expr),
	fun() ->
		AcTuAlReS = (catch (Expr)),
		case AcTuAlReS of
		    Expected -> ?LOG("Ok, ~p~n", [AcTuAlReS]);
		    _        ->	?ERROR(AcTuAlReS)
	       end,
		AcTuAlReS
	end()).

-define(RECEIVE(Expected),
	?VERIFY(Expected, ssh_test_lib:flush())).

-define(MULTI_RECEIVE(Expected),
	?VERIFY(lists:sort(Expected), lists:sort(ssh_test_lib:flush()))).

-define(ACQUIRE_NODES(N, Config),
	ssh_test_lib:prepare_test_case([init, {stop_app, ssh}],
				   N, Config, ?FILE, ?LINE)).

