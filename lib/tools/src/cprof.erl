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
-module(cprof).

%% Call count profiling tool.

-export ([start/0, start/1, start/2, start/3,
	  stop/0, stop/1, stop/2, stop/3,
	  restart/0, restart/1, restart/2, restart/3,
	  pause/0, pause/1, pause/2, pause/3,
	  analyse/0, analyse/1, analyse/2,
	  analyze/0, analyze/1, analyze/2]).



start() ->
    tr({'_','_','_'}, true) + tr(on_load, true).

start({_,_,_} = MFA) ->
    tr(MFA, true);
start({FuncSpec}) ->
    tr(FuncSpec, true);
start(M) ->
    tr({M,'_','_'}, true).

start(M,F) ->
    tr({M,F,'_'}, true).

start(M,F,A) ->
    tr({M,F,A}, true).



stop() ->
    tr({'_','_','_'}, false) + tr(on_load, false).

stop({_,_,_} = MFA) ->
    tr(MFA, false);
stop({FuncSpec}) ->
    tr(FuncSpec, false);
stop(M) ->
    tr({M,'_','_'}, false).

stop(M,F) ->
    tr({M,F,'_'}, false).

stop(M,F,A) ->
    tr({M,F,A}, false).



restart() ->
    tr({'_','_','_'}, restart).

restart({_,_,_} = MFA) ->
    tr(MFA, restart);
restart({FuncSpec}) ->
    tr(FuncSpec, restart);
restart(M) ->
    tr({M,'_','_'}, restart).

restart(M,F) ->
    tr({M,F,'_'}, restart).

restart(M,F,A) ->
    tr({M,F,A}, restart).



pause() ->
    tr({'_','_','_'}, pause) + tr(on_load, false).

pause({_,_,_} = MFA) ->
    tr(MFA, pause);
pause({FuncSpec}) ->
    tr(FuncSpec, pause);
pause(M) ->
    tr({M,'_','_'}, pause).

pause(M,F) ->
    tr({M,F,'_'}, pause).

pause(M,F,A) ->
    tr({M,F,A}, pause).



analyse() ->
    analyse(1).

analyse(Limit) when integer(Limit) ->
    L = [X || {M,N,_}=X <- [analyse(M, Limit) 
			       || M <- [element(1, Mod) 
					|| Mod <- code:all_loaded()]],
		 N > 0, M /= ?MODULE],
    N = lists:foldl(fun ({_,C,_}, Q) -> Q+C end, 0, L),
    {N, lists:reverse(lists:keysort(2, L))};
analyse(M) when atom(M) ->
    analyse(M, 1).

analyse(M, Limit) when atom(M), integer(Limit) ->
    L = [{MFA,C} 
	 || {MFA,{_,C}} <- [begin
				MFA = {M,F,A},
				{MFA, erlang:trace_info(MFA, call_count)}
			    end
			    || {F, A} <- M:module_info(functions)],
	    integer(C)],
    N = lists:foldl(fun ({_, C}, Q) -> Q+C end, 0, L),
    {M, N, lists:reverse(lists:keysort(2, [Y || {_,C}=Y <- L, C >= Limit]))}.



analyze() ->
    analyse().

analyze(X) ->
    analyse(X).

analyze(X, Y) ->
    analyse(X, Y).



tr(FuncSpec, State) ->
    erlang:trace_pattern(FuncSpec, State, [call_count]).
