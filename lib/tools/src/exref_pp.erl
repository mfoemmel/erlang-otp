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
%%
%% Pretty printing of analysis results.
%%

-module(exref_pp).

-export([pp2_start/1]).




pp2_start(AnalysisResult) ->
    io:format("~n",[]),
    pp2(AnalysisResult),
    ok.



pp2({call, M, Call2}) when list(M) ->
    io:format("# CALL GRAPH FOR FUNCTIONS IN MODULES ~w~n",[lists:sort(M)]),
    pp2(Call2);
pp2({call, {M,F,A}, Call2}) ->
    io:format("# CALL GRAPH FOR FUNCTION ~w:~w/~w~n", [M, F, A]),
    pp2(Call2);
pp2({use, M, Use2}) when list(M) ->
    io:format("# USE GRAPH FOR FUNCTIONS IN MODULES ~w~n",[lists:sort(M)]),
    pp2(Use2);
pp2({use, M, Use2}) when atom(M) ->
    io:format("# USE GRAPH FOR FUNCTIONS IN MODULE ~w~n",[M]),
    pp2(Use2);
pp2({use, {M,F,A}, Use2}) ->
    io:format("# USE GRAPH FOR FUNCTION ~w:~w/~w~n", [M, F, A]),
    pp2(Use2);
pp2({module_call,M, Dos}) ->
    io:format("# MODULE GRAPH FOR MODULES ~w~n", [lists:sort(M)]),
    pp2(Dos);
pp2({undefined, {M,F,A}, X}) ->
    io:format("Undefined ~w:~w/~w~n", [M, F, A]),
    pp2(X);
pp2({called_by, {File,Line,Mod,F,A}}) ->
    Short = filename:basename(File),
    io:format("~s:~w: called by ~w:~w/~w~n", [Short,Line,Mod,F,A]);
pp2({mcall, X}) ->
    pp2_mcall(X, []);
pp2({muse, X}) ->
    pp2_muse(X, []);
pp2({module_use, M, X}) ->
    io:format("# MODULE USE FOR MODULES ~w~n", [lists:sort(M)]), 
    pp2(X);
pp2({undefined_functions, M, X}) ->
    io:format("# UNDEFINED FUNCTIONS IN MODULES ~w~n", [lists:sort(M)]), 
    pp2(X);
pp2({exports_not_called, M, X}) ->
    io:format("# EXPORTED FUNCTIONS NOT CALLED IN MODULES ~w~n",
	      [lists:sort(M)]),
    pp2(X);
pp2({locals_not_called, M, X}) ->
    io:format("# LOCALS FUNCTIONS NOT CALLED IN MODULES ~w~n",
	      [lists:sort(M)]),
    pp2(X);
pp2({recursive_modules, M}) ->
    io:format("# RECURSIVE MODULES ~n", []), 
    pp2(M);
pp2([[Mod,ModList]|T]) when atom(Mod),list(ModList) ->
    io:format("~s:~w~n",[Mod,ModList]),
    pp2(T);
pp2({call1, local, [File,Line,M,F,A], V}) ->
    Short = filename:basename(File),
    io:format("~s:~w: ~w:~w/~w: ", [Short,Line,M,F,A]),
    pp2(V);
pp2({call1, exported, [File,Line,M,F,A], V}) ->
    Short = filename:basename(File),
    io:format("~s:~w:*~w:~w/~w: ", [Short,Line,M,F,A]),
    pp2(V);
pp2([H|T]) ->
    pp2(H),
    pp2(T);
pp2([]) -> [];
pp2({called, X}) -> 
    pp2(X),
    io:format("~n");
pp2({called1,M,F,A}) -> 
    io:format("~w/~w ", [F,A]);
pp2({called2,M,F,A}) -> 
    io:format("~w:~w/~w ", [M,F,A]);
pp2({local_def, {File,Line,M,F,A}, X}) ->
    Short = filename:basename(File),
    io:format("~s:~w: Local def ~w:~w/~w~n", [Short,Line,M,F,A]),
    pp2(X);
pp2({{exported,compiler}, {File,Line,M,F,A}, X}) ->
    Short = filename:basename(File),
    io:format("~s:~w: Exported def(by compiler) *~w:~w/~w~n",[Short,Line,M,F,A]),
    pp2(X);
pp2({{local,compiler}, {File,Line,M,F,A}, X}) ->
    Short = filename:basename(File),
    io:format("~s:~w: Local def(by compiler) ~w:~w/~w~n",[Short,Line,M,F,A]),
    pp2(X);
pp2({exported_def, {File,Line,M,F,A}, X}) ->
    Short = filename:basename(File),
    io:format("~s:~w: Exported def *~w:~w/~w~n",[Short,Line,M,F,A]),
    pp2(X);
pp2({undefined_def, {M,F,A}, X}) ->
    io:format("$undef:$undef: Undefined def ~w:~w/~w~n", [M, F, A]),
    pp2(X);
pp2({module_not_loaded, {M,F,A}, X}) ->
    io:format("$no_module:$undef: Module not in exref-graph ~w:~w/~w~n", [M, F, A]),
    pp2(X);
pp2({not_called, {File,Line,M,F,A}}) ->
    Short = filename:basename(File),
    io:format("~s:~w: ~w:~w/~w~n", [Short,Line,M,F,A]);
pp2(X) -> io:format("*** pretty error:~p\n", [X]).






%%
%% Output call
%%

pp2_mcall([{M1,M2}|Ms], M1) ->
    io:format("~w ", [M2]),
    pp2_mcall(Ms, M1);
pp2_mcall([{M1,M2}|Ms], []) ->
    io:format("~w: ~w ", [M1,M2]),
    pp2_mcall(Ms, M1);
pp2_mcall([{M1,M2}|Ms], _) ->
    io:format("~n~w: ~w ", [M1,M2]),
    pp2_mcall(Ms, M1);
pp2_mcall([], _) ->
    io:format("~n", []).

%%
%% Output use
%%

pp2_muse([{M1,M2}|Ms], M2) ->
    io:format("~w ", [M1]),
    pp2_muse(Ms, M2);
pp2_muse([{M1,M2}|Ms], []) ->
    io:format("~w: ~w ", [M2,M1]),
    pp2_muse(Ms, M2);
pp2_muse([{M1,M2}|Ms], _) ->
    io:format("~n~w: ~w ", [M2,M1]),
    pp2_muse(Ms, M2);
pp2_muse([], _) ->
    io:format("~n",[]).

