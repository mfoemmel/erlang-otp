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
-module(icpreproc).



-export([preproc/2]).


-import(lists, [filter/2]).


%%----------------------------------------------------------------------
%%----------------------------------------------------------------------


preproc(G, File) ->
    Cmd		= icgen:get_opt(G, preproc_cmd),
    Flags	= icgen:get_opt(G, preproc_flags),


    case Cmd of
	"erl" ->
	    case ic_pp:run(File,Flags) of
		{ok, [$#, $ , $1 | Rest], []} ->
		    [$#, $ , $1 | Rest];
		{ok, [$#, $ , $1 | Rest], Warning} ->
		    print_warning(G,Warning),
		    [$#, $ , $1 | Rest];
		{error,Error} ->
		    print_error(G,Error)
	    end;
	
	_ ->
	    Line	= Cmd++" "++Flags++" "++File,
	    case os:cmd(Line) of
		[$#, $ , $1 | Rest] ->			% Ok output
		    [$#, $ , $1 | Rest];
		X ->
		    icgen:fatal_error(G, {preproc, filter(X)})
	    end
    end.


filter(X) ->  
    X2 = divide_nl(X, []),
    filter_x_switch(X2).


divide_nl([10 | Xs], Out) ->
    [lists:reverse(Out) | divide_nl(Xs, [])];
divide_nl([X | Xs], Out) -> divide_nl(Xs, [X|Out]);
divide_nl([], Out) -> lists:reverse(Out).


filter_x_switch(L) ->
    filter(fun([$g,$c,$c,$:,$ ,$W,$a,$r,$n,$i,$n,$g,$:,$ ,$`,$-,$x,$ | _]) ->
		   false;
	      (_) -> true end, L).


print_error(G,[]) ->
    ok;
print_error(G,[{File,Line,Text}]) ->
    ErrorText = File++":"++integer_to_list(Line)++": "++Text,
    icgen:fatal_error(G, {ic_pp_error, ErrorText}),
    ok;
print_error(G,[{File,Line,Text}|T]) ->
    ErrorText = File++":"++integer_to_list(Line)++": "++Text,
    icgen:error(G, {ic_pp_error, ErrorText}),
    print_error(G,T);
print_error(G,[H]) ->
    ErrorText = H++"\n",
    icgen:fatal_error(G, {ic_pp_error, ErrorText}),
    ok;
print_error(G,[H|T]) ->
    ErrorText = H++"\n",
    icgen:error(G, {ic_pp_error, ErrorText}),
    print_error(G,T).


print_warning(G,[]) ->
    ok;
print_warning(G,[{File,Line,Text}|T]) ->
    WarText = File++":"++integer_to_list(Line)++": "++Text,
    icgen:warn(G, {ic_pp_warning, WarText}),
    print_warning(G,T);
print_warning(G,[H|T]) ->
    WarText = H++"\n",
    icgen:warn(G, {ic_pp_warning, WarText}),
    print_warning(G,T).


