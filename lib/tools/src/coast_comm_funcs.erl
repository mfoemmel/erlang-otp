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


-module(coast_comm_funcs).



-export([remove_from_database/1,
	 get_source_files/1
	]).


-include("coast_server.hrl").






%%%*********************************************************************
%%% EXTERNAL FUNCTIONS
%%%*********************************************************************




remove_from_database(Module) ->
    ets:delete(?TABLE_NAME, {module_initialised, Module}),
    ets:delete(?TABLE_NAME, {first_code_line, Module}),
    %ets:delete(?TABLE_NAME, {end_of_function, Module}),
    OldEntries = ets:match(?TABLE_NAME, 
			   {{Module, '$1', '$2', '$3', '$4', '$5', '$6'}, 
			    '_', '_', '_'}),
    remove_old_entries(Module, OldEntries).




get_source_files(Modules) ->
    get_source_files(Modules, []).



	

%%%*********************************************************************
%%% INTERNAL FUNCTIONS
%%%*********************************************************************




get_source_files([Module | T], Acc) ->
    SrcFile = case catch Module:module_info(attributes) of
		  {'EXIT', Reason} ->
		      {error, {no_such_module, Module}};
		  ModuleInfo ->
		      case catch lists:keysearch(coast_compiled, 1, ModuleInfo) of
			  false ->
			      {error, {not_coast_compiled, Module}};
			  {value, {coast_compiled, [{Module, FileName}]}} ->
			      FileName
		      end
	      end,
    get_source_files(T, [SrcFile | Acc]);
get_source_files([], Acc) ->
    lists:reverse(Acc).



remove_old_entries(Module, [[F, A, I, C, CL, L] | T]) ->
    ets:delete(?TABLE_NAME, {Module, F, A, I, C, CL, L}),
    remove_old_entries(Module, T);
remove_old_entries(Module, []) ->
    ok.





