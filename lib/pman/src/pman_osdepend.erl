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
%%%----------------------------------------------------------------------
%%% Purpose : This file is a OS independent front end to all OS specific
%%%           functions.
%%%----------------------------------------------------------------------

-module(pman_osdepend).

%%-compile(export_all).
-export([options_file_name/0, mkdir_for_file/1]).


%%
%% This function returns the options file name, independent of 
%% platform. 
%% All platform specific functionality can be found in the modules:
%% pman_windows_95, pman_windows_nt and pman_unix
%%

options_file_name() ->
    tool_utils:options_filename("pman").

    


%% Tries to create the directory DirName. Missing parent directories
%% ARE created.  The function sucessively tries to create all parent
%% directories, and may if there is a problem along the way result in
%% some directories being created, but still exiting with an error
%% message.
%%

mkdir_p(DirName) ->

    %% The fun Makedir is used in lists:foldl to succesively 
    %% create the directories that make up the entire DirName
    Makedir = fun(Comp, Acc) ->
		      Dir = filename:join([Acc, Comp]),
		      case file:make_dir(Dir) of
			  {error, eexist} -> Dir ;
			  {error, _Reason} -> exit({error, mkdir_failed});
			  ok -> Dir
		      end
	      end,
    

    [Drive | Rest]  = filename:split(DirName),

    lists:foldl(Makedir, Drive, Rest).


%%
%% Utilizes mkdir_p to create a directory necessary for creating the specified
%% file.
%%

mkdir_for_file(FileName) ->
    All = filename:split(FileName),
    Butlast = butlast(All),
    DirName = filename:join(Butlast),
    mkdir_p(DirName).


%%
%% Missing lists-library function.
%%

butlast([X]) -> [];
butlast([Hd|Tl]) -> [Hd | butlast(Tl)].

