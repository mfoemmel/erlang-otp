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
-module(start).

-export([init/0]).



init() ->
    Apps = application:which_applications(),
    case lists:keysearch(sasl,1,Apps) of
	{value,_Tuple0} ->
	    done;
	_Other0 ->
	    application:start(sasl)
    end,
    case lists:keysearch(mnesia,1,Apps) of
	{value,_Tuple1} ->
	    done;
	_Other1 ->
	    application:start(mnesia)
    end,
    case lists:keysearch(eva,1,Apps) of
	{value,_Tuple2} ->
	    done;
	_Other2 ->
	    catch eva_sup:create_tables_log([]),
	    disk_log:open([{name,"default_log"},
			   {format,internal},
			   {type,wrap},
			   {size,{10000,10}}]),
	    eva_sup:start_link_log("default_log"),
	    application:start(eva)
    end,
    case lists:keysearch(mesh, 1, Apps) of
	{value,_Tuple3} ->
	    done;
	_Other3 ->
	    mesh:create_tables([]),
	    application:start(mesh)
    end.
	    
	    
