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

-module(mesh_sup).


-behaviour(supervisor).


   %% Internal exports.
-export([init/1]).


-include("mesh_app.hrl").



%%%*********************************************************************
%%%  EXPORTED FUNCTIONS
%%%*********************************************************************


%%======================================================================
%% Function:      init/1
%%
%% Return Value:  
%%
%% Description:   
%%
%% Parameters:    
%%======================================================================

init(mesh_sup_init) ->
    case mnesia:wait_for_tables([mesh_type, 
				 mesh_meas], 
				60000) of
	ok ->
	    {ok, {?SupFlags, [?MeshServerSpec]}};
	StopReason ->
	    {stop, StopReason}
    end.



