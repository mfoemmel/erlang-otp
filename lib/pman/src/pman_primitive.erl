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

-module(pman_primitive).

-export([seemod/3,add_mod/2, remove_mod/2]).


%% ---------------------------------------------------------------
%% Apply appropiate checks before removing a module from the grid
%%
%% Return: {true,Seemods} or false
%% ---------------------------------------------------------------

remove_mod(allmods,Mod) ->
    {true,[Mod]};
remove_mod(Seemods,Mod) ->
    case lists:member(Mod,Seemods) of
	false ->
	    {true,[Mod|Seemods]};
	true ->
	    false
    end.
  
%% -----------------------------------------------------------------
%% Apply appropiate checks before adding a module to the visible mods

%% add_mod/2 

add_mod(allmods,Mod) ->
    false;
add_mod(Seemods,Mod) ->
    case lists:member(Mod,Seemods) of
	true ->
	    case lists:delete(Mod,Seemods) of
		[]    -> {true,allmods};
		Other -> {true,Other}
	    end;
	false ->
	    false
    end.


%%
%% seemod(Module, Mods, Seemodules)
%%
%% seemod/3 returns 
%%
%% ARGUMENTS
%%
%% Module	???
%% Mods		???
%% Seemodules	???
%%
%%
%% RETURNS: 
%%   true	if ???
%%   false	if ???
%%
seemod(Module, Mods, allmods) ->
    case ordsets:is_element(Module, Mods) of
	false ->  %% At least not hide it
	    true;
	_ ->
	    false
    end;
seemod(Module, Mods, Seemods) ->
    case lists:member(Module, Seemods) of
	false ->
	    seemod(Module, Mods, allmods);
	true ->
	    false
    end.




