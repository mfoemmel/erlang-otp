%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% Time-stamp: <01/07/25 11:55:15 happi>
%% ====================================================================
%%  Filename : 	hipe_temp_map.erl
%%  Module   :	hipe_temp_map
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-07-24 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: happi $
%%              $Date: 2001/07/25 10:32:55 $
%%              $Revision: 1.2 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_temp_map).
-export([cols2tuple/2, is_spilled/2, find/2, to_substlist/1]).

%
% Convert a list of [{R0, C1}, {R1, C2}, ...} to a tuple {C1, C2, ...}.
%
% The R's must be unique but do not have to be sorted and they can be sparse.
%

cols2tuple(Map, Target) ->
  SortedMap = lists:keysort(1, Map), 
  cols2tuple(0, SortedMap, [], Target). 

 
%% Build a dense mapping 
cols2tuple(N, [], Vs, Target) ->
  %% Done reverse the list and convert to tuple.
  list_to_tuple(lists:reverse(Vs));
cols2tuple(N, [{R, C}|Ms], Vs, Target) when N =:= R ->
  %% N makes sure the mapping is dense. N is he next key.
  cols2tuple(N+1, Ms, [C|Vs], Target);
cols2tuple(N, SourceMapping, Vs, Target) ->
  %% The source was sparce, make up some placeholders...
  Val = 	      
    case Target:is_precolored(N) of
      %% If it is precolored we now what to map it to.
      true -> {reg,N};
      false -> unknown
    end,
  cols2tuple(N+1, 
	     SourceMapping, 
	     [Val|Vs],
	     Target).


%% True if temp Temp is spilled.
is_spilled(Temp, Map) ->
  case element(Temp+1, Map) of
    {reg, R} -> false;
    {spill, N} -> true;
    unknown -> false;
    _ -> exit({?MODULE, ?LINE, bad_temp_map, Temp, Map})
  end.
       

%% Returns the inf temp Temp is mapped to.
find(Temp, Map) ->
  element(Temp+1, Map).


%% Converts a temp_map tuple back to a (sorted) key-list.
to_substlist(Map) ->
  T = tuple_to_list(Map),
  mapping(T,0).

mapping([R|Rs], Temp) ->
  [{Temp, R}| mapping(Rs, Temp+1)];
mapping([], _ ) -> [].
