%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% Time-stamp: <02/05/28 15:46:26 happi>
%% ====================================================================
%%  Filename : 	hipe_temp_map.erl
%%  Module   :	hipe_temp_map
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-07-24 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: kostis $
%%              $Date: 2004/01/23 21:34:53 $
%%              $Revision: 1.9 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_temp_map).
-export([cols2tuple/2, is_spilled/2, %% sorted_cols2tuple/2,
	 in_reg/2, in_fp_reg/2, find/2, to_substlist/1]).

%%-define(DO_ASSERT,true).
-include("../main/hipe.hrl").


%%
%% Convert a list of [{R0, C1}, {R1, C2}, ...} to a temp_map
%% (Currently implemented as a tuple) tuple {C1, C2, ...}.
%%
%% The indices (Ri) must be unique but do not have to be sorted and 
%% they can be sparse.
%% Note that the first allowed index is 0 -- this will be mapped to 
%% element 1

cols2tuple(Map, Target) ->
  ?ASSERT(check_list(Map)),
  SortedMap = lists:keysort(1, Map), 
  cols2tuple(0, SortedMap, [], Target). 

%% sorted_cols2tuple(Map, Target) ->
%%   ?ASSERT(check_list(Map)),
%%   ?ASSERT(Map =:= lists:keysort(1, Map)),
%%   cols2tuple(0, Map, [], Target). 

%% Build a dense mapping 
cols2tuple(_, [], Vs, _) ->
  %% Done reverse the list and convert to tuple.
  list_to_tuple(lists:reverse(Vs));
cols2tuple(N, [{R, C}|Ms], Vs, Target) when N =:= R ->
  %% N makes sure the mapping is dense. N is he next key.
  cols2tuple(N+1, Ms, [C|Vs], Target);
cols2tuple(N, SourceMapping, Vs, Target) ->
  %% The source was sparce, make up some placeholders...
  Val = 	      
    case Target:is_precoloured(N) of
      %% If it is precoloured, we know what to map it to.
      true -> 
	case Target of
	  hipe_sparc_specific_fp ->{fp_reg, N};
	  _->{reg,N}
	end;
      false -> unknown
    end,
  cols2tuple(N+1, SourceMapping, [Val|Vs], Target).

-ifdef(DO_ASSERT).
%% True if temp Temp is spilled.
is_spilled(Temp, Map) ->
  case catch element(Temp+1, Map) of
    {reg, R} -> false;
    {fp_reg, R}-> false;
    {spill, N} -> true;
    unknown -> false;
    Error -> ?EXIT({bad_temp_map, Temp, Map, Error})
 end.

%% True if temp Temp is allocated to a reg.
in_reg(Temp, Map) ->
  case catch element(Temp+1, Map) of
    {reg, R} -> true;
    {fp_reg, R} -> false;
    {spill, N} -> false;
    unknown -> false;
    _ -> ?EXIT({bad_temp_map, Temp, Map})
  end.
       
%% True if temp Temp is allocated to a fp_reg.
in_fp_reg(Temp, Map) ->
  case catch element(Temp+1, Map) of
    {fp_reg, R} -> true;
    {reg, R} -> false;
    {spill, N} -> false;
    unknown -> false;
    _ -> ?EXIT({bad_temp_map, Temp, Map})
  end.

%% Returns the inf temp Temp is mapped to.
find(Temp, Map) ->
  case catch element(Temp+1, Map) of
    {'EXIT',_} ->
      ?EXIT({bad_temp_map, Temp, Map});
    Val -> Val
  end.

-else. %% No assert

%% True if temp Temp is spilled.
is_spilled(Temp, Map) ->
  case element(Temp+1, Map) of
    {reg, _R} -> false;
    {fp_reg, _R}-> false;
    {spill, _N} -> true;
    unknown -> false;
    _ -> ?EXIT({bad_temp_map, Temp, Map})
  end.
    
%% True if temp Temp is allocated to a reg.
in_reg(Temp, Map) ->
  case element(Temp+1, Map) of
    {reg, _R} -> true;
    {fp_reg, _R}-> false;
    {spill, _N} -> false;
    unknown -> false;
    _ -> ?EXIT({bad_temp_map, Temp, Map})
  end.
       
%% True if temp Temp is allocated to a fp_reg.
in_fp_reg(Temp, Map) ->
  case catch element(Temp+1, Map) of
    {fp_reg, _R} -> true;
    {reg, _R} -> false;
    {spill, _N} -> false;
    unknown -> false;
    _ -> ?EXIT({bad_temp_map, Temp, Map})
  end.


%% Returns the inf temp Temp is mapped to.
find(Temp, Map) -> element(Temp+1, Map).

-endif.


%% Converts a temp_map tuple back to a (sorted) key-list.
to_substlist(Map) ->
  T = tuple_to_list(Map),
  mapping(T,0).

mapping([R|Rs], Temp) ->
  [{Temp, R}| mapping(Rs, Temp+1)];
mapping([], _ ) -> [].

-ifdef(DO_ASSERT).
check_list([{I,_}|Rest]) ->
  is_integer(I) andalso (I >= 0) andalso
    no_dups(I,Rest) andalso check_list(Rest);
check_list([])       -> true;
check_list(_)        -> false.
no_dups(I,[{I,_}|_]) -> false;
no_dups(I,[_|Rest]) ->  no_dups(I,Rest);
no_dups(_,[]) ->        true.
-endif.
