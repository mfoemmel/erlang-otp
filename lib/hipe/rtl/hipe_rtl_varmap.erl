%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% Time-stamp: <02/09/28 21:59:55 happi>
%% ====================================================================
%%  Filename : 	hipe_rtl_varmap.erl
%%  Module   :	hipe_rtl_varmap
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-04-10 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: happi $
%%              $Date: 2002/09/29 20:21:20 $
%%              $Revision: 1.7 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_rtl_varmap).
-export([init/1,
	 ivs2rvs/2,
	 icode_var2rtl_var/2,
	 icode_label2rtl_label/2]).


%-------------------------------------------------------------------------

-include("../main/hipe.hrl").
-include("hipe_icode2rtl.hrl").

%-------------------------------------------------------------------------

init(Icode) ->
  init_icode(Icode),
  init_rtl(Icode).

%%
%% Initialized gensym for Icode
init_icode(ICode) ->
  hipe_gensym:init(icode),
  {_IVMin, IVMax} = hipe_icode:icode_var_range(ICode),
  {_ILMin, ILMax} = hipe_icode:icode_label_range(ICode),
  ?ASSERT(begin
	    Code = hipe_icode:icode_code(ICode),
	    ActualVmax = hipe_icode:highest_var(Code),
	    ActualLmax = hipe_icode:highest_label(Code),
	    ((ActualVmax =< IVMax) 
	     and
	     (ActualLmax =< ILMax))
	  end),
  %% io:format("IVMax ~w ActualVmax ~w\n",[IVMax,ActualVmax]),
  hipe_gensym:set_var(icode,IVMax+1),
  hipe_gensym:set_label(icode,ILMax+1).
%    hipe_gensym:set_var(icode,ActualVmax+1),
%    hipe_gensym:set_label(icode,ActualLmax+1).

%% Initialize gensym for rtl
init_rtl(Fun) ->
    hipe_gensym:init(rtl),
    hipe_gensym:set_var(rtl,hipe_rtl_arch:first_virtual_reg()),
    hipe_gensym:set_label(rtl,0),
    VarMap = new_var_map(),
    {_Args,_VarMap1} = ivs2rvs(hipe_icode:icode_params(Fun), VarMap).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ____________________________________________________________________
%%      
%% Mapping of vars and labels from Icode to RTL
%% ____________________________________________________________________
%% 
%%

%% Convert a list of icode variables to a list of rtl variables
ivs2rvs([], VarMap) ->
   {[], VarMap};
ivs2rvs([V|Vs], VarMap) ->
   {NewV, VarMap0} = icode_var2rtl_var(V, VarMap),
   {NewVs, VarMap1} = ivs2rvs(Vs, VarMap0),
   {[NewV|NewVs], VarMap1}.


%
% Convert icode variable to a rtl variable
%

icode_var2rtl_var(Var, Map) ->
  Value = lookup(Var,Map),
  
  case Value of
    none ->
      case type_of_var(Var) of
	fvar ->
	  NewVar = hipe_rtl:mk_new_fpreg(),
	  {NewVar, insert(Var, NewVar, Map)};

	var ->
	  NewVar = hipe_rtl:mk_new_var(),
	  {NewVar, insert(Var, NewVar, Map)};
	reg ->
	  NewVar = hipe_rtl:mk_new_reg(),
	  {NewVar, insert(Var, NewVar, Map)}
      end;
    {value,NewVar} ->
      {NewVar, Map}
  end.

						%
% Simple type test
% 
type_of_var(X) ->
  case hipe_icode:is_fvar(X) of
    true ->
      fvar;
    false ->
      case hipe_icode:is_var(X) of
	true ->
	  var;
	false ->
	  case hipe_icode:is_reg(X) of
	    true ->
	      reg;
	    false ->
	      false
	  end
      end
  end.


%
% Convert icode label to a rtl variable
%

icode_label2rtl_label(LabelName, Map) ->
  case lookup(LabelName, Map) of
    {value, NewLabel} ->
      {NewLabel, Map};
    none ->
      NewLabel = hipe_rtl:mk_new_label(),
      {NewLabel, insert(LabelName, NewLabel,Map)}
  end.


new_var_map() ->
   gb_trees:empty().

lookup(V,Map) ->
  gb_trees:lookup(V,Map).

insert(Key,Val,Map) ->
  gb_trees:insert(Key,Val,Map).
