%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% Time-stamp: <01/04/10 15:53:57 happi>
%% ====================================================================
%%  Filename : 	hipe_rtl_varmap.erl
%%  Module   :	hipe_rtl_varmap
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-04-10 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: happi $
%%              $Date: 2001/04/10 22:42:27 $
%%              $Revision: 1.1 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_rtl_varmap).
-export([init/1,
	 ivs2rvs/2,
	 icode_var2rtl_var/2,
	 icode_label2rtl_label/2,
	 new_var_map/0]).

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
    {IVMin, IVMax} = hipe_icode:icode_var_range(ICode),
    {ILMin, ILMax} = hipe_icode:icode_label_range(ICode),
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
    {Args, VarMap1} = ivs2rvs(hipe_icode:icode_params(Fun), VarMap).


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
  case hipe_icode:is_var(Var) of 
    true ->
      case lists:keysearch(Var, 1, Map) of
	{value, {_, NewVar}} ->
	  {NewVar, Map};
	false ->
	  NewVar = hipe_rtl:mk_new_var(),
	  {NewVar, [{Var, NewVar}|Map]}
      end;
    false ->
      {Var, Map}
  end.

%
% Convert icode label to a rtl variable
%

icode_label2rtl_label(LabelName, Map) ->
   case lists:keysearch(LabelName, 1, Map) of
      {value, {_, NewLabel}} ->
	 {NewLabel, Map};
      false ->
	 NewLabel = hipe_rtl:mk_new_label(),
	 {NewLabel, [{LabelName, NewLabel}|Map]}
   end.


new_var_map() ->
   [].

