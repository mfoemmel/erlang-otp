%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%%  Filename : 	hipe_ceach.erl
%%  Module   :	hipe_ceach
%%  Purpose  :  Compile each function in a module, possibly
%%              applying a fun between each compilation.
%%              Usefull for bug-hunting by pinpointing a function 
%%              that when compiled causes a bug.
%%
%%  Notes    : 
%%  History  :	* 2001-12-11 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: happi $
%%              $Date: 2002/10/10 06:18:30 $
%%              $Revision: 1.3 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_ceach).
-export([c/2,c/1,c/3]).

c(Mod) ->
  [comp(Mod,F,A)
   || {F,A} <- Mod:module_info(functions)].

c(Mod,O) ->
  [comp(Mod,F,A,O)
   || {F,A} <- Mod:module_info(functions)].

c(Mod,O,Fn) ->
  [{comp(Mod,F,A,O),Fn()}
   || {F,A} <- Mod:module_info(functions)].

comp(Mod,F,A) ->
  io:format("~w:~w/~w...",[Mod,F,A]),
  hipe:c({Mod,F,A}),
  io:format("Ok\n").

comp(Mod,F,A,O) ->
  io:format("~w:~w/~w...",[Mod,F,A]),
  hipe:c({Mod,F,A},O),
  io:format("Ok\n").
