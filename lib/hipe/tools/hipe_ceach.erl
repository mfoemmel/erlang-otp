%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% ====================================================================
%%  Filename : 	hipe_ceach.erl
%%  Module   :	hipe_ceach
%%  Purpose  :  Compile each function in a module, possibly
%%              applying a fun between each compilation.
%%              Useful for bug-hunting by pinpointing a function 
%%              that when compiled causes a bug.
%%
%%  Notes    : 
%%  History  :	* 2001-12-11 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: kostis $
%%              $Date: 2007/02/17 09:03:56 $
%%              $Revision: 1.5 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_ceach).
-export([c/1, c/2, c/3]).

c(Mod) ->
  lists:foreach(fun({F,A}) -> comp(Mod, F, A) end,
		Mod:module_info(functions)).

c(Mod, O) ->
  lists:foreach(fun({F,A}) -> comp(Mod, F, A, O) end,
		Mod:module_info(functions)).

c(Mod, O, Fn) ->
  lists:foreach(fun({F,A}) -> comp(Mod, F, A, O), Fn() end,
		Mod:module_info(functions)).

comp(Mod, F, A) ->
  io:format("~w:~w/~w... ", [Mod, F, A]),
  hipe:c({Mod, F, A}),
  io:format("OK\n").

comp(Mod, F, A, O) ->
  io:format("~w:~w/~w... ", [Mod, F, A]),
  hipe:c({Mod, F, A}, O),
  io:format("OK\n").
