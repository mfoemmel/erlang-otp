%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2002 by Erik Johansson.  
%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%%  Filename : 	hipe_jit.erl
%%  Module   :	hipe_jit
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2002-03-14 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: happi $
%%              $Date: 2002/03/19 00:01:53 $
%%              $Revision: 1.1 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_jit).
-export([start/0]).

-record(state,{mode=start,threshold=5000,sleep=5000,time=1000}).

start() ->
  spawn(fun() ->
	    loop(#state{})
	end).

loop(State) ->
  case State#state.mode of
    start ->
      start(State);
    wait ->
      wait(State);
    _ ->
      sleep(State)
  end.

sleep(State) ->
  receive 
    quit -> ok
  after State#state.sleep ->
      loop(State#state{mode=start})
  end.

start(State) ->
  catch hipe_profile:prof(),
   catch  hipe_profile:clear(),
  loop(State#state{mode=wait}).
      
wait(State) ->
  receive 
    quit -> ok
  after State#state.time ->
      R = [M || {M,C} <-   (catch hipe_profile:mods_res()), C > 
		  State#state.threshold],
      catch  hipe_profile:prof_off(),
      lists:foreach(fun(M) ->
			io:format("Compile ~w\n",[M]),
			hipe:c(M,[o2,verbose])
		    end, R)
      
  end,
  loop(State#state{mode=sleep}).
