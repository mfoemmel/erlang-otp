%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2002 by Erik Johansson.  
%% ====================================================================
%%  Filename : 	hipe_jit.erl
%%  Module   :	hipe_jit
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2002-03-14 Erik Johansson (happi@csd.uu.se): Created.
%%  CVS      :
%%              $Author: kostis $
%%              $Date: 2008/03/07 22:40:33 $
%%              $Revision: 1.4 $
%% ====================================================================
%% @doc
%%    A tool to enable using the HiPE compiler as an automatic JIT
%%    compiler rather than a user-controlled one.
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_jit).

-export([start/0]).

-record(state, {mode = start     :: 'sleep' | 'start' | 'wait',
	       	threshold = 5000 :: non_neg_integer(),
		sleep = 5000     :: non_neg_integer(),
		time = 1000      :: non_neg_integer()}).

%%---------------------------------------------------------------------

-spec(start/0 :: () -> pid()).
%% @doc
%%    Starts an Erlang process which calls the HiPE compiler every
%%    now and then (when it sees it fit to do so).
%% @end
start() ->
  spawn(fun () -> loop(#state{}) end).

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
  catch hipe_profile:clear(),
  loop(State#state{mode=wait}).

wait(State) ->
  receive 
    quit -> ok
  after State#state.time ->
    R = [M || {M,C} <- (catch hipe_profile:mods_res()),
			C > State#state.threshold],
    catch hipe_profile:prof_off(),
    lists:foreach(fun(M) ->
		    io:format("Compile ~w\n",[M]),
		    hipe:c(M,[o2,verbose])
		  end, R)
  end,
  loop(State#state{mode=sleep}).
