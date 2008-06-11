%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% Time-stamp: <2008-04-20 14:53:36 richard>
%% ====================================================================
%%  Filename : 	hipe_timer.erl
%%  Module   :	hipe_timer
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-03-15 Erik Johansson (happi@it.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: richardc $
%%              $Date: 2008/04/20 13:01:14 $
%%              $Revision: 1.4 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_timer).
-export([tr/1,t/1,timer/1,time/1,empty_time/0]).
-export([advanced/2]).

t(F) ->
  {EWT,ERT} = empty_time(),
  {WT,RT} = time(F),
  {WT-EWT,(RT-ERT)/1000}.

tr(F) ->
  {EWT,ERT} = empty_time(),
  {R,{WT,RT}} = timer(F),
  {R,{WT-EWT,(RT-ERT)/1000}}.
  

empty_time() ->
  {WT1,WT2,WT3}=erlang:now(),
  {A,_}=erlang:statistics(runtime),
  {WT12,WT22,WT32}=erlang:now(),
  {B,_}=erlang:statistics(runtime),
  {(WT12-WT1)*1000000+(WT22-WT2)+(WT32-WT3)/1000000,B-A}.

time(F) -> 
  {WT1,WT2,WT3}=erlang:now(),
  {A,_}=erlang:statistics(runtime),

  F(),

  {WT12,WT22,WT32}=erlang:now(),
  {B,_}=erlang:statistics(runtime),

  {(WT12-WT1)*1000000+(WT22-WT2)+(WT32-WT3)/1000000,B-A}.

timer(F) -> 
  {WT1,WT2,WT3}=erlang:now(),
  {A,_}=erlang:statistics(runtime),

  R = F(),

  {WT12,WT22,WT32}=erlang:now(),
  {B,_}=erlang:statistics(runtime),

  {R,{(WT12-WT1)*1000000+(WT22-WT2)+(WT32-WT3)/1000000,B-A}}.

advanced(_Fun, I) when I < 2 -> false;
advanced(Fun, Iterations) ->
  S = lists:seq(1,Iterations),
  R = Fun(),
  Measurements = 
    lists:map(fun (_) ->
		  t(Fun)
	      end, S),
  {Wallclock, RunTime} = split(Measurements),
  WMin = min(Wallclock),
  RMin = min(RunTime),
  WMax = max(Wallclock),
  RMax = max(RunTime),
  WMean = mean(Wallclock),
  RMean = mean(RunTime),
  WMedian = median(Wallclock),
  RMedian = median(RunTime),
  WVariance = variance(Wallclock),
  RVariance = variance(RunTime),
  WStddev = stddev(Wallclock),
  RStddev = stddev(RunTime),
  WVarCoff = 100 * WStddev / WMean,
  RVarCoff = 100 * RStddev / RMean,
  WSum = lists:sum(Wallclock),
  RSum = lists:sum(RunTime),

  [{wallclock,[{min,WMin},
	       {max,WMax},
	       {mean,WMean},
	       {median,WMedian},
	       {variance,WVariance},
	       {stdev,WStddev},
	       {varcoff, WVarCoff},
	       {sum, WSum},
	       {values,Wallclock}	       
	       ]},
  {runtime,[{min,RMin},
	       {max,RMax},
	       {mean,RMean},
	       {median,RMedian},
	       {variance,RVariance},
	       {stdev,RStddev},
	       {varcoff, RVarCoff},
	       {sum, RSum},
	       {values,RunTime}	       
	       ]},
   {iterations, Iterations},
   {result,R}
   ].


min([V|Vs]) ->
  min(Vs,V).
min([V|Vs], Min) when V >= Min ->
  min(Vs,Min);
min([V|Vs], _) ->
  min(Vs, V);
min([],Min) -> Min.

max([V|Vs]) ->
  max(Vs,V).
max([V|Vs], Max) when V =< Max ->
  max(Vs,Max);
max([V|Vs], _) ->
  max(Vs, V);
max([],Max) -> Max.

  
split(M) -> 
  split(M,[],[]).

split([{W,R}|More], AccW, AccR) ->
  split(More, [W|AccW], [R|AccR]);
split([],AccW, AccR) ->
  {AccW, AccR}.
  

mean(L) ->
  mean(L,0,0).

mean([V|Vs], No, Sum) ->
  mean(Vs,No+1,Sum+V);
mean([], No, Sum) when No > 0 ->
  Sum/No;
mean([], _No, _Sum) ->
  exit(empty_list).
  
median(L) ->
  S = length(L),
  SL = lists:sort(L),
  case even(S) of
    true ->
      (lists:nth((S div 2),SL) +
       lists:nth((S div 2)+1,SL)) / 2;
    false ->
       lists:nth((S div 2),SL)
  end.

even(S) ->
  (S band 1) =:= 0.

%% diffs(L,V) ->
%%   lists:map(fun(X) ->
%%		X - V
%%	    end, L).

square_diffs(L,V) ->
  lists:map(fun(X) ->
		D = (X - V),
		D * D
	    end, L).


variance(L) ->
  Mean = mean(L),
  N = length(L),
  if N > 1 ->
      lists:sum(square_diffs(L,Mean)) / (N-1);
     true -> exit(two_few_values)
  end.

stddev(L) ->
  math:sqrt(variance(L)).
