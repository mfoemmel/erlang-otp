-module(hipe_timing).
-export([start_timer/0,stop_timer/1]).

%-export([reset/0,         % resets the timer
%	 time/0,          % deprecated, run_time()-gc_time()
%	 real_time/0,     % wall clock since reset
%	 run_time/0,      % user+system time since reset
%	 usr_time/0,      % user time since reset
%	 sys_time/0,      % system time since reset
%	 gc_time/0,       % user+system time during gc since reset
%	 gc_usr_time/0,   % user time during gc since reset
%	 gc_sys_time/0]). % system time during gc since reset


%reset() ->
%   statistics(reset_timer).

%time() ->
%   run_time() - gc_time().

%real_time() ->
%   statistics(real_time).

%run_time() ->
%   statistics(usr_time) + statistics(sys_time).

%usr_time() ->
%   statistics(usr_time).

%sys_time() ->
%   statistics(sys_time).


%gc_time() ->
%   statistics(gc_usr_time) + statistics(gc_sys_time).

%gc_usr_time() ->
%   statistics(gc_usr_time).

%gc_sys_time() ->
%   statistics(gc_sys_time).


start_timer() ->
  {Total,Last} = erlang:statistics(runtime),
  Total.

stop_timer(T) ->
  {Total,Last} = erlang:statistics(runtime),
  Total - T.

