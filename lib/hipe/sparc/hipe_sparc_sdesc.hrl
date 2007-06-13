%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%%  Filename : 	hipe_sparc_sdesc.hrl
%%  Purpose  :  
%%  Notes    :  
%%  History  :	* 2001-11-20 Erik Johansson (happi@csd.uu.se): Created.
%%  CVS      :
%%              $Author: kostis $
%%              $Date: 2007/05/12 09:31:04 $
%%              $Revision: 1.3 $
%% ====================================================================
%% Implements :
%% 
%% @type sdesc().
%% A sdesc has the following properties
%% <ul>
%%  <li> <code>Size::integer()</code> -- The size in words of local
%%  temps.</li>
%%  <li> <code>LiveSlots::[integer()]</code> -- A list of slots that are
%%  live at the call site, The slots are counted from the stack pointer,
%%  with position <code>0 == SP-1</code></li>
%%  <li> <code>Exception::bool()</code> -- True if this call is within
%%  an exception handler.</li>
%%  <li> <code>Arity::integer()</code> -- The stack-arity of the caller
%%  i.e. the number of arguments on the stack before the return
%%  address.</li>
%% </ul>
%% <p> The total size of a stack frame is Size+Arity+1. (The +1 is for the
%%         return address.)
%% </p>
%% Example of how a frame in a function with 5 arguments in registers
%% and 2 arguments on the stack, 4 local variables on the stack (2 of 
%% them live).
%% At the moment the sparc stack grows towards higher addresses,
%% but downwards in this picture.
%% <pre>
%%     | PREV FRAME |
%%     |------------| 
%%     | Arg 6      | 
%%     | Arg 7      | Arity = 2
%%     |------------|
%%     | RA         |
%%     |------------|
%%     | Spill 3    |
%%  *  | Spill 2    |
%%     | Spill 1    | Size = 4
%%  *  | Spill 0    |
%%     |------------|
%%     |  F R E E   | - SP
%%
%% LiveSlots = [0,2]
%% </pre>
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([sdesc_size/1, sdesc_size_update/2,
	 sdesc_live_slots/1, sdesc_live_slots_update/2,
	 sdesc_arity/1, sdesc_arity_update/2,
	 pp_sdesc/2]).

-record(stack_desc, {size       = 0  :: integer(),
		     live_slots = [] :: [integer()],
		     arity      = 0  :: integer()}).

%% @spec sdesc_mk_empty() -> sdesc()
sdesc_mk_empty() ->
  #stack_desc{}.

sdesc_size(#stack_desc{size=Size}) when is_integer(Size), 0 =< Size -> Size.
sdesc_live_slots(#stack_desc{live_slots=Slots}) -> Slots.
sdesc_arity(#stack_desc{arity=A}) when is_integer(A), 0 =< A -> A.

sdesc_size_update(SD, FSize) when is_integer(FSize), 0 =< FSize ->
  SD#stack_desc{size=FSize}.
sdesc_live_slots_update(SD, Live) ->
  SD#stack_desc{live_slots=Live}.
sdesc_arity_update(SD, Exn) when is_integer(Exn), 0 =< Exn ->
  SD#stack_desc{arity=Exn}.

pp_sdesc(Dev,SD) ->
  io:format(Dev, "<|~w| Live: ~w>", [sdesc_size(SD),sdesc_live_slots(SD)]).
