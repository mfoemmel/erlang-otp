%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%%  Filename : 	hipe_sparc_stack_descriptors.erl
%%  Module   :	hipe_sparc_stack_descriptors
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-11-20 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: happi $
%%              $Date: 2002/05/13 16:51:09 $
%%              $Revision: 1.6 $
%% ====================================================================
%%  Exports  :
%% 
%% @type sdesc(Size::integer(),LiveSlots::[integer()],Exeception::bool(), 
%%                      Arity::integer()).
%% A sdesc has the following properties foo
%% <ul>
%%  <li> Size -- The size in words of local temps.</li>
%%  <li> LiveSlots -- A list of slots that are live at the call site, 
%%                               The slots are counted from the stack pointer,
%%                               with position 0 == SP-1</li>
%%  <li> Exception -- True if this call is within an exception handler.</li>
%%  <li> Arity -- The stack-arity of the caller i.e. the number of arguments
%%                         on the stack before the return address.</li>
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

-module(hipe_sparc_stack_descriptors).
-export([empty/0,
	set_live/2,
	get_live/1,
	set_size/2,
	get_size/1,
	set_exn/2,
	get_exn/1,
	set_arity/2,
	get_arity/1,
	pp/2]).

-record(stack_desc,{size=0,live_slots=[],exn=[], arity=0}).

%% @spec empty() -> sdesc()
empty() ->
  #stack_desc{}.

set_arity(SD, Exn) ->
  SD#stack_desc{arity=Exn}.

set_exn(SD, Exn) ->
  SD#stack_desc{exn=Exn}.

set_live(SD, Live) ->
  SD#stack_desc{live_slots=Live}.

set_size(SD, FSize) ->
  SD#stack_desc{size=FSize}.

pp(Dev,SD) ->
  io:format(Dev, "<|~w| Live: ~w>", [get_size(SD),get_live(SD)]).

get_live(SD)->
  SD#stack_desc.live_slots.

get_size(SD)->
  SD#stack_desc.size.

get_exn(SD)->
  SD#stack_desc.exn.


get_arity(SD)->
  SD#stack_desc.arity.


