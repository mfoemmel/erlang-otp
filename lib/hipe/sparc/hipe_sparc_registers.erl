%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id$
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% hipe_sparc_registers.erl
%%

-module(hipe_sparc_registers).

-export([reg_name/1,
	 first_virtual/0,
	 is_precolored/1,
	 all_precolored/0,
	 allocatable/0,
	 is_fixed/1,
	 fixed/0,
	 number_of_physical/0,
	 register_args/0,
	 physical_name/1,
	 global/0,
	 stack_pointer/0,
	 stack_limit/0,
	 heap_pointer/0,
	 heap_limit/0,
	 return_address/0,
	 proc_pointer/0,
	 fcalls/0,
	 zero/0,
	 icc/0,
	 xcc/0,
	 y/0,
	 arg/1,
	 temp0/0,
	 temp1/0,
	 temp2/0,
	 temp3/0]).

%%
%% Registers are described by non-negative integers.
%% Numbers 0..35 denote physical registers:
%%	0..31 denote the standard integer registers.
%%	32 and 33 denote icc and xcc.
%%	34 denotes the y register.
%%	35 denotes zero/%g0 (XXX: why is this 35 instead of 0???)
%% Numbers > 35 denote virtual registers
%%

-define(G0,0).
-define(G1,1).
-define(G2,2).
-define(G3,3).
-define(G4,4).
-define(G5,5).
-define(G6,6).
-define(G7,7).
-define(O0,8).
-define(O1,9).
-define(O2,10).
-define(O3,11).
-define(O4,12).
-define(O5,13).
-define(O6,14).
-define(O7,15).
-define(L0,16).
-define(L1,17).
-define(L2,18).
-define(L3,19).
-define(L4,20).
-define(L5,21).
-define(L6,22).
-define(L7,23).
-define(I0,24).
-define(I1,25).
-define(I2,26).
-define(I3,27).
-define(I4,28).
-define(I5,29).
-define(I6,30).
-define(I7,31).
-define(ICC,32).
-define(XCC,33).
-define(Y,34).
-define(Z,?G0).
               

-define(NR_PHYSICAL,31).
-define(LAST_PRECOLOURED,35).

%%
%% Symbolic name of a register, to be used in assembly listings etc.
%%
reg_name(R) ->
    case R of
	?G0 -> "%g0";
	?G1 -> "%g1";
	?G2 -> "%g2";
	?G3 -> "%g3";
	?G4 -> "%g4";
	?G5 -> "%g5";
	?G6 -> "%g6";
	?G7 -> "%g7";
	?O0 -> "%o0";
	?O1 -> "%o1";
	?O2 -> "%o2";
	?O3 -> "%o3";
	?O4 -> "%o4";
	?O5 -> "%o5";
	?O6 -> "%sp";
	?O7 -> "%o7";
	?L0 -> "%l0";
	?L1 -> "%l1";
	?L2 -> "%l2";
	?L3 -> "%l3";
	?L4 -> "%l4";
	?L5 -> "%l5";
	?L6 -> "%l6";
	?L7 -> "%l7";
	?I0 -> "%i0";
	?I1 -> "%i1";
	?I2 -> "%i2";
	?I3 -> "%i3";
	?I4 -> "%i4";
	?I5 -> "%i5";
	?I6 -> "%fp";
	?I7 -> "%i7";
	%% these won't probably occur, but ...
	?ICC -> "%icc";
	?XCC -> "%xcc";
	?Y -> "%y";
	?Z  -> "%g0";	% ugly

        %% to handle code before regalloc:
        Other -> "%r" ++ integer_to_list(Other)
    end.

%%
%% Pre-allocated registers.
%%
-define(STACK_POINTER,?L6).
-define(STACK_LIMIT,?L7).
-define(HEAP_POINTER,?I1).
-define(HEAP_LIMIT,?I2).
-define(PROC_POINTER,?I0).
-define(FCALLS,?L5).
-define(RETURN_ADDRESS,?O7).
-define(ARG0,?O0).
-define(ARG1,?O1). 
-define(ARG2,?O2).
-define(ARG3,?O3).
-define(ARG4,?O4).
-define(TEMP0,?O5).	% used in emu <-> native transitions
-define(TEMP1,?I3).	% used in emu <-> native transitions
-define(TEMP2,?L4).	% used in emu <-> native transitions
-define(TEMP3,?L3).

%%
%% The lowest of the virtual registers.
%%
first_virtual() -> ?LAST_PRECOLOURED + 1.

%%
%% The number of physical registers
%%
number_of_physical() -> ?NR_PHYSICAL.

%%
%% True if a register number is precolored.
%%
is_precolored(X) -> X =< ?LAST_PRECOLOURED.

%%
%% The precolored registers.
%%
stack_pointer() -> ?STACK_POINTER.
stack_limit() -> ?STACK_LIMIT.
heap_pointer() -> ?HEAP_POINTER.
heap_limit() -> ?HEAP_LIMIT.
proc_pointer() -> ?PROC_POINTER.
fcalls() -> ?FCALLS.
return_address() -> ?RETURN_ADDRESS.
icc() -> ?ICC.
xcc() -> ?XCC.
y() -> ?Y.
zero() -> ?Z.
arg(X) ->
   case X of
      0 -> ?ARG0;
      1 -> ?ARG1;
      2 -> ?ARG2;
      3 -> ?ARG3;
      4 -> ?ARG4;
      Other -> exit({?MODULE, {"Argument out of range", Other}})
   end.
temp0() -> ?TEMP0.
temp1() -> ?TEMP1.
temp2() -> ?TEMP2.
temp3() -> ?TEMP3.

%%
%% A list of all allocatable regs
%%
%% Check sparc ABI for potential bugs ;-)
%%
allocatable() ->
   [     ?G1, ?G2, ?G3, ?G4, ?G5, ?G6,
                             ?O5,      ?O7,
    ?L0, ?L1, ?L2, ?L3, ?L4, ?L5, ?L6, ?L7,
    ?I0, ?I1, ?I2, ?I3, ?I4, ?I5     , ?I7,
   %% To discourage the regalloc from using argument registers they
   %% are placed at the end.  This should be handled somewhere else.
    ?O4,  ?O3, ?O2,  ?O1, ?O0].


%%
%% Fixed registers.
%%
is_fixed(?STACK_POINTER) -> true;
is_fixed(?STACK_LIMIT) -> true;
is_fixed(?HEAP_POINTER) -> true;
is_fixed(?HEAP_LIMIT) -> true;
is_fixed(?PROC_POINTER) -> true;
is_fixed(?FCALLS) -> true;
is_fixed(?RETURN_ADDRESS) -> true;
is_fixed(_) -> false.

fixed() ->
  [?STACK_POINTER, 
   ?STACK_LIMIT,
   ?HEAP_POINTER,
   ?HEAP_LIMIT,
   ?PROC_POINTER,
   ?FCALLS,
   ?RETURN_ADDRESS].

%%
%% Global registers. Always live, never saved in call frames.
%%
global() ->
   [?STACK_POINTER,
    ?STACK_LIMIT,
    ?HEAP_POINTER,
    ?HEAP_LIMIT,
    ?PROC_POINTER,
    ?FCALLS].

%%
%% A list of all precolored regs
%%
all_precolored() ->
   [?STACK_POINTER,
    ?STACK_LIMIT,
    ?HEAP_POINTER,
    ?HEAP_LIMIT,
    ?PROC_POINTER,
    ?FCALLS,
    ?RETURN_ADDRESS,
    ?Z,
    ?XCC,
    ?ICC,
    ?Y,
    ?ARG0,
    ?ARG1,
    ?ARG2,
    ?ARG3,
    ?ARG4].

%%
%% The number of arguments that are passed in registers.
%%
register_args() -> 5.

%%
%% The actual register number a precolored register should use.
%% This is the same as the precolored name for all but the zero-register.
%% XXX: why? this deserves to die!
%% 
%%physical_name(?Z) -> 0;
physical_name(P) -> P.
