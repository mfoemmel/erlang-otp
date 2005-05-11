%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id: hipe_sparc_registers.erl,v 1.24 2005/01/19 11:59:40 kostis Exp $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% File: hipe_sparc_registers.erl
%%
%% @doc
%% See the file: OTP_DIR/erts/emulator/hipe/hipe_sparc_abi.txt
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc_registers).

-export([reg_name/1,
	 fpreg_name/1,
	 first_virtual/0,
	 %% call_clobbered/0,
	 is_precoloured/1,
	 all_precoloured/0,
	 allocatable/0,
	 is_fixed/1,
	 %% fixed/0,
	 %% number_of_physical/0,
	 register_args/0,
	 register_rets/0,
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
	 %% fcc/1,
	 %% y/0,
	 arg/1,
	 ret/1,
	 temp0/0,
	 temp1/0,
	 temp2/0,
	 %% temp3/0,
	 cpsave/0,
	 %% cplink/0
         alignment/0
	]).

-include("../rtl/hipe_literals.hrl").


%%
%% Registers are described by non-negative integers.
%% Numbers 0..38 denote physical registers:
%%	0..31 denote the standard integer registers.
%%	32 and 33 denote icc and xcc.
%%            34 to 37 denote fcc0 to fcc3
%%	38 denotes the y register.
%% Numbers > 38 denote virtual registers
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
-define(FCC0,34).
-define(FCC1,35).
-define(FCC2,36).
-define(FCC3,37).
-define(Y,38).
-define(Z,?G0).
               

-define(NR_PHYSICAL,31).
-define(LAST_PRECOLOURED,38).


%% call_clobbered() ->
%%   allocatable().


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
	?FCC0 -> "%fcc0";
	?FCC1 -> "%fcc1";
	?FCC2 -> "%fcc2";
	?FCC3 -> "%fcc3";
	?Y -> "%y";
        %% to handle code before regalloc:
        Other -> "%r" ++ integer_to_list(Other)
    end.



%% TODO: Consider cleaning up this function.
fpreg_name(FR) ->
  "%f" ++ integer_to_list(FR).

%%
%% Pre-allocated registers.
%%
-define(STACK_POINTER,?SPARC_REG_NSP).
-define(STACK_LIMIT,?SPARC_REG_NSP_LIMIT).
-define(HEAP_POINTER,?SPARC_REG_HP).
-define(HEAP_LIMIT,?SPARC_REG_HP_LIMIT).
-define(PROC_POINTER,?SPARC_REG_P).
-define(FCALLS,?SPARC_REG_FCALLS).
-define(RETURN_ADDRESS,?SPARC_REG_RA).
-define(ARG0,?SPARC_REG_ARG0).
-define(ARG1,?SPARC_REG_ARG1). 
-define(ARG2,?SPARC_REG_ARG2).
-define(ARG3,?SPARC_REG_ARG3).
-define(ARG4,?SPARC_REG_ARG4).
-define(ARG5,?SPARC_REG_ARG5).
-define(ARG6,?SPARC_REG_ARG6). 
-define(ARG7,?SPARC_REG_ARG7).
-define(ARG8,?SPARC_REG_ARG8).
-define(ARG9,?SPARC_REG_ARG9).
-define(ARG10,?SPARC_REG_ARG10).
-define(ARG11,?SPARC_REG_ARG11). 
-define(ARG12,?SPARC_REG_ARG12).
-define(ARG13,?SPARC_REG_ARG13).
-define(ARG14,?SPARC_REG_ARG14).
-define(ARG15,?SPARC_REG_ARG15).
-define(TEMP0,?SPARC_REG_TEMP0).%% used in emu <-> native transitions
-define(TEMP1,?SPARC_REG_TEMP1).	%% used in emu <-> native transitions
-define(TEMP2,?SPARC_REG_TEMP2).	%% used in emu <-> native transitions
-define(TEMP3,?SPARC_REG_TEMP3).

-define(CPSAVE, ?SPARC_REG_TEMP2).   %% used in calls to inc_stack.
-define(CPLINK, ?SPARC_REG_TEMP1).   %% see hipe_sparc_glue.S

%%
%% The lowest of the virtual registers.
%%
first_virtual() -> ?LAST_PRECOLOURED + 1.

%%
%% The number of physical registers
%%
%% number_of_physical() -> ?NR_PHYSICAL.

%%
%% True if a register number is precoulored.
%%
is_precoloured(X) -> X =< ?LAST_PRECOLOURED.

%%
%% The precoloured registers.
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
%% fcc(N) -> 
%%   case N of
%%     0 -> ?FCC0;
%%     1 -> ?FCC1;
%%     2 -> ?FCC2;
%%     3 -> ?FCC3
%%   end.
%% y() -> ?Y.
zero() -> ?Z.
arg(X) ->
   case X of
      0 -> ?ARG0;
      1 -> ?ARG1;
      2 -> ?ARG2;
      3 -> ?ARG3;
      4 -> ?ARG4;
      5 -> ?ARG5;
      6 -> ?ARG6;
      7 -> ?ARG7;
      8 -> ?ARG8;
      9 -> ?ARG9;
      10 -> ?ARG10;
      11 -> ?ARG11;
      12 -> ?ARG12;
      13 -> ?ARG13;
      14 -> ?ARG14;
      15 -> ?ARG15;
      Other -> exit({?MODULE, {"Argument out of range", Other}})
   end.

ret(X) ->
   case X of
      0 -> ?ARG15;
      1 -> ?ARG0;
      2 -> ?ARG1;
      3 -> ?ARG2;
      4 -> ?ARG3;
      5 -> ?ARG4;
      6 -> ?ARG5;
      7 -> ?ARG6;
      8 -> ?ARG7;
      9 -> ?ARG8;
      10 -> ?ARG9;
      11 -> ?ARG10;
      12 -> ?ARG11;
      13 -> ?ARG12;
      14 -> ?ARG13;
      15 -> ?ARG14;
      Other -> exit({?MODULE, {"Ret value of range", Other}})
   end.
temp0() -> ?TEMP0.
temp1() -> ?TEMP1.
temp2() -> ?TEMP2.
%% temp3() -> ?TEMP3.
cpsave() -> ?CPSAVE.
%% cplink() -> ?CPLINK.
  

%%
%% A list of all allocatable regs
%% see .../erts/emulator/hipe/hipe_sparc_abi.txt
%%   http://soldc.sun.com/articles/sparcv9abi.html
%%   http://www.users.qwest.net/~eballen1/sparc.tech.links.html
%%   http://compilers.iecc.com/comparch/article/93-12-073
%%
%%  Global registers summary
%%  Reg  | SPARC V8 (32-bit) | SPARC V8PLUS (64-bit) | SPARC V9 (64-bit) 
%%  %g0  | Constant 0        | Constant 0            | Constant 0 
%%  %g1  | Scratch           | Scratch               | Scratch 
%%  %g2  | Application       | Application           | Application 
%%  %g3  | Application       | Application           | Application 
%%  %g4  | Application       | Application           | Scratch 
%%  %g5  | System            | Scratch               | Scratch 
%%  %g6  | System            | System                | System 
%%  %g7  | System            | System                | System 
%%                             <Current ARCH>  
%%
%% Does gcc generate code that uses %g2 to %g4 ?
%%  If not these could perhaps be used for P, HP, H-Limit
%%

allocatable() ->
  %% To discourage the regalloc from using argument registers they
  %% are placed at the end. This should be handled somewhere else.
  [?TEMP3, ?TEMP2, ?TEMP1, ?ARG14, ?ARG13, ?ARG12,
   ?ARG11, ?ARG10, ?ARG9, ?ARG8, ?ARG7, ?ARG6, ?ARG5, 
   ?ARG4, ?ARG3, ?ARG2, ?ARG1, ?ARG0, ?ARG15]. 

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
%% A list of all precoulored regs
%%
all_precoloured() ->
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
    ?TEMP0,
    ?TEMP1,
    ?TEMP2,
    ?TEMP3,
    ?ARG0,
    ?ARG1,
    ?ARG2,
    ?ARG3,
    ?ARG4,
    ?ARG5,
    ?ARG6,
    ?ARG7,
    ?ARG8,
    ?ARG9,
    ?ARG10,
    ?ARG11,
    ?ARG12,
    ?ARG13,
    ?ARG14,
    ?ARG15
].

%%
%% The number of arguments that are passed in registers.
%%
register_args() -> ?SPARC_ARGS_IN_REGS.
register_rets() -> ?SPARC_ARGS_IN_REGS.

%%
%% The actual register number a precoulored register should use.
physical_name(P) -> P.

alignment() -> 4.

