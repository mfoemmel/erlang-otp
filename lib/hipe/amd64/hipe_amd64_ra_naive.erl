%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

-define(HIPE_X86_RA_NAIVE, hipe_amd64_ra_naive).
-define(HIPE_X86_REGISTERS, hipe_amd64_registers).
-define(HIPE_X86_SPECIFIC_FP, hipe_amd64_specific_sse2).
-define(ECX, rcx).
-define(MOVE64, #move64{} -> do_move64(I)).
-define(DO_JMP_SWITCH,
	do_jmp_switch(I) ->
	   #jmp_switch{temp=Temp, jtab=Tab} = I,
	   case temp_is_pseudo(Temp) of
	     false ->
	       case temp_is_pseudo(Tab) of
		 false ->
		   [I];
		 true ->
		   Reg = hipe_x86:mk_temp(hipe_amd64_registers:temp0(),
					  'untagged'),
		   [hipe_x86:mk_move(Temp, Reg), I#jmp_switch{jtab=Reg}]
	       end;
	     true ->
	       Reg = hipe_x86:mk_temp(hipe_amd64_registers:temp1(),
				      'untagged'),
	       case temp_is_pseudo(Tab) of
		 false ->
		   [hipe_x86:mk_move(Temp, Reg), I#jmp_switch{temp=Reg}];
		 true ->
		   Reg2 = hipe_x86:mk_temp(hipe_amd64_registers:temp0(),
					   'untagged'),
		   [hipe_x86:mk_move(Temp, Reg),
		    hipe_x86:mk_move(Tab, Reg2),
		    I#jmp_switch{temp=Reg, jtab=Reg2}]
	       end
	   end).

-define(DO_MOVE64,
	do_move64(I) ->
	   #move64{dst=Dst} = I,
	   case is_mem_opnd(Dst) of
	     false ->
	       [I];
	     true ->     
	       Reg = hipe_amd64_registers:temp1(),
	       NewDst = clone(Dst, Reg),
	       [I#move64{dst=NewDst}, hipe_x86:mk_move(NewDst, Dst)]
	   end).


-include("../x86/hipe_x86_ra_naive.erl").
