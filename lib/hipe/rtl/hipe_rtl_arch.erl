%% -*- erlang-indent-level: 4 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% Time-stamp: <01/04/10 14:20:15 happi>
%% ====================================================================
%%  Filename : 	hipe_rtl_arch.erl
%%  Module   :	hipe_rtl_arch
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-04-10 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: mikpe $
%%              $Date: 2001/09/13 19:53:30 $
%%              $Revision: 1.5 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_rtl_arch).
-export([first_virtual_reg/0, 
	 heap_pointer_reg/0,
	 heap_limit_reg/0,
	 fcalls_reg/0,
	 add_ra_reg/1,
	 reg_name/1,
	 is_precoloured/1,
	 live_at_return/0,
	 pcb_load/2,
	 pcb_store/2,
	 call_bif/5
	]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ____________________________________________________________________
%%      
%% ARCH-specific stuff
%% ____________________________________________________________________
%% 
%%
%% XXX: x86 might not _have_ real registers for some of these things
%%

first_virtual_reg() ->
    case get(hipe_target_arch) of
	ultrasparc ->
	    hipe_sparc_registers:first_virtual();
	x86 ->
	    hipe_x86_registers:first_virtual()
    end.

heap_pointer_reg() ->
    case get(hipe_target_arch) of
	ultrasparc ->
	    hipe_sparc_registers:heap_pointer();
	x86 ->
	    hipe_x86_registers:heap_pointer()
    end.

heap_limit_reg() ->
    case get(hipe_target_arch) of
	ultrasparc ->
	    hipe_sparc_registers:heap_limit();
	x86 ->
	    hipe_x86_registers:heap_limit()
    end.

fcalls_reg() ->
    case get(hipe_target_arch) of
	ultrasparc ->
	    hipe_sparc_registers:fcalls();
	x86 ->
	    hipe_x86_registers:fcalls()
    end.

add_ra_reg(Rest) ->
    case get(hipe_target_arch) of
	ultrasparc ->
	    [hipe_rtl:mk_var(hipe_sparc_registers:return_address()) | Rest];
	x86 ->
	    Rest
    end.

reg_name(Reg) ->
    case get(hipe_target_arch) of
	ultrasparc ->
	    hipe_sparc_registers:reg_name(Reg);
	x86 ->
	    hipe_x86_registers:reg_name(Reg)
    end.

is_precoloured(Reg) ->
    case get(hipe_target_arch) of
	ultrasparc ->
	    hipe_sparc_registers:is_precolored(Reg);
	x86 ->
	    hipe_x86_registers:is_precoloured(Reg)
    end.

live_at_return() ->
    case get(hipe_target_arch) of
	ultrasparc ->
	    ordsets:from_list(lists:map(fun(R) -> hipe_rtl:mk_reg(R) end,
					hipe_sparc_registers:global()));
	x86 ->
	    ordsets:from_list(lists:map(fun({R,_}) -> hipe_rtl:mk_reg(R) end,
					hipe_x86_registers:live_at_return()))
    end.

%%%
%%% PCB accesses.
%%% Wrapped to avoid leaking the PCB pointer to the wrong places.
%%%

pcb_load(Dst, Off) ->
    hipe_rtl:mk_load(Dst, proc_pointer(), hipe_rtl:mk_imm(Off)).

pcb_store(Off, Src) ->
    hipe_rtl:mk_store(proc_pointer(), hipe_rtl:mk_imm(Off), Src).

proc_pointer() ->	% must not be exported
    case get(hipe_target_arch) of
	ultrasparc ->
	    hipe_rtl:mk_reg(hipe_sparc_registers:proc_pointer());
	x86 ->
	    hipe_rtl:mk_reg(hipe_x86_registers:proc_pointer())
    end.

%%%
%%% Special BIF calls.
%%% Wrapped to avoid leaking the PCB pointer to the wrong places,
%%% and to allow ARCH-specific expansion.
%%%

call_bif(Dst, Name, Args, Cont, Fail) ->
    NewArgs =
	case prefix_pptr(Name) of
	    true ->
		[proc_pointer() | Args];
	    false ->
		Args
	end,
    hipe_rtl:mk_call(Dst, Name, NewArgs, c, Cont, Fail).

prefix_pptr(Name) ->
    case get(hipe_target_arch) of
	ultrasparc ->
	    sparc_prefix_pptr(Name);
	x86 ->
	    false
    end.

sparc_prefix_pptr(mbox_empty)		-> true;
sparc_prefix_pptr(get_msg)		-> true;
sparc_prefix_pptr(next_msg)		-> true;
sparc_prefix_pptr(select_msg)		-> true;
sparc_prefix_pptr(bs_get_integer)	-> true;
sparc_prefix_pptr(bs_get_float)		-> true;
sparc_prefix_pptr(bs_get_binary_all)	-> true;
sparc_prefix_pptr(bs_get_binary)	-> true;
sparc_prefix_pptr(bs_final)		-> true;
sparc_prefix_pptr(Name) -> exit({?MODULE,sparc_prefix_pptr,Name}).
