%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% ====================================================================
%%  Filename : 	hipe_rtl_exceptions.erl
%%  Module   :	hipe_rtl_exceptions
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-04-10 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%      $Id$
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_rtl_exceptions).

-export([gen_exception/2,
	 gen_exit_atom/2,
	 gen_fail_code/4,
	 gen_funcall_fail/5,
	 exit_code/1,
	 gen_fail/3,
	 gen_restore_catch/3]).

-include("../main/hipe.hrl").
-include("hipe_literals.hrl").

%% ____________________________________________________________________
%% Handle the Icode instruction
%% FAIL
%%
gen_fail(I, FailReason, ExitInfo) ->
  case FailReason of
    [Reason] ->
      case hipe_icode:fail_type(I) of
	exit -> 
	  gen_exit(Reason);
	throw ->
	  gen_throw(Reason);
	fault ->
	  gen_fault(Reason,ExitInfo)
      end;
    [Arg1,Arg2] ->
      case hipe_icode:fail_type(I) of
%% NEW_EXCEPTIONS
%% 	error ->
 	fault2 ->
	  Reason = Arg1, Trace = Arg2,
	  gen_fault2(Reason,Trace,ExitInfo);
	'raise' ->
	  Reason = Arg1, Type = Arg2,
	  gen_raise(Reason,Type,ExitInfo)
      end
  end.

%% ____________________________________________________________________
%% RESTORE_CATCH
%%
gen_restore_catch(I, VarMap, ConstTab) ->
  %% This is the exception handler, we have to get the exception value
  %% into ReasonDst, and the exception type into TypeDst

  FValueVar = hipe_rtl:mk_new_var(),
  TmpVar = hipe_rtl:mk_new_var(),
  FreasonReg = hipe_rtl:mk_new_reg(),
  NoExitLbl = hipe_rtl:mk_new_label(),
  ExitLbl = hipe_rtl:mk_new_label(), 
  ThrowLbl = hipe_rtl:mk_new_label(), 
  ThrowLblName = hipe_rtl:label_name(ThrowLbl),
  NoThrowLbl = hipe_rtl:mk_new_label(), 
  NoThrowLblName = hipe_rtl:label_name(NoThrowLbl),
  NoExitLblName = hipe_rtl:label_name(NoExitLbl),
  ExitLblName = hipe_rtl:label_name(ExitLbl),
  EndLbl = hipe_rtl:mk_new_label(), 
  EndLblName  = hipe_rtl:label_name(EndLbl),
  case hipe_icode:restore_catch_type(I) of
    'catch' ->
      {[ReasonDst], Map0} = 
	hipe_rtl_varmap:ivs2rvs([hipe_icode:restore_catch_reason_dst(I)],
				VarMap),
      {[
	%% NOTE: restore_catch must be the first instruction in the
	%% catch handler
	hipe_rtl:mk_restore_catch([FValueVar]),
	hipe_rtl_arch:pcb_load(FreasonReg, ?P_FREASON),
	hipe_rtl:mk_branch(FreasonReg, eq, hipe_rtl:mk_imm(?EXTAG_THROWN),
			   NoExitLblName, ExitLblName, 0.45),
%% NEW_EXCEPTIONS
%% 	hipe_rtl:mk_alu(FreasonReg, FreasonReg, 'and',
%% 			hipe_rtl:mk_imm(?EXC_CLASSBITS)),
%% 	hipe_rtl:mk_branch(FreasonReg, eq, hipe_rtl:mk_imm(?EXTAG_THROWN), 
%% 			   ThrowLblName, NoThrowLblName, 0.45),
%% 	NoThrowLbl,
%% 	hipe_rtl:mk_branch(FreasonReg, eq, 
%% 			   hipe_rtl:mk_imm(?EXTAG_EXIT), 
%% 			   ExitLblName, NoExitLblName, 0.55),
%% 	NoExitLbl,
%% 	hipe_rtl:mk_move(TmpVar, hipe_rtl:mk_imm(hipe_tagscheme:mk_nil())),
%% 	hipe_rtl_primops:gen_gc_mk_tuple(FValueVar, [FValueVar,TmpVar]),
%% 	hipe_rtl:mk_goto(ExitLblName),
	ExitLbl,
	hipe_rtl:mk_load_atom(TmpVar, 'EXIT'),
	hipe_rtl_primops:gen_gc_mk_tuple(ReasonDst, [TmpVar,FValueVar]),
	hipe_rtl:mk_goto(EndLblName),
	NoExitLbl,
%% NEW_EXCEPTIONS
%% 	ThrowLbl,
	hipe_rtl:mk_move(ReasonDst, FValueVar),
	hipe_rtl:mk_goto(EndLblName),
	EndLbl
       ], 
       Map0, ConstTab};

    %% HANDLE TRY
    'try' ->
      {[ReasonDst,TypeDst], Map0} = 
	hipe_rtl_varmap:ivs2rvs([hipe_icode:restore_catch_reason_dst(I),
				 hipe_icode:restore_catch_type_dst(I)], 
				VarMap),
      {[
	%% NOTE: restore_catch must be the first instruction in the
	%% try handler
	hipe_rtl:mk_restore_catch([FValueVar]),
	hipe_rtl_arch:pcb_load(FreasonReg, ?P_FREASON),
%% NEW_EXCEPTIONS
%% 	hipe_rtl:mk_alu(FreasonReg, FreasonReg, 'and',
%% 			hipe_rtl:mk_imm(?EXC_CLASSBITS)),
 	hipe_rtl:mk_branch(FreasonReg, eq, hipe_rtl:mk_imm(?EXTAG_THROWN), 
			   ThrowLblName, NoThrowLblName, 0.45),
	NoThrowLbl,
	hipe_rtl:mk_branch(FreasonReg, eq, 
 			   hipe_rtl:mk_imm(?EXTAG_EXIT), 
			   ExitLblName, NoExitLblName, 0.55),
	ExitLbl,
	hipe_rtl:mk_load_atom(TypeDst, 'EXIT'),
%% NEW_EXCEPTIONS
%% 	NoExitLbl,
%% 	hipe_rtl:mk_load_atom(TypeDst, 'error'),
	hipe_rtl:mk_move(ReasonDst, FValueVar),
	hipe_rtl:mk_goto(EndLblName),
	NoExitLbl,
	hipe_rtl:mk_load_atom(TypeDst, 'ERROR'),
%% NEW_EXCEPTIONS
%% 	ExitLbl,
%% 	hipe_rtl:mk_load_atom(TypeDst, 'exit'),
	hipe_rtl:mk_move(ReasonDst, FValueVar),
	hipe_rtl:mk_goto(EndLblName),
	ThrowLbl,
	hipe_rtl:mk_load_atom(TypeDst, 'throw'),
	hipe_rtl:mk_move(ReasonDst, FValueVar),
	hipe_rtl:mk_goto(EndLblName),
	EndLbl
       ], 
       Map0, ConstTab}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ____________________________________________________________________
%%      
%% Exceptions and error codes. 
%% ____________________________________________________________________
%%
%% Generate code to throw an exception from a BIF call which failed.
%%
gen_exit(Reason) ->
  DebugCode =
    ?IF_DEBUG_LEVEL(2,begin {M,F,A} = get(hipe_mfa),
		    [hipe_rtl:mk_load_atom(Reason,
				     list_to_atom(atom_to_list(M)++":"++
						  atom_to_list(F)++"/"++
						  integer_to_list(A)))] 
		      end ,[]),
  DebugCode ++ [hipe_rtl:mk_enter({erlang,exit,1}, [Reason], remote)].

exit_var(EI) ->
  element(1,EI).
exit_lbl(EI) ->
  element(2,EI).
exit_code(EI) ->
  element(3,EI).
%% exit_mfa(EI) ->
%%   element(4,EI).
exit_trace_label(EI) ->
  element(5,EI).
exit_badarg_label(EI) ->
  element(6,EI).
exit_badarith_label(EI) ->
  element(7,EI).

gen_exception(MFA, ConstTab) ->
  ReasonVar = hipe_rtl:mk_new_var(),
  TraceVar = hipe_rtl:mk_new_var(),
  ExitL = hipe_rtl:mk_new_label(),
  ETermVar = hipe_rtl:mk_new_var(),
  BadArgL = hipe_rtl:mk_new_label(),
  BadArithL = hipe_rtl:mk_new_label(),
  %% Tmp = hipe_rtl:mk_new_reg(),

  {NewTab, TraceLabel} = hipe_consttab:insert_term(ConstTab, [MFA]),

  {{ReasonVar, 
    hipe_rtl:label_name(ExitL),
    [ExitL,
     hipe_rtl:mk_load_address(TraceVar, TraceLabel, constant),
     %% hipe_tagscheme:tag_cons(TraceVar, Tmp),
     hipe_rtl:mk_gctest(3),
     hipe_rtl_primops:gen_gc_mk_tuple(ETermVar, [ReasonVar,TraceVar]),
     hipe_rtl:mk_enter({erlang,fault,1}, [ETermVar], remote),
     %% We speculatively generate common exit atoms...
     BadArgL,
     hipe_rtl:mk_load_atom(ReasonVar, badarg),
     hipe_rtl:mk_goto(hipe_rtl:label_name(ExitL)),
     BadArithL,
     hipe_rtl:mk_load_atom(ReasonVar, badarith),
     hipe_rtl:mk_goto(hipe_rtl:label_name(ExitL))],
    MFA, TraceLabel,
    hipe_rtl:label_name(BadArgL),
    hipe_rtl:label_name(BadArithL)
   }, NewTab}.

gen_trace(Dst, ExitInfo) ->
  %% Tmp = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_load_address(Dst, exit_trace_label(ExitInfo), constant)].
  %% hipe_tagscheme:tag_cons(Dst, Tmp)].

gen_throw(Reason) ->
  hipe_rtl:mk_enter({erlang,throw,1}, [Reason], remote).

gen_fault(Reason, ExitInfo) ->
  [hipe_rtl:mk_move(exit_var(ExitInfo), Reason),
   hipe_rtl:mk_goto(exit_lbl(ExitInfo))].

gen_fault2(Reason, Trace, _ExitInfo) ->
  ETermVar = hipe_rtl:mk_new_var(),
  [hipe_rtl_primops:gen_gc_mk_tuple(ETermVar, [Reason,Trace]),
   hipe_rtl:mk_enter({erlang,fault,1}, [ETermVar], remote)].

gen_raise(Reason, Type, _ExitInfo) ->
  [hipe_rtl:mk_enter({hipe_bifs,raise,2}, [Reason,Type], remote)].

gen_exit_atom(Atom, ExitInfo) ->
  case Atom of
    badarg ->
      [hipe_rtl:mk_goto(exit_badarg_label(ExitInfo))];
    badarith ->
      [hipe_rtl:mk_goto(exit_badarith_label(ExitInfo))];
    _ ->
      [hipe_rtl:mk_load_atom(exit_var(ExitInfo), Atom),
       hipe_rtl:mk_goto(exit_lbl(ExitInfo))]
  end.

%% gen_bif_exit(FailContinuation, Result, ExitInfo) ->
%%   case FailContinuation of
%%     [] ->
%%       [hipe_rtl_arch:pcb_load(exit_var(ExitInfo), ?P_FVALUE),
%%        hipe_rtl:mk_goto(exit_lbl(ExitInfo))];
%%     _ -> %% TODO: Here
%%       Reason = hipe_rtl:mk_new_var(),
%%       MkTermCode = gen_exit_term(Result, Reason, ExitInfo),
%%       [hipe_rtl_arch:pcb_load(Reason, ?P_FVALUE),
%%        MkTermCode,
%%        hipe_rtl:mk_fail_to(Result,FailContinuation)]
%%   end.

%% TODO: Do only one GC test...
gen_exit_term(Dst, Reason, ExitInfo) ->
  Tmp = hipe_rtl:mk_new_var(),
  TraceCode = gen_trace(Tmp, ExitInfo),
  [TraceCode,
   hipe_rtl_primops:gen_gc_mk_tuple(Dst, [Reason,Tmp])].

%% TODO: Fix This.
gen_fail_code(FailContinuation, Result, ExitReason, ExitInfo) ->
  case FailContinuation of
    [] ->
      gen_exit_atom(ExitReason, ExitInfo);
    _ -> %% TODO: Here
      Reason = hipe_rtl:mk_new_var(),
      MkTermCode = gen_exit_term(Result, Reason, ExitInfo),
      
      [hipe_rtl:mk_load_atom(Reason, ExitReason),
       MkTermCode,
       hipe_rtl:mk_fail_to(Result, FailContinuation)]
  end.

%% TODO: move this to a library function
gen_exit_badfun(Fun, _ExitInfo, Result) ->
  BadfunVar = hipe_rtl:mk_new_var(),
  [hipe_rtl:mk_load_atom(BadfunVar, badfun),
   hipe_rtl_primops:gen_gc_mk_tuple(Result, [BadfunVar,Fun])].

gen_funcall_fail(Fail, Fun, BadFunLab, BadArityLab, ExitInfo) ->
  TraceVar = hipe_rtl:mk_new_var(),
  TraceCode = gen_trace(TraceVar, ExitInfo),
  ExitVar = exit_var(ExitInfo),
  case Fail of
    [] ->
      [BadFunLab, 
       gen_exit_badfun(Fun, ExitInfo, ExitVar),
%%       TraceCode,
       hipe_rtl:mk_goto(exit_lbl(ExitInfo)),
       BadArityLab, 
       gen_exit_atom(badarity, ExitInfo)];
    Lbl ->
      Reason = hipe_rtl:mk_new_var(),
      Result = hipe_rtl:mk_new_var(),
      %% io:format("Fun = ~p\n", [Fun]),
      [BadFunLab, 
       gen_exit_badfun(Fun, ExitInfo, Result),
       TraceCode,
       hipe_rtl_primops:gen_gc_mk_tuple(ExitVar, [Result,TraceVar]),
       hipe_rtl:mk_fail_to(ExitVar, Lbl),
       BadArityLab,
       hipe_rtl:mk_load_atom(Reason, badarity),
       gen_exit_term(Result, Reason, ExitInfo),
       hipe_rtl:mk_fail_to(Result, Lbl)]
  end.
