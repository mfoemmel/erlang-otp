%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% Time-stamp: <02/03/09 10:33:34 happi>
%% ====================================================================
%%  Filename : 	hipe_rtl_exceptions.erl
%%  Module   :	hipe_rtl_exceptions
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-04-10 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: happi $
%%              $Date: 2002/09/28 12:34:11 $
%%              $Revision: 1.16 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_rtl_exceptions).

-export([gen_exception/2,
	 gen_exit_atom/2,
	 gen_exit_term/3,
	 gen_fail_code/4,
	 gen_bif_exit/3, 
	 gen_funcall_fail/5,
	 exit_code/1,
	 exit_mfa/1,
	 gen_fail/4,
	 gen_restore_catch/4]).

-include("../main/hipe.hrl").
-include("hipe_icode2rtl.hrl").
-include("hipe_literals.hrl").

%% ____________________________________________________________________
%% Handle the Icode instruction
%% FAIL
%%
gen_fail(I, VarMap, ConstTab, ExitInfo) ->
  case hipe_rtl_varmap:ivs2rvs(hipe_icode:fail_reason(I), VarMap) of
    {[Reason], Map0} ->
      case hipe_icode:fail_type(I) of
	exit -> 
	  {gen_exit(Reason), Map0, ConstTab};
	throw ->
	  {gen_throw(Reason), Map0, ConstTab};
	fault ->
	  {gen_fault(Reason,ExitInfo), Map0, ConstTab}
      end;
    {[Arg1,Arg2], Map0} ->
      case hipe_icode:fail_type(I) of
	fault2 ->
	  Reason = Arg1, Trace = Arg2,
	  {gen_fault2(Reason,Trace,ExitInfo), Map0, ConstTab}
      end
  end.

%% ____________________________________________________________________
%% RESTORE_CATCH
%%
gen_restore_catch(I, VarMap, ConstTab, Options) ->
  %% This is the exception handler, we have to get the exception value
  %% into Dst.
  {[Dst], Map0} = hipe_rtl_varmap:ivs2rvs([hipe_icode:restore_catch_dst(I)], VarMap),
  FValueVar = hipe_rtl:mk_new_var(),
  TmpVar = hipe_rtl:mk_new_var(),
  FreasonReg = hipe_rtl:mk_new_reg(),
  NoExitLbl = hipe_rtl:mk_new_label(),
  ExitLbl = hipe_rtl:mk_new_label(), 
  NoExitLblName = hipe_rtl:label_name(NoExitLbl),
  ExitLblName = hipe_rtl:label_name(ExitLbl),
  EndLbl = hipe_rtl:mk_new_label(), 
  EndLblName  = hipe_rtl:label_name(EndLbl),
  {[
    %% NOTE: restore_catch must be the first instruction in the catch handler
    hipe_rtl:mk_restore_catch([FValueVar]),
    hipe_rtl_arch:pcb_load(FreasonReg, ?P_FREASON),
    hipe_rtl:mk_branch(FreasonReg, eq, hipe_rtl:mk_imm(?FREASON_THROWN), 
		       NoExitLblName, ExitLblName, 0.45),
    ExitLbl,
    hipe_rtl:mk_load_atom(TmpVar, 'EXIT'),
    hipe_rtl_primops:gen_mk_tuple(Dst, [TmpVar, FValueVar], ?DoAddGC(Options)),
    hipe_rtl:mk_goto(EndLblName),
    NoExitLbl,
    hipe_rtl:mk_move(Dst, FValueVar),
    hipe_rtl:mk_goto(EndLblName),
    EndLbl
   ], 
   Map0, ConstTab}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ____________________________________________________________________
%%      
%% Exceptions and error codes. 
%% ____________________________________________________________________
%% 
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
  DebugCode ++ [hipe_rtl:mk_enter({erlang,exit,1}, [Reason], c)].

exit_var(EI) ->
  element(1,EI).
exit_lbl(EI) ->
  element(2,EI).
exit_code(EI) ->
  element(3,EI).

exit_mfa(EI) ->
  element(4,EI).
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
     hipe_rtl_primops:gen_mk_tuple(ETermVar, [ReasonVar,TraceVar], []),
     hipe_rtl:mk_enter({erlang,fault,1}, [ETermVar], c),
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
  hipe_rtl:mk_enter({erlang,throw,1}, [Reason], c).

gen_fault(Reason, ExitInfo) ->
  [hipe_rtl:mk_move(exit_var(ExitInfo), Reason),
   hipe_rtl:mk_goto(exit_lbl(ExitInfo))].


gen_fault2(Reason, Trace, _ExitInfo) ->
  ETermVar = hipe_rtl:mk_new_var(),
  [hipe_rtl_primops:gen_mk_tuple(ETermVar, [Reason,Trace], ?DoAddGC([])),
   hipe_rtl:mk_enter({erlang,fault,1}, [ETermVar], c)].


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


gen_bif_exit(FailContinuation, Result, ExitInfo) ->
  case FailContinuation of
    [] ->
      [hipe_rtl_arch:pcb_load(exit_var(ExitInfo), ?P_FVALUE),
      hipe_rtl:mk_goto(exit_lbl(ExitInfo))];
    _ -> %% TODO: Here
      Reason = hipe_rtl:mk_new_var(),
      MkTermCode = gen_exit_term(Result, Reason, ExitInfo),
      
      [hipe_rtl_arch:pcb_load(Reason, ?P_FVALUE),
       MkTermCode,
       hipe_rtl:mk_fail_to(Result,FailContinuation)]
  end.


%% TODO: Do only one GC test...
gen_exit_term(Dst, Reason, ExitInfo) ->
  Tmp = hipe_rtl:mk_new_var(),
  TraceCode = gen_trace(Tmp, ExitInfo),
  [TraceCode,
   hipe_rtl_primops:gen_mk_tuple(Dst, [Reason,Tmp], ?DoAddGC([]))].


%% TODO: Fix This.
gen_fail_code(FailContinuation, Result, ExitReason, ExitInfo) ->
  case FailContinuation of
    [] -> gen_exit_atom(ExitReason, ExitInfo);
    _ -> %% TODO: Here
      Reason = hipe_rtl:mk_new_var(),
      MkTermCode = gen_exit_term(Result, Reason, ExitInfo),
      
      [hipe_rtl:mk_load_atom(Reason, ExitReason),
       MkTermCode,
       hipe_rtl:mk_fail_to(Result,FailContinuation)]
  end.

%% Todo: move this to a library function
gen_exit_badfun(Fun, _ExitInfo, Result) ->
  BadfunVar = hipe_rtl:mk_new_var(),
  [hipe_rtl:mk_load_atom(BadfunVar, badfun),
   hipe_rtl_primops:gen_mk_tuple(Result, [BadfunVar,Fun], ?DoAddGC([]))].


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
      _FCode = 
	[BadFunLab, 
	 gen_exit_badfun(Fun, ExitInfo,Result),
	 TraceCode,
	 hipe_rtl_primops:gen_mk_tuple(ExitVar, [Result, TraceVar], 
				       ?DoAddGC([])),
	 hipe_rtl:mk_fail_to(ExitVar,Lbl),
	 BadArityLab, 
	 hipe_rtl:mk_load_atom(Reason, badarity),
	 gen_exit_term(Result, Reason, ExitInfo),
	 hipe_rtl:mk_fail_to(Result,Lbl)]
  end.
