% -*- erlang-indent-level: 2 -*-
%%=======================================================================
%% File        : hipe_beam_to_icode.erl
%% Author      : Kostis Sagonas
%% Description : Translates symbolic BEAM code to Icode
%%=======================================================================
%% $Id$
%%=======================================================================
%% @doc
%%    This file translates symbolic BEAM code to Icode which is HiPE's
%%    intermediate code representation.  Either the code of an entire
%%    module, or the code of a specified function can be translated.
%@
%%=======================================================================

-module(hipe_beam_to_icode).

-export([disasm/1,module/2,mfa/3]).

%%-----------------------------------------------------------------------

%% Uncomment the following lines to turn on debugging for this module
%% or comment them to it turn off.  Debug-level 6 inserts a print in
%% each compiled function.

%%-ifndef(DEBUG).
%%-define(DEBUG,6).
%%-endif.
-export([mk_var/1]).
-include("../main/hipe.hrl").
-define(no_debug_msg(Str,Xs),ok).
%%-define(no_debug_msg(Str,Xs),msg(Str,Xs)).

-define(mk_debugcode(MFA, Env, Code),
	case element(1,MFA) of
	  io -> %% We do not want to loop infinitely if we are compiling
	    %% the module io.
	    {Code,Env};
	  _ ->
	    MFAVar = mk_var(new),
	    StringVar = mk_var(new),
	    Ignore = mk_var(new),
	    MkMfa = hipe_icode:mk_mov(MFAVar,hipe_icode:mk_const([MFA])),
	    {M,F,A} = MFA,
	    MkString = hipe_icode:mk_mov(StringVar,
					 hipe_icode:mk_const(
					   atom_to_list(M) ++ ":" ++ atom_to_list(F) ++"/"++ integer_to_list(A) ++ 
					   " Native enter fun ~w\n")),
	    Call =
	      hipe_icode:mk_call([Ignore],io,format,[StringVar,MFAVar],remote),
	    {[MkMfa,MkString,Call | Code], Env}
	end).


%%-----------------------------------------------------------------------
%% @doc
%% Get BEAM code from `.beam' files or direct from binaries.
%%   File is either a file name or a binary containing the code.
%% @end
%%-----------------------------------------------------------------------

disasm(File) ->
  case beam_disasm:file(File) of
    {beam_file,DisasmBeam} ->
      {value,{code,ModuleCode}} = lists:keysearch(code,1,DisasmBeam),
      {value,{comp_info,CompInfo}} = lists:keysearch(comp_info,1,DisasmBeam),
      {value,{options,CompOpts}} = lists:keysearch(options,1,CompInfo),
      HCompOpts = case lists:keysearch(hipe,1,CompOpts) of
		    {value,{hipe,L}} when is_list(L) -> L;
		    {value,{hipe,X}} -> [X];
		    _ -> []
		  end,
      {ModuleCode, HCompOpts};
    Error ->
      io:format("~s\n", [beam_lib:format_error(Error)]),
      ?EXIT(no_beam_code)
  end.

%%-----------------------------------------------------------------------
%% @doc
%% Translates the code of a whole module into Icode.
%%   Returns a tuple whose first argument is a list of {{M,F,A}, ICode} 
%%   pairs, and its second argument is the list of HiPE compiler options.
%% @end
%%-----------------------------------------------------------------------

module(BeamCode, Options) ->
  {ModCode, ClosureInfo} = preprocess_code(BeamCode),
  pp_beam(ModCode, Options),
  [trans_beam_function_chunk(FunCode, ClosureInfo) || FunCode <- ModCode].

trans_beam_function_chunk(FunBeamCode, ClosureInfo) ->
  {M,F,A} = find_mfa(FunBeamCode),
  {_,Icode} = trans_mfa_code(M,F,A, FunBeamCode, ClosureInfo),
  {{M,F,A},Icode}.

%%-----------------------------------------------------------------------
%% @doc
%% Translates the BEAM code of a single function into Icode.
%%   Returns a tuple whose first argument is list of {{M,F,A}, ICode}
%%   pairs, where the first entry is that of the given MFA, and the
%%   following (in undefined order) are those of the funs that are
%%   defined in the function, and recursively, in the funs.  The
%%   second argument of the tuple is the HiPE compiler options
%%   contained in the file.
%% @end
%%-----------------------------------------------------------------------

mfa(BeamCode, MFA, Options) ->
  {ModCode, ClosureInfo} = preprocess_code(BeamCode),
  mfa_loop([MFA], [], sets:new(), ModCode, ClosureInfo, Options).

mfa_loop([{M, F, A} = MFA | MFAs], Ack, Seen, ModCode, ClosureInfo,
	 Options) ->
  case sets:is_element(MFA, Seen) of
    true ->
      mfa_loop(MFAs, Ack, Seen, ModCode, ClosureInfo, Options);
    false ->
      {Icode, FunMFAs} = mfa_get(M, F, A, ModCode, ClosureInfo, Options),
      mfa_loop(FunMFAs ++ MFAs, [{MFA, Icode} | Ack],
	       sets:add_element(MFA, Seen),
	       ModCode, ClosureInfo, Options)
  end;
mfa_loop([], Ack, _, _, _, _) ->
  lists:reverse(Ack).

mfa_get(M, F, A, ModCode, ClosureInfo, Options) ->
  BeamCode = get_fun(ModCode, M,F,A),
  pp_beam([BeamCode],Options),  % cheat by using a list
  {_,Icode} = trans_mfa_code(M,F,A, BeamCode, ClosureInfo),
  FunMFAs = get_fun_mfas(BeamCode),
  {Icode,FunMFAs}.

get_fun_mfas([{patched_make_fun,MFA,_,_,_}|BeamCode]) ->
  [MFA|get_fun_mfas(BeamCode)];
get_fun_mfas([_|BeamCode]) ->
  get_fun_mfas(BeamCode);
get_fun_mfas([]) ->
  [].

%%-----------------------------------------------------------------------
%% The main translation function.
%%-----------------------------------------------------------------------

trans_mfa_code(M,F,A, FunBeamCode, ClosureInfo) ->
  ?no_debug_msg("disassembling: {~p,~p,~p} ...", [M,F,A]),
  hipe_gensym:init(icode),
  Env = env__store_mfa(M,F,A, env__mk_env()),
  %% Extract the function arguments
  FunArgs = extract_fun_args(A),
  %% Record the function arguments
  {FunLbl,Env1} = mk_label(new,Env),
  Env2 = env__store_entrypoint(hipe_icode:label_name(FunLbl), Env1),
  Code1 = lists:flatten(trans_fun(FunBeamCode,Env2)),
  Code2 = fix_catches(Code1),
  %% Debugcode
  ?IF_DEBUG_LEVEL(5,
		  {Code3,_Env3} = ?mk_debugcode({M,F,A}, Env2,Code2),
		  {Code3,_Env3} = {Code2,Env2}), % Code3 = fix_gotos(Code2),

  %% For stack optimization
  {Code4,IsLeaf} = case hipe_icode:is_leaf0(Code3) of
		     true  -> {[FunLbl,hipe_icode:mk_redtest()|Code3],true};
		     false -> {[FunLbl,hipe_icode:mk_redtest()|Code3],false}
		   end,

  IsClosure = case get_closure_info({M,F,A}, ClosureInfo) of
		not_a_closure -> false;
		_ -> true
	      end,

  Icode = hipe_icode:mk_icode({M,F,A},FunArgs,IsClosure,IsLeaf,
			      remove_dead_code(Code4),
			      hipe_gensym:var_range(icode),
			      hipe_gensym:label_range(icode)),

  Icode1 = %% If this function is the code for a closure ...
    case get_closure_info({M,F,A}, ClosureInfo) of
      not_a_closure -> Icode;
      CI -> %% ... then patch the code to 
	%% get the free_vars from the closure
	patch_closure_entry(Icode,CI)
    end,

  ?no_debug_msg("ok~n", []),
  {FunBeamCode,Icode1}.


%%-----------------------------------------------------------------------
%% The main translation switch.
%%-----------------------------------------------------------------------

%%--- label & func_info combo ---
trans_fun([{label,B},{label,_},{func_info,MFA},{label,L}|Instructions], Env) ->
  trans_fun([{label,B},{func_info,MFA},{label,L}|Instructions], Env);
trans_fun([{label,B},
	   {func_info,[{atom,_M},{atom,_F},_A]},
	   {label,L}|Instructions], Env) ->
  %% Emit code to handle badmatch errors.  The BEAM test instructions
  %% branch to this label if they fail inside a clause body.
  %% Obviously, we must goto past this error point on normal entry.
  Begin = mk_label(B,Env),
  V = mk_var(new),
  EntryPt = mk_label(L,Env),
  Goto = hipe_icode:mk_goto(hipe_icode:label_name(EntryPt)),
  Mov = hipe_icode:mk_mov(V, hipe_icode:mk_const(function_clause)),
  Fail = hipe_icode:mk_fail([V],fault),
  [Goto,Begin,Mov,Fail,EntryPt | trans_fun(Instructions,Env)];
%%--- label ---
trans_fun([{label,L}|Instructions], Env) ->
  [mk_label(L,Env) | trans_fun(Instructions, Env)];
%%--- int_code_end --- SHOULD NEVER OCCUR HERE
%%--- call ---
trans_fun([{call,_N,{M,F,A}}|Instructions], Env) ->
  Args = extract_fun_args(A),
  Dst = [mk_var({r,0})],
  case {M,F,A} of
    {erlang,exit,1} ->
      [hipe_icode:mk_fail(Args,exit) | 
       trans_fun(Instructions,Env)];
    {erlang, throw, 1} ->
      [hipe_icode:mk_fail(Args,throw) | 
       trans_fun(Instructions,Env)];
    {erlang, fault, 1} ->
      [hipe_icode:mk_fail(Args,fault) | 
       trans_fun(Instructions,Env)];
    {erlang, fault, 2} ->
      [hipe_icode:mk_fail(Args,fault2) | 
       trans_fun(Instructions,Env)];
    _ ->
      [hipe_icode:mk_call(Dst,M,F,Args,local) | trans_fun(Instructions,Env)]
  end;
%%--- call_last ---
%% Differs from call_only in that it deallocates the environment
trans_fun([{call_last,_N,{M,F,A},_}|Instructions], Env) ->
  %% IS IT OK TO IGNORE LAST ARG ??
  ?no_debug_msg("  translating call_last: ~p ...~n", [Env]),
  case env__get_mfa(Env) of
    {M,F,A} ->
      %% Does this case really happen, or is it covered by call_only?
      Entry = env__get_entry(Env),
      [hipe_icode:mk_comment(tail_recursive), 
       hipe_icode:mk_goto(Entry) | trans_fun(Instructions,Env)];
    _ ->
      Args = extract_fun_args(A),
      [hipe_icode:mk_enter(M,F,Args,local) | trans_fun(Instructions,Env)]
  end;
%%--- call_only ---
%% Used when the body contains only one call in which case 
%% an environment is not needed/created.
trans_fun([{call_only,_N,{M,F,A}}|Instructions], Env) ->
  ?no_debug_msg("  translating call_only: ~p ...~n", [Env]),
  case env__get_mfa(Env) of
    {M,F,A} ->
      Entry = env__get_entry(Env),
      [hipe_icode:mk_comment(self_tail_recursive),
       hipe_icode:mk_goto(Entry) | trans_fun(Instructions,Env)];
    _ ->
      Args = extract_fun_args(A),
      [hipe_icode:mk_enter(M,F,Args,local) | trans_fun(Instructions,Env)]
  end;
%%--- call_ext ---
trans_fun([{call_ext,_N,{M,F,A}}|Instructions], Env) ->
  Args = extract_fun_args(A),
  Dst = [mk_var({r,0})],
  case {M,F,A} of
    {erlang,exit,1} ->
      [hipe_icode:mk_fail(Args,exit) | trans_fun(Instructions,Env)];
    {erlang,throw,1} ->
      [hipe_icode:mk_fail(Args,throw) | trans_fun(Instructions,Env)];
    {erlang,fault,1} ->
      [hipe_icode:mk_fail(Args,fault) | trans_fun(Instructions,Env)];
    {erlang,fault,2} ->
      [hipe_icode:mk_fail(Args,fault2) | trans_fun(Instructions,Env)];
    _ ->
      [hipe_icode:mk_comment(call_ext),
       hipe_icode:mk_call(Dst,M,F,Args,remote) | trans_fun(Instructions,Env)]
  end;
%%--- call_ext_last ---
trans_fun([{call_ext_last,_N,{M,F,A},_}|Instructions], Env) ->
  %% IS IT OK TO IGNORE LAST ARG ??
  Args = extract_fun_args(A),
  %% Dst = [mk_var({r,0})],
  case {M,F,A} of
    {erlang,exit,1} ->
      [hipe_icode:mk_fail(Args,exit) | trans_fun(Instructions,Env)];
    {erlang,throw,1} ->
      [hipe_icode:mk_fail(Args,throw) | trans_fun(Instructions,Env)];
    {erlang,fault,1} ->
      [hipe_icode:mk_fail(Args,fault) | trans_fun(Instructions,Env)];
    {erlang,fault,2} ->
      [hipe_icode:mk_fail(Args,fault2) | trans_fun(Instructions,Env)];
    _ ->
      [hipe_icode:mk_comment(call_ext_last),
       hipe_icode:mk_enter(M,F,Args,remote) | trans_fun(Instructions,Env)]
  end;
%%--- bif0 ---
trans_fun([{bif0,BifName,Reg}|Instructions], Env) ->
  BifInsts = trans_bif0(BifName,Reg),
  [hipe_icode:mk_comment({bif0,BifName}),BifInsts|trans_fun(Instructions,Env)];
%%--- bif1 ---
trans_fun([{bif1,BifName,{f,Lbl},Args,Reg}|Instructions], Env) ->
  {BifInsts,Env1} = trans_bif(1,BifName,Lbl,Args,Reg,Env),
  [hipe_icode:mk_comment({bif1,BifName})|BifInsts] ++ trans_fun(Instructions,Env1);
%%--- bif2 ---
trans_fun([{bif2,BifName,{f,Lbl},Args,Reg}|Instructions], Env) ->
  {BifInsts,Env1} = trans_bif(2,BifName,Lbl,Args,Reg,Env),
  [hipe_icode:mk_comment({bif2,BifName})|BifInsts] ++ trans_fun(Instructions,Env1);
%%--- allocate --- IGNORED ON PURPOSE
trans_fun([{allocate,_,_}|Instructions], Env) ->
  trans_fun(Instructions,Env);
%%--- allocate_heap --- IGNORED ON PURPOSE
trans_fun([{allocate_heap,_,_,_}|Instructions], Env) ->
  trans_fun(Instructions,Env);
%%--- allocate_zero --- IGNORED ON PURPOSE
trans_fun([{allocate_zero,_,_}|Instructions], Env) ->
  trans_fun(Instructions,Env);
%%--- allocate_heap_zero --- IGNORED ON PURPOSE
trans_fun([{allocate_heap_zero,_,_,_}|Instructions], Env) ->
  trans_fun(Instructions,Env);
%%--- test_heap --- IGNORED ON PURPOSE
trans_fun([{test_heap,_,_}|Instructions], Env) ->
  trans_fun(Instructions,Env);
%%--- init --- IGNORED - CORRECT??
trans_fun([{init,_}|Instructions], Env) ->
  trans_fun(Instructions,Env);
%%--- deallocate --- IGNORED ON PURPOSE
trans_fun([{deallocate,_}|Instructions], Env) ->
  trans_fun(Instructions,Env);
%%--- return ---
trans_fun([return|Instructions], Env) ->
  [hipe_icode:mk_return([mk_var({r,0})]) | trans_fun(Instructions,Env)];
%%--- send ---
trans_fun([send|Instructions], Env) ->
  I = hipe_icode:mk_primop([mk_var({r,0})], {erlang,send,2},
			   [mk_var({x,0}),mk_var({x,1})]),
  [I | trans_fun(Instructions,Env)];
%%--- remove_message ---
trans_fun([remove_message|Instructions], Env) ->
  [hipe_icode:mk_primop([],select_msg,[]) | trans_fun(Instructions,Env)];
%%--- timeout --- 
trans_fun([timeout|Instructions], Env) ->
  [hipe_icode:mk_primop([],clear_timeout,[]) | trans_fun(Instructions,Env)];
%%--- loop_rec ---
trans_fun([{loop_rec,{_,Lbl},Reg}|Instructions], Env) ->
  {Movs,[Temp],Env1} = get_constants_in_temps([Reg],Env),
  {GetMsgLbl,Env2} = mk_label(new,Env1),
  MboxEmpty = hipe_icode:mk_if(mbox_empty,[],
			       map_label(Lbl),
			       hipe_icode:label_name(GetMsgLbl)),
  GetMsg = hipe_icode:mk_primop([Temp],get_msg,[]),
  Movs ++ [MboxEmpty,GetMsgLbl,GetMsg | trans_fun(Instructions,Env2)];
%%--- loop_rec_end ---
trans_fun([{loop_rec_end,{_,Lbl}}|Instructions], Env) ->
  Loop = hipe_icode:mk_goto(map_label(Lbl)),
  [hipe_icode:mk_primop([],next_msg,[]),Loop | trans_fun(Instructions,Env)];
%%--- wait ---
trans_fun([{wait,{_,Lbl}}|Instructions], Env) ->
  Susp = hipe_icode:mk_primop([],suspend_msg,[]),
  Loop = hipe_icode:mk_goto(map_label(Lbl)),
  [Susp, Loop | trans_fun(Instructions,Env)];
%%--- wait_timeout ---
trans_fun([{wait_timeout,{_,Lbl},Reg}|Instructions], Env) ->
  {Movs,[Temp],Env1} = get_constants_in_temps([Reg],Env),
  SetTmout = hipe_icode:mk_primop([],set_timeout,[Temp]),
  {DoneLbl,Env2} = mk_label(new,Env1),
  SuspTmout = hipe_icode:mk_if(suspend_msg_timeout,[],
			       map_label(Lbl),hipe_icode:label_name(DoneLbl)),
  Movs ++ [SetTmout,SuspTmout,DoneLbl
	   | trans_fun(Instructions,Env2)];
%%--------------------------------------------------------------------
%%--- Translation of arithmetics {arith, ...} ---
%%--------------------------------------------------------------------
trans_fun([{arith,{Op,SrcRs,DstR},{f,L}}|Instructions], Env) ->
  {ICode,NewEnv} = trans_arith(Op,SrcRs,DstR,L,Env),
  ICode ++ trans_fun(Instructions,NewEnv);
%%--------------------------------------------------------------------
%%--- is_lt ---
trans_fun([{is_lt,{f,Lbl},Arg1,Arg2}|Instructions], Env) ->
  {ICode,Env1} = trans_test_guard('<',Lbl,Arg1,Arg2,Env),
  ICode ++ trans_fun(Instructions,Env1);
%%--- is_ge ---
trans_fun([{is_ge,{f,Lbl},Arg1,Arg2}|Instructions], Env) ->
  {ICode,Env1} = trans_test_guard('>=',Lbl,Arg1,Arg2,Env),
  ICode ++ trans_fun(Instructions,Env1);
%%--- is_eq ---
trans_fun([{is_eq,{f,Lbl},Arg1,Arg2}|Instructions], Env) ->
  {ICode,Env1} = trans_test_guard('==',Lbl,Arg1,Arg2,Env),
  ICode ++ trans_fun(Instructions,Env1);
%%--- is_ne ---
trans_fun([{is_ne,{f,Lbl},Arg1,Arg2}|Instructions], Env) ->
  {ICode,Env1} = trans_test_guard('/=',Lbl,Arg1,Arg2,Env),
  ICode ++ trans_fun(Instructions,Env1);
%%--- is_eq_exact ---
trans_fun([{is_eq_exact,{f,Lbl},Arg1,Arg2}|Instructions], Env) ->
  {ICode,Env1} = trans_is_eq_exact(Lbl,Arg1,Arg2,Env),
  ICode ++ trans_fun(Instructions,Env1);
%%--- is_ne_exact ---
trans_fun([{is_ne_exact,{f,Lbl},Arg1,Arg2}|Instructions], Env) ->
  {ICode,Env1} = trans_test_guard('=/=',Lbl,Arg1,Arg2,Env),
  ICode ++ trans_fun(Instructions,Env1);
%%--------------------------------------------------------------------
%%--- Translation of type tests {is_TYPE, ...} ---
%%--------------------------------------------------------------------
%%--- is_integer ---
trans_fun([{is_integer,{f,Lbl},Arg}|Instructions], Env) ->
  {Code,Env1} = trans_type_test(integer,Lbl,Arg,Env),
  [Code | trans_fun(Instructions,Env1)];
%%--- is_float ---
trans_fun([{is_float,{f,Lbl},Arg}|Instructions], Env) ->
  {Code,Env1} = trans_type_test(float,Lbl,Arg,Env),
  [Code | trans_fun(Instructions,Env1)];
%%--- is_number ---
trans_fun([{is_number,{f,Lbl},Arg}|Instructions], Env) ->
  {Code,Env1} = trans_type_test(number,Lbl,Arg,Env),
  [Code | trans_fun(Instructions,Env1)];
%%--- is_atom ---
trans_fun([{is_atom,{f,Lbl},Arg}|Instructions], Env) ->
  {Code,Env1} = trans_type_test(atom,Lbl,Arg,Env),
  [Code | trans_fun(Instructions,Env1)];
%%--- is_pid ---
trans_fun([{is_pid,{f,Lbl},Arg}|Instructions], Env) ->
  {Code,Env1} = trans_type_test(pid,Lbl,Arg,Env),
  [Code | trans_fun(Instructions,Env1)];
%%--- is_ref ---
trans_fun([{is_ref,{f,Lbl},Arg}|Instructions], Env) ->
  {Code,Env1} = trans_type_test(reference,Lbl,Arg,Env),
  [Code | trans_fun(Instructions,Env1)];
%%--- is_port ---
trans_fun([{is_port,{f,Lbl},Arg}|Instructions], Env) ->
  {Code,Env1} = trans_type_test(port,Lbl,Arg,Env),
  [Code | trans_fun(Instructions,Env1)];
%%--- is_nil ---
trans_fun([{is_nil,{f,Lbl},Arg}|Instructions], Env) ->
  {Code,Env1} = trans_type_test(nil,Lbl,Arg,Env),
  [Code | trans_fun(Instructions,Env1)];
%%--- is_binary ---
trans_fun([{is_binary,{f,Lbl},Arg}|Instructions], Env) ->
  {Code,Env1} = trans_type_test(binary,Lbl,Arg,Env),
  [Code | trans_fun(Instructions,Env1)];
%%--- is_constant ---
trans_fun([{is_constant,{f,Lbl},Arg}|Instructions], Env) ->
  {Code,Env1} = trans_type_test(constant,Lbl,Arg,Env),
  [Code | trans_fun(Instructions,Env1)];
%%--- is_list ---
trans_fun([{is_list,{f,Lbl},Arg}|Instructions], Env) ->
  {Code,Env1} = trans_type_test(list,Lbl,Arg,Env),
  [Code | trans_fun(Instructions,Env1)];
%%--- is_nonempty_list ---
trans_fun([{is_nonempty_list,{f,Lbl},Arg}|Instructions], Env) ->
  {Code,Env1} = trans_type_test(cons,Lbl,Arg,Env),
  [Code | trans_fun(Instructions,Env1)];
%%--- is_tuple ---
trans_fun([{is_tuple,{f,Lbl},Xreg},
	   {test_arity,{f,Lbl},Xreg,N}|Instructions], Env) ->
  trans_fun([{test_arity,{f,Lbl},Xreg,N}|Instructions],Env);
trans_fun([{is_tuple,{_,Lbl},Arg}|Instructions], Env) ->
  {Code,Env1} = trans_type_test(tuple,Lbl,Arg,Env),
  [Code | trans_fun(Instructions,Env1)];
%%--------------------------------------------------------------------
%%--- test_arity ---
trans_fun([{test_arity,{f,Lbl},Reg,N}|Instructions], Env) ->
  {True,Env1} = mk_label(new,Env),
  I = hipe_icode:mk_type(mk_var(Reg),{tuple,N}, 
			 hipe_icode:label_name(True),map_label(Lbl)),
  [I,True | trans_fun(Instructions,Env1)];
%%--- select_val ---
trans_fun([{select_val,Reg,{f,Lbl},Len,CasePairs}|Instructions], Env) ->
  {SwVar,Cases} = trans_select_stuff(Reg,CasePairs),
  I = hipe_icode:mk_switch_val(SwVar,map_label(Lbl),Len,Cases),
  ?no_debug_msg("switch_val instr is ~p~n",[I]),
  [I | trans_fun(Instructions,Env)];
%%--- select_tuple_arity ---
trans_fun([{select_tuple_arity,Reg,{f,Lbl},Len,CasePairs}|Instructions],Env) ->
  {SwVar,Cases} = trans_select_stuff(Reg,CasePairs),
  I = hipe_icode:mk_switch_tuple_arity(SwVar,map_label(Lbl),Len,Cases),
  ?no_debug_msg("switch_tuple_arity instr is ~p~n",[I]),
  [I | trans_fun(Instructions,Env)];    
%%--- jump ---
trans_fun([{jump,{_,L}}|Instructions], Env) ->
  Label = mk_label(L,Env),
  I = hipe_icode:mk_goto(hipe_icode:label_name(Label)),
  [I | trans_fun(Instructions,Env)];

%%--- move ---
trans_fun([{move,Src,Dst}|Instructions], Env) ->
  Dst1 = mk_var(Dst),
  Src1 = case is_var(Src) of
	   true ->
	     mk_var(Src);
	   false ->
	     trans_const(Src)
	 end,
  [hipe_icode:mk_mov(Dst1,Src1) | trans_fun(Instructions,Env)];


%%--- catch --- ITS PROCESSING IS POSTPONED
trans_fun([{'catch',N,{_,EndLabel}}|Instructions], Env) ->
  [{'catch',N,EndLabel} | trans_fun(Instructions,Env)];
%%--- catch_end --- ITS PROCESSING IS POSTPONED
trans_fun([{catch_end,N}|Instructions], Env) ->
  [{catch_end,N} | trans_fun(Instructions,Env)];

%%--- try --- ITS PROCESSING IS POSTPONED
trans_fun([{'try',N,{_,EndLabel}}|Instructions], Env) ->
  [{'try',N,EndLabel} | trans_fun(Instructions,Env)];

%%--- try_end --- ITS PROCESSING IS POSTPONED
trans_fun([{try_end,N}|Instructions], Env) ->
  [{try_end,N} | trans_fun(Instructions,Env)];

%%--- try_case --- This instruction is not needed in Icode
%%                 Since this BB is indicated by the try instruction
trans_fun([{try_case,_N}|Instructions], Env) ->
  trans_fun(Instructions,Env);

%%--- try_case_end --- XXX: THIS IS A PRELIMINARY IMPLEMENTATION
trans_fun([{try_case_end,Arg}|Instructions], Env) ->
  BadArg = trans_arg(Arg),
  ErrVar = mk_var(new),
  V = mk_var(new),
  Atom = hipe_icode:mk_mov(ErrVar,hipe_icode:mk_const(try_clause)),
  Tuple = hipe_icode:mk_primop([V],mktuple,[ErrVar,BadArg]),
  Fail = hipe_icode:mk_fail([V], fault),
  [Atom,Tuple,Fail | trans_fun(Instructions,Env)];

%%--- raise ---
trans_fun([{raise,Arg1,Arg2}|Instructions], Env) ->
  Var1 = trans_arg(Arg1),
  Var2 = trans_arg(Arg2),
  I = hipe_icode:mk_fail([Var1,Var2],'raise'),
  [I | trans_fun(Instructions,Env)];


%%--- get_list ---
trans_fun([{get_list,List,Head,Tail}|Instructions], Env) ->
  I1 = hipe_icode:mk_primop([mk_var(Head)],unsafe_hd,[mk_var(List)]),
  I2 = hipe_icode:mk_primop([mk_var(Tail)],unsafe_tl,[mk_var(List)]),
  %% Handle the cases where the dest overwrites the src!!
  if 
    Head /= List ->
      [I1, I2 | trans_fun(Instructions,Env)];
    Tail /= List ->
      [I2, I1 | trans_fun(Instructions,Env)];
    true ->
      %% WARNING!!  We should take care of this case!!!!!
      io:format("WARNING!!!: hd and tl regs identical in get_list~n"),
      exit(warning)
  end;
%%--- get_tuple_element ---
trans_fun([{get_tuple_element,Xreg,Index,Dst}|Instructions], Env) ->
  I = hipe_icode:mk_primop([mk_var(Dst)],
			   {unsafe_element,Index+1},
			   [mk_var(Xreg)]),
  [I | trans_fun(Instructions,Env)];
%%--- set_tuple_element ---
trans_fun([{set_tuple_element,Elem,Tuple,Index}|Instructions], Env) ->
  %% io:format("BEAM_2_ICODE ~p ~p ~p\n", [Elem,Tuple,Index]),
  Elem1 = case is_var(Elem) of
	    true -> mk_var(Elem);
	    false -> trans_const(Elem)
	  end,
  I = hipe_icode:mk_primop([],
			   {unsafe_update_element,Index+1},
			   [mk_var(Tuple),Elem1]),
  [I | trans_fun(Instructions,Env)];
%%--- put_string ---
trans_fun([{put_string,_Len,String,Dst}|Instructions], Env) ->
  Mov = hipe_icode:mk_mov(mk_var(Dst),trans_const(String)),
  [Mov | trans_fun(Instructions,Env)];
%%--- put_list ---
trans_fun([{put_list,Car,Cdr,Dest}|Instructions], Env) ->
  {M1,V1,Env2} = mk_move_and_var(Car,Env),
  {M2,V2,Env3} = mk_move_and_var(Cdr,Env2),
  D = mk_var(Dest),
  M1 ++ M2 ++ [hipe_icode:mk_primop([D],cons,[V1,V2])
	       | trans_fun(Instructions,Env3)];
%%--- put_tuple ---
trans_fun([{put_tuple,_Size,Reg}|Instructions], Env) ->
  {Moves,Instructions2,Vars,Env2} = trans_puts(Instructions,Env),
  Dest = [mk_var(Reg)],
  Src = lists:reverse(Vars),
  Primop = hipe_icode:mk_primop(Dest,mktuple,Src),
  Moves ++ [Primop | trans_fun(Instructions2,Env2)];
%%--- put --- SHOULD NOT REALLY EXIST HERE; put INSTRUCTIONS ARE HANDLED ABOVE.
%%--- badmatch ---
trans_fun([{badmatch,Arg}|Instructions], Env) ->
  BadVar = trans_arg(Arg),
  ErrVar = mk_var(new),
  V = mk_var(new),
  Atom = hipe_icode:mk_mov(ErrVar,hipe_icode:mk_const(badmatch)),
  Tuple = hipe_icode:mk_primop([V],mktuple,[ErrVar,BadVar]),
  Fail = hipe_icode:mk_fail([V], fault),
  [Atom,Tuple,Fail | trans_fun(Instructions,Env)];
%%--- if_end ---
trans_fun([if_end|Instructions], Env) ->
  V = mk_var(new),
  Mov = hipe_icode:mk_mov(V,hipe_icode:mk_const(if_clause)),
  Fail = hipe_icode:mk_fail([V], fault),
  [Mov,Fail | trans_fun(Instructions, Env)];
%%--- case_end ---
trans_fun([{case_end,Arg}|Instructions], Env) ->
  BadArg = trans_arg(Arg),
  ErrVar = mk_var(new),
  V = mk_var(new),
  Atom = hipe_icode:mk_mov(ErrVar,hipe_icode:mk_const(case_clause)),
  Tuple = hipe_icode:mk_primop([V],mktuple,[ErrVar,BadArg]),
  Fail = hipe_icode:mk_fail([V], fault),
  [Atom,Tuple,Fail | trans_fun(Instructions,Env)];
%%--- enter_fun ---
trans_fun([{call_fun,N},{deallocate,_},return|Instructions], Env) ->
  Args = extract_fun_args(N+1), %% +1 is for the fun itself
  [hipe_icode:mk_comment(enter_fun),
   hipe_icode:mk_enter_primop(enter_fun,Args) | trans_fun(Instructions,Env)];
%%--- call_fun ---
trans_fun([{call_fun,N}|Instructions], Env) ->
  Args = extract_fun_args(N+1), %% +1 is for the fun itself
  Dst = [mk_var({r,0})],
  [hipe_icode:mk_comment(call_fun),
   hipe_icode:mk_primop(Dst,call_fun,Args) | trans_fun(Instructions,Env)];
%%--- patched_make_fun --- make_fun/make_fun2 after fixups
trans_fun([{patched_make_fun,MFA,Magic,FreeVarNum,Index}|Instructions], Env) ->
  Args = extract_fun_args(FreeVarNum),
  Dst = [mk_var({r,0})],
  Fun = hipe_icode:mk_primop(Dst,{mkfun,MFA,Magic,Index},Args),
  ?no_debug_msg("mkfun translates to: ~p~n",[Fun]),
  [Fun | trans_fun(Instructions,Env)];
%%--- is_function ---
trans_fun([{is_function,{f,Lbl},Arg}|Instructions], Env) ->
  {Code,Env1} = trans_type_test(function,Lbl,Arg,Env),
  [Code | trans_fun(Instructions,Env1)];
%%--- call_ext_only ---
trans_fun([{call_ext_only,_N,{M,F,A}}|Instructions], Env) ->
  Args = extract_fun_args(A),
  [hipe_icode:mk_comment(call_ext_only),
   hipe_icode:mk_enter(M,F,Args,remote) | trans_fun(Instructions,Env)];

%%--------------------------------------------------------------------
%%--- Translation of binary instructions ---
%%--------------------------------------------------------------------
%%
%% This code uses a somewhat unorthodox translation:
%%  Since we do not want non-erlang values as arguments to Icode
%%  instructions some compile time constants are coded into the
%%  name of the function (or rather the primop).
%%
%% TODO: Make sure all cases of argument types are covered.
%%

%%---  bs_get_float --- 
%%{bs_get_float,{f,24},{y,0},1,{field_flags,0},{x,2}
trans_fun([{bs_get_float,{f,Lbl},Size,Unit,{field_flags,Flags0},X}|
	   Instructions], Env) ->  
  Dst = mk_var(X),
  Flags = resolve_native_endianess(Flags0),
  %% Get the type of get_float
  {Name, Args} = 
    case Size of
      {integer, NoBits} when NoBits >= 0 -> 
	{{bs_get_float, NoBits*Unit,Flags}, []};
      {integer, NoBits} when NoBits < 0 ->
	?EXIT({bad_bs_size_constant,Size});
      BitReg -> % Use a number of bits only known at runtime.
	Bits = mk_var(BitReg),
	{{bs_get_float,Unit,Flags},[Bits]}
    end,
  %% Generate code for calling the bs-op.
  trans_op_call({hipe_bs_primop,Name}, Lbl, Args, [Dst], Env, Instructions);
%%---  bs_put_float --- 
trans_fun([{bs_put_float,{f,Lbl},Size,Unit,{field_flags,Flags0},Source}|
	   Instructions], Env) ->
  Flags = resolve_native_endianess(Flags0),
  %% Get source
  {Src, SourceInstrs, ConstInfo} = 
    case is_var(Source) of
      true ->
	{mk_var(Source),[], var};
      false ->
	case Source of
	  {float, X} when float(X) ->
	    C = trans_const(Source),
	    SrcVar = mk_var(new),
	    I = hipe_icode:mk_mov(SrcVar, C),
	    {SrcVar,[I],pass};
	  _ -> 
	    C = trans_const(Source),
	    SrcVar = mk_var(new),
	    I = hipe_icode:mk_mov(SrcVar, C),
	    {SrcVar,[I],fail}
	end
    end,

  %% Get type of put_float
  {Name, Args,Env2} = 
    case Size of
      {integer, NoBits} when NoBits >= 0 -> %% Create a N*Unit bits float
	{{bs_put_float, NoBits*Unit, Flags, ConstInfo}, [Src],Env};
      {integer, NoBits} when NoBits < 0 ->
	?EXIT({bad_bs_size_constant,Size});
      BitReg -> % Use a number of bits only known at runtime.
	Bits = mk_var(BitReg),
	{{bs_put_float, Unit, Flags, ConstInfo}, [Src,Bits],Env}
    end,

  %% Generate code for calling the bs-op. 
  SourceInstrs ++ 
    trans_op_call({hipe_bs_primop,Name}, Lbl, Args, [], Env2, Instructions);  

%%--- bs_put_binary ---
%% Create a sub-binary.
trans_fun([{bs_put_binary,{f,Lbl},Size,Unit,{field_flags,Flags},Source}|
	   Instructions], Env) ->

  %% Get the source of the binary.
  {Src, SrcInstrs} = 
    case is_var(Source) of
      true ->
	{mk_var(Source),[]};
      false ->
	C = trans_const(Source),
	SrcVar = mk_var(new),
	I = hipe_icode:mk_mov(SrcVar, C),
	{SrcVar,[I]}
    end,

  %% Get type of put_binary
  {Name, Args,Env2} = 
    case Size of
      {atom, all} -> %% put all bits
	{{bs_put_binary_all, Flags},[Src],Env};
      {integer, NoBits} when NoBits >= 0 -> %% Create a N*Unit bits subbinary
	{{bs_put_binary, NoBits*Unit, Flags}, [Src],Env};
      {integer, NoBits} when NoBits < 0 ->
	?EXIT({bad_bs_size_constant,Size});
      BitReg -> % Use a number of bits only known at runtime.
	Bits = mk_var(BitReg),
	{{bs_put_binary, Unit, Flags},[Src, Bits],Env}
    end,

  %% Generate code for calling the bs-op.
    SrcInstrs ++ trans_op_call({hipe_bs_primop, Name}, 
			       Lbl, Args, [], Env2, Instructions);

%%--- bs_init ---
trans_fun([{bs_init,Size,{field_flags,Flags}} |Instructions], Env) ->
  %% What to do with Arg1 and Arg2?
  Name =
  case get(hipe_inline_bs) of
    true ->
      {bs_init,Size,Flags};
    _ ->
      bs_init
  end,
  [hipe_icode:mk_primop([],{hipe_bs_primop,Name},[]) |
   trans_fun(Instructions, Env)];
%%--- bs_need_buf ---
trans_fun([{bs_need_buf,_Need}|Instructions], Env) ->
  %% Can safely be ignored, according to Bjorn. Prop to RTL level per
  trans_fun(Instructions, Env);
%%--- bs_put_string ---
trans_fun([{bs_put_string,SizeInBytes,{string,String}}|Instructions], Env) ->
  [hipe_icode:mk_primop([],
			{hipe_bs_primop,{bs_put_string, String, SizeInBytes}},
			[]) |
   trans_fun(Instructions, Env)];
%%--- bs_final ---
trans_fun([{bs_final,{f,_Lbl},Dst}|Instructions], Env) ->
  %% What to do with "_Lbl"?
  [hipe_icode:mk_primop([mk_var(Dst)], {hipe_bs_primop, bs_final},[]) |
   trans_fun(Instructions, Env)];
%%--- bs_start_match ---
trans_fun([{bs_start_match,{f,Lbl},X}|Instructions], Env) ->
  Bin = mk_var(X),
  trans_op_call({hipe_bs_primop, bs_start_match}, Lbl, [Bin],
		    [], Env, Instructions);
%%--- bs_get_integer --- changed
trans_fun([{bs_get_integer,{f,Lbl},Size,Unit,{field_flags,Flags0},X}|
	   Instructions], Env) ->
  Dst = mk_var(X),
  Flags = resolve_native_endianess(Flags0),
  %% Get size-type 
  {Name, Args} = 
    case Size of
      {integer, NoBits} when NoBits >= 0 -> %% Create a N*Unit bits subbinary
	{{bs_get_integer, NoBits*Unit,Flags}, []};
      {integer, NoBits} when NoBits < 0 ->
	?EXIT({bad_bs_size_constant,Size});
      BitReg -> % Use a number of bits only known at runtime.
	Bits = mk_var(BitReg),
	{{bs_get_integer,Unit,Flags},[Bits]}
    end,
  %% Generate code for calling the bs-op.
  trans_op_call({hipe_bs_primop,Name}, Lbl, Args, [Dst], Env, Instructions);

%%--- bs_put_integer ---
trans_fun([{bs_put_integer,{f,Lbl},Size,Unit,{field_flags,Flags0},Source}|
	   Instructions], Env) ->
  Flags = resolve_native_endianess(Flags0),
  %% Get size-type 
  
  %% Get the source of the binary.
  {Src, SrcInstrs, ConstInfo} = 
    case is_var(Source) of
      true ->
	{mk_var(Source),[], var};
      false ->
	case Source of
	  {integer, X} when integer(X) ->
	    C = trans_const(Source),
	    SrcVar = mk_var(new),
	    I = hipe_icode:mk_mov(SrcVar, C),
	    {SrcVar,[I], pass};
	  _ ->
	     C = trans_const(Source),
	    SrcVar = mk_var(new),
	    I = hipe_icode:mk_mov(SrcVar, C),
	    {SrcVar,[I], fail}
	    
	end
    end,
  {Name, Args, Env2} = 
    case is_var(Size) of
      true ->
	SVar = mk_var(Size),
	{{bs_put_integer,Unit,Flags, ConstInfo},[SVar], Env};
      false ->
	case Size of
	  {integer, NoBits} when NoBits >= 0 -> 
	    {{bs_put_integer, NoBits*Unit,Flags, ConstInfo}, [], Env};
	  _ -> 
	    ?EXIT({bad_bs_size_constant,Size})
	      end
    end,
  SrcInstrs ++
    trans_op_call({hipe_bs_primop, Name}, 
		  Lbl, [Src|Args], [], Env2, Instructions);
%%--- bs_save ---
trans_fun([{bs_save,Index}| Instructions], Env) ->
  [hipe_icode:mk_primop([],{hipe_bs_primop,{bs_save,Index}},[]) |
   trans_fun(Instructions, Env)];
%%--- bs_restore ---
trans_fun([{bs_restore,Index}| Instructions], Env) ->
  [hipe_icode:mk_primop([],{hipe_bs_primop,{bs_restore,Index}},[]) |
	 trans_fun(Instructions, Env)];
%%--- bs_test_tail ---
trans_fun([{bs_test_tail,{f,Lbl},Numbits}| Instructions], Env) ->
  trans_op_call({hipe_bs_primop,{bs_test_tail, Numbits}}, 
		Lbl, [], [], Env, Instructions);
%%--- bs_skip_bits ---
trans_fun([{bs_skip_bits,{f,Lbl},Size,NumBits,{field_flags,Flags}}|
	   Instructions], Env) -> 
  %% the current match buffer
  {Name, Args} = 
    case Size of
      {atom, all} -> %% Skip all bits
	{{bs_skip_bits_all, Flags},[]};
      {integer, BitSize} when BitSize >= 0-> %% Skip N bits
	{{bs_skip_bits, BitSize*NumBits}, []};
      {integer, BitSize} when BitSize < 0 ->
	?EXIT({bad_bs_size_constant,Size});
      X -> % Skip a number of bits only known at runtime.
	Src = mk_var(X),
	{{bs_skip_bits,NumBits},[Src]}
    end,
  trans_op_call({hipe_bs_primop,Name}, Lbl, Args, [], Env, Instructions);
%%--- bs_get_binary ---
trans_fun([{bs_get_binary,{f,Lbl},Size,Unit,{field_flags,Flags},X}| 
	   Instructions], Env) ->
  {Name, Args} = 
    case Size of
      {atom, all} -> %% put all bits
	{{bs_get_binary_all, Flags},[]};
      {integer, NoBits} when NoBits >= 0 -> %% Create a N*Unit bits subbinary
	{{bs_get_binary, NoBits*Unit, Flags}, []};
      {integer, NoBits} when NoBits < 0 ->
	?EXIT({bad_bs_size_constant,Size});
      BitReg -> % Use a number of bits only known at runtime.
	Bits = mk_var(BitReg),
	{{bs_get_binary,Unit, Flags},[Bits]}
    end,
  Dsts = [mk_var(X)],
  trans_op_call({hipe_bs_primop,Name}, Lbl, Args, Dsts, Env, Instructions);
%%--------------------------------------------------------------------
%%--- Translation of floating point instructions ---
%%

%%--- fclearerror ---
trans_fun([fclearerror|Instructions], Env) ->
  case get(hipe_inline_fp) of
    true ->  
      [hipe_icode:mk_primop([], fclearerror, []) | 
       trans_fun(Instructions,Env)];
    _ -> 
      trans_fun(Instructions,Env)
  end;

%%--- fcheckerror ---
trans_fun([{fcheckerror,{_,Fail}}|Instructions], Env) ->
  case get(hipe_inline_fp) of
    true ->
      {ContLbl, Env1} = mk_label(new, Env),
      case Fail of
	0 -> 
	  [hipe_icode:mk_primop([], fcheckerror, [],
				hipe_icode:label_name(ContLbl), []),
	   ContLbl | trans_fun(Instructions,Env1)];
	_ -> %% Can this happen?
	  {Guard,Env2} =
	    make_guard([],fcheckerror,[],
		       hipe_icode:label_name(ContLbl),map_label(Fail),Env1),
	  [Guard, ContLbl | trans_fun(Instructions,Env2)]
      end;
    _ ->
      trans_fun(Instructions, Env)
  end;
%%--- fmove ---
trans_fun([{fmove,Src,Dst}|Instructions], Env) ->
  case get(hipe_inline_fp) of
    true ->
      Dst1 = mk_var(Dst),
      Src1 = case is_var(Src) of
	       true -> mk_var(Src);
	       false -> trans_const(Src)
	     end,
      case{hipe_icode:is_fvar(Dst1),
	   hipe_icode:is_fvar(Src1)} of
	{true, true} -> %% fvar := fvar 
	  [hipe_icode:mk_fmov(Dst1,Src1) | trans_fun(Instructions,Env)];
	{false, true} -> %% var := fvar
	  [hipe_icode:mk_primop([Dst1], unsafe_tag_float, [Src1]) |
	   trans_fun(Instructions,Env)];
	{true, false} -> %% fvar := var or fvar := constant
	  [hipe_icode:mk_primop([Dst1], unsafe_untag_float, [Src1]) |
	   trans_fun(Instructions,Env)]      
      end;
    _ ->
      trans_fun([{move,Src,Dst}|Instructions], Env)
  end;
%%--- fconv ---
trans_fun([{fconv,Eterm,FReg}|Instructions], Env) ->
  case get(hipe_inline_fp) of
    true ->
      Src = case is_var(Eterm) of
	      true -> mk_var(Eterm);
	      false -> trans_const(Eterm)
	    end,
      {ContLbl, Env1} = mk_label(new, Env),
      Dst = mk_var(FReg),
      [hipe_icode:mk_primop([Dst], conv_to_float, [Src], 
			    hipe_icode:label_name(ContLbl), []),
       ContLbl| trans_fun(Instructions, Env1)];
    _ ->
      trans_fun([{fmove,Eterm,FReg}|Instructions], Env)
  end;
%%--- fadd ---
trans_fun([{fadd,Lab,SrcRs,DstR}|Instructions], Env) ->
  case get(hipe_inline_fp) of
    true ->
      trans_fun([{arith,{fp_add,SrcRs,DstR},Lab}|Instructions], Env);
    _ ->
      trans_fun([{arith,{'+',SrcRs,DstR},Lab}|Instructions], Env)
  end;
%%--- fsub ---
trans_fun([{fsub,Lab,SrcRs,DstR}|Instructions], Env) ->
  case get(hipe_inline_fp) of
    true ->
      trans_fun([{arith,{fp_sub,SrcRs,DstR},Lab}|Instructions], Env);
    _ ->
      trans_fun([{arith,{'-',SrcRs,DstR},Lab}|Instructions], Env)
  end;
%%--- fmult ---
trans_fun([{fmul,Lab,SrcRs,DstR}|Instructions], Env) ->
  case get(hipe_inline_fp) of
    true ->
      trans_fun([{arith,{fp_mul,SrcRs,DstR},Lab}|Instructions], Env);
    _ ->
      trans_fun([{arith,{'*',SrcRs,DstR},Lab}|Instructions], Env)
  end;
%%--- fdiv ---
trans_fun([{fdiv,Lab,SrcRs,DstR}|Instructions], Env) ->
  case get(hipe_inline_fp) of
    true ->
      trans_fun([{arith,{fp_div,SrcRs,DstR},Lab}|Instructions], Env);
    _ ->
      trans_fun([{arith,{'/',SrcRs,DstR},Lab}|Instructions], Env)
  end;
%%--- fnegate ---
trans_fun([{fnegate,Lab,[SrcR],DestR}|Instructions], Env) ->
  case get(hipe_inline_fp) of
    true ->
      Src = mk_var(SrcR),
      Dst = mk_var(DestR),
      [hipe_icode:mk_primop([Dst], fnegate, [Src])| 
       trans_fun(Instructions,Env)];
    _ ->
      trans_fun([{arith,{'-',[{float,0.0},SrcR],DestR},Lab}|Instructions], Env)
  end;

%%--------------------------------------------------------------------
%%--- ERROR HANDLING ---
trans_fun([X|_], _) ->
  ?EXIT({'trans_fun/2',X});
trans_fun([], _) ->
  [].

%%-----------------------------------------------------------------------
%% trans_bif0(BifName, DestReg)
%% trans_bif(Arity, BifName, FailLab, Args, DestReg, Environment)
%%-----------------------------------------------------------------------

trans_bif0(BifName, DestReg) ->
  ?no_debug_msg("  found BIF0: ~p() ...~n", [BifName]),
  BifRes = mk_var(DestReg),
  Module = hipe_bif:bif_module(0,BifName),
  I = hipe_icode:mk_primop([BifRes],{Module,BifName,0},[]),
  [I].

trans_bif(Arity, BifName, Lbl, Args, DestReg, Env) ->
  ?no_debug_msg("  found BIF: ~p(~p) ...~n", [BifName,Args]),
  BifRes = mk_var(DestReg),
  {Movs, SrcVars, Env1} = get_constants_in_temps(Args,Env),
  Module = hipe_bif:bif_module(Arity,BifName),
  case Lbl of
    0 -> % Bif is not in a guard
      I = hipe_icode:mk_primop([BifRes],{Module,BifName,Arity},SrcVars),
      {Movs ++ [I], Env1};
    _ -> % Bif occurs in a guard - fail silently to Lbl
      {GuardI,Env2} =
	make_fallthrough_guard([BifRes],{Module,BifName,Arity},SrcVars,
			       map_label(Lbl),Env1),
      {[Movs,GuardI], Env2}
  end.

trans_op_call(Name, Lbl, Args, Dests, Env, Instructions) ->
  {Code,Env1} =
    case Lbl of
      0 -> % Op is not in a guard
	I = hipe_icode:mk_primop(Dests, Name, Args),
	{[I], Env};
      _ -> % op occurs in a guard - fail silently to Lbl
	  make_fallthrough_guard(Dests,Name,Args,map_label(Lbl),Env)
    end,
  [Code|trans_fun(Instructions, Env1)].

%%-----------------------------------------------------------------------
%% trans_arith(Op, SrcVars, Des, Lab, Env) -> { Icode, NewEnv }
%%     Handles m_plus, m_minus, m_times, m_div, int_div, int_rem,
%%             int_band, int_bor, int_bxor, int_bsl, int_bsr, int_bnot.
%%     A failure label of type {f,0} means in a body.
%%     A failure label of type {f,L} where L>0 means in a guard.
%%        Within a guard a failure should branch to the next guard and
%%        not trigger an exception!!
%%     Handles body arithmetic with Icode primops!
%%     Handles guard arithmetic with Icode guardops!
%%-----------------------------------------------------------------------

trans_arith(Op, SrcRs, DstR, Lbl, Env) ->
  {Movs,SrcVars,Env1} = get_constants_in_temps(SrcRs,Env),
  DstVar = mk_var(DstR),

  %%io:format("hipe_beam_to_icode:trans_arith()\n ~w := ~w ~w\n",
  %%		[DstVar,SrcVars, Op]),
  case Lbl of
    0 ->  % Body arithmetic
      Primop = hipe_icode:mk_primop([DstVar],Op,SrcVars),
      {Movs++[Primop], Env1};
    _ ->  % Guard arithmetic
      {Guard,Env2} = 
	make_fallthrough_guard([DstVar],Op,SrcVars,
			       map_label(Lbl),Env1),
      {[Movs,Guard], Env2}
  end.

%%-----------------------------------------------------------------------
%%-----------------------------------------------------------------------

trans_test_guard(TestOp,F,Arg1,Arg2,Env) ->
  {Movs,Vars,Env1} = get_constants_in_temps([Arg1,Arg2],Env),
  {True,Env2} = mk_label(new,Env1),
  I = hipe_icode:mk_if(TestOp,Vars,hipe_icode:label_name(True),map_label(F)),
  {[Movs,I,True], Env2}.

%%-----------------------------------------------------------------------
%%-----------------------------------------------------------------------

make_fallthrough_guard(DstVar,GuardOp,Args,FailLName,Env) ->
  {ContL,Env1} = mk_label(new,Env),
  ContLName = hipe_icode:label_name(ContL),
  {Guard,Env2} =
    make_guard(DstVar,GuardOp,Args,ContLName,FailLName,Env1),
  {[Guard,ContL],Env2}.

%% Make sure DstVar gets initialised after a fail:
make_guard([DstVar],GuardOp,Args,ContLName,FailLName,Env) ->
  {RestorCatchL,Env1} = mk_label(new,Env),
  RestorCatchLName =  hipe_icode:label_name(RestorCatchL),
  GuardOpIns = hipe_icode:mk_guardop([DstVar],GuardOp,Args,
				  ContLName, 
				  RestorCatchLName),
  Restore = [RestorCatchL,
	    hipe_icode:mk_restore_catch(DstVar,RestorCatchLName),
	    hipe_icode:mk_goto(FailLName)],
  {[GuardOpIns|Restore], Env1};
%% A guard that does not return anything:
make_guard([],GuardOp,Args,ContLName,FailLName,Env) ->
  {[hipe_icode:mk_guardop([],GuardOp,Args,ContLName,FailLName)],
   Env};
%% Some bs-ops have multiple retvals...
%% XXX: For now we assume they only return regs...
%%      regs should not be re-introduced after a fail
make_guard(Dests,GuardOp,Args,ContLName,FailLName,Env) ->
  {[hipe_icode:mk_guardop(Dests,GuardOp,Args,ContLName,FailLName)],
   Env}.
  %% XXX: code for initialising all return values of a guardop
  %%      not in use since only strange binary ops have multiple 
  %%      retvals.
  % {RestorCatchL,Env1} = mk_label(new,Env),
  % RestorCatchLName =  hipe_icode:label_name(RestorCatchL),
  % GuardOpIns = hipe_icode:mk_guardop(Dests,GuardOp,Args,
  % 				  ContLName, 
  % 				  RestorCatchLName),
  % Restore = [RestorCatchL,
  % 	    hipe_icode:mk_restore_catch(hd(Dests),RestorCatchLName),
  % 	    nillify_rest(tl(Dests)),
  % 	    hipe_icode:mk_goto(FailLName)],
  %  {[GuardOpIns|Restore], Env1}.
  %
  % nillify_rest([Var|Vars]) ->
  %   [hipe_icode:mk_mov(Var,hipe_icode:mk_const([]))|nillify_rest(Vars)];
  % nillify_rest([]) -> [].

%%-----------------------------------------------------------------------
%% trans_type_test(Test, Lbl, Arg, Env) -> { Icode, NewEnv }
%%     Handles all type tests like is_integer etc. 
%%-----------------------------------------------------------------------

trans_type_test(Test, Lbl, Arg, Env) ->
  {True,Env1} = mk_label(new,Env),
  {Move,Var,Env2} =  mk_move_and_var(Arg,Env1),
  I = hipe_icode:mk_type(Var,Test,
			 hipe_icode:label_name(True),map_label(Lbl)),
  {[Move,I,True],Env2}.

%%-----------------------------------------------------------------------
%% trans_puts(Code, Environment) -> 
%%            { Movs, Code, Vars, NewEnv }
%%-----------------------------------------------------------------------

trans_puts(Code, Env) ->
  trans_puts(Code, [], [], Env).

trans_puts([{put,X}|Code], Vars, Moves, Env) ->
  case type(X) of
    var ->
      Var = mk_var(X),
      trans_puts(Code,[Var|Vars],Moves,Env);
    {const,C} ->
      Var = mk_var(new),
      Move = hipe_icode:mk_mov(Var,hipe_icode:mk_const(C)),
      trans_puts(Code,[Var|Vars],[Move|Moves],Env)
  end;
trans_puts(Code, Vars, Moves, Env) ->    %% No more put operations
  {Moves, Code, Vars, Env}.

%%-----------------------------------------------------------------------
%% The code for this instruction is a bit large because we are treating
%% different cases differently.  We want to use the icode `type' 
%% instruction when it is applicable to take care of match expressions.
%%-----------------------------------------------------------------------

trans_is_eq_exact(Lbl,Arg1,Arg2,Env) ->
  case {is_var(Arg1),is_var(Arg2)} of
    {true,true} ->
      {True,Env1} = mk_label(new,Env),
      I = hipe_icode:mk_if('=:=',
			   [mk_var(Arg1),mk_var(Arg2)],
			   hipe_icode:label_name(True),map_label(Lbl)),
      {[I,True], Env1};
    {true,false} -> %% Right argument is a constant -- use type()!
      trans_is_eq_exact_var_const(Lbl, Arg1, Arg2, Env);
    {false,true} -> %% mirror of the case above; swap args
      trans_is_eq_exact_var_const(Lbl, Arg2, Arg1, Env);
    {false,false} -> %% Both arguments are constants !!!
      case Arg1 =:= Arg2 of
	true ->   {[], Env};
	false ->   
	  {Never,Env1} = mk_label(new,Env),
	  I = hipe_icode:mk_goto(map_label(Lbl)),
	  {[I,Never], Env1}
      end
  end.

trans_is_eq_exact_var_const(Lbl, Arg1, Arg2, Env) -> % var =:= const
  {True,Env1} = mk_label(new, Env),
  NewArg1 = mk_var(Arg1),
  TrueLabName = hipe_icode:label_name(True),
  FalseLabName = map_label(Lbl),
  I = case Arg2 of
	{float,Float} ->
	  hipe_icode:mk_if(op_exact_eqeq_2,
			   [NewArg1, hipe_icode:mk_const(Float)],
			   TrueLabName, FalseLabName);
	_ ->
	  hipe_icode:mk_type(NewArg1, Arg2,
			     TrueLabName, FalseLabName)
      end,
  {[I,True], Env1}.

%%-----------------------------------------------------------------------
%%-----------------------------------------------------------------------

mk_move_and_var(Var,Env) ->
  case type(Var) of
    {const,C} ->
      V = mk_var(new),
      {[hipe_icode:mk_mov(V,hipe_icode:mk_const(C))], V, Env};
    var ->
      V = mk_var(Var),
      {[], V, Env}
  end.

%%-----------------------------------------------------------------------
%% Find names of closures and number of free vars.
%%-----------------------------------------------------------------------
-record(closure_info,{mfa, arity, fv_arity}).

find_closure_info(Code) -> mod_find_closure_info(Code, []).

mod_find_closure_info([FunCode|MoreCode], CI) -> 
  mod_find_closure_info(MoreCode,find_closure_info(FunCode, CI));
mod_find_closure_info([], CI) ->
  CI.

find_closure_info([{patched_make_fun,MFA={_M,_F,A},_Magic,FreeVarNum,_Index}|BeamCode],
		  ClosureInfo) ->
  NewClosure = %% A-FreeVarNum+1 (The real arity + 1 for the closure)
    #closure_info{mfa=MFA, arity=A-FreeVarNum+1, fv_arity=FreeVarNum},
  find_closure_info(BeamCode, [NewClosure|ClosureInfo]);
find_closure_info([_Inst|BeamCode], ClosureInfo) ->
  find_closure_info(BeamCode, ClosureInfo);
find_closure_info([], ClosureInfo) ->
  ClosureInfo.

%%-----------------------------------------------------------------------
%% Is closure
%%-----------------------------------------------------------------------

get_closure_info(MFA, [CI|Rest]) ->
  case CI#closure_info.mfa of
    MFA -> CI;
    _ -> get_closure_info(MFA, Rest)
  end;
get_closure_info(_, []) ->
  not_a_closure.

%%-----------------------------------------------------------------------
%% Patch closure entry.
%%-----------------------------------------------------------------------

patch_closure_entry(Icode, ClosureInfo)->
  Arity = ClosureInfo#closure_info.arity, 
  %% ?msg("Arity ~w\n",[Arity]),
  {Args, Closure, FreeVars} = 
    split_params(Arity, hipe_icode:icode_params(Icode), []),
  [Start|_] = hipe_icode:icode_code(Icode),
  {_LMin, LMax} = hipe_icode:icode_label_range(Icode),
  hipe_gensym:set_label(icode,LMax+1),
  {_VMin, VMax} = hipe_icode:icode_var_range(Icode),
  hipe_gensym:set_var(icode,VMax+1),
  MoveCode = gen_get_free_vars(FreeVars, Closure,
			       hipe_icode:label_name(Start)),
  Icode1 = hipe_icode:icode_code_update(Icode, MoveCode ++
					hipe_icode:icode_code(Icode)),
  Icode2 = hipe_icode:icode_params_update(Icode1, Args),
  Icode2.

%%-----------------------------------------------------------------------

gen_get_free_vars(Vars, Closure, StartName) ->
  [hipe_icode:mk_new_label()] ++ 
    get_free_vars(Vars, Closure, 1, []) ++ [hipe_icode:mk_goto(StartName)].

get_free_vars([V|Vs], Closure, No, MoveCode) ->
  %% TempV = hipe_icode:mk_new_var(),
  get_free_vars(Vs, Closure, No+1,
		[%% hipe_icode:mk_mov(TempV,hipe_icode:mk_const(No)),
		 hipe_icode:mk_primop([V],{closure_element, No}, [Closure])
		 |MoveCode]);
get_free_vars([],_,_,MoveCode) ->
  MoveCode.

%%-----------------------------------------------------------------------

split_params(1, [Closure|OrgArgs], Args) ->
  {lists:reverse([Closure|Args]), Closure, [Closure|OrgArgs]};
split_params(1, [], Args) ->
  Closure = hipe_icode:mk_new_var(),
  {lists:reverse([Closure|Args]),  Closure, [Closure]};
split_params(N, [ArgN|OrgArgs], Args) ->
  split_params(N-1, OrgArgs, [ArgN|Args]).

%%-----------------------------------------------------------------------

preprocess_code(ModuleCode) ->
  PatchedCode = patch_R7_funs(ModuleCode),
  ClosureInfo = find_closure_info(PatchedCode),
  {PatchedCode, ClosureInfo}.

%%-----------------------------------------------------------------------
%% Patches the "make_fun" BEAM instructions of R7 so that they also
%% contain the index that the BEAM loader generates for funs.
%% 
%% The index starts from 0 and is incremented by 1 for each make_fun
%% instruction encountered.
%%
%% Retained only for compatibility with BEAM code prior to R8.
%%
%% Temporarily, it also rewrites R8-PRE-RELEASE "make_fun2"
%% instructions, since their embedded indices don't work.
%%-----------------------------------------------------------------------

patch_R7_funs(ModuleCode) ->
  patch_make_funs(ModuleCode, 0).

patch_make_funs([F0|Fs], FunIndex0) ->
  {F,FunIndex} = patch_make_funs(F0, FunIndex0, []),
  [F|patch_make_funs(Fs, FunIndex)];
patch_make_funs([], _) -> [].

patch_make_funs([{make_fun,MFA,Magic,FreeVarNum}|Is], FunIndex, Acc) ->
  Patched = {patched_make_fun,MFA,Magic,FreeVarNum,FunIndex},
  patch_make_funs(Is, FunIndex+1, [Patched|Acc]);
patch_make_funs([{make_fun2,MFA,_BogusIndex,Magic,FreeVarNum}|Is], FunIndex, Acc) ->
  Patched = {patched_make_fun,MFA,Magic,FreeVarNum,FunIndex},
  patch_make_funs(Is, FunIndex+1, [Patched|Acc]);
patch_make_funs([I|Is], FunIndex, Acc) ->
  patch_make_funs(Is, FunIndex, [I|Acc]);
patch_make_funs([], FunIndex, Acc) ->
  {lists:reverse(Acc),FunIndex}.

%%-----------------------------------------------------------------------

find_mfa([{label,_}|Code]) ->
  find_mfa(Code);
find_mfa([{func_info,[{atom,M},{atom,F},A]}|_]) ->
  {M, F, A}.

%%-----------------------------------------------------------------------

%% Localize a particular function in a module
get_fun([[L, {func_info,[{atom,M},{atom,F},A]} | Is] | _], M,F,A) ->
  [L, {func_info,[{atom,M},{atom,F},A]} | Is];
get_fun([[_L1,_L2, {func_info,[{atom,M},{atom,F},A]} | _Is] | _], M,F,A) ->
  ?EXIT({'get_fun/4','Consecutive labels found; please re-create the .beam file'});
%%    [_L1,_L2, {func_info,[{atom,M},{atom,F},A]} | _Is];
get_fun([_|Rest], M,F,A) ->
  get_fun(Rest, M,F,A).    

%%-----------------------------------------------------------------------
%% Takes a list of arguments and returns the constants of them into
%% fresh temporaries.  Return a triple consisting of a list of move
%% instructions, a list of proper icode arguments and the new environment.
%%-----------------------------------------------------------------------

get_constants_in_temps(Args, Env) ->
  get_constants_in_temps(Args, [], [], Env).

get_constants_in_temps([Arg|Args], Instrs, Temps, Env) ->
  case get_constant_in_temp(Arg, Env) of
    {none,ArgVar,Env1} ->
      get_constants_in_temps(Args, Instrs, [ArgVar|Temps], Env1);
    {Instr,Temp,Env1} ->
      get_constants_in_temps(Args, [Instr|Instrs], [Temp|Temps], Env1)
  end;
get_constants_in_temps([], Instrs, Temps, Env) ->
  {lists:reverse(Instrs), lists:reverse(Temps), Env}.

%%  If Arg is a constant then put Arg in a fresh temp!
get_constant_in_temp(Arg, Env) ->
  case is_var(Arg) of
    true ->  % Convert into Icode variable format before return
      {none, mk_var(Arg), Env};
    false -> % Create a new temp and move the constant into it
      Temp = mk_var(new),
      Const = trans_const(Arg),
      {hipe_icode:mk_mov(Temp, Const), Temp, Env}
  end.

%%-----------------------------------------------------------------------
%% Makes a list of function arguments.
%%-----------------------------------------------------------------------

extract_fun_args(A) ->
  lists:reverse(extract_fun_args1(A)).

extract_fun_args1(0) ->
  [];
extract_fun_args1(1) ->
  [mk_var({r,0})];
extract_fun_args1(N) ->
  [mk_var({x,N-1}) | extract_fun_args1(N-1)].

%%-----------------------------------------------------------------------
%% Auxiliary translation for arguments of select_val & select_tuple_arity
%%-----------------------------------------------------------------------

trans_select_stuff(Reg, CasePairs) ->
  SwVar = case is_var(Reg) of
	    true ->
	      mk_var(Reg);
	    false ->
	      trans_const(Reg)
	  end,
  Cases = trans_pairs(CasePairs),
  {SwVar,Cases}.

trans_pairs([{Symbol,{f,Lbl}}|L]) ->
  [{trans_const(Symbol),map_label(Lbl)} | trans_pairs(L)];
trans_pairs([]) ->
  [].

%%-----------------------------------------------------------------------
%% Makes an Icode argument from a BEAM argument..
%%-----------------------------------------------------------------------
trans_arg(Arg) ->
  case is_var(Arg) of
    true ->
      mk_var(Arg);
    false ->
      trans_const(Arg)
  end.

%%-----------------------------------------------------------------------
%% Makes an Icode constant from a BEAM constant.
%%-----------------------------------------------------------------------

trans_const(Const) ->
  case Const of
    {atom,Atom} ->
      hipe_icode:mk_const(Atom);
    {integer,N} when is_integer(N) ->
      hipe_icode:mk_const(N);
    {float,Float} when is_float(Float) ->
      hipe_icode:mk_const(Float);
    {string,String} ->
      hipe_icode:mk_const(String);
    nil ->
      hipe_icode:mk_const([]);
    _  ->
      hipe_icode:mk_const(Const)
  end.

%%-----------------------------------------------------------------------
%% Make an icode variable of proper type
%%   Variables == 0 (mod 4) are X regs
%%   Variables == 1 (mod 4) are Y regs
%%   Variables == 2 (mod 4) are FR regs
%%   Variables == 3 (mod 4) are new temporaries
%% Tell hipe_gensym to update its state for each new thing created!!
%%-----------------------------------------------------------------------

mk_var({r,0}) ->
  hipe_icode:mk_var(0);

mk_var({x,R}) when is_integer(R) ->
  V = 5*R,
  hipe_gensym:update_vrange(icode,V),
  hipe_icode:mk_var(V);
mk_var({y,R}) when is_integer(R) ->
  V = 5*R+1,
  hipe_gensym:update_vrange(icode,V),
  hipe_icode:mk_var(V);
mk_var({fr,R}) when is_integer(R) ->
  V = 5*R+2,
  hipe_gensym:update_vrange(icode,V),
  case get(hipe_inline_fp) of
    true ->
      hipe_icode:mk_fvar(V);
    _ ->
      hipe_icode:mk_var(V)
  end;
mk_var(new) ->
  T = hipe_gensym:new_var(icode),
  V = 5*(T)+3,
  hipe_gensym:update_vrange(icode,V),
  hipe_icode:mk_var(V);
mk_var(reg) ->
  T = hipe_gensym:new_var(icode),
  V = 5*(T)+4,
  hipe_gensym:update_vrange(icode,V),
  hipe_icode:mk_reg(V).

%%-----------------------------------------------------------------------
%% Make an icode label of proper type
%%   Labels == 0 (mod 2) are actually occuring in the BEAM code
%%   Labels == 1 (mod 2) are new labels generated by the translation
%%-----------------------------------------------------------------------

mk_label(L, _) when is_integer(L) ->
  hipe_gensym:update_lblrange(icode,2*L),
  hipe_icode:mk_label(2*L);
mk_label(new, Env) ->
  L = hipe_gensym:new_label(icode),
  hipe_gensym:update_lblrange(icode,2*L+1),
  {hipe_icode:mk_label(2*L+1), Env}.

%% Maps from the BEAM's labelling scheme to our labelling scheme.
%% See mk_label to understand how it works.

map_label(L) ->
  2*L.

%%-----------------------------------------------------------------------
%% Returns the type of the given variables.
%%-----------------------------------------------------------------------

type({x,_}) ->
  var;
type({y,_}) ->
  var;
type({fr,_}) ->
  var;
type({atom,A}) ->
  {const,A};
type(nil) ->
  {const,[]};
type({integer,X}) when is_integer(X) ->
  {const,X};
type({float,X}) when is_float(X) ->
  {const,X};
type(X) ->
  ?EXIT({'type/1',unknown,X}).

%%-----------------------------------------------------------------------
%% Returns true iff the argument is a variable.
%%-----------------------------------------------------------------------

is_var({x,_}) ->
  true;
is_var({y,_}) ->
  true;
is_var({fr,_}) ->
  true;
is_var(_) ->
  false.

%%-----------------------------------------------------------------------
%% Fixes the code for catches by adding some code.
%%-----------------------------------------------------------------------

fix_catches([{'catch',N,Lbl}|Code]) ->
  {NewContLbl, _} =  mk_label(new,env__mk_env()),
  NewCont = hipe_icode:label_name(NewContLbl),
  CatchLbl = map_label(Lbl),
  {NewCatchLbl, _} =  mk_label(new,env__mk_env()),
  NewCatch =  hipe_icode:label_name(NewCatchLbl),
  {EndLabel,_} = mk_label(new,env__mk_env()),
  {CodeToCatch,RestOfCode} = split_code(Code,{catch_end,N}),
  CatchedCode = fix_catches(CodeToCatch),
  Code2 = fix_catches(RestOfCode),
  GotoEndLabel = hipe_icode:mk_goto(hipe_icode:label_name(EndLabel)),

  [hipe_icode:mk_pushcatch(NewCatch,NewCont),NewContLbl] ++ 
    CatchedCode ++
    [hipe_icode:mk_label(CatchLbl),
     hipe_icode:mk_remove_catch(NewCatch),
     GotoEndLabel,
     NewCatchLbl,
     hipe_icode:mk_restore_catch(
       mk_var({r,0}), NewCatch),
     GotoEndLabel,EndLabel | Code2];

fix_catches([{'try',N,Lbl}|Code]) ->
  {NewContLbl, _} =  mk_label(new,env__mk_env()),
  NewCont = hipe_icode:label_name(NewContLbl),
  CatchLbl = map_label(Lbl),
  {NewCatchLbl, _} =  mk_label(new,env__mk_env()),
  NewCatch =  hipe_icode:label_name(NewCatchLbl),
  {EndLabel,_} = mk_label(new,env__mk_env()),
  {CodeToCatch,RestOfCode} = split_code(Code,{try_end,N}),

  CatchedCode = fix_catches(CodeToCatch),
  Code2 = fix_catches(RestOfCode),
  GotoEndLabel = hipe_icode:mk_goto(hipe_icode:label_name(EndLabel)),
  GotoCatchLabel = hipe_icode:mk_goto(CatchLbl),

  [hipe_icode:mk_pushtry(NewCatch,NewCont),NewContLbl] ++ CatchedCode ++
    [
     hipe_icode:mk_remove_catch(NewCatch),
     GotoEndLabel,
     NewCatchLbl,
     hipe_icode:mk_restore_try(mk_var({x,1}),
				 mk_var({r,0}),
				 NewCatch),
     GotoCatchLabel,EndLabel | Code2];
fix_catches([Instr|Code]) ->
  [Instr|fix_catches(Code)];
fix_catches([]) ->
  [].

%% splits the code according to [ Before, To, After ]
split_code([], _) ->
  {[], []};
split_code([Inst = {enter,_,_,_,_},{label,_,_},To|Code], To) ->
  {NewLabel,_} = mk_label(new,env__mk_env()),
  {[Inst,NewLabel], Code};
split_code([{label,_,_},To|Code], To) ->
  {[], Code};
split_code([To|Code], To) ->
  {[], Code};
split_code([Instr|Code], To) ->
  {Lst1, Lst2} = split_code(Code, To),
  {[Instr|Lst1], Lst2}.

%%-----------------------------------------------------------------------
%% Removes the code between a fail instruction and the closest following
%% label.
%%-----------------------------------------------------------------------

remove_dead_code([I|Code]) ->
  case hipe_icode:type(I) of
    fail ->
      [I|remove_dead_code(skip_to_label(Code))];
    _ ->
      [I|remove_dead_code(Code)]
  end;
remove_dead_code([]) ->
  [].

%% returns the instructions from the closest label
skip_to_label([I|Code]) ->
  case hipe_icode:type(I) of
    label -> [I|Code];
    _ -> skip_to_label(Code)
  end;
skip_to_label([]) ->
  [].

%%-----------------------------------------------------------------------
%% This needs to be extended in case new architectures are added.
%%-----------------------------------------------------------------------

resolve_native_endianess(Flags) ->
  case {Flags band 16#10, get(hipe_target_arch)} of
    {16#10, ultrasparc} ->
      Flags band 5;
    {16#10, x86} ->
      (Flags bor 2) band 7;  
    _ ->
      Flags band 7
  end.

%%-----------------------------------------------------------------------
%% Potentially useful for debugging.
%%-----------------------------------------------------------------------

pp_beam(BeamCode, Options) ->
  case proplists:get_value(pp_beam,Options) of
    true ->
      beam_pp:pp(BeamCode);
    {file,FileName} ->
      {ok,File} = file:open(FileName,[write]),
      beam_pp:pp(File,BeamCode);
    _ -> %% includes "false" case
      ok
  end.   

%%-----------------------------------------------------------------------
%% Handling of environments -- used to process local tail calls.
%%-----------------------------------------------------------------------

%% Environment 
-record(environment, {mfa = {},entry}).

%% Constructor!
env__mk_env() ->
  #environment{}.

%% The MFA of the current function 
env__store_mfa(M, F, A, Env) ->
  Env#environment{mfa = {M,F,A}}.

%% The entrypoint to the current function
env__store_entrypoint(Entry, Env) ->
  Env#environment{entry = Entry}.

%% Get current MFA
env__get_mfa(Env) ->
  Env#environment.mfa.

%% Get entry point of the current function!
env__get_entry(Env) ->
  Env#environment.entry.

%%-----------------------------------------------------------------------
