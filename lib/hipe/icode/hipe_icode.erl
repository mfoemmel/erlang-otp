%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Intermediate Code
%% ====================================================================
%%  Filename : 	hipe_icode.erl
%%  Module   :	hipe_icode
%%  Purpose  :  Provide primops for the Icode data structure.
%%  Notes    : 
%%  History  :	1997-? Erik Johansson (happi@csd.uu.se): Created.
%%           :  2001-01-30 EJ (happi@csd.uu.se): 
%%                             Apply, primop, guardop removed
%%  CVS      :
%%              $Author: richardc $
%%              $Date: 2002/09/24 13:48:04 $
%%              $Revision: 1.26 $
%% ====================================================================
%%  TODO     :  Add some assertions to the constructors.
%%              Split into several modules.
%%
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_icode).

-include("hipe_icode.hrl").
-include("../main/hipe.hrl").

-define(hash, hipe_hash).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% icode:
%% ~~~~~~
%%    icode consists of a function ({M,F,A}), a list of params and
%%    a list of instructions.
%%
%% instructions (and their components):
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%%    'if'               - cond, args, true_label, false_label
%%    switch_val         - arg, fail_label, length, [cases]
%%    switch_tuple_arity - arg, fail_label, length, [cases]
%%                         cases are pairs: {symbol, label}
%%    type               - typ_expr, arg, true_label, false_label
%%    goto               - label
%%    label              - name
%%
%%    mov                - dst, src
%%
%%    call               - [dst], fun, [arg], type, continuation, fail, code_change, in_guard
%%                         type is one of {local, remote, primop}
%%                         code_cahnge and in_guard is either true or false.
%%    enter              - fun, [arg], type, code_change
%%                         type is one of {local, remote, primop}
%%                         code_change is either true or false.
%%    return             - [var]
%%
%%    pushcatch          - label
%%    restore_catch      - dst, label
%%    remove_catch       - label
%%    fail               - [reason], type
%%                         type is one of {exit, throw, fault, fault2}  
%%                         for fault2 reason is actually reason and trace.
%%
%%    comment            - text
%%
%%  - 'fun' is a tuple {Module, Function, Arity}
%%  - A constant can only show up on the rhs of a mov instruction
%%      and in 'if' and switch_*
%%
%%        Classification of primops should be like this:
%%         - erlang:exit/1, erlang:throw/1 erlang:fault/1 
%%           erlang:fault/2 
%%            should use the fail-instruction in Icode.
%%         - Calls or tail-recursive calls to  BIFs, operators, or
%%            internal functions, all other Erlang functions
%%            should be implemented with call or enter respectively.
%%
%%
%% primops:
%% ~~~~~~~~
%%  Constructors:
%%    cons                       - [Car, Cdr]
%%    mktuple                    - [Element1, Element2, ..., ElementN]
%%    call_fun                   - [BoundArg1, ..., BoundArgN, Fun]
%%    enter_fun                  - [BoundArg1, ..., BoundArgN, Fun]
%%    {mkfun,MFA,MagicNum,Index} - [FreeVar1, FreeVar2, ..., FreeVarN]
%%
%%  Binaries:
%%    bs_init
%%    {bs_put_string, Bytes, Size}
%%    bs_final
%%
%%  Selectors:
%%    element              - [Index, Tuple]
%%    unsafe_hd            - [List]
%%    unsafe_tl            - [List]
%%    {unsafe_element, N}  - [Tuple], N:integer
%%    {unsafe_update_element, N}  - [Tuple, Val], N:integer
%%
%%  Arithmetic:       [Arg1, Arg2]
%%    '+','-','*','/','div','rem',
%%    'band','bor','bxor','bnot'
%%    'bsl','bsr',
%%
%%  Receive:         
%%    get_msg       - []
%%    next_msg      - []
%%    select_msg    - []
%%    set_timeout   - [Timeout]
%%    suspend_msg   - []
%%
%%  Low-level:
%%    redtest
%%    gc_test
%%
%%    <and bifs> as a MFA.
%%
%% guardops: (primops that can be used in guards and can fail.)
%% ~~~~~~~~
%%  Selectors:
%%    element - [Index, Tuple]
%%    unsafe_hd -[List]
%%    unsafe_tl -[List]
%%    {unsafe_element, N} - [Tuple], N:integer
%%
%%  Arithmetic:       [Arg1, Arg2]
%%    '+','-','*','/','div','rem',
%%   'band','bor','bxor','bnot'
%%   'bsl','bsr',
%%    fix_add, fix_sub               %% Do these exist?
%%
%%  Concurrency:
%%    {erlang,self,0}          - [] 
%%
%%
%% relops (as used in if instruction):
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%%    gt, lt, geq, leq,
%%    eqeq, neq, exact_eqeq, exact_neq
%%
%%
%% type expressions
%% ~~~~~~~~~~~~~~~~
%%    list
%%    nil
%%    cons
%%    tuple
%%    {tuple, N}
%%    atom
%%    {atom, Atom}
%%    constant
%%    number
%%    integer
%%    {integer, N}
%%    fixnum
%%    bignum
%%    float
%%    pid
%%    port
%%    reference
%%    binary
%%    function
%% ____________________________________________________________________



%% ____________________________________________________________________
%% 
%%
%% Exports
%%
-export([mk_icode/7, %% mk_icode(Fun, Params, IsClosure, IsLeaf, 
	 %%          Code, VarRange, LabelRange)
	 mk_icode/8, %% mk_icode(Fun, Params, IsClosure, IsLeaf, 
	 %%          Code, Data, VarRange, LabelRange)
	 icode_fun/1,
	 icode_params/1,
	 icode_params_update/2,
	 icode_is_closure/1,
	 icode_is_leaf/1,
	 icode_code/1,
	 icode_code_update/2,
	 icode_data/1,
	 icode_data_update/2,
	 icode_var_range/1,
	 icode_label_range/1,
	 icode_info/1,
	 icode_info_add/2,
	 icode_info_update/2]).

-export([	 
		 mk_if/4,           %% mk_if(Op, Args, TrueLbl, FalseLbl)
		 mk_if/5,           %% mk_if(Op, Args, TrueLbl, FalseLbl, P)
		 if_op/1,
		 if_true_label/1,
		 if_false_label/1,
		 if_args/1,
		 if_pred/1,
		 is_if/1,


		 mk_switch_val/4,
		 mk_switch_val/5,
		 switch_val_arg/1,
		 switch_val_fail_label/1,
		 switch_val_length/1,
		 switch_val_cases/1,
		 switch_val_info/1,
		 is_switch_val/1,

		 mk_switch_tuple_arity/4,
		 mk_switch_tuple_arityl/5,
		 switch_tuple_arity_arg/1,
		 switch_tuple_arity_fail_label/1,
		 switch_tuple_arity_length/1,
		 switch_tuple_arity_cases/1,
		 switch_tuple_arity_info/1,
		 is_switch_tuple_arity/1,


		 mk_type/4,        %% mk_type(X, Type, TrueLbl, FalseLbl)
		 mk_type/5,        %% mk_type(X, Type, TrueLbl, FalseLbl, P)
		 type_var/1,
		 type_type/1,
		 type_true_label/1,
		 type_false_label/1,
		 type_pred/1,
		 is_type/1,

		 mk_guardop/5,     %% mk_guardop(Dst, Fun, Args,  Continuation, Fail)
		 mk_primop/3,      %% mk_primop(Dst, Fun, Args)
		 mk_primop/5,      %% mk_primop(Dst, Fun, Args, Cont, Fail)
		 mk_call/5,        %% mk_call(Dst, Mod, Fun, Args, Type)
		 mk_call/7,        %% mk_call(Dst, Mod, Fun, Args, Type,
		 %%        Continuation, Fail)
		 call_dst/1,
		 call_dst_update/2,
		 call_args/1,
		 call_args_update/2,
		 call_fun/1,
		 call_type/1,
		 call_continuation/1,
		 call_fail/1,
		 call_set_fail/2,
		 call_set_continuation/2,
		 is_call/1, 
		 call_in_guard/1,
		 call_code_change/1,

		 mk_goto/1,                % mk_goto(Lbl)
		 goto_label/1,

		 mk_enter/4,               %% mk_enter(Mod, Fun, Args, Type)
		 mk_enter_primop/2,        %% mk_enter(Op, Type)
		 enter_fun/1,
		 enter_args/1,
		 enter_type/1,
		 is_enter/1,

		 mk_fclearerror/0,
		 mk_fmov/2,                % mk_fmov(Dst, Src)
		 mk_fmov/3,                % mk_fmov(Dst, Src, Negate)
		 mk_unsafe_untag_float/2,  % (Dst, Src)
		 mk_unsafe_tag_float/2,    % (Dst, Src)
		 mk_conv_to_float/2,       % (Dst, Src)

		 mk_gctest/0,
		 mk_redtest/0,
		 mk_return/1,              % mk_return(Vars)
		 mk_fail/1,                % mk_fail(Reason) type = exit
		 mk_fail/2,                % mk_fail(Reason, Type)
		 mk_mov/2,                 % mk_mov(Dst, Src)
		 mk_movs/2,                % mk_movs(DstList, SrcList)
		 mk_pushcatch/1,           % mk_pushcatch(Label)
		 mk_restore_catch/2,       % mk_restore_catch(Dst, Label)
		 mk_remove_catch/1,        % mk_remove_catch(Label)
		 mk_elements/2,            % mk_elements(Tuple, Vars)
		 mk_label/1,               % mk_label(Name)
		 mk_new_label/0,           % mk_new_label()
		 mk_comment/1,             % mk_comment(Text)
		 mk_const/1,               % mk_const(Const)
		 mk_const_fun/4,           % mk_const_fun(MFA,U,I,Args)
		 mk_var/1,                 % mk_var(Id)
		 mk_reg/1,                 % mk_reg(Id)
		 mk_fvar/1,                % mk_fvar(Id)
		 mk_new_var/0,             % mk_new_var()
                 mk_phi/2,                 % mk_phi(Name, PredList)
		 info_add/2,
		 info_update/2,
                 phi_name/1]).


%%
%% Identifiers
%%

-export([type/1,
	 is_fail/1,
	 is_return/1,
	 is_mov/1,
	 is_pushcatch/1,
	 is_restore_catch/1,
	 is_remove_catch/1,
	 is_goto/1,
	 is_label/1,
	 is_comment/1,
	 is_const/1,
	 is_const_fun/1,
	 is_var/1,
	 is_fvar/1,
	 is_reg/1,
	 is_var_or_fvar_or_reg/1,
	 is_uncond/1,
	 is_fclearerror/1,
	 is_fmov/1,
	 is_unsafe_untag_float/1,
	 is_unsafe_tag_float/1,
	 is_conv_to_float/1,
         is_phi/1]).


%%
%% Selectors
%%

-export([subst_phi_arg/3,
	 phi_getArgMap/1,
	 phi_predList/1,
	 phi_dst/1,
	 mov_dst/1,
	 mov_src/1,
	 mov_src_update/2,
	 pushcatch_label/1,
	 restore_catch_dst/1,
	 restore_catch_label/1,
	 remove_catch_label/1,
	 label_name/1,
	 comment_text/1,
	 return_vars/1,
	 fail_reason/1,
	 fail_type/1,
	 var_name/1,
	 fvar_name/1,
   reg_name/1,		 
	 const_value/1,
	 info/1,
	 fmov_dst/1,
	 fmov_src/1,
	 fmov_negate/1,
	 fmov_src_update/2,
	 unsafe_untag_float_dst/1,
	 unsafe_untag_float_src/1,
	 unsafe_untag_float_src_update/2,
	 unsafe_tag_float_dst/1,
	 unsafe_tag_float_src/1,
	 unsafe_tag_float_src_update/2,
	 conv_to_float_dst/1,
	 conv_to_float_src/1,
	 conv_to_float_src_update/2]).

%%
%% Misc
%%

-export([args/1,
	 uses/1,
	 defines/1,
         phi_getArgMap/1,
         phi_predList/1,
         phi_dst/1,
	 is_pure/1,
	 pp/1,
	 pp/2,
	 pp_exit/1,
	 preprocess_code/1,
	 strip_comments/1,
	 subst/2,
	 subst_uses/2,
	 subst_defines/2,
	 redirect_jmp/3,
	 successors/1,
	 is_branch/1,
	 is_leaf/1,
	 is_leaf0/1 % needs to be used in beam_to_icode
	]).

-export([highest_var/1,highest_label/1]).


%% ____________________________________________________________________
%% 
%% icode
%%

mk_icode(Fun, Params, Closure, Leaf, Code, VarRange, LabelRange) ->
  #icode{'fun'=Fun, params=Params, code=Code,
	 closure=Closure,
	 leaf=Leaf,
	 data=hipe_consttab:new(),
	 var_range=VarRange,
	 label_range=LabelRange}.
mk_icode(Fun, Params, Closure, Leaf, Code, Data, VarRange, LabelRange) ->
  #icode{'fun'=Fun, params=Params, code=Code,
	 data=Data, closure=Closure, leaf=Leaf,
	 var_range=VarRange,
	 label_range=LabelRange}.
icode_fun(Icode) -> Icode#icode.'fun'.
icode_params(Icode) -> Icode#icode.params.
icode_params_update(Icode, Params) -> 
  Icode#icode{params=Params}.
icode_is_closure(Icode) -> Icode#icode.closure.
icode_is_leaf(Icode) -> Icode#icode.leaf.
icode_code(Icode) -> Icode#icode.code.
icode_code_update(Icode,NewCode) -> 
  Vmax = hipe_icode:highest_var(NewCode),
  Lmax = hipe_icode:highest_label(NewCode),
  Icode#icode{code=NewCode, var_range={0,Vmax},
	      label_range={0,Lmax}}.
icode_data(Icode) -> Icode#icode.data.
icode_data_update(Icode, NewData) -> Icode#icode{data=NewData}.
icode_var_range(Icode) -> Icode#icode.var_range.
icode_label_range(Icode) -> Icode#icode.label_range.
icode_info(Icode) -> Icode#icode.info.
icode_info_add(Icode, Info) -> Icode#icode{info=[Info|icode_info(Icode)]}.
icode_info_update(Icode, Info) -> Icode#icode{info=Info}.

%% ____________________________________________________________________
%% Instructions
%%

%%
%% if
%%

mk_if(Op, Args, TrueLbl, FalseLbl) ->
  #'if'{op=Op, args=Args, true_label=TrueLbl, false_label=FalseLbl, p=0.5}.
mk_if(Op, Args, TrueLbl, FalseLbl, P) ->
  #'if'{op=Op, args=Args, true_label=TrueLbl, false_label=FalseLbl, p=P}.
if_op(Cond) -> Cond#'if'.op.
if_args(Cond) -> Cond#'if'.args.
if_true_label(Cond) -> Cond#'if'.true_label.
if_false_label(Cond) -> Cond#'if'.false_label.
if_pred(Cond) -> Cond#'if'.p.
is_if(I) when record(I, 'if') -> true;
is_if(_) -> false.

%%
%% switch_val
%%

mk_switch_val(Arg, Fail_label, Length, Cases) ->
  #switch_val{arg=Arg, fail_label=Fail_label, length=Length, cases=Cases}.
mk_switch_val(Arg, Fail_label, Length, Cases, Info) ->
  #switch_val{arg=Arg, fail_label=Fail_label, length=Length,
	      cases=Cases, info = Info}.
switch_val_arg(Cond) -> Cond#switch_val.arg.
switch_val_fail_label(Cond) -> Cond#switch_val.fail_label.
switch_val_length(Cond) -> Cond#switch_val.length.
switch_val_cases(Cond) -> Cond#switch_val.cases.
switch_val_info(Cond) -> Cond#switch_val.info.
is_switch_val(I) when record(I, switch_val) -> true;
is_switch_val(_) -> false.

%%
%% switch_val
%%

mk_switch_tuple_arity(Arg, Fail_label, Length, Cases) ->
  #switch_tuple_arity{arg=Arg, fail_label=Fail_label, length=Length, cases=Cases}.
mk_switch_tuple_arityl(Arg, Fail_label, Length, Cases, Info) ->
  #switch_tuple_arity{arg=Arg, fail_label=Fail_label, length=Length,
		      cases=Cases, info = Info}.
switch_tuple_arity_arg(Cond) -> Cond#switch_tuple_arity.arg.
switch_tuple_arity_fail_label(Cond) -> Cond#switch_tuple_arity.fail_label.
switch_tuple_arity_length(Cond) -> Cond#switch_tuple_arity.length.
switch_tuple_arity_cases(Cond) -> Cond#switch_tuple_arity.cases.
switch_tuple_arity_info(Cond) -> Cond#switch_tuple_arity.info.
is_switch_tuple_arity(I) when record(I, switch_tuple_arity) -> true;
is_switch_tuple_arity(_) -> false.

%%
%% type
%%

mk_type(X, Type, TrueLbl, FalseLbl) -> 
  #type{type=Type, var=X, true_label=TrueLbl, false_label=FalseLbl, p=0.5}.
mk_type(X, Type, TrueLbl, FalseLbl, P) -> 
  #type{type=Type, var=X, true_label=TrueLbl, false_label=FalseLbl, p=P}.
type_var(T) -> T#type.var.
type_type(T) -> T#type.type.
type_true_label(T) -> T#type.true_label.
type_false_label(T) -> T#type.false_label.
type_pred(T) -> T#type.p.
is_type(I) when record(I, type) -> true;
is_type(_) -> false.

%%
%% goto
%%

mk_goto(Lbl) -> #goto{label=Lbl}.
goto_label(G) -> G#goto.label.
is_goto(I) when record(I, goto) -> true;
is_goto(_) -> false.

%%
%% return
%%

mk_return(Xs) -> #return{vars=Xs}.
return_vars(R) -> R#return.vars.
is_return(I) when record(I, return) -> true;
is_return(_) -> false.

%%
%% fail
%%

mk_fail(Reason) when list(Reason) -> #fail{reason=Reason, type=exit}.
mk_fail(Reason, Type) when list(Reason) -> #fail{reason=Reason, type=Type}.
is_fail(I) when record(I, fail) -> true;
is_fail(_) -> false.
fail_reason(F) -> F#fail.reason.
fail_type(F) -> F#fail.type.

%%
%% move
%%

mk_mov(X, Y) -> #mov{dst=X, src=Y}.
mov_dst(M) -> M#mov.dst.
mov_src(M) -> M#mov.src.
mov_src_update(M, NewSrc) -> M#mov{src=NewSrc}.
is_mov(I) when record(I, mov) -> true;
is_mov(_) -> false.


%%
%% phi  
%%

mk_phi(Var, PredList) ->
  #phi{dst = Var, name = Var, args = ?hash:empty(),
       predList = PredList}.
phi_dst(P) -> P#phi.dst.
phi_name(P) -> P#phi.name.
phi_predList(P) -> P#phi.predList.
phi_getArgMap(P) -> P#phi.args.
subst_phi_arg(P, Pred, Value) ->
  P#phi{args = ?hash:update(Pred, Value, P#phi.args)}.
phi_args(P) -> 
  get_phi_args(P#phi.predList, P#phi.args, P#phi.name, []).

replace_phi_args([Pred | T], Map, Subst) ->
  Map2 = case ?hash:lookup(Pred, Map) of
           not_found    ->   Map;
           {found, Var} ->   replace_phi_arg(Pred, Map, Var, Subst)
         end,
  replace_phi_args(T, Map2, Subst);

replace_phi_args([], Map, _) -> Map.

replace_phi_arg(Pred, Map, Var, [{Var, New} | T]) ->
  replace_phi_arg(Pred, ?hash:update(Pred, New, Map), Var, T);

replace_phi_arg(Pred, Map, Var, [_ | T]) ->
  replace_phi_arg(Pred, Map, Var, T);

replace_phi_arg(_, Map, _, []) -> Map.

get_phi_args([Pred | T], Map, Name, Result) ->
  Arg = case ?hash:lookup(Pred, Map) of
          not_found      -> Name;
          {found, Other} -> Other
        end, 
    get_phi_args(T, Map, Name, [Arg|Result]);

get_phi_args([], _, _, Result) -> Result.

is_phi(I) when record(I, phi) -> true;
is_phi(_) -> false.

%%
%% call
%%

mk_call(Dst, M, F, Args, Type) -> %% Deprecated Happi 001106.
  Change = 
    case Type of
      local -> false;
      remote -> true;
      primop -> false;
      _ -> exit(bad_call_type)
    end,
  #call{dst=Dst, 'fun'={M,F,length(Args)}, args=Args, type=Type,
	continuation_label=[],fail_label=[],
	in_guard=false, code_change=Change}.
mk_call(Dst, M, F, Args, Type, Continuation, Fail) -> 
  #call{dst=Dst, 'fun'={M,F,length(Args)}, args=Args, type=Type,
	continuation_label=Continuation, fail_label=Fail,
	in_guard=false, code_change=false}.
call_dst(C) -> C#call.dst.
call_dst_update(C,Dest) -> C#call{dst=Dest}.
call_args(C) -> C#call.args.
call_args_update(C,Args) -> C#call{args=Args}.
call_fun(C) -> C#call.'fun'.
call_type(C) -> C#call.type.
call_continuation(C) -> C#call.continuation_label.
call_fail(C) ->  C#call.fail_label.
call_set_continuation(I, Continuation) ->  
  I#call{continuation_label = Continuation}.
call_set_fail(I, Fail) ->
  I#call{fail_label = Fail}.
is_call(I) when record(I, call) -> true;
is_call(_) -> false.
call_in_guard(I) -> I#call.in_guard.
call_code_change(I) -> I#call.code_change.

%%
%% enter
%%

mk_enter(Mod, Fun, Args, Type) -> 
  #enter{'fun'={Mod, Fun, length(Args)}, args=Args, type=Type}.
enter_fun(E) -> E#enter.'fun'.
enter_args(E) -> E#enter.args.
enter_type(E) -> E#enter.type.
%%enter_fail(I) ->  I#enter.fail_label.
%%enter_set_fail(I, Fail) ->
%%  I#enter{fail_label = Fail}.
is_enter(I) when record(I, enter) -> true;
is_enter(_) -> false.

mk_enter_primop(Op, Args) ->
  #enter{'fun'=Op, args=Args, type=primop}.

%%
%% pushcatch
%%

mk_pushcatch(Label) ->
  #pushcatch{label=Label}.
pushcatch_label(P) -> P#pushcatch.label.
is_pushcatch(I) when record(I, pushcatch) -> true;
is_pushcatch(_) -> false.

%%
%% remove_catch
%%

mk_remove_catch(Label) -> #remove_catch{id = Label}.
remove_catch_label(P) -> P#remove_catch.id.
is_remove_catch(I) when record(I, remove_catch) -> true;
is_remove_catch(_) -> false.

%%
%% restore_catch
%%

mk_restore_catch(Dst, Label) ->
  #restore_catch{dst=Dst, id=Label}.
restore_catch_dst(P) -> P#restore_catch.dst.
restore_catch_label(P) -> P#restore_catch.id.
is_restore_catch(I) when record(I, restore_catch) -> true;
is_restore_catch(_) -> false.

%%
%% label
%%

mk_label(Name) -> #label{name=Name}.
label_name(L) -> L#label.name.
is_label(I) when record(I, label) ->true;
is_label(_) -> false.

%%
%% comment
%%

mk_comment(Txt) -> #comment{text=Txt}.
comment_text(C) -> C#comment.text.
is_comment(I) when record(I, comment) -> true;
is_comment(_) -> false.

%% ____________________________________________________________________
%% 

%%
%% Arguments (variables and constants)
%%

mk_const(C) -> {const, {flat, C}}.
mk_const_fun(MFA,U,I,Args) -> {const, {const_fun, {MFA,U,I,Args}}}.
const_value({const, {flat, X}}) -> X;
const_value({const, {const_fun, X}}) -> X.
is_const({const, _}) -> true;
is_const(_) -> false.
is_const_fun({const, {const_fun, _}}) -> true;
is_const_fun(_) -> false.


mk_var(V) -> {var, V}.
var_name({var, Name}) -> Name.
is_var({var, _}) -> true;
is_var(_) -> false.

mk_reg(V) -> {reg, V}.
reg_name({reg, Name}) -> Name.
is_reg({reg, _}) -> true;
is_reg(_) -> false.

is_var_or_fvar_or_reg({var, _}) -> true;
is_var_or_fvar_or_reg({fvar, _}) -> true;
is_var_or_fvar_or_reg({reg, _}) -> true;

is_var_or_fvar_or_reg(_) -> false.

mk_fvar(V) -> {fvar, V}.
fvar_name({fvar, Name}) -> Name.
is_fvar({fvar, _}) -> true;
is_fvar(_) -> false.

%%
%% Misc. primops
%%

mk_primop(Dst, Fun, Args) ->
  #call{dst=Dst, 'fun'=Fun, args=Args, type=primop,
	continuation_label=[],fail_label=[],
	in_guard=false, code_change=false}.
mk_primop(Dst, Fun, Args, Cont, Fail) ->
  #call{dst=Dst, 'fun'=Fun, args=Args, type=primop,
	continuation_label=Cont ,fail_label=Fail,
	in_guard=false, code_change=false}.
mk_guardop(Dst, Fun, Args, True, False) ->
  #call{dst=Dst, 'fun'=Fun, args=Args, type=primop,
	continuation_label=True,fail_label=False,
	in_guard=true, code_change=false}.
mk_gctest() -> mk_primop([], gc_test, []).
mk_redtest() -> mk_primop([], redtest, []).


%%
%% Floating point
%%

%%
%% fclearerror
%%

mk_fclearerror() ->
  #fclearerror{info=[]}.
is_fclearerror(C) when record(C, fclearerror) -> true;
is_fclearerror(_) -> false.

%%
%% fmove
%%

%% negate is either true or false. False is default
mk_fmov(X, Y) ->
  mk_fmov(X, Y, false).
mk_fmov(X, Y, Neg) -> #fmov{dst=X, src=Y, negate=Neg}.
fmov_dst(M) -> M#fmov.dst.
fmov_src(M) -> M#fmov.src.
fmov_src_update(M, NewSrc) -> M#fmov{src=NewSrc}.
fmov_negate(M) -> M#fmov.negate.
is_fmov(I) when record(I, fmov) -> true;
is_fmov(_) -> false.

%%
%% Unsafe untag float
%%

mk_unsafe_untag_float(X, Y) -> #unsafe_untag_float{dst=X, src=Y}.
unsafe_untag_float_dst(U) -> U#unsafe_untag_float.dst.
unsafe_untag_float_src(U) -> U#unsafe_untag_float.src.
unsafe_untag_float_src_update(U, NewSrc) -> U#unsafe_untag_float{src=NewSrc}.
is_unsafe_untag_float(U) when record(U, unsafe_untag_float) -> true;
is_unsafe_untag_float(_) -> false.

%%
%% Unsafe tag float
%%

mk_unsafe_tag_float(X, Y) -> #unsafe_tag_float{dst=X, src=Y}.
unsafe_tag_float_dst(U) -> U#unsafe_tag_float.dst.
unsafe_tag_float_src(U) -> U#unsafe_tag_float.src.
unsafe_tag_float_src_update(U, NewSrc) -> U#unsafe_tag_float{src=NewSrc}.
is_unsafe_tag_float(U) when record(U, unsafe_tag_float) -> true;
is_unsafe_tag_float(_) -> false.

%%
%% Convert to float
%%

mk_conv_to_float(X, Y) -> #conv_to_float{dst=X, src=Y}.
conv_to_float_dst(C) -> C#conv_to_float.dst.
conv_to_float_src(C) -> C#conv_to_float.src.
conv_to_float_src_update(C, NewSrc) -> C#conv_to_float{src=NewSrc}.
is_conv_to_float(C) when record(C, conv_to_float) -> true;
is_conv_to_float(_) -> false.
  

%%
%% info for all instructions
%%

info(I) ->
  case type(I) of
    'if' -> I#'if'.info;
    type -> I#type.info;
    goto -> I#goto.info;
    mov -> I#mov.info;
    fail -> I#fail.info;
    call -> I#call.info;
    enter -> I#enter.info;
    return -> I#return.info;
    pushcatch -> I#pushcatch.info;
    restore_catch -> I#restore_catch.info;
    remove_catch -> I#remove_catch.info;
    comment -> I#comment.info;
    switch_val -> I#switch_val.info;
    switch_tuple_arity -> I#switch_tuple_arity.info;
    label -> I#label.info;
    fclearerror -> I#fclearerror.info;
    fmov -> I#fmov.info;
    unsafe_untag_float -> I#unsafe_untag_float.info;
    unsafe_tag_float -> I#unsafe_tag_float.info;
    conv_to_float -> I#conv_to_float.info  
  end.


info_add(Instr, Info) ->
  info_update(Instr, [Info | info(Instr)]).


info_update(I, NewInfo) ->
  case type(I) of 
    'if' -> I#'if'{info = NewInfo};
    type -> I#type{info = NewInfo};
    goto -> I#goto{info = NewInfo};
    mov -> I#mov{info = NewInfo};
    fail -> I#fail{info = NewInfo};
    call -> I#call{info = NewInfo};
    enter -> I#enter{info = NewInfo};
    return -> I#return{info = NewInfo};
    pushcatch -> I#pushcatch{info = NewInfo};
    restore_catch -> I#restore_catch{info = NewInfo};
    remove_catch -> I#remove_catch{info = NewInfo};
    comment -> I#comment{info = NewInfo};
    switch_val -> I#switch_val{info = NewInfo};
    switch_tuple_arity -> I#switch_tuple_arity{info = NewInfo};
    label -> I#label{info = NewInfo};
    fclearerror -> I#fclearerror{info=NewInfo};
    fmov -> I#fmov{info = NewInfo};
    unsafe_untag_float -> I#unsafe_untag_float{info = NewInfo};
    unsafe_tag_float -> I#unsafe_tag_float{info = NewInfo};
    conv_to_float -> I#conv_to_float{info = NewInfo}
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Liveness info 
%%

uses(I) ->
  remove_constants(args(I)).

args(I) ->
  case element(1, I) of
    'if' -> if_args(I);
    switch_val -> [switch_val_arg(I)];
    switch_tuple_arity -> [switch_tuple_arity_arg(I)];
    type -> [type_var(I)];
    mov -> [mov_src(I)];
    fail -> fail_reason(I);
    call -> I#call.args;
    enter -> I#enter.args;
    return -> I#return.vars;
    fmov -> [fmov_src(I)];
    unsafe_untag_float -> [unsafe_untag_float_src(I)];
    unsafe_tag_float -> [unsafe_tag_float_src(I)];
    conv_to_float -> [conv_to_float_src(I)];
    phi -> get_phi_args(I#phi.predList, I#phi.args, I#phi.name, []);
    %%    goto -> [];
    %%    pushcatch -> [];
    %%    restore_catch -> [];
    %%    remove_catch -> [];
    %%    comment -> [];
    %%    label -> []
    _ -> []
  end.


defines(I) ->
  case element(1, I) of
    mov -> remove_constants([I#mov.dst]);
    fmov -> remove_constants([I#mov.dst]);
    call -> remove_constants(I#call.dst);
    restore_catch -> remove_constants([I#restore_catch.dst]);
    unsafe_untag_float -> [unsafe_untag_float_dst(I)];
    unsafe_tag_float -> [unsafe_tag_float_dst(I)];
    conv_to_float -> [conv_to_float_dst(I)];
    phi -> remove_constants([I#phi.dst]);
    %%    'if' -> [];
    %%    switch_val -> [];
    %%    switch_tuple_arity -> [];
    %%    type -> [];
    %%    goto -> [];
    %%    fail -> [];
    %%    enter -> [];
    %%    return -> [];
    %%    pushcatch -> [];
    %%    remove_catch -> [];
    %%    comment -> [];
    %%    label -> []
    _ -> []
  end.



remove_constants([]) ->
  [];
remove_constants([{const, _}|Xs]) ->
  remove_constants(Xs);
remove_constants([{var, Var}|Xs]) ->
  [{var, Var} | remove_constants(Xs)];
remove_constants([{fvar, Var}|Xs]) ->
  [{fvar, Var} | remove_constants(Xs)];
remove_constants([{colored, Var}|Xs]) ->
  [{colored, Var} | remove_constants(Xs)];
remove_constants([_|Xs]) ->
  remove_constants(Xs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Utilities
%%

%%
%% Returns the type of the icode instruction X. Hopefully, this can
%% be compiled better than repeated calls to hipe_icode:is_branch/is_mov/...
%%

type(I) when tuple(I) -> element(1, I);
type(O) -> ?EXIT({bad_icode_type,O}).

%%
%% Substitution: replace occurrences of X by Y if {X,Y} is in the
%%   Subst_list.

subst(Subst, X) ->
  subst_defines(Subst, subst_uses(Subst,X)).

subst_uses(Subst, X) ->
  case type(X) of
    'if' -> X#'if'{args=subst_list(Subst, X#'if'.args)};
    switch_val -> X#switch_val{arg=subst1(Subst, X#switch_val.arg)};
    switch_tuple_arity ->
      X#switch_tuple_arity{arg=subst1(Subst, X#switch_tuple_arity.arg)};
    type -> X#type{var=subst1(Subst, X#type.var)};
    mov -> X#mov{src=subst1(Subst, X#mov.src)};
    fail -> X#fail{reason=subst_list(Subst,fail_reason(X))};
    call -> X#call{args=subst_list(Subst, X#call.args)};
    enter -> X#enter{args=subst_list(Subst, X#enter.args)};
    return -> X#return{vars=subst_list(Subst, X#return.vars)};
    fmov -> X#fmov{src=subst1(Subst, X#fmov.src)};
    unsafe_untag_float -> X#unsafe_untag_float
			    {src=subst1(Subst, X#unsafe_untag_float.src)};
    unsafe_tag_float -> X#unsafe_tag_float
			  {src=subst1(Subst, X#unsafe_tag_float.src)};
    conv_to_float -> X#conv_to_float
		       {src=subst1(Subst, X#conv_to_float.src)};
    phi -> X#phi{args = replace_phi_args(X#phi.predList, X#phi.args, Subst)};
    _ -> X
	 %%    goto -> X;
	 %%    pushcatch -> X;
	 %%    restore_catch -> X;
	 %%    remove_catch -> X;
	 %%    comment -> X;
	 %%    label -> X
  end.

subst_defines(Subst,X) ->
  case type(X) of
    mov ->  X#mov{dst=subst1(Subst,X#mov.dst)};
    call -> X#call{dst=subst_list(Subst,X#call.dst)};
    restore_catch -> X#restore_catch{dst=subst1(Subst,X#restore_catch.dst)};
    fmov ->  X#fmov{dst=subst1(Subst,X#mov.dst)};
    unsafe_untag_float -> X#unsafe_untag_float
			    {dst=subst1(Subst, unsafe_untag_float_dst(X))};
    unsafe_tag_float -> X#unsafe_tag_float
			  {dst=subst1(Subst, unsafe_tag_float_dst(X))};
    conv_to_float -> X#conv_to_float
		       {dst=subst1(Subst, conv_to_float_dst(X))};
    phi -> X#phi{dst = subst1(Subst, X#phi.dst)};
    _ -> X
  end.
%%    'if' -> X;
%%    switch_val -> X;
%%    switch_tuple_arity -> X;
%%    type -> X;
%%    goto -> X;
%%    fail -> X;
%%    enter -> X;
%%    return -> X;
%%    pushcatch -> X;
%%    remove_catch -> X;
%%    comment -> X;
%%    label -> X
%%  end.


subst_list(S,Xs) ->
  [subst1(S,X) || X <- Xs].

subst1([],X) -> X;
subst1([{X,Y}|_],X) -> Y;
subst1([_|Xs],X) -> subst1(Xs,X).

%%
%% The successors of a jump.
%%

successors(Jmp) ->
  case type(Jmp) of
    'if' -> [Jmp#'if'.true_label, Jmp#'if'.false_label];
    goto -> [Jmp#goto.label];
    switch_val -> [Jmp#switch_val.fail_label|
		   lists:map(fun (C) -> element(2,C) end,
			     Jmp#switch_val.cases)];
    switch_tuple_arity -> [Jmp#switch_tuple_arity.fail_label|
			   lists:map(fun (C) -> element(2,C) end,
				     Jmp#switch_tuple_arity.cases)];
    type -> [Jmp#type.true_label, Jmp#type.false_label];
    call -> [Jmp#call.continuation_label| 
	     case Jmp#call.fail_label of [] -> []; L -> [L] end];
    _ -> []
  end.


%%
%% Redirect jumps from label Old to label New.  If the instruction
%% does not jump to Old it is unchanged.
%%

redirect_jmp(Jmp, ToOld, ToNew) ->
  case type(Jmp) of
    'if' ->
      NewJmp = if ToOld =:= Jmp#'if'.true_label -> Jmp#'if'{true_label=ToNew};
		  true -> Jmp
	       end,
      if ToOld =:= NewJmp#'if'.false_label -> NewJmp#'if'{false_label=ToNew};
	 true -> NewJmp
      end;
    switch_val ->
      NewJmp = if ToOld =:= Jmp#switch_val.fail_label -> 
		   Jmp#switch_val{fail_label=ToNew};
		  true -> Jmp
	       end,
      NewJmp#switch_val{cases = 
			lists:map(fun (Pair) ->
				      case Pair of 
					({Val,ToOld}) -> {Val, ToNew};
					(Unchanged) -> Unchanged
				      end
				  end, 
				  NewJmp#switch_val.cases)
		       };
    switch_tuple_arity ->
      NewJmp = if ToOld =:= Jmp#switch_tuple_arity.fail_label -> 
		   Jmp#switch_tuple_arity{fail_label=ToNew};
		  true -> Jmp
	       end,
      NewJmp#switch_tuple_arity{cases = 
				lists:map(fun (Pair) -> 
					      case Pair of
						({Val,ToOld}) -> {Val, ToNew};
						(Unchanged) -> Unchanged
					      end
					  end, 
					  NewJmp#switch_tuple_arity.cases)
			       };


    type ->
      NewJmp = if ToOld =:= Jmp#type.true_label -> Jmp#type{true_label=ToNew};
		  true -> Jmp
	       end,
      if ToOld =:= NewJmp#type.false_label -> NewJmp#type{false_label=ToNew};
	 true -> NewJmp
      end;

    goto ->
      if ToOld =:= Jmp#goto.label -> Jmp#goto{label=ToNew};
	 true -> Jmp
      end;

    call -> 
      NewCont = case Jmp#call.continuation_label of
		  ToOld -> ToNew;
		  OldCont  -> OldCont
		end,
      NewFail = case Jmp#call.fail_label of
		  ToOld -> ToNew;
		  OldFail -> OldFail
		end,
      Jmp#call{continuation_label = NewCont, 
	       fail_label = NewFail};
    pushcatch ->
      case pushcatch_label(Jmp) of
	ToOld -> Jmp#pushcatch{label = ToNew};
	_  -> Jmp
      end;
    _ ->
      Jmp
  end.

%%
%% Is this an unconditional jump (causes a basic block not to have a 
%% fallthrough successor)
%%

is_uncond(I) ->
  case type(I) of
    goto -> true;
    fail -> true;
    enter -> true;
    return -> true;
    call -> 
      case call_fail(I) of
	[] -> 
	  case call_continuation(I) of
	    [] -> false;
	    _ -> true
	  end;
	_ -> true
      end;
    _ -> false
  end.

%%
%% Is this a branch i.e. a (possibly conditional) discontinuation of linear 
%% control flow.
%%

is_branch(I) ->
  case type(I) of
    'if' -> true;
    switch_val -> true;
    switch_tuple_arity -> true;
    type -> true;
    goto -> true;
    fail -> true;
    call -> 
      case call_fail(I) of
	[] -> 
	  case call_continuation(I) of
	    [] -> false;
	    _ -> true
	  end;
	_ -> true
      end;
    enter -> true;
    return -> true;
    _ -> false
  end.

%%
%% Make a new variable
%%

mk_new_var() ->
  mk_var(hipe_gensym:get_next_var(icode)).

%%
%% Make a new label
%%

mk_new_label() ->
  mk_label(hipe_gensym:get_next_label(icode)).

%%
%% Make a bunch of mov operations
%%

mk_movs([], []) ->
  [];
mk_movs([X|Xs], [Y|Ys]) ->
  [mk_mov(X, Y) | mk_movs(Xs, Ys)].

%%
%% Make a series of element operations
%%

mk_elements(_, []) -> 
  [];
mk_elements(Tuple, [X|Xs]) ->
  [mk_primop([X], {unsafe_element, length(Xs)+1}, [Tuple]) | 
   mk_elements(Tuple, Xs)].


%%
%% Convert old icode to new icode.
%%  Compliant with calls ending basic blocks.
%%

preprocess_code(Icode) ->
  Code = icode_code(Icode),
  CompliantCode =  preprocess_code(Code,[]),
  hipe_icode:icode_code_update(Icode,CompliantCode).

preprocess_code([I0|Is0], Acc) ->
  {I,Is} = fallthrough_fixup(I0, Is0),
  preprocess_code(Is, [I | Acc]);
preprocess_code([], Acc) ->
  lists:reverse(Acc).


fallthrough_fixup(I0, Is0 = [I1|_]) ->
  case is_label(I1) of
    false ->
      {I0, Is0};
    true ->
      case is_branch(I0) of
	true ->
	  {I0, Is0};
	false -> % non-branch before label, doubleplusungood
	  %% io:format(standard_io, "~w: fixed unsafe fallthrough <~w,~w>\n", [?MODULE,I0,I1]),
	  {I0, [mk_goto(label_name(I1)) | Is0]}
      end
  end;
fallthrough_fixup(I0, []) ->
  {I0, []}.


%%
%% Remove comments from intermediate code
%%

strip_comments(ICode) ->
  icode_code_update(ICode,strip_comment(icode_code(ICode))).

strip_comment([]) ->
  [];
strip_comment([I|Xs]) ->
  case is_comment(I) of 
    true -> strip_comment(Xs);
    false -> [I|strip_comment(Xs)]
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Is an instruction pure or not
%%

is_pure(I) ->
  case type(I) of
    mov -> true;
    call ->
      is_pure_op(call_fun(I));
    _ -> false
  end.

is_pure_op({unsafe_element, _}) -> true;
is_pure_op(unsafe_hd) -> true;
is_pure_op(unsafe_tl) -> true;
is_pure_op(self) -> true;
is_pure_op({erlang,make_ref,0}) -> true;
is_pure_op({erlang,date,0}) -> true;
is_pure_op({erlang,time,0}) -> true;
is_pure_op({erlang,node,0}) -> true;
is_pure_op({erlang,get,0}) -> true;
is_pure_op({erlang,get,1}) -> true;
is_pure_op(_) -> false.

%%
%% True iff Icode function is a leaf function
%%

is_leaf(Icode) ->
  icode_is_leaf(Icode).


is_leaf0([]) ->
  true;
is_leaf0([I|Is]) ->
  case type(I) of
    call ->
      case hipe_bif:is_bif(call_fun(I)) of
	false -> 
	  false; %% Call to nonbif
	true -> 
	  case call_fun(I) of
	    call_fun -> false; %% Call to closure
	    {erlang, apply, _} -> false; %% MetaCall
	    _ -> is_leaf0(Is) %% Call to bif is ok
	  end
      end;
    enter -> false;
    _ ->
      is_leaf0(Is)
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% PrettyPrinter
%%
%% - changed pp_instr => pp_instrs + pp_instr as in RTL and Sparc
%% - added pp_exit/1 as in RTL + Sparc.

pp(Icode) ->
  pp(standard_io, Icode).

pp(Dev, Icode) ->
  {Mod, Fun, _Arity} = icode_fun(Icode),
  Args =  icode_params(Icode),
  io:format(Dev, "~w:~w(", [Mod, Fun]),
  pp_args(Dev, Args),
  io:format(Dev, ") ->~n", []),
  io:format(Dev, "%% Info:~w\n",
	    [[case icode_is_closure(Icode) of
		true -> 'Closure'; 
		false -> 'Not a closure'
	      end,
	      case icode_is_leaf(Icode) of
		true -> 'Leaf function'; 
		false -> 'Not a leaf function'
	      end |
	      icode_info(Icode)]]),
  pp_instrs(Dev, icode_code(Icode)),
  io:format(Dev, "%% Data:\n", []),
  hipe_data_pp:pp(Dev, icode_data(Icode), icode, "").

pp_instrs(_Dev, []) ->
  ok;
pp_instrs(Dev, [I|Is]) ->
  case catch pp_instr(Dev, I) of
    {'EXIT',_Rsn} ->
      io:format(Dev, '*** ~w ***~n',[I]);
    _ ->
      ok
  end,
  pp_instrs(Dev, Is).

%%%%%%%%%%%%%%%%%%%%

pp_exit(Icode) ->
  pp_exit(standard_io, Icode).

pp_exit(Dev, Icode) ->
  {Mod, Fun, _Arity} = icode_fun(Icode),
  Args =  icode_params(Icode),
  io:format(Dev, "~w:~w(", [Mod, Fun]),
  pp_args(Dev, Args),
  io:format(Dev, ") ->~n", []),
  pp_instrs_exit(Dev, icode_code(Icode)).

pp_instrs_exit(_Dev, []) ->
  ok;
pp_instrs_exit(Dev, [I|Is]) ->
  case catch pp_instr(Dev, I) of
    {'EXIT',_Rsn} ->
      exit({pp,I});
    _ ->
      ok
  end,
  pp_instrs_exit(Dev, Is).

%%%%%%%%%%%%%%%%%%%%

pp_instr(Dev, I) ->
  case type(I) of 
    label ->
      io:format(Dev, "~p: ", [label_name(I)]),
      case  info(I) of
	[] -> io:format(Dev, "~n",[]);
	Info -> io:format(Dev, "~w~n", [Info])
      end;

    comment ->
      io:format(Dev, "    % ~p~n", [comment_text(I)]);

    phi ->
      io:format(Dev, "    ", []),
      pp_arg(Dev, phi_dst(I)),
      io:format(Dev, " := phi(", []),
      pp_args(Dev, phi_args(I)),
      io:format(Dev, ")~n", []);

    mov ->
      io:format(Dev, "    ", []),
      pp_arg(Dev, mov_dst(I)),
      io:format(Dev, " := ", []),
      pp_arg(Dev, mov_src(I)),
      io:format(Dev, "~n", []);

    call ->
      case call_in_guard(I) of
	true ->
	  io:format(Dev, " <G>", []);
	_ ->
	  io:format(Dev, "    ", [])
      end,
      case call_dst(I) of
	[] -> ok;
	Dst ->
	  pp_args(Dev, Dst),
	  io:format(Dev, " := ", [])
      end,
      hipe_icode_primops:pp(call_fun(I), Dev),
      io:format(Dev, "(", []),
      pp_args(Dev, call_args(I)),
      case call_continuation(I) of
	[] ->
	  io:format(Dev, ") (~w)", [call_type(I)]);
	CC ->
	  io:format(Dev, ") (~w) -> ~w",
		    [call_type(I),CC])
      end,

      case call_fail(I) of
	[] ->  io:format(Dev, "~n", []);
	Fail ->  io:format(Dev, ", #fail ~w~n", [Fail])
      end;
    enter ->
      io:format(Dev, "    ", []),
      case enter_fun(I) of
	{Mod, Fun, _Arity} ->
	  io:format(Dev, "~w:~w(", [Mod, Fun]);
	{Fun, _Arity} ->
	  io:format(Dev, "~w(", [Fun]);
	Fun ->
	  io:format(Dev, "~w(", [Fun])
      end,
      pp_args(Dev, enter_args(I)),
      io:format(Dev, ") (~w) ~n", 
		[enter_type(I)]);
    return ->
      io:format(Dev, "    return(", []),
      pp_args(Dev, return_vars(I)),
      io:format(Dev, ")~n", []);
    pushcatch ->
      io:format(Dev, "    pushcatch -> ~w~n", [pushcatch_label(I)]);
    restore_catch ->
      io:format(Dev, "    ", []),
      pp_arg(Dev, restore_catch_dst(I)),
      io:format(Dev, " := restore_catch(~w)~n",
		[restore_catch_label(I)]);
    remove_catch ->
      io:format(Dev, "    remove_catch(~w)~n", 
		[remove_catch_label(I)]);
    fail ->
      Type = case fail_type(I) of
	       fault2 -> fault;
	       T -> T
	     end,
      io:format(Dev, "    fail(~w, [", [Type]),
      pp_args(Dev, fail_reason(I)),
      io:put_chars(Dev, "])\n");
    'if' ->
      io:format(Dev, "    if ~w(", [if_op(I)]),
      pp_args(Dev, if_args(I)),
      io:format(Dev, ") then ~p (~.2f) else ~p~n", 
		[if_true_label(I), if_pred(I), if_false_label(I)]);
    switch_val ->
      io:format(Dev, "    switch_val ",[]),
      pp_arg(Dev, switch_val_arg(I)),
      pp_switch_val_cases(Dev,switch_val_cases(I)),
      io:format(Dev, "    fail -> ~w\n", 
		[switch_val_fail_label(I)]);
    switch_tuple_arity ->
      io:format(Dev, "    switch_tuple_arity ",[]),
      pp_arg(Dev, switch_tuple_arity_arg(I)),
      io:format(Dev, "~w fail-to ~w\n", 
		[switch_tuple_arity_cases(I),
		 switch_tuple_arity_fail_label(I)]);
    type ->
      io:format(Dev, "    if is_", []),
      pp_type(Dev, type_type(I)),
      io:format(Dev, "(", []),
      pp_arg(Dev, type_var(I)),
      io:format(Dev, ") then ~p (~.2f) else ~p~n", 
		[type_true_label(I), type_pred(I), type_false_label(I)]);
    goto ->
      io:format(Dev, "    goto ~p~n", [goto_label(I)]);
    fclearerror ->
      io:format(Dev, "    fclearerror~n", []);
    fmov ->
      io:format(Dev, "    ", []),
      pp_arg(Dev, fmov_dst(I)),
      case fmov_negate(I) of
	true ->
	  io:format(Dev, " f:= -", []);
	false ->
	  io:format(Dev, " f:= ", [])
      end,
      pp_arg(Dev, fmov_src(I)),
      io:format(Dev, "~n", []);
    unsafe_untag_float ->
      io:format(Dev, "    ", []),
      pp_arg(Dev, unsafe_untag_float_dst(I)),
      io:format(Dev, " f:= ", []),
      pp_arg(Dev, unsafe_untag_float_src(I)),
      io:format(Dev, "~n", []);
    unsafe_tag_float ->
      io:format(Dev, "    ", []),
      pp_arg(Dev, unsafe_tag_float_dst(I)),
      io:format(Dev, " f:= ", []),
      pp_arg(Dev, unsafe_tag_float_src(I)),
      io:format(Dev, "~n", []);
    conv_to_float ->
      io:format(Dev, "    ", []),
      pp_arg(Dev, conv_to_float_dst(I)),
      io:format(Dev, " f:= ", []),
      pp_arg(Dev, conv_to_float_src(I)),
      io:format(Dev, "~n", [])
  end.

pp_arg(Dev, {var, V}) when integer(V) ->
  io:format(Dev, "v~p", [V]);
pp_arg(Dev, {var, V}) ->
  io:format(Dev, "~p", [V]);
pp_arg(Dev, {fvar, V}) -> % Added
  io:format(Dev, "fv~p", [V]);
pp_arg(Dev, {reg, V}) -> % Added
  io:format(Dev, "r~p", [V]);
pp_arg(Dev, C) ->
  io:format(Dev, "~p", [const_value(C)]).

pp_args(_Dev, []) -> ok;
pp_args(Dev, [A]) ->
  pp_arg(Dev, A);
pp_args(Dev, [A|Args]) ->
  pp_arg(Dev, A),
  io:format(Dev, ", ", []),
  pp_args(Dev, Args).

pp_type(Dev, T) ->
  io:format(Dev, "~w", [T]).

pp_switch_val_cases(Dev, Cases) ->
  io:format(Dev, " of\n",[]),
  pp_switch_val_cases(Dev, Cases,1),
  io:format(Dev, "",[]).


pp_switch_val_cases(Dev, [{Val,L}], _Pos) -> 
  io:format(Dev, "        ",[]),
  pp_arg(Dev, Val),
  io:format(Dev, " -> ~w\n", [L]);
pp_switch_val_cases(Dev, [{Val, L}|Ls], Pos) -> 
  io:format(Dev, "        ",[]),
  pp_arg(Dev, Val),
  io:format(Dev, " -> ~w;\n", [L]),
  NewPos = Pos,
  %%    case Pos of
  %%      5 -> io:format(Dev, "\n              ",[]),
  %%	   0;
  %%      N -> N + 1
  %%    end,
  pp_switch_val_cases(Dev, Ls, NewPos);
pp_switch_val_cases(_Dev, [], _) -> ok.


%% ---------------------------------------------

highest_var(Code) ->
  highest_var(Code,0).

highest_var([I|Is],Max) ->
  Defs = defines(I),
  Uses = uses(I),
  highest_var(Is,new_max(Defs++Uses,Max));
highest_var([],Max) ->
  Max.

new_max([V|Vs],Max) ->
  case is_var(V) of
    true ->
      VName = var_name(V);
    false ->
      VName = fvar_name(V)
  end,
  if VName > Max ->
      new_max(Vs, VName);
     true ->
      new_max(Vs, Max)
  end;
new_max([],Max) -> Max.

%% ----------------------------------------

highest_label(Code) ->
  highest_label(Code,0).

highest_label([I|Is],Max) ->
  case is_label(I) of 
    true ->
      L = label_name(I),
      NewMax = if L > Max -> L; true -> Max end,
      highest_label(Is,NewMax);
    false ->
      highest_label(Is,Max)
  end;
highest_label([],Max) ->
  Max.
