%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HiPE Intermediate Code
%% ====================================================================
%%  Filename : 	hipe_icode.erl
%%  Module   :	hipe_icode
%%  Purpose  :  Provide primops for the Icode data structure.
%%  History  :	1997-? Erik Johansson (happi@csd.uu.se): Created.
%%              2001-01-30 EJ (happi@csd.uu.se):
%%                             Apply, primop, guardop removed
%%              2003-03-15 ES (happi@acm.org):
%%                             Started commenting in Edoc.
%%                             Moved pretty printer to separate file.
%%
%% $Id$
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%@doc
%% This module implements "Linear Icode" and Icode instructions. 
%%          
%% <p> Icode is a simple (in that it has few instructions) imperative
%% language, used as the first Intermediate Code in the HiPE compiler.
%% Icode is closely related to Erlang, and Icode instructions operate
%% on Erlang terms. </p>
%%
%% <h2><a href="#type-icode">Icode</a></h2>
%%
%% <p> Linear Icode for a function consists of:
%%     <ul>
%%       <li> the function's name (`{M,F,A}'), </li>
%%       <li> a list of parameters, </li>
%%       <li> a list of instructions, </li>
%%       <li> data, </li>
%%       <li> information about whether the function is a leaf function, </li>
%%       <li> information about whether the function is a closure, and </li>
%%       <li> the range for labels and variables in the code. </li>
%%     </ul>
%% </p>
%%
%% <h2><a href="#type-icode_instruction">Icode Instructions</a> (and
%% their components)</h2>
%%
%% Control flow:
%% <dl>
%%    <dt><code><a href="#type-if">'if'</a> 
%%          {Cond::<a href="#type-cond">cond()</a>, 
%%           Args::[<a href="#type-arg">arg()</a>],
%%           TrueLabel::<a href="#type-label_name">label_name()</a>, 
%%           FalseLabel::<a href="#type-label_name">label_name()</a>
%%          } :: 
%%           <a href="#type-icode_instruction">icode_instruction()</a></code></dt>
%%    <dd>
%%        The if instruction compares the arguments (Args) with
%%        condition (Cond) and jumps to either TrueLabel or
%%        FalseLabel. (At the moment...) There are only binary
%%        conditions so the number of arguments should be two.
%%        <p>
%%        An if instructions ends a basic block and should be followed
%%        by a label (or be the last instruction of the code).
%%        </p></dd>
%%
%%    <dt><code><a href="#type-switch_val">switch_val</a> 
%%                    {Arg::<a href="#type-arg">arg()</a>, 
%%                     FailLabel::<a href="#type-label_name">label_name()</a>, 
%%                     Length::integer(), 
%%                     Cases::[{<a href="#type-symbol">symbol()</a>,<a
%%                     href="#type-label_name">label_name()</a>}] %% }::
%%           <a href="#type-icode_instruction">icode_instruction()</a></code></dt>
%%    <dd>
%%        The switch_val instruction compares the argument Arg to the
%%        symbols in the lists Cases, control is transfered to the label
%%        that corresponds to the first symbol that matches.  If no
%%        symbol matches control is transfered to FailLabel.  (NOTE: The
%%        length argument is not currently in use.)
%%        <p>
%%        The switch_val instruction can be assumed to be implemented as
%%        efficiently as possible given the symbols in the case
%%        list. (Jump-table, bianry-serach, or nested ifs)
%%        </p><p>
%%        A switch_val instructions ends a basic block and should be
%%        followed by a label (or be the last instruction of the code).
%%        </p></dd>
%%
%%    <dt><code><a href="#type-switch_tuple_arity">switch_tuple_arity</a>
%%         {
%%          Arg::<a href="#type-arg">arg()</a>, 
%%          FailLabel::<a href="#type-label_name">label_name()</a>, 
%%          Length::integer(),  
%%          Cases::[{integer(),<a href="#type-label_name">label_name()</a>}]
%%        }::
%%           <a href="#type-icode_instruction">icode_instruction()</a></code></dt>
%%    <dd>
%%        The switch_tuple_arity instruction compares the size of the
%%        tuple in the argument Arg to the integers in the lists Cases,
%%        control is transfered to the label that corresponds to the
%%        first integer that matches.  If no integer matches control is
%%        transfered to FailLabel.  (NOTE: The length argument is not
%%        currently in use.)
%%        <p>
%%        The switch_tuple_arity instruction can be assumed to be
%%        implemented as efficently as possible given the symbols in the
%%        case list. (Jump-table, bianry-serach, or nested ifs)
%%        </p><p>
%%        A switch_tuple_arity instructions ends a basic block and
%%        should be followed by a label (or be the last instruction of
%%        the code).
%%        </p></dd>
%%
%%    <dt>`type {typ_expr, arg, true_label, false_label}}'</dt>
%%    <dt>`goto {label}'</dt>
%%    <dt>`label {name}'</dt>
%% </dl>
%%
%% Moves:
%% <dl>
%%    <dt>`move {dst, src}'</dt>
%%    <dt>`fmove {dst, src}'</dt>
%%    <dt>`phi {dst, arglist}'</dt>
%% </dl>
%%
%% Function application:
%% <dl>
%%    <dt>`call {[dst], fun, [arg], type, continuation, fail,
%%               in_guard}'</dt>
%%    <dd>
%%        Where `type' is one of {`local', `remote',
%%        `primop'} and `in_guard' is either `true' or `false'.</dd>
%%    <dt>`enter {fun, [arg], type}'</dt>
%%    <dd>
%%        Where `type' is one of {`local', `remote',
%%        `primop'} and `in_guard' is either
%%        `true' or `false'.</dd>
%%    <dt>`return {[var]}'</dt>
%%    <dd>
%%        <strong>WARNING:</strong> Multiple return values are yet not
%%        fully implemented and tested.
%%    </dd>
%% </dl>
%%
%% Error handling:
%% <dl>
%%    <dt>`begin_try {label, successor}'</dt>
%%    <dt>`end_try'</dt>
%%    <dt>`begin_handler {dstlist}'</dt>
%%    <dt>`fail {Args, Class}'</dt>
%%    <dd>Where `Class' is one of 
%%      {`exit', `throw', `error', `rethrow'}. For `error/2', `[args]'
%%      is `[Reason,Trace]'. For `rethrow', `Args' is
%%      `[Exception,Reason]' - this only occurs in autogenerated code.
%%    </dd>
%% </dl>
%%
%% Comments:
%% <dl>
%%    <dt>`comment{Text::string()}'</dt>
%% </dl>
%%
%% <h4>Notes</h4>
%%
%%  <p> A constant can only show up on the RHS of a `move' instruction
%%      and in `if' and `switch_*'</p>
%%  <p>
%%      Classification of primops should be like this:
%%      <ul>
%%      <li> `erlang:exit/1, erlang:throw/1, erlang:error/1,
%%           erlang:error/2, erlang:fault/1',
%%           and `erlang:fault/2' should use the
%%           {@link fail(). fail-instruction} in Icode.</li>
%%      <li> Calls or tail-recursive calls to BIFs, operators, or internal
%%           functions should be implemented with `call' or `enter' 
%%           respectively, with the primop flag set.</li>
%%      <li> All other Erlang functions should be implemented with `call'
%%           or `enter' respectively, without the primop flag set.</li>
%%      </ul>
%%  </p>
%%
%% <h4>Primops</h4>
%%
%% <pre>
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
%%    {closure_element, N} - [Fun], N:integer
%%
%%  Arithmetic:       [Arg1, Arg2]
%%    '+','-','*','/','div','rem',
%%    'band','bor','bxor','bnot'
%%    'bsl','bsr',
%%
%%  Receive:         
%%    check_get_msg - []
%%    next_msg      - []
%%    select_msg    - []
%%    set_timeout   - [Timeout]
%%    clear_timeout - []	 %% stupid name - only resets message pointer
%%    suspend_msg   - []
%%
%% </pre>
%%
%% <h4>Guardops: (primops that can be used in guards and can fail)</h4>
%%  <pre>
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
%% </pre>
%%
%%
%% <h4>Relational Operations (Cond in if instruction)</h4>
%% <pre>
%%    gt, lt, geq, leq,
%%    eqeq, neq, exact_eqeq, exact_neq
%% </pre>
%%
%% <h4>Type tests</h4>
%% <pre>
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
%% </pre>
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%=====================================================================

-module(hipe_icode).
-include("../main/hipe.hrl").
-include("hipe_icode.hrl").

%% @type icode(Fun, Params, IsClosure, IsLeaf, Code, Data, VarRange,LabelRange)
%%           Fun = mfa()
%%           Params = [var()]
%%           IsClosure = bool()
%%           IsLeaf = bool()
%%           Code = [icode_instruction()]
%%           Data = data()
%%           VarRange = {integer(),integer()}
%%           LabelRange = {integer(),integer()}
%%
%% @type icode_instruction(I) 
%%   I = if() | switch_val() | switch_tuple_arity() | type() | goto() | label()
%%        | move() | fmove() | phi() | call() | enter() | return() 
%%        | begin_try() | end_try() | begin_handler() | fail() | comment()
%%
%% @type if(Cond, Args, TrueLabel, FalseLabel)
%%    Cond = cond()
%%    Args = [arg()]
%%    TrueLabel = label_name()
%%    FalseLabel = label_name()
%%
%% @type switch_val(Arg, FailLabel, Length, Cases) 
%%    Arg = arg()
%%    FailLabel=label_name()
%%    Length = integer()
%%    Cases = [{symbol(),label_name()}]
%%
%% @type switch_tuple_arity(Arg, FailLabel, Length, Cases)
%%    Arg = arg()
%%    FailLabel = label_name()
%%    Length = integer()
%%    Cases = [{symbol(), label_name()}]
%%
%% @type type(TypeExpr, Arg, True_label, False_label)
%%    TypeExpr = type_typr()
%%    Args = [arg()]
%%    TrueLabel = label_name()
%%    FalseLabel = label_name()
%%
%% @type goto(Label) Label = label_name()
%%
%% @type label(Name) Name=label_name()
%%
%% @type move(Dst, Src) Dst = var() Src = arg()
%%
%% @type fmove(Dst, Src) Dst = fvar() Src = farg()
%%
%% @type phi(Dst, Id, Arglist) 
%%           Dst = var()|fvar()
%%           Id = var()|fvar()
%%           Arglist=[{Pred, Src}]
%%           Pred = label_name()
%%           Var = var | fvar() 
%%
%% @type call(Dst, Fun, Arg, Type, Continuation, InGuard)
%%                   Dst = [var()]
%%                   Fun = mfa() | primop() | closure() 
%%                   Arg = [var()]
%%                   Type = call_type()
%%                   Continuation = [] | label_name()
%%                   Fail = []  | label_name()
%%                   InGuard = bool()
%%
%% @type enter(Fun, Arg, Type)
%%                   Fun = mfa() | primop() | closure() 
%%                   Arg = [var()] 
%%                   Type = call_type()
%%
%% @type return (Vars) Vars = [var()]
%%
%% @type begin_try(Fail, Successor) 
%%           Fail = label_name() Successor = label_name()
%%
%% @type end_try()
%%
%% @type begin_handler(Dst) 
%%           Dst = [var()]
%%
%% @type fail(Args,Class,Label)
%%    Args = [var()]
%%    Class = exit_class()
%%    Label = label_name()
%%
%% @type comment(Text) Text = string()

%% @type call_type()  = 'local' | 'remote' | 'primop'
%% @type exit_class() = 'exit' | 'throw' | 'error' | 'rethrow'
%% @type cond() = gt | lt | geq | leq | eqeq | neq | exact_eqeq | exact_neq
%% @type type_type() = 
%%      list
%%    | nil
%%    | cons
%%    | tuple
%%    | {tuple, integer()}
%%    | atom
%%    | {atom, atom()}
%%    | constant
%%    | number
%%    | integer
%%    | {integer, integer()}
%%    | fixnum
%%    | bignum
%%    | float
%%    | pid
%%    | port
%%    | reference
%%    | binary
%%    | function
%%
%% @type mfa(Mod,Fun,Arity) = {atom(),atom(),integer()}

%% @type arg() = var() | const()
%% @type farg() = fvar() | float()
%% @type var(Name) Name=integer()
%% @type fvar(Name) Name=integer()
%% @type label_name(Name) Name = integer()
%% @type symbol(S) = atom() | number()
%% @type const(C)  C = const_fun() | immediate()
%% @type const_fun(MFA,U,I,Args) = {MFA,U,I,Args}
%%    MFA = mfa()
%%    U = integer()
%%    I = integer()
%%    Args = [var()]
%% @type immediate(I) = I
%%    I = term()
%% @end


%% ____________________________________________________________________
%%
%% Exports
%%
-export([mk_icode/7, %% mk_icode(Fun, Params, IsClosure, IsLeaf, 
		     %%          Code, VarRange, LabelRange)
	 mk_icode/8, %% mk_icode(Fun, Params, IsClosure, IsLeaf, 
		     %%          Code, Data, VarRange, LabelRange)
	 mk_typed_icode/8,
	 icode_fun/1,
	 icode_params/1,
	 icode_params_update/2,
	 icode_is_closure/1,
	 icode_is_leaf/1,
	 icode_code/1,
	 icode_code_update/2,
	 icode_data/1,
	 %% icode_data_update/2,
	 icode_var_range/1,
	 icode_label_range/1,
	 icode_info/1,
	 icode_info_update/2]).

-export([mk_if/4,           %% mk_if(Op, Args, TrueLbl, FalseLbl)
	 %% mk_if/5,	    %% mk_if(Op, Args, TrueLbl, FalseLbl, Prob)
	 if_op/1,
	 if_true_label/1,
	 if_false_label/1,
	 if_args/1,
	 if_pred/1,
	 %% is_if/1,
	 
	 mk_switch_val/4,
	 %% mk_switch_val/5,
	 switch_val_arg/1,
	 switch_val_fail_label/1,
	 %% switch_val_length/1,
	 switch_val_cases/1,
	 switch_val_cases_update/2,
	 %% is_switch_val/1,
	 
	 mk_switch_tuple_arity/4,
	 %% mk_switch_tuple_arityl/5,
	 switch_tuple_arity_arg/1,
	 switch_tuple_arity_fail_label/1,
	 switch_tuple_arity_fail_label_update/2,
	 %% switch_tuple_arity_length/1,
	 switch_tuple_arity_cases/1,
	 switch_tuple_arity_cases_update/2,
	 %% is_switch_tuple_arity/1,

	 mk_type/4,        %% mk_type(Args, Type, TrueLbl, FalseLbl)
	 mk_type/5,	   %% mk_type(Args, Type, TrueLbl, FalseLbl, P)
	 type_args/1,
	 %% type_args_update/2,
	 type_type/1,
	 type_true_label/1,
	 type_false_label/1,
	 type_pred/1,
	 %% is_type/1,

	 mk_guardop/5,     %% mk_guardop(Dst, Fun, Args, Continuation, Fail)
	 mk_primop/3,      %% mk_primop(Dst, Fun, Args)
	 mk_primop/5,      %% mk_primop(Dst, Fun, Args, Cont, Fail)
	 mk_typed_call/6,  %% mk_call(Dst, Mod, Fun, Args, Type, DstType)
	 mk_call/5,	   %% mk_call(Dst, Mod, Fun, Args, Type)
	 %% mk_call/7,	   %% mk_call(Dst, Mod, Fun, Args, Type,
	                   %%         Continuation, Fail)
	 mk_call/8,	   %% mk_call(Dst, Mod, Fun, Args, Type,
	                   %%         Continuation, Fail, Guard)
	 call_dstlist/1,
	 call_dstlist_update/2,
	 call_dst_type/1,
	 call_args/1,
	 call_args_update/2,
	 call_fun/1,
	 call_fun_update/2,
	 call_type/1,
	 call_continuation/1,
	 call_fail_label/1,
	 call_set_fail_label/2,
	 call_set_continuation/2,
	 is_call/1, 
	 call_in_guard/1,

	 mk_goto/1,              %% mk_goto(Lbl)
	 goto_label/1,
	 
	 mk_enter/4,             %% mk_enter(Mod, Fun, Args, Type)
	 mk_enter_primop/2,      %% mk_enter_primop(Op, Type)
	 enter_fun/1,
	 enter_fun_update/2,
	 enter_args/1,
	 enter_args_update/2,
	 enter_type/1,
	 %% is_enter/1,
	 
	 mk_fmove/2,             %% mk_fmove(Dst, Src)

	 mk_return/1,            %% mk_return(Vars)
	 %% mk_fail/1,	         %% mk_fail(Args) class = exit
	 mk_fail/2,              %% mk_fail(Args, Class)
	 %% mk_fail/3,           %% mk_fail(Args, Class, Label)
	 mk_move/2,              %% mk_move(Dst, Src)
	 mk_moves/2,             %% mk_moves(DstList, SrcList)
	 mk_begin_try/2,         %% mk_begin_try(Label, Successor)
	 mk_begin_handler/1,     %% mk_begin_handler(ReasonDst)
	 mk_end_try/0,           %% mk_end_try()
	 %% mk_elements/2,       %% mk_elements(Tuple, Vars)
	 mk_label/1,             %% mk_label(Name)
	 mk_new_label/0,         %% mk_new_label()
	 mk_comment/1,           %% mk_comment(Text)
	 mk_const/1,             %% mk_const(Const)
	 %% mk_const_fun/4,	 %% mk_const_fun(MFA,U,I,Args)
	 mk_var/1,               %% mk_var(Id)
	 annotate_var/2,         %% annotate_var(Var, Type)
	 unannotate_var/1,       %% unannotate_var(Var)
	 mk_reg/1,               %% mk_reg(Id)
	 mk_fvar/1,              %% mk_fvar(Id)
	 mk_new_var/0,           %% mk_new_var()
	 mk_new_fvar/0,          %% mk_new_fvar()
	 mk_new_reg/0,           %% mk_new_reg()
	 mk_phi/1,               %% mk_phi(Id)
	 mk_phi/2                %% mk_phi(Id, ArgList)
	]).

%%
%% Identifiers
%%

-export([type/1,
	 %% is_fail/1,
	 %% is_return/1,
	 is_move/1,
	 %% is_begin_try/1,
	 is_begin_handler/1,
	 %% is_end_try/1,
	 is_goto/1,
	 is_label/1,
	 is_comment/1,
	 is_const/1,
	 is_const_fun/1,
	 is_var/1,
	 is_annotated_var/1,
	 is_fvar/1,
	 is_reg/1,
	 is_var_or_fvar_or_reg/1,
	 %% is_uncond/1,
	 %% is_fmove/1,
         is_phi/1]).

%%
%% Selectors
%%

-export([phi_dst/1,
         phi_id/1,
         %% phi_args/1,
         phi_arg/2,
         phi_arglist/1,
	 phi_enter_pred/3,
	 phi_remove_pred/2,
	 phi_redirect_pred/3,
	 move_dst/1,
	 move_src/1,
	 move_src_update/2,
	 begin_try_label/1,
	 begin_try_successor/1,
	 begin_handler_dstlist/1,
	 label_name/1,
	 comment_text/1,
	 return_vars/1,
	 fail_args/1,
	 fail_class/1,
	 fail_label/1,
	 fail_set_label/2,
	 %% var_name/1,
	 var_annotation/1,
	 %% fvar_name/1,
	 %% reg_name/1,		 
	 const_value/1,
	 %% info/1,
	 fmove_dst/1,
	 fmove_src/1
	 %% fmove_src_update/2
	]).

%%
%% Misc
%%

-export([args/1,
	 uses/1,
	 defines/1,
	 is_safe/1,
	 strip_comments/1,
	 subst/2,
	 subst_uses/2,
	 subst_defines/2,
	 redirect_jmp/3,
	 successors/1,
	 fails_to/1,
	 is_branch/1,
	 is_leaf_code/1 % needs to be used in beam_to_icode
	]).

-export([highest_var/1,highest_label/1]).

%%---------------------------------------------------------------------
%% 
%% Icode
%%
%%---------------------------------------------------------------------

%% @spec mk_icode(Fun::mfa(), Params::[var()], Closure::bool(), 
%%                Leaf::bool(), Code::[icode_instruction()],
%%                VarRange::{integer(),integer()}, 
%%                LabelRange::{integer(),integer()}) -> icode()
%%
mk_icode(Fun, Params, Closure, Leaf, Code, VarRange, LabelRange) ->
  #icode{'fun'=Fun, params=Params, code=Code,
	 closure=Closure,
	 leaf=Leaf,
	 data=hipe_consttab:new(),
	 var_range=VarRange,
	 label_range=LabelRange}.
%% @spec mk_icode(Fun::mfa(), Params::[var()], Closure::bool(), Leaf::bool(), 
%%                Code::[icode_instruction()],  Data::data(),
%%                VarRange::{integer(),integer()}, 
%%                LabelRange::{integer(),integer()}) -> icode()
%%
mk_icode(Fun, Params, Closure, Leaf, Code, Data, VarRange, LabelRange) ->
  #icode{'fun'=Fun, params=Params, code=Code,
	 data=Data, closure=Closure, leaf=Leaf,
	 var_range=VarRange,
	 label_range=LabelRange}.
mk_typed_icode(Fun, Params, Closure, Leaf, Code, VarRange, 
	       LabelRange, ArgType) ->
  #icode{'fun'=Fun, 
	 params=Params,
	 code=Code,
	 closure=Closure,
	 leaf=Leaf,
	 data=hipe_consttab:new(),
	 var_range=VarRange,
	 label_range=LabelRange,
	 info=[{arg_type, ArgType}]}.
%% @spec icode_fun(I::icode()) -> mfa()
icode_fun(#icode{'fun'=MFA}) -> MFA.
%% @spec icode_params(I::icode()) -> [var()]
icode_params(#icode{params=Params}) -> Params.
%% @spec icode_params_update(I::icode(),[var()]) -> icode()
icode_params_update(Icode, Params) -> 
  Icode#icode{params=Params}.
%% @spec icode_is_closure(I::icode()) -> bool()
icode_is_closure(#icode{closure=Closure}) -> Closure.
%% @spec icode_is_leaf(I::icode()) -> bool()
icode_is_leaf(#icode{leaf=Leaf}) -> Leaf.
%% @spec icode_code(I::icode()) -> [icode_instruction()]
icode_code(#icode{code=Code}) -> Code.
%% @spec icode_code_update(I::icode(), [icode_instruction()]) -> icode()
icode_code_update(Icode, NewCode) -> 
  Vmax = highest_var(NewCode),
  Lmax = highest_label(NewCode),
  Icode#icode{code=NewCode, var_range={0,Vmax}, label_range={0,Lmax}}.
%% @spec icode_data(I::icode()) -> data()
icode_data(#icode{data=Data}) -> Data.
%% %% @spec icode_data_update(I::icode(),data()) -> icode()
%% icode_data_update(Icode, NewData) -> Icode#icode{data=NewData}.
icode_var_range(#icode{var_range=VarRange}) -> VarRange.
icode_label_range(#icode{label_range=LabelRange}) -> LabelRange.
icode_info(#icode{info=Info}) -> Info.
icode_info_update(Icode, Info) -> Icode#icode{info=Info}.

%% ____________________________________________________________________
%% Instructions
%%

%%
%% if
%%

mk_if(Op, Args, TrueLbl, FalseLbl) ->
  #'if'{op=Op, args=Args, true_label=TrueLbl, false_label=FalseLbl, p=0.5}.
%% mk_if(Op, Args, TrueLbl, FalseLbl, P) ->
%%   #'if'{op=Op, args=Args, true_label=TrueLbl, false_label=FalseLbl, p=P}.
if_op(#'if'{op=Op}) -> Op.
if_args(#'if'{args=Args}) -> Args.
if_true_label(#'if'{true_label=TrueLbl}) -> TrueLbl.
if_true_label_update(IF, TrueLbl) ->
  IF#'if'{true_label=TrueLbl}.
if_false_label(#'if'{false_label=FalseLbl}) -> FalseLbl.
if_false_label_update(IF, FalseLbl) ->
  IF#'if'{false_label=FalseLbl}.
if_pred(#'if'{p=P}) -> P.

%%
%% switch_val
%%

mk_switch_val(Arg, FailLbl, Length, Cases) ->
  #switch_val{arg=Arg, fail_label=FailLbl, length=Length, cases=Cases}.
switch_val_arg(#switch_val{arg=Arg}) -> Arg.
switch_val_fail_label(#switch_val{fail_label=FailLbl}) -> FailLbl.
switch_val_fail_label_update(SV, FailLbl) ->
  SV#switch_val{fail_label=FailLbl}.
%% switch_val_length(#switch_val{length=Length}) -> Length.
switch_val_cases(#switch_val{cases=Cases}) -> Cases.
switch_val_cases_update(SV, NewCases) -> 
  SV#switch_val{cases = NewCases}.

%%
%% switch_tuple_arity
%%

mk_switch_tuple_arity(Arg, FailLbl, Length, Cases) ->
  #switch_tuple_arity{arg=Arg, fail_label=FailLbl, length=Length, cases=Cases}.
switch_tuple_arity_arg(#switch_tuple_arity{arg=Arg}) -> Arg.
switch_tuple_arity_fail_label(#switch_tuple_arity{fail_label=FailLbl}) -> 
  FailLbl.
switch_tuple_arity_fail_label_update(S, FailLbl) -> 
  S#switch_tuple_arity{fail_label=FailLbl}.
%% switch_tuple_arity_length(#switch_tuple_arity{length=Length}) -> Length.
switch_tuple_arity_cases(#switch_tuple_arity{cases=Cases}) -> Cases.
switch_tuple_arity_cases_update(Cond, NewCases) -> 
  Cond#switch_tuple_arity{cases = NewCases}.

%%
%% type
%%

mk_type(X, Type, TrueLbl, FalseLbl) -> 
  #type{type=Type, args=X, true_label=TrueLbl, false_label=FalseLbl, p=0.5}.
mk_type(X, Type, TrueLbl, FalseLbl, P) -> 
  #type{type=Type, args=X, true_label=TrueLbl, false_label=FalseLbl, p=P}.
type_type(#type{type=Type}) -> Type.
type_args(#type{args=Args}) -> Args.
%% type_args_update(T, Args) -> T#type{args=Args}.
type_true_label(#type{true_label=TrueLbl}) -> TrueLbl.
type_false_label(#type{false_label=FalseLbl}) -> FalseLbl.
type_pred(#type{p=P}) -> P.

%%
%% goto
%%

mk_goto(Lbl) -> #goto{label=Lbl}.
goto_label(#goto{label=Lbl}) -> Lbl.
is_goto(#goto{}) -> true;
is_goto(_) -> false.

%%
%% return
%%

mk_return(Vars) -> #return{vars=Vars}.
return_vars(#return{vars=Vars}) -> Vars.

%%
%% fail
%%

%% mk_fail(Args) when list(Args) -> #fail{class=error, args=Args}.
mk_fail(Args, Class) when list(Args) ->
  case Class of
    error -> ok;
    exit -> ok;
    rethrow -> ok;
    throw -> ok;
    _ -> exit({bad_fail_class, Class})
  end,
  #fail{class=Class, args=Args}.
%% mk_fail(Args, Class, Label) when list(Args) ->
%%   #fail{class=Class, args=Args, fail_label=Label}.
fail_class(#fail{class=Class}) -> Class.
fail_args(#fail{args=Args}) -> Args.
fail_label(#fail{fail_label=Label}) -> Label.
fail_set_label(I=#fail{}, Label) ->
  I#fail{fail_label = Label}.


%%
%% move
%%

mk_move(Dst, Src) -> #move{dst=Dst, src=Src}.
move_dst(#move{dst=Dst}) -> Dst.
move_src(#move{src=Src}) -> Src.
move_src_update(M, NewSrc) -> M#move{src=NewSrc}.
is_move(#move{}) -> true;
is_move(_) -> false.

%%
%% phi
%%

%% The id field is not entirely redundant. It is used in mappings
%% in the SSA pass since the dst field can change.
mk_phi(Var) -> #phi{dst = Var, id = Var, arglist = []}.
mk_phi(Var, ArgList) -> #phi{dst = Var, id = Var, arglist = ArgList}.
phi_dst(#phi{dst=Dst}) -> Dst.
phi_id(#phi{id=Id}) -> Id.
phi_arglist(#phi{arglist=ArgList}) -> ArgList.
phi_args(P) -> [X || {_,X} <- phi_arglist(P)].
phi_arg(P, Pred) -> 
  case lists:keysearch(Pred, 1, phi_arglist(P)) of
    false -> exit('No such predecessor to phi', {Pred, P});
    {value, {_, Var}} -> Var
  end.
is_phi(#phi{}) -> true;
is_phi(_) -> false.
phi_enter_pred(P, Pred, Var) ->
  P#phi{arglist=[{Pred,Var}|lists:keydelete(Pred, 1, phi_arglist(P))]}.
phi_remove_pred(P, Pred) ->
  P#phi{arglist=lists:keydelete(Pred, 1, phi_arglist(P))}.
phi_argvar_subst(P, Subst) -> 
  NewArgList = [{Pred, subst1(Subst, Var)} || {Pred,Var} <- phi_arglist(P)],
  P#phi{arglist=NewArgList}.
phi_redirect_pred(P, OldPred, NewPred)->
  Subst = [{OldPred, NewPred}],
  NewArgList = [{subst1(Subst, Pred), Var} || {Pred,Var} <- phi_arglist(P)],
  P#phi{arglist=NewArgList}.

%%
%% primop and guardop
%%
%% Whether a function is a "primop" - i.e., an internal thing - or not,
%% is really only shown by its name.  An {M,F,A} always represents a
%% function in some Erlang module (althought it might be a BIF, and
%% could possibly be inline expanded).  It is convenient to let the
%% constructor functions check the name and set the type automatically,
%% especially for guardops - some guardops are primitives and some are
%% MFA:s, and this way we won't have to rewrite all calls to mk_guardop
%% to flag whether they are primops or not.

mk_primop(DstList, Fun, ArgList) ->
  mk_primop(DstList, Fun, ArgList, [], []).
mk_primop(DstList, Fun, ArgList, Continuation, Fail) ->
  mk_primop(DstList, Fun, ArgList, Continuation, Fail, false).
mk_primop(DstList, Fun, ArgList, Continuation, Fail, Guard) ->
  Type = find_call_type(Fun),
  make_call(DstList, Fun, ArgList, Type, Continuation, Fail, Guard).

%% Note that a 'guardop' is just a primop that occurred in a guard.

mk_guardop(DstList, Fun, ArgList, True, False) ->
  mk_primop(DstList, Fun, ArgList, True, False, true).

find_call_type(Fun) ->
  case Fun of
    {M,F,A} when atom(M), atom(F), integer(A) -> remote;
    {_,_,_} -> exit({bad_primop, Fun});
    _ -> primop
  end.

%%
%% call
%%
mk_typed_call(Dst, M, F, Args, Type, DstType) ->
  Call = mk_call(Dst, M, F, Args, Type),
  Call#call{dst_type=DstType}.

mk_call(DstList, M, F, ArgList, Type) ->
  mk_call(DstList, M, F, ArgList, Type, [], [], false).
%% mk_call(DstList, M, F, ArgList, Type, Continuation, Fail) ->
%%   mk_call(DstList, M, F, ArgList, Type, Continuation, Fail, false).
mk_call(DstList, M, F, ArgList, Type, Continuation, Fail, Guard)
  when atom(M), atom(F) ->
  Fun = {M,F,length(ArgList)},
  make_call(DstList, Fun, ArgList, Type, Continuation, Fail, Guard).

%% The common constructor for all calls
%%
%% Note: If the "guard" flag is `true', it means that if the call fails,
%% we can simply jump to the Fail label (if it exists) without
%% generating any additional exception information - it isn't needed.
%%
make_call(DstList, Fun, ArgList, Type, Continuation, Fail, InGuard) ->
  case Type of
    local -> ok;
    remote -> ok;
    primop -> ok;
    _ -> exit({bad_call_type, Type})
  end,
  #call{dstlist=DstList, 'fun'=Fun, args=ArgList,
	type=Type, continuation=Continuation, fail_label=Fail,
	in_guard=InGuard}.
call_dstlist(#call{dstlist=DstList}) -> DstList.
call_dstlist_update(C,Dest) -> C#call{dstlist=Dest}.
call_type(#call{type=Type}) -> Type.
call_dst_type(#call{dst_type=DstType}) -> DstType.
%% @spec (C::call()) -> [arg()]
call_args(#call{args=Args}) -> Args.
%% @spec (C::call(), [arg()]) -> call()
call_args_update(C,Args) -> C#call{args=Args}.
call_fun(#call{'fun'=Fun}) -> Fun.
%% Note that updating the name field requires recomputing the call type,
%% in case it changes from a remote/local call to a primop call.
call_fun_update(C, Fun) ->
  Type = case find_call_type(Fun) of
	   primop -> primop;
	   _ -> call_type(C)
	 end,
  C#call{'fun'=Fun, type=Type}.
call_continuation(#call{continuation=Continuation}) -> Continuation.
call_fail_label(#call{fail_label=Fail}) -> Fail.
call_set_continuation(I, Continuation) ->
  I#call{continuation = Continuation}.
call_set_fail_label(I=#call{}, Fail) ->
  case Fail of
    [] ->
      I#call{fail_label = Fail, in_guard=false};
    _  ->
      I#call{fail_label = Fail}
  end.
is_call(#call{}) -> true;
is_call(_) -> false.
call_in_guard(#call{in_guard=InGuard}) -> InGuard.


%%
%% enter
%%

mk_enter(M, F, Args, Type) when atom(M), atom(F) ->
  case Type of
    local -> ok;
    remote -> ok;
    primop -> ok;
    _ -> exit({bad_enter_type, Type})
  end,
  #enter{'fun'={M,F,length(Args)}, args=Args, type=Type}.
enter_fun(#enter{'fun'=Fun}) -> Fun.
enter_fun_update(E, Fun) ->
  Type = case find_call_type(Fun) of
	   primop -> primop;
	   _ -> enter_type(E)
	 end,
  E#enter{'fun'=Fun, type=Type}.
enter_args(#enter{args=Args}) -> Args.
enter_args_update(E, Args) -> E#enter{args=Args}.
enter_type(#enter{type=Type}) -> Type.

mk_enter_primop(Op, Args) ->
  #enter{'fun'=Op, args=Args, type=primop}.

%%
%% begin_try
%%

%% The reason that begin_try is a branch instruction is just so that it
%% keeps the fail-to block linked into the cfg, until the exception
%% handling instructions are eliminated.

mk_begin_try(Label, Successor) ->
  #begin_try{label=Label, successor=Successor}.
begin_try_label(#begin_try{label=Label}) -> Label.
begin_try_successor(#begin_try{successor=Successor}) -> Successor.

%%
%% end_try
%%

mk_end_try() -> #end_try{}.

%%
%% begin_handler
%%

mk_begin_handler(Dstlist) ->
  #begin_handler{dstlist=Dstlist}.
begin_handler_dstlist(#begin_handler{dstlist=Dstlist}) -> Dstlist.
is_begin_handler(#begin_handler{}) -> true;
is_begin_handler(_) -> false.

%%
%% label
%%

mk_label(Name) -> #label{name=Name}.
label_name(#label{name=Name}) -> Name.
is_label(#label{}) ->true;
is_label(_) -> false.

%%
%% comment
%%

%% @spec mk_comment(Txt::term()) -> comment()
%% @doc If `Txt' is a list of characters (possibly deep), it will be
%% printed as a string; otherwise, `Txt' will be printed as a term.
mk_comment(Txt) -> #comment{text=Txt}.
comment_text(#comment{text=Txt}) -> Txt.
%% @spec is_comment(Instr::icode_instruction()) -> bool()
%% @doc  True if this is the Icode instruction for comments.
is_comment(#comment{}) -> true;
is_comment(_) -> false.

%% ____________________________________________________________________
%% 

%%
%% Arguments (variables and constants)
%%

mk_const(C) -> {const,{flat,C}}.
%% mk_const_fun(MFA,U,I,Args) -> {const,{const_fun,{MFA,U,I,Args}}}.
const_value({const,{flat,X}}) -> X;
const_value({const,{const_fun,X}}) -> X.
%% @spec is_const(icode_arg()) -> bool()
is_const({const,_}) -> true;
is_const(_) -> false.
%% @spec is_const_fun(icode_arg()) -> bool()
is_const_fun({const,{const_fun,_}}) -> true;
is_const_fun(_) -> false.

mk_var(V) -> {var,V}.
var_name({var,Name}) -> Name.
%% @spec is_var(icode_arg()) -> bool()
is_var({var,_}) -> true;
is_var(_) -> false.
  
%% This representation of a variable is used in the constraint based
%% type pass and when pretty printing typed icode.
annotate_var({var, Name}, Type)-> {var, Name, Type};
annotate_var({var, Name, _OldType}, Type) -> {var, Name, Type}.
is_annotated_var({var, _Name, _Type})-> true;
is_annotated_var(_) -> false.
var_annotation({var, _Name, Type})-> Type.
unannotate_var({var, Name, _}) -> {var, Name}.

 
mk_reg(V) -> {reg,V}.
reg_name({reg,Name}) -> Name.
%% @spec is_reg(icode_arg()) -> bool()
is_reg({reg,_}) -> true;
is_reg(_) -> false.

%% @spec is_var_or_fvar_or_reg(icode_arg()) -> bool()
is_var_or_fvar_or_reg({var,_}) -> true;
is_var_or_fvar_or_reg({fvar,_}) -> true;
is_var_or_fvar_or_reg({reg,_}) -> true;
is_var_or_fvar_or_reg(_) -> false.

mk_fvar(V) -> {fvar,V}.
fvar_name({fvar,Name}) -> Name.
%% @spec is_fvar(icode_arg()) -> bool()
is_fvar({fvar,_}) -> true;
is_fvar(_) -> false.

%%
%% Floating point Icode instructions.
%%

%%
%% fmove
%%

mk_fmove(Dst, Src) -> #fmove{dst=Dst, src=Src}.
fmove_dst(#fmove{dst=Dst}) -> Dst.
fmove_src(#fmove{src=Src}) -> Src.
%% fmove_src_update(M, NewSrc) -> M#fmove{src=NewSrc}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Liveness info 
%%

%% @spec uses(icode_instruction()) -> [icode_arg()]
uses(Instr) ->
  remove_constants(args(Instr)).

%% @spec args(I::icode_instruction()) -> [var()]
args(I) ->
  case type(I) of
    'if' -> if_args(I);
    switch_val -> [switch_val_arg(I)];
    switch_tuple_arity -> [switch_tuple_arity_arg(I)];
    type -> type_args(I);
    move -> [move_src(I)];
    fail -> fail_args(I);
    call -> call_args(I);
    enter -> enter_args(I);
    return -> return_vars(I);
    fmove -> [fmove_src(I)];
    phi -> phi_args(I);
    %%    goto -> [];
    %%    begin_try -> [];
    %%    begin_handler -> [];
    %%    end_try -> [];
    %%    comment -> [];
    %%    label -> []
    _ -> []
  end.

defines(I) ->
  case type(I) of
    move -> remove_constants([move_dst(I)]);
    fmove -> remove_constants([fmove_dst(I)]);
    call -> remove_constants(call_dstlist(I));
    begin_handler -> remove_constants(begin_handler_dstlist(I));
    phi -> remove_constants([phi_dst(I)]);
    %%    'if' -> [];
    %%    switch_val -> [];
    %%    switch_tuple_arity -> [];
    %%    type -> [];
    %%    goto -> [];
    %%    fail -> [];
    %%    enter -> [];
    %%    return -> [];
    %%    begin_try -> [];
    %%    end_try -> [];
    %%    comment -> [];
    %%    label -> []
    _ -> []
  end.


remove_constants([]) ->
  [];
remove_constants([{const,_}|Xs]) ->
  remove_constants(Xs);
remove_constants([{reg,Var}|Xs]) ->
  [{reg,Var} | remove_constants(Xs)];
remove_constants([{var,Var}|Xs]) ->
  [{var,Var} | remove_constants(Xs)];
remove_constants([{var,Var, Ann}|Xs]) ->
  [{var,Var, Ann} | remove_constants(Xs)];
remove_constants([{fvar,Var}|Xs]) ->
  [{fvar,Var} | remove_constants(Xs)];
remove_constants([{colored,Var}|Xs]) ->
  [{colored,Var} | remove_constants(Xs)];
remove_constants([_|Xs]) ->
  remove_constants(Xs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Utilities
%%

%% @doc 
%%    Returns the type of the Icode instruction X. Hopefully, this can
%%    be compiled better than repeated calls to hipe_icode:is_branch/is_move/..
%% @end

type(I) when tuple(I) -> element(1, I);
type(O) -> ?EXIT({bad_icode_type,O}).

%%
%% Substitution: replace occurrences of X by Y if {X,Y} is in the
%%   Subst_list.

subst(Subst, X) ->
  subst_defines(Subst, subst_uses(Subst,X)).

subst_uses(Subst, X) ->
  case type(X) of
    'if' -> X#'if'{args = subst_list(Subst, if_args(X))};
    switch_val -> X#switch_val{arg = subst1(Subst, switch_val_arg(X))};
    switch_tuple_arity ->
      X#switch_tuple_arity{arg = subst1(Subst, switch_tuple_arity_arg(X))};
    type -> X#type{args = subst_list(Subst, type_args(X))};
    move -> X#move{src = subst1(Subst, move_src(X))};
    fail -> X#fail{args = subst_list(Subst, fail_args(X))};
    call -> X#call{args = subst_list(Subst, call_args(X))};
    enter -> X#enter{args = subst_list(Subst, enter_args(X))};
    return -> X#return{vars = subst_list(Subst, return_vars(X))};
    fmove -> X#fmove{src = subst1(Subst, fmove_src(X))};
    phi -> phi_argvar_subst(X, Subst);
%%  goto -> X;
%%  begin_try -> X;
%%  begin_handler -> X;
%%  end_try -> X;
%%  comment -> X;
%%  label -> X
    _ -> X
  end.

subst_defines(Subst, X) ->
  case type(X) of
    move -> X#move{dst = subst1(Subst, move_dst(X))};
    call -> X#call{dstlist = subst_list(Subst, call_dstlist(X))};
    begin_handler -> 
      X#begin_handler{dstlist = subst_list(Subst,
					   begin_handler_dstlist(X))};
    fmove -> X#fmove{dst = subst1(Subst, move_dst(X))};
    phi -> X#phi{dst = subst1(Subst, phi_dst(X))};
%%    'if' -> X;
%%    switch_val -> X;
%%    switch_tuple_arity -> X;
%%    type -> X;
%%    goto -> X;
%%    fail -> X;
%%    enter -> X;
%%    return -> X;
%%    begin_try -> X;
%%    end_try -> X;
%%    comment -> X;
%%    label -> X
    _ -> X
  end.

subst_list(S,Xs) ->
  [subst1(S,X) || X <- Xs].

subst1([],X) -> X;
subst1([{X,Y}|_],X) -> Y;
subst1([_|Xs],X) -> subst1(Xs,X).

%%
%% @doc Returns the successors of an Icode branch instruction.
%%

successors(Jmp) ->
  case type(Jmp) of
    'if' -> [if_true_label(Jmp), if_false_label(Jmp)];
    goto -> [goto_label(Jmp)];
    switch_val -> [switch_val_fail_label(Jmp)|
		   lists:map(fun (C) -> element(2,C) end,
			     switch_val_cases(Jmp))];
    switch_tuple_arity -> [switch_tuple_arity_fail_label(Jmp)|
			   lists:map(fun (C) -> element(2,C) end,
				     switch_tuple_arity_cases(Jmp))];
    type -> [type_true_label(Jmp), type_false_label(Jmp)];
    call -> [call_continuation(Jmp)|
	     case call_fail_label(Jmp) of [] -> []; L -> [L] end];
    begin_try -> [begin_try_successor(Jmp), begin_try_label(Jmp)];
    fail -> case fail_label(Jmp) of [] -> []; L -> [L] end;
    _ -> []
  end.

%%
%% @doc Returns the fail-labels of an Icode instruction.
%%

fails_to(I) ->
  case type(I) of
    switch_val -> [switch_val_fail_label(I)];
    switch_tuple_arity -> [switch_tuple_arity_fail_label(I)];
    call -> [call_fail_label(I)];
    begin_try -> [begin_try_label(I)];  % just for safety
    fail -> [fail_label(I)];
    _ -> []
  end.

%%
%% @doc Redirects jumps from label Old to label New. If the
%%      instruction does not jump to Old, it remains unchanged.
%%

redirect_jmp(Jmp, ToOld, ToOld) ->
  Jmp;    % no need to do anything
redirect_jmp(Jmp, ToOld, ToNew) ->
  NewIns =
    case type(Jmp) of
      'if' ->
	NewJmp = case if_true_label(Jmp) of
		   ToOld -> if_true_label_update(Jmp, ToNew);
		   _ -> Jmp
		 end,
	case if_false_label(NewJmp) of
	  ToOld -> if_false_label_update(NewJmp, ToNew);
	  _ -> NewJmp
	end;
      goto ->
	case goto_label(Jmp) of
	  ToOld -> Jmp#goto{label=ToNew};
	  _ -> Jmp
	end;
      switch_val ->
	NewJmp = case switch_val_fail_label(Jmp) of
		   ToOld -> switch_val_fail_label_update(Jmp, ToNew);
		   _ -> Jmp
		 end,
	NewJmp#switch_val{cases = 
			  lists:map(fun (Pair) ->
					case Pair of 
					  ({Val,ToOld}) -> {Val,ToNew};
					  (Unchanged) -> Unchanged
					end
				    end, 
				    switch_val_cases(NewJmp))
			 };
      switch_tuple_arity ->
	NewJmp = case switch_tuple_arity_fail_label(Jmp) of
		   ToOld -> 
		     Jmp#switch_tuple_arity{fail_label=ToNew};
		   _ -> Jmp
		 end,
	NewJmp#switch_tuple_arity{cases = 
				  lists:map(fun (Pair) -> 
						case Pair of
						  ({Val,ToOld}) -> {Val,ToNew};
						  (Unchanged) -> Unchanged
						end
					    end, 
					    switch_tuple_arity_cases(NewJmp))
				 };
      type ->
	NewJmp = case type_true_label(Jmp) of
		   ToOld -> Jmp#type{true_label=ToNew};
		   _ -> Jmp
		 end,
	case type_false_label(NewJmp) of
	  ToOld -> NewJmp#type{false_label=ToNew};
	  _ -> NewJmp
	end;
      call -> 
	NewCont = case call_continuation(Jmp) of
		    ToOld -> ToNew;
		    OldCont -> OldCont
		  end,
	NewFail = case call_fail_label(Jmp) of
		    ToOld -> ToNew;
		    OldFail -> OldFail
		  end,
	Jmp#call{continuation = NewCont, 
		 fail_label = NewFail};
      begin_try ->
	NewLabl = case begin_try_label(Jmp) of
		    ToOld ->  ToNew;
		    OldLab -> OldLab
		  end,
	NewSucc = case begin_try_successor(Jmp) of
		    ToOld ->  ToNew;
		    OldSucc -> OldSucc
		  end,
	Jmp#begin_try{label = NewLabl,successor=NewSucc};
      fail ->
	case fail_label(Jmp) of
	  ToOld -> Jmp#fail{fail_label=ToNew};
	  _ -> Jmp
	end;
      _ ->  Jmp
    end,
  simplify_branch(NewIns).

%%
%% @doc Turns a branch into a goto if it has only one successor and it
%%      is safe to do so.
%%

simplify_branch(I) ->
  case ordsets:from_list(successors(I)) of
    [Label] ->
      Goto = mk_goto(Label),
      case type(I) of
	type -> Goto;
	'if' -> Goto;
	switch_tuple_arity -> Goto;
	switch_val -> Goto;
	_ -> I
      end;
    _ -> I
  end.

%%
%% Is this an unconditional jump (causes a basic block not to have a 
%% fallthrough successor).
%%

%% is_uncond(I) ->
%%   case type(I) of
%%     goto -> true;
%%     fail -> true;
%%     enter -> true;
%%     return -> true;
%%     call -> 
%%       case call_fail_label(I) of
%% 	[] -> 
%% 	  case call_continuation(I) of
%% 	    [] -> false;
%% 	    _ -> true
%% 	  end;
%% 	_ -> true
%%       end;
%%     _ -> false
%%   end.

%% @spec is_branch(icode_instruction()) -> bool()
%%
%% @doc Succeeds if the Icode instruction is a branch. I.e. a
%%      (possibly conditional) discontinuation of linear control flow.
%% @end

is_branch(Instr) ->
  case type(Instr) of
    'if' -> true;
    switch_val -> true;
    switch_tuple_arity -> true;
    type -> true;
    goto -> true;
    fail -> true;
    call -> 
      case call_fail_label(Instr) of
	[] -> 
	  case call_continuation(Instr) of
	    [] -> false;
	    _ -> true
	  end;
	_ -> true
      end;
    enter -> true;
    return -> true;
    begin_try -> true;
    _ -> false
  end.

%%
%% @doc Makes a new variable.
%%

mk_new_var() ->
  mk_var(hipe_gensym:get_next_var(icode)).

%%
%% @doc Makes a new fp variable.
%%

mk_new_fvar() ->
  mk_fvar(hipe_gensym:get_next_var(icode)).

%%
%% @doc Makes a new register.
%%

mk_new_reg() ->
  mk_reg(hipe_gensym:get_next_var(icode)).

%%
%% @doc Makes a new label.
%%

mk_new_label() ->
  mk_label(hipe_gensym:get_next_label(icode)).

%%
%% @doc Makes a bunch of move operations.
%%

mk_moves([], []) ->
  [];
mk_moves([X|Xs], [Y|Ys]) ->
  [mk_move(X, Y) | mk_moves(Xs, Ys)].

%%
%% Makes a series of element operations.
%%

%% mk_elements(_, []) -> 
%%   [];
%% mk_elements(Tuple, [X|Xs]) ->
%%   [mk_primop([X], {unsafe_element, length(Xs)+1}, [Tuple]) | 
%%    mk_elements(Tuple, Xs)].

%%
%% @doc Removes comments from Icode.
%%

strip_comments(ICode) ->
  icode_code_update(ICode, no_comments(icode_code(ICode))).

no_comments([]) ->
  [];
no_comments([I|Xs]) ->
  case is_comment(I) of 
    true -> no_comments(Xs);
    false -> [I|no_comments(Xs)]
  end.

%%-----------------------------------------------------------------------

%% @spec is_safe(icode_instruction()) -> bool()
%%
%% @doc True if an Icode instruction is safe (can be removed if the
%% result is not used).

is_safe(Instr) ->
  case type(Instr) of
    move -> true;
    fmove -> true;
    phi -> true;
    begin_handler -> true;
    call ->
      case call_fun(Instr) of
	{M,F,A} ->
	  erl_bifs:is_safe(M,F,A);
	Op ->
	  hipe_icode_primops:is_safe(Op)
      end;
    _ -> false
  end.

%% @spec is_leaf_code([icode_instruction()]) -> bool()
%% @doc Returns `false' if code sequence is not leaf code,
%% otherwise `true'.

is_leaf_code([]) ->
  true;
is_leaf_code([I|Is]) ->
  case type(I) of
    call ->
      case call_type(I) of
	primop -> 
	  case call_fun(I) of
	    call_fun -> false;		% Calls closure
	    enter_fun -> false;		% Calls closure
	    _ -> is_leaf_code(Is)	% Other primop calls are ok
	  end;
	_ -> 
	  false    % non-primop call
      end;
    enter -> false;
    _ ->
      is_leaf_code(Is)
  end.

%%-----------------------------------------------------------------------

highest_var(Code) ->
  highest_var(Code, 0).

highest_var([I|Is], Max) ->
  Defs = defines(I),
  Uses = uses(I),
  highest_var(Is, new_max(Defs++Uses,Max));
highest_var([], Max) ->
  Max.

new_max([V|Vs], Max) ->
  VName = 
    case is_var(V) of
      true ->
	var_name(V);
      false ->
	case is_fvar(V) of
	  true ->
	    fvar_name(V);
	  _ ->
	    reg_name(V)
	end
    end,
  if VName > Max ->
      new_max(Vs, VName);
     true ->
      new_max(Vs, Max)
  end;
new_max([], Max) ->
  Max.

%%-----------------------------------------------------------------------

highest_label(Code) ->
  highest_label(Code, 0).

highest_label([I|Is], Max) ->
  case is_label(I) of 
    true ->
      L = label_name(I),
      NewMax = if L > Max -> L; true -> Max end,
      highest_label(Is, NewMax);
    false ->
      highest_label(Is, Max)
  end;
highest_label([], Max) ->
  Max.

%%-----------------------------------------------------------------------
