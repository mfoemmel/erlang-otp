%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
%%% Purpose : Optimise jumps and remove unreachable code.

-module(beam_jump).

-export([module/2]).

%%% The following optimisations are done:
%%%
%%% (1) Any unreachable code is removed.  Unreachable code is code after
%%%     jump, call_last and other instructions which never transfer control
%%%     to the following statement.  Code is unreachable up to the next
%%%     *referenced* label.  Note that the optimisations below might
%%%     generate more possibilities for removing unreachable code.
%%%
%%% (2) This code:
%%%	L1:	jump L2
%%%        .
%%%        .
%%%        .
%%%     L2: ...
%%%
%%%    will be changed to
%%%
%%%    jump L2
%%%        .
%%%        .
%%%        .
%%%    L1:
%%%    L2: ...
%%%
%%%    If the jump is unreachable, it will be removed according to (1).
%%%
%%% (3) case_end, if_end, and badmatch instructions are moved to the end of
%%%     the function and combined:
%%%
%%%    L1:  badmatch {x,0}
%%%        .
%%%        .
%%%        .
%%%    L2:  badmatch {x,0}
%%%        .
%%%        .
%%%        .
%%%    will be changed to
%%%
%%%	   badmatch {x,0}
%%%        .
%%%        .
%%%        .
%%%	   badmatch {x,0}
%%%        .
%%%        .
%%%        .
%%%    L1:  
%%%    L2:  badmatch {x,0}
%%%
%%%    If the original badmatch instructions are unreachable, they will be removed.
%%%
%%%  (4) In
%%%
%%%	 jump L1
%%%      L1:
%%%
%%%	the jump will be removed.
%%%
%%% Terminology note: The optimisation done here is called unreachable-code
%%% elimination, NOT dead-code elimination.  Dead code elimination
%%% means the removal of instructions that are executed, but have no visible
%%% effect on the program state.
%%% 

-import(lists, [reverse/2]).

-record(st, {fc,				% Label for function class errors.
	     mcode,				% Moved code (case_end, if_end).
	     mlbl,				% Moved labels.
	     labels,				% Set of referenced labels.
	     mglobal}).				% Moved global code.

module({Mod,Exp,Attr,Fs,Lc}, Opt) ->
    Functions = functions(Fs, dict:new()),
    {ok,{Mod,Exp,Attr,Functions,Lc}}.

functions([F], Global0) ->
    {{function,Name,Arity,Clabel,Asm},Global1} = function(F, Global0),
    [{function,Name,Arity,Clabel,Asm ++
      insert_global_code(dict:dict_to_list(Global1))}];
functions([F0|Fs], Global0) ->
    {F1,Global1} = function(F0, Global0),
    [F1|functions(Fs, Global1)];
functions([], Global) -> [].

insert_global_code([{Code,Ls}|Cls]) ->
    [{label,L} || L <- Ls]  ++ Code ++ insert_global_code(Cls);
insert_global_code([]) -> [].

function({function,Name,Arity,CLabel,Asm0}, Global) ->
    [{label,Fc},Fi,{label,Entry}|_] = Asm0,
    Lbls = ordsets:list_to_set([Entry]),
    St0 = #st{fc=Fc,mcode=dict:new(),mlbl=dict:new(),labels=Lbls,mglobal=Global},
    {Asm1,St1} = opt(Asm0, [], St0),
    {{function,Name,Arity,CLabel,Asm1},St1#st.mglobal}.

opt([{test,Test,Lbl}=I|Is], Acc, St) ->
    opt(Is, [I|Acc], label_used(Lbl, St));
opt([{test,Test,Lbl,A1}=I|Is], Acc, St) ->
    opt(Is, [I|Acc], label_used(Lbl, St));
opt([{test,Test,Lbl,A1,A2}=I|Is], Acc, St) ->
    opt(Is, [I|Acc], label_used(Lbl, St));
opt([{test,Test,Lbl,A1,A2,A3}=I|Is], Acc, St) ->
    opt(Is, [I|Acc], label_used(Lbl, St));
opt([{test,Test,Lbl,A1,A2,A3,A4}=I|Is], Acc, St) ->
    opt(Is, [I|Acc], label_used(Lbl, St));
opt([{select_val,R,Fail,{list,Vls}}|Is], Acc, St) ->
    skip_unreachable(Is, [{select_val,R,Fail,{list,Vls}}|Acc],
		     label_used([Fail|Vls], St));
opt([{select_tuple_arity,R,Fail,{list,Vls}}|Is], Acc, St) ->
    skip_unreachable(Is, [{select_tuple_arity,R,Fail,{list,Vls}}|Acc],
	      label_used([Fail|Vls], St));
opt([{'catch',R,Lbl}|Is], Acc, St) ->
    opt(Is, [{'catch',R,Lbl}|Acc], label_used(Lbl, St));
opt([{label,L1},{jump,{f,L2}}|Is], [Prev|Acc], St0) ->
    St1 = St0#st{mlbl=dict:append(L2, L1, St0#st.mlbl)},
    opt([Prev,{jump,{f,L2}}|Is], Acc, label_used({f,L2}, St1));
opt([{label,L},{case_end,R}|Is], [Prev|Acc], St0) ->
    I = {case_end,R},
    St1 = St0#st{mcode=dict:append(I, L, St0#st.mcode)},
    opt([Prev,I|Is], Acc, St1);
opt([{label,L},{badmatch,R}|Is], [Prev|Acc], St0) ->
    I = {badmatch,R},
    St1 = St0#st{mcode=dict:append(I, L, St0#st.mcode)},
    opt([Prev,I|Is], Acc, St1);
opt([{label,L},if_end|Is], [Prev|Acc], St0) ->
    I = if_end,
    St1 = St0#st{mcode=dict:append(I, L, St0#st.mcode)},
    opt([Prev,I|Is], Acc, St1);
opt([{label,Lbl}|Is], Acc, St0) ->
    case dict:find(Lbl, St0#st.mlbl) of
	{ok,Lbls} ->
	    %% Essential to remove the list of labels from the dictionary,
	    %% since we will rescan the inserted labels.  We MUST rescan.
	    St1 = St0#st{mlbl=dict:erase(Lbl, St0#st.mlbl)},
	    insert_labels([Lbl|Lbls], Is, Acc, St1);
	error -> opt(Is, [{label,Lbl}|Acc], St0)
    end;
opt([{jump,{f,Lbl}},{label,Lbl}|Is], Acc, St) ->
    opt([{label,Lbl}|Is], Acc, St);
opt([{jump,Lbl}|Is], Acc, St) ->
    skip_unreachable(Is, [{jump,Lbl}|Acc], label_used(Lbl, St));
opt([{loop_rec,Lbl,R}|Is], Acc, St) ->
    opt(Is, [{loop_rec,Lbl,R}|Acc], label_used(Lbl, St));
opt([{call_ext,1,{extfunc,erlang,exit,1}}=I|Is], Acc, St) ->
    opt_exit(I, Is, Acc, St);
opt([{call_ext_last,1,{extfunc,erlang,exit,1},D}=I|Is], Acc, St) ->
    opt_exit(I, Is, Acc, St);
opt([{call_ext_only,1,{extfunc,erlang,exit,1}}=I|Is], Acc, St) ->
    opt_exit(I, Is, Acc, St);
opt([I|Is], Acc, St) ->
    case is_unreachable_after(I) of
	true  -> skip_unreachable(Is, [I|Acc], St);
	false -> opt(Is, [I|Acc], St)
    end;
opt([], Acc, St0) ->
    Code = reverse(Acc, insert_moved_code(St0)),
    {case dict:find(St0#st.fc, St0#st.mlbl) of
	 {ok,Lbls} -> [{label,L} || L <- Lbls] ++ Code;
	 error -> Code
     end,St0}.

%% opt_exit(ExitInstruction, UnoptimizedInsts, OptimizedInstrs)
%%  If a call to the erlang:exit/1 function always is 
%%  preceeded by a basic block to which control is always transferred
%%  by a jump, we can move it to the end of the module and perhaps
%%  coalesce it with other identical blocks.  Even if no coalescing
%%  occurs, further optimizations are usually possible at the original
%%  location of the exit/1 call.
%%
%%  Note: If the very first instruction in a function is an
%%  exit/1, it is not safe to move away the instruction.  The entry
%%  label MUST be located directly after the func_info instruction.

opt_exit(Exit, Is, [{block,_},{label,_},{func_info,_,_,_}|_]=Acc, St) ->
    skip_unreachable(Is, [Exit|Acc], St);
opt_exit(Exit, Is, [{block,B},{label,L},Dead|More]=Acc, St0) ->
    case is_unreachable_after(Dead) of
	false -> skip_unreachable(Is, [Exit|Acc], St0);
	true ->
	    Moved = [{block,B},Exit],
	    St1 = St0#st{mglobal=dict:append(Moved, L, St0#st.mglobal)},
	    opt([Dead|Is], More, St1)
    end;
opt_exit(Exit, Is, Acc, St) ->
    skip_unreachable(Is, [Exit|Acc], St).

insert_labels([L|Ls], Is, [{jump,{f,L}}|Acc], St) ->
    insert_labels(Ls, [{label,L}|Is], Acc, St);
insert_labels([L|Ls], Is, Acc, St) ->
    insert_labels(Ls, [{label,L}|Is], Acc, St);
insert_labels([], Is, Acc, St) ->
    opt(Is, Acc, St).

insert_moved_code(St) ->
    insert_moved_code1(dict:dict_to_list(St#st.mcode)).

insert_moved_code1([{Code,Ls}|Cls]) ->
    [ {label,L} || L <- Ls ] ++ [Code|insert_moved_code1(Cls)];
insert_moved_code1([]) -> [].

%% Skip unreachable code up to the next referenced label.

skip_unreachable([{label,L}|Is], [{jump,{f,L}}|Acc], St) ->
    opt([{label,L}|Is], Acc, St);
skip_unreachable([{label,L}|Is], Acc, St) ->
    case ordsets:is_element(L, St#st.labels) of
	true  -> opt([{label,L}|Is], Acc, St);
	false -> skip_unreachable(Is, Acc, St)
    end;
skip_unreachable([I|Is], Acc, St) ->
    skip_unreachable(Is, Acc, St);
skip_unreachable([], Acc, St) ->
    opt([], Acc, St).

%% Add one or more label to the set of used labels.

label_used({f,L}, St) ->
    St#st{labels=ordsets:add_element(L, St#st.labels)};
label_used([H|T], St0) ->
    label_used(T, label_used(H, St0));
label_used([], St) -> St;
label_used(Other, St) -> St.

%% is_unreachable_after(Instruction) -> true|false
%%  Returns true if code after Instruction is unreachable.

is_unreachable_after({func_info,M,F,A}) -> true;
is_unreachable_after(return) -> true;
is_unreachable_after(if_end) -> true;
is_unreachable_after({case_end,_}) -> true;
is_unreachable_after({badmatch,_}) -> true;
is_unreachable_after({call_ext_last,Ar,ExtFunc,D}) -> true;
is_unreachable_after({call_ext_only,Ar,ExtFunc}) -> true;
is_unreachable_after({call_last,Ar,Lbl,D}) -> true;
is_unreachable_after({call_only,Ar,Lbl}) -> true;
is_unreachable_after({jump,Lbl}) -> true;
is_unreachable_after({call_ext,1,{extfunc,erlang,exit,1}}) -> true;
is_unreachable_after({call_ext,1,{extfunc,erlang,throw,1}}) -> true;
is_unreachable_after({call_ext,1,{extfunc,erlang,fault,1}}) -> true;
is_unreachable_after({call_ext,1,{extfunc,erlang,fault,2}}) -> true;
is_unreachable_after({select_val,R,Lbl,Cases}) -> true;
is_unreachable_after({select_tuple_arity,R,Lbl,Cases}) -> true;
is_unreachable_after({wait,_}) -> true;
is_unreachable_after(I) -> false.
