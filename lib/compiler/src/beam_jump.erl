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
%%%   (5) If test instructions are used to skip a single jump instruction,
%%%       the test is inverted and the jump is eliminated (provided that
%%%       the test can be inverted).  Example:
%%%
%%%       is_eq L1 {x,1} {x,2}
%%%       jmp L2
%%%       L1:
%%%
%%%       will be changed to
%%%
%%%       is_ne L2 {x,1} {x,2}
%%%
%%%       (The label L1 will be retained if there were previous references to it.)
%%%
%%% Terminology note: The optimisation done here is called unreachable-code
%%% elimination, NOT dead-code elimination.  Dead code elimination
%%% means the removal of instructions that are executed, but have no visible
%%% effect on the program state.
%%% 

-import(lists, [reverse/1,reverse/2,map/2]).
-compile({inline,[{is_label_used,2}]}).

-record(st, {fc,				% Label for function class errors.
	     mcode,				% Moved code (case_end, if_end).
	     mlbl,				% Moved labels.
	     labels				% Set of referenced labels.
	    }).

module({Mod,Exp,Attr,Fs,Lc}, Opt) ->
    {ok,{Mod,Exp,Attr,map(fun function/1, Fs),Lc}}.

function({function,Name,Arity,CLabel,Asm0}) ->
    [{label,Fc},Fi,{label,Entry}|_] = Asm0,
    Lbls = ordsets:list_to_set([Fc,Entry]),
    St0 = #st{fc=Fc,mcode=dict:new(),mlbl=dict:new(),labels=Lbls},
    {Asm1,St} = opt(Asm0, [], St0),
    Asm = remove_unused_labels(Asm1, St0),
    {function,Name,Arity,CLabel,Asm}.


opt([{test,Test0,{f,Lnum}=Lbl,Ops0}=I|Is0], Acc, St) ->
    case Is0 of
	[{jump,To}|[{label,Lnum}|Is2]=Is1] ->
	    case invert_test(Test0, Ops0) of
		{yes,Test,Ops} ->
		    Is = case is_label_used(Lnum, St) of
			     true -> Is1;
			     false -> Is2
			 end,
		    opt(Is, [{test,Test,To,Ops}|Acc], label_used(To, St));
		no ->
		    opt(Is0, [I|Acc], label_used(Lbl, St))
	    end;
	Other ->
	    opt(Is0, [I|Acc], label_used(Lbl, St))
    end;
opt([{select_val,R,Fail,{list,Vls}}=I|Is], Acc, St) ->
    skip_unreachable(Is, [I|Acc], label_used([Fail|Vls], St));
opt([{select_tuple_arity,R,Fail,{list,Vls}}=I|Is], Acc, St) ->
    skip_unreachable(Is, [I|Acc], label_used([Fail|Vls], St));
opt([{'catch',R,Lbl}=I|Is], Acc, St) ->
    opt(Is, [I|Acc], label_used(Lbl, St));
opt([{label,L1},{jump,{f,L2}}=I|Is], [Prev|Acc], St0) ->
    St1 = St0#st{mlbl=dict:append(L2, L1, St0#st.mlbl)},
    opt([Prev,I|Is], Acc, label_used({f,L2}, St1));
opt([{label,L},{case_end,R}=I|Is], [Prev|Acc], St0) ->
    St1 = St0#st{mcode=dict:append([I], L, St0#st.mcode)},
    opt([Prev,I|Is], Acc, St1);
opt([{label,L},{badmatch,R}=I|Is], [Prev|Acc], St0) ->
    St1 = St0#st{mcode=dict:append([I], L, St0#st.mcode)},
    opt([Prev,I|Is], Acc, St1);
opt([{label,L},if_end=I|Is], [Prev|Acc], St0) ->
    St1 = St0#st{mcode=dict:append([I], L, St0#st.mcode)},
    opt([Prev,I|Is], Acc, St1);
opt([{label,Lbl}=I|Is], Acc, St0) ->
    case dict:find(Lbl, St0#st.mlbl) of
	{ok,Lbls} ->
	    %% Essential to remove the list of labels from the dictionary,
	    %% since we will rescan the inserted labels.  We MUST rescan.
	    St1 = St0#st{mlbl=dict:erase(Lbl, St0#st.mlbl)},
	    insert_labels([Lbl|Lbls], Is, Acc, St1);
	error -> opt(Is, [I|Acc], St0)
    end;
opt([{jump,{f,Lbl}},{label,Lbl}=I|Is], Acc, St) ->
    opt([I|Is], Acc, St);
opt([{jump,Lbl}=I|Is], Acc, St) ->
    skip_unreachable(Is, [I|Acc], label_used(Lbl, St));
opt([{loop_rec,Lbl,R}=I|Is], Acc, St) ->
    opt(Is, [I|Acc], label_used(Lbl, St));
opt([{bif,Name,Lbl,As,R}=I|Is], Acc, St) ->
    opt(Is, [I|Acc], label_used(Lbl, St));
opt([{bs_put_integer,Lbl,Bits,Unit,Fl,Val}=I|Is], Acc, St) ->
    opt(Is, [I|Acc], label_used(Lbl, St));
opt([{bs_put_binary,Lbl,Bits,Unit,Fl,Val}=I|Is], Acc, St) ->
    opt(Is, [I|Acc], label_used(Lbl, St));
opt([{bs_put_float,Lbl,Bits,Unit,Fl,Val}=I|Is], Acc, St) ->
    opt(Is, [I|Acc], label_used(Lbl, St));
opt([{bs_final,Lbl,R}=I|Is], Acc, St) ->
    opt(Is, [I|Acc], label_used(Lbl, St));
opt([{call_ext,1,{extfunc,erlang,exit,1}}=I|Is], Acc, St) ->
    opt_exit(I, Is, Acc, St);
opt([{call_ext_last,1,{extfunc,erlang,exit,1},D}=I|Is], Acc, St) ->
    opt_exit(I, Is, Acc, St);
opt([{call_ext_only,1,{extfunc,erlang,exit,1}}=I|Is], Acc, St) ->
    opt_exit(I, Is, Acc, St);
opt([{call_ext,1,{extfunc,erlang,fault,1}}=I|Is], Acc, St) ->
    opt_exit(I, Is, Acc, St);
opt([{call_ext_last,1,{extfunc,erlang,fault,1},D}=I|Is], Acc, St) ->
    opt_exit(I, Is, Acc, St);
opt([{call_ext_only,1,{extfunc,erlang,fault,1}}=I|Is], Acc, St) ->
    opt_exit(I, Is, Acc, St);
opt([{call_ext,1,{extfunc,erlang,fault,2}}=I|Is], Acc, St) ->
    opt_exit(I, Is, Acc, St);
opt([{call_ext_last,1,{extfunc,erlang,fault,2},D}=I|Is], Acc, St) ->
    opt_exit(I, Is, Acc, St);
opt([{call_ext_only,1,{extfunc,erlang,fault,2}}=I|Is], Acc, St) ->
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

%% invert_test(Test0, Ops0) -> no | {yes,Test,Ops}

invert_test(is_eq, Ops) -> {yes,is_ne,Ops};
invert_test(is_ne, Ops) -> {yes,is_eq,Ops};
invert_test(is_eq_exact, Ops) -> {yes,is_ne_exact,Ops};
invert_test(is_ne_exact, Ops) -> {yes,is_eq_exact,Ops};
invert_test(_, _) -> no.

%% opt_exit(ExitInstruction, UnoptimizedInsts, OptimizedInstrs, State)
%%  If a call to the erlang:exit/1 function always is 
%%  preceeded by a basic block to which control is always transferred
%%  by a jump, we can move it to the end of the function and perhaps
%%  coalesce it with other identical blocks.  Even if no coalescing
%%  occurs, further optimizations are usually possible at the original
%%  location of the exit/1 call. (We used to move it to the end of
%%  the module, but that could cause problems when unused functions
%%  are later removed.)
%%
%%  Note: If the very first instruction in a function is an
%%  exit/1, it is not safe to move away the instruction.  The entry
%%  label MUST be located directly after the func_info instruction.

opt_exit(Exit, Is, [{block,_},{label,_},{func_info,_,_,_}|_]=Acc, St) ->
    skip_unreachable(Is, [Exit|Acc], St);
opt_exit(Exit, Is, [{kill,Y}|Acc], St) ->
    opt_exit(Exit, Is, Acc, St);
opt_exit(Exit, Is, [{block,B},{label,L},Dead|More]=Acc, St0) ->
    case is_unreachable_after(Dead) of
	false -> skip_unreachable(Is, [Exit|Acc], St0);
	true ->
	    Moved = [{block,B},Exit],
	    St1 = St0#st{mcode=dict:append(Moved, L, St0#st.mcode)},
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
    Code = dict:fold(fun(Code0, Ls, Acc) ->
			     [{label,L} || L <- Ls ] ++ Code0 ++ Acc
		     end, [], St#st.mcode),
    case Code of
	[] -> [];
	_ -> [{'%','Moved code'}|Code]
    end.

%% Skip unreachable code up to the next referenced label.

skip_unreachable([{label,L}|Is], [{jump,{f,L}}|Acc], St) ->
    opt([{label,L}|Is], Acc, St);
skip_unreachable([{label,L}|Is], Acc, St) ->
    case is_label_used(L, St) of
	true  -> opt([{label,L}|Is], Acc, St);
	false -> skip_unreachable(Is, Acc, St)
    end;
skip_unreachable([I|Is], Acc, St) ->
    skip_unreachable(Is, Acc, St);
skip_unreachable([], Acc, St) ->
    opt([], Acc, St).

%% Add one or more label to the set of used labels.

label_used({f,0}, St) -> St;
label_used({f,L}, St) ->St#st{labels=ordsets:add_element(L, St#st.labels)};
label_used([H|T], St0) -> label_used(T, label_used(H, St0));
label_used([], St) -> St;
label_used(Other, St) -> St.

%% Test is label is used.

is_label_used(L, St) ->
    ordsets:is_element(L, St#st.labels).

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

%%%
%%% Remove any remaining unused labels.
%%%

remove_unused_labels(Is, St0) ->
    St = lbl(Is, St0),
    remove_unused_labels(Is, St, []).

remove_unused_labels(Is, St, [{block,Bl2},{block,Bl1}|Acc]) ->
    Block = {block,beam_block:merge_blocks(Bl1, Bl2)},
    remove_unused_labels(Is, St, [Block|Acc]);
remove_unused_labels([{label,L}=I|Is], St, Acc) ->
    case is_label_used(L, St) of
	true -> remove_unused_labels(Is, St, [I|Acc]);
	false -> remove_unused_labels(Is, St, Acc)
    end;
remove_unused_labels([I|Is], St, Acc) ->
    remove_unused_labels(Is, St, [I|Acc]);
remove_unused_labels([], St, Acc) ->
    reverse(Acc).

lbl([{test,Test0,Lbl,Ops0}|Is], St) ->
    lbl(Is, label_used(Lbl, St));
lbl([{select_val,R,Fail,{list,Vls}}|Is], St) ->
    lbl(Is, label_used([Fail|Vls], St));
lbl([{select_tuple_arity,R,Fail,{list,Vls}}|Is], St) ->
    lbl(Is, label_used([Fail|Vls], St));
lbl([{'catch',R,Lbl}|Is], St) ->
    lbl(Is, label_used(Lbl, St));
lbl([{jump,Lbl}|Is], St) ->
    lbl(Is, label_used(Lbl, St));
lbl([{loop_rec,Lbl,R}|Is], St) ->
    lbl(Is, label_used(Lbl, St));
lbl([{loop_rec_end,Lbl}|Is], St) ->
    lbl(Is, label_used(Lbl, St));
lbl([{wait,Lbl}|Is], St) ->
    lbl(Is, label_used(Lbl, St));
lbl([{wait_timeout,Lbl,To}|Is], St) ->
    lbl(Is, label_used(Lbl, St));
lbl([{bif,Name,Lbl,As,R}|Is], St) ->
    lbl(Is, label_used(Lbl, St));
lbl([{bs_put_integer,Lbl,Bits,Unit,Fl,Val}|Is], St) ->
    lbl(Is, label_used(Lbl, St));
lbl([{bs_put_binary,Lbl,Bits,Unit,Fl,Val}|Is], St) ->
    lbl(Is, label_used(Lbl, St));
lbl([{bs_put_float,Lbl,Bits,Unit,Fl,Val}|Is], St) ->
    lbl(Is, label_used(Lbl, St));
lbl([{bs_final,Lbl,R}=I|Is], St) ->
    lbl(Is, label_used(Lbl, St));
lbl([I|Is], St) ->
    lbl(Is, St);
lbl([], St) -> St.
