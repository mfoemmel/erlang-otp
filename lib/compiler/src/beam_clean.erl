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
%% Purpose : Clean up, such as removing unused functions.

-module(beam_clean).

-export([module/2]).
-import(lists, [member/2,map/2,foldl/3,mapfoldl/3]).

-compile({inline,{label,2}}).

module({Mod,Exp,Attr,Fs0,Lc0}, Opt) ->
    Order = [Lbl || {function,_,_,Lbl,_} <- Fs0],
    All = foldl(fun({function,_,_,Lbl,_}=Func,D) -> dict:store(Lbl, Func, D) end,
		dict:new(), Fs0),
    {WorkList,Used0} = exp_to_labels(Fs0, Exp),
    Used = find_all_used(WorkList, All, Used0),
    Fs1 = remove_unused(Order, Used, All),
    {Fs,Lc} = clean_labels(Fs1),
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

%% Convert the export list ({Name,Arity} pairs) to a list of entry labels.

exp_to_labels(Fs, Exp) -> exp_to_labels(Fs, Exp, [], sets:new()).
    
exp_to_labels([{function,Name,Arity,Lbl,_}|Fs], Exp, Acc, Used) ->
    case member({Name,Arity}, Exp) of
	true -> exp_to_labels(Fs, Exp, [Lbl|Acc], sets:add_element(Lbl, Used));
	false -> exp_to_labels(Fs, Exp, Acc, Used)
    end;
exp_to_labels([], _, Acc, Used) -> {Acc,Used}.

%% Remove the unused functions.

remove_unused([F|Fs], Used, All) ->
    case sets:is_element(F, Used) of
	false -> remove_unused(Fs, Used, All);
	true -> [dict:fetch(F, All)|remove_unused(Fs, Used, All)]
    end;
remove_unused([], _, _) -> [].
	    
%% Find all used functions.

find_all_used([F|Fs0], All, Used0) ->
    {function,_,_,_,Code} = dict:fetch(F, All),
    {Fs,Used} = update_work_list(Code, {Fs0,Used0}),
    find_all_used(Fs, All, Used);
find_all_used([], _All, Used) -> Used.

update_work_list([{call,_,{f,L}}|Is], Sets) ->
    update_work_list(Is, add_to_work_list(L, Sets));
update_work_list([{call_last,_,{f,L},_}|Is], Sets) ->
    update_work_list(Is, add_to_work_list(L, Sets));
update_work_list([{call_only,_,{f,L}}|Is], Sets) ->
    update_work_list(Is, add_to_work_list(L, Sets));
update_work_list([{make_fun,{f,L},_,_}|Is], Sets) ->
    update_work_list(Is, add_to_work_list(L, Sets));
update_work_list([{make_fun2,{f,L},_,_,_}|Is], Sets) ->
    update_work_list(Is, add_to_work_list(L, Sets));
update_work_list([_|Is], Sets) ->
    update_work_list(Is, Sets);
update_work_list([], Sets) -> Sets.

add_to_work_list(F, {Fs,Used}=Sets) ->
    case sets:is_element(F, Used) of
	true -> Sets;
	false -> {[F|Fs],sets:add_element(F, Used)}
    end.


%%%
%%% Coalesce adjacent labels. Renumber all labels to eliminate gaps.
%%% This cleanup will slightly reduce file size and slightly speed up loading.
%%%

-record(st, {lmap,				%Translation tables for labels.
	     entry,				%Number of entry label.
	     lc					%Label counter
	     }).

clean_labels(Fs0) ->
    St0 = #st{lmap=dict:new(),lc=1},
    {Fs1,St1} = mapfoldl(fun function_renumber/2, St0, Fs0),
    Dict = St1#st.lmap,
    {map(fun(F) -> function_replace(F, Dict) end, Fs1),St1#st.lc}.
		
function_renumber({function,Name,Arity,_Entry,Asm0}, St0) ->
    {Asm,St1} = renumber_labels(Asm0, [], St0),
    {{function,Name,Arity,St1#st.entry,Asm},St1}.

renumber_labels([{label,Old}|Is], [{label,New}|_]=Acc, St0) ->
    D = dict:store(Old, New, St0#st.lmap),
    renumber_labels(Is, Acc, St0#st{lmap=D});
renumber_labels([{label,Old}|Is], Acc, St0) ->
    New = St0#st.lc,
    D = dict:store(Old, New, St0#st.lmap),
    renumber_labels(Is, [{label,New}|Acc], St0#st{lmap=D,lc=New+1});
renumber_labels([{func_info,_,_,_}=Fi|Is], Acc, St0) ->
    renumber_labels(Is, [Fi|Acc], St0#st{entry=St0#st.lc});
renumber_labels([I|Is], Acc, St0) ->
    renumber_labels(Is, [I|Acc], St0);
renumber_labels([], Acc, St0) -> {Acc,St0}.

function_replace({function,Name,Arity,Entry,Asm0}, Dict) ->
    Asm = case catch replace(Asm0, [], Dict) of
	      {'EXIT',_}=Reason ->
		  exit(Reason);
	      {error,{undefined_label,Lbl}=Reason} ->
		  io:format("Function ~s/~w refers to undefined label ~w\n",
			    [Name,Arity,Lbl]),
		  exit(Reason);
	      Asm1 when list(Asm1) -> Asm1
	  end,
    {function,Name,Arity,Entry,Asm}.

replace([{test,Test,{f,Lbl},Ops}|Is], Acc, D) ->
    replace(Is, [{test,Test,{f,label(Lbl, D)},Ops}|Acc], D);
replace([{select_val,R,{f,Fail},{list,Vls0}}|Is], Acc, D) ->
    Vls = map(fun ({f,L}) -> {f,label(L, D)};
		  (Other) -> Other end, Vls0),
    replace(Is, [{select_val,R,{f,label(Fail, D)},{list,Vls}}|Acc], D);
replace([{select_tuple_arity,R,{f,Fail},{list,Vls0}}|Is], Acc, D) ->
    Vls = map(fun ({f,L}) -> {f,label(L, D)};
		  (Other) -> Other end, Vls0),
    replace(Is, [{select_tuple_arity,R,{f,label(Fail, D)},{list,Vls}}|Acc], D);
replace([{'catch',R,{f,Lbl}}|Is], Acc, D) ->
    replace(Is, [{'catch',R,{f,label(Lbl, D)}}|Acc], D);
replace([{jump,{f,Lbl}}|Is], Acc, D) ->
    replace(Is, [{jump,{f,label(Lbl, D)}}|Acc], D);
replace([{loop_rec,{f,Lbl},R}|Is], Acc, D) ->
    replace(Is, [{loop_rec,{f,label(Lbl, D)},R}|Acc], D);
replace([{loop_rec_end,{f,Lbl}}|Is], Acc, D) ->
    replace(Is, [{loop_rec_end,{f,label(Lbl, D)}}|Acc], D);
replace([{wait,{f,Lbl}}|Is], Acc, D) ->
    replace(Is, [{wait,{f,label(Lbl, D)}}|Acc], D);
replace([{wait_timeout,{f,Lbl},To}|Is], Acc, D) ->
    replace(Is, [{wait_timeout,{f,label(Lbl, D)},To}|Acc], D);
replace([{bif,Name,{f,Lbl},As,R}|Is], Acc, D) when Lbl =/= 0 ->
    replace(Is, [{bif,Name,{f,label(Lbl, D)},As,R}|Acc], D);
replace([{call,Ar,{f,Lbl}}|Is], Acc, D) ->
    replace(Is, [{call,Ar,{f,label(Lbl,D)}}|Acc], D);
replace([{call_last,Ar,{f,Lbl},N}|Is], Acc, D) ->
    replace(Is, [{call_last,Ar,{f,label(Lbl,D)},N}|Acc], D);
replace([{call_only,Ar,{f,Lbl}}|Is], Acc, D) ->
    replace(Is, [{call_only,Ar,{f,label(Lbl, D)}}|Acc], D);
replace([{make_fun,{f,Lbl},U1,U2}|Is], Acc, D) ->
    replace(Is, [{make_fun,{f,label(Lbl, D)},U1,U2}|Acc], D);
replace([{make_fun2,{f,Lbl},U1,U2,U3}|Is], Acc, D) ->
    replace(Is, [{make_fun2,{f,label(Lbl, D)},U1,U2,U3}|Acc], D);
replace([{bs_put_integer,{f,Lbl},Bits,Unit,Fl,Val}|Is], Acc, D) when Lbl =/= 0 ->
    replace(Is, [{bs_put_integer,{f,label(Lbl, D)},Bits,Unit,Fl,Val}|Acc], D);
replace([{bs_put_binary,{f,Lbl},Bits,Unit,Fl,Val}|Is], Acc, D) when Lbl =/= 0 ->
    replace(Is, [{bs_put_binary,{f,label(Lbl, D)},Bits,Unit,Fl,Val}|Acc], D);
replace([{bs_put_float,{f,Lbl},Bits,Unit,Fl,Val}|Is], Acc, D) when Lbl =/= 0 ->
    replace(Is, [{bs_put_float,{f,label(Lbl, D)},Bits,Unit,Fl,Val}|Acc], D);
replace([{bs_final,{f,Lbl},R}|Is], Acc, D) when Lbl =/= 0 ->
    replace(Is, [{bs_final,{f,label(Lbl, D)},R}|Acc], D);
replace([I|Is], Acc, D) ->
    replace(Is, [I|Acc], D);
replace([], Acc, _) -> Acc.

label(Old, D) ->
    case dict:find(Old, D) of
	{ok,Val} -> Val;
	error -> throw({error,{undefined_label,Old}})
    end.
	    
