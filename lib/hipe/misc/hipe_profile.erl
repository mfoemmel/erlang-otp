%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	   ANNOTATION OF PROGRAMS WITH PROFILE-INFORMATION
%
% Annotate CFGs on icode, RTL and sparc levels with counters.
% There are three versions (each takes CFG and const table Ctab):
%   blocks(CFG,Ctab): each block is given a counter
%   arcs(CFG,Ctab):   each arc is given a counter
%   events(CFG,Ctab):  maintain an event counter throughout CFG
%
% Returns:  {NewCFG, NewCtab, CountersAddrs, NumCtrs}
%
% *** UNFINISHED ***
% - at this time, ONLY SPARC level is provided
% - should also provide paths(CFG), which counts the paths through CFG
%   using the Ball-Larus algorithm.

-module(hipe_profile).
-export([blocks/2,
	 arcs/2,
	 events/2,
	 read_counters/1,
	 zero_counters/1,
	 compute_counters/1,
	 annot/1
	 ]).

-define(align,4).
%-define(align,8).
-define(elt_type,word).
%-define(elt_type,dword).
-define(counter_type,uw).
%-define(counter_type,ux).
-define(label_ty,constant).

-define(debug(Str,Args),ok).
%-define(debug(Str,Args),io:format(Str,Args)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Add a counter to each block.
%
% Returns { CFG, Ctab, CtrLabel, NumCtrs, Lst }
%   where
%    CFG is the instrumented CFG
%    Ctab is a constant table holding declared profile counters
%    CtrLabel is the label of the profile counters block
%    NumCtrs is the number of counters that was declared
%    Lst is a list of {Block_label, Counter_ID}

blocks(CFG,Ctab) ->
    ?debug('starting~n',[]),
    % Create two new temps:
    {Lo,Hi} = hipe_sparc_cfg:var_range(CFG),
    Ctr = hipe_sparc:mk_reg(Hi+1),
    Tmp = hipe_sparc:mk_reg(Hi+2),
    CFG1 = hipe_sparc_cfg:var_range_update(CFG,{Lo,Hi+2}),
    % Insert counters into CFG
    % 1. declare CtrLabel (could be done more elegantly)
    % 2. insert counters
    % 3. set CtrLabel to refer to the proper # of counters
    ?debug('block insert~n',[]),
    {Ctab1, CtrLabel } = 
	hipe_consttab:insert_global_block(Ctab,?align,?elt_type,
				     hipe_consttab:repeat(0,0)),
    ?debug('annot blocks~n',[]),
    {NumCtrs1, NewCFG, Lst} = block_counters(hipe_sparc_cfg:labels(CFG1),
					     CFG1,Ctr,CtrLabel,Tmp,0),
    NumCtrs = NumCtrs1-1,
    ?debug('update block~n',[]),
    NewCtab = hipe_consttab:update_global_block(Ctab,CtrLabel,?align,?elt_type,
					   hipe_consttab:repeat(NumCtrs1,0)),
    { NewCFG, NewCtab, CtrLabel, NumCtrs, Lst }.

block_counters(Ls,CFG,Ctr,CtrL,Tmp,N) ->
    block_counters(Ls,CFG,Ctr,CtrL,Tmp,N,[]).

block_counters([],CFG,Ctr,CtrL,Tmp,N,Ctrs_lst) -> {N,CFG,Ctrs_lst};
block_counters([L|Ls],CFG,Ctr,CtrL,Tmp,N,Ctrs) ->
    NewCFG = block_counter(CFG,L,Ctr,CtrL,Tmp,N),
    block_counters(Ls,NewCFG,Ctr,CtrL,Tmp,N+1,[{L,N}|Ctrs]).

block_counter(CFG,L,Ctr,CtrL,Tmp,N) ->
    Code = hipe_bb:code(hipe_sparc_cfg:bb(CFG,L)),
    NewCode = counter(Ctr,CtrL,Tmp,N,Code),
    NewBB = hipe_bb:mk_bb(NewCode),
    hipe_sparc_cfg:bb_update(CFG,L,NewBB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Instrument a CFG with profiling code that can count the probabilities
% of arcs as well as block frequencies. This is done by
% - splitting critical edges
% - inserting counters in all blocks
% - constructing arc expressions wherefrom we can easily reconstruct
%   block frequencies and arc probabilities.

arcs(CFG,Ctab) ->
    Ls = hipe_sparc_cfg:labels(CFG),
    {NewCFG,CritEs} = split_critical_edges(CFG),
    {InstrCFG,NewCtab,CtrL,NumCtrs,Ctr_lst} = blocks(NewCFG,Ctab),
    ArcExps = arc_exps(Ls,hipe_sparc_cfg:succ_map(InstrCFG),CritEs,Ctr_lst),
    {InstrCFG,NewCtab,CtrL,NumCtrs,Ctr_lst,ArcExps}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% An arc (m -> n) is critical if m has more than one successor and
% n has more than one predecessor. We split such arcs by inserting
% a new basic block k, s.t. (m -> k -> n).
%
% Returns {NewCFG, [{K,N}]}
%   where {M,K,N} signifies an arc (M->N) split into (M->K->N).
%   We say that this is a 'redirect of K to N'.

split_critical_edges(CFG) ->
    Succ = hipe_sparc_cfg:succ_map(CFG),
    Pred = hipe_sparc_cfg:pred_map(CFG),
    split_ce(hipe_sparc_cfg:labels(CFG),CFG,Succ,Pred).

split_ce(Ls,CFG,Succ,Pred) ->
    split_ce(Ls,CFG,Succ,Pred,[]).

split_ce([],CFG,Succ,Pred,CE) -> {CFG,CE};
split_ce([M|Ms],CFG,Succ,Pred,CE) ->
    {NewCFG,NewCE} = split_edges(M,CFG,Succ,Pred,CE),
    split_ce(Ms,NewCFG,Succ,Pred,NewCE).

split_edges(M,CFG,Succ,Pred,CE) ->
    case crit_pred(M,Succ) of
	true ->
	    split_es(M,hipe_sparc_cfg:succ(Succ,M),CFG,Pred,CE);
	false ->
	    {CFG,CE}
    end.

split_es(M,[],CFG,Pred,CE) -> {CFG,CE};
split_es(M,[N|Ns],CFG,Pred,CE) ->
    case crit_succ(N,Pred) of
	true ->
	    {K,NxtCFG} = next_label(CFG),
	    NewBB = hipe_bb:mk_bb([hipe_sparc:goto_create(M)]),
	    NxtCFG2 = hipe_sparc_cfg:bb_update(NxtCFG,K,NewBB),
	    NxtCFG3 = hipe_sparc_cfg:redirect_jmp(NxtCFG2,M,N,K),
	    split_es(M,Ns,NxtCFG3,Pred,[{K,N}|CE]);
	false ->
	    split_es(M,Ns,CFG,Pred,CE)
    end.

% get a fresh label

next_label(CFG) ->
    {Lo,Hi} = hipe_sparc_cfg:label_range(CFG),
    {Hi+1,hipe_sparc_cfg:label_range_update(CFG,{Lo,Hi+1})}.

% A critical predecessor has more than one successor

crit_pred(L,Succ) ->
    case length(hipe_sparc_cfg:pred(Succ,L)) of
	0 -> false;
	1 -> false;
	_ -> true
    end.

% A critical successor has more than one predecessor

crit_succ(L,Pred) ->
    case length(hipe_sparc_cfg:pred(Pred,L)) of
	0 -> false;
	1 -> false;
	_ -> true
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Given a modified CFG, a list of {K,N} redirects (K is a block
% splitting the critical edge going to N) and a list of per-block
% counters, we now compute profiling expressions for each block:
%   {L, {BlockFreq,[{SuccL,SuccProb}]}}
%
% Note: if L has 0 or 1 successors, the problem is easy ;-)
%
% Note: Ls is assumed to be the labels of the original CFG, while
%   Succ is the successors of the instrumented CFG; Redirects is a
%   list of {RedirecL,OldL} pairs and Ctrs is a list of {Label,Ctr} pairs.

arc_exps(Ls,Succ,Redirects,Ctrs) ->
    arc_exps_list(Ls,Succ,Redirects,Ctrs).

arc_exps_list(Ls,Succ,Redir,Ctrs) ->
    [ {L, arc_exp(L,hipe_sparc_cfg:succ(Succ,L),Redir,Ctrs)} || L <- Ls ].

arc_exp(L,Succ,Redir,Ctrs) ->
    C = ctr_of(L,Ctrs),
    { C, 
     [ {redirect_of(S,Redir),{divide,ctr_of(S,Ctrs),C}} || S <- Succ ]}.

ctr_of(L,[{L,C}|_]) -> C;
ctr_of(L,[_|Xs]) -> ctr_of(L,Xs);
ctr_of(L,[]) ->
    exit({no_counter_for,L}).

redirect_of(S,[]) -> S;
redirect_of(S,[{S,OldL}|_]) -> OldL;
redirect_of(S,[_|Xs]) -> redirect_of(S,Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Event profiling inserts code to read hardware specific counters
% and to maintain a count in an array of counters.
%
% Start counter:
%   reg = read_hw()
%   reg1 = get_hi(reg)
%   reg2 = get_lo(reg)
%
% Stop counter:
%   t1 = read_hw()
%   t2 = load_addr(ctr_array)
%   t3 = load(t2+C1)
%   t4 = get_hi(t1)
%   t4 = t4 - reg1
%   t3 = t3 + t4
%   store(t2+C1) = t3
%   t3 = load(t2+C2)
%   t4 = get_lo(t1)
%   t4 = t4 - reg2
%   t3 = t3 + t4
%   store(t2+C2) = t3

events(CFG,Ctab) ->
    % also, must declare the counter array as usual
    St = make_event_regs(),
    NewCFG = event_count_blocks(hipe_sparc_cfg:labels(CFG),CFG,St),
    start_counter(hipe_sparc_cfg:start(CFG),CFG,St).

make_event_regs() -> nyi.
start_counter(_,_,_) -> nyi.
stop_clock(_) -> nyi.

event_count_blocks([],CFG,St) -> CFG;
event_count_blocks([L|Ls],CFG,St) ->
    event_count_blocks(Ls,event_count_block(CFG,L,St),St).

event_count_block(CFG,L,St) ->
    BB = hipe_bb:code(hipe_sparc_cfg:bb(CFG,L)),
    NewBB = hipe_bb:mk_bb(ec_instrs(BB,St)),
    hipe_sparc_cfg:bb_update(CFG,L,NewBB).

ec_instrs([],St) -> [];
ec_instrs([I|Is],St) ->
    case ec_type(I) of
	none ->
	    [I|ec_instrs(Is,St)];
	stop_start_around ->
	    stop_clock(St) ++ [I] ++ start_clock(St) ++ ec_instrs(Is,St);
	stop_before ->
	    stop_clock(St) ++ [I] ++ ec_instrs(Is,St);
	start_before ->
	    start_clock(St) ++ [I] ++ ec_instrs(Is,St);
	stop_after ->
	    [I] ++ stop_clock(St) ++ ec_instrs(Is,St);
	start_after ->
	    [I] ++ start_clock(St) ++ ec_instrs(Is,St)
    end.

ec_type(I) ->
    case hipe_sparc:type(I) of
	call_link ->
	    stop_start_around;
	jmp ->
	    stop_after;
	_ ->
	    none
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Code to start and stop an event clock.
%
% Note: assumes T1-T4, Reg1, Reg2 are sparc vars,
%  C1, C2 are sparc immediates and CtrL a valid label.
%
% Note: The new registers that are introduced are not saved across
%  procedure calls. Thus, event counting must stop the clock before
%  each call, and start it again after the call.

start_clock({T1,T2,T3,T4,CtrL,Reg1,Reg2}) ->
    [read_pic(T1),
     get_hi(Reg1,T1),
     get_lo(Reg2,T1)
    ].

stop_clock({T1,T2,T3,T4,CtrL,Reg1,Reg2},C1,C2) ->
    [read_pic(T1),
     hipe_sparc:load_address_create(T2,CtrL,type,[]),
     hipe_sparc:load_create(T3,uw,T2,C1,[]),
     get_hi(T4,T3),
     hipe_sparc:alu_create(T4,T4,'-',Reg1,[]),
     hipe_sparc:store_create(T2,C1,uw,T4,[]),
     hipe_sparc:load_create(T3,uw,T2,C1,[]),
     get_lo(T4,T3),
     hipe_sparc:alu_create(T4,T4,'-',Reg2,[]),
     hipe_sparc:store_create(T2,C2,uw,T4,[])
    ].

read_pic(Dst) -> nyi.

get_hi(Dst,Src) -> % 64-bit shift >> 32 bits
    nyi.

get_lo(Dst,Src) -> % mask 32 low bits from 64 bits
    nyi.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Note: if Ctr is initialized to CtrL
%       - at each entry point and
%       - after each call
%  then we can simply use Ctr instead and avoid the load_address_create.
%  Saves some time.
%
% This can be done as follows:
% * walk Code, after each call_link perform load_address_create
% * ... and insert "Ctr := load_address(CtrL)" at all entrypoints

counter(Ctr,CtrL,Tmp,N,Code) ->
    Off = hipe_sparc:mk_imm(N*?align),
    [hipe_sparc:comment_create({counter,N},[]),
     hipe_sparc:load_address_create(Ctr,CtrL,?label_ty,[]),
     hipe_sparc:load_create(Tmp,?counter_type,Ctr,Off,[{counter,N}]),
     hipe_sparc:alu_create(Tmp,Tmp,'+',hipe_sparc:mk_imm(1),[]),
     hipe_sparc:store_create(Ctr,Off,?counter_type,Tmp,[{counter,N}]),
     hipe_sparc:comment_create({counter,N,done},[])
     | Code
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Collect profiling information
% - read_counters(MFA) returns a list of {Label, Number_of_visits}
% - compute_counters( {CounterList, ComputeList} ) computes the
%   values of implicit counters (defined as, say, {add,C1,C2} or
%   similar); returns [{Label,Counter}]
%   * note: the ComputeList can only refer to CounterList.

compute_counters({Ctrs,Comp}) ->
    [ {L, counter_value_of(E,Ctrs)} ||  {L,E} <- Comp ].

counter_value_of(N,Ctrs) when integer(N) -> find_value(N,Ctrs);
counter_value_of({add,E1,E2},Ctrs) ->
    counter_value_of(E1,Ctrs) + counter_value_of(E2,Ctrs);
counter_value_of({sub,E1,E2},Ctrs) ->
    counter_value_of(E1,Ctrs) - counter_value_of(E2,Ctrs);
counter_value_of({mul,E1,E2},Ctrs) ->
    counter_value_of(E1,Ctrs) * counter_value_of(E2,Ctrs).

find_value(Ctr,[{Ctr,Val}|_]) -> Val;
find_value(Ctr,[_|Xs]) -> find_value(Ctr,Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_counters(MFA) ->
  io:format("Geting counters for ~w\n",[MFA]),
    read_ctrs(MFA,counter_array(MFA),counter_info(MFA)).

read_ctrs(MFA,CtrL,Cs_lst) ->
    Ctrs = ncode_server:find_address(MFA,CtrL),
    [ {L, b32(erlang:hipe_peek(Ctrs+N*4))} || {L,N} <- Cs_lst ].

b32({Hi16,Lo16}) -> Hi16 bsl 16 + Lo16.

zero_counters(MFA) ->
    zero_ctrs(MFA,counter_array(MFA),counter_info(MFA)).

zero_ctrs(MFA,CtrL,Cs_lst) ->
    Ctrs = ncode_server:find_address(MFA,CtrL),
    zero_counters(Cs_lst,Ctrs).

zero_counters([],Ctrs) -> ok;
zero_counters([{_,N}|Ns],Ctrs) ->
    erlang:hipe_poke(Ctrs+N*4,hipe_converters:word_to_tuple(0)),
    zero_counters(Ns,Ctrs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Annotate the original CFG of an MFA with collected profiling info.

annot(MFA) ->
    case profiled_type(MFA) of
	block ->
	    CFG = saved_cfg(MFA),
	    CtrL = counter_array(MFA),
	    Ctrs_lst = counter_info(MFA),
	    Prof_info = read_ctrs(MFA,CtrL,Ctrs_lst),
	    annot_blocks(Prof_info,CFG);
	arc ->
	    CFG = saved_cfg(MFA),
	    CtrL = counter_array(MFA),
	    Ctrs_lst = counter_info(MFA),
	    Arc_exps = arc_expressions(MFA),
	    Prof_info = read_ctrs(MFA,CtrL,Ctrs_lst),
	    Prof_arcs = eval_arc_exps(Arc_exps,Prof_info),
	    { CFG, Prof_arcs };
	none ->
	    exit({not_compiled_with_profiling,MFA})
    end.

annot_blocks([],CFG) -> CFG;
annot_blocks([{L,Visits}|Xs],CFG) ->
    NewBB = hipe_bb:mk_bb([hipe_sparc:comment_create({visits,Visits},[])
		      | hipe_bb:code(hipe_sparc_cfg:bb(CFG,L))]),
    NewCFG = hipe_sparc_cfg:bb_update(CFG,L,NewBB),
    annot_blocks(Xs,NewCFG).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Evaluate arc expressions. Returns a list of
%   {Label, {Freq, [{SuccLabel,Prob}]}}

eval_arc_exps(AEs,Prof_info) ->
    [ eval_arc_exp(AE,Prof_info) || AE <- AEs ].

eval_arc_exp({L,C,SuccExps},Prof_info) ->
    {L, {freq(C,Prof_info),
	 [ {S, freq(C1,Prof_info)/freq(C2,Prof_info)}
	  || {S, {divide,C1,C2}} <- SuccExps ]}}.

freq(C,[{C,X}|_]) -> X;
freq(C,[_|Xs]) -> freq(C,Xs);
freq(C,[]) -> exit({eval_arc_exp,{counter_not_found,C}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Lookup a given attribute.

profiled_type(MFA) ->
    case lookup_info(MFA,profiled) of
	not_found ->
	    none;
	{found,arc} ->
	    arc;
	{found,block} ->
	    block
    end.

set_unprofiled(MFA) ->
    update_info(MFA,profiled,false).

set_profiled(MFA) ->
    update_info(MFA,profiled,true).

counter_array(MFA) ->
    get_info(MFA,counter_array).

set_counter_array(MFA,L) ->
    update_info(MFA,counter_array,L).

counter_info(MFA) ->
    get_info(MFA,counter_info).

set_counter_info(MFA,Lst) ->
    update_info(MFA,counter_info,Lst).

saved_cfg(MFA) ->
    get_info(MFA,saved_cfg).

set_saved_cfg(MFA,CFG) ->
    update_info(MFA,saved_cfg,CFG).

arc_expressions(MFA) ->
    get_info(MFA,arc_exprs).

set_arc_expressions(MFA,AEs) ->
    update_info(MFA,arc_exprs,AEs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Thanks to the interface from ncode_server,
% we have to resort to inefficient antics. Hopefully not on the 
% critical path :-)
%
% (ncode_server should use a set ets, rather than a bag ets.)

insert_info(MFA,Key,Info) ->
    case lookup_info(MFA,Key) of
	not_found ->
	    ncode_server:add_userinfo(MFA,Key,Info);
	{found,_} ->
	    exit({{insert_info,3},key_present})
    end.

update_info(MFA,Key,Info) ->
    case lookup_info(MFA,Key) of
	not_found ->
	    ok;
	{found,_} ->
	    ncode_server:delete_userinfo(MFA,Key)
    end,
    ncode_server:add_userinfo(MFA,Key,Info).

lookup_info(MFA,Key) ->
    case catch ncode_server:lookup_userinfo(MFA,Key) of
	{'EXIT',_} -> not_found;
	Val -> {found,Val}
    end.

get_info(MFA,Key) ->
    ncode_server:lookup_userinfo(MFA,Key).

% A rising scale

enum(0,N) -> [];
enum(K,N) when K > 0 ->
    [N|enum(K-1,N+1)].
