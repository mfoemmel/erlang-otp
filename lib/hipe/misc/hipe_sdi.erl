%%% -*- erlang-indent-level: 2 -*-
%%% $Id$
%%%
%%% An implementation of the algorithm described in:
%%% "Assembling Code for Machines with Span-Dependent Instructions",
%%% Thomas G. Szymanski, CACM 21(4), April 1978, pp. 300--308.
%%%
%%% Copyright (C) 2000, 2004  Mikael Pettersson

-module(hipe_sdi).
-export([pass1_init/0,
	 pass1_add_label/3,
	 pass1_add_sdi/4,
	 pass2/1]).

-include("hipe_sdi.hrl").

-record(label_data, {address, prevSdi}).

-record(sdi_data, {address, label, si}).

-record(pass1, {prevSdi, preS, labelMap}).

%%% "During the first pass we assign addresses to instructions
%%% and build a symbol table of labels and their addresses
%%% according to the minimum address assignment. We do this by
%%% treating each sdi as having its shorter length. We also
%%% number the sdi's [sic] from 1 to n in order of occurrence
%%% and record in the symbol table entry for each label the
%%% number of sdi's [sic] preceding it in the program.
%%% Simultaneously with pass 1 we build a set
%%% S = {(i,a,l,c) | 1 <= i <= n, a is the minimum address of
%%%	 the ith sdi, l and c, are the label and constant
%%%	 components of the operand of the ith sdi respectively}."
%%%
%%% Implementation notes:
%%% - We number the SDIs from 0 to n-1, not from 1 to n.
%%% - SDIs target only labels, so the constant offsets are omitted.
%%% - The set S is represented by a vector S[0..n-1] such that if
%%%   (i,a,l) is in the set, then S[i] = (a,l).
%%% - The symbol table maps a label to its minimum address and the
%%%   number of the last SDI preceding it (-1 if none).
%%% - To allow this module to make architecture-specific decisions
%%%   without using callbacks or making it architecture-specific,
%%%   the elements in the set S include a fourth component, SdiInfo,
%%%   supplied by the caller of this module.

pass1_init() ->
  #pass1{prevSdi=(-1), preS=[], labelMap=gb_trees:empty()}.

pass1_add_label(Pass1, Address, Label) ->
  #pass1{prevSdi=PrevSdi, labelMap=LabelMap} = Pass1,
  LabelData = #label_data{address=Address, prevSdi=PrevSdi},
  LabelMap2 = gb_trees:insert(Label, LabelData, LabelMap),
  Pass1#pass1{labelMap=LabelMap2}.

pass1_add_sdi(Pass1, Address, Label, SdiInfo) ->
  #pass1{prevSdi=PrevSdi, preS=PreS} = Pass1,
  SdiData = #sdi_data{address=Address, label=Label, si=SdiInfo},
  Pass1#pass1{prevSdi=PrevSdi+1, preS=[SdiData|PreS]}.

pass1_finalise(#pass1{prevSdi=PrevSdi, preS=PreS, labelMap=LabelMap}) ->
  {PrevSdi+1, vector_from_list(lists:reverse(PreS)), LabelMap}.

%%% Pass2.

pass2(Pass1) ->
  {N,SDIS,LabelMap} = pass1_finalise(Pass1),
  LONG = mk_long(N),
  SPAN = mk_span(N, SDIS, LabelMap),
  PARENTS = mk_parents(N, SDIS, LabelMap),
  update_long(N, SDIS, SPAN, PARENTS, LONG),
  {INCREMENT,CodeSizeIncr} = mk_increment(N, LONG),
  {adjust_label_map(LabelMap, INCREMENT), CodeSizeIncr}.

%%% "Between passes 1 and 2 we will construct an integer table
%%% LONG[1:n] such that LONG[i] is nonzero if and only if the
%%% ith sdi must be given a long form translation. Initially
%%% LONG[i] is zero for all i."
%%%
%%% Implementation notes:
%%% - LONG is an integer array indexed from 0 to N-1.

mk_long(N) ->
  array_mk(N, 0).

%%% "At the heart of our algorithm is a graphical representation
%%% of the interdependencies of the sdi's [sic] of the program.
%%% For each sdi we construct a node containing the empty span
%%% of that instruction. Nodes of this graph will be referred to
%%% by the number of the sdi to which they correspond. Directed
%%% arcs are now added to the graph so that i->j is an arc if
%%% and only if the span of the ith sdi depends on the size of
%%% the jth sdi, that is, the jth sdi lies between the ith sdi
%%% and the label occurring in its operand. It is easy to see
%%% that the graph we have just described can be constructed from
%%% the information present in the set S and the symbol table.
%%%
%%% The significance if this graph is that sizes can be assigned
%%% to the sdi's [sic] of the program so that the span of the ith
%%% sdi is equal to the number appearing in node i if and only if
%%% all the children of i can be given short translations."
%%%
%%% Implementation notes:
%%% - The nodes are represented by an integer array SPAN[0..n-1]
%%%   such that SPAN[i] contains the current span of sdi i.
%%% - Since the graph is traversed from child to parent nodes in
%%%   Step 3, the edges are represented by a vector PARENTS[0..n-1]
%%%   such that PARENTS[j] = { i | i is a parent of j }.

mk_span(N, SDIS, LabelMap) ->
  initSPAN(0, N, SDIS, LabelMap, array_mk(N, 0)).

initSPAN(SdiNr, N, SDIS, LabelMap, SPAN) ->
  if SdiNr >= N -> SPAN;
     true ->
      SdiData = vector_sub(SDIS, SdiNr),
      #sdi_data{address=SdiAddress, label=Label} = SdiData,
      LabelData = gb_trees:get(Label, LabelMap),
      #label_data{address=LabelAddress} = LabelData,
      SdiSpan = LabelAddress - SdiAddress,
      array_update(SPAN, SdiNr, SdiSpan),
      initSPAN(SdiNr+1, N, SDIS, LabelMap, SPAN)
  end.

mk_parents(N, SDIS, LabelMap) ->
  eachParent(0, N, SDIS, LabelMap, p_map_init(N)).

eachParent(SdiNr, N, SDIS, LabelMap, ParentsMap) ->
  if SdiNr >= N -> vector_from_list(gb_trees:values(ParentsMap));
     true ->
      SdiData = vector_sub(SDIS, SdiNr),
      #sdi_data{label=Label} = SdiData,
      LabelData = gb_trees:get(Label, LabelMap),
      #label_data{prevSdi=PrevSdi} = LabelData,
      {LO,HI} =		% inclusive
	if SdiNr =< PrevSdi -> {SdiNr+1, PrevSdi};	% forwards
	   true -> {PrevSdi+1, SdiNr-1}			% backwards
	end,
      eachParent(SdiNr+1, N, SDIS, LabelMap,
		 eachChild(LO, HI, SdiNr, ParentsMap))
  end.

eachChild(Child, HI, Parent, ParentsMap) ->
  if Child > HI -> ParentsMap;
     true -> eachChild(Child+1, HI, Parent,
		       addParent(ParentsMap, Parent, Child))
  end.

addParent(ParentsMap, Parent, Child) ->
  gb_trees:update(Child, [Parent | gb_trees:get(Child, ParentsMap)],
		  ParentsMap).

p_map_init(N) -> p_map_init(N-1, []).
p_map_init(N, OD) ->
  if N < 0 -> gb_trees:from_orddict(OD);
     true -> p_map_init(N-1, [{N,[]} | OD])
  end.

%%% "After the structure is built we process it as follows.
%%% For any node i whose listed span exceeds the architectural
%%% limit for a short form instruction, the LONG[i] equal to
%%% the difference between the long and short forms of the ith
%%% sdi. Increment the span of each parent of i by LONG[i] if
%%% the parent precedes the child in the program. Otherwise,
%%% decrement the span of the parent by LONG[i]. Finally, remove
%%% node i from the graph. Clearly this process must terminate.
%%% Any nodes left in the final graph correspond to sdi's [sic]
%%% which can be translated in the short form."
%%%
%%% Implementation notes:
%%% - We use a simple worklist algorithm, operating on a set
%%%   of SDIs known to require long form.
%%% - A node is removed from the graph by setting its span to zero.
%%% - The result is the updated LONG array. Afterwards, S, SPAN,
%%%   and PARENTS are no longer useful.

update_long(N, SDIS, SPAN, PARENTS, LONG) ->
  WKL = initWKL(N-1, SDIS, SPAN, []),
  processWKL(WKL, SDIS, SPAN, PARENTS, LONG).

initWKL(SdiNr, SDIS, SPAN, WKL) ->
  if SdiNr < 0 -> WKL;
     true ->
      SdiSpan = array_sub(SPAN, SdiNr),
      WKL2 = updateWKL(SdiNr, SDIS, SdiSpan, WKL),
      initWKL(SdiNr-1, SDIS, SPAN, WKL2)
  end.

processWKL([], _SDIS, _SPAN, _PARENTS, _LONG) -> [];
processWKL([Child|WKL], SDIS, SPAN, PARENTS, LONG) ->
  WKL2 = updateChild(Child, WKL, SDIS, SPAN, PARENTS, LONG),
  processWKL(WKL2, SDIS, SPAN, PARENTS, LONG).

updateChild(Child, WKL, SDIS, SPAN, PARENTS, LONG) ->
  case array_sub(SPAN, Child) of
    0 -> WKL;						% removed
    _ ->
      SdiData = vector_sub(SDIS, Child),
      Incr = sdiLongIncr(SdiData),
      array_update(LONG, Child, Incr),
      array_update(SPAN, Child, 0),			% remove child
      PS = vector_sub(PARENTS, Child),
      updateParents(PS, Child, Incr, SDIS, SPAN, WKL)
  end.

updateParents([], _Child, _Incr, _SDIS, _SPAN, WKL) -> WKL;
updateParents([P|PS], Child, Incr, SDIS, SPAN, WKL) ->
  WKL2 = updateParent(P, Child, Incr, SDIS, SPAN, WKL),
  updateParents(PS, Child, Incr, SDIS, SPAN, WKL2).

updateParent(Parent, Child, Incr, SDIS, SPAN, WKL) ->
  case array_sub(SPAN, Parent) of
    0 -> WKL;						% removed
    OldSpan ->
      NewSpan =
	if Parent < Child -> OldSpan + Incr;
	   true -> OldSpan - Incr
	end,
      array_update(SPAN, Parent, NewSpan),
      updateWKL(Parent, SDIS, NewSpan, WKL)
  end.

updateWKL(SdiNr, SDIS, SdiSpan, WKL) ->
  case sdiSpanIsShort(vector_sub(SDIS, SdiNr), SdiSpan) of
    true -> WKL;
    false -> [SdiNr|WKL]
  end.

sdiSpanIsShort(#sdi_data{si=#sdi_info{lb=LB,ub=UB}}, SdiSpan) ->
  SdiSpan >= LB andalso SdiSpan =< UB.

sdiLongIncr(#sdi_data{si=#sdi_info{incr=Incr}}) -> Incr.

%%% "Now construct a table INCREMENT[0:n] by defining
%%% INCREMENT[0] = 0 and INCREMENT[i] = INCREMENT[i-1]+LONG[i]
%%% for 1 <= i <= n. INCREMENT[i] represents the total increase
%%% in size of the first i sdi's [sic] in the program."
%%%
%%% Implementation notes:
%%% - INCREMENT is an integer vector indexed from 0 to n-1.
%%%   INCREMENT[i] = SUM(0 <= j <= i)(LONG[j]), for 0 <= i < n.
%%% - Due to the lack of an SML-like Array.extract operation,
%%%   INCREMENT is an array, not an immutable vector.

mk_increment(N, LONG) ->
  initINCR(0, 0, N, LONG, array_mk(N, 0)).

initINCR(SdiNr, PrevIncr, N, LONG, INCREMENT) ->
  if SdiNr >= N -> {INCREMENT, PrevIncr};
     true ->
      SdiIncr = PrevIncr + array_sub(LONG, SdiNr),
      array_update(INCREMENT, SdiNr, SdiIncr),
      initINCR(SdiNr+1, SdiIncr, N, LONG, INCREMENT)
  end.

%%% "At this point we can adjust the addresses of each label L
%%% in the symbol table. If L is preceded by i sdi's [sic] in
%%% the program, then add INCREMENT[i] to the value of L in the
%%% symbol table."
%%%
%%% Implementation notes:
%%% - Due to the 0..n-1 SDI numbering, a label L with address
%%%   a and previous sdi i is remapped to a+incr(i), where
%%%   incr(i) = if i < 0 then 0 else INCREMENT[i].

adjust_label_map(LabelMap, INCREMENT) ->
  applyIncr(gb_trees:to_list(LabelMap), INCREMENT, gb_trees:empty()).

applyIncr([], _INCREMENT, LabelMap) -> LabelMap;
applyIncr([{Label,LabelData}|List], INCREMENT, LabelMap) ->
  #label_data{address=Address, prevSdi=PrevSdi} = LabelData,
  Incr =
    if PrevSdi < 0 -> 0;
       true -> array_sub(INCREMENT, PrevSdi)
    end,
  applyIncr(List, INCREMENT,
	    gb_trees:insert(Label, Address+Incr, LabelMap)).

%%% ADT for immutable vectors, indexed from 0 to N-1.
%%% Currently implemented as tuples.
%%% Used for the 'SDIS' and 'PARENTS' vectors.

vector_from_list(Values) -> list_to_tuple(Values).
vector_sub(Vec, I) -> element(I+1, Vec).

%%% ADT for mutable integer arrays, indexed from 0 to N-1.
%%% Currently implemented as HiPE arrays.
%%% Used for the 'LONG', 'SPAN', and 'INCREMENT' arrays.

array_mk(N, Init) -> hipe_bifs:array(N, Init).
array_update(A, I, V) -> hipe_bifs:array_update(A, I, V).
array_sub(A, I) -> hipe_bifs:array_sub(A, I).
