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
%% Purpose : Annotates kernel expression with life-time information.

%% This module contains two passes.
%%
%% 1. The first pass traverses the code and annotates each kexpr with
%% used and new variables.  While this might seem unnecessary to do in
%% a seperate pass it simplifies the rest and saves much extra
%% traversing to do the same later.
%%
%% 2. The second pass works out variable lifetimes and annotates match
%% kexprs with variable database and index and other kexprs with just
%% their index. We solve the problem keeping track of variables in a
%% graph by numbering all kexprs in a branch. When we reach a
%% sub-branch we continue numbering the kexprs in the sub-branch
%% consecutively. In the main branch, however, the whole sub-branch
%% just receives one index which is used in the variable
%% database. With this scheme there are no problems with branches
%% continuing after a split into sub-branches, however, variables
%% occuring in sub-branches may not be allocated optimally.

-module(v2_life).

-export([module/2, vdb_find/2]).

-import(lists, [map/2,foldl/3,mapfoldl/3]).
-import(ordsets, [add_element/2,del_element/2,
		  intersection/2,union/1,union/2,subtract/2]).
%% ,is_element/2,union/1,union/2,intersection/1,intersection/2,subtract/2]).

-include("sys_life.hrl").

%% Annotated kernel expressions.
-record(a, {ke,an}).				%Annotated kexprs.

%% Variable usage annotations.
-record(u, {ke,us=[],ns=[]}).			%Variable usage annotation

-record(dummy, {}).

module({Mod,Exp,Attr,Forms}, Options) ->
    {Fs,St} = functions(Forms, Options, #dummy{}),
    {ok, {Mod,Exp,Attr,Fs}}.

functions(Forms, Options, St0) ->
    mapfoldl(fun (F, St) -> function(F, St) end, St0, Forms).

function({function,Name,As,B0}, St) ->
    %% io:fwrite("~p/~p~n", [Name,length(As)]),
    Arity = length(As),
    Ab = vusage_fun(B0),
    %%{{function,Name,Arity,As,Ab},St};
    {Vb,Vdb} = vlife_fun(Ab, As),
    {{function,Name,Arity,As,Vb,Vdb},St};
function({asm,Name,Arity,Code}, St) ->
    {{asm,Name,Arity,Code},St}.

%% -type vusage_fun(Kexpr) -> Ukexpr.
%%  Annotate the kexprs with used and new variables.  We place an
%%  annotation at every level in all kexprs.  The annotation is a
%%  wrapper and refers to the wrapped thing.

vusage_fun(Es) -> uexprs(Es).

%% -type uexprs([Kexpr]) -> [Ukexpr].

uexprs(Es) -> map(fun (E) -> uexpr(E) end, Es).

%% -type uexpr(Kexpr) -> annKexpr.

uexpr({scope,M,Rs}) ->
    Am = umatch(M),
    #u{ke={scope,Am,Rs},us=Am#u.us,ns=return_vars(Rs)};
uexpr({set,{var,V},Con}) ->
    #u{ke={set,{var,V},Con},us=con_vars(Con),ns=[V]};
uexpr({receive_loop,Te,{var,Rvar},Rm,Tes,Rs}) ->
    %% This is complicated as there are two separate internal structures.
    Tus = simple_vars([Te]),
    Arm = umatch(Rm),
    Ates = uexprs(Tes),
    Usarm = del_element(Rvar, Arm#u.us),
    Usates = subtract(used_in_any(Ates), new_in_any(Ates)),
    Used = union([Usarm,Usates,Tus]),
    #u{ke={receive_loop,Te,{var,Rvar},Arm,#u{ke=Ates,us=Usates},Rs},
       us=Used,ns=return_vars(Rs)};
uexpr(receive_next) -> #u{ke=receive_next,us=[],ns=[]};
uexpr(receive_accept) -> #u{ke=receive_accept,us=[],ns=[]};
uexpr(receive_reject) -> #u{ke=receive_reject,us=[],ns=[]};
uexpr({'catch',Es,{var,R}}) ->
    Ues = uexprs(Es),
    #u{ke={'catch',Ues,{var,R}},
       us=subtract(used_in_any(Ues), new_in_any(Ues)),
       ns=[R]};
uexpr({call,{var,V},As,Rs}) ->
    #u{ke={call,{var,V},As,Rs},us=simple_vars([{var,V}|As]),ns=return_vars(Rs)};
uexpr({call,F,As,Rs}) ->
    #u{ke={call,F,As,Rs},us=simple_vars(As),ns=return_vars(Rs)};
uexpr({enter,{var,V},As}) ->
    #u{ke={enter,{var,V},As},us=simple_vars([{var,V}|As]),ns=[]};
uexpr({enter,F,As}) ->
    #u{ke={enter,F,As},us=simple_vars(As),ns=[]};
uexpr({bif,F,As,Rs}) ->
    #u{ke={bif,F,As,Rs},us=simple_vars(As),ns=return_vars(Rs)};
uexpr({test,Test,As}) ->
    #u{ke={test,Test,As},us=simple_vars(As),ns=[]};
uexpr({return,Rs}) ->
    #u{ke={return,Rs},
       us=foldl(fun (R, Rvs) -> union(con_vars(R), Rvs) end, [], Rs),
       ns=[]};
uexpr({break,Bs}) ->
    #u{ke={break,Bs},
       us=foldl(fun (B, Bvs) -> union(con_vars(B), Bvs) end, [], Bs),
       ns=[]}.

umatch({try,F,S}) ->
    Af = umatch(F),
    As = umatch(S),
    #u{ke={try,Af,As},
       us=used_in_any([Af,As]),
       ns=[]};
umatch({select,{var,V},Scs}) ->
    Ascs = map(fun (S) -> uselect(S) end, Scs),
    #u{ke={select,{var,V},Ascs},
       us=add_element(V, used_in_any(Ascs)),
       ns=[]};
umatch({guard,Gcs}) ->
    Agcs = map(fun (G) -> uguard(G) end, Gcs),
    #u{ke={guard,Agcs},us=used_in_any(Agcs),ns=[]};
umatch({block,Es}) ->
    Ues = uexprs(Es),
    #u{ke={block,Ues},
       us=subtract(used_in_any(Ues), new_in_any(Ues)),
       ns=[]};
umatch({match_fail,{Reason,Term}}) ->
    Evs = con_vars(Term),
    #u{ke={match_fail,{Reason,Term}},us=Evs,ns=[]};
umatch({match_fail,E}) ->
    Evs = con_vars(E),
    #u{ke={match_fail,E},us=Evs,ns=[]}.

uselect({Type,Scs}) ->
    Ascs = map(fun (S) -> uselect1(S) end, Scs),
    #u{ke={Type,Ascs},
       us=used_in_any(Ascs),
       ns=[]}.

uselect1({sclause,Val,B}) ->
    Ab = umatch(B),
    #u{ke={sclause,Val,Ab},
       us=subtract(Ab#u.us, con_vars(Val)),
       ns=[]}.

uguard({gclause,G,B}) ->
    Ag = uexprs(G),
    Ab = umatch(B),
    #u{ke={gclause,Ag,Ab},
       us=subtract(union(used_in_any(Ag), Ab#u.us), new_in_any(Ag)),
       ns=[]}.

%% -type return_vars([var()]) -> [Var].
%% -type simple_vars([simple()]) -> [Var].
%% -type con_vars([constr()|simple()]) -> [Var].

return_vars(Rs) ->
    foldl(fun ({var,V}, Vs) -> add_element(V, Vs) end, [], Rs).

simple_vars(As) ->
    foldl(fun ({var,V}, Vs) -> add_element(V, Vs);
	      (Simple, Vs) -> Vs end, [], As).

con_vars({cons,Es}) -> simple_vars(Es);
con_vars({tuple,Es}) -> simple_vars(Es);
con_vars(Simple) -> simple_vars([Simple]).

%% used_in_any([Akexpr]) -> [Var].
%% new_in_any([Akexpr]) -> [Var].

used_in_any(Ues) ->
    foldl(fun (Ue, Us) -> union(Ue#u.us, Us) end, [], Ues).

new_in_any(Ues) ->
    foldl(fun (Ue, Ns) -> union(Ue#u.ns, Ns) end, [], Ues).

%% -type vlife_fun([Uexpr], [Arg]) -> {[Lkexpr],MaxI,Vdb}.
%%  Annotate each kexpr with an index and a variable database giving
%%  the first and last indexes for each variable which affects the
%%  kexpr.  The annotation is a wrapper and the variable database
%%  refers to the thing inside the wrapper.

vlife_fun(Ues, As) ->
    Vdb0 = foldl(fun ({var,V}, Vdb) -> new_var(V, 0, Vdb) end, [], As),
    {Les,MaxI,Vdb1} = vexprs(Ues, 1, Vdb0),
    {Les,Vdb1}.

%% vexpr(Ukexpr, I, Vdb) -> Lkexpr.
%%  Calculate variable lifetimes, annotate blocks and guard structures
%%  (these bottom out) with the "local" variable lifetime structure.
%%  Try sub-branches are counted as one op externally.

vexpr(Ue, I, Vdb) -> vexpr(Ue#u.ke, Ue#u.us, Ue#u.ns, I, Vdb).

vexpr({scope,M0,Rs}, Us, Ns, I, Vdb) ->
    %% Work out imported variables which need to be locked.
    Mdb0 = vlife_sub(I, I+1, Vdb),
    M1 = vmatch(M0, I+1, Us, Mdb0),
    #l{ke={scope,M1,Rs},i=I,vdb=use_vars(Us, I+1, Mdb0)};
vexpr({receive_loop,Te,{var,Rvar},Rm0,Tes0,Rs}, Us, Ns, I, Vdb) ->
    %% Work out imported variables which need to be locked.
    Rdb = vlife_sub(I, I+1, Vdb),
    %% This is complicated as there are two separate internal structures.
    %% First do the message matching expression.
    Rm1 = vmatch(Rm0, I+1, Us, new_var(Rvar, I, Rdb)),
    {Tes1,MaxI,Tdb} = vexprs(Tes0#u.ke, I+1, Rdb),
    #l{ke={receive_loop,Te,{var,Rvar},Rm1,#l{ke=Tes1,i=I+1,vdb=Tdb},Rs},
       i=I,vdb=use_vars(Us, I+1, Rdb)};
vexpr({'catch',Es0,{var,R}}, Us, Ns, I, Vdb) ->
    %% Lock variables that are alive before the catch and used afterwards.
    %% Don't lock variables that are only used inside the catch.
    %% Add catch tag 'variable'.
    Cdb0 = vlife_sub(I, I+1, Vdb),
    {Es1,Cmax,Cdb1} = vexprs(Es0, I+1, add_var({catch_tag,I}, I, 1000000, Cdb0)),
    #l{ke={'catch',Es1,{var,R}},i=I,vdb=Cdb1};
vexpr(E, Us, Ns, I, Vdb) -> #l{ke=E,i=I}.	%The rest are "flat"

%% vexprs([annExpr], Index, Vdb) -> {[Expr],MaxIndex,Vdb}.
%%  Go down the expression list building the variable lifetime
%%  structure as we go. Scope's and catches need to see the total
%%  variable lifetime structure so rest of list is done first.

vexprs([Ue|Ues], I, Vdb0) ->
    Vdb1 = new_vars(Ue#u.ns, I, use_vars(Ue#u.us, I, Vdb0)),
    {Les,MaxI,Vdb2} = vexprs(Ues, I+1, Vdb1),
    Le = vexpr(Ue, I, Vdb2),
    {[Le|Les],MaxI,Vdb2};
vexprs([], I, Vdb) -> {[],I,Vdb}.

%% vmatch(Ukmatch, I, [Lvar], Vdb) -> {Lkmatch,Vdb}.
%%  Calculate variable lifetimes through a match structure.  Increment
%%  the index at each level where something might happen.  This makes
%%  it easier to keep track of when variables come from.  This also
%%  makes it necessary to handle block explicitly.  Lock 'select'
%%  variables until we complete in a block.

vmatch(Ue, I, Lvs, Vdb) ->
    vmatch(Ue#u.ke, Ue#u.us, Ue#u.ns, I, Lvs, Vdb).

vmatch({try,F0,S0}, Us, Ns, I, Lvs, Vdb0) ->
    Vdb1 = use_vars(union(Us, Lvs), I+1, Vdb0),
    F = vmatch(F0, I+1, Lvs, Vdb1),
    S = vmatch(S0, I+1, Lvs, Vdb1),
    #l{ke={try,F,S},i=I,vdb=Vdb1};
vmatch({select,{var,V},Scs0}, Us, Ns, I, Lvs0, Vdb0) ->
    Lvs1 = add_element(V, Lvs0),		%Lock for safety!
    Vdb1 = use_vars(union(Us, Lvs1), I+1, Vdb0),
    Scs1 = map(fun (S) -> vselect(S, I+1, Lvs1, Vdb1) end, Scs0),
    #l{ke={select,{var,V},Scs1},i=I,vdb=Vdb1};
vmatch({guard,Gcs0}, Us, Ns, I, Lvs, Vdb0) ->
    Vdb1 = use_vars(union(Us, Lvs), I+1, Vdb0),
    Gcs1 = map(fun (G) -> vguard(G, I+1, Lvs, Vdb1) end, Gcs0),
    #l{ke={guard,Gcs1},i=I,vdb=Vdb1};
vmatch({block,Ues}, Us, Ns, I, Lvs, Vdb0) ->
    %% Make sure no select variables disappear to early.
    Vdb1 = use_vars(Lvs, I, Vdb0),
    {Les,MaxI,Vdb2} = vexprs(Ues, I+1, Vdb1),
    #l{ke={block,Les},i=I,vdb=Vdb2};
vmatch({match_fail,R}, Us, Ns, I, Lvs, Vdb0) ->
    Vdb1 = use_vars(union(Us, Lvs), I, Vdb0),
    #l{ke={match_fail,R},i=I,vdb=Vdb1}.

vselect(#u{ke={Type,Scs0},us=Us}, I, Lvs, Vdb0) ->
    Vdb1 = use_vars(union(Us, Lvs), I+1, Vdb0),
    Scs1 = map(fun (S) -> vselect1(S, I+1, Lvs, Vdb1) end, Scs0),
    #l{ke={Type,Scs1},i=I,vdb=Vdb1}.

vselect1(#u{ke={sclause,Val,B0},us=Us}, I, Lvs0, Vdb0) ->
    Pvs = con_vars(Val),
    Lvs1 = union(intersection(Pvs, B0#u.us), Lvs0), %Lock for safety!
    Vdb1 = use_vars(union(Us, Lvs1), I+1, new_vars(Pvs, I, Vdb0)),
    B1 = vmatch(B0, I+1, Lvs1, Vdb1),
    #l{ke={sclause,Val,B1},i=I,vdb=use_vars(B0#u.us, I+1, Vdb1)}.

vguard(#u{ke={gclause,G0,B0},us=Us}, I, Lvs, Vdb0) ->
    {G1,MaxG,Vdb1} = vexprs(G0, I+1, Vdb0),
    Vdb2 = use_vars(union(Us, Lvs), MaxG, Vdb1),
    B1 = vmatch(B0, MaxG, Lvs, Vdb2),
    #l{ke={gclause,G1,B1},i=I,vdb=use_vars(B0#u.us, MaxG, Vdb2)}.

%% new_var(VarName, I, Vdb) -> Vdb.
%% new_vars([VarName], I, Vdb) -> Vdb.
%% use_var(VarName, I, Vdb) -> Vdb.
%% use_vars([VarName], I, Vdb) -> Vdb.
%% add_var(VarName, F, L, Vdb) -> Vdb.
%%  Functions for accessing the variable lifetime database.  The
%%  database is an ordered list of triples {Var,First,last} and is
%%  managed by the functions vdb_find and vdb_store.

new_var(V, I, Vdb) ->
    case vdb_find(V, Vdb) of
	{V,F,L} when I < F -> vdb_store(V, I, L, Vdb);
	{V,F,L} -> Vdb;
	error -> vdb_store(V, I, I, Vdb)
    end.

new_vars(Vs, I, Vdb0) ->
    foldl(fun (V, Vdb) -> new_var(V, I, Vdb) end, Vdb0, Vs).

use_var(V, I, Vdb) ->
    case vdb_find(V, Vdb) of
	{V,F,L} when I > L -> vdb_store(V, F, I, Vdb);
	{V,F,L} -> Vdb;
	error -> vdb_store(V, I, I, Vdb)
    end.

use_vars(Vs, I, Vdb0) ->
    foldl(fun (V, Vdb) -> use_var(V, I, Vdb) end, Vdb0, Vs).

add_var(V, Fv, Lv, Vdb) ->
    use_var(V, Lv, new_var(V, Fv, Vdb)).

vdb_find(V, Vdb) ->
    %% Peformance note: Profiling shows that this function accounts for
    %% a lot of the execution time when huge constants terms are built.
    %% Using the BIF lists:keysearch/3 is a lot faster than the
    %% original Erlang version.
    case lists:keysearch(V, 1, Vdb) of
	{value,Vd} -> Vd;
	false -> error
    end.

%vdb_find(V, [Vd|Vdb]) when V < element(1, Vd) -> error;
%vdb_find(V, [Vd|Vdb]) when V == element(1, Vd) -> Vd;
%vdb_find(V, [Vd|Vdb]) when V > element(1, Vd) -> vdb_find(V, Vdb);
%vdb_find(V, []) -> error.

vdb_store(V, F, L, [Vd|Vdb]) when V < element(1, Vd) ->
    [{V,F,L},Vd|Vdb];
vdb_store(V, F, L, [Vd|Vdb]) when V == element(1, Vd) ->
    [{V,F,L}|Vdb];
vdb_store(V, F, L, [Vd|Vdb]) when V > element(1, Vd) ->
    [Vd|vdb_store(V, F, L, Vdb)];
vdb_store(V, F, L, []) -> [{V,F,L}].

%% -type vlife_sub(Min, Max, Vdb) -> Vdb.
%%  Make a new VDB suitable for a a sub-block by removing all
%%  variables not in use between Min and Max and definitely locking
%%  all variables that are allocated before Min and used after Max.

vlife_sub(Min, Max, Vdb) ->
    [ if L >= Max -> {V,F,1000000};		%Lock passed through var
	 true -> {V,F,L}			%Pass in
      end || {V,F,L} <- Vdb, F < Min, L >= Min ].
