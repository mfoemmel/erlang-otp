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
%% Purpose : Converts intermediate assembly code to final format.

-module(beam_flatten).

-export([module/2]).
-import(lists, [reverse/1, reverse/2, map/2]).

module({Mod,Exp,Attr,Fs,Lc}, Opt) ->
    {ok,{Mod,Exp,Attr,map(fun function/1, Fs),Lc}}.

function({function,Name,Arity,CLabel,Asm}) ->
    {function,Name,Arity,CLabel,block(Asm, [])}.

block([{block,Is0}|Is1], Acc) -> block(Is1, norm_block(Is0, Acc));
block([I|Is], Acc) -> block(Is, [I|Acc]);
block([], Acc) -> reverse(Acc).

norm_block([{allocate,R,Alloc}|Is], Acc) ->
    norm_block(Is, reverse(norm_allocate(Alloc, R), Acc));
norm_block([I|Is], Acc) -> norm_block(Is, [norm(I)|Acc]);
norm_block([], Acc) -> Acc.
    
norm({set,[D],As,{bif,N}})        -> {bif,N,nofail,As,D};
norm({set,[D],As,{bif,N,F}})      -> {bif,N,F,As,D};
norm({set,[D],[S],move})          -> {move,S,D};
norm({set,[D],[S],fmove})         -> {fmove,S,D};
norm({set,[D],[S],fconv})         -> {fconv,S,D};
norm({set,[D],[S1,S2],put_list})  -> {put_list,S1,S2,D};
norm({set,[D],[],{put_tuple,A}})  -> {put_tuple,A,D};
norm({set,[],[S],put})            -> {put,S};
norm({set,[D],[],{put_string,L,S}})       -> {put_string,L,S,D};
norm({set,[D],[S],{get_tuple_element,I}}) -> {get_tuple_element,S,I,D};
norm({set,[],[S,D],{set_tuple_element,I}}) -> {set_tuple_element,S,D,I};
norm({set,[D1,D2],[S],get_list})          -> {get_list,S,D1,D2};
norm({set,[],[],remove_message})   -> remove_message;
norm({set,[],[],fclearerror}) -> fclearerror;
norm({set,[],[],fcheckerror}) -> {fcheckerror,{f,0}};
norm({'%',_}=Comment)   -> Comment;
norm({'%live',R})                  -> {'%live',R}.

norm_allocate({Any,nostack,Nh,[]}, Regs) -> [{test_heap,Nh,Regs}];
norm_allocate({zero,0,Nh,[]}, Regs) -> norm_allocate({nozero,0,Nh,[]}, Regs);
norm_allocate({zero,Ns,0,[]}, Regs) -> [{allocate_zero,Ns,Regs}];
norm_allocate({zero,Ns,Nh,[]}, Regs) -> [{allocate_heap_zero,Ns,Nh,Regs}];
norm_allocate({nozero,Ns,0,Inits}, Regs) -> [{allocate,Ns,Regs}|Inits];
norm_allocate({nozero,Ns,Nh,Inits}, Regs) -> [{allocate_heap,Ns,Nh,Regs}|Inits].
