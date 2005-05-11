%%=======================================================================
%% File        : hipe_gensym.erl
%% Author      : Eric Johansson, Kostis Sagonas
%% Description : Generates unique symbols and fresh numbers.
%%=======================================================================
%% $Id: hipe_gensym.erl,v 1.4 2004/02/07 16:14:30 kostis Exp $
%%=======================================================================
%% Notes: Written while we were in Montreal, Canada for PPDP-2000 as an
%%        exercise in Principles and Practice of Declarative Programming!
%%=======================================================================

-module(hipe_gensym).

-export([%% init/0, new_var/0, new_label/0,
	 %% update_lblrange/1, update_vrange/1, var_range/0, label_range/0,
	 set_var/1, get_var/0, get_next_var/0,
	 set_label/1, get_label/0, get_next_label/0]).
-export([init/1, new_var/1, new_label/1,
	 update_vrange/2, update_lblrange/2, var_range/1, label_range/1,
	 set_var/2, get_var/1, get_next_var/1,
	 set_label/2, get_label/1, get_next_label/1]).

%%-----------------------------------------------------------------------
%% 
%% 
%%-----------------------------------------------------------------------

%% init() ->
%%   put(var_count, 0),
%%   put(label_count, 0),
%%   put(var_min, 0),
%%   put(var_max, 0),
%%   put(lbl_min, 1),
%%   put(lbl_max, 1).

init(What) ->
  put({What,var_count}, 0),
  put({What,label_count}, 0),
  put({What,var_min}, 0),
  put({What,var_max}, 0),
  put({What,lbl_min}, 1),
  put({What,lbl_max}, 1).

%% new_var() ->
%%   V = get(var_count),
%%   put(var_count, V+1),
%%   V.

new_var(What) ->
  V = get({What,var_count}),
  put({What,var_count}, V+1),
  V.

%% new_label() ->
%%   L = get(label_count),
%%   put(label_count, L+1),
%%   L.

new_label(What) ->
  L = get({What,label_count}),
  put({What,label_count}, L+1),
  L.

%% update_vrange(V) ->
%%   Vmax = get(var_max),
%%   Vmin = get(var_min),
%%   put(var_min, lists:min([V, Vmin])),
%%   put(var_max, lists:max([V, Vmax])).

update_vrange(What,V) ->
  Vmax = get({What,var_max}),
  Vmin = get({What,var_min}),
  put({What,var_min}, lists:min([V, Vmin])),
  put({What,var_max}, lists:max([V, Vmax])).

%% update_lblrange(L) ->
%%   Lmax = get(lbl_max),
%%   Lmin = get(lbl_min),
%%   put(lbl_min, lists:min([L, Lmin])),
%%   put(lbl_max, lists:max([L, Lmax])).

update_lblrange(What,L) ->
  Lmax = get({What,lbl_max}),
  Lmin = get({What,lbl_min}),
  put({What,lbl_min}, lists:min([L, Lmin])),
  put({What,lbl_max}, lists:max([L, Lmax])).

%% var_range() ->
%%   {get(var_min), get(var_max)}.

var_range(What) ->
  {get({What,var_min}), get({What,var_max})}.

%% label_range() ->
%%   {get(lbl_min), get(lbl_max)}.

label_range(What) ->
  {get({What,lbl_min}), get({What,lbl_max})}.
 
%%-----------------------------------------------------------------------
%% Variable counter
%%-----------------------------------------------------------------------

set_var(X) ->
  put(var_max, X).
set_var(What,X) ->
  put({What,var_max}, X).

get_var() ->
  get(var_max).
get_var(What) ->
  get({What,var_max}).

get_next_var() ->
  C = get(var_max),
  put(var_max, C+1),
  C+1.
get_next_var(What) ->
  C = get({What,var_max}),
  put({What,var_max}, C+1),
  C+1.

%%-----------------------------------------------------------------------
%% Label counter
%%-----------------------------------------------------------------------

set_label(X) ->
  put(lbl_max, X).
set_label(What,X) ->
  put({What,lbl_max}, X).

get_label() ->
  get(lbl_max).
get_label(What) ->
  get({What,lbl_max}).

get_next_label() ->
  C = get(lbl_max),
  put(lbl_max, C+1),
  C+1.
get_next_label(What) ->
  C = get({What,lbl_max}),
  put({What,lbl_max}, C+1),
  C+1.

%%-----------------------------------------------------------------------
