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
-module(erl_open_port).

-export([open_port/2]).

%%% This module is a wrapper between the user-visible open_port/2
%%% (defined in erlang.erl) and the BIF open_port_prim/2 (which used
%%% to be open_port/2 itself). The options 'cd' and 'env' are picked
%%% up, and transformed into a format which is easier to handle
%%% for open_port_prim/2 (namely, binaries). The OS-dependent
%%% handling (sorted/nonsorted environment, and case sensitivity)
%%% is also done here.

%%% All option parsing and checking could be done here, and not
%%% in open_port_prim/2, which would simplify the C code
%%% considerably.

merge_env([], Env) ->
    Env;
merge_env([{Name, false} | Rest], Env) ->
    Env1 = delete_env_var(Name, Env),
    merge_env(Rest, Env1);
merge_env([{Name, Val} | Rest], Env) ->
    Env1 = change_env_var(Name, Val, Env),
    merge_env(Rest, Env1).

delete_env_var(Name, []) ->
    [];
delete_env_var(Name, [Def | Rest]) ->
    case match(Name, Def) of
	true ->
	    Rest;
	_ ->
	    [Def | delete_env_var(Name, Rest)]
    end.

combine(Name, Val) ->
    Name ++ "=" ++ Val.

change_env_var(Name, Val, []) ->
    [combine(Name, Val)];
change_env_var(Name, Val, [Def | Rest]) ->
    case match(Name, Def) of
	true ->
	    [combine(Name, Val) | Rest];
	_ ->
	    case precedes(Name, Def) of
		true ->
		    [combine(Name, Val) , Def | Rest];
		_ ->
		    [Def | change_env_var(Name, Val, Rest)]
	    end
    end.

single_os_type() ->
    X = os:type(),
    case X of
	{Sys, _} -> Sys;
	_ -> X
    end.

%%% Currently, we don't handle these options for VxWorks, so
%%% crash here.
match(Name, Def) ->
    case single_os_type() of
	win32 ->
	    uc_lc_match(Name, Def);
	unix ->
	    strict_match(Name, Def)
    end.


%%% ASCII only. Should support Latin-1 too?
tolower(C) when C >= $A, C =< $Z ->
    C + 32;
tolower(C) ->
    C.

uc_lc_equal(L, L) ->
    true;
uc_lc_equal(L1, L2) ->
    tolower(L1) == tolower(L2).

uc_lc_match([], [$= | _]) ->
    true;
uc_lc_match([L1 | Rest1], [L2 | Rest2]) ->
    case uc_lc_equal(L1, L2) of
	true ->
	    uc_lc_match(Rest1, Rest2);
	_ ->
	    false
    end;
uc_lc_match(_, _) ->
    false.

strict_match([], [$= | _]) ->
    true;
strict_match([L | Rest1], [L | Rest2]) ->
    strict_match(Rest1, Rest2);
strict_match(_, _) ->
    false.

precedes(Name, Def) ->
    case single_os_type() of
	win32 ->
	    uc_lc_precedes(Name, Def);
	unix ->
	    false
    end.

%%% true also when equal
uc_lc_precedes([], _) ->
    true;
uc_lc_precedes(_, [$= | _]) ->
    false;
uc_lc_precedes([L1 | Rest1], [L2 | Rest2]) ->
    LL1 = tolower(L1),
    LL2 = tolower(L2),
    if
	LL1 =< LL2 ->
	    uc_lc_precedes(Rest1, Rest2);
	true ->
	    false
    end.

%%%------------------------------------------------------------
%%% Build a binary containing all the strings.

make_binary(List) ->
    list_to_binary(add_nul_chars(List)).

add_nul_chars([]) ->
    [[0]];
add_nul_chars([Str | Rest]) ->
    [Str, [0] | add_nul_chars(Rest)].

%%%------------------------------------------------------------

open_port({spawn, Cmd}, Settings0) ->
    Settings = transform_settings(Settings0),
    erlang:open_port_prim({spawn, Cmd}, Settings);
open_port(Name, Settings) ->
    erlang:open_port_prim(Name, Settings).

transform_settings([]) ->
    [];
transform_settings([Option | Rest]) ->
    [transform_one(Option) | transform_settings(Rest)].

transform_one({cd, Dir}) ->
    Bin = list_to_binary(Dir ++ [0]),
    {cd, Bin};
transform_one({env, List}) ->
    Env = lists:reverse(erlang:info(getenv)),
    Bin = make_binary(merge_env(List, Env)),
    {env, Bin};
transform_one(X) ->
    X.
