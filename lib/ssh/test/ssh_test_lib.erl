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
%%----------------------------------------------------------------------
-module(ssh_test_lib).

%% Various utilities

-export([expect_msg/0, one_of/4, one_of/6, copyfile/3,
	 get_id_keys/1, remove_id_keys/1, rx/2,
	 %%collect_data/1, collect_data/2,
	 collect_data_upto/2]).

expect_msg() ->
    receive
	Msg ->
	    Msg
    after 1000 ->
	    timeout
    end.

one_of(A, B, A, B) ->
    ok;
one_of(A, B, B, A) ->
    ok.

one_of(A, B, C, A, B, C) ->
    ok;
one_of(A, B, C, A, C, B) ->
    ok;
one_of(A, B, C, B, A, C) ->
    ok;
one_of(A, B, C, B, C, A) ->
    ok;
one_of(A, B, C, C, A, B) ->
    ok;
one_of(A, B, C, C, B, A) ->
    ok.

%% copy private keys to given dir from ~/.ssh
get_id_keys(DstDir) ->
    SrcDir = filename:join(os:getenv("HOME"), ".ssh"),
    RsaOk = copyfile(SrcDir, DstDir, "id_rsa"),
    DsaOk = copyfile(SrcDir, DstDir, "id_dsa"),
    case {RsaOk, DsaOk} of
	{{ok, _}, {ok, _}} -> {ok, both};
	{{ok, _}, _} -> {ok, rsa};
	{_, {ok, _}} -> {ok, dsa};
	{Error, _} -> Error
    end.

remove_id_keys(Dir) ->
    file:delete(filename:join(Dir, "id_rsa")),
    file:delete(filename:join(Dir, "id_dsa")).

copyfile(SrcDir, DstDir, Fn) ->
    file:copy(filename:join(SrcDir, Fn),
	      filename:join(DstDir, Fn)).

cr_lf_2_cr(L) ->
    cr_lf_2_cr(L, []).

cr_lf_2_cr([], Acc) ->
    lists:reverse(Acc);
cr_lf_2_cr([$\r, $\n | Rest], Acc) ->
    cr_lf_2_cr(Rest, [$\n | Acc]);
cr_lf_2_cr([C | Rest], Acc) ->
    cr_lf_2_cr(Rest, [C | Acc]).

rx(Data, Rx) ->
    D = cr_lf_2_cr(Data),
    case regexp:match(D, Rx) of
 	{match, _Pos, _Length} ->
 	     ok;
	E ->
	    io:format("rx: D=~p Rx=~p E=~p\n", [D, Rx, E]),
	    ok = D
    end.


%% collect_data(Port) ->
%%     collect_data(Port, "").

%% collect_data(Port, Acc) ->
%%     receive
%% 	{Port, {data, Data}} ->
%% 	    collect_data(Port, Acc ++ Data)
%%     after 1000 ->
%% 	    Acc
%%     end.	    

collect_data_upto(Port, Upto) ->
    collect_data_upto(Port, Upto, "").

collect_data_upto(Port, Upto, Acc) ->
    receive
	{Port, {data, Data}} ->
	    D = Acc ++ Data,
	    case lists:suffix(Upto, D) of
		true ->
		    D;
		false ->
		    collect_data_upto(Port, Upto, D)
	    end
    after 5000 ->
	    {timeout, Acc}
    end.
