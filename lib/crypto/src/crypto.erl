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

%% Purpose : Main Crypto API module.

-module(crypto).

-export([start/0, stop/0, info/0]).
-export([md5/1, md5_init/0, md5_update/2, md5_final/1]).
-export([sha/1, sha_init/0, sha_update/2, sha_final/1]).
-export([md5_mac/2, md5_mac_96/2, sha_mac/2, sha_mac_96/2]).
-export([des_cbc_encrypt/3, des_cbc_decrypt/3, des_cbc_ivec/1]).
-export([des_ede3_cbc_encrypt/5, des_ede3_cbc_decrypt/5]).

-define(INFO,		 0).
-define(MD5,		 1).
-define(MD5_INIT,	 2).
-define(MD5_UPDATE,	 3).
-define(MD5_FINAL,	 4).
-define(SHA,		 5).
-define(SHA_INIT,	 6).
-define(SHA_UPDATE,	 7).
-define(SHA_FINAL,	 8).
-define(MD5_MAC,	 9).
-define(MD5_MAC_96,	 10).
-define(SHA_MAC,	 11).
-define(SHA_MAC_96,	 12).
-define(DES_CBC_ENCRYPT, 13).
-define(DES_CBC_DECRYPT, 14).
-define(DES_EDE3_CBC_ENCRYPT, 15).
-define(DES_EDE3_CBC_DECRYPT, 16).

-define(FUNC_LIST, [md5,
		    md5_init,
		    md5_update,
		    md5_final,
		    sha,
		    sha_init,
		    sha_update,
		    sha_final,
		    md5_mac,
		    md5_mac_96,
		    sha_mac,
		    sha_mac_96,
		    des_cbc_encrypt,
		    des_cbc_decrypt,
		    des_ede3_cbc_encrypt,
		    des_ede3_cbc_decrypt]).

start() ->
    application:start(crypto).

stop() ->
    application:stop(crypto).

info() ->
    lists:map(fun(I) -> lists:nth(I, ?FUNC_LIST) end, 
	      binary_to_list(control(?INFO, []))).

%% Below Key and Data are binaries or IO-lists. IVec is a binary.
%% Output is always a binary. Context is a binary.

%%
%%  MESSAGE DIGESTS
%%

%%
%%  MD5
%%
md5(Data) ->
    control(?MD5, Data).

md5_init() ->
    control(?MD5_INIT, []).

md5_update(Context, Data) ->
    control(?MD5_UPDATE, [Context, Data]).

md5_final(Context) ->
    control(?MD5_FINAL, Context).

%%
%% SHA
%%
sha(Data) ->
    control(?SHA, Data).

sha_init() ->
    control(?SHA_INIT, []).

sha_update(Context, Data) ->
    control(?SHA_UPDATE, [Context, Data]).

sha_final(Context) ->
    control(?SHA_FINAL, Context).

%%
%%  MESSAGE AUTHENTICATION CODES
%%

%%
%%  MD5_MAC
%%
md5_mac(Key, Data) ->
    control_bin(?MD5_MAC, Key, Data).

md5_mac_96(Key, Data) ->
    control_bin(?MD5_MAC_96, Key, Data).

%%
%%  SHA_MAC
%%
sha_mac(Key, Data) ->
    control_bin(?SHA_MAC, Key, Data).

sha_mac_96(Key, Data) ->
    control_bin(?SHA_MAC_96, Key, Data).

%%
%% CRYPTO FUNCTIONS
%%

%%
%% DES - in cipher block chaining mode (CBC)
%%
des_cbc_encrypt(Key, IVec, Data) ->
    control(?DES_CBC_ENCRYPT, [Key, IVec, Data]).

des_cbc_decrypt(Key, IVec, Data) ->
    control(?DES_CBC_DECRYPT, [Key, IVec, Data]).

%%
%% dec_cbc_ivec(Data) -> binary()
%%
%% Returns the IVec to be used in the next iteration of 
%% des_cbc_[encrypt|decrypt].
%%
des_cbc_ivec(Data) when binary(Data) -> 
    {_, IVec} = split_binary(Data, size(Data) - 8),
    IVec;
des_cbc_ivec(Data) when list(Data) ->
    des_cbc_ivec(list_to_binary(Data)).

%%
%% DES - in cipher block chaining mode (CBC)
%%
des_ede3_cbc_encrypt(Key1, Key2, Key3, IVec, Data) ->
    control(?DES_EDE3_CBC_ENCRYPT, [Key1, Key2, Key3, IVec, Data]).

des_ede3_cbc_decrypt(Key1, Key2, Key3, IVec, Data) ->
    control(?DES_EDE3_CBC_DECRYPT, [Key1, Key2, Key3, IVec, Data]).


%%
%%  LOCAL FUNCTIONS
%%
control_bin(Cmd, Key, Data) when binary(Key) ->
    control(Cmd, [sizehdr(size(Key)), Key, Data]);
control_bin(Cmd, Key, Data) when list(Key) ->
    control(Cmd, [sizehdr(flen(Key)), Key, Data]).

control(Cmd, Data) ->
    [{port, Port}| _] = ets:lookup(crypto_server_table, port),
    erlang:port_control(Port, Cmd, Data).

sizehdr(N) ->
    [(N bsr 24) band 255,
     (N bsr 16) band 255,
     (N bsr  8) band 255,
     N band 255].

%% Flat length of IOlist
flen(L) ->
    flen(L, 0).

flen([H| T], N) when list(H) ->
    flen(H, flen(T, N));
flen([H| T], N) when binary(H) ->
    flen(T, N + size(H));
flen([H| T], N) when integer(H), 0 =< H, H =<  255 ->
    flen(T, N + 1);
flen([], N) ->
    N.
