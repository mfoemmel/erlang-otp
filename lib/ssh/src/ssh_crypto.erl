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

-module(ssh_crypto).

-export([
	 info/0,
	 rand_bytes/1,
	 rand_uniform/2,
	 mod_exp/3,
	 dss_verify/3,
	 rsa_verify/3,
	 aes128_cbc_encrypt/3,
	 aes128_cbc_decrypt/3
	]).

-define(INFO,			0).
-define(RAND_BYTES,     	1).
-define(RAND_UNIFORM,   	2).
-define(MOD_EXP,        	3).
-define(DSS_VERIFY,     	4).
-define(RSA_VERIFY,		5).
-define(AES128_CBC_ENCRYPT, 	6).
-define(AES128_CBC_DECRYPT, 	7).

-define(FUNC_LIST, [
		    rand_bytes,
                    rand_uniform,
                    mod_exp,
                    dss_verify,
		    rsa_verify,
                    aes128_cbc_encrypt,
                    aes128_cbc_decrypt
		   ]).

info() ->
    lists:map(fun(I) -> lists:nth(I, ?FUNC_LIST) end, 
              binary_to_list(control(?INFO, []))).

rand_bytes(Bytes) ->
    control(?RAND_BYTES,[<<Bytes:32/integer>>]).

rand_uniform(From,To) when binary(From), binary(To) ->
    case control(?RAND_UNIFORM,[From,To]) of
	<<Len:32/integer, MSB, Rest/binary>> when MSB > 127 ->
	    <<(Len + 1):32/integer, 0, MSB, Rest/binary>>;
	Whatever ->
	    Whatever
    end;
rand_uniform(From,To) when integer(From),integer(To) ->
    BinFrom = mpint(From),
    BinTo = mpint(To),
    case rand_uniform(BinFrom,BinTo) of
        Result when binary(Result) ->
            erlint(Result);
        Other ->
            Other
    end.

mod_exp(Base,Exponent,Modulo) ->
    case control(?MOD_EXP,[Base,Exponent,Modulo]) of
	<<Len:32/integer, MSB, Rest/binary>> when MSB > 127 ->
	    <<(Len + 1):32/integer, 0, MSB, Rest/binary>>;
	Whatever ->
	    Whatever
    end.

dss_verify(Dgst,Signature,Key) ->
    control(?DSS_VERIFY, [Dgst,Signature,Key]).

rsa_verify(Dgst,Signature,Key) ->
    control(?RSA_VERIFY, [Dgst,Signature,Key]).

%%
%% AES - with 128 bit key in cipher block chaining mode (CBC)
%%
aes128_cbc_encrypt(Key, IVec, Data) ->
    control(?AES128_CBC_ENCRYPT, [Key, IVec, Data]).

aes128_cbc_decrypt(Key, IVec, Data) ->
    control(?AES128_CBC_DECRYPT, [Key, IVec, Data]).

control(Cmd, Data) ->
    [{port, Port}| _] = ets:lookup(ssh_crypto_server_table, port),
    erlang:port_control(Port, Cmd, Data).

erlint(<<MPIntSize:32/integer,MPIntValue/binary>>) ->
    Bits= MPIntSize * 8,
    <<Integer:Bits/integer>> = MPIntValue,
    Integer.

mpint(ErlInteger) when integer(ErlInteger) ->
    Binary = int2bin(ErlInteger),
    mpint(Binary);
mpint(Binary) when binary(Binary) ->
    <<(size(Binary)):32/integer, Binary/binary>>.

int2bin(Integer) ->
    int2bin(Integer, []).

int2bin(0,List) ->
    list_to_binary(List);
int2bin(Integer,List) ->
    int2bin(Integer bsr 8, [Integer band 255|List]).
