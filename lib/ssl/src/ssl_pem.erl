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

-module(ssl_pem).

%%% Purpose: Reading and writing of PEM type encoded files for SSL.

%% NB write_file/2 is only preliminary.

%% PEM encoded files have the following structure:
%%
%%	<text>
%%	-----BEGIN SOMETHING-----<CR><LF>
%%	<Base64 encoding line><CR><LF>
%%	<Base64 encoding line><CR><LF>
%%	...
%%	-----END SOMETHING-----<CR><LF>
%%	<text>
%%
%% A file can contain several BEGIN/END blocks. Text lines between
%% blocks are ignored.

-export([read_file/1, write_file/2]).

%% Read a PEM file and return each decoding as a binary. 

read_file(File) ->
    {ok, Fd} = file:open(File, [read]),
    Result = decode_file(Fd),
    file:close(Fd),
    Result.

decode_file(Fd) ->
    decode_file(Fd, [], [], notag).

decode_file(Fd, _RLs, Ens, notag) ->
    case io:get_line(Fd, "") of
	"-----BEGIN CERTIFICATE REQUEST-----" ++ _ ->
	    decode_file(Fd, [], Ens, cert_req);
	"-----BEGIN CERTIFICATE-----" ++ _ ->
	    decode_file(Fd, [], Ens, cert);
	"-----BEGIN RSA PRIVATE KEY-----" ++ _ ->
	    decode_file(Fd, [], Ens, rsa_private_key);
	eof ->
	    {ok, lists:reverse(Ens)};
	_ ->
	    decode_file(Fd, [], Ens, notag)
    end;
decode_file(Fd, RLs, Ens, Tag) ->
    case io:get_line(Fd, "") of
	"-----END" ++ _ ->			% XXX sloppy
	    Cs = lists:flatten(lists:reverse(RLs)), 
	    Bin = ssl_base64:join_decode(Cs),
	    decode_file(Fd, [], [{Tag, Bin}| Ens], notag);
	eof ->
	    {ok, lists:reverse(Ens)};
	L ->
	    decode_file(Fd, [L|RLs], Ens, Tag)
    end.

write_file(File, Ds) ->
    file:write_file(File, encode_file(Ds)).

encode_file(Ds) ->
    lists:map(
      fun({cert, Bin}) -> 
	      %% PKIX (X.509)
	      ["-----BEGIN CERTIFICATE-----\n",
	       ssl_base64:encode_split(Bin),
	       "-----END CERTIFICATE-----\n\n"];
	 ({cert_req, Bin}) -> 
	      %% PKCS#10
	      ["-----BEGIN CERTIFICATE REQUEST-----\n",
	       ssl_base64:encode_split(Bin),
	       "-----END CERTIFICATE REQUEST-----\n\n"];
	 ({rsa_private_key, Bin}) -> 
	      %% PKCS#?
	      ["XXX Following key assumed not encrypted\n",
	       "-----BEGIN RSA PRIVATE KEY-----\n",
	       ssl_base64:encode_split(Bin),
	       "-----END RSA PRIVATE KEY-----\n\n"]
      end, Ds).

    
