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


-ifndef(ssl_internal).
-define(ssl_internal, true).

%% basic binary constructors
-define(BOOLEAN(X),  X:8/unsigned-big-integer).
-define(BYTE(X),     X:8/unsigned-big-integer).
-define(UINT16(X),   X:16/unsigned-big-integer).
-define(UINT24(X),   X:24/unsigned-big-integer).
-define(UINT32(X),   X:32/unsigned-big-integer).
-define(UINT64(X),   X:64/unsigned-big-integer).
-define(STRING(X),   ?UINT32((size(X))), (X)/binary).

-define(byte(X),   << ?BYTE(X) >> ).
-define(uint16(X), << ?UINT16(X) >> ).
-define(uint24(X), << ?UINT24(X) >> ).
-define(uint32(X), << ?UINT32(X) >> ).
-define(uint64(X), << ?UINT64(X) >> ).

-define(CDR_MAGIC, "GIOP").
-define(CDR_HDR_SIZE, 12).

-define(DEFAULT_TIMEOUT, 5000).

%% Common enumerate values in for SSL-protocols 
-define(NULL, 0).
-define(TRUE, 0).
-define(FALSE, 1).

-define(DEFAULT_SUPPORTED_VERSIONS, [sslv3]). % TODO: This is temporary
%-define(DEFAULT_SUPPORTED_VERSIONS, ['tlsv1.1', tlsv1, sslv3]).

-record(ssl_options, {
	  verify,     % 
	  depth,      %
	  certfile,   %
	  keyfile,    %
	  key,	      %
	  password,   %
	  cacertfile, %
	  ciphers,    %
	  reuse_sessions, %
	  debug       %
	  }).

-record(socket_options, {
	  mode, 
	  packet,
	  header,
	  active
	 }).

-endif. % -ifdef(ssl_internal).





