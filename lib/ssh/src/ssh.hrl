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

%%
%% SSH definitions
%%

-ifndef(SSH_HRL).
-define(SSH_HRL, 1).

-define(SSH_DEFAULT_PORT, 22).
-define(SSH_MAX_PACKET_SIZE, (256*1024)).

-define(FALSE, 0).
-define(TRUE,  1).
%% basic binary constructors
-define(BOOLEAN(X),  X:8/unsigned-big-integer).
-define(BYTE(X),     X:8/unsigned-big-integer).
-define(UINT16(X),   X:16/unsigned-big-integer).
-define(UINT32(X),   X:32/unsigned-big-integer).
-define(UINT64(X),   X:64/unsigned-big-integer).
-define(STRING(X),   ?UINT32((size(X))), (X)/binary).

%% building macros
-define(boolean(X),
	case X of
	    true -> <<?BOOLEAN(1)>>;
	    false -> (<<?BOOLEAN(0)>>)
	end).

-define(byte(X),   << ?BYTE(X) >> ).
-define(uint16(X), << ?UINT16(X) >> ).
-define(uint32(X), << ?UINT32(X) >> ).
-define(uint64(X), << ?UINT64(X) >> ).

-define(string(X),
	if list(X) ->
		(fun(__B) -> (<<?STRING(__B)>>) end)(list_to_binary(X));
	   atom(X) ->
		(fun(__B) -> (<<?STRING(__B)>>) end)(list_to_binary(atom_to_list(X)));
	   binary(X) ->
		(<<?STRING(X)>>)
	end).

-define(binary(X),
	<<?STRING(X)>>).

-ifdef(debug).
-define(dbg(Debug, Fmt, As),
	case (Debug) of
	    true ->
		io:format([$# | (Fmt)], (As));
	    _ ->
		ok
	end).
-else.
-define(dbg(Debug, Fmt, As), ok).
-endif.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% BASIC transport messages
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(SSH_MSG_DISCONNECT,             1).
-define(SSH_MSG_IGNORE,                 2).
-define(SSH_MSG_UNIMPLEMENTED,          3).
-define(SSH_MSG_DEBUG,                  4).
-define(SSH_MSG_SERVICE_REQUEST,        5).
-define(SSH_MSG_SERVICE_ACCEPT,         6).

-define(SSH_MSG_KEXINIT,                20).
-define(SSH_MSG_NEWKEYS,                21).


-record(ssh_msg_disconnect,
	{
	  code,         %% uint32
	  description,  %% string
	  language      %% string
	 }).

-record(ssh_msg_ignore,
	{
	  data          %% string
	 }).

-record(ssh_msg_unimplemented,
	{
	  sequence     %% uint32
	 }).

-record(ssh_msg_debug,
	{
	  always_display,  %% boolean
	  message,         %% string
	  language         %% string
	 }).


-record(ssh_msg_service_request,
	{
	  name     %% string (service name)
	 }).

-record(ssh_msg_service_accept,
	{
	  name     %% string
	 }).

-record(ssh_msg_kexinit,
	{
	  cookie,                                   %% random(16)
	  kex_algorithms,                           %% string
	  server_host_key_algorithms,               %% string    
	  encryption_algorithms_client_to_server,   %% string    
	  encryption_algorithms_server_to_client,   %% string    
	  mac_algorithms_client_to_server,          %% string
	  mac_algorithms_server_to_client,          %% string    
	  compression_algorithms_client_to_server,  %% string
	  compression_algorithms_server_to_client,  %% string
	  languages_client_to_server,               %% string
	  languages_server_to_client,               %% string
	  first_kex_packet_follows=false,           %% boolean
	  %% (reserved for future extension)
	  reserved=0                                %% uint32=0
	 }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% KEY DH messages
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% diffie-hellman-group1-sha1
-define(SSH_MSG_KEXDH_INIT,  30).
-define(SSH_MSG_KEXDH_REPLY,  31).

-record(ssh_msg_kexdh_init,
	{
	  e  %% mpint
	 }).

-record(ssh_msg_kexdh_reply,
	{
	  public_host_key,  %% string (K_S)
	  f,                %% mpint
	  h_sig             %% string, signature of H
	 }).

-record(ssh_msg_newkeys,
	{}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% KEY DH GEX messages
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% diffie-hellman-group-exchange-sha1
-define(SSH_MSG_KEX_DH_GEX_REQUEST_OLD, 30).
-define(SSH_MSG_KEX_DH_GEX_REQUEST,     34).
-define(SSH_MSG_KEX_DH_GEX_GROUP,       31).
-define(SSH_MSG_KEX_DH_GEX_INIT,        32).
-define(SSH_MSG_KEX_DH_GEX_REPLY,       33).

-record(ssh_msg_kex_dh_gex_request,
	{
	  min,
	  n,
	  max
	 }).

-record(ssh_msg_kex_dh_gex_request_old,
	{
	  n
	 }).

-record(ssh_msg_kex_dh_gex_group,
	{
	  p,  %% prime
	  g   %% generator
	 }).

-record(ssh_msg_kex_dh_gex_init,
	{
	  e
	 }).

-record(ssh_msg_kex_dh_gex_reply,
	{
	  public_host_key,  %% string (K_S)
	  f,
	  h_sig
	 }).


-define(SSH_CIPHER_NONE, 0).
-define(SSH_CIPHER_3DES, 3).
-define(SSH_CIPHER_AUTHFILE, ?SSH_CIPHER_3DES).


-record(ssh_key,
	{
	  type,
	  public,
	  private,
	  comment = ""
	 }).

%% assertion macro
-define(ssh_assert(Expr, Reason),
	case Expr of
	    true -> ok;
	    _ -> exit(Reason)
	end).

%% error codes
-define(SSH_DISCONNECT_HOST_NOT_ALLOWED_TO_CONNECT,   1).
-define(SSH_DISCONNECT_PROTOCOL_ERROR,   2).
-define(SSH_DISCONNECT_KEY_EXCHANGE_FAILED,   3).
-define(SSH_DISCONNECT_RESERVED,   4).
-define(SSH_DISCONNECT_MAC_ERROR,   5).
-define(SSH_DISCONNECT_COMPRESSION_ERROR,   6).
-define(SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,   7).
-define(SSH_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED,   8).
-define(SSH_DISCONNECT_HOST_KEY_NOT_VERIFIABLE,   9).
-define(SSH_DISCONNECT_CONNECTION_LOST,  10).
-define(SSH_DISCONNECT_BY_APPLICATION,  11).
-define(SSH_DISCONNECT_TOO_MANY_CONNECTIONS,  12).
-define(SSH_DISCONNECT_AUTH_CANCELLED_BY_USER,  13).
-define(SSH_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE,  14).
-define(SSH_DISCONNECT_ILLEGAL_USER_NAME,  15).

%%%----------------------------------------------------------------------
%%% #   DH_14_xxx
%%% Description: Oakley group 14 prime numbers and generator. Used in
%%%              diffie-hellman-group1-sha1 key exchange method.
%%%----------------------------------------------------------------------
%%%----------------------------------------------------------------------
%%% #   DH_14_P
%%% Description: Prime for this group
%%%----------------------------------------------------------------------

-define(DH_14_P,
	<<000,000,000,129,000,255,255,255,255,255,255,255,255,201,015,218,
	  162,033,104,194,052,196,198,098,139,128,220,028,209,041,002,078,
	  008,138,103,204,116,002,011,190,166,059,019,155,034,081,074,008,
	  121,142,052,004,221,239,149,025,179,205,058,067,027,048,043,010,
	  109,242,095,020,055,079,225,053,109,109,081,194,069,228,133,181,
	  118,098,094,126,198,244,076,066,233,166,055,237,107,011,255,092,
	  182,244,006,183,237,238,056,107,251,090,137,159,165,174,159,036,
	  017,124,075,031,230,073,040,102,081,236,230,083,129,255,255,255,
	  255,255,255,255,255>>).

%%%----------------------------------------------------------------------
%%% #   DH_14_G
%%% Description: Generator for DH_14_P.
%%%----------------------------------------------------------------------

-define(DH_14_G, <<0,0,0,1,2>>).

%%%----------------------------------------------------------------------
%%% #   DH_14_Q
%%% Description: Group order (DH_14_P - 1) / 2.
%%%----------------------------------------------------------------------

-define(DH_14_Q,
	<<000,000,000,128,127,255,255,255,255,255,255,255,228,135,237,081,
	  016,180,097,026,098,099,049,069,192,110,014,104,148,129,039,004,
	  069,051,230,058,001,005,223,083,029,137,205,145,040,165,004,060,
	  199,026,002,110,247,202,140,217,230,157,033,141,152,021,133,054,
	  249,047,138,027,167,240,154,182,182,168,225,034,242,066,218,187,
	  049,047,063,099,122,038,033,116,211,027,246,181,133,255,174,091,
	  122,003,091,246,247,028,053,253,173,068,207,210,215,079,146,008,
	  190,037,143,243,036,148,051,040,246,115,041,192,255,255,255,255,
	  255,255,255,255>>).

%%%----------------------------------------------------------------------
%%% #   SSH_PORT
%%% Description: The default server port to connect to.
%%%----------------------------------------------------------------------

-define(SSH_PORT, 22).

%%%----------------------------------------------------------------------
%%% #   SSH_VERSION
%%% Description: The SSH transport protocol version we implement.
%%%----------------------------------------------------------------------

-define(SSH_VERSION, "2.0").

%%%----------------------------------------------------------------------
%%% #   COMPATIBLE_VERSIONS
%%% Description: The SSH transport protocol versions compatible with our
%%%              version.
%%%----------------------------------------------------------------------

-define(SSH_COMPATIBLE_VERSIONS, ["2.0","1.99"]).

%%%----------------------------------------------------------------------
%%% #   SOFTWARE_VERSION
%%% Description: The SSH transport protocol versions compatible with our
%%%              version.
%%%----------------------------------------------------------------------

-define(SSH_SOFTWARE_VERSION, "Erlang/OTP").

%%%----------------------------------------------------------------------
%%% #   SSH_VERSION_STRING
%%% Description: The string we send when connecting to SSH server.
%%%----------------------------------------------------------------------

-define(SSH_VERSION_STRING,
	"SSH-" ++ ?SSH_VERSION ++ "-" ++ ?SSH_SOFTWARE_VERSION).

%%%----------------------------------------------------------------------
%%% #   SSH_LINE_TERM
%%% Description: This is what we use to terminate lines in our version of
%%%              the SSH transport protocol.
%%%----------------------------------------------------------------------

-define(SSH_LINE_TERM, "\r\n").

%%%----------------------------------------------------------------------
%%% #   SSH_FALSE SSH_TRUE
%%% Description: The representation of boolean values in the SSH protocol.
%%%----------------------------------------------------------------------

-define(SSH_FALSE, 0).
-define(SSH_TRUE, 1).

%%%----------------------------------------------------------------------
%%% #   SSH_ALG_xxx
%%% Description: The names of algorithms we support.
%%%----------------------------------------------------------------------

-define(SSH_ALG_KEX_DH_GROUP1,	"diffie-hellman-group1-sha1").
-define(SSH_ALG_PUB_KEY_DSS,	"ssh-dss").
-define(SSH_ALG_PUB_KEY_RSA,	"ssh-rsa").
-define(SSH_ALG_HMAC_SHA1,	"hmac-sha1").
-define(SSH_ALG_HMAC_MD5,	"hmac-md5").
-define(SSH_ALG_3DES_CBC,	"3des-cbc").
-define(SSH_ALG_AES128_CBC,	"aes128-cbc").

%%%----------------------------------------------------------------------
%%% #2.2   DEFINITION OF RECORDS
%%%----------------------------------------------------------------------

-record(sftp_file_attrs, {size =	'_',
			  uid =		'_',
			  gid =		'_',
			  permissions = '_',
			  atime =	'_',
			  mtime =	'_',
			  extended =	[]}).
-record(ssh_pty, {term = "", % e.g. "xterm"
		  width = 80,
		  height = 25,
		  pixel_width = 1024,
		  pixel_height = 768,
		  modes = <<>>}).

%% -record(ssh_key, {type =     '_',
%% 		  public =   '_',
%% 		  private =  '_',
%% 		  comment = ""}).

%%%----------------------------------------------------------------------
%%% #2.3   DEFINITION OF MACROS
%%%----------------------------------------------------------------------

-define(SSH_STRING(String),
	[<<(length(String)):32/integer>>, 
	 list_to_binary(String)]).

-define(SSH_UINT_32(Integer), <<(Integer):32/integer>>).

-endif. % SSH_HRL defined
