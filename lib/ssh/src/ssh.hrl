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

-ifndef(SSH_HRL).
-define(SSH_HRL, 1).

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
%%% #   SSH_MSG_xxx
%%% Description: Packet types in the SSH transport protocol.
%%%              30-49 are for key exchange packets.
%%%----------------------------------------------------------------------

-define(SSH_MSG_DISCONNECT, 	1).
-define(SSH_MSG_IGNORE, 	2).
-define(SSH_MSG_UNIMPLEMENTED, 	3).
-define(SSH_MSG_DEBUG, 		4).
-define(SSH_MSG_SERVICE_REQUEST,5).
-define(SSH_MSG_SERVICE_ACCEPT,	6).
-define(SSH_MSG_KEXINIT,	20).
-define(SSH_MSG_NEWKEYS,	21).

-define(SSH_MSG_KEXDH_INIT,	30).
-define(SSH_MSG_KEXDH_REPLY,	31).

%%%----------------------------------------------------------------------
%%% #   SSH_DISCONNECT_xxx
%%% Description: Reason codes for SSH_MSG_DISCONNECT packets.
%%%----------------------------------------------------------------------

-define(SSH_DISCONNECT_HOST_NOT_ALLOWED_TO_CONNECT,      1).
-define(SSH_DISCONNECT_PROTOCOL_ERROR,                   2).
-define(SSH_DISCONNECT_KEY_EXCHANGE_FAILED,              3).
-define(SSH_DISCONNECT_RESERVED,                         4).
-define(SSH_DISCONNECT_MAC_ERROR,                        5).
-define(SSH_DISCONNECT_COMPRESSION_ERROR,                6).
-define(SSH_DISCONNECT_SERVICE_NOT_AVAILABLE,            7).
-define(SSH_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED,   8).
-define(SSH_DISCONNECT_HOST_KEY_NOT_VERIFIABLE,          9).
-define(SSH_DISCONNECT_CONNECTION_LOST,                 10).
-define(SSH_DISCONNECT_BY_APPLICATION,                  11).
-define(SSH_DISCONNECT_TOO_MANY_CONNECTIONS,            12).
-define(SSH_DISCONNECT_AUTH_CANCELLED_BY_USER,          13).
-define(SSH_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE,  14).
-define(SSH_DISCONNECT_ILLEGAL_USER_NAME,               15).

%%%----------------------------------------------------------------------
%%% #   SSH_MSG_USERAUTH_xxx
%%% Description: Packet types used by the user authentication protocol.
%%%----------------------------------------------------------------------

-define(SSH_MSG_USERAUTH_REQUEST,		50).
-define(SSH_MSG_USERAUTH_FAILURE,		51).
-define(SSH_MSG_USERAUTH_SUCCESS,		52).
-define(SSH_MSG_USERAUTH_BANNER,		53).

-define(SSH_MSG_USERAUTH_PK_OK,			60).

-define(SSH_MSG_USERAUTH_PASSWD_CHANGEREQ,	60).

%%%----------------------------------------------------------------------
%%% #   SSH_MSG_xxx
%%% Description: Packet types used by the connection protocol.
%%%----------------------------------------------------------------------

-define(SSH_MSG_GLOBAL_REQUEST,			80).
-define(SSH_MSG_REQUEST_SUCCESS,		81).
-define(SSH_MSG_REQUEST_FAILURE,		82).
-define(SSH_MSG_CHANNEL_OPEN,			90).
-define(SSH_MSG_CHANNEL_OPEN_CONFIRMATION,	91).
-define(SSH_MSG_CHANNEL_OPEN_FAILURE,		92).
-define(SSH_MSG_CHANNEL_WINDOW_ADJUST,		93).
-define(SSH_MSG_CHANNEL_DATA,			94).
-define(SSH_MSG_CHANNEL_EXTENDED_DATA,		95).
-define(SSH_MSG_CHANNEL_EOF,			96).
-define(SSH_MSG_CHANNEL_CLOSE,			97).
-define(SSH_MSG_CHANNEL_REQUEST,		98).
-define(SSH_MSG_CHANNEL_SUCCESS,		99).
-define(SSH_MSG_CHANNEL_FAILURE,		100).

%%%----------------------------------------------------------------------
%%% #   SSH_EXTENDED_DATA_xxx
%%% Description: Type codes for SSH_MSG_CHANNEL_EXTENDED_DATA packages
%%%----------------------------------------------------------------------

-define(SSH_EXTENDED_DATA_STDERR, 1).

%%%----------------------------------------------------------------------
%%% #   SSH_OPEN_xxx
%%% Description: Reason codes for SSH_MSG_OPEN_FAILURE packages.
%%%----------------------------------------------------------------------

-define(SSH_OPEN_ADMINISTRATIVELY_PROHIBITED,	1).
-define(SSH_OPEN_CONNECT_FAILED,		2).
-define(SSH_OPEN_UNKNOWN_CHANNEL_TYPE,		3).
-define(SSH_OPEN_RESOURCE_SHORTAGE,		4).

%%%----------------------------------------------------------------------
%%% #   SSH_FILE_TRANSFER_VERSION
%%% Description: The version of the filetransfer protocol we implement.
%%%----------------------------------------------------------------------

-define(SSH_FILE_TRANSFER_VERSION,	3).

%%%----------------------------------------------------------------------
%%% #   SSH_FXP_xxx
%%% Description: Request and initialization packet types for file transfer
%%%              protocol.
%%%----------------------------------------------------------------------

-define(SSH_FXP_INIT,		1).
-define(SSH_FXP_VERSION,	2).
-define(SSH_FXP_OPEN,		3).
-define(SSH_FXP_CLOSE,		4).
-define(SSH_FXP_READ,		5).
-define(SSH_FXP_WRITE,		6).
-define(SSH_FXP_LSTAT,		7).
-define(SSH_FXP_FSTAT,		8).
-define(SSH_FXP_SETSTAT,	9).
-define(SSH_FXP_FSETSTAT,	10).
-define(SSH_FXP_OPENDIR,	11).
-define(SSH_FXP_READDIR,	12).
-define(SSH_FXP_REMOVE,		13).
-define(SSH_FXP_MKDIR,		14).
-define(SSH_FXP_RMDIR,		15).
-define(SSH_FXP_REALPATH,	16).
-define(SSH_FXP_STAT,		17).
-define(SSH_FXP_RENAME,		18).
-define(SSH_FXP_READLINK,	19).
-define(SSH_FXP_SYMLINK,	20).
-define(SSH_FXP_STATUS,		101).
-define(SSH_FXP_HANDLE,		102).
-define(SSH_FXP_DATA,		103).
-define(SSH_FXP_NAME,		104).
-define(SSH_FXP_ATTRS,		105).
-define(SSH_FXP_EXTENDED,	200).
-define(SSH_FXP_EXTENDED_REPLY,	201).

%%%----------------------------------------------------------------------
%%% #   SSH_FXP_xxx
%%% Description: Response packet types for file transfer protocol.
%%%----------------------------------------------------------------------

-define(SSH_FX_OK,			0).
-define(SSH_FX_EOF,			1).
-define(SSH_FX_NO_SUCH_FILE,		2).
-define(SSH_FX_PERMISSION_DENIED,	3).
-define(SSH_FX_FAILURE,			4).
-define(SSH_FX_BAD_MESSAGE,		5).
-define(SSH_FX_NO_CONNECTION,		6).
-define(SSH_FX_CONNECTION_LOST,		7).
-define(SSH_FX_OP_UNSUPPORTED,		8).

%%%----------------------------------------------------------------------
%%% #   SSH_FILEXFER_xxx
%%% Description: Bits for file attributes bit mask
%%%----------------------------------------------------------------------

-define(SSH_FILEXFER_ATTR_SIZE,		16#00000001).
-define(SSH_FILEXFER_ATTR_UIDGID,	16#00000002).
-define(SSH_FILEXFER_ATTR_PERMISSIONS,	16#00000004).
-define(SSH_FILEXFER_ATTR_ACMODTIME,	16#00000008).
-define(SSH_FILEXFER_ATTR_EXTENDED,	16#80000000).

%%%----------------------------------------------------------------------
%%% #   SSH_FXF_xxx
%%% Description: Bits for file pflags bit mask
%%%----------------------------------------------------------------------

-define(SSH_FXF_READ,		16#00000001).
-define(SSH_FXF_WRITE,		16#00000002).
-define(SSH_FXF_APPEND,		16#00000004).
-define(SSH_FXF_CREAT,		16#00000008).
-define(SSH_FXF_TRUNC,		16#00000010).
-define(SSH_FXF_EXCL,		16#00000020).

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

%%%----------------------------------------------------------------------
%%% #2.3   DEFINITION OF MACROS
%%%----------------------------------------------------------------------

-define(SSH_STRING(String),
	[<<(length(String)):32/integer>>, 
	 list_to_binary(String)]).

-define(SSH_UINT_32(Integer), <<(Integer):32/integer>>).

-endif. % SSH_HRL defined
