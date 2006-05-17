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

%%% Description: SSH user authenication

-module(ssh_userauth).

-export([auth/3, auth_remote/3, disconnect/2,
	 reg_user_auth_server/0, get_auth_users/0,
	 get_user_from_cm/1]).

-include("ssh.hrl").
-include("ssh_userauth.hrl").
-include("PKCS-1.hrl").

-define(PREFERRED_PK_ALG, ssh_rsa).

other_alg(ssh_rsa) -> ssh_dsa;
other_alg(ssh_dsa) -> ssh_rsa.

auth(SSH, Service, Opts) ->
    case user_name(Opts) of
	{ok, User} ->
	    _Failure = none(SSH, Service, User),
	    AlgM = proplists:get_value(public_key_alg,
				       Opts, ?PREFERRED_PK_ALG),
	    case public_key(SSH, Service, User, Opts, AlgM) of
		ok -> ok;
		{failure, _F} ->
		    case public_key(SSH, Service, User, Opts,
				    other_alg(AlgM)) of
			ok ->
			    ok;
			{failure, _F} ->
			    passwd(SSH, Service, User, Opts);
			Error -> Error
		    end;
		Error -> Error
	    end;
	Error -> Error
    end.

%% Find user name
user_name(Opts) ->
    Env = case os:type() of
	      {win32, _} -> "USERNAME";
	      {unix, _} -> "LOGNAME"
	  end,
    case proplists:get_value(user, Opts, os:getenv(Env)) of
	false ->
	    case os:getenv("USER") of
		false -> {error, no_user};
		User1 -> {ok, User1}
	    end;
	User2 -> {ok, User2}
    end.

%% Public-key authentication
public_key(SSH, Service, User, Opts, AlgM) ->
    SSH ! {ssh_install, userauth_pk_messages()},
    Alg = AlgM:alg_name(),
    case ssh_file:private_identity_key(Alg, Opts) of
	{ok, PrivKey} ->
	    PubKeyBlob = ssh_file:encode_public_key(PrivKey),
	    SigData = build_sig_data(SSH, User, Service, Alg, PubKeyBlob),
	    Sig = AlgM:sign(PrivKey, SigData),
	    SigBlob = list_to_binary([?string(Alg), ?string(Sig)]),
	    SSH ! {ssh_msg, self(),
		   #ssh_msg_userauth_request{
				  user = User,
				  service = Service,
				  method = "publickey",
				  data = [?TRUE,
					  ?string(Alg),
					  ?string(PubKeyBlob),
					  ?string(SigBlob)]}},
	    public_key_reply(SSH);
	{error, enoent} ->
	    {failure, enoent};
	Error ->
	    Error
    end.

%% Send none to find out supported authentication methods
none(SSH, Service, User) ->
    SSH ! {ssh_install, 
	   userauth_messages() ++ userauth_passwd_messages()},
    SSH ! {ssh_msg, self(), 
	   #ssh_msg_userauth_request { user = User,
				       service = Service,
				       method = "none",
				       data = <<>> }},
    passwd_reply(SSH).

%% Password authentication for ssh-connection
passwd(SSH, Service, User, Opts) -> 
    SSH ! {ssh_install, 
	   userauth_messages() ++ userauth_passwd_messages()},
    do_password(SSH, Service, User, Opts).


check_password(User, Password, Opts) ->
    case proplists:get_value(pwdfun, Opts) of
	undefined ->
	    Static = get_password_option(Opts, User),
	    if
		Password == Static ->
		    true;
		true ->
		    {false, "Bad password"}
	    end;
	PW ->
	    PW(User, Password)
    end.


disconnect(Handle, Opts) ->
     case proplists:get_value(disconnectfun, Opts) of
	undefined ->
	     ok;
	 DF ->
	     DF(Handle)
     end.

connected_fun(User, PeerAddr, Opts, Method) ->
     case proplists:get_value(connectfun, Opts) of
	undefined ->
	     ok;
	 CF ->
	     CF(User, PeerAddr, Method)
     end.

%% Reason for failfun() is a structured tuple, the following values are 
%% possible

% {passwd, Str}   - if the password was bad 
% {authmethod, none}  - if the client attempted to use none as method
% {authmethod, OtherStr} - if the client tried to use an unknown authmeth
% {error, key_unacceptable} - if a the key sent to us wasn't verifiable
% {error, inconsistent_key} - we got a broken DSA key
% {error, invalid_signature} - again, broken key
% {error, {{openerr, PosixAtom}, {file, Filename}}} - if we failed to open the 
%                                                     file containing the user keys
% {error, nouserdir}   - No dir configured for user which contains keys


failfun(User, Opts, Reason) ->
     case proplists:get_value(failfun, Opts) of
	undefined ->
	     error_logger:format("~p failed to login: ~p~n", [User, Reason]);
	 FF ->
	     FF(User, Reason)
     end.



get_password_option(Opts, User) ->
    Passwords = proplists:get_value(user_passwords, Opts, []),
    case lists:keysearch(User, 1, Passwords) of
	{value, {User, Pw}} -> Pw;
	false -> proplists:get_value(password, Opts, false)
    end.

do_password(SSH, Service, User, Opts) ->
    Password = case proplists:get_value(password, Opts) of
		   undefined -> ssh_transport:read_password(SSH, "ssh password: ");
		   PW -> PW
	       end,
    ?dbg(true, "do_password: User=~p Password=~p\n", [User, Password]),
    SSH ! {ssh_msg, self(), 
	   #ssh_msg_userauth_request { user = User,
				       service = Service,
				       method = "password",
				       data =
				       <<?BOOLEAN(?FALSE),
					?STRING(list_to_binary(Password))>> }},
    case passwd_reply(SSH) of
	ok -> ok;
	{error, E} -> {error, E};
	_  -> do_password(SSH, Service, User, Opts)
    end.	    

public_key_reply(SSH) ->
    receive
	{ssh_msg, SSH, R} when record(R, ssh_msg_userauth_success) ->
	    ok;
	{ssh_msg, SSH, R} when record(R, ssh_msg_userauth_failure) ->
	    {failure,
	     string:tokens(R#ssh_msg_userauth_failure.authentications, ",")};
	{ssh_msg, SSH, R} when record(R, ssh_msg_userauth_banner) ->
	    io:format("~w", [R#ssh_msg_userauth_banner.message]),
	    public_key_reply(SSH);
	{ssh_msg, SSH, R} when record(R, ssh_msg_disconnect) ->
	    {error, disconnected};
	_Other ->
	    ?dbg(true, "public_key_reply: Other=~w\n", [_Other]),
	    {error, unknown_msg}
    end.

passwd_reply(SSH) ->
    receive
	{ssh_msg, SSH, R} when record(R, ssh_msg_userauth_success) ->
	    ok;
	{ssh_msg, SSH, R} when record(R, ssh_msg_userauth_failure) ->
	    {failure,
	     string:tokens(R#ssh_msg_userauth_failure.authentications, ",")};
	{ssh_msg, SSH, R} when record(R, ssh_msg_userauth_banner) ->
	    io:format("~w", [R#ssh_msg_userauth_banner.message]),
	    passwd_reply(SSH);
	{ssh_msg, SSH, R} when record(R, ssh_msg_userauth_passwd_changereq) ->
	    {error, R};
	{ssh_msg, SSH, R} when record(R, ssh_msg_disconnect) ->
	    {error, disconnected};
	Other ->
	    ?dbg(true, "passwd_reply: Other=~w\n", [Other]),
	    self() ! Other,
	    {error, unknown_msg}
    end.

auth_remote(SSH, Service, Opts) ->
    SSH ! {ssh_install, 
	   userauth_messages() ++ userauth_passwd_messages()},
    ssh_transport:service_accept(SSH, "ssh-userauth"),
    do_auth_remote(SSH, Service, Opts).

%% validate_password(User, Password, Opts) ->
%%     OurPwd = get_password_option(Opts, User),
%%     Password == OurPwd.

do_auth_remote(SSH, Service, Opts) ->
    receive
	{ssh_msg, SSH, #ssh_msg_userauth_request { user = User,
						   service = Service,
						   method = "password",
						   data = Data}} ->
	    <<_:8, ?UINT32(Sz), BinPwd:Sz/binary>> = Data,
	    Password = binary_to_list(BinPwd),
 	    %% If we need to support PAM session mgmt this
 	    %% function needs to return an opaque handle back to
 	    %% to ssh_cm so that the session can be ended properly
 	    %% at logout
 	    Ret =  check_password(User, Password, Opts),
 	    case Ret of
 		true ->
 		    {ok, Peer} = ssh_transport:peername(SSH),
 		    connected_fun(User, Peer, Opts, "password"),
 		    SSH ! {ssh_msg, self(), #ssh_msg_userauth_success{}},
 		    {ok, {undefined, User}};
 		{true, Handle} ->
 		    {ok, Peer} = ssh_transport:peername(SSH),
 		    connected_fun(User, Peer, Opts, "password"),
 		    SSH ! {ssh_msg, self(), #ssh_msg_userauth_success{}},
 		    {ok, Handle};
 		{false, Str} ->
 		    failfun(User, Opts, {passwd, Str}),
		    SSH ! {ssh_msg, self(), #ssh_msg_userauth_failure {
					  authentications = "",
					  partial_success = false}},
		    do_auth_remote(SSH, Service, Opts)
	    end;
	{ssh_msg, SSH, #ssh_msg_userauth_request { user = User,
						   service = Service,
						   method = "none",
						   data = _Data}} ->
	    failfun(User, Opts, {authmethod, none}),
	    SSH ! {ssh_msg, self(), #ssh_msg_userauth_failure {
				  authentications = "publickey,password",
				  partial_success = false}},
	    do_auth_remote(SSH, Service, Opts);
	{ssh_msg, SSH, #ssh_msg_userauth_request  { user = User,
						    service = Service,
						    method = "publickey",
						    data = Data}} ->
	    <<?BOOLEAN(HaveSig), ?UINT32(ALen), BAlg:ALen/binary, 
	     ?UINT32(KLen), KeyBlob:KLen/binary, SigWLen/binary>> = Data,
	    Alg = binary_to_list(BAlg),
	    %% is ssh_file the proper module?
	    %% shouldn't this fun be in a ssh_key.erl?
 	    case HaveSig of
 		?TRUE ->
		    case verify_sig(SSH, User, Service, Alg,
				    KeyBlob, SigWLen, Opts) of
			ok ->
 			    {ok, Peer} = ssh_transport:peername(SSH),
			    SSH ! {ssh_msg, self(),
				   #ssh_msg_userauth_success{}},
 			    connected_fun(User, Peer, Opts, "publickey"),
			    reg_user_auth(self(), User),
 			    {ok, {undefined, User}};
 			{error, ERR} ->
 			    %% we have a file, but failed to open/read it
 			    failfun(User, Opts, ERR),
			    SSH ! {ssh_msg, self(),
				   #ssh_msg_userauth_failure{
						  authentications="publickey,password",
						  partial_success = false}},
			    do_auth_remote(SSH, Service, Opts)
		    end;
		?FALSE ->
		    SSH ! {ssh_install, userauth_pk_messages()},
		    SSH ! {ssh_msg, self(),
			   #ssh_msg_userauth_pk_ok{
					  algorithm_name = Alg,
					  key_blob = KeyBlob}},
		    do_auth_remote(SSH, Service, Opts)
	    end;
	{ssh_msg, SSH, #ssh_msg_userauth_request  { user = User,
						    service = Service,
						    method = Other,
						    data = _Data}} ->
	    failfun(User, Opts, {authmethod, Other}),
	    SSH ! {ssh_msg, self(),
		   #ssh_msg_userauth_failure {
				  authentications = "publickey,password",
				  partial_success = false}},
	    do_auth_remote(SSH, Service, Opts);
	Other ->
	    ?dbg(true, "Other ~p~n", [Other]),
	    {error, Other}
    end.

alg_to_module("ssh-dss") ->
    ssh_dsa;
alg_to_module("ssh-rsa") ->
    ssh_rsa.

%% make a signature for PGP user auth
build_sig_data(SSH, User, Service, Alg, KeyBlob) ->
    %% {P1,P2} = Key#ssh_key.public,
    %% EncKey = ssh_bits:encode([Alg,P1,P2], [string, mpint, mpint]),
    SessionID = ssh_transport:get_session_id(SSH),
    Sig = [?binary(SessionID),
	   ?SSH_MSG_USERAUTH_REQUEST,
	   ?string(User),
	   ?string(Service),
	   ?binary(<<"publickey">>),
	   ?TRUE,
	   ?string(Alg),
	   ?binary(KeyBlob)],
    list_to_binary(Sig).

%% verify signature for PGP user auth
verify_sig(SSH, User, Service, Alg, KeyBlob, SigWLen, Opts) ->
    {ok, Key} = ssh_file:decode_public_key_v2(KeyBlob, Alg),
    case ssh_file:lookup_user_key(User, Alg, Opts) of
	{ok, OurKey} ->
	    case OurKey of
		Key ->
		    NewSig = build_sig_data(SSH, User, Service, Alg, KeyBlob),
		    <<?UINT32(AlgSigLen), AlgSig:AlgSigLen/binary>> = SigWLen,
		    <<?UINT32(AlgLen), _Alg:AlgLen/binary,
		     ?UINT32(SigLen), Sig:SigLen/binary>> = AlgSig,
		    M = alg_to_module(Alg),
		    M:verify(OurKey, NewSig, Sig);
		_ ->
		    {error, key_unacceptable}
	    end;
	Error -> Error
    end.

%% the messages for userauth
userauth_messages() ->
    [ {ssh_msg_userauth_request, ?SSH_MSG_USERAUTH_REQUEST,
       [string, 
	string, 
	string, 
	'...']},

      {ssh_msg_userauth_failure, ?SSH_MSG_USERAUTH_FAILURE,
       [string, 
	boolean]},

      {ssh_msg_userauth_success, ?SSH_MSG_USERAUTH_SUCCESS,
       []},

      {ssh_msg_userauth_banner, ?SSH_MSG_USERAUTH_BANNER,
       [string, 
	string]}].

userauth_passwd_messages() ->
    [ 
      {ssh_msg_userauth_passwd_changereq, ?SSH_MSG_USERAUTH_PASSWD_CHANGEREQ,
       [string, 
	string]}
     ].

userauth_pk_messages() ->
    [ {ssh_msg_userauth_pk_ok, ?SSH_MSG_USERAUTH_PK_OK,
       [string, % algorithm name
	string]} % key blob
     ].

%% user registry (which is a really simple server)
reg_user_auth(Pid, User) ->
    case whereis(?MODULE) of
	undefined ->
	    ok;
	UPid ->
	    UPid ! {reg, User, Pid, self()}
    end.

get_auth_users() ->
    Self = self(),
    case whereis(?MODULE) of
	undefined ->
	    {error, no_user_auth_reg};
	Pid ->
	    Pid ! {get, Self},
	    receive
		{Self, R} -> R
	    end
    end.

get_user_from_cm(CM) ->
    case get_auth_users() of
	{ok, UsersCMs} ->
	    case lists:keysearch(CM, 2, UsersCMs) of
		{value, {User, CM}} ->
		    User;
		_ ->
		    error
	    end;
	_ ->
	    error
    end.

reg_user_auth_server() ->
    is_pid(whereis(?MODULE)) andalso unregister(?MODULE),
    Pid = spawn(fun() -> reg_user_auth_server_loop([]) end),
    register(?MODULE, Pid).

reg_user_auth_server_loop(Users) ->
    receive
	{get, From} ->
	    NewUsers = [{U, P} || {U, P} <- Users,
				  erlang:is_process_alive(P)],
	    From ! {From, {ok, NewUsers}},
	    reg_user_auth_server_loop(NewUsers);
	{reg, User, Pid, From} ->
	    NewUsers = [{User, Pid} | Users],
	    From ! {From, ok},
	    reg_user_auth_server_loop(NewUsers);
	_ ->
	    ok
    end.
