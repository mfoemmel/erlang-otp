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

-module(ssh_userauth).

-include("ssh.hrl").

-export([
	 passwd/4
	]).

%%%----------------------------------------------------------------------
%%% #             passwd/4
%%%     
%%% Input:        Ssh, UserName, Password, Service : 
%%% Output:       {error, Reason} | ok
%%%
%%% Description:  Authenticates service Service as user UserName with Password.
%%%----------------------------------------------------------------------

passwd(Ssh, Name, Password, Service) ->
    case ssh_transport:send_msg(Ssh,
				[?SSH_MSG_SERVICE_REQUEST,
				 ?SSH_STRING("ssh-userauth")]) of
	ok ->
	    ssh_transport:set_active_once(Ssh),
	    receive
		{ssh_transport, Ssh, <<?SSH_MSG_SERVICE_ACCEPT,_/binary>>} ->
		    case ssh_transport:send_msg(Ssh, 
						[?SSH_MSG_USERAUTH_REQUEST,
						 ?SSH_STRING(Name),
						 ?SSH_STRING(Service),
						 ?SSH_STRING("password"),
						 0,
						 ?SSH_STRING(Password)]) of
			ok ->
			    ssh_transport:set_active_once(Ssh),
			    receive
				{ssh_transport, Ssh, 
				 <<?SSH_MSG_USERAUTH_SUCCESS>>} ->
				    ok;
				{ssh_transport, Ssh,
				 <<?SSH_MSG_USERAUTH_FAILURE,
				 StrSize:32/integer,
				 Str:StrSize/binary,
				 Boolean>>} ->
				    case regexp:split(binary_to_list(Str),
						      ",") of
					{ok,ListOfMethods} ->
					    case lists:member("password",
							      ListOfMethods) of
						true ->
						    {error,wrong_password};
						false ->
						    {error,password_not_supported}
					    end;
					Other ->
					    {error,protocol_error}
				    end;
				{ssh_transport_error, Ssh, Error} ->
				    {error, {transport_error, Error}}
			    end;
			Other ->
			    Other
		    end;
		{ssh_transport_error, Ssh, Error} ->
		    {error, {transport_error, Error}}
	    end;
	Other ->
	    Other
    end.
