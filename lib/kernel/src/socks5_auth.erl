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
-module(socks5_auth).

%% Subnegotiations for socks5

-export([negotiate/2]).

-define(UNAME_PASSWD_VER, 16#01).

%% Simple version
negotiate(S, none) ->
    {ok, S};
negotiate(S, user) ->
    %% Check in inet_db for file or user/password ???
    io:format("Enter Socks5 User/Password (. to cancel)\n"),
    case read_user_password() of
	{ok, {User,Password}} ->
	    inet_tcp:send(S, [?UNAME_PASSWD_VER, length(User), User,
			      length(Password), Password]),
	    case inet_tcp:recv(S, 2) of
		{ok, [?UNAME_PASSWD_VER, 0]} ->
		    {ok, S};
		_ ->
		    inet_tcp:close(S),
		    {error, "socks5 negotiation failure"}
	    end;
	Error -> 
	    inet_tcp:close(S), 
	    Error
    end;
negotiate(S, gssapi) ->
    inet_tcp:close(S),
    {error, "gssapi negotiation not implemented yet"};
negotiate(S, error) ->
    inet_tcp:close(S),
    {error, einval};
negotiate(S,_Method) ->
    inet_tcp:close(S),
    {error, "unknown subnegotiation"}.


read_user_password() ->
    case prompt_string('User: ') of
	{ok, User} ->
	    case prompt_string('Password: ') of
		{ok, Password} ->
		    {ok, {User, Password}};
		Error -> Error
	    end;
	Error -> Error
    end.
    
prompt_string(Prompt) ->
    Line = io:get_line(Prompt),
    Len = length(Line),
    if Len == 1 ->
	    prompt_string(Prompt);
       Line == ".\n" ->
	    {error, "cancel"};
       Len >= 256 ->
	    {ok, lists:sublist(Line, 1, 255)};
       true ->
	    {ok, lists:sublist(Line, 1, Len-1)}
    end.
