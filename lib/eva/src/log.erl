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
-module(log).

%% External exports
-export([open/3, set_admin_status/2, close/1, get_logs/0, transfer/5]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% Name = string()

open(Name, Type, WrapTime) when list(Name) ->
    gen_server:call(log_server, {open, Name, Type, WrapTime}).

set_admin_status(Name, Status) ->
    gen_server:call(log_server, {set_admin_status, Name, Status}).

close(Name) ->
    gen_server:call(log_server, {close, Name}).

get_logs() ->
    gen_server:call(log_server, get_logs).    

transfer(Address, User, Passwd, DestFile, SearchFunc) ->
    case ftp:open(Address) of
	{ok, FtpId} ->
	    case ftp:user(FtpId, User, Passwd) of
		ok ->
		    case ftp:send_chunk_start(FtpId, DestFile) of
			ok ->
			    loop_transfer(FtpId, SearchFunc, start);
			{error, _} ->
			    {error, ftp_write_error}
		    end;
		{error, _} ->
		    {error, ftp_login_error}
	    end;
	{error, _} ->
	    {error, ftp_bad_address}
    end.

loop_transfer(FtpId, SearchFunc, Cont) ->
    {M,F,A} = SearchFunc,
    case catch apply(M, F, [Cont | A]) of
	eof ->
	    ftp:send_chunk_end(FtpId),
	    ftp:close(FtpId),
	    ok;
	{error, R} ->
	    ftp:send_chunk_end(FtpId),
	    ftp:close(FtpId),
	    {error, {bad_search_result, R}};
	{NCont, Bytes} ->
	    case ftp:send_chunk(FtpId, Bytes) of
		ok ->
		    loop_transfer(FtpId, SearchFunc, NCont);
		{error, R} ->
		    ftp:send_chunk_end(FtpId),
		    ftp:close(FtpId),
		    {error, ftp_transfer_error}
	    end;
	{'EXIT', R} ->
	    ftp:send_chunk_end(FtpId),
	    ftp:close(FtpId),
	    {error, {bad_search_result, R}}
    end.

