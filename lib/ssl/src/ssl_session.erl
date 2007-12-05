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
%%----------------------------------------------------------------------
%% Purpose: Handles ssl sessions
%%----------------------------------------------------------------------

-module(ssl_session).

-include("ssl_handshake.hrl").
-include("ssl_internal.hrl").

%% Internal application API
-export([is_new/2, id/1, id/2, create_cache/0, remove_cache/1,
	 cache_first/0, cache_next/1, cache_update/2,
	 cache_update/3, cache_delete/2, 
	 cache_delete/3, cache_lookup/2, cache_lookup/3,
	 safe_fix_cache/1]).

-define(GEN_UNIQUE_ID_MAX_TRIES, 10).

%%--------------------------------------------------------------------
%% Function: is_new(ClientSuggestedId, ServerDecidedId) -> true | false
%%
%%      ClientSuggestedId = binary() 
%%      ServerDecidedId = binary()
%%
%% Description: Checks if the session id decided by the server is a
%%              new or resumed sesion id.
%%--------------------------------------------------------------------
is_new(<<>>, _) ->
    true;
is_new(SessionId, SessionId) ->
    false;
is_new(_, _) ->
    true.

%%--------------------------------------------------------------------
%% Function: id(ClientInfo) -> SessionId 
%%
%%      ClientInfo = {HostIP, Port, SslOpts}
%%      HostIP = ipadress()
%%      Port = integer() 
%%      SessionId = binary()
%%
%% Description: Should be called by the client side to get an id 
%%              for the client hello message.
%%--------------------------------------------------------------------
id(ClientInfo) ->
    case select_session(ClientInfo) of
	no_session ->
	    <<>>;
	SessionId ->
	    SessionId
    end.

%%--------------------------------------------------------------------
%% Function: id(Port, SuggestedSessionId) -> SessionId 
%%
%%      Port = integer() 
%%      SuggestedSessionId = SessionId = binary()
%%
%% Description: Should be called by the server side to get an id 
%%              for the server hello message.
%%--------------------------------------------------------------------
id(Port, <<>>) ->
    new_id(Port, ?GEN_UNIQUE_ID_MAX_TRIES);

id(Port, SuggestedSessionId) ->
    case is_resumable(SuggestedSessionId, Port) of
	true ->
	    SuggestedSessionId;
	false ->
	    new_id(Port, ?GEN_UNIQUE_ID_MAX_TRIES)
    end.

%%--------------------------------------------------------------------
%% Function: create_cache() -> Cache  
%%
%%	Cache - Reference to the cash (opaque)    
%%
%% Description: Creates a new session cache.
%%--------------------------------------------------------------------
create_cache() ->
    ets:new(cache_name(), [named_table, set, protected]).

%%--------------------------------------------------------------------
%% Function: remove_cache(Cache) -> 
%%
%%      Cache - as returned by create_cash/0
%%
%% Description: Removes the cash 
%%--------------------------------------------------------------------
remove_cache(Cache) ->
    ets:delete(Cache).

%%--------------------------------------------------------------------
%% Function: cache_update([HostIP,] Port, Session) -> _
%%      HostIP = ipadress()
%%      Port = integer()
%%      Session = #session{}
%%      Cache - as returned by create_cash/0
%%
%% Description: Caches a new session or updates a already cached one.
%%--------------------------------------------------------------------
%Used by client side
cache_update(HostIP, Port, Session) ->
    ets:insert(cache_name(), 
	       {{{HostIP, Port}, Session#session.session_id}, Session}).

% Used by server side
cache_update(Port, Session) ->
    ets:insert(cache_name(), {{Port, Session#session.session_id}, Session}).

%%--------------------------------------------------------------------
%% Function: cache_delete(HostIP, Port, Session) -> _   
%%      HostIP =  Host = string() | ipadress()
%%      Port =  integer()
%%      Session = #session{}
%%
%% Description: Delets a cache entry
%%--------------------------------------------------------------------
%Used by client side
cache_delete(HostIP, Port, Session) ->
    ets:delete(cache_name(), {{HostIP, Port}, Session#session.session_id}).
% Used by server side
cache_delete(Port, Session) ->
    ets:delete(cache_name(), {Port, Session#session.session_id}).

%%--------------------------------------------------------------------
%% Function: cache_lookup([HostIP,] Port, SessionId) -> ok   
%%      HostIP =  Host = string() | ipadress()
%%      Port = integer()
%%      Session = #session{}
%%
%% Description: Looks up a cach entry
%%--------------------------------------------------------------------
%Used by client side
cache_lookup(HostIP, Port, SessionId) ->
    case ets:lookup(cache_name(), {{HostIP, Port}, SessionId}) of
	[{{{HostIP, Port}, SessionId}, Session}] ->
	    Session;
	[] ->
	    undefined
    end.
% Used by server side
cache_lookup(Port, SessionId) ->
    case ets:lookup(cache_name(), {Port, SessionId}) of
	[{{Port, SessionId}, Session}] ->
	    Session;
	[] ->
	    undefined
    end.
%%--------------------------------------------------------------------
%% Function: cache_first() -> Key | cache_end   
%%   
%%      Key = {HostIP, Port, Session} | {Port, SessionId}
%%      HostIP =  HostIP = string() | ipadress()
%%      Port =  integer()
%%      Session = #session{}
%%
%% Description: Returns the key of the first entry in the cache or
%% cache_end if it is empty.
%%--------------------------------------------------------------------
cache_first() ->
    case ets:first(cache_name()) of
	'$end_of_table' ->
	    cache_end;
	Key ->
	    Key
    end.
%%--------------------------------------------------------------------
%% Function: cache_next() -> Key | cache_end   
%%
%%     Key = {HostIP, Port, Session} | {Port, SessionId}
%%     HostIP =  HostIP = string() | ipadress()
%%     Port =  integer()
%%     Session = #session{}
%%
%% Description: Looks up a cach entry
%%--------------------------------------------------------------------
cache_next(Key) ->	    
    case ets:next(cache_name(), Key) of
	'$end_of_table' ->
	    cache_end;
	NewKey ->
	    NewKey
    end.
%%--------------------------------------------------------------------
%% Function: safe_fix_cache(Boolean) -> 
%%
%%      Boolean = true | false
%%
%% Description: Fixes a cach table for safe "next"-traversal.
%%--------------------------------------------------------------------
safe_fix_cache(Boolean) ->
    ets:safe_fixtable(cache_name(), Boolean).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
cache_name() ->
    ssl_otp_session_cache.

select_session({HostIP, Port, SslOpts}) ->    
    case ets:select(cache_name(), 
		    [{{{{HostIP, Port},'$1'}, '$2'},[],['$$']}]) of
	[]  -> 
	    no_session; 
	Sessions ->
	    select_session(Sessions, SslOpts)
    end.

select_session(Sessions, #ssl_options{ciphers = Ciphers,
				      reuse_sessions = ReuseSession}) ->
    IsResumable = 
 	fun(Session) -> 
 		ReuseSession andalso (Session#session.is_resumable) andalso  
 		    lists:member(Session#session.cipher_suite, Ciphers)
 	end,
    case [Id ||  [Id, Session] <- Sessions, IsResumable(Session)] of
 	[] ->
 	    no_session;
 	List ->
 	    hd(List)
    end.
	    
%% If we can not generate a not allready in use session ID in
%% ?GEN_UNIQUE_ID_MAX_TRIES we make the new session uncacheable The
%% value of ?GEN_UNIQUE_ID_MAX_TRIES is stolen from open SSL which
%% states : "If we can not find a session id in
%% ?GEN_UNIQUE_ID_MAX_TRIES either the RAND code is broken or someone
%% is trying to open roughly very close to 2^128 (or 2^256) SSL sessions to our server"
new_id(_, 0) ->
    <<>>;
new_id(Port, Tries) ->
    Id = crypto:rand_bytes(?NUM_OF_SESSION_ID_BYTES),
    case cache_lookup(Port, Id) of
	undefined ->
	    Now =  calendar:datetime_to_gregorian_seconds({date(), time()}),
	    %% New sessions can not be set to resumable
	    %% until handshake is compleate and the
	    %% other session values are set.
	    cache_update(Port, #session{session_id = Id,
					is_resumable = false,
					time_stamp = Now}),
	    Id;
	_ ->
	    new_id(Port, Tries - 1)
    end.

is_resumable(SuggestedSessionId, Port) ->
    case cache_lookup(Port, SuggestedSessionId) of
	#session{} = Session ->
	    Session#session.is_resumable;
	undefined ->
	    false
    end.
