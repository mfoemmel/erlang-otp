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

-include_lib("kernel/include/file.hrl").

-ifndef(SERVER_SOFTWARE).
-define(SERVER_SOFTWARE,"inets/develop").	% Define in Makefile!
-endif.
-define(SERVER_PROTOCOL,"HTTP/1.0").
-define(SOCKET_CHUNK_SIZE,8192).
-define(SOCKET_MAX_POLL,25).
-define(FILE_CHUNK_SIZE,64*1024).
-define(NICE(Reason),lists:flatten(atom_to_list(?MODULE)++": "++Reason)).
-define(DEFAULT_CONTEXT,
	[{errmsg,"[an error occurred while processing this directive]"},
	 {timefmt,"%A, %d-%b-%y %T %Z"},
	 {sizefmt,"abbrev"}]).
%% Use this macro with caution !
%-define(DEBUG(Format, Args), io:format("(~p:~p) : "++Format++"~n",
%				       [?MODULE, ?LINE]++Args)).
-define(DEBUG(F,A),[]).

-record(init_data,{peername,resolve}).
-record(mod,{init_data,
	     data=[],
	     socket_type=ip_comm,
	     socket,
	     config_db,
	     method,
	     request_uri,
	     http_version,
	     request_line,
	     parsed_header=[],
	     entity_body,
	     connection}).
