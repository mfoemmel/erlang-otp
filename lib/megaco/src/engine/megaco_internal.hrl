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
%% Purpose: Define internal data structures and error codes
%%----------------------------------------------------------------------

-define(APPLICATION, megaco).

%% -define(debug, true).

%% N.B. Update megaco_config when a new field is added
-record(conn_data,
	{
	  conn_handle, 
	  serial,
	  max_serial,
	  request_timer,
	  long_request_timer,

	  %% Auto send of ack: false | true 
	  %% (if accu_ack_timer is zero (0), then acks will
	  %%  be sent immediatly)
	  auto_ack,           
	  
	  %% Accumulate ack's, and send them later. Either 
	  %% when the timer expires or when maxcount number 
	  %% of ack's has been accumulated (whichever happens
	  %% first).
	  %% (only valid if auto_ack is true)
	  accu_ack_timer,      % 0 (don't accumulate ack)
	  accu_ack_maxcount,   % 10
	  ack_sender,          % The ack sender process ref, or undefined

	  pending_timer, 
	  reply_timer, 
	  control_pid,
	  monitor_ref,
	  send_mod,
	  send_handle,
	  encoding_mod,
	  encoding_config,
	  protocol_version,
	  auth_data,
	  user_mod,
	  user_args,
	  reply_action,       % call | cast
	  reply_data
	 }).

%% N.B. Update megaco_config when a new field is added
-record(remote_conn_data,
	{conn_handle,
	 user_node,
	 monitor_ref}).

%%%----------------------------------------------------------------------
%%% Event Trace
%%%----------------------------------------------------------------------

%% -define(report(Level, C, Label, Contents),
%% 	   if
%% 	       record(C, conn_data), list(Contents) ->
%% 		   megaco:report_event(Level, C#conn_data.conn_handle,
%% 				       Label, [{line, ?MODULE, ?LINE}, C | Contents]);
%% 	       record(C, megaco_receive_handle) ->
%% 		   megaco:report_event(Level, C#megaco_receive_handle.local_mid,
%% 				       Label, [{line, ?MODULE, ?LINE}, C | Contents]);
%% 	       record(C, megaco_conn_handle) ->
%% 		   megaco:report_event(Level, C#megaco_conn_handle.local_mid,
%% 				       Label, [{line, ?MODULE, ?LINE}, C | Contents]);
%% 	       true ->
%% 		   megaco:report_event(Level, ?APPLICATION,
%% 				       Label, [{line, ?MODULE, ?LINE}, C | Contents])
%% 	   end).

-define(report(Level, C, Label, Contents),
	megaco:report_event(Level, ?APPLICATION, Label,
			    [{line, ?MODULE, ?LINE}, C | Contents])).

-define(report_important(C, Label, Contents), ?report(20, C, Label, Contents)).
-define(report_verbose(  C, Label, Contents), ?report(40, C, Label, Contents)).
-define(report_debug(    C, Label, Contents), ?report(60, C, Label, Contents)).
-define(report_trace(    C, Label, Contents), ?report(80, C, Label, Contents)).
