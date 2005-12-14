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
	  %% (if true, and if trans_ack is false or trans_timer 
	  %% is zero (0), then acks will be sent immediatly)
	  auto_ack,

	  %% ------
	  %% Accumulate trans acks/requests and send them "all" later
	  %% in one bigger message.
	  %% For this to take effekt, trans_timer has to be > 0
	  %% trans_ack and/or trans_req has to be true.
	  %% Accumulate transactions, and send them later, either 
	  %% when the timer expires, when maxcount number of
	  %% transactions has been accumulated or in the case
	  %% requests, when the maxsize number of bytes has been
	  %% accumulated (whichever happens first). 
	  %% (Note that, for acks, this is only valid if auto_ack 
	  %% is true)

	  trans_ack,            % false
	  trans_ack_maxcount,   % 10

	  trans_req,            % false   
	  trans_req_maxcount,   % 10
	  trans_req_maxsize,    % 2048

	  trans_timer,          % 0 (don't accumulate transactions)
	  trans_sender,         % The trans sender process ref, or undefined

	  pending_timer, 
	  
	  %% ------
	  %% These counter's is used for the MGCOriginatedPendingLimit
	  %% and MGOriginatedPendingLimit counters (of the root package).
	  %% If the user is an MGC, the 
	  %%   sent_pending_limit - represent MGCOriginatedPendingLimit
	  %%   recv_pending_limit - represent MGOriginatedPendingLimit
	  %% If the user is an MG, the 
	  %%   sent_pending_limit - represent MGOriginatedPendingLimit
	  %%   recv_pending_limit - represent MGCOriginatedPendingLimit
	  sent_pending_limit,  % infinity | integer() > 0
	  recv_pending_limit,  % infinity | integer() > 0

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
	  reply_data,
	  threaded,
	  strict_version      % true
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

-ifdef(megaco_trace_io).
-define(report(Level, C, Label, Contents), 
	io:format("*** [~s] ~p ~p *** "
		  "~n   ~p[~p] " ++ Label ++ 
		  "~n   ~p"
		  "~n   ~p"
		  "~n", 
		  [megaco:format_timestamp(now()), 
		   self(), Level, ?MODULE, ?LINE, C, Contents])).
-else.
-define(report(Level, C, Label, Contents),
	megaco:report_event(Level, ?APPLICATION, Label,
			    [{line, ?MODULE, ?LINE}, C | Contents])).
-endif.

-define(report_important(C, Label, Contents), ?report(20, C, Label, Contents)).
-define(report_verbose(  C, Label, Contents), ?report(40, C, Label, Contents)).
-define(report_debug(    C, Label, Contents), ?report(60, C, Label, Contents)).
-define(report_trace(    C, Label, Contents), ?report(80, C, Label, Contents)).


%%%----------------------------------------------------------------------
%%% Debug
%%%----------------------------------------------------------------------

-ifdef(megaco_debug).
-define(d(F,A), io:format("~w: " ++ F ++ "~n",[?MODULE|A])).
-else.
-define(d(F,A), ok).
-endif.
