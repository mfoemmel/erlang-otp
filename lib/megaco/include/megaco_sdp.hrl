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
%% Purpose: 
%%----------------------------------------------------------------------
%%
%%
%% Explanaition of the fields in the SDP body
%%   (See RFC 2327 for the complete decription)
%%
%% Session descriptions
%%
%%  v        protocol version
%%  o        owner/creator and session identifier 
%%  s        session name                          (not used)
%%  i        session information                   (not used)
%%  u        URI of description                    (not used)
%%  e        email address                         (not used)
%%  p        phone number                          (not used) 
%%  c        connection information
%%  b        bandwidth information
%%  z        time zone adjustment
%%  k        encryption key
%%  a        zero or more session attribute lines
%%  Zero or more media descriptions
%%
%% Time descriptions
%%
%%  t        time the session is active
%%  r        zero or more repeat times
%%  
%% Media descriptions
%%
%%  m        media name and transport address
%%  i        media title
%%  c        connection information - optional if included at session-level
%%  b        bandwidth information
%%  k        encryption key
%%  a        zero or more media attribute lines
%%
%% An SDP-body is a list of the folowing tuples
%%
%%   {FieldID, FieldValue} where
%%
%%   FieldID = atom()
%%   FiledValue is a term()
%%
%% FieldID    FieldValue
%% 
%% o          #sdp_o{}
%% c          #sdp_c{}
%% m          #sdp_m{}
%% a          #sdp_a_rtpmap{}  iff 'att-field'=rtpmap
%% a          #sdp_a_ptime{}   iff 'att-field'=ptime
%% <other>    string()
%%
%%----------------------------------------------------------------------
-ifndef(sdp_).
-define(sdp_, true).


-record(sdp_v, {
	  version					% integer()
	 }).

-record(sdp_o, {
          user_name,                            % string()
          session_id,                           % string()
          version,                              % string()
          network_type,                         % string()
          address_type,                         % string()
          address                               % string()
         }).
          
-record(sdp_c, {
          network_type,                         % string()
          address_type,                         % string()
          connection_addr                       % string() | 
						% {multicast, string(), 
                                                %  ttl-integer(), params-[]}
         }).

-record(sdp_m, {
          media,                                % string()
          port,                                 % integer()
          num_ports,                            % integer()
          transport,                            % string()
          fmt_list = []                         % [ string() ]
         }).

-record(sdp_b, {
          modifier,                             % string()
          bandwidth                             % integer()
         }).

-record(sdp_t, {
          start,                                % integer()
          stop                                  % integer()
         }).

-record(sdp_r, {
          repeat_interval,                      % string()
          active_duration,                      % string()
	  list_of_offsets                       % [ string() ]
	 }).
      
-record(sdp_z, {
	  list_of_adjustments                   % [ string() ]
	 }).

-record(sdp_k, {
	  method,                               % string()
	  encryption_key                        % string()
	 }).

-record(sdp_a, {
	  attribute,                             % atom()
	  value                                 % type depends on variable
	 }).

-record(sdp_a_rtpmap, {
          payload_type,                         % integer()
          encoding_name,                        % string()
          clock_rate,                           % integer()
          encoding_parms = []                   % [ string() ]
         }).
                
-record(sdp_a_ptime, {
          packet_time                           % integer()
         }).

-record(sdp_s, {
	  name                                  % string()
	 }).
-record(sdp_i, {
	  session_descriptor                    % string()
	 }).
-record(sdp_u, {
	  uri                                   % string()
	 }).
-record(sdp_e, {
	  email                                 % string()
	 }).
-record(sdp_p, {
	  phone_number                          % string()
	 }).

          
-endif.
