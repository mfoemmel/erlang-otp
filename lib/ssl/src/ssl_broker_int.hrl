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

%% Purpose: record definitions shared between ssl_prim.erl and ssl_broker.erl

-record(st, {brokertype = nil,	% connector | listener | acceptor
	     server = nil, 	% pid of ssl_server
	     client = nil, 	% client pid
	     collector = nil, 	% client pid, or collector during change of 
	     			% controlling process
	     fd = nil, 		% fd of "external" socket in port program
	     active = true, 	% true | false
	     opts = [], 	% options
	     thissock = nil,    % this sslsocket
	     newif = true,	% new ssl interface
	     proxysock = nil, 	% local proxy socket within Erlang
	     status = nil,	% open | closing | closed 
	     debug = false	%
	    }).
