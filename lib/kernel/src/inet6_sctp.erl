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
%% Portions created by Ericsson are Copyright 2007, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%% The SCTP protocol was added 2006
%% by Leonid Timochouk <l.timochouk@gmail.com>
%% and Serge Aleynikov  <serge@hq.idt.net>
%% at IDT Corp. Adapted by the OTP team at Ericsson AB.
%%
%%     $Id$
%%
-module(inet_sctp).

%% This module provides functions for communicating with
%% sockets using the SCTP protocol.  The implementation assumes that
%% the OS kernel supports SCTP providing user-level SCTP Socket API:
%%     http://tools.ietf.org/html/draft-ietf-tsvwg-sctpsocket-13

-include("inet_int.hrl").

-export([open/1,sendmsg/3]).

open(Opts) ->
    catch inet:sctp_options(Opts, inet) of
	{ok,#sctp_opts{fd=Fd, ifaddr=Addr, port=Port, opts=Opts}} ->
	    inet:open(Fd, Addr, Port, Opts, sctp, inet, ?MODULE);
	Error -> Error
    end.

sendmsg(S, SRI, Data) ->
    prim_inet:sendmsg(S, SRI, Data).

