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

-include("inet_sctp.hrl").
-include("inet_int.hrl").

-define(FAMILY, inet).
-export([getserv/1,getaddr/2,translate_ip/1]).
-export([open/1,close/1,listen/2,connect/5,sendmsg/3,recv/2]).



getserv(Port) when Port band 16#ffff =:= Port -> {ok, Port};
getserv(Name) when is_atom(Name) ->
    inet:getservbyname(Name, sctp);
getserv(_) ->
    {error,einval}.

getaddr(Address, Timer) ->
    inet:getaddr_tm(Address, ?FAMILY, Timer).

translate_ip(IP) ->
    inet:translate_ip(IP, ?FAMILY).


    
open(Opts) ->
    case inet:sctp_options(Opts, ?MODULE) of
	{ok,#sctp_opts{fd=Fd,ifaddr=Addr,port=Port,opts=SOs}} ->
	    inet:open(Fd, Addr, Port, SOs, sctp, ?FAMILY, ?MODULE);
	Error -> Error
    end.

close(S) ->
    prim_inet:close(S).

listen(S, Flag) ->
    prim_inet:listen(S, Flag).
	
connect(S, Addr, Port, Opts, Timer) ->
    case prim_inet:chgopts(S, Opts) of
	ok ->
	    Timeout = inet:timeout(Timer),
	    case prim_inet:connect(S, Addr, Port, Timeout) of
		ok ->
		    connect_get_assoc(S, Addr, Port, Timer);
		Error -> Error
	    end;
	Error -> Error
    end.

connect_get_assoc(S, Addr, Port, Timer) ->
    case recv(S, inet:timeout(Timer)) of
	{ok, {Addr, Port, [], Ev = #sctp_assoc_change{}}} ->
	    %% Yes, got Assoc Change on this destination:
	    %% check the status:
	    case Ev of
		#sctp_assoc_change{state=comm_up} ->
		    %% Yes, successfully connected, return the whole
		    %% sctp_assoc_change event (containing, in particular,
		    %% the AssocID).
		    %% NB: we consider the connection to be successful
		    %% even if the number of OutStreams is not the same
		    %% as requested by the user:
		    {ok,Ev};
		_ ->
		    %% Any other event: Error:
		    {error,Ev}
	    end;
    % Any other message received instead of that Assoc Change:
    % currently treated as an error:
    {error,_}=Error ->
        Error;
    Hmm ->
        % FIXME: this should never happen
        {error,Hmm}
    end.

sendmsg(S, SRI, Data) ->
    prim_inet:sendmsg(S, SRI, Data).

recv(S, Timeout) ->
    prim_inet:recvfrom(S, 0, Timeout).
