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
%%
%% Defines used for erlang boot/load protocol
%%

-define(EBOOT_PORT, 4368).    %% same as epmd but for udp !

-define(EBOOT_REQUEST, "EBOOTQ").
-define(EBOOT_REPLY,   "EBOOTR").

-define(EBOOT_RETRY,                 3). % number of retry before sleep
-define(EBOOT_REQUEST_DELAY,       500). % delay between retry
-define(EBOOT_SHORT_RETRY_SLEEP, 10000). % initial sleep time between boot attempt's
-define(EBOOT_UNSUCCESSFUL_TRIES,   10). % retries before longer sleep
-define(EBOOT_LONG_RETRY_SLEEP,  60000). % sleep time after a number of unsuccessful tries 
