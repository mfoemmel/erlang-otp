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
%% This record is returned by inet:gethostbyaddr/2 and inet:gethostbyname/2.

-record(hostent,
	{
	 h_name,          %% offical name of host
	 h_aliases = [],  %% alias list
	 h_addrtype,      %% host address type
	 h_length,        %% length of address
	 h_addr_list = [] %% list of addresses from name server
	}).
