%%--------------------------------------------------------------------
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
%% File    : imr.hrl
%% Author  : Per Danielsson <pd@gwaihir>
%% Purpose : Record definitions for structs used in the
%%         : implementation repository.
%% Created : 14 Oct 1997 by Per Danielsson <pd@gwaihir>
%%----------------------------------------------------------------------

-record(imprec,	{server_name,
		 primary_activation_mode = unshared,
			%shared, unshared, or per-method
		 secondary_activation_mode = per_client, %*** ???
			%per-client, per-client-process or multiple-client. 
		 persistent_only = false,	%true or false
		 owner,
		 permissions,
		 activation_orders}).
