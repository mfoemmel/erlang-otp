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
%% compilation information record
-record(cdata, {module_identity,
		asn1_types = [], 
		mes = [], 
		traps = [], 
		mibfuncs, 
		sequences = [],
		imported_macros = [], 
		objectgroups = [],
		notificationgroups = [], 
		imports,
		oid_ets,
		status_ets}).


-record(mc_module_identity,
	{name,
	 last_updated,
	 organization,
	 contact_info,
	 description,
	 revisions = [],   %% A list of mc_revision
	 name_assign
	}
       ).

-record(mc_revision,
	{revision,
	 description
	}
       ).

-record(mc_object_type,
	{name,
	 syntax,
	 units,
	 max_access,
	 status,
	 description,
	 reference,
	 kind, 
	 name_assign
	}
       ).


-record(mc_new_type,
	{name, 
	 macro,
	 status,
	 description,
	 reference,
	 display_hint,
	 syntax
	}
       ).


-record(mc_trap,
	{name,
	 enterprise,
	 vars,
	 description,
	 reference,
	 num
	}
       ).


-record(mc_notification, 
	{name,
	 vars,
	 status,
	 description,
	 reference,
	 name_assign
	 }
       ).


-record(mc_module_compliance,
	{name,
	 status,
	 description,
	 reference,
	 module,
	 name_assign
	}
       ).


-record(mc_object_group,
	{name,
	 objects,
	 status,
	 description,
	 reference,
	 name_assign
	}
       ).


-record(mc_notification_group,
	{name,
	 objects,
	 status,
	 description,
	 reference,
	 name_assign
	}
       ).


-record(mc_sequence, 
	{name,
	 fields
	}
       ).


-record(mc_internal,
	{name, 
	 macro,
	 parent,
	 sub_index
	}
       ).

