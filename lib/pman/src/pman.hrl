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

-record(gs_pman,{win,		       % GS top window
		 frame,		       % GS top frame
		 grid,		       % GS process info grid
		 size,
		 see,		       % Excluded modules (???)
		 hide_modules = [],    % Excluded modules
		 ospid_display = [],
		 old = [],	       % [{node, [pid]}] 
				       % List of all previous PIDs,
				       % i.e. all PIDs from the
				       % previous invocation of
				       % the refresh.
		 show_pids = [],       % List of explicitly shown PIDs
		 hide_pids = [],       % List of expl. hidden PIDs
		 hide_new = false,     % New process visibility
		 hide_system = false,  % System process visibility
		 nodes,
		 focus = 1,	       % PID line with focus
		 focus_pid,            % pid in focus
		 node,		       % Current node being supervised
		 options}).


