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
%%%----------------------------------------------------------------------
%%% Purpose : This header file contains the record for the modules
%%%           dbg_ui_view and dbg_ui_trace.
%%%----------------------------------------------------------------------



-record(trace,{meta,         % The pid of the process attaching the traced pid
	       pid,          % The pid we are tracing
	       cm,           % The current module being shown in the window
	       cm_obsolete,  % true=current module needs reload, otherwise not.

	       %% State of the traced Pid:
	       state,        % The state of the executing process
			     % One of ?DEAD, ?RUNNING, ?BREAK, ?WAIT
	       reason,	     % If state is ?DEAD, this is the EXIT reason
	       s_module,     % If state is ?BREAK or ?WAIT, this is the module
	       s_line,       % If state is?BREAK or ?WAIT, this is the line

	       cmd_no,       % Dummy variable
	       trace,        % Trace Frame in attach window status
	       bind,         % Bind Frame in attach window status
	       button,       % Button Frame in attach window status
	       eval,         % Eval Frame in attach window status
	       breaks,       % Breaks in the CM
	       stack,        % Stack Pointer
	       stack_flag,   % Stack flag status
	       btrace,       % Back Trace value
	       size,         % {Width,Height} of the window
	       win,          % Window Identifier
	       coords,       % Mouse pointer position
	       line,         % The line in the display editor
	       dir,          % Last int dir
	       mods,         % Modules which are trace compiled
	       btn_press,    % Last time the editor had a buttonpress
	       edt_marked,   % Row in editor has been marked
	       rec_edit}).   % Pid for the record editor
 
%% These are the states we assume that a process can be in.
%%

-define(DEAD, pid_dead).
-define(RUNNING, pid_running).
-define(BREAK, pid_at_break).
-define(WAIT, pid_waiting).

%%
%% Empty stack

-define(EMPTY_STACK, {1,1}).
