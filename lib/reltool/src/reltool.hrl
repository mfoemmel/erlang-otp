%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%

-define(APPLICATION,      reltool). 
-define(MISSING_APP,      '*MISSING*').
-define(MISSING_APP_TEXT, "*MISSING*").

-record(common,
        {sys_debug,         % term()
         wx_debug,          % term()
         trap_exit,         % bool()
         app_tab,           % ets_tab()
         mod_tab,           % ets_tab()
         mod_used_by_tab}). % ets_tab()

-record(sys,
        {mod_cond,         % all | app | ebin | derived | none
         incl_cond,        % include | exclude | derived
         debug_info,       % keep | strip
         app_file,         % keep | strip | all
	 profile,          % standalone | development | embedded
	 emu_name,         % string()
         incl_erts_dirs,   % all | [directory()] | {add, [directory()]} | {del, [directory()]}
         excl_erts_dirs,   % all | [directory()] | {add, [directory()]} | {del, [directory()]}
         incl_app_dirs,    % all | [directory()] | {add, [directory()]} | {del, [directory()]}
         excl_app_dirs,    % all | [directory()] | {add, [directory()]} | {del, [directory()]}
         root_dir,         % directory()
         lib_dirs,         % [directory()]
         escripts,         % [file()]
         apps,             % [#app{}]
         boot_rel,         % string()
         rels}).           % [#rel{}]

-record(rel,
        {name,             % string()
         vsn,              % string()
         rel_apps}).       % [#rel_app{}]

-record(rel_app,
        {name,             % atom()
         type,             % permanent | transient | temporary | load | none
         incl_apps}).      % [atom()]

-record(app,
        {%% Static
         name,            % atom()
         is_escript,      % bool()
         mod_cond,        % all | app | ebin | derived | none | undefined
         incl_cond,       % include | exclude | derived | undefined
         debug_info,      % keep | strip | undefined
         app_file,        % keep | strip | all | undefined
         incl_app_dirs,   % all | [directory()] | {add, [directory()]} | {del, [directory()]} | undefined
         excl_app_dirs,   % all | [directory()] | {add, [directory()]} | {del, [directory()]} | undefined
         use_selected_vsn,% bool() | undefined
         active_dir,      % dir_name()
         sorted_dirs,     % [dir_name()]
         vsn,             % string() e.g. "4.7"
         label,           % string() e.g. "mnesia" or "mnesia-4.7"  
         info,            % #app_info{} | undefined
         mods,            % [#mod{}]
         type,            % permanent | transient | temporary | load | none
         %% Dynamic
         status,          % missing | ok
         uses_mods,       % [atom()]
         used_by_mods,    % [atom()]
         uses_apps,       % [atom()]
         used_by_apps,    % [atom()]
         is_pre_included, % bool()
         is_included}).   % bool()

-record(mod,
        {%% Static
          name,            % atom()
          app_name,        % atom()
          incl_cond,       % include | exclude | derived | undefined
          debug_info,      % keep | strip | undefined
          is_app_mod,      % bool(),
          is_ebin_mod,     % bool(),
          uses_mods,       % [module()]
          exists,          % bool()
          %% Dynamic
          status,          % missing | ok
          used_by_mods,    % [atom()]
          is_pre_included, % bool() | undefined
          is_included}).   % bool() | undefined

%% app      - Include all modules in app file
%% ebin     - Include all modules on ebin directory
%% derived  - Include only those modules that others are dependent on

-record(app_info,
        {description = "",
         id = "",
         vsn = "",
         modules = [],
         maxP = infinity,
         maxT = infinity,
         registered = [],
         incl_apps = [],
         applications = [],
         env = [],
         mod = undefined,
         start_phases = undefined}).
 
-define(ERR_IMAGE,    0).
-define(WARN_IMAGE,   1).
-define(QUEST_IMAGE,  2).
-define(TICK_IMAGE,   3).
-define(CROSS_IMAGE,  4).
-define(SOURCE_IMAGE, 5).

-define(KEYSEARCH(Key, Pos, List),
        reltool_utils:safe_keysearch(Key, Pos, List, ?MODULE, ?LINE)).

-define(DEFAULT_INCL_COND, derived).
-define(DEFAULT_MOD_COND, all).
-define(DEFAULT_DEBUG_INFO, keep).
-define(DEFAULT_APP_FILE, keep).
-define(DEFAULT_INCL_APP_DIRS, ["ebin", "priv"]).
-define(DEFAULT_EXCL_APP_DIRS, []).
-define(DEFAULT_INCL_ERTS_DIRS, ["bin"]).
-define(DEFAULT_EXCL_ERTS_DIRS, []).
-define(DEFAULT_REL_NAME, "start_clean").
