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
%% Portions created by Ericsson are Copyright 2000, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%

%%% This file is meant to be included by xref_* only.

-define(VAR_EXPR, '$F_EXPR').
-define(MOD_EXPR, '$M_EXPR').

%%% Filenames are stored as directory and basename. A lot of heap can
%%% be saved by keeping only one (or few) copy of the directory name.

%% 'data' in xref_mod holds "raw" data (as sets) for each module. The
%% data in 'variables' is derived from raw data.
-record(xref, {
	  version = 1,                  % version of the xref record
	  mode = functions,
	  variables = not_set_up,       % table of variables

	  modules = dict:new(),         % dict-of(xref_mod())
	  applications = dict:new(),    % dict-of(xref_app())
	  releases = dict:new(),        % dict-of(xref_rel())

	  library_path = [],         % [string()] | code_path
	  libraries = dict:new(),    % dict-of(xref_lib())

	  builtins_default = false,  % Default value of the 'builtins' option.
	  recurse_default = false,   % Default value of the 'recurse' option.
	  verbose_default = false,   % Default value of the 'verbose' option.
	  warnings_default = true    % Default value of the 'warnings' option.
	  }).

-record(xref_mod, {
	  name = '',
	  app_name = [],     % [] or [AppName]
	  dir = "",          % string(), directory where the BEAM file resides
	  mtime,             % modification time for file
	  builtins,          % whether calls to built-in functions are included
	  info,              % number of exports, locals etc.
	  no_unresolved = 0, % number of unresolved calls
	  data               
	  %% Data has been read from the BEAM file, and is represented here
          %% as a list of sets.
	  %% If xref.mode = functions:
          %% [
	  %% DefAt,         M -> P(V * N)
	  %% L,             M -> P(V)
	  %% X,             M -> P(V)
	  %% LCallAt,       M -> P(V * V -> P(N))
	  %% XCallAt,       M -> P(V * V -> P(N))
	  %% CallAt,        M -> P(V * V -> P(N))
	  %% LC,            M -> P(V * V)
	  %% XC,            M -> P(V * V)
	  %% LU,            M -> P(V)
	  %% EE,            M -> P(EV * EV)
	  %% ECallAt,       M -> P(EV * EV -> P(N))
	  %% Unres,         M -> P(V * V)
	  %% LPredefined    M -> P(V)
          %% ]
	  %%
	  %% If xref.mode = modules:
          %% [
	  %% X,             M -> P(V)
	  %% I              M -> P(V)
          %% ]
	  }).

-record(xref_app, {
	  name = '',
	  rel_name = [], % [] or [RelName]
	  vsn = [],
	  dir = ""     % where BEAM files are read from
	  }).

-record(xref_rel, {
	  name = '',
	  dir = ""     % where application directories reside
	  }).

-record(xref_lib, {
	  name = '',   % atom(), module name 
	  dir = ""     % string(), directory where the file resides
	  }).

-record(xref_var, {
	  name = '',   % atom(), variable name
	  value,       % set or pair of sets, variable value
	  vtype,       % VarType (predef, tmp, user)
	  otype,       % ObjectType (vertex, edge, etc.)
	  type         % Type (function, module, etc.)
	  }).
