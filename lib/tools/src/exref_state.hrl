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
%% Definition of the data type state.
%%
-record(state, {
		fgraph = nograph,     %% function call graph
		mgraph = nograph,     %% module graph
		includes = [],        %% include file path list
		excludes = [],        %% exclude module list
		defs = [],            %% epp pre defs
	        module = undefined,   %% current module
	        auto = undefined,     %% modules to auto include
	        file = undefined,     %% current file
	        imports = undefined,  %% imports in module
	        exports = undefined,  %% exports in module
	        options = undefined,  %% exref options
	        modules = [],         %% list of modules in graph (global)
	        paths = [],           %% list of tuples, {AbsolutePath, Module}.
	                              %% Used to be able to reload correct modules
	                              %% after a delete module operation has been
	                              %% performed on another module (which is
	                              %% done by deleting all and then reloading
	                              %% some).
                failed = []           %% List of modules which couldn't be loaded.
	  }).
