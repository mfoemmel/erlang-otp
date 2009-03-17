%% -*- erlang-indent-level: 4 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2009. All Rights Reserved.
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
%%
%% Purpose: Exposes type definitions used also in other parts of
%%	    the system (e.g. in the translation from Beam to Icode).

%%
%% Possibly the following type declaration does not belong here...
%%
-type beam_instr() :: 'bs_init_writable' | 'fclearerror' | 'if_end'
                    | 'remove_message' | 'return' | 'send' | 'timeout'
                    | tuple().  %% XXX: Very underspecified - FIX THIS

%%
%% This is a labeled_export tuple returned by beam_lib:chunks/2.
%% It has the form {fun_name(), arity(), label()}.
%% DOES NOT BELONG HERE - IT BELONGS IN beam_lib.hrl BUT THERE IS NO SUCH FILE
%%
-type lbl_export() :: {atom(), byte(), non_neg_integer()}.

%%-----------------------------------------------------------------------
%% Record definitions
%%-----------------------------------------------------------------------

-record(function, {name      :: atom(),
		   arity     :: byte(),
		   entry,    %% unused ??
		   code = [] :: [beam_instr()]}).

-record(beam_file, {module          :: atom(),
		    exports    = [] :: [lbl_export()],
		    attributes = [] :: [{atom(),_}],
		    comp_info  = [] :: [{atom(),_}],
		    code       = [] :: [#function{}]}).
