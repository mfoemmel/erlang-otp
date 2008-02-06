%% -*- erlang-indent-level: 4 -*-
%%=======================================================================
%% File        : beam_disasm.hrl
%% Author      : Kostis Sagonas
%% Description : Exposes type definitions used also in other parts of
%%		 the system (e.g. in the translation from Beam to Icode)
%%=======================================================================

%%
%% Possibly the following type declaration does not belong here...
%%
-type(beam_instr() :: 'bs_init_writable' | 'fclearerror' | 'if_end'
                    | 'remove_message' | 'return' | 'send' | 'timeout'
                    | tuple()).  %% XXX: Very underspecified - FIX THIS

%%
%% This is a labeled_export tuple returned by beam_lib:chunks/2.
%% It has the form {fun_name(), arity(), label()}.
%% DOES NOT BELONG HERE - IT BELONGS IN beam_lib.hrl BUT THERE IS NO SUCH FILE
%%
-type(lbl_export() :: {atom(), byte(), non_neg_integer()}).

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
