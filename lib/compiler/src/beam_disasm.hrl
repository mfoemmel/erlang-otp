%% -*- erlang-indent-level: 4 -*-
%%=======================================================================
%% File        : beam_disasm.hrl
%% Author      : Kostis Sagonas
%% Description : Exposes type definitions used also in other parts of
%%		 the system (e.g. in the translation from Beam to Icode)
%%=======================================================================

-record(function, {name :: atom(),
		   arity:: byte(),
		   entry,   % unused ??
		   code :: list()}).  % list of instructions

