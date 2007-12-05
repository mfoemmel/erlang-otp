%% -*- erlang-indent-level: 2 -*-
%%===========================================================================
%% File        : typer_options.hrl
%% Author      : Kostis Sagonas <kostis@it.uu.se>
%% Description : Defines symbolic names for main TypEr options
%%===========================================================================

-define(SHOW, show).
-define(SHOW_EXPORTED, show_exported).
-define(ANNOTATE, annotate).
-define(ANNOTATE_INC_FILES, annotate_inc_files).

-type(typer_option() :: ?SHOW | ?SHOW_EXPORTED | ?ANNOTATE | ?ANNOTATE_INC_FILES).
