%% -*- erlang-indent-level: 2 -*-
%%============================================================================
%% File    : cfg.hrl
%% Author  : Kostis Sagonas <kostis@it.uu.se>
%% Purpose : Contains typed record declarations for the CFG data structures
%%
%% $Id$
%%============================================================================

%%
%% This is supposed to be local but appears here for the time being just for the
%% declaration below
%%
-record(cfg_info, {'fun',      % :: mfa(),
                   start_label,
                   is_closure    :: bool(),
                   closure_arity :: byte(),
                   is_leaf       :: bool(),
                   params,     % :: list()
                   extra,
                   info=[]}).
-type(cfg_info() :: #cfg_info{}).

%%
%% The following type is the one that is to be used by other modules
%%
-record(cfg, {table,  %
              info :: cfg_info(),
              data}). %
-type(cfg() :: #cfg{}).
