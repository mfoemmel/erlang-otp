%% -*- erlang-indent-level: 2 -*-
%%============================================================================
%% File    : cfg.hrl
%% Author  : Kostis Sagonas <kostis@it.uu.se>
%% Purpose : Contains typed record declarations for the CFG data structures
%%
%% $Id$
%%============================================================================

%%
%% This is supposed to be local but appears here for the time being
%% just so that it is used below
%%
-record(cfg_info, {'fun'         :: mfa(),
                   start_label   :: non_neg_integer(),
                   is_closure    :: bool(),
                   closure_arity :: byte(),
                   is_leaf       :: bool(),
                   params,     % :: list()
                   info = []}).  %% this field seems not needed; take out??

%%
%% Data is a triple with a dict of constants, a list of labels and an integer
%%
-type(cfg_data() :: {dict(), [non_neg_integer()], non_neg_integer()}).

%%
%% The following is to be used by other modules
%%
-record(cfg, {table :: gb_tree(),
              info  :: #cfg_info{},
              data  :: cfg_data()}).
-type(cfg() :: #cfg{}).
