-module(hipe_gen_cfg).

-export([start_label/1,
	 succ/2,
         pred/2
	]).

%%-define(DO_ASSERT, true).
-define(GEN_CFG, true).	     % needed for cfg.inc

-type(dict()    :: tuple()). % needed for cfg.hrl
-type(gb_tree() :: tuple()). % needed for cfg.hrl

-include("../main/hipe.hrl").
-include("cfg.hrl").
-include("cfg.inc").

