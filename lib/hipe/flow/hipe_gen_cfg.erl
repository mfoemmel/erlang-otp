-module(hipe_gen_cfg).

-export([start_label/1,
	 succ/2,
         pred/2,
         pred_map/1
	]).

%%-define(DO_ASSERT, true).
-define(GEN_CFG,true).	% needed for cfg.inc below

-include("../main/hipe.hrl").
-include("cfg.hrl").
-include("cfg.inc").

