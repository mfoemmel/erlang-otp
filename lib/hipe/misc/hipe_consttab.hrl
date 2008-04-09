%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------------

-type(dict() :: tuple()).   % temporarily until the dict module is fixed

-type(hipe_constlbl() :: non_neg_integer()).
-type(hipe_consttab() :: {dict(), [hipe_constlbl()], hipe_constlbl()}).

%%-----------------------------------------------------------------------------
