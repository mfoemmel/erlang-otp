%%%-------------------------------------------------------------------
%%% File    : hipe_icode_type.hrl
%%% Author  : Tobias Lindahl <tobiasl@csd.uu.se>
%%% Created :  2 Sep 2004 by Tobias Lindahl <tobiasl@csd.uu.se>
%%%-------------------------------------------------------------------

-define(TYPE_DEPTH, 3).


%% For Dialyzer's classification of warnings.

-define(WARN_RETURN_NO_EXIT, warn_return_no_exit).
-define(WARN_RETURN_ONLY_EXIT, warn_return_only_exit).
-define(WARN_RETURN_BOTH_EXIT_AND_RETURN, warn_return_both_exit_and_return).
-define(WARN_RETURN_ONLY_RETURN, warn_return_only_return).
-define(WARN_NOT_CALLED, warn_not_called).
-define(WARN_TYPE_GUARD, warn_type_guard).
-define(WARN_NON_PROPER_LIST, warn_non_proper_list).
-define(WARN_TUPLE_AS_FUN, warn_tuple_as_fun).
-define(WARN_FUN_APP, warn_fun_app).
-define(WARN_MATCHING, warn_matching).
-define(WARN_COMP, warn_comp).
-define(WARN_GUARDS, warn_guards).
-define(WARN_OLD_BEAM, warn_old_beam).
-define(WARN_FAILING_CALL, warn_failing_call).
