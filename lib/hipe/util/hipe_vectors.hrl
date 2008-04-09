%%-define(USE_TUPLES, true).
-define(USE_GBTREES, true).

-ifdef(USE_TUPLES).
-type(hipe_vector() :: tuple()).
-endif.

-ifdef(USE_GBTREES).
-type(gb_tree()     :: tuple()).  % XXX: Temporarily
-type(hipe_vector() :: gb_tree()).
-endif.
