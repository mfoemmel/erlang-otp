%%% $Id$
%%% A thin abstraction layer which permits plugging in alternative
%%% implementations in place of BEAM's built-in 'vector:' operations.
%%%
%%% The abstraction implements the following macros:
%%%
%%% ?vector_from_list(List) -> Vector
%%% ?vector_new(Size, Init) -> Vector
%%% ?vector_to_list(Vector) -> List
%%% ?vector_get(Index, Vector) -> Value
%%% ?vector_set(Index, Vector, Value) -> NewVector

%%% uncomment exactly one of these
-define(VECTOR_USING_BEAM,1).
%-define(VECTOR_USING_GBTREES,1).

%%%
%%% Using BEAM's built-in 'vector:' operations.
%%%
-ifdef(VECTOR_USING_BEAM).
-define(vector_from_list(List), vector:from_list(List)).
-define(vector_new(Size, Init), vector:new(Size, Init)).
-define(vector_to_list(Vec), vector:to_list(Vec)).
-define(vector_get(Ix, Vec), vector:get(Ix, Vec)).
-define(vector_set(Ix, Vec, Val), vector:set(Ix, Vec, Val)).
-endif.

%%%
%%% BEAM-like 'vector:' implementation on top of gb_trees.
%%% All valid keys are always present in the tree, since this simplifies
%%% the to_list/1, get/2, and set/3 operations.
%%%
-ifdef(VECTOR_USING_GBTREES).
-define(vector_from_list(List), hipe_vector:from_list_gb(List)).
-define(vector_new(Size, Init), hipe_vector:from_list_gb(lists:duplicate(Size, Init))).
-define(vector_to_list(Vec), gb_trees:values(Vec)).
-define(vector_get(Ix, Vec), gb_trees:get(Ix, Vec)).
-define(vector_set(Ix, Vec, Val), gb_trees:update(Ix, Val, Vec)).
-endif.

%%%
%%% BEAM-like 'vector:' implementation on top of ordinary tuples.
%%% This is a trivial reference implementation with a very slow set/3 operation.
%%% Disabled by default.
%%%
-ifdef(notdef).
-define(vector_from_list(List), list_to_tuple(List)).
-define(vector_new(Size, Init), list_to_tuple(lists:duplicate(Size, Init))).
-define(vector_to_list(Vec), tuple_to_list(Vec)).
-define(vector_get(Ix, Vec), element(Ix, Vec)).
-define(vector_set(Ix, Vec, Val), setelement(Ix, Vec, Val)).
-endif.
