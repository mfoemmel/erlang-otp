%%% $Id$
%%% A thin abstraction layer which permits plugging in alternative
%%% implementations in place of BEAM's built-in 'vector:' operations.
%%%
%%% This module implements any support functions needed by the
%%% macros defined in hipe_vector.hrl.

-module(hipe_vector).

-include("hipe_vector.hrl").

-ifdef(VECTOR_USING_GBTREES).

-export([from_list_gb/1]).

from_list_gb(List) ->
    from_list_gb(List, 1, []).

from_list_gb([Value|Values], Index, RevOrdDict) ->
    from_list_gb(Values, Index+1, [{Index,Value}|RevOrdDict]);
from_list_gb([], _, RevOrdDict) ->
    gb_trees:from_orddict(lists:reverse(RevOrdDict)).

-endif.
