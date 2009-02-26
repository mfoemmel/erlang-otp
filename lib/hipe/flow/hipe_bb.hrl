%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : bb.hrl
%%% Author  : Per Gustafsson <pergu@it.uu.se>
%%% Description : Typed record declaration for basic blocks 
%%%
%%% Created : 20 Dec 2007 by Per Gustafsson <pergu@it.uu.se>
%%%-------------------------------------------------------------------

-record(bb, {code=[] :: [_]}).

-type bb() :: #bb{}.
