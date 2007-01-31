%% -*- erlang-indent-level: 2 -*-
%%=======================================================================
%% File        : hipe_icode_primops.hrl
%% Author      : Kostis Sagonas
%% Description : Contains definitions for HiPE's primitive operations.
%%=======================================================================
%% $Id$
%%=======================================================================

-record(apply_N, {arity::byte()}).

-record(closure_element, {n::byte()}).

-record(element, {index::integer()}).

-record(gc_test, {need::integer()}).

-record(mkfun, {mfa::mfa(), magic_num::integer(), index::integer()}).

-record(unsafe_element, {index::integer()}).

-record(unsafe_update_element, {index::integer()}).
