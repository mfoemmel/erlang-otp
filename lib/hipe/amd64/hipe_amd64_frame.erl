%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

-define(HIPE_X86_FRAME,     hipe_amd64_frame).
-define(HIPE_X86_REGISTERS, hipe_amd64_registers).
-define(HIPE_X86_LIVENESS,  hipe_amd64_liveness).
-define(LEAF_WORDS,	    ?AMD64_LEAF_WORDS).

-include("../x86/hipe_x86_frame.erl").
