%%% -*- erlang-indent-level: 2 -*-
%%% $Id: hipe_amd64_liveness.erl,v 1.4 2005/03/29 19:39:47 mikpe Exp $

-define(HIPE_X86_LIVENESS,	hipe_amd64_liveness).
-define(HIPE_X86_DEFUSE,	hipe_amd64_defuse).
-define(HIPE_X86_REGISTERS,	hipe_amd64_registers).

-include("../x86/hipe_x86_liveness.erl").
