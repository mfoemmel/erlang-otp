%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

-define(HIPE_AMD64,		true).
-define(HIPE_X86_RA_FINALISE,	hipe_amd64_ra_finalise).
-define(HIPE_X86_REGISTERS,	hipe_amd64_registers).
-define(HIPE_X86_X87,		hipe_amd64_x87).
-include("../x86/hipe_x86_ra_finalise.erl").
