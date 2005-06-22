%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

-define(HIPE_AMD64,		true).
-define(HIPE_RTL_TO_X86,	hipe_rtl_to_amd64).
-define(HIPE_X86_REGISTERS,	hipe_amd64_registers).
-define(ECX,			rcx).
-define(EAX,			rax).
-include("../x86/hipe_rtl_to_x86.erl").
