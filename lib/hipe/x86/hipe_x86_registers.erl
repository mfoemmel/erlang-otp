%%% $Id$
%%%
%%% TODO:
%%% - Do we need a pseudo reg for the condition codes?

-module(hipe_x86_registers).

-export([reg_name/1,
	 first_virtual/0,
	 is_precoloured/1,
	 all_precoloured/0,
	 eax/0,
	 ecx/0,
	 temp0/0,
	 temp1/0,
	 esp/0,
	 proc_pointer/0,
	 heap_pointer/0,
	 heap_limit/0,
	 fcalls/0,
	 proc_offset/1,
	 esp_limit_offset/0,
	 is_fixed/1,
	 fixed/0,
	 allocatable/0,
	 nr_args/0,
	 arg/1,
	 is_arg/1,
	 args/1,
	 call_clobbered/0,
	 tailcall_clobbered/0,
	 live_at_return/0]).

-include("../rtl/hipe_literals.hrl").

-define(EAX, 0).
-define(ECX, 1).
-define(EDX, 2).
-define(EBX, 3).
-define(ESP, 4).
-define(EBP, 5).
-define(ESI, 6).
-define(EDI, 7).
-define(FCALLS, 8).		% proc field alias
-define(HEAP_LIMIT, 9).		% proc field alias
-define(LAST_PRECOLOURED, 9).

-define(ARG0, ?EAX).
-define(ARG1, ?EDX).
-define(ARG2, ?ECX).
-define(ARG3, ?EBX).
-define(ARG4, ?EDI).

-define(TEMP0, ?EBX).	% XXX: was EAX
-define(TEMP1, ?EDI).	% XXX: was EDX


-define(PROC_POINTER, ?EBP).
-define(HEAP_POINTER, ?ESI).

reg_name(R) ->
    case R of
	?EAX -> "%eax";
	?ECX -> "%ecx";
	?EDX -> "%edx";
	?EBX -> "%ebx";
	?ESP -> "%esp";
	?EBP -> "%ebp";
	?ESI -> "%esi";
	?EDI -> "%edi";
	?FCALLS -> "%fcalls";
	?HEAP_LIMIT -> "%hplim";
	Other -> "%r" ++ integer_to_list(Other)
    end.

first_virtual() -> ?LAST_PRECOLOURED + 1.

is_precoloured(X) -> X =< ?LAST_PRECOLOURED.

all_precoloured() ->
    [?EAX,
     ?ECX,
     ?EDX,
     ?EBX,
     ?ESP,
     ?EBP,
     ?ESI,
     ?EDI,
     ?FCALLS,
     ?HEAP_LIMIT].

eax() -> ?EAX.
ecx() -> ?ECX.
temp0() -> ?TEMP0.
temp1() -> ?TEMP1.
esp() -> ?ESP.
proc_pointer() -> ?PROC_POINTER.
fcalls() -> ?FCALLS.
heap_pointer() -> ?HEAP_POINTER.
heap_limit() -> ?HEAP_LIMIT.

proc_offset(?FCALLS) -> ?P_FCALLS;
proc_offset(?HEAP_LIMIT) -> ?P_HP_LIMIT;
proc_offset(_) -> false.

esp_limit_offset() -> ?P_NSP_LIMIT.

is_fixed(?ESP) -> true;
is_fixed(?PROC_POINTER) -> true;
is_fixed(?FCALLS) -> true;
is_fixed(?HEAP_POINTER) -> true;
is_fixed(?HEAP_LIMIT) -> true;
is_fixed(_) -> false.

fixed() ->
    [?ESP, ?PROC_POINTER, ?FCALLS, ?HEAP_POINTER, ?HEAP_LIMIT].

allocatable() ->
    [?EDX, ?ECX, ?EBX, ?EDI, ?EAX].

nr_args() -> ?X86_NR_ARG_REGS.

arg(N) ->
    if N < ?X86_NR_ARG_REGS ->
	    case N of
		0 -> ?ARG0;
		1 -> ?ARG1;
		2 -> ?ARG2;
		3 -> ?ARG3;
		4 -> ?ARG4;
		_ -> exit({?MODULE, arg, N})
	    end;
       true ->
	    exit({?MODULE, arg, N})
    end.

is_arg(R) ->
    case R of
	?ARG0 -> ?X86_NR_ARG_REGS > 0;
	?ARG1 -> ?X86_NR_ARG_REGS > 1;
	?ARG2 -> ?X86_NR_ARG_REGS > 2;
	?ARG3 -> ?X86_NR_ARG_REGS > 3;
	?ARG4 -> ?X86_NR_ARG_REGS > 4;
	_ -> false
    end.

args(Arity) ->
    Max = ?X86_NR_ARG_REGS,
    N = if Arity > Max -> Max; true -> Arity end,
    args(N-1, []).

args(I, Rest) when I < 0 -> Rest;
args(I, Rest) -> args(I-1, [arg(I) | Rest]).

call_clobbered() ->
    [{?EAX,tagged},{?EAX,untagged},	% does the RA strip the type or not?
     {?EDX,tagged},{?EDX,untagged},
     {?ECX,tagged},{?ECX,untagged},
     {?EBX,tagged},{?EBX,untagged},
     {?EDI,tagged},{?EDI,untagged}].

tailcall_clobbered() ->		% tailcall crapola needs two temps
    [{?TEMP0,tagged},{?TEMP0,untagged},
     {?TEMP1,tagged},{?TEMP1,untagged}].

live_at_return() ->
    [{?EAX,tagged}
     %% XXX: should the following (fixed) regs be included or not?
     ,{?ESP,untagged}
     ,{?PROC_POINTER,untagged}
     %% Lets try not!
     %% If these are included they will interfere with other 
     %% temps during regalloc, but regs FCALLS and HEAP_LIMIT
     %% don't even exist at regalloc.
     ,{?FCALLS,untagged}
     ,{?HEAP_POINTER,untagged}
     ,{?HEAP_LIMIT,untagged}
    ].
