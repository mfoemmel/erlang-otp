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
	 edx/0,
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
edx() -> ?EDX.
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

call_clobbered() ->
    [{?EAX,tagged},{?EAX,untagged},	% does the RA strip the type or not?
     {?EDX,tagged},{?EDX,untagged},
     {?ECX,tagged},{?ECX,untagged},
     {?EBX,tagged},{?EBX,untagged},
     {?EDI,tagged},{?EDI,untagged}].

tailcall_clobbered() ->		% tailcall args shuffle clobbers eax
    [{?EAX,tagged},{?EAX,untagged}].

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
