% hipe_x86_ext_format.hrl
% Definitions for unified external object format
% Currently: x86, sparc
% Copyright (C) 2001 Ulf Magnusson
% Email: ulf.magnusson@ubm-computing.com

-define(LOAD_ATOM,0).
-define(CALL_BIF,1).
-define(LOAD_ADDRESS,2).
-define(MFA,3).
-define(CALL_REMOTE,4).
-define(CALL_LOCAL_COLD,5).
-define(CALL_LOCAL,6).
-define(CALL_LOCAL_HOT,7).
-define(SDESC,8).

-define(TERM,0).
-define(BLOCK,1).
-define(SORTEDBLOCK,2).

-define(TRUE,1).
-define(FALSE,0).


-define(CONST_TYPE2EXT(T),
	case T of
	    term -> ?TERM;
	    sorted_block -> ?SORTEDBLOCK;
            block -> ?BLOCK
        end).

-define(EXT2CONST_TYPE(E),
	case E of
	    ?TERM -> term;
	    ?SORTEDBLOCK -> sorted_block;
	    ?BLOCK -> block
	end).

-define(BOOL2EXT(B),
	case B of
	    true -> ?TRUE;
	    false -> ?FALSE
        end).

-define(EXT2BOOL(E),
	case E of
	    ?TRUE -> true;
	    ?FALSE -> false
	end).

-define(PATCH_TYPE2EXT(A),
	case A of
	    load_atom -> ?LOAD_ATOM;
	    mfa -> ?MFA;
	    load_address -> ?LOAD_ADDRESS;
	    call_bif -> ?CALL_BIF;
	    sdesc -> ?SDESC;
	    {call_link,localfun,hot} ->  ?CALL_LOCAL_HOT;
	    {call_link,localfun,cold} ->  ?CALL_LOCAL_COLD;
	    {call_link,localfun,none} ->  ?CALL_LOCAL;
	    {call_link,remotefun} ->  ?CALL_REMOTE
        end).

-define(EXT2PATCH_TYPE(E),
	case E of
	    ?LOAD_ATOM -> load_atom;
	    ?MFA -> mfa;
	    ?LOAD_ADDRESS -> load_address;
	    ?CALL_BIF -> call_bif;
	    ?SDESC -> sdesc;
	    ?CALL_REMOTE -> call_remote;
	    ?CALL_LOCAL_HOT -> call_local_hot;
	    ?CALL_LOCAL_COLD -> call_local_cold;
	    ?CALL_LOCAL -> call_local
	end).






























