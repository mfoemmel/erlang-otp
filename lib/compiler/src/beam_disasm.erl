% -*- erlang-indent-level: 4 -*-
%%=======================================================================
%% File        : beam_disasm.erl
%% Author      : Kostis Sagonas
%% Description : Disassembles an R5-R8 .beam file into symbolic BEAM code
%%=======================================================================
%% $Id$
%%=======================================================================
%% Notes:
%%   1. It does NOT work for .beam files of previous BEAM versions.
%%   2. If handling of new BEAM instructions is needed, this should be 
%%      inserted at the end of function resolve_inst().
%%=======================================================================

-module(beam_disasm).

-export([file/1, format_error/1]).

-author("Kostis Sagonas").

-include("beam_opcodes.hrl").

%%-----------------------------------------------------------------------

-define(NO_DEBUG(Str,Xs),ok).
-define(DEBUG(Str,Xs),io:format(Str,Xs)).
-define(exit(Reason),exit({?MODULE,?LINE,Reason})).

%%-----------------------------------------------------------------------
%% Error information

format_error({error, Module, Error}) ->
    Module:format_error(Error);
format_error({internal, Error}) ->
    io_lib:format("~p: disassembly failed with reason ~P.",
		  [?MODULE, Error, 25]).

%%-----------------------------------------------------------------------
%% The main exported function
%%   File is either a file name or a binary containing the code.
%%   Returns `{beam_file, [...]}' or `{error, Module, Reason}'.
%%   Call `format_error({error, Module, Reason})' for an error string.
%%-----------------------------------------------------------------------

file(File) ->
    case beam_lib:info(File) of
	Info when list(Info) ->
	    {value,{chunks,Chunks}} = lists:keysearch(chunks,1,Info),
	    case catch process_chunks(File, Chunks) of
		{'EXIT', Error} ->
		    {error, ?MODULE, {internal, Error}};
		Result ->
		    Result
	    end;
	Error ->
	    Error
    end.

%%-----------------------------------------------------------------------
%% Interface might need to be revised -- do not depend on it.
%%-----------------------------------------------------------------------

process_chunks(F,ChunkInfoList) ->
    {ok,{_,Chunks}} = beam_lib:chunks(F, ["Atom","Code","StrT","ImpT","ExpT"]),
    [{"Atom",AtomBin},{"Code",CodeBin},{"StrT",StrBin},
     {"ImpT",ImpBin},{"ExpT",ExpBin}] = Chunks,
    LambdaBin = optional_chunk(F, "FunT", ChunkInfoList),
    LocBin = optional_chunk(F, "LocT", ChunkInfoList),
    AttrBin = optional_chunk(F, "Attr", ChunkInfoList),
    CompBin = optional_chunk(F, "CInf", ChunkInfoList),
    Atoms = beam_disasm_atoms(AtomBin),
    Exports = beam_disasm_exports(ExpBin,Atoms),
    Imports = beam_disasm_imports(ImpBin,Atoms),
    LocFuns = beam_disasm_exports(LocBin,Atoms),
    Lambdas = beam_disasm_lambdas(LambdaBin,Atoms),
    Str = beam_disasm_strings(StrBin),
    Str1 = binary_to_list(Str),  %% for debugging -- use Str as far as poss.
    Sym_Code = beam_disasm_code(CodeBin,Atoms,Imports,Str,Lambdas),
    Attributes = beam_disasm_attributes(AttrBin),
    CompInfo = beam_disasm_compilation_info(CompBin),
    All = [{exports,Exports},
	   {imports,Imports},
	   {code,Sym_Code},
	   {atoms,Atoms},
	   {local_funs,LocFuns},
	   {strings,Str1},
	   {attributes,Attributes},
	   {comp_info,CompInfo}],
    {beam_file,[Item || {_Key,Data}=Item <- All, Data =/= none]}.

%%-----------------------------------------------------------------------
%% Retrieve an optional chunk or none if the chunk doesn't exist.
%%-----------------------------------------------------------------------

optional_chunk(F, ChunkTag, ChunkInfo) ->
    case lists:keymember(ChunkTag, 1, ChunkInfo) of
	true ->
	    {ok,{_,[{ChunkTag,Chunk}]}} = beam_lib:chunks(F, [ChunkTag]),
	    Chunk;
	false -> none
    end.

%%-----------------------------------------------------------------------
%% UTILITIES -- these actually exist in file "beam_lib"
%%           -- they should be moved into a common utils file.
%%-----------------------------------------------------------------------

i32([X1,X2,X3,X4]) ->
    (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4.

get_int(B) ->
    {I, B1} = split_binary(B, 4),
    {i32(binary_to_list(I)), B1}.

%%-----------------------------------------------------------------------
%% Disassembles the atom table of a BEAM file.
%% - atoms are stored in order 1 ... N (N = Num_atoms, in fact),
%% - each atom name consists of a length byte, followed by that many
%%   bytes of name
%% (nb: atom names max 255 chars?!)
%%-----------------------------------------------------------------------

beam_disasm_atoms(AtomTabBin) ->
    {_NumAtoms,B} = get_int(AtomTabBin),
    disasm_atoms(B).

disasm_atoms(AtomBin) ->
    disasm_atoms(binary_to_list(AtomBin),1).

disasm_atoms([Len|Xs],N) ->
    {AtomName,Rest} = get_atom_name(Len,Xs),
    [{N,list_to_atom(AtomName)}|disasm_atoms(Rest,N+1)];
disasm_atoms([],_) ->
    [].

get_atom_name(Len,Xs) ->
    get_atom_name(Len,Xs,[]).

get_atom_name(N,[X|Xs],RevName) when N > 0 ->
    get_atom_name(N-1,Xs,[X|RevName]);
get_atom_name(0,Xs,RevName) ->
    { lists:reverse(RevName), Xs }.

%%-----------------------------------------------------------------------
%% Disassembles the export table of a BEAM file.
%%-----------------------------------------------------------------------

beam_disasm_exports(none, _) -> none;
beam_disasm_exports(ExpTabBin, Atoms) ->
    {_NumAtoms,B} = get_int(ExpTabBin),
    disasm_exports(B,Atoms).

disasm_exports(Bin,Atoms) ->
    resolve_exports(collect_exports(binary_to_list(Bin)),Atoms).

collect_exports([F3,F2,F1,F0,A3,A2,A1,A0,L3,L2,L1,L0|Exps]) ->
    [{i32([F3,F2,F1,F0]),  % F = function (atom ID)
      i32([A3,A2,A1,A0]),  % A = arity (int)
      i32([L3,L2,L1,L0])}  % L = label (int)
     |collect_exports(Exps)];
collect_exports([]) ->
    [].

resolve_exports(Exps,Atoms) ->
    [ {lookup_key(F,Atoms), A, L} || {F,A,L} <- Exps ].

%%-----------------------------------------------------------------------
%% Disassembles the import table of a BEAM file.
%%-----------------------------------------------------------------------

beam_disasm_imports(ExpTabBin,Atoms) ->
    {_NumAtoms,B} = get_int(ExpTabBin),
    disasm_imports(B,Atoms).

disasm_imports(Bin,Atoms) ->
    resolve_imports(collect_imports(binary_to_list(Bin)),Atoms).

collect_imports([M3,M2,M1,M0,F3,F2,F1,F0,A3,A2,A1,A0|Exps]) ->
    [{i32([M3,M2,M1,M0]),  % M = module (atom ID)
      i32([F3,F2,F1,F0]),  % F = function (atom ID)
      i32([A3,A2,A1,A0])}  % A = arity (int)
     |collect_imports(Exps)];
collect_imports([]) ->
    [].

resolve_imports(Exps,Atoms) ->
    [ {lookup_key(M,Atoms), lookup_key(F,Atoms), A} || {M,F,A} <- Exps ].

%%-----------------------------------------------------------------------
%% Disassembles the lambda (fun) table of a BEAM file.
%%-----------------------------------------------------------------------

beam_disasm_lambdas(none, _) -> none;
beam_disasm_lambdas(<<_:32,Tab/binary>>, Atoms) ->
    disasm_lambdas(Tab, Atoms, 0).

disasm_lambdas(<<F:32,A:32,Lbl:32,Index:32,NumFree:32,OldUniq:32,More/binary>>,
	       Atoms, OldIndex) ->
    Info = {lookup_key(F, Atoms),A,Lbl,Index,NumFree,OldUniq},
    [{OldIndex,Info}|disasm_lambdas(More, Atoms, OldIndex+1)];
disasm_lambdas(<<>>, _, _) -> [].

%%-----------------------------------------------------------------------
%% Disassembles the code chunk of a BEAM file:
%%   - The code is first disassembled into a long list of instructions.
%%   - This list is then split into functions and all names are resolved.
%%-----------------------------------------------------------------------

beam_disasm_code(CodeBin,Atoms,Imports,Str,Lambdas) ->
    [_SS3,_SS2,_SS1,_SS0,  % Sub-Size (length of information before code)
     _IS3,_IS2,_IS1,_IS0,  % Instruction Set Identifier (always 0)
     _OM3,_OM2,_OM1,_OM0,  % Opcode Max
     _L3,_L2,_L1,_L0,_F3,_F2,_F1,_F0|Code] = binary_to_list(CodeBin),
    case catch disasm_code(Code) of
	{'EXIT',Rsn} ->
	    ?NO_DEBUG('code disasm failed: ~p~n',[Rsn]),
	    ?exit(Rsn);
	DisasmCode ->
	    Functions = get_function_chunks(DisasmCode),
	    LocLabels = local_labels(Functions,Atoms),
	    [ resolve_names(F,Atoms,Imports,Str,LocLabels,Lambdas) || F <- Functions ]
    end.

%%-----------------------------------------------------------------------

disasm_code([B|Bs]) ->
    {Instr,RestBs} = disasm_instr(B,Bs),
    [Instr|disasm_code(RestBs)];
disasm_code([]) ->
    [].

%%-----------------------------------------------------------------------
%% Splits the code stream into chunks representing the code of functions.
%%
%% NOTE: code actually looks like
%%   label L1: ... label Ln:
%%     func_info ...
%%   label entry:
%%     ...
%%     <on failure, use label Li to show where things died>
%%     ...
%% So the labels before each func_info should be included as well.
%% Ideally, only one such label is needed, but the BEAM compiler
%% before R8 didn't care to remove the redundant ones.
%%-----------------------------------------------------------------------

get_function_chunks([I|Code]) ->
    {LastI,RestCode,Labs} = split_head_labels(I,Code,[]),
    get_funs(LastI,RestCode,Labs,[]);
get_function_chunks([]) ->
    ?exit(empty_code_segment).

get_funs(PrevI,[I|Is],RevF,RevFs) ->
    case I of
	{func_info,_Info} ->
	    [H|T] = RevF,
	    {Last,Fun,TrailingLabels} = split_head_labels(H,T,[]),
	    get_funs(I, Is, [PrevI|TrailingLabels], add_fun([Last|Fun],RevFs));
	_ ->
	    get_funs(I, Is, [PrevI|RevF], RevFs)
    end;
get_funs(PrevI,[],RevF,RevFs) ->
    case PrevI of
	{int_code_end,[]} ->
	    emit_funs(add_fun(RevF,RevFs));
	_ ->
	    ?DEBUG('warning: code segment did not end with int_code_end~n',[]),
	    emit_funs(add_fun([PrevI|RevF],RevFs))
    end.

split_head_labels({label,L},[I|Code],Labs) ->
    split_head_labels(I,Code,[{label,L}|Labs]);
split_head_labels(I,Code,Labs) ->
    {I,Code,Labs}.

add_fun([],Fs) ->
    Fs;
add_fun(F,Fs) ->
    [ lists:reverse(F) | Fs ].

emit_funs(Fs) ->
    lists:reverse(Fs).

%%-----------------------------------------------------------------------
%% Collects local labels -- I am not sure this is 100% what is needed.
%%-----------------------------------------------------------------------

local_labels(Funs,Atoms) ->
    [ local_label(Fun,Atoms) || Fun <- Funs ].

%% The first clause below attempts to provide some (limited form of)
%% backwards compatibility; it is not needed for .beam files generated
%% by the R8 compiler.  The clause should one fine day be taken out.
local_label([{label,_},{label,L}|Code],Atoms) ->
    local_label([{label,L}|Code],Atoms);
local_label([{label,_},
	     {func_info,[M0,F0,{u,A}]},
	     {label,[{u,L1}]}|_],Atoms) ->
    {atom,M} = resolve_arg(M0,Atoms),
    {atom,F} = resolve_arg(F0,Atoms),
    {L1, {M, F, A}};
local_label(Code,_) ->
    io:format('beam_disasm: no label in ~p~n',[Code]),
    {-666,{none,none,0}}.

%%-----------------------------------------------------------------------
%% Disassembles a single BEAM instruction; most instructions are handled
%% in a generic way; indexing instructions are handled separately.
%%-----------------------------------------------------------------------

disasm_instr(B,Bs) ->
    {SymOp, Arity} = beam_opcodes:opname(B),
    case {SymOp,Arity} of
	{select_val,3} ->
	    disasm_select_inst(select_val,Bs);
	{select_tuple_arity,3} ->
	    disasm_select_inst(select_tuple_arity,Bs);
	_ ->
	    case catch decode_n_args(Arity,Bs) of
		{'EXIT',Rsn} ->
		    ?NO_DEBUG("decode_n_args(~p,~p) failed~n",[Arity,Bs]),
		    {{'EXIT',{SymOp,Arity,Rsn}},[]};
		{Args,RestBs} ->
		    ?NO_DEBUG("instr ~p~n",[{SymOp,Args}]),
		    {{SymOp,Args}, RestBs}
	    end
    end.

%%-----------------------------------------------------------------------
%% Disassembles a BEAM select_* instruction used for indexing.
%%   Currently handles {select_val,3} and {select_tuple_arity,3} insts.
%%
%%   The arruments of a "select"-type instruction look as follows:
%%       <reg>, {f,FailLabel}, {list, <num cases>, [<case1> ... <caseN>]}
%%   where each case is of the form [symbol,{f,Label}].
%%-----------------------------------------------------------------------

disasm_select_inst(Inst,Bs) ->
    {X, Bs1} = decode_arg(Bs),
    {F, Bs2} = decode_arg(Bs1),
    {Z, Bs3} = decode_arg(Bs2),
    {U, Bs4} = decode_arg(Bs3),
    {u,Len} = U,
    {List, RestBs} = decode_n_args(Len,Bs4),
    {{Inst,[X,F,{Z,U,List}]}, RestBs}.

%%-----------------------------------------------------------------------
%% decode_arg([Byte]) -> { Arg, [Byte] }
%%
%% - an arg can have variable length, so we must return arg + remaining bytes
%% - decodes an argument into its 'raw' form: { Tag, Value }
%%   several types map to a single tag, so the byte code instr must then
%%   assign a type to it
%%-----------------------------------------------------------------------

decode_arg([B|Bs]) ->
    Tag = decode_tag(B band 2#111),
    ?NO_DEBUG('Tag = ~p, B = ~p, Bs = ~p~n',[Tag,B,Bs]),
    case Tag of
	z ->
	    decode_z_tagged(Tag,B,Bs);
	_ ->
	    %% all other cases are handled as if they were integers
	    decode_int(Tag,B,Bs)
    end.

%%-----------------------------------------------------------------------
%% Decodes an integer value.  Handles positives, negatives, and bignums.
%%
%% Tries to do the opposite of:
%%   beam_asm:encode(1, 5) =            [81]
%%   beam_asm:encode(1, 1000) =         [105,232]
%%   beam_asm:encode(1, 2047) =         [233,255]
%%   beam_asm:encode(1, 2048) =         [25,8,0]
%%   beam_asm:encode(1,-1) =            [25,255,255]
%%   beam_asm:encode(1,-4294967295) =   [121,255,0,0,0,1]
%%   beam_asm:encode(1, 4294967295) =   [121,0,255,255,255,255]
%%   beam_asm:encode(1, 429496729501) = [121,99,255,255,255,157]
%%-----------------------------------------------------------------------

decode_int(Tag,B,Bs) when (B band 16#08) == 0 ->
    %% N < 16 = 4 bits, NNNN:0:TTT
    N = B bsr 4,
    {{Tag,N},Bs};
decode_int(Tag,B,Bs) when (B band 16#10) == 0 ->
    %% N < 2048 = 11 bits = 3:8 bits, NNN:01:TTT, NNNNNNNN
    [B1|Bs1] = Bs,
    Val0 = B band 2#11100000,
    N = (Val0 bsl 3) bor B1,
    ?NO_DEBUG('NNN:01:TTT, NNNNNNNN = ~n~p:01:~p, ~p = ~p~n', [Val0,Tag,B,N]),
    {{Tag,N},Bs1};
decode_int(Tag,B,Bs) ->
    {Len,Bs1} = decode_int_length(B,Bs),
    {IntBs,RemBs} = take_bytes(Len,Bs1),
    N = build_arg(IntBs),
    [F|_] = IntBs,
    Num = if F > 127, Tag == i -> decode_negative(N,Len);
	     true -> N
	  end,
    ?NO_DEBUG('Len = ~p, IntBs = ~p, Num = ~p~n', [Len,IntBs,Num]),
    {{Tag,Num},RemBs};
decode_int(_,B,_) ->
    ?exit({decode_int,B}).

decode_int_length(B,Bs) ->
    %% The following imitates get_erlang_integer() in beam_load.c
    %% Len is the size of the integer value in bytes
    case B bsr 5 of
	7 ->
	    {Arg,ArgBs} = decode_arg(Bs),
	    case Arg of
		{u,L} ->
		    {L+9,ArgBs};  % 9 stands for 7+2
		_ -> 
		    ?exit({decode_int,weird_bignum_sublength,Arg})
	    end;
	L ->
	    {L+2,Bs}
    end.
    
decode_negative(N,Len) ->
    N - (1 bsl (Len*8)). % 8 is number of bits in a byte

%%-----------------------------------------------------------------------
%% Decodes lists and floating point numbers.
%%-----------------------------------------------------------------------

decode_z_tagged(Tag,B,Bs) when (B band 16#08) == 0 ->
    N = B bsr 4,
    case N of
	0 -> % float
	    decode_float(Bs);
	1 -> % list
	    {{Tag,N},Bs};
	2 -> % fr
	    decode_fr(Bs);
	_ ->
	    ?exit({decode_z_tagged,{invalid_extended_tag,N}})
    end;
decode_z_tagged(_,B,_) ->
    ?exit({decode_z_tagged,{weird_value,B}}).

decode_float(Bs) ->
    {FL,RestBs} = take_bytes(8,Bs),
    <<Float:64/float>> = list_to_binary(FL),
    {{float,Float},RestBs}.

decode_fr(Bs) ->
    {{u,Fr},RestBs} = decode_arg(Bs),
    {{fr,Fr},RestBs}.

%%-----------------------------------------------------------------------
%% take N bytes from a stream, return { Taken_bytes, Remaining_bytes }
%%-----------------------------------------------------------------------

take_bytes(N,Bs) ->
    take_bytes(N,Bs,[]).

take_bytes(N,[B|Bs],Acc) when N > 0 ->
    take_bytes(N-1,Bs,[B|Acc]);
take_bytes(0,Bs,Acc) ->
    { lists:reverse(Acc), Bs }.

%%-----------------------------------------------------------------------
%% from a list of bytes Bn,Bn-1,...,B1,B0
%% build  (Bn << 8*n) bor ... bor B1 << 8 bor B0 << 0
%%-----------------------------------------------------------------------

build_arg(Bs) ->
    build_arg(Bs,0).

build_arg([B|Bs],N) ->
    build_arg(Bs, (N bsl 8) bor B);
build_arg([],N) ->
    N.

%%-----------------------------------------------------------------------
%% Decodes a bunch of arguments and returns them in a list
%%-----------------------------------------------------------------------

decode_n_args(N,Bs) when N >= 0 ->
    decode_n_args(N,[],Bs).

decode_n_args(N,Acc,Bs0) when N > 0 ->
    { A1, Bs1 } = decode_arg(Bs0),
    decode_n_args(N-1,[A1|Acc],Bs1);
decode_n_args(0,Acc,Bs0) ->
    { lists:reverse(Acc), Bs0 }.

%%-----------------------------------------------------------------------
%% Convert a numeric tag value into a symbolic one
%%-----------------------------------------------------------------------

decode_tag(?tag_u) -> u;
decode_tag(?tag_i) -> i;
decode_tag(?tag_a) -> a;
decode_tag(?tag_x) -> x;
decode_tag(?tag_y) -> y;
decode_tag(?tag_f) -> f;
decode_tag(?tag_h) -> h;
decode_tag(?tag_z) -> z;
decode_tag(X) ->
    ?exit({unknown_tag,X}).

%%-----------------------------------------------------------------------
%% - replace all references {a,I} with the atom with index I (or {atom,A})
%% - replace all references to {i,K} in an external call position with
%%    the proper MFA (position in list, first elt = 0, yields MFA to use)
%% - resolve strings, represented as <offset, length>, into their
%%   actual values by using string table
%%    (note: string table should be passed as a BINARY so that we can
%%    use binary_to_list/3!)
%% - convert instruction to its readable form ...
%% 
%% Currently, only the first three are done (systematically, at least).
%%
%% Note: It MAY be premature to remove the lists of args, since that
%%  representation means it is simpler to iterate over all args, etc.
%%-----------------------------------------------------------------------

resolve_names(Fun,Atoms,Imports,Str,Lbls,Lambdas) ->
    [ resolve_inst(Instr,Atoms,Imports,Str,Lbls,Lambdas) || Instr <- Fun ].

%%
%% New make_fun2/4 instruction added in August 2001 (R8).
%% We handle it specially here to avoid adding an argument to
%% the clause for every instruction.
%%

resolve_inst({make_fun2,Args},Atoms,_,_,Lbls,Lambdas) ->
    [OldIndex] = resolve_args(Args,Atoms),
    {value,{OldIndex,{F,A,_Lbl,_Index,NumFree,OldUniq}}} =
	lists:keysearch(OldIndex, 1, Lambdas),
    [{_,{M,_,_}}|_] = Lbls,			% Slighly kludgy.
    {make_fun2,{M,F,A},OldIndex,OldUniq,NumFree};
resolve_inst(Instr,Atoms,Imports,Str,Lbls,_Lambdas) ->
    resolve_inst(Instr,Atoms,Imports,Str,Lbls).

resolve_inst({label,[{u,L}]},_,_,_,_) ->
    {label,L};
resolve_inst({func_info,RawMFA},Atoms,_,_,_) ->
    {func_info,resolve_args(RawMFA,Atoms)};
% resolve_inst(int_code_end,_,_,_,_) ->  % instruction already handled
%    int_code_end;                       % should not really be handled here
resolve_inst({call,[{u,N},{f,L}]},_,_,_,Lbls) ->
    {call,N,catch lookup_key(L,Lbls)};
resolve_inst({call_last,[{u,N},{f,L},{u,U}]},_,_,_,Lbls) ->
    {call_last,N,catch lookup_key(L,Lbls),U};
resolve_inst({call_only,[{u,N},{f,L}]},_,_,_,Lbls) ->
    {call_only,N,catch lookup_key(L,Lbls)};
resolve_inst({call_ext,[{u,N},{u,MFAix}]},_,Imports,_,_) ->
    {call_ext,N,catch lists:nth(MFAix+1,Imports)};
resolve_inst({call_ext_last,[{u,N},{u,MFAix},{u,X}]},_,Imports,_,_) ->
    {call_ext_last,N,catch lists:nth(MFAix+1,Imports),X};
resolve_inst({bif0,Args},Atoms,Imports,_,_) ->
    [Bif,Reg] = resolve_args(Args,Atoms),
    {_Mod,BifName,_Arity} = lists:nth(Bif+1,Imports),
    %?NO_DEBUG('bif0(~p, ~p)~n',[BifName,Reg]),
    {bif0,BifName,Reg};
resolve_inst({bif1,Args},Atoms,Imports,_,_) ->
    [F,Bif,A1,Reg] = resolve_args(Args,Atoms),
    {_Mod,BifName,_Arity} = lists:nth(Bif+1,Imports),
    %?NO_DEBUG('bif1(~p, ~p, ~p, ~p, ~p)~n',[Bif,BifName,F,[A1],Reg]),
    {bif1,BifName,F,[A1],Reg};
resolve_inst({bif2,Args},Atoms,Imports,_,_) ->
    [F,Bif,A1,A2,Reg] = resolve_args(Args,Atoms),
    {_Mod,BifName,_Arity} = lists:nth(Bif+1,Imports),
    %?NO_DEBUG('bif2(~p, ~p, ~p, ~p, ~p)~n',[Bif,BifName,F,[A1,A2],Reg]),
    {bif2,BifName,F,[A1,A2],Reg};
resolve_inst({allocate,[{u,X0},{u,X1}]},_,_,_,_) ->
    {allocate,X0,X1};
resolve_inst({allocate_heap,[{u,X0},{u,X1},{u,X2}]},_,_,_,_) ->
    {allocate_heap,X0,X1,X2};
resolve_inst({allocate_zero,[{u,X0},{u,X1}]},_,_,_,_) ->
    {allocate_zero,X0,X1};
resolve_inst({allocate_heap_zero,[{u,X0},{u,X1},{u,X2}]},_,_,_,_) ->
    {allocate_heap_zero,X0,X1,X2};
resolve_inst({test_heap,[{u,X0},{u,X1}]},_,_,_,_) ->
    {test_heap,X0,X1};
resolve_inst({init,[Dst]},_,_,_,_) ->
    {init,Dst};
resolve_inst({deallocate,[{u,L}]},_,_,_,_) ->
    {deallocate,L};
resolve_inst({return,[]},_,_,_,_) ->
    return;
resolve_inst({send,[]},_,_,_,_) ->
    send;
resolve_inst({remove_message,[]},_,_,_,_) ->
    remove_message;
resolve_inst({timeout,[]},_,_,_,_) ->
    timeout;
resolve_inst({loop_rec,[Lbl,Dst]},_,_,_,_) ->
    {loop_rec,Lbl,Dst};
resolve_inst({loop_rec_end,[Lbl]},_,_,_,_) ->
    {loop_rec_end,Lbl};
resolve_inst({wait,[Lbl]},_,_,_,_) ->
    {wait,Lbl};
resolve_inst({wait_timeout,[Lbl,Int]},Atoms,_,_,_) ->
    {wait_timeout,Lbl,resolve_arg(Int,Atoms)};
resolve_inst({m_plus,Args},Atoms,_,_,_) ->
    [W,SrcR1,SrcR2,DstR] = resolve_args(Args,Atoms),
    {arith, {'+', [SrcR1,SrcR2], DstR}, W};
resolve_inst({m_minus,Args},Atoms,_,_,_) ->
    [W,SrcR1,SrcR2,DstR] = resolve_args(Args,Atoms),
    {arith, {'-', [SrcR1,SrcR2], DstR}, W};
resolve_inst({m_times,Args},Atoms,_,_,_) ->
    [W,SrcR1,SrcR2,DstR] = resolve_args(Args,Atoms),
    {arith, {'*', [SrcR1,SrcR2], DstR}, W};
resolve_inst({m_div,Args},Atoms,_,_,_) ->
    [W,SrcR1,SrcR2,DstR] = resolve_args(Args,Atoms),
    {arith, {'/', [SrcR1,SrcR2], DstR}, W};
resolve_inst({int_div,Args},Atoms,_,_,_) ->
    [W,SrcR1,SrcR2,DstR] = resolve_args(Args,Atoms),
    {arith, {'div', [SrcR1,SrcR2], DstR}, W};
resolve_inst({int_rem,Args},Atoms,_,_,_) ->
    [W,SrcR1,SrcR2,DstR] = resolve_args(Args,Atoms),
    {arith, {'rem', [SrcR1,SrcR2], DstR}, W};
resolve_inst({int_band,Args},Atoms,_,_,_) ->
    [W,SrcR1,SrcR2,DstR] = resolve_args(Args,Atoms),
    {arith, {'band', [SrcR1,SrcR2], DstR}, W};
resolve_inst({int_bor,Args},Atoms,_,_,_) ->
    [W,SrcR1,SrcR2,DstR] = resolve_args(Args,Atoms),
    {arith, {'bor', [SrcR1,SrcR2], DstR}, W};
resolve_inst({int_bxor,Args},Atoms,_,_,_) ->
    [W,SrcR1,SrcR2,DstR] = resolve_args(Args,Atoms),
    {arith, {'bxor', [SrcR1,SrcR2], DstR}, W};
resolve_inst({int_bsl,Args},Atoms,_,_,_) ->
    [W,SrcR1,SrcR2,DstR] = resolve_args(Args,Atoms),
    {arith, {'bsl', [SrcR1,SrcR2], DstR}, W};
resolve_inst({int_bsr,Args},Atoms,_,_,_) ->
    [W,SrcR1,SrcR2,DstR] = resolve_args(Args,Atoms),
    {arith, {'bsr', [SrcR1,SrcR2], DstR}, W};
resolve_inst({int_bnot,Args},Atoms,_,_,_) -> % slightly differs from the rest
    [W,SrcR,DstR] = resolve_args(Args,Atoms),
    {arith, {'bnot', [SrcR], DstR}, W};
resolve_inst({is_lt,Args},Atoms,_,_,_) ->
    [L,Src1,Src2] = resolve_args(Args,Atoms),
    {is_lt,L,Src1,Src2};
resolve_inst({is_ge,Args},Atoms,_,_,_) ->
    [L,Src1,Src2] = resolve_args(Args,Atoms),
    {is_ge,L,Src1,Src2};
resolve_inst({is_eq,Args},Atoms,_,_,_) ->
    [L,Src1,Src2] = resolve_args(Args,Atoms),
    {is_eq,L,Src1,Src2};
resolve_inst({is_ne,Args},Atoms,_,_,_) ->
    [L,Src1,Src2] = resolve_args(Args,Atoms),
    {is_ne,L,Src1,Src2};
resolve_inst({is_eq_exact,Args},Atoms,_,_,_) ->
    [L,Src1,Src2] = resolve_args(Args,Atoms),
    {is_eq_exact,L,Src1,Src2};
resolve_inst({is_ne_exact,Args},Atoms,_,_,_) ->
    [L,Src1,Src2] = resolve_args(Args,Atoms),
    {is_ne_exact,L,Src1,Src2};
resolve_inst({is_integer,Args},Atoms,_,_,_) ->
    [L,Src] = resolve_args(Args,Atoms),
    {is_integer,L,Src};
resolve_inst({is_float,Args},Atoms,_,_,_) ->
    [L,Src] = resolve_args(Args,Atoms),
    {is_float,L,Src};
resolve_inst({is_number,Args},Atoms,_,_,_) ->
    [L,Src] = resolve_args(Args,Atoms),
    {is_number,L,Src};
resolve_inst({is_atom,Args},Atoms,_,_,_) ->
    [L,Src] = resolve_args(Args,Atoms),
    {is_atom,L,Src};
resolve_inst({is_pid,Args},Atoms,_,_,_) ->
    [L,Src] = resolve_args(Args,Atoms),
    {is_pid,L,Src};
resolve_inst({is_ref,Args},Atoms,_,_,_) ->
    [L,Src] = resolve_args(Args,Atoms),
    {is_ref,L,Src};
resolve_inst({is_port,Args},Atoms,_,_,_) ->
    [L,Src] = resolve_args(Args,Atoms),
    {is_port,L,Src};
resolve_inst({is_nil,Args},Atoms,_,_,_) ->
    [L,Src] = resolve_args(Args,Atoms),
    {is_nil,L,Src};
resolve_inst({is_binary,Args},Atoms,_,_,_) ->
    [L,Src] = resolve_args(Args,Atoms),
    {is_binary,L,Src};
resolve_inst({is_constant,Args},Atoms,_,_,_) ->
    [L,Src] = resolve_args(Args,Atoms),
    {is_constant,L,Src};
resolve_inst({is_list,Args},Atoms,_,_,_) ->
    [L,Src] = resolve_args(Args,Atoms),
    {is_list,L,Src};
resolve_inst({is_nonempty_list,Args},Atoms,_,_,_) ->
    [L,Src] = resolve_args(Args,Atoms),
    {is_nonempty_list,L,Src};
resolve_inst({is_tuple,Args},Atoms,_,_,_) ->
    [L,Src] = resolve_args(Args,Atoms),
    {is_tuple,L,Src};
resolve_inst({test_arity,Args},Atoms,_,_,_) ->
    [L,Src1,Src2] = resolve_args(Args,Atoms),
    {test_arity,L,Src1,Src2};
resolve_inst({select_val,Args},Atoms,_,_,_) ->
    [Reg,FLbl,{{z,1},{u,Len},List}] = Args,
    CasePairs = form_case_pairs(resolve_args(List,Atoms)),
    {select_val,Reg,FLbl,Len div 2,CasePairs};
resolve_inst({select_tuple_arity,Args},Atoms,_,_,_) ->
    [Reg,FLbl,{{z,1},{u,Len},List}] = Args,
    CasePairs = form_case_pairs(resolve_args(List,Atoms)),
    {select_tuple_arity,Reg,FLbl,Len div 2,CasePairs};
resolve_inst({jump,[Lbl]},_,_,_,_) ->
    {jump,Lbl};
resolve_inst({'catch',[Dst,Lbl]},_,_,_,_) ->
    {'catch',Dst,Lbl};
resolve_inst({catch_end,[Dst]},_,_,_,_) ->
    {catch_end,Dst};
resolve_inst({move,[Src,Dst]},Atoms,_,_,_) ->
    {move,resolve_arg(Src,Atoms),Dst};
resolve_inst({get_list,[Src,Dst1,Dst2]},_,_,_,_) ->
    {get_list,Src,Dst1,Dst2};
resolve_inst({get_tuple_element,[Src,{u,Off},Dst]},Atoms,_,_,_) ->
    {get_tuple_element,resolve_arg(Src,Atoms),Off,resolve_arg(Dst,Atoms)};
resolve_inst({set_tuple_element,[Src,Dst,{u,Off}]},Atoms,_,_,_) ->
    {set_tuple_element,resolve_arg(Src,Atoms),resolve_arg(Dst,Atoms),Off};
resolve_inst({put_string,[{u,Len},{u,Off},Dst]},_,_,Strings,_) ->
    String = if Len > 0 -> binary_to_list(Strings, Off+1, Off+Len);
		true -> ""
	     end,
    ?NO_DEBUG('put_string(~p, {string,~p}, ~p)~n',[Len,String,Dst]),
    {put_string,Len,{string,String},Dst};
resolve_inst({put_list,[Src1,Src2,Dst]},Atoms,_,_,_) ->
    {put_list,resolve_arg(Src1,Atoms),resolve_arg(Src2,Atoms),Dst};
resolve_inst({put_tuple,[{u,Arity},Dst]},_,_,_,_) ->
    {put_tuple,Arity,Dst};
resolve_inst({put,[Src]},Atoms,_,_,_) ->
    {put,resolve_arg(Src,Atoms)};
resolve_inst({badmatch,[X]},_,_,_,_) ->
    {badmatch,X};
resolve_inst({if_end,[]},_,_,_,_) ->
    if_end;
resolve_inst({case_end,[X]},Atoms,_,_,_) ->
    {case_end,resolve_arg(X, Atoms)};
resolve_inst({call_fun,[{u,N}]},_,_,_,_) ->
    {call_fun,N};
resolve_inst({make_fun,Args},Atoms,_,_,Lbls) ->
    [{f,L},Magic,FreeVars] = resolve_args(Args,Atoms),
    {make_fun,catch lookup_key(L,Lbls),Magic,FreeVars};
resolve_inst({is_function,[F,X]},_,_,_,_) ->
    {is_function,F,X};
resolve_inst({call_ext_only,[{u,N},{u,MFAix}]},_,Imports,_,_) ->
    {call_ext_only,N,catch lists:nth(MFAix+1,Imports)};
%%
%% Instructions for handling binaries added in R7A & R7B
%%
resolve_inst({bs_start_match,[F,Reg]},_,_,_,_) ->
    {bs_start_match,F,Reg};
resolve_inst({bs_get_integer,[Lbl,Arg2,{u,N},{u,U},Arg5]},Atoms,_,_,_) ->
    [A2,A5] = resolve_args([Arg2,Arg5],Atoms),
    ?NO_DEBUG('bs_get_integer(~p,~p,~p,~p,~p})~n',[Lbl,A2,N,U,A5]),
    {bs_get_integer,Lbl,A2,N,decode_field_flags(U),A5};
resolve_inst({bs_get_float,[Lbl,Arg2,{u,N},{u,U},Arg5]},Atoms,_,_,_) ->
    [A2,A5] = resolve_args([Arg2,Arg5],Atoms),
    ?NO_DEBUG('bs_get_float(~p,~p,~p,~p,~p})~n',[Lbl,A2,N,U,A5]),
    {bs_get_float,Lbl,A2,N,decode_field_flags(U),A5};
resolve_inst({bs_get_binary,[Lbl,Arg2,{u,N},{u,U},Arg5]},Atoms,_,_,_) ->
    [A2,A5] = resolve_args([Arg2,Arg5],Atoms),
    ?NO_DEBUG('bs_get_binary(~p,~p,~p,~p,~p)~n',[Lbl,A2,N,U,A5]),
    {bs_get_binary,Lbl,A2,N,decode_field_flags(U),A5};
resolve_inst({bs_skip_bits,[Lbl,Arg2,{u,N},{u,U}]},Atoms,_,_,_) ->
    [A2] = resolve_args([Arg2],Atoms),
    ?NO_DEBUG('bs_skip_bits(~p,~p,~p,~p)~n',[Lbl,A2,N,U]),
    {bs_skip_bits,Lbl,A2,N,decode_field_flags(U)};
resolve_inst({bs_test_tail,[F,{u,N}]},_,_,_,_) ->
    {bs_test_tail,F,N};
resolve_inst({bs_save,[{u,N}]},_,_,_,_) ->
    {bs_save,N};
resolve_inst({bs_restore,[{u,N}]},_,_,_,_) ->
    {bs_restore,N};
resolve_inst({bs_init,[{u,N},{u,U}]},_,_,_,_) ->
    {bs_init,N,decode_field_flags(U)};
resolve_inst({bs_final,[F,X]},_,_,_,_) ->
    {bs_final,F,X};
resolve_inst({bs_put_integer,[Lbl,Arg2,{u,N},{u,U},Arg5]},Atoms,_,_,_) ->
    [A2,A5] = resolve_args([Arg2,Arg5],Atoms),
    {bs_put_integer,Lbl,A2,N,decode_field_flags(U),A5};
resolve_inst({bs_put_binary,[Lbl,Arg2,{u,N},{u,U},Arg5]},Atoms,_,_,_) ->
    [A2,A5] = resolve_args([Arg2,Arg5],Atoms),
    ?NO_DEBUG('bs_put_binary(~p,~p,~p,~p,~p})~n',[Lbl,A2,N,U,A5]),
    {bs_put_binary,Lbl,A2,N,decode_field_flags(U),A5};
resolve_inst({bs_put_float,[Lbl,Arg2,{u,N},{u,U},Arg5]},Atoms,_,_,_) ->
    [A2,A5] = resolve_args([Arg2,Arg5],Atoms),
    ?NO_DEBUG('bs_put_float(~p,~p,~p,~p,~p})~n',[Lbl,A2,N,U,A5]),
    {bs_put_float,Lbl,A2,N,decode_field_flags(U),A5};
resolve_inst({bs_put_string,[{u,Len},{u,Off}]},_,_,Strings,_) ->
    String = if Len > 0 -> binary_to_list(Strings, Off+1, Off+Len);
		true -> ""
	     end,
    ?NO_DEBUG('bs_put_string(~p, {string,~p})~n',[Len,String]),
    {bs_put_string,Len,{string,String}};
resolve_inst({bs_need_buf,[{u,N}]},_,_,_,_) ->
    {bs_need_buf,N};

%%
%% Instructions for handling floating point numbers added in June 2001 (R8).
%%
resolve_inst({fclearerror,[]},_,_,_,_) ->
    fclearerror;
resolve_inst({fcheckerror,Args},Atoms,_,_,_) ->
    [Fail] = resolve_args(Args,Atoms),
    {fcheckerror,Fail};
resolve_inst({fmove,Args},Atoms,_,_,_) ->
    [FR,Reg] = resolve_args(Args,Atoms),
    {fmove,FR,Reg};
resolve_inst({fconv,Args},Atoms,_,_,_) ->
    [Reg,FR] = resolve_args(Args,Atoms),
    {fconv,Reg,FR};
resolve_inst({fadd,Args},Atoms,_,_,_) ->
    [F,A1,A2,Reg] = resolve_args(Args,Atoms),
    {fadd,F,[A1,A2],Reg};
resolve_inst({fsub,Args},Atoms,_,_,_) ->
    [F,A1,A2,Reg] = resolve_args(Args,Atoms),
    {fsub,F,[A1,A2],Reg};
resolve_inst({fmul,Args},Atoms,_,_,_) ->
    [F,A1,A2,Reg] = resolve_args(Args,Atoms),
    {fmul,F,[A1,A2],Reg};
resolve_inst({fdiv,Args},Atoms,_,_,_) ->
    [F,A1,A2,Reg] = resolve_args(Args,Atoms),
    {fdiv,F,[A1,A2],Reg};
resolve_inst({fnegate,Args},Atoms,_,_,_) ->
    [F,Arg,Reg] = resolve_args(Args,Atoms),
    {fnegate,F,[Arg],Reg};

%%
%% Catches instructions that are not yet handled.
%%

resolve_inst(X,_,_,_,_) ->
    ?exit({resolve_inst,X}).

%%-----------------------------------------------------------------------
%% Resolves arguments in a generic way.
%%-----------------------------------------------------------------------

resolve_args(Args,Atoms) ->
    [ resolve_arg(Arg,Atoms) || Arg <- Args ].

resolve_arg({u,N},_) ->
    N;
resolve_arg({i,N},_) ->
    {integer,N};
resolve_arg({a,0},_) ->
    nil;
resolve_arg({a,I},Atoms) ->
    {atom,catch lookup_key(I,Atoms)};
resolve_arg(Arg,_) ->
    Arg.

%%-----------------------------------------------------------------------
%% The purpose of the following is just to add a hook for future changes.
%% Currently, field flags are numbers 1-2-4-8 and only two of these
%% numbers (BSF_LITTLE 2 -- BSF_SIGNED 4) have a semantic significance;
%% others are just hints for speeding up the execution; see "erl_bits.h".
%%-----------------------------------------------------------------------

decode_field_flags(FF) ->
    {field_flags,FF}.

%%-----------------------------------------------------------------------
%% Each string is denoted in the assembled code by its offset into this
%% binary.  This binary contains all strings concatenated together.
%%-----------------------------------------------------------------------

beam_disasm_strings(Bin) ->
    Bin.

%%-----------------------------------------------------------------------
%% Disassembles the attributes of a BEAM file.
%%-----------------------------------------------------------------------

beam_disasm_attributes(none) -> none;
beam_disasm_attributes(AttrBin) -> binary_to_term(AttrBin).

%%-----------------------------------------------------------------------
%% Disassembles the compilation information of a BEAM file.
%%-----------------------------------------------------------------------

beam_disasm_compilation_info(none) -> none;
beam_disasm_compilation_info(Bin) ->  binary_to_term(Bin).

%%-----------------------------------------------------------------------
%% Private Utilities
%%-----------------------------------------------------------------------

form_case_pairs([A,L|Rest]) ->
    [{A,L}|form_case_pairs(Rest)];
form_case_pairs([]) ->
    [].

%%-----------------------------------------------------------------------

lookup_key(Key,[{Key,Val}|_]) ->
    Val;
lookup_key(Key,[_|KVs]) ->
    lookup_key(Key,KVs);
lookup_key(Key,[]) ->
    ?exit({lookup_key,{key_not_found,Key}}).

%%-----------------------------------------------------------------------
