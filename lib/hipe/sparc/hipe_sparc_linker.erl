%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2000 by Erik Johansson.  All Rights Reserved 
%% Time-stamp: <01/07/04 11:28:35 happi>
%% ====================================================================
%%  Filename : 	hipe_sparc_linker.erl
%%  Module   :	hipe_sparc_linker
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2000-10-30 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: kostis $
%%              $Date: 2001/08/16 10:48:49 $
%%              $Revision: 1.8 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc_linker).
-export([link/5, pack_constants/1, preprocess/5, assemble/3]).

-include("../../kernel/src/hipe_ext_format.hrl").
-include("../main/hipe.hrl").

%%=====================================================================

link(MFA, Code, IsClosure, ConstTab, Flag) ->
  Closures =
    if IsClosure =:= true -> [MFA];
       true -> []
    end,

  {ConstSize, ConstMap, RefsFromConsts} = 
    pack_constants(MFA, ConstTab),

  {HotCode, ColdCode, HotSize, ColdSize, ExportMap} = 
    preprocess(Code, MFA),

  NewCode = [{MFA, HotCode, ColdCode}],


  {HotSize, ColdSize, AsmHCode,AsmCCode, HotRefs, ColdRefs} = 
    assemble(NewCode, 
	     init_export_map(ExportMap), %% Convert to more efficent
	     init_const_map(ConstMap)),  %%  datastructures.

  AsmCode = {{HotSize,AsmHCode, HotRefs},
	     {ColdSize,AsmCCode, ColdRefs}},

  Bin = hipe_sparc_saver:mk_external(ConstSize, 
				     ConstMap, 
				     RefsFromConsts, 
				     ExportMap,
				     AsmCode, 
				     Closures),
  {Mod,_,_} = MFA,
  hipe_sparc_loader:load(Mod, Bin).
  

%% Move to sparc_assembler...
assemble(Code,Map,ConstMap) ->
  assemble1(Code,0,0,
	    init_export_map(Map),      %% Convert to more efficent
	    init_const_map(ConstMap),  %%  datastructures.
	    [],[],[],[]).

assemble1([{MFA,Hot,Cold}|Rest],HAddr,CAddr,Map,ConstMap,AccHCode,AccCCode,AccHRefs,AccCRefs) ->

  {HCode,NewHAddr,HRefs} = 
    assemble(Hot,MFA,HAddr,
	     local_labels(MFA,Map),
	     ConstMap,AccHRefs,[],hot),
  {CCode,NewCAddr,CRefs} = assemble(Cold,MFA,CAddr,
				    local_labels(MFA,Map),
				    ConstMap,AccCRefs,[],cold),
  assemble1(Rest,NewHAddr,NewCAddr,Map,ConstMap,AccHCode++HCode,AccCCode++CCode,HRefs,CRefs);
assemble1([],HAddr,CAddr,Map,ConstMap,AccHCode,AccCCode,AccHRefs,AccCRefs) ->
  {HAddr,CAddr,AccHCode,AccCCode,AccHRefs,AccCRefs}.

assemble([I|Is],MFA,Addr,Map,ConstMap,Refs,Code,Seg) ->
%%  io:format("~w ~w\n",[Addr,I]),
  Type = hipe_sparc:type(I),

  case  
    case Type of
      call_link -> resolve_call_link(I,Addr,Refs,MFA,Map,Seg);
      b -> resolve_b(I,Addr,Refs,MFA,Map,Seg);
      goto -> resolve_goto(I,Addr,Refs,MFA,Map,Seg);
      load_address -> resolve_load_address(I,Addr,Refs,MFA,Map,ConstMap);
      load_atom -> resolve_load_atom(I,Addr,Refs,MFA,Map);
      load_word_index -> resolve_load_word_index(I,Addr,Refs,MFA,Map,ConstMap);
      alu -> {hipe_sparc_assemble:assemble_alu(I),Addr+4,Refs};
      alu_cc -> {hipe_sparc_assemble:assemble_alu_cc(I),Addr+4,Refs};
      store -> {hipe_sparc_assemble:assemble_store(I),Addr+4,Refs};
      move -> {hipe_sparc_assemble:assemble_move(I),Addr+4,Refs};
      load -> {hipe_sparc_assemble:assemble_load(I),Addr+4,Refs};
      jmp -> {hipe_sparc_assemble:assemble_jmp(I),Addr+4,Refs};
      nop -> {hipe_sparc_assemble:assemble_nop(I),Addr+4,Refs};
      sethi -> {hipe_sparc_assemble:assemble_sethi(I),Addr+4,Refs};
      Other -> exit({bad_type,Other})
    end of
    {[I1,I2],NewAddr,NewRefs} ->
      assemble(Is,MFA,NewAddr,Map,ConstMap,NewRefs,[I1,I2|Code],Seg);
    {C,NewAddr,NewRefs}  ->
      assemble(Is,MFA,NewAddr,Map,ConstMap,NewRefs,[C|Code],Seg)
  end;
assemble([],_,Addr,_,_,Refs,Code,_) ->
  {lists:reverse(Code),Addr,Refs}.

resolve_load_address(Instr,Addr,Refs,MFA,Map,ConstMap)->
  Dest = hipe_sparc:load_address_dest(Instr),
  Address = hipe_sparc:load_address_address(Instr),
  {AbsoluteAddress,Ref} = 
    case hipe_sparc:load_address_type(Instr) of
      label ->
	{Zone,Offset} = find(Address, Map),
	{Offset,[{?PATCH_TYPE2EXT(load_address),
		 Addr,
		 {label,Zone,Offset,MFA}}]}; 
      function -> 
	case hipe_bif:is_bif(Address) of
	  false ->
	    case lists:member(local,hipe_sparc:info(Instr)) of
	      true -> %% local enter
		{0,[{?PATCH_TYPE2EXT(load_address),
		     Addr, {local_function,Address}}]};
	      false -> %% remote enter
		{0,[{?PATCH_TYPE2EXT(load_address),
		     Addr, {remote_function,Address}}]}
	    end;
	  true -> 
	    {0, %% bif_address(Address),
	     [{?PATCH_TYPE2EXT(load_address),
		Addr, {bif,Address}}]}
	end;
      constant ->
	ConstNo = find_const({MFA,Address},ConstMap),
	{0,
	 [{?PATCH_TYPE2EXT(load_address),
		Addr, {const, ConstNo}}]};
      'catch' ->
	{Zone,Offset} = find(Address,Map),

	%% CatchIndex=hipe_bifs:catch_table_insert(LabelAddress,hipe_bifs:catch_table_nil()),
        %% hipe_bifs:catch_index_to_word(CatchIndex),
	{0,[{?PATCH_TYPE2EXT(load_address),
		Addr, {'catch', Zone,Offset,MFA}}]};
      closure ->
	{0,[{?PATCH_TYPE2EXT(load_address),
		     Addr, {closure, Address}}]};
      Type ->     
	exit([{problem,{not_handled,{address,Type}}},{at,Instr}])
    end,
  
  Hi = hipe_sparc:mk_imm(hipe_sparc_op:high22(AbsoluteAddress)),
  Lo = hipe_sparc:mk_imm(hipe_sparc_op:bits_10(AbsoluteAddress)),
  I1 = hipe_sparc:sethi_create(Dest,Hi,[]),
  I2 = hipe_sparc:alu_create(Dest,Dest,'or',Lo,[]),

  {[
    hipe_sparc_assemble:assemble_instr(alu,I2),
    hipe_sparc_assemble:assemble_instr(sethi,I1)],
   Addr+8,
   Ref++Refs}.

resolve_goto(I,Addr,Refs,MFA,Map,Seg) ->
  Dest = hipe_sparc:goto_label(I),
  {DestSeg,Address} = find(Dest,Map),
  RelDest = (Address - Addr) div 4,
  if DestSeg == Seg ->
      case catch hipe_sparc_assert:check_branch_length(RelDest) of
	true -> true;
	{'EXIT',{too_long_branch,Length}} ->
	  exit([{problem,too_long_branch},
		{address,Addr},
		{length,Length}])
      end,
      NewI = hipe_sparc:goto_label_update(I,RelDest),
      Code = hipe_sparc_assemble:assemble_instr(goto,NewI),
      {Code,Addr+4,Refs};
     true ->
      NewI = hipe_sparc:goto_label_update(I,0),
      Code = hipe_sparc_assemble:assemble_instr(goto,NewI),
      {Code,Addr+4,[{goto,Addr,{DestSeg,Address}}|Refs]}
  end.
  
resolve_b(I,Addr,Refs,MFA,Map,Seg) ->
  Dest = hipe_sparc:b_label(I),
  {DestSeg,Address} = find(Dest,Map),
  RelDest = (Address - Addr) div 4,
  if DestSeg == Seg ->
      case catch hipe_sparc_assert:check_branch_length(RelDest) of
	true -> true;
	{'EXIT',{too_long_branch,Length}} ->
	  exit([{problem,too_long_branch},
		{address,Addr},
		{length,Length}])
      end,
      NewI = hipe_sparc:b_label_update(I,RelDest),
      Code = hipe_sparc_assemble:assemble_instr(b,NewI),
      {Code,Addr+4,Refs};
     true ->
      NewI = hipe_sparc:b_label_update(I,0),
      Code = hipe_sparc_assemble:assemble_instr(b,NewI),
      {Code,Addr+4,[{b,Addr,{DestSeg,Address}}|Refs]}
  end.

resolve_load_atom(Instr,Addr,Refs,MFA,Map)->
  Atom = hipe_sparc:load_atom_atom(Instr),
  AtomRep = 0, %% atom_to_word(Atom), 
  Dest = hipe_sparc:load_atom_dest(Instr),
  Info = hipe_sparc:info(Instr),
  I1 = hipe_sparc:sethi_create(Dest,
			  hipe_sparc:mk_imm(AtomRep bsr 10),Info),
  I2 = hipe_sparc:alu_create(Dest,Dest, 'or', 
			hipe_sparc:mk_imm(AtomRep band 16#3FF),Info),
  {[hipe_sparc_assemble:assemble_instr(alu,I2),
    hipe_sparc_assemble:assemble_instr(sethi,I1)],
   Addr+8,[{?PATCH_TYPE2EXT(load_atom),Addr, Atom}|Refs]}.

resolve_load_word_index(Instr,Addr,Refs,MFA,Map,ConstMap) ->
  Index =hipe_sparc:load_word_index_index(Instr),
  Block =hipe_sparc:load_word_index_block(Instr),
  Dest = hipe_sparc:load_word_index_dest(Instr),
  Info = hipe_sparc:info(Instr),
  ConstNo = find_const({MFA,Block},ConstMap),
  AtomRep = 0,
  I1 = hipe_sparc:sethi_create(Dest,
			       hipe_sparc:mk_imm(AtomRep bsr 10),Info),
  I2 = hipe_sparc:alu_create(Dest,Dest, 'or', 
			     hipe_sparc:mk_imm(AtomRep band 16#3FF),Info),
  {[hipe_sparc_assemble:assemble_instr(alu,I2),
    hipe_sparc_assemble:assemble_instr(sethi,I1)],
   Addr+8,[{?PATCH_TYPE2EXT(load_word_index),Addr, {word_index, ConstNo, Index}}|Refs]}.
   
resolve_call_link(Instr,Addr,Refs,MFA,Map,Seg)->
  %% TODO: Make this simpler since the address is of no importance.
  Target = hipe_sparc:call_link_target(Instr),

  case hipe_sparc:call_link_type(Instr) of
    closure -> 
      NewI = hipe_sparc:jmp_link_create(
	       hipe_sparc:call_link_target(Instr),
	       hipe_sparc:mk_imm(0), 
	       hipe_sparc:mk_reg(hipe_sparc_registers:return_address()),
	       hipe_sparc:call_link_args(Instr),
	       hipe_sparc:info(Instr)),
      Code =  hipe_sparc_assemble:assemble_instr(jmp_link,NewI),
      {Code,Addr+4, Refs};
    _ -> 
      case hipe_bif:is_bif(Target) of 
	true ->
	  RelTargetAdr = 0, %% bif_address(Target) - Addr
	  NewI = hipe_sparc:call_link_target_update(Instr, RelTargetAdr),
	  Code = hipe_sparc_assemble:assemble_instr(call_link,NewI),
	  {Code,Addr+4,
	   [{?PATCH_TYPE2EXT(call_bif),
	     Addr, Target}| Refs]};
	false -> %% Not a BIF
%	  DestMod = element(1, Target),
%	  Mod = element(1,MFA),
%	  {RelDest,NewRefs} =

%	    if Mod == DestMod ->  %% 'Local call'
%		{DestSeg,FunAddress} = find({Target,entry},Map),
%		%%	  io:format("Found Addr ~w\n",[FunAddress]),
%		RelDest1 = 
%		  if DestSeg == Seg -> FunAddress - Addr;
%		     true -> FunAddress
%		  end,
		NewRefs =
		  case lists:member(local,hipe_sparc:info(Instr)) of
		    true -> %% true local call
%		      {DestSeg,FunAddress} = find({Target,entry},Map),
		      %% io:format("True local ~w ~w\n",[MFA, Target]),
		      [{?PATCH_TYPE2EXT(
			  {call_link,localfun,none}),
			Addr, Target} | Refs];
		    _ -> %% Remote call to local fun
		      %% io:format("Remote local ~w ~w\n",[MFA, Target]),
		      [{?PATCH_TYPE2EXT(
			  {call_link, remotefun}),
			Addr, Target} | Refs]
		  end,
%		{RelDest1, NewRefs1};
%	  {0,NewRefs1},
%	       true -> %% 'non-local' call
%		%% io:format("Remote call ~w ~w\n",[MFA, Target]),
%		{0,
%		 [{?PATCH_TYPE2EXT({call_link,
%						    remotefun}),
%		   Addr, Target}| Refs]}
%	    end,
	  UsedRelDest = 0,
	  NewI = hipe_sparc:call_link_target_update(Instr,UsedRelDest),
	  Code = hipe_sparc_assemble:assemble_instr(call_link,NewI),
	  {Code,Addr+4,NewRefs}
      end
  end.


  
      

%% Use bif:bif_addres/1.
%bif_address({M,F,A}) ->
%  case catch hipe_bifs:bif_address(M,F,A) of
%    Address when integer(Address) ->  
%      Address;
%    {'EXIT',_} ->
%      exit({no_internal_function,{M,F,A}})
%  end;
%bif_address(Name) ->
%  case catch hipe_bifs:primop_address(Name) of
%    Address when integer(Address) ->  
%      Address;
%    {'EXIT',_} ->
%      exit({no_internal_function,Name})
%  end.


preprocess(Code, MFA) ->
  preprocess(Code, MFA, 0, 0, []).
  
preprocess({{Block,Entry},Rest},MFA,HotAddress,ColdAddress,Map) ->
%%  io:format("~w at ~w ~w\n",[MFA,HotAddress,ColdAddress]),
  process([{Block,Entry}|Rest],[],[],HotAddress,ColdAddress,
	      [{{MFA,entry},Block,
		case Block of
		  hot ->
		    HotAddress;
		 cold ->
		   ColdAddress
		end}|Map],
	  MFA).


process([{Block,Code}|Rest],Hot,Cold,HotSize,ColdSize,Map,MFA) ->

  case Block of
    hot ->
      {NewCode,NewSize,NewMap} = process_instr(Code,HotSize,Map,MFA,[],hot),
      process(Rest,NewCode++Hot,Cold,NewSize,ColdSize,NewMap,MFA);
    cold ->
      {NewCode,NewSize,NewMap} = process_instr(Code,ColdSize,Map,MFA,[],cold),
      process(Rest,Hot,NewCode++Cold,HotSize,NewSize,NewMap,MFA)
  end;
process([],Hot,Cold,HotSize,ColdSize,Map,_) ->
  {Hot,Cold,HotSize,ColdSize,Map}.

process_instr([I|Is],Size,Map,MFA,AccCode,Block) ->
  case hipe_sparc:type(I) of
    label ->
      process_instr(Is,Size,[{{MFA,hipe_sparc:label_name(I)},Block,Size}|Map],MFA,AccCode,Block);
    comment ->
      process_instr(Is,Size,Map,MFA,AccCode,Block);
    load_address ->
%%      io:format("~w\n",[I]),
      process_instr(Is,Size+8,Map,MFA,[I|AccCode],Block);
    load_atom ->
%%      io:format("~w\n",[I]),
      process_instr(Is,Size+8,Map,MFA,[I|AccCode],Block);
    load_word_index ->
      process_instr(Is,Size+8,Map,MFA,[I|AccCode],Block);
    Other ->
      process_instr(Is,Size+4,Map,MFA,[I|AccCode],Block)
  end;
process_instr([],Size,Map,_,Code,_) ->
  {lists:reverse(Code),Size,Map}.


pack_constants(MFA, ConstTab) ->
  pack_constants([{MFA,[],ConstTab}]).

pack_constants(Data) ->
  pack_constants(Data,0,0,[],[]).

pack_constants([{MFA,_,ConstTab}|Rest],AccSize,ConstNo,Acc,Refs) ->
  Labels = hipe_consttab:labels(ConstTab),
  %% RefToLabels = hipe_consttab:referred_labels(ConstTab),
  {Size, Map, NewConstNo, RefToLabels} = 
    pack_labels(Labels,MFA,ConstTab,AccSize, ConstNo,[], []),
  NewRefs =
    case  RefToLabels of 
      [] -> Refs;
      _ -> [{MFA,RefToLabels}| Refs]
    end,

  pack_constants(Rest, Size, NewConstNo, 
		 Map++Acc, NewRefs);
pack_constants([], Size, _, Acc, Refs) -> {Size, Acc, Refs}.

pack_labels([{Label,ref}|Labels],MFA,ConstTab,AccSize, ConstNo, Acc, Refs) ->
  pack_labels(Labels,MFA,ConstTab,AccSize, ConstNo, Acc, Refs);
pack_labels([Label|Labels],MFA,ConstTab,AccSize, ConstNo, Acc, Refs) ->
  Const = hipe_consttab:lookup(Label,ConstTab),
  Size = hipe_consttab:const_size(Const),
  Align = hipe_consttab:const_align(Const),
  Start = 
    case AccSize rem Align of
      0 -> AccSize;

      N -> AccSize + (Align -N)
    end,
  Need = Size, %% Do we need to consider alignment?
  %% io:format("Const ~w, Size ~w, Need ~w\n",[Const,Size,Need]),
  RawType = hipe_consttab:const_type(Const),
  Type = ?CONST_TYPE2EXT(RawType),
  RawData = hipe_consttab:const_data(Const),
  {Data, NewRefs} = 
    case RawType of
      term -> {RawData,[]};
      sorted_block -> {RawData, []};
      block -> 
	case RawData of
	  {ElementType, ElementData} ->
	    decompose_block(ElementType, ElementData, Start);
	  {ElementType, ElementData, SortOrder} ->
	      ElementSize = hipe_consttab:size_of(ElementType),
	    {TblData, TblRefs} = get_sorted_refs(ElementData, SortOrder),
	    {hipe_consttab:decompose({ElementType, TblData}), [{sorted,Start,TblRefs}]}
	end
	    
    end,
		
  pack_labels(Labels,MFA,ConstTab,Start+Need,ConstNo+1,
	      [{MFA,Label,ConstNo,
		Start,Need,
		Type, Data,
		?BOOL2EXT(hipe_consttab:const_exported(Const))}|
	       Acc],
	      NewRefs++Refs);
pack_labels([],_,_,AccSize,ConstNo,Acc,Refs) ->
  {AccSize,Acc, ConstNo, Refs}.

decompose_block(ElementType, Data, Addr) ->
  ElementSize = hipe_consttab:size_of(ElementType),
  {NewData, Refs} = get_refs(Data,Addr,ElementSize),
  {hipe_consttab:decompose({ElementType, NewData}), Refs}.

get_refs([{label,L}|Rest],Pos,4) ->
  {NewData, Refs} = get_refs(Rest, Pos+4, 4),
  {[0|NewData],[{L,Pos}|Refs]};
get_refs([D|Rest],Pos, Size) ->
  {NewData, Refs} = get_refs(Rest, Pos+Size, Size),
  {[D|NewData],Refs};
get_refs([],_,_) ->
  {[],[]}.

get_sorted_refs([{label,L}|Rest], [Ordering|Os]) ->
  {NewData, Refs} = get_sorted_refs(Rest, Os),
  {[0|NewData],[{L,Ordering}|Refs]};
get_sorted_refs([D|Rest], [Ordering|Os]) ->
  {NewData, Refs} = get_sorted_refs(Rest, Os),
  {[D|NewData],Refs};
get_sorted_refs([],[]) ->
  {[],[]}.


%% ------------------------------------------------------------------
%% Constant map
%%

%%find_const({MFA,Label},[{MFA,Label,ConstNo,_,_, _, _,_}|_])->
%%  ConstNo;
%%find_const(N,[_|R]) ->
%%  find_const(N,R).

find_const(Name, Tree) ->
  case gb_trees:lookup(Name,Tree) of
    {value,V} -> V;
    none -> ?EXIT({could_not_find_constant, Name, Tree})
  end.

init_const_map(List) ->
  init_const_map(List, gb_trees:empty()).
init_const_map([{MFA,Label,ConstNo,_,_, _, _,_} | List], Tree) ->
  init_const_map(List,gb_trees:insert({MFA,Label}, ConstNo ,Tree));
init_const_map([], Tree) -> Tree.


%% ------------------------------------------------------------------
%% Label map
%%

-ifdef(LISTMAP).
find(Name,[{Name,Seg,Addr}|_]) ->
  {Seg,Addr};
find(Name,[_|Rest]) ->
  find(Name,Rest);
find({Name,entry},[]) -> %% This function is not being compiled.
  {none, 0}.
init_export_map(List) ->
  List.
-else.

-ifdef(GBTREEMAP).
find(Name, Tree) ->
  case gb_trees:lookup(Name,Tree) of
    {value,V} -> V;
    none -> {none,0}  %% This function is not being compiled.
  end.

init_export_map(List) ->
  init_export_map(List, gb_trees:empty()).
init_export_map([{Name,Seg,Addr}|List], Tree) ->
  init_export_map(List,gb_trees:insert(Name,{Seg,Addr},Tree));
init_export_map([], Tree) -> Tree.
-else.


-ifdef(DICTMAP).

find(Name, Tree) ->
  case dict:find(Name,Tree) of
    {ok,V} -> V;
    error -> {none,0}  %% This Label is not being compiled.
  end.
init_export_map(List) ->
  init_export_map(List, dict:new()).
init_export_map3([{Name,Seg,Addr}|List], Tree) ->
  init_export_map3(List,dict:store(Name,{Seg,Addr},Tree));
init_export_map3([], Tree) -> Tree.

-else.
-ifdef(DOUBLEDICTMAP).
find({M,L}, Tree) ->
  case dict:find(M,Tree) of
    {ok,D} -> 
      case dict:find(L,D) of
	{ok,V} -> V;
	error -> {none,0}  
      end;
    error -> {none,0}  %% This Label is not being compiled.
  end.

init_export_map(List) ->
  init_export_map(List, dict:new()).

init_export_map([{{M,L},Seg,Addr}|List], Tree) ->
  init_export_map(List,init_dm(M,L,{Seg,Addr},Tree));
init_export_map([], Tree) -> Tree.
init_dm(M,L,Data,Tree) ->
  case dict:find(M,Tree) of
    {ok, T2} ->
      dict:store(M, dict:store(L,Data,T2), Tree);
    error ->
      dict:store(M, dict:store(L,Data,dict:new()), Tree)
  end.
-else.

-ifdef(ARRAYMAP).
local_labels(MFA, Map) ->
  case gb_trees:lookup(MFA, Map) of
    {value,T} -> T;
    none -> ?EXIT({mfa_not_in_map,MFA,Map})
  end.
  

find(L, Arr) ->
  {hot,hipe_bifs:array_sub(Arr,L)}.

init_export_map(List) ->
  MaxL = highestl(List,0),
  init_export_map(List, gb_trees:empty(), MaxL).
init_export_map([{{M,L},Seg,Addr}|List], Tree, MaxL) ->
  init_export_map(List,init_m(M,L,Addr,Tree, MaxL), MaxL);
init_export_map([], Tree, _) -> 
  Tree.

highestl([{{_,entry},_,_}|List], Max) ->
  highestl(List, Max);

highestl([{{_,L},_,_}|List], Max) ->
  if L > Max ->
      highestl(List, L);
     true ->
      highestl(List,Max)
  end;
highestl([], Max) -> Max+1.

init_m(M,entry,Data,Tree, MaxL) -> Tree;
init_m(M,L,Data,Tree, MaxL) ->
  case gb_trees:lookup(M,Tree) of
    {value, T2} ->
      hipe_bifs:array_update(T2,L,Data),
      Tree;
    none ->
      Arr = hipe_bifs:array(MaxL,0),
      hipe_bifs:array_update(Arr,L,Data),
      gb_trees:insert(M, Arr, Tree)
  end.

-else.

%% ----------------------------------------------------
%% DEFAULT
%% DOUBLTREESMAP

local_labels(MFA, Map) ->
  case gb_trees:lookup(MFA, Map) of
    {value,T} -> T;
    none -> ?EXIT({mfa_not_in_map,MFA,Map})
  end.
  

find(L, Tree) ->
  case gb_trees:lookup(L,Tree) of
    {value,V} -> V;
    none -> ?EXIT({label_not_in_map,L,Tree})
  end.

init_export_map(List) ->
  init_export_map(List, gb_trees:empty()).
init_export_map([{{M,L},Seg,Addr}|List], Tree) ->
  init_export_map(List,init_m(M,L,{Seg,Addr},Tree));
init_export_map([], Tree) -> Tree.


init_m(M,L,Data,Tree) ->
  case gb_trees:lookup(M,Tree) of
    {value, T2} ->
      gb_trees:update(M, gb_trees:insert(L,Data,T2), Tree);
    none ->
      gb_trees:insert(M, gb_trees:insert(L,Data,gb_trees:empty()), Tree)
  end.




-endif.
-endif.
-endif.
-endif.
-endif.




		      




