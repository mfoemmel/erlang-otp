%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Filename : 	hipe_sparc_linker.erl
%%  Module   :	hipe_sparc_linker
%%  History  :	* 2000-10-30 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: pegu2945 $
%%              $Date: 2002/11/05 12:22:43 $
%%              $Revision: 1.22 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc_linker).
-export([pack_constants/1, preprocess/5, assemble/3]).

-include("../../kernel/src/hipe_ext_format.hrl").
-include("../main/hipe.hrl").
-include("../rtl/hipe_literals.hrl").
%%=====================================================================


%% Move to sparc_assembler...
assemble(Code,Map,ConstMap) ->
  assemble1(Code,0,0,
	    init_export_map(Map),      %% Convert to more efficent
	    init_const_map(ConstMap),  %%  datastructures.
	    [],[],[],[]).

assemble1([{MFA,Hot,Cold}|Rest],HAddr,CAddr,Map,ConstMap,AccHCode,AccCCode,AccHRefs,AccCRefs) ->
  %% io:format("Assembling ~w\n",[MFA]),
  {HCode,NewHAddr,HRefs} = 
    assemble(Hot,MFA,HAddr,
	     local_labels(MFA,Map),
	     ConstMap,AccHRefs,[],hot),
  {CCode,NewCAddr,CRefs} = assemble(Cold,MFA,CAddr,
				    local_labels(MFA,Map),
				    ConstMap,AccCRefs,[],cold),
  assemble1(Rest,NewHAddr,NewCAddr,Map,ConstMap,AccHCode++HCode,AccCCode++CCode,HRefs,CRefs);
assemble1([],HAddr,CAddr,_Map,_ConstMap,AccHCode,AccCCode,AccHRefs,AccCRefs) ->
  {HAddr,CAddr,AccHCode,AccCCode,AccHRefs,AccCRefs}.

assemble([I|Is],MFA,Addr,Map,ConstMap,Refs,Code,Seg) ->
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
      store_fp -> {hipe_sparc_assemble:assemble_store_fp(I),Addr+4,Refs};
      load_fp -> {hipe_sparc_assemble:assemble_load_fp(I),Addr+4,Refs};
      fb -> {hipe_sparc_assemble:assemble_fb(I),Addr+4,Refs};
      fop -> {hipe_sparc_assemble:assemble_fop(I),Addr+4,Refs};
      fcmp -> {hipe_sparc_assemble:assemble_fcmp(I),Addr+4,Refs};
      fmov -> {hipe_sparc_assemble:assemble_fmov(I),Addr+4,Refs};
      conv_fp -> {hipe_sparc_assemble:assemble_conv_fp(I),Addr+4,Refs};
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
  Ref = 
    case hipe_sparc:load_address_type(Instr) of
      label ->
	{Zone,Offset} = find(Address, Map),
	[{?PATCH_TYPE2EXT(load_address),
	  Addr,
	  {label,Zone,Offset,MFA}}]; 
      function -> 
	case lists:member(local,hipe_sparc:info(Instr)) of
	  true -> %% local enter
	    [{?PATCH_TYPE2EXT(load_address),
	      Addr, {local_function,Address}}];
	  false -> %% remote enter
	    [{?PATCH_TYPE2EXT(load_address),
	      Addr, {remote_function,Address}}]
	end;
      constant ->
	ConstNo = find_const({MFA,Address},ConstMap),
	[{?PATCH_TYPE2EXT(load_address),
	  Addr, {const, ConstNo}}];
      closure ->
	[{?PATCH_TYPE2EXT(load_address),
		     Addr, {closure, Address}}];
      c_const ->
	[{?PATCH_TYPE2EXT(load_address),
		     Addr, {c_const, Address}}];
      Type ->     
	exit([{problem,{not_handled,{address,Type}}},{at,Instr}])
    end,
  
  Hi = hipe_sparc:mk_imm(0),
  Lo = hipe_sparc:mk_imm(0),
  I1 = hipe_sparc:sethi_create(Dest,Hi,[]),
  I2 = hipe_sparc:alu_create(Dest,Dest,'or',Lo,[]),

  {[
    hipe_sparc_assemble:assemble_instr(alu,I2),
    hipe_sparc_assemble:assemble_instr(sethi,I1)],
   Addr+8,
   Ref++Refs}.

resolve_goto(I,Addr,Refs,_MFA,Map,Seg) ->
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
  
resolve_b(I,Addr,Refs,_MFA,Map,Seg) ->
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

resolve_load_atom(Instr,Addr,Refs,_MFA,_Map)->
  Atom = hipe_sparc:load_atom_atom(Instr),
  Dest = hipe_sparc:load_atom_dest(Instr),
  Info = hipe_sparc:info(Instr),
  I1 = hipe_sparc:sethi_create(Dest,
			       hipe_sparc:mk_imm(0),Info),
  I2 = hipe_sparc:alu_create(Dest,Dest, 'or', 
			     hipe_sparc:mk_imm(0),Info),
  {[hipe_sparc_assemble:assemble_instr(alu,I2),
    hipe_sparc_assemble:assemble_instr(sethi,I1)],
   Addr+8,[{?PATCH_TYPE2EXT(load_atom),Addr, Atom}|Refs]}.

resolve_load_word_index(_Instr,_Addr,_Refs,_MFA,_Map,_ConstMap) ->
  ?EXIT({nyi,resolve_load_word_index}).
%%  Index =hipe_sparc:load_word_index_index(Instr),
%%  Block =hipe_sparc:load_word_index_block(Instr),
%% Dest = hipe_sparc:load_word_index_dest(Instr),
%%  Info = hipe_sparc:info(Instr),
%%
%%  ConstNo = find_const({MFA,Block},ConstMap),
%%  I1 = hipe_sparc:sethi_create(Dest,
%%			       hipe_sparc:mk_imm(0),Info),
%%  I2 = hipe_sparc:alu_create(Dest,Dest, 'or', 
%%			     hipe_sparc:mk_imm(0),Info),
%%  {[hipe_sparc_assemble:assemble_instr(alu,I2),
%%    hipe_sparc_assemble:assemble_instr(sethi,I1)],
%%   Addr+8,[{?PATCH_TYPE2EXT(load_word_index),Addr, {word_index, ConstNo, Index}}|Refs]}.
   
resolve_call_link(Instr,Addr,OldRefs,_MFA,Map,_Seg)->
  Target = hipe_sparc:call_link_target(Instr),
  ExnLab = hipe_sparc:call_link_fail(Instr),


  %% Get the stack descriptor information
  SD = hipe_sparc:call_link_stack_desc(Instr),
  FSize = hipe_sparc_stack_descriptors:get_size(SD),
  Live =  list_to_tuple(hipe_sparc_stack_descriptors:get_live(SD)),
  Arity = case hipe_sparc_stack_descriptors:get_arity(SD) of
	    N when N > ?SPARC_ARGS_IN_REGS -> N - ?SPARC_ARGS_IN_REGS;
	    _ -> 0
	  end,
  ExnRA =
    case ExnLab of
      [] -> [];	% don't cons up a new one
      _ -> find(ExnLab,Map)
    end,

  %% The stack descriptor needs to be inserted into the system at load-time.
  Refs = 
    [{?PATCH_TYPE2EXT(sdesc),
      Addr,
      ?STACK_DESC(ExnRA, FSize, Arity, Live)} | OldRefs],

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
      NewRefs =
	case lists:member(local,hipe_sparc:info(Instr)) of
	  true -> %%  local call
	    [{?PATCH_TYPE2EXT(call_local),
	      Addr, Target} | Refs];
	  _ -> %% Remote call
	    [{?PATCH_TYPE2EXT(call_remote),
	      Addr, Target} | Refs]
	end,
      NewI = hipe_sparc:call_link_target_update(Instr,0),
      Code = hipe_sparc_assemble:assemble_instr(call_link,NewI),
      {Code,Addr+4,NewRefs}
  end.





  
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
      process_instr(Is,Size+8,Map,MFA,[I|AccCode],Block);
    load_atom ->
      process_instr(Is,Size+8,Map,MFA,[I|AccCode],Block);
    load_word_index ->
      process_instr(Is,Size+8,Map,MFA,[I|AccCode],Block);
    _Other ->
      process_instr(Is,Size+4,Map,MFA,[I|AccCode],Block)
  end;
process_instr([],Size,Map,_,Code,_) ->
  {lists:reverse(Code),Size,Map}.




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

pack_labels([{_Label,ref}|Labels],MFA,ConstTab,AccSize, ConstNo, Acc, Refs) ->
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
get_sorted_refs([D|Rest], [_Ordering|Os]) ->
  {NewData, Refs} = get_sorted_refs(Rest, Os),
  {[D|NewData],Refs};
get_sorted_refs([],[]) ->
  {[],[]}.


%% ------------------------------------------------------------------
%% Constant map
%%

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
% init_export_map3([{Name,Seg,Addr}|List], Tree) ->
%   init_export_map3(List,dict:store(Name,{Seg,Addr},Tree));
% init_export_map3([], Tree) -> Tree.

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




		      




