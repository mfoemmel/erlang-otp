%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%%  Filename : 	hipe_unified_loader.erl
%%  Module   :	hipe_unified_loader
%%  Purpose  :  To load code into memory and link it to the system.
%%  Notes    :  See hipe_ext_format.hrl for description of the external 
%%               format.
%%              See also the todolist below.
%%
%% ====================================================================
%% Exported functions (short description):
%%   load(Mod,Bin) - Loads the module Mod from the binary Bin into
%%		     memory and links it with the system
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO:
%%
%%   Problems with the order in which things are done.
%%   export_funs should atomically patch referenses to make fe and
%%   make beam stubs. !!
%%
%%   Each function should have two proper databases.
%%   Describe the patch algorithm:
%%     For each function MFA that is (re)compiled to Address:
%%     1.  For the old MFA 
%%         a. RefsTo = MFA->refers_to
%%         b. for each {F,Adr} in RefsTo: remove Adr from F->is_reffered
%%         c. RefsFrom = MFA->is_reffered
%%         d. For each {Adr,Type} in RefsFrom: 
%%                update instr at Adr to reffere to Address instead.
%%     2.  For the new MFA
%%         a. MFA->is_reffered=RefsFrom 
%%     3.  For each function F referenced in the code at Offset:
%%                 add {Address+Offset,Type} to F->is_reffered
%%                 add {F,Address+Offset} to MFA->refferec_to
%%     4.  Make Address the entrypoint for MFA
%%
%%   Test a lot. 
%%   Add exporting of exported constants.
%%   Add freeing of old code. 
%%   Inline hipe_sparc_ext_format somehow.
%%   Test a lot.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(hipe_unified_loader).
-export([load_module/3, load/2,
	 patch_to_emu/1,

	 is_loaded/1,

	 write_words/2,
	 write_bytes/2]).

%%-define(DEBUG,true).
-define(DO_ASSERT,true).
-include("../../hipe/main/hipe.hrl").
-include("hipe_ext_format.hrl").

version_check(Version, Mod) ->
  Ver = ?version(),
  case Version < Ver of
    true -> 
      ?msg("WARNING: Module (~w) has version ~w\n",
	  [Mod, Version]);
    _ -> true
  end.

system_check(CRC, Mod) ->
  case hipe_bifs:check_crc(CRC) of
    false ->
      ?error_msg("Module (~w) was compiled for an incompatible "
		 "runtime system\n",
		 [Mod]),
      ?EXIT({bad_crc,Mod});
    _ -> true
  end.

%% ====================================================================
load_module(Mod, Bin, Beam) ->
  ?debug_msg("************ Loading Module ~w ************\n",[Mod]),

  %% Loading a whole module, let the beam loader patch closures.
  put(hipe_patch_closures,false),



  [{Version, CheckSum},
   ConstSize, ConstMap, LabelMap,
   ExportMap,
   HotSize,   HotCode,  HRefs,
   ColdSize,  ColdCode, CRefs]
    = binary_to_term(Bin),

  %% Check that we are loading up-to-date code.
  version_check(Version, Mod),
  system_check(CheckSum, Mod), %% Throws exception...

  {ConstAddr,ConstMap2} = alloc_constants(ConstSize,ConstMap),

  %% Write the code to memory.
  HotAddress =  hipe_bifs:alloc_code(HotSize+ColdSize),
  ColdAddress =  HotAddress + HotSize,
  ?ASSERT(init_assert_patch(HotAddress,HotSize+ColdSize)),

    case erlang:system_info(hipe_architecture) of
      ultrasparc -> %% On the sparc -- write one 32 bit word at the time
	write(HotCode,HotAddress),
	write(ColdCode,ColdAddress);
     x86 -> %% On the x86 write one byte at the time.
	write(HotCode,HotAddress,0,HotSize),	
	write(ColdCode,ColdAddress,0,ColdSize);
      _ -> 
	do_nada
    end,
 
  
  %% Patch references to code labels in data seg.
  patch_consts(LabelMap, ConstAddr, HotAddress),
  

  %% Find out which functions are being loaded (And where).
  %% Note: Addresses are sorted descending.
  {MFAs, Addresses} = exports(ExportMap,HotAddress),

  %% Remove references to old versions of the module.
  ReferencesToPatch = get_and_remove_refs(MFAs),

  %% Patch all dynamic referenses in the code.
  %%  Function calls, Atoms, Constants, System calls
  patch(HRefs,HotAddress,{ConstMap2,HotAddress,ColdAddress}, Addresses),
  patch(CRefs,ColdAddress,{ConstMap2,HotAddress,ColdAddress}, Addresses),

  %% Find all closures in the code.
  HotClosurePatches = find_closure_patches(HRefs),
  ColdClosurePatches = find_closure_patches(CRefs),
  AddressesOfClosuresToPatch =
    calculate_addresses(HotClosurePatches,HotAddress, Addresses) ++
    calculate_addresses(ColdClosurePatches,ColdAddress, Addresses),

  %% Tell the system where the loaded funs are. 
  %%  (patches the BEAM code to redirect to native.)
  export_funs(Mod, Beam, Addresses,AddressesOfClosuresToPatch),

  %% Patch reffering functions to call the new function
  redirect_old_refs(ReferencesToPatch, Addresses),

  ?debug_msg("****************Loader Finished****************\n",[]),
  {module,Mod}.  %% for compatibility with code:load_file/1

%% --------------------------------------------------------------------
%% 
load(Mod,Bin) ->
  ?debug_msg("********* Loading funs in module ~w *********\n",[Mod]),

  %% Loading just some functions in a module, patch closures separately.
  put(hipe_patch_closures,true),

  %% Unpack the binary.
  [{Version, CheckSum},
   ConstSize, ConstMap, LabelMap,
   ExportMap,
   HotSize,   HotCode,  HRefs,
   ColdSize,  ColdCode, CRefs]
    = binary_to_term(Bin),

  version_check(Version, Mod),
  system_check(CheckSum, Mod), %% Throws exception...

  {ConstAddr,ConstMap2} = alloc_constants(ConstSize,ConstMap),
   
  %% Write the code to memory.
  HotAddress =  hipe_bifs:alloc_code(HotSize+ColdSize),
  ColdAddress =  HotAddress + HotSize,
  ?ASSERT(init_assert_patch(HotAddress,HotSize+ColdSize)),

    case erlang:system_info(hipe_architecture) of
      ultrasparc -> 
	write(HotCode,HotAddress),
	write(ColdCode,ColdAddress);
     x86 -> 
	write(HotCode,HotAddress,0,HotSize),	
	write(ColdCode,ColdAddress,0,ColdSize);
      _ -> 
	do_nada
    end,
  
  %% Patch references to code labels in data seg.
  %%
  patch_consts(LabelMap, ConstAddr, HotAddress),
  

  %% Find out which functions are being loaded (And where).
  %% Note: Addresses are sorte descending.
  {MFAs, Addresses} = exports(ExportMap,HotAddress),
  %% 
  ReferencesToPatch = get_and_remove_refs(MFAs),

  %% Patch all dynamic referenses in the code.
  %%  Function calls, Atoms, Constants, System calls
  patch(HRefs,HotAddress,{ConstMap2,HotAddress,ColdAddress}, Addresses),
  patch(CRefs,ColdAddress,{ConstMap2,HotAddress,ColdAddress}, Addresses),

  %% Tell the system where the loaded funs are. 
  %%  (patches the BEAM code to redirect to native.)
  export_funs(Addresses),

  %% Patch reffering functions to call the new function
  redirect_old_refs(ReferencesToPatch, Addresses),

  ?debug_msg("****************Loader Finished****************\n",[]),
  {module,Mod}.  %% for compatibility with code:load_file/1

%% ____________________________________________________________________
%% 




exports(ExportMap, BaseAddress) ->
  exports(ExportMap, BaseAddress, [], []).

exports([Offset, M,F,A, Closure |Rest], BaseAddress, MFAs, Addresses) ->
  MFA = {M,F,A},
  Address = BaseAddress + Offset,
  exports(Rest, BaseAddress, [MFA | MFAs], [{Address, MFA, Closure} |
					  Addresses]);
exports([], _, MFAs, Addresses) ->
  {MFAs, Addresses}.

mod({M,_F,_A}) -> M.

%% ____________________________________________________________________
%% 
calculate_addresses(PatchOffsets,Base,Addresses) ->
  TransFun =
    case erlang:system_info(hipe_architecture) of
      ultrasparc -> 
	fun(Os) ->
	    hipe_sparc_loader:offsets_to_addresses(Os, Base)
	end;
      x86 ->
      	fun(Os) ->
	    hipe_x86_loader:offsets_to_addresses(Os, Base)
	end;
      _ -> fun (_) -> [] end
    end,
  [{Data,
    TransFun(Offsets),
    get_native_address(DestMFA,Addresses)} || 
    {{DestMFA,_,_}=Data,Offsets} <- PatchOffsets].



%% ____________________________________________________________________
%% 


find_closure_patches([{Type, Refs} | Rest]) ->
  case ?EXT2PATCH_TYPE(Type) of 
    load_address -> 
      find_closure_refs(Refs,Rest);
    _ ->
      find_closure_patches(Rest)
  end;
find_closure_patches([]) -> [].
find_closure_refs([{Dest,Offsets}|Rest],Refs) ->
  case Dest of
    {closure,Data} ->
      [{Data,Offsets}|find_closure_refs(Rest,Refs)];
    _ ->
      find_closure_refs(Rest,Refs)
  end;
find_closure_refs([],Refs) ->
  find_closure_patches(Refs).



%%
%%
%% ____________________________________________________________________
%% Patching 
%%  @spec patch(refs(), BaseAddress:integer(), ConstAndZone:term(), Addresses:term())->
%%   @type refs()=[{RefType:integer(), Reflist:reflist()} | refs()]
%%
%%   @type reflist()=   [{Data:term(), Offsets:offests()}|reflist()]
%%   @type offsets()=   [Offset:integer() | offsets()]
%% @doc
%%  The patchlist is a list of lists of patches of a type.
%%  For each type the list of references is sorted so that several
%%  references to the same type of data comes after eachother
%%  we use this to look up the address of a reffered function only once.
%%
patch([{Type, SortedRefs} | Rest], BaseAddress, ConstAndZone, Addresses)->
 ?debug_msg("Patching ~w at [~w+offset] with ~w\n",
	     [Type,BaseAddress,SortedRefs]),
  case ?EXT2PATCH_TYPE(Type) of 
    call_local -> 
      patch_all_call_local(SortedRefs, BaseAddress, Addresses);
    call_remote ->
      patch_all_call_remote(0, SortedRefs, BaseAddress, Addresses, []);
    Other -> 
      patch_all(Other, SortedRefs, BaseAddress, ConstAndZone, Addresses)
  end,
  patch(Rest, BaseAddress, ConstAndZone, Addresses);
patch([],_,_, _) -> true.


%% ____________________________________________________________________
%% 
patch_all_call_local([{DestMFA, Offsets}| SortedRefs], 
		     BaseAddress, Addresses)->
  PatchFun = 
    case bif_address(DestMFA) of
      false -> 
	%% Find the address of the caller.
	DestAddress = mfa_to_address(DestMFA, Addresses),
	fun(Offset) ->
	    Address = BaseAddress + Offset,
	    CallerMFA = address_to_mfa(Address, Addresses),
	    add_ref(CallerMFA, DestMFA, Address,call),
	    ?ASSERT(assert_local_patch(Address)),
	    patch_instr(Address,DestAddress,call)
	end;
      BifAddress when integer(BifAddress) ->
	%% If it is a bif we will not need to backpatch the call.
	fun(Offset) ->
	    ?ASSERT(assert_local_patch(BaseAddress+Offset)),
	    patch_instr(BaseAddress+Offset, BifAddress, call)
	end
  end,
  [PatchFun(O) || O <- Offsets],
  patch_all_call_local( SortedRefs,BaseAddress, Addresses);
patch_all_call_local([],_,_) -> 
  true.



%% ____________________________________________________________________
%% 
patch_all_call_remote(Mod, [{{Mod,F,A} = DestMFA,Offsets} | SortedRefs], 
		      BaseAddress, Addresses, NativeaddressesInMod) ->
  case bif_address(DestMFA) of
    false -> 
      %% Check if this remote call is to a function that is compiled now.
      DA = case mfa_to_address(DestMFA, Addresses) of
	     false -> %% Not compiled now, look in list of native addresses.

	       find_fa(F,A,NativeaddressesInMod,DestMFA);
	     DstAdr -> DstAdr %% We have the address.
	   end,
      patch_all_call_remote_offsets(Offsets,DestMFA, 
				    DA, BaseAddress, Addresses);
    BifAddress ->
      PatchFun = 
	fun(Offset) ->
	    ?ASSERT(assert_local_patch(BaseAddress+Offset)),
	    patch_instr(BaseAddress+Offset, BifAddress, call)
	end,
      [PatchFun(O) || O <- Offsets]
  end,
  patch_all_call_remote(Mod, SortedRefs,
			BaseAddress, Addresses, 
			NativeaddressesInMod);
patch_all_call_remote(LastMod, [{DestMFA,Offsets} | SortedRefs], 
		      BaseAddress, Addresses, _) ->
  case bif_address(DestMFA) of
    false -> 
      case mfa_to_address(DestMFA, Addresses) of
	false -> %% Not compiled now, look in list of native addresses
	  {Mod,F,A} = DestMFA,
	  NativeaddressesInMod = 
	    case is_loaded(Mod) of 
	      true -> Mod:module_info(native_addresses);
	      false -> []
	    end,
	  DA =  find_fa(F,A,NativeaddressesInMod,DestMFA),
	  patch_all_call_remote_offsets(Offsets,DestMFA, DA, BaseAddress, 
					Addresses),
	  patch_all_call_remote(Mod, SortedRefs,
				BaseAddress, Addresses, 
				NativeaddressesInMod);
	DA -> %% Compiled now, we have the address.
	  patch_all_call_remote_offsets(Offsets,DestMFA, DA, BaseAddress, 
					Addresses),
	  patch_all_call_remote(LastMod, SortedRefs,
				BaseAddress, Addresses, [])
      end;
    BifAddress ->
      PatchFun = 
	fun(Offset) ->
	    ?ASSERT(assert_local_patch(BaseAddress+Offset)),
	    patch_instr(BaseAddress+Offset, BifAddress, call)
	end,
      [PatchFun(O) || O <- Offsets],
      patch_all_call_remote(LastMod, SortedRefs,
			    BaseAddress, Addresses, [])
      
  end;
patch_all_call_remote(_,[],_,_,_) -> true.


patch_all_call_remote_offsets([Offset|Offsets], DestMFA, DestAddress, 
			     BaseAddress, Addresses) ->
  Address = BaseAddress + Offset,
  CallerMFA = address_to_mfa(Address, Addresses),
  add_ref(CallerMFA, DestMFA, Address, call),
  ?ASSERT(assert_local_patch(Address)),
  patch_instr(Address,DestAddress,call),
  patch_all_call_remote_offsets(Offsets, DestMFA, DestAddress, 
			     BaseAddress, Addresses);
patch_all_call_remote_offsets([],_,_,_,_) -> true.

%% ____________________________________________________________________
%% 

patch_all(Type, [{Dest,Offsets}|Rest], BaseAddress, ConstAndZone, Addresses)->
  patch_all_offsets(Type, Dest, Offsets, BaseAddress, ConstAndZone, Addresses),
  patch_all(Type, Rest, BaseAddress, ConstAndZone, Addresses);
patch_all(_, [], _, _, _) -> true.

patch_all_offsets(Type, Data, [Offset|Offsets], BaseAddress,
		  ConstAndZone, Addresses) ->
  ?debug_msg("Patching ~w at [~w+~w] with ~w\n",
	     [Type,BaseAddress,Offset, Data]),
  Address = BaseAddress + Offset,
  patch_offset(Type, Data, Address, ConstAndZone, Addresses),
  ?debug_msg("Patching done\n",[]),
  patch_all_offsets(Type, Data, Offsets, BaseAddress, ConstAndZone, Addresses);
patch_all_offsets(_,_,[],_,_,_) -> true.


patch_offset(mfa, MFA, Address, _ConstAndZone, Addresses) ->
      patch_any_call(MFA, Address, Addresses);
patch_offset(load_address, Data, Address, ConstAndZone, Addresses) ->
      patch_load_address(Data, Address, ConstAndZone, Addresses);
patch_offset(load_atom, Atom, Address, _, _) ->
      patch_atom(Address, Atom);
patch_offset(call_local, MFA, Address, _ConstAndZone, Addresses) ->
      %% This is a call to a local fun that is not compiled now.
      patch_any_call(MFA, Address, Addresses);
patch_offset(sdesc, Data, Address, ConstAndZone, _Addresses) ->
      patch_sdesc(Data, Address, ConstAndZone);
patch_offset(Type,Data, Address, _ConstAndZone, _Addresses) -> 
      ?error_msg("Unknown ref ~w ~w ~w\n",[Type, Address, Data]),
      exit({unknown_reference, Type, Address, Data}).





patch_load_address({local_function,DestMFA},Address,_, Addresses)->
  %% TODO: Bifs should not generate refs...
  DestAddress = get_bif_or_native_address(DestMFA, Addresses),
  CallerMFA = address_to_mfa(Address, Addresses),
  add_ref(CallerMFA, DestMFA,Address,sethiref),
  add_ref(CallerMFA, DestMFA,Address+4,orref),
  ?ASSERT(assert_local_patch(Address)),
  patch_instr(Address, DestAddress, constant);
patch_load_address({remote_function,DestMFA},Address,_, Addresses)->
  %% TODO: Bifs should not generate refs...
  %% TODO: Handle references to local and remote functions differently.
  DestAddress = get_bif_or_native_address(DestMFA, Addresses),
  CallerMFA = address_to_mfa(Address, Addresses),
  add_ref(CallerMFA, DestMFA,Address,sethiref),
  add_ref(CallerMFA, DestMFA,Address+4,orref),
  ?ASSERT(assert_local_patch(Address)),
  patch_instr(Address, DestAddress, constant);

patch_load_address({offset,Offset}, Address,
		   {_ConstMap,HotAddress,_ColdAddress}, _Addresses) ->
  patch_instr(Address,Offset+HotAddress,offset);
patch_load_address({constant,Name}, Address, Info, _Addresses) ->
  patch_constant(Address,Name,Info);
patch_load_address({const,Name},Address,Info, _Addresses)->
  patch_constant(Address,Name,Info);
patch_load_address({label,Offset}, Address,
		   {_ConstMap,HotAddress,_ColdAddress}, _Addresses) ->
  patch_label(Address,HotAddress,Offset);
patch_load_address({label,hot,Offset,_MFA},Address,
		   {_ConstMap,HotAddress,_ColdAddress}, _Addresses) ->
  patch_label(Address,Offset,HotAddress);
patch_load_address({label,cold,Offset,_MFA},Address,
		   {_ConstMap,_HotAddress,ColdAddress}, _Addresses) ->
  patch_label(Address,Offset,ColdAddress);
patch_load_address({closure,{DestMFA,Uniq, Index}}, Address, _, Addresses) ->
  patch_closure(DestMFA,Uniq,Index,Address,Addresses);
patch_load_address({c_const,CConst},Address, _Info, _Addresses) ->
  patch_instr(Address, bif_address(CConst), c_const).

patch_label(Address,Offset,Base) ->
  patch_instr(Address,Offset+Base,label).

patch_closure(DestMFA,Uniq,Index,Address,Addresses)->
  case get(hipe_patch_closures) of
    false ->
      true; %% This is taken care of when registering the module.
    true -> %% We are not loading a module patch these closures
      DestAddress = get_native_address(DestMFA, Addresses),
      BEAMAddress = get_emu_address(DestMFA),
      FE = hipe_bifs:make_fe(DestAddress, mod(DestMFA), 
			     {Uniq, Index,BEAMAddress}),
      ?debug_msg("Patch FE(~w) to 0x~s->0x~s (emu:0x~s)\n",
		 [DestMFA, hipe_converters:int_to_hex(FE),
		  hipe_converters:int_to_hex(DestAddress),
		  hipe_converters:int_to_hex(BEAMAddress)]),
      ?ASSERT(assert_local_patch(Address)),
      patch_instr(Address, FE, closure) 
  end.

patch_constant(Address, Name, Info) ->
  ConstAddress = find_const(Name,element(1,Info)),
  patch_instr(Address, ConstAddress, constant). 

patch_consts(Labels, DataAddress, CodeAddress) ->
  PatchLabelOrLabels =
    fun 
      ({Pos,Offset}) ->
	?ASSERT(assert_local_patch(CodeAddress+Offset)),
	hipe_bifs:write_u32(DataAddress+Pos,
			    CodeAddress+Offset);
      ({sorted,Base,UnOrderdList}) ->
	sort_and_write(UnOrderdList, Base, 
		       DataAddress, CodeAddress)
    end,
  lists:map(PatchLabelOrLabels, Labels).

patch_sdesc(?STACK_DESC(SymExnRA, FSize, Arity, Live), 
	    Address, {_ConstMap,HotAddress,ColdAddress}) ->
  ExnRA =
    case SymExnRA of
      [] -> 0; %% No catch
      {Zone, Offset} ->
	case Zone of
	  hot ->
	    Offset+HotAddress;
	  cold -> 
	    Offset+ColdAddress
	end;
      LabelOffset -> %% x86 doesn't use zones.    
	HotAddress + LabelOffset
    end,
  ?ASSERT(assert_local_patch(Address)),
  hipe_bifs:enter_sdesc({Address, ExnRA, FSize, Arity, Live}).

patch_atom(Address,Atom) ->
  ?ASSERT(assert_local_patch(Address)),
  patch_instr(Address, hipe_bifs:atom_to_word(Atom), atom).

%% Patch a call-site with the address of the callee. 
%% The target of a call can be a BIF or an Erlang function.
%% We have to check for both possibilities, BIF first
patch_any_call(DestMFA, Address, Addresses) ->
  case bif_address(DestMFA) of
    false ->
      %% Find the address of the caller.
      CallerMFA = address_to_mfa(Address, Addresses),
      %% Register the call-site for backpatching at code change.
      add_ref(CallerMFA,DestMFA,Address,call),
      ?ASSERT(assert_local_patch(Address)),
      patch_erl_call(DestMFA, Address, Addresses);
    BifAddress when integer(BifAddress) ->
      %% If it is a bif we will not need to backpatch the call.
      ?ASSERT(assert_local_patch(Address)),
      patch_instr(Address, BifAddress, call)
  end.
%% Not a bif, but it might be a call to the module beeing compiled now.
%% (The "is-local-test" is a bit ugly relaying of the representation of Addresses.)
patch_erl_call({M,_,_}=DestMFA,Address,
	       [{_,{M,_,_},_}|_]=Addresses) ->
  %% Find the address of the callee.
  CalleeAddress = get_native_address(DestMFA, Addresses),
  ?ASSERT(assert_local_patch(Address)),
  patch_instr(Address, CalleeAddress, call);
%% Not a local call -- find the native code of the callee.
patch_erl_call(DestMFA,Address,_Addresses) ->
  CalleeAddress = get_native_address(DestMFA),
  ?ASSERT(assert_local_patch(Address)),
  patch_instr(Address, CalleeAddress, call).



%%
%%
patch_instr(Address, ToAddress, Type) ->
  case erlang:system_info(hipe_architecture) of
    ultrasparc -> 
      hipe_sparc_loader:patch_instr(Address, ToAddress,
						Type);
    x86 -> 
      hipe_x86_loader:patch_instr(Address, ToAddress,
					     Type);
    _ -> %% Unsuported arch -- do nothing.
      ok
  end.


%% ____________________________________________________________________
%% 


bif_address({M,F,A}) ->
  hipe_bifs:bif_address(M,F,A);
bif_address(Name) ->
  hipe_bifs:primop_address(Name).

alloc_constants(ByteSize,Constants) ->
  WordNeed = (ByteSize div 4) + 2, %% +2 to ensure 8 byte alignment...
  Addr = hipe_bifs:alloc_constant(WordNeed),
  AlignedAddress =
    case Addr rem 8 of
      0 -> Addr;
      N -> Addr + (8-N)
    end,
  {AlignedAddress,alloc_constant(Constants,AlignedAddress,[],[], Addr,
				Addr+ WordNeed*4)}.

alloc_constant([ConstNo,Offset,ByteNeed,Type,Exported, Data|Rest],
	       Addr, Acc, UsedAdr, Min,Max) ->
  %?msg("Const ~w\n",[[ConstNo,Offset, ByteNeed,Type,Exported, Data]]),
  Address = Addr+Offset,
  ?ASSERT((Address =< Max) and (Address >= Min)),
  ?ASSERT((Address+ByteNeed =< Max) and (Address+ByteNeed >= Min)),
  ?ASSERT(not lists:member(Address, UsedAdr)),
  Res = 
    case ?EXT2CONST_TYPE(Type) of
      term ->
	    hipe_bifs:copy_term(Data,Address,ByteNeed div 4);
      sorted_block ->
	L = lists:sort(lists:map(fun(Term) ->
				       hipe_bifs:term_to_word(Term)
				   end, Data)),
	write_words(L,Address),
	Address;
      block ->
	case Data of
	  {Lbls, []} ->
	    write_bytes(Lbls,Address),
	    Address;
	  {Lbls, SortOrder} ->
	    SortedLbls =  lists:map(fun({_,Lbl}) -> Lbl end,
			      lists:sort(group(Lbls, SortOrder))),
	    write_words(SortedLbls,Address),
	    Address;
	%% write_block(Data, Address, Labels),
	  Lbls ->
	    write_bytes(Lbls,Address),
	    Address
	end
    end,
  alloc_constant(Rest,Addr, 
		 [{ConstNo, 
		  {Res, ?EXT2BOOL(Exported)}}|
		  Acc],
		lists:seq(Address,Address+ByteNeed-1)++UsedAdr,Min,Max);
alloc_constant([],_, Acc,_,_,_) -> Acc.

find_const(ConstNo, [{ConstNo,{Addr,_}}|_ConstMap]) ->
  Addr;
find_const(ConstNo, [_|ConstMap]) ->
  find_const(ConstNo, ConstMap);
find_const(ConstNo, []) ->
  ?error_msg("Constant not found ~w\n",[ConstNo]),
  exit({constant_not_found,ConstNo}).



bytes_to_32(B4,B3,B2,B1) ->
  (B4 bsl 24) bor (B3 bsl 16) bor
    (B2 bsl 8) bor B1.

sort_and_write(UnOrderdList, Base, DataAddress, CodeAddress) ->
  WriteAndInc = 
    fun ({_, Offset}, Pos) ->
	?ASSERT(assert_local_patch(CodeAddress+Offset)),
	hipe_bifs:write_u32(DataAddress+Pos,
			    CodeAddress+Offset),
	Pos + 4
    end,
  lists:foldl(WriteAndInc, Base, sort_on_representation(UnOrderdList)).

sort_on_representation(List) ->
  TupleToTermRep = 
    fun({Term, Offset}) ->
	{hipe_bifs:term_to_word(Term),
	 Offset}
    end, 
  lists:sort(lists:map(TupleToTermRep, List)).
  
add_ref(CallerMFA, CalleeMFA, Address, RefType) ->
  %% io:format("Adding ref ~w\n",[{CallerMFA, CalleeMFA, Address, RefType}]),
  Refs =
    case hipe_bifs:get_funinfo({CallerMFA, refers_to}) of
      [{_,OldRefs}] ->
	OldRefs;
      [] ->
	[]
    end,
  hipe_bifs:set_funinfo({{CallerMFA, refers_to}, [CalleeMFA| Refs]}),

  CalleeRefs =
    case hipe_bifs:get_funinfo({CalleeMFA, referred_from}) of
      [{_,OldRs}] ->
	OldRs;
      [] ->
	[]
    end,
  hipe_bifs:set_funinfo({{CalleeMFA, referred_from}, 
		    [{CallerMFA, Address, RefType}| 
		     CalleeRefs]}).


%%
%% Code for handling reload of beam code.
%%

%% @doc Redirects any calls from native code to a previously loaded
%%           native code version of this module to the new emulated code.
patch_to_emu(Mod) ->
  case is_loaded(Mod) of
    true ->
      %% Get exported functions
      MFAs = [{Mod,Fun, Arity} || {Fun, Arity} <-
				    Mod:module_info(exports)],
      %% Find all callsites that calls these MFAs.
      %%  as a sideeffect: create native subs for any MFAs that are referred.
      ReferencesToPatch = emu_get_and_remove_refs(MFAs),
      redirect_to_emu(ReferencesToPatch);
    false ->
      %% The first time we load the module no redirection needs to be done.
      ok
  end.



%% @spec is_loaded(Module::atom()) -> bool()
%% @doc Checks whether a module is loaded or not.
is_loaded(M) ->
  case catch hipe_bifs:fun_to_address({M,module_info,0}) of
    I when integer(I) ->
      true;
    _ -> false
  end.

get_emu_address(MFA) ->
  case catch hipe_bifs:fun_to_address(MFA) of
    {'EXIT',_} -> %% No BEAM function
      {M,F,A} = MFA,
      Address = hipe_bifs:emu_stub(M, F, A),
      Address;
    Addr ->  
      Addr
  end.


make_stub({_,_,A} = MFA) ->
  EmuAddress = get_emu_address(MFA),
  StubAddress = make_native_stub(EmuAddress, A),
  hipe_bifs:set_funinfo({{MFA,native_address}, StubAddress}).

make_native_stub(EmuAddress, A) ->
  case erlang:system_info(hipe_architecture) of
    ultrasparc -> hipe_sparc_loader:make_native_stub(EmuAddress, A);
    x86 -> hipe_x86_loader:make_native_stub(EmuAddress, A);
    _ -> %% Unsuported arch -- do nothing.
      ok
  end.


redirect_to_emu([{MFA,Refs}|Rest]) ->
  redirect_to_emu(Refs,MFA),
  redirect_to_emu(Rest);
redirect_to_emu([]) ->
  true.

redirect_to_emu([{_MFA, Address, Type}| Refs], ToMFA) ->
  StubAddress = get_native_address(ToMFA),
  ?IF_DEBUG({{M,F,A},{M2,F2,A2}} = {_MFA, ToMFA}, no_debug),
  ?debug_msg("(RE)LINKING ~w:~w/~w (@ 0x~s) to EMU ~w:~w/~w (@ 0x~s)\n",
	     [M,F,A, hipe_converters:int_to_hex(Address), 
	     M2,F2,A2, hipe_converters:int_to_hex(StubAddress)]),
  patch_instr(Address, StubAddress, Type),
  redirect_to_emu(Refs, ToMFA);
redirect_to_emu([],_) ->
  true.

emu_get_and_remove_refs(MFAs) ->
  %% Find all call sites that calls these MFAs
  %% If a call site is found create a new native stub for that MFA.
  ReferredFrom = emu_get_refs_from(MFAs, []),
  remove_refs_to(MFAs),
  ReferredFrom.

get_and_remove_refs(MFAs) ->
  %% Find all call sites that calls these MFAs
  %% If a call site is found create a new native stub for that MFA.
  ReferredFrom = get_refs_from(MFAs, []),
  remove_refs_to(MFAs),
  ReferredFrom.

remove_refs_to([MFA|MFAs]) ->
  %% This (new version of MFA) do not refer 
  %% to any other functions (yet).
  %% Remove all referred_from from all functions that 
  %% MFA refers_to.
  case hipe_bifs:get_funinfo({MFA, refers_to}) of
    [] -> true;
    [{{MFA, refers_to}, RefsTo}] ->
      remove_refs(RefsTo, MFA)
  end,
  %% Indicate that MFA refers_to no other function.
  hipe_bifs:set_funinfo({{MFA, refers_to}, []}),
  remove_refs_to(MFAs);
remove_refs_to([]) ->
  true.

emu_get_refs_from([MFA|MFAs], Acc) ->
  %% Get all MFAs that refers_to MFA, these need to be patched.
  case hipe_bifs:get_funinfo({MFA, referred_from}) of
    [{{_,_}, Refs}] ->
      %% There are references to this MFA, a new stub is needed.
      make_stub(MFA),
      %% Keep all refs and handle rest.
      emu_get_refs_from(MFAs,[{MFA,Refs}|Acc]);
    [] -> 
      emu_get_refs_from(MFAs, Acc)
  end;
emu_get_refs_from([], Acc) ->
  Acc.

get_refs_from([MFA|MFAs], Acc) ->
  %% Get all MFAs that refers_to MFA, these need to be patched.
  case hipe_bifs:get_funinfo({MFA, referred_from}) of
    [{{_,_}, Refs}] ->
      %% Keep all refs and handle rest.
      get_refs_from(MFAs,[{MFA,Refs}|Acc]);
    [] -> 
      get_refs_from(MFAs, Acc)
  end;
get_refs_from([], Acc) ->
  Acc.

remove_refs([MFA2| Rest], MFA) ->
  remove_ref(MFA2, MFA),
  remove_refs(Rest, MFA);
remove_refs([], _) ->
  true.

remove_ref(CalleeMFA, CallerMFA) ->
  Refs = 
    case hipe_bifs:get_funinfo({CalleeMFA, referred_from}) of
      [{{_,_}, Rs}] -> Rs;
      [] -> []
    end,
  NewRefs = remove_ref(Refs, CallerMFA, []), 
  hipe_bifs:set_funinfo({{CalleeMFA, referred_from}, NewRefs}).


remove_ref([First | Rest], CallerMFA, Acc) ->
  if element(1,First) =:= CallerMFA ->
      remove_ref(Rest, CallerMFA, Acc);
     true -> 
      remove_ref(Rest, CallerMFA, [First|Acc])
  end;
remove_ref([], _CallerMFA, Acc) -> Acc.


redirect_old_refs(Refs, Addresses) ->
  redirect(Refs, Addresses).

redirect([{MFA,Refs}|Rest],Addresses) ->
  redirect(Refs,MFA, Addresses),
  redirect(Rest, Addresses);
redirect([],_) ->
  true.

redirect([{_MFA, Address, Type}| Refs], ToMFA, Addresses) ->
  NewAddress = get_native_address(ToMFA, Addresses),
  ?IF_DEBUG({{M,F,A},{M2,F2,A2}} = {_MFA, ToMFA}, no_debug),
  ?debug_msg("(RE)LINKING ~w:~w/~w (@ 0x~s) to ~w:~w/~w (@ 0x~s)\n",
	     [M,F,A, hipe_converters:int_to_hex(Address), 
	     M2,F2,A2, hipe_converters:int_to_hex(NewAddress)]),
  patch_instr(Address, NewAddress, Type),
  redirect(Refs, ToMFA,Addresses);
redirect([],_,_) ->
  true.




address_to_mfa(Address, [{Adr,MFA,_}|_Rest]) when Address >= Adr -> MFA;
address_to_mfa(Address, [_ | Rest]) -> address_to_mfa(Address, Rest);
address_to_mfa(Address, []) -> 
  ?error_msg("Local adddress not found ~w\n",[Address]),
  exit({?MODULE, local_address_not_found}).


export_funs(Mod, Beam, Addresses, ClosuresToPatch) ->
 Fs = [ {F, A, Address} || {Address , {_M, F, A}, _} <- Addresses ],
 code:make_stub_module(Mod, Beam, {Fs,ClosuresToPatch}).

export_funs([{Address, MFA, Closure} | Addresses]) ->
  ?IF_DEBUG({M,F,A} = MFA, no_debug),
  ?IF_DEBUG(
     case Closure of
       false ->
	 ?debug_msg("LINKING: ~w:~w/~w to (0x~s)\n",
		    [M,F,A, hipe_converters:int_to_hex(Address)]);
       true ->
	 ?debug_msg("LINKING: ~w:~w/~w to closure (0x~s)\n",
		    [M,F,A, hipe_converters:int_to_hex(Address)])
     end, no_debug),
  set_native_address(MFA, Address, Closure),
  export_funs(Addresses);
export_funs([]) ->
  true.


set_native_address(MFA, Address, Closure) ->
  hipe_bifs:set_funinfo({{MFA,native_address}, Address}),
  hipe_bifs:set_native_address(MFA, Address, Closure).

get_bif_or_native_address(MFA, Addresses) ->
  case bif_address(MFA) of
    false -> get_native_address(MFA, Addresses);
    Address -> Address
  end.

%% To find the native code of an MFA we need to look in 3 places:
%%  1. If it is compiled now look in the Addresses data structure.
%%  2. Then look in native_addresses from module info. 
%%  3. Then (the function might have been singled compiled) look in
%%      hipe_funinfo
%%  If all else fails create a native stub for the MFA 
get_native_address(MFA, Addresses) ->
  case mfa_to_address(MFA, Addresses) of
    Adr when integer(Adr) -> Adr;
    false -> get_native_address(MFA)
  end.

get_native_address(MFA={Mod,F,A}) ->
  case hipe_unified_loader:is_loaded(Mod) of 
    true ->  
      find_fa(F,A,Mod:module_info(native_addresses),MFA);
    false -> 
      get_native_address_3(MFA)
  end.

get_native_address_3(MFA={_M,_F,A}) ->
  case hipe_bifs:get_funinfo({MFA,native_address}) of
    [] -> 
      BEAMAddress = get_emu_address(MFA),
      StubAddress = make_native_stub(BEAMAddress,A),
      hipe_bifs:set_funinfo({{MFA,native_address}, StubAddress}),
      StubAddress;
    [{_,Address}] -> Address
  end.

mfa_to_address(MFA, [{Adr,MFA,_}|_Rest]) -> Adr;
mfa_to_address(MFA, [_ | Rest]) -> mfa_to_address(MFA, Rest);
mfa_to_address(_  , []) -> false.

find_fa(F, A, [{F,A, Address} | _],_) -> Address;
find_fa(F, A, [_ | Addresses],MFA) -> find_fa(F, A, Addresses,MFA);
find_fa(_F, _A, [],MFA) -> get_native_address_3(MFA).

%% ____________________________________________________________________
%% 

write_words([W|Rest],Addr) ->
  hipe_bifs:write_u32(Addr, W),
  write_words(Rest,Addr+4);
write_words([],_) -> true.
write_bytes([B|Rest],Addr) ->
  hipe_bifs:write_u8(Addr, B),
  write_bytes(Rest,Addr+1);
write_bytes([],_) -> true.

write(Vector,Addr,Index,Length) ->
  case hipe_bifs:array_length(Vector) of
    0 -> true;
    _ ->
      write_vector(Vector,Addr,Index,Length)
  end.

write_vector(_Vector,_Addr,Length,Length ) -> true;
write_vector(Vector,Addr,Index,Length) ->
  hipe_bifs:write_u8(Addr,hipe_bifs:array_sub(Vector,Index)),
  write_vector(Vector,Addr+1,Index+1,Length).

write([B4,B3,B2,B1|Rest],Addr) ->
  hipe_bifs:write_u32(Addr,bytes_to_32(B4,B3,B2,B1)),
  write(Rest,Addr+4);
write([],_) -> true.


group([],[]) ->
  [];
group([B1,B2,B3,B4|Ls],[O|Os]) -> 
  [{hipe_bifs:term_to_word(O),bytes_to_32(B4,B3,B2,B1)}|group(Ls,Os)].

%% ____________________________________________________________________
%% 

-ifdef(DO_ASSERT).

init_assert_patch(Base,Size) ->
  put(hipe_assert_code_area,{Base,Base+Size}),
  true.

assert_local_patch(Address) ->
  {First,Last} = get(hipe_assert_code_area),
  Address >= First andalso Address < (Last).

-endif.
