%% -*- erlang-indent-level: 2 -*-
%% Copyright (c) 2000 by Erik Johansson.  All Rights Reserved 
%% ====================================================================
%%  Filename : 	hipe_sparc_loader.erl
%%  Module   :	hipe_sparc_loader
%%  Purpose  :  To load SPARC code into memory and link it to the system.
%%  Notes    :  See hipe_sparc_ext_format for description of the external 
%%               format.
%%              Very sparc specific.
%%              Also, the native code-stubs for non-existing functions
%%               depends on the use of specific sparc registers.
%%              See also the todolist below.
%%
%% CVS: $Id$
%% ====================================================================
%% Exported functions (short description):
%%   load(Mod,Bin) - Loads the module Mod from the binary Bin into
%%		     memory and links it with the system
%%   patch_to_emu(Mod) - Patches Mod when newer emulated code has been
%%			 loaded.
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
%%         a. RefsTo = MFA->reffers_to
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
%%   Add support for freeing of catch-indices.
%%   Add freeing of old code. 
%%   Add freeing of catch-indices.
%%   Inline hipe_sparc_ext_format somehow.
%%   Test a lot.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc_loader).
-export([load_module/3,load/2,patch_to_emu/1]).

-define(DO_ASSERT,true).
%-define(DEBUG,true).
-include("hipe.hrl").
-include("hipe_ext_format.hrl").
-include("hipe_unified_loader.hrl").

%% ====================================================================
load_module(Mod, Bin, Beam) ->
  put(hipe_patch_closures,false),
  %% ?debug_msg("In loader \n",[]),
  %% Mod:module_info(), %% Make sure the mod is loaded.
  %% io:format("~w\n",[binary_to_term(Bin)]),
  %%  ?debug_msg("Loading ~w\nFrom: ~p",[Mod, Beam]),
  [Version,
   ConstSize, ConstMap, LabelMap,
   ExportMap,
   HotSize,   HotCode,  HRefs,
   ColdSize,  ColdCode, CRefs]
    = binary_to_term(Bin),
  %% ?debug_msg("0\n",[]),
  ?version_check(Version, Mod),

  %% io:format("~w ~w\n",[ConstSize, ConstMap]),
  {ConstAddr,ConstMap2} = 
    alloc_constants(ConstSize,ConstMap),
   
  %% ?debug_msg("ConstData @ ~w ~w words\n",[ConstAddr,ConstSize div 4]),
  
  %% ?debug_msg("1\n",[]),
  %% TODO: Add exporting of exported constsants.

  %% Write the code to memory.
  HotAddress =  hipe_bifs:alloc_code(HotSize+ColdSize),
  ColdAddress =  HotAddress + HotSize,
  write(HotCode,HotAddress),
  write(ColdCode,ColdAddress),
  %% ?debug_msg("2\n",[]),
  
  %% Patch references to code labels in data seg.
  %%
  patch_consts(LabelMap, ConstAddr, HotAddress),
  

  %% Find out which functions are being loaded (And where).
  %% Note: Addresses are sorted descending.
  {MFAs, Addresses} = exports(ExportMap,HotAddress),
  %% ?debug_msg("3\n",[]),
  %% 
  ReferencesToPatch = get_and_remove_refs(MFAs),
  %% ?debug_msg("4 ~w\n",[ReferencesToPatch]),
  %% Patch all dynamic referenses in the code.
  %%  Function calls, Atoms, Constants, System calls
  %% ?debug_msg("HRefs ~p\n",[HRefs]),
  patch(HRefs,HotAddress,{ConstMap2,HotAddress,ColdAddress}, Addresses),
  patch(CRefs,ColdAddress,{ConstMap2,HotAddress,ColdAddress}, Addresses),
  HotClosurePatches = find_closure_patches(HRefs),
  ColdClosurePatches = find_closure_patches(CRefs),
  AddressesOfClosuresToPatch =
    calculate_addresses(HotClosurePatches,HotAddress, Addresses) ++
    calculate_addresses(ColdClosurePatches,ColdAddress, Addresses),

  %% ?debug_msg("5\n",[]),
  %% Tell the system where the loaded funs are. 
  %%  (patches the BEAM code to redirect to native.)
  export_funs(Mod, Beam, Addresses,AddressesOfClosuresToPatch),
  %% ?debug_msg("6\n",[]),
  %% Patch reffering functions to call the new function
  redirect_old_refs(ReferencesToPatch, Addresses),
  %% ?debug_msg("7\n",[]),
  {module,Mod}.  %% for compatibility with code:load_file/1

load(Mod,Bin) ->
  put(hipe_patch_closures,true),
  %% ?debug_msg("In loader \n",[]),
  %% Mod:module_info(), %% Make sure the mod is loaded.
  %% io:format("~w\n",[binary_to_term(Bin)]),
  %% ?debug_msg("Loading ~w\n ",[Mod]),
  [Version,
   ConstSize, ConstMap, LabelMap,
   ExportMap,
   HotSize,   HotCode,  HRefs,
   ColdSize,  ColdCode, CRefs]
    = binary_to_term(Bin),
  %% ?debug_msg("0\n",[]),
  ?version_check(Version, Mod),

  %% io:format("~w ~w\n",[ConstSize, ConstMap]),
  {ConstAddr,ConstMap2} = 
    alloc_constants(ConstSize,ConstMap),
   
  %% ?debug_msg("ConstData @ ~w ~w words\n",[ConstAddr,ConstSize div 4]),
  
  %% ?debug_msg("1\n",[]),
  %% TODO: Add exporting of exported constants.

  %% Write the code to memory.
  HotAddress =  hipe_bifs:alloc_code(HotSize+ColdSize),
  ColdAddress =  HotAddress + HotSize,
  write(HotCode,HotAddress),
  write(ColdCode,ColdAddress),
  %% ?debug_msg("2\n",[]),
  
  %% Patch references to code labels in data seg.
  %%
  patch_consts(LabelMap, ConstAddr, HotAddress),
  

  %% Find out which functions are being loaded (And where).
  %% Note: Addresses are sorte descending.
  {MFAs, Addresses} = exports(ExportMap,HotAddress),
  %% ?debug_msg("3\n",[]),
  %% 
  ReferencesToPatch = get_and_remove_refs(MFAs),
 %%  ?debug_msg("4 ~p\n",[ReferencesToPatch]),
  %% Patch all dynamic referenses in the code.
  %%  Function calls, Atoms, Constants, System calls
  patch(HRefs,HotAddress,{ConstMap2,HotAddress,ColdAddress}, Addresses),
  patch(CRefs,ColdAddress,{ConstMap2,HotAddress,ColdAddress}, Addresses),
  HotClosurePatches = find_closure_patches(HRefs),
  ColdClosurePatches = find_closure_patches(CRefs),
  %% ?debug_msg("5\n",[]),
  %% Tell the system where the loaded funs are. 
  %%  (patches the BEAM code to redirect to native.)
  export_funs(Addresses),
  %% ?debug_msg("6\n",[]),
  %% Patch reffering functions to call the new function
  redirect_old_refs(ReferencesToPatch, Addresses),
  %% ?debug_msg("7\n",[]),
  {module,Mod}.  %% for compatibility with code:load_file/1


%% ____________________________________________________________________
%% 
%% We do not want to use map or listcomprehensions here since 
%% the module list might not be loaded.
calculate_addresses([{{DestMFA,_,_}=Data,Offsets}|Rest],Base,Addresses) ->
  DestAddress = get_fun_address(DestMFA, Addresses),
  [{Data,offest_to_address(Offsets,Base), DestAddress} |
   calculate_addresses(Rest,Base,Addresses)];
calculate_addresses([],_,_) -> [].
offest_to_address([O|Os],Base) ->
  [{O+Base,sethi},{O+Base+4,'or'}|offest_to_address(Os,Base)];
offest_to_address([],_) -> [].


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
  

patch([{Type, Refs} | Rest], BaseAddress, ConstAndZone, Addresses)->
  SortedRefs = lists:sort(Refs),
  case ?EXT2PATCH_TYPE(Type) of 
    call_local_hot -> 
      patch_all_call_local(SortedRefs, BaseAddress, Addresses);
    call_remote ->
      patch_all_call_remote(0, SortedRefs, BaseAddress, Addresses, []);
    call_local_cold -> 
      patch_all_call_local(SortedRefs, BaseAddress, Addresses);

%%    load_address -> 
%%      patch_load_address(Data, Address, ConstAndZone, Addresses);
%%    call_local -> 
%%      %% This is a call to a local fun that is not compiled now.
%%      patch_call_local_emu(Data, Address, Addresses);


    Other -> 
      patch_all(Type, Refs, BaseAddress, ConstAndZone, Addresses)
  end,


  patch(Rest, BaseAddress, ConstAndZone, Addresses);
patch([],_,_, _) -> true.

patch_all(Type, [{Dest,Offsets}|Rest], BaseAddress, ConstAndZone, Addresses)->
  patch_all_offsets(Type, Dest, Offsets, BaseAddress, ConstAndZone, Addresses),
  patch_all(Type, Rest, BaseAddress, ConstAndZone, Addresses);
patch_all(_, [], _, _, _) -> true.

%% ____________________________________________________________________
%% 
patch_all_call_local([{DestMFA, Offsets}| SortedRefs], 
		     BaseAddress, Addresses)->
  DestAddress = mfa_to_address(DestMFA, Addresses),
%%  DestAddress = get_fun_address(DestMFA),
  patch_all_call_local_offsets(Offsets,DestMFA, 
			       DestAddress, BaseAddress, Addresses);
patch_all_call_local([],_,_) -> true.
patch_all_call_local_offsets([Offset|Offsets], DestMFA, DestAddress, 
			     BaseAddress, Addresses) ->
  Address = BaseAddress + Offset,
  CallerMFA = address_to_mfa(Address, Addresses),
  add_ref(CallerMFA, DestMFA, Address,calllinkref()),
  ?debug_msg("Patching ~w at [~w+~w]\n",
	     [call_local,BaseAddress,Offset]),

  patch_call_instr(Address,DestAddress),
  patch_all_call_local_offsets(Offsets, DestMFA, DestAddress, 
			       BaseAddress, Addresses);
patch_all_call_local_offsets([],_,_,_,_) ->
  true.


%% ____________________________________________________________________
%% 
patch_all_call_remote(Mod, [{{Mod,F,A} = DestMFA,Offsets} | SortedRefs], 
		      BaseAddress, Addresses, NativeaddressesInMod) ->
  %% Check if this remote call is to a function that is compiled now.
  DA = case mfa_to_address(DestMFA, Addresses) of
	 false -> %% Not compiled now, look in list of native addresses.
	   get_native_fun_address(DestMFA, NativeaddressesInMod);
	 DstAdr -> DstAdr %% We have the address.
       end,

  patch_all_call_remote_offsets(Offsets,DestMFA, DA, BaseAddress, Addresses),
  patch_all_call_remote(Mod, SortedRefs,
			BaseAddress, Addresses, NativeaddressesInMod);
patch_all_call_remote(LastMod, [{{Mod,F,A} = DestMFA,Offsets} | SortedRefs], 
		      BaseAddress, Addresses, _) ->
  case mfa_to_address(DestMFA, Addresses) of
    false -> %% Not compiled now, look in list of native addresses
      NativeaddressesInMod = 
	case is_loaded(Mod) of 
	  true -> Mod:module_info(native_addresses);
	  false -> []
	end,
      DA = get_native_fun_address(DestMFA, NativeaddressesInMod),
      patch_all_call_remote_offsets(Offsets,DestMFA, DA, BaseAddress, 
				    Addresses),
      patch_all_call_remote(Mod, SortedRefs,
			    BaseAddress, Addresses, NativeaddressesInMod);
    DA -> %% Compiled now, we have the address.
      patch_all_call_remote_offsets(Offsets,DestMFA, DA, BaseAddress, 
				    Addresses),
      patch_all_call_remote(LastMod, SortedRefs,
			    BaseAddress, Addresses, [])
      end;

patch_all_call_remote(_,[],_,_,_) -> true.


patch_all_call_remote_offsets([Offset|Offsets], DestMFA, DestAddress, 
			     BaseAddress, Addresses) ->
  Address = BaseAddress + Offset,
  CallerMFA = address_to_mfa(Address, Addresses),
  ?debug_msg("Patching ~w at [~w+~w] to ~w\n",
	     [call_remote,BaseAddress,Offset, DestAddress]),

  add_ref(CallerMFA, DestMFA, Address, calllinkref()),
  patch_call_instr(Address, DestAddress),
  patch_all_call_remote_offsets(Offsets, DestMFA, DestAddress, 
			     BaseAddress, Addresses);
patch_all_call_remote_offsets([],_,_,_,_) -> true.



patch_all_offsets(Type, Data, [Offset|Offsets], BaseAddress,
		  ConstAndZone, Addresses) ->
  Address = BaseAddress + Offset,
  ?debug_msg("Patching ~w at [~w+~w]\n",
	     [?EXT2PATCH_TYPE(Type),BaseAddress,Offset]),
  case ?EXT2PATCH_TYPE(Type) of 
    call_bif -> %% This should be removed
      patch_call_bif(Data, Address, Addresses);
    load_address -> 
      patch_load_address(Data, Address, ConstAndZone, Addresses);
    load_atom ->
      patch_hi_or(Address, hipe_bifs:atom_to_word(Data));
    call_local -> 
      %% This is a call to a local fun that is not compiled now.
      patch_call_local_emu(Data, Address, Addresses);
    Other -> 
      ?error_msg("Unknown ref ~w ~w ~w\n",[Type, Offset, Data]),
      exit({unknown_reference, Type, Offset, Data})
  end,
  
  patch_all_offsets(Type, Data, Offsets, BaseAddress, ConstAndZone, Addresses);
patch_all_offsets(_,_,[],_,_,_) -> true.

patch_call_local_emu(DestMFA, Address, Addresses)->
  %% This is a local call to a fun that is not compiled at 
  %% this time.
  CallerMFA = address_to_mfa(Address, Addresses),
  DestAddress = get_fun_address(DestMFA),

  add_ref(CallerMFA, DestMFA, Address,calllinkref()),
  patch_call_instr(Address,DestAddress).



patch_call_bif(BIF,Address, Addresses)->
  BifAddress = bif_address(BIF),
  patch_call_instr(Address, BifAddress).



patch_load_address({const,Name},Address,Info, Addresses)->
  ConstAddress = find_const(Name,element(1,Info)),
  patch_hi_or(Address,ConstAddress);
patch_load_address({bif,BIF},Address,_, Addresses)->
  BifAddress = bif_address(BIF),
  patch_hi_or(Address, BifAddress);
patch_load_address({local_function,DestMFA},Address,_, Addresses)->
  Arity = element(3,DestMFA),
  DestAddress = get_fun_address(DestMFA, Addresses),
  CallerMFA = address_to_mfa(Address, Addresses),
  add_ref(CallerMFA, DestMFA,Address,sethiref()),
  add_ref(CallerMFA, DestMFA,Address+4,orref()),
  patch_hi_or(Address, DestAddress);
patch_load_address({remote_function,DestMFA},Address,_, Addresses)->
  %% TODO: Handle references to local and remote functions differnetly.
  Arity = element(3,DestMFA),
  DestAddress = get_fun_address(DestMFA, Addresses),
  CallerMFA = address_to_mfa(Address, Addresses),
  add_ref(CallerMFA, DestMFA,Address,sethiref()),
  add_ref(CallerMFA, DestMFA,Address+4,orref()),
  patch_hi_or(Address, DestAddress);
patch_load_address({closure,{DestMFA,Uniq, Index}},Address,_, Addresses)->
  %% TODO: Put fe in global table
  case get(hipe_patch_closures) of
    false ->
      true; %% This is taken care of when registering the module.
    true -> %% We are not loading a module patch these closures
      Arity = element(3,DestMFA),
      DestAddress = get_fun_address(DestMFA, Addresses),
      BEAMAddress = get_emu_address(DestMFA),
      FE = hipe_bifs:make_fe(DestAddress, element(1,DestMFA), 
			     {Uniq, Index,BEAMAddress}),
      ?debug_msg("Patch FE(~w) to 0x~s->0x~s (emu:0x~s)\n",
		 [DestMFA, hipe_converters:int_to_hex(FE),
		  hipe_converters:int_to_hex(DestAddress),
		  hipe_converters:int_to_hex(BEAMAddress)]), 
      patch_hi_or(Address, FE)
  end;

patch_load_address({label,RefZone,Offset,MFA},Address,{ConstMap,HotAddress,ColdAddress}, Addresses) ->
  case RefZone of
    hot ->
      patch_hi_or(Address,Offset+HotAddress);
    cold -> 
      patch_hi_or(Address,Offset+ColdAddress)
  end;
patch_load_address({'catch',RefZone,Offset,MFA},
		   Address,{ConstMap,HotAddress,ColdAddress}, Addresses) ->
  LabelAddress = 
    case RefZone of
      hot ->
	Offset+HotAddress;
      cold -> 
	Offset+ColdAddress
    end,
  %% TODO: Fix the insert so that indices can be reused.
  CatchIndex = hipe_bifs:catch_table_insert(LabelAddress, hipe_bifs:catch_table_nil()),
  CatchVal = hipe_bifs:catch_index_to_word(CatchIndex),
  patch_hi_or(Address, CatchVal).


patch_call_instr(Address,DestAddress) ->
  RelDest = DestAddress - Address,
  NewI = ((1 bsl 30) bor (bits_32(RelDest) bsr 2)),
  hipe_bifs:write_u32(Address,NewI).

calllinkref() ->
  1.
sethiref() ->
  2.
orref()->
  3.

patch_instr(Addr, Val, Type) ->
  case Type of
    1 -> patch_call_instr(Addr, Val);
    2 -> Hi = (Val bsr 10),
	 patch_sethi(Addr, Hi);
    3 -> Lo = (Val band 16#3FF),
	 patch_alu(Addr, Lo)
  end.
      

patch_hi_or(Address,Value) ->
  %% io:format("Address ~w, Value ~w\n",[Address, Value]),
  Hi = (Value bsr 10),
  patch_sethi(Address,Hi),
  Lo = (Value band 16#3FF),
  patch_alu(Address+4,Lo).

patch_sethi(Addr,Bits) ->
  I = hipe_bifs:read_u32(Addr),
  NewI = ((I band (2#1111111111 bsl 22)) bor bits_22(Bits)),
  hipe_bifs:write_u32(Addr,NewI).


patch_alu(Addr,Bits) ->
  I = hipe_bifs:read_u32(Addr),
  NewI = ((I band (2#1111111111111111111 bsl 13)) bor bits_13(Bits)),
  hipe_bifs:write_u32(Addr,NewI).


%bits_10(X) -> X band      16#3ff.
bits_13(X) -> X band     16#1fff.
%bits_14(X) -> X band     16#3fff.
%bits_16(X) -> X band     16#ffff.
%bits_19(X) -> X band    16#7ffff.
bits_22(X) -> X band   16#3fffff.
%bits_30(X) -> X band 16#3fffffff.
bits_32(X) -> X band 16#ffffffff.


write([B4,B3,B2,B1|Rest],Addr) ->
  hipe_bifs:write_u32(Addr,bytes_to_32(B4,B3,B2,B1)),
  write(Rest,Addr+4);
write([],_) -> true.


redirect_old_refs(Refs, Addresses) ->
  %% io:format("To patch ~w \n", [Refs]),
  redirect(Refs, Addresses).

redirect([{MFA,Refs}|Rest],Addresses) ->
  redirect(Refs,MFA, Addresses),
  redirect(Rest, Addresses);
redirect([],_) ->
  true.

redirect([{MFA, Address, Type}| Refs], ToMFA, Addresses) ->
  NewAddress = get_fun_address(ToMFA, Addresses),
  {M,F,A} = MFA,
  {M2,F2,A2} = ToMFA,
  ?debug_msg("(RE)LINKING ~w:~w/~w (@ 0x~s) to ~w:~w/~w (@ 0x~s)\n",
	    [M,F,A, hipe_converters:int_to_hex(Address), 
	     M2,F2,A2, hipe_converters:int_to_hex(NewAddress)]),
  patch_instr(Address, NewAddress, Type),
  redirect(Refs, ToMFA,Addresses);
redirect([],_,_) ->
  true.




set_native_address(MFA, Address, Closure) ->
  hipe_bifs:set_funinfo({{MFA,native_address}, Address}),
  hipe_bifs:set_native_address(MFA, Address, Closure).

get_native_address(MFA) ->
  case hipe_bifs:get_funinfo({MFA,native_address}) of
    [] -> false;
    [{_,Address}] -> Address
  end.

make_native_stub(Address, Arity) ->
  CodeSize = 5*4,
  Mem = hipe_bifs:alloc_code(CodeSize),
  RelOffset = (hipe_bifs:primop_address(callemu)-(Mem+3*4)) ,

  Code = 
    [%% sethi $temp0, hi(Address)
     ((452984832 band (2#1111111111 bsl 22)) bor bits_22((Address bsr 10))), 
     %% move $temp2, $CP
     2819620879,
     %% or $temp0, lo(Address)
     ((2584961024 band (2#1111111111111111111 bsl 13)) 
      bor bits_13((Address band 16#3FF))),
     %% Call callemu

     ((1 bsl 30) bor (bits_32(RelOffset) bsr 2)),

     %% mov $temp1, Arity
     ((3054510081 band (2#1111111111111111111 bsl 13)) bor
      bits_13(Arity))],
  write_words(Code, Mem),
  Mem.

%make_native_closure_stub(Arity) ->
%  case Arity of
%    0 -> exit(bad_closurearity);
%    1 -> hipe_bifs:primop_address(ccallemu0);
%    2 -> hipe_bifs:primop_address(ccallemu1);
%    3 -> hipe_bifs:primop_address(ccallemu2);
%    4 -> hipe_bifs:primop_address(ccallemu3);
%    5 -> hipe_bifs:primop_address(ccallemu4);
%    _ ->
%      %% Todo: Only create this stub once for each arity.
%      CodeSize = 2*4,
%      Mem = hipe_bifs:alloc_code(CodeSize),
%      Code = 
%	[
%	 %% Call callemu
%	 ((1 bsl 30) bor (bits_32(hipe_bifs:primop_address(ccallemu5)-
%				  (Mem+3*4)) bsr 2)),
%	 %% mov $temp1, Arity
%	 ((3054510081 band (2#1111111111111111111 bsl 13)) bor
%	  bits_13(Arity))],
%      write_words(Code, Mem),
%      Mem
%  end.



address_to_mfa(Address, [{Adr,MFA,_}|Rest]) when Address >= Adr -> MFA;
address_to_mfa(Address, [_ | Rest]) -> address_to_mfa(Address, Rest);
address_to_mfa(Address, []) -> 
  ?error_msg("Local adddress not found ~w\n",[Address]),
  exit({?MODULE, local_address_not_found}).

mfa_to_address(MFA, [{Adr,MFA,_}|Rest]) -> Adr;
mfa_to_address(MFA, [_ | Rest]) -> mfa_to_address(MFA, Rest);
mfa_to_address(_  , []) -> false.

get_native_fun_address({_, F, A} = MFA, Addresses) ->
  get_native_fun_address(F, A, Addresses, MFA).
get_native_fun_address(F, A, [{F,A, Address} | _], _) ->  
  Address;
get_native_fun_address(F, A, [_ | Addresses], MFA) ->
  get_native_fun_address(F, A, Addresses, MFA);
get_native_fun_address(F, A, _, MFA) ->
  %% Make a stub if a stub does not exist.
  case get_native_address(MFA) of
    false ->
      BEAMAddress = get_emu_address(MFA),
      StubAddress = make_native_stub(BEAMAddress,A),
      hipe_bifs:set_funinfo({{MFA,native_address}, StubAddress}),
      StubAddress;
    Address -> Address
  end.
 
 
get_fun_address(MFA, Addresses) ->
  case mfa_to_address(MFA, Addresses) of
    Adr when integer(Adr) -> Adr;
    false -> get_fun_address(MFA)
  end.
      
get_fun_address(DestMFA) ->
  case get_native_address(DestMFA) of
    false -> %% No Native code.
      BEAMAddress = get_emu_address(DestMFA),
      make_native_stub(BEAMAddress,element(3,DestMFA));      
    FunAddress -> FunAddress
  end.

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
      %% io:format("~w:~w/~w - stubaddress 0x~s\n",
      %%	   [M,F,A,hipe_converters:int_to_hex(Address)]),
      Address;
    Addr ->  
      Addr
  end.

make_stubs([{_,_,A} = MFA|MFAs]) ->
  EmuAddress = get_emu_address(MFA),
  StubAddress = make_native_stub(EmuAddress, A),
  hipe_bifs:set_funinfo({{MFA,native_address}, StubAddress}),
  make_stubs(MFAs);
make_stubs([]) ->
  true.

patch_to_emu(Mod) ->
  case is_loaded(Mod) of
    true ->
      MFAs = [{Mod,Fun, Arity} || {Fun, Arity} <-
				    Mod:module_info(exports)],
      make_stubs(MFAs),
      ReferencesToPatch = get_and_remove_refs(MFAs),
      redirect_to_emu(ReferencesToPatch);
    false ->
      ok
  end.

redirect_to_emu([{MFA,Refs}|Rest]) ->
  redirect_to_emu(Refs,MFA),
  redirect_to_emu(Rest);
redirect_to_emu([]) ->
  true.

redirect_to_emu([{{M,F,A}, Address, Type}| Refs],  
		{M2,F2,A2} = ToMFA) ->
  StubAddress = get_native_address(ToMFA),
  ?debug_msg("(RE)LINKING ~w:~w/~w (@ 0x~s) to EMU ~w:~w/~w (@ 0x~s)\n",
	    [M,F,A, hipe_converters:int_to_hex(Address), 
	     M2,F2,A2, hipe_converters:int_to_hex(StubAddress)]),
  patch_instr(Address, StubAddress, Type),
  redirect_to_emu(Refs, ToMFA);
redirect_to_emu([],_) ->
  true.

export_funs(Mod, Beam, Addresses, ClosuresToPatch) ->
?IF_DEBUG_LEVEL(1, 
 [ ?debug_msg("Exporting ~w:~w/~w @ 0x~s\n",[M,F, A, hipe_converters:int_to_hex(Address)])
	|| {Address , {M, F, A}, _} <- Addresses ], ok),
 Fs = [ {F, A, Address} || {Address , {M, F, A}, _} <- Addresses ],
 case catch code:make_stub_module(Mod, Beam, {Fs,ClosuresToPatch}) of
   {'EXIT',R} ->
     ?EXIT({could_not_create_beam_stub});
   Res -> Res
 end.

export_funs([{Address, MFA, Closure} | Addresses]) ->
  {M,F,A} = MFA,
  case Closure of
    false ->
      ?debug_msg("LINKING: ~w:~w/~w to (0x~s)\n",
		 [M,F,A, hipe_converters:int_to_hex(Address)]);
    true ->
      ?debug_msg("LINKING: ~w:~w/~w to closure (0x~s)\n",
		 [M,F,A, hipe_converters:int_to_hex(Address)])
  end,
  set_native_address(MFA, Address, Closure),
  export_funs(Addresses);
export_funs([]) ->
  true.
