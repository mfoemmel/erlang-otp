%%% $Id$
%%% hipe_x86_loader
%%% Copyright (C) 2001 Ulf Magnusson
%%% Email: ulf.magnusson@ubm-computing.com

-module(hipe_x86_loader).
-export([load/2,load_module/3,patch_to_emu/1]).
-include("../../hipe/main/hipe.hrl").
-include("../../hipe/x86/hipe_x86.hrl").
-include("hipe_ext_format.hrl").


load_module(Mod,Bin,Beam) ->
  put(hipe_patch_closures,false),
    print("******************** Loading ******************\n"),
    [Version,
     ConstSize, ConstMap, LabelMap,
     ExportMap,
     HotSize,   HotCode,  HRefs,
     ColdSize,  ColdCode, CRefs]
	= binary_to_term(Bin),
    hipe_unified_loader:version_check(Version, Mod),

    print("Load ~w bytes\n",[HotSize]),
    {ConstAddr,ConstMap2} = hipe_unified_loader:alloc_constants(ConstSize,ConstMap),

    %% Write the code to memory.
    HotAddress =  hipe_bifs:alloc_code(HotSize+ColdSize),
    ColdAddress =  HotAddress + HotSize,
    write(HotCode,HotAddress,0,HotSize),
    %write(ColdCode,ColdAddress,0,ColdSize),
    
    %% Patch references to code labels in data seg.
    hipe_unified_loader:patch_consts(LabelMap, ConstAddr, HotAddress),
    
    %% Find out which functions are being loaded (And where).
    %% Note: Addresses are sorte descending.
    {MFAs, Addresses} = hipe_unified_loader:exports(ExportMap,HotAddress),
    print("Export MFAs:~w\n",[Addresses]),
  
    ReferencesToPatch = hipe_unified_loader:get_and_remove_refs(MFAs),
    %print("Refs to patch:~w\n",[ReferencesToPatch]),

    print("Patches from assembler:~w\n",[HRefs]),
    %% Patch all dynamic referenses in the code.
    %%  Function calls, Atoms, Constants, System calls
    %io:format("before patch\n"),
    patch(HRefs,HotAddress,{ConstMap2,HotAddress,ColdAddress}, Addresses),
    patch(CRefs,ColdAddress,{ConstMap2,HotAddress,ColdAddress}, Addresses),
    HotClosurePatches = find_closure_patches(HRefs),
    ColdClosurePatches = find_closure_patches(CRefs),
%%    ?msg("To patch ~w\n", [HRefs]),
    AddressesOfClosuresToPatch =
      calculate_addresses(HotClosurePatches,HotAddress, Addresses) ++
      calculate_addresses(ColdClosurePatches,ColdAddress, Addresses),
%%    ?msg("Addresses to patch ~w\n", [AddressesOfClosuresToPatch]),
    %% Tell the system where the loaded funs are. 
    %%  (patches the BEAM code to redirect to native.)
   export_funs(Mod, Beam, Addresses,AddressesOfClosuresToPatch),

    %% Patch reffering functions to call the new function
    redirect_old_refs(ReferencesToPatch),

    print("****************Loader Finished****************\n"),
    {module,Mod}.  %% for compatibility with code:load_file/1

%% ____________________________________________________________________
%% 




load(Mod,Bin) ->

    print("******************** Loading ******************\n"),
  put(hipe_patch_closures,true),
    Mod:module_info(), %% Make sure the mod is loaded.
    [Version,
     ConstSize, ConstMap, LabelMap,
     ExportMap,
     HotSize,   HotCode,  HRefs,
     ColdSize,  ColdCode, CRefs]
	= binary_to_term(Bin),
    hipe_unified_loader:version_check(Version, Mod),

    print("Load ~w bytes\n",[HotSize]),
    {ConstAddr,ConstMap2} = hipe_unified_loader:alloc_constants(ConstSize,ConstMap),

    %% Write the code to memory.
    HotAddress =  hipe_bifs:alloc_code(HotSize+ColdSize),
    ColdAddress =  HotAddress + HotSize,
    write(HotCode,HotAddress,0,HotSize),
    %write(ColdCode,ColdAddress,0,ColdSize),
    
    %% Patch references to code labels in data seg.
    hipe_unified_loader:patch_consts(LabelMap, ConstAddr, HotAddress),
    
    %% Find out which functions are being loaded (And where).
    %% Note: Addresses are sorte descending.
    {MFAs, Addresses} = hipe_unified_loader:exports(ExportMap,HotAddress),
    print("Export MFAs:~w\n",[Addresses]),
  
    ReferencesToPatch = hipe_unified_loader:get_and_remove_refs(MFAs),
    %print("Refs to patch:~w\n",[ReferencesToPatch]),

    print("Patches from assembler:~w\n",[HRefs]),
    %% Patch all dynamic referenses in the code.
    %%  Function calls, Atoms, Constants, System calls
    %io:format("before patch\n"),
    patch(HRefs,HotAddress,{ConstMap2,HotAddress,ColdAddress}, Addresses),
    patch(CRefs,ColdAddress,{ConstMap2,HotAddress,ColdAddress}, Addresses),

    %% Tell the system where the loaded funs are. 
    %%  (patches the BEAM code to redirect to native.)
    export_funs(Addresses),

    %% Patch reffering functions to call the new function
    redirect_old_refs(ReferencesToPatch),

    print("****************Loader Finished****************\n"),
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
  [{O+Base,address}|offest_to_address(Os,Base)];
offest_to_address([],_) -> [].


%% ____________________________________________________________________
%% 

find_closure_patches([{Type,Address,{closure,Data}} | Rest]) ->
      [{Data,[Address]}|find_closure_patches(Rest)];
find_closure_patches([_ | Rest]) ->
 find_closure_patches(Rest);
find_closure_patches([]) -> [].
  

% patch([{Type, Refs} | Rest], BaseAddress, ConstAndZone, Addresses)->
%   patch_all(Type, Refs, BaseAddress, ConstAndZone, Addresses),
%   patch(Rest, BaseAddress, ConstAndZone, Addresses);
% patch([],_,_, _) -> true.

% patch_all(Type, [{Dest,Offsets}|Rest], BaseAddress, ConstAndZone, Addresses)->
%   patch_all_offsets(Type, Dest, Offsets, BaseAddress, ConstAndZone, Addresses),
%   patch_all(Type, Rest, BaseAddress, ConstAndZone, Addresses);
% patch_all(_, [], _, _, _) -> true.

patch([],_,_,_) -> true;
patch([{Type,Addr,Data}|Rest],HotAddress,{ConstMap2,HotAddress,ColdAddress}, Addresses) ->
    patch_offset(Type,Data,Addr,HotAddress,{ConstMap2,HotAddress,ColdAddress},Addresses),
    patch(Rest,HotAddress,{ConstMap2,HotAddress,ColdAddress}, Addresses).

% Time for some patching, ja
patch_offset(Type, Data, Offset, BaseAddress,
		  ConstAndZone, Addresses) ->
    Address = BaseAddress + Offset,
    print("Patching ~w at [~w+~w]\n",[?EXT2PATCH_TYPE(Type),BaseAddress,Offset]),
    case ?EXT2PATCH_TYPE(Type) of 
	load_atom ->
	    %print("Patching Atom:~w at address:~w\n",[Data,Address]),
	    hipe_bifs:write_u32(Address,hipe_bifs:atom_to_word(Data));
	mfa ->
	    %print("Patching jmp/call to MFA=~w\n",[Data]),
	    %print("Addresses:~w\n",[Addresses]);
	    patch_mfa(Data, Address, Addresses);
	call_bif ->
	    %print("Patching call to bif:~w\n",[Data]),
	    BifAddress = hipe_unified_loader:bif_address(Data) - Address - 4, % should be + 4 ?
	    hipe_bifs:write_s32(Address,BifAddress);
	load_address ->
	    print("Patching load (constant/label/catch):~w\n",[Data]),
	    patch_load_address(Data, Address, ConstAndZone, Addresses);
	sdesc ->
	    %% io:format("Patching stack-descriptor\n"),
	    #x86_sdesc{exnlab=ExnLab,fsize=FSize,skip=Skip,arity=Arity} = Data,
	    %% XXX: alloc_code/1 is semantically wrong here because we're
	    %% storing data not code. (This will hurt on K7 and P4.)
	    %% alloc_constant/1 would be wrong too, since _it's_ supposed
	    %% to be used for Erlang terms only. Eventually we should have
	    %% a per-compilation-unit non-Erlang data area, for stuff like
	    %% stack descriptors and jump tables.
	    %%
	    ExnRA =
		case ExnLab of
		    [] -> 0;
		    LabelOffset ->
			{_,HotAddress,_} = ConstAndZone,
			HotAddress + LabelOffset
		end,
	    %%
	    AltRA = hipe_bifs:alloc_code(8),
	    hipe_bifs:write_u8(AltRA, 16#E9),
	    hipe_bifs:write_s32(AltRA+1, Address - (AltRA+5)),
	    %%
	    AltSDescPtr = hipe_bifs:alloc_code(28),	% XXX: not code
	    hipe_bifs:write_u32(AltSDescPtr+ 0, 0),	% hash link
	    hipe_bifs:write_u32(AltSDescPtr+ 4, AltRA),	% RA (key)
	    hipe_bifs:write_u32(AltSDescPtr+ 8, 0),	% AltRA=0
	    hipe_bifs:write_u32(AltSDescPtr+12, ExnRA),
	    hipe_bifs:write_u32(AltSDescPtr+16, FSize),
	    hipe_bifs:write_u32(AltSDescPtr+20, Arity),
	    hipe_bifs:write_u32(AltSDescPtr+24, 0),	% SkipLength=0
	    hipe_bifs:enter_sdesc(AltSDescPtr),
	    %%
	    SkipLength = length(Skip),
	    SDescPtr = hipe_bifs:alloc_code(28 + SkipLength*4),
	    hipe_bifs:write_u32(SDescPtr+ 0, 0),	% hash link
	    hipe_bifs:write_u32(SDescPtr+ 4, Address),	% RA (key)
	    hipe_bifs:write_u32(SDescPtr+ 8, AltRA),
	    hipe_bifs:write_u32(SDescPtr+12, ExnRA),
	    hipe_bifs:write_u32(SDescPtr+16, FSize),
	    hipe_bifs:write_u32(SDescPtr+20, Arity),
	    hipe_bifs:write_u32(SDescPtr+24, SkipLength),
	    lists:foldl(fun(Value, Off) -> hipe_bifs:write_u32(Off, Value), Off+4 end, SDescPtr+28, Skip),
	    hipe_bifs:enter_sdesc(SDescPtr);
	Other -> 
	    print("Unknown ref ~w ~w ~w\n",[Type, Offset, Data]),
	    exit({unknown_reference, Type, Offset, Data})
    end.
    
    %patch_all_offsets(Type, Data, Offsets, BaseAddress, ConstAndZone, Addresses);
%patch_all_offsets(_,_,[],_,_,_) -> true.

callback_abs_address() ->
  1.

address_to_mfa(Address, [{Adr,MFA,_}|Rest]) when Address >= Adr -> MFA;
address_to_mfa(Address, [_ | Rest]) -> address_to_mfa(Address, Rest);
address_to_mfa(Address, []) -> 
  ?error_msg("Local adddress not found ~w\n",[Address]),
  exit({?MODULE, local_address_not_found}).

%%% The target of an MFA call can be a BIF or an Erlang function.
%%% We have to check for both possibilities, BIF first.
patch_mfa(DestMFA={M,F,A}, Address, Addresses) ->
    CalleeAddress =
	case catch hipe_bifs:bif_address(M,F,A) of
	    BifAddress when integer(BifAddress) ->
		BifAddress;
	    _ ->
		DestAddress =
		    if
			element(1, element(2,hd(Addresses))) == element(1,DestMFA) ->
			    get_fun_address(DestMFA, Addresses);
			true ->
			    get_fun_address(DestMFA)
		    end,
		CallerMFA = address_to_mfa(Address, Addresses),
		hipe_unified_loader:add_ref(CallerMFA, DestMFA, Address,
					    callback_abs_address()),
		DestAddress
	end,
    Offset = CalleeAddress - Address - 4,
    hipe_bifs:write_s32(Address, Offset).

% This only supports 'constants' unlike the sparc version
patch_load_address( {constant,Name},Address,Info, Addresses)->
    ConstAddress = hipe_unified_loader:find_const(Name,element(1,Info)),
    hipe_bifs:write_u32( Address, ConstAddress );
patch_load_address( {label,Offset},Address,{ConstMap,HotAddress,ColdAddress}, Addresses)->
    hipe_bifs:write_s32( Address, HotAddress+Offset );
patch_load_address( {offset,Offset}, Address,{ConstMap,HotAddress,ColdAddress}, Addresses ) ->
    RealAddr = Offset+HotAddress,
    print("Offset patched at Absolute addr:~w relative:~w\n",[RealAddr,Offset]),
    hipe_bifs:write_u32(Address, RealAddr);
patch_load_address({closure,{DestMFA,Uniq, Index}},Address,_, Addresses) ->
    %% TODO: Put fe in global table
    case get(hipe_patch_closures) of
	false ->
	    true; %% This is taken care of when registering the module.
	true ->   %% We are not loading a module patch these closures
	    Arity = element(3,DestMFA),
	    DestAddress = get_fun_address(DestMFA, Addresses),
	    BEAMAddress = get_emu_address(DestMFA),
	    FE = hipe_bifs:make_fe(DestAddress, element(1,DestMFA), 
				   {Uniq, Index,BEAMAddress}),
	    ?debug_msg("Patch FE(~w) to 0x~s->0x~s (emu:0x~s)\n",
		       [DestMFA, hipe_converters:int_to_hex(FE),
			hipe_converters:int_to_hex(DestAddress),
			hipe_converters:int_to_hex(BEAMAddress)]), 
	    hipe_bifs:write_u32(Address, FE)
    end.
   

callback_patching(Addr, Val, Type) ->
  case Type of
      1 -> 
	  print("Callback 'call' patch, addr:~w, value:~w type:~w\n",[Addr,Val,Type]),
	  Rel32 = Val - Addr - 4,
	  hipe_bifs:write_s32(Addr,Rel32);
      _ ->
	  print("Error: Undefined Callback, FIXME\n"),
	  ?EXIT(undefinedcallback)
  end.

write(Vector,Addr,Length,Length ) -> true;
write(Vector,Addr,Index,Length) ->
  hipe_bifs:write_u8(Addr,hipe_bifs:array_sub(Vector,Index)),
  write(Vector,Addr+1,Index+1,Length).

redirect_old_refs(Refs) ->
  %% print("To patch ~w \n", [Refs]),
  redirect(Refs).

redirect([{MFA,Refs}|Rest]) ->
  redirect(Refs,MFA),
  redirect(Rest);
redirect([]) ->
  true.

redirect([{MFA, Address, Type}| Refs], ToMFA) ->
  NewAddress = get_fun_address(ToMFA),
  {M,F,A} = MFA,
  {M2,F2,A2} = ToMFA,
  print("(RE)LINKING ~w:~w/~w (@ 0x~s) to ~w:~w/~w (@ 0x~s)\n",
	    [M,F,A, hipe_converters:int_to_hex(Address), 
	     M2,F2,A2, hipe_converters:int_to_hex(NewAddress)]),
  callback_patching(Address, NewAddress, Type),
  redirect(Refs, ToMFA);
redirect([],_) ->
  true.

set_native_address(MFA, Address, Closure) ->
  hipe_bifs:set_funinfo({{MFA,native_address}, Address}),
  hipe_bifs:set_native_address(MFA, Address, Closure).

get_native_address(MFA) ->
  case hipe_bifs:get_funinfo({MFA,native_address}) of
    [] -> false;
    [{_,Address}] -> Address
  end.

%Native code stub must be encoded manually since encode module
%is not available at bootloader time...
make_native_stub(Address, Arity) ->
    CodeSize = 15,
    Mem = hipe_bifs:alloc_code(CodeSize),
    Callemu = hipe_bifs:primop_address(callemu) - Mem - CodeSize,
    Code = [% mov eax, Address
	    2#10111000,     %%%2#10100011, 
	    Address band 16#000000ff,
	    (Address bsr 8) band 16#000000ff,
	    (Address bsr 16) band 16#000000ff,
	    (Address bsr 24) band 16#000000ff,
	    % mov edx, Arity
	    2#10111010,    %%%2#10110010,
	    Arity band 16#000000ff,
	    (Arity bsr 8) band 16#000000ff,
	    (Arity bsr 16) band 16#000000ff,
	    (Arity bsr 24) band 16#000000ff,
            % Jmp callemu, rel32 position
	    %%2#11101001,
	    233,
	    Callemu band 16#000000ff,
	    (Callemu bsr 8) band 16#000000ff,
	    (Callemu bsr 16) band 16#000000ff,
	    (Callemu bsr 24) band 16#000000ff
	   ],
    hipe_unified_loader:write_bytes(Code, Mem),
    Mem.

mfa_to_address(MFA, [{Adr,MFA,_}|Rest]) -> Adr;
mfa_to_address(MFA, [_ | Rest]) -> mfa_to_address(MFA, Rest);
mfa_to_address(_  , []) -> false.
  
get_fun_address(MFA, Addresses) ->
  case mfa_to_address(MFA, Addresses) of
    Adr when integer(Adr) -> Adr;
    false -> get_fun_address(MFA)
  end.
      
get_fun_address(DestMFA) ->
    case get_native_address(DestMFA) of
	false -> %% No Native code.
	    print( "MFA:~w not in native code, make native stub\n",[DestMFA]),
	    BEAMAddress = get_emu_address(DestMFA),
	    make_native_stub(BEAMAddress,element(3,DestMFA));      
	FunAddress -> 
	    FunAddress
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
      Address = hipe_bifs:emu_stub(M, F, A);
      %% print("Address ~s\n",[hipe_converters:int_to_hex(Address)]),
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

% Back call to loader, should redirect some old refs
patch_to_emu(Mod) ->
  case is_loaded(Mod) of
    true ->
      MFAs = [{Mod,Fun, Arity} || {Fun, Arity} <-
				    Mod:module_info(exports)],
      make_stubs(MFAs),
      ReferencesToPatch = hipe_unified_loader:get_and_remove_refs(MFAs),
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
  print("(RE)LINKING ~w:~w/~w (@ 0x~s) to EMU ~w:~w/~w (@ 0x~s)\n",
	    [M,F,A, hipe_converters:int_to_hex(Address), 
	     M2,F2,A2, hipe_converters:int_to_hex(StubAddress)]),
  callback_patching(Address, StubAddress, Type),
  redirect_to_emu(Refs, ToMFA);
redirect_to_emu([],_) ->
  true.

print( String ) ->
    %Flags = get(hipe_x86_flags),
    ?IF_DEBUG(io:format(String,[]),no_debug).
print( String, Arglist ) ->
    %Flags = get(hipe_x86_flags),
    ?IF_DEBUG(io:format(String,Arglist),no_debug).

export_funs(Mod, Beam, Addresses, ClosuresToPatch) ->
 Fs = [ {F, A, Address} || {Address , {M, F, A}, _} <- Addresses ],
 code:make_stub_module(Mod, Beam, {Fs,ClosuresToPatch}).

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
