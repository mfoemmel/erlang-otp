%%% $Id$
%%% -*- erlang-indent-level: 2 -*-
%%% hipe_unified_loader.erl
%%% This file contains the parts which are identical to all availale loaders
%%% Currently: x86,sparc

-module(hipe_unified_loader).
-export([version_check/2,
	 exports/2,
	 bif_address/1,
	 alloc_constants/2,
	 find_const/2,
	 patch_consts/3,
	 bytes_to_32/4,
	 add_ref/4,
	 get_and_remove_refs/1,
	 write_words/2,
	 write_bytes/2]).
-include("../../hipe/main/hipe.hrl").
-include("hipe_ext_format.hrl").

version_check(Version, Mod) ->
  Ver = ?version(),
  case Version < Ver of
    true -> 
      ?msg("WARNING: Module (Mod) has version ~w\n",
	  [Mod, Version]);
    _ -> true
  end.

exports(ExportMap, BaseAddress) ->
  exports(ExportMap, BaseAddress, [], []).

exports([Offset, M,F,A, Closure |Rest], BaseAddress, MFAs, Addresses) ->
  MFA = {M,F,A},
  Address = BaseAddress + Offset,
  exports(Rest, BaseAddress, [MFA | MFAs], [{Address, MFA, Closure} |
					  Addresses]);
exports([], _, MFAs, Addresses) ->
  {MFAs, Addresses}.

bif_address({M,F,A}) ->
  case catch hipe_bifs:bif_address(M,F,A) of
    Address when integer(Address) ->  
      Address;
    {'EXIT',_} ->
      ?error_msg("no internal function ~w:~w/~w\n",[M,F,A]),
      exit({no_internal_function,{M,F,A}})
  end;
bif_address(Name) ->
  case catch hipe_bifs:primop_address(Name) of
    Address when integer(Address) ->  
      Address;
    {'EXIT',_} ->
      ?error_msg("no internal function ~w\n",[Name]),
      exit({no_internal_function,Name})
  end.

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
	    write_words(Lbls,Address),
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

find_const(ConstNo, [{ConstNo,{Addr,_}}|ConstMap]) ->
  Addr;
find_const(ConstNo, [_|ConstMap]) ->
  find_const(ConstNo, ConstMap);
find_const(ConstNo, []) ->
  ?error_msg("Constant not found ~w\n",[ConstNo]),
  exit({constant_not_found,ConstNo}).

patch_consts(Labels, DataAddress, CodeAddress) ->
  %% ?msg("Labels: ~w\n",[Labels]),
  PatchLabelOrLabels =
    fun 
      ({Pos,Offset}) -> 
	hipe_bifs:write_u32(DataAddress+Pos,
			    CodeAddress+Offset);
      ({sorted,Base,UnOrderdList}) ->
	sort_and_write(UnOrderdList, Base, 
		       DataAddress, CodeAddress)
    end,
  lists:map(PatchLabelOrLabels, Labels).

bytes_to_32(B4,B3,B2,B1) ->
  (B4 bsl 24) bor (B3 bsl 16) bor
    (B2 bsl 8) bor B1.

sort_and_write(UnOrderdList, Base, DataAddress, CodeAddress) ->
  WriteAndInc = 
    fun ({_, Offset}, Pos) -> 
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
    case hipe_bifs:get_funinfo({CallerMFA, reffers_to}) of
      [{_,OldRefs}] ->
	OldRefs;
      [] ->
	[]
    end,
  hipe_bifs:set_funinfo({{CallerMFA, reffers_to}, [CalleeMFA| Refs]}),

  CalleeRefs =
    case hipe_bifs:get_funinfo({CalleeMFA, reffered_from}) of
      [{_,OldRs}] ->
	OldRs;
      [] ->
	[]
    end,
  hipe_bifs:set_funinfo({{CalleeMFA, reffered_from}, 
		    [{CallerMFA, Address, RefType}| 
		     CalleeRefs]}).

get_and_remove_refs(MFAs) ->
  %% io:format("MFAs to link ~w\n",[MFAs]),
  RefferedFrom = get_refs_from(MFAs, []),
  remove_refs_to(MFAs),
  RefferedFrom.

remove_refs_to([MFA|MFAs]) ->
  %% This (new version of MFA) do not reffer 
  %% to any other functions (yet).
  %% Remove all reffered_from from all functions that 
  %% MFA reffers_to.
  case hipe_bifs:get_funinfo({MFA, reffers_to}) of
    [] -> true;
    [{{MFA, reffers_to}, RefsTo}] ->
      remove_refs(RefsTo, MFA)
  end,
  %% Indicate that MFA reffers_to no other function.
  hipe_bifs:set_funinfo({{MFA, reffers_to}, []}),
  remove_refs_to(MFAs);
remove_refs_to([]) ->
  true.

get_refs_from([MFA|MFAs], Acc) ->
  %% Get all MFAs that reffer_to MFA, these need to be patched.
  %% io:format("Refs from ~w\n",[MFA]),
  case hipe_bifs:get_funinfo({MFA, reffered_from}) of
    [{{_,_}, Refs}] ->
       %% io:format("~w is reffered from ~w\n",[MFA, Refs]),
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
    case hipe_bifs:get_funinfo({CalleeMFA, reffered_from}) of
      [{{_,_}, Rs}] -> Rs;
      [] -> []
    end,
  NewRefs = remove_ref(Refs, CallerMFA, []), 
  hipe_bifs:set_funinfo({{CalleeMFA, reffered_from}, NewRefs}).


remove_ref([First | Rest], CallerMFA, Acc) ->
  if element(1,First) =:= CallerMFA ->
      remove_ref(Rest, CallerMFA, Acc);
     true -> 
      remove_ref(Rest, CallerMFA, [First|Acc])
  end;
remove_ref([], CallerMFA, Acc) -> Acc.

write_words([W|Rest],Addr) ->
  hipe_bifs:write_u32(Addr, W),
  write_words(Rest,Addr+4);
write_words([],_) -> true.
write_bytes([B|Rest],Addr) ->
  hipe_bifs:write_u8(Addr, B),
  write_bytes(Rest,Addr+1);
write_bytes([],_) -> true.

group([],[]) ->
  [];
group([B1,B2,B3,B4|Ls],[O|Os]) -> 
  [{hipe_bifs:term_to_word(O),hipe_unified_loader:bytes_to_32(B4,B3,B2,B1)}|group(Ls,Os)].
