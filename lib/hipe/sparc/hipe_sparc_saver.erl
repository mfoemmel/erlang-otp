%% -*- erlang-indent-level: 2 -*-
%% Copyright (c) 2000 by Erik Johansson.  
%% ====================================================================
%%  Filename : 	sparc_saver.erl
%%  Module   :	sparc_saver
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2000-02-22 Erik Johansson (happi@csd.uu.se): Created.
%% CVS: $Id$
%% ====================================================================
%% Exported functions (short description):
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The external format for SPARC code is a binary representation of an
%%  Erlang list of the form:
%%  [Version:version,
%%   ConstSize:size, ConstMap:constmap, LabelMap:labelmap,
%%   ExportMap:exportmap,
%%   HotSize:size,   HotCode:code,      HotRefs:refs,
%%   ColdSize:size,  ColdCode:code,     ColdRefs:refs
%%  ]
%%  Where
%%   version:   {{Major:integer, Minor:integer, Increment:integer},
%%                    SYSTEM-CHECKSUM}
%%   size:      integer
%%   constmap:  [ConstNo:integer, Offset:integer, Need:integer,
%%               Type:consttype, Exported:bool, 
%%               Data:term | constmap]
%%   labelmap:  [{DataOffset:integer, CodeOffset:integer}|
%%               labelmap]
%%   exportmap: [Offset: integer, Module:atom, Function:atom,
%%               Arity:integer | exportmap] * sorted on Offset *
%%   code:      [B4:byte, B3:byte, B2:byte: B1:byte | code] 
%%   refs:      [{RefType:integer, Reflist:reflist} | refs]
%%
%%   reflist:   [{Data:term, Offsets:offests}|reflist]
%%   offsets:   [Offset:integer | offsets]
%%
%%   constype:  0 | 1  (0 -> term (arbitrary erlang term), 
%%                      1 -> block (a list of bytes)
%%   bool:      0 | 1  (false , true)
%%   mode:      hot | cold

-module(hipe_sparc_saver).
-export([save/3, mk_external/6]).

%-ifndef(DEBUG).
%-define(DEBUG,true).
%-endif.
-include("../main/hipe.hrl").
-include("../rtl/hipe_literals.hrl").

%%
%%
%%
 
save(CompiledCode, Closures, _Flags) ->
    {ConstSize,ConstMap, RefsFromConsts} =
	hipe_sparc_linker:pack_constants(CompiledCode),
    %%  io:format("Const Size ~w\n",[ConstSize]),
    {HotSize,ColdSize,Map,Code} = get_code(CompiledCode),
    {_HAddr,_CAddr,AccHCode,AccCCode,AccHRefs,AccCRefs} = 
	hipe_sparc_linker:assemble(Code,Map,ConstMap),
    AsmCode = {{HotSize,AccHCode,AccHRefs},
	       {ColdSize,AccCCode,AccCRefs}},
    mk_external(ConstSize, ConstMap, RefsFromConsts, Map, AsmCode, Closures).

mk_external(ConstSize, ConstMap, RefsFromConsts, ExportMap, AsmCode, Closures) ->
  {{HotSize,AccHCode,AccHRefs},
   {ColdSize,AccCCode,AccCRefs}} = AsmCode,
  
  term_to_binary([{?version(),?HIPE_SYSTEM_CRC},
		  ConstSize,
		  slim_constmap(ConstMap),
		  mk_labelmap(RefsFromConsts, ExportMap, HotSize),
		  slim_exportmap(ExportMap, HotSize, Closures),
		  HotSize,words32towords8(AccHCode),
		  hipe_pack_constants:slim_refs(AccHRefs),
		  ColdSize,words32towords8(AccCCode),
		  hipe_pack_constants:slim_refs(AccCRefs)]).



words32towords8(List) ->
  lists:foldr( fun word32towords8/2, [], List).

word32towords8(X1,Acc) ->
  X2=(X1 bsr 8),
  X3=(X2 bsr 8), 
  X4 = X3 bsr 8,
  [X4, (X3 band 16#ff), X2 band 16#ff, X1 band 16#ff | Acc].


get_code(Code) ->
  get_code(Code,0,0,[],[]).

get_code([{MFA,Code,_}|Rest],HotAddress,ColdAddress,Map,AccCode) ->
  {NewHot,NewCold,HotSize,ColdSize,NewMap} =
    hipe_sparc_linker:preprocess(Code,MFA,HotAddress,ColdAddress,Map),
  get_code(Rest,HotSize,ColdSize,NewMap,[{MFA,NewHot,NewCold}|AccCode]);
get_code([],HotSize,ColdSize,Map,Acc) ->
  {HotSize,ColdSize,Map,lists:reverse(Acc)}.


slim_constmap(Map) ->
  slim_constmap(Map,[]).
slim_constmap([{_MFA, _Label, ConstNo,
		Offset, Need, Type, Term, Export}|Rest],Acc) ->
  slim_constmap(Rest, [ConstNo, Offset, Need, Type, Export, Term| Acc]);
slim_constmap([],Acc) -> Acc.

mk_labelmap(Map, ExportMap, HotSize) ->
  %% msg("Map: ~w Map\n",[Map]),
  LblMap = lists:flatten(mk_labelmap(Map, ExportMap, [] , 
				     HotSize)),
  %% msg("LblMap: ~w Map\n",[LblMap]),
  LblMap.


mk_labelmap([{MFA, Labels}| Rest], ExportMap, Acc, HotSize) ->
  Map = 
    lists:map(
      fun 
	({L,Pos}) ->
	  {Pos,find_offset({MFA,L}, ExportMap, HotSize)};
	({sorted,Base,OrderedLabels}) ->
	  {sorted, Base, [{Order, find_offset({MFA,L}, ExportMap, HotSize)}
			  || {L,Order} <- OrderedLabels]
	  }
      end,
      Labels),
  %% msg("Map: ~w Map\n",[Map]),
  mk_labelmap(Rest, ExportMap, [Map,Acc], HotSize);


mk_labelmap([],_,Acc,_) -> Acc.

find_offset({MFA,L},[{{MFA,L},Mode,Adr}|_Rest], HotSize) ->
  case Mode of
    hot -> Adr;
    cold -> Adr + HotSize
  end;
find_offset(L,[_|Rest], HotSize) ->
  find_offset(L,Rest,HotSize);
find_offset(L,[],_) ->
  ?EXIT({label_not_found,L}).


slim_exportmap(Map, HotSize, Closures) ->
  SortedMap = lists:sort(slim_exportmap1(Map, HotSize, [])),
  slim_sorted_exportmap(SortedMap, Closures).

slim_exportmap1([{{{M,F,A},entry},Mode,Adr}|Rest], HotSize, Acc) ->
  NewAdr =
    case Mode of
      hot -> Adr;
      cold -> Adr + HotSize
    end,
  slim_exportmap1(Rest, HotSize, [{NewAdr,M,F,A}|Acc]);
slim_exportmap1([_|Rest], HotSize, Acc) ->
  slim_exportmap1(Rest, HotSize, Acc);
slim_exportmap1([], _HotSize, Acc) -> Acc.

slim_sorted_exportmap([{Adr,M,F,A}|Rest], Closures) ->
  IsClosure = lists:member({M,F,A}, Closures),
  [Adr, M,F,A, IsClosure | slim_sorted_exportmap(Rest, Closures)];
slim_sorted_exportmap([],_) -> [].


