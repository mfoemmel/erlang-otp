%%% $Id: hipe_pack_constants.erl,v 1.5 2004/08/21 16:39:00 kostis Exp $

-module(hipe_pack_constants).
-export([pack_constants/2,slim_refs/1,slim_constmap/1]).
-include("../../kernel/src/hipe_ext_format.hrl").

pack_constants(Data, Align) ->
  pack_constants(Data,0,Align,0,[],[]).

pack_constants([{MFA,_,ConstTab}|Rest],Size,Align,ConstNo,Acc,Refs) ->
  Labels = hipe_consttab:labels(ConstTab),
  %% RefToLabels = hipe_consttab:referred_labels(ConstTab),
  {NewSize, NewAlign, Map, NewConstNo, RefToLabels} =
    pack_labels(Labels,MFA,ConstTab,Size,Align,ConstNo,Acc,[]),
  NewRefs =
    case  RefToLabels of 
      [] -> Refs;
      _ -> [{MFA,RefToLabels}| Refs]
    end,
  pack_constants(Rest, NewSize, NewAlign, NewConstNo, Map, NewRefs);
pack_constants([], Size, Align, _, Acc, Refs) -> {Align, Size, Acc, Refs}.

%%%
%%% pack_labels converts a ConstTab to a packed ConstMap, which
%%% maps {MFA,Label} pairs to information about individual constants,
%%% including their ConstNo and start offset in the constants pool.
%%%
pack_labels([{_Label,ref}|Labels],MFA,ConstTab,Size,Align,ConstNo,Acc,Refs) ->
  pack_labels(Labels,MFA,ConstTab,Size,Align,ConstNo,Acc,Refs);
pack_labels([Label|Labels],MFA,ConstTab,AccSize,OldAlign,ConstNo, Acc, Refs) ->
  Const = hipe_consttab:lookup(Label,ConstTab),

  Align = hipe_consttab:const_align(Const),
  NewAlign =
    if Align > OldAlign -> Align;
       true -> OldAlign
    end,
  Start = 
    case AccSize rem Align of
      0 -> AccSize;

      N -> AccSize + (Align -N)
    end,
  %% io:format("Const ~w\n",[Const]),
  RawType = hipe_consttab:const_type(Const),
  Type = ?CONST_TYPE2EXT(RawType),
  RawData = hipe_consttab:const_data(Const),

  case RawType of
    term ->
      %% If the constant term is already in the constant map we want
      %% to use the same constant number so that, in the end, the
      %% constant term is not duplicated.
      case lists:keysearch(RawData, 7, Acc) of
	false ->
	  NewInfo = {pcm_entry,MFA,Label,ConstNo,0,Type,RawData},
	  pack_labels(Labels,MFA,ConstTab,AccSize,OldAlign,ConstNo+1,
		      [NewInfo|Acc],Refs);
	{value, {pcm_entry, _OtherMFA, _OtherLabel, OtherConstNo, _OtherStart,
		 Type, RawData}} ->
	  NewInfo = {pcm_entry,MFA,Label,OtherConstNo,0,
		     Type,RawData},
	  pack_labels(Labels,MFA,ConstTab,AccSize,OldAlign,ConstNo,
		      [NewInfo|Acc],Refs);
	{value, _ } ->
	  NewInfo = {pcm_entry,MFA,Label,ConstNo,0,Type,RawData},
	  pack_labels(Labels,MFA,ConstTab,AccSize,OldAlign,ConstNo+1,
		      [NewInfo|Acc],Refs)
      end;
    sorted_block -> 
      Need = hipe_consttab:const_size(Const),
      NewInfo = {pcm_entry,MFA,Label,ConstNo,Start,Type,RawData},
      pack_labels(Labels,MFA,ConstTab,Start+Need,NewAlign,ConstNo+1,
		  [NewInfo|Acc],Refs);
    block ->
      Need = hipe_consttab:const_size(Const),
      {Data, NewRefs} =
	case RawData of
	  {ElementType, ElementData} ->
	    decompose_block(ElementType, ElementData, Start);
	  {ElementType, ElementData, SortOrder} ->
	    {TblData, TblRefs} = get_sorted_refs(ElementData, SortOrder),
	    {hipe_consttab:decompose({ElementType, TblData}), 
	     [{sorted,Start,TblRefs}]}
	end,
      NewInfo = {pcm_entry,MFA,Label,ConstNo,Start,Type,Data},
      pack_labels(Labels,MFA,ConstTab,Start+Need,NewAlign,ConstNo+1,
		  [NewInfo|Acc],NewRefs++Refs)
  end;
pack_labels([],_,_,Size,Align,ConstNo,Acc,Refs) ->
  {Size,Align,Acc,ConstNo,Refs}.

decompose_block(ElementType, Data, Addr) ->
  ElementSize = hipe_consttab:size_of(ElementType),
  {NewData, Refs} = get_refs(Data,Addr,ElementSize),
  {hipe_consttab:decompose({ElementType, NewData}), Refs}.

get_refs([{label,L}|Rest],Pos,ElementSize) ->
  {NewData, Refs} = get_refs(Rest, Pos+ElementSize, ElementSize),
  {[0|NewData],[{L,Pos}|Refs]};
get_refs([D|Rest],Pos, ElementSize) ->
  {NewData, Refs} = get_refs(Rest, Pos+ElementSize, ElementSize),
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



slim_refs([]) -> [];
slim_refs(Refs) ->
  [Ref|Rest] = lists:keysort(1,Refs),
  compact_ref_types(Rest,element(1,Ref),[Ref],[]).

compact_ref_types([Ref|Refs],Type,AccofType,Acc) ->
  case element(1,Ref) of
    Type ->
      compact_ref_types(Refs,Type,[Ref|AccofType], Acc);
    NewType ->
      compact_ref_types(Refs, NewType,[Ref], 
			[{Type,lists:sort(compact_dests(AccofType))}|
			 Acc])
  end;
compact_ref_types([],Type,AccofType,Acc) ->
  [{Type,lists:sort(compact_dests(AccofType))}|Acc].


%% compact_dests([]) -> [];	% clause is redundant
compact_dests(Refs) ->
  [Ref|Rest] = lists:keysort(3,Refs),
  compact_dests(Rest,element(3,Ref),[element(2,Ref)],[]).


compact_dests([Ref|Refs],Dest,AccofDest,Acc) ->
  case element(3,Ref) of
    Dest ->
      compact_dests(Refs,Dest,[element(2,Ref)|AccofDest], Acc);
    NewDest ->
      compact_dests(Refs, NewDest,[element(2,Ref)], [{Dest,AccofDest}|Acc])
  end;
compact_dests([],Dest,AccofDest,Acc) ->
  [{Dest,AccofDest}|Acc].

%%%
%%% slim_constmap/1 takes a packed ConstMap, as produced by
%%% pack_labels called from hipe_pack_constants:pack_constants/2,
%%% and converts it to the slimmed and flattened format ConstMap
%%% which is put in object files.
%%%
slim_constmap(Map) ->
  slim_constmap(Map,gb_sets:new(),[]).
slim_constmap([{pcm_entry, _MFA, _Label, ConstNo,
		Offset, Type, Term}|Rest],Inserted,Acc) ->
  case gb_sets:is_member(ConstNo, Inserted) of
    true ->
      slim_constmap(Rest, Inserted, Acc);
    false ->
      NewInserted = gb_sets:insert(ConstNo, Inserted),
      slim_constmap(Rest, NewInserted,
		    [ConstNo, Offset, Type, Term | Acc])
  end;
slim_constmap([],_Inserted,Acc) -> Acc.
