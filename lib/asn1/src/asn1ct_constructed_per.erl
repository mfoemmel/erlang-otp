%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(asn1ct_constructed_per).

-export([gen_encode_constructed_notag/2,gen_encode_choice_notag/2]).
-export([gen_decode_constructed_notag/2,gen_decode_choice_notag/2]).
-export([gen_encode_seqof/3,gen_decode_seqof/3]).
-include("asn1_records.hrl").
%-compile(export_all).

-import(asn1ct_gen, [emit/1,demit/1]).


%% ENCODE GENERATOR FOR SEQUENCE TYPE  ** **********


gen_encode_constructed_notag(Typename,D) when record(D,type) ->
    asn1ct_name:start(),
    asn1ct_name:new(term),
    asn1ct_name:new(bytes),
    {_SeqOrSet,CompList} = D#type.def,
    case Optionals = optionals(CompList) of
	[] ->
	    emit({{var,asn1ct_name:next(val)}," = ?RT_PER:list_to_record(",
		  {asis,Typename},", ",{var,asn1ct_name:curr(val)},"),",nl});
	_ ->
	    emit({"{",{var,asn1ct_name:next(val)},
		  ",Opt} = ?RT_PER:fixoptionals(",
		  {asis,Optionals},",",
		  {var,asn1ct_name:curr(val)},"),",nl})
    end,
    asn1ct_name:new(val),
    Ext = extensible(CompList),
    case Ext of
	{ext,_,NumExt} when NumExt > 0 ->
	    emit(["Extensions = ?RT_PER:fixextensions(",{asis,Ext},
		  ", ",{curr,val},"),",nl]);
	_ -> true
    end,
    emit({"[",nl}),
    MaybeComma1 = case Ext of
	{ext,Pos,NumExt2} when NumExt2 > 0 -> 
	    emit({"?RT_PER:setext(Extensions =/= [])"}),
	    ", ";
	{ext,Pos,_} -> 
	    emit({"?RT_PER:setext(false)"}),
	    ", ";
	_ -> 
	    ""
    end,
    MaybeComma2 = case optionals(CompList) of
		      [] -> MaybeComma1;
		      _ -> 
			  emit(MaybeComma1),
			  emit("?RT_PER:setoptionals(Opt)"),
			  {",",nl}
		  end,
    gen_enc_components_call(Typename,CompList,MaybeComma2,Ext),
    emit({"].",nl}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generate decode function for SEQUENCE and SET
%%
gen_decode_constructed_notag(Typename,D) when record(D,type) ->
    asn1ct_name:start(),
    {_SeqorSet,CompList} = D#type.def,
    Ext = extensible(CompList),
    MaybeComma1 = case Ext of
		      {ext,Pos,NumExt} -> 
			  gen_dec_extension_value("Bytes"),
			  {",",nl};    
		      _ -> 
			  ""
		  end,
    MaybeComma2 = case optionals(CompList) of
		      [] -> MaybeComma1;
		      Optionals -> 
			  Bcurr = asn1ct_name:curr(bytes),
			  Bnext = asn1ct_name:next(bytes),
			  emit(MaybeComma1),
			  emit({"{Opt,",{var,Bnext},"} = ?RT_PER:getoptionals(",
				{var,Bcurr},",",{asis,Optionals},",",
				{asis,length(mkcnamelist(CompList))},")"}),
			  asn1ct_name:new(bytes),
			  ", "    
		  end,
    gen_dec_components_call(Typename,CompList,MaybeComma2,Ext),
    case asn1ct_name:all(term) of
	[] -> emit(MaybeComma2); % no components at all
	_ -> emit({com,nl})
    end,
% we don't return named lists any more   Cnames = mkcnamelist(CompList), 
    demit({"Result = "}), %dbg
    %% return value as record
    emit({"{{",{asis,Typename}}),
    mkvlist(asn1ct_name:all(term)),
    emit({"},",{var,asn1ct_name:curr(bytes)},"}"}),
    emit({".",nl}).


%% ENCODE GENERATOR FOR THE CHOICE TYPE *******
%% assume Val = {Alternative,AltType}
%% generate
%%[
%% ?RT_PER:set_choice(element(1,Val),Altnum,Altlist,ext),
%%case element(1,Val) of
%%    alt1 ->
%%	encode_alt1(element(2,Val));
%%    alt2 ->
%%	encode_alt2(element(2,Val))
%%end
%%].

gen_encode_choice_notag(Typename,D) when record(D,type) ->
    {'CHOICE',CompList} = D#type.def,
    emit({"[",nl}),
    Ext = extensible(CompList),
    gen_enc_choice(Typename,CompList,Ext),
    emit({nl,"].",nl}).

gen_decode_choice_notag(Typename,D) when record(D,type) ->
    asn1ct_name:start(),
    asn1ct_name:new(bytes),
    {'CHOICE',CompList} = D#type.def,
    Ext = extensible(CompList),
    gen_dec_choice(Typename,CompList,Ext),
    emit({".",nl}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Encode generator for SEQUENCE OF type


gen_encode_seqof(Typename,SeqOrSetOf,D) when record(D,type) ->
    asn1ct_name:start(),
% Val = [Component]
% ?RT_PER:encode_length(length(Val)),
% lists:
    {_SeqOrSetOf,ComponentType} = D#type.def,
    emit({"[",nl}),
    Constraint = D#type.constraint,
    SizeConstraint = case lists:keysearch('SizeConstraint',1,Constraint) of
			 {value,{_,Range}} -> Range;
			 false -> undefined
		     end,
    emit({nl,"?RT_PER:encode_length(",
	  {asis,SizeConstraint},
	  ",length(Val)),",nl}),
    emit({"'enc_",Typename,
	      "_components'(Val,[])"}),
    emit({nl,"].",nl}),
    gen_encode_seqof_components(Typename,SeqOrSetOf,ComponentType).

gen_decode_seqof(Typename,SeqOrSetOf,D) when record(D,type) ->
    asn1ct_name:start(),
% Val = [Component]
% ?RT_PER:encode_length(length(Val)),
% lists:
    {_SeqOrSetOf,ComponentType} = D#type.def,
    Constraint = D#type.constraint,
    SizeConstraint = case lists:keysearch('SizeConstraint',1,Constraint) of
			 {value,{_,Range}} -> Range;
			 false -> undefined
		     end,
    emit({nl,"{Num,Bytes1} = ?RT_PER:decode_length(Bytes,",{asis,SizeConstraint},"),",nl}),
    emit({"'dec_",Typename,
	      "_components'(Num,Bytes1,Telltype,[]).",nl}),
    gen_decode_seqof_components(Typename,SeqOrSetOf,ComponentType).

gen_encode_seqof_components(Typename,SeqOrSetOf,Cont) ->
    emit({"'enc_",Typename,"_components'([],Acc) -> lists:reverse(Acc);",nl,nl}),
    emit({"'enc_",Typename,"_components'([H|T],Acc) ->",nl}),
    emit({"'enc_",Typename,"_components'(T,"}), 
    emit({"["}),
    %% the component encoder
    Constructed_Suffix = case SeqOrSetOf of
			     'SET OF' -> "_SETOF";
			     'SEQUENCE OF' -> "_SEQOF"
			 end,
    Conttype = asn1ct_gen:get_inner(Cont#type.def),
    case asn1ct_gen:type(Conttype) of
	{primitive,bif} ->
	    asn1ct_gen_per:gen_encode_prim(per,Cont,false,"H");
	{constructed,bif} ->
	    NewTypename = list_to_atom(lists:concat([Typename,Constructed_Suffix])),
	    emit({"'enc_",NewTypename,"'(H)",nl,nl});
	#typereference{val=Ename} ->
	    emit({"'enc_",Ename,"'(H)",nl,nl});
	#'Externaltypereference'{module=EMod,type=EType} ->
	    emit({"'",EMod,"':'enc_",EType,"'(H)",nl,nl});
	_ ->
	    emit({"'enc_",Conttype,"'(H)",nl,nl})
    end,
    emit({" | Acc]).",nl}).

gen_decode_seqof_components(Typename,SeqOrSetOf,Cont) ->
    emit({"'dec_",Typename,"_components'(0,Bytes,Telltype,Acc) ->",nl,"{lists:reverse(Acc),Bytes};",nl}),
    emit({"'dec_",Typename,"_components'(Num,Bytes,Telltype,Acc) ->",nl}),
    emit({"{Term,Remain} = "}),
    Constructed_Suffix = case SeqOrSetOf of
			     'SET OF' -> "_SETOF";
			     'SEQUENCE OF' -> "_SEQOF"
			 end,
    Conttype = asn1ct_gen:get_inner(Cont#type.def),
    case asn1ct_gen:type(Conttype) of
	{primitive,bif} ->
	    asn1ct_gen_per:gen_dec_prim(per,Cont,"Bytes"),
	    emit({com,nl});
	{constructed,bif} ->
	    NewTypename = list_to_atom(lists:concat([Typename,Constructed_Suffix])),
	    emit({"'dec_",NewTypename,"'(Bytes,Telltype),",nl});
	#typereference{val=Dname} ->
	    emit({"'dec_",Dname,"'(Bytes,Telltype),",nl});
	#'Externaltypereference'{module=EMod,type=EType} ->
	    emit({"'",EMod,"':'dec_",EType,"'(Bytes,Telltype),",nl});
	_ ->
	    emit({"'dec_",Conttype,"'(Bytes,Telltype),",nl})
    end,
    emit({"'dec_",Typename,"_components'(Num-1,Remain,Telltype,[Term|Acc]).",nl}). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% General and special help functions (not exported)

mkvlist([H|T]) ->
    emit(","),
    mkvlist2([H|T]);
mkvlist([]) ->
    true.
mkvlist2([H,T1|T]) ->
    emit({{var,H},","}),
    mkvlist2([T1|T]);
mkvlist2([H|T]) ->
    emit({{var,H}}),
    mkvlist2(T);
mkvlist2([]) ->
    true.

mkcnamelist({L1,L2}) ->
    mkcnamelist(L1,[]); % temporary wrong hack
mkcnamelist(L) ->
    mkcnamelist(L,[]).

mkcnamelist([#'ComponentType'{name=Name}|Rest],Acc) ->
    mkcnamelist(Rest,[Name|Acc]);
mkcnamelist([H|T],Acc) ->
    mkcnamelist(T,Acc);
mkcnamelist([],Acc) ->
    lists:reverse(Acc).

extensible(CompList) when list(CompList) ->
    noext;
extensible({RootList,ExtList}) ->
    {ext,length(RootList)+1,length(ExtList)}.

gen_dec_extension_value(Bytesvar) ->
    emit({"{Ext,",{next,bytes},"} = ?RT_PER:getext(",{curr,bytes},")"}),
    asn1ct_name:new(bytes).

optionals({L,Ext}) -> optionals(L,[],1);
optionals(L) -> optionals(L,[],1).

optionals([{'EXTENSIONMARK',_,_}|Rest],Acc,Pos) ->
    optionals(Rest,Acc,Pos); % optionals in extension are currently not handled
optionals([#'ComponentType'{name=Name,prop='OPTIONAL'}|Rest],Acc,Pos) ->
		 optionals(Rest,[{Name,Pos}|Acc],Pos+1);
optionals([#'ComponentType'{name=Name,prop={'DEFAULT',_}}|Rest],Acc,Pos) ->
		 optionals(Rest,[{Name,Pos}|Acc],Pos+1);
optionals([#'ComponentType'{}|Rest],Acc,Pos) ->
		 optionals(Rest,Acc,Pos+1);
optionals([],Acc,_) ->
    lists:reverse(Acc).



gen_enc_components_call(TopType, {CompList,ExtList}, MaybeComma, Ext) ->
    %% The type has extensionmarker
    Rpos = gen_enc_components_call1(TopType, CompList, 1, MaybeComma, noext),
    case Ext of
	{ext,_,ExtNum} when ExtNum > 0 ->
	    emit([nl,
		  ",Extensions",nl]);
	_ -> true
    end,
    Epos = gen_enc_components_call1(TopType, ExtList, Rpos,MaybeComma,Ext);
gen_enc_components_call(TopType, CompList, MaybeComma, Ext) ->
    %% The type has no extensionmarker
    gen_enc_components_call1(TopType, CompList, 1, MaybeComma, Ext).

gen_enc_components_call1(TopType,
			 [#'ComponentType'{name=Cname,typespec=Type,prop=Prop}|Rest],
			 Tpos,
			 MaybeComma, Ext) ->
    Pos = case Ext of
	      noext -> Tpos;
	      {ext,Epos,Enum} -> Tpos - Epos + 1
	  end,
    emit(MaybeComma),
    case Prop of
	'OPTIONAL' ->
	    gen_enc_component_optional(TopType,Cname,Type,Tpos,  Ext);
	{'DEFAULT',DefVal} ->
	    gen_enc_component_default(TopType,Cname,Type,Tpos, Ext);
	_ ->
	    case Ext of
		{ext,ExtPos,_} when Tpos >= ExtPos ->
		    gen_enc_component_optional(TopType,Cname,Type,Tpos,Ext);
		_ ->
		    gen_enc_component_mandatory(TopType,Cname,Type,Tpos, Ext)
	    end
    end,
    case Rest of
	[] ->
	    Pos+1;
	_ ->
	    emit({com,nl}),
	    gen_enc_components_call1(TopType,Rest,Tpos+1,"", Ext)
    end;
gen_enc_components_call1(_TopType,[],Pos,_,_) ->
	Pos.

gen_enc_component_default(TopType,Cname,Type,Pos,Ext) ->
    Element = io_lib:format("?RT_PER:cindex(~w,Val1,~w)",[Pos+1,Cname]),
    emit({"case ",Element," of",nl}),
%    case Ext of
%	{ext,ExtPos,_} when Pos >= ExtPos ->
%	    emit({"asn1_NOEXTVALUE -> [];",nl});
%	_ ->
	    emit({"asn1_DEFAULT -> [];",nl}),
%    end,
    emit({"_ ->",nl}),
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    emit({nl,"%% attribute number ",Pos," with type ",
	      InnerType,nl}),
    gen_enc_line(TopType,Cname,Type,Element, Pos, Ext),
    emit({nl,"end"}).

gen_enc_component_optional(TopType,Cname,Type,Pos,Ext) ->
    Element = io_lib:format("?RT_PER:cindex(~w,Val1,~w)",[Pos+1,Cname]),
    emit({"case ",Element," of",nl}),
%    case Ext of
%	{ext,ExtPos,_} when Pos >= ExtPos ->
%	    emit({"asn1_NOEXTVALUE -> [];",nl});
%	_ ->
	    emit({"asn1_NOVALUE -> [];",nl}),
%    end,
    emit({"_ ->",nl}),
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    emit({nl,"%% attribute number ",Pos," with type ",
	      InnerType,nl}),
    gen_enc_line(TopType,Cname,Type,Element, Pos, Ext),
    emit({nl,"end"}).

gen_enc_component_mandatory(TopType,Cname,Type,Pos,Ext) ->
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    emit({nl,"%% attribute number ",Pos," with type ",
	      InnerType,nl}),
    gen_enc_line(TopType,Cname,Type,[],Pos,Ext).

gen_enc_line(TopType, Cname, Type, [], Pos, Ext) ->
    Element = io_lib:format("?RT_PER:cindex(~w,~s,~w)",[Pos+1,asn1ct_gen_per:mk_var(asn1ct_name:curr(val)),Cname]),
    gen_enc_line(TopType,Cname,Type,Element, Pos, Ext);
gen_enc_line(TopType,Cname,Type,Element, Pos, Ext) ->
    Atype = asn1ct_gen:get_inner(Type#type.def),
    case Ext of
	{ext,Ep1,_} when  Pos >= Ep1 ->
	    emit(["?RT_PER:encode_open_type(dummy,?RT_PER:complete("]);
	_ -> true
    end,
    case asn1ct_gen:type(Atype) of
	#'Externaltypereference'{module=Mod,type=EType} ->
	    emit({"'",Mod,"':'enc_",
		  EType,"'(",Element,")"});
	#typereference{val=Ename} ->
		    emit({"'enc_",
			  Ename,"'(",Element,")"});
	{notype,_} ->
		    emit({"'enc_",
			  Atype,"'(",Element,")"});
	{primitive,bif} ->
	    asn1ct_gen_per:gen_encode_prim(per,Type,false,Element);
	{constructed,bif} ->
	    NewTypename = lists:concat([TopType,"_",Cname]),
	    emit({"'enc_",
		      NewTypename,"'(",Element,")"})
    end,
    case Ext of 
	{ext,Ep2,_} when Pos >= Ep2 ->
	    emit(["))"]);
	_ -> true
    end.

gen_dec_components_call(TopType,{CompList,ExtList},MaybeComma,Ext) ->
    %% The type has extensionmarker
    Rpos = gen_dec_components_call1(TopType, CompList, 1, MaybeComma, noext),
    emit([",",nl,"{Extensions,",{next,bytes},"} = "]),
    emit(["?RT_PER:getextension(Ext,",{curr,bytes},"),",nl]),
    asn1ct_name:new(bytes),
    Epos = gen_dec_components_call1(TopType,ExtList,Rpos,"",Ext),
    case ExtList of
	[] -> true;
	_ -> emit([",",nl])
    end,
    emit([{next,bytes},"= ?RT_PER:skipextensions(",{curr,bytes},",",
	  length(ExtList)+1,",Extensions)",nl]),
    asn1ct_name:new(bytes);

gen_dec_components_call(TopType,CompList,MaybeComma,Ext) ->
    %% The type has no extensionmarker
    gen_dec_components_call1(TopType, CompList, 1, MaybeComma,Ext).


gen_dec_components_call1(TopType,
			 [#'ComponentType'{name=Cname,typespec=Type,prop=Prop}|Rest],
			 Tpos, MaybeComma,Ext) ->
    Pos = case Ext of
	      noext -> Tpos;
	      {ext,Epos,Enum} -> Tpos - Epos + 1
	  end,
    emit(MaybeComma),
    asn1ct_name:new(term),
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    emit({nl,"%% attribute number ",Tpos," with type ",
	      InnerType,nl}),
    emit({"{",{curr,term},",",{next,bytes},"} = "}),
    case {Ext,Prop} of
	{noext,mandatory} -> true; % generate nothing
	{noext,_} ->
	    Element = io_lib:format("element(~w,Opt)",[Pos]),
	    emit({"case ",Element," of",nl}),
	    emit({"1 ->"});
	_ ->
	    emit(["case Extensions of",nl]),
	    emit(["_ when size(Extensions) >= ",Pos,",element(",Pos,",Extensions) == 1 ->",nl])
    end,

    gen_dec_line(TopType,Cname,Type,Tpos,Ext),
    case {Ext,Prop} of
	{noext,mandatory} -> true; % generate nothing
	{noext,_} ->
	    emit([";",nl,"0 ->"]),
	    gen_dec_component_no_val(TopType,Cname,Type,Prop,Tpos,Ext);
	_ ->
	    emit([";",nl,"_  ->",nl]),
%%	    emit([";",nl,"_ when element(",Pos,",Extensions) == 0 ->",nl]),
	    gen_dec_component_no_val(TopType,Cname,Type,Prop,Tpos,Ext)
    end,    
    case {Ext,Prop} of
	{noext,mandatory} -> true; % generate nothing
	{noext,_} ->
	    emit([nl,"end"]);
	_ ->
	    emit([nl,"end"])
	    
%%	    emit([";",nl,"_ -> % unknown extension",nl]),
%%	    emit(["{asn1_NOEXTVALUE,",{curr,bytes},"}",nl,"end"])
    end,
    asn1ct_name:new(bytes),
    case Rest of
	[] ->
	    Pos+1;
	_ ->
	    emit({com,nl}),
	    gen_dec_components_call1(TopType,Rest,Tpos+1,"",Ext)
    end;

gen_dec_components_call1(_TopType,[],Pos,_,_) ->
	Pos.


gen_dec_component_no_val(TopType,Cname,Type,_,Pos,{ext,Ep,Enum}) when Pos >= Ep ->
    emit({"{asn1_NOEXTVALUE,",{curr,bytes},"}",nl});
gen_dec_component_no_val(TopType,Cname,Type,{'DEFAULT',DefVal},Pos,_) ->
    emit(["{",{asis,DefVal},",",{curr,bytes},"}",nl]);
gen_dec_component_no_val(TopType,Cname,Type,'OPTIONAL',Pos,_) ->
    emit({"{asn1_NOVALUE,",{curr,bytes},"}",nl});
gen_dec_component_no_val(TopType,Cname,Type,mandatory,Pos,{ext,Ep,Enum}) ->
    emit({"{asn1_NOEXTVALUE,",{curr,bytes},"}",nl}).
    

gen_dec_line(TopType,Cname,Type,Pos,Ext)  ->
    Atype = asn1ct_gen:get_inner(Type#type.def),
    BytesVar0 = asn1ct_gen_per:mk_var(asn1ct_name:curr(bytes)),
    BytesVar = case Ext of
		   {ext,Ep,_} when Pos >= Ep ->
		       emit(["begin",nl,"{TmpVal",Pos,",Trem",Pos,
			     "}=?RT_PER:decode_open_type(",
			     {curr,bytes},",[]),",nl,
			    "{TmpValx",Pos,",_}="]),
		       io_lib:format("TmpVal~p",[Pos]);
		   _ -> BytesVar0
	       end,
	    
    case asn1ct_gen:type(Atype) of
	#'Externaltypereference'{module=Mod,type=EType} ->
	    emit({"'",Mod,"':'dec_",EType,"'(",BytesVar,
		  ",telltype)"});
	{primitive,bif} ->
	    asn1ct_gen_per:gen_dec_prim(per,Type,BytesVar);
	#typereference{val=Dname} ->
	    emit({"'dec_",Dname,"'(",
		  BytesVar,
		  ",telltype)"});
	{notype,_} ->
	    emit({"'dec_",Atype,"'(",
		  BytesVar,
		  ",telltype)"});
	{constructed,bif} ->
	    NewTypename = lists:concat([TopType,"_",Cname]),
	    emit({"'dec_",
			  NewTypename,"'(",BytesVar,
			  ",Telltype)"})
    end,
    case Ext of 
	{ext,Ep2,_} when Pos >= Ep2 ->
	    emit([", {TmpValx",Pos,",Trem",Pos,"}",nl,"end"]);
	_ -> true
    end.

gen_enc_choice(TopType,CompList,Ext) ->
    gen_enc_choice_tag(CompList, [], Ext),
    emit({com,nl}),
    emit({"case element(1,Val) of",nl}),
    gen_enc_choice2(TopType, CompList, Ext),
    emit({nl,"end"}).

gen_enc_choice_tag({C1,C2}, Acc, Ext) ->
    N1 = get_name_list(C1),
    N2 = get_name_list(C2),
    emit(["?RT_PER:set_choice(element(1,Val),",
	  {asis,{N1,N2}},", ",{asis,{length(N1),length(N2)}},")"]);
gen_enc_choice_tag(C, Acc, Ext) ->
    N = get_name_list(C),
    emit(["?RT_PER:set_choice(element(1,Val),",
	  {asis,N},", ",{asis,length(N)},")"]).

get_name_list(L) ->
    get_name_list(L,[]).

get_name_list([#'ComponentType'{name=Name}|T], Acc) ->
    get_name_list(T,[Name|Acc]);
get_name_list([], Acc) ->
    lists:reverse(Acc).

%gen_enc_choice_tag([H|T],Acc,Ext) when record(H,'ComponentType') ->
%    gen_enc_choice_tag(T,[H#'ComponentType'.name|Acc],Ext);
%gen_enc_choice_tag([H|T],Acc,Ext) -> % skip EXTENSIONMARK
%    gen_enc_choice_tag(T,Acc,Ext);
%gen_enc_choice_tag([],Acc,Ext) ->
%    Length = length(Acc),
%    emit({"?RT_PER:set_choice(element(1,Val),",{asis,Length},",",
%	  {asis,lists:reverse(Acc)},",",{asis,Ext},")"}),
%    Length.

gen_enc_choice2(TopType, {L1,L2}, Ext) ->
    gen_enc_choice2(TopType, L1 ++ L2, 0, Ext);
gen_enc_choice2(TopType, L, Ext) ->
    gen_enc_choice2(TopType, L, 0, Ext).

gen_enc_choice2(TopType,[H1,H2|T], Pos, Ext) 
when record(H1,'ComponentType'), record(H2,'ComponentType') ->
    Cname = H1#'ComponentType'.name,
    Type = H1#'ComponentType'.typespec,
    emit({{asis,Cname}," ->",nl}),
    gen_enc_line(TopType,Cname,Type,"element(2,Val)", Pos+1, Ext),
    emit({";",nl}),
    gen_enc_choice2(TopType,[H2|T], Pos+1, Ext);
gen_enc_choice2(TopType,[H1|T], Pos, Ext) when record(H1,'ComponentType') ->
    Cname = H1#'ComponentType'.name,
    Type = H1#'ComponentType'.typespec,
    emit({{asis,H1#'ComponentType'.name}," ->",nl}),
    gen_enc_line(TopType,Cname,Type,"element(2,Val)", Pos+1, Ext),
    gen_enc_choice2(TopType,T, Pos+1, Ext);
gen_enc_choice2(TopType,[], _, _)  ->
    true.

gen_dec_choice(TopType,CompList,{ext,Pos,NumExt}) ->
    emit({"{Ext,",{curr,bytes},"} = ?RT_PER:getbit(Bytes),",nl}),
    asn1ct_name:new(bytes),
    gen_dec_choice1(TopType,CompList,{ext,Pos,NumExt});
gen_dec_choice(TopType,CompList,noext) ->
    gen_dec_choice1(TopType,CompList,noext).

gen_dec_choice1(TopType,CompList,noext) ->
    emit({"{Choice,",{curr,bytes},
	  "} = ?RT_PER:getchoice(",{prev,bytes},",",
	  length(CompList),", 0),",nl}),
    emit({"{Cname,{Val,NewBytes}} = case Choice of",nl}),
    gen_dec_choice2(TopType,CompList,noext),
    emit({nl,"end,",nl}),
    emit({nl,"{{Cname,Val},NewBytes}"});
gen_dec_choice1(TopType,{RootList,ExtList},Ext) ->
    NewList = RootList ++ ExtList,
    gen_dec_choice1(TopType, NewList, Ext);
gen_dec_choice1(TopType,CompList,{ext,ExtPos,ExtNum}) ->
    emit({"{Choice,",{curr,bytes},
	  "} = ?RT_PER:getchoice(",{prev,bytes},",",
	  length(CompList)-ExtNum,",Ext ),",nl}),
    emit({"{Cname,{Val,NewBytes}} = case Choice + Ext*",ExtPos-1," of",nl}),
    Cpos = gen_dec_choice2(TopType,CompList,{ext,ExtPos,ExtNum}),
    emit([";",nl,"ExtNum -> {asn1_ExtAlt, ?RT_PER:decode_open_type(",{curr,bytes},",[])}"]),
    emit({nl,"end,",nl}),
    emit({nl,"{{Cname,Val},NewBytes}"}).


gen_dec_choice2(TopType,L,Ext) ->
    gen_dec_choice2(TopType,L,0,Ext).

gen_dec_choice2(TopType,[H1,H2|T],Pos,Ext) 
when record(H1,'ComponentType'), record(H2,'ComponentType') ->
    Cname = H1#'ComponentType'.name,
    Type = H1#'ComponentType'.typespec,
    emit({Pos," -> {",{asis,Cname},",",nl}),
    gen_dec_line(TopType,Cname,Type,Pos+1,Ext),
    emit({"};",nl}),
    gen_dec_choice2(TopType,[H2|T],Pos+1,Ext);
gen_dec_choice2(TopType,[H1,H2|T],Pos,Ext) when record(H1,'ComponentType') ->
    gen_dec_choice2(TopType,[H1|T],Pos,Ext); % skip extensionmark
gen_dec_choice2(TopType,[H1|T],Pos,Ext) when record(H1,'ComponentType') ->
    Cname = H1#'ComponentType'.name,
    Type = H1#'ComponentType'.typespec,
    emit({Pos," -> {",{asis,Cname},",",nl}),
    gen_dec_line(TopType,Cname,Type,Pos+1,Ext),
    emit("}"),
    gen_dec_choice2(TopType,[T],Pos+1);
gen_dec_choice2(TopType,[H1|T],Pos,Ext) ->
    gen_dec_choice2(TopType,T,Pos,Ext);% skip extensionmark
gen_dec_choice2(TopType,[],Pos,Ext)  ->
    Pos.
