% ``The contents of this file are subject to the Erlang Public License,
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

-export([gen_encode_sequence/3]).
-export([gen_decode_sequence/3]).
-export([gen_encode_set/3]).
-export([gen_decode_set/3]).
-export([gen_encode_sof/4]).
-export([gen_decode_sof/4]).
-export([gen_encode_choice/3]).
-export([gen_decode_choice/3]).

-include("asn1_records.hrl").
%-compile(export_all).

-import(asn1ct_gen, [emit/1,demit/1]).


%% ENCODE GENERATOR FOR SEQUENCE TYPE  ** **********


gen_encode_set(Erules,TypeName,D) ->
    gen_encode_constructed(Erules,TypeName,D).

gen_encode_sequence(Erules,TypeName,D) ->
    gen_encode_constructed(Erules,TypeName,D).

gen_encode_constructed(Erules,Typename,D) when record(D,type) ->
    asn1ct_name:start(),
    asn1ct_name:new(term),
    asn1ct_name:new(bytes),
    {CompList,TableConsInfo} = 
	case D#type.def of
	    #'SEQUENCE'{tablecinf=TCI,components=CL} ->
		{CL,TCI};
	    #'SET'{tablecinf=TCI,components=CL} ->
		{CL,TCI}
	end,
    case Optionals = optionals(CompList) of
	[] ->
	    emit([{var,asn1ct_name:next(val)}," = ?RT_PER:list_to_record("]),
	    emit(["'",asn1ct_gen:list2rname(Typename),"'"]),
	    emit([", ",{var,asn1ct_name:curr(val)},"),",nl]);
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
    EncObj =
	case TableConsInfo of
	    {ObjectSet,AttrN,N,UniqueFieldName} -> %% N is index of attribute that determines constraint
		ObjectEncode = lists:concat(['Obj',AttrN]),
		emit({ObjectEncode," = ",nl}),
		emit({"   'getenc_",ObjectSet,"'(",{asis,UniqueFieldName},", ",nl}),
		emit({indent(35),"?RT_PER:cindex(",N+1,", ",
		      {var,asn1ct_name:curr(val)},", ",{asis,AttrN},")),",nl}),
		{AttrN,ObjectEncode};
	    _ ->
		false
	end,
    emit({"[",nl}),
    MaybeComma1 = 
	case Ext of
	    {ext,Pos,NumExt2} when NumExt2 > 0 -> 
		emit({"?RT_PER:setext(Extensions =/= [])"}),
		", ";
	    {ext,Pos,_} -> 
		emit({"?RT_PER:setext(false)"}),
		", ";
	    _ -> 
		""
	end,
    MaybeComma2 = 
	case optionals(CompList) of
	    [] -> MaybeComma1;
	    _ -> 
		emit(MaybeComma1),
		emit("?RT_PER:setoptionals(Opt)"),
		{",",nl}
	end,
    gen_enc_components_call(Typename,CompList,MaybeComma2,EncObj,Ext),
    emit({"].",nl}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generate decode function for SEQUENCE and SET
%%
gen_decode_set(Erules,Typename,D) ->
    gen_decode_constructed(Erules,Typename,D).

gen_decode_sequence(Erules,Typename,D) ->
    gen_decode_constructed(Erules,Typename,D).

gen_decode_constructed(Erules,Typename,D) when record(D,type) ->
    asn1ct_name:start(),
    {CompList,TableConsInfo} = 
	case D#type.def of
	    #'SEQUENCE'{tablecinf=TCI,components=CL} ->
		{CL,TCI};
	    #'SET'{tablecinf=TCI,components=CL} ->
		{CL,TCI}
	end,
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
    {DecObjInf,UniqueFName} = 
	case TableConsInfo of
	    {ObjectSet,AttrN,N,UniqueFieldName} ->%% N is index of attribute that determines constraint
%%		{AttrN,ObjectSet};
		F = fun(#'ComponentType'{typespec=CT})->
			    case {CT#type.constraint,CT#type.tablecinf} of
				{[],[{objfun,_}|R]} -> true;
				_ -> false
			    end
		    end,
		case lists:any(F,CompList) of
		    true ->
			{{AttrN,{deep,ObjectSet,UniqueFieldName}},UniqueFieldName};
		    false ->
			{{AttrN,ObjectSet},UniqueFieldName}
		end;
	    _ ->
		{false,false}
	end,
    {AccTerm,AccBytes} =
	gen_dec_components_call(Typename,CompList,MaybeComma2,DecObjInf,Ext),
    case asn1ct_name:all(term) of
	[] -> emit(MaybeComma2); % no components at all
	_ -> emit({com,nl})
    end,
    case {AccTerm,AccBytes} of
	{[],[]} ->
	    ok;
	{L,[]} ->
	    ok;
	{[{ObjSet,LeadingAttr,Term}],ListOfOpenTypes} ->
	    DecObj = lists:concat(['DecObj',LeadingAttr,Term]),
	    emit({DecObj," =",nl,"   'getdec_",ObjSet,"'(",
		  {asis,UniqueFName},", ",Term,"),",nl}),
	    gen_dec_listofopentypes(DecObj,ListOfOpenTypes,false)
    end,
    %% we don't return named lists any more   Cnames = mkcnamelist(CompList), 
    demit({"Result = "}), %dbg
    %% return value as record
    emit(["{{'",asn1ct_gen:list2rname(Typename),"'"]),
    mkvlist(asn1ct_name:all(term)),
    emit({"},",{var,asn1ct_name:curr(bytes)},"}"}),
    emit({".",nl,nl}).

gen_dec_listofopentypes(_,[],_) ->
    emit(nl);
gen_dec_listofopentypes(DecObj,[{Cname,{FirstPFN,PFNList},Term}|Rest],Update) ->
    case Update of
	true ->
%%	    asn1ct_name:new(bytes),
	    asn1ct_name:new(term);
	_->
%%	    asn1ct_name:new(bytes),
	    asn1ct_name:new(term)
    end,
%%    emit({"{",{curr,term},", ",{curr,bytes},"} = "}),
    asn1ct_name:new(tmpterm),
    emit({"{",{curr,term},", _} = ",nl}),
    emit({indent(3),"case ",DecObj,"(",
	  {asis,FirstPFN},", ",Term,", telltype,",{asis,PFNList},") of",nl}),
    emit({indent(6),"{'EXIT', _} ->",nl}),
    emit({indent(9),"throw({runtime_error,{","'Type not compatible with table constraint'",",",Term,"}});",nl}),
    emit({indent(6),{curr,tmpterm}," ->",nl}),
    emit({indent(9),{curr,tmpterm},nl}),
    emit({indent(3),"end,",nl}),
    gen_dec_listofopentypes(DecObj,Rest,true).


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

gen_encode_choice(Erules,Typename,D) when record(D,type) ->
    {'CHOICE',CompList} = D#type.def,
    emit({"[",nl}),
    Ext = extensible(CompList),
    gen_enc_choice(Typename,CompList,Ext),
    emit({nl,"].",nl}).

gen_decode_choice(Erules,Typename,D) when record(D,type) ->
    asn1ct_name:start(),
    asn1ct_name:new(bytes),
    {'CHOICE',CompList} = D#type.def,
    Ext = extensible(CompList),
    gen_dec_choice(Typename,CompList,Ext),
    emit({".",nl}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Encode generator for SEQUENCE OF type


gen_encode_sof(Erules,Typename,SeqOrSetOf,D) when record(D,type) ->
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
    ObjFun =
	case D#type.tablecinf of
	    [{objfun,_}|R] ->
		", ObjFun";
	    _->
		""
	end,
    emit({nl,indent(3),"?RT_PER:encode_length(",
	  {asis,SizeConstraint},
	  ",length(Val)),",nl}),
    emit({indent(3),"'enc_",asn1ct_gen:list2name(Typename),
	      "_components'(Val",ObjFun,", [])"}),
    emit({nl,"].",nl}),
    gen_encode_sof_components(Typename,SeqOrSetOf,ComponentType).

gen_decode_sof(Erules,Typename,SeqOrSetOf,D) when record(D,type) ->
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
    ObjFun =
	case D#type.tablecinf of
	    [{objfun,_}|R] ->
		", ObjFun";
	    _ ->
		""
	end,
    emit({nl,"{Num,Bytes1} = ?RT_PER:decode_length(Bytes,",{asis,SizeConstraint},"),",nl}),
    emit({"'dec_",asn1ct_gen:list2name(Typename),
	      "_components'(Num, Bytes1, Telltype",ObjFun,", []).",nl}),
    gen_decode_sof_components(Typename,SeqOrSetOf,ComponentType).

gen_encode_sof_components(Typename,SeqOrSetOf,Cont) ->
    ObjFun =
	case Cont#type.tablecinf of
	    [{objfun,_}|R] ->
		", ObjFun";
	    _ ->
		""
	end,
    emit({"'enc_",asn1ct_gen:list2name(Typename),"_components'([]",
	  ObjFun,", Acc) -> lists:reverse(Acc);",nl,nl}),
    emit({"'enc_",asn1ct_gen:list2name(Typename),"_components'([H|T]",
	  ObjFun,", Acc) ->",nl}),
    emit({"'enc_",asn1ct_gen:list2name(Typename),"_components'(T"}), 
    emit({ObjFun,", ["}),
    %% the component encoder
    Constructed_Suffix = asn1ct_gen:constructed_suffix(SeqOrSetOf,
						       Cont#type.def),
    
    Conttype = asn1ct_gen:get_inner(Cont#type.def),
    case asn1ct_gen:type(Conttype) of
	{primitive,bif} ->
	    asn1ct_gen_per:gen_encode_prim(per,Cont,false,"H");
	{constructed,bif} ->
	    NewTypename = [Constructed_Suffix|Typename],
	    emit({"'enc_",asn1ct_gen:list2name(NewTypename),"'(H",
		  ObjFun,")",nl,nl});
	#typereference{val=Ename} ->
	    emit({"'enc_",Ename,"'(H)",nl,nl});
	#'Externaltypereference'{module=EMod,type=EType} ->
	    emit({"'",EMod,"':'enc_",EType,"'(H)",nl,nl});
	_ ->
	    emit({"'enc_",Conttype,"'(H)",nl,nl})
    end,
    emit({" | Acc]).",nl}).

gen_decode_sof_components(Typename,SeqOrSetOf,Cont) ->
    ObjFun =
	case Cont#type.tablecinf of
	    [{objfun,_}|R] ->
		", ObjFun";
	    _ ->
		""
	end,
    emit({"'dec_",asn1ct_gen:list2name(Typename),
	  "_components'(0, Bytes, Telltype",ObjFun,", Acc) ->",nl,
	  indent(3),"{lists:reverse(Acc), Bytes};",nl}),
    emit({"'dec_",asn1ct_gen:list2name(Typename),
	  "_components'(Num, Bytes, Telltype",ObjFun,", Acc) ->",nl}),
    emit({indent(3),"{Term,Remain} = "}),
    Constructed_Suffix = asn1ct_gen:constructed_suffix(SeqOrSetOf,
						       Cont#type.def),
    Conttype = asn1ct_gen:get_inner(Cont#type.def),
    case asn1ct_gen:type(Conttype) of
	{primitive,bif} ->
	    asn1ct_gen_per:gen_dec_prim(per,Cont,"Bytes"),
	    emit({com,nl});
	{constructed,bif} ->
	    NewTypename = [Constructed_Suffix|Typename],
	    emit({"'dec_",asn1ct_gen:list2name(NewTypename),
		  "'(Bytes, Telltype",ObjFun,"),",nl});
	#typereference{val=Dname} ->
	    emit({"'dec_",Dname,"'(Bytes,Telltype),",nl});
	#'Externaltypereference'{module=EMod,type=EType} ->
	    emit({"'",EMod,"':'dec_",EType,"'(Bytes,Telltype),",nl});
	_ ->
	    emit({"'dec_",Conttype,"'(Bytes,Telltype),",nl})
    end,
    emit({indent(3),"'dec_",asn1ct_gen:list2name(Typename),
	  "_components'(Num-1, Remain, Telltype",ObjFun,", [Term|Acc]).",nl}). 


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



gen_enc_components_call(TopType,{CompList,ExtList},MaybeComma,DynamicEnc,Ext) ->
    %% The type has extensionmarker
    Rpos = gen_enc_components_call1(TopType,CompList,1,MaybeComma,DynamicEnc,noext),
    case Ext of
	{ext,_,ExtNum} when ExtNum > 0 ->
	    emit([nl,
		  ",Extensions",nl]);
	_ -> true
    end,
    Epos = gen_enc_components_call1(TopType,ExtList,Rpos,MaybeComma,DynamicEnc,Ext);
gen_enc_components_call(TopType, CompList, MaybeComma, DynamicEnc, Ext) ->
    %% The type has no extensionmarker
    gen_enc_components_call1(TopType,CompList,1,MaybeComma,DynamicEnc,Ext).

gen_enc_components_call1(TopType,
			 [#'ComponentType'{name=Cname,typespec=Type,prop=Prop}|Rest],
			 Tpos,
			 MaybeComma, DynamicEnc, Ext) ->
    Pos = case Ext of
	      noext -> Tpos;
	      {ext,Epos,Enum} -> Tpos - Epos + 1
	  end,
    emit(MaybeComma),
    case Prop of
	'OPTIONAL' ->
	    gen_enc_component_optional(TopType,Cname,Type,Tpos,DynamicEnc,Ext);
	{'DEFAULT',DefVal} ->
	    gen_enc_component_default(TopType,Cname,Type,Tpos,DynamicEnc,Ext);
	_ ->
	    case Ext of
		{ext,ExtPos,_} when Tpos >= ExtPos ->
		    gen_enc_component_optional(TopType,Cname,Type,Tpos,DynamicEnc,Ext);
		_ ->
		    gen_enc_component_mandatory(TopType,Cname,Type,Tpos,DynamicEnc,Ext)
	    end
    end,
    case Rest of
	[] ->
	    Pos+1;
	_ ->
	    emit({com,nl}),
	    gen_enc_components_call1(TopType,Rest,Tpos+1,"",DynamicEnc,Ext)
    end;
gen_enc_components_call1(_TopType,[],Pos,_,_,_) ->
	Pos.

gen_enc_component_default(TopType,Cname,Type,Pos,DynamicEnc,Ext) ->
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
    gen_enc_line(TopType,Cname,Type,Element, Pos,DynamicEnc,Ext),
    emit({nl,"end"}).

gen_enc_component_optional(TopType,Cname,Type,Pos,DynamicEnc,Ext) ->
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
    gen_enc_line(TopType,Cname,Type,Element, Pos,DynamicEnc,Ext),
    emit({nl,"end"}).

gen_enc_component_mandatory(TopType,Cname,Type,Pos,DynamicEnc,Ext) ->
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    emit({nl,"%% attribute number ",Pos," with type ",
	      InnerType,nl}),
    gen_enc_line(TopType,Cname,Type,[],Pos,DynamicEnc,Ext).

gen_enc_line(TopType, Cname, Type, [], Pos,DynamicEnc,Ext) ->
    Element = io_lib:format("?RT_PER:cindex(~w,~s,~w)",[Pos+1,asn1ct_gen:mk_var(asn1ct_name:curr(val)),Cname]),
    gen_enc_line(TopType,Cname,Type,Element, Pos,DynamicEnc,Ext);
gen_enc_line(TopType,Cname,Type,Element, Pos,DynamicEnc,Ext) ->
    Atype = 
	case Type#type.constraint of
	    {tableconstraint_info,_} ->
		Type#type.def;
	    _ ->
		asn1ct_gen:get_inner(Type#type.def)
	end,
    case Ext of
	{ext,Ep1,_} when  Pos >= Ep1 ->
	    emit(["?RT_PER:encode_open_type(dummy,?RT_PER:complete("]);
	_ -> true
    end,
    case Atype of
	{typefield,_} ->
	    case DynamicEnc of
		{LeadingAttrName,Fun} ->
		    {_,RefedFieldName} = Type#type.constraint,
		    case RefedFieldName of
			{notype,T} ->
			    throw({error,{notype,type_from_object,T}});
			{_,Name} when atom(Name) ->
			    emit({"?RT_PER:encode_open_type([],?RT_PER:complete(",nl}),
			    emit({"   ",Fun,"(",{asis,Name},", ",
				  Element,", [])))"});
			_ ->
			    throw({asn1,{'internal error'}})
		    end
	    end;
	{objectfield,PrimFieldName1,PFNList} ->
	    case DynamicEnc of
		{LeadingAttrName,Fun} ->
		    emit({"?RT_PER:encode_open_type([],"
			  "?RT_PER:complete(",nl}),
		    emit({"   ",Fun,"(",{asis,PrimFieldName1},
			  ", ",Element,", ",{asis,PFNList},")))"})
	    end;
	_ ->
	    CurrMod = get(currmod),
	    case asn1ct_gen:type(Atype) of
		#'Externaltypereference'{module=Mod,type=EType} when 
		      (CurrMod==Mod) ->
		    emit({"'enc_",EType,"'(",Element,")"});
		#'Externaltypereference'{module=Mod,type=EType} ->
		    emit({"'",Mod,"':'enc_",
			  EType,"'(",Element,")"});
		#typereference{val=Ename} ->
		    emit({"'enc_",Ename,"'(",Element,")"});
		{notype,_} ->
		    emit({"'enc_",Atype,"'(",Element,")"});
		{primitive,bif} ->
		    EncType =
			case Atype of
			    {fixedtypevaluefield,_,Btype} ->
				Btype;
			    _ ->
				Type
			end,
		    NewElement = 
			case EncType#type.def of
			    {'ENUMERATED',_} ->
				io_lib:format("element2IfTuple(~s)",
					      [Element]);
			    _ ->
				Element
			end,
		    asn1ct_gen_per:gen_encode_prim(per,EncType,
						   false,NewElement);
		{constructed,bif} ->
		    NewTypename = [Cname|TopType],
		    case {Type#type.tablecinf,DynamicEnc} of
			{[{objfun,_}|R],{_,EncFun}} ->
%%			    emit({"?RT_PER:encode_open_type([],",
%%				  "?RT_PER:complete(",nl}),
			    emit({"'enc_",
				  asn1ct_gen:list2name(NewTypename),
				  "'(",Element,", ",EncFun,")"});
			_ ->
			    emit({"'enc_",
				  asn1ct_gen:list2name(NewTypename),
				  "'(",Element,")"})
		    end
	    end
    end,
    case Ext of 
	{ext,Ep2,_} when Pos >= Ep2 ->
	    emit(["))"]);
	_ -> true
    end.

gen_dec_components_call(TopType,{CompList,ExtList},MaybeComma,DecInfObj,Ext) ->
    %% The type has extensionmarker
    {Rpos,AccTerm,AccBytes} = 
	gen_dec_components_call1(TopType, CompList, 1, MaybeComma,DecInfObj,
				 noext,[],[]),
    emit([",",nl,"{Extensions,",{next,bytes},"} = "]),
    emit(["?RT_PER:getextension(Ext,",{curr,bytes},"),",nl]),
    asn1ct_name:new(bytes),
    {Epos,AccTermE,AccBytesE} = 
	gen_dec_components_call1(TopType,ExtList,Rpos,"",DecInfObj,Ext,[],[]),
    case ExtList of
	[] -> true;
	_ -> emit([",",nl])
    end,
    emit([{next,bytes},"= ?RT_PER:skipextensions(",{curr,bytes},",",
	  length(ExtList)+1,",Extensions)",nl]),
    asn1ct_name:new(bytes),
    {AccTerm++AccTermE,AccBytes++AccBytesE};

gen_dec_components_call(TopType,CompList,MaybeComma,DecInfObj,Ext) ->
    %% The type has no extensionmarker
    {_,AccTerm,AccBytes} =
	gen_dec_components_call1(TopType, CompList, 1, MaybeComma,DecInfObj,Ext,[],[]),
    {AccTerm,AccBytes}.


gen_dec_components_call1(TopType,
			 [#'ComponentType'{name=Cname,typespec=Type,prop=Prop}|Rest],
			 Tpos,MaybeComma,DecInfObj,Ext,AccTerm,AccBytes) ->
    Pos = case Ext of
	      noext -> Tpos;
	      {ext,Epos,Enum} -> Tpos - Epos + 1
	  end,
    emit(MaybeComma),
%%    asn1ct_name:new(term),
    InnerType = 
	case Type#type.constraint of
	    {tableconstraint_info,_} ->
		Type#type.def;
	    _ ->
		asn1ct_gen:get_inner(Type#type.def)
	end,
    case InnerType of
	#'Externaltypereference'{type=T} ->
	    emit({nl,"%%  attribute number ",Tpos," with type ",
		  T,nl});
	IT when tuple(IT) ->
	    emit({nl,"%%  attribute number ",Tpos," with type ",
		  element(2,IT),nl});
	_ ->
	    emit({nl,"%% attribute number ",Tpos," with type ",
		  InnerType,nl})
    end,

    case {InnerType,Type#type.tablecinf} of
	{{typefield,_},_} ->
%%	    emit({"{",{next,bytes},", _} = "});
%%%	    asn1ct_name:delete(bytes),
	    asn1ct_name:new(tmpterm),
	    emit({"{",{curr,tmpterm},", ",{next,bytes},"} = "});
	{{objectfield,_,_},_} ->
	    asn1ct_name:new(tmpterm),
	    emit({"{",{curr,tmpterm},", ",{next,bytes},"} = "});
%%	{_,[{objfun,_}|R]} ->
%%	    asn1ct_name:new(tmpterm),
%%	    emit({"{",{curr,tmpterm},", ",{next,bytes},"} = "});
	_ ->
	    asn1ct_name:new(term),
	    emit({"{",{curr,term},",",{next,bytes},"} = "})
    end,

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

    {TermVar,BytesVar} = gen_dec_line(TopType,Cname,Type,Tpos,DecInfObj,Ext),
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
	    {Pos+1,AccTerm++TermVar,AccBytes++BytesVar};
	_ ->
	    emit({com,nl}),
	    gen_dec_components_call1(TopType,Rest,Tpos+1,"",DecInfObj,Ext,
				     AccTerm++TermVar,AccBytes++BytesVar)
    end;

gen_dec_components_call1(_TopType,[],Pos,_,_,_,AccTerm,AccBytes) ->
	{Pos,AccTerm,AccBytes}.


%%gen_dec_component_no_val(TopType,Cname,Type,_,Pos,{ext,Ep,Enum}) when Pos >= Ep ->
%%    emit({"{asn1_NOEXTVALUE,",{curr,bytes},"}",nl});
gen_dec_component_no_val(TopType,Cname,Type,{'DEFAULT',DefVal},Pos,_) ->
    emit(["{",{asis,DefVal},",",{curr,bytes},"}",nl]);
gen_dec_component_no_val(TopType,Cname,Type,'OPTIONAL',Pos,_) ->
    emit({"{asn1_NOVALUE,",{curr,bytes},"}",nl});
gen_dec_component_no_val(TopType,Cname,Type,mandatory,Pos,{ext,Ep,Enum}) ->
    emit({"{asn1_NOVALUE,",{curr,bytes},"}",nl}).
    

gen_dec_line(TopType,Cname,Type,Pos,DecInfObj,Ext)  ->
    Atype = 
	case Type#type.constraint of
	    {tableconstraint_info,_} ->
		Type#type.def;
	    _ ->
		asn1ct_gen:get_inner(Type#type.def)
	end,
    BytesVar0 = asn1ct_gen:mk_var(asn1ct_name:curr(bytes)),
    BytesVar = case Ext of
		   {ext,Ep,_} when Pos >= Ep ->
		       emit(["begin",nl,"{TmpVal",Pos,",Trem",Pos,
			     "}=?RT_PER:decode_open_type(",
			     {curr,bytes},",[]),",nl,
			     "{TmpValx",Pos,",_}="]),
		       io_lib:format("TmpVal~p",[Pos]);
		   _ -> BytesVar0
	       end,
    SaveBytes = 
	case Atype of
	    {typefield,_} ->
		case DecInfObj of
		    false ->
			{_,{_,RefedFieldName}} = Type#type.constraint,
			asn1ct_name:new(tmpterm),
			emit({indent(3),"{",{curr,tmpterm},", ",{next,bytes},
			      "} = ?RT_PER:decode_open_type(",{curr,bytes},
			      ", []),",nl}),
			emit({indent(3),"case ObjFun(",{asis,RefedFieldName},
			      ",",{curr,tmpterm},",telltype,[]) of", nl}),
			emit({indent(6),"{'EXIT',_} ->",nl}),
			emit({indent(9),"throw({runtime_error,{'Type not ",
			      "compatible with tableconstraint',",
			      {curr,tmpterm},"}});",nl}),
			asn1ct_name:new(tmpterm),
			emit({indent(6),"{",{curr,tmpterm},", _} ->",nl}),
			emit({indent(9),"{",Cname,", {",{curr,tmpterm},", ",
			      {next,bytes},"}}",nl}),
			emit({indent(3),"end"}),
			[];
		    _ ->
			emit({"?RT_PER:decode_open_type(",{curr,bytes},
			      ", [])"}),
			{_,{_,RefedFieldName}} = Type#type.constraint,
			[{Cname,{RefedFieldName,[]},
			  asn1ct_gen:mk_var(asn1ct_name:curr(tmpterm))}]
		    %%		  asn1ct_gen:mk_var(asn1ct_name:next(bytes))}]
		end;
	    {objectfield,PrimFieldName1,PFNList} ->
		emit({"?RT_PER:decode_open_type(",{curr,bytes},", [])"}),
		[{Cname,{PrimFieldName1,PFNList},
		  asn1ct_gen:mk_var(asn1ct_name:curr(tmpterm))}];
	    _ ->
		CurrMod = get(currmod),
		case asn1ct_gen:type(Atype) of
		    #'Externaltypereference'{module=Mod,type=EType} when
			  CurrMod == Mod ->
			emit({"'dec_",EType,"'(",BytesVar,",telltype)"});
		    #'Externaltypereference'{module=Mod,type=EType} ->
			emit({"'",Mod,"':'dec_",EType,"'(",BytesVar,
			      ",telltype)"});
		    {primitive,bif} ->
			case Atype of
			   {fixedtypevaluefield,_,Btype} ->
				asn1ct_gen_per:gen_dec_prim(per,Btype,
							    BytesVar);
			    _ ->
				asn1ct_gen_per:gen_dec_prim(per,Type,
							    BytesVar)
			end;
		    #typereference{val=Dname} ->
			emit({"'dec_",Dname,"'(",BytesVar,",telltype)"});
		    {notype,_} ->
			emit({"'dec_",Atype,"'(",BytesVar,",telltype)"});
		    {constructed,bif} ->
			NewTypename = [Cname|TopType],
			case Type#type.tablecinf of
			    [{objfun,_}|R] ->
				emit({"'dec_",asn1ct_gen:list2name(NewTypename),
				      "'(",BytesVar,", Telltype, ObjFun)"});
			    _ ->
				emit({"'dec_",asn1ct_gen:list2name(NewTypename),
				      "'(",BytesVar,", Telltype)"})
			end
		end,
		case DecInfObj of
		    {Cname,{_,OSet,UniqueFName}} ->
			emit({",",nl,"ObjFun = 'getdec_",OSet,"'(",
			      {asis,UniqueFName},", ",{curr,term},")"});
		    _ ->
			ok
		end,
		[]
%%	    {ObjSet,FieldName} ->
%%		asn1ct_name:new(bytes),
%%		emit({"{",{next,bytes},",_} = ?RT_PER:decode_open_type(",
%%		      {curr,bytes},"[]),",nl}),
%%		[{ObjSet,Cname,asn1ct_name:curr(bytes)}]
	end,
    case Ext of 
	{ext,Ep2,_} when Pos >= Ep2 ->
	    emit([", {TmpValx",Pos,",Trem",Pos,"}",nl,"end"]);
	_ -> true
    end,
    %% Prepare return value
    case DecInfObj of
	{Cname,ObjSet} ->
	    {[{ObjSet,Cname,asn1ct_gen:mk_var(asn1ct_name:curr(term))}],
	     SaveBytes};
	_ ->
	    {[],SaveBytes}
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
    EncObj =
	case Type#type.constraint of
	    {tableconstraint_info,_} ->
		{no_attr,"ObjFun"};
	    _ ->
		false
	end,
    emit({{asis,Cname}," ->",nl}),
    gen_enc_line(TopType,Cname,Type,"element(2,Val)", Pos+1,EncObj,Ext),
    emit({";",nl}),
    gen_enc_choice2(TopType,[H2|T], Pos+1, Ext);
gen_enc_choice2(TopType,[H1|T], Pos, Ext) when record(H1,'ComponentType') ->
    Cname = H1#'ComponentType'.name,
    Type = H1#'ComponentType'.typespec,
    EncObj =
	case Type#type.constraint of
	    {tableconstraint_info,_} ->
		{no_attr,"ObjFun"};
	    _ ->
		false
	end,
    emit({{asis,H1#'ComponentType'.name}," ->",nl}),
    gen_enc_line(TopType,Cname,Type,"element(2,Val)", Pos+1,EncObj,Ext),
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
    case Type#type.def of
	{typefield,_} ->
	    emit({Pos," -> ",nl}),
	    gen_dec_line(TopType,Cname,Type,Pos+1,false,Ext),
	    emit({";",nl});
	_ ->
	    emit({Pos," -> {",{asis,Cname},",",nl}),
	    gen_dec_line(TopType,Cname,Type,Pos+1,false,Ext),
	    emit({"};",nl})
    end,
    gen_dec_choice2(TopType,[H2|T],Pos+1,Ext);
gen_dec_choice2(TopType,[H1,H2|T],Pos,Ext) when record(H1,'ComponentType') ->
    gen_dec_choice2(TopType,[H1|T],Pos,Ext); % skip extensionmark
gen_dec_choice2(TopType,[H1|T],Pos,Ext) when record(H1,'ComponentType') ->
    Cname = H1#'ComponentType'.name,
    Type = H1#'ComponentType'.typespec,
    case Type#type.def of
	{typefield,_} ->
	    emit({Pos," -> ",nl}),
	    gen_dec_line(TopType,Cname,Type,Pos+1,false,Ext);
	_ ->
	    emit({Pos," -> {",{asis,Cname},",",nl}),
	    gen_dec_line(TopType,Cname,Type,Pos+1,false,Ext),
	    emit("}")
    end,
    gen_dec_choice2(TopType,[T],Pos+1);
gen_dec_choice2(TopType,[H1|T],Pos,Ext) ->
    gen_dec_choice2(TopType,T,Pos,Ext);% skip extensionmark
gen_dec_choice2(TopType,[],Pos,Ext)  ->
    Pos.

indent(N) ->
    lists:duplicate(N,32). % 32 = space














