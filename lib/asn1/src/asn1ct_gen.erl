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
-module(asn1ct_gen).

-include("asn1_records.hrl").
%%-compile(export_all).
-export([pgen_exports/3,
	 pgen_hrl/4,
	 gen_head/3,
	 demit/1,
	 emit/1,
	 fopen/2,
	 get_inner/1,type/1,def_to_tag/1]).

pgen_exports(Erules,Module,{Types,Values,_}) ->
    emit({"-export([encoding_rule/0]).",nl}),
    case Types of
	[] -> ok;
	_ ->
	    emit({"-export([",nl}),
	    case Erules of
		ber ->
		    gen_exports1(Types,"enc_",2);
		ber_bin ->
		    gen_exports1(Types,"enc_",2);
		_ ->
		    gen_exports1(Types,"enc_",1)
	    end,
	    emit({"-export([",nl}),
	    gen_exports1(Types,"dec_",2),
	    case Erules of
		ber ->
		    emit({"-export([",nl}),
		    gen_exports1(Types,"dec_",3);
		ber_bin ->
		    emit({"-export([",nl}),
		    gen_exports1(Types,"dec_",3);
		_ -> ok
	    end
    end,
    case Values of
	[] -> ok;
	_ ->
	    emit({"-export([",nl}),
	    gen_exports1(Values,"",0)
    end,
    emit({"encoding_rule() ->",nl}),
    emit({{asis,Erules},"."}),
%%    if %% XXX
%%	Erules == ber_bin ->
%%	    emit({{asis,ber},"."});
%%	true ->
%%	    emit({{asis,Erules},"."})
%%    end,
    emit({nl,nl}).

gen_exports1([F1,F2|T],Prefix,Arity) ->
	emit({"'",Prefix,F1,"'/",Arity,com,nl}),
	gen_exports1([F2|T],Prefix,Arity);
gen_exports1([Flast|T],Prefix,Arity) ->
	emit({"'",Prefix,Flast,"'/",Arity,nl,"]).",nl,nl}).

open_hrl(OutFile,Module) ->
    File = lists:concat([OutFile,".hrl"]),
    Fid = fopen(File,write),
    put(gen_file_out,Fid),
    gen_hrlhead(Module).

%% EMIT functions ************************
%% ***************************************

						% debug generation
demit(Term) ->
    case get(asndebug) of
	true -> emit(Term);
	_ ->true
    end.

						% always generation

emit({external,M,T}) ->
    emit(T);

emit({prev,Variable}) when atom(Variable) ->
    emit({var,asn1ct_name:prev(Variable)});

emit({next,Variable}) when atom(Variable) ->
    emit({var,asn1ct_name:next(Variable)});

emit({curr,Variable}) when atom(Variable) ->
    emit({var,asn1ct_name:curr(Variable)});
    
emit({var,Variable}) when atom(Variable) ->
    [Head|V] = atom_to_list(Variable),
    emit([Head-32|V]);

emit({var,Variable}) ->
    [Head|V] = Variable,
    emit([Head-32|V]);

emit({asis,What}) ->
    format(get(gen_file_out),"~w",[What]);

emit(nl) ->
    nl(get(gen_file_out));

emit(com) ->
    emit(",");

emit(tab) ->
    put_chars(get(gen_file_out),"     ");

emit(What) when integer(What) ->
    put_chars(get(gen_file_out),integer_to_list(What));

emit(What) when list(What), integer(hd(What)) ->
    put_chars(get(gen_file_out),What);

emit(What) when atom(What) ->
    put_chars(get(gen_file_out),atom_to_list(What));

emit(What) when tuple(What) ->
    emit_parts(tuple_to_list(What));

emit(What) when list(What) ->
    emit_parts(What);

emit(X) ->
    exit({'cant emit ',X}).

emit_parts([]) -> true;
emit_parts([H|T]) ->
    emit(H),
    emit_parts(T).

format(undefined,X,Y) ->
    io:format(X,Y);
format(X,Y,Z) ->
    io:format(X,Y,Z).

nl(undefined) -> io:nl();
nl(X) -> io:nl(X).

put_chars(undefined,X) ->
    io:put_chars(X);
put_chars(Y,X) ->
    io:put_chars(Y,X).

fopen(F, Mode) ->
    case file:open(F, Mode) of
	{ok, Fd} -> 
	    Fd;
	{error, Reason} ->
	    io:format("** Can't open file ~p ~n", [F]),
	    exit({error,Reason})
    end.

pgen_hrl(Erules,Module,TypeOrVal,_Indent) ->
    put(currmod,Module),
    {Types,_,_} = TypeOrVal,
    case pgen_hrltypes(Erules,Module,Types,0) of
	0 -> 0;
	X ->
	    Fid = get(gen_file_out),
	    file:close(Fid),
	    io:format("--~p--~n",[{generated,lists:concat([get(outfile),".hrl"])}]),
	    X
    end.

pgen_hrltypes(_,_,[],NumRecords) ->
    NumRecords;
pgen_hrltypes(Erules,Module,[H|T],NumRecords) ->
%    io:format("records = ~p~n",NumRecords),
    Typedef = asn1_db:dbget(Module,H),
    AddNumRecords = gen_record(Typedef,NumRecords),
    pgen_hrltypes(Erules,Module,T,NumRecords+AddNumRecords).

%% Generate record functions **************
%% Generates an Erlang record for each named and unnamed SEQUENCE and SET in the ASN.1 
%% module. If no SEQUENCE or SET is found there is no .hrl file generated

gen_record(Tdef,NumRecords) when record(Tdef,typedef) ->
    Name = Tdef#typedef.name,
    Type = Tdef#typedef.typespec,
    gen_record(Name,Type,NumRecords).
    
gen_record(Name,[#'ComponentType'{name=Cname,typespec=Type}|T],Num) ->
    Num2 = gen_record(list_to_atom(lists:concat([Name,"_",Cname])),Type,Num),
    gen_record(Name,T,Num2);
gen_record(Name,[H|T],Num) -> % skip EXTENSIONMARK
    gen_record(Name,T,Num);
gen_record(_Name,[],Num) ->
    Num;

gen_record(Name,Type,Num) when record(Type,type) ->    
    Def = Type#type.def,
    Rec = case Def of
	      {'SEQUENCE',_CompList} -> 
		  {record,Def};
	      {'SET',_CompList} -> 
		  {record,Def}; 
	      {'CHOICE',_CompList} -> {inner,Def};
	      {'SEQUENCE OF',_CompList} -> {list_to_atom(lists:concat([Name,"_SEQOF"])),Def};
	      {'SET OF',_CompList} -> {list_to_atom(lists:concat([Name,"_SETOF"])),Def};
	      _ -> false
    end,
    case Rec of
	false -> Num;
	{record,{SeqOrSet,CompList}} ->
	    case Num of
		0 -> open_hrl(get(outfile),get(currmod));
		_ -> true
	    end,
	    emit({"-record('",Name,"',{",nl}),
	    RootList = case CompList of
			   _ when list(CompList) ->
			       CompList;
			   {_Rl,_} -> _Rl
		       end,
	    gen_record2(Name,'SEQUENCE',RootList),
	    case CompList of
		{_,[]} ->
		    emit({"}). % with extension mark",nl,nl});
		{Tr,ExtensionList2} ->
		    case Tr of
			[] -> true;
			_ -> emit({",",nl})
		    end,
		    emit({"%% with extensions",nl}),
		    gen_record2(Name, 'SEQUENCE', ExtensionList2, ext),
		    emit({"}).",nl,nl});
		_ -> 
		    emit({"}).",nl,nl})
	    end,
	    gen_record(Name,CompList,Num+1);
	{inner,{'CHOICE', CompList}} ->
	    gen_record(Name,CompList,Num);
	{NewName,{_, CompList}} ->
	    gen_record(NewName,CompList,Num)
    end;
gen_record(_,_,NumRecords) -> % skip CLASS etc for now.
     NumRecords.
		    
gen_head(Erules,Mod,Hrl) ->
    {Rtmac,Rtmod} = case Erules of
			per ->
			    emit({"%% Generated by the Erlang ASN.1 PER-"
				  "compiler version:",asn1ct:vsn(),nl}),
			    {"RT_PER",?RT_PER};
			ber ->
			    emit({"%% Generated by the Erlang ASN.1 BER-"
				  "compiler version:",asn1ct:vsn(),nl}),
			    {"RT_BER",?RT_BER};
			ber_bin ->
			    emit({"%% Generated by the Erlang ASN.1 BER-"
				  "compiler version, utilizing bit-syntax:",
				  asn1ct:vsn(),nl}),
			    {"RT_BER_BIN",?RT_BER_BIN}
    end,
    emit({"%% Purpose: encoder and decoder to the types in mod ",Mod,nl,nl}),
    emit({"-module('",Mod,"').",nl}),
    put(currmod,Mod),
    %emit({"-compile(export_all).",nl}),
    case Hrl of
	0 -> true;
	_ -> 
	    emit({"-include(\"",Mod,".hrl\").",nl})
    end,
    emit(["-define('",Rtmac,"',",Rtmod,").",nl]).

gen_hrlhead(Mod) ->
    emit({"%% Generated by the Erlang ASN.1 compiler version:",asn1ct:vsn(),nl}),
    emit({"%% Purpose: Erlang record definitions for each named and unnamed",nl}),
    emit({"%% SEQUENCE and SET in module ",Mod,nl,nl}),
    emit({nl,nl}).

gen_record2(Name,SeqOrSet,Comps) ->
    gen_record2(Name,SeqOrSet,Comps,noext).

gen_record2(Name,SeqOrSet,[],Extension) ->
    true;
gen_record2(Name,SeqOrSet,[H],Extension) ->
    #'ComponentType'{name=Cname} = H,
    emit({asis,Cname}),
    gen_record_default(H, Extension);
gen_record2(Name,SeqOrSet,[H|T], Extension) ->
    #'ComponentType'{name=Cname} = H,
    emit({asis,Cname}),
    gen_record_default(H, Extension),
    emit(", "),
    gen_record2(Name,SeqOrSet,T, Extension).

%gen_record_default(C, ext) ->
%    emit(" = asn1_NOEXTVALUE");
gen_record_default(#'ComponentType'{prop='OPTIONAL'}, _)->
    emit(" = asn1_NOVALUE"); 
gen_record_default(#'ComponentType'{prop={'DEFAULT',Dval}}, _)->
    emit(" = asn1_DEFAULT"); 
gen_record_default(_, _) ->
    true.

get_inner(A) when atom(A) -> A;    
get_inner(Ext) when record(Ext,'Externaltypereference') -> Ext;    
get_inner(Tref) when record(Tref,typereference) -> Tref;
get_inner(T) when tuple(T) -> element(1,T).

type(X) when record(X,'Externaltypereference') ->
    X;
type(X) when record(X,typereference) ->
    X;
type('ASN1_OPEN_TYPE') ->
    'ASN1_OPEN_TYPE';
type(X) ->
    %%    io:format("asn1_types:type(~p)~n",[X]),
    case catch type2(X) of
	{'EXIT',_} ->
	    {notype,X};
	Normal ->
	    Normal
    end.

type2(X) ->
    case prim_bif(X) of
	true ->
	    {primitive,bif};
	false ->
	    case construct_bif(X) of
		true ->
		    {constructed,bif};
		false ->
		    {undefined,user}
	    end
    end.

prim_bif(X) ->
    lists:member(X,['INTEGER' ,
		    'ENUMERATED',
		    'OBJECT IDENTIFIER',
		    'ANY',
		    'NULL',
		    'BIT STRING' ,
		    'OCTET STRING' ,
		    'ObjectDescriptor',
		    'NumericString',
		    'TeletexString',
		    'VideotexString',
		    'UTCTime',
		    'GeneralizedTime',
		    'GraphicString',
		    'VisibleString',
		    'GeneralString',
		    'PrintableString',
		    'IA5String',
		    'UniversalString',
		    'BMPString',
		    'ENUMERATED',
		    'BOOLEAN']).

construct_bif(T) ->
    lists:member(T,['SEQUENCE' ,
		    'SEQUENCE OF' ,
		    'CHOICE' ,
		    'SET' ,
		    'SET OF']).

def_to_tag(#tag{class=Class,number=Number}) ->
    {Class,Number};
def_to_tag(Def) ->
    {'UNIVERSAL',get_inner(Def)}.
    




