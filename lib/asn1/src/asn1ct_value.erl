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
-module(asn1ct_value).

%%  Generate Erlang values for ASN.1 types.
%%  The value is randomized within it's constraints

-include("asn1_records.hrl").
%-compile(export_all).

-export([get_type/3]).



%% Generate examples of values ******************************
%%****************************************x


get_type(M,Typename,Tellname) ->
    case asn1_db:dbget(M,Typename) of
	undefined -> 
	    {asn1_error,{not_found,{M,Typename}}};
	Tdef when record(Tdef,typedef) ->
	    Type = Tdef#typedef.typespec,
	    get_type(M,Typename,Type,Tellname);
	Err ->
	    {asn1_error,{other,Err}}
    end.

get_type(M,Typename,Type,Tellname) when record(Type,type) ->
    InnerType = get_inner(Type#type.def),
    case asn1ct_gen:type(InnerType) of
	#'Externaltypereference'{module=Emod,type=Etype} ->
	    get_type(Emod,Etype,Tellname);
	{_,user} ->
	    case Tellname of
		yes -> {Typename,get_type(M,InnerType,no)};
		no -> get_type(M,InnerType,no)
	    end;
	{notype,_} ->
	    true;
	{primitive,bif} ->
	    get_type_prim(Type);
	'ASN1_OPEN_TYPE' ->
	    "open_type";
	{constructed,bif} ->
	    get_type_constructed(M,Typename,InnerType,Type)
    end;
get_type(M,Typename,#'ComponentType'{name = Name,prop= Prop,typespec = Type},Tellname)  ->
    get_type(M,list_to_atom(lists:concat([Typename,"_",Name])),Type,no);
get_type(_,_,_,_) -> % 'EXTENSIONMARK'
    undefined.

get_inner(A) when atom(A) -> A;    
get_inner(Ext) when record(Ext,'Externaltypereference') -> Ext;    
get_inner({typereference,_Pos,Name}) -> Name;
get_inner(T) when tuple(T) -> element(1,T).



get_type_constructed(M,Typename,InnerType,D) when record(D,type) ->
    case InnerType of
	'SET' ->
	    get_sequence(M,Typename,D);
	'SEQUENCE' ->
	    get_sequence(M,Typename,D);
	'CHOICE' ->
	    get_choice(M,Typename,D);
	'SEQUENCE OF' ->
	    get_sequence_of(M,Typename,D,"_SEQOF");
	'SET OF' ->
	    get_sequence_of(M,Typename,D,"_SETOF");
	Other ->
	    exit({nyi,InnerType})
    end.

get_sequence(M,Typename,Type) ->
    {_SEQorSET,CompList} = Type#type.def,
    case get_components(M,Typename,CompList) of
        [] ->
            {Typename};
        C ->
            list_to_tuple([Typename|C])
    end.

get_components(M,Typename,{Root,Ext}) ->
    get_components(M,Typename,Root++Ext);

%% Should enhance this *** HERE *** with proper handling of extensions

get_components(M,Typename,[H|T]) ->
    [get_type(M,Typename,H,no)|
    get_components(M,Typename,T)];
get_components(M,Typename,[]) ->
    [].

get_choice(M,Typename,Type) ->
    {'CHOICE',TCompList} = Type#type.def,
    case TCompList of
	[] -> 
	    {asn1_EMPTY,asn1_EMPTY};
	{[],_} ->
	    {asn1_EMPTY,asn1_EMPTY};
	{CompList,_} -> % Should be enhanced to handle extensions too
	    C = lists:nth(random(length(CompList)),CompList),
	    {C#'ComponentType'.name,get_type(M,Typename,C,no)};
	CompList when list(CompList) ->
	    C = lists:nth(random(length(CompList)),CompList),
	    {C#'ComponentType'.name,get_type(M,Typename,C,no)}
    end.
    
get_sequence_of(M,Typename,Type,TypeSuffix) ->
    %% should generate length according to constraints later
    {_,Oftype} = Type#type.def,
    C = Type#type.constraint,
    S = size_random(C),
    NewTypeName = list_to_atom(lists:concat([Typename,TypeSuffix])),
    gen_list(M,NewTypeName,Oftype,no,S).

gen_list(M,Typename,Oftype,Tellname,0) ->
    [];
gen_list(M,Typename,Oftype,Tellname,N) ->
    [get_type(M,Typename,Oftype,no)|gen_list(M,Typename,Oftype,Tellname,N-1)].
    
get_type_prim(D) ->
    C = D#type.constraint,
    case D#type.def of
	'INTEGER' ->
	    i_random(C);
	{'INTEGER',NamedNumberList} ->
	    NN = [X||{X,Y} <- NamedNumberList],
	    case NN of 
		[] ->
		    i_random(C);
		_ ->
		    lists:nth(random(length(NN)),NN)
	    end;
	{'ENUMERATED',NamedNumberList} ->
	    NN = [X||{X,Y} <- NamedNumberList],
	    case NN of
		[] ->
		    asn1_EMPTY;
		_ ->
		    lists:nth(random(length(NN)),NN)
	    end;
	{'BIT STRING',NamedNumberList} ->
	    NN = [X||{_,X,Y} <- NamedNumberList],
	    case NN of
		[] ->
		    Bl1 =lists:reverse(adjust_list(size_random(C),[1,0,1,1])),
		    lists:reverse(lists:dropwhile(fun(0)->true;(1)->false end,Bl1));
		_ ->
		    [lists:nth(random(length(NN)),NN)]
	    end;
	'ANY' ->
	    exit({asn1_error,nyi,'ANY'});
	'NULL' ->
	    'NULL';
	'OBJECT IDENTIFIER' ->
	    Len = random(3),
	    Olist = [(random(1000)-1)||X <-lists:seq(1,Len)],
	    list_to_tuple([random(3)-1,random(40)-1|Olist]);
	'ObjectDescriptor' ->
	    object_descriptor_nyi;
	'BOOLEAN' ->
	    true;
	'OCTET STRING' ->
	    adjust_list(size_random(C),c_string(C,"OCTET STRING"));
	'NumericString' ->
	    adjust_list(size_random(C),c_string(C,"0123456789"));
	'TeletexString' ->
	    adjust_list(size_random(C),c_string(C,"TeletexString"));
	'VideotexString' ->
	    adjust_list(size_random(C),c_string(C,"VideotexString"));
	'UTCTime' ->
	    "97100211-0500";
	'GeneralizedTime' ->
	    "19971002103130.5";
	'GraphicString' ->
	    adjust_list(size_random(C),c_string(C,"GraphicString"));
	'VisibleString' ->
	    adjust_list(size_random(C),c_string(C,"VisibleString"));
	'GeneralString' ->
	    adjust_list(size_random(C),c_string(C,"GeneralString"));
	'PrintableString' ->
	    adjust_list(size_random(C),c_string(C,"PrintableString"));
	'IA5String' ->
	    adjust_list(size_random(C),c_string(C,"IA5String"));
	'BMPString' ->
	    adjust_list(size_random(C),c_string(C,"BMPString"));
	'UniversalString' ->
	    adjust_list(size_random(C),c_string(C,"UniversalString"));
	XX ->
	    exit({asn1_error,nyi,XX})
    end.

c_string(undefined,Default) ->
    Default;
c_string(C,Default) ->
    case get_constraint(C,'PermittedAlphabet') of
	{'SingleValue',Sv} when list(Sv) ->
	    Sv;
	{'SingleValue',V} when integer(V) ->
	    [V];
	no ->
	    Default
    end.

random(Upper) ->
    {A1,A2,A3} = erlang:now(),
    random:seed(A1,A2,A3),
    random:uniform(Upper).

size_random(C) ->
    case get_constraint(C,'SizeConstraint') of
	no ->
	    c_random({0,5},no);
	{Lb,Ub} when Ub-Lb =< 4 ->
	    c_random({Lb,Ub},no);
	{Lb,Ub}  ->
	    c_random({Lb,Lb+4},no);
	Sv ->
	    c_random(no,Sv)
    end.
	
i_random(C) ->
    c_random(get_constraint(C,'ValueRange'),get_constraint(C,'SingleValue')).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% c_random(Range,SingleValue)
%% only called from other X_random functions

c_random(VRange,Single) ->
    case {VRange,Single} of
	{no,no} ->
	    random(16#fffffff) - (16#fffffff bsr 1);
	{R,no} ->
	    case R of 
		{Lb,Ub} when integer(Lb),integer(Ub) ->
		    Range = Ub - Lb +1,
		    Lb + (random(Range)-1);
		{Lb,'MAX'} ->
		    Lb + random(16#fffffff)-1;
		{'MIN',Ub} ->
		    Ub - random(16#fffffff)-1
	    end;
	{_,S} when integer(S) ->
	    S;
	{_,S} when list(S) ->
	    lists:nth(random(length(S)),S)
    end.

adjust_list(Len,Orig) ->
    adjust_list1(Len,Orig,Orig,[]).

adjust_list1(0,Orig,[Oh|Ot],Acc) ->
    lists:reverse(Acc);
adjust_list1(Len,Orig,[],Acc) ->
    adjust_list1(Len,Orig,Orig,Acc);
adjust_list1(Len,Orig,[Oh|Ot],Acc) ->
    adjust_list1(Len-1,Orig,Ot,[Oh|Acc]).

%% for future use
%allowed_constraint('BIT STRING') -> c_return([yes,yes,no,yes,no,no,no]);
%allowed_constraint('BOOLEAN') -> c_return([yes,yes,no,no,no,no,no]);
%allowed_constraint('CHOICE') -> c_return([yes,yes,no,no,no,no,yes]);
%allowed_constraint('EMBEDDED-PDV') -> c_return([yes,no,no,no,no,no,yes]);
%allowed_constraint('ENUMERATED') -> c_return([yes,yes,no,no,no,no,no]);
%allowed_constraint('EXTERNAL') -> c_return([yes,no,no,no,no,no,yes]);
%allowed_constraint('INSTANCE OF') -> c_return([yes,yes,no,no,no,no,yes]);
%allowed_constraint('INTEGER') -> c_return([yes,yes,yes,no,no,no,no]);
%allowed_constraint('NULL') -> c_return([yes,yes,no,no,no,no,no]);
%allowed_constraint('OBJECT-CLASS-FIELD-TYPE') -> c_return([yes,yes,no,no,no,no,no]);
%allowed_constraint('OBJECT IDENTIFIER') -> c_return([yes,yes,no,no,no,no,no]);
%allowed_constraint('OCTET STRING') -> c_return([yes,yes,no,yes,no,no,no]);
%allowed_constraint('OPEN TYPE') -> c_return([no,no,no,no,no,yes,no]);
%allowed_constraint('REAL') -> c_return([yes,yes,yes,no,no,no,yes]);
%allowed_constraint('BMPString') -> c_return([yes,yes,yes,yes,yes,no,no]);
%allowed_constraint('IA5String') -> c_return([yes,yes,yes,yes,yes,no,no]);
%allowed_constraint('NumericString') -> c_return([yes,yes,yes,yes,yes,no,no]);
%allowed_constraint('PrintableString') -> c_return([yes,yes,yes,yes,yes,no,no]);
%allowed_constraint('VisibleString') -> c_return([yes,yes,yes,yes,yes,no,no]);
%allowed_constraint('UniversalString') -> c_return([yes,yes,yes,yes,yes,no,no]);
%allowed_constraint('SEQUENCE') -> c_return([yes,yes,no,no,no,no,yes]);
%allowed_constraint('SEQUENCE OF') -> c_return([yes,yes,no,yes,no,no,yes]);
%allowed_constraint('SET') -> c_return([yes,yes,no,no,no,no,yes]);
%allowed_constraint('SET OF') -> c_return([yes,yes,no,yes,no,no,yes]);
%allowed_constraint(_) -> c_return([yes,no,no,yes,no,no,yes]). % Unrestricted charstrings
%
%c_return(L) ->
%    c_return1(['SingleValue',
%	       'ContainedSubtype',
%	       'ValueRange',
%	       'SizeConstraint',
%	       'PermittedAlphabet',
%	       'Typeconstraint',
%	       'InnerSubtyping'],L).
%
%c_return1([Nh|Nt],[no|Ct]) ->
%    c_return1(Nt,Ct);
%c_return1([Nh|Nt],[yes|Ct]) ->
%    [Nh|c_return1(Nt,Ct)];
%c_return1([],_) ->
%    [].

get_constraint(C,Key) ->
    case lists:keysearch(Key,1,C) of
	false ->
	     no;
	{value,{_,V}} -> 
	    V
    end.
