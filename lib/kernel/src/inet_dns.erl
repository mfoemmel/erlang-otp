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
-module(inet_dns).

%% Dns record enocde/decode

-export([decode/1, encode/1]).
-export([decode_header/1, encode_header/1]).
-export([number_of_queries/1, number_of_answers/1,
	 number_of_authority/1, number_of_resources/1]).

-import(lists, [reverse/1, reverse/2, nthtail/2]).

-include("inet_int.hrl").
-include("inet_dns.hrl").

%% get N bits from X starting at bit B
-define(bitf(X, N, B), (((X) bsr (B)) band ((1 bsl (N))-1))).

-define(tolower(C),
	if (C) >= $A, (C) =< $Z -> ((C)-$A)+$a;
	   true -> C
	end).

%%
%% Decode a dns buffer.
%%

decode(Buffer) ->
    case decode_header(Buffer) of
	{ok, H, [QD1,QD0,AN1,AN0,NS1,NS0,AR1,AR0 | Ptr0]} ->
	    Qd = ?u16(QD1, QD0),
	    An = ?u16(AN1, AN0),
	    Ns = ?u16(NS1, NS0),
	    Ar = ?u16(AR1, AR0),
	    case decode_sections(Qd, An, Ns, Ar, Buffer, Ptr0) of
		{ok, {QdList,AnList,NsList,ArList}} ->
		    {ok, #dns_rec {
				   header = H,
				   qdlist = QdList, 
				   anlist = AnList,
				   nslist = NsList,
				   arlist = ArList }};
		Error -> Error
	    end;
	Error -> Error
    end.


decode_header([ID1,ID0,F1,F0 | Rest]) ->
    H = #dns_header {
		     id = ?u16(ID1,ID0),
		     %% Flag byte 0
		     qr =     ?bitf(F1, 1, 7),
		     opcode = ?bitf(F1, 4, 3),
		     aa =     ?bitf(F1, 1, 2),
		     tc =     ?bitf(F1, 1, 1),
		     rd =     ?bitf(F1, 1, 0),
		     %% Flag byte 1      
		     ra =     ?bitf(F0, 1, 7),
		     pr =     ?bitf(F0, 1, 6),
		     rcode =  ?bitf(F0, 4, 0)
		     },
    {ok, H, Rest};
decode_header(_) -> {error, fmt}.

number_of_queries(Packet) when length(Packet) >= 12 ->
    [_,_,_,_, 
     QD0,QD1 | _] = Packet,
    ?u16(QD0, QD1).

number_of_answers(Packet) when length(Packet) >= 12 ->
    [_,_,_,_, 
     _,_,AN0,AN1 | _] = Packet,
    ?u16(AN0, AN1).

number_of_authority(Packet) when length(Packet) >= 12 ->
    [_,_,_,_, 
     _,_,_,_,NS0,NS1 | _] = Packet,
    ?u16(NS0, NS1).

number_of_resources(Packet) when length(Packet) >= 12 ->
    [_,_,_,_, 
     _,_,_,_,_,_,AR0,AR1 | _] = Packet,
    ?u16(AR0, AR1).

decode_sections(Qd, An, Ns, Ar, Buffer, Ptr0) ->
    case decode_query_section(Qd, Buffer, Ptr0) of
	{ok, {QdList, Ptr1}} ->
	    case decode_res_section(An, Buffer, Ptr1) of
		{ok, {AnList, Ptr2}} ->
		    case decode_res_section(Ns, Buffer, Ptr2) of
			{ok, {NsList, Ptr3}} ->
			    case decode_res_section(Ar, Buffer, Ptr3) of
				{ok, {ArList,_Ptr4}} ->
				    {ok, {QdList,AnList,NsList,ArList}};
				Error -> Error
			    end;
			Error -> Error
		    end;
		Error -> Error
	    end;
	Error -> Error
    end.

%%
%% Decode queries
%%
decode_query_section(0, _, Ptr0) -> 
    { ok, {[], Ptr0}};
decode_query_section(N, Buffer, Ptr0) when N >= 0 ->
    decode_query_section(N, Buffer, Ptr0, []);
decode_query_section(_, _, _) -> 
    {error, fmt}.

decode_query_section(0, _, Ptr, Ls) -> 
    { ok, {reverse(Ls), Ptr}};
decode_query_section(Count, Buffer, Ptr0, Ls) ->
    case dn_expand(Ptr0, Buffer) of
	error -> 
	    {error, fmt};
	{Name,_} ->
	    case dn_skip(Ptr0) of
		[T0,T1,C0,C1 | Ptr1] ->
		    decode_query_section(
		      Count-1, Buffer, Ptr1,
		      [ #dns_query {
				domain = Name,
				type = decode_type(?i16(T0,T1)),
				class = decode_class(?i16(C0,C1))} | Ls]);
		_ -> {error, fmt}
	    end
    end.
%%
%% Decode resources
%%
decode_res_section(0, _, Ptr0) -> 
    { ok, {[], Ptr0}};
decode_res_section(N, Buffer, Ptr0) when N >= 0 ->
    decode_res_section(N, Buffer, Ptr0, []);
decode_res_section(_, _, _) -> 
    {error, fmt}.
    
decode_res_section(0, _, Ptr, Ls) -> 
    {ok, {reverse(Ls), Ptr}};
decode_res_section(Count, Buffer, Ptr0, Ls) ->
    case dn_expand(Ptr0, Buffer) of
	error -> 
	    {error, fmt};
	{Name,_} ->
	    case dn_skip(Ptr0) of
		[T1,T0,C1,C0,TTL3,TTL2,TTL1,TTL0,L1,L0 | Ptr1] ->
		    Len = ?i16(L1,L0),
		    case get_data(Len, Ptr1) of
			error -> {error, fmt};
			{Data, Ptr2} ->
			    Type = decode_type(?i16(T1,T0)),
			    Class =  decode_class(?i16(C1,C0)),
			    NData = decode_data(Type, Class, Data, Buffer),
			    decode_res_section(
			      Count-1, Buffer, Ptr2,
			      [
			       #dns_rr 
			       {
				domain = Name,
				type = Type,
				class = Class,
				ttl = ?i32(TTL3,TTL2,TTL1,TTL0),
				data = NData
			       } | Ls])
		    end;
		_ -> {error, fmt}
	    end
    end.



%%
%% Encode a user query
%%

encode(Q) ->
    H = encode_header(Q#dns_rec.header),
    Qd = length(Q#dns_rec.qdlist),
    An = length(Q#dns_rec.anlist),
    Ns = length(Q#dns_rec.nslist),
    Ar = length(Q#dns_rec.arlist),
    B0 = H ++ ?int16(Qd) ++ ?int16(An) ++ ?int16(Ns) ++ ?int16(Ar),
    {B1,Ptrs0} = encode_query_section(Q#dns_rec.qdlist, [], B0),
    {B2,Ptrs1} = encode_res_section(Q#dns_rec.anlist, Ptrs0, B1),
    {B3,Ptrs2} = encode_res_section(Q#dns_rec.nslist, Ptrs1, B2),
    {B4,_Ptrs3} = encode_res_section(Q#dns_rec.arlist, Ptrs2, B3),
    {ok, B4}.

encode_header(H) ->
    F1 = 
	(H#dns_header.qr bsl 7) bor
	(H#dns_header.opcode bsl 3) bor
	(H#dns_header.aa bsl 2) bor
	(H#dns_header.tc bsl 1) bor
	H#dns_header.rd,
    F0 = 
	(H#dns_header.ra bsl 7) bor
	(H#dns_header.pr bsl 6) bor
	H#dns_header.rcode,
    ?int16(H#dns_header.id) ++ [F1, F0].


encode_query_section([Q | Qs], Ptrs, Buffer) ->
    DName = Q#dns_query.domain,
    Type = encode_type(Q#dns_query.type),
    Class = encode_class(Q#dns_query.class),
    {NBuffer, NPtrs} = dn_compress(DName, Ptrs, Buffer),
    encode_query_section(Qs, NPtrs, NBuffer ++ ?int16(Type) ++ ?int16(Class));
encode_query_section([], Ptrs,Buffer) -> {Buffer, Ptrs}.


encode_res_section([R | Rs], Ptrs, Buffer) ->
    DName = R#dns_rr.domain,
    Type = encode_type(R#dns_rr.type),
    Class = encode_class(R#dns_rr.class),
    {NBuffer, NPtrs} = dn_compress(DName, Ptrs, Buffer),
    DataPtr = length(NBuffer) + 10, % length(i16, i16, i32, i16) = 10
    {Data,_Ptrs1} = encode_data(R#dns_rr.type,
				R#dns_rr.class,
				R#dns_rr.data,
				NPtrs,
				DataPtr),
    N = length(Data),
    encode_res_section(Rs, NPtrs, NBuffer ++ 
		       ?int16(Type) ++ ?int16(Class) ++
		       ?int32(R#dns_rr.ttl) ++
		       ?int16(N) ++ Data);
encode_res_section([], Ptrs, Buffer) -> {Buffer, Ptrs}.

%%
%% Resource types
%%
decode_type(Type) ->
    case Type of
	?T_A -> ?S_A;
	?T_NS -> ?S_NS;
	?T_MD -> ?S_MD;
	?T_MF -> ?S_MF;
	?T_CNAME -> ?S_CNAME;
	?T_SOA -> ?S_SOA;
	?T_MB  -> ?S_MB;
	?T_MG  -> ?S_MG;
	?T_MR  -> ?S_MR;
	?T_NULL -> ?S_NULL;
	?T_WKS  -> ?S_WKS;
	?T_PTR  -> ?S_PTR;
	?T_HINFO -> ?S_HINFO;
	?T_MINFO -> ?S_MINFO;
	?T_MX -> ?S_MX;
	?T_TXT -> ?S_TXT;
	?T_AAAA -> ?S_AAAA;
	?T_SRV -> ?S_SRV;
	%% non standard
	?T_UINFO -> ?S_UINFO;
	?T_UID -> ?S_UID;
	?T_GID -> ?S_GID;
	?T_UNSPEC -> ?S_UNSPEC;
	%% Query type values which do not appear in resource records
	?T_AXFR -> ?S_AXFR;
	?T_MAILB -> ?S_MAILB;
	?T_MAILA -> ?S_MAILA;
	?T_ANY  -> ?S_ANY;
	_ -> Type    %% raw unknown type
    end.

%%
%% Resource types
%%
encode_type(Type) ->
    case Type of
	?S_A -> ?T_A;
	?S_NS -> ?T_NS;
	?S_MD -> ?T_MD;
	?S_MF -> ?T_MF;
	?S_CNAME -> ?T_CNAME;
	?S_SOA -> ?T_SOA;
	?S_MB -> ?T_MB;
	?S_MG -> ?T_MG;
	?S_MR -> ?T_MR;
	?S_NULL -> ?T_NULL;
	?S_WKS -> ?T_WKS;
	?S_PTR -> ?T_PTR;
	?S_HINFO -> ?T_HINFO;
	?S_MINFO -> ?T_MINFO;
	?S_MX -> ?T_MX;
	?S_TXT -> ?T_TXT;
	?S_AAAA -> ?T_AAAA;
	?S_SRV -> ?T_SRV;
	%% non standard
	?S_UINFO -> ?T_UINFO;
	?S_UID -> ?T_UID;
	?S_GID -> ?T_GID;
	?S_UNSPEC -> ?T_UNSPEC;
	%% Query type values which do not appear in resource records
	?S_AXFR -> ?T_AXFR;
	?S_MAILB -> ?T_MAILB;
	?S_MAILA -> ?T_MAILA;
	?S_ANY -> ?T_ANY;
	Type when is_integer(Type) -> Type    %% raw unknown type
    end.

%%
%% Resource clases
%%

decode_class(Class) ->
    case Class of
	?C_IN -> in;
	?C_CHAOS ->  chaos;
	?C_HS -> hs;
	?C_ANY -> any;
	_ -> Class    %% raw unknown class
    end.


encode_class(Class) ->
    case Class of
	in -> ?C_IN;
	chaos -> ?C_CHAOS;
	hs -> ?C_HS;
	any -> ?C_ANY;
	Class when is_integer(Class) -> Class    %% raw unknown class
    end.

%%
%% Decode data field
%%
decode_data(?S_A,  in, [A,B,C,D], _)   -> {A,B,C,D};
decode_data(?S_AAAA,in, As, _) when length(As) =:= 16 ->
    [X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16] = As,
    { ?u16(X1,X2),?u16(X3,X4),?u16(X5,X6),?u16(X7,X8),
     ?u16(X9,X10),?u16(X11,X12),?u16(X13,X14),?u16(X15,X16)};
decode_data(?S_NS, _, Dom, Buffer)    -> decode_domain(Dom, Buffer);
decode_data(?S_MD, _, Dom, Buffer)    -> decode_domain(Dom, Buffer); 
decode_data(?S_MF, _, Dom, Buffer)    -> decode_domain(Dom, Buffer); 
decode_data(?S_CNAME, _, Dom, Buffer) -> decode_domain(Dom, Buffer);
decode_data(?S_SOA, _, Data, Buffer) ->
    case dn_expand(Data, Buffer) of
	error -> error;
	{MNAME, Data1} ->
	    case dn_expand(Data1, Buffer) of
		error -> error;
		{RNAME, Data2} ->
		    case Data2 of
			[S3,S2,S1,S0,
			 R3,R2,R1,R0,
			 Y3,Y2,Y1,Y0,
			 X3,X2,X1,X0,
			 M3,M2,M1,M0 | _] ->
			    {MNAME, RNAME, 
			     ?u32(S3,S2,S1,S0), ?i32(R3,R2,R1,R0),
			     ?i32(Y3,Y2,Y1,Y0), ?i32(X3,X2,X1,X0),
			     ?u32(M3,M2,M1,M0) };
			_ -> error
		    end
	    end
    end;
decode_data(?S_MB, _, Dom, Buffer)    -> decode_domain(Dom, Buffer); 
decode_data(?S_MG, _, Dom, Buffer)    -> decode_domain(Dom, Buffer); 
decode_data(?S_MR, _, Dom, Buffer)    -> decode_domain(Dom, Buffer); 
decode_data(?S_NULL, _, Data, _Buffer) -> Data;
decode_data(?S_WKS, in, [A,B,C,D, P | BitMap], _Buffer) -> 
    { {A,B,C,D}, P, BitMap};
decode_data(?S_PTR, _, Dom, Buffer)   -> decode_domain(Dom, Buffer);
decode_data(?S_HINFO, _, Data, _Buffer) ->
    {CPU, RData} = get_data(hd(Data), tl(Data)),
    {OS, _} = get_data(hd(RData), tl(RData)),
    {CPU, OS};
decode_data(?S_MINFO, _, Data, Buffer) ->
    case dn_expand(Data, Buffer) of
	error -> error;
	{RM, Data1} ->
	    case dn_expand(Data1, Buffer) of
		error -> error;
		{EM, _} -> {RM, EM}
	    end
    end;
decode_data(?S_MX, _, [P1,P0 | Dom], Buffer) ->
    { ?i16(P1,P0), decode_domain(Dom, Buffer) };
decode_data(?S_SRV, _, [P1,P0, W1,W0, Po1,Po0 | Dom], Buffer) ->
    { ?i16(P1,P0), ?i16(W1,W0), ?i16(Po1,Po0), decode_domain(Dom, Buffer) };
decode_data(?S_TXT, _, Data, _Buffer) -> Data;
%% sofar unknown or non standard
decode_data(_, _, Data, _Buffer) ->
    Data.

decode_domain(Data, Buffer) ->
    case dn_expand(Data, Buffer) of
	error -> error;
	{Dn, _} -> Dn
    end.

%%
%% Get N bytes from Ptr
%%
get_data(0, Ptr) -> {[], Ptr};
get_data(N, Ptr) when N > 0 -> get_data(N, Ptr, []);
get_data(_, _) -> error.

get_data(0, Ptr, Data) ->    {reverse(Data), Ptr};
get_data(N, [H|T], Data) ->  get_data(N-1, T, [H|Data]);
get_data(_, [], _) ->        error.

%%
%% Expand compressed domain names
%% Return expanded name and the tail of Dn or error
%%
dn_expand(Dn, Buffer) ->
    dn_exp(Dn, Buffer, []).

dn_exp([0 | T], _, []) ->  % Root domain
    {".", T};
dn_exp([0 | T], _, Name) ->
    {reverse(Name), T};
dn_exp([N | T], Buffer, Name) when N band ?INDIR_MASK =:= 0 ->
    if Name =:= [] ->
	    dn_exp_label(N, T, Name, Buffer);
       true ->
	    dn_exp_label(N, T, [$. | Name], Buffer)
    end;
dn_exp(_, Buffer, Name) when length(Name) > length(Buffer) -> 
    error;
dn_exp([N1,N2 | T], Buffer, Name) when N1 band ?INDIR_MASK =:= ?INDIR_MASK ->
    Offset = ((N1 band 16#3f) bsl 8) bor N2,
    case catch nthtail(Offset, Buffer) of
	{'EXIT', _} -> error;
	NDn -> 
	    %% We have to keep the Tail of original Dn in order to
	    %% prohibit ending up with the tail from an offset.
	    case dn_exp(NDn, Buffer, Name) of
		{ExpName, _} -> {ExpName, T};
		Res          -> Res
	    end
    end;
dn_exp([], _, _) ->
    error.

dn_exp_label(0, T, Name, Buffer) ->
    dn_exp(T, Buffer, Name);
dn_exp_label(N, [H|T], Name, Buffer) ->
    dn_exp_label(N-1, T, [H|Name], Buffer).

%%
%% Encode data field
%%
encode_data(?S_A, in, {A,B,C,D}, Ptrs, _)  -> {[A,B,C,D], Ptrs};
encode_data(?S_AAAA, in, As, Ptrs, _) when tuple_size(As) =:= 8 ->
    {X1,X2,X3,X4,X5,X6,X7,X8} = As,
    A = ?int16(X1) ++ ?int16(X2) ++ ?int16(X3) ++ ?int16(X4) ++ 
	?int16(X5) ++ ?int16(X6) ++ ?int16(X7) ++ ?int16(X8),
    {A, Ptrs};
encode_data(?S_NS, in, Domain, Ptrs, L)    -> dn_compress(Domain, Ptrs, [], L);
encode_data(?S_MD, in, Domain, Ptrs, L)    -> dn_compress(Domain, Ptrs, [], L);
encode_data(?S_MF, in, Domain, Ptrs, L)    -> dn_compress(Domain, Ptrs, [], L);
encode_data(?S_CNAME, in, Domain, Ptrs, L) -> dn_compress(Domain, Ptrs, [], L);
encode_data(?S_SOA, in, {MN, RN, S, Ref, Ret, E, M}, Ptrs, L) ->
    {B0, P0} = dn_compress(MN, Ptrs, [], L),
    {B1, P1} = dn_compress(RN, P0, B0, length(B0) + L),
    Data = B1 ++ ?int32(S) ++ ?int32(Ref) ++ ?int32(Ret) ++
	?int32(E) ++ ?int32(M),
    {Data, P1};
encode_data(?S_MB, in, Domain, Ptrs, L)    -> dn_compress(Domain, Ptrs, [], L);
encode_data(?S_MG, in, Domain, Ptrs, L)    -> dn_compress(Domain, Ptrs, [], L);
encode_data(?S_MR, in, Domain, Ptrs, L)    -> dn_compress(Domain, Ptrs, [], L);
encode_data(?S_NULL, in, Data, Ptrs, _)    -> {Data, Ptrs};
encode_data(?S_WKS, in, {{A,B,C,D},P,BitMap}, Ptrs, _) ->
    {[A,B,C,D,P|BitMap], Ptrs};
encode_data(?S_PTR, in, Domain, Ptrs, L)   -> dn_compress(Domain, Ptrs, [], L);
encode_data(?S_HINFO, in, {CPU, OS}, Ptrs, _) ->
    {[length(CPU)|CPU] ++ [length(OS)|OS], Ptrs};
encode_data(?S_MINFO, in, {RM, EM}, Ptrs, L) ->
    {B0, P0} = dn_compress(RM, Ptrs, [], L),
    dn_compress(EM, P0, B0, length(B0) + L);
encode_data(?S_MX, in, {Pref, Exch}, Ptrs, L) ->
    {EDom, NPtrs} = dn_compress(Exch, Ptrs, [], L),
    {?int16(Pref) ++ EDom, NPtrs};
encode_data(?S_SRV, in, {Prio, Weight, Port, Target}, Ptrs, L) ->
    {EDom, NPtrs} = dn_compress(Target, Ptrs, [], L),
    {?int16(Prio) ++ ?int16(Weight) ++ ?int16(Port) ++ EDom, NPtrs};
encode_data(?S_TXT, in, Data, Ptrs, _)     -> {Data, Ptrs};
%% sofar unknown or non standard
encode_data(_, _, Data, Ptrs, _)        -> {Data, Ptrs}.

%%
%% Compress a name given list names already compressed
%% The format of compressed names are
%% {Offset, Name}
%%
%% Return {NewBuffer, NewNames}
%% or   Error
%%
dn_compress("", Ns, Buffer) ->  %% Root domain
    dn_compress(".", Ns, Buffer);
dn_compress(Name, Ns, Buffer) ->
    {Buf, NNs} = dn_comp(Name, Ns, [], length(Buffer)),
    {Buffer ++ Buf, NNs}.

dn_compress(Name, Ns, Buffer, FullLength) ->
    {Buf, NNs} = dn_comp(Name, Ns, [], FullLength),
    {Buffer ++ Buf, NNs}.

dn_comp([], Ns0, Buf, _Offset) ->
    { Buf, Ns0};
dn_comp(Name, Ns0, Buf, Offset) ->
    case dn_find(Name, Ns0) of
	{true, Offs} ->
	    Ptr = [(Offs bsr 8) bor ?INDIR_MASK, Offs band 16#ff],
	    { Buf ++ Ptr, Ns0 };
	false ->
	    { Buf ++ dn_comp_labels(Name, []), [{Name, Offset} | Ns0] }
    end.

dn_comp_labels([$\\, 0 | _Garbage], Cn) ->
    dn_comp_labels([], Cn);
dn_comp_labels([$.], Cn) ->
    dn_comp_labels([], Cn);
dn_comp_labels([$. | Name], Cn) ->
    [length(Cn) | reverse(Cn, dn_comp_labels(Name, []))];
dn_comp_labels([$\\, C | Name], Cn) ->
    dn_comp_labels(Name, [C, $\\ | Cn]);
dn_comp_labels([C | Name], Cn) ->
    dn_comp_labels(Name, [C | Cn]);
dn_comp_labels([], Cn) ->
    [length(Cn) | reverse(Cn, [0])].
    
%%
%% Skip over a compressed domain name
%%
dn_skip([0|Dn]) ->
    Dn;
dn_skip([N|Dn]) when N band ?INDIR_MASK =:= 0 ->
    case catch nthtail(N, Dn) of
	{'EXIT', _} -> error;
	NDn -> dn_skip(NDn)
    end;
dn_skip([N1,_N2|Dn]) when N1 band ?INDIR_MASK =:= ?INDIR_MASK ->
    Dn;
dn_skip(_) -> error.

%%
%% Lookup a compressed name (not stored as compressed !!!)
%%

dn_find(Name, [{Nm, Offset} | Ns]) -> 
    case cmp_name(Name, Nm) of
	true -> {true, Offset};
	false -> dn_find(Name, Ns)
    end;
dn_find(_, []) -> false.


cmp_name(Name, Nm) when length(Name) =:= length(Nm) ->
    cmp_lower(Name, Nm);
cmp_name(_, _) -> false.

cmp_lower([H0|T0], [H1|T1]) ->
    HH0 = ?tolower(H0),
    HH1 = ?tolower(H1),
    if HH0 =:= HH1 -> cmp_lower(T0, T1);
       true -> false
    end;
cmp_lower([], []) -> true.

