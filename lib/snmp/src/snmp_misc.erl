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
-module(snmp_misc).

%% need definition of mib record
-include("snmp_types.hrl").

-define(VMODULE,"MISC").
-include("snmp_verbosity.hrl").


-export([aliasname/2,
	 aliasname_impl/3,
	 andApplyAll/2,
	 assq/2,
	 bits_to_int/2,
	 bits_to_int/3,
	 diff/2,
	 ensure_trailing_dir_delimiter/1,
	 error/2,
	 error/3,
	 filter/3,
	 foreach/3,
	 format_pdu/2,
	 format_val/4,
	 format_vb/2,
	 format_vbs/2,
	 format/3,
	 get_option/3,
	 get_sec_level/1,
	 get_vsns/0,
	 ip/1,
	 is_auth/1,
	 is_bitstring/1,
	 is_oid/1,
	 is_oid2/1,
	 is_priv/1,
	 is_reportable/1,
	 is_reportable_pdu/1,
	 is_string/1,
	 is_tag_member/2,
	 is_tmask_match/3,
	 keyreplaceadd/4,
	 load_mib/1,
	 loop/5,
	 make_mini_mib/1,
	 make_mini_mib_elem/1,
	 mem_size/1,
	 mk_msg_flags/2,
	 multi_map/2,
	 now/0,
	 now/1,
	 oid/2,
	 open_file/1,
	 p/2,
	 read/2,
	 read/3,
	 read_mib/1,
	 read_noexit/2,
	 remove_dubbletts/1,
	 set_option/3,
	 sleep/1,
	 str_xor/2,
	 strip_extension_from_filename/2,
	 symbolify_oid/2,
	 time/3,
	 to_upper/1,
	 type/2,
	 update_me/1,
	 update_trap/1]).

get_vsns() ->
    F = fun(Ver) ->
		case application:get_env(snmp, Ver) of
		    {ok, true} -> [Ver];
		    {ok, false} -> [];
		    {ok, Bad} -> exit({snmp, bad_type, {Ver, Bad}});
		    _ -> [Ver] % default is true
		end
	end,
    Vsn1 = F(v1),
    Vsn2 = F(v2),
    Vsn3 = F(v3),
    Vsn1 ++ Vsn2 ++ Vsn3.

sleep(Time) ->
    receive
	after Time ->
		true
    end.

to_upper([C|Cs]) when C >= $a, C =< $z -> [C-($a-$A)|to_upper(Cs)];
to_upper([C|Cs]) -> [C|to_upper(Cs)];
to_upper([]) -> [].

%% Returns time in ms = sec/1000
now() -> now(ms).
now(ms) ->
    Now = erlang:now(),
    element(1,Now)*1000000000+
	element(2,Now)*1000+
	(element(3,Now) div 1000);
%% Returns time in cs = sec/100
now(cs) ->
    Now = erlang:now(),
    element(1,Now)*100000000+
        element(2,Now)*100+
	(element(3,Now) div 10000);
now(sec) ->
    Now = erlang:now(),
    element(1,Now)*1000000+
        element(2,Now)+
	(element(3,Now) div 1000000).
    

andApplyAll(Module, []) -> true;
andApplyAll(Module, [{Func, Args} | FAs]) ->
    case apply(Module, Func, Args) of
	true -> andApplyAll(Module, FAs);
	Reason -> Reason
    end.

is_string([]) -> true;
is_string([Tkn | Str]) when integer(Tkn), Tkn >= 0, Tkn =< 255 ->
    is_string(Str);
is_string(X) -> false.


is_oid([E1, E2| Rest]) when length(Rest) =< 126, E1 *40 + E2 =< 255 ->
    is_oid2(Rest);
is_oid([E1]) when E1 =< 2 ->
    true;
is_oid(_) -> false.

is_oid2([]) -> true;
is_oid2([Nbr | RestOid]) when integer(Nbr), 0 =< Nbr, Nbr =< 2147483647 ->
    is_oid2(RestOid);
is_oid2(_) -> false.
    
is_bitstring([]) -> true;
is_bitstring([Nbr | RestBitstring]) when integer(Nbr), Nbr >= 0, Nbr =< 1 ->
    is_bitstring(RestBitstring);
is_bitstring(X) -> false.
    

%% Check if a Tag is a member in a TagList.  Tags and TagLists are defined
%% in SNMP-TARGET-MIB
is_tag_member(Tag, TagList) ->
    check_tag_list(TagList, [], lists:reverse(Tag)).

check_tag_list([32 | T], Res, Gat) ->
    tag_delimiter_found(Res, Gat, T);
check_tag_list([9 | T], Res, Gat) ->
    tag_delimiter_found(Res, Gat, T);
check_tag_list([13 | T], Res, Gat) ->
    tag_delimiter_found(Res, Gat, T);
check_tag_list([11 | T], Res, Gat) ->
    tag_delimiter_found(Res, Gat, T);
check_tag_list([Char | T], Res, Gat) ->
    check_tag_list(T, [Char | Res], Gat);
check_tag_list([], Res, Gat) ->
    tag_delimiter_found(Res, Gat, []).

tag_delimiter_found(Gat, Gat, T) ->
    true;
tag_delimiter_found(_Res, Gat, []) ->
    false;
tag_delimiter_found(_Res, Gat, T) ->
    check_tag_list(T, [], Gat).
    

%% Pre: length(TAddr1) == length(TAddr2)
%%      length(TMask) == 0 | length(TAddr1)
is_tmask_match(TAddr1, TAddr2, []) ->
    true;
is_tmask_match([H1 | T1], [H2 | T2], [M1 | M2]) ->
    if
	(H1 band M1) == (H2 band M1) ->
	    is_tmask_match(T1, T2, M2);
	true ->
	    false
    end.
    

%%--------------------------------------------------
%% Not a real assq, but what the heck, it's useful.
%%--------------------------------------------------
assq(Key, List) ->
    case lists:keysearch(Key, 1, List) of
	{value, {Key, Val}} -> {value, Val};
	_ -> false
    end.
    
get_option(Key, Options, Default) ->
    case lists:keysearch(Key, 1, Options) of
	{value, {_Key, Value}} -> Value;
	_ -> Default
    end.

set_option(Key, Val, Opts) ->
    keyreplaceadd(Key, 1, Opts, {Key, Val}).

keyreplaceadd(Key, Pos, List, New) ->
    case lists:keysearch(Key, Pos, List) of
	{value, _} -> lists:keyreplace(Key, Pos, List, New);
	_ -> [New | List]
    end.

is_auth(SecLevel) ->
    1 == (SecLevel band 1).

is_priv(SecLevel) ->
    2 == (SecLevel band 2).

is_reportable([MsgFlag]) ->
    4 == (MsgFlag band 4).

%% [OTP-3416] 
%% [RFC 2571] Confirmed Class: GetRequest-PDU, GetNextRequest-PDU, 
%% GetBulkRequest-PDU, SetRequest-PDU, and InformRequest-PDU. 
%% Unconfirmed Class: Report-PDU, Trapv2-PDU, and GetResponse-PDU. 
%% [RFC 2572] The reportableFlag MUST always be zero when the message 
%% contains a PDU from the Unconfirmed Class; it MUST always be one 
%% for a PDU from the Confirmed Class, 
%%
is_reportable_pdu('get-request') -> true;
is_reportable_pdu('get-next-request') -> true;
is_reportable_pdu('get-bulk-request') -> true;
is_reportable_pdu('set-request') -> true;
is_reportable_pdu('inform-request') -> true;
is_reportable_pdu(_) -> false.

mk_msg_flags(PduType, SecLevel) ->
    Flags1 = case is_reportable_pdu(PduType) of
		 true -> 4;
		 false -> 0
	     end,
    [Flags1 bor SecLevel].

get_sec_level([Flag]) ->
    SecLevel = Flag band 3,
    case {is_auth(SecLevel), is_priv(SecLevel)} of
	{false, false} -> noAuthNoPriv;
	{true, false} -> authNoPriv;
	{true, true} -> authPriv
    end.


%% diff(L1, L2) -> L1 - L2.
%% Ex. [1, 2, 3, 4] - [1, 3, 4] = [2, 3, 4]
diff(L1, []) -> L1;
diff([H | T1], [H | T2]) -> diff(T1, T2);
diff(L1, _) -> L1.

%% ExtraArgs behovs eftersom inte lambda finns.
filter(Pred, ExtraArgs, [H | T]) ->
    case apply(Pred, [H | ExtraArgs]) of
        true -> [H | filter(Pred, ExtraArgs, T)];
        false -> filter(Pred, ExtraArgs, T)
    end;
filter(Pred, ExtraArgs, []) -> [].
 
foreach(Function,ExtraArgs,[H | T]) ->
    apply(Function, [H | ExtraArgs]),
    foreach(Function, ExtraArgs, T);
foreach(Function,ExtraArgs,[]) -> true.


str_xor([H1|T1], [H2|T2]) ->
    [H1 bxor H2 | str_xor(T1, T2)];
str_xor([], []) ->
    [].

%%-----------------------------------------------------------------
%% Pre: ListOfLists is a list of N lists, each of length M.
%%      Func is a function of arity N.
%% Returns: A list of length M where element Y is the result of
%%          applying Func on [Elem(Y, List1), ..., Elem(Y, ListN)].
%%-----------------------------------------------------------------
multi_map(Func, [[] | ListOfLists]) -> [];
multi_map(Func, ListOfLists) ->
    [apply(Func, lists:map({erlang, hd}, [], ListOfLists)) |
     multi_map(Func, lists:map({erlang, tl}, [], ListOfLists))].

%% Primitive performance analysis.
time(M,F,A) ->
    statistics(runtime),
    R = apply(M, F, A),
    {R, statistics(runtime)}.

%% How much memory is allocated for X? At least some kind of upper estimation...
mem_size(X) ->
    E = ets:new(tmp, [set, protected]),
    M1 = ets:info(E, memory),
    ets:insert(E, {make_ref(), X}),
    M2 = ets:info(E, memory),
    ets:delete(E),
    M2 - M1.

%% safely prints the data (even if the format happens to be wrong)
p(Format, Data) ->
    case io:format(Format, Data) of
	ok -> ok;
	{error, R} ->
	    io:format("Error in format (~s) ~w.~n", [Format, Data]),
	    {error, R}
    end.

strip_extension_from_filename(FileName, Ext) when atom(FileName) ->
    strip_extension_from_filename(atom_to_list(FileName), Ext);

strip_extension_from_filename(FileName, Ext) when list(FileName) ->
    case lists:suffix(Ext, FileName) of
	true -> lists:sublist(FileName, 1, length(FileName) - length(Ext));
	false -> FileName
    end.

%%----------------------------------------------------------------------
%% Returns: {ok, Mib}|{error, Reason}
%% The reason for having the function if this module is:
%% The compiler package and the agent package are separated, this is
%% the only common module.
%%----------------------------------------------------------------------
read_mib(FileName) ->
    case file:read_file(FileName) of
	{ok, Bin} ->
	    MibInThisVer = #mib{},
	    case binary_to_term(Bin) of
		Mib1 when record(Mib1, mib) ->
		    Mib2 = update_mib(Mib1),
		    if 
			Mib2#mib.mib_format_version
			== MibInThisVer#mib.mib_format_version ->
			    case Mib2#mib.misc of
				X when integer(X) -> % old format
				    {ok, Mib2#mib{misc = []}};
				_ ->
				    {ok, Mib2}
			    end;
			true ->
			    {error, 'wrong mib format version tag'}
		    end;
		Q -> {error, 'bad format'}
	    end;
	{error, Reason} -> {error, Reason}
    end.


%% This function is used to update MEs, traps and notifications which
%% was changed as of release 3.2 (a description field was added).
%% E.g. old style records will be converted to the new style, with
%% the description field set the default value (the atom undefined).
update_mib(M) ->
    ?vtrace("update_mib -> entry",[]),
    case {update_mes(M#mib.mes),update_traps(M#mib.traps)} of
	{{true,Mes},{true,Traps}} ->
	    ?vinfo("converted some mes and traps for ~s",[M#mib.name]),
	    M#mib{mes = Mes, traps = Traps};
	{{true,Mes},false} ->
	    ?vinfo("converted some mes ~s",[M#mib.name]),
	    M#mib{mes = Mes};
	{false,{true,Traps}} ->
	    ?vinfo("converted some traps for ~s",[M#mib.name]),
	    M#mib{traps = Traps};
	{false,false} ->
	    ?vtrace("update_mib -> "
		    "~n   compiled with current version of the compiler",[]),
	    M
    end.
    
update_mes(Mes)     -> update_mib_data(Mes,update_me).
update_traps(Traps) -> update_mib_data(Traps,update_trap).

update_mib_data(Data,F) -> update_mib_data(Data,[],false,F).

update_mib_data([],_UpdatedData,false,_F) ->
    false;
update_mib_data([],UpdatedData,true,_F) ->
    {true,lists:reverse(UpdatedData)};
update_mib_data([D|Ds],UpdatedData,Updated,F) ->
    case apply(?MODULE,F,[D]) of
	{true,D1} ->
	    update_mib_data(Ds,[D1|UpdatedData],true,F);
	false ->
	    update_mib_data(Ds,[D|UpdatedData],Updated,F)
    end.

%% Old style (pre 3.2.0) me record, make a new one with default description
update_me({me, Oid, EntryType, AliasName, Asn1Type, Access, Mfa,
	   Imported, AssocList}) -> 
    ?vtrace("update_me -> convert old style me: "
	    "~n   Oid:       ~w"
	    "~n   AliasName: ~p",[Oid,AliasName]),
    Me = #me{oid       = Oid,
	     entrytype = EntryType, 
	     aliasname = AliasName, 
	     asn1_type = Asn1Type,
	     access    = Access , 
	     mfa       = Mfa, 
	     imported  = Imported,
	     assocList = AssocList},
    {true, Me};

% Up-to-date me
update_me(_Me) -> false.


%% Old style (pre 3.2.0) trap record, make a new one with default description
update_trap({trap, TrapName, EnterpriseOid, SpecificCode, OidObjects}) ->
    ?vtrace("update_trap -> update old style trap:"
	    "~n   EnterpriseOid: ~w"
	    "~n   Name:          ~p",[EnterpriseOid,TrapName]),
    Trap = #trap{trapname      = TrapName, 
		 enterpriseoid = EnterpriseOid,  
		 specificcode  = SpecificCode, 
		 oidobjects    = OidObjects},
    {true, Trap};

%% Old style (pre 3.2.0) notification record, make a new one with default 
%% description
update_trap({notification,TrapName,Oid,OidObjects}) ->
    ?vtrace("update_trap -> update old style notification:"
	    "~n   Oid:  ~w"
	    "~n   Name: ~p",[Oid,TrapName]),
    Notification = #notification{trapname   = TrapName, 
				 oid        = Oid,  
				 oidobjects = OidObjects},
    {true,Notification};

% Up-to-date trap or notification
update_trap(_Trap) -> false.



%%----------------------------------------------------------------------
%% Converts a list of named bits to the integer value.
%% Returns: integer()|error
%%----------------------------------------------------------------------
bits_to_int(Val,Kibbles) ->
    bits_to_int(Val,Kibbles,0).

bits_to_int([],Kibbles,Res) -> Res;
bits_to_int([Kibble|Ks],Kibbles,Res) ->
    case snmp_misc:assq(Kibble,Kibbles) of
	{value,V} ->
	    bits_to_int(Ks,Kibbles,Res + round(math:pow(2,V)));
	_ ->
	    error
    end.

			     
%%----------------------------------------------------------------------
%% Returns: {ok, {int(),int(),int(),int()}} | {error, Reason}
%%----------------------------------------------------------------------
ip(Host) ->
    inet:getaddr(Host, inet).

ensure_trailing_dir_delimiter([]) -> "/";
ensure_trailing_dir_delimiter(DirSuggestion) ->
    case lists:last(DirSuggestion) of
	$/ -> DirSuggestion;
	Q -> lists:append(DirSuggestion,"/")
    end.

%%%--------------------------------------------------
%%% The Mini MIB representation
%%%--------------------------------------------------

%% Returns a Mini MIB
make_mini_mib(Mibs) ->
    remove_dubbletts(lists:keysort(1,
			   lists:append(lists:map(fun load_mib/1, Mibs)))).

%%----------------------------------------------------------------------
%% Returns: A list of {Oid, Aliasname, Type}
%%----------------------------------------------------------------------
load_mib(MIB) ->
    F1 = snmp_misc:strip_extension_from_filename(MIB, ".bin"),
    ActualFileName = lists:append(F1, ".bin"),
    case snmp_misc:read_mib(ActualFileName) of
	{ok, #mib{mes = MEs, traps = Traps}} -> make_mini_mib_elem(MEs++Traps);
	{error, Reason} -> exit({error, {MIB,Reason}})
    end.

%%----------------------------------------------------------------------
%% Pre: List is sorted (dublettes are list neighbours)
%%----------------------------------------------------------------------
remove_dubbletts([X,X|T]) -> remove_dubbletts([X|T]);
remove_dubbletts([X|T]) -> [X|remove_dubbletts(T)];
remove_dubbletts([]) -> [];
remove_dubbletts([X]) -> [X].

%%----------------------------------------------------------------------
%% Args: A list if Mes
%% Returns: a list of {Oid, Aliasname, Type}
%%----------------------------------------------------------------------
make_mini_mib_elem([]) -> [];
make_mini_mib_elem([#me{aliasname = N, oid = Oid, entrytype = variable,
			 asn1_type = #asn1_type{bertype = Type}} | T]) ->
    [{Oid, N, Type} | make_mini_mib_elem(T)];
make_mini_mib_elem([#me{aliasname = N, oid = Oid, entrytype = table_column,
			 asn1_type = ASN1}|T]) when record(ASN1, asn1_type)->
    [{Oid, N, ASN1#asn1_type.bertype} | make_mini_mib_elem(T)];
make_mini_mib_elem([#me{aliasname = N, oid = Oid,
			 asn1_type = undefined}|T]) ->
    [{Oid, N, undefined} | make_mini_mib_elem(T)];
make_mini_mib_elem([#notification{trapname = N, oid = Oid}|T]) ->
    [{Oid, N, undefined} | make_mini_mib_elem(T)];
make_mini_mib_elem([_|T]) ->
    make_mini_mib_elem(T).


%% returns: Oid|false
oid(MiniMib, AliasName) ->
    case lists:keysearch(AliasName, 2, MiniMib) of
	{value, {Oid, Aliasname, _Type}} -> Oid;
	false -> false
    end.

%% returns: Type|false
type(MiniMib, Oid) ->
    {_Oid, _Name, Type} = aliasname(MiniMib, Oid),
    Type.

%%----------------------------------------------------------------------
%% Returns: false | {Oid, Aliasname, Type}
%%----------------------------------------------------------------------
aliasname(MiniMib, Oid) ->
    aliasname_impl(MiniMib, Oid, false).

%%----------------------------------------------------------------------
%% Returns: false | {Oid, Aliasname, Type}
%%----------------------------------------------------------------------
aliasname_impl([], _Oid, Res) -> Res;
aliasname_impl([{Oid, Aliasname, Type}|T], OidX, Res) when Oid =< OidX ->
    case lists:prefix(Oid, OidX) of
	true ->
	    aliasname_impl(T, OidX, {Oid, Aliasname, Type});
	false ->
	    aliasname_impl(T, OidX, Res)
    end;
aliasname_impl([{Oid, Aliasname, Type}|T], OidX, Res) ->
    Res.

format_pdu(PDU, MiniMib) when record(PDU, pdu) ->
    #pdu{type=T, error_status=ES, error_index=EI,
	 request_id=RID,varbinds=VBs}=PDU,
    Txt1 = if
	       ES == noError, EI == 0 -> "";
	       true ->
		   io_lib:format("*!*!* An error occured. *!*!* ~n"
				 "Error status = ~w, index = ~w.~n",
				 [ES, EI])
	   end,
    Txt2 = if T=='snmpv2-trap' ->
		   io_lib:format("v2 Trap,          Request Id:~w~n", [RID]);
	      T == 'get-request' ->
		   io_lib:format("Get request,      Request Id:~w~n", [RID]);
	      T == 'get-next-request' ->
		   io_lib:format("Get-Next request, Request Id:~w~n", [RID]);
	      T == 'get-bulk-request' ->
		   io_lib:format("Get-Bulk request, Request Id:~w~n", [RID]);
	      T == 'set-request' ->
		   io_lib:format("Set request,      Request Id:~w~n", [RID]);
	      T == 'get-response' ->
		   io_lib:format("Response,         Request Id:~w~n", [RID]);
	      T == 'inform-request' ->
		   io_lib:format("Inform Request    Request Id:~w~n", [RID]);
	      T == report ->
		   io_lib:format("Report            Request Id:~w~n", [RID]);
	      true -> ""
	   end,
    [Txt1,Txt2,format_vbs(VBs, MiniMib)|"\n"];

format_pdu(#trappdu{enterprise = Enterprise, agent_addr = AgentAddr,
		  generic_trap = GenericTrap, specific_trap = SpecificTrap,
		  time_stamp = TimeStamp, varbinds = VBs}, MiniMib) ->
    [io_lib:format("v1 Trap~n"
		   "     Generic: ~w~n"
		   "  Enterprise: ~w~n"
		   "    Specific: ~w~n"
		   "  Agent addr: ~w~n"
		   "   TimeStamp: ~w~n",
		   [GenericTrap,
		    element(1,symbolify_oid(MiniMib,Enterprise)),SpecificTrap,
		    AgentAddr, TimeStamp]),
     format_vbs(VBs, MiniMib) | "\n"].

format_vbs(Vbs, MiniMib) ->
    lists:map(fun (VB) -> format_vb(VB,MiniMib) end, Vbs).

format_vb(#varbind{oid = Oid, variabletype = Type, value = Value}, MiniMib) ->
    {Soid, Mtype} = symbolify_oid(MiniMib, Oid),
    [io_lib:format("  ~w = ", [Soid]),
     format_val(Type, Mtype, Value, MiniMib) | "\n"].

format(Max, F, A) when integer(Max) ->
    case lists:flatten(io_lib:format(F,A)) of
	S when length(S) > Max ->
	    case lists:suffix("\n", S) of
		true ->
		    lists:concat([lists:sublist(S,Max), "...\n"]);
		false ->
		    lists:concat([lists:sublist(S,Max), "..."])
	    end;
	S ->
	    S
    end.


%%----------------------------------------------------------------------
%% Returns: (a nested) symbolified oid.
%%----------------------------------------------------------------------
symbolify_oid(MiniMib, Oid) ->
    case snmp_misc:aliasname(MiniMib, Oid) of
	false ->
	    {Oid, unknown};
	{FoundOid, Aliasname, Type} ->
 	    Rest = snmp_misc:diff(Oid, FoundOid),
 	    {[Aliasname| Rest], Type}
    end.

format_val('OCTET STRING', 'BITS', Val, MiniMib) ->
    io_lib:format("~w", [snmp_pdus:octet_str_to_bits(Val)]);
format_val('OBJECT IDENTIFIER', _, Val, MiniMib) ->
    {NVal, _} = symbolify_oid(MiniMib, Val),
    io_lib:format("~w", [NVal]);
format_val(_, _, Val, MiniMib) ->
    io_lib:format("~p", [Val]).
    

%% Ret. Res | exit(Error)
read(File, CheckFunc) -> 
    ?vtrace("~n   read file  '~s'"
	    "~n   check with '~p'",[File,CheckFunc]),
    Fd = open_file(File),
    case loop(Fd, [], CheckFunc, 1, File) of
	{error, Line, R} ->	
	    ?vdebug("failed reading from file:"
		    "~n   Line: ~p"
		    "~n   R:    ~p",[Line,R]),
	    file:close(Fd),
	    error(File, Line-1, R);
	Res ->
	    ?vtrace("done with: ~p",[Res]),
	    file:close(Fd),
	    Res
    end.

open_file(File) ->
    case file:open(File, read) of
	{ok, Fd} -> Fd;
	Error ->
	    error("Cannot open file ~p", [File])
    end.

%% Ret. {ok, Res} | {error, Line, Error} | {error, open_file}
read_noexit(File, CheckFunc) ->
    case file:open(File, read) of
	{ok, Fd} ->
	    case loop(Fd, [], CheckFunc, 1, File) of
		{error, Line, R} ->
		    file:close(Fd),
		    {error, Line, R};
		Res ->
		    file:close(Fd),
		    {ok, Res}
	    end;
	Error ->
	    {error, open_file}
    end.

%%-----------------------------------------------------------------
%% Ret: {error, Line, Reason} | Row
%%-----------------------------------------------------------------
loop(Fd, Res, Func, StartLine, File) ->
    case read(Fd, "", StartLine) of
	{ok, Row, EndLine} ->
	    case catch apply(Func, [Row]) of
		{ok, NewRow} ->
		    loop(Fd, [NewRow | Res], Func, EndLine, File);
		true -> 
		    loop(Fd, [Row | Res], Func, EndLine, File);
		Error ->
		    {error, EndLine, Error}
	    end;
	{error, EndLine, Error} ->
	    {error, EndLine, Error};
	eof ->
	    Res
    end.

%%-----------------------------------------------------------------
%% io:read modified to give us line numbers.
%%-----------------------------------------------------------------
read(Io, Prompt, StartLine) ->
    case io:request(Io, {get_until,Prompt,erl_scan,tokens,[StartLine]}) of
	{ok,Toks,EndLine} ->
	    case erl_parse:parse_term(Toks) of
		{ok, Term} ->
		    {ok, Term, EndLine};
		{error,{Line,erl_parse,Error}} ->
		    {error, Line, {parse_error, Error}}
	    end;
	{error,E,EndLine} ->
	    {error,EndLine,E};
	{eof,EndLine} ->
	    eof;
	Other ->
	    Other
    end.

error(Format, Error) ->
    config_err(Format, Error),
    exit(configuration_error).
error(File, Line, Error) ->
    config_err("~p:~w:\n  ~p", [File, Line, Error]),
    exit(configuration_error).


config_err(F, A) ->
    snmp_error_report:config_err(F, A).
