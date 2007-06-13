-module(snmpc_mib_gram).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("snmpc_mib_gram.yrl", 621).
%%----------------------------------------------------------------------

-include("snmp_types.hrl").
-include("snmpc.hrl").

% value
val(Token) -> element(3, Token).

line_of(Token) -> element(2, Token).

%% category
cat(Token) -> element(1, Token). 

statusv1(Tok) ->
    case val(Tok) of
        mandatory -> mandatory;
        optional -> optional;
        obsolete -> obsolete;
        deprecated -> deprecated;
        Else -> return_error(line_of(Tok),
                             "syntax error before: " ++ atom_to_list(Else))
    end.

statusv2(Tok) ->
    case val(Tok) of
        current -> current;
        deprecated -> deprecated;
        obsolete -> obsolete;
        Else -> return_error(line_of(Tok),
                             "syntax error before: " ++ atom_to_list(Else))
    end.

accessv1(Tok) ->
    case val(Tok) of
        'read-only' -> 'read-only';
        'read-write' -> 'read-write';
        'write-only' -> 'write-only';
        'not-accessible' -> 'not-accessible';
        Else -> return_error(line_of(Tok),
                             "syntax error before: " ++ atom_to_list(Else))
    end.

accessv2(Tok) ->
    case val(Tok) of
        'not-accessible' -> 'not-accessible';
        'accessible-for-notify' -> 'accessible-for-notify';
        'read-only' -> 'read-only';
        'read-write' -> 'read-write';
        'read-create' -> 'read-create';
        Else -> return_error(line_of(Tok),
                             "syntax error before: " ++ atom_to_list(Else))
    end.

%% ---------------------------------------------------------------------
%% Various basic record build functions
%% ---------------------------------------------------------------------

make_module_identity(Name, LU, Org, CI, Desc, Revs, NA) ->
    #mc_module_identity{name         = Name,
                        last_updated = LU,
	                organization = Org,
	                contact_info = CI,
	                description  = Desc,
	                revisions    = Revs, 
	                name_assign  = NA}.

make_revision(Rev, Desc) ->
    #mc_revision{revision    = Rev,
	         description = Desc}.

make_object_type(Name, Syntax, MaxAcc, Status, Desc, Ref, Kind, NA) ->
    #mc_object_type{name        = Name,
                    syntax      = Syntax,
	            max_access  = MaxAcc,
	            status      = Status,
	            description = Desc,
	            reference   = Ref,
	            kind        = Kind, 
	            name_assign = NA}.

make_object_type(Name, Syntax, Units, MaxAcc, Status, Desc, Ref, Kind, NA) ->
    #mc_object_type{name        = Name,
                    syntax      = Syntax, 
                    units       = Units, 
	            max_access  = MaxAcc,
	            status      = Status,
	            description = Desc,
	            reference   = Ref,
	            kind        = Kind, 
	            name_assign = NA}.

make_new_type(Name, Macro, Syntax) ->
    #mc_new_type{name   = Name, 
	         macro  = Macro,
                 syntax = Syntax}.

make_new_type(Name, Macro, DisplayHint, Status, Desc, Ref, Syntax) ->
    #mc_new_type{name         = Name, 
	         macro        = Macro,
                 status       = Status,
                 description  = Desc,
                 reference    = Ref,
	         display_hint = DisplayHint,
                 syntax       = Syntax}.

make_trap(Name, Ent, Vars, Desc, Ref, Num) ->
    #mc_trap{name        = Name,
             enterprise  = Ent,
             vars        = Vars,
             description = Desc,
	     reference   = Ref,
	     num         = Num}.

make_notification(Name, Vars, Status, Desc, Ref, NA) ->
    #mc_notification{name        = Name,
                     vars        = Vars,
                     status      = Status,
                     description = Desc,
	             reference   = Ref,
	             name_assign = NA}.

make_module_compliance(Name, Status, Desc, Ref, Mod, NA) ->
    #mc_module_compliance{name        = Name,
                          status      = Status,
                          description = Desc,
	                  reference   = Ref,
                          module      = Mod,
	                  name_assign = NA}.

make_object_group(Name, Objs, Status, Desc, Ref, NA) ->
    #mc_object_group{name        = Name,
                     objects     = Objs,
                     status      = Status,
                     description = Desc,
	             reference   = Ref,
	             name_assign = NA}.

make_notification_group(Name, Objs, Status, Desc, Ref, NA) ->
    #mc_notification_group{name        = Name,
                           objects     = Objs,
                           status      = Status,
                           description = Desc,
	                   reference   = Ref,
	                   name_assign = NA}.

make_sequence(Name, Fields) ->
    #mc_sequence{name   = Name, 
                 fields = Fields}.

make_internal(Name, Macro, Parent, SubIdx) ->
    #mc_internal{name      = Name, 
                 macro     = Macro, 
                 parent    = Parent, 
                 sub_index = SubIdx}.



%% ---------------------------------------------------------------------


%%----------------------------------------------------------------------
%% Purpose: Find how much room needs to be allocated for the data type
%%          (when sending it in a PDU (the maximum difference will be 
%%           the size allocated)).
%%          This is applicable for OCTET STRINGs and OBJECT IDENTIFIERs.
%%
%%     Or : Find the range of integers in the integer list.
%%          This is applicable for INTEGERs
%%
%% Arg: A list of integers.
%%----------------------------------------------------------------------

make_range_integer(RevHexStr, h) ->
    erlang:list_to_integer(lists:reverse(RevHexStr), 16);
make_range_integer(RevHexStr, 'H') ->
    erlang:list_to_integer(lists:reverse(RevHexStr), 16);
make_range_integer(RevBitStr, b) ->
    erlang:list_to_integer(lists:reverse(RevBitStr), 2);
make_range_integer(RevBitStr, 'B') ->
    erlang:list_to_integer(lists:reverse(RevBitStr), 2);
make_range_integer(RevStr, Base) ->
    throw({error, {invalid_base, Base, lists:reverse(RevStr)}}).

make_range(XIntList) ->
    IntList = lists:flatten(XIntList),
    {range, lists:min(IntList), lists:max(IntList)}.

make_defval_for_string(Line, Str, Atom) ->
    case lists:member(Atom, [h, 'H', b, 'B']) of
	true ->
	    case catch make_defval_for_string2(Str, Atom) of
		Defval when list(Defval) ->
		    Defval;
		{error, ErrStr} ->
		    snmpc_lib:print_error("Bad DEFVAL ~w string ~p - ~s",
						 [Atom, Str, ErrStr],
						 Line),
		    "";
		_Else ->
		    snmpc_lib:print_error("Bad DEFVAL ~w string ~p",
						 [Atom, Str],
						 Line),
		    ""
	    end;
	false ->
	    snmpc_lib:print_error("Bad DEFVAL string type ~w for ~p",
					 [Atom, Str],
					 Line),
	    ""
    end.
	    

make_defval_for_string2([], h) -> [];
make_defval_for_string2([X16,X|HexString], h) ->
    lists:append(hex_to_bytes(snmpc_misc:to_upper([X16,X])),
		 make_defval_for_string2(HexString, h));
make_defval_for_string2([_Odd], h) ->
    throw({error, "odd number of bytes in hex string"});
make_defval_for_string2(HexString, 'H') ->
    make_defval_for_string2(HexString,h);

make_defval_for_string2(BitString, 'B') ->
    bits_to_bytes(BitString);
make_defval_for_string2(BitString, b) ->
    make_defval_for_string2(BitString, 'B').

bits_to_bytes(BitStr) ->
    lists:reverse(bits_to_bytes(lists:reverse(BitStr), 1, 0)).

bits_to_bytes([], 1, _Byte) ->   % empty bitstring
    [];
bits_to_bytes([], 256, _Byte) -> % correct; multiple of 8
    [];
% If we are to support arbitrary length of bitstrings.  This migth
% be needed in the new SMI.
%bits_to_bytes([], N, Byte) ->
%    [Byte];
bits_to_bytes([], _N, _Byte) ->
    throw({error, "not a multiple of eight bits in bitstring"});
bits_to_bytes(Rest, 256, Byte) ->
    [Byte | bits_to_bytes(Rest, 1, 0)];
bits_to_bytes([$1 | T], N, Byte) ->
    bits_to_bytes(T, N*2, N + Byte);
bits_to_bytes([$0 | T], N, Byte) ->
    bits_to_bytes(T, N*2, Byte);
bits_to_bytes([_BadChar | _T], _N, _Byte) ->
    throw({error, "bad character in bit string"}).

%%----------------------------------------------------------------------
%% These HEX conversion routines are stolen from module asn1_bits by 
%% klacke@erix.ericsson.se
%% I didn't want to ship the entire asn1-compiler so I used cut-and-paste.
%%----------------------------------------------------------------------

%% hex_to_bytes(HexNumber) when is_atom(HexNumber) ->
%%     hex_to_bytes(atom_to_list(HexNumber));

hex_to_bytes(HexNumber) ->
    case length(HexNumber) rem 2 of
	1 ->  %% Odd
	    hex_to_bytes(lists:append(HexNumber,[$0]),[]);
	0 ->  %% even
	    hex_to_bytes(HexNumber,[])
    end.

hex_to_bytes([],R) ->
    lists:reverse(R);
hex_to_bytes([Hi,Lo|Rest],Res) ->
    hex_to_bytes(Rest,[hex_to_byte(Hi,Lo)|Res]).

hex_to_four_bits(Hex) ->
    if
	Hex == $0 -> 0;
	Hex == $1 -> 1;
	Hex == $2 -> 2;
	Hex == $3 -> 3;
	Hex == $4 -> 4;
	Hex == $5 -> 5;
	Hex == $6 -> 6;
	Hex == $7 -> 7;
	Hex == $8 -> 8;
	Hex == $9 -> 9;
	Hex == $A -> 10;
	Hex == $B -> 11;
	Hex == $C -> 12;
	Hex == $D -> 13;
	Hex == $E -> 14;
	Hex == $F -> 15;
	true -> throw({error, "bad hex character"})
    end.

hex_to_byte(Hi,Lo) ->
    (hex_to_four_bits(Hi) bsl 4) bor hex_to_four_bits(Lo).

kind(DefValPart,IndexPart) ->
    case DefValPart of
	undefined ->
	    case IndexPart of
		{indexes, undefined} -> {variable, []};
		{indexes, Indexes}  ->
		    {table_entry, {indexes, Indexes}};
		{augments,Table} ->
		    {table_entry,{augments,Table}}
	    end;
	{defval, DefVal} -> {variable, [{defval, DefVal}]}
    end.    

display_hint(Val) ->
    case val(Val) of
        Str when list(Str) ->
            lists:reverse(Str);
        _ ->
            throw({error, {invalid_display_hint, Val}})
    end.

units(Val) ->
    case val(Val) of
        Str when list(Str) ->
            lists:reverse(Str);
        _ ->
            throw({error, {invalid_units, Val}})
    end.

ensure_ver(Ver, Line, What) ->
    case get(snmp_version) of
	Ver -> ok;
	_Other ->
	    snmpc_lib:print_error(
	      "~s is only allowed in SNMPv~p.",[What,Ver],Line)
    end.


ensure_ver(Ver,Token) ->
    ensure_ver(Ver,line_of(Token), atom_to_list(cat(Token))).

filter_v2imports(2,'Integer32')  -> {builtin, 'Integer32'};
filter_v2imports(2,'Counter32')  -> {builtin, 'Counter32'};
filter_v2imports(2,'Gauge32')    -> {builtin, 'Gauge32'};
filter_v2imports(2,'Unsigned32') -> {builtin, 'Unsigned32'};
filter_v2imports(2,'Counter64')  -> {builtin, 'Counter64'};
filter_v2imports(_,Type)         -> {type, Type}.
    
w(F, A) ->
    snmpc_lib:w(F, A).

%i(F, A) ->
%    io:format("~w:" ++ F ++ "~n", [?MODULE|A]).


-file("/ldisk/daily_build/otp_prebuild_r11b.2007-06-11_19/otp_src_R11B-5/bootstrap/lib/parsetools/include/yeccpre.hrl", 0).
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
%%     $Id $
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

parse(Tokens) ->
    yeccpars0(Tokens, false).

parse_and_scan({F, A}) -> % Fun or {M, F}
    yeccpars0([], {F, A});
parse_and_scan({M, F, A}) ->
    yeccpars0([], {{M, F}, A}).

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
	true ->
	    Message;
	_ ->
	    io_lib:write(Message)
    end.

% To be used in grammar files to throw an error message to the parser
% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function,{return_error,2}}).
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

yeccpars0(Tokens, MFA) ->
    try yeccpars1(Tokens, MFA, 0, [], [])
    catch 
        throw: {error, {_Line, ?MODULE, _M}} = Error -> 
                   Error % probably from return_error/1
    end.

% Don't change yeccpars1/6 too much, it is called recursively by yeccpars2/8!
yeccpars1([Token | Tokens], Tokenizer, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens,
	      Tokenizer);
yeccpars1([], {F, A}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, _Endline} ->
	    yeccpars1(Tokens, {F, A}, State, States, Vstack);
        {eof, _Endline} ->
            yeccpars1([], false, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], false, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, {'$end', 999999}, [], false).

% For internal use only.
yeccerror(Token) ->
    {error,
     {element(2, Token), ?MODULE,
      ["syntax error before: ", yecctoken2string(Token)]}}.

yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format('~s', [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:format('~w', [A]);
yecctoken2string({_Cat, _, Val}) -> io_lib:format('~w', [Val]);
yecctoken2string({'dot', _}) -> io_lib:format('~w', ['.']);
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:format('~w', [Other]);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("./snmpc_mib_gram.erl", 446).

yeccpars2(0, variable, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(1, 'DEFINITIONS', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(2, '$end', _, __Stack, _, _, _) ->
 {ok, hd(__Stack)};
yeccpars2(2, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(3, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_3_(__Stack),
 yeccpars2(yeccgoto(mibname, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(4, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '::=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(5, 'BEGIN', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(6, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(7, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(implies, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(8, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [8 | __Ss], [__T | __Stack]);
yeccpars2(8, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(9, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_9_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(implies, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(10, 'IMPORTS', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [10 | __Ss], [__T | __Stack]);
yeccpars2(10, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_10_(__Stack),
 yeccpars2(11, __Cat, [10 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(11, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, variable, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(12, 'AutonomousType', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'Counter', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'DateAndTime', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'DisplayString', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'Gauge', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'InstancePointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'IpAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'MODULE-COMPLIANCE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'MODULE-IDENTITY', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'MacAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'NOTIFICATION-GROUP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'NOTIFICATION-TYPE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'NetworkAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'OBJECT-GROUP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'OBJECT-IDENTITY', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'OBJECT-TYPE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'Opaque', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'PhysAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'RowPointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'RowStatus', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'StorageType', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'TAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'TDomain', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'TEXTUAL-CONVENTION', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'TRAP-TYPE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'TestAndIncr', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'TimeInterval', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'TimeStamp', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'TimeTicks', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'TruthValue', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, 'VariablePointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, variable, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(13, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, 'FROM', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(14, 'AutonomousType', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'Counter', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'DateAndTime', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'DisplayString', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'Gauge', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'InstancePointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'IpAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'MODULE-COMPLIANCE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'MODULE-IDENTITY', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'MacAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'NOTIFICATION-GROUP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'NOTIFICATION-TYPE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'NetworkAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'OBJECT-GROUP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'OBJECT-IDENTITY', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'OBJECT-TYPE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'Opaque', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'PhysAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'RowPointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'RowStatus', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'StorageType', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'TAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'TDomain', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'TEXTUAL-CONVENTION', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'TRAP-TYPE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'TestAndIncr', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'TimeInterval', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'TimeStamp', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'TimeTicks', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'TruthValue', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'VariablePointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, variable, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_14_(__Stack),
 yeccpars2(yeccgoto(imports, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(15, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(16, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_16_(__Stack),
 yeccpars2(yeccgoto(listofimports, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(17, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_17_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(18, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_18_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(19, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_19_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(20, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_20_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(21, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_21_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(22, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_22_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(23, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_23_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(24, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_24_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(25, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_25_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(26, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_26_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(27, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_27_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(28, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_28_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(29, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_29_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(30, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_30_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(31, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_31_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(32, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_32_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(33, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_33_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(34, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_34_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(35, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_35_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(36, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_36_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(37, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_37_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(38, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_38_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(39, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_39_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(40, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_40_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(41, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_41_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(42, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_42_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(43, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_43_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(44, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_44_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(45, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_45_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(46, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_46_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(47, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_47_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(48, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_48_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(49, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_49_(__Stack),
 yeccpars2(yeccgoto(import_stuff, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(50, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_50_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(import, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(51, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_51_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(imports, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(52, 'AutonomousType', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'Counter', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'DateAndTime', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'DisplayString', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'Gauge', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'InstancePointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'IpAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'MODULE-COMPLIANCE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'MODULE-IDENTITY', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'MacAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'NOTIFICATION-GROUP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'NOTIFICATION-TYPE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'NetworkAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'OBJECT-GROUP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'OBJECT-IDENTITY', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'OBJECT-TYPE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'Opaque', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'PhysAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'RowPointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'RowStatus', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'StorageType', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'TAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'TDomain', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'TEXTUAL-CONVENTION', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'TRAP-TYPE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'TestAndIncr', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'TimeInterval', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'TimeStamp', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'TimeTicks', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'TruthValue', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'VariablePointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, variable, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(53, variable, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(54, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_54_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(imports_from_one_mib, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(55, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_55_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(listofimports, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(56, 'END', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 398, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(57, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(definition, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(58, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(definition, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(59, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(definition, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(60, 'OBJECT', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 227, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, 'OBJECT-TYPE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 360, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, 'TRAP-TYPE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 361, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(61, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(definition, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(62, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [62 | __Ss], [__T | __Stack]);
yeccpars2(62, '::=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [62 | __Ss], [__T | __Stack]);
yeccpars2(62, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(63, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(definition, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(64, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_64_(__Stack),
 yeccpars2(109, __Cat, [64 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(65, 'MODULE-IDENTITY', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(66, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, variable, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_66_(__Stack),
 yeccpars2(yeccgoto(v1orv2, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(67, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_67_(__Stack),
 yeccpars2(yeccgoto(listofdefinitions, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(68, 'MODULE-IDENTITY', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_68_MODULE-IDENTITY'(__Stack),
 yeccpars2(yeccgoto(mibid, hd(__Ss)), 'MODULE-IDENTITY', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(68, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_68_(__Stack),
 yeccpars2(yeccgoto(objectname, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(69, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_69_(__Stack),
 yeccpars2(yeccgoto(newtypename, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(70, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_70_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(listofdefinitions, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(71, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_71_(__Stack),
 yeccpars2(yeccgoto(objectname, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(72, 'LAST-UPDATED', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(73, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(74, 'ORGANIZATION', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(75, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_75_(__Stack),
 yeccpars2(yeccgoto(last_updated, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(76, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(77, 'CONTACT-INFO', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 79, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(78, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_78_(__Stack),
 yeccpars2(yeccgoto(oranization, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(79, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 81, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(80, 'DESCRIPTION', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 82, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(81, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_81_(__Stack),
 yeccpars2(yeccgoto(contact_info, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(82, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 84, [82 | __Ss], [__T | __Stack]);
yeccpars2(82, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_82_(__Stack),
 yeccpars2(83, __Cat, [82 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(83, 'REVISION', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 88, [83 | __Ss], [__T | __Stack]);
yeccpars2(83, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_83_(__Stack),
 yeccpars2(86, __Cat, [83 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(84, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_84_(__Stack),
 yeccpars2(yeccgoto(descriptionfield, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(85, 'REVISION', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 88, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_85_(__Stack),
 yeccpars2(yeccgoto(revisionpart, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(86, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, '::=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(87, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_87_(__Stack),
 yeccpars2(yeccgoto(revisions, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(88, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 90, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(89, 'DESCRIPTION', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 91, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(90, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_90_(__Stack),
 yeccpars2(yeccgoto(revision_string, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(91, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 93, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(92, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_92_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(revision, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(93, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_93_(__Stack),
 yeccpars2(yeccgoto(revision_desc, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(94, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_94_(__Stack),
 __Nss = lists:nthtail(11, __Ss),
 yeccpars2(yeccgoto(moduleidentity, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(95, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 96, [95 | __Ss], [__T | __Stack]);
yeccpars2(95, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(96, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(97, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(fatherobjectname, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(98, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(99, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(100, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(101, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_101_(__Stack),
 yeccpars2(yeccgoto(parentintegers, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(102, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_102_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(parentintegers, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(103, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [103 | __Ss], [__T | __Stack]);
yeccpars2(103, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(104, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(105, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_105_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(parentintegers, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(106, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_106_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(parentintegers, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(107, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_107_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(nameassign, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(108, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_108_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(revisions, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(109, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, variable, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_109_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(v1orv2, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(110, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(definitionv2, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(111, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(definitionv2, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(112, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(definitionv2, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(113, 'MODULE-COMPLIANCE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 224, [113 | __Ss], [__T | __Stack]);
yeccpars2(113, 'NOTIFICATION-GROUP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 225, [113 | __Ss], [__T | __Stack]);
yeccpars2(113, 'NOTIFICATION-TYPE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 226, [113 | __Ss], [__T | __Stack]);
yeccpars2(113, 'OBJECT', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 227, [113 | __Ss], [__T | __Stack]);
yeccpars2(113, 'OBJECT-GROUP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 228, [113 | __Ss], [__T | __Stack]);
yeccpars2(113, 'OBJECT-IDENTITY', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 229, [113 | __Ss], [__T | __Stack]);
yeccpars2(113, 'OBJECT-TYPE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 230, [113 | __Ss], [__T | __Stack]);
yeccpars2(113, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(114, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(definitionv2, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(115, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(definitionv2, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(116, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(definitionv2, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(117, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(definitionv2, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(118, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(definitionv2, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(119, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [119 | __Ss], [__T | __Stack]);
yeccpars2(119, '::=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [119 | __Ss], [__T | __Stack]);
yeccpars2(119, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(120, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(definitionv2, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(121, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(definitionv2, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(122, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_122_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(listofdefinitionsv2, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(123, 'AutonomousType', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, 'BIT', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, 'BITS', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, 'Counter', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, 'DateAndTime', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, 'DisplayString', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, 'Gauge', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, 'INTEGER', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, 'InstancePointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, 'IpAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, 'MacAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, 'NetworkAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, 'OBJECT', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, 'OCTET', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, 'Opaque', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, 'PhysAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, 'RowPointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, 'RowStatus', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, 'SEQUENCE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 145, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, 'StorageType', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 146, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, 'TAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 147, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, 'TDomain', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 148, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, 'TEXTUAL-CONVENTION', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 149, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, 'TestAndIncr', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 150, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, 'TimeInterval', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 151, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, 'TimeStamp', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 152, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, 'TimeTicks', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 153, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, 'TruthValue', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 154, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, 'VariablePointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 155, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, variable, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 156, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(124, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 204, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_124_(__Stack),
 yeccpars2(yeccgoto(syntax, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(125, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 204, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_125_(__Stack),
 yeccpars2(yeccgoto(syntax, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(126, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_126_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(newtype, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(127, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_127_(__Stack),
 yeccpars2(yeccgoto(type, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(128, 'STRING', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 202, [128 | __Ss], [__T | __Stack]);
yeccpars2(128, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(129, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 181, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(130, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_130_(__Stack),
 yeccpars2(yeccgoto(type, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(131, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_131_(__Stack),
 yeccpars2(yeccgoto(type, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(132, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_132_(__Stack),
 yeccpars2(yeccgoto(type, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(133, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_133_(__Stack),
 yeccpars2(yeccgoto(type, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(134, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 199, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(type, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(135, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_135_(__Stack),
 yeccpars2(yeccgoto(type, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(136, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(type, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(137, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_137_(__Stack),
 yeccpars2(yeccgoto(type, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(138, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(type, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(139, 'IDENTIFIER', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 198, [139 | __Ss], [__T | __Stack]);
yeccpars2(139, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(140, 'STRING', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 197, [140 | __Ss], [__T | __Stack]);
yeccpars2(140, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(141, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(type, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(142, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_142_(__Stack),
 yeccpars2(yeccgoto(type, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(143, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_143_(__Stack),
 yeccpars2(yeccgoto(type, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(144, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_144_(__Stack),
 yeccpars2(yeccgoto(type, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(145, 'OF', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 172, [145 | __Ss], [__T | __Stack]);
yeccpars2(145, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 174, [145 | __Ss], [__T | __Stack]);
yeccpars2(145, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(146, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_146_(__Stack),
 yeccpars2(yeccgoto(type, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(147, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_147_(__Stack),
 yeccpars2(yeccgoto(type, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(148, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_148_(__Stack),
 yeccpars2(yeccgoto(type, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(149, 'DISPLAY-HINT', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 158, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_149_(__Stack),
 yeccpars2(157, __Cat, [149 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(150, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_150_(__Stack),
 yeccpars2(yeccgoto(type, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(151, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_151_(__Stack),
 yeccpars2(yeccgoto(type, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(152, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_152_(__Stack),
 yeccpars2(yeccgoto(type, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(153, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(type, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(154, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_154_(__Stack),
 yeccpars2(yeccgoto(type, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(155, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_155_(__Stack),
 yeccpars2(yeccgoto(type, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(156, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(usertype, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(157, 'STATUS', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 160, [157 | __Ss], [__T | __Stack]);
yeccpars2(157, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(158, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 159, [158 | __Ss], [__T | __Stack]);
yeccpars2(158, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(159, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_159_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(displaypart, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(160, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 162, [160 | __Ss], [__T | __Stack]);
yeccpars2(160, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(161, 'DESCRIPTION', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 164, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_161_(__Stack),
 yeccpars2(163, __Cat, [161 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(162, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_162_(__Stack),
 yeccpars2(yeccgoto(statusv2, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(163, 'REFERENCE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 167, [163 | __Ss], [__T | __Stack]);
yeccpars2(163, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_163_(__Stack),
 yeccpars2(166, __Cat, [163 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(164, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 165, [164 | __Ss], [__T | __Stack]);
yeccpars2(164, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(165, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_165_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(description, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(166, 'SYNTAX', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 169, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(167, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 168, [167 | __Ss], [__T | __Stack]);
yeccpars2(167, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(168, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_168_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(referpart, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(169, 'AutonomousType', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, 'BIT', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, 'BITS', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, 'Counter', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, 'DateAndTime', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, 'DisplayString', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, 'Gauge', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, 'INTEGER', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, 'InstancePointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, 'IpAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, 'MacAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, 'NetworkAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, 'OBJECT', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, 'OCTET', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, 'Opaque', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, 'PhysAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, 'RowPointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, 'RowStatus', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, 'SEQUENCE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 171, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, 'StorageType', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 146, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, 'TAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 147, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, 'TDomain', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 148, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, 'TestAndIncr', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 150, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, 'TimeInterval', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 151, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, 'TimeStamp', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 152, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, 'TimeTicks', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 153, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, 'TruthValue', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 154, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, 'VariablePointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 155, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, variable, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 156, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(170, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_170_(__Stack),
 __Nss = lists:nthtail(9, __Ss),
 yeccpars2(yeccgoto(textualconvention, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(171, 'OF', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 172, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(172, variable, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 156, [172 | __Ss], [__T | __Stack]);
yeccpars2(172, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(173, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_173_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(syntax, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(174, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 177, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(175, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 193, [175 | __Ss], [__T | __Stack]);
yeccpars2(175, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 194, [175 | __Ss], [__T | __Stack]);
yeccpars2(175, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(176, 'AutonomousType', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'BIT', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'BITS', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 180, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'Counter', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'DateAndTime', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'DisplayString', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'Gauge', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'INTEGER', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'InstancePointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'IpAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'MacAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'NetworkAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'OBJECT', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'OCTET', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'Opaque', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'PhysAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'RowPointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'RowStatus', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'SEQUENCE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 171, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'StorageType', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 146, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'TAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 147, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'TDomain', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 148, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'TestAndIncr', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 150, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'TimeInterval', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 151, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'TimeStamp', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 152, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'TimeTicks', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 153, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'TruthValue', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 154, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, 'VariablePointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 155, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, variable, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 156, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(177, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(fieldname, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(178, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(fsyntax, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(179, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_179_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(fields, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(180, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 181, [180 | __Ss], [__T | __Stack]);
yeccpars2(180, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_180_(__Stack),
 yeccpars2(yeccgoto(fsyntax, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(181, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 183, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(182, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 187, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 188, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(183, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 184, [183 | __Ss], [__T | __Stack]);
yeccpars2(183, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(184, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 185, [184 | __Ss], [__T | __Stack]);
yeccpars2(184, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(185, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 186, [185 | __Ss], [__T | __Stack]);
yeccpars2(185, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(186, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_186_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(namedbits, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(187, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 189, [187 | __Ss], [__T | __Stack]);
yeccpars2(187, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(188, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_188_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(syntax, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(189, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 190, [189 | __Ss], [__T | __Stack]);
yeccpars2(189, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(190, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 191, [190 | __Ss], [__T | __Stack]);
yeccpars2(190, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(191, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 192, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(192, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_192_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(namedbits, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(193, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 177, [193 | __Ss], [__T | __Stack]);
yeccpars2(193, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(194, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_194_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(tableentrydefinition, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(195, 'AutonomousType', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'BIT', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'BITS', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 180, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'Counter', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'DateAndTime', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'DisplayString', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'Gauge', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'INTEGER', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'InstancePointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'IpAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'MacAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'NetworkAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'OBJECT', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'OCTET', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'Opaque', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'PhysAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'RowPointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'RowStatus', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'SEQUENCE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 171, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'StorageType', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 146, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'TAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 147, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'TDomain', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 148, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'TestAndIncr', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 150, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'TimeInterval', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 151, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'TimeStamp', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 152, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'TimeTicks', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 153, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'TruthValue', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 154, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, 'VariablePointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 155, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, variable, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 156, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(196, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_196_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(fields, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(197, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_197_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(198, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_198_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(199, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 183, [199 | __Ss], [__T | __Stack]);
yeccpars2(199, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(200, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 187, [200 | __Ss], [__T | __Stack]);
yeccpars2(200, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 201, [200 | __Ss], [__T | __Stack]);
yeccpars2(200, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(201, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_201_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(syntax, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(202, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_202_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(203, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_203_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(syntax, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(204, 'SIZE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 207, [204 | __Ss], [__T | __Stack]);
yeccpars2(204, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 208, [204 | __Ss], [__T | __Stack]);
yeccpars2(204, quote, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 209, [204 | __Ss], [__T | __Stack]);
yeccpars2(204, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(205, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 222, [205 | __Ss], [__T | __Stack]);
yeccpars2(205, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 215, [205 | __Ss], [__T | __Stack]);
yeccpars2(205, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(206, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 218, [206 | __Ss], [__T | __Stack]);
yeccpars2(206, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_206_(__Stack),
 yeccpars2(yeccgoto(sizedescr, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(207, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 212, [207 | __Ss], [__T | __Stack]);
yeccpars2(207, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(208, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_208_(__Stack),
 yeccpars2(yeccgoto(range_num, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(209, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 210, [209 | __Ss], [__T | __Stack]);
yeccpars2(209, variable, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 211, [209 | __Ss], [__T | __Stack]);
yeccpars2(209, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(210, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_210_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(range_num, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(211, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_211_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(range_num, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(212, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 208, [212 | __Ss], [__T | __Stack]);
yeccpars2(212, quote, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 209, [212 | __Ss], [__T | __Stack]);
yeccpars2(212, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(213, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 214, [213 | __Ss], [__T | __Stack]);
yeccpars2(213, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 215, [213 | __Ss], [__T | __Stack]);
yeccpars2(213, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(214, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 217, [214 | __Ss], [__T | __Stack]);
yeccpars2(214, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(215, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 208, [215 | __Ss], [__T | __Stack]);
yeccpars2(215, quote, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 209, [215 | __Ss], [__T | __Stack]);
yeccpars2(215, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(216, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 215, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_216_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(sizedescr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(217, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_217_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(size, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(218, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 219, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(219, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 208, [219 | __Ss], [__T | __Stack]);
yeccpars2(219, quote, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 209, [219 | __Ss], [__T | __Stack]);
yeccpars2(219, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(220, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 208, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, quote, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 209, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_220_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(sizedescr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(221, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 215, [221 | __Ss], [__T | __Stack]);
yeccpars2(221, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_221_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(sizedescr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(222, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_222_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(size, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(223, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_223_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(syntax, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(224, 'STATUS', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 321, [224 | __Ss], [__T | __Stack]);
yeccpars2(224, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(225, 'NOTIFICATIONS', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 312, [225 | __Ss], [__T | __Stack]);
yeccpars2(225, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(226, 'OBJECTS', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 291, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_226_(__Stack),
 yeccpars2(305, __Cat, [226 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(227, 'IDENTIFIER', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 303, [227 | __Ss], [__T | __Stack]);
yeccpars2(227, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(228, 'OBJECTS', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 291, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_228_(__Stack),
 yeccpars2(290, __Cat, [228 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(229, 'STATUS', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 284, [229 | __Ss], [__T | __Stack]);
yeccpars2(229, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(230, 'SYNTAX', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 231, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(231, 'AutonomousType', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'BIT', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'BITS', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'Counter', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'DateAndTime', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'DisplayString', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'Gauge', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'INTEGER', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'InstancePointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'IpAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'MacAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'NetworkAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'OBJECT', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'OCTET', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'Opaque', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'PhysAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'RowPointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'RowStatus', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'SEQUENCE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 171, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'StorageType', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 146, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'TAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 147, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'TDomain', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 148, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'TestAndIncr', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 150, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'TimeInterval', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 151, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'TimeStamp', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 152, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'TimeTicks', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 153, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'TruthValue', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 154, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, 'VariablePointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 155, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, variable, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 156, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(232, 'UNITS', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 234, [232 | __Ss], [__T | __Stack]);
yeccpars2(232, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_232_(__Stack),
 yeccpars2(233, __Cat, [232 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(233, 'MAX-ACCESS', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 236, [233 | __Ss], [__T | __Stack]);
yeccpars2(233, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(234, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 235, [234 | __Ss], [__T | __Stack]);
yeccpars2(234, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(235, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_235_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(unitspart, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(236, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 238, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(237, 'STATUS', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 239, [237 | __Ss], [__T | __Stack]);
yeccpars2(237, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(238, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_238_(__Stack),
 yeccpars2(yeccgoto(accessv2, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(239, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 162, [239 | __Ss], [__T | __Stack]);
yeccpars2(239, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(240, 'DESCRIPTION', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 241, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(241, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 84, [241 | __Ss], [__T | __Stack]);
yeccpars2(241, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_241_(__Stack),
 yeccpars2(242, __Cat, [241 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(242, 'REFERENCE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 167, [242 | __Ss], [__T | __Stack]);
yeccpars2(242, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_242_(__Stack),
 yeccpars2(243, __Cat, [242 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(243, 'AUGMENTS', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 245, [243 | __Ss], [__T | __Stack]);
yeccpars2(243, 'INDEX', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 246, [243 | __Ss], [__T | __Stack]);
yeccpars2(243, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_243_(__Stack),
 yeccpars2(244, __Cat, [243 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(244, 'DEFVAL', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 262, [244 | __Ss], [__T | __Stack]);
yeccpars2(244, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_244_(__Stack),
 yeccpars2(261, __Cat, [244 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(245, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 257, [245 | __Ss], [__T | __Stack]);
yeccpars2(245, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(246, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 247, [246 | __Ss], [__T | __Stack]);
yeccpars2(246, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(247, 'IMPLIED', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 252, [247 | __Ss], [__T | __Stack]);
yeccpars2(247, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [247 | __Ss], [__T | __Stack]);
yeccpars2(247, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(248, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(index, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(249, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_249_(__Stack),
 yeccpars2(yeccgoto(indextypesv2, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(250, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 254, [250 | __Ss], [__T | __Stack]);
yeccpars2(250, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 255, [250 | __Ss], [__T | __Stack]);
yeccpars2(250, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(251, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(indextypev2, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(252, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [252 | __Ss], [__T | __Stack]);
yeccpars2(252, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(253, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_253_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(indextypev2, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(254, 'IMPLIED', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 252, [254 | __Ss], [__T | __Stack]);
yeccpars2(254, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [254 | __Ss], [__T | __Stack]);
yeccpars2(254, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(255, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_255_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(indexpartv2, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(256, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_256_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(indextypesv2, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(257, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [257 | __Ss], [__T | __Stack]);
yeccpars2(257, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(258, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(entry, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(259, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 260, [259 | __Ss], [__T | __Stack]);
yeccpars2(259, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(260, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_260_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(indexpartv2, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(261, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, '::=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(262, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 263, [262 | __Ss], [__T | __Stack]);
yeccpars2(262, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(263, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 264, [263 | __Ss], [__T | __Stack]);
yeccpars2(263, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 265, [263 | __Ss], [__T | __Stack]);
yeccpars2(263, quote, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 266, [263 | __Ss], [__T | __Stack]);
yeccpars2(263, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 267, [263 | __Ss], [__T | __Stack]);
yeccpars2(263, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 268, [263 | __Ss], [__T | __Stack]);
yeccpars2(263, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(264, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 282, [264 | __Ss], [__T | __Stack]);
yeccpars2(264, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(265, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 281, [265 | __Ss], [__T | __Stack]);
yeccpars2(265, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(266, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 277, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, variable, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 278, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(267, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 276, [267 | __Ss], [__T | __Stack]);
yeccpars2(267, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(268, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 271, [268 | __Ss], [__T | __Stack]);
yeccpars2(268, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_268_(__Stack),
 yeccpars2(269, __Cat, [268 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(269, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 274, [269 | __Ss], [__T | __Stack]);
yeccpars2(269, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(270, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 272, [270 | __Ss], [__T | __Stack]);
yeccpars2(270, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(defbitsvalue, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(271, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_271_(__Stack),
 yeccpars2(yeccgoto(defbitsnames, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(272, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 273, [272 | __Ss], [__T | __Stack]);
yeccpars2(272, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(273, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_273_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(defbitsnames, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(274, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 275, [274 | __Ss], [__T | __Stack]);
yeccpars2(274, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(275, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_275_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(defvalpart, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(276, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_276_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(defvalpart, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(277, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 280, [277 | __Ss], [__T | __Stack]);
yeccpars2(277, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(278, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 279, [278 | __Ss], [__T | __Stack]);
yeccpars2(278, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(279, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_279_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(defvalpart, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(280, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_280_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(defvalpart, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(281, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_281_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(defvalpart, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(282, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_282_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(defvalpart, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(283, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_283_(__Stack),
 __Nss = lists:nthtail(14, __Ss),
 yeccpars2(yeccgoto(objecttypev2, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(284, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 162, [284 | __Ss], [__T | __Stack]);
yeccpars2(284, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(285, 'DESCRIPTION', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 286, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(286, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 287, [286 | __Ss], [__T | __Stack]);
yeccpars2(286, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(287, 'REFERENCE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 167, [287 | __Ss], [__T | __Stack]);
yeccpars2(287, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_287_(__Stack),
 yeccpars2(288, __Cat, [287 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(288, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [288 | __Ss], [__T | __Stack]);
yeccpars2(288, '::=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [288 | __Ss], [__T | __Stack]);
yeccpars2(288, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(289, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_289_(__Stack),
 __Nss = lists:nthtail(7, __Ss),
 yeccpars2(yeccgoto(objectidentity, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(290, 'STATUS', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 298, [290 | __Ss], [__T | __Stack]);
yeccpars2(290, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(291, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 292, [291 | __Ss], [__T | __Stack]);
yeccpars2(291, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(292, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [292 | __Ss], [__T | __Stack]);
yeccpars2(292, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(293, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 295, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 296, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(294, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_294_(__Stack),
 yeccpars2(yeccgoto(objects, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(295, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [295 | __Ss], [__T | __Stack]);
yeccpars2(295, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(296, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_296_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(objectspart, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(297, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_297_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(objects, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(298, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 162, [298 | __Ss], [__T | __Stack]);
yeccpars2(298, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(299, 'DESCRIPTION', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 164, [299 | __Ss], [__T | __Stack]);
yeccpars2(299, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_299_(__Stack),
 yeccpars2(300, __Cat, [299 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(300, 'REFERENCE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 167, [300 | __Ss], [__T | __Stack]);
yeccpars2(300, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_300_(__Stack),
 yeccpars2(301, __Cat, [300 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(301, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [301 | __Ss], [__T | __Stack]);
yeccpars2(301, '::=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [301 | __Ss], [__T | __Stack]);
yeccpars2(301, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(302, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_302_(__Stack),
 __Nss = lists:nthtail(7, __Ss),
 yeccpars2(yeccgoto(objectgroup, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(303, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [303 | __Ss], [__T | __Stack]);
yeccpars2(303, '::=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [303 | __Ss], [__T | __Stack]);
yeccpars2(303, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(304, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_304_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(objectidentifier, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(305, 'STATUS', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 306, [305 | __Ss], [__T | __Stack]);
yeccpars2(305, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(306, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 162, [306 | __Ss], [__T | __Stack]);
yeccpars2(306, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(307, 'DESCRIPTION', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 308, [307 | __Ss], [__T | __Stack]);
yeccpars2(307, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(308, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 84, [308 | __Ss], [__T | __Stack]);
yeccpars2(308, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_308_(__Stack),
 yeccpars2(309, __Cat, [308 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(309, 'REFERENCE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 167, [309 | __Ss], [__T | __Stack]);
yeccpars2(309, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_309_(__Stack),
 yeccpars2(310, __Cat, [309 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(310, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [310 | __Ss], [__T | __Stack]);
yeccpars2(310, '::=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [310 | __Ss], [__T | __Stack]);
yeccpars2(310, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(311, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_311_(__Stack),
 __Nss = lists:nthtail(8, __Ss),
 yeccpars2(yeccgoto(notification, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(312, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 313, [312 | __Ss], [__T | __Stack]);
yeccpars2(312, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(313, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [313 | __Ss], [__T | __Stack]);
yeccpars2(313, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(314, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 295, [314 | __Ss], [__T | __Stack]);
yeccpars2(314, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 315, [314 | __Ss], [__T | __Stack]);
yeccpars2(314, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(315, 'STATUS', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 316, [315 | __Ss], [__T | __Stack]);
yeccpars2(315, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(316, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 162, [316 | __Ss], [__T | __Stack]);
yeccpars2(316, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(317, 'DESCRIPTION', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 164, [317 | __Ss], [__T | __Stack]);
yeccpars2(317, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_317_(__Stack),
 yeccpars2(318, __Cat, [317 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(318, 'REFERENCE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 167, [318 | __Ss], [__T | __Stack]);
yeccpars2(318, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_318_(__Stack),
 yeccpars2(319, __Cat, [318 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(319, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [319 | __Ss], [__T | __Stack]);
yeccpars2(319, '::=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [319 | __Ss], [__T | __Stack]);
yeccpars2(319, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(320, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_320_(__Stack),
 __Nss = lists:nthtail(10, __Ss),
 yeccpars2(yeccgoto(notificationgroup, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(321, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 162, [321 | __Ss], [__T | __Stack]);
yeccpars2(321, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(322, 'DESCRIPTION', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 164, [322 | __Ss], [__T | __Stack]);
yeccpars2(322, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_322_(__Stack),
 yeccpars2(323, __Cat, [322 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(323, 'REFERENCE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 167, [323 | __Ss], [__T | __Stack]);
yeccpars2(323, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_323_(__Stack),
 yeccpars2(324, __Cat, [323 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(324, 'MODULE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 328, [324 | __Ss], [__T | __Stack]);
yeccpars2(324, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_324_(__Stack),
 yeccpars2(326, __Cat, [324 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(325, 'MODULE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 328, [325 | __Ss], [__T | __Stack]);
yeccpars2(325, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_325_(__Stack),
 yeccpars2(yeccgoto(modulepart, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(326, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [326 | __Ss], [__T | __Stack]);
yeccpars2(326, '::=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [326 | __Ss], [__T | __Stack]);
yeccpars2(326, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(327, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_327_(__Stack),
 yeccpars2(yeccgoto(modules, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(328, variable, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [328 | __Ss], [__T | __Stack]);
yeccpars2(328, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_328_(__Stack),
 yeccpars2(329, __Cat, [328 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(329, 'MANDATORY-GROUPS', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 332, [329 | __Ss], [__T | __Stack]);
yeccpars2(329, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_329_(__Stack),
 yeccpars2(331, __Cat, [329 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(330, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_330_(__Stack),
 yeccpars2(yeccgoto(modulenamepart, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(331, 'GROUP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 341, [331 | __Ss], [__T | __Stack]);
yeccpars2(331, 'OBJECT', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 342, [331 | __Ss], [__T | __Stack]);
yeccpars2(331, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_331_(__Stack),
 yeccpars2(338, __Cat, [331 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(332, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 333, [332 | __Ss], [__T | __Stack]);
yeccpars2(332, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(333, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [333 | __Ss], [__T | __Stack]);
yeccpars2(333, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(334, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 295, [334 | __Ss], [__T | __Stack]);
yeccpars2(334, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 335, [334 | __Ss], [__T | __Stack]);
yeccpars2(334, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(335, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_335_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(mandatorypart, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(336, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_336_(__Stack),
 yeccpars2(yeccgoto(compliance, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(337, 'GROUP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 341, [337 | __Ss], [__T | __Stack]);
yeccpars2(337, 'OBJECT', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 342, [337 | __Ss], [__T | __Stack]);
yeccpars2(337, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_337_(__Stack),
 yeccpars2(yeccgoto(compliancepart, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(338, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_338_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(module, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(339, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_339_(__Stack),
 yeccpars2(yeccgoto(compliance, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(340, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_340_(__Stack),
 yeccpars2(yeccgoto(compliances, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(341, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [341 | __Ss], [__T | __Stack]);
yeccpars2(341, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(342, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [342 | __Ss], [__T | __Stack]);
yeccpars2(342, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(343, 'SYNTAX', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 345, [343 | __Ss], [__T | __Stack]);
yeccpars2(343, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_343_(__Stack),
 yeccpars2(344, __Cat, [343 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(344, 'WRITE-SYNTAX', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 348, [344 | __Ss], [__T | __Stack]);
yeccpars2(344, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_344_(__Stack),
 yeccpars2(347, __Cat, [344 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(345, 'AutonomousType', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, 'BIT', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, 'BITS', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, 'Counter', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, 'DateAndTime', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, 'DisplayString', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, 'Gauge', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, 'INTEGER', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, 'InstancePointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, 'IpAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, 'MacAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, 'NetworkAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, 'OBJECT', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, 'OCTET', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, 'Opaque', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, 'PhysAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, 'RowPointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, 'RowStatus', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, 'SEQUENCE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 171, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, 'StorageType', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 146, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, 'TAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 147, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, 'TDomain', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 148, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, 'TestAndIncr', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 150, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, 'TimeInterval', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 151, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, 'TimeStamp', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 152, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, 'TimeTicks', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 153, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, 'TruthValue', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 154, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, 'VariablePointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 155, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, variable, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 156, [345 | __Ss], [__T | __Stack]);
yeccpars2(345, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(346, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_346_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(syntaxpart, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(347, 'MIN-ACCESS', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 351, [347 | __Ss], [__T | __Stack]);
yeccpars2(347, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_347_(__Stack),
 yeccpars2(350, __Cat, [347 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(348, 'AutonomousType', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, 'BIT', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, 'BITS', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, 'Counter', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, 'DateAndTime', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, 'DisplayString', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, 'Gauge', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, 'INTEGER', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, 'InstancePointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, 'IpAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, 'MacAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, 'NetworkAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, 'OBJECT', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, 'OCTET', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, 'Opaque', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, 'PhysAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, 'RowPointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, 'RowStatus', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, 'SEQUENCE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 171, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, 'StorageType', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 146, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, 'TAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 147, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, 'TDomain', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 148, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, 'TestAndIncr', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 150, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, 'TimeInterval', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 151, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, 'TimeStamp', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 152, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, 'TimeTicks', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 153, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, 'TruthValue', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 154, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, 'VariablePointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 155, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, variable, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 156, [348 | __Ss], [__T | __Stack]);
yeccpars2(348, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(349, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_349_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(writesyntaxpart, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(350, 'DESCRIPTION', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 164, [350 | __Ss], [__T | __Stack]);
yeccpars2(350, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_350_(__Stack),
 yeccpars2(353, __Cat, [350 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(351, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 238, [351 | __Ss], [__T | __Stack]);
yeccpars2(351, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(352, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_352_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(accesspart, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(353, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_353_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(object, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(354, 'DESCRIPTION', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 164, [354 | __Ss], [__T | __Stack]);
yeccpars2(354, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_354_(__Stack),
 yeccpars2(355, __Cat, [354 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(355, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_355_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(compliancegroup, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(356, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_356_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(compliances, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(357, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_357_(__Stack),
 __Nss = lists:nthtail(7, __Ss),
 yeccpars2(yeccgoto(modulecompliance, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(358, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_358_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(modules, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(359, 'AutonomousType', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, 'BIT', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, 'BITS', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, 'Counter', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, 'DateAndTime', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, 'DisplayString', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, 'Gauge', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, 'INTEGER', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, 'InstancePointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, 'IpAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, 'MacAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, 'NetworkAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, 'OBJECT', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, 'OCTET', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, 'Opaque', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, 'PhysAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, 'RowPointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, 'RowStatus', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, 'SEQUENCE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 145, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, 'StorageType', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 146, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, 'TAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 147, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, 'TDomain', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 148, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, 'TestAndIncr', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 150, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, 'TimeInterval', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 151, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, 'TimeStamp', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 152, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, 'TimeTicks', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 153, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, 'TruthValue', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 154, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, 'VariablePointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 155, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, variable, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 156, [359 | __Ss], [__T | __Stack]);
yeccpars2(359, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(360, 'SYNTAX', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 376, [360 | __Ss], [__T | __Stack]);
yeccpars2(360, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(361, 'ENTERPRISE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 362, [361 | __Ss], [__T | __Stack]);
yeccpars2(361, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(362, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [362 | __Ss], [__T | __Stack]);
yeccpars2(362, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(363, 'VARIABLES', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 365, [363 | __Ss], [__T | __Stack]);
yeccpars2(363, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_363_(__Stack),
 yeccpars2(364, __Cat, [363 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(364, 'DESCRIPTION', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 164, [364 | __Ss], [__T | __Stack]);
yeccpars2(364, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_364_(__Stack),
 yeccpars2(372, __Cat, [364 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(365, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 366, [365 | __Ss], [__T | __Stack]);
yeccpars2(365, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(366, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [366 | __Ss], [__T | __Stack]);
yeccpars2(366, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(367, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 369, [367 | __Ss], [__T | __Stack]);
yeccpars2(367, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 370, [367 | __Ss], [__T | __Stack]);
yeccpars2(367, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(368, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_368_(__Stack),
 yeccpars2(yeccgoto(variables, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(369, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [369 | __Ss], [__T | __Stack]);
yeccpars2(369, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(370, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_370_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(varpart, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(371, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_371_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(variables, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(372, 'REFERENCE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 167, [372 | __Ss], [__T | __Stack]);
yeccpars2(372, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_372_(__Stack),
 yeccpars2(373, __Cat, [372 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(373, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [373 | __Ss], [__T | __Stack]);
yeccpars2(373, '::=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [373 | __Ss], [__T | __Stack]);
yeccpars2(373, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(374, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 375, [374 | __Ss], [__T | __Stack]);
yeccpars2(374, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(375, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_375_(__Stack),
 __Nss = lists:nthtail(8, __Ss),
 yeccpars2(yeccgoto(traptype, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(376, 'AutonomousType', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, 'BIT', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, 'BITS', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, 'Counter', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, 'DateAndTime', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, 'DisplayString', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, 'Gauge', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, 'INTEGER', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, 'InstancePointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, 'IpAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, 'MacAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, 'NetworkAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, 'OBJECT', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, 'OCTET', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, 'Opaque', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, 'PhysAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, 'RowPointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, 'RowStatus', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, 'SEQUENCE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 171, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, 'StorageType', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 146, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, 'TAddress', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 147, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, 'TDomain', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 148, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, 'TestAndIncr', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 150, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, 'TimeInterval', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 151, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, 'TimeStamp', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 152, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, 'TimeTicks', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 153, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, 'TruthValue', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 154, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, 'VariablePointer', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 155, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, variable, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 156, [376 | __Ss], [__T | __Stack]);
yeccpars2(376, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(377, 'ACCESS', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 378, [377 | __Ss], [__T | __Stack]);
yeccpars2(377, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(378, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 380, [378 | __Ss], [__T | __Stack]);
yeccpars2(378, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(379, 'STATUS', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 381, [379 | __Ss], [__T | __Stack]);
yeccpars2(379, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(380, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_380_(__Stack),
 yeccpars2(yeccgoto(accessv1, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(381, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 383, [381 | __Ss], [__T | __Stack]);
yeccpars2(381, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(382, 'DESCRIPTION', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 384, [382 | __Ss], [__T | __Stack]);
yeccpars2(382, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(383, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_383_(__Stack),
 yeccpars2(yeccgoto(statusv1, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(384, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 84, [384 | __Ss], [__T | __Stack]);
yeccpars2(384, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_384_(__Stack),
 yeccpars2(385, __Cat, [384 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(385, 'REFERENCE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 167, [385 | __Ss], [__T | __Stack]);
yeccpars2(385, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_385_(__Stack),
 yeccpars2(386, __Cat, [385 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(386, 'INDEX', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 388, [386 | __Ss], [__T | __Stack]);
yeccpars2(386, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_386_(__Stack),
 yeccpars2(387, __Cat, [386 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(387, 'DEFVAL', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 262, [387 | __Ss], [__T | __Stack]);
yeccpars2(387, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_387_(__Stack),
 yeccpars2(396, __Cat, [387 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(388, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 389, [388 | __Ss], [__T | __Stack]);
yeccpars2(388, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(389, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [389 | __Ss], [__T | __Stack]);
yeccpars2(389, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(390, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_390_(__Stack),
 yeccpars2(yeccgoto(indextypesv1, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(391, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 393, [391 | __Ss], [__T | __Stack]);
yeccpars2(391, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 394, [391 | __Ss], [__T | __Stack]);
yeccpars2(391, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(392, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(indextypev1, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(393, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [393 | __Ss], [__T | __Stack]);
yeccpars2(393, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(394, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_394_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(indexpartv1, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(395, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_395_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(indextypesv1, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(396, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [396 | __Ss], [__T | __Stack]);
yeccpars2(396, '::=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [396 | __Ss], [__T | __Stack]);
yeccpars2(396, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(397, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_397_(__Stack),
 __Nss = lists:nthtail(13, __Ss),
 yeccpars2(yeccgoto(objecttypev1, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(398, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_398_(__Stack),
 __Nss = lists:nthtail(6, __Ss),
 yeccpars2(yeccgoto(mib, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(__Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.1",{missing_state_in_action_table, __Other}}).

yeccgoto(accesspart, 347) ->
 350;
yeccgoto(accessv1, 378) ->
 379;
yeccgoto(accessv2, 236) ->
 237;
yeccgoto(accessv2, 351) ->
 352;
yeccgoto(compliance, 331) ->
 340;
yeccgoto(compliance, 337) ->
 356;
yeccgoto(compliancegroup, 331) ->
 339;
yeccgoto(compliancegroup, 337) ->
 339;
yeccgoto(compliancepart, 331) ->
 338;
yeccgoto(compliances, 331) ->
 337;
yeccgoto(contact_info, 79) ->
 80;
yeccgoto(defbitsnames, 268) ->
 270;
yeccgoto(defbitsvalue, 268) ->
 269;
yeccgoto(definition, 11) ->
 67;
yeccgoto(definition, 66) ->
 70;
yeccgoto(definitionv2, 109) ->
 122;
yeccgoto(defvalpart, 244) ->
 261;
yeccgoto(defvalpart, 387) ->
 396;
yeccgoto(description, 161) ->
 163;
yeccgoto(description, 299) ->
 300;
yeccgoto(description, 317) ->
 318;
yeccgoto(description, 322) ->
 323;
yeccgoto(description, 350) ->
 353;
yeccgoto(description, 354) ->
 355;
yeccgoto(description, 364) ->
 372;
yeccgoto(descriptionfield, 82) ->
 83;
yeccgoto(descriptionfield, 241) ->
 242;
yeccgoto(descriptionfield, 308) ->
 309;
yeccgoto(descriptionfield, 384) ->
 385;
yeccgoto(displaypart, 149) ->
 157;
yeccgoto(entry, 257) ->
 259;
yeccgoto(fatherobjectname, 96) ->
 98;
yeccgoto(fieldname, 174) ->
 176;
yeccgoto(fieldname, 193) ->
 195;
yeccgoto(fields, 174) ->
 175;
yeccgoto(fsyntax, 176) ->
 179;
yeccgoto(fsyntax, 195) ->
 196;
yeccgoto(implies, 4) ->
 5;
yeccgoto(implies, 62) ->
 359;
yeccgoto(implies, 86) ->
 95;
yeccgoto(implies, 119) ->
 123;
yeccgoto(implies, 261) ->
 95;
yeccgoto(implies, 288) ->
 95;
yeccgoto(implies, 301) ->
 95;
yeccgoto(implies, 303) ->
 95;
yeccgoto(implies, 310) ->
 95;
yeccgoto(implies, 319) ->
 95;
yeccgoto(implies, 326) ->
 95;
yeccgoto(implies, 373) ->
 374;
yeccgoto(implies, 396) ->
 95;
yeccgoto(import, 10) ->
 11;
yeccgoto(import_stuff, 12) ->
 16;
yeccgoto(import_stuff, 14) ->
 16;
yeccgoto(import_stuff, 52) ->
 55;
yeccgoto(imports, 12) ->
 15;
yeccgoto(imports, 14) ->
 51;
yeccgoto(imports_from_one_mib, 12) ->
 14;
yeccgoto(imports_from_one_mib, 14) ->
 14;
yeccgoto(index, 247) ->
 251;
yeccgoto(index, 252) ->
 253;
yeccgoto(index, 254) ->
 251;
yeccgoto(index, 389) ->
 392;
yeccgoto(index, 393) ->
 392;
yeccgoto(indexpartv1, 386) ->
 387;
yeccgoto(indexpartv2, 243) ->
 244;
yeccgoto(indextypesv1, 389) ->
 391;
yeccgoto(indextypesv2, 247) ->
 250;
yeccgoto(indextypev1, 389) ->
 390;
yeccgoto(indextypev1, 393) ->
 395;
yeccgoto(indextypev2, 247) ->
 249;
yeccgoto(indextypev2, 254) ->
 256;
yeccgoto(last_updated, 73) ->
 74;
yeccgoto(listofdefinitions, 11) ->
 66;
yeccgoto(listofdefinitionsv2, 64) ->
 109;
yeccgoto(listofimports, 12) ->
 13;
yeccgoto(listofimports, 14) ->
 13;
yeccgoto(mandatorypart, 329) ->
 331;
yeccgoto(mib, 0) ->
 2;
yeccgoto(mibid, 11) ->
 65;
yeccgoto(mibname, 0) ->
 1;
yeccgoto(mibname, 328) ->
 330;
yeccgoto(module, 324) ->
 327;
yeccgoto(module, 325) ->
 358;
yeccgoto(modulecompliance, 109) ->
 121;
yeccgoto(moduleidentity, 11) ->
 64;
yeccgoto(modulenamepart, 328) ->
 329;
yeccgoto(modulepart, 324) ->
 326;
yeccgoto(modules, 324) ->
 325;
yeccgoto(nameassign, 86) ->
 94;
yeccgoto(nameassign, 261) ->
 283;
yeccgoto(nameassign, 288) ->
 289;
yeccgoto(nameassign, 301) ->
 302;
yeccgoto(nameassign, 303) ->
 304;
yeccgoto(nameassign, 310) ->
 311;
yeccgoto(nameassign, 319) ->
 320;
yeccgoto(nameassign, 326) ->
 357;
yeccgoto(nameassign, 396) ->
 397;
yeccgoto(namedbits, 181) ->
 182;
yeccgoto(namedbits, 199) ->
 200;
yeccgoto(newtype, 11) ->
 63;
yeccgoto(newtype, 66) ->
 63;
yeccgoto(newtype, 109) ->
 120;
yeccgoto(newtypename, 11) ->
 62;
yeccgoto(newtypename, 66) ->
 62;
yeccgoto(newtypename, 109) ->
 119;
yeccgoto(notification, 109) ->
 118;
yeccgoto(notificationgroup, 109) ->
 117;
yeccgoto(object, 331) ->
 336;
yeccgoto(object, 337) ->
 336;
yeccgoto(objectgroup, 109) ->
 116;
yeccgoto(objectidentifier, 11) ->
 61;
yeccgoto(objectidentifier, 66) ->
 61;
yeccgoto(objectidentifier, 109) ->
 115;
yeccgoto(objectidentity, 109) ->
 114;
yeccgoto(objectname, 11) ->
 60;
yeccgoto(objectname, 66) ->
 60;
yeccgoto(objectname, 96) ->
 97;
yeccgoto(objectname, 109) ->
 113;
yeccgoto(objectname, 247) ->
 248;
yeccgoto(objectname, 252) ->
 248;
yeccgoto(objectname, 254) ->
 248;
yeccgoto(objectname, 257) ->
 258;
yeccgoto(objectname, 292) ->
 294;
yeccgoto(objectname, 295) ->
 297;
yeccgoto(objectname, 313) ->
 294;
yeccgoto(objectname, 333) ->
 294;
yeccgoto(objectname, 341) ->
 354;
yeccgoto(objectname, 342) ->
 343;
yeccgoto(objectname, 362) ->
 363;
yeccgoto(objectname, 366) ->
 368;
yeccgoto(objectname, 369) ->
 371;
yeccgoto(objectname, 389) ->
 248;
yeccgoto(objectname, 393) ->
 248;
yeccgoto(objects, 292) ->
 293;
yeccgoto(objects, 313) ->
 314;
yeccgoto(objects, 333) ->
 334;
yeccgoto(objectspart, 226) ->
 305;
yeccgoto(objectspart, 228) ->
 290;
yeccgoto(objecttypev1, 11) ->
 59;
yeccgoto(objecttypev1, 66) ->
 59;
yeccgoto(objecttypev2, 109) ->
 112;
yeccgoto(oranization, 76) ->
 77;
yeccgoto(parentintegers, 98) ->
 99;
yeccgoto(parentintegers, 101) ->
 102;
yeccgoto(parentintegers, 105) ->
 106;
yeccgoto(range_num, 204) ->
 206;
yeccgoto(range_num, 212) ->
 206;
yeccgoto(range_num, 215) ->
 206;
yeccgoto(range_num, 219) ->
 220;
yeccgoto(range_num, 220) ->
 206;
yeccgoto(referpart, 163) ->
 166;
yeccgoto(referpart, 242) ->
 243;
yeccgoto(referpart, 287) ->
 288;
yeccgoto(referpart, 300) ->
 301;
yeccgoto(referpart, 309) ->
 310;
yeccgoto(referpart, 318) ->
 319;
yeccgoto(referpart, 323) ->
 324;
yeccgoto(referpart, 372) ->
 373;
yeccgoto(referpart, 385) ->
 386;
yeccgoto(revision, 83) ->
 87;
yeccgoto(revision, 85) ->
 108;
yeccgoto(revision_desc, 91) ->
 92;
yeccgoto(revision_string, 88) ->
 89;
yeccgoto(revisionpart, 83) ->
 86;
yeccgoto(revisions, 83) ->
 85;
yeccgoto(size, 124) ->
 223;
yeccgoto(size, 125) ->
 203;
yeccgoto(sizedescr, 204) ->
 205;
yeccgoto(sizedescr, 212) ->
 213;
yeccgoto(sizedescr, 215) ->
 216;
yeccgoto(sizedescr, 220) ->
 221;
yeccgoto(statusv1, 381) ->
 382;
yeccgoto(statusv2, 160) ->
 161;
yeccgoto(statusv2, 239) ->
 240;
yeccgoto(statusv2, 284) ->
 285;
yeccgoto(statusv2, 298) ->
 299;
yeccgoto(statusv2, 306) ->
 307;
yeccgoto(statusv2, 316) ->
 317;
yeccgoto(statusv2, 321) ->
 322;
yeccgoto(syntax, 123) ->
 126;
yeccgoto(syntax, 169) ->
 170;
yeccgoto(syntax, 176) ->
 178;
yeccgoto(syntax, 195) ->
 178;
yeccgoto(syntax, 231) ->
 232;
yeccgoto(syntax, 345) ->
 346;
yeccgoto(syntax, 348) ->
 349;
yeccgoto(syntax, 359) ->
 126;
yeccgoto(syntax, 376) ->
 377;
yeccgoto(syntaxpart, 343) ->
 344;
yeccgoto(tableentrydefinition, 11) ->
 58;
yeccgoto(tableentrydefinition, 66) ->
 58;
yeccgoto(tableentrydefinition, 109) ->
 111;
yeccgoto(textualconvention, 109) ->
 110;
yeccgoto(traptype, 11) ->
 57;
yeccgoto(traptype, 66) ->
 57;
yeccgoto(type, 123) ->
 125;
yeccgoto(type, 169) ->
 125;
yeccgoto(type, 176) ->
 125;
yeccgoto(type, 195) ->
 125;
yeccgoto(type, 231) ->
 125;
yeccgoto(type, 345) ->
 125;
yeccgoto(type, 348) ->
 125;
yeccgoto(type, 359) ->
 125;
yeccgoto(type, 376) ->
 125;
yeccgoto(unitspart, 232) ->
 233;
yeccgoto(usertype, 123) ->
 124;
yeccgoto(usertype, 169) ->
 124;
yeccgoto(usertype, 172) ->
 173;
yeccgoto(usertype, 176) ->
 124;
yeccgoto(usertype, 195) ->
 124;
yeccgoto(usertype, 231) ->
 124;
yeccgoto(usertype, 345) ->
 124;
yeccgoto(usertype, 348) ->
 124;
yeccgoto(usertype, 359) ->
 124;
yeccgoto(usertype, 376) ->
 124;
yeccgoto(v1orv2, 11) ->
 56;
yeccgoto(variables, 366) ->
 367;
yeccgoto(varpart, 363) ->
 364;
yeccgoto(writesyntaxpart, 344) ->
 347;
yeccgoto(__Symbol, __State) ->
 erlang:error({yecc_bug,"1.1",{__Symbol, __State, missing_in_goto_table}}).

-compile({inline,{yeccpars2_3_,1}}).
-file("snmpc_mib_gram.yrl", 451).
yeccpars2_3_([__1 | __Stack]) ->
 [begin
   val ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_9_,1}}).
-file("snmpc_mib_gram.yrl", 405).
yeccpars2_9_([__3,__2,__1 | __Stack]) ->
 [begin
   w ( "Sloppy asignment on line ~p" , [ line_of ( __1 ) ] ) , __1
  end | __Stack].

-compile({inline,{yeccpars2_10_,1}}).
-file("snmpc_mib_gram.yrl", 223).
yeccpars2_10_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_14_,1}}).
-file("snmpc_mib_gram.yrl", 226).
yeccpars2_14_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_16_,1}}).
-file("snmpc_mib_gram.yrl", 232).
yeccpars2_16_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_17_,1}}).
-file("snmpc_mib_gram.yrl", 271).
yeccpars2_17_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'AutonomousType' }
  end | __Stack].

-compile({inline,{yeccpars2_18_,1}}).
-file("snmpc_mib_gram.yrl", 240).
yeccpars2_18_([__1 | __Stack]) ->
 [begin
   { builtin , 'Counter' }
  end | __Stack].

-compile({inline,{yeccpars2_19_,1}}).
-file("snmpc_mib_gram.yrl", 285).
yeccpars2_19_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'DateAndTime' }
  end | __Stack].

-compile({inline,{yeccpars2_20_,1}}).
-file("snmpc_mib_gram.yrl", 261).
yeccpars2_20_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'DisplayString' }
  end | __Stack].

-compile({inline,{yeccpars2_21_,1}}).
-file("snmpc_mib_gram.yrl", 241).
yeccpars2_21_([__1 | __Stack]) ->
 [begin
   { builtin , 'Gauge' }
  end | __Stack].

-compile({inline,{yeccpars2_22_,1}}).
-file("snmpc_mib_gram.yrl", 273).
yeccpars2_22_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'InstancePointer' }
  end | __Stack].

-compile({inline,{yeccpars2_23_,1}}).
-file("snmpc_mib_gram.yrl", 239).
yeccpars2_23_([__1 | __Stack]) ->
 [begin
   { builtin , 'IpAddress' }
  end | __Stack].

-compile({inline,{yeccpars2_24_,1}}).
-file("snmpc_mib_gram.yrl", 251).
yeccpars2_24_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'MODULE-COMPLIANCE' }
  end | __Stack].

-compile({inline,{yeccpars2_25_,1}}).
-file("snmpc_mib_gram.yrl", 247).
yeccpars2_25_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'MODULE-IDENTITY' }
  end | __Stack].

-compile({inline,{yeccpars2_26_,1}}).
-file("snmpc_mib_gram.yrl", 265).
yeccpars2_26_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'MacAddress' }
  end | __Stack].

-compile({inline,{yeccpars2_27_,1}}).
-file("snmpc_mib_gram.yrl", 253).
yeccpars2_27_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'NOTIFICATION-GROUP' }
  end | __Stack].

-compile({inline,{yeccpars2_28_,1}}).
-file("snmpc_mib_gram.yrl", 249).
yeccpars2_28_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'NOTIFICATION-TYPE' }
  end | __Stack].

-compile({inline,{yeccpars2_29_,1}}).
-file("snmpc_mib_gram.yrl", 237).
yeccpars2_29_([__1 | __Stack]) ->
 [begin
   { builtin , 'NetworkAddress' }
  end | __Stack].

-compile({inline,{yeccpars2_30_,1}}).
-file("snmpc_mib_gram.yrl", 255).
yeccpars2_30_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'OBJECT-GROUP' }
  end | __Stack].

-compile({inline,{yeccpars2_31_,1}}).
-file("snmpc_mib_gram.yrl", 257).
yeccpars2_31_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'OBJECT-IDENTITY' }
  end | __Stack].

-compile({inline,{yeccpars2_32_,1}}).
-file("snmpc_mib_gram.yrl", 235).
yeccpars2_32_([__1 | __Stack]) ->
 [begin
   { builtin , 'OBJECT-TYPE' }
  end | __Stack].

-compile({inline,{yeccpars2_33_,1}}).
-file("snmpc_mib_gram.yrl", 242).
yeccpars2_33_([__1 | __Stack]) ->
 [begin
   { builtin , 'Opaque' }
  end | __Stack].

-compile({inline,{yeccpars2_34_,1}}).
-file("snmpc_mib_gram.yrl", 263).
yeccpars2_34_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'PhysAddress' }
  end | __Stack].

-compile({inline,{yeccpars2_35_,1}}).
-file("snmpc_mib_gram.yrl", 277).
yeccpars2_35_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'RowPointer' }
  end | __Stack].

-compile({inline,{yeccpars2_36_,1}}).
-file("snmpc_mib_gram.yrl", 279).
yeccpars2_36_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'RowStatus' }
  end | __Stack].

-compile({inline,{yeccpars2_37_,1}}).
-file("snmpc_mib_gram.yrl", 287).
yeccpars2_37_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'StorageType' }
  end | __Stack].

-compile({inline,{yeccpars2_38_,1}}).
-file("snmpc_mib_gram.yrl", 291).
yeccpars2_38_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'TAddress' }
  end | __Stack].

-compile({inline,{yeccpars2_39_,1}}).
-file("snmpc_mib_gram.yrl", 289).
yeccpars2_39_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'TDomain' }
  end | __Stack].

-compile({inline,{yeccpars2_40_,1}}).
-file("snmpc_mib_gram.yrl", 259).
yeccpars2_40_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'TEXTUAL-CONVENTION' }
  end | __Stack].

-compile({inline,{yeccpars2_41_,1}}).
-file("snmpc_mib_gram.yrl", 236).
yeccpars2_41_([__1 | __Stack]) ->
 [begin
   { builtin , 'TRAP-TYPE' }
  end | __Stack].

-compile({inline,{yeccpars2_42_,1}}).
-file("snmpc_mib_gram.yrl", 269).
yeccpars2_42_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'TestAndIncr' }
  end | __Stack].

-compile({inline,{yeccpars2_43_,1}}).
-file("snmpc_mib_gram.yrl", 283).
yeccpars2_43_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'TimeInterval' }
  end | __Stack].

-compile({inline,{yeccpars2_44_,1}}).
-file("snmpc_mib_gram.yrl", 281).
yeccpars2_44_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'TimeStamp' }
  end | __Stack].

-compile({inline,{yeccpars2_45_,1}}).
-file("snmpc_mib_gram.yrl", 238).
yeccpars2_45_([__1 | __Stack]) ->
 [begin
   { builtin , 'TimeTicks' }
  end | __Stack].

-compile({inline,{yeccpars2_46_,1}}).
-file("snmpc_mib_gram.yrl", 267).
yeccpars2_46_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'TruthValue' }
  end | __Stack].

-compile({inline,{yeccpars2_47_,1}}).
-file("snmpc_mib_gram.yrl", 275).
yeccpars2_47_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'VariablePointer' }
  end | __Stack].

-compile({inline,{yeccpars2_48_,1}}).
-file("snmpc_mib_gram.yrl", 244).
yeccpars2_48_([__1 | __Stack]) ->
 [begin
   { node , val ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_49_,1}}).
-file("snmpc_mib_gram.yrl", 243).
yeccpars2_49_([__1 | __Stack]) ->
 [begin
   filter_v2imports ( get ( snmp_version ) , val ( __1 ) )
  end | __Stack].

-compile({inline,{yeccpars2_50_,1}}).
-file("snmpc_mib_gram.yrl", 224).
yeccpars2_50_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_51_,1}}).
-file("snmpc_mib_gram.yrl", 227).
yeccpars2_51_([__2,__1 | __Stack]) ->
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_54_,1}}).
-file("snmpc_mib_gram.yrl", 230).
yeccpars2_54_([__3,__2,__1 | __Stack]) ->
 [begin
   { { val ( __3 ) , lists : reverse ( __1 ) } , line_of ( __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_55_,1}}).
-file("snmpc_mib_gram.yrl", 233).
yeccpars2_55_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __3 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_64_,1}}).
-file("snmpc_mib_gram.yrl", 503).
yeccpars2_64_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_66_,1}}).
-file("snmpc_mib_gram.yrl", 212).
yeccpars2_66_([__1 | __Stack]) ->
 [begin
   { v1_mib , lists : reverse ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_67_,1}}).
-file("snmpc_mib_gram.yrl", 220).
yeccpars2_67_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{'yeccpars2_68_MODULE-IDENTITY',1}}).
-file("snmpc_mib_gram.yrl", 476).
'yeccpars2_68_MODULE-IDENTITY'([__1 | __Stack]) ->
 [begin
   val ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_68_,1}}).
-file("snmpc_mib_gram.yrl", 450).
yeccpars2_68_([__1 | __Stack]) ->
 [begin
   val ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_69_,1}}).
-file("snmpc_mib_gram.yrl", 453).
yeccpars2_69_([__1 | __Stack]) ->
 [begin
   val ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_70_,1}}).
-file("snmpc_mib_gram.yrl", 221).
yeccpars2_70_([__2,__1 | __Stack]) ->
 [begin
   [ __2 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_71_,1}}).
-file("snmpc_mib_gram.yrl", 450).
yeccpars2_71_([__1 | __Stack]) ->
 [begin
   val ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_75_,1}}).
-file("snmpc_mib_gram.yrl", 477).
yeccpars2_75_([__1 | __Stack]) ->
 [begin
   lists : reverse ( val ( __1 ) )
  end | __Stack].

-compile({inline,{yeccpars2_78_,1}}).
-file("snmpc_mib_gram.yrl", 478).
yeccpars2_78_([__1 | __Stack]) ->
 [begin
   lists : reverse ( val ( __1 ) )
  end | __Stack].

-compile({inline,{yeccpars2_81_,1}}).
-file("snmpc_mib_gram.yrl", 479).
yeccpars2_81_([__1 | __Stack]) ->
 [begin
   lists : reverse ( val ( __1 ) )
  end | __Stack].

-compile({inline,{yeccpars2_82_,1}}).
-file("snmpc_mib_gram.yrl", 407).
yeccpars2_82_(__Stack) ->
 [begin
   undefined
  end | __Stack].

-compile({inline,{yeccpars2_83_,1}}).
-file("snmpc_mib_gram.yrl", 481).
yeccpars2_83_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_84_,1}}).
-file("snmpc_mib_gram.yrl", 406).
yeccpars2_84_([__1 | __Stack]) ->
 [begin
   lists : reverse ( val ( __1 ) )
  end | __Stack].

-compile({inline,{yeccpars2_85_,1}}).
-file("snmpc_mib_gram.yrl", 482).
yeccpars2_85_([__1 | __Stack]) ->
 [begin
   lists : reverse ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_87_,1}}).
-file("snmpc_mib_gram.yrl", 484).
yeccpars2_87_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_90_,1}}).
-file("snmpc_mib_gram.yrl", 489).
yeccpars2_90_([__1 | __Stack]) ->
 [begin
   lists : reverse ( val ( __1 ) )
  end | __Stack].

-compile({inline,{yeccpars2_92_,1}}).
-file("snmpc_mib_gram.yrl", 487).
yeccpars2_92_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   make_revision ( __2 , __4 )
  end | __Stack].

-compile({inline,{yeccpars2_93_,1}}).
-file("snmpc_mib_gram.yrl", 490).
yeccpars2_93_([__1 | __Stack]) ->
 [begin
   lists : reverse ( val ( __1 ) )
  end | __Stack].

-compile({inline,{yeccpars2_94_,1}}).
-file("snmpc_mib_gram.yrl", 472).
yeccpars2_94_([__12,__11,__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   MI = make_module_identity ( __1 , __4 , __6 , __8 ,
    __10 , __11 , __12 ) ,
    { MI , line_of ( __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_101_,1}}).
-file("snmpc_mib_gram.yrl", 426).
yeccpars2_101_([__1 | __Stack]) ->
 [begin
   [ val ( __1 ) ]
  end | __Stack].

-compile({inline,{yeccpars2_102_,1}}).
-file("snmpc_mib_gram.yrl", 428).
yeccpars2_102_([__2,__1 | __Stack]) ->
 [begin
   [ val ( __1 ) | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_105_,1}}).
-file("snmpc_mib_gram.yrl", 427).
yeccpars2_105_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   [ val ( __3 ) ]
  end | __Stack].

-compile({inline,{yeccpars2_106_,1}}).
-file("snmpc_mib_gram.yrl", 429).
yeccpars2_106_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   [ val ( __3 ) | __5 ]
  end | __Stack].

-compile({inline,{yeccpars2_107_,1}}).
-file("snmpc_mib_gram.yrl", 397).
yeccpars2_107_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { __3 , __4 }
  end | __Stack].

-compile({inline,{yeccpars2_108_,1}}).
-file("snmpc_mib_gram.yrl", 485).
yeccpars2_108_([__2,__1 | __Stack]) ->
 [begin
   [ __2 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_109_,1}}).
-file("snmpc_mib_gram.yrl", 211).
yeccpars2_109_([__2,__1 | __Stack]) ->
 [begin
   { v2_mib , [ __1 | lists : reverse ( __2 ) ] }
  end | __Stack].

-compile({inline,{yeccpars2_122_,1}}).
-file("snmpc_mib_gram.yrl", 504).
yeccpars2_122_([__2,__1 | __Stack]) ->
 [begin
   [ __2 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_124_,1}}).
-file("snmpc_mib_gram.yrl", 337).
yeccpars2_124_([__1 | __Stack]) ->
 [begin
   { { type , val ( __1 ) } , line_of ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_125_,1}}).
-file("snmpc_mib_gram.yrl", 338).
yeccpars2_125_([__1 | __Stack]) ->
 [begin
   { { type , cat ( __1 ) } , line_of ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_126_,1}}).
-file("snmpc_mib_gram.yrl", 319).
yeccpars2_126_([__3,__2,__1 | __Stack]) ->
 [begin
   NT = make_new_type ( __1 , dummy , __3 ) ,
    { NT , line_of ( __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_127_,1}}).
-file("snmpc_mib_gram.yrl", 383).
yeccpars2_127_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,{yeccpars2_130_,1}}).
-file("snmpc_mib_gram.yrl", 374).
yeccpars2_130_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 1 , __1 ) , __1
  end | __Stack].

-compile({inline,{yeccpars2_131_,1}}).
-file("snmpc_mib_gram.yrl", 390).
yeccpars2_131_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,{yeccpars2_132_,1}}).
-file("snmpc_mib_gram.yrl", 378).
yeccpars2_132_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,{yeccpars2_133_,1}}).
-file("snmpc_mib_gram.yrl", 375).
yeccpars2_133_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 1 , __1 ) , __1
  end | __Stack].

-compile({inline,{yeccpars2_135_,1}}).
-file("snmpc_mib_gram.yrl", 384).
yeccpars2_135_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,{yeccpars2_137_,1}}).
-file("snmpc_mib_gram.yrl", 380).
yeccpars2_137_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,{yeccpars2_142_,1}}).
-file("snmpc_mib_gram.yrl", 379).
yeccpars2_142_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,{yeccpars2_143_,1}}).
-file("snmpc_mib_gram.yrl", 386).
yeccpars2_143_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,{yeccpars2_144_,1}}).
-file("snmpc_mib_gram.yrl", 387).
yeccpars2_144_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,{yeccpars2_146_,1}}).
-file("snmpc_mib_gram.yrl", 391).
yeccpars2_146_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,{yeccpars2_147_,1}}).
-file("snmpc_mib_gram.yrl", 393).
yeccpars2_147_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,{yeccpars2_148_,1}}).
-file("snmpc_mib_gram.yrl", 392).
yeccpars2_148_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,{yeccpars2_149_,1}}).
-file("snmpc_mib_gram.yrl", 412).
yeccpars2_149_(__Stack) ->
 [begin
   undefined
  end | __Stack].

-compile({inline,{yeccpars2_150_,1}}).
-file("snmpc_mib_gram.yrl", 382).
yeccpars2_150_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,{yeccpars2_151_,1}}).
-file("snmpc_mib_gram.yrl", 389).
yeccpars2_151_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,{yeccpars2_152_,1}}).
-file("snmpc_mib_gram.yrl", 388).
yeccpars2_152_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,{yeccpars2_154_,1}}).
-file("snmpc_mib_gram.yrl", 381).
yeccpars2_154_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,{yeccpars2_155_,1}}).
-file("snmpc_mib_gram.yrl", 385).
yeccpars2_155_([__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | __Stack].

-compile({inline,{yeccpars2_159_,1}}).
-file("snmpc_mib_gram.yrl", 411).
yeccpars2_159_([__2,__1 | __Stack]) ->
 [begin
   display_hint ( __2 )
  end | __Stack].

-compile({inline,{yeccpars2_161_,1}}).
-file("snmpc_mib_gram.yrl", 409).
yeccpars2_161_(__Stack) ->
 [begin
   undefined
  end | __Stack].

-compile({inline,{yeccpars2_162_,1}}).
-file("snmpc_mib_gram.yrl", 601).
yeccpars2_162_([__1 | __Stack]) ->
 [begin
   statusv2 ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_163_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_163_(__Stack) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_165_,1}}).
-file("snmpc_mib_gram.yrl", 408).
yeccpars2_165_([__2,__1 | __Stack]) ->
 [begin
   lists : reverse ( val ( __2 ) )
  end | __Stack].

-compile({inline,{yeccpars2_168_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_168_([__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_170_,1}}).
-file("snmpc_mib_gram.yrl", 508).
yeccpars2_170_([__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   NT = make_new_type ( __1 , 'TEXTUAL-CONVENTION' , __4 ,
    __6 , __7 , __8 , __10 ) ,
    { NT , line_of ( __3 ) }
  end | __Stack].

-compile({inline,{yeccpars2_173_,1}}).
-file("snmpc_mib_gram.yrl", 347).
yeccpars2_173_([__3,__2,__1 | __Stack]) ->
 [begin
   { { sequence_of , val ( __3 ) } , line_of ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_179_,1}}).
-file("snmpc_mib_gram.yrl", 328).
yeccpars2_179_([__2,__1 | __Stack]) ->
 [begin
   [ { val ( __1 ) , __2 } ]
  end | __Stack].

-compile({inline,{yeccpars2_180_,1}}).
-file("snmpc_mib_gram.yrl", 332).
yeccpars2_180_([__1 | __Stack]) ->
 [begin
   { { bits , [ { dummy , 0 } ] } , line_of ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_186_,1}}).
-file("snmpc_mib_gram.yrl", 362).
yeccpars2_186_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   [ { val ( __1 ) , val ( __3 ) } ]
  end | __Stack].

-compile({inline,{yeccpars2_188_,1}}).
-file("snmpc_mib_gram.yrl", 344).
yeccpars2_188_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) ,
    { { bits , __3 } , line_of ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_192_,1}}).
-file("snmpc_mib_gram.yrl", 364).
yeccpars2_192_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   [ { val ( __3 ) , val ( __5 ) } | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_194_,1}}).
-file("snmpc_mib_gram.yrl", 323).
yeccpars2_194_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   Seq = make_sequence ( __1 , lists : reverse ( __5 ) ) ,
    { Seq , line_of ( __3 ) }
  end | __Stack].

-compile({inline,{yeccpars2_196_,1}}).
-file("snmpc_mib_gram.yrl", 330).
yeccpars2_196_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   [ { val ( __3 ) , __4 } | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_197_,1}}).
-file("snmpc_mib_gram.yrl", 368).
yeccpars2_197_([__2,__1 | __Stack]) ->
 [begin
   { 'OCTET STRING' , line_of ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_198_,1}}).
-file("snmpc_mib_gram.yrl", 370).
yeccpars2_198_([__2,__1 | __Stack]) ->
 [begin
   { 'OBJECT IDENTIFIER' , line_of ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_201_,1}}).
-file("snmpc_mib_gram.yrl", 342).
yeccpars2_201_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { { integer_with_enum , 'INTEGER' , __3 } , line_of ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_202_,1}}).
-file("snmpc_mib_gram.yrl", 369).
yeccpars2_202_([__2,__1 | __Stack]) ->
 [begin
   { 'BIT STRING' , line_of ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_203_,1}}).
-file("snmpc_mib_gram.yrl", 339).
yeccpars2_203_([__2,__1 | __Stack]) ->
 [begin
   { { type_with_size , cat ( __1 ) , __2 } , line_of ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_206_,1}}).
-file("snmpc_mib_gram.yrl", 355).
yeccpars2_206_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_208_,1}}).
-file("snmpc_mib_gram.yrl", 358).
yeccpars2_208_([__1 | __Stack]) ->
 [begin
   val ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_210_,1}}).
-file("snmpc_mib_gram.yrl", 359).
yeccpars2_210_([__2,__1 | __Stack]) ->
 [begin
   make_range_integer ( val ( __1 ) , val ( __2 ) )
  end | __Stack].

-compile({inline,{yeccpars2_211_,1}}).
-file("snmpc_mib_gram.yrl", 360).
yeccpars2_211_([__2,__1 | __Stack]) ->
 [begin
   make_range_integer ( val ( __1 ) , val ( __2 ) )
  end | __Stack].

-compile({inline,{yeccpars2_216_,1}}).
-file("snmpc_mib_gram.yrl", 356).
yeccpars2_216_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 , __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_217_,1}}).
-file("snmpc_mib_gram.yrl", 350).
yeccpars2_217_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   make_range ( __4 )
  end | __Stack].

-compile({inline,{yeccpars2_220_,1}}).
-file("snmpc_mib_gram.yrl", 353).
yeccpars2_220_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 , __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_221_,1}}).
-file("snmpc_mib_gram.yrl", 354).
yeccpars2_221_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 , __4 | __5 ]
  end | __Stack].

-compile({inline,{yeccpars2_222_,1}}).
-file("snmpc_mib_gram.yrl", 349).
yeccpars2_222_([__3,__2,__1 | __Stack]) ->
 [begin
   make_range ( __2 )
  end | __Stack].

-compile({inline,{yeccpars2_223_,1}}).
-file("snmpc_mib_gram.yrl", 340).
yeccpars2_223_([__2,__1 | __Stack]) ->
 [begin
   { { type_with_size , val ( __1 ) , __2 } , line_of ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_226_,1}}).
-file("snmpc_mib_gram.yrl", 612).
yeccpars2_226_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_228_,1}}).
-file("snmpc_mib_gram.yrl", 612).
yeccpars2_228_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_232_,1}}).
-file("snmpc_mib_gram.yrl", 598).
yeccpars2_232_(__Stack) ->
 [begin
   undefined
  end | __Stack].

-compile({inline,{yeccpars2_235_,1}}).
-file("snmpc_mib_gram.yrl", 599).
yeccpars2_235_([__2,__1 | __Stack]) ->
 [begin
   units ( __2 )
  end | __Stack].

-compile({inline,{yeccpars2_238_,1}}).
-file("snmpc_mib_gram.yrl", 603).
yeccpars2_238_([__1 | __Stack]) ->
 [begin
   accessv2 ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_241_,1}}).
-file("snmpc_mib_gram.yrl", 407).
yeccpars2_241_(__Stack) ->
 [begin
   undefined
  end | __Stack].

-compile({inline,{yeccpars2_242_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_242_(__Stack) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_243_,1}}).
-file("snmpc_mib_gram.yrl", 588).
yeccpars2_243_(__Stack) ->
 [begin
   { indexes , undefined }
  end | __Stack].

-compile({inline,{yeccpars2_244_,1}}).
-file("snmpc_mib_gram.yrl", 442).
yeccpars2_244_(__Stack) ->
 [begin
   undefined
  end | __Stack].

-compile({inline,{yeccpars2_249_,1}}).
-file("snmpc_mib_gram.yrl", 590).
yeccpars2_249_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_253_,1}}).
-file("snmpc_mib_gram.yrl", 593).
yeccpars2_253_([__2,__1 | __Stack]) ->
 [begin
   { implied , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_255_,1}}).
-file("snmpc_mib_gram.yrl", 586).
yeccpars2_255_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { indexes , lists : reverse ( __3 ) }
  end | __Stack].

-compile({inline,{yeccpars2_256_,1}}).
-file("snmpc_mib_gram.yrl", 591).
yeccpars2_256_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __3 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_260_,1}}).
-file("snmpc_mib_gram.yrl", 587).
yeccpars2_260_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { augments , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_268_,1}}).
-file("snmpc_mib_gram.yrl", 445).
yeccpars2_268_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_271_,1}}).
-file("snmpc_mib_gram.yrl", 447).
yeccpars2_271_([__1 | __Stack]) ->
 [begin
   [ val ( __1 ) ]
  end | __Stack].

-compile({inline,{yeccpars2_273_,1}}).
-file("snmpc_mib_gram.yrl", 448).
yeccpars2_273_([__3,__2,__1 | __Stack]) ->
 [begin
   [ val ( __3 ) | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_275_,1}}).
-file("snmpc_mib_gram.yrl", 433).
yeccpars2_275_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { defval , __4 }
  end | __Stack].

-compile({inline,{yeccpars2_276_,1}}).
-file("snmpc_mib_gram.yrl", 441).
yeccpars2_276_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { defval , lists : reverse ( val ( __3 ) ) }
  end | __Stack].

-compile({inline,{yeccpars2_279_,1}}).
-file("snmpc_mib_gram.yrl", 438).
yeccpars2_279_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { defval , make_defval_for_string ( line_of ( __1 ) , lists : reverse ( val ( __3 ) ) ,
    val ( __4 ) ) }
  end | __Stack].

-compile({inline,{yeccpars2_280_,1}}).
-file("snmpc_mib_gram.yrl", 435).
yeccpars2_280_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { defval , make_defval_for_string ( line_of ( __1 ) , lists : reverse ( val ( __3 ) ) ,
    val ( __4 ) ) }
  end | __Stack].

-compile({inline,{yeccpars2_281_,1}}).
-file("snmpc_mib_gram.yrl", 431).
yeccpars2_281_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { defval , val ( __3 ) }
  end | __Stack].

-compile({inline,{yeccpars2_282_,1}}).
-file("snmpc_mib_gram.yrl", 432).
yeccpars2_282_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { defval , val ( __3 ) }
  end | __Stack].

-compile({inline,{yeccpars2_283_,1}}).
-file("snmpc_mib_gram.yrl", 581).
yeccpars2_283_([__15,__14,__13,__12,__11,__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   Kind = kind ( __14 , __13 ) ,
    OT = make_object_type ( __1 , __4 , __5 , __7 , __9 ,
    __11 , __12 , Kind , __15 ) ,
    { OT , line_of ( __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_287_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_287_(__Stack) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_289_,1}}).
-file("snmpc_mib_gram.yrl", 514).
yeccpars2_289_([__8,__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { Parent , SubIndex } = __8 ,
    Int = make_internal ( __1 , 'OBJECT-IDENTITY' ,
    Parent , SubIndex ) ,
    { Int , line_of ( __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_294_,1}}).
-file("snmpc_mib_gram.yrl", 614).
yeccpars2_294_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_296_,1}}).
-file("snmpc_mib_gram.yrl", 611).
yeccpars2_296_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   lists : reverse ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_297_,1}}).
-file("snmpc_mib_gram.yrl", 615).
yeccpars2_297_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __3 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_299_,1}}).
-file("snmpc_mib_gram.yrl", 409).
yeccpars2_299_(__Stack) ->
 [begin
   undefined
  end | __Stack].

-compile({inline,{yeccpars2_300_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_300_(__Stack) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_302_,1}}).
-file("snmpc_mib_gram.yrl", 521).
yeccpars2_302_([__8,__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   OG = make_object_group ( __1 , __3 , __5 , __6 , __7 , __8 ) ,
    { OG , line_of ( __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_304_,1}}).
-file("snmpc_mib_gram.yrl", 301).
yeccpars2_304_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { Parent , SubIndex } = __4 ,
    Int = make_internal ( __1 , dummy , Parent , SubIndex ) ,
    { Int , line_of ( __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_308_,1}}).
-file("snmpc_mib_gram.yrl", 407).
yeccpars2_308_(__Stack) ->
 [begin
   undefined
  end | __Stack].

-compile({inline,{yeccpars2_309_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_309_(__Stack) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_311_,1}}).
-file("snmpc_mib_gram.yrl", 608).
yeccpars2_311_([__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   Not = make_notification ( __1 , __3 , __5 , __7 , __8 , __9 ) ,
    { Not , line_of ( __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_317_,1}}).
-file("snmpc_mib_gram.yrl", 409).
yeccpars2_317_(__Stack) ->
 [begin
   undefined
  end | __Stack].

-compile({inline,{yeccpars2_318_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_318_(__Stack) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_320_,1}}).
-file("snmpc_mib_gram.yrl", 527).
yeccpars2_320_([__11,__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   NG = make_notification_group ( __1 , __5 , __8 , __9 ,
    __10 , __11 ) ,
    { NG , line_of ( __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_322_,1}}).
-file("snmpc_mib_gram.yrl", 409).
yeccpars2_322_(__Stack) ->
 [begin
   undefined
  end | __Stack].

-compile({inline,{yeccpars2_323_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_323_(__Stack) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_324_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_324_(__Stack) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_325_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_325_([__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_327_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_327_([__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_328_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_328_(__Stack) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_329_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_329_(__Stack) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_330_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_330_([__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_331_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_331_(__Stack) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_335_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_335_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_336_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_336_([__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_337_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_337_([__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_338_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_338_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_339_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_339_([__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_340_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_340_([__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_343_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_343_(__Stack) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_344_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_344_(__Stack) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_346_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_346_([__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_347_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_347_(__Stack) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_349_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_349_([__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_350_,1}}).
-file("snmpc_mib_gram.yrl", 409).
yeccpars2_350_(__Stack) ->
 [begin
   undefined
  end | __Stack].

-compile({inline,{yeccpars2_352_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_352_([__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_353_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_353_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_354_,1}}).
-file("snmpc_mib_gram.yrl", 409).
yeccpars2_354_(__Stack) ->
 [begin
   undefined
  end | __Stack].

-compile({inline,{yeccpars2_355_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_355_([__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_356_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_356_([__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_357_,1}}).
-file("snmpc_mib_gram.yrl", 533).
yeccpars2_357_([__8,__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   MC = make_module_compliance ( __1 , __4 , __5 , __6 ,
    __7 , __8 ) ,
    { MC , line_of ( __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_358_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_358_([__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_363_,1}}).
-file("snmpc_mib_gram.yrl", 399).
yeccpars2_363_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_364_,1}}).
-file("snmpc_mib_gram.yrl", 409).
yeccpars2_364_(__Stack) ->
 [begin
   undefined
  end | __Stack].

-compile({inline,{yeccpars2_368_,1}}).
-file("snmpc_mib_gram.yrl", 401).
yeccpars2_368_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_370_,1}}).
-file("snmpc_mib_gram.yrl", 400).
yeccpars2_370_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_371_,1}}).
-file("snmpc_mib_gram.yrl", 402).
yeccpars2_371_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __3 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_372_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_372_(__Stack) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_375_,1}}).
-file("snmpc_mib_gram.yrl", 295).
yeccpars2_375_([__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   Trap = make_trap ( __1 , __4 , lists : reverse ( __5 ) ,
    __6 , __7 , val ( __9 ) ) ,
    { Trap , line_of ( __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_380_,1}}).
-file("snmpc_mib_gram.yrl", 455).
yeccpars2_380_([__1 | __Stack]) ->
 [begin
   accessv1 ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_383_,1}}).
-file("snmpc_mib_gram.yrl", 457).
yeccpars2_383_([__1 | __Stack]) ->
 [begin
   statusv1 ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_384_,1}}).
-file("snmpc_mib_gram.yrl", 407).
yeccpars2_384_(__Stack) ->
 [begin
   undefined
  end | __Stack].

-compile({inline,{yeccpars2_385_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_385_(__Stack) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_386_,1}}).
-file("snmpc_mib_gram.yrl", 417).
yeccpars2_386_(__Stack) ->
 [begin
   { indexes , undefined }
  end | __Stack].

-compile({inline,{yeccpars2_387_,1}}).
-file("snmpc_mib_gram.yrl", 442).
yeccpars2_387_(__Stack) ->
 [begin
   undefined
  end | __Stack].

-compile({inline,{yeccpars2_390_,1}}).
-file("snmpc_mib_gram.yrl", 419).
yeccpars2_390_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_394_,1}}).
-file("snmpc_mib_gram.yrl", 416).
yeccpars2_394_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { indexes , lists : reverse ( __3 ) }
  end | __Stack].

-compile({inline,{yeccpars2_395_,1}}).
-file("snmpc_mib_gram.yrl", 420).
yeccpars2_395_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __3 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_397_,1}}).
-file("snmpc_mib_gram.yrl", 313).
yeccpars2_397_([__14,__13,__12,__11,__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   Kind = kind ( __13 , __12 ) ,
    OT = make_object_type ( __1 , __4 , __6 , __8 , __10 ,
    __11 , Kind , __14 ) ,
    { OT , line_of ( __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_398_,1}}).
-file("snmpc_mib_gram.yrl", 204).
yeccpars2_398_([__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { Version , Defs } = __6 ,
    # pdata { mib_version = Version ,
    mib_name = __1 ,
    imports = __5 ,
    defs = Defs }
  end | __Stack].


-file("snmpc_mib_gram.yrl", 970).
