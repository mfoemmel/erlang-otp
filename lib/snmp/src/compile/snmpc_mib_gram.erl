-module(snmpc_mib_gram).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("snmpc_mib_gram.yrl", 624).
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


-file("/ldisk/daily_build/otp_prebuild_r12b.2007-12-04_15/otp_src_R12B-0/bootstrap/lib/parsetools/include/yeccpre.hrl", 0).
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
-spec(return_error/2 :: (integer(), any()) -> no_return()).
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.2").

yeccpars0(Tokens, MFA) ->
    try yeccpars1(Tokens, MFA, 0, [], [])
    catch 
        error: Error ->
            Stacktrace = erlang:get_stacktrace(),
            try yecc_error_type(Error, Stacktrace) of
                {syntax_error, Token} ->
                    yeccerror(Token);
                {missing_in_goto_table=Tag, State} ->
                    Desc = {State, Tag},
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                Stacktrace);
                {missing_in_goto_table=Tag, Symbol, State} ->
                    Desc = {Symbol, State, Tag},
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        throw: {error, {_Line, ?MODULE, _M}} = Error -> 
            Error % probably from return_error/2
    end.

yecc_error_type(function_clause, [{?MODULE,F,[_,_,_,_,Token,_,_]} | _]) ->
    "yeccpars2" ++ _ = atom_to_list(F),
    {syntax_error, Token};
yecc_error_type({case_clause,{State}}, [{?MODULE,yeccpars2,_}|_]) ->
    %% Inlined goto-function
    {missing_in_goto_table, State};
yecc_error_type(function_clause, [{?MODULE,F,[State]}|_]) ->
    "yeccgoto_" ++ SymbolL = atom_to_list(F),
    {ok,[{atom,_,Symbol}]} = erl_scan:string(SymbolL),
    {missing_in_goto_table, Symbol, State}.

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

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Stack1, [Token | Tokens], 
          Tokenizer) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Stack1 | Vstack], Token, Tokens, Tokenizer);
yeccpars1(State1, State, States, Vstack, Stack1, [], {F, A}) ->
    case apply(F, A) of
        {ok, Tokens, _Endline} ->
	    yeccpars1(State1, State, States, Vstack, Stack1, Tokens, {F, A});
        {eof, _Endline} ->
            yeccpars1(State1, State, States, Vstack, Stack1, [], false);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1(State1, State, States, Vstack, Stack1, [], false) ->
    yeccpars2(State, '$end', [State1 | States], [Stack1 | Vstack],
              {'$end', 999999}, [], false).

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



-file("./snmpc_mib_gram.erl", 497).

yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(120=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(125=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(126=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(128=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(130=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(147=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(148=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(149=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(150=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(151=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(152=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(153=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(154=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(155=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(156=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(157=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(158=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(159=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(160=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(161=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(162=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(163=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(164=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_164(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(165=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(166=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(167=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(168=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(169=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(170=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(171=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_171(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(172=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(173=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(174=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(175=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(176=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(177=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(178=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(179=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(180=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(181=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(182=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_182(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(183=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(184=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(185=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_185(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(186=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_186(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(187=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_187(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(188=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(189=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(190=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_190(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(191=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_191(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(192=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(193=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(194=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_194(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(195=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(196=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_196(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(197=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_197(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(198=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(199=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(200=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(201=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(202=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(203=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(204=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(205=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(206=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(207=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_207(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(208=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(209=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_209(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(210=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_210(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(211=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_211(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(212=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(213=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(214=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(215=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(216=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_216(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(217=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(218=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_218(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(219=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(220=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_220(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(221=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(222=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(223=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_223(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(224=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_224(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(225=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_225(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(226=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_226(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(227=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(228=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(229=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(230=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(231=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(232=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_232(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(233=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_233(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(234=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_234(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(235=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_235(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(236=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_236(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(237=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_237(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(238=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_238(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(239=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(240=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_240(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(241=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_241(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(242=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_242(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(243=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(244=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(245=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_245(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(246=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_246(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(247=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_247(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(248=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_248(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(249=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_249(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(250=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_250(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(251=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_251(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(252=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(253=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_253(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(254=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_247(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(255=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_255(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(256=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_256(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(257=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(258=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(259=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_259(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(260=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_260(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(261=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(262=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_262(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(263=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_263(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(264=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_264(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(265=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_265(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(266=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_266(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(267=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_267(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(268=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_268(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(269=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_269(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(270=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_270(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(271=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_271(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(272=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_272(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(273=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_273(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(274=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_274(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(275=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_275(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(276=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_276(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(277=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_277(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(278=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_278(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(279=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_279(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(280=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_280(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(281=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_281(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(282=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_282(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(283=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_283(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(284=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(285=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_285(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(286=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_286(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(287=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_287(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(288=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(289=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_289(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(290=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_290(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(291=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_291(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(292=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(293=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_293(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(294=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_294(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(295=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(296=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_296(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(297=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_297(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(298=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(299=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_299(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(300=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_300(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(301=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(302=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_302(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(303=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(304=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_304(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(305=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_305(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(306=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(307=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_307(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(308=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_308(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(309=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_309(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(310=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(311=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_311(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(312=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_312(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(313=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(314=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_314(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(315=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_315(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(316=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(317=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_317(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(318=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_318(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(319=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(320=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_320(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(321=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(322=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_322(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(323=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_323(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(324=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_324(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(325=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_325(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(326=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(327=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_327(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(328=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_328(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(329=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_329(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(330=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_330(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(331=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_331(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(332=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_332(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(333=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(334=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_334(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(335=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_335(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(336=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_336(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(337=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_337(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(338=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_338(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(339=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_339(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(340=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_340(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(341=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(342=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(343=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(344=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(345=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(346=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_346(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(347=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_347(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(348=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(349=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(350=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(351=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_236(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(352=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_352(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(353=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_353(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(354=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_354(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(355=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_355(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(356=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_356(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(357=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_357(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(358=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_358(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(359=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_359(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(360=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_360(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(361=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_361(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(362=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(363=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_363(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(364=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_364(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(365=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_365(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(366=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(367=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_367(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(368=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_368(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(369=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(370=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_370(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(371=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_371(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(372=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_372(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(373=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(374=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_374(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(375=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_375(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(376=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(377=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_377(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(378=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_378(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(379=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_379(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(380=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_380(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(381=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_381(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(382=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_382(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(383=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_383(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(384=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_384(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(385=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_385(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(386=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_386(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(387=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_387(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(388=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_388(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(389=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(390=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_390(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(391=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_391(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(392=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_392(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(393=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(394=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_394(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(395=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_395(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(396=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(397=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_397(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(398=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_398(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.2",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr).

yeccpars2_1(S, 'DEFINITIONS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 4, Ss, Stack, T, Ts, Tzr).

yeccpars2_2(_S, '$end', _Ss, Stack,  _T, _Ts, _Tzr) ->
 {ok, hd(Stack)}.

yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_3_(Stack),
 yeccpars2(yeccgoto_mibname(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_4(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, '::=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr).

yeccpars2_5(S, 'BEGIN', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr).

yeccpars2_6(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr).

yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_implies(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_8(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr).

yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_9_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_implies(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_10(S, 'IMPORTS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_(Stack),
 yeccpars2(11, Cat, [10 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_11(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr).

yeccpars2_12(S, 'AutonomousType', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'Counter', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'DateAndTime', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'DisplayString', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'Gauge', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'InstancePointer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'IpAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'MODULE-COMPLIANCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'MODULE-IDENTITY', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'MacAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'NOTIFICATION-GROUP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'NOTIFICATION-TYPE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'NetworkAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'OBJECT-GROUP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'OBJECT-IDENTITY', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'OBJECT-TYPE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'Opaque', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'PhysAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'RowPointer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'RowStatus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'StorageType', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'TAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'TDomain', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'TEXTUAL-CONVENTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'TRAP-TYPE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'TestAndIncr', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'TimeInterval', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'TimeStamp', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'TimeTicks', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'TruthValue', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, 'VariablePointer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr).

yeccpars2_13(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(S, 'FROM', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr).

yeccpars2_14(S, 'AutonomousType', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'Counter', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'DateAndTime', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'DisplayString', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'Gauge', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'InstancePointer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'IpAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'MODULE-COMPLIANCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'MODULE-IDENTITY', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'MacAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'NOTIFICATION-GROUP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'NOTIFICATION-TYPE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'NetworkAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'OBJECT-GROUP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'OBJECT-IDENTITY', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'OBJECT-TYPE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'Opaque', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'PhysAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'RowPointer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'RowStatus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'StorageType', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'TAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'TDomain', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'TEXTUAL-CONVENTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'TRAP-TYPE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'TestAndIncr', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'TimeInterval', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'TimeStamp', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'TimeTicks', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'TruthValue', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'VariablePointer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_14_(Stack),
 yeccpars2(yeccgoto_imports(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_15(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr).

yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_16_(Stack),
 yeccpars2(yeccgoto_listofimports(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_17_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_18_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_19_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_20_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_21_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_22_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_23_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_24_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_25_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_26_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_27_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_28_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_29_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_30_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_32_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_33_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_34_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_35_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_36_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_37_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_38_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_39_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_40_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_41_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_42_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_43_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_44_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_45_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_46_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_47_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_48_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_49_(Stack),
 yeccpars2(yeccgoto_import_stuff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_50_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_import(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_51_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_imports(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_52: see yeccpars2_12

yeccpars2_53(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr).

yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_54_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_imports_from_one_mib(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_55_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_listofimports(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_56(S, 'END', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 398, Ss, Stack, T, Ts, Tzr).

yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_definition(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_definition(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_definition(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_60(S, 'OBJECT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 227, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(S, 'OBJECT-TYPE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 360, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(S, 'TRAP-TYPE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 361, Ss, Stack, T, Ts, Tzr).

yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_definition(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_62: see yeccpars2_4

yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_definition(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_64_(Stack),
 yeccpars2(109, Cat, [64 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_65(S, 'MODULE-IDENTITY', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr).

yeccpars2_66(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_66_(Stack),
 yeccpars2(yeccgoto_v1orv2(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_67_(Stack),
 yeccpars2(yeccgoto_listofdefinitions(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_68(_S, 'MODULE-IDENTITY', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_68_MODULE-IDENTITY'(Stack),
 yeccpars2(yeccgoto_mibid(hd(Ss)), 'MODULE-IDENTITY', Ss, NewStack, T, Ts, Tzr);
yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_68_(Stack),
 yeccpars2(yeccgoto_objectname(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_69_(Stack),
 yeccpars2(yeccgoto_newtypename(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_70_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_listofdefinitions(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_71_(Stack),
 yeccpars2(yeccgoto_objectname(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_72(S, 'LAST-UPDATED', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr).

yeccpars2_73(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr).

yeccpars2_74(S, 'ORGANIZATION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr).

yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_75_(Stack),
 yeccpars2(yeccgoto_last_updated(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_76(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr).

yeccpars2_77(S, 'CONTACT-INFO', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_78_(Stack),
 yeccpars2(yeccgoto_oranization(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_79(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr).

yeccpars2_80(S, 'DESCRIPTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr).

yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_81_(Stack),
 yeccpars2(yeccgoto_contact_info(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_82(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_82_(Stack),
 yeccpars2(83, Cat, [82 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_83(S, 'REVISION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_83_(Stack),
 yeccpars2(86, Cat, [83 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_84_(Stack),
 yeccpars2(yeccgoto_descriptionfield(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_85(S, 'REVISION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_85_(Stack),
 yeccpars2(yeccgoto_revisionpart(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_86: see yeccpars2_4

yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_87_(Stack),
 yeccpars2(yeccgoto_revisions(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_88(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr).

yeccpars2_89(S, 'DESCRIPTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr).

yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_90_(Stack),
 yeccpars2(yeccgoto_revision_string(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_91(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr).

yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_92_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_revision(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_93_(Stack),
 yeccpars2(yeccgoto_revision_desc(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_(Stack),
 Nss = lists:nthtail(11, Ss),
 yeccpars2(yeccgoto_moduleidentity(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_95(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr).

yeccpars2_96(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr).

yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_fatherobjectname(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_98(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_98(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr).

yeccpars2_99(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr).

yeccpars2_100(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr).

yeccpars2_101(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_(Stack),
 yeccpars2(yeccgoto_parentintegers(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_parentintegers(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_103(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr).

yeccpars2_104(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr).

yeccpars2_105(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_105_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_parentintegers(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_parentintegers(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_nameassign(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_108_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_revisions(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_109(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_109_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_v1orv2(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_definitionv2(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_definitionv2(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_definitionv2(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_113(S, 'MODULE-COMPLIANCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 224, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(S, 'NOTIFICATION-GROUP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 225, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(S, 'NOTIFICATION-TYPE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 226, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(S, 'OBJECT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 227, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(S, 'OBJECT-GROUP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 228, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(S, 'OBJECT-IDENTITY', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 229, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(S, 'OBJECT-TYPE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 230, Ss, Stack, T, Ts, Tzr).

yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_definitionv2(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_definitionv2(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_definitionv2(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_definitionv2(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_definitionv2(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_119: see yeccpars2_4

yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_definitionv2(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_definitionv2(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_122_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_listofdefinitionsv2(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_123(S, 'AutonomousType', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 127, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'BIT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'BITS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 129, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'Counter', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'DateAndTime', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'DisplayString', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'Gauge', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'INTEGER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'InstancePointer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'IpAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'MacAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'NetworkAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'OBJECT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'OCTET', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'Opaque', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'PhysAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'RowPointer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 143, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'RowStatus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 144, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'SEQUENCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 145, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'StorageType', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 146, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'TAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 147, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'TDomain', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'TEXTUAL-CONVENTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 149, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'TestAndIncr', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 150, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'TimeInterval', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 151, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'TimeStamp', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 152, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'TimeTicks', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 153, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'TruthValue', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 154, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'VariablePointer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 155, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr).

yeccpars2_124(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_124_(Stack),
 yeccpars2(yeccgoto_syntax(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_125(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_(Stack),
 yeccpars2(yeccgoto_syntax(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_126_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_newtype(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_127_(Stack),
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_128(S, 'STRING', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr).

yeccpars2_129(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr).

yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_130_(Stack),
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_131_(Stack),
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_132_(Stack),
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_133_(Stack),
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_134(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_135_(Stack),
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_137_(Stack),
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_138(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_139(S, 'IDENTIFIER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr).

yeccpars2_140(S, 'STRING', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr).

yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_142_(Stack),
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_143_(Stack),
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_144_(Stack),
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_145(S, 'OF', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr).

yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_146_(Stack),
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_147(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_147_(Stack),
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_148(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_148_(Stack),
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_149(S, 'DISPLAY-HINT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_149_(Stack),
 yeccpars2(157, Cat, [149 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_(Stack),
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_151_(Stack),
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_152_(Stack),
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_154_(Stack),
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_155_(Stack),
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_156(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_usertype(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_157(S, 'STATUS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 160, Ss, Stack, T, Ts, Tzr).

yeccpars2_158(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 159, Ss, Stack, T, Ts, Tzr).

yeccpars2_159(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_159_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_displaypart(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_160(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr).

yeccpars2_161(S, 'DESCRIPTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_161_(Stack),
 yeccpars2(163, Cat, [161 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_162_(Stack),
 yeccpars2(yeccgoto_statusv2(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_163(S, 'REFERENCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_163_(Stack),
 yeccpars2(166, Cat, [163 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_164(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 165, Ss, Stack, T, Ts, Tzr).

yeccpars2_165(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_165_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_description(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_166(S, 'SYNTAX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr).

yeccpars2_167(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 168, Ss, Stack, T, Ts, Tzr).

yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_168_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_referpart(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_169(S, 'AutonomousType', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 127, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'BIT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'BITS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 129, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'Counter', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'DateAndTime', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'DisplayString', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'Gauge', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'INTEGER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'InstancePointer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'IpAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'MacAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'NetworkAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'OBJECT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'OCTET', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'Opaque', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'PhysAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'RowPointer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 143, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'RowStatus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 144, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'SEQUENCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 171, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'StorageType', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 146, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'TAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 147, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'TDomain', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'TestAndIncr', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 150, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'TimeInterval', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 151, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'TimeStamp', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 152, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'TimeTicks', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 153, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'TruthValue', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 154, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, 'VariablePointer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 155, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr).

yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_170_(Stack),
 Nss = lists:nthtail(9, Ss),
 yeccpars2(yeccgoto_textualconvention(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_171(S, 'OF', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr).

yeccpars2_172(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr).

yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_173_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_syntax(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_174(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 177, Ss, Stack, T, Ts, Tzr).

yeccpars2_175(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr).

yeccpars2_176(S, 'AutonomousType', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 127, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'BIT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'BITS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'Counter', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'DateAndTime', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'DisplayString', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'Gauge', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'INTEGER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'InstancePointer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'IpAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'MacAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'NetworkAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'OBJECT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'OCTET', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'Opaque', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'PhysAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'RowPointer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 143, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'RowStatus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 144, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'SEQUENCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 171, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'StorageType', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 146, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'TAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 147, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'TDomain', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'TestAndIncr', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 150, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'TimeInterval', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 151, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'TimeStamp', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 152, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'TimeTicks', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 153, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'TruthValue', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 154, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'VariablePointer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 155, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr).

yeccpars2_177(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_fieldname(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_fsyntax(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_179(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_179_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_fields(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_180(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_180_(Stack),
 yeccpars2(yeccgoto_fsyntax(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_181(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr).

yeccpars2_182(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr).

yeccpars2_183(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr).

yeccpars2_184(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr).

yeccpars2_185(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr).

yeccpars2_186(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_186_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_namedbits(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_187(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr).

yeccpars2_188(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_188_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_syntax(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_189(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr).

yeccpars2_190(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr).

yeccpars2_191(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr).

yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_192_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_namedbits(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_193: see yeccpars2_174

yeccpars2_194(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_194_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_tableentrydefinition(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_195: see yeccpars2_176

yeccpars2_196(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_196_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_fields(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_197(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_197_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_198_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_199: see yeccpars2_181

yeccpars2_200(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_200(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr).

yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_201_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_syntax(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_202_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_203(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_203_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_syntax(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_204(S, 'SIZE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 207, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 208, Ss, Stack, T, Ts, Tzr);
yeccpars2_204(S, quote, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 209, Ss, Stack, T, Ts, Tzr).

yeccpars2_205(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 222, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 215, Ss, Stack, T, Ts, Tzr).

yeccpars2_206(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 218, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_(Stack),
 yeccpars2(yeccgoto_sizedescr(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_207(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 212, Ss, Stack, T, Ts, Tzr).

yeccpars2_208(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_208_(Stack),
 yeccpars2(yeccgoto_range_num(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_209(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 210, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 211, Ss, Stack, T, Ts, Tzr).

yeccpars2_210(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_210_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_range_num(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_211(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_211_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_range_num(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_212(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 208, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, quote, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 209, Ss, Stack, T, Ts, Tzr).

yeccpars2_213(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 214, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 215, Ss, Stack, T, Ts, Tzr).

yeccpars2_214(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 217, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_215: see yeccpars2_212

yeccpars2_216(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 215, Ss, Stack, T, Ts, Tzr);
yeccpars2_216(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_216_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_sizedescr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_217_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_size(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_218(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 219, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_219: see yeccpars2_212

yeccpars2_220(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 208, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(S, quote, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 209, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_220_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_sizedescr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_221(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 215, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_221_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_sizedescr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_222_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_size(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_223(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_223_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_syntax(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_224(S, 'STATUS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 321, Ss, Stack, T, Ts, Tzr).

yeccpars2_225(S, 'NOTIFICATIONS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 312, Ss, Stack, T, Ts, Tzr).

yeccpars2_226(S, 'OBJECTS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 291, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_226_(Stack),
 yeccpars2(305, Cat, [226 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_227(S, 'IDENTIFIER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 303, Ss, Stack, T, Ts, Tzr).

yeccpars2_228(S, 'OBJECTS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 291, Ss, Stack, T, Ts, Tzr);
yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_228_(Stack),
 yeccpars2(290, Cat, [228 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_229(S, 'STATUS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 284, Ss, Stack, T, Ts, Tzr).

yeccpars2_230(S, 'SYNTAX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 231, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_231: see yeccpars2_169

yeccpars2_232(S, 'UNITS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 234, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_232_(Stack),
 yeccpars2(233, Cat, [232 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_233(S, 'MAX-ACCESS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 236, Ss, Stack, T, Ts, Tzr).

yeccpars2_234(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 235, Ss, Stack, T, Ts, Tzr).

yeccpars2_235(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_235_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_unitspart(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_236(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 238, Ss, Stack, T, Ts, Tzr).

yeccpars2_237(S, 'STATUS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 239, Ss, Stack, T, Ts, Tzr).

yeccpars2_238(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_238_(Stack),
 yeccpars2(yeccgoto_accessv2(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_239: see yeccpars2_160

yeccpars2_240(S, 'DESCRIPTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 241, Ss, Stack, T, Ts, Tzr).

yeccpars2_241(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_241_(Stack),
 yeccpars2(242, Cat, [241 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_242(S, 'REFERENCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr);
yeccpars2_242(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_242_(Stack),
 yeccpars2(243, Cat, [242 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_243(S, 'AUGMENTS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 245, Ss, Stack, T, Ts, Tzr);
yeccpars2_243(S, 'INDEX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 246, Ss, Stack, T, Ts, Tzr);
yeccpars2_243(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_243_(Stack),
 yeccpars2(244, Cat, [243 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_244(S, 'DEFVAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 262, Ss, Stack, T, Ts, Tzr);
yeccpars2_244(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_244_(Stack),
 yeccpars2(261, Cat, [244 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_245(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 257, Ss, Stack, T, Ts, Tzr).

yeccpars2_246(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 247, Ss, Stack, T, Ts, Tzr).

yeccpars2_247(S, 'IMPLIED', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 252, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr).

yeccpars2_248(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_index(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_249(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_249_(Stack),
 yeccpars2(yeccgoto_indextypesv2(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_250(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 254, Ss, Stack, T, Ts, Tzr);
yeccpars2_250(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 255, Ss, Stack, T, Ts, Tzr).

yeccpars2_251(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_indextypev2(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_252: see yeccpars2_96

yeccpars2_253(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_253_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_indextypev2(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_254: see yeccpars2_247

yeccpars2_255(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_255_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_indexpartv2(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_256(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_256_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_indextypesv2(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_257: see yeccpars2_96

yeccpars2_258(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_entry(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_259(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 260, Ss, Stack, T, Ts, Tzr).

yeccpars2_260(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_260_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_indexpartv2(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_261: see yeccpars2_4

yeccpars2_262(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 263, Ss, Stack, T, Ts, Tzr).

yeccpars2_263(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 264, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 265, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(S, quote, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 266, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 267, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 268, Ss, Stack, T, Ts, Tzr).

yeccpars2_264(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 282, Ss, Stack, T, Ts, Tzr).

yeccpars2_265(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 281, Ss, Stack, T, Ts, Tzr).

yeccpars2_266(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 277, Ss, Stack, T, Ts, Tzr);
yeccpars2_266(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 278, Ss, Stack, T, Ts, Tzr).

yeccpars2_267(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 276, Ss, Stack, T, Ts, Tzr).

yeccpars2_268(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 271, Ss, Stack, T, Ts, Tzr);
yeccpars2_268(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_268_(Stack),
 yeccpars2(269, Cat, [268 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_269(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 274, Ss, Stack, T, Ts, Tzr).

yeccpars2_270(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 272, Ss, Stack, T, Ts, Tzr);
yeccpars2_270(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_defbitsvalue(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_271(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_271_(Stack),
 yeccpars2(yeccgoto_defbitsnames(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_272(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 273, Ss, Stack, T, Ts, Tzr).

yeccpars2_273(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_273_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_defbitsnames(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_274(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 275, Ss, Stack, T, Ts, Tzr).

yeccpars2_275(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_275_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_defvalpart(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_276(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_276_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_defvalpart(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_277(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 280, Ss, Stack, T, Ts, Tzr).

yeccpars2_278(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 279, Ss, Stack, T, Ts, Tzr).

yeccpars2_279(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_279_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_defvalpart(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_280(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_280_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_defvalpart(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_281(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_281_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_defvalpart(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_282(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_282_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_defvalpart(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_283(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_283_(Stack),
 Nss = lists:nthtail(14, Ss),
 yeccpars2(yeccgoto_objecttypev2(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_284: see yeccpars2_160

yeccpars2_285(S, 'DESCRIPTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 286, Ss, Stack, T, Ts, Tzr).

yeccpars2_286(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 287, Ss, Stack, T, Ts, Tzr).

yeccpars2_287(S, 'REFERENCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_287_(Stack),
 yeccpars2(288, Cat, [287 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_288: see yeccpars2_4

yeccpars2_289(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_289_(Stack),
 Nss = lists:nthtail(7, Ss),
 yeccpars2(yeccgoto_objectidentity(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_290(S, 'STATUS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 298, Ss, Stack, T, Ts, Tzr).

yeccpars2_291(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 292, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_292: see yeccpars2_96

yeccpars2_293(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 295, Ss, Stack, T, Ts, Tzr);
yeccpars2_293(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 296, Ss, Stack, T, Ts, Tzr).

yeccpars2_294(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_294_(Stack),
 yeccpars2(yeccgoto_objects(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_295: see yeccpars2_96

yeccpars2_296(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_296_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_objectspart(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_297(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_297_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_objects(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_298: see yeccpars2_160

yeccpars2_299(S, 'DESCRIPTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_299(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_299_(Stack),
 yeccpars2(300, Cat, [299 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_300(S, 'REFERENCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr);
yeccpars2_300(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_300_(Stack),
 yeccpars2(301, Cat, [300 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_301: see yeccpars2_4

yeccpars2_302(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_302_(Stack),
 Nss = lists:nthtail(7, Ss),
 yeccpars2(yeccgoto_objectgroup(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_303: see yeccpars2_4

yeccpars2_304(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_304_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_objectidentifier(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_305(S, 'STATUS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 306, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_306: see yeccpars2_160

yeccpars2_307(S, 'DESCRIPTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 308, Ss, Stack, T, Ts, Tzr).

yeccpars2_308(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_308(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_308_(Stack),
 yeccpars2(309, Cat, [308 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_309(S, 'REFERENCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr);
yeccpars2_309(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_309_(Stack),
 yeccpars2(310, Cat, [309 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_310: see yeccpars2_4

yeccpars2_311(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_(Stack),
 Nss = lists:nthtail(8, Ss),
 yeccpars2(yeccgoto_notification(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_312(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 313, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_313: see yeccpars2_96

yeccpars2_314(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 295, Ss, Stack, T, Ts, Tzr);
yeccpars2_314(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 315, Ss, Stack, T, Ts, Tzr).

yeccpars2_315(S, 'STATUS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 316, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_316: see yeccpars2_160

yeccpars2_317(S, 'DESCRIPTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_317(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_317_(Stack),
 yeccpars2(318, Cat, [317 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_318(S, 'REFERENCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr);
yeccpars2_318(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_318_(Stack),
 yeccpars2(319, Cat, [318 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_319: see yeccpars2_4

yeccpars2_320(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_320_(Stack),
 Nss = lists:nthtail(10, Ss),
 yeccpars2(yeccgoto_notificationgroup(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_321: see yeccpars2_160

yeccpars2_322(S, 'DESCRIPTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_322_(Stack),
 yeccpars2(323, Cat, [322 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_323(S, 'REFERENCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr);
yeccpars2_323(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_323_(Stack),
 yeccpars2(324, Cat, [323 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_324(S, 'MODULE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 328, Ss, Stack, T, Ts, Tzr);
yeccpars2_324(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_324_(Stack),
 yeccpars2(326, Cat, [324 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_325(S, 'MODULE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 328, Ss, Stack, T, Ts, Tzr);
yeccpars2_325(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_325_(Stack),
 yeccpars2(yeccgoto_modulepart(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_326: see yeccpars2_4

yeccpars2_327(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_327_(Stack),
 yeccpars2(yeccgoto_modules(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_328(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_328(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_328_(Stack),
 yeccpars2(329, Cat, [328 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_329(S, 'MANDATORY-GROUPS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 332, Ss, Stack, T, Ts, Tzr);
yeccpars2_329(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_329_(Stack),
 yeccpars2(331, Cat, [329 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_330(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_330_(Stack),
 yeccpars2(yeccgoto_modulenamepart(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_331(S, 'GROUP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 341, Ss, Stack, T, Ts, Tzr);
yeccpars2_331(S, 'OBJECT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 342, Ss, Stack, T, Ts, Tzr);
yeccpars2_331(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_331_(Stack),
 yeccpars2(338, Cat, [331 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_332(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 333, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_333: see yeccpars2_96

yeccpars2_334(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 295, Ss, Stack, T, Ts, Tzr);
yeccpars2_334(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 335, Ss, Stack, T, Ts, Tzr).

yeccpars2_335(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_335_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_mandatorypart(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_336(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_336_(Stack),
 yeccpars2(yeccgoto_compliance(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_337(S, 'GROUP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 341, Ss, Stack, T, Ts, Tzr);
yeccpars2_337(S, 'OBJECT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 342, Ss, Stack, T, Ts, Tzr);
yeccpars2_337(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_337_(Stack),
 yeccpars2(yeccgoto_compliancepart(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_338(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_338_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_module(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_339(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_339_(Stack),
 yeccpars2(yeccgoto_compliance(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_340(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_340_(Stack),
 yeccpars2(yeccgoto_compliances(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_341: see yeccpars2_96

%% yeccpars2_342: see yeccpars2_96

yeccpars2_343(S, 'SYNTAX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 345, Ss, Stack, T, Ts, Tzr);
yeccpars2_343(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_343_(Stack),
 yeccpars2(344, Cat, [343 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_344(S, 'WRITE-SYNTAX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 348, Ss, Stack, T, Ts, Tzr);
yeccpars2_344(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_344_(Stack),
 yeccpars2(347, Cat, [344 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_345: see yeccpars2_169

yeccpars2_346(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_346_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_syntaxpart(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_347(S, 'MIN-ACCESS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 351, Ss, Stack, T, Ts, Tzr);
yeccpars2_347(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_347_(Stack),
 yeccpars2(350, Cat, [347 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_348: see yeccpars2_169

yeccpars2_349(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_349_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_writesyntaxpart(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_350(S, 'DESCRIPTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_350_(Stack),
 yeccpars2(353, Cat, [350 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_351: see yeccpars2_236

yeccpars2_352(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_352_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_accesspart(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_353(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_353_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_object(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_354(S, 'DESCRIPTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_354(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_354_(Stack),
 yeccpars2(355, Cat, [354 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_355(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_355_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_compliancegroup(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_356(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_356_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_compliances(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_357(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_357_(Stack),
 Nss = lists:nthtail(7, Ss),
 yeccpars2(yeccgoto_modulecompliance(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_358(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_358_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_modules(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_359(S, 'AutonomousType', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 127, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(S, 'BIT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(S, 'BITS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 129, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(S, 'Counter', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(S, 'DateAndTime', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(S, 'DisplayString', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(S, 'Gauge', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(S, 'INTEGER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(S, 'InstancePointer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(S, 'IpAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(S, 'MacAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(S, 'NetworkAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(S, 'OBJECT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(S, 'OCTET', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(S, 'Opaque', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(S, 'PhysAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(S, 'RowPointer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 143, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(S, 'RowStatus', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 144, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(S, 'SEQUENCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 145, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(S, 'StorageType', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 146, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(S, 'TAddress', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 147, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(S, 'TDomain', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(S, 'TestAndIncr', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 150, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(S, 'TimeInterval', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 151, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(S, 'TimeStamp', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 152, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(S, 'TimeTicks', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 153, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(S, 'TruthValue', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 154, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(S, 'VariablePointer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 155, Ss, Stack, T, Ts, Tzr);
yeccpars2_359(S, variable, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr).

yeccpars2_360(S, 'SYNTAX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 376, Ss, Stack, T, Ts, Tzr).

yeccpars2_361(S, 'ENTERPRISE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 362, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_362: see yeccpars2_96

yeccpars2_363(S, 'VARIABLES', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 365, Ss, Stack, T, Ts, Tzr);
yeccpars2_363(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_363_(Stack),
 yeccpars2(364, Cat, [363 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_364(S, 'DESCRIPTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_364(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_364_(Stack),
 yeccpars2(372, Cat, [364 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_365(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 366, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_366: see yeccpars2_96

yeccpars2_367(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 369, Ss, Stack, T, Ts, Tzr);
yeccpars2_367(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 370, Ss, Stack, T, Ts, Tzr).

yeccpars2_368(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_368_(Stack),
 yeccpars2(yeccgoto_variables(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_369: see yeccpars2_96

yeccpars2_370(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_varpart(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_371(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_371_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_variables(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_372(S, 'REFERENCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr);
yeccpars2_372(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_372_(Stack),
 yeccpars2(373, Cat, [372 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_373: see yeccpars2_4

yeccpars2_374(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 375, Ss, Stack, T, Ts, Tzr).

yeccpars2_375(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_375_(Stack),
 Nss = lists:nthtail(8, Ss),
 yeccpars2(yeccgoto_traptype(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_376: see yeccpars2_169

yeccpars2_377(S, 'ACCESS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 378, Ss, Stack, T, Ts, Tzr).

yeccpars2_378(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 380, Ss, Stack, T, Ts, Tzr).

yeccpars2_379(S, 'STATUS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 381, Ss, Stack, T, Ts, Tzr).

yeccpars2_380(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_380_(Stack),
 yeccpars2(yeccgoto_accessv1(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_381(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 383, Ss, Stack, T, Ts, Tzr).

yeccpars2_382(S, 'DESCRIPTION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 384, Ss, Stack, T, Ts, Tzr).

yeccpars2_383(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_383_(Stack),
 yeccpars2(yeccgoto_statusv1(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_384(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_384(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_384_(Stack),
 yeccpars2(385, Cat, [384 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_385(S, 'REFERENCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_(Stack),
 yeccpars2(386, Cat, [385 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_386(S, 'INDEX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 388, Ss, Stack, T, Ts, Tzr);
yeccpars2_386(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_386_(Stack),
 yeccpars2(387, Cat, [386 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_387(S, 'DEFVAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 262, Ss, Stack, T, Ts, Tzr);
yeccpars2_387(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_387_(Stack),
 yeccpars2(396, Cat, [387 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_388(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 389, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_389: see yeccpars2_96

yeccpars2_390(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_390_(Stack),
 yeccpars2(yeccgoto_indextypesv1(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_391(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 393, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 394, Ss, Stack, T, Ts, Tzr).

yeccpars2_392(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_indextypev1(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_393: see yeccpars2_96

yeccpars2_394(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_394_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_indexpartv1(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_395(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_395_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_indextypesv1(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_396: see yeccpars2_4

yeccpars2_397(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_397_(Stack),
 Nss = lists:nthtail(13, Ss),
 yeccpars2(yeccgoto_objecttypev1(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_398(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_398_(Stack),
 Nss = lists:nthtail(6, Ss),
 yeccpars2(yeccgoto_mib(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccgoto_accesspart(347) -> 350.

yeccgoto_accessv1(378) -> 379.

yeccgoto_accessv2(236) -> 237;
yeccgoto_accessv2(351) -> 352.

yeccgoto_compliance(331) -> 340;
yeccgoto_compliance(337) -> 356.

yeccgoto_compliancegroup(331) -> 339;
yeccgoto_compliancegroup(337) -> 339.

yeccgoto_compliancepart(331) -> 338.

yeccgoto_compliances(331) -> 337.

yeccgoto_contact_info(79) -> 80.

yeccgoto_defbitsnames(268) -> 270.

yeccgoto_defbitsvalue(268) -> 269.

yeccgoto_definition(11) -> 67;
yeccgoto_definition(66) -> 70.

yeccgoto_definitionv2(109) -> 122.

yeccgoto_defvalpart(244) -> 261;
yeccgoto_defvalpart(387) -> 396.

yeccgoto_description(161) -> 163;
yeccgoto_description(299) -> 300;
yeccgoto_description(317) -> 318;
yeccgoto_description(322) -> 323;
yeccgoto_description(350) -> 353;
yeccgoto_description(354) -> 355;
yeccgoto_description(364) -> 372.

yeccgoto_descriptionfield(82) -> 83;
yeccgoto_descriptionfield(241) -> 242;
yeccgoto_descriptionfield(308) -> 309;
yeccgoto_descriptionfield(384) -> 385.

yeccgoto_displaypart(149) -> 157.

yeccgoto_entry(257) -> 259.

yeccgoto_fatherobjectname(96) -> 98.

yeccgoto_fieldname(174) -> 176;
yeccgoto_fieldname(193) -> 195.

yeccgoto_fields(174) -> 175.

yeccgoto_fsyntax(176) -> 179;
yeccgoto_fsyntax(195) -> 196.

yeccgoto_implies(4) -> 5;
yeccgoto_implies(62) -> 359;
yeccgoto_implies(86) -> 95;
yeccgoto_implies(119) -> 123;
yeccgoto_implies(261) -> 95;
yeccgoto_implies(288) -> 95;
yeccgoto_implies(301) -> 95;
yeccgoto_implies(303) -> 95;
yeccgoto_implies(310) -> 95;
yeccgoto_implies(319) -> 95;
yeccgoto_implies(326) -> 95;
yeccgoto_implies(373) -> 374;
yeccgoto_implies(396) -> 95.

yeccgoto_import(10) -> 11.

yeccgoto_import_stuff(12) -> 16;
yeccgoto_import_stuff(14) -> 16;
yeccgoto_import_stuff(52) -> 55.

yeccgoto_imports(12) -> 15;
yeccgoto_imports(14) -> 51.

yeccgoto_imports_from_one_mib(12) -> 14;
yeccgoto_imports_from_one_mib(14) -> 14.

yeccgoto_index(247) -> 251;
yeccgoto_index(252) -> 253;
yeccgoto_index(254) -> 251;
yeccgoto_index(389) -> 392;
yeccgoto_index(393) -> 392.

yeccgoto_indexpartv1(386) -> 387.

yeccgoto_indexpartv2(243) -> 244.

yeccgoto_indextypesv1(389) -> 391.

yeccgoto_indextypesv2(247) -> 250.

yeccgoto_indextypev1(389) -> 390;
yeccgoto_indextypev1(393) -> 395.

yeccgoto_indextypev2(247) -> 249;
yeccgoto_indextypev2(254) -> 256.

yeccgoto_last_updated(73) -> 74.

yeccgoto_listofdefinitions(11) -> 66.

yeccgoto_listofdefinitionsv2(64) -> 109.

yeccgoto_listofimports(12) -> 13;
yeccgoto_listofimports(14) -> 13.

yeccgoto_mandatorypart(329) -> 331.

yeccgoto_mib(0) -> 2.

yeccgoto_mibid(11) -> 65.

yeccgoto_mibname(0) -> 1;
yeccgoto_mibname(328) -> 330.

yeccgoto_module(324) -> 327;
yeccgoto_module(325) -> 358.

yeccgoto_modulecompliance(109) -> 121.

yeccgoto_moduleidentity(11) -> 64.

yeccgoto_modulenamepart(328) -> 329.

yeccgoto_modulepart(324) -> 326.

yeccgoto_modules(324) -> 325.

yeccgoto_nameassign(86) -> 94;
yeccgoto_nameassign(261) -> 283;
yeccgoto_nameassign(288) -> 289;
yeccgoto_nameassign(301) -> 302;
yeccgoto_nameassign(303) -> 304;
yeccgoto_nameassign(310) -> 311;
yeccgoto_nameassign(319) -> 320;
yeccgoto_nameassign(326) -> 357;
yeccgoto_nameassign(396) -> 397.

yeccgoto_namedbits(181) -> 182;
yeccgoto_namedbits(199) -> 200.

yeccgoto_newtype(11) -> 63;
yeccgoto_newtype(66) -> 63;
yeccgoto_newtype(109) -> 120.

yeccgoto_newtypename(11) -> 62;
yeccgoto_newtypename(66) -> 62;
yeccgoto_newtypename(109) -> 119.

yeccgoto_notification(109) -> 118.

yeccgoto_notificationgroup(109) -> 117.

yeccgoto_object(331) -> 336;
yeccgoto_object(337) -> 336.

yeccgoto_objectgroup(109) -> 116.

yeccgoto_objectidentifier(11) -> 61;
yeccgoto_objectidentifier(66) -> 61;
yeccgoto_objectidentifier(109) -> 115.

yeccgoto_objectidentity(109) -> 114.

yeccgoto_objectname(11) -> 60;
yeccgoto_objectname(66) -> 60;
yeccgoto_objectname(96) -> 97;
yeccgoto_objectname(109) -> 113;
yeccgoto_objectname(247) -> 248;
yeccgoto_objectname(252) -> 248;
yeccgoto_objectname(254) -> 248;
yeccgoto_objectname(257) -> 258;
yeccgoto_objectname(292) -> 294;
yeccgoto_objectname(295) -> 297;
yeccgoto_objectname(313) -> 294;
yeccgoto_objectname(333) -> 294;
yeccgoto_objectname(341) -> 354;
yeccgoto_objectname(342) -> 343;
yeccgoto_objectname(362) -> 363;
yeccgoto_objectname(366) -> 368;
yeccgoto_objectname(369) -> 371;
yeccgoto_objectname(389) -> 248;
yeccgoto_objectname(393) -> 248.

yeccgoto_objects(292) -> 293;
yeccgoto_objects(313) -> 314;
yeccgoto_objects(333) -> 334.

yeccgoto_objectspart(226) -> 305;
yeccgoto_objectspart(228) -> 290.

yeccgoto_objecttypev1(11) -> 59;
yeccgoto_objecttypev1(66) -> 59.

yeccgoto_objecttypev2(109) -> 112.

yeccgoto_oranization(76) -> 77.

yeccgoto_parentintegers(98) -> 99;
yeccgoto_parentintegers(101) -> 102;
yeccgoto_parentintegers(105) -> 106.

yeccgoto_range_num(204) -> 206;
yeccgoto_range_num(212) -> 206;
yeccgoto_range_num(215) -> 206;
yeccgoto_range_num(219) -> 220;
yeccgoto_range_num(220) -> 206.

yeccgoto_referpart(163) -> 166;
yeccgoto_referpart(242) -> 243;
yeccgoto_referpart(287) -> 288;
yeccgoto_referpart(300) -> 301;
yeccgoto_referpart(309) -> 310;
yeccgoto_referpart(318) -> 319;
yeccgoto_referpart(323) -> 324;
yeccgoto_referpart(372) -> 373;
yeccgoto_referpart(385) -> 386.

yeccgoto_revision(83) -> 87;
yeccgoto_revision(85) -> 108.

yeccgoto_revision_desc(91) -> 92.

yeccgoto_revision_string(88) -> 89.

yeccgoto_revisionpart(83) -> 86.

yeccgoto_revisions(83) -> 85.

yeccgoto_size(124) -> 223;
yeccgoto_size(125) -> 203.

yeccgoto_sizedescr(204) -> 205;
yeccgoto_sizedescr(212) -> 213;
yeccgoto_sizedescr(215) -> 216;
yeccgoto_sizedescr(220) -> 221.

yeccgoto_statusv1(381) -> 382.

yeccgoto_statusv2(160) -> 161;
yeccgoto_statusv2(239) -> 240;
yeccgoto_statusv2(284) -> 285;
yeccgoto_statusv2(298) -> 299;
yeccgoto_statusv2(306) -> 307;
yeccgoto_statusv2(316) -> 317;
yeccgoto_statusv2(321) -> 322.

yeccgoto_syntax(123) -> 126;
yeccgoto_syntax(169) -> 170;
yeccgoto_syntax(176) -> 178;
yeccgoto_syntax(195) -> 178;
yeccgoto_syntax(231) -> 232;
yeccgoto_syntax(345) -> 346;
yeccgoto_syntax(348) -> 349;
yeccgoto_syntax(359) -> 126;
yeccgoto_syntax(376) -> 377.

yeccgoto_syntaxpart(343) -> 344.

yeccgoto_tableentrydefinition(11) -> 58;
yeccgoto_tableentrydefinition(66) -> 58;
yeccgoto_tableentrydefinition(109) -> 111.

yeccgoto_textualconvention(109) -> 110.

yeccgoto_traptype(11) -> 57;
yeccgoto_traptype(66) -> 57.

yeccgoto_type(123) -> 125;
yeccgoto_type(169) -> 125;
yeccgoto_type(176) -> 125;
yeccgoto_type(195) -> 125;
yeccgoto_type(231) -> 125;
yeccgoto_type(345) -> 125;
yeccgoto_type(348) -> 125;
yeccgoto_type(359) -> 125;
yeccgoto_type(376) -> 125.

yeccgoto_unitspart(232) -> 233.

yeccgoto_usertype(123) -> 124;
yeccgoto_usertype(169) -> 124;
yeccgoto_usertype(172) -> 173;
yeccgoto_usertype(176) -> 124;
yeccgoto_usertype(195) -> 124;
yeccgoto_usertype(231) -> 124;
yeccgoto_usertype(345) -> 124;
yeccgoto_usertype(348) -> 124;
yeccgoto_usertype(359) -> 124;
yeccgoto_usertype(376) -> 124.

yeccgoto_v1orv2(11) -> 56.

yeccgoto_variables(366) -> 367.

yeccgoto_varpart(363) -> 364.

yeccgoto_writesyntaxpart(344) -> 347.

-compile({inline,{yeccpars2_3_,1}}).
-file("snmpc_mib_gram.yrl", 454).
yeccpars2_3_([__1 | Stack]) ->
 [begin
   val ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_9_,1}}).
-file("snmpc_mib_gram.yrl", 408).
yeccpars2_9_([__3,__2,__1 | Stack]) ->
 [begin
   w ( "Sloppy asignment on line ~p" , [ line_of ( __1 ) ] ) , __1
  end | Stack].

-compile({inline,{yeccpars2_10_,1}}).
-file("snmpc_mib_gram.yrl", 226).
yeccpars2_10_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_14_,1}}).
-file("snmpc_mib_gram.yrl", 229).
yeccpars2_14_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_16_,1}}).
-file("snmpc_mib_gram.yrl", 235).
yeccpars2_16_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_17_,1}}).
-file("snmpc_mib_gram.yrl", 274).
yeccpars2_17_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'AutonomousType' }
  end | Stack].

-compile({inline,{yeccpars2_18_,1}}).
-file("snmpc_mib_gram.yrl", 243).
yeccpars2_18_([__1 | Stack]) ->
 [begin
   { builtin , 'Counter' }
  end | Stack].

-compile({inline,{yeccpars2_19_,1}}).
-file("snmpc_mib_gram.yrl", 288).
yeccpars2_19_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'DateAndTime' }
  end | Stack].

-compile({inline,{yeccpars2_20_,1}}).
-file("snmpc_mib_gram.yrl", 264).
yeccpars2_20_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'DisplayString' }
  end | Stack].

-compile({inline,{yeccpars2_21_,1}}).
-file("snmpc_mib_gram.yrl", 244).
yeccpars2_21_([__1 | Stack]) ->
 [begin
   { builtin , 'Gauge' }
  end | Stack].

-compile({inline,{yeccpars2_22_,1}}).
-file("snmpc_mib_gram.yrl", 276).
yeccpars2_22_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'InstancePointer' }
  end | Stack].

-compile({inline,{yeccpars2_23_,1}}).
-file("snmpc_mib_gram.yrl", 242).
yeccpars2_23_([__1 | Stack]) ->
 [begin
   { builtin , 'IpAddress' }
  end | Stack].

-compile({inline,{yeccpars2_24_,1}}).
-file("snmpc_mib_gram.yrl", 254).
yeccpars2_24_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'MODULE-COMPLIANCE' }
  end | Stack].

-compile({inline,{yeccpars2_25_,1}}).
-file("snmpc_mib_gram.yrl", 250).
yeccpars2_25_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'MODULE-IDENTITY' }
  end | Stack].

-compile({inline,{yeccpars2_26_,1}}).
-file("snmpc_mib_gram.yrl", 268).
yeccpars2_26_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'MacAddress' }
  end | Stack].

-compile({inline,{yeccpars2_27_,1}}).
-file("snmpc_mib_gram.yrl", 256).
yeccpars2_27_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'NOTIFICATION-GROUP' }
  end | Stack].

-compile({inline,{yeccpars2_28_,1}}).
-file("snmpc_mib_gram.yrl", 252).
yeccpars2_28_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'NOTIFICATION-TYPE' }
  end | Stack].

-compile({inline,{yeccpars2_29_,1}}).
-file("snmpc_mib_gram.yrl", 240).
yeccpars2_29_([__1 | Stack]) ->
 [begin
   { builtin , 'NetworkAddress' }
  end | Stack].

-compile({inline,{yeccpars2_30_,1}}).
-file("snmpc_mib_gram.yrl", 258).
yeccpars2_30_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'OBJECT-GROUP' }
  end | Stack].

-compile({inline,{yeccpars2_31_,1}}).
-file("snmpc_mib_gram.yrl", 260).
yeccpars2_31_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'OBJECT-IDENTITY' }
  end | Stack].

-compile({inline,{yeccpars2_32_,1}}).
-file("snmpc_mib_gram.yrl", 238).
yeccpars2_32_([__1 | Stack]) ->
 [begin
   { builtin , 'OBJECT-TYPE' }
  end | Stack].

-compile({inline,{yeccpars2_33_,1}}).
-file("snmpc_mib_gram.yrl", 245).
yeccpars2_33_([__1 | Stack]) ->
 [begin
   { builtin , 'Opaque' }
  end | Stack].

-compile({inline,{yeccpars2_34_,1}}).
-file("snmpc_mib_gram.yrl", 266).
yeccpars2_34_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'PhysAddress' }
  end | Stack].

-compile({inline,{yeccpars2_35_,1}}).
-file("snmpc_mib_gram.yrl", 280).
yeccpars2_35_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'RowPointer' }
  end | Stack].

-compile({inline,{yeccpars2_36_,1}}).
-file("snmpc_mib_gram.yrl", 282).
yeccpars2_36_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'RowStatus' }
  end | Stack].

-compile({inline,{yeccpars2_37_,1}}).
-file("snmpc_mib_gram.yrl", 290).
yeccpars2_37_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'StorageType' }
  end | Stack].

-compile({inline,{yeccpars2_38_,1}}).
-file("snmpc_mib_gram.yrl", 294).
yeccpars2_38_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'TAddress' }
  end | Stack].

-compile({inline,{yeccpars2_39_,1}}).
-file("snmpc_mib_gram.yrl", 292).
yeccpars2_39_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'TDomain' }
  end | Stack].

-compile({inline,{yeccpars2_40_,1}}).
-file("snmpc_mib_gram.yrl", 262).
yeccpars2_40_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'TEXTUAL-CONVENTION' }
  end | Stack].

-compile({inline,{yeccpars2_41_,1}}).
-file("snmpc_mib_gram.yrl", 239).
yeccpars2_41_([__1 | Stack]) ->
 [begin
   { builtin , 'TRAP-TYPE' }
  end | Stack].

-compile({inline,{yeccpars2_42_,1}}).
-file("snmpc_mib_gram.yrl", 272).
yeccpars2_42_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'TestAndIncr' }
  end | Stack].

-compile({inline,{yeccpars2_43_,1}}).
-file("snmpc_mib_gram.yrl", 286).
yeccpars2_43_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'TimeInterval' }
  end | Stack].

-compile({inline,{yeccpars2_44_,1}}).
-file("snmpc_mib_gram.yrl", 284).
yeccpars2_44_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'TimeStamp' }
  end | Stack].

-compile({inline,{yeccpars2_45_,1}}).
-file("snmpc_mib_gram.yrl", 241).
yeccpars2_45_([__1 | Stack]) ->
 [begin
   { builtin , 'TimeTicks' }
  end | Stack].

-compile({inline,{yeccpars2_46_,1}}).
-file("snmpc_mib_gram.yrl", 270).
yeccpars2_46_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'TruthValue' }
  end | Stack].

-compile({inline,{yeccpars2_47_,1}}).
-file("snmpc_mib_gram.yrl", 278).
yeccpars2_47_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , { builtin , 'VariablePointer' }
  end | Stack].

-compile({inline,{yeccpars2_48_,1}}).
-file("snmpc_mib_gram.yrl", 247).
yeccpars2_48_([__1 | Stack]) ->
 [begin
   { node , val ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_49_,1}}).
-file("snmpc_mib_gram.yrl", 246).
yeccpars2_49_([__1 | Stack]) ->
 [begin
   filter_v2imports ( get ( snmp_version ) , val ( __1 ) )
  end | Stack].

-compile({inline,{yeccpars2_50_,1}}).
-file("snmpc_mib_gram.yrl", 227).
yeccpars2_50_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_51_,1}}).
-file("snmpc_mib_gram.yrl", 230).
yeccpars2_51_([__2,__1 | Stack]) ->
 [begin
   [ __1 | __2 ]
  end | Stack].

-compile({inline,{yeccpars2_54_,1}}).
-file("snmpc_mib_gram.yrl", 233).
yeccpars2_54_([__3,__2,__1 | Stack]) ->
 [begin
   { { val ( __3 ) , lists : reverse ( __1 ) } , line_of ( __2 ) }
  end | Stack].

-compile({inline,{yeccpars2_55_,1}}).
-file("snmpc_mib_gram.yrl", 236).
yeccpars2_55_([__3,__2,__1 | Stack]) ->
 [begin
   [ __3 | __1 ]
  end | Stack].

-compile({inline,{yeccpars2_64_,1}}).
-file("snmpc_mib_gram.yrl", 506).
yeccpars2_64_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_66_,1}}).
-file("snmpc_mib_gram.yrl", 215).
yeccpars2_66_([__1 | Stack]) ->
 [begin
   { v1_mib , lists : reverse ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_67_,1}}).
-file("snmpc_mib_gram.yrl", 223).
yeccpars2_67_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{'yeccpars2_68_MODULE-IDENTITY',1}}).
-file("snmpc_mib_gram.yrl", 479).
'yeccpars2_68_MODULE-IDENTITY'([__1 | Stack]) ->
 [begin
   val ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_68_,1}}).
-file("snmpc_mib_gram.yrl", 453).
yeccpars2_68_([__1 | Stack]) ->
 [begin
   val ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_69_,1}}).
-file("snmpc_mib_gram.yrl", 456).
yeccpars2_69_([__1 | Stack]) ->
 [begin
   val ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_70_,1}}).
-file("snmpc_mib_gram.yrl", 224).
yeccpars2_70_([__2,__1 | Stack]) ->
 [begin
   [ __2 | __1 ]
  end | Stack].

-compile({inline,{yeccpars2_71_,1}}).
-file("snmpc_mib_gram.yrl", 453).
yeccpars2_71_([__1 | Stack]) ->
 [begin
   val ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_75_,1}}).
-file("snmpc_mib_gram.yrl", 480).
yeccpars2_75_([__1 | Stack]) ->
 [begin
   lists : reverse ( val ( __1 ) )
  end | Stack].

-compile({inline,{yeccpars2_78_,1}}).
-file("snmpc_mib_gram.yrl", 481).
yeccpars2_78_([__1 | Stack]) ->
 [begin
   lists : reverse ( val ( __1 ) )
  end | Stack].

-compile({inline,{yeccpars2_81_,1}}).
-file("snmpc_mib_gram.yrl", 482).
yeccpars2_81_([__1 | Stack]) ->
 [begin
   lists : reverse ( val ( __1 ) )
  end | Stack].

-compile({inline,{yeccpars2_82_,1}}).
-file("snmpc_mib_gram.yrl", 410).
yeccpars2_82_(Stack) ->
 [begin
   undefined
  end | Stack].

-compile({inline,{yeccpars2_83_,1}}).
-file("snmpc_mib_gram.yrl", 484).
yeccpars2_83_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_84_,1}}).
-file("snmpc_mib_gram.yrl", 409).
yeccpars2_84_([__1 | Stack]) ->
 [begin
   lists : reverse ( val ( __1 ) )
  end | Stack].

-compile({inline,{yeccpars2_85_,1}}).
-file("snmpc_mib_gram.yrl", 485).
yeccpars2_85_([__1 | Stack]) ->
 [begin
   lists : reverse ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_87_,1}}).
-file("snmpc_mib_gram.yrl", 487).
yeccpars2_87_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_90_,1}}).
-file("snmpc_mib_gram.yrl", 492).
yeccpars2_90_([__1 | Stack]) ->
 [begin
   lists : reverse ( val ( __1 ) )
  end | Stack].

-compile({inline,{yeccpars2_92_,1}}).
-file("snmpc_mib_gram.yrl", 490).
yeccpars2_92_([__4,__3,__2,__1 | Stack]) ->
 [begin
   make_revision ( __2 , __4 )
  end | Stack].

-compile({inline,{yeccpars2_93_,1}}).
-file("snmpc_mib_gram.yrl", 493).
yeccpars2_93_([__1 | Stack]) ->
 [begin
   lists : reverse ( val ( __1 ) )
  end | Stack].

-compile({inline,{yeccpars2_94_,1}}).
-file("snmpc_mib_gram.yrl", 475).
yeccpars2_94_([__12,__11,__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   MI = make_module_identity ( __1 , __4 , __6 , __8 ,
    __10 , __11 , __12 ) ,
    { MI , line_of ( __2 ) }
  end | Stack].

-compile({inline,{yeccpars2_101_,1}}).
-file("snmpc_mib_gram.yrl", 429).
yeccpars2_101_([__1 | Stack]) ->
 [begin
   [ val ( __1 ) ]
  end | Stack].

-compile({inline,{yeccpars2_102_,1}}).
-file("snmpc_mib_gram.yrl", 431).
yeccpars2_102_([__2,__1 | Stack]) ->
 [begin
   [ val ( __1 ) | __2 ]
  end | Stack].

-compile({inline,{yeccpars2_105_,1}}).
-file("snmpc_mib_gram.yrl", 430).
yeccpars2_105_([__4,__3,__2,__1 | Stack]) ->
 [begin
   [ val ( __3 ) ]
  end | Stack].

-compile({inline,{yeccpars2_106_,1}}).
-file("snmpc_mib_gram.yrl", 432).
yeccpars2_106_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   [ val ( __3 ) | __5 ]
  end | Stack].

-compile({inline,{yeccpars2_107_,1}}).
-file("snmpc_mib_gram.yrl", 400).
yeccpars2_107_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { __3 , __4 }
  end | Stack].

-compile({inline,{yeccpars2_108_,1}}).
-file("snmpc_mib_gram.yrl", 488).
yeccpars2_108_([__2,__1 | Stack]) ->
 [begin
   [ __2 | __1 ]
  end | Stack].

-compile({inline,{yeccpars2_109_,1}}).
-file("snmpc_mib_gram.yrl", 214).
yeccpars2_109_([__2,__1 | Stack]) ->
 [begin
   { v2_mib , [ __1 | lists : reverse ( __2 ) ] }
  end | Stack].

-compile({inline,{yeccpars2_122_,1}}).
-file("snmpc_mib_gram.yrl", 507).
yeccpars2_122_([__2,__1 | Stack]) ->
 [begin
   [ __2 | __1 ]
  end | Stack].

-compile({inline,{yeccpars2_124_,1}}).
-file("snmpc_mib_gram.yrl", 340).
yeccpars2_124_([__1 | Stack]) ->
 [begin
   { { type , val ( __1 ) } , line_of ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_125_,1}}).
-file("snmpc_mib_gram.yrl", 341).
yeccpars2_125_([__1 | Stack]) ->
 [begin
   { { type , cat ( __1 ) } , line_of ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_126_,1}}).
-file("snmpc_mib_gram.yrl", 322).
yeccpars2_126_([__3,__2,__1 | Stack]) ->
 [begin
   NT = make_new_type ( __1 , dummy , __3 ) ,
    { NT , line_of ( __2 ) }
  end | Stack].

-compile({inline,{yeccpars2_127_,1}}).
-file("snmpc_mib_gram.yrl", 386).
yeccpars2_127_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | Stack].

-compile({inline,{yeccpars2_130_,1}}).
-file("snmpc_mib_gram.yrl", 377).
yeccpars2_130_([__1 | Stack]) ->
 [begin
   ensure_ver ( 1 , __1 ) , __1
  end | Stack].

-compile({inline,{yeccpars2_131_,1}}).
-file("snmpc_mib_gram.yrl", 393).
yeccpars2_131_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | Stack].

-compile({inline,{yeccpars2_132_,1}}).
-file("snmpc_mib_gram.yrl", 381).
yeccpars2_132_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | Stack].

-compile({inline,{yeccpars2_133_,1}}).
-file("snmpc_mib_gram.yrl", 378).
yeccpars2_133_([__1 | Stack]) ->
 [begin
   ensure_ver ( 1 , __1 ) , __1
  end | Stack].

-compile({inline,{yeccpars2_135_,1}}).
-file("snmpc_mib_gram.yrl", 387).
yeccpars2_135_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | Stack].

-compile({inline,{yeccpars2_137_,1}}).
-file("snmpc_mib_gram.yrl", 383).
yeccpars2_137_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | Stack].

-compile({inline,{yeccpars2_142_,1}}).
-file("snmpc_mib_gram.yrl", 382).
yeccpars2_142_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | Stack].

-compile({inline,{yeccpars2_143_,1}}).
-file("snmpc_mib_gram.yrl", 389).
yeccpars2_143_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | Stack].

-compile({inline,{yeccpars2_144_,1}}).
-file("snmpc_mib_gram.yrl", 390).
yeccpars2_144_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | Stack].

-compile({inline,{yeccpars2_146_,1}}).
-file("snmpc_mib_gram.yrl", 394).
yeccpars2_146_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | Stack].

-compile({inline,{yeccpars2_147_,1}}).
-file("snmpc_mib_gram.yrl", 396).
yeccpars2_147_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | Stack].

-compile({inline,{yeccpars2_148_,1}}).
-file("snmpc_mib_gram.yrl", 395).
yeccpars2_148_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | Stack].

-compile({inline,{yeccpars2_149_,1}}).
-file("snmpc_mib_gram.yrl", 415).
yeccpars2_149_(Stack) ->
 [begin
   undefined
  end | Stack].

-compile({inline,{yeccpars2_150_,1}}).
-file("snmpc_mib_gram.yrl", 385).
yeccpars2_150_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | Stack].

-compile({inline,{yeccpars2_151_,1}}).
-file("snmpc_mib_gram.yrl", 392).
yeccpars2_151_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | Stack].

-compile({inline,{yeccpars2_152_,1}}).
-file("snmpc_mib_gram.yrl", 391).
yeccpars2_152_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | Stack].

-compile({inline,{yeccpars2_154_,1}}).
-file("snmpc_mib_gram.yrl", 384).
yeccpars2_154_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | Stack].

-compile({inline,{yeccpars2_155_,1}}).
-file("snmpc_mib_gram.yrl", 388).
yeccpars2_155_([__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) , __1
  end | Stack].

-compile({inline,{yeccpars2_159_,1}}).
-file("snmpc_mib_gram.yrl", 414).
yeccpars2_159_([__2,__1 | Stack]) ->
 [begin
   display_hint ( __2 )
  end | Stack].

-compile({inline,{yeccpars2_161_,1}}).
-file("snmpc_mib_gram.yrl", 412).
yeccpars2_161_(Stack) ->
 [begin
   undefined
  end | Stack].

-compile({inline,{yeccpars2_162_,1}}).
-file("snmpc_mib_gram.yrl", 604).
yeccpars2_162_([__1 | Stack]) ->
 [begin
   statusv2 ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_163_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_163_(Stack) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_165_,1}}).
-file("snmpc_mib_gram.yrl", 411).
yeccpars2_165_([__2,__1 | Stack]) ->
 [begin
   lists : reverse ( val ( __2 ) )
  end | Stack].

-compile({inline,{yeccpars2_168_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_168_([__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_170_,1}}).
-file("snmpc_mib_gram.yrl", 511).
yeccpars2_170_([__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   NT = make_new_type ( __1 , 'TEXTUAL-CONVENTION' , __4 ,
    __6 , __7 , __8 , __10 ) ,
    { NT , line_of ( __3 ) }
  end | Stack].

-compile({inline,{yeccpars2_173_,1}}).
-file("snmpc_mib_gram.yrl", 350).
yeccpars2_173_([__3,__2,__1 | Stack]) ->
 [begin
   { { sequence_of , val ( __3 ) } , line_of ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_179_,1}}).
-file("snmpc_mib_gram.yrl", 331).
yeccpars2_179_([__2,__1 | Stack]) ->
 [begin
   [ { val ( __1 ) , __2 } ]
  end | Stack].

-compile({inline,{yeccpars2_180_,1}}).
-file("snmpc_mib_gram.yrl", 335).
yeccpars2_180_([__1 | Stack]) ->
 [begin
   { { bits , [ { dummy , 0 } ] } , line_of ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_186_,1}}).
-file("snmpc_mib_gram.yrl", 365).
yeccpars2_186_([__4,__3,__2,__1 | Stack]) ->
 [begin
   [ { val ( __1 ) , val ( __3 ) } ]
  end | Stack].

-compile({inline,{yeccpars2_188_,1}}).
-file("snmpc_mib_gram.yrl", 347).
yeccpars2_188_([__4,__3,__2,__1 | Stack]) ->
 [begin
   ensure_ver ( 2 , __1 ) ,
    { { bits , __3 } , line_of ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_192_,1}}).
-file("snmpc_mib_gram.yrl", 367).
yeccpars2_192_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   [ { val ( __3 ) , val ( __5 ) } | __1 ]
  end | Stack].

-compile({inline,{yeccpars2_194_,1}}).
-file("snmpc_mib_gram.yrl", 326).
yeccpars2_194_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   Seq = make_sequence ( __1 , lists : reverse ( __5 ) ) ,
    { Seq , line_of ( __3 ) }
  end | Stack].

-compile({inline,{yeccpars2_196_,1}}).
-file("snmpc_mib_gram.yrl", 333).
yeccpars2_196_([__4,__3,__2,__1 | Stack]) ->
 [begin
   [ { val ( __3 ) , __4 } | __1 ]
  end | Stack].

-compile({inline,{yeccpars2_197_,1}}).
-file("snmpc_mib_gram.yrl", 371).
yeccpars2_197_([__2,__1 | Stack]) ->
 [begin
   { 'OCTET STRING' , line_of ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_198_,1}}).
-file("snmpc_mib_gram.yrl", 373).
yeccpars2_198_([__2,__1 | Stack]) ->
 [begin
   { 'OBJECT IDENTIFIER' , line_of ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_201_,1}}).
-file("snmpc_mib_gram.yrl", 345).
yeccpars2_201_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { { integer_with_enum , 'INTEGER' , __3 } , line_of ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_202_,1}}).
-file("snmpc_mib_gram.yrl", 372).
yeccpars2_202_([__2,__1 | Stack]) ->
 [begin
   { 'BIT STRING' , line_of ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_203_,1}}).
-file("snmpc_mib_gram.yrl", 342).
yeccpars2_203_([__2,__1 | Stack]) ->
 [begin
   { { type_with_size , cat ( __1 ) , __2 } , line_of ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_206_,1}}).
-file("snmpc_mib_gram.yrl", 358).
yeccpars2_206_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_208_,1}}).
-file("snmpc_mib_gram.yrl", 361).
yeccpars2_208_([__1 | Stack]) ->
 [begin
   val ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_210_,1}}).
-file("snmpc_mib_gram.yrl", 362).
yeccpars2_210_([__2,__1 | Stack]) ->
 [begin
   make_range_integer ( val ( __1 ) , val ( __2 ) )
  end | Stack].

-compile({inline,{yeccpars2_211_,1}}).
-file("snmpc_mib_gram.yrl", 363).
yeccpars2_211_([__2,__1 | Stack]) ->
 [begin
   make_range_integer ( val ( __1 ) , val ( __2 ) )
  end | Stack].

-compile({inline,{yeccpars2_216_,1}}).
-file("snmpc_mib_gram.yrl", 359).
yeccpars2_216_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 , __3 ]
  end | Stack].

-compile({inline,{yeccpars2_217_,1}}).
-file("snmpc_mib_gram.yrl", 353).
yeccpars2_217_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   make_range ( __4 )
  end | Stack].

-compile({inline,{yeccpars2_220_,1}}).
-file("snmpc_mib_gram.yrl", 356).
yeccpars2_220_([__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __1 , __4 ]
  end | Stack].

-compile({inline,{yeccpars2_221_,1}}).
-file("snmpc_mib_gram.yrl", 357).
yeccpars2_221_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __1 , __4 | __5 ]
  end | Stack].

-compile({inline,{yeccpars2_222_,1}}).
-file("snmpc_mib_gram.yrl", 352).
yeccpars2_222_([__3,__2,__1 | Stack]) ->
 [begin
   make_range ( __2 )
  end | Stack].

-compile({inline,{yeccpars2_223_,1}}).
-file("snmpc_mib_gram.yrl", 343).
yeccpars2_223_([__2,__1 | Stack]) ->
 [begin
   { { type_with_size , val ( __1 ) , __2 } , line_of ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_226_,1}}).
-file("snmpc_mib_gram.yrl", 615).
yeccpars2_226_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_228_,1}}).
-file("snmpc_mib_gram.yrl", 615).
yeccpars2_228_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_232_,1}}).
-file("snmpc_mib_gram.yrl", 601).
yeccpars2_232_(Stack) ->
 [begin
   undefined
  end | Stack].

-compile({inline,{yeccpars2_235_,1}}).
-file("snmpc_mib_gram.yrl", 602).
yeccpars2_235_([__2,__1 | Stack]) ->
 [begin
   units ( __2 )
  end | Stack].

-compile({inline,{yeccpars2_238_,1}}).
-file("snmpc_mib_gram.yrl", 606).
yeccpars2_238_([__1 | Stack]) ->
 [begin
   accessv2 ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_241_,1}}).
-file("snmpc_mib_gram.yrl", 410).
yeccpars2_241_(Stack) ->
 [begin
   undefined
  end | Stack].

-compile({inline,{yeccpars2_242_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_242_(Stack) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_243_,1}}).
-file("snmpc_mib_gram.yrl", 591).
yeccpars2_243_(Stack) ->
 [begin
   { indexes , undefined }
  end | Stack].

-compile({inline,{yeccpars2_244_,1}}).
-file("snmpc_mib_gram.yrl", 445).
yeccpars2_244_(Stack) ->
 [begin
   undefined
  end | Stack].

-compile({inline,{yeccpars2_249_,1}}).
-file("snmpc_mib_gram.yrl", 593).
yeccpars2_249_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_253_,1}}).
-file("snmpc_mib_gram.yrl", 596).
yeccpars2_253_([__2,__1 | Stack]) ->
 [begin
   { implied , __2 }
  end | Stack].

-compile({inline,{yeccpars2_255_,1}}).
-file("snmpc_mib_gram.yrl", 589).
yeccpars2_255_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { indexes , lists : reverse ( __3 ) }
  end | Stack].

-compile({inline,{yeccpars2_256_,1}}).
-file("snmpc_mib_gram.yrl", 594).
yeccpars2_256_([__3,__2,__1 | Stack]) ->
 [begin
   [ __3 | __1 ]
  end | Stack].

-compile({inline,{yeccpars2_260_,1}}).
-file("snmpc_mib_gram.yrl", 590).
yeccpars2_260_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { augments , __3 }
  end | Stack].

-compile({inline,{yeccpars2_268_,1}}).
-file("snmpc_mib_gram.yrl", 448).
yeccpars2_268_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_271_,1}}).
-file("snmpc_mib_gram.yrl", 450).
yeccpars2_271_([__1 | Stack]) ->
 [begin
   [ val ( __1 ) ]
  end | Stack].

-compile({inline,{yeccpars2_273_,1}}).
-file("snmpc_mib_gram.yrl", 451).
yeccpars2_273_([__3,__2,__1 | Stack]) ->
 [begin
   [ val ( __3 ) | __1 ]
  end | Stack].

-compile({inline,{yeccpars2_275_,1}}).
-file("snmpc_mib_gram.yrl", 436).
yeccpars2_275_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { defval , __4 }
  end | Stack].

-compile({inline,{yeccpars2_276_,1}}).
-file("snmpc_mib_gram.yrl", 444).
yeccpars2_276_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { defval , lists : reverse ( val ( __3 ) ) }
  end | Stack].

-compile({inline,{yeccpars2_279_,1}}).
-file("snmpc_mib_gram.yrl", 441).
yeccpars2_279_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { defval , make_defval_for_string ( line_of ( __1 ) , lists : reverse ( val ( __3 ) ) ,
    val ( __4 ) ) }
  end | Stack].

-compile({inline,{yeccpars2_280_,1}}).
-file("snmpc_mib_gram.yrl", 438).
yeccpars2_280_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { defval , make_defval_for_string ( line_of ( __1 ) , lists : reverse ( val ( __3 ) ) ,
    val ( __4 ) ) }
  end | Stack].

-compile({inline,{yeccpars2_281_,1}}).
-file("snmpc_mib_gram.yrl", 434).
yeccpars2_281_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { defval , val ( __3 ) }
  end | Stack].

-compile({inline,{yeccpars2_282_,1}}).
-file("snmpc_mib_gram.yrl", 435).
yeccpars2_282_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { defval , val ( __3 ) }
  end | Stack].

-compile({inline,{yeccpars2_283_,1}}).
-file("snmpc_mib_gram.yrl", 584).
yeccpars2_283_([__15,__14,__13,__12,__11,__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   Kind = kind ( __14 , __13 ) ,
    OT = make_object_type ( __1 , __4 , __5 , __7 , __9 ,
    __11 , __12 , Kind , __15 ) ,
    { OT , line_of ( __2 ) }
  end | Stack].

-compile({inline,{yeccpars2_287_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_287_(Stack) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_289_,1}}).
-file("snmpc_mib_gram.yrl", 517).
yeccpars2_289_([__8,__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { Parent , SubIndex } = __8 ,
    Int = make_internal ( __1 , 'OBJECT-IDENTITY' ,
    Parent , SubIndex ) ,
    { Int , line_of ( __2 ) }
  end | Stack].

-compile({inline,{yeccpars2_294_,1}}).
-file("snmpc_mib_gram.yrl", 617).
yeccpars2_294_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_296_,1}}).
-file("snmpc_mib_gram.yrl", 614).
yeccpars2_296_([__4,__3,__2,__1 | Stack]) ->
 [begin
   lists : reverse ( __3 )
  end | Stack].

-compile({inline,{yeccpars2_297_,1}}).
-file("snmpc_mib_gram.yrl", 618).
yeccpars2_297_([__3,__2,__1 | Stack]) ->
 [begin
   [ __3 | __1 ]
  end | Stack].

-compile({inline,{yeccpars2_299_,1}}).
-file("snmpc_mib_gram.yrl", 412).
yeccpars2_299_(Stack) ->
 [begin
   undefined
  end | Stack].

-compile({inline,{yeccpars2_300_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_300_(Stack) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_302_,1}}).
-file("snmpc_mib_gram.yrl", 524).
yeccpars2_302_([__8,__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   OG = make_object_group ( __1 , __3 , __5 , __6 , __7 , __8 ) ,
    { OG , line_of ( __2 ) }
  end | Stack].

-compile({inline,{yeccpars2_304_,1}}).
-file("snmpc_mib_gram.yrl", 304).
yeccpars2_304_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { Parent , SubIndex } = __4 ,
    Int = make_internal ( __1 , dummy , Parent , SubIndex ) ,
    { Int , line_of ( __2 ) }
  end | Stack].

-compile({inline,{yeccpars2_308_,1}}).
-file("snmpc_mib_gram.yrl", 410).
yeccpars2_308_(Stack) ->
 [begin
   undefined
  end | Stack].

-compile({inline,{yeccpars2_309_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_309_(Stack) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_311_,1}}).
-file("snmpc_mib_gram.yrl", 611).
yeccpars2_311_([__9,__8,__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   Not = make_notification ( __1 , __3 , __5 , __7 , __8 , __9 ) ,
    { Not , line_of ( __2 ) }
  end | Stack].

-compile({inline,{yeccpars2_317_,1}}).
-file("snmpc_mib_gram.yrl", 412).
yeccpars2_317_(Stack) ->
 [begin
   undefined
  end | Stack].

-compile({inline,{yeccpars2_318_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_318_(Stack) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_320_,1}}).
-file("snmpc_mib_gram.yrl", 530).
yeccpars2_320_([__11,__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   NG = make_notification_group ( __1 , __5 , __8 , __9 ,
    __10 , __11 ) ,
    { NG , line_of ( __2 ) }
  end | Stack].

-compile({inline,{yeccpars2_322_,1}}).
-file("snmpc_mib_gram.yrl", 412).
yeccpars2_322_(Stack) ->
 [begin
   undefined
  end | Stack].

-compile({inline,{yeccpars2_323_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_323_(Stack) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_324_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_324_(Stack) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_325_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_325_([__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_327_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_327_([__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_328_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_328_(Stack) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_329_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_329_(Stack) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_330_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_330_([__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_331_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_331_(Stack) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_335_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_335_([__4,__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_336_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_336_([__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_337_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_337_([__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_338_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_338_([__4,__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_339_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_339_([__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_340_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_340_([__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_343_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_343_(Stack) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_344_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_344_(Stack) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_346_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_346_([__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_347_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_347_(Stack) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_349_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_349_([__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_350_,1}}).
-file("snmpc_mib_gram.yrl", 412).
yeccpars2_350_(Stack) ->
 [begin
   undefined
  end | Stack].

-compile({inline,{yeccpars2_352_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_352_([__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_353_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_353_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_354_,1}}).
-file("snmpc_mib_gram.yrl", 412).
yeccpars2_354_(Stack) ->
 [begin
   undefined
  end | Stack].

-compile({inline,{yeccpars2_355_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_355_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_356_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_356_([__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_357_,1}}).
-file("snmpc_mib_gram.yrl", 536).
yeccpars2_357_([__8,__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   MC = make_module_compliance ( __1 , __4 , __5 , __6 ,
    __7 , __8 ) ,
    { MC , line_of ( __2 ) }
  end | Stack].

-compile({inline,{yeccpars2_358_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_358_([__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_363_,1}}).
-file("snmpc_mib_gram.yrl", 402).
yeccpars2_363_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_364_,1}}).
-file("snmpc_mib_gram.yrl", 412).
yeccpars2_364_(Stack) ->
 [begin
   undefined
  end | Stack].

-compile({inline,{yeccpars2_368_,1}}).
-file("snmpc_mib_gram.yrl", 404).
yeccpars2_368_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_370_,1}}).
-file("snmpc_mib_gram.yrl", 403).
yeccpars2_370_([__4,__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_371_,1}}).
-file("snmpc_mib_gram.yrl", 405).
yeccpars2_371_([__3,__2,__1 | Stack]) ->
 [begin
   [ __3 | __1 ]
  end | Stack].

-compile({inline,{yeccpars2_372_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_372_(Stack) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_375_,1}}).
-file("snmpc_mib_gram.yrl", 298).
yeccpars2_375_([__9,__8,__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   Trap = make_trap ( __1 , __4 , lists : reverse ( __5 ) ,
    __6 , __7 , val ( __9 ) ) ,
    { Trap , line_of ( __2 ) }
  end | Stack].

-compile({inline,{yeccpars2_380_,1}}).
-file("snmpc_mib_gram.yrl", 458).
yeccpars2_380_([__1 | Stack]) ->
 [begin
   accessv1 ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_383_,1}}).
-file("snmpc_mib_gram.yrl", 460).
yeccpars2_383_([__1 | Stack]) ->
 [begin
   statusv1 ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_384_,1}}).
-file("snmpc_mib_gram.yrl", 410).
yeccpars2_384_(Stack) ->
 [begin
   undefined
  end | Stack].

-compile({inline,{yeccpars2_385_,1}}).
-file("snmpc_mib_gram.yrl", 0).
yeccpars2_385_(Stack) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_386_,1}}).
-file("snmpc_mib_gram.yrl", 420).
yeccpars2_386_(Stack) ->
 [begin
   { indexes , undefined }
  end | Stack].

-compile({inline,{yeccpars2_387_,1}}).
-file("snmpc_mib_gram.yrl", 445).
yeccpars2_387_(Stack) ->
 [begin
   undefined
  end | Stack].

-compile({inline,{yeccpars2_390_,1}}).
-file("snmpc_mib_gram.yrl", 422).
yeccpars2_390_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_394_,1}}).
-file("snmpc_mib_gram.yrl", 419).
yeccpars2_394_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { indexes , lists : reverse ( __3 ) }
  end | Stack].

-compile({inline,{yeccpars2_395_,1}}).
-file("snmpc_mib_gram.yrl", 423).
yeccpars2_395_([__3,__2,__1 | Stack]) ->
 [begin
   [ __3 | __1 ]
  end | Stack].

-compile({inline,{yeccpars2_397_,1}}).
-file("snmpc_mib_gram.yrl", 316).
yeccpars2_397_([__14,__13,__12,__11,__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   Kind = kind ( __13 , __12 ) ,
    OT = make_object_type ( __1 , __4 , __6 , __8 , __10 ,
    __11 , Kind , __14 ) ,
    { OT , line_of ( __2 ) }
  end | Stack].

-compile({inline,{yeccpars2_398_,1}}).
-file("snmpc_mib_gram.yrl", 207).
yeccpars2_398_([__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { Version , Defs } = __6 ,
    # pdata { mib_version = Version ,
    mib_name = __1 ,
    imports = __5 ,
    defs = Defs }
  end | Stack].


-file("snmpc_mib_gram.yrl", 973).
