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

%% Backwards compatibility ;-)
%% erl44: yecc:yecc(snmp_mib_gram,snmp_mib_gram,false,snmp_mib_gram), c(snmp_mib_gram).
%% erl431: yecc:yecc(snmp_mib_gram,snmp_mib_gram,snmp_mib_gram), c(snmp_mib_gram).
%% erl45: yecc:yecc("snmp_mib_gram","snmp_mib_gram"), c(snmp_mib_gram).
%% ----------------------------------------------------------------------
Nonterminals 
%% ----------------------------------------------------------------------
accessv1
definition
defvalpart
description
descriptionfield
displaypart
entry
namedbits
fatherobjectname
fieldname
fields
implies
import
import_stuff
imports
imports_from_one_mib
index
indexpartv1
indextypev1
indextypesv1
parentintegers
listofdefinitions
listofimports
mib
mibname
nameassign
newtype
newtypename
objectidentifier
objectname
objecttypev1
referpart
size
sizedescr
statusv1
syntax
tableentrydefinition
traptype
type
usertype
variables
varpart

%v2
moduleidentity
revisionpart
revisions
listofdefinitionsv2
mibid
revision
v1orv2
objectidentity
objecttypev2
unitspart
indexpartv2
indextypesv2
indextypev2
statusv2
accessv2
notification
objectspart
objects
definitionv2
textualconvention
objectgroup
notificationgroup
modulecompliance
modulepart
modules
module
modulenamepart
mandatorypart
compliancepart
compliances
compliance
compliancegroup
object
syntaxpart
writesyntaxpart
accesspart
fsyntax
defbitsvalue
defbitsnames
.
%% ----------------------------------------------------------------------
Terminals 
%% ----------------------------------------------------------------------
integer variable atom string quote '{' '}' '::=' ':' '=' ',' '.' '(' ')' ';' '|'
'ACCESS'
'BEGIN'
'BIT'
'Counter'
'DEFINITIONS'
'DEFVAL'
'DESCRIPTION'
'DISPLAY-HINT'
'END'
'ENTERPRISE'
'FROM'
'Gauge'
'IDENTIFIER'
'IMPORTS'
'INDEX'
'INTEGER'
'IpAddress'
'NetworkAddress'
'OBJECT'
'OBJECT'
'OBJECT-TYPE'
'OCTET'
'OF'
'Opaque'
'REFERENCE'
'SEQUENCE'
'SIZE'
'STATUS'
'STRING'
'SYNTAX'
'TRAP-TYPE'
'TimeTicks'
'VARIABLES'
'deprecated'
'mandatory'
'not-accessible'
'obsolete'
'optional'
'read-only'
'read-write'
'write-only'

%v2
'LAST-UPDATED'
'ORGANIZATION'
'CONTACT-INFO'
'MODULE-IDENTITY'
'NOTIFICATION-TYPE'
'MODULE-COMPLIANCE'
'OBJECT-GROUP'
'NOTIFICATION-GROUP'
'REVISION'
'OBJECT-IDENTITY'
'current'
'MAX-ACCESS'
'accessible-for-notify'
'read-create'
'UNITS'
'AUGMENTS'
'IMPLIED'
'OBJECTS'
'TEXTUAL-CONVENTION'
'OBJECT-GROUP'
'NOTIFICATION-GROUP'
'NOTIFICATIONS'
'MODULE-COMPLIANCE'
'MODULE'
'MANDATORY-GROUPS'
'GROUP'
'WRITE-SYNTAX'
'MIN-ACCESS'
'BITS'
.


Rootsymbol mib.
Endsymbol '$end'.
% **********************************************************************

mib -> mibname 'DEFINITIONS' implies 'BEGIN'
       import v1orv2 'END' 
    : {Version,Defs} = '$6',
	{Version, '$1', '$5', Defs}.

v1orv2 -> moduleidentity listofdefinitionsv2 :
			  {v2_mib,['$1'|lists:reverse('$2')]}.
v1orv2 -> listofdefinitions : {v1_mib,lists:reverse('$1')}.

definition -> objectidentifier : '$1'.
definition -> objecttypev1 : '$1'.
definition -> newtype : '$1'.
definition -> tableentrydefinition : '$1'.
definition -> traptype : '$1'.

listofdefinitions -> definition : ['$1'].
listofdefinitions -> listofdefinitions definition : ['$2' | '$1'].

import -> '$empty' : {{import,[]},0}.
import -> 'IMPORTS' imports ';' : {{import, '$2'},line_of('$1')}.

imports -> imports_from_one_mib : ['$1'].
imports -> imports_from_one_mib imports : ['$1' | '$2'].

imports_from_one_mib -> listofimports 'FROM' variable 
          : {{val('$3'), '$1'}, line_of('$2')}.

listofimports -> import_stuff : ['$1'].
listofimports -> listofimports ',' import_stuff : ['$3' | '$1'].

import_stuff -> 'OBJECT-TYPE' : {builtin, 'OBJECT-TYPE'}.
import_stuff -> 'TRAP-TYPE' : {builtin, 'TRAP-TYPE'}.
import_stuff -> 'NetworkAddress' : {builtin, 'NetworkAddress'}.
import_stuff -> 'TimeTicks' : {builtin, 'TimeTicks'}.
import_stuff -> 'IpAddress' : {builtin, 'IpAddress'}.
import_stuff -> 'Counter' : {builtin, 'Counter'}.
import_stuff -> 'Gauge' : {builtin, 'Gauge'}.
import_stuff -> 'Opaque' : {builtin, 'Opaque'}.
import_stuff -> variable : filter_v2imports(get(snmp_version), val('$1')).
import_stuff -> atom : {node, val('$1')}.
%%v2
import_stuff -> 'MODULE-IDENTITY'
       : ensure_ver(2,'$1'), {builtin, 'MODULE-IDENTITY'}.
import_stuff -> 'NOTIFICATION-TYPE' 
       : ensure_ver(2,'$1'), {builtin, 'NOTIFICATION-TYPE'}.
import_stuff -> 'MODULE-COMPLIANCE' 
       : ensure_ver(2,'$1'), {builtin, 'MODULE-COMPLIANCE'}.
import_stuff -> 'NOTIFICATION-GROUP' 
       : ensure_ver(2,'$1'), {builtin, 'NOTIFICATION-GROUP'}.
import_stuff -> 'OBJECT-GROUP' 
       : ensure_ver(2,'$1'), {builtin, 'OBJECT-GROUP'}.
import_stuff -> 'OBJECT-IDENTITY' 
       : ensure_ver(2,'$1'), {builtin, 'OBJECT-IDENTITY'}.
import_stuff -> 'TEXTUAL-CONVENTION' 
       : ensure_ver(2,'$1'), {builtin, 'TEXTUAL-CONVENTION'}.

traptype -> objectname 'TRAP-TYPE' 'ENTERPRISE' objectname varpart
	    description referpart implies integer
          : {{trap, '$1', '$4', lists:reverse('$5'), '$6', 
	val('$9')}, line_of('$2')}.

% defines a name to an internal node.
objectidentifier -> objectname 'OBJECT' 'IDENTIFIER' nameassign
		: {FatherName, SubIndex} = '$4',
		{{internal,dummy,'$1', FatherName, SubIndex}, line_of('$2')}.

% defines name, access and type for a variable.
objecttypev1 ->	objectname 'OBJECT-TYPE' 
		'SYNTAX' syntax
               	'ACCESS' accessv1
		'STATUS' statusv1
                'DESCRIPTION' descriptionfield
		referpart indexpartv1 defvalpart
		nameassign : 
		DefValPart = '$13', IndexPart = '$12', Status = '$8',
		NameAssign = '$14',
           {{object_type,'$1', '$4', '$6', kind(DefValPart,IndexPart),
	     Status, '$10',  NameAssign}, line_of('$2')}.

newtype -> newtypename implies syntax :
       {{new_type, dummy, '$1', '$3'}, line_of('$2')}.

tableentrydefinition -> newtypename implies 'SEQUENCE' '{' fields '}' 
 : {{sequence, '$1', {fieldList, lists:reverse('$5')}}, line_of('$3')}.

% returns: list of {<fieldname>, <asn1_type>}
fields -> fieldname fsyntax : 
	[{val('$1'), '$2'}].

fields -> fields ',' fieldname fsyntax :  [{val('$3'), '$4'} | '$1'].

fsyntax -> 'BITS' : {{bits,[{dummy,0}]},line_of('$1')}.
fsyntax -> syntax : '$1'.

fieldname -> atom : '$1'.

syntax -> usertype : {{type, val('$1')}, line_of('$1')}.
syntax -> type : {{type, cat('$1')},line_of('$1')}.
syntax -> type size : {{type_with_size, cat('$1'), '$2'},line_of('$1')}.
syntax -> usertype size : {{type_with_size,val('$1'), '$2'},line_of('$1')}.
syntax -> 'INTEGER' '{' namedbits '}' : 
         {{integer_with_enum, 'INTEGER', '$3'},line_of('$1')}.
syntax -> 'BITS' '{' namedbits '}' : ensure_ver(2,'$1'),
	     {{bits, '$3'},line_of('$1')}.
syntax -> 'SEQUENCE' 'OF' usertype : {{sequence_of,val('$3')},line_of('$1')}.

size -> '(' sizedescr ')' : make_range('$2').
size -> '(' 'SIZE' '(' sizedescr  ')' ')' : make_range('$4').

%% Returns a list of integers describing a range.
sizedescr -> integer '.' '.' integer : [val('$1'), val('$4')].
sizedescr -> integer '.' '.' integer sizedescr :[val('$1'), val('$4') |'$5'].
sizedescr -> integer : [val('$1')].
sizedescr -> sizedescr '|' sizedescr : ['$1', '$3'].

namedbits -> atom '(' integer ')' : [{val('$1'), val('$3')}].
namedbits -> namedbits ',' atom '(' integer ')' :
		 [{val('$3'), val('$5')} | '$1'].

usertype -> variable : '$1'.

type -> 'OCTET' 'STRING' : {'OCTET STRING', line_of('$1')}.
type -> 'BIT' 'STRING' : {'BIT STRING', line_of('$1')}.
type -> 'OBJECT' 'IDENTIFIER' : {'OBJECT IDENTIFIER', line_of('$1')}.
type -> 'INTEGER' : '$1'.
type -> 'NetworkAddress' : '$1'.
type -> 'IpAddress' : '$1'.
type -> 'Counter' : ensure_ver(1,'$1'),'$1'.
type -> 'Gauge' : ensure_ver(1,'$1'),'$1'.
type -> 'TimeTicks' : '$1'.
type -> 'Opaque' : '$1'.

% Returns: {FatherName, SubIndex}   (the parent)
nameassign -> implies '{' fatherobjectname parentintegers '}'
		: {'$3', '$4' }.

varpart -> '$empty' : [].
varpart -> 'VARIABLES' '{' variables '}' : '$3'.
variables -> objectname : ['$1'].
variables -> variables ',' objectname : ['$3' | '$1'].

implies -> '::=' : '$1'.
implies -> ':' ':' '=' : w("Sloppy asignment on line ~p", [line_of('$1')]), '$1'.
descriptionfield -> string : {'DESCRIPTION', lists:reverse(val('$1'))}.
descriptionfield -> '$empty' : {'DESCRIPTION', undefined}.
description -> 'DESCRIPTION' string : {'DESCRIPTION', lists:reverse(val('$2'))}.
description -> '$empty' : {'DESCRIPTION', undefined}.
displaypart -> 'DISPLAY-HINT' string.
displaypart -> '$empty'.

% returns: {indexes, undefined} 
%        | {indexes, IndexList} where IndexList is a list of aliasnames.
indexpartv1 -> 'INDEX' '{' indextypesv1 '}' : {indexes, lists:reverse('$3')}.
indexpartv1 -> '$empty' : {indexes, undefined}.

indextypesv1 -> indextypev1 : ['$1'].
indextypesv1 -> indextypesv1 ',' indextypev1 : ['$3' | '$1'].

indextypev1 ->  index : '$1'.

index -> objectname : '$1'.

parentintegers -> integer : [val('$1')].
parentintegers -> atom '(' integer ')' : [val('$3')].
parentintegers -> integer parentintegers : [val('$1') | '$2'].
parentintegers -> atom '(' integer ')' parentintegers : [val('$3') | '$5'].

defvalpart -> 'DEFVAL' '{' integer '}' : {defval, val('$3')}.
defvalpart -> 'DEFVAL' '{' atom '}' : {defval, val('$3')}.
defvalpart -> 'DEFVAL' '{' '{' defbitsvalue '}' '}' : {defval, '$4'}.
defvalpart -> 'DEFVAL' '{' quote atom '}' 
     : {defval, make_defval_for_string(line_of('$1'), lists:reverse(val('$3')),
				       val('$4'))}.
defvalpart -> 'DEFVAL' '{' quote variable '}' 
     : {defval, make_defval_for_string(line_of('$1'), lists:reverse(val('$3')),
				       val('$4'))}.
defvalpart -> 'DEFVAL' '{' string '}' 
     : {defval, lists:reverse(val('$3'))}.
defvalpart -> '$empty' : undefined.

defbitsvalue -> defbitsnames : '$1'.
defbitsvalue -> '$empty' : [].

defbitsnames -> atom  : [val('$1')].
defbitsnames -> defbitsnames ',' atom  : [val('$3') | '$1'].

objectname -> atom : val('$1').
mibname -> variable : val('$1').
fatherobjectname -> objectname : '$1'.
newtypename -> variable : val('$1').

accessv1 -> 'read-only' : 'read-only'.
accessv1 -> 'read-write' : 'read-write'.
accessv1 -> 'write-only' : 'write-only'.
accessv1 -> 'not-accessible' : 'not-accessible'.

statusv1 -> 'mandatory' : 'mandatory'.
statusv1 -> 'optional' : 'optional'.
statusv1 -> 'obsolete' : 'obsolete'.
statusv1 -> 'deprecated' : 'deprecated'.

referpart -> 'REFERENCE' string.
referpart -> '$empty'.

%%----------------------------------------------------------------------
%% SNMPv2 grammatics
%%v2
%%----------------------------------------------------------------------
moduleidentity -> mibid 'MODULE-IDENTITY' 'LAST-UPDATED' string
	'ORGANIZATION' string 'CONTACT-INFO' string 
	'DESCRIPTION' descriptionfield revisionpart 
	nameassign : 
	{FatherName, SubIndex} = '$12',
	{{internal,'MODULE-IDENTITY','$1',FatherName,SubIndex},line_of('$2')}.

mibid -> atom : val('$1').

revisionpart -> '$empty'.
revisionpart -> revisions.

revisions -> revision.
revisions -> revisions revision.
revision -> 'REVISION' string 'DESCRIPTION' string.

definitionv2 -> objectidentifier : '$1'.
definitionv2 -> objecttypev2 : '$1'.
definitionv2 -> textualconvention : '$1'.
definitionv2 -> objectidentity : '$1'.
definitionv2 -> newtype : '$1'.
definitionv2 -> tableentrydefinition : '$1'.
definitionv2 -> notification : '$1'.
definitionv2 -> objectgroup : '$1'.
definitionv2 -> notificationgroup : '$1'.
definitionv2 -> modulecompliance : '$1'.

listofdefinitionsv2 -> definitionv2 : ['$1'].
listofdefinitionsv2 -> listofdefinitionsv2 definitionv2 : ['$2' | '$1'].

textualconvention -> newtypename implies 'TEXTUAL-CONVENTION' displaypart
    'STATUS' statusv2 description referpart 'SYNTAX' syntax :
    {{new_type,'TEXTUAL-CONVENTION','$1','$10'},line_of('$3')}.

objectidentity -> objectname 'OBJECT-IDENTITY' 'STATUS' statusv2
	'DESCRIPTION' string referpart nameassign : 
	{FatherName, SubIndex} = '$8',
	{{internal,'OBJECT-IDENTITY','$1',FatherName, SubIndex}, line_of('$2')}.

objectgroup -> objectname 'OBJECT-GROUP' objectspart 
        'STATUS' statusv2 description referpart nameassign :
	      {{object_group,'$1','$3','$5'},line_of('$2')}.

notificationgroup -> objectname 'NOTIFICATION-GROUP' 'NOTIFICATIONS' '{'
        objects '}' 'STATUS' statusv2 description referpart nameassign :
	{{notification_group,'$1','$5','$8'},line_of('$2')}.

modulecompliance -> objectname 'MODULE-COMPLIANCE' 'STATUS' statusv2
        description referpart modulepart nameassign : 
		{{module_compliance,'$1'},line_of('$2')}.

modulepart -> '$empty'.
modulepart -> modules.

modules -> module.
modules -> modules module.
    
module -> 'MODULE' modulenamepart mandatorypart compliancepart.

modulenamepart -> mibname.
modulenamepart -> '$empty'.

mandatorypart -> 'MANDATORY-GROUPS' '{' objects '}'.
mandatorypart -> '$empty'.
    
compliancepart -> compliances.
compliancepart -> '$empty'.

compliances -> compliance.
compliances -> compliances compliance.

compliance -> compliancegroup.
compliance -> object.

compliancegroup -> 'GROUP' objectname description.

object -> 'OBJECT' objectname syntaxpart writesyntaxpart accesspart description.

syntaxpart -> 'SYNTAX' syntax.
syntaxpart -> '$empty'.

writesyntaxpart -> 'WRITE-SYNTAX' syntax.
writesyntaxpart -> '$empty'.
    
accesspart -> 'MIN-ACCESS' accessv2.
accesspart -> '$empty'.
    
objecttypev2 ->	objectname 'OBJECT-TYPE' 
		'SYNTAX' syntax
                unitspart
               	'MAX-ACCESS' accessv2
		'STATUS' statusv2
                'DESCRIPTION' descriptionfield
                referpart indexpartv2 defvalpart
		nameassign : 
		DefValPart = '$14', IndexPart = '$13', Status = '$9',
		NameAssign = '$15',
           {{object_type,'$1', '$4', '$7', kind(DefValPart,IndexPart),
	     Status, '$11', NameAssign}, line_of('$2')}.

indexpartv2 -> 'INDEX' '{' indextypesv2 '}' : {indexes, lists:reverse('$3')}.
indexpartv2 -> 'AUGMENTS' '{' entry  '}' : {augments, '$3'}.
indexpartv2 -> '$empty' : {indexes, undefined}.

indextypesv2 -> indextypev2 : ['$1'].
indextypesv2 -> indextypesv2 ',' indextypev2 : ['$3' | '$1'].

indextypev2 ->  'IMPLIED' index : {implied,'$2'}.
indextypev2 ->  index : '$1'.

entry -> objectname : '$1'.

unitspart -> '$empty'.
unitspart -> 'UNITS' string.

statusv2 -> 'current' : 'mandatory'.
statusv2 -> 'deprecated' : 'deprecated'.
statusv2 -> 'obsolete' : 'obsolete'.

accessv2 -> 'not-accessible' : 'not-accessible'.
accessv2 -> 'accessible-for-notify' : 'accessible-for-notify'.
accessv2 -> 'read-only' : 'read-only'.
accessv2 -> 'read-write' : 'read-write'.
accessv2 -> 'read-create' : 'read-create'.

notification -> objectname 'NOTIFICATION-TYPE' objectspart
                'STATUS' statusv2 'DESCRIPTION' descriptionfield referpart nameassign :
		{{notification,'$1','$3','$5', '$7', '$9'},line_of('$2')}.

objectspart -> 'OBJECTS' '{' objects '}' : lists:reverse('$3').
objectspart -> '$empty' : [].

objects -> objectname : ['$1'].
objects -> objects ',' objectname : ['$3'|'$1'].

%%----------------------------------------------------------------------
Erlang code.
%%----------------------------------------------------------------------

-include("snmp_types.hrl").

% value
val(Token) -> element(3, Token).

line_of(Token) -> element(2, Token).

%% category
cat(Token) -> element(1, Token). 

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
		    snmp_compile_lib:print_error("Bad DEFVAL ~w string ~p - ~s",
						 [Atom, Str, ErrStr],
						 Line),
		    "";
		_Else ->
		    snmp_compile_lib:print_error("Bad DEFVAL ~w string ~p",
						 [Atom, Str],
						 Line),
		    ""
	    end;
	false ->
	    snmp_compile_lib:print_error("Bad DEFVAL string type ~w for ~p",
					 [Atom, Str],
					 Line),
	    ""
    end.
	    

make_defval_for_string2([], h) -> [];
make_defval_for_string2([X16,X|HexString], h) ->
    lists:append(hex_to_bytes(snmp_misc:to_upper([X16,X])),
		 make_defval_for_string2(HexString, h));
make_defval_for_string2([Odd], h) ->
    throw({error, "odd number of bytes in hex string"});
make_defval_for_string2(HexString, 'H') ->
    make_defval_for_string2(HexString,h);

make_defval_for_string2(BitString, 'B') ->
    bits_to_bytes(BitString);
make_defval_for_string2(BitString, b) ->
    make_defval_for_string2(BitString, 'B').

bits_to_bytes(BitStr) ->
    lists:reverse(bits_to_bytes(lists:reverse(BitStr), 1, 0)).

bits_to_bytes([], 1, Byte) ->   % empty bitstring
    [];
bits_to_bytes([], 256, Byte) -> % correct; multiple of 8
    [];
% If we are to support arbitrary length of bitstrings.  This migth
% be needed in the new SMI.
%bits_to_bytes([], N, Byte) ->
%    [Byte];
bits_to_bytes([], N, Byte) ->
    throw({error, "not a multiple of eight bits in bitstring"});
bits_to_bytes(Rest, 256, Byte) ->
    [Byte | bits_to_bytes(Rest, 1, 0)];
bits_to_bytes([$1 | T], N, Byte) ->
    bits_to_bytes(T, N*2, N + Byte);
bits_to_bytes([$0 | T], N, Byte) ->
    bits_to_bytes(T, N*2, Byte);
bits_to_bytes([BadChar | T], N, Byte) ->
    throw({error, "bad character in bit string"}).

%%----------------------------------------------------------------------
%% These HEX conversion routines are stolen from module asn1_bits by 
%% klacke@erix.ericsson.se
%% I didn't want to ship the entire asn1-compiler so I used cut-and-paste.
%%----------------------------------------------------------------------
hex_to_bytes(HexNumber) when atom(HexNumber) ->
    hex_to_bytes(atom_to_list(HexNumber));

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

ensure_ver(Ver, Line, What) ->
    case get(snmp_version) of
	Ver -> ok;
	Other ->
	    snmp_compile_lib:print_error(
	      "~s is only allowed in SNMPv~p.",[What,Ver],Line)
    end.


ensure_ver(Ver,Token) ->
    ensure_ver(Ver,line_of(Token), atom_to_list(cat(Token))).

filter_v2imports(2,'Integer32') -> {builtin, 'Integer32'};
filter_v2imports(2,'Counter32') -> {builtin, 'Counter32'};
filter_v2imports(2,'Gauge32') -> {builtin, 'Gauge32'};
filter_v2imports(2,'Unsigned32') -> {builtin, 'Unsigned32'};
filter_v2imports(2,'Counter64') -> {builtin, 'Counter64'};
filter_v2imports(_,Type) -> {type,Type}.
    
w(F, A) ->
    snmp_compile_lib:w(F, A).
