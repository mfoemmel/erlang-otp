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
%%----------------------------------------------------------------------
%% Purpose: Handle meta data about packages
%%----------------------------------------------------------------------

-module(megaco_binary_name_resolver_v2).

-include_lib("megaco/src/engine/megaco_message_internal.hrl").

-define(LOWER(Char),
	if
	    Char >= $A, Char =< $Z ->
		Char - ($A - $a);
	    true ->
		Char
	end).

-export([packages/0,
	 capabilities/0,
	 capabilities/1,
         decode_name/3,
	 encode_name/3
        ]).

encode_name(Config, term_id, TermId) ->
    case megaco:encode_binary_term_id(Config, TermId) of
	{ok, TermId2} ->
	    TermId2;
	{error, _Reason} ->
	    exit({bad_term_id, TermId})
    end;	
encode_name(_Config, Scope, Item) ->
%     i("encode_name(~p) -> entry with Item: ~p",[Scope, Item]),
    encode(Scope, Item).

decode_name(Config, term_id, TermId) ->
    case megaco:decode_binary_term_id(Config, TermId) of
	{ok, TermId2} ->
	    TermId2;
	{error, _Reason} ->
	    exit({bad_term_id, TermId})
    end;
decode_name(_Config, Scope, Item) ->
%     i("decode_name(~p) -> entry with Item: ~p",[Scope, Item]),
    decode(Scope, Item).

%%----------------------------------------------------------------------
%% 12.1.1 Package
%% 
%% Overall description of the package, specifying:
%% 
%%         Package Name: only descriptive,
%%         PackageID:  Is an identifier
%%         Description:
%%         Version:
%%                 A new version of a package can only add additional
%%                 Properties, Events, Signals, Statistics and new possible
%%                 values for an existing parameter described in the
%%                 package. No deletions or modifications shall be allowed.
%%                 A version is an integer in the range from 1 to 99.
%% 
%%         Designed to be extended only (Optional):
%% 
%%                This indicates that the package has been expressly 
%%                designed to be extended by others, not to be directly 
%%                referenced.  For example, the package may not have any 
%%                function on its own or be nonsensical on its own.  
%%                The MG SHOULD NOT publish this PackageID when reporting 
%%                packages.
%%
%%         Extends (Optional):
%%                 A package may extend an existing package. The version of
%%                 the original package must be specified. When a package
%%                 extends another package it shall only add additional
%%                 Properties, Events, Signals, Statistics and new possible
%%                 values for an existing parameter described in the original
%%                 package. An extended package shall not redefine or
%%                 overload a name defined in the original package.
%%                 Hence, if package B version 1 extends package A version 1,
%%                 version 2 of B will not be able to extend the A version 2
%%                 if A version 2 defines a name already in B version 1.
%% 
%% 12.1.2.  Properties
%% 
%% Properties defined by the package, specifying:
%% 
%%         Property Name: only descriptive.
%%         PropertyID:  Is an identifier
%%         Description:
%%         Type: One of:
%%                 String: UTF-8 string
%%                 Integer: 4 byte signed integer
%%                 Double: 8 byte signed integer
%%                 Character: Unicode UTF-8 encoding of a single letter.
%%                         Could be more than one octet.
%%                 Enumeration: One of a list of possible unique values (See 12.3)
%%                 Sub-list: A list of several values from a list
%%                 Boolean
%%         Possible Values:
%%         Defined in:
%%                 Which descriptor the property is defined in.  LocalControl
%%                 is for stream dependent properties. TerminationState is for
%%                 stream independent properties.
%%         Characteristics: Read / Write or both, and (optionally), global:
%%                 Indicates whether a property is read-only, or read-write,
%%                 and if it is global.  If Global is omitted, the property
%%                 is not global.  If a property is declared as global,
%%                 the value of the property is shared by all terminations
%%                 realizing the package.
%% 
%% 12.1.3.  Events
%% 
%% Events defined by the package, specifying:
%% 
%%         Event name: only descriptive.
%%         EventID:  Is an identifier
%%         Description:
%%         EventsDescriptor Parameters:
%%                 Parameters used by the MGC to configure the event,
%%                 and found in the EventsDescriptor.  See section 12.2.
%%         ObservedEventsDescriptor Parameters:
%%                 Parameters returned to the MGC in  Notify requests and in
%%                 replies to command requests from the MGC that audit
%%                 ObservedEventsDescriptor, and found in the
%%                 ObservedEventsDescriptor.  See section 12.2.
%% 
%% 12.1.4.  Signals
%% 
%% Signals defined by the package, specifying:
%% 
%%         Signal Name: only descriptive.
%%         SignalID:  Is an identifier. SignalID is used in a
%%                         SignalsDescriptor
%%         Description
%%         SignalType: One of:
%%                         OO (On/Off)
%%                         TO (TimeOut)
%%                         BR (Brief)
%% 
%% 
%% Note:SignalType may be defined such that it is dependent on the
%%           value of one or more parameters. Signals that would be played
%%      with SignalType BR should have a default duration. The package has
%%      to define the default duration and signalType.
%% 
%%           Duration: in hundredths of seconds
%%           Additional Parameters: See section 12.2
%% 
%% 
%% 12.1.5.  Statistics
%% 
%% Statistics defined by the package, specifying:
%% 
%%         Statistic name: only descriptive.
%%         StatisticID:  Is an identifier
%%         StatisticID is used in a StatisticsDescriptor
%%         Description
%%         Units: unit of measure, e.g. milliseconds, packets
%% 
%% 
%% 12.1.6.  Procedures
%% 
%% Additional guidance on the use of the package.
%% 
%% 12.2.  Guidelines to defining  Properties, Statistics and Parameters to
%% Events and Signals.
%% 
%%         Parameter Name: only descriptive
%%         ParameterID: Is an identifier
%%         Type: One of:
%%                 String: UTF-8 octet string
%%                 Integer: 4 octet signed integer
%%                 Double: 8 octet signed integer
%%                 Character: Unicode UTF-8 encoding of a single letter.
%%                         Could be more than one octet.
%%                 Enumeration: One of a list of possible unique values
%%                         (See 12.3)
%%                 Sub-list: A list of several values from a list
%%                 Boolean
%%         Possible values:
%%         Description:
%% 
%% 
%% 12.3.  Lists
%% 
%% Possible values for parameters include enumerations.  Enumerations may
%% be defined in a list.  It is recommended that the list be IANA
%% registered so that packages that extend the list can be defined without
%% concern for conflicting names.
%% 
%% 12.4.  Identifiers
%% 
%% Identifiers in text encoding shall be strings of up to 64 characters,
%% containing no spaces, starting with an alphanumeric character and con-
%% sisting of alphanumeric characters and / or digits, and possibly includ-
%% ing the special character underscore ("_").  Identifiers in binary
%% encoding are 2 octets long.  Both text and binary values shall be speci-
%% fied for each identifier, including identifiers used as values in
%% enumerated types.
%%----------------------------------------------------------------------

capabilities() ->
    [{P, capabilities(P)} || P <- packages()].

%% -record(property, {name, type, values, defined_in, characteristics}).

%%----------------------------------------------------------------------
%% List all known packages
%% 'native' and 'all' are not real packages
%%----------------------------------------------------------------------

packages() ->
    [
     "g",        % Generic
     "root",	 % Base Root Package
     "tonegen",	 % Tone Generator Package
     "tonedet",	 % Tone Detection Package
     "dg",	 % Basic DTMF Generator Package
     "dd",	 % DTMF detection Package
     "cg",	 % Call Progress Tones Generator Package
     "cd",	 % Call Progress Tones Detection Package
     "al",	 % Analog Line Supervision Package
     "ct",	 % Basic Continuity Package
     "nt",	 % Network Package
     "rtp",	 % RTP Package
     "swb",	 % SwitchBoard Package
     "tdmc",     % TDM Circuit Package
     ""          % Native pseudo package
    ].

%%----------------------------------------------------------------------
%% List all matching capabilities
%%----------------------------------------------------------------------

capabilities(Package) ->
    case Package of
        "g"       -> capabilities_g();
        "root"    -> capabilities_root();
        "tonegen" -> capabilities_tonegen();
        "tonedet" -> capabilities_tonedet();
        "dg"      -> capabilities_dg();
        "dd"      -> capabilities_dd();
        "cg"      -> capabilities_cg();
        "cd"      -> capabilities_cd();
        "al"      -> capabilities_al();
        "ct"      -> capabilities_ct();
        "nt"      -> capabilities_nt();
        "rtp"     -> capabilities_rtp();
	"swb"     -> capabilities_swb();
        "tdmc"    -> capabilities_tdmc();
        ""        -> capabilities_native()
    end.

%%----------------------------------------------------------------------
%% Decode package name to internal form
%% Scope  ::= property | event | signal | statistics
%%----------------------------------------------------------------------

decode(mid, Package) ->
    decode_mid(Package);
decode(package, Package) ->
    decode_package(Package);
decode(profile, Package) ->
    decode_profile(Package);
decode(dialplan, Dialplan) ->
    decode_dialplan(Dialplan);
decode(Scope, [A, B | Item]) when atom(Scope) ->
%     i("decode(~p) -> entry with"
%       "~n   A:    ~p"
%       "~n   B:    ~p"
%       "~n   Item: ~p",[Scope,A,B,Item]),
    case decode_package([A, B]) of
	"" ->
% 	    i("decode_package -> \"no\" package",[]),
	    decode_item(Scope, [A, B], Item);
	Package ->
% 	    i("decode -> Package: ~p",[Package]),
	    Package ++ "/" ++ decode_item(Scope, [A, B], Item)
    end;
decode({Scope, [A, B | Item]}, SubItem) when atom(Scope) ->
%     i("decode(~p) -> entry with"
%       "~n   A:       ~p"
%       "~n   B:       ~p"
%       "~n   Item:    ~p"
%       "~n   SubItem: ~p",[Scope, A, B, Item, SubItem]),
    decode_item({Scope, Item}, [A, B], SubItem).

decode_item(Scope, [A, B], Item) ->
%     i("decode_item -> entry",[]),
    case A of
        16#00 -> 
            case B of
                16#0e -> decode_g(Scope, Item);
                16#0f -> decode_root(Scope, Item);
                16#01 -> decode_tonegen(Scope, Item);
                16#02 -> decode_tonedet(Scope, Item);
                16#03 -> decode_dg(Scope, Item);
                16#04 -> decode_dd(Scope, Item);
                16#05 -> decode_cg(Scope, Item);
                16#06 -> decode_cd(Scope, Item);
                16#09 -> decode_al(Scope, Item);
                16#0a -> decode_ct(Scope, Item);
                16#0b -> decode_nt(Scope, Item);
                16#0c -> decode_rtp(Scope, Item);
                16#0d -> decode_tdmc(Scope, Item);
                16#00 -> decode_native(Scope, Item)
            end;
        16#fe ->
            case B of
                %% Proprietary extension
                16#fe -> decode_swb(Scope, Item)
            end;
        16#ff ->
            case B of
                16#ff when Item == [16#ff, 16#ff] -> "*"
            end
    end.

decode_package(Package) ->
%     i("decode_package -> entry with Package: ~p",[Package]),
    [A, B] = Package,
    case A of
        16#00 -> 
            case B of
                16#0e -> "g";
                16#0f -> "root";
                16#01 -> "tonegen";
                16#02 -> "tonedet";
                16#03 -> "dg";
                16#04 -> "dd";
                16#05 -> "cg";
                16#06 -> "cd";
                16#09 -> "al";
                16#0a -> "ct";
                16#0b -> "nt";
                16#0c -> "rtp";
                16#0d -> "tdmc";
                16#00 -> ""
            end;
        16#fe ->
            case B of
                16#fe ->  "swb"
            end;
        16#ff ->
            case B of
                16#ff -> "*"
            end
    end.

decode_profile([A, B]) ->
    case A of
        16#00 -> 
            case B of
                16#fe -> "resgw";
		_     -> "profile" ++ [A + $0, B + $0]
            end;
	_ ->
	    "profile" ++ [A + $0, B + $0]
    end.

decode_dialplan([A, B]) ->
    "dialplan" ++ [A + $0, B + $0].

decode_mid(Mid) ->
    case Mid of
	{domainName, DN} ->
	    Lower = to_lower(DN#'DomainName'.name),	    
	    {domainName, DN#'DomainName'{name = Lower}};
	{deviceName, PathName} ->
	    Lower = to_lower(PathName),
	    {deviceName, Lower};
	Other ->
	    Other
    end.

to_lower(Chars) ->
    [?LOWER(Char) || Char <- Chars].

%%----------------------------------------------------------------------
%% Encode package name from internal form
%% Scope  ::= property | event | signal | statistics
%%----------------------------------------------------------------------

encode(mid, Package) ->
    encode_mid(Package);
encode(package, Package) ->
    encode_package(Package);
encode(profile, Profile) ->
    encode_profile(Profile);
encode(dialplan, Dialplan) ->
    encode_dialplan(Dialplan);
encode(Scope, PackageItem) when atom(Scope) ->
%     i("encode(~p) -> entry with PackageItem: ~p",[Scope, PackageItem]),
    case string:tokens(PackageItem, [$/]) of
	[Package, Item] ->
%             i("encode -> "
%               "~n   Package: ~p"
%               "~n   Item:    ~p",[Package, Item]),
	    encode_package(Package) ++ encode_item(Scope, Package, Item);
	[Item] ->
%             i("encode -> Item: ~p",[Item]),
	    [16#00, 16#00 | encode_native(Scope, Item)]
    end;
encode({Scope, PackageItem}, SubItem) when atom(Scope) ->
%     i("encode(~p) -> entry with"
%       "~n   PackageItem: ~p"
%       "~n   SubItem:     ~p",[Scope, PackageItem, SubItem]),
    case string:tokens(PackageItem, [$/]) of
	[Package, Item] ->
%             i("encode -> "
%               "~n   Package: ~p"
%               "~n   Item:    ~p",[Package, Item]),
	    encode_item({Scope, Item}, Package, SubItem);
	[_Item] ->
%             i("encode -> _Item: ~p",[_Item]),
	    encode_native(Scope, SubItem)
    end.

encode_item(_Scope, _Package, "*") ->
    [16#ff, 16#ff];
encode_item(Scope, Package, Item) ->
%     i("encode_item(~s) -> entry",[Package]),
    case Package of
        "g"       -> encode_g(Scope, Item);
        "root"    -> encode_root(Scope, Item);
        "tonegen" -> encode_tonegen(Scope, Item);
        "tonedet" -> encode_tonedet(Scope, Item);
        "dg"      -> encode_dg(Scope, Item);
        "dd"      -> encode_dd(Scope, Item);
        "cg"      -> encode_cg(Scope, Item);
        "cd"      -> encode_cd(Scope, Item);
        "al"      -> encode_al(Scope, Item);
        "ct"      -> encode_ct(Scope, Item);
        "nt"      -> encode_nt(Scope, Item);
        "rtp"     -> encode_rtp(Scope, Item);
        "swb"     -> encode_swb(Scope, Item);
        "tdmc"    -> encode_tdmc(Scope, Item);
        ""        -> encode_native(Scope, Item)
    end.

encode_package(Package) ->
    case Package of
        "g"       -> [16#00, 16#0e];
        "root"    -> [16#00, 16#0f];
        "tonegen" -> [16#00, 16#01];
        "tonedet" -> [16#00, 16#02];
        "dg"      -> [16#00, 16#03];
        "dd"      -> [16#00, 16#04];
        "cg"      -> [16#00, 16#05];
        "cd"      -> [16#00, 16#06];
        "al"      -> [16#00, 16#09];
        "ct"      -> [16#00, 16#0a];
        "nt"      -> [16#00, 16#0b];
        "rtp"     -> [16#00, 16#0c];
        "swb"     -> [16#00, 16#0c];
        "tdmc"    -> [16#00, 16#0d];
        ""        -> [16#00, 16#00];
	"*"       -> [16#ff, 16#ff]
    end.

encode_profile(Profile) ->
    case Profile of
        "resgw" ->
	    [16#00, 16#fe];
	[$p, $r, $o, $f, $i, $l, $e | Name] ->
	    case Name of
		[A, B] -> [A - $0, B - $0];
		[B]    -> [0, B - $0];
		[]     -> [0, 0]
	    end
    end.

encode_dialplan(Dialplan) ->
    case Dialplan of
	[$d, $i, $a, $l, $p, $l, $a, $n | Name] ->
	    case Name of
		[A, B] -> [A - $0, B - $0];
		[B]    -> [0, B - $0];
		[]     -> [0, 0]
	    end
    end.

encode_mid(Mid) ->
    Mid.


%%----------------------------------------------------------------------
%% Name:    g - Generic
%% Version: 1
%% Extends: None
%% Purpose: Generic package for commonly encountered items
%%----------------------------------------------------------------------

capabilities_g() ->
    [
     {event, "cause"},
     {event, "sc"}
    ].

encode_g(event, Item) ->
    case Item of
	"cause"  -> [16#00, 16#01];
	"sc"     -> [16#00, 16#02]
    end;

encode_g({event_parameter, Item}, SubItem) ->
    case Item of
	"cause"  -> 
	    case SubItem of
		"Generalcause" -> [16#00, 16#01];
		"Failurecause" -> [16#00, 16#02]
	    end;
	"sc" ->
	    case SubItem of
		"SigID" -> [16#00, 16#01];
		"Meth"  -> [16#00, 16#02];
		"SLID"  -> [16#00, 16#03]
	    end
    end.

decode_g(event, Item) ->
    case Item of
	[16#00, 16#01] -> "cause";
	[16#00, 16#02] -> "sc"
    end;

decode_g({event_parameter, Item}, SubItem) ->
    case Item of
        [16#00, 16#01] -> % Event: cause
            case SubItem of
                [16#00, 16#01] -> "Generalcause";
		[16#00, 16#02] -> "Failurecause"
            end;

        [16#00, 16#02] -> % Event: sc
            case SubItem of
                [16#00, 16#01] -> "SigID";
                [16#00, 16#02] -> "Meth";
		[16#00, 16#03] -> "SLID"
            end
    end.


%%----------------------------------------------------------------------
%% Name:    root - Base Root Package
%% Version: 1
%% Extends: None
%% Purpose: This package defines Gateway wide properties.
%%----------------------------------------------------------------------

capabilities_root() ->
    [
     {property, "maxNumberOfContexts"},
     {property, "maxTerminationsPerContext"},
     {property, "normalMGExecutionTime"},
     {property, "normalMGCExecutionTime"},
     {property, "MGProvisionalResponseTimerValue"},  %% BUGBUG: leading capital?
     {property, "MGCProvisionalResponseTimerValue"}, %% BUGBUG: leading capital?
     {property, "MGCOriginatedPendingLimit"},        %% BUGBUG: leading capital?
     {property, "MGOriginatedPendingLimit"}          %% BUGBUG: leading capital?
    ].

encode_root(Scope, Item) ->
    case Scope of
        property ->
            case Item of
                "maxNumberOfContexts"              -> [16#00, 16#01];
                "maxTerminationsPerContext"        -> [16#00, 16#02];
                "normalMGExecutionTime"            -> [16#00, 16#03];
                "normalMGCExecutionTime"           -> [16#00, 16#04];
                "MGProvisionalResponseTimerValue"  -> [16#00, 16#05];
                "MGCProvisionalResponseTimerValue" -> [16#00, 16#06];
                "MGCOriginatedPendingLimit"        -> [16#00, 16#07];
                "MGOriginatedPendingLimit"         -> [16#00, 16#08]
            end
    end.

decode_root(Scope, Item) ->
    case Scope of
        property ->
            case Item of
                [16#00, 16#01] -> "maxNumberOfContexts";
                [16#00, 16#02] -> "maxTerminationsPerContext";
                [16#00, 16#03] -> "normalMGExecutionTime";
                [16#00, 16#04] -> "normalMGCExecutionTime";
                [16#00, 16#05] -> "MGProvisionalResponseTimerValue";
		[16#00, 16#06] -> "MGCProvisionalResponseTimerValue";
		[16#00, 16#07] -> "MGCOriginatedPendingLimit";
		[16#00, 16#08] -> "MGOriginatedPendingLimit"
            end
    end.


%%----------------------------------------------------------------------
%% Name:    tonegen - Tone Generator Package
%% Version: 1
%% Extends: None
%% Purpose: This package defines signals to generate audio tones.
%%          This package does not specify parameter values. It is
%%          intended to be extendable. Generally, tones are defined
%%          as an individual signal with a parameter, ind,
%%          representing "interdigit" time delay, and a tone id to
%%          be used with playtones.  A tone id should be kept
%%          consistent with any tone generation for the same tone.
%%          MGs are expected to be provisioned with the characteristics
%%          of appropriate tones for the country in which the MG is located.
%%----------------------------------------------------------------------

capabilities_tonegen() ->
    [
     {signal, "pt"}
    ].

encode_tonegen(signal, Item) ->
    case Item of
	"pt" -> [16#00, 16#01]
    end;

encode_tonegen({signal_parameter, Item}, SubItem) ->
    case Item of
        "pt" ->
            case SubItem of
                "tl"  -> [16#00, 16#01];
                "ind" -> [16#00, 16#02]
            end
    end.

decode_tonegen(signal, Item) ->
    case Item of
	[16#00, 16#01] -> "pt"
    end;

decode_tonegen({signal_parameter, Item}, _SubItem) ->
    case Item of
        [16#00, 16#01] -> % Event: pt
            case Item of
                [16#00, 16#01] -> "tl";
                [16#00, 16#02] -> "ind"
            end
    end.


%%----------------------------------------------------------------------
%% Name:    tonedet - Tone Detection Package
%% Version: 1
%% Extends: None
%% Purpose: This Package defines events for audio tone detection.
%%          Tones are selected by name (tone id). MGs are expected
%%          to be provisioned with the characteristics of appropriate
%%          tones for the country in which the MG is located.
%%          
%%          This package does not specify parameter values.
%%          It is intended to be extendable.
%%----------------------------------------------------------------------

capabilities_tonedet() ->
    [
     {event, "std"},
     {event, "etd"},
     {event, "ltd"}
    ].

encode_tonedet(event, Item) ->
    case Item of
	"std" -> [16#00, 16#01];
	"etd" -> [16#00, 16#02];
	"ltd" -> [16#00, 16#03]
    end;

encode_tonedet({event_parameter, Item}, SubItem) ->
    case Item of
        "std" ->
            case SubItem of
                "tl"  -> [16#00, 16#01];
                "tid" -> [16#00, 16#03]
            end;
        "etd" ->
            case SubItem of
                "tl"  -> [16#00, 16#01];
                "tid" -> [16#00, 16#03];
                "dur" -> [16#00, 16#02]
            end;
        "ltd" ->
            case SubItem of
                "tl"  -> [16#00, 16#01];
                "dur" -> [16#00, 16#02];
                "tid" -> [16#00, 16#03]
            end
    end.

decode_tonedet(event, Item) ->
    case Item of
	[16#00, 16#01] -> "std";
	[16#00, 16#02] -> "etd";
	[16#00, 16#03] -> "ltd"
    end;

decode_tonedet({event_parameter, Item}, SubItem) ->
    case Item of
        [16#00, 16#01] -> % Event std
            case SubItem of
                [16#00, 16#01] -> "tl";
                [16#00, 16#03] -> "tid"
            end;
        [16#00, 16#02] -> % Event etd
            case SubItem of
                [16#00, 16#01] -> "tl";
                [16#00, 16#03] -> "tid";
                [16#00, 16#02] -> "dur"
            end;
        [16#00, 16#03] -> % Event ltd
            case SubItem of
                [16#00, 16#01] -> "tl";
                [16#00, 16#02] -> "dur";
                [16#00, 16#03] -> "tid"
            end
    end.

%%----------------------------------------------------------------------
%% Name:    dg - Basic DTMF Generator Package
%% Version: 1
%% Extends: tonegen  version 1
%% Purpose: This package defines the basic DTMF tones as signals and
%%          extends the allowed values of parameter tl of playtone
%%          in tonegen.
%%----------------------------------------------------------------------

capabilities_dg() ->
    [
     {signal, "d0"},
     {signal, "d1"},
     {signal, "d2"},
     {signal, "d3"},
     {signal, "d4"},
     {signal, "d5"},
     {signal, "d6"},
     {signal, "d7"},
     {signal, "d8"},
     {signal, "d9"},
     {signal, "d*"},
     {signal, "d#"},
     {signal, "da"},
     {signal, "db"},
     {signal, "dc"},
     {signal, "dd"}
    ].

encode_dg(Scope, Item) ->
    case Scope of
        signal ->
            case Item of
                "d0" -> [16#00, 16#10];
                "d1" -> [16#00, 16#11];
                "d2" -> [16#00, 16#12];
                "d3" -> [16#00, 16#13];
                "d4" -> [16#00, 16#14];
                "d5" -> [16#00, 16#15];
                "d6" -> [16#00, 16#16];
                "d7" -> [16#00, 16#17];
                "d8" -> [16#00, 16#18];
                "d9" -> [16#00, 16#19];
                "d*" -> [16#00, 16#20];
                "d#" -> [16#00, 16#21];
                "da" -> [16#00, 16#1a];
                "db" -> [16#00, 16#1b];
                "dc" -> [16#00, 16#1c];
                "dd" -> [16#00, 16#1d]
            end
    end.

decode_dg(Scope, Item) ->
    case Scope of
        signal ->
            case Item of
                [16#00, 16#10] -> "d0";
                [16#00, 16#11] -> "d1";
                [16#00, 16#12] -> "d2";
                [16#00, 16#13] -> "d3";
                [16#00, 16#14] -> "d4";
                [16#00, 16#15] -> "d5";
                [16#00, 16#16] -> "d6";
                [16#00, 16#17] -> "d7";
                [16#00, 16#18] -> "d8";
                [16#00, 16#19] -> "d9";
                [16#00, 16#20] -> "d*";
                [16#00, 16#21] -> "d#";
                [16#00, 16#1a] -> "da";
                [16#00, 16#1b] -> "db";
                [16#00, 16#1c] -> "dc";
                [16#00, 16#1d] -> "dd"
            end
    end.

%%----------------------------------------------------------------------
%% Name:    dd - DTMF detection Package
%% Version: 1
%% Extends: tonedet version 1
%% Purpose: This package defines the basic DTMF tones detection.
%%          Tones are selected by name (tone id). MGs are expected
%%          to be provisioned with the characteristics of appropriate
%%          tones for the country in which the MG is located.
%%          
%%          This package does not specify parameter values.
%%          It is intended to be extendable.
%% 
%% Additional tone id values are all tone ids described in package dg
%% (basic DTMF generator package).
%% 
%% The following table maps DTMF events to digit map symbols as described
%% in section 7.1.14.
%% 
%%                    _________________________________
%%                   | DTMF Event        | Symbol     |
%%                   | d0                |  "0"       |
%%                   | d1                |  "1"       |
%%                   | d2                |  "2"       |
%%                   | d3                |  "3"       |
%%                   | d4                |  "4"       |
%%                   | d5                |  "5"       |
%%                   | d6                |  "6"       |
%%                   | d7                |  "7"       |
%%                   | d8                |  "8"       |
%%                   | d9                |  "9"       |
%%                   | da                |  "A" or "a"|
%%                   | db                |  "B" or "b"|
%%                   | dc                |  "C" or "c"|
%%                   | dd                |  "D" or "d"|
%%                   | ds                |  "E" or "e"|
%%                   | do                |  "F" or "f"|
%%                   |___________________|____________|
%% 
%%----------------------------------------------------------------------

capabilities_dd() ->
    [
     {event, "ce"}
    ].

encode_dd(event, Item) ->
    case Item of
	"ce" -> [16#00, 16#04]
    end;

encode_dd({event_parameter, Item}, SubItem) ->
    case Item of
        "ce" ->
            case SubItem of
                "ds"   -> [16#00, 16#01];
		"Meth" -> [16#00, 16#03]
            end
    end.

decode_dd(event, Item) ->
    case Item of
	[16#00, 16#04] -> "ce"
    end;

decode_dd({event_parameter, Item}, SubItem) ->
    case Item of
         [16#00, 16#04] -> % Event ce
            case SubItem of
                [16#00, 16#01] -> "ds";
		[16#00, 16#03] -> "Meth"
            end
    end.

%%----------------------------------------------------------------------
%% Name:    cg - Call Progress Tones Generator Package
%% Version: 1
%% Extends: tonegen version 1
%% Purpose: This package defines the basic call progress tones as signals
%%          and extends the allowed values of the tl parameter of
%%          playtone in tonegen.
%%----------------------------------------------------------------------

capabilities_cg() ->
    [
     {signal, "dt"},
     {signal, "rt"},
     {signal, "bt"},
     {signal, "ct"},
     {signal, "sit"},
     {signal, "wt"},
     {signal, "prt"},
     {signal, "cw"},
     {signal, "cr"}
    ].


encode_cg(Scope, Item) ->
    case Scope of
        signal ->
            case Item of
                "dt"  -> [16#00, 16#30];
                "rt"  -> [16#00, 16#31];
                "bt"  -> [16#00, 16#32];
                "ct"  -> [16#00, 16#33];
                "sit" -> [16#00, 16#34];
                "wt"  -> [16#00, 16#35];
                "prt" -> [16#00, 16#36];
                "cw"  -> [16#00, 16#37];
                "cr"  -> [16#00, 16#38]
            end
    end.

decode_cg(Scope, Item) ->
    case Scope of
        signal ->
            case Item of
                [16#00, 16#30] -> "dt";              
                [16#00, 16#31] -> "rt";           
                [16#00, 16#32] -> "bt";              
                [16#00, 16#33] -> "ct";        
                [16#00, 16#34] -> "sit";         
                [16#00, 16#35] -> "wt";              
                [16#00, 16#36] -> "prt"; 
                [16#00, 16#37] -> "cw";         
                [16#00, 16#38] -> "cr"
            end
    end.

%%----------------------------------------------------------------------
%% Name:    cd - Call Progress Tones Detection Package
%% Version: 1
%% Extends: tonedet version 1
%% Purpose: This package defines the basic call progress detection tones.
%%          This Package extends the possible values of tone id
%%          in the "start tone detected", "end tone detected" and
%%          "long tone detected" events.
%%       Additional values
%%             tone id values are defined for start tone detected,
%%                   end tone detected and long tone detected with
%%                   the same values as those in package cg (call
%%                   progress tones generation package).
%% 
%% The required set of tone ids corresponds to Recommendation E.180/Q.35
%% [ITU-T Recommendation E.180/Q.35 (1998)].  See Recommendation E.180/Q.35
%% for definition of the meanings of these tones.
%%----------------------------------------------------------------------

capabilities_cd() ->
    [
     {event, "dt"},
     {event, "rt"},
     {event, "bt"},
     {event, "ct"},
     {event, "sit"},
     {event, "wt"},
     {event, "prt"},
     {event, "cw"},
     {event, "cr"}
    ].


encode_cd(Scope, Item) ->
    case Scope of
        event ->
            case Item of
                "dt" -> [16#00, 16#30];
                "rt" -> [16#00, 16#31];
                "bt" -> [16#00, 16#32];
                "ct" -> [16#00, 16#33];
                "sit"-> [16#00, 16#34];
                "wt" -> [16#00, 16#35];
                "prt"-> [16#00, 16#36];
                "cw" -> [16#00, 16#37];
                "cr" -> [16#00, 16#38]
            end
    end.

decode_cd(Scope, Item) ->
    case Scope of
        event ->
            case Item of
                [16#00, 16#30] -> "dt";              
                [16#00, 16#31] -> "rt";           
                [16#00, 16#32] -> "bt";              
                [16#00, 16#33] -> "ct";        
                [16#00, 16#34] -> "sit";         
                [16#00, 16#35] -> "wt";              
                [16#00, 16#36] -> "prt"; 
                [16#00, 16#37] -> "cw";         
                [16#00, 16#38] -> "cr"
            end
    end.

%%----------------------------------------------------------------------
%% Name:    al - Analog Line Supervision Package
%% Version: 1
%% Extends: None
%% Purpose: This package defines events and signals for an analog line.
%%----------------------------------------------------------------------

capabilities_al() ->
    [
     {event,  "on"},
     {event,  "of"},
     {event,  "fl"},
     {signal, "ri"}
    ].

encode_al(event, Item) ->
%     i("encode_al(event) -> entry with Item: ~p",[Item]),
    case Item of
	"on" -> [16#00, 16#04];
	"of" -> [16#00, 16#05];
	"fl" -> [16#00, 16#06]
    end;

encode_al({event_parameter, Item}, SubItem) ->
%     i("encode_al({event_parameter,~p}) -> entry with SubItem: ~p",
%       [Item,SubItem]),
    case Item of
	"on" ->
            case SubItem of
                "strict" -> [16#00, 16#01];
                "init"   -> [16#00, 16#02]
            end;
        "of" ->
            case SubItem of
                "strict" -> [16#00, 16#01];
                "init"   -> [16#00, 16#02]
            end;
        "fl" ->
            case SubItem of
                "mindur" -> [16#00, 16#04];
                "maxdur" -> [16#00, 16#05]
            end
    end;

encode_al(signal, Item) ->
%     i("encode_al(signal) -> entry with Item: ~p",[Item]),
    case Item of
	"ri"    -> [16#00, 16#02]
    end;

encode_al({signal_parameter, Item}, SubItem) ->
%     i("encode_al({signal_parameter,~p}) -> entry with SubItem: ~p",
%       [Item,SubItem]),
    case Item of
        "ri" ->
            case SubItem of
                "cad"  -> [16#00, 16#06];
                "freq" -> [16#00, 16#07]
            end
    end.

decode_al(event, SubItem) ->
%     i("decode_al(event) -> entry with"
%       "~n   SubItem: ~p",[SubItem]),
    case SubItem of
	[16#00, 16#04] -> "on";
	[16#00, 16#05] -> "of";
	[16#00, 16#06] -> "fl"
    end;

decode_al({event_parameter,Item}, SubItem) ->
%     i("decode_al({event_parameter,~p}) -> entry with"
%       "~n   SubItem: ~p",[Item,SubItem]),
    case Item of
        [16#00,16#04] -> %% Event: on
            case SubItem of
		[16#00, 16#01] -> "strict";
                [16#00, 16#02] -> "init"
            end;
        [16#00,16#05] -> %% Event: of
            case SubItem of
		[16#00, 16#01] -> "strict";
                [16#00, 16#02] -> "init"
            end;
        [16#00,16#06] -> %% Event: fl
            case SubItem of
		[16#00, 16#04] -> "mindur";
                [16#00, 16#05] -> "maxdur"
            end
    end;

decode_al(signal, SubItem) ->
%     i("decode_al(signal) -> entry with"
%       "~n   SubItem: ~p",[SubItem]),
    case SubItem of
	[16#00, 16#02] -> "ri"
    end;

decode_al({signal_parameter,Item}, SubItem) ->
%     i("decode_al({signal_parameter,~p}) -> entry with"
%       "~n   SubItem: ~p",[Item,SubItem]),
    case Item of
        [16#00,16#02] -> %% Event: ri
            case SubItem of
		[16#00, 16#06] -> "cad";
                [16#00, 16#07] -> "freq"
            end
    end.


%%----------------------------------------------------------------------
%% Name:    ct - Basic Continuity Package
%% Version: 1
%% Extends: None
%% Purpose: This package defines events and signals for continuity test.
%%          The continuity test includes provision of either a loopback
%%          or transceiver functionality.
%%----------------------------------------------------------------------

capabilities_ct() ->
    [
     {event,  "cmp"},
     {signal, "ct"},
     {signal, "rsp"}
    ].

encode_ct(event, Item) ->
    case Item of
	"cmp" -> [16#00, 16#05]
    end;
encode_ct({event_parameter, Item}, SubItem) ->
    case Item of
        "cmp" ->
            case SubItem of
                "res" -> [16#00, 16#08]
            end
    end;
encode_ct(signal, Item) ->
    case Item of
	"ct"  -> [16#00, 16#03];
	"rsp" -> [16#00, 16#04]
    end.

decode_ct(event, Item) ->
    case Item of
	[16#00, 16#05] -> "cmp"
    end;
decode_ct({event_parameter, Item}, SubItem) ->
    case Item of
        [16#00, 16#05] -> % Event cmp
            case SubItem of
                [16#00, 16#08] -> "res"
            end
    end;
decode_ct(signal, Item) ->
    case Item of
	[16#00, 16#03] -> "ct";
	[16#00, 16#04] -> "rsp"
    end.

%%----------------------------------------------------------------------
%% Name:    nt - Network Package
%% Version: 1
%% Extends: None
%% Purpose: This package defines properties of network terminations
%%          independent of network type.
%%----------------------------------------------------------------------

capabilities_nt() ->
    [
     {property,   "jit"},
     {event,      "netfail"},
     {event,      "qualert"},
     {statistics, "dur"},
     {statistics, "os"},
     {statistics, "or"}
    ].

encode_nt(property, Item) ->
    case Item of
	"jit" -> [16#00, 16#07]
    end;
encode_nt(event, Item) ->
    case Item of
	"netfail" -> [16#00, 16#05];
	"qualert" -> [16#00, 16#06]
    end;
encode_nt({event_parameter, Item}, SubItem) ->
    case Item of
        "netfail" ->
            case SubItem of
                "cs" -> [16#00, 16#06]
            end;
        "qualert" ->
            case SubItem of
                "th"  -> [16#00, 16#01]
            end
    end;
encode_nt(statistics, Item) ->
    case Item of
	"dur" -> [16#00, 16#01];
	"os"  -> [16#00, 16#02];
	"or"  -> [16#00, 16#03]
    end.

decode_nt(property, Item) ->
    case Item of
	[16#00, 16#07] -> "jit"
    end;
decode_nt(event, Item) ->
    case Item of
	[16#00, 16#05] -> "netfail";
	[16#00, 16#06] -> "qualert"
    end;
decode_nt({event_parameter, Item}, SubItem) ->
    case Item of
        [16#00, 16#05] -> % Event netfail
            case SubItem of
                [16#00, 16#06] -> "cs"
            end;
        [16#00, 16#06] -> % Event qualert
            case Item of
                [16#00, 16#01] -> "th"
            end
	end;
decode_nt(statistics, Item) ->
    case Item of
	[16#00, 16#01] -> "dur";
	[16#00, 16#02] -> "os";
	[16#00, 16#03] -> "or"
    end.

%%----------------------------------------------------------------------
%% Name:    rtp - RTP Package
%% Version: 1
%% Extends: nt version 1
%% Purpose: This package is used to support packet based multimedia
%%          data transfer by means of the Real-time Transport Protocol
%%          (RTP) [RFC 1889].
%%----------------------------------------------------------------------

capabilities_rtp() ->
    [
     {event,      "pltrans"},
     {statistics, "ps"},
     {statistics, "pr"},
     {statistics, "pl"},
     {statistics, "jit"},
     {statistics, "delay"}
    ].

encode_rtp(event, Item) ->
    case Item of
	"pltrans" -> [16#00, 16#01]
    end;
encode_rtp({event_parameter, Item}, SubItem) ->
    case Item of
        "pltrans" ->
            case SubItem of
                "rtppltype" -> [16#00, 16#01]
            end
    end;
encode_rtp(statistics, Item) ->
    case Item of
	"ps"    -> [16#00, 16#04];
	"pr"    -> [16#00, 16#05];
	"pl"    -> [16#00, 16#06];
	"jit"   -> [16#00, 16#07];
	"delay" -> [16#00, 16#08]
    end.

decode_rtp(event, Item) ->
    case Item of
	[16#00, 16#01] -> "pltrans"
    end;
decode_rtp({event_parameterm, Item}, SubItem) ->
    case Item of
        [16#00, 16#01] -> % Event pltrans
            case SubItem of
                [16#00, 16#01] -> "rtppltype"
            end
    end;
decode_rtp(statistics, Item) ->
    case Item of
	[16#00, 16#04] -> "ps";
	[16#00, 16#05] -> "pr";
	[16#00, 16#06] -> "pl";
	[16#00, 16#07] -> "jit";
	[16#00, 16#08] -> "delay"
    end.


%%----------------------------------------------------------------------
%% Name:    tdmc - TDM Circuit Package
%% Version: 1
%% Extends: nt version 1
%% Purpose: This package is used to support TDM circuit terminations.
%%----------------------------------------------------------------------

capabilities_tdmc() ->
    [
     {property, "ec"},
     {property, "gain"}
    ].

encode_tdmc(Scope, Item) ->
    case Scope of
        property ->
            case Item of
                "ec"   -> [16#00, 16#08];
                "gain" -> [16#00, 16#0a]
            end
    end.

decode_tdmc(Scope, Item) ->
    case Scope of
        property ->
            case Item of
                [16#00, 16#08] -> "ec";
                [16#00, 16#0a] -> "gain"
            end
    end.


%%----------------------------------------------------------------------
%% Name:    swb - SwitchBoard Package
%% Version: 1
%% Extends: none
%% Purpose: This package is used to support SwitchBoard specials
%%----------------------------------------------------------------------

capabilities_swb() ->
    [
     {statistics, "fs"}, % Free slots
     {statistics, "as"}  % Allocated slots
    ].

encode_swb(Scope, Item) ->
    case Scope of
        statistics ->
            case Item of
                "fs" -> [16#00, 16#00];
                "as" -> [16#00, 16#01]
            end
    end.

decode_swb(Scope, Item) ->
    case Scope of
        statistics ->
            case Item of
                [16#00, 16#00] -> "fs";
                [16#00, 16#01] -> "as"
            end
    end.


%%----------------------------------------------------------------------
%% Name:  native -  Pseudo package  
%% Version: 1
%% Extends: None
%% Purpose: Native tags for media stream properties
%%
%% Parameters for Local descriptors and Remote descriptors are
%% specified as tag-value pairs if binary encoding is used for the
%% protocol.  This annex contains the property names (PropertyID), the
%% tags (Property Tag), type of the property (Type) and the values
%% (Value).Values presented in the Value field when the field contains
%% references shall be regarded as "information". The reference
%% contains the normative values.  If a value field does not contain a
%% reference then the values in that field can be considered as
%% "normative".
%% 
%% Tags are given as hexadecimal numbers in this annex. When setting
%% the value of a property, a MGC may underspecify the value according
%% to one of the mechanisms specified in section 7.1.1.
%% 
%% For type "enumeration" the value is represented by the value in brack-
%% ets, e.g., Send(0), Receive(1).
%%----------------------------------------------------------------------
%% 
%% C.1.  General Media Attributes
%% 
%% ________________________________________________________________________
%% |PropertyID   | Tag |  Type     |  Value                               |
%% |Media        |1001 |Enumeration|Audio(0), Video(1) ,Data(2),          |
%% |TransMode    |1002 |Enumeration|Send(0), Receive(1), Send&Receive(2)  |
%% |NumChan      |1003 |UINT       | 0-255                                |
%% |SamplingRate |1004 |UINT       | 0-2^32                               |
%% |Bitrate      |1005 |Integer    |(0..4294967295) Note-units of 100 bit |
%% |Acodec       |1006 |Octet str  |Audio Codec Type                      |
%% |Samplepp     |1007 |UINT       |Maximum samples/fr per packet:0-65535 |
%% |Silencesupp  |1008 |BOOLEAN    |Silence Suppression                   |
%% |Encrypttype  |1009 |Octet str  |Ref.: rec. H.245                      |
%% |Encryptkey   |100A |Octet str  |SIZE(0..65535) Encryption key         |
%% |Echocanc     |100B |Enumeration|Echo Canceller:Off(0),G.165(1),G168(2)|
%% |Gain         |100C |UINT       |Gain in db: 0-65535                   |
%% |Jitterbuff   |100D |UINT       |Jitter buffer size in ms: 0-65535     |
%% |PropDelay    |100E |UINT       |  Propagation Delay: 0..65535         |
%% |RTPpayload   |100F |integer    |Payload type in RTP Profile           |
%% |_____________|_____|___________|______________________________________|
%% 
%% 
%% C.2.  Mux Properties
%% 
%% _________________________________________________________________________
%% |PropertyID|  Tag       |  Type        |  Value                         |
%% |H.221     |  2001      |  Octet string|   H222LogicalChannelParameters |
%% |H223      |  2002      |  Octet string|   H223LogicalChannelParameters |
%% |V76       |  2003      |  Octet String|   V76LogicalChannelParameters  |
%% |H2250     |  2004      |  Octet String|   H2250LogicalChannelParameters|
%% |__________|____________|______________|________________________________|
%% 
%% 
%% C.3.  General Bearer Properties
%% 
%%  _____________________________________________________________________
%% | PropertyID|  Tag       |  Type       |  Value                      |
%% | Mediatx   |  3001      |  Enumeration|  Media Transport Type       |
%% | BIR       |  3002      |  4 OCTET    |  Value depends on transport |
%% | NSAP      |  3003      |  1-20 OCTETS|  Ref: ITU X.213 Annex A     |
%% |___________|____________|_____________|_____________________________|
%% 
%% C.4.  General ATM Properties
%% 
%%    _________________________________________________________________
%%   | PropertyID|  Tag |  Type       |  Value                        |
%%   | AESA      |  4001|  20 OCTETS  |  ATM End System Address       |
%%   | VPVC      |  4002|  2x16b int  |  VPC-VCI                      |
%%   | SC        |  4003|  4 bits     |  Service Category             |
%%   | BCOB      |  4004|  5b integer |  Broadband Bearer Class       |
%%   | BBTC      |  4005|  octet      |  Broadband Transfer Capability|
%%   | ATC       |  4006|  Enumeration|  I.371 ATM Traffic Cap.       |
%%   | STC       |  4007|  2 bits     |  Susceptibility to clipping   |
%%   | UPCC      |  4008|  2 bits     |  User Plane Connection config |
%%   | PCR0      |  4009|  24b integer|  Peak Cell Rate CLP=0         |
%%   | SCR0      |  400A|  24b integer|  Sustainable Cell Rate CLP=0  |
%%   | MBS0      |  400B|  24b integer|  Maximum Burst Size CLP=0     |
%%   | PCR1      |  400C|  24b integer|  Peak Cell Rate CLP=0+1       |
%%   | SCR2      |  400D|  24b integer|  Sustain. Cell Rate CLP=0+1   |
%%   | MBS3      |  400E|  24b integer|  Maximum Burst Size CLP=0+1   |
%%   | BEI       |  400F|  Boolean    |  Best Effort Indicator        |
%%   | TI        |  4010|  Boolean    |  Tagging                      |
%%   | FD        |  4011|  Boolean    |  Frame Discard                |
%%   | FCDV      |  4012|  24b integer|  Forward P-P CDV              |
%%   | BCDV      |  4013|  24b integer|  Backward P-P CDV             |
%%   | FCLR0     |  4014|  8b integer |  Fwd Cell Loss Ratio CLP=0    |
%%   | BCLR0     |  4015|  8b integer |  Bkwd P-P CLR CLP=0           |
%%   | FCLR1     |  4016|  8b integer |  Fwd Cell Loss Ratio CLP=0+1  |
%%   | BCLR1     |  4017|  8b integer |  Bkwd P-P CLR CLP=0+1         |
%%   | FCDV      |  4018|  24b integer|  Fwd Cell Delay Variation     |
%%   | BCDV      |  4019|  24b integer|  Bkwd Cell Delay Variation    |
%%   | FACDV     |  401A|  24b integer|  Fwd Acceptable P-P-P CDV     |
%%   | BACDV     |  401B|  24b integer|  Bkwd Acceptable P-P CDV      |
%%   | FCCDV     |  401C|  24b integer|  Fwd Cumulative P-P CDV       |
%%   | BCCDV     |  401D|  24b integer|  Bkwd Cumulative P-P CDV      |
%%   | FCLR      |  401E|  8b integer |  Acceptable Fwd CLR           |
%%   | BCLR      |  401F|  8b integer |  Acceptable Bkwd CLR          |
%%   | EETD      |  4020|  16b integer|  End-to-end transit delay     |
%%   | Mediatx   |  4021|             |  AAL Type                     |
%%   | QosClass  |  4022|  Integer    |  0-4 Qos Class                |
%%   | AALtype   |  4023|  1 OCTET    |  AAL Type Reference           |
%%   |___________|______|_____________|_______________________________|
%% 
%% 
%% C.5.  Frame Relay
%% 
%%  ______________________________________________________________________
%% | PropertyID|  Tag |  Type            |  Value                        |
%% | DLCI      |  5001|  Unsigned Integer|  Data link connection id      |
%% | CID       |  5002|  Unsigned Integer|  sub-channel id.              |
%% | SID       |  5003|  Unsigned Integer|  silence insertion descriptor |
%% | Payload   |  5004|  Unsigned Integer|  Primary Payload Type         |
%% |___________|______|__________________|_______________________________|
%% 
%% 
%% C.6.  IP
%% 
%%     ________________________________________________________________
%%    | PropertyID|  Tag       |  Type        |  Value                |
%%    | IPv4      |  6001      |  32 BITS     |  Ipv4Address          |
%%    | IPv6      |  6002      |  128 BITS    |  IPv6 Address         |
%%    | Port      |  6003      |  Unsigned Int|  Port                 |
%%    | Porttype  |  6004      |  Enumerated  |  TCP(0),UDP(1),SCTP(2)|
%%    | UDP       |  6004      |  Boolean     |                       |
%%    |___________|____________|______________|_______________________|
%%    
%% BUGBUG: UDP 6004 should be 6005??
%% 
%% C.7.  ATM AAL2
%% 
%% _______________________________________________________________________________
%% |PropertyID|  Tag    |  Type         |  Value                                 |
%% |AESA      |  7001   |  20 OCTETS    |  AAL2 service endpoint address         |
%% |BIR       |  See C.3|  4 OCTETS     |  Served user generated reference       |
%% |ALC       |  7002   |  12 OCTETS    |  AAL2 link                             |
%% |SSCS      |  7003   |  8..14 OCTETS |  Service specific convergence sublayer |
%% |SUT       |  7004   |  1..254 octets|  Served user transport param           |
%% |TCI       |  7005   |  BOOLEAN      |  Test connection                       |
%% |Timer_CU  |  7006   |  32b integer  |  Timer-CU                              |
%% |MaxCPSSDU |  7007   |  8b integer   |  Max. Common Part Sublayer SDU         |
%% |SCLP      |  7008   |  Boolean      |  Set Cell Local PriorityLP bit         |
%% |EETR      |  7009   |  Boolean      |  End to End Timing Required            |
%% |CID       |  700A   |  8 bits       |  subchannel id                         |
%% |__________|_________|_______________|________________________________________|
%% 
%% 
%% C.8.  ATM AAL1
%% 
%% ________________________________________________________________________________
%% |PropertyID|  Tag          |  Type         |  Value                            |
%% |BIR       |  See Table C.3|  4 OCTETS     |  GIT(Generic Identifier Transport)|
%% |AAL1ST    |  8001         |  1 OCTET      |  AAL1 Subtype:                    |
%% |8002      |  1 OCTET      |  CBR Rate     |                                   |
%% |SCRI      |  8003         |  1 OCTET      |  Source Clock Frequency Recovery  |
%% |ECM       |  8004         |  1 OCTET      |  Error Correction Method          |
%% |SDTB      |  8005         |  16b integer  |  Structured Data Transfer Blcksize|
%% |PFCI      |  8006         |  8b integer   |  Partially filled cells identifier|
%% |EETR      |  See Table C.7|  See Table C.7|                                   |
%% |__________|_______________|_______________|___________________________________|
%% 
%% C.9.  Bearer Capabilities
%% ________________________________________________________________________
%% |PropertyID   |  Tag |  Type       |  Value                            |
%% |TMR          |  9001|  1 OCTET    |  Transmission Medium Requirement  |
%% |TMRSR        |  9002|  1 OCTET    |  Trans. Medium Requirement Subrate|
%% |Contcheck    |  9003|  BOOLEAN    |  Continuity Check                 |
%% |ITC          |  9004|  5 BITS     |  Information Transfer Capability  |
%% |TransMode    |  9005|  2 BITS     |  Transfer Mode                    |
%% |TransRate    |  9006|  5 BITS     |  Transfer Rate                    |
%% |MULT         |  9007|  7 BITS     |  Rate Multiplier                  |
%% |layer1prot   |  9008|  5 BITS     |  User Information Layer 1 Protocol|
%%					 Reference: ITU Recommendation Q.931 
%%					 Bits 5 4 3 2 1 
%%					 00001   CCITT standardized rate adaption V.110 and X.30. 
%%					 00010 - Recommendation G.711 u-law 
%%					 00011 - Recommendation G.711 A-law 
%%					 00100   Recommendation G.721 32 kbit/s 
%%					 ADPCM and Recommendation I.460. 
%%					 00101 - Recommendations H.221 and H.242 
%%					 00110   Recommendations H.223 and H.245 
%%					 00111   Non-ITU-T standardized rate adaption. 
%%					 01000   ITU-T standardized rate adaption V.120. 
%%					 01001   CCITT standardized rate adaption X.31 
%%					 HDLC flag stuffing. 
%%					 All other values are reserved.           
%% |Syncasync    |  9009|  BOOLEAN    |  Synchronous-Asynchronous         |
%% |Userrate     |  900B|  5 BITS     |  User Rate Reference              |
%% |INTRATE      |  900C|  2 BITS     |  Intermediate Rate                |
%% |Nictx        |  900D|  BOOLEAN    |  Tx Network Independent Clock     |
%% |Nicrx        |  900E|  BOOLEAN    |  Rx Network independent clock     |
%% |Flowconttx   |  900F|  BOOLEAN    |  Tx Flow Control                  |
%% |Flowcontrx   |  9010|  BOOLEAN    |  Rx Flow control                  |
%% |Rateadapthdr |  9011|  BOOLEAN    |  Rate adapt header-no header      |
%% |Multiframe   |  9012|  BOOLEAN    |  Multiple frame estab.            |
%% |OPMODE       |  9013|  BOOLEAN    |  Mode of operation                |
%% |Llidnegot    |  9014|  BOOLEAN    |  Logical link identifier neg.     |
%% |Assign       |  9015|  BOOLEAN    |  Assignor-assignee                |
%% |Inbandneg    |  9016|  BOOLEAN    |  In-band or out-band negotiation  |
%% |Stopbits     |  9017|  2 BITS     |  Number of stop bits              |
%% |Databits     |  9018|  2 BIT      |  Number of data bits              |
%% |Parity       |  9019|  3 BIT      |  Parity information               |
%% |Duplexmode   |  901A|  BOOLEAN    |  Mode duplex                      |
%% |Modem        |  901B|  6 BIT      |  Modem Type                       |
%% |layer2prot   |  901C|  5 BIT      |  User info layer 2 protocol       |
%% |layer3prot   |  901D|  5 BIT      |  User info layer 3 protocol       |
%% |addlayer3prot|  901E|  OCTET      |  Addl User Info L3 protocol       |
%% |DialledN     |  901F|  30 OCTETS  |  Dialled Number                   |
%% |DiallingN    |  9020|  30 OCTETS  |  Dialling Number                  |
%% |ECHOCI       |  9021|  Enumeration|  Echo Control Information         |
%% |NCI          |  9022|  1 OCTET    |  Nature of Connection Indicators  |
%% |USI          |  9023|  OCTET      |  User Service Information         |
%% |             |      |  STRING     |  Reference: ITU Recommendation Q.763 Section 3.57 |
%% |_____________|______|_____________|___________________________________|
%% 
%% 
%% C.10.  AAL5 Properties
%% 
%%  ______________________________________________________________________
%% | PropertyID|  Tag    |  Type       |  Value                          |
%% | FMSDU     |  A001   |  32b integer|  Forward Maximum CPCS-SDU Size: |
%% | BMSDU     |  A002   |  2b integer |  Backwards Maximum CPCS-SDU Size|
%% | SSCS      |  See C.7|  See C.7    |  See table C.                   |
%% | SC        |  See C.4|  See C.4    |  See table C.4                  |
%% |___________|_________|_____________|_________________________________|
%% 
%% C.11.  SDP Equivalents
%% 
%%      ______________________________________________________________
%%     | PropertyID|  Tag |  Type  |  Value                          |
%%     | SDP_V     |  B001|  STRING|  Protocol Version               |
%%     | SDP_O     |  B002|  STRING|  Owner-creator and session ID   |
%%     | SDP_S     |  B003|  STRING|  Sesson name                    |
%%     | SDP_I     |  B004|  STRING|  Session identifier             |
%%     | SDP_U     |  B005|  STRING|  URI of descriptor              |
%%     | SDC_E     |  B006|  STRING|  email address                  |
%%     | SDP_P     |  B007|  STRING|  phone number                   |
%%     | SDP_C     |  B008|  STRING|  Connection information         |
%%     | SDP_B     |  B009|  STRING|  Bandwidth Information          |
%%     | SDP_Z     |  B00A|  STRING|  time zone adjustment           |
%%     | SDP_K     |  B00B|  STRING|  Encryption Key                 |
%%     | SDP_A     |  B00C|  STRING|  Zero or more session attributes|
%%     | SDP_T     |  B00D|  STRING|  Active Session Time            |
%%     | SDP_R     |  B00E|  STRING|  Zero or more repeat times      |
%%     | SDP_M     |  B00F|  STRING|  Media name and transport addr  |
%%     |           |      |        |  Reference: IETF RFC 2327       |
%%     |___________|______|________|_________________________________|
%% 
%% 
%% C.12.  H.245
%% 
%% ________________________________________________________________________
%% |OLC   |  C001|  octet string|  H.245 OpenLogicalChannel structure.    |
%% |OLCack|  C002|  octet string|   H.245 OpenLogicalChannelAck structure.|
%% |OLCcnf|  C003|  octet string|   OpenLogicalChannelConfirm structure.  |
%% |OLCrej|  C004|  octet string|   OpenLogicalChannelReject structure.   |
%% |CLC   |  C005|  octet string|   CloseLogicalChannel structure.        |
%% |CLCack|  C006|  octet string|   CloseLogicalChannelAck structure.     |
%% |______|______|______________|_________________________________________|
%% 
%%----------------------------------------------------------------------

capabilities_native() ->
    [
     {property, "Media"},
     {property, "TransMode"},
     {property, "NumChan"},
     {property, "SamplingRate"},
     {property, "RTPpayload"},
     {property, "IPv4"},
     {property, "IPv6"},
     {property, "Port"},
     {property, "Porttype"},
     {property, "UDP"},
     {property, "v"},
     {property, "o"},
     {property, "s"},
     {property, "i"},
     {property, "u"},
     {property, "e"},
     {property, "p"},
     {property, "c"},
     {property, "b"},
     {property, "z"},
     {property, "k"},
     {property, "a"},
     {property, "t"},
     {property, "r"},
     {property, "m"}
     %% {property, "SDP_V"},
     %% {property, "SDP_O"},
     %% {property, "SDP_S"},
     %% {property, "SDP_I"},
     %% {property, "SDP_U"},
     %% {property, "SDP_E"},
     %% {property, "SDP_P"},
     %% {property, "SDP_C"},
     %% {property, "SDP_B"},
     %% {property, "SDP_Z"},
     %% {property, "SDP_K"},
     %% {property, "SDP_A"},
     %% {property, "SDP_T"},
     %% {property, "SDP_R"},
     %% {property, "SDP_M"} 
    ].

encode_native(Scope, Item) ->
    case Scope of
        property ->
	    case Item of
		%% General
		"Media"        -> [16#10, 16#01];
		"TransMode"    -> [16#10, 16#02];
		"NumChan"      -> [16#10, 16#03];
		"SamplingRate" -> [16#10, 16#04];
		"Bitrate"      -> [16#10, 16#05];
		"Acodec"       -> [16#10, 16#06];
		"Samplepp"     -> [16#10, 16#07];
		"Silencesupp"  -> [16#10, 16#08];
		"Encrypttype"  -> [16#10, 16#09];
		"Encryptkey"   -> [16#10, 16#0a];
		"Echocanc"     -> [16#10, 16#0b];
		"Gain"         -> [16#10, 16#0c];
		"Jitterbuff"   -> [16#10, 16#0d];
		"PropDelay"    -> [16#10, 16#0e];
		"RTPpayload"   -> [16#10, 16#0f];

		%% IP
		"IPv4"         -> [16#60, 16#01];
		"IPv6"         -> [16#60, 16#02];
		"Port"         -> [16#60, 16#03];
		"Porttype"     -> [16#60, 16#04];
		"UDP"          -> [16#60, 16#05];

		%% %% SDP
		%% "SDP_V" -> [16#b0, 16#01];
		%% "SDP_O" -> [16#b0, 16#02];
		%% "SDP_S" -> [16#b0, 16#03];
		%% "SDP_I" -> [16#b0, 16#04];
		%% "SDP_U" -> [16#b0, 16#05];
		%% "SDP_E" -> [16#b0, 16#06];
		%% "SDP_P" -> [16#b0, 16#07];
		%% "SDP_C" -> [16#b0, 16#08];
		%% "SDP_B" -> [16#b0, 16#09];
		%% "SDP_Z" -> [16#b0, 16#0a];
		%% "SDP_K" -> [16#b0, 16#0b];
		%% "SDP_A" -> [16#b0, 16#0c];
		%% "SDP_T" -> [16#b0, 16#0d];
		%% "SDP_R" -> [16#b0, 16#0e];
		%% "SDP_M" -> [16#b0, 16#0f]

		%% SDP
		"v" -> [16#b0, 16#01];
		"o" -> [16#b0, 16#02];
		"s" -> [16#b0, 16#03];
		"i" -> [16#b0, 16#04];
		"u" -> [16#b0, 16#05];
		"e" -> [16#b0, 16#06];
		"p" -> [16#b0, 16#07];
		"c" -> [16#b0, 16#08];
		"b" -> [16#b0, 16#09];
		"z" -> [16#b0, 16#0a];
		"k" -> [16#b0, 16#0b];
		"a" -> [16#b0, 16#0c];
		"t" -> [16#b0, 16#0d];
		"r" -> [16#b0, 16#0e];
		"m" -> [16#b0, 16#0f] 
	    end
    end.

decode_native(Scope, [Type, Item]) ->
    case Scope of
        property ->
            case Type of
                16#10 ->
		    case Item of
			16#01 -> "Media";
			16#02 -> "TransMode";
			16#03 -> "NumChan";
			16#04 -> "SamplingRate";
			16#05 -> "Bitrate";
			16#06 -> "Acodec";
			16#07 -> "Samplepp";
			16#08 -> "Silencesupp";
			16#09 -> "Encrypttype";
			16#0a -> "Encryptkey";
			16#0b -> "Echocanc";
			16#0c -> "Gain";
			16#0d -> "Jitterbuff";
			16#0e -> "PropDelay";
			16#0f -> "RTPpayload"
		    end;
                16#60->
		    case Item of
			16#01 -> "IPv4";
			16#02 -> "IPv6";
			16#03 -> "Port";
			16#04 -> "Porttype";
			16#05 -> "UDP"
		    end;
                16#b0 ->
		    case Item of
			16#01 -> "v";
			16#02 -> "o";
			16#03 -> "s";
			16#04 -> "i";
			16#05 -> "u";
			16#06 -> "e";
			16#07 -> "p";
			16#08 -> "c";
			16#09 -> "b";
			16#0a -> "z";
			16#0b -> "k";
			16#0c -> "a";
			16#0d -> "t";
			16#0e -> "r";
			16#0f -> "m"

			%% 16#01 -> "SDP_V";
			%% 16#02 -> "SDP_O";
			%% 16#03 -> "SDP_S";
			%% 16#04 -> "SDP_I";
			%% 16#05 -> "SDP_U";
			%% 16#06 -> "SDP_E";
			%% 16#07 -> "SDP_P";
			%% 16#08 -> "SDP_C";
			%% 16#09 -> "SDP_B";
			%% 16#0a -> "SDP_Z";
			%% 16#0b -> "SDP_K";
			%% 16#0c -> "SDP_A";
			%% 16#0d -> "SDP_T";
			%% 16#0e -> "SDP_R";
			%% 16#0f -> "SDP_M"
		    end
            end
    end.

% i(F,A) ->
%     i(get(dbg),F,A).

% i(true,F,A) ->
%     S1 = io_lib:format("NRES2: " ++ F ++ "~n",A),
%     S2 = lists:flatten(S1),
%     io:format("~s",[S2]);
% i(_,_F,_A) ->
%     ok.
