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
%% Purpose: Test application config
%%----------------------------------------------------------------------

-module(megaco_sdp_test).

-compile(export_all).

-include("megaco_test_lib.hrl").
-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").
-include_lib("megaco/include/megaco_sdp.hrl").

t()     -> megaco_test_lib:t(?MODULE).
t(Case) -> megaco_test_lib:t({?MODULE, Case}).

%% Test server callbacks
init_per_testcase(Case, Config) ->
    megaco_test_lib:init_per_testcase(Case, Config).

fin_per_testcase(Case, Config) ->
    megaco_test_lib:fin_per_testcase(Case, Config).

%%----------------------------------------------------------------------
%% Top test case
%%----------------------------------------------------------------------

all(suite) ->
    [
     parse
    ].

parse(suite) ->
    [];
parse(Config) when list(Config) ->

    %%-------------------------------------
    %% Test data
    %%-------------------------------------
    PV = #'PropertyParm'{name="v", value=["0"]},
    PC = #'PropertyParm'{name="c", value=["IP IN4 123.123.123.123"]},
    PM1 = #'PropertyParm'{name="m", value=["audio 2000 RTP/AVP 0"]},
    PM2 = #'PropertyParm'{name="m", value=["audio 2000/2 RTP/AVP 96 97 98"]},
    PO = #'PropertyParm'{name="o", value=["kalle 1212 2 34 3434 565656"]},
    PA1 = #'PropertyParm'{name="a", value=["rtpmap:0 xxx/120"]},
    PA2 = #'PropertyParm'{name="a", value=["rtpmap:0 xxx/120/1/2/3"]},
    PA3 = #'PropertyParm'{name="a", value=["ptime:12"]},
    PA4 = #'PropertyParm'{name="a", value=["quality:10"]},
    PA5 = #'PropertyParm'{name="a", value=["recvonly"]},
    PB = #'PropertyParm'{name="b", value=["2:512"]},
    PT = #'PropertyParm'{name="t", value=["1200 1300"]},
    PR = #'PropertyParm'{name="r", value=["10 100 2 4 6"]},
    PZ = #'PropertyParm'{name="z", value=["12121212 -1h 34343434 0"]},
    PK = #'PropertyParm'{name="k", value=["method:key"]},
    PS = #'PropertyParm'{name="s", value=["new session"]},
    PI = #'PropertyParm'{name="i", value=["0"]},
    PU = #'PropertyParm'{name="u", value=["0"]},
    PE = #'PropertyParm'{name="e", value=["kalle@company.se"]},
    PP = #'PropertyParm'{name="p", value=["+4687273000"]},
    PNR = #'PropertyParm'{name="not_recognized", value=["whatever"]},


    PG1 = [],
    PG2 = [PV, PC, PM1, PA1, PA4],
    PG3 = [PV, PO, PA2],

    PGL1 = [],
    PGL2 = asn1_NOVALUE,
    PGL3 = [PG2],
    PGL4 = [PG2, PG3],

    PLRDesc1 = #'LocalRemoteDescriptor'{propGrps = PGL1},
    PLRDesc2 = #'LocalRemoteDescriptor'{propGrps = PGL2},
    PLRDesc3 = #'LocalRemoteDescriptor'{propGrps = PGL3},
    PLRDesc4 = #'LocalRemoteDescriptor'{propGrps = PGL4},


    SV = #sdp_v{version="0"},
    SC = #sdp_c{network_type = "IP",
		address_type = "IN4",
		connection_addr = "123.123.123.123"
	       },
    SM1 = #sdp_m{media = "audio",
		port = 2000,
		num_ports = undefined,
		transport = "RTP/AVP",
		fmt_list = ["0"]
	       },
    SM2 = #sdp_m{media = "audio",
		port = 2000,
		num_ports = 2,
		transport = "RTP/AVP",
		fmt_list = ["96", "97", "98"]
	       },
    SO = #sdp_o{user_name = "kalle",
		   session_id = "1212",
		   version = "2",
		   network_type = "34",
		   address_type = "3434",
		   address = "565656"
	       },
    SA1 = #sdp_a_rtpmap{payload_type = 0, 
			  encoding_name = "xxx",
			  clock_rate = 120,
			  encoding_parms = []
		      },
    SA2 = #sdp_a_rtpmap{payload_type = 0, 
			  encoding_name = "xxx",
			  clock_rate = 120,
			  encoding_parms = ["1", "2", "3"]
		      },
    SA3 = #sdp_a_ptime{packet_time = 12},
    SA4 = #sdp_a{attribute = "quality", value = "10"},
    SA5 = #sdp_a{attribute = "recvonly"},
    SB = #sdp_b{modifier="2",
		bandwidth=512
	       },
    ST = #sdp_t{start=1200, 
		stop=1300},
    SR = #sdp_r{repeat_interval="10",
		active_duration="100", 
		list_of_offsets=["2", "4", "6"]
	       },
    SZ = #sdp_z{list_of_adjustments=["12121212", "-1h", "34343434", "0"]},
    SK = #sdp_k{method="method", 
		encryption_key="key"
	       },
    SS = #sdp_s{name="new session"},
    SI = #sdp_i{session_descriptor="0"},
    SU = #sdp_u{uri="0"},
    SE = #sdp_e{email="kalle@company.se"},
    SP = #sdp_p{phone_number="+4687273000"},
    SNR = #'PropertyParm'{name="not_recognized", value=["whatever"]},

    SG1 = [],
    SG2 = [SV, SC, SM1, SA1, SA4],
    SG3 = [SV, SO, SA2],
    
    SGL1 = [],
    SGL2 = asn1_NOVALUE,
    SGL3 = [SG2],
    SGL4 = [SG2, SG3],
    
    SLRDesc1 = #'LocalRemoteDescriptor'{propGrps = SGL1},
    SLRDesc2 = #'LocalRemoteDescriptor'{propGrps = SGL2},
    SLRDesc3 = #'LocalRemoteDescriptor'{propGrps = SGL3},
    SLRDesc4 = #'LocalRemoteDescriptor'{propGrps = SGL4},

    %%-------------------------------------
    %% Test starting
    %%-------------------------------------
    
    %%-------------------------------------
    %% Parse tests
    %%-------------------------------------
    
    ?VERIFY(SV, megaco_sdp:parse_propertyparm(PV)),
    ?VERIFY(SC, megaco_sdp:parse_propertyparm(PC)),
    ?VERIFY(SM1, megaco_sdp:parse_propertyparm(PM1)),
    ?VERIFY(SM2, megaco_sdp:parse_propertyparm(PM2)),
    ?VERIFY(SO, megaco_sdp:parse_propertyparm(PO)),
    ?VERIFY(SA1, megaco_sdp:parse_propertyparm(PA1)),
    ?VERIFY(SA2, megaco_sdp:parse_propertyparm(PA2)),
    ?VERIFY(SA3, megaco_sdp:parse_propertyparm(PA3)),
    ?VERIFY(SA4, megaco_sdp:parse_propertyparm(PA4)),
    ?VERIFY(SA5, megaco_sdp:parse_propertyparm(PA5)),
    ?VERIFY(SB, megaco_sdp:parse_propertyparm(PB)),
    ?VERIFY(ST, megaco_sdp:parse_propertyparm(PT)),
    ?VERIFY(SR, megaco_sdp:parse_propertyparm(PR)),
    ?VERIFY(SZ, megaco_sdp:parse_propertyparm(PZ)),
    ?VERIFY(SK, megaco_sdp:parse_propertyparm(PK)),
    ?VERIFY(SS, megaco_sdp:parse_propertyparm(PS)),
    ?VERIFY(SI, megaco_sdp:parse_propertyparm(PI)),
    ?VERIFY(SU, megaco_sdp:parse_propertyparm(PU)),
    ?VERIFY(SE, megaco_sdp:parse_propertyparm(PE)),
    ?VERIFY(SP, megaco_sdp:parse_propertyparm(PP)),
    ?VERIFY(SNR, megaco_sdp:parse_propertyparm(PNR)),


    ?VERIFY(SG1, megaco_sdp:parse_propertygroup(PG1)),
    ?VERIFY(SG2, megaco_sdp:parse_propertygroup(PG2)),
    ?VERIFY(SG3, megaco_sdp:parse_propertygroup(PG3)),
   
    ?VERIFY(SGL1, megaco_sdp:parse_propertygroup_list(PGL1)),
    ?VERIFY(SGL2, megaco_sdp:parse_propertygroup_list(PGL2)),
    ?VERIFY(SGL3, megaco_sdp:parse_propertygroup_list(PGL3)),
    ?VERIFY(SGL4, megaco_sdp:parse_propertygroup_list(PGL4)),
    
    ?VERIFY(SLRDesc1, megaco_sdp:parse_LRDesc(PLRDesc1)),
    ?VERIFY(SLRDesc2, megaco_sdp:parse_LRDesc(PLRDesc2)),
    ?VERIFY(SLRDesc3, megaco_sdp:parse_LRDesc(PLRDesc3)),
    ?VERIFY(SLRDesc4, megaco_sdp:parse_LRDesc(PLRDesc4)),

    %%-------------------------------------
    %% Generate tests
    %%-------------------------------------
    
    ?VERIFY(PV, megaco_sdp:gen_propertyparm(SV)),
    ?VERIFY(PC, megaco_sdp:gen_propertyparm(SC)),
    ?VERIFY(PM1, megaco_sdp:gen_propertyparm(SM1)),
    ?VERIFY(PM2, megaco_sdp:gen_propertyparm(SM2)),
    ?VERIFY(PO, megaco_sdp:gen_propertyparm(SO)),
    ?VERIFY(PA1, megaco_sdp:gen_propertyparm(SA1)),
    ?VERIFY(PA2, megaco_sdp:gen_propertyparm(SA2)),
    ?VERIFY(PA3, megaco_sdp:gen_propertyparm(SA3)),
    ?VERIFY(PA4, megaco_sdp:gen_propertyparm(SA4)),
    ?VERIFY(PA5, megaco_sdp:gen_propertyparm(SA5)),
    ?VERIFY(PB, megaco_sdp:gen_propertyparm(SB)),
    ?VERIFY(PT, megaco_sdp:gen_propertyparm(ST)),
    ?VERIFY(PR, megaco_sdp:gen_propertyparm(SR)),
    ?VERIFY(PZ, megaco_sdp:gen_propertyparm(SZ)),
    ?VERIFY(PK, megaco_sdp:gen_propertyparm(SK)),
    ?VERIFY(PS, megaco_sdp:gen_propertyparm(SS)),
    ?VERIFY(PI, megaco_sdp:gen_propertyparm(SI)),
    ?VERIFY(PU, megaco_sdp:gen_propertyparm(SU)),
    ?VERIFY(PE, megaco_sdp:gen_propertyparm(SE)),
    ?VERIFY(PP, megaco_sdp:gen_propertyparm(SP)),
    ?VERIFY(PNR, megaco_sdp:gen_propertyparm(SNR)),


    ?VERIFY(PG1, megaco_sdp:gen_propertygroup(SG1)),
    ?VERIFY(PG2, megaco_sdp:gen_propertygroup(SG2)),
    ?VERIFY(PG3, megaco_sdp:gen_propertygroup(SG3)),
   
    ?VERIFY(PGL1, megaco_sdp:gen_propertygroup_list(SGL1)),
    ?VERIFY(PGL2, megaco_sdp:gen_propertygroup_list(SGL2)),
    ?VERIFY(PGL3, megaco_sdp:gen_propertygroup_list(SGL3)),
    ?VERIFY(PGL4, megaco_sdp:gen_propertygroup_list(SGL4)),
    
    ?VERIFY(PLRDesc1, megaco_sdp:gen_LRDesc(SLRDesc1)),
    ?VERIFY(PLRDesc2, megaco_sdp:gen_LRDesc(SLRDesc2)),
    ?VERIFY(PLRDesc3, megaco_sdp:gen_LRDesc(SLRDesc3)),
    ?VERIFY(PLRDesc4, megaco_sdp:gen_LRDesc(SLRDesc4)),

    ok.
    
