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
-module(httpd_test).

-export([all/1, ssl/1, ip_comm/1, 
	 init_per_testcase/2, fin_per_testcase/2]).

-export([init_ip_comm/1, stop_ip_comm/1, 
	 ip_mod/1,  ip_load/1,  ip_block/1,  ip_restart/1,  ip_misc/1]).

-export([ip_mod_alias/1, ip_mod_actions/1, ip_mod_security/1, 
	 ip_mod_auth/1, ip_mod_auth_api/1, ip_mod_auth_mnesia_api/1,
	 ip_mod_htaccess/1, 
	 ip_mod_cgi/1, ip_mod_esi/1, ip_mod_get/1, ip_mod_head/1,
	 ip_all_modules/1,

	 ip_heavy_load/1, ip_medium_load/1, ip_light_load/1,

	 ip_dos_hostname/1,

	 ip_simple_block1/1, ip_simple_block2/1,
	 ip_simple_block3/1, ip_simple_block4/1, ip_simple_block5/1,
	 ip_block_when_active/1,
	 ip_block_when_active_to1/1, ip_block_when_active_to2/1,
	 ip_nd_block_when_active1/1, ip_nd_block_when_active2/1,
	 ip_nd_block_blocker_crash1/1, ip_nd_block_blocker_crash2/1,

	 ip_simple_restart1/1,
	 ip_simple_restart2/1,
	 ip_simple_restart3/1,

	 ip_time_test/1]).

-export([init_ssl/1, stop_ssl/1, 
	 ssl_mod/1,  ssl_load/1,  ssl_block/1,  ssl_restart/1,  ssl_misc/1]).

-export([ssl_mod_alias/1, ssl_mod_actions/1, ssl_mod_security/1,
	 ssl_mod_htaccess/1, 
	 ssl_mod_auth/1, ssl_mod_auth_api/1, ssl_mod_auth_mnesia_api/1,
	 ssl_mod_cgi/1, ssl_mod_esi/1, ssl_mod_get/1, ssl_mod_head/1,
	 ssl_all_modules/1,

	 ssl_heavy_load/1, ssl_medium_load/1, ssl_light_load/1,

	 ssl_dos_hostname/1,

	 ssl_simple_block1/1, ssl_simple_block2/1,
	 ssl_simple_block3/1, ssl_simple_block4/1, ssl_simple_block5/1,
	 ssl_block_when_active/1,
	 ssl_block_when_active_to1/1, ssl_block_when_active_to2/1,
	 ssl_nd_block_when_active1/1, ssl_nd_block_when_active2/1,
	 ssl_nd_block_blocker_crash1/1, ssl_nd_block_blocker_crash2/1,

	 ssl_simple_restart1/1,
	 ssl_simple_restart2/1,
	 ssl_simple_restart3/1,

	 ssl_time_test/1]).


-export([http_1_1/1, 
	 init_http_test/1, stop_http_test/1,
	 host/1,
	 chunked/1,
	 expect/1,
	 range/1,
	 if_test/1,
	 http_trace/1,
	 http1_1_head/1,
	 mod_cgi_chunked_encoding_test/1]).

-export([setup_mnesia/0, cleanup_mnesia/0, load_test_client/8]).
-export([do_long_poll/6,do_block_nd_server/5,do_block_server/4]).

-export([event/4]).
-export([ssl_password_cb/0]).


-include("inets_test_lib.hrl").
-include("httpd_test_lib.hrl").
-include_lib("kernel/include/file.hrl").


%% Seconds before successful auths timeout.
-define(auth_timeout,5).

%% Minutes before failed auths timeout. (NOT TESTED!)
-define(fail_expire_time,1). 

%% Number of polling processes
-define(heavy_load_proc_cnt,20).

%% Time between polls
-define(heavy_load_poll_timeout,100).


-define(HTTPD_STATUS(P,S),?LOG(P ++ status_to_string(S),[])).


%% Some verbosity macros

-ifdef(httpd_manager_verbosity).
-define(MAN_VERBOSITY,?httpd_manager_verbosity).
-else.
-define(MAN_VERBOSITY,silence).
-endif.

-ifdef(httpd_request_handler_verbosity).
-define(REQ_VERBOSITY,?httpd_request_handler_verbosity).
-else.
-define(REQ_VERBOSITY,silence).
-endif.

-ifdef(httpd_acceptor_verbosity).
-define(ACC_VERBOSITY,?httpd_acceptor_verbosity).
-else.
-define(ACC_VERBOSITY,silence).
-endif.

-ifdef(httpd_auth_verbosity).
-define(AUTH_VERBOSITY,?httpd_auth_verbosity).
-else.
-define(AUTH_VERBOSITY,silence).
-endif.

-ifdef(httpd_security_verbosity).
-define(SEC_VERBOSITY,?httpd_security_verbosity).
-else.
-define(SEC_VERBOSITY,silence).
-endif.


%%
%% -----
%%

all(doc) ->
    ["Test suites for the HTTP server (HTTPD).",
     "There are three different test sub-suites."];

all(suite) ->
    [http_1_1, ip_comm, ssl].


%%
%% -----
%%

init_per_testcase(Case, Conf0) when list(Conf0) ->
    ?LOG("init_per_testcase -> entry with"
	 "~n   Case:  ~p"
	 "~n   Conf0: ~p", [Case, Conf0]),
    Conf1 = wd_start(Conf0, ?MINS(3)),
    Conf2 = ?UPDATE(data_dir,?inets_data_dir, Conf1),
    Conf3 = ?UPDATE(priv_dir,?inets_priv_dir, Conf2),
    ?LOG("init_per_testcase -> "
	 "~n   Conf3: ~p", [Conf3]),
    Conf3.

fin_per_testcase(Case, Config) when list(Config) ->
    ?LOG("fin_per_testcase -> entry with"
	 "~n   Case:   ~p"
	 "~n   Config: ~p", [Case, Config]),
    Port = ?CONFIG(port, Config),
    (catch httpd:stop(Port)),
    wd_stop(Config),
    ?SLEEP(1000),
    ?LOG("fin_per_testcase -> done", []),
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%% Test case for testing of http conformance                        %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
http_1_1(doc) ->
    ["Testing the conformance to the http/1.1 protocol"];
http_1_1(suite) ->
    Cases = 
	[host,
	 chunked,
	 expect,
	 range,
	 if_test,
	 http_trace,
	 http1_1_head,
	 mod_cgi_chunked_encoding_test
	],
    ?EXPANDABLE(init_http_test, Cases, stop_http_test).

init_http_test(suite) -> [];
init_http_test(doc) -> [];
init_http_test(Config) when list(Config) ->
    ?DEBUG("http test init", []),
    init_suite(Config, ip_comm, 8099).

stop_http_test(suite) -> [];
stop_http_test(doc) -> [];
stop_http_test(Config) when list(Config) ->
    ?DEBUG("http test stop", []),
    stop_suite(Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test case: ssl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ssl(suite) ->
    Cases = 
	[
	 ssl_mod,
	 ssl_load,
	 ssl_misc,
	 ssl_block,
	 ssl_restart
	],
    ?EXPANDABLE(init_ssl, Cases, stop_ssl).


ssl_mod(suite) ->
    Cases = 
	[
	 ssl_mod_alias,
	 ssl_mod_actions,
	 ssl_mod_security,
	 ssl_mod_htaccess,
	 ssl_mod_auth,
	 ssl_mod_auth_api,
	 ssl_mod_auth_mnesia_api,
	 ssl_mod_cgi,
	 ssl_mod_esi,
	 ssl_mod_get,
	 ssl_mod_head,
	 ssl_all_modules
	],
    ?EXPANDABLE(init_ssl, Cases, stop_ssl).


ssl_load(suite) ->
    Cases = 
	[
	 ssl_light_load,
	 ssl_medium_load,
	 ssl_heavy_load
	],
    ?EXPANDABLE(init_ssl, Cases, stop_ssl).


ssl_misc(suite) ->
    Cases = 
	[
	 ssl_dos_hostname,
	 ssl_time_test
	],
    ?EXPANDABLE(init_ssl, Cases, stop_ssl).


ssl_block(suite) ->
    Cases = 
	[
	 ssl_simple_block1,
	 ssl_simple_block2,
	 ssl_simple_block3,
	 ssl_simple_block4,
	 ssl_simple_block5,
	 ssl_block_when_active,
	 ssl_block_when_active_to1,
	 ssl_block_when_active_to2,
	 ssl_nd_block_when_active1,
	 ssl_nd_block_when_active2,
	 ssl_nd_block_blocker_crash1,
	 ssl_nd_block_blocker_crash2
	],
    ?EXPANDABLE(init_ssl, Cases, stop_ssl).


ssl_restart(suite) ->
    Cases = 
	[
	 ssl_simple_restart1,
	 ssl_simple_restart2,
	 ssl_simple_restart3
	],
    ?EXPANDABLE(init_ssl, Cases, stop_ssl).


init_ssl(suite) -> [];
init_ssl(doc) -> [];
init_ssl(Config) when list(Config) ->
    ?DEBUG("ssl test init", []),
    set_ssl_portprogram_dir(Config),
    init_suite(Config, ssl, 8099).

stop_ssl(suite) -> [];
stop_ssl(doc) -> [];
stop_ssl(Config) when list(Config) ->
    ?DEBUG("ssl test stop", []),
    stop_suite(Config).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test case: ip_comm
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ip_comm(suite) ->
    Cases = 
	[
	 ip_mod,
	 ip_load,
	 ip_block,
	 ip_restart,
	 ip_misc
	],
    ?EXPANDABLE(init_ip_comm, Cases, stop_ip_comm).



ip_mod(suite) ->
    Cases = 
	[
	 ip_mod_alias,
	 ip_mod_actions,
	 ip_mod_security,
	 ip_mod_auth,
	 ip_mod_htaccess,
	 ip_mod_auth_api,
	 ip_mod_auth_mnesia_api,
	 ip_mod_cgi,
	 ip_mod_esi,
	 ip_mod_get,
	 ip_mod_head,
	 ip_all_modules
	],
    ?EXPANDABLE(init_ip_comm, Cases, stop_ip_comm).


ip_load(suite) ->
    Cases = 
	[
	 ip_light_load,
	 ip_medium_load,
	 ip_heavy_load
	],
    ?EXPANDABLE(init_ip_comm, Cases, stop_ip_comm).


ip_block(suite) ->
    Cases = 
	[
	 ip_simple_block1,
	 ip_simple_block2,
	 ip_simple_block3,
	 ip_simple_block4,
	 ip_simple_block5,
	 ip_block_when_active,
	 ip_block_when_active_to1,
	 ip_block_when_active_to2,
	 ip_nd_block_when_active1,
	 ip_nd_block_when_active2,
	 ip_nd_block_blocker_crash1,
	 ip_nd_block_blocker_crash2
	],
    ?EXPANDABLE(init_ip_comm, Cases, stop_ip_comm).


ip_restart(suite) ->
    Cases = 
	[
	 ip_simple_restart1,
	 ip_simple_restart2,
	 ip_simple_restart3
	],
    ?EXPANDABLE(init_ip_comm, Cases, stop_ip_comm).


ip_misc(suite) ->
    Cases = 
	[
	 ip_dos_hostname,
	 ip_time_test
	],
    ?EXPANDABLE(init_ip_comm, Cases, stop_ip_comm).


init_ip_comm(suite) -> [];
init_ip_comm(doc) -> [];
init_ip_comm(Config) when list(Config) ->
    ?DEBUG("ip test init", []),
    init_suite(Config, ip_comm, 8099).

stop_ip_comm(suite) -> [];
stop_ip_comm(doc) -> [];
stop_ip_comm(Config) when list(Config) ->
    ?DEBUG("ip test stop", []),
    stop_suite(Config).


ip_time_test(suite) -> [];
ip_time_test(doc) ->   ["Request time test"];
ip_time_test(Config) when list(Config) ->
    ?line {M,H,P,_N} = start_all([{verbosity, []}|Config], false),
    time_test(M,H,P),
    ok = stop_all(Config).

ip_simple_restart3(suite) -> [];
ip_simple_restart3(doc) ->   ["Simple web server (disturbing) restart"];
ip_simple_restart3(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    simple_restart3(M,H,P,N),
    ok = stop_all(Config).

ip_simple_restart2(suite) -> [];
ip_simple_restart2(doc) ->   ["Simple web server (non-disturbing) restart"];
ip_simple_restart2(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    simple_restart2(M,H,P,N),
    ok = stop_all(Config).

ip_simple_restart1(suite) -> [];
ip_simple_restart1(doc) ->   ["Simple web server restart, without a preceding block (erroneous)"];
ip_simple_restart1(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    simple_restart1(M,H,P,N),
    ok = stop_all(Config).

ip_nd_block_blocker_crash2(suite) -> [];
ip_nd_block_blocker_crash2(doc) ->   ["The process performing a disturbing block (with timeout) of a web-server crashes"];
ip_nd_block_blocker_crash2(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    nd_block_blocker_crash2(M,H,P,N),
    ok = stop_all(Config).

ip_nd_block_blocker_crash1(suite) -> [];
ip_nd_block_blocker_crash1(doc) ->   ["The process performing a non-disturbing block of a web-server crashes"];
ip_nd_block_blocker_crash1(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    nd_block_blocker_crash1(M,H,P,N),
    ok = stop_all(Config).

ip_nd_block_when_active2(suite) -> [];
ip_nd_block_when_active2(doc) ->   ["Perform a non-disturbing block of a web-server that failes (timeout) when it's active"];
ip_nd_block_when_active2(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    nd_block_when_active2(M,H,P,N),
    ok = stop_all(Config).

ip_nd_block_when_active1(suite) -> [];
ip_nd_block_when_active1(doc) ->   ["Perform a non-disturbing block of a web-server when it's active"];
ip_nd_block_when_active1(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    nd_block_when_active1(M,H,P,N),
    ok = stop_all(Config).

ip_block_when_active_to2(suite) -> [];
ip_block_when_active_to2(doc) ->   ["Perform a disturbing block with timeout of a web-server when it's active (should time out and force the block)"];
ip_block_when_active_to2(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    block_when_active_to2(M,H,P,N),
    ok = stop_all(Config).

ip_block_when_active_to1(suite) -> [];
ip_block_when_active_to1(doc) ->   ["Perform a disturbing block with timeout of a web-server when it's active (does not time out)"];
ip_block_when_active_to1(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    block_when_active_to1(M,H,P,N),
    ok = stop_all(Config).

ip_block_when_active(suite) -> [];
ip_block_when_active(doc) ->   ["Perform a disturbing block of a web-server when it's active"];
ip_block_when_active(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    block_when_active(M,H,P,N),
    ok = stop_all(Config).

ip_simple_block5(suite) -> [];
ip_simple_block5(doc) ->   ["Simple server block(non-disturbing,Timeout)/unblock test case"];
ip_simple_block5(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    simple_block5(M,H,P,N),
    ok = stop_all(Config).

ip_simple_block4(suite) -> [];
ip_simple_block4(doc) ->   ["Simple server block(non-disturbing,Timeout)/unblock test case"];
ip_simple_block4(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    simple_block4(M,H,P,N),
    ok = stop_all(Config).

ip_simple_block3(suite) -> [];
ip_simple_block3(doc) ->   ["Simple server block(non-disturbing)/unblock test case"];
ip_simple_block3(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    simple_block3(M,H,P,N),
    ok = stop_all(Config).

ip_simple_block2(suite) -> [];
ip_simple_block2(doc) ->   ["Second simple server block/unblock test case"];
ip_simple_block2(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    simple_block2(M,H,P,N),
    ok = stop_all(Config).

ip_simple_block1(suite) -> [];
ip_simple_block1(doc) ->   ["First simple server block/unblock test case"];
ip_simple_block1(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    simple_block1(M,H,P,N),
    ok = stop_all(Config).

ip_dos_hostname(suite) -> [];
ip_dos_hostname(doc) ->   ["Hostname Denial of Service test case"];
ip_dos_hostname(Config) when list(Config) ->
    C1 = [{max_header_size,256},{max_header_action,reply414}|Config],
    ?line {M,H,P,N} = start_all(C1, false),
    dos_hostname(M,H,P,N),
    ok = stop_all(Config).

ip_heavy_load(suite) -> [];
ip_heavy_load(doc) ->   ["heavy load test"];
ip_heavy_load(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    heavy_load(M,H,P,N),
    ok = stop_all(Config).

ip_medium_load(suite) -> [];
ip_medium_load(doc) ->   ["Medium load test"];
ip_medium_load(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    medium_load(M,H,P,N),
    ok = stop_all(Config).

ip_light_load(suite) -> [];
ip_light_load(doc) ->   ["Light load test"];
ip_light_load(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    light_load(M,H,P,N),
    ok = stop_all(Config).

ip_all_modules(suite) -> [];
ip_all_modules(doc) ->   ["All modules test"];
ip_all_modules(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, true),
    all_modules(M,H,P,N),
    ok = stop_all(Config).

ip_mod_alias(suite) -> [];
ip_mod_alias(doc) -> ["Module test: mod_alias"];
ip_mod_alias(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    mod_alias(M, H, P, N),
    ?line ok = stop_all(Config).

ip_mod_actions(suite) -> [];
ip_mod_actions(doc) -> ["Module test: mod_actions"];
ip_mod_actions(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    mod_actions(M, H, P, N),
    ok = stop_all(Config).

ip_mod_security(suite) -> [];
ip_mod_security(doc) ->   ["Module test: mod_security"];
ip_mod_security(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    mod_security(Config, M, H, P, N),
    ok = stop_all(Config).

ip_mod_htaccess(suite) -> [];
ip_mod_htaccess(doc) -> ["Module test: mod_htaccess"];

%% Controls the mod_htaccess function 
%% First it creates the files and directories nesseccary to do the testing
%% then it controls that the mod_htaccess functions correctly and after that 
%% it will remove the files that it has created
ip_mod_htaccess(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false,[mod_htaccess]),
    mkTestData(filename:join([?CONFIG(data_dir,Config),"server_root/htdocs"]),
	       ?CONFIG(address,Config)),
    mod_htaccess(M, H, P, N),

%%     delTestData(filename:join([?CONFIG(data_dir, Config),
%% 			       "server_root/htdocs"])),

    ok = stop_all(Config).

ip_mod_auth(suite) -> [];
ip_mod_auth(doc) -> ["Module test: mod_auth"];
ip_mod_auth(Config) when list(Config) ->
    ?LOG("ip_mod_auth -> entry", []),
    ?line {M,H,P,N} = start_all(Config, false),
    mod_auth(M, H, P, N),
    ok = stop_all(Config),
    ?LOG("ip_mod_auth -> done", []),
    ok.

ip_mod_auth_api(suite) -> [];
ip_mod_auth_api(doc) -> ["Module test: mod_auth_api"];
ip_mod_auth_api(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, true),
    mod_auth_api(Config, M, H, P, N),
    ok = stop_all(Config).

ip_mod_auth_mnesia_api(suite) -> [];
ip_mod_auth_mnesia_api(doc) -> ["Module test: mod_auth_mnesia_api"];
ip_mod_auth_mnesia_api(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, true),
    mod_auth_mnesia_api(Config, M, H, P, N),
    ok = stop_all(Config).

ip_mod_cgi(suite) -> [];
ip_mod_cgi(doc) -> ["Module test: mod_cgi"];
ip_mod_cgi(Config) when list(Config) ->
    VConf = [{manager_verbosity,         trace}, 
             {request_handler_verbosity, trace}],
    ?line {M,H,P,N} = start_all([{verbosity, VConf}|Config], false),
    mod_cgi(M, H, P, N),
    ok = stop_all(Config).

ip_mod_esi(suite) -> [];
ip_mod_esi(doc) -> ["Module test: mod_esi"];
ip_mod_esi(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    mod_esi(M, H, P, N),
    ok = stop_all(Config).

ip_mod_get(suite) -> [];
ip_mod_get(doc) -> ["Module test: mod_get"];
ip_mod_get(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    mod_get(M, H, P, N),
    ok = stop_all(Config).

ip_mod_head(suite) -> [];
ip_mod_head(doc) -> ["Module test: mod_head"];
ip_mod_head(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    mod_head(M, H, P, N),
    ok = stop_all(Config).


ssl_time_test(suite) -> [];
ssl_time_test(doc) ->   ["Request time test"];
ssl_time_test(Config) when list(Config) ->
    ?line {M,H,P,_N} = start_all([{verbosity, []}] ++ Config, false),
    time_test(M,H,P),
    ok = stop_all(Config).

ssl_simple_restart3(suite) -> [];
ssl_simple_restart3(doc) ->   ["Simple web server (disturbing) restart"];
ssl_simple_restart3(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    simple_restart3(M,H,P,N),
    ok = stop_all(Config).

ssl_simple_restart2(suite) -> [];
ssl_simple_restart2(doc) ->   ["Simple web server (non-disturbing) restart"];
ssl_simple_restart2(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    simple_restart2(M,H,P,N),
    ok = stop_all(Config).

ssl_simple_restart1(suite) -> [];
ssl_simple_restart1(doc) ->   ["Simple web server restart, without a preceding block (erroneous)"];
ssl_simple_restart1(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    simple_restart1(M,H,P,N),
    ok = stop_all(Config).

ssl_nd_block_blocker_crash2(suite) -> [];
ssl_nd_block_blocker_crash2(doc) ->   ["The process performing a disturbing block (with timeout) of a web-server crashes"];
ssl_nd_block_blocker_crash2(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    nd_block_blocker_crash2(M,H,P,N),
    ok = stop_all(Config).

ssl_nd_block_blocker_crash1(suite) -> [];
ssl_nd_block_blocker_crash1(doc) ->   ["The process performing a non-disturbing block of a web-server crashes"];
ssl_nd_block_blocker_crash1(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    nd_block_blocker_crash1(M,H,P,N),
    ok = stop_all(Config).

ssl_nd_block_when_active2(suite) -> [];
ssl_nd_block_when_active2(doc) ->   ["Perform a non-disturbing block of a web-server that failes (timeout) when it's active"];
ssl_nd_block_when_active2(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    nd_block_when_active2(M,H,P,N),
    ok = stop_all(Config).

ssl_nd_block_when_active1(suite) -> [];
ssl_nd_block_when_active1(doc) ->   ["Perform a non-disturbing block of a web-server when it's active"];
ssl_nd_block_when_active1(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    nd_block_when_active1(M,H,P,N),
    ok = stop_all(Config).

ssl_block_when_active_to2(suite) -> [];
ssl_block_when_active_to2(doc) ->   ["Perform a disturbing block with timeout of a web-server when it's active (should time out and force the block)"];
ssl_block_when_active_to2(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    block_when_active_to2(M,H,P,N),
    ok = stop_all(Config).

ssl_block_when_active_to1(suite) -> [];
ssl_block_when_active_to1(doc) ->   ["Perform a disturbing block with timeout of a web-server when it's active (does not time out)"];
ssl_block_when_active_to1(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    block_when_active_to1(M,H,P,N),
    ok = stop_all(Config).

ssl_block_when_active(suite) -> [];
ssl_block_when_active(doc) ->   ["Perform a disturbing block of a web-server when it's active"];
ssl_block_when_active(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    block_when_active(M,H,P,N),
    ok = stop_all(Config).

ssl_simple_block5(suite) -> [];
ssl_simple_block5(doc) ->   ["Simple server block(non-disturbing,Timeout)/unblock test case"];
ssl_simple_block5(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    simple_block5(M,H,P,N),
    ok = stop_all(Config).

ssl_simple_block4(suite) -> [];
ssl_simple_block4(doc) ->   ["Simple server block(non-disturbing,Timeout)/unblock test case"];
ssl_simple_block4(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    simple_block4(M,H,P,N),
    ok = stop_all(Config).

ssl_simple_block3(suite) -> [];
ssl_simple_block3(doc) ->   ["Simple server block(non-disturbing)/unblock test case"];
ssl_simple_block3(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    simple_block3(M,H,P,N),
    ok = stop_all(Config).

ssl_simple_block2(suite) -> [];
ssl_simple_block2(doc) ->   ["Second simple server block/unblock test case"];
ssl_simple_block2(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    simple_block2(M,H,P,N),
    ok = stop_all(Config).

ssl_simple_block1(suite) -> [];
ssl_simple_block1(doc) ->   ["First simple server block/unblock test case"];
ssl_simple_block1(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all([{poll_send_delay, 5000}] ++ Config, false),
    simple_block1(M,H,P,N),
    ok = stop_all(Config).

ssl_dos_hostname(suite) -> [];
ssl_dos_hostname(doc) ->   ["Hostname Denial of Service test case"];
ssl_dos_hostname(Config) when list(Config) ->
    C1 = [{max_header_size,256}, {max_header_action,reply414}|Config],
    ?line {M,H,P,N} = start_all(C1, false),
    dos_hostname(M,H,P,N),
    ok = stop_all(Config).

ssl_light_load(suite) -> [];
ssl_light_load(doc) ->   ["Light load test"];
ssl_light_load(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    light_load(M,H,P,N),
    ok = stop_all(Config).

ssl_medium_load(suite) -> [];
ssl_medium_load(doc) ->   ["Medium load test"];
ssl_medium_load(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    medium_load(M,H,P,N),
    ok = stop_all(Config).

ssl_heavy_load(suite) -> [];
ssl_heavy_load(doc) ->   ["Heavy load test"];
ssl_heavy_load(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    heavy_load(M,H,P,N),
    ok = stop_all(Config).

ssl_all_modules(suite) -> [];
ssl_all_modules(doc) ->   ["All modules test"];
ssl_all_modules(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, true),
    all_modules(M,H,P,N),
    ok = stop_all(Config).

ssl_mod_actions(suite) -> [];
ssl_mod_actions(doc) -> ["Module test: mod_actions"];
ssl_mod_actions(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    mod_actions(M, H, P, N),
    ok = stop_all(Config).

ssl_mod_alias(suite) -> [];
ssl_mod_alias(doc) -> ["Module test: mod_alias"];
ssl_mod_alias(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    mod_alias(M, H, P, N),
    ok = stop_all(Config).

ssl_mod_security(suite) -> [];
ssl_mod_security(doc) ->   ["Module test: mod_security"];
ssl_mod_security(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    mod_security(Config, M, H, P, N),
    ok = stop_all(Config).

ssl_mod_htaccess(suite) -> [];
ssl_mod_htaccess(doc) -> ["Module test: mod_htaccess"];
ssl_mod_htaccess(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false,[mod_htaccess]),
    mkTestData(filename:join([?CONFIG(data_dir,Config),"server_root/htdocs"]),?CONFIG(address,Config)),
    mod_htaccess(M, H, P, N),
    delTestData(filename:join([?CONFIG(data_dir, Config),"server_root/htdocs"])),
    ok = stop_all(Config).


ssl_mod_auth(suite) -> [];
ssl_mod_auth(doc) -> ["Module test: mod_auth"];
ssl_mod_auth(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    mod_auth(M, H, P, N),
    ok = stop_all(Config).

ssl_mod_auth_api(suite) -> [];
ssl_mod_auth_api(doc) -> ["Module test: mod_auth_api"];
ssl_mod_auth_api(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, true),
    mod_auth_api(Config, M, H, P, N),
    ok = stop_all(Config).

ssl_mod_auth_mnesia_api(suite) -> [];
ssl_mod_auth_mnesia_api(doc) -> ["Module test: mod_auth_mnesia_api"];
ssl_mod_auth_mnesia_api(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, true),
    mod_auth_mnesia_api(Config, M, H, P, N),
    ok = stop_all(Config).

ssl_mod_cgi(suite) -> [];
ssl_mod_cgi(doc) -> ["Module test: mod_cgi"];
ssl_mod_cgi(Config) when list(Config) ->
    VConf = [{manager_verbosity,         trace}, 
             {request_handler_verbosity, trace}],
    ?line {M,H,P,N} = start_all([{verbosity, VConf}|Config], false),
    mod_cgi(M, H, P, N),
    ok = stop_all(Config).

ssl_mod_esi(suite) -> [];
ssl_mod_esi(doc) -> ["Module test: mod_esi"];
ssl_mod_esi(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    mod_esi(M, H, P, N),
    ok = stop_all(Config).

ssl_mod_get(suite) -> [];
ssl_mod_get(doc) -> ["Module test: mod_get"];
ssl_mod_get(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    mod_get(M, H, P, N),
    ok = stop_all(Config).

ssl_mod_head(suite) -> [];
ssl_mod_head(doc) -> ["Module test: mod_head"];
ssl_mod_head(Config) when list(Config) ->
    ?line {M,H,P,N} = start_all(Config, false),
    mod_head(M, H, P, N),
    ok = stop_all(Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%%                                                                  %%
%% Test cases for testing of conformance to the HTTP/1.1 standard   %%
%%                                                                  %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%----------------------------------------------------------------------
%%Test that the host name request header field is parsed right
%%Specification rfc2616 chapter 3.1
%%----------------------------------------------------------------------
host(suite)->[];
host(doc) ->   ["Control that the server accepts/rejects requests with/ without host"];
host(Config) when list(Config) ->
    %% M = Mode
    %% H = Host
    %% P = Port
    %% N = Node
    ?line {M,H,P,N} = start_all(Config, false),
    host(M,H,P,N),
    ok = stop_all(Config).

host(M,H,P,N)->
    %No host needed for HTTP/1.1
    ?line e_poll(M,H,P,N, "GET / HTTP/1.0\r\n\r\n",
		 [{statuscode, 200}]),
    
    %No host must generate an error
    ?line e_poll_persistent(M,H,P,N, "GET / HTTP/1.1\r\n\r\n",[{statuscode, 400}]),
    
    %If it is a fully qualified URL no host is needed
    ?line e_poll_persistent(M,H,P,N, "GET HTTP://"++ H ++ ":"++ integer_to_list(P)++
			    "/ HTTP/1.1\r\n\r\n",[{statuscode, 200}]),

    %If both look at the url.
    ?line e_poll_persistent(M,H,P,N, "GET HTTP://"++ H ++ ":"++ integer_to_list(P)++ 
			    "/ HTTP/1.1\r\nHost:"++H++ "\r\n\r\n",[{statuscode, 200}]),
    
    %%Allow the request if its a Host field  
    ?line e_poll_persistent(M,H,P,N, "GET / HTTP/1.1\r\nHost:"++H++ "\r\n\r\n",[{statuscode, 200}]).
    
%%----------------------------------------------------------------------
%%Test Chunked transfer Encoding
%%RFC2616, Chapter 3.6.1, 14.40  and 14.41
%%----------------------------------------------------------------------

chunked(suite)->[];
chunked(doc) ->   ["Control that the server accepts chunked requests"];
chunked(Config) when list(Config) ->
    %% M = Mode
    %% H = Host
    %% P = Port
    %% N = Node
    ?line {M,H,P,N} = start_all(Config, false),
    chunked(M,H,P,N),
    ok = stop_all(Config).



%% If the body of the message is encoded used the chunked transfer encoding 
%% it looks somethin like this:
%% METHOD URI HTTP/VSN
%% Transfer-Encoding: chunked
%% CRLF
%% ChunkSize
%% Chunk
%% ChunkSize
%% Chunk
%% 0
%% Trailer

chunked(M,H,P,N)->
    %No host needed for HTTP/1.1
    ?line e_poll_persistent(M,H,P,N, 
			    "GET / HTTP/1.1\r\n"++ 
			    "Host:"++H++"\r\n"
			    "Transfer-Encoding:chunked\r\n"++
			    "\r\n"++
			    "A\r\n"++
			    "1234567890\r\n"++
			    "4\r\n"++
			    "HEJ!\r\n"++
			    "0\r\n\r\n",
		 [{statuscode, 200}]),
    %No host needed for HTTP/1.1
    ?line e_poll_persistent(M,H,P,N, 
			    "GET / HTTP/1.1\r\n"++ 
			    "Host:"++H++"\r\n"
			    "Transfer-Encoding:chunked\r\n"++
			    "Trailer:Content-Type\r\n"++
			    "\r\n"++
			    "A\r\n"++
			    "1234567890\r\n"++
			    "4\r\n"++
			    "HEJ!\r\n"++
			    "0\r\n"
			    "Content-Type:text/plain\r\n\r\n",
		 [{statuscode, 200}]).



%----------------------------------------------------------------------
% Control that the server response to a 100 expect request two times
% First to validate the request and then the actual response
%----------------------------------------------------------------------
expect(suite)->[];
expect(doc) ->   ["Control that the server handles request with the expect header field appropiate"];
expect(Config) when list(Config) ->
    %% M = Mode
    %% H = Host
    %% P = Port
    %% N = Node
    ?line {M,H,P,N} = start_all(Config, false),
    expect(M,H,P,N),
    ok = stop_all(Config).

expect(Mode,Host,Port,Node)->
    ?DEBUG("expect() -> ~n"
	   "  Host:    ~p ~n"
	   "  Port:    ~p ~n",
	   [Host,Port]),
    {ok, Socket} = ?CONNECT(Mode, Host, Port),
    ?DEBUG("expect() -> Socket = ~p",[Socket]),
    Request="GET / HTTP/1.1\r\nHost:"++Host++"\r\nContent-Length:22\r\nExpect:100-continue\r\n\r\n",
    ?SEND(Mode, Socket,Request),
    case get_100response(Socket) of
	ok ->
	    ?DEBUG("expect() -> 100 Continue received, now validate",[]),
	    ?SEND(Mode, Socket,"1234567890123456789012"),
	    ?DEBUG("expect() -> Body sent to the server",[]),
	    Res = httpd_test_lib:validate_ppoll(Request, Socket, [], [],
						[{statuscode,200}], 
						Node, Port, head),
	    ?DEBUG("expect() -> Validation result: ~p",[Res]),
	    ?CLOSE(Mode,Socket),
	    Res;
	error ->
	    ?DEBUG("expect() -> 100 Continue not received, ERROR",[]),
	    ?CLOSE(Mode,Socket),
	    ?line ?FAIL(expect100)
    end.
	    




get_100response(Socket)->
    ?DEBUG("get_100response()-> Response",[]),
    receive
	{tcp,Socket,Data} ->
	    case string:str(Data,"\r\n") of
		0 ->
		    get_100response(Socket);
		N ->
		    case Data of
			[$H,$T,$T,$P,$/,$1,$.,$1,$ ,$1,$0,$0 |Rest]->
			    ?DEBUG("get_100response()-> Got 100 Continue",[]),
			    ok;
			_ ->
			    error
		    end
	    end;
	Error ->
	    ?DEBUG("get_100response()-> Error in recieving 100 Continue",[]),
	    error
    end.

%%----------------------------------------------------------------------
%%Control the possibility to make range requests.....
%%A Range request is a request for one or more parts of
%%a document. Acrobat  Reader uses this feature quite much
%%----------------------------------------------------------------------
range(suite)->[];
range(doc) ->   ["Control that the server can handle range requests to plain files"];
range(Config) when list(Config) ->
    %% M = Mode,
    %%H = Host,
    %%P = Port
    %% N = Node
    mkRangeTestData(filename:join([?CONFIG(data_dir,Config),"server_root/htdocs"])),
    ?line {M,H,P,N} = start_all(Config, false),
    range(M,H,P,N),
    rmRangeTestData(filename:join([?CONFIG(data_dir,Config),"server_root/htdocs"])),
    ok = stop_all(Config).
    

range(M,H,P,N)->
     %No host needed for HTTP/1.0
    ?line e_poll_persistent(M,H,P,N,"GET /range.txt HTTP/1.1\r\nHost:"++H
			    ++"\r\nRange:bytes=110-120\r\n\r\n",[{statuscode,416}]),
    %%The simples of all range request a range
    Request1="GET /range.txt HTTP/1.1\r\nHost:"++H++"\r\nRange:bytes=0-9\r\n\r\n",    
    ?line {ok, Socket1} = ?CONNECT(M, H, P),
    ?SEND(M, Socket1,Request1),
    ?line ok=validateRangeRequest(Socket1,[],"1234567890",$2,$0,$6),
    ?CLOSE(M,Socket1),
    
    %Request the end of the file
    Request2="GET /range.txt HTTP/1.1\r\nHost:"++H++"\r\nRange:bytes=90-\r\n\r\n",    
    ?line {ok, Socket2} = ?CONNECT(M, H, P),
    ?SEND(M, Socket2,Request2),
    ?line ok=validateRangeRequest(Socket2,[],"1234567890",$2,$0,$6),
    ?CLOSE(M,Socket2),
    
    %%The las byte in the file
    Request3="GET /range.txt HTTP/1.1\r\nHost:"++H++"\r\nRange:bytes=-1\r\n\r\n",    
    ?line {ok, Socket3} = ?CONNECT(M, H, P),
    ?SEND(M, Socket3,Request3),
    ?line ok=validateRangeRequest(Socket3,[],"0",$2,$0,$6),
    ?CLOSE(M,Socket3),

    %%Multi Range
    Request4="GET /range.txt HTTP/1.1\r\nHost:"++H++"\r\nRange:bytes=0-0,2-2,-1\r\n\r\n",    
    ?line {ok, Socket4} = ?CONNECT(M, H, P),
    ?SEND(M, Socket4,Request4),
    ?line ok=validateRangeRequest(Socket4,[],"130",$2,$0,$6),
    ?CLOSE(M,Socket4).




validateRangeRequest(Socket,Response,ValidBody,C,O,DE)->
    ?DEBUG("validateRangeRequest()->",[]),
    receive
	{tcp,Socket,Data} ->
	    case string:str(Data,"\r\n") of
		0->
		    validateRangeRequest(Socket,Response++Data,ValidBody,C,O,DE);
		N ->
		    case Response++Data of
			[$H,$T,$T,$P,$/,$1,$.,$1,$ ,C,O,DE |Rest]->
			    ?DEBUG("validate()-> Got "++[C,O,DE] ++ " Continue",[]),
			    case [C,O,DE] of
				"206" ->
				    validateRangeRequest1(Socket,Response++Data,ValidBody);
				_ ->
				    bad_code
			    end;
			_->
			    error
		    end
	    end;
	Error ->
	    ?DEBUG("validateRangeRequest()-> Error in receiving  the response",[]),
	    error
    end.

validateRangeRequest1(Socket,Response,ValidBody)->	
    ?DEBUG("validateRangeRequest()->",[]),
    case httpd_test_lib:end_of_header(Response) of
	false->
	    receive
		{tcp,Socket,Data} ->
		    validateRangeRequest1(Socket,Response++Data,ValidBody);
		_->
		    error
	    end;
	{true,Head1,Body,Size}->
	    %%In this case size will be 0 if it is a multipart so dint use it.
	    validateRangeRequest2(Socket,Head1,Body,ValidBody,getRangeSize(Head1))
    end.

validateRangeRequest2(Socket,Head,Body,ValidBody,{multiPart,Boundary})->
    ?DEBUG("validateRangeRequest2()->SoFar:~p~nMultipart:true~n",[Body]),
    case endReached(Body,Boundary) of
	true ->
	    validateMultiPartRangeRequest(Body,ValidBody,Boundary);
	false->
	    receive
		{tcp,Socket,Data}->
		    validateRangeRequest2(Socket,Head,Body++Data,ValidBody,{multiPart,Boundary});
		{tcp_closed,Socket} ->
		    error;
		_ ->
		    error
	    end
    end;
validateRangeRequest2(Socket,Head,Body,ValidBody,BodySize) when integer(BodySize) ->
    ?DEBUG("validateRangeRequest2()->SoFar:~p",[Body]),
    case length(Body)  of
	Size when Size==BodySize ->
	    ?DEBUG("Retrived Range Body:~p",[Body]),
	    case Body of
		ValidBody ->
		    ok;
		Body ->
		    error
	    end;	
	Size when Size<BodySize ->
	    receive
		{tcp,Socket,Data} ->
		    validateRangeRequest2(Socket,Head,
					  Body++Data,ValidBody,BodySize);
		_ ->
		    error
	    end;
	Error ->
	    ?DEBUG("validateRangeRequest2()-> Body To big",[]),
	    error
    end.


validateMultiPartRangeRequest(Body,ValidBody,Boundary)->
    case regexp:split(Body,"--"++Boundary++"--") of
	%%Last is the epilogue and must be ignored 
	{ok,[First|Last]}->
	    %%First is now the actuall http request body.
	    case regexp:split(First,"--"++Boundary) of
		%%Parts is now a list of ranges and the heads for each range
		%%Gues we try to split out the body
		{ok,Parts}->
		    case lists:flatten(lists:map(fun splitRange/1,Parts)) of
			ValidBody->
			    ok;
		       ParsedBody->
			    ?DEBUG("Retrived Range Body:~p",[ParsedBody]),
			    error=ParsedBody
		    end
	    end;
	_ ->
	    error
    end.


splitRange(Part)->	    
    case regexp:split(Part,"\r\n\r\n") of
	{ok,[Head,Body]} ->
	    string:substr(Body,1,length(Body)-2);
	_ ->
	    []
    end.


	    


endReached(Body,Boundary)->
    EndBound="--"++Boundary++"--",
    case string:str(Body,EndBound) of
	0 -> 
	    false;
	_ ->
	    true
    end.
    
	    
getRangeSize(Head)->
    ?DEBUG("getrangeSize()-> Head:~s",[Head]),
    case controlMimeType(Head) of
	{multiPart,BoundaryString}->
	    {multiPart,BoundaryString};
	_->
	    case regexp:match(Head,"Content-Range:bytes=.*\r\n") of
		{match,Start,Lenght}->
		    %%Get the range data remove the fieldname and the end of line.
		    RangeInfo=string:substr(Head,Start+20,Lenght-(20-2)),
		    ?DEBUG("getrangeSize()-> singleRange",[]), 
		    rangeSize(RangeInfo);
		_->
		    error
	    end
    end.
%%RangeInfo is NNN1-NNN2/NNN3
%%NNN1=RangeStartByte
%%NNN2=RangeEndByte
%%NNN3=total amount of bytes in file 
rangeSize(RangeInfo)->
    ?DEBUG("rangeSize()-> RangeInfo:~s ~n",[RangeInfo]), 
    StartByte=lists:takewhile(fun(X)->
				      num(X,true)
			      end,RangeInfo),
    RangeInfo2=string:substr(RangeInfo,length(StartByte)+2),
    EndByte=lists:takewhile(fun(X)->
				    num(X,true)
			    end,RangeInfo2),
    ?DEBUG("rangeSize()-> StartByte:~s ,EndByte:~s ~n",[StartByte,EndByte]), 
    case list_to_integer(EndByte)-list_to_integer(StartByte) of
	Val when number(Val) ->
	    Val+1; %%Add one since it is startByte to endbyte ie 0-0 is 1 byte 0-99 is 100 byutes
	_Val ->
	    error
    end.
    
num(CharVal,RetVal)when CharVal>=48,CharVal=<57->
    RetVal;
num(_CharVal,true)->
    false;
num(_CharVal,false)->
    true.



controlMimeType(Head)->
    ?DEBUG("controlMimeType()-> ~s" ,[Head]),
    case regexp:match(Head,"Content-Type:multipart/byteranges.*\r\n") of
	{match,Start,Length}->
	    FieldNameLen=length("Content-Type:multipart/byteranges"),
	    case clearBoundary(string:substr(Head,Start+FieldNameLen,Length-(FieldNameLen+2))) of
		error ->
		    ?DEBUG("controlMimeType()->error",[]),
		    error;
		BoundaryStr->
		    ?DEBUG("controlMimeType()->{multiPart, ~p}",[BoundaryStr]),
		    {multiPart,BoundaryStr}
	    end;
	nomatch->
	    0;
	_ ->
	    error
    end.

clearBoundary(Boundary)->
    ?DEBUG("clearBoundary()-> ~s",[Boundary]),
    case regexp:match(Boundary,"boundary=.*\$") of
	{match,Start1,Length1}->
	    BoundLen=length("boundary="),
	    string:substr(Boundary,Start1+BoundLen,Length1-BoundLen);
	_ ->
	    error
    end.


%%----------------------------------------------------------------------
%% If_test test the If-Modified-Since, If-UnModifed-Since
%% ,If-Match If-None_match header fields.
%%----------------------------------------------------------------------
if_test(suite)->[];
if_test(doc) ->   
    ["Test that the if - request header fields is handled correclty"];
if_test(Config) when list(Config) ->
    DocRoot = filename:join([?CONFIG(data_dir,Config),
			     "server_root/htdocs"]),
    %% M = Mode,
    %% H = Host,
    %% P = Port
    %% N = Node
    ?line {M,H,P,N} = start_all(Config, false),
    if_test(M,H,P,N,DocRoot),
    ok = stop_all(Config).

% if_test_get(Host, If, Tail) ->
%     "GET / HTTP/1.1\r\nHost:" ++ Host ++ If ++ Tail ++ ++"\r\n\r\n".

% if_test_mod_get(Host, Since) ->
%     Tail = httpd_util:rfc1123_date(calendar:gregorian_seconds_to_datetime(Since)),
%     if_test_get(Host, "\r\nIf-Modified-Since:", Tail).

% if_test_umod_get(Host, Since) ->
%     Tail = httpd_util:rfc1123_date(calendar:gregorian_seconds_to_datetime(Since)),
%     if_test_get(Host, "\r\nIf-Unmodified-Since:", Tail).

% if_test_ematch_get(Host, FileInfo) ->
%     Etag = httpd_util:create_etag(FileInfo),
%     if_test_get(Host, "If-Match:", Etag).

% if_test_match_get(Host) ->
%     if_test_get(Host, "If-Match:", "NotEtag").

if_test(M,H,P,N,DocRoot)->
    ?line {ok, FileInfo} = 
	file:read_file_info(filename:join([DocRoot,"index.html"])),
    CreatedSec = 
	calendar:datetime_to_gregorian_seconds(FileInfo#file_info.mtime),

    %% Test that we get the data when the file is modified
    ?line ok = e_poll_persistent(M,H,P,N, 
				 "GET / HTTP/1.1\r\nHost:"++H++"\r\nIf-Modified-Since:" ++
				 httpd_util:rfc1123_date(calendar:gregorian_seconds_to_datetime(CreatedSec-1))++"\r\n\r\n",
				 [{statuscode, 200}]),
    ?line ok = e_poll_persistent(M,H,P,N, 
				  "GET / HTTP/1.1\r\nHost:"++H++"\r\nIf-Modified-Since:"++ 
				  httpd_util:rfc1123_date(calendar:gregorian_seconds_to_datetime(CreatedSec+100))++"\r\n\r\n",
				  [{statuscode, 304}]),
    %%Control that the If-Unmodified-Header lmits the response
    ?line ok = e_poll_persistent(M,H,P,N, 
				 "GET / HTTP/1.1\r\nHost:"++H++"\r\nIf-Unmodified-Since:" ++
				 httpd_util:rfc1123_date(calendar:gregorian_seconds_to_datetime(CreatedSec+1))++"\r\n\r\n",
				 [{statuscode, 200}]),
    ?line ok = e_poll_persistent(M,H,P,N, 
				 "GET / HTTP/1.1\r\nHost:"++H++"\r\nIf-Unmodified-Since:"++ 
				 httpd_util:rfc1123_date(calendar:gregorian_seconds_to_datetime(CreatedSec-1))++"\r\n\r\n",
				 [{statuscode, 412}]),

    %% Control that we gets the body when the etag match

    ?line ok = e_poll_persistent(M,H,P,N, 
				 "GET / HTTP/1.1\r\nHost:"++H++"\r\n"++
				 "If-Match:"++ httpd_util:create_etag(FileInfo)++"\r\n\r\n",
				 [{statuscode, 200}]),
    ?line ok = e_poll_persistent(M,H,P,N, 
				 "GET / HTTP/1.1\r\nHost:"++H++"\r\n"++
				 "If-Match:NotEtag\r\n\r\n",
				 [{statuscode, 412}]),

    %% Control the response when the if-none-match header is there
    ?line ok = e_poll_persistent(M,H,P,N, 
				 "GET / HTTP/1.1\r\nHost:"++H++"\r\n"++
				 "If-None-Match:NoTaag,"++ httpd_util:create_etag(FileInfo)++"\r\n\r\n",
				 [{statuscode, 304}]),
%%    %?line ok = e_poll_persistent(M,H,P,N, 
%%         			  "PUT / HTTP/1.1\r\nHost:"++H++"\r\n"++
%				  "If-None-Match:"++NoTag,httpd_util:create_etag(FileInfo)++"\r\n"
%				  [{statuscode,412}]),
    ?line ok = e_poll_persistent(M,H,P,N, 
				 "GET / HTTP/1.1\r\nHost:"++H++"\r\n"++
				 "If-None-Match:NotEtag,NeihterEtag\r\n\r\n",
				 [{statuscode,200}]).
    
%%----------------------------------------------------------------------
%% The trace method is used as a traceroute for the webb by sending the
%% Max-Forwards Request header field with values from 1 to n the user can find
%% out which proxys that was involved in the response 

http_trace(suite)->[];
http_trace(doc) ->   ["Test the trace module "];
http_trace(Config) when list(Config) ->
    %% M = Mode,
    %% H = Host,
    %% P = Port
    %% N = Node
    ?line {M,H,P,N} = start_all(Config, false),
    http_trace(M,H,P,N),
    ok = stop_all(Config).


http_trace(M,H,P,N)->
    %% Test that we got 200 when we do a 
    ?line ok = e_poll_persistent(M,H,P,N, 
				 "TRACE / HTTP/1.1\r\n"++
				 "Host:"++H++"\r\n"++
				 "Max-Forwards:2\r\n\r\n",[{statuscode, 200}]),
    ?line ok = e_poll_persistent(M,H,P,N, 
				 "TRACE / HTTP/1.0\r\n\r\n",
				 [{statuscode, 501}]).


%%----------------------------------------------------------------------
%% Control that mod_include mod_esi mod_cgi and mod_include
%%----------------------------------------------------------------------
http1_1_head(suite)->[];
http1_1_head(doc) ->   ["Test the trace module "];
http1_1_head(Config) when list(Config) ->
    %% M = Mode,
    %% H = Host,
    %% P = Port
    %% N = Node
    ?line {M,H,P,N} = start_all(Config, false),
    http1_1_head(M,H,P,N),
    ok = stop_all(Config).


http1_1_head(M,H,P,N)->
    %% mod_include 
    ?line poll(M,H,P,N,"HEAD /fsize.shtml HTTP/1.0\r\n\r\n", "200"),
    ?line poll(M,H,P,N,"HEAD /fsize.shtml HTTP/1.1\r\nhost:" ++ H  ++ "\r\n\r\n", "200"),
    %% mod_esi
    ?line poll(M,H,P,N,"HEAD /cgi-bin/erl/httpd_example/newformat HTTP/1.0\r\n\r\n", "200"),
    ?line poll(M,H,P,N,"HEAD /cgi-bin/erl/httpd_example/newformat HTTP/1.1\r\nhost:" ++ H  ++ "\r\n\r\n", "200"),
    %% mod_cgi
    Script =
	case os:type() of
	    {win32, _} ->
		"printenv.bat";
	    _ ->
		"printenv.sh"
	end,
    ?line poll(M,H,P,N,"HEAD /cgi-bin/" ++ Script ++ " HTTP/1.0\r\n\r\n", "200"),
    ?line poll(M,H,P,N,"HEAD /cgi-bin/" ++ Script ++ " HTTP/1.1\r\nhost:" ++ H  ++ "\r\n\r\n", "200").

%%----------------------------------------------------------------------
%% Mod_cgi use chunked encoding on the body for a http1.1 request test it !
%%----------------------------------------------------------------------

mod_cgi_chunked_encoding_test(suite)->[];
mod_cgi_chunked_encoding_test(doc) ->   ["Test the trace module "];
mod_cgi_chunked_encoding_test(Config) when list(Config) ->
    %% M = Mode,
    %% H = Host,
    %% P = Port
    %% N = Node
    ?line {M,H,P,N} = start_all(Config, false),
    mod_cgi_chunked_encoding_test(M,H,P,N,
				  ["GET /cgi-bin/printenv.sh  HTTP/1.1\r\nHost:"++H++"\r\n\r\n",  
				   "GET /cgi-bin/erl/httpd_example/newformat  HTTP/1.1\r\nHost:"++H++"\r\n\r\n"]),
    ok = stop_all(Config).


mod_cgi_chunked_encoding_test(M,H,P,N,[])->
    ok;
mod_cgi_chunked_encoding_test(M,H,P,N,[Request1|Requests])->
    %%mod_cgi
    ?INFO("RequestURL: ~s",[Request1]),
    ?line {ok, Socket1} = ?CONNECT(M, H, P),
    ?line ?SEND(M, Socket1,Request1),
    ?line control_chunked(M,Socket1,"200"),
    ?line ?CLOSE(M,Socket1).
    

control_chunked(Mode,Socket,Status)->
    control_chunked(Mode,Socket,Status,{head,[]}).

control_chunked(Mode,Socket,Status,{head,HeadContent})->
    case httpd_util:split(HeadContent,"\r\n\r\n|\n\n",2) of
	{ok,[Head,Body]}->
	    case control_chunked_head(Head,Status) of
		ok ->
		    gather_chunks(Mode,Socket,lists:reverse(Body));
		{error,Reason} ->
		    {error,Reason}
	    end;
	{ok,[Head]} ->
	    receive 
		{tcp,Socket,Data}->
		    control_chunked(Mode,Socket,Status,
				    {head,HeadContent++Data});
		{tcp_closed,Socket} ->
		    {error,socketClosed};
		{tcp_error,socket,Reason} ->
		    {error,socketerror}
	    end
    end.

%%----------------------------------------------------------------------
%%Get the data and add it reversed first then we easaly find
%%the end and can use lists:reverse to get the whole string.
%%----------------------------------------------------------------------
gather_chunks(Mode,Socket,[$\n, $\r, $0 |Body])->
    control_chunks(lists:reverse([$\n, $\r, $0 |Body]));

gather_chunks(Mode,Socket,Body)->				       
    receive
	{tcp,Socket,Data}->
	    gather_chunks(Mode,Socket,lists:reverse(Data)++Body);
	{tcp_closed,Socket} ->
	    {error,"Socket Closed"};
	{tcp_error,Socket,Reason}->
	    {error,"Socket Error"}
    end.

%%----------------------------------------------------------------------
%% Control the collected chunks according to size
%%----------------------------------------------------------------------
control_chunks(Chunks0)->
    %Remove any newlines in the beginning
    Chunks=lists:dropwhile(fun is_newline/1,Chunks0),
    control_chunks1(Chunks).

control_chunks1(Chunks)->
    Size=get_chunk_size(Chunks),
    case Size of 
	0 ->
	    %%Cant control that there is anything left since there might be trailers
	    ok;
	N->
	    %%Remove SizeCRLF Size number of signs and the trailing CRLF and control next part
	   case remove_chunk(Chunks,Size) of
	       {ok,Chunks1}->
		   control_chunks1(Chunks1);
	       _ ->
		   {error,bad_chunk_size}
	   end
    end.

get_chunk_size(Chunks)->
    SizeList=lists:takewhile(fun is_hex_sign/1,Chunks),
    httpd_util:hexlist_to_integer(SizeList).


remove_chunk(Chunks,Size)->
    %%remove the size row
    Chunks1=remove_size(Chunks),
    case remove_chunk1(Chunks1,Size) of
	[$\r,$\n|Rest]->
	    {ok,Rest};
	Rest ->
	    {error,Rest}
    end.

remove_chunk1(Chunks,Size)->
    lists:substring(Chunks,Size+1). %%Add 1 since we want to remove sign one to Size, including Siz
remove_size(Chunks)->
    lists:dropwhile(fun is_newline/1,lists:drop_while( fun not_newline/1,Chunks)).
    

%%----------------------------------------------------------------------
%%Control whether a sign is valid for a hexlist
%%----------------------------------------------------------------------
is_hex_sign(X) when X>47,X<58 ->
    true;
is_hex_sign(X) when X>64,X<71 ->
    true;
is_hex_sign(_X) ->
    false.
%%----------------------------------------------------------------------
%%controls that a sign is not a newline
%%----------------------------------------------------------------------       
not_newline($\r)->
    fasle;
not_newline($\r) ->
    false;
not_newline(_) ->
    true.
%%----------------------------------------------------------------------
%%Controls whether a sign is a newline sign
%%----------------------------------------------------------------------
is_newline($\r)->
    true;
is_newline($\r) ->
    true;
is_newline(_) ->
    false.
%%----------------------------------------------------------------------	
%%A valid head for chunked encoding must contain a transfer-encoding
%% header field and a 200 status code.
%%---------------------------------------------------------------------- 				   
control_chunked_head(Head,Status)->
    case regexp:first_match(Head,"HTTP/1.1 "++Status) of
	{ok,1,N}->
	    case regexp:first_match(Head,"Transer-Encoding:[\s\t]*chunked") of
		{ok,Start,Length}->
		    ok;
		_ ->
		    {error,"Bad transfer encoding field"}
	    end;
	_->
	    {error,"Bad statuscode"}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%%                                                                  %%
%% create_config                                                    %%
%%                                                                  %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_config(Config, Port, Type, Host, Options) ->
    ?LOG("Create config for: <~p,~p,~p>",[Port,Type,Host]),
    ServerRoot = filename:join(
		   [filename:absname(?CONFIG(data_dir,Config)),
		    "server_root"]),
    ?DEBUG("ServerRoot: ~p", [ServerRoot]),
    PrivDir = ?CONFIG(priv_dir, Config),
    ?DEBUG("PrivDir: ~p", [PrivDir]),
    file:make_dir(filename:join([PrivDir, "logs"])),
    Mods         = io_lib:format("~p",[?MODULE]),
    Funcs        = io_lib:format("~p",[ssl_password_cb]),
    MaxHdrSize   = inets_test_lib:get_config(max_header_size, Config, 256),
    MaxHdrAction = inets_test_lib:get_config(max_header_action, Config, close),
    MaxHdrSz     = io_lib:format("~p",[MaxHdrSize]),
    MaxHdrAct    = io_lib:format("~p",[MaxHdrAction]),
    ?DEBUG("before SLL assign", []),
    SSL =
	case Type of
	    ssl ->
		[cline(["SSLCertificateFile ", 
			filename:join(ServerRoot, "ssl/ssl_server.pem")]),
		 cline(["SSLCertificateKeyFile ",
			filename:join(ServerRoot, "ssl/ssl_server.pem")]),
		 cline(["SSLCACertificateFile ",
			filename:join(ServerRoot, "ssl/ssl_server.pem")]),
		 cline(["SSLPasswordCallbackModule ", Mods]),
		 cline(["SSLPasswordCallbackFunction ", Funcs]),
		 cline(["SSLVerifyClient 0"]),
		 cline(["SSLVerifyDepth 1"])];
	    _ ->
		[]
	end,
    %% ?DEBUG("SSL: ~p", [SSL]),
    %% We dont want to do the mod_htaccess control in every test 
    %% So only in the cases when the mod_htaccess key is in the 
    Mod_order = case lists:member(mod_htaccess,Options) of
		    false ->
			"Modules mod_alias mod_auth mod_security "
			    "mod_responsecontrol mod_trace mod_esi "
			    "mod_actions mod_cgi mod_include mod_dir "
			    "mod_range mod_get "
			    "mod_head mod_log mod_disk_log";
		    true ->
			"Modules mod_alias mod_htaccess mod_auth mod_security "
			    "mod_responsecontrol mod_trace mod_esi "
			    "mod_actions mod_cgi mod_include mod_dir "
			    "mod_range mod_get "
			    "mod_head mod_log mod_disk_log"
		end,
	    
    %% ?DEBUG("Mod_order: ~n~s", [Mod_order]),
    C=[
       cline(["Port ", integer_to_list(Port)]),
       cline(["ServerName ", Host]),
       cline(["SocketType ", atom_to_list(Type)]),
       cline([Mod_order]),
       cline(["ServerAdmin mattias@erix.ericsson.se"]),
       cline(["ServerRoot ", ServerRoot]),
       cline(["ErrorLog ", PrivDir, 
	      "/logs/error_log_", integer_to_list(Port)]),
       cline(["TransferLog ", PrivDir, 
	      "/logs/access_log_", integer_to_list(Port)]),
       cline(["SecurityLog ", PrivDir, 
	      "/logs/security_log_", integer_to_list(Port)]),
       cline(["ErrorDiskLog ", PrivDir, 
	      "/logs/error_disk_log_", integer_to_list(Port)]),
       cline(["ErrorDiskLogSize ", "200000 ", "10"]),
       cline(["TransferDiskLog ", PrivDir, 
	      "/logs/access_disk_log_", integer_to_list(Port)]),
       cline(["TransferDiskLogSize ", "200000 ", "10"]),
       cline(["SecurityDiskLog ", PrivDir, 
	      "/logs/security_disk_log_", integer_to_list(Port)]),
       cline(["SecurityDiskLogSize ", "200000 ", "10"]),
       cline(["MaxClients 10"]),
       cline(["MaxHeaderSize ",MaxHdrSz]),
       cline(["MaxHeaderAction ",MaxHdrAct]),
       cline(["DocumentRoot ", filename:join(ServerRoot, "htdocs")]),
       cline(["DirectoryIndex ", "index.html ", "welcome.html"]),
       cline(["DefaultType ", "text/plain"]),
       SSL,
       mod_alias_config(ServerRoot),
       
       config_directory(filename:join([ServerRoot,"htdocs", "open"]),
			"Open Area", 
			filename:join(ServerRoot, "auth/passwd"),
			filename:join(ServerRoot, "auth/group"),
			plain,
			"user one Aladdin",
			filename:join(PrivDir, "security_data")),
       config_directory(filename:join([ServerRoot,"htdocs", "secret"]),
			"Secret Area", 
			filename:join(ServerRoot, "auth/passwd"),
			filename:join(ServerRoot, "auth/group"),
			plain,
			"group group1 group2",
			filename:join(PrivDir, "security_data")),
       config_directory(filename:join([ServerRoot,"htdocs", "secret", "top_secret"]),
			"Top Secret Area", 
			filename:join(ServerRoot, "auth/passwd"),
			filename:join(ServerRoot, "auth/group"),
			plain,
			"group group3",
			filename:join(PrivDir, "security_data")),

       config_directory(filename:join([ServerRoot,"htdocs", "dets_open"]),
			"Dets Open Area", 
			filename:join(PrivDir, "passwd"),
			filename:join(PrivDir, "group"),
			dets,
			"user one Aladdin",
			filename:join(PrivDir, "security_data")),
       config_directory(filename:join([ServerRoot,"htdocs", "dets_secret"]),
			"Dets Secret Area", 
			filename:join(PrivDir, "passwd"),
			filename:join(PrivDir, "group"),
			dets,
			"group group1 group2",
			filename:join(PrivDir, "security_data")),
       config_directory(filename:join([ServerRoot,"htdocs", "dets_secret", "top_secret"]),
			"Dets Top Secret Area", 
			filename:join(PrivDir, "passwd"),
			filename:join(PrivDir, "group"),
			dets,
			"group group3",
			filename:join(PrivDir, "security_data")),

       config_directory(filename:join([ServerRoot,"htdocs", "mnesia_open"]),
			"Mnesia Open Area", 
			false,
			false,
			mnesia,
			"user one Aladdin",
			filename:join(PrivDir, "security_data")),
       config_directory(filename:join([ServerRoot,"htdocs", "mnesia_secret"]),
			"Mnesia Secret Area", 
			false,
			false,
			mnesia,
			"group group1 group2",
			filename:join(PrivDir, "security_data")),
       config_directory(filename:join(
			  [ServerRoot,"htdocs", "mnesia_secret","top_secret"]),
			"Mnesia Top Secret Area", 
			false,
			false,
			mnesia,
			"group group3",
			filename:join(PrivDir, "security_data"))
      ],
    %% ?DEBUG("C: ~n~p", [C]),
    FileName = integer_to_list(Port)++".conf",
    ?DEBUG("after filename assign", []),
    PrivDir = ?CONFIG(priv_dir,Config),
    ?DEBUG("PrivDir: ~p", [PrivDir]),
    NewConfigFile = filename:join([PrivDir,FileName]),
    ?DEBUG("NewConfigFile: ~p", [NewConfigFile]),
    ?line ok = file:write_file(NewConfigFile,C),
    ?LOG("Config file written to ~s", [NewConfigFile]),
    NewConfigFile.


cline(List) ->
    lists:flatten([List, "\r\n"]).

cline_if_set(Name, false) ->
    [];
cline_if_set(Name, Var) when list(Var) ->
    cline([Name, " ", Var]);
cline_if_set(Name, Var) when atom(Var)->
    cline([Name, " ", atom_to_list(Var)]).

config_directory(Dir, AuthName, AuthUserFile, AuthGroupFile, AuthDBType, Require, SF) ->
    file:delete(SF),
    [
     cline(["<Directory ", Dir, ">"]),
     cline(["SecurityDataFile ", SF]),
     cline(["SecurityMaxRetries 3"]),
     cline(["SecurityFailExpireTime ", integer_to_list(?fail_expire_time)]),
     cline(["SecurityBlockTime 1"]),
     cline(["SecurityAuthTimeout ", integer_to_list(?auth_timeout)]),
     cline(["SecurityCallbackModule ", atom_to_list(?MODULE)]),
     cline_if_set("AuthUserFile", AuthUserFile),
     cline_if_set("AuthGroupFile", AuthGroupFile),
     cline_if_set("AuthName", AuthName),
     cline_if_set("AuthDBType", AuthDBType),
     cline(["require ", Require]),
     cline(["</Directory>\r\n"])
    ].

mod_alias_config(Root) ->
    [
     cline(["Alias /icons/ ", filename:join(Root,"icons"), "/"]),
     cline(["Alias /pics/ ", filename:join(Root, "icons"), "/"]),
     cline(["ScriptAlias /cgi-bin/ ", filename:join(Root, "cgi-bin"), "/"]),
     cline(["ScriptAlias /htbin/ ", filename:join(Root, "cgi-bin"), "/"]),
     cline(["ErlScriptAlias /cgi-bin/erl httpd_example io"]),
     cline(["EvalScriptAlias /eval httpd_example io"])
    ].


%% init_mnesia_on_node

init_mnesia_on_node(Config, Port, NodeName) ->
    ?LOG("Setting up mnesia on node ~p", [NodeName]),
    case rpc:call(NodeName, ?MODULE, cleanup_mnesia, []) of
	ok ->
	    ok;
	Other ->
	    ?FAIL({failed_to_cleanup_mnesia, Other})
    end,
    case rpc:call(NodeName, ?MODULE, setup_mnesia, []) of
	{atomic, ok} ->
	    ok;
	Other2 ->
	    ?FAIL({failed_to_setup_mnesia, Other2})
    end,
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test functions used by all test cases.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_nof_clients(Load) ->
    get_nof_clients(os:type(),Load).

get_nof_clients(vxworks,light)  -> 1;
get_nof_clients(vxworks,medium) -> 3;
get_nof_clients(vxworks,heavy)  -> 5;
get_nof_clients(_,light)        -> 5;
get_nof_clients(_,medium)       -> 10;
get_nof_clients(_,heavy)        -> 20.
    
    
%%
%% light load
%%

light_load(M,H,P,N) ->
    load_test(M,H,P,N,get_nof_clients(light)).


%%
%% medium load
%%

medium_load(M,H,P,N) ->
    load_test(M,H,P,N,get_nof_clients(medium)).


%%
%% heavy load
%%

heavy_load(M,H,P,N) ->
    load_test(M,H,P,N,get_nof_clients(heavy)).


%%
%% load_test
%%

load_test(M,H,P,N,NofTesters) ->
    URIs = 
	[
	 "/index.html", 
	 "/echo.shtml", 
	 "/",
	 "/flastmod.shtml", 
	 "/misc/"
	],
    Fun = fun(Mod,Host,Port,Node, Req, Exp) -> 
		  poll(Mod, Host, Port, Node, Req, Exp) end,
    load_test(Fun, URIs ++ URIs, M,H,P,N, NofTesters, []).


load_test(_Fun, _URIs, _M,_H,_P,_N, 0, []) ->
    ?DEBUG("load_test -> done",[]),
    ok;
load_test(Fun, URIs, M,H,P,N, 0, List) ->
    ?DEBUG("load_test -> List: ~p",[List]),
    receive 
	{Pid, done} ->
	    ?LOG("load_test -> client ~p done",[Pid]),
	    load_test(Fun, URIs, M,H,P,N, 0, lists:delete(Pid, List));
	{'EXIT', Pid, normal} ->
	    ?LOG("load_test -> client ~p exited: normal",[Pid]),
	    load_test(Fun, URIs, M,H,P,N, 0, lists:delete(Pid, List));
	{'EXIT', Pid, Reason} ->
	    Str = lists:flatten(io_lib:format("client ~p exited: ~p", 
					      [Pid,Reason])),
	    ?LOG("load_test -> ~s",[Str]),
	    ?HTTPD_STATUS("http server status after pid exit: ",
			  httpd_status(N,P)),
	    ?SLEEP(5000),
	    ?HTTPD_STATUS("http server status after some sleep: ",
			  httpd_status(N,P)),
	    ?FAIL(Str);
	Other ->
	    ?LOG("load_test -> Other: ~p",[Other]),
	    load_test(Fun, URIs, M,H,P,N, 0, List)
    end;
load_test(Fun, URIs, M,H,P,N, X, List) ->
    ?DEBUG("load_test -> create client",[]),
    Pid = spawn_link(?MODULE,load_test_client,
		     [Fun, URIs, M, H, P, N, self(), 100]),
    load_test(Fun, lists:reverse(URIs), M,H,P,N, X-1, [Pid|List]).

load_test_client(_Fun, [], _M,_H,_P,_N, Boss, _Timeout) ->
    ?DEBUG("load_test_client -> heavy load client done", []),
    load_test_client_done(Boss);
load_test_client(Fun, [URI|URIs], M,H,P,N, Boss, Timeout) ->    
    ?DEBUG("load_test_client -> heavy load client poll for ~p", [URI]),
    Req = "GET "++URI++" HTTP/1.0\r\nConnection: Close\r\n"
	"From: m@erix\r\nReferer: http://www.ericsson.se/\r\n\r\n",
    Timeout1 = 
	case (catch Fun(M, H, P, N, Req, ["200", "500", "503"])) of
	    {'EXIT', {suite_failed, connection_closed, _, _}} ->
		%% Some platforms seems to handle heavy load badly.
		%% So, back off and see if this helps
		?LOG("load_test_client -> request failed: connection_closed", []),
		2 * Timeout;
	    _ ->
		Timeout
	end,
    ?SLEEP(Timeout1),
    load_test_client(Fun, URIs, M,H,P,N, Boss, Timeout1).

load_test_client_done(Boss) ->
    Boss ! {self(), done}.


%% 
%% Denial Of Service (DOS) attack test case
%% 

dos_hostname(M,H,P,N) ->
    H1 = {"","200"},
    H2 = {"dummy-host.ericsson.se","200"},
    H3 = {"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","414"},
    Hosts = [H1,H2,H3],
    ?line dos_hostname_poll(M,H,P,N,Hosts).

dos_hostname_poll(M,H,P,N,Hosts) ->
    ?line [dos_hostname_poll1(M,H,P,N,Host,Code) || {Host,Code} <- Hosts].
    
dos_hostname_poll1(M,H,P,N,Host,Code) ->
    ?DEBUG("dos_hostname_poll1() => send get rquest with host: ~s", [Host]),
    ?line poll(M,H,P,N,dos_hostname_request(Host),Code).
        
dos_hostname_request(H) ->
    "GET / HTTP/1.0\r\n" ++ H ++ "\r\n\r\n".


%% 
%% Simple block/unblock test case 1:
%% 1) (Start the server) read => 200, 
%% 3) block
%% 4) unblock, read => 200,
%% 

simple_block1(M,H,P,N) ->
    %% The block/unblock is a very crude way to make sure the listener
    %% has the right verbosity...
    ?LOG("simple_block1() -> 0.a) first block",[]),
    ?line block_server(N,H,P),
    ?LOG("simple_block1() -> 0.b) then unblock",[]),
    ?line unblock_server(N,H,P),


    Req = "GET / HTTP/1.0\r\ndummy-host.ericsson.se:\r\n\r\n",
    ?LOG("simple_block1() -> "
	   "1) start: get the admin state, should be unblocked", []),
    ?line unblocked = get_admin_state(N,H,P),

    ?LOG("simple_block1() -> 2) poll (200 expected)", []),
    ?line poll(M,H,P,N,Req,"200"),

    ?LOG("simple_block1() -> 3) block server", []),
    ?line ok = block_server(N,H,P),

    ?LOG("simple_block1() -> 4) get the admin state, should be blocked", []),
    ?line blocked = get_admin_state(N,H,P),

    ?LOG("simple_block1() -> 5) sleep 90 sec", []),
    ?SLEEP(90000),

    ?LOG("simple_block1() -> 6) unblock server", []),
    ?line ok = unblock_server(N,H,P),

    ?LOG("simple_block1() -> 7) get the admin state, should be unblocked", []),
    ?line unblocked = get_admin_state(N,H,P),

    ?LOG("simple_block1() -> 8) sleep 5 sec", []),
    ?SLEEP(5000),

    ?LOG("simple_block1() -> 9) poll (200 expected)", []),
    ?line poll(M,H,P,N,Req,"200").


%% 
%% Simple block/unblock test case 2:
%% 1) (Start the server) read => 200, 
%% 3) block, read => 503, 
%% 4) unblock, read => 200,
%% 

simple_block2(M,H,P,N) ->
    %% The block/unblock is a very crude way to make sure the listener
    %% has the right verbosity...
    ?LOG("simple_block2() -> 0.a) first block",[]),
    ?line block_server(N,H,P),
    ?LOG("simple_block2() -> 0.b) then unblock",[]),
    ?line unblock_server(N,H,P),


    Req = "GET / HTTP/1.0\r\ndummy-host.ericsson.se:\r\n\r\n",
    ?LOG("simple_block2() -> "
	   "1) start: get the admin state, should be unblocked", []),
    ?line unblocked = get_admin_state(N,H,P),

    ?LOG("simple_block2() -> 2) poll (200 expected)", []),
    ?line poll(M,H,P,N,Req,"200"),

    ?LOG("simple_block2() -> 3) block server", []),
    ?line ok = block_server(N,H,P),

    ?LOG("simple_block2() -> 4) get the admin state, should be blocked", []),
    ?line blocked = get_admin_state(N,H,P),

    ?LOG("simple_block2() -> 5) sleep 90 sec", []),
    ?SLEEP(90000),

    ?LOG("simple_block2() -> 6) poll (503 expected)", []),
    put(poll_send_delay, 1000),
    ?line poll(M,H,P,N,Req,"503"),
    erase(poll_send_delay),

%     case (catch poll(M,H,P,N,Req,"503")) of
% 	ok ->
% 	    ?LOG("simple_block2() -> 6.a) ok", []),
% 	    ok;
% 	{'EXIT', {suite_failed, connection_closed}} ->
% 	    ?LOG("simple_block2() -> 6.b) connection_closed", []),
% 	    ok;
% 	{'EXIT', {suite_failed, Reason}} ->
% 	    ?FAIL(Reason)
%     end,

    ?LOG("simple_block2() -> 7) unblock server", []),
    ?line ok = unblock_server(N,H,P),

    ?LOG("simple_block2() -> 8) get the admin state, should be unblocked", []),
    ?line unblocked = get_admin_state(N,H,P),

    ?LOG("simple_block2() -> 9) sleep 5 sec", []),
    ?SLEEP(5000),

    ?LOG("simple_block2() -> 10) poll (200 expected)", []),
    ?line poll(M,H,P,N,Req,"200").


%% 
%% Simple block/unblock test case 3
%% 1) (Start the server) get admin state => unblocked, 
%% 2) block, get admin state => blocked, 
%% 3) unblock, get admin state => unblocked,
%% 

simple_block3(M,H,P,N) ->
    ?line unblocked = get_admin_state(N,H,P),
    ?line block_server(N,H,P),
    ?line blocked = get_admin_state(N,H,P),
    ?line unblock_server(N,H,P),
    ?line unblocked = get_admin_state(N,H,P).


%% 
%% Simple block/unblock test case 4
%% 1) (Start the server) get admin state => unblocked, 
%% 2) block(non-disturbing), get admin state => blocked, 
%% 3) unblock, get admin state => unblocked,
%% 

simple_block4(M,H,P,N) ->
    ?DEBUG("simple_block4() -> "
	   "start: get the admin state, should be unblocked", []),
    ?line unblocked = get_admin_state(N,H,P),
    ?DEBUG("simple_block4() -> block the server", []),
    ?line block_nd_server(N,H,P),
    ?DEBUG("simple_block4() -> "
	   "get the admin state, should be blocked", []),
    ?line blocked = get_admin_state(N,H,P),
    ?DEBUG("simple_block4() -> unblock the server", []),
    ?line unblock_server(N,H,P),
    ?DEBUG("simple_block4() -> "
	   "get the admin state, should be unblocked", []),
    ?line unblocked = get_admin_state(N,H,P).


%% 
%% Simple block/unblock test case 5
%% 1) (Start the server) get admin state => unblocked, 
%% 2) block(non-disturbing,Timeout), get admin state => blocked, 
%% 3) unblock, get admin state => unblocked,
%% 

simple_block5(M,H,P,N) ->
    ?line unblocked = get_admin_state(N,H,P),
    ?line block_nd_server(N,H,P,5000),
    ?line blocked = get_admin_state(N,H,P),
    ?line unblock_server(N,H,P),
    ?line unblocked = get_admin_state(N,H,P).


%% Tests that the server is forced into blocked state
%% when an disturbing block is issued on an active server:
%% E.g. the ongoing requests are terminated
block_when_active(M,H,P,N) ->
    ?DEBUG("block_when_active() -> entry",[]),
    Old = ?ETRAP_GET(),
    ?DEBUG("block_when_active() -> trap_exit = ~p",[Old]),
    ?ETRAP_SET(true),
    ?DEBUG("block_when_active() -> trap_exit = ~p",[?ETRAP_GET()]),
    %% The block/unblock is a very crude way to make sure the listener
    %% has the right verbosity...
    ?DEBUG("block_when_active() -> first block",[]),
    ?line block_server(N,H,P),
    ?DEBUG("block_when_active() -> then unblock",[]),
    ?line unblock_server(N,H,P),
    ?DEBUG("block_when_active() -> and now start the poll process",[]),
    ?line Pid = long_poll(M,H,P,N,"200",45000),
    ?DEBUG("block_when_active() -> sleep some to make sure it's underway",[]),
    ?SLEEP(5000),
    ?DEBUG("block_when_active() -> now do the disturbing block",[]),
    ?line block_server(N,H,P),
    ?DEBUG("block_when_active() -> await completion of the poll process",[]),
    ?line await_suite_failed_process_exit(Pid, "poller", Old, 60000, 
					  connection_closed),
    ?ETRAP_SET(Old),
    ok.
	  
	    
%% Tests that the server which is in active usage state is 
%% allowed to complete all onging requests when an disturbing 
%% block with timeout is performed.
block_when_active_to1(M,H,P,N) ->
    Old = ?ETRAP_GET(),
    ?ETRAP_SET(true),
    ?DEBUG("block_when_active_to1 -> entry",[]),
    %% The block/unblock is a very crude way to make sure the listener
    %% has the right verbosity...
    ?DEBUG("block_when_active_to1 -> first block",[]),
    ?line block_server(N,H,P),
    ?DEBUG("block_when_active_to1 -> then unblock",[]),
    ?line unblock_server(N,H,P),
    ?DEBUG("block_when_active_to1 -> and now start the poll process",[]),
    ?line Poller = long_poll(M,H,P,N,"200",60000),
    ?DEBUG("block_when_active_to1 -> "
	"sleep some to make sure it's underway",[]),
    ?SLEEP(5000),
    ?DEBUG("block_when_active_to1 -> now do the disturbing block",[]),
    ?line Blocker = blocker(N,H,P,35000),
    ?DEBUG("block_when_active_to1 -> "
	"await completion of the blocker process",[]),
    ?line await_normal_process_exit(Blocker, "blocker", Old, 40000),
    ?DEBUG("block_when_active_to2 -> "
	"await completion of the poller process",[]),
    ?line await_normal_process_exit(Poller, "poller", Old, 30000),
    ?ETRAP_SET(Old),
    ok.
	  

%% Tests that the server is forced into blocked state
%% when an disturbing block with timeout times out on an active server:
%% E.g. the ongoing requests are terminated
block_when_active_to2(M,H,P,N) ->
    Old = ?ETRAP_GET(),
    ?ETRAP_SET(true),
    ?DEBUG("block_when_active_to2 -> entry",[]),
    %% The block/unblock is a very crude way to make sure the listener
    %% has the right verbosity...
    ?DEBUG("block_when_active_to2 -> first block",[]),
    ?line block_server(N,H,P),
    ?DEBUG("block_when_active_to2 -> then unblock",[]),
    ?line unblock_server(N,H,P),
    ?DEBUG("block_when_active_to2 -> and now start the poll process",[]),
    ?line Poller = long_poll(M,H,P,N,"200",40000),
    ?DEBUG("block_when_active_to2 -> sleep some to make sure it's underway",[]),
    ?SLEEP(5000),
    ?DEBUG("block_when_active_to2 -> now do the disturbing block with timeout",[]),
    ?line Blocker = blocker(N,H,P,10000),
    ?DEBUG("block_when_active_to2 -> "
	"await completion of the blocker process",[]),
    ?line await_normal_process_exit(Blocker, "blocker", Old, 15000),
    ?DEBUG("block_when_active_to2 -> "
	"await completion of the poller process",[]),
    ?line await_suite_failed_process_exit(Poller, "poller", Old, 50000, 
					  connection_closed),
    ?ETRAP_SET(Old),
    ok.

blocker(N,H,P,Timeout) ->
    spawn_link(?MODULE,do_block_server,[N,H,P,Timeout]).

do_block_server(N,H,P,Timeout) ->
    ?line ok = block_server(N,H,P,Timeout),
    exit(normal).

	    
%% Tests that the non-disturbing block does not
%% disturb the ongoing request
nd_block_when_active1(M,H,P,N) ->
    Old = ?ETRAP_GET(),
    ?ETRAP_SET(true),
    ?DEBUG("nd_block_when_active1 -> entry",[]),
    %% The block/unblock is a very crude way to make sure the listener
    %% has the right verbosity...
    ?DEBUG("nd_block_when_active1 -> first block",[]),
    ?line ok = block_server(N,H,P),
    ?DEBUG("nd_block_when_active1 -> then unblock",[]),
    ?line unblock_server(N,H,P),
    ?DEBUG("nd_block_when_active1 -> and now start the poll process",[]),
    ?line Pid = long_poll(M,H,P,N,"200",60000),
    ?DEBUG("nd_block_when_active1 -> "
	"sleep some to make sure it's underway",[]),
    ?SLEEP(5000),
    ?DEBUG("nd_block_when_active1 -> now do the non-disturbing block",[]),
    ?line ok = block_nd_server(N,H,P,40000),
    ?DEBUG("nd_block_when_active1 -> await completion of the poll process",[]),
    ?line await_normal_process_exit(Pid, "poller", Old, 60000),
    ?ETRAP_SET(Old),
    ok.


%% Tests that the non-disturbing block times out and that 
%% the server retains the original state, e.g. unblocked
nd_block_when_active2(M,H,P,N) ->
    Old = ?ETRAP_GET(),
    ?ETRAP_SET(true),
    ?DEBUG("nd_block_when_active2 -> entry",[]),
    %% The block/unblock is a very crude way to make sure the listener
    %% has the right verbosity...
    ?DEBUG("nd_block_when_active2 -> first block",[]),
    ?line block_server(N,H,P),
    ?DEBUG("nd_block_when_active2 -> then unblock",[]),
    ?line unblock_server(N,H,P),
    ?DEBUG("nd_block_when_active2 -> and now start the poll process",[]),
    ?line Poller = long_poll(M,H,P,N,"200",45000),
    ?DEBUG("nd_block_when_active2 -> "
	"sleep some to make sure it's underway",[]),
    ?SLEEP(5000),
    ?DEBUG("nd_block_when_active2 -> "
	"now do the non-disturbing block which times out",[]),
    ?line Blocker = blocker_nd(N,H,P,10000,{error,timeout}),
    ?DEBUG("nd_block_when_active2 -> "
	"await completion of the block process", []),
    ?line await_normal_process_exit(Blocker, "blocker", Old, 15000),
    ?line unblocked = get_admin_state(N,H,P),
    ?DEBUG("nd_block_when_active2 -> await completion of the poll process",[]),
    ?line await_normal_process_exit(Poller, "poller", Old, 50000),
    ?ETRAP_SET(Old),
    ok.


%% Tests that the non-disturbing block is cancelled
%% when the blocker process dies
nd_block_blocker_crash1(M,H,P,N) ->
    Old = ?ETRAP_GET(),
    ?ETRAP_SET(true),
    ?DEBUG("nd_block_blocker_crash1 -> entry",[]),
    %% The block/unblock is a very crude way to make sure the listener
    %% has the right verbosity...
    ?DEBUG("nd_block_blocker_crash1 -> first block",[]),
    ?line ok = block_server(N,H,P),
    ?DEBUG("nd_block_blocker_crash1 -> then unblock",[]),
    ?line unblock_server(N,H,P),
    ?DEBUG("nd_block_blocker_crash1 -> and now start the poll process",[]),
    ?line Poller = long_poll(M,H,P,N,"200",60000),
    ?DEBUG("nd_block_blocker_crash1 -> "
	"sleep some to make sure the poll is underway",[]),
    ?SLEEP(5000),
    ?DEBUG("nd_block_blocker_crash1 -> "
	"now do the non-disturbing block",[]),
    ?line Blocker = blocker_nd(N,H,P,10000,ok),
    ?DEBUG("nd_block_blocker_crash1 -> "
	"sleep some to make sure the block is underway",[]),
    ?SLEEP(5000),
    ?DEBUG("nd_block_blocker_crash1 -> and now kill the blocker",[]),
    exit(Blocker,simulate_blocker_crash),
    ?DEBUG("nd_block_blocker_crash1 -> and now await poller completion",[]),
    ?line await_normal_process_exit(Poller, "poller", Old, 60000),
    ?ETRAP_SET(Old),
    ok.

%% Tests that the non-disturbing block is cancelled
%% when the blocker process dies
nd_block_blocker_crash2(M,H,P,N) ->
    Old = ?ETRAP_GET(),
    ?ETRAP_SET(true),
    ?DEBUG("nd_block_blocker_crash2 -> entry",[]),
    %% The block/unblock is a very crude way to make sure the listener
    %% has the right verbosity...
    ?DEBUG("nd_block_blocker_crash2 -> first block",[]),
    ?line ok = block_server(N,H,P),
    ?DEBUG("nd_block_blocker_crash2 -> then unblock",[]),
    ?line unblock_server(N,H,P),
    ?DEBUG("nd_block_blocker_crash2 -> and now start the poll process",[]),
    ?line Poller = long_poll(M,H,P,N,"200",60000),
    ?DEBUG("nd_block_blocker_crash2 -> "
	"sleep some to make sure the poll is underway",[]),
    ?SLEEP(5000),
    ?DEBUG("nd_block_blocker_crash2 -> "
	"now do the disturbing block with timeout",[]),
    ?line Blocker = blocker(N,H,P,10000),
    ?DEBUG("nd_block_blocker_crash2 -> "
	"sleep some to make sure the block is underway",[]),
    ?SLEEP(5000),
    ?DEBUG("nd_block_blocker_crash2 -> and now kill the blocker",[]),
    exit(Blocker,simulate_blocker_crash),
    ?DEBUG("nd_block_blocker_crash2 -> and now await poller completion",[]),
    ?line await_normal_process_exit(Poller, "poller", Old, 60000),
    ?ETRAP_SET(Old),
    ok.


await_suite_failed_process_exit(Pid, Name, Old, Timeout, Why) ->
    ?DEBUG("await_suite_failed_process_exit(~s) -> entry with"
	"~n   Why: ~p", [Name, Why]),
    receive 
	{'EXIT', Pid, {suite_failed, Why, _Mod, _Line}} ->
	    ?DEBUG("await_suite_failed_process_exit -> "
		"received expectede exit from ~s", [Name]),
	    ok;
	{'EXIT', Pid, Reason} ->
	    ?DEBUG("await_suite_failed_process_exit -> "
		"unexpected exit of ~s process: ~p", [Name, Reason]),
	    ?ETRAP_SET(Old),
	    Err = 
		lists:flatten(
		  io_lib:format("unexpected exit of ~s process: ~p",
				[Name, Reason])),
	    ?FAIL(Err)
    after Timeout ->
	    ?DEBUG("await_suite_failed_process_exit -> ERROR - ~s timeout",
		[Name]),
	    ?FLUSH(),
	    ?ETRAP_SET(Old),
	    ?FAIL("timeout while waiting for " ++ Name)
    end.
	  
	    
await_normal_process_exit(Pid, Name, Old, Timeout) ->
    receive
	{'EXIT', Pid, normal} ->
	    ?DEBUG("await_normal_process_exit -> "
		"normal exit of ~s => now check the state",[Name]),
	    ok;
	{'EXIT', Pid, Reason} ->
	    ?DEBUG("await_normal_process_exit -> ~s process failed:"
		"~n   Reason: ~p",[Name, Reason]),
	    Err = 
		lists:flatten(
		  io_lib:format("unexpected exit of ~s process: ~p",
				[Name, Reason])),
	    ?ETRAP_SET(Old),
	    ?FAIL(Err)
    after Timeout ->
	    ?DEBUG("await_normal_process_exit -> ERROR - ~s timeout",[Name]),
	    ?FLUSH(),
	    ?ETRAP_SET(Old),
	    ?FAIL("timeout while waiting for " ++ Name)
    end.

    
blocker_nd(N,H,P,Timeout,Reply) ->
    spawn_link(?MODULE,do_block_nd_server,[N,H,P,Timeout,Reply]).

do_block_nd_server(N,H,P,Timeout,Reply) ->
    ?line Reply = block_nd_server(N,H,P,Timeout),
    ?DEBUG("do_block_nd_server() -> received the expected reply (~p)",[Reply]),
    exit(normal).

long_poll(M,H,P,N,StatusCode,Timeout) ->
    spawn_link(?MODULE,do_long_poll,[M,H,P,N,StatusCode,Timeout]).

do_long_poll(M,H,P,N,StatusCode,Timeout) ->
    ?DEBUG("do_long_poll() -> do poll",[]),
    Mod  = "httpd_example",
    Func = "delay",
    Req  = lists:flatten(io_lib:format("GET /eval?" ++ Mod ++ ":" ++ Func ++ 
				       "(~p) HTTP/1.0\r\n\r\n",[30000])),
    ?line poll(M,H,P,N,Req,"200",Timeout),
    ?DEBUG("do_long_poll() -> poll done",[]),
    exit(normal).


simple_restart1(M,H,P,N) ->
    ?LOG("simple_restart1() -> restart server", []),
    ?line {error,_Reason} = restart_server(N,H,P).


simple_restart2(M,H,P,N) ->
    ?LOG("simple_restart2() -> "
	   "1) start: get the admin state, should be unblocked", []),
    ?line unblocked = get_admin_state(N,H,P),

    ?LOG("simple_restart2() -> 2) block server", []),
    ?line ok = block_server(N,H,P),

    ?LOG("simple_restart2() -> 3) restart server", []),
    ?line ok = restart_server(N,H,P),

    ?LOG("simple_restart2() -> 4) unblock server", []),
    ?line ok = unblock_server(N,H,P),

    ?LOG("simple_restart2() -> "
	 "5) get the admin state, should be unblocked", []),
    ?line unblocked = get_admin_state(N,H,P).


simple_restart3(M,H,P,N) ->
    ?LOG("simple_restart3() -> "
	   "1) start: get the admin state, should be unblocked", []),
    ?line unblocked = get_admin_state(N,H,P),

    ?LOG("simple_restart3() -> 2) block server", []),
    ?line ok = block_nd_server(N,H,P),

    ?LOG("simple_restart3() -> 3) restart server", []),
    ?line ok = restart_server(N,H,P),

    ?LOG("simple_restart3() -> 4) unblock server", []),
    ?line ok = unblock_server(N,H,P),

    ?LOG("simple_restart3() -> "
	 "5) get the admin state, should be unblocked", []),
    ?line unblocked = get_admin_state(N,H,P).


time_test(M, H, P) ->
    ?LOG("time_test() -> start", []),
    ?line ok = httpd_time_test:t(M, H, P).


%%
%% all modules
%%

all_modules(M, H, P, N) ->
    mod_actions(M,H,P,N),
    mod_alias(M,H,P,N),
    mod_auth(M,H,P,N),
    mod_cgi(M,H,P,N),
    mod_esi(M,H,P,N),
    mod_get(M,H,P,N),
    mod_head(M,H,P,N).

%% mod_actions
%%
%% TODO: Test the 'Action' directive.
mod_actions(M,H,P,N) ->
    %% Check "Script" directive
    ?line poll(M,H,P,N,"HEAD / HTTP/1.0\r\n\r\n", "200").
    
%% mod_alias
%%
%% TODO: Test the DirectoryIndex directive more extensively.
mod_alias(M, H, P, N) ->
    %% Chech "Alias" directive
    ?line e_poll(M,H,P,N, "GET /pics/blahonga_bild.gif HTTP/1.0\r\n\r\n",
		 [{statuscode, 404}]),
    ?line e_poll(M,H,P,N, "GET /pics/icon.sheet.gif HTTP/1.0\r\n\r\n",
		 [{statuscode, 200},
		  {header, 'Content-Type',"image/gif"},
		  {header, 'Server'},
		  {header, 'Date'}]),
    %% Check "DirectoryIndex" directive
    ?line e_poll(M,H,P,N, "GET / HTTP/1.0\r\n\r\n",
		 [{statuscode, 200},
		  {header, 'Content-Type',"text/html"},
		  {header, 'Server'},
		  {header, 'Date'}]),
    ?line e_poll(M,H,P,N, "GET /misc/ HTTP/1.0\r\n\r\n",
		 [{statuscode, 200},
		  {header, 'Content-Type',"text/html"},
		  {header, 'Server'},
		  {header, 'Date'}]),
    %% Check redirection if trailing slash is missing.
    ?line e_poll(M,H,P,M, "GET /misc HTTP/1.0\r\n\r\n",
		 [{statuscode, 301},
		  {header, 'Location'},
		  {header, 'Content-Type', "text/html"}]).    


%% mod_security
mod_security(Config, M,H,P, N) ->
    global:register_name(mod_security_test, self()),   % Receive events
    Port = ?CONFIG(port, Config),
    SR = filename:join([filename:absname(?CONFIG(data_dir,Config)), "server_root"]),
    Node = ?CONFIG(nodename, Config),

    OpenDir = filename:join([SR, "htdocs", "open"]),

    %% Test blocking / unblocking of users.

    %% /open, require user one Aladdin
    ?DEBUG("mod_security -> access to directory '/open': remove all users",[]),
    ?line remove_users(Node, SR, H, P, "open"),

    ?DEBUG("mod_security -> authenticate unknown user 'one'",[]),
    ?line auth_request(M,H,P,N,"/open/", "one", "onePassword", 401),
    ?line receive_security_event({event, auth_fail, Port, OpenDir,
				  [{user, "one"}, {password, "onePassword"}]},N,P),

    ?DEBUG("mod_security -> authenticate unknown user 'two'",[]),
    ?line auth_request(M,H,P,N,"/open/", "two", "twoPassword", 401),
    ?line receive_security_event({event, auth_fail, Port, OpenDir,
				  [{user, "two"}, {password, "twoPassword"}]},N,P),

    ?DEBUG("mod_security -> authenticate unknown user 'Aladdin'",[]),
    ?line auth_request(M,H,P,N,"/open/", "Aladdin", "AladdinPassword", 401),
    ?line receive_security_event({event, auth_fail, Port, OpenDir,
				  [{user, "Aladdin"},{password, "AladdinPassword"}]},N,P),

    ?DEBUG("mod_security -> add users 'one' and 'two'",[]),
    ?line add_user(Node, SR, P, "open", "one", "onePassword", []),
    ?line add_user(Node, SR, P, "open", "two", "twoPassword", []),

    ?DEBUG("mod_security -> authenticate user 'one' with wrong passwd",[]),
    ?line auth_request(M,H,P,N,"/open/", "one", "WrongPassword", 401),
    ?line receive_security_event({event, auth_fail, Port, OpenDir,
				  [{user, "one"}, {password, "WrongPassword"}]},N,P),
 
    ?DEBUG("mod_security -> authenticate user 'one' with wrong passwd again",[]),
    ?line auth_request(M,H,P,N,"/open/", "one", "WrongPassword", 401),
    ?line receive_security_event({event, auth_fail, Port, OpenDir,
				  [{user, "one"}, {password, "WrongPassword"}]},N,P),
    ?DEBUG("mod_security -> await block for user 'one'",[]),
    ?line receive_security_event({event, user_block, Port, OpenDir,
				  [{user, "one"}]},N,P),

    ?DEBUG("mod_security -> unregister mod_security client",[]),
    global:unregister_name(mod_security_test),   % No more events.

    ?DEBUG("mod_security -> authenticate user 'one' (should become blocked) "
	   "with wrong passwd",[]),
    ?line auth_request(M,H,P,N,"/open/", "one", "WrongPassword", 401),
    ?DEBUG("mod_security -> authenticate user 'one' (=> block)",[]),
    ?line auth_request(M,H,P,N,"/open/", "one", "onePassword", 403),

    %% User "one" should be blocked now..
    ?DEBUG("mod_security -> list blocked users: should be 'one'",[]),
    ?line [{"one",_, Port, OpenDir,_}] = list_blocked_users(Node,Port),
    ?line [{"one",_, Port, OpenDir,_}] = list_blocked_users(Node,Port,OpenDir),

    ?DEBUG("mod_security -> unblock user 'one'",[]),
    ?line true = unblock_user(Node, "one", Port, OpenDir),
    %% User "one" should not be blocked any more..
    ?DEBUG("mod_security -> list blocked users: should be none",[]),
    ?line [] = list_blocked_users(Node, Port),
    ?line [] = list_blocked_users(Node, Port, OpenDir),
    ?DEBUG("mod_security -> authenticate user 'one'",[]),
    ?line auth_request(M,H,P,N,"/open/", "one", "onePassword", 200),

    %% Test list_auth_users & auth_timeout
    ?DEBUG("mod_security -> test list_auth_users & auth_timeout",[]),
    ?line ["one"] = list_auth_users(Node, Port),
    ?line ["one"] = list_auth_users(Node, Port, OpenDir),
    ?line auth_request(M,H,P,N,"/open/", "two", "onePassword", 401),
    ?line ["one"] = list_auth_users(Node, Port),
    ?line ["one"] = list_auth_users(Node, Port, OpenDir),
    ?line auth_request(M,H,P,N,"/open/", "two", "twoPassword", 401),
    ?line ["one"] = list_auth_users(Node, Port),
    ?line ["one"] = list_auth_users(Node, Port, OpenDir),
    ?SLEEP(?auth_timeout*1001),  % Wait for successful auth to timeout.
    ?line [] = list_auth_users(Node, Port),
    ?line [] = list_auth_users(Node, Port, OpenDir),
    %% "two" is blocked.
    ?line true = unblock_user(Node, "two", Port, OpenDir),
    %% Test explicit blocking. Block user 'two'.
    ?line [] = list_blocked_users(Node,Port,OpenDir),
    ?line true = block_user(Node, "two", Port, OpenDir, 10),
    ?line auth_request(M,H,P,N,"/open/", "two", "twoPassword", 401),
    ok.

event(What, Port, Dir, Data) ->
    ?DEBUG("event -> ~n"
	   "      What = ~p~n"
	   "      Data = ~p", 
	   [What,Data]),
    Msg = {event, What, Port, Dir, Data},
    Str = lists:flatten(io_lib:format("Security event occured: ~p", [Msg])),
    case global:whereis_name(mod_security_test) of
	undefined ->
	    ?DEBUG("event -> IGNORE: ~s", [Str]);
	Pid ->
	    ?LOG("event -> Str: ~s", [Str]),
	    ?DEBUG("event -> send to: ~p", [Pid]),
	    global:send(mod_security_test, Msg)
    end.

receive_security_event(Event,N,P) ->
    ?DEBUG("receive_security_event -> await event: ~n"
	   "        ~p",[Event]),
    receive 
	Event ->
	    ?LOG("receive_security_event -> received expected event",[]),
	    ok;
	{'EXIT',Pid,Reason} ->
	   ?DEBUG("receive_security_event -> ~n"
		  "        received exit signal from '~p': ~p",[Pid,Reason]),
	    receive_security_event(Event,N,P);
	
	%{_,{socket_closed,normal}} ->
	%    receive_security_event(Event,N,P);
	Other ->
	    ?INFO("receive_security_event ->\n"
		  "        received unexpected event: ~p~n"
		  "                 expected event:   ~p", 
		  [Other,Event]),
	    ?HTTPD_STATUS("http server status after uknown event: ",
			  httpd_status(N,P)),
	    ?FAIL("Wrong message received")
    after 5000 ->
	    ?INFO("Event receive timeout: expected ~p", [Event]),
	    ?HTTPD_STATUS("http server status after timeout: ",
			  httpd_status(N,P)),
	    ?FAIL("Did not receive event.")
    end.

%% M = Mode
%% H = Host
%% P = Port
%% N = Node
mod_htaccess(M, H, P, N) ->
    %%----------------------------------------------------------------------
    %% Control that authentication required!
    %% Control that the pages that shall be 
    %% authenticated really need authenticatin
    ?line e_poll(M,H,P,N, "GET /ht/open/ HTTP/1.0\r\n\r\n",
		 [{statuscode, 401},
		  {header, 'WWW-Authenticate'}]),
    ?line e_poll(M,H,P,N, "GET /ht/secret/ HTTP/1.0\r\n\r\n",
		 [{statuscode, 401},
		  {header, 'WWW-Authenticate'}]),
    ?line e_poll(M,H,P,N, "GET /ht/secret/top_secret/ HTTP/1.0\r\n\r\n",
		 [{statuscode, 401},
		  {header, 'WWW-Authenticate'}]),

    %%----------------------------------------------------------------------
    %% Control that not just the first user iin the list is valid

    %% Control the first user
    %% Authennticating ["one:OnePassword" user first in user list]
    ?line auth_request(M,H,P,N, "/ht/open/dummy.html", "one", "OnePassword", 200),
    %% Control the second user
    %% Authentication OK and a directory listing is supplied! 
    %% ["Aladdin:open sesame" user second in user list]
    ?line auth_request(M,H,P,N, "/ht/open/","Aladdin", "AladdinPassword", 200),
    
    %%----------------------------------------------------------------------
    %% Contro that bad passwords and userids get a good denial
    %% User correct but wrong password! ["one:one" user first in user list]
    ?line auth_request(M,H,P,N, "/ht/open/", "one", "one", 401),
    %% Neither user or password correct! ["dummy:dummy"]
    ?line auth_request(M,H,P,N, "/ht/open/", "dummy", "dummy", 401),
    
    %%----------------------------------------------------------------------
    %% Control that authetication still works, even if its a member in a group
    %% Authentication OK! ["two:TwoPassword" user in first group]
    ?line auth_request(M,H,P,N, "/ht/secret/dummy.html", "two", "TwoPassword", 200),
    %% Authentication OK and a directory listing is supplied! 
    %% ["three:ThreePassword" user in second group]
    ?line auth_request(M,H,P,N,"/ht/secret/", "three", "ThreePassword", 200),
    
    %%----------------------------------------------------------------------
    %% Deny users with bad passwords even if the user is a group member
    %% User correct but wrong password! ["two:two" user in first group]
    ?line auth_request(M,H,P,N, "/ht/secret/", "two", "two", 401),
    %% Neither user or password correct! ["dummy:dummy"]
    ?line auth_request(M,H,P,N,"/ht/secret/", "dummy", "dummy", 401),
    %%---------------------------------------------------------------------
    %% control that we deny the users that are in subnet above the allowed
    ?line auth_request(M,H,P,N,"/ht/blocknet/dummy.html", "four", "FourPassword", 403),
    %% Control that we only applies the rules to the right methods
    ?line poll(M,H,P,N, "HEAD /ht/blocknet/dummy.html HTTP/1.0\r\n\r\n","200"),
    %%----------------------------------------------------------------------
    %% Control that the rerquire directive can be overrideen
    ?line auth_request(M,H,P,N, "/ht/secret/top_secret/", "Aladdin", "AladdinPassword", 401),
    
    %% Authentication still required!
    ?line e_poll(M,H,P,N, "GET /ht/open/ HTTP/1.0\r\n\r\n",
		 [{statuscode, 401},
		  {header, 'WWW-Authenticate'}]),
    ?line e_poll(M,H,P,N, "GET /ht/secret/ HTTP/1.0\r\n\r\n",
		 [{statuscode, 401},
		  {header, 'WWW-Authenticate'}]),
    ?line e_poll(M,H,P,N, "GET /ht/secret/top_secret/ HTTP/1.0\r\n\r\n",
		 [{statuscode, 401},
		  {header, 'WWW-Authenticate'}]).

%% mod_auth
mod_auth(M, H, P, N) ->
    %% Authentication required!
    ?line e_poll(M,H,P,N, "GET /open/ HTTP/1.0\r\n\r\n",
		 [{statuscode, 401},
		  {header, 'WWW-Authenticate'}]),
    ?line e_poll(M,H,P,N, "GET /secret/ HTTP/1.0\r\n\r\n",
		 [{statuscode, 401},
		  {header, 'WWW-Authenticate'}]),
    ?line e_poll(M,H,P,N, "GET /secret/top_secret/ HTTP/1.0\r\n\r\n",
		 [{statuscode, 401},
		  {header, 'WWW-Authenticate'}]),
    %% Authentication OK! ["one:OnePassword" user first in user list]
    ?line auth_request(M,H,P,N, "/open/dummy.html", "one", "onePassword", 200),
    %% Authentication OK and a directory listing is supplied! ["Aladdin:open sesame" user second in user list]
    ?line auth_request(M,H,P,N, "/open/","Aladdin", "AladdinPassword", 200),
    %% User correct but wrong password! ["one:one" user first in user list]
    ?line auth_request(M,H,P,N, "/open/", "one", "one", 401),
    %% Neither user or password correct! ["dummy:dummy"]
    ?line auth_request(M,H,P,N, "/open/", "dummy", "dummy", 401),
    %% Authentication OK! ["two:TwoPassword" user in first group]
    ?line auth_request(M,H,P,N, "/secret/dummy.html", "two", "twoPassword", 200),
    %% Authentication OK and a directory listing is supplied! ["three:ThreePassword" user in second group]
    ?line auth_request(M,H,P,N,"/secret/", "three", "threePassword", 200),
    %% User correct but wrong password! ["two:two" user in first group]
    ?line auth_request(M,H,P,N, "/secret/", "two", "two", 401),
    %% Neither user or password correct! ["dummy:dummy"]
    ?line auth_request(M,H,P,N,"/secret/", "dummy", "dummy", 401),
    %% Nested secret/top_secret OK! ["Aladdin:open sesame"]
    ?line auth_request(M,H,P,N, "/secret/top_secret/", "Aladdin", "AladdinPassword", 200),
    %% Authentication still required!
    ?line e_poll(M,H,P,N, "GET /open/ HTTP/1.0\r\n\r\n",
		 [{statuscode, 401},
		  {header, 'WWW-Authenticate'}]),
    ?line e_poll(M,H,P,N, "GET /secret/ HTTP/1.0\r\n\r\n",
		 [{statuscode, 401},
		  {header, 'WWW-Authenticate'}]),
    ?line e_poll(M,H,P,N, "GET /secret/top_secret/ HTTP/1.0\r\n\r\n",
		 [{statuscode, 401},
		  {header, 'WWW-Authenticate'}]).


mod_auth_mnesia_api(Config, M, H, P, N) ->
    %% Create three groups:
    %% group1 : one Aladdin
    %% group2 : two
    %% group3 : three
    ?line mod_auth_mnesia:store_user("one", "onePassword", P, "/mnesia_open", ""),
    ?line mod_auth_mnesia:store_user("Aladdin", "AladdinPassword", P, "/mnesia_open", ""),
    ?line mod_auth_mnesia:store_user("two", "twoPassword", P, "/mnesia_open", ""),
    ?line mod_auth_mnesia:store_user("three", "threePassword", P, "/mnesia_open", ""),
    ?line Users = mod_auth_mnesia:list_users(P, "/mnesia_open"),
    ?DEBUG("Users: ~p~n", [Users]),
    ?line ok = check_lists_members(Users,["Aladdin","one","two","three"]),
    %% ?line {ok,["Aladdin","one","two","three"]} = Users,
    

    ?line true = mod_auth_mnesia:store_group_member("group1", "one", P, "/mnesia_open", ""),
    ?line true = mod_auth_mnesia:store_group_member("group1","Aladdin", P, "/mnesia_open", ""),
    ?line true = mod_auth_mnesia:store_group_member("group2","two", P, "/mnesia_open", ""),
    ?line true = mod_auth_mnesia:store_group_member("group3","three", P, "/mnesia_open", ""),
    %% Check that all three created groups exist.
    ?line Groups = mod_auth_mnesia:list_groups(P, "/mnesia_open"),
    ?DEBUG("Groups: ~p~n", [Groups]),
    ?line ok = check_lists_members(Groups,["group1","group2","group3"]),
    %% ?line {ok,["group1","group2","group3"]} = Groups,

    %% Check that the members of all groups are correct.
    ?line Group1 = mod_auth_mnesia:list_group_members("group1", P, "/mnesia_open"),
    ?DEBUG("Group1: ~p~n", [Group1]),
    ?line ok = check_lists_members(Group1,["one","Aladdin"]),
    %% ?line {ok,["one","Aladdin"]} = Group1,
    ?line Group2 = mod_auth_mnesia:list_group_members("group2", P, "/mnesia_open"),
    ?DEBUG("Group2: ~p~n", [Group2]),
    ?line {ok,["two"]} = Group2,
    ?line Group3 = mod_auth_mnesia:list_group_members("group3", P, "/mnesia_open"),
    ?DEBUG("Group3: ~p~n", [Group3]),
    ?line {ok,["three"]} = Group3,
    
    %% Delete user 'one' from group one and check that he was removed correctly.
    ?line true = mod_auth_mnesia:remove_group_member("group1", "one", P, "/mnesia_open", ""),
    ?line Group1_1 = mod_auth_mnesia:list_group_members("group1", P, "/mnesia_open"),
    ?DEBUG("Group1_1: ~p~n", [Group1_1]),
    ?line {ok,["Aladdin"]} = Group1_1,

    %% Remove group1 and check that the group was removed correctly.
    ?line true = mod_auth_mnesia:remove_group("group1", P, "/mnesia_open", ""),
    ?line Groups_1 = mod_auth_mnesia:list_groups(P, "/mnesia_open"),
    ?DEBUG("Groups_1: ~p~n", [Groups_1]),
    ?line ok = check_lists_members(Groups_1,["group2","group3"]),
    %% ?line {ok,["group2","group3"]} = Groups_1,

    %% Check that the other users still exist in their groups.
    ?line Users_1 = mod_auth_mnesia:list_users(P, "/mnesia_open"),
    ?DEBUG("Users_1: ~p~n", [Users_1]),
    ?line ok = check_lists_members(Users_1,["Aladdin","one","two","three"]),
    %% ?line {ok,["Aladdin","one","two","three"]} = Users_1,
    ?line Group2_1 = mod_auth_mnesia:list_group_members("group2", P, "/mnesia_open"),
    ?DEBUG("Group2_1: ~p~n", [Group2_1]),
    ?line {ok,["two"]} = Group2_1,
    ?line Group3_1 = mod_auth_mnesia:list_group_members("group3", P, "/mnesia_open"),
    ?DEBUG("Group3_1: ~p~n", [Group3_1]),
    ?line {ok,["three"]} = Group3_1,

    %% Remove the remaining groups/users and check that all users/groups are removed.
    ?line true = mod_auth_mnesia:remove_group("group2", P, "/mnesia_open", ""),
    ?line true = mod_auth_mnesia:remove_group("group3", P, "/mnesia_open", ""),
    ?line {ok, []} = mod_auth_mnesia:list_groups(P, "/mnesia_open"),
    ?line true = mod_auth_mnesia:remove_user("one", P, "/mnesia_open", ""),
    ?line true = mod_auth_mnesia:remove_user("Aladdin", P, "/mnesia_open", ""),
    ?line true = mod_auth_mnesia:remove_user("two", P, "/mnesia_open", ""),
    ?line true = mod_auth_mnesia:remove_user("three", P, "/mnesia_open", ""),
    ?line {ok, []} = mod_auth_mnesia:list_users(P, "/mnesia_open"),
    ok.

%%
%% What to test here:
%%
%% /open                      - plain,  require user one Aladdin
%% /secret                    - plain,  require group group1 group2
%% /secret/top_secret         - plain,  require group group3
%% /dets_open                 - dets,   require user one Aladdin
%% /dets_secret               - dets,   require group group1 group2
%% /dets_secret/top_secret    - dets,   require group group3
%% /mnesia_open/              - mnesia, require user one Aladdin
%% /mnesia_secret/            - mnesia, require group group1 group2
%% /mnesia_secret/top_secret/ - mnesia, require group group3
mod_auth_api(Config, M, H, P, N) ->
    test_auth_api(Config, M,H,P,N, ""),
    test_auth_api(Config, M,H,P,N, "dets_"),
    test_auth_api(Config, M,H,P,N, "mnesia_"),
    ok.

%% group1: one Aladdin
%% group2: two
%% group3: three
test_auth_api(Config, M,H,P,N, Type) ->
    ?LOG("API test for '~s'", [Type]),
    SR = filename:join(
	   [filename:absname(?CONFIG(data_dir,Config)), "server_root"]),
    Node = ?CONFIG(nodename, Config),
    %%Address = ?CONFIG(address,Config),

    ?line poll(M,H,P,N, "GET / HTTP/1.0\r\n\r\n", "200"),
    ?line auth_request(M,H,P,N, "/", "one", "WrongPassword", 200),

    %%Change the password to DummyPassword then try to add a user 
    %%Get an error and set it to NoPassword
    ?line ok=update_password(Node,SR,H,P,Type++"open","NoPassword","DummyPassword"),
    ?line {error,bad_password} = add_user(Node, SR, P, Type++"open", "one", "onePassword", []),
    ?line ok=update_password(Node,SR,H,P,Type++"open","DummyPassword","NoPassword"),
    %% Test /*open, require user one Aladdin
    ?line remove_users(Node, SR, H, P, Type++"open"),
    ?line auth_request(M,H,P,N,"/"++Type++"open/", "one", "onePassword", 401),
    ?line auth_request(M,H,P,N,"/"++Type++"open/", "two", "twoPassword", 401),
    ?line auth_request(M,H,P,N,"/"++Type++"open/", "Aladdin", "onePassword", 401),
    ?line add_user(Node, SR, P, Type++"open", "one", "onePassword", []),
    ?line add_user(Node, SR, P, Type++"open", "two", "twoPassword", []),
    ?line add_user(Node, SR, P, Type++"open", "Aladdin", "AladdinPassword", []),
    ?DEBUG("list_users (open): ~p~n", 
	   [list_users(Node, SR, H, P, Type++"open")]),
    ?line auth_request(M,H,P,N, "/"++Type++"open/", "one", "WrongPassword", 401),
    ?line auth_request(M,H,P,N,"/"++Type++"open/", "one", "onePassword", 200),
    ?line auth_request(M,H,P,N,"/"++Type++"open/", "two", "twoPassword", 401),
    ?line auth_request(M,H,P,N, "/"++Type++"open/", "Aladdin", "WrongPassword", 401),
    ?line auth_request(M,H,P,N,"/"++Type++"open/", "Aladdin", "AladdinPassword", 200),
    ?line remove_users(Node, SR, H, P, Type++"open"),
    ?line {ok, []} = list_users(Node, SR, H, P, Type++"open"),

    %% -- -  -

    remove_users(Node, SR, H, P, Type++"secret"),
    ?line {ok, []} = list_users(Node, SR, H, P, Type++"secret"),
    ?line auth_request(M,H,P,N,"/"++Type++"secret/", "one", "onePassword", 401),
    ?line auth_request(M,H,P,N,"/"++Type++"secret/", "two", "twoPassword", 401),
    ?line auth_request(M,H,P,N,"/"++Type++"secret/", "three", "threePassword", 401),
    ?line add_user(Node, SR, P, Type++"secret", "one", "onePassword", []),
    ?line add_user(Node, SR, P, Type++"secret", "two", "twoPassword", []),
    ?line add_user(Node, SR, P, Type++"secret", "Aladdin", "AladdinPassword",[]),
    ?line add_group_member(Node, SR, P, Type++"secret", "one", "group1"),
    ?line add_group_member(Node, SR, P, Type++"secret", "two", "group1"),
    ?line add_group_member(Node, SR, P, Type++"secret", "Aladdin", "group2"),
    ?line ?LOG("list_group_members (group1): ~p~n", 
	       [list_group_members(Node, SR, P, Type++"secret", "group1")]),
    ?line ?LOG("list_group_members (group2): ~p~n", 
	       [list_group_members(Node, SR, P, Type++"secret", "group2")]),
    ?line auth_request(M,H,P,N,"/"++Type++"secret/", "one", "onePassword", 200),
    ?line auth_request(M,H,P,N,"/"++Type++"secret/", "two", "twoPassword", 200), % -
    ?line auth_request(M,H,P,N,"/"++Type++"secret/", "Aladdin", "AladdinPassword", 200),
    ?line auth_request(M,H,P,N,"/"++Type++"secret/", "three", "threePassword", 401),
    ?line remove_users(Node, SR, H, P, Type++"secret"),
    ?line {ok, []} = list_users(Node, SR, H, P, Type++"secret"),
    ?line remove_groups(Node, SR, H, P, Type++"secret"),
    ?line Directory = filename:join([SR, "htdocs", Type++"secret"]),
    ?line {ok, []} = list_groups(Node, SR, H, P, Directory),

    %% -- -  -

    remove_users(Node, SR, H, P, Type++"secret/top_secret"),
    remove_groups(Node, SR, H, P, Type++"secret/top_secret"),
    ?line auth_request(M,H,P,N,"/"++Type++"secret/top_secret/","three","threePassword",401),
    ?line auth_request(M,H,P,N,"/"++Type++"secret/top_secret/", "two", "twoPassword", 401),
    ?line add_user(Node, SR, P, Type++"secret/top_secret","three","threePassword",[]),
    ?line add_user(Node, SR, P, Type++"secret/top_secret","two","twoPassword",[]),
    ?line add_group_member(Node, SR, P, Type++"secret/top_secret", "three", "group3"),
    ?line auth_request(M,H,P,N,"/"++Type++"secret/top_secret/","three","threePassword",200),
    ?line auth_request(M,H,P,N,"/"++Type++"secret/top_secret/", "two", "twoPassword", 401),
    ?line add_group_member(Node, SR, P, Type++"secret/top_secret", "two", "group3"),
    ?line auth_request(M,H,P,N,"/"++Type++"secret/top_secret/", "two", "twoPassword", 200),
    
    ?line remove_users(Node, SR, H, P, Type++"secret/top_secret"),
    ?line {ok, []} = list_users(Node, SR, H, P, Type++"secret/top_secret"),
    ?line remove_groups(Node, SR, H, P, Type++"secret/top_secret"),
    ?line Directory2 = filename:join([SR, "htdocs", Type++"secret/top_secret"]),
    ?line {ok, []} = list_groups(Node, SR, H, P, Directory2),
    ?line auth_request(M,H,P,N,"/"++Type++"secret/top_secret/", "two", "twoPassword", 401),
    ?line auth_request(M,H,P,N,"/"++Type++"secret/top_secret/","three","threePassword",401),
    ok.

update_password(Node,SR,Address,P,Dir,Old,New)->
    Directory=filename:join([SR,"htdocs",Dir]),
    rpc:call(Node,mod_auth,update_password,[undefined,P,Directory,Old,New,New]).

remove_groups(Node, SR, H, P, Dir) ->
    Directory = filename:join([SR, "htdocs", Dir]),
    {ok, Groups} = list_groups(Node, SR, H, P, Directory),
    ?line lists:foreach(fun(X) ->
				delete_group(Node, X, P, Directory)
			end,
			Groups),
    ?line {ok, []} = list_groups(Node, SR, H, P, Directory),
    ok.

delete_group(Node, Group, P, Dir) ->
    Addr = undefined, 
    rpc:call(Node, mod_auth, delete_group, [Group, Addr, P, Dir]).

list_groups(Node, SR, H, P, Dir) ->
    Addr = undefined, 
    rpc:call(Node, mod_auth, list_groups, [Addr, P, Dir]).

add_group_member(Node, SR, P, Dir, User, Group) ->
    Addr = undefined, 
    rpc:call(Node, mod_auth, add_group_member, [Group, User, Addr, P, 
						filename:join(
						  [SR,"htdocs",Dir])]).
-ifdef(inets_log).
list_group_members(Node, SR, P, Dir, Group) ->
    Addr = undefined, 
    rpc:call(Node, mod_auth, list_group_members, [Group, Addr, P, 
						  filename:join(
						    [SR, "htdocs",Dir])]).
-endif.

remove_users(Node, SR, H, P, Dir) ->
    ?DEBUG("remove_users -> Dir: ~p", [Dir]),
    %% List users, delete them, and make sure they are gone.
    ?line case list_users(Node, SR, H, P, Dir) of
	      {ok, Users} ->
		  ?DEBUG("remove_users -> Users: ~p", [Users]),
		  ?line lists:foreach(fun(X) -> 
					      delete_user(Node, SR, H, 
							  P, Dir, X)
				      end,
				      Users),
		  ?line {ok, []} = list_users(Node, SR, H, P, Dir);
	      _ ->
		  ?DEBUG("remove_users -> no users", []),
		  ok
	  end.

add_user(Node, Root, Port, Dir, User, Password, UserData) ->
    ?DEBUG("add_user -> User: ~s", [User]),
    Addr = undefined, 
    Directory = filename:join([Root, "htdocs", Dir]),
    rpc:call(Node, mod_auth, add_user, [User,Password,UserData,Addr,Port,Directory]).

delete_user(Node, Root, Host, Port, Dir, User) ->
    ?DEBUG("delete_users -> User: ~s", [User]),
    Addr = undefined, 
    Directory = filename:join([Root, "htdocs", Dir]),
    rpc:call(Node, mod_auth, delete_user, [User, Addr, Port, Directory]).

list_users(Node, Root, Host, Port, Dir) ->
    ?DEBUG("list_users -> Dir: ~s", [Dir]),
    Addr = undefined, 
    Directory = filename:join([Root, "htdocs", Dir]),
    rpc:call(Node, mod_auth, list_users, [Addr, Port, Directory]).

restart_server(Node,_Host,Port) ->
    Addr = undefined, 
    rpc:call(Node, httpd, restart, [Addr, Port]).

% restart_server(Node, _Host, Port, Mode) ->
%     Addr = undefined, 
%     rpc:call(Node, httpd, restart, [Addr, Port, Mode]).

% restart_server(Node, _Host, Port, Mode, Timeout) ->
%     Addr = undefined, 
%     rpc:call(Node, httpd, restart, [Addr, Port, Mode, Timeout]).

block_server(Node,_Host,Port) ->
    Addr = undefined, 
    rpc:call(Node, httpd, block, [Addr, Port]).

block_server(Node,_Host,Port,Timeout) ->
    Addr = undefined, 
    rpc:call(Node, httpd, block, [Addr, Port, disturbing, Timeout]).

block_nd_server(Node,_Host,Port) ->
    Addr = undefined, 
    rpc:call(Node, httpd, block, [Addr, Port, non_disturbing]).

block_nd_server(Node,_Host,Port,Timeout) ->
    Addr = undefined, 
    rpc:call(Node, httpd, block, [Addr, Port, non_disturbing, Timeout]).

unblock_server(Node,_Host,Port) ->
    Addr = undefined, 
    rpc:call(Node, httpd, unblock, [Addr, Port]).

get_admin_state(Node,_Host,Port) ->
    Addr = undefined, 
    rpc:call(Node, httpd, get_admin_state, [Addr, Port]).

get_usage_state(Node,_Host,Port) ->
    Addr = undefined, 
    rpc:call(Node, httpd, get_usage_state, [Addr, Port]).

%% mod_auth_mnesia1 (DEBUG: erl -sname a -mnesia dir '"/tmp/AuthMnesiaDir"')

-record(httpd_user, {user_name, password, user_data}).
-record(httpd_group,{group_name, userlist}).

setup_mnesia() ->
  setup_mnesia([node()]).

setup_mnesia(Nodes) ->
    mnesia:create_schema(Nodes),
    mnesia:start(),
    mnesia:create_table(httpd_user,[{attributes, record_info(fields, httpd_user)}, {disc_copies,Nodes}, {type, set}]),
    mnesia:create_table(httpd_group,[{attributes, record_info(fields, httpd_group)}, {disc_copies,Nodes}, {type,bag}]).

cleanup_mnesia() ->
    ok  = mnesia:start(),
    mnesia:delete_table(httpd_user),
    mnesia:delete_table(httpd_group),
    stopped = mnesia:stop(),
    mnesia:delete_schema([node()]).

%% mod_cgi

mod_cgi(M,H,P,N) ->
    ?LOG("mod_cgi() -> ~p, ~p, ~p", [M,H,P]),
    do_mod_cgi(skip_cgi(),M,H,P,N).

skip_cgi() -> skip_cgi(os:type()).

skip_cgi(vxworks) -> skip;
skip_cgi(_)       -> dont_skip.


do_mod_cgi(skip,_,_,_,_) ->
    ?LOG("do_mod_cgi() -> skip test",[]),
    ?SKIP("cgi does not work on this platform");
do_mod_cgi(_,M,H,P,N) ->
    ?LOG("do_mod_cgi() -> do cgi test",[]),
    {PE, PE2} =
	case os:type() of
	    {win32, _} ->
		{"printenv.bat", "printenv.sh"};
	    _ ->
		{"printenv.sh", "printenv.bat"}
	end,

    %% The length (> 100) is intentional
    ?LOG("do_mod_cgi() -> do the loooong POST",[]),
    ?line cpoll(M,H,P,N,"POST /cgi-bin/"++PE++" HTTP/1.0\r\n"
		"Content-Length:100 \r\n\r\n "
		"ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
		"ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
		"ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
		"ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
		"ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
		"ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
		"ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
		"ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
		"ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
		"ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
		"ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
		"ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
		"ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
		"ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
		"ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
		"ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
		"ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ"
		" \r\n\r\n","200"),
    ?LOG("do_mod_cgi() -> loooong POST done",[]),

    ?line poll(M,H,P,N,"GET /cgi-bin/"++PE++" HTTP/1.0\r\n\r\n","200"),
    ?line poll(M,H,P,N,"GET /cgi-bin/not_there HTTP/1.0\r\n\r\n",["404","500"]),
    ?line poll(M,H,P,N,"GET /cgi-bin/"++PE++"?Nisse:kkk?sss/lll HTTP/1.0\r\n\r\n","200"),

    ?line poll(M,H,P,N,"POST /cgi-bin/"++PE++" HTTP/1.0\r\n\r\n","200"),
    ?line poll(M,H,P,N,"GET /htbin/"++PE++" HTTP/1.0\r\n\r\n","200"),
    ?line poll(M,H,P,N,"GET /htbin/not_there HTTP/1.0\r\n\r\n",["404","500"]),
    ?line poll(M,H,P,N,"GET /htbin/"++PE++"?Nisse:kkk?sss/lll HTTP/1.0\r\n\r\n","200"),
    ?line poll(M,H,P,N,"POST /htbin/"++PE++" HTTP/1.0\r\n\r\n","200"),
    %% Execute an existing, but bad CGI script..
    ?line poll(M,H,P,N,"POST /htbin/"++PE2++" HTTP/1.0\r\n\r\n","404"),
    ?line poll(M,H,P,N,"POST /cgi-bin/"++PE2++" HTTP/1.0\r\n\r\n","404"),
    ok.

%% mod_esi

mod_esi(M,H,P,N) ->
    %% Check "ErlScriptAlias" and "EvalScriptAlias" directives
    ?line poll(M,H,P,N,"GET /eval?httpd_example:print(\"Hi!\") HTTP/1.0\r\n\r\n","200"),
    ?line poll(M,H,P,N,"GET /eval?not_allowed:print(\"Hi!\") HTTP/1.0\r\n\r\n","403"),
    ?line poll(M,H,P,N,"GET /eval?httpd_example:undef(\"Hi!\") HTTP/1.0\r\n\r\n","500"),
    ?line poll(M,H,P,N,"GET /cgi-bin/erl/httpd_example HTTP/1.0\r\n\r\n","400"),
    ?line poll(M,H,P,N,"GET /cgi-bin/erl/httpd_example:get HTTP/1.0\r\n\r\n","200"),
    ?line poll(M,H,P,N,"GET /cgi-bin/erl/httpd_example:get?input=4711 HTTP/1.0\r\n\r\n","200"),
    ?line poll(M,H,P,N,"GET /cgi-bin/erl/httpd_example:post HTTP/1.0\r\n\r\n","200"),
    ?line poll(M,H,P,N,"GET /cgi-bin/erl/not_allowed:post HTTP/1.0\r\n\r\n","403"),
    ?line poll(M,H,P,N,"GET /cgi-bin/erl/httpd_example:undef HTTP/1.0\r\n\r\n","500"),
    ?line poll(M,H,P,N,"GET /cgi-bin/erl/httpd_example/yahoo HTTP/1.0\r\n\r\n","302"),
    ok.

%% mod_get

mod_get(M,H,P,N) ->
    ?line e_poll(M,H,P,N,"GET /index.html HTTP/1.0\r\n\r\n",
		 [{statuscode, 200},
		  {header, 'Content-Type', "text/html"},
		  {header, 'Date'},
		  {header, 'Server'}]),
    ?line e_poll_persistent(M,H,P,N,"GET /fsize.shtml HTTP/1.1\r\nHost:" ++ H ++  "\r\n\r\n", 
		 [{statuscode, 200},
		  {header, 'Content-Type', "text/html"},
		  {header, 'Date'},
		  {header, 'Server'}]),
    ?line e_poll(M,H,P,N,"GET /fsize.shtml HTTP/1.0\r\n\r\n", 
		 [{statuscode, 200},
		  {header, 'Content-Type'},
		  {header, 'Server'},
		  {header, 'Date'}]),
    ?line e_poll(M,H,P,N,"GET /secret/dummy.html HTTP/1.0\r\n\r\n", 
		 [{statuscode, 401},
		  {header, 'WWW-Authenticate'}]),
    ?line e_poll(M,H,P,N,"GET /index.html HTTP/1.0\r\n\r\n", 
		 [{statuscode, 200},
		  {header, 'Server'},
		  {header, 'Date'},
		  {header, 'Content-Type', "text/html"}]),
    ok.

%% mod_head

mod_head(M,H,P,N) ->
  ?line poll(M,H,P,N,"HEAD /index.html HTTP/1.0\r\n\r\n","200"),
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Controller functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_all(Config, Mnesia) ->
    ?DEBUG("start_all() -> entry with Mnesia = ~p", [Mnesia]),
    start_all(Config, Mnesia, []).
start_all(Config, Mnesia, Options) ->
    ?DEBUG("start_all(): Options = ~p", [Options]),
    Port = ?CONFIG(port, Config),
    ?DEBUG("start_all(): Port = ~p", [Port]),
    Host = ?CONFIG(hostname, Config),
    ?DEBUG("start_all(): Host = ~p", [Host]),
    NodeName = ?CONFIG(nodename, Config),
    ?DEBUG("start_all(): NodeName = ~p", [NodeName]),
    SockType = ?CONFIG(sock_type, Config),
    ?DEBUG("start_all(): SockType = ~p", [SockType]),
    ConfigFile = create_config(Config, Port, SockType, Host, Options),
    ?DEBUG("start_all(): ConfigFile = ~p", [ConfigFile]),

    case Mnesia of
	true ->
	    ?line ok = init_mnesia_on_node(Config, Port, NodeName);
	_ ->
	    ok
    end,

    VConf = [{manager_verbosity,          ?MAN_VERBOSITY}, 
	     {request_handler_verbosity,  ?REQ_VERBOSITY}, 
	     {acceptor_verbosity,         ?ACC_VERBOSITY}, 
	     {auth_verbosity,             ?AUTH_VERBOSITY}, 
	     {security_verbosity,         ?SEC_VERBOSITY}],
    V = inets_test_lib:get_config(verbosity, Config, VConf),
    ?line ?LOG("Starting HTTP server: ~s", [ConfigFile]),
    case (catch httpd_start(NodeName, ConfigFile, V)) of
	 {ok, Pid} when pid(Pid) ->
	    ?DEBUG("HTTP server: ~p", [Pid]),
	    await_started(Config,NodeName,Host,Port),
	    ok;
	{error, {already_started, Pid1}} when pid(Pid1) ->
	    ?LOG("already started (~p): stop and try again", [Pid1]),
	    Res = httpd_stop(NodeName, ConfigFile),
	    ?DEBUG("stop result: ~p", [Res]),
	    ?SLEEP(1000), %% give it some time to die
	    case (catch httpd_start(NodeName, ConfigFile, V)) of
		{ok, Pid2} when pid(Pid2) ->
		    ?DEBUG("HTTP server: ~p", [Pid2]),
		    await_started(Config, NodeName, Host, Port),
		    ok;
		{error, {already_started, Pid3}} when pid(Pid3) ->
		    ?LOG("already started (~p): send a kill signal and then try", [Pid3]),
		    exit(Pid3, kill),
		    case (catch httpd_start(NodeName, ConfigFile, V)) of
			{ok, Pid4} when pid(Pid4) ->
			    ?DEBUG("HTTP server: ~p", [Pid4]),
			    await_started(Config, NodeName, Host, Port),
			    ok;
			Other ->
			    ?LOG("Start failed: ~p", [Other]),
			    ?line ?FAIL({could_not_start_httpd, Other})
		    end;
		Other ->
		    ?LOG("Start failed: ~p", [Other]),
		    ?line ?FAIL({could_not_start_httpd, Other})
	    end;
	Other ->
	    ?LOG("Start failed: ~p", [Other]),
	    ?line ?FAIL({could_not_start_httpd, Other})
    end,
    {SockType, Host, Port, NodeName}.
    

stop_all(Config) when list(Config) ->
    Addr     = undefined,
    Port     = ?CONFIG(port, Config),
    NodeName = ?CONFIG(nodename, Config),
    ?HTTPD_STATUS("http server final status: ", httpd_status(NodeName,Port)),
    ?line httpd_stop(NodeName, Addr,Port).


await_started(Config,NodeName,Host,Port) ->
    await_started(Config,NodeName,Host,Port,10).

await_started(Config,_NodeName,_Host,_Port,0) ->
    ?DEBUG("HTTP server start timeout", []),
    ?line ?FAIL({could_not_start_httpd, timeout});
    
await_started(Config,NodeName,Host,Port,N) ->
    ?DEBUG("await HTTP server start (~p)", [N]),
    case (catch httpd_status(NodeName,Port)) of
	not_started ->
	    ?DEBUG("HTTP server not started", []),
	    ?SLEEP(500),
	    await_started(Config,NodeName,Host,Port,N-1);
	Other ->
	    ?DEBUG("HTTP server started: ~p", [Other]),
	    ok
    end.


stop_suite(Config) when list(Config) ->
    Addr = undefined,
    Port = ?CONFIG(port, Config),
    NodeName = ?CONFIG(nodename, Config),
    httpd_stop(NodeName, Addr,Port),
    ok.

init_suite(Config0, Type, Port) ->    
    ?DEBUG("init_suite() => Config0: ~p", [Config0]),
    ?DEBUG("init_suite() => Type: ~p and Port: ~p", [Type,Port]),
    Config1 = lists:keydelete(nodename, 1, Config0),
    ?DEBUG("init_suite() => Config1: ~p", [Config1]),
    Config2 = lists:keydelete(hostname, 1, Config1),
    ?DEBUG("init_suite() => Config2: ~p", [Config2]),
    Config3 = lists:keydelete(sock_type, 1, Config2),
    ?DEBUG("init_suite() => Config3: ~p", [Config3]),
    Config = lists:keydelete(port, 1, Config3),
    ?DEBUG("init_suite() => Config: ~p", [Config]),
    Host = ?HOSTNAME(),
    ?LOG("init ~p sub-suite. Using port ~p on ~s", [Type, Port, Host]),
    NodeName = node(),
    ?DEBUG("init_suite() => NodeName: ~p", [NodeName]),
    [
     {nodename, NodeName},
     {hostname, Host},
     {sock_type, Type}, 
     {port, Port},
     {address,getaddr()}|Config].

getaddr() ->
    {ok,HostName} = inet:gethostname(),
    {ok,{A1,A2,A3,A4}} = inet:getaddr(HostName,inet),
    lists:flatten(io_lib:format("~p.~p.~p.~p",[A1,A2,A3,A4])).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utility functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

auth_request(M,H,P,N,URI, User, Passwd, Expect) ->
    ?DEBUG("auth_request -> ~n"
	   "     URI:    ~p~n"
	   "     User:   ~s~n"
	   "     Passwd: ~s~n"
	   "     Expect: ~p", 
	   [URI, User, Passwd, Expect]),
    Req = ["GET ", URI, " HTTP/1.0\r\n", 
	   "Authorization: Basic ", encode_base64(User++":"++Passwd),
	   "\r\n\r\n"],
    e_poll(M,H,P,N, lists:flatten(Req), [{statuscode, Expect}]).

encode_base64([]) ->
    [];
encode_base64([A]) ->
    [e(A bsr 2), e((A band 3) bsl 4), $=, $=];
encode_base64([A,B]) ->
    [e(A bsr 2), e(((A band 3) bsl 4) bor (B bsr 4)), e((B band 15) bsl 2), $=];
encode_base64([A,B,C|Ls]) ->
    encode_base64_do(A,B,C, Ls).
encode_base64_do(A,B,C, Rest) ->
    BB = (A bsl 16) bor (B bsl 8) bor C,
    [e(BB bsr 18), e((BB bsr 12) band 63), 
     e((BB bsr 6) band 63), e(BB band 63)|encode_base64(Rest)].

e(X) when X >= 0, X < 26 -> X+65;
e(X) when X > 25, X < 52 -> X+71;
e(X) when X > 51, X < 62 -> X-4;
e(62) ->                    $+;
e(63) ->                    $/;
e(X)  ->                    exit({bad_encode_base64_token, X}).



%% e_poll/5 - extended_poll, to be able to keep poll for backward compability.

e_poll_persistent(Mode, Host, Port, Node, Request, Options) ->
    ?PPOLL(Mode, Host, Port, Node, Request, Options).
    

e_poll(Mode, Host, Port, Node, Request, Options) ->
    ?EPOLL(Mode, Host, Port, Node, Request, Options).
    


%% poll

poll(Mode, Host, Port, Node, Request, StatusCode) ->
    ?POLL(Mode, Host, Port, Node, Request, StatusCode, 30000).

poll(Mode, Host, Port, Node, Request, StatusCode, Timeout) ->
    ?POLL(Mode, Host, Port, Node, Request, StatusCode, Timeout).


cpoll(Mode, Host, Port, Node, Request, StatusCode) ->
    ?CPOLL(Mode, Host, Port, Node, Request, StatusCode, 30000).

% cpoll(Mode, Host, Port, Node, Request, StatusCode, Timeout) ->
%     ?CPOLL(Mode, Host, Port, Node, Request, StatusCode, Timeout).




%% This is a callback function used supply a password for the 
%% HTTPD application.
%%
ssl_password_cb() ->
    ?DEBUG("ssl_password_cb -> entry: return dummy password",[]),
    "dummy-ssl-password".


set_ssl_portprogram_dir(Config) ->
    DataDir = 
	case ?CONFIG(data_dir, Config) of
	    undefined ->
		?inets_data_dir; % Inets own test server
	    Dir ->
		Dir
	end,
    ?DEBUG("set_ssl_portprogram_dir -> DataDir = ~s",[DataDir]),
    DirList   = filename:split(DataDir), 
    ?DEBUG("set_ssl_portprogram_dir -> DirList = ~p",[DirList]),
    Len       = length(DirList),
    ?DEBUG("set_ssl_portprogram_dir -> Len = ~p",[Len]),
    NDirList  = lists:sublist(DirList, Len - 1),
    ?DEBUG("set_ssl_portprogram_dir -> NDirList = ~p",[NDirList]),
    SSLLibDir = filename:join(NDirList ++ ["all_SUITE_data", "bin"]),
    ?DEBUG("set_ssl_portprogram_dir -> SSLLibDir = ~s",[SSLLibDir]),
    os:putenv("ERL_SSL_PORTPROGRAM_DIR", SSLLibDir),

    %% Why is this needed when run with the inets test server and not
    %% when run by the test server?
%     case os:getenv("LD_LIBRARY_PATH") of
% 	false ->
% 	    %% ignore
% 	    ok;
% 	LD0 ->
% 	    LibDir = filename:join(NDirList ++ ["all_SUITE_data", "lib"]),
% 	    LD1    = lists:concat([LibDir, ":", LD0]),
% 	    ?DEBUG("set_ssl_portprogram_dir -> new LD_LIBRARY_PATH:~n~s", 
% 		   [LD1]),
% 	    os:putenv("LD_LIBRARY_PATH", LD1)
%     end.
    LD = filename:join(NDirList ++ ["all_SUITE_data", "lib"]),
    ?DEBUG("set_ssl_portprogram_dir -> new LD_LIBRARY_PATH:~n~s", [LD]),
    os:putenv("LD_LIBRARY_PATH", LD).


%%------------------------------------------------------------------
%%
%% Help functions
%%

list_blocked_users(Node,Port) ->
    Addr = undefined, % Assumed to be on the same host
    rpc:call(Node, mod_security, list_blocked_users, [Addr,Port]).

list_blocked_users(Node,Port,Dir) ->
    Addr = undefined, % Assumed to be on the same host
    rpc:call(Node, mod_security, list_blocked_users, [Addr,Port,Dir]).

block_user(Node,User,Port,Dir,Sec) ->
    Addr = undefined, % Assumed to be on the same host
    rpc:call(Node, mod_security, block_user, [User, Addr, Port, Dir, Sec]).

unblock_user(Node,User,Port,Dir) ->
    Addr = undefined, % Assumed to be on the same host
    rpc:call(Node, mod_security, unblock_user, [User, Addr, Port, Dir]).

list_auth_users(Node,Port) ->
    Addr = undefined, % Assumed to be on the same host
    rpc:call(Node, mod_security, list_auth_users, [Addr,Port]).
    
list_auth_users(Node,Port,Dir) ->
    Addr = undefined, % Assumed to be on the same host
    rpc:call(Node, mod_security, list_auth_users, [Addr,Port,Dir]).
    

check_lists_members({ok,L},L) -> 
    ok;
check_lists_members({ok,L1},L2) ->
    check_lists_members1(lists:sort(L1),lists:sort(L2));
check_lists_members(Error,_L) ->
    Error.

check_lists_members1(L,L) ->
    ok;
check_lists_members1(L1,L2) ->
    {error,{lists_not_equal,L1,L2}}.


wd_start(Conf, Def) ->
    Timeout = inets_test_lib:get_config(tc_timeout, Conf, Def),
    Pid     = ?WD_START(Timeout),
    [{watchdog, Pid}|Conf].

wd_stop(Conf) ->
    case inets_test_lib:get_config(watchdog, Conf) of
	Pid when pid(Pid) ->
	    ?WD_STOP(Pid);
	_ ->
	    ok
    end.
    

httpd_status(NodeName,Port) ->
    rpc:call(NodeName,httpd,get_status,[Port,5000]).

httpd_stop(NodeName, ConfigFile) ->
    rpc:call(NodeName, httpd, stop,[ConfigFile]).
    
httpd_stop(NodeName,Addr,Port) ->
    rpc:call(NodeName, httpd, stop,[Addr,Port]).
    
httpd_start(NodeName, ConfigFile, Verbosity) ->
    rpc:call(NodeName, httpd, start,[ConfigFile, Verbosity]).
    



status_to_string(L) when list(L) ->
    lists:flatten(status_to_string1(L));
status_to_string(O) ->
    lists:flatten(io_lib:format("Unknown status: ~p",[O])).

status_to_string1([]) -> "";
status_to_string1([H|T]) ->
    "~n     " ++ status_item_to_string(H) ++ status_to_string1(T).

status_item_to_string(V) ->
    status_item_to_string1(V).

status_item_to_string1({usage_state,S}) ->
    io_lib:format("Usage state:                   ~p",[S]);
status_item_to_string1({current_conn,N}) ->
    io_lib:format("Current number of connections: ~p",[N]);
status_item_to_string1({max_conn,N}) ->
    io_lib:format("Maximum number of connections: ~p",[N]);
status_item_to_string1({last_heavy_load,never}) ->
    io_lib:format("Last heavy load:               never",[]);
status_item_to_string1({last_heavy_load,Date}) ->
    io_lib:format("Last heavy load:               ~s",
		  [httpd_util:rfc1123_date(Date)]);
status_item_to_string1({last_connection,never}) ->
    io_lib:format("Last connection:               never",[]);
status_item_to_string1({last_connection,Date}) ->
    io_lib:format("Last connection:               ~s",
		  [httpd_util:rfc1123_date(Date)]);
status_item_to_string1({auth_status, undefined}) ->
    "Auth server status info: undefined";
status_item_to_string1({auth_status, Status}) ->
    "Auth server status info: " ++ process_status(Status);
status_item_to_string1({security_status, undefined}) ->
    "Security server status info: undefined";
status_item_to_string1({security_status, Status}) ->
    "Security server status info: " ++ process_status(Status);
status_item_to_string1({listener_status,Status}) ->
    "Listener status info: " ++ process_status(Status);
status_item_to_string1({manager_status,Status}) ->
    "Manager status info: " ++ process_status(Status);
status_item_to_string1(_) ->
    "".

process_status([]) -> "";
process_status([H|T]) ->
    "~n          " ++ process_status1(H) ++ process_status(T).

process_status1({pid,Pid}) ->
    io_lib:format("Pid:                  ~p",[Pid]);
process_status1({status,Status}) ->
    io_lib:format("Status:               ~p",[Status]);
process_status1({message_queue_len,MsgQLen}) ->
    io_lib:format("Message queue length: ~p",[MsgQLen]);
process_status1({stack_size,StackSize}) ->
    io_lib:format("Stack size:           ~p",[StackSize]);
process_status1({heap_size,HeapSize}) ->
    io_lib:format("Heap size:            ~p",[HeapSize]);
process_status1({reductions,Reductions}) ->
    io_lib:format("Reductions:           ~p",[Reductions]);
process_status1({current_function,CurrentFunc}) ->
    io_lib:format("Current function:     ~p",[CurrentFunc]);
process_status1(_) ->
    "".


%%Creates the directories and the files that is used to test htaccess 
mkTestData(Path,IpAddress)->
    mkdirs(Path),
    
    mkHtmlFile(filename:join([Path,"ht/open/dummy.html"])),
    mkHtmlFile(filename:join([Path,"ht/blocknet/dummy.html"])),
    mkHtmlFile(filename:join([Path,"ht/secret/dummy.html"])),
    mkHtmlFile(filename:join([Path,"ht/secret/top_secret/dummy.html"])),
    
    mkHtAccessFile(filename:join([Path,"ht/open/.htaccess"]),Path,"user one Aladdin"),
    mkHtAccessFile(filename:join([Path,"ht/secret/.htaccess"]),Path,"group group1 group2"),
    mkHtAccessFile(filename:join([Path,"ht/secret/top_secret/.htaccess"]),Path,"user four"),
    mkHtAccessFile(filename:join([Path,"ht/blocknet/.htaccess"]),Path,nouser,IpAddress),
   
    makeUsrAndGrpFile(filename:join([Path,"ht","users.file"]),"one:OnePassword\ntwo:TwoPassword\nthree:ThreePassword\nfour:FourPassword\nAladdin:AladdinPassword"),
    makeUsrAndGrpFile(filename:join([Path,"ht","groups.file"]),"group1: two one\ngroup2: two three").				   
%%Delete the files that was used to test the htaccess function 
delTestData(Path)->
    file:delete(filename:join([Path,"ht/open/dummy.html"])),
    file:delete(filename:join([Path,"ht/secret/dummy.html"])),
    file:delete(filename:join([Path,"ht/secret/top_secret/dummy.html"])),
    file:delete(filename:join([Path,"ht/blocknet/dummy.html"])),
    
    file:delete(filename:join([Path,"ht/blocknet/.htaccess"])),
    file:delete(filename:join([Path,"ht/open/.htaccess"])),
    file:delete(filename:join([Path,"ht/secret/.htaccess"])),
    file:delete(filename:join([Path,"ht/secret/top_secret/.htaccess"])),
    file:delete(filename:join([Path,"ht","users.file"])),
    file:delete(filename:join([Path,"ht","groups.file"])),
    rmdirs(Path).

%Creates a very simple test html page
mkHtmlFile(PathAndFileName)->
    file:write_file(PathAndFileName,list_to_binary(
	 "<html><head><title>test</title></head>
         <body>testar</body></html>")).
                                     
%%Creates a htacess file 
mkHtAccessFile(PathAndFileName,BaseDir,RequireData)->
    file:write_file(PathAndFileName,list_to_binary(
"AuthUserFile "++ BaseDir ++"/ht/users.file\nAuthGroupFile "++BaseDir++"/ht/groups.file\nAuthName Test\nAuthType Basic\n<Limit>\nrequire " ++RequireData ++"\n</Limit>")).
		 
mkHtAccessFile(PathAndFileName,BaseDir,nouser,IpAddress)->
    file:write_file(PathAndFileName,list_to_binary(
"AuthUserFile "++ BaseDir ++"/ht/users.file\nAuthGroupFile "++BaseDir++"/ht/groups.file\nAuthName Test\nAuthType Basic\n<Limit GET>\n\tallow from " ++ formatIp(IpAddress,string:rchr(IpAddress,$.)) ++ "\n</Limit>")).
		
%%Pos is the first time the last position of a .
formatIp(IpAddress,Pos)when Pos > 0->
    case lists:nth(Pos,IpAddress) of
	$.->
	    case lists:nth(Pos-2,IpAddress) of
		$.->
		   formatIp(IpAddress,Pos-3);
		_->
		    lists:sublist(IpAddress,Pos-2)++"."
	    end;
	_ ->
	    formatIp(IpAddress,Pos-1)
    end;
%%Ok the ipaddress is N,N,N,NNN use an ipaddress that isnt the servers
formatIp(IpAddress,Pos)->
    "1"++IpAddress.
    
%%Prints the data to the file 
makeUsrAndGrpFile(PathAndFileName,Data)->
    file:write_file(PathAndFileName,list_to_binary(Data)).


%%Creates the directories that is used to test the .html files
mkdirs(Path)->
    file:make_dir(filename:join([Path,"ht"])),
    file:make_dir(filename:join([Path,"ht/open"])),
    file:make_dir(filename:join([Path,"ht/blocknet"])),
    file:make_dir(filename:join([Path,"ht/secret"])),
    file:make_dir(filename:join([Path,"ht/secret/top_secret"])).

%%Removes the directories that is used to test the htaccess function
rmdirs(Path)->
    file:del_dir(filename:join([Path,"ht/secret/top_secret"])),
    file:del_dir(filename:join([Path,"ht/secret"])),
    file:make_dir(filename:join([Path,"ht/blocknet"])),
    file:del_dir(filename:join([Path,"ht/open"])),
    file:del_dir(filename:join([Path,"ht"])).




%%Make a file 100 bytes long containing 012...9*10
mkRangeTestData(Path)->
    PathAndFileName=filename:join([Path,"range.txt"]),
    file:write_file(PathAndFileName,list_to_binary(["12345678901234567890",
						   "12345678901234567890",
						   "12345678901234567890",
						   "12345678901234567890",
						   "12345678901234567890"])).

rmRangeTestData(Path)->
    file:delete(filename:join([Path,"range.txt"])).


