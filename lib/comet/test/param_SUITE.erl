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
-module(param_SUITE).

%% Test functions
-export([all/1,not_run/1,init_per_testcase/2, fin_per_testcase/2,
	 i4_param/1, date_param/1, string_param/1, double_param/1]).

-include("test_server.hrl").
-define(default_timeout, ?t:minutes(1)).

init_per_testcase(Case, Config) ->
    ?line Datadir=?config(data_dir, Config),
    ?line os:cmd("regsvr32 /s" ++ filename:nativename(filename:join(Datadir, "ErlComTestServ.DLL"))),
    ?line Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].
fin_per_testcase(Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

all(suite) ->
    case os:type() of
	{win32, _} ->
	    [i4_param, date_param, string_param, double_param];
	_ ->
	    [not_run]
    end.

not_run(doc) -> ["Doesn't run on UNIX"];
not_run(suite) -> [];
not_run(Config) when list(Config) -> {comment, "Doesn't run on UNIX."}.

get_erl_test_obj() ->
    ?line {ok, Pid}= erl_com:start_driver(),
    ?line T= erl_com:new_thread(Pid),
    ?line Obj= erl_com:create_object(T, "{5FFFAC7E-E087-11D3-AC85-00C04F9DA8C8}", 1),
    ?line erl_com:query_interface(Obj, "{5FFFAC7D-E087-11D3-AC85-00C04F9DA8C8}").

i4_param(suite) -> [];
i4_param(doc) -> ["Testing VT_I4 parameters"];
i4_param(Config) when list(Config) -> 
    ?line I= get_erl_test_obj(),
    ?line 2400= erl_com:invoke(I, "I4Add", [1200, 1200]),
    ?line -24= erl_com:invoke(I, "I4Add", [-12, {vt_i4, -12}]),
    ?line 0= erl_com:invoke(I, "I4Add", [12, {vt_i4, -12}]),
    ok.

date_param(suite) -> [];
date_param(doc) -> ["Testing VT_DATE parameters"];
date_param(Config) when list(Config) ->
    ?line I= get_erl_test_obj(),
    ?line Comnow= erl_com:invoke(I, "GetCurrentDate", []),
    ?line Comdate= calendar:now_to_universal_time(Comnow),
    ?line Date= calendar:universal_time(),
    ?line Comdate_s= calendar:datetime_to_gregorian_seconds(Comdate),
    ?line Date_s= calendar:datetime_to_gregorian_seconds(Date),
    ?line true=2>abs(Comdate_s - Date_s),
% %     ?line 1= erl_com:invoke(I, "DaysBetween",
% % 			      [{{1999, 1, 1}, {0,0,0}}, {{1999, 1, 2}, {0,0,0}}]),
    ok.

string_param(suite) -> [];
string_param(doc) -> ["Testing VT_STRING parameters"];
string_param(Config) when list(Config) ->
    ?line I= get_erl_test_obj(),
    ?line Rev= erl_com:invoke(I, "ReverseString", ["abcdefghijklmnopq"]),
    ?line erlang:display(Rev),
    ok.

double_param(suite) -> [];
double_param(doc) -> ["Testing VT_FLOAT parameters"];
double_param(Config) when list(Config) ->
    ?line I= get_erl_test_obj(),
    ?line Double= erl_com:invoke(I, "R8Add", [2.1, 1.2]),
    ?line Double=2.1+1.2,
    ok.



		       


