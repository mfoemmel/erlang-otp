%%<copyright>
%% <year>1996-2008</year>
%% <holder>Ericsson AB, All Rights Reserved</holder>
%%</copyright>
%%<legalnotice>
%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson AB.
%%</legalnotice>
%%
-ifdef(line_trace).
-line_trace(true).
-define(line,
	put(test_server_loc,{?MODULE,?LINE}),
	io:format(lists:concat([?MODULE,",",integer_to_list(?LINE),": ~p"]),
		  [erlang:now()]),).
-else.
-define(line,put(test_server_loc,{?MODULE,?LINE}),).
-endif.
-define(t,test_server).
-define(config,test_server:lookup_config).


