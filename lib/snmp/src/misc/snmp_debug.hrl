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

-ifdef(snmp_debug).
-define(d(F,A),
	io:format("~p:~p:~p:" ++ F ++ "~n",[self(),?MODULE,?LINE]++A)).

%% Same as 'd' but without the ending newline ('~n').
-define(d_b(F,A),
	io:format("~p:~p:~p:" ++ F,[self(),?MODULE,?LINE]++A)).
%% To be used together with 'd_b'. Note: NO ending newline ('~n')..
-define(d_e(F,A),
	io:format(F,A)).
-else.
-define(d(F,A),ok).
-define(d_b(F,A),ok).
-define(d_e(F,A),ok).
-endif.




