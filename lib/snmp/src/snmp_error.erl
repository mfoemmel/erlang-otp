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
-module(snmp_error).

%%%-----------------------------------------------------------------
%%% Implements different error mechanisms.
%%%-----------------------------------------------------------------
-export([user_err/2, config_err/2]).

%%-----------------------------------------------------------------
%% This function is called when there is an error in a user
%% supplied item, e.g. instrumentation function.
%%-----------------------------------------------------------------
user_err(Format, X) -> 
    Form = lists:concat(["** User error: ", Format, "\n"]),
    catch error_logger:error_msg(Form, X).

%%-----------------------------------------------------------------
%% This function is called when there is a configuration error,
%% either at startup (in a conf-file) or at run-time (e.g. when 
%% information in the configuration tables are inconsistent.)
%%-----------------------------------------------------------------
config_err(Format, X) ->
    Form = lists:concat(["** Configuration error: ", Format, "\n"]),
    catch error_logger:error_msg(Form, X).
