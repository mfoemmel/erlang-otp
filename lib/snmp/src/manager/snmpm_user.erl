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

-module(snmpm_user).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{handle_agent,    4}, 
     {handle_pdu,      5},
     {handle_trap,     4},
     {handle_inform,   4},
     {handle_report,   4}];
behaviour_info(_) ->
    undefined.


%% handle_agent(Addr, Port, SnmpInfo, UserData) -> Reply
%% Addr        -> term()
%% Port        -> integer()
%% SnmpInfo    -> {ErrorStatus, ErrorIndex, Varbinds}
%% UserId      -> term()
%% ErrorStatus -> atom()
%% ErrorIndex  -> integer()
%% Varbinds    -> [varbind()]
%% UserData    -> term()     (supplied when the user regester)
%% Reply       -> ignore | {register, UserId, agent_info()}
%% agent_info() -> [{agent_info_item(), agent_info_value()}]
%%                 This is the same info as in update_agent_info/4

%% handle_pdu(Addr, Port, ReqId, SnmpResponse, UserData) -> Reply
%% Addr         -> term()
%% Port         -> integer()
%% ReqId        -> term() (returned when calling ag(...), ...)
%% SnmpResponse -> {ErrorStatus, ErrorIndex, Varbinds}
%% ErrorStatus  -> atom()
%% ErrorIndex   -> integer()
%% Varbinds     -> [varbind()]
%% UserData     -> term()     (supplied when the user regester)
%% Reply        -> ignore 

%% handle_trap(Addr, Port, SnmpTrapInfo, UserData) -> Reply
%% Addr         -> term()
%% Port         -> integer()
%% SnmpTrapInfo -> {Enteprise, Generic, Spec, Timestamp, Varbinds} |
%%                 {ErrorStatus, ErrorIndex, Varbinds}
%% Enteprise    -> oid()
%% Generic      -> integer() 
%% Spec         -> integer() 
%% Timestamp    -> integer() 
%% ErrorStatus  -> atom()
%% ErrorIndex   -> integer()
%% Varbinds     -> [varbind()]
%% UserData     -> term()     (supplied when the user regester)
%% Reply        -> ignore | unregister | {register, UserId, agent_info()}

%% handle_inform(Addr, Port, SnmpInform, UserData) -> Reply
%% Addr        -> term()
%% Port        -> integer()
%% SnmpInform  -> {ErrorStatus, ErrorIndex, Varbinds}
%% ErrorStatus -> atom()
%% ErrorIndex  -> integer()
%% Varbinds    -> [varbind()]
%% UserData    -> term()     (supplied when the user regester)
%% Reply       -> ignore | unregister | {register, UserId, agent_info()}
%%          

%% handle_report(Addr, Port, SnmpReport, UserData) -> Reply
%% Addr        -> term()
%% Port        -> integer()
%% SnmpReport  -> {ErrorStatus, ErrorIndex, Varbinds}
%% ErrorStatus -> integer()
%% ErrorIndex  -> integer()
%% Varbinds    -> [varbind()]
%% UserData    -> term()     (supplied when the user regester)
%% Reply       -> ignore | unregister | {register, UserId, agent_info()}

