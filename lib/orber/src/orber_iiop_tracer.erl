%%--------------------------------------------------------------------
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
%%--------------------------------------------------------------------
%% File    : orber_iiop_tracer.erl
%% Purpose : Use for debugging only.
%%--------------------------------------------------------------------

-module(orber_iiop_tracer).





%% Interceptor functions.
-export([new_out_connection/3,
	 new_in_connection/3,
	 closed_in_connection/1,
	 closed_out_connection/1,
	 in_request_encoded/6,
	 in_reply_encoded/6,
	 out_reply_encoded/6,
	 out_request_encoded/6,
	 in_request/6,
	 in_reply/6,
	 out_reply/6,
	 out_request/6]).


%%--------------- INTERCEPTOR FUNCTIONS ----------------------
%%------------------------------------------------------------
%% function : new_in_connection
%% Arguments: 
%% Returns  : 
%%------------------------------------------------------------
new_in_connection(Arg, CHost, Port) ->
    error_logger:info_msg("=============== new_in_connection ========
Node      : ~p
From Host : ~p
From Port : ~p
==========================================~n", 
              [node(), CHost, Port]),
    {CHost, Port}.
 
%%------------------------------------------------------------
%% function : new_out_connection
%% Arguments: 
%% Returns  : 
%%------------------------------------------------------------
new_out_connection(Arg, SHost, Port) ->
    error_logger:info_msg("=============== new_out_connection =======
Node      : ~p
To Host   : ~p
To Port   : ~p
==========================================~n", 
              [node(), SHost, Port]),
    {SHost, Port}.
 
%%------------------------------------------------------------
%% function : closed_in_connection
%% Arguments: 
%% Returns  : 
%%------------------------------------------------------------
closed_in_connection(Arg) ->
    error_logger:info_msg("=============== closed_in_connection =====
Node      : ~p
Connection: ~p
==========================================~n", 
              [node(), Arg]),
    Arg.
 
%%------------------------------------------------------------
%% function : closed_out_connection
%% Arguments: 
%% Returns  : 
%%------------------------------------------------------------
closed_out_connection(Arg) ->
    error_logger:info_msg("=============== closed_out_connection ====
Node      : ~p
Connection: ~p
==========================================~n", 
              [node(), Arg]),
    Arg.
 
%%------------------------------------------------------------
%% function : in_request_encoded
%% Arguments: 
%% Returns  : 
%%------------------------------------------------------------
in_request_encoded(Ref, ObjKey, Ctx, Op, Bin, Args) ->
    error_logger:info_msg("=============== in_request_encoded =======
Connection: ~p
Operation : ~p
Body      : ~p
Context   : ~p
==========================================~n", 
              [Ref, Op, Bin, Ctx]),
    {Bin, Args}.
 
%%------------------------------------------------------------
%% function : in_reply_encoded
%% Arguments: 
%% Returns  : 
%%------------------------------------------------------------
in_reply_encoded(Ref, ObjKey, Ctx, Op, Bin, Args) ->
    error_logger:info_msg("============== in_reply_encoded ==========
Connection: ~p
Operation : ~p
Body      : ~p
Context   : ~p
==========================================~n", 
              [Ref, Op, Bin, Ctx]),
    {Bin, Args}.
 
%%------------------------------------------------------------
%% function : out_reply_encoded
%% Arguments: 
%% Returns  : 
%%------------------------------------------------------------
out_reply_encoded(Ref, ObjKey, Ctx, Op, Bin, Args) ->
    error_logger:info_msg("============== out_reply_encoded =========
Connection: ~p
Operation : ~p
Body      : ~p
Context   : ~p
Object    : ~p
==========================================~n", 
              [Ref, Op, Bin, Ctx, ObjKey]),
    {Bin, Args}.
 
%%------------------------------------------------------------
%% function : out_request_encoded
%% Arguments: 
%% Returns  : 
%%------------------------------------------------------------
out_request_encoded(Ref, ObjKey, Ctx, Op, Bin, Args) ->
    error_logger:info_msg("============== out_request_encoded =======
Connection: ~p
Operation : ~p
Body      : ~p
Context   : ~p
==========================================~n", 
              [Ref, Op, Bin, Ctx]),
    {Bin, Args}.
 
%%------------------------------------------------------------
%% function : in_request
%% Arguments: 
%% Returns  : 
%%------------------------------------------------------------
in_request(Ref, ObjKey, Ctx, Op, Params, Args) ->
    error_logger:info_msg("=============== in_request ===============
Connection: ~p
Operation : ~p
Parameters: ~p
Context   : ~p
Object    : ~p
==========================================~n", 
              [Ref, Op, Params, Ctx, ObjKey]),
    {Params, Args}.
 
%%------------------------------------------------------------
%% function : in_reply
%% Arguments: 
%% Returns  : 
%%------------------------------------------------------------
in_reply(Ref, ObjKey, Ctx, Op, Reply, Args) ->
    error_logger:info_msg("=============== in_reply =================
Connection: ~p
Operation : ~p
Reply     : ~p
Context   : ~p
==========================================~n", 
              [Ref, Op, Reply, Ctx]),
    {Reply, Args}.
 
%%------------------------------------------------------------
%% function : out_reply
%% Arguments: 
%% Returns  : 
%%------------------------------------------------------------
out_reply(Ref, ObjKey, Ctx, Op, Reply, Args) ->
    error_logger:info_msg("=============== out_reply ================
Connection: ~p
Operation : ~p
Reply     : ~p
Context   : ~p
Object    : ~p
==========================================~n", 
              [Ref, Op, Reply, Ctx, ObjKey]),
    {Reply, Args}.
 
%%------------------------------------------------------------
%% function : out_request
%% Arguments: 
%% Returns  : 
%%------------------------------------------------------------
out_request(Ref, ObjKey, Ctx, Op, Params, Args) ->
    error_logger:info_msg("=============== out_request ==============
Connection: ~p
Operation : ~p
Parameters: ~p
Context   : ~p
==========================================~n", 
              [Ref, Op, Params, Ctx]),
    {Params, Args}.




%%======================================================================
%% END OF MODULE
%%======================================================================

