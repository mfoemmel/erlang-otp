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
%% Purpose: Externalize/internalize Megaco/H.248 messages
%%----------------------------------------------------------------------

-module(megaco_erl_dist_encoder).

-behaviour(megaco_encoder).

-export([encode_message/2, decode_message/2]).

-include("megaco_message_internal.hrl").

%%----------------------------------------------------------------------
%% Convert a 'MegacoMessage' record into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------

encode_message(Config, MegaMsg) when record(MegaMsg, 'MegacoMessage') ->
    {ok, erlang:term_to_binary(MegaMsg, Config)};
encode_message(_Config, _MegaMsg) ->
    {error, not_a_megaco_message}.
	    
%%----------------------------------------------------------------------
%% Convert a binary into a 'MegacoMessage' record
%% Return {ok, MegacoMessageRecord} | {error, Reason}
%%----------------------------------------------------------------------
decode_message(_Config, Bin) ->
    case catch erlang:binary_to_term(Bin) of
	MegaMsg when record(MegaMsg, 'MegacoMessage') ->
	    {ok, MegaMsg};
	{'EXIT', _Reason} ->
	    {error, bad_binary}
    end.
