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
%%% Purpose : The GS object

-module(gstk_gs).

-export([mk_create_opts_for_child/4,
	 config/3,
	 read/3,
	 read_option/5,
	 option/5]).

-include("gstk.hrl").

%%----------------------------------------------------------------------
%% The GS object implementation
%%----------------------------------------------------------------------

mk_create_opts_for_child(DB,Cgstkid, Pgstkid, Opts) ->
    gstk_generic:mk_create_opts_for_child(DB,Cgstkid,Pgstkid,Opts).

config(DB, Gstkid, Opts) ->
    Cmd=gstk_generic:make_command(Opts,Gstkid,"",DB),
    gstk:exec(Cmd),
    ok.

read(DB, Gstkid, Opt) ->
    gstk_generic:read_option(DB, Gstkid, Opt).

% No options of my own
read_option(Option,Gstkid, _TkW,_DB,_) -> 
    {bad_result, {Gstkid#gstkid.objtype, invalid_option, Option}}.

option(_Option, _Gstkid, _TkW, _DB,_) ->
    invalid_option.
