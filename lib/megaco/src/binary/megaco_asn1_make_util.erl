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
%% Purpose : Make utility to find asn1 version
%%----------------------------------------------------------------------

-module(megaco_asn1_make_util).

%%-----------------------------------------------------------------
%% Public interface
%%-----------------------------------------------------------------

-export([asn1version_check/1]).


%% This is a really ugly make utility...
asn1version_check([V]) when list(V) ->
    asn1version_check(V);
asn1version_check(V) ->
    asn1version_check(V, code:which(asn1ct)).

asn1version_check(V, Path) when list(Path) ->
    case (catch get_asn1version(Path)) of
	{ok, Version} when Version >= V ->
	    yes();
	Else ->
	    no()
    end;
asn1version_check(_, _) ->
    no().

get_asn1version(Path) ->
    %% Find ASN1 application dir:
    AppDirIdx  = str(Path, "asn1-"),
    EbinDirIdx = str(Path, "/ebin/"),
    AppDir     = string:sub_string(Path, AppDirIdx, EbinDirIdx-1),
    
    %% Fins the version number
    AppVersionStartIdx = str(AppDir, "-"),
    AppVersion         = string:sub_string(AppDir, AppVersionStartIdx+1),
    {ok, AppVersion}.

    
str(Base, Str) ->
    case string:str(Base, Str) of
	Idx2 when Idx2 == 0 ->
	    throw(error);
	Idx2 ->
	    Idx2
    end.
    
no() ->
    io:format("no~n", []).

yes() ->
    io:format("yes~n", []).
