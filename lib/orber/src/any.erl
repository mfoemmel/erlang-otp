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
%%-----------------------------------------------------------------
%% File: any.erl
%% Description:
%%    This file conatins the interface for the any type
%%
%% Creation date: 980410
%%
%%-----------------------------------------------------------------
-module(any).

-include_lib("orber/include/corba.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([create/0, create/2,
	 set_typecode/2, get_typecode/1,
	 set_value/2, get_value/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------
create() ->
    #any{}.

create(TC, V) ->
    case orber_tc:check_tc(TC) of
	true ->
	    #any{typecode=TC, value=V};
	false ->
	    corba:raise(#'BAD_TYPECODE'{completion_status=?COMPLETED_NO})
    end.

set_typecode(Any, TC) ->
    case orber_tc:check_tc(TC) of
	true ->
	    Any#any{typecode=TC};
	false ->
	    corba:raise(#'BAD_TYPECODE'{completion_status=?COMPLETED_NO})
    end.

get_typecode(Any) ->
    Any#any.typecode.

set_value(Any, V) ->
     Any#any{value=V}.

get_value(Any) ->
    Any#any.value.

