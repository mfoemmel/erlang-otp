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
%% File: CosNaming_BindingIterator_impl.erl
%% Author: Lars Thorsen
%% 
%% Creation date: 970902
%% Modified:
%%
%%-----------------------------------------------------------------
-module('CosNaming_BindingIterator_impl').

-include_lib("orber/include/corba.hrl").
-include("CosNaming.hrl").
-include("orber_cosnaming.hrl").


%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([init/1, terminate/2]).
-export([next_one/1, next_n/2, destroy/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: init/1
%% Args:
%% Returns: 
%%-----------------------------------------------------------------
init(State) ->
    {ok, State}.

%%-----------------------------------------------------------------
%% Func: terminate/2
%% Args:
%% Returns: 
%%-----------------------------------------------------------------
terminate(Reason, State) ->
    ok.

next_one({SubObjectKey, Counter}) ->
    _RF = ?read_function({orber_CosNaming, SubObjectKey}),
    case orber_cosnaming_utils:query_result(mnesia:transaction(_RF)) of
	error ->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO});
	TotalList ->
	    if
		length(TotalList) - Counter =< 0 ->
		    {{false, null}, {SubObjectKey, Counter}};
		true ->
		    NewCounter = Counter + 1,
		    {N, T, O} = lists:nth(NewCounter, TotalList),
		    Binding = #'CosNaming_Binding'{binding_name=[N],
						   binding_type=T},
		    {{true, Binding}, {SubObjectKey, NewCounter}}
	    end
    end.

next_n({SubObjectKey, Counter}, How_many) ->
    _RF = ?read_function({orber_CosNaming, SubObjectKey}),
    case orber_cosnaming_utils:query_result(mnesia:transaction(_RF)) of
	error ->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO});
	TotalList ->
	    if
		length(TotalList) - Counter =< 0 ->
		    {{false, []}, {SubObjectKey, Counter}};
		true ->
		    List = lists:sublist(TotalList, Counter + 1, How_many),
		    BList = lists:map(
			      fun({N, T, O}) ->
				      #'CosNaming_Binding'{binding_name=[N],
							   binding_type=T}
			      end, List),
		    NewCounter = length(TotalList) - Counter,
		    {{true, BList}, {SubObjectKey, NewCounter}}
	    end
    end.
		    
destroy(OE_State) ->
    %% Objkey server removes objectkey when receiving EXIT signal.
    {stop, normal, ok, OE_State}.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
