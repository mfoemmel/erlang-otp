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
-module(mnemosyne_cost).

-export([output_size/2,
	 selective_cost/2]).

-include("mnemosyne_internal_form.hrl").


%%%================================================================
%%% 		Exports

%% The average number of tuples that a call of G will return

output_size(P, BoundVars) when is_record(P,pred_sym), P#pred_sym.type==table ->
    {Card, Image, MaxSel} = mnemosyne_catalog:image(P#pred_sym.functor),
    if  MaxSel == 0 -> 0; %% ????
	true ->
	    selectivity(P#pred_sym.args, BoundVars, Image, P#pred_sym.functor,
			1, 1) / MaxSel
    end;
output_size(P, BoundVars) ->
    1.

%%%---------------- Cost function(s)


%% Cost = f(estimated_selectivity)

selective_cost(P, BoundVars) when is_record(P,pred_sym) ->
    case P#pred_sym.type of
	rule when P#pred_sym.recursive==recursive -> 
	    {5, 0, bound_factor(P,BoundVars)};
	table -> 
	    {3,
	     mul_location_factor(P, output_size(P,BoundVars)),
	     bound_factor(P,BoundVars)}
    end;
selective_cost({'#not',C,NGs}, BoundVars) ->
    case mnemosyne_unify:variables_and_annonymous(NGs) of
	[] -> {4,length(NGs),0};
	N ->  {6,length(NGs),length(N)}
    end;
selective_cost(E, BoundVars) when is_record(E,erl_expr) ->
    {1,0,0}.



%%%================================================================
%%% 		Private


%% Fraction of bound variables/orignal num vars.  Range [0..100]

bound_factor(P, AllBoundVars) ->
    OrigVars = P#pred_sym.original_args_vars,
    if  OrigVars==[] -> 
	    1;
	true ->
	    VarsNow = 
		mnemosyne_unify:variables_and_annonymous(P#pred_sym.args),
	    BoundVars = ordsets:union(ordsets:intersection(VarsNow,
							   AllBoundVars),
				      ordsets:subtract(OrigVars,VarsNow)),
	    100 - ((100*card(BoundVars)) div card(OrigVars))
    end.



selectivity([A|Args], BoundVars, Image, Tab, Pos, Acc) ->
    selectivity(Args, BoundVars, Image, Tab, Pos+1,
		case A of
		    {'#var','_'} -> Acc;
		    {'#var',V} ->
			case ordsets:is_element(V,BoundVars) of
			    true -> Acc*element(Pos,Image);
			    false -> Acc
			end;
		    _ -> Acc*element(Pos,Image)
		end);
selectivity([], _, _, _, _, Acc) ->
    Acc.



mul_location_factor(P, V) ->
   case location(P) of
       remote -> 100*V;
       local -> V
   end.


location(P) ->
    case lists:member(P#pred_sym.functor, 
		      mnesia:system_info(local_tables)) of
	true -> local;
	false -> remote
    end.


card(S) -> length(ordsets:to_list(S)).


