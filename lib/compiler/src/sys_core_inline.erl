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
%% Purpose : Function inlining optimisation for Core.

%% This simple function inliner works in two stages:
%%
%% 1. First it extracts all the inlineable functions, either given
%% explicitly or of light enough weight, and inlines them with
%% themselves.  This inlining only uses lighter functions to save
%% recursion and a real code explosion.
%%
%% 2. Run through the rest of the functions inlining all calls to
%% inlineable functions.
%%
%% The weight function is VERY simple, we count the number of nodes in
%% the function body.  We would like to remove non-exported,
%% inlineable functions but this is not trivial as they may be
%% (mutually) recursive.
%%
%% This module will catch many access functions and allow code to use
%% extra functions for clarity which are then explicitly inlined for
%% speed with a compile attribute.  See the example below.
%%
%% It is not clear that inlining will give you very much.

-module(sys_core_inline).

%%-compile({inline,{match_fail_fun,0}}).

-export([module/2]).

-import(lists, [member/2,map/2,foldl/3,mapfoldl/3]).

-include("core_parse.hrl").

%% Inline status.
-record(inline, {exports=[],thresh=0,inline=[]}).

%% General function info.
-record(fstat, {func,				%Function name
		arity,				%         arity
		fdef,				%Original definition
		weight=0,			%Weight
		inline=false,			%Inline func flag
		modified=false}).		%Mod flag

%% Inlineable function info.
-record(ifun, {func,				%Function name
	       arity,				%         arity
	       vars,				%Fun vars
	       body,				%    body
	       weight}).			%Weight

module(#c_mdef{exports=Es,body=B0}=Mod, Opts) ->
    case inline_option(10, 0, Opts) of
	{Thresh,Fs} when integer(Thresh), Thresh > 0; Fs /= [] ->
	    B1 = inline(B0, #inline{exports=Es,thresh=Thresh,inline=Fs}),
	    {ok,Mod#c_mdef{body=B1}};
	Other -> {ok,Mod}
    end.
    
inline_option(OnVal, OffVal, Opts) ->
    foldl(fun ({inline,{F,A}=Val}, {T,Fs}) ->
		  {T,[Val|Fs]};
	      ({inline,Val}, {T,Fs}) when list(Val) ->
		  {T,Val ++ Fs};
	      ({inline,Val}, {T,Fs}) when integer(Val) ->
		  {Val,Fs};
	      (inline, {T,Fs}) -> {OnVal,Fs};
	      (noinline, {T,Fs}) -> {OffVal,Fs};
	      (Opt, Def) -> Def
	  end, {0,[]}, Opts).

%% inline([Func], Stat) -> [Func].
%%  Here we do all the work.

inline(Fs0, St0) ->
    %% Generate list of augmented functions.
    Fs1 = map(fun (#c_fdef{func=F,arity=A,body=#c_fun{vars=Vs,body=B}}=Fdef) ->
		      Weight = core_lib:fold(fun weight_func/2, 0, B),
		      #fstat{func=F,arity=A,fdef=Fdef,
			     weight=Weight}
	      end, Fs0),
    %% Get inlineable functions, and inline them with themselves.
    {Fs2,Is0} = mapfoldl(fun (Fst, Ifs) ->
			      case is_inlineable(Fst, St0#inline.thresh,
						 St0#inline.inline) of
				  true ->
				      Ffun = (Fst#fstat.fdef)#c_fdef.body,
				      If = #ifun{func=Fst#fstat.func,
						 arity=Fst#fstat.arity,
						 vars=Ffun#c_fun.vars,
						 body=Ffun#c_fun.body,
						 weight=Fst#fstat.weight},
				      {Fst#fstat{inline=true},[If|Ifs]};
				  false -> {Fst,Ifs}
			      end
		      end, [], Fs1),
    Is1 = lists:map(fun (#ifun{body=B}=If) ->
			    If#ifun{body=core_lib:map(match_fail_fun(), B)}
		    end, Is0),
    Is2 = lists:map(fun (If) -> inline_inline(If, Is1) end, Is1),
    %% We would like to remove inlined, non-exported functions here,
    %% but this can be difficult as they may be recursive.
    %% Use fixed inline functions on all functions.
    Fs3 = lists:map(fun (F) -> inline_func(F, Is2) end, Fs2),
    %% Regenerate module body.
    map(fun (#fstat{fdef=Fdef,inline=I,modified=M}) ->
		case M of
		    true -> sys_core_fold:function(Fdef);
		    false -> Fdef
		end
	end, Fs3).

%% is_inlineable(Fstat, Thresh, [Inline]) -> bool().

is_inlineable(#fstat{weight=W}, Thresh, Ofs) when W =< Thresh -> true;
is_inlineable(#fstat{func=F,arity=A}, Thresh, Ofs) ->
    member({F,A}, Ofs).

%% inline_inline(Ifun, [Inline]) -> Ifun.
%%  Try to inline calls in an inlineable function.  To save us from a
%%  to great code explosion we only inline functions "smaller" than
%%  ourselves.

inline_inline(#ifun{body=B,weight=Iw}=If, Is) ->
    Inline = fun (#c_call{op=#c_local{name=F,arity=A},args=As}=Call) ->
		     case find_inl(F, A, Is) of
			 #ifun{vars=Vs,body=B,weight=W} when W < Iw ->
			     #c_let{vars=Vs,
				     arg=core_lib:make_values(As),
				     body=B};
			 Other -> Call
		     end;
		 (Core) -> Core
	     end,
    If#ifun{body=core_lib:map(Inline, B)}.

%% inline_func(Fstat, [Inline]) -> Fstat.
%%  Try to inline calls in a normal function.  Here we inline anything
%%  in the inline list.

inline_func(#fstat{fdef=F0}=Fstat, Is) ->
    Inline = fun (#c_call{op=#c_local{name=F,arity=A},args=As}=Call, Mod) ->
		     case find_inl(F, A, Is) of
			 #ifun{vars=Vs,body=B} ->
			     {#c_let{vars=Vs,
				     arg=core_lib:make_values(As),
				     body=B},
			      true};			%Have modified
			 Other -> {Call,Mod}
		     end;
		 (Core, Mod) -> {Core,Mod}
	     end,
    {F1,Mod} = core_lib:mapfold(Inline, false, F0),
    Fstat#fstat{fdef=F1,modified=Mod}.

weight_func(Core, Acc) -> Acc + 1.

%% match_fail_fun() -> fun/1.
%% Return a function to use with map to fix inlineable functions
%% function_clause match_fail (if they have one).

match_fail_fun() ->
    fun (#c_call{op=#c_internal{name=match_fail,arity=1},
		 args=[#c_tuple{es=[#c_atom{name=function_clause}|As]}]}=C) ->
	    Fail = #c_tuple{es=[#c_atom{name=case_clause},
				#c_tuple{es=As}]},
	    C#c_call{args=[Fail]};
	(Other) -> Other
    end.

%% find_inl(Func, Arity, [Inline]) -> #ifun{} | no.

find_inl(F, A, [#ifun{func=F,arity=A}=If|Is]) -> If;
find_inl(F, A, [If|Is]) -> find_inl(F, A, Is);
find_inl(F, A, []) -> no.
