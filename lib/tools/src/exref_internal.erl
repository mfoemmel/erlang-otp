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
%%
%% Description:
%%
%% Contains the function internal.
%%
%% !!! Note !!!
%% What is an internal function?
%% Well, that depends on the runtime system.
%% The set of BIFs may vary between installations;
%% a function can be internal or undefined depending
%% on where it is called (in the context of which
%% system).
%% So how should internal behave?
%% It can either return true if the functor/arity
%% is included in the set of {BIFs, optional BIFs},
%% even if the system doesn't supply such a BIF.
%% It is then possible to use a static enumeration
%% of all possible BIFs. In this case it is possible
%% that exref does not correctly diagnose undefined
%% functions.
%% Or internal could return true only when the system
%% supports a BIF denominated by functor/arity. This
%% could be accomplished by creating an enumeration of
%% BIFs individually for every installation. Or one
%% can try to call the BIF and see what happens (using
%% catch apply(erlang, F, A)).
%% There are three problems with this:
%% 1) It doesn't work perfectly. It is possible to check
%%    whether erlang:hash/2 is supplied, but there is no
%%    way to tell if hash/2 is supplied. The reason for
%%    this is that it's impossible to use apply without
%%    a module name, and prefixing with erlang would not
%%    be correct if the analysed code does not prefix with
%%    erlang.
%% 2) There could be nasty side effects from calling a BIF.
%% 3) Even if exref would correctly diagnose undefined
%%    functions on the runtime system where it was currently
%%    executing, this might not be what the user was
%%    interested in. What if the analysed program is
%%    intended to run on another runtime system (one
%%    on which applications like exref can't be executed).
%%    Then exref does not find undefined functions
%%    correctly for the intended target system.
%%
%% See also the notes on internal/2 and internal/3.
%%

-module(exref_internal).
-export([internal/2, internal/3]).




% internal/2
%
% Returns true if Functor/Arity is to be regarded as
% an erlang internal function, which is true for:
% - functions recognised by erl_internal:bif/2
% - guards (found with erl_internal:type_test/2).
%
% Usage:
% internal(Functor, Arity) ->
%     true | false
%
% NOTE:
% It is assumed that all BIFs recognised by erl_internal:bif/2, and
% all type tests recognised by erl_internal:type_test/2 are compulsory
% for every system. internal/2 will not behave correctly if:
% - Functor/Arity is a BIF recognised by erl_internal:bif/2, but
%   still not provided.
% - Functor/Arity is a type test recognised by erl_internal:type_test/2,
%   but still not provided.
%
% It is also assumed that all compulsory BIFs and type tests are
% recognised by erl_internal:bif/2 and erl_internal:type_test/2.
% internal/2 will not behave correctly if:
% - Functor/Arity is a BIF or type test that can be called without
%   prefixing it with "erlang:", but is not recognised by
%   erl_internal:bif/2 or erl_internal:type_test/2.
%
internal(Functor, Arity) ->
    case erl_internal:bif(Functor, Arity) of
	true ->
	    true;
	false ->
	    case erl_internal:type_test(Functor, Arity) of
		true ->
		    true;
		false ->
		    false
	    end
    end.


% internal/3
%
% Returns true if Module:Functor/Arity is to be regarded as
% an erlang internal function, which is true for:
% - guards (found with erl_internal:type_test/2).
% - functions that are defined so that they can be called with
%   apply(erlang, Name, Arity) without returning {'EXIT', {undef, _}}.
%
% Usage:
% internal(Functor, Arity) ->
%     true | false
%
% NOTE:
% The use of apply may cause undesired side effects!!!
%
% It is assumed that all BIFs recognised by erl_internal:bif/2, and
% all type tests recognised by erl_internal:type_test/2 are compulsory
% for every system. internal/3 will not behave correctly if:
% - Functor/Arity is a BIF recognised by erl_internal:bif/2, but
%   still not provided.
% - Functor/Arity is a type test recognised by erl_internal:type_test/2,
%   but still not provided.
%
internal(erlang, Functor, Arity)  ->
    case erl_internal:bif(Functor, Arity) of
	true ->
	    true;
	false ->
	    case erl_internal:type_test(Functor, Arity) of
		true ->
		    true;
		false ->
		    code:ensure_loaded(erlang),
		    case catch apply(erlang, Functor, seq(1, Arity)) of
			{'EXIT', {undef, _}} ->
			    false;
			_Other ->
			    true
		    end
	    end
    end;
internal(Module, Functor, Arity) ->
    false.


% seq/2
%
% Returns a list of integers in sequence from lower to
% upper.
% Example: seq(1, 3) -> [1, 2, 3]
%          seq(1, 1) -> [1]
%          seq(1, 0) -> []
%
% Usage:
% seq(integer(), integer()) ->
%     [integer()]
%
seq(Lower, Upper) when Lower > Upper ->
    [];
seq(Lower, Upper) ->
    lists:seq(Lower, Upper).
