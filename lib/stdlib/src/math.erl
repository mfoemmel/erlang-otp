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
-module(math).

-export([pi/0,acos/1,acosh/1,asin/1,asinh/1,atan/1,atanh/1,
	 atan2/2,cos/1,cosh/1,erf/1,erfc/1,exp/1,
	 log/1,log10/1,pow/2,sin/1,sinh/1,sqrt/1,tan/1,tanh/1]).

pi() -> 3.1415926535897932.

cos(X) ->     erlang:m_cos(X).
cosh(X) ->    erlang:m_cosh(X).
sin(X) ->     erlang:m_sin(X).
sinh(X) ->    erlang:m_sinh(X).
tan(X) ->     erlang:m_tan(X).
tanh(X) ->    erlang:m_tanh(X).
acos(X) ->    erlang:m_acos(X).
acosh(X) ->   erlang:m_acosh(X).
asin(X) ->    erlang:m_asin(X).
asinh(X) ->   erlang:m_asinh(X).
atan(X) ->    erlang:m_atan(X).
atanh(X) ->   erlang:m_atanh(X).
erf(X) ->     erlang:m_erf(X).
erfc(X) ->    erlang:m_erfc(X).
exp(X) ->     erlang:m_exp(X).
log(X) ->     erlang:m_log(X).
log10(X) ->   erlang:m_log10(X).
sqrt(X) ->    erlang:m_sqrt(X).
pow(X,Y) ->   erlang:m_pow(X,Y).
atan2(X,Y) -> erlang:m_atan2(X, Y).


