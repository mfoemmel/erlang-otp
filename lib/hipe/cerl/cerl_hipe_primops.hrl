%% ========================-*-erlang-*-=================================
%% Predefined Core Erlang primitive operations used by HiPE
%%
%% Copyright (C) 2000 Richard Carlsson
%%
%% This library is free software; you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public
%% License as published by the Free Software Foundation; either
%% version 2 of the License, or (at your option) any later
%% version.
%%
%% This library is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General
%% Public License along with this library; if not, write to the
%% Free Software Foundation, Inc., 59 Temple Place, Suite 330,
%% Boston, MA 02111-1307 USA
%%
%% Author contact: richardc@csd.uu.se
%% =====================================================================

%% These definitions give the internal names of primops in HiPE.  Not
%% all of these (e.g., 'not'/'and'/'or', and the type tests), are
%% recognized primops on the Icode level, but are inline-expanded by the
%% translation from Core Erlang to Icode. They are simply there to help
%% the translation.

-define(PRIMOP_NOT, 'not').			% arity 1
-define(PRIMOP_AND, 'and').			% arity 2
-define(PRIMOP_OR, 'or').			% arity 2
-define(PRIMOP_ADD, '+').			% arity 2
-define(PRIMOP_IDENTITY, identity).		% arity 1
-define(PRIMOP_SUB, '-').			% arity 2
-define(PRIMOP_NEG, neg).			% arity 1
-define(PRIMOP_MUL, '*').			% arity 2
-define(PRIMOP_DIV, '/').			% arity 2
-define(PRIMOP_INTDIV, 'div').			% arity 2
-define(PRIMOP_REM, 'rem').			% arity 2
-define(PRIMOP_BAND, 'band').			% arity 2
-define(PRIMOP_BOR, 'bor').			% arity 2
-define(PRIMOP_BXOR, 'bxor').			% arity 2
-define(PRIMOP_BNOT, 'bnot').			% arity 1
-define(PRIMOP_BSL, 'bsl').			% arity 2
-define(PRIMOP_BSR, 'bsr').			% arity 2
-define(PRIMOP_EQ, '==').			% arity 2
-define(PRIMOP_NE, '/=').			% arity 2
-define(PRIMOP_EXACT_EQ, '=:=').		% arity 2
-define(PRIMOP_EXACT_NE, '=/=').		% arity 2
-define(PRIMOP_LT, '<').			% arity 2
-define(PRIMOP_GT, '>').			% arity 2
-define(PRIMOP_LE, '=<').			% arity 2
-define(PRIMOP_GE, '>=').			% arity 2
-define(PRIMOP_IS_ATOM, 'is_atom').		% arity 1
-define(PRIMOP_IS_BIGNUM, 'is_bignum').		% arity 1
-define(PRIMOP_IS_BINARY, 'is_binary').		% arity 1
-define(PRIMOP_IS_CONSTANT, 'is_constant').	% arity 1
-define(PRIMOP_IS_FIXNUM, 'is_fixnum').		% arity 1
-define(PRIMOP_IS_FLOAT, 'is_float').		% arity 1
-define(PRIMOP_IS_FUNCTION, 'is_function').	% arity 1
-define(PRIMOP_IS_INTEGER, 'is_integer').	% arity 1
-define(PRIMOP_IS_LIST, 'is_list').		% arity 1
-define(PRIMOP_IS_NUMBER, 'is_number').		% arity 1
-define(PRIMOP_IS_PID, 'is_pid').		% arity 1
-define(PRIMOP_IS_PORT, 'is_port').		% arity 1
-define(PRIMOP_IS_REFERENCE, 'is_reference').	% arity 1
-define(PRIMOP_IS_TUPLE, 'is_tuple').		% arity 1
-define(PRIMOP_EXIT, exit).			% arity 1
-define(PRIMOP_THROW, throw).			% arity 1
-define(PRIMOP_ERROR, error).			% arity 1
-define(PRIMOP_RECEIVE_SELECT, receive_select).	% arity 0
-define(PRIMOP_RECEIVE_NEXT, receive_next).	% arity 0
-define(PRIMOP_MAKE_FUN, make_fun).		% arity 6
-define(PRIMOP_APPLY_FUN, apply_fun).		% arity 2
-define(PRIMOP_FUN_ELEMENT, closure_element).	% arity 2
