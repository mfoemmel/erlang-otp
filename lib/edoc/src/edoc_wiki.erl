%% =====================================================================
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% $Id: edoc_wiki.erl,v 1.2 2004/08/24 23:57:40 richardc Exp $
%%
%% @private
%% @copyright 2001-2003 Richard Carlsson
%% @author Richard Carlsson <richardc@csd.uu.se>
%% @see edoc
%% @end
%% =====================================================================

%% @doc EDoc wiki expansion.

%% Whatever happens in this module, it must interact nicely with the
%% following XML-parsing. E.g. auto-detecting paragraph breaks would be
%% nice, but could be tricky to get right.
%%
%% At present, all we handle is `...' and ``...'' quoting with automatic
%% XML-escaping. To produce a single "`" character not within
%% "<code>...</code>", write "`'". Whitespace is automatically removed
%% from the beginning and the end of the quoted strings, so e.g. "` ` '"
%% will translate to "<code>`</code>", and "`` ' ''" to
%% "<code>'</code>". A quoted Erlang atom can be written as "``
%% 'frob@blah' ''" (note the spaces).

-module(edoc_wiki).

-export([expand/2]).

-include("edoc.hrl").


%% Expand wiki stuff in arbitrary text.

expand(Cs, L) ->
    lists:reverse(expand(Cs, L, [])).

%% Interestingly, the reverse of "code" is "edoc". :-)

expand([$`, $' | Cs], L, As) ->
    expand(Cs, L, [$` | As]);
expand([$`, $` | Cs], L, As) ->
    expand_double(edoc_lib:strip_space(Cs), L, ">edoc<" ++ As);
expand([$` | Cs], L, As) ->
    expand_single(edoc_lib:strip_space(Cs), L, ">edoc<" ++ As);
expand([$\n = C | Cs], L, As) ->
    expand(Cs, L + 1, [C | As]);
expand([C | Cs], L, As) ->
    expand(Cs, L, [C | As]);
expand([], _, As) ->
    As.

expand_single(Cs, L, As) ->
    expand_single(Cs, L, As, L).

expand_single([$' | Cs], L, As, _L0) ->
    expand(Cs, L, ">edoc/<" ++ edoc_lib:strip_space(As));
expand_single([$< | Cs], L, As, L0) ->
    expand_single(Cs, L, ";tl&" ++ As, L0);
expand_single([$> | Cs], L, As, L0) ->
    expand_single(Cs, L, ";tg&" ++ As, L0);
expand_single([$& | Cs], L, As, L0) ->
    expand_single(Cs, L, ";pma&" ++ As, L0);
expand_single([$\n = C | Cs], L, As, L0) ->
    expand_single(Cs, L + 1, [C | As], L0);
expand_single([C | Cs], L, As, L0) ->
    expand_single(Cs, L, [C | As], L0);
expand_single([], L, _, L0) ->
    throw_error(L0, {"`-quote ended unexpectedly at line ~w.", [L]}).


expand_double(Cs, L, As) ->
    expand_double(Cs, L, As, L).

expand_double([$', $' | Cs], L, As, _L0) ->
    expand(Cs, L, ">edoc/<" ++ edoc_lib:strip_space(As));
expand_double([$< | Cs], L, As, L0) ->
    expand_double(Cs, L, ";tl&" ++ As, L0);
expand_double([$> | Cs], L, As, L0) ->
    expand_double(Cs, L, ";tg&" ++ As, L0);
expand_double([$& | Cs], L, As, L0) ->
    expand_double(Cs, L, ";pma&" ++ As, L0);
expand_double([$\n = C | Cs], L, As, L0) ->
    expand_double(Cs, L + 1, [C | As], L0);
expand_double([C | Cs], L, As, L0) ->
    expand_double(Cs, L, [C | As], L0);
expand_double([], L, _, L0) ->
    throw_error(L0, {"``-quote ended unexpectedly at line ~w.", [L]}).



throw_error(L, D) ->
    throw({error, L, D}).
