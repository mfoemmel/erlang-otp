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

-module(image).

-export([start/2, stop/1]).
-export([fill/4, line/6, color/4, text/6, fontSize/2]).
-export([rectangle/6, filledRectangle/6]).

start(W,H) ->
    egd:create(W,H).

stop(Gif) ->
    Image = egd:image(Gif, gif),
    egd:destroy(Gif),
    Image.

line(Gif, X1, Y1, X2, Y2, Color) ->
    egd:line(Gif, {X1, Y1}, {X2, Y2}, Color).

fill(Gif, X, Y, Color) ->
    egd:fill(Gif, {X, Y}, Color).

color(Gif, R, G, B) ->
    egd:color(Gif, {R, G, B}).

text(Gif, Type, X, Y, Text, Color) ->
    egd:text(Gif, Type, {X, Y}, Text, Color).

fontSize(Im, Font) ->
    egd:fontSize(Im, Font).

rectangle(Image, X0, Y0, X1, Y1, Color) ->
    egd:rectangle(Image, {X0, Y0}, {X1, Y1}, Color).

filledRectangle(Image, X0, Y0, X1, Y1, Color) ->
    egd:filledRectangle(Image, {X0, Y0}, {X1, Y1}, Color).
