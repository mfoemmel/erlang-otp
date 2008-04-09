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
%% @doc egd - erlang graphical drawer 
%%
%% 

-module(egd).

-export([create/2, destroy/1, information/1]).
-export([text/5, line/4, color/1, color/2]).
-export([rectangle/4, filledRectangle/4, filledEllipse/4]).
-export([render/1, render/2, render/3]).

-export([filledTriangle/5, polygon/3]).

-export([save/2]).

-include("egd.hrl").

%%==========================================================================
%%
%% 		Type definitions 
%%
%%==========================================================================

%% @type egd_image()
%% @type point() = {integer(), integer()}
%% @type color()  
%% @type render_option() = {render_engine, opaque} | {render_engine, alpha}

-type(egd_image() :: pid()). 
-type(point() :: {non_neg_integer(), non_neg_integer()}).
-type(render_option() :: {'render_engine', 'opaque'} | {'render_engine', 'alpha'}).
-type(color() :: {float(), float(), float(), float()}).

%%==========================================================================
%%
%%		Interface functions	
%%
%%==========================================================================

%% @spec create(integer(), integer()) -> egd_image()
%% @doc Creates an image area and returns its reference.

-spec(create/2 :: (Width :: integer(), Height :: integer()) -> 
	egd_image()).

create(Width,Height) ->
    spawn_link(fun() -> init(trunc(Width),trunc(Height)) end).


%% @spec destroy(egd_image()) -> ok
%% @doc Destroys the image.

-spec(destroy/1 :: (Image :: egd_image()) -> ok).

destroy(Image) ->
   call(Image, stop),
   ok.


%% @spec render(egd_image()) -> binary() 
%% @equiv render(Image, png, [{render_engine, opaque}])


render(Image) ->
    render(Image, png, [{render_engine, opaque}]).

%% @spec render(egd_image(), png | raw_bitmap) -> binary() 
%% @equiv render(Image, Type, [{render_engine, opaque}])

render(Image, Type) ->
    render(Image, Type, [{render_engine, opaque}]).

%% @spec render(egd_image(), png | raw_bitmap, [render_option()]) -> binary() 
%% @doc Renders a binary from the primitives specified by egd_image(). The
%% 	binary can either be a raw bitmap with rgb tripplets or a binary in png
%%	format.

-spec(render/3 :: (
	Image :: egd_image(), 
	Type :: 'png' | 'raw_bitmap',
	Options :: [render_option()]) -> binary()).

render(Image, Type, Options) ->
    {render_engine, RenderType} = proplists:lookup(render_engine, Options),
    call(Image, {render, Type, RenderType}),
    receive Binary -> Binary end,
    Binary.


%% @spec information(egd_image()) -> ok
%% @hidden
%% @doc Writes out information about the image. This is a debug feature
%%	mainly.

information(Pid) ->
    call(Pid, information),
    ok.

%% @spec line(egd_image(), point(), point, color()) -> ok
%% @doc Creates a line object from P1 to P2 in the image.

-spec(line/4 :: (
	Image :: egd_image(),
	P1 :: point(),
	P2 :: point(),
	Color :: color()) -> 'ok').

line(Image, P1, P2, Color) ->
    call(Image, {line, P1, P2, Color}),
    ok.

%% @spec color({byte(), byte(), byte()}) -> color()
%% @doc Creates a color reference.

color(Color) ->
    egd_primitives:color(Color).

%% @spec color(egd_image(), {byte(), byte(), byte()}) -> color()
%% @doc Creates a color reference.
%% @hidden

color(_Image, Color) ->
    egd_primitives:color(Color).

%% @spec text(egd_image(), point(), font(), string(), color()) -> ok
%% @doc Creates a text object.

text(Image, P, Font, Text, Color) ->
    call(Image, {text, P, Font, Text, Color}),
    ok.

%% @spec rectangle(egd_image(), point(), point(), color()) -> ok
%% @doc Creates a rectangle object.

rectangle(Image, P1, P2, Color) ->
    call(Image, {rectangle, P1, P2, Color}),
    ok.

%% @spec filledRectangle(egd_image(), point(), point(), color()) -> ok
%% @doc Creates a filled rectangle object.

filledRectangle(Image, P1, P2, Color) ->
    call(Image, {filled_rectangle, P1, P2, Color}),
    ok.

%% @spec filledEllipse(egd_image(), point(), point(), color()) -> ok
%% @doc Creates a filled ellipse object.

filledEllipse(Image, P1, P2, Color) ->
    call(Image, {filled_ellipse, P1, P2, Color}),
    ok.

%% @spec filledTriangle(egd_image(), point(), point(), point(), color()) -> ok
%% @hidden
%% @doc Creates a filled triangle object.

filledTriangle(Image, P1, P2, P3, Color) ->
    call(Image, {filled_triangle, P1, P2, P3, Color}),
    ok.

%% @spec polygon(egd_image(), [point()], color()) -> ok
%% @hidden
%% @doc Creates a filled filled polygon object.

polygon(Image, Pts, Color) ->
    call(Image, {polygon, Pts, Color}),
    ok.


%% @spec save(binary(), string()) -> ok
%% @doc Saves the binary to file. 

save(Binary, Filename) when is_binary(Binary) ->
    file:write_file(Filename, Binary),
    ok.
% ---------------------------------
% Aux functions 
% ---------------------------------

call(Pid, Command) ->
   Pid ! {self(), Command}.

% ---------------------------------
% Server loop 
% ---------------------------------

init(W,H) ->
    Image = egd_primitives:create(W,H),
    loop(Image).

loop(Image) ->
    receive
	% Quitting
	{_Pid, quit} -> ok;
	
	% Rendering
    	{Pid, {render, BinaryType, RenderType}} ->
	    case BinaryType of
		raw_bitmap ->
		    Bitmap = egd_render:binary(Image, RenderType),
		    Pid ! Bitmap,
		    loop(Image);
		png ->
		    Bitmap = egd_render:binary(Image, RenderType),
		    Png = egd_png:binary(
			Image#image.width,
			Image#image.height,
			Bitmap),
		    Pid ! Png,
		    loop(Image);
		Unhandled ->
		    Pid ! {error, {format, Unhandled}},
		    loop(Image)
	     end;

	% Drawing primitives
	{_Pid, {line, P1, P2, C}} ->
	    loop(egd_primitives:line(Image, P1, P2, C));
	{_Pid, {text, P, Font, Text, C}} ->
	    loop(egd_primitives:text(Image, P, Font, Text, C));
	{_Pid, {filled_ellipse, P1, P2, C}} ->
	    loop(egd_primitives:filledEllipse(Image, P1, P2, C));
	{_Pid, {filled_rectangle, P1, P2, C}} ->
	    loop(egd_primitives:filledRectangle(Image, P1, P2, C));
	{_Pid, {filled_triangle, P1, P2, P3, C}} ->
	    loop(egd_primitives:filledTriangle(Image, P1, P2, P3, C));
	{_Pid, {polygon, Pts, C}} ->
	    loop(egd_primitives:polygon(Image, Pts, C));
	{_Pid, {rectangle, P1, P2, C}} ->
	    loop(egd_primitives:rectangle(Image, P1, P2, C));
	{_Pid, information} ->
	    egd_primitives:info(Image),
	    loop(Image)

    end.
