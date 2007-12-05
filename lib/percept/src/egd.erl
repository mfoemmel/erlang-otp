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

%% @author Björn-Egil Dahlberg
%% @doc Library egd is an interface for the gd library. Currently only a
%% 	subset of gd's functions are implemented. The egd module should be
%%	considered experimental and behaviour may change in future releases.

-module(egd).

-export([create/2, destroy/1]).
-export([image/2, save/2, resample/3, rotate/2]).

-export([	
	pixel/3, 
	line/4, 
	color/2, 
	color/4, 
	text/5, 
	textUp/5, 
	fontSize/2, 
	fill/3
	]).
-export([
	rectangle/4, 
	polygon/3,
	arc/7
	]).
-export([
	filledRectangle/4, 
	filledPolygon/3, 
	filledArc/8, 
	filledEllipse/5
	]).

%%==========================================================================
%%
%% 		Definitions 
%%
%%==========================================================================

-define(egd_create(), 1).
-define(egd_color(), 5).
-define(egd_pixel(), 10).
-define(egd_line(), 13).
-define(egd_rectangle(), 14).
-define(egd_filled_rectangle(), 15).
-define(egd_polygon(), 16).
-define(egd_filled_polygon(), 17).
-define(egd_arc(), 18).
-define(egd_filled_arc(), 19).
-define(egd_filled_ellipse(), 20).
-define(egd_text(), 21).
-define(egd_text_up(), 22).
-define(egd_fill(), 23).
-define(egd_resample(), 60).
-define(egd_rotate(), 61).
-define(egd_font_size(), 62).
-define(egd_gif(), 100).
-define(egd_jpeg(), 101).
-define(egd_png(), 102).

%%==========================================================================
%%
%% 		Type definitions 
%%
%%==========================================================================

%% @type image(). Image reference. Do not assume anything about the refrence
%%	data structure. It may change in future releases.
%% @type color(). Color reference. Do not assume anything about the refrence
%%      data structure. It may change in future releases.
%% @type point() = {integer(), integer()}. Point tuple with X and Y coordinates.
%% @type font() = tiny | small | medium | large | giant. Font atoms.

-type(image() :: port()).
-type(color() :: integer()).
-type(point() :: {non_neg_integer(), non_neg_integer()}).
-type(font() :: 'tiny' | 'small' | 'medium' | 'large' | 'giant').

%%==========================================================================
%%
%% 		Interface functions
%%
%%==========================================================================

%% @spec create(integer(), integer()) -> image()
%% @doc Creates an images with specified dimensions. The color palette is in
%% 	truecolor.

-spec(create/2 :: (
	Width :: pos_integer(),
	Height :: pos_integer()) ->
	image()).

create(Width,Height) ->
    ok = load_egd_driver(),
    Image = open_port({spawn, "egd_drv"}, [binary, {packet,2}]),
    call_image(Image, [?egd_create(),round(Width),round(Height)]),
    Image.

%% @spec destroy(image()) -> ok | error
%% @doc Destroys specified image.

-spec(destroy/1 :: (Image :: image()) -> 'ok' | 'error').

destroy(Image) ->
    Image ! {self(), close},
    receive
    	{Image, closed} -> ok;
	_ -> error
    end.

%% @spec image(image(), Type) -> binary() 
%%	Type = gif | png | jpeg | {jpeg, Quality}
%%	Quality = integer()
%% @doc Returns a binary containing the image data.

-spec(image/2 :: (
	Image :: image(),
	Type :: 'gif' | 'png' | 'jpeg' | {'jpeg', 0..100}) ->
	binary() | iolist()).

image(Image, Type) ->
    case Type of 
        gif -> call_image(Image, [?egd_gif()]);
	{jpeg, Quality} -> call_image(Image, [?egd_jpeg(), Quality]);
	jpeg -> call_image(Image, [?egd_jpeg(), 100]);
	png -> call_image(Image, [?egd_png()])
    end.

%% ------------------------
%% Coloring
%% ------------------------

%% @spec color(image(), byte(), byte(), byte()) -> color()
%% @doc Returns a color reference.

-spec(color/4 :: (
	Image :: image(),
	R :: 0..255,
	G :: 0..255,
	B :: 0..255) ->
	color()).

color(Image, R,G,B) ->
    [Color] = decode32(call_image(Image, [?egd_color(),R,G,B])),
    Color.

%% @spec color(image(), RGB) -> Color
%%	RGB = white | black | red | blue | green | {byte(), byte(), byte()} 
%%	Color = color() | {error, {invalid_color, RGB}}
%% @doc Returns a color reference.

-spec(color/2 :: (
	Image :: image(),
	Color :: atom() | {0..255,0..255,0..255}) ->
	color() | {error, any()}).

color(Image, RGB) when is_atom(RGB) ->
    case RGB of
    	white -> color(Image, 255,255,255);
	black -> color(Image, 0,0,0);
	red -> color(Image, 255,0,0);
	blue -> color(Image, 0,0,255);
	green -> color(Image, 0,255,0);
	_-> {error, {invalid_color, RGB}}
    end;
color(Image, {R,G,B}=RGB) when is_tuple(RGB) ->
    color(Image, R,G,B).

%% @spec fill(image(), Center::point(), color()) -> ok
%% @doc Fills an enclosed area with Center as starting point.

-spec(fill/3 :: (
	Image :: image(),
	Point :: point(),
	Color :: color()) ->
	ok).

fill(Image, {X,Y}, Color) ->
    call_image(Image, [?egd_fill(), X, Y, Color]),
    ok.
  

%% ------------------------
%% Manipulating images
%% ------------------------


%% @spec resample(image(), integer(), integer()) -> ok
%% @doc Resizes the image to the new dimensions specified by Width and Height.

-spec(resample/3 :: (
	Image :: image(),
	Width :: non_neg_integer(),
	Height :: non_neg_integer()) ->
	ok).

resample(Image, Width, Height) ->
    call_image(Image, [?egd_resample(), Width, Height]),
    ok.

%% @spec rotate(image(), integer()) -> ok
%% @doc Rotates the image Angle degrees (not radians).
%% @todo Angle should be float().

-spec(rotate/2 :: (
	Image :: image(),
	Angle :: integer()) ->
	ok).

rotate(Image, Angle) ->
    call_image(Image, [?egd_rotate(), Angle]),
    ok.

%% ------------------------
%% Drawing functions
%% ------------------------


%% @spec pixel(image(), Point::point(), color()) -> ok
%% @doc Sets a pixel in the image with color Color.

-spec(pixel/3 :: (
	Image :: image(),
	Point :: point(),
	Color :: color()) ->
	ok).

pixel(Image, {X, Y}, Color) ->
    call_image(Image, [?egd_pixel(), X, Y, Color]),
    ok.

%% @spec line(image(), Start::point(), End::point(), color()) -> ok
%% @doc Draws a line from (X1,Y1) to (X2,Y2) in the image with color Color.

-spec(line/4 :: (
	Image :: image(),
	Point1 :: point(),
	Point2 :: point(),
	Color :: color()) ->
	ok).

line(Image, {X1, Y1}, {X2, Y2}, Color) ->
    call_image(Image, [?egd_line(), X1, Y1, X2, Y2, Color]),
    ok.

%% @spec arc(image(), Center::point(), integer(), integer(), integer(), integer(), color()) -> ok
%% @doc Draws an arc with centerpoint in Center, width Width and height Height. 
%% Start and End are degrees (not radians) of the arc.

-spec(arc/7 :: (
	Image :: image(),
	Center :: point(),
	Width :: pos_integer(),
	Height :: pos_integer(),
	Start :: integer(),
	End :: integer(),
	Color :: color()) ->
	ok).	

arc(Image, {Cx, Cy}, Width, Height, Start, End, Color) ->
    call_image(Image, [?egd_arc(), Cx, Cy, Width, Height, Start, End, Color]),
    ok.

%% @spec polygon(image(), [points()], color()) -> ok
%% @doc Draws a polygon in the image with color Color. The last point and first points forms the polygon closure.

-spec(polygon/3 :: (
	Image :: image(),
	Points :: [point()],
	Color :: color()) ->
	ok).

polygon(Image, Points, Color) ->
    call_image(Image, [?egd_polygon(), Color] ++ points_to_list(Points)),
    ok.

%% @spec rectangle(image(), Start::point(), End::point(), color()) -> ok
%% @doc Draws a rectangle within the specified coordinates.

-spec(rectangle/4 :: (
	Image :: image(),
	Start :: point(),
	End :: point(),
	Color :: color()) ->
	ok).

rectangle(Image, {X1, Y1}, {X2, Y2}, Color) ->
    call_image(Image, [?egd_rectangle(), X1, Y1, X2, Y2, Color]),
    ok.

%% @spec filledArc(image(), Center::point(), integer(), integer(), integer(), integer(), color(), Options) -> ok
%%	Options = [Option]
%%	Option = arc | chord | no_fill | edged 
%% @doc Draws a filled arc with centerpoint in (Cx, Cy), width Width and height Height. Start and End are degrees (not radians) of the arc.

-spec(filledArc/8 :: (
	Image :: image(),
	Center :: point(),
	Width :: pos_integer(),
	Height :: pos_integer(),
	Start :: integer(),
	End :: integer(),
	Color :: color(),
	Options :: [atom()]) ->
	ok).

filledArc(Image, {Cx, Cy}, Width, Height, Start, End, Color, Options) ->
    Style = get_arc_options(Options),
    call_image(Image, [?egd_filled_arc(), Cx, Cy, Width, Height, Start, End, Color, Style]),
    ok.

%% @spec filledEllipse(image(), Center::point(), integer(), integer(), color()) -> ok
%% @doc Draws a filled ellipse with color Color and center point at (Cx, Cy) using specified dimensions.

-spec(filledEllipse/5 :: (
	Image :: image(),
	Center :: point(),
	Width :: pos_integer(),
	Height :: pos_integer(),
	Color :: color()) ->
	ok).

filledEllipse(Image, {Cx, Cy}, Width, Height, Color) ->
    call_image(Image, [?egd_filled_ellipse(), Cx, Cy, Width, Height, Color]),
    ok.

%% @spec filledPolygon(image(), [point()], color()) -> ok
%% @doc Draws a filled polygon in the image with color Color. The last point and first points forms the polygon closure.

-spec(filledPolygon/3 :: (
	Image :: image(),
	Points :: [point()],
	Color :: color()) ->
	ok).

filledPolygon(Image, Points, Color) ->
    call_image(Image, [?egd_filled_polygon(), Color] ++ points_to_list(Points)),
    ok.

%% @spec filledRectangle(image(), Start::point(), End::point(), color()) -> ok
%% @doc Draws a filled rectangle within the specified coordinates.

-spec(filledRectangle/4 :: (
	Image :: image(),
	Point1 :: point(),
	Point2 :: point(),
	Color :: color()) ->
	ok).	

filledRectangle(Image, {X1, Y1}, {X2, Y2}, Color) ->
    call_image(Image, [?egd_filled_rectangle(), X1, Y1, X2, Y2, Color]),
    ok.

% text

%% @spec text(image(), font(), Point::point(), string(), color()) -> ok
%% @doc Draws a horizontal text strip using specified font and color.

-spec(text/5 :: (
	Image :: image(),
	Font :: font(),
	Point :: point(),
	String :: string(),
	Color :: color()) ->
	ok).	

text(Image, Font, {X,Y}, String, Color) ->
    Size = font2size(Font),
    call_image(Image, [?egd_text(), X, Y, Size, Color, length(String)] ++ lists:flatten(String)),
    ok.

%% @spec textUp(image(), font(), Point::point(), string(), color()) -> ok
%% @doc Draws a vertical text strip using specified font and color.

-spec(textUp/5 :: (
	Image :: image(),
	Font :: font(),
	Point :: point(),
	String :: string(),
	Color :: color()) ->
	ok).

textUp(Image, Font, {X,Y}, String, Color) ->
    Size = font2size(Font),
    call_image(Image, [?egd_text_up(), X, Y, Size, Color, length(String)] ++ lists:flatten(String)),
    ok.

%% @spec fontSize(image(), font()) -> {Width, Height}
%%	Widht = integer()
%%	Height = integer()
%% @doc Returns the width and height of the font in pixel units.

-spec(fontSize/2 :: (
	Image :: image(),
	Font :: font()) ->
	{non_neg_integer(), non_neg_integer()}).

fontSize(Image, Font) ->
    Size = font2size(Font),
    Binary = call_image(Image, [?egd_font_size(), Size]),
    [W, H] = decode32(Binary),
    {W,H}.

%% ------------------------
%% File handling
%% ------------------------

%% @spec save(binary(), string()) -> {ok, Filename} | {error, Reason}
%% @doc Saves the binary of the image to a file.

-spec(save/2 :: (
	ImageBinary :: binary(),
	Filename :: string()) ->
	{ok, string()} | {error, any()}).


save(ImageBinary, Filename) ->
    case file:open(Filename, [binary, write]) of
        {ok, Dev} ->
    	    Image = binary_to_list(ImageBinary),
	    io:put_chars(Dev, Image),
	    file:close(Dev),
	    {ok, Filename};
	Error -> Error
    end.

%%==========================================================================
%%
%% 		Auxiliary functions 
%%
%%==========================================================================

font2size(Font) ->
    case Font of
        tiny -> 1;
	small -> 2;
	medium -> 3;
	large -> 4;
	giant -> 5;
	_ -> 3 
    end.

% for serial points

points_to_list(Points) -> 
    Length = length(Points),
    points_to_list(Points, [Length]).

points_to_list([], Out) -> lists:reverse(Out);
points_to_list([{X,Y}|Points], Out) ->
    points_to_list(Points, [Y, X | Out]).

get_arc_options(Options) -> get_arc_options(Options, 0).
get_arc_options([], Res) -> Res;
get_arc_options([Option|Options], Res) ->
    case Option of 
        arc -> get_arc_options(Options, Res + 1);
        chord -> get_arc_options(Options, Res + 2);
        no_fill -> get_arc_options(Options, Res + 4);
        edged -> get_arc_options(Options, Res + 8)
    end.

% Port communication

call_image(Image, Command) ->
    Bytes = encode32(Command),
    Image ! {self(),{command, Bytes}},
    receive
	{Image, {data, Data}} -> Data
    end.

% encode/decode

encode32(List) -> encode32(List, []).
encode32([], Bytes) -> lists:reverse(Bytes);
encode32([H|T], Bytes) -> 
    B0 = H band 255,
    B1 = (H bsr 8) band 255,
    B2 = (H bsr 16) band 255,
    B3 = (H bsr 24) band 255,
    % Bits needs to be in reverse order
    % lists:reverse reorders everything
    encode32(T, [B0,B1,B2,B3|Bytes]).

decode32(Binary) -> decode32(binary_to_list(Binary), []).
decode32([], Values) -> lists:reverse(Values);
decode32([B3,B2,B1,B0| Tail], Values) ->
    Value = (B3 bsl 24) + (B2 bsl 16) + (B1 bsl 8) + B0,
    decode32(Tail, [Value | Values]).

%% Driver

load_egd_driver() ->
    Arch = erlang:system_info(system_architecture),
    LibPath = filename:join([code:priv_dir(percept), "lib"]),
    OpenPath = filename:join([LibPath, Arch ]),

    Driver = "egd_drv",

    case erl_ddll:load_driver(LibPath, Driver) of
    	ok -> ok;
	{error, already_loaded} -> ok;
	{error, _} -> 
	    case erl_ddll:load_driver(OpenPath, Driver) of
		ok -> ok;
		{error, already_loaded} -> ok;
		Unhandled -> Unhandled
	    end
    end.
