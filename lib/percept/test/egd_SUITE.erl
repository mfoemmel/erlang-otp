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
-module(egd_SUITE).
-include("test_server.hrl").

%% Test server specific exports
-export([all/1]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([libgd/1, fini/1]).

%% Test cases
-export([
	image_create_and_destroy/1, 
	image_shape/1, 
	image_colors/1, 
	image_font/1,
	image_pixels/1,
	image_gif_compliant/1,
	image_jfif_compliant/1,
	image_png_compliant/1
	]).

%% Default timetrap timeout (set in init_per_testcase)
-define(default_timeout, ?t:minutes(1)).

init_per_suite(Config) when is_list(Config) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    Config.

end_per_suite(Config) when is_list(Config) ->
    Config.

init_per_testcase(_Case, Config) ->
    Dog = ?t:timetrap(?default_timeout),
    [{max_size, 300}, {watchdog,Dog} | Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

all(suite) ->
    % Test cases
    [	{conf, libgd, [
	image_create_and_destroy, 
	image_shape, 
	image_colors, 
	image_font, 
	image_pixels,
	image_gif_compliant,
	image_png_compliant,
	image_jfif_compliant
	], fini}
    ].

libgd(Config) when is_list(Config) ->
    case has_libgd() of
	true ->
	    Config;
	_ ->
	    {skip, "egd is not available without libgd"}
    end.

fini(Config) when is_list(Config) ->
    Config.

%%----------------------------------------------------------------------
%% Tests
%%----------------------------------------------------------------------

image_create_and_destroy(suite) ->
    [];
image_create_and_destroy(doc) ->
    ["Image creation and destroy test."];
image_create_and_destroy(Config) when is_list(Config) ->
    {W,H} = get_size(?config(max_size, Config)),
    ?line Image = egd:create(W, H),
    ?line ok = egd:destroy(Image),
    ok.

image_colors(suite) ->
    [];
image_colors(doc) ->
    ["Image color test."];
image_colors(Config) when is_list(Config) ->
    {W,H} = get_size(?config(max_size, Config)),
    ?line Image = egd:create(W, H),
    put(image_size, {W,H}),

    RGB = get_rgb(),
    ?line Black = egd:color(Image, black),
    ?line Red = egd:color(Image, red),
    ?line Green = egd:color(Image, green),
    ?line Blue = egd:color(Image, blue),
    ?line Random = egd:color(Image, RGB),  

    ?line ok = egd:line(Image, get_point(), get_point(), Random),
    ?line ok = egd:line(Image, get_point(), get_point(), Red),
    ?line ok = egd:line(Image, get_point(), get_point(), Green),
    ?line ok = egd:line(Image, get_point(), get_point(), Black),
    ?line ok = egd:line(Image, get_point(), get_point(), Blue),
 
    ?line ok = egd:destroy(Image),
    erase(image_size),
    ok.

image_shape(suite) ->
    [];
image_shape(doc) ->
    ["Image shape api test."];
image_shape(Config) when is_list(Config) ->
    {W,H} = get_size(?config(max_size, Config)),
    put(image_size, {W,H}),
    ?line Im = egd:create(W, H),
    
    ?line Bgc = egd:color(Im, white),
    ?line Fgc = egd:color(Im, black),

    ?line ok = egd:fill(Im, get_point(), Bgc),
    ?line ok = egd:pixel(Im, get_point(), Fgc),
    ?line ok = egd:line(Im, get_point(), get_point(), Fgc), 
    ?line ok = egd:rectangle(Im, get_point(), get_point(), Fgc),
    ?line ok = egd:polygon(Im, get_points(random:uniform(20)), Fgc),
    {X, Y} = get_size(?config(max_size, Config)),
    ?line ok = egd:arc(Im, get_point(), X, Y, get_angle(), get_angle(), Fgc),
    ?line ok = egd:filledRectangle(Im, get_point(), get_point(), Fgc),
    ?line ok = egd:filledPolygon(Im, get_points(random:uniform(20)), Fgc), 
    ?line ok = egd:filledArc(Im, get_point(), X, Y, get_angle(), get_angle(), Fgc, [chord, edged]),
    ?line ok = egd:filledEllipse(Im, get_point(), X, Y, Fgc),

    ?line ok = egd:destroy(Im),
    erase(image_size),
    ok.
     
image_font(suite) ->
    [];
image_font(doc) ->
    ["Image font test."];
image_font(Config) when is_list(Config) ->
    {W,H} = get_size(?config(max_size, Config)),
    put(image_size, {W,H}),
    ?line Im = egd:create(W, H),
    ?line Bgc = egd:color(Im, white),
    ?line Fgc = egd:color(Im, black),
    ?line ok = egd:fill(Im, get_point(), Bgc),
    
    % textit
    lists:foreach(
	fun(Font) ->
	    ?line ok = egd:text(Im, Font, get_point(), "Hello World", Fgc),
	    ?line ok = egd:textUp(Im, Font, get_point(), "My World", Fgc),
	    ?line {_,_} = egd:fontSize(Im, Font)
	end, [tiny, small, medium, large, giant]), 
    
    ?line ok = egd:destroy(Im),
    erase(image_size),
    ok.

image_pixels(suite) ->
    [];
image_pixels(doc) ->
    ["Image pixels test."];
image_pixels(Config) when is_list(Config) ->
    {W,H} = get_size(?config(max_size, Config)),
    put(image_size, {W,H}),
    ?line Im = egd:create(W, H),
    
    Points = [ {X,Y} || 
	X <- lists:seq(0, W - 1),
	Y <- lists:seq(0, H - 1)],

    lists:foreach(
	fun(Point) ->
	    RGB = get_rgb(),
	    ?line Color = egd:color(Im, RGB),
	    ?line ok = egd:pixel(Im, Point, Color)
	end, Points), 

    ?line ok = egd:destroy(Im),
    erase(image_size),
    ok.

image_gif_compliant(suite) ->
    [];
image_gif_compliant(doc) ->
    ["Image gif compliant test."];
image_gif_compliant(Config) when is_list(Config) ->
    {W,H} = get_size(?config(max_size, Config)),
    put(image_size, {W,H}),
    ?line Im = egd:create(W, H),
    ?line Bgc = egd:color(Im, white),
    ?line Fgc = egd:color(Im, black),
    ?line ok = egd:fill(Im, get_point(), Bgc),
    ?line ok = egd:filledRectangle(Im, get_point(), get_point(), Fgc),

    ?line Bin = egd:image(Im, gif),
    ?line true = binary_is_gif_compliant(Bin),
    
    ?line ok = egd:destroy(Im),
    erase(image_size),
    ok.
     
image_png_compliant(suite) ->
    [];
image_png_compliant(doc) ->
    ["Image png compliant test."];
image_png_compliant(Config) when is_list(Config) ->
    {W,H} = get_size(?config(max_size, Config)),
    put(image_size, {W,H}),
    ?line Im = egd:create(W, H),
    ?line Bgc = egd:color(Im, white),
    ?line Fgc = egd:color(Im, black),
    ?line ok = egd:fill(Im, get_point(), Bgc),
    ?line ok = egd:filledRectangle(Im, get_point(), get_point(), Fgc),
    
    ?line Bin = egd:image(Im, png),
    ?line true = binary_is_png_compliant(Bin),
    
    ?line ok = egd:destroy(Im),
    erase(image_size),
    ok.

image_jfif_compliant(suite) ->
    [];
image_jfif_compliant(doc) ->
    ["Image jfif compliant test."];
image_jfif_compliant(Config) when is_list(Config) ->
    {W,H} = get_size(?config(max_size, Config)),
    put(image_size, {W,H}),
    ?line Im = egd:create(W, H),
    ?line Bgc = egd:color(Im, white),
    ?line Fgc = egd:color(Im, black),
    ?line ok = egd:fill(Im, get_point(), Bgc),
    ?line ok = egd:filledRectangle(Im, get_point(), get_point(), Fgc),
    
    ?line Bin = egd:image(Im, jpeg),
    ?line true = binary_is_jfif_compliant(Bin),
    
    ?line ok = egd:destroy(Im),
    erase(image_size),
    ok.

%%----------------------------------------------------------------------
%% Auxiliary tests
%%----------------------------------------------------------------------

%% jfif header by specification
%% 2 bytes, length
%% 5 bytes, identifier ="JFIF\0"
%% 2 bytes, version, (major, minor)
%% 1 byte , units
%% However, JFIF seems to start at 6 (7 with 1-index)?
   
binary_is_jfif_compliant(JpegBin) ->
    ?line {Bin, _} = split_binary(JpegBin, 11),
    List = binary_to_list(Bin),
    case lists:sublist(List, 7, 4) of 
	"JFIF" -> true;
	Other ->
	   io:format("img -> ~p~n", [Other]),
	   false
    end.

binary_is_gif_compliant(GifBin) ->
    ?line {Bin, _} = split_binary(GifBin, 10),
    List = binary_to_list(Bin),
    case lists:sublist(List, 1,5) of
	"GIF87" -> true;
	Other -> 
	   io:format("img -> ~p~n", [Other]),
	   false
    end.

binary_is_png_compliant(PngBin) ->
    ?line {Bin, _} = split_binary(PngBin, 10),
    List = binary_to_list(Bin),
    case lists:sublist(List, 2,3) of
	"PNG" -> true;
	Other ->
	   io:format("img -> ~p~n", [Other]),
	   false
    end.

%%----------------------------------------------------------------------
%% Auxiliary
%%----------------------------------------------------------------------


get_rgb() ->
   R = random:uniform(256), 
   G = random:uniform(256),   
   B = random:uniform(256),
   {R,G,B}.

get_angle() ->
   random:uniform(360).

get_point() ->
    get_point(get(image_size)).
get_point({W,H}) ->
   X = random:uniform(W),
   Y = random:uniform(H),
   {X,Y}.

get_size(Max) ->
    W = random:uniform(Max),
    H = random:uniform(Max),
    {W,H}.

get_points(N) ->
    get_points(N, []).
get_points(0, Out) ->
   Out;
get_points(N, Out) ->
    get_points(N - 1, [get_point() | Out]).

has_libgd() ->
    case code:priv_dir(percept) of
	{error, _} -> false;
	Path ->
	    Arch = erlang:system_info(system_architecture),
	    LibPath = filename:join([Path, "lib"]),
	    OpenPath = filename:join([LibPath, Arch ]),

	    Driver = "egd_drv",

	    case erl_ddll:load_driver(LibPath, Driver) of
	    	ok -> true;
		{error, already_loaded} -> true;
		{error, _} -> 
		    case erl_ddll:load_driver(OpenPath, Driver) of
			ok -> true;
			{error, already_loaded} -> true;
			_ -> false
		    end
	    end
    end.
