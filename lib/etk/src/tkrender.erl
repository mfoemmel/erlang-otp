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
%% Interface to rendering engine
%%
%%
-module(tkrender).

-import(lists, [reverse/1]).
-export([init/0]).

-export([box/1, box/4, 
	 cone/1, cone/5,
	 cylinder/1, cylinder/6,
	 sphere/1, sphere/2]).
-export([sprite/2]).


-export([def_box/1, def_box/4, 
	 def_cone/1, def_cone/6,
	 def_cylinder/1, def_cylinder/7,
	 def_sphere/1, def_sphere/3]).

-define(MOVE_RATE, 2).
-define(ANGLE_RATE, 4).

-define(CONE_SIDES, 32).
-define(CYLINDER_SIDES, 32).
-define(DISC_SIDES, 32).
-define(SPHERE_SIDES, 16).


bindw(Event, Fun) ->
    tk:bind("Camera", Event, ['%W'], Fun).

init() ->
    bindw("<Enter>", fun(W) -> tk:focus([W]) end),

    bindw("<Leave>", fun(W) -> tk:focus([]) end),

    %% Forward
    bindw("<Up>",
	  fun(W) -> tk:cmd(W, [move, 0, 0, ?MOVE_RATE]) end),

    %% Backward
    bindw("<Down>", 
	  fun(W) -> tk:cmd(W, [move, 0, 0, -?MOVE_RATE]) end),

    %% Fast Forward
    bindw("<Shift-Up>",
	  fun(W) -> tk:cmd(W, [move, 0, 0, 4*?MOVE_RATE]) end),

    %% Fast Backward
    bindw("<Shift-Down>", 
	  fun(W) -> tk:cmd(W, [move, 0, 0, -4*?MOVE_RATE]) end),

    %% Strafe Left
    bindw("<Alt-Left>",
	  fun(W) -> tk:cmd(W, [move, -?MOVE_RATE, 0, 0])  end),

    %% Strafe Right
    bindw("<Alt-Right>",
	  fun(W) -> tk:cmd(W, [move, ?MOVE_RATE, 0, 0]) end),

    %% Turn Left
    bindw("<Left>",
	  fun(W) -> tk:cmd(W, [pan, ?ANGLE_RATE]) end),

    %% Turn Right
    bindw("<Right>",
	  fun(W) -> tk:cmd(W, [pan, -?ANGLE_RATE]) end),

    %% Turn Up
    bindw("<Prior>",
	  fun(W) -> tk:cmd(W, [tilt, -?ANGLE_RATE]) end),

    %% Turn Down
    bindw("<Next>",
	  fun(W) -> tk:cmd(W, [tilt, ?ANGLE_RATE]) end),

    %% Spin Left
    bindw("<Shift-Left>",
	  fun(W) -> tk:cmd(W, [revolve, -?ANGLE_RATE]) end),

    %% Spin Left
    bindw("<Shift-Right>",
	  fun(W) -> tk:cmd(W, [revolve, ?ANGLE_RATE]) end).


vertex(W, X, Y, Z) ->
    tk:cmd(W, [create, vertex, X, Y, Z]).


polygon(W, Vs) ->
    tk:cmd(W, [create, polygon] ++ Vs).

triangle(W, V1, V2, V3) ->
    tk:cmd(W, [create, polygon, V1, V2, V3]).

quad(W, V1, V2, V3, V4) ->
    tk:cmd(W, [create, polygon, V1, V2, V3, V4]).


box(Parent) -> box(Parent, 2, 2, 2).

box(Parent, Width, Height, Depth) ->
    Model = etk:model(Parent, []),
    def_box(Model, Width, Height, Depth),
    Model.

cone(Parent) -> cone(Parent, 1, 2, true, true).

cone(Parent, Radius, Height, NeedSide, NeedBottom) ->
    Model = etk:model(Parent, []),
    def_cone(Model, Radius, ?CONE_SIDES, Height, NeedSide, NeedBottom),
    Model.

cylinder(Parent) -> cylinder(Parent, 1, 2, true, true, true).

cylinder(Parent, Radius, Height, NeedTop, NeedSide, NeedBottom) ->
    Model = etk:model(Parent, []),
    def_cylinder(Model, Radius, ?CYLINDER_SIDES, Height, 
		 NeedTop, NeedSide, NeedBottom),
    Model.

sphere(Parent) -> sphere(Parent, 1).

sphere(Parent, Radius) ->
    Model = etk:model(Parent, []),
    def_sphere(Model, Radius, ?SPHERE_SIDES),
    Model.

%%
%% Create a sprite
%%
sprite(Parent, Texture) ->
    Model = etk:model(Parent, [{alignment, xyz}]),
    V1 = tk:cmd(Model, [create, vertex, -1, -1, 0, {uv, "0 0"}]),
    V2 = tk:cmd(Model, [create, vertex,  1, -1, 0, {uv, "1 0"}]),
    V3 = tk:cmd(Model, [create, vertex,  1,  1, 0, {uv, "1 1"}]),
    V4 = tk:cmd(Model, [create, vertex, -1,  1, 0, {uv, "0 1"}]),
    tk:cmd(Model, [create, polygon, V1, V2, V3, V4,
		   {lightsampling, "facet"},
		   {texturemodes, ""},
		   {texture, Texture}]),
    Model.
    
%%
%% Box
%%
%%  Default: Width = 2, Height = 2, Depth = 2
%%
def_box(W) -> def_box(W, 2, 2, 2).

def_box(W, Width, Height, Depth) ->
    W2 = Width / 2,
    H2 = Height / 2,
    D2 = Depth / 2,
    V0 = vertex(W, -W2, -H2, D2),
    V1 = vertex(W, -W2, H2, D2),
    V2 = vertex(W, -W2, H2, -D2),
    V3 = vertex(W, -W2, -H2, -D2),
    V4 = vertex(W, W2, -H2, D2),
    V5 = vertex(W, W2, H2, D2),
    V6 = vertex(W, W2, H2, -D2),
    V7 = vertex(W, W2, -H2, -D2),
    quad(W, V0, V1, V2, V3),
    quad(W, V7, V6, V5, V4),
    quad(W, V4, V5, V1, V0),
    quad(W, V3, V2, V6, V7),
    quad(W, V1, V5, V6, V2),
    quad(W, V1, V5, V6, V2),
    quad(W, V3, V7, V4, V0).

%%
%% Cone
%%
%% Default: Radius = 1, Height = 2
%%          NeedSide = true, NeedBottm = true
%%
def_cone(W) -> def_cone(W, 1, ?CONE_SIDES, 2, true, true).

def_cone(W, Radius, Sides, Height, NeedSide, NeedBottom) ->
    H2 = Height/2,
    XZ = circle_coords(Sides, Radius),
    Vs = circle_vertices(W, XZ, -H2),
    Top = vertex(W, 0, H2, 0),
    if NeedBottom == true ->
	    polygon(W, Vs);
       true -> false
    end,
    if NeedSide == true ->
	    add_cone_side(W, Vs, hd(Vs), Top);
       true -> false
    end.

add_cone_side(W, [Last], First, Top) ->
    triangle(W, First, Last, Top);
add_cone_side(W, [V1,V2|Vs], First, Top) ->
    triangle(W, V2, V1, Top),
    add_cone_side(W, [V2|Vs], First, Top).

%%
%% Cylinder
%%
%% Default: Radius = 1, Height = 2,
%%          NeedTop = true, NeedSide = true, NeedBottom = true
%%
def_cylinder(W) -> def_cylinder(W, 1, ?CYLINDER_SIDES, 2, true, true, true).

def_cylinder(W, Radius, Sides, Height, NeedTop, NeedSide, NeedBottom) ->
    H2 = Height/2,
    XZ = circle_coords(Sides, Radius),
    VsTop = circle_vertices(W, XZ, H2),
    VsBot = circle_vertices(W, XZ, -H2),
    if NeedTop == true -> polygon(W, reverse(VsTop));
       true -> false
    end,
    if NeedBottom == true -> polygon(W, VsBot);
       true -> false
    end,
    if NeedSide == true -> 
	    add_cylinder_side(W, VsTop, hd(VsTop), VsBot, hd(VsBot));
       true -> false
    end.

add_cylinder_side(W, [TLast], TFirst, [BLast], BFirst) ->
    quad(W, TLast, TFirst, BFirst, BLast);
add_cylinder_side(W, [VT1,VT2|VT], TFirst, [VB1,VB2|VB], BFirst) ->
    quad(W, VT1, VT2, VB2, VB1),
    add_cylinder_side(W, [VT2|VT], TFirst, [VB2|VB], BFirst).

%%
%% Sphere:
%%
def_sphere(W) ->
    def_sphere(W, 1, ?SPHERE_SIDES).

def_sphere(W, Radius, Sides) ->
    NorthPole = vertex(W, 0, Radius, 0),
    SouthPole = vertex(W, 0, -Radius, 0),
    Pi = math:pi(),
    Delta = Pi / (Sides-2),
    Angle = -(Pi / 2) + Delta,
    VLL = sphere_vertices(W, Sides-2, Radius, Sides, Angle, Delta, []),
    polygon(W, hd(VLL)),
    add_sphere_side(W, VLL).

add_sphere_side(W, [VL]) ->
    polygon(W, VL);
add_sphere_side(W, [VL1, VL2 | VLL]) ->
    add_sphere_stripe(W, VL1, hd(VL1), VL2, hd(VL2)),
    add_sphere_side(W, [VL2|VLL]).

add_sphere_stripe(W, [V1], V0, [W1], W0) ->
    quad(W, V1, V0, W0, W1);
add_sphere_stripe(W, [V1,V2|VL], V0, [W1,W2|WL], W0) ->
    quad(W, V1, V2, W2, W1),
    add_sphere_stripe(W, [V2|VL], V0, [W2|WL], W0).


sphere_vertices(W, 0, _, _, _, _, L) -> L;
sphere_vertices(W, I, Radius, Sides, Phi, Delta, L) ->
    R = Radius*math:cos(Phi),
    Y = Radius*math:sin(Phi),
    XZ = circle_coords(Sides, R),
    YV = circle_vertices(W, XZ, Y),
    sphere_vertices(W, I-1, Radius, Sides, Phi+Delta, Delta, [YV|L]).

%%
%% Util
%%
%%
circle_coords(N, R) ->
    circle_coords(N, 0, 2*math:pi() / N, R, []).

circle_coords(0, _, _, _, L) ->  L;
circle_coords(I, Phi, Delta, R, L) ->
    X = R*math:cos(Phi),
    Y = R*math:sin(Phi),
    circle_coords(I-1, Phi+Delta, Delta, R, [{X,Y} | L]).

circle_vertices(W, XZ, Y) ->
    circle_vertices(W, XZ, Y, []).
    

circle_vertices(W, [{X,Z} | XZ], Y, L) ->
    circle_vertices(W, XZ, Y, [vertex(W,X,Y,Z) | L]);
circle_vertices(_, [], _, L) -> L.


