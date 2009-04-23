%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%

%% 
%% @doc egd_primitives 
%%


-module(egd_primitives).
-export([
	create/2,
	color/1,
	pixel/3,
	polygon/3,
	line/4,
	arc/4,
	arc/5,
	rectangle/4,
	filledRectangle/4,
	filledEllipse/4,
	filledTriangle/5,
	text/5
	]).

-export([
	info/1,
	object_info/1,
	rgb_float2byte/1
	]).
-export([
	arc_to_edges/3,
	convex_hull/1,
	edges/1
	]).

-include("egd.hrl").


%% API info
info(I) ->
    W = I#image.width, H = I#image.height,
    io:format("Dimensions: ~p x ~p~n", [W,H]),
    io:format("Number of image objects: ~p~n", [length(I#image.objects)]),
    TotalPoints = info_objects(I#image.objects,0),
    io:format("Total points: ~p [~p %]~n", [TotalPoints, 100*TotalPoints/(W*H)]),
    ok.

info_objects([],N) -> N;
info_objects([O | Os],N) ->
    Points = length(O#image_object.points),
info_objects(Os,N+Points).

object_info(O) ->
    io:format("Object information: ~p~n", [O#image_object.type]),
    io:format("- Number of points: ~p~n", [length(O#image_object.points)]),
    io:format("-     Bounding box: ~p~n", [O#image_object.span]),
    io:format("-            Color: ~p~n", [O#image_object.color]),
    ok.

%% interface functions

line(I, Sp, Ep, Color) ->
    I#image{objects = [
	#image_object{ 
	type   = line, 
	points = [Sp, Ep],
	span   = span([Sp, Ep]),
	color  = Color} | I#image.objects]}.
	
arc(I, {Sx,Sy} = Sp, {Ex,Ey} = Ep, Color) ->
    X = Ex - Sx,
    Y = Ey - Sy,
    R = math:sqrt(X*X + Y*Y)/2,
    arc(I, Sp, Ep, R, Color).
	
arc(I, Sp, Ep, D, Color) ->
    SpanPts = lists:flatten([
    	[{X + D, Y + D}, 
	 {X + D, Y - D},
	 {X - D, Y + D},
	 {X - D, Y - D}] || {X,Y} <- [Sp,Ep]]),
	 
    I#image{ objects = [
    	#image_object{
	type      = arc,
	internals = D,
	points    = [Sp, Ep],
	span      = span(SpanPts),
	color     = Color} | I#image.objects]}.
	
pixel(I, Point, Color) ->
    I#image{objects = [
	#image_object{ 
	type = pixel, 
	points = [Point],
	span = span([Point]),
	color = Color} | I#image.objects]}.

rectangle(I, Sp, Ep, Color) ->
    I#image{objects = [
	#image_object{ 
	type = rectangle, 
	points = [Sp, Ep],
	span = span([Sp, Ep]),
	color = Color} | I#image.objects]}.

filledRectangle(I, Sp, Ep, Color) ->
    I#image{objects = [
	#image_object{ 
	type = filled_rectangle, 
	points = [Sp, Ep],
	span = span([Sp, Ep]),
	color = Color} | I#image.objects]}.

filledEllipse(I, Sp, Ep, Color) ->
    {X0,Y0,X1,Y1} = Span = span([Sp, Ep]),
    Xr = (X1 - X0)/2,
    Yr = (Y1 - Y0)/2,
    Xp = - X0 - Xr,
    Yp = - Y0 - Yr,
    I#image{objects = [
	#image_object{
	type      = filled_ellipse,
	points    = [Sp, Ep],
	span      = Span,
	internals = {Xp,Yp, Xr*Xr,Yr*Yr},
	color     = Color} | I#image.objects]}.

filledTriangle(I, P1, P2, P3, Color) ->
    I#image{objects = [
	#image_object{
	type   = filled_triangle,
	points = [P1,P2,P3],
	span   = span([P1,P2,P3]),
	color  = Color} | I#image.objects]}.


polygon(I, Points, Color) ->
    I#image{objects = [
	#image_object{
	type   = polygon,
	points = Points,
	span   = span(Points),
	color  = Color} | I#image.objects]}.

create(W, H) ->
    #image{ width = W, height = H}.

color({R,G,B}) -> rgba_byte2float({R,G,B, 255});
color(C)       -> rgba_byte2float(C).

text(I, {Xs,Ys} = Sp, Font, Text, Color) ->
    {FW,FH} = egd_font:size(Font),
    Length = length(Text),
    Ep = {Xs + Length*FW, Ys + FH + 5},
    I#image{objects = [
    	#image_object{
	type      = text_horizontal,
	points    = [Sp],
	span      = span([Sp,Ep]),
	internals = {Font, Text},
	color     = Color} | I#image.objects]}.


%%% Generic transformations

%% arc_to_edges
%% In:
%%	P1  :: point(),
%%	P2  :: point(),
%%	D   :: float(),
%% Out:
%%	Res :: [edges()]

arc_to_edges(P0, P1, D) when abs(D) < 0.5 -> [{P0,P1}];
arc_to_edges({X0,Y0}, {X1,Y1}, D) ->
    Vx = X1 - X0,
    Vy = Y1 - Y0,

    Mx = X0 + 0.5 * Vx,
    My = Y0 + 0.5 * Vy,
    
    % Scale V by Rs
    L  = math:sqrt(Vx*Vx + Vy*Vy),
    Sx = D*Vx/L, 
    Sy = D*Vy/L,
    
    Bx = trunc(Mx - Sy),
    By = trunc(My + Sx),

    arc_to_edges({X0,Y0}, {Bx,By}, D/4) ++ arc_to_edges({Bx,By}, {X1,Y1}, D/4).

%% edges
%% In:
%%	Pts :: [point()]
%% Out:
%%	Edges :: [{point(),point()}]

edges([]) -> [];
edges([P0|_] = Pts)  -> edges(Pts, P0,[]).
edges([P1], P0, Out) -> [{P1,P0}|Out];
edges([P1,P2|Pts],P0,Out) -> edges([P2|Pts],P0,[{P1,P2}|Out]).

%% convex_hull
%% In:
%%	Ps  :: [point()]
%% Out:
%%	Res :: [point()]

convex_hull(Ps) ->
    P0 = lower_right(Ps),
    [P1|Ps1] = lists:sort(fun
    	(P2,P1) ->
	    case point_side({P1,P0},P2) of
	    	left -> true;
		_    -> false
	    end
	end, Ps -- [P0]),
    convex_hull(Ps1, [P1,P0]).

convex_hull([], W) -> W;
convex_hull([P|Pts], [P1,P2|W]) ->
    case point_side({P2,P1},P) of
    	left -> convex_hull(Pts, [P,P1,P2|W]);
	_    -> convex_hull([P|Pts], [P2|W])
    end.
	
lower_right([P|Pts]) -> lower_right(P, Pts).
lower_right(P, []) -> P;
lower_right({X0,Y0}, [{_,Y}|Pts]) when Y < Y0 -> lower_right({X0,Y0}, Pts);
lower_right({X0,Y0}, [{X,Y}|Pts]) when X < X0, Y < Y0 -> lower_right({X0,Y0}, Pts);
lower_right(_,[P|Pts]) -> lower_right(P, Pts).

point_side({{X0,Y0}, {X1, Y1}}, {X2, Y2}) -> point_side((X1 - X0)*(Y2 - Y0) - (X2 - X0)*(Y1 - Y0)).
point_side(D) when D > 0 -> left;
point_side(D) when D < 0 -> right;
point_side(_) -> on_line.

%% AUX

span(Points) ->
    Xs = [TX||{TX, _} <- Points],
    Ys = [TY||{_, TY} <- Points],
    Xmin = lists:min(Xs),
    Xmax = lists:max(Xs),
    Ymin = lists:min(Ys),
    Ymax = lists:max(Ys),
    {Xmin,Ymin,Xmax,Ymax}.

rgb_float2byte({R,G,B}) -> rgb_float2byte({R,G,B,1.0});
rgb_float2byte({R,G,B,A}) -> 
    {trunc(R*255), trunc(G*255), trunc(B*255), trunc(A*255)}.

rgba_byte2float({R,G,B,A}) ->
    {R/255,G/255,B/255,A/255}.
