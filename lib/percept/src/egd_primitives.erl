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
%% @doc egd_primitives 
%%


-module(egd_primitives).
-export([
	info/1,
	object_info/1,
	create/2,
	color/1,
	pixel/3,
	polygon/3,
	line/4,
	rectangle/4,
	filledRectangle/4,
	filledEllipse/4,
	filledTriangle/5,
	text/5,
	rgb_float2byte/1
	]).
-export([
	polygon_tri/1,
	polygon_ls/1,
	line_ls/2,
	line_points/2,
	triangle_ls/3
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

line(I, StartPoint, EndPoint, Color) ->
    I#image{objects = [
	#image_object{ 
	type = line, 
	%points = line_points(StartPoint,EndPoint),
	intervals = line_ls(StartPoint,EndPoint),
	span = span([StartPoint, EndPoint]),
	color = Color} | I#image.objects]}.
	
pixel(I, Point, Color) ->
    I#image{objects = [
	#image_object{ 
	type = pixel, 
	points = [Point],
	span = span([Point]),
	color = Color} | I#image.objects]}.

rectangle(I, StartPoint, EndPoint, Color) ->
    I#image{objects = [
	#image_object{ 
	type = rectangle, 
	points = [StartPoint, EndPoint],
	span = span([StartPoint, EndPoint]),
	color = Color} | I#image.objects]}.

filledRectangle(I, StartPoint, EndPoint, Color) ->
    I#image{objects = [
	#image_object{ 
	type = filled_rectangle, 
	points = [StartPoint, EndPoint],
	span = span([StartPoint, EndPoint]),
	color = Color} | I#image.objects]}.

filledEllipse(I, StartPoint, EndPoint, Color) ->
    {X0,Y0,X1,Y1} = Span = span([StartPoint, EndPoint]),
    Xr = (X1 - X0)/2,
    Yr = (Y1 - Y0)/2,
    Xp = - X0 - Xr,
    Yp = - Y0 - Yr,
    I#image{objects = [
	#image_object{
	type = filled_ellipse,
	points = [StartPoint, EndPoint],
	span = Span,
	internals = {Xp,Yp, Xr*Xr,Yr*Yr},
	color = Color} | I#image.objects]}.

filledTriangle(I, P1, P2, P3, Color) ->
    I#image{objects = [
	#image_object{
	type = filled_triangle,
	intervals = triangle_ls(P1,P2,P3),
	points = [P1,P2,P3],
	span = span([P1,P2,P3]),
	color = Color} | I#image.objects]}.


polygon(I, Points, Color) ->
    PLSs = polygon_ls(Points),
    io:format("PLSs: ~p ~n", [length(PLSs)]),
    I#image{objects = [
	#image_object{
	type = polygon,
	points = Points,
	intervals = PLSs,
	span = span(Points),
	color = Color} | I#image.objects]}.

create(W, H) ->
    #image{ width = W, height = H}.

color({R,G,B}) ->
    rgb_byte2float({R,G,B, 255});
color(C) ->
    rgb_byte2float(C).

text(I, {Xs,Ys} = StartPoint, FontName, Text, Color) ->
    {FW,FH} = egd_font:size(FontName),
    Length = length(Text),
    EndPoint = {Xs + Length*FW, Ys + FH + 5},
    I#image{objects = [
    	#image_object{
	type=text_horizontal,
	points = [StartPoint],
	span = span([StartPoint,EndPoint]),
	internals = [],
	intervals = text_intervals(StartPoint, FontName, Text), 
	color = Color} | I#image.objects]}.

%%%%%%%%%%%


% triangle 

triangle_ls(P1,P2,P3) ->
    io:format("triangle: ~p ~p ~p ~n", [P1,P2,P3]),
    % Find top point (or left most top point),
    % From that point, two lines will be drawn to the 
    % other points.
    % For each Y step, 
  	% bresenham_line_interval for each of the two lines
	% Find the left most and the right most for those lines
    % At an end point, a new line to the point already being drawn
    % repeat same procedure as above
    [Sp1, Sp2, Sp3] = tri_pt_ysort([P1,P2,P3]),   
    triangle_ls_lp(tri_ls_ysort(line_ls(Sp1,Sp2)), Sp2,tri_ls_ysort(line_ls(Sp1,Sp3)), Sp3, []).

% There will be Y mismatches between the two lists since bresenham is not perfect.
% I can be remedied with checking intervals this could however be costly and
% it may not be necessary, depending on how exact we need the points to be.
% It should at most differ by one and endpoints should be fine.

triangle_ls_lp([],_,[],_,Out) -> Out;
triangle_ls_lp(LSs1, P1, [], P2, Out) -> 
    SLSs = tri_ls_ysort(line_ls(P2,P1)),
    N2 = length(SLSs),
    N1 = length(LSs1),
    io:format("New: LSs1 = ~p, SLSs = ~p~n", [N1,N2]),
    if 
	N1 > N2 ->
	    [_|ILSs] = LSs1,
    	    triangle_ls_lp(ILSs, SLSs, Out);
	N2 > N1 ->
	    [_|ILSs] = SLSs,
    	    triangle_ls_lp(LSs1, ILSs, Out);
	true ->
    	    triangle_ls_lp(LSs1, SLSs, Out)
    end;
triangle_ls_lp([], P1, LSs2, P2, Out) ->
    SLSs = tri_ls_ysort(line_ls(P1,P2)),
    N1 = length(SLSs),
    N2 = length(LSs2),
    if 
	N1 > N2 ->
	    [_|ILSs] = SLSs,
    	    triangle_ls_lp(ILSs, LSs2, Out);
	N2 > N1 ->
	    [_|ILSs] = LSs2,
    	    triangle_ls_lp(SLSs, ILSs, Out);
	true ->
	    triangle_ls_lp(SLSs, LSs2, Out)
    end;
triangle_ls_lp([LS1|LSs1],P1,[LS2|LSs2],P2, Out) ->
    {Y, Xl1, Xr1} = LS1,
    {_, Xl2, Xr2} = LS2,
    Xr = lists:max([Xl1,Xr1,Xl2,Xr2]),
    Xl = lists:min([Xl1,Xr1,Xl2,Xr2]),
    triangle_ls_lp(LSs1,P1, LSs2, P2, [{Y,Xl,Xr}|Out]).    

triangle_ls_lp([],[],Out) -> Out;
triangle_ls_lp([],_,Out) -> Out;
triangle_ls_lp(_,[],Out) -> Out;
triangle_ls_lp([LS1|LSs1], [LS2|LSs2], Out) ->
    {Y, Xl1, Xr1} = LS1,
    {_, Xl2, Xr2} = LS2,
    Xr = lists:max([Xl1,Xr1,Xl2,Xr2]),
    Xl = lists:min([Xl1,Xr1,Xl2,Xr2]),
    triangle_ls_lp(LSs1, LSs2, [{Y,Xl,Xr}|Out]).    
       
tri_pt_ysort(Pts) ->
    % {X,Y}
    lists:sort(
	fun ({_,Y1},{_,Y2}) ->
	   if Y1 > Y2 -> false; true -> true end
	end, Pts).

tri_ls_ysort(LSs) ->
    % {Y, Xl, Xr}
    lists:sort(
	fun ({Y1,_,_},{Y2,_,_}) ->
	   if Y1 > Y2 -> false; true -> true end
	end, LSs).

% polygon_ls
% In:
%	Pts :: [{X,Y}]
% Out:
% 	LSs :: [{Y,Xl,Xr}]
% Purpose:
%	Make polygon line spans
% Algorithm:
%	1. Find the left most (lm) point
%	2. Find the two points adjacent to that point
%		The tripplet will make a triangle
%	3. Ensure no points lies within the triangle
%	4a.No points within triangle, 
%		make triangle,
%	   	remove lm point
%		1.
%	4b.point(s) within triangle,
%				


polygon_ls(Pts) ->
    % Make triangles
    Tris = polygon_tri(Pts),
    % interval triangles
    lists:flatten(polygon_tri_ls(Tris, [])).

polygon_tri_ls([], Out) -> Out;
polygon_tri_ls([{P1,P2,P3}|Tris], Out) ->
    polygon_tri_ls(Tris, [triangle_ls(P1,P2,P3)|Out]).

polygon_tri(Pts) ->
    polygon_tri(polygon_lm_pt(Pts), []).


polygon_tri([P1,P2,P3],Tris) -> [{P1,P2,P3}|Tris];
polygon_tri([P2,P1,P3|Pts], Tris) ->
    case polygon_tri_test(P1,P2,P3,Pts) of
	false -> polygon_tri(polygon_lm_pt([P2,P3|Pts]), [{P1,P2,P3}|Tris]);
	[LmPt|Ptsn] -> polygon_tri([P2,P1,LmPt,P3|Ptsn], Tris)
    end.

polygon_tri_test(P1,P2,P3, Pts) ->
    polygon_tri_test(P1,P2,P3, Pts, []).
    
polygon_tri_test(_,_,_, [], _) -> false;
polygon_tri_test(P1,P2,P3,[Pt|Pts], Ptsr) ->
    case point_inside_triangle(Pt, P1,P2,P3) of
    	false -> polygon_tri_test(P1,P2,P3, Pts, [Pt|Ptsr]);
    	true -> [Pt|Pts] ++ lists:reverse(Ptsr) 
    end.

% polygon_lm_pt
% In:
%	Pts ::  [{X,Y}]
% Out
%	LmPts = [{X0,Y0},{Xmin,Y0},{X1,Y1},...]
% Purpose:
%	 The order of the list is important
%	 rotate the elements until Xmin is first
%	 This is not extremly fast.

polygon_lm_pt(Pts) ->
    Xs = [X||{X,_}<-Pts],
    polygon_lm_pt(Pts, lists:min(Xs), []).

polygon_lm_pt([Pt0,{X,_}=Ptm | Pts], Xmin, Ptsr) when X > Xmin ->
    polygon_lm_pt([Ptm|Pts], Xmin, [Pt0|Ptsr]);
polygon_lm_pt(Pts, _,  Ptsr) ->
    Pts ++ lists:reverse(Ptsr).


% return true if P is inside triangle (p1,p2,p3),
% otherwise false.

points_same_side({P1x,P1y}, {P2x,P2y}, {L1x,L1y}, {L2x,L2y}) ->
    ((P1x - L1x)*(L2y - L1y) - (L2x - L1x)*(P1y - L1y) *
     (P2x - L1x)*(L2y - L1y) - (L2x - L1x)*(P2y - L1y)) >= 0.

point_inside_triangle(P, P1, P2, P3) ->
    points_same_side(P, P1, P2, P3) and 
    points_same_side(P, P2, P1, P3) and 
    points_same_side(P, P3, P1, P2).
   
% Bresenham line
line_points({Xi0,Yi0}, {Xi1,Yi1}) ->
    % swap X with Y if line is steep
    Steep = abs(Yi1 - Yi0) > abs(Xi1 - Xi0),
    {Xs0, Ys0, Xs1, Ys1} = case Steep of
	true -> {Yi0,Xi0,Yi1,Xi1};
	false -> {Xi0,Yi0,Xi1,Yi1}
    end,

    {X0,Y0,X1,Y1} = case Xs0 > Xs1 of
	true -> {Xs1,Ys1,Xs0,Ys0};
	false -> {Xs0,Ys0,Xs1,Ys1}
    end,

    DX = X1 - X0,
    DY = abs(Y1 - Y0),

    Error = -DX/2,

    Ystep = case Y0 < Y1 of
	true -> 1;
	false -> -1
    end, 

    line_points_step({X0, X1},Y0, DX, DY, Ystep, Steep, Error, []).

line_points_step({X,X1},Y,DX,DY,Ystep,Steep,E,Out) when X =< X1 ->
    NewOut = case Steep of
	true -> [{Y,X}|Out];
	false -> [{X,Y}|Out]
    end,
    Error = E + DY,
    case E >= 0 of
	true ->
	    line_points_step({X+1,X1},Y+Ystep,DX,DY,Ystep,Steep,Error-DX,NewOut);
	false ->
	    line_points_step({X+1,X1},Y,DX,DY,Ystep,Steep,Error,NewOut)
    end;
line_points_step({_,_},_Y,_DX,_DY,_Ystep,_Steep,_E,Out) -> Out.


%% line_ls
%% In:
%%	P1 :: point()
%%	P2 :: point()
%% Out:
%%	{{Ymin,Ymax}, LSD :: line_step_data()}
%% Purpose:
%% 	Instead of points -> intervals


line_ls({Xi0, Yi0},{Xi1,Yi1}) ->
    % swap X with Y if line is steep
    Steep = abs(Yi1 - Yi0) > abs(Xi1 - Xi0),

    {Xs0, Ys0, Xs1, Ys1} = case Steep of
	true -> {Yi0,Xi0,Yi1,Xi1};
	false -> {Xi0,Yi0,Xi1,Yi1}
    end,

    {X0,Y0,X1,Y1} = case Xs0 > Xs1 of
	true -> {Xs1,Ys1,Xs0,Ys0};
	false -> {Xs0,Ys0,Xs1,Ys1}
    end,

    DX = X1 - X0,
    DY = abs(Y1 - Y0),

    Error = -DX/2,

    Ystep = case Y0 < Y1 of
	true -> 1;
	false -> -1
    end, 
    case Steep of
	false ->
	    line_ls_step_not_steep({X0, X1},Y0, DX, DY, Ystep, Error, X0, []);
	true -> 
	    line_ls_step_steep({X0, X1},Y0, DX, DY, Ystep, Error, X0, [])
    end.
	 

%% line_ls_step_(not)_steep
%% In:
%% Out:
%%	[{Yi, Xl,Xr}]
%% Purpose:
%% 	Produce an line_interval for each Yi (Y index)	

% Iterating the X-axis

line_ls_step_not_steep({X,X1},Y,Dx,Dy,Ys,E, X0, LSs) when X < X1 ->
    case E >= 0 of
	true ->
	    line_ls_step_not_steep({X+1,X1},Y+Ys,Dx,Dy,Ys, E - Dx + Dy, X+1,[{Y,X0,X}|LSs]);
	false ->
	    line_ls_step_not_steep({X+1,X1},Y,Dx,Dy,Ys, E + Dy, X0, LSs)
    end;
line_ls_step_not_steep({X,_},Y,_Dx,_Dy,_Ystep,_E,X0,LSs) ->
    [{Y,X0,X}|LSs].

% Iterating the Y-axis
line_ls_step_steep({X,X1},Y,Dx,Dy,Ystep,E, X0, LSs) when X =< X1 ->
    case E >= 0 of
	true ->
	    line_ls_step_steep({X + 1,X1},Y+Ystep,Dx,Dy,Ystep,E - Dx + Dy,X,[{X,Y,Y}|LSs]);
	false ->
	    line_ls_step_steep({X + 1,X1},Y,Dx,Dy,Ystep,E + Dy,X0, [{X,Y,Y}|LSs])
    end;
line_ls_step_steep({_X,_},_Y,_Dx,_Dy,_Ystep,_E,_X0,LSs) -> 
    LSs.


% span
span(Points) ->
    Xs = [TX||{TX, _} <- Points],
    Ys = [TY||{_, TY} <- Points],
    Xmin = lists:min(Xs),
    Xmax = lists:max(Xs),
    Ymin = lists:min(Ys),
    Ymax = lists:max(Ys),
    {Xmin,Ymin,Xmax,Ymax}.

% color conversions

rgb_float2byte({R,G,B,A}) ->
    {trunc(R*255), trunc(G*255), trunc(B*255), trunc(A*255)}.

rgb_byte2float({R,G,B,A}) ->
    {R/255,G/255,B/255,A/255}.

% Text

text_intervals(Point, Font, Chars) ->
    {_Fw,Fh} = egd_font:size(Font),
    text_intervals(Point, Fh, Font, Chars, []).
    
% This is stupid. The starting point is the top left (Ptl) but the font
% offsets is relative to the bottom right origin,
%  {Xtl,Ytl} -------------------------
%            |                       |
%            |    Glyph BoundingBox  |
%            |     --------          |
%            |     |Bitmap| Gh       |
%       FH   |-Gx0-|Data  |          |
%            |     --------          |
%            |        |              |
%            |       Gy0             |
%            |        |              |
% Glyph (0,0)------------------------- Gxm (Glyph X move)
%                     FW
% Therefore, we need Yo, which is Yo = FH - Gy0 - Gh,
% Font height minus Glyph Y offset minus Glyph bitmap data boundingbox
% height.

text_intervals( _, _, _, [], Out) -> lists:flatten(Out);
text_intervals({Xtl,Ytl}, Fh, Font, [Code|Chars], Out) ->
    {{_Gw, Gh, Gx0, Gy0, Gxm}, LSs} = egd_font:glyph(Font, Code),
    % Set offset points from translation matrix to point in TeInVe.
    Yo = Fh - Gh + Gy0,
    GLSs = text_intervals_vertical({Xtl+Gx0,Ytl+Yo},LSs, []),
    text_intervals({Xtl+Gxm,Ytl}, Fh, Font, Chars, [GLSs|Out]).

text_intervals_vertical( _, [], Out) -> Out;
text_intervals_vertical({Xtl, Ytl}, [LS|LSs], Out) -> 
    H = lists:foldl( 
	fun ({Xl,Xr}, RLSs) ->
	    [{Ytl, Xl + Xtl, Xr + Xtl}|RLSs]
	end, [], LS),
    text_intervals_vertical({Xtl, Ytl+1}, LSs, [H|Out]).
