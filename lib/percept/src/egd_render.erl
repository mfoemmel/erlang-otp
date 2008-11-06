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
%% @doc egd_render 
%%

-module(egd_render).

-export([binary/1, binary/2]).
-compile(inline).
-compile(export_all).

-include("egd.hrl").
-define('DummyC',0).

binary(Image) -> binary(Image, opaque).

binary(Image, Type) ->
    parallel_binary(Image,Type).

parallel_binary(Image,Type) ->
    H = Image#image.height,
    Np = erlang:system_info(schedulers),
    N = lists:min([H,Np]),
    Hs = trunc(H/N),
    Pids = partition_binary(Image, Hs, {0,H}, Type),
    receive_binaries(Pids).

receive_binaries(Pids) -> receive_binaries(Pids, []).
receive_binaries([], Out) -> erlang:list_to_binary(Out); 
receive_binaries([{Pid,_}|Pids], Out) ->
    receive_binaries(Pids, [receive {Pid, B} -> B end | Out]).
	
partition_binary(Image,Hs,H,Type) -> partition_binary(Image,Hs,H,Type,[]).
partition_binary(_,_,{H,H},_,Pids) -> Pids;
partition_binary(Image,Hs,{Hi,Hm},Type, Pids) ->
    Yu = lists:min([Hi + Hs, Hm]), 
    Yl = Hi,
    Me = self(),
    Pid = spawn_link( fun() -> do_parallel_binary(Image, {Yl, Yu}, Type, Me) end),
    partition_binary(Image, Hs, {Yu,Hm}, Type, [{Pid, {Yu, Yl}}|Pids]).

do_parallel_binary(Image, {Hu, Hl}, Type, Pid) ->
    Bin = erlang:list_to_binary(
	scanlines(	{Hu,Hl}, 
			Image#image.objects, 
			{0, 0,Image#image.width - 1, Image#image.background}, 
			Type, 
			[])),
    Pid ! {self(), Bin}.

scanlines({Y,Y}, _, _,_, Scanlines) -> Scanlines;
scanlines({Yi,Y}, Os, {_,_,Width,_}=LSB, Type, Scanlines) ->
    OLSs = parse_objects_on_line(Y-1, Width, Os),
    URLSs = resulting_line_spans([LSB|OLSs],Type),

    % FIXME: Can we keep the list sorted instead of sorting it?
    % sort descending
    RLSs = lists:reverse(URLSs),

    Scanline = resulting_scanline(RLSs,Width),

    scanlines({Yi, Y - 1}, Os, LSB, Type, [Scanline|Scanlines]).

resulting_scanline(RLSs, Width) -> resulting_scanline(RLSs, Width, []).
resulting_scanline([], _, Scanlines) -> Scanlines;
resulting_scanline([{_,Xl, Xr, C} | RLSs], Width, Scanlines) ->
    {R,G,B,_} = rgb_float2byte(C),
    %% FIXME: optimize
    Scanline = lists:duplicate(trunc(Xr - Xl + 1), <<R:8,G:8,B:8>>),
    resulting_scanline(RLSs, Width, [Scanline|Scanlines]).

resulting_line_spans(LSs,Type) ->
    %% Build a list of "transitions" from left to right.
    Trans = line_spans_to_trans(LSs),
    %% Convert list of "transitions" to linespans.
	trans_to_line_spans(Trans,Type).

line_spans_to_trans(LSs) ->
    Trans = [],
    line_spans_to_trans(LSs,Trans,0).

line_spans_to_trans([],Db,_) ->
    lists:sort(Db);
line_spans_to_trans([{_,L,R,C}|LSs],Db,Z) ->
    line_spans_to_trans(LSs,[{{L,Z,start},C},{{R+1,Z,stop},C}|Db],Z+1).

trans_to_line_spans(Trans,Type) ->
    trans_to_line_spans(simplify_trans(Trans,Type,[],{0,0,0,0},[])).

trans_to_line_spans(SimpleTrans) ->
    trans_to_line_spans1(SimpleTrans,[]).

trans_to_line_spans1([],Spans) ->
    Spans;
trans_to_line_spans1([_],Spans) ->
    Spans;
trans_to_line_spans1([{L1,_},{L2,C2}|SimpleTrans],Spans) ->
    %% We are going backwards now...
    trans_to_line_spans1([{L2,C2}|SimpleTrans],[{?DummyC,L2,L1-1,C2}|Spans]).

simplify_trans([],_,_,_,Acc) ->
    Acc;
simplify_trans([{{L,_,_},_}|_] = Trans,Type,Layers,OldC,Acc) ->
    {NextTrans,RestTrans} =
	lists:splitwith(fun({{L1,_,_},_}) when L1 == L ->
				true;
			   (_) ->
				false
			end, Trans),
    {C,NewLayers} = color(NextTrans,Layers,Type,OldC),
    case OldC of
        C -> %% No change in color, so transition unnecessary.
            simplify_trans(RestTrans,Type,NewLayers,OldC,Acc);
        _ ->
            simplify_trans(RestTrans,Type,NewLayers,C,[{L,C}|Acc])
    end.

color(Trans,Layers,Type,OldC) ->
    case modify_layers(Layers,Trans) of
        Layers ->
            {OldC,Layers};
        NewLayers ->
            {color(NewLayers,Type),NewLayers}
    end.

color([],_) -> {0,0,0,0};
color([{_,C}|_],opaque) -> C;    
color(Layers,alpha) -> color1({0,0,0,0},Layers).

color1(Color,[]) -> Color;
color1(Color,[{_,C}|Layers]) -> color1(blend(Color,C),Layers).

blend(C1,C2) -> alpha_blend(C1,C2).

modify_layers(Layers,[]) -> Layers;
modify_layers(Layers,[{{_,Z,Op},C}|Trans]) ->
    modify_layers(case Op of
		      start ->
			  add_layer(Layers,Z,C);
		      stop ->
			  remove_layer(Layers,Z,C)
		  end,
		  Trans).

add_layer([{Z1,_}=H|Layers],Z,C) when Z1 > Z ->
    [H|add_layer(Layers,Z,C)];
add_layer(Layers,Z,C) ->
    [{Z,C}|Layers].

remove_layer(Layers,Z,C) ->
    Layers -- [{Z,C}].
    

alpha_blend({R1,G1,B1,A1}, {R2,G2,B2,A2}) ->
  Beta = A2*(1 - A1),
  A = A1 + Beta,
  R = R1*A1 + R2*Beta,
  G = G1*A1 + G2*Beta,
  B = B1*A1 + B2*Beta,
  {R,G,B,A}.

parse_objects_on_line(Y, Width, Objects) ->
    parse_objects_on_line(Y, 1, Width, Objects, []).
parse_objects_on_line(_Y, _Z, _, [], Out) -> lists:flatten(Out);
parse_objects_on_line(Y, Z, Width, [O|Os], Out) ->
    case is_object_on_line(Y, O) of
    	false ->
	    parse_objects_on_line(Y, Z + 1, Width, Os, Out);
	true ->
	    OLs = object_line_data(Y, Z, O),
	    TOLs = trim_object_line_data(OLs, Width),
	    parse_objects_on_line(Y, Z + 1, Width, Os, [TOLs|Out])
    end.

trim_object_line_data(OLs, Width) ->
    trim_object_line_data(OLs, Width, []).
trim_object_line_data([], _, Out) -> Out;
trim_object_line_data([{Z, Xl, Xr, C}|OLs], Width, Out) ->
    if 
	Xl > Width ->
            trim_object_line_data(OLs, Width, Out);
	Xr < 0 ->
            trim_object_line_data(OLs, Width, Out);
	true ->
           trim_object_line_data(OLs, Width, [{Z, lists:max([0,Xl]), lists:min([Xr,Width]), C}|Out])
    end.

% object_line_data
% In:
%	Y :: index of height
%	Z :: index of depth
%	Object :: image_object()
% Out:
%	OLs = [{Z, Xl, Xr, Color}]
%	Z = index of height
%	Xl = left X index
%	Xr = right X index 
% Purpose:
%	Calculate the length (start and finish index) of an objects horizontal
%	line given the height index.

object_line_data(Y, Z, Object) -> object_line_data(Y, Z, Object, Object#image_object.type).
object_line_data(Y, Z, O, rectangle) ->
    {X0, Y0, X1, Y1} = O#image_object.span,
    if
	Y0 == Y ; Y1 == Y ->
    	    [{Z, X0, X1, O#image_object.color}];
	true ->
    	    [{Z, X0, X0, O#image_object.color},
	     {Z, X1, X1, O#image_object.color}]
    end;
object_line_data(_Y, Z, O, filled_rectangle) ->
    {X0, _Y0, X1, _Y1} = O#image_object.span,
    [{Z, X0, X1, O#image_object.color}];
object_line_data(Y, Z, O, filled_ellipse) ->
    {X0,Y0,X1,Y1} = O#image_object.span,
    if 
    	X1 - X0 == 0 -> % if the width is exactly one pixel
	    [{Z, X1, X0, O#image_object.color}];
	X1 - X0 < 0 -> throw(bad_ellipse_width);
	Y1 - Y0 == 0 -> % Height exactly one pixel, get width
	    [{Z, X0, X1, O#image_object.color}];
	true ->
	    Xr = (X1 - X0)/2,
	    Yr = (Y1 - Y0)/2,
	    Yo = trunc(Y - Y0 - Yr),
	    Yo2 = Yo*Yo,
	    Yr2 = Yr*Yr,
	    Xo = math:sqrt((1 - Yo2/Yr2))*Xr,
	    [{Z, round(X0 - Xo + Xr), round(X0 + Xo + Xr), O#image_object.color}]
    end;
object_line_data(Y, Z, O, filled_triangle) ->
    Is = O#image_object.intervals,
    case lists:keysearch(Y, 1, Is) of
   	{value, {Y, Xl, Xr}} -> [{Z, Xl, Xr, O#image_object.color}];
	false -> []
    end;    

object_line_data(Y, Z, O, line) ->
    Is = O#image_object.intervals,
    case lists:keysearch(Y, 1, Is) of
   	{value, {Y, Xl, Xr}} -> [{Z, Xl, Xr, O#image_object.color}];
	false -> []
    end;    

object_line_data(Y, Z, O, polygon) ->
    Is = lists:filter(
	fun({Yp,_,_}) ->
	    if Yp == Y -> true; true -> false end
    	end, O#image_object.intervals),
    [	{Z, Xl, Xr, O#image_object.color} ||
	{_, Xl, Xr} <- Is];
object_line_data(Y, Z, O, polygon_points_only) ->
    Ps = lists:filter( 
    	fun({_,Yp}) ->
	if Yp == Y -> true; true -> false end
    	end, O#image_object.internals),
    Xs = [ X || {X,_} <- Ps],
    Xs_length = length(Xs),
    if 
    	Xs_length < 1 ->
    	    [];
	true ->
	    Xl = lists:min(Xs),
	    Xr = lists:max(Xs),
	    [{Z, Xl, Xr, O#image_object.color}]
    end;
object_line_data(Y, Z, O, text_horizontal) ->
    % FIXME: optimize!
    lists:foldl(
	fun ({Yg,Xl,Xr}, Out) ->
	    if 
		Yg == Y ->
		    [{Z, Xl, Xr, O#image_object.color}|Out];
		true ->
		    Out
	    end
	end, [], O#image_object.intervals);
object_line_data(_, Z, O, _Type) ->
    % faked
    {X0,_Y0,X1,_Y1} = O#image_object.span,
    [{Z, X0, X1, O#image_object.color}].

is_object_on_line(Y, Object) ->
    is_object_bounds_on_line(Y, Object#image_object.span). 
    
is_object_bounds_on_line(Y, {_,Y0,_,Y1}) ->
    if 
    	Y < Y0 -> false;
	Y > Y1 -> false;
	true -> true
    end.

rgb_float2byte({R,G,B,A}) ->
    {trunc(R*255), trunc(G*255), trunc(B*255), trunc(A*255)}.

