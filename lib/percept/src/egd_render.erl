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
%% @doc egd_render 
%%

-module(egd_render).

-export([binary/1, binary/2]).
-compile(inline).

-include("egd.hrl").
-define('DummyC',0).

binary(Image) -> binary(Image, opaque).

binary(Image, Type) ->
    parallel_binary(Image,Type).

parallel_binary(Image = #image{ height = Height },Type) ->
    case lists:min([erlang:system_info(schedulers), Height]) of
        1 ->
	    % if the height or the number of schedulers is 1
	    % do the scanlines in this process.
	    W  = Image#image.width,
	    Bg = Image#image.background,
	    Os = Image#image.objects,
	    erlang:list_to_binary(lists:map(fun
	    	(Y) -> scanline(Y, Os, {0,0,W - 1, Bg}, Type)
	    end, lists:seq(1, Height)));
	Np ->
	    Pids    = start_workers(Np, Type),
    	    Handler = handle_workers(Height, Pids),
    	    init_workers(Image, Handler, Pids),
	    Res = receive_binaries(Height),
    	    finish_workers(Pids),
	    Res
    end.

start_workers(Np, Type) -> start_workers(Np, Type, []).
start_workers( 0,    _, Pids) -> Pids;
start_workers(Np, Type, Pids) when Np > 0 -> 
    start_workers(Np - 1, Type, [spawn_link(fun() -> worker(Type) end)|Pids]).

worker(Type) ->
    receive
	{Pid, data, #image{ objects = Os, width = W, background = Bg }} -> worker(Os, W, Bg, Type, Pid)
    end.

worker(Objects, Width, Bg, Type, Collector) ->
    receive
    	{Pid, scan, {Ys, Ye}} ->
	    lists:foreach(fun
		(Y) ->
		    Bin = erlang:list_to_binary(scanline(Y, Objects, {0,0,Width - 1, Bg}, Type)),
		    Collector ! {scan, Y, Bin}
		end, lists:seq(Ys,Ye)),
	    Pid ! {self(), scan_complete},
	    worker(Objects, Width, Bg, Type, Collector);
    	{Pid, scan, Y} ->
	    Bin = erlang:list_to_binary(scanline(Y, Objects, {0,0,Width - 1, Bg}, Type)),
	    Collector ! {scan, Y, Bin},
	    Pid ! {self(), scan_complete},
	    worker(Objects, Width, Bg, Type, Collector);
	{_, done} ->
	 ok
    end.

init_workers(_Image, _Handler, []) -> ok;
init_workers(Image, Handler, [Pid|Pids]) ->
    Pid ! {self(), data, Image}, 
    Handler ! {Pid, scan_complete},
    init_workers(Image, Handler, Pids).

handle_workers(H, Pids) -> spawn_link(fun() -> handle_workers(H, H, length(Pids)) end).
handle_workers(_, 0, _) -> ok;
handle_workers(H, Hi, Np) when H > 0 ->
    N = trunc(Hi/(2*Np)),
    receive 
	{Pid, scan_complete} -> 
	    if N < 2 ->
	    	Pid ! {self(), scan, Hi},
		handle_workers(H, Hi - 1, Np);
	    true ->
	    	Pid ! {self(), scan, {Hi - N, Hi}},
	   	handle_workers(H, Hi - 1 - N, Np)
	end
    end.

finish_workers([]) -> ok;
finish_workers([Pid|Pids]) ->
    Pid ! {self(), done},
    finish_workers(Pids).

receive_binaries(H) -> receive_binaries(H, []).
receive_binaries(0, Bins) -> erlang:list_to_binary(Bins);
receive_binaries(H, Bins) when H > 0 ->
    receive
        {scan, H, Bin} -> 
	    receive_binaries(H - 1, [Bin|Bins])
    end.


scanline(Y, Os, {_,_,Width,_}=LSB, Type) ->
    OLSs  = parse_objects_on_line(Y-1, Width, Os),
    URLSs = resulting_line_spans([LSB|OLSs],Type),

    % FIXME: Can we keep the list sorted instead of sorting it?
    % sort descending
    RLSs = lists:reverse(URLSs),

    resulting_scanline(RLSs,Width).

resulting_scanline(RLSs, Width) -> resulting_scanline(RLSs, Width, []).
resulting_scanline([], _, Scanlines) -> Scanlines;
resulting_scanline([{_,Xl, Xr, C} | RLSs], Width, Scanlines) ->
    {R,G,B,_} = rgb_float2byte(C),
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
    trans_to_line_spans(simplify_trans(Trans,Type,[],{0.0,0.0,0.0,0.0},[])).

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

color([],_) -> {0.0,0.0,0.0,0.0};
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
  Beta = A2*(1.0 - A1),
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
object_line_data(Y, Z, #image_object{ span = {X0, Y0, X1, Y1}, color = C}, rectangle) ->
    if
	Y0 =:= Y ; Y1 =:= Y ->
    	    [{Z, X0, X1, C}];
	true ->
    	    [{Z, X0, X0, C},
	     {Z, X1, X1, C}]
    end;

object_line_data(_Y, Z, #image_object{ span = {X0, _, X1, _}, color = C}, filled_rectangle) ->
    [{Z, X0, X1, C}];

object_line_data(Y, Z, #image_object{ span = {X0,Y0,X1,Y1}, color = C}, filled_ellipse) ->
    if 
    	X1 - X0 == 0 -> % if the width is exactly one pixel
	    [{Z, X1, X0, C}];
	X1 - X0 < 0 -> throw(bad_ellipse_width);
	Y1 - Y0 == 0 -> % Height exactly one pixel, get width
	    [{Z, X0, X1, C}];
	true ->
	    Xr = (X1 - X0)/2,
	    Yr = (Y1 - Y0)/2,
	    Yo = trunc(Y - Y0 - Yr),
	    Yo2 = Yo*Yo,
	    Yr2 = Yr*Yr,
	    Xo = math:sqrt((1 - Yo2/Yr2))*Xr,
	    [{Z, round(X0 - Xo + Xr), round(X0 + Xo + Xr), C}]
    end;

object_line_data(Y, Z, #image_object{ intervals = Is, color = C}, filled_triangle) ->
    case lists:keysearch(Y, 1, Is) of
   	{value, {Y, Xl, Xr}} -> [{Z, Xl, Xr, C}];
	false -> []
    end;    

object_line_data(Y, Z, #image_object{ intervals = Is, color = C}, line) ->
    case dict:find(Y, Is) of
	{ok, {Xl, Xr}} -> [{Z, Xl, Xr, C}];
	_ -> []
    end;

object_line_data(Y, Z, O, polygon) ->
    Is = lists:filter(
	fun({Yp,_,_}) ->
	    if Yp == Y -> true; true -> false end
    	end, O#image_object.intervals),
    [	{Z, Xl, Xr, O#image_object.color} ||
	{_, Xl, Xr} <- Is];

object_line_data(Y, Z, #image_object{ color = C, internals = Internals }, polygon_points_only) ->
    Ps = lists:filter(fun
	    ({_,Yp}) ->
		if 
		    Yp =:= Y -> true; 
		    true -> false 
		end
    	    end, Internals),
    Xs = [ X || {X,_} <- Ps],
    Xs_length = length(Xs),
    if 
    	Xs_length < 1 ->
    	    [];
	true ->
	    Xl = lists:min(Xs),
	    Xr = lists:max(Xs),
	    [{Z, Xl, Xr, C}]
    end;

object_line_data(Y, Z, #image_object{ color = C, intervals = Is }, text_horizontal) ->
    % FIXME: optimize!
    lists:foldl(
	fun ({Yg,Xl,Xr}, Out) ->
	    if 
		Yg == Y ->
		    [{Z, Xl, Xr, C}|Out];
		true ->
		    Out
	    end
	end, [], Is);
object_line_data(_, Z, #image_object{ span = {X0,_,X1,_}, color = C}, _) ->
    % faked
    [{Z, X0, X1, C}].

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

