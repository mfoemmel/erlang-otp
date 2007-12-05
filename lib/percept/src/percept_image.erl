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

-module(percept_image).
-export([	proc_lifetime/5,
		percentage/3,
		graph/3, 
		graph/4, 
		activities/3, 
		activities/4]).
-record(graph_area, {x = 0, y = 0, width, height}).

%%% -------------------------------------
%%% GRAF
%%% -------------------------------------

%% graph(Widht, Height, Range, Data)

graph(Width, Height, {RXmin, RYmin, RXmax, RYmax}, Data) ->
    Data2 = [{X, Y1 + Y2} || {X, Y1, Y2} <- Data],
    MinMax = percept_analyzer:minmax(Data2),
    {Xmin, Ymin, Xmax, Ymax} = MinMax, 
    graf1(Width, Height,{	lists:min([RXmin, Xmin]), 
    				lists:min([RYmin, Ymin]),
    				lists:max([RXmax, Xmax]), 
				lists:max([RYmax, Ymax])}, Data).

%% graph(Widht, Height, Data) = Image
%% In:
%%	Width = integer(),
%%	Height = integer(),
%%	Data = [{Time, Procs, Ports}]
%%	Time = float()
%%	Procs = integer()
%%	Ports = integer()
%% Out:
%%	Image = binary()

graph(Width, Height, Data) ->
    Data2 = [{X, Y1 + Y2} || {X, Y1, Y2} <- Data],
    Bounds = percept_analyzer:minmax(Data2),
    graf1(Width, Height, Bounds, Data).

graf1(Width, Height, {Xmin, Ymin, Xmax, Ymax}, Data) ->
    
    %% Calculate areas
    HO = 20,
    GrafArea = #graph_area{x = HO, y = 4, width = Width - 2*HO, height = Height - 17},
    XticksArea = #graph_area{x = HO, y = Height - 13, width = Width - 2*HO, height = 13},
    YticksArea = #graph_area{x = 1, y = 4, width = HO, height = Height - 17},
    
    %% Initiate Image

    Image = image:start(Width, Height),
    
    %% Set colors
    
    Bkgr = image:color(Image, 200, 200, 200),
    Black = image:color(Image, 0, 0, 0),
    ProcColor = image:color(Image, 0, 255, 0),
    PortColor = image:color(Image, 255, 0, 0),
    
    image:fill(Image, 10,10, Bkgr),
    %% Draw graf, xticks and yticks
    draw_graf(Image, Data, {Black, ProcColor, PortColor}, GrafArea, {Xmin, Ymin, Xmax, Ymax}),
    draw_xticks(Image, Black, XticksArea, {Xmin, Xmax}, Data),
    draw_yticks(Image, Black, YticksArea, {Ymin, Ymax}),
    
    %% Kill image and return binaries
    image:stop(Image).

%% draw_graf(Image, Data, Color, GraphArea, DataBounds)
%% Image, port to Image
%% Data, list of three tuple data, (X, Y1, Y2)
%% Color, {ForegroundColor, ProcFillColor, PortFillColor}
%% DataBounds, {Xmin, Ymin, Xmax, Ymax}

draw_graf(Gif, [{SX,SY1,SY2}|Data], Colors, GraphArea, {Xmin, _Ymin, Xmax, Ymax}) ->
    
    #graph_area{x = X0, y = Y0, width = Width, height = Height} = GraphArea,
    {Black, ProcColor, PortColor} = Colors,

    DX = (Width)/(Xmax - Xmin),
    DY = (Height)/(Ymax),
    
    lists:foldl( 
	fun ({X,Y1,Y2}, {PX, PY}) ->
	    NX1 = trunc(X0 + PX*DX - Xmin*DX),
	    NX2 = trunc(X0 + X*DX - Xmin*DX),
	    if 
	    	abs(trunc(NX1) - trunc(NX2)) > 0 ->
		    NY1 = trunc(Y0 + Height - PY*DY),
		    NY2 = trunc(Y0 + Height - Y1*DY),
		    NY3 = trunc(Y0 + Height - (Y2 + Y1)*DY),

		    ZLY = trunc(Y0 + Height),
	    
		    % Fill procs
		    image:filledRectangle(
		    	Gif,
			NX1,
			ZLY,
			NX2,
			NY2,
		        ProcColor),
	    
		    % fill ports
		    image:filledRectangle(
		    	Gif,
			NX1,
			NY2,
			NX2,
			NY3,
		    	PortColor),
		    % top line
		    image:line(
		    	Gif,
			NX1,
			NY3,
			NX2,
			NY3,
			Black),
		    % left line
		    image:line(Gif,
		    	NX1,
			NY1,
			NX1,
			NY3,
			Black),
		    {X, Y1 + Y2};
		true ->
		    {PX,PY}
	    end
	end, {SX, SY2 + SY1}, Data).

draw_xticks(Image, Color, XticksArea, {Xmin, Xmax}, Data) ->
    #graph_area{x = X0, y = Y0, width = Width} = XticksArea,
    
    DX = Width/(Xmax - Xmin),
    Offset = X0 - Xmin*DX, 
    Y = trunc(Y0),
    Font = small,
    {FontW, _FontH} = image:fontSize(Image, Font),
    image:line(Image, trunc(X0), Y, trunc(X0 + Width), Y, Color), 
    lists:foldl(
    	fun ({X,_,_}, PX) ->
	    X1 = trunc(Offset + X*DX),
	    
	    % Optimization:
	    % if offset has past half the previous text
	    % start checking this text
	    
	    if 
	    	X1 > PX ->
		    Text = lists:flatten(io_lib:format("~.3f", [float(X)])),
		    TextLength = length(Text),
		    TextWidth = TextLength*FontW,
		    Spacing = 2,
		    if 
		    	X1 > PX + round(TextWidth/2) + Spacing ->
		    	    image:line(Image, X1, Y - 3, X1, Y + 3, Color),
		    	    image:text(Image, Font, X1 - round(TextWidth/2), Y + 2, Text, Color),
			    X1 + round(TextWidth/2) + Spacing;
			true ->
			    PX
		    end;
		true ->
		    PX
	    end
	end, 0, Data).

draw_yticks(Gif, Color, YticksArea, {_, Ymax}) ->
    #graph_area{x = X0, y = Y0, width = Width, height = Height} = YticksArea,
    DY = (Height)/(Ymax),
    Ys = lists:seq(0, trunc(Ymax)),
    X = trunc(X0 + Width),
    image:line(Gif, X, trunc(0 + Y0), X, trunc(Y0 + Height), Color),
    lists:foreach(
    	fun (Y) ->
	    Y1 = trunc(Y0 + Height -  Y*DY),
	    image:line(Gif, X - 3, Y1, X + 3, Y1, Color), 
	    Text = lists:flatten(io_lib:format("~p", [Y])),
	    image:text(Gif, small, 0, Y1 - 4, Text, Color)
	end, Ys).

%%% -------------------------------------
%%% ACTIVITIES
%%% -------------------------------------

%% activities(Width, Height, Range, Activities) -> Binary
%% In:
%%	Width = integer()
%%	Height = integer()
%%	Range = {float(), float()}
%%	Activities = [{float(), active | inactive}]
%% Out:
%%	Binary = binary()

activities(Width, Height, {UXmin, UXmax}, Activities) ->
    Xs = [ X || {X,_} <- Activities],
    Xmin = lists:min(Xs),
    Xmax = lists:max(Xs),
    activities0(Width, Height, {lists:min([Xmin, UXmin]), lists:max([UXmax, Xmax])}, Activities).

activities(Width, Height, Activities) ->
    Xs = [ X || {X,_} <- Activities],
    Xmin = lists:min(Xs),
    Xmax = lists:max(Xs),
    activities0(Width, Height, {Xmin, Xmax}, Activities).

activities0(Width, Height, {Xmin, Xmax}, Activities) ->
    
    Image = image:start(Width, Height),
    Grey = image:color(Image, 200, 200, 200),
    HO = 20,
    ActivityArea = #graph_area{x = HO, y = 0, width = Width - 2*HO, height = Height},
    image:filledRectangle(Image, 0, 0, Width, Height, Grey),
    draw_activity2(Image, {Xmin, Xmax}, ActivityArea, Activities),
    image:stop(Image).

draw_activity2(Image, {Xmin, Xmax}, ActivityArea, [{X, Activity}|Activities]) ->
    #graph_area{width = Width} = ActivityArea,

    White = image:color(Image, 255, 255, 255),
    Green = image:color(Image, 0, 255, 0),
    Black = image:color(Image, 0, 0, 0),
    
    DX = (Width)/(Xmax - Xmin),
    draw_activity2(Image, {DX, X, Activity}, ActivityArea, {Green, White, Black}, Activities).

draw_activity2(_Image, {_DX, _PX, _PA}, _ActivityArea, _Colors, []) -> ok;
draw_activity2(Image, {DX, PX, PA}, ActivityArea, {C1, C2, C3}=Colors, [{X, Act}|Data]) ->
    #graph_area{x = X0, height=Height} = ActivityArea,
    X1 = erlang:trunc(X0 + DX*PX),
    X2 = erlang:trunc(X0 + DX*X),
    case PA of 
	inactive ->
	    image:filledRectangle(Image, X1, 0, X2, Height - 1, C2),
	    image:rectangle(Image, X1, 0, X2, Height - 1, C3),
	    draw_activity2(Image, {DX, X, Act}, ActivityArea, Colors, Data);
	active ->
	    image:filledRectangle(Image, X1, 0, X2, Height - 1, C1),
	    image:rectangle(Image, X1, 0, X2, Height - 1, C3),
    	    draw_activity2(Image, {DX, X, Act}, ActivityArea, Colors, Data)
    end.


%%% -------------------------------------
%%% Process lifetime
%%% Used by processes page
%%% -------------------------------------

proc_lifetime(Width, Height, Start, End, ProfileTime) ->
    Im = image:start(round(Width), round(Height)),
    Grey = image:color(Im, 200,200,200),
    Black = image:color(Im, 0, 0, 0),
    Green = image:color(Im, 0, 255, 0),

    % Background

    image:fill(Im, round(Width/2), round(Height/2), Grey),
    
    % Ratio and coordinates

    DX = (Width-1)/ProfileTime,
    X1 = round(DX*Start),
    X2 = round(DX*End),

    % Paint
    image:filledRectangle(Im, X1, 0, X2, Height - 1, Green),
    image:rectangle(Im, X1, 0, X2, Height - 1, Black),

    image:stop(Im).

%%% -------------------------------------
%%% Percentage
%%% Used by process_info page
%%% Percentage should be 0.0 -> 1.0
%%% -------------------------------------
percentage(Width, Height, Percentage) ->
    Im = image:start(round(Width), round(Height)),
    Grey = image:color(Im, 200,200,200),
    Black = image:color(Im, 0, 0, 0),
    Green = image:color(Im, 0, 255, 0),

    % Background

    image:fill(Im, round(Width/2), round(Height/2), Grey),
    
    % Ratio and coordinates

    X = round(Width - 1 - Percentage*(Width - 1)),

    % Paint
    image:filledRectangle(Im, X, 0, Width - 1, Height - 1, Green),
    {FontW, _} = image:fontSize(Im, tiny), 
    String = lists:flatten(io_lib:format("~.10B %", [round(100*Percentage)])),

    image:text(	Im, 
    		tiny,
		round(Width/2 - (FontW*length(String)/2)), 
		1, 
		String,
		Black),
    image:rectangle(Im, X, 0, Width - 1, Height - 1, Black),

    image:stop(Im).

