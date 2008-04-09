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

-include("egd.hrl").

%% Definitions
%-type(line_span_data() :: {
%	Z  	:: integer(), 
%	X0 	:: integer(), 
%	X1 	:: integer(), 
%	Color 	:: rgba_float()}).

binary(Image) -> 
    binary(Image, opaque).

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
    URLSs = resulting_line_spans(lists:reverse([LSB|OLSs]),Type, []),

    % FIXME: Can we keep the list sorted instead of sorting it?
    % sort descending
    RLSs = lists:sort(fun ({_,A,_,_}, {_,B,_,_}) -> if A < B -> false; true -> true end end, URLSs),

    Scanline = resulting_scanline(RLSs,Width),

    scanlines({Yi, Y - 1}, Os, LSB, Type, [Scanline|Scanlines]).

resulting_scanline(RLSs, Width) -> resulting_scanline(RLSs, Width, []).
resulting_scanline([], _, Scanlines) -> Scanlines;
resulting_scanline([{_,Xl, Xr, C} | RLSs], Width, Scanlines) ->
    {R,G,B,_} = rgb_float2byte(C),
    %% FIXME: optimize
    Scanline = lists:duplicate(trunc(Xr - Xl + 1), <<R:8,G:8,B:8>>),
    resulting_scanline(RLSs, Width, [Scanline|Scanlines]).

%print_line_spans([]) -> io:format("~n"), ok;
%print_line_spans([{_,Xl,Xr,_}|LSs]) ->
%    io:format("[~p,~p]", [Xl,Xr]),
%    print_line_spans(LSs).

%check_binary(Bin, {_,_,W, _}) ->
%   Size = erlang:size(erlang:list_to_binary(Bin)),
%   ScanlineWidth = trunc(Size/3),
%   if 
%   	ScanlineWidth =/= W + 1 ->
%	    io:format("check_binary: Image width ~p vs. Scanline width ~p~n", [W+1,ScanlineWidth]),
%	    throw(bad_binary);
%	true -> ok
%   end.

%check_line_spans([]) -> ok;
%check_line_spans([_]) -> ok; 
%check_line_spans([LSa, LSb | LSs]) ->
%    {_,Xal,_Xar,_} = LSa,
%    {_,_Xbl,Xbr,_} = LSb,
%    Overlap = do_lines_overlap(LSa,LSb),
%    LLSa = ls_length(LSa),
%    LLSb = ls_length(LSb),
%
%    if
%	LLSa < 0 ->
%	    io:format("check_line_spans:~p ~n", [Overlap]),
%	    debug_print_ls_states([{ls_left, LSa},{ls_right, LSb}]),
%	    throw({bad_length, ls_left});
%	LLSb < 0 ->
%	    io:format("check_line_spans:~p ~n", [Overlap]),
%	    debug_print_ls_states([{ls_left, LSa},{ls_right, LSb}]),
%	    throw({bad_length, ls_right});
%    	Overlap =/= false ->
%	    io:format("check_line_spans:~p ~n", [Overlap]),
%	    debug_print_ls_states([{ls_left, LSa},{ls_right, LSb}]),
%	    throw(overlap_error);
%	Xal - Xbr =/= 1 ->	
%	    io:format("check_line_spans:~p~n", [Overlap]),
%	    debug_print_ls_states([{ls_left, LSa},{ls_right, LSb}]),
%	    throw(contiuation_error);
%	true -> check_line_spans([LSb|LSs])
%    end.

% resulting_line_spans
% In:
%	LineIntervals = [line_span_data()]
% Out:
%	LineIntervals
% Purpose:
%	Reduce line spans. This function handles the overlapping color spans on a line.
%	Måste troligen ha en sorterad lista med object, där översta objecte ligger först i stacken.
%	FÖr varje object måste man titta under och se vilka den överlappar och antingen,
%	1) simple
%		Ta bort de som täcks eller dela upp underliggande span från det överlappande.
%	2) alpha
%		todo...

resulting_line_spans([], _, MLSs) -> MLSs;
resulting_line_spans([LS|LSs], Type, MLSs) ->
    resulting_line_spans(LSs, Type, merge_line_spans(LS, MLSs, Type)).


% merge_line_spans
% In: merge_line_spans/3
%	CLS :: line_span_data(), current line span
%	MLSs :: [line_span_data()] (non overlapping, continious, unsorted)
%	Type :: alpha | opaque (render engine, color blending or not)
% In: merge_line_spans/3
%	 CLSs :: [line_span_data()], current line spans
%	LMLSs :: [line_span_data()], left merged line spans (iter)
%	RMLSs :: [line_span_data()], right merged line spans (iter)
%	Type :: alpha | opaque (render engine, color blending or not)
% Out:
%	[line_span_data()] (non overlapping, continious, unsorted)
% Purpose:
%	Entwines color intervals with non overlapping color intervals,
%	Remember: CLS may split and become several CLSs (at least two) -> handle it.

% OK, here we go!
merge_line_spans(CLS, MLSs, Type) ->
    merge_line_spans([CLS], [], MLSs, Type). 	% init
merge_line_spans(CLS, [], [], _Type) -> 
    CLS; % If it is the first line span of MLSs
merge_line_spans(CLSs, MLSs, [], _Type) -> 
    lists:flatten([CLSs|MLSs]);   % Here we have iterated through all MLSs; add CLSs.
merge_line_spans([], LMLSs, RMLSs, _Type) -> 
    lists:flatten([LMLSs|RMLSs]); % FIXME: optimize? all CLSs is shadowed (in opaque)
merge_line_spans(CLSs, LMLSs, [ILS|RMLSs], Type) -> 
    merge_line_spans([], CLSs, LMLSs, ILS, RMLSs, Type).

merge_line_spans(LCLSs, [], LMLSs, ILS, RMLSs, Type) -> 
    merge_line_spans(LCLSs, [ILS|LMLSs], RMLSs, Type);
merge_line_spans(LCLSs, [{Zc,Xcl,Xcr,Cc}=CLS|RCLSs], LMLSs, {_Zi,Xil,Xir,_Ci}=ILS, RMLSs, opaque) ->
    % Compare the current line span with the iterating one and see if
    % something shines through the overlapping object.
    % Think: How does each RMLS affect the CLS object if they overlap?

    %  ILS ------------ (upper)
    %  CLS ------------ (lower)
    case do_lines_overlap(CLS,ILS) of
	% the current line span is ...
	left_partial -> % cut it
	    CLS0 = {Zc, Xcl, Xil - 1, Cc},
	    merge_line_spans([CLS0|LCLSs], RCLSs, LMLSs, ILS, RMLSs, opaque);
	right_partial -> % cut it
	    CLS0 = {Zc, Xir + 1, Xcr, Cc},
	    merge_line_spans([CLS0|LCLSs], RCLSs, LMLSs, ILS, RMLSs, opaque);
	outspaced -> % split and cut it
	    CLS0l = {Zc, Xcl, Xil - 1, Cc},
	    CLS0r = {Zc, Xir + 1, Xcr, Cc},
	    % Are we not supposed to put CLS0r in RCLSs to view it with other
	    % MLSs? Seems to work now though...
	    merge_line_spans([CLS0l, CLS0r | LCLSs], RCLSs, LMLSs, ILS, RMLSs, opaque);
	shadowed -> % drop it
	    merge_line_spans(LCLSs, RCLSs, LMLSs, ILS, RMLSs, opaque);
	false -> 
	    merge_line_spans([CLS|LCLSs], RCLSs, LMLSs, ILS, RMLSs, opaque) % do the same for the next CLS
    end;
merge_line_spans(LCLSs, [{Zc,Xcl,Xcr,Cc}=CLS|RCLSs], LMLSs, {Zi,Xil,Xir,Ci}=ILS, RMLSs, alpha) ->
    %  ILS ------------ (upper)
    %  CLS ------------ (lower)
    case do_lines_overlap(CLS,ILS) of
	% the current line span is ...
	left_partial -> % cut it
	    if 
	    	Xir > Xcr ->
	    	CLS0 = {Zc, Xcl, Xil - 1, Cc},              % Cut current line span
	    	ALS  = {Zi, Xil, Xcr, alpha_blend(Ci,Cc)},  % alphalized part of iterating line span
	    	ILS0 = {Zi, Xcr + 1, Xir, Ci},              % Cut part of iterating line span
%	        ok = check_line_spans([ILS0, ALS, CLS0]), 
	    	merge_line_spans([CLS0|LCLSs], RCLSs, LMLSs, ALS, [ILS0|RMLSs], alpha);
	    true ->
	    	CLS0 = {Zc, Xcl, Xil - 1, Cc},              % Cut current line span
	    	ALS  = {Zi, Xil, Xcr, alpha_blend(Ci,Cc)},  % alphalized part of iterating line span
%	        ok = check_line_spans([ALS, CLS0]), 
	    	merge_line_spans([CLS0|LCLSs], RCLSs, LMLSs, ALS, RMLSs, alpha)
	    end;
	right_partial -> % cut it
	    if 
	    Xil < Xcl ->
	    	ILS0 = {Zi, Xil, Xcl - 1, Ci},              % Cut part of iterating line span
	    	ALS  = {Zi, Xcl, Xir, alpha_blend(Ci,Cc)},  % alphalized part of iterating line span
	    	CLS0 = {Zc, Xir + 1, Xcr, Cc},              % Cut current line span
%	        ok = check_line_spans([CLS0, ALS, ILS0]), 
	    	merge_line_spans([CLS0|LCLSs], RCLSs, LMLSs, ILS0, [ALS|RMLSs],alpha);
	    true ->
	    	ALS  = {Zi, Xcl, Xir, alpha_blend(Ci,Cc)},  % alphalized part of iterating line span
	    	CLS0 = {Zc, Xir + 1, Xcr, Cc},              % Cut current line span
%	        ok = check_line_spans([CLS0, ALS]), 
	    	merge_line_spans([CLS0|LCLSs], RCLSs, LMLSs, ALS, RMLSs, alpha)
	    end;
	outspaced -> % split and cut it
	    CLS0l = {Zc, Xcl, Xil - 1, Cc},
	    ALS   = {Zi, Xil, Xir, alpha_blend(Ci,Cc)},
	    CLS0r = {Zc, Xir + 1, Xcr, Cc},
%	    ok = check_line_spans([CLS0r, ALS, CLS0l]), 
	    merge_line_spans([CLS0l,CLS0r | LCLSs], RCLSs, LMLSs, ALS, RMLSs, alpha);
	shadowed -> % dont drop it, alpha it
	    if 
	    Xil == Xcl , Xir == Xcr ->
		ALS = {Zi, Xil, Xir, alpha_blend(Ci,Cc)},
	        merge_line_spans(LCLSs, RCLSs, LMLSs, ALS, RMLSs, alpha);
	    Xil == Xcl ->
		ALS  = {Zi, Xil, Xcr, alpha_blend(Ci,Cc)},
		ILSr = {Zi, Xcr + 1, Xir, Ci},
%	        ok = check_line_spans([ILSr, ALS]), 
	        merge_line_spans(LCLSs, RCLSs, LMLSs, ALS, [ILSr|RMLSs], alpha);
	    Xir == Xcr ->
	        ILSl = {Zi, Xil, Xcl - 1, Ci},
	        ALS   = {Zi, Xcl, Xcr, alpha_blend(Ci,Cc)},
%	        ok = check_line_spans([ALS,ILSl]), 
	        merge_line_spans(LCLSs, RCLSs, LMLSs, ILSl, [ALS|RMLSs], alpha);
	    true ->
	        ILS0l = {Zi, Xil, Xcl - 1, Ci},
	        ALS   = {Zc, Xcl, Xcr, alpha_blend(Ci,Cc)},
	        ILS0r = {Zi, Xcr + 1, Xir, Ci},
%	        ok = check_line_spans([ILS0r, ALS, ILS0l]), 
	        merge_line_spans(LCLSs, RCLSs, LMLSs, ILS0l, [ILS0r,ALS|RMLSs], alpha)
	    end;
	false -> 
	    merge_line_spans([CLS|LCLSs], RCLSs, LMLSs, ILS, RMLSs, alpha) % do the same for the next CLS
    end.



alpha_blend({R1,G1,B1,A1}, {R2,G2,B2,A2}) ->
  Beta = A2*(1 - A1),
  A = A1 + Beta,
  R = R1*A1 + R2*Beta,
  G = G1*A1 + G2*Beta,
  B = B1*A1 + B2*Beta,
  {R,G,B,A}.

do_lines_overlap({_,Xll, Xlr, _}, {_,Xul, Xur,_}) ->
    if 
	% Xul -------------- Xur
	% Xll -------------- Xlr
    	Xlr < Xul -> false;
	Xll > Xur -> false;
	true ->
	   if 
		% ---------
		%      ---------
	   	Xul =< Xll , Xur < Xlr -> right_partial;

		%     ------------
		% ------
		Xll < Xul , Xlr =< Xur -> left_partial;
		
		%    --------    
		% ---------------
		Xul > Xll , Xur < Xlr -> outspaced;
		
		% -------------
		%     ----
		Xul =< Xll , Xur >= Xlr -> shadowed;
		true ->
		    %debug_print_ls_states([{upper_ls, LSu}, {lower_ls, LSl}]),
		    throw(non_complete_closure)
	   end
    end.
    
%ls_length({_,Xl,Xr,_}) -> Xr - Xl.

%debug_print_ls_states([]) -> ok;
%debug_print_ls_states([{Name, {_,Xl,Xr,_}=LS}|LSs]) ->
%    io:format("- ~p Xl=~p, Xr=~p -> L=~p~n", [Name, Xl,Xr,ls_length(LS)]),
%    debug_print_ls_states(LSs);
%debug_print_ls_states([{Name, Msg}|LSs]) -> 
%    io:format("- ~p -> ~p~n", [Name, Msg]),
%    debug_print_ls_states(LSs).

% parse_objects_on_line
% In: 
%	Y :: index of height
%	Width :: width of image
%	Objects :: [image_object()]
% Out:
%	LineData :: [ObjectLineData]
%	ObjectLineData :: {Z, X,  Length, RGB}
% Purpose:
%	Calculate the resulting length and color for each object.

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

