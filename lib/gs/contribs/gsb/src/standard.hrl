%% Copyright (c) 1996 by Fredrik Ström & Peter Molin.All Rights Reserved 
%% Time-stamp: <>
%% ====================================================================
%%  Filename : 	standard.hrl
%%  Module   :	standard
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 1996-09-09 Fredrik Strom EX (fredriks@erlang.ericsson.se): Created.
%% ====================================================================
%% Exported functions (short description):
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(GRID_SIZE,10).
-define(GRID_COLOR,gray).

-define(MAX,10000).
-define(MIN,0).

-define(MAIN_CURSOR,arrow).
-define(RESIZE_CURSOR,resize).
-define(MOVE_CURSOR,hand).

-record(select,{nx,ny,sx,sy,ex,ey,wx,wy,height,width,border}).
-record(grid,{snap,width,height}).
-record(stand,{cpopup,spopup,sitems,citems}).

-record(grid_item,{parent}).
