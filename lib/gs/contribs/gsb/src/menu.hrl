%% Copyright (c) 1996 by Fredrik Ström & Peter Molin.All Rights Reserved 
%% Time-stamp: <>
%% ====================================================================
%%  Filename : 	menu.hrl
%%  Module   :	menu
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 1996-09-26 Fredrik Strom EX (fredriks@erlang.ericsson.se): Created.
%% ====================================================================
%% Exported functions (short description):
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%-module(menu).

-record(mcont,{children=[],lastx=0,lasty=0,win,winx,winy,visible,sizeleft}).
-record(mitem,{menu,win,winx,winy}).
-record(mwidget,{x,y,winx,winy,key}).
-record(select,{nx,ny,sx,sy,ex,ey,wx,wy,height,width,border}).

-define(BAR_HEIGHT,30).
-define(ITEM_HEIGHT,25).
-define(SEP_HEIGHT,2).
-define(ITEM_WIDTH,80).






