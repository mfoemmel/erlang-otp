%% Copyright (c) 1996 by Fredrik Ström & Peter Molin.All Rights Reserved 
%% Time-stamp: <>
%% ====================================================================
%%  Filename : 	draw.hrl
%%  Module   :	draw
%%  Purpose  :  headerfile for draw.erl 
%%  Notes    : 
%%  History  :	* 1996-09-03 Fredrik Strom EX (fredriks@erlang.ericsson.se): Created.
%% ====================================================================
%% Exported functions (short description):
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% the state needed in the draw loop.
%% selected: a list of selected widgets.
%% shift: used to select several widgets.
%% gsb: a handle to gsb.
%% windows: the windows in the application. We must have controll of these
%%          since a click in a window should unselect other selected widgets.
%% standard: a field for class data, when a new editor is added a new field
%%           can be added if data neeed to be in the state.
%%           pop_menu pointer can be saved here.
%% menu: see above.
%% click: used to tell draw that clicks should be ignored. A click ona widget
%%        also generates a click from the backgroundwindow and somtimes
%%        this must be avoided.
-record(draw,{selected=[],shift=false,windows=[],gsb,click,menu=[]}).


%% runtime info needed in all widgets.
%% class: what module standard, menu...
%% class_info: implementation information.
%% gstype: obvious.
%% softparent: the highest gsparent in the softwidget.
%% type: soft or hard.
%% parent: gsparent (need bnot be == softparent)
-record(widget,{type,gstype,softparent,class,parent,class_data}).

