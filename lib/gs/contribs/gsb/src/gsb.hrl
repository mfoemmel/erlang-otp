%% Copyright (c) 1996 by Fredrik Ström o Peter Molin.  All Rights Reserved 
%% Time-stamp: <>
%% ====================================================================
%%  Filename : 	gsb.hrl.
%%  Module   :	gsb
%%  Purpose  :  Header file for GSB project.
%%  Notes    : 
%%  History  :	* 1996-07-10 Peter Molin EX (peterm@csd.uu.se): Created.
%% ====================================================================
%% Exported functions (short description):
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-define(BASEWIDTH,300).
-define(BASEHEIGHT, 330).
-define(BASEX,300).
-define(BASEY,300).
-define(TOOLHEIGHT,265).
-define(TOOLWIDTH,150).
-define(TBOXX,0).
-define(TBOXY,25).
-define(HIERX,150).
-define(HIERY,25).
-define(LABELHEIGHT,25).
-define(LABELWIDTH,?BASEWIDTH).
-define(LABELX,0).
-define(LABELY,300).
-define(TIMEOUT, 10000).
-define(DEBUG, true).


%% gsb_db record
%% key      - unik key
%% type     - dynamic/static widget
%%            (dynamic - in the drawing area, static in toolbox)
%% name     - User defined name, only for dynamic objects
%% props    - properites for the widget
%% events   - events for the widget
%% children - children to the widget
%% counter  - Only for static widgets, keeps track of created widgets
%% pre_defined - Only for static widgets, differintiate widgets and softwidgets

-record(gsb_db, {key, name, class, gs_type, type, hierarchy, module, exports,
		 props, events, children, parent}).

-record(tb_db, {key, name, class, gs_type, type, module, exports, props,
		events, children, counter}).

-record(create_tb, {key, name, class, gs_type, type, module,exports, props,
		    events, children}).

-record(hierarchy, {visible, children_visible, level, text}).

-record(option_db, {name, editor}).

-record(create_widget, {key,name,class,gs_type,type=hard,softparent=foo,
			props,children}).

%% the state in the gsb loop
%% windows      - win record
%% tool         - Pid, PId to the toolbox.
%% too_file     - FileName
%% project_file - standard filename for projects.
%% draw         - Pid
%% option       - Pid
%% hier         - Pid to hierarchy
%% gs           - the top graphical intance
%% db           - the gsb_db.     

-record(gsb,{windows,tool,tool_file,win_file,draw,action,
	     option,hier,db,gs,win,clipboard,selected}).


%% window info.
%% key    - window
%% name   - window name to show in the menues
%% file   - filename id widget is a top window else undefined.
%% gslist - list of menuitems that represent this window. If the widget
%%          is a top window it occurs in the save menu, save_as menu and in
%%          the remove window. All window accurs in the window menu.
-record(win,{key,name,file,gslist}).

%% the datafieled in menuitems.
%% command - save,save_as,remove,show
%% win     - the correspondig widget.
-record(item,{command,win}).



%% a clipboard contains widget to paste.
%% state : copy - elements should not be deleted in the database when
%%                a new copy or cut is made.
%%         cut - the elements should be deleted in the database.
%% elements: the acutall elements.
-record(clip,{state=copy,elements=[]}).
