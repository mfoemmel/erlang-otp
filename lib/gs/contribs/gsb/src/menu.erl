%% Copyright (c) 1996 by Fredrik Ström & Peter Molin.All Rights Reserved 
%% Time-stamp: <>
%% ====================================================================
%%  Filename : 	menu.erl
%%  Module   :	menu
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 1996-09-26 Fredrik Strom EX (fredriks@erlang.ericsson.se): Created.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exported functions (short description):
%% ------------------------------------------
%% create(CreateRecord,Parent,Info)-> NInfo
%% CreateRecord - a record from the database to create graphically
%% Parent - the gs parent of the widget in CreateRecord
%% Info - the state in the draw loop.
%% ----------------------------------------
%% create(WidgetType,Parent,X,Y,Info) -> NInfo
%% WidgetType - the widget to create.
%% Parent - the parent to WidgetType.
%% Info - the state in the draw state.
%% -----------------------------------------
%% delete(Widget,Info)-> NInfo
%% - remove the widget from gs.
%% -----------------------------------------
%% paste(Widget,Widget,WidgeData,X,Y,Info)-> NInfo
%% Widget - the new parent to the widgets in WidgetList.
%% WidgetData - implementation stuff.
%% Info - the state in the draw loop.
%% ------------------------------------------
%% select_widget(Widget,WidgetId,WidgetData,ParentId,Info)->Pid.
%% Widget - the widget to select
%% WidgetData - implementation stuff.
%% Pid a pointer to a process that handle movement and resizeing
%% ------------------------------------------
-module(menu).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/2 $').
-revision('$Revision: 1.13 $').

-export([create_widget/3,create_widget/6,delete_widget/2,config_widget/3,
	 paste_widget/6,select_widget/6,select/7,popup_menu/6]).

-include("draw.hrl").
-include("gsb.hrl").
-include("menu.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exported functions                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ____________________________________________________________________
%%
%%  create_widget(Widget,Parent,Data,X,Y,Info)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

create_widget(Widget,Parent,Data,X,Y,Info)->
  create_widget(create,Widget,Parent,Data,X,Y,Info).

%% ____________________________________________________________________
%%
%%  create_widget(Record,Parent,Info)    
%%  Args    : Record - create record see gsb.hrl.
%%            Info - the draw state, where inforamtion can be retrevied and/or
%%                   saved.
%%  Returns :
%%  Comments: The widget is alreday created in the database and all we have
%%            to do is to create the graphics.
%% ____________________________________________________________________


create_widget(Record,Parent,Info) when record(Record,create_widget)->
  Key=Record#create_widget.key,
  GsType=Record#create_widget.gs_type,
  Type=Record#create_widget.type,
  SoftParent=Record#create_widget.softparent,
  Name=Record#create_widget.name,
  Class=Record#create_widget.class,

  Props=Record#create_widget.props,
  %% the data attribute can be set to runtime information. But since we use
  %% this attribute during the build, it must stay in the gsb_db only
  NProps=standard:remove_data_prop(Props),
  PData=gs:read(Parent,data),
  PGsType=PData#widget.gstype,
     
  Label=case lists:keysearch(label,1,Props) of
	  {value,{label,{text,Text}}}->Text;
	  Other->
	    "Undefined"
	end,
  
  Data=#widget{class=Class,parent=Parent,type=Type,gstype=GsType,
	       softparent=SoftParent},
  
  case GsType of
    %% create menubar
    menubar->
      PWidth=gs:read(Parent,width),
      {value,{height,Height}}=lists:keysearch(height,1,Props),
      MData=#mcont{winx=0,winy=0,win=Parent,sizeleft=PWidth},
      NData=insert_menu(Data,MData),
      gs:create(frame,Key,Parent,[{x,0},{y,0},{width,PWidth},
				  {buttonpress,true},{bw,1},{relief,raised},
				  {height,Height},{data,NData}]),
      Menubars=Info#draw.menu,
      Info#draw{menu=[Parent|Menubars]};
    
    %%create a menubutton
    menubutton->
      PMData=get_menu_data(PData),
      {X,Y,WinX,WinY,Win,SizeLeft}=get_data(PMData),
      %% need the window info for menues.
      NMData=#mitem{winx=WinX,winy=WinY,win=Win},
      NData=insert_menu(Data,NMData),
      Item=gs:create(button,Key,Parent,
		     [{x,X},{y,Y},{width,?ITEM_WIDTH},{label,{text,Label}},
		      {buttonpress,true},{click,false},
		      {height,?ITEM_HEIGHT},{data,NData}]),
      %% update the father.
      ListEl=#mwidget{winx=WinX,winy=WinY,x=X,y=Y,key=Item},
      NSizeLeft=SizeLeft-?ITEM_WIDTH,
      if
	(NSizeLeft < ?ITEM_WIDTH)->
	  NPMData=insert_last(PMData,ListEl,X+?ITEM_WIDTH,Y,
			      NSizeLeft+?ITEM_WIDTH),
	  NPData=insert_menu(PData,NPMData),
	  NPWidth=gs:read(Parent,width)+?ITEM_WIDTH,
	  gs:config(Parent,[{width,NPWidth},{data,NPData}]);
	true->
	  NPMData=insert_last(PMData,ListEl,X+?ITEM_WIDTH,Y,NSizeLeft),
	  NPData=insert_menu(PData,NPMData),
	  gs:config(Parent,[{data,NPData}])
      end,
      Info;

    %% create a menu
    menu->
      PMData=get_menu_data(PData),
      {WinX,WinY,Win}=get_data(PMData),
      {NWinX,NWinY}=case PGsType of
		      menubutton->{WinX,WinY+?ITEM_HEIGHT};
		      menuitem_cascade->{WinX+?ITEM_WIDTH,WinY}
		    end,
      NMData=#mcont{winx=NWinX,winy=NWinY,win=Win,visible=true},
      NData=insert_menu(Data#widget{parent=Win},NMData),
      Item=gs:create(frame,Key,Win,
		     [{x,NWinX},{y,NWinY},{width,?ITEM_WIDTH},
		      {buttonpress,true},{bw,1},{relief,raised},
		      {height,?ITEM_HEIGHT},{data,NData}]),
      %% update the father.
      NPMData=PMData#mitem{menu=Item},	  
      NPData=insert_menu(PData,NPMData),
      gs:config(Parent,{data,NPData}),
      Info;
    %% menuitem
    _->
      case PGsType of
	%% create a menuitem
	menu->
	  PMData=get_menu_data(PData),
	  {X,Y,WinX,WinY,Win,_SizeLeft}=get_data(PMData),
	  %% need the window info for menues.
	  NMData=#mitem{winx=WinX,winy=WinY,win=Win},
	  {ButtonType,NGsType,IHeight,BLabel}=get_menu_values(Props,Label),
	  NData=insert_menu(Data#widget{gstype=NGsType},NMData),
	  Item=gs:create(ButtonType,Key,Parent,
			 [{x,X},{y,Y},{width,?ITEM_WIDTH},{buttonpress,true},
			  {label,{text,BLabel}},{click,false},
			  {height,IHeight},{data,NData}]),
	  
	  ListEl=#mwidget{winx=WinX,winy=WinY,x=X,y=Y,key=Item},
	  %% update the father.
	  PHeight=gs:read(Parent,height),
	  NPHeight=PHeight+IHeight,
	  NPMData=insert_last(PMData,ListEl,X,Y+IHeight,IHeight),
	  NPData=insert_menu(PData,NPMData),

	  gs:config(Parent,[{height,NPHeight},{data,NPData}]),
	  Info
      end
  end.



get_menu_values(Props,Label)->
  case lists:keysearch(itemtype,1,Props) of
    {value,{itemtype,radio}}->{radiobutton,menuitem_radio,?ITEM_HEIGHT,Label};
    {value,{itemtype,check}}->{checkbutton,menuitem_check,?ITEM_HEIGHT,Label};
    {value,{itemtype,cascade}}->{button,menuitem_cascade,?ITEM_HEIGHT,Label};
    {value,{itemtype,normal}}->{button,menuitem,?ITEM_HEIGHT,Label};
    {value,{itemtype,separator}}->{button,menuitem_separator,?SEP_HEIGHT,""}
  end.
	
%% ____________________________________________________________________
%%
%%  paste_widget(Widget,Parent,Data,X,Y,Info)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

paste_widget(Widget,Parent,Data,X,Y,Info)->
  create_widget(copy,Widget,Parent,Data,X,Y,Info).

%% ____________________________________________________________________
%%
%%  select_widget(Widget,WidgetId,WidgetData,ParentId,Info)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

select_widget(Draw,Widget,WidgetId,WidgetData,ParentId,Info)->
  spawn(menu,select,[Info#draw.gsb,Draw,Widget,WidgetData,
			 WidgetId,ParentId,Info]).

%% ____________________________________________________________________
%%
%%  delete_widget(Widget,,WidgetData,Info)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

delete_widget(Widget,Info)->
  case gs:read(Widget,id) of
    %% the widget is already destroyed. The menubar will leave it's windowname
    %% in the drawstate because we cannot get the windowname. Both the
    %% graphical and the database representation is already destroyed.
    {error,{no_such_object,_Id}}->
      Info;
    %% the widget exists.
    Other->
      WidgetData=gs:read(Widget,data),
      GsType=WidgetData#widget.gstype,
      case GsType of
	menubutton->
	  remove_item(Widget,WidgetData),
	  Info;
	menu->
	  Id=gs:read(Widget,id),
	  MData=get_menu_data(WidgetData),
	  delete_menu(Id,MData),
	  Info;
	menubar ->
	  remove_mbar(Widget,WidgetData,Info);
	%% menuitem
	_MenuItem->
	  remove_item(Widget,WidgetData),
	  Info
      end
  end.

%% ____________________________________________________________________
%%
%%  config_widget(Widget,Config,Info).    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

config_widget(Widget,Config,Info)->
  gs:config(Widget,Config),
  gsb:config_widgets(Info#draw.gsb,[Widget],props,Config).
      
%% ____________________________________________________________________
%%
%%  popup_menu(Widget,WidgetData,Info)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

popup_menu(Widget,Win,WidgetData,X,Y,Info)->
  Selected=length(Info#draw.selected),
  Clipboard=gsb:clipboard_status(Info#draw.gsb),
  {SPopup,SItems}=create_popup(Win,Clipboard,Selected),
  handle_menu(standard:popup(SPopup,SItems,X,Y,Info),Info),
  gs:destroy(SPopup),
  Info.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_menuitem(Type)->
  case Type of
    menuitem_cascade->true;
    menuitem_radio ->true;
    menuitem_check ->true;
    menuitem_separator ->true;
    menuitem ->true;
    _ -> false
  end.

%% ____________________________________________________________________
%%
%%  create_widget(Type,WidgetType,Parent,ParentData,X,Y,Info)    
%%  Args    :
%%  Returns :
%%  Comments:
%% Type could be a softwidget type and not allowed. Test with
%% the gstype. This info must come from draw.
%% ____________________________________________________________________

create_widget(Type,Widget,Parent,ParentData,X,Y,Info)->
  ParentType=ParentData#widget.gstype,
  case Type of
    create->
      case allowed_parent(Widget,ParentType,Parent,Info) of
	true->
	  gsb:create_widget(Info#draw.gsb,Widget,Parent,[]);
	false->  
	  messages:debug(":~p is not an allowed parent to:~p",
			 [ParentType,Widget],menu,create_widget)
      end;
    copy->
      %% cannot use gs:read here since the copy can be executed after
      %% a cut and the widget is then destroyed.
      WType=gsb:lookup_widget(Info#draw.gsb,Widget,gs_type),
      case allowed_parent(WType,ParentType,Parent,Info) of
	true->
	  gsb:copy_widget(Info#draw.gsb,Widget,Parent,[]);
	false->
	  messages:debug(":~p is not an allowed parent to:~p",
			 [ParentType,WType],menu,create_widget)
      end
  end,
  Info.

%% ____________________________________________________________________
%%
%%  allowed_parent(WType,PType)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________


allowed_parent(WType,PType,Parent,Info)->
  case PType of
    window when (WType == menubar)->
      not_already_menubar(Parent,Info);
    window->
      false;
    menubar when (WType == menubutton)->
      true;
    menubar ->
      false;
    menubutton when (WType == menu) ->
      true;
    menubutton->
      false;
    menu ->
      is_menuitem(WType);
    menuitem_cascade when (WType == menu) ->
      true;
    menuitem_radio->
      false;
    menuitem_check->
      false;
    menuitem ->
      false;
    
    Other->
      false
  end.

%% ____________________________________________________________________
%%
%%  get_menu_data(Widget)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

get_menu_data(Data) when record(Data,widget) ->
  Data#widget.class_data;

get_menu_data(Widget)->
  Data=gs:read(Widget,data),
  Data#widget.class_data.

%% ____________________________________________________________________
%%
%%  get_last_coords(Data) when record(Data,mcont->    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

get_data(Data) when record(Data,mcont)->
  LastX=Data#mcont.lastx,
  LastY=Data#mcont.lasty,
  WinX=LastX+Data#mcont.winx,
  WinY=LastY+Data#mcont.winy,
  {LastX,LastY,WinX,WinY,Data#mcont.win,Data#mcont.sizeleft};

get_data(Data) when record(Data,mitem)->
  WinX=Data#mitem.winx,
  WinY=Data#mitem.winy,
  {WinX,WinY,Data#mitem.win}.
  
%% ____________________________________________________________________
%%
%%  already_menubar(Window,Info)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

not_already_menubar(Window,Info)->
  Menubars=Info#draw.menu,
  case lists:member(Window,Menubars) of
    true->
      false;
    false ->
      true
  end.
%% ____________________________________________________________________
%%
%%  insert_menu(Data,Menu)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

insert_menu(Data,Menu)when record(Data,widget)->
  Data#widget{class_data=Menu}.

%% ____________________________________________________________________
%%
%%  insert_last(Menu,Item,NX,NY)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

insert_last(Menu,Item,X,Y,NSize) when record(Menu,mcont)->
  Items=Menu#mcont.children,
  Menu#mcont{lastx=X,lasty=Y,children=insert_last(Items,Item),sizeleft=NSize}.

insert_last([Item|Rest],NItem)->
  [Item|insert_last(Rest,NItem)];

insert_last([],Item)->
  [Item].

%% ____________________________________________________________________
%%
%%  swap_state(Widget,Data)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

toggle_state(Widget,WData)->
  MWData=get_menu_data(WData),
  case MWData#mitem.menu of
    undefined->
      done;
    Menu->
      Data=gs:read(Menu,data),
      MData=get_menu_data(Data),
      case MData#mcont.visible of
	true->
	  hide_menu(Menu,Data,MData);
	false->
	  show_menu(Menu,Data,MData)
      end
  end.
	 
%% ____________________________________________________________________
%%
%%  show_menu(Draw,Widget,WidgetId,WidgetData,ParentId,Info)
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

show_menu(Menu,Data,MData)->
  NMData=MData#mcont{visible=true},
  NData=insert_menu(Data,NMData),
  gs:config(Menu,[{x,MData#mcont.winx},{y,MData#mcont.winy},
		  {data,NData}]).

%% ____________________________________________________________________
%%
%%  hide_item(WidgetId,WidgetData)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

hide_item(Widget,MWData)->
  case MWData#mitem.menu of
    undefined->
      done;
    Menu->
      Data=gs:read(Menu,data),
      MData=get_menu_data(Data),
      hide_menu(Menu,Data,MData)
  end.

hide_items([Item|Rest])->
  Key=Item#mwidget.key,
  hide_item(Key,get_menu_data(gs:read(Key,data))),
  hide_items(Rest);
hide_items([]) ->
  done.

%% ____________________________________________________________________
%%
%%  hide_menu(Menu,Data,MData)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

hide_menu(Menu,Data,MData)->
  Items=MData#mcont.children,
  hide_items(Items),
  NMData=MData#mcont{visible=false},
  NData=insert_menu(Data,NMData),
  gs:config(Menu,[{x,1000},{data,NData}]).


%% ____________________________________________________________________
%%
%%  remove_cont(Widget,WidgetData,Info)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

remove_mbar(Widget,WidgetData,Info)->
  Parent=WidgetData#widget.parent,
  Menubars=Info#draw.menu,
  NMenubars=lists:delete(Parent,Menubars),
  gs:destroy(Widget),
  Info#draw{menu=NMenubars}.

%% ____________________________________________________________________
%%
%%  remove_item(Widget,WidgetData)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

remove_item(Widget,WidgetData)->
  Parent=WidgetData#widget.parent,
  PData=gs:read(Parent,data),
  PMData=get_menu_data(PData),
  WidgetId=gs:read(Widget,id),
  GsType=WidgetData#widget.gstype,
  NMData=remove_child(GsType,PMData,WidgetId),
  NPData=insert_menu(PData,NMData),
  %% måste kolla check,radio osv
  case is_menuitem(GsType) of
    true->
      PHeight=gs:read(Parent,height),
      WHeight=gs:read(Widget,height),
      gs:config(Parent,[{height,PHeight-WHeight},{data,NPData}]);
    false->
      gs:config(Parent,{data,NPData})
  end,
  gs:destroy(Widget).

%% ____________________________________________________________________
%%
%%  remove_child(Type,MData,Widget)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

remove_child(Type,MData,Widget)->
  Children=MData#mcont.children,
  {NChildren,LastX,LastY}=remove_item(Type,Widget,Children),
  MData#mcont{children=NChildren,lastx=LastX,lasty=LastY}.

%% ____________________________________________________________________
%%
%%  remove_item(Type,Id,Children)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

remove_item(Type,Id,[Item|Rest])->
  case Item#mwidget.key of
    Id->
      WinX=Item#mwidget.winx,
      WinY=Item#mwidget.winy,
      X=Item#mwidget.x,
      Y=Item#mwidget.y,
      
      {NChildren,LastX,LastY}=propagate_children(Type,Rest,WinX,WinY,X,Y);
    Other ->
      {NChildren,LastX,LastY}=remove_item(Type,Id,Rest),
      {[Item|NChildren],LastX,LastY}
  end.

%% ____________________________________________________________________
%%
%%  propagate_children(Type,Children,WinX,WinY,X,Y)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

propagate_children(Type,[Item|Rest],WinX,WinY,X,Y)->
  Id=Item#mwidget.key,
  case Type of
    menubutton->
      Width=gs:read(Id,width),
      NItem=propagate(Type,Item,WinX,WinY,X,Y),
      {NChildren,LastX,LastY}=propagate_children(Type,Rest,WinX+Width,WinY,
						 X+Width,Y),
      {[NItem|NChildren],LastX,LastY};
    %% menu item
    _ ->
      Height=gs:read(Id,height),
      NItem=propagate(Type,Item,WinX,WinY,X,Y),
      {NChildren,LastX,LastY}=propagate_children(Type,Rest,WinX,WinY+Height,
						 X,Y+Height),
      {[NItem|NChildren],LastX,LastY}
  end;

propagate_children(_Type,[],_WinX,_WinY,X,Y) ->
    {[],X,Y}.

%% ____________________________________________________________________
%%
%%  propagate(Type,Item,WinX,WimY,X,Y)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

propagate(Type,Item,WinX,WinY,X,Y)->
  Id=Item#mwidget.key,
  Data=gs:read(Id,data),
  MData=get_menu_data(Data),
  Menu=MData#mitem.menu,
  NMData=MData#mitem{winx=WinX,winy=WinY},
  NData=insert_menu(Data,NMData),
  gs:config(Id,[{x,X},{y,Y},{data,NData}]),
  case Type of
    menubutton->
      Height=gs:read(Id,height),
      propagate_menu(Menu,WinX,WinY+Height);
    %% menuitem
    _->
      Width=gs:read(Id,width),
      propagate_menu(Menu,WinX+Width,WinY)
  end,
  #mwidget{x=X,y=Y,winx=WinX,winy=WinY,key=Id}.

propagate_menu(Menu,WinX,WinY)->
  case Menu of
    undefined->
      done;
    Menu->
      Data=gs:read(Menu,data),
      MData=get_menu_data(Data),
      Children=MData#mcont.children,
      NChildren=propagate_children(menuitem,Children,WinX,WinY,0,0),
      NMData=MData#mcont{children=NChildren},
      NData=insert_menu(Data,NMData),
      gs:config(Menu,[{x,WinX},{y,WinY},{data,NData}])
  end.

delete_menu(MData) when record(MData,mitem)->
  case MData#mitem.menu of
    undefined->
      done;
    Menu ->
      Data=gs:read(Menu,data),
      MMData=get_menu_data(Data),
      Children=MMData#mcont.children,
      delete_children(Children),
      gs:destroy(Menu)
  end.

delete_menu(Menu,MData)->
  Children=MData#mcont.children,
  delete_children(Children),
  gs:destroy(Menu).


delete_item(Id,MData)->
  gs:destroy(Id),
  delete_menu(MData).

delete_children([Item|Rest])->
  Id=Item#mwidget.key,
  Data=gs:read(Id,data),
  MData=get_menu_data(Data),
  delete_item(Id,MData);
delete_children([]) ->
  done.

%% ____________________________________________________________________
%%
%%  select(Widget,WidgetData)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

select(Gsb,Draw,Widget,WidgetData,WidgetId,ParentId,Info)->

  ParentData=gs:read(ParentId,data),
  PHeight=gs:read(ParentId,height),
  PWidth=gs:read(ParentId,width),
  
  Width=case WidgetData#widget.gstype of
	  menubar->
	    DWidth=PWidth-gs:read(WidgetId,width),
	    MData=get_menu_data(WidgetData),
	    NSizeLeft=MData#mcont.sizeleft+DWidth,
	    NMData=MData#mcont{sizeleft=NSizeLeft},
	    NData=insert_menu(WidgetData,NMData),
	    gs:config(WidgetId,[{data,NData},{width,PWidth}]),
	    PWidth;
	  _ ->
	    gs:read(WidgetId,width)
	end,
  	  
  Height=gs:read(WidgetId,height),
  X=gs:read(WidgetId,x),
  Y=gs:read(WidgetId,y),
  Border=3,

  NX=X, NY=Y,SX=X,
  SY=Y+Height-Border,
  EX=X+Width-Border,
  EY=Y,WX=X,WY=Y,
  
  gs:create(frame,north,ParentId,
	    [{x,NX},{y,NY},{width,Width},{height,Border},{bg,yellow},
	     {buttonpress,true},{enter,true},{leave,true}]),
  
  
  gs:create(frame,south,ParentId,
	    [{x,SX},{y,SY},{width,Width},{height,Border},
	     {bg,yellow},{buttonpress,true},{enter,true},{leave,true}]),
	  		    
	     
  gs:create(frame,west,ParentId,
	    [{x,WX},{y,WY},{width,Border},{height,Height},{bg,yellow},
	     {buttonpress,true},{enter,true},{leave,true}]),
    
  gs:create(frame,east,ParentId,
	    [{x,EX},{y,EY},{width,Border},{height,Height},
	     {bg,yellow},{buttonpress,true},{enter,true},{leave,true}]),
  
  Select=#select{nx=NX,ny=NY,sx=SX,sy=SY,ex=EX,ey=EY,wx=WX,wy=WY,
		 height=Height,width=Width,border=Border},
  
  select_loop(Gsb,Draw,Widget,WidgetId,PHeight,PWidth,Select).
  

%% ____________________________________________________________________
%%
%%  select_loop(Gsb,Widget,WidgetId,ParentHeight,ParentWidth,Grid,Select)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

select_loop(Gsb,Draw,Widget,Id,PHeight,PWidth,Select)->
  receive
    exit->
      %% remove the lock in case it's on.
      Draw ! {draw,click},
      %%gs:config(Id,{cursor,Cursor}),
      
      %% these frames have the same father as the soposed selected widget
      %% but if the widget  is a window the window is the father. If the
      %% window is deleted the also die
      case gs:read(Id,id) of
	{error,{no_such_object,Id}}->
	  done;
	Other ->
	  gs:destroy(north),
	  gs:destroy(south),
	  gs:destroy(east),
	  gs:destroy(west)
      end;

    done->
      select_loop(Gsb,Draw,Widget,Id,PHeight,PWidth,Select);
    
    {selected_again}->
      Data=gs:read(Id,data),
      case Data#widget.gstype of
	menu->
	  select_loop(Gsb,Draw,Widget,Id,PHeight,PWidth,Select);
	menubar->
	  select_loop(Gsb,Draw,Widget,Id,PHeight,PWidth,Select);
	Other->
	  toggle_state(Id,Data),
	  select_loop(Gsb,Draw,Widget,Id,PHeight,PWidth,Select)
      end;
	
    {move,DX,DY}->
      select_loop(Gsb,Draw,Widget,Id,PHeight,PWidth,Select);
	
    {config,Config}->
      gs:config(Id,Config),
      gsb:config_widgets(Gsb,[Widget],props,Config),
      select_loop(Gsb,Draw,Widget,Id,PHeight,PWidth,Select);
        
    {gs,Dir,enter,Data,Other}->
      Draw ! {draw,no_click},
      select_loop(Gsb,Draw,Widget,Id,PHeight,PWidth,Select);

    {gs,Dir,leave,Data,Other}->
      Draw ! {draw,click},
      select_loop(Gsb,Draw,Widget,Id,PHeight,PWidth,Select);
        
    
    {gs,Dir,buttonpress,Data,[1,X,Y|_]}->
      select_loop(Gsb,Draw,Widget,Id,PHeight,PWidth,Select)

  end.


%% ____________________________________________________________________
%%
%%  create_standard_popup(Parent)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

create_popup(Parent,Clipboard,Selected)->
  
  M=gs:create(menu,Parent,[]),
  
  gs:create(menuitem, M, [{label,{text, "Cut"}},{data,cut}]),
  gs:create(menuitem, M, [{label,{text, "Copy"}},{data,copy}]),
  gs:create(menuitem, M, [{label,{text, "Delete"}},{data,delete}]),

  if(Clipboard == empty)->
      gs:create(menuitem,M,[{label,{text, "Paste"}},{enable,false}]);
    true->
      gs:create(menuitem,M, [{label,{text, "Paste"}},{data,paste}])
  end,
  
  {M,[cut,copy,paste,delete]}.

%% ____________________________________________________________________
%%
%%  handle_menu(Command,Info)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

handle_menu(Command,Info)->
  case Command of
    copy->
      gsb:copy_selected(Info#draw.gsb);
    cut->
      gsb:cut_selected(Info#draw.gsb);
    paste->
      gsb:paste_selected(Info#draw.gsb);
    delete->
      gsb:delete_selected(Info#draw.gsb);
    cancel->
      done
  end,
  done.
  

