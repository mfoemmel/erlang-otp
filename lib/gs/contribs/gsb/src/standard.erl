%% Copyright (c) 1996 by Fredrik Ström & Peter Molin.All Rights Reserved 
%% Time-stamp: <>
%% ====================================================================
%%  Filename : 	standard.erl
%%  Module   :	standard
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 1996-09-09 Fredrik Strom EX (fredriks@erlang.ericsson.se): Created.
%% ====================================================================
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
%% paste(Widget,WidgeData,X,Y,Info)-> NInfo
%% Widget - the new parent to the widgets in WidgetList.
%% WidgetData - implementation stuff.
%% Info - the state in the draw loop.
%% ------------------------------------------
%% select_widget(Widget,WidgetId,WidgetData,ParentId,Info)-> {WidgetKey,Pid}.
%% Widget - the widget to select
%% WidgetData - implementation stuff.
%% Pid a pointer to a process that handle movement and resizeing. class
%% dependent.
%% ------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(standard).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/3 $').
-revision('$Revision: 1.29 $').

-export([create_widget/3,create_widget/6,delete_widget/2,config_widget/3,
	 select_widget/6,paste_widget/6,select/7,popup_menu/6,popup/5,
	 remove_data_prop/1]).

-include("draw.hrl").
-include("standard.hrl").
-include("gsb.hrl").

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
  NProps=remove_data_prop(Props),
  gs:create(GsType,Key,Parent,NProps),
  %% windows and frames have a grid, and must be created separately.
  case GsType of
    window ->
      %% top windows use the global grid size, but
      %% other frames and windows use the same grid size as
      %% their parent.
      Grid=case gsb:is_top(Info#draw.gsb,Parent) of
	     true->
	       TGrid=#grid{snap=true,width=?GRID_SIZE,height=?GRID_SIZE},
	       create_grid(Key,TGrid);
	     false->
	       %% get parent grid size.
	       PGrid=get_grid_data(Parent),
	       create_grid(Key,PGrid)
	   end,
      
      Data=#widget{class=Class,parent=Parent,type=Type,
		   gstype=GsType,softparent=SoftParent},
      NData=insert_grid(Data,Grid),
      Events=[{buttonpress,true},{keypress,true},{buttonrelease,true},
	      {keyrelease,true},{data,NData},{configure,true},
	      {cursor,?MAIN_CURSOR}],
      gs:config(Key,Events),
      NWindows=[Key|Info#draw.windows],
      Info#draw{windows=NWindows};

    
    frame ->
      %% get parent grid size.
      PGrid=get_grid_data(Parent),
      Grid=create_grid(Key,PGrid),
      Data=#widget{class=Class,parent=Parent,type=Type,gstype=GsType,
		   softparent=SoftParent},
      NData=insert_grid(Data,Grid),
      Events=[{buttonpress,true},{bw,1},{relief,raised},
	      {data,NData},{cursor,?MAIN_CURSOR}],
      gs:config(Key,Events),
      Info;

    Other->
      Data=#widget{class=Class,parent=Parent,type=Type,
		   gstype=GsType,softparent=SoftParent},
      Events=[{buttonpress,true},{cursor,?MAIN_CURSOR},{data,Data}],
      gs:config(Key,Events),
      Info
  
  end.
  
%% ____________________________________________________________________
%%
%%  paste(Widget,Parent,Data,X,Y,Info)    
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
  spawn(standard,select,[Info#draw.gsb,Draw,Widget,WidgetData,
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
    %% the widget is aready destroyed.
    {error,{no_such_object,_Id}}->
      done;
    _->
      gs:destroy(Widget)
  end,
  %% windows shold be removed from the draw State.
  %% comment: lists:delete(foo,[bar])->[bar].
  NWindows=lists:delete(Widget,Info#draw.windows),
  Info#draw{windows=NWindows}.

%% ____________________________________________________________________
%%
%%  config_widget(Widget,Config)    
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
  case WidgetData#widget.gstype of
    window->
      container_popup(Widget,WidgetData,X,Y,Info);
    frame ->
      container_popup(Win,WidgetData,X,Y,Info);
    Other->
      standard_popup(Win,WidgetData,X,Y,Info)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ____________________________________________________________________
%%
%%  create_widget(Type,WidgetType,Parent,ParentData,X,Y,Info)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

%% FIXA att rätt pappa kollas.
create_widget(Type,Widget,Parent,ParentData,X,Y,Info)->
  ParentType=ParentData#widget.gstype,
  case allowed_parent(Widget,ParentType) of
    true->
      Grid=get_grid_data(ParentData),
      GX=snap_to_grid(X,x,Grid),
      GY=snap_to_grid(Y,y,Grid),
      case Type of
	create->
	  gsb:create_widget(Info#draw.gsb,Widget,Parent,[{x,GX},{y,GY}]);
	copy->
	  gsb:copy_widget(Info#draw.gsb,Widget,Parent,[{x,GX},{y,GY}])
      end;
    
    false ->
      messages:debug(":~p is not an allowed parent to:~p",[Parent,Widget],
		     standard,standard)
  end,
  Info.


%% ____________________________________________________________________
%%
%%  remove_data_prop(Props)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

remove_data_prop(Props)->
  remove_data_prop(Props,[]).

remove_data_prop([{data,Value}|Rest],Acc)->
  remove_data_prop(Rest,Acc);
remove_data_prop([Other|Rest],Acc) ->
  remove_data_prop(Rest,[Other|Acc]);
remove_data_prop([],Acc) ->
  Acc.

%% ____________________________________________________________________
%%
%%  create_container_popup(Parent)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

create_container_popup(Parent,Clipboard,Selected)->
  {SPopup,SItems}=create_standard_popup(Parent,Clipboard,Selected),

  MI=gs:create(menuitem,SPopup,[{label,{text,grid}},{itemtype,cascade}]),
  M=gs:create(menu,MI,[]),
  gs:create(menuitem,M,[{label,{text,"Snap"}},{data,snap}]),
  gs:create(menuitem,M,[{label,{text,"Dont Snap"}},{data,dont_snap}]),
  {SPopup,lists:append([snap,dont_snap],SItems)}.


%% ____________________________________________________________________
%%
%%  create_standard_popup(Parent)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

create_standard_popup(Parent,Clipboard,Selected)->
  
  M=gs:create(menu,Parent,[]),
  
  gs:create(menuitem, M, [{label,{text, "Cut"}},{data,cut}]),
  gs:create(menuitem, M, [{label,{text, "Copy"}},{data,copy}]),
  gs:create(menuitem, M, [{label,{text, "Delete"}},{data,delete}]),

  if(Clipboard == empty)->
      gs:create(menuitem,M,[{label,{text, "Paste"}},{enable,false}]);
    true->
      gs:create(menuitem,M, [{label,{text, "Paste"}},{data,paste}])
  end,
  
  gs:create(menuitem, M, [{itemtype,separator}]),

  MI=gs:create(menuitem,M, [{label,{text,"Center"}},{itemtype,cascade}]),
  M2=gs:create(menu,MI,[]),  
  
  gs:create(menuitem,M2,[{label,{text, "Center X"}},{data,center_x}]),
  gs:create(menuitem,M2,[{label,{text, "Center Y"}},{data,center_y}]),
  
  if (Selected < 2)->
      gs:create(menuitem,M,[{enable,false},{label,{text,"Align"}}]),
      gs:create(menuitem,M,[{enable,false},
			    {label,{text, "Shrink to smallest"}}]),
      gs:create(menuitem,M,[{enable,false},{label,{text, "Grow to Biggest"}}]),
      {M,[cut,copy,paste,delete,center_x,center_y]};
     
     true->
      MI2=gs:create(menuitem,M, [{label,{text,"Align"}},{itemtype,cascade}]),
      M3=gs:create(menu,MI2,[]),  
      
      gs:create(menuitem,M3,[{label,{text, "Align Left"}},{data,left}]),
      gs:create(menuitem,M3,[{label,{text, "Align Right"}},{data,right}]),
      gs:create(menuitem,M3,[{label,{text, "Align Up"}},{data,up}]),
      gs:create(menuitem,M3,[{label,{text, "Align Down"}},{data,down}]),
      
      MI3=gs:create(menuitem,M, [{label,{text, "Shrink to smallest"}},
				 {itemtype,cascade}]),
      M4=gs:create(menu,MI3,[]),  
      gs:create(menuitem,M4,[{label,{text, "Height"}},{data,smallest_height}]),
      gs:create(menuitem,M4,[{label,{text, "Width"}},{data,smallest_width}]),
      
      MI4=gs:create(menuitem,M,[{label,{text, "Grow to Biggest"}},
				{itemtype,cascade}]),
      M5=gs:create(menu,MI4,[]),  
      gs:create(menuitem,M5,[{label,{text, "Height"}},{data,biggest_height}]),
      gs:create(menuitem,M5,[{label,{text, "Width"}},{data,biggest_width}]),
  
      {M,[smallest_height,smallest_width,biggest_width,biggest_height,
	  cut,copy,paste,delete,left,right,up,down,center_x,center_y]}
  end.

%% ____________________________________________________________________
%%
%%  container_popup(Widget,WidgetData,X,Y,Info)
%%  Args    :
%%  Returns :
%%  Comments: a 
%% ____________________________________________________________________

container_popup(Widget,WidgetData,X,Y,Info)->
  Selected=length(Info#draw.selected),
  Clipboard=gsb:clipboard_status(Info#draw.gsb),
  %% create each time to get the right father.
  {CPopup,CItems}=create_container_popup(Widget,Clipboard,Selected),
  case standard(popup(CPopup,CItems,X,Y,Info),Info) of
    done->
      done;
    snap ->
      set_gridstate(Widget,WidgetData,true);
    dont_snap ->
      set_gridstate(Widget,WidgetData,false)
  end,
  gs:destroy(CPopup),
  Info.

%% ____________________________________________________________________
%%
%%  standard_popup(Widget,WidgetData,X,Y,Info)
%%  Args    :
%%  Returns :
%%  Comments:  
%% ____________________________________________________________________

standard_popup(Win,WidgetData,X,Y,Info)->
  Selected=length(Info#draw.selected),
  Clipboard=gsb:clipboard_status(Info#draw.gsb),
  %% create each time to get the right father.
  %% use win as father because button osv can be a father to a menu.
  {SPopup,SItems}=create_standard_popup(Win,Clipboard,Selected),
  standard(popup(SPopup,SItems,X,Y,Info),Info),
  gs:destroy(SPopup),
  Info.


%% ____________________________________________________________________
%%
%%  standard(Command,Gsb)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

standard(Command,Info)->
  case Command of
    copy->
      gsb:copy_selected(Info#draw.gsb),
      done;
    cut->
      gsb:cut_selected(Info#draw.gsb),
      done;
    paste->
      gsb:paste_selected(Info#draw.gsb),
      done;
    delete->
      gsb:delete_selected(Info#draw.gsb),
      done;
    cancel->
      done;
    center_x->
      center_selected(Info#draw.selected,x,self(),standard,Info);
    center_y->
      center_selected(Info#draw.selected,y,self(),standard,Info);
    %% handle align functionality.
    Other->
      align_selected(Other,Info#draw.selected,self(),standard)
  end.



%% ____________________________________________________________________
%%
%%  popup(Widget,Items,X,Y)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

popup(Menu,Items,X,Y,Info)->
  gs:config(Menu,{post_at,{X,Y}}),
  receive
    {gs,Item,click,Data,Other}->
      case lists:member(Data,Items) of
	true-> Data;
	false->
	  fix_event_order({gs,Item,click,Data,Other},Info),
	  cancel
      end;
    {gs,Widget,buttonpress,Data,Other} ->
      fix_event_order({gs,Widget,buttonpress,Data,Other},Info),
      cancel
  end.



%% ____________________________________________________________________
%%
%%  align_selected(Type,Parent,Selected)    
%%  Args    :
%%  Returns :
%%  Comments: Handle align commands and return done. Otherwise return
%%            Other.
%% ____________________________________________________________________

align_selected(Dir,Selected,Draw,Class)->
  case Dir of
    left->
      {NSelected,Config}=min(Selected,x,Class),
      draw:config_widgets(Draw,NSelected,props,Config);
    right->
      {NSelected,Config}=max(Selected,x,Class),
      draw:config_widgets(Draw,NSelected,props,Config);
    up->
      {NSelected,Config}=min(Selected,y,Class),
      draw:config_widgets(Draw,NSelected,props,Config);
    down->
      {NSelected,Config}=max(Selected,y,Class),
      draw:config_widgets(Draw,NSelected,props,Config);
    smallest_height ->
      {NSelected,Config}=min(Selected,height,Class),
      draw:config_widgets(Draw,NSelected,props,Config);
    biggest_height ->
      {NSelected,Config}=max(Selected,height,Class),
      draw:config_widgets(Draw,NSelected,props,Config);
    smallest_width ->
      {NSelected,Config}=min(Selected,width,Class),
      draw:config_widgets(Draw,NSelected,props,Config);
    biggest_width ->
      {NSelected,Config}=max(Selected,width,Class),
      draw:config_widgets(Draw,NSelected,props,Config);
    %% not a standard feature.
    Other ->Other
  end.

%% ____________________________________________________________________
%%
%%  max(Selected,Option,Class)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

max(Selected,Option,Class)->
  max(Selected,Option,Class,?MIN,[]).

max([{Widget,Pid,Class}|Rest],Option,Class,Biggest,Selected)->
  Value=gs:read(Widget,Option),
  if(Value > Biggest) ->
      max(Rest,Option,Class,Value,[{Widget,Pid,Class}|Selected]);
    true ->max(Rest,Option,Class,Biggest,[{Widget,Pid,Class}|Selected])
  end;

max([{Widget,Pid,_Class}|Rest],Option,Class,Biggest,Selected)->
  max(Rest,Option,Class,Biggest,Selected);

max([],Option,_,Biggest,Selected) ->
  {Selected,{Option,Biggest}}.


%% ____________________________________________________________________
%%
%%  min(Selection,Option,Class,Smallest)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

min(Selection,Option,Class)->
  min(Selection,Option,Class,?MAX,[]).
   
min([{Widget,Pid,Class}|Rest],Option,Class,Smallest,Selected)->
  Value=gs:read(Widget,Option),
  if(Value < Smallest) ->
      min(Rest,Option,Class,Value,[{Widget,Pid,Class}|Selected]);
    true ->min(Rest,Option,Class,Smallest,[{Widget,Pid,Class}|Selected])
  end;

min([{Widget,Pid,_Class}|Rest],Option,Class,Smallest,Selected)->
  min(Rest,Option,Class,Smallest,Selected);

min([],Option,_,Smallest,Selected) ->
  {Selected,{Option,Smallest}}.
  

%% ____________________________________________________________________
%%
%%  center_selected(Selected,Info)
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

center_selected([{Widget,Pid,Class}|Rest],Dir,Draw,Class,Info)->
  Data=gs:read(Widget,data),
  Parent=Data#widget.parent,
  Grid=get_grid_data(Data),
  case gsb:is_top(Info#draw.gsb,Parent) of  
    true->
      done;
    false ->
      case Dir of
	x->
	  PWidth=gs:read(Parent,width),
	  Width=gs:read(Widget,width),
	  Middle=PWidth div 2 - Width div 2,
	  GX=snap_to_grid(Middle,x,Grid),
	  draw:config_widgets(Draw,[{Widget,Pid,Class}],props,{x,GX});
	y ->
	  PHeight=gs:read(Parent,height),
	  Height=gs:read(Widget,height),
	  Middle=PHeight div 2 - Height div 2,
	  GY=snap_to_grid(Middle,y,Grid),
	  draw:config_widgets(Draw,[{Widget,Pid,Class}],props,{y,GY})
      end
  end,
  center_selected(Rest,Dir,Draw,Class,Info);

center_selected([{Widget,_Pid,Class}|Rest],Dir,Draw,Class,Info)->
  center_selected(Rest,Dir,Draw,Class,Info);

center_selected([],_Dir,_Draw,_Class,_Info) ->
  done.

%% ____________________________________________________________________
%%
%%  fix_event_order(Message,Info)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

fix_event_order({gs,Widget,Type,Data,Other},Info)->
  self() ! {gs,Widget,Type,Data,Other},
  
  case lists:member(Widget,Info#draw.windows) of
    false ->
      %% we want the order widget, window in draw.
      receive
	{gs,Window,Type,WData,WOther}->
	  self() ! {gs,Window,Type,WData,WOther}
      end;
    true ->
      done
  end.

%% ____________________________________________________________________
%%
%%  select(Widget,WidgetData)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

select(Gsb,Draw,Widget,WidgetData,Id,ParentId,Info)->
  %% the selection have the same parent as the widget to select so that
  %% the coords system is right, but with windows the windows coords
  %% system must be used( otherwise we dont se the selection box.
  Cursor=gs:read(Id,cursor),
  GSType=WidgetData#widget.gstype,
  {NMainCursor,ResizeCursor,Parent,X,Y}=
    case GSType of
      window-> {Cursor,Cursor,Id,0,0};
      _->{?MOVE_CURSOR,?RESIZE_CURSOR,ParentId,gs:read(Id,x),gs:read(Id,y)}
    end,
  
  gs:config(Id,{cursor,NMainCursor}),
  Width=gs:read(Id,width),
  Height=gs:read(Id,height),
  Border=3,
  
  ParentData=gs:read(Parent,data),
  Grid=get_grid_data(ParentData),
  PHeight=gs:read(Parent,height),
  PWidth=gs:read(Parent,width),
  
  NX=X, NY=Y,SX=X,
  SY=Y+Height-Border,
  EX=X+Width-Border,
  EY=Y,WX=X,WY=Y,
  
  gs:create(frame,north,Parent,
	    [{x,NX},{y,NY},{width,Width},{height,Border},{bg,yellow},
	     {buttonpress,true},{enter,true},{leave,true},
	     {cursor,ResizeCursor}]),
  
  
  gs:create(frame,south,Parent,
	    [{x,SX},{y,SY},{width,Width},{height,Border},
	     {bg,yellow},{buttonpress,true},{enter,true},{leave,true},
	     {cursor,ResizeCursor}]),
	  		    
	     
  gs:create(frame,west,Parent,
	    [{x,WX},{y,WY},{width,Border},{height,Height},{bg,yellow},
	     {buttonpress,true},{cursor,ResizeCursor},{enter,true},
	     {leave,true}]),
  
  
  gs:create(frame,east,Parent,
	    [{x,EX},{y,EY},{width,Border},{height,Height},
	     {bg,yellow},{buttonpress,true},{enter,true},{leave,true},
	     {cursor,ResizeCursor}]),
  
  Select=#select{nx=NX,ny=NY,sx=SX,sy=SY,ex=EX,ey=EY,wx=WX,wy=WY,
		 height=Height,width=Width,border=Border},
  
  select_loop(Gsb,Draw,Widget,Id,GSType,PHeight,PWidth,Grid,Select,Cursor).
  

%% ____________________________________________________________________
%%
%%  select_loop(Gsb,Widget,WidgetId,ParentHeight,ParentWidth,Grid,Select)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

select_loop(Gsb,Draw,Widget,Id,GSType,PHeight,PWidth,Grid,Select,Cursor)->
  receive
    exit->
      %% remove the lock in case it's on.
      Draw ! {draw,click},
      %% these frames have the same father as the soposed selected widget
      %% but if the widget  is a window the window is the father. If the
      %% window is deleted the also die
      case gs:read(Id,id) of
	{error,{no_such_object,Id}}->
	  done;
	Other ->
	  gs:config(Id,{cursor,Cursor}),
	  gs:destroy(north),
	  gs:destroy(south),
	  gs:destroy(east),
	  gs:destroy(west)
      end;

    {done,DX,DY}->
      case GSType of
	window->
	  select_loop(Gsb,Draw,Widget,Id,GSType,PHeight,PWidth,Grid,Select,
		      Cursor);
	_->
	  NSelect=move_selection(DX,DY,Select,PHeight,PWidth,Grid),
	  config_widget_to_selection(Gsb,Widget,Id,NSelect),
	  select_loop(Gsb,Draw,Widget,Id,GSType,PHeight,PWidth,Grid,NSelect,
		      Cursor)
      end;

    {selected_again}->
      select_loop(Gsb,Draw,Widget,Id,GSType,PHeight,PWidth,Grid,Select,
		  Cursor);

    %% special treatment for windows.
    {win_config,Width,Height}->
      %% change the size of the selectetion also.
      NSelect=config_window(Gsb,Widget,Id,Select,PHeight,PWidth,Grid,
			    Width,Height),
      select_loop(Gsb,Draw,Widget,Id,GSType,PHeight,PWidth,Grid,NSelect,
		  Cursor);
  
        
    {move,DX,DY}->
      case GSType of
	window->
	  select_loop(Gsb,Draw,Widget,Id,GSType,PHeight,PWidth,Grid,Select,
		    Cursor);
	_->
	  move_selection(DX,DY,Select,PHeight,PWidth,Grid),
	  select_loop(Gsb,Draw,Widget,Id,GSType,PHeight,PWidth,Grid,Select,
		      Cursor)
      end;
    
    {config,Config}->
      case GSType of
	window->
	  config_window(Gsb,Widget,Id,Config),
	  select_loop(Gsb,Draw,Widget,Id,GSType,PHeight,PWidth,Grid,Select,
		      Cursor);
	_->
	  case config(Gsb,Widget,Id,Config,Select,PHeight,PWidth,Grid) of
	    done->  
	      select_loop(Gsb,Draw,Widget,Id,GSType,PHeight,PWidth,Grid,
			  Select,Cursor);
	    NSelect->
	      config_widget_to_selection(Gsb,Widget,Id,NSelect),
	      select_loop(Gsb,Draw,Widget,Id,GSType,PHeight,PWidth,Grid,
			  NSelect,Cursor)
	  end
      end;
        
    {gs,Dir,enter,Data,Other}->
      Draw ! {draw,no_click},
      select_loop(Gsb,Draw,Widget,Id,GSType,PHeight,PWidth,Grid,Select,Cursor);

    {gs,Dir,leave,Data,Other}->
      Draw ! {draw,click},
      select_loop(Gsb,Draw,Widget,Id,GSType,PHeight,PWidth,Grid,Select,Cursor);
        
    
    {gs,Dir,buttonpress,Data,[1,X,Y|_]}->
      case GSType of
	window->
	  select_loop(Gsb,Draw,Widget,Id,GSType,PHeight,PWidth,Grid,Select,		      Cursor);
	_->
	  gs:config(Dir,{motion,true}),
	  gs:config(Dir,{buttonrelease,true}),
	  NSelect=resize(Dir,X,Y,PHeight,PWidth,Grid,Select),
	  config_widget_to_selection(Gsb,Widget,Id,NSelect),
	  select_loop(Gsb,Draw,Widget,Id,GSType,PHeight,PWidth,Grid,
		      NSelect,Cursor)
      end
  end.
  
  
%% ____________________________________________________________________
%%
%%  move_selection(DX,DY,Select,PHeight,PWidth,Grid)
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

move_selection(DX,DY,Select,PHeight,PWidth,Grid)->
  WHeight=Select#select.height,
  WWidth=Select#select.width,
  Border=Select#select.border,
  NNY=clip_to_box(Select#select.ny+DY,WHeight,PHeight),
  NNX=clip_to_box(Select#select.nx+DX,WWidth,PWidth),
  GNX=snap_to_grid(NNX,x,Grid),
  GNY=snap_to_grid(NNY,y,Grid),
  
  NSY=GNY+WHeight-Border,
  NEX=GNX+WWidth-Border,

  gs:config(north,[{x,GNX},{y,GNY}]),
  gs:config(south,[{x,GNX},{y,NSY}]),
  gs:config(east,[{x,NEX},{y,GNY}]),
  gs:config(west,[{x,GNX},{y,GNY}]),
  Select#select{nx=GNX,ny=GNY,sx=GNX,sy=NSY,ex=NEX,ey=GNY,wx=GNX,wy=GNY}.


%% ____________________________________________________________________
%%
%%  config_widget(Gsb,Widget,WidgetId,Select)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

config_widget_to_selection(Gsb,Widget,WidgetId,Select)->
  X=Select#select.nx,
  Y=Select#select.ny,
  Height=Select#select.height,
  Width=Select#select.width,
  gs:config(WidgetId,[{x,X},{y,Y},{height,Height},{width,Width}]),
  gsb:config_widgets(Gsb,[Widget],props,[{x,X},{y,Y},{height,Height},
				      {width,Width}]).

%% ____________________________________________________________________
%%
%%  config_selection(Option,    
%%  Args    :
%%  Returns :
%%  Comments: Windows dont have a position.
%% ____________________________________________________________________
config_window(Gsb,Widget,Id,{Option,Value})->
  case Option of
    x->
      done;
    y->
      done;
    width->
      gs:config(Id,{Option,Value});
    height->
      gs:config(Id,{Option,Value});    
    Other->
      gs:config(Id,{Option,Value}),
      gsb:config_widgets(Gsb,[Widget],props,{Option,Value})
  end.

config_window(Gsb,Widget,Id,Select,PHeight,PWidth,Grid,Width,Height)->
  NX=Select#select.nx,
  EX=NX+Width-Select#select.border,
  NY=Select#select.ny,
  SY=NY+Height-Select#select.border,

  gs:config(north,{width,Width}),
  gs:config(south,[{width,Width},{y,SY}]),
  gs:config(east,[{x,EX},{height,Height}]),
  gs:config(west,{height,Height}),
  
  gsb:config_widgets(Gsb,[Widget],props,[{width,Width},{height,Height}]),
  Select#select{width=Width,height=Height,ex=EX,sy=SY}.

config(Gsb,Widget,Id,{Option,Value},Select,PHeight,PWidth,Grid)->
  case Option of
    x->
      DX=Value-Select#select.nx,
      move_selection(DX,0,Select,PHeight,PWidth,Grid);
    y->
      DY=Value-Select#select.ny,
      move_selection(0,DY,Select,PHeight,PWidth,Grid);
    width->
      NX=Select#select.nx,
      EX=NX+Value-Select#select.border,
      gs:config(north,{width,Value}),
      gs:config(south,{width,Value}),
      gs:config(east,{x,EX}),
      Select#select{width=Value,ex=EX};
    height->
      NY=Select#select.ny,
      SY=NY+Value-Select#select.border,
      gs:config(east,{height,Value}),
      gs:config(west,{height,Value}),
      gs:config(south,{y,SY}),
      Select#select{height=Value,sy=SY};
    Other->
      gs:config(Id,{Option,Value}),
      gsb:config_widgets(Gsb,[Widget],props,{Option,Value}),
      done
  end.
     

%% ____________________________________________________________________
%%
%%  resize(Dir,X,Y,Select)
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

resize(north,ClickX,ClickY,PHeight,PWidth,Grid,Select)->
  receive
    {gs,north,buttonrelease,Data, [1,NX,NY|_]}->
      DY=get_delta(NY,ClickY),
      NSelect=handle_north_resize(Select,Grid,DY),
      gs:config(north,[{motion,false}]),
      NSelect;
    
    %% ____________________________________________________________________
    %% move the frame according to the realtive movement of the north
    %% resize handle.
    {gs,north,motion,Data,[X,Y|_]}->
      {NX,NY}=flush(north,X,Y),
      %% The relative movement.
      DY=get_delta(NY,ClickY),
      NSelect=handle_north_resize(Select,Grid,DY),
      resize(north,ClickX,ClickY,PHeight,PWidth,Grid,NSelect)
  end;

resize(south,ClickX,ClickY,PHeight,PWidth,Grid,Select)->
  receive
    {gs,south,buttonrelease,Data, [1,NX,NY|_]}->
      DY=get_delta(NY,ClickY),
      NSelect=handle_south_resize(Select,Grid,PHeight,DY),
      gs:config(south,[{motion,false}]),
      NSelect;
    
    %% ____________________________________________________________________
    %% move the frame according to the realtive movement of the north
    %% resize handle.
    {gs,south,motion,Data,[X,Y|_]}->
      {NX,NY}=flush(south,X,Y),
      %% The relative movement.
      DY=get_delta(NY,ClickY),
      NSelect=handle_south_resize(Select,Grid,PHeight,DY),
      resize(south,ClickX,ClickY,PHeight,PWidth,Grid,NSelect)
  end;

resize(east,ClickX,ClickY,PHeight,PWidth,Grid,Select)->
  receive
    {gs,east,buttonrelease,Data, [1,NX,NY|_]}->
      DX=get_delta(NX,ClickX),
      NSelect=handle_east_resize(Select,Grid,PWidth,DX),
      gs:config(east,[{motion,false}]),
      NSelect;
    
    %% ____________________________________________________________________
    %% move the frame according to the realtive movement of the north
    %% resize handle.
    {gs,east,motion,Data,[X,Y|_]}->
      {NX,NY}=flush(east,X,Y),
      %% The relative movement.
      DX=get_delta(NX,ClickX),
      NSelect=handle_east_resize(Select,Grid,PWidth,DX),
      resize(east,ClickX,ClickY,PHeight,PWidth,Grid,NSelect)
  end;

resize(west,ClickX,ClickY,PHeight,PWidth,Grid,Select)->
  receive
    {gs,west,buttonrelease,Data, [1,NX,NY|_]}->
      DX=get_delta(NX,ClickX),
      NSelect=handle_west_resize(Select,Grid,DX),
      gs:config(west,[{motion,false}]),
      NSelect;
    
    %% ____________________________________________________________________
    %% move the frame according to the realtive movement of the north
    %% resize handle.
    {gs,west,motion,Data,[X,Y|_]}->
      {NX,NY}=flush(west,X,Y),
      %% The relative movement.
      DX=get_delta(NX,ClickX),
      NSelect=handle_west_resize(Select,Grid,DX),
      resize(west,ClickX,ClickY,PHeight,PWidth,Grid,NSelect)
  end.



%% ____________________________________________________________________
%%
%%  get_delta(New,Click)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________
get_delta(New,Click) when New < 0->
  Delta=0-Click+New;

get_delta(New,Click) ->
  New-Click.

%% ____________________________________________________________________
%%
%%  handle_?_resize(Select,Grid,D?)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

handle_north_resize(Select,Grid,DY)->
  GNoY=snap_to_grid(clip_to_zero(Select#select.ny+DY),y,Grid),
  DY2=GNoY-Select#select.ny,
  %% new height 
  NewHeight=Select#select.height-DY2,
  %% update height and y position
  gs:config(north,{y,GNoY}),
  %% Update the other resize handles to the new height.
  NSY=GNoY+NewHeight-Select#select.border,
  gs:config(south,{y,NSY}),
  gs:config(east,[{y,GNoY},{height,NewHeight}]),
  gs:config(west,[{y,GNoY},{height,NewHeight}]),
  Select#select{height=NewHeight,ny=GNoY,ny=NSY}.


handle_south_resize(Select,Grid,PHeight,DY)->
  Border=Select#select.border,
  SouthY=Select#select.sy+Border,
  GSouthY=snap_to_grid(clip_to_value(SouthY+DY,PHeight),y,Grid),
  NSY=GSouthY-Border,
  DY2=NSY-Select#select.sy,
  %% new height 
  NewHeight=Select#select.height+DY2,
  gs:config(south,{y,NSY}),
  gs:config(east,{height,NewHeight}),
  gs:config(west,{height,NewHeight}),
  Select#select{height=NewHeight,sy=NSY}.

handle_east_resize(Select,Grid,PWidth,DX)->
  Border=Select#select.border,
  EastX=Select#select.ex+Border,
  CEastX=clip_to_value(EastX+DX,PWidth),
  GEX=snap_to_grid(CEastX-Border,x,Grid),
  DX2=GEX-Select#select.ex,
  %% new height 
  NewWidth=Select#select.width+DX2,
  %% update height and y position
  gs:config(north,[{width,NewWidth}]),
  %% Update the other resize handles to the new height.
  gs:config(south,[{width,NewWidth}]),
  gs:config(east,[{x,GEX}]),
  Select#select{width=NewWidth,ex=GEX}.

handle_west_resize(Select,Grid,DX)->
  GWX=snap_to_grid(clip_to_zero(Select#select.wx+DX),x,Grid),
  DX2=GWX-Select#select.wx,
  %% new height 
  NewWidth=Select#select.width-DX2,
  %% update height and y position
  gs:config(north,[{x,GWX},{width,NewWidth}]),
  %% Update the other resize handles to the new height.
  gs:config(south,[{x,GWX},{width,NewWidth}]),
  gs:config(east,[{x,GWX+NewWidth-Select#select.border}]),
  gs:config(west,[{x,GWX}]),
  Select#select{width=NewWidth,wx=GWX,nx=GWX,sx=GWX,
		ex=GWX+NewWidth-Select#select.border}.

%% ____________________________________________________________________
%%
%%  flush    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

flush(Widget,X,Y)->
  receive
    {gs,Widget,motion,_Data,[NX,NY]}->
      flush(Widget,NX,NY)
  after
    0->
      {X,Y}
  end.

%% ____________________________________________________________________
%%
%%  clip_to_box    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

clip_to_box(Val,Offset,Limit)->
  if(Val+Offset>Limit)->Limit-Offset;
    (Val<0) ->0;
    true -> Val
  end.

clip_to_zero(Val)->
  if(Val<0) -> 0;
    true -> Val
  end.

clip_to_value(Val,Limit)->
  if(Val>Limit)->Limit;
    true ->Val
  end.

%% ____________________________________________________________________
%%
%%  snap_to_grid(X,Y,Grid)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

snap_to_grid(Value,_Type,undefined)->
  Value;
snap_to_grid(Value,Type,Grid)->
  case Type of
    x->snap(Value,Grid#grid.width,Grid#grid.snap);
    y->snap(Value,Grid#grid.height,Grid#grid.snap)
  end.

snap(Value,Size,SnapState)->
  case SnapState of
    false-> Value;
    true ->
      VRest=Value rem Size,
      grid_round(VRest,Size,Value)
  end.

%% ____________________________________________________________________
%%
%%  grid_round(Rest,Grid,X)    
%%  Returns : the nearest grid coordinate.
%%  Args    :
%% ____________________________________________________________________

grid_round(Rest,Grid,Val)->
  if (Rest>Grid/2) -> Val+(Grid-Rest);
    true ->Val-Rest
  end.

%% ____________________________________________________________________
%%
%%  allowed_parent(Widget,Parent)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

allowed_parent(window,Parent)->
  case Parent of
    window->true;
    _->false
  end;

allowed_parent(Other,Parent)->
  case Parent of
    window->true;
    frame->true;
    _->false
  end.

%% ____________________________________________________________________
%%
%%  grid_management    
%% ____________________________________________________________________

%% ____________________________________________________________________
%%
%%  create_grid(Widget,Grid)    
%%  Returns :
%%  Args    : a grid record.
%% ____________________________________________________________________

create_grid(Widget,Grid)->
  Grid.

%% ____________________________________________________________________
%%
%%  get_grid_data(Parent).    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

get_grid_data(Data) when record(Data,widget)->
  Data#widget.class_data;

get_grid_data(Widget)->
  Data=gs:read(Widget,data),
  Data#widget.class_data.

%% ____________________________________________________________________
%%
%%  insert_grid(Data,Grid)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

insert_grid(Data,Grid)->
  Data#widget{class_data=Grid}.


%% ____________________________________________________________________
%%
%%  snap_to_grid(Widget,WidgetData)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

set_gridstate(Widget,WidgetData,State)->
  Grid=get_grid_data(WidgetData),
  NGrid=Grid#grid{snap=State},
  NData=insert_grid(WidgetData,NGrid),
  gs:config(Widget,{data,NData}).




 
  



