%% Copyright (c) 1996 by Fredrik Ström & Peter Molin.All Rights Reserved 
%% Time-stamp: <>
%% ====================================================================
%%  Filename : 	draw.erl
%%  Module   :	draw
%%  Purpose  :  the draw area.
%%  Notes    : 
%%  History  :	* 1996-08-27 Fredrik Strom EX (fredriks@erlang.ericsson.se):
%%                Created.
%% ====================================================================
%% Exported functions (short description):
%% ---------------------------------------
%% start(GsbPid) -> DrawPid.
%% - start the editor, and links to Gsb.
%% - GsbPid is the contact process.
%% ---------------------------------------
%% stop(DrawPid)-> true.
%% - stop the editor.
%% ---------------------------------------
%% select_widgets(DrawPid,[Key|Keys])
%% - select is a list of widgets that are selected in this draw window.
%% ---------------------------------------
%% delete_widget(DrawPid,Class,Widget)
%% - delete the widget.
%% ---------------------------------------
%% config_widget(DrawPid,Class,Widget,Type,Config)
%% - set the option of widget to value.
%% ---------------------------------------
%% create_widget(DrawPid,Parent,CRecord)-> WidgetKey
%% - create a widget from gsb, in our case the different draw windows
%%   can be created from menues in the mainwindow.
%% ---------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(draw).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/2 $').
-revision('$Revision: 1.29 $').

-export([start/1,draw/1,stop/1,select_widgets/2,
	 delete_widget/3,config_widget/4,
	 create_widget/4,config_widgets/4]).


%%
-include("draw.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exported functions                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(GsbPid)->
  spawn_link(draw,draw,[GsbPid]).

stop(DrawPid)->
  DrawPid ! {draw,stop},
  done.

select_widgets(DrawPid,Selected)->
  DrawPid ! {draw,{select,Selected}},
  done.

delete_widget(DrawPid,Widget,Class)->
  DrawPid ! {draw,delete,Widget,Class},
  done.

config_widget(DrawPid,Widget,props,Config)->
  DrawPid ! {draw,config,Widget,Config},
  done.

config_widgets(DrawPid,Widgets,props,Config)->
  DrawPid ! {draw,config_selected,Widgets,Config},
  done.

create_widget(DrawPid,Class,Parent,Record)->
  DrawPid ! {draw,create,Class,Parent,Record},
  done.

draw(GsbPid)->
  %% process state.
  Info=#draw{gsb=GsbPid,click=true},
  process_flag(trap_exit,true),
  draw_loop(Info).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ____________________________________________________________________
%%
%%  draw_loop(Info)    
%%  Args    : Info, a "main"record see draw.hrl
%%  Returns : nothing
%%  Comments: main process loop.
%% ____________________________________________________________________

draw_loop(Info)->
  receive
    %% ____________________________________________________________________
    %% Export part
    %% ____________________________________________________________________
    %%
    %% stop the circus nicely
    {draw,stop}->
      destroy_windows(Info#draw.windows),
      done;

    %% ____________________________________________________________________
    %% don;t care a bout clicks on the main window. Then a widget is selected
    %% separate process handle that widget, but a click is generated on the
    %% main window anyway. We don't want that click.
    {draw,no_click}->
      draw_loop(Info#draw{click=false});

    %% do care about the clicks
    {draw,click}->
      draw_loop(Info#draw{click=true});
    
    
    %% ____________________________________________________________________
    %% gsb send a list with one new element first in the selected list.
    
    {draw,{select,[Widget|Widgets]}} ->
      %% extact those widgets that should be unselected.
      {Selected,NotSelected}=extract_selected(Info#draw.selected,Widgets),
      %% unselect
      unselect_widget(NotSelected),
      Data=gs:read(Widget,data),
      NInfo=select_widget(Widget,Data,Info#draw{selected=Selected,shift=true}),
      draw_loop(NInfo#draw{shift=false});

    %% ____________________________________________________________________
    %% 
    {draw,delete,Widget,Class}->
      NInfo=apply(Class,delete_widget,[Widget,Info]),
      case member(Widget,NInfo#draw.selected) of
	{true,Pid,Class}->
	  unselect_widget({Widget,Pid,Class});
	false->done
      end,
      draw_loop(NInfo);
    
    %% ____________________________________________________________________
    %% 
    {draw,config,Widget,Config}->
      case member(Widget,Info#draw.selected) of
	{true,Pid,Class}->
	  broadcast({Widget,Pid,Class},{config,Config});
	false->
	  Data=gs:read(Widget,data),
	  Class=Data#widget.class,
	  apply(Class,config_widget,[Widget,Config,Info])
      end,
      draw_loop(Info);

    %% ____________________________________________________________________
    %% 
    {draw,config_selected,Selected,Config}->
      broadcast(Selected,{config,Config}),
      draw_loop(Info);
        
    %% ____________________________________________________________________
    %% 
    {draw,create,Class,Parent,Record}->
      NInfo=apply(Class,create_widget,[Record,Parent,Info]),
      draw_loop(NInfo);


    %% ____________________________________________________________________
    %% A window have been resized.
    {gs,Window,configure,_,[Width,Height,_X,_Y|_]}->
      case member(Window,Info#draw.selected) of
	{true,Pid,Class}->
	  broadcast({Window,Pid,Class},{win_config,Width,Height});
	%% otherwise we're done.
	false->
	  Data=gs:read(Window,data),
	  Class=Data#widget.class,
	  apply(Class,config_widget,[Window,{width,Width},Info]),
	  apply(Class,config_widget,[Window,{height,Height},Info])
      end,
      draw_loop(Info);

    %% ____________________________________________________________________
    %% shift is used to select several widgets at the same time.
    {gs,_Win,keypress,_,['Shift_L',_,_Controll|_]}->
      draw_loop(Info#draw{shift=true});

    %% ____________________________________________________________________
    %% shift is used to select several widgets at the same time.
    {gs,_Win,keyrelease,_,['Shift_L',_,_Controll|_]}->
      draw_loop(Info#draw{shift=false});

    %% ____________________________________________________________________
    %% delete selected widgets.
    {gs,Widget,keypress,_,['Delete',_,_Controll|_]}->
      gsb:delete_selected(Info#draw.gsb),
      draw_loop(Info);
    
    %% ____________________________________________________________________
    %% don't clogg the mailbox.
    {gs,_Win,keyrelease,_,[_Key,_KeyCode,_Controll|_]}->
      draw_loop(Info);

    %% ____________________________________________________________________
    %% 

    {gs,Widget,buttonpress,WidgetData,[1, X, Y|_]} ->
      %% when a widget is clicked on a click is also generated from the window
      %% this event most be removed from the mailbox. 
      NInfo=case WidgetData#widget.gstype of
	      window->
		case Info#draw.click of
		  true->
		    handle_buttonpress(Widget,Widget,WidgetData,X,Y,X,Y,Info);
		  false->
		    Info
		end;
	      _ ->
		receive
		  %% remove the window event.
		  {gs,Window,buttonpress,WindowData,[1,WX,WY|_]}->
		    handle_buttonpress(Widget,Window,WidgetData,WX,WY,X,Y,Info)
		end
	    end,
      draw_loop(NInfo);

    %% ____________________________________________________________________
    %% 
    {gs,Widget,buttonpress,Data,[3, X, Y|_]} ->
      case Data#widget.gstype of
	window->
	  NInfo=apply(Data#widget.class,popup_menu,
		      [Widget,Widget,Data,X,Y,Info]),
	  draw_loop(NInfo);
	
	Other ->
	  receive
	    {gs,Win,buttonpress,Data2,[3,WX,WY|_]}->
	      NInfo=apply(Data#widget.class,popup_menu,
			  [Widget,Win,Data,WX,WY,Info]),
	      draw_loop(NInfo)
	  end
      end;
    
    %% ____________________________________________________________________
    %% don't mind the clicks from buttons and listboxes 
    {gs,_Widget,click,_Data,_Other}->
      draw_loop(Info);

    %% ____________________________________________________________________
    %% windows have buttonrelease, remove these event from the mailbox when we
    %% are ooutside the move loop.
    {gs,Widget,buttonrelease,Data,Other}->
      draw_loop(Info);

    %% ____________________________________________________________________
    %% handle a destroyed window.
    {gs,Widget,destroy,Data,Other}->
      gsb:delete_widget(Info#draw.gsb,Widget),
      draw_loop(Info);

    %% ____________________________________________________________________
    %% somebody died on us.
    {'EXIT',Process,Reason}->
      messages:debug("received exit with reason:~p",[Reason],draw,draw_loop),
      exit(killed)

  end.
    
%% ____________________________________________________________________
%%
%%  move_loop(Info)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________
    
move_loop(ClickX,ClickY,Window,Selected)->
  receive

    %% ____________________________________________________________________
    %% button release indicates that the movement is done.
    {gs,Window,buttonrelease,Data, [1,X,Y|_]}->
      %% tell the selection process to config the widget to the selection.
      DX=X - ClickX,
      DY=Y - ClickY,
      broadcast(Selected,{done,DX,DY}),
      %% no unnessesary events.
      gs:config(Window,[{motion,false}]),
      done;
        
    %% ____________________________________________________________________
    %% tell the selection how to move.
    {gs,Window,motion,_Data,[X,Y|_]}->
      {NX,NY}=flush(Window,X,Y),
      DX=NX - ClickX,
      DY=NY - ClickY,
      broadcast(Selected,{move,DX,DY}),
      move_loop(ClickX,ClickY,Window,Selected)
  end.

%% ____________________________________________________________________
%%
%%  flush(Widget,X,Y)    
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
%%  select_widget(Type,Widget,Info)    
%%  Args    : Type - single:select one and unselect the rest.
%%                 - mulitible: kepp the old ones and add the new.
%%  Returns : NInfo - a new state (selected is changed).
%%  Comments:
%% ____________________________________________________________________

select_widget(Widget,Data,Info)->
  case Data#widget.type of
    soft->
      Parent=Data#widget.softparent,
      %% the top in the softwidget?
      if
	(Parent == Widget)->
	  select_widget_type(Widget,Data,Info);
	%% no.
	true->
	  %% traverse the tree one step up.
	  PData=gs:read(Parent,data),
	  select_widget(Parent,PData,Info)
      end;
    hard->
      select_widget_type(Widget,Data,Info)
  end.


select_widget_type(Widget,Data,Info)->
  case member(Widget,Info#draw.selected) of
    {true,Pid,Class}->
      {already_selected,{Widget,Pid,Class}};
    false->
      case Info#draw.shift of
	%% select several.
	true->
	  select_widget(multiple,Widget,Data,Info);
	
	%% select one.
	false->
	  select_widget(single,Widget,Data,Info)
      end
  end.
  

select_widget(Type,Widget,Data,Info)->
  Parent=Data#widget.parent,
  ParentId=gs:read(Parent,id),
  WidgetId=gs:read(Widget,id),
  Class=Data#widget.class,
  %% call the right class module
  Pid=apply(Class,select_widget,[self(),Widget,WidgetId,Data,ParentId,Info]),
  Selected={Widget,Pid,Class},
  
  case Type of
    single->
      unselect_widget(Info#draw.selected),
      Info#draw{selected=[Selected]};
    
    multiple ->
      Info#draw{selected=[Selected|Info#draw.selected]}
  end.


%% ____________________________________________________________________
%%
%%  unselect_widget(WidgetList)    
%%  Args    : Selected   - [{WidgetKey,Pid}|Rest]
%%                       - Pid is a pointer to a process that handle
%%                         movement and resizeing.
%%  Returns : done.
%%  Comments:
%% ____________________________________________________________________

unselect_widget(Selected)->
  broadcast(Selected,exit).


%% ____________________________________________________________________
%%
%%  extrac_selected(Selected,NSelected,Selected,NotSelected)    
%%  Args    : Selected   - [{WidgetKey,Data,Pid}|Rest]
%%                       - Pid is a pointer to a process that handle
%%                         movement and resizeing.
%%            NewSelected - [WidgetKey|Rest]
%%  Returns : Two list: Selected and not selected.
%%  Comments: 
%%            
%% ____________________________________________________________________

extract_selected(Selected,[]) ->
  {[],Selected};

extract_selected(Selected,NewSelected) ->
  extract_selected(Selected,NewSelected,[],[]).

extract_selected([{Key,Pid,Class}|Rest],NSelected,Selected,NotSelected) ->
  case lists:member(Key,NSelected) of
    false->
      extract_selected(Rest,NSelected,Selected,[{Key,Pid,Class}|NotSelected]);
    true->
      extract_selected(Rest,NSelected,[{Key,Pid,Class}|Selected],NotSelected)
  end;
extract_selected([],_NSelected,Selected,NotSelected)->
  {Selected,NotSelected}.

%% ____________________________________________________________________
%%
%%  create_widget_list(Selected)    
%%  Args    :Selected   - [{WidgetKey,Pid,Class}|Rest]
%%                       - Pid is a pointer to a process that handle
%%                         movement and resizeing.w
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

%% must preserve the order of the selected elements
create_widget_list([{WidgetKey,Pid,Class}|Rest])->
  [WidgetKey|create_widget_list(Rest)];
   
create_widget_list([])->
  [].

%% ____________________________________________________________________
%%
%%  destroy_windows(List)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

destroy_windows([Window|Rest]) ->
  gs:destroy(Window),
  destroy_windows(Rest);

destroy_windows([])->
  done.  
%% ____________________________________________________________________
%%
%%  broadcast(PidList,Message)    
%%  Args    : PidList -[Pid|Rest] 
%%  Returns :done
%%  Comments:
%% ____________________________________________________________________

broadcast([{_Key,Pid,_Class}|Rest],Message)->
  Pid ! Message,
  broadcast(Rest,Message);

broadcast({_Key,Pid,_Class},Message)->
  Pid ! Message;

broadcast([],_Message)->
  done.
%% ____________________________________________________________________
%%
%%  paste(Elements,Widget,WidgetData,Info)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

paste([Widget|Rest],Parent,Data,X,Y,Info) ->
  Class=gsb:lookup_widget(Info#draw.gsb,Widget,class),
  NInfo=apply(Class,paste_widget,
	      [Widget,Parent,Data,X,Y,Info]),
  paste(Rest,Parent,Data,X+10,Y+10,NInfo);

paste([],_Parent,_Data,X,Y,Info)->
  Info.

%% ____________________________________________________________________
%%
%%  member(Selected,WidgetKey)    
%%  Args    :Selected   - [{WidgetKey,Pid,Class}|Rest]
%%                       - Pid is a pointer to a process that handle
%%                         movement and resizeing.
%%           WidgetKey - the key to search for.
%%  Returns : {true,Pid,Class}/false
%%  Comments:
%% ____________________________________________________________________


member(WidgetKey,[{WidgetKey,Pid,Class}|Rest]) -> {true,Pid,Class};

member(WidgetKey,[_Other|Rest]) ->
  member(WidgetKey,Rest);

member(_WidgetKey,[])-> false.
	  
%%% ____________________________________________________________________
%%%
%%%  handle_buttonpress(Widget,WidgetData,X,Y,Info)    
%%%  Args    :
%%%  Returns :
%%%  Comments:
%%% ____________________________________________________________________

handle_buttonpress(Widget,Win,Data,WinX,WinY,X,Y,Info)->
  case Data#widget.type of
    soft->
      select(Widget,Win,Data,WinX,WinY,Info);
    hard ->
      %% if we don't want to move the widget then what?
      case gsb:get_action(Info#draw.gsb) of
	none->
	  select(Widget,Win,Data,WinX,WinY,Info);
	
	Elements when list(Elements)->
	  NInfo=paste(Elements,Widget,Data,X,Y,Info);
	
	%% create a widget at this location
	%% we also want the gstype in case of a softwidget.
	{WidgetType,Class}->
	  apply(Class,create_widget,[WidgetType,Widget,Data,X,Y,Info])
      end
  end.


select(Widget,Win,Data,WinX,WinY,Info)->
  case select_widget(Widget,Data,Info) of
    {already_selected,Selected}->
      broadcast(Selected,{selected_again}),
      gs:config(Win,[{motion,true}]),
      move_loop(WinX,WinY,Win,Info#draw.selected),
      Info;
    NInfo ->
      WidgetList=create_widget_list(NInfo#draw.selected),
      gsb:select_widgets(NInfo#draw.gsb,WidgetList),
      NInfo
  end.

