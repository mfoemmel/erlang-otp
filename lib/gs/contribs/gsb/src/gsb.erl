%% Copyright (c) 1996 by Fredrik Ström & Peter Molin.All Rights Reserved 
%% Time-stamp: <>
%% ====================================================================
%%  Filename : 	gsb.erl
%%  Module   :	gsb
%%  Purpose  :  the module that connects the main window,draw, hierarchy,
%%               the toolbox, option and the dynamic database.
%%  Notes    : 
%%  History  :	* 1996-08-27 Fredrik Strom EX (fredriks@erlang.ericsson.se):
%%                Created.
%% ====================================================================
%% Exported functions (short description):
%% ---------------------------------------
%% start()
%% - start the whole system.
%% ---------------------------------------
%% select_widgets(GsbPid,[Key|Keys])
%% - tell draw and option the selected widgets.
%%   called from hierarchy.
%% ---------------------------------------
%% config_widgets(GsbPid,Type,[{Option.Value}|Rest]
%% - all widgets in the selected list is configured.
%%   called from option
%% ---------------------------------------
%% create_widget(GsbPid,Parent,Type,Props)->create_record
%% - Create an entry in the
%%   database, 
%%   called from draw.
%% ---------------------------------------
%% copy_widget(GsbPid,NewPatent,WidgetKey) ->create_record
%% - Create a copy of an entry in the database call draw with a create.
%%   called from draw.
%% ---------------------------------------
%% delete_selected(GsbPid)
%% - delete selected widgets in the database and tell draw and hierachy.
%% ---------------------------------------
%% delete_widget(GsbPid,Widget)
%% - delete selected widget
%% ---------------------------------------
%% save_as_template(GsbPid,WidgetKey,FileName)
%% - save the Widget to file. The widget can then be used in the toolbox.
%%   called from draw.
%% ---------------------------------------
%% get_new_key(GsbPid)->Key from toolbox.
%% command(Pid, Module, Function, Arg)
%% - make apply({Module, Function}, Arg)
%% ---------------------------------------
%% copy_to_clipboard(GsbPid)
%% - copy the widgets in selected to the clipboard
%% ---------------------------------------
%% cut_to_clipboard(GsbPid)
%% - copy the selected widgets to the clipboard and destroy the graphic
%%      representation in draw and hierarchy.
%% ---------------------------------------
%% get_action(GsbPid)
%% - get the next action. Where action is none/paste/{Widget,Class}
%% ---------------------------------------
%% set_action(GgsbPid,Action) where Action is none/paste/{Widget,Class}
%% ---------------------------------------
%% is_top_window(GsbPid,Window) ->true or false.
%% ---------------------------------------
%% set_label(GsbPid,String)
%% ---------------------------------------
%% NEW
%% lookup(GsbPid, Key, Type)
%% config(GsbPid, Key, Type, Data)
%% config_toolbox(GsbPid, Key, Type, Data)
%% select_toolbox(GsbPid, Data) 
%% clipboard_status(GsbPid)-> empty or full.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(gsb).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/4 $').
-revision('$Revision: 1.56 $').
%% External exports.
-export([start/0, stop/1, select_widgets/2,
	create_widget/4,copy_widget/4,lookup_widget/3,lookup_widget/2,
	save_as_template/2,command/2, create_window/1, receive_message/1,
	copy_selected/1,cut_selected/1,clipboard_status/1,
	set_action/2,get_action/1,is_top/2,paste_selected/1,
	select_toolbox/2, config_toolbox/4,delete_selected/1,delete_widget/2,
	full_path_of_gsb/0,set_label/2, get_new_key/2, config_widgets/4]).

%% gen_server callbacks.
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2]).

-include("gsb.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% Exported functions                                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 


start() ->
  case gen_server:start(gsb,[],[]) of
    {ok, Pid} -> Pid;
    {error, Reason} -> messages:error("~p", [Reason], start, gsb)
  end.

init([]) ->
  Gs=gs:start(),
  {MainWindow, MenuHeight}=create_window(Gs),
  Draw=draw:start(self()),
  ToolPid=toolbox:start(self(),MainWindow, MenuHeight),
  toolbox:read(ToolPid, ".toolbox"), 
  Db=gsb_db:start(self(), ToolPid),
  Hier=hierarchy:start(self(),MainWindow, Db, MenuHeight),
  gs:config(main_window, {map, true}),
  Option=option:start(self(), MainWindow, Db),
  option:read(Option, ".option"),
  Clipboard=#clip{},
  %% process_flag(trap_exit,true),
  {ok, #gsb{windows=[],tool=ToolPid,tool_file=".toolbox",win=MainWindow,
	    win_file="untitled.rc",draw=Draw,option=Option,hier=Hier,
	    db=Db, gs = Gs,action=none,selected=[],clipboard=Clipboard}}.



%% ____________________________________________________________________
%% calls

stop(Pid) ->
  gen_server:call(Pid, stop,inf).
lookup_widget(GsbPid,Widget,Type)->
  gen_server:call(GsbPid,{lookup_widget,Widget,Type},infinity).
lookup_widget(GsbPid, WidgetKey) ->
  gen_server:call(GsbPid,{lookup_widget,WidgetKey},infinity).
get_action(GsbPid)->
  gen_server:call(GsbPid, get_action,infinity).

is_top(GsbPid,Window)->
  gen_server:call(GsbPid, {is_top,Window},infinity).

command(GsbPid,{Mod,Func,Args})->
  gen_server:call(GsbPid,{command,Mod,Func,Args},infinity).
select_toolbox(GsbPid, Data) ->
  gen_server:call(GsbPid, {select_toolbox, Data},infinity). 
config_toolbox(GsbPid, Key, Type, Value) ->
  gen_server:call(GsbPid, {config_toolbox, Key, Type, Value},infinity).
get_new_key(GsbPid, GsType) ->
  gen_server:call(GsbPid, {get_new_key, GsType},infinity).

clipboard_status(GsbPid)->
  gen_server:call(GsbPid, {clipboard_status},infinity).


%% ____________________________________________________________________
%% casts.

set_label(GsbPid,String)->
  gen_server:cast(GsbPid,{label,String}).

select_widgets(GsbPid,Selected)->
  gen_server:cast(GsbPid,{select,self(),Selected}).

config_widgets(GsbPid,Widgets,Type,Config)->
  gen_server:cast(GsbPid,{config_widgets,self(),Widgets,Type,Config}).

create_widget(GsbPid,Type,Parent,Props)->
  gen_server:cast(GsbPid,{create,Type,Parent,Props}).

copy_widget(GsbPid,Widget,NewParent,Props)->
  gen_server:cast(GsbPid,{copy,Widget,NewParent,Props}).

cut_selected(GsbPid)->
  gen_server:cast(GsbPid,{cut_to_clip}).

copy_selected(GsbPid)->
  gen_server:cast(GsbPid,{copy_to_clip}).

delete_selected(GsbPid)->
  gen_server:cast(GsbPid,{delete}).

delete_widget(GsbPid,Widget)->
  gen_server:cast(GsbPid,{delete,Widget}).

paste_selected(GsbPid)->
  gen_server:cast(GsbPid, {set_action,paste}).

save_as_template(GsbPid,Widget)->
  gen_server:cast(GsbPid,{save_as_template,Widget}).

set_action(GsbPid,Action)->
  gen_server:cast(GsbPid, {set_action,Action}).


%% catch all
receive_message(Module) ->
    case ?DEBUG of
	true ->
	    receive
	      {Module, Message} ->
		    Message;
	      {'EXIT', Pid, Reason} ->
		exit(normal)
	    end;
	false ->
	    receive
		{Module, Message} ->
		    Message
	    after ?TIMEOUT ->
		    messages:error("Missing message", [],
				   gsb, receive_message)
	    end
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% Server CALLbacks exported functions                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

handle_call(get_action,_From,Info)->
  toolbox:unselect(Info#gsb.tool),
  case Info#gsb.action of
    paste->
      Clipboard=Info#gsb.clipboard,
      {reply,Clipboard#clip.elements,Info#gsb{action=none}};
    Other->
      {reply,Other,Info#gsb{action=none}}
  end;

%% ____________________________________________________________________
%%
%%  editing of toolbox    
%% ____________________________________________________________________

handle_call({config_toolbox, Key, Type, Data}, From, Info) ->
  R = toolbox:config_toolbox(Info#gsb.tool, Key, Type, Data),
  {reply, R, Info};
handle_call({select_toolbox, Data}, From, Info) ->
  R = option:select_toolbox(Info#gsb.option, Data),
  {reply, R, Info};
handle_call({get_new_key, GsType}, From, Info) ->
  R = toolbox:get_new_key(Info#gsb.tool, GsType),
  {reply, R, Info};

%% ____________________________________________________________________
%%
%%  is_top_window(Window)    
%%  Args    : Window
%%  Returns :true if the window is a "top window"
%%  Comments:
%% ____________________________________________________________________

handle_call({is_top,Window},_From,Info)->
  if
    (Window == Info#gsb.gs)->
      {reply,true,Info};
    true ->{reply,false,Info}
  end;

%% ____________________________________________________________________
%%
%%  lookup    
%% ____________________________________________________________________

handle_call({lookup_widget, Key},_From,Info)->
  Result = gsb_db:lookup(Info#gsb.db, Key),
  {reply,Result, Info};
handle_call({lookup_widget, Key, Type},_From,Info)->
  Result = gsb_db:lookup(Info#gsb.db, Key, Type),
  {reply,Result, Info};
handle_call({config_widget, Key, Type, Data},_From,Info)->
  Result = gsb_db:config(Info#gsb.db, Key, Type, Data),
  {reply,Result, Info};
%% ____________________________________________________________________
%%
%%  commands    
%% ____________________________________________________________________

handle_call({command, gsb_db, Func, Args},_From,Info)->
  NewArgs = [Info#gsb.db|Args],
  Result = apply(gsb_db, Func, NewArgs),
  {reply,Result, Info};

handle_call({command,Module,Func,Args},_From,Info)->
  {reply,apply(Module,Func,Args),Info};

%% ____________________________________________________________________
%%
%%  clipboard
%% ____________________________________________________________________

handle_call({clipboard_status},_From,Info)->
  Clipboard=Info#gsb.clipboard,
  Size=length(Clipboard#clip.elements),
  if(Size >0)-> {reply,full, Info};
    true ->{reply,empty, Info}
  end;

handle_call(Other, From, Info) ->
  messages:error("~p, ~p, ~p", [Other, From, Info], gsb, handle_call),
  {reply, ok, Info}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% Server CASTbacks exported functions                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

%% ____________________________________________________________________
%%
%%  selection    
%%  Comments:
%% ____________________________________________________________________
%% 
handle_cast({select,From,Selected},Info)->
  if
    %% from draw
    (From == Info#gsb.draw) ->
      %% tell hierachy.
      hierarchy:select_widgets(Info#gsb.hier,Selected);
    
    %% from hierarchy
    (From == Info#gsb.hier) ->
      %% tell draw.
      draw:select_widgets(Info#gsb.draw,Selected);
    
    true->
      messages:debug("selected from: ~p",[From], gsb, handle_call)
  end,
  %% tell option.
  option:select_widgets(Info#gsb.option,Selected),
  NInfo=Info#gsb{selected=Selected},
  {noreply,NInfo};


%% ____________________________________________________________________
%%
%%  config    
%% ____________________________________________________________________

handle_cast({config_widgets,From,Keys,Type,Config},Info)->
  if
    (From == Info#gsb.option)  ->     
      NInfo=config_private_widgets(Keys,Type,Config,Info),
      {noreply,NInfo};
    true->
      gsb_db:config_widgets(Info#gsb.db,Keys,Type,Config),
      Selected=Info#gsb.selected,
      option:select_widgets(Info#gsb.option,Selected),
      {noreply,Info}
  end;

%% ____________________________________________________________________
%%
%%  creation    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

handle_cast({create,Type,Parent,Props},Info)->
  %% OBS get a template from the toolbox db.
  TRecord=toolbox:create(Info#gsb.tool,Type),
  case is_top2(Parent,Info) of
    true->
      {File,CRecord}=gsb_db:create_top(Info#gsb.db,TRecord,Props),
      Key=CRecord#create_widget.key,
      NInfo=propagate_create(File,CRecord,Parent,Info),
      option:select_widgets(Info#gsb.option,[Key]),
      hierarchy:select_widgets(Info#gsb.hier,[Key]),
      draw:select_widgets(Info#gsb.draw,[Key]),
      {noreply,NInfo#gsb{selected=[Key]}};
    
    false->
      CRecord=gsb_db:create(Info#gsb.db,Parent,TRecord,Props),
      Key=CRecord#create_widget.key,
      NInfo=propagate_create(CRecord,Parent,hard,Key,Info),
      option:select_widgets(Info#gsb.option,[Key]),
      hierarchy:select_widgets(Info#gsb.hier,[Key]),
      draw:select_widgets(Info#gsb.draw,[Key]),
      {noreply,NInfo#gsb{selected=[Key]}}
  end;
 
%% ____________________________________________________________________
%%
%%  copying    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

handle_cast({copy,Widget,NewParent,Props},Info)->
  CRecord=gsb_db:copy(Info#gsb.db,Widget,NewParent,Props),
  Key=CRecord#create_widget.key,
  NInfo=propagate_create(CRecord,NewParent,hard,Key,Info),
  option:select_widgets(Info#gsb.option,[Key]),
  hierarchy:select_widgets(Info#gsb.hier,[CRecord#create_widget.key]),
  draw:select_widgets(Info#gsb.draw,[Key]),
  {noreply,NInfo#gsb{selected=[Key]}};

handle_cast({cut_to_clip},Info)->
  NInfo=cut_to_clip(Info),
  {noreply,NInfo};

handle_cast({copy_to_clip},Info)->
  NInfo=copy_to_clip(Info),
  {noreply,NInfo};

%% ____________________________________________________________________
%%
%%  delete widget    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

handle_cast({delete},Info)->
  Selected=Info#gsb.selected,
  NInfo=propagate_delete_children(Selected,Info,no),
  {noreply,NInfo#gsb{selected=[]}};

handle_cast({delete,Widget},Info)->
  %% yes indicates that the children should be remove from the
  %% selection list also. 
  NInfo=propagate_delete_children([Widget],Info,yes),
  {noreply,NInfo};


%% ____________________________________________________________________
%%
%%  actions    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________


handle_cast({set_action,Action},Info)->
  {noreply,Info#gsb{action=Action}};

handle_cast({label,String},Info)->
  gs:config(label,{label,{text,String}}),
  {noreply,Info};


%% ____________________________________________________________________
%%
%%  template()    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

handle_cast({save_as_template,Widget},Info)->
  case Info#gsb.selected of
    [Key] ->
      %% get the current dir
      {ok,Dir}=file:get_cwd(),
      case save_soft_dialog:start(Info#gsb.gs) of
	{ok, FileName, Module, Exports} ->
	  %% save the toolbox to file.
	  gsb_db:write_as_template(Info#gsb.tool,FileName, Key,
				   Module, Exports),
	  {noreply,Info};
	cancel ->
	  {noreply,Info}
      end;
    _ ->
      messages:warning(Info#gsb.gs, "Only one widget can be topwidget"),
      {noreply,Info}
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% Server callbacks graphical interface                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% ____________________________________________________________________
%% project management
handle_info({gs,new_project,click,Data,Other},Info) ->
  %% get a template from the toolbox db.
  TRecord=toolbox:create(Info#gsb.tool,window),
  {File,CRecord}=gsb_db:create_top(Info#gsb.db,TRecord,[]),
  Key=CRecord#create_widget.key,
  NInfo=propagate_create(File,CRecord,Info#gsb.gs,Info),
  option:select_widgets(Info#gsb.option,[Key]),
  hierarchy:select_widgets(Info#gsb.hier,[Key]),
  draw:select_widgets(Info#gsb.draw,[Key]),
  {noreply,NInfo#gsb{selected=[Key]}};  


handle_info({gs,open_project,click,Data,Other},Info) ->
  %% get the current dir
  {ok,Dir}=file:get_cwd(),
  case file_dialog:start(Dir,Info#gsb.win_file,load) of
    {ok, NPath, NName} ->
      NFilename = lists:append([NPath, NName]),
      %% create entrye in gsb_db
      CRecord=gsb_db:read(Info#gsb.db,NFilename),
      %% create the widget in the editor.
      Key=CRecord#create_widget.key,
      NInfo=propagate_create(NFilename,CRecord,Info#gsb.gs,Info),
      option:select_widgets(Info#gsb.option,[Key]),
      hierarchy:select_widgets(Info#gsb.hier,[Key]),
      draw:select_widgets(Info#gsb.draw,[Key]),
      {noreply,NInfo#gsb{selected=[Key]}};  
    cancel ->
      {noreply, Info}
  end;

handle_info({gs,exit,click,Data,Other},Info)->
  {stop,normal,Info};

%% ____________________________________________________________________
%% toolbox management

handle_info({gs,import_widget,click,Data,Other},Info)->
  %% get the current dir
  {ok,Dir}=file:get_cwd(),
  %% retreive several widgets from a file.
  case browse_dialog:start(Dir) of
    {NewPath, NewName, WidgetList} ->
      FileName= lists:append([NewPath, NewName]),
      %% add the widgets to the toolbox
      toolbox:add(Info#gsb.tool, FileName, WidgetList),
      {noreply, Info};
    cancel ->
      {noreply, Info}
  end;

handle_info({gs,save_toolbox,click,Data,Other},Info)->
  %% save the toolbox to file.
  toolbox:write(Info#gsb.tool,Info#gsb.tool_file),
  {noreply, Info};

handle_info({gs,save_toolbox_as,click,Data,Other},Info)->
  %% get the current dir
  {ok,Dir}=file:get_cwd(),
  case file_dialog:start(Dir,Info#gsb.tool_file, save) of
    {ok, NPath, NName} ->
      NFilename = lists:append([NPath, NName]),
      %% save the toolbox to file.
      toolbox:write(Info#gsb.tool,NFilename),
      {noreply, Info#gsb{tool_file=NFilename}};
    
    cancel ->
      {noreply, Info}
  end;

handle_info({gs, save_as_softwidget, click, Data, Other}, Info) ->
  %% get the current dir
    case Info#gsb.selected of
    [Key] ->
      %% get the current dir
      {ok,Dir}=file:get_cwd(),
      case save_soft_dialog:start(Info#gsb.gs) of
	{ok, NPath, FileName,Module, Exports} ->
	  gsb_db:write_as_template(Info#gsb.db,FileName, Key,
				   Module, Module, Exports),
	  {noreply,Info};
	cancel ->
	  {noreply,Info}
      end;
    _ ->
      messages:warning(Info#gsb.gs, "Only one widget can be topwidget"),
      {noreply,Info}
  end;

%% ____________________________________________________________________
%% cut/copy/paste and delete.
handle_info({gs,cut,click,Data,Other},Info) ->
  NInfo=cut_to_clip(Info),
  {noreply,NInfo};

handle_info({gs,copy,click,Data,Other},Info) ->
  NInfo=copy_to_clip(Info),
  {noreply,NInfo};

handle_info({gs,paste,click,Data,Other},Info) ->
  {noreply, Info#gsb{action=paste}};

handle_info({gs,delete,click,Data,Other},Info) ->
  Selected=Info#gsb.selected,
  NInfo=propagate_delete_children(Selected,Info,no),
  {noreply,NInfo#gsb{selected=[]}};

handle_info({gs,option,click, Data, Other}, Info) ->
  option:show(Info#gsb.option),
  {noreply, Info};
%% ____________________________________________________________________
%% Help
handle_info({gs,tutorial,click,Data,Other},Info) ->
  {noreply,Info};
handle_info({gs,about,click,Data,Other},Info) ->
  gsb_run:start(Info#gsb.gs, about),
  rec_about_loop(Info);

  

%% ____________________________________________________________________
%%
handle_info({gs,Button,click,Data,Other},Info) when record(Data,item) ->
  case Data#item.command of

    save ->
      case get_filename(Info#gsb.windows,Data#item.win) of
	false->
	  messages:debug("no window named ~p",[Data#item.win],
			 gsb,handle_info),
	  {noreply, Info};
	Filename->
	  gsb_db:write(Info#gsb.db,Data#item.win,Filename),
	  {noreply, Info}
      end;
    
    save_as->
      %% get the current dir
      {ok,Dir}=file:get_cwd(),
      case file_dialog:start(Dir,Info#gsb.win_file,save) of
	{ok,NPath,NName} ->
	  NFilename = lists:append([NPath, NName]),
	  gsb_db:write(Info#gsb.db,Data#item.win,NFilename),
	  NWindows=set_filename(Info#gsb.windows,Data#item.win,NFilename),
	  {noreply, Info#gsb{windows=NWindows}};
	
	cancel ->
	  {noreply, Info}
      end;
    
    save_all->
      save_all(Info#gsb.windows,Info#gsb.db);
    
    close->
      NInfo=propagate_delete_children([Data#item.win],Info,yes),
      {noreply, NInfo};
    
    show ->      
      draw:config_widget(Info#gsb.draw,Data#item.win,props,{map,true}),
      {noreply, Info}
  end;

%% ____________________________________________________________________
%% display text when the cursor is over a menubutton 
handle_info({gs,MenuButton,enter,Data,_Alist},Info)->
  gs:config(label,{label,{text,Data}}),
  {noreply, Info};

%% ____________________________________________________________________
%% set the helptext
handle_info({label,String},Info)->
  gs:config(label,{label,{text,String}}),
  {noreply, Info};

handle_info({'EXIT',Pid,Reason},Info)->
  {stop,normal,Info};

%% ____________________________________________________________________
%% dummy clause
handle_info(Other, Info) ->
  messages:debug("Unknown message: ~p", [Other], gsb, handle_info),
  {stop, normal, Info}.

terminate(Reason,Info)->
  draw:stop(Info#gsb.draw),
  gsb_db:stop(Info#gsb.db),
  option:stop(Info#gsb.option),
  hierarchy:stop(Info#gsb.hier),
  toolbox:stop(Info#gsb.tool),
  gs:destroy(Info#gsb.win),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% Internal functions                                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

%% ____________________________________________________________________
%%
%%  create_window(GsParent)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

create_window(GsParent)->
  gsb_run:start(GsParent, gsb),
  gs:config(main_window, {default, all, {font, {screen,12}}}),
  gs:create(menu,save_menu,save_project,[]),
  gs:create(menu,save_as_menu,save_project_as,[]),
  gs:create(menu,close_menu,close_project,[]),
  MainId=gs:read(main_window, id),
  MH = gs:read(bar, height),
  {MainId, MH}.

%% ____________________________________________________________________
%%
%%  propagate_create(File,CRecord,Parent,Info)
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

propagate_create(File,CRecord,Parent,Info)->
  Key=CRecord#create_widget.key,
  Name=CRecord#create_widget.name,
  Class=CRecord#create_widget.class,
  Children=CRecord#create_widget.children,
  Type=CRecord#create_widget.type,

  NRecord=CRecord#create_widget{children=undefined,softparent=Key},
  draw:create_widget(Info#gsb.draw,Class,Parent,NRecord),
  
  SRecord=#item{command=save,win=Key},
  Save=gs:create(menuitem,save_menu,
		 [{label,{text,Name}},{data,SRecord}]),
  ARecord=#item{command=save_as,win=Key},
  SaveAs=gs:create(menuitem,save_as_menu,
		   [{label,{text,Name}},{data,ARecord}]),
  ClRecord=#item{command=close,win=Key},
  Close=gs:create(menuitem,close_menu,
		   [{label,{text,Name}},{data,ClRecord}]),
  
  WRecord=#item{command=show,win=Key},
  Show=gs:create(menuitem,window_menu,
		 [{label,{text,Name}},{data,WRecord}]),
  
  WinRec=#win{key=Key,name=Name,file=File,
	      gslist=[Save,SaveAs,Close,Show]},
  NWindows=[WinRec|Info#gsb.windows],
  hierarchy:add_top(Info#gsb.hier,Key,Name),
  NInfo=Info#gsb{windows=NWindows},
  propagate_create_children(Children,Key,Type,Key,NInfo).
  

propagate_create(CRecord,Parent,State,SoftParent,Info)->
  Key=CRecord#create_widget.key,
  Name=CRecord#create_widget.name,
  GsType=CRecord#create_widget.gs_type,
  Type=CRecord#create_widget.type,
  Class=CRecord#create_widget.class,
  Children=CRecord#create_widget.children,
  
  NInfo=case GsType of
	  window->
	    WRecord=#item{command=show,win=Key},
	    Show=gs:create(menuitem,window_menu,
			   [{label,{text,Name}},{data,WRecord}]),
	    
	    WinRec=#win{key=Key,name=Name,gslist=[Show]},
	    NWindows=[WinRec|Info#gsb.windows],
	    Info#gsb{windows=NWindows};
	  _->
	    Info
	end,

  case State of
    soft when (Type == soft)->      
      NRecord=CRecord#create_widget{children=undefined,softparent=SoftParent},
      draw:create_widget(Info#gsb.draw,Class,Parent,NRecord),
      propagate_create_children(Children,Key,Type,SoftParent,NInfo);
      
    Other->
      NRecord=CRecord#create_widget{children=undefined,softparent=Key},
      draw:create_widget(Info#gsb.draw,Class,Parent,NRecord),
      hierarchy:add(Info#gsb.hier,Parent,Key,Name),
      propagate_create_children(Children,Key,Type,Key,NInfo)
  end.

%% ____________________________________________________________________
%% 

propagate_create_children([CRecord|Rest],Parent,State,SoftParent,Info) ->
  NInfo=propagate_create(CRecord,Parent,State,SoftParent,Info),
  propagate_create_children(Rest,Parent,State,SoftParent,NInfo);

propagate_create_children([],_Parent,_State,_SoftParent,Info)->
  Info.

%% ____________________________________________________________________
%%
%%  propagate_delete(CRecord,Info)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

propagate_delete(GRecord,Info,State)->
  %% recurse down and remove the children first.
  Children=GRecord#gsb_db.children,
  NInfo=propagate_delete_children(Children,Info,State),

  %% then the widget.
  Key=GRecord#gsb_db.key,
  GsType=GRecord#gsb_db.gs_type,
  Class=GRecord#gsb_db.class,
  Clipboard=Info#gsb.clipboard,
  NElements=lists:delete(Key,Clipboard#clip.elements),
  NClipboard=Clipboard#clip{elements=NElements},
  NNInfo=case GsType of
	  window->
	    NWindows=delete_window(Key,NInfo#gsb.windows),
	    NInfo#gsb{windows=NWindows,clipboard=NClipboard};
	  _->
	    NInfo#gsb{clipboard=NClipboard}
	end,
  Selected=NNInfo#gsb.selected,
  NSelected=case State of
	      yes->lists:delete(Key,Selected);
	      no-> Selected
	    end,
  draw:delete_widget(NNInfo#gsb.draw,Key,Class),
  gsb_db:delete(NNInfo#gsb.db,Key),  
  hierarchy:delete(NNInfo#gsb.hier,Key),
  NNInfo#gsb{selected=NSelected}.

  

%% ____________________________________________________________________
%% 

propagate_delete_children([Widget|Rest],Info,State) ->
  GRecord=gsb_db:lookup(Info#gsb.db,Widget),
  %% remove the widget.
  NInfo=propagate_delete(GRecord,Info,State),
  %% remove the rest.
  propagate_delete_children(Rest,NInfo,State);

propagate_delete_children([],Info,_)->
  Info.

%% ____________________________________________________________________
%%
%%  cut_to_clipboard(Selected,Info)    
%%  Args    : Selected   - [{WidgetKey,Pid}|Rest]
%%                       - Pid is a pointer to a process that handle
%%                         movement and resizeing.
%%            Info - state in the draw loop.
%%  Returns : NInfo - new clipboard and new selected.
%%  Comments:
%% ____________________________________________________________________

cut_to_clip(Info)->
  Selected=Info#gsb.selected,
  Clipboard=Info#gsb.clipboard,
  NInfo=delete_graphics(Selected,Info),
  clear_clipboard(Clipboard,Info#gsb.db),
  option:unselect(Info#gsb.option),
  NClipboard=Clipboard#clip{elements=Selected,state=cut},
  NInfo#gsb{clipboard=NClipboard,selected=[]}.




%% ____________________________________________________________________
%%
%%  delete_graphics(Selected,Info)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

delete_graphics([Key|Rest],Info) ->
  %% kill the children first.
  Children=gsb_db:lookup(Info#gsb.db,Key,children),
  Class=gsb_db:lookup(Info#gsb.db,Key,class),
  NInfo=delete_graphics(Children,Info),
  GsType=gsb_db:lookup(Info#gsb.db,Key,gs_type),
  NNInfo=case GsType of
	   window->
	     NWindows=delete_window(Key,NInfo#gsb.windows),
	     NInfo#gsb{windows=NWindows};
	   _->
	     NInfo
	 end,
  %% then the daddy.
  draw:delete_widget(NNInfo#gsb.draw,Key,Class),  
  hierarchy:delete(NNInfo#gsb.hier,Key),
  %% and the rest.
  delete_graphics(Rest,Info);

delete_graphics([],Info)->
  Info. 
  
%% ____________________________________________________________________
%%
%%  copy_to_clip(Info)    
%%  Args    : Info - the state in the draw loop
%%  Returns : NInfo the new state.
%%  Comments:
%% ____________________________________________________________________

copy_to_clip(Info)->
  Selected=Info#gsb.selected,
  Clipboard=Info#gsb.clipboard,
  NClipboard=Clipboard#clip{elements=Selected},
  Info#gsb{clipboard=NClipboard}.


%% ____________________________________________________________________
%%
%%  clear_clipboard(Clipboard)    
%%  Args    : Clipboard  record av type clipboard.
%%  Returns : nothing
%%  Comments:
%% ____________________________________________________________________

clear_clipboard(Clipboard,Db)->
  case Clipboard#clip.state of
    cut -> clear(Clipboard#clip.elements,Db);
    _ ->done
  end.

clear([],_Db)->
  done;
clear([Widget|Rest],Db) ->
  gsb_db:delete(Db,Widget),
  clear(Rest,Db).

%% ____________________________________________________________________
%%
%%  get_filename(Windows,Win).    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

get_filename([Win|Rest],WinKey)->
  if
    (WinKey == Win#win.key)->
      Win#win.file;
    true->
      get_filename(Rest,WinKey)
  end;

get_filename([],WinKey)->
  false.

%% ____________________________________________________________________
%%
%%  set_filename(Windows,WinKey,File)    
%%  Args    : Windows- list of win records
%%            winKey the Window to change properties for.
%%  Returns : A new windowlist.
%%  Comments:
%% ____________________________________________________________________

set_filename(Windows,WinKey,File)->
  set_filename(Windows,WinKey,File,[]).

set_filename([],WinKey,_File,Extracted)->
  Extracted;

set_filename([Win|Rest],WinKey,File,Extracted)->
  if
    (WinKey == Win#win.key) ->
      NWin=Win#win{file=File},
      set_filename(Rest,WinKey,File,[NWin|Extracted]);
    true->
      set_filename(Rest,WinKey,File,[Win|Extracted])
  end.

%% ____________________________________________________________________
%%
%%  set_window_name(Windows,Selected,Config),
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

set_window_name(Windows,Selected,Name)->
  set_window_name(Windows,Selected,Name,[]).

set_window_name([Win|Rest],Selected,Name,Extracted)->
  case lists:member(Win#win.key,Selected) of
    true ->
      set_name(Win#win.gslist,Name),
      set_window_name(Rest,Selected,Name,[Win#win{name=Name}|Extracted]);
    false->
      set_window_name(Rest,Selected,Name,[Win|Extracted])  
  end;
set_window_name([],_Selected,_Name,Extracted) ->
  Extracted.

%% ____________________________________________________________________
%%
%%  set_name(Widgets,Name)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

set_name([Widget|Rest],Name)->
  gs:config(Widget,{label,{text,Name}}),
  set_name(Rest,Name);

set_name([],_Name) ->
  done.

%% ____________________________________________________________________
%%
%%  save_all(Windows).    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

save_all([],Db)->
  done;
save_all([Win|Rest],Db) ->
  gsb_db:write(Db,Win#win.key,Win#win.file),
  save_all(Rest,Db).

is_top2(Parent,Info)->
  case Info#gsb.gs of
   Parent->true;
    _ ->false
  end.
%% ____________________________________________________________________
%%
%%  delete_window(Windows,Widget)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

delete_window(Widget,Windows)->
  delete_windows(Widget,Windows,[]).

delete_windows(_Widget,[],Extracted)->
  Extracted;
delete_windows(Widget,[Win|Rest],Extacted) ->
  if
    (Widget == Win#win.key)->
      destroy(Win#win.gslist),
      delete_windows(Widget,Rest,Extacted);
    true->
      delete_windows(Widget,Rest,[Win|Extacted])
  end.


destroy([Widget|Rest]) ->
  gs:destroy(Widget),
  destroy(Rest);

destroy([])->
  done.

config_all_draw(Draw, [First|Rest], Config) ->
  draw:config_widget(Draw,First,props,Config),
  config_all_draw(Draw, Rest, Config);
config_all_draw(Draw, [], Config) -> true.

%% ____________________________________________________________________
%%
%%  config_private_widgets(Keys,Type,Config, Info)    
%%  Args    :
%%  Returns :
%%  Comments:
%% ____________________________________________________________________

config_private_widgets(Keys,Type,Config,Info) ->
  case Type of
    exports ->
      gsb_db:config_widgets(Info#gsb.db, Keys, Type, Config),
      Info;
    props->
      %% draw:config_widgets(Info#gsb.draw,Keys,props,Config),
      case Config of
	{name, Value} ->
	  case Keys of
	    [Key]->
	      hierarchy:config(Info#gsb.hier,Key,Value),
	      gsb_db:config_widgets(Info#gsb.db,Keys,name,Value),
	      Info#gsb{windows =
			set_window_name(Info#gsb.windows,
					Info#gsb.selected,Value)};
	    Other ->
	      messages:debug("Widget ~p names should be unique",[Other],
			     gsb,config_private_widgets),
	      Info
	  end;
	{data, Value} ->
	  gsb_db:config_widgets(Info#gsb.db,Keys, props, {data,Value});
	_ ->
	  config_all_draw(Info#gsb.draw, Keys, Config),
	  Info
      end;
    _ ->
      gsb_db:config_widgets(Info#gsb.db,Keys,Type,Config),
      Info
  end.

%% ____________________________________________________________________
%%
%%  full_path_of_gsb()    
%%  Args    :
%%  Returns :A directory of the current gsb
%%  Comment :Needed for network installations
%% ____________________________________________________________________

%% This is not an application so we don't have their way of knowing
%% a private data directory where the GIF files are located (this directory).
%% We can find GS and makes it relative from there /kgb

-define(EbinFromGsPriv,"../contribs/ebin").

full_path_of_gsb() ->
    GsPrivDir = code:priv_dir(gs),
    filename:join(GsPrivDir,?EbinFromGsPriv).

rec_about_loop(Info) ->
  receive
    {gs,cont,click,Data,Other} ->
      gs:destroy(about),
      {noreply,Info};
    Other -> rec_about_loop(Info)
  end.
