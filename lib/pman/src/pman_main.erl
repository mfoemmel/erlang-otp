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
%%%----------------------------------------------------------------------
%%% Purpose : Process code for the main process of PMAN
%%%----------------------------------------------------------------------

-module(pman_main).

-export([init/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Constants
%%
-include("pman_win.hrl").
-include("pman.hrl").


-define(REFRESH_TIME,5000).

-define(REQUIRES_FOCUS,				%List of menus that shall
	['Trace Process',			%be disabled if no process
	 'Kill',				%is in focus
	 'Hide Selected Process',
	 'Module']).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% init/2 - Initialize the environment, open the windows,
%%    and call the main loop.
%%
%% Arguments:
%%
%% PidCaller		The calling process
%% LIModuleExcluded	a list of module names
%%
%% Returns:
%%   Unspecified return value, this function should be spawnd.
%%
%% Exits:
%%   


init(PidCaller, OSModuleExcluded) ->

    Grid_size =  get_grid_size() + 1,


    process_flag(trap_exit, true),

    %% Monitor all nodes in the distributed system
    %% FIXME: This doesn`t handle the case when a nodes gets distributed
    %% FIXME: after PMAN is started.
    case is_alive() of

	%% We have a distributed system
	true ->
	    net_kernel:monitor_nodes(true);
	
	%% No distribution
	_Otherwise ->
	    ok
    end,
    LINode = get_nodes(),

    
    %% Create the main window
    {Window, Grid,Frame, Visible}  =
	pman_win:pman_window(Grid_size, OSModuleExcluded, LINode),

    Noshell = case pman_shell:find_shell() of
		  noshell -> true;
		  _ -> false
	      end,

    Pman_data = #gs_pman{win=Window, grid=Grid, frame=Frame,
			 size=Visible,
			 see=OSModuleExcluded,
			 hide_modules=OSModuleExcluded,
			 focus= 1,
			 node= node(),
			 noshell = Noshell,
			 nodes= LINode},

    Pman_data2 = refresh(Pman_data),


    New_Pman_data = restore_options(Pman_data2),

    
    %% Notify caller that the process appears
    %% to have been started.
    PidCaller ! {initialization_complete, self()},

    %% Initiate a 'catch all' trace pattern so call tracing works
    erlang:trace_pattern({'_', '_', '_'}, true, [local]),

    loop(New_Pman_data).




%% ---------------------------------------------------------------
%% In case we are limiting the view to just a number of 
%% different modules AND we have a very large set of processes
%% we don't make grids for all the procs we don't want to see
%%
%% Return: Number of visible processes + 60
%% ---------------------------------------------------------------

get_grid_size() ->
    get_grid_size(processes(), 60).

get_grid_size([], Ack) -> Ack;
get_grid_size([Process|LIProcess],  Ack) ->

    case (catch pman_process:pinfo_notag(Process,current_module)) of

	%% Dead or unreachable process, don't count it
	{'EXIT', _Reason} ->
	    get_grid_size(LIProcess, Ack);


	_Module ->
	    get_grid_size(LIProcess, Ack+1)
    end.




%% get_nodes/1 -  Returns a list of nodes in the system.
%% If not distributed, a list of the current node (nonode@nohost).
%%

get_nodes() ->
    [node()|nodes()].


%% ---------------------------------------------------------------
%% With respect to the node menu last time it was updated, remove
%% all nodes which have died, and add all the new ones.
%% Returns: #gs_pman
%% ---------------------------------------------------------------

update_nodes_menu(Pman_data) ->
    OSNodesNew = ordsets:from_list(get_nodes()),
    OSNodesOld = ordsets:from_list(Pman_data#gs_pman.nodes),

    OSNodesAdd = ordsets:subtract(OSNodesNew,OSNodesOld),
    OSNodesDelete = ordsets:subtract(OSNodesOld,OSNodesNew),
    
    pman_win:remove_menu(OSNodesDelete),
    pman_win:add_menu(node,OSNodesAdd,"Show"),



    Pman_data#gs_pman{nodes=OSNodesNew}.

%% ---------------------------------------------------------------
%% Focus is the highlited process in the grid. This set of 
%% functions keep track of it and handle changes to it, cheking
%% if the change is legitimate. It also retrives the data stored
%% for each process which is currently on focus.
%% ---------------------------------------------------------------

%% ---------------------------------------------------------------
%% When a new focus is given by a user, we need to check if we are
%% within the bounds of the grid, and if not, either choose the
%% header fields or the last available process.
%% Returnd: New position Int
%% ---------------------------------------------------------------

check_focus_choice(_To,      0) -> 1;
check_focus_choice(0,    Size) -> Size; 
check_focus_choice(To,   Size) -> 
    case Size+1 of
	To -> 1;
	_  -> To
    end.

%% ----------------------------------------------------------------
%% Return: Position of Element in List or false if it is not a member
%% ----------------------------------------------------------------

get_pos(_L, undefined) ->
    false;

get_pos(List, Element) ->
    get_pos(lists:reverse(List), Element,1).  % the list has to be reversed

get_pos([], _, _) -> 
    false;

get_pos([Element | _Rest], Element, Count) ->
    Count;

get_pos([_ | Rest], Element, Count) ->
    get_pos(Rest, Element, Count + 1).



%% ----------------------------------------------------------------
%% Return: Data associated with the Pid in focus: {pidfunc,Pid,Mod}
%% ----------------------------------------------------------------
%%
%% get_pid_in_focus/1 - Returns the data associated with the selected gridline.
%%
%% Arguments:
%%   Pman_data	The Pman_data state record
%%
%% Returns:
%%   false	If the focus is in the header
%%   Data	Data on the form {(???)} for the selected gridline.
%%
%%
get_pid_in_focus(Pman_data) ->
    Grid = Pman_data#gs_pman.grid,
    
     case Pman_data#gs_pman.focus of
	1   -> 
	    false;
	Row -> 
	     case gs:read(Grid, {obj_at_row, Row}) of
		 undefined ->
		     false;
		 GridLine ->
		     D = gs:read(GridLine,data),
		     {true, D}
	     end
     end.

%% ----------------------------------------------------------------
%% Upon refreshing pman, we choose the new focus in the following
%% order, as well as handling the menus which may be disabled
%% ----------------------------------------------------------------

%% set_focus/2 - Is called automatically after a refresh to set the focus
%%               to the correct process.
%%
%% It chooses from the following alternatives, in this order:
%% 1.The old Pid which was in focus, whatever the new pos is
%% 2.The same position as the old Pid which is now dead
%% 3.The last Pid in the grid.
%%
%% Returns:
%%  An updated Pman_data state record
%%


set_focus(OSPid,Pman_data) ->

    case get_pos(OSPid,Pman_data#gs_pman.focus_pid) of
	false ->
	    disable_pid_actions(),
	    Pman_data#gs_pman{focus = 1, focus_pid = undefined};
	
	Pos   ->
	    enable_pid_actions(),
	    Pman_data#gs_pman{focus = Pos + 1}
    end.

%%
%% Restore saved options from file
%%

restore_options(Pman_data)->
    %% Platform dependent code for determining where to find the 
    %% save user options.
    File = pman_osdepend:options_file_name(),
    Parent = Pman_data#gs_pman.win,
    case pman_options:read_from_file(File) of
	{default, format, Options} ->
	    spawn(tool_utils, notify,
		  [Parent,
		   "Problem with option file contents.\nUsing system defaults."]),
	    Pman_data#gs_pman{options=Options};
	{default, file, Options} ->
	    Pman_data#gs_pman{options=Options};
	Options ->
	    Pman_data#gs_pman{options=Options}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Refresh the main overview window. 
%% (This is called automatically every ?REFRESH_TIME millisecond)
%%

execute_cmd('Refresh',Pman_data,_Data,_Args) ->

    %% At refresh time, the set of processes to show (ospid_display)
    %% need to be updated. Refresh can be manual or automatic.

    refresh(Pman_data);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Focus commands
%%

%% With mouse click
execute_cmd({focus,To}, Pman_data,_Data,_Args) ->

    focus(To, Pman_data);

%% Previous line
execute_cmd(focus_previous, Pman_data,_Data,_Args) ->

    focus(Pman_data#gs_pman.focus-1, Pman_data);

%% Next line
execute_cmd(focus_next, Pman_data,_Data,_Args) ->

    focus(Pman_data#gs_pman.focus+1, Pman_data);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Show all running processes. Actually, make sure that
%% all unshown (new) and all explicitly hidden are added to the set of shown 
%% processes.
%%
%% The filtering (like Hide system,  and Auto Hide New) remain in effect,
%% but notice that all processes become *explicitly* shown, and therefore
%% the filters will be ineffective for filtering out the processes.
%% New processes however, will be covered by the filters.
%% 

execute_cmd('Show All',Pman_data,_Data,_Args) ->

    OSPidAll = pman_process:r_processes(Pman_data#gs_pman.node),
    
    New_Pman_data = Pman_data#gs_pman{hide_pids=[],
				      show_pids=OSPidAll,
				      ospid_display=OSPidAll},

    refresh(New_Pman_data);



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Open a list of not shown (hidden + new + system) processes (PIDS)
%% where the user can select which processes shall be shown.
%% 
execute_cmd('Show Selected',Pman_data,_Data,_Args) ->

    %% Open a dialog with a list of all PIDs that are not currently shown
    OSPidAll = pman_process:r_processes(Pman_data#gs_pman.node),
    OSPidDisplay =  Pman_data#gs_pman.ospid_display,
    OSPidAlternatives = ordsets:subtract(OSPidAll, OSPidDisplay),

    Title = "Select Processes to Show",

    %% This is a bit contrived due to the fairly poor functionality of gs:lists
    %% First make a list of tuples with Pids and their String desrcription
    %% to be used in the selection dialog. Then extract the list of
    %% strings to be sent to the dialog function. 
    %% Some of this should perhaps be included in a more general dialog
    %% function, that handles "dictionary"-type choice lists.

    Pairs = pman_process:mk_procname_pairs(OSPidAlternatives),
    Strings =
	lists:map(fun(T) ->
			  element(2,T)
		  end,
		  Pairs),

    DialogResult = pman_tool:modal_multiple_select(Pman_data#gs_pman.win,
						   Title,
						   Strings),

    %% Next, if a selection has been made, match the selected strings
    %% with its corresponding Pid in the tuple-list, and extract the Pids.

    case DialogResult of
	{cancelled, _Reason} ->
	    Pman_data;
	
	List when list(List) -> 

	    List2 =
		lists:map(fun(S) ->
				  case lists:keysearch(S, 2, Pairs) of
				      {value, {Pid, _String}} ->
					  Pid;
				      false -> false
				  end
			  end,
			  List),
				  
	    %% Add the selected PIDs from the dialog to the list
	    %% of explicitly shown PIDs
	    OSPidShown = ordsets:union(Pman_data#gs_pman.show_pids,
				       ordsets:from_list(List2)),
	    New_Pman_data = Pman_data#gs_pman{show_pids = OSPidShown},
	    
	    %% Refresh the list of PIDs
	    refresh(New_Pman_data)
    end;






%% Start Help window

execute_cmd('Help',Pman_data,_Data,_Args)  ->
    HelpFile = filename:join(code:priv_dir(pman), "../doc/index.html"),
    tool_utils:open_help(gse:start([{kernel, true}]), HelpFile),
    Pman_data;


%% Trace the shell

execute_cmd('Trace Shell',Pman_data,_Data,_Args) ->
    case pman_shell:find_shell() of
	noshell ->
	    Pman_data;
	Shell -> 
	    pman_shell:start({{shell,Shell},self()},Pman_data#gs_pman.options),
	    Pman_data#gs_pman{noshell = false}
    end;


%% Start Trace Window

execute_cmd('Trace Process',Pman_data,_Data,_Args) ->
    case get_pid_in_focus(Pman_data) of
	false      ->
	    Pman_data;
	{true,{pidfunc,Pid,_}} ->
	    pman_shell:start({Pid,self()},
			     Pman_data#gs_pman.options),
	    Pman_data
    end;

%% Open trace windows for all pids linked to the pid in focus

execute_cmd('All Links',Pman_data,_Data,_Args) ->
    case get_pid_in_focus(Pman_data) of
	false      -> Pman_data;
	{true,{pidfunc,Pid,_}} ->
	    {links,Pids} = pman_process:pinfo(Pid, links),
	    pman_shell:start_list(Pids,
				  self(),
				  Pman_data#gs_pman.options), 
	    Pman_data
    end;

%%  Open trace window for a specific pid linked to the pid in focus

execute_cmd({'Links',LPid},Pman_data,_Data,_Args) ->
    case get_pid_in_focus(Pman_data) of
	false      -> Pman_data;
	{true,{pidfunc,_Pid,_}} ->
	    pman_shell:start(LPid, Pman_data#gs_pman.options),
	    Pman_data
    end;

%% Kill the pid in focus

execute_cmd('Kill',Pman_data,_Data,_Args) ->
    case get_pid_in_focus(Pman_data) of
	false      -> 
	    Pman_data;
	{true,{pidfunc,Pid,_}} ->
	    exit(Pid,kill),
	    Pman_data
    end;

%% Open window with module information

execute_cmd('Module',Pman_data,_Data,_Args) ->
    case get_pid_in_focus(Pman_data) of
	false            -> Pman_data;		%0
	
	{true,{pidfunc,_,{ModuleName,_,_}}} ->
	    pman_module_info:start(ModuleName),
	    
	    Pman_data
    end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Hide an explicitly selected process (PID).
%%

execute_cmd('Hide Selected Process', Pman_data, _Data, _Args) ->
    case get_pid_in_focus(Pman_data) of
	%% No process selected
	false -> 
	    Pman_data;
	%% Hide selected process
	{true,{pidfunc,Pid,{_Mod,_Fun,_}}} ->

	    %% Add it to the list of hidden PIDs
	    NewHidden = ordsets:add_element(Pid,Pman_data#gs_pman.hide_pids),
	    Pman_data1 =
		Pman_data#gs_pman{hide_pids=NewHidden},

	    %% Remove it from the list of shown PIDs
	    NewShown = ordsets:del_element(Pid,Pman_data#gs_pman.show_pids),
	    Pman_data2 =
		Pman_data1#gs_pman{show_pids=NewShown},

	    %% Refresh
	    refresh(Pman_data2)
    end;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Hide modules
%%
%% Opens a dialog where the user can select from a list of 
%% the loaded modules or enter an arbitrary module name. 
%%
%% The selected modules are added to the list of hidden modules.
%%

execute_cmd('Hide Modules',Pman_data,_Data,_Args) ->

    %% Get all loaded modules, and then strip unnecessary info

    LITupleLoaded = code:all_loaded(),
    MapFun =
	fun(T) ->
		element(1,T)
	end,
    OSModuleLoaded =
	ordsets:from_list(lists:map(MapFun, LITupleLoaded)),

    %% Let the user select which of the loaded modules to exclude from the
    %% process overview

    Title = "Module selection",
    DialogResult = pman_tool:modal_multiple_select(Pman_data#gs_pman.win,
						   Title,
						   OSModuleLoaded),
    case DialogResult of
	{cancelled, _Reason} ->
	    Pman_data;
	Selection ->
	    OSDialogResult = ordsets:from_list(Selection),
    
	    OSModuleHidden = ordsets:union(OSDialogResult,
					   Pman_data#gs_pman.hide_modules),
    
	    refresh(Pman_data#gs_pman{hide_modules=OSModuleHidden})
    end;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Hide all
%%
%% Move all currently shown processes to the set of hidden 
%% processes. This is performed instantly, and any new processes are 
%% treated according to the other settings.
%%
%% Already unshown processes due to the filtering are not affected.
%% I.e. they will remain hidden, but will be shown if the filter is
%% switched off.

execute_cmd('Hide All',Pman_data,_Data,_Args) ->

    OSPidAll = pman_process:r_processes(Pman_data#gs_pman.node),

    New_Pman_data =
	Pman_data#gs_pman{hide_pids = OSPidAll,
			  show_pids = [],
			  ospid_display = [],
			  focus = 1,
			  size = 0},

    

    %% Update the user interface
    disable_pid_actions(),


    %% Refresh
    refresh(New_Pman_data);



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Explicitly show a specific process (PID)
%%

execute_cmd('Show Selected Process', Pman_data, _Data, _Args) ->
    case get_pid_in_focus(Pman_data) of

	%% No process selected
	false            -> 
	    Pman_data;

	%% Show selected process
	{true,{pidfunc,Pid,{_Mod,_Fun,_}}} ->

	    %% Add it to the list of shown PIDs
	    Shown = Pman_data#gs_pman.show_pids,
	    Pman_data1 = Pman_data#gs_pman{show_pids=[Pid|Shown]},

	    %% Refresh
	    refresh(Pman_data1)
    end;

%%
%% Set default options for tracing
%%

execute_cmd('Default Options',Pman_data,_Data,_Args) ->
    OldOptions = Pman_data#gs_pman.options,
    NewOptions = pman_options:dialog(Pman_data#gs_pman.win,
				     "Default Trace Options",
                                     OldOptions),
    case NewOptions of
	{error, _Reason} ->
	    Pman_data;
	Options ->
	    Pman_data#gs_pman{options=Options}
    end;

%%
%% Save the set default options to the users pman-frofile file
%%

execute_cmd('Save Options', Pman_data,_Data,_Args)->
    %% Platform dependent code for determining where to store
    %% the user options
    FileName = pman_osdepend:options_file_name(),
    Parent = Pman_data#gs_pman.win,
    Options = Pman_data#gs_pman.options,

    %% Before trying to save anything to an options file, make
    %% sure that the directory where it should be stored exists.

    pman_osdepend:mkdir_for_file(FileName),
    
    case catch pman_options:save_to_file(Options,FileName) of
	{'EXIT', {file_problem, _Reason}} ->
	    tool_utils:notify(Parent,"Could not save options.");
	true ->
	    tool_utils:notify(Parent,"Options saved in\n" ++ FileName)

    end,
    Pman_data;
    

    

%% Exit the application

execute_cmd('Exit',Pman_data,_Data,_Args) ->
    gs:destroy(Pman_data#gs_pman.win),
    exit(topquit);


%% Change traced node to Node.

execute_cmd({node,Node},Pman_data,_Data,_Args) ->
    CurrentNode = Pman_data#gs_pman.node,
    Pman_data1 = Pman_data#gs_pman{node=Node},

    case CurrentNode of
	% No change
	Node ->
	    Pman_data1;
	Old    ->
%	    Title = io_lib:format("PMAN:Overview on ~w",[Node]),
%	    gs:config(Pman_data1#gs_pman.win,{title, Title}),
	    gse:disable(Node),
	    gse:enable(Old),
	    refresh(Pman_data1)
    end;
			    

%% Resize Window.
%% 
%% When the window is resized, a configure event is received,
%% this must be "forwarded" to the geometry managing frame.
%% It is also forwarded to a "manual" geometry manager for the grid.
%% 
execute_cmd({configure,W,H,_X,_Y},Pman_data,_Data,_Args) ->
    
    gse:resize(Pman_data#gs_pman.frame, W, H-?MENU_HEIGHT),

    Grid = Pman_data#gs_pman.grid,
    case abs(W - gs:read(Grid,width) - 6) of
	0 ->
	    ok;   %%Avoid refreshig width. Takes lots of processor power
	_Anything ->
	    Cols = pman_win:calc_columnwidths(W-6),
	    gs:config(Grid,Cols)
    end,
    pman_win:configwin(Grid , W, H),
    Pman_data;

%%
%% Process display options
%%

%%
%% Hide system processes
%%
%% The checkbutton for hiding system processes has been selected.

execute_cmd('Hide System', Pman_data, _Data, Args ) ->
    [_Text, _Group, Bool|_Rest] = Args,
    New_Pman_data = Pman_data#gs_pman{hide_system=Bool},
    refresh(New_Pman_data);



%%
%% Hide new processes
%%
%% The checkbutton for hiding new processes has been selected.

execute_cmd('Auto Hide New', Pman_data, _Data, Args ) ->
    [_Text, _Group, Bool|_Rest] = Args,

    Pman_data2 = Pman_data#gs_pman{hide_new=Bool},

    %% Update all nodes' old pids with their current pids
    Pman_data3 = oldPids_update_nodes (Pman_data2#gs_pman.nodes, Pman_data2),

    refresh(Pman_data3);




execute_cmd(_Cmd,Pman_data,_Data,_Args) -> Pman_data.

%% ---------------------------------------------------------------
%% Execute the Various command requests received Through events
%% in GS. Events created by the User or by the Window Manager.
%% ---------------------------------------------------------------

gs_cmd(Cmd,Pman_data) ->
    case Cmd of
       
	%%Window manager commands
        {gs,_W,configure ,Data,[W,H,X,Y|_]} ->
	    execute_cmd({configure,W,H,X,Y},Pman_data,Data,[]);
        {gs,_W,destroy, _,_        } ->
	    exit(topquit);    %%T={node,N},{show,M}

	%% Dynamic commands
	
	%% Click in any object where the GS Data field is a 2-tuple
	{gs,_W,click,T, Args} when tuple(T), size(T) == 2 ->
	    execute_cmd(T,Pman_data,[],Args);
	
	%% Single click in the process list sets focus to the clicked process
	{gs,_Gl,click,{pidfunc,_,_},[_Col,Row|_T]} when integer(Row) ->
	    focus(Row,Pman_data);
	{gs,_Gl,doubleclick,{pidfunc,_P,_M},[_Col,Row| _]} when integer(Row) ->
	    execute_cmd('Trace Process',Pman_data,[],[]);
      
	%%Menu Commands / Button presses
        {gs,Command, click, Data, Args}	    ->
	    execute_cmd(Command,Pman_data,Data,Args);

        %% Keyboard accelerator commands
	{gs,_W,keypress,[],['Up',_,0,0]}   ->
	    focus(Pman_data#gs_pman.focus-1,Pman_data);

	{gs,_W,keypress,[],['Down',_,0,0]} ->
	    focus(Pman_data#gs_pman.focus+1,Pman_data);

	{gs,_W,keypress,[],['Return',_,0,0]} ->
	    execute_cmd('Trace',Pman_data,[],[]);
        {gs,_W,keypress,_D,[Key,_,0,1]} ->
	    execute_cmd(key(Key),Pman_data,[],[]);


     	_Other ->

	    Pman_data
    end.

%%
%% Key accelerators translations
%%

key(e) -> 'Exit';
key(r) -> 'Refresh';
key(d) -> 'Hide Selected';
key(i) -> 'Hide All';
key(a) -> 'Show All';
key(s) -> 'Shell';
key(t) -> 'Trace';
key(m) -> 'Module';
key(l) -> 'All Links';
key(k) -> 'Kill';
key(h) -> 'Help';
key(z) -> 'Exit';
key(p) -> focus_previous;
key(n) -> focus_next;
key(O) -> O.


%%
%% Convenience functions for disabling/enabling menu items that require that
%% a process is selected.
%%

disable_pid_actions() ->
    lists:foreach({gse, disable}, ?REQUIRES_FOCUS).


enable_pid_actions()  ->
    lists:foreach({gse, enable}, ?REQUIRES_FOCUS).


%% Check if node is running in noshell mode and if so disable the
%% 'Trace Shell' menu option.

trace_shell_possible(#gs_pman{noshell = true}) ->
    gse:disable('Trace Shell');
trace_shell_possible(_) ->
    ok.

%% ---------------------------------------------------------------
%% The main loop for the  pman window
%% ---------------------------------------------------------------

loop(Pman_data) ->



    receive
        %% test events - ping, stop
        {ping, Pid} ->
            Pid ! {pman, alive},
            loop (Pman_data);
 
        {stop, Pid} ->
            Pid ! {pman, stopped},
	    gs:destroy(Pman_data#gs_pman.win),
	    exit(stopped);

	{nodedown,Node} ->
	    T = io_lib:format("Node~n~p~ndown.",[Node]),
	    pman_win:dialog_window(gse:start(),T),
	    Pman_data2 = oldPids_delete_node (Pman_data, Node),

	    New_Pman_data =
		case is_alive() of

		    % This node changed name/became undistributed.
		    % So we need to get rid of all distribution data.
		    false ->
			Pman_data2#gs_pman{node=node()};

		    %Another node died
		    true ->
			case Pman_data2#gs_pman.node of
			    
			    %% Supervised node died,
			    %% change overview to current node
			    Node ->
				execute_cmd({node,node()},
					    Pman_data2,ignore,ignore);
			    _Otherwise ->
				Pman_data2
			end
		end,
	    loop(update_nodes_menu(New_Pman_data));

	{nodeup, Node} ->
	    Pman_data2 = oldPids_add_node (Pman_data, Node),
	    loop(update_nodes_menu(Pman_data2));

	%% Ignore EXIT signals from "inferior" processes. 
	{'EXIT', _Pid, _Reason} ->
	    loop(Pman_data);
	

	Cmd -> 
	    case catch gs_cmd(Cmd,Pman_data) of
		{'EXIT',badrpc} ->
		    T = "ERROR: Could not access node",
		    pman_win:msg_win(T),
		    loop(Pman_data);
		{'EXIT', dead} ->
		    T = "ERROR: The process is dead",
		    pman_win:msg_win(T),
		    loop(Pman_data);
		{'EXIT',topquit} ->
		    exit(topquit);
		{'EXIT',Why} ->
		    exit({received_destructive_EXIT_signal,Why});
		New_Pman_data ->

		    loop(New_Pman_data)
	    end
    after ?REFRESH_TIME ->
	    New_Pman_data = refresh(Pman_data),
	    loop(New_Pman_data)

    end.


%% refresh/1 - refreshes the main window (Catches unexpected errors and exits)
%%
refresh(Pman_data) ->

    case catch refresh2(Pman_data) of
	{'EXIT', Reason} ->
	    io:format("Internal ERROR in ~p:refresh/1:~p~n",[?MODULE,Reason]),
	    exit(Reason);
	Otherwise ->
	    Otherwise
    end.


%% 

refresh2(Pman_data) ->

    Pman_data1 = update_nodes_menu(Pman_data),

    Current = get_current_name(Pman_data1),


    Nodes = get_nodes(),
    Pman_data2 = Pman_data1#gs_pman{node=Current,
				    nodes = Nodes},
    
    gse:disable(Current),


    %% At refresh time, the set of processes to show (ospid_display)
    %% need to be updated. Refresh can be manual or automatic.

    %%
    %% Update 
    %%

    {OSPidShow, CPidHidden} = get_display_info(Pman_data2),

    %% Then update the record:

    Pman_data3 = Pman_data2#gs_pman{ospid_display = OSPidShow},

    %% Update the window title
    gse:config(Pman_data3#gs_pman.win,
	       [{title, lists:concat(["PMAN:Overview on ", Current])}]),

    %%
    %% Now do the actual window updating
    %%


    Size = pman_win:uppdate(Pman_data3#gs_pman.grid,
			    OSPidShow,
			    CPidHidden),

    %%
    %% All processes are now displayed.
    %% Now set the focus appropriately.

    Pman_data4 = set_focus(OSPidShow,Pman_data3),

    focus(Pman_data4#gs_pman.focus, Pman_data3),

    trace_shell_possible(Pman_data4),
    
    case Size of
	1 -> ok;
	_ -> gse:enable('Hide All')
    end,

    Pman_data4#gs_pman{size = Size}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Change focus

focus(To,Pman_data) ->
    Real_to = check_focus_choice(To,Pman_data#gs_pman.size),

    Current = Pman_data#gs_pman.focus,
    Pman_data1 = Pman_data#gs_pman{focus=Real_to},

    Focus_pid = case get_pid_in_focus(Pman_data1) of
		    {true,{pidfunc,Pid,_}} ->
			pman_win:change_colour(Pman_data1#gs_pman.grid,
					       Current,Real_to),
			enable_pid_actions(),
			Pid;
		    false ->
			disable_pid_actions(),
			undefined
		end,

    Pman_data1#gs_pman{focus_pid = Focus_pid}.




%% get_display_info/1 returns a tuple with the processes to show and
%%    a number of hidden processes
%%
get_display_info(Pman_data) ->
    
    %%
    %% Update data structures.
    %%

    OSPidAll = pman_process:r_processes(Pman_data#gs_pman.node),
    OSPidHidden = Pman_data#gs_pman.hide_pids,
    OSPidShown = Pman_data#gs_pman.show_pids,

    %% Conditionally add new processes to "Don't show set"
    OSPidDontShow1 =
	case Pman_data#gs_pman.hide_new of
	    true ->
		case oldPids_get (Pman_data, Pman_data#gs_pman.node) of
		    %% The node has not been visited before, 
		    %% so it has no old pids to remove from OSPidAll.
		    %% Hide none.
		    [] -> 
			[];

		    OSPidOld ->
			ordsets:subtract(OSPidAll, OSPidOld)
		end;

	    false ->
		[]
	end,
    
    %% Conditionally add hidden system processes to "Don't show set"
    OSPidDontShow2 =
	case Pman_data#gs_pman.hide_system of
	    true ->
		OSPidSystem =
		    lists:filter({pman_process, is_system_process},
				 OSPidAll),
		ordsets:union(OSPidDontShow1, OSPidSystem);
	    false ->
		OSPidDontShow1
	end,

    %% Add all explicitly hidden processes to "Don't show set".

    OSPidDontShow3 = ordsets:union(OSPidDontShow2, OSPidHidden),

    %% Prepare a function for 

    FilterFun =
	fun(P) ->
		pman_process:is_hidden_by_module(P,
						 Pman_data#gs_pman.hide_modules)
	end,
    OSPidDontShow4 = ordsets:union(OSPidDontShow3,
				   lists:filter(FilterFun, OSPidAll)),


    %% Finally, remove all processes that are supposed to be hidden from the
    %% set of currently running processes, and then add the set of 
    %% explicitly shown processes.

    OSPidShow1 = ordsets:subtract(OSPidAll, OSPidDontShow4),
    OSPidShow2 = ordsets:union(OSPidShow1, OSPidShown),

    %%  (???) And even more finally :-) (Beware this is a temporary kludge)
    %% Since the hiding/showing mechanisms does not handle multiple nodes
    %% we may get unwanted processes displayed when we are running a
    %% distributed system. So here we make sure that we only display 
    %% process running on the "current" node.
    
    OSPidShow3 = ordsets:intersection(OSPidShow2, OSPidAll),
   

    
    %% Calculate number of currently hidden processes
    CPidHidden = length(OSPidAll) - length(OSPidShow3) ,


    {OSPidShow3, CPidHidden}.



%% supervised_node_name/2 - returns the name for the supervised node.

%% "This" node may have two names, either 'nonode@nohost', or any other
%% name.

get_current_name(Pman_data) ->
    LastName = Pman_data#gs_pman.node,

    This = node(),
    
    case LastName of
	'nonode@nohost' ->
	    node();

	_Otherwise ->
	    % 1. Previous was this, but distributed.
	    case This of
		'nonode@nohost' ->
		    node();

	    % 2. Last was another node.
		_Remote ->
		    LastName
	    
	    end
    end.



%%% oldPids_add_node  /2, 3
%%%

oldPids_add_node (Pman_data, Node) ->
    Old2 = [{Node, []} | Pman_data#gs_pman.old],
    Pman_data#gs_pman{old = Old2}.

oldPids_add_node (Pman_data, Node, Pids) ->
    Old2 = [{Node, Pids} | Pman_data#gs_pman.old],
    Pman_data#gs_pman{old = Old2}.



%%% oldPids_delete_node  /2
%%%

oldPids_delete_node (Pman_data, Node) ->
    Old2 = lists:keydelete (Node, 1, Pman_data#gs_pman.old),
    Pman_data#gs_pman{old = Old2}.



%%% oldPids_update_node  /3
%%%

oldPids_update_node (Pman_data, Node, Pids) ->
    Pman_data2 = oldPids_delete_node (Pman_data, Node),
    oldPids_add_node (Pman_data2, Node, Pids).



%%% oldPids_get  /2
%%%
%%% oldPids_get returns the old Pids of the given node.
%%%

oldPids_get (#gs_pman{old = Old}, Node) ->
    case lists:keysearch (Node, 1, Old) of
	{value, {_Node, Pids}} ->
	    Pids;

	false ->
	    []
    end.



%%% oldPids_update_nodes  /2
%%%
%%% Updates the old pids of the given nodes with the current pids.
%%%

oldPids_update_nodes ([], Pman_data) ->
    Pman_data;

oldPids_update_nodes ([Node | T], Pman_data) ->
    OSPidAll = pman_process:r_processes(Node),
    Pman_data2 = oldPids_update_node (Pman_data, Node, OSPidAll),
    oldPids_update_nodes (T, Pman_data2).

