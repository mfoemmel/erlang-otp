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
%%% Purpose : dbg_ui_winman is a windows manager for the different
%%%           windows in the debugger.
%%% Purpose : Common functions for the windows system, like menues and so on.
%%%           Other window functions for the GUI can be added.
%%%----------------------------------------------------------------------

-module (dbg_ui_winman).


-export ([stop/0, 
	  on_top/1, 
	  insert_win/3, 
	  delete_win/1, 
	  delete_att_win/1,
	  win_started/2,
	  print_wins/0]).

-export ([init/1]).

%%% Created : 19 Feb 1998 by Stefan Anbratt <stefan@erlang.ericsson.se>
%%%----------------------------------------------------------------------

-export ([windows_menu/1, update_windows_menu/1]).

-define (WIN_LOOP, win_manager).   



%%% start  /1
%%%
%%% start returns ok to the calling process if the 
%%% win_manager is already started or else it will 
%%% be started.
%%%
%%% Pre:
%%%    Pid  ==  pid of the calling process
%%%

start (Pid) ->
    Reg_Pids = registered (),     % registered Pids

    case lists:member (?WIN_LOOP, Reg_Pids) of
	false ->
	    spawn (?MODULE, init, [Pid]);

	_ ->
	    call (already_started),
	    Pid ! ok
    end,
    started.



%%% stop  /0
%%%

stop () ->
    call (stop).



%%% insert_win  /3
%%%
%%% insert_win inserts the given window in the windows menu.
%%%
%%% If a window with the same name already has been started,
%%% delete the old window and insert the new.
%%%
%%% Pre:  
%%%    Type  ==  atom
%%%              type of window  ==  monitor  ||  view  ||  trace
%%%    Win   ==  object identifier 
%%%    Pid   ==  process identifier
%%%              Pid that created the window
%%%

insert_win (Type, Win, Pid) ->
    start (self ()),
    receive
	_ ->
	    true
    end,

    Name = gs:read (Win, title),
    Atom_Name = dbg_ui_aux:to_atom (Name),
    Attached_pid = dbg_ui_aux:get_att_pid (Type, Name),

    call ({insert_win, {Type, Atom_Name, Win, Pid, Attached_pid}}).



%%% delete_win  /1
%%%
%%% delete_win deletes the given window from the windows menu.
%%% 
%%% Pre:
%%%    Win  ==  object identifier
%%%

delete_win (Win) ->
    call ({delete_win, Win}).



%%% delete_att_win  /1
%%%
%%% delete_att_win deletes the given process' window
%%%
%%% Pre:
%%%    Pid  ==  process identifier
%%%             attached process
%%%

delete_att_win (Pid) ->
    call ({delete_att_win, Pid}).



%%% exists_win  /2
%%%
%%% exists_win returns true or false to the calling process 
%%% if the given window already exists or not.
%%%
%%% Pre:
%%%    Name  ==  atom  //  string
%%%              name of window
%%%
%%% Def:
%%%    exists_win  ==  true  ||  false
%%%

exists_win (Pid, Name) ->
    start (self ()),
    receive
	_ ->
	    true
    end,
    
    Atom_Name = dbg_ui_aux:to_atom (Name),
    call ({exists_win, Pid, Atom_Name}).



%%% on_top  /1
%%%
%%% on_top raises the given window
%%%
%%% Pre:
%%%    Win  ==  object identifier
%%%

on_top (Win) ->
    call ({on_top, Win}).



%%% print_wins  /0
%%%
%%% print_wins prints the windows at the prompt
%%%

print_wins () ->
    call (print_wins).



%%% call  /1
%%%
%%% call calls the main receive loop
%%%
%%% Pre:
%%%    Request  ==  term
%%%

call (Request) ->
    ?WIN_LOOP ! Request.



%%% init  /1
%%%

init (Pid) ->
    register (?WIN_LOOP, self()),
    process_flag (trap_exit, true),
    L = [{monitor, 'Start Monitor Window', null_monitor, null_monitor, null}],
    Pid ! ok,     % the win man will be started
    loop (L).



%%% loop  /1
%%%

loop (L) ->
    receive
        {insert_win, Elem} ->
	    {_Type, _Atom_Name, _Win, Pid, Attached_pid} = Elem,
	    link (Pid),
            New_L = insert_win (L, Elem),
	    update (New_L, L),
	    on_top_1 (Elem),
            loop (New_L);
	
        {delete_win, Elem} ->
            New_L = delete_win (L, Elem),
	    update (New_L, L),
            loop (New_L);
	
        {delete_att_win, Elem} ->
            delete_att_win (L, Elem),
            loop (L);
	
	{exists_win, Pid, Name} ->
	    exists_win (L, Pid, Name),
	    loop (L);

        {on_top, Win} ->
            on_top_1 (Win),
            loop (L);
	
	already_started ->        
	    loop (L);

	print_wins ->
	    print_wins (L),
	    loop (L);

	stop ->
            true;
	
	{'EXIT', Pid, _Reason} ->
	    New_L = pid_terminated (L, Pid),
	    update (New_L, L),
	    loop (New_L);

	_ ->
            loop (L)
    end.	



%%% insert_win  /2
%%%
%%% insert_win returns a new list after inserted the given element.
%%%
%%% If a window with the same name already has been started,
%%% delete the old window and insert the new.
%%%
%%% If the window is the first real monitor window, 
%%% the default null_monitor window is replaced.
%%%
%%% Pre:  
%%%    L        ==  list 
%%%                 list of Elems
%%%    Elem     ==  {Type, Name, Win, Pid}
%%%    Type     ==  atom
%%%                 type of window  ==  monitor  ||  view  ||  trace
%%%    Name     ==  atom  ||  string
%%%                 name of the window
%%%    Win      ==  object identifier 
%%%    Pid      ==  process identifier
%%%                 Pid that created the window
%%%    Att_pid  ==  process identifier
%%%                 the attached process
%%%
%%% Def:  
%%%    insert_win (L, Elem) == [Elem | L]
%%%

insert_win (L, {monitor, Name, Win, Pid, Att_pid}) ->
    New_L = case lists:keymember (null_monitor, 3, L) of
		true ->
		    lists:keyreplace (monitor, 1, L, 
				      {monitor, Name, Win, Pid, Att_pid});
		
		_ ->
		    [{monitor, Name, Win, Pid, Att_pid} | L]
	    end,

    sort (New_L);

 
insert_win (L, Elem) ->
    New_L = [Elem | L],
    sort (New_L).



%%% delete_win  /2
%%%
%%% delete_win returns a new list after deleting the given window 
%%% from the windows menu.
%%%  
%%% If the monitor window is to be deleted, all the other windows 
%%% are also deleted.
%%%
%%% Pre:  
%%%    L    ==  list
%%%    Win  ==  object identifier
%%%
%%% Def:
%%%    a list without Win
%%%

delete_win (L, Win) ->         
    case lists:keysearch (Win, 3, L) of
	{value, {monitor, _, _, _, _}} ->
	    New_L = lists:keydelete (Win, 3, L),
	    delete_all_win_proc (New_L),
	    New_L;            

	_ ->
	    lists:keydelete (Win, 3, L)
    end.



%%% delete_att_win  /2
%%%
%%% delete_att_win returns a new list after deleting the given 
%%% process' window from the windows menu and as a process.
%%%  
%%% kill_win/1 is called which sends a delete message to the windows.
%%%
%%% Pre:  
%%%    L        ==  list
%%%    Att_pid  ==  process identifier
%%%                 attached process
%%%
%%% Def:
%%%    
%%%

delete_att_win ([], _) ->
    true;


delete_att_win ([{_, _, _, Pid, Att_pid} | T], Att_pid) ->         
    kill_win (Pid),
    delete_att_win (T, Att_pid);


delete_att_win ([H | T], Att_pid) ->
    delete_att_win (T, Att_pid).



%%% delete_all_win_proc  /1
%%%
%%% delete_all_win_proc sends a destroy messages to all 
%%% the processes connected with the windows in the given list.
%%%
%%% Pre:  
%%%    L  ==  list
%%%
%%% Post:  
%%%    the windows connected with the processes are destroyed
%%%
 
delete_all_win_proc ([]) ->
    [];


delete_all_win_proc ([{_, _, _, Pid, _} | T]) ->
    kill_win (Pid),
    delete_all_win_proc (T).



%%% kill_win  /1
%%%
%%% kill_win sends a destroy window message to the given process.
%%%
%%% Pre:
%%%    Pid  ==  process identifier
%%%
%%% Post:
%%%    the window to the process are destroyed
%%%

kill_win (Pid) ->
    Pid ! {gs, null, destroy, null, null}.
    


%%% exists_win  /3
%%%
%%% exists_win returns the existing window
%%% or false if the window already exists or not.
%%%
%%% Pre:
%%%    L     ==  list
%%%    Pid   ==  process identifier
%%%    Name  ==  atom
%%%              name of window
%%%
%%% Def:
%%%    exists_win  ==  {true, Win}  ||  false
%%%

exists_win (L, Pid, Name) ->
    Answ = case lists:keysearch (Name, 2, L) of
	       {value, {_, _, Win, _, _}} ->
		   {true, Win};
	       _ -> 
		   false
	   end,

    Pid ! Answ.



%%% on_top_1  /1
%%%
%%% on_top_1 raises the given window to the top.
%%%
%%% If it is the null_monitor window that is to be raised,
%%% debugger:start () is clled.
%%%
%%% Pre:  
%%%    Win  ==  object identifier
%%%
%%% Post:
%%%    gs:config (Win, [raise])
%%%

on_top_1 (null_monitor) ->
    spawn (debugger, start, []);


on_top_1 ({_, _, Win, _, _}) ->
    on_top_1 (Win);


on_top_1 (Win) ->    
    gs:config (Win, [raise,
		     {iconify, false},
		     {setfocus, true}]).



%%% update  /2 /3
%%%
%%% update sends a update message to all windows with the given list.
%%%
%%% When there are no windows left or the last windows is 
%%% the null_monitor window, call stop ().
%%%
%%% Pre:
%%%    New_L  ==  list
%%%               list to update the windows with
%%%    Old_L  ==  list               
%%%               list to be deleted in the windows
%%%

%%% No windows to update

update ([], _) ->     
    stop ();


%%% Only null_monitor window left

update ([{monitor, _, null_monitor, _, _} | []], _) ->     
    stop ();                                            


update (New_L, Old_L) ->
    update (New_L, New_L, Old_L).


%%% All windows updated

update ([], _, _) ->
    ok;


%%% Don't update the null_monitor window

update ([{_, _, _, null_monitor, _} | T], New_L, Old_L) ->
    update (T, New_L, Old_L);


update ([{_, _, _, Pid, _} | T], New_L, Old_L) ->
    Pid ! {update_windows, {New_L, Old_L}},
    update (T, New_L, Old_L).
    


%%% print_wins  /1
%%%
%%% print_windows prints the given list.
%%%
%%% Pre:  
%%%    L  ==  list
%%%

print_wins (L) ->
    io:format ("~nWindows: ~p~n", [L]).



%%% pid_terminated  /2
%%%

pid_terminated (L, Pid) ->
    case lists:keysearch (Pid, 4, L) of
	{value, {_Type, _Atom_Name, Win, Pid, _Attached_pid}} ->
	    delete_win (L, Win);
	
	_False ->
	    L
    end.



%%% sort  /1
%%%
%%% sort returns the given list sorted.
%%%
%%% 1 - sort the 'age' of the windows
%%% 2 - put the monitor window on top
%%%
%%% Pre:
%%%    L  ==  list
%%%

sort (L) ->
    L_1 = lists:keysort (3, L),

    case lists:keysearch (monitor, 1, L_1) of
	{value, Tuple} ->

	    case lists:nth (1, L_1) =/= Tuple of
		true ->
		    L_2 = lists:delete (Tuple, L_1),
		    lists:append ([Tuple], L_2); 

		_ ->
		    L_1
	    end;
	
	_ ->
	    L_1
    end.




%%% win_started  /2
%%%
%%% win_started checks if the windows is already started.
%%% If it is started it exits, else it's raised.
%%%
%%% Pre:
%%%    Pid       ==  process identifier
%%%    Win_Name  ==  atom  //  string
%%%                  name of the window         
%%%
%%% Post:
%%%    The window is not opened if it already exists.
%%%

win_started (Pid, Win_Name) ->
    exists_win (Pid, Win_Name),

    receive
	{true, Old_Win} ->
	    on_top (Old_Win),
	    true;

	_ ->
	    false
    end.







%%% windows_menu  /1
%%%

windows_menu (MenuBar) ->
    MenuButtWindows = gs:menubutton(menu_butt_windows, MenuBar, 
				    [{label, {text, " Windows "}},
				     {underline, 1}]),
    gs:menu('WindowsMenu', MenuButtWindows, []).
    


%%% update_windows_menu  /1
%%%
%%% update_windows_menu updates the menuitems under the windows menu.
%%%
%%% Pre:  
%%%    tuple with two lists, the old and the new list
%%%
%%% Post: 
%%%    The windows menu is updated
%%% 

update_windows_menu ({New_L, Old_L}) ->
    gs:destroy ('WindowsMenu'),
    gs:menu('WindowsMenu', menu_butt_windows, []),
    update_windows_menu_1 (New_L, 1).    % create the new menu



update_windows_menu_1 ([], Count) ->
    true;


%% The monitor window should be on top with a separator after.
%%

update_windows_menu_1 ([{monitor, Name, Win, Pid, _} | T], Count) ->
    gs:menuitem (Name, 'WindowsMenu', [{label, {text, Name}},
				       {underline, 0},
				       {data, [win_menu, Win]}]),
    gs:menuitem (separator, 'WindowsMenu', [{itemtype, separator}]),
    update_windows_menu_1 (T,Count);


%% This clause is used for menu items after the 9th.

update_windows_menu_1 ([{_, Name, Win, _, _} | T], Count) when Count > 9->
    Label = lists:flatten(io_lib:format("  ~s",[Name])),
    gs:menuitem (Name, 'WindowsMenu', [{label, {text, Label}},
				       {data, [win_menu, Win]}]),
    update_windows_menu_1 (T, Count);


%% This clause is used for the 9 first menu items.

update_windows_menu_1 ([{_, Name, Win, _, _} | T], Count) when Count < 10->
    Label = lists:flatten(io_lib:format("~w ~s",[Count, Name])),
    gs:menuitem (Name, 'WindowsMenu', [{label, {text, Label}},
				       {underline,0},
				       {data, [win_menu, Win]}]),
    update_windows_menu_1 (T, Count + 1).





