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
%%% Purpose : When jumping between modules in a single Attachment window
%%%           only one GS Editor object was used. This caused big delays
%%%           when large modules had to be reloaded every time one changed 
%%%           module.
%%%           Instead every attach window should have a number of editors
%%%           that only has to be raised when that module is called.
%%%           A module is only loaded the first time it's called.
%%% Future  : Make it possible to change number of editors from the menu.
%%%----------------------------------------------------------------------

-module(dbg_ui_cache).


-export ([
	  start/1,
	  get_editor/2,
	  get_editors/1,
	  current_editor/1,
	  insert_editor/3,
	  change_no_of_editors/2,
	  print_db/0
	 ]).

-export ([init/1]).


-define (CACHE_LOOP, dbg_cache_loop).
-define (DEFAULT_NO_OF_EDITORS, 10).  % Number of editors


%%% cache  ==  record ()
%%%
%%% trace_pid        ==  pid (), pid of the trace window
%%% no_of_editors    ==  integer (), number of editors in the trace window
%%% parent_editor    ==  {ModuleName, GsEditor}
%%% current_editor   ==  {ModuleName, GsEditor}
%%% editors          ==  [{ModuleName, GsEditor}]
%%%                      Used editors after parent_editor.
%%% ModuleName       ==  atom ()
%%% GsEditor         ==  Gs editor object
%%%

-record (cache, {
	   trace_pid,        
	   no_of_editors = ?DEFAULT_NO_OF_EDITORS,    
	   parent_editor,
	   current_editor,   
	   editors = []
	  }).



%%% start (TracePid)
%%%
%%% TracePid  ==  pid ()
%%%
%%% Starts the cache server, if it's not already started,
%%% and initiates the calling process (trace- or view window).
%%%

start (TracePid) ->
    case whereis (?CACHE_LOOP) of
	undefined ->
	    spawn (?MODULE, init, [TracePid]);

	Pid when pid (Pid) ->
	    call ({initiate_pid, TracePid});
	
	_ ->
	    exit (dbg_ui_cache_undefined_error)          
    end.



%%% get_editor (Pid, Module) ->
%%%     Editor  |  exists_not
%%%
%%% Pid     ==  pid ()
%%% Module  ==  atom ()
%%% Editor  ==  Gs editor object
%%%
%%% Returns the Gs editor object connected with the given module.
%%%

get_editor (Pid, Module) ->
    call_with_result ({get_editor, Pid, Module}).



%%% get_editors (Pid) ->
%%%     Editor
%%%
%%% Pid     ==  pid ()
%%% Editor  ==  Gs editor object
%%%
%%% Returns the Gs editors connected withe the given process.
%%%

get_editors (Pid) ->
    call_with_result ({get_editors, Pid}).



%%% current_editor (Pid) ->
%%%     Editor
%%%
%%% Pid     ==  pid ()
%%% Editor  ==  Gs editor object
%%%
%%% Returns the current Gs editor of the given process.
%%%

current_editor (Pid) ->
    call_with_result ({current_editor, Pid}).



%%% insert_editor (Pid, Module, Editor)
%%%
%%% Pid     ==  pid ()
%%% Module  ==  atom ()
%%% Editor  ==  Gs editor object
%%%
%%% Inserts a new Gs editor connected with the module.
%%%

insert_editor (Pid, Module, Editor) ->
    call ({insert_editor, Pid, Module, Editor}).



%%% change_no_of_editors (Pid, NewNoOfEditors)
%%%
%%% Pid             ==  pid ()
%%% NewNoOfEditors  ==  integer ()
%%%
%%% Cahnges the number of allowed Gs editors of the given process.
%%%

change_no_of_editors (Pid, NewNoOfEditors) ->
    call ({change_no_of_editors, Pid, NewNoOfEditors}).



%%% print_db ()
%%%
%%% Prints the server data base in the erlang shell.
%%%

print_db () ->
    call (print_db).



%%% init (TracePid)
%%%

init (TracePid) ->
    register (?CACHE_LOOP, self ()),
    process_flag(trap_exit, true),
    call ({initiate_pid, TracePid}),

    loop ([]).



%%% call (Request)
%%%
%%% Request  ==  term ()
%%%

call (Request) ->
    ?CACHE_LOOP ! Request.



%%% call_with_result (Request) ->
%%%     Result
%%%
%%% Request, Result  ==  term ()
%%%

call_with_result (Request) ->
    ?CACHE_LOOP ! Request,
    receive
        {dbg_ui_cache, Result} ->
            Result
    end.



%%% loop (DB)
%%%
%%% DB  ==  [#cache{}]
%%%

loop (DB) ->
    receive
	{initiate_pid, TracePid} ->
	    link (TracePid),
	    NewDB = initiate_pid (TracePid, DB),
	    loop (NewDB);
	

	{get_editor, TracePid, Module} ->
	    {NewDB, Result} = get_editor (TracePid, Module, DB),
	    TracePid ! {dbg_ui_cache, Result},
	    loop (NewDB);


	{get_editors, TracePid} ->
	    Result = get_editors (TracePid, DB),
	    TracePid ! {dbg_ui_cache, Result},
	    loop (DB);


	{current_editor, TracePid} ->
	    Result = current_editor (TracePid, DB),
	    TracePid ! {dbg_ui_cache, Result},
	    loop (DB);


	{insert_editor, TracePid, Module, Editor} ->
	    NewDB = insert_editor (TracePid, Module, Editor, DB),
	    loop (NewDB);

	
	{change_no_of_editors, TracePid, NewNoOfEditors} ->
	    NewDB = change_no_of_editors (TracePid, NewNoOfEditors, DB),
	    loop (NewDB);

	    
	{'EXIT', Pid, _Reason} ->
	    case delete_trace_pid (Pid, DB) of
		[] ->
		    true;

		NewDB ->
		    loop (NewDB)
	    end;
	
	
	print_db ->
	    io:format ("~n~p~n", [DB]),
	    loop (DB);


	stop ->
	    true;
	
	
	Other ->
	    loop (DB)
    end.



%%% initiate_pid (TracePid, DB) ->
%%%     NewDB
%%% 
%%% NewDB  ==  [#cache{}]
%%%

initiate_pid (TracePid, DB) ->
    [#cache{trace_pid = TracePid} | DB].

		  

%%% get_editor (TracePid, Module, DB) ->
%%%     Editor  |  exists_not
%%%
%%% PRE: TracePid should exist in DB. Initiated with initiate_pid.
%%%

get_editor (TracePid, Module, DB) ->
    case lists:keysearch (TracePid, #cache.trace_pid, DB) of
	{value, #cache{no_of_editors = NoOfEditors,
		       parent_editor = ParentEditor,
		       editors = Editors}} ->

	    case lists:keysearch (Module, 1, Editors) of
		{value, {_Module, Editor}} ->
		    NewRecord = #cache{trace_pid = TracePid, 
				       no_of_editors = NoOfEditors,
				       current_editor = {Module, Editor},
				       parent_editor = ParentEditor,
				       editors = Editors},
		    NewDB = lists:keyreplace (TracePid, #cache.trace_pid, 
					      DB, NewRecord),
		    {NewDB, Editor};

		_false ->
		    {DB, exists_not}
	    end;
	
	_false ->
	    {DB, exists_not}
    end.



%%% get_editors (TracePid, DB) ->
%%%     Editors  |  exists_not
%%%
%%% PRE: TracePid should exist in DB. Initiated with initiate_pid.
%%%

get_editors (TracePid, DB) ->
    case lists:keysearch (TracePid, #cache.trace_pid, DB) of
	{value, #cache{parent_editor = undefined}} ->
	    [];

	{value, #cache{editors = Editors}} ->
	    extract_gs_editors (Editors, []);
	
	_false ->
	    exists_not
    end.



%%% extract_gs_editors (Editors, Acc) ->
%%%     Acc
%%%

extract_gs_editors ([{_Module, Editor} | T], Acc) ->
    extract_gs_editors (T, [Editor | Acc]);

extract_gs_editors ([], Acc) ->
    Acc.



%%% current_editor (TracePid, DB) ->
%%%     CurrentEditor  |  exists_not
%%%
%%% PRE: TracePid should exist in DB. Initiated with initiate_pid.
%%%

current_editor (TracePid, DB) ->
    case lists:keysearch (TracePid, #cache.trace_pid, DB) of
	{value, #cache{current_editor = {_Mod, CurrentEditor}}} ->
	    CurrentEditor;
	
	_false ->
	    exists_not
    end.



%%% insert_editor (TracePid, Module, Editor, DB) ->
%%%     NewDB
%%%
%%% PRE: Editor should not exist. Checked with get_editor.
%%%

insert_editor (TracePid, Module, Editor, DB) ->
    case lists:keysearch (TracePid, #cache.trace_pid, DB) of
	{value, #cache{no_of_editors = NoOfEditors,
		       parent_editor = undefined,
		       editors = Editors}} ->
	    NewRecord = #cache{trace_pid = TracePid, 
			       no_of_editors = NoOfEditors,
			       current_editor = {Module, Editor},
			       parent_editor = {Module, Editor},
			       editors = [{Module, Editor} | Editors]},
	    lists:keyreplace (TracePid, #cache.trace_pid, DB, NewRecord);

	{value, #cache{no_of_editors = NoOfEditors,
		       current_editor = CurrentEditor, 
		       parent_editor = ParentEditor, 
		       editors = Editors}} ->
	    
	    NewEditors = 
		case length (Editors) < NoOfEditors of
		    true ->
			[{Module, Editor} | Editors];
		    
		    _false ->
			[{Module, Editor} | 
			 delete_editor (Editors, CurrentEditor,ParentEditor)]
		end,
	    
	    NewRecord = #cache{trace_pid = TracePid, 
			       no_of_editors = NoOfEditors,
			       current_editor = {Module, Editor},
			       parent_editor = ParentEditor,
			       editors = NewEditors},
	    lists:keyreplace (TracePid, #cache.trace_pid, DB, NewRecord);
	
	_false ->
	    DB
    end.



%%% change_no_of_editors (TracePid, NewNoOfEditors, DB) ->
%%%     NewDB
%%%

change_no_of_editors (TracePid, NewNoOfEditors, DB) ->
    case lists:keysearch (TracePid, #cache.trace_pid, DB) of
	{value, #cache{no_of_editors = NoOfEditors,
		       current_editor = CurrentEditor,
		       parent_editor = ParentEditor, 
		       editors = Editors}} ->
	    
	    NewRecord = #cache{trace_pid = TracePid, 
			       no_of_editors = NewNoOfEditors,
			       current_editor = CurrentEditor,
			       parent_editor = ParentEditor,
			       editors = Editors},
	    lists:keyreplace (TracePid, #cache.trace_pid, DB, NewRecord);
	
	_false ->
	    DB
    end.



%%% delete_trace_pid (TracePid, DB) ->
%%%     NewDB
%%%

delete_trace_pid (TracePid, DB) ->
    lists:keydelete (TracePid, #cache.trace_pid, DB).



%%% delete_editor (Editors, CurrentEditor, ParentEditor) ->
%%%     NewEditors
%%%
%%% Deletes an editor acording to FIFO, 
%%% but not the current editor or the parent_editor.
%%% The editor is not deleted as a Gs editor object
%%% but only removed from the DB list.
%%%

delete_editor (Editors, CurrentEditor, ParentEditor) ->
    case lists:reverse (Editors) of
	[ParentEditor, H2 | T] ->
	    lists:reverse ([ParentEditor | T]);

	[CurrentEditor, H2 | T] ->
	    lists:reverse ([CurrentEditor | T]);

	[_H | T] ->
	    lists:reverse (T)
    end.



