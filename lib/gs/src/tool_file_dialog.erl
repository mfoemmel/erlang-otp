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
-module(tool_file_dialog).
-include_lib("kernel/include/file.hrl").

-export([start/1]).
-export([open_save_init/2, multiselect_init/2]).

-define(WIDTH,250).
-define(HEIGHT,400).
-define(BUTTW,65).
-define(BUTTH,30).

-define(DEFAULT_SAVE_FILE,"NoName").

%% The options are:
%%     {type,open|save|multiselect}
%%     {extensions,[extension()]} where extension() is for example ".erl"
%%     {dir,string()}   (where string() is an absolute path)
%%     {file, string()  (where string() is a filename (no path)
%%     {state,State} where State is a state returned by a previous filedialog
%% open|save dialog returns {ok,AbsFile,State}|{error,Reason}
%% A multiselect box returns directly and delivers messages on the form:
%%                            {select,AbsFile,State}|{close,State}

%% State contains filedialog internal information about for example the
%% current directory. This makes it possible to restart a new filedialog
%% from the same directory that the previous was closed from.

start(Options) ->
    NewOpt = set_state(Options),
    case gs:assq(type,NewOpt) of
	{value,multiselect} ->
	    spawn_link(?MODULE,multiselect_init,[self(),NewOpt]);
	{value,_Type} ->
	    spawn_link(?MODULE,open_save_init,[self(),NewOpt]),
	    receive
		{fd_result,X} ->
		    X
	    end
    end.
    
set_state(Opts) ->
    case gs:assq(state,Opts) of
	false -> Opts;
	{value,Dir} ->
	    case gs:assq(dir,Opts) of
		false ->
		    [{dir,Dir}|Opts];
		{value,_V} ->
		    Opts
	    end
    end.

make_window(Title, Options) ->
    _W = gs:window(win,gs:start(),[{configure,true},
				   {title,Title},{width,?WIDTH},
				   {height,?HEIGHT}]),
    Marg = {fixed,5},
    P = gs:frame(resizer, win,[{packer_x,[Marg,{stretch,1},Marg]},
			       {packer_y,[Marg,
					  {stretch,10},
					  {stretch,1,2*?BUTTH},
					  Marg]}]),
    _F = gs:frame(buttframe,resizer,[{packer_x,
				      [{stretch,1},{fixed,?BUTTW},{stretch,1},
				       {fixed,?BUTTW},{stretch,1},
				       {fixed,?BUTTW},{stretch,1}]},
				     {packer_y,[{stretch,1},
						{fixed,?BUTTH},
						{stretch,1}]},
				     {pack_x,2},{pack_y,3}]),
    {ok, DefaultDir} = file:get_cwd(),

    SaveOptions = get_save_options(Options),

    _FD = tool_genfd:create(P,[{dir, get_val_or_default(dir,
							Options,
							DefaultDir)},
			       {extensions,
				get_val_or_default(extensions,Options,[])},
			       {hidden,[]}|
			       SaveOptions]).    



get_save_options(Options) ->
    case gs:assq(type,Options) of
	{value, save} ->
	    [{save,true},
	     {file, get_val_or_default(file,
				       Options,
				       ?DEFAULT_SAVE_FILE)}];
	_Else -> [{save,false}]
    end.



%% ======================================================================
%% Below is the code for open/save.

-record(bstate,{file_dialog, old_wh, misc}).

%% Returns: {ok, AbsFile} | {error,Reason}
open_save_loop(State) ->
    #bstate{old_wh={OldW,OldH}} = State,
    receive
	{gs,win,destroy,_,_} ->
	    {error,close};
	{gs,cancel,click,_,_} ->
	    {error, cancel};
	{gs,win,configure,_,[OldW,OldH|_]} ->
	    open_save_loop(State);
	{gs,win,configure,_,[W,H|_]} ->
	    gs:config(resizer,[{width,W},{height,H}]),
	    open_save_loop(State#bstate{old_wh={W,H}});

	{gs,open_save,click,_,_} ->
	    R = tool_genfd:get_files(FD=State#bstate.file_dialog),
	    case R of
		{selection,{Dir,{[NewDir],[]}}} ->
		    tool_genfd:set_dir(FD,Dir++"/"++NewDir),
		    open_save_loop(State);
		{selection,{_Dir,{[],[]}}} ->
		    gs:config(open_save,beep),
		    open_save_loop(State);
		{selection,{Dir,{[],[File]}}} ->
		    AbsFile = Dir++"/"++File,
		    ValidateFun = State#bstate.misc,
		    case ValidateFun(AbsFile) of
			true ->
			    {ok,AbsFile,Dir};
			false ->
			    gs:config(open_save,beep),
			    open_save_loop(State)
		    end
	    end;
	{file_dialog,{selection,{Dir,{[],[File]}}}} ->
	    AbsFile = Dir++"/"++File,
	    ValidateFun = State#bstate.misc,
	    case ValidateFun(AbsFile) of
		true ->
		    {ok,AbsFile,Dir};
		false ->
		    open_save_loop(State)
	    end;
	Msg ->
	    io:format("loop got other:~p~n",[Msg]),
	    open_save_loop(State)
    end.


open_save_init(From, Options) ->
    {Title,Butt,Fun} = case gs:assq(type,Options) of
			   {value,open} ->
			       {"Open file", "Open", fun exists/1};
			   {value,save} ->
			       {"Save file", "Save", fun (_File) -> true end}
		       end,
    FD = make_window(Title,Options),
    gs:button(open_save,buttframe,[{label,{text,Butt}},{pack_x,2},{pack_y,2}]),
    _C=gs:button(cancel,buttframe,[{label,{text,"Cancel"}},
				   {pack_x,6},{pack_y,2}]),
    gs:config(win,{map,true}),
    gs:config(resizer,[{width,?WIDTH},{height,?HEIGHT}]),
    X=open_save_loop(#bstate{file_dialog=FD,misc=Fun,old_wh={?WIDTH,?HEIGHT}}),
    tool_genfd:close(FD),
    From ! {fd_result,X}.


    
exists(AbsFile) ->
    case file:read_file_info(AbsFile) of
	{ok,#file_info{type=regular}} ->
	    true;
	_ -> false
    end.
    
get_val_or_default(Key,Dict,Default) ->
    case gs:assq(Key,Dict) of
	{value,V} ->
	    V;
	_ -> Default
    end.

%% ======================================================================
%% Below is the code for the multiselect box.


multiselect_init(From,Options) ->
    FD = make_window("Select files",Options),
    gs:button(select,buttframe,[{label,{text,"Select"}},
				{pack_x,2},{pack_y,2}]),
    gs:button(all,buttframe,[{label,{text,"All"}},{pack_x,4},{pack_y,2}]),
    gs:button(close,buttframe,[{label,{text,"Done"}},{pack_x,6},{pack_y,2}]),
    gs:config(resizer,[{width,?WIDTH},{height,?HEIGHT}]),
    gs:config(win,{map,true}),
    multi_select_loop(#bstate{file_dialog=FD,old_wh={?WIDTH,?HEIGHT},
			      misc=From}),
    tool_genfd:close(FD).

multi_select_loop(State) ->
    #bstate{misc=Owner,old_wh={OldW,OldH}} = State,
    receive
	{gs,win,destroy,_,_} -> Owner ! say_bye(State);
	{gs,close,click,_,_} -> Owner ! say_bye(State);
	{gs,win,configure,_,[OldW,OldH|_]} ->
	    multi_select_loop(State);
	{gs,win,configure,_,[W,H|_]} ->
	    gs:config(resizer,[{width,W},{height,H}]),
	    multi_select_loop(State#bstate{old_wh={W,H}});

	{gs,select,click,_,_} ->
	    R=tool_genfd:get_files(FD=State#bstate.file_dialog),
	    case R of
		{selection,{Dir,{[NewDir],[]}}} ->
		    tool_genfd:set_dir(FD,Dir++"/"++NewDir),
		    multi_select_loop(State);
		{selection,{_Dir,{[],[]}}} ->
		    gs:config(select,beep),
		    multi_select_loop(State);
		{selection,{Dir,{[],[File]}}} ->
		    select_one(Owner,Dir,File, State)
	    end;
	{gs,all,click,_,_} ->
	    {selection,{Dir,{_Dirs,Files}}} =
		tool_genfd:get_all(State#bstate.file_dialog),
	    lists:foreach(
	      fun (File) ->
		      Owner ! {select,Dir++"/"++File}
	      end, Files),
	    Owner ! {close,Dir};
	{file_dialog,{selection,{Dir,{[],[File]}}}} ->
	    select_one(Owner,Dir,File,State);
	Msg ->
	    io:format("multi_select_loop got other:~p~n",[Msg]),
	    multi_select_loop(State)
    end.

say_bye(#bstate{file_dialog=FD}) ->
    {selection,{Dir,_}} = tool_genfd:get_files(FD),
    {close,Dir}.

select_one(Owner,Dir,File, State) ->
    AbsFile = Dir++"/"++File,
    case exists(AbsFile) of
	true ->
	    tool_genfd:hide(State#bstate.file_dialog,Dir,File),
	    Owner ! {select, AbsFile};
	false ->
	    gs:config(select,beep)
    end,
    multi_select_loop(State).
    
