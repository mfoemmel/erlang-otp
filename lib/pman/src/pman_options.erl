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

-module(pman_options).

-compile(export_all).
-export([read_from_file/1,
	 save_to_file/2,
	 dialog/3]).


%%
%% Graphical resources
%%

-define(WIN_WIDTH,350).
-define(WIN_HEIGHT,350).

-define(TOP_WINDOW,xx_pman_option_window_xx).
-define(TOP_FRAME,xx_pman_top_frame_xx).

-include("pman_options.hrl").

-record(loop_data, {resize_frame,
		    calling_pid}).



loop(LoopData) ->
    receive
	{gs,_Id,destroy,_Data,_Arg} -> 
	    LoopData#loop_data.calling_pid ! {self (), {error,destroyed}};

	{gs, ?TOP_WINDOW, configure, _Data, [W, H | _]} ->
	    gse:config(LoopData#loop_data.resize_frame,
		       [{width,W},{height,H}]), % repack
	    loop(LoopData);

	{gs, ok_button,click,_Data,_Arg} ->
	    Options=get_options_from_window(),
	    gse:unmap(?TOP_WINDOW),
	    LoopData#loop_data.calling_pid ! {self (), Options};

	{gs, cancel_button,click,_Data,_Arg} ->
	    gse:unmap(?TOP_WINDOW),
	    LoopData#loop_data.calling_pid ! {self (), {error,cancelled}};

	{gs, trace_spawn, click, _Data,[_Text,_,Value]} ->
	    group_radio(Value, trace_spawn_all, [trace_spawn_all,
						 trace_spawn_first]),
	    loop(LoopData);

	{gs, trace_link, click, _Data,[_Text,_,Value]} ->
	    group_radio(Value, trace_link_all, [trace_link_all,
						trace_link_first]),
	    loop(LoopData);

	{gs, trace_in_window, click, _Data, _Arg} ->
	    lists:foreach({gse,disable},[trace_file, trace_file_browse]),
	    loop(LoopData);

	{gs, trace_to_file, click, _Data, [_Text,_,Value]} ->
	    lists:foreach({gse,enable},[trace_file, trace_file_browse]),
	    loop(LoopData);

	{gs, trace_file_browse, click,_Data,_Arg} ->
	    Result = tool_utils:file_dialog([{type,save},
					     {file, "Untitled.log"}]),
	    case Result of
		{error, _Reason} ->
		    loop(LoopData);
		{ok, Name,_State} ->
		    gse:config(trace_file, [{text, Name}]),
		    loop(LoopData)
	    end;

	Other ->
	    %% io:format("loop got: ~p~n",[Other]),
	    loop(LoopData)
    end.

group_radio(Value, Default, GroupList) ->
    case Value of
	true ->
	    gse:select(Default),
	    lists:foreach({gse,enable}, GroupList);
	false ->
	    lists:foreach({gse,deselect}, GroupList),
	    lists:foreach({gse,disable}, GroupList)
    end.




-define(LabelDefaults,[{justify,left},
		       {align,w}]).
-define(ButtonDefaults,[{justify,left},
			{align,w}]).



%%
%% Create a window, or return a value indicating that is is already created.
%% 

dialog(Parent, Title, Options) ->
    Pid = spawn (?MODULE, dialog, [self (), Parent, Title, Options]),

    receive
	{Pid, Value} ->
	    Value
    end.

dialog(Calling_Pid, Parent, Title, undefined) ->
    dialog(Calling_Pid, Parent, Title, #trace_options{});

dialog(Calling_Pid, Parent, Title, Options) ->

    %% Check if the dialog has already been created, in that 
    %% case, we can reuse it. Otherwise a new dialog is created.

    case gse:name_occupied(?TOP_WINDOW) of
	false -> make_window(Parent, Title);
	true -> ok
    end,

    %% Window has now been created or may be re-used

    update_window_from_options(Options),

    gse:resize(?TOP_FRAME,?WIN_WIDTH,?WIN_HEIGHT),
    gse:map(?TOP_WINDOW),

    

    loop(#loop_data{resize_frame=?TOP_FRAME, calling_pid = Calling_Pid}).



make_window(Parent, Title) ->

    gse:named_window(?TOP_WINDOW,
		     Parent,[{title,Title},
			     {configure,true},
			     {width, ?WIN_WIDTH},
			     {height, ?WIN_HEIGHT}
			    ]),

    gse:named_frame(?TOP_FRAME,?TOP_WINDOW, [{bw,3},%{bg,white},
				       {packer_x,[{stretch,1,175},
						  {stretch,1,175}]},
				       {packer_y,[{stretch,3},
						  {stretch,2},
						  {stretch,1}]}]),
%%================================================================
    F11 = gse:frame(?TOP_FRAME,[{bw,3},%{bg,yellow},
				{pack_xy,{1,1}},
				{packer_x,[{stretch,1},
					   {stretch,20},
					   {stretch,2}
					  ]},
				{packer_y,[{stretch,2},
					   {stretch,1},
					   {stretch,1},
					   {stretch,1},
					   {stretch,1},
					   {stretch,1},
					   {stretch,1},
					   {stretch,1}
					  ]}]),

    gse:label(F11,[{pack_xy,{2,1}},
			{label,{text,"Trace output options:"}}| ?LabelDefaults
		       ]),

    gse:named_checkbutton(trace_send, F11, [{pack_xy,{2,2}},
			 {label,{text, "Trace send"}}|?ButtonDefaults]),
    gse:named_checkbutton(trace_receive, F11, [{pack_xy,{2,3}},
			 {label,{text, "Trace receive"}}|?ButtonDefaults]),
    gse:named_checkbutton(trace_functions,F11, [{pack_xy,{2,4}},
			 {label,{text, "Trace functions"}}|?ButtonDefaults]),
    gse:named_checkbutton(trace_events,F11, [{pack_xy,{2,5}},
			 {label,{text, "Trace events"}}|?ButtonDefaults]),


%%================================================================
    F21 = gse:frame(?TOP_FRAME,[{bw,3},%{bg,red},
			  {pack_xy,{2,1}},
			   {packer_x,[{stretch,1},
				      {stretch,2},
				      {stretch,2},
				      {stretch,20},
				      {stretch,1}
				     ]},
			   {packer_y,[{stretch,2},
				      {stretch,1},
				      {stretch,1},
				      {stretch,1},
				      {stretch,1},
				      {stretch,1},
				      {stretch,1},
				      {stretch,1},
				      {stretch,1}
				     ]}]),


    gse:label(F21,[{pack_xy,{{2,4},1}},
		  {label,{text,"Inheritance options:"}}| ?LabelDefaults
		 ]),

    gse:named_checkbutton(trace_spawn,F21, [{pack_xy,{{2,4},2}},
			 {data,trace_send},
			 {label,{text, "Inherit on spawn"}}|?ButtonDefaults]),
    gse:named_radiobutton(trace_spawn_all,F21, [{pack_xy,{{3,4},3}},
			 {group,spawn},
			 {data,trace_receive},
			 {label,{text, "All spawns"}}|?ButtonDefaults]),
    gse:named_radiobutton(trace_spawn_first,F21, [{pack_xy,{{3,4},4}},
			 {group,spawn},
			 {data,trace_receive},
			 {label,{text, "First spawn only"}}|?ButtonDefaults]),
    gse:named_checkbutton(trace_link,F21, [{pack_xy,{{2,4},6}},
			 {data,trace_send},
			 {label,{text, "Inherit on link"}}|?ButtonDefaults]),
    gse:named_radiobutton(trace_link_all,F21, [{pack_xy,{{3,4},7}},
			 {group,link},
			 {data,trace_receive},
			 {label,{text, "All links"}}|?ButtonDefaults]),

    gse:named_radiobutton(trace_link_first,F21, [{pack_xy,{{3,4},8}},
			 {group,link},
			 {data,trace_receive},
			 {label,{text, "First link only"}}|?ButtonDefaults]),


%%================================================================

    F12 = gse:frame(?TOP_FRAME,[{bw,3},%{bg,blue},
			  {pack_xy,{{1,2},2}},
			  {packer_x,[{stretch,1},
				    {stretch,5}, %Label
				    {stretch,1},
				    {stretch,10}, %Field
				    {stretch,1},
				    {stretch,5}, %Button
				    {stretch,1}]},
			  {packer_y,[{stretch,2},
				     {stretch,1},
				     {stretch,1},
				     {stretch,1}]}]),
    
    gse:label(F12,[{pack_xy,{{2,6},1}},
		  {label,{text,"Trace output options:"}}|?LabelDefaults]),
    gse:named_radiobutton(trace_in_window,F12, [{pack_xy,{{2,6},2}},
			 {group, trace_dest},
			 {label,{text, "In window"}}|?ButtonDefaults]),
    gse:named_radiobutton(trace_to_file,F12, [{pack_xy,{2,3}},
			 {group, trace_dest},
			 {label,{text, "To file"}}|?ButtonDefaults]),
    gse:named_entry(trace_file,F12, [{pack_xy,{4,3}}]),
    gse:named_button(trace_file_browse,F12,[{pack_xy,{6,3}},
		   {label,{text," Browse..."}}|?ButtonDefaults]),
    





%%================================================================

    F13 = gse:frame(?TOP_FRAME,[{bw,3},%{bg,green},
			  {pack_xy,{{1,2},3}},
				{packer_x,[{stretch, 1},
					   {fixed, 60},
					   {stretch, 1},
					   {fixed, 60},
					   {stretch, 1}]},
			  {packer_y,[{stretch,1},
				    {fixed, 30},
				    {stretch,1}]}]),
    
    

    
    gse:named_button(ok_button,F13,[{pack_xy,{2,2}},
		   {label,{text,"OK"}}]),
    gse:named_button(cancel_button,F13,[{pack_xy,{4,2}},
		   {label,{text,"Cancel"}}]).


update_window_from_options(Options) ->
    %% Trace output
    gse:config(trace_send,[{select,Options#trace_options.send}]),
    gse:config(trace_receive,[{select,Options#trace_options.treceive}]),
    gse:config(trace_functions,[{select,Options#trace_options.functions}]),
    gse:config(trace_events,[{select,Options#trace_options.events}]),

    %% Trace inheritance
    case (Options#trace_options.inherit_on_all_spawn or
	  Options#trace_options.inherit_on_1st_spawn) of
	true ->
	    gse:select(trace_spawn),
	    gse:config(trace_spawn_all,
		       [{select,Options#trace_options.inherit_on_all_spawn}]),
	    gse:config(trace_spawn_first,
		       [{select, Options#trace_options.inherit_on_1st_spawn}]);
	false ->
	    lists:foreach({gse,disable}, [trace_spawn_all,trace_spawn_first])
    end,

    case (Options#trace_options.inherit_on_all_link or
	  Options#trace_options.inherit_on_1st_link) of
	true -> gse:select(trace_link),
	    gse:config(trace_link_all,
		      [{select,Options#trace_options.inherit_on_all_link}]),
	    gse:config(trace_link_first,
		       [{select, Options#trace_options.inherit_on_1st_link}]);
	false ->
	    lists:foreach({gse, disable}, [trace_link_all,trace_link_first])
    end,
    

    %% Trace ouput destinations

    gse:config(trace_in_window,[{select,(not Options#trace_options.to_file)}]),

    gse:config(trace_to_file,[{select,Options#trace_options.to_file}]),
    gse:config(trace_file, [{text, Options#trace_options.file}]),
    case Options#trace_options.to_file of
	true ->
	    ok;
	false ->
	    lists:foreach({gse, disable}, [trace_file, trace_file_browse])
    end.




get_options_from_window() ->
    #trace_options{send = gse:read(trace_send,select),
		   treceive = gse:read(trace_receive,select),
		   functions = gse:read(trace_functions,select),
		   events = gse:read(trace_events,select),
		   inherit_on_1st_spawn = gse:read(trace_spawn_first,select),
		   inherit_on_all_spawn = gse:read(trace_spawn_all,select),
		   inherit_on_1st_link = gse:read(trace_link_first,select),
		   inherit_on_all_link = gse:read(trace_link_all,select),
		   to_file = gse:read(trace_to_file,select),
		   file = gse:read(trace_file,text)}.

		
		    
	


%%
%% read_from_file(File)
%%     returns the options saved in File.
%%     If no options ca be found, then the default options are
%%     returned.



read_from_file(File)->
    case file:file_info(File) of	
	{ok, {_Size,regular,read_write,_AccessTime,_ModifyTime,_,_}} ->
	    read_from_ok_file(File);
	_Else -> {default, file, #trace_options{}}
    end.

read_from_ok_file(File) ->
    case file:consult(File) of
	{ok, [Term]} ->
	    %% This is the case when options can actually be read from
	    %% the file, all others result in program default options.
	    if record(Term,trace_options) ->
		    Term;
	       true -> {default, format, #trace_options{}}
	    end;
	{ok, []} -> {default,format,#trace_options{}};
	{ok, [Term1,Term2|Rest]} -> {default,format,#trace_options{}};
	{error, read} -> {default, format, #trace_options{}};
	{error, open} -> {default, file, #trace_options{}};
	_Anything -> {default, file, #trace_options{}}
    end.

%%
%% save_to_file(Options,File)
%%
%% Problems with saving will result in an exit.
%%

save_to_file(Options, File) ->

    case file:open(File,[write]) of
	{error, Reason} ->
	     exit({file_problem, File});
	{error, Reason} ->
	     exit({file_problem, File});
	{ok, Fd} ->
	    {{Year,Month,Day},{H,M,S}} = calendar:local_time(),
	    io:format(Fd,"%%%~n",[]),
	    io:format(Fd,"%%% File: ~s~n",[File]),
	    io:format(Fd,"%%% Date: ~w-~2..0w-~2..0w, ~2..0w:~2..0w:~2..0w~n",
		      [Year,Month,Day,H,M,S]),
	    io:format(Fd,"%%%~n",[]),
	    io:format(Fd,"%%% This file was created by PMAN. ~n",[]),
	    io:format(Fd,"%%%~n",[]),
	    io:format(Fd,"%%% DO NOT EDIT! ~n",[]),
	    io:format(Fd,"%%%~n",[]),
	    io:format(Fd,"%%%~n",[]),
	    io:format(Fd,"~p.~n",[Options]),
	    file:close(Fd),
	    true
    end.
