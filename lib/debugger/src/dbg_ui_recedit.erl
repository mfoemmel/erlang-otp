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
%%% Purpose : To edit records in a more easy way through gs entries.
%%%           It also manages other terms except references, ports
%%%           and binaries.
%%% Notes   : The record editor can easily be extended to handle 
%%%           several bindings.
%%%----------------------------------------------------------------------

-module(dbg_ui_recedit).

-export ([
	  start/2,              % start (Bindings, Module)
	  update/2,             % update (RecEditPid, Bindings, Module)
	  extract_records/2     % extract_records (File, IncludePaths)
	 ]).

-export ([
	  init/3
	 ]).



-define (MIN_W, 250).   % min. win width
-define (MIN_H, 300).   % max. win height
    
-record (data, {attach_pid,       
		frames,           
		curr_frame,
		type,                     % record | term | eval
		records,                  % records in the file
		bindings,                 % current bindings
		win_data = {500,          % win width (A)
			    500,          % win height (B)
			    50,           % button frame height (C)
			    1,            % border witdh (D)
			    500-2*1,      % frame width == A-2*D 
			    500-25-50-2*1,% frame height == B-25-C-2*D
			    60}}).        % button width 


%%% start  /2
%%%
%%% There is one record editor for each process.
%%%
%%% Pre:
%%%    Binding   ==  [{Variable, Value}] 
%%%                  The current bindings from the stack in the debugger.
%%%    Variable  ==  atom ()
%%%                  The name of the variable.
%%%    Value     ==  term ()
%%%                  The value bound to Variable.
%%%    Module    ==  atom ()
%%%                  The current module at startup time.
%%%

start (Binding, Module) ->
    spawn_link (?MODULE, init, [Binding, Module, self ()]).



%%% update  /3
%%%
%%% update removes the old binding and shows the given binding
%%%
%%% Pre:
%%%    RecEditPid  ==  pid ()
%%%                    The pid for the record editor
%%%    Binding     ==  [{Variable, Value}]
%%%                    

update (RecEditPid, Bindings) ->
    RecEditPid ! {update, Bindings, self ()}.



%%% init  /3
%%%

init (Bindings, Module, AttachP) ->
    process_flag (trap_exit, true),    
    
    {ok, AbsFile} = dbg_idb:lookup (Module, mod_file),
    File = filename:basename (AbsFile),

    case extract_records ([AbsFile], []) of
	{error, Error1} ->
	    exit ('Incorrect module or include file');

	Records ->   % Records = [{File, RecordName, Fields}]

	    Data = #data{},
	    win_init (Data#data.win_data, File),
	    
	    {Frames, CurrFrame, Type} = frame_init (Bindings, Records,
						   Data#data.win_data),

	    loop (Data#data{attach_pid = AttachP,
			    frames = Frames,
			    curr_frame = CurrFrame,
			    type = Type,
			    records = Records,
			    bindings = Bindings})
    end.



%%% loop  /1
%%%

loop (Data) ->
    receive

	%% Update with new bindings
	%%

	{update, Bindings, CallingPid} when CallingPid == 
					    Data#data.attach_pid ->

	    delete_old_frames (Data#data.frames),
	    {Frames, CurrFrame, Type} = frame_init (Bindings, 
						    Data#data.records,
						    Data#data.win_data),
	    loop (Data#data{frames = Frames, 
			    curr_frame = CurrFrame,
			    type = Type,
			    bindings = Bindings});


	%% Menuitem clicked
	%%

	{gs, eval_menuitem, click, _, _} ->
	    gs:config (eval_frame, raise),
	    loop (Data#data{curr_frame = eval_frame, type = eval});

	{gs, _menuitem, click, {edit_item, {Type, RecordName,Val}},[Var |_]} ->
	    {Frames1, Frame1} =
		case frame_exists (Var, Data#data.frames) of
		    {true, Frame} ->
			{Data#data.frames, Frame};

		    false ->    % OBSOLETE !!??
			Frame = create_frame ({Type, RecordName, Var, Val},
					     Data#data.win_data),
			{[{Var, Frame} | Data#data.frames], Frame}
		end,

	    gs:config (Frame1, raise),
	    gs:config (bind_frame, raise),
	    loop (Data#data{frames = Frames1, curr_frame = Frame1, 
			    type = Type});



	%% Evaluate frame
	%%

	{gs, eval_button, click, _D, _A} ->
	    case Data#data.type of
		record ->
		    {Entries, _Frames, Record, Var} = 
			gs:read (Data#data.curr_frame, data), 

		    case catch read_entries (Entries) of
			{error, Error} ->
			    show_error (Error);

			Val ->
			    Cmd = lists:flatten (io_lib:format ("~s={~s,~s}", 
								[Var, Record, 
								 Val])),
			    send_data (Data#data.attach_pid, Cmd)
		    end;

		eval ->
		    Entry = gs:read (Data#data.curr_frame, data), 
		    Cmd = gs:read (Entry, text),
		    send_data (Data#data.attach_pid, Cmd);

		term ->
		    Entry = gs:read (Data#data.curr_frame, data), 
		    Var = gs:read (Entry, data),

		    case catch read_entries ([Entry]) of
			{error, Error} ->
			    show_error (Error);

			Val ->
			    Cmd = lists:flatten (io_lib:format ("~s=~s", 
								[Var, Val])),
			    send_data (Data#data.attach_pid, Cmd)
		    end
	    end,

	    loop (Data);


	%% Clear entries
	%%

	{gs, clear_button, click, _D, _A} ->
	    case Data#data.type of
		record ->
		    {Entries, _Frames, _Record, _Var} = 
			gs:read (Data#data.curr_frame, data), 
		    clear_entries (Entries);

		_Eval_Term ->
		    Entry = gs:read (Data#data.curr_frame, data),
		    clear_entries ([Entry])
	    end,
	    loop (Data);


	%% Keypress
	%%

	{gs, eval_entry, keypress, _,['Return' | _]} ->
	    Cmd = gs:read (eval_entry, text),
	    send_data (Data#data.attach_pid, Cmd),
	    loop (Data);


	%% Navigate through the frames
	%%

	{gs, _, click, {next_frame, Frame}, _} ->
	    gs:config (Frame, raise),
	    loop (Data);

	{gs, _, click, {prev_frame, Frame}, _} ->
	    gs:config (Frame, raise),
	    loop (Data);


	%% Close
	%% 

	{gs, close_info, click, _D, _A} ->
	    gs:config (info_label, {text, ""}),
	    gs:config (info_frame, lower),
	    loop (Data);

	{gs, _, click, _, ["Close" |_]} ->
	    exit (normal);

	{gs, win, destroy, _D, _Args} ->
	    exit (normal);


	%% Configure window
	%%

	{gs, win, configure, _D, [Width, Height | _T]}  ->
	    {WW, WH, _BTFH, _BW, _FW, _FH, _BTW} = Data#data.win_data,

	    NewData = 
		case {Width, Height} of 
		    {Width, Height} when WW == Width, WH == Height ->
			Data;
		    
		    _Resized ->
			resize_window (Data, Width, Height)
		end,

	    loop (NewData);


	{'EXIT', LinkedPid, _Reason} when LinkedPid == Data#data.attach_pid ->
	    exit (normal);


	Other ->
	    loop (Data)
    end.




%%%%%%%%%%  extract records (start)  %%%%%%%%%%


%%% extract_records  /2
%%%
%%% extract_records returns the defined records from 
%%% the given files and their included files.
%%% If the files are not in the current directory
%%% the absolute filename must be given.
%%% One must include the paths of the included files,
%%% unless they are in the current directory.
%%%
%%% The included files (e.g. ".hrl") are not to be listed in the 
%%% given files, they are automatic included by the program.
%%%
%%% extract_records does not support nested records. Instead
%%% it shows {nested_rec, Record_name}, see below.
%%%
%%% Pre:
%%%    Files         ==  [string ()]
%%%    IncludePaths  ==  [string ()]
%%%
%%% Def:
%%%    extract_records  ==  [{File, Record_name, Fields}]  ||  {error, Error}
%%%
%%%    File         ==  string ()
%%%    Record_name  ==  atom ()
%%%    Fields       ==  [{Attribute, Value}]
%%%    Attribute    ==  atom ()
%%%    Value        ==  term ()  ||  {nested_rec, Record_name}
%%%

extract_records (Files, IncludePaths) ->
    case file:get_cwd () of
	{ok, Cwd} ->
	    case catch extract_records (Files, [Cwd | IncludePaths], []) of
		{error, Error} ->
		    {error, Error};

		Records ->
		    Records
	    end;

	{error, Error} ->
	    {error, Error}
    end.


extract_records ([], _, L) ->
    lists:reverse (L);

extract_records ([File | T], IncludePaths, L) ->
    Fd = open_file (File, IncludePaths),

    case parse_file (Fd, File) of
	{error, Error} ->
	    throw ({error, Error});

	EppRecords ->
	    close_file (Fd),

	    case catch parse_records (EppRecords) of
		{error, Error} ->
		    throw ({error, Error});

		Records ->
		    extract_records (T, IncludePaths, 
				     lists:append (Records, L))
	    end
    end.




%%% open_file  /2
%%%

open_file (File, IncludePath) ->
    case epp:open (File, IncludePath) of
	{ok, Fd} ->
	    Fd;

	{error, Error} ->
	    throw ({error, Error})
    end.



%%% close_file  /1
%%%

close_file (Fd) ->
    case catch epp:close (Fd) of
	{'EXIT', Error} ->
	    throw ({error, Error});

	Else ->
	    ok
    end.



%%% parse_file  /2
%%%
%%% parse_file parses the given file and its included files
%%% and returns a list with the records. The record fields are
%%% returned in raw epp style.
%%%
%%% Example:
%%%   The record my_rec is defined in Fd.
%%%
%%%   -record (my_rec, {name, phone = 555}).
%%%
%%%   The expression epp:parse_erl_form(Fd) evaluates to:
%%%
%%%   {ok,{attribute,67,
%%%                  record,
%%%                  {my_rec,[{record_field,67,{atom,67,name}},
%%%                           {record_field,67,{atom,67,phone},
%%%                            {integer,67,555}}]}}}
%%%
%%% Pre:
%%%    Fd    ==  file descriptor
%%%    File  ==  string ()
%%%
%%% Def:
%%%    parse_file == [{File, Record, EppFields}]  ||  throw ({error, Error})
%%%
%%%    File       ==  string ()
%%%    Record     ==  string ()
%%%    EppFields  ==  list ()
%%%                   unparsed attributes and values
%%%

parse_file (Fd, File) ->
    parse_file (epp:parse_erl_form (Fd), Fd, File, []).


parse_file ({eof, _}, _Fd, _File, L) ->
    lists:reverse (L);

parse_file ({error, Error}, Fd, _File, _L) ->
    throw ({error, Error});

parse_file ({ok, {attribute, _, record, {Record, EppFields}}}, Fd, File,L) ->
    parse_file (epp:parse_erl_form (Fd), Fd, File, 
		[{File, Record, EppFields} | L]);

parse_file (_, Fd, File, L) ->
    parse_file (epp:parse_erl_form (Fd), Fd, File, L).



%%% parse_records  /1
%%%
%%% parse_records extracts the attributes and values from
%%% the given record fields.
%%% 

parse_records (RawRecords) ->
    parse_records (RawRecords, []).


parse_records ([], L) ->   
    L;

parse_records ([{File, RecordName, EppFields} | T], L) ->
    {Record, Fields} = parse_record ({RecordName, EppFields}), 
    parse_records (T, [{File, Record, Fields} | L]).



%%% parse_record  /1
%%%

parse_record ({Name, L}) ->
    {Name, parse_record (L, [])}.


parse_record ([], L) ->
    lists:reverse (L);

parse_record ([H | T], L) ->
    NameTuple = element (3, H),
    FieldName = element (3, NameTuple),

    ValueTuple = 
	case size (H) of
	    4 ->
		element (4, H);

	    _ ->
		undefined
	end,

    FieldValue = parse_value (ValueTuple),
    parse_record (T, [{FieldName, FieldValue} | L]).



%%% parse_value  /1
%%%

parse_value (undefined) ->
    undefined;

parse_value ({tuple, _, L}) ->
    make_tuple (L);

parse_value ({record, _, Name, L}) ->  % the record has a record == is nested
    {nested_rec, Name};

parse_value (Tuple) ->
    case element (1, Tuple) of
	cons ->
	    make_list (Tuple);

	nil ->
	    [];

	_Term ->
	    make_term (Tuple)
    end.



%%% make_term  /1
%%%

make_term (Term) ->
    case erl_parse:parse_term ([Term, {dot, 1}]) of
	{ok, Value} ->
	    Value;

	{error, Error} ->
	    throw ({error, Error})
    end.



%%% make_tuple  /1
%%%

make_tuple (L) ->
    make_tuple (L, []).


make_tuple ([], L) ->
    L1 = lists:reverse (L),
    list_to_tuple (L1);

make_tuple ([H | T], L) ->
    Term = parse_value (H),
    make_tuple (T, [Term | L]).



%%% make_list  /1
%%%

make_list (Tuple) ->
    make_list (Tuple, []).


make_list ({_cons, _, Tuple, {nil, _}}, L) ->
    Term = parse_value (Tuple),
    lists:reverse ([Term | L]);

make_list ({_cons, _, Tuple, Next}, L) ->
    Term = parse_value (Tuple),
    make_list (Next, [Term | L]).



%%%%%%%%%%  extract records  (end) %%%%%%%%%%



%%% frame_init  /3
%%%

frame_init (Bindings, Records, WinData) ->
    %% MatchedBindings  ==  [{record, RecordName, Variable, Fields}]
    %% Rest  ==  [{term, Variable, Value}]

    {MatchedBindings, _Rest} = match_bindings_with_records (Bindings,
							    Records),
    {Var1, Frame1, Type1} =
	case MatchedBindings of
	    [] ->   % Not a record
		{V, Val} = hd (Bindings),
		Var = lists:flatten (io_lib:format ("~s", [V])),
		win_update ([{term, Var, Val}]),
		Frame = create_frame ({term, undefined, Var, Val}, WinData),
		{Var, Frame, term};

	    _Matched ->
		{Type, RecordName, V, Val} = hd (MatchedBindings),
		Var = lists:flatten (io_lib:format ("~s", [V])),
		win_update (MatchedBindings),
		Frame = create_frame ({Type, RecordName, Var, Val}, WinData),
		{Var, Frame, Type}
	end,

    gs:config (Frame1, raise),
    gs:config (bind_frame, raise),

    {[{Var1, Frame1}], Frame1, Type1}.



%%% frame_exists  /2
%%%

frame_exists (Name, L) ->
    case lists:keysearch (Name, 1, L) of
	{value, {Name, Frame}} ->
	    {true, Frame};

	false ->
	    false
    end.



%%% read_entries  /1
%%%

read_entries (Entries) ->
    Val = read_entries1 (Entries, []),
    tl (Val).  % remove the first ","

read_entries1 ([], L) ->
    L;

read_entries1 ([H | T], L) ->
    Term = 
	case string_to_term (gs:read (H, text)) of
	    {ok, NewTerm} -> 
		NewTerm;

	    {error, Error} ->
		throw ({error, Error})
	end,

    read_entries1 (T, L ++ "," ++ Term).



%%% match_bindings_with_records  /2
%%%

match_bindings_with_records (Bindings, Records) ->
    {BindingTuples, NonTuples} = get_tuples (Bindings),
    {MatchedRecords, Rest} = match_records (BindingTuples, Records),
    {MatchedRecords, lists:append (NonTuples, Rest)}.



%%% match_records  /2
%%%
%%% match_records compares the name and the size of the bindings
%%%

match_records (TL, RL) ->     % TL - Tuple List, RL - Record List
    match_records (TL, RL, RL, [], []).


match_records ([], _, _, L, Rest) ->
    {lists:reverse (L), lists:reverse (Rest)};

match_records ([{Var, Val} | T], [], RL, L, Rest) ->
    match_records (T, RL, RL, L, [{term, Var, Val} | Rest]);

match_records ([{Var, Val} | T], [{_File, RecName, Fields} | TRL], RL, L,Rest) 
  when element (1, Val) == RecName,
       (size (Val) - 1) == length (Fields) ->

    %% ValList  ==  [{attr, value}]
    %%
    Val1 = tuple_to_list (Val),
    ValList = complete_binding (tl (Val1), Fields),  
    match_records (T, RL, RL, [{record, hd (Val1), Var, ValList} | L], Rest);

match_records (TL, [HRL | TRL], RL, L, Rest) ->
    match_records (TL, TRL, RL, L, Rest).



%%% complete_binding  /2
%%%
%%% complete_binding insert the attribute names
%%% from the defined record into the matched binding.
%%%

complete_binding (Values, Attributes) ->
    complete_binding (Values, Attributes, []).

complete_binding ([], _, L) ->
    lists:reverse (L);

complete_binding ([H | T1], [{Attr, _Def} | T2], L) ->
    complete_binding (T1, T2, [{Attr, H} | L]).



%%% win_init  /2
%%%

win_init ({WIN_W, WIN_H, BTF_H, BW, F_W, F_H, BUTT_W}, File) ->

    Gs = gs:start (),

    gs:window (win, Gs, [{x, 50}, {y, 50},
			 {width, WIN_W}, {height, WIN_H},
			 {title, io_lib:format ("Record Editor: ~p",
						[File])},
			 {configure, true}]),

    %% menu_bar
    %%

    gs:menubar (menu_bar, win, []),

    gs:menubutton (menu_butt_file, menu_bar, 
		   [{label, {text, " File "}}, {underline, 1}]),

    gs:menu (file_menu, menu_butt_file, []),

    gs:menuitem (close_menuitem, file_menu,
		 [{label, {text, "Close"}}, {underline, 0}]),


    gs:menubutton (menu_butt_edit, menu_bar, 
		   [{label, {text, " Edit "}}, {underline, 1}]),



    %% info_frame
    %%

    gs:frame (info_frame, win,
	      [{x, 0}, {y, 25}, {width, F_W}, {height, F_H}, 
	       {bw, BW}]),

    gs:label (info_label, info_frame, 
	      [{x, 0}, {y, 20}, {width, F_W - 10}, {height, F_H - 40},
	       {label, {text, ""}}, {bw, BW}]),

    gs:button (close_info, info_frame, 
	       [{x, F_W - BUTT_W - 10}, {y, F_H - 40}, 
		{width, BUTT_W}, {label, {text, "Close"}}]),



    %% eval_frame
    %%

    gs:frame (eval_frame, win, 
	      [{x, 0}, {y, 25}, {width, F_W}, {height, F_H}, 
	       {data, eval_entry}, {bw, BW}]),

    gs:label (eval_label, eval_frame, 
	      [{x, 13}, {y, 10}, {label, {text, "Evaluate:"}},
	       {align, w}]),

    gs:entry (eval_entry, eval_frame,
	      [{x, 10}, {y, 40}, {width, F_W - 20}, {keypress, true}]),




    %% bind_frame
    %%

    gs:frame (bind_frame, win, 
	      [{x, 0}, {y, 25}, {width, F_W}, {height, F_H}]),



    %% button_frame
    %%

    gs:frame (button_frame, win, 
	      [{x, 0}, {y, WIN_H - BTF_H}, {width, F_W}, {height, BTF_H},
	       {bw, BW}]),

    RowWidth = BUTT_W * 3 + 20 * 2,

    gs:button (eval_button, button_frame, 
	       [{x, F_W / 2 - RowWidth / 2}, {y, 10}, 
		{width, BUTT_W}, {label, {text, "Update"}}]),

    gs:button (clear_button, button_frame, 
	       [{x, F_W / 2  - RowWidth / 2 + BUTT_W + 20}, {y, 10}, 
		{width, BUTT_W}, {label, {text, "Clear"}}]),

    gs:button (close_button, button_frame, 
	       [{x, F_W / 2 + RowWidth / 2 - BUTT_W}, {y, 10}, 
		{width, BUTT_W}, {label, {text, "Close"}}]),

    gs:config (win, {map, true}).






%%% resize_window  /3
%%%

resize_window (Data, Width, Height) when Width > ?MIN_W, Height > ?MIN_H ->
    {_WW, _WH, BTFH, BW, _FW, _FH, BTW} = Data#data.win_data,
    NewFW = frame_width (Width, BW),
    NewFH = frame_height (Height, BTFH, BW), 
    NewData = Data#data{win_data = {Width, Height, BTFH, BW, 
				    NewFW, NewFH, BTW}}, 

    resize_button_frame (Height, BTFH, NewFW, BTW),
    gs:config (bind_frame, [{width, NewFW}, {height, NewFH}]),
    gs:config (eval_frame, [{width, NewFW}, {height, NewFH}]),
    gs:config (info_frame, [{width, NewFW}, {height, NewFH}]),
    gs:config (eval_entry, {width, NewFW - 20}),

    delete_old_frames (Data#data.frames),
    {Frames, CurrFrame, Type} = frame_init (NewData#data.bindings, 
					    NewData#data.records, 
					    NewData#data.win_data),
    
    NewData#data{frames = Frames, curr_frame = CurrFrame, 
		 type = Type};


resize_window (Data, Width, Height) ->
    {_WW, _WH, BTFH, BW, _FW, _FH, BTW} = Data#data.win_data,

    {NewWW, NewWH, NewFW, NewFH} =
	case {Width, Height} of
	    {Width, Height} when Width < ?MIN_W, Height < ?MIN_H ->
		gs:config (win, [{width,?MIN_W}, {height, ?MIN_H}]),
		{?MIN_W, ?MIN_H, 
		 frame_width (?MIN_W, BW), frame_height (?MIN_H, BTFH, BW)};
 
	    {Width, Height} when Width > ?MIN_W ->
		gs:config (win, [{width, Width}, {height, ?MIN_W}]),
		{Width, ?MIN_H,
		 frame_width (Width, BW), frame_height (?MIN_H, BTFH, BW)};

	    _Else ->
		gs:config (win, [{width,?MIN_W}, {height, Height}]),
		{?MIN_W, Height,
		 frame_width (?MIN_W, BW), frame_height (Height, BTFH, BW)}
	end,

    NewData = Data#data{win_data = {NewWW, NewWH, BTFH, BW, 
				    NewFW, NewFH, BTW}}, 
    delete_old_frames (Data#data.frames),
    {Frames, CurrFrame, Type} = frame_init (NewData#data.bindings, 
					    NewData#data.records, 
					    NewData#data.win_data),
    resize_button_frame (NewWH, BTFH, NewFW, BTW),
    gs:config (bind_frame, [{width, NewFW}, {height, NewFH}]),
    gs:config (eval_frame, [{width, NewFW}, {height, NewFH}]),
    gs:config (info_frame, [{width, NewFW}, {height, NewFH}]),
    gs:config (eval_entry, {width, NewFW - 20}),

    NewData#data{frames = Frames, curr_frame = CurrFrame, type = Type}.



%%% resize_button_frame  /4
%%%

resize_button_frame (WH, BTFH, FW, BTW) ->
    gs:config (button_frame, [{y, WH - BTFH}, {width, FW}]),
    RowWidth = BTW * 3 + 20 * 2,
    gs:config (eval_button, {x, FW / 2 - RowWidth / 2}),
    gs:config (clear_button, {x, FW / 2  - RowWidth / 2 + BTW + 20}),
    gs:config (close_button, {x, FW / 2 + RowWidth / 2 - BTW}).



%%% win_update  /1
%%%

win_update (Bindings) ->
    update_edit_menu (Bindings).



%%% update_edit_menu  /1
%%%

update_edit_menu (Bindings) ->
    gs:destroy (edit_menu), 
    gs:menu (edit_menu, menu_butt_edit, []),
    gs:menuitem (eval_menuitem, edit_menu,
		 [{label, {text, "Evaluate"}}, {underline, 0}]),
    insert_bindings (Bindings).



%%% insert_bindings  /1
%%%
%%% The menu items carries some data so when clicking
%%% on it it knews what variable to look at.
%%%

insert_bindings ([]) ->
    ok;

insert_bindings (Bindings) ->
    gs:menuitem (edit_menu, [{itemtype, separator}]),
    insert_bindings_1 (Bindings).


insert_bindings_1 ([]) ->
    ok;

insert_bindings_1 ([{Type, Var, Data} | T]) ->
    gs:menuitem (edit_menu, 
		 [{label, {text, io_lib:format ("~s", [Var])}},
		  {data, {edit_item, {Type, [], Data}}}]),
    insert_bindings_1 (T);

insert_bindings_1 ([{Type, Record, Var, Data} | T]) ->
    gs:menuitem (edit_menu, 
		 [{label, {text, io_lib:format ("~s", [Var])}},
		  {data, {edit_item, {Type, Record, Data}}}]),
    insert_bindings_1 (T).



%%% create_frame  /2
%%%
%%% The frames have its data in the 'data' option
%%%

create_frame (FrameData, WinData) ->
    create_frame1 (FrameData, WinData).


create_frame1 ({term, _, Var, Val}, {_WW,_WH,_BTFH, BW, FW, FH, BTW}) ->
    Id = gs:frame (bind_frame,
		   [{x, 0}, {y, 0}, 
		    {width, FW}, {height, FH}, {bw, BW}]),

    [Entry] = insert_entries ([Id], 1, 0, 1, 1, "", [{Var, Val}], 
			      [], FW, FH, BTW),

    gs:config (Id, {data, Entry}),
    Id;


create_frame1 ({record, Record, Var, Fields}, 
	       {_WW,_WH,_BTFH, BW, FW, FH, BTW}) ->
    Rows = (FH div 60) - 2,                % -2 to have place for nav. arrows
    NoOfEntries = length (Fields),

    NoOfFrames = 
	case (NoOfEntries rem Rows) == 0 of
	    true ->
		NoOfEntries div Rows;

	    false ->
		(NoOfEntries div Rows) + 1
	end,

    FrameList = create_record_frames (NoOfFrames, [], FW, FH, BW),
    Entries = insert_entries (FrameList, 1, 0, Rows, NoOfEntries, 
			      Var, Fields, [], FW, FH, BTW),

    gs:config (hd (FrameList), {data, {Entries, tl (FrameList), Record, Var}}),
    hd (FrameList). 



%%% create_record_frames  /5
%%%

create_record_frames (0, L, _FW, _FH, _BW) ->
    lists:reverse (L);

create_record_frames (N, L, FW, FH, BW) ->
    FrameId = gs:frame (bind_frame, 
			[{x, 0}, {y, 0}, {width, FW}, {height, FH}, {bw, BW}]), 
    create_record_frames (N - 1, [FrameId | L], FW, FH, BW).



%%% insert_entries  /11
%%%
%%% FL      ==  list of frames
%%% CF      ==  current frame
%%% Rows    ==  number of rows in a frame
%%% Rows    ==  
%%% NOE     ==  total number of entries
%%% Var     ==  variabel that the record is bound to 
%%%             when not record Var ==  ""
%%% Fields  ==  [{Attribute, Val}]
%%% EL      ==  entry list
%%% FW      ==  frame width
%%% FH      ==  frame hight
%%% BTW     ==  button width
%%%

%%% No more entries to create (NOE == 0), finish it off.
%%% Return the collected entries (EntryList).
%%%

insert_entries (FL, CF, _, _, 0, Var, _, EntryList, FW, FH, BTW) ->
    gs:label (lists:nth (CF, FL), 
	      [{x, 0}, {y, 5}, {width, FW - 5},
	       {label, {text, io_lib:format ("~s", [Var])}}]),

    case CF == 1 of
	true ->
	    ok;

	false ->
	    gs:button (lists:nth (CF, FL),
		       [{x, FW / 2 - 10 - BTW / 2}, {y, FH - 50}, 
			{width, BTW / 2}, {height, 22},
			{label, {text, " <  "}}, 
			{data, {prev_frame, lists:nth (CF - 1, FL)}}]),

	    gs:button (lists:nth (CF, FL),
		       [{x, FW / 2 + 10}, {y, FH - 50}, 
			{width, BTW / 2},{height, 22},
			{label, {text, "  > "}}, 
			{enable, false}])
    end,

    lists:reverse (EntryList);


%%% The frame CF is full with entries (Rows = Rows), 
%%% start with a new one
%%%

insert_entries (FL, CF, Rows, Rows, NOE, Var, Fields, EL, FW, FH, BTW) ->
    gs:label (lists:nth (CF, FL), 
	      [{x, 0}, {y, 5}, {width, FW - 5},
	       {label, {text, io_lib:format ("~s", [Var])}}]),

    case length (FL) == CF of

	%% only one frame
	%%

	true when CF == 1 ->
	    ok;

	%% first frame
	%%

	false when CF == 1 ->
	    gs:button (lists:nth (CF, FL),
		       [{x, FW / 2 - 10 - BTW / 2}, {y, FH - 50}, 
			{width, BTW / 2}, {height, 22},
			{label, {text, " <  "}}, 
			{enable, false}]),

	    gs:button (lists:nth (CF, FL),
		       [{x, FW / 2 + 10}, {y, FH - 50}, 
			{width, BTW / 2},{height, 22},
			{label, {text, "  > "}}, 
			{data, {next_frame, lists:nth (CF + 1, FL)}}]);

	%% frames in between
	%%

	_ ->
	    gs:button (lists:nth (CF, FL),
		       [{x, FW / 2 - 10 - BTW / 2}, {y, FH - 50}, 
			{width, BTW / 2}, {height, 22},
			{label, {text, " <  "}}, 
			{data, {prev_frame, lists:nth (CF - 1, FL)}}]),

	    gs:button (lists:nth (CF, FL),
		       [{x, FW / 2 + 10}, {y, FH - 50}, 
			{width, BTW / 2},{height, 22},
			{label, {text, "  > "}}, 
			{data, {next_frame, lists:nth (CF + 1, FL)}}])
    end,

    insert_entries (FL, CF + 1, 0, Rows, NOE, Var, Fields, EL, FW, FH, BTW);


%%% The value (Val) is a list that could be a list or a string.
%%%

insert_entries (FL, CF, Row, Rows, NOE, Var, [{Attr, Val} | T], EL, FW,FH, BTW) 
  when list (Val) ->
    gs:label (lists:nth (CF, FL), 
	      [{x, 10}, {y, 30 + Row * 60},
	       {label, {text, io_lib:format ("~s =", [Attr])}},
	       {align, w}]),

    Val2 = io_lib:format ("~p", [Val]),

    EId = gs:entry (lists:nth (CF, FL),
		    [{x, 20}, {y, 60 + Row * 60 - 5}, {width, FW - 30}, 
		     {data, Attr}, {text, Val2}, 
		     {keypress, true}]),

    insert_entries (FL, CF, Row +1, Rows, NOE -1, Var, T, [EId|EL], FW, FH,BTW);


insert_entries (FL, CF, Row, Rows, NOE, Var, [{Attr, Val} | T], EL,FW,FH,BTW) ->
    gs:label (lists:nth (CF, FL), 
	      [{x, 10}, {y, 30 + Row * 60},
	       {label, {text, io_lib:format ("~s =", [Attr])}},
	       {align, w}]),

    EId = gs:entry (lists:nth (CF, FL),
		    [{x, 20}, {y, 60 + Row * 60 - 5}, 
		     {width, FW - 30}, 
		     {data, Attr}, {text, Val}, 
		     {keypress, true}]),
    
    insert_entries (FL, CF, Row +1, Rows, NOE -1, Var, T, [EId | EL],FW,FH,BTW).



%%% get_tuples  /1
%%%
%%% Returns the tuples from the given list
%%%

get_tuples (L) ->
    get_tuples (L, [], []).


get_tuples ([], Tuples, Rest) ->
    {lists:reverse (Tuples), lists:reverse (Rest)};

get_tuples ([H | T], Tuples, Rest) when tuple (element (2, H)) ->
    get_tuples (T, [H | Tuples], Rest);

get_tuples ([{Var, Val} | T], Tuples, Rest) ->
    get_tuples (T, Tuples, [{term, Var, Val} | Rest]).



%%% send_data  /2
%%%

send_data (_AttachPid, []) ->
    ok;

send_data (AttachPid, Cmd) ->
    AttachPid ! {term_edit, Cmd}.



%%% string_to_term  /1
%%% 
%%% string_to_term tries to parse the given string into an
%%% erlang term.
%%%
%%% OBS!! It doesn't cover ports, references and binaries
%%%
%%% (modyfied from tv_db_search.erl)
%%%

string_to_term ("") ->
    {ok, "undefined"};

string_to_term (Str) ->
    case catch erl_scan:string(Str ++ ". ") of
	{ok, ScannedStr, _No} ->
	    case erl_parse:parse_term(ScannedStr) of
		{ok, Term} ->
		    {ok, lists:flatten (io_lib:format ("~w", [Term]))};

		Other ->
		    %% May be a PID, have to check this, since erl_scan
		    %% currently cannot handle this case...  :-(

		    case parse_pid (Str) of
			{ok, Term} ->
			    {ok, Term};

			_Error ->
			    {_error, {_no, _erl_parse, Error}} = Other,
			    {error, {Str, Error}}
		    end
	    end;

	Error ->
	    {_error, {_no, _erl_scan,{_string,_no1_, Error1}}, _no2} = Error,
	    {error, {Str, Error1}}
    end.



%%% pars_pid  /1
%%%

parse_pid (Str) ->
    case regexp:matches (Str, "\<[0-9]+\.[0-9]+\.[0-9]+\>") of
	{match, []} ->
	    no_match_of_pid;

	{match, L} ->
	    parse_pid (Str, L, 0);

	Error ->
	    Error
    end.


parse_pid (Str, [], _) ->
    {ok, Str};

parse_pid (Str, [{Pos, Length} | T], AddOn) ->
    StartStr = string:substr (Str, 1, Pos - 1 + AddOn),
    PidStr = string:substr (Str, Pos + AddOn, Length),
    RestStr = string:substr (Str, Pos + Length + AddOn),

    NewPidStr = "list_to_pid(\"" ++ PidStr ++ "\")",
    parse_pid (StartStr ++ NewPidStr ++ RestStr, T, 
	       AddOn + string:len ("list_to_pid(\"\")")).



%%% clear_entries  /1
%%%

clear_entries ([]) ->
    ok;

clear_entries ([H | T]) ->
    Length = string:len (gs:read (H, text)),
    gs:config (H, {delete, {0, Length}}),
    clear_entries (T).



%%% show_error  /1
%%%

show_error ({Str, Error}) ->
    gs:config (info_label, 
	       {label, {text, io_lib:format("Error in ~p:\n~p",
					    [Str, Error])}}),
    gs:config (info_frame, raise).



%%% frame_height  /3
%%%

frame_height (WH, BTFH, BW) ->
    WH - 25 - BTFH - 2*BW.



%%% frame_width  /3
%%%

frame_width (WW, BW) ->
    WW - 2*BW.



%%% delete_old_frames  /1
%%%
%%% delete_old_frames deletes the gs frames in the given list
%%%
%%% Pre:
%%%    L  ==  [{Var, Frame}]
%%%

delete_old_frames (L) ->
    lists:map (fun ({_Var, Frame}) -> 
		       gs:config (Frame, {delete, true}) end,
	       L).
	       
