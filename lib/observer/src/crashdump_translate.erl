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
-module(crashdump_translate).

%% 
%% This module translates an old erlang crash dump (pre OTP R10) to
%% the new format with tags that can be read by the crashdump viewer
%% tool.
%%

%% API
%-compile(export_all).
-export([old2new/2]).

-define(divider,"--------------------------------------------------\n").
-define(chunk_size,1000). % number of bytes read from crashdump at a time
-define(max_line_size,500). % max number of bytes (i.e. characters) the
			    % line/1 function can return

%%%-----------------------------------------------------------------
%%% Translate an old dump to a newer format
old2new(ReadFile,WriteFile) ->
    case {file:open(ReadFile,[read,read_ahead,raw,binary]),
	  file:open(WriteFile,[write,delayed_write,raw,binary])} of
	{{ok,Read},{ok,Write}} -> 
	    case catch translate(Read,Write) of
		{'EXIT',_Reason} = EXIT -> 
		    exit(EXIT);
		_other -> % thown if dump is truncated
		    erase(chunk),
		    file:close(Read),
		    file:close(Write)
	    end;
	{{ok,Read},Error} -> 
	    file:close(Read),
	    {error,{open,WriteFile,Error}};
	{Error,{ok,Write}} -> 
	    file:close(Write),
	    {error,{open,ReadFile,Error}};
	{ReadError,WriteError} -> 
	    {error,{open,{ReadFile,ReadError},{WriteFile,WriteError}}}
    end.

%%%-----------------------------------------------------------------
%%% Internal functions
translate(Read,Write) ->
    {Truncated,InputLine} = 
	case line(Read) of
	    {eof,InputLine0} -> {true,InputLine0};
	    InputLine0 -> {false,InputLine0}
	end,
    case InputLine of
	"<Erlang crash dump>\n" ->
	    write(Write,"=erl_crash_dump:0.0\n"),
	    translate_summary(Read,Write),
	    translate(Read,Write);
	"\n" ->
	    translate(Read,Write);
	?divider ->
	    translate(Read,Write);
	"Process Information\n" ->
	    drop_line(Read), % divider
	    translate_processes(Read,Write),
	    translate(Read,Write);
	"Zombie Process Information\n" ->
	    %% throwing away zombie information
	    _ = line(Read), % "Processes kept: ...."
	    _ = line(Read), % probably a divider
	    translate(Read,Write);
	"Port Information\n" ->
	    drop_line(Read), % divider
	    translate_ports(Read,Write),
	    translate(Read,Write);
	"Internal Table Information\n" ->
	    drop_line(Read), % divider
	    translate_internal_tables(Read,Write),
	    translate(Read,Write);
	"ETS tables\n" ->
	    drop_line(Read), % divider
	    translate_ets_tables(Read,Write),
	    translate(Read,Write);
	"Timers\n" ->
	    drop_line(Read), % divider
	    translate_timers(Read,Write),
	    translate(Read,Write);
	"Distribution Information\n" ->
	    translate_dist_info(Read,Write),
	    translate(Read,Write);
	"Loaded Modules Information\n" ->
	    drop_line(Read), % divider
	    translate_loaded_modules(Read,Write),
	    translate(Read,Write);
	"Fun table\n" ->
	    drop_line(Read), % divider
	    translate_funs(Read,Write),
	    translate(Read,Write);
	"Atoms\n" ->
	    drop_line(Read), % divider
	    write(Write,"=atoms\n"),
	    {_,{Atoms,N}} = read_to_empty_line_reverse(Read),
	    write(Write,[Atoms,"=num_atoms:",integer_to_list(N),"\n"]),
	    translate(Read,Write);
	"Memory allocation information\n" -> 
	    write(Write,"=old_instr_data\n"),
	    drop_to_empty_line(Read),
	    translate(Read,Write);
	"<End of Erlang crash dump>\n" ->
	    write(Write,"=end\n");
	PartOfLine when Truncated ->
	    truncated(PartOfLine);
	Line ->
	    unexpected(Line),
	    translate(Read,Write)
    end.

translate_summary(Read,Write) ->
    copy_line(Read,Write), % date
    drop_line(Read), % newline
    copy_line(Read,Write), % slogan
    drop_line(Read), % newline
    drop_line(Read), % newline
    system_version(Read,Write), % emulator
    copy_line(Read,Write,"Compiled: "). % compile time
    
system_version(Read,Write) ->
    Heading = "System version: ",
    {Truncated,InputLine} = 
	case line(Read) of
	    {eof,InputLine0} -> {true,InputLine0};
	    InputLine0 -> {false,InputLine0}
	end,
    case InputLine of
	"Erlang (BEAM) emulator version " ++ Rest ->
	    {ErtsVsn,_} = split(Rest),
	    put(erts_vsn,ErtsVsn),
	    write(Write,[Heading,InputLine]);
	_ ->
	    write(Write,[Heading,InputLine])
    end,
    if Truncated -> truncated();
       true -> ok
    end.


translate_processes(Read,Write) ->
    {Truncated,InputLine} = 
	case line(Read) of
	    {eof,InputLine0} -> {true,InputLine0};
	    InputLine0 -> {false,InputLine0}
	end,
    case InputLine of
	"\n" ->
	    ok;  % all processes translated
	Line -> 
	    %% handles both Line and {eof,PartOfLine}
	    Rest1 = copy_word(Write,Line,$ ,"=proc:"), 
	    ModifiedRest1 = 
		case Rest1 of
		    "Process is garbing, limited information." ++ R2 ->
			"Garbing." ++ R2;
		    _ ->
			Rest1
		end,
	    Rest2 = copy_word(Write,ModifiedRest1,$.,"State: "),
	    case Rest2 of
		"\n" -> 
		    ok;
		" Registered as: " ++ NameNl ->
		    write(Write,["Name: ",NameNl]);		    
		TruncText when Truncated ->
		    truncated(TruncText);
	        Unexpected ->
		    unexpected(Unexpected)
	    end,
	    translate_process(Read,Write,line(Read))
    end.
translate_process(Read,Write,InputLine0) ->
    {Truncated,InputLine} = 
	case InputLine0 of
	    {eof,I} -> {true,I};
	    InputLine0 -> {false,InputLine0}
	end,
    case InputLine of
	?divider ->
	    translate_processes(Read,Write); % translate next process
	"Spawned as: " ++ _Func = InitFuncNl ->
	    write(Write,InitFuncNl),
	    translate_process(Read,Write,line(Read));
	"Message buffer data: " ++ Rest ->
	    write(Write,["Heap fragment data: ", Rest]),
	    translate_process(Read,Write,line(Read));
	"Message queue (" ++ Rest ->
	    write_msg_q_stuff(Write,Rest,Truncated),
	    translate_process(Read,Write,line(Read));
	{part,"Message queue (" ++ Rest} ->
	    write_msg_q_stuff(Write,Rest,Truncated),
	    write_rest_of_line(Read,Write),
	    translate_process(Read,Write,line(Read));
	"Last calls:" ++ _nl ->
	    write(Write,"Last calls: "),
	    NextLine = write_last_calls(Read,Write),
	    translate_process(Read,Write,NextLine);
	"Reductions " ++ Rest1 ->
	    Rest2 = copy_word(Write,Rest1,$ ,"Reductions: "),
	    case Rest2 of
		"stack+heap " ++ Rest3 ->
		    Rest4 = copy_word(Write,Rest3,$ ,"Stack+heap: "),
		    case Rest4 of
			"old_heap_sz=" ++ OldHeapNl ->
			    write(Write,["OldHeap: ",OldHeapNl]);
			TruncText when Truncated ->
			    truncated(TruncText);
			Unexpected ->
			    unexpected(Unexpected)
		    end;
		"heap_sz " ++ _Rest3 ->
		    write(Write,"Stack: -1\n"),
		    put(shared_heap,true);
		TruncText when Truncated ->
		    truncated(TruncText);
		Unexpected ->
		    unexpected(Unexpected)
	    end,
	    translate_process(Read,Write,line(Read));
	"Heap unused=" ++ Rest1 ->
	    case get(shared_heap) of
		true ->
		    ok;
		_ ->
		    Rest2 = copy_word(Write,Rest1,$ ,"Heap unused: "),
		    case Rest2 of
			"OldHeap unused=" ++ OldHeapUnusedNl ->
			    write(Write,["OldHeap unused: ",OldHeapUnusedNl]);
			TruncText when Truncated ->
			    truncated(TruncText);
			Unexpected ->
			    unexpected(Unexpected)
		    end
	    end,
	    translate_process(Read,Write,line(Read));
	"new heap: start    top      sp       end" ++ _ ->
	    NewHeapLine = case line(Read) of
			      {eof,L} -> {eof,string:strip(L,left)};
			      L -> string:strip(L,left)
			  end,
	    New1 = copy_word(Write,NewHeapLine,$ ,"New heap start: "),
	    New2 = copy_word(Write,New1,$ ,"New heap top: "),
	    New3 = copy_word(Write,New2,$ ,"Stack top: "),
	    _New4 = copy_word(Write,New3,$ ,"Stack end: "),
	    translate_process(Read,Write,line(Read));
	"old heap: start    top      end" ++ _ ->
	    OldHeapLine = case line(Read) of
			      {eof,L} -> {eof,string:strip(L,left)};
			      L -> string:strip(L,left)
			  end,
	    Old1 = copy_word(Write,OldHeapLine,$ ,"Old heap start: "),
	    Old2 = copy_word(Write,Old1,$ ,"Old heap top: "),
	    _Old3 = copy_word(Write,Old2,$ ,"Old heap end: "),
	    translate_process(Read,Write,line(Read));
	{part,Line} ->
	    write(Write,Line),
	    write_rest_of_line(Read,Write),
	    translate_process(Read,Write,line(Read));
	PartOfLine when Truncated ->
	    truncated(PartOfLine);
	Line ->
	    write(Write,Line),
	    translate_process(Read,Write,line(Read))
    end.

write_msg_q_stuff(Write,Rest1,Truncated) ->
    Rest2 = copy_word(Write,Rest1,$ ,"Message queue length: "),
    case Rest2 of
	"messages): " ++ MsgQ ->
	    write(Write,["Message queue: ",MsgQ]);
	"message): " ++ MsgQ ->
	    write(Write,["Message queue: ",MsgQ]);
	TruncText when Truncated ->
	    truncated(TruncText);
	Unexpected ->
	    unexpected(Unexpected)
    end.

write_last_calls(Read,Write) ->
    case line(Read) of
	"  " ++ Call -> 
	    write(Write,[Call--"\n"," "]),
	    write_last_calls(Read,Write);
	Line -> 
	    write(Write,"\n"),
	    Line
    end.

translate_ports(Read,Write) ->
    case line(Read) of
	"\n" ->
	    ok;
	?divider ->
	    translate_ports(Read,Write);
	"<" ++ Rest ->
	    {Slot,_nl} = split($>,Rest),
	    write(Write,["=port:#Port<0.",Rest,"Slot: ",Slot,"\n"]),
	    translate_ports(Read,Write);
	"Port is UNIX fd " ++ Rest0 ->
	    {Fd,Rest1} = split(Rest0),
	    write(Write,["Port is UNIX fd ",Rest1--"\n",": ",Fd,"\n"]),
	    translate_ports(Read,Write);
	{eof,PartOfLine} ->
	    truncated(PartOfLine);
	Line ->
	    write(Write,Line),
	    translate_ports(Read,Write)
    end.

translate_internal_tables(Read,Write) ->
    translate_internal_tables(Read,Write,[]).
translate_internal_tables(Read,Write,AtomSpace) ->
    Line = line(Read),
    {Truncated,InputLine} = 
	case Line of
	    {eof,InputLine0} -> {true,InputLine0};
	    InputLine0 -> {false,InputLine0}
	end,
    case InputLine of
	"Hash Table(" ++ Rest ->
	    translate_hash_table(Write,Rest),
	    translate_internal_tables(Read,Write,AtomSpace);
	"Index Table(" ++ Rest ->
	    translate_index_table(Write,Rest),
	    translate_internal_tables(Read,Write,AtomSpace);
	"Atom space  " ++ Rest ->
	    {Used,Allocated} = split($/,Rest),
	    translate_internal_tables(Read,Write,{Used,Allocated--"\n"});
	"Allocated " ++ _ ->
	    translate_allocated_areas(Read,Write,Line,AtomSpace);
	"sl_alloc: disabled\n" ->
	    write(Write,"=allocator:sl_alloc\noption e: false\n"),
	    translate_internal_tables(Read,Write,AtomSpace);
	"sl_alloc: ver(" ++ Rest ->
	    translate_sl_alloc(Read,Write,Rest,AtomSpace);
	"sl_alloc: carriers size(" ++ Rest ->
	    translate_sl_alloc_r7_r8(Write,"carriers size",Rest),
	    translate_internal_tables(Read,Write,AtomSpace);
	"Mmap chunks " ++ Chunks ->
	    write(Write,["=allocator:sl_alloc\nversion: release1\nmmap chunks: ",
			 Chunks]),
	    case line(Read) of
		"Mmap size " ++ Size ->
		    write(Write,["mmap size: ",Size]);
		{eof,PartOfLine} ->
		    truncated(PartOfLine)
	    end,
	    translate_internal_tables(Read,Write,AtomSpace);
	"Memory (bytes):\n" ->
	    write(Write,"=memory\n"),
	    translate_memory_and_allocated_area_r9b(Read,Write,AtomSpace);
	PartOfLine when Truncated ->
	    truncated(PartOfLine);
	Unexpected ->
	    unexpected(Unexpected),
	    translate_internal_tables(Read,Write,AtomSpace)
    end.

translate_hash_table(Write,Line) ->
    Rest1 = translate_internal_table_line(Write,Line,"=hash_table:", ", size"),
    Rest2 = translate_internal_table_line(Write,Rest1,"size: ", ", used"),
    Rest3 = translate_internal_table_line(Write,Rest2,"used: ", ", objs"),
    Rest4 = translate_internal_table_line(Write,Rest3,"objs: ", ", depth"),
    translate_internal_table_line(Write, Rest4, "depth: ", []).
				    
translate_index_table(Write,Line) ->
    Rest1 = translate_internal_table_line(Write,Line,"=index_table:",", size"),
    Rest2 = translate_internal_table_line(Write,Rest1,"size: ", ", limit"),
    Rest3 = translate_internal_table_line(Write,Rest2,"limit: ", ", used"),
    Rest4 = translate_internal_table_line(Write,Rest3,"used: ", ", rate"),
    translate_internal_table_line(Write, Rest4, "rate: ", []).

translate_internal_table_line(Write,Line,ThisHeading,NextMatch) ->
    Rest1 = copy_word(Write,Line,$),ThisHeading),
    case split($(,Rest1) of
	{NextMatch,Rest2} when Rest2=/=[]; NextMatch=:=[] ->
	    Rest2;
	{_Other,_Rest2} ->
	    truncated(Rest1)
    end.
	    
translate_allocated_areas(Read,Write,Line,{Used,Allocated}) ->
    write(Write,["=allocated_areas\natom_space: ",Allocated," ",Used,"\n"]),
    translate_allocated_areas(Read,Write,Line).

translate_allocated_areas(Read,Write,Line) ->
    {Truncated,InputLine} = 
	case Line of
	    {eof,InputLine0} -> {true,InputLine0};
	    InputLine0 -> {false,InputLine0}
	end,
    case InputLine of
    	?divider ->
	    ok;
	"Allocated by " ++ Rest ->
	    {Tag,Value} = split(Rest),
	    write(Write,[Tag,": ",Value]),
	    translate_allocated_areas(Read,Write,line(Read));
	"Allocated binary data " ++ Value -> % R8B/R9B
	    write(Write,["binary: ",Value]),
	    translate_allocated_areas(Read,Write,line(Read));
	"Allocated binary " ++ Value ->      % R7B
	    write(Write,["binary: ",Value]),
	    translate_allocated_areas(Read,Write,line(Read));
	"Totally allocated " ++ Value ->     % R7B
	    write(Write,["total: ",Value]),
	    translate_allocated_areas(Read,Write,line(Read));	    
	"Maximum allocated " ++ Value ->     % R7B
	    write(Write,["maximum: ",Value]),
	    translate_allocated_areas(Read,Write,line(Read));	    
	PartOfLine when Truncated ->
	    truncated(PartOfLine);
	Unexpected ->
	    unexpected(Unexpected),
	    translate_allocated_areas(Read,Write,line(Read))
    end.
	
translate_memory_and_allocated_area_r9b(Read,Write,AtomSpace) ->
    {Truncated,InputLine} = 
	case line(Read) of
	    {eof,InputLine0} -> {true,InputLine0};
	    InputLine0 -> {false,InputLine0}
	end,
    case InputLine of
	"Misc allocated areas (bytes):\n" ->
	    write(Write,"=allocated_areas\n"),
	    translate_memory_and_allocated_area_r9b(Read,Write,AtomSpace);
	"sl_alloc: ver(" ++ Rest ->
	    translate_sl_alloc(Read,Write,Rest,AtomSpace);
	"sl_alloc: disabled\n" ->
	    write(Write,"=allocator:sl_alloc\noption e: false\n");
	PartOfLine when Truncated ->
	    write(Write,PartOfLine),
	    truncated();
	Line ->
	    write(Write,Line),
	    translate_memory_and_allocated_area_r9b(Read,Write,AtomSpace)
    end.

translate_sl_alloc(Read,Write,Vsn0,AtomSpace) ->
    {Vsn,_} = split($),Vsn0),
    write(Write,["=allocator:sl_alloc\nversion: ",Vsn,"\noption e: true\n"]),
    do_translate_sl_alloc(Read,Write,AtomSpace,undefined).

do_translate_sl_alloc(Read,Write,AtomSpace,SbcOrMbc) ->
    Line = line(Read),
    {Truncated,InputLine} = 
	case Line of
	    {eof,InputLine0} -> {true,InputLine0};
	    InputLine0 -> {false,InputLine0}
	end,
    case InputLine of
	?divider ->
	    ok;
	"Allocated " ++ _ ->
	    translate_allocated_areas(Read,Write,Line,AtomSpace);
	"          sbc: cno(" ++ Rest1 ->
	    Rest2 = copy_word(Write,Rest1,$:,"no of sbcs: "),
	    case copy_word(Write,Rest2,$),"no of seg sbcs: ") of
		", csz(" ++ Rest3 ->
		    Rest4 = copy_word(Write,Rest3,$:,"sbcs size: "),
		    _Rest5 = copy_word(Write,Rest4,$),"seg sbcs size: ");
		PartOfLine when Truncated ->
		    truncated(PartOfLine)
	    end,
	    do_translate_sl_alloc(Read,Write,AtomSpace,"sbc");
	"               mcno(" ++ Rest ->
	    translate_sl_alloc_line(Write,Rest,
				    [["max no of ",SbcOrMbc,"s: "],
				     ["max ",SbcOrMbc,"s size: "]], 
				    [", mcsz"]),
	    do_translate_sl_alloc(Read,Write,AtomSpace,SbcOrMbc);
	"               bno(" ++ Rest ->
	    translate_sl_alloc_line(Write,Rest,
				    [["no of ",SbcOrMbc," blocks: "],
				     [SbcOrMbc," blocks size: "],
				     [SbcOrMbc," blocks adm size: "]],
				    [", bsz",", asz"]),
	    do_translate_sl_alloc(Read,Write,AtomSpace,SbcOrMbc);
	"               mbno(" ++ Rest ->
	    translate_sl_alloc_line(Write,Rest,
				    [["max no of ",SbcOrMbc," blocks: "],
				     ["max ",SbcOrMbc," block size: "]], 
				    [", mbsz"]),
	    do_translate_sl_alloc(Read,Write,AtomSpace,SbcOrMbc);
	"          mbc: cno(" ++ Rest1 ->
	    Rest2 = copy_word(Write,Rest1,$:,"no of mbcs: "),
	    case copy_word(Write,Rest2,$),"no of seg mbcs: ") of
		", csz(" ++ Rest3 ->
		    Rest4 = copy_word(Write,Rest3,$:,"mbcs size: "),
		    _Rest5 = copy_word(Write,Rest4,$),"seg mbcs size: ");
		PartOfLine when Truncated ->
		    truncated(PartOfLine)
	    end,
	    do_translate_sl_alloc(Read,Write,AtomSpace,"mbc");
	PartOfLine when Truncated ->
	    truncated(PartOfLine);
	Unexpected ->
	    unexpected(Unexpected),
	    do_translate_sl_alloc(Read,Write,AtomSpace,SbcOrMbc)
    end.
	    
	    

translate_sl_alloc_line(Write,Rest1,[Heading|Headings],[Match|Matches]) ->
    Rest2 = copy_word(Write,Rest1,$),Heading),
    case split($(,Rest2) of
	{Match,Rest3} when Rest2=/=[] ->
	    translate_sl_alloc_line(Write,Rest3,Headings,Matches);
	{_Other,_Rest3} ->
	    truncated(Rest2)
    end;
translate_sl_alloc_line(Write,Rest1,[Heading],[]) ->
    copy_word(Write,Rest1,$),Heading).
    

translate_sl_alloc_r7_r8(Write,Heading,Line) ->
    write(Write,["=allocator:sl_alloc\nversion: release2\n"]),
    do_translate_sl_alloc_r7_r8(Write,Heading,Line).

do_translate_sl_alloc_r7_r8(Write,Heading1,Line) ->
    case split($),Line) of
	{Value,", " ++ Rest1} ->
	    write(Write,[Heading1,": ",Value,"\n"]),
	    case split($(,Rest1) of
		{Heading2,Rest2} when Rest2=/=[] ->
		    do_translate_sl_alloc_r7_r8(Write,Heading2,Rest2);
		_ ->
		    truncated(Rest1)
	    end;
	{Value,"\n"} ->
	    write(Write,[Heading1,": ",Value,"\n"]);
	_ ->
	    truncated(Line)
    end.

translate_ets_tables(Read,Write) ->
    case line(Read) of
	"In slot " ++ Slot ->
	    translate_ets(Read,Write,["Slot: ",Slot]);
	"\n" ->
	    ok;
	{eof,PartOfLine} ->
	    truncated(PartOfLine)
    end.

translate_ets(Read,Write,Acc) ->
    case line(Read) of
	"Table " ++ Rest1 = Line ->
	    case split($(,Rest1) of
		{Table,"with name)"++NameNl} ->
		    translate_ets(Read,Write,
				  [Acc,"Table: ",Table,"\n","Name: ",NameNl]);
		_ ->
		    truncated(Line)
	    end;
	"Owner " ++ OwnerNl ->
	    write(Write,["=ets:",OwnerNl,Acc]),
	    translate_ets(Read,Write,"");
	"Table's got " ++ Got ->
	    case split(Got) of
		{Objects,"objects\n"} -> 
		    write(Write,["Objects: ",Objects,"\n"]);
		{Words,"words of active data\n"} ->
		    write(Write,["Words: ",Words,"\n"])
	    end,
	    translate_ets(Read,Write,Acc);
	"\n" ->
	    translate_ets_tables(Read,Write);
	{eof,PartOfLine} ->
	    case Acc of
		"" -> ok;
		Acc -> write(Write,["=ets:unknown\n",Acc])
	    end,
	    truncated(PartOfLine);
	Line ->
	    write(Write,Line),
	    translate_ets(Read,Write,Acc)
    end.

translate_timers(Read,Write) ->
    {Truncated,InputLine} = 
	case line(Read) of
	    {eof,InputLine0} -> {true,InputLine0};
	    InputLine0 -> {false,InputLine0}
	end,
    case InputLine of
	?divider ->
	    ok;
	"message=" ++ Rest1 ->
	    {Msg,Rest2} = get_msg(Rest1,Truncated,[]),
	    case split($,,Rest2) of
		{Pid," time left " ++ TimeLeftNl} when not Truncated  ->
		    write(Write,["=timer:",Pid,"\n",
				 "Message: ",Msg,"\n",
				 "Time left: ",TimeLeftNl]),
		    translate_timers(Read,Write);
		{[],[]} ->
		    write(Write,["=timer:unknown\n",
				 "Message: ",Msg,"\n"]),
		    truncated();
		{PartOfPid,[]} ->
		    write(Write,["=timer:",PartOfPid,"\n",
				 "Message: ",Msg,"\n"]),
		    truncated();
		{Pid,PartOfLine} when Truncated  ->
		    write(Write,["=timer:",Pid,"\n",
				 "Message: ",Msg,"\n"]),
		    truncated(PartOfLine);
		{_,_} ->
		    unexpected(Rest2),
		    translate_timers(Read,Write)
		end;
	PartOfLine when Truncated ->
	    truncated(PartOfLine);
	Unexpected ->
	    unexpected(Unexpected),
	    translate_timers(Read,Write)
    end.

get_msg(Str,Truncated,Acc) ->
    case split($,,Str) of
	{RestOfMsg," pid=" ++ Rest} ->
	    [$,|Msg] = lists:reverse([RestOfMsg,$,|Acc]),
	    {Msg,Rest};
	{PartOfMsg,Rest} when Truncated ->
	    {PartOfMsg,Rest};
	{PartOfMsg,Rest} ->
	    get_msg(Rest,Truncated,[PartOfMsg,$,|Acc])
    end.
    

translate_dist_info(Read,Write) ->
    case line(Read) of
	"------------------------\n" ->
	    translate_dist_info2(Read,Write,"=visible_node:");
	"Not alive\n" ->
	    write(Write,"=no_distribution\n");
	{eof,PartOfLine} ->
	    truncated(PartOfLine)
    end.
	    
translate_dist_info2(Read,Write,Tag) ->
    {Truncated,InputLine} = 
	case line(Read) of
	    {eof,InputLine0} -> {true,InputLine0};
	    InputLine0 -> {false,InputLine0}
	end,    
    case InputLine of
	"Alive but not holding any connections \n" ->
	    ok;
	"-- Visible nodes -------\n" ->
	    drop_line(Read), % divider
	    translate_node_info(Read,Write,line(Read),"=visible_node:"),
	    translate_dist_info2(Read,Write,Tag);
	"-- Hidden nodes --------\n" ->
	    drop_line(Read), % divider
	    translate_node_info(Read,Write,line(Read),"=hidden_node:"),
	    translate_dist_info2(Read,Write,Tag);
	"-- Not connected -------\n" ->		    
	    drop_line(Read), % divider
	    translate_node_info(Read,Write,line(Read),"=not_connected:"),
	    translate_dist_info2(Read,Write,Tag);
	"\n" ->
	    ok; % all nodes done
	"------------------------\n" ->
	    translate_dist_info2(Read,Write,Tag);
	TruncText when Truncated ->
	    truncated(TruncText);
	Node ->
	    translate_node_info(Read,Write,Node,Tag),
	    translate_dist_info2(Read,Write,Tag)
    end.


translate_node_info(Read,Write,Node,Tag) ->    
    case copy_word(Write,Node,$ ,Tag) of
	": Connection to:" ++ Rest1 ->
	    case copy_word(Write,Rest1,$ ,"Name: ") of
		"Controller:"++Rest2 ->
		    %% Here (_Rest3) i might be throwing away some 
		    %% info about cookies (R7/R8)!!
		    _Rest3 = copy_word(Write,Rest2,$ ,"Controller: "),
		    translate_node_info2(Read,Write,Tag);
		Unexpected ->
		    unexpected(Unexpected),
		    %% Would never come here if truncated, and I know
		    %% it's a node - so I'll just dive into it.
		    translate_node_info2(Read,Write,Tag)
	    end;
	": " ++ Name ->
	    write(Write,["Name: ",Name]),
	    translate_node_info2(Read,Write,Tag);
	Unexpected ->
	    %% dont know what to do, so I can't dive further into this
	    unexpected(Unexpected)
    end.

translate_node_info2(Read,Write,Tag) ->
    {Truncated,InputLine} = 
	case line(Read) of
	    {eof,InputLine0} -> {true,InputLine0};
	    InputLine0 -> {false,InputLine0}
	end,
    case InputLine of
	"Creation: " ++ _ = CreationNl ->
	    get_all_creations(Read,Write,CreationNl),
	    translate_node_info2(Read,Write,Tag);
	"Processes holding remote links to " ++ _node -> % R7/R8
	    translate_links(Read,Write,Tag);
	"Remote links and monitors to/from " ++ _node -> % R9
	    translate_links(Read,Write,Tag);
	"error .. " ++ Error ->
	    write(Write,["Error: ",Error]),
	    translate_node_info2(Read,Write,Tag);
	"------------------------\n" ->
	    ok;
	"\n" ->
	    translate_node_info2(Read,Write,Tag);
	PartOfLine when Truncated ->
	    truncated(PartOfLine);
	Unexpected ->
	    unexpected(Unexpected),
	    translate_node_info2(Read,Write,Tag)
    end.


get_all_creations(Read,Write,Creation) ->
    {Truncated,InputLine} = 
	case line(Read) of
	    {eof,InputLine0} -> {true,InputLine0};
	    InputLine0 -> {false,InputLine0}
	end,
    case InputLine of
	"Creation: " ++ CreationNl ->
	    get_all_creations(Read,Write,(Creation--"\n")++" "++CreationNl);
	"\n" ->
	    write(Write,Creation);
	PartOfLine when Truncated ->
	    write(Write,Creation),
	    truncated(PartOfLine);
	Unexpected ->
	    unexpected(Unexpected)
    end.

translate_links(Read,Write,Tag) ->
    {Truncated,InputLine} = 
	case line(Read) of
	    {eof,InputLine0} -> {true,InputLine0};
	    InputLine0 -> {false,InputLine0}
	end,
    case InputLine of
	"------------------------\n" ->
	    ok;
	"\n" -> 
	    ok;
	"<" ++ _ = Line -> 
	    {Proc1,Rest} = split(Line),
	    {Text,Proc2} = 
		case Rest of
		    "linked to " ++ P -> % R7/R8
			{"Remote link: ",P};
		    "is linked to " ++ P -> % R9
			{"Remote link: ",P};
		    "is monitoring " ++ P -> % R9
			{"Remote monitoring: ",P};
		    "is being monitored by " ++ P -> % R9
			{"Remotely monitored by: ",P};
		    _ when Truncated ->
			truncated(Line)
		end,
	    {Local,Remote} = 
		case Proc1 of
		    "<0."++ _ -> {Proc1,Proc2};
		    _ -> {Proc2--"\n",[Proc1,"\n"]}
		end,
	    write(Write,[Text,Local," ",Remote]),
	    translate_links(Read,Write,Tag);
	Unexpected ->
	    unexpected(Unexpected),
	    translate_links(Read,Write,Tag)
    end.


translate_loaded_modules(Read,Write) ->
    write(Write,"=loaded_modules\n"),	   
    case read_to_empty_line_reverse(Read) of
	{eof,{Mods,_}} ->
	    write(Write,["Current code: unknown\n",
			 "Old code: unknown\n"]),
	    translate_loaded_modules2(Write,Mods);
	{all,{Mods,_}} ->
	    translate_loaded_modules_totals(Read,Write,Mods)
    end.

translate_loaded_modules_totals(Read,Write,Mods) ->
    {Truncated,InputLine} = 
	case line(Read) of
	    {eof,InputLine0} -> {true,InputLine0};
	    InputLine0 -> {false,InputLine0}
	end,
    case InputLine of
	"Totals. Current code = " ++ Rest1 ->
	    Rest2 = copy_word(Write,Rest1,$ ,"Current code: "),
	    case Rest2 of
		"Old code = " ++ OldCodeNl ->
		    write(Write,["Old code: ", OldCodeNl]),
		    translate_loaded_modules2(Write,Mods);
		PartOfLine when Truncated ->
		    translate_loaded_modules2(Write,Mods),
		    truncated(PartOfLine)
	    end;
	PartOfLine when Truncated ->
	    translate_loaded_modules2(Write,Mods),
	    truncated(PartOfLine);
	Unexpected ->
	    translate_loaded_modules2(Write,Mods),
	    unexpected(Unexpected)
    end.

translate_loaded_modules2(Write,[Mod|Mods]) ->
    {M,SizeNl} = split(Mod),
    write(Write,["=mod:",M,"\n","Current size: ",SizeNl]),
    translate_loaded_modules2(Write,Mods);
translate_loaded_modules2(_Write,[]) ->
    ok.

    
translate_funs(Read,Write) ->    
    {Truncated,InputLine} = 
	case line(Read) of
	    {eof,InputLine0} -> {true,InputLine0};
	    InputLine0 -> {false,InputLine0}
	end,
    case InputLine of
	"\n" ->
	    ok; % all funs done
	"module=" ++ Rest1 ->
	    case copy_word(Write,Rest1,$ ,"=fun\nModule: ") of
		"uniq=" ++ Rest2 ->
		    case copy_word(Write,Rest2,$ ,"Uniq: ") of
			"index=" ++ Rest3 ->
			    IndexNl = Rest3 -- ":",
			    write(Write,["Index: ", IndexNl]),
			    translate_fun(Read,Write),
			    translate_funs(Read,Write);
			PartOfLine when Truncated ->
			    truncated(PartOfLine)
		    end;
		PartOfLine when Truncated ->
		    truncated(PartOfLine)
	    end;
	PartOfLine when Truncated ->
	    truncated(PartOfLine);
	Unexpected ->
	    unexpected(Unexpected),
	    translate_funs(Read,Write)
    end.

translate_fun(Read,Write) ->
    {Truncated,InputLine} = 
	case line(Read) of
	    {eof,InputLine0} -> {true,InputLine0};
	    InputLine0 -> {false,InputLine0}
	end,
    case InputLine of
	"\n" ->
	    ok; % next fun
	"  address=" ++ AddressNl ->
	    write(Write,["Address: ", AddressNl]),
	    translate_fun(Read,Write);
	"  native_address=" ++ NativeAddressNl ->
	    write(Write,["Native_address: ",NativeAddressNl]),
	    translate_fun(Read,Write);
	"  refc=" ++ RefcNl ->
	    write(Write,["Refc: ", RefcNl]),
	    translate_fun(Read,Write);
	PartOfLine when Truncated ->
	    truncated(PartOfLine);
	Unexpected ->
	    unexpected(Unexpected),
	    translate_fun(Read,Write)
    end.

%%%-----------------------------------------------------------------
%%% Common library

truncated() ->
    io:format("WARNING: Dump is truncated~n",[]),
    throw({error,truncated}).
truncated(NoText) when NoText=:="\n";NoText=:=""->
    truncated();
truncated(Text) ->
    io:format("WARNING: Dump is truncated. Last text found:~n"
	      "~s~n",[Text]),
    throw({error,truncated}).

unexpected(Text) ->
    io:format("WARNING: Found unexpected text in dump:~n~p~n",[Text]).
    
read_to_empty_line_reverse(Read) ->
    read_to_empty_line_reverse(Read,[],0).
read_to_empty_line_reverse(Read,Acc,N) ->
    case line(Read) of
	"\n" ->
	    {all,{Acc,N}};
	{eof,PartOfLine} ->
	    {eof,{[PartOfLine|Acc],N+1}}; % truncated file
	Line ->
	    read_to_empty_line_reverse(Read,[Line|Acc],N+1)
    end.

drop_to_empty_line(Read) ->
    case line(Read) of
	"\n" ->
	    ok;
	{eof,_PartOfLine} ->
	    truncated();
	_Line ->
	    drop_to_empty_line(Read)
    end.

get_chunk(Fd) ->
    case erase(chunk) of
	undefined ->
	    read(Fd);
	Bin ->
	    {ok,Bin}
    end.

read(Fd) ->
    file:read(Fd,?chunk_size).

write(Fd,Str) ->
    file:write(Fd,list_to_binary(Str)).

%% Read chunks from dump. Return one whole line or ?max_line_size
%% number of characters.
line(Fd) ->
    case get_chunk(Fd) of
	{ok,Bin} -> line(Fd,Bin,[],0);
	eof -> truncated()
    end.

line(_Fd,Bin,Acc,?max_line_size) ->
    put(chunk,Bin),
    {part,lists:reverse(Acc)};
line(_Fd,<<$\n:8,Bin/binary>>,Acc,_N) ->
    put(chunk,Bin),
    lists:reverse([$\n|Acc]);
line(Fd,<<$\r:8,Bin/binary>>,Acc,N) ->
    %% ignore "\r"
    line(Fd,Bin,Acc,N);
line(Fd,<<Char:8,Bin/binary>>,Acc,N) ->
    line(Fd,Bin,[Char|Acc],N+1);
line(Fd,<<>>,Acc,N) ->
    case read(Fd) of
	{ok,Bin} ->
	    line(Fd,Bin,Acc,N+1);
	eof ->
	    {eof,lists:reverse(Acc)}
    end.
    
%% Copy the rest of the started line from dump to translated file.
write_rest_of_line(Read,Write) ->
    case get_chunk(Read) of
	{ok,Bin} -> write_rest_of_line(Read,Write,Bin,[]);
	eof -> truncated()
    end.

write_rest_of_line(_Read,Write,<<$\n:8,Bin/binary>>,Acc) ->
    put(chunk,Bin),
    write(Write,lists:reverse([$\n|Acc]));
write_rest_of_line(Read,Write,<<$\r:8,Bin/binary>>,Acc) ->
    %% ignore "\r"
    write_rest_of_line(Read,Write,Bin,Acc);
write_rest_of_line(Read,Write,<<Char:8,Bin/binary>>,Acc) ->
    write_rest_of_line(Read,Write,Bin,[Char|Acc]);
write_rest_of_line(Read,Write,<<>>,Acc) ->
    case read(Read) of
	{ok,Bin} ->
	    write(Write,lists:reverse(Acc)),
	    write_rest_of_line(Read,Write,Bin,[]);
	eof ->
	    write(Write,lists:reverse(Acc)),
	    truncated()
    end.

split(Str) -> % default separator is space
    split($ ,Str,[]).
split(Char,Str) ->
    split(Char,Str,[]).
split(Char,[Char|Str],Acc) -> % match Char
    {lists:reverse(Acc),Str};
split(_Char,[$\n|Str],Acc) -> % new line
    {lists:reverse(Acc),Str};
split(Char,[H|T],Acc) ->
    split(Char,T,[H|Acc]);
split(_Char,[],Acc) ->
    {lists:reverse(Acc),[]}. % truncated line


copy_line(Read,Write) ->
    copy_line(Read,Write,[]).
copy_line(Read,Write,Heading) ->
    case line(Read) of
	{eof,PartOfLine} -> 
	    write(Write,[Heading,PartOfLine]),
	    truncated();
	Line ->
	    write(Write,[Heading,Line])
    end.

drop_line(Read) ->
    case line(Read) of
	{eof,_PartOfLine} ->
	    truncated();
	_Line ->
	    ok
    end.

copy_word(Write,Line0,SplitChar,Heading) ->
    case Line0 of
	{eof,PartOfLine} -> 
	    {Word,Rest} = split(SplitChar,PartOfLine),
	    write(Write,[Heading,Word,"\n"]),
	    case Rest of
		[] -> truncated();
		_ -> Rest
	    end;
	Line ->
	    {Word,Rest} = split(SplitChar,Line),
	    write(Write,[Heading,Word,"\n"]),
	    Rest
    end.
	
