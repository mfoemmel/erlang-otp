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
%% This file was derived from:
%%
%% console.tcl --
%%
%% This code constructs the console window for an application.  It
%% can be used by non-unix systems that do not have built-in support
%% for shells.
%%
%% SCCS: @(%%) console.tcl 1.28 96/03/07 16:32:14
%%
%% Copyright (c) 1995-1996 Sun Microsystems, Inc.
%%
%% See the file "license.terms" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
-module(tkconsole).

-include("tk.hrl").

-export([start/0,
	 console/0,
	 shell/0,
	 console/1,
	 shell/1,
	 con_init/3,
	 con_tkinit/3,
	 con_invoke/2,
	 con_write/4,
	 con_output/3,
	 con_input/2]).

-import(lists, [reverse/1]).

%% TODO: history - remember partially written command

mkpos(X,Y) ->
    "@" ++ integer_to_list(X) ++ "," ++ integer_to_list(Y).

index(W, Where) ->
    tk:rcmd(W, [index, Where]).

start() -> 
    console().

shell() -> shell([]).

shell(Opts) ->
    con_start("Eshell", shell, Opts).

console() ->
    console([]).

console(Opts) ->
    con_start("Console", console, Opts).

con_start(Name, Type, Opts) ->
    spawn(tkconsole, con_init, [Name, Type, Opts]).

con_init(Name, Type, Opts) ->
    etk:start(),
    process_flag(trap_exit, true),
    group_leader(self(), self()),
    TopLevel = if
		   Type == shell ->
		       etk:toplevel(Opts);
		   true ->
		       case tk:winfo([exists, ".console"]) of
			   1 ->
			       tk:wm([deiconify, "."]),
			       exit(normal);
			   0 ->
			       user_console(self()),
			       "."
		       end
	       end,
    Con = con_tkinit(TopLevel, Name, self()),
    catch_loop(Con, shell:start()).

%%
%% Change user to console:
%% 1. Unregister user.
%% 2. Set console as user.
%% 3. Change group leader to console for all that had
%%    old user as group leader.
%% 3. kill old user.
%%
user_console(Console) ->
    case whereis(user) of
	undefined ->
	    register(user, Console);
	User ->
	    unregister(user),
	    register(user, Console),
	    error_logger:tty(false),
	    error_logger:tty(true),
	    lists:foreach(
	      fun(Pid) ->
		      case process_info(Pid, group_leader) of
			  User -> group_leader(Console, Pid);
			  _ -> false
		      end
	      end, processes()),
	    exit(User, kill)
    end.


catch_loop(Port, Shell) ->
    case catch server_loop(Port, []) of
	new_shell ->
	    exit(Shell, kill),
	    catch_loop(Port, shell:start());
	{'EXIT',R} ->
	    exit(R)
    end.

server_loop(Port, Buf0) ->
    receive
	{Con,{data,Bytes}} ->
	    Nosh = get(noshell),
	    case lists:member(7,Bytes) of
		true when  Nosh == undefined ->
		    throw(new_shell);
		_ ->
		    server_loop(Port,lists:append(Buf0,Bytes))
	    end;
	{io_request,From,ReplyAs,Request} when pid(From) ->
	    Buf = io_request(Request, From, ReplyAs, Port, Buf0),
	    server_loop(Port, Buf);
	{Port, eof} ->
	    put(eof, true),
	    server_loop(Port, Buf0);
	%% Ignore messages from port here.
	{'EXIT',Port,badsig} ->			%Ignore badsig errors
	    server_loop(Port, Buf0);
	{'EXIT',Port,What} ->			%Port has exited
	    exit(What);
	{'EXIT', _, {tkwidget,Port}} ->
	    exit(terminated);
	Other ->				%Ignore other messages
	    server_loop(Port, Buf0)
    end.


%% NewSaveBuffer = io_request(Request, FromPid, ReplyAs, Port, SaveBuffer)

io_request(Req, From, ReplyAs, Port, Buf0) ->
    case io_request(Req, Port, Buf0) of
	{Status,Reply,Buf} ->
	    io_reply(From, ReplyAs, Reply),
	    Buf;
	{exit,What} ->
	    tk:destroy(tk:parentof(Port)),
	    exit(What)
    end.

io_request({put_chars,Chars}, Port, Buf) ->
    case io_lib:deep_char_list(Chars) of	%Check deep list
	true ->
	    case Chars of
		[$*,$* | _] ->
		    con_output(Port, "stderr", Chars);
		_ ->
		    con_output(Port, "stdout", Chars)
	    end,
	    {ok,ok,Buf};
	false ->
	    {error,{error,put_chars},Buf}
    end;
io_request({put_chars,Mod,Func,Args}, Port, Buf) ->
    io_request({put_chars,catch apply(Mod,Func,Args)}, Port, Buf);
io_request({get_until,Prompt,M,F,As}, Port, Buf) ->
    case get(eof) of
	undefined ->
	    get_until(Prompt, M, F, As, Port, Buf);
	true when Buf == [] ->
	    {ok, eof, Buf};
	_ ->
	    get_until(Prompt, M, F, As, Port, Buf)
    end;
io_request({requests,Reqs}, Port, Buf) ->
    io_requests(Reqs, {ok,ok,Buf}, Port);
io_request(R, Port, Buf) ->			%Unknown request
    {ok,{error,{request,R}},Buf}.		%Ignore but give error (?)

%% Status = io_requests(RequestList, PrevStat, Port)
%%  Process a list of output requests as long as the previous status is 'ok'.

io_requests([R|Rs], {ok,Res,Buf}, Port) ->
    io_requests(Rs, io_request(R, Port, Buf), Port);
io_requests([_|_], Error, _) ->
    Error;
io_requests([], Stat, _) ->
    Stat.

%% io_reply(From, ReplyAs, Reply)
%%  The function for sending i/o command acknowledgement.
%%  The ACK contains the return value.

io_reply(From, ReplyAs, Reply) ->
    From ! {io_reply,ReplyAs,Reply}.


get_until(Prompt, M, F, As, Port, Buf) ->
    prompt(Port, Prompt, normal),
    tk:cmd(Port, [yview, {pickplace, insert}]),
    get_until1(Prompt, M, F, As, Port, Buf).

get_until1(Prompt, M, F, As, Port, []) ->
    receive
	{Port,{data,Bytes}} ->
	    Nosh = get(noshell),
	    case lists:member(7,Bytes) of
		true when Nosh == undefined ->
		    throw(new_shell);
		_ ->
		    get_until2(catch apply(M, F, [[],Bytes|As]),
			       M, F, As, Port)
	    end;
	{Port, eof} ->
	    put(eof, true),
	    {ok, eof, []};
%%	{io_request,From,ReplyAs,Request} when pid(From) ->
%%	    get_until1_out(Request, From, ReplyAs, Prompt, M, F, As, Port);
	{io_request,From,ReplyAs,{put_chars,Chars}} when pid(From) ->
	    get_until1_out({put_chars,Chars}, From, ReplyAs,
			   Prompt, M, F, As, Port);
	{io_request,From,ReplyAs,{put_chars,M1,F1,A1}} when pid(From) ->
	    get_until1_out({put_chars,M1,F1,A1}, From, ReplyAs,
			   Prompt, M, F, As, Port);
	{'EXIT',From,What} when node(From) == node() ->
	    {exit,What}
    end;

get_until1(Prompt, M, F, As, Port, Buf) ->
    get_until2(catch apply(M, F, [[],Buf|As]), M, F, As, Port).

get_until1_out(Req, From, ReplyAs, Prompt, M, F, As, Port) ->
    io_request({put_chars,"^R\n"}, Port, []),	%"Cancel" prompt
    io_request(Req, From, ReplyAs, Port, []),	%No new buf here!
    get_until1(Prompt, M, F, As, Port, []).

get_until2({more,Cont}, M, F, As, Port) ->
    prompt(Port, ">", partial),
    tk:cmd(Port, [yview, {pickplace, insert}]),
    receive
	{Port,{data,Bytes}} ->
	    Nosh = get(noshell),
	    case lists:member(7,Bytes) of
		true when Nosh == undefined ->
		    throw(new_shell);
		_ ->
		    get_until2(catch apply(M, F, [Cont,Bytes|As]),
			       M, F, As, Port)
	    end;
	{Port, eof} ->
	    put(eof, true),
	    get_until2(catch apply(M, F, [Cont,eof|As]),M, F, As, Port);
	{'EXIT',From,What} when node(From) == node() ->
	    {exit,What}
    end;
get_until2({done,Result,Buf}, M, F, As, Port) ->
    {ok,Result,Buf};
get_until2(Other, M, F, As, Port) ->
    {error,{error,get_until},[]}.

%% prompt(Port, Prompt)
%%  Print Prompt onto port Port, special case just atoms and print unquoted.

prompt(Port, Prompt, Partial) when atom(Prompt) ->
    List = io_lib:format('~s', [Prompt]),
    con_prompt(Port, List, Partial);
prompt(Port, {format,Format,Args},Partial) ->
    case catch io_lib:format(Format,Args) of
	{'EXIT',_} ->
	    con_prompt("???", Port, Partial);
	List ->
	    con_prompt(Port, List, Partial)
    end;
prompt(Port, Prompt, Partial) ->
    List = io_lib:write(Prompt),
    con_prompt(Port, Prompt, List).


con_prompt(Con, Prompt, Partial) ->
    Index = if Partial == normal ->
		    Temp = index(Con, "end - 1 char"),
		    tk:cmd(Con, [mark, set, output, "end"]),
		    con_output(Con, stdout, Prompt),
		    Temp;
	       true ->
		    Temp = index(Con, output),
		    tk:cmd(Con, [mark, set, output, "end"]),
		    con_output(Con, stdout, Prompt),
		    Temp
	    end,    
    tk:cmd(Con, [mark, set, output, Index]),
    tktext:tkTextSetCursor(Con, "end"),
    tk:cmd(Con, [mark, set, promptEnd, insert]),
    tk:cmd(Con, [mark, gravity, promptEnd, left]).

%% tkConsoleInit --
%% This procedure constructs and configures the console windows.
%%
%% Arguments:
%% 	None.

con_tkinit(TopLevel, Name, Shell) ->
    Win = if TopLevel == "." -> 
		  tk:wm([protocol, TopLevel, "WM_DELETE_WINDOW",
			 fun() -> tk:wm([withdraw, TopLevel]) end]),
		  tk:wm([deiconify, TopLevel]),
		  "";
	     true ->
		  TopLevel
	  end,

    Con = Win ++ ".console",
    Sb = Win ++ ".sb",
    Platform = tk:getvar("tcl_platform(platform)"),
    case Platform of
	"macintosh" ->
	    %% Use the native scrollbar for the console
	    tk:rename("scrollbar", ""),
	    tk:rename("macscrollbar", "scrollbar");
	_ ->
	    false
    end,
    tk:text(Con, [{yscrollcommand,
			  fun(From, To) ->
				  tk:cmd(Sb, [set,From,To])
			  end},
		  {setgrid, true}]),
    tk:scrollbar(Sb, [{command,
		       fun(As) ->
			       tk:cmd(Con, [yview | As])
		       end}]),
    tk:pack(Sb, [{side,right},{fill,both}]),
    tk:pack(Con, [{fill,both}, {expand, 1}, {side, left}]),

    case Platform of
	"machintosh" ->
	    %% after idle {.console configure -font {Monaco 9 normal}}
	    tk:cmd(Sb, [configure, {bg, "white"}]),
	    tk:cmd(Con, [configure,
			 {bg, "white"}, {bd,  0},
			 {highlightthickness, 0},
			 {selectbackground, "black"},
			 {selectforeground, "white"},
			 {selectborderwidth, 0},
			 {insertwidth,  1}]),
	    tk:cmd(Con, [tag, configure, "sel", {relief, ridge}]),
	    tk:bind(Con, "<FocusIn>", [],
		    fun() ->
			    tk:cmd(Con, [tag, configure,
					 "sel", {borderwidth, 0}]),
			    tk:cmd(Con, [configure,
					 {selectbackground,"black"},
					 {selectforeground,"white"}])
		    end),
	    tk:bind(Con, "<FocusOut>", [],
		    fun() ->
			    tk:cmd(Con, [tag, configure,
					 "sel", {borderwidth, 2}]),
			    tk:cmd(Con, [configure,
					 {selectbackground,"white"},
					 {selectforeground,"black"}])
		    end);
	_  ->
	    false
    end,

    con_bind(Con, Shell),

    tk:cmd(Con, [tag, configure, "stderr", {foreground, "red"}]),
    tk:cmd(Con, [tag, configure, "stdin", {foreground,"blue"}]),
    tk:focus([Con]),

    tk:wm([title, TopLevel, Name]),

    %% flush stdout

    tk:cmd(Con, [mark, set, output, index(Con, "end - 1 char")]),
    tktext:tkTextSetCursor(Con, "end"),
    tk:cmd(Con, [mark, set, "promptEnd", insert]),
    tk:cmd(Con, [mark, gravity, "promptEnd", left]),
    Con.



%% tkConsoleInvoke --
%% Processes the command line input.  If the command is complete it
%% is evaled in the main interpreter.  Otherwise, the continuation
%% prompt is added and more input may be added.
%%
%% Arguments:
%% None.

con_invoke(Con, Pid) ->
    Rs = tk:rcmd(Con, [tag, ranges, input]),
    Ranges = string:tokens(Rs, " "),
    Cmd = get_cmd(Con, Ranges, []),
    tk:cmd(Con, [mark, set, output, "end"]),
    tk:cmd(Con, [tag, delete, input]),
    Pid ! {Con, {data, Cmd}}.

get_cmd(Con, [Start, Stop | Ranges], Cmd) ->
    get_cmd(Con, Ranges, Cmd ++ tk:rcmd(Con, [get, Start, Stop]));
get_cmd(Con, [], Cmd) -> Cmd.

%% tkConsoleBind --
%% This procedure first ensures that the default bindings for the Text
%% class have been defined.  Then certain bindings are overridden for
%% the class.
%%
%% Arguments:
%% None.

con_bind(Con, Shell) ->
    tk:bindtags(Con, [Con ++ " Text . all"]),

    %% Ignore all Alt, Meta, and Control keypresses unless explicitly bound.
    %% Otherwise, if a widget binding for one of these is defined, the
    %% <KeyPress> class binding will also fire and insert the character,
    %% which is wrong.  Ditto for <Escape>.

    tk:bind(Con, "<Alt-KeyPress>", [], fun() -> false end), %% nothing
    tk:bind(Con, "<Meta-KeyPress>", [], fun() -> false end), %% nothing
    tk:bind(Con, "<Control-KeyPress>", [], fun() -> false end), %% nothing
    tk:bind(Con, "<Escape>", [], fun() -> false end), %% nothing
    tk:bind(Con, "<KP_Enter>", [], fun() -> false end),  %% nothing

    tk:bind(Con, "<Tab>", ['%W'],
	    fun(W) ->
		    con_input(W, "\t"),
		    tk:focus([W]),
		    break
	    end),
    tk:bind(Con, "<Return>", ['%W'],
	    fun(W) ->
		    tk:cmd(W, [mark, set, insert, "end - 1c"]),
		    con_input(W, "\n"),
		    con_invoke(W, Shell),
		    break
	    end),

    tk:bind(Con, "<Delete>", ['%W'],
	    fun(W) ->
		    case tk:cmd(W, [tag, nextrange, sel, "1.0", "end"]) of
			"" ->
			    case tk:cmd(W, [compare,insert,"<=",promptEnd]) of
				1 -> break;
				_ -> false
			    end;
			_ ->
			    tk:cmd(W, [tag, remove, sel, "sel.first",
				       promptEnd])
		    end
	    end),

    tk:bind(Con, "<BackSpace>", ['%W'],
	    fun(W) ->
		    case tk:cmd(W, [tag, nextrange, sel, "1.0", "end"]) of
			"" ->
			    case tk:cmd(W, [compare,insert,"<=",promptEnd]) of
				1 -> break;
				_ -> false
			    end;
			_ ->
			    tk:cmd(W, [tag, remove, sel, "sel.first",
				       promptEnd])
		    end
	    end),

    lists:foreach(
      fun(Left) ->
	      tk:bind(Con, Left, ['%W'],
		      fun(W) ->
			      case tk:cmd(W, [compare, insert, "<", promptEnd]) of
				  1 ->
				      tktext:tkTextSetCursor(W, "insert linestart");
				  _ ->
				      tktext:tkTextSetCursor(W, "promptEnd")
			      end,
			      break
		      end)
      end, ["<Control-a>", "<Home>"]),

    lists:foreach(
      fun(Right) ->
	      tk:bind(Con, Right, ['%W'],
		      fun(W) ->
			      tktext:tkTextSetCursor(W, "insert lineend"),
			      break
		      end)
      end, ["<Control-e>", "<End>"]),
				  
    tk:bind(Con, "<Control-d>", ['%W'],
	    fun(W) ->
		    case tk:cmd(W, [compare, insert, "<", promptEnd]) of
			1 -> break;
			_ -> true
		    end
	    end),

    tk:bind(Con, "<Control-k>", ['%W'],
	    fun(W) ->
		    case tk:cmd(W, [compare, insert, "<", promptEnd]) of
			1 -> tk:cmd(W, [mark, set, insert, promptEnd]);
			_ -> false
		    end
	    end),

    tk:bind(Con, "<Control-t>", ['%W'],
	    fun(W) ->
		    case tk:cmd(W, [compare, insert, "<", promptEnd]) of
			1 -> break;
			_ -> true
		    end
	    end),

    tk:bind(Con, "<Meta-d>", ['%W'],
	    fun(W) ->
		    case tk:cmd(W, [compare, insert, "<", promptEnd]) of
			1 ->  break;
			_ ->  true
		    end
	    end),

    tk:bind(Con, "<Meta-BackSpace>", ['%W'],
	    fun(W) ->
		    case tk:cmd(W, [compare, insert, "<=", promptEnd]) of
			1 -> break;
			_ -> true
		    end
	    end),

    tk:bind(Con, "<Control-h>", ['%W'], 
	    fun(W) ->
		    case tk:cmd(W, [compare, insert, "<=", promptEnd]) of
			1 -> break;
			_ -> true
		    end
	    end),

    tk:bind(Con, "<Control-v>", ['%W'],
	    fun(W) ->
		    case tk:cmd(W, [compare, insert, ">", promptEnd]) of
			1 ->
			    catch 
				begin
				    tk:cmd(W, [insert, insert,
					       tk:rselection([get, {displayof,W}]),
					       "input stdin"]),
				    tk:cmd(W, [see, insert])
				end;
			_ ->
			    false
		    end,
		    break
	    end),
    
    tk:bind(Con, "<Insert>", ['%W'],
	    fun(W) ->
		    catch con_input(W, tk:rselection([get,{displayof,W}])),
		    break
	    end),

    tk:bind(Con, "<KeyPress>", ['%W', '%A'],
	    fun(W,A) ->
		    con_input(W, A),
		    break
	    end),

    lists:foreach(
      fun(Left) ->
	      tk:bind(Con, Left, ['%W'],
		      fun(W) ->
			      case tk:cmd(W, [compare, insert, "==", promptEnd]) of
				  1 -> break;
				  _ ->
				      tktext:tkTextSetCursor(W, "insert-1c"),
				      break
			      end
		      end)
      end, ["<Control-b>", "<Left>"]),

    lists:foreach(
      fun(Right) ->
	      tk:bind(Con, Right, ['%W'],
		      fun(W) ->
			      tktext:tkTextSetCursor(W, "insert+1c"),
			      break
		      end)
      end, ["<Control-f>", "<Right>"]),

    tk:bind(Con, "<Control-i>", ['%W'], fun(W) -> tktext:tk_textCopy(W) end),

    tk:bind(Con, "<F18>", ['%W'], 
	    fun(W) -> tk_conPaste(W,Shell), break end),
    tk:bind(Con, "<Control-y>", ['%W'], 
	    fun(W) -> tk_conPaste(W,Shell), break end),

    tk:bind(Con, "<Control-g>", ['%W'],
	    fun(W) ->
		    Shell ! {W, {data, [7]}}
	    end),
    
    tk:bind(Con, "<ButtonRelease-2>", ['%W', '%x', '%y'],
	    fun(W,X,Y) ->
		    case ?tkget(mouseMoved) of
			1 ->
			    tkConPaste(W,X,Y);
			_ ->
			    case ?tkget(tk_strictMotif) of
				false ->
				    tkConPaste(W, X, Y);
				true -> false
			    end
		    end,
		    break
	    end).

%%
%% Paste at input position
%%
tkConPaste(W, X, Y) ->
    tk:cmd(W, [mark, set, insert, tkConClosestGap(W,X,Y)]),
    catch
	begin
	    Clip = tk:rselection([get,{displayof,W},
				  {selection,"PRIMARY"}]),
	    con_input(W, remove_lines(Clip))
	end,
    case tk:cget(W, state) of
	"normal" -> tk:focus([W]);
	_ -> false
    end.

%%
%% Find input position
%%
tkConClosestGap(W,X,Y) ->
    Pos = index(W, mkpos(X,Y)),
    case tk:cmd(W, [bbox, Pos]) of
	[B0,_,B2 | _] ->
	    if 
		X - B0 < B2/2 -> Pos;
		true ->
		    index(W, Pos ++ " + 1 char")
	    end;
	_ ->
	    Pos
    end.

%%
%% Normal paste
%%
tk_conPaste(W,Shell) ->
    catch
	begin
	    Clip = tk:rselection([get,{displayof,W},
				  {selection,"CLIPBOARD"}]),
	    List = split_lines(Clip),
	    lists:foreach(
	      fun(X) ->
		      tk:cmd(W,[mark,set,insert,"end - 1c"]),
		      con_input(W, "\n"),
		      con_invoke(W, Shell),
		      con_input(W, X)
	      end, List)
	end.


%% Given Cs characters split them into lines etiher \r or \n or both
split_lines(Cs) ->
    split_lines(Cs, [], []).

split_lines([$\n,$\r | Cs], Line, Ls) ->
    split_lines(Cs, [], [reverse(Line) | Ls]);
split_lines([$\n | Cs], Line, Ls) ->
    split_lines(Cs,  [], [reverse(Line) | Ls]);
split_lines([$\r | Cs], Line, Ls) ->
    split_lines(Cs, [], [reverse(Line) | Ls]);
split_lines([C | Cs], Line, Ls) ->
    split_lines(Cs, [C|Line], Ls);
split_lines([], [], Ls) ->
    reverse(Ls);
split_lines([], Line, Ls) ->
    reverse([reverse(Line) | Ls]).

%% Given a characters list remove \r and \n and \n\r replace with blank

remove_lines([$\n,$\r | Cs]) -> [$  | remove_lines(Cs)];
remove_lines([$\n | Cs]) -> [$  | remove_lines(Cs)];
remove_lines([$\r | Cs]) -> [$  | remove_lines(Cs)];
remove_lines([X | Cs]) -> [X | remove_lines(Cs)];
remove_lines([]) -> [].

%% tkConsoleInsert --
%% Insert a string into a text at the point of the insertion cursor.
%% If there is a selection in the text, and it covers the point of the
%% insertion cursor, then delete the selection before inserting.  Insertion
%% is restricted to the prompt area.
%%
%% Arguments:
%% w -		The text window in which to insert the string
%% s -		The string to insert (usually just a single character)

con_input(Con, "") ->
    false;
con_input(Con, S) ->
    catch 
	begin
	    case tk:cmd(Con, [compare, "sel.first", "<=", "insert"]) of
		1 ->
		    case tk:cmd(Con,[compare,"sel.last",">=","insert"]) of
			1 ->
			    tk:cmd(Con, [tag, remove, "sel.first",
				       "promptEnd"]),
			    tk:cmd(Con, [delete, "sel.first","sel.last"]);
			0 -> false
		    end;
		0 -> false
	    end
	end,

    case tk:cmd(Con, [compare, "insert", "<", "promptEnd"]) of
	1 -> tk:cmd(Con, [mark, set, insert, 'end']);
	_ -> false
    end,
    tk:cmd(Con, [insert, insert, S, "input stdin"]),
    tk:cmd(Con, [see, insert]).


%% tkConsoleOutput --
%%
%% This routine is called directly by ConsolePutsCmd to cause a string
%% to be displayed in the console.
%%
%% Arguments:
%% dest -	The output tag to be used: either "stderr" or "stdout".
%% string -	The string to be displayed.

con_write(Con, Dest, Fmt, Args) ->
    case catch io_lib:fwrite(Fmt, Args) of
	{'EXIT',_} -> false;
	String ->
	    con_output(Con, Dest, String)
    end.

con_output(Con, Dest, String) ->
    tk:cmd(Con, [insert, output, lists:flatten(String), Dest]),
    tk:cmd(Con, [see, insert]).

