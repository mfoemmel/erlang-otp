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
-module(os).

%% Provides a common operating system interface.

-export([type/0, version/0, cmd/1, find_executable/1, find_executable/2]).

-include("file.hrl").

%% Internal exports.
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, terminate/2]).

type() ->
    case erlang:info(os_type) of
	{vxworks, _} ->
	    vxworks;
	Else -> Else
    end.

version() ->
    erlang:info(os_version).

find_executable(Name) ->
    case os:getenv("PATH") of
	false -> find_executable(Name, []);
	Path  -> find_executable(Name, Path)
    end.

find_executable(Name, Path) ->
    Extensions = extensions(),
    case filename:pathtype(Name) of
	relative ->
	    find_executable1(Name, split_path(Path), Extensions);
	_ ->
	    verify_executable(Name, Extensions)
    end.

find_executable1(Name, [Base|Rest], Extensions) ->
    Complete0 = filename:join(Base, Name),
    case verify_executable(Complete0, Extensions) of
	{ok, Complete} ->
	    Complete;
	error ->
	    find_executable1(Name, Rest, Extensions)
    end;
find_executable1(_Name, [], _Extensions) ->
    false.

verify_executable(Name0, [Ext|Rest]) ->
    Name1 = Name0++Ext,
    case os:type() of
	vxworks ->
	    %% We consider all existing VxWorks files to be executable
	    case file:read_file_info(Name1) of
		{ok, _} ->
		    {ok, Name1};
		_ ->
		    verify_executable(Name0, Rest)
	    end;
	_ ->
	    case file:read_file_info(Name1) of
		{ok, #file_info{mode=Mode}} when Mode band 8#111 /= 0 ->
		    %% XXX This test for execution permission is not fool-proof on
		    %% Unix, since we test if any execution bit is set.
		    {ok, Name1};
		_ ->
		    verify_executable(Name0, Rest)
	    end
    end;
verify_executable(_, []) ->
    error.

split_path(Path) ->
    case type() of
	{win32, _} ->
	    {ok,Curr} = file:get_cwd(),
	    split_path(Path, $;, [], [Curr]);
	_ -> 
	    split_path(Path, $:, [], [])
    end.

split_path([Sep|Rest], Sep, Current, Path) ->
    split_path(Rest, Sep, [], [reverse_element(Current)|Path]);
split_path([C|Rest], Sep, Current, Path) ->
    split_path(Rest, Sep, [C|Current], Path);
split_path([], _, Current, Path) ->
    lists:reverse(Path, [reverse_element(Current)]).

reverse_element([]) ->
    ".";
reverse_element(List) ->
    lists:reverse(List).

extensions() ->
    case type() of
	{win32, _} -> [".exe", ".com", ".bat"];
	{unix, _} -> [""];
	vxworks -> [""]
    end.

%% Executes the given command in the default shell for the operating system.

cmd(Cmd) ->
    validate(Cmd),
    case type() of
	{unix, _} ->
	    unix_cmd(Cmd);
	{win32, Wtype} ->
	    Command = case {os:getenv("COMSPEC"),Wtype} of
			  {false,windows} -> lists:concat(["command.com /c", Cmd]);
			  {false,_} -> lists:concat(["cmd /c", Cmd]);
			  {Cspec,_} -> lists:concat([Cspec," /c",Cmd])
		      end,
	    Port = open_port({spawn, Command}, [stream, in, eof, hide]),
	    get_data(Port, []);
% VxWorks uses a 'sh -c hook' in 'vxcall.c' to run os:cmd.
	vxworks ->
	    Command = lists:concat(["sh -c '", Cmd, "'"]),
	    Port = open_port({spawn, Command}, [stream, in, eof]),
	    get_data(Port, [])
    end.

validate(Atom) when atom(Atom) ->
    ok;
validate(List) when list(List) ->
    validate1(List).

validate1([C|Rest]) when 0 =< C, C < 256 ->
    validate1(Rest);
validate1([List|Rest]) when list(List) ->
    validate1(List),
    validate1(Rest);
validate1([]) ->
    ok.

get_data(Port, Sofar) ->
    receive
	{Port, {data, Bytes}} ->
	    get_data(Port, [Sofar|Bytes]);
	{Port, eof} ->
	    Port ! {self(), close}, 
	    receive
		{Port, closed} ->
		    true
	    end, 
	    receive
		{'EXIT',  Port,  _} -> 
		    ok
	    after 1 ->				% force context switch
		    ok
	    end, 
	    lists:flatten(Sofar)
    end.

%% start()
%%
%% The -s flag implies that only the positional parameters are set,
%% and the commands are read from standard input. We set the 
%% $1 parameter for easy identification of the resident shell.
%%
-define(SHELL, "sh -s unix:cmd 2>&1").

ensure_started() ->
    case whereis(os_server) of
	undefined ->
	    C = {os_server, {os, start_link, []}, permanent,
		 1000, worker, [os]},
	    supervisor:start_child(kernel_safe_sup, C);
	_ -> ok
    end.

start_link() ->
    gen_server:start_link({local, os_server}, os, [], []).

unix_cmd(Cmd) ->
    ensure_started(),
    gen_server:call(os_server, {command, Cmd}, infinity).

%%
%% INIT
%%

init([]) ->
    process_flag(trap_exit, true),
    process_flag(priority, low),
    Port = start_port(),
    {ok, Port}.

%%
%% HANDLERS
%%

handle_call({command, Cmd}, _From, Port) ->
    case mk_cmd(Cmd) of
	{ok, C} ->
	    Port ! {self(), {command, C}},
	    {Exited, Result} = unix_get_data(Port),
	    Port0 = case Exited of 
			true ->
			    start_port();
			_ ->
			    Port
		    end,
	    {reply, Result, Port0};
	{error, Error} ->
	    {reply, Error, Port}
    end.

handle_info({'EXIT', Port, _}, Port) ->
    Port0 = start_port(),
    {noreply, Port0};
%% Occasional output after a ^D is discarded here.
handle_info(Info, Port) ->
    {noreply, Port}.

terminate(Reason, Port) ->
    ok.

%%
%% mk_cmd(Cmd) -> {ok, ShellCommandString} | {error, ErrorString}
%%
%% We do not allow any input to Cmd (hence commands that want
%% to read from standard input will return immediately).
%% Standard error is redirected to standard output.
%%
%% We use ^D (= EOT = 4) to mark the end of the stream.
%%
mk_cmd(Cmd) when atom(Cmd) ->			% backward comp.
    mk_cmd(atom_to_list(Cmd));
mk_cmd(Cmd) ->
    case file:get_cwd() of
	{ok, Cwd0} ->
	    Cwd = escape_string(Cwd0),

	    %% We insert a new line after the command, in case the command
	    %% contains a comment character.
	    {ok, io_lib:format("(cd \"~s\";~s\n) </dev/null; echo  \"\^D\"\n",
			       [Cwd, Cmd])};
	{error, Error} ->
	    {error, file:format_error(Error)}
    end.

%% Escapes dollars and double-quotes in a string.
escape_string([$$|T]) ->
    [$\\, $$| escape_string(T)];
escape_string([$"|T]) ->
    [$\\, $"| escape_string(T)];
escape_string([C|T]) ->
    [C| escape_string(T)];
escape_string([]) ->
    [].

%%
%%  start_port() -> Port
%%
start_port() ->
    open_port({spawn, ?SHELL},[stream]).

%%
%%  unix_get_data(Port) -> {Exited, Result}
%%
unix_get_data(Port) ->
    unix_get_data(Port, []).

unix_get_data(Port, Sofar) ->
    receive
	{Port,{data, Bytes}} ->
	    case eot(Bytes) of
		{done, Last} ->
		    {false, lists:flatten([Sofar| Last])};
		more  ->
		    unix_get_data(Port, [Sofar| Bytes])
	    end;
	{'EXIT', Port, _} ->
	    {true, lists:flatten(Sofar)}
    end.

%%
%% eot(String) -> more | {done, Result}
%%
eot(Bs) ->
    eot(Bs, []).

eot([4| Bs], As) ->
    {done, lists:reverse(As)};
eot([B| Bs], As) ->
    eot(Bs, [B| As]);
eot([], As) ->
    more.
