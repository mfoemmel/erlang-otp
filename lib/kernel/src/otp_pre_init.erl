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
-module(otp_pre_init).

%% Initialising before calling the init module.

-export([main/1]).

-import(os, [getenv/1,putenv/2]).

-record(st, {prog,				% Name of program.
	     all_args,
	     halt}).
						% Arguments for init module.
%% TODO:
%%
%% Halt if needed.
%% -nohup
%% -keep_window
%% -detached
%% -v
%% -x
%% +e, ERL_MAX_ETS_TABLES
%% +h size
%% +s size
%% +B
%% +l
%% +v (verbose, only in debug compiled code).
%% +V version
%% ERLANG_EMU_FLAGS
%% Error handling for HOME
%%

%% Needed modules:
%%  file_prim

%% Not supported.
%% -man
%% -x

main([Prog|Argv]) ->
    parse(Argv, [], #st{prog=progname()}).

progname() ->
    case getenv("PROGNAME") of
	false -> "erl";
	Name -> Name
    end.
	    
parse([Arg|As], Init, St) ->
    case arg(Arg, As) of
	ignore ->
	    parse(As, Init, St);
	include ->
	    parse(As, [Arg|Init], St);
	{init,Ias,MoreArgs} ->
	    parse(MoreArgs, reverse(Ias, Init), St);
	{error,String} ->
	    erlang:display_string(String),
	    erlang:display_nl(),
	    halt(1)
    end;
parse([], Init0, St) ->
    fix_path(),
    Home = get_home(),
    Root = get_root(),
    Init1 = reverse(Init0),
    Init2 = ["-root",Root,"-progname",St#st.prog,"--",
	     "-home",Home|Init1],
    Init = [list_to_atom(Arg) || Arg <- Init2],
%    erlang:display(Init),
    init:boot(Init).

arg("-compile", As) ->
    {init,["-noshell","-noinput","-s","c","lc_batch"],As};
arg("-env", As0) ->
    case As0 of
	[Key,Value|As] ->
	    putenv(Key, Value),
	    ignore;
	Other ->
	    {error,"Missing KEY and/or VALUE for -env."}
    end;
arg("-keep_window", As) ->
    {error,"Not implemented yet"};
arg("-man", As) ->
    {error,"The -man option is no longer supported"};
arg("-make", As) ->
    {init,["-noshell","-noinput","-s","make","all"],As};
arg("-name"=Opt, As0) ->
    name(Opt, As0);
arg("-noshell", As) ->
    {init,["-noshell", "-noinp_shell"],As};
arg("-noinput", As) ->
    {init,["-noshell", "-noinput"],As};
arg("-nohup", As) ->
    {error,"Not implemented yet"};
arg("-sname"=Opt, As0) ->
    name(Opt, As0);
arg("-version", As0) ->
    arg("+V", As0);
arg("-x", As) ->
    {error,"The '-x' option is no longer supported"};
arg("+e", As0) ->
    {error,"Not implemented yet"};
arg("+V", As0) ->
    erlang:display_string(erlang:info(system_version)),
    halt();
arg(Arg, As) ->
    include.

name(Opt, As0) ->
    case As0 of
	[Name|As] ->
	    erlang:open_port_prim({spawn,"epmd -daemon"}, []),
	    {init,[Opt,Name],As};
	[] ->
	    usage(Opt)
    end.

fix_path() ->
    BinDir = getenv("BINDIR"),
    Path = getenv("PATH"),
    case part_of(BinDir, Path) of
	false -> putenv("PATH", BinDir ++ ":" ++ Path);
	true -> ok
    end.

part_of(Substr, [_|T]=Str) ->
    case prefix(Substr, Str) of
	true -> true;
	false -> part_of(Substr, T)
    end;
part_of(Substr, []) -> false.

get_home() ->
    %% XXX We should do error checking here.
    case platform() of
	win32 ->
	    HomeDrive = getenv("HOMEDRIVE"),
	    HomePath = getenv("HOMEPATH"),
	    HomeDrive ++ HomePath;
	Other ->
	    getenv("HOME")
    end.

get_root() ->
    case getenv("ROOTDIR") of
	false -> error("No ROOTDIR environment variable.");
	Value -> Value
    end.
	    
error(Reason) ->
    erlang:display({error,Reason}),
    exit(oops).
    
usage(Opt) ->
    erlang:display({usage,Opt}),
    exit(oops).

%%% Utility functions.

reverse(List) -> lists:reverse(List, []).
reverse(List, Acc) -> lists:reverse(List, Acc).

platform() ->
    case erlang:info(os_type) of
	{unix,_} -> unix;
	{win32,_} -> win32;
	vxworks -> vxworks
    end.

%% prefix(Prefix, List) -> (true | false)

prefix([X|PreTail], [X|Tail]) ->
    prefix(PreTail, Tail);
prefix([], List) -> true;
prefix(_, _) -> false.
