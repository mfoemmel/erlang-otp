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
%% Purpose : Basic make facility

%% Compares date stamps of .erl and Object files - recompiles when
%% necessary.
%% Files to be checked are contained in a file 'Emakefile' 
%% If Emakefile is missing the current directory is used.

-module(make).

-export([all/0,all/1,files/1,files/2]).

-export([process_1_file/4]).

-include_lib("kernel/include/file.hrl").

all() ->
    all([]).

all(Options) ->
    case file:consult('Emakefile') of
	{ok,Files} ->
	    files(Files, Options);
	{error,enoent} ->
	    files(extract(file:list_dir(".")), Options);
	{error,Other} ->
	    error
    end.


%% extract(FileList)
%%  Extract all the files whose name end in ".erl".

extract({ok,Fs}) ->
    extract1(Fs);
extract({error,What}) ->
    [].

extract1([F|Fs]) ->
    case lists:suffix(".erl", F) of
	true ->
	    [lists:sublist(F, 1, length(F) - 4)|extract1(Fs)];
	false ->
	    extract1(Fs)
    end;
extract1([]) ->
    [].

files(Fs) ->
    files(Fs, []).

files(Fs, Opts) ->
    case lists:member(par, Opts) of
	true ->
	    F = process_flag(trap_exit, true),
	    Ns = [node()|nodes()],
	    Res = par_make(Fs, lists:member(noexec, Opts), load_opt(Opts), 
			   fix(Opts), Ns, []),
	    process_flag(trap_exit, F),
	    Res;
	
	false ->
	    process(Fs, lists:member(noexec, Opts), load_opt(Opts), Opts)
    end.

%% Any flags that are not recognixed as make flags are passed directly
%% to the compiler.
%% So for example make:all([load, par, trace]) will make everything
%% in parallel, compiling with trace code and load it.

fix([H|T]) -> 
    case lists:member(H,[par,noexec,load,netload,noload]) of
	true -> fix(T);
	false -> [H|fix(T)]
    end;
fix([]) -> [].
	
par_make([], _,_,_,_,[]) -> 
    up_to_date;
par_make([Job|Fs0], NoExec, Load, Opts, [Node|Nodes],Ack) ->
    Id = spawn_link(Node, make, process_1_file, [Job,NoExec, Load, Opts]),
    par_make(Fs0, NoExec, Load, Opts, Nodes, [{Id,Job} |Ack]);
par_make([Job|Fs0], NoExec, Load, Opts,[], Pids) ->
    receive
	{'EXIT' ,Pid, noconnection} ->
	    case lists:keysearch(Pid, 1, Pids) of
		false ->
		    par_make([Job|Fs0], NoExec, Load, Opts,[], Pids);
		{value,{_, Oldjob}} ->
		    par_make([Oldjob,Job|Fs0], NoExec, Load, Opts,[],
			     lists:keydelete(Pid,1,Pids))
	    end;
	{'EXIT', Pid, {parmake,Reason}} ->
	    Id = spawn_link(node(Pid), make, process_1_file, 
			    [Job,NoExec, Load, Opts]),
	    par_make(Fs0, NoExec, Load, Opts, [], 
		     [{Id,Job} | lists:keydelete(Pid,1,Pids)])
    end;
par_make([], NoExec, Load, Opts,Nodes, Pids) ->
    receive
	{'EXIT' ,Pid, noconnection} ->
	    case lists:keysearch(Pid, 1, Pids) of
		false ->
		    par_make([], NoExec, Load, Opts,[], Pids);
		{value,{_, Oldjob}} ->
		    par_make([Oldjob], NoExec, Load, Opts,[],
			     lists:keydelete(Pid,1,Pids))
	    end;

	{'EXIT', Pid, {parmake,Reason}} ->
	    par_make([], NoExec, Load, Opts,Nodes,lists:keydelete(Pid,1,Pids))
    end.

load_opt(Opts) ->
    case lists:member(netload,Opts) of
	true -> 
	    netload;
	false ->
	    case lists:member(load,Opts) of
		true ->
		    load;
		_ ->
		    noload
	    end
    end.

process([H|T], NoExec, Load, Opts) ->
    case recompilep(coerce_2_list(H), NoExec, Load, Opts) of
	error ->
	    error;
	_ ->
	    process(T, NoExec, Load, Opts)
    end;
process([], NoExec, Load, Opts) ->
    up_to_date.

%% Spawned by par_make
process_1_file(File, NoExec, Load, Opts) ->
    process_flag(trap_exit, true),
    R = (catch recompilep(coerce_2_list(File), NoExec, Load, Opts)),
    exit({parmake, R}).

recompilep(File, NoExec, Load, Opts) ->
    case exists(lists:append(filename:basename(File),
			     code:objfile_extension())) of
	true ->
	    recompilep1(File, NoExec, Load, Opts);
	false ->
	    recompile(File, NoExec, Load, Opts)
    end.
 
recompilep1(File, NoExec, Load, Opts ) ->
    {ok, Erl} = file:read_file_info(lists:append(File, ".erl")),
    {ok, Obj} = file:read_file_info(lists:append(filename:basename(File),
						 code:objfile_extension())),
    case {readable(Erl), writable(Obj)} of
	{true, true} ->
	    recompilep1(Erl, Obj, File, NoExec, Load, Opts);
	_ ->
	    error
    end.

recompilep1(#file_info{mtime=Te},
	    #file_info{mtime=To}, File, NoExec, Load, Opts) when Te > To ->
    recompile(File, NoExec, Load, Opts);
recompilep1(_Erl, #file_info{mtime=To}, File, NoExec, Load, Opts) ->
    recompile2(To, File, NoExec, Load, Opts).

%% recompile2(ObjMTime, File, NoExec, Load, Opts)
%% Check if file is of a later date than include files.
recompile2(ObjMTime, File, NoExec, Load, Opts) ->
    IncludePath = include_opt(Opts),
    case check_includes(lists:append(File, ".erl"), IncludePath, ObjMTime) of
	true ->
	    recompile(File, NoExec, Load, Opts);
	false ->
	    false
    end.

include_opt([{i,Path}|Rest]) ->
    [Path|include_opt(Rest)];
include_opt([_First|Rest]) ->
    include_opt(Rest);
include_opt([]) ->
    [].

%% recompile(File, NoExec, Load, Opts)
%% Actually recompile and load the file, depending on the flags.
%% Where load can be netload | load | noload

recompile(File, true, Load, Opts) ->
    io:format("Out of date: ~s\n",[File]);
recompile(File, false, noload, Opts) ->
    io:format("Recompile: ~s\n",[File]),
    compile:file(File, [report_errors, error_summary |Opts]);
recompile(File, false, load, Opts) ->
    io:format("Recompile: ~s\n",[File]),
    c:c(File, Opts);
recompile(File, false, netload, Opts) ->
    io:format("Recompile: ~s\n",[File]),
    c:nc(File, Opts).

exists(File) ->
    case file:read_file_info(File) of
	{ok, _} ->
	    true;
	_ ->
	    false
    end.

readable(#file_info{access=read_write}) -> true;
readable(#file_info{access=read})       -> true;
readable(_) -> false.

writable(#file_info{access=read_write}) -> true;
writable(#file_info{access=write})      -> true;
writable(_) -> false.

coerce_2_list(X) when atom(X) ->
    atom_to_list(X);
coerce_2_list(X) ->
    X.

%%% If you an include file is found with a modification
%%% time larger than the modification time of the object
%%% file, return true. Otherwise return false.
check_includes(File, IncludePath, ObjMTime) ->
    Path = [filename:dirname(File)|IncludePath], 
    case epp:open(File, Path, []) of
	{ok, Epp} ->
	    check_includes2(Epp, File, ObjMTime);
	Error ->
	    false
    end.
    
check_includes2(Epp, File, ObjMTime) ->
    case epp:parse_erl_form(Epp) of
	{ok, {attribute, 1, file, {File, 1}}} ->
	    check_includes2(Epp, File, ObjMTime);
	{ok, {attribute, 1, file, {IncFile, 1}}} ->
	    case file:read_file_info(IncFile) of
		{ok, #file_info{mtime=MTime}} when MTime > ObjMTime ->
		    epp:close(Epp),
		    true;
		_ ->
		    check_includes2(Epp, File, ObjMTime)
	    end;
	{ok, _} ->
	    check_includes2(Epp, File, ObjMTime);
	{eof, _} ->
	    epp:close(Epp),
	    false;
	{error, Error} ->
	    check_includes2(Epp, File, ObjMTime)
    end.
