%%<copyright>
%% <year>1997-2007</year>
%% <holder>Ericsson AB, All Rights Reserved</holder>
%%</copyright>
%%<legalnotice>
%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson AB.
%%</legalnotice>
%%
%%% Purpose : Autoconf for Windows.

-module(ts_autoconf_win32).
-export([configure/0]).

-include("ts.hrl").

configure() ->
    case variables() of
	{ok, Vars0} ->
	    Vars = [{obj,".obj"}, {exe, ".exe"}|Vars0],
	    ts_lib:subst_file("conf_vars.in", "conf_vars", Vars);
	Error ->
	    Error
    end.

variables() ->
    run_tests(tests(), []).

run_tests([{Prompt, Tester}|Rest], Vars) ->
    io:format("checking ~s... ", [Prompt]),
    case catch Tester(Vars) of
	{'EXIT', Reason} ->
	    io:format("FAILED~nExit status: ~p~n", [Reason]),
	    {error, auto_conf_failed};
	{Result, NewVars} ->
	    io:format("~s~n", [lists:concat([Result])]),
	    run_tests(Rest, NewVars)
    end;
run_tests([], Vars) ->
    {ok, Vars}.

%%% The tests.

tests() ->
    [{"host system type", fun system_type/1},
     {"CPU type", fun cpu/1},
     {"for Microsoft Visual C++", fun visual_cxx/1},
     {"for location of SSL libraries", fun ssl/1},
     {"for location of Java compiler", fun javac/1}].

system_type(Vars) ->
    Os = case os:type() of
	     {win32, nt} ->
		 case os:version() of
		     {4,_,_} -> "Windows NT";
		     {5,0,_} -> "Windows 2000";
		     {5,1,_} -> "Windows XP";
		     {5,2,_} -> "Windows 2003";
		     {_,_,_} -> "Windows NCC-1701-D"
		 end;
	     {win32, windows} -> 
		 case os:version() of
		     {4,0,_} ->  "Windows 95";
		     {4,10,_} -> "Windows 98"
		 end;
	     {win32, _} -> "Windows"
	 end,
    {Os, [{host_os, Os}, {host, "win32"}|Vars]}.

cpu(Vars) ->
    Arch = os:getenv("PROCESSOR_ARCHITECTURE"),
    Level0 = os:getenv("PROCESSOR_Level"),
    Cpu = case {Arch, Level0} of
	      {"x86", Level} when list(Level) ->
		  "i" ++ Level ++ "86";
	      {Other, _Level} when list(Other) ->
		  Other;
	      {false, _} ->
		  "i386"
	  end,
    {Cpu, [{host_cpu, Cpu}|Vars]}.
	
visual_cxx(Vars) ->
    case os:find_executable("cl") of
	false ->
	    {no, Vars};
	Path when list(Path) ->
	    {DEFAULT_THR_LIB,
	     ERTS_THR_LIB,
	     DLL,
	     DBG_LINK,
	     DBG_COMP,
	     OPT} =
		case is_debug_build() of
		    true ->
			{"-MTd ",
			 "-MDd ",
			 "-LDd ",
			 "-debug -pdb:none ",
			 "-Z7 -DDEBUG",
			 " "};
		    false ->
			{"-MT ",
			 "-MD ",
			 "-LD ",
			 " ",
			 " ",
			 "-Ox "}
		end,
	    WIN32 = "-D__WIN32__ ",
	    ERTS_CFLAGS = ERTS_THR_LIB ++ WIN32 ++ OPT ++ DBG_COMP,
	    LIBS = "ws2_32.lib",
	    CC = "cl -nologo",
	    {Path, [{'CC', CC},
		    {'LD', CC},
		    {'SHLIB_LD', CC},
		    {'SHLIB_LDFLAGS', ERTS_THR_LIB ++ DLL},
		    {'SHLIB_LDLIBS', "-link " ++ DBG_LINK ++ "kernel32.lib"},
		    {'SHLIB_EXTRACT_ALL', ""},
		    {'CFLAGS', DEFAULT_THR_LIB ++ WIN32 ++ DBG_COMP},
		    {'EI_CFLAGS', DEFAULT_THR_LIB ++ WIN32 ++ DBG_COMP},
		    {'ERTS_CFLAGS', ERTS_CFLAGS},
		    {'SHLIB_CFLAGS', ERTS_CFLAGS++DLL},
		    {'CROSSLDFLAGS', ""},
		    {'DEFS', ""},
		    {'SHLIB_SUFFIX', ".dll"},
		    {'ERTS_LIBS', ERTS_THR_LIB ++ LIBS},
		    {'LIBS', DEFAULT_THR_LIB ++ "-link " ++ DBG_LINK ++ LIBS}
		    | Vars]}
    end.

ssl(Vars) ->
    {"win32",[{'SSLEAY_ROOT',"win32"}|Vars]}.

javac(Vars) ->
    case os:find_executable("javac") of
	false ->
	    {no, Vars};
	Path when list(Path) ->
	    {Path, [{'JAVAC', "javac"} | Vars]}
    end.

is_debug_build() ->
    case catch string:str(erlang:system_info(system_version), "debug") of
	Int when integer(Int), Int > 0 ->
	    true;
	_ ->
	    false
    end.
