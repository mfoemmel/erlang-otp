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
-module(icgen).



%% Constructor for the generator structure
-export([new/1]).

%% File handling
%%-export([new_skelfile/4, close_skelfile/1, skelfile/1, skelfiled/1]).
%%-export([new_stubfile/3, close_stubfile/1, stubfile/1, stubfiled/1]).
%%-export([new_hrlfile/2, close_hrlfile/1]). 
-export([include_file/1, include_file_stack/1]).
-export([stubfiled/1, skelfiled/1, hrlfiled/1]).
-export([skelscope/1]). %%, set_skelscope/2]).
-export([stubscope/1]). %%, set_stubscope/2]).
-export([filename_push/4, filename_pop/2]).
-export([is_skelfile_open/1, is_stubfile_open/1, is_hrlfile_open/1]).
-export([remove_ext/1]).
-export([emit_stub_head/4,nl/1,join/2]).
-export([gen_includes/3,gen_includes/4]).


%%-export([add_free_def/3, get_free_def/2]).

%% Symtab handling
-export([symtab/1]). %%, set_symtab/2]).
-export([symtab_new/0]).
-export([symtab_store/3, symtab_retrieve/2, symtab_intf_resolv/3]).

%% Auxillary table
-export([auxtab/1]).

%% Type Kind table
-export([tktab/1]).

%% Pragma table
-export([pragmatab/1]).

%% Option table
-export([optiontab/1]).

%% Option check
-export([allowed_opt/2]).

%% Sequence Type table, is used to keep track of all sequence head types that is generated
-export([typedeftab/1, get_basetype/2, insert_typedef/3]).

%% ETS-table removal
-export([free_table_space/1,table_space/1,process_space/0]).

%% Scoped identifier handling
-export([scoped_id_new/1, scoped_id_new_global/1, scoped_id_add/2, 
	 scoped_id_strip/1, scoped_id_is_global/1, get_full_scoped_name/3]).
%%, scoped_id_new_global/0, 
%%	 scoped_id_is_top/1, %%scoped_id_2str/1,
%%%%	 scoped_id_2str2/1, 
%%]).

%% Pre proc file stack handling
-export([push_file/2, pop_file/2, sys_file/2, do_gen/1]).

%% Scope handling
%%-export([scope_new/0, scope_push/3, scope_pop/1, scope_add/3, scope_add/4]).

%% Error and warning handling
-export([return/1, get_error_count/1]).
-export([error/2, fatal_error/2, format_error/1]).
-export([warn/2, format_warn/1]). %%, reset_warn/1]).

%% Option handling
-export([add_opt/3, get_opt/2, which_opts/1]).
-export([set_idlfile/2, set_module/2]). %%, set_impl/2]).
-export([idlfile/1, module/1, impl/1]).

%% Utility department
-export([to_colon/1, to_undersc/1]).
-export([is_oneway/1]).
-export([emit/2, emit/3]).
-export([mk_name/2, mk_oe_name/2, mk_var/1, mk_list/1, mk_align/1]).
-export([get_id/1, get_id2/1, get_idlist/1]).
-export([get_dimension/1]).
-export([get_line/1, get_body/1, get_type/1]).
-export([to_atom/1, to_list/1]).
-export([add_dot_erl/1, add_dot_c/1, add_dot_idl/1]).
-export([comment/2, comment/3, mcomment/2, mcomment_light/2]).
-export([comment/4, mcomment/3, mcomment_light/3]).
-export([nl/1,defaultBe/0,codeDirective/2]).
-export([export/2, record/5]).
%%-export([idlist2str/1, idlist2estr/1]).

-export([float_to_version/1]).


-define(fd_get(FDName, FuncName), FDName(G) -> G#genobj{FDName=file_open(hd(G#genobj.FDName), hd(G#genobj.FuncName))}).


%%-import(lists, [foreach/2]).

-include("icforms.hrl").
-include("ic.hrl").
-include_lib("kernel/include/file.hrl").



%%--------------------------------------------------------------------
%%
%% Initialisation stuff
%%
%% 
%%
%%--------------------------------------------------------------------


new(Opts) ->
    OptDB	= ets:new(options, [set, public]),
    Warns	= ets:new(warnings, [set, public]),
    Aux		= ets:new(aux, [set, public]),
    Tk		= ets:new(tktab, [set, public]),
    PragmaTab   = ets:new(pragmatab, [bag, public]),
    TypeDefTab  = ets:new(c_typedeftab, [set, public]),
    KeyWordTab  = icscan:add_keyw(),
    G = #genobj{options=OptDB, 
		warnings=Warns, 
		symtab=symtab_new(), 
		auxtab=Aux, 
		tktab=Tk, 
		pragmatab=PragmaTab,
		c_typedeftab=TypeDefTab,
		keywtab=KeyWordTab},
    init_errors(G),
    add_opt(G, default_opts, true),
    read_cfg(G, Opts),				% Read any config files
    add_opt(G, Opts, true),
    G.



%%--------------------------------------------------------------------
%%
%% Table removal
%%
%% 
%%
%%--------------------------------------------------------------------

free_table_space(G) ->
    %% Free ets tables
    ets:delete(G#genobj.options),
    ets:delete(G#genobj.symtab),
    ets:delete(G#genobj.warnings),
    ets:delete(G#genobj.auxtab),
    ets:delete(G#genobj.tktab),
    ets:delete(G#genobj.pragmatab),
    ets:delete(G#genobj.c_typedeftab),
    ets:delete(G#genobj.keywtab),    
    %% Close file descriptors
    close_fd(G#genobj.skelfiled),
    close_fd(G#genobj.stubfiled),
    close_fd(G#genobj.interfacefiled),
    close_fd(G#genobj.helperfiled),
    close_fd(G#genobj.holderfiled),
    close_fd(G#genobj.includefiled).

close_fd([]) ->
    ok;
close_fd([Fd|Fds]) ->
    file_close(Fd),
    close_fd(Fds).



%%--------------------------------------------------------------------
%%
%% Table memory usage
%%
%% 
%%
%%--------------------------------------------------------------------

table_space(G) ->
    OptT=4*element(2,element(1,ets:info(G#genobj.options))),
    SymT=4*element(2,element(1,ets:info(G#genobj.symtab))),
    WarT=4*element(2,element(1,ets:info(G#genobj.warnings))),
    AuxT=4*element(2,element(1,ets:info(G#genobj.auxtab))),
    TkT=4*element(2,element(1,ets:info(G#genobj.tktab))),
    PraT=4*element(2,element(1,ets:info(G#genobj.pragmatab))),
    TypT=4*element(2,element(1,ets:info(G#genobj.c_typedeftab))),
    io:format("Option table ends at    ~p bytes\n",[OptT]),
    io:format("Symbol table ends at    ~p bytes\n",[SymT]),
    io:format("Warning table ends at   ~p bytes\n",[WarT]),
    io:format("Aux table ends at       ~p bytes\n",[AuxT]),
    io:format("Typecode table ends at  ~p bytes\n",[TkT]),
    io:format("Pragma table ends at    ~p bytes\n",[PraT]),
    io:format("Typedef table ends at   ~p bytes\n",[TypT]),
    io:format("-----------------------------------------------\n"),
    io:format("Totally used  ~p bytes\n",[OptT+SymT+WarT+AuxT+TkT+PraT+TypT]).



%%--------------------------------------------------------------------
%%
%% Process memory usage
%%
%% 
%%
%%--------------------------------------------------------------------

process_space() ->
    Pheap=4*element(2,element(2,lists:keysearch(heap_size,1,process_info(self())))),
    Pstack=4*element(2,element(2,lists:keysearch(stack_size,1,process_info(self())))),
    io:format("Process current heap = ~p bytes\n",[Pheap]),
    io:format("Symbol current stack = ~p bytes\n",[Pstack]),
    io:format("-----------------------------------------------\n"),
    io:format("Totally used  ~p bytes\n\n",[Pheap+Pstack]).






%%--------------------------------------------------------------------
%%
%% File handling stuff
%%
%% 
%%	Shall open a file for writing. Also sets up the generator with
%%	usefull bits of information
%%
%%--------------------------------------------------------------------

find_skel_name(G, Name) ->
    N1 = to_colon(Name),
    N2 = to_undersc(Name),
    case {get_opt(G, {serv, N1}),
	  get_opt(G, {serv, N2})} of
	{false, false} -> N2 ++ "_serv";
	{X, Y} when X /= false -> to_list(X);
	{X, Y} when Y /= false -> to_list(Y)
    end.

find_impl_name(G, Name) ->
    N1 = to_colon(Name),
    N2 = to_undersc(Name),
    case {get_opt(G, {impl, N1}),
	  get_opt(G, {impl, N2})} of
	{false, false} ->
	    case {get_opt(G, {impl, "::"++N1}),
		  get_opt(G, {impl, N2})} of
		{false, false} -> N2 ++ "_impl";
		{X, Y} when X /= false -> to_list(X);
		{X, Y} when Y /= false -> to_list(Y)
	    end;
	{X, Y} when X /= false -> to_list(X);
	{X, Y} when Y /= false -> to_list(Y)
    end.




%% Pushes a file name, can also push ignore in which case means that
%% no files should ever be opened at this scope. Note that empty in
%% the file descriptor entries means that the file just isn't open
%% yet.
filename_push(G, N, ignore, _) ->
    G#genobj{stubfile=[ignore | G#genobj.stubfile],
	     stubfiled=[ignore | G#genobj.stubfiled],
	     skelfile=[ignore | G#genobj.skelfile],
	     skelfiled=[ignore | G#genobj.skelfiled],
    	     includefile=[ignore | G#genobj.includefile],
	     includefiled=[ignore | G#genobj.includefiled]};

filename_push(G, N, X, Lang) ->
    Fullname = [get_id2(X) | N],
    EName = to_undersc(Fullname),
    
    DoGen = do_gen(G),

    ImplName = find_impl_name(G, Fullname),

    SkelName=undefined, SkelFName=undefined, Skel=undefined,
    
    StubName = case Lang of
		   erlang ->
		       %%?ifopt2(G, gen_stub, 
		       join(get_opt(G, stubdir), add_dot_erl(EName));
		   %%	       ignore);
		   c ->
		       %%?ifopt2(G, gen_stub, 
		       join(get_opt(G, stubdir), add_dot_c(EName));
			 %%      ignore);
		   c_server ->
		       %%?ifopt2(G, gen_stub, 
		       join(get_opt(G, stubdir), add_dot_c(EName++"__s"));
		   %%      ignore);
		   erlang_no_stub ->
		       undefined;
		   c_no_stub ->
		       undefined; 
		   c_server_no_stub ->
		       undefined  
	       end,
    Stub = if DoGen==true -> 
		   case StubName of
		       undefined ->
			   ignore;
		       _ ->
			   emit_stub_head(G, file_open(empty, StubName), EName, Lang)
		   end;
	      true -> ignore end,
    
    HrlName = case Lang of
		  erlang ->
		      ?ifopt2(G, gen_hrl, 
			      join(get_opt(G, stubdir), add_dot_hrl(EName)),
			      ignore);
		  c ->
		      ?ifopt2(G, gen_hrl, 
			      join(get_opt(G, stubdir), add_dot_h(EName)),
			      ignore);
		  c_server ->
		      ?ifopt2(G, gen_hrl, 
			      join(get_opt(G, stubdir), add_dot_h(EName++"__s")),
			      ignore);
		  erlang_no_stub ->
		      ?ifopt2(G, gen_hrl, 
			      join(get_opt(G, stubdir), add_dot_hrl(EName)),
			      ignore);
		  c_no_stub ->
		      ?ifopt2(G, gen_hrl, 
			      join(get_opt(G, stubdir), add_dot_h(EName)),
			      ignore);
		  c_server_no_stub ->
		      ?ifopt2(G, gen_hrl, 
			      join(get_opt(G, stubdir), add_dot_h(EName++"__s")),
			      ignore)
	      end,
    Hrl = if  DoGen==true -> 
		  case Lang of
		      erlang_no_stub ->
			  emit_hrl_head(G, file_open(empty, HrlName), EName, erlang);
		      c_no_stub ->
			  emit_hrl_head(G, file_open(empty, HrlName), EName, c);
		      c_server_no_stub ->
			  emit_hrl_head(G, file_open(empty, HrlName), EName, c_server);
		      _ ->
			  emit_hrl_head(G, file_open(empty, HrlName), EName, Lang)
		  end;
	      true -> ignore end,
    
    G#genobj{impl=ImplName,
	     %%	     skelfile=[SkelFName | G#genobj.skelfile],
	     %%	     skelfiled=[Skel | G#genobj.skelfiled],
	     stubfile=[StubName | G#genobj.stubfile],
	     stubfiled=[Stub | G#genobj.stubfiled],
	     includefile=[HrlName | G#genobj.includefile],
	     includefiled=[Hrl | G#genobj.includefiled]}.

%% special join:
join([], File) ->
    File;
join(Path, File) ->
    filename:join(Path, File).


filename_pop(G, Lang) ->
%%    io:format("Popped file names: ~p~n", [hd(G#genobj.stubfile)]),
%%    case is_skelfile_open(G) of
%%	true -> emit_skel_footer(G);
%%	false -> ok end,
%%    file_close(hd(G#genobj.skelfiled)),
    file_close(hd(G#genobj.stubfiled)),
    emit_hrl_foot(G, Lang),
    file_close(hd(G#genobj.includefiled)),
    G#genobj{stubfile=tl(G#genobj.stubfile),
	     stubfiled=tl(G#genobj.stubfiled),
%%	     skelfile=tl(G#genobj.skelfile),
%%	     skelfiled=tl(G#genobj.skelfiled),
    	     includefile=tl(G#genobj.includefile),
	     includefiled=tl(G#genobj.includefiled)}.

file_close(empty) -> ok;
file_close(ignore) -> ok;
file_close(Fd) -> 
    file:close(Fd).

remove_ext(File) ->
    filename:rootname(filename:basename(File)).

skelfiled(G) -> hd(G#genobj.skelfiled).
stubfiled(G) -> hd(G#genobj.stubfiled).
hrlfiled(G) -> hd(G#genobj.includefiled).

include_file(G) -> hd(G#genobj.includefile).
include_file_stack(G) -> G#genobj.includefile.



emit_stub_head(G, ignore, Name, _) -> ignore;
emit_stub_head(G, F1, Name, erlang) ->
    mcomment(F1, ["Standard implementation stubs.",
		  "",
		  io_lib:format("This file has been generated from ~p,",
				[icgen:idlfile(G)]),
		  io_lib:format("by using the IC compiler, version ~s .",
				[?COMPILERVSN]),
		  "",
		  "DO NOT EDIT THIS FILE."]),
    nl(F1),
    emit(F1, "-module(~p).\n\n", [list_to_atom(Name)]),
    emit(F1, "\n\n"), F1;
emit_stub_head(G, F1, Name, c) ->
    mcomment(F1, ["Standard implementation stubs.",
		  "",
		  io_lib:format("This file has been generated from ~p,",
				[icgen:idlfile(G)]),
		  io_lib:format("by using the IC compiler, version ~s .",
				[?COMPILERVSN]),
		  "",
		  "DO NOT EDIT THIS FILE."], c),
    emit(F1, "\n\n"), F1;
emit_stub_head(G, F1, Name, c_server) ->
    mcomment(F1, ["Standard implementation stubs.",
		  "",
		  io_lib:format("This file has been generated from ~p,",
				[icgen:idlfile(G)]),
		  io_lib:format("by using the IC compiler, version ~s .",
				[?COMPILERVSN]),
		  "",
		  "DO NOT EDIT THIS FILE."], c),
    emit(F1, "\n\n"), F1.

%% Name is Fully scoped (undescore) name of interface or module    
emit_hrl_head(G, ignore, Name, _) -> ignore;
emit_hrl_head(G, Fd, Name, erlang) ->
    mcomment(Fd, ["Standard Erlang header file.",
		  "",
		  io_lib:format("This file has been generated from ~p,",
				[icgen:idlfile(G)]),
		  io_lib:format("by using the IC compiler, version ~s .",
				[?COMPILERVSN]),
		  "",
		  "DO NOT EDIT THIS FILE."]),
    nl(Fd),
    nl(Fd),
    IfdefName = ic_util:to_uppercase(Name++"_HRL"),
    emit(Fd, "-ifndef(~s).~n", [IfdefName]),
    emit(Fd, "-define(~s, true).~n", [IfdefName]),
    nl(Fd),
    nl(Fd),
    Fd;
emit_hrl_head(G, Fd, Name, c) ->
    mcomment(Fd, ["Standard C header file.",
		  "",
		  io_lib:format("This file has been generated from ~p,",
				[icgen:idlfile(G)]),
		  io_lib:format("by using the IC compiler, version ~s .",
				[?COMPILERVSN]),
		  "",
		  "DO NOT EDIT THIS FILE."], c),
    nl(Fd),
    nl(Fd),
    IfdefName = ic_util:to_uppercase(Name++"_H"),
    emit(Fd, "#ifndef ~s~n", [IfdefName]),
    emit(Fd, "#define ~s ~n", [IfdefName]),
    nl(Fd),
    nl(Fd),
    Fd;
emit_hrl_head(G, Fd, Name, c_server) ->
    mcomment(Fd, ["Standard C header file.",
		  "",
		  io_lib:format("This file has been generated from ~p,",
				[icgen:idlfile(G)]),
		  io_lib:format("by using the IC compiler, version ~s .",
				[?COMPILERVSN]),
		  "",
		  "DO NOT EDIT THIS FILE."], c),
    nl(Fd),
    nl(Fd),
    IfdefName = ic_util:to_uppercase(Name++"__S_H"),
    emit(Fd, "#ifndef ~s~n", [IfdefName]),
    emit(Fd, "#define ~s ~n", [IfdefName]),
    nl(Fd),
    nl(Fd),
    Fd.



emit_hrl_foot(G, erlang) ->
    case is_hrlfile_open(G) of
	true ->
	    Fd = hrlfiled(G),
	    nl(Fd),
	    nl(Fd),
	    emit(Fd, "-endif.\n"),
	    nl(Fd),
	    nl(Fd),
	    Fd;
	false ->
	    ok
    end;
emit_hrl_foot(G, erlang_no_stub) ->
    case is_hrlfile_open(G) of
	true ->
	    Fd = hrlfiled(G),
	    nl(Fd),
	    nl(Fd),
	    emit(Fd, "-endif.\n"),
	    nl(Fd),
	    nl(Fd),
	    Fd;
	false ->
	    ok
    end;
emit_hrl_foot(G, c) ->
    case is_hrlfile_open(G) of
	true ->
	    Fd = hrlfiled(G),
	    nl(Fd),
	    nl(Fd),
	    emit(Fd, "#ifdef __cplusplus\n"),
            emit(Fd, "}\n"),
            emit(Fd, "#endif\n"),
            nl(Fd),
	    emit(Fd, "#endif\n"),
	    nl(Fd),
	    nl(Fd),
	    Fd;
	false ->
	    ok
    end;
emit_hrl_foot(G, c_server) ->
    case is_hrlfile_open(G) of
	true ->
	    Fd = hrlfiled(G),
	    nl(Fd),
	    nl(Fd),
	    emit(Fd, "#ifdef __cplusplus\n"),
            emit(Fd, "}\n"),
            emit(Fd, "#endif\n"),
            nl(Fd),
	    emit(Fd, "#endif\n"),
	    nl(Fd),
	    nl(Fd),
	    Fd;
	false ->
	    ok
    end;
emit_hrl_foot(G, c_no_stub) ->
    case is_hrlfile_open(G) of
	true ->
	    Fd = hrlfiled(G),
	    nl(Fd),
	    nl(Fd),
	    emit(Fd, "#ifdef __cplusplus\n"),
            emit(Fd, "}\n"),
            emit(Fd, "#endif\n"),
            nl(Fd),
	    emit(Fd, "#endif\n"),
	    nl(Fd),
	    nl(Fd),
	    Fd;
	false ->
	    ok
    end;
emit_hrl_foot(G, c_server_no_stub) ->
    case is_hrlfile_open(G) of
	true ->
	    Fd = hrlfiled(G),
	    nl(Fd),
	    nl(Fd),
	    emit(Fd, "#ifdef __cplusplus\n"),
            emit(Fd, "}\n"),
            emit(Fd, "#endif\n"),
            nl(Fd),
	    emit(Fd, "#endif\n"),
	    nl(Fd),
	    nl(Fd),
	    Fd;
	false ->
	    ok
    end.



%%file_open({ok, Fd}, _) -> Fd;
file_open(_, ignore) -> ignore;
file_open(empty, Name) -> 
    case file:rawopen(Name, {binary, write}) of
	{ok, Fd} -> Fd;
	Other -> exit(Other)
    end.

is_skelfile_open(G) ->
    if  hd(G#genobj.skelfiled) /= empty, hd(G#genobj.skelfiled) /= ignore
	-> true;
	true -> false
    end.
is_stubfile_open(G) ->
    if  hd(G#genobj.stubfiled) /= empty, hd(G#genobj.stubfiled) /= ignore
	-> true;
	true -> false
    end.
is_hrlfile_open(G) ->
    if  hd(G#genobj.includefiled) /= empty, hd(G#genobj.includefiled) /= ignore
	-> true;
	true -> false
    end.


%%--------------------------------------------------------------------
%%
%% Generation primitives
%%
%% 
%%
%%--------------------------------------------------------------------

%% Emit outputs a formatted string 
emit(nil, _) -> ok;
emit(F, Str) ->
    file:write(F, Str).

emit(nil, _, _) -> ok;
emit(F, Format, Args) ->
    file:write(F, io_lib:format(Format, Args)).


comment(Fd, C) ->
    comment(Fd, C, [], "%%").
comment(Fd, C, A) ->
    comment(Fd, C, A, "%%").
comment(Fd, C, A, c) -> 
    emit(Fd, "/* " ++ C ++ " */\n", A);
comment(Fd, C, A, CommentSequence) ->
    emit(Fd, CommentSequence ++ " " ++ C ++ "\n", A).

%% Outputs multiline comments with nice delimiters
%% Input arg is a list of strings
mcomment(Fd, List) ->
    mcomment(Fd, List, "%%").

mcomment(Fd, List, c) ->
    emit(Fd, "/*"
	 "------------------------------------------------------------\n"),
    emit(Fd, " *" ++ "\n"),
    lists:foreach(fun(C) -> comment(Fd, C, [], " *") end, List),
    emit(Fd, " *" ++ "\n"),
    emit(Fd, " *" 
	 "------------------------------------------------------------*/\n"),
    ok;
mcomment(Fd, List, CommentSequence) ->
    emit(Fd, CommentSequence ++
	 "------------------------------------------------------------\n"),
    emit(Fd, CommentSequence ++ "\n"),
    lists:foreach(fun(C) -> comment(Fd, C, [], CommentSequence) end, List),
    emit(Fd, CommentSequence ++ "\n"),
    emit(Fd, CommentSequence ++
	 "------------------------------------------------------------\n"),
    ok.


%% As above but a little lighter
%% 
mcomment_light(Fd, List) ->
    mcomment_light(Fd, List, "%%").
mcomment_light(Fd, List, c) ->
    emit(Fd, "\n/*\n"),
    lists:foreach(fun(C) -> comment(Fd, C, [], " *") end, List),
    emit(Fd, " */\n"),
    ok;
mcomment_light(Fd, List, CommentSequence) ->
    emit(Fd, CommentSequence),
    lists:foreach(fun(C) -> comment(Fd, C, [], CommentSequence) end, List),
    emit(Fd, CommentSequence ++ "\n"),
    ok.

nl(Fd) ->
    emit(Fd, "\n").

-define(IFRIDFIELD(G), mk_name(G, "ID")).



record(G, X, Name, _IFRID, Recs) when record(X, struct) ->
    F = hrlfiled(G),
    emit(F, "-record(~p, {~p", [to_atom(Name),hd(Recs)]),
    lists:foreach(fun(X) -> emit(F, ", ~p", [X]) end, tl(Recs)),
    emit(F, "}).\n");
record(G, X, Name, _IFRID, _Recs) when record(X, union) ->
    F = hrlfiled(G),
    emit(F, "-record(~p, {label, value}).\n",[to_atom(Name)]);
record(G, X, Name, IFRID, Recs) when length(Recs) > 3 ->
    F = hrlfiled(G),
    emit(F, "-record(~p,~n        {~p=~p", 
	 [to_atom(Name), to_atom(?IFRIDFIELD(G)), IFRID]),
    rec2(F, "", ", ", Recs),
    emit(F, "}).\n");
record(G, X, Name, IFRID, Recs) ->
    F = hrlfiled(G),
    emit(F, "-record(~p, {~p=~p", [to_atom(Name), to_atom(?IFRIDFIELD(G)),
				     IFRID]),
    lists:foreach(fun(X) -> emit(F, ", ~p", [X]) end, Recs),
    emit(F, "}).\n").


rec2(F, Align, Delim, [M1 , M2, M3 | Ms]) ->
    emit(F, "~s~s~p, ~p, ~p", [Delim, Align, M1, M2, M3]),
    rec2(F, "         ", ",\n", Ms);
rec2(F, Align, Delim, [M1 , M2]) ->
    emit(F, "~s~s~p, ~p", [Delim, Align, M1, M2]);
rec2(F, Align, Delim, [M]) ->
    emit(F, "~s~s~p", [Delim, Align, M]);
rec2(F, Align, Delim, []) ->
    ok.


export(F, [E1, E2, E3 | Exports]) ->
    emit(F, "-export([~s]).\n", [exp_list([E1, E2, E3])]),
    export(F, Exports);
export(F, []) -> ok;
export(F, Exports) ->
    emit(F, "-export([~s]).\n", [exp_list(Exports)]).

exp_list([E1 | L]) ->
    exp_to_string(E1) ++ 
	lists:map(fun(E) -> ", " ++ exp_to_string(E) end, L).


exp_to_string({F,N}) -> io_lib:format("~p/~p", [to_atom(F), N]).


%%--------------------------------------------------------------------
%%
%% Handling of "free" (no obvious file) definitions
%%
%% There will always be things in global scope or in a scope such that
%% no file is open when they are found. It happens if the output file
%% is on interface level, but some constants are on module level for
%% instance.
%%
%% All defs on a certain scope level must go to the same file and
%% because we can only have one file open at a time we must store
%% eveything we're uncertain of and print tehm all at the
%% end. Printing will be done at the end of source idl file.
%%
%%--------------------------------------------------------------------
%%add_free_def(G, Scope, Def) -> ok.
%%    ets:insert(scopetab(G), {Scope, Def}).

%%get_free_def(G, Scope) -> [].
%%    ets:lookup(scopetab(G), Scope).




%%--------------------------------------------------------------------
%%
%% Handling of pre processor file commands
%%
%%
%%--------------------------------------------------------------------

push_file(G, Id) ->
    %%    io:format("Push file: ~p", [Id]),
    New = G#genobj.filestack+1,
    set_idlfile(G, Id),
    G#genobj{filestack=New, do_gen=true_or_not(New)}.
pop_file(G, Id) ->
%%    io:format("Pop file: ~p", [Id]),
    New = G#genobj.filestack-1,
    set_idlfile(G, Id),
    G#genobj{filestack=New, do_gen=true_or_not(New)}.
sys_file(G, Id) -> G#genobj{sysfile=true}.

true_or_not(X) when X < 2 -> 
%%    io:format(" (gen on)~n", []),
    true;
true_or_not(_) -> 
%%    io:format(" (gen off)~n", []),
    false.


do_gen(G) -> G#genobj.do_gen.


%%--------------------------------------------------------------------
%%
%% Error and warning utilities.
%%
%% Note that errors are somewhat brutal and that warnings are kept in
%% a list for the user to extract at a later stage. The handling of
%% warnings is entirely up to the user while handling of errors is
%% never left to the user.
%%
%%--------------------------------------------------------------------

return(G) ->
    case get_opt(G, silent2) of
	true ->
	    case get_error_count(G) of
		0 -> {ok, get_list(G, warn_list)};
		X -> {error, get_list(G, warn_list), get_list(G, error_list)}
	    end;
	false ->
	    case get_error_count(G) of
		0 -> ok;
		X -> print_error(G, {error, g, idlfile(G), {error_count, X}}),
		     error
	    end
    end.


get_list(G, ListName) ->
    ErrList = ?lookup(G#genobj.options, ListName).


%% Public function for reporting an error
error(G, Err) ->
    Error = {error, g, idlfile(G), Err},
    case insert_in_list(G, Error, error_list) of
	new ->
	    print_error(G, Error),
	    MaxErrs = get_opt(G, maxerrs),
	    case incr_counter(G, error_count) of
		X when X >= MaxErrs ->
		    fatal_error(G, {error_count_exceeded, X});
		_ -> Error
	    end;
	old -> 
	    Error
end.

%% Public function for reporting an error. NOTE: also stops execution
fatal_error(G, Err) ->
    Error = {error, g, idlfile(G), Err},
    insert_in_list(G, Error, error_list),
    incr_counter(G, error_count),
    print_error(G, Error),
    throw(Error).


%% Public function for reporting a warning
warn(G, Warn) ->
    Warning = {warn, g, idlfile(G), Warn},
    case insert_in_list(G, Warning, warn_list) of
	new ->
	    print_warn(G, Warning),
	    MaxErrs = get_opt(G, maxwarns),
	    case incr_counter(G, warn_count) of
		X when X >= MaxErrs ->
		    fatal_error(G, {warn_count_exceeded, X});
		_ -> ok
	    end;
	old -> ok
end.


%% Initialisation of all counters and lists associated with errors and
%% warnings.
init_errors(G) ->
    reset_counter(G, error_count),
    reset_counter(G, warn_count),
    reset_list(G, error_list),
    reset_list(G, warn_list),
    ok.



%%--------------------------------------------------------------------
%% Counter and list (warn and error) handling
%%

incr_counter(G, Counter) ->
    Num = ?lookup(G#genobj.options, Counter) + 1,
    ?insert(G#genobj.options, Counter, Num),
    Num.

reset_counter(G, Counter) ->
    ?insert(G#genobj.options, Counter, 0).

get_error_count(G) ->
    ?lookup(G#genobj.options, error_count).

reset_list(G, ListName) ->
    ?insert(G#genobj.options, ListName, []).

insert_in_list(G, Item, ListName) ->
    List = ?lookup(G#genobj.options, ListName),
    case lists:member(Item, List) of
	true -> old;
	false ->
	    ?insert(G#genobj.options, ListName, [Item| List]),
	    new
    end.


%%--------------------------------------------------------------------
%% 
%% Nice printouts of errors and warnings
%%


%% Errors

print_error(G, Error) ->
    case {get_opt(G, silent), get_opt(G, silent2)} of
	{true, _} -> ok;
	{_, true} -> ok;
	_ -> format_error(Error)
    end,
    error.

%%format_error({error, G, Err}) ->
%%    case {get_opt(G, silent), get_opt(G, silent2)} of
%%	{true, _} -> ok;
%%	{_, true} -> ok;
%%	_ -> format_error2({error, G, Err})
%%    end.

format_error({error, _, File, {parse_error, Line, Args}}) ->
    Fmt = lists:foldl(fun(_, Acc) -> [$~, $s | Acc] end, [], Args),
    display(File, Line, Fmt, Args);
format_error({error, _, File, {error_count, X}}) ->
    display(File, "~p errors found", [X]);
format_error({error, _, File, {error_count_exceeded, X}}) ->
    display(File, "too many errors found (~p)", [X]);
format_error({error, _, File, {warn_count_exceeded, X}}) ->
    display(File, "too many warnings found (~p)", [X]);
format_error({error, _, File, {inherit_name_collision, 
			       {Orig, Item}, {Base, NewItem}}}) ->
    display(File, get_line(Item), "~s collides with ~s", 
	    [pp([get_id2(Item) | Orig]), pp([get_id2(NewItem) | Base])]);
format_error({error, _, File, {unsupported_op, {'~', Line}}}) ->
    display(File, Line, "unsupported unary operation ~~", []);
format_error({error, _, File, {multiply_defined, X}}) ->
    display(File, get_line(X), "multiple defined identifier ~p", [get_id2(X)]);
format_error({error, _, File, {illegal_spelling, X}}) ->
    display(File, get_line(X), 
%	    "illegal spelling of identifier ~s (capitalisation?)", 
	    "identifier ~p is multiply declared - differs in case only",
	    [get_id2(X)]);
format_error({error, _, File, {illegal_enumerant_value, X}}) ->
    display(File, get_line(X), 
	    "Enumerant ~s's value collide by name with other type", 
	    [get_id2(X)]);
format_error({error, _, File, {illegal_forward, X}}) ->
    display(File, get_line(X), 
	    "cannot inherit from forwarded interface ~s", [get_id2(X)]);
format_error({error, _, File, {illegal_const_t, X, Type}}) ->
    display(File, get_line(X), 
	    "Illegal constant type ~s of ~s", [pp(Type), get_id2(X)]);
format_error({error, _, File, {multiple_cases, X}}) ->
    display(File, get_line(X), "multiple case values ~s", [pp(X)]);
format_error({error, _, File, {symtab_not_found, X}}) ->
    display(File, get_line(X), "undeclared identifier ~s", [get_id2(X)]);
format_error({error, _, File, {preproc, Lines}}) ->
    display(File, "preprocessor error: ~s", [hd(Lines)]);
format_error({error, _, File, {ic_pp_error, Lines}}) ->
    display(File, "preprocessor error: ~s", [Lines]);
format_error({error, _, File, {illegal_float, Line}}) ->
    display(File, Line, "illegal floating point number", []);
format_error({error, _, File, {bad_type_combination, E, V1, V2}}) ->
    display(File, get_line(E), "incompatible types, ~p and ~p", [V1, V2]);
format_error({error, _, File, {bad_type_combination, E, V1, V2}}) ->
    display(File, get_line(E), "incompatible types, ~p and ~p", [V1, V2]);
format_error({error, _, File, {bad_oneway_type, X, TK}}) ->
    display(File, get_line(X), "oneway operations must be declared void", []);
format_error({error, _, File, {inout_spec_for_c, X, Arg}}) ->
    display(File, get_line(X), "inout parameter ~s specified in native c mode",
	    [Arg]);
format_error({error, _, File, {sequence_not_defined, X, Arg}}) ->
    display(File, get_line(X), "sequence ~s not defined", [Arg]);
format_error({error, _, File, {illegal_typecode_for_c, Arg}}) ->
    display(File, not_specified, "illegal typecode ~s used in native c mode",
	    [Arg]);
format_error({error, _, File, {name_not_found, N}}) ->
    display(File, not_specified, "name ~s not found", [N]);
format_error({error, _, File, {illegal_typecode_for_c, Arg, N}}) ->
    display(File, not_specified, "illegal typecode ~p used for ~p in native c mode", [Arg, N]);
format_error({error, _, File, {oneway_outparams, X}}) ->
    display(File, get_line(X), 
	    "oneway operations may not have out or inout parameters", []);
format_error({error, _, File, {oneway_raises, X}}) ->
    display(File, get_line(X), "oneway operations may not raise exceptions",
	    []);
format_error({error, _, File, {bad_tk_match, T, TK, V}}) ->
    display(File, get_line(T),
	    "value ~p does not match declared type ~s", [V, pp(TK)]);
format_error({error, _, File, {bad_scope_enum_case, ScopedId}}) ->
    display(File, get_line(ScopedId),
	    "scoped enum identifiers not allowed as case (~s)", 
	    [pp(ScopedId)]);
format_error({error, _, File, {bad_type, Expr, Op, TypeList, V}}) ->
    display(File, get_line(Expr),
	    "parameter value ~p to ~s is of illegal type", [V, pp(Op)]);
format_error({error, _, File, {bad_case_type, TK, X, Val}}) ->
    display(File, get_line(X), 
	    "case value ~s does not match discriminator type ~s", 
	    [case_pp(X, Val), pp(TK)]);
format_error({error, _, File, {tk_not_found, X}}) ->
    display(File, get_line(X), "undeclared identifier ~s", [pp(X)]);
%%% New format_errors
format_error({error, _, File, {illegal_switch_t, Arg, _N}}) ->
    display(File, get_line(Arg), "illegal switch", []);  
format_error({error, _, File, {inherit_resolve, Arg, N}}) ->
    display(File, get_line(Arg), "cannot resolve ~s", [N]);
format_error({error, _, File, {bad_escape_character, Line, Char}}) ->
    display(File, Line, "bad escape character \"~c\"", [Char]);  
format_error({error, _, File, {pragma_code_opt_bad_option_list, Line}}) ->
    display(File, Line, "bad option list on pragma \"CODEOPT\"", []);
format_error({error, _, File, {bad_string, Line}}) ->
    display(File, Line, "bad string", []).





%% Warnings
print_warn(G, Warn) ->
    case {get_opt(G, silent), get_opt(G, silent2)} of
	{true, _} -> ok;
	{_, true} -> ok;
	_ -> format_warn(Warn)
    end.

format_warn({warn, _, File, {ic_pp_warning, Lines}}) ->
    display(File, "preprocessor warning: ~s", [Lines]);
format_warn({warn, _, _File, {cfg_open, Reason, File}}) ->
    display(File, "warning: could not open file: ~p", [File]);
format_warn({warn, _, _File, {cfg_read, File}}) ->
    display(File, "warning: syntax error in configuration file", []);
format_warn({warn, _, File, {multi_modules, Id}}) ->
    display(File, get_line(Id), "warning: multiple modules in file", []);
format_warn({warn, _, File, {illegal_opt, Opt}}) ->
    display(File, "warning: unrecognised option: ~p", [Opt]);
format_warn({warn, _, File, {nested_mod, Id}}) ->
    display(File, get_line(Id), "warning: nested module: ~s", [get_id(Id)]);
format_warn({warn, _, File, {inherit_name_shadow, {Orig, Item}, 
			     {Base, NewItem}}}) ->
    display(File, get_line(Item), 
	    "warning: ~s shadows ~s", [pp([get_id2(Item) | Orig]),
				       pp([get_id2(NewItem) | Base])]);
format_warn({warn, _, File, {internal_307, X, Y}}) ->
    %% If global Scope variable is not [] at top level constant
    display(File, get_line(X), "warning: internal 307: ~p ~p", [X, Y]).

%% Display an error or warning
display(File, Line, F, A) ->
    io:format("~p on line ~p: ~s~n", [File, Line, io_lib:format(F, A)]).
display(File, F, A) ->
    io:format("~p: ~s~n", [File, io_lib:format(F, A)]).





%% pretty print various stuff

pp({tk_string, _}) -> "string";
pp(tk_long) -> "long";
pp(tk_short) -> "short";
pp(tk_ushort) -> "unsigned short";
pp(tk_ulong) -> "unsigned long";
pp(tk_float) -> "float";
pp(tk_double) -> "double";
pp(tk_boolean) -> "boolean";
pp(tk_char) -> "char";
pp(tk_octet) -> "octet";
pp(tk_null) -> "null";
pp(tk_void) -> "void";
pp(tk_any) -> "any";
pp({tk_objref, _, _}) -> "object reference";
pp(rshift) -> ">>";
pp(lshift) -> "<<";
pp(X) when element(1, X) == tk_enum -> "enum";
pp(X) when record(X, scoped_id) -> to_colon(X);
pp(X) when element(1, X) == '<identifier>' -> get_id(X);
pp(X) when list(X), list(hd(X)) -> to_colon(X);
pp({_, Num, Beef}) when integer(Num) -> Beef;
pp({Beef, Num}) when integer(Num) -> to_list(Beef);
pp(X) -> to_list(X).

%% special treatment of case label names
case_pp(X, Val) when record(X, scoped_id) -> pp(X);
case_pp(X, Val) -> pp(Val).


%%--------------------------------------------------------------------
%%
%% Generation go-get utilities
%%
%%	Feeble attempt at virtual funtions.
%%
%%--------------------------------------------------------------------

get_dimension(X) when record(X, array)       -> 
    [element(3, L) || L <- X#array.size].

%% Should find the name hidden in constructs
get_id( {'<identifier>', LineNo, Id} ) -> Id;
get_id(Id) when list(Id), integer(hd(Id)) -> Id;
get_id(X) when record(X, scoped_id) -> X#scoped_id.id;
get_id(X) when record(X, array) -> get_id(X#array.id);
get_id( {'<string_literal>', LineNo, Id} ) -> Id.

get_line({'<identifier>', LineNo, Id}) -> LineNo;
get_line(X) when record(X, scoped_id) -> X#scoped_id.line;
get_line(X) when record(X, module)      -> get_line(X#module.id);
get_line(X) when record(X, interface)   -> get_line(X#interface.id);
get_line(X) when record(X, forward)     -> get_line(X#forward.id);
get_line(X) when record(X, const)       -> get_line(X#const.id);
get_line(X) when record(X, typedef)     -> get_line(X#typedef.id);
get_line(X) when record(X, struct)      -> get_line(X#struct.id);
get_line(X) when record(X, member)      -> get_line(X#member.id);
get_line(X) when record(X, union)       -> get_line(X#union.id);
get_line(X) when record(X, case_dcl)    -> get_line(X#case_dcl.id);
get_line(X) when record(X, enum)	-> get_line(X#enum.id);
get_line(X) when record(X, enumerator)	-> get_line(X#enumerator.id);
get_line(X) when record(X, array)       -> get_line(X#array.id);
get_line(X) when record(X, attr)	-> get_line(X#attr.id);
get_line(X) when record(X, except)	-> get_line(X#except.id);
get_line(X) when record(X, op)		-> get_line(X#op.id);
get_line(X) when record(X, param)       -> get_line(X#param.id);
get_line(X) when record(X, id_of)       -> get_line(X#id_of.id);

get_line({'or', T1, T2}) ->	get_line(T1);
get_line({'xor', T1, T2}) ->	get_line(T1);
get_line({'and', T1, T2}) ->	get_line(T1);
get_line({'rshift', T1, T2}) ->	get_line(T1);
get_line({'lshift', T1, T2}) ->	get_line(T1);
get_line({'+', T1, T2}) ->	get_line(T1);
get_line({'-', T1, T2}) ->	get_line(T1);
get_line({'*', T1, T2}) ->	get_line(T1);
get_line({'/', T1, T2}) ->	get_line(T1);
get_line({'%', T1, T2}) ->	get_line(T1);
get_line({{'-', Line}, T}) ->	get_line(T);
get_line({{'+', Line}, T}) ->	get_line(T);
get_line({{'~', Line}, T}) ->	get_line(T);
get_line({_, X, _}) when integer(X) -> X;
get_line({A, N}) when integer(N)	-> N;
get_line(_)				-> -1.


%%--------------------------------------------------------------------
%%
%% High level get functions.
%%
%%	These are highly polymorphic functions that will get the id,
%%	body and type of a record (those records output from the
%%	parser).
%%
%% NOTE: The typedef node (the alias) is special, because the type
%%	field is a type definition and therefore considered a body,
%%	and the type of a typedef is its name.
%%

get_id2(X) when record(X, module)       -> get_id(X#module.id);
get_id2(X) when record(X, interface)    -> get_id(X#interface.id);
get_id2(X) when record(X, forward)      -> get_id(X#forward.id);
get_id2(X) when record(X, const)        -> get_id(X#const.id);
get_id2(X) when record(X, typedef)      -> get_id(hd(X#typedef.id));
get_id2(X) when record(X, struct)       -> get_id(X#struct.id);
get_id2(X) when record(X, member)       -> get_id(hd(X#member.id));
get_id2(X) when record(X, union)        -> get_id(X#union.id);
get_id2(X) when record(X, case_dcl)     -> get_id(X#case_dcl.id);
get_id2(X) when record(X, enum)		-> get_id(X#enum.id);
get_id2(X) when record(X, enumerator)	-> get_id(X#enumerator.id);
get_id2(X) when record(X, array)        -> get_id(X#array.id);
get_id2(X) when record(X, attr)		-> get_id(X#attr.id);
get_id2(X) when record(X, except)       -> get_id(X#except.id);
get_id2(X) when record(X, op)		-> get_id(X#op.id);
get_id2(X) when record(X, param)        -> get_id(X#param.id);
get_id2(X) when record(X, type_dcl)     -> get_id2(X#type_dcl.type);
get_id2(X) when record(X, scoped_id)	-> scoped_id_strip(X);
get_id2(X) when record(X, preproc)	-> get_id(X#preproc.id);
get_id2(X) when record(X, id_of)	-> get_id2(X#id_of.id);
get_id2(X) -> get_id(X).

get_body(X) when record(X, module)      -> X#module.body;
get_body(X) when record(X, interface)   -> X#interface.body;
get_body(X) when record(X, struct)      -> X#struct.body;
get_body(X) when record(X, union)       -> X#union.body;
get_body(X) when record(X, enum)        -> X#enum.body;
get_body(X) when record(X, typedef)     -> X#typedef.type; % See Note 
get_body(X) when record(X, except)      -> X#except.body.

get_type(X) when record(X, const)       -> X#const.type;
get_type(X) when record(X, type_dcl)    -> X#type_dcl.type;
get_type(X) when record(X, typedef)     -> X#typedef.id; % See Note 
get_type(X) when record(X, member)      -> X#member.type;
get_type(X) when record(X, union)       -> X#union.type;
get_type(X) when record(X, case_dcl)    -> X#case_dcl.type;
get_type(X) when record(X, sequence)    -> X#sequence.type;
get_type(X) when record(X, attr)        -> X#attr.type;
get_type(X) when record(X, op)		-> X#op.type;
get_type(X) when record(X, param)       -> X#param.type.
%%get_type(X) when record(X, id_of)       -> get_type(X#id_of.type).


%% Get idlist returns the list of identifiers found in typedefs, case
%% dcls etc.
get_idlist(X) when record(X, typedef)	-> X#typedef.id;
get_idlist(X) when record(X, member)	-> X#member.id;
get_idlist(X) when record(X, case_dcl)	-> X#case_dcl.label;
get_idlist(X) when record(X, attr)	-> X#attr.id.

%% mk_list produces a nice comma separated string of variable names
mk_list([]) -> [];
mk_list([Arg | Args]) ->
    Arg ++ mk_list2(Args).
mk_list2([Arg | Args]) ->
    ", " ++ Arg ++ mk_list2(Args);
mk_list2([]) -> [].


%% Shall convert a string to a Erlang variable name (Capitalise)
mk_var( [N | Str] ) when N >= $a, N =< $z ->
    [ N+$A-$a | Str ];
mk_var( [N | Str] )  when N >= $A, N =< $Z -> [N | Str].
    
%% Shall produce a "public" name for name. When we introduce new
%% identifiers in the mapping that must not collide with those from
%% the IDL spec.
%%
%% NOTE: Change name of IFR ID in system exceptions in corba.hrl when
%% prefix is changed here.
%%
mk_name(Gen, Name) -> lists:flatten(["OE_" | Name]).
mk_oe_name(Gen, Name) -> lists:flatten(["oe_" | Name]).

mk_align(String) ->
    lists:flatten(io_lib:format("((~s)+sizeof(double)-1)&~~(sizeof(double)-1)",[String])).

to_atom(A) when atom(A) -> A;
to_atom(L) when list(L) -> list_to_atom(L).

to_list(A) when list(A) -> A;
to_list(L) when atom(L) -> atom_to_list(L);
to_list(X) when integer(X) -> integer_to_list(X).

add_dot_erl(F) ->
    File = to_list(F),
    F2 = lists:reverse(File),
    case F2 of 
	[$l, $r, $e, $. | Rest] -> File;
	_ -> File ++ ".erl"
    end.

add_dot_hrl(F) ->
    File = to_list(F),
    F2 = lists:reverse(File),
    case F2 of 
	[$l, $r, $h, $. | Rest] -> File;
	_ -> File ++ ".hrl"
    end.

add_dot_c(F) ->
    File = to_list(F),
    F2 = lists:reverse(File),
    case F2 of 
	[$c, $. | Rest] -> File;
	_ -> File ++ ".c"
    end.

add_dot_h(F) ->
    File = to_list(F),
    F2 = lists:reverse(File),
    case F2 of 
	[$h, $. | Rest] -> File;
	_ -> File ++ ".h"
    end.

add_dot_idl(F) ->
    File = to_list(F),
    F2 = lists:reverse(File),
    case F2 of 
	[$l, $d, $i, $. | Rest] -> File;
	_ -> File ++ ".idl"
    end.




%%--------------------------------------------------------------------
%%
%% Symbol table routines
%%
%%	Symbol tables handles mappings Id -> Value, where Id is an
%%	ordinary Id from the parser (or a string) and value is an
%%	arbitrary term.
%%
%%--------------------------------------------------------------------

symtab_new() ->
    ets:new(symtab, [set, public]).

symtab_store(G, N, X) ->
    Name = [get_id2(X) | N],
    %%io:format("Adding id: ~p~n", [N]),
    case symtab_soft_retrieve(G, Name) of
	{error, _} ->
	    ets:insert(G#genobj.symtab, {Name, X});
	{ok, Y} when record(Y, forward) ->
	    ets:insert(G#genobj.symtab, {Name, X});
	{ok, Y} ->
	    error(G, {multiply_defined, X})
    end.


%% symtab_retrieve		--------------------------------------
%%
%%	Makes a lookup in the symbol table for Id. Will throw
%%	not_found if it fails.
symtab_retrieve(G, Id) ->
    case ets:lookup(G#genobj.symtab, Id) of
	[{_, Val}] -> Val;
	[] -> error(G, {symtab_not_found, Id})
    end.


%% symtab_soft_retrieve		--------------------------------------
%%
%%	Same as symtab_retrieve but will use tagged return values.
%%
symtab_soft_retrieve(G, Id) ->
    case ets:lookup(G#genobj.symtab, Id) of
	[{_, Val}] -> {ok, Val};
	[] -> {error, {symtab_not_found, Id}}
    end.


%% symtab_intf_resolv		--------------------------------------
%%   and symtab_intf_resolv2
%%
%%	Tries to resolv the interface identifier reference. The id can
%%	be either a scoped name or an standard identifier. The
%%	function returns a global reference to the id.
%%
%%	Will throw not_found if the id really cannot be found. Will
%%	throw illegal_forward if any forward references are founf in
%%	the inheritance list.
%%
symtab_intf_resolv(G, Scope, Id) ->
    case scoped_id_is_global(Id) of
	true ->
	    symtab_retrieve(G, Id),
	    Id;
	false ->
	    symtab_intf_resolv2(G, Scope, Id)
    end.

symtab_intf_resolv2(G, Scope, Id) ->
    N = scoped_id_add(Scope, Id),
    case symtab_soft_retrieve(G, scoped_id_strip(N)) of
	{ok, F} when record(F, forward) ->
	    error(G, {illegal_forward, Id}), [];
	{ok, Val} -> scoped_id_mk_global(N);
	_ ->
	    case scoped_id_is_top(Scope) of
		false ->
		    symtab_intf_resolv2(G, scoped_id_up_one(Scope), Id);
		true ->
		    error(G, {symtab_not_found, Id}), []
	    end
    end.



%%--------------------------------------------------------------------
%%
%% Scoped id routines
%%
%%	A scoped id is an id written as M::Id in IDL. Scoped ids are
%%	implemented as lists of id in reverse order, so M1::F1 becomes
%%	[F1, M1].
%%
%%--------------------------------------------------------------------

get_full_scoped_name(G, N, S)  when element(1, S) == scoped_id ->
    ictype:scoped_lookup(G, icgen:tktab(G), N, S).

%%scoped_id_new_global() ->
%%    X=scoped_id_new(), X#scoped_id{type=global}.

scoped_id_new_global(Id) ->
    X=scoped_id_new(Id), X#scoped_id{type=global}.

%%scoped_id_new() ->
%%    #scoped_id{}.

scoped_id_new(Id) ->
    #scoped_id{line=get_line(Id), id=[get_id(Id)]}.

%% Adds one more id to the list of ids
scoped_id_add(S1, S2) when record(S2, scoped_id) ->
    S1#scoped_id{id=S2#scoped_id.id ++  S1#scoped_id.id, 
		 line=S2#scoped_id.line};
scoped_id_add(S, Id) ->
    S#scoped_id{id=[get_id(Id) | S#scoped_id.id], line=get_line(Id)}.


scoped_id_mk_global(S) -> S#scoped_id{type=global}.

scoped_id_is_global(S) when record(S, scoped_id), S#scoped_id.type==global -> 
    true;
scoped_id_is_global(_) -> false.

%% Top level scope (i.e no more cd ..)
scoped_id_is_top(S) when S#scoped_id.id==[] -> true;
scoped_id_is_top(_) -> false.


scoped_id_up_one(S) -> S#scoped_id{id=tl(S#scoped_id.id)}. % cd .. in scope
%%scoped_id_get_def(S) -> hd(S#scoped_id.id).	% Last added id
scoped_id_strip(S) -> S#scoped_id.id.		% Strips all junk


%% Produce a colon (or under score) separated string repr of the name
%% X
%%
to_colon(X) when element(1, X) == scoped_id ->
    to_colon2(scoped_id_strip(X));
to_colon(L) -> to_colon2(L).

to_colon2([X]) -> X;
to_colon2([X | Xs]) -> to_colon2(Xs) ++ "::" ++ X;
to_colon2([]) -> "".

to_undersc(X) when element(1, X) == scoped_id ->
    to_undersc2(scoped_id_strip(X));
to_undersc(L) -> to_undersc2(L).

to_undersc2([X]) -> X;
to_undersc2([X | Xs]) -> to_undersc2(Xs) ++ "_" ++ X;
to_undersc2([]) -> "".

is_oneway(X) when record(X, op)  ->
    case  X#op.oneway of
	{oneway, _} -> true;
	_ -> false
    end;
is_oneway(X) -> false.




%%--------------------------------------------------------------------
%%
%% Storage routines
%%
%% The generator object G is used to store many usefull bits of
%% information so that the information doesn't need to get passed
%% around everywhere.
%%
%%--------------------------------------------------------------------


skelscope(G)	-> G#genobj.skelscope.
stubscope(G)	-> G#genobj.stubscope.
symtab(G)	-> G#genobj.symtab.
auxtab(G)	-> G#genobj.auxtab.
tktab(G)	-> G#genobj.tktab.
impl(G)		-> G#genobj.impl.
pragmatab(G)    -> G#genobj.pragmatab.
optiontab(G)    -> G#genobj.options.
typedeftab(G)    -> G#genobj.c_typedeftab.

idlfile(G)	-> ?lookup(G#genobj.options, idlfile).
module(G)	-> ?lookup(G#genobj.options, module).

set_idlfile(G, X)	-> ?insert(G#genobj.options, idlfile, X).
set_module(G, X)	-> ?insert(G#genobj.options, module, get_id(X)).


%%--------------------------------------------------------------------
%%
%% Option handling
%%
%% Valid options are: (those with * is NotYetImpl)
%%
%% pedantic - makes the compiler really nitty-gritty about its input
%%
%% Wall - those warning options that we feel an IDL programmer should
%% care about. Not as picky as pedantic
%%
%% warn_multi_mod - warn if several modules are declared in the same
%% IDL file
%%
%% warn_nested_mod - warn if there are nested modules. This is not a
%% problem but it breakes the rule that modules are put into one file
%% each.
%%
%% warn_name_shadow - warn if identifiers are shadow through inherited
%% interfaces. Default is true.
%%
%% warn_quoted_atom - warn if atoms needs quote, this makes Erlang
%% code less nice but is certainly no error.
%%
%% nowarn - suppress all warning messages. Will still output warnings
%% if silent2 option is used
%%
%% always_outargs - force object server implementation return the
%% tuple {RetVal, OutArgs, NewState} even if there are no OutArgs. If
%% this option is not set then such an operation implementation is
%% assumed to return {RetVal, NewState}
%%
%% use_proc_dict - use the process dictionary in the client
%% stubs. This means that client stubs return RetVal instead of {ok,
%% RetVal, OutArgs} and that corba:get_outargs() returns OutArgs. The
%% out arguments are stored with the key '$corba_outargs'.
%%
%% module_group - use the top module as file name for both skeletons
%% and stubs. Default value is false which means that each interface
%% is put in a separate file.
%%
%% skel_module_group - group all interfaces in a module in one
%% skeleton file as opposed to one skeleton file for each
%% interface. Defaults to false.
%%
%% stub_module_group - group all interface stubs from a module in one
%% stub file as opposed to one stub file for each interface. Default
%% is false.
%%
%% *help - prints a small summary of the compiler usage
%%
%% silent - suppresses all messages from the compiler
%%
%% silent2 - suppresses all messages from the compiler and returns all
%% warnings or errors as lists. Returns {ok, WarnList} or {error,
%% WarnList, ErrList}
%%
%% *noexec - runs the compiler but does not open files or write to
%% files.
%%
%% {serv, <ModName>} - sets the name of the implementation skeleton
%% file. This defaults to ModName_skel.
%%
%% {impl, <ModName>} - sets the name of the interface server
%% implementation module name. This defaults to InterfaceName_impl
%%
%% {outdir, Dir} - use Dir as the directory to put all generated
%% files.
%%
%% {servdir, Dir} - put all generated skel files in the directory Dir.
%%
%% {stubdir, Dir} - put all generated stub files in the directory Dir.
%%
%% {this, InterfaceOrOpName} - puts the OE_THIS parameter into the
%% impl. call. This option can be used both on whole interfaces an on
%% distinct operations. Fullscoped names must be used (as in {this,
%% "M1::I1::Op"}). The option can be given in 3 ways: {this, Name}
%% means this will be added to all matching Name or as {{this, Name},
%% true} or this can explicitly be asked to be left out as in {{this,
%% Name}, false} which enables OE_THIS to be passed to all ops of an
%% interface except those set by the false flag.
%%
%% cfgfile - sets the name of the config file that is read at
%% startup. The order of the different ways to set options is: default
%% setting, configuration file, options given when generator is
%% called. Default name for this file is .ic_config
%%
%% serv_last_call - tells what the last handle_call clause should
%% do. It can have the values exception, which makes the last clause
%% return a CORBA exception and exit which does not generate a last clause
%% (which will make the server crash on an unknown call)
%%
%%
%% -- UNDOCUMENTED --
%%
%% debug - prints debug information
%%
%% tokens - prints the tokens from the tokenizer and then exit
%%
%% form - prints the form from the parser and then exit
%%
%% tform - form returned from type check
%%
%% time - if true then time is measured during compilation
%%
%% use_leex - tries to use a leex scanner. Can have the values 1, 2,
%% or boolean() for different styles of leex scanners
%%
%% 
%%--------------------------------------------------------------------
allowed_opt(default_opts, V)		-> true;
allowed_opt(debug, V)			-> is_bool(V);
allowed_opt(tokens, V)			-> is_bool(V);
allowed_opt(form, V)			-> is_bool(V);
allowed_opt(tform, V)			-> is_bool(V);
allowed_opt(time, V)			-> is_bool(V);
allowed_opt(use_leex, V)		-> is_intorbool(V);
allowed_opt(maxerrs, V)			-> is_intorinfinity(V);
allowed_opt(maxwarns, V)		-> is_intorinfinity(V);
allowed_opt(nowarn, V)			-> is_bool(V);
allowed_opt(show_opts, V)		-> is_bool(V);

allowed_opt(help, V)			-> is_bool(V);
allowed_opt('Wall', V)			-> is_bool(V);
allowed_opt(warn_multi_mod, V)		-> is_bool(V);
allowed_opt(warn_quoted_atom, V)	-> is_bool(V);
allowed_opt(warn_nested_mod, V)		-> is_bool(V);
allowed_opt(warn_name_shadow, V)	-> is_bool(V);
allowed_opt(module_group, V)		-> is_bool(V);
allowed_opt(skel_module_group, V)	-> is_bool(V);
allowed_opt(stub_module_group, V)	-> is_bool(V);
allowed_opt(always_outargs, V)		-> is_bool(V);
allowed_opt(pedantic, V)		-> is_bool(V);
%%allowed_opt(gen_serv, V)		-> is_bool(V);
%%allowed_opt(gen_stub, V)		-> is_bool(V);
allowed_opt(gen_hrl, V)			-> is_bool(V);
allowed_opt(serv_last_call, exception)	-> true;
allowed_opt(serv_last_call, exit)	-> true;
allowed_opt(silent, V)			-> is_bool(V);
allowed_opt(silent2, V)			-> is_bool(V);
allowed_opt({serv, _}, V)		-> true;
allowed_opt({impl, _}, V)		-> true;
allowed_opt(outdir, V)			-> true;
allowed_opt(servdir, V)			-> true;
allowed_opt(stubdir, V)			-> true;
allowed_opt(cfgfile, V)			-> true;
allowed_opt(use_preproc, V)		-> is_bool(V);
allowed_opt(preproc_cmd, V)		-> true;
allowed_opt(preproc_flags, V)		-> true;
allowed_opt(this, V)			-> true;
allowed_opt({this, _}, V)		-> is_bool(V);
allowed_opt(handle_info, V)		-> true;
allowed_opt({handle_info, _}, V)	-> is_bool(V);
allowed_opt(timeout, V)		        -> true;
allowed_opt({timeout, _}, V)     	-> is_bool(V);
allowed_opt(scoped_op_calls, V)         -> is_bool(V);
% Compatibility option (semantic check limitation)
allowed_opt(scl, V)                     -> is_bool(V);
% Added switches for non corba generation
allowed_opt(be, erl_corba)	        -> true;
allowed_opt(be, erl_genserv)	        -> true;
allowed_opt(be, c_genserv)		-> true;
allowed_opt(be, erl_plain)		-> true; 
allowed_opt(be, c_server)		-> true;
allowed_opt(be, c_client)		-> true;
allowed_opt(be, java)    		-> true;
% Noc backend
allowed_opt(be, noc)  	                -> true; 
allowed_opt({broker,_},{_,transparent}) -> true; 
allowed_opt({broker,_},{_,Term})  	-> is_term(Term);
allowed_opt({use_tk,_},V)               -> is_bool(V);
%
allowed_opt(precond, {M, F})            -> true;
allowed_opt({precond, _}, {M, F})       -> true;
allowed_opt(postcond, {M, F})           -> true;
allowed_opt({postcond, _}, {M, F})      -> true;
allowed_opt(no_codechange, V)           -> is_bool(V);
allowed_opt(_, _)			-> false.

is_bool(true) -> true;
is_bool(false) -> true;
is_bool(_) -> false.

is_intorbool(X) when integer(X) -> true;
is_intorbool(X) -> is_bool(X).

is_intorinfinity(X) when integer(X) -> true;
is_intorinfinity(infinity) -> true;
is_intorinfinity(X) -> false.


is_term(Term) when tuple(Term) -> true;
is_term(_NoTerm) -> false.
    

-define(DEFAULTCFGFILE, ".ic_config").

which_opts(G) ->
    ets:match(G#genobj.options, {{option, '$1'}, '$2'}).

add_opt(G, KList, Val) when list(KList) ->
    lists:foreach(fun({K, V}) -> add_opt(G, K, V);
		  (K) -> add_opt(G, K, Val) end,
		  KList);

add_opt(G, servdir, V) ->
    do_add_opt(G, servdir, assure_directory(G, to_list(V)));
add_opt(G, stubdir, V) ->
    do_add_opt(G, stubdir, assure_directory(G, to_list(V)));
add_opt(G, K, V) ->
    do_add_opt(G, K, V).


assure_directory(G, Dir) ->
    Dirs = filename:split(Dir),
    check_dirs(Dirs, [], filename:pathtype(Dir)).
    
check_dirs([X | Xs], SoFar, Type) ->
    New = if  SoFar == [], Type /= absolute ->
		  X;
	      true ->
		  filename:join(SoFar, X)
	  end,
    assert_dir(New),
    check_dirs(Xs, New, Type);
check_dirs([], SoFar, Type) ->
    SoFar.

assert_dir(D) ->
    case file:read_file_info(D) of
	{ok, X} when X#file_info.type == directory -> ok;
	_ -> case file:make_dir(D) of
		 ok -> ok;
		 _ -> exit({could_not_create, D})
	     end
    end.

%%assure_directory(G, Dir) ->
%%    Dir2 = parse_dir([], Dir),
%%%%    io:format("Trying dir: ~p to ~p~n", [Dir, Dir2]),
%%    Dir2.

%%parse_dir([],[]) -> [];
%%parse_dir(SoFar, [$/ | Rest]) ->
%%    New = SoFar ++ [$/],
%%    assert_dir(New),
%%    parse_dir(New, Rest);
%%parse_dir(SoFar, [C|Rest]) ->
%%    parse_dir(SoFar++[C], Rest);
%%parse_dir(SoFar, []) -> 
%%    assert_dir(SoFar),
%%    fix(SoFar).

%%assert_dir(D) ->
%%    case file:file_info(D) of
%%	{ok, {_, directory, _, _, _, _, _}} -> ok;
%%	_ -> case file:make_dir(D) of
%%		 ok -> ok;
%%		 _ -> exit({could_not_create, D})
%%	     end
%%    end.

%%fix(D) ->
%%    F = case lists:reverse(D) of
%%	[$/ | R] -> D;
%%	_ -> D ++ "/"
%%    end,
%%    F.
    

do_add_opt(G, handle_info, V) when V /= true, V /= false ->
    ?insert(G#genobj.options, {option, {handle_info, V}}, true);
do_add_opt(G, timeout, V) when V /= true, V /= false ->
    ?insert(G#genobj.options, {option, {timeout, V}}, true);
do_add_opt(G, this, V) ->
    ?insert(G#genobj.options, {option, {this, V}}, true);
do_add_opt(G, {this, V}, false) ->
    ?insert(G#genobj.options, {option, {this, V}}, force_false);
do_add_opt(G, scoped_op_calls, V) when V /= true, V /= false ->
    ?insert(G#genobj.options, {option, {scoped_op_calls, V}}, false);
do_add_opt(G, K, V) ->
    case allowed_opt(K, V) of
	true ->
	    case expand_opt(K) of
		L when list(L) ->
		    add_opt(G, L, V);
		_ ->
		    %%io:format("Add opt: ~p ~p~n", [K, V]),
		    ?insert(G#genobj.options, {option, K}, V)
	    end;
	_ ->
	    warn(G, {illegal_opt, K})
    end.

get_opt(G, K) ->
    case ets:lookup(G#genobj.options, {option, K}) of
	[] -> false;
	[{{_, K}, V}] -> V
    end.



expand_opt(pedantic) -> [warn_multi_mod, warn_quoted_atom, always_outargs];
expand_opt(module_group) -> [skel_module_group, stub_module_group];
expand_opt('Wall') -> [warn_multi_mod, warn_nested_mod, warn_name_shadow];
expand_opt(outdir) -> [servdir, stubdir];
expand_opt(default_opts) -> 
    ['Wall', gen_hrl, {serv_last_call, exception},
     {outdir, []}, use_preproc, {preproc_cmd, "erl"}, 
     {preproc_flags, ""}, {maxerrs, 10}, {maxwarns, infinity}];
%% gcc preproc command {preproc_cmd, "gcc -x c++ -E"}
expand_opt(Opt) -> Opt.


%% Use this if user not provide 
%% a backend.
defaultBe() -> erl_corba.
    


%%
%% Read any config file
read_cfg(G, Opts) ->
    Name = case lists:keysearch(cfgfile, 1, Opts) of
	       {value, {_, N}} -> to_list(N);
	       _ -> ?DEFAULTCFGFILE
	   end,
    case file:consult(Name) of
	{ok, OptList} ->
	    add_opt(G, OptList, true);
	X when Name == ?DEFAULTCFGFILE -> ok;
%%	{error, X} ->
%%	    warn(G, {cfg_open, X, Name});
	X -> warn(G, {cfg_open, X, Name})
    end.


float_to_version({_,_,Str}) -> Str.


%%-------------------------------------------------------------------------------------
%%
%% Trackrecording of generated sequence type structs, thist is just used for C today.
%%
%%-------------------------------------------------------------------------------------


get_basetype(G, MyId) ->
    case ?lookup(typedeftab(G), MyId) of
	[] ->
	     MyId;
	X ->
	    get_basetype(G, X)
    end.

insert_typedef(G, "erlang_term", _) ->
    ok;
insert_typedef(G, MyId, DefinedAsId) ->
    ?insert(typedeftab(G), MyId, DefinedAsId).




codeDirective(G,X) ->
    case produceCode(X) of
        true ->
            case get_opt(G, be) of
                c_genserv ->
                    c;
		c_client ->
		    c;
		c_server ->
		    c_server;
                _ ->
                    erlang
            end;
        false ->
            case get_opt(G, be) of              
                c_genserv ->
                    c_no_stub;
		c_client ->
		    c_no_stub;
		c_server ->
		    c_server_no_stub;
                _ ->
                    erlang_no_stub
            end
    end.

%% Checks if X should produce code
produceCode(X) when record(X, module) ->
    case get_body(X) of
        [] ->
            true;
        List ->
            produceModuleCode(List)
    end;
produceCode(X) ->    
    false.
    
produceModuleCode([]) ->
    false;
produceModuleCode([X|Xs]) when record(X, const) ->
    true;
produceModuleCode([X|Xs]) ->
    produceModuleCode(Xs).
 


%% Includes needed c file headers for included idl files
gen_includes(Fd,G,Type) ->
    case Type of
	c_client ->
	    IncludeList = 
		ic_pragma:get_included_c_headers(G),
	    gen_includes_loop(Fd,IncludeList,Type);
	c_server ->
	    IncludeList = 
		ic_pragma:get_included_c_headers(G),
	    gen_includes_loop(Fd,IncludeList,Type);
	_ ->
	    ok
    end,
    nl(Fd),
    emit(Fd, "#ifdef __cplusplus\n"),
    emit(Fd, "extern \"C\" {\n"),
    emit(Fd, "#endif\n\n").


%% Includes needed c file headers for local interfaces
gen_includes(Fd,G,X,Type) ->
    case Type of
	c_client ->
	    IncludeList = 
		ic_pragma:get_local_c_headers(G,X),
	    gen_includes_loop(Fd,IncludeList,Type);
	c_server ->
	    IncludeList = 
		ic_pragma:get_local_c_headers(G,X),
	    gen_includes_loop(Fd,IncludeList,Type);
	_ ->
	    ok
    end,
    nl(Fd),
    emit(Fd, "#ifdef __cplusplus\n"),
    emit(Fd, "extern \"C\" {\n"),
    emit(Fd, "#endif\n\n").


gen_includes_loop(_,[],_) ->
    ok;
gen_includes_loop(Fd,[I|Is],Type) ->
    L = string:tokens(I,"/"),
    File = lists:last(L),
    case File of
	"erlang" -> % Erlang is NOT generated that way !
	    gen_includes_loop(Fd,Is,Type);
	"oe_erlang" -> % Erlang is NOT generated that way !
	    gen_includes_loop(Fd,Is,Type);
	_ ->
	    case Type of
		c_client ->
		    emit(Fd, "#include \"~s.h\"\n", [File]);
		c_server ->
		    emit(Fd, "#include \"~s__s.h\"\n", [File]);
		_ ->
		    ok
	    end,
	    gen_includes_loop(Fd,Is,Type)
    end.




