%% Copyright (c) 1996 by Fredrik Ström o Peter Molin.  All Rights Reserved 
%% Time-stamp: <>
%% ====================================================================
%%  Filename : 	gsb_run.erl
%%  Module   :	gsb_run
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 1996-07-31 Peter Molin EX (peterm@csd.uu.se): Created.
%% ====================================================================
%% Exported functions (short description):
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(gsb_run).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/3 $').
-revision('$Revision: 1.11 $').
-export([start/2, start_sw/3,transfer/1, rc2erl/1, swr2erl/1,
	 get_value/2]).

%% ____________________________________________________________________
%%
%%  start(Filename)    
%%  Returns : a gs id.
%%  Args    :
%%  Comment : Start gs, create all widgets in Filename, 
%% ____________________________________________________________________

start(GS, Module) ->
  case locate_resource(Module, rc) of
    {jam_file, UiModuleName} ->
      apply({UiModuleName, start}, [GS]);
    {rc_file, AbsFileName} ->
      List = read_list(AbsFileName),
      create_widgets(GS, List, [])
  end.

start_sw(Gs, Module, Opts) ->
  {GsOpts, UserOpts} = split_opts(Opts, [],[]),
  case locate_resource(Module, swr) of
    {jam_file, UiModuleName} ->
      RemainingOpts = apply({UiModuleName,start},[Gs, GsOpts, UserOpts]),
      {ok, RemainingOpts};
    {rc_file, AbsPath} ->
      List = read_list(AbsPath),
      create_soft_widgets(Gs, List,GsOpts),
      {ok, UserOpts};
    {error, Reason} ->
      {error, Reason}
  end.

locate_resource(Module, Type) ->
  ListModule = atom_to_list(Module),
  ListUiModule = lists:append([ListModule, "_ui"]),
  UiModule = list_to_atom(ListUiModule),
  {ok,CurrPath} = file:get_cwd(),
  ExpectedFileName =
    case Type of
      swr ->
	lists:append([CurrPath,"/",ListModule,".swr"]);
      rc ->
	lists:append([CurrPath,"/",ListModule,".rc"])
    end,
  %% First look in current directory for resource file
  case file:open(ExpectedFileName, read) of
    {ok, Fd} ->
      file:close(Fd),
      {rc_file, ExpectedFileName};
    {error, Reason} ->
      %% Then look for the module
      case code:which(UiModule) of
	non_existing ->
	  {error, Reason};
	Atom ->
	  {jam_file, UiModule}
      end
  end.

get_value(Type, Opts) ->
  case lists:keysearch(Type, 1, Opts) of
    false ->
      io:format("gsb_run error, missing option ~p~n", [Type]),
      exit(error);
    {value, {Type, Value}} ->
      Value
  end.

transfer(Filename) ->
  List = read_list(Filename),
  write_new_format(Filename, List).

rc2erl(Module) ->
  {ok, Path} = file:get_cwd(),
  ListModule = atom_to_list(Module),
  UiModuleName = list_to_atom(lists:append([ListModule, "_ui"])),
  FileName = lists:append([Path, "/", ListModule, ".rc"]), 
  List = read_list(FileName),
  write_erl_code(List, UiModuleName).

swr2erl(Module) ->
  {ok, Path} = file:get_cwd(),
  ListModule = atom_to_list(Module),
  UiModuleName = list_to_atom(lists:append([ListModule, "_ui"])),
  FileName = lists:append([Path, "/", ListModule, ".swr"]),
  List = read_list(FileName),
  write_sw_erl_code(List, UiModuleName).

split_opts([{{Name, Type}, Value}|Rest], GsOpts, UserOpts) ->
  split_opts(Rest, [{{Name, Type}, Value}|GsOpts], UserOpts);
split_opts([Opt|Rest], GsOpts, UserOpts) ->
  split_opts(Rest, GsOpts, [Opt|UserOpts]);
split_opts([], GsOpts, UserOpts) ->
  {GsOpts, UserOpts}.

%% ____________________________________________________________________
%%
%%  create_widgets(GS, List)    
%%  Returns : GS identifier
%%  Args    :
%% ____________________________________________________________________

create_widgets(Parent,[{N,C,GsType,none,[],P,E,Children}|Rest], Acc) ->
  Options = lists:append([P, E]),
  gs:create(GsType, N, Parent, Options),
  NewAcc = create_widgets(N,Children,[]),
  create_widgets(Parent, Rest, lists:append([NewAcc, Acc]));
create_widgets(Parent,[{N,_,GsType,Module,Exports,_,_,Children}|Rest],Acc)->
  ParentObjId = gs:read(Parent, id),
  Pid = apply({Module, start},[ParentObjId,Exports]),
  create_widgets(Parent, Rest, [{Parent, Module, Pid}|Acc]);
create_widgets(Parent, [], Acc) -> Acc.


create_soft_widgets(Parent,[{N,_,GsType,T,Exp,P,E,Children}|Rest],GsOpts)->
  {NewData, RestGsOpts} = get_relevant(GsOpts,N, [], []),
  NewProps=replace_all(P, NewData),
  Options=lists:append([NewProps, E]),
  gs:create(GsType, N, Parent, Options),
  NewGsOpts = create_soft_widgets(N, Children, RestGsOpts),
  create_soft_widgets(Parent, Rest, NewGsOpts);
create_soft_widgets(Parent, [], GsOpts) -> GsOpts.


%% ____________________________________________________________________
%%
%%  get_data(Type, Resource)    
%%  Args    :
%%  Returns :
%%  Comment :
%% ____________________________________________________________________

get_data(Type, [{Type, Data}|_]) -> Data;
get_data(Type, [{Other, _}|Rest]) -> get_data(Type, Rest);
get_data(_, []) -> [].

%% ____________________________________________________________________
%%
%%  get_relevant(Data, Name, SetAcc, RestAcc)    
%%  Args    :
%%  Returns :
%%  Comment :
%% ____________________________________________________________________

get_relevant([{{Name, Type}, Value}|Rest], Name, SetAcc, RestAcc) ->
  get_relevant(Rest, Name, [{Type, Value}|SetAcc], RestAcc);
get_relevant([{{Other, Type}, Value}|Rest], Name, SetAcc, RestAcc) ->
  get_relevant(Rest, Name, SetAcc, [{Other, Type, Value}|RestAcc]);
get_relevant([], _Name, SetAcc, RestAcc) -> {SetAcc, RestAcc};
get_relevant([Other|Rest], Name, SetAcc, RestAcc) ->
  get_relevant(Rest, Name, SetAcc, RestAcc).

%% ____________________________________________________________________
%%
%%  split_runtime(Data)    
%%  Args    :
%%  Returns :
%%  Comment :
%% ____________________________________________________________________

split_runtime([{{Key, Type}, Value}|Rest], GsAcc, ModuleAcc) ->
  split_runtime(Rest, [{{Key, Type}, Value}|GsAcc], ModuleAcc);
split_runtime([Other|Rest], GsAcc, ModuleAcc) ->
  split_runtime(Rest, GsAcc, [Other|ModuleAcc]);
split_runtime([], GsAcc, ModuleAcc) -> {GsAcc, ModuleAcc}.

%% ____________________________________________________________________
%%
%%  replace_all(Data, List)    
%%  Args    :
%%  Returns :
%%  Comment :
%% ____________________________________________________________________

replace_all(Data, [First|Rest]) ->
  NewList = replace_type(Data, [], First),	       
  replace_all(NewList, Rest);
replace_all(List, []) -> List.

replace_type([{EntryType, _Value}|Rest], Saved, {EntryType, Value}) ->
  lists:append([{EntryType, Value}|Saved], Rest);
replace_type([{O, V}|Rest],Saved, {EntryType, Value}) ->
  replace_type(Rest, [{O,V}|Saved], {EntryType, Value}).

%% ____________________________________________________________________
%%
%%  write_erl_code(List, FileName).    
%%  Args    :
%%  Returns :
%%  Comment :
%% ____________________________________________________________________

write_erl_code(List, UiModuleName) ->
  WriteFileName=lists:append([atom_to_list(UiModuleName), ".erl"]),
  case file:open(WriteFileName, write) of
    {ok, Fd} ->
      write_header(Fd, UiModuleName),
      {Counter, Returns} = write_top_widget(Fd, List),
      io:format(Fd,",~n    [",[]),
      write_returns(Fd,Returns),
      io:format(Fd,"]. ~n",[]),
      file:close(Fd);
    {error, Reason} ->
      io:format("gsb_run:write_erl_code -> Cannot write file ~p:~p ~n",
		[WriteFileName, Reason])
  end.

write_sw_erl_code(List, UiModuleName) ->
  WriteFileName=lists:append([atom_to_list(UiModuleName), ".erl"]),
  case file:open(WriteFileName, write) of
    {ok, Fd} ->
      write_sw_header(Fd, UiModuleName),
      {Counter, Returns} = write_top_widget(Fd, List),
      write_opts(Fd),
      io:format(Fd,",~n    [", []),
      write_returns(Fd,Returns),
      io:format(Fd,"]. ~n",[]),
      write_config_opts(Fd),
      file:close(Fd);
    {error, Reason} ->
      io:format("gsb_run:write_erl_code -> Cannot write file ~p:~p ~n",
		[WriteFileName, Reason])
  end.

%% ____________________________________________________________________
%%
%%  write_header(FileName)    
%%  Args    :
%%  Returns :
%%  Comment :
%% ____________________________________________________________________

write_header(Fd, UiModuleName)->
  io:format(Fd,"%% This file is generated by gsb_run:rc2erl/1~n",[]),
  io:format(Fd,"-module(~p). ~n", [UiModuleName]),
  io:format(Fd,"-export([start/1]). ~n~n", []),
  io:format(Fd,"start(GsParent) -> ~n",[]).

write_sw_header(Fd, UiModuleName)->
  io:format(Fd,"%% This file is generated by gsb_run:swr2erl/1~n",[]),
  io:format(Fd,"-module(~p). ~n", [UiModuleName]),
  io:format(Fd,"-export([start/2]). ~n~n", []),
  io:format(Fd,"start(GsParent, GsOpts) -> ~n",[]).

%% ____________________________________________________________________
%%
%%  create_top_widget(Fd, List, Acc)    
%%  Args    :
%%  Returns :
%%  Comment : Assumption. A resource file always has a window in top!
%% ____________________________________________________________________

write_top_widget(Fd,[{Name,Class,GsType,Module,Exports,Props,Events,
		       Children}]) ->
  Options = lists:append([Props, Events]),
  SortOptions = lists:keysort(1,Options),
  io:format(Fd,"    gs:create(~w, ~w, GsParent, ~w)",
	    [GsType,Name,SortOptions]),
  write_widgets(Fd, Name, Children, [], 1).

write_widgets(Fd,Parent,
	       [{Name,Class,GsType,none,[],Props,Events,Children}|Rest],
	      Acc, Counter)->
  Options = lists:append([Props, Events]),
  SortOptions = lists:keysort(1,Options),
  io:format(Fd, ",~n    gs:create(~w, ~w, ~w, ~w)",
	    [GsType, Name, Parent, SortOptions]),
  {NewCounter, NewAcc} = write_widgets(Fd, Name, Children, [], Counter),
  write_widgets(Fd,Parent, Rest, lists:append([Acc, NewAcc]), NewCounter);
write_widgets(Fd,Parent,[{Name,_,GsType,Module,Exports,_,_,Children}|Rest],
	      Acc, Counter)->
  {NewCounter, NewVar}  = new_var(Counter),
  io:format(Fd,",~n   ~s= apply({~w,start},[gs:read(~w,id),~w])",
	    [NewVar,Module,Parent, Exports]),
  write_widgets(Fd,Parent, Rest, [{Parent, Module, NewVar}|Acc], NewCounter);
write_widgets(Fd,Parent,[], Acc, Counter) -> {Counter, Acc}.

new_var(Counter) ->
  {Counter+1,lists:append(["Pid",integer_to_list(Counter)])}.

%% ____________________________________________________________________
%%
%%  write_returns(Fd, List)    
%%  Args    :
%%  Returns :
%%  Comment :
%% ____________________________________________________________________

write_returns(Fd, [{Parent, Module, Pid}]) ->
  io:format(Fd, "{~p,~p,~s}",[Parent, Module, Pid]);
write_returns(Fd, [{Parent, Module, Pid}|Rest]) ->
  io:format(Fd, "{~p,~p,~s},",[Parent, Module, Pid]);
write_returns(Fd,[]) -> ok.

%% ____________________________________________________________________
%%
%%  write_opts(Fd)    
%%  Args    :
%%  Returns :
%%  Comment :
%% ____________________________________________________________________

write_opts(Fd) ->
  io:format(Fd,",~n    config_opts(GsOpts)",[]).

write_config_opts(Fd) ->
  io:format(Fd,"~nconfig_opts([{{Name, Option}, Value}|Rest]) ->~n",[]),
  io:format(Fd,"    gs:config(Name, {Option, Value}),~n",[]),
  io:format(Fd,"    config_opts(Rest);~n", []),
  io:format(Fd,"config_opts([]) -> true.~n", []).

%% ____________________________________________________________________
%%
%%  read_list(FileName)    
%%  Returns :
%%  Args    :
%% ____________________________________________________________________

read_list(FileName) ->
  case file:open(FileName, read) of
    {ok, Fd} ->
      file:close(Fd);
    {error, Reason} ->
      io:format("gsb_run:read_list -> Error opening file ~p:~p~n",
		[FileName,Reason]),
      exit(Reason)
  end,
  case file:consult(FileName) of
    {ok, TermList} -> TermList;
    {error, Reason2} -> 
      io:format("gsb_run:read_list -> Cannot open file ~p:~p~n",
		[FileName, Reason2]),
      exit(Reason2)
  end.

write_new_format(Filename, List) ->
  case file:open(Filename, write) of
    {ok, Fd} ->
      write_new_format_list(List, Fd),
      file:close(Fd),
      true;
    {error, Reason} ->
      io:format("gsb_run:write_new_format -> Cannot write file ~s ~n",
		     [Filename])
  end.

write_new_format_list([{Type, Name, Props, Events, Children}|Rest], Fd) ->
  io:format(Fd, "{~w,standard,~w,none,[],~w,~w,[",
	    [Name, Type,Props, Events]),
  write_new_format_list_c(Children, Fd),
  io:format(Fd, "]}. ~n", []),
  write_new_format_list(Rest, Fd);
write_new_format_list([], _Fd) -> true.

write_new_format_list_c([{Type, Name, Props, Events, Children}],Fd)->
  io:format(Fd, "{~w,standard,~w,none,[],~w,~w,[",
	    [Name, Type,Props, Events]),
  write_new_format_list_c(Children, Fd),
  io:format(Fd, "]}", []);

write_new_format_list_c([{Type, Name, Props, Events, Children}|Rest],Fd)->
  io:format(Fd, "{~w,standard,~p,none,[],~w,~w,[",
	    [Name, Type,Props, Events]),
  write_new_format_list_c(Children, Fd),
  io:format(Fd, "]},", []),
  write_new_format_list_c(Rest, Fd);
write_new_format_list_c([], Fd) ->
  true.

