%% Copyright (c) 1996 by Fredrik Ström o Peter Molin.  All Rights Reserved 
%% Time-stamp: <>
%% ====================================================================
%%  Filename : 	gsb_db.erl
%%  Module   :	gsb_db
%%  Purpose  :  Module containing databases for GSB
%%  Notes    : 
%%  History  :	* 1996-07-10 Peter Molin EX (peterm@csd.uu.se): Created.
%% ====================================================================
%% Exported functions (short description):
%%
%% stop(Pid) ->
%% read(Pid, FileName) ->
%% write(Pid, Keys, FileName) ->
%% insert(Pid, Data) ->
%% lookup(Pid, Key) ->
%% lookup(Pid, Key, EntryType) ->
%% config(Pid, Param) ->
%% delete(Pid, Key) ->
%% list(Pid, Type) ->
%%
%% Need to be implemented by Module:
%%
%% list(Type) Type = read|
%% read(List, U, {Module, Record, DB})
%% set_data(EntryType, CurrData, Data)
%% get_data(EntryType, CurrData)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(gen_db).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/2 $').
-revision('$Revision: 1.12 $').
-include("gsb.hrl").
-export([start/3, stop/1, read/3, write/3, insert/2, config/4,
	 delete/2, list/2, lookup/2, lookup/3, read_list/1, write_list/3]).
-export([handle_call/3, terminate/2, init/1]).
%% -import([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% Exported functions                                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

start(Module, Parent, Record) ->
  case gen_server:start_link(
	 gen_db, {Module, Parent, Record}, []) of
    {ok, Pid} -> Pid;
    {error, Reason} -> messages:error("~p", [Reason], start, gen_db)
  end.

init({Module, Parent, Record}) ->
  DB = ets:new(Record, [public, set,{keypos, 2}]),
  {ok, {Module, Parent, DB}}.

stop(Pid) ->
  gen_server:call(Pid, stop, infinity).
read(Pid, Gsb, FileName) ->
  gen_server:call(Pid, {read, Gsb, FileName}, infinity).
write(Pid, Keys, FileName) ->
  gen_server:call(Pid, {write, Keys, FileName}, infinity).
write_list(Pid, List, FileName) ->
  gen_server:call(Pid, {write_list, List, FileName}, infinity).
insert(Pid, Data) ->
  gen_server:call(Pid, {insert, Data}, infinity).
lookup(Pid, Key) ->
  gen_server:call(Pid, {lookup, Key}, infinity).
lookup(Pid, Key, EntryType) ->
  gen_server:call(Pid, {lookup, Key, EntryType}, infinity).
config(Pid, Key, Type, Data) ->
  gen_server:call(Pid, {config, Key, Type, Data}, infinity).
delete(Pid, Key) ->
  gen_server:call(Pid, {delete, Key}, infinity).
list(Pid, Type) ->
  gen_server:call(Pid, {list, Type}, infinity).

%% ____________________________________________________________________
%%
%%  handle_call(Request, {From, Tag}, {Module, Parent, DB})    
%%  Returns : {reply, Reply, {Module, Parent, DB}}
%%  Args    :
%% ____________________________________________________________________

handle_call(stop, _, State) ->
  {stop, normal, true, State};
handle_call({read, Gsb, FileName}, U, {Module, Parent, DB}) ->
  List = read_list(FileName),
  Reply = apply({Module, read}, [List, Gsb, U, {Module, Parent, DB}]),
  {reply, Reply, {Module, Parent, DB}};
handle_call({write, Keys, FileName}, U, {Module, Parent, DB}) ->
  List = apply({Module, lookup_write_data}, [Keys, U, {Module, Parent, DB}]),
  case file:open(FileName, write) of
    {ok, Fd} ->
      write_list_int(List, Fd),
      file:close(Fd),
      {reply, true, {Module, Parent, DB}};
    {error, Reason} ->
      messages:error("Cannot write file ~s ~n",
		     [FileName], 
		     gen_test, 
		     write), 
      {stop, error, {Module, Parent, DB}}
  end;
handle_call({write_list, List, FileName}, U, {Module, Parent, DB}) ->
  case file:open(FileName, write) of
    {ok, Fd} ->
      write_list_int(List, Fd),
      file:close(Fd),
      {reply, true, {Module, Parent, DB}};
    {error, Reason} ->
      messages:error("Cannot write file ~s ~n",
		     [FileName], 
		     gen_test, 
		     write), 
      {stop, error, {Module, Parent, DB}}
  end;
handle_call({insert, Data}, _, {Module, Parent, DB}) ->
  Reply = ets:insert(DB, Data),
  {reply, Reply , {Module, Parent, DB}};
handle_call({list, Type}, _, {Module, Parent, DB}) ->
  Selection = apply({Module, list}, [Type]),
  Reply = ets:match(DB, Selection),
  {reply, Reply , {Module, Parent, DB}};
handle_call({config, Key, Type, Data}, _,{Module, Parent, DB}) ->
  CurrData = rec_ets_message(ets:lookup(DB, Key)),
  NewData = apply({Module, set_data}, [Type, CurrData, Data]),
  Reply = ets:insert(DB, NewData),
  {reply, Reply , {Module, Parent, DB}};
handle_call({delete, Key}, _,{Module, Parent, DB})  ->
  Reply = ets:delete(DB, Key),
  {reply, Reply, {Module, Parent, DB}};
handle_call({lookup, Key}, _,{Module, Parent, DB})  ->
  Reply = rec_ets_message(ets:lookup(DB, Key)),
  {reply, Reply , {Module, Parent, DB}};
handle_call({lookup, Key, EntryType}, _,{Module, Parent, DB})  ->
  CurrData = rec_ets_message(ets:lookup(DB, Key)),
  Reply= apply({Module, get_data}, [EntryType, CurrData]),
  {reply, Reply , {Module, Parent, DB}};
handle_call(Other, _, {Module, Parent, DB}) ->
  messages:error("Undefined message: ~p", [Other], gsb_db, loop),
  {reply, undefined_message, {Module, Parent, DB}}.

terminate(Reason, State) ->
  ok.

rec_ets_message(Data) when list(Data)->
  case Data of
    [] -> [];
    [D] -> D
  end;
rec_ets_message(Data) ->
  messages:error("Unidentified return from lookup ~p", [Data],
		 gsb_db, rec_ets_message).

read_list(FileName) ->
  Path = code:get_path(),
  case file:path_open(Path, FileName, read) of
    {ok, Fd, _} ->
      file:close(Fd);
    {error, Reason} ->
      messages:error("Error opening file ~p: ~p ~n",
		     [FileName,Reason], gsb_db, read_list),
      exit(Reason)
  end,
  case file:path_consult(Path, FileName) of
    {ok, TermList, Fullname} -> TermList;
    {error, open} -> 
      messages:error("Cannot open file ~s~n",
		     [FileName], gsb_db, read_list),
      exit('Cannot open file');
    {error, read}  ->
      messages:error("Cannot read file ~s~n",
		     [FileName], gsb_db, read_list),
      exit('Cannot read file')
  end.

write_list_int([], _Fd) -> true;
write_list_int([First|Rest], Fd) ->
  io:format(Fd, "~w. ~n", [First]),
  write_list_int(Rest, Fd).


