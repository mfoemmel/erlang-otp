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
%%% Purpose : Examples of mnesia_sesion interface to mnesia
%%%----------------------------------------------------------------------

-module(person_ex).

-compile(export_all).
%%-export([Function/Arity, ...]).

%% Use setup:go() to initiate the example

%%%Includes
%% Include to get the mnesia types, used in create table
-include_lib("mnesia_session/include/mnesia.hrl").   

%% Include my own definitions
-include("persons.hrl").  
%%%Includes

run() ->
    run(node()).

run(ServerNode) ->
    {ConnPid, SessPid} = start_session(ServerNode),
    io:format("Create a persons_person table~n"),
    create_person_table(SessPid),
    PD = insert_person(SessPid, "Dan", male, 27, 97185, "none", []),    
    io:format("Inserted person ~p in mnesia~n", [PD]),
    PN = insert_person(SessPid, "Niklas", male, 38, 99999, "Lisa", ["Jonas", "Ola"]), 
    io:format("Inserted person ~p in mnesia~n", [PN]),
    RES = get_person(SessPid, "Dan"),
    io:format("Read person ~p from mnesia ~p ~n", ["Dan", RES]),
    mnesia_connector:disconnect(ConnPid, SessPid).

% Starts a mnesia_session object
%%%Init
start_session(Node) ->
    ConnPid = rpc:call(Node, mnesia_session_lib, lookup_connector, []),
    %% Use the factory to create a session
    SessPid = mnesia_connector:connect(ConnPid),
    {ConnPid, SessPid}.
%%%Init

%% Some examples of how to use the mnesia_session interface!

%%%create_table
create_person_table(ObjKey) ->
    %% Define the table properties
    Attrs = [atom_to_list(F) || F <- record_info(fields, persons_person)],
    TabDef = #mnesia_TableDef{type = bag, mode = read_write, 
			      ram_copies = [],
			      disc_copies = [], 
			      disc_only_copies = [],
			      index_list = [4],  %% Index on married_to
			      attributes = Attrs,
			      %% OBSERVE that the record_name must be 
			      %% exactly the same as the name of the 
			      %% structure/record  
			      record_name = "persons_person"},
    
    Res = mnesia_session:create_table(ObjKey, "persons", TabDef),
    case Res of 
	{ok, ""} ->	   
	    ok;
	Else->
	    io:format("ERROR: ~s~n", [Else])
    end.
%%%create_table

%%%dirty_write
insert_person(SessionKey, Name, Sex, Age, Phone, Mt, Ch) 
  when list(Name), atom(Sex), integer(Age), 
       integer(Phone), list(Mt), list(Ch) ->
    
    Data = #persons_data{sex = Sex, age = Age, phone = Phone},
    Person = #persons_person{name = Name, personData = Data, 
			     married_to = Mt, children = Ch},
    
    {ok, ""} = mnesia_session:dirty_write(SessionKey, "persons", Person),
    Person.
%%%dirty_write

%%%dirty_read
get_person(SessionKey, Name) when list(Name) ->
    {ok, RObj, ""} = 
	mnesia_session:dirty_read(SessionKey, "persons", Name),    
    hd(RObj).
%%%dirty_read
