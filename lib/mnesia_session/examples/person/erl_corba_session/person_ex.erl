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
%%% Purpose : Examples of mnesia_corba_sesion interface to mnesia
%%%----------------------------------------------------------------------

%% Start the client with:
%%  erl -sname client -orber iiop_port 5001 -orber bootstrap_port 5002 -orber domain \"client\"
%%  And the server with:
%%  erl -sname server -orber domain \"server\"

-module(person_ex).

-compile(export_all).

%%%Includes
%% Include to get the mnesia types, used in create table
-include_lib("mnesia_session/include/mnesia.hrl").   

%% Include to get the corba types i.e. any
-include_lib("orber/include/corba.hrl").

%% Include my own types
-include("persons.hrl").  
%%%Includes

run() ->
    %% Port 4001 is orber's default"),
    Host = list_to_atom(hd(string:tokens(net_adm:localhost(), [$.]))),
    run(Host, 4001).

run(ServerHost, Port) ->
    {Cok, Sok} = start_corba_session(ServerHost, Port),
    io:format("Create a persons_person table~n"),
    create_person_table(Sok),
    PD = insert_person(Sok, "Dan", male, 27, 97185, "none", []),    
    io:format("Inserted person ~p in mnesia~n", [PD]),
    PN = insert_person(Sok, "Niklas", male, 38, 99999, "Lisa", ["Jonas", "Ola"]), 
    io:format("Inserted person ~p in mnesia~n", [PN]),
    RES = get_person(Sok, "Dan"),
    io:format("Read person ~p from mnesia ~p ~n", ["Dan", RES]),
    mnesia_corba_connector:disconnect(Cok, Sok).

% Starts a mnesia_corba_session object
%%%Init
start_corba_session(Host, Port) ->
    %% Lookup the inital corba name sever
    Addr = "iiop://" ++ atom_to_list(Host) ++ ":" ++ integer_to_list(Port),
    NS = corba:resolve_initial_references_remote("NameService", [Addr]),

    %% Create a corba name object 
    NC = lname_component:set_id(lname_component:create(),
				"mnesia_corba_connector"),    
    N = lname:insert_component(lname:create(), 1, NC),

    %% Lookup the object reference to the factory mnesia_corba_connector
    Cok = 'CosNaming_NamingContext':resolve(NS, N),

    %% Use the factory to create a session
    Sok = mnesia_corba_connector:connect(Cok),
    {Cok, Sok}.
%%%Init

%% Some examples of how to use the mnesia_corba_session interface!

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
			      %% NOTE that the record name must be exactly the 
			      %% same as the name of the structure/record to be used
			      record_name = "persons_person"},    

    Res = mnesia_corba_session:create_table(ObjKey, "persons", TabDef),
    case Res of 
	{ok, ""} ->	   
	    ok;
	Else ->
	    io:format("ERROR: ~s~n", [Else])
    end.
%%%create_table

%%%dirty_write
insert_person(SessionKey, Name, Sex, Age, Phone, Mt, Ch) ->
    Data = #persons_data{sex = Sex, age = Age, phone = Phone},
    Person = #persons_person{name = Name, personData = Data, 
			     married_to = Mt, children = Ch},
    
    Any = #any{typecode = persons_person:tc(), value = Person},
    {ok, ""} = mnesia_corba_session:dirty_write(SessionKey, "persons", Any),
    Person.
%%%dirty_write

%%%dirty_read
get_person(SessionKey, Name) ->
    Obj = #any{typecode = {tk_string, 0}, value = Name},
    {ok, RObjs, ""} = 
	mnesia_corba_session:dirty_read(SessionKey, "persons", Obj),
    RObj = hd(RObjs),
    RObj#any.value.
%%%dirty_read

