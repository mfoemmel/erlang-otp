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

-module(test1).



-export([start/0,
	 init/0
	]).




init() ->
    mesh_server:start_link(),
    loop().


loop() ->
    receive 
	Msg ->
	    loop()
    end.


start() ->
    mnesia:start(),
    mnesia:create_schema([]),
    mnesia:delete_table(mesh_type),
    mnesia:delete_table(mesh_meas),
    mesh_init:init_tables(),
    spawn(?MODULE,init,[]),
    
    mesh:watchdog_setup(5,10),

    mesh:register_type(diversity1, "", diversity, 10),
    mesh:register_type(diversity2, "", diversity, 8),
    mesh:register_type(diversity3, "", diversity, 6),
    mesh:register_type(diversity4, "", diversity, 4),
    mesh:register_type(diversity5, "", diversity, 2),
    mesh:register_type(diversity6, "", diversity, 0),  %% Alarm here!

    mesh:unregister_type(diversity1), %% Alarm cleared here!
    mesh:unregister_type(diversity1),
    mesh:unregister_type(diversity2),
    mesh:unregister_type(diversity3),
    mesh:unregister_type(diversity4),
    mesh:unregister_type(diversity5),
    mesh:register_type(diversity6, "", diversity, 0),  %% Alarm here!

    mesh:watchdog_setup(6,10),  %% Alarm cleared here!
    
    mesh:register_type(diversity1, "", diversity, 10),
    mesh:register_type(diversity2, "", diversity, 8),
    mesh:register_type(diversity3, "", diversity, 6),
    mesh:register_type(diversity4, "", diversity, 4),
    mesh:register_type(diversity5, "", diversity, 2),
    mesh:register_type(diversity6, "", diversity, 0).
    
