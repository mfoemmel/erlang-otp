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
-ifndef(_memsup_hrl).
-define(_memsup_hrl,true).
%%% This file has to be kept consistent with ../c_src/memsup.h. 
%%% Keep consistence manually.

%% Defines

-define( SHOW_MEM , 1 ).
-define( SHOW_SYSTEM_MEM , 2 ).
-define( SHOW_SYSTEM_MEM_END , 8#0 ).
%% tags for extended statistics
-define( MEM_SYSTEM_TOTAL , 1 ).
-define( MEM_TOTAL , 2 ).
-define( MEM_FREE , 3 ).
-define( MEM_LARGEST_FREE , 4 ).
-define( MEM_NUMBER_OF_FREE , 5 ).
%% extensions 
-define( MEM_BUFFERS , 6 ).
-define( MEM_CACHED , 7 ).
-define( MEM_SHARED , 8 ).
-define( SWAP_TOTAL , 9 ).
-define( SWAP_FREE , 10 ).

-endif.

