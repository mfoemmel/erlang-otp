%%----------------------------------------------------------------------
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
%%----------------------------------------------------------------------
%% File        : CosFileTransfer_FileIterator_impl.erl
%% Description : 
%%
%% Created     : 12 Sept 2000
%%----------------------------------------------------------------------
-module('CosFileTransfer_FileIterator_impl').




%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include("cosFileTransferApp.hrl").

%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([init/1,
	 terminate/2,
	 code_change/3,
	 handle_info/2]).

%% Interface functions
-export([next_one/2,
	 next_n/3,
	 destroy/2]).

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
-export([]).

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------

%%======================================================================
%% External functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function   : init/1
%% Returns    : {ok, State}          |
%%              {ok, State, Timeout} |
%%              ignore               |
%%              {stop, Reason}
%% Description: Initiates the server
%%----------------------------------------------------------------------
init([FileList]) ->
    {ok, FileList}.

%%----------------------------------------------------------------------
%% Function   : terminate/2
%% Returns    : any (ignored by gen_server)
%% Description: Shutdown the server
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%----------------------------------------------------------------------
%% Function   : code_change/3
%% Returns    : {ok, NewState}
%% Description: Convert process state when code is changed
%%----------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%---------------------------------------------------------------------%
%% function : handle_info/2
%% Arguments: 
%% Returns  : 
%% Effect   : 
%%----------------------------------------------------------------------
handle_info(Info, State) ->
    case Info of
        {'EXIT', Pid, Reason} ->
            {stop, Reason, State};
        Other ->
            {noreply, State}
    end.

%%======================================================================
%% CosFileTransfer::FileIterator
%%======================================================================
%%---------------------------------------------------------------------%
%% Function   : next_one
%% Arguments  : 
%% Returns    : {boolean(), FileWrapper}
%% Description: 
%%----------------------------------------------------------------------
next_one(OE_This, []) ->
    {reply, {false, 
	     #'CosFileTransfer_FileWrapper'{the_file = corba:create_nil_objref(),
					    file_type = nfile}}, []};
next_one(OE_This, [FileWrapper]) ->
    {reply, {true, FileWrapper}, []};
next_one(OE_This, [FileWrapper|Rest]) ->
    {reply, {true, FileWrapper}, Rest}.

%%---------------------------------------------------------------------%
%% Function   : next_n
%% Arguments  : HowMany - ulong()
%% Returns    : {boolean(), FileWrapperList}
%% Description: 
%%----------------------------------------------------------------------
next_n(OE_This, [], _) ->
    {reply, {false, []}, []};
next_n(OE_This, FileWrapperList, HowMany) when HowMany > length(FileWrapperList) ->
    {reply, {true, FileWrapperList}, []};
next_n(OE_This, FileWrapperList, HowMany) ->
    {reply, {true, lists:sublist(FileWrapperList, HowMany)}, 
     lists:nthtail(HowMany, FileWrapperList)}.

%%---------------------------------------------------------------------%
%% Function   : destroy
%% Arguments  : -
%% Returns    : -
%% Description: 
%%----------------------------------------------------------------------
destroy(OE_This, State) ->
    {stop, normal, ok, State}.


%%======================================================================
%% Internal functions
%%======================================================================

    

%%======================================================================
%% END OF MODULE
%%======================================================================
