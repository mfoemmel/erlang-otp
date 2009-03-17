%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
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
%% %CopyrightEnd%
%%%-------------------------------------------------------------------
%%% File    : wx.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : 
%%%
%%% Created : 22 Feb 2007 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------

%% @doc A port of <a href="http://www.wxwidgets.org/">wxWidgets</a>.
%% 
%% This is the base api of <a href="http://www.wxwidgets.org/">wxWidgets</a>.
%% This module contains functions for 
%% starting and stopping the wx-server, and other utility functions.
%%
%% wxWidgets is object oriented, and not functional.  Where an erlang
%% module represents a class, and the object created by this class
%% have an own type, wxCLASS().  This module represents the base
%% class, and all other wxMODULE's are sub-classes of this class.
%% 
%% Objects of a class are created with wxCLASS:new(...) and destroyed with 
%% wxCLASS:destroy(). Member functions are called with wxCLASS:member(Object, ...)
%% instead of as in C++ Object.member(...).
%% 
%% Sub class modules inherits (non static) functions from it's parents.
%% the inherited functions are not documented in the sub-classes.
%% 
%% This erlang port of wxWidgets, tries to be a one-to-one mapping with the
%% original wxWidgets library. Some things are different though, optional arguments
%% uses property lists and can be in any order. The main difference is the event
%% handling which is different from the original library. See {@link wxEvtHandler}.
%%
%% The following classes are implemented directly as erlang types: <br />
%% wxPoint={x,y},wxSize={w,h},wxRect={x,y,w,h},wxColour={r,g,b [,a]},
%% wxString={@link //stdlib/unicode:charlist()},
%% wxGBPosition={r,c},wxGBSpan={rs,cs},wxGridCellCoords={r,c}.
%%
%% Where all strings are decoded/encoded as UTF32 in native format.
%%
%% wxWidgets uses a process specific environment, which is created by {@link wx:new/0},
%% to allow usage from other procesess use {@link get_env/1} and {@link set_env/0}.
%%
%% Global (classless) functions are located in the wx_misc module.

%% @type wxObject().      Opaque object 
%% @type wx_env().  Wx process enviroment
%% @type wx_mem().  Wx memory area
%% @type colour().  A 3 or 4 tuple: {R,G,B,A} or as argument {R,G,B} is also accepted
%%                  where each colour channal is a an integer between 0-255. 
%% @type datetime(). {{Year,Month,Day}, {Hour,Minute,Second}} in local timezone.
%% @type mouseState().   See #wxMouseState{} defined in wx.hrl


-module(wx).

-export([parent_class/1, new/0, destroy/0,
	 get_env/0,set_env/1, debug/1,
	 batch/1,foreach/2,map/2,foldl/3,foldr/3,
	 getObjectType/1, typeCast/2, 
	 null/0, is_null/1, create_memory/1, get_memory_bin/1]).

-include("wxe.hrl").

%% @hidden
parent_class(_) -> true. %% Let the null pointers be sent down.

%% @spec () -> wxObject()
%% @doc Starts a wx server.
new() ->
    wxe_server:start(),
    null().

%% @spec () -> ok
%% @doc Stops a wx server.
destroy() ->
    wxe_server:stop(),
    erase(?WXE_IDENTIFIER),
    ok.

%% @spec () -> wx_env()
%% @doc Get this process current wx environment.
%% Can be sent to other processes be allow them use wx functionality.
%% @see set_env/1
get_env() ->
    case get(?WXE_IDENTIFIER) of
	undefined -> erlang:error({wxe,unknown_port});
	Env = #wx_env{} -> Env
    end.

%% @spec (wx_env()) -> ok
%% @doc Sets the process wx environment, allows this process to use
%% another process wx environment.
set_env(#wx_env{sv=Pid} = Env) ->
    put(?WXE_IDENTIFIER, Env),
    %%    wxe_util:cast(?REGISTER_PID, <<>>),
    wxe_server:register_me(Pid),
    ok.

%% @spec () -> wxObject()
%% @doc returns the null object
null() ->
    #wx_ref{ref= 0,type=wx}.

%% @spec (wxObject()) -> boolean()
%% @doc Returns true if object is null, false otherwise
is_null(#wx_ref{ref=NULL}) -> NULL =:= 0.

%% @spec (wxObject()) -> atom()
%% @doc Returns the object type
getObjectType(#wx_ref{type=Type}) ->
    Type.

%% @spec (wxObject(), atom()) -> wxObject()
%% @doc Casts the object to class NewType.
%%  It is needed when using functions like wxWindow:findWindow/2, which 
%%  returns an generic wxObject type.
typeCast(Old=#wx_ref{}, NewType) when is_atom(NewType) ->
    Old#wx_ref{type=NewType}.

%% @spec (function()) -> term()
%% @doc Batches all <c>wx</c> commands used in the fun.
%% Improves perfomance of the command processing, grabs the wxWidgets thread
%% so GUI may not be updated directly (and no event processing will be done) 
%%
%% @see map/2
%% @see foreach/2
%% @see foldl/3
%% @see foldr/3
batch(Fun) ->
    ok = wxe_util:cast(?BATCH_BEGIN, <<>>),
    try Fun()
    catch 
	error:W -> erlang:exit({W, erlang:get_stacktrace()});
	throw:W -> erlang:throw(W);
	exit:W  -> erlang:exit(W)
    after 
        ok = wxe_util:cast(?BATCH_END, <<>>)
    end.

%% @spec (function(), list()) -> ok
%% @doc Behaves like {@link //stdlib/lists:foreach/2} but batches wx commands see {@link batch/1}. 
foreach(Fun, List) ->
    ok = wxe_util:cast(?BATCH_BEGIN, <<>>),
    try lists:foreach(Fun, List)
    catch 
	error:W -> erlang:exit({W, erlang:get_stacktrace()});
	throw:W -> erlang:throw(W);
	exit:W  -> erlang:exit(W)
    after 
        ok = wxe_util:cast(?BATCH_END, <<>>)
    end.

%% @spec (function(), list()) -> list()
%% @doc Behaves like {@link //stdlib/lists:map/2} but batches wx commands see {@link batch/1}. 
map(Fun, List) ->
    ok = wxe_util:cast(?BATCH_BEGIN, <<>>),
    try lists:map(Fun, List)
    catch 
	error:W -> erlang:exit({W, erlang:get_stacktrace()});
	throw:W -> erlang:throw(W);
	exit:W  -> erlang:exit(W)
    after 
        ok = wxe_util:cast(?BATCH_END, <<>>)
    end.

%% @spec (function(), term(), list()) -> term()
%% @doc Behaves like {@link //stdlib/lists:foldl/3} but batches wx commands see {@link batch/1}. 
foldl(Fun, Acc, List) ->
    ok = wxe_util:cast(?BATCH_BEGIN, <<>>),
    try lists:foldl(Fun, Acc, List)
    catch 
	error:W -> erlang:exit({W, erlang:get_stacktrace()});
	throw:W -> erlang:throw(W);
	exit:W  -> erlang:exit(W)
    after 
        ok = wxe_util:cast(?BATCH_END, <<>>)
    end.

%% @spec (function(), term(), list()) -> term()
%% @doc Behaves like {@link //stdlib/lists:foldr/3} but batches wx commands see {@link batch/1}. 
foldr(Fun, Acc, List) ->
    ok = wxe_util:cast(?BATCH_BEGIN, <<>>),
    try lists:foldr(Fun, Acc, List)
    catch 
	error:W -> erlang:exit({W, erlang:get_stacktrace()});
	throw:W -> erlang:throw(W);
	exit:W  -> erlang:exit(W)
    after 
        ok = wxe_util:cast(?BATCH_END, <<>>)
    end.

-define(MIN_BIN_SIZE, 64).  %% Current emulator min off heap size

%% @spec (integer()) -> wx_memory()
%% @doc Creates a memory area (of Size in bytes) which can be used by an external library (i.e. opengl).
%% It is up to the client to keep an reference to this object so it does
%% not get garbage collected by erlang while still in use by the external
%% library.
%%
%% This is far from erlang's intentional usage and can crash the erlang emulator,
%% use it carefully.
create_memory(Size) when Size > ?MIN_BIN_SIZE ->
    #wx_mem{bin = <<0:(Size*8)>>, size = Size};
create_memory(Size) ->
    #wx_mem{bin = <<0:((?MIN_BIN_SIZE+1)*8)>>, size = Size}.

%% @spec (wx_memory()) -> binary()
%% @doc Returns the memory area as a binary.
get_memory_bin(#wx_mem{bin=Bin, size=Size}) when Size > ?MIN_BIN_SIZE ->
    Bin;
get_memory_bin(#wx_mem{bin=Bin, size=Size}) ->
    <<WithCorrectSize:Size/binary, _/binary>> = Bin,
    WithCorrectSize.

%% @spec (atom()) -> ok
%%   Level = none | verbose | trace
%% @doc Sets debug level, if debug is verbose or trace
%% each call is printed on console
debug(none) -> debug(0);
debug(verbose) -> debug(1);
debug(trace) -> debug(2);
debug(Level) when is_integer(Level) ->
    case get(?WXE_IDENTIFIER) of
	undefined -> erlang:error({wxe,unknown_port});
	Env = #wx_env{} -> 
	    put(?WXE_IDENTIFIER, Env#wx_env{debug=Level}),
	    ok
    end.

