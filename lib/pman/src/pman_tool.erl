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

-module(pman_tool).

%% ---------------------------------------------------------------
%% The user interface exports 
%% ---------------------------------------------------------------

%%-compile(export_all).
-include("assert.hrl").

%% Exported interface
-export([modal_single_select/3,
	 modal_multiple_select/3]).


%% Internal exports
%%

-export ([modal_select_worker/5]).

%Callback functions
-export([ok_cb/5,
	 cancel_cb/5,
	 window_cb/5,
	 listbox_cb/5]).



%% State record.
%%

-record(state,{topwin,
	       frame,
	       calling_pid,
	       caption,
	       listbox,
	       ok_button,
	       cancel_button,
	       choices}).

%% Constants
-define(WIN_WIDTH, 350).
-define(WIN_HEIGHT, 350).

-define(DEFAULT_SELECTMODE, single).
-define(SINGLE_CAPTION,
	{label, {text, "Select one of the following:"}}).
-define(MULTI_CAPTION,
	{label, {text, "Select one or more of the following:"}}).

%%
%% The function modal_single_select(Parent, Choices) enters it's own event
%% loop disregarding all gs events that does not apply to it's functionality.
%%
%% No proper grab is performed, so to the user it may appear as if other 
%% GUI operations are available unless the caller handles the greying out 
%% of all unavailable operations.
%%
%% It is assumed that all choices are unique, since no distinction is made
%% between several equal choices. 
%% 
%% modal_single_select(Parent, Choices)
%% Choices = [String, String,...]
%%

modal_single_select(Parent, Title, Choices) ->
    Pid = spawn (?MODULE, modal_select_worker, [self (),
						Parent, Title, Choices, 
						[{selectmode, single}]]),
    receive 
	{Pid, Value} ->
	    Value
    end.
    

modal_multiple_select(Parent, Title, Choices) ->
    Pid = spawn (?MODULE, modal_select_worker, [self (), 
						Parent, Title, Choices, 
						[{selectmode, single}]]),
    receive 
	{Pid, Value} ->
	    Value
    end.



modal_select_worker(CallingPid, Parent, Title, Choices, Options) ->

    %% create window
    State = create_window(Parent, Title, Choices),


    %% 
    add_callback(State#state.ok_button, {?MODULE, ok_cb}, []),
    add_callback(State#state.cancel_button, {?MODULE, cancel_cb}, []),
    add_callback(State#state.topwin, {?MODULE, window_cb}, []),
    add_callback(State#state.listbox, {?MODULE, listbox_cb}, []),


    %% Set the selectmode according to the specified options
    case lists:keysearch(selectmode, 1, Options) of
	{value, {_Key, multiple}} ->
	    gse:config(State#state.listbox, [{selectmode, multiple}]),
	    gse:config(State#state.caption, [?MULTI_CAPTION]);
	{value, {_Key, single}} ->
	    gse:config(State#state.listbox, [{selectmode, single}]),
	    gse:config(State#state.caption, [?SINGLE_CAPTION]);

	false  ->
	    gse:config(State#state.listbox,
		       [{selectmode, ?DEFAULT_SELECTMODE}])
    end,
    
    
    gse:map(State#state.topwin),
    
    %% enter event loop
    gen_loop(State#state{calling_pid = CallingPid}).
    
    





%% create_window/1 
%% creates a window and returns a state record, where
%% all interesting components are items.

create_window(Parent, Title, Choices) ->


    %% Top window and a frame that covers it entirely, to allow
    %% usage of the packer for geometry management.
    Topwin = gse:window(Parent,[{width, ?WIN_WIDTH},
				{height,?WIN_HEIGHT},
				{configure, true},
				{title, Title}]),
    Frame = gse:frame(Topwin,[{packer_x,[{stretch,1},
					 {stretch,1}]},
			      {packer_y,[{stretch,1},
					 {stretch,5},
					 {stretch,1}]}]),

    %% Caption above the list of items 
    Caption = gse:label(Frame,[{pack_x,{1,2}},
			       {pack_y,{1,1}}]),
    %% List of selectable items
    Listbox = gse:listbox(Frame, [{pack_x,{1,2}},
				  {pack_y,{2,2}},
				  {selection,0},
				  {doubleclick, true},
				  {items, str_choices(Choices)}]),

    


    %% OK & Cancel buttons in a separate frame.
    F13 = gse:frame(Frame,[{bw,1},%{bg,green},
			   {pack_xy,{{1,2},3}},
			   {packer_x,[{stretch,1},
				      {fixed, 60},
				      {stretch,1},
				      {fixed, 60},
				      {stretch,1}]},
			   {packer_y,[{stretch,1},
				      {fixed, 30},
				      {stretch,1}]}]),


    OKButton = gse:button(F13,[{pack_xy,{2,2}},
			       {label,{text,"OK"}}]),
    CancelButton = gse:button(F13,[{pack_xy,{4,2}},
				   {label,{text,"Cancel"}}]),

    gse:resize(Frame, ?WIN_WIDTH, ?WIN_HEIGHT),
    #state{topwin=Topwin,
	   frame=Frame,
	   caption=Caption,
	   listbox=Listbox,
	   ok_button=OKButton,
	   cancel_button=CancelButton,
	   choices=Choices}.


%% str_choices_strings(Choices)
%% takes a list of Erlang terms as argument,
%% returns a list of strings with the print-representation of those terms. 
%% [one, 1, "foo", {snulle, bulle}, [1,2,3]] results in 
%% ["one","1","[102,111,111]","{snulle,bulle}","[1,2,3]"]
str_choices(Choices) ->
    MkString = fun(E) ->
		       lists:flatten(io_lib:format("~s",[E]))
	       end,
    lists:map(MkString, Choices).



%%
%% Callback functions
%%
%% Callback_cb(Object,
%%             Event, 
%%             State,
%%             UserData
%%            )

%% This function is called when the OK button is pressed.
ok_cb(Object, Event, Args, State, UserData) ->
    return_selection(State).

%% This function is called when the Cancle button is pressed.
cancel_cb(Object, Event, Args, State, UserData) ->
    gse:destroy(State#state.topwin),
    {return, {cancelled, cancel}}.
    
%% This function is called when an event occurs in a window..
window_cb(Object, destroy, Args, State, UserData) ->
    {return, {cancelled, destroyed}};

window_cb(Object, configure, [Width, Height|_Rest], State, UserData) ->
    gse:resize(State#state.frame, Width, Height),
    {state, State}.



%% A doubleclick int the listbox will cause the dialog to return
listbox_cb(Object, doubleclick, Args, State, UserData) ->
    return_selection(State).



return_selection(State) ->

    GetObject =
	fun(I) ->
		lists:nth(I+1, State#state.choices) 
	end,

    case gs:read(State#state.listbox, selection) of
	%% Empty selection
	[] ->
	    gse:destroy(State#state.topwin),
	    {return, {cancelled, no_selection}};
	%% One or more items selected
	[Hd|Tl] ->
	    Selection = lists:map(GetObject, [Hd|Tl]),
	    gse:destroy(State#state.topwin),
	    {return, Selection}
    
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% General functions for user interface development.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%
%% General loop, to be used by all gs-applications.
%% Will call any registered callbacks.
%% 
    
    
gen_loop(State) ->


    receive

	%% Handle all cases of events where no callback is defined.
	{gs, Object, Event, [], _Args} ->
	    ?ALWAYS_ASSERT("gen_loop: No callback defined"),	    
	    gen_loop(State);

	%% Handle all evets with a defined callback.
	{gs, Object, Event, {callback, Callback}, Args} ->
	    case apply_callback(Object, Event, Args, Callback, State) of
		{return, Value} -> 
		    State#state.calling_pid ! {self (), Value};

		{state, State1} -> 
		    gen_loop(State1)
	    end;
	AnyEvent ->
	    ?ALWAYS_ASSERT("Received unexpected event"),
	    gen_loop(State)
	    
    end.

%%
%% 
%%

apply_callback(Object, Event, Args, Callback, State) ->
    {Module, Function, UserData} = Callback,
    apply(Module, Function, [Object, Event, Args, State, UserData]).




%% All callback functions must return one of the following type of return
%% values
%% {return, Value}          The loop will terminate, and return Value, and 
%%                          the State
%% {state, State}           Loop continues, with new State
%%

add_callback(Object, {Module, Function}, UserData) ->
    gse:config(Object, [{data, {callback, {Module, Function, UserData}}}]).





