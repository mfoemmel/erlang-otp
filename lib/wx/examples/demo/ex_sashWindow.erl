%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009. All Rights Reserved.
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

-module(ex_sashWindow).

-behavoiur(wx_object).

%% Client API
-export([start/1]).

%% wx_object callbacks
-export([init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_event/2]).

-include_lib("wx/include/wx.hrl").

-record(state, 
	{
	  parent,
	  config,
	  top_sash,
	  bottom_sash
	}).

-define(TOP_SASH, 1).
-define(BOTTOM_SASH, 2).

start(Config) ->
    wx_object:start_link(?MODULE, Config, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Config) ->
    wx:batch(fun() -> do_init(Config) end).

do_init(Config) ->
    Parent = proplists:get_value(parent, Config),  
    Panel = wxPanel:new(Parent, []),

    %% Setup sizers
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
				 [{label, "wxSashWindow"}]),

    TopSash = wxSashWindow:new(Panel, [{id, ?TOP_SASH}]),
    wxSashWindow:setBackgroundColour(TopSash, ?wxRED),
    BottomSash = wxSashWindow:new(Panel, [{id, ?BOTTOM_SASH}]),
    wxSashWindow:setBackgroundColour(BottomSash, ?wxBLUE),
    {W,H} = wxPanel:getSize(Panel),
    wxSashWindow:setMinSize(TopSash, {W,H div 2}),
    wxSashWindow:setMinSize(BottomSash, {W,H div 2}),
    wxSashWindow:setSize(TopSash, {0,0,W,H div 2}),
    wxSashWindow:setSize(BottomSash, {0,H div 2,W,H div 2}),

    wxSashWindow:setSashVisible(TopSash, ?wxSASH_BOTTOM, true),
    wxPanel:connect(Panel, sash_dragged),
    wxPanel:connect(Panel, size),
    %wxPanel:setMinSize(Panel, {100,100}),

    %% Add to sizers
    Options = [{flag, ?wxEXPAND}, {proportion, 1}],

    wxSizer:add(Sizer, TopSash, Options),
    wxSizer:add(Sizer, BottomSash, Options),
    wxSizer:add(MainSizer, Sizer, Options),

    wxPanel:setSizer(Panel, MainSizer),
    wxSizer:fit(MainSizer, Panel),
    wxSizer:setSizeHints(MainSizer, Panel),
    {Panel, #state{parent=Panel, config=Config,
		   top_sash = TopSash, bottom_sash = BottomSash}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks handled as normal gen_server callbacks
handle_info(Msg, State) ->
    demo:format(State#state.config, "Got Info ~p\n", [Msg]),
    {noreply, State}.

handle_call(Msg, _From, State) ->
    demo:format(State#state.config, "Got Call ~p\n", [Msg]),
    {reply,{error, nyi}, State}.

%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{event = #wxSash{dragRect = {_X,Y, _W, H}}},
	     State = #state{top_sash = TopSash,
			    bottom_sash = BottomSash}) ->
    {OldW, OldH} = wxPanel:getSize(State#state.parent),
    Diff = OldH - H,
    {OldX, _} = wxSashWindow:getPosition(BottomSash),
    wxSashWindow:setMinSize(BottomSash, {OldW,Diff}),
    wxSashWindow:setMinSize(TopSash, {OldW,H}),
    wxSashWindow:setSize(BottomSash, {OldX, Y,OldW,Diff}),
    wxSashWindow:setSize(TopSash, {OldW,H}),
    wxPanel:refresh(State#state.parent),
    {noreply, State};
handle_event(#wx{event = #wxSize{size = {W, H}}},
	     State = #state{top_sash = TopSash,
			    bottom_sash = BottomSash}) ->
    wx:batch(fun() ->
		     {OldX, OldY} = wxSashWindow:getPosition(BottomSash),
		     {_OldW,OldH} = wxSashWindow:getSize(TopSash),
		     NewH = H - OldH,
		     if OldH < 10; H < 10 ->
			     ignore;			 
			 H < OldH +10 ->
			     wxSashWindow:setMinSize(TopSash, {W,OldH-3}),
			     wxSashWindow:setMinSize(BottomSash, {W,NewH}),
			     wxSashWindow:setSize(TopSash, {W,OldH-3}),
			     wxSashWindow:setSize(BottomSash, {OldX,OldY-3,W,NewH});
			true ->
			     wxSashWindow:setMinSize(TopSash, {W,OldH}),
			     wxSashWindow:setMinSize(BottomSash, {W,NewH}),
			     wxSashWindow:setSize(TopSash, {W,OldH}),
			     wxSashWindow:setSize(BottomSash, {OldX,OldY,W,NewH})
		     end,
		     wxPanel:refresh(State#state.parent)
	     end),

    {noreply, State};
handle_event(Ev = #wx{}, State = #state{}) ->
    demo:format(State#state.config, "Got Event ~p\n", [Ev]),
    {noreply, State}.

code_change(_, _, State) ->
    {stop, ignore, State}.

terminate(_Reason, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

