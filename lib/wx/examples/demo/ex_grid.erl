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

-module(ex_grid).

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
	  config
	}).

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
				 [{label, "wxGrid"}]),
    Grid1 = create_grid1(Panel),
    %% Add to sizers
    Options = [{flag, ?wxEXPAND}, {proportion, 1}],

    wxSizer:add(Sizer, Grid1, Options),
    wxSizer:add(MainSizer, Sizer, Options),

    wxPanel:setSizer(Panel, MainSizer),
    {Panel, #state{parent=Panel, config=Config}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks handled as normal gen_server callbacks
handle_info(Msg, State) ->
    demo:format(State#state.config, "Got Info ~p\n", [Msg]),
    {noreply, State}.

handle_call(Msg, _From, State) ->
    demo:format(State#state.config, "Got Call ~p\n", [Msg]),
    {reply,{error, nyi}, State}.

%% Async Events are handled in handle_event as in handle_info
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

create_grid1(Panel) ->
    Grid = wxGrid:new(Panel, 2, []),
    wxGrid:createGrid(Grid, 100, 5),
    Font = wxFont:new(16, ?wxFONTFAMILY_SWISS,
		      ?wxFONTSTYLE_NORMAL,
		      ?wxFONTWEIGHT_NORMAL, []),
    Fun =
	fun(Int) ->
		wxGrid:setCellValue(Grid, Int, 0, "Value"),
		wxGrid:setCellValue(Grid, Int, 1, "Value"),
		wxGrid:setCellValue(Grid, Int, 2, "Value"),
		wxGrid:setCellValue(Grid, Int, 3, "Read only"),
		wxGrid:setCellTextColour(Grid, Int, 3, ?wxWHITE),
		wxGrid:setReadOnly(Grid, Int, 3, [{isReadOnly,true}]),
		wxGrid:setCellValue(Grid, Int, 4, "Value"),
		case Int rem 4 of
		    0 -> wxGrid:setCellBackgroundColour(Grid, Int, 3, ?wxRED);
		    1 -> wxGrid:setCellBackgroundColour(Grid, Int, 3, ?wxGREEN),
			 wxGrid:setCellTextColour(Grid, Int, 2, {255,215,0,255});
		    2 -> wxGrid:setCellBackgroundColour(Grid, Int, 3, ?wxBLUE);
		    _ -> wxGrid:setCellBackgroundColour(Grid, Int, 1, ?wxCYAN),
			 wxGrid:setCellValue(Grid, Int, 1,
					     "Centered\nhorizontally"),
			 wxGrid:setCellAlignment(Grid, Int, 4,
						 0,?wxALIGN_CENTER),
			 wxGrid:setCellValue(Grid, Int, 4,
					     "Centered\nvertically"),
			 wxGrid:setCellAlignment(Grid, Int, 1,
						 ?wxALIGN_CENTER,0),
			 wxGrid:setCellTextColour(Grid, Int, 3, ?wxBLACK),
			 wxGrid:setCellAlignment(Grid, Int, 2,
						 ?wxALIGN_CENTER,
						 ?wxALIGN_CENTER),
			 wxGrid:setCellFont(Grid, Int, 0, Font),
			 wxGrid:setCellValue(Grid, Int, 2,
					     "Centered vertically\nand horizontally"),
			 wxGrid:setRowSize(Grid, Int, 80)
		end
	end,
    wx:foreach(Fun, lists:seq(0,99)),
    wxGrid:setColSize(Grid, 2, 150),
    wxGrid:connect(Grid, grid_cell_change),
    Grid.


