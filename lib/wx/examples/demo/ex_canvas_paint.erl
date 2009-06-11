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

-module(ex_canvas_paint).

-behavoiur(wx_object).

%% Client API
-export([start/1]).

%% wx_object callbacks
-export([init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_event/2, handle_sync_event/3]).

-include_lib("wx/include/wx.hrl").

-record(state, 
	{
	  parent,
	  config,
	  canvas,
	  pen,
	  brush,
	  old_pos,
	  bitmap
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
				 [{label, "wxDC"}]),
    Canvas = wxPanel:new(Panel, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),
    wxPanel:setToolTip(Panel,
		       "Left-click and hold to draw something - release to stop drawing.\n"
		       "Middle-click to fill with pink\n"
		       "Middle-dclick to fill with white.\n"
		       "Right-click to clear."),

    Brush = wxBrush:new(?wxWHITE),
    Pen = wxPen:new(?wxBLACK, [{width, 2}]),

    %% Add to sizers
    wxSizer:add(Sizer, Canvas, [{flag, ?wxEXPAND},
				{proportion, 1}]),

    wxSizer:add(MainSizer, Sizer, [{flag, ?wxEXPAND},
				   {proportion, 1}]),
    {W,H} = wxPanel:getSize(Canvas),
    Bitmap = wxBitmap:new(W,H),
    
    wxPanel:connect(Canvas, paint, [callback]),
    wxPanel:connect(Canvas, size),
    wxPanel:connect(Canvas, left_down),
    wxPanel:connect(Canvas, left_dclick),
    wxPanel:connect(Canvas, left_up),
    wxPanel:connect(Canvas, right_down),
    wxPanel:connect(Canvas, middle_down),
    wxPanel:connect(Canvas, middle_dclick),
    wxPanel:setSizer(Panel, MainSizer),
    wxSizer:layout(MainSizer),
    {Panel, #state{parent=Panel, config=Config,
		   canvas = Canvas, pen = Pen,
		   brush = Brush, bitmap = Bitmap}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks handled as normal gen_server callbacks
handle_info(Msg, State) ->
    demo:format(State#state.config, "Got Info ~p\n", [Msg]),
    {noreply, State}.

handle_call(Msg, _From, State) ->
    demo:format(State#state.config, "Got Call ~p\n", [Msg]),
    {reply,{error, nyi}, State}.

%% Sync event from callback events, paint event must be handled in callbacks
%% otherwise nothing will be drawn on windows.
handle_sync_event(#wx{event = #wxPaint{}}, _wxObj, #state{canvas=Canvas, bitmap=Bitmap}) ->
    DC = wxPaintDC:new(Canvas),
    redraw(DC, Bitmap),
    wxPaintDC:destroy(DC),
    ok.

%% Async Events are handled in handle_event as in handle_info
%% Draw a line
handle_event(#wx{event = #wxMouse{type = motion, x = X, y = Y}},
	     State = #state{canvas = Canvas, pen = Pen, brush = Brush}) ->
    Fun =
	fun(DC) -> wxDC:setPen(DC, Pen),
		   wxBrush:setColour(Brush, ?wxBLACK),
		   wxDC:setBrush(DC, Brush),
		   wxDC:drawLine(DC, {X,Y}, State#state.old_pos)
	end,
    draw(Canvas,State#state.bitmap, Fun),
    {noreply, State#state{old_pos = {X,Y}}};
handle_event(#wx{event = #wxSize{size = {W,H}}}, State = #state{bitmap=Prev}) ->
    wxBitmap:destroy(Prev),
    Bitmap = wxBitmap:new(W,H),
    draw(State#state.canvas, Bitmap, fun(DC) -> wxDC:clear(DC) end),
    {noreply, State#state{bitmap=Bitmap}};
handle_event(#wx{event = #wxMouse{type = left_dclick,x = X,y = Y}}, State = #state{}) ->
    wxPanel:connect(State#state.canvas, motion),
    {noreply, State#state{old_pos = {X,Y}}};
handle_event(#wx{event = #wxMouse{type = left_down,x = X,y = Y}}, State = #state{}) ->
    wxPanel:connect(State#state.canvas, motion),
    {noreply, State#state{old_pos = {X,Y}}};

%% Fill with pink color
handle_event(#wx{event = #wxMouse{type = middle_down,x = X, y =Y}}, State = #state{}) ->
    Fun =
	fun(DC) -> wxBrush:setColour(State#state.brush, {255,125,255,255}),
		   wxDC:setBrush(DC, State#state.brush),
		   wxDC:floodFill(DC, {X,Y}, ?wxBLACK, [{style, ?wxFLOOD_BORDER}])
	end,
    
    draw(State#state.canvas, State#state.bitmap, Fun),
    {noreply, State};

%% Fill with white color
handle_event(#wx{event = #wxMouse{type = middle_dclick,x = X, y =Y}}, State = #state{}) ->
    Fun =
	fun(DC) -> wxBrush:setColour(State#state.brush, ?wxWHITE),
		   wxDC:setBrush(DC, State#state.brush),
		   wxDC:floodFill(DC, {X,Y}, ?wxBLACK, [{style, ?wxFLOOD_BORDER}])
	end,
    
    draw(State#state.canvas,  State#state.bitmap,Fun),
    {noreply, State};
handle_event(#wx{event = #wxMouse{type = left_up}}, State = #state{}) ->
    wxPanel:disconnect(State#state.canvas, motion),
    {noreply, State};
handle_event(#wx{event = #wxMouse{type = right_down}}, State = #state{}) ->
    draw(State#state.canvas, State#state.bitmap, fun(DC) -> wxDC:clear(DC) end),
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

draw(Canvas, Bitmap, Fun) ->
    MemoryDC = wxMemoryDC:new(Bitmap),
    CDC = wxClientDC:new(Canvas),

    Fun(MemoryDC),
    
    wxDC:blit(CDC, {0,0},
	      {wxBitmap:getWidth(Bitmap), wxBitmap:getHeight(Bitmap)},
	      MemoryDC, {0,0}),
    
    wxClientDC:destroy(CDC),
    wxMemoryDC:destroy(MemoryDC).


redraw(DC, Bitmap) ->
    MemoryDC = wxMemoryDC:new(Bitmap),

    wxDC:blit(DC, {0,0},
	      {wxBitmap:getWidth(Bitmap), wxBitmap:getHeight(Bitmap)},
	      MemoryDC, {0,0}),

    wxMemoryDC:destroy(MemoryDC).


