%%--------------------------------------------------------------------
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
%% File    : CosTime_TIO_impl.erl
%% Purpose : 
%% Created : 10 Feb 2000
%%----------------------------------------------------------------------

-module('CosTime_TIO_impl').

%%--------------- INCLUDES -----------------------------------
-include("cosTimeApp.hrl").

%%--------------- EXPORTS ------------------------------------
%%--------------- External -----------------------------------
%% Attributes (external)
-export(['_get_time_interval'/2]).

%% Interface functions
-export([spans/3, overlaps/3, time/2]).

%%--------------- gen_server specific exports ----------------
-export([handle_info/2, code_change/3]).
-export([init/1, terminate/2]).


%% Data structures
-record(state, {interval,
		tdf,
		timer}).
%% Data structures constructors
-define(get_InitState(I,T,TO), 
	#state{interval = I,
	       tdf      = T,
	       timer    = TO}).

%% Data structures selectors
-define(get_IntervalT(S),      S#state.interval).
-define(get_Lower(S),          (S#state.interval)#'TimeBase_IntervalT'.lower_bound).
-define(get_Upper(S),          (S#state.interval)#'TimeBase_IntervalT'.upper_bound).
-define(get_Tdf(S),            S#state.tdf).
-define(get_TimerObj(S),       S#state.timer).

%%-----------------------------------------------------------%
%% function : handle_info, code_change
%% Arguments: 
%% Returns  : 
%% Effect   : Functions demanded by the gen_server module. 
%%------------------------------------------------------------

code_change(OldVsn, State, Extra) ->
    {ok, State}.
handle_info(Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------%
%% function : init, terminate
%% Arguments: 
%%-----------------------------------------------------------

init([Interval, Tdf, Timer]) ->
    {ok, ?get_InitState(Interval, Tdf, Timer)}.

terminate(Reason, State) ->
    ok.

%%-----------------------------------------------------------
%%------------------------ attributes -----------------------
%%-----------------------------------------------------------
%%----------------------------------------------------------%
%% Attribute: '_get_time_interval'
%% Type     : readonly
%% Returns  : 
%%-----------------------------------------------------------
'_get_time_interval'(OE_THIS, State) ->
    {reply, ?get_IntervalT(State), State}.

%%-----------------------------------------------------------
%%------- Exported external functions -----------------------
%%-----------------------------------------------------------
%%----------------------------------------------------------%
%% function : spans
%% Arguments: Time - UTO
%% Returns  : CosTime::OverLapType - enum()
%%            TIO - out-parameter.
%%-----------------------------------------------------------
spans(OE_THIS, State, Time) ->
    ?time_TypeCheck(Time, 'CosTime_UTO'),
    case catch 'CosTime_UTO':'_get_utc_time'(Time) of
	#'TimeBase_UtcT'{time = Btime, inacclo = InaccL, inacchi=InaccH} ->
	    Inaccuarcy = ?concat_TimeT(InaccH, InaccL),
	    BL = Btime - Inaccuarcy,
	    BU = Btime + Inaccuarcy,
	    L = ?get_Lower(State),
	    U = ?get_Upper(State),
	    {Type, NewL, NewU} = 
		if
		    L=<BL, U>=BU ->
			{'OTContainer',BL,BU};
		    L>=BL, U=<BU ->
			{'OTContained',L,U};
		    L<BL, U=<BU, U>=BL ->
			{'OTOverlap',BL,U};
		    L>=BL, L=<BU, U>BU ->
			{'OTOverlap',L,BU};
		    L>BU ->
			{'OTNoOverlap',BU,L};
		    true ->
			{'OTNoOverlap',U,BL}
		end,
	    {reply, 
	     {Type,
	      'CosTime_TIO':oe_create([#'TimeBase_IntervalT'{lower_bound=NewL, 
							     upper_bound=NewU},
				       ?get_Tdf(State),
				       ?get_TimerObj(State)], 
				      [{pseudo,true}|?CREATE_OPTS])}, 
	     State};
	_ ->
	    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
    end.


%%----------------------------------------------------------%
%% function : overlaps
%% Arguments: Interval - TIO
%% Returns  : CosTime::OverLapType - enum()
%%            TIO - out-parameter.
%%-----------------------------------------------------------
overlaps(OE_THIS, State, Interval) ->
    ?time_TypeCheck(Interval, 'CosTime_TIO'),
    case catch 'CosTime_TIO':'_get_time_interval'(Interval) of
	#'TimeBase_IntervalT'{lower_bound=BL, upper_bound=BU} ->
	    L = ?get_Lower(State),
	    U = ?get_Upper(State),
	    {Type, NewL, NewU} = 
		if
		    L=<BL, U>=BU ->
			{'OTContainer',BL,BU};
		    L>=BL, U=<BU ->
			{'OTContained',L,U};
		    L<BL, U=<BU, U>=BL ->
			{'OTOverlap',BL,U};
		    L>=BL, L=<BU, U>BU ->
			{'OTOverlap',L,BU};
		    L>BU ->
			{'OTNoOverlap',BU,L};
		    true ->
			{'OTNoOverlap',U,BL}
		end,
	    {reply, 
	     {Type,
	      'CosTime_TIO':oe_create([#'TimeBase_IntervalT'{lower_bound=NewL, 
							     upper_bound=NewU},
				       ?get_Tdf(State),
				       ?get_TimerObj(State)], 
				      [{pseudo,true}|?CREATE_OPTS])}, 
	     State};
	_ ->
	    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
    end.

%%----------------------------------------------------------%
%% function : time
%% Arguments: -
%% Returns  : UTO
%%-----------------------------------------------------------
time(OE_THIS, State) ->
    L = ?get_Lower(State),
    H = ?get_Upper(State),
    Utc = #'TimeBase_UtcT'{time=(erlang:trunc(((H-L)/2))+L),
			   inacclo=L,
			   inacchi=H,
			   tdf=?get_Tdf(State)},
    {reply, 
     'CosTime_UTO':oe_create([Utc, ?get_TimerObj(State)], [{pseudo,true}|?CREATE_OPTS]), 
     State}.


%%--------------- LOCAL FUNCTIONS ----------------------------

%%--------------- MISC FUNCTIONS, E.G. DEBUGGING -------------
%%--------------- END OF MODULE ------------------------------
