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
-module(diversity).


%% Necessary callback module interface functions!
-export([init/1,
	 terminate/1,
	 create_measurement/5,
	 delete_measurement/3,
	 start_measurement/3,
	 stop_measurement/2,
	 reset_measurement/3,

	 set_upper_threshold/5,  %% Optional!
	 set_lower_threshold/5,  %% Optional!
	 remove_threshold/3,     %% Optional!
	 enable_threshold/3,     %% Optional!
	 disable_threshold/3     %% Optional!
	]).



-export([mrp/0,
	 meas/2
	]).




init(TypeName) ->
    spawn(?MODULE, mrp, []).



terminate(undefined) ->
    ok;
terminate(MRP) ->
    MRP ! terminate,
    ok.



create_measurement(undefined, _Type, _Meas, _Res, _Args) ->
    {error, no_mrp};
create_measurement(MrpInit, TypeName, MeasName, ResId, InitArgs) ->
    MrpInit ! {create_measurement, self(), MeasName, InitArgs},
    receive 
	{Pid,MrpInit} ->
	    io:format("~p, ~p~n", [MeasName,Pid]),
	    MrpInit
    end.


start_measurement(MRP, MeasName, Args) ->
    MRP ! {start, MeasName, Args},
    ok.

stop_measurement(MRP, MeasName) ->
    MRP ! {stop, MeasName},
    ok.


reset_measurement(MRP, MeasName, Args) ->
    MRP ! {reset, MeasName, Args},
    ok.


delete_measurement(MRP, MeasName, Args) ->
    MRP ! {delete_measurement, MeasName, Args},
    ok.







mrp() ->
    process_flag(trap_exit, true),
    mrp_loop([]).



mrp_loop(MeasList) ->
    receive
	{start, Meas, Args} ->
	    case lists:keysearch(Meas,1,MeasList) of
		false ->
		    done;
		{value,{Meas,Pid}} ->
		    Pid ! {start, Args}
	    end,
	    mrp_loop(MeasList);
	{stop, Meas} ->
	    case lists:keysearch(Meas,1,MeasList) of
		false ->
		    done;
		{value,{Meas,Pid}} ->
		    Pid ! stop
	    end,
	    mrp_loop(MeasList);
	{reset, Meas, Args} ->
	    case lists:keysearch(Meas,1,MeasList) of
		false ->
		    done;
		{value,{Meas,Pid}} ->
		    Pid ! {reset, Args}
	    end,
	    mrp_loop(MeasList);
	{create_measurement, Sender, MeasName, InitArgs} ->
	    Pid = spawn_link(?MODULE, meas, [self(), MeasName]),
	    Sender ! {Pid, self()},
	    mrp_loop([{MeasName, Pid} | MeasList]);
	{delete_measurement, MeasName, InitArgs} ->
	    case lists:keysearch(MeasName, 1, MeasList) of
		false ->
		    mrp_loop(MeasList);
		{value, {MeasName,Pid}} ->
		    exit(Pid, kill),
		    mrp_loop(MeasList)
	    end;
	{'EXIT', Pid, Reason} ->
	    case lists:keysearch(Pid, 2, MeasList) of
		false ->
		    mrp_loop(MeasList);
		{value, {Name,Pid}} ->
		    mesh:measurement_terminated(Name, Reason),
		    mrp_loop(lists:keydelete(Pid, 2, MeasList))
	    end;
	Other ->
	    mrp_loop(MeasList)
    end.




meas(MRP, Name) ->
    process_flag(trap_exit, true),
    meas_started(MRP, Name, []).




meas_started(MRP, Name, Args) ->
    receive
	{start,NewArgs} ->
	    case NewArgs of
		kill ->
		    done;
		measurement_test ->
		    mesh:measurement_report(Name,   0, time()),
		    mesh:measurement_report(Name,  20, time()),
		    mesh:measurement_report(Name,  40, time()),
		    mesh:measurement_report(Name,  60, time()),
		    mesh:measurement_report(Name,  80, time()),
		    mesh:measurement_report(Name, 100, time()),
		    meas_started(MRP, Name, NewArgs);
		_Other ->
		    meas_started(MRP, Name, NewArgs)
	    end;
	stop ->
	    meas_stopped(MRP, Name, Args);
	{reset,NewArgs} ->
	    case NewArgs of
		kill ->
		    done;
		_Other ->
		    io:format("Measurement ~p reset!~n", [Name]),
		    meas_started(MRP, Name, Args)
	    end;
	{'EXIT', MRP, Reason} ->
	    done;
	_Other ->
	    meas_started(MRP, Name, Args)
    after 
	5000 ->
	    case Args of
		measure ->
		    mesh:measurement_report(Name, element(3,time()), time());
		_Other ->
		    done
	    end,
	    meas_started(MRP, Name, Args)
    end.





meas_stopped(MRP, Name, Args) when Args /= kill ->
    receive
	{start,NewArgs} ->
	    case NewArgs of
		kill ->
		    done;
		_Other ->
		    meas_started(MRP, Name, NewArgs)
	    end;
	stop ->
	    meas_stopped(MRP, Name, Args);
	{reset,NewArgs} ->
	    case NewArgs of
		kill ->
		    done;
		_Other ->
		    io:format("Measurement ~p reset!~n", [Name]),
		    meas_started(MRP, Name, Args)
	    end;
	{'EXIT', MRP, Reason} ->
	    done;
	_Other ->
	    meas_stopped(MRP, Name, Args)
    end;
meas_stopped(MRP, Name, Args) ->
    done.





set_upper_threshold(MRP, MeasId, ThreshId, Value, Status) ->
    ok.

set_lower_threshold(MRP, MeasId, ThreshId, Value, Status) ->
    ok.

remove_threshold(MRP, MeasId, ThreshId) ->
    ok.

enable_threshold(MRP, MeasId, ThreshId) ->
    ok.

disable_threshold(MRP, MeasId, ThreshId) ->
    ok.

