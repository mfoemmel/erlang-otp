-module(xc).
-author('jakob@erix.ericsson.se').

-include("erl_com.hrl").

%% enum XlChartFormat
-define(XlPieExploded, 69).
-define(XlPie, 5).

%% enum XlChartLocation
-define(xlLocationAsNewSheet, 1).
-define(xlLocationAsObject, 2).
-define(xlLocationAutomatic, 3.


%% enum XlRowCol
-define(xlColumns, 2).
-define(xlRows, 1).


-export([populate_area/4, f/3, make_graph/6, sample1/0]).

to_cell_col(C) when C > 26 ->
	[C / 26 + 64, C rem 26 + 64];
to_cell_col(C) ->
	[C+64].

populate_area(E, _, _, []) ->
	ok;
populate_area(E, Row, Col, [Data | Resten]) ->
	Cell= to_cell_col(Col)++integer_to_list(Row),
	io:format(" ~s ~n ", [Cell]),
	N= erl_com:property_get(E, "range", [Cell]),
	Range= erl_com:package_interface(E, N),
	erl_com:property_put(Range, "Value", Data),
	erl_com:release(Range),
	populate_area(E, Row+1, Col, Resten).

f(E, _, []) ->
	ok;
f(E, Startcell, [Data | Resten]) ->
	{R, C}= Startcell,
	Cell= "R"++integer_to_list(R)++"C"++integer_to_list(C),
	io:format(" ~p ~n ", [Cell]),
	f(E, {R+1, C}, Resten).

make_graph(E, Row1, Col1, Row2, Col2, Title) ->
	Charts = erl_com:package_interface(E, erl_com:property_get(E, "Charts")),
	erl_com:invoke(Charts, "Add"),
	ActiveChart= erl_com:package_interface(E, erl_com:property_get(E, "ActiveChart")),
	erl_com:property_put(ActiveChart, "ChartType", {vt_i4, ?XlPieExploded}),
	erl_com:invoke(ActiveChart, "Location", [{vt_i4, ?xlLocationAsObject}, "Sheet1"]),
	Chart= erl_com:package_interface(E, erl_com:property_get(E, "ActiveChart")),
	R= to_cell_col(Col1)++integer_to_list(Row1)++":"
	 ++to_cell_col(Col2)++integer_to_list(Row2),
	io:format(" ~s ~n ", [R]),
	Range= erl_com:property_get(E, "Range", [R]),
	erl_com:invoke(Chart, "SetSourceData", [{vt_unknown, Range}, {vt_i4, ?xlColumns}]),
	erl_com:property_put(Chart, "HasTitle", true),
	ChartTitle= erl_com:package_interface(E, erl_com:property_get(Chart, "ChartTitle")),
	erl_com:property_put(ChartTitle, "Caption", Title).
	%erl_com:release(erl_com:package_interface(E, Range)),
	%erl_com:release(ActiveChart),
	%erl_com:release(Charts).

sample1() ->
	{ok, Pid}= erl_com:start_process(),
	T= erl_com:new_thread(Pid),
	E= erl_com:create_dispatch(T, "Excel.Application", ?CLSCTX_LOCAL_SERVER),
	erl_com:property_put(E, "Visible", true),
	Wb= erl_com:package_interface(T, erl_com:property_get(E, "Workbooks")),
	erl_com:invoke(Wb, "Add"),
	populate_area(E, 1, 1, ["Erlang", "Java", "C++"]),
	populate_area(E, 1, 2, ["25", "100", "250"]),
	make_graph(E, 1, 1, 3, 2, "Programfel i Ericssonprojekt, språkuppdelning"),
	{T, E, Wb}.
