%% an example of using COM with generated code
%%
%% the code was generated with these commands:
%%    erl_com:get_program(a),
%%    erl_com:gen_typelib({a, "c:\\program files\\microsoft office\\office\\mso97.dll"}),
%%    erl_com:gen_typelib({a, "c:\\program files\\microsoft office\\office\\excel8.olb"}, dispatch),
%%    erl_com:gen_interface({a, "c:\\program files\\microsoft office\\office\\excel8.olb"},
%%                          "_Application", dispatch, [{also_prefix_these, ["application"]}, {prefix, "x"}]),

-module(xc_gen).
-author('jakob@erix.ericsson.se').

-include("erl_com.hrl").
-include("xlChartType.hrl").
-include("xlChartLocation.hrl").
-include("xlRowCol.hrl").

-compile(export_all).

to_cell_col(C) when C > 26 ->
    [C / 26 + 64, C rem 26 + 64];
to_cell_col(C) ->
    [C+64].

populate_area(E, _, _, []) ->
    ok;
populate_area(E, Row, Col, [Data | Resten]) ->
    Cell= to_cell_col(Col)++integer_to_list(Row),
    Range= xapplication:range(E, Cell),
    range:value(Range, Data),
    erl_com:release(Range),
    populate_area(E, Row+1, Col, Resten).

make_graph(E, Row1, Col1, Row2, Col2, Title) ->
    Charts= xapplication:charts(E),
    NewChart= charts:add(Charts),
    erl_com:release(Charts),
    0= chart:chartType(NewChart, ?xlPieExploded),
    Chart= chart:location(NewChart, ?xlLocationAsObject, "Sheet1"),
    erl_com:release(NewChart),
    R= to_cell_col(Col1)++integer_to_list(Row1)++":"
	++to_cell_col(Col2)++integer_to_list(Row2),
    Range= xapplication:range(E, R),
    []= chart:setSourceData(Chart, Range, ?xlColumns),
    0= chart:hasTitle(Chart, true),
    ChartTitle= chart:chartTitle(Chart),
    0= chartTitle:caption(ChartTitle, Title),
    erl_com:release(Range),
    erl_com:release(Chart),
    erl_com:release(ChartTitle),
    ok.

sample1() ->
    {ok, _Pid}= erl_com:get_program(xc_gen),
    E= erl_com:create_dispatch(xc_gen, "Excel.Application", ?CLSCTX_LOCAL_SERVER),
    0= xapplication:visible(E, true),
    Wb= xapplication:workbooks(E),
    W= workbooks:add(Wb),
    erl_com:release(W),
    erl_com:release(Wb),
    populate_area(E, 1, 1, ["Erlang", "Java", "C++"]),
    populate_area(E, 1, 2, ["25", "100", "250"]),
    ok= make_graph(E, 1, 1, 3, 2, "Bugs in source code, by language"),
    E.
