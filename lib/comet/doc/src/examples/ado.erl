-module(ado).
-author('jakob@erix.ericsson.se').

-compile(export_all).
%%-export([Function/Arity, ...]).

%% these are generated from ADO:
%% erl_com:create_object(Ado, "ADODB.Connection"), com_gen:gen_typelib(Ado).

-include("cursorlocationenum.hrl").
-include("cursortypeenum.hrl").
-include("locktypeenum.hrl").
-include("commandtypeenum.hrl").

select_sample() ->
    Sql= "select * from titles order by title",
    select_sample(Sql).

select_sample(Sql) ->
    {ok, Pid1} = erl_com:get_program(a),
    C= connection_class:create_object(a),
    %% Load the Driver and connect to the database.
    Strconn= "Provider=SQLOLEDB;Initial Catalog=pubs;"
	"Data Source=eomer;User Id=sa;Password=;",             
    connection:open(C, Strconn),
    %% do the select
    Rs= connection:execute(C, Sql),
    %% get Fields
    Fields= recordset:fields(Rs),
    N= fields:count(Fields),
    %% get names
    Fl= lists:map(fun(J) -> fields:item(Fields, J) end, lists:seq(0, N-1)),
    %% get each field
    Nl= lists:map(fun(F) -> field:name(F) end, Fl),
    %% read values
    Vl= read_all(Rs, Fl, recordset:eOF(Rs), [Nl]),
    erl_com:release(Fields),
    erl_com:release(Rs),
    erl_com:release(C),
    Vl.


read_row(Fl) ->
    lists:map(fun(F) -> field:value(F) end, Fl).

%% read all values
read_all(Rs, Fl, true, Acc) ->
    lists:reverse(Acc);
read_all(Rs, Fl, false, Acc0) ->
    Acc= [read_row(Fl) | Acc0],
    recordset:moveNext(Rs),
    %% limit to 100 records
    read_all(Rs, Fl, (length(Acc) > 100) or recordset:eOF(Rs), Acc).
    

map2_(F, [], _, Acc) ->
    Acc;
map2_(F, _, [], Acc) ->
    Acc;
map2_(F, [A0 | A], [B0 | B], Acc0) ->
    Acc= [F(A0, B0) | Acc0],
    map2_(F, A, B, Acc).

map2(F, A, B) ->
    lists:reverse(map2_(F, A, B, [])).

map3_(F, [], _, _, Acc) ->
    Acc;
map3_(F, _, [], _, Acc) ->
    Acc;
map3_(F, _, _, [], Acc) ->
    Acc;
map3_(F, [A0 | A], [B0 | B], [C0 | C], Acc0) ->
    Acc= [F(A0, B0, C0) | Acc0],
    map3_(F, A, B, C, Acc).

map3(F, A, B, C) ->
    lists:reverse(map3_(F, A, B, C, [])).


insert_sample() ->
    %% Start a new COM server. The application must already be started.
    {ok, Pid1} = erl_com:get_program(a),
    %% Load the Driver and connect to the database, make recordset directly
    Strconn= "Provider=SQLOLEDB;Initial Catalog=pubs;Data Source=eomer;User Id=sa;Password=;",
    Strsql= "select * from titles",
    Rs= recordset_class:create_object(a),
    recordset:open(Rs, Strsql, Strconn, ?adOpenForwardOnly, ?adLockOptimistic),
    %% Add a new row
    recordset:addNew(Rs),
    Fields= recordset:fields(Rs),
    N= fields:count(Fields),
    %% Get each field
    Fl= lists:map(fun(J) -> fields:item(Fields, J) end, lists:seq(0, N-1)),
    Nl= lists:map(fun(F) -> field:name(F) end, Fl),
    %% Fields: title_id, title, type, pub_id, price, advance, royalty, ytd_sales, notes, pubdate
    %% Have some nice values
    FVals = ["TC8789", "Kul med prolog?", "UNDECIDED   ", "1389", 8.99,
	     8000, 10, 2000, "Det är inte SÅ kul med Prolog.", 0],
    %% Set values of new row
    map3(fun(F, V, Na) ->
		 io:format("name ~s value ~p ~n", [Na, V]),
 		 []= field:value(F, V) end, Fl, FVals, Nl),
    %% "Commit" to the DB
    recordset:update(Rs).

delete_sample(Title_id) ->
    {ok, Pid1} = erl_com:get_program(a),
    %% Load the Driver and connect to the database, create recordset directly
    Strconn= "Provider=SQLOLEDB;Initial Catalog=pubs;Data Source=eomer;User Id=sa;Password=;",
    Strsql= "select * from titles", 
    Filter= "title_id='" ++ Title_id ++ "'",
    Rs= recordset_class:create_object(a),
    []= recordset:open(Rs, Strsql, Strconn, ?adOpenForwardOnly, ?adLockOptimistic),
    %% Set the filter, required for delete (I think?)
    []= recordset:filter(Rs, Filter),
    %% Delete
    []= recordset:delete(Rs),
    %% And "commit"
    []= recordset:update(Rs).
