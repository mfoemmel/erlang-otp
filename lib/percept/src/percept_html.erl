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

-module(percept_html).
-export([
	page/3, 
	codelocation_page/3, 
	databases_page/3, 
	load_database_page/3, 
	processes_page/3, 
	concurrency_page/3,
	process_info_page/3
	]).
-export([
	value2pid/1, 
	pid2value/1, 
	get_option_value/2,
	join_strings_with/2
	]).

-include("percept.hrl").
-include_lib("kernel/include/file.hrl").


%% API

page(SessionID, Env, Input) ->
    mod_esi:deliver(SessionID, header()),
    mod_esi:deliver(SessionID, menu()), 
    mod_esi:deliver(SessionID, overview_content(Env, Input)),
    mod_esi:deliver(SessionID, footer()).

processes_page(SessionID, _, _) ->
    mod_esi:deliver(SessionID, header()),
    mod_esi:deliver(SessionID, menu()), 
    mod_esi:deliver(SessionID, processes_content()),
    mod_esi:deliver(SessionID, footer()).

concurrency_page(SessionID, Env, Input) ->
    mod_esi:deliver(SessionID, header()),
    mod_esi:deliver(SessionID, menu()), 
    mod_esi:deliver(SessionID, concurrency_content(Env, Input)),
    mod_esi:deliver(SessionID, footer()).

databases_page(SessionID, _, _) ->
    mod_esi:deliver(SessionID, header()),
    mod_esi:deliver(SessionID, menu()), 
    mod_esi:deliver(SessionID, databases_content()),
    mod_esi:deliver(SessionID, footer()).
    
codelocation_page(SessionID, Env, Input) ->
    mod_esi:deliver(SessionID, header()),
    mod_esi:deliver(SessionID, menu()), 
    mod_esi:deliver(SessionID, codelocation_content(Env, Input)),
    mod_esi:deliver(SessionID, footer()).

process_info_page(SessionID, Env, Input) ->
    mod_esi:deliver(SessionID, header()),
    mod_esi:deliver(SessionID, menu()), 
    mod_esi:deliver(SessionID, process_info_content(Env, Input)),
    mod_esi:deliver(SessionID, footer()).

load_database_page(SessionID, Env, Input) ->
    mod_esi:deliver(SessionID, header()),

    % Very dynamic page, handled differently
    load_database_content(SessionID, Env, Input),
    mod_esi:deliver(SessionID, footer()).

%%% --------------------------- %%%
%%% 	Content pages		%%%
%%% --------------------------- %%%

overview_content(_Env, Input) ->
    Query = httpd:parse_query(Input),
    Min = get_option_value("range_min", Query),
    Max = get_option_value("range_max", Query),
    Width = 700,
    Height = 400,
    TotalProfileTime = ?seconds( percept_db:select({system, stop_ts}), 
    				 percept_db:select({system, start_ts})),
    RegisteredProcesses = length(percept_db:select({information, procs})),
    RegisteredPorts = length(percept_db:select({information, ports})), 
    
    InformationTable = 
	"<table>" ++
	table_line(["Profile time:", TotalProfileTime]) ++
	table_line(["Processes:", RegisteredProcesses]) ++
	table_line(["Ports:", RegisteredPorts]) ++
%    	table_line(["Active processes:", 0]) ++
%    	table_line(["Average concurrency:", 0]) ++ 
    	table_line(["Min. range:", Min]) ++
    	table_line(["Max. range:", Max]) ++
    	"</table>",
    
    Header = "
    <div id=\"percept_right\">" ++ InformationTable ++ "</div>\n
    <div id=\"percept_middle\"><center>
    <form name=form_area method=POST action=/cgi-bin/percept_html/page>
    <input name=data_min type=hidden value=" ++ term2html(float(Min)) ++ ">
    <input name=data_max type=hidden value=" ++ term2html(float(Max)) ++ ">\n",

    
    RangeTable = 
	"<table>"++
	table_line([
	    "Min:", 
	    "<input name=range_min value=" ++ term2html(float(Min)) ++">",
	    "<select name=\"graph_select\" onChange=\"select_image()\">
	    	<option disabled=true value=\""++ url_graph(Width, Height, Min, Max, []) ++"\" />Ports
	    	<option disabled=true value=\""++ url_graph(Width, Height, Min, Max, []) ++"\" />Processes
	    	<option value=\""++ url_graph(Width, Height, Min, Max, []) ++"\" />Ports & Processes
	    </select>",
	    "<input type=submit value=Update>" 
	    ]) ++
	table_line([
	    "Max:", 
	    "<input name=range_max value=" ++ term2html(float(Max)) ++">",
	    "",
	    "<a href=/cgi-bin/percept_html/codelocation_page?range_min=" ++ term2html(Min) ++ "&range_max=" ++ term2html(Max) ++ ">Code location</a>"
	    ]) ++
    	"</table>",
    
    MainTable = 
	"<table>" ++
	table_line([div_tag_graph(Width, Height, Min, Max, [])]) ++
	table_line([RangeTable]) ++
	"</table>",

    Footer = "</center></div></form>",
    
    Header ++ MainTable ++ Footer.

div_tag_graph(Width, Height, Min, Max, Pids) ->
    UrlGraph = url_graph(Width, Height, Min, Max, Pids),
    "
    <div id=\"percept_graph\" 
	onMouseDown=\"select_down(event)\" 
	onMouseMove=\"select_move(event)\" 
	onMouseUp=\"select_up(event)\"

	style=\"
	background-image:url('" ++ UrlGraph ++ "');
	background-size: 100%;
	background-origin: content;
	width:" ++ term2html(Width) ++ "px;
	height:"++ term2html(Height) ++ "px;
	position:relative;
	\">
	
	<div id=\"percept_areaselect\"
	style=\"background-color:#ef0909;
	position:relative;
	visibility:hidden;
	z-index:2;
	width:40px;
	height:40px;\"></div></div>".

-spec(url_graph/5 :: (
	Widht :: non_neg_integer(),
	Height :: non_neg_integer(),
	Min :: float(),
	Max :: float(),
	Pids :: [pid()]) -> string()).

url_graph(W, H, Min, Max, []) ->
    "/cgi-bin/percept_graph/graph?range_min=" ++ term2html(float(Min)) 
    	++ "&range_max=" ++ term2html(float(Max))
	++ "&width=" ++ term2html(float(W))
	++ "&height=" ++ term2html(float(H)).
%url_graph(W, H, Min, Max, Pids) ->
%    PidValues = [pid2value(Pid) || Pid <- Pids],
%    PidRequests = join_strings_with(PidValues, ":"),
%    "/cgi-bin/percept_graph/graph?range_min=" ++ term2html(float(Min)) 
%    	++ "&range_max=" ++ term2html(float(Max))
%	++ "&width=" ++ term2html(float(W))
%	++ "&height=" ++ term2html(float(H))
%	++ "&pids=" ++ PidRequests.

%%% process_info_content

process_info_content(_Env, Input) ->
    Query = httpd:parse_query(Input),
    Pid = get_option_value("pid", Query),
   
 
    [I] = percept_db:select({information, Pid}),
    case I#information.entry of
    	{_,_,Arguments} ->
	    ArgumentString = lists:flatten( [term2html(Arg) ++ "<br>" || Arg <- Arguments]);
	_ -> 
	    ArgumentString = ""
    end,

    TimeTable = html_table([
	["", "Timestamp", "Profile Time"],
	["Start",
		term2html(I#information.start),
		term2html(procstarttime(I#information.start))],
	["Stop",
		term2html(I#information.stop),
		term2html(procstoptime(I#information.stop))]
	]),   

    InfoTable = "<table>" ++
    table_line(["<b>Pid</b>", I#information.id, ""]) ++
    table_line(["<b>Name</b>", I#information.name, ""]) ++
    table_line(["<b>Entrypoint</b>", mfa2html(I#information.entry), ""]) ++
    table_line(["<b>Arguments</b>", ArgumentString, ""]) ++ 
    table_line(["<b>Timetable</b>", TimeTable, ""]) ++
    table_line(["Parent", pid2html(I#information.parent), ""]) ++
    table_line(["Children", term2html(I#information.children), ""]) ++
    "</table>",
    
    PidActivities = percept_db:select({activity, [{id, Pid}]}),
    WaitingMfas = percept_analyzer:waiting_activities(PidActivities),
    
    TotalWaitTime = lists:foldl(
    	fun ({Time, _, _}, Out) ->
	    Out + Time
	end, 0, WaitingMfas),


    WaitingMfaTable = "<table>" ++
    table_line([	"<b>Percentage</b>", 
			"<b>Total</b>", 
			"<b>Mean</b>",
			"<b>StdDev</b>", 
			"<b>#recv</b>", 
			"<b>Module:Function/Arity</b>\n"]) ++
    lists:flatten([
    	table_line([
	    image_string(percentage, [
		{width, 100},
		{height, 10},
		{percentage, Time/TotalWaitTime}]),
	    term2html(Time), 
	    term2html(Mean),
	    term2html(StdDev),
	    term2html(N), 
	    mfa2html(MFA)
	]) || {Time, MFA, {Mean, StdDev, N}} <- WaitingMfas]) ++
    "</table>",
   
    "<div id=\"percept_content\">" ++
    InfoTable ++ "<br>" ++
    WaitingMfaTable ++
    "</div>".

%%% concurrency content

concurrency_content(_Env, Input) ->
    %% Get query
    Query = httpd:parse_query(Input),
    
    %% Collect selected pids and generate id tags
    Pids = [value2pid(PidValue) || {PidValue, Case} <- Query, 
    	Case == "on", 
	PidValue /= "select_all"],
    
    IDs = [{id, Pid} || Pid <- Pids],

    % FIXME: A lot of extra work here, redo

    %% Analyze activities and calculate area bounds
    Activities = percept_db:select({activity, IDs}),
    StartTs = percept_db:select({system, start_ts}), 
    Counts = [{Time, Y1 + Y2} 
    	|| {Time, Y1, Y2} <- percept_analyzer:activities2count(Activities, StartTs)],
    {T0,_,T1,_} = percept_analyzer:minmax(Counts),

    % FIXME: End
    
    PidValues = [pid2value(Pid) || Pid <- Pids],

    %% Generate activity bar requests
    ActivityBarTable = lists:foldl(
    	fun(Pid, Out) ->
	    ValueString = pid2value(Pid),
	    Out ++ 
	    	table_line([
	    	    pid2html(Pid),
		    image_string(activity, [
			{"pid", ValueString}, 
			{min, T0},
			{max, T1},
			{height, 10}])
		])
	end, [], Pids),

    %% Make pids request string
    PidsRequest = join_strings_with(PidValues, ":"),

    "<div id=\"percept_content\">
    <table cellspacing=0 cellpadding=0 border=0>" ++
    table_line([
    	"",
	image_string(graph, [
	    {range_min, T0},
	    {range_max, T1},
	    {"pids", PidsRequest}])
    ]) ++
    ActivityBarTable ++
    "</table>
    </div>\n".

processes_content() ->
    Ports = percept_db:select({information, ports}),
    UnsortedProcesses = percept_db:select({information, procs}),
    SystemStartTS = percept_db:select({system, start_ts}),
    SystemStopTS = percept_db:select({system, stop_ts}),
    ProfileTime = ?seconds(	SystemStopTS, 
				SystemStartTS),
    Processes = lists:sort(
    	fun (A, B) ->
	    if
	    	A#information.id > B#information.id -> true;
		true -> false
	    end
	end, UnsortedProcesses),
    
    ProcsHtml = lists:foldl(
    	fun (I, Out) ->
	    StartTime = procstarttime(I#information.start),
	    EndTime = procstoptime(I#information.stop),
	    Prepare = 
	    	table_line([
		    "<input type=checkbox name=" ++ pid2value(I#information.id) ++ ">",
		    pid2html(I#information.id),
		    image_string(proc_lifetime, [
			{profiletime, ProfileTime},
			{start, StartTime},
			{"end", term2html(float(EndTime))},
			{width, 100},
			{height, 10}]),
	    	    mfa2html(I#information.entry),
		    term2html(I#information.name),
		    pid2html(I#information.parent)
		    ]),
	    [Prepare|Out]    
	end, [], Processes),

    PortsHtml = lists:foldl(
    	fun (I, Out) ->
	    StartTime = procstarttime(I#information.start),
	    EndTime = procstoptime(I#information.stop),
	    Prepare = 
	    	table_line([
		    "",
		    pid2html(I#information.id),
		    image_string(proc_lifetime, [
			{profiletime, ProfileTime},
			{start, StartTime},
			{"end", term2html(float(EndTime))},
			{width, 100},
			{height, 10}]),
		    mfa2html(I#information.entry),
		    term2html(I#information.name),
		    pid2html(I#information.parent)
		]),
		[Prepare|Out]
	end, [], Ports),

    Selector = "<table>" ++
	table_line([
	    "<input onClick='selectall()' type=checkbox name=select_all>Select all"]) ++
	table_line([
	    "<input type=submit value=Compare>"]) ++
    	"</table>",

    if 
	length(ProcsHtml) > 0 ->
	    ProcsHtmlResult = 
	    "<tr><td><b>Processes</b></td></tr>
	    <tr><td>
 	   <table width=700 cellspacing=0 border=0>
		<tr>
		<td align=middle width=40><b>Select</b></td>
		<td align=middle width=40><b>Pid</b></td>
		<td><b>Lifetime</b></td>
		<td><b>Entrypoint</b></td>
		<td><b>Name</b></td>
		<td><b>Parent</b></td>
		</tr>" ++
		lists:flatten(ProcsHtml) ++ 
	    "</table>
	    </td></tr>";
	true ->
	    ProcsHtmlResult = ""
    end,
    if 
	length(PortsHtml) > 0 ->
    	    PortsHtmlResult = " 
	    <tr><td><b>Ports</b></td></tr>
	    <tr><td>
	    	<table width=700 cellspacing=0 border=0>
		<tr>
		<td align=middle width=40><b>Select</b></td>
		<td align=left width=40><b>Pid</b></td>
		<td><b>Lifetime</b></td>
		<td><b>Entrypoint</b></td>
		<td><b>Name</b></td>
		<td><b>Parent</b></td>
		</tr>" ++
		lists:flatten(PortsHtml) ++
		"</table>
	    </td></tr>";
	true ->
	    PortsHtmlResult = ""
     end,


    Middle = "<div id=\"percept_middle\">
    <table>" ++
    ProcsHtmlResult ++
    PortsHtmlResult ++ 
    "</table>
    </div>\n",
    
    Right = "<div id=\"percept_right\">"
    ++ Selector ++ 
    "</div>\n",
    
    "<form name=process_select method=POST action=/cgi-bin/percept_html/concurrency_page>" ++
    Right ++ Middle ++ "</form>".

procstarttime(TS) ->
    case TS of
    	undefined -> 0.0;
	TS -> ?seconds(TS,percept_db:select({system, start_ts}))
    end.

procstoptime(TS) ->
    case TS of
    	undefined -> ?seconds(	percept_db:select({system, stop_ts}), 
				percept_db:select({system, start_ts}));
	TS -> ?seconds(TS, percept_db:select({system, start_ts}))
    end.

databases_content() ->
    "<div id=\"percept_content\">
	<form name=load_percept_file method=post action=/cgi-bin/percept_html/load_database_page>
	<center>
	<table>
	    <tr><td>Enter file to analyse:</td><td><input type=hidden name=path /></td></tr>
	    <tr><td><input type=file name=file size=40 /></td><td><input type=submit value=Load onClick=\"path.value = file.value;\" /></td></tr>
	</table>
	</center>
	</form>
	</div>".

load_database_content(SessionId, _Env, Input) ->
    Query = httpd:parse_query(Input),
    {_,{_,Path}} = lists:keysearch("file", 1, Query),
    {_,{_,File}} = lists:keysearch("path", 1, Query),
    Filename = filename:join(Path, File),
    % Check path/file/filename
    
    mod_esi:deliver(SessionId, "<div id=\"percept_middle\">"), 
    case file:read_file_info(Filename) of
	{ok, _} ->
    	    Content = "<center>
    	    Parsing: " ++ Filename ++ "<br>
    	    </center>",
	    mod_esi:deliver(SessionId, Content),
	    case percept:analyze(Filename) of
		{error, Reason} ->
	    	    mod_esi:deliver(SessionId, error_msg("Analyze" ++ term2html(Reason)));
		_ ->
		    Complete = "<center><a href=\"/cgi-bin/percept_html/page\">View</a></center>",
	    	   mod_esi:deliver(SessionId, Complete)
	    end;
	{error, Reason} ->
	    mod_esi:deliver(SessionId, error_msg("File" ++ term2html(Reason)))
    end,
    mod_esi:deliver(SessionId, "</div>"). 

codelocation_content(_Env, Input) ->
    Query = httpd:parse_query(Input),
    Min = get_option_value("range_min", Query),
    Max = get_option_value("range_max", Query),
    StartTs = percept_db:select({system, start_ts}),
    TsMin = percept_analyzer:seconds2ts(Min, StartTs),
    TsMax = percept_analyzer:seconds2ts(Max, StartTs),
    Acts = percept_db:select({	activity, 
				[{ts_min, TsMin}, {ts_max, TsMax}]
				}),
    Act0 = hd(Acts),
    {_RcMin, RcMax} = percept_analyzer:minmax_activities(
	Acts,
	Act0#activity.runnable_count),

    "<div id=\"percept_content\">
    <table>
    <tr style=\"border-bottom:1px; \">
	<td>Time</td><td>Pid</td><td>Activty</td><td>module:function/arity</td>
    </tr>" ++ 
    lists:flatten(
	[ table_line([	term2html(?seconds(A#activity.timestamp,StartTs)),
			pid2html(A#activity.id),
			term2html(A#activity.state),
			mfa2html(A#activity.where),
			image_string(percentage, [
			    {percentage, A#activity.runnable_count/RcMax},
			    {height, 10},
			    {width, 200}])	
			]) || A <- Acts
	]) ++ "</table></div>\n".


%%% --------------------------- %%%
%%% 	Utility functions	%%%
%%% --------------------------- %%%

%% Should be in string stdlib?

join_strings(Strings) ->
    lists:flatten(Strings).

-spec(join_strings_with/2 :: (
	Strings :: [string()],
	Separator :: string()) ->
	string()).

join_strings_with([S1, S2 | R], S) ->
    join_strings_with([join_strings_with(S1,S2,S) | R], S);
join_strings_with([S], _) ->
    S.
join_strings_with(S1, S2, S) ->
    join_strings([S1,S,S2]).

%%% Generic erlang2html

-spec(html_table/1 :: (Table :: [[string()]]) -> string()).

html_table(Table) ->
    "<table>" ++ html_table_tr(Table) ++ "</table>".
html_table_tr([]) -> "";
html_table_tr([Tr|Table]) ->
    "<tr>" ++ html_table_td(Tr) ++ "</tr>" ++ html_table_tr(Table).
html_table_td([]) -> "";
html_table_td([Td|Tr]) ->
    "<td>" ++ Td ++ "</td>" ++ html_table_td(Tr).

-spec(table_line/1 :: (Table :: [any()]) -> string()).

table_line(List) -> table_line(List, ["<tr>"]).
table_line([], Out) -> lists:flatten(lists:reverse(["</tr>\n"|Out]));
table_line([Element | Elements], Out) when is_list(Element) ->
    table_line(Elements, ["<td>" ++ Element ++ "</td>" |Out]);
table_line([Element | Elements], Out) ->
    table_line(Elements, ["<td>" ++ term2html(Element) ++ "</td>"|Out]).

-spec(term2html/1 :: (any()) -> string()).

term2html(Term) when is_float(Term)->
   lists:flatten(io_lib:format("~f", [Term]));
term2html(Term) ->
   lists:flatten(io_lib:format("~p", [Term])).

-spec(mfa2html/1 :: (MFA :: {
	atom(), 
	atom(),
	list() | integer()}) ->
	string()). 

mfa2html({Module, Function, Arguments}) when is_list(Arguments) ->
    io_lib:format("~p:~p/~p", [Module, Function, length(Arguments)]);
mfa2html({Module, Function, Arity}) when is_integer(Arity) ->
    io_lib:format("~p:~p/~p", [Module, Function, Arity]);
mfa2html(_) ->
    "undefined".

-spec(pid2html/1 :: (Pid :: pid() | port()) -> string()).

pid2html(Pid) when is_pid(Pid) ->
    PidString = term2html(Pid),
    PidValue = pid2value(Pid),
    "<a href=\"/cgi-bin/percept_html/process_info_page?pid="++PidValue++"\">"++PidString++"</a>";
pid2html(Pid) when is_port(Pid) ->
    term2html(Pid);
pid2html(_) ->
    "undefined".

-spec(image_string/1 :: (Request :: string()) -> string()).

image_string(Request) ->
    "<img border=0 src=\"/cgi-bin/percept_graph/" ++
    Request ++ 
    " \">".

-spec(image_string/2 :: (atom() | string(), list()) -> string()).

image_string(Request, Options) when is_atom(Request), is_list(Options) ->
     image_string(erlang:atom_to_list(Request), Options, []);
image_string(Request, Options) when is_list(Options) ->
     image_string(Request, Options, []).

%image_string(Request, [], Out) ->
%    image_string(join_strings([Request | lists:reverse(Out)]));
image_string(Request, [{Type, Value} | Opts], Out) when is_atom(Type), is_number(Value) ->
    Opt = join_strings(["?",term2html(Type),"=",term2html(Value)]),
    image_string0(Request, Opts, [Opt|Out]);
image_string(Request, [{Type, Value} | Opts], Out) ->
    Opt = join_strings(["?",Type,"=",Value]),
    image_string0(Request, Opts, [Opt|Out]).
image_string0(Request, [], Out) ->
    image_string(join_strings([Request | lists:reverse(Out)]));
image_string0(Request, [{Type, Value} | Opts], Out) when is_atom(Type), is_number(Value) ->
    Opt = join_strings(["&",term2html(Type),"=",term2html(Value)]),
    image_string0(Request, Opts, [Opt|Out]);
image_string0(Request, [{Type, Value} | Opts], Out) ->
    Opt = join_strings(["&",Type,"=",Value]),
    image_string0(Request, Opts, [Opt|Out]).
        

%%% percept conversions

-spec(pid2value/1 :: (Pid :: pid()) -> string()).

pid2value(Pid) ->
    String = lists:flatten(io_lib:format("~p", [Pid])),
    lists:sublist(String, 2, erlang:length(String)-2).

-spec(value2pid/1 :: (Value :: string()) -> pid()).

value2pid(Value) ->
   String = lists:flatten("<" ++ Value ++ ">"),
   erlang:list_to_pid(String).


%%% get value

-spec(get_option_value/2 :: (
	Option :: string(),
	Options :: [{string(),any()}]) ->
	{'error', any()} | bool() | pid() | [pid()] | number()).

get_option_value(Option, Options) ->
    case catch get_option_value0(Option, Options) of
	{'EXIT', Reason} -> {error, Reason};
	Value -> Value
    end.

get_option_value0(Option, Options) ->
     case lists:keysearch(Option, 1, Options) of
    	false -> get_default_option_value(Option);
	{value, {Option, _Value}} when Option == "fillcolor" -> true;
	{value, {Option, Value}} when Option == "pid" -> value2pid(Value);
	{value, {Option, Value}} when Option == "pids" -> 
	    [value2pid(PidValue) || PidValue <- string:tokens(Value,":")];
	{value, {Option, Value}} -> get_number_value(Value);
	_ -> {error, undefined}
    end.

get_default_option_value(Option) ->
    case Option of 
    	"fillcolor" -> false;
	"range_min" -> float(0.0);
	"pids" -> [];
	"range_max" ->
	    Acts = percept_db:select({activity, []}),
	    #activity{ timestamp = Start } = hd(Acts),
	    #activity{ timestamp = Stop } = hd(lists:reverse(Acts)),
 
%	    Start = percept_db:select({system, start_ts}),
%	    Stop = percept_db:select({system, stop_ts}),
	    ?seconds(Stop,Start);
	"width" -> 700;
	"height" -> 400;
	_ -> {error, {undefined_default_option, Option}}
    end.

-spec(get_number_value/1 :: (Value :: string()) ->
	number() | {'error', 'illegal_number'}).

get_number_value(Value) ->
    % Try float
    case string:to_float(Value) of
    	{error, no_float} ->
	    % Try integer
	    case string:to_integer(Value) of
		{error, _} -> {error, illegal_number};
		{Integer, _} -> Integer
	    end;
	{error, _} -> {error, illegal_number};
	{Float, _} -> Float
    end.

%%% --------------------------- %%%
%%% 	html prime functions	%%%
%%% --------------------------- %%%

header() ->
%    "Expires: 0\r\n" ++
 %   "Cache-Control: max-age=0, no-store, no-cache, must-revalidate\r\n" ++
  %  "Pragma: no-cache\r\n" ++
    "Content-Type: text/html\r\n\r\n" ++
    "<html>
    <head>
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">
    <title>.percept.</title>
    <link href=\"/css/percept.css\" rel=\"stylesheet\" type=\"text/css\">
    <script type=\"text/javascript\" src=\"/javascript/percept_error_handler.js\"></script>
    <script type=\"text/javascript\" src=\"/javascript/percept_select_all.js\"></script>
    <script type=\"text/javascript\" src=\"/javascript/percept_area_select.js\"></script>
    </head>
    <body>
    <div id=\"percept_top\"><a href=/index.html><h1>.percept.</h1></a></div>
    \n".

footer() ->
     %<div id=\"percept_bottom\"></div>
    "</body>
     </html>\n".

menu() ->
    "<div id=\"percept_navigation\">
	<ul>
	<h2>Menu</h2>
     	<li><a href=/cgi-bin/percept_html/page>overview</a></li>
     	<li><a href=/cgi-bin/percept_html/processes_page>processes</a></li>
     	<li><a href=/cgi-bin/percept_html/databases_page>databases</a></li>
     </ul></div>\n".

-spec(error_msg/1 :: (Error :: string()) -> string()).

error_msg(Error) ->
    "<table bgcolor=#787878 width=300 border=0 cellspacing=0 cellpadding=0>
	<tr height=5><td></td> <td></td></tr>
	<tr><td width=150 align=right><b>Error: </b></td> <td align=left>"++ Error ++ "</td></tr>
	<tr height=5><td></td> <td></td></tr>
     </table>\n".
