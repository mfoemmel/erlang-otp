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
-module(snmpc_misc).

%% need definition of mib record
-include("snmp_types.hrl").
-include("snmpc_misc.hrl").


-export([assq/2,
	 bits_to_int/2,
	 ensure_trailing_dir_delimiter/1,
	 foreach/3,
	 is_string/1,
	 map/3,
	 read_mib/1,
	 read_noexit/2,
	 strip_extension_from_filename/2,
	 to_upper/1]).


%%--------------------------------------------------
%% Not a real assq, but what the heck, it's useful.
%%--------------------------------------------------
assq(Key, List) ->
    case lists:keysearch(Key, 1, List) of
	{value, {Key, Val}} -> {value, Val};
	_ -> false
    end.


%%----------------------------------------------------------------------
%% Converts a list of named bits to the integer value.
%% Returns: integer()|error
%%----------------------------------------------------------------------
bits_to_int(Val,Kibbles) ->
    bits_to_int(Val,Kibbles,0).

bits_to_int([],_Kibbles,Res) -> Res;
bits_to_int([Kibble|Ks],Kibbles,Res) ->
    case snmp_misc:assq(Kibble,Kibbles) of
	{value,V} ->
	    bits_to_int(Ks,Kibbles,Res + round(math:pow(2,V)));
	_ ->
	    error
    end.


ensure_trailing_dir_delimiter([]) -> "/";
ensure_trailing_dir_delimiter(DirSuggestion) ->
    case lists:last(DirSuggestion) of
	$/ -> DirSuggestion;
	_ -> lists:append(DirSuggestion,"/")
    end.


strip_extension_from_filename(FileName, Ext) when atom(FileName) ->
    strip_extension_from_filename(atom_to_list(FileName), Ext);

strip_extension_from_filename(FileName, Ext) when list(FileName) ->
    case lists:suffix(Ext, FileName) of
	true -> lists:sublist(FileName, 1, length(FileName) - length(Ext));
	false -> FileName
    end.


to_upper([C|Cs]) when C >= $a, C =< $z -> [C-($a-$A)|to_upper(Cs)];
to_upper([C|Cs]) -> [C|to_upper(Cs)];
to_upper([]) -> [].


is_string([]) -> true;
is_string([Tkn | Str]) when integer(Tkn), Tkn >= 0, Tkn =< 255 ->
    is_string(Str);
is_string(_) -> false.


foreach(Function, ExtraArgs, [H | T]) ->
    apply(Function, [H | ExtraArgs]),
    foreach(Function, ExtraArgs, T);
foreach(_Function, _ExtraArgs, []) -> true.


map(F, Eas, List) -> [ apply(F, [E|Eas]) || E <- List ].



%%----------------------------------------------------------------------
%% Returns: {ok, Mib}|{error, Reason}
%% The reason for having the function if this module is:
%% The compiler package and the agent package are separated, this is
%% the only common module.
%%----------------------------------------------------------------------
read_mib(FileName) ->
    (catch do_read_mib(FileName)).

do_read_mib(FileName) ->
    ?read_mib(FileName).


%% Ret. {ok, Res} | {error, Line, Error} | {error, open_file}
read_noexit(File, CheckFunc) ->
    case file:open(File, read) of
	{ok, Fd} ->
	    case loop(Fd, [], CheckFunc, 1, File) of
		{error, Line, R} ->
		    file:close(Fd),
		    {error, Line, R};
		Res ->
		    file:close(Fd),
		    {ok, Res}
	    end;
	_Error ->
	    {error, open_file}
    end.


%%-----------------------------------------------------------------
%% Ret: {error, Line, Reason} | Row
%%-----------------------------------------------------------------
loop(Fd, Res, Func, StartLine, File) ->
    case read(Fd, "", StartLine) of
	{ok, Row, EndLine} ->
	    case (catch apply(Func, [Row])) of
		{ok, NewRow} ->
		    loop(Fd, [NewRow | Res], Func, EndLine, File);
		true -> 
		    loop(Fd, [Row | Res], Func, EndLine, File);
		Error ->
		    {error, EndLine, Error}
	    end;
	{error, EndLine, Error} ->
	    {error, EndLine, Error};
	eof ->
	    Res
    end.


%%-----------------------------------------------------------------
%% io:read modified to give us line numbers.
%%-----------------------------------------------------------------
read(Io, Prompt, StartLine) ->
    case io:request(Io, {get_until, Prompt, erl_scan, tokens, [StartLine]}) of
	{ok, Toks, EndLine} ->
	    case erl_parse:parse_term(Toks) of
		{ok, Term} ->
		    {ok, Term, EndLine};
		{error, {Line, erl_parse, Error}} ->
		    {error, Line, {parse_error, Error}}
	    end;
	{error, E, EndLine} ->
	    {error, EndLine, E};
	{eof, _EndLine} ->
	    eof;
	Other ->
	    Other
    end.

