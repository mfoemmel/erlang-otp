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
%% Purpose : Finishes the code generation for binary matching.

-module(beam_bs).

-export([module/2]).

-import(lists, [reverse/1,keysearch/3,keydelete/3]).

module({Mod,Exp,Attr,Forms0,Lbl}, Opts) ->
    Forms = [function(F) || F <- Forms0],
    {ok,{Mod,Exp,Attr,Forms,Lbl}}.

function({function,Name,Arity,CLabel,Asm0}) ->
    Dict = needed(Asm0, 0, []),
    Asm = replace(Asm0, Dict, []),
    {function,Name,Arity,CLabel,Asm}.

needed([{label,Lbl},{bs_restore,Name}|T], N, Dict0) ->
    case keysearch(Name, 1, Dict0) of
	{value,{Name,_}} -> needed(T, N, Dict0);
	false -> needed(T, N+1, [{Name,N}|Dict0])
    end;
needed([H|T], N, Dict) -> needed(T, N, Dict);
needed([], N, Dict) -> Dict.

replace([{bs_save,Name}=Save,{bs_restore,Name}|T], Dict, Acc) ->
    replace([Save|T], Dict, Acc);
replace([{bs_save,Name}|[{test,bs_test_tail,_,_}|_]=T], Dict, Acc) ->
    replace(T, keydelete(Name, 1, Dict), Acc);
replace([{bs_save,Name}|T], Dict, Acc) ->
    case keysearch(Name, 1, Dict) of
	{value,{Name,N}} ->
	    replace(T, Dict, [{bs_save,N}|Acc]);
	false ->
	    replace(T, Dict, Acc)
    end;
replace([{bs_restore,Name}|T], Dict, Acc) ->
    case keysearch(Name, 1, Dict) of
	{value,{Name,N}} ->
	    replace(T, Dict, [{bs_restore,N}|Acc]);
	false ->
	    replace(T, Dict, Acc)
    end;
replace([{bs_put_integer,_,{integer,_},_,Fl0,{integer,_}}=H|T]=List, Dict, Acc) ->
    case collect_string(List, [], 0) of
	{Len,Str,Rest} when Len >= 2 ->
	    replace(Rest, Dict, [{bs_put_string,Len,{string,Str}}|Acc]);
	_ ->
	    replace(T, Dict, [H|Acc])
    end;
replace([{test,bs_test_tail,F,[Bits]}|T], Dict,
	[{test,bs_skip_bits,F,[{integer,I},Unit,Flags]}|Acc]) ->
    replace(T, Dict, [{test,bs_test_tail,F,[Bits+I*Unit]}|Acc]);
replace([{test,bs_skip_bits,F,[{integer,I1},Unit1,_]}|T], Dict,
	[{test,bs_skip_bits,F,[{integer,I2},Unit2,Flags]}|Acc]) ->
    replace(T, Dict, [{test,bs_skip_bits,F,[{integer,I1*Unit1+I2*Unit2},1,Flags]}|Acc]);
replace([H|T], Dict, Acc) ->
    replace(T, Dict, [H|Acc]);
replace([], Dict, Acc) -> reverse(Acc).

collect_string([{bs_put_integer,_,{integer,Sz},Unit,Flags,{integer,V}}|T]=List,
	       Acc, Len) when Sz*Unit =:= 8 ->
    Byte = V band 16#FF,
    collect_string(T, [Byte|Acc], Len+1);
collect_string(Rest, Acc, Len) -> {Len,reverse(Acc),Rest}.

    
