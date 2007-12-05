%%<copyright>
%% <year>2005-2007</year>
%% <holder>Ericsson AB, All Rights Reserved</holder>
%%</copyright>
%%<legalnotice>
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
%% The Initial Developer of the Original Code is Ericsson AB.
%%</legalnotice>
%%

%%% Description: user interaction for SSH

-module(ssh_io).

-export([yes_no/1, read_password/1]).
-import(lists, [reverse/1]).


read_line(Prompt) when list(Prompt) ->
    io:get_line(list_to_atom(Prompt));
read_line(Prompt) when atom(Prompt) ->
    io:get_line(Prompt).

read_ln(Prompt) ->
    trim(read_line(Prompt)).

yes_no(Prompt) ->
    io:format("~s [y/n]?", [Prompt]),
    case read_ln('') of
	"y" -> yes;
	"n" -> no;
	"Y" -> yes;
	"N" -> no;
	_ ->
	    io:format("please answer y or n\n"),
	    yes_no(Prompt)
    end.


%% FIXME: no echo!
read_password(Prompt) ->
    case read_ln(Prompt) of
	"" ->
	    read_password(Prompt);
	Pass -> Pass
    end.

trim(Line) when list(Line) ->
    reverse(trim1(reverse(trim1(Line))));
trim(Other) -> Other.

trim1([$\s|Cs]) -> trim(Cs);
trim1([$\r|Cs]) -> trim(Cs);
trim1([$\n|Cs]) -> trim(Cs);
trim1([$\t|Cs]) -> trim(Cs);
trim1(Cs) -> Cs.
    


