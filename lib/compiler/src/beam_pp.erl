%%======================================================================
%% File        : beam_pp.erl
%% Author      : Kostis Sagonas
%% Description : Pretty-prints a list of symbolic BEAM instructions
%%======================================================================
%% $Id$
%%=======================================================================
%% Notes: Probably the output can be made prettier.
%%=======================================================================

-module(beam_pp).

-export([pp/1,pp/2]).

%%======================================================================

pp(Code) ->
    pp(standard_io,Code).

pp(Stream, []) ->
    case Stream of  %% I am not sure whether this is necessary
	standard_io -> ok;
	_ -> file:close(Stream)
    end;
pp(Stream, [FunCode|FunCodes]) ->
    pp_mfa(Stream,FunCode),
    put_nl(Stream),
    pp(Stream,FunCodes).

pp_mfa(Stream, FunCode) ->
    lists:foreach(fun(Instr) -> print_instr(Stream, Instr) end, FunCode).

print_instr(Stream, Label) when element(1,Label) == label ->
    io:format(Stream, "  label ~p:\n", [element(2,Label)]);
print_instr(Stream, Op) ->
    io:format(Stream, "    ~p\n", [Op]).

put_nl(Stream) ->
    io:format(Stream,"\n",[]).

%%======================================================================
