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
-module(asn1ct).

%% Compile Time functions for ASN.1 (e.g ASN.1 compiler).

%%-compile(export_all).
%% Public exports
-export([compile/1, compile/2]).
-export([start/0, start/1, stop/0]).
-export([encode/2, encode/3, decode/3]).
-export([test/1, test/2, test/3, value/2]).
%% Application internal exports
-export([compile_asn/3,compile_asn1/3,compile_py/3,compile/3,value/1,vsn/0]).
-include("asn1_records.hrl").
-include_lib("stdlib/include/erl_compile.hrl").

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This is the interface to the compiler
%% 
%% 


compile(File) ->
    compile(File,[]).

compile(File,Options) when list(Options) ->
    Result = case filename:extension(File) of
		 [] ->
		     case file:read_file_info(lists:concat([File,".asn1"])) of
			 {ok,FileInfo} ->
			     (catch compile1(lists:concat([File,".asn1"]),Options));
			 _Error ->
			     case file:read_file_info(lists:concat([File,".asn"])) of
				 {ok,FileInfo} ->
				     (catch compile1(lists:concat([File,".asn"]),Options));
				 _Error ->
				     (catch compile1(lists:concat([File,".py"]),Options))
			     end
		     end;
		 _ ->
		     (catch compile1(File,Options))
	     end.

%compile1(File,Options) when list(Options) ->
%    io:format("Erlang ASN.1 version ~p compiling ~p ~n",[?vsn,File]),
%    Ext = filename:extension(File),
%    Base = filename:basename(File,Ext),
%    OutFile = outfile(Base,"",Options),
%    DbFile = outfile(Base,"asn1db",Options),
%    Includes = [I || {i,I} <- Options],
%    EncodingRule = get_rule(Options),
%    case asn1ct_tok:file(File) of
%	{error,Reason} ->
%	    io:format("~p~n",[Reason]),
%	    {error,Reason};
%        Tokens ->
%	    case asn1ct_pbegin:parse(Tokens) of
%		{error,Reason} ->
%		    {error,Reason};
%		{ok,M} ->
%		    start(["."|Includes]),
%		    asn1ct_check:storeindb(M),
%		    Module = asn1_db:dbget(M#module.name,'MODULE'),
%		    State = #state{mname=Module#module.name,module=Module,erule=EncodingRule},
%		    Check = asn1ct_check:check(State,Module#module.typeorval),
%		    case {Check,lists:member(abs,Options)} of
%			{_,true} ->
%			    pretty2(M#module.name,lists:concat([OutFile,".abs"]));
%			{ok,_} ->
%			    asn1_db:dbsave(DbFile,M#module.name),
%			    io:format("--~p--~n",[{generated,DbFile}]),
%			    debug_on(Options),
%			    Result = 
%				case EncodingRule of
%				    ber ->
%					asn1ct_gen_ber:pgen(OutFile,EncodingRule,M#module.name,Module#module.typeorval),
%					debug_off(Options),
%					erl_compile(OutFile,Options);
%				    _ ->
%					asn1ct_gen_per:pgen(OutFile,EncodingRule,M#module.name,Module#module.typeorval),
%					debug_off(Options),
%					erl_compile(OutFile,Options)
%				end;
%			{Err,_} ->
%			    Err

%			    %% case atom_to_list(M#module.name) of
%			    %%Base ->
%			    %% ok;
%			    %% _ ->
%			    %% io:format("Warning Different ASN.1 modulename (~p) and filename (~p)~n~n",[M#module.name,Base])
%			    %% end
%		    end
%	    end
%    end.

%parse([Htoks|Rtoks]) ->
%    Result = asn1ct_pbegin:parse(Htoks),
%    case parse1(Rtoks, [Result]) of
%	[{ok,M}|OkAndErrors] ->
%	    {Types,Errors} = findtypes_and_values(OkAndErrors,[],[]),
%	    case length(Errors) of
%		N when N > 0 ->
%		    {error,Errors};
%		0 ->
%		    {ok,M#module{typeorval=Types}}
%	    end;
%	OkAndErrors ->
%	    {_,Errors} = findtypes_and_values(OkAndErrors,[],[])
%    end.

%parse1([[{'END',Lno},{'$end',_}]], Errors) ->
%    lists:reverse(Errors);
%parse1([[{'END',Lno},{'$end',_}]|Rtoks], Errors) ->
%    lists:reverse([{error,{'tokens after','END'}}|Errors]);
%parse1([], Errors) ->
%    lists:reverse([{error,{missing,'END'}}|Errors]);
%parse1([Htoks|Rtoks], Errors) ->
%    Result = asn1ct_ptype:parse(Htoks),
%    parse1(Rtoks,[Result|Errors]).
    
%findtypes_and_values([{ok,H}|T],Tacc,Vacc) ->
%    findtypes_and_values(T,[H|Tacc],Vacc);
%findtypes_and_values([{error,H}|T],Tacc,Vacc) ->
%    findtypes_and_values(T,Tacc,[H|Vacc]);
%findtypes_and_values([],Tacc,Vacc) ->
%    {lists:reverse(Tacc),lists:reverse(Vacc)}.
    
compile1(File,Options) when list(Options) ->
    io:format("Erlang ASN.1 version ~p compiling ~p ~n",[?vsn,File]),
    io:format("Compiler Options: ~p~n",[Options]),
    Ext = filename:extension(File),
    Base = filename:basename(File,Ext),
    OutFile = outfile(Base,"",Options),
    DbFile = outfile(Base,"asn1db",Options),
    Includes = [I || {i,I} <- Options],
    EncodingRule = get_rule(Options),
    case asn1ct_tok:file(File) of
	{error,Reason} ->
	    io:format("~p~n",[Reason]),
	    {error,Reason};
        Tokens ->
	    case asn1ct_parser:parse(Tokens) of
		{error,{Line,Mod,[Message,Token]}} ->
		    io:format("~p ~p at line ~p~n",[Message,Token,Line]),
		    {error,{Line,[Message,Token]}};
		{ok,M} ->
		    cmp(M#module.name,File),
		    start(["."|Includes]),
		    case asn1ct_check:storeindb(M) of 
			ok   ->
			    Module = asn1_db:dbget(M#module.name,'MODULE'),
			    State = #state{mname=Module#module.name,module=Module,erule=EncodingRule},
			    Check = asn1ct_check:check(State,Module#module.typeorval),
			    case {Check,lists:member(abs,Options)} of
				{_,true} ->
				    pretty2(M#module.name,lists:concat([OutFile,".abs"]));
				{ok,_} ->
				    asn1_db:dbsave(DbFile,M#module.name),
				    io:format("--~p--~n",[{generated,DbFile}]),
				    debug_on(Options),
				    Result = 
					case EncodingRule of
					    ber ->
						asn1ct_gen_ber:pgen(OutFile,EncodingRule,M#module.name,Module#module.typeorval),
						debug_off(Options),
						
						erl_compile(OutFile,Options);
					    _ ->
						asn1ct_gen_per:pgen(OutFile,EncodingRule,M#module.name,Module#module.typeorval),
						debug_off(Options),
						erl_compile(OutFile,Options)
					end;
				{Err,false} ->
				    Err

			    end;
			Err ->
			    Err
		    end
	    end
    end.

get_rule(Options) ->
    case [Rule ||Rule <-[per,ber],
		 Opt <- Options,
		 Rule==Opt] of
	[Rule] ->
	    Rule;
	[Rule|T] ->
	    Rule;
	[] ->
	    ber
    end.

erl_compile(OutFile,Options) ->
    case lists:member(noobj,Options) of
	true ->
	    ok;
	_ ->
	    ErlOpt = case lists:keysearch(outdir, 1, Options) of
		{value, {outdir, Odir}} -> [{outdir,Odir}]; 
		_ -> []
	    end,
	    case c:c(OutFile,ErlOpt) of
		{ok,Module} ->
		    ok;
		_ ->
		    {error,'no_compilation'}
	    end
    end.

debug_on(Options) ->
    case lists:member(debug,Options) of
	true ->
	    put(asndebug,true);
	_ ->
	    true
    end,
    case lists:member(keyed_list,Options) of
	true ->
	    put(asn_keyed_list,true);
	_ ->
	    true
    end.
    

debug_off(Options) ->
    erase(asndebug),
    erase(asn_keyed_list).
    

outfile(Base, Ext, Opts) when atom(Ext) ->
    outfile(Base, atom_to_list(Ext), Opts);
outfile(Base, Ext, Opts) ->
    Obase = case lists:keysearch(outdir, 1, Opts) of
		{value, {outdir, Odir}} -> filename:join(Odir, Base);
		Other -> Base			% Not found or bad format
	    end,
    case Ext of
	[] ->
	    Obase;
	_ ->
	    Obase++"."++Ext
    end.

%% compile(AbsFileName, Options)
%%   Compile entry point for erl_compile.

compile_asn(File,OutFile,Options) ->
    compile(lists:concat([File,".asn"]),OutFile,Options).

compile_asn1(File,OutFile,Options) ->
    compile(lists:concat([File,".asn1"]),OutFile,Options).

compile_py(File,OutFile,Options) ->
    compile(lists:concat([File,".py"]),OutFile,Options).

compile(File, _OutFile, Options) ->
    case compile(File, make_erl_options(Options)) of
	compiled -> ok;
	Other -> Other
    end.

%% Converts generic compiler options to specific options.

make_erl_options(Opts) ->
    
    %% This way of extracting will work even if the record passed
    %% has more fields than known during compilation.
    
    Includes0 = Opts#options.includes,
    Defines = Opts#options.defines,
    Outdir = Opts#options.outdir,
    Warning = Opts#options.warning,
    Verbose = Opts#options.verbose,
    Specific = Opts#options.specific,
    Optimize = Opts#options.optimize,
    OutputType = Opts#options.output_type,
    Cwd = Opts#options.cwd,
    
    Includes = 
	case Opts#options.ilroot of
	    undefined ->
		Includes0;
	    Ilroot ->
		[Ilroot|Includes0]
	end,
    
    Options =
	case Verbose of
	    true ->  [verbose];
	    false -> []
	end ++
%%%	case Warning of
%%%	    0 -> [];
%%%	    _ -> [report_warnings]
%%%	end ++
	[] ++
	case Optimize of
	    0 -> [];
	    _ -> [fast]
	end ++
	lists:map(
	  fun ({Name, Value}) ->
		  {d, Name, Value};
	      (Name) ->
		  {d, Name}
	  end,
	  Defines) ++
	case OutputType of
	    undefined -> [ber]; % temporary default (ber when it's ready)
	    ber -> [ber];
	    per -> [per]
	end,
    
    Options++[report_errors, {cwd, Cwd}, {outdir, Outdir}|
	      lists:map(fun(Dir) -> {i, Dir} end, Includes)]++Specific.

pretty2(Module,AbsFile) ->
    start(),
    {ok,F} = file:open(AbsFile,write),
    M = asn1_db:dbget(Module,'MODULE'),
    {Types,Values,ParameterizedTypes} = M#module.typeorval,
    io:format(F,"%%%%%%%%%%%%%%%%%%% TYPES in ~p  %%%%%%%%%%%%%%%%%%%~n",[Module]),
    lists:foreach(fun(T)-> io:format(F,"~s\n",
				     [asn1ct_pretty_format:term(asn1_db:dbget(Module,T))])
		  end,Types),
    io:format(F,"%%%%%%%%%%%%%%%%%%% VALUES in ~p  %%%%%%%%%%%%%%%%%%%~n",[Module]),
    lists:foreach(fun(T)-> io:format(F,"~s\n",
				     [asn1ct_pretty_format:term(asn1_db:dbget(Module,T))])
		  end,Values),
    io:format(F,"%%%%%%%%%%%%%%%%%%% Parameterized Types in ~p  %%%%%%%%%%%%%%%%%%%~n",[Module]),
    lists:foreach(fun(T)-> io:format(F,"~s\n",
				     [asn1ct_pretty_format:term(asn1_db:dbget(Module,T))])
		  end,ParameterizedTypes).

start() ->
    Includes = ["."],
    start(Includes).


start(Includes) when list(Includes) ->
    asn1_db:dbstart(Includes).

stop() ->
    save(),
    asn1_db:stop_server(ns),
    asn1_db:stop_server(rand),
    stopped.

save() ->
    asn1_db:dbstop().

%%clear() ->
%%    asn1_db:dbclear().

encode(Module,Term) ->
    asn1rt:encode(Module,Term).

encode(Module,Type,Term) ->
    asn1rt:encode(Module,Type,Term).

decode(Module,Type,Bytes) ->
    asn1rt:decode(Module,Type,Bytes).


test(Module) ->
    start(),
    M = asn1_db:dbget(Module,'MODULE'),
    {Types,Values,Ptypes} = M#module.typeorval,
    test_each(Module,Types).

test_each(Module,[Type | Rest]) ->
    case test(Module,Type) of
	{ok,Result} ->
	    test_each(Module,Rest);
	Error ->
	    Error
    end;
test_each(Module,[]) ->
    ok.

test(Module,Type) ->
    io:format("~p:~p~n",[Module,Type]),
    case (catch value(Module,Type)) of 
	{ok,Val} -> 
	    test(Module,Type,Val);
	{'EXIT',Reason} -> 
	    {error,{asn1,{value,Reason}}}
    end.

test(Module,Type,Value) ->
    case catch encode(Module,Type,Value) of
	{ok,Bytes} ->
	    case decode(Module,Type,lists:flatten(Bytes)) of
		{ok,Value} -> {ok,{Module,Type,Value}};
		{ok,Res} -> {error,{asn1,{encode_decode_mismatch,{{Module,Type,Value},Res}}}};
		Error -> {error,{asn1,{{decode,{Module,Type,Value},Error}}}}
	    end;
	Error ->
	    {error,{asn1,{encode,{{Module,Type,Value},Error}}}}
    end.

value(Module) ->
    start(),
    M = asn1_db:dbget(Module,'MODULE'),
    {Types,Values,Ptypes} = M#module.typeorval,
    lists:map(fun(A) ->value(Module,A) end,Types).

value(Module,Type) ->
    start(),
    case catch asn1ct_value:get_type(Module,Type,no) of
	{error,Reason} ->
	    {error,Reason};
	{'EXIT',Reason} ->
	    {error,Reason};
	Result ->
	    {ok,Result}
    end.

cmp(Module,InFile) ->
    Base = filename:basename(InFile),
    Dir = filename:dirname(InFile),
    Ext = filename:extension(Base),
    Finfo = file:read_file_info(InFile),
    Minfo = file:read_file_info(filename:join(Dir,lists:concat([Module,Ext]))),
    case Finfo of
	Minfo ->
	    ok;
	_ ->
	    io:format("asn1error: Modulename and filename must be equal~n",[]),
	    throw(error)
    end.

vsn() ->
    ?vsn.
