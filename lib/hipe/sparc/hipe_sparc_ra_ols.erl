%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Interface for register allocating SPARC code.  
%%

-module(hipe_sparc_ra_ols).
-export([alloc/2]).

-define(countspills,true).

-ifdef(countspills).
-define(count_spills(X), X).
-define(update_spillsum,
	case get(totalspill) of 
	  {__Loads,__Stores} ->
	    put(totalspill,{__Loads+get(loads),
			    __Stores+get(stores)}),
	    case get(spilledtemps) of
	      __Temps -> 
		put(spilledtemps, __Temps+get(temps));
	      _ ->
		true
	    end;
	  _ -> true 
	end).
-else.
-define(count_spills(X), true).
-define(update_spillsum,true).
-endif.

%
% Calls regalloc, rewrite the code after register allocation.
%
% Coloring are given as a list of {Reg, {reg, NewReg}} or 
% {Reg, {spill, SpillIndex}}.
%

alloc(SparcCfg, Options) ->
  %% io:format("~w\n",[erlang:statistics(runtime)]),
  ?count_spills({put(loads,0),put(stores,0),put(temps,0)}),
  {Coloring, NewSpillIndex} = 
    hipe_ls_regalloc:regalloc(SparcCfg,
			      (hipe_sparc_registers:allocatable() -- 
                                   hipe_sparc_registers:fixed()),
			      [hipe_sparc_cfg:start(SparcCfg)] ++ 
				   hipe_sparc_cfg:fail_entrypoints(SparcCfg),
			      0,
			      hipe_sparc_specific:number_of_temporaries(SparcCfg),
			      Options,
			      hipe_sparc_specific),
  %% io:format("~w\n",[erlang:statistics(runtime)]),

  case NewSpillIndex > 0 of
    false ->
      ColTuple = hipe_sparc_ra_ls:cols2tuple(Coloring),
      Labels = hipe_sparc_cfg:labels(SparcCfg),
      hipe_sparc_ra_ls:rewrite(SparcCfg,ColTuple);
    true ->
      hipe_sparc_ra_ls:alloc(SparcCfg, Options)
  end.
