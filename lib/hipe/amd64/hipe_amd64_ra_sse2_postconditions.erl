%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

-module(hipe_amd64_ra_sse2_postconditions).
-export([check_and_rewrite/4]).
-include("hipe_amd64.hrl").
-define(HIPE_INSTRUMENT_COMPILER, true).
-include("../main/hipe.hrl").
-define(count_temp(T), ?cons_counter(counter_mfa_mem_temps, T)).


check_and_rewrite(AMD64Defun, Coloring, DontSpill, _Options) ->  
  %%io:format("Converting\n"),
  TempMap = hipe_temp_map:cols2tuple(Coloring,hipe_amd64_specific_sse2),
  %%io:format("Rewriting\n"),
  #defun{code=Code0} = AMD64Defun,
  {Code1, NewDontSpill} = do_insns(Code0, TempMap, [], DontSpill),
  {AMD64Defun#defun{code=Code1,
		  var_range={0, hipe_gensym:get_var(amd64)}}, 
   Coloring, NewDontSpill}.

do_insns([I|Insns], TempMap, Is, DontSpill) ->
  {NewIs, NewDontSpill} = do_insns(Insns, TempMap, Is, DontSpill),
  {NewI, FinalDontSpill} = do_insn(I, TempMap, NewDontSpill),
%%%   case [I] of
%%%     NewI -> ok;
%%%     _ ->
%%%       io:format("\n~w ->\n ~w\n------------\n",[I,NewI])
%%%   end,
  {NewI ++ NewIs, FinalDontSpill};
do_insns([],_, Is, DontSpill) ->
  {Is, DontSpill}.

do_insn(I, TempMap, DontSpill) ->	% Insn -> Insn list
  case I of
    #fmove{} ->
      do_fmove(I, TempMap, DontSpill);
    #fp_unop{} ->
      do_fp_unop(I, TempMap, DontSpill);
    #fp_binop{} ->
      do_fp_binop(I, TempMap, DontSpill);
    _ ->
      %% All non sse2 ops
      {[I], DontSpill}
  end.

%%% Fix an fp_binop.
do_fp_binop(I, TempMap, DontSpill) ->
  #fp_binop{src=Src,dst=Dst} = I,
  case is_mem_opnd(Dst, TempMap) of
    true ->
      Tmp = clone(Dst),
      {[#fmove{src=Dst, dst=Tmp},
	I#fp_binop{src=Src,dst=Tmp},
	#fmove{src=Tmp,dst=Dst}],
       [Tmp|DontSpill]};
    false ->
      {[I], DontSpill}
  end.

do_fp_unop(I, TempMap, DontSpill) ->
  #fp_unop{arg=Arg} = I,
  case is_mem_opnd(Arg, TempMap) of
    true ->
      Tmp = clone(Arg),
      {[#fmove{src=Arg, dst=Tmp},
	I#fp_unop{arg=Tmp},
	#fmove{src=Tmp,dst=Arg}],
       [Tmp|DontSpill]};
    false ->
      {[I], DontSpill}
  end.

%%% Fix an fmove op.
do_fmove(I, TempMap, DontSpill) ->
  #fmove{src=Src,dst=Dst} = I,
  case is_mem_opnd(Dst, TempMap) and is_mem_opnd(Src, TempMap) of
    true ->
      Tmp = clone(Src),
      {[#fmove{src=Src, dst=Tmp},I#fmove{src=Tmp,dst=Dst}],
       [Tmp|DontSpill]};
    false ->
      {[I], DontSpill}
  end.

%%% Check if an operand denotes a memory cell (mem or pseudo).

is_mem_opnd(Opnd, TempMap) ->
  R =
    case Opnd of
      #amd64_mem{} -> true;
      #amd64_temp{} -> 
	Reg = hipe_amd64:temp_reg(Opnd),
	case hipe_amd64:temp_is_allocatable(Opnd) of
	  true -> 
	    case size(TempMap) > Reg of 
	      true ->
		case 
		  hipe_temp_map:is_spilled(Reg,
					   TempMap) of
		  true ->
		    ?count_temp(Reg),
		    true;
		  false -> false
		end;
	      _ -> false
	    end;
	  false -> true
	end;
      _ -> false
    end,
  %%  io:format("Op ~w mem: ~w\n",[Opnd,R]),
  R.

%%% Check if an operand is a spilled Temp.

%%src_is_spilled(Src, TempMap) ->
%%  case hipe_amd64:is_temp(Src) of
%%    true ->
%%      Reg = hipe_amd64:temp_reg(Src),
%%      case hipe_amd64:temp_is_allocatable(Src) of
%%	true -> 
%%	  case size(TempMap) > Reg of 
%%	    true ->
%%	      case hipe_temp_map:is_spilled(Reg, TempMap) of
%%		true ->
%%		  ?count_temp(Reg),
%%		  true;
%%		false ->
%%		  false
%%	      end;
%%	    false ->
%%	      false
%%	  end;
%%	false -> true
%%      end;
%%    false -> false
%%  end.

%% is_spilled(Temp, TempMap) ->
%%   case hipe_amd64:temp_is_allocatable(Temp) of
%%     true ->
%%       Reg = hipe_amd64:temp_reg(Temp),
%%       case size(TempMap) > Reg of 
%%  	true ->
%%  	  case hipe_temp_map:is_spilled(Reg, TempMap) of
%%  	    true ->
%%  	      ?count_temp(Reg),
%%  	      true;
%%  	    false ->
%%  	      false
%%  	  end;
%%  	false ->
%%  	  false
%%       end;
%%     false -> true
%%   end.

%%% Make Reg a clone of Dst (attach Dst's type to Reg).

clone(Dst) ->
  Type =
    case Dst of
      #amd64_mem{} -> hipe_amd64:mem_type(Dst);
      #amd64_temp{} -> hipe_amd64:temp_type(Dst)
    end,
  hipe_amd64:mk_new_temp(Type).

%%% Make a certain reg into a clone of Dst

% clone2(Dst, Reg) ->
%   Type =
%     case Dst of
%       #amd64_mem{} -> hipe_amd64:mem_type(Dst);
%       #amd64_temp{} -> hipe_amd64:temp_type(Dst)
%     end,
%   hipe_amd64:mk_temp(Reg,Type).
