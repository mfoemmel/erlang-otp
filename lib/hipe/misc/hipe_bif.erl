%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%		   INFORMATION ON BUILTIN FUNCTIONS
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_bif).
-export([is_bif/1, fails/2, bif_module/2, error_type/1,
	known_heap_need/1]).

%======================================================================

is_bif({M,F,A}) when atom(M), atom(F), integer(A) -> 
    case hipe_bifs:bif_address(M,F,A) of
	I when integer(I) ->
	    true;
	false -> 
	false
%	case {M,F,A} of
%	  {lists,map,2} -> true;
%	  _ -> false
%	end
    end;
is_bif({hipe_bs_primop,_}) -> true; 
is_bif(Atom) when atom(Atom) ->
  case catch hipe_bifs:primop_address(Atom) of
    Address when integer(Address) ->  
      true;
    _ -> 
      case Atom of
	cons -> true;
	unsafe_hd -> true;
	unsafe_tl -> true;
	mktuple -> true;
	unsafe_element -> true;
	gc_test -> true;
	element -> true;
	redtest -> true;
	get_msg -> true;
	next_msg -> true;
	select_msg -> true;
	clear_timeout -> true;
	suspend_msg -> true;
	call_fun -> true;
	enter_fun -> true;
	_ -> false
      end
  end;
is_bif(Tuple) when tuple(Tuple) ->
  case element(1, Tuple) of
    mkfun -> true;
    unsafe_element -> true;
    unsafe_update_element -> true;
    closure_element -> true;
    gc_test -> true;
    _ -> false
  end;
is_bif(_) ->
    false.

%======================================================================
%
% False if a bif always succeeds, true otherwise
%

fails(0,_BifName) ->
    false;
fails(1,BifName) ->	%% NEEDS UPDATING - is currently incomplete
    case BifName of
	erase -> false;
	get -> false;
	get_keys -> false;
        show_nstack -> false;
	throw -> false;    %% odd but right
	_ -> true
    end;
fails(2,BifName) -> %% possibly very incomplete!
    case BifName of
	put -> false;
	_ -> true
    end;
fails(3,BifName) -> %% possibly very incomplete!
    case BifName of
% 	spawn -> false;
% 	spawn_link -> false;
	_ -> true %% at this moment only apply -- this should be fixed.
    end.


error_type(Bif) ->
  case Bif of
    set_timeout -> timeout_value;
    _ -> badarg
  end.
      

%======================================================================
%
% Returns the module name of the bif
%

bif_module(Arity,BifName) ->
    erlang.

%======================================================================
% pure(Bif) ->	% XXX: Needs updating
%    case Bif of
%        bif_abs_1 -> true;
%        bif_append_2 -> true;
%        bif_atom_to_list_1 -> true;
%        bif_binary_to_list_1 -> true;
%        bif_binary_to_list_3 -> true;
%        bif_binary_to_term_1 -> true;
%        bif_concat_binary_1 -> true;
%        bif_do_integer_arith_3 -> true;
%        bif_do_integer_bnot_1 -> true;
%        bif_do_mixed_arith_3 -> true;
%        bif_do_mixed_comp_3 -> true;
%        bif_element_2 -> true;
%        bif_float_1 -> true;
%        bif_float_to_list_1 -> true;
%        bif_float_to_words_1 -> true;
%        bif_hash_2 -> true;
%        bif_hd_1 -> true;
%        bif_integer_to_list_1 -> true;
%        bif_length_1 -> true;
%        bif_list_to_atom_1 -> true;
%        bif_list_to_binary_1 -> true;
%        bif_list_to_float_1 -> true;
%        bif_list_to_integer_1 -> true;
%        bif_list_to_pid_1 -> true;
%        bif_list_to_tuple_1 -> true;
%        bif_math_2 -> true;
%        bif_math_3 -> true;
%        bif_math1_2 -> true;
%        bif_math1_cos_1 -> true;
%        bif_math1_sin_1 -> true;
%        bif_math1_sqrt_1 -> true;
%        bif_math2_atan2_2 -> true;
%        bif_math2_pow_2 -> true;
%        bif_pid_to_list_1 -> true;
%        bif_round_1 -> true;
%        bif_self_0 -> true;
%        bif_setelement_3 -> true;
%        bif_size_1 -> true;
%        bif_split_binary_2 -> true;
%        bif_subtract_2 -> true;
%        bif_term_to_binary_1 -> true;
%        bif_tl_1 -> true;
%        bif_trunc_1 -> true;
%        bif_tuple_to_list_1 -> true;
%        bif_type_2 -> true;
%       _ -> false
%    end.

known_heap_need(Bif) when atom(Bif) ->
  case Bif of
    cons -> true;
    mktuple -> true;
    unsafe_hd -> true;
    unsafe_tl -> true;
    unsafe_element -> true;
    element -> true;
    _ -> false
  end;
known_heap_need(Bif) ->
  case Bif of
    {erlang, element,2} -> true;
    {unsafe_element, N}  -> true;
    {unsafe_update_element, N}  -> true;
    {erlang, self, 0} -> true;
    {erlang, length, 1} -> true;
    {erlang, size, 1} -> true;
    _ -> false
  end.
      
