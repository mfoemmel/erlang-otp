%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%%  Filename : 	hipe_sparc_verify.erl
%%  Module   :	hipe_sparc_verify
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-10-25 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: richardc $
%%              $Date: 2002/10/01 12:47:17 $
%%              $Revision: 1.2 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc_verify).
-export([verify/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Verifies that a code sequence is well-formed.
%%
%% NOTE: does not consider branch offsets.

verify([],Version) -> ok;
verify([X|Xs],Version) ->
  case catch ver(X,Version) of
    {'EXIT',_} ->
      report('instruction ~w malformed~n',[X]);
    _ -> ok
  end,
  verify(Xs,Version).

ver(Label,_Version) when is_record(Label,label) ->
  L = Label#label.id,
  if
    is_integer(L), L > 0 -> ok
  end;

ver(Nop,_Version) when is_record(Nop,nop) -> 
  ok;
ver(Comment,_Version) when is_record(Comment,comment) -> 
  ok;
ver(Align,_Version) when is_record(Align,align) ->
  N = Align#align.alignment,
  if
    N == 1 -> ok;
    N == 2 -> ok;
    N == 4 -> ok;
    N == 8 -> ok;
    N == 16 -> ok
  end;
ver(Move,_Version) when is_record(Move,move) ->
  Dst = Move#move.dst,
  Src = Move#move.src,
  case {reg(Dst),reg_or_imm13(Src)} of
    {true,true} -> ok
  end;
ver(Cmov_cc,Version) when is_record(Cmov_cc,cmov_cc) ->
  if 
    Version == 9 ->
      Dst = Cmov_cc#cmov_cc.dst,
      Src = Cmov_cc#cmov_cc.src,
      CC = Cmov_cc#cmov_cc.cc,
      case {reg(Dst),reg_or_imm11(Src),int_cc(CC)} of
	{true,true,true} -> ok
      end
  end;
ver(Cmov_r,Version) when is_record(Cmov_r,cmov_r) ->
  if 
    Version == 9 ->
      Dst = Cmov_r#cmov_r.dst,
      Src = Cmov_r#cmov_r.src,
      Pred = Cmov_r#cmov_r.reg,
      Cond = Cmov_r#cmov_r.rcc,
      case {reg(Dst),reg_or_imm10(Src),reg(Pred),reg_cc(Cond)} of
	{true,true,true,true} -> ok
      end
  end;
ver(Alu,Version) when is_record(Alu,alu) ->
  Dst = Alu#alu.dst,
  Src1 = Alu#alu.src1,
  Op = Alu#alu.op,
  Src2 = Alu#alu.src2,
  case {reg(Dst),reg(Src1),alu_op(Op,Version),reg_or_imm13(Src2)} of
    {true,true,true,true} ->
      ok
  end;
ver(Alu_cc,Version) when is_record(Alu_cc,alu_cc) ->
  Dst = Alu_cc#alu_cc.dst,
  Src1 = Alu_cc#alu_cc.src1,
  Op = Alu_cc#alu_cc.op,
  Src2 = Alu_cc#alu_cc.src2,
  case {reg(Dst),reg(Src1),alu_cc_op(Op,Version),reg_or_imm13(Src2)} of
    {true,true,true,true} -> ok
  end;
ver(Sethi,_Version) when is_record(Sethi,sethi) ->
  Dst = Sethi#sethi.dst,
  Const = Sethi#sethi.const,
  case {reg(Dst),imm22(Const)} of
    {true,true} -> ok
  end;
ver(Load,_Version) when is_record(Load,load) ->
  Dst = Load#load.dst,
  Type = Load#load.type,
  Src = Load#load.src,
  Off = Load#load.off,
  case {reg(Dst),loading_type(Type),reg(Src),reg_or_imm13(Off)} of
    {true,true,true,true} -> ok
  end;
ver(Store,_Version) when is_record(Store,store) ->
  Dst = Store#store.dst,
  Off = Store#store.off,
  Type = Store#store.type,
  Src = Store#store.src,
  case {reg(Dst),reg_or_imm13(Off),storing_type(Type),reg(Src)} of
    {true,true,true,true} -> ok
  end;
ver(B,Version) when is_record(B,b) ->
  CC = B#b.cc,
  Pred = B#b.pred,
  Annul = B#b.annul,
  if
    Version == 9 ->
      case {int_cc(CC),prediction(Pred),annul_info(Annul)} of
	{true,true,true} -> ok
      end;
    Version == 8 ->
      case {int_cc(CC),prediction(Pred),annul_info(Annul)} of
	{true,true,true} -> ok
      end
  end;
ver(Br,Version) when is_record(Br,br) ->
  Reg = Br#br.reg,
  RC = Br#br.rcc,
  Pred = Br#br.pred,
  Annul = Br#br.annul,
  if
    Version == 9 ->
      case {reg(Reg),reg_cc(RC),prediction(Pred),annul_info(Annul)} of
	{true,true,true,true} -> ok
      end
  end;
ver(Jmp_link,_Version) when is_record(Jmp_link,jmp_link) ->
  Target = Jmp_link#jmp_link.target,
  Off = Jmp_link#jmp_link.off,
  Link = Jmp_link#jmp_link.link,
  case {reg(Target),reg_or_imm13(Off),reg(Link)} of
    {true,true,true} -> ok
  end;
ver(Jmp,_Version) when is_record(Jmp,jmp) ->
  Target = Jmp#jmp.target,
  Off = Jmp#jmp.off,
  case {reg(Target),reg_or_imm13(Off)} of
    {true,true} -> ok
  end;
ver(Call_link,_Version) when is_record(Call_link,call_link) ->
  Link = Call_link#call_link.link,
  case reg(Link) of
    true -> ok
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
						%
						% Condition code handling.
						%

reg_cc(RCond) ->	% for BPr and CMOVr isns
  case RCond of
    'z'	-> true;
    'lez'	-> true;
    'lz'	-> true;
    'nz'	-> true;
    'gz'	-> true;
    'gez'	-> true;
    _	-> false	% XXX: serious error, should exit
  end.

int_cc(Cond) ->
  case Cond of
    'a'	-> true;
    'n'	-> true;
    'ne'	-> true;
    'e'	-> true;
    'g'	-> true;
    'le'	-> true;
    'ge'	-> true;
    'l'	-> true;
    'gu'	-> true;
    'leu'	-> true;
    'geu'	-> true;
    'lu'	-> true;
    'pos'	-> true;
    'neg'	-> true;
    'vc'	-> true;
    'vs'	-> true;
    _	-> false	% XXX: serious error, should exit
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reg({reg,R}) when is_integer(R), R >= 0 -> true;
reg(_) -> false.

reg_or_imm13({imm,Imm}) ->
  imm13(Imm);
reg_or_imm13(Reg) -> reg(Reg).

reg_or_imm11({imm,Imm}) ->
  imm11(Imm);
reg_or_imm11(Reg) -> reg(Reg).

reg_or_imm10({imm,Imm}) ->
  imm10(Imm);
reg_or_imm10(Reg) -> reg(Reg).

						% Note: does not verify branch offsets

						%label_imm16({label,L}) when L > 0 -> true;  
						%label_imm16(_) -> false.

						%label_imm19({label,L}) when L > 0 -> true;
						%label_imm19(_) -> false.

						%label_imm22({label,L}) when L > 0 -> true;
						%label_imm22(_) -> false.

						%label_imm30({label,L}) when L > 0 -> true;
						%label_imm30(_) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

imm10(N) -> imm(N,10).
imm11(N) -> imm(N,11).
imm13(N) -> imm(N,13).
%imm16(N) -> imm(N,16).
%imm19(N) -> imm(N,19).
imm22(N) -> imm(N,22).

imm(Imm,N) when Imm >= -((1 bsl N+1)-1), Imm < (1 bsl N+1) ->
  true;
imm(Imm,N) ->
  false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prediction(taken) -> true;
prediction(untaken) -> true;
prediction(_) -> false.

annul_info(a) -> true;
annul_info(na) -> true;
annul_info(_) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loading_type(X) -> data_type(X).

storing_type(X) -> data_type(X).

data_type(sb) -> true;
data_type(sh) -> true;
data_type(sw) -> true;
data_type(ub) -> true;
data_type(uh) -> true;
data_type(uw) -> true;
data_type(xw) -> true;
data_type(_) -> false.

alu_op(X,Version) -> int_op(X,Version).

alu_cc_op(X,Version) -> int_op(X,Version).

int_op('+',Version) -> is_version(Version,[8,9]);
int_op('+c',Version) -> is_version(Version,[8,9]);
int_op('and',Version) -> is_version(Version,[8,9]);
int_op('andn',Version) -> is_version(Version,[8,9]);
int_op('or',Version) -> is_version(Version,[8,9]);
int_op('orn',Version) -> is_version(Version,[8,9]);
int_op('xor',Version) -> is_version(Version,[8,9]);
int_op('xnor',Version) -> is_version(Version,[8,9]);
int_op('-',Version) -> is_version(Version,[8,9]);
int_op('-c',Version) -> is_version(Version,[8,9]);
int_op('<<',Version) -> is_version(Version,[8,9]);
int_op('>>',Version) -> is_version(Version,[8,9]);
int_op('>>?',Version) -> is_version(Version,[8,9]);
int_op('*s',Version) -> is_version(Version,[8,9]);
int_op('*u',Version) -> is_version(Version,[8,9]);
int_op('/s',Version) -> is_version(Version,[8,9]);
int_op('/u',Version) -> is_version(Version,[8,9]);
int_op('<<64',Version) -> is_version(Version,9);
int_op('>>64',Version) -> is_version(Version,9);
int_op('>>?64',Version) -> is_version(Version,9);
int_op('*64',Version) -> is_version(Version,9);
int_op('/s64',Version) -> is_version(Version,9);
int_op('/u64',Version) -> is_version(Version,9);
int_op(_,_) -> false.

is_version(Version,Version) -> true;
is_version(Version,VersionList) -> member(Version,VersionList).

member(Version,[Version|_]) -> true;
member(Version,[_|Versions]) -> member(Version,Versions);
member(_,[]) -> false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

report(String,Args) -> io:format(String,Args), io:format('~n',[]).


