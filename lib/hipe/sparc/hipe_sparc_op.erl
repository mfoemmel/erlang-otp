%% Copyright (c) 1997 by Erik Johansson.  All Rights Reserved 
%% Time-stamp: <01/02/15 00:19:52 happi>
%% ====================================================================
%%  Filename : 	hipe_sparc_op.erl
%%  Module   :	hipe_sparc_op
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 1997-04-14 Erik Johansson (happi@csd.uu.se): Created.
%% CVS:
%%    $Author: richardc $
%%    $Date: 2001/03/26 18:37:09 $
%%    $Revision: 1.1.1.1 $
%% ====================================================================
%% Exported functions (short description):
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc_op).
-export([annul_bit/1,predicate_bit/1,bits_10/1,
	 bits_high/1,bits_low/1,high22/1,
	 sti/3,st/3,ldi/3,ld/3,call/1,jumpli/3,nop/0,sethi/2,
	 savei/3,restorei/3,

	 cc_bits/1, rcc_bits/1,

	 bicc/3, bpcc/4, bpr/5,

	 sll/3,slli/3,srl/3,srli/3,sra/3,srai/3,
	 sllx/3,sllix/3,srlx/3,srlix/3,srax/3,sraix/3,
	 add/3,addi/3,addcc/3,addicc/3,
	 sub/3,subi/3,subcc/3,subicc/3,
	 and_op/3,andi/3,andcc/3,andicc/3,andn/3,
	 andni/3,andncc/3,andnicc/3,or_op/3,ori/3,orcc/3,oricc/3,
	 xor_op/3,xori/3,
	 xnor_op/3,xnori/3,
	 xorcc/3,xoricc/3,
	 taddcc/3, taddcci/3, tsubcc/3, tsubcci/3,
	
	 make_bitlist/2]).

annul_bit(a) -> 1;
annul_bit(na) -> 0.

predicate_bit(true) -> 1;
predicate_bit(false) -> 0.

bits_10(X) -> X band      16#3ff.
bits_13(X) -> X band     16#1fff.
bits_14(X) -> X band     16#3fff.
%bits_16(X) -> X band     16#ffff.
bits_19(X) -> X band    16#7ffff.
bits_22(X) -> X band   16#3fffff.
%bits_30(X) -> X band 16#3fffffff.
bits_32(X) -> X band 16#ffffffff.

%% Splits a 16 bits displacement for format22d into low (bit 0-13) and high (bit 14-15 into bit 20-21)
bits_16_high(X) -> (X band 16#c000) bsl 6.
bits_16_low(X) -> bits_14(X).

bits_low(X) -> X band 16#ffff.
bits_high(X) -> (X bsr 16) band 16#ffff.

high22(X) -> X bsr 10.


format1(Disp30) ->
  ((1 bsl 30) bor (bits_32(Disp30) bsr 2)).

format2a(Rd, Op2, Imm22)  ->
  ((Rd bsl 25) bor (Op2 bsl 22) bor bits_22(Imm22)).

format2b(A, Cond, Op2, Disp22)  ->
  ((A bsl 29) bor (Cond bsl 25) bor (Op2 bsl 22) bor
   bits_22(Disp22)).

format2c(A,Cond,Op2,CC1,CC0,P,Disp19) ->
  ((A bsl 29) bor (Cond bsl 25) bor (Op2 bsl 22) bor
   (CC1 bsl 21) bor (CC0 bsl 20) bor (P bsl 19) 
   bor bits_19(Disp19)).

format2d(A,RCond,Op2,Disp16,P,Rs1) ->
  ((A bsl 29) bor (RCond bsl 25) bor (Op2 bsl 22) bor
   bits_16_high(Disp16) bor (P bsl 19) bor (Rs1 bsl 14) 
   bor bits_16_low(Disp16)).



format3h(Op, Rd, Op3, Rs1)  ->
  ((Op bsl 30) bor (Rd bsl 25) bor (Op3 bsl 19) bor (Rs1 bsl
  14)).


format3a(Op, Rd, Op3, Rs1, Asi, Rs2)  ->
  (format3h(Op, Rd, Op3, Rs1) bor (Asi bsl 5) bor (Rs2)).

format3ax(Op, Rd, Op3, Rs1, Asi, Rs2)  ->
  (format3h(Op, Rd, Op3, Rs1) bor (Asi bsl 5) bor (1 bsl 12) bor (Rs2)).


format3b(Op, Rd, Op3, Rs1, Simm13)  ->
  (format3h(Op, Rd, Op3, Rs1) bor (1 bsl 13) bor bits_13(Simm13)).

format3bx(Op, Rd, Op3, Rs1, Simm13)  ->
  (format3h(Op, Rd, Op3, Rs1) bor (1 bsl 13) bor (1 bsl 12) bor bits_13(Simm13)).


%format3c(Op, Rd, Op3, Rs1, Opf, Rs2)  ->
%  (format3h(Op, Rd, Op3, Rs1) bor ((Opf) bsl 5) bor (Rs2)).



sti(Data, Addr, Offs) ->  format3b(3, Data, 16#04, Addr, Offs).
st(Data, Addr, Offs) ->   format3a(3, Data, 16#04, Addr, 0, Offs).
ldi(Addr, Offs, Res) ->   format3b(3, Res, 16#00, Addr, Offs).
ld(Addr, Offs, Res)  ->   format3a(3, Res, 16#00, Addr, 0, Offs).
call(Addr)  ->            format1(Addr).
jumpli(To, Offs, Rd) ->   format3b(2, Rd, 16#38, To, Offs).
nop()   ->                format2a(0, 16#4, 0).
sethi(High22, Reg) ->     format2a(Reg, 16#4, High22).
savei(Rs1, Imm, Rd) ->    format3b(2, Rd, 16#3c, Rs1, Imm).
restorei(Rs1, Imm, Rd)->  format3b(2, Rd, 16#3d, Rs1, Imm).

cc_bits(Cond) ->
    case Cond of
	'a'	-> 2#1000;
	'n'	-> 2#0000;
	'ne'	-> 2#1001;
	'e'	-> 2#0001;
	'g'	-> 2#1010;
	'le'	-> 2#0010;
	'ge'	-> 2#1011;
	'l'	-> 2#0011;
	'gu'	-> 2#1100;
	'leu'	-> 2#0100;
	'geu'	-> 2#1101;	% a.k.a. 'cc'
	'lu'	-> 2#0101;	% a.k.a. 'cs'
	'pos'	-> 2#1110;
	'neg'	-> 2#0110;
	'vc'	-> 2#1111;
	'vs'	-> 2#0111;
	_	-> exit({cond_not_handled,Cond})
    end.

rcc_bits(RCond) ->		% for BPr and CMOVr insns
    case RCond of
	'z'	-> 2#001;
	'lez'	-> 2#010;
	'lz'	-> 2#011;
	'nz'	-> 2#101;
	'gz'	-> 2#110;
	'gez'	-> 2#111;
	_	-> exit({rcond_not_handled,RCond})
    end.

% for SPARC V7/V8 compatibility
bicc(Cond, Annul, Disp) ->		% b<cond><,annul> <disp>
    format2b(Annul, Cond, 2#010, Disp).

% only for SPARC V9
bpcc(Cond, Annul, Pred, Disp) ->	% bp<cond><,annul><,pred> <disp>
    format2c(Annul, Cond, 2#001, 0, 0, Pred, Disp).

% only for SPARC V9
bpr(RCond, Annul, Pred, Reg, Disp) ->	% bpr<cond><,annul><,pred> <reg>,<disp>
    format2d(Annul, RCond, 2#011, Disp, Pred, Reg).

sll(Val, Shreg, Res) ->   format3a(2, Res, 16#25, Val, 0, Shreg).
sllx(Val, Shreg, Res) ->  format3ax(2, Res, 16#25, Val, 0, Shreg).
slli(Val, Shcnt, Res) ->  format3b(2, Res, 16#25, Val, Shcnt).
sllix(Val, Shcnt, Res) -> format3bx(2, Res, 16#25, Val, Shcnt).
srl(Val, Shreg, Res) ->   format3a(2, Res, 16#26, Val, 0, Shreg).
srli(Val, Shcnt, Res) ->  format3b(2, Res, 16#26, Val, Shcnt).
sra(Val, Shreg, Res) ->   format3a(2, Res, 16#27, Val, 0, Shreg).
srai(Val, Shcnt, Res) ->  format3b(2, Res, 16#27, Val, Shcnt).
srlx(Val, Shreg, Res) ->   format3ax(2, Res, 16#26, Val, 0, Shreg).
srlix(Val, Shcnt, Res) ->  format3bx(2, Res, 16#26, Val, Shcnt).
srax(Val, Shreg, Res) ->   format3ax(2, Res, 16#27, Val, 0, Shreg).
sraix(Val, Shcnt, Res) ->  format3bx(2, Res, 16#27, Val, Shcnt).

add(Op1, Op2, Res) ->     format3a(2, Res, 16#00, Op1, 0, Op2).
addi(Op1, Imm, Res) ->    format3b(2, Res, 16#00, Op1, Imm).
addcc(Op1, Op2, Res) ->   format3a(2, Res, 16#10, Op1, 0, Op2).
addicc(Op1, Imm, Res) ->  format3b(2, Res, 16#10, Op1, Imm).

sub(Op1, Op2, Res) ->     format3a(2, Res, 16#04, Op1, 0, Op2).
subi(Op1, Imm, Res) ->    format3b(2, Res, 16#04, Op1, Imm).
subcc(Op1, Op2, Res) ->   format3a(2, Res, 16#14, Op1, 0, Op2).
subicc(Op1, Imm, Res) ->  format3b(2, Res, 16#14, Op1, Imm).

and_op(Op1, Op2, Res) ->     format3a(2, Res, 16#01, Op1, 0, Op2).
andi(Op1, Imm, Res) ->    format3b(2, Res, 16#01, Op1, Imm).
andcc(Op1, Op2, Res) ->   format3a(2, Res, 16#11, Op1, 0, Op2).
andicc(Op1, Imm, Res) ->  format3b(2, Res, 16#11, Op1, Imm).

andn(Op1, Op2, Res) ->     format3a(2, Res, 16#05, Op1, 0, Op2).
andni(Op1, Imm, Res) ->    format3b(2, Res, 16#05, Op1, Imm).
andncc(Op1, Op2, Res) ->   format3a(2, Res, 16#15, Op1, 0, Op2).
andnicc(Op1, Imm, Res) ->  format3b(2, Res, 16#15, Op1, Imm).

or_op(Op1, Op2, Res) ->      format3a(2, Res, 16#02, Op1, 0, Op2).
ori(Op1, Imm, Res) ->     format3b(2, Res, 16#02, Op1, Imm).
orcc(Op1, Op2, Res) ->    format3a(2, Res, 16#12, Op1, 0, Op2).
oricc(Op1, Imm, Res) ->   format3b(2, Res, 16#12, Op1, Imm).

xor_op(Op1, Op2, Res) ->     format3a(2, Res, 16#03, Op1, 0, Op2).
xori(Op1, Imm, Res) ->    format3b(2, Res, 16#03, Op1, Imm).
xnor_op(Op1, Op2, Res) ->  format3a(2, Res, 16#07, Op1, 0, Op2).
xnori(Op1, Imm, Res) ->    format3b(2, Res, 16#07, Op1, Imm).
xorcc(Op1, Op2, Res) ->   format3a(2, Res, 16#13, Op1, 0, Op2).
xoricc(Op1, Imm, Res) ->  format3b(2, Res, 16#13, Op1, Imm).

taddcc(Rs1, Rs2, Rd) ->		format3a(2, Rd, 16#20, Rs1, 0, Rs2).
taddcci(Rs1, Simm13, Rd) ->	format3b(2, Rd, 16#20, Rs1, Simm13).
tsubcc(Rs1, Rs2, Rd) ->		format3a(2, Rd, 16#21, Rs1, 0, Rs2).
tsubcci(Rs1, Simm13, Rd) ->	format3b(2, Rd, 16#21, Rs1, Simm13).

make_bitlist(sethi,hi) ->
  bitlist_sethi(21);
make_bitlist(alu,low) ->
  bitlist_lowlow(10);
make_bitlist(call,low) ->
  bitlist_call(29);
make_bitlist(I,Type) -> 
  exit({combination_not_handled,I,Type}).

bitlist_sethi(0) -> [{10,0}];
bitlist_sethi(N) ->
  [{10+N,N}|bitlist_sethi(N-1)].

bitlist_lowlow(0) -> [{0,0}];
bitlist_lowlow(N) ->
  [{N,N}|bitlist_lowlow(N-1)].

bitlist_call(0) -> [{2,0}];
bitlist_call(N) ->
  [{N+2,N}|bitlist_call(N-1)].
