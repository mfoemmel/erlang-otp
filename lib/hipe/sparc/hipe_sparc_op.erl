%% ====================================================================
%%  Filename : 	hipe_sparc_op.erl
%%  Module   :	hipe_sparc_op
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 1997-04-14 Erik Johansson (happi@csd.uu.se): Created.
%% CVS:
%%    $Author: happi $
%%    $Date: 2002/05/13 10:13:08 $
%%    $Revision: 1.6 $
%% ====================================================================
%% Exported functions (short description):
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc_op).
-export([annul_bit/1,predicate_bit/1,bits_10/1,
	 bits_high/1,bits_low/1,high22/1,
	 sti/3,st/3,ldi/3,ld/3,call/1,jumpli/3,nop/0,sethi/2,
	 savei/3,restorei/3,

	 stb/3,sth/3,stw/3,stx/3,stbi/3,sthi/3,stwi/3,stxi/3,

	 ldsb/3,ldsh/3,ldsw/3,ldub/3,lduh/3,lduw/3,ldx/3,
	 ldsbi/3,ldshi/3,ldswi/3,ldubi/3,lduhi/3,lduwi/3,ldxi/3,


	 cc_bits/1, rcc_bits/1, fcc_bits/1,

	 bicc/3, bpcc/4, bpr/5, 
	 fbfcc/3, %% Obsolete in V9
	 fbpfcc/5,
	 fbpfcc0/4, fbpfcc1/4, fbpfcc2/4, fbpfcc3/4,


	 sll/3,slli/3,srl/3,srli/3,sra/3,srai/3,
	 sllx/3,sllix/3,srlx/3,srlix/3,srax/3,sraix/3,
	 add/3,addi/3,addcc/3,addicc/3,
	 addc/3,addci/3,addccc/3,addcicc/3,

	 sub/3,subi/3,subcc/3,subicc/3,
	 subc/3,subci/3,subccc/3,subcicc/3,
	 
	 and_op/3,andi/3,andcc/3,andicc/3,andn/3,
	 andni/3,andncc/3,andnicc/3,or_op/3,ori/3,orcc/3,oricc/3,
	 xor_op/3,xori/3,
	 xnor_op/3,xnori/3,
	 xorcc/3,xoricc/3,
	 taddcc/3, taddcci/3, tsubcc/3, tsubcci/3,
	 fxtos/2, fxtod/2, fxtoq/2,
	 fitos/2, fitod/2, fitoq/2,
	 fstox/2, fdtox/2, fqtox/2,
	 fstoi/2, fdtoi/2, fqtoi/2,
	 fstod/2, fstoq/2, fdtos/2, fdtoq/2, fqtos/2, fqtod/2,
	 fmovs/2, fnegs/2, fabss/2,
	 fmovd/2, fnegd/2, fabsd/2,
	 fmovq/2, fnegq/2, fabsq/2,
	 fsqrts/2, fsqrtd/2, fsqrtq/2,
	 fadds/3, faddd/3, faddq/3, 	 
	 fsubs/3, fsubd/3, fsubq/3, 

	 fmuls/3, fmuld/3, fmulq/3, 
	 fsmuld/3, fdmulq/3, 
	 fdivs/3, fdivd/3, fdivq/3, 
	 fcmps/2, fcmpd/2, fcmpq/2, fcmpes/2, fcmped/2, fcmpeq/2,
	 fcmps/3, fcmpd/3, fcmpq/3, fcmpes/3, fcmped/3, fcmpeq/3,

	 stf/3, stfi/3, stdf/3, stdfi/3, stfsr/3, stfsri/3, 
	 stqf/3, stqfi/3,
	 ldf/3, ldfi/3, lddf/3, lddfi/3, ldqf/3, ldqfi/3,
	 ldxfsr/2, ldxfsri/2,
  	
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

format3c(Op, Rd, Op3, Rs1, Opf, Rs2)  ->
  (format3h(Op, Rd, Op3, Rs1) bor (Opf bsl 5) bor (Rs2)).

%% format3d(Op, Rd, Op3, Rs1, I, Rs2)  ->
%%  (format3h(Op, Rd, Op3, Rs1) bor (I bsl 13) bor (Rs2)).





sti(Data, Addr, Offs) ->  format3b(3, Data, 16#04, Addr, Offs).
st(Data, Addr, Offs) ->   format3a(3, Data, 16#04, Addr, 0, Offs).
stb(Data, Addr, Offs) ->   format3a(3, Data, 2#000101, Addr, 0, Offs).
sth(Data, Addr, Offs) ->   format3a(3, Data, 2#000110, Addr, 0, Offs).
stw(Data, Addr, Offs) ->   format3a(3, Data, 2#000100, Addr, 0, Offs).
stx(Data, Addr, Offs) ->   format3a(3, Data, 2#001110, Addr, 0, Offs).
stbi(Data, Addr, Offs) ->   format3b(3, Data, 2#000101, Addr, Offs).
sthi(Data, Addr, Offs) ->   format3b(3, Data, 2#000110, Addr, Offs).
stwi(Data, Addr, Offs) ->   format3b(3, Data, 2#000100, Addr, Offs).
stxi(Data, Addr, Offs) ->   format3b(3, Data, 2#001110, Addr, Offs).

ldi(Addr, Offs, Res) ->   format3b(3, Res, 16#00, Addr, Offs).
ld(Addr, Offs, Res)  ->   format3a(3, Res, 16#00, Addr, 0, Offs).
ldsb(Addr, Offs, Res)  ->   format3a(3, Res, 2#001001, Addr, 0, Offs).
ldsh(Addr, Offs, Res)  ->   format3a(3, Res, 2#001010, Addr, 0, Offs).
ldsw(Addr, Offs, Res)  ->   format3a(3, Res, 2#001000, Addr, 0, Offs).
ldub(Addr, Offs, Res)  ->   format3a(3, Res, 2#000001, Addr, 0, Offs).
lduh(Addr, Offs, Res)  ->   format3a(3, Res, 2#000010, Addr, 0, Offs).
lduw(Addr, Offs, Res)  ->   format3a(3, Res, 2#000000, Addr, 0, Offs).
ldx(Addr, Offs, Res)  ->   format3a(3, Res, 2#001011, Addr, 0, Offs).
ldsbi(Addr, Offs, Res)  ->   format3b(3, Res, 2#001001, Addr, Offs).
ldshi(Addr, Offs, Res)  ->   format3b(3, Res, 2#001010, Addr, Offs).
ldswi(Addr, Offs, Res)  ->   format3b(3, Res, 2#001000, Addr, Offs).
ldubi(Addr, Offs, Res)  ->   format3b(3, Res, 2#000001, Addr, Offs).
lduhi(Addr, Offs, Res)  ->   format3b(3, Res, 2#000010, Addr, Offs).
lduwi(Addr, Offs, Res)  ->   format3b(3, Res, 2#000000, Addr, Offs).
ldxi(Addr, Offs, Res)  ->   format3b(3, Res, 2#001011, Addr, Offs).


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

fcc_bits(Cond) ->
  case Cond of
    'a'	->  2#1000;
    'n'	->  2#0000;
    'u' ->  2#0111;
    'g'	->  2#0110;
    'ug' -> 2#0101;
    'l'	->  2#0100;
    'ul' -> 2#0011;
    'lg' -> 2#0010;
    'ne' -> 2#0001;
    'e'	->  2#1001;
    'ue' -> 2#1010;
    'ge' -> 2#1011;
    'uge'-> 2#1100;	
    'le' -> 2#1101;
    'ule'-> 2#1110;
    'o'	->  2#1111;
    _	-> exit({fcc_cond_not_handled,Cond})
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

% for SPARC V7/V8 compatibility
fbfcc(Cond, Annul, Disp) ->	        % fb<cond><,annul><,pred> <disp>
  format2b(Annul, Cond, 2#110, Disp).

% only for SPARC V9
fbpfcc(N,Cond, Annul, Pred, Disp) ->	
  case N of 
    0 -> fbpfcc0(Cond, Annul, Pred, Disp);
    1 -> fbpfcc1(Cond, Annul, Pred, Disp);
    2 -> fbpfcc2(Cond, Annul, Pred, Disp);
    3 -> fbpfcc3(Cond, Annul, Pred, Disp)
  end.
      
fbpfcc0(Cond, Annul, Pred, Disp) ->
    format2c(Annul, Cond, 2#101, 0, 0, Pred, Disp).
fbpfcc1(Cond, Annul, Pred, Disp) ->
    format2c(Annul, Cond, 2#101, 0, 1, Pred, Disp).
fbpfcc2(Cond, Annul, Pred, Disp) ->
    format2c(Annul, Cond, 2#101, 1, 0, Pred, Disp).
fbpfcc3(Cond, Annul, Pred, Disp) ->
    format2c(Annul, Cond, 2#101, 1, 1, Pred, Disp).



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
addc(Op1, Op2, Res) ->    format3a(2, Res, 2#001000, Op1, 0, Op2).
addci(Op1, Imm, Res) ->   format3b(2, Res, 2#001000, Op1, Imm).
addccc(Op1, Op2, Res) ->  format3a(2, Res, 2#011000, Op1, 0, Op2).
addcicc(Op1, Imm, Res) -> format3b(2, Res, 2#011000, Op1, Imm).

sub(Op1, Op2, Res) ->     format3a(2, Res, 16#04, Op1, 0, Op2).
subi(Op1, Imm, Res) ->    format3b(2, Res, 16#04, Op1, Imm).
subcc(Op1, Op2, Res) ->   format3a(2, Res, 16#14, Op1, 0, Op2).
subicc(Op1, Imm, Res) ->  format3b(2, Res, 16#14, Op1, Imm).
subc(Op1, Op2, Res) ->    format3a(2, Res, 16#12, Op1, 0, Op2).
subci(Op1, Imm, Res) ->   format3b(2, Res, 16#12, Op1, Imm).
subccc(Op1, Op2, Res) ->  format3a(2, Res, 16#28, Op1, 0, Op2).
subcicc(Op1, Imm, Res) -> format3b(2, Res, 16#28, Op1, Imm).

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

fpop1(Rs1,Opf,Rs2,Rd) ->        format3a(2#10, Rd, 2#110100, Rs1, Opf, Rs2).
fpop2(Rs1,Opf,Rs2,Rd) ->        format3a(2#10, Rd, 2#110101, Rs1, Opf, Rs2).

fxtos(Rs2, Rd) ->               fpop1(0,2#010000100,Rs2,Rd).
fxtod(Rs2, Rd) ->               fpop1(0,2#010001000,Rs2,Rd).
fxtoq(Rs2, Rd) ->               fpop1(0,2#010001100,Rs2,Rd).
fitos(Rs2, Rd) ->               fpop1(0,2#011000100,Rs2,Rd).
fitod(Rs2, Rd) ->               fpop1(0,2#011001000,Rs2,Rd).
fitoq(Rs2, Rd) ->               fpop1(0,2#011001100,Rs2,Rd).

fstox(Rs2, Rd) ->               fpop1(0,2#010000001,Rs2,Rd).
fdtox(Rs2, Rd) ->               fpop1(0,2#010000010,Rs2,Rd).
fqtox(Rs2, Rd) ->               fpop1(0,2#010000011,Rs2,Rd).
fstoi(Rs2, Rd) ->               fpop1(0,2#011010001,Rs2,Rd).
fdtoi(Rs2, Rd) ->               fpop1(0,2#011010010,Rs2,Rd).
fqtoi(Rs2, Rd) ->               fpop1(0,2#011010011,Rs2,Rd).
  
fstod(Rs2, Rd) ->               fpop1(0,2#011001001,Rs2,Rd).
fstoq(Rs2, Rd) ->               fpop1(0,2#011001101,Rs2,Rd).
fdtos(Rs2, Rd) ->               fpop1(0,2#011000110,Rs2,Rd).
fdtoq(Rs2, Rd) ->               fpop1(0,2#011001110,Rs2,Rd).
fqtos(Rs2, Rd) ->               fpop1(0,2#011000111,Rs2,Rd).
fqtod(Rs2, Rd) ->               fpop1(0,2#011001011,Rs2,Rd).
  
fmovs(Rs2, Rd) ->               fpop1(0,2#000000001,Rs2,Rd).
fnegs(Rs2, Rd) ->               fpop1(0,2#000000101,Rs2,Rd).
fabss(Rs2, Rd) ->               fpop1(0,2#000001001,Rs2,Rd).
fmovd(Rs2, Rd) ->               fpop1(0,2#000000010,Rs2,Rd).
fnegd(Rs2, Rd) ->               fpop1(0,2#000000110,Rs2,Rd).
fabsd(Rs2, Rd) ->               fpop1(0,2#000001010,Rs2,Rd).
fmovq(Rs2, Rd) ->               fpop1(0,2#000000011,Rs2,Rd).
fnegq(Rs2, Rd) ->               fpop1(0,2#000000111,Rs2,Rd).
fabsq(Rs2, Rd) ->               fpop1(0,2#000001011,Rs2,Rd).

fsqrts(Rs2, Rd) ->              fpop1(0,2#000101001,Rs2,Rd).
fsqrtd(Rs2, Rd) ->              fpop1(0,2#000101010,Rs2,Rd).
fsqrtq(Rs2, Rd) ->              fpop1(0,2#000101011,Rs2,Rd).

fadds(Rs1, Rs2, Rd) ->          fpop1(Rs1,2#001000001,Rs2,Rd).
faddd(Rs1, Rs2, Rd) ->          fpop1(Rs1,2#001000010,Rs2,Rd).
faddq(Rs1, Rs2, Rd) ->          fpop1(Rs1,2#001000011,Rs2,Rd).
fsubs(Rs1, Rs2, Rd) ->          fpop1(Rs1,2#001000101,Rs2,Rd).
fsubd(Rs1, Rs2, Rd) ->          fpop1(Rs1,2#001000110,Rs2,Rd).
fsubq(Rs1, Rs2, Rd) ->          fpop1(Rs1,2#001000111,Rs2,Rd).

fmuls(Rs1, Rs2, Rd) ->          fpop1(Rs1,2#001001001,Rs2,Rd).
fmuld(Rs1, Rs2, Rd) ->          fpop1(Rs1,2#001001010,Rs2,Rd).
fmulq(Rs1, Rs2, Rd) ->          fpop1(Rs1,2#001001011,Rs2,Rd).
fsmuld(Rs1, Rs2, Rd) ->         fpop1(Rs1,2#001101001,Rs2,Rd).
fdmulq(Rs1, Rs2, Rd) ->         fpop1(Rs1,2#001101110,Rs2,Rd).
fdivs(Rs1, Rs2, Rd) ->          fpop1(Rs1,2#001001101,Rs2,Rd).
fdivd(Rs1, Rs2, Rd) ->          fpop1(Rs1,2#001001110,Rs2,Rd).
fdivq(Rs1, Rs2, Rd) ->          fpop1(Rs1,2#001001111,Rs2,Rd).

%% Uses fcc0
fcmps(Rs1, Rs2) ->              fpop2(Rs1,2#001010001,Rs2,0).
fcmpd(Rs1, Rs2) ->              fpop2(Rs1,2#001010010,Rs2,0).
fcmpq(Rs1, Rs2) ->              fpop2(Rs1,2#001010011,Rs2,0).
fcmpes(Rs1, Rs2) ->             fpop2(Rs1,2#001010101,Rs2,0).
fcmped(Rs1, Rs2) ->             fpop2(Rs1,2#001010110,Rs2,0).
fcmpeq(Rs1, Rs2) ->             fpop2(Rs1,2#001010111,Rs2,0).

fcmps(N, Rs1, Rs2) ->           fpcn(N,2#001010001,Rs1,Rs2).
fcmpd(N, Rs1, Rs2) ->           fpcn(N,2#001010010,Rs1,Rs2).
fcmpq(N, Rs1, Rs2) ->           fpcn(N,2#001010011,Rs1,Rs2).
fcmpes(N, Rs1, Rs2) ->          fpcn(N,2#001010101,Rs1,Rs2).
fcmped(N, Rs1, Rs2) ->          fpcn(N,2#001010110,Rs1,Rs2).
fcmpeq(N, Rs1, Rs2) ->          fpcn(N,2#001010111,Rs1,Rs2).

stf(Rd, Rs1, Rs2) ->            format3a(2#11, Rd, 2#100100, Rs1, 0, Rs2).
stfi(Rd, Rs1, Offset) ->        format3b(2#11, Rd, 2#100100, Rs1, Offset).
stdf(Rd, Rs1, Rs2) ->           format3a(2#11, Rd, 2#100111, Rs1, 0, Rs2).
stdfi(Rd, Rs1, Offset) ->       format3b(2#11, Rd, 2#100111, Rs1, Offset).
stqf(Rd, Rs1, Rs2) ->           format3a(2#11, Rd, 2#100110, Rs1, 0, Rs2).
stqfi(Rd, Rs1, Offset) ->       format3b(2#11, Rd, 2#100110, Rs1, Offset).
stfsr(Rd, Rs1, Rs2) ->          format3a(2#11, Rd, 2#100101, Rs1, 0, Rs2).
stfsri(Rd, Rs1, Offset) ->      format3b(2#11, Rd, 2#100101, Rs1, Offset).

ldf(Rd, Rs1, Rs2) ->            format3a(2#11, Rd, 2#100000, Rs1, 0, Rs2).
ldfi(Rd, Rs1, Offset) ->        format3b(2#11, Rd, 2#100000, Rs1, Offset).
lddf(Rd, Rs1, Rs2) ->           format3a(2#11, Rd, 2#100011, Rs1, 0, Rs2).
lddfi(Rd, Rs1, Offset) ->       format3b(2#11, Rd, 2#100011, Rs1, Offset).
ldqf(Rd, Rs1, Rs2) ->           format3a(2#11, Rd, 2#100010, Rs1, 0, Rs2).
ldqfi(Rd, Rs1, Offset) ->       format3b(2#11, Rd, 2#100010, Rs1, Offset).
ldxfsr(Rs1, Rs2) ->             format3a(2#11,  1, 2#100001, Rs1, 0, Rs2).
ldxfsri(Rs1, Offset) ->         format3b(2#11,  1, 2#100001, Rs1, Offset).
  


fpcn(N, Opf, Rs1, Rs2) -> 
  case N of
    0 -> fpc0(Opf, Rs1, Rs2);
    1 -> fpc1(Opf, Rs1, Rs2);
    2 -> fpc2(Opf, Rs1, Rs2);
    3 -> fpc3(Opf, Rs1, Rs2)
  end.
      
fpc0(Opf, Rs1, Rs2) ->          format3c(2#10, 2#00000, 2#110101, Rs1, Opf, Rs2).
fpc1(Opf, Rs1, Rs2) ->          format3c(2#10, 2#00001, 2#110101, Rs1, Opf, Rs2).
fpc2(Opf, Rs1, Rs2) ->          format3c(2#10, 2#00010, 2#110101, Rs1, Opf, Rs2).
fpc3(Opf, Rs1, Rs2) ->          format3c(2#10, 2#00011, 2#110101, Rs1, Opf, Rs2).


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
