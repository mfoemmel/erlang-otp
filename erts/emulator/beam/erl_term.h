/* ``The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved via the world wide web at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */

#ifndef __ERL_TERM_H
#define __ERL_TERM_H

/*
 * Defining ET_DEBUG to 1 causes all type-specific data access
 * macros to perform runtime type checking. This is very useful
 * during development but reduces performance, so ET_DEBUG should
 * be disabled during benchmarking or release.
 */
/* #define ET_DEBUG 1 */
#if defined(DEBUG) && !defined(ET_DEBUG)
#define ET_DEBUG 1
#endif

#if ET_DEBUG
#define _ET_DECLARE_CHECKED(TF,F,TX) extern TF checked_##F(TX,const char*,unsigned)
#define _ET_APPLY(F,X)	checked_##F(X,__FILE__,__LINE__)
#else
#define _ET_DECLARE_CHECKED(TF,F,TX)
#define _ET_APPLY(F,X)	_unchecked_##F(X)
#endif

#define _TAG_PRIMARY_SIZE	2
#define _TAG_PRIMARY_MASK	0x3
#define TAG_PRIMARY_HEADER	0x0
#define TAG_PRIMARY_LIST	0x1
#define TAG_PRIMARY_BOXED	0x2
#define TAG_PRIMARY_IMMED1	0x3

#define primary_tag(x)	((x) & _TAG_PRIMARY_MASK)

#define _TAG_IMMED1_SIZE	4
#define _TAG_IMMED1_MASK	0xF
#define _TAG_IMMED1_PID		((0x0 << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_IMMED1)
#define _TAG_IMMED1_PORT	((0x1 << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_IMMED1)
#define _TAG_IMMED1_IMMED2	((0x2 << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_IMMED1)
#define _TAG_IMMED1_SMALL	((0x3 << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_IMMED1)

#define _TAG_IMMED2_SIZE	6
#define _TAG_IMMED2_MASK	0x3F
#define _TAG_IMMED2_ATOM	((0x0 << _TAG_IMMED1_SIZE) | _TAG_IMMED1_IMMED2)
#define _TAG_IMMED2_CATCH	((0x1 << _TAG_IMMED1_SIZE) | _TAG_IMMED1_IMMED2)
#define _TAG_IMMED2_NIL		((0x3 << _TAG_IMMED1_SIZE) | _TAG_IMMED1_IMMED2)

/*
 * HEADER representation:
 *
 *	aaaaaaaaaaaaaaaaaaaaaaaaaatttt00	arity:26, tag:4
 *
 * HEADER tags:
 *
 *	0000	ARITYVAL
 *      0001    VECTOR
 *	001x	BIGNUM with sign bit		|
 *	0100	REF				|
 *	0101	FUN				| THINGS
 *	0110	FLONUM				|
 *	1000	REFC_BINARY	|		|
 *	1001	HEAP_BINARY	| BINARIES	|
 *	1010	SUB_BINARY	|		|
 *
 * COMMENTS:
 *
 * - The tag is zero for arityval and non-zero for thing headers.
 * - A single bit differentiates between positive and negative bignums.
 * - A single bit differentiates between binaries and non-binaries.
 *
 * XXX: globally replace XXX_SUBTAG with TAG_HEADER_XXX
 */
#define ARITYVAL_SUBTAG		(0x0 << _TAG_PRIMARY_SIZE) /* TUPLE */
#define VECTOR_SUBTAG		(0x1 << _TAG_PRIMARY_SIZE) /* VECTOR */
#define POS_BIG_SUBTAG		(0x2 << _TAG_PRIMARY_SIZE) /* BIG: tags 2&3 */
#define NEG_BIG_SUBTAG		(0x3 << _TAG_PRIMARY_SIZE) /* BIG: tags 2&3 */
#define _BIG_SIGN_BIT		(0x1 << _TAG_PRIMARY_SIZE)
#define REF_SUBTAG		(0x4 << _TAG_PRIMARY_SIZE) /* REF */
#define FUN_SUBTAG		(0x5 << _TAG_PRIMARY_SIZE) /* FUN */
#define FLOAT_SUBTAG		(0x6 << _TAG_PRIMARY_SIZE) /* FLOAT */
#define _BINARY_XXX_MASK	(0x7 << _TAG_PRIMARY_SIZE)
#define REFC_BINARY_SUBTAG	(0x8 << _TAG_PRIMARY_SIZE) /* BINARY */
#define HEAP_BINARY_SUBTAG	(0x9 << _TAG_PRIMARY_SIZE) /* BINARY */
#define SUB_BINARY_SUBTAG	(0xA << _TAG_PRIMARY_SIZE) /* BINARY */

#define _TAG_HEADER_ARITYVAL	(TAG_PRIMARY_HEADER|ARITYVAL_SUBTAG)
#define _TAG_HEADER_VECTOR	(TAG_PRIMARY_HEADER|VECTOR_SUBTAG)
#define _TAG_HEADER_FUN		(TAG_PRIMARY_HEADER|FUN_SUBTAG)
#define _TAG_HEADER_POS_BIG	(TAG_PRIMARY_HEADER|POS_BIG_SUBTAG)
#define _TAG_HEADER_NEG_BIG	(TAG_PRIMARY_HEADER|NEG_BIG_SUBTAG)
#define _TAG_HEADER_FLOAT	(TAG_PRIMARY_HEADER|FLOAT_SUBTAG)
#define _TAG_HEADER_REF		(TAG_PRIMARY_HEADER|REF_SUBTAG)
#define _TAG_HEADER_REFC_BIN	(TAG_PRIMARY_HEADER|REFC_BINARY_SUBTAG)
#define _TAG_HEADER_HEAP_BIN	(TAG_PRIMARY_HEADER|HEAP_BINARY_SUBTAG)
#define _TAG_HEADER_SUB_BIN	(TAG_PRIMARY_HEADER|SUB_BINARY_SUBTAG)

#define _TAG_HEADER_MASK	0x3F
#define _HEADER_SUBTAG_MASK	0x3C	/* 4 bits for subtag */
#define _HEADER_ARITY_OFFS	6

#define header_is_transparent(x) \
 (((x) & (_HEADER_SUBTAG_MASK-VECTOR_SUBTAG)) == ARITYVAL_SUBTAG)
#define header_is_arityval(x)	(((x) & _HEADER_SUBTAG_MASK) == ARITYVAL_SUBTAG)
#define header_is_thing(x)	(!header_is_transparent((x)))

#define _CPMASK		0x3

/* immediate object access methods */
#define is_immed(x)		(((x) & _TAG_PRIMARY_MASK) == TAG_PRIMARY_IMMED1)
#define is_not_immed(x)		(!is_immed((x)))
#define IS_CONST(x)		is_immed((x))
#define is_not_both_immed(x,y)	is_not_immed(((x)&(y)))

/* boxed object access methods */
#define _is_aligned(x)		(((Uint)(x) & 0x3) == 0)
#define _unchecked_make_boxed(x)	((Uint)(x) + TAG_PRIMARY_BOXED)
_ET_DECLARE_CHECKED(Eterm,make_boxed,Eterm*);
#define make_boxed(x)		_ET_APPLY(make_boxed,(x))
#if 1
#define _is_not_boxed(x)	((x) & (_TAG_PRIMARY_MASK-TAG_PRIMARY_BOXED))
#define _unchecked_is_boxed(x)	(!_is_not_boxed((x)))
_ET_DECLARE_CHECKED(int,is_boxed,Eterm);
#define is_boxed(x)		_ET_APPLY(is_boxed,(x))
#else
#define is_boxed(x)		(((x) & _TAG_PRIMARY_MASK) == TAG_PRIMARY_BOXED)
#endif
#define _unchecked_boxed_val(x) ((Eterm*)((x) - TAG_PRIMARY_BOXED))
_ET_DECLARE_CHECKED(Eterm*,boxed_val,Eterm);
#define boxed_val(x)		_ET_APPLY(boxed_val,(x))

/* cons cell ("list") access methods */
#define _unchecked_make_list(x)	((Uint)(x) + TAG_PRIMARY_LIST)
_ET_DECLARE_CHECKED(Eterm,make_list,Eterm*);
#define make_list(x)		_ET_APPLY(make_list,(x))
#if 1
#define _unchecked_is_not_list(x) ((x) & (_TAG_PRIMARY_MASK-TAG_PRIMARY_LIST))
_ET_DECLARE_CHECKED(int,is_not_list,Eterm);
#define is_not_list(x)		_ET_APPLY(is_not_list,(x))
#define is_list(x)		(!is_not_list((x)))
#else
#define is_list(x)		(((x) & _TAG_PRIMARY_MASK) == TAG_PRIMARY_LIST)
#define is_not_list(x)		(!is_list((x)))
#endif
#define _unchecked_list_val(x)	((Eterm*)((x) - TAG_PRIMARY_LIST))
_ET_DECLARE_CHECKED(Eterm*,list_val,Eterm);
#define list_val(x)		_ET_APPLY(list_val,(x))

#define CONS(hp, car, cdr) \
        (*(hp) = (car), \
         *((hp)+1) = (cdr), \
          make_list(hp))

#define CAR(x)  *(x)
#define CDR(x)  *((x)+1)

/* generic tagged pointer (boxed or list) access methods */
#define _unchecked_ptr_val(x)	((Eterm*)((x) & ~0x3))
#define ptr_val(x)		_unchecked_ptr_val((x))	/*XXX*/
#define _unchecked_offset_ptr(x,offs)	((x)+((offs)*sizeof(Eterm)))
#define offset_ptr(x,offs)	_unchecked_offset_ptr(x,offs)	/*XXX*/

/* fixnum ("small") access methods */
#define SMALL_BITS	(28)
#define SMALL_DIGITS	(8)
#define MAX_SMALL	((1 << (SMALL_BITS-1))-1)
#define MIN_SMALL	(-(1 << (SMALL_BITS-1)))
#define make_small(x)	(((x) << _TAG_IMMED1_SIZE) + _TAG_IMMED1_SMALL)
#define is_small(x)	(((x) & _TAG_IMMED1_MASK) == _TAG_IMMED1_SMALL)
#define is_not_small(x)	(!is_small((x)))
#define is_byte(x)	(((x) & 0xFFFFF00F) == _TAG_IMMED1_SMALL)
#define MY_IS_SSMALL(x) (((Uint) (((x) >> (SMALL_BITS-1)) + 1)) < 2)
#define _unchecked_unsigned_val(x)	((x) >> _TAG_IMMED1_SIZE)
_ET_DECLARE_CHECKED(Uint,unsigned_val,Eterm);
#define unsigned_val(x)	_ET_APPLY(unsigned_val,(x))
#define _unchecked_signed_val(x)	((Sint)(x) >> _TAG_IMMED1_SIZE)
_ET_DECLARE_CHECKED(Sint,signed_val,Eterm);
#define signed_val(x)	_ET_APPLY(signed_val,(x))

#if _TAG_IMMED1_SMALL == 0x0F
#define is_both_small(x,y) (((x) & (y) & _TAG_IMMED1_MASK) == _TAG_IMMED1_SMALL)
#elif _TAG_IMMED1_SMALL == 0x00
#define is_both_small(x,y) ((((x)|(y)) & _TAG_IMMED1_MASK) == _TAG_IMMED1_SMALL)
#else
#define is_both_small(x,y) (is_small(x) && is_small(y))
#endif

/* NIL access methods */
#define NIL		((~0 << _TAG_IMMED2_SIZE) | _TAG_IMMED2_NIL)
#define is_nil(x)	((x) == NIL)
#define is_not_nil(x)	((x) != NIL)

/* atom access methods */
#define make_atom(x)	(((x) << _TAG_IMMED2_SIZE) + _TAG_IMMED2_ATOM)
#define is_atom(x)	(((x) & _TAG_IMMED2_MASK) == _TAG_IMMED2_ATOM)
#define is_not_atom(x)	(!is_atom(x))
#define _unchecked_atom_val(x)	((x) >> _TAG_IMMED2_SIZE)
_ET_DECLARE_CHECKED(Uint,atom_val,Eterm);
#define atom_val(x)	_ET_APPLY(atom_val,(x))

/* header (arityval or thing) access methods */
#define _make_header(sz,tag)	(((sz) << _HEADER_ARITY_OFFS) + (tag))
#define is_header(x)	(((x) & _TAG_PRIMARY_MASK) == TAG_PRIMARY_HEADER)
#define _unchecked_header_arity(x)	((x) >> _HEADER_ARITY_OFFS)
_ET_DECLARE_CHECKED(Uint,header_arity,Eterm);
#define header_arity(x)	_ET_APPLY(header_arity,(x))

/* arityval access methods */
#define make_arityval(sz)	_make_header((sz),_TAG_HEADER_ARITYVAL)
#define is_arity_value(x)	(((x) & _TAG_HEADER_MASK) == _TAG_HEADER_ARITYVAL)
#define is_not_arity_value(x)	(!is_arity_value((x)))
#define _unchecked_arityval(x)	_unchecked_header_arity((x))
_ET_DECLARE_CHECKED(Uint,arityval,Eterm);
#define arityval(x)		_ET_APPLY(arityval,(x))

/* thing access methods */
#define is_thing(x)	(is_header((x)) && header_is_thing((x)))
#define _unchecked_thing_arityval(x)	_unchecked_header_arity((x))
_ET_DECLARE_CHECKED(Uint,thing_arityval,Eterm);
#define thing_arityval(x)	_ET_APPLY(thing_arityval,(x))
#define _unchecked_thing_subtag(x)	((x) & _HEADER_SUBTAG_MASK)
_ET_DECLARE_CHECKED(Uint,thing_subtag,Eterm);
#define thing_subtag(x)		_ET_APPLY(thing_subtag,(x))

/*
 * Magic non-value object.
 * Used as function return error and "absent value" indicator
 * in the original runtime system. The new runtime system also
 * uses it as forwarding marker for CONS cells.
 *
 * This value is 0 in the original runtime system, which unfortunately
 * promotes sloppy programming practices. It also prevents some useful
 * tag assignment schemes, e.g. using a 2-bit tag 00 for FIXNUM.
 *
 * To help find code which makes unwarranted assumptions about zero,
 * we now use a non-zero bit-pattern in debug mode.
 */
#if ET_DEBUG
#define THE_NON_VALUE	_make_header(0,_TAG_HEADER_FLOAT)
#else
#define THE_NON_VALUE	(0)
#endif
#define is_non_value(x)	((x) == THE_NON_VALUE)
#define is_value(x)	((x) != THE_NON_VALUE)

/* binary object access methods */
#define is_binary_header(x)	(((x) & (_TAG_HEADER_MASK-_BINARY_XXX_MASK)) == _TAG_HEADER_REFC_BIN)
#define make_binary(x)	make_boxed((Eterm*)(x))
#define is_binary(x)	(is_boxed((x)) && is_binary_header(*boxed_val((x))))
#define is_not_binary(x) (!is_binary((x)))
#define _unchecked_binary_val(x) _unchecked_boxed_val((x))
_ET_DECLARE_CHECKED(Eterm*,binary_val,Eterm);
#define binary_val(x)	_ET_APPLY(binary_val,(x))

/* process binaries stuff (special case of binaries) */
#define HEADER_PROC_BIN	_make_header(PROC_BIN_SIZE-1,_TAG_HEADER_REFC_BIN)

/* fun objects */
#define HEADER_FUN		_make_header(ERL_FUN_SIZE-1,_TAG_HEADER_FUN)
#define is_fun_header(x)	(((x) & _TAG_HEADER_MASK) == _TAG_HEADER_FUN)
#define make_fun(x)		make_boxed((Eterm*)(x))
#define is_fun(x)		(is_boxed((x)) && is_fun_header(*boxed_val((x))))
#define is_not_fun(x)		(!is_fun((x)))
#define _unchecked_fun_val(x)   _unchecked_boxed_val((x))
_ET_DECLARE_CHECKED(Eterm*,fun_val,Eterm);
#define fun_val(x)		_ET_APPLY(fun_val,(x))

/* bignum access methods */
#define make_pos_bignum_header(sz)	_make_header((sz),_TAG_HEADER_POS_BIG)
#define make_neg_bignum_header(sz)	_make_header((sz),_TAG_HEADER_NEG_BIG)
#define _is_bignum_header(x)	(((x) & (_TAG_HEADER_MASK-_BIG_SIGN_BIT)) == _TAG_HEADER_POS_BIG)
#define _unchecked_bignum_header_is_neg(x)	((x) & _BIG_SIGN_BIT)
_ET_DECLARE_CHECKED(int,bignum_header_is_neg,Eterm);
#define bignum_header_is_neg(x)	_ET_APPLY(bignum_header_is_neg,(x))
#define _unchecked_bignum_header_neg(x)	((x) | _BIG_SIGN_BIT)
_ET_DECLARE_CHECKED(Eterm,bignum_header_neg,Eterm);
#define bignum_header_neg(x)	_ET_APPLY(bignum_header_neg,(x))
#define _unchecked_bignum_header_arity(x)	_unchecked_header_arity((x))
_ET_DECLARE_CHECKED(Uint,bignum_header_arity,Eterm);
#define bignum_header_arity(x)	_ET_APPLY(bignum_header_arity,(x))
#define BIG_ARITY_MAX		((1 << 19)-1)
#define make_big(x)	make_boxed((x))
#define is_big(x)	(is_boxed((x)) && _is_bignum_header(*boxed_val((x))))
#define is_not_big(x)	(!is_big((x)))
#define _unchecked_big_val(x)	_unchecked_boxed_val((x))
_ET_DECLARE_CHECKED(Eterm*,big_val,Eterm);
#define big_val(x)		_ET_APPLY(big_val,(x))

/* flonum ("float") access methods */
#define HEADER_FLONUM	_make_header(2,_TAG_HEADER_FLOAT)
#define make_float(x)	make_boxed((x))
#define is_float(x)	(is_boxed((x)) && *boxed_val((x)) == HEADER_FLONUM)
#define is_not_float(x)	(!is_float(x))
#define _unchecked_float_val(x)	_unchecked_boxed_val((x))
_ET_DECLARE_CHECKED(Eterm*,float_val,Eterm);
#define float_val(x)	_ET_APPLY(float_val,(x))

/* Float definition for byte and word access */
typedef double ieee754_8;

typedef union float_def
{
    ieee754_8 fd;
    byte   fb[sizeof(ieee754_8)];
    Uint16 fs[sizeof(ieee754_8) / sizeof(Uint16)];
    Uint32 fw[sizeof(ieee754_8) / sizeof(Uint32)];
} FloatDef;

#define GET_DOUBLE(x, f) (f).fw[0] = *(float_val(x)+1), \
                         (f).fw[1] = *(float_val(x)+2)

#define PUT_DOUBLE(f, x)  *(x) = HEADER_FLONUM, \
                          *((x)+1) = (f).fw[0], \
			  *((x)+2) = (f).fw[1]

/* tuple access methods */
#define make_tuple(x)	make_boxed((x))
#define is_tuple(x)	(is_boxed((x)) && is_arity_value(*boxed_val((x))))
#define is_not_tuple(x)	(!is_tuple((x)))
#define _unchecked_tuple_val(x)	_unchecked_boxed_val(x)
_ET_DECLARE_CHECKED(Eterm*,tuple_val,Eterm);
#define tuple_val(x)	_ET_APPLY(tuple_val,(x))

#define TUPLE0(t) \
        ((t)[0] = make_arityval(0), \
        make_tuple(t))
#define TUPLE1(t,e1) \
        ((t)[0] = make_arityval(1), \
        (t)[1] = (e1), \
        make_tuple(t))
#define TUPLE2(t,e1,e2) \
        ((t)[0] = make_arityval(2), \
        (t)[1] = (e1), \
        (t)[2] = (e2), \
        make_tuple(t))
#define TUPLE3(t,e1,e2,e3) \
        ((t)[0] = make_arityval(3), \
        (t)[1] = (e1), \
        (t)[2] = (e2), \
        (t)[3] = (e3), \
        make_tuple(t))
#define TUPLE4(t,e1,e2,e3,e4) \
        ((t)[0] = make_arityval(4), \
        (t)[1] = (e1), \
        (t)[2] = (e2), \
        (t)[3] = (e3), \
        (t)[4] = (e4), \
        make_tuple(t))
#define TUPLE5(t,e1,e2,e3,e4,e5) \
        ((t)[0] = make_arityval(5), \
        (t)[1] = (e1), \
        (t)[2] = (e2), \
        (t)[3] = (e3), \
        (t)[4] = (e4), \
        (t)[5] = (e5), \
        make_tuple(t))
#define TUPLE6(t,e1,e2,e3,e4,e5,e6) \
        ((t)[0] = make_arityval(6), \
        (t)[1] = (e1), \
        (t)[2] = (e2), \
        (t)[3] = (e3), \
        (t)[4] = (e4), \
        (t)[5] = (e5), \
	(t)[6] = (e6), \
        make_tuple(t))

#define TUPLE7(t,e1,e2,e3,e4,e5,e6,e7) \
        ((t)[0] = make_arityval(7), \
        (t)[1] = (e1), \
        (t)[2] = (e2), \
        (t)[3] = (e3), \
        (t)[4] = (e4), \
        (t)[5] = (e5), \
	(t)[6] = (e6), \
	(t)[7] = (e7), \
        make_tuple(t))

#define TUPLE8(t,e1,e2,e3,e4,e5,e6,e7,e8) \
        ((t)[0] = make_arityval(8), \
        (t)[1] = (e1), \
        (t)[2] = (e2), \
        (t)[3] = (e3), \
        (t)[4] = (e4), \
        (t)[5] = (e5), \
	(t)[6] = (e6), \
	(t)[7] = (e7), \
	(t)[8] = (e8), \
        make_tuple(t))

/* pid layout
**
**    Serial  Number     Node    Creat  Tag 
**   +---------------------------------------+
**   |  3  |  15       |   8    |   2 |  4   |
**   +---------------------------------------+
*/
#define _PID_SERIAL_BITS	3
#define _PID_NODE_BITS   	8
#define _PID_CREAT_BITS  	2
#define _PID_NUMBER_BITS 	15

#define MAX_NODE	(1 << _PID_NODE_BITS)

/* Minimum NUMBER of processes for a small system to start */
#define MIN_PROCESS	16

/* Maximum NUMBER of process identifiers */
#define MAX_PROCESS	(1 << _PID_NUMBER_BITS)

/* Maximum NUMBER of serial numbers */
#define MAX_SERIAL	(1 << _PID_SERIAL_BITS)

/* MAX value for the creation field in pid, port and reference */
#define MAX_CREATION	(1 << _PID_CREAT_BITS)

#define make_pid3(Ser,Node,Number,Creation) \
  ((Eterm)(((Ser)<<29)|((Number)<<14)|((Node)<<6)|((Creation)<<4)|_TAG_IMMED1_PID))

#define make_pid(Ser,Node,Number) \
  make_pid3(Ser,Node,Number,ORIG_CREATION)

#define is_pid(x)	(((x) & _TAG_IMMED1_MASK) == _TAG_IMMED1_PID)
#define is_not_pid(x)	(!is_pid(x))

#define _GETBITS(X,Pos,Size) (((X) >> (Pos)) & ~(~0 << (Size)))

/* This macro get Size bits starting at low order position Pos
   and adjusts the bits to the right 
   bits are numbered from 1 - 32 */

#define _unchecked_pid_serial(x)	_GETBITS(x,29,_PID_SERIAL_BITS)
_ET_DECLARE_CHECKED(Uint,pid_serial,Eterm);
#define pid_serial(x)	_ET_APPLY(pid_serial,(x))

#define _unchecked_pid_number(x)	_GETBITS(x,14,_PID_NUMBER_BITS)
_ET_DECLARE_CHECKED(Uint,pid_number,Eterm);
#define pid_number(x)	_ET_APPLY(pid_number,(x))

#define _unchecked_pid_node(x)		_GETBITS(x,6,_PID_NODE_BITS)
_ET_DECLARE_CHECKED(Uint,pid_node,Eterm);
#define pid_node(x)	_ET_APPLY(pid_node,(x))

#define _unchecked_pid_creation(x)	_GETBITS(x,4,_PID_CREAT_BITS)
_ET_DECLARE_CHECKED(Uint,pid_creation,Eterm);
#define pid_creation(x)	_ET_APPLY(pid_creation,(x))

/* port layout
**
**  Node   Number           Creat   Tag
** +------------------------------------+
** |   8   |  18              | 2 |  4  |
** +------------------------------------+
*/
#define PORT_NUMBER_BITS	18

/* Highest port-ID part in a term of type Port 
   Not necessarily the same as the variable erl_max_ports
   which defines the maximum number of simultaneous Ports
   in the Erlang node. MAX_PORT is a hard upper limit.
*/
#define MAX_PORT       (1 << PORT_NUMBER_BITS)

#define make_port3(Node,Number,Creation) \
 ((Eterm)(((Node)<<24)|((Number)<<6)|((Creation)<<4)|_TAG_IMMED1_PORT))
#define make_port2(Node,Number) make_port3(Node,Number,ORIG_CREATION)

#define is_port(x)	(((x) & _TAG_IMMED1_MASK) == _TAG_IMMED1_PORT)
#define is_not_port(x)	(!is_port(x))

#define _unchecked_port_node(x)		_GETBITS(x,24,_PID_NODE_BITS)
_ET_DECLARE_CHECKED(Uint,port_node,Eterm);
#define port_node(x)	_ET_APPLY(port_node,(x))

#define _unchecked_port_number(x)	_GETBITS(x,6,PORT_NUMBER_BITS)
_ET_DECLARE_CHECKED(Uint,port_number,Eterm);
#define port_number(x)	_ET_APPLY(port_number,(x))

#define _unchecked_port_creation(x)	_unchecked_pid_creation(x)
_ET_DECLARE_CHECKED(Uint,port_creation,Eterm);
#define port_creation(x)	_ET_APPLY(port_creation,(x))

#define _is_pid_or_port(x)	(is_pid(x) || is_port(x))
#define _unchecked_pid_or_port_creation(x)	_unchecked_pid_creation(x)
_ET_DECLARE_CHECKED(Uint,pid_or_port_creation,Eterm);
#define pid_or_port_creation(x)	_ET_APPLY(pid_or_port_creation,(x))

/* refhead layout (a.k.a. "old refs")
**
**  Node   Number           Creat   Tag
** +------------------------------------+
** |   8   |  18              | 2 |  4  |
** +------------------------------------+
*/
#define _REF_NUMBER_BITS	18

/* Maximum number of references in the system */
#define MAX_REFERENCE	(1 << _REF_NUMBER_BITS)

/*XXX: don't actually need a tag */
#define make_ref3(Node,Number,Creation) \
 ((Uint)(((Node)<<24) | ((Number)<<6) | ((Creation)<<4) | _TAG_IMMED1_PORT))

#define make_ref2(Node,Number) make_ref3(Node,Number,ORIG_CREATION)

#define _refhead_node(x)	_GETBITS(x,24,_PID_NODE_BITS)
#define _refhead_creation(x)	_GETBITS(x,4,_PID_CREAT_BITS)

/* ref layout (a.k.a. "new refs")
 *
 *	+---------------+
 * -->	| thing word	|
 *	|---------------|
 *	| ref head	|
 *	|---------------|
 *	| word 0	|
 *	| word 1	|
 *	| word 2	|
 *	+---------------+
 */
#define REF_WORDS	3

typedef struct {
    Eterm t;			/* thing word */
    Uint h;			/* "head", like an old ref (NOT Eterm!) */
    Uint w[REF_WORDS];
} Ref;

#define make_ref_header(sz)	_make_header((sz),_TAG_HEADER_REF)
#define _is_ref_header(x)	(((x) & _TAG_HEADER_MASK) == _TAG_HEADER_REF)
#define make_ref(x)	make_boxed((Eterm*)(x))
#define is_ref(x)	(is_boxed((x)) && _is_ref_header(*boxed_val((x))))
#define is_not_ref(x)	(!is_ref((x)))
#define _unchecked_ref_val(x)	_unchecked_boxed_val((x))
_ET_DECLARE_CHECKED(Eterm*,ref_val,Eterm);
#define ref_val(x)	_ET_APPLY(ref_val,(x))

/* Ref *r1, *r2; */
#define eqref(r1,r2)	(eq(make_ref(r1), make_ref(r2)))

#define ref_ptr(x)	((Ref*)ref_val(x))
#define REF_ARITY(xp)	thing_arityval(*(xp))
#define ref_arity(x)	REF_ARITY(ref_val(x))

#define ref_node(x)	_refhead_node(ref_ptr(x)->h)
#define ref_creation(x)	_refhead_creation(ref_ptr(x)->h)
#define ref_number(x)	(ref_ptr(x)->w[0])

/* number tests */

#define is_integer(x)		(is_small(x) || is_big(x))
#define is_not_integer(x)	(!is_integer(x))
#define is_number(x)		(is_integer(x) || is_float(x))

#define SMALL_MINUS_ONE	make_small(-1)
#define SMALL_ZERO	make_small(0)
#define SMALL_ONE	make_small(1)

#define ENULL		0

/* on some architectures CP contains labels which are not aligned */
#ifdef NOT_ALIGNED
#error "fix yer arch, like"
#endif

#define _unchecked_make_cp(x)	((Eterm)(x))
_ET_DECLARE_CHECKED(Eterm,make_cp,Uint*);
#define make_cp(x)	_ET_APPLY(make_cp,(x))

#define is_not_CP(x)	((x) & _CPMASK)
#define is_CP(x)	(!is_not_CP(x))

#define _unchecked_cp_val(x)	((Uint*)(x))
_ET_DECLARE_CHECKED(Uint*,cp_val,Eterm);
#define cp_val(x)	_ET_APPLY(cp_val,(x))

#define make_catch(x)	(((x) << _TAG_IMMED2_SIZE) | _TAG_IMMED2_CATCH)
#define is_catch(x)	(((x) & _TAG_IMMED2_MASK) == _TAG_IMMED2_CATCH)
#define is_not_catch(x)	(!is_catch(x))
#define _unchecked_catch_val(x)	((x) >> _TAG_IMMED2_SIZE)
_ET_DECLARE_CHECKED(Uint,catch_val,Eterm);
#define catch_val(x)	_ET_APPLY(catch_val,(x))

#define make_blank(X)	((X) = NIL)


/* vector object access methods */
#define HEADER_VECTOR		_make_header(4,_TAG_HEADER_VECTOR)
#define make_vector(x)		make_boxed((Eterm*)(x))
#define is_vector_header(x) (((x) & _TAG_HEADER_MASK) == _TAG_HEADER_VECTOR)
#define is_vector(x) (is_boxed((x)) && is_vector_header(*boxed_val((x))))
#define is_not_vector(x) (!is_vector((x)))
#define _unchecked_vector_val(x)   _unchecked_boxed_val((x))
_ET_DECLARE_CHECKED(Eterm*,vector_val,Eterm);
#define vector_val(x)		_ET_APPLY(vector_val,(x))
#define _unchecked_vector_arity(x)   _unchecked_header_arity((x))
_ET_DECLARE_CHECKED(Uint,vector_arity,Eterm);
#define vector_arity(x)		_ET_APPLY(vector_arity,(x))

/*
 * Overloaded tags.
 *
 * SMALL = 15
 * ATOM/NIL=7
 *
 * Note that the two least significant bits in SMALL/ATOM/NIL always are 3;
 * thus, we can distinguish register from literals by looking at only these
 * two bits.
 */

#define X_REG_DEF	0
#define Y_REG_DEF	1
#define R_REG_DEF	2

#define beam_reg_tag(x)	((x) & 3)

#define make_rreg()	R_REG_DEF
#define make_xreg(ix)	(((ix) * sizeof(Eterm)) | X_REG_DEF)
#define make_yreg(ix)	(((ix) * sizeof(Eterm)) | Y_REG_DEF)

#define _is_xreg(x)	(beam_reg_tag(x) == X_REG_DEF)
#define _is_yreg(x)	(beam_reg_tag(x) == Y_REG_DEF)

#define _unchecked_x_reg_offset(R)	((R) - X_REG_DEF)
_ET_DECLARE_CHECKED(Uint,x_reg_offset,Uint);
#define x_reg_offset(R)	_ET_APPLY(x_reg_offset,(R))

#define _unchecked_y_reg_offset(R)	((R) - Y_REG_DEF)
_ET_DECLARE_CHECKED(Uint,y_reg_offset,Uint);
#define y_reg_offset(R)	_ET_APPLY(y_reg_offset,(R))

#define reg_index(R) ((R) / sizeof(Eterm))

#define _unchecked_x_reg_index(R)	((R) >> 2)
_ET_DECLARE_CHECKED(Uint,x_reg_index,Uint);
#define x_reg_index(R)	_ET_APPLY(x_reg_index,(R))

#define _unchecked_y_reg_index(R)	((R) >> 2)
_ET_DECLARE_CHECKED(Uint,y_reg_index,Uint);
#define y_reg_index(R)	_ET_APPLY(y_reg_index,(R))

/*
 * Backwards compatibility definitions:
 * - #define virtal *_DEF constants with values that fit term order:
 *   number < atom < ref < fun < port < pid < tuple < nil < cons < binary
 * - tag_val_def() function generates virtual _DEF tag
 * - not_eq_tags() and NUMBER_CODE() defined in terms
 *   of the tag_val_def() function
 */

#define BINARY_DEF	0
#define LIST_DEF	1
#define NIL_DEF		2
#define VECTOR_DEF      3
#define TUPLE_DEF	4
#define PID_DEF		5
#define PORT_DEF	6
#define FUN_DEF		7
#define REF_DEF		8
#define ATOM_DEF	9
#define FLOAT_DEF	10
#define BIG_DEF		11
#define SMALL_DEF	12

#if ET_DEBUG
extern unsigned tag_val_def_debug(Eterm, const char*, unsigned);
#define tag_val_def(x)	tag_val_def_debug((x),__FILE__,__LINE__)
#else
extern unsigned tag_val_def(Eterm);
#endif
#define not_eq_tags(X,Y)	(tag_val_def((X)) ^ tag_val_def((Y)))

#define NUMBER_CODE(x,y)	((tag_val_def(x) << 4) | tag_val_def(y))
#define _NUMBER_CODE(TX,TY)	((TX << 4) | TY)
#define SMALL_SMALL	_NUMBER_CODE(SMALL_DEF,SMALL_DEF)
#define SMALL_BIG 	_NUMBER_CODE(SMALL_DEF,BIG_DEF)
#define SMALL_FLOAT 	_NUMBER_CODE(SMALL_DEF,FLOAT_DEF)
#define BIG_SMALL 	_NUMBER_CODE(BIG_DEF,SMALL_DEF)
#define BIG_BIG 	_NUMBER_CODE(BIG_DEF,BIG_DEF)
#define BIG_FLOAT 	_NUMBER_CODE(BIG_DEF,FLOAT_DEF)
#define FLOAT_SMALL 	_NUMBER_CODE(FLOAT_DEF,SMALL_DEF)
#define FLOAT_BIG 	_NUMBER_CODE(FLOAT_DEF,BIG_DEF)
#define FLOAT_FLOAT	_NUMBER_CODE(FLOAT_DEF,FLOAT_DEF)

#endif	/* __ERL_TERM_H */
