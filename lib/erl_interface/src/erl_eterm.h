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
#ifndef _ERL_ETERM_H
#define _ERL_ETERM_H

#ifndef SILENT
#include <stdio.h>
#endif

#include "portability.h"

__ERL_BEGIN_DECL

#define ERL_COMPOUND (1 << 7)

#define ERL_UNDEF        0
#define ERL_INTEGER      1
#define ERL_U_INTEGER    2 /* unsigned int */
#define ERL_ATOM         3
#define ERL_PID          4
#define ERL_PORT         5
#define ERL_REF          6
#define ERL_CONS         (7 | ERL_COMPOUND)
#define ERL_LIST         ERL_CONS
#define ERL_NIL          8
#define ERL_EMPTY_LIST   ERL_NIL
#define ERL_TUPLE        (9 | ERL_COMPOUND)
#define ERL_BINARY      10
#define ERL_FLOAT       11
#define ERL_VARIABLE    (12 | ERL_COMPOUND) /* used in patterns */
#define ERL_SMALL_BIG   13
#define ERL_U_SMALL_BIG 14
#define ERL_FUNCTION    (15 | ERL_COMPOUND)
#define ERL_BIG         16

/*  Erlang terms in C  */

typedef struct _header {
  unsigned int count:24;	/* reference counter */
  unsigned int type:8;		/* type of Erlang term */
} Erl_Header;

typedef struct _integer {
  Erl_Header h;
  int i;
} Erl_Integer;

typedef struct _uinteger {
  Erl_Header h;
  unsigned int u;
} Erl_Uinteger;

typedef struct _float {
  Erl_Header h;
  double f;
} Erl_Float;

typedef struct _atom {
  Erl_Header h;
  int len;
  char *a;
} Erl_Atom;

typedef struct _pid {
  Erl_Header h;
  char * node;
  unsigned int number;
  unsigned int serial;
  unsigned char creation;
} Erl_Pid;

typedef struct _port {    
  Erl_Header h;
  char * node;
  unsigned int number;
  unsigned char creation;
} Erl_Port;

typedef struct _ref {
  Erl_Header h;
  char * node;
  int len;
  unsigned int n[3];
  unsigned char creation;
} Erl_Ref;

struct _eterm; /* forward */    

typedef struct _list {
  Erl_Header h;
  struct _eterm *head;
  struct _eterm *tail;
} Erl_List;

typedef struct _empty_list {
  Erl_Header h;
} Erl_EmptyList;

typedef struct _tuple {
  Erl_Header h;
  int size;
  struct _eterm **elems;
} Erl_Tuple;

typedef struct _binary {
  Erl_Header h;
  int size;
  unsigned char *b;
} Erl_Binary;

typedef struct _big {
  Erl_Header h;
  int arity;
  int is_neg;
  unsigned short *digits;
} Erl_Big;

/* Variables may only exist in patterns. 
 * Note: identical variable names in a pattern 
 * denotes the same value.
 */
typedef struct _variable {    
  Erl_Header h;
  int len;           
  char *name;        
  struct _eterm *v;  
} Erl_Variable;


typedef struct _function {
    Erl_Header h;
    int size;			/* size of closure */
    int arity;			/* arity for new (post R7) external funs */
    char md5[16];		/* md5 for new funs */
    int new_index;		/* new funs */
    struct _eterm*  creator;	/* pid */
    struct _eterm*  module;	/* module */
    struct _eterm*  index;
    struct _eterm*  uniq;
    struct _eterm** closure;
} Erl_Function;


typedef struct _eterm {
  union {
    Erl_Integer   ival;
    Erl_Uinteger  uival; 
    Erl_Float     fval;
    Erl_Atom      aval;
    Erl_Pid       pidval;     
    Erl_Port      portval;    
    Erl_Ref       refval;   
    Erl_List      lval;
    Erl_EmptyList nval;
    Erl_Tuple     tval;
    Erl_Binary    bval;
    Erl_Variable  vval;
    Erl_Function  funcval;
    Erl_Big       bigval;
  } uval;
} ETERM;

#define ERL_MAX_COUNT     0xffffff
#define ERL_HEADER(x)     ((Erl_Header *)x)
#define ERL_COUNT(x)      (ERL_HEADER(x)->count)
#define ERL_TYPE(x)       (ERL_HEADER(x)->type)

#define ERL_MAX ((1 << 27)-1)
#define ERL_MIN -(1 << 27)

/*
 * Macros used for retrieving values from Erlang terms.
 */

#define ERL_INT_VALUE(x) ((x)->uval.ival.i)
#define ERL_INT_UVALUE(x) ((x)->uval.uival.u)

#define ERL_FLOAT_VALUE(x) ((x)->uval.fval.f)

#define ERL_ATOM_PTR(x) ((x)->uval.aval.a)
#define ERL_ATOM_SIZE(x) ((x)->uval.aval.len)

#define ERL_PID_NODE(x) ((x)->uval.pidval.node)
#define ERL_PID_NUMBER(x) ((x)->uval.pidval.number)
#define ERL_PID_SERIAL(x) ((x)->uval.pidval.serial)
#define ERL_PID_CREATION(x) ((x)->uval.pidval.creation)

#define ERL_PORT_NODE(x) ((x)->uval.portval.node)
#define ERL_PORT_NUMBER(x) ((x)->uval.portval.number)
#define ERL_PORT_CREATION(x) ((x)->uval.portval.creation)

#define ERL_REF_NODE(x) ((x)->uval.refval.node)
#define ERL_REF_NUMBER(x) ((x)->uval.refval.n[0])
#define ERL_REF_NUMBERS(x) ((x)->uval.refval.n)
#define ERL_REF_LEN(x) ((x)->uval.refval.len)
#define ERL_REF_CREATION(x) ((x)->uval.refval.creation)

#define ERL_TUPLE_SIZE(x) ((x)->uval.tval.size)

/* NOTE!!! This is 0-based!! (first item is number 0)
 * Note too that element/2 (in Erlang) and
 * erl_element() are both 1-based.
 */
#define ERL_TUPLE_ELEMS(x) ((x)->uval.tval.elems)
#define ERL_TUPLE_ELEMENT(x, i) (ERL_TUPLE_ELEMS(x)[(i)])

#define ERL_BIN_SIZE(x) ((x)->uval.bval.size)
#define ERL_BIN_PTR(x) ((x)->uval.bval.b)

#define ERL_CONS_HEAD(x) ((x)->uval.lval.head)
#define ERL_CONS_TAIL(x) ((x)->uval.lval.tail)

#define ERL_VAR_LEN(x) ((x)->uval.vval.len)
#define ERL_VAR_NAME(x) ((x)->uval.vval.name)
#define ERL_VAR_VALUE(x) ((x)->uval.vval.v)

#define ERL_CLOSURE_SIZE(x)  ((x)->uval.funcval.size)
#define ERL_FUN_CREATOR(x)   ((x)->uval.funcval.creator)
#define ERL_FUN_MODULE(x)    ((x)->uval.funcval.module)
#define ERL_FUN_UNIQ(x)      ((x)->uval.funcval.uniq)
#define ERL_FUN_INDEX(x)     ((x)->uval.funcval.index)
#define ERL_FUN_ARITY(x)     ((x)->uval.funcval.arity)
#define ERL_FUN_NEW_INDEX(x) ((x)->uval.funcval.new_index)
#define ERL_FUN_MD5(x)       ((x)->uval.funcval.md5)
#define ERL_CLOSURE(x)       ((x)->uval.funcval.closure)
#define ERL_CLOSURE_ELEMENT(x,i) (ERL_CLOSURE(x)[(i)])

#define ERL_BIG_ARITY(x)     ((x)->uval.bigval.arity)
#define ERL_BIG_IS_NEG(x)    ((x)->uval.bigval.is_neg)
#define ERL_BIG_DIGITS(x)    ((x)->uval.bigval.digits)
#define ERL_BIG_DIGIT(x,i)   (ERL_BIG_DIGITS(x)[(i)])

/*
 * Typing checking macros.
 */

#define ERL_IS_DEFINED(x)  (ERL_TYPE(x) != 0)
#define ERL_IS_COMPOUND(x) (ERL_TYPE(x) & ERL_COMPOUND)

#define ERL_IS_INTEGER(x)  (ERL_TYPE(x) == ERL_INTEGER)
#define ERL_IS_UNSIGNED_INTEGER(x)  (ERL_TYPE(x) == ERL_U_INTEGER)
#define ERL_IS_FLOAT(x)    (ERL_TYPE(x) == ERL_FLOAT)
#define ERL_IS_ATOM(x)     (ERL_TYPE(x) == ERL_ATOM)
#define ERL_IS_PID(x)      (ERL_TYPE(x) == ERL_PID)
#define ERL_IS_PORT(x)     (ERL_TYPE(x) == ERL_PORT)
#define ERL_IS_REF(x)      (ERL_TYPE(x) == ERL_REF)
#define ERL_IS_CONS(x)     (ERL_TYPE(x) == ERL_CONS)
#define ERL_IS_NIL(x)      (ERL_TYPE(x) == ERL_NIL)
#define ERL_IS_EMPTY_LIST(x) ERL_IS_NIL(x)
#define ERL_IS_TUPLE(x)    (ERL_TYPE(x) == ERL_TUPLE)
#define ERL_IS_BINARY(x)   (ERL_TYPE(x) == ERL_BINARY)
#define ERL_IS_FUNCTION(x) (ERL_TYPE(x) == ERL_FUNCTION)
#define ERL_IS_BIG(x)      (ERL_TYPE(x) == ERL_BIG)

#define ERL_IS_LIST(x)     (ERL_IS_CONS(x) || ERL_IS_EMPTY_LIST(x))


typedef unsigned char Erl_Heap;

typedef struct _heapmark {
  unsigned long mark;      /* id */
  int size;                /* size of buffer */
  Erl_Heap *base;          /* points to start of buffer */
  Erl_Heap *cur;           /* points into buffer */
  struct _heapmark *prev;  /* previous heapmark */
} Erl_HeapMark;


extern void erl_common_init __ERL_P((void *, long));
extern ETERM *erl_mk_atom __ERL_P((char*));
extern ETERM *erl_mk_var __ERL_P((char*));
extern ETERM *erl_mk_int __ERL_P((int));
extern ETERM *erl_mk_uint __ERL_P((unsigned int));
extern ETERM *erl_mk_tuple __ERL_P((ETERM**,int));
extern ETERM *erl_mk_list __ERL_P((ETERM**,int));
extern ETERM *erl_mk_empty_list __ERL_P((void));
extern ETERM *erl_mk_string __ERL_P((char*));
extern ETERM *erl_mk_estring __ERL_P((char*, int));
extern ETERM *erl_mk_float __ERL_P((double));
extern ETERM *erl_element __ERL_P((int,ETERM*));
extern ETERM *erl_mk_binary __ERL_P((char*,int));
extern ETERM *erl_mk_pid __ERL_P((const char*,unsigned int,unsigned int,unsigned char));
extern ETERM * __erl_mk_reference __ERL_P((const char *, size_t, unsigned int n[], unsigned char));

extern ETERM *erl_mk_ref __ERL_P((const char*,unsigned int,unsigned char));
extern ETERM *erl_mk_long_ref __ERL_P((const char*,unsigned int,unsigned int,unsigned int,unsigned char));
extern ETERM *erl_mk_port __ERL_P((const char*,unsigned int,unsigned char));
extern ETERM *erl_cons __ERL_P((ETERM*,ETERM*));
extern ETERM *erl_hd __ERL_P((ETERM*));
extern ETERM *erl_tl __ERL_P((ETERM*));
extern int erl_length __ERL_P((ETERM*));

extern int erl_iolist_length __ERL_P((ETERM*));
extern ETERM* erl_iolist_to_binary __ERL_P((ETERM* term));
extern char* erl_iolist_to_string __ERL_P((ETERM* term));

extern ETERM *erl_copy_term __ERL_P((ETERM*));
extern int erl_size __ERL_P((ETERM*));
extern ETERM *erl_var_content __ERL_P((ETERM*, char*));
extern int erl_current_fix_desc __ERL_P((void));
#ifndef SILENT
extern int erl_print_term __ERL_P((FILE*,ETERM*));
#endif

__ERL_END_DECL

#endif
