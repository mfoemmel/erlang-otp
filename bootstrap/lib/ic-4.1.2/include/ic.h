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
#include <stdlib.h>
#include <string.h>
#include <ei.h>
#include <erl_interface.h>

#ifndef __IC_H__
#define __IC_H__

#ifdef __cplusplus
extern "C" {
#endif

/* Standard type mapping */

#ifndef __CORBA_SHORT__
#define __CORBA_SHORT__
typedef short           CORBA_short;
#endif

#ifndef __CORBA_LONG__
#define __CORBA_LONG__
typedef long            CORBA_long;
#endif

/* CORBA_long_long = long because of erl_interface limitation */
#ifndef __CORBA_LONG_LONG__
#define __CORBA_LONG_LONG__
typedef long            CORBA_long_long;  /* LONG LONG */
#endif

#ifndef __CORBA_UNSIGNED_SHORT__
#define __CORBA_UNSIGNED_SHORT__
typedef unsigned short  CORBA_unsigned_short;
#endif

#ifndef __CORBA_UNSIGNED_LONG__
#define __CORBA_UNSIGNED_LONG__
typedef unsigned long   CORBA_unsigned_long;
#endif

/* CORBA_unsigned long_long = unsigned long because of erl_interface limitation */
#ifndef __CORBA_UNSIGNED_LONG_LONG__
#define __CORBA_UNSIGNED_LONG_LONG__
typedef unsigned long  CORBA_unsigned_long_long; /* UNSIGNED LONG LONG */
#endif

#ifndef __CORBA_FLOAT__
#define __CORBA_FLOAT__
typedef float           CORBA_float;
#endif

#ifndef __CORBA_DOUBLE__
#define __CORBA_DOUBLE__
typedef double          CORBA_double;
#endif


#ifndef __CORBA_LONG_DOUBLE__
#define __CORBA_LONG_DOUBLE__
typedef double          CORBA_long_double; /* LONG DOUBLE */
#endif

#ifndef __CORBA_CHAR__
#define __CORBA_CHAR__
typedef char            CORBA_char;
#endif

#ifndef __CORBA_WCHAR__
#define __CORBA_WCHAR__
typedef unsigned long   CORBA_wchar;    /* WCHAR */
#endif

#ifndef __CORBA_BOOLEAN__
#define __CORBA_BOOLEAN__
typedef unsigned char   CORBA_boolean;
#endif

#ifndef __CORBA_OCTET__
#define __CORBA_OCTET__
typedef char            CORBA_octet;
#endif

#ifndef CORBA_enum
#define CORBA_enum      enum
#endif

#ifndef __ERLANG_BINARY__
#define __ERLANG_BINARY__
typedef struct {
  CORBA_unsigned_long _maximum;
  CORBA_unsigned_long _length;
  CORBA_octet* _buffer;
} erlang_binary;                        /* ERLANG BINARY */
#endif



/* Object__   definition */
typedef void *CORBA_Object;


/* Exception discriminators */
#ifndef CORBA_NO_EXCEPTION
#define CORBA_NO_EXCEPTION      0
#endif

#ifndef CORBA_SYSTEM_EXCEPTION
#define CORBA_SYSTEM_EXCEPTION -1
#endif

#ifndef CORBA_USER_EXCEPTION
#define CORBA_USER_EXCEPTION   -2
#endif

/* System exceptions */

#define UNKNOWN          "UNKNOWN"
#define BAD_PARAM        "BAD_PARAM"
#define NO_MEMORY        "NO_MEMORY"
#define IMPL_LIMIT       "IMP_LIMIT"
#define COMM_FAILURE     "COMM_FAILURE"
#define INV_OBJREF       "INV_OBJREF"
#define NO_PERMISSION    "NO_PERMISSION"
#define INTERNAL         "INTERNAL"
#define MARSHAL          "MARSHAL"
#define INITIALIZE       "INITIALIZE"
#define NO_IMPLEMENT     "NO_IMPLEMENT"
#define BAD_TYPECODE     "BAD_TYPECODE"
#define BAD_OPERATION    "BAD_OPERATION"
#define NO_RESOURCES     "NO_RESOURCES"
#define NO_RESPONSE      "NO_RESPONSE"
#define PERSIST_STORE    "PERSIST_STORE"
#define BAD_INV_ORDER    "BAD_INV_ORDER"
#define TRANSIENT        "TRANSIENT"
#define FREE_MEM         "FREE_MEM"
#define INV_IDENT        "INV_IDENT"
#define INV_FLAG         "INV_FLAG"
#define INTF_REPOS       "INTF_REPOS"
#define BAD_CONTEXT      "BAD_CONTEXT"
#define OBJ_ADAPTER      "OBJ_ADAPTER"
#define DATA_CONVERSION  "DATA_CONVERSION"
#define OBJ_NOT_EXIST    "OBJECT_NOT_EXIST"



/* Exception type */
typedef int CORBA_exception_type; 


#ifndef __CORBA_ENVIRONMENT__
#define __CORBA_ENVIRONMENT__

/* Environment definition */
typedef struct {

  /*----- CORBA compatibility part ---------------------------------------*/
  CORBA_exception_type   _major;          /* Exception tag, initially set 
					     to CORBA_NO_EXCEPTION        */

  /*----- External Implementation part - initiated by the user -----------*/
  int                    _fd;             /* File descriptor              */
  int                    _inbufsz;        /* Size of input buffer         */
  char                  *_inbuf;          /* Pointer to always dynamically
					     allocated buffer for input   */
  int                    _outbufsz;       /* Size of output buffer        */
  char                  *_outbuf;         /* Pointer to always dynamically
					     allocated buffer for output  */
  int                    _memchunk;       /* Size of memory chunks in bytes, 
					     used for increasing the output 
					     buffer, set to >= 32, should 
					     be around >= 1024 for 
					     performance reasons          */
  char                   _regname[256];   /* Pointer for registered name  */
  erlang_pid            *_to_pid;         /* Process identity for caller  */
  erlang_pid            *_from_pid;       /* Process identity for callee  */ 


  /*----- Internal Implementation part - used by the server/client -------*/
  int                    _iin;            /* Index for input buffer       */
  int                    _iout;           /* Index for output buffer      */
  char                   _operation[256]; /* Pointer for operation name   */
  int                    _received;       /* Used to count parameters     */
  erlang_pid             _caller;         /* Used to identify the caller  */
  erlang_ref             _unique;         /* Used to identify the call    */
  CORBA_char            *_exc_id;         /* Exception id field           */
  void                  *_exc_value;      /* Exception value field        */
  
  unsigned int          _ref_counter_1;   /* Counter for reference        */
  unsigned int          _ref_counter_2;   /* Counter for reference        */
  unsigned int          _ref_counter_3;   /* Counter for reference        */

} CORBA_Environment; 

#endif


/* Corba standard functions */

extern void CORBA_free(void *);

extern CORBA_char *CORBA_string_alloc(CORBA_unsigned_long);

extern CORBA_wchar *CORBA_wstring_alloc(CORBA_unsigned_long);

extern CORBA_char *CORBA_exception_id(CORBA_Environment *ev);

extern void *CORBA_exception_value(CORBA_Environment *ev);

extern void CORBA_exception_free(CORBA_Environment *ev);

extern void CORBA_exc_set(CORBA_Environment *ev,
			  CORBA_exception_type Major,
			  CORBA_char *Id,
			  CORBA_char *Value);


extern CORBA_Environment *CORBA_Environment_alloc(int inbufsz, int outbufsz);

extern void ic_init_ref(CORBA_Environment *ev, erlang_ref *ref);

extern int ic_compare_refs(erlang_ref *ref1, erlang_ref *ref2);


/* Internally used stuff */

#define __OE_MEMCHUNK__   1024
#define __OE_VSNSZ__         1
#define __OE_LONGSZ__        7
#define __OE_LONGLONGSZ__    7
#define __OE_ULONGSZ__       7
#define __OE_ULONGLONGSZ__   7
#define __OE_DOUBLESZ__     32
#define __OE_CHARSZ__        2
#define __OE_WCHARSZ__       7  
#define __OE_TUPLEHDRSZ__    5
#define __OE_LISTHDRSZ__     5

/* The actual size of a wide char */
#define __OE_WCHAR_SIZE_OF__ 4  

/* Encoders */ 
extern int oe_ei_encode_version(CORBA_Environment *ev);
extern int oe_ei_encode_long(CORBA_Environment *ev, long p);
extern int oe_ei_encode_longlong(CORBA_Environment *ev, CORBA_long_long p);
extern int oe_ei_encode_ulong(CORBA_Environment *ev, unsigned long p);
extern int oe_ei_encode_ulonglong(CORBA_Environment *ev, CORBA_unsigned_long_long p);
extern int oe_ei_encode_double(CORBA_Environment *ev, double p);
extern int oe_ei_encode_char(CORBA_Environment *ev, char p);
extern int oe_ei_encode_wchar(CORBA_Environment *ev, CORBA_wchar p);
extern int oe_ei_encode_string(CORBA_Environment *ev, const char *p);
extern int oe_ei_encode_wstring(CORBA_Environment *ev, CORBA_wchar *p);
extern int oe_ei_encode_atom(CORBA_Environment *ev, const char *p);
extern int oe_ei_encode_pid(CORBA_Environment *ev, const erlang_pid *p);
extern int oe_ei_encode_port(CORBA_Environment *ev, const erlang_port *p);
extern int oe_ei_encode_ref(CORBA_Environment *ev, const erlang_ref *p);
extern int oe_ei_encode_term(CORBA_Environment *ev, void *t); 
extern int oe_ei_encode_tuple_header(CORBA_Environment *ev, int arity);
extern int oe_ei_encode_list_header(CORBA_Environment *ev, int arity);
extern int oe_encode_erlang_binary(CORBA_Environment *ev, erlang_binary *binary);
#define oe_ei_encode_empty_list(ev) oe_ei_encode_list_header(ev,0)

/* Decoders */
extern int oe_ei_decode_wchar(const char *buf, int *index, CORBA_wchar *p);
extern int oe_ei_decode_wstring(const char *buf, int *index, CORBA_wchar *p);
extern int oe_ei_decode_longlong(const char *buf, int *index, CORBA_long_long *p);
extern int oe_ei_decode_ulonglong(const char *buf, int *index, CORBA_unsigned_long_long *p);
extern int oe_decode_erlang_binary(CORBA_Environment *ev, char *buf, int *index, erlang_binary *binary);

/* Size calculators */
extern int oe_sizecalc_erlang_binary(CORBA_Environment *ev, int *index, int *size);

/* Print functions */
extern int print_erlang_binary(erlang_binary*);

/* Length counter for wide strings */
extern int ic_wstrlen(CORBA_wchar * p); 

  /* Wide string comparison */
extern int ic_wstrcmp(CORBA_wchar * ws1, CORBA_wchar * ws2);

/* Put for 64-bits integer type */
#define put64le(s,n) do { \
  (s)[0] = (n) & 0xff;  \
  (s)[1] = ((n) >>  8) & 0xff; \
  (s)[2] = ((n) >>  16) & 0xff; \
  (s)[3] = ((n) >>  24) & 0xff; \
  (s)[4] = ((n) >>  32) & 0xff; \
  (s)[5] = ((n) >>  40) & 0xff; \
  (s)[6] = ((n) >>  48) & 0xff; \
  (s)[7] = ((n) >>  56) & 0xff; \
  (s)[8] = ((n) >>  64) & 0xff; \
  (s) += 8; \
} while (0)

/* Get for 64-bits integer type */
#define get64le(s) \
  ((s) += 8, \
   ((((unsigned char *)(s))[-1] << 56) | \
    (((unsigned char *)(s))[-2] << 48) | \
    (((unsigned char *)(s))[-3] << 40) | \
    (((unsigned char *)(s))[-4] << 32) | \
    (((unsigned char *)(s))[-5] << 24) | \
    (((unsigned char *)(s))[-6] << 16) | \
    (((unsigned char *)(s))[-7] << 8) | \
    ((unsigned char *)(s))[-8]))



/* New stuff */ 

/* Generic function pointer */
typedef void (*___generic___(void *whatever,...));

/* Operation declaration */
typedef struct {

    char* interface;
    char* name;
    ___generic___* function;
    
} ___operation___;

/* Operation map declaration */
typedef struct {

    int length;
    ___operation___* operations;

} ___map___;


/* Generic switch */
extern int ___switch___(CORBA_Object, CORBA_Environment *, ___map___ *);

/* Generic call information function */
extern int ___call_info___(CORBA_Object, CORBA_Environment *); 

/* Map merging */
extern ___map___* ___merge___(___map___ *, int); 

       
#ifdef __cplusplus
}
#endif

#endif





























