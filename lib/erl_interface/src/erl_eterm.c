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
/*
 * Purpose: Representation of Erlang terms.
 */  

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "erl_eterm.h"
#include "erl_malloc.h"
#include "erl_error.h"
#include "erl_locking.h"
#include "erl_internal.h"

#define ERL_IS_BYTE(x) (ERL_IS_INTEGER(x) && ((x)->uval.ival.i & ~0xFF) == 0)

static void iolist_to_buf(ETERM* term, char** bufp);

extern void erl_init_marshal(void);

/* all initialisation of erl_interface modules should be called from here */
/* order is important: erl_malloc and erl_resolve depend on erl_locking */
/* NOTE: don't call this directly - please use erl_init() macro defined 
   in erl_locking.h! */
void erl_common_init(void *hp,long heap_size)
{
  erl_init_locking();
  erl_init_malloc(hp, heap_size);
  erl_init_marshal();
#ifdef VXWORKS
    erl_init_resolve();
#endif
}

/*
 * Create an INTEGER. Depending on its value it 
 * may end up as a BigNum.
 */
ETERM *erl_mk_int(int i)
{
    ETERM *ep;

    ep = erl_alloc_eterm(ERL_INTEGER);
    ERL_COUNT(ep) = 1;
    ep->uval.ival.i = i;
    return ep;
}

/*
 * Create an UNSIGNED INTEGER. Depending on its 
 * value it may end up as a BigNum.
 */
ETERM *erl_mk_uint(unsigned int u)
{
    ETERM *ep;

    ep = erl_alloc_eterm(ERL_U_INTEGER);
    ERL_COUNT(ep) = 1;
    ep->uval.uival.u = u;
    return ep;
}

/*
 * Create a FLOAT.
 */
ETERM *erl_mk_float(double d)
{
    ETERM *ep;

    ep = erl_alloc_eterm(ERL_FLOAT);
    ERL_COUNT(ep) = 1;
    ep->uval.fval.f = d;
    return ep;
}

/*
 * Create an ATOM 
 */
ETERM *erl_mk_atom(char *s)
{
  ETERM *ep;

  /* ASSERT(s != NULL); */
  if (!s) return NULL;

  ep = erl_alloc_eterm(ERL_ATOM);
  ERL_COUNT(ep) = 1;
  ep->uval.aval.len = strlen(s);
  ep->uval.aval.a = (char *) erl_malloc(1 + ep->uval.aval.len );
  strcpy(ep->uval.aval.a, s);
  return ep;
} 

/*
 * Given a string as input, creates a list.
 */
ETERM *erl_mk_string(char *s)
{
  /* ASSERT(s != NULL); */
  if (!s) return NULL;

    return erl_mk_estring(s, strlen(s));
}

ETERM *erl_mk_estring(char *s, int len)
{
    ETERM *ep;
    int i;

    if ((!s) || (len < 0)) return NULL;

    /*
     * ASSERT(s != NULL);
     * ASSERT(len >= 0);
     */

    ep = erl_mk_empty_list();
    for (i = len-1; i >= 0; i--) {
	ETERM* integer;
	ETERM* cons;

	integer = erl_alloc_eterm(ERL_INTEGER);
	ERL_COUNT(integer) = 1;
	integer->uval.ival.i = s[i];

	cons = erl_alloc_eterm(ERL_LIST);
	ERL_COUNT(cons) = 1;
	HEAD(cons) = integer;
	TAIL(cons) = ep;
	ep = cons;
    }
    return ep;
}

/*
 * Create a PID.
 */
ETERM *erl_mk_pid(const char *node, 
		  unsigned int number, 
		  unsigned int serial, 
		  unsigned char creation)
{
    ETERM *ep;

    if (!node) return NULL;
    /* ASSERT(node != NULL); */

    ep = erl_alloc_eterm(ERL_PID);
    ERL_COUNT(ep) = 1;
    strcpy(ep->uval.pidval.node, node);
    ep->uval.pidval.number   = number & 0x7fff; /* 15 bits */
    ep->uval.pidval.serial   = serial & 0x07;  /* 3 bits */
    ep->uval.pidval.creation = creation & 0x03; /* 2 bits */
    return ep;
}

/*
 * Create a PORT.
 */
ETERM *erl_mk_port(const char *node, 
		   unsigned int number, 
		   unsigned char creation)
{
    ETERM *ep;

    if (!node) return NULL;
    /* ASSERT(node != NULL); */

    ep = erl_alloc_eterm(ERL_PORT);
    ERL_COUNT(ep) = 1;
    strcpy(ep->uval.portval.node, node);
    ep->uval.portval.number   = number & 0x3ffff; /* 18 bits */
    ep->uval.portval.creation = creation & 0x03; /* 2 bits */
    return ep;
}

/*
 * Create a REFERENCE.
 */
ETERM *erl_mk_ref(const char *node, 
		  unsigned int number, 
		  unsigned char creation)
{
    ETERM *ep;

    if (!node) return NULL;
    /* ASSERT(node != NULL); */

    ep = erl_alloc_eterm(ERL_REF);
    ERL_COUNT(ep) = 1;
    strcpy(ep->uval.refval.node, node);
    ep->uval.refval.len   = 1;
    ep->uval.refval.n[0]   = number & 0x3ffff; /* 18 bits */
    ep->uval.refval.n[1]   = 0;
    ep->uval.refval.n[2]   = 0;
    ep->uval.refval.creation = creation & 0x03; /* 2 bits */
    return ep;
}

/*
 * Create a long REFERENCE.
 */
ETERM *erl_mk_long_ref(const char *node, 
		       unsigned int n1, unsigned int n2, unsigned int n3,
		       unsigned char creation)
{
    ETERM *ep;

    if (!node) return NULL;
    /* ASSERT(node != NULL); */

    ep = erl_alloc_eterm(ERL_REF);
    ERL_COUNT(ep) = 1;
    strcpy(ep->uval.refval.node, node);
    ep->uval.refval.len   = 3;
    ep->uval.refval.n[0]   = n3 & 0x3ffff; /* 18 bits */
    ep->uval.refval.n[1]   = n2;
    ep->uval.refval.n[2]   = n1;
    ep->uval.refval.creation = creation & 0x03; /* 2 bits */
    return ep;
}

/*
 * Create a BINARY.
 */
ETERM *erl_mk_binary(char *b, int size)
{
    ETERM *ep;

    if ((!b) || (size < 0)) return NULL;
    /* ASSERT(b != NULL); */

    ep = erl_alloc_eterm(ERL_BINARY);
    ERL_COUNT(ep) = 1;
    ep->uval.bval.size = size;
    ep->uval.bval.b = (unsigned char *) erl_malloc(ep->uval.bval.size);
    memcpy(ep->uval.bval.b, b, ep->uval.bval.size);
    return ep;
}

/*
 * Create a TUPLE. For each element in the tuple
 * bump its reference counter.
 */
ETERM *erl_mk_tuple(ETERM **arr,int size)
{
    ETERM *ep;
    int i;

    if ((!arr) || (size < 0)) return NULL;
    for (i=0; i<size; i++) if (!arr[i]) return NULL;
    /* ASSERT(arr != NULL); */
	
    ep = erl_alloc_eterm(ERL_TUPLE);
    ERL_COUNT(ep) = 1;
    ep->uval.tval.size = size;
    ep->uval.tval.elems = (ETERM**) erl_malloc((1 + size) * (sizeof(ETERM*)));
    for (i = 0; i < size; i++) {
      /* ASSERT(arr[i] != NULL); */
      ERL_COUNT(arr[i])++;
      ep->uval.tval.elems[i] = arr[i];
    }
    return ep;
}

/*
 * SET an ELEMENT in a TUPLE. Free the old element
 * and bump the reference counter of the new one.
 * Return 1 on success, otherwise 0.
 */
int erl_setelement(int ix, ETERM *ep, ETERM *vp)
{
  if ((!ep) || (!vp)) return 0;
  /* ASSERT(ep != NULL);
   * ASSERT(vp != NULL);
   */

    if ((ERL_TYPE(ep) == ERL_TUPLE) && (ix <= ep->uval.tval.size)) {
	erl_free_term(ep->uval.tval.elems[ix-1]);
	ep->uval.tval.elems[ix-1] = vp;
	ERL_COUNT(vp)++;
	return 1;
    }  
    erl_err_msg("<ERROR> erl_setelement: Bad type to setelement or out of range \n");
    return 0;
}

/* 
 * Extract an ELEMENT from a TUPLE. Bump the 
 * reference counter on the extracted object.
 */
ETERM *erl_element(int ix, ETERM *ep)
{
  if ((!ep) || (ix < 0)) return NULL;
  /*
   * ASSERT(ep != NULL);
   * ASSERT(ix >= 0);
   */

    if ((ERL_TYPE(ep) == ERL_TUPLE) &&  (ix <= ep->uval.tval.size)) {
	ERL_COUNT(ep->uval.tval.elems[ix-1])++;
	return ep->uval.tval.elems[ix-1];
    }
    else 
	return NULL;
} /* erl_element */

ETERM *erl_mk_empty_list(void)
{
    ETERM *ep;

    ep = erl_alloc_eterm(ERL_EMPTY_LIST);
    ERL_COUNT(ep) = 1;
    return ep;
}

/*
 * Construct a new list by CONS'ing a HEAD on
 * to the TAIL. Bump the reference counter on
 * the head and tail object. Note that we allow
 * non-well formed lists to be created.
 */
ETERM *erl_cons(ETERM *hd, ETERM *tl)
{
    ETERM *ep;

    if ((!hd) || (!tl)) return NULL;

    /*
     * ASSERT(hd != NULL);
     * ASSERT(tl != NULL);
     */

    ep = erl_alloc_eterm(ERL_LIST);
    ERL_COUNT(ep) = 1;
    HEAD(ep) = hd;
    TAIL(ep) = tl;
    ERL_COUNT(hd)++;
    ERL_COUNT(tl)++;
    return ep;
}

/*
 * Extract the HEAD of a LIST. Bump the reference 
 * counter on the head object.
 */
ETERM *erl_hd(ETERM *ep)
{
  if (!ep) return NULL;
  /* ASSERT(ep != NULL); */

    if (ERL_TYPE(ep) != ERL_LIST) {
	return (ETERM *) NULL; 
    }
    ERL_COUNT(ep->uval.lval.head)++;
    return ep->uval.lval.head;
}

/* 
 * Extract the TAIL of a LIST. Bump the reference
 * counter on the tail object.
 */
ETERM *erl_tl(ETERM *ep)
{
    ETERM *tl;

    if (!ep) return NULL;
    /* ASSERT(ep != NULL); */

    if (ERL_TYPE(ep) != ERL_LIST) {
	return (ETERM *) NULL; 
    }

    tl = TAIL(ep);
    ERL_COUNT(tl)++;
    return tl;
}

/*
 * Create a LIST from an array of elements. Note that
 * we create it from the last element in the array to
 * the first. Also, note that we decrement the reference
 * counter for each member in the list but the first one.
 * This is done because of the use of erl_cons.
 */

ETERM *erl_mk_list(ETERM **arr, int size)
{
    ETERM *ep;
    int i;

    if ((!arr) || (size < 0)) return NULL;
    for (i=0; i<size; i++) if (!arr[i]) return NULL;
    
    /* ASSERT(arr != NULL); */
    ep = erl_mk_empty_list();
    if (size > 0) {
	ERL_COUNT(ep)--;
    }

    for (i = size-1; i >= 0; i--) {
      /* ASSERT(arr[i] != NULL); */
	ep = erl_cons(arr[i], ep);
	if (i > 0)
	    ERL_COUNT(ep)--;	/* Internal reference */
    }
    return ep;
}

/* 
 * Create an empty VARIABLE. 
 */
ETERM *erl_mk_var(char *s)
{
    ETERM *ep;

    if (!s) return NULL;
    
    /* ASSERT(s != NULL); */

    ep = erl_alloc_eterm(ERL_VARIABLE);
    ERL_COUNT(ep) = 1;
    ep->uval.vval.len  = strlen(s);
    ep->uval.vval.name = (char *) erl_malloc(1 + ep->uval.vval.len );
    strcpy(ep->uval.vval.name, s);
    ep->uval.vval.v = (ETERM *) NULL;
    return ep;
}

/* 
 * Return the CONTENT of a VARIABLE with NAME.
 * If the content is non-nil then bump its
 * reference counter.
 */
ETERM *erl_var_content(ETERM *ep, char *name)
{
  int i;
  ETERM *vp;

  if ((!ep) || (!name)) return NULL;
    
  /*   ASSERT(ep != NULL); */

  switch(ERL_TYPE(ep)) 
    {
    case ERL_VARIABLE:
      if (strcmp((const char *) ep->uval.vval.name, (const char *) name) == 0) {
	if ((vp = ep->uval.vval.v)) {
	  ERL_COUNT(vp)++;
	  return vp;
	}
      }
      break;

    case ERL_LIST:
      while (ep && (ERL_TYPE(ep) != ERL_EMPTY_LIST)) {
	if ((vp = erl_var_content(HEAD(ep), name))) return vp;
	ep = TAIL(ep);
      }
      break;

    case ERL_TUPLE:
      for (i=0; i<ep->uval.tval.size; i++) 
	if ((vp = erl_var_content(ep->uval.tval.elems[i], name))) return vp;
      break;
    
    default:
      /* variables can't occur in other types */
      break;
    }
    
  /* nothing found ! */
  return NULL;
}

/*
 * Return the SIZE of a TUPLE or a BINARY.
 * At failure -1 is returned.
 */
int erl_size(ETERM *ep)
{
  if (!ep) return -1;
  
  /* ASSERT(ep != NULL); */

  switch (ERL_TYPE(ep)) {
  case ERL_TUPLE:
    return ep->uval.tval.size;
    break;
  case ERL_BINARY:
    return ep->uval.bval.size;
    break;
  }

  /* all others */
  return -1;
}

/*
 * Return the LENGTH of a LIST.
 * At failure -1 is returned (this include non-proper lists like [a|b]).
 */
int erl_length(ETERM *ep)
{
    int n = 0;

    if (!ep) return -1;
    /* ASSERT(ep != NULL); */

    while (ERL_TYPE(ep) == ERL_LIST) {
      n++;
      ep = TAIL(ep);
    }
    if (!ERL_IS_EMPTY_LIST(ep))
      return -1;

    return n;
}


/***********************************************************************
 * I o l i s t   f u n c t i o n s
 *
 * The following functions handles I/O lists.
 *
 * Informally, an I/O list is a deep list of characters and binaries,
 * which can be sent to an Erlang port.
 *
 * Formally, in BNF, an I/O list is defined as:
 *
 * iolist ::= []
 *        |   Binary
 *        |   [iohead | iolist]
 *        ;
 *
 * iohead ::= Binary
 *        |   Byte (integer in the range [0..255])
 *        |   iolist
 *        ;
 *
 * Note that versions of Erlang/OTP prior to R2 had a slightly more
 * restricted definition of I/O lists, in that the tail of a an I/O list
 * was not allowed to be a binary.  The erl_interface functions
 * for I/O lists follows the more liberal rules described by the BNF
 * description above.
 ***********************************************************************/

/*
 * This function converts an I/O list to a '\0' terminated C string.
 * The I/O list must not contain any occurrences of the integer 0.
 *
 * The string will be in memory allocated by erl_malloc().  It is the
 * responsibility of the caller to eventually call erl_free() to free
 * the memory.
 *
 * Returns: NULL if the list was not an I/O list or contained
 * the integer 0, otherwise a pointer to '\0' terminated string.
 */

char*
erl_iolist_to_string(term)
    ETERM* term;		/* Term to convert to a string. */
{
    ETERM* bin;

    if ((bin = erl_iolist_to_binary(term)) == NULL) {
	return NULL;
    } else {
	char* result = NULL;

	if (memchr(ERL_BIN_PTR(bin), '\0', ERL_BIN_SIZE(bin)) == NULL) {
	    result = (char *) erl_malloc(ERL_BIN_SIZE(bin)+1);
	    memcpy(result, ERL_BIN_PTR(bin), ERL_BIN_SIZE(bin));
	    result[ERL_BIN_SIZE(bin)] = '\0';
	}
	erl_free_term(bin);
	return result;
    }
}

/*
 * This function converts an I/O list to a binary term.
 *
 * Returns: NULL if the list was not an I/O list, otherwise
 * an ETERM pointer pointing to a binary term.
 */

ETERM*erl_iolist_to_binary(ETERM* term)
{
    ETERM *dest;
    int size;
    char* ptr;

    if (!term) return NULL;
    /* ASSERT(term != NULL); */

    /*
     * Verify that the term is an I/O list and get its length.
     */

    size = erl_iolist_length(term);
    if (size == -1) {
	return NULL;
    }

    /*
     * Allocate the binary and copy the contents of the I/O list into it.
     */

    dest = erl_alloc_eterm(ERL_BINARY);
    ERL_COUNT(dest) = 1;
    ERL_BIN_SIZE(dest) = size;
    ptr = ERL_BIN_PTR(dest) = (unsigned char *) erl_malloc(size);
    iolist_to_buf(term, &ptr);

    /*
     * If ptr doesn't point exactly one byte beyond the end of the
     * binary, something must be seriously wrong.
     */

    if (ERL_BIN_PTR(dest) + size != (unsigned char *) ptr) return NULL;
    /* ASSERT(ERL_BIN_PTR(dest) + size == (unsigned char *) ptr); */

    return dest;
}

/*
 * Returns the length of an I/O list.
 *
 * Returns: -1 if the term if the given term is not a I/O list,
 * or the length otherwise.
 */

int
erl_iolist_length(term)
    ETERM* term;
{
    int len = 0;

    while (ERL_IS_CONS(term)) {
	ETERM* obj = HEAD(term);

	if (ERL_IS_BYTE(obj)) {
	    len++;
	} else if (ERL_IS_CONS(obj)) {
	    int i;
	    if ((i = erl_iolist_length(obj)) < 0)
		return i;
	    len += i;
	} else if (ERL_IS_BINARY(obj)) {
	    len += obj->uval.bval.size;
	} else if (!ERL_IS_EMPTY_LIST(obj)) {
	    return(-1);
	}
	term = TAIL(term);
    }
    if (ERL_IS_EMPTY_LIST(term))
	return len;
    else if (ERL_IS_BINARY(term))
	return len + term->uval.bval.size;
    else
	return -1;
}

/*
 * Return a brand NEW COPY of an ETERM.
 */
ETERM *erl_copy_term(ETERM *ep)
{
    int i;
    ETERM *cp;

    if (!ep) return NULL;
    /* ASSERT(ep != NULL); */
    
    cp = erl_alloc_eterm(ERL_TYPE(ep));
    ERL_COUNT(cp) = 1;

    switch(ERL_TYPE(cp)) {
    case ERL_INTEGER:
    case ERL_SMALL_BIG:
	cp->uval.ival.i = ep->uval.ival.i;
	break;
    case ERL_U_SMALL_BIG:
	cp->uval.uival.u = ep->uval.uival.u;
	break;
    case ERL_FLOAT:
	cp->uval.fval.f = ep->uval.fval.f;
	break;
    case ERL_ATOM:
	cp->uval.aval.len = ep->uval.aval.len;
	cp->uval.aval.a = (char*) erl_malloc(1 + ep->uval.aval.len);
	memcpy(cp->uval.aval.a, ep->uval.aval.a, ep->uval.aval.len );
	cp->uval.aval.a[cp->uval.aval.len] = 0x0; /* OTP-2956 */
	break;
    case ERL_PID:
	memcpy((void *) &cp->uval.pidval, (const void *) &ep->uval.pidval, sizeof(Erl_Pid));
	ERL_COUNT(cp) = 1;
	break;
    case ERL_PORT:
	memcpy((void *) &cp->uval.portval, (const void *) &ep->uval.portval, sizeof(Erl_Port));
	ERL_COUNT(cp) = 1;
	break;
    case ERL_REF:
	memcpy((void *) &cp->uval.refval, (const void *) &ep->uval.refval, sizeof(Erl_Ref));
	ERL_COUNT(cp) = 1;
	break;
    case ERL_LIST:
	HEAD(cp) = erl_copy_term(HEAD(ep));
	TAIL(cp) = erl_copy_term(TAIL(ep));
	break;
    case ERL_EMPTY_LIST:
	break;
    case ERL_TUPLE:
	cp->uval.tval.size = i = ep->uval.tval.size;
	cp->uval.tval.elems = (ETERM**) erl_malloc(i * sizeof(ETERM*));
	for(i=0; i<ep->uval.tval.size; i++) 
	    cp->uval.tval.elems[i] = erl_copy_term(ep->uval.tval.elems[i]);
	break;
    case ERL_BINARY:
	cp->uval.bval.size = ep->uval.bval.size;
	cp->uval.bval.b = (unsigned char *) erl_malloc(ep->uval.bval.size);
	memcpy(cp->uval.bval.b, ep->uval.bval.b, ep->uval.bval.size);
	break;
    default:
	erl_err_msg("<ERROR> erl_copy_term: wrong type encountered !");
	erl_free_term(cp);
	return (ETERM *) NULL;
    }
    
    return cp;
}

#ifndef SILENT

#ifdef SUNOS4
extern int fprintf();
#endif

static int print_string(FILE* fp, ETERM* ep);
static int is_printable_list(ETERM* term);

/*
 * PRINT out an ETERM.
 */

int erl_print_term(FILE *fp, ETERM *ep)
{
    int j,i,doquote;
    int ch_written = 0; /* counter of written chars */

    if ((!fp) || (!ep)) return 0;
    /* ASSERT(ep != NULL); */

    j = i = doquote = 0;
    switch(ERL_TYPE(ep)) 
    {
    case ERL_ATOM:
      if (!islower(ep->uval.aval.a[0]))
	doquote = 1;
      else 
	for (i=1; i<ep->uval.aval.len; i++) {
	  if (isalnum(ep->uval.aval.a[i]) || (ep->uval.aval.a[i]=='_'))
	    continue;
	  else {
	    doquote = 1;
	    break;
	  }
	} /* for */
      if (doquote) {
	putc('\'', fp);
	ch_written++; 
      }
      i=0;
      while (i < ep->uval.aval.len) { 
	putc(ep->uval.aval.a[i++], fp);
	ch_written++;
      }
      if (doquote) {
	putc('\'', fp);
	ch_written++;
      }
      break;
    case ERL_VARIABLE:
      if (!isupper(ep->uval.vval.name[0])) {
	doquote = 1;
	putc('\'', fp);
	ch_written++;
      }
      while (i < ep->uval.vval.len) { 
	putc(ep->uval.vval.name[i++], fp);
	ch_written++;
      }
      if (doquote) {
	putc('\'', fp);
	ch_written++;
      }
      break;
    case ERL_PID:
      ch_written += fprintf(fp, "<%s.%d.%d>", ep->uval.pidval.node,
	      ep->uval.pidval.number, ep->uval.pidval.serial);
      break;
    case ERL_PORT:
      ch_written += fprintf(fp, "#Port");
      break;
    case ERL_REF:
      ch_written += fprintf(fp, "#Ref");
      break;
    case ERL_EMPTY_LIST:
      ch_written += fprintf(fp, "[]");
      break;
    case ERL_LIST: 
	if (is_printable_list(ep)) {
	    ch_written += print_string(fp, ep);
	} else {
	    putc('[', fp);
	    ch_written++;
	    while (ERL_IS_CONS(ep)) {
		ch_written += erl_print_term(fp, HEAD(ep));
		ep = TAIL(ep);
		if (ERL_IS_CONS(ep)) {
		    putc(',', fp);
		    ch_written++;
		}
	    }
	    if (!ERL_IS_EMPTY_LIST(ep)) {
		putc('|', fp);
		ch_written++;
		ch_written += erl_print_term(fp, ep);
	    }
	    putc(']', fp);
	    ch_written++;
	}
	break;
    case ERL_TUPLE:
      putc('{', fp);
      ch_written++;
      for (i=0; i<ep->uval.tval.size; i++) {
	ch_written += erl_print_term(fp, ep->uval.tval.elems[j++] );
	if (i != ep->uval.tval.size-1) {
	  putc(',', fp);
	  ch_written++;
	}
      }
      putc('}', fp);
      ch_written++;
      break;
    case ERL_BINARY:
      ch_written += fprintf(fp, "#Bin");
      break;
    case ERL_INTEGER:
    case ERL_SMALL_BIG:
      ch_written += fprintf(fp, "%d",ep->uval.ival.i);
      break;
    case ERL_U_SMALL_BIG:
      ch_written += fprintf(fp, "%d",ep->uval.uival.u);
      break;
    case ERL_FLOAT:
      ch_written += fprintf(fp, "%f", ep->uval.fval.f);
      break;
    default:
      ch_written = -10000;
      erl_err_msg("<ERROR> erl_print_term: Bad type of term !");
    }
  return ch_written;
}

static int
print_string(FILE* fp, ETERM* ep)
{
    int ch_written = 0; /* counter of written chars */
  
    putc('"', fp);
    ch_written++;
    while (ERL_IS_CONS(ep)) {
	int c = HEAD(ep)->uval.ival.i;

	if (c >= ' ') {
	    putc(c, fp);
	    ch_written++;
	}
	else {
	    switch (c) {
	    case '\n': fputs("\\n", fp); ch_written += 2; break;
	    case '\r': fputs("\\r", fp); ch_written += 2; break;
	    case '\t': fputs("\\t", fp); ch_written += 2; break;
	    case '\v': fputs("\\v", fp); ch_written += 2; break;
	    case '\b': fputs("\\b", fp); ch_written += 2; break;
	    case '\f': fputs("\\f", fp); ch_written += 2; break;
		break;
	    default:
		ch_written += fprintf(fp, "\\%o", c);
		break;
	    }
	}
	ep = TAIL(ep);
    }
    putc('"', fp);
    ch_written++;
    return ch_written;
}

/*
 * Returns 1 if term is a list of printable character, otherwise 0.
 */

static int
is_printable_list(term)
    ETERM* term;
{
    while (ERL_TYPE(term) == ERL_LIST) {
	ETERM* head = HEAD(term);

	if (!ERL_IS_BYTE(head)) {
	    return 0;
	}
	if (head->uval.ival.i < ' ') {
	    switch (head->uval.ival.i) {
	    case '\n':
	    case '\r':
	    case '\t':
	    case '\v':
	    case '\b':
	    case '\f':
		break;
	    default:
		return 0;
	    }
	}
	term = TAIL(term);
    }

    return ERL_IS_EMPTY_LIST(term);
}

#endif

/*
 * Retrieves the bytes from an I/O list and copy into a buffer.
 *
 * NOTE! It is the responsibility of the caller to ensure that
 * that the buffer is big enough (typically by calling
 * erl_iolist_length()), and that the term is an I/O list.
 */

static void
iolist_to_buf(term, bufp)
    ETERM* term;		/* Term to convert to bytes. */
    char** bufp;		/* Pointer to pointer to buffer
				 * where the bytes should be stored.
				 * On return, the pointer will point beyond
				 * the last byte stored.
				 */
{
    char* dest = *bufp;

    while (ERL_IS_CONS(term)) {
	ETERM* obj = HEAD(term);

	if (ERL_IS_BYTE(obj)) {
	    *dest++ = ERL_INT_VALUE(obj);
	} else if (ERL_IS_CONS(obj)) {
	    iolist_to_buf(obj, &dest);
	} else if (ERL_IS_BINARY(obj)) {
	    memcpy(dest, ERL_BIN_PTR(obj), ERL_BIN_SIZE(obj));
	    dest += ERL_BIN_SIZE(obj);
	} else {
	    /*
	     * Types have been checked by caller.
	     */
	  if (!ERL_IS_EMPTY_LIST(obj)) return;
	  /* ASSERT(ERL_IS_EMPTY_LIST(obj)); */
	}
	term = TAIL(term);
    }
    if (ERL_IS_BINARY(term)) {
	memcpy(dest, ERL_BIN_PTR(term), ERL_BIN_SIZE(term));
	dest += ERL_BIN_SIZE(term);
    } else {
	/*
	 * Types have been checked by caller.
	 */
      if (!ERL_IS_EMPTY_LIST(term)) return;
      /* ASSERT(ERL_IS_EMPTY_LIST(term));*/
    }
    *bufp = dest;
}
