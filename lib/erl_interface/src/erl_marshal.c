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
 * Purpose: Decoding and encoding Erlang terms.
 */  
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <string.h>
/* #include "external.h" */
#include "erl_eterm.h"
#include "erl_malloc.h"
#include "erl_error.h"
#include "erl_internal.h"

#include "eiext.h" /* replaces external.h */

/* I hate those warnings... :-(  */
#ifdef SUNOS4 
extern int sscanf();
extern int memset();
#endif

static int is_string(ETERM* term);

/* Used when comparing two encoded byte arrays */
/* this global data is ok (from threading point of view) since it is
 * initialized once and never changed
 */
#define CMP_ARRAY_SIZE 256
static char cmp_array[CMP_ARRAY_SIZE]; 
static int init_cmp_array_p=1; /* initialize array, the first time */

#if defined(VXWORKS) && CPU == PPC860
#include <limits.h>
extern int erl_fp_compare(unsigned *a, unsigned *b);
#endif

#define ERL_REF_CMP 5

void erl_init_marshal(void)
{
  if (init_cmp_array_p) {
    memset(cmp_array, 0, CMP_ARRAY_SIZE);
    cmp_array[ERL_NIL_EXT] = 1;
    cmp_array[ERL_SMALL_INTEGER_EXT] = 2;
    cmp_array[ERL_INTEGER_EXT] = 2;
    cmp_array[ERL_FLOAT_EXT] = 3;
    cmp_array[ERL_ATOM_EXT] = 4;
    cmp_array[ERL_REFERENCE_EXT] = ERL_REF_CMP;
    cmp_array[ERL_NEW_REFERENCE_EXT] = ERL_REF_CMP;
    cmp_array[ERL_PORT_EXT] = 6;
    cmp_array[ERL_SMALL_TUPLE_EXT] = 7;
    cmp_array[ERL_LARGE_TUPLE_EXT] = 7;
    cmp_array[ERL_STRING_EXT] = 8;
    cmp_array[ERL_LIST_EXT] = 8;
    cmp_array[ERL_BINARY_EXT] = 9;
    init_cmp_array_p = 0;
  }
}

/*==============================================================
 * Marshalling routines.
 *==============================================================
 */

/* 
 * The actual ENCODE engine.
 * Returns 0 on success, otherwise 1.
 */
extern int erl_encode_it(ETERM *ep, unsigned char **ext, int dist)
{
    int i;
    unsigned int u;
    
    switch(ERL_TYPE(ep)) 
    {
    case ERL_ATOM:
	i =  ep->uval.aval.len;
	*(*ext)++ = ERL_ATOM_EXT;
	*(*ext)++ = (i >>8) &0xff;
	*(*ext)++ = i &0xff;
	memcpy((void *) *ext, (const void *) ep->uval.aval.a, i);
	*ext += i;
	return 0;

    case ERL_INTEGER:
	i = ep->uval.ival.i;
	/* ERL_SMALL_BIG */
	if ((i > ERL_MAX) || (i < ERL_MIN)) { 
	    *(*ext)++ = ERL_SMALL_BIG_EXT;
	    *(*ext)++ = 4;		/* four bytes */
	    if ((*(*ext)++ = ((i>>31) & 0x01))) /* sign byte  */ 
	      i = -i;
	    *(*ext)++ = i  & 0xff;	/* LSB first  */
	    *(*ext)++ = (i >> 8) & 0xff;
	    *(*ext)++ = (i >> 16) & 0xff;
	    *(*ext)++ = (i >> 24) & 0x7f; /* Don't include the sign bit */
	    return 0;
	} 
	/* SMALL_INTEGER */
	if ((i < 256) && (i >= 0)) {
	    *(*ext)++ = ERL_SMALL_INTEGER_EXT;
	    *(*ext)++ = i & 0xff;
	    return 0;
	}
	/* INTEGER */
	*(*ext)++ = ERL_INTEGER_EXT;
	*(*ext)++ = (i >> 24) & 0xff;
	*(*ext)++ = (i >> 16) & 0xff;
	*(*ext)++ = (i >> 8) & 0xff;
	*(*ext)++ = i  & 0xff;
	return 0;

    case ERL_U_INTEGER:
	u = ep->uval.uival.u;
	/* ERL_U_SMALL_BIG */
	if (u > ERL_MAX) {
	*(*ext)++ = ERL_SMALL_BIG_EXT;
	*(*ext)++ = 4;		/* four bytes */
	*(*ext)++ = 0;		/* sign byte  */ 
	*(*ext)++ = u  & 0xff;	/* LSB first  */
	*(*ext)++ = (u >> 8) & 0xff;
	*(*ext)++ = (u >> 16) & 0xff;
	*(*ext)++ = (u >> 24) & 0xff; 
	return 0;
	}
	/* SMALL_INTEGER */
	if ((u < 256) && (u >= 0)) {
	    *(*ext)++ = ERL_SMALL_INTEGER_EXT;
	    *(*ext)++ = u & 0xff;
	    return 0;
	}
	/* INTEGER */
	*(*ext)++ = ERL_INTEGER_EXT;
	*(*ext)++ = (u >> 24) & 0xff;
	*(*ext)++ = (u >> 16) & 0xff;
	*(*ext)++ = (u >> 8) & 0xff;
	*(*ext)++ = u  & 0xff;
	return 0;

    case ERL_PID:
	*(*ext)++ = ERL_PID_EXT;    
	/* First poke in node as an atom */    
	i = strlen(ep->uval.pidval.node);
	*(*ext)++ = ERL_ATOM_EXT;
	*(*ext)++ = (i >>8) &0xff;
	*(*ext)++ = i &0xff;
	memcpy(*ext, ep->uval.pidval.node, i);
	*ext += i;
	/* And then fill in the integer fields */
	i = ep->uval.pidval.number;
	*(*ext)++ = (i >>24) &0xff;
	*(*ext)++ = (i >>16) &0xff;
	*(*ext)++ = (i >>8) &0xff;
	*(*ext)++ = i &0xff;
	i = ep->uval.pidval.serial;
	*(*ext)++ = (i >>24) &0xff;
	*(*ext)++ = (i >>16) &0xff;
	*(*ext)++ = (i >>8) &0xff;
	*(*ext)++ = i &0xff;
	*(*ext)++ = ep->uval.pidval.creation;
	return 0;
    case ERL_REF:
	if (dist >= 4 && ep->uval.refval.len > 1) {
	    int len, j;

	    *(*ext)++ = ERL_NEW_REFERENCE_EXT;

	    i = strlen(ep->uval.refval.node);
	    len = ep->uval.refval.len;
	    *(*ext)++ = (len >>8) &0xff;
	    *(*ext)++ = len &0xff;

	    *(*ext)++ = ERL_ATOM_EXT;
	    *(*ext)++ = (i >>8) &0xff;
	    *(*ext)++ = i &0xff;
	    memcpy(*ext, ep->uval.refval.node, i);
	    *ext += i;
	    *(*ext)++ = ep->uval.refval.creation;
	    /* Then the integer fields */
	    for (j = 0; j < ep->uval.refval.len; j++) {
		i = ep->uval.refval.n[j];
		*(*ext)++ = (i >>24) &0xff;
		*(*ext)++ = (i >>16) &0xff;
		*(*ext)++ = (i >>8) &0xff;
		*(*ext)++ = i &0xff;
	    }
	} else {
	    *(*ext)++ = ERL_REFERENCE_EXT;
	    /* First poke in node as an atom */
	    i = strlen(ep->uval.refval.node);
	    *(*ext)++ = ERL_ATOM_EXT;
	    *(*ext)++ = (i >>8) &0xff;
	    *(*ext)++ = i &0xff;
	    memcpy(*ext, ep->uval.refval.node, i);
	    *ext += i;
	    /* Then the integer fields */
	    i = ep->uval.refval.n[0];
	    *(*ext)++ = (i >>24) &0xff;
	    *(*ext)++ = (i >>16) &0xff;
	    *(*ext)++ = (i >>8) &0xff;
	    *(*ext)++ = i &0xff;
	    *(*ext)++ = ep->uval.refval.creation;
	}
	return 0;
    case ERL_PORT:
	*(*ext)++ = ERL_PORT_EXT;
	/* First poke in node as an atom */
	i = strlen(ep->uval.portval.node);
	*(*ext)++ = ERL_ATOM_EXT;
	*(*ext)++ = (i >>8) &0xff;
	*(*ext)++ = i &0xff;
	memcpy(*ext, ep->uval.portval.node, i);
	*ext += i;
	/* Then the integer fields */
	i = ep->uval.portval.number;
	*(*ext)++ = (i >>24) &0xff;
	*(*ext)++ = (i >>16) &0xff;
	*(*ext)++ = (i >>8) &0xff;
	*(*ext)++ = i &0xff;
	*(*ext)++ = ep->uval.portval.creation;
	return 0;
    case ERL_EMPTY_LIST:
	*(*ext)++ = ERL_NIL_EXT;
	break;
    case ERL_LIST:
	i = is_string(ep);
	if (0 < i && i < 0x10000) { /* String. */
	    *(*ext)++ = ERL_STRING_EXT;
	    *(*ext)++ = (i >>8) &0xff;
	    *(*ext)++ = i &0xff;
	    while (ERL_TYPE(ep) == ERL_LIST) {
		*(*ext)++ = HEAD(ep)->uval.ival.i;
		ep = TAIL(ep);
	    }
	    break;
	} else {		/* List. */
	    i = erl_length(ep);
	    *(*ext)++ = ERL_LIST_EXT;
	    *(*ext)++ = (i >>24) &0xff;
	    *(*ext)++ = (i >>16) &0xff;
	    *(*ext)++ = (i >>8) &0xff;
	    *(*ext)++ = i &0xff;
	    while (ERL_TYPE(ep) == ERL_LIST) {
		if (erl_encode_it(HEAD(ep), ext, dist))
		    return 1;
		ep = TAIL(ep);
	    }
	    i = erl_encode_it(ep, ext, dist);
	    return i;
	}
    case ERL_TUPLE:
	i = ep->uval.tval.size;
	if (i <= 0xff) {
	    *(*ext)++ = ERL_SMALL_TUPLE_EXT;
	    *(*ext)++ = i & 0xff;
	}
	else {
	    *(*ext)++ = ERL_LARGE_TUPLE_EXT;
	    *(*ext)++ = (i >> 24) & 0xff;
	    *(*ext)++ = (i >> 16 ) & 0xff;
	    *(*ext)++ = (i >> 8) & 0xff;
	    *(*ext)++ = i & 0xff;
	}
	for (i=0; i<ep->uval.tval.size; i++)
	    if (erl_encode_it(ep->uval.tval.elems[i], ext, dist))
		return 1;
	break;
    case ERL_FLOAT:
	*(*ext)++ = ERL_FLOAT_EXT;
	memset(*ext, 0, 31);
	sprintf((char *) *ext, "%.20e", ep->uval.fval.f);
	*ext += 31;
	break;
    case ERL_BINARY:
	*(*ext)++ = ERL_BINARY_EXT;
	i = ep->uval.bval.size;
	*(*ext)++ = (i >> 24) & 0xff;
	*(*ext)++ = (i >> 16) & 0xff;
	*(*ext)++ = (i >> 8) & 0xff;
	*(*ext)++ = i  & 0xff;
	memcpy((char *) *ext, (char*) ep->uval.bval.b, i);
	*ext += i;
	break;
    default:
	return 1;
    }
    return 0;
}

/* 
 * ENCODE an ETERM into a BUFFER, assuming BUFFER is of 
 * enough size. At success return number of bytes written 
 * into it, otherwise return 0.
 */
int erl_encode3(ETERM *ep, unsigned char *t, int dist)
{
  unsigned char *x = t;
  
  *x++ = ERL_VERSION_MAGIC;
  if (erl_encode_it(ep, &x, dist)) {
#ifdef DEBUG
    erl_err_msg("<ERROR> erl_encode: Error while encoding");
#endif
    return 0;
  }
  return (x - t);

}

int erl_encode(ETERM *ep, unsigned char *t)
{
    return erl_encode3(ep, t, 4);
}

/* determine the buffer size that will be required for the eterm */
static int erl_term_len_helper(ETERM *ep, int dist);

int erl_term_len2(ETERM *ep, int dist)
{
  return 1+erl_term_len_helper(ep, dist);
}

int erl_term_len(ETERM *ep)
{
  return 1+erl_term_len_helper(ep, 4);
}

static int erl_term_len_helper(ETERM *ep, int dist)
{
  int len = 0;
  int i;
  unsigned int u;

  if (ep) {
    switch (ERL_TYPE(ep)) {
    case ERL_ATOM:
      i = ep->uval.aval.len;
      len = i + 3;
      break;

    case ERL_INTEGER:
      i = ep->uval.ival.i;
      if ((i > ERL_MAX) || (i < ERL_MIN)) len = 7;
      else if ((i < 256) && (i >= 0)) len = 2; 
      else len = 5;
      break;

    case ERL_U_INTEGER:
      u = ep->uval.uival.u;
      if (u > ERL_MAX) len = 7;
      else if (u  < 256) len = 2;
      else len = 5;
      break;

    case ERL_PID:
      /* 1 + N + 4 + 4 + 1 where N = 3 + strlen */
      i = strlen(ep->uval.pidval.node);
      len = 13 + i;
      break;

    case ERL_REF:
      i = strlen(ep->uval.refval.node);
      if (dist >= 4 && ep->uval.refval.len > 1) {
	  len = 1 + 2 + (i+3) + 1 + ep->uval.refval.len * 4;
      } else {
	  /* 1 + N + 4 + 1 where N = 3 + strlen */
	  len = 9 + i;
      }
      break;

    case ERL_PORT:
      /* 1 + N + 4 + 1 where N = 3 + strlen */
      i = strlen(ep->uval.portval.node);
      len = 9 + i;
      break;

    case ERL_EMPTY_LIST:
      len = 1;
      break;

    case ERL_LIST:
      i = is_string(ep);
      if ((i > 0) && (i < 0x10000)) { /* string: 3 + strlen */
	for (len = 3; ERL_TYPE(ep) == ERL_LIST; ep =  TAIL(ep)) {
	  len++;
	}
      }
      else { /* list: 5 + len(elem1) + len(elem2) ... */
	for (len = 5; ERL_TYPE(ep) == ERL_LIST; ep =  TAIL(ep)) {
	  len += erl_term_len_helper(HEAD(ep), dist);
	}
	len += erl_term_len_helper(ep, dist); /* last element */
      }
      break;

    case ERL_TUPLE:
      /* (2 or 5) + len(elem1) + len(elem2) ... */
      i = ep->uval.tval.size;
      if (i <= 0xff) len = 2;
      else len = 5;
      
      for (i=0; i<ep->uval.tval.size; i++) {
	len += erl_term_len_helper(ep->uval.tval.elems[i], dist);
      }
      break;

    case ERL_FLOAT:
      len = 32;
      break;

    case ERL_BINARY:
      i = ep->uval.bval.size;
      len = 5 + i;
      break;

    default:
#ifdef DEBUG
      fprintf(stderr,"Shouldn't happen: erl_term_len, unknown term type: '%c'\n",ERL_TYPE(ep));
#endif
      exit(1);
    }
  }

  return len;
}

/* 
 * This one makes it easy to ENCODE several CONSECUTIVE
 * ETERM's into the same buffer. 
 */
int erl_encode_buf(ETERM *ep, unsigned char **ext)
{
  unsigned char *start=*ext;
  
  *(*ext)++ = ERL_VERSION_MAGIC;
  if (erl_encode_it(ep, ext, 0)) {
#ifdef DEBUG
    erl_err_msg("<ERROR> erl_encode_buf: Error while encoding\n");
#endif
    return 0;
  }
  return (*ext - start);

} /* erl_encode_buf */

/*
 * A nice macro to make it look cleaner in the 
 * cases of PID's,PORT's and REF's below. 
 * It reads the NODE name from a buffer.
 */
#define READ_THE_NODE(ext,cp,len,i) \
/* eat first atom, repr. the node */ \
if (**ext != ERL_ATOM_EXT) \
  return (ETERM *) NULL; \
*ext += 1; \
i = (**ext << 8) | (*ext)[1]; \
cp = (char *) *(ext) + 2; \
*ext += (i + 2); \
len = i > MAXNODE_LEN ? MAXNODE_LEN - 1 : i

/*
 * The actual DECODE engine.
 * Returns NULL in case of failure.
 */
static ETERM *erl_decode_it(unsigned char **ext)
{
  char *cp;
  ETERM *ep,*tp,*np;
  unsigned int u,sign;
  int i,j,len,arity;
  double ff;
  
  /* Assume we are going to decode an integer */
  ep = erl_alloc_eterm(ERL_INTEGER);
  ERL_COUNT(ep) = 1;

  switch (*(*ext)++) 
    {
    case ERL_INTEGER_EXT:
      i = (int) (**ext << 24) | ((*ext)[1] << 16) |
	((*ext)[2] << 8) | (*ext)[3];
      *ext += 4;
      ep->uval.ival.i = i;
      return ep;

    case ERL_SMALL_INTEGER_EXT:
      i = *(*ext)++;
      ep->uval.ival.i = i;
      return ep;

    case ERL_SMALL_BIG_EXT:
      arity = *(*ext)++; 
      goto big_cont;
    case ERL_LARGE_BIG_EXT:
      arity = (**ext << 24) | ((*ext)[1])<< 16 | 
	((*ext)[2]) << 8 |((*ext)[3]); 
      *ext += 4;
    big_cont:
      sign = *(*ext)++; 
      if (arity != 4)             
	goto big_truncate;
      if ((*ext)[3] & 0x80) { 
	/* MSB already occupied ! */
	if (sign)
	  goto big_truncate;
	else {                
	  /* It will fit into an unsigned int !! */
	  u = (((*ext)[3] << 24)|((*ext)[2])<< 16|((*ext)[1]) << 8 |(**ext));
	  ERL_TYPE(ep) = ERL_U_INTEGER;
	  ep->uval.uival.u = u;
	  /* *ext += i; */
	  *ext += arity;
	  return ep;
	}
      }
      else {       
	/* It will fit into an int !! 
	 * Note: It comes in "one's-complement notation" 
	 */
	if (sign)
	  i = (int) (~(((*ext)[3] << 24) | ((*ext)[2])<< 16 |
		       ((*ext)[1]) << 8 | (**ext)) | (unsigned int) sign);
	else
	  i = (int) (((*ext)[3] << 24) | ((*ext)[2])<< 16 |
		     ((*ext)[1]) << 8 | (**ext));
	ep->uval.ival.i = i;
	*ext += arity;
	return ep;
      }
    big_truncate: 
      /* truncate to: (+/-) 1 */
#ifdef DEBUG
      erl_err_msg("<WARNING> erl_decode_it: Integer truncated...");
#endif
      ep->uval.ival.i = sign?-1:1;
      *ext += arity;
      return ep;
      
    case ERL_ATOM_EXT:
      ERL_TYPE(ep) = ERL_ATOM;
      i = (**ext << 8) | (*ext)[1];
      cp = (char *) *(ext) + 2;
      *ext += (i + 2);
      ep->uval.aval.len = i;
      ep->uval.aval.a = (char *) erl_malloc(i+1);
      memcpy(ep->uval.aval.a, cp, i);
      ep->uval.aval.a[i]='\0';
      return ep;
      
    case ERL_PID_EXT:
      ERL_TYPE(ep) = ERL_PID;	
      READ_THE_NODE(ext,cp,len,i);
      memcpy(ep->uval.pidval.node, cp, len);
      ep->uval.pidval.node[len] = '\0';
      /* get the integers */
      i = (**ext << 24) | ((*ext)[1]) << 16| ((*ext)[2]) << 8| ((*ext)[3]);
      ep->uval.pidval.number = i;
      *ext += 4;
      i = (**ext << 24) | ((*ext)[1]) << 16| ((*ext)[2]) << 8| ((*ext)[3]);	
      *ext += 4;
      ep->uval.pidval.serial = i;
      i =  *(*ext)++; 
      ep->uval.pidval.creation = i;
      return ep;
      
    case ERL_REFERENCE_EXT:
      ERL_TYPE(ep) = ERL_REF;
      READ_THE_NODE(ext,cp,len,i);
      memcpy(ep->uval.refval.node, cp, len);
      ep->uval.refval.node[len] = '\0';
      /* get the integers */
      i = (**ext << 24) | ((*ext)[1]) << 16| ((*ext)[2]) << 8| ((*ext)[3]) ;
      ep->uval.refval.len = 1;
      ep->uval.refval.n[0] = i;
      *ext += 4;
      i = *(*ext)++; 
      ep->uval.refval.creation = i;
      return ep;

    case ERL_NEW_REFERENCE_EXT: {
	int len2;
	int n;

	ERL_TYPE(ep) = ERL_REF;
	len2 = (**ext << 8) | (*ext)[1];
	*ext += 2;
	READ_THE_NODE(ext,cp,len,i);
	memcpy(ep->uval.refval.node, cp, len);
	ep->uval.refval.node[len] = '\0';
	i = *(*ext)++; 
	ep->uval.refval.creation = i;
	/* get the integers */
	n = len2;
	for (j = 0; j < n; j++) {
	    i = (**ext << 24) | ((*ext)[1]) << 16 |
		((*ext)[2]) << 8 | ((*ext)[3]);
	    ep->uval.refval.n[j] = i;
	    (*ext) += 4;
	}
	ep->uval.refval.len = n;
	return ep;
    }

    case ERL_PORT_EXT:
      ERL_TYPE(ep) = ERL_PORT;
      READ_THE_NODE(ext,cp,len,i);
      memcpy(ep->uval.portval.node, cp, len);
      ep->uval.portval.node[len] = '\0';
      /* get the integers */
      i = (**ext << 24) | ((*ext)[1]) << 16| ((*ext)[2]) << 8| ((*ext)[3]) ;
      ep->uval.portval.number = i;
      *ext += 4;
      i = *(*ext)++;
      ep->uval.portval.creation = i;
      return ep;

    case ERL_NIL_EXT:
      ERL_TYPE(ep) = ERL_EMPTY_LIST;
      return ep;

    case ERL_LIST_EXT:
      ERL_TYPE(ep) = ERL_LIST;
      i = (**ext << 24) | ((*ext)[1] << 16) |((*ext)[2] << 8) | (*ext)[3];
      *ext += 4;	
      /* ASSERT(i != 0);	*/	/* Should be represented by ERL_NIL_EXT. */
      tp = ep;
      for (j = 0; j < i; j++) 
	  if ((HEAD(tp) = erl_decode_it(ext)) == NULL) 
	      goto failure;
	  else if (j + 1 < i) {
	      /* We have to watch out for how we allocates the
	       * last tail element since we may encounter non-
	       * well formed lists.
	       */
	      np = erl_alloc_eterm(ERL_LIST);
	      ERL_COUNT(np) = 1;
	      TAIL(tp) = np;
	      tp = np;
	  }
      if ((TAIL(tp) = erl_decode_it(ext)) == NULL) 
	  goto failure;
      return ep;

    case ERL_STRING_EXT:
      {
	  unsigned char* s;
	  
	  ERL_TYPE(ep) = ERL_EMPTY_LIST;
	  i = (**ext << 8) | ((*ext)[1]);
	  *ext += 2;
	  s = *ext+i;

	  while (*ext < s) {
	      ETERM* integer;
	      ETERM* cons;

	      integer = erl_alloc_eterm(ERL_INTEGER);
	      ERL_COUNT(integer) = 1;
	      integer->uval.ival.i = *--s;

	      cons = erl_alloc_eterm(ERL_LIST);
	      ERL_COUNT(cons) = 1;
	      HEAD(cons) = integer;
	      TAIL(cons) = ep;
	      ep = cons;
	  }
	  *ext += i;
	  return ep;
      }

    case ERL_SMALL_TUPLE_EXT:
      ERL_TYPE(ep) = ERL_TUPLE;
      i = *(*ext)++;
      goto decode_tuple;

    case ERL_LARGE_TUPLE_EXT:
      i = (**ext << 24) | ((*ext)[1]) << 16 | 
	((*ext)[2]) << 8 | ((*ext)[3]) ;	
      *ext += 4;
    decode_tuple:
      ep->uval.tval.size = i;
      j = (i + 1) * sizeof(ETERM*);
      ep->uval.tval.elems = (ETERM**) erl_malloc(j);
      memset(ep->uval.tval.elems, 0, j); /* in case of failure below... */
      for (i=0; i<ep->uval.tval.size; i++)
	if ((tp = erl_decode_it(ext)) == NULL)
	  goto failure;
	else
	  ep->uval.tval.elems[i] = tp;
      return ep;

    case ERL_FLOAT_EXT:
      ERL_TYPE(ep) = ERL_FLOAT;
      if (sscanf((char *) *ext, "%lf", &ff) != 1)
	goto failure;
      *ext += 31;
      ep->uval.fval.f = ff;
      return ep;

    case ERL_BINARY_EXT:
      ERL_TYPE(ep) = ERL_BINARY;
      i = (**ext << 24) | ((*ext)[1] << 16) |
	((*ext)[2] << 8) | (*ext)[3];
      *ext += 4;
      ep->uval.bval.size = i;
      ep->uval.bval.b = (unsigned char *) erl_malloc(i);
      memcpy(ep->uval.bval.b, *ext, i);
      *ext += i;
      return ep;

    } /* switch */

 failure:
  erl_free_term(ep);
  return (ETERM *) NULL;

} /* erl_decode_it */

/*
 * DECODE a buffer of BYTES into an ETERM.
 * Returns NULL in case of failure.
 */
ETERM *erl_decode(unsigned char *t) 
{
  ETERM *ep;
  unsigned char *ext;

  ext = t;

  /* We ignore the version magic since it might be
   * possible that the buffer has been manipulated
   * with erl_peek_ext.
   */
  if (*ext == ERL_VERSION_MAGIC) 
    ext++;  

  ep = NULL;
  ep = erl_decode_it(&ext);
#ifdef DEBUG
  if (!ep) erl_err_msg("<ERROR> erl_decode: Error while decoding");
#endif
  return ep;

} /* erl_decode */

/* 
 * This one makes it possible to DECODE two CONSECUTIVE 
 * ETERM's in the same buffer. 
 */
ETERM *erl_decode_buf(unsigned char **ext) 
{
  ETERM *ep;
  
  /* We ignore the version magic since it might be
   * possible that the buffer has been manipulated
   * with erl_peek_ext.
   */
  if (**ext == ERL_VERSION_MAGIC) 
    (*ext)++;

  ep = NULL;
  ep = erl_decode_it(ext);
#ifdef DEBUG
    if (!ep) erl_err_msg("<ERROR> erl_decode_buf: Error while decoding");
#endif
  return ep;

} /* erl_decode_buf */


/*==============================================================
 * Ok, here comes routines for inspecting/manipulating 
 * an encoded buffer of bytes.
 *==============================================================
 */

/*
 * Return 1 if the VERSION MAGIC in the BUFFER is the
 * same as the this library version.
 */
int erl_verify_magic(unsigned char *ext)
{

  if (*ext == ERL_VERSION_MAGIC) 
    return 1;
  else
    return 0;

} /* erl_verify_magic */

/*
 * Return the TYPE of an ENCODED ETERM.
 * At failure, return 0.
 */ 
char erl_ext_type(unsigned char *ext)
{
  if (*ext == ERL_VERSION_MAGIC) 
    return erl_ext_type(ext+1);
  
  switch(*ext) 
    {
    case ERL_SMALL_INTEGER_EXT:
    case ERL_INTEGER_EXT:
      return ERL_INTEGER;
    case ERL_ATOM_EXT:
      return ERL_ATOM;
    case ERL_PID_EXT:
      return ERL_PID;
    case ERL_PORT_EXT:
      return ERL_PORT;
    case ERL_REFERENCE_EXT:
      return ERL_REF;
    case ERL_NEW_REFERENCE_EXT:
      return ERL_REF;
    case ERL_NIL_EXT: 
      return ERL_EMPTY_LIST;
    case ERL_LIST_EXT:
      return ERL_LIST;
    case ERL_SMALL_TUPLE_EXT:
    case ERL_LARGE_TUPLE_EXT:
      return ERL_TUPLE;
    case ERL_FLOAT_EXT:
      return ERL_FLOAT;
    case ERL_BINARY_EXT:
      return ERL_BINARY;
    default:
      return 0;

    } /* switch */

} /* erl_ext_type */

/* 
 * Returns the number of elements in compund
 * terms. For other kind of terms zero is returned.
 * At failure -1 is returned.
 */
int erl_ext_size(unsigned char *t)
{
  int i;
  unsigned char *v;

  if (*t == ERL_VERSION_MAGIC) 
    return erl_ext_size(t+1);
 
  v = t+1;
  switch(*t) 
    {
    case ERL_SMALL_INTEGER_EXT:
    case ERL_INTEGER_EXT:
    case ERL_ATOM_EXT:
    case ERL_PID_EXT:
    case ERL_PORT_EXT:
    case ERL_REFERENCE_EXT:
    case ERL_NEW_REFERENCE_EXT:
    case ERL_NIL_EXT: 
    case ERL_BINARY_EXT:
    case ERL_STRING_EXT:
    case ERL_FLOAT_EXT:
      return 0;
      break;
    case ERL_SMALL_TUPLE_EXT:
      i = v[0];
      return i;
      break;
    case ERL_LIST_EXT:
    case ERL_LARGE_TUPLE_EXT:
      i = (v[0] << 24) | (v[1] << 16) | (v[2] << 8) | v[3];
      return i;
      break;
    default:
      return -1;
      break;
  } /* switch */

} /* ext_size */

/*
 * A nice macro that eats up the atom pointed to.
 */
#define JUMP_ATOM(ext,i) \
if (**ext != ERL_ATOM_EXT) \
  return 0; \
*ext += 1; \
i = (**ext << 8) | (*ext)[1]; \
*ext += (i + 2)

/*
 * MOVE the POINTER PAST the ENCODED ETERM we
 * are currently pointing at. Returns 1 at
 * success, otherwise 0.
 */
static int jump(unsigned char **ext) 
{
  int j,k,i=0;
  int n;
    
  switch (*(*ext)++) 
    {
    case ERL_VERSION_MAGIC:
      return jump(ext);
    case ERL_INTEGER_EXT:
      *ext += 4;
      break;
    case ERL_SMALL_INTEGER_EXT:
      *ext += 1;
      break;
    case ERL_ATOM_EXT:
      i = (**ext << 8) | (*ext)[1];
      *ext += (i + 2);
      break;
    case ERL_PID_EXT:
      /* eat first atom */
      JUMP_ATOM(ext,i);
      *ext += 9; /* Two int's and the creation field */
      break;
    case ERL_REFERENCE_EXT:
    case ERL_PORT_EXT:
      /* first field is an atom */
      JUMP_ATOM(ext,i);
      *ext += 5; /* One int and the creation field */
      break;
    case ERL_NEW_REFERENCE_EXT:
      n = (**ext << 8) | (*ext)[1];
      *ext += 2;
      /* first field is an atom */
      JUMP_ATOM(ext,i);
      *ext += 4*n+1;
      break;
    case ERL_NIL_EXT:
      /* We just passed it... */
      break;
    case ERL_LIST_EXT:
      i = j = 0;
      j = (**ext << 24) | ((*ext)[1] << 16) |((*ext)[2] << 8) | (*ext)[3];
      *ext += 4;	
      for(k=0; k<j; k++) 
	if ((i = jump(ext)) == 0)
	  return(0);
      if (**ext == ERL_NIL_EXT) {
	*ext += 1;
	break;
      }
      if (jump(ext) == 0) return 0;
      break;
    case ERL_STRING_EXT:
      i = **ext << 8 | (*ext)[1];
      *ext += 2 + i;
      break;
    case ERL_SMALL_TUPLE_EXT:
      i = *(*ext)++;
      goto jump_tuple;
    case ERL_LARGE_TUPLE_EXT:
      i = (**ext << 24) | ((*ext)[1] << 16) |((*ext)[2] << 8) | (*ext)[3];
      *ext += 4;
    jump_tuple:
      for (j = 0; j < i; j++) 
	if ((k = jump(ext)) == 0)
	  return(0);
      break;
    case ERL_FLOAT_EXT:
      *ext += 31;
      break;
    case ERL_BINARY_EXT:
      i = (**ext << 24) | ((*ext)[1] << 16) |((*ext)[2] << 8) | (*ext)[3];
      *ext += 4+i;
      break;
    default:
      return 0;
    } /* switch */

  return 1;

} /* jump */

/* 
 * The actual PEEK engine.
 */
static unsigned char *peek_ext(unsigned char **ext, int jumps)
{
  int i;

  switch (*(*ext)++) 
    {
    case ERL_VERSION_MAGIC:
      return peek_ext(ext, jumps);
    case ERL_SMALL_TUPLE_EXT:
      i = *(*ext)++;
      goto do_the_peek_stuff;
    case ERL_LARGE_TUPLE_EXT:
    case ERL_LIST_EXT:
      i = (**ext << 24) | ((*ext)[1]) << 16| ((*ext)[2]) << 8| ((*ext)[3]) ;  
      *ext += 4;
    do_the_peek_stuff:
      if (i <= jumps)   {
#ifdef DEBUG
	erl_err_msg("<ERROR> peek_ext: Out of range"); 
#endif
	return NULL;
      }
      for(i=0; i<jumps; i++)
	if (!jump(ext)) {
#ifdef DEBUG
	  erl_err_msg("<ERROR> peek_ext: Bad data"); 
#endif
	  return NULL;
	}
      return *ext;
    default:
#ifdef DEBUG
      erl_err_msg("<ERROR> peek_ext: Can't peek in non list/tuple type");
#endif
      return NULL;
    } /* switch */

} /* peek_ext */
	
/*
 * Return a POINTER TO the N:TH ELEMENT in a
 * COMPUND ENCODED ETERM.
 */
unsigned char *erl_peek_ext(unsigned char *ext, int jumps)
{
  unsigned char *x=ext;

  return peek_ext(&x, jumps);  

} /* erl_peek_ext */

/* 
 * Lexically compare two strings of bytes,
 * (string s1 length l1 and s2 l2).
 * Return: -1 if s1 < s2
 *	    0 if s1 = s2
 *	    1 if s1 > s2 
 */
static int cmpbytes(unsigned char* s1,int l1,unsigned char* s2,int l2)
{
  int i;
  i = 0;
  while((i < l1) && (i < l2)) {
    if (s1[i] < s2[i]) return(-1);
    if (s1[i] > s2[i]) return(1);
    i++;
  }
  if (l1 < l2) return(-1);
  if (l1 > l2) return(1);
  return(0);

} /* cmpbytes */

/* 
 * We now know that both byte arrays are of the same type.
 */
static int compare_top_ext(unsigned char**, unsigned char **); /* forward */

static int cmp_refs(unsigned char **e1, unsigned char **e2)
{
    int l1, l2;
    int t1, t2;
    int n1, n2, n;
    int c1, c2;
    int ret, i;

    t1 = *(*e1)++;
    t2 = *(*e2)++;

    if (t1 == ERL_REFERENCE_EXT) {
	JUMP_ATOM(e1,i); 
	n1 = 1;
    } else {
	l1 = (**e1 << 8) | (*e1)[1];
	*e1 += 2;
	JUMP_ATOM(e1,i);
	n1 = (l1 - (i+3) - 1) / 4;
	c1 = **e1;
	*e1 += 1;
    }

    if (t2 == ERL_REFERENCE_EXT) {
	JUMP_ATOM(e2,i); 
	n2 = 1;
    } else {
	l2 = (**e2 << 8) | (*e2)[1];
	*e2 += 2;
	JUMP_ATOM(e2,i);
	n2 = (l2 - (i+3) - 1) / 4;
	c2 = **e2;
	*e2 += 1;
    }

    n = n1;
    if (n > n2)
	n = n2;
    ret = cmpbytes(*e1,4*n,*e2,4*n);

    *e1 += 4*n1;
    *e2 += 4*n2;

    if (t1 == ERL_REFERENCE_EXT)
	c1 = *(*e1)++;
    if (t2 == ERL_REFERENCE_EXT)
	c2 = *(*e2)++;

    if (ret == 0) {
	if (c1 < c2)
	    ret = -1;
	else if (c1 > c2)
	    ret = 1;
	else
	    ret = 0;
    }
    return ret;
}

static int cmp_exe2(unsigned char **e1, unsigned char **e2)
{
  int min,  ret,i,j,k;
  double ff1, ff2;
  
  *e2 += 1;
  switch (*(*e1)++) 
    {
    case ERL_SMALL_INTEGER_EXT:
      if (**e1 < **e2) ret = -1;
      else if (**e1 > **e2) ret = 1;
      else ret = 0;
      *e1 += 1; *e2 += 1;
      return ret;
    case ERL_INTEGER_EXT:
      i = (int) (**e1 << 24) | ((*e1)[1] << 16) |((*e1)[2] << 8) | (*e1)[3];
      j = (int) (**e2 << 24) | ((*e2)[1] << 16) |((*e2)[2] << 8) | (*e2)[3];
      if ( i < j) 
	ret = -1;
      else if ( i > j) 
	ret = 1;
      else 
	ret = 0;
      *e1 += 4; *e2 += 4;
      return ret;
    case ERL_ATOM_EXT:
      i = (**e1 << 8) | (*e1)[1];
      j = (**e2 << 8) | (*e2)[1];
      ret = cmpbytes(*e1 +2, i, *e2 +2, j);
      *e1 += (i + 2);
      *e2 += (j + 2);
      return ret;
    case ERL_PID_EXT:
      JUMP_ATOM(e1,i); 
      JUMP_ATOM(e2,i);
      ret = cmpbytes(*e1,9, *e2,9);
      *e1 += 9; *e2 += 9; return ret;
    case ERL_PORT_EXT:
      JUMP_ATOM(e1,i); JUMP_ATOM(e2,i);
      ret = cmpbytes(*e1,5, *e2,5);
      *e1 += 5; *e2 += 5; return ret;
    case ERL_NIL_EXT: return 0;
    case ERL_LIST_EXT:
      i = (**e1 << 24) | ((*e1)[1] << 16) |((*e1)[2] << 8) | (*e1)[3];
      *e1 += 4;
      j = (**e2 << 24) | ((*e2)[1] << 16) |((*e2)[2] << 8) | (*e2)[3];
      *e2 += 4;
      if ( i == j && j == 0 ) return 0;
      min = (i < j) ? i : j;
      k = 0;
      while (1) {
	if (k++ == min)
	  return compare_top_ext(e1 , e2);
	if ((ret = compare_top_ext(e1 , e2)) == 0) 
	  continue;
	return ret;
      }
    case ERL_STRING_EXT:
      i = (**e1 << 8) | ((*e1)[1]);
      *e1 += 2;
      j = (**e2 << 8) | ((*e2)[1]);
      *e2 += 2;
      ret = cmpbytes(*e1, i, *e2, j);
      *e1 += i;
      *e2 += j;
      return ret;
    case ERL_SMALL_TUPLE_EXT:
      i = *(*e1)++; 	j = *(*e2)++;
      if (i < j) return -1;
      if (j > j ) return 1;
      while (i--) {
	if ((j = compare_top_ext(e1, e2))) return j;
      }
      return 0;
    case ERL_LARGE_TUPLE_EXT:
      i = (**e1 << 24) | ((*e1)[1]) << 16| ((*e1)[2]) << 8| ((*e1)[3]) ;	
      *e1 += 4;
      j = (**e2 << 24) | ((*e2)[1]) << 16| ((*e2)[2]) << 8| ((*e2)[3]) ;	
      *e2 += 4;
      if (i < j) return -1;
      if (j > j ) return 1;
      while (i--) {
	if ((j = compare_top_ext(e1, e2))) return j;
      }
      return 0;
    case ERL_FLOAT_EXT:
      if (sscanf((char *) *e1, "%lf", &ff1) != 1)
	return -1;
      *e1 += 31;
      if (sscanf((char *) *e2, "%lf", &ff2) != 1)
	return -1;
      *e2 += 31;
#if defined(VXWORKS) && CPU == PPC860
      return erl_fp_compare((unsigned *) &ff1, (unsigned *) &ff2);
#else
      if (ff1 < ff2) return -1;
      if (ff1 > ff2) return 1;
      return 0;
#endif
    case ERL_BINARY_EXT:
      i = (**e1 << 24) | ((*e1)[1] << 16) |((*e1)[2] << 8) | (*e1)[3];
      *e1 += 4;
      j = (**e2 << 24) | ((*e2)[1] << 16) |((*e2)[2] << 8) | (*e2)[3];
      *e2 += 4;
      ret = cmpbytes(*e1, i , *e2 , j);
      *e1 += i; *e2 += j;
      return ret;
    default:
      return cmpbytes(*e1, 1, *e2, 1);

    } /* switch */
  
} /* cmp_exe2 */

/* 
 * If the arrays are of the same type, then we
 * have to do a real compare.
 */
/* 
 * COMPARE TWO encoded BYTE ARRAYS e1 and e2.
 * Return: -1 if e1 < e2
 *          0 if e1 == e2 
 *          1 if e2 > e1   
 */
static int compare_top_ext(unsigned char**e1, unsigned char **e2)
{
  if (**e1 == ERL_VERSION_MAGIC) (*e1)++;
  if (**e2 == ERL_VERSION_MAGIC) (*e2)++;
  if (cmp_array[**e1] < cmp_array[**e2]) return -1;
  if (cmp_array[**e1] > cmp_array[**e2]) return 1;
  
  if (cmp_array[**e1] == ERL_REF_CMP)
      return cmp_refs(e1, e2);

  return cmp_exe2(e1, e2);
}

int erl_compare_ext(unsigned char *e1, unsigned char *e2)
{
  return compare_top_ext(&e1, &e2); 
} /* erl_compare_ext */

#if defined(VXWORKS) && CPU == PPC860
/* also used from erl_format */
int erl_fp_compare(unsigned *a, unsigned *b) 
{
    /* Big endian mode of powerPC, IEEE floating point. */
    unsigned a_split[4] = {a[0] >> 31,             /* Sign bit */
                           (a[0] >> 20) & 0x7FFU,  /* Exponent */
                           a[0] & 0xFFFFFU,        /* Mantissa MS bits */
                           a[1]};                  /* Mantissa LS bits */
    unsigned b_split[4] = {b[0] >> 31,
                           (b[0] >> 20) & 0x7FFU,
                           b[0] & 0xFFFFFU,
                           b[1]};
    int a_is_infinite, b_is_infinite;
    int res;


    /* Make -0 be +0 */
    if (a_split[1] == 0 && a_split[2] == 0 && a_split[3] == 0)
        a_split[0] = 0;
    if (b_split[1] == 0 && b_split[2] == 0 && b_split[3] == 0)
        b_split[0] = 0;
    /* Check for infinity */
    a_is_infinite = (a_split[1] == 0x7FFU && a_split[2] == 0 && 
                     a_split[3] == 0);
    b_is_infinite = (b_split[1] == 0x7FFU && b_split[2] == 0 && 
                     b_split[3] == 0);

    if (a_is_infinite && !b_is_infinite)
        return (a_split[0]) ? -1 : 1;
    if (b_is_infinite && !a_is_infinite)
        return (b_split[0]) ? 1 : -1;
    if (a_is_infinite && b_is_infinite)
        return b[0] - a[0]; 
    /* Check for indeterminate or nan, infinite is already handled, 
     so we only check the exponent. */
    if((a_split[1] == 0x7FFU) || (b_split[1] == 0x7FFU))
        return INT_MAX; /* Well, they are not equal anyway, 
                           abort() could be an alternative... */

    if (a_split[0] && !b_split[0])
        return -1;
    if (b_split[0] && !a_split[0])
        return 1;
    /* Compare */
    res = memcmp(a_split + 1, b_split + 1, 3 * sizeof(unsigned));
    /* Make -1, 0 or 1 */
    res = (!!res) * ((res < 0) ? -1 : 1); 
    /* Turn sign if negative values */
    if (a_split[0]) /* Both are negative */
        res = -1 * res;
    return res;
}
#endif


/* 
 * Checks if a term is a "string": a flat list of byte-sized integers.
 *
 * Returns: 0 if the term is not a string, otherwise the length is returned.
 */

static int
is_string(term)
    ETERM* term;
{
    int len = 0;

    while (ERL_TYPE(term) == ERL_LIST) {
	ETERM* head = HEAD(term);

	if (!ERL_IS_INTEGER(head) || head->uval.ival.i > 255) {
	    return 0;
	}
	len++;
	term = TAIL(term);
    }

    if (ERL_IS_EMPTY_LIST(term)) {
	return len;
    }
    return 0;
}
