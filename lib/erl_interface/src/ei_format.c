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
* Function:
* ei_format to build binary format terms a bit like printf
*/

#ifdef VXWORKS
#include <vxWorks.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>

#ifdef VRTX
#define __READY_EXTENSIONS__
#include <errno.h>
#endif

#include "ei_format.h"
#ifdef VXWORKS
typedef va_list va_list_p;
#define VA_LIST_FROM_P(VaList) (VaList)
#define VA_LIST_TO_P(VaList) (VaList)
#else
typedef va_list *va_list_p;
#define VA_LIST_FROM_P(VaList) (*(VaList))
#define VA_LIST_TO_P(VaList) (&(VaList))
#endif


static int eiformat(const char** s, va_list_p va, ei_x_buff* x);

static int xformat(const char* s, va_list va, ei_x_buff* x)
{
    int res = ei_x_encode_version(x);

    if (res >= 0)
	res = eiformat(&s, VA_LIST_TO_P(va), x);
    return res;
}

/* forwards of parse functions */
static int pformat(const char **fmt, va_list_p pap, ei_x_buff* x);
static int plist(const char **fmt, va_list_p pap, ei_x_buff* x, int size);
static int ptuple(const char **fmt, va_list_p pap, ei_x_buff* x, int size);
static int pquotedatom(const char **fmt, ei_x_buff* x);
static int pdigit(const char **fmt, ei_x_buff* x);
static int patom(const char **fmt, ei_x_buff* x);
static int pstring(const char **fmt, ei_x_buff* x);

/* format a string into an ei_x_buff, except the version token */
static int eiformat(const char **fmt, va_list_p va, ei_x_buff* x)
{
    const char* p = *fmt;
    int res;
    ei_x_buff x2;

    while (isspace(*p))
	++p;
    switch (*p) {
    case '~':
	res = pformat(&p, va, x);
	break;
    case '[':
	res = ei_x_new(&x2);
	if (res >= 0)
	    res = plist(&p, va, &x2, 0);
	if (res > 0)
	    res = ei_x_encode_list_header(x, res);
	if (res >= 0)
	    res = ei_x_append(x, &x2);
	ei_x_free(&x2);
	break;
    case '{':
	res = ei_x_new(&x2);
	if (res >= 0)
	    res = ptuple(&p, va, &x2, 0);
	if (res >= 0)
	    res = ei_x_encode_tuple_header(x, res);
	if (res >= 0)
	    res = ei_x_append(x, &x2);
	ei_x_free(&x2);
	break;
    case '"':
	res = pstring(&p, x);
	break;
    case '\'':
	res = pquotedatom(&p, x);
	break;
    default:
	if (isdigit(*p))
	    res = pdigit(&p, x);
	else if (islower(*p))
	    res = patom(&p, x);
	else
	    res = -1;
	break;
	/*
	Variables
	*/
    }
    *fmt = p;
    return res;
}

static int patom(const char **fmt, ei_x_buff* x)
{
    const char *start=*fmt;
    char c;
    int len;
    
    for (;;) {
	c = *(*fmt)++;
	if (isalnum((int) c) || (c == '_') || (c == '@'))
	    continue;
	else
	    break;
    }
    --(*fmt);
    len = *fmt - start;
    if (len > MAXATOMLEN)
	len = MAXATOMLEN;
    return ei_x_encode_atom_len(x, start, len);
}

/* Check if integer or float */
static int pdigit(const char **fmt, ei_x_buff* x)
{
    const char *start=*fmt;
    char c;
    int len, dotp=0;
    double d;
    long l;

    for (;;) {
	c = *(*fmt)++;
	if (isdigit((int)c))
	    continue;
	else if (!dotp && (c == '.')) {
	    dotp = 1;
	    continue;
	} else
	    break;
    } 
    --(*fmt);
    len = *fmt - start;
    if (dotp) {
	sscanf(start, "%lf", &d);
	return ei_x_encode_double(x, d);
    } else {
	sscanf(start, "%ld", &l);
	return ei_x_encode_long(x, l);
    }
}

/* "string" */
static int pstring(const char **fmt, ei_x_buff* x)
{
    const char *start = ++(*fmt); /* skip first quote */
    char c;
    int res;
    
    for (;;) {
	c = *(*fmt)++;
	if (c == '\0')
	    return -1;
	if (c == '"') {
	    if (*((*fmt)-1) == '\\')
		continue;
	    else
		break;
	} else
	    continue;
    }
    res = ei_x_encode_string_len(x, start, *fmt - start - 1);
    return res;
}

/* 'atom' */
static int pquotedatom(const char **fmt, ei_x_buff* x)
{
    const char *start = ++(*fmt); /* skip first quote */
    char c;
    int res;
    
    for (;;) {
	c = *(*fmt)++;
	if (c == 0)
	    return -1;
	if (c == '\'') {
	    if (*((*fmt)-1) == '\\')
		continue;
	    else
		break;
	} else 
	    continue;
    } 
    res = ei_x_encode_atom_len(x, start, *fmt - start - 1);
    return res;
}


 /* 
  * The format letters are:
  *   w  -  An ETERM erl_interface term
  *   a  -  An atom
  *   s  -  A string
  *   i  -  An integer
  *   l  -  A long integer
  *   u  -  An unsigned long integer
  *   f  -  A float 
  *   d  -  A double float 
  */
static int pformat(const char **fmt, va_list_p pap, ei_x_buff* x)
{
    int res = 0;
    ++(*fmt);	/* skip tilde */
    switch (*(*fmt)++) {
    /*case 'w':
        rc = ei_x_encode_term(buf, index, va_arg(*pap, ETERM*));
        break;*/
    case 'a': 
	res = ei_x_encode_atom(x, va_arg(VA_LIST_FROM_P(pap), char *));
	break;
    case 's':
	res = ei_x_encode_string(x, va_arg(VA_LIST_FROM_P(pap), char *));
	break;
    case 'i':
	res = ei_x_encode_long(x, va_arg(VA_LIST_FROM_P(pap), long));
	break;
    case 'l':
	res = ei_x_encode_long(x, va_arg(VA_LIST_FROM_P(pap), long));
	break;
    case 'u':
	res = ei_x_encode_ulong(x, va_arg(VA_LIST_FROM_P(pap), unsigned long));
	break;
    case 'f':	/* note that float is expanded to double (C calling conventions) */
    case 'd':
	res = ei_x_encode_double(x, va_arg(VA_LIST_FROM_P(pap), double));
	break;	
    default:
	res = -1;
	break;
    }
    return res;
}

/* encode a tuple */
static int ptuple(const char **fmt, va_list_p pap, ei_x_buff* x, int size)
{
    int res = 0;
    const char* p = *fmt;
    char after = *p++;
    
    if (after == '}')
	return size;
    while (isspace(*p))
	++p;
    switch (*p++) {
    case '}':
	if (after == ',')
	    res = -1;
	else
	    res = size;
	break;
    case ',':
	if (after == ',' || after == '{')
	    res = -1;
	else
	    res = ptuple(&p, pap, x, size);
	break;
    default:
	--p;
	res = eiformat(&p, pap, x);
	if (res >= 0)
	    res = ptuple(&p, pap, x, size + 1);
	break;
	/*
	Variables
	*/
    }
    *fmt = p;
    return res;
}

/* encode a list */
static int plist(const char **fmt, va_list_p pap, ei_x_buff* x, int size)
{
    int res = 0;
    const char* p = *fmt;
    char after = *p++;

    if (after == ']')
	--p;
    while (isspace(*p))
	++p;
    switch (*p++) {
    case ']':
	if (after == ',')
	    res = -1;
	else {
	    if (after != '|')
		ei_x_encode_empty_list(x);
	    res = size;
	}
	break;
    case '|':
	if (after == '|' || after == ',')
	    res = -1;
	else
	    res = plist(&p, pap, x, size);
	break;
    case ',':
	if (after == '|' || after == ',')
	    res = -1;
	else
	    res = plist(&p, pap, x, size);
	break;
    default:
	--p;
	res = eiformat(&p, pap, x);
	++size;
	if (res >= 0) {
	    if (after == '|') {
	        while (isspace(*p))
		    ++p;
		if (*p != ']')
		    res = -1;
	    } else
		res = plist(&p, pap, x, size);
	}
	break;
	/*
	Variables
	*/
    }
    *fmt = p;
    return res;
}

int ei_x_format(ei_x_buff* x, const char *fmt, ... )
{
    int res;
    va_list ap;
    va_start(ap, fmt);
    res = xformat(fmt, ap, x);
    va_end(ap);
    return res;
}

int ei_x_format_wo_ver(ei_x_buff* x, const char *fmt, ... )
{
    int res;
    va_list ap;
    va_start(ap, fmt);
    res = eiformat(&fmt, VA_LIST_TO_P(ap), x);
    va_end(ap);
    return res;
}
 
#if 0
 /* 
  * Perform a pattern match between a pattern p and a term t. 
  * As a side effect bind any unbound variables in p.
  * Return true or false.
  */
static int ematch(ETERM *p, ETERM *t)
{
    unsigned int type_p;
    unsigned int type_t;
    ETERM *tmp;
    
    /* two NULLs are equal, one is not... */
    if (!p && !t) return ERL_TRUE;
    if (!p || !t) return ERL_FALSE;
    /*
    * ASSERT(p != NULL);
    * ASSERT(t != NULL);
    */
    
    type_p = ERL_TYPE(p);
    type_t = ERL_TYPE(t);
    
    if (type_t == ERL_VARIABLE) {
	if (t->uval.vval.v == NULL)
	    return ERL_FALSE; /* Can't have an unbound variable here ! */
	else 
	    t = t->uval.vval.v;
    }
    
    if (type_p != ERL_VARIABLE && type_p != type_t)
	return ERL_FALSE;
    
    switch (type_p) {
	
    case ERL_ATOM:
	return p->uval.aval.len == t->uval.aval.len &&
	    memcmp(p->uval.aval.a, t->uval.aval.a, p->uval.aval.len) == 0;
	
    case ERL_VARIABLE:
	if (strcmp(p->uval.vval.name, "_") == 0) /* anon. variable */
	    return ERL_TRUE;
	else if ((tmp = find_lvar(p->uval.vval.name)) != (ETERM *) NULL) {
	/* v points to NULL in cases like erl_format("{X,X}") for the
	    second variable */
	    if (p->uval.vval.v == NULL) 	  
		p->uval.vval.v = erl_copy_term(tmp); 
	    return ematch(p->uval.vval.v, t);
	}
	else {
	    /* check if the variable is bound already */
	    if (p->uval.vval.v != NULL) {
		if (ematch(p->uval.vval.v, t) == ERL_TRUE ){
		    add_lvar(p);
		    return ERL_TRUE;
		} 
		else
		    return ERL_FALSE;
	    }
	    else {
		p->uval.vval.v = erl_copy_term(t);
		add_lvar(p);
		return ERL_TRUE;
	    }
	}
	break;
	
    case ERL_PID:
	if ((strcmp(ERL_PID_NODE(p), ERL_PID_NODE(t)) == 0) &&
	    (ERL_PID_NUMBER(p) == ERL_PID_NUMBER(t)) &&
	    (ERL_PID_SERIAL(p) == ERL_PID_SERIAL(t)) &&
	    (ERL_PID_CREATION(p) == ERL_PID_CREATION(t)))
	    return ERL_TRUE;
	else
	    return ERL_FALSE;
	break;
	
    case ERL_PORT:
	if ((strcmp(ERL_PORT_NODE(p), ERL_PORT_NODE(t)) == 0) &&
	    (ERL_PORT_NUMBER(p) == ERL_PORT_NUMBER(t)) &&
	    (ERL_PORT_CREATION(p) == ERL_PORT_CREATION(t)))
	    return ERL_TRUE;
	else
	    return ERL_FALSE;
	break;
	
    case ERL_REF: {
	int i, len;
	
	if (strcmp(ERL_REF_NODE(p), ERL_REF_NODE(t)) != 0 ||
	    ERL_REF_CREATION(p) != ERL_REF_CREATION(t))
	    return ERL_FALSE;
	
	/* XXX: {len=1, n={42}} and {len=3, n={42, 17, 13}} tests equal. */
	len = ERL_REF_LEN(p);
	if (len > ERL_REF_LEN(t))
	    len = ERL_REF_LEN(t);
	
	for (i = 0; i < len; i++)
	    if (ERL_REF_NUMBERS(p)[i] != ERL_REF_NUMBERS(t)[i])
		return ERL_FALSE;
	    
	    return ERL_TRUE;
	    break;
		  }
	
    case ERL_EMPTY_LIST:
	return ERL_TRUE;
	
    case ERL_LIST: 
	while (ERL_IS_CONS(p) && ERL_IS_CONS(t)) {
	    if (ematch(p->uval.lval.head, t->uval.lval.head) == ERL_FALSE)
		return ERL_FALSE;
	    p = p->uval.lval.tail;
	    t = t ->uval.lval.tail;
	}
	return ematch(p, t);
	
    case ERL_TUPLE: 
	{
	    int i;
	    if (erl_size(p) != erl_size(t))
		return ERL_FALSE;
	    else {
		for(i=0; i<erl_size(p); i++)
		    if (ematch(p->uval.tval.elems[i],t->uval.tval.elems[i]) == ERL_FALSE)
			return ERL_FALSE;
		    return ERL_TRUE;
	    }
	}
	break;
	
    case ERL_BINARY: 
	{
	    int i;
	    if ((i = p->uval.bval.size) != t->uval.bval.size)
		return ERL_FALSE;
	    else
		return (memcmp(p->uval.bval.b,t->uval.bval.b,i)==0) ? ERL_TRUE : ERL_FALSE;
	}
	break;
	
    case ERL_INTEGER:
	return (p->uval.ival.i == t->uval.ival.i) ? ERL_TRUE : ERL_FALSE;
	break;
	
    case ERL_SMALL_BIG:
    case ERL_U_SMALL_BIG:
    /* This case can't happend since it is impossible
    * to create a bignum from the C code.
	*/
	return ERL_FALSE; 
	break;
	
    case ERL_FLOAT:
#if defined(VXWORKS) && CPU == PPC860
	{
	    int erl_fp_compare(unsigned *a, unsigned *b);
	    return (erl_fp_compare((unsigned *)&(p->uval.fval.f),
		(unsigned *)&(t->uval.fval.f)) == 0) 
		? ERL_TRUE : ERL_FALSE;
	}
#else
	return (p->uval.fval.f == t->uval.fval.f) ? ERL_TRUE : ERL_FALSE;
#endif
	break;
    default:
	return ERL_FALSE;
	break;
  }
  
  /* erl_err_msg("ematch: Unknown type == %c\n", type_p);   */
  return ERL_FALSE;
  
} /* ematch */


int erl_match(ETERM *p, ETERM *t)
{
    int i; 
    
    if ((i = ematch(p, t)) == ERL_FALSE)
	undo_bindings();
    release_chain();
    return i;
    
} /* erl_match */

#endif
