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
#include "eidef.h"
#include "eiext.h"
#include "putget.h"

#define abs(p) (((p)<0) ? -(p) : p)

/* long -> erl_integer */
/* note that this is the only place where data is stored Little Endian */

#ifdef EI_64BIT
int ei_encode_long(char *buf, int *index, long p)
{
    return ei_encode_longlong(buf, index, p);
}
#endif

int ei_encode_longlong(char *buf, int *index, EI_LONGLONG p)
{
    char *s = buf + *index;
    char *s0 = s;

    if ((p < 256) && (p >= 0)) {
	if (!buf) s += 2;
	else {
	    put8(s,ERL_SMALL_INTEGER_EXT);
	    put8(s,(p & 0xff));
	}
    } else if ((p <= ERL_MAX) && (p >= ERL_MIN)) {
	/* FIXME: Non optimal, could use (p <= LONG_MAX) && (p >= LONG_MIN)
	   and skip next case */
	if (!buf) s += 5;
	else {
	    put8(s,ERL_INTEGER_EXT);
	    put32be(s,p);
	}
    } else {
	/* Bignum, we don't know size, i.e. limb count needed */
	unsigned up = abs(p); /* FIXME name uabs(x) not to confuse with abs */
	if (buf) {
	    char *arityp;
	    int arity = 0;

	    put8(s,ERL_SMALL_BIG_EXT);
	    arityp = s++;	/* fill in later */
	    put8(s, p < 0);            /* save sign separately */
	    put32le(s, abs(p));        /* OBS: Little Endian, and p now positive */
	    while (up) {
		*s++ = up & 0xff; /* take lowest byte */
		up >>= 8;	  /* shift unsigned */
		arity++;
	    }
	    put8(arityp,arity);
	} else {
	    s += 3;		/* Type, arity and sign */
	    while (up) {
		s++;		/* take lowest byte */
		up >>= 8;	/* shift unsigned */
	    }
	}
    }
  
    *index += s-s0; 

    return 0; 
}

