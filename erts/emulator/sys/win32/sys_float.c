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
/* Float conversions */

#include "sys.h"
#include "signal.h"

/* global variable for floating point checks, (see sys.h) */
/* Note! This is part of the interface Machine <---> sys.c */
volatile int erl_fp_exception = 0;

static void fpe_exception(int sig);

void
erts_sys_init_float(void)
{
}

int 
sys_chars_to_double(buf, fp)
char* buf; double* fp;
{
    char *s = buf;

    /* Robert says that something like this is what he really wanted:
     *
     * 7 == sscanf(Tbuf, "%[+-]%[0-9].%[0-9]%[eE]%[+-]%[0-9]%s", ....);
     * if (*s2 == 0 || *s3 == 0 || *s4 == 0 || *s6 == 0 || *s7)
     *   break;
     */

    /* Scan string to check syntax. */
    if (*s == '+' || *s == '-')
      s++;
	    
    if (!isdigit(*s))		/* Leading digits. */
      return -1;
    while (isdigit(*s)) s++;
    if (*s++ != '.')		/* Decimal part. */
      return -1;
    if (!isdigit(*s))
      return -1;
    while (isdigit(*s)) s++;
    if (*s == 'e' || *s == 'E') {
	/* There is an exponent. */
	s++;
	if (*s == '+' || *s == '-')
	  s++;
	if (!isdigit(*s))
	  return -1;
	while (isdigit(*s)) s++;
    }
    if (*s)			/* That should be it */
      return -1;
    
    if (sscanf(buf, "%lf", fp) != 1)
	return -1;
    return 0;
}

/* 
** Convert a double to ascii format 0.dddde[+|-]ddd
** return number of characters converted
*/

int
sys_double_to_chars(fp, buf)
double fp; char* buf;
{
    (void) sprintf(buf, "%.20e", fp);
    return strlen(buf);
}

int
matherr(struct _exception *exc)
{
    erl_fp_exception = 1;
    DEBUGF(("FP exception (matherr) (0x%x) (%d)\n", exc->type, erl_fp_exception));
    return 1;
}

static void
fpe_exception(int sig)
{
    erl_fp_exception = 1;
    DEBUGF(("FP exception\n"));
}
