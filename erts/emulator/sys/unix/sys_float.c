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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"


volatile int erl_fp_exception = 0;

#ifdef NO_FPE_SIGNALS

void
erts_sys_init_float(void)
{
# ifdef SIGFPE
    sys_sigset(SIGFPE, SIG_IGN); /* Ignore so we can test for NaN and Inf */
# endif
}

#else  /* !NO_FPE_SIGNALS */

#if defined(__i386__) && defined(__GNUC__)

static void unmask_fpe(void)
{
    unsigned short cw;
    __asm__ __volatile__("fstcw %0" : "=m"(cw));
    cw &= ~(0x01|0x04|0x08);   /* unmask IM, ZM, OM */
    __asm__ __volatile__("fldcw %0" : : "m"(cw));
}

void erts_restore_x87(void)
{
    __asm__ __volatile__("fninit");
    unmask_fpe();
}

#else  /* !(__i386__ && __GNUC__) */

#define unmask_fpe()   fpsetmask(FP_X_INV | FP_X_OFL | FP_X_DZ)

#endif /* __i386__ && __GNUC__ */

#if defined(__linux__) && defined(__i386__)

#include <ucontext.h>

static void fpe_sig_action(int sig, siginfo_t *si, void *puc)
{
    ucontext_t *uc = puc;
    mcontext_t *mc = &uc->uc_mcontext;
    fpregset_t fpstate = mc->fpregs;
    fpstate->sw &= ~0xFF;
    erl_fp_exception = 1;
}

void erts_sys_init_float(void)
{
    struct sigaction act;
    memset(&act, 0, sizeof act);
    act.sa_sigaction = fpe_sig_action;
    act.sa_flags = SA_SIGINFO;
    sigaction(SIGFPE, &act, NULL);
    unmask_fpe();
}

#else  /* !(__linux__ && __i386__) */

static void fpe_sig_handler(int sig)
{
    erl_fp_exception = 1;
}

void
erts_sys_init_float(void)
{
    sys_sigset(SIGFPE, fpe_sig_handler);
    unmask_fpe();
}

#endif /* __linux__ && __i386__ */

#endif /* NO_FPE_SIGNALS */

/* 
 ** Convert a double to ascii format 0.dddde[+|-]ddd
 ** return number of characters converted
 */

int
sys_double_to_chars(double fp, char* buf)
{
    (void) sprintf(buf, "%.20e", fp);
    return strlen(buf);
}

/* Float conversion */

int
sys_chars_to_double(char* buf, double* fp)
{
    char *s = buf;

    /* The following check is incorporated from the Vee machine */
    
#define ISDIGIT(d) ((d) >= '0' && (d) <= '9')

    /* Robert says that something like this is what he really wanted:
     *
     * 7 == sscanf(Tbuf, "%[+-]%[0-9].%[0-9]%[eE]%[+-]%[0-9]%s", ....);
     * if (*s2 == 0 || *s3 == 0 || *s4 == 0 || *s6 == 0 || *s7)
     *   break;
     */

    /* Scan string to check syntax. */
    if (*s == '+' || *s == '-')
      s++;
	    
    if (!ISDIGIT(*s))		/* Leading digits. */
      return -1;
    while (ISDIGIT(*s)) s++;
    if (*s++ != '.')		/* Decimal part. */
      return -1;
    if (!ISDIGIT(*s))
      return -1;
    while (ISDIGIT(*s)) s++;
    if (*s == 'e' || *s == 'E') {
	/* There is an exponent. */
	s++;
	if (*s == '+' || *s == '-')
	  s++;
	if (!ISDIGIT(*s))
	  return -1;
	while (ISDIGIT(*s)) s++;
    }
    if (*s)			/* That should be it */
      return -1;

#if defined(HP9000) || defined(ISC32)
    errno = 0;
#endif
    ERTS_FP_CHECK_INIT();
    *fp = atof(buf);
    ERTS_FP_ERROR(*fp, return -1);
#ifdef DEBUG
    if (errno == ERANGE)
	fprintf(stderr, "errno = ERANGE in list_to_float\n\r");
#endif

#if defined(HP9000) || defined(ISC32)
    if (errno == ERANGE)
	return -1;
/*
**  Replaces following code:
**   if (errno == ERANGE) {
**       *fp = 1.2e300;		
**       *fp = *fp / 1.5e-100;	
**   }				
*/

#endif
    return 0;
}

int
matherr(struct exception *exc)
{
    erl_fp_exception = 1;
    return 1;
}
