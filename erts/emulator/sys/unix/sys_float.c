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

#if (defined(__i386__) || defined(__x86_64__)) && defined(__GNUC__)

static void unmask_x87(void)
{
    unsigned short cw;
    __asm__ __volatile__("fstcw %0" : "=m"(cw));
    cw &= ~(0x01|0x04|0x08);   /* unmask IM, ZM, OM */
    __asm__ __volatile__("fldcw %0" : : "m"(cw));
}

#if defined(__x86_64__)
static void unmask_sse2(void)
{
    unsigned int mxcsr;
    __asm__ __volatile__("stmxcsr %0" : "=m"(mxcsr));
    mxcsr &= ~(0x003F|0x0780); /* clear exn flags, unmask exns (not PE, UE) */
    __asm__ __volatile__("ldmxcsr %0" : : "m"(mxcsr));
}
#else
#define unmask_sse2()	do{}while(0)
#endif

static void unmask_fpe(void)
{
    unmask_x87();
    unmask_sse2();
}

void erts_restore_fpu(void)
{
    __asm__ __volatile__("fninit");
    unmask_x87();
}

#elif defined(__powerpc__) && defined(__linux__)

#include <sys/prctl.h>

static void set_fpexc(unsigned int val)
{
    if( prctl(PR_SET_FPEXC, val) < 0 ) {
	perror("PR_SET_FPEXC");
	exit(1);
    }
}

static void set_fpscr(unsigned int fpscr)
{
    union {
	double d;
	unsigned int fpscr[2];
    } u;
    u.fpscr[0] = 0xFFF80000;
    u.fpscr[1] = fpscr;
    __asm__ __volatile__("lfd%U0 0,%0; mtfsf 255,0" : : "m"(u.d) : "fr0");
}

static void unmask_fpe(void)
{
    set_fpexc(PR_FP_EXC_PRECISE);
    set_fpscr(0x80|0x40|0x10);	/* VE, OE, ZE; not UE or XE */
}

#else

#define unmask_fpe()   fpsetmask(FP_X_INV | FP_X_OFL | FP_X_DZ)

#endif

#if defined(__linux__) && defined(__x86_64__)
static void skip_sse2_insn(mcontext_t *mc)
{
    unsigned char *pc0 = (unsigned char*)(mc->gregs[REG_RIP]);
    unsigned char *pc = pc0;
    unsigned int opcode;
    unsigned int nr_skip_bytes;

    opcode = *pc++;
    switch( opcode ) {
    case 0x66: case 0xF2: case 0xF3:
	opcode = *pc++;
    }
    if( (opcode & 0xF0) == 0x40 )
	opcode = *pc++;
    do {
	switch( opcode ) {
	case 0x0F:
	    opcode = *pc++;
	    switch( opcode ) {
	    case 0x2A: /* cvtpi2ps,cvtsi2sd,cvtsi2ss /r */
	    case 0x2C: /* cvttpd2pi,cvttps2pi,cvttsd2si,cvtss2si /r */
	    case 0x2D: /* cvtpd2pi,cvtps2pi,cvtsd2si,cvtss2si /r */
	    case 0x2E: /* ucomisd,ucomiss /r */
	    case 0x2F: /* comisd,comiss /r */
	    case 0x51: /* sqrtpd,sqrtps,sqrtsd,sqrtss /r */
	    case 0x58: /* addpd,addps,addsd,addss /r */
	    case 0x59: /* mulpd,mulps,mulsd,mulss /r */
	    case 0x5A: /* cvtpd2ps,cvtps2pd,cvtsd2ss,cvtss2sd /r */
	    case 0x5B: /* cvtdq2ps,cvtps2dq,cvttps2dq /r */
	    case 0x5C: /* subpd,subps,subsd,subss /r */
	    case 0x5D: /* minpd,minps,minsd,minss /r */
	    case 0x5E: /* divpd,divps,divsd,divss /r */
	    case 0x5F: /* maxpd,maxps,maxsd,maxss /r */
	    case 0xE6: /* cvtpd2dq,cvttpd2dq /r */
		nr_skip_bytes = 0;
		continue;
	    case 0xC2: /* cmppd,cmpps,cmpsd,cmpss /r /ib */
		nr_skip_bytes = 1;
		continue;
	    }
	}
	fprintf(stderr, "%s: unexpected code at %p:", __FUNCTION__, pc0);
	do {
	    fprintf(stderr, " %02X", *pc0++);
	} while( pc0 < pc );
	fprintf(stderr, "\r\n");
	abort();
    } while( 0 );

    /* Past the opcode. Parse and skip the mod/rm and sib bytes. */
    opcode = *pc++;
    switch( (opcode >> 6) & 3 ) {	/* inspect mod */
    case 0:
	switch( opcode & 7 ) {		/* inspect r/m */
	case 4:
	    opcode = *pc++;		/* sib */
	    switch( opcode & 7 ) {	/* inspect base */
	    case 5:
		nr_skip_bytes += 4;	/* disp32 */
		break;
	    }
	    break;
	case 5:
	    nr_skip_bytes += 4;		/* disp32 */
	    break;
	}
	break;
    case 1:
	nr_skip_bytes += 1;		/* disp8 */
	switch( opcode & 7 ) {		/* inspect r/m */
	case 4:
	    pc += 1;			/* sib */
	    break;
	}
	break;
    case 2:
	nr_skip_bytes += 4;		/* disp32 */
	switch( opcode & 7 ) {		/* inspect r/m */
	case 4:
	    pc += 1;			/* sib */
	    break;
	}
	break;
    case 3:
	break;
    }

    /* Past mod/rm and sib. Skip any disp, and /ib for cmp{pd,ps,sd,ss}. */
    pc += nr_skip_bytes;

    /* The longest instruction handled above is 11 bytes. So there is
       no need to check the 15-byte instruction length limit here. */

    /* Done. */
    mc->gregs[REG_RIP] = (long)pc;
}
#endif /* __linux__ && __x86_64__ */

#if defined(__linux__) && (defined(__i386__) || defined(__x86_64__) || defined(__powerpc__))

#include <ucontext.h>

static void fpe_sig_action(int sig, siginfo_t *si, void *puc)
{
    ucontext_t *uc = puc;
    mcontext_t *mc = &uc->uc_mcontext;
#if defined(__x86_64__)
    fpregset_t fpstate = mc->fpregs;
    /* A failed SSE2 instruction will restart. To avoid
       looping, we must update RIP to skip the instruction
       (leaving garbage in the destination).
       The alternative is to mask SSE2 exceptions now and
       unmask them again later in erts_check_fpe(), but that
       relies too much on other code being cooperative. */
    if( fpstate->mxcsr & 0x000F ) {
	fpstate->mxcsr &= ~(0x003F|0x0780);
	skip_sse2_insn(mc);
    }
    fpstate->swd &= ~0xFF;
#elif defined(__i386__)
    fpregset_t fpstate = mc->fpregs;
    fpstate->sw &= ~0xFF;
#elif defined(__powerpc__)
    unsigned long *regs = &mc->regs->gpr[0];
    regs[PT_NIP] += 4;
    regs[PT_FPSCR] = 0x80|0x40|0x10;	/* VE, OE, ZE; not UE or XE */
#endif
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

#else  /* !(__linux__ && (__i386__ || __x86_64__ || __powerpc__)) */

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

#endif /* __linux__ && (__i386__ || __x86_64__ || __powerpc__) */

#endif /* NO_FPE_SIGNALS */

/* The following check is incorporated from the Vee machine */
    
#define ISDIGIT(d) ((d) >= '0' && (d) <= '9')

/* 
 ** Convert a double to ascii format 0.dddde[+|-]ddd
 ** return number of characters converted
 **
 ** These two functions should maybe use localeconv() to pick up
 ** the current radix character, but since it is uncertain how
 ** expensive such a system call is, and since no-one has heard
 ** of other radix characters than '.' and ',' an ad-hoc 
 ** low execution time solution is used instead.
 */

int
sys_double_to_chars(double fp, char *buf)
{
    char *s = buf;
    
    (void) sprintf(buf, "%.20e", fp);
    /* Search upto decimal point */
    if (*s == '+' || *s == '-') s++;
    while (ISDIGIT(*s)) s++;
    if (*s == ',') *s++ = '.'; /* Replace ',' with '.' */
    /* Scan to end of string */
    while (*s) s++;
    return s-buf; /* i.e strlen(buf) */
}

/* Float conversion */

int
sys_chars_to_double(char* buf, double* fp)
{
    char *s = buf, *t, *dp;

    /* Robert says that something like this is what he really wanted:
     * (The [.,] radix test is NOT what Robert wanted - it was added later)
     *
     * 7 == sscanf(Tbuf, "%[+-]%[0-9][.,]%[0-9]%[eE]%[+-]%[0-9]%s", ....);
     * if (*s2 == 0 || *s3 == 0 || *s4 == 0 || *s6 == 0 || *s7)
     *   break;
     */

    /* Scan string to check syntax. */
    if (*s == '+' || *s == '-') s++;
    if (!ISDIGIT(*s))		/* Leading digits. */
      return -1;
    while (ISDIGIT(*s)) s++;
    if (*s != '.' && *s != ',')	/* Decimal part. */
      return -1;
    dp = s++;			/* Remember decimal point pos just in case */
    if (!ISDIGIT(*s))
      return -1;
    while (ISDIGIT(*s)) s++;
    if (*s == 'e' || *s == 'E') {
	/* There is an exponent. */
	s++;
	if (*s == '+' || *s == '-') s++;
	if (!ISDIGIT(*s))
	  return -1;
	while (ISDIGIT(*s)) s++;
    }
    if (*s)			/* That should be it */
      return -1;

#ifdef NO_FPE_SIGNALS
    errno = 0;
#endif
    ERTS_FP_CHECK_INIT();
    *fp = strtod(buf, &t);
    ERTS_FP_ERROR(*fp, return -1);
    if (t != s) {		/* Whole string not scanned */
	/* Try again with other radix char */
	*dp = (*dp == '.') ? ',' : '.';
	errno = 0;
	ERTS_FP_CHECK_INIT();
	*fp = strtod(buf, &t);
	ERTS_FP_ERROR(*fp, return -1);
    }

#ifdef DEBUG
    if (errno == ERANGE)
	fprintf(stderr, "errno = ERANGE in list_to_float\n\r");
#endif
#ifdef NO_FPE_SIGNALS
    if (errno == ERANGE)
	return -1;
#endif
/*
**  Replaces following code:
**   if (errno == ERANGE) {
**       *fp = 1.2e300;		
**       *fp = *fp / 1.5e-100;	
**   }				
*/

    return 0;
}

int
matherr(struct exception *exc)
{
    erl_fp_exception = 1;
    return 1;
}
