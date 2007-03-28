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
#include "global.h"
#include "erl_process.h"


#ifdef NO_FPE_SIGNALS

void
erts_sys_init_float(void)
{
# ifdef SIGFPE
    sys_sigset(SIGFPE, SIG_IGN); /* Ignore so we can test for NaN and Inf */
# endif
}

static ERTS_INLINE void set_current_fp_exception(void)
{
    /* nothing to do */
}

#else  /* !NO_FPE_SIGNALS */

#ifdef ERTS_SMP
static erts_tsd_key_t fpe_key;

/* once-only initialisation early in the main thread (via erts_sys_init_float()) */
static void erts_init_fp_exception(void)
{
    /* XXX: the wrappers prevent using a pthread destructor to
       deallocate the key's value; so when/where do we do that? */
    erts_tsd_key_create(&fpe_key);
}

void erts_thread_init_fp_exception(void)
{
    int *fpe = erts_alloc(ERTS_ALC_T_FP_EXCEPTION, sizeof(*fpe));
    erts_tsd_set(fpe_key, fpe);
}

static ERTS_INLINE volatile int *erts_thread_get_fp_exception(void)
{
    return (volatile int*)erts_tsd_get(fpe_key);
}
#else /* !SMP */
#define erts_init_fp_exception()	/*empty*/
static volatile int fp_exception;
#define erts_thread_get_fp_exception()	(&fp_exception)
#endif /* SMP */

volatile int *erts_get_current_fp_exception(void)
{
    Process *c_p;

    c_p = erts_get_current_process();
    if (c_p)
	return &c_p->fp_exception;
    return erts_thread_get_fp_exception();
}

static void set_current_fp_exception(void)
{
    volatile int *fpexnp = erts_get_current_fp_exception();
    ASSERT(fpexnp != NULL);
    *fpexnp = 1;
}

/* Is there no standard identifier for Darwin/MacOSX ? */
#if defined(__APPLE__) && defined(__MACH__) && !defined(__DARWIN__)
#define __DARWIN__ 1
#endif

#if (defined(__i386__) || defined(__x86_64__)) && defined(__GNUC__)

static void unmask_x87(void)
{
    unsigned short cw;
    __asm__ __volatile__("fstcw %0" : "=m"(cw));
    cw &= ~(0x01|0x04|0x08);   /* unmask IM, ZM, OM */
    __asm__ __volatile__("fldcw %0" : : "m"(cw));
}

static void unmask_sse2(void)
{
    unsigned int mxcsr;
    __asm__ __volatile__("stmxcsr %0" : "=m"(mxcsr));
    mxcsr &= ~(0x003F|0x0680); /* clear exn flags, unmask OM, ZM, IM (not PM, UM, DM) */
    __asm__ __volatile__("ldmxcsr %0" : : "m"(mxcsr));
}

#if defined(__x86_64__) || defined(__DARWIN__)
static inline int cpu_has_sse2(void) { return 1; }
#else /* !__x86_64__ */
/*
 * Check if an x86-32 processor has SSE2.
 */
static unsigned int xor_eflags(unsigned int mask)
{
    unsigned int eax, edx;

    eax = mask;			/* eax = mask */
    __asm__("pushfl\n\t"
	    "popl %0\n\t"	/* edx = original EFLAGS */
	    "xorl %0, %1\n\t"	/* eax = mask ^ EFLAGS */
	    "pushl %1\n\t"
	    "popfl\n\t"		/* new EFLAGS = mask ^ original EFLAGS */
	    "pushfl\n\t"
	    "popl %1\n\t"	/* eax = new EFLAGS */
	    "xorl %0, %1\n\t"	/* eax = new EFLAGS ^ old EFLAGS */
	    "pushl %0\n\t"
	    "popfl"		/* restore original EFLAGS */
	    : "=d"(edx), "=a"(eax)
	    : "1"(eax));
    return eax;
}

static __inline__ unsigned int cpuid_eax(unsigned int op)
{
    unsigned int eax;
    __asm__("cpuid"
	    : "=a"(eax)
	    : "0"(op)
	    : "bx", "cx", "dx");
    return eax;
}

static __inline__ unsigned int cpuid_edx(unsigned int op)
{
    unsigned int eax, edx;
    __asm__("cpuid"
	    : "=a"(eax), "=d"(edx)
	    : "0"(op)
	    : "bx", "cx");
    return edx;
}

/* The AC bit, bit #18, is a new bit introduced in the EFLAGS
 * register on the Intel486 processor to generate alignment
 * faults. This bit cannot be set on the Intel386 processor.
 */
static __inline__ int is_386(void)
{
    return ((xor_eflags(1<<18) >> 18) & 1) == 0;
}

/* Newer x86 processors have a CPUID instruction, as indicated by
 * the ID bit (#21) in EFLAGS being modifiable.
 */
static __inline__ int has_CPUID(void)
{
    return (xor_eflags(1<<21) >> 21) & 1;
}

static int cpu_has_sse2(void)
{
    unsigned int maxlev, features;
    static int has_sse2 = -1;

    if (has_sse2 >= 0)
	return has_sse2;
    has_sse2 = 0;

    if (is_386())
	return 0;
    if (!has_CPUID())
	return 0;
    maxlev = cpuid_eax(0);
    /* Intel A-step Pentium had a preliminary version of CPUID.
       It also didn't have SSE2. */
    if ((maxlev & 0xFFFFFF00) == 0x0500)
	return 0;
    /* If max level is zero then CPUID cannot report any features. */
    if (maxlev == 0)
	return 0;
    features = cpuid_edx(1);
    has_sse2 = (features & (1 << 26)) != 0;

    return has_sse2;
}
#endif /* !__x86_64__ */

static void unmask_fpe(void)
{
    unmask_x87();
    if (cpu_has_sse2())
	unmask_sse2();
}

void erts_restore_fpu(void)
{
    __asm__ __volatile__("fninit");
    unmask_x87();
}

#elif defined(__sparc__) && defined(__linux__)

static void unmask_fpe(void)
{
    unsigned long fsr;

    __asm__("st %%fsr, %0" : "=m"(fsr));
    fsr &= ~(0x1FUL << 23);	/* clear FSR[TEM] field */
    fsr |= (0x1AUL << 23);	/* enable NV, OF, DZ exceptions */
    __asm__ __volatile__("ld %0, %%fsr" : : "m"(fsr));
}

#elif (defined(__powerpc__) && defined(__linux__)) || (defined(__ppc__) && defined(__DARWIN__))

#if defined(__linux__)
#include <sys/prctl.h>

static void set_fpexc_precise(void)
{
    if (prctl(PR_SET_FPEXC, PR_FP_EXC_PRECISE) < 0) {
	perror("PR_SET_FPEXC");
	exit(1);
    }
}

#elif defined(__DARWIN__)

#include <mach/mach.h>
#include <pthread.h>

/*
 * FE0 FE1	MSR bits
 *  0   0	floating-point exceptions disabled
 *  0   1	floating-point imprecise nonrecoverable
 *  1   0	floating-point imprecise recoverable
 *  1   1	floating-point precise mode
 *
 * Apparently:
 * - Darwin 5.5 (MacOS X <= 10.1) starts with FE0 == FE1 == 0,
 *   and resets FE0 and FE1 to 0 after each SIGFPE.
 * - Darwin 6.0 (MacOS X 10.2) starts with FE0 == FE1 == 1,
 *   and does not reset FE0 or FE1 after a SIGFPE.
 */
#define FE0_MASK	(1<<11)
#define FE1_MASK	(1<<8)

/* a thread cannot get or set its own MSR bits */
static void *fpu_fpe_enable(void *arg)
{
    thread_t t = *(thread_t*)arg;
    struct ppc_thread_state state;
    unsigned int state_size = PPC_THREAD_STATE_COUNT;

    if (thread_get_state(t, PPC_THREAD_STATE, (natural_t*)&state, &state_size) != KERN_SUCCESS) {
	perror("thread_get_state");
	exit(1);
    }
    if ((state.srr1 & (FE1_MASK|FE0_MASK)) != (FE1_MASK|FE0_MASK)) {
#if 0
	/* This would also have to be performed in the SIGFPE handler
	   to work around the MSR reset older Darwin releases do. */
	state.srr1 |= (FE1_MASK|FE0_MASK);
	thread_set_state(t, PPC_THREAD_STATE, (natural_t*)&state, state_size);
#else
	fprintf(stderr, "srr1 == 0x%08x, your Darwin is too old\n", state.srr1);
	exit(1);
#endif
    }
    return NULL; /* Ok, we appear to be on Darwin 6.0 or later */
}

static void set_fpexc_precise(void)
{
    thread_t self = mach_thread_self();
    pthread_t enabler;

    if (pthread_create(&enabler, NULL, fpu_fpe_enable, &self)) {
	perror("pthread_create");
    } else if (pthread_join(enabler, NULL)) {
	perror("pthread_join");
    }
}

#endif

static void set_fpscr(unsigned int fpscr)
{
    union {
	double d;
	unsigned int fpscr[2];
    } u;
    u.fpscr[0] = 0xFFF80000;
    u.fpscr[1] = fpscr;
    __asm__ __volatile__("mtfsf 255,%0" : : "f"(u.d));
}

static void unmask_fpe(void)
{
    set_fpexc_precise();
    set_fpscr(0x80|0x40|0x10);	/* VE, OE, ZE; not UE or XE */
}

#else

#define unmask_fpe()   fpsetmask(FP_X_INV | FP_X_OFL | FP_X_DZ)

#endif

#if (defined(__linux__) && (defined(__x86_64__) || defined(__i386__))) || (defined(__DARWIN__) && defined(__i386__)) || (defined(__FreeBSD__) && (defined(__x86_64__) || defined(__i386__))) || (defined(__sun__) && defined(__x86_64__))
#include <ucontext.h>

#if defined(__linux__) && defined(__x86_64__)
#define mc_pc(mc)	((mc)->gregs[REG_RIP])
typedef mcontext_t *erts_mcontext_ptr_t;
#elif defined(__linux__) && defined(__i386__)
#define mc_pc(mc)	((mc)->gregs[REG_EIP])
typedef mcontext_t *erts_mcontext_ptr_t;
#elif defined(__DARWIN__) && defined(__i386__)
#define mc_pc(mc)	((mc)->ss.eip)
typedef mcontext_t erts_mcontext_ptr_t;
#elif defined(__FreeBSD__) && defined(__x86_64__)
#define mc_pc(mc)	((mc)->mc_rip)
typedef mcontext_t *erts_mcontext_ptr_t;
#elif defined(__FreeBSD__) && defined(__i386__)
#define mc_pc(mc)	((mc)->mc_eip)
typedef mcontext_t *erts_mcontext_ptr_t;
#elif defined(__sun__) && defined(__x86_64__)
#define mc_pc(mc)	((mc)->gregs[REG_RIP])
typedef mcontext_t *erts_mcontext_ptr_t;
#endif

static void skip_sse2_insn(erts_mcontext_ptr_t mc)
{
    unsigned char *pc0 = (unsigned char*)mc_pc(mc);
    unsigned char *pc = pc0;
    unsigned int opcode;
    unsigned int nr_skip_bytes;

    opcode = *pc++;
    switch (opcode) {
    case 0x66: case 0xF2: case 0xF3:
	opcode = *pc++;
    }
#if defined(__x86_64__)
    if ((opcode & 0xF0) == 0x40)
	opcode = *pc++;
#endif
    do {
	switch (opcode) {
	case 0x0F:
	    opcode = *pc++;
	    switch (opcode) {
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
	} while (pc0 < pc);
	fprintf(stderr, "\r\n");
	abort();
    } while (0);

    /* Past the opcode. Parse and skip the mod/rm and sib bytes. */
    opcode = *pc++;
    switch ((opcode >> 6) & 3) {	/* inspect mod */
    case 0:
	switch (opcode & 7) {		/* inspect r/m */
	case 4:
	    opcode = *pc++;		/* sib */
	    switch (opcode & 7) {	/* inspect base */
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
	switch (opcode & 7) {		/* inspect r/m */
	case 4:
	    pc += 1;			/* sib */
	    break;
	}
	break;
    case 2:
	nr_skip_bytes += 4;		/* disp32 */
	switch (opcode & 7) {		/* inspect r/m */
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
    mc_pc(mc) = (long)pc;
}
#endif /* (__linux__ && (__x86_64__ || __i386__)) || (__DARWIN__ && __i386__) || (__FreeBSD__ && (__x86_64__ || __i386__)) || (__sun__ && __x86_64__) */

#if (defined(__linux__) && (defined(__i386__) || defined(__x86_64__) || defined(__sparc__) || defined(__powerpc__))) || (defined(__DARWIN__) && (defined(__i386__) || defined(__ppc__))) || (defined(__FreeBSD__) && (defined(__x86_64__) || defined(__i386__))) || (defined(__sun__) && defined(__x86_64__))

#if defined(__linux__) && defined(__i386__)
#include <asm/sigcontext.h>
#elif defined(__FreeBSD__) && defined(__x86_64__)
#include <sys/types.h>
#include <machine/fpu.h>
#elif defined(__FreeBSD__) && defined(__i386__)
#include <sys/types.h>
#include <machine/npx.h>
#endif
#include <ucontext.h>
#include <string.h>

static void fpe_sig_action(int sig, siginfo_t *si, void *puc)
{
    ucontext_t *uc = puc;
#if defined(__linux__)
#if defined(__x86_64__)
    mcontext_t *mc = &uc->uc_mcontext;
    fpregset_t fpstate = mc->fpregs;
    /* A failed SSE2 instruction will restart. To avoid
       looping, we must update RIP to skip the instruction
       (leaving garbage in the destination).
       The alternative is to mask SSE2 exceptions now and
       unmask them again later in erts_check_fpe(), but that
       relies too much on other code being cooperative. */
    if (fpstate->mxcsr & 0x000D) { /* OE|ZE|IE; see unmask_sse2() */
	fpstate->mxcsr &= ~(0x003F|0x0680);
	skip_sse2_insn(mc);
    }
    fpstate->swd &= ~0xFF;
#elif defined(__i386__)
    mcontext_t *mc = &uc->uc_mcontext;
    fpregset_t fpstate = mc->fpregs;
    if ((fpstate->status >> 16) == X86_FXSR_MAGIC &&
	((struct _fpstate*)fpstate)->mxcsr & 0x000D) {
	((struct _fpstate*)fpstate)->mxcsr &= ~(0x003F|0x0680);
	skip_sse2_insn(mc);
    }
    fpstate->sw &= ~0xFF;
#elif defined(__sparc__)
    /* on SPARC the 3rd parameter points to a sigcontext not a ucontext */
    struct sigcontext *sc = (struct sigcontext*)puc;
    sc->si_regs.pc = sc->si_regs.npc;
    sc->si_regs.npc = (unsigned long)sc->si_regs.npc + 4;
#elif defined(__powerpc__)
#if defined(__powerpc64__)
    mcontext_t *mc = &uc->uc_mcontext;
    unsigned long *regs = &mc->gp_regs[0];
#else
    mcontext_t *mc = uc->uc_mcontext.uc_regs;
    unsigned long *regs = &mc->gregs[0];
#endif
    regs[PT_NIP] += 4;
    regs[PT_FPSCR] = 0x80|0x40|0x10;	/* VE, OE, ZE; not UE or XE */
#endif
#elif defined(__DARWIN__) && defined(__i386__)
    mcontext_t mc = uc->uc_mcontext;
    if (mc->fs.fpu_mxcsr & 0x000D) {
	mc->fs.fpu_mxcsr &= ~(0x003F|0x0680);
	skip_sse2_insn(mc);
    }
    *(unsigned short *)&mc->fs.fpu_fsw &= ~0xFF;
#elif defined(__DARWIN__) && defined(__ppc__)
    mcontext_t mc = uc->uc_mcontext;
    mc->ss.srr0 += 4;
    mc->fs.fpscr = 0x80|0x40|0x10;
#elif defined(__FreeBSD__) && defined(__x86_64__)
    mcontext_t *mc = &uc->uc_mcontext;
    struct savefpu *savefpu = (struct savefpu*)&mc->mc_fpstate;
    struct envxmm *envxmm = &savefpu->sv_env;
    if (envxmm->en_mxcsr & 0x000D) {
	envxmm->en_mxcsr &= ~(0x003F|0x0680);
	skip_sse2_insn(mc);
    }
    envxmm->en_sw &= ~0xFF;
#elif defined(__FreeBSD__) && defined(__i386__)
    mcontext_t *mc = &uc->uc_mcontext;
    union savefpu *savefpu = (union savefpu*)&mc->mc_fpstate;
    if (mc->mc_fpformat == _MC_FPFMT_XMM) {
	struct envxmm *envxmm = &savefpu->sv_xmm.sv_env;
	if (envxmm->en_mxcsr & 0x000D) {
	    envxmm->en_mxcsr &= ~(0x003F|0x0680);
	    skip_sse2_insn(mc);
	}
	envxmm->en_sw &= ~0xFF;
    } else {
	struct env87 *env87 = &savefpu->sv_87.sv_env;
	env87->en_sw &= ~0xFF;
    }
#elif defined(__sun__) && defined(__x86_64__)
    mcontext_t *mc = &uc->uc_mcontext;
    struct fpchip_state *fpstate = &mc->fpregs.fp_reg_set.fpchip_state;
    if (fpstate->mxcsr & 0x000D) {
	fpstate->mxcsr &= ~(0x003F|0x0680);
	skip_sse2_insn(mc);
    }
    fpstate->sw &= ~0xFF;
#endif
    set_current_fp_exception();
}

static void erts_thread_catch_fp_exceptions(void)
{
    struct sigaction act;
    memset(&act, 0, sizeof act);
    act.sa_sigaction = fpe_sig_action;
    act.sa_flags = SA_SIGINFO;
    sigaction(SIGFPE, &act, NULL);
    unmask_fpe();
}

#else  /* !((__linux__ && (__i386__ || __x86_64__ || __powerpc__)) || (__DARWIN__ && (__i386__ || __ppc__))) */

static void fpe_sig_handler(int sig)
{
    set_current_fp_exception();
}

static void erts_thread_catch_fp_exceptions(void)
{
    sys_sigset(SIGFPE, fpe_sig_handler);
    unmask_fpe();
}

#endif /* (__linux__ && (__i386__ || __x86_64__ || __powerpc__)) || (__DARWIN__ && (__i386__ || __ppc__))) */

/* once-only initialisation early in the main thread */
void erts_sys_init_float(void)
{
    erts_init_fp_exception();
    erts_thread_catch_fp_exceptions();
}

#endif /* NO_FPE_SIGNALS */

void erts_thread_init_float(void)
{
#ifdef ERTS_SMP
    /* This allows Erlang schedulers to leave Erlang-process context
       and still have working FP exceptions. XXX: is this needed? */
    erts_thread_init_fp_exception();
#endif

#if !defined(NO_FPE_SIGNALS) && (defined(__DARWIN__) || defined(__FreeBSD__))
    /* Darwin (7.9.0) does not appear to propagate FP exception settings
       to a new thread from its parent. So if we want FP exceptions, we
       must manually re-enable them in each new thread.
       FreeBSD 6.1 appears to suffer from a similar issue. */
    erts_thread_catch_fp_exceptions();
#endif
}

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
    volatile int *fpexnp = erts_get_current_fp_exception();
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
    __ERTS_FP_CHECK_INIT(fpexnp);
    *fp = strtod(buf, &t);
    __ERTS_FP_ERROR_THOROUGH(fpexnp, *fp, return -1);
    if (t != s) {		/* Whole string not scanned */
	/* Try again with other radix char */
	*dp = (*dp == '.') ? ',' : '.';
	errno = 0;
	__ERTS_FP_CHECK_INIT(fpexnp);
	*fp = strtod(buf, &t);
	__ERTS_FP_ERROR_THOROUGH(fpexnp, *fp, return -1);
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
    set_current_fp_exception();
    return 1;
}
