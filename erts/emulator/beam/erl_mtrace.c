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
 * Description:	Memory allocation trace. The trace is sent over a
 *              tcp/ip connection.
 *
 *              The trace format is not intended to be documented.
 *              Instead a library for parsing the trace will be
 *              distributed. This in order to more easily be able
 *              to make changes in the trace format. The library
 *              for parsing the trace is currently not included in
 *              the OTP distribution, but will be in the future.
 *
 * Author: 	Rickard Green
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#define ERTS_MTRACE_INTERNAL__
#include "sys.h"
#include "global.h"
#include "erl_sock.h"

#define TRACE_PRINTOUTS 0
#ifdef TRACE_PRINTOUTS
#define MSB2BITS(X) ((((unsigned)(X))+1)*8)
#endif

#ifdef USE_THREADS
#define ERL_THREADS_EMU_INTERNAL__
#include "erl_threads.h"
#define LOCK	erts_mutex_lock(mutex)
#define UNLOCK	erts_mutex_unlock(mutex)
static erts_mutex_t mutex;
#else
#define LOCK
#define UNLOCK
#endif

#define TRACE_BUF_SZ 				(16*1024)


#define UI8_MSB_EHF_SZ				ERTS_MT_UI8_MSB_EHDR_FLD_SZ
#define UI16_MSB_EHF_SZ				ERTS_MT_UI16_MSB_EHDR_FLD_SZ
#define UI32_MSB_EHF_SZ				ERTS_MT_UI32_MSB_EHDR_FLD_SZ
#define UI64_MSB_EHF_SZ				ERTS_MT_UI64_MSB_EHDR_FLD_SZ
#define UI_MSB_EHF_SZ				ERTS_MT_UI64_MSB_EHDR_FLD_SZ
#define TAG_EHF_SZ				ERTS_MT_TAG_EHDR_FLD_SZ

#define UI8_MSB_EHF_MSK				ERTS_MT_UI8_MSB_EHDR_FLD_MSK
#define UI16_MSB_EHF_MSK			ERTS_MT_UI16_MSB_EHDR_FLD_MSK
#define UI32_MSB_EHF_MSK			ERTS_MT_UI32_MSB_EHDR_FLD_MSK
#define UI_MSB_EHF_MSK				ERTS_MT_UI64_MSB_EHDR_FLD_MSK
#define UI64_MSB_EHF_MSK			ERTS_MT_UI64_MSB_EHDR_FLD_MSK
#define TAG_EHF_MSK				ERTS_MT_TAG_EHDR_FLD_MSK

#define UI8_SZ					(1)
#define UI16_SZ					(2)
#define UI32_SZ					(4)
#define UI64_SZ					(8)
#ifdef ARCH_64
#  define UI_SZ					UI64_SZ
#else
#  define UI_SZ					UI32_SZ
#endif

#define WRITE_UI8(P, V) (*(P) = (byte) ((V) & 0xff))

#define WRITE_UI16(P, V)						\
  ((P)[0] = (byte) (((V) >>  8) & 0xff),				\
   (P)[1] = (byte) ( (V)        & 0xff))

#define WRITE_UI32(P, V)						\
  ((P)[0] = (byte) (((V) >> 24) & 0xff),				\
   (P)[1] = (byte) (((V) >> 16) & 0xff),				\
   (P)[2] = (byte) (((V) >>  8) & 0xff),				\
   (P)[3] = (byte) ( (V)        & 0xff))

#define WRITE_UI64(P, V)						\
  ((P)[0] = (byte) (((V) >> 56) & 0xff),				\
   (P)[1] = (byte) (((V) >> 48) & 0xff),				\
   (P)[2] = (byte) (((V) >> 40) & 0xff),				\
   (P)[3] = (byte) (((V) >> 32) & 0xff),				\
   (P)[4] = (byte) (((V) >> 24) & 0xff),				\
   (P)[5] = (byte) (((V) >> 16) & 0xff),				\
   (P)[6] = (byte) (((V) >>  8) & 0xff),				\
   (P)[7] = (byte) ( (V)        & 0xff))

#define PUT_UI8(P, V)  (WRITE_UI8((P),  (V)), (P) += UI8_SZ)
#define PUT_UI16(P, V) (WRITE_UI16((P), (V)), (P) += UI16_SZ)
#define PUT_UI32(P, V) (WRITE_UI32((P), (V)), (P) += UI32_SZ)
#define PUT_UI64(P, V) (WRITE_UI64((P), (V)), (P) += UI64_SZ)

#define PUT_VSZ_UI16(P, M, V)						\
do {									\
    Uint16 v__ = (Uint16) (V);						\
    if (v__ >= (((Uint16) 1) << 8)) (M) = 1; else (M) = 0;		\
    switch ((M)) {							\
    case 1: *((P)++) = (byte) ((v__ >>  8) & 0xff);			\
    case 0: *((P)++) = (byte) ( v__        & 0xff);			\
    }									\
} while (0)

#define PUT_VSZ_UI32(P, M, V)						\
do {									\
    Uint32 v__ = (Uint32) (V);						\
    if (v__ >= (((Uint32) 1) << 16)) {					\
	if (v__ >= (((Uint32) 1) << 24)) (M) = 3; else (M) = 2;		\
    } else {								\
	if (v__ >= (((Uint32) 1) << 8)) (M) = 1; else (M) = 0;		\
    }									\
    switch ((M)) {							\
    case 3: *((P)++) = (byte) ((v__ >> 24) & 0xff);			\
    case 2: *((P)++) = (byte) ((v__ >> 16) & 0xff);			\
    case 1: *((P)++) = (byte) ((v__ >>  8) & 0xff);			\
    case 0: *((P)++) = (byte) ( v__        & 0xff);			\
    }									\
} while (0)

#ifdef ARCH_64

#define PUT_VSZ_UI64(P, M, V)						\
do {									\
    Uint64 v__ = (Uint64) (V);						\
    if (v__ >= (((Uint64) 1) << 32)) {					\
	if (v__ >= (((Uint64) 1) << 48)) {				\
	    if (v__ >= (((Uint64) 1) << 56)) (M) = 7; else (M) = 6;	\
	} else {							\
	    if (v__ >= (((Uint64) 1) << 40)) (M) = 5; else (M) = 4;	\
	}								\
    } else {								\
	if (v__ >= (((Uint64) 1) << 16)) {				\
	    if (v__ >= (((Uint64) 1) << 24)) (M) = 3; else (M) = 2;	\
	} else {							\
	    if (v__ >= (((Uint64) 1) << 8)) (M) = 1; else (M) = 0;	\
	}								\
    }	    								\
    switch ((M)) {							\
    case 7: *((P)++) = (byte) ((v__ >> 56) & 0xff);			\
    case 6: *((P)++) = (byte) ((v__ >> 48) & 0xff);			\
    case 5: *((P)++) = (byte) ((v__ >> 40) & 0xff);			\
    case 4: *((P)++) = (byte) ((v__ >> 32) & 0xff);			\
    case 3: *((P)++) = (byte) ((v__ >> 24) & 0xff);			\
    case 2: *((P)++) = (byte) ((v__ >> 16) & 0xff);			\
    case 1: *((P)++) = (byte) ((v__ >>  8) & 0xff);			\
    case 0: *((P)++) = (byte) ( v__        & 0xff);			\
    }									\
} while (0)

#define PUT_VSZ_UI	PUT_VSZ_UI64
#else /* #ifdef ARCH_64 */
#define PUT_VSZ_UI	PUT_VSZ_UI32
#endif /* #ifdef ARCH_64 */

#define MAKE_TBUF_SZ(SZ)						\
  (TRACE_BUF_SZ < (SZ)							\
   ? (disable_trace(1, "Internal buffer overflow", 0), 0)		\
   : (endp - tracep < (SZ) ? send_trace_buffer() : 1))


static void disable_trace(int error, char *reason, int eno);
static int send_trace_buffer(void);

#ifdef DEBUG
void
check_alloc_entry(byte *sp, byte *ep,
		  Uint16 type, int type_n,
		  Uint res, int res_n,
		  Uint size, int size_n,
		  Uint32 ti,int ti_n);
void
check_realloc_entry(byte *sp, byte *ep, int no_previous_block, int moved,
		    Uint16 type, int type_n,
		    Uint res, int res_n,
		    Uint ptr, int ptr_n,
		    Uint size, int size_n,
		    Uint32 ti,int ti_n);
void
check_free_entry(byte *sp, byte *ep,
		 Uint ptr, int ptr_n,
		 Uint32 ti,int ti_n);
void
check_time_inc_entry(byte *sp, byte *ep,
		     Uint32 secs, int secs_n,
		     Uint32 usecs, int usecs_n);
#endif



int erts_mtrace_enabled;
static erts_sock_t socket_desc;
static byte trace_buffer[TRACE_BUF_SZ];
static byte *tracep;
static byte *endp;
static SysTimeval last_tv;

char* erl_errno_id(int error);

#define INVALID_TIME_INC (0xffffffff)

static ERTS_INLINE Uint32
get_time_inc(void)
{
    Sint32 secs;
    Sint32 usecs;
    Uint32 res;
    SysTimeval tv;
    sys_gettimeofday(&tv);

    secs = tv.tv_sec - last_tv.tv_sec;
    if (tv.tv_usec >= last_tv.tv_usec)
	usecs = tv.tv_usec - last_tv.tv_usec;
    else {
	secs--;
	usecs = 1000000 + tv.tv_usec - last_tv.tv_usec;
    }

    ASSERT(0 <= usecs);
    ASSERT(usecs < 1000000);

    if (secs < 0) {
	/* Clock stepped backwards; we pretend that no time has past. */
	res = 0;
    }
    else if (secs < ERTS_MT_TIME_INC_SECS_MASK) {
	res = ((((Uint32) secs) << ERTS_MT_TIME_INC_SECS_SHIFT)
	       | (((Uint32) usecs) << ERTS_MT_TIME_INC_USECS_SHIFT));
    }
    else {
	/* Increment too large to fit in a 32-bit integer;
	   put a time inc entry in trace ... */
	if (MAKE_TBUF_SZ(UI16_SZ + 2*UI32_SZ)) {
	    byte *hdrp;
	    Uint16 hdr;
	    int secs_n, usecs_n;

	    hdrp = tracep;
	    tracep += 2;

	    PUT_VSZ_UI32(tracep, secs_n,  secs);
	    PUT_VSZ_UI32(tracep, usecs_n, usecs);

	    hdr = usecs_n;

	    hdr <<= UI32_MSB_EHF_SZ;
	    hdr |= secs_n;

	    hdr <<= TAG_EHF_SZ;
	    hdr |= ERTS_MT_TIME_INC_TAG;

	    WRITE_UI16(hdrp, hdr);
#ifdef DEBUG
	    check_time_inc_entry(hdrp, tracep,
				 (Uint32) secs, secs_n,
				 (Uint32) usecs, usecs_n);
#endif
	    res = 0;
	}
	else {
	    res = INVALID_TIME_INC;
	}
    }

    last_tv = tv;
    return res;
}


static void
disable_trace(int error, char *reason, int eno)
{
#define ENO_BUF_SZ 100
    char buf[ENO_BUF_SZ];
    char *eno_str;

    if (eno) {
	eno_str = erl_errno_id(eno);

	if (strcmp(eno_str, "unknown") == 0)
	    sprintf(buf, ": %d", eno);
	else {
	    int i;
	    buf[0] = ':'; buf[1] = ' ';
	    for (i = 2; i < ENO_BUF_SZ && eno_str[i-2]; i++)
		buf[i] = eno_str[i-2];
	}
	eno_str = buf;
    }
    else
	eno_str = "";

    erts_mtrace_enabled = 0;
    erts_sock_close(socket_desc);
    socket_desc = ERTS_SOCK_INVALID_SOCKET;
    cerr_pos = 0;
    erl_printf(erts_initialized ? CBUF : CERR,
	       "Memory trace disabled: %s%s\n", reason, eno_str);
    if (erts_initialized) {
	if (error)
	    send_error_to_logger(NIL);
	else
	    send_error_to_logger(NIL);
    }

#undef ENO_BUF_SZ
}

static int
send_trace_buffer(void)
{
    ssize_t ssz;
    size_t sz;

    sz = tracep - trace_buffer;
    tracep = trace_buffer;

    do {
	ssz = erts_sock_send(socket_desc, (void  *) tracep, sz);
	if (ssz < 0) {
	    int socket_errno = erts_sock_errno();

#ifdef EINTR
	    if (socket_errno == EINTR)
		continue;
#endif
	    disable_trace(0, "Connection lost", socket_errno);
	    return 0;
	}
	if (ssz > sz) {
	    disable_trace(1, "Unexpected error", 0);
	    return 0;
	}
	tracep += ssz;
	sz -= ssz;
    } while (sz);

    tracep = trace_buffer;
    return 1;
}

#if ERTS_ALC_N_MAX >= (1 << 16)
#error "Excessively large type numbers"
#endif


static int
write_trace_header(void)
{
    Uint32 flags;
    Uint32 hdr_sz;
    byte *hdr_szp;
    int i, no, str_len;
    const char *str;

    if (!MAKE_TBUF_SZ(5*UI32_SZ + 2*UI16_SZ))
	return 0;

    flags = 0;
#ifdef ARCH_64
    flags |= ERTS_MT_64_BIT_FLAG;
#endif

    PUT_UI32(tracep, ERTS_MT_START_WORD);
    PUT_UI32(tracep, ERTS_MT_MAJOR_VSN);
    PUT_UI32(tracep, ERTS_MT_MINOR_VSN);
    PUT_UI32(tracep, flags);
    hdr_szp = tracep;
    tracep += UI32_SZ;

    PUT_UI16(tracep, ERTS_ALC_A_MAX);
    PUT_UI16(tracep, ERTS_ALC_N_MAX);

    hdr_sz = 5*UI32_SZ + 2*UI16_SZ;

    for (i = ERTS_ALC_A_MIN; i <= ERTS_ALC_A_MAX; i++) {
	str = ERTS_ALC_A2AD(i);
	ASSERT(str);
	str_len = strlen(str);
	if (str_len >= (1 << 8)) {
	    disable_trace(1, "Excessively large allocator string", 0);
	    return 0;
	}
	    
	if (!MAKE_TBUF_SZ(UI8_SZ + 2*UI16_SZ + str_len))
	    return 0;
	hdr_sz += UI8_SZ + 2*UI16_SZ + str_len;

	PUT_UI16(tracep, ERTS_MT_ALLOCATOR_TAG);
	PUT_UI16(tracep, (Uint16) i);
	PUT_UI8( tracep, (byte) str_len);
	memcpy((void *) tracep, (void *) str, str_len);
	tracep += str_len;
    }

    for (i = ERTS_ALC_N_MIN; i <= ERTS_ALC_N_MAX; i++) {
	str = ERTS_ALC_N2TD(i);
	ASSERT(str);

	str_len = strlen(str);
	if (str_len >= (1 << 8)) {
	    disable_trace(1, "Excessively large type string", 0);
	    return 0;
	}

	no = ERTS_ALC_T2A(ERTS_ALC_N2T(i));
	if (!erts_allctrs_info[no].enabled)
	    no = ERTS_ALC_A_SYSTEM;
	ASSERT(ERTS_ALC_A_MIN <= no && no <= ERTS_ALC_A_MAX);

	if (!MAKE_TBUF_SZ(UI8_SZ + 3*UI16_SZ + str_len))
	    return 0;

	hdr_sz += UI8_SZ + 3*UI16_SZ + str_len;

	PUT_UI16(tracep, ERTS_MT_BLOCK_TYPE_TAG);
	PUT_UI16(tracep, (Uint16) i);
	PUT_UI8( tracep, (byte) str_len);
	memcpy((void *) tracep, (void *) str, str_len);
	tracep += str_len;
	PUT_UI16(tracep, no);
    }

    WRITE_UI32(hdr_szp,   hdr_sz);

    return 1;
}

static void *mtrace_alloc(ErtsAlcType_t, void *, Uint);
static void *mtrace_realloc(ErtsAlcType_t, void *, void *, Uint);
static void mtrace_free(ErtsAlcType_t, void *, void *);

static ErtsAllocatorFunctions_t real_allctrs[ERTS_ALC_A_MAX+1];

void erts_mtrace_init(char *receiver)
{
    socket_desc = ERTS_SOCK_INVALID_SOCKET;
    erts_mtrace_enabled = receiver != NULL;

    if (erts_mtrace_enabled) {
	int i;
	unsigned a, b, c, d, p;
	byte ip_addr[4];
	Uint16 port;
	
	/* Install trace functions */
	ASSERT(sizeof(erts_allctrs) == sizeof(real_allctrs));

	sys_memcpy((void *) real_allctrs,
		   (void *) erts_allctrs,
		   sizeof(erts_allctrs));

	for (i = ERTS_ALC_A_MIN; i <= ERTS_ALC_A_MAX; i++) {
	    erts_allctrs[i].alloc	= mtrace_alloc;
	    erts_allctrs[i].realloc	= mtrace_realloc;
	    erts_allctrs[i].free	= mtrace_free;
	    erts_allctrs[i].extra	= (void *) &real_allctrs[i];
	}


#ifdef USE_THREADS
	mutex = erts_mutex_sys(ERTS_MUTEX_SYS_MTRACE);
	if(!mutex || erts_mutex_set_default_atfork(mutex))
	    erl_exit(1, "Failed to initialize mtrace mutex\n");
#endif

	socket_desc = erts_sock_open();
	if (socket_desc == ERTS_SOCK_INVALID_SOCKET) {
	    disable_trace(1, "Failed to open socket", erts_sock_errno());
	    return;
	}

	if (5 != sscanf(receiver, "%u.%u.%u.%u:%u", &a, &b, &c, &d, &p)
	    || a >= (1 << 8) || b >= (1 << 8)|| c >= (1 << 8) || d >= (1 << 8)
	    || p >= (1 << 16)) {
	    disable_trace(1, "Invalid receiver address", 0);
	    return;
	}

	ip_addr[0] = (byte) a;
	ip_addr[1] = (byte) b;
	ip_addr[2] = (byte) c;
	ip_addr[3] = (byte) d; 

	port = (Uint16) p;

	if (!erts_sock_connect(socket_desc, ip_addr, 4, port)) {
	    disable_trace(1, "Failed to connect to receiver",
			  erts_sock_errno());
	    return;
	}
	sys_gettimeofday(&last_tv);
	tracep = trace_buffer;
	endp = trace_buffer + TRACE_BUF_SZ;
	write_trace_header();
    }
}

void
erts_mtrace_stop(void)
{
    LOCK;
    if (erts_mtrace_enabled) {
	Uint32 ti = get_time_inc();
    
	if (ti != INVALID_TIME_INC && MAKE_TBUF_SZ(UI16_SZ + UI32_SZ)) {
	    byte *hdrp;
	    Uint16 hdr;
	    int ti_n;

	    hdrp = tracep;
	    tracep += 2;

	    PUT_VSZ_UI32(tracep, ti_n,  ti);

	    hdr = ti_n;

	    hdr <<= TAG_EHF_SZ;
	    hdr |= ERTS_MT_STOP_TAG;

	    WRITE_UI16(hdrp, hdr);

	    if(send_trace_buffer()) {
		erts_mtrace_enabled = 0;
		erts_sock_close(socket_desc);
		socket_desc = ERTS_SOCK_INVALID_SOCKET;
	    }
	}
    }
    UNLOCK;
}

void
erts_mtrace_exit(Uint32 exit_value)
{
    LOCK;
    if (erts_mtrace_enabled) {
	Uint32 ti = get_time_inc();
    
	if (ti != INVALID_TIME_INC && MAKE_TBUF_SZ(UI16_SZ + 2*UI32_SZ)) {
	    byte *hdrp;
	    Uint16 hdr;
	    int ti_n, exit_value_n;

	    hdrp = tracep;
	    tracep += 2;

	    PUT_VSZ_UI32(tracep, exit_value_n,  exit_value);
	    PUT_VSZ_UI32(tracep, ti_n,  ti);

	    hdr = ti_n;

	    hdr <<= UI32_MSB_EHF_SZ;
	    hdr |= exit_value_n;

	    hdr <<= TAG_EHF_SZ;
	    hdr |= ERTS_MT_EXIT_TAG;

	    WRITE_UI16(hdrp, hdr);

	    if(send_trace_buffer()) {
		erts_mtrace_enabled = 0;
		erts_sock_close(socket_desc);
		socket_desc = ERTS_SOCK_INVALID_SOCKET;
	    }
	}
    }
    UNLOCK;
}

static void *
mtrace_alloc(ErtsAlcType_t n, void *extra, Uint size)
{
    ErtsAllocatorFunctions_t *real_af = (ErtsAllocatorFunctions_t *) extra;
    void *res;
    Uint32 ti;

    LOCK;

    res = (*real_af->alloc)(n, real_af->extra, size);

    if (erts_mtrace_enabled) {
	Uint16 t_no = (Uint16) n;
	ti = get_time_inc();

	if (ti != INVALID_TIME_INC
	    && MAKE_TBUF_SZ(UI8_SZ + UI16_SZ + 2*UI_SZ + UI32_SZ)) {
	    byte *hdrp;
	    Uint16 hdr;
	    int t_no_n, res_n, size_n, ti_n;

	    hdrp = tracep;
	    tracep += 2;

	    PUT_VSZ_UI16(tracep, t_no_n, t_no);
	    PUT_VSZ_UI(  tracep, res_n, res);
	    PUT_VSZ_UI(  tracep, size_n, size);
	    PUT_VSZ_UI32(tracep, ti_n, ti);

	    hdr = ti_n;

	    hdr <<= UI_MSB_EHF_SZ;
	    hdr |= size_n;

	    hdr <<= UI_MSB_EHF_SZ;
	    hdr |= res_n;

	    hdr <<= UI16_MSB_EHF_SZ;
	    hdr |= t_no_n;

	    hdr <<= TAG_EHF_SZ;
	    hdr |= ERTS_MT_ALLOC_TAG;

	    WRITE_UI16(hdrp, hdr);

#if TRACE_PRINTOUTS
	    fprintf(stderr,
		    "{alloc, {%u, %u, %u}, {%u, %u, %u, %u}}\n",

		    (unsigned) t_no, (unsigned) res, (unsigned) size,

		    MSB2BITS(t_no_n), MSB2BITS(res_n),
		    MSB2BITS(size_n), MSB2BITS(ti_n));
	    fflush(stderr);
#endif

#ifdef DEBUG
	    check_alloc_entry(hdrp, tracep,
			      t_no, t_no_n,
			      (Uint) res, res_n,
			      size, size_n,
			      ti, ti_n);
#endif

	}

    }

    UNLOCK;

    return res;
}

static void *
mtrace_realloc(ErtsAlcType_t n, void *extra, void *ptr, Uint size)
{
    ErtsAllocatorFunctions_t *real_af = (ErtsAllocatorFunctions_t *) extra;
    void *res;
    Uint32 ti;

    LOCK;

    res = (*real_af->realloc)(n, real_af->extra, ptr, size);

    if (erts_mtrace_enabled) {
	Uint16 t_no;
	int no_previous_block;
	int moved;
	ti = get_time_inc();

	no_previous_block = (ptr == NULL);
	moved = (!no_previous_block && res != ptr);

	if (ti != INVALID_TIME_INC
	    && MAKE_TBUF_SZ(2*UI16_SZ + (moved ? 3 : 2)*UI_SZ + UI32_SZ)) {
	    byte *hdrp;
	    Uint16 hdr;
	    int t_no_n, res_n, ptr_n, size_n, ti_n;

	    hdrp = tracep;
	    tracep += 2;

	    if (no_previous_block) {
		t_no = (Uint16) n;
		PUT_VSZ_UI16(tracep, t_no_n, t_no);
	    }
	    else {
		t_no = 0;
		t_no_n = 0;
	    }
	    PUT_VSZ_UI(  tracep, res_n, res);
	    if (moved)
		PUT_VSZ_UI(  tracep, ptr_n, ptr);
	    else
		ptr_n = 0;
	    PUT_VSZ_UI(  tracep, size_n, size);
	    PUT_VSZ_UI32(tracep, ti_n, ti);

	    hdr = ti_n;

	    hdr <<= UI_MSB_EHF_SZ;
	    hdr |= size_n;

	    if (moved) {
		hdr <<= UI_MSB_EHF_SZ;
		hdr |= ptr_n;
	    }

	    hdr <<= UI_MSB_EHF_SZ;
	    hdr |= res_n;

	    if (no_previous_block) {
		hdr <<= UI16_MSB_EHF_SZ;
		hdr |= t_no_n;
	    }

	    hdr <<= TAG_EHF_SZ;
	    if (no_previous_block)
		hdr |= ERTS_MT_REALLOC_NPB_TAG;
	    else if (moved)
		hdr |= ERTS_MT_REALLOC_MV_TAG;
	    else
		hdr |= ERTS_MT_REALLOC_NMV_TAG;

	    WRITE_UI16(hdrp, hdr);

#if TRACE_PRINTOUTS
	    if (moved)
		fprintf(stderr,
			"{realloc_mv, {%u, %u, %u, %u}, {%u, %u, %u, %u, %u}}\n",

			(unsigned) t_no, (unsigned) res,
			(unsigned) ptr, (unsigned) size,

			MSB2BITS(t_no_n), MSB2BITS(res_n),
			MSB2BITS(ptr_n), MSB2BITS(size_n),
			MSB2BITS(ti_n));
	    else
		fprintf(stderr,
			"{realloc_nmv, {%u, %u, %u}, {%u, %u, %u, %u}}\n",

			(unsigned) t_no, (unsigned) ptr, (unsigned) size,

			MSB2BITS(t_no_n), MSB2BITS(ptr_n),
			MSB2BITS(size_n), MSB2BITS(ti_n));
		
	    fflush(stderr);
#endif

#ifdef DEBUG
	    check_realloc_entry(hdrp, tracep, no_previous_block, moved,
				t_no, t_no_n,
				(Uint) res, res_n,
				(Uint) ptr, ptr_n,
				size, size_n,
				ti, ti_n);
#endif

	}
    }

    UNLOCK;

    return res;

}

static void
mtrace_free(ErtsAlcType_t n, void *extra, void *ptr)
{
    ErtsAllocatorFunctions_t *real_af = (ErtsAllocatorFunctions_t *) extra;
    Uint32 ti;

    LOCK;

    (*real_af->free)(n, real_af->extra, ptr);

    if (erts_mtrace_enabled) {
	ti = get_time_inc();

	if (ti != INVALID_TIME_INC
	    && MAKE_TBUF_SZ(2*UI16_SZ + UI_SZ + UI32_SZ)) {
	    byte *hdrp;
	    Uint16 hdr;
	    int ptr_n, ti_n;

	    hdrp = tracep;
	    tracep += 2;

	    PUT_VSZ_UI(  tracep, ptr_n,  ptr);
	    PUT_VSZ_UI32(tracep, ti_n,   ti);

	    hdr = ti_n;

	    hdr <<= UI_MSB_EHF_SZ;
	    hdr |= ptr_n;

	    hdr <<= TAG_EHF_SZ;
	    hdr |= ERTS_MT_FREE_TAG;

	    WRITE_UI16(hdrp, hdr);


#if TRACE_PRINTOUTS
	    fprintf(stderr,
		    "{free, {%u}, {%u, %u}}\n",
		    (unsigned) ptr, MSB2BITS(ptr_n), MSB2BITS(ti_n));
		
	    fflush(stderr);
#endif

#ifdef DEBUG
	    check_free_entry(hdrp, tracep,
			     (Uint) ptr, ptr_n,
			     ti, ti_n);
#endif
	}

    }

    UNLOCK;
}

#ifdef DEBUG

#define GET_UI16(P) ((P) += UI16_SZ, \
		     (((Uint16) (*((P) - 2) << 8)) | ((Uint16) (*((P) - 1)))))

static void
check_ui(Uint16 *hdrp, byte **pp, Uint ui, int msb,
	 Uint16 f_mask, Uint16 f_size)
{
    Uint x;
    int n;

    ASSERT((msb & ~f_mask) == 0);

    n = (int) (*hdrp & f_mask);

    ASSERT(n == msb);

    *hdrp >>= f_size;

    x = 0;
    switch (n) {
#ifdef ARCH_64
    case 7: x |= *((*pp)++); x <<= 8;
    case 6: x |= *((*pp)++); x <<= 8;
    case 5: x |= *((*pp)++); x <<= 8;
    case 4: x |= *((*pp)++); x <<= 8;
#endif
    case 3: x |= *((*pp)++); x <<= 8;
    case 2: x |= *((*pp)++); x <<= 8;
    case 1: x |= *((*pp)++); x <<= 8;
    case 0: x |= *((*pp)++); break;
    default: ASSERT(0);
    }

    ASSERT(x == ui);
}


void
check_alloc_entry(byte *sp, byte *ep,
		  Uint16 t_no, int t_no_n,
		  Uint res, int res_n,
		  Uint size, int size_n,
		  Uint32 ti,int ti_n)
{
    byte *p = sp;
    Uint16 hdr;

    ASSERT((ERTS_MT_ALLOC_TAG & ~TAG_EHF_MSK) == 0);

    hdr = GET_UI16(p);
    ASSERT((hdr & TAG_EHF_MSK) == ERTS_MT_ALLOC_TAG);
    hdr >>= TAG_EHF_SZ;

    check_ui(&hdr, &p, t_no, t_no_n, UI16_MSB_EHF_MSK, UI16_MSB_EHF_SZ);
    check_ui(&hdr, &p, res,  res_n,  UI_MSB_EHF_MSK,   UI_MSB_EHF_SZ);
    check_ui(&hdr, &p, size, size_n, UI_MSB_EHF_MSK,   UI_MSB_EHF_SZ);
    check_ui(&hdr, &p, ti,   ti_n,   UI32_MSB_EHF_MSK, UI32_MSB_EHF_SZ);

    ASSERT(hdr == 0);
    ASSERT(p == ep);
}

void
check_realloc_entry(byte *sp, byte *ep, int no_previous_block, int moved,
		    Uint16 t_no, int t_no_n,
		    Uint res, int res_n,
		    Uint ptr, int ptr_n,
		    Uint size, int size_n,
		    Uint32 ti,int ti_n)
{
    byte *p = sp;
    Uint16 hdr;

    ASSERT((ERTS_MT_REALLOC_MV_TAG & ~TAG_EHF_MSK) == 0);
    ASSERT((ERTS_MT_REALLOC_NMV_TAG & ~TAG_EHF_MSK) == 0);

    hdr = GET_UI16(p);
    if (no_previous_block) {
	ASSERT((hdr & TAG_EHF_MSK) == ERTS_MT_REALLOC_NPB_TAG);
    }
    if (moved) {
	ASSERT((hdr & TAG_EHF_MSK) == ERTS_MT_REALLOC_MV_TAG);
    }
    else {
	ASSERT((hdr & TAG_EHF_MSK) == ERTS_MT_REALLOC_NMV_TAG);
    }
    hdr >>= TAG_EHF_SZ;

    if (no_previous_block)
	check_ui(&hdr, &p, t_no, t_no_n, UI16_MSB_EHF_MSK, UI16_MSB_EHF_SZ);
    check_ui(&hdr, &p, res,  res_n,  UI_MSB_EHF_MSK,   UI_MSB_EHF_SZ);
    if (moved)
	check_ui(&hdr, &p, ptr,  ptr_n,  UI_MSB_EHF_MSK,   UI_MSB_EHF_SZ);
    check_ui(&hdr, &p, size, size_n, UI_MSB_EHF_MSK,   UI_MSB_EHF_SZ);
    check_ui(&hdr, &p, ti,   ti_n,   UI32_MSB_EHF_MSK, UI32_MSB_EHF_SZ);

    ASSERT(hdr == 0);
    ASSERT(p == ep);
}

void
check_free_entry(byte *sp, byte *ep,
		 Uint ptr, int ptr_n,
		 Uint32 ti,int ti_n)
{
    byte *p = sp;
    Uint16 hdr;

    ASSERT((ERTS_MT_FREE_TAG & ~TAG_EHF_MSK) == 0);

    hdr = GET_UI16(p);
    ASSERT((hdr & TAG_EHF_MSK) == ERTS_MT_FREE_TAG);
    hdr >>= TAG_EHF_SZ;

    check_ui(&hdr, &p, ptr,  ptr_n,  UI_MSB_EHF_MSK,   UI_MSB_EHF_SZ);
    check_ui(&hdr, &p, ti,   ti_n,   UI32_MSB_EHF_MSK, UI32_MSB_EHF_SZ);

    ASSERT(hdr == 0);
    ASSERT(p == ep);

}

void
check_time_inc_entry(byte *sp, byte *ep,
		     Uint32 secs, int secs_n,
		     Uint32 usecs, int usecs_n)
{
    byte *p = sp;
    Uint16 hdr;

    ASSERT((ERTS_MT_TIME_INC_TAG & ~TAG_EHF_MSK) == 0);

    hdr = GET_UI16(p);
    ASSERT((hdr & TAG_EHF_MSK) == ERTS_MT_TIME_INC_TAG);
    hdr >>= TAG_EHF_SZ;

    check_ui(&hdr, &p, secs,  secs_n,  UI32_MSB_EHF_MSK, UI32_MSB_EHF_SZ);
    check_ui(&hdr, &p, usecs, usecs_n, UI32_MSB_EHF_MSK, UI32_MSB_EHF_SZ);

    ASSERT(hdr == 0);
    ASSERT(p == ep);

}

#endif /* #ifdef DEBUG */

