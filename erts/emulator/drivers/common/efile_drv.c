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
 * Purpose: Provides file and directory operations.
 *
 * This file is generic, and does the work of decoding the commands
 * and encoding the responses.  System-specific functions are found in
 * the unix_efile.c and win_efile.c files.
 */

/* Operations */

#define FILE_OPEN		 1 /* Essential for startup */
#define FILE_READ		 2
#define FILE_LSEEK		 3
#define FILE_WRITE		 4
#define FILE_FSTAT		 5 /* Essential for startup */
#define FILE_PWD                 6 /* Essential for startup */
#define FILE_READDIR             7 /* Essential for startup */
#define FILE_CHDIR               8
#define FILE_FSYNC               9
#define FILE_MKDIR              10
#define FILE_DELETE             11
#define FILE_RENAME             12
#define FILE_RMDIR              13
#define FILE_TRUNCATE           14
#define FILE_READ_FILE          15 /* Essential for startup */
#define FILE_WRITE_INFO		16
#define FILE_PREAD              17
#define FILE_PWRITE             18
#define FILE_LSTAT            	19
#define FILE_READLINK        	20
#define FILE_LINK             	21
#define FILE_SYMLINK          	22
#define FILE_CLOSE		23
#define FILE_PWRITEV		24
#define FILE_PREADV		25
#define FILE_SETOPT		26
#define FILE_IPREAD             27

/* Return codes */

#define FILE_RESP_OK         0
#define FILE_RESP_ERROR      1
#define FILE_RESP_DATA       2
#define FILE_RESP_NUMBER     3
#define FILE_RESP_INFO       4
#define FILE_RESP_NUMERR     5
#define FILE_RESP_LDATA      6
#define FILE_RESP_N2DATA     7
#define FILE_RESP_EOF        8

/* Options */

#define FILE_OPT_DELAYED_WRITE 0
#define FILE_OPT_READ_AHEAD    1

/* IPREAD variants */

#define IPREAD_S32BU_P32BU 0

/* Limits */

#define FILE_SEGMENT_READ  (256*1024)
#define FILE_SEGMENT_WRITE (256*1024)


#include <stdlib.h>
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "sys.h"
#include "erl_driver.h"
#include "erl_efile.h"
#include "erl_threads.h"
#include "zlib.h"
#include "gzio.h"
#include <ctype.h>

extern int erts_async_max_threads;
extern void erl_exit(int n, char *fmt, _DOTS_);


/*#define TRACE 1*/
#ifdef TRACE
#    define TRACE_C(c) (putchar(c))
#    define TRACE_S(s) (fputs((s), stdout))
#    define TRACE_F(args) (printf args)
#else
#    define TRACE_C(c) ((void)(0))
#    define TRACE_S(s) ((void)(0))
#    define TRACE_F(args) ((void)(0))
#endif


#ifdef USE_THREADS
#define MUTEX_CREATE(pm) do {*(pm) = erts_mutex_create();} while (0)
#define MUTEX_DESTROY(m) \
do {int code = erts_mutex_destroy(m); \
    if (code) erl_exit(1, "erts_mutex_destroy returned %d in " \
                       __FILE__ ":%d", code, __LINE__);} while (0)
#define MUTEX_LOCK(m) \
do {if (erts_async_max_threads > 0) { \
        int code = erts_mutex_lock(m); \
        if (code) erl_exit(1, "erts_mutex_lock returned %d in " \
                           __FILE__ ":%d", code, __LINE__);}} while (0)
#define MUTEX_UNLOCK(m) \
do {if (erts_async_max_threads > 0) { \
    int code = erts_mutex_unlock(m); \
        if (code) erl_exit(1, "erts_mutex_unlock returned %d in " \
                           __FILE__ ":%d", code, __LINE__);}} while (0)
#else
#define MUTEX_CREATE(pm) \
do {*(pm) = (erts_mutex_t) (((long double *) NULL) + 1);} while (0)
#define MUTEX_DESTROY(m) do {} while (0)
#define MUTEX_LOCK(m)    do {} while (0)
#define MUTEX_UNLOCK(m)  do {} while (0)
#endif



#if 0
/* Experimental, for forcing all file operations to use the same thread. */
static unsigned file_fixed_key = 1;
#define KEY(desc) (&file_fixed_key)
#else
#define KEY(desc) (&(desc)->key)
#endif



#if     MAXPATHLEN >= BUFSIZ
#define    RESBUFSIZE  MAXPATHLEN+1
#else
#define    RESBUFSIZE  BUFSIZ
#endif

#define GET_TIME(i, b) \
    (i).year  = get_int32((b) + 0 * 4); \
    (i).month = get_int32((b) + 1 * 4); \
    (i).day   = get_int32((b) + 2 * 4); \
    (i).hour  = get_int32((b) + 3 * 4); \
    (i).minute = get_int32((b) + 4 * 4); \
    (i).second = get_int32((b) + 5 * 4)

#define PUT_TIME(i, b) \
  put_int32((i).year,  (b) + 0 * 4); \
  put_int32((i).month, (b) + 1 * 4); \
  put_int32((i).day,   (b) + 2 * 4); \
  put_int32((i).hour,  (b) + 3 * 4); \
  put_int32((i).minute,(b) + 4 * 4); \
  put_int32((i).second,(b) + 5 * 4)



typedef unsigned char uchar;

static ErlDrvData file_start(ErlDrvPort port, char* command);
static int file_init(void);
static void file_stop(ErlDrvData);
static void file_output(ErlDrvData, char* buf, int len);
static int file_control(ErlDrvData, unsigned int command, 
			char* buf, int len, char **rbuf, int rlen);
static void file_timeout(ErlDrvData);
static void file_outputv(ErlDrvData, ErlIOVec*);
static void file_async_ready(ErlDrvData, ErlDrvThreadData);
static void file_flush(ErlDrvData);



enum e_timer {timer_idle, timer_again, timer_write};

struct t_data;

typedef struct {
    int             fd;
    ErlDrvPort      port;
    unsigned        key;      /* Async queue key */
    unsigned        flags;    /* Original flags from FILE_OPEN. */
    void          (*invoke)(void *);
    struct t_data  *d;
    void          (*free)(void *);
    struct t_data  *cq_head;  /* Queue of incoming commands */
    struct t_data  *cq_tail;  /* -""- */
    enum e_timer    timer_state;
    Uint32          read_bufsize;
    ErlDrvBinary   *read_binp;
    Uint32          read_offset;
    Uint32          read_size;
    Uint32          write_bufsize;
    unsigned long   write_delay;
    int             write_error;
    Efile_error     write_errInfo;
    erts_mutex_t    q_lock;   /* Locks the driver queue,
			       * and the fields below. */
    Uint32          write_buffered;
} file_descriptor;


static int error_reply(file_descriptor*, Efile_error* errInfo);

struct erl_drv_entry efile_driver_entry = {
    file_init,
    file_start,
    file_stop,
    file_output,
    NULL,
    NULL,
    "efile",
    NULL,
    NULL,
    file_control,
    file_timeout,
    file_outputv,
    file_async_ready,
    file_flush
};



static int thread_short_circuit;

#define DRIVER_ASYNC(level, desc, f_invoke, data, f_free) \
if (thread_short_circuit >= (level)) { \
    (*(f_invoke))(data); \
    file_async_ready((ErlDrvData)(desc), (data)); \
} else { \
    driver_async((desc)->port, KEY(desc), (f_invoke), (data), (f_free)); \
}



struct t_pbuf_spec {
    Uint32 offset;
    Uint32 size;
};

struct t_pwritev {
    ErlDrvPort         port;
    erts_mutex_t      *q_lockp;
    Uint32             size;
    Uint32             free_size;
    Uint32             cnt;
    Uint32             n;
    struct t_pbuf_spec specs[1];
};

struct t_preadv {
    ErlIOVec eiov;
    Uint32   n;
    Uint32   cnt;
    Uint32   size;
    Uint32   offsets[1];
};

struct t_data
{
    struct t_data *next;
    int            command;
    int            level;
    void         (*invoke)(void *);
    void         (*free)(void *);
    int            again;
    int            reply;
    int            result_ok;
    Efile_error    errInfo;
    int            flags;
    int            fd;
    /**/
    Efile_info info;
    EFILE_DIR_HANDLE dir_handle; /* Handle to open directory. */
    ErlDrvBinary *bin;
    char *buf;
    unsigned location;
    int drive;
    int n;
    int offset;
    unsigned bytesRead;		/* Bytes read from the file. */
    /**/
    union {
	struct {
	    int      offset;
	    int      origin;
	    unsigned location;
	} lseek;
	struct {
	    ErlDrvPort    port;
	    erts_mutex_t *q_lockp;
	    Uint32        size;
	    Uint32        free_size;
	    Uint32        reply_size;
	} writev;
	struct t_pwritev pwritev;
	struct t_preadv  preadv;
	struct {
	    ErlDrvBinary *binp;
	    Uint32        bin_offset;
	    Uint32        bin_size;
	    Uint32        size;
	} read;
	struct {
	    ErlDrvBinary *binp;
	    Uint32        size;
	    Uint32        offset;
	    char          name[1];
	} read_file;
    } c;
    char b[1];
};



/*********************************************************************
 * ErlIOVec manipulation functions.
 */

/* char *EV_CHAR_P(ErlIOVec *ev, int p, int q) */
#define EV_CHAR_P(ev, p, q) \
    (((char *)(ev)->iov[(q)].iov_base) + (p))

/* char *EV_UCHAR_P(ErlIOVec *ev, int p, int q) */
#define EV_UCHAR_P(ev, p, q) \
    (((unsigned char *)(ev)->iov[(q)].iov_base) + (p))

/* int EV_GET_CHAR(ErlIOVec *ev, char *p, int *pp, int *qp) */
#define EV_GET_CHAR(ev, p, pp, qp) \
    (*(pp)+1 <= (ev)->iov[*(qp)].iov_len \
     ? (*(p) = *EV_CHAR_P(ev, *(pp), *(qp)), \
        *(pp) = (*(pp)+1 < (ev)->iov[*(qp)].iov_len \
                 ? *(pp)+1 \
                 : ((*(qp))++, 0)), \
        !0) \
     : 0)

/* int EV_GET_UINT32(ErlIOVec *ev, Uint32 *p, int *pp, int *qp) */
#define EV_GET_UINT32(ev, p, pp, qp) \
    (*(pp)+4 <= (ev)->iov[*(qp)].iov_len \
     ? (*(p) = (*EV_UCHAR_P(ev, *(pp), *(qp)) << 24) \
                | (*EV_UCHAR_P(ev, *(pp)+1, *(qp)) << 16) \
                | (*EV_UCHAR_P(ev, *(pp)+2, *(qp)) << 8)\
                | *EV_UCHAR_P(ev, *(pp)+3, *(qp)), \
        *(pp) = (*(pp)+4 < (ev)->iov[*(qp)].iov_len \
                 ? *(pp)+4 \
                 : ((*(qp))++, 0)), \
        !0) \
     : 0)



#if 0

static void ev_clear(ErlIOVec *ev) {
    ASSERT(ev);
    ev->size = 0;
    ev->vsize = 0;
    ev->iov = NULL;
    ev->binv = NULL;
}

/* Assumes that ->iov and ->binv were allocated with sys_alloc().
 */
static void ev_free(ErlIOVec *ev) {
    if (! ev) {
	return;
    }
    if (ev->vsize > 0) {
	int i;
	ASSERT(ev->iov);
	ASSERT(ev->binv);
	for (i = 0; i < ev->vsize; i++) {
	    if (ev->binv[i]) {
		driver_free_binary(ev->binv[i]);
	    }
	}
	sys_free(ev->iov);
	sys_free(ev->binv);
    }
}

/* Copy the contents from source to dest.
 * Data in binaries is not copied, just the pointers; 
 * and refc is incremented.
 */
static ErlIOVec *ev_copy(ErlIOVec *dest, ErlIOVec *source) {
    int *ip;
    ASSERT(dest);
    ASSERT(source);
    if (source->vsize == 0) {
	/* Empty source */
	ev_clear(dest);
	return dest;
    }
    /* Allocate ->iov and ->binv */
    dest->iov = 
	sys_alloc_from(200, sizeof(*dest->iov) * source->vsize);
    if (! dest->iov) {
	return NULL;
    }
    dest->binv = 
	sys_alloc_from(200, sizeof(*dest->binv) * source->vsize);
    if (! dest->binv) {
	sys_free(dest->iov);
	return NULL;
    }
    dest->size = source->size;
    /* Copy one vector element at the time. 
     * Use *ip as an alias for dest->vsize to improve readabiliy.
     * Keep dest consistent in every iteration by using 
     * dest->vsize==*ip as loop variable.
     */
    for (ip = &dest->vsize, *ip = 0;  *ip < source->vsize;  (*ip)++) {
	if (source->iov[*ip].iov_len == 0) {
	    /* Empty vector element */
	    dest->iov[*ip].iov_len = 0;
	    dest->iov[*ip].iov_base = NULL;
	    dest->binv[*ip] = NULL;
	} else {
	    /* Non empty vector element */
	    if (source->binv[*ip]) {
		/* Contents in binary - copy pointers and increment refc */
		dest->iov[*ip] = source->iov[*ip];
		dest->binv[*ip] = source->binv[*ip];
		source->binv[*ip]->refc++;
	    } else {
		/* Contents not in binary - allocate new binary and copy data */
		if (! (dest->binv[*ip] = 
		       driver_alloc_binary(source->iov[*ip].iov_len))) {
		    goto failed;
		}
		sys_memcpy(dest->binv[*ip]->orig_bytes,
			   source->iov[*ip].iov_base,
			   source->iov[*ip].iov_len);
		dest->iov[*ip].iov_base = dest->binv[*ip]->orig_bytes;
		dest->iov[*ip].iov_len = source->iov[*ip].iov_len;
	    }
	}
    }
    return dest;
 failed:
    ev_free(dest);
    return NULL;
}

#endif

/*********************************************************************
 * Command queue functions
 */

static void cq_enq(file_descriptor *desc, struct t_data *d) {
    ASSERT(d);
    if (desc->cq_head) {
	ASSERT(desc->cq_tail);
	ASSERT(!desc->cq_tail->next);
	desc->cq_tail = desc->cq_tail->next = d;
    } else {
	ASSERT(desc->cq_tail == NULL);
	desc->cq_head = desc->cq_tail = d;
    }
    d->next = NULL;
}

static struct t_data *cq_deq(file_descriptor *desc) {
    struct t_data *d = desc->cq_head;
    ASSERT(d || (!d && !desc->cq_tail));
    if (d) {
	ASSERT(!d->next || (d->next && desc->cq_tail != d));
	if ((desc->cq_head = d->next) == NULL) {
	    ASSERT(desc->cq_tail == d);
	    desc->cq_tail = NULL;
	}
    }	
    return d;
}



/*********************************************************************
 * Driver entry point -> init
 */
static int 
file_init(void)
{
    char *p = getenv("ERL_EFILE_THREAD_SHORT_CIRCUIT");
    thread_short_circuit = p ? atoi(p) : 0;
    return 0;
}

/*********************************************************************
 * Driver entry point -> start
 */
static ErlDrvData 
file_start(ErlDrvPort port, char* command) 

{
    file_descriptor* desc;

    if ((desc = (file_descriptor*)
	 sys_alloc_from(200, sizeof(file_descriptor))) == NULL)
	return ERL_DRV_ERROR_GENERAL;
    desc->fd = -1;
    desc->port = port;
    desc->key = (unsigned) port;
    desc->flags = 0;
    desc->invoke = NULL;
    desc->d = NULL;
    desc->free = NULL;
    desc->cq_head = NULL;
    desc->cq_tail = NULL;
    desc->timer_state = timer_idle;
    desc->read_bufsize = 0;
    desc->read_binp = NULL;
    desc->read_offset = 0;
    desc->read_size = 0;
    desc->write_delay = 0L;
    desc->write_bufsize = 0;
    desc->write_error = 0;
    MUTEX_CREATE(&desc->q_lock);
    if (desc->q_lock == NULL)
	return ERL_DRV_ERROR_GENERAL;
    desc->write_buffered = 0;
    return (ErlDrvData) desc;
}

static void free_data(void *data)
{
    sys_free(data);
}

static void do_close(int flags, int fd) {
    if (flags & EFILE_COMPRESSED) {
	gzclose((gzFile)(fd));
    } else {
	efile_closefile(fd);
    }
}

static void invoke_close(void *data)
{
    struct t_data *d = (struct t_data *) data;
    d->again = 0;
    do_close(d->flags, d->fd);
}

/*********************************************************************
 * Driver entry point -> stop
 */
static void 
file_stop(ErlDrvData e)
{
    file_descriptor* desc = (file_descriptor*)e;

    TRACE_C('p');

    if (desc->fd >= 0) {
	do_close(desc->flags, desc->fd);
	desc->fd = -1;
	desc->flags = 0;
    }
    if (desc->read_binp) {
	driver_free_binary(desc->read_binp);
    }
    MUTEX_DESTROY(desc->q_lock);
    sys_free(desc);
}


/*
 * Sends back an error reply to Erlang.
 */

static void error_reply_posix(file_descriptor *desc, int posix_errno) {
    char response[256];		/* Response buffer. */
    char* s;
    char* t;
    
    /*
     * Contents of buffer sent back:
     *
     * +-----------------------------------------+
     * | FILE_RESP_ERROR | Posix error id string |
     * +-----------------------------------------+
     */

    TRACE_C('E');

    response[0] = FILE_RESP_ERROR;
    for (s = erl_errno_id(posix_errno), t = response+1; *s; s++, t++)
	*t = tolower(*s);
    driver_output2(desc->port, response, t-response, NULL, 0);
}

static void numerr_reply_posix(file_descriptor *desc, 
			       Uint32 num, int posix_errno) {
    char response[256];		/* Response buffer. */
    char* s;
    char* t;
    
    /*
     * Contents of buffer sent back:
     *
     * +----------------------------------------------------------------------+
     * | FILE_RESP_ERROR | 32-bit number (big-endian) | Posix error id string |
     * +----------------------------------------------------------------------+
     */

    TRACE_C('N');

    response[0] = FILE_RESP_NUMERR;
    put_int32(num, response+1);
    for (s = erl_errno_id(posix_errno), t = response+1+4; *s; s++, t++)
	*t = tolower(*s);
    driver_output2(desc->port, response, t-response, NULL, 0);
}



static int error_reply(file_descriptor *desc, 
		       Efile_error *errInfo) /* The error codes. */
{
    error_reply_posix(desc, errInfo->posix_errno);
    return 0;
}

static int numerr_reply(file_descriptor *desc, 
			Uint32 num, Efile_error *errInfo) /* The error codes. */
{
    numerr_reply_posix(desc, num, errInfo->posix_errno);
    return 0;
}

static int ok_reply(file_descriptor *desc) {
    uchar c = FILE_RESP_OK;

    TRACE_C('K');

    driver_output2(desc->port, &c, 1, NULL, 0);
    return 0;
}

static int reply(desc, ok, errInfo)
file_descriptor* desc;
int ok;
Efile_error* errInfo;
{
    if (!ok)
	error_reply(desc, errInfo);
    else 
	ok_reply(desc);
    return 0;
}

static int numeric_reply(desc, result)
file_descriptor* desc; 
int result;
{
    uchar tmp[5];

    /*
     * Contents of buffer sent back:
     *
     * +-----------------------------------------------+
     * | FILE_RESP_NUMBER | 32-bit number (big-endian) |
     * +-----------------------------------------------+
     */

    TRACE_C('R');

    tmp[0] = FILE_RESP_NUMBER;
    put_int32(result, tmp+1);
    driver_output2(desc->port, tmp, sizeof(tmp), NULL, 0);
    return 0;
}

#if 0
static void again_reply(file_descriptor *desc) {
    uchar tmp[1];
    tmp[0] = FILE_RESP_AGAIN;
    driver_output2(desc->port, tmp, sizeof(tmp), NULL, 0);
}
#endif

static void ev_reply(file_descriptor *desc, char response, ErlIOVec *ev) {
    unsigned char tmp[1];
    /* Data arriving at the Erlang process:
     * [Response, Binary0, Binary1, .... | BinaryN-1]
     */
    tmp[0] = response;
    driver_outputv(desc->port, tmp, sizeof(tmp), ev, 0);
}

static void data_reply(file_descriptor *desc, 
		       ErlDrvBinary *binp, Uint32 offset, Uint32 len) {
    unsigned char header[5];
    /* Data arriving at the Erlang process:
     * [?FILE_RESP_DATA, 32-bit length (big-endian) | Data]
     */
    TRACE_C('D');

    header[0] = FILE_RESP_DATA;
    put_int32(len, header+1);
    driver_output_binary(desc->port, header, sizeof(header),
			 binp, offset, len);
}

#if 0
static void num2_data_reply(file_descriptor *desc,
			    Uint32 x, Uint32 y,
			    ErlDrvBinary *binp, Uint32 offset, Uint32 len) {
    unsigned char header[1 + 3*sizeof(Uint32)];
    /* Data arriving at the Erlang process:
     * [?FILE_RESP_N2DATA, 32-bit x (big-endian), 32-bit y (big-endian),
     *  32-bit length (big-endian) | Data]
     */

    header[0] = FILE_RESP_N2DATA;
    put_int32(x,   header+1+0*sizeof(Uint32));
    put_int32(y,   header+1+1*sizeof(Uint32));
    if (! binp) {
	put_int32(0, header+1+2*sizeof(Uint32));
	driver_output2(desc->port, header, sizeof(header), 
			header+sizeof(header), 0);
    } else {
	put_int32(len, header+1+2*sizeof(Uint32));
	driver_output_binary(desc->port, header, sizeof(header),
			     binp, offset, len);
    }
}
#endif

static int eof_reply(file_descriptor *desc) {
    uchar c = FILE_RESP_EOF;

    driver_output2(desc->port, &c, 1, NULL, 0);
    return 0;
}


 
static void invoke_name(void *data, int (*f)(Efile_error *, char *))
{
    struct t_data *d = (struct t_data *) data;
    char *name = (char *) d->b;

    d->again = 0;
    d->result_ok = (*f)(&d->errInfo, name);
}

static void invoke_mkdir(void *data)
{
    invoke_name(data, efile_mkdir);
}

static void invoke_rmdir(void *data)
{
    invoke_name(data, efile_rmdir);
}

static void invoke_delete_file(void *data)
{
    invoke_name(data, efile_delete_file);
}

static void invoke_chdir(void *data)
{
    invoke_name(data, efile_chdir);
}

static void invoke_fsync(void *data)
{
    struct t_data *d = (struct t_data *) data;
    int fd = d->fd;

    d->again = 0;
    d->result_ok = efile_fsync(&d->errInfo, fd);
}

static void invoke_truncate(void *data)
{
    struct t_data *d = (struct t_data *) data;
    int fd = d->fd;

    d->again = 0;
    d->result_ok = efile_truncate_file(&d->errInfo, &fd, d->flags);
}

static void invoke_read(void *data)
{
    struct t_data *d = (struct t_data *) data;
    int status, segment;
    int size, read_size;

    segment = d->again && d->c.read.bin_size >= 2*FILE_SEGMENT_READ;
    if (segment) {
	size = FILE_SEGMENT_READ;
    } else {
	size = d->c.read.bin_size;
    }
    read_size = size;
    if (d->flags & EFILE_COMPRESSED) {
	read_size = gzread((gzFile)d->fd, 
			   d->c.read.binp->orig_bytes + d->c.read.bin_offset,
			   size);
	status = (read_size != -1);
	if (!status) {
	    d->errInfo.posix_errno = EIO;
	}
    } else {
	status = efile_read(&d->errInfo, d->flags, d->fd,
			    d->c.read.binp->orig_bytes + d->c.read.bin_offset,
			    size,
			    &read_size);
    }
    if ( (d->result_ok = status)) {
	ASSERT(read_size <= size);
	d->c.read.bin_offset += read_size;
	if (read_size < size || !segment) {
	    d->c.read.bin_size = 0;
	    d->again = 0;
	} else {
	    d->c.read.bin_size -= read_size;
	}
    } else {
	d->again = 0;
    }
}

static void free_read(void *data)
{
    struct t_data *d = (struct t_data *) data;

    driver_free_binary(d->c.read.binp);
    sys_free(d);
}

static void invoke_read_file(void *data)
{
    struct t_data *d = (struct t_data *) data;
    unsigned size, read_size;
    int chop;

    size = d->c.read_file.size;
    chop = d->again && size >= FILE_SEGMENT_READ*2;
    if (chop) {
	size = FILE_SEGMENT_READ;
    }
    read_size = size;
    d->result_ok = 
	efile_read(&d->errInfo, 
		   EFILE_MODE_READ, 
		   d->fd, 
		   d->c.read_file.binp->orig_bytes + d->c.read_file.offset,
		   size, 
		   &read_size);
    if (d->result_ok) {
	if (read_size != size) {
	    ASSERT(read_size < size);
	    d->c.read_file.size = d->c.read_file.offset + read_size;
	} else {
	    d->c.read_file.offset += read_size;
	    d->c.read_file.size -= read_size;
	    if (chop) {
		return;
	    }
	}
    }
    d->again = 0;
    efile_closefile(d->fd);
}

static void free_read_file(void *data)
{
    struct t_data *d = (struct t_data *) data;

    driver_free_binary(d->c.read_file.binp);
    sys_free(d);
}



static void invoke_pread(void *data)
{
    struct t_data *d = (struct t_data *) data;

    d->again = 0;
    d->result_ok = efile_pread(&d->errInfo, d->fd, d->offset,
			       d->bin->orig_bytes, 
			       d->n, &d->bytesRead);
}

static void invoke_preadv(void *data)
{
    struct t_data   *d = (struct t_data *) data;
    struct t_preadv *c = &d->c.preadv;
    ErlIOVec        *ev = &c->eiov;
    unsigned         bytes_read_so_far = 0;
    unsigned char   *p = (unsigned char *)ev->iov[0].iov_base 
	+ sizeof(Uint32)*(c->cnt + 1);

    while (c->cnt < c->n) {
	unsigned read_size = ev->iov[1 + c->cnt].iov_len - c->size;
	unsigned bytes_read = 0;
	int chop = d->again 
	    && bytes_read_so_far + read_size >= 2*FILE_SEGMENT_READ;
	if (chop) {
	    ASSERT(bytes_read_so_far < FILE_SEGMENT_READ);
	    read_size = FILE_SEGMENT_READ + FILE_SEGMENT_READ/2
		- bytes_read_so_far;
	}
	if ( (d->result_ok 
	      = efile_pread(&d->errInfo, 
			    d->fd,
			    c->offsets[c->cnt] + c->size,
			    ev->iov[1 + c->cnt].iov_base + c->size,
			    read_size,
			    &bytes_read))) {
	    bytes_read_so_far += bytes_read;
	    if (chop && bytes_read == read_size) {
		c->size += bytes_read;
		return;
	    }
	    ASSERT(bytes_read <= read_size);
	    ev->iov[1 + c->cnt].iov_len = bytes_read + c->size;
	    ev->size += bytes_read + c->size;
	    put_int32(bytes_read + c->size, p); p += sizeof(Uint32);
	    c->size = 0;
	    c->cnt++;
	    if (d->again 
		&& bytes_read_so_far >= FILE_SEGMENT_READ
		&& c->cnt < c->n) {
		return;
	    }
	} else {
	    /* In case of a read error, ev->size will not be correct,
	     * which does not matter since no read data is returned
	     * to Erlang.
	     */
	    break;
	}
    }					
    d->again = 0;
}

static void free_preadv(void *data) {
    struct t_data *d = data;
    int            i;
    ErlIOVec      *ev = &d->c.preadv.eiov;
    
    for(i = 0; i < ev->vsize; i++) {
	driver_free_binary(ev->binv[i]);
    }
    sys_free(d);
}

#if 0
static void invoke_write(void *data)
{
    struct t_data *d = (struct t_data *) data;

    int status;			/* Status of write operation. */

    if (d->flags & EFILE_COMPRESSED) {
	status = gzwrite((gzFile)d->fd, (char *) d->b, d->n) == d->n;
    } else {
	status = efile_write(&d->errInfo, d->flags, d->fd,
			     (char *) d->b, d->n);
    }

    d->result_ok = status;
}
#endif

/* invoke_writev and invoke_pwritev are the only thread functions that
 * access non-thread data i.e the port queue and a mutex in the port
 * structure that is used to lock the port queue.
 *
 * They can do this because there is no risk that they might be 
 * invoked after the port has been terminated, since the port will not
 * be terminated until the port queue is empty.
 */

static void invoke_writev(void *data) {
    struct t_data *d = (struct t_data *) data;
    SysIOVec      *iov;
    int            iovlen;
    int            iovcnt;
    Uint32         size;
    Uint32         p;
    int            segment;

    segment = d->again && d->c.writev.size >= 2*FILE_SEGMENT_WRITE;
    if (segment) {
	size = FILE_SEGMENT_WRITE;
    } else {
	size = d->c.writev.size;
    }
    MUTEX_LOCK(*d->c.writev.q_lockp);
    iov = driver_peekq(d->c.writev.port, &iovlen);
    ASSERT(driver_sizeq(d->c.writev.port) >= size);
    /* Calculate iovcnt */
    for (p = 0, iovcnt = 0;
	 p < size && iovcnt < iovlen;
	 p += iov[iovcnt++].iov_len)
	;
    if (iovcnt > 0) {
	ASSERT(iov[iovcnt-1].iov_len > p - size);
	iov[iovcnt-1].iov_len -= p - size;
	if (d->flags & EFILE_COMPRESSED) {
	    int i, status = 1;
	    for (i = 0; i < iovcnt; i++) {
		if (iov[i].iov_base && iov[i].iov_len > 0) {
		    /* Just in case, I do not know what gzwrite does
		     * with errno.
		     */
		    errno = EINVAL; 
		    if (! (status = 
			   gzwrite((gzFile)d->fd, 
				   iov[i].iov_base,
				   iov[i].iov_len)) == iov[i].iov_len) {
			d->errInfo.posix_errno =
			    d->errInfo.os_errno = errno; /* XXX Correct? */
			break;
		    }
		}
	    }
	    d->result_ok = status;
	} else {
	    d->result_ok = efile_writev(&d->errInfo, 
					d->flags, d->fd,
					iov, iovcnt, size);
	}
	iov[iovcnt-1].iov_len += p - size;
    } else {
	d->result_ok = 1;
    }
    MUTEX_UNLOCK(*d->c.writev.q_lockp);
    if (! d->result_ok) {
	d->again = 0;
    } else {
	d->c.writev.free_size = size;
	d->c.writev.size -= size;
	if (! segment) {
	    d->again = 0;
	}
	TRACE_F(("w%u", (unsigned)size));

    }
}

static void free_writev(void *data) {
    struct t_data *d = data;

    MUTEX_LOCK(*d->c.writev.q_lockp);
    driver_deq(d->c.writev.port, d->c.writev.size + d->c.writev.free_size);
    MUTEX_UNLOCK(*d->c.writev.q_lockp);
    sys_free(d);
}

static void invoke_pwd(void *data)
{
    struct t_data *d = (struct t_data *) data;

    d->again = 0;
    d->result_ok = efile_getdcwd(&d->errInfo,d->drive, d->b+1,
				 RESBUFSIZE-1);
}

static void invoke_readlink(void *data)
{
    struct t_data *d = (struct t_data *) data;
    uchar resbuf[RESBUFSIZE];	/* Result buffer. */

    d->again = 0;
    d->result_ok = efile_readlink(&d->errInfo, d->b, (char *) resbuf+1,
				  RESBUFSIZE-1);
    if (d->result_ok != 0)
	strcpy((char *) d->b + 1, resbuf+1);
}

static void invoke_pwrite(void *data)
{
    struct t_data *d = (struct t_data *) data;

    d->again = 0;
    d->result_ok = efile_pwrite(&d->errInfo, d->fd, (char *) d->b,
				d->n, d->offset);
}

static void invoke_pwritev(void *data) {
    struct t_data    *d = (struct t_data *) data;
    SysIOVec         *iov;
    int               iovlen;
    int               iovcnt;
    struct t_pwritev *c = &d->c.pwritev;
    int               p;
    int               segment;
    Uint32            size, write_size;

    segment = d->again && c->size >= 2*FILE_SEGMENT_WRITE;
    if (segment) {
	size = FILE_SEGMENT_WRITE;
    } else {
	size = c->size;
    }
    d->result_ok = !0;
    p = 0;
    MUTEX_LOCK(*c->q_lockp);
    iov = driver_peekq(c->port, &iovlen);
    for (iovcnt = 0, c->free_size = 0;
	 c->cnt < c->n && iovcnt < iovlen && c->free_size < size;
	 c->cnt++) {
	int chop;
	write_size = c->specs[c->cnt].size;
	if (iov[iovcnt].iov_len - p < write_size) {
	    /* Mismatch between pos/size spec and what is queued */
	    d->errInfo.posix_errno = EINVAL;
	    d->result_ok = 0;
	    d->again = 0;
	    goto done;
	}
	chop = segment && c->free_size + write_size >= 2*FILE_SEGMENT_WRITE;
	if (chop) {
	    ASSERT(c->free_size < FILE_SEGMENT_WRITE);
	    write_size = FILE_SEGMENT_WRITE + FILE_SEGMENT_WRITE/2 
		- c->free_size;
	}
	d->result_ok = efile_pwrite(&d->errInfo, d->fd,
				    iov[iovcnt].iov_base + p,
				    write_size,
				    c->specs[c->cnt].offset);
	if (! d->result_ok) {
	    d->again = 0;
	    goto done;
	}
	c->free_size += write_size; 
	c->size -= write_size;
	if (chop) { 
	    c->specs[c->cnt].offset += write_size;
	    c->specs[c->cnt].size -= write_size;
	    /* Schedule out (d->again != 0) */
	    goto done;
	}
	/* Move forward in buffer */
	p += write_size;
	ASSERT(iov[iovcnt].iov_len >= p);
	if (iov[iovcnt].iov_len == p) {
	    /* Move to next iov[], we trust that it is not a 
	     * zero length vector, and thereby depend on that
	     * such are not queued.
	     */
	    iovcnt++; p = 0;
	}
    }
    if (! segment) {
	if (c->cnt != c->n) {
	    /* Mismatch between number of 
	     * pos/size specs vs number of queued buffers .
	     */
	    d->errInfo.posix_errno = EINVAL;
	    d->result_ok = 0;
	    d->again = 0;
	} else {
	    ASSERT(c->free_size == size);
	    d->again = 0;
	}
    }
 done:
    MUTEX_UNLOCK(*c->q_lockp);
}

static void free_pwritev(void *data) {
    struct t_data *d = data;

    MUTEX_LOCK(*d->c.writev.q_lockp);
    driver_deq(d->c.pwritev.port, d->c.pwritev.free_size + d->c.pwritev.size);
    MUTEX_UNLOCK(*d->c.writev.q_lockp);
    sys_free(d);
}

static void invoke_flstat(void *data)
{
    struct t_data *d = (struct t_data *) data;

    d->again = 0;
    d->result_ok = efile_fileinfo(&d->errInfo, &d->info,
				  d->b, d->command == FILE_LSTAT);
}

static void invoke_link(void *data)
{
    struct t_data *d = (struct t_data *) data;
    char *name = d->b;
    char *new_name;

    d->again = 0;
    new_name = name+strlen(name)+1;
    d->result_ok = efile_link(&d->errInfo, name, new_name);
}

static void invoke_symlink(void *data)
{
    struct t_data *d = (struct t_data *) data;
    char *name = d->b;
    char *new_name;

    d->again = 0;
    new_name = name+strlen(name)+1;
    d->result_ok = efile_symlink(&d->errInfo, name, new_name);
}

static void invoke_rename(void *data)
{
    struct t_data *d = (struct t_data *) data;
    char *name = d->b;
    char *new_name;

    d->again = 0;
    new_name = name+strlen(name)+1;
    d->result_ok = efile_rename(&d->errInfo, name, new_name);
}

static void invoke_write_info(void *data)
{
    struct t_data *d = (struct t_data *) data;

    d->again = 0;
    d->result_ok = efile_write_info(&d->errInfo, &d->info, d->b);
}

static void invoke_lseek(void *data)
{
    struct t_data *d = (struct t_data *) data;
    int status;

    d->again = 0;
    if (d->flags & EFILE_COMPRESSED) {
	status = 1;
	d->c.lseek.location = gzseekk((gzFile)d->fd, 
				      d->c.lseek.offset, d->c.lseek.origin);
	if (d->c.lseek.location == -1) {
	    d->errInfo.posix_errno = errno;
	    status = 0;
	}
    } else {
	status = efile_seek(&d->errInfo, d->fd, 
			    d->c.lseek.offset, d->c.lseek.origin,
			    &d->c.lseek.location);
    }
    d->result_ok = status;
}

static void invoke_readdir(void *data)
{
    struct t_data *d = (struct t_data *) data;
    int s;
    uchar resbuf[RESBUFSIZE];	/* Result buffer. */

    d->again = 0;
    resbuf[0] = FILE_RESP_OK;
    d->errInfo.posix_errno = 0;

    s = efile_readdir(&d->errInfo, d->b, &d->dir_handle,
		      (char *) resbuf+1, RESBUFSIZE-1);
    if (s) {
	d->n = 1 + strlen((char*) resbuf+1);
	d->result_ok = 1;
    }
    else {
	d->n = 1;
	d->result_ok = (d->errInfo.posix_errno == 0);
    }
    if (d->buf != NULL)
	sys_free(d->buf);
    d->buf = sys_alloc_from(200, d->n);
    memcpy(d->buf, resbuf, d->n);
}

static void invoke_open(void *data)
{
    struct t_data *d = (struct t_data *) data;

    unsigned size;		/* Size of file (not used). */
    int status = 1;		/* Status of open call. */

    d->again = 0;
    if ((d->flags & EFILE_COMPRESSED) == 0) {
	status = efile_openfile(&d->errInfo, d->b, d->flags, &d->fd, &size);
    } else {
	char* mode = NULL;

	if ((d->flags & (EFILE_MODE_READ|EFILE_MODE_WRITE)) ==
	    (EFILE_MODE_READ|EFILE_MODE_WRITE)) {
	    status = 0;
	    d->errInfo.posix_errno = EINVAL;
	} else {
	    mode = (d->flags & EFILE_MODE_READ) ? "rb" : "wb";
	    d->fd = (int) gzopen(d->b, mode);
	    if ((gzFile)d->fd == NULL) {
		if (errno == 0) {
		    errno = ENOMEM;
		}
		d->errInfo.posix_errno = errno;
		status = 0;
	    }
	}
    }

    d->result_ok = status;
}

static void free_pread(void *data)
{
    struct t_data *d = (struct t_data *) data;

    driver_free_binary(d->bin);
    sys_free(d);
}

static void free_readdir(void *data)
{
    struct t_data *d = (struct t_data *) data;

    if (d->buf != NULL)
	sys_free(d->buf);
    sys_free(d);
}



static void try_free_read_bin(file_descriptor *desc) {
    if ((desc->read_size == 0)
	&& (desc->read_offset >= desc->read_binp->orig_size)) {
	ASSERT(desc->read_offset == desc->read_binp->orig_size);
	driver_free_binary(desc->read_binp);
	desc->read_binp = NULL;
	desc->read_offset = 0;
	desc->read_size = 0;
    }
}



static int try_again(file_descriptor *desc, struct t_data *d) {
    if (! d->again) {
	return 0;
    }
    switch (d->command) {
    case FILE_WRITE:
	MUTEX_LOCK(*d->c.writev.q_lockp);
	driver_deq(d->c.writev.port, d->c.writev.free_size);
	MUTEX_UNLOCK(*d->c.writev.q_lockp);
	break;
    case FILE_PWRITEV:
	MUTEX_LOCK(*d->c.writev.q_lockp);
	driver_deq(d->c.pwritev.port, d->c.pwritev.free_size);
	MUTEX_UNLOCK(*d->c.writev.q_lockp);
	break;
    }
    if (desc->timer_state != timer_idle) {
	driver_cancel_timer(desc->port);
    }
    desc->timer_state = timer_again;
    driver_set_timer(desc->port, 0L);
    desc->invoke = d->invoke;
    desc->d = d;
    desc->free = d->free;
    return !0;
}



static void cq_execute(file_descriptor *desc) {
    struct t_data *d;
    register void *void_ptr; /* Soft cast variable */
    if (desc->timer_state == timer_again)
	return;
    if (! (d = cq_deq(desc)))
	return;
    TRACE_F(("x%i", (int)d->command));
    d->again = erts_async_max_threads == 0;
    DRIVER_ASYNC(d->level, desc, d->invoke, void_ptr=d, d->free);
}

static int async_write(file_descriptor *desc, int *errp,
		       int reply, Uint32 reply_size) {
    struct t_data *d;
    if (! (d = sys_alloc_from(200, sizeof(struct t_data) - 1))) {
	if (errp) *errp = ENOMEM;
	return -1;
    }
    TRACE_F(("w%u", (unsigned)desc->write_buffered));
    d->command = FILE_WRITE;
    d->fd = desc->fd;
    d->flags = desc->flags;
    d->c.writev.port = desc->port;
    d->c.writev.q_lockp = &desc->q_lock;
    d->c.writev.size = desc->write_buffered;
    d->reply = reply;
    d->c.writev.free_size = reply_size;
    d->c.writev.reply_size = reply_size;
    d->invoke = invoke_writev;
    d->free = free_writev;
    d->level = 1;
    cq_enq(desc, d);
    desc->write_buffered = 0;
    return 0;
}

static int flush_write(file_descriptor *desc, int *errp) {
    int    result;
    MUTEX_LOCK(desc->q_lock);
    if (desc->write_buffered > 0) {
	result = async_write(desc, errp, 0, 0);
    } else {
	result = 0;
    }
    MUTEX_UNLOCK(desc->q_lock);
    return result;
}

static int check_write_error(file_descriptor *desc, int *errp) {
    if (desc->write_error) {
	if (errp) *errp = desc->write_errInfo.posix_errno;
	desc->write_error = 0;
	return -1;
    }
    return 0;
}

static int flush_write_check_error(file_descriptor *desc, int *errp) {
    int r;
    if ( (r = flush_write(desc, errp)) != 0) {
	check_write_error(desc, NULL);
	return r;
    } else {
	return check_write_error(desc, errp);
    }
}

static int async_lseek(file_descriptor *desc, int *errp, int reply, 
		       int offset, int origin) {
    struct t_data *d;
    if (! (d = sys_alloc_from(200, sizeof(struct t_data)))) {
	*errp = ENOMEM;
	return -1;
    }
    d->flags = desc->flags;
    d->fd = desc->fd;
    d->command = FILE_LSEEK;
    d->reply = reply;
    d->c.lseek.offset = offset;
    d->c.lseek.origin = origin;
    d->invoke = invoke_lseek;
    d->free = free_data;
    d->level = 1;
    cq_enq(desc, d);
    return 0;
}

static void flush_read(file_descriptor *desc) {
    desc->read_offset = 0;
    desc->read_size = 0;
    if (desc->read_binp) {
	driver_free_binary(desc->read_binp);
	desc->read_binp = NULL;
    }
}

static int lseek_flush_read(file_descriptor *desc, int *errp) {
    int r = 0;
    Uint32 read_size = desc->read_size;
    if (read_size > 0) {
	flush_read(desc);
	if ((r = async_lseek(desc, errp, 0, -read_size, EFILE_SEEK_CUR)) 
	    < 0) {
	    return r;
	}
    } else {
	flush_read(desc);
    }
    return r;
}



/*********************************************************************
 * Driver entry point -> ready_async
 */
static void 
file_async_ready(ErlDrvData e, ErlDrvThreadData data)
{
    file_descriptor *desc = (file_descriptor*)e;
    struct t_data *d = (struct t_data *) data;
    char header[5];		/* result code + count */
    uchar resbuf[RESBUFSIZE];	/* Result buffer. */


    TRACE_C('r');

    if (try_again(desc, d)) {
	return;
    }

    switch (d->command)
    {
      case FILE_READ:
	if (!d->result_ok) {
	    error_reply(desc, &d->errInfo);
	} else {
	    Uint32 available_bytes = 
		d->c.read.bin_offset + d->c.read.bin_size - desc->read_offset;
	    if (available_bytes < d->c.read.size) {
		d->c.read.size = available_bytes;
	    }
	    data_reply(desc, d->c.read.binp, 
		       desc->read_offset, d->c.read.size);
	    desc->read_offset += d->c.read.size;
	    desc->read_size = 
		d->c.read.bin_offset + d->c.read.bin_size - desc->read_offset;
	    try_free_read_bin(desc);
	}
	free_read(data);
	break;
      case FILE_PREAD:
	if (!d->result_ok) {
	    error_reply(desc, &d->errInfo);
	} else {
	    header[0] = FILE_RESP_DATA;
	    put_int32(d->bytesRead, header+1);
	    TRACE_C('R');
	    driver_output_binary(desc->port, header, sizeof(header),
				 d->bin, 0, d->bytesRead);
	}
	free_pread(data);
	break;
      case FILE_READ_FILE:
	if (!d->result_ok)
	    error_reply(desc, &d->errInfo);
	else {
	    header[0] = FILE_RESP_OK;
	    TRACE_C('R');
	    driver_output_binary(desc->port, header, 1, 
				 d->c.read_file.binp, 0, d->c.read_file.offset);
	}
	free_read_file(data);
	break;
      case FILE_PWRITE:
	  if (!d->result_ok) {
	      error_reply(desc, &d->errInfo);
	  } else {
	      numeric_reply(desc, d->n);
	  }
	  free_data(data);
	  break;
      case FILE_WRITE:
	  if (d->reply) {
	      if (! d->result_ok) {
		  error_reply(desc, &d->errInfo);
	      } else {
		  numeric_reply(desc, d->c.writev.reply_size);
	      }
	  } else {
	      if (! d->result_ok) {
		  desc->write_error = !0;
		  desc->write_errInfo = d->errInfo;
	      }
	  }
	  free_writev(data);
	  break;
      case FILE_LSEEK:
	  if (d->reply) {
	      if (d->result_ok)
		  numeric_reply(desc, d->c.lseek.location);
	      else
		  error_reply(desc, &d->errInfo);
	  }
	  free_data(data);
	  break;
      case FILE_MKDIR:
      case FILE_RMDIR:
      case FILE_CHDIR:
      case FILE_DELETE:
      case FILE_FSYNC:
      case FILE_TRUNCATE:
      case FILE_LINK:
      case FILE_SYMLINK:
      case FILE_RENAME:
      case FILE_WRITE_INFO:
	reply(desc, d->result_ok, &d->errInfo);
	free_data(data);
	break;
      case FILE_PWD:
      case FILE_READLINK:
        {
	    int length;
	    char *resbuf = d->b;

	    if (!d->result_ok)
		error_reply(desc, &d->errInfo);
	    else {
		resbuf[0] = FILE_RESP_OK;
		length = 1+strlen((char*) resbuf+1);
		TRACE_C('R');
		driver_output2(desc->port, resbuf, length, NULL, 0);
	    }
	    free_data(data);
	    break;
	}
      case FILE_OPEN:
	if (!d->result_ok) {
	    error_reply(desc, &d->errInfo);
	} else {
	    desc->fd = d->fd;
	    desc->flags = d->flags;
	    numeric_reply(desc, d->fd);
	}
	free_data(data);
	break;
      case FILE_FSTAT:
      case FILE_LSTAT:
        {
	    if (d->result_ok) {
		resbuf[0] = FILE_RESP_INFO;

		put_int32(d->info.size_high,         &resbuf[1 + (0 * 4)]);
		put_int32(d->info.size_low,          &resbuf[1 + (1 * 4)]);
		put_int32(d->info.type,              &resbuf[1 + (2 * 4)]);

		PUT_TIME(d->info.accessTime, resbuf + 1 + 3*4);
		PUT_TIME(d->info.modifyTime, resbuf + 1 + 9*4);
		PUT_TIME(d->info.cTime, resbuf + 1 + 15*4);

		put_int32(d->info.mode,              &resbuf[1 + (21 * 4)]);
		put_int32(d->info.links,             &resbuf[1 + (22 * 4)]);
		put_int32(d->info.major_device,      &resbuf[1 + (23 * 4)]);
		put_int32(d->info.minor_device,      &resbuf[1 + (24 * 4)]);
		put_int32(d->info.inode,             &resbuf[1 + (25 * 4)]);
		put_int32(d->info.uid,               &resbuf[1 + (26 * 4)]);
		put_int32(d->info.gid,               &resbuf[1 + (27 * 4)]);
		put_int32(d->info.access,            &resbuf[1 + (28 * 4)]);

#define RESULT_SIZE (1 + (29 * 4))
		TRACE_C('R');
		driver_output2(desc->port, resbuf, RESULT_SIZE, NULL, 0);
#undef RESULT_SIZE
	    } else
		error_reply(desc, &d->errInfo);
	}
	free_data(data);
	break;
      case FILE_READDIR:
	if (!d->result_ok)
	    error_reply(desc, &d->errInfo);
	else {
	    TRACE_C('R');
	    driver_output2(desc->port, d->buf, d->n, NULL, 0);
	}
	if (d->n == 1)
	    free_readdir(data);
	else
	    DRIVER_ASYNC(2, desc, invoke_readdir, (void *) d,
			 free_readdir);
	break;
	/* See file_stop */
      case FILE_CLOSE:
	  if (d->reply) {
	      ok_reply(desc);
	  }
	  free_data(data);
	  break;
      case FILE_PWRITEV:
	  if (!d->result_ok) {
	      numerr_reply(desc, d->c.pwritev.cnt, &d->errInfo);
	  } else {
	      numeric_reply(desc, d->c.pwritev.n);
	  }
	  free_pwritev(data);
	  break;
      case FILE_PREADV:
	  if (!d->result_ok) {
	      error_reply(desc, &d->errInfo);
	  } else {
	      ev_reply(desc, FILE_RESP_LDATA, &d->c.preadv.eiov);
	  }
	  free_preadv(data);
	  break;
      case FILE_IPREAD:
	  if (!d->result_ok) {
	      error_reply(desc, &d->errInfo);
	  } else {
	      ev_reply(desc, FILE_RESP_N2DATA, &d->c.preadv.eiov);
	  }
	  free_preadv(data);
	  break;
      default:
	abort();
    }
    if (desc->write_buffered != 0 && desc->timer_state == timer_idle) {
	desc->timer_state = timer_write;
	driver_set_timer(desc->port, desc->write_delay);
    }
    cq_execute(desc);
}

/*********************************************************************
 * Driver entry point -> output
 */
static void 
file_output(ErlDrvData e, char* buf, int count)
{
    file_descriptor* desc = (file_descriptor*)e;
    Efile_error errInfo;	/* The error codes for the last operation. */
    int fd;			/* The file descriptor for this port, if any,
				 * -1 if none.
				 */
    char* name;			/* Points to the filename in buf. */
    int command;
    struct t_data *d = NULL;


    TRACE_C('o');

    fd  = desc->fd;
    name = buf+1;
    command = *(uchar*)buf++;

    switch(command) {

    case FILE_MKDIR:
    {
	d = sys_alloc_from(200, sizeof(struct t_data) - 1
			   + strlen(name) + 1);
	
	strcpy(d->b, name);
	d->command = command;
	d->invoke = invoke_mkdir;
	d->free = free_data;
	d->level = 2;
	goto done;
    }
    case FILE_RMDIR:
    {
	d = sys_alloc_from(200, sizeof(struct t_data) - 1
			   + strlen(name) + 1);
	
	strcpy(d->b, name);
	d->command = command;
	d->invoke = invoke_rmdir;
	d->free = free_data;
	d->level = 2;
	goto done;
    }
    case FILE_DELETE:
    {
	d = sys_alloc_from(200, sizeof(struct t_data) - 1
			   + strlen(name) + 1);
	
	strcpy(d->b, name);
	d->command = command;
	d->invoke = invoke_delete_file;
	d->free = free_data;
	d->level = 2;
	goto done;
    }
    case FILE_RENAME:
	{
	    char* new_name;

	    new_name = name+strlen(name)+1;
	    d = sys_alloc_from(200, sizeof(struct t_data) - 1
			       + strlen(name) + 1
			       + strlen(new_name) + 1);
	
	    strcpy(d->b, name);
	    strcpy(d->b + strlen(name) + 1, new_name);
	    d->flags = desc->flags;
	    d->fd = fd;
	    d->command = command;
	    d->invoke = invoke_rename;
	    d->free = free_data;
	    d->level = 2;
	    goto done;
	}
    case FILE_CHDIR:
    {
	d = sys_alloc_from(200, sizeof(struct t_data) - 1
			   + strlen(name) + 1);
	
	strcpy(d->b, name);
	d->command = command;
	d->invoke = invoke_chdir;
	d->free = free_data;
	d->level = 2;
	goto done;
    }
    case FILE_PWD:
        {
	    d = sys_alloc_from(200, sizeof(struct t_data) - 1
			       + RESBUFSIZE + 1);
	
	    d->drive = *(uchar*)buf;
	    d->command = command;
	    d->invoke = invoke_pwd;
	    d->free = free_data;
	    d->level = 2;
	    goto done;
	}

    case FILE_READDIR: 
#ifdef USE_THREADS
	if (erts_async_max_threads > 0)
	{
	    d = sys_alloc_from(200, sizeof(struct t_data) - 1
			       + strlen(name) + 1);
	
	    strcpy(d->b, name);
	    d->dir_handle = NULL;
	    d->buf = NULL;
	    d->command = command;
	    d->invoke = invoke_readdir;
	    d->free = free_readdir;
	    d->level = 2;
	    goto done;
	}
	else   
#endif
	{
	    uchar resbuf[RESBUFSIZE];
	    EFILE_DIR_HANDLE dir_handle; /* Handle to open directory. */

	    errInfo.posix_errno = 0;
	    dir_handle = NULL;
	    resbuf[0] = FILE_RESP_OK;

	    while (efile_readdir(&errInfo, name, &dir_handle,
				 (char *) resbuf+1, RESBUFSIZE-1)) {
		int length = 1 + strlen((char*) resbuf+1);
		driver_output2(desc->port, resbuf, length, NULL, 0);
	    }
	    if (errInfo.posix_errno != 0) {
		error_reply(desc, &errInfo);
		return;
	    }
	    TRACE_C('R');
	    driver_output2(desc->port, resbuf, 1, NULL, 0);
	    return;
	}
    case FILE_OPEN:
	{
	    d = sys_alloc_from(200, sizeof(struct t_data) - 1
			       + strlen(buf+4) + 1);
	
	    d->flags = get_int32((uchar*)buf);
	    name = buf+4;
	    strcpy(d->b, name);
	    d->command = command;
	    d->invoke = invoke_open;
	    d->free = free_data;
	    d->level = 2;
	    goto done;
	}

    case FILE_FSYNC:
    {
	d = sys_alloc_from(200, sizeof(struct t_data));
	
	d->fd = fd;
	d->command = command;
	d->invoke = invoke_fsync;
	d->free = free_data;
	d->level = 2;
	goto done;
    }

    case FILE_FSTAT: 
    case FILE_LSTAT:
    {
	d = sys_alloc_from(200, sizeof(struct t_data) - 1
			   + strlen(name) + 1);
	
	strcpy(d->b, name);
	d->fd = fd;
	d->command = command;
	d->invoke = invoke_flstat;
	d->free = free_data;
	d->level = 2;
	goto done;
    }
    case FILE_PWRITE:
	{
	    int offset;		/* Offset for pwrite. */

	    d = sys_alloc_from(200, sizeof(struct t_data) - 1
			       + count-5);
	
	    offset = get_int32((uchar*)buf);
	    d->offset = offset;
	    memcpy(d->b, buf+4, count-5);
	    d->n = count-5;
	    d->fd = fd;
	    d->command = command;
	    d->invoke = invoke_pwrite;
	    d->free = free_data;
	    d->level = 1;
	    goto done;
	}

    case FILE_PREAD:
	{
	    int offset;		/* Offset for seek. */
	    ErlDrvBinary* bin;	/* The binary data. */

	    offset = get_int32(buf);
	    count = get_int32(buf+4);
	    if ((bin = driver_alloc_binary(count)) == NULL) {
		errInfo.posix_errno = ENOMEM;
		errInfo.os_errno = 0;
		error_reply(desc, &errInfo);
		goto done;
	    }
	{
	    d = sys_alloc_from(200, sizeof(struct t_data) - 1);
	    
	    d->flags = desc->flags;
	    d->bin = bin;
	    d->n = count;
	    d->offset = offset;
	    d->fd = fd;
	    d->command = command;
	    d->invoke = invoke_pread;
	    d->free = free_pread;
	    d->level = 1;
	    goto done;
	}
	}	

    case FILE_TRUNCATE:
        {
	    d = sys_alloc_from(200, sizeof(struct t_data));
	    
	    d->flags = desc->flags;
	    d->fd = fd;
	    d->command = command;
	    d->invoke = invoke_truncate;
	    d->free = free_data;
	    d->level = 2;
	    goto done;
	}

    case FILE_WRITE_INFO:
	{
	    d = sys_alloc_from(200, sizeof(struct t_data) - 1
			       + strlen(buf+21*4) + 1);
	    
	    d->info.mode = get_int32(buf + 0 * 4);
	    d->info.uid = get_int32(buf + 1 * 4);
	    d->info.gid = get_int32(buf + 2 * 4);
	    GET_TIME(d->info.accessTime, buf + 3 * 4);
	    GET_TIME(d->info.modifyTime, buf + 9 * 4);
	    GET_TIME(d->info.cTime, buf + 15 * 4);
	    strcpy(d->b, buf+21*4);
	    d->command = command;
	    d->invoke = invoke_write_info;
	    d->free = free_data;
	    d->level = 2;
	    goto done;
	}

    case FILE_READLINK:
	{
	    d = sys_alloc_from(200, sizeof(struct t_data) - 1
			       + RESBUFSIZE + 1);
	
	    strcpy(d->b, name);
	    d->command = command;
	    d->invoke = invoke_readlink;
	    d->free = free_data;
	    d->level = 2;
	    goto done;
	}

    case FILE_LINK:
	{
	    char* new_name;

	    new_name = name+strlen(name)+1;
	    d = sys_alloc_from(200, sizeof(struct t_data) - 1
			       + strlen(name) + 1
			       + strlen(new_name) + 1);
	
	    strcpy(d->b, name);
	    strcpy(d->b + strlen(name) + 1, new_name);
	    d->flags = desc->flags;
	    d->fd = fd;
	    d->command = command;
	    d->invoke = invoke_link;
	    d->free = free_data;
	    d->level = 2;
	    goto done;
	}

    case FILE_SYMLINK:
	{
	    char* new_name;

	    new_name = name+strlen(name)+1;
	    d = sys_alloc_from(200, sizeof(struct t_data) - 1
			       + strlen(name) + 1
			       + strlen(new_name) + 1);
	
	    strcpy(d->b, name);
	    strcpy(d->b + strlen(name) + 1, new_name);
	    d->flags = desc->flags;
	    d->fd = fd;
	    d->command = command;
	    d->invoke = invoke_symlink;
	    d->free = free_data;
	    d->level = 2;
	    goto done;
	}

    }

    /*
     * Ignore anything else -- let the caller hang.
     */
     
    return;

 done:
    if (d) {
	cq_enq(desc, d);
    }
}

/*********************************************************************
 * Driver entry point -> flush
 */
static void 
file_flush(ErlDrvData e) {
    file_descriptor *desc = (file_descriptor *)e;
    int r;

    TRACE_C('f');

    r = flush_write(desc, NULL);
    /* Only possible reason for bad return value is ENOMEM, and 
     * there is nobody to tell...
     */
    ASSERT(r == 0); 
    r = 0; /* Avoiding warning */
    cq_execute(desc);
}



/*********************************************************************
 * Driver entry point -> control
 */
static int 
file_control(ErlDrvData e, unsigned int command, 
			 char* buf, int len, char **rbuf, int rlen) {
    file_descriptor *desc = (file_descriptor *)e;
    switch (command) {
    default:
	return 0;
    } /* switch (command) */
    ASSERT(0);
    desc = NULL; /* XXX Avoid warning while empty switch */
    return 0;
}

/*********************************************************************
 * Driver entry point -> timeout
 */
static void 
file_timeout(ErlDrvData e) {
    file_descriptor *desc = (file_descriptor *)e;
    enum e_timer timer_state = desc->timer_state;

    TRACE_C('t');

    desc->timer_state = timer_idle;
    switch (timer_state) {
    case timer_idle:
	ASSERT(0);
	break;
    case timer_again:
	ASSERT(desc->invoke);
	ASSERT(desc->free);
	driver_async(desc->port, KEY(desc), desc->invoke, desc->d, desc->free);
	break;
    case timer_write: {
	int r = flush_write(desc, NULL);
	/* Only possible reason for bad return value is ENOMEM, and 
	 * there is nobody to tell...
	 */
	ASSERT(r == 0); 
	r = 0; /* Avoiding warning */
    } break;
    } /* case */
}



/*********************************************************************
 * Driver entry point -> outputv
 */
static void 
file_outputv(ErlDrvData e, ErlIOVec *ev) {
    file_descriptor* desc = (file_descriptor*)e;
    char command;
    int p, q;
    int err;

    TRACE_C('v');

    p = 0; q = 1;
    if (! EV_GET_CHAR(ev, &command, &p, &q)) {
	/* Empty command */
	error_reply_posix(desc, EINVAL);
	goto done;
    }
    /* 'command' contains the decoded command number,
     * 'p' and 'q' point out the next byte in the command:
     * ((char *)ev->iov[q].iov_base) + p;
     */
    
    TRACE_F(("%i", (int)command));

    switch (command) {

    case FILE_CLOSE: {
	flush_read(desc);
	if (flush_write_check_error(desc, &err) < 0) {
	    error_reply_posix(desc, err);
	    goto done;
	}
	if (ev->size != 1) {
	    /* Wrong command length */
	    error_reply_posix(desc, EINVAL);
	    goto done;
	}
	if (desc->fd >= 0) {
	    struct t_data *d;
	    if (! (d = sys_alloc_from(200, sizeof(struct t_data)))) {
		error_reply_posix(desc, ENOMEM);
	    } else {
		d->command = command;
		d->reply = !0;
		d->fd = desc->fd;
		d->flags = desc->flags;
		d->invoke = invoke_close;
		d->free = free_data;
		d->level = 2;
		cq_enq(desc, d);
		desc->fd = -1;
		desc->flags = 0;
	    }
	} else {
	    error_reply_posix(desc, EBADF);
	}
    } goto done;

    case FILE_READ: {
	Uint32 size, alloc_size;
	struct t_data *d;
	if (flush_write_check_error(desc, &err) < 0) {
	    error_reply_posix(desc, err);
	    goto done;
	}
	if (ev->size != 1+sizeof(Uint32) 
	    || !EV_GET_UINT32(ev, &size, &p, &q)) {
	    /* Wrong buffer length to contain the read count */
	    numerr_reply_posix(desc, 0, EINVAL);
	    goto done;
	}
	if ((! (desc->flags & EFILE_MODE_READ)) || (desc->fd < 0)) {
	    error_reply_posix(desc, EBADF);
	    goto done;
	}
	if (size == 0) {
	    numeric_reply(desc, size);
	    goto done;
	}
	if (desc->read_size >= size) {
	    /* We already have all data */
	    data_reply(desc, desc->read_binp, desc->read_offset, size);
	    desc->read_offset += size;
	    desc->read_size -= size;
	    try_free_read_bin(desc);
	    goto done;
	}
	/* We may have some of the data 
	 */
	/* Motivation for the following strange formula:
	 * If the read request is for such a large block as more than 
	 * half the buffer size it may lead to a lot of unnecessary copying, 
	 * since the tail of the old buffer is copied to the head of the
	 * new, and if the tail is almost half the buffer it is a lot
	 * to copy. Therefore allocate the exact amount needed in 
	 * this case, giving no lingering tail. */
	alloc_size = 
	    size > (desc->read_bufsize>>1) ? 
	    size : desc->read_bufsize;
	if (! desc->read_binp) {
	    /* Need to allocate a new binary for the result */
	    if (! (desc->read_binp = driver_alloc_binary(alloc_size))) {
		error_reply_posix(desc, ENOMEM);
		goto done;
	    }
	} else {
	    /* We already have a buffer */
	    if (desc->read_binp->orig_size - desc->read_offset < size) {
		/* Need to allocate a new binary for the result */
		ErlDrvBinary *binp;
		if (! (binp = driver_alloc_binary(alloc_size))) {
		    error_reply_posix(desc, ENOMEM);
		    goto done;
		}
		/* Move data we already have to the new binary */
		sys_memcpy(binp->orig_bytes, 
			   desc->read_binp->orig_bytes + desc->read_offset,
			   desc->read_size);
		driver_free_binary(desc->read_binp);
		desc->read_offset = 0;
		desc->read_binp = binp;
	    }
	} 
	if (! (d = sys_alloc_from(200, sizeof(struct t_data)))) {
	    error_reply_posix(desc, ENOMEM);
	    goto done;
	}
	d->command = command;
	d->reply = !0;
	d->fd = desc->fd;
	d->flags = desc->flags;
	d->c.read.binp = desc->read_binp;
	d->c.read.bin_offset = desc->read_offset + desc->read_size;
	d->c.read.bin_size = desc->read_binp->orig_size - d->c.read.bin_offset;
	d->c.read.size = size;
	d->c.read.binp->refc++;
	d->invoke = invoke_read;
	d->free = free_read;
	d->level = 1;
	cq_enq(desc, d);
    } goto done; /* case FILE_READ: */

    case FILE_WRITE: {
	int skip = 1;
	int size = ev->size - skip;
	if (lseek_flush_read(desc, &err) < 0) {
	    error_reply_posix(desc, err);
	    goto done;
	}
	if (! (desc->flags & EFILE_MODE_WRITE)) {
	    error_reply_posix(desc, EBADF);
	    goto done;
	}
	if (size <= 0) {
	    numeric_reply(desc, size);
	    goto done;
	}
	MUTEX_LOCK(desc->q_lock);
	if (driver_enqv(desc->port, ev, skip)) {
	    MUTEX_UNLOCK(desc->q_lock);
	    error_reply_posix(desc, ENOMEM);
	    goto done;
	}
	desc->write_buffered += size;
	if (desc->write_buffered < desc->write_bufsize) {
	    MUTEX_UNLOCK(desc->q_lock);
	    numeric_reply(desc, size);
	    if (desc->timer_state == timer_idle) {
		driver_set_timer(desc->port, desc->write_delay);
		desc->timer_state = timer_write;
	    }
	} else {
	    if (async_write(desc, &err, !0, size) != 0) {
		MUTEX_UNLOCK(desc->q_lock);
		error_reply_posix(desc, err);
		goto done;
	    } else {
		MUTEX_UNLOCK(desc->q_lock);
	    }
	}
    } goto done;
    
    case FILE_PWRITEV: {
	Uint32 i, j, n, total;
	struct t_data *d;
	if (lseek_flush_read(desc, &err) < 0) {
	    error_reply_posix(desc, err);
	    goto done;
	}
	if (flush_write_check_error(desc, &err) < 0) {
	    error_reply_posix(desc, err);
	    goto done;
	}
	if (ev->size < 1+sizeof(Uint32) 
	    || !EV_GET_UINT32(ev, &n, &p, &q)) {
	    /* Buffer too short to contain even the number of pos/size specs */
	    numerr_reply_posix(desc, 0, EINVAL);
	    goto done;
	}
	if (n == 0) {
	    /* Trivial case - nothing to write */
	    if (ev->size != 1+sizeof(Uint32)) {
		error_reply_posix(desc, err);
	    } else {
		numeric_reply(desc, 0);
	    }
	    goto done;
	}
	if (ev->size < 1+sizeof(Uint32)*(1+2*n)) {
	    /* Buffer too short to contain even the pos/size specs */
	    numerr_reply_posix(desc, 0, EINVAL);
	    goto done;
	}
	d = sys_alloc_from(200, 
			   sizeof(struct t_data) 
			   + (n-1) * sizeof(struct t_pwritev));
	if (! d) {
	    numerr_reply_posix(desc, 0, ENOMEM);
	    goto done;
	}
	d->command = command;
	d->reply = !0;
	d->fd = desc->fd;
	d->flags = desc->flags;
	d->c.pwritev.port = desc->port;
	d->c.pwritev.q_lockp = &desc->q_lock;
	d->c.pwritev.n = n;
	d->c.pwritev.cnt = 0;
	total = 0;
	j = 0;
	/* Create pos/size specs in the thread data structure
	 * for all non-zero size binaries. Calculate total size.
	 */
	for(i = 0; i < n; i++) {
	    Uint32 size;
	    if (!EV_GET_UINT32(ev, &d->c.pwritev.specs[j].offset, &p, &q)
		|| !EV_GET_UINT32(ev, &size, &p, &q)) {
		/* Misalignment in buffer */
		numerr_reply_posix(desc, 0, EINVAL);
		sys_free(d);
		goto done;
	    }
	    if (size > 0) {
		total += size;
		d->c.pwritev.specs[j].size = size;
		j++;
	    }
	}
	d->c.pwritev.size = total;
	d->c.pwritev.free_size = 0;
	if (j == 0) {
	    /* Trivial case - nothing to write */
	    sys_free(d);
	    numeric_reply(desc, 0);
	} else {
	    int skip = 1 + sizeof(Uint32)*(1 + 2*n);
	    if (skip + total != ev->size) {
		/* Actual amount of data does not match 
		 * total of all pos/size specs
		 */
		sys_free(d);
		numerr_reply_posix(desc, 0, EINVAL);
	    } else {
		/* Enqueue the data */
		MUTEX_LOCK(desc->q_lock);
		driver_enqv(desc->port, ev, skip);
		MUTEX_UNLOCK(desc->q_lock);
		/* Execute the command */
		d->invoke = invoke_pwritev;
		d->free = free_pwritev;
		d->level = 1;
		cq_enq(desc, d);
	    }
	}
    } goto done; /* case FILE_PWRITEV: */

    case FILE_PREADV: {
	register void * void_ptr;
	Uint32 i, n;
	struct t_data *d;
	ErlIOVec *res_ev;
	if (lseek_flush_read(desc, &err) < 0) {
	    error_reply_posix(desc, err);
	    goto done;
	}
	if (flush_write_check_error(desc, &err) < 0) {
	    error_reply_posix(desc, err);
	    goto done;
	}
	if (ev->size < 1+sizeof(Uint32)
	    || !EV_GET_UINT32(ev, &n, &p, &q)) {
	    /* Buffer too short to contain even the number of pos/size specs */
	    numerr_reply_posix(desc, 0, EINVAL);
	    goto done;
	}
	if (ev->size != 1+sizeof(Uint32)*(1+2*n)) {
	    /* Buffer wrong length to contain the pos/size specs */
	    numerr_reply_posix(desc, 0, EINVAL);
	    goto done;
	}
	/* Create the thread data structure with the contained ErlIOVec 
	 * and corresponding binaries for the response 
	 */
	d = sys_alloc_from(200, 
			   sizeof(struct t_data) 
			   + ((n-1) * sizeof(Uint32))
			   + ((1+n) 
			      * (sizeof(SysIOVec) + sizeof(ErlDrvBinary*))));
	if (! d) {
	    numerr_reply_posix(desc, 0, ENOMEM);
	    goto done;
	}
	d->command = command;
	d->reply = !0;
	d->fd = desc->fd;
	d->flags = desc->flags;
	d->c.preadv.n = n;
	d->c.preadv.cnt = 0;
	d->c.preadv.size = 0;
	res_ev = &d->c.preadv.eiov;
	/* XXX possible alignment problems here for weird machines */
	res_ev->vsize = 1+d->c.preadv.n;
	res_ev->iov = void_ptr = &d->c.preadv.offsets[d->c.preadv.n];
	res_ev->binv = void_ptr = &res_ev->iov[res_ev->vsize];
	/* Read in the pos/size specs and allocate binaries for the results */
	for (i = 1; i < 1+n; i++) {
	    Uint32 size;
	    if (!EV_GET_UINT32(ev, &d->c.preadv.offsets[i-1], &p, &q)
		|| !EV_GET_UINT32(ev, &size, &p, &q)) {
		numerr_reply_posix(desc, 0, EINVAL);
		break;
	    }
	    if (! (res_ev->binv[i] = driver_alloc_binary(size))) {
		numerr_reply_posix(desc, 0, ENOMEM);
		break;
	    } else {
		res_ev->iov[i].iov_len  = size;
		res_ev->iov[i].iov_base = res_ev->binv[i]->orig_bytes;
	    }
	}
	if (i < 1+n) {
	    for (i--; i > 0; i--) {
		driver_free_binary(res_ev->binv[i]);
	    }
	    sys_free(d);
	    goto done;
	}
	/* Allocate the header binary (index 0) */
	res_ev->binv[0] = driver_alloc_binary(sizeof(Uint32)*(1+n));
	if (! res_ev->binv[0]) {
	    numerr_reply_posix(desc, 0, ENOMEM);
	    for (i = 1; i < 1+n; i++) {
		driver_free_binary(res_ev->binv[i]);
	    }
	    sys_free(d);
	    goto done;
	}
	res_ev->iov[0].iov_len = sizeof(Uint32)*(1+n);
	res_ev->iov[0].iov_base = res_ev->binv[0]->orig_bytes;
	/* Fill in the number of buffers in the header */
	put_int32(n, res_ev->iov[0].iov_base);
	/**/
	res_ev->size = res_ev->iov[0].iov_len;
	if (n == 0) {
	    /* Trivial case - nothing to read */
	    ev_reply(desc, FILE_RESP_LDATA, res_ev);
	    free_preadv(d);
	    goto done;
	} else {
	    d->invoke = invoke_preadv;
	    d->free = free_preadv;
	    d->level = 1;
	    cq_enq(desc, d);
	}
    } goto done; /* case FILE_PREADV: */

    case FILE_LSEEK: {
	Sint32 offset;		/* Offset for seek. */
	Uint32 origin;		/* Origin of seek. */
	if (lseek_flush_read(desc, &err) < 0) {
	    error_reply_posix(desc, err);
	    goto done;
	}
	if (flush_write_check_error(desc, &err) < 0) {
	    error_reply_posix(desc, err);
	    goto done;
	}
	if (ev->size != 1+2*sizeof(Uint32)
	    || !EV_GET_UINT32(ev, &offset, &p, &q)
	    || !EV_GET_UINT32(ev, &origin, &p, &q)) {
	    /* Wrong length of buffer to contain offset and origin */
	    error_reply_posix(desc, EINVAL);
	    goto done;
	} 
	if (async_lseek(desc, &err, !0, offset, origin) < 0) {
	    error_reply_posix(desc, err);
	    goto done;
	}
    } goto done;

    case FILE_READ_FILE: {
	struct t_data *d;
	unsigned size;
	if (ev->size < 1+1) {
	    /* Buffer contains empty name */
	    error_reply_posix(desc, ENOENT);
	    goto done;
	}
	if (ev->size-1 != ev->iov[q].iov_len-p) {
	    /* Name not in one single buffer */
	    error_reply_posix(desc, EINVAL);
	    goto done;
	}
	d = sys_alloc_from(200, sizeof(struct t_data) + ev->size-1);
	if (! d) {
	    error_reply_posix(desc, ENOMEM);
	    goto done;
	}
	d->command = command;
	d->reply = !0;
	/* Copy name */
	memcpy(d->c.read_file.name, EV_CHAR_P(ev, p, q), ev->size-1);
	d->c.read_file.name[ev->size-1] = '\0';
	if (! efile_openfile(&d->errInfo, d->c.read_file.name, EFILE_MODE_READ, 
			     &d->fd, &size)) {
	    error_reply(desc, &d->errInfo);
	    sys_free(d);
	    goto done;
	}
	if (! (d->c.read_file.binp = driver_alloc_binary(size))) {
	    error_reply_posix(desc, ENOMEM);
	    efile_closefile(d->fd);
	    sys_free(d);
	    goto done;
	}
	d->c.read_file.size = size;
	d->c.read_file.offset = 0;
	d->invoke = invoke_read_file;
	d->free = free_read_file;
	d->level = 2;
	cq_enq(desc, d);
    } goto done;

    case FILE_IPREAD: {
	/* This operation cheets by using invoke_preadv() and free_preadv()
	 * instead of own invoke and free functions. Therefore the 
	 * result format is a bit awkward - the header binary contains one
	 * extra 32 bit field that invoke_preadv() fortunately ignores,
	 * and the first 32 bit field does not contain the number of 
	 * data binaries which invoke_preadv() also ignores.
	 */
	register void * void_ptr;
	char mode;
	Uint32 hdr_offset, max_size, offset, size;
	unsigned bytes_read = 0;
	Efile_error errInfo;
	struct t_data *d;
	char buf[2*sizeof(Uint32)];
	ErlIOVec *res_ev;
	if (! EV_GET_CHAR(ev, &mode, &p, &q)) {
	    /* Empty command */
	    error_reply_posix(desc, EINVAL);
	    goto done;
	}
	if (mode != IPREAD_S32BU_P32BU) {
	    error_reply_posix(desc, EINVAL);
	    goto done;
	}
	if (lseek_flush_read(desc, &err) < 0) {
	    error_reply_posix(desc, err);
	    goto done;
	}
	if (flush_write_check_error(desc, &err) < 0) {
	    error_reply_posix(desc, err);
	    goto done;
	}
	if (ev->size < 1+1+2*sizeof(Uint32)
	    || !EV_GET_UINT32(ev, &hdr_offset, &p, &q)
	    || !EV_GET_UINT32(ev, &max_size, &p, &q)) {
	    /* Buffer too short to contain 
	     * the header offset and max size spec */
	    numerr_reply_posix(desc, 0, EINVAL);
	    goto done;
	}
	if (! efile_pread(&errInfo, desc->fd, hdr_offset, 
			  buf, sizeof(buf), &bytes_read)) {
	    error_reply(desc, &errInfo);
	    goto done;
	}
	if (bytes_read != sizeof(buf)) {
	    eof_reply(desc);
	    goto done;
	}
	size = get_int32(buf);
	offset = get_int32(buf+sizeof(Uint32));
	if (size > max_size) {
	    eof_reply(desc);
	    goto done;
	}
	/* Create the thread data structure with the contained ErlIOVec 
	 * and corresponding binaries for the response 
	 */
	d = sys_alloc_from(200, 
			   sizeof(struct t_data)
			   + 2*(sizeof(SysIOVec) + sizeof(ErlDrvBinary*)));
	if (! d) {
	    numerr_reply_posix(desc, 0, ENOMEM);
	    goto done;
	}
	d->command = command;
	d->reply = !0;
	d->fd = desc->fd;
	d->flags = desc->flags;
	d->c.preadv.n = 1;
	d->c.preadv.cnt = 0;
	d->c.preadv.size = 0;
	d->c.preadv.offsets[0] = offset;
	res_ev = &d->c.preadv.eiov;
	/* XXX possible alignment problems here for weird machines */
	res_ev->vsize = 1+d->c.preadv.n;
	res_ev->iov = void_ptr = &d->c.preadv.offsets[d->c.preadv.n];
	res_ev->binv = void_ptr = &res_ev->iov[res_ev->vsize];
	/* Allocate the header binary */
	if (! (res_ev->binv[0] = driver_alloc_binary(3*sizeof(Uint32)))) {
	    numerr_reply_posix(desc, 0, ENOMEM);
	    sys_free(d);
	    goto done;
	}
	res_ev->iov[0].iov_len = 3*sizeof(Uint32);
	res_ev->iov[0].iov_base = res_ev->binv[0]->orig_bytes;
	res_ev->size = res_ev->iov[0].iov_len;
	put_int32(offset, res_ev->iov[0].iov_base);
	put_int32(size, ((char *)res_ev->iov[0].iov_base) + 2*sizeof(Uint32));
	if (size == 0) {
	    put_int32(size, 
		      ((char *)res_ev->iov[0].iov_base) + sizeof(Uint32));
	    res_ev->vsize = 1;
	    ev_reply(desc, FILE_RESP_N2DATA, res_ev);
	    driver_free_binary(res_ev->binv[0]);
	    sys_free(d);
	    goto done;
	}
	/* Allocate the result data binary */
	if (! (res_ev->binv[1] = driver_alloc_binary(size))) {
	    numerr_reply_posix(desc, 0, ENOMEM);
	    driver_free_binary(res_ev->binv[0]);
	    sys_free(d);
	    goto done;
	}
	res_ev->iov[1].iov_len = size;
	res_ev->iov[1].iov_base = res_ev->binv[1]->orig_bytes;
	d->invoke = invoke_preadv;
	d->free = free_preadv;
	d->level = 1;
	cq_enq(desc, d);
    } goto done; /* case FILE_IPREAD: */

    case FILE_SETOPT: {
	char opt;
	if (ev->size < 1+1
	    || !EV_GET_CHAR(ev, &opt, &p, &q)) {
	    /* Buffer too short to contain even the option type */
	    error_reply_posix(desc, EINVAL);
	    goto done;
	}
	switch (opt) {
	case FILE_OPT_DELAYED_WRITE: {
	    Uint32 size = 0, delay = 0;
	    if (ev->size != 1+1+2*sizeof(Uint32)
		|| !EV_GET_UINT32(ev, &size, &p, &q)
		|| !EV_GET_UINT32(ev, &delay, &p, &q)) {
		/* Buffer has wrong length to contain the option values */
		error_reply_posix(desc, EINVAL);
		goto done;
	    }
	    desc->write_bufsize = size;
	    desc->write_delay = (unsigned long)delay;
	    ok_reply(desc);
	} goto done;
	case FILE_OPT_READ_AHEAD: {
	    if (ev->size != 1+1+sizeof(Uint32)
		|| !EV_GET_UINT32(ev, &desc->read_bufsize, &p, &q)) {
		/* Buffer has wrong length to contain the option values */
		error_reply_posix(desc, EINVAL);
		goto done;
	    }
	    ok_reply(desc);
	} goto done;
	default:
	    error_reply_posix(desc, EINVAL);
	    goto done;
	} /* case FILE_OPT_DELAYED_WRITE: */
    } ASSERT(0); goto done; /* case FILE_SETOPT: */
    
    } /* switch(command) */
    
    if (lseek_flush_read(desc, &err) < 0) {
	error_reply_posix(desc, err);
	goto done;
    }
    if (flush_write_check_error(desc, &err) < 0) {
	error_reply_posix(desc, err);
	goto done;
    } else {
	/* Flatten buffer and send it to file_output(desc, buf, len) */
	int len = ev->size;
	char *buf = sys_alloc_from(200, len);
	if (! buf) {
	    error_reply_posix(desc, ENOMEM);
	    goto done;
	}
	driver_vec_to_buf(ev, buf, len);
	file_output((ErlDrvData) desc, buf, len);
	sys_free(buf);
	goto done;
    }

 done:
    cq_execute(desc);
}
