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
 * I/O routines for manipulating ports.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"

/* must be included BEFORE global.h (since it includes erl_driver.h) */
#include "erl_sys_driver.h"

#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "dist.h"
#include "big.h"
#include "erl_binary.h"

/* XXX Remove when driver interface has been cleaned-up. */
void *ddll_open(char *);
void *ddll_sym(void *, char *);
int ddll_close(void *);
char *ddll_error(void);

extern ErlDrvEntry fd_driver_entry;
extern ErlDrvEntry vanilla_driver_entry;
extern ErlDrvEntry spawn_driver_entry;
extern ErlDrvEntry *driver_tab[];

DE_List*   driver_list;	/* Where should this be defined? */
Port*      erts_port;
Uint       bytes_out;          /* No bytes sent out of the system */
Uint       bytes_in;           /* No bytes gotten into the system */

Uint erts_max_ports;
Uint erts_port_tab_index_mask;

const ErlDrvTermData driver_term_nil = (ErlDrvTermData)NIL;



static FUNCTION(void, terminate_port, (int));

#define NUM2PORT(ix) ( ( ((ix) < 0) || ((ix)>=erts_max_ports) || \
			(erts_port[(ix)].status == FREE)) ? NULL : &erts_port[ix])

#define IOQ(ix) ( ( ((ix) < 0) || \
                    ((ix) >= erts_max_ports) || \
                    (erts_port[(ix)].status == FREE)) ? NULL : \
                   &erts_port[(ix)].ioq )

/*
 * Line buffered I/O.
 */
typedef struct line_buf_context {
    LineBuf **b;
    char *buf;
    int left;
    int retlen;
} LineBufContext;

#define LINEBUF_EMPTY 0
#define LINEBUF_EOL 1
#define LINEBUF_NOEOL 2
#define LINEBUF_ERROR -1

#define LINEBUF_STATE(LBC) ((*(LBC).b)->data[0])

#define LINEBUF_DATA(LBC) (((*(LBC).b)->data) + 1)
#define LINEBUF_DATALEN(LBC) ((LBC).retlen)

#define LINEBUF_INITIAL 100


/* The 'number' field in a port now has two parts: the lowest bits
   contain the index in the port table, and the higher bits are a counter
   which is incremented each time we look for a free port and start from
   the beginning of the table. erts_max_ports is the number of file descriptors,
   rounded up to a power of 2.
   To get the index from a port, use the macro 'internal_port_index';
   'port_number' returns the whole number field.
*/

static int port_extra_shift;
static int port_extra_limit;
static int port_extra_n;
static int last_port;

static int
get_free_port(void)
{
    int i;

    i = last_port + 1;
    while(1) {
	if (i == erts_max_ports) {
	    i = 0;
	    if (++port_extra_n >= port_extra_limit)
	       port_extra_n = 0;
	    continue;
	}
	if (i == last_port)
	    return -1;
	if (erts_port[i].status == FREE) {
	    last_port = i;
	    return(i);
	}
	i++;
    }
}

/*
** Initialize v_start to point to the small fixed vector.
** Once (reallocated) we never reset the pointer to the small vector
** This is a possible optimisation.
*/
static void initq(Port* prt)
{
    ErlIOQueue* q = &prt->ioq;

    q->size = 0;
    q->v_head = q->v_tail = q->v_start = q->v_small;
    q->v_end = q->v_small + SMALL_IO_QUEUE;
    q->b_head = q->b_tail = q->b_start = q->b_small;
    q->b_end = q->b_small + SMALL_IO_QUEUE;
}

static void stopq(Port* prt)
{
    ErlIOQueue* q = &prt->ioq;
    ErlDrvBinary** binp = q->b_head;

    if (q->v_start != q->v_small)
	erts_free(ERTS_ALC_T_IOQ, (void *) q->v_start);

    while(binp < q->b_tail) {
	if (*binp != NULL)
	    driver_free_binary(*binp);
	binp++;
    }
    if (q->b_start != q->b_small)
	erts_free(ERTS_ALC_T_IOQ, (void *) q->b_start);
    q->v_start = q->v_end = q->v_head = q->v_tail = NULL;
    q->b_start = q->b_end = q->b_head = q->b_tail = NULL;
    q->size = 0;
}



static void
setup_port(int port_num, Eterm pid, ErlDrvEntry *driver, long ret, char *name)
{
    Port* prt = &erts_port[port_num];

    prt->status = CONNECTED;
    prt->control_flags = 0;
    prt->connected = pid;
    prt->drv_ptr = driver;
    prt->drv_data = ret;
    prt->bytes_in = 0;
    prt->bytes_out = 0;
    prt->dist_entry = NULL;
    prt->reg = NULL;
    sys_memset(&prt->tm, 0, sizeof(ErlTimer));
    if (prt->name != NULL) {
	erts_free(ERTS_ALC_T_PORT_NAME, (void *) prt->name);
    }
    prt->name = (char*) erts_alloc(ERTS_ALC_T_PORT_NAME, sys_strlen(name)+1);
    prt->suspended  = NULL;
    sys_strcpy(prt->name, name);
    prt->links = NULL;
    prt->linebuf = NULL;
    prt->bp = NULL;
    prt->data = am_undefined;    
    initq(prt);
}

void
wake_process_later(Eterm this_port, Process *process)
{
    int port_pos;
    ProcessList** p;
    ProcessList* new_p;

    port_pos = internal_port_index(this_port);
    if (erts_port[port_pos].status == FREE)
	return;

    for (p = &(erts_port[port_pos].suspended); *p != NULL; p = &((*p)->next))
	/* Empty loop body */;

    new_p = (ProcessList *) erts_alloc(ERTS_ALC_T_PROC_LIST,
				       sizeof(ProcessList));
    new_p->pid = process->id; /* Internal pid */
    new_p->next = NULL;
    *p = new_p;
}

/*
   Opens a driver.
   Returns the non-negative port number, if successful.
   If there is an error, -1 or -2 or -3 is returned. -2 means that
   there is valid error information in 'errno'.
   Returning -3 means that an error in the given options was detected.
   The driver start function must obey the same conventions.
*/
int
open_driver(ErlDrvEntry* driver,	/* Pointer to driver entry. */
	    Eterm pid,			/* Current process. */
	    char* name,			/* Driver name. */
	    SysDriverOpts* opts)	/* Options. */
{
    int port_num;
    long ret;

    if ((port_num = get_free_port()) < 0)
    {
       errno = ENFILE;
       return -2;
    }

    erts_port[port_num].id = make_internal_port(port_num +
						(port_extra_n
						 << port_extra_shift));

    if (driver == &spawn_driver_entry) {
	char *p;
	DE_List *de;

	/*
	 * Dig out the name of the driver or port program.
	 */

	p = name;
	while(*p != '\0' && *p != ' ')
	    p++;
	if (*p == '\0')
	    p = NULL;
	else
	    *p = '\0';

	/*
	 * Search for a driver having this name.  Defaults to spawn_driver
	 * if not found.
	 */
	 
	for (de = driver_list; de != NULL; de = de->next) {
	    if (strcmp(de->drv->driver_name, name) == 0) {
		driver = de->drv;
		break;
	    }
	}
	if (p != NULL)
	    *p = ' ';
    }

    if (driver != &spawn_driver_entry && opts->exit_status) {
	return -3;
    }

    /*
     * We'll set up the port before calling the start function,
     * to allow message sending and setting timers in the start function.
     */

    ret = 0;
    setup_port(port_num, pid, driver, ret, name);
    if (driver->start) {
	ret = (long)(*driver->start)((ErlDrvPort)port_num, name, opts);
    }
    if (ret == -1 || ret == -2 || ret == -3) {
	/*
	 * Must clean up the port.
	 */
	erl_cancel_timer(&(erts_port[port_num].tm));
	stopq(&erts_port[port_num]);
	erts_port[port_num].status = FREE;
	if (erts_port[port_num].linebuf != NULL) {
	    erts_free(ERTS_ALC_T_LINEBUF,
		      (void *) erts_port[port_num].linebuf);
	    erts_port[port_num].linebuf = NULL;
	}
	return ret;
    }
    erts_port[port_num].drv_data = ret;
    return port_num;
}

/* Fills a possibly deep list of chars and binaries into vec
** Small characters are first stored in the buffer buf of length ln
** binaries found are copied and linked into msoh
** Return  vector length on succsess,
**        -1 on overflow
**        -2 on type error
*/

#define SET_VEC(iov, bv, bin, ptr, len, vlen) do { \
   (iov)->iov_base = (ptr); \
   (iov)->iov_len = (len); \
   *(bv)++ = (bin); \
   (iov)++; \
   (vlen)++; \
} while(0)

#define SET_BINARY_VEC(obj)					\
do {								\
    Eterm _real;						\
    Uint _offset;						\
    Eterm* _bptr;						\
    int _size = binary_size(obj);				\
    GET_REAL_BIN(obj, _real, _offset);				\
    _bptr = binary_val(_real);					\
    if (thing_subtag(*_bptr) == REFC_BINARY_SUBTAG) {		\
	ProcBin* _pb = (ProcBin *) _bptr;			\
        if (bin_limit && _size < bin_limit) {                   \
            if (len < _size) {                                  \
                goto L_overflow;                                \
            }                                                   \
            sys_memcpy(buf, _pb->bytes+_offset, _size);         \
            csize += _size;                                     \
            buf += _size;                                       \
            len -= _size;                                       \
        } else {                                                \
	    if (csize != 0) {					\
	        SET_VEC(iov, binv, cbin, cptr, csize, vlen);	\
	        cptr = buf;				       	\
	        csize = 0;				       	\
	    }							\
	    SET_VEC(iov, binv, Binary2ErlDrvBinary(_pb->val),	\
		    _pb->bytes+_offset, _size, vlen);		\
        }                                                       \
    } else {							\
	ErlHeapBin* hb = (ErlHeapBin *) _bptr;			\
	if (len < _size) {					\
	    goto L_overflow;					\
	}							\
	sys_memcpy(buf, ((byte *) hb->data) + _offset, _size);	\
        csize += _size;						\
	buf += _size;						\
	len -= _size;						\
    }								\
} while (0)				  

static int io_list_to_vec(Eterm obj, /* io-list */
			  SysIOVec* iov, /* io vector */
			  ErlDrvBinary** binv, /* binary reference vector */
			  ErlDrvBinary* cbin,  /* binary to store characters */
			  int bin_limit) /* small binaries limit */
{
    DECLARE_ESTACK(s);
    Eterm* objp;
    char *buf  = cbin->orig_bytes;
    int len    = cbin->orig_size;
    int csize  = 0;
    int vlen   = 0;
    char* cptr = buf;

    goto L_jump_start;  /* avoid push */

    while (!ESTACK_ISEMPTY(s)) {
	obj = ESTACK_POP(s);
    L_jump_start:
	if (is_list(obj)) {
	L_iter_list:
	    objp = list_val(obj);
	    obj = CAR(objp);
	    if (is_byte(obj)) {
		if (len == 0)
		    goto L_overflow;
		*buf++ = unsigned_val(obj);
		csize++;
		len--;
	    } else if (is_binary(obj)) {
		SET_BINARY_VEC(obj);
	    } else if (is_list(obj)) {
		ESTACK_PUSH(s, CDR(objp));
		goto L_iter_list;    /* on head */
	    } else if (!is_nil(obj)) {
		goto L_type_error;
	    }	    
	    obj = CDR(objp);
	    if (is_list(obj))
		goto L_iter_list; /* on tail */
	    else if (is_binary(obj)) {
		SET_BINARY_VEC(obj);
	    } else if (!is_nil(obj)) {
		goto L_type_error;
	    }
	} else if (is_binary(obj)) {
	    SET_BINARY_VEC(obj);
	} else if (!is_nil(obj)) {
	    goto L_type_error;
	}
    }

    if (csize != 0) {
	SET_VEC(iov, binv, cbin, cptr, csize, vlen);
    }

    DESTROY_ESTACK(s);
    return vlen;

 L_type_error:
    DESTROY_ESTACK(s);
    return -2;

 L_overflow:
    DESTROY_ESTACK(s);
    return -1;
}

#define IO_LIST_VEC_COUNT(obj)						\
do {									\
    int _size = binary_size(obj);					\
    Eterm _real;							\
    Uint _offset;							\
    GET_REAL_BIN(obj, _real, _offset);					\
    if (thing_subtag(*binary_val(_real)) == REFC_BINARY_SUBTAG) {	\
	b_size += _size;						\
	in_clist = 0;							\
	v_size++;							\
        if (_size >= bin_limit) {                                       \
            p_in_clist = 0;                                             \
            p_v_size++;                                                 \
        } else {                                                        \
            p_c_size += _size;                                          \
            if (!p_in_clist) {                                          \
                p_in_clist = 1;                                         \
                p_v_size++;                                             \
            }                                                           \
        }                                                               \
    } else {								\
	c_size += _size;						\
	if (!in_clist) {						\
	    in_clist = 1;						\
	    v_size++;							\
	}								\
	p_c_size += _size;						\
	if (!p_in_clist) {						\
	    p_in_clist = 1;						\
	    p_v_size++;							\
	}								\
    }									\
} while (0)


/* 
** Size of a io list in bytes
** return -1 if error
** returns:            - Total size of io list
**           vsize     - SysIOVec size needed for a writev
**           csize     - Number of bytes not in binary (in the common binary)
**           pvsize    - SysIOVec size needed if packing small binaries
**           pcsize    - Number of bytes in the common binary if packing
*/

static int 
io_list_vec_len(Eterm obj, int* vsize, int* csize, 
		int bin_limit, /* small binaries limit */
		int * pvsize, int * pcsize)
{
    DECLARE_ESTACK(s);
    Eterm* objp;
    int v_size = 0;
    int c_size = 0;
    int b_size = 0;
    int in_clist = 0;
    int p_v_size = 0;
    int p_c_size = 0;
    int p_in_clist = 0;

    goto L_jump_start;  /* avoid a push */

    while (!ESTACK_ISEMPTY(s)) {
	obj = ESTACK_POP(s);
    L_jump_start:
	if (is_list(obj)) {
	L_iter_list:
	    objp = list_val(obj);
	    obj = CAR(objp);

	    if (is_byte(obj)) {
		c_size++;
		if (!in_clist) {
		    in_clist = 1;
		    v_size++;
		}
		p_c_size++;
		if (!p_in_clist) {
		    p_in_clist = 1;
		    p_v_size++;
		}
	    }
	    else if (is_binary(obj)) {
		IO_LIST_VEC_COUNT(obj);
	    }
	    else if (is_list(obj)) {
		ESTACK_PUSH(s, CDR(objp));
		goto L_iter_list;   /* on head */
	    }
	    else if (!is_nil(obj)) {
		goto L_type_error;
	    }

	    obj = CDR(objp);
	    if (is_list(obj))
		goto L_iter_list;   /* on tail */
	    else if (is_binary(obj)) {  /* binary tail is OK */
		IO_LIST_VEC_COUNT(obj);
	    }
	    else if (!is_nil(obj)) {
		goto L_type_error;
	    }
	}
	else if (is_binary(obj)) {
	    IO_LIST_VEC_COUNT(obj);
	}
	else if (!is_nil(obj)) {
	    goto L_type_error;
	}
    }

    DESTROY_ESTACK(s);
    if (vsize != NULL)
	*vsize = v_size;
    if (csize != NULL)
	*csize = c_size;
    if (pvsize != NULL)
	*pvsize = p_v_size;
    if (pcsize != NULL)
	*pcsize = p_c_size;
    return c_size + b_size;

 L_type_error:
    DESTROY_ESTACK(s);
    return -1;
}

#define ERL_SMALL_IO_BIN_LIMIT (4*ERL_ONHEAP_BIN_LIMIT)
#define SMALL_WRITE_VEC  16


/* write data to a port */
int write_port(Eterm caller_id, int ix, Eterm list)
{
    char *buf;
    Port* p = &erts_port[ix];
    ErlDrvEntry *drv = p->drv_ptr;
    int size;
    
    p->caller = caller_id;
    if (drv->outputv != NULL) {
	int vsize;
	int csize;
	int pvsize;
	int pcsize;
	int blimit;
	SysIOVec iv[SMALL_WRITE_VEC];
	ErlDrvBinary* bv[SMALL_WRITE_VEC];
	SysIOVec* ivp;
	ErlDrvBinary**  bvp;
	ErlDrvBinary* cbin;
	ErlIOVec ev;

	if ((size = io_list_vec_len(list, &vsize, &csize, 
				    ERL_SMALL_IO_BIN_LIMIT,
				    &pvsize, &pcsize)) < 0) {
	    goto bad_value;
	}
	/* To pack or not to pack (small binaries) ...? */
	vsize++;
	if (vsize <= SMALL_WRITE_VEC) {
	    /* Do NOT pack */
	    blimit = 0;
	} else {
	    /* Do pack */
	    vsize = pvsize + 1;
	    csize = pcsize;
	    blimit = ERL_SMALL_IO_BIN_LIMIT;
	}
	/* Use vsize and csize from now on */
	if (vsize <= SMALL_WRITE_VEC) {
	    ivp = iv;
	    bvp = bv;
	} else {
	    ivp = (SysIOVec *) erts_alloc(ERTS_ALC_T_TMP,
					  vsize * sizeof(SysIOVec));
	    bvp = (ErlDrvBinary**) erts_alloc(ERTS_ALC_T_TMP,
					      vsize * sizeof(ErlDrvBinary*));
	}
	cbin = driver_alloc_binary(csize);
	if (!cbin)
	    erts_alloc_enomem(ERTS_ALC_T_DRV_BINARY, sizeof(Binary) + csize);

	/* Element 0 is for driver usage to add header block */
	ivp[0].iov_base = NULL;
	ivp[0].iov_len = 0;
	bvp[0] = NULL;
	ev.vsize = io_list_to_vec(list, ivp+1, bvp+1, cbin, blimit);
	ev.vsize++;
#if 0
	/* This assertion may say something useful, but it can
	   be falsified during the emulator test suites. */
	ASSERT((ev.vsize >= 0) && (ev.vsize == vsize));
#endif
	ev.size = size;  /* total size */
	ev.iov = ivp;
	ev.binv = bvp;
	(*drv->outputv)((ErlDrvData)p->drv_data, &ev);
	if (ivp != iv) {
	    erts_free(ERTS_ALC_T_TMP, (void *) ivp);
	}
	if (bvp != bv) {
	    erts_free(ERTS_ALC_T_TMP, (void *) bvp);
	}
	driver_free_binary(cbin);
    } else {
	int r;
	
	size = TMP_BUF_SIZE - 5;
	r = io_list_to_buf(list, (char*)tmp_buf, size);
	if (r >= 0) {
	    size -= r;
	    if (drv->output) {
		(*drv->output)((ErlDrvData)p->drv_data,(char*) tmp_buf, size);
	    }
	}
	else if (r == -2)
	    goto bad_value;
	else if (r == -1) { /* Overflow */
	    if ((size = io_list_len(list)) < 0) {
		goto bad_value;
	    }

	    /*
	     * I know drivers that pad space with '\0' this is clearly
	     * incorrect but I don't feel like fixing them now, insted
	     * add ONE extra byte.
	     */
	    buf = erts_alloc(ERTS_ALC_T_TMP, size+1); 
	    r = io_list_to_buf(list, buf, size);
	    if (drv->output) {
		(*drv->output)((ErlDrvData)p->drv_data, (char*)buf, size);
	    }
	    erts_free(ERTS_ALC_T_TMP, buf);
	}
    }
    p->bytes_out += size;
    bytes_out += size;
    return 0;

 bad_value:
    cerr_pos = 0;
    erl_printf(CBUF, "Bad value on output port '%s'\n", p->name);
    send_error_to_logger(NIL);
    return 1;
    
}

/* initialize the port array */
void init_io(void)
{
    int i;
    ErlDrvEntry** dp;
    ErlDrvEntry* drv;
    char *maxports;

    maxports = getenv("ERL_MAX_PORTS");
    if (maxports != NULL) 
	erts_max_ports = atoi(maxports);
    else
	erts_max_ports = sys_max_files();

    last_port = 0;
    if (erts_max_ports > ERTS_MAX_PORTS)
	erts_max_ports = ERTS_MAX_PORTS;
    else if (erts_max_ports < 1024)
	erts_max_ports = 1024;

    port_extra_shift = erts_fit_in_bits(erts_max_ports - 1);
    port_extra_limit = 1 << (ERTS_PORTS_BITS - port_extra_shift);
    port_extra_n = 0;

    erts_port_tab_index_mask = ~(~((Uint) 0) << port_extra_shift);
    erts_max_ports = 1 << port_extra_shift;

    driver_list = NULL;

    if (erts_max_ports * sizeof(Port) <= erts_max_ports) {
	/* More memory needed than the whole address space. */
	erts_alloc_enomem(ERTS_ALC_T_PORT_TABLE, ~((Uint) 0));
    }

    erts_port = (Port *) erts_alloc(ERTS_ALC_T_PORT_TABLE,
				    erts_max_ports * sizeof(Port));

    bytes_out = 0;
    bytes_in = 0;

    for (i = 0; i < erts_max_ports; i++) {
	erts_port[i].status = FREE;
	erts_port[i].name = NULL;
	erts_port[i].links = NULL;
	erts_port[i].linebuf = NULL;
    }

    sys_init_io(tmp_buf, (size_t)TMP_BUF_SIZE);

    if (fd_driver_entry.init != NULL)
	(*fd_driver_entry.init)();
    if (vanilla_driver_entry.init != NULL)
	(*vanilla_driver_entry.init)();
    if (spawn_driver_entry.init != NULL)
	(*spawn_driver_entry.init)();
    for (dp = driver_tab; *dp != NULL; dp++) {
	drv = *dp;
	add_driver_entry(*dp);	/* will call init as well! */
    }
}

/*
 * Buffering of data when using line oriented I/O on ports
 */

/* 
 * Buffer states 
 */
#define LINEBUF_MAIN 0
#define LINEBUF_FULL 1
#define LINEBUF_CR_INSIDE 2
#define LINEBUF_CR_AFTER 3

/*
 * Creates a LineBuf to be added to the port structure,
 * Returns: Pointer to a newly allocated and initialized LineBuf.
 * Parameters:
 * bufsiz - The (maximum) size of the line buffer.
 */
LineBuf *allocate_linebuf(bufsiz)
int bufsiz;
{
    int ovsiz = (bufsiz < LINEBUF_INITIAL) ? bufsiz : LINEBUF_INITIAL;
    LineBuf *lb = (LineBuf *) erts_alloc(ERTS_ALC_T_LINEBUF,
					 sizeof(LineBuf)+ovsiz);
    lb->ovsiz = ovsiz;
    lb->bufsiz = bufsiz;
    lb->ovlen = 0;
    lb->data[0] = LINEBUF_MAIN; /* state */
    return lb;
}

/*
 * Initializes a LineBufContext to be used in calls to read_linebuf
 * or flush_linebuf.
 * Returns: 0 if ok, <0 on error.
 * Parameters:
 * lc - Pointer to an allocated LineBufContext.
 * lb - Pointer to a LineBuf structure (probably from the Port structure).
 * buf - A buffer containing the data to be read and split to lines.
 * len - The number of bytes in buf.
 */
static int init_linebuf_context(LineBufContext *lc, LineBuf **lb, char *buf, int len)
{
    if(lc == NULL || lb == NULL)
	return -1;
    lc->b = lb;
    lc->buf = buf;
    lc->left = len;
    return 0;
}

static void resize_linebuf(LineBuf **b)
{
    int newsiz = (((*b)->ovsiz * 2) > (*b)->bufsiz) ? (*b)->bufsiz : 
	(*b)->ovsiz * 2;
    *b = (LineBuf *) erts_realloc(ERTS_ALC_T_LINEBUF,
				  (void *) *b,
				  sizeof(LineBuf)+newsiz);
    (*b)->ovsiz = newsiz;
}

/*
 * Delivers all data in the buffer regardless of newlines (always
 * an LINEBUF_NOEOL. Has to be called until it return LINEBUF_EMPTY.
 * Return values and barameters as read_linebuf (see below).
 */
static int flush_linebuf(LineBufContext *bp)
{
    bp->retlen = (*bp->b)->ovlen;
    switch(LINEBUF_STATE(*bp)){
    case LINEBUF_CR_INSIDE:
	if((*bp->b)->ovlen >= (*bp->b)->ovsiz)
	    resize_linebuf(bp->b);
	LINEBUF_DATA(*bp)[((*bp->b)->ovlen)++] = '\r';
	++bp->retlen; /* fall through instead of switching state... */
    case LINEBUF_MAIN:
    case LINEBUF_FULL:
	(*bp->b)->ovlen = 0;
	LINEBUF_STATE(*bp) = LINEBUF_MAIN;
	if(!bp->retlen)
	    return LINEBUF_EMPTY;
	return LINEBUF_NOEOL;
    case LINEBUF_CR_AFTER:
	LINEBUF_STATE(*bp) = LINEBUF_CR_INSIDE;
	(*bp->b)->ovlen = 0;
	if(!bp->retlen)
	    return LINEBUF_EMPTY;
	return LINEBUF_NOEOL;
    default:
	return LINEBUF_ERROR;
    }    
}

/*
 * Reads input from a buffer and "chops" it up in lines.
 * Has to be called repeatedly until it returns LINEBUF_EMPTY
 * to get all lines in buffer.
 * Handles both <LF> and <CR><LF> style newlines.
 * On Unix, this is slightly incorrect, as <CR><LF> is NOT to be regarded
 * as a newline together, but i treat newlines equally in all systems
 * to avoid putting this in sys.c or clutter it with #ifdef's.
 * Returns: LINEBUF_EMPTY if there is no more data that can be
 *   determined as a line (only part of a line left), LINEBUF_EOL if a whole
 *   line could be delivered and LINEBUF_NOEOL if the buffer size has been
 *   exceeded. The data and the data length can be accesed through the 
 *   LINEBUF_DATA and the LINEBUF_DATALEN macros applied to the LineBufContext.
 * Parameters: 
 * bp - A LineBufContext that is initialized with 
 *   the init_linebuf_context call. The context has to be retained during
 *   all calls that returns other than LINEBUF_EMPTY. When LINEBUF_EMPTY
 *   is returned the context can be discarded and a new can be created when new
 *   data arrives (the state is saved in the Port structure).
 */
static int read_linebuf(LineBufContext *bp)
{
    for(;;){
	if(bp->left == 0)
	    return LINEBUF_EMPTY;
	if(*bp->buf == '\n'){
	    LINEBUF_STATE(*bp) = LINEBUF_MAIN;
	    ++(bp->buf);
	    --(bp->left);
	    bp->retlen = (*bp->b)->ovlen;
	    (*bp->b)->ovlen = 0;
	    return LINEBUF_EOL;
	}
	switch(LINEBUF_STATE(*bp)){
	case LINEBUF_MAIN:
	    if((*bp->b)->ovlen == (*bp->b)->bufsiz)
		LINEBUF_STATE(*bp) = LINEBUF_FULL;
	    else if(*bp->buf == '\r'){
		++(bp->buf);
		--(bp->left);
		LINEBUF_STATE(*bp) = LINEBUF_CR_INSIDE;
	    } else {
		if((*bp->b)->ovlen >= (*bp->b)->ovsiz)
		    resize_linebuf(bp->b);
		LINEBUF_DATA(*bp)[((*bp->b)->ovlen)++] = *((bp->buf)++);
		--(bp->left);
	    }
	    continue;
	case LINEBUF_FULL:
	    if(*bp->buf == '\r'){
		++(bp->buf);
		--(bp->left);
		LINEBUF_STATE(*bp) = LINEBUF_CR_AFTER;
	    } else {
		bp->retlen = (*bp->b)->ovlen;
		(*bp->b)->ovlen = 0;
		LINEBUF_STATE(*bp) = LINEBUF_MAIN;
		return LINEBUF_NOEOL;
	    }
	    continue;
	case LINEBUF_CR_INSIDE:
	    if((*bp->b)->ovlen >= (*bp->b)->ovsiz)
		resize_linebuf(bp->b);
	    LINEBUF_DATA(*bp)[((*bp->b)->ovlen)++] = '\r';
	    LINEBUF_STATE(*bp) = LINEBUF_MAIN;
	    continue;
	case LINEBUF_CR_AFTER:
	    bp->retlen = (*bp->b)->ovlen;
	    (*bp->b)->ovlen = 0;
	    LINEBUF_STATE(*bp) = LINEBUF_CR_INSIDE;
	    return LINEBUF_NOEOL;
	default:
	    return LINEBUF_ERROR;
	}
    }
}


/* 
 * Deliver a "read" message.
 * hbuf -- byte that are always formated as a list
 * hlen -- number of byte in header
 * buf  -- data 
 * len  -- length of data
 */

static void deliver_read_message(Port* prt, Eterm to,
				 char *hbuf, int hlen,
				 char *buf, int len, int eol)
{
    int need;
    Eterm listp;
    Eterm tuple;
    Process* rp;
    Eterm* hp;

    need = 3 + 3;
    if (prt->status & LINEBUF_IO) {
	need += 3;
    }
    if (prt->status & BINARY_IO) {
	need += hlen*2 + PROC_BIN_SIZE;
    } else {
	need += (hlen + len)*2;
    }
    if (is_not_internal_pid(to))
	return;
    if (internal_pid_index(to) >= erts_max_processes)
	return;
    rp = process_tab[internal_pid_index(to)];

    if (INVALID_PID(rp, to))
	return;

#ifdef SHARED_HEAP
    if(rp->status != P_RUNNING) {
	hp = erts_global_alloc(need);
    } else {
        hp = HAlloc(rp, need);
    }
#else
    /*
     * XXX Multi-thread note: Allocating on another process's heap.
     */
    hp = HAlloc(rp, need);
#endif

    listp = NIL;
    if ((prt->status & BINARY_IO) == 0) {
	listp = buf_to_intlist(&hp, buf, len, listp);
    } else if (buf != NULL) {
	ProcBin* pb;
	Binary* bptr;

	bptr = erts_bin_nrml_alloc(len);
	bptr->flags = 0;
	bptr->orig_size = len;
	bptr->refc = 1;
	sys_memcpy(bptr->orig_bytes, buf, len);

	pb = (ProcBin *) hp;
	pb->thing_word = HEADER_PROC_BIN;
	pb->size = len;
	pb->next = MSO(rp).mso;
	MSO(rp).mso = pb;
	pb->val = bptr;
	pb->bytes = bptr->orig_bytes;
	hp += PROC_BIN_SIZE;

	MSO(rp).overhead += pb->size / BINARY_OVERHEAD_FACTOR / sizeof(Eterm);
	listp = make_binary(pb);
    }

    /* Prepend the header */
    if (hlen > 0) {
	listp = buf_to_intlist(&hp, hbuf, hlen, listp);
    }

    if (prt->status & LINEBUF_IO){
	listp = TUPLE2(hp, (eol) ? am_eol : am_noeol, listp); 
	hp += 3;
    }
    tuple = TUPLE2(hp, am_data, listp);
    hp += 3;

    tuple = TUPLE2(hp, prt->id, tuple);
    hp += 3;

    queue_message_tt(rp, NULL, tuple, am_undefined);
}

/* 
 * Deliver all lines in a line buffer, repeats calls to
 * deliver_read_message, and takes the same parameters.
 */
static void deliver_linebuf_message(Port* prt, Eterm to, 
				    char* hbuf, int hlen,
				    char *buf, int len)
{
    LineBufContext lc;
    int ret;
    if(init_linebuf_context(&lc,&(prt->linebuf), buf, len) < 0)
	return;
    while((ret = read_linebuf(&lc)) > LINEBUF_EMPTY)
	deliver_read_message(prt, to, hbuf, hlen, LINEBUF_DATA(lc), 
			     LINEBUF_DATALEN(lc), (ret == LINEBUF_EOL));
}

/*
 * Deliver any nonterminated lines in the line buffer before the
 * port gets closed.
 * Has to be called before terminate_port.
 * Parameters:
 * prt -  Pointer to a Port structure for this port.
 */
static void flush_linebuf_messages(Port *prt)
{
    LineBufContext lc;
    int ret;

    if(prt == NULL || !(prt->status & LINEBUF_IO))
	return;

    if(init_linebuf_context(&lc,&(prt->linebuf), NULL, 0) < 0)
	return;
    while((ret = flush_linebuf(&lc)) > LINEBUF_EMPTY)
	deliver_read_message(prt, prt->connected, NULL, 0, LINEBUF_DATA(lc), 
			     LINEBUF_DATALEN(lc), (ret == LINEBUF_EOL));
}    

static void
deliver_vec_message(
		    Port* prt,			/* Port */
		    Eterm to,			/* Receiving pid */
		    char* hbuf,			/* "Header" buffer... */
		    int hlen,			/* ... and its length */
		    ErlDrvBinary** binv,	/* Vector of binaries */
		    SysIOVec* iov,		/* I/O vector */
		    int vsize,			/* Size of binv & iov */
		    int csize)			/* Size of characters in 
						   iov (not hlen) */
{
    int need;
    Eterm listp;
    Eterm tuple;
    Process* rp;
    Eterm* hp;

    need = 12;
    if (prt->status & BINARY_IO) {
	need += (2+PROC_BIN_SIZE)*vsize + hlen*2;
    } else {
	need += (hlen+csize)*2;
    }

    if (is_not_internal_pid(to))
	return;

    if (internal_pid_index(to) >= erts_max_processes)
	return;
    rp = process_tab[internal_pid_index(to)];
    if (INVALID_PID(rp, to))
	return;

#ifdef SHARED_HEAP
    if(rp->status != P_RUNNING) {
	hp = erts_global_alloc(need);
    } else {
        hp = HAlloc(rp, need);
    }
#else
    /*
     * XXX Multi-thread note: Allocating on another process's heap.
     */
    hp = HAlloc(rp, need);
#endif

    listp = NIL;
    iov += (vsize-1);  /* start from end (for concat) */
    binv += (vsize-1); /* - || - */

    while (vsize--) {
	if ((prt->status & BINARY_IO) == 0) {
	    listp = buf_to_intlist(&hp, iov->iov_base, iov->iov_len, listp);
	} else {
	    ErlDrvBinary* b;
	    ProcBin* pb = (ProcBin*) hp;
	    byte* base;

	    if ((b = *binv) == NULL) {
		b = driver_alloc_binary(iov->iov_len);
		sys_memcpy(b->orig_bytes, iov->iov_base, iov->iov_len);
		base = b->orig_bytes;
	    } else {
		/* Must increment reference count, caller calls free */
		b->refc++;
		base = iov->iov_base;
	    }
	    pb->thing_word = HEADER_PROC_BIN;
	    pb->size = iov->iov_len;
	    pb->next = MSO(rp).mso;
	    MSO(rp).mso = pb;
	    pb->val = ErlDrvBinary2Binary(b);
	    pb->bytes = base;
	    hp += PROC_BIN_SIZE;
	    
	    MSO(rp).overhead += iov->iov_len / BINARY_OVERHEAD_FACTOR /
		sizeof(Eterm);

	    if (listp == NIL)  /* compatible with deliver_bin_message */
		listp = make_binary(pb);
	    else {
		listp = CONS(hp, make_binary(pb), listp);
		hp += 2;
	    }
	}
	iov--;
	binv--;
    }

    if (hlen > 0) {		/* Prepend the header */
	listp = buf_to_intlist(&hp, hbuf, hlen, listp);
    }

    tuple = TUPLE2(hp, am_data, listp);
    hp += 3;
    tuple = TUPLE2(hp, prt->id, tuple);
    hp += 3;

    queue_message_tt(rp, NULL, tuple, am_undefined);
}


static void deliver_bin_message(Port*  prt,         /* port */
				Eterm to,           /* receivng pid */
				char* hbuf,         /* "header" buffer */
				int hlen,           /* and it's length */
				ErlDrvBinary* bin,  /* binary data */
				int offs,           /* offset into binary */
				int len)            /* length of binary */
{
    SysIOVec vec;

    vec.iov_base = bin->orig_bytes+offs;
    vec.iov_len = len;
    deliver_vec_message(prt, to, hbuf, hlen, &bin, &vec, 1, len);
}

/* flush the port I/O queue and terminate if empty */
/* 
 * Note. 
 *
 * The test for (p->status != FREE) is important since the driver's 
 * flush function might call driver_async, which when using no threads
 * and being short circuited will notice that the io queue is empty
 * (after calling the driver's async_ready) and recursively call 
 * terminate_port. So when we get back here, the port is already terminated.
 */
static void flush_port(int ix)
{
    Port *p = &erts_port[ix];

    if (p->drv_ptr->flush != NULL) {
	(*p->drv_ptr->flush)((ErlDrvData)p->drv_data);
    }
    if ((p->status != FREE) && (p->ioq.size == 0)) {
	terminate_port(ix);
    }
}


/* stop and delete a port that is CLOSING */
static void
terminate_port(int ix)
{
    Port* prt = &erts_port[ix];
    ErlDrvEntry *drv;

    if (prt->status & SEND_CLOSED) {
	prt->status &= ~SEND_CLOSED;
	deliver_result(prt->id, prt->connected, am_closed);
    }
    erl_cancel_timer(&prt->tm);

    drv = prt->drv_ptr;
    if ((drv != NULL) && (drv->stop != NULL)) {
	(*drv->stop)((ErlDrvData)prt->drv_data);
    }
    stopq(prt);        /* clear queue memory */
    prt->status = FREE;
    prt->drv_ptr = NULL;
    if(prt->linebuf != NULL){
	erts_free(ERTS_ALC_T_LINEBUF, (void *) prt->linebuf);
	prt->linebuf = NULL;
    }
    if (prt->bp != NULL) {
	free_message_buffer(prt->bp);
	prt->bp = NULL;
	prt->data = am_undefined;
    }

    ASSERT(prt->dist_entry == NULL);
}


/*
   Does schedule_exit, and also if we are to exit ourselves, stashes
   away the reason, so we can do a BIF_ERROR(USER_EXIT, ...) later.
*/

static void
schedule_exit2(Process *rp, Eterm reason)
{
   if (rp->status == P_RUNNING) {
       rp->status = P_EXITING;
       rp->fvalue = reason;
   } else {
       schedule_exit(rp, reason);
   }
}

/* 'from' is sending 'this_port' an exit signal, (this_port must be internal).
 * If reason is normal we don't do anything, *unless* from is our connected
 * process in which case we close the port. Any other reason kills the port.
 * If 'from' is ourself we always die.
 * When a driver has data in ioq then driver will be set to closing
 * and become inaccessible to the processes. One exception exists and
 * that is to kill a port till reason kill. Then the port is stopped.
 * 
 */
void
do_exit_port(Eterm this_port, Eterm from, Eterm reason)
{
   Eterm item;
   Process *rp;
   int ix;
   DistEntry *dep;
   Port* p;
   ErlLink* lnk;
   Eterm rreason;

   ASSERT(is_internal_port(this_port));

   ix = internal_port_index(this_port);
   p = &erts_port[ix];
   rreason = (reason == am_kill) ? am_killed : reason;

   if ((p->status == FREE) ||
       (p->status & EXITING) ||
       ((reason == am_normal) &&
	((from != p->connected) && (from != this_port))))
      return;

   if (p->id == erts_default_tracer) {
       erts_default_tracer = NIL;
       erts_default_process_flags &= ~TRACE_FLAGS;
   }

   /*
    * Setting the port to not busy here, frees the list of pending
    * processes and makes them runnable.
    */
   set_busy_port((ErlDrvPort)ix, 0);

   if (p->reg != NULL)
      unregister_name(NULL, p->reg->name);

   p->status |= EXITING;
   lnk = p->links;
   p->links = NULL;

   while(lnk != NULL) {
      item = lnk->item;
      switch(lnk->type) {
       case LNK_LINK:
	   if (is_external_pid(item)) {
	       dep = external_pid_dist_entry(item);
	       if(dep != erts_this_dist_entry)
		   dist_exit(dep, this_port, item, rreason);
	   }
	   else if (is_internal_pid(item)) {
	       if ((rp = pid2proc(item)) != NULL) {
		  del_link(find_link(&rp->links,LNK_LINK,this_port,NIL));
		  if (rp->flags & F_TRAPEXIT)
		      deliver_exit_message(this_port, rp, rreason);
		  else if (rreason != am_normal)
		      schedule_exit2(rp, rreason);
	       }
	    }
	   break;

/* Arndt's comment: how can this case arise? A port can't have a link
   to a node, can it? */
       case LNK_NODE:
	   ASSERT(is_node_name_atom(item));
	   dep = erts_sysname_to_connected_dist_entry(item);
	   if(dep)
	       del_link(find_link(&dep->links,LNK_NODE,this_port,NIL));
	   else {
	       /* XXX Is this possible? Shouldn't this link
		  previously have been removed if the node
		  had previously been disconnected. */
	       ASSERT(0);
	   }
	   break;

       case LNK_OMON:
       case LNK_TMON:	    
       default:
	 erl_exit(1, "bad type in link list\n");
	 break;
      }
      del_link(&lnk);
   }

   if ((p->status & DISTRIBUTION) && p->dist_entry) {
       do_net_exits(p->dist_entry);
       DEREF_DIST_ENTRY(p->dist_entry); 
       p->dist_entry = NULL; 
       p->status &= ~DISTRIBUTION;
   }

   if ((reason != am_kill) && (p->ioq.size > 0)) {
      p->status &= ~EXITING;	/* must turn it off */
      p->status |= CLOSING;
      flush_port(ix);
   }
   else
      terminate_port(ix);
}

/* About the states EXITING and CLOSING used above.
**
** EXITING is a recursion protection for do_exit_port(). It is unclear
** whether this state is necessary or not, it might be possible
** to merge it with CLOSING. EXITING only persists over a section of
** sequential (but highly recursive) code.
**
** CLOSING is a state where the port is in Limbo, waiting to pass on.
** All links are removed, and the port receives in/out-put events so
** as soon as the port queue gets empty terminate_port() is called.
*/



/* Command should be of the form
**   {PID, close}
**   {PID, {command, io-list}}
**   {PID, {connect, New_PID}}
**
**
*/
void port_command(Eterm caller_id, Eterm port_id, Eterm command)
{
    int ix;
    Eterm *tp;
    Eterm pid;
    Process* rp;

    if (is_not_internal_port(port_id))
	return;
    ix = internal_port_index(port_id);
    if (INVALID_PORT(erts_port+ix, port_id))
	return;

    if (is_tuple(command)) {
	tp = tuple_val(command);
	if ((tp[0] == make_arityval(2)) &&
	    ((pid = erts_port[ix].connected) == tp[1])) {
	    /* PID must be connected */
	    if (tp[2] == am_close) {
		erts_port[ix].status |= SEND_CLOSED;
		do_exit_port(port_id,pid,am_normal);
		return;
	    }
	    else if (is_tuple(tp[2])) {
		tp = tuple_val(tp[2]);
		if (tp[0] == make_arityval(2)) {
		    if (tp[1] == am_command) {
			if (write_port(caller_id, ix, tp[2]) == 0)
			    return;
		    }
		    if ((tp[1] == am_connect) && is_internal_pid(tp[2])) {
			erts_port[ix].connected = tp[2];
			deliver_result(port_id, pid, am_connected);
			return;
		    }
		}
	    }
	}
    }

    if ((rp = pid2proc(erts_port[ix].connected)) != NULL) {
	if (rp->flags & F_TRAPEXIT)
	    deliver_exit_message(port_id, rp, am_badsig);
	else 
	    schedule_exit2(rp, am_badsig);
    }
}

/*
 * Control a port synchronously. 
 * Returns either a list or a binary.
 */
int
port_control(Process* p, Port* prt, Uint command, Eterm iolist, Eterm* resp)
{
    byte* to_port = NULL;	/* Buffer to write to port. */
				/* Initialization is for shutting up
				   warning about use before set. */
    int to_len = 0;		/* Length of buffer. */
    int must_free = 0;		/* True if the buffer should be freed. */
    char port_result[64];	/* Buffer for result from port. */
    char* port_resp;		/* Pointer to result buffer. */
    int ret = 1;		/* Return value (default ok) */
    ErlDrvEntry* drv;
    int n;

    prt->caller = p->id; /* Internal pid */
    drv = prt->drv_ptr;

    if (drv->control == NULL) {
	DEBUGF(("No control routine\n"));
	return 0;
    }

    /*
     * Convert the iolist to a buffer, pointed to by to_port,
     * and with its length in to_len.
     */
    if (is_binary(iolist)) {
	GET_BINARY_BYTES(iolist, to_port);
	to_len = binary_size(iolist);
    } else {
	int r;
	/* XXX Random amounts of extra memory allocated/reserved.
	   The + 5 and + 20 should be removed in R8. */
	r = io_list_to_buf(iolist, (char*) tmp_buf, TMP_BUF_SIZE - 5);
	if (r >= 0) {
	    to_port = tmp_buf;
	    to_len = TMP_BUF_SIZE - 5 - r;
	} else if (r == -2) {
	    return 0;
	} else if (r == -1) {	/* Overflow */
	    if ((to_len = io_list_len(iolist)) < 0) {
		return 0;
	    }
	    must_free = 1;
	    to_port = erts_alloc(ERTS_ALC_T_TMP, to_len + 20);
	    r = io_list_to_buf(iolist, to_port, to_len+20);
	    ASSERT(r == 20);
	}
    }

    /*
     * Call the port's control routine.
     */
    port_resp = port_result;
    n = drv->control((ErlDrvData)prt->drv_data, command, to_port, to_len,
		     &port_resp, sizeof(port_result));
    if (must_free)
	erts_free(ERTS_ALC_T_TMP, (void *) to_port);

    if ((n >= 0) && (prt->control_flags & PORT_CONTROL_FLAG_BINARY)) {
	ErlDrvBinary *b = (ErlDrvBinary*)port_resp;

	if (b == NULL) {
	    *resp = NIL;
	    return ret;
	} else {
	    Eterm* hp;
	    ProcBin* pb;

	    hp = HAlloc(p, PROC_BIN_SIZE);
	    pb = (ProcBin *) hp;

	    pb->thing_word = HEADER_PROC_BIN;
	    pb->size = b->orig_size;
	    pb->next = MSO(p).mso;
	    MSO(p).mso = pb;
	    pb->val = ErlDrvBinary2Binary(b);
	    pb->bytes = b->orig_bytes;
	    MSO(p).overhead += b->orig_size / BINARY_OVERHEAD_FACTOR / sizeof(Eterm);
	    *resp = make_binary(pb);
	    return ret;
	}
    }
    if (n < 0) {
	ret = 0;
    } else {
	Eterm* hp = HAlloc(p, 2*n);
	*resp = buf_to_intlist(&hp, port_resp, n, NIL);
    }
    if (port_resp != port_result)
	driver_free(port_resp);
    return ret;
}

void
print_port_info(int i, CIO fp)
{
    Port* p = &erts_port[i];
    ErlLink* lnk;
    int k = 0;

    if (p->status == FREE)
	return;
    erl_printf(fp, "=port:");
    display(p->id, fp);
    erl_printf(fp, "\n");
    erl_printf(fp, "Slot: %d\n", i);
    if (p->status & CONNECTED) {
	erl_printf(fp,"Connected: ");
	display(p->connected, fp);
	erl_printf(fp,"\n");
    }
    lnk = p->links;
    while(lnk != NULL) {
	if (is_pid(lnk->item)) 
	    k++;
	lnk = lnk->next;
    }
    if (k > 0) {
	erl_printf(fp,"Links: ");
	lnk = p->links;
	while(lnk != NULL) {
	    if (is_pid(lnk->item))
		display(lnk->item, fp);
	    lnk = lnk->next;
	}
	erl_printf(fp,"\n");
    }

    if (p->reg != NULL) {
	erl_printf(fp,"Registered as: ");
	print_atom(atom_val(p->reg->name), fp);
	erl_printf(fp,"\n");
    }

    if (p->drv_ptr == &fd_driver_entry) {
	erl_printf(fp,"Port is UNIX fd not opened by emulator: %s\n", p->name);
    } else if (p->drv_ptr == &vanilla_driver_entry) {
	erl_printf(fp,"Port is a file: %s\n",p->name);
    } else if (p->drv_ptr == &spawn_driver_entry) {
	erl_printf(fp,"Port controls external process: %s\n",p->name);
    } else {
	erl_printf(fp,"Port controls linked-in driver: %s\n",p->name);
    }
}

void
set_busy_port(ErlDrvPort port_num, int on)
{
    Process* proc;

    if (on) {
        erts_port[port_num].status |= PORT_BUSY;
    } else {
        ProcessList* p = erts_port[port_num].suspended;
        erts_port[port_num].status &= ~PORT_BUSY;
        erts_port[port_num].suspended = NULL;


	/*
	 * Resume, in a round-robin fashion, all processes waiting on the port.
	 * 
	 * This version submitted by Tony Rogvall. The earlier version used
	 * to resume the processes in order, which caused starvation of all but
	 * the first process.
	 */

        if (p != NULL) {
            Eterm pid0;
            ProcessList* pl = p->next;

            pid0 = p->pid;	/* Get first pid (should be scheduled last) */
            erts_free(ERTS_ALC_T_PROC_LIST, (void *) p);

	    ASSERT(is_internal_pid(pid0));
            p = pl;
            while (p != NULL) {
                Eterm pid = p->pid;
                pl = p->next;
		ASSERT(is_internal_pid(pid));
		if (internal_pid_index(pid) < erts_max_processes) {
		    proc = process_tab[internal_pid_index(pid)];
		    if (!INVALID_PID(proc, pid)) {
			erl_resume(proc);
		    }
		}
                erts_free(ERTS_ALC_T_PROC_LIST, (void *) p);
                p = pl;
            }
	    
	    if (internal_pid_index(pid0) < erts_max_processes) {
		proc = process_tab[internal_pid_index(pid0)];
		if (!INVALID_PID(proc, pid0))
		    erl_resume(proc);
	    }
        }
    }
}

void set_port_control_flags(ErlDrvPort port_num, int flags)
{
    erts_port[port_num].control_flags = flags;
}

int get_port_flags(ErlDrvPort ix) { 
    Port* prt = NUM2PORT(ix);

    if (prt == NULL)
	return 0;

    return (prt->status & BINARY_IO ? PORT_FLAG_BINARY : 0) 
	| (prt->status & LINEBUF_IO ? PORT_FLAG_LINE : 0);
}


void dist_port_command(p, buf, len)
Port* p; byte* buf; int len;
{
    p->caller = NIL;
    if (p->drv_ptr->output != NULL) {
	(*p->drv_ptr->output)((ErlDrvData)p->drv_data, buf, len);
    }
}


void input_ready(ix, hndl)
int ix; 
int hndl;
{
    Port* p = &erts_port[ix];

    if (p->status != FREE) {
	(*p->drv_ptr->ready_input)((ErlDrvData)p->drv_data,(ErlDrvEvent)hndl);
	/* NOTE some windows drivers use input_ready for input and output */
	if ((p->status & CLOSING) && (p->ioq.size == 0)) {
	    terminate_port(ix);
	}
    } else {
	/* The driver has gone away without de-selecting! */
	cerr_pos = 0;
	erl_printf(CBUF, "#Port<0.%d>: %s: "
		   "Input driver gone away without deselecting!\n", 
		   ix, p->name ? p->name : "(unknown)" );
	erts_send_warning_to_logger(NIL, tmp_buf, cerr_pos);
	driver_select((ErlDrvPort)ix, (ErlDrvEvent)hndl, DO_READ, 0);
    }
}

void output_ready(ix, hndl)
int ix;
int hndl;
{
    Port* p = &erts_port[ix];

    if (p->status != FREE) {
	(*p->drv_ptr->ready_output)((ErlDrvData)p->drv_data, (ErlDrvEvent)hndl);
	if ((p->status & CLOSING) && (p->ioq.size == 0)) {
	    terminate_port(ix);
	}
    } else {
	cerr_pos = 0;
	erl_printf(CBUF, "#Port<0.%d>: %s: "
		   "Output driver gone away without deselecting!\n", 
		   ix, p->name ? p->name : "(unknown)" );
	erts_send_warning_to_logger(NIL, tmp_buf, cerr_pos);
	driver_select((ErlDrvPort)ix, (ErlDrvEvent)hndl, DO_WRITE, 0);
    }
}

int async_ready(int ix, void* data)
{
    Port* p = &erts_port[ix];
    int need_free = 1;

    if (p->status != FREE) {
	if (p->drv_ptr->ready_async != NULL) {
	    (*p->drv_ptr->ready_async)((ErlDrvData)p->drv_data, data);
	    need_free = 0;
	}
	if ((p->status & CLOSING) && (p->ioq.size == 0)) {
	    terminate_port(ix);
	}
    }
    return need_free;
}

void event_ready(int ix, int hndl, ErlDrvEventData event_data) {
    Port* p = &erts_port[ix];

    if (p->status == FREE) {
	cerr_pos = 0;
	erl_printf(CBUF, "#Port<0.%d>: %s: "
		   "Event driver gone away without deselecting!\n", 
		   ix, p->name ? p->name : "(unknown)" );
	erts_send_warning_to_logger(NIL, tmp_buf, cerr_pos);
	driver_event((ErlDrvPort)ix, (ErlDrvEvent)hndl, NULL);
    } else if (! p->drv_ptr->event) {
	cerr_pos = 0;
	erl_printf(CBUF, "#Port<0.%d>: %s: "
		   "Event driver does not implement ->event()!\n", 
		   ix, p->name ? p->name : "(unknown)" );
	send_error_to_logger(NIL);
	driver_event((ErlDrvPort)ix, (ErlDrvEvent)hndl, NULL);
    } else {
	(*p->drv_ptr->event)((ErlDrvData)p->drv_data, 
			     (ErlDrvEvent)hndl, 
			     event_data);
	if ((p->status & CLOSING) && (p->ioq.size == 0)) {
	    terminate_port(ix);
	}
    }
}


/* timer wrapper MUST check for closing */
static void port_timeout_proc(Port* p)
{
    ErlDrvEntry* drv = p->drv_ptr;

    if ((p->status == FREE) || (drv->timeout == NULL))
	return;
    (*drv->timeout)((ErlDrvData)p->drv_data);
    if ((p->status & CLOSING) && (p->ioq.size == 0)) {
	terminate_port(internal_port_index(p->id));
    }
}

ErlDrvTermData driver_mk_term_nil(void)
{
    return driver_term_nil;
}

void driver_report_exit(ix, status)
int ix;
int status;
{
   Port* prt = NUM2PORT(ix);
   Eterm* hp;
   Eterm tuple;
   Process *rp;
   Eterm pid;

   pid = prt->connected;
   ASSERT(is_internal_pid(pid));
   if (internal_pid_index(pid) >= erts_max_processes)
       return;
   rp = process_tab[internal_pid_index(pid)];
   if (INVALID_PID(rp, pid))
      return;

   hp = HAlloc(rp, 3+3);
   tuple = TUPLE2(hp, am_exit_status, make_small(status));
   hp += 3;
   tuple = TUPLE2(hp, prt->id, tuple);

   queue_message_tt(rp, NULL, tuple, am_undefined);
}



/*
 * Generate an Erlang term from data in an array (representing a simple stack
 * machine to build terms).
 * Returns:
 * 	-1 on error in input data
 *       0 if the message was not delivered (bad to pid or closed port)
 *       1 if the message was delivered successfully
 */

static int
driver_deliver_term(Port* prt, Eterm to, ErlDrvTermData* data, int len)
{
    int need = 0;
    int depth = 0;
    int max_depth = 0;
    Eterm* hp;
    ErlDrvTermData* ptr;
    ErlDrvTermData* ptr_end;
    DECLARE_ESTACK(stack); 
    Eterm mess = NIL;		/* keeps compiler happy */
    Process* rp;

    if (is_not_internal_pid(to)) /* e.g. dist_port_command set caller = NIL */
	return 0;
    if (prt->status & CLOSING)
	return 0;
    rp = internal_pid_index(to) < erts_max_processes ?
	process_tab[internal_pid_index(to)] : NULL;
    if (INVALID_PID(rp, to))
	return 0;

    /*
     * Check ErlDrvTermData for consistency and calculate needed heap size
     * and stack depth.
     */
    ptr = data;
    ptr_end = ptr + len;
    while (ptr < ptr_end) {
	ErlDrvTermData tag = *ptr++;
	switch(tag) {
	case ERL_DRV_NIL: /* no arguments */
	    depth++;
	    break;
	case ERL_DRV_ATOM: /* atom argument */
	    if ((ptr >= ptr_end) || (!is_atom(ptr[0]))) return -1;
	    ptr++;
	    depth++;
	    break;
	case ERL_DRV_INT:  /* signed int argument */
	    if (ptr >= ptr_end) return -1;
	    /* check for bignum */
	    if (!IS_SSMALL((int)ptr[0]))
		need += 2;  /* use small_to_big */
	    ptr++;
	    depth++;
	    break;
	case ERL_DRV_PORT:  /* port argument */
	    if ((ptr >= ptr_end) || (!is_port(ptr[0]))) return -1;
	    ptr++;
	    depth++;
	    break;
	case ERL_DRV_BINARY: /* ErlDrvBinary*, size, offs */
	    if ((ptr+1 >= ptr_end) || (ptr[0] == 0)) return -1;
	    need += PROC_BIN_SIZE;
	    ptr += 3;
	    depth++;
	    break;
	case ERL_DRV_STRING: /* char*, length */
	    if ((ptr+1 >= ptr_end) || (ptr[0] == 0)) return -1;
	    need += ptr[1] * 2;
	    ptr += 2;
	    depth++;
	    break;
	case ERL_DRV_STRING_CONS: /* char*, length */
	    if ((ptr+1 >= ptr_end) || (ptr[0] == 0)) return -1;
	    need += ptr[1] * 2;
	    if (depth > max_depth) max_depth = depth;
	    depth -= 1;
	    if (depth < 0) return -1;
	    ptr += 2;
	    depth++;
	    break;
	case ERL_DRV_LIST: /* int */
	    if ((ptr >= ptr_end) || (ptr[0] == 0)) return -1;
	    need += (ptr[0]-1)*2;  /* list cells */
	    if (depth > max_depth) max_depth = depth;
	    depth -= ptr[0];
	    if (depth < 0) return -1;
	    ptr++;
	    depth++;
	    break;
	case ERL_DRV_TUPLE: /* int */
	    if (ptr >= ptr_end) return -1;
	    need += ptr[0]+1;   /* vector positions + arityval */
	    if (depth > max_depth) max_depth = depth;
	    depth -= ptr[0];
	    if (depth < 0) return -1;
	    ptr++;
	    depth++;
	    break;
	default:
	    return -1;
	}
    }

    if ((depth != 1) || (ptr != ptr_end))
	return -1;

    /* Create message buffer */
    hp = HAlloc(rp, need);

    /* +1 needed for case basic type, then max_depth=0 is not set */
    /* FIXME: since we know the depth we could add a macro ESTACK2 where 
     * depth is known (max_depth+1)
     */

    /*
     * Interpret the instructions and build the term.
     */
    ptr = data;
    ptr_end = ptr + len;
    while (ptr < ptr_end) {
	ErlDrvTermData tag = *ptr++;

	switch(tag) {
	case ERL_DRV_NIL: /* no arguments */
	    mess = NIL;
	    break;

	case ERL_DRV_ATOM: /* atom argument */
	    mess = ptr[0];
	    ptr++;
	    break;

	case ERL_DRV_INT:  /* signed int argument */
	    if (IS_SSMALL((int)ptr[0]))
		mess = make_small((int)ptr[0]);
	    else {
		mess = small_to_big((int)ptr[0], hp);
		hp += 2;
	    }
	    ptr++;
	    break;

	case ERL_DRV_PORT:  /* port argument */
	    mess = ptr[0];
	    ptr++;
	    break;

	case ERL_DRV_BINARY: { /* ErlDrvBinary*, size, offs */
	    ProcBin* pb;
	    ErlDrvBinary* b = (ErlDrvBinary*) ptr[0];

	    b->refc++;  /* caller must free binary !!! */

	    pb = (ProcBin *) hp;
	    pb->thing_word = HEADER_PROC_BIN;
	    pb->size =  ptr[1];
	    pb->next = MSO(rp).mso;
	    MSO(rp).mso = pb;
	    pb->val = ErlDrvBinary2Binary(b);
	    pb->bytes = ((byte*) b->orig_bytes) + ptr[2];
	    mess =  make_binary(pb);
	    hp += PROC_BIN_SIZE;
	    MSO(rp).overhead += pb->size / 
		BINARY_OVERHEAD_FACTOR / sizeof(Eterm);
	    ptr += 3;
	    break;
	}

	case ERL_DRV_STRING: /* char*, length */
	    mess = buf_to_intlist(&hp, (char*)ptr[0], ptr[1], NIL);
	    ptr += 2;
	    break;

	case ERL_DRV_STRING_CONS:  /* char*, length */
	    mess = ESTACK_POP(stack);
	    mess = buf_to_intlist(&hp, (char*)ptr[0], ptr[1], mess);
	    ptr += 2;
	    break;

	case ERL_DRV_LIST: { /* int */
	    int i = (int) ptr[0];

	    mess = ESTACK_POP(stack);
	    i--;
	    while(i > 0) {
		Eterm hd = ESTACK_POP(stack);

		mess = CONS(hp, hd, mess);
		hp += 2;
		i--;
	    }
	    ptr++;
	    break;
	}

	case ERL_DRV_TUPLE: { /* int */
	    int size = (int)ptr[0];
	    Eterm* tp = hp;

	    *tp = make_arityval(size);
	    mess = make_tuple(tp);

	    tp += size;   /* point at last element */
	    hp = tp+1;    /* advance "heap" pointer */

	    while(size--) {
		*tp-- = ESTACK_POP(stack);
	    }
	    ptr++;
	    break;
	}
	}
	ESTACK_PUSH(stack, mess);
    }
    mess = ESTACK_POP(stack);  /* get resulting value */

    DESTROY_ESTACK(stack);

    queue_message_tt(rp, NULL, mess, am_undefined);  /* send message */
    return 1;
}


int 
driver_output_term(ErlDrvPort ix, ErlDrvTermData* data, int len)
{
    Port* prt = NUM2PORT(ix);
    if (prt == NULL) return -1;
    
    return driver_deliver_term(prt, prt->connected, data, len);
}


int
driver_send_term(ErlDrvPort ix, ErlDrvTermData to, ErlDrvTermData* data, int len)
{
    Port* prt = NUM2PORT(ix);

    if (prt == NULL) {
	return -1;
    }
    return driver_deliver_term(prt, to, data, len);
}


/*
 * Output a binary with hlen bytes from hbuf as list header
 * and data is len length of bin starting from offset offs.
 */

int driver_output_binary(ErlDrvPort ix, char* hbuf, int hlen,
			 ErlDrvBinary* bin, int offs, int len)
{
    Port* prt = NUM2PORT(ix);

    if (prt == NULL)
	return -1;
    if (prt->status & CLOSING)
	return 0;

    prt->bytes_in += (hlen + len);
    bytes_in += (hlen + len);
    if (prt->status & DISTRIBUTION) {
	return net_mess2(prt->dist_entry, (byte*)hbuf, hlen,
			 (byte*)(bin->orig_bytes+offs), len);
    }
    else
	deliver_bin_message(prt, prt->connected, 
			    hbuf, hlen, bin, offs, len);
    return 0;
}

/* driver_output2:
** Delivers hlen bytes from hbuf to the port owner as a list;
** after that, the port settings apply, buf is sent as binary or list.
**
** Example: if hlen = 3 then the port owner will receive the data
** [H1,H2,H3 | T]
*/
int driver_output2(ErlDrvPort ix, char* hbuf, int hlen, char* buf, int len)
{
    Port* prt = NUM2PORT(ix);

    if (prt == NULL)
	return -1;
    if (prt->status & CLOSING)
	return 0;
    
    prt->bytes_in += (hlen + len);
    bytes_in += (hlen + len);
    if (prt->status & DISTRIBUTION) {
	if (len == 0)
	    return net_mess2(prt->dist_entry, NULL, 0, (byte*)hbuf, hlen);
	else
	    return net_mess2(prt->dist_entry, (byte*)hbuf,hlen,(byte*)buf,len);
    }
    else if(prt->status & LINEBUF_IO)
	deliver_linebuf_message(prt, prt->connected, hbuf, hlen, buf, len);
    else
	deliver_read_message(prt, prt->connected, 
			      hbuf, hlen, buf, len, 0);
    return 0;
}

/* Interface functions available to driver writers */

int driver_output(ErlDrvPort ix, char* buf, int len)
{
    return driver_output2(ix, NULL, 0, buf, len);
}

int driver_outputv(ErlDrvPort ix, char* hbuf, int hlen, ErlIOVec* vec, int skip)
{
    int n;
    int len;
    int size;
    SysIOVec* iov;
    ErlDrvBinary** binv;
    Port* prt;

    size = vec->size - skip;   /* Size of remaining bytes in vector */
    ASSERT(size >= 0);
    if (size <= 0)
	return driver_output2(ix, hbuf, hlen, NULL, 0);
    ASSERT(hlen >= 0);       /* debug only */
    if (hlen < 0)
	hlen = 0;

    prt = NUM2PORT(ix);
    if (prt == NULL)
	return -1;
    if (prt->status & CLOSING)
	return 0;

    /* size > 0 ! */
    iov = vec->iov;
    binv = vec->binv;
    n = vec->vsize;
    /* we use do here to strip iov_len=0 from beginning */
    do {
	len = iov->iov_len;
	if (len <= skip) {
	    skip -= len;
	    iov++;
	    binv++;
	    n--;
	} else {
	    iov->iov_base += skip;
	    iov->iov_len -= skip;
	    skip = 0;
	}
    } while (skip > 0);

    /* XXX handle distribution !!! */
    prt->bytes_in += (hlen + size);
    bytes_in += (hlen + size);
    deliver_vec_message(prt, prt->connected, hbuf, hlen, binv, iov, n, size);
    return 0;
}

/* Copy bytes from a vector into a buffer
** input is a vector a buffer and a max length
** return bytes copied
*/
int driver_vec_to_buf(vec, buf, len)
ErlIOVec* vec; 
char* buf;
int len;
{
    SysIOVec* iov = vec->iov;
    int n = vec->vsize;
    int orig_len = len;

    while(n--) {
	int ilen = iov->iov_len;
	if (ilen < len) {
	    sys_memcpy(buf, iov->iov_base, ilen);
	    len -= ilen;
	    buf += ilen;
	    iov++;
	}
	else {
	    sys_memcpy(buf, iov->iov_base, len);
	    return orig_len;
	}
    }
    return (orig_len - len);
}

/*
** Allocation/Deallocation of binary objects 
*/

ErlDrvBinary*
driver_alloc_binary(int size)
{
    Binary* bin;

    if (size < 0) {
	cerr_pos = 0;
	erl_printf(CBUF,
		   "Bad use of driver_alloc_binary(%d): called with "
		   "with negative size as argument",
		   size);
	send_error_to_logger(NIL);
	size = 0;
    }

    bin = erts_bin_drv_alloc_fnf(size);
    if (!bin)
	return NULL; /* The driver write must take action */
    bin->flags = BIN_FLAG_DRV;
    bin->refc = 1;
    bin->orig_size = size;
    return Binary2ErlDrvBinary(bin);
}

/* Reallocate space hold by binary */

ErlDrvBinary* driver_realloc_binary(bin, size)
ErlDrvBinary* bin; int size;
{
    Binary* oldbin;
    Binary* newbin;

    if (!bin || size < 0) {
	cerr_pos = 0;
	erl_printf(CBUF,
		   "Bad use of driver_realloc_binary(%p, %d): "
		   "called with ",
		   bin, size);
	if (!bin) {
	    erl_printf(CBUF, "NULL pointer as first argument");
	    if (size < 0)
		erl_printf(CBUF, ", and ");
	}
	if (size < 0) {
	    erl_printf(CBUF, "negative size as second argument");
	    size = 0;
	}
	send_error_to_logger(NIL);
	if (!bin)
	    return driver_alloc_binary(size);
    }

    oldbin = ErlDrvBinary2Binary(bin);
    newbin = (Binary *) erts_bin_realloc_fnf(oldbin, size);
    if (!newbin)
	return NULL;

    newbin->orig_size = size;
    return Binary2ErlDrvBinary(newbin);
}


void driver_free_binary(dbin)
ErlDrvBinary* dbin;
{
    Binary *bin;
    if (!dbin) {
	cerr_pos = 0;
	erl_printf(CBUF,
		   "Bad use of driver_free_binary(%p): called with "
		   "NULL pointer as argument", dbin);
	send_error_to_logger(NIL);
	return;
    }

    bin = ErlDrvBinary2Binary(dbin);
    if (bin->refc == 1) {
	if (bin->flags & BIN_FLAG_MATCH_PROG) {
	    erts_match_set_free(bin);
	} else {
	    erts_bin_free(bin);
	}
    } else {
	bin->refc--;
    }
}


/* 
 * Allocation/deallocation of memory for drivers 
 */

void *driver_alloc(size_t size)
{
    return erts_alloc_fnf(ERTS_ALC_T_DRV, (Uint) size);
}

void *driver_realloc(void *ptr, size_t size)
{
    return erts_realloc_fnf(ERTS_ALC_T_DRV, ptr, (Uint) size);
}

void driver_free(void *ptr)
{
    erts_free(ERTS_ALC_T_DRV, ptr);
}


/* expand queue to hold n elements in tail or head */
static int expandq(ErlIOQueue* q, int n, int tail)
/* tail: 0 if make room in head, make room in tail otherwise */
{
    int h_sz;  /* room before header */
    int t_sz;  /* room after tail */
    int q_sz;  /* occupied */
    int nvsz;
    SysIOVec* niov;
    ErlDrvBinary** nbinv;

    h_sz = q->v_head - q->v_start;
    t_sz = q->v_end -  q->v_tail;
    q_sz = q->v_tail - q->v_head;

    if (tail && (n <= t_sz)) /* do we need to expand tail? */
	return 0;
    else if (!tail && (n <= h_sz))  /* do we need to expand head? */
	return 0;
    else if (n > (h_sz + t_sz)) { /* need to allocate */
	/* we may get little extra but it ok */
	nvsz = (q->v_end - q->v_start) + n; 

	niov = erts_alloc_fnf(ERTS_ALC_T_IOQ, nvsz * sizeof(SysIOVec));
	if (!niov)
	    return -1;
	nbinv = erts_alloc_fnf(ERTS_ALC_T_IOQ, nvsz * sizeof(ErlDrvBinary**));
	if (!nbinv) {
	    erts_free(ERTS_ALC_T_IOQ, (void *) niov);
	    return -1;
	}
	if (tail) {
	    sys_memcpy(niov, q->v_head, q_sz*sizeof(SysIOVec));
	    if (q->v_start != q->v_small)
		erts_free(ERTS_ALC_T_IOQ, (void *) q->v_start);
	    q->v_start = niov;
	    q->v_end = niov + nvsz;
	    q->v_head = q->v_start;
	    q->v_tail = q->v_head + q_sz;

	    sys_memcpy(nbinv, q->b_head, q_sz*sizeof(ErlDrvBinary*));
	    if (q->b_start != q->b_small)
		erts_free(ERTS_ALC_T_IOQ, (void *) q->b_start);
	    q->b_start = nbinv;
	    q->b_end = nbinv + nvsz;
	    q->b_head = q->b_start;
	    q->b_tail = q->b_head + q_sz;	
	}
	else {
	    sys_memcpy(niov+nvsz-q_sz, q->v_head, q_sz*sizeof(SysIOVec));
	    if (q->v_start != q->v_small)
		erts_free(ERTS_ALC_T_IOQ, (void *) q->v_start);
	    q->v_start = niov;
	    q->v_end = niov + nvsz;
	    q->v_tail = q->v_end;
	    q->v_head = q->v_tail - q_sz;
	    
	    sys_memcpy(nbinv+nvsz-q_sz, q->b_head, q_sz*sizeof(ErlDrvBinary*));
	    if (q->b_start != q->b_small)
		erts_free(ERTS_ALC_T_IOQ, (void *) q->b_start);
	    q->b_start = nbinv;
	    q->b_end = nbinv + nvsz;
	    q->b_tail = q->b_end;
	    q->b_head = q->b_tail - q_sz;
	}
    }
    else if (tail) {  /* move to beginning to make room in tail */
	sys_memmove(q->v_start, q->v_head, q_sz*sizeof(SysIOVec));
	q->v_head = q->v_start;
	q->v_tail = q->v_head + q_sz;
	sys_memmove(q->b_start, q->b_head, q_sz*sizeof(ErlDrvBinary*));
	q->b_head = q->b_start;
	q->b_tail = q->b_head + q_sz;
    }
    else {   /* move to end to make room */
	sys_memmove(q->v_end-q_sz, q->v_head, q_sz*sizeof(SysIOVec));
	q->v_tail = q->v_end;
	q->v_head = q->v_tail-q_sz;
	sys_memmove(q->b_end-q_sz, q->b_head, q_sz*sizeof(ErlDrvBinary*));
	q->b_tail = q->b_end;
	q->b_head = q->b_tail-q_sz;
    }

    return 0;
}



/* Put elements from vec at q tail */
int driver_enqv(ErlDrvPort ix, ErlIOVec* vec, int skip)
{
    int n;
    int len;
    int size;
    SysIOVec* iov;
    ErlDrvBinary** binv;
    ErlDrvBinary*  b;
    ErlIOQueue* q = IOQ(ix);

    if (q == NULL)
	return -1;

    size = vec->size - skip;
    ASSERT(size >= 0);       /* debug only */
    if (size <= 0)
	return 0;

    iov = vec->iov;
    binv = vec->binv;
    n = vec->vsize;

    /* we use do here to strip iov_len=0 from beginning */
    do {
	len = iov->iov_len;
	if (len <= skip) {
	    skip -= len;
	    iov++;
	    binv++;
	    n--;
	}
	else {
	    iov->iov_base += skip;
	    iov->iov_len -= skip;
	    skip = 0;
	}
    } while(skip > 0);

    if (q->v_tail + n >= q->v_end)
	expandq(q, n, 1);

    /* Queue and reference all binaries (remove zero length items) */
    while(n--) {
	if ((len = iov->iov_len) > 0) {
	    if ((b = *binv) == NULL) { /* speical case create binary ! */
		b = driver_alloc_binary(len);
		sys_memcpy(b->orig_bytes, iov->iov_base, len);
		*q->b_tail++ = b;
		q->v_tail->iov_len = len;
		q->v_tail->iov_base = b->orig_bytes;
		q->v_tail++;
	    }
	    else {
		b->refc++;
		*q->b_tail++ = b;
		*q->v_tail++ = *iov;
	    }
	}
	iov++;
	binv++;
    }
    q->size += size;      /* update total size in queue */
    return 0;
}

/* Put elements from vec at q head */
int driver_pushqv(ErlDrvPort ix, ErlIOVec* vec, int skip)
{
    int n;
    int len;
    int size;
    SysIOVec* iov;
    ErlDrvBinary** binv;
    ErlDrvBinary* b;
    ErlIOQueue* q = IOQ(ix);

    if (q == NULL)
	return -1;

    if ((size = vec->size - skip) <= 0)
	return 0;
    iov = vec->iov;
    binv = vec->binv;
    n = vec->vsize;

    /* we use do here to strip iov_len=0 from beginning */
    do {
	len = iov->iov_len;
	if (len <= skip) {
	    skip -= len;
	    iov++;
	    binv++;
	    n--;
	}
	else {
	    iov->iov_base += skip;
	    iov->iov_len -= skip;
	    skip = 0;
	}
    } while(skip > 0);

    if (q->v_head - n < q->v_start)
	expandq(q, n, 0);

    /* Queue and reference all binaries (remove zero length items) */
    iov += (n-1);  /* move to end */
    binv += (n-1); /* move to end */
    while(n--) {
	if ((len = iov->iov_len) > 0) {
	    if ((b = *binv) == NULL) { /* speical case create binary ! */
		b = driver_alloc_binary(len);
		sys_memcpy(b->orig_bytes, iov->iov_base, len);
		*--q->b_head = b;
		q->v_head--;
		q->v_head->iov_len = len;
		q->v_head->iov_base = b->orig_bytes;
	    }
	    else {
		b->refc++;
		*--q->b_head = b;
		*--q->v_head = *iov;
	    }
	}
	iov--;
	binv--;
    }
    q->size += size;      /* update total size in queue */
    return 0;
}


/*
** Remove size bytes from queue head
** Return number of bytes that remain in queue
*/
int driver_deq(ErlDrvPort ix, int size)
{
    ErlIOQueue* q = IOQ(ix);
    int len;
    int sz;

    if ((q == NULL) || (sz = (q->size - size)) < 0)
	return -1;
    q->size = sz;
    while (size > 0) {
	ASSERT(q->v_head != q->v_tail);

	len = q->v_head->iov_len;
	if (len <= size) {
	    size -= len;
	    driver_free_binary(*q->b_head);
	    *q->b_head++ = NULL;
	    q->v_head++;
	}
	else {
	    q->v_head->iov_base += size;
	    q->v_head->iov_len -= size;
	    size = 0;
	}
    }

    /* restart pointers (optimised for enq) */
    if (q->v_head == q->v_tail) {
	q->v_head = q->v_tail = q->v_start;
	q->b_head = q->b_tail = q->b_start;
    }
    return sz;
}


int driver_peekqv(ErlDrvPort ix, ErlIOVec *ev) {
    ErlIOQueue *q = IOQ(ix);
    ASSERT(ev);

    if (! q) {
	return -1;
    } else {
	if ((ev->vsize = q->v_tail - q->v_head) == 0) {
	    ev->size = 0;
	    ev->iov = NULL;
	    ev->binv = NULL;
	} else {
	    ev->size = q->size;
	    ev->iov = q->v_head;
	    ev->binv = q->b_head;
	}
	return q->size;
    }
}

SysIOVec* driver_peekq(ErlDrvPort ix, int* vlenp)  /* length of io-vector */
{
    ErlIOQueue* q = IOQ(ix);

    if (q == NULL)
	return NULL;
    if ((*vlenp = (q->v_tail - q->v_head)) == 0)
	return NULL;
    return q->v_head;
}


int driver_sizeq(ErlDrvPort ix)
{
    ErlIOQueue* q = IOQ(ix);

    if (q == NULL)
	return -1;
    return q->size;
}


/* Utils */

/* Enqueue a binary */
int driver_enq_bin(ErlDrvPort ix, ErlDrvBinary* bin, int offs, int len)
{
    SysIOVec      iov;
    ErlIOVec      ev;

    ASSERT(len >= 0);
    if (len == 0)
	return 0;
    iov.iov_base = bin->orig_bytes + offs;
    iov.iov_len  = len;
    ev.vsize = 1;
    ev.size = len;
    ev.iov = &iov;
    ev.binv = &bin;
    return driver_enqv(ix, &ev, 0);
}

int driver_enq(ErlDrvPort ix, char* buffer, int len)
{
    int code;
    ErlDrvBinary* bin;

    ASSERT(len >= 0);
    if (len == 0)
	return 0;
    if ((bin = driver_alloc_binary(len)) == NULL)
	return -1;
    sys_memcpy(bin->orig_bytes, buffer, len);
    code = driver_enq_bin(ix, bin, 0, len);
    driver_free_binary(bin);  /* dereference */
    return code;
}

int driver_pushq_bin(ErlDrvPort ix, ErlDrvBinary* bin, int offs, int len)
{
    SysIOVec      iov;
    ErlIOVec      ev;

    ASSERT(len >= 0);
    if (len == 0)
	return 0;
    iov.iov_base = bin->orig_bytes + offs;
    iov.iov_len  = len;
    ev.vsize = 1;
    ev.size = len;
    ev.iov = &iov;
    ev.binv = &bin;
    return driver_pushqv(ix, &ev, 0);
}

int driver_pushq(ErlDrvPort ix, char* buffer, int len)
{
    int code;
    ErlDrvBinary* bin;

    ASSERT(len >= 0);
    if (len == 0)
	return 0;

    if ((bin = driver_alloc_binary(len)) == NULL)
	return -1;
    sys_memcpy(bin->orig_bytes, buffer, len);
    code = driver_pushq_bin(ix, bin, 0, len);
    driver_free_binary(bin);  /* dereference */
    return code;
}

int driver_set_timer(ErlDrvPort ix, Uint t)
{
    Port* prt = NUM2PORT(ix);

    if (prt == NULL)
	return -1;
    if (prt->drv_ptr->timeout == NULL)
	return -1;
    erl_set_timer(&prt->tm, (ErlTimeoutProc) port_timeout_proc, NULL, prt, t);
    return 0;
}

int driver_cancel_timer(ErlDrvPort ix)
{
    Port* prt = NUM2PORT(ix);

    if (prt == NULL) {
	return -1;
    }
    erl_cancel_timer(&prt->tm);
    return 0;
}


int
driver_read_timer(ErlDrvPort ix, unsigned long* t)
{
    Port* prt = NUM2PORT(ix);

    if (prt == NULL) {
	return -1;
    }
    *t = time_left(&prt->tm);
    return 0;
}

static int
driver_failure_term(ErlDrvPort ix, Eterm term, int eof)
{
    Port* prt = NUM2PORT(ix);

    if (prt == NULL)
	return -1;
    if (eof)
	flush_linebuf_messages(prt);
    if (prt->status & CLOSING) {
	terminate_port(ix);
    } else if (eof && (prt->status & SOFT_EOF)) {
	deliver_result(erts_port[ix].id, prt->connected, am_eof);
    } else {
	/* XXX UGLY WORK AROUND, Let do_exit_port terminate the port */
	prt->ioq.size = 0;
	do_exit_port(erts_port[ix].id, erts_port[ix].id,
		     eof ? am_normal : term);
    }
    return 0;
}



/*
** Do a (soft) exit. unlink the connected process before doing
** driver posix error or (normal)
*/
int driver_exit(ErlDrvPort ix, int err)
{
    Port* prt = NUM2PORT(ix);
    Process* rp;
  
    if (prt == NULL)
        return -1;

    /* unlink connected */
    del_link(find_link(&prt->links, LNK_LINK, prt->connected, NIL));

    /* unlink port from connected */
    if ((rp = pid2proc(prt->connected)) != NULL)
        del_link(find_link(&rp->links, LNK_LINK, prt->id, NIL));

    if (err == 0)
        return driver_failure_term(ix, am_normal, 0);
    else {
        char* err_str = erl_errno_id(err);
        Eterm am_err = am_atom_put(err_str, sys_strlen(err_str));
        return driver_failure_term(ix, am_err, 0);
    }
}


int driver_failure(ErlDrvPort ix, int code)
{
    return driver_failure_term(ix, make_small(code), code == 0);
}

int driver_failure_atom(ErlDrvPort ix, char* string)
{
    Eterm am = am_atom_put(string, strlen(string));

    return driver_failure_term(ix, am, 0);
}

int driver_failure_posix(ErlDrvPort ix, int err)
{
    return driver_failure_atom(ix, erl_errno_id(err));
}

int driver_failure_eof(ErlDrvPort ix)
{
    return driver_failure_term(ix, NIL, 1);
}



ErlDrvTermData driver_mk_atom(char* string)
{
    Eterm am = am_atom_put(string, sys_strlen(string));
    
    return (ErlDrvTermData) am;
}

ErlDrvTermData driver_mk_port(ErlDrvPort ix)
{
    Port* prt = NUM2PORT(ix);

    return (ErlDrvTermData) prt->id;
}

ErlDrvTermData driver_connected(ErlDrvPort ix)
{
    Port* prt = NUM2PORT(ix);

    if (prt == NULL)
	return NIL;
    return prt->connected;
}

ErlDrvTermData driver_caller(ErlDrvPort ix)
{
    Port* prt = NUM2PORT(ix);

    if (prt == NULL)
	return NIL;
    return prt->caller;
}


/*
** Make a HARD link to the driver which makes
** the erl_ddll to suspend unloading

* Not sure this is safe! ?J

*/

int driver_attach(ErlDrvPort ix)
{
    Port* prt = NUM2PORT(ix);
    DE_Handle* dh;
    if (prt == NULL) return -1;

    if ((dh = (DE_Handle*)prt->drv_ptr->handle ) == NULL)
	return -1;
    dh->ref_count++;
    return 0;
}

/* Unlink from a driver */
int driver_detach(ErlDrvPort ix)
{
    Port* prt = NUM2PORT(ix);
    DE_Handle* dh;
    if (prt == NULL) return -1;

    if ((dh = (DE_Handle*)prt->drv_ptr->handle ) == NULL)
	return -1;
    dh->ref_count--;
    DEBUGF(("io: driver_detach: ref_count=%d\r\n", dh->ref_count));
    if (dh->ref_count == 0) {
	if (dh->cb != NULL) {
	    return (*dh->cb)(dh->ca[0], dh->ca[1], dh->ca[2], dh->ca[3]);
	}
    }
    return 0;
}



/* 
 * Functions for maintaining a list of driver_entry struct
 */

void add_driver_entry(drv)
    ErlDrvEntry *drv;
{
    DE_List *p = erts_alloc(ERTS_ALC_T_DRV_ENTRY_LIST, sizeof(DE_List));

    p->drv = drv;
    p->next = driver_list;
    p->de_hndl = (DE_Handle*) drv->handle;
    driver_list = p;
    if (drv->init != NULL) {
	(*drv->init)();
    }
}

int remove_driver_entry(drv)
    ErlDrvEntry *drv;
{
    DE_List **p = &driver_list;
    DE_List *q;

    while (*p != NULL) {
	if ((*p)->drv == drv) {
	    q = *p;
	    *p = (*p)->next;
	    erts_free(ERTS_ALC_T_DRV_ENTRY_LIST, (void *) q);
	    return 1;
	}
	p = &(*p)->next;
    }
    return 0;
}

/* very useful function that can be used in entries that are not used
 * so that not every driver writer must supply a personal version
 */
int null_func(void)
{
    return 0;
}

/* Shared / dynamic link libraries libraries */
/* XXX These should go in a separated module */

void *driver_dl_open(char * path)
{
    return ddll_open(path);
}

void *driver_dl_sym(void * handle, char *func_name)
{
    return (void *)ddll_sym(handle, func_name);
}
    
int driver_dl_close(void *handle)
{
    return ddll_close(handle);
}

char *driver_dl_error(void) 
{
    return ddll_error();
}
