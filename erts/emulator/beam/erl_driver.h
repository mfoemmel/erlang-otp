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
 * Include file for erlang driver writers.
 */

#ifndef __ERL_DRIVER_H__
#define __ERL_DRIVER_H__

#if defined(VXWORKS)
#  include <ioLib.h>
typedef struct iovec SysIOVec;
#elif defined(__WIN32__)
/*
 * This structure can be cast to a WSABUF structure.
 */
typedef struct _SysIOVec {
    unsigned long iov_len;
    char* iov_base;
} SysIOVec;

#else  /* Unix */
#  ifdef HAVE_UIO_H
#  include <sys/uio.h>

typedef struct iovec SysIOVec;

#  else

typedef struct {
    char* iov_base;
    int   iov_len;
} SysIOVec;

#  endif
#endif

#ifdef __cplusplus
#   define EXTERN extern "C"
#else
#   define EXTERN extern
#endif

/* Values for mode arg to driver_select() */

#define DO_READ	 (1 << 0)
#define DO_WRITE (1 << 1)

#define PORT_CONTROL_FLAG_BINARY	1
#define PORT_CONTROL_FLAG_HEAVY		2

/*
 * A binary as seen in a driver. Note that a binary should never be
 * altered by the driver when it has been sent to Erlang.
 */

typedef struct erl_drv_binary {
    int orig_size;        /* total length of binary */
    int refc;             /* number of references to this binary */
    char orig_bytes[1];   /* the data (char instead of byte!) */
} ErlDrvBinary;


/*
 * Note: These types are incomplete to catch type errors easier.
 */

typedef struct _erl_drv_data* ErlDrvData; /* Data to be used by the driver itself. */
typedef struct _erl_drv_event* ErlDrvEvent; /* An event to be selected on. */
typedef struct _erl_drv_port* ErlDrvPort; /* A port descriptor. */
typedef struct _erl_drv_port* ErlDrvThreadData; /* Thread data. */

/*
 * Error codes that can be return from driver.
 */

/*
 * Exception code from open_port/2 will be {'EXIT',{einval,Where}}.
 */
#define ERL_DRV_ERROR_GENERAL ((ErlDrvData) -1)

/*
 * Exception code from open_port/2 will be {'EXIT',{Errno,Where}},
 * where Errno is a textual representation of the errno variable
 * (e.g. eacces if errno is EACCES).
 */
#define ERL_DRV_ERROR_ERRNO ((ErlDrvData) -2)

/*
 * Exception code from open_port/2 will be {'EXIT',{badarg,Where}}.
 */
#define ERL_DRV_ERROR_BADARG ((ErlDrvData) -3)

typedef struct erl_io_vec {
    int vsize;			/* length of vectors */
    int size;			/* total size in bytes */
    SysIOVec* iov;
    ErlDrvBinary** binv;
} ErlIOVec;

/*
 * This structure defines a driver.
 */

typedef struct erl_drv_entry {
    int (*init)(void);		/* called at system start up for statically
				   linked drivers, and after loading for
				   dynamically loaded drivers */ 
    ErlDrvData (*start)(ErlDrvPort port, char *command);
				/* called when open_port/2 is invoked.
				   return value -1 means failure. */
    void (*stop)(ErlDrvData drv_data);
                                /* called when port is closed, and when the
				   emulator is halted. */
    void (*output)(ErlDrvData drv_data, char *buf, int len);
				/* called when we have output from erlang to 
				   the port */
    void (*ready_input)(ErlDrvData drv_data, ErlDrvEvent event); 
				/* called when we have input from one of 
				   the driver's handles) */
    void (*ready_output)(ErlDrvData drv_data, ErlDrvEvent event);  
				/* called when output is possible to one of 
				   the driver's handles */
    char *driver_name;		/* name supplied as command 
				   in open_port XXX ? */
    void (*finish)(void);        /* called before unloading the driver -
				   DYNAMIC DRIVERS ONLY */
    void *handle;		/* not used -- here for backwards compatibility */
    int (*control)(ErlDrvData drv_data, unsigned int command, char *buf, 
		   int len, char **rbuf, int rlen); 
				/* "ioctl" for drivers - invoked by 
				   port_command/3) */
    void (*timeout)(ErlDrvData drv_data);	/* Handling of timeout in driver */
    void (*outputv)(ErlDrvData drv_data, ErlIOVec *ev);
				/* called when we have output from erlang
				   to the port */
    void (*ready_async)(ErlDrvData drv_data, ErlDrvThreadData thred_data);
} ErlDrvEntry;

/*
 * This macro is used to name a dynamic driver's init function in
 * a way that doesn't lead to conflicts. This is crucial when using
 * operating systems that has one namespace for all symbols
 * (e.g. VxWorks). Example: if you have an dynamic driver C source
 * file named echo_drv.c, you use the macro like this:
 * 
 *    DRIVER_INIT(echo_drv)
 *    {
 *	 ....
 *    }
 *
 * This function well be called by the Erlang I/O system when the driver is loaded.
 * It must initialize a ErlDrvEntry structure and return a pointer to it.
 */

#if defined(VXWORKS)
#    define DRIVER_INIT(DRIVER_NAME) ErlDrvEntry* DRIVER_NAME  ## _init(void)
#elif defined(__WIN32__)
#    define DRIVER_INIT(DRIVER_NAME) __declspec(dllexport) ErlDrvEntry* driver_init(void)
#else 
#    define DRIVER_INIT(DRIVER_NAME) ErlDrvEntry* driver_init(void)
#endif

/*
 * These are the functions available for driver writers.
 */

EXTERN int driver_select(ErlDrvPort port, ErlDrvEvent event, int mode, int on);
EXTERN int driver_output(ErlDrvPort port, char *buf, int len);
EXTERN int driver_output2(ErlDrvPort port, char *hbuf, int hlen, 
			  char *buf, int len);
EXTERN int driver_output_binary(ErlDrvPort port, char *hbuf, int hlen,
				ErlDrvBinary* bin, int offset, int len);
EXTERN int driver_outputv(ErlDrvPort port, char* hbuf, int hlen, ErlIOVec *ev,
			  int skip);
EXTERN int driver_vec_to_buf(ErlIOVec *ev, char *buf, int len);
EXTERN int driver_set_timer(ErlDrvPort port, unsigned long time);
EXTERN int driver_cancel_timer(ErlDrvPort port);
EXTERN int driver_read_timer(ErlDrvPort port, unsigned long *time_left);

/*
 * The following functions are used to initiate a close of a port
 * from a driver.
 */
EXTERN int driver_failure_eof(ErlDrvPort port);
EXTERN int driver_failure_atom(ErlDrvPort port, char *string);
EXTERN int driver_failure_posix(ErlDrvPort port, int error);
EXTERN int driver_failure(ErlDrvPort port, int error);


EXTERN char* erl_errno_id(int error);
EXTERN void set_busy_port(ErlDrvPort port, int on);
EXTERN void set_port_control_flags(ErlDrvPort port, int flags);

/* Binary interface */

/*
 * NOTE: DO NOT overwrite a binary with new data (if the data is delivered);
 * since the binary is a shared object it MUST be written once.
 */

EXTERN ErlDrvBinary* driver_alloc_binary(int size);
EXTERN ErlDrvBinary* driver_realloc_binary(ErlDrvBinary *bin, int size);
EXTERN void driver_free_binary(ErlDrvBinary *bin);

/* Allocation interface */
EXTERN void *driver_alloc(size_t size);
EXTERN void *driver_realloc(void *ptr, size_t size);
EXTERN void driver_free(void *ptr);

/* Queue interface */
EXTERN int driver_enq(ErlDrvPort port, char* buf, int len);
EXTERN int driver_pushq(ErlDrvPort port, char* buf, int len);
EXTERN int driver_deq(ErlDrvPort port, int size);
EXTERN int driver_sizeq(ErlDrvPort port);
EXTERN int driver_enq_bin(ErlDrvPort port, ErlDrvBinary *bin, int offset, 
			  int len);
EXTERN int driver_pushq_bin(ErlDrvPort port, ErlDrvBinary *bin, int offset,
			    int len);

EXTERN SysIOVec* driver_peekq(ErlDrvPort port, int *vlen);
EXTERN int driver_enqv(ErlDrvPort port, ErlIOVec *ev, int skip);
EXTERN int driver_pushqv(ErlDrvPort port, ErlIOVec *ev, int skip);

/*
 * Add and remove driver entries.
 */
EXTERN void add_driver_entry(ErlDrvEntry *de);
EXTERN int remove_driver_entry(ErlDrvEntry *de);

#endif
