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
 * Purpose: Send trace messages to a file.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef __WIN32__
#include <io.h>
#define write _write
#define open _open
#define close _close
#else
#include <unistd.h>
#endif
#include <errno.h>
#include <sys/types.h>
#include <fcntl.h>
#ifdef VXWORKS
#include "reclaim.h"
#endif

#ifdef DEBUG
#ifndef __WIN32__
#define ASSERT(X) do {if (!(X)) {erl_exit(1,"%s",#X);} } while(0)
#else
#include <assert.h>
#define ASSERT(X) assert(X)
#endif
#else
#define ASSERT(X)
#endif

#include "erl_driver.h"

/*
** Protocol from driver:
** '\0' -> ok
** '\1' ++ String -> {error, Atom}
** Protocol when opening (arguments to start):
** <Filename>
** Where...
** Filename, a string:
**    The filename where the trace output is to be written.
** Port control messages handled:
** 'f' -> '\0' (ok) | '\1' ++ String (error) : Flush file.
**
** The package written to the file looks like this:
** +--+--------+-----------------------------------+
** |Op|Size NBO|Term in external format or empty   |
** +--+--------+-----------------------------------+
** Op, a char, for conformance with the IP driver:
**    0 = binary, 1 = drop
**    If Op is 1, then Size reflects the number of dropped messages. The 
**    op 1 is never used in this driver.
** Size, a 32 bit interger in network byte order:
**    Either the size of the binary term, or the number of packet's dropped. 
** Term, an array of bytes:
**    An erlang term in the external format or simply empty if Op == 1, the
**    term is Size long.
*/ 

typedef int FILETYPE;

#define BUFFER_SIZE (BUFSIZ*8)

#define OP_BINARY 0
#define OP_DROP   1

/*
** State structure
*/

typedef struct trace_file_data {
    FILETYPE fd;
    ErlDrvPort port;
    struct trace_file_data *next;
    int buff_siz;
    int buff_pos;
    unsigned char buff[1]; /* You guessed it, will be longer... */
} TraceFileData;

static TraceFileData *first_data; 

/*
** Interface routines
*/
static ErlDrvData trace_file_start(ErlDrvPort port, char *buff);
static void trace_file_stop(ErlDrvData handle);
static void trace_file_output(ErlDrvData handle, char *buff, int bufflen);
static void trace_file_finish(void);
static int trace_file_control(ErlDrvData handle, unsigned int command, 
			      char* buf, int count, char** res, int res_size);

/*
** Internal routines
*/
static void *my_alloc(size_t size);
static int my_write(TraceFileData *data, unsigned char *buff, int siz);
static void my_flush(TraceFileData *data);
static void put_be(unsigned n, unsigned char *s);
static void close_unlink_port(TraceFileData *data); 
/*
** The driver struct
*/
ErlDrvEntry trace_file_driver_entry = {
    NULL,		   /* F_PTR init, N/A */
    trace_file_start,      /* L_PTR start, called when port is opened */
    trace_file_stop,       /* F_PTR stop, called when port is closed */
    trace_file_output,     /* F_PTR output, called when erlang has sent */
    NULL,                  /* F_PTR ready_input, called when input descriptor 
			      ready */
    NULL,                  /* F_PTR ready_output, called when output 
			      descriptor ready */
    "trace_file_drv",      /* char *driver_name, the argument to open_port */
    trace_file_finish,     /* F_PTR finish, called when unloaded */
    NULL,                  /* void * that is not used (BC) */
    trace_file_control,    /* F_PTR control, port_control callback */
    NULL,                  /* F_PTR timeout, reserved */
    NULL                   /* F_PTR outputv, reserved */
};

/*
** Driver initialization routine
*/
DRIVER_INIT(trace_file_drv)
{
    first_data = NULL;
    return &trace_file_driver_entry;
}

/*
** Driver interface routines
*/

/*
** Open a port
*/
static ErlDrvData trace_file_start(ErlDrvPort port, char *buff)
{
    TraceFileData *ret;
    char *ptr;
    FILETYPE fd;

#ifdef HARDDEBUG
    fprintf(stderr,"hello (%s)\r\n", buff);
#endif
    for(ptr = buff; *ptr != '\0' && *ptr != ' '; ++ptr)
	;
    while(*ptr != '\0' && *ptr == ' ')
	++ptr;
    if (ptr == '\0')
	return ERL_DRV_ERROR_BADARG;
    
    if ((fd = open(ptr, O_WRONLY | O_TRUNC | O_CREAT
#ifdef O_BINARY
		   | O_BINARY
#endif
		   , 0777)) < 0) {
	return ERL_DRV_ERROR_ERRNO;
    }
    ret = my_alloc(sizeof(TraceFileData) - 1 + BUFFER_SIZE);
    ret->fd = fd;
    ret->port = port;
    ret->next = first_data;
    ret->buff_siz = BUFFER_SIZE;
    ret->buff_pos = 0;
    first_data = ret;

    return (ErlDrvData) ret;
}


/*
** Close a port
*/
static void trace_file_stop(ErlDrvData handle)
{
    close_unlink_port((TraceFileData *) handle);
}

/*
** Data sent from erlang to port.
*/
static void trace_file_output(ErlDrvData handle, char *buff, int bufflen)
{
    TraceFileData *data = (TraceFileData *) handle;
    unsigned char b[5] = "";
    put_be((unsigned) bufflen, b + 1);
    if (my_write(data, b, sizeof(b)) < 0 || 
	my_write(data, buff, bufflen) < 0) {
	driver_failure_atom(data->port, "write_error");
    }
}

/*
** Control message from erlang, we handle $f, which is flush.
*/
static int trace_file_control(ErlDrvData handle, unsigned int command, 
			      char* buf, int count, char** res, int res_size)
{
    if (command == 'f') {
	TraceFileData *data = (TraceFileData *) handle;
	my_flush(data);
	if (res_size < 1) {
	    *res = malloc(1);
	}
	**res = '\0';
	return 1;
    } 
    return -1;
}
/*
** Driver unloaded
*/
static void trace_file_finish(void)
{
    while (first_data != NULL) {
	close_unlink_port(first_data);
    }
}

/*
** Internal helpers
*/

/*
** Yet another malloc wrapper
*/
static void *my_alloc(size_t size) 
{
    void *ret;
    if ((ret = (void *) driver_alloc(size)) == NULL) {
	/* May or may not work... */
	fprintf(stderr, "Could not allocate %d bytes of memory in %s.",
		(int) size, __FILE__);
	exit(1);
    }
    return ret;
}


static int my_write(TraceFileData *data, unsigned char *buff, int siz) 
{
    int wrote;

    if (data->buff_siz - data->buff_pos >= siz) {
	memcpy(data->buff + data->buff_pos, buff, siz);
	data->buff_pos += siz; 
	return siz;
    }
    
    wrote = data->buff_siz - data->buff_pos;
    memcpy(data->buff + data->buff_pos, buff, wrote);
    if (write(data->fd, data->buff, data->buff_siz) != data->buff_siz) {
	return -1;
    }
    data->buff_pos = 0;
    if (siz - wrote >= data->buff_siz) {
	/* Write directly, no need to buffer... */
	if (write(data->fd, buff + wrote, siz - wrote) != 
	    siz - wrote) {
	    return -1;
	}
	return siz;
    } 
    memcpy(data->buff, buff + wrote, siz - wrote);
    data->buff_pos = siz - wrote;
    return siz;
}

static void my_flush(TraceFileData *data)
{
    write(data->fd, data->buff, data->buff_pos);
    data->buff_pos = 0;
}

/*
** Write unsigned to buffer in big endian
*/
static void put_be(unsigned n, unsigned char *s)
{
    s[0] = n >> 24;
    s[1] = (n >> 16) & 0xFFFFU;
    s[2] = (n >> 8) & 0xFFFFU;
    s[3] = n & 0xFFFFU;
}

/*
** Close the whole port and clean up
*/
static void close_unlink_port(TraceFileData *data) 
{
    TraceFileData **tmp;

    my_flush(data);
    close(data->fd);

    for(tmp = &first_data; *tmp != NULL && *tmp != data; 
	tmp = &((*tmp)->next))
	;
    if (*tmp != NULL) {
	*tmp = (*tmp)->next;
    }
    driver_free(data);
}



