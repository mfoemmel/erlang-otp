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
** This is a trace port, which means it cannot be busy and takes
** no opcodes.
** Send trace messages over the net.
*/
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#ifdef __WIN32__
#    include <winsock2.h>
#    include <windows.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef __WIN32__
#    ifdef VXWORKS
#        include <sockLib.h>
#        include <sys/times.h>
#        include <iosLib.h>
#        include <taskLib.h>
#        include <selectLib.h>
#        include <ioLib.h>
#        include "reclaim.h"
#    endif 
#        include <unistd.h>
#        include <errno.h>
#        include <sys/types.h>
#        include <sys/socket.h>
#        include <netinet/in.h>
#        include <fcntl.h>
#endif

#ifdef DEBUG
#    ifndef __WIN32__
         /* erl_exit is not available to dll_drivers on windows. */
         extern int erl_exit(int, char *, ...);
#        define ASSERT(X)                               \
                    do {				\
                        if (!(X)) {			\
                            erl_exit(1,"%s",#X);	\
                        } 				\
                    } while(0) 
#    else 
#        include <assert.h>
#        define ASSERT(X) assert(X)
#    endif
#else 
#    define ASSERT(X)
#endif 

#include "driver.h"

/*
** Protocol from driver:
** '\0' -> ok
** '\1' ++ String -> {error, Atom}
** Protocol when opening (arguments to start):
** <Portno> <Quesize> <Fl>
** Where...
** Portno, ascii string representing a number:
**     The TCP port number to listen to.
** Quesize, ascii string representing a number:
**     The number of messages to que up before dropping.
** Fl, ascii string representing a flag byte:
**     0x1 -> Drop oldest when que is full (instead of last arrived)
**     0x2 -> Fill the que even if noone is listening.
**
** The package sent over the network looks like this:
** +--+--------+-----------------------------------+
** |Op|Size NBO|Term in external format or empty   |
** +--+--------+-----------------------------------+
** Op, a char:
**    0 = binary, 1 = drop
**    If Op is 1, then Size reflects the number of dropped messages.
** Size, a 32 bit interger in network byte order:
**    Either the size of the binary term, or the number of packet's dropped. 
** Term, an array of bytes:
**    An erlang term in the external format or simply empty if Op == 1, the
**    term is Size long.
*/ 

/*
** SO, most of the differences between WinDoze and Posixish OS'es 
** is handeled here, but the multiplexing (driver_select) is also quite
** interesting, see my_driver_select further down in the file...
*/

#ifndef __WIN32__
#    define FLAG_READ DO_READ
#    define FLAG_WRITE DO_WRITE
     typedef int SOCKET;
#    define INVALID_SOCKET -1
#    define IS_INVALID_SOCKET(S) ((S) < 0)
#    define closesocket(Sock) close(Sock)
#    define ERRNO errno
#    ifdef EWOULDBLOCK
#        define ERRNO_BLOCK EWOULDBLOCK
#    else
#        ifdef EAGAIN
#            define ERRNO_BLOCK EAGAIN
#        else
#            error "No EWOULDBLOCK found"
#        endif
#    endif
#else /* Win32 */
#    define FLAG_READ FD_READ | FD_CONNECT | FD_ACCEPT
#    define FLAG_WRITE FD_WRITE
#    define ERRNO WSAGetLastError()
#    define ERRNO_BLOCK WSAEWOULDBLOCK
#    define IS_INVALID_SOCKET(S) ((S) == INVALID_SOCKET)
#endif


/*
** Option flags
*/
#define FLAG_DROP_OLDEST    1
#define FLAG_FILL_ALWAYS    2
#define FLAG_LISTEN_PORT    4

/*
** Op's in messages
*/
#define OP_BINARY 0
#define OP_DROP   1
     
/*
** State structure
*/

typedef struct trace_ip_message {
    int siz; /* the size of the "binary data" */
    int written; /* if only a part was written, when == siz, all is written */
    unsigned char bin[1]; /* The opcode, the Size and optionally the binary. */
} TraceIpMessage;
    

typedef struct trace_ip_data {
    unsigned flags;
    int listen_portno;
    SOCKET listenfd;
    SOCKET fd;
#ifdef __WIN32__
    unsigned listen_event_mask;
    HANDLE listen_event;
    unsigned event_mask;
    HANDLE event;
#endif
    long port;
    struct trace_ip_data *next;
    int quesiz;
    int questart;
    int questop;
    TraceIpMessage *que[1]; /* You guessed it, will be longer... */
} TraceIpData;

static TraceIpData *first_data; 

/*
** Interface routines
*/
static long trace_ip_start(long port, char *buff);
static int trace_ip_stop(long handle);
static int trace_ip_output(long handle, unsigned char *buff, int bufflen);
#ifdef __WIN32__
static int trace_ip_event(long handle, HANDLE event);
#endif
static int trace_ip_ready_input(long handle, int fd);
static int trace_ip_ready_output(long handle, int fd);
static int trace_ip_finish(void); /* No arguments, despite what might be stated
				     in any documentation */

/*
** Internal routines
*/
static void *my_alloc(size_t size);
static int write_until_done(SOCKET s, char *buff, int bufflen);
static unsigned get_be(unsigned char *s);
static void put_be(unsigned n, unsigned char *s);
static TraceIpData *lookup_data_by_port(int portno);
static int set_nonblocking(SOCKET sock);
static TraceIpMessage *make_buffer(int datasiz, unsigned char op, 
				   unsigned number);
static void enque_message(TraceIpData *data, unsigned char *buff, int bufflen,
			  int byteswritten);
static void clean_que(TraceIpData *data);
static void close_client(TraceIpData *data);
static int trywrite(TraceIpData *data, unsigned char *buff, int bufflen);
static SOCKET my_accept(SOCKET sock);
static void close_unlink_port(TraceIpData *data);
static int my_driver_select(TraceIpData *desc, SOCKET fd, int flags, int on);
/*
** The driver struct
*/
DriverEntry trace_ip_driver_entry = {
    null_func,             /* F_PTR init, N/A */
    trace_ip_start,        /* L_PTR start, called when port is opened */
    trace_ip_stop,         /* F_PTR stop, called when port is closed */
    trace_ip_output,       /* F_PTR output, called when erlang has sent */
#ifdef __WIN32__
    trace_ip_event,        /* Anything happens on an associated event */
    null_func,             /* Write selections not supported on win32 */
#else
    trace_ip_ready_input,  /* F_PTR ready_input, called when input descriptor 
			      ready */
    trace_ip_ready_output, /* F_PTR ready_output, called when output 
			      descriptor ready */
#endif
    "trace_ip_drv",        /* char *driver_name, the argument to open_port */
    trace_ip_finish,       /* F_PTR finish, called when unloaded */
    null_func,             /* F_PTR control, port_command callback */
    null_func,             /* F_PTR timeout, reserved */
    null_func              /* F_PTR outputv, reserved */
};

/*
** Driver initialization routine
**
** No matter what's stated otherwise, this function shall return a pointer.
*/

#ifdef __WIN32__
__declspec(dllexport) void * driver_init(void *handle)
#else
void *DRIVER_INIT(trace_ip_drv)(void *handle) 
#endif
{
    first_data = NULL;
    trace_ip_driver_entry.handle = handle; /* ??? What is this, and why? */
    return &trace_ip_driver_entry;
}

/*
** Driver interface routines
*/

/*
** Open a port
*/
static long trace_ip_start(long port, char *buff)
{
    TraceIpData *ret;
    int portno;
    int quesiz;
    int flags;
    SOCKET s;
    struct sockaddr_in sin;
    int reuse = 1;

#ifdef HARDDEBUG
    fprintf(stderr,"trace_ip_drv/trace_ip_start (%s)\r\n", buff);
#endif
    if (sscanf(buff,"trace_ip_drv %d %d %d",&portno, &quesiz, &flags) != 3)
	return -1;

    if (flags > 3 || flags < 0 || portno < 0 || quesiz < 0)
	return -1;
    
    if (lookup_data_by_port(portno) != NULL)
	return -1;
    
    if (IS_INVALID_SOCKET(s = socket(AF_INET, SOCK_STREAM, 0)))
	return -1;

    if (setsockopt(s, SOL_SOCKET, SO_REUSEADDR, 
		   (char *) &reuse, sizeof(reuse)) < 0) {
	closesocket(s);
	return -1;
    }


    memset(&sin, 0, sizeof(sin));
    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = INADDR_ANY;
    sin.sin_port = htons((short) portno);
    
    if (bind(s,(struct sockaddr *) &sin, sizeof(sin)) != 0) {
	closesocket(s);
	return -1;
    }
    
    if (listen(s, 1)) { /* No significant backlog needed */
	closesocket(s);
	return -1;
    }

    if (set_nonblocking(s) != 0){
	closesocket(s);
	return -1;
    }
    
    /* OK, the socket is created, lets build the structure. */
    /* Deliberately one more pointer than the quesize specified... */
    ret = my_alloc(sizeof(TraceIpData) + 
		   quesiz * sizeof(TraceIpMessage *));
    
    ret->flags = flags | FLAG_LISTEN_PORT;
    ret->listen_portno = portno;
    ret->listenfd = s;
    ret->fd = INVALID_SOCKET;
    ret->port = port;
    ret->next = first_data;
    ret->quesiz = quesiz+1;
    ret->questart = ret->questop = 0;
    memset(ret->que, 0, ret->quesiz);
    
    first_data = ret;
#ifdef __WIN32__
    ret->listen_event_mask = 0;
    ret->listen_event = 0;
    ret->event_mask = 0;
    ret->event = 0;
#endif
    my_driver_select(ret, s, FLAG_READ, 1);

    return (long) ret;
}

/*
** Close a port
*/
static int trace_ip_stop(long handle)
{
    close_unlink_port((TraceIpData *) handle);
    return 0;
}

/*
** Data sent from erlang to port.
*/
static int trace_ip_output(long handle, unsigned char *buff, int bufflen)
{
    TraceIpData *data = (TraceIpData *) handle;
    if (data->flags & FLAG_LISTEN_PORT) {
	if (data->flags & FLAG_FILL_ALWAYS) {
	    enque_message(data, buff, bufflen, 0);
	}
	return 0;
    }
    if (data->que[data->questart] != NULL) {
	trace_ip_ready_output((long) data, data->fd);
    }
    if (data->que[data->questart] == NULL) {
	int written = trywrite(data, buff, bufflen);
	if (written >= 0 && written < bufflen + 5) {
	    enque_message(data, buff, bufflen, written);
	    my_driver_select(data, data->fd, FLAG_WRITE, 1);
	}
	return 0;
    }
    enque_message(data, buff, bufflen, 0);
    return 0;
}

/*
** We have something to read from a file descriptor
*/
static int trace_ip_ready_input(long handle, int fd)
{
    TraceIpData *data = (TraceIpData *) handle;
    int client;

    if (!(data->flags & FLAG_LISTEN_PORT) && fd == data->listenfd) {
	/*
	** Someone tries to connect to already connected port, 
	** just accept and close.
	*/
	if (!IS_INVALID_SOCKET((client = my_accept(data->listenfd)))) {
	    closesocket(client);
	}
	return 0;
    }

    if (fd == data->listenfd) {
	/*
	** Maybe accept, we are a listen port...
	*/
	if (!IS_INVALID_SOCKET((client = my_accept(data->listenfd)))) {
	    data->fd = client;
	    set_nonblocking(client);
	    if (data->que[data->questart] != NULL) {
		my_driver_select(data, data->fd, FLAG_WRITE | FLAG_READ, 1);
	    } else {
		my_driver_select(data, data->fd, FLAG_READ, 1);
	    }
	    data->flags &= ~(FLAG_LISTEN_PORT);
	}
	return 0;
    }

    ASSERT(fd == data->fd);
    close_client(data);
    return 0;
}

#ifdef __WIN32__
static int trace_ip_event(long handle, HANDLE event) 
{
   TraceIpData *data = (TraceIpData *) handle;
   if (event == data->event) {
       return trace_ip_ready_output(handle, data->fd);
   } else if (event == data->listen_event) {
       return trace_ip_ready_input(handle, data->listenfd);
   } else {
       return -1;
   }
}
#endif __WIN32__
       
/*
** We can write to a file descriptor
*/
static int trace_ip_ready_output(long handle, int fd)
{
    TraceIpData *data = (TraceIpData *) handle;
    int res;
    int towrite;
    TraceIpMessage *tim;

    ASSERT(!(data->flags & FLAG_LISTEN_PORT) &&
	   data->que[data->questart] != NULL &&
	   fd == data->fd);
    
    tim = data->que[data->questart];
    towrite = tim->siz - tim->written;
    while((res = write_until_done(data->fd, 
				  tim->bin + tim->written, towrite)) 
	  == towrite) {
	driver_free(tim);
	data->que[data->questart] = NULL;
	if (data->questart == data->questop) {
	    /*
	    ** We wrote the last message in the que, dont
	    ** step forward, just 'deselect'
	    */
	    my_driver_select(data, data->fd, FLAG_WRITE, 0);
	    /*
	    ** We are really done...
	    */
	    return 0;
	}
	if (++(data->questart) == data->quesiz)
	    data->questart = 0;
	tim = data->que[data->questart];
	ASSERT(tim != NULL);
	towrite = tim->siz - tim->written;
    }
    if (res < 0) {
	close_client(data);
	return 0;
    }

    tim->written += res;
    return 0;
}

/*
** Driver unloaded
*/
static int trace_ip_finish(void)
{
    while (first_data != NULL) {
	close_unlink_port(first_data);
    }
    return 0;
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

/*
** Write to a nonblocking descriptor until it states EWOULDBLOCK
*/
static int write_until_done(SOCKET s, char *buff, int bufflen)
{
    int ret = 0;
    int res = 0;

#ifdef HARDDEBUG
    fprintf(stderr, "Writing %d characters.\r\n", bufflen);
#endif

    while(ret < bufflen && (res = send(s, buff + ret, bufflen - ret, 0)) > 0) {
	ret += res;
    }
    if (ret < bufflen) {
	if (res == 0) {
	    fprintf(stderr, "internal error in trace_ip_drv, "
		     "write to nonblocking "
		     "returned 0!");
	    exit(1);
	} else if (ERRNO != ERRNO_BLOCK) {
		return -1;
	}
    }
    return ret;
}

/*
** Translate big endian integer in buffer to unsigned
*/
static unsigned get_be(unsigned char *s)
{
    return s[3] | (s[2] << 8) | (s[1] << 16) | s[0] << 24;
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
** Lookup a port's data structure by the *TCP/IP port number*
*/
static TraceIpData *lookup_data_by_port(int portno)
{
    TraceIpData *tmp = first_data;
    while (tmp != NULL && tmp->listen_portno != portno)
	tmp = tmp->next;
    return tmp;
}

/*
** Create a TraceIpMessage buffer (the binary data NOT filled in)
*/
static TraceIpMessage *make_buffer(int datasiz, unsigned char op, 
				   unsigned number)
{
    TraceIpMessage *ret = my_alloc(sizeof(TraceIpMessage) + 
				   (datasiz + 4)); 
    ret->siz = datasiz + 5;
    ret->written = 0;
    ret->bin[0] = op;
    put_be(number, ret->bin + 1);
    return ret;
}

/*
** Add message to que, discarding in a politically correct way...
** The FLAG_DROP_OLDEST is currently ingored...
*/
static void enque_message(TraceIpData *data, unsigned char *buff, int bufflen,
			  int byteswritten)
{
    int diff = data->questop - data->questart;
    TraceIpMessage *tim;

    if (diff == -1 || diff == data->quesiz - 1) {
	put_be(get_be((data->que[data->questop])->bin + 1) + 1, 
	       (data->que[data->questop])->bin + 1);
    } else if (diff == -2 || diff == data->quesiz - 2) {
	ASSERT(byteswritten == 0);
	if (++(data->questop) ==  data->quesiz) {
	    data->questop = 0;
	}
	data->que[data->questop] = make_buffer(0, OP_DROP, 1);
    } else {
	if (data->que[data->questop] != NULL &&
	    ++(data->questop) ==  data->quesiz) {
	    data->questop = 0;
	}
	tim = make_buffer(bufflen, OP_BINARY, bufflen);
	tim->written = byteswritten;
	memcpy(tim->bin + 5, buff, bufflen);
	data->que[data->questop] = tim;
    }
}

/*
** Clean a que
*/
static void clean_que(TraceIpData *data)
{
    int b = data->questart;
    int e = data->questop;

    while (b != e) {
	if (data->que[b] != NULL) {
	    driver_free(data->que[b]);
	    data->que[b] = NULL;
	}
	if (++b >= data->quesiz) {
	    b = 0;
	}
    }
    if (data->que[b] != NULL) {
	driver_free(data->que[b]);
	data->que[b] = NULL;
    }
    data->questart = data->questop = 0;
}
    
/*
** Cleanup closed client (or close the client and cleanup)
*/
static void close_client(TraceIpData *data) 
{
    my_driver_select(data, data->fd, FLAG_WRITE | FLAG_READ, 0);
    closesocket(data->fd);
    data->flags |= FLAG_LISTEN_PORT;
    if (!(data->flags & FLAG_FILL_ALWAYS)) {
	clean_que(data);
    }
}

/*
** Try to write a message from erlang directly (only called when que is empty 
** and client is connected)
*/
static int trywrite(TraceIpData *data, unsigned char *buff, int bufflen)
{
    unsigned char op[5];
    int res;

    op[0] = OP_BINARY;
    put_be(bufflen, op + 1);

    if ((res = write_until_done(data->fd, op, 5)) < 0) {
	close_client(data);
	return -1;
    }
    if (res < 5) {
	return res;
    }

    if ((res = write_until_done(data->fd, buff, bufflen)) < 0) {
	close_client(data);
	return -1;
    }

    return res + 5;
}

/*
** accept wrapper
*/
static SOCKET my_accept(SOCKET sock)
{
    struct sockaddr_in sin;
    int sin_size = sizeof(sin);

    return accept(sock, (struct sockaddr *) &sin, &sin_size);
}

/*
** Close the whole port and clean up
*/
static void close_unlink_port(TraceIpData *data) 
{
    TraceIpData **tmp;

    data->flags = 0;
    if (!IS_INVALID_SOCKET(data->fd)) {
	close_client(data);
    }
    my_driver_select(data, data->listenfd, FLAG_READ, 0);
    closesocket(data->listenfd);

    for(tmp = &first_data; *tmp != NULL && *tmp != data; 
	tmp = &((*tmp)->next))
	;
    if (*tmp != NULL) {
	*tmp = (*tmp)->next;
    }
    driver_free(data);
}




#ifdef __WIN32__
/*
** Mostly stolen from inet_drv in the emulator.
*/
static int my_driver_select(TraceIpData *desc, SOCKET fd, 
				 int flags, int on)
{
    HANDLE *event;
    unsigned *event_mask;
    int ret = -1;
    unsigned save_event_mask; 

    if(fd == desc->listenfd) {
	event = &(desc->listen_event);
	event_mask = &(desc->listen_event_mask);
    } else if(fd == desc->fd) {
	event = &(desc->event);
	event_mask = &(desc->event_mask);
    } else { 
	return -1;
    }

    save_event_mask = *event_mask;

    if (on) {
	*event_mask |= flags;
    } else {
	*event_mask &= (~flags);
    }
    if (*event_mask != 0 && *event == 0) { 
	*event = WSACreateEvent();
	driver_select(desc->port, *event, DO_READ, 1);
    }

    /* The RIGHT WAY (TM) to do this is to make sure:
       A) The cancelling of all network events is done with
          NULL as the event parameter (bug in NT's winsock),
       B) The actual event handle is reset so that it is only
          raised if one of the requested network events is active,
       C) Avoid race conditions by making sure that the event cannot be set
          while we are preparing to set the correct network event mask.
       The simplest way to do it is to turn off all events, reset the
       event handle and then, if event_mask != 0, turn on the appropriate
       events again. */
    if (WSAEventSelect(fd, NULL, 0) != 0) {
	*event_mask = save_event_mask;
	goto error;
    }
    if (!ResetEvent(*event)) {
	*event_mask = 0;
	goto error;
    }
    if (*event_mask != 0) {
	if (WSAEventSelect(fd, 
			   *event, 
			   *event_mask) != 0) {
	    *event_mask = 0;
	    goto error;
	}
    }
    ret = 0;
error:
    if (*event_mask == 0 && *event != 0) {
	WSAEventSelect(fd, NULL, 0); /* Not necessary? 
					Well, actually I dont know, and
					MS documentation states nothing
					about what happens if WSAEventSelect
					is called with empty event mask and 
					then the event is deleted. */ 
	driver_select(desc->port, *event, DO_READ, 0);
	WSACloseEvent(*event);
	*event = 0;
    }
    return ret;
}

#else /* UNIX/VXWORKS */

static int my_driver_select(TraceIpData *desc, SOCKET fd, int flags, int on)
{
    return driver_select(desc->port, fd, flags, on);
}	    

#endif /* !__WIN32__ */

/*
** Set socket nonblocking, keep this at end of file.
*/
#undef ERRNO_BLOCK
#undef ASSERT
#ifndef WANT_NONBLOCKING
#define WANT_NONBLOCKING
#endif
#include "sys.h"
static int set_nonblocking(SOCKET sock)
{
    SET_NONBLOCKING(sock);
    return 0;
}


