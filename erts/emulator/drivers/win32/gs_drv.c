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
 * Purpose: Linked in driver to communicate with Wish (Windows only).
 */

#include <winsock2.h>
#include <windows.h>
#include <process.h>
#include "global.h" /* For NIL */
#include "sys.h"
#include "driver.h"
#include "winsock_func.h"

EXTERN_FUNCTION(int, send_error_to_logger, (uint32));

/*
 * Local functions.
 */
static SOCKET start_local_sock(char*, PROCESS_INFORMATION*);
static int start_wish(char*, unsigned, PROCESS_INFORMATION*);
static int start_winsock(void);
static int error(int port_num, char* format, ...);
static char* strsave(char* string);
static DWORD WINAPI threaded_reader(LPVOID param);
static char* last_error(void);
static char* last_wsa_error(void);

/*
 * Local variables.
 */
typedef struct {
    int port;			/* Port number. */
    SOCKET local_sock;		/* Socket which wish is connecte to. */
    HANDLE read_finished;	/* From reader: Read finished. */
    HANDLE start_reading;	/* Ask the threaded reader to start reading. */
    char* wish_buffer;		/* Buffer for reading from Wish. */
    size_t wish_buffer_size;	/* Size of buffer. */
    size_t bytes_read;		/* Bytes currently in buffer. */
    PROCESS_INFORMATION pinfo;	/* For wish. */
} GsPort;

static char key_name[] = "Software\\Ericsson\\Erlang\\";
static char key_value_name[] = "current_version";

static int gs_init(void);
static long gs_start();
static int gs_stop();
static int gs_from_erlang();
static int gs_from_wish();

struct driver_entry gs_driver_entry = {
    gs_init, gs_start, gs_stop, gs_from_erlang,
    gs_from_wish, null_func, "gs__drv__"
};

static int
gs_init(void)
{
    DEBUGF(("gs_init()\n"));
    return 0;
}

static long
gs_start(int port, char* buf)
{
    DWORD rtid;
    GsPort* p;
    char* s;

    DEBUGF(("gs_start(%d, %s)\n", port, buf));

    if ((s = strchr(buf, ' ')) != NULL) {
	buf = s+1;
    } else {
	buf = "";
    }
    if (start_winsock() == -1) {
	return -1;
    }
    p = sys_alloc(sizeof(GsPort));
    if (p == NULL) {
	return -1;
    }
    p->port = port;
    p->local_sock = start_local_sock(buf, &(p->pinfo));
    p->read_finished = CreateEvent(NULL, FALSE, FALSE, NULL);
    p->start_reading = CreateEvent(NULL, FALSE, TRUE, NULL);
    p->wish_buffer_size = 2048;
    p->wish_buffer = sys_alloc(p->wish_buffer_size);
    p->bytes_read = 0;

    driver_select(port, p->read_finished, DO_READ, 1);
    _beginthreadex(NULL, 0, threaded_reader, (LPVOID) p, 0, &rtid);
    return (long) p;
}

static int
gs_stop(long clientData)
{
    GsPort* p = (GsPort *) clientData;
    int i;

    TerminateProcess(p->pinfo.hProcess, 0);
    driver_select(p->port, p->read_finished, DO_READ, 0);
    switch (i = WaitForSingleObject(p->read_finished, INFINITE)) {
    case WAIT_FAILED:
	error(-1, "wait for read_finished failed: %s", last_error());
	break;
    case WAIT_OBJECT_0:
	break;
    default:
	error(-1, "unpexected result from wait for read_finished: %d", i);
	break;
    }
    (*winSock.closesocket)(p->local_sock);
    p->local_sock = 0;
    SetEvent(p->start_reading);
    WaitForSingleObject(p->read_finished, INFINITE);
    sys_free(p->wish_buffer);
    sys_free(p);
    return 1;
}

static int
gs_from_erlang(clientData, buf, count)
long clientData;
byte *buf;
int count;
{
    GsPort* p = (GsPort *) clientData;

    if ((*winSock.send)(p->local_sock, buf, count, 0) < 0) {
	return error(p->port, "error writing to wish shell: %s",
		     last_wsa_error());
    }
    return 1;
}

static int
gs_from_wish(long clientData, int event)
{
    GsPort* p = (GsPort *) clientData;

    driver_output(p->port, p->wish_buffer, p->bytes_read);
    SetEvent(p->start_reading);
    return 1;
}

static DWORD WINAPI
threaded_reader(LPVOID param)
{
    GsPort* p = (GsPort *) param;
    
    for (;;) {
	int i;

	switch (i = WaitForSingleObject(p->start_reading, INFINITE)) {
	case WAIT_OBJECT_0:
	    break;
	case WAIT_FAILED:
	    /* XXX Error */
	    break;
	default:
	    /* XXX Error */
	    break;
	}
	    
	if (p->local_sock == 0) {
	    SetEvent(p->read_finished);
	    return 0;
	}
	p->bytes_read = (*winSock.recv)(p->local_sock, p->wish_buffer,
					p->wish_buffer_size, 0);
	if (p->bytes_read < 0) {
	    /* XXX Failed */
	    return 0;
	}
	SetEvent(p->read_finished);
    }
}

static int
start_wish(char *initScript, unsigned portno, PROCESS_INFORMATION* pinfo)
{
    STARTUPINFO si;
    char cmd[1024];
    char* wish;

    memset(&si, 0, sizeof(si));
    si.cb = sizeof(si);
    wish = getenv("_GS_WISH_EXECUTABLE_");
    if (wish == NULL) {
	return error(-1, "The environment variable _GS_WISH_EXECUTABLE_ not set");
    }
    sprintf(cmd, "\"%s\" \"%s\" -- %ld", wish, initScript, portno);
    if (!CreateProcess(NULL, cmd, NULL, NULL, TRUE,
		      0, NULL, NULL, &si, pinfo)) {
	return error(-1, "Failed to start %s: %s\n", cmd, last_error());
    }
    return 0;
}

static int
start_winsock(void)
{
    WORD wVersionRequested;
    WSADATA wsaData;
    
    if (!tcp_lookup_functions()) {
	return error(-1, "Failed to load winsock dll");
    }

    wVersionRequested = MAKEWORD(1, 1);
    
    if ((*winSock.WSAStartup)(wVersionRequested, &wsaData) != 0) {
	return error(-1, "WSAStartup failed: %s", last_wsa_error());
    }
    
    if(LOBYTE(wsaData.wVersion) != 1 ||
       HIBYTE(wsaData.wVersion) != 1) {
	(*winSock.WSACleanup)();
	return error(-1, "WSAStartup returned version %d.%d",
		     HIBYTE(wsaData.wVersion),
		     LOBYTE(wsaData.wVersion));
    }
    return 0;
}

static SOCKET
start_local_sock(char* initScript, PROCESS_INFORMATION* pinfo)
{
    struct sockaddr_in iserv_addr;
    SOCKET tmpsock;
    int length;
    u_int portno;
    static unsigned long one = 1;
    SOCKET local_sock;

    /*
     * Create a socket to which Wish can connect.
     */

    tmpsock = (*winSock.socket)(PF_INET, SOCK_STREAM, 0);
    if (tmpsock == INVALID_SOCKET) {
	return error(-1, "socket failed: %s", last_wsa_error());
    }
    memset((char *) &iserv_addr, 0, sizeof(iserv_addr));
    iserv_addr.sin_family = AF_INET;
    iserv_addr.sin_addr.s_addr = (*winSock.htonl)(INADDR_ANY);
    iserv_addr.sin_port = 0;

    if ((*winSock.bind)(tmpsock, (struct sockaddr *) &iserv_addr,
			sizeof(iserv_addr))
	     == SOCKET_ERROR) {
	(*winSock.closesocket)(tmpsock);
	return error(-1, "bind on listen socket failed: %s", last_wsa_error());
    }
    if ((*winSock.listen)(tmpsock, 5) == SOCKET_ERROR) {
	(*winSock.closesocket)(tmpsock);
	return error(-1, "listen failed: %s", last_wsa_error());
    }
    length = sizeof(iserv_addr);
    if ((*winSock.getsockname)(tmpsock, (struct sockaddr *) &iserv_addr, &length)
	== SOCKET_ERROR) {
	(*winSock.closesocket)(tmpsock);
	return error(-1, "getsockname on listen socket failed: %s",
		     last_wsa_error);
    }
    portno = (*winSock.ntohs)(iserv_addr.sin_port);

    /*
     * Start Wish and wait for it to connect.
     */

    if (start_wish(initScript, portno, pinfo) == -1) {
	(*winSock.closesocket)(tmpsock);
	return (SOCKET) -1;
    }

    local_sock = (*winSock.accept)(tmpsock, (struct sockaddr *) 0, (int *) 0);
    (*winSock.closesocket)(tmpsock);
    if (local_sock == INVALID_SOCKET) {
	return error(-1, "accept failed: %s", last_wsa_error);
    }

    if ((*winSock.setsockopt)(local_sock, IPPROTO_TCP, TCP_NODELAY,
		   (char *)&one, sizeof(one)) == SOCKET_ERROR) {
	(*winSock.closesocket)(local_sock);
	return error(-1, "failed to set TCP_NODELAY option: %s",
		     last_wsa_error());
    }

    return local_sock;
}

static char*
strsave(char* string)
{
    char* p = sys_alloc(strlen(string)+1);

    if (p != NULL) {
	strcpy(p, string);
    }
    return p;
}

static int
error(int port_num, char* format, ...)
{
    char sbuf[2048];
    va_list ap;

    va_start(ap, format);
    vsprintf(sbuf, format, ap);
    va_end(ap);

    sys_printf(CBUF, "%s\n", sbuf);
    send_error_to_logger(NIL);
    if (port_num != -1) {
	driver_failure(port_num, -1);
    }
    return -1;
}

static char*
last_error(void)
{
    return win32_errorstr(GetLastError());
}

static char*
last_wsa_error(void)
{
    return win32_errorstr((*winSock.WSAGetLastError)());
}
