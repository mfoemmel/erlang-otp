/*
** etk.c
**
** Implements a subset of tcl to execute erlang
** functions (as scripts)
**
**  Works with: Tk4.2 (& Tcl7.6)
*/
#include <stdio.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "etk.h"
#include "tkstr.h"

#include <tk.h>

#if (TK_MAJOR_VERSION != 4) && (TK_MINOR_VERSION != 2)
#error "bad tk version should be 4.2"
#endif

#if (TCL_MAJOR_VERSION != 7) && (TCL_MINOR_VERSION != 6)
#error "bad tcl version should be 7.6"
#endif

static SOCKET erl_input =  0;
static SOCKET erl_output = 1;

static int erl_closed = 0;

typedef unsigned char u_int8;

/* Minimum fix buffer size is 8!!! */
#define IO_BUFFER_SIZE 16*1024

static u_int8  iobuffer[IO_BUFFER_SIZE];
static u_int8* iobuf;
static int iobuf_size;

#ifndef __WIN32__
extern int matherr();
int *tclDummyMathPtr = (int *) matherr;
#endif

EXTERN int Tcl_LinkVar();
int (*tclDummyLinkVarPtr)() = Tcl_LinkVar;

static Tk_Window mainWindow;	/* The main window for the application.  If
				 * NULL then the application no longer
				 * exists. */
static Tcl_Interp *interp;	/* Interpreter for this application. */
static int synchronize ;
static int withdrawn;
static char *name = NULL;
static char *display = NULL;
static char *geometry = NULL;
static char *colormap;
static char *visual;
static int  etk_port = -1;   /* port number to use if connecting */
static int rest = 0;

#ifdef __WIN32__
static HINSTANCE etk_hInstance;
extern int Etk_WinSockFunctions(Ws2Funcs*);
#endif

extern int TkCreateFrame(ClientData clientData,
			 Tcl_Interp *interp,
			 int argc, char **argv,
			 int toplevel,char *appName);
extern int Etk_Execute(int, ClientData, Tcl_Interp*, int, char**);


static Tk_ArgvInfo argTable[] = {
    {"-colormap", TK_ARGV_STRING, (char *) NULL, (char *) &colormap,
        "Colormap for main window"},
    {"-display", TK_ARGV_STRING, (char *) NULL, (char *) &display,
        "Display to use"},
    {"-geometry", TK_ARGV_STRING, (char *) NULL, (char *) &geometry,
        "Initial geometry for window"},
    {"-name", TK_ARGV_STRING, (char *) NULL, (char *) &name,
        "Name to use for application"},
    {"-sync", TK_ARGV_CONSTANT, (char *) 1, (char *) &synchronize,
        "Use synchronous mode for display server"},
    {"-visual", TK_ARGV_STRING, (char *) NULL, (char *) &visual,
        "Visual for main window"},
    /* Added for Etk */
    {"-port", TK_ARGV_INT, (char*) -1, (char *) &etk_port,
	 "connect etk to main program with a socket" },
    {"-withdrawn", TK_ARGV_CONSTANT, (char*) 1, (char *) &withdrawn,
	 "Start etk main window withdrawn" },
    {"--", TK_ARGV_REST, (char *) 1, (char *) &rest,
        "Pass all remaining arguments through to script"},
    {(char *) NULL, TK_ARGV_END, (char *) NULL, (char *) NULL,
        (char *) NULL}
};

/*
**
*/
void Etk_Exit(code, msg)
int code; char* msg;
{
    if (code != 0)
	fprintf(stderr, "etk: %s\n", msg);
    Tcl_Exit(code);
}

static int erl_read_fill(SOCKET fd, char* buf, int len)
{
    int i, got=0;
    do {
	if ((i = sock_read(fd, buf+got, len-got)) <= 0)
	    return (i);
	got += i;
    } while (got < len);
    return (len);
}

static int erl_write_fill(SOCKET fd, char *buf, int len)
{
  int i,done=0;
  do {
    if ((i = sock_write(fd, buf+done, len-done)) <= 0)
	return (i);
    done += i;
  } while (done < len);
  return (len);
}

static void expand_iobuffer(int size)
{
    if (size > iobuf_size) {
	if (iobuf == iobuffer)
	    iobuf = malloc(size);
	else
	    iobuf = realloc(iobuf, size);
	if (iobuf == NULL)
	    Etk_Exit(1, "out of memory");
	iobuf_size = size;
    }
}

static SOCKET erl_connect(short port)
{
    struct sockaddr_in a;
    SOCKET s = INVALID_SOCKET;    
    int len = sizeof(a);
    int on = 1;

#ifdef __WIN32__
    WORD wVersionRequested;
    WSADATA wsaData;
    if (!Etk_WinSockFunctions(&Ws2))
	goto error;
    wVersionRequested = MAKEWORD(2,0);
    if ((*Ws2.WSAStartup)(wVersionRequested, &wsaData) != 0)
	goto error;
    if ((LOBYTE(wsaData.wVersion) != 2) || (HIBYTE(wsaData.wVersion) != 0))
	goto error;
#endif
    if ((s = sock_open(AF_INET, SOCK_STREAM, 0)) == INVALID_SOCKET)
	goto error;
    memset((char*) &a, 0, sizeof(a));
    a.sin_family = AF_INET;
    a.sin_addr.s_addr = sock_htonl(INADDR_LOOPBACK);
    a.sin_port = sock_htons(port);
    if (sock_connect(s, (struct sockaddr*) &a, len) == SOCKET_ERROR)
	goto error;
    /* The TCP_NODELAY option is needed */ 
    sock_setopt(s, IPPROTO_TCP, TCP_NODELAY, (char*) &on, sizeof(int));    
    SET_NONBLOCKING(s);    
    return s;

 error:
    if (s != INVALID_SOCKET)
	sock_close(s);
#ifdef __WIN32__
    (*Ws2.WSACleanup)();
#endif    
    return INVALID_SOCKET;
}

/*
** Send argv info to erlang as:
**
**  32-bit-length  data
*/

int SendToErlang(cmd, clientData, interp, argc, argv)
int cmd; ClientData clientData; Tcl_Interp* interp; int argc; char** argv;
{
    int i;
    int len;
    int buflen;
    u_int8* ptr;

    if (erl_closed)
	return TCL_OK;

 again:
    ptr = iobuf + 4;    /* save four byte header in iobuf */
    PUT_INT16(ptr, 0);
    ptr += 2;
    PUT_INT16(ptr, cmd);
    ptr += 2;
    buflen = 8;        /* used sofar */

    for (i = 1; i < argc; i++) {
	int n = strlen(argv[i]);
	buflen += n + 4;
	if (buflen > iobuf_size) {  /* overflow */
	    /* get size of rest */
	    i++;
	    while(i < argc) {
		buflen += strlen(argv[i]) + 4;
		i++;
	    }
	    expand_iobuffer(buflen); /* fix io-buffer size */
	    goto again;              /* redo it */
	}
	PUT_INT32(ptr, n);
	ptr += 4;
	memcpy(ptr, argv[i], n);
	ptr += n;
    }
    len = buflen - 4;
    PUT_INT32(iobuf, len);
    erl_write_fill(erl_output, iobuf, buflen);
    return TCL_OK;
}

/*
** Callback for tkerror & bgerror
*/
static int tkError(data, interp, argc, argv)
ClientData data; Tcl_Interp* interp; int argc; char** argv;
{
    return SendToErlang(TK_ERL_TKERROR, data, interp, argc, argv);
}

/*
** Callback for tkscreenchanged
*/
static int tkScreenChanged(data, interp, argc, argv)
ClientData data; Tcl_Interp* interp; int argc; char** argv;
{
    return SendToErlang(TK_ERL_TKSCREEN, data, interp, argc, argv);
}

/*
** Callback for event functions
** args:  event Id argv[2] argv[3] ... 
** write  Id, argv[2], argv[3] ...
*/

static int ErlangEvent(data, interp, argc, argv)
ClientData data; Tcl_Interp* interp; int argc; char** argv;
{
    return SendToErlang(TK_ERL_EVENT, data, interp, argc, argv);
}

/*
** Callback for invoke functions
** args:  invoke Id argv[2] argv[3] ...
** write  Id, argv[2], argv[3] ...
*/

static int ErlangInvoke(data, interp, argc, argv)
ClientData data; Tcl_Interp* interp; int argc; char** argv;
{
    return SendToErlang(TK_ERL_INVOKE, data, interp, argc, argv);
}

/*
** Callback for operation function
** args: operation id argv[2] argv[3]
** write: id argv[2] argv[3]
*/
int ErlangOperation(data, interp, argc, argv)
ClientData data; Tcl_Interp* interp; int argc; char** argv;
{
    return SendToErlang(TK_ERL_OPERATION, data, interp, argc, argv);
}

/*
**
**  INPUT:
**  +--------+--------+
**  |  Seq   | Cmd(1) |   2+2 bytes
**  +--------+--------+
**  | Argc = n        |   4 bytes
**  +-----------------+
**  | OP | Argv[0]    |   4 bytes
**  +-----------------+
**  | OP | Argv[1]    |   4 bytes
**  +-----------------+
**  |       ...       |
**  +-----------------+
**  | OP | Argv[n-1]  |   4 bytes
**  +-----------------+
**  |  0   0   0   0  |
**  +-----------------+
**  | Data Area Len   |   4 bytes
**  +-----------------+
**  | Len(4) Data     |
**  | Len(4) Data     |
**  | ...             |
**  |                 |
**  +-----------------+
**  |                 |
**
**         ...
**  |                 |
**  +--------+--------+
**  |  Seq   | Cmd(N) |   2+2 bytes
**  +--------+--------+
**  | Command Len(N)  |   4 bytes
**  +-----------------+
**         ...
**  |                 |
**  +-----------------+
**
**
**  OUTPUT:
**  +--------+--------+
**  |  Seq   | Code   |   2+2 bytes
**  +--------+--------+
**  | Reply string    |
**  |     ...         |
**  +-----------------+
**
**  Reply is from the first failed command on Error
**  the last command if Ok
**  
*/

void ErlangInputProc(data, mask)
ClientData data; int mask;
{
    SOCKET s = (int) data;
    int code;
    int len;
    int n;
    int seq0;
    u_int8* p_pos;
    u_int8* p_end;
    u_int8 lenbuf[4];

    if ((n = erl_read_fill(s, (char*)lenbuf, 4)) < 0) {
	erl_closed = 1;
	Etk_Exit(1, "read error");
    }
    else if (n == 0) {
	erl_closed = 1;
	Etk_Exit(0, "eof");
    }

    len = GET_INT32(lenbuf);
    expand_iobuffer(len);
    if ((n = erl_read_fill(s, (char*) iobuf, len)) < 0)
	Etk_Exit(1, "read error");
    else if (n == 0)
	Etk_Exit(0, "eof");

    p_pos = iobuf;
    p_end = p_pos + len;
    seq0 = -1;

    while(p_pos < p_end) {
	int argc;
	char** argv;
	int seq;
	int cmd;
	int i;
	u_int8* ptr;
	u_int8* p = p_pos;
	
	seq = GET_INT16(p);     p += 2;
	cmd = GET_INT16(p);     p += 2;
	argc = GET_INT32(p);    p += 4;
	argv = (char**) (p);
	ptr = p + 4*(argc+1);       /* data area pointer */
	len = GET_INT32(ptr);  ptr += 4;
	p_pos = ptr + len;          /* start of next command */

	if (seq0 == -1)
	    seq0 = seq;
	else if (seq0 != seq)
	    Etk_Exit(1, "bad sequence number");

	/* Contruct argv from offsets and indices */
	for (i = 0; i < argc; i++) {
	    int offs;
	    p = (u_int8*) (argv + i);
	    switch (*p++) {
	    case TK_OP_OFFSET:
		len = GET_INT32(ptr);   /* get data length */
		ptr += 4;              /* skip length field */
		argv[i] = ptr;
		ptr += len;            /* prepare for next data */
		break;
	    case TK_OP_OPTION:
		offs = GET_INT24(p);
		if (offs >= OPT_MAX)
		    Etk_Exit(1, "bad option offset");
		argv[i] = tkopt[offs];
		break;
	    case TK_OP_STRING:
		offs = GET_INT24(p);
		if (offs >= STR_MAX)
		    Etk_Exit(1, "bad string offset");
		argv[i] = tkstr[offs];
		break;
	    default:
		Etk_Exit(1, "bad option type");
	    }
	}
	Tcl_ResetResult(interp);
	switch(Etk_Execute(cmd,(ClientData)mainWindow,interp,argc,argv)) {
	case TCL_BREAK:
	    Tcl_ResetResult(interp);
	    return;
	case TCL_OK:
	    code = TK_ERL_OK;
	    break;
	case TCL_ERROR:
	    code = TK_ERL_ERROR;
	    goto error;
	}
    }

 error:
    len =  strlen(interp->result);
    expand_iobuffer(len + 8);
    PUT_INT32(iobuf, len+4);    /* length header */
    PUT_INT16(iobuf+4, seq0);   /* sequence reply */
    PUT_INT16(iobuf+6, code);   /* reply code */
    memcpy(iobuf+8, interp->result, len);
    Tcl_ResetResult(interp);
    erl_write_fill(erl_output, iobuf, len+8);
}


#ifdef __WIN32__

#ifndef TCL_ACTIVE
#define TCL_ACTIVE (1<<4)
#endif

extern void TkWinXInit(HINSTANCE hInstance);

static void DisplaySetupProc(ClientData clientData, int flags)
{
    if ((flags & TCL_WINDOW_EVENTS) && (Tk_GetNumMainWindows()>0)) {
	Tcl_WatchFile((Tcl_File) NULL, TCL_ACTIVE);
    }
}

static void DisplayCheckProc(ClientData clientData, int flags)
{
    return;
}

HINSTANCE Etk_GetInstance()
{
    return etk_hInstance;
}


static void TkCreateWinEventSource()
{
    Tcl_CreateEventSource(DisplaySetupProc, DisplayCheckProc,
			  (ClientData) NULL);
    TkWinXInit((HINSTANCE)etk_hInstance);
}

#endif

/*
** Port main loop
*/

#ifdef __WIN32__
int
WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
		    PSTR szCmdLine, int iCmdShow)
{
    int argc = __argc;
    char **argv = __argv;
#else
int
main(int argc, char **argv)
{
#endif
    /* Initialize */
    int code;
    char* av[20];
    int   ac;
    char* class;

    iobuf = iobuffer;
    iobuf_size = IO_BUFFER_SIZE;

#ifdef __WIN32__
    etk_hInstance = hInstance;
#endif
    erl_closed = 0;
    interp = Tcl_CreateInterp();

    synchronize = 0;
    withdrawn = 0;
    etk_port = -1;
    name = display = geometry = colormap = visual = NULL; 

    if (Tk_ParseArgv(interp, (Tk_Window) NULL, &argc, argv, argTable, 
		     TK_ARGV_NO_DEFAULTS) != TCL_OK) {
	fprintf(stderr, "%s\n", interp->result);
	exit(1);
    }
    /* On unix erl_input and erl_output will work for pipes */
    erl_input  = 0;
    erl_output = 1;
    if (etk_port != -1) {
	if ((erl_input = erl_connect(etk_port)) == INVALID_SOCKET) {
	    fprintf(stderr, "etk: could not connect\n");
	    exit(1);
	}
	erl_output = erl_input;
    }
#ifdef __WIN32__
    else {
	fprintf(stderr, "etk: option -port must be given\n");
	exit(1);
    }
#endif

    if (name == NULL) {
	char* p = strrchr(argv[0], '/');
	if (p == NULL)
	    name = argv[0];
	else
	    name = p+1;
    }

    class = (char *) ckalloc((unsigned) (strlen(name) + 1));
    strcpy(class, name);
    class[0] = toupper((unsigned char) class[0]);

    av[0] = "toplevel";
    av[1] = ".";
    av[2] = "-class";
    av[3] = class;
    ac = 4;
 
    if (display != NULL) {
	av[ac] = "-screen";
	av[ac+1] = display;
	ac += 2;
    }
    if (colormap != NULL) {
	av[ac] = "-colormap";
	av[ac+1] = colormap;
	ac += 2;
    }
    if (visual != NULL) {
	av[ac] = "-visual";
	av[ac+1] = visual;
	ac += 2;
    }
    av[ac] = NULL;
    code = TkCreateFrame((ClientData) NULL, interp, ac, av, 1, name);
    ckfree(class);
    if (code != TCL_OK) {
	fprintf(stderr, "%s\n", interp->result);
	exit(1);
    }
    mainWindow = Tk_MainWindow(interp);

    Tcl_CreateCommand(interp, "event", 
		      (Tcl_CmdProc*)ErlangEvent, 
		      (ClientData) erl_output, 
		      (Tcl_CmdDeleteProc*) NULL);

    Tcl_CreateCommand(interp, "invoke", 
		      (Tcl_CmdProc*)ErlangInvoke, 
		      (ClientData) erl_output, 
		      (Tcl_CmdDeleteProc*) NULL);

    Tcl_CreateCommand(interp, "bgerror", 
		      (Tcl_CmdProc*)tkError, 
		      (ClientData) erl_output,
		      (Tcl_CmdDeleteProc*) NULL);

    Tcl_CreateCommand(interp, "tkScreenChanged", 
		      (Tcl_CmdProc*)tkScreenChanged, 
		      (ClientData) erl_output,
		      (Tcl_CmdDeleteProc*) NULL);

    Tcl_ResetResult(interp);

#ifndef __WIN32__
    if (synchronize) {
	XSynchronize(Tk_Display(mainWindow), True);
    }
#endif

    if (geometry != NULL) {
	av[0] = "wm";
	av[1] = "geometry";
	av[2] = ".";
	av[3] = geometry;
	av[4] = NULL;

	if (Tk_WmCmd((ClientData) mainWindow, interp, 4, av) != TCL_OK) {
	    fprintf(stderr, "%s\n", interp->result);
	    exit(1);
	}
    }

    if (withdrawn) {
	av[0] = "wm";
	av[1] = "withdraw";
	av[2] = ".";
	av[3] = NULL;
	if (Tk_WmCmd((ClientData) mainWindow, interp, 3, av) != TCL_OK) {
	    fprintf(stderr, "%s\n", interp->result);
	    exit(1);
	}
    }

#ifdef __WIN32__
    TkCreateWinEventSource();
    Tcl_CreateFileHandler(Tcl_GetFile((ClientData)erl_input,
				      TCL_WIN_SOCKET),
			  TK_READABLE, (Tcl_FileProc*) ErlangInputProc,
			  (ClientData) erl_input);
#else
    TkCreateXEventSource(); 
    Tcl_CreateFileHandler(Tcl_GetFile((ClientData)erl_input, TCL_UNIX_FD),
			  TK_READABLE, ErlangInputProc,
			  (ClientData) erl_input);
#endif

    while (Tk_GetNumMainWindows() > 0) {
        Tcl_DoOneEvent(0);
    }
    Etk_Exit(0, "done");
    return 0;
}
