/*
** etk_drv.c
**
** Implements a subset of tcl to execute erlang
** functions (as scripts)
**
**  Works with: Tk4.2 (& Tcl7.6) 
**  solaris threads/ pthreads / win32 
**
*/

#include <stdio.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>

#include "etk.h"
#include "tkstr.h"

#include <tk.h>      /* correct version of tk.h */

#include "driver.h"

#if (TK_MAJOR_VERSION != 4) && (TK_MINOR_VERSION != 2)
#error "bad tk version should be 4.2"
#endif

#if (TCL_MAJOR_VERSION != 7) && (TCL_MINOR_VERSION != 6)
#error "bad tcl version should be 7.6"
#endif

#ifndef USE_THREADS
#error "etk_drv must be compiled with threads"
#endif


#if defined(POSIX_THREADS)
#include <pthread.h>

typedef pthread_t       THREAD_T;
typedef pthread_mutex_t LOCK_T;

#elif defined(SOLARIS_THREADS)
#include <thread.h>

typedef thread_t        THREAD_T;
typedef mutex_t         LOCK_T;

#elif defined(__WIN32__)

typedef HANDLE           THREAD_T;
typedef CRITICAL_SECTION LOCK_T;

#elif
#error "bad thread configuration"
#endif

#define ALIGN_SZ	sizeof(double)
#define MESG_HDR_SZ	(sizeof(etk_mesg_t)-ALIGN_SZ)
#define MESG_BUF_SZ     256	/* must be dividable with ALIGN_SZ!!! */
#define MESG_FIXED_SZ	(MESG_HDR_SZ + MESG_BUF_SZ)
#define MESG_NO_FIXED	4	/* number of fixed messages */

typedef unsigned char u_int8;

typedef struct etk_mesg {
    struct etk_mesg* next; /* next in free list */
    int dynamic;			 /* fixed message or not */
    int len;
    u_int8 data[ALIGN_SZ];	 /* use sizeof(double) for alignment */
} etk_mesg_t;

typedef struct etk_desc {
    SOCKET s;          /* Erlang side socket or INVALID_SOCKET if not open */
    SOCKET s_tk;       /* Tk side socket or INVALID if not open */

    HANDLE event;      /* Event handle (same as s in unix) */
    long  event_mask;  /* current FD events */
    long  port;        /* the port identifier */
} etk_desc_t;

/* allocate a pool of fix sized messages (each message must be of size=N bytes
This should fix some memory allocation overhead problems.	
*/

static int  etk_finish();
static int  etk_init();
static long etk_start();
static int  etk_stop();
static int  etk_command();
static int  etk_input();
static int  etk_output();
#ifdef __WIN32__
static int  etk_event();
#endif

DriverEntry etk_driver_entry =
{
    etk_init,
    etk_start,
    etk_stop,
    etk_command,
#ifdef __WIN32__
    etk_event,
#else
    etk_input,
#endif
    etk_output,
    "etk_drv",
    NULL,		/* handle */
    NULL,		/* ctrl */
    NULL,		/* timeout */
    NULL		/* outputv */
};

static etk_desc_t* etk_desc;
static char*         etk_port_arg;  /* must be freed */
static THREAD_T      etk_tid;	     /* The thread */
/* Lock to avoid erlang/otp to unload the driver while
** tk is still terminating.
** The lock is taken in Etk_Init when tk starts execute 
** and is released in Etk_Exit.
*/
static LOCK_T       etk_lock;

/* #define DEBUG  */
/* #define TRACE  */

#ifdef DEBUG
#define DPRINT(fmt, arg) printf(fmt, arg)
#else
#define DPRINT(fmt, arg)
#endif

#ifdef TRACE
#define TPRINT(fmt, a1)     printf(fmt, a1)
#define TPRINT2(fmt, a1,a2) printf(fmt, a1,a2)
#else
#define TPRINT(fmt, a1)
#define TPRINT2(fmt, a1, a2)
#endif

#define ETK_MESG_OK      0
#define ETK_MESG_CLOSED  1
#define ETK_MESG_ERROR   2

static int erl_closed = 0;

#ifndef __WIN32__
extern int matherr();
int *tclDummyMathPtr = (int *) matherr;
#endif

EXTERN int Tcl_LinkVar();
int (*tclDummyLinkVarPtr)() = Tcl_LinkVar;

/* The main window for the application.  If
 * NULL then the application no longer
 * exists. */
static Tk_Window mainWindow = NULL; 

static Tcl_Interp *interp = NULL; /* Interpreter for this application. */
static int synchronize;
static int withdrawn;
static char *name = NULL;
static char *display = NULL;
static char *geometry = NULL;
static char *colormap;
static char *visual;
static int rest = 0;

#ifdef __WIN32__
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
    {"-withdrawn", TK_ARGV_CONSTANT, (char*) 1, (char *) &withdrawn,
	 "Start erlkt main window withdrawn" },
    {"--", TK_ARGV_REST, (char *) 1, (char *) &rest,
	 "Pass all remaining arguments through to script"},
    {(char *) NULL, TK_ARGV_END, (char *) NULL, (char *) NULL,
	 (char *) NULL}
};


/* --------------------------------------------------------------------------

   TK DRIVER THREAD STUFF

 --------------------------------------------------------------------------*/

static int ThreadsInit()
{
    /* Dummy (but may be used for other thread packages) */
	return 0;
}

/* Block some signals to this thread */
static int ThreadBlockSignals()
{
#if defined(POSIX_THREADS)
    sigset_t new;

    sigemptyset(&new);
    sigaddset(&new, SIGINT);   /* block interrupt */
    sigaddset(&new, SIGCHLD);  /* block pipe signals */
    sigaddset(&new, SIGUSR1);  /* block user defined signal */
    return pthread_sigmask(SIG_BLOCK, &new, NULL);
#elif defined(SOLARIS_THREADS)
    sigset_t new;

    sigemptyset(&new);
    sigaddset(&new, SIGINT);   /* block interrupt */
    sigaddset(&new, SIGCHLD);  /* block pipe signals */
    sigaddset(&new, SIGUSR1);  /* block user defined signal */
    return thr_sigsetmask(SIG_BLOCK, &new, NULL);
#else
    return 0;
#endif
}

/* create a thread */
static int ThreadSpawn(func, param, tid)
void* (*func)(void*); void* param; THREAD_T* tid;
{
#if defined(POSIX_THREADS)
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    return pthread_create(tid, &attr, func, param);
#elif defined(SOLARIS_THREADS)
    return thr_create(NULL, 0, func, param,
		      THR_BOUND | THR_NEW_LWP | THR_DETACHED, tid);
#elif defined(__WIN32__)
    HANDLE h;
    DWORD ID;

    h = (HANDLE) _beginthreadex(NULL, 0, (LPTHREAD_START_ROUTINE) func, 
								(LPVOID)param, 0, &ID);
    if (h == INVALID_HANDLE_VALUE)
		return -1;
    CloseHandle(h);
	*tid = h;
	return 0;
#endif
}

/* wait for thread with id to terminate */
static int ThreadJoin(tid, result)
THREAD_T tid; void** result;
{
#if defined(POSIX_THREADS)
    return pthread_join(tid, result);
#elif defined(SOLARIS_THREADS)
    return thr_join(tid, NULL, result);
#elif defined(__WIN32__)
    WaitForSingleObject(tid, INFINITE); /* FIX ERRORS */
    *result = 0;
    CloseHandle(tid);
    return 0;
#endif
}


/* Brutal stop */
static int ThreadKill(tid)
THREAD_T tid;
{
#if defined(POSIX_THREADS)
    return pthread_kill(tid, SIGINT);
#elif defined(SOLARIS_THREADS)
    return thr_kill(tid, SIGINT);
#elif defined(__WIN32__)
    return -1;
#endif
}


/* Terminate current thread */
static void ThreadExit(result)
void* result;
{
#if defined(POSIX_THREADS)
    pthread_exit(result);
#elif defined(SOLARIS_THREADS)
    thr_exit(result);
#elif defined(__WIN32__)
    _endthreadex((unsigned) result);
#endif
}

static int LockInit(lockp)
LOCK_T* lockp;
{
#if defined(POSIX_THREADS)
    return pthread_mutex_init(lockp, NULL);
#elif defined(SOLARIS_THREADS)
    return mutex_init(lockp, USYNC_THREAD, NULL);
#elif defined(__WIN32__)
    InitializeCriticalSection(lockp);
	return 0;
#endif
}

static int LockDestroy(lockp)
LOCK_T* lockp;
{
#if defined(POSIX_THREADS)
    pthread_mutex_destroy(lockp);
#elif defined(SOLARIS_THREADS)
    mutex_destroy(lockp);
#elif defined(__WIN32__)
    DeleteCriticalSection(lockp);
	return 0;
#endif
}

static int Lock(lockp)
LOCK_T* lockp;
{
#if defined(POSIX_THREADS)
    pthread_mutex_lock(lockp);
#elif defined(SOLARIS_THREADS)
    mutex_lock(lockp);
#elif defined(__WIN32__)
    EnterCriticalSection(lockp);
	return 0;
#endif
}

static int Unlock(lockp)
LOCK_T* lockp;
{
#if defined(POSIX_THREADS)
    pthread_mutex_lock(lockp);
#elif defined(SOLARIS_THREADS)
    mutex_unlock(lockp);
#elif defined(__WIN32__)
    LeaveCriticalSection(lockp);
	return 0;
#endif
}

/* --------------------------------------------------------------------------

   MESSAGE STUFF

 --------------------------------------------------------------------------*/

static LOCK_T message_lock;
static etk_mesg_t* message_pool;
static char mesg_buffer[MESG_NO_FIXED*MESG_FIXED_SZ];

static void init_message_pool()
{
    char* ptr = mesg_buffer;
    etk_mesg_t* mesg;
    int i;

    LockInit(&message_lock);
    message_pool = NULL;
    for (i = 0; i < MESG_NO_FIXED; i++) {
	mesg = (etk_mesg_t*) ptr;
	ptr += MESG_FIXED_SZ;
	mesg->next = message_pool;
	message_pool = mesg;
    }
}

static etk_mesg_t* alloc_message(len)
int len;
{
    etk_mesg_t* mesg = NULL;

    if (len <= MESG_BUF_SZ) {
	Lock(&message_lock);
	if (message_pool != NULL) {
	    mesg = message_pool;
	    mesg->dynamic = 0;
	    message_pool = mesg->next;
	}
	Unlock(&message_lock);
    }
    if (mesg == NULL) {
	mesg = (etk_mesg_t*) malloc(MESG_HDR_SZ+len);
	if (mesg == NULL)
	    return NULL;
	mesg->dynamic = 1;
    }
    mesg->len = len;
    return mesg;
}

static void free_message(mesg)
etk_mesg_t* mesg;
{
    if (mesg->dynamic)
	free(mesg);
    else {
	Lock(&message_lock);
	mesg->next = message_pool;
	message_pool = mesg;
	Unlock(&message_lock);
    }
}

static etk_mesg_t* realloc_message(mesg, len)
etk_mesg_t* mesg; int len;
{
    if (len > mesg->len) {
	if (mesg->dynamic || (len > MESG_BUF_SZ)) {
	    free_message(mesg);
	    return alloc_message(len);
	}
    }
    mesg->len = len;
    return mesg;
}

/* --------------------------------------------------------------------------

   TK DRIVER STUFF

 --------------------------------------------------------------------------*/

static int Etk_SendMsg(s, mesg)
SOCKET s; etk_mesg_t* mesg;
{
    int n;
	
    n = sock_send(s, (char*) &mesg, sizeof(etk_mesg_t*), 0);
    if (n != sizeof(etk_mesg_t*)) {
	free_message(mesg);
	return -1;
    }
    return 0;
}

static etk_mesg_t* Etk_RecvMsg(s, err)
SOCKET s; int* err;
{
    etk_mesg_t* mesg;
    int n;

    if ((n = sock_recv(s,(char*)&mesg,sizeof(etk_mesg_t*),0)) == 0) {
	*err = ETK_MESG_CLOSED;
	return NULL;
    }
    else if (n != sizeof(etk_mesg_t*)) {
	*err = ETK_MESG_ERROR;
	return NULL;
    }
    *err = ETK_MESG_OK;
    return mesg;
}

/* Create socket pair i.e something like a bidirectional pipe */
static int sockpair(SOCKET pair[2])
{
    struct sockaddr_in a;
    SOCKET s0 = INVALID_SOCKET;
    SOCKET s1 = INVALID_SOCKET;
    SOCKET s2 = INVALID_SOCKET;
    int len = sizeof(a);
    int on = 1;

    if ((s0 = sock_open(AF_INET, SOCK_STREAM, 0)) == INVALID_SOCKET)
	goto error;

    memset((char*) &a, 0, sizeof(a));
    a.sin_family = AF_INET;
    a.sin_addr.s_addr = sock_htonl(INADDR_LOOPBACK);
    a.sin_port = sock_htons(0);

    if (sock_bind(s0,(struct sockaddr*) &a,sizeof(a)) == SOCKET_ERROR)
	goto error;

    if (sock_name(s0, (struct sockaddr*) &a, &len) == SOCKET_ERROR)
	goto error;

    if (sock_listen(s0, 1) == SOCKET_ERROR)
	goto error;

    if ((s1 = sock_open(AF_INET, SOCK_STREAM, 0)) == INVALID_SOCKET)
	goto error;

    if (sock_connect(s1, (struct sockaddr*) &a, len) == SOCKET_ERROR)
	goto error;

    if ((s2 = sock_accept(s0, (struct sockaddr*) &a, &len)) == INVALID_SOCKET)
	goto error;
    sock_close(s0);
    /* The TCP_NODELAY option are needed */ 
    sock_setopt(s1, IPPROTO_TCP, TCP_NODELAY, (char*) &on, sizeof(int));
    sock_setopt(s2, IPPROTO_TCP, TCP_NODELAY, (char*) &on, sizeof(int));
    SET_NONBLOCKING(s1);
    SET_NONBLOCKING(s2);
    pair[0] = s1;
    pair[1] = s2;
    return 0;

error:
    if (s0 != INVALID_SOCKET)
	sock_close(s0);
    if (s1 != INVALID_SOCKET)
	sock_close(s1);
    if (s2 != INVALID_SOCKET)
	sock_close(s2);
    return -1;
}


/*
** Must map exit => thr_exit
*/
void Etk_Exit(code, msg)
int code; char* msg;
{
    etk_mesg_t* mesg;
    int reason;
    DPRINT("Etk_Exit: %s\n\r", msg);

    if (etk_port_arg != NULL) {
	free(etk_port_arg);
	etk_port_arg = NULL;
    }
    sock_shutdown(etk_desc->s_tk,SD_SEND);
    while((mesg = Etk_RecvMsg(etk_desc->s_tk,&reason)) != NULL)
	free_message(mesg);
    sock_close(etk_desc->s_tk);
    erl_closed = 1;
    if (mainWindow != NULL)
	Tk_DestroyWindow(mainWindow);
    if (interp != NULL)
	Tcl_DeleteInterp(interp);
    Unlock(&etk_lock);   /* locked while executing */
    /* A tiny hole */
    ThreadExit(code);
}


/*
** Send argv info to erlang as:
**
**  32-bit-length  data
*/

static int SendToErlang(cmd, clientData, interp, argc, argv)
int cmd; ClientData clientData; Tcl_Interp* interp; int argc; char** argv;
{
    int i;
    int len;
    u_int8* ptr;
    etk_mesg_t* mesg;

    if (erl_closed)
	return TCL_OK;

    /* calc total length */
    len = 0;
    for (i = 1; i < argc; i++)
	len += strlen(argv[i]) + 4;

    mesg = alloc_message(len+4);
    ptr = mesg->data;

    PUT_INT16(ptr, 0); 
    ptr += 2;
    PUT_INT16(ptr, cmd);
    ptr += 2;

    for (i = 1; i < argc; i++) {
	int n = strlen(argv[i]);
	PUT_INT32(ptr, n); ptr += 4;
	memcpy(ptr, argv[i], n);
	ptr += n;
    }
    Etk_SendMsg(etk_desc->s_tk, mesg);
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
**
*/

static void ErlangInputProc(data, mask)
ClientData data; int mask;
{
    int code;
    int reason;
    int len;
    int seq0;
    u_int8* p_pos;
    u_int8* p_end;
    etk_mesg_t* mesg;

    if ((mesg = Etk_RecvMsg(etk_desc->s_tk,&reason)) == NULL) {
	if (reason == ETK_MESG_CLOSED)
	    Etk_Exit(0, "eof");
	else
	    Etk_Exit(1, "input error");
    }
    TPRINT2("ErlangInputProc: message(%s), len = %d\n\r", 
		mesg->dynamic?"dynaic":"fixed", mesg->len);

    p_pos = mesg->data;
    p_end = p_pos + mesg->len;
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
	else if (seq0 != seq) {
	    free_message(mesg);
	    Etk_Exit(1, "bad sequence number");
	}

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
		if (offs >= OPT_MAX) {
		    free_message(mesg);
		    Etk_Exit(1, "bad option offset");
		}
		argv[i] = tkopt[offs];
		break;
	    case TK_OP_STRING:
		offs = GET_INT24(p);
		if (offs >= STR_MAX) {
		    free_message(mesg);
		    Etk_Exit(1, "bad string offset");
		}
		argv[i] = tkstr[offs];
		break;
	    default:
		free_message(mesg);
		Etk_Exit(1, "bad option type");
	    }
	}
	Tcl_ResetResult(interp);
	switch(Etk_Execute(cmd,(ClientData)mainWindow,interp,argc,argv)) {
	case TCL_BREAK:
	    free_message(mesg);
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
    len = strlen(interp->result);
    mesg = realloc_message(mesg, len+4);
    PUT_INT16(mesg->data, seq0);
    PUT_INT16(mesg->data+2, code);
    memcpy(mesg->data+4, interp->result, len);
    Tcl_ResetResult(interp);
    Etk_SendMsg(etk_desc->s_tk, mesg);
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

static void TkCreateWinEventSource()
{
    Tcl_CreateEventSource(DisplaySetupProc, DisplayCheckProc,
			  (ClientData) NULL);
    TkWinXInit((HINSTANCE)etk_driver_entry.handle);
}

#endif

/*
** Port main loop
*/

static void Etk_Main(argc, argv)
int argc; char** argv;
{
    /* Initialize */
    int code;
    char* av[20];
    int   ac;
    char* class;

    erl_closed = 0;
    interp = Tcl_CreateInterp();

    synchronize = 0;
    name = display = geometry = colormap = visual = NULL; 

    if (Tk_ParseArgv(interp, (Tk_Window) NULL, &argc, argv, argTable, 
		     TK_ARGV_NO_DEFAULTS) != TCL_OK) {
	Etk_Exit(1, interp->result);
    }

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
    if (code != TCL_OK)
	Etk_Exit(1, interp->result);
    mainWindow = Tk_MainWindow(interp);

    Tcl_CreateCommand(interp, "event", 
		      (Tcl_CmdProc*) ErlangEvent, 
		      (ClientData) 0, (Tcl_CmdDeleteProc*) NULL);

    Tcl_CreateCommand(interp, "invoke", 
		      (Tcl_CmdProc*) ErlangInvoke, 
		      (ClientData) 0, (Tcl_CmdDeleteProc*) NULL);

    Tcl_CreateCommand(interp, "bgerror", 
		      (Tcl_CmdProc*) tkError, 
		      (ClientData) 0, (Tcl_CmdDeleteProc*) NULL);

    Tcl_CreateCommand(interp, "tkScreenChanged", 
		      (Tcl_CmdProc*) tkScreenChanged, 
		      (ClientData) 0, (Tcl_CmdDeleteProc*) NULL);
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

	if (Tk_WmCmd((ClientData) mainWindow, interp, 4, av) != TCL_OK)
	    Etk_Exit(1, interp->result);
    }

    if (withdrawn) {
	av[0] = "wm";
	av[1] = "withdraw";
	av[2] = ".";
	av[3] = NULL;
	if (Tk_WmCmd((ClientData) mainWindow, interp, 3, av) != TCL_OK) {
	    Etk_Exit(1, interp->result);
	}
    }
#ifdef __WIN32__
    TkCreateWinEventSource();
    Tcl_CreateFileHandler(Tcl_GetFile((ClientData)etk_desc->s_tk,
				      TCL_WIN_SOCKET),
			  TK_READABLE, (Tcl_FileProc*) ErlangInputProc,
			  (ClientData) 0);
#else
    TkCreateXEventSource();
    Tcl_CreateFileHandler(Tcl_GetFile((ClientData)etk_desc->s_tk,
				      TCL_UNIX_FD),
			  TK_READABLE, (Tcl_FileProc*) ErlangInputProc,
			  (ClientData) 0);
#endif

    while (Tk_GetNumMainWindows() > 0) {
        Tcl_DoOneEvent(0);
    }
    Etk_Exit(0, "done");
}


#define ETK_MAX_ARGV 10

#define IS_BLANK(c) ( ((c) <= ' ') && \
                      (((c) == ' ') || ((c) == '\t') || ((c) == '\n')))

/* Create argument vectors etc, call Etk_Main */
static void* Etk_Init(arg)
void* arg;
{
    char* argv[ETK_MAX_ARGV+1];
    char* ptr;
    int argc;
    int c;

    ThreadBlockSignals();
    Lock(&etk_lock);   /* lock while executing */

    argv[0] = "etk";
    argc = 1;
    ptr = (char*) arg;

    while(argc < ETK_MAX_ARGV) {
	c = *ptr;
	while(IS_BLANK(c))
	    c = *++ptr;  /* skip leading blanks */
	if (c == 0)
	    break;
	argv[argc] = ptr;   /* set argument */
	c = *++ptr;
	while((c != 0) && !IS_BLANK(c))
	    c = *++ptr;
	argc++;
	if (c == 0)
	    break;
	else
	    *ptr++ = 0;
    }
    argv[argc] = 0;
    Etk_Main(argc, argv);
    return 0;
}

#ifdef __WIN32__
/* Called from tclWinSocket.c !!! */

HINSTANCE Etk_GetInstance()
{
    return (HINSTANCE) etk_driver_entry.handle;
}
#endif


/* since tk is not reentrant (its compiled so) we may only have
   one copy of tk active !!!
*/

/* Loadable driver */ 
#ifdef LOADABLE
int DRIVER_INIT(etk_drv)(void* handle)
{
    etk_driver_entry.handle = handle;
    etk_driver_entry.driver_name = "etk_drv";
    etk_driver_entry.finish = etk_finish;
    etk_driver_entry.init = etk_init;
    etk_driver_entry.start = etk_start;
    etk_driver_entry.stop = etk_stop;
    etk_driver_entry.output = etk_command;
#ifdef __WIN32__
    etk_driver_entry.ready_input = etk_event;
#else
    etk_driver_entry.ready_input = etk_input;
#endif
    etk_driver_entry.ready_output = etk_output;
    etk_driver_entry.timeout = NULL;
    etk_driver_entry.outputv = NULL;
    etk_driver_entry.control = NULL;
    return (int) &etk_driver_entry;
}
#endif

static int etk_init()
{
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
    LockInit(&etk_lock);
    ThreadsInit();
    init_message_pool();

    if ((etk_desc = malloc(sizeof(etk_desc_t))) == NULL)
	goto error;
    etk_desc->port = -1;
    etk_desc->s     = INVALID_SOCKET;
    etk_desc->s_tk  = INVALID_SOCKET;
    etk_desc->event = INVALID_EVENT;
    etk_desc->event_mask = 0;

    return 0;

error:
#ifdef __WIN32__
    (*Ws2.WSACleanup)();
#endif
	/* The driver should be removed if not valid */
    remove_driver_entry(&etk_driver_entry);
    return -1;
}

static int etk_finish()
{
    Lock(&etk_lock);
    Unlock(&etk_lock);
    LockDestroy(&etk_lock);
    LockDestroy(&message_lock);
    if (etk_desc != NULL)
	free(etk_desc);
    return 0;
}

/* start the etk main thread */
static long etk_start(port, args)
long port; char* args;
{
    int len;
    etk_desc_t* desc = etk_desc;
    SOCKET pair[2];

    if (desc->port != -1)
	return -1;

    /* make a local copy of args (since we will patch it up) */
    len = strlen(args);
    if ((etk_port_arg = (char*)malloc(len+1)) == NULL) {
	DPRINT("cound not allocate\n\r", 0);
	return -1;
    }
    strcpy(etk_port_arg, args);
    if (sockpair(pair) < 0) {
	free(etk_port_arg);
	return -1;
    }
    desc->s = pair[0];
    desc->s_tk = pair[1];

    /* Start TK thread */
    if (ThreadSpawn(Etk_Init, (void*) etk_port_arg, &etk_tid) < 0) {
	DPRINT("could not spawn thread\n\r", 0);
	sock_close(desc->s);
	sock_close(desc->s_tk);
	free(etk_port_arg);
	return -1;
    }
    desc->port = port;
    desc->event = sock_create_event(desc);
    desc->event_mask = 0;
    sock_select(desc, (FD_READ | FD_CLOSE), 1);
#ifdef __WIN32__
    driver_select(desc->port, (int)desc->event, DO_READ, 1);
#endif
    return (long) desc;
}

/* stop the etk main thread (close pipe, should terminate thread?) */
static int etk_stop(desc)
etk_desc_t* desc;
{
    etk_mesg_t* mesg;
    int reason;

#ifdef __WIN32__
    driver_select(desc->port, (int)desc->event, DO_READ, 0);
#endif
    sock_select(desc, (FD_READ | FD_CLOSE), 0);
    sock_close_event(desc->event);
    desc->port = -1;

    sock_shutdown(desc->s, SD_SEND);
    while((mesg = Etk_RecvMsg(desc->s,&reason)) != NULL)
	free_message(mesg);
    sock_close(desc->s);

    /* ThreadJoin(etk_tid, &result) */
    /* Since we cant destroy locks here tell start to doit next round */
    return 0;
}

/* put commands from erlang on tk input queue */
static int etk_command(desc, buf, len)
etk_desc_t* desc; char* buf; int len;
{
    etk_mesg_t* mesg;

    /* allocate buffer */
    mesg = alloc_message(len);
    memcpy(mesg->data, buf, len);

    Etk_SendMsg(desc->s, mesg);
    return 1;
}

#ifdef __WIN32__

static int etk_event(desc, event)
etk_desc_t* desc; HANDLE event;
{
    WSANETWORKEVENTS netEv;
    int do_close = 1;

    TPRINT("etk_event\n", 0);
    if ((*Ws2.WSAEnumNetworkEvents)(desc->s, desc->event,&netEv) != 0)
	return -1;
    if (netEv.lNetworkEvents == 0)  /* NOTHING */
	return 0;

    if (netEv.lNetworkEvents & FD_READ) {
	do_close = 0;
	etk_input(desc, event);
    }
    if (netEv.lNetworkEvents & FD_WRITE) {
	do_close = 0;
	etk_output(desc, event);
    }
    if (do_close && (netEv.lNetworkEvents & FD_CLOSE)) {
	/* Handle close from TK */
	driver_failure(desc->port, 0);
	return -1;
    }
    return 0;
}

#endif

/* called when tk has formated messages to send or results */
static int etk_input(desc, event)
etk_desc_t* desc; HANDLE event;
{
    etk_mesg_t* mesg;
    int reason;

    if ((mesg = Etk_RecvMsg(desc->s,&reason)) != NULL) {
	TPRINT2("etk_input: message(%s) len = %d\n\r", 
		mesg->dynamic?"dynaic":"fixed", mesg->len);    
	driver_output(desc->port, (char*) mesg->data, mesg->len);
	free_message(mesg);
	return 1;
    }
    else if (reason == ETK_MESG_CLOSED) {
	DPRINT("etk_input: tk closed\n\r", 0);
	driver_failure(desc->port, 0);
	return 0;
    }
    else {
	DPRINT("etk_input: tk failure\n", 0);
	driver_failure(desc->port, -1);
	return -1;
    }
}

/* called when tk has processed a command and may recive more */
/* FIXME: when sender socket is full, we get a non-blocking
** condition. We must save the number of none sent events
** and send them when the socket is ready for write 
*/
static int etk_output(desc, event)
etk_desc_t* desc; HANDLE event;
{
    /* we may have to send more pipe wakeup's */
    /* messages shold have been put on the queue already */
    return 1;
}

