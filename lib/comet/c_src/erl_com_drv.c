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
 *
 */

/*
 * Purpose: Erlang-COM connection, enable erlang to call COM objects
 */

/*
 * This source file will compile to either a port-driver or a port-program,
 * depending on the predefined symbol PORT_DRIVER (0 or 1)
 * They operate quite a bit differently internally, see documentation
 */

#ifndef __WIN32__
#error "comet and COM is only available under Windows!"
#endif 

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#define COBJMACROS 1
#include <windows.h>
#include <oleauto.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <process.h>
#include <io.h>
#define write _write
#define open _open
#define close _close
#include <errno.h>
#include <sys/types.h>
#include <fcntl.h>

#ifdef DEBUG
#include <assert.h>
#define ASSERT(X) assert(X)
#else
#define ASSERT(X)
#endif

#if PORT_DRIVER
#include "erl_driver.h"
#else
#define driver_alloc(n) malloc(n)
#define driver_realloc(p, n) realloc(p, n)
#define driver_free(p) free(p)
#endif

#include "ei.h"
#include "ei_format.h"

#include "erl_com_drv.h"

/*
 ** State structure
 */

#if !PORT_DRIVER
typedef int ErlDrvPort;
typedef void* ErlDrvData;
#endif

/* state for erlang thread, one of these for each thread */
typedef struct erl_com_thread_data_TAG {
    HANDLE emulator_event;	/* event used for driver_select */
    HANDLE event;		/* event used by driver to start thread */
    unsigned thread;		/* the thread handle (from _beginthreadex) */
    int op;			/* operation */
    int thread_n;		/* index of thread in erl_data */
    ei_x_buff x;		/* buffer */
    int stop;			/* flag to stop a thread */
    void** com_i_ptrs;		/* the interfaces held in the thread */
    int n_com_i_ptrs;
} erl_com_thread_data;

/* the threads data is stored globally */
#define N_MAX_THREADS (MAXIMUM_WAIT_OBJECTS - 3) 
/* note that the max threads is only needed in port_programs, (since
 * driver_select uses a special hack that Björn G implemented),
 * but let's be reasonable */

static erl_com_thread_data* erl_data[N_MAX_THREADS]; /* threads data */
static int n_threads;			/* current # of threads (and data) */


/* before the DLL is unloaded, we must be 100% certain that 
 * no threads are running within it, therefore we wait
 * for all running threads (a.k.a. join) in driver_finish */
static HANDLE wait_threads[N_MAX_THREADS]; /* threads to wait for in finish */
static n_wait_threads;

/*
 ** Interface routines
 */
static ErlDrvData drv_erl_com_drv_start(ErlDrvPort port, char *buff);
static void drv_erl_com_drv_stop(ErlDrvData handle);
static void drv_erl_com_drv_output(ErlDrvData handle, char *buff, int bufflen);
#if PORT_DRIVER
static void drv_erl_com_drv_finish(void);
static void drv_erl_com_drv_readyoutput(ErlDrvData h, ErlDrvEvent event);
#endif

static void return_int(int i, erl_com_thread_data* data);

static void init_dates();

static erl_com_thread_data* new_erl_com_thread(ErlDrvPort port);

#define NEW_THREAD_SPECIAL_N 255

#if PORT_DRIVER
/*
 ** The driver struct
 */
ErlDrvEntry drv_erl_com_drv_driver_entry = {
    NULL,			/* F_PTR init, N/A */
    drv_erl_com_drv_start,	/* L_PTR start, called when port is opened */
    drv_erl_com_drv_stop,	/* F_PTR stop, called when port is closed */
    drv_erl_com_drv_output,	/* F_PTR output, called when erlang has sent */
    NULL,			/* F_PTR ready_input, called when input descriptor ready */
    drv_erl_com_drv_readyoutput,/* F_PTR ready_output, called when output descriptor ready */
    "erl_com_drv",		/* char *driver_name, the argument to open_port */
    drv_erl_com_drv_finish,	/* F_PTR finish, called when unloaded */
    NULL,			/* F_PTR control, port_command callback */
    NULL,			/* F_PTR timeout, reserved */
    NULL			/* F_PTR outputv, reserved */
};

/*
 ** Driver initialization routine
 */
DRIVER_INIT(erl_com_drv)
{
    n_threads = 0;
    init_dates();
    return &drv_erl_com_drv_driver_entry;
}
#else

/*
 ** Main
 */

/* environment for the input_thread, which reads and signals pipe input */
typedef struct input_env_TAG {
    HANDLE in_h;		/* handle of input thread */
    HANDLE event;		/* event that the input thread waits on */
    HANDLE input_event;		/* event for main thread to wait on */
    int op;			/* op read */
    int thread_n;		/* thread index */
    char* buff;			/* input buffer */
    int bufflen;		/* current length of input data (<= buffsz) */
    int buffsz;			/* allocated size of input buffer */
} input_env;

/* input thread, it never stops
 * we must use a separate thread for this, since win32 can't
 * wait on pipes with WaitForMultipleObjects */
static DWORD WINAPI input_thread(LPVOID par)
{
    input_env* ienv = (input_env*)par;
    //DebugBreak();
    for (;;) {
	/* read packet size (we use packet 4 for this port) */
	int n, m;
	if (!ReadFile(ienv->in_h, &n, 4, &m, NULL)) {
	    fprintf(stderr, "erl_com: couldn't read packet size \n");
	    exit(1); /*!*/
	}
	n = ntohl(n);		/* erlang is big-endian */
	if (ienv->buffsz < n) {
	    ienv->buffsz = n * 2 + 100;
	    ienv->buff = driver_realloc(ienv->buff, ienv->buffsz);
	}
	if (ienv->buff == NULL) {
	    fprintf(stderr, "erl_com: couldn't allocate buffer (size %d) \n", ienv->buffsz);
	    exit(1); /*!*/
	}
	/* read packet */
	if (!ReadFile(ienv->in_h, ienv->buff, n, &m, NULL)) {
	    fprintf(stderr, "erl_com: couldn't read packet (wanted %d, got %d) \n", n, m);
	    exit(1); /*!*/
	}
	ienv->bufflen = n;
	ienv->op = ienv->buff[0];
	ienv->thread_n = ienv->buff[1];
        SetEvent(ienv->input_event);
	WaitForSingleObjectEx(ienv->event, INFINITE, FALSE);
    }
}

int main(int argc, char* argv[])
{
    int waiting_on[N_MAX_THREADS]; /* flags active threads */
    UINT input_thread_h;	/* thread handle for the input thread */
    input_env in_env;		/* state for the input thread */
    int i, result, m;
    unsigned tid; 
    /* get handles for the bidirectional pipe erlang uses
     * to communicate with the port */
    HANDLE out_h = GetStdHandle(STD_OUTPUT_HANDLE);
    in_env.in_h = GetStdHandle(STD_INPUT_HANDLE);
    in_env.buffsz = 1;
    in_env.buff = driver_alloc(1);
    if (in_env.in_h == INVALID_HANDLE_VALUE || out_h == INVALID_HANDLE_VALUE) {
	return 1;
    }
    /* init and start the input thread */
    in_env.input_event = CreateEvent(NULL, FALSE, FALSE, NULL);
    in_env.event = CreateEvent(NULL, FALSE, FALSE, NULL);
    if (in_env.input_event == NULL || in_env.event == NULL) {
	return 1;
    }
    in_env.buff = driver_alloc(4);
    in_env.bufflen = in_env.buffsz = 4;
    if (in_env.buff == NULL) {
	return 1;
    }
    input_thread_h = _beginthreadex(NULL, 0, input_thread, &in_env, 0, &tid);
    if (input_thread_h == 0) {
	return 1;
    }
    /* some extra init */
    result = 0;
    n_threads = 1;
    init_dates();
    erl_data[0] = new_erl_com_thread(0);
    waiting_on[0] = FALSE;
    
    /* we have to do the select ourselves, driver_select is only 
     * available in driver-ports */
    for (;;) {
	int n = 0;
	/* prepare the wait array */
	HANDLE wait_events[N_MAX_THREADS+1];
	wait_events[n++] = in_env.input_event;
	for (i = 0; i < n_threads; ++i) {
	    if (erl_data[i] != NULL && waiting_on[i]) {
		wait_events[n++] = erl_data[i]->emulator_event;
	    }
	}
	/* select in win32 */
	n = WaitForMultipleObjects(n, wait_events, FALSE, INFINITE);
	//DebugBreak();
	n -= WAIT_OBJECT_0;
	if (n == 0) {		/* this is the input_thread */
	    char* tmpbuff; int tmpbufflen, tmpbuffsz;
	    /* check for the special operation of creating a new thread */
	    if (in_env.op == com_NewThread) {
		int t;
		//DebugBreak();
		if (n_threads >= N_MAX_THREADS) {
		    t = -1;
		} else {
		    erl_data[n_threads] = new_erl_com_thread(0);
		    waiting_on[n_threads] = FALSE;
		    t = n_threads;
		    ++n_threads;
		}
		{
		    char buf[100]; int index = 0;
		    ei_encode_version(buf, &index);
		    ei_encode_tuple_header(buf, &index, 2);
		    ei_encode_long(buf, &index, NEW_THREAD_SPECIAL_N);
		    ei_encode_long(buf, &index, t);
		    n = htonl(index);
		    if (!WriteFile(out_h, &n, sizeof(n), &m, NULL)) {
			fprintf(stderr, "erl_com: couldn't write packet size (newthread)\n");
			exit(1);
		    }
		    if (!WriteFile(out_h, buf, index, &n, NULL)) {
			fprintf(stderr, "erl_com: couldn't write packet (wanted %d, got %d) \n",
				index, n);
			exit(1);
		    }
		}
	    } else if (in_env.op == com_Quit) {
		break;
	    } else {		/* a call from erlang to a com_thread */
		/* check that the com-thread is available (if not, the gen_server 
		 * is ill) */
		if (erl_data[in_env.thread_n] == NULL || waiting_on[in_env.thread_n])
		    return 1;
		i = in_env.thread_n;
		waiting_on[i] = TRUE;
		tmpbuff = erl_data[i]->x.buff;
		tmpbufflen = erl_data[i]->x.index;
		tmpbuffsz = erl_data[i]->x.buffsz;
		erl_data[i]->x.buff = in_env.buff;
		erl_data[i]->x.index = in_env.bufflen;
		erl_data[i]->x.buffsz = in_env.buffsz;
		in_env.buff = tmpbuff;
		in_env.bufflen = tmpbufflen;
		in_env.buffsz = tmpbuffsz;
		/* start the com_thread */
		SetEvent(erl_data[i]->event);
	    }
	    /* tell the input_thread to keep on listening */
	    SetEvent(in_env.event);
	} else {
	    /* a thread has done its work, let's write it back to erlang */
	    for (i = 0; i < n_threads; ++i) {
		if (wait_events[n] == erl_data[i]->emulator_event)
		    break;
	    }
	    if (i > n_threads) {
		fprintf(stderr, "erl_com: thread done, couldn't find thread %d\n", i);
		exit(1);
	    }
	    waiting_on[i] = FALSE;
	    /* write packet size (packet 4) */
	    n = htonl(erl_data[i]->x.index); /* erlang is big-endian */
	    if (!WriteFile(out_h, &n, sizeof(n), &m, NULL)) {
		fprintf(stderr, "erl_com: couldn't write packet size (res) \n");
		exit(1);
	    }
	    if (!WriteFile(out_h, erl_data[i]->x.buff, erl_data[i]->x.index, &n, NULL)) {
		fprintf(stderr, "erl_com: couldn't write packet (res) (wanted %d, got %d) \n", 
			erl_data[i]->x.index, n);
		exit(1);
	    }
	}
    }
    return result;
}

#endif

/*
 ** Driver interface routines
 */

/*
 ** Open a port
 */

static ErlDrvData drv_erl_com_drv_start(ErlDrvPort port, char *buff)
{
    //DebugBreak();
    erl_data[n_threads] = new_erl_com_thread(port);
    ++n_threads;
    return (ErlDrvData)port;
}

/* create and start a new com_thread */
static unsigned _stdcall erl_com_thread_main(void*);

static erl_com_thread_data* new_erl_com_thread(ErlDrvPort port)
{
    erl_com_thread_data* ret = (erl_com_thread_data*)driver_alloc(sizeof(erl_com_thread_data));
    unsigned tid;

    ret->event = CreateEvent(NULL, FALSE, FALSE, NULL);  
    ret->emulator_event = CreateEvent(NULL, FALSE, FALSE, NULL);
#if PORT_DRIVER
    driver_select(port, ret->emulator_event, DO_WRITE, 1);
#endif
    ret->x.buff = NULL;
    ret->x.index = 0;
    ret->x.buffsz = 0;
    ret->stop = FALSE;
    ret->n_com_i_ptrs = 0;
    ret->com_i_ptrs = driver_alloc(sizeof(void*));
    ret->com_i_ptrs[0] = 0;
    //DebugBreak();
    ret->thread = _beginthreadex(NULL, 0, erl_com_thread_main, ret, 0, &tid);
    return ret;
}

#if PORT_DRIVER
/*
 ** Close a port
 */
static void drv_erl_com_drv_stop(ErlDrvData handle)
{
    ErlDrvPort port = (ErlDrvPort) handle;
    int i;
    n_wait_threads = 0;
    //DebugBreak();
    for (i = 0; i < n_threads; ++i) {
	erl_com_thread_data* data = erl_data[i];
	if (data != NULL) {
	    driver_select(port, data->emulator_event, DO_WRITE, 0);
	    data->stop = TRUE;
	    wait_threads[n_wait_threads++] = (HANDLE)data->thread;
	    SetEvent(data->event);
	}
    }
}

/*
 ** Ready to send input
 */

static void drv_erl_com_drv_readyoutput(ErlDrvData handle, ErlDrvEvent event)
{
    erl_com_thread_data* data;
    int i;
    for (i = 0; i < n_threads; ++i) {
	data = erl_data[i];
	if (data != NULL && data->emulator_event == event)
	    break;
    } 
    if (i == n_threads) 
	return;
    driver_output((ErlDrvPort)handle, data->x.buff, data->x.index);
}

/*
 ** Data sent from erlang to port.
 */
static void drv_erl_com_drv_output(ErlDrvData handle, char *buff, int bufflen)
{
    ErlDrvPort port = (ErlDrvPort)handle;
    int op = buff[0];
    int i, thread = buff[1];
    erl_com_thread_data* data;
    //DebugBreak();
    if (op == com_NewThread) {
        char b[100]; int index = 0;
	for (i = 0; i < n_threads; ++i) {
	    if (erl_data[i] == NULL)
		break;
	}
	if (i == N_MAX_THREADS) 
	    return;
	data = erl_data[i] = new_erl_com_thread(port);
	if (i == n_threads)
	    ++n_threads;
	index = 0;
	ei_encode_version(b, &index);
	ei_encode_tuple_header(b, &index, 2);
	ei_encode_long(b, &index, NEW_THREAD_SPECIAL_N);
	ei_encode_long(b, &index, i);
	driver_output(port, b, index);
    } else {
	if (thread >= 0 && thread < n_threads) {
	    data = erl_data[thread];
	    data->x.buffsz = data->x.index = bufflen;
	    data->x.buff = driver_realloc(data->x.buff, bufflen);
	    memcpy(data->x.buff, buff, bufflen);
	    SetEvent(data->event);
	}
    }
}
#endif

void perform_op(erl_com_thread_data* data);

/* the simple com_thread, it uses the two event to sync with the emulator */
static unsigned _stdcall erl_com_thread_main(void* par)
{
    int i;
    erl_com_thread_data* data = (erl_com_thread_data*)par;
    CoInitialize(NULL);
    for (;;) {
	WaitForSingleObjectEx(data->event, INFINITE, FALSE);
	if (data->stop) break;
	perform_op(data);
	SetEvent(data->emulator_event);
    }
    driver_free(data->com_i_ptrs);
    CoUninitialize();
    CloseHandle(data->event);
    CloseHandle(data->emulator_event);
    driver_free(data);
    for (i = 0; i < n_threads; ++i) {
	if (erl_data[i] == data)
	    erl_data[i] = NULL;
    }
    return 0;
}

/*
 ** Driver unloaded
 */
static void drv_erl_com_drv_finish(void)
{
    /* make 100% sure that no threads are running in the DLL */
    WaitForMultipleObjects(n_wait_threads, wait_threads, TRUE, INFINITE);
}

/*
 ** Internal helpers
 */

/* add to interface list */
static int add_interface(erl_com_thread_data* d, void* p)
{
    int f = (int)d->com_i_ptrs[0];  // OK, this is ugly!
    if (f != 0) {
	d->com_i_ptrs[0] = d->com_i_ptrs[f];
    } else {
	f = d->n_com_i_ptrs+1;
	d->com_i_ptrs = driver_realloc(d->com_i_ptrs, (d->n_com_i_ptrs+2)*sizeof(void*));
	d->n_com_i_ptrs++;
    }
    d->com_i_ptrs[f] = p;
    return f;
}

/* create com object, and add to interface list */
static HRESULT create_obj(erl_com_thread_data* d, BSTR clsid, BSTR refiid, CLSCTX clsctx, int* i)
{
    LPVOID p;
    CLSID clsid_;
    IID riid;
    HRESULT r = CLSIDFromString(clsid, &clsid_);
    if (FAILED(r)) {
	r = CLSIDFromProgID(clsid, &clsid_);
    }
    if (SUCCEEDED(r)) {
	if (SysStringLen(refiid) == 0) {
	    riid = IID_IUnknown;
	} else {
	    r = IIDFromString(refiid, &riid);
	}
    }
    *i = -1;
    if (SUCCEEDED(r)) {
	r = CoCreateInstance(&clsid_, NULL, clsctx, &riid, &p);
	if (SUCCEEDED(r))
	    *i = add_interface(d, p);
    }
    return r;
}

/* get and bind to com object, and add to interface list */
static HRESULT get_obj(erl_com_thread_data* d, BSTR name, BSTR refiid, int* i)
{
    LPVOID p;
    IID riid;
    HRESULT r;

    *i = -1;
    if (SysStringLen(name) == 0) {
	r = IIDFromString(refiid, &riid);
	if (FAILED(r)) 
	    r = CLSIDFromProgID(refiid, &riid);
	if (SUCCEEDED(r))
	    r = GetActiveObject(&riid, NULL, (IUnknown**)&p);
    } else {
	if (SysStringLen(refiid) == 0)
	    riid = IID_IUnknown;
	else
	    r = IIDFromString(refiid, &riid);
	if (SUCCEEDED(r)) {
	    BIND_OPTS bo;
	    bo.cbStruct = sizeof(BIND_OPTS);
	    bo.grfFlags = 0;
	    bo.grfMode = STGM_READWRITE;
	    bo.dwTickCountDeadline = 0;
	    r = CoGetObject(name, &bo, &riid, &p);
	}
    }
    if (SUCCEEDED(r))
	*i = add_interface(d, p);
    return r;
}

/* call com release (and link to free list) */
static HRESULT release_(erl_com_thread_data* d, int i)
{
    HRESULT r = IUnknown_Release((LPUNKNOWN)d->com_i_ptrs[i]);
    d->com_i_ptrs[i] = d->com_i_ptrs[0];
    *(int*)&d->com_i_ptrs[0] = i;   // ugly in a way, beautyful in another
    return r;
}

/* call com query_interface, and add to interface list */
static HRESULT query_interface(erl_com_thread_data* d, int i, BSTR iid, int* ni)
{
    LPVOID p;
    IID iid_;
    HRESULT r = IIDFromString(iid, &iid_);
    *ni = -1;
    if (SUCCEEDED(r)) {
	r = IUnknown_QueryInterface((LPUNKNOWN)d->com_i_ptrs[i], &iid_, &p);
	if (SUCCEEDED(r)) {
	    *ni = add_interface(d, p); 
	}
    }
    return r;
}

/* convert ascii to UNICODE */
static OLECHAR* get_ole_str(const char* s)
{
    int n = MultiByteToWideChar(CP_ACP, 0, s, -1, NULL, 0);
    OLECHAR* r, * oles = (OLECHAR*)driver_alloc(n*sizeof(OLECHAR));
    MultiByteToWideChar(CP_ACP, 0, s, -1, oles, n);
    r = SysAllocString(oles);
    driver_free(oles);
    return r;
}

/* convert UNICODE to ascii */
static char* get_erl_str(OLECHAR* oles)
{
    int n = WideCharToMultiByte(CP_ACP, 0, oles, -1, NULL, 0, NULL, NULL);
    char* s = (char*)driver_alloc(n);
    WideCharToMultiByte(CP_ACP, 0, oles, -1, s, n, NULL, NULL);
    return s;
}

static double date_1970;

static void init_dates()
{
    UDATE s;
    memset(&s, '\0', sizeof(s));
    s.st.wYear = 1970;
    s.st.wMonth = 1;
    s.st.wDay = 1;
    VarDateFromUdate(&s, 0, &date_1970);
}

static char* get_erl_str(OLECHAR* oles);

static int ei_x_encode_and_free_bstr(ei_x_buff* x, BSTR* b)
{    
    int r;
    char* s;
    if (b == NULL) {
	return -1; }
    s = get_erl_str(*b);
    SysFreeString(*b);
    *b = NULL;
    r = ei_x_encode_string(x, s);
    driver_free(s);
    return r;
}

static const char* bools[] = {"false", "true"};
static const char vt_error[] = "error";
static char com_null[] = "null";

/* convert COM variant to erlang binary format */
static void encode_variant(erl_com_thread_data* d, VARIANT* v)
{
    long l;
    unsigned long u;
    double dbl;
    char b[50];
    OLECHAR* oles;
    void* p;
    ei_x_buff* x = &d->x;
    BOOL ref = (v->vt & VT_BYREF) != 0;
    switch (v->vt) {
    case VT_EMPTY:
	ei_x_encode_empty_list(x);
	break;
    case VT_UI1:
    case VT_UI1 | VT_BYREF:
	u = ref ? *v->pbVal : v->bVal;
	ei_x_encode_ulong(x, u);
	break;
    case VT_UI2:
    case VT_UI2 | VT_BYREF:
	u = ref ? *v->puiVal : v->uiVal;
	ei_x_encode_ulong(x, u);
	break;
    case VT_UINT:
    case VT_UI4:
    case VT_UINT | VT_BYREF:
    case VT_UI4 | VT_BYREF:
	u = ref ? *v->pulVal : v->ulVal;
	ei_x_encode_ulong(x, u);
	break;
    case VT_I1:
    case VT_I1 | VT_BYREF:
	l = ref ? *v->pcVal : v->cVal;
	ei_x_encode_long(x, l);
	break;
    case VT_I2:
    case VT_I2 | VT_BYREF:
	l = ref ? *v->piVal : v->iVal;
	ei_x_encode_long(x, l);
	break;
    case VT_INT:
    case VT_I4:
    case VT_INT | VT_BYREF:
    case VT_I4 | VT_BYREF:
	l = ref ? *v->plVal : v->lVal;
	ei_x_encode_long(x, l);
	break;
    case VT_R4:
    case VT_R4 | VT_BYREF:
	dbl = ref ? *v->pfltVal : v->fltVal;
	ei_x_encode_double(x, dbl);
	break;
    case VT_VARIANT:
    case VT_CY:
    case VT_DECIMAL:
    case VT_I8:
    case VT_UI8:
	if (FAILED(VariantChangeType(v, v, 0, VT_R8)))
	    goto no_conv;
    case VT_R8:
    case VT_R8 | VT_BYREF:
	dbl = ref ? *v->pdblVal : v->dblVal;
	ei_x_encode_double(x, dbl);
	break;
    case VT_BSTR:
    case VT_BSTR | VT_BYREF:
	oles = ref ? *v->pbstrVal : v->bstrVal;
	ei_x_encode_and_free_bstr(x, &oles);
	break;
    case VT_BOOL:
    case VT_BOOL | VT_BYREF:
	l = ref ? *v->pboolVal : v->boolVal;
	l &= 1;
	ei_x_encode_atom(x, bools[l]);
	break;
    case VT_NULL:
	l = ref ? *v->pboolVal : v->boolVal;
	l &= 1;
	ei_x_encode_atom(x, com_null);
	break;
    case VT_DATE:
    case VT_DATE | VT_BYREF:
	dbl = ref ? *v->pdate : v->date;
	ei_x_encode_tuple_header(x, 3);
	dbl -= date_1970;
	l = (long)(dbl / 1000000.0 * (24 * 60 * 60));
	dbl -= l * 1000000.0 / (24 * 60 * 60);
	ei_x_encode_long(x, l);
	l = (long)(dbl * (24 * 60 * 60));
	ei_x_encode_long(x, l);
	dbl -= l / (24 * 60 * 60);
	l = (long)(dbl / 1000000 / (24 * 60 * 60));
	ei_x_encode_long(x, l);
	break;
    case VT_DISPATCH:	case VT_DISPATCH | VT_BYREF:
    case VT_UNKNOWN:	case VT_UNKNOWN | VT_BYREF:
	p = ref ? *v->ppunkVal : v->punkVal;
	ei_x_encode_long(x, add_interface(d, p));
	break;
    case VT_ERROR:
    default:
no_conv:
	sprintf(b, "COMET unknown type %d", v->vt);
	ei_x_encode_string(x, b);
	break;
    }
}

/* decode double from either a double or a long in erlang binary form */
static int my_decode_double(const char* buff, int* index, double* d)
{
    int r = 0, is, t, s;
    ei_get_type(buff, index, &t, &s);
    switch (t) {
    case ERL_FLOAT_EXT:
	r = ei_decode_double(buff, index, d);
	break;
    case ERL_SMALL_INTEGER_EXT:
    case ERL_INTEGER_EXT:
	r = ei_decode_long(buff, index, &is);
	*d = is;
	break;
    }
    return r;
}

/* encode and return variant to gen_server */
static void return_variant(VARIANT* res, erl_com_thread_data* d)
{
    ei_x_buff* x = &d->x;
    x->index = 0;
    ei_x_encode_version(x);
    ei_x_encode_tuple_header(x, 2);
    ei_x_encode_long(x, d->thread_n);
    encode_variant(d, res);
}

/* encode and return COM error code to gen_server */
static void return_err(HRESULT r, int bad_param_no, erl_com_thread_data* d)
{
    LPVOID msg;
    ei_x_buff* x = &d->x;
    x->index = 0;
    ei_x_encode_version(x);
    ei_x_encode_tuple_header(x, 2);
    ei_x_encode_long(x, d->thread_n);
    ei_x_encode_tuple_header(x, 3+(bad_param_no!=-1));
    ei_x_encode_atom(x, "com_error");
    ei_x_encode_long(x, r);
    FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | 
		      FORMAT_MESSAGE_FROM_SYSTEM | 
		      FORMAT_MESSAGE_IGNORE_INSERTS,
		  NULL,
		  r,
		  MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
		  (LPTSTR) &msg,
		  0,
		  NULL);
    if (msg == NULL) msg = "";
    ei_x_encode_string(x, msg);
    if (bad_param_no != -1)
	ei_x_encode_long(x, bad_param_no);
    LocalFree(msg);
}

/* encode and return COM error exception */
static void return_exception(HRESULT r, BSTR bstrDescription, erl_com_thread_data* d)
{
    ei_x_buff* x = &d->x;
    x->index = 0;
    ei_x_encode_version(x);
    ei_x_encode_tuple_header(x, 2);
    ei_x_encode_long(x, d->thread_n);
    ei_x_encode_tuple_header(x, 3);
    ei_x_encode_atom(x, "com_error");
    ei_x_encode_ulong(x, r);
    ei_x_encode_and_free_bstr(x, &bstrDescription);
}


/* encode and return int to gen_server */
static void return_int(int i, erl_com_thread_data* d)
{
    ei_x_buff* x = &d->x;
    x->index = 0;
    ei_x_encode_version(x);
    ei_x_encode_tuple_header(x, 2);
    ei_x_encode_long(x, d->thread_n);
    ei_x_encode_long(x, i);
}

/* encode and return multiple out parameters in a list */
static void return_var_pars(DISPPARAMS* disp_pars, 
		     int n_results, erl_com_thread_data* d)
{
    int i;
    ei_x_buff* x = &d->x;
    UINT j;
    x->index = 0;
    ei_x_encode_version(x);
    ei_x_encode_tuple_header(x, 2);
    ei_x_encode_long(x, d->thread_n);
    ei_x_encode_list_header(x, n_results);
    for (i = 0, j = disp_pars->cArgs-1; i < n_results; ++i) {
	while ((disp_pars->rgvarg[j].vt & VT_BYREF) == 0 && j>= 0) --j;
	if (j >= 0) {
	    encode_variant(d, &disp_pars->rgvarg[j]);
	} else {
	    ei_x_encode_long(x, -1);
	}
    }
    ei_x_encode_empty_list(x);
}


static HRESULT get_var_pars(int n, erl_com_thread_data* d, int* index,
			    VARIANTARG** var_pars, DISPPARAMS* disp_pars, 
			    VARIANT* refs, int* n_refs, UINT* bad_par);

static HRESULT get_stdcall_pars(int n, erl_com_thread_data* d, int* index, 
				char** stdcall_pars, int* pari,
				VARIANT* refs, int* n_refs, UINT* bad_par);

static HRESULT return_stdcall_pars(VARIANT* results, int n_results, erl_com_thread_data* d);

static HRESULT __cdecl stdcall_apply(char* i_p, long m_offset, char* stdcall_pars, int pari);

enum { N_RESULTS = 100 };

static void get_interface_info(erl_com_thread_data* d, int index);
static void get_typelib_info(erl_com_thread_data* d, int index);

/* 
 * perform operation requested in gen_server, return result
 */
static void perform_op(erl_com_thread_data* d)
{
    int index = 0;
    int v, n, i, m;
    char s[200];
    VARIANT results[N_RESULTS];
    int n_results = 0;
    OLECHAR* oles, * oles2;
    HRESULT r;
    char* buff = d->x.buff;
    int buffsz = d->x.buffsz;
    d->op = buff[index++];	/* get operation */
    d->thread_n = buff[index++]; /* get thread (we know it, we should do a check instead) */
    ei_decode_version(buff, &index, &v);
    //DebugBreak();
    switch (d->op) {
    /* create a com object
     */
    case com_CreateObject:
	ei_decode_tuple_header(buff, &index, &n);
	if (n != 3) {
	    return_err(1, 0, d);
	    break;
	}
	ei_decode_string(buff, &index, s);
	oles = get_ole_str(s);
	ei_decode_string(buff, &index, s);
	oles2 = get_ole_str(s);
	ei_decode_long(buff, &index, &m);
	r = create_obj(d, oles, oles2, (CLSCTX)m, &i);
	SysFreeString(oles);
	SysFreeString(oles2);
	if (SUCCEEDED(r)) {
	    return_int(i, d); 
	} else {
	    return_err(r, 0, d);
	}
	break;
    /* get a running com object
     * name, iid -> interface
     */
    case com_GetObject:
	ei_decode_tuple_header(buff, &index, &n);
	if (n != 2) {
	    return_err(1, 0, d);
	    break;
	}
	ei_decode_string(buff, &index, s);
	oles = get_ole_str(s);
	ei_decode_string(buff, &index, s);
	oles2 = get_ole_str(s);
	r = get_obj(d, oles, oles2, &i);
	SysFreeString(oles);
	SysFreeString(oles2);
	if (SUCCEEDED(r)) {
	    return_int(i, d); 
	} else {
	    return_err(r, 0, d);
	}
	break;
    /* QueryInterface
     */
    case com_QueryInterface:
	ei_decode_tuple_header(buff, &index, &n);
	if (n != 2) {
	    return_err(1, 0, d);
	    break;
	}
	ei_decode_long(buff, &index, &i);
	ei_decode_string(buff, &index, s);
	oles = get_ole_str(s);
	r = query_interface(d, i, oles, &i);
	SysFreeString(oles);
	if (SUCCEEDED(r)) {
	    return_int(i, d);
	} else {
	    return_err(r, 0, d);
	}
	break;
    /* release a com object
     */
    case com_Release:
	ei_decode_long(buff, &index, &i);
	r = release_(d, i);
	if (SUCCEEDED(r)) {
	    return_int(0, d);
	} else {
	    return_err(r, 0, d);
	}
	break;
    /* invoke a com method (dispatch)
     */
    case com_Invoke:
    case com_PropertyGet:
	{
	    EXCEPINFO excep;
	    VARIANT var_result;
	    VARIANTARG* var_pars;
	    DISPPARAMS disp_pars;
	    UINT bad_par;
	    WORD flags;
	    int t;
	    VariantInit(&var_result);
	    ei_decode_tuple_header(buff, &index, &n);
	    if (n != 3) {
		return_err(2, 0, d);
		break;
	    }
	    ei_decode_long(buff, &index, &i);
	    ei_decode_long(buff, &index, &m);
	    disp_pars.cNamedArgs = 0;
	    disp_pars.rgdispidNamedArgs = NULL;
	    ei_get_type(buff, &index, &t, &n);
	    if (t == ERL_NIL_EXT) {
		ei_decode_list_header(buff, &index, &n);
	    } else if (t != ERL_LIST_EXT) {
		n = 1;
	    } else {
		ei_decode_list_header(buff, &index, &n);
	    }
	    flags = (d->op==com_Invoke)? DISPATCH_METHOD : DISPATCH_PROPERTYGET;
	    r = get_var_pars(n, d, &index, &var_pars, &disp_pars, results,
			     &n_results, &bad_par);
	    if (SUCCEEDED(r)) {
		r = IDispatch_Invoke((LPDISPATCH)d->com_i_ptrs[i],
				     m, &IID_NULL, LOCALE_SYSTEM_DEFAULT,
				     flags, &disp_pars, &var_result, &excep,
				     &bad_par);
		if (SUCCEEDED(r)) {
		    if (n_results > 0) {
			if (var_result.vt != VT_EMPTY) {
			    disp_pars.rgvarg[disp_pars.cArgs].pfltVal = &var_result.fltVal;
			    disp_pars.rgvarg[disp_pars.cArgs++].vt = var_result.vt + VT_BYREF;
			}
			return_var_pars(&disp_pars, n_results, d);
		    } else
			return_variant(&var_result, d);
		}
		if (r == DISP_E_EXCEPTION) {
		    return_exception(r, excep.bstrDescription, d);
		    r = S_OK;
		}
	    }
	    if (FAILED(r)) {
	        if (bad_par != -1)
		    bad_par = n - bad_par - 1; /* Dispatch numbers them backwards */
		return_err(r, bad_par, d);
	    }
	    driver_free(var_pars);
	}
	break;
    /* perform a stdcall on a com method, given its offset in the vtable
     * and its parameters
     */
    case com_Call:
	{
	    UINT bad_par;
	    int t, pari;
	    char* stdcall_pars;

	    ei_decode_tuple_header(buff, &index, &n);
	    if (n != 3) {
		return_err(2, 0, d);
		break; }
	    ei_decode_long(buff, &index, &i);
	    ei_decode_long(buff, &index, &m);
	    ei_get_type(buff, &index, &t, &n);
	    if (t == ERL_NIL_EXT || t == ERL_LIST_EXT)
		ei_decode_list_header(buff, &index, &n);
	    else {
		return_err(3, 0, d);
		break;
	    }
	    r = get_stdcall_pars(n, d, &index, &stdcall_pars, &pari, results,
				 &n_results, &bad_par);
	    if (SUCCEEDED(r)) {
		r = stdcall_apply(d->com_i_ptrs[i], m, stdcall_pars, pari);
		if (SUCCEEDED(r)) {
		    return_stdcall_pars(results, n_results, d);
		} else {
		    return_err(r, bad_par, d);
		}
	    }
	    driver_free(stdcall_pars);
	}
	break;
    /* put property (dispatch)
     */
    case com_PropertyPut:
    case com_PropertyPutRef:
	{
	    VARIANTARG* var_pars;
	    DISPPARAMS disp_pars;
	    DISPID dispip_propput = DISPID_PROPERTYPUT;
	    WORD flag = d->op == com_PropertyPut ? 
		DISPATCH_PROPERTYPUT : DISPATCH_PROPERTYPUTREF;
	    UINT bad_par;
	    int t;
	    ei_decode_tuple_header(buff, &index, &n);
	    if (n != 3) {
		return_err(2, 0, d);
		break; }
	    ei_decode_long(buff, &index, &i);
	    ei_decode_long(buff, &index, &m);
	    disp_pars.rgdispidNamedArgs = &dispip_propput; 
	    disp_pars.cNamedArgs = 1;
	    ei_get_type(buff, &index, &t, &n);
	    if (t == ERL_NIL_EXT) {
		ei_decode_list_header(buff, &index, &n);
	    } else if (t != ERL_LIST_EXT) {
		n = 1;
	    } else {
		ei_decode_list_header(buff, &index, &n);
	    }
	    r = get_var_pars(n, d, &index, &var_pars, &disp_pars,
			     results, &n_results, &bad_par);
	    if (SUCCEEDED(r)) {
		r = IDispatch_Invoke((LPDISPATCH)d->com_i_ptrs[i],
				     m,
				     &IID_NULL,
				     LOCALE_SYSTEM_DEFAULT,
				     flag,
				     &disp_pars,
				     NULL,
				     NULL,
				     &bad_par);
	    }
	    if (SUCCEEDED(r)) {
		return_int(0, d);
	    } else {
		return_err(r, 0, d);
	    }
	    driver_free(var_pars);
	}
	break;
    /* get method id from name
     */
    case com_GetMethodID:
	{
	    DISPID method;
	    ei_decode_tuple_header(buff, &index, &n);
	    if (n != 2) {
		return_err(3, 0, d);
		break;
	    }
	    ei_decode_long(buff, &index, &i);
	    ei_decode_string(buff, &index, s);
	    oles = get_ole_str(s);
	    r = IDispatch_GetIDsOfNames((LPDISPATCH)d->com_i_ptrs[i],
					&IID_NULL, 
					&oles,
					1, 
					LOCALE_SYSTEM_DEFAULT, 
					&method);
	    SysFreeString(oles);
	    if (SUCCEEDED(r)) {
		return_int(method, d);
	    } else {
		return_err(r, 0, d);
	    }
	}
	break;
    /* end com thread 
     */
    case com_EndThread:
	// ska någon release göras här??? !!
	d->stop = TRUE;
	break;
    case com_CurrentThread:
	return_int(n_threads-1, d);
	break;
    case com_Next:
    case com_NextIntf:
	{
	    IEnumVARIANT* ev;
	    VARIANT v;
	    DWORD nr;
	    ei_decode_long(buff, &index, &i);
	    if (i <= 0 || i > d->n_com_i_ptrs) {
		return_err(DISP_E_EXCEPTION, 0, d);
		break; 
	    }
	    ev = (IEnumVARIANT*)d->com_i_ptrs[i];
	    r = IEnumVARIANT_Next(ev, 1, &v, &nr);
	    if (SUCCEEDED(r)) {
		if (r == S_OK) {
		    if (d->op == com_NextIntf) {
			v.ppunkVal = *(IUnknown***)&v;
			v.vt = VT_UNKNOWN;
		    }
		return_variant(&v, d);
		} else { /* return {} when end of Enum (since it's never a variant) */
		    ei_x_buff* x = &d->x;
		    x->index = 0;
		    ei_x_encode_version(x);
		    ei_x_encode_tuple_header(x, 2);
		    ei_x_encode_long(x, d->thread_n);
		    ei_x_encode_tuple_header(x, 0);
    		}
    	    } else {
		return_err(r, 0, d);
	    }
	}
	break;	   
    case com_Reset:
	{
	    IEnumVARIANT* ev;
	    ei_decode_long(buff, &index, &i);
	    if (i <= 0 || i > d->n_com_i_ptrs) {
		return_err(DISP_E_EXCEPTION, 0, d);
		break; 
	    }
	    ev = (IEnumVARIANT*)d->com_i_ptrs[i];
	    r = IEnumVARIANT_Reset(ev);
	    if (SUCCEEDED(r))
		return_int(r, d);
    	    else
		return_err(r, 0, d);
	}
	break;	   
    /* get type information from com
     * to be used for erlang code generation real soon now
     */
    case com_GetInterfaceInfo:
	get_interface_info(d, index);
	break;
    case com_GetTypeLibInfo:
	get_typelib_info(d, index);
	break;
    case com_Test:
	{
	    ei_x_buff* x = &d->x;
	    DebugBreak();
	    return_int(-12, d);
	    break;
	}
    /* NewThread is intercepted, nobody should get here */
    case com_NewThread:
    default:
	return_err(E_UNEXPECTED, 0, d);
	break;
    }
}

/* convert date, two formats accepted:
   now() result (Ms, s, mikros)   and
   {{y, m, d}, {h, m, s}}
   (where h, m and s are optional) */
static int my_decode_date(const char* buff, int* index, DATE* date)
{
    long l;
    int arity;
    *date = 0;
    ei_decode_tuple_header(buff, index, &arity);
    if (arity == 2) {		/* universal_time */
	UDATE s;
	ei_decode_tuple_header(buff, index, &arity);
	if (arity != 3) 
	    return 0;
	memset(&s, '\0', sizeof(s));
	ei_decode_long(buff, index, &l);
	s.st.wYear = (USHORT)l;
	ei_decode_long(buff, index, &l);
	s.st.wMonth = (USHORT)l;
	ei_decode_long(buff, index, &l);
	s.st.wDay = (USHORT)l;
	ei_decode_tuple_header(buff, index, &arity);
	if (arity >= 1) {
	    ei_decode_long(buff, index, &l);
	    s.st.wHour = (USHORT)l;
	}
	if (arity >= 2) {
	    ei_decode_long(buff, index, &l);
	    s.st.wMinute = (USHORT)l;
	}
	if (arity == 3) {
	    ei_decode_long(buff, index, &l);
	    s.st.wSecond = (USHORT)l;
	}
	VarDateFromUdate(&s, 0, date);
    } else if (arity == 3) {	/* now format */
	*date = date_1970;
	ei_decode_long(buff, index, &l);
	*date += l * (1000000.0 / (24 * 60 * 60.0));
	ei_decode_long(buff, index, &l);
	*date += l / (24 * 60 * 60.0);
	ei_decode_long(buff, index, &l);
	*date += l / 1000000.0 / (24 * 60 * 60.0);
    } else
	return -1;
    return 0;
}

/* decode string and convert to BSTR */
static int my_decode_string(const char* buff, int* index, BSTR* oles)
{
    char* s;
    int t, sz;
    ei_get_type(buff, index, &t, &sz);
    if (t == ERL_NIL_EXT) {
	ei_decode_list_header(buff, index, &t);
	*oles = NULL;
	return 0;
    } else if (t != ERL_STRING_EXT) {
	*oles = NULL;
	return -1;
    }
    s = driver_alloc(sz+1);
    ei_decode_string(buff, index, s);
    *oles = get_ole_str(s);
    driver_free(s);
    return 0;
}

/* hmmm... */
static int my_decode_interface(const char* buff, int* index, int* intf)
{
    char b[MAXATOMLEN];
    int t, sz;
    ei_get_type(buff, index, &t, &sz);
    if (t == ERL_SMALL_INTEGER_EXT || t == ERL_INTEGER_EXT) {
	ei_decode_long(buff, index, intf); 
    } else if (t != ERL_SMALL_TUPLE_EXT || sz != 4) {
	*intf = -1;
	return -1;
    } else {
	erlang_pid pid;
	ei_decode_tuple_header(buff, index, &sz);
	ei_decode_atom(buff, index, b);
	ei_decode_pid(buff, index, &pid);
	ei_decode_long(buff, index, &t);
	ei_decode_long(buff, index, intf);
    }
    return 0;
}

static HRESULT get_1_var_par(char* buff, int* index, VARIANTARG* curarg, 
			     VARIANT* refs, int* n_refs, erl_com_thread_data* d);


static int my_decode_bool(const char* buff, int* index, void* o)
{
    HRESULT r;
    int t, sz;
    VARIANTARG v;
    ei_get_type(buff, index, &t, &sz);
    if (t == ERL_SMALL_INTEGER_EXT || t == ERL_INTEGER_EXT)
	return ei_decode_long(buff, index, o);
    if (t == ERL_SMALL_TUPLE_EXT || t == ERL_LARGE_TUPLE_EXT)
	return -1;
    r = get_1_var_par((char*)buff, index, &v, NULL, NULL, NULL);
    if (SUCCEEDED(r))
	r = VariantChangeType(&v, &v, 0, VT_BOOL);
    memmove(o, &v.boolVal, sizeof(v.boolVal));
    return FAILED(r) ? -1 : 0;
}

static int my_decode_variant(const char* buff, int* index, void* o)
{
    HRESULT r;
    int t, sz;
    VARIANTARG v;
    ei_get_type(buff, index, &t, &sz);
    if (t == ERL_SMALL_TUPLE_EXT || t == ERL_LARGE_TUPLE_EXT)
	return -1;
    r = get_1_var_par((char*)buff, index, &v, NULL, NULL, NULL);
    memmove(o, &v, sizeof(v));
    return FAILED(r) ? -1 : 0;
}



/* 
 * we use a table of functions to convert from erlang binary
 * format to com types
 */
typedef int (*erl_decf)(const char* buff, int* index, void* dst);

typedef struct var_erl_conv_TAG {
    const char* erl_type_atom;
    erl_decf erl_dec;
    VARTYPE vt1;
    VARTYPE vt2;
    size_t  sz;
} var_erl_conv;

static var_erl_conv converter[] = {
    { "vt_i1",   ei_decode_long,  VT_I4,   VT_I1,   1 },
    { "vt_u1",   ei_decode_ulong, VT_UI4,  VT_UI1,  1 },
    { "vt_i2",   ei_decode_long,  VT_I4,   VT_I2,   2 },
    { "vt_u2",   ei_decode_ulong, VT_UI4,  VT_UI2,  2 },
    { "vt_i4",   ei_decode_long,  VT_I4,   0,       4 },
    { "vt_u4",   ei_decode_ulong, VT_UI4,  0,	    4 },
    { "vt_int",  ei_decode_long,  VT_INT,  0,       4 },
    { "vt_uint", ei_decode_ulong, VT_UINT, 0,	    4 },
    { "vt_r8",   my_decode_double,VT_R8,   0,       8 },
    { "vt_r4",   my_decode_double,VT_R8,   VT_R4,   4 },
    { "vt_decimal", my_decode_double,
				  VT_R8,   VT_DECIMAL, 16 },
    { "vt_currency", my_decode_double,
				  VT_R8,   VT_CY,   8 },
    { "vt_r4",   my_decode_double,VT_R8,   VT_R4,   4 },
    { "vt_bool", my_decode_bool,  VT_I4,   VT_BOOL, 4 },
    { "vt_date", my_decode_date,  VT_DATE, 0,       8 },
    { "vt_str",  my_decode_string,VT_BSTR, 0,       4 },
    { "vt_variant", my_decode_variant,
				  VT_VARIANT, 0,    16 },
    { "vt_unknown", my_decode_interface, 
				  VT_UNKNOWN, 0,    4 },
    { "vt_dispatch", my_decode_interface, 
				  VT_DISPATCH, 0,   4 },
    { "enum",	 ei_decode_long,  VT_I4,   0,   4 },
    { NULL, NULL, 0, 0 }
};

/* the two following functions convert a parameter from erlang binary format 
 * to com type, also supports the {vt_type, value} tuple */

/* convert to stdcall stack form */
static HRESULT get_stdcall_tuple_parameter(char* buff, int* index, char* stdcall_pars, int* pari,
					   VARIANT* refs, int* n_refs, 
					   erl_com_thread_data* d)
{
    int arity, t, sz;
    char atomname[MAXATOMLEN+1], data[100];
    BOOL ref = FALSE;
    var_erl_conv* conv;
    VARIANT interarg;

    ei_decode_tuple_header(buff, index, &arity);
    if (arity != 2)
	return E_INVALIDARG;
    ei_get_type(buff, index, &t, &sz);
    if (t != ERL_ATOM_EXT)
	return E_INVALIDARG;
    ei_decode_atom(buff, index, atomname);
    ei_get_type(buff, index, &t, &sz);
    if (t == ERL_ATOM_EXT) {
	char a2[MAXATOMLEN+1];
	int ix = *index;
	ei_decode_atom(buff, &ix, a2);
	if (strcmp(a2, "out") != 0) {
	    if (strcmp(atomname, "vt_variant") != 0 && 
		strcmp(atomname, "vt_bool") != 0)
		return E_INVALIDARG;
	} else {
	    ref = TRUE;
	    *index = ix;
	}
    }
    for (conv = converter; conv->erl_type_atom != NULL; ++conv) {
	if (strcmp(atomname, conv->erl_type_atom) == 0) {
	    goto found;
	}
    }
    return E_INVALIDARG;
 found:
    if (ref) {
	if (refs == NULL || n_refs == NULL)
	    return E_INVALIDARG;
	memset(&refs[*n_refs], 255, sizeof(refs[0]));
	if (conv->erl_dec == my_decode_variant)
	    *(void* *)&stdcall_pars[*pari] = &refs[*n_refs];
	else {
	    *(void* *)&stdcall_pars[*pari] = &refs[*n_refs].dblVal;
	}
	/* NB: vt this is just a parameter to return_stdcall_params, we SHOULD NOT set VT_BYREF here,
	       as it would confuse return_stdcall_params (and trick it into an extra indirection) */
        refs[*n_refs].vt = conv->vt2 != 0 ? conv->vt2 : conv->vt1; 
	++*n_refs;
	*pari += sizeof(void*);
	return S_OK;
    }
    if (conv->erl_dec(buff, index, data) < 0) 
	return E_INVALIDARG;
    interarg.vt = conv->vt1;
    if (conv->erl_dec == my_decode_interface) {	/* unfortunately, we don't pass d to the decoder */
	interarg.punkVal = (LPUNKNOWN)d->com_i_ptrs[*(int*)data];
    } else if (conv->erl_dec == my_decode_variant) {	/* not so clean */
	memcpy(&stdcall_pars[*pari], data, conv->sz);
        *pari += conv->sz;
	return S_OK;
    } else {
	memcpy(&interarg.dblVal, data, sizeof(double));
	if (conv->vt2 != 0) {
	    HRESULT r = VariantChangeType(&interarg, &interarg, 0, conv->vt2);
		if (FAILED(r))
		    return r;
	}
    }
    memcpy(&stdcall_pars[*pari], &interarg.dblVal, conv->sz);
    *pari += conv->sz;
    return S_OK;
}

/* convert to com variant */
static HRESULT get_tuple_parameter(char* buff, int* index, VARIANTARG* curarg,
				   VARIANT* refs, int* n_refs, 
				   erl_com_thread_data* d)
{
    int arity, t, sz;
    char atomname[MAXATOMLEN+1], data[100];
    VARIANTARG interarg;
    BOOL ref = FALSE;
    var_erl_conv* conv;

    ei_decode_tuple_header(buff, index, &arity);
    if (arity != 2) return E_INVALIDARG;
    ei_get_type(buff, index, &t, &sz);
    if (t != ERL_ATOM_EXT) return E_INVALIDARG;
    ei_decode_atom(buff, index, atomname);
    ei_get_type(buff, index, &t, &sz);
    if (t == ERL_ATOM_EXT) {
	char a2[MAXATOMLEN+1];
	int ix = *index;
	ei_decode_atom(buff, &ix, a2);
	if (strcmp(a2, "out") != 0) {
	    if (strcmp(atomname, "vt_variant") != 0 && 
		strcmp(atomname, "vt_bool") != 0)
		return E_INVALIDARG;
	} else {
	    ref = TRUE;
	    *index = ix;
	}
    }
    for (conv = converter; conv->erl_type_atom != NULL; ++conv) {
	if (strcmp(atomname, conv->erl_type_atom) == 0) {
	    goto found;
	}
    }
    return E_INVALIDARG;
 found:
    if (ref) {
	if (refs == NULL || n_refs == NULL) return E_INVALIDARG;
	if (conv->erl_dec == my_decode_variant)
	    curarg->pvarVal = &refs[*n_refs];
	else
	    curarg->pdblVal = &refs[*n_refs].dblVal;
	curarg->vt = conv->vt2;
	if (curarg->vt == 0) curarg->vt = conv->vt1;
	curarg->vt |= VT_BYREF;
	++(*n_refs);
	return S_OK;
    }
    if (conv->erl_dec == NULL || conv->erl_dec(buff, index, data) < 0) 
	return E_INVALIDARG;
    interarg.vt = conv->vt1;
    if (conv->erl_dec == my_decode_interface) { /* see get_stdcall_tuple_parameter */
	if (d == NULL)
	    return E_INVALIDARG;
	interarg.punkVal = (LPUNKNOWN)d->com_i_ptrs[*(int*)data];
    } else if (conv->erl_dec == my_decode_variant) {	/* not so clean */
	memcpy(&interarg, data, sizeof(interarg));    
    } else
	memcpy(&interarg.dblVal, data, sizeof(double));
    if (conv->vt2 != 0) {
	return VariantChangeType(curarg, &interarg, 0, conv->vt2);
    } else {
	*curarg = interarg;
    }
    return S_OK;
}

/* get variant parameters from erlang binary format list */
static HRESULT get_1_var_par(char* buff, int* index, VARIANTARG* curarg, VARIANT* refs, int* n_refs, erl_com_thread_data* d)
{
    int t, sz, j, tmp;
    double dbl;
    char* s, atomname[MAXATOMLEN+1];
    BSTR oles;
    HRESULT r = S_OK;
    VariantInit(curarg);
    ei_get_type(buff, index, &t, &sz);
    switch (t) {
    case ERL_SMALL_INTEGER_EXT:
    case ERL_INTEGER_EXT:
	ei_decode_long(buff, index, &j);
	curarg->vt = VT_I4;
	curarg->intVal = j;
	break;
    case ERL_FLOAT_EXT:
	ei_decode_double(buff, index, &dbl);
	curarg->vt = VT_R8;
	curarg->dblVal = dbl;
	break;
    case ERL_SMALL_TUPLE_EXT:
    case ERL_LARGE_TUPLE_EXT:
	r = get_tuple_parameter(buff, index, curarg, refs, n_refs, d);
	break;
    case ERL_STRING_EXT:
	s = driver_alloc(sz);
	ei_decode_string(buff, index, s);
	oles = get_ole_str(s);
	curarg->vt = VT_BSTR;
	curarg->bstrVal = oles;
	break;
    case ERL_ATOM_EXT:
	ei_decode_atom(buff, index, atomname);
	curarg->vt = VT_BOOL;
	if (strcmp(atomname, bools[TRUE]) == 0) {
	    curarg->boolVal = TRUE;
	} else if (strcmp(atomname, bools[FALSE]) == 0) {
	    curarg->boolVal = FALSE;
	} else if (strcmp(atomname, vt_error) == 0) {
	    curarg->vt = VT_ERROR;
	    curarg->scode = DISP_E_PARAMNOTFOUND;
	}
	break;
    case ERL_NIL_EXT:   // we can't tell empty list from empty string...
	ei_decode_list_header(buff, index, &tmp);
	s = driver_alloc(sz);
	oles = get_ole_str("");
	curarg->vt = VT_BSTR;
	curarg->bstrVal = oles;
	break;
    case ERL_LIST_EXT:
    case ERL_BINARY_EXT:
    case ERL_SMALL_BIG_EXT:
    case ERL_LARGE_BIG_EXT:
    case ERL_PASS_THROUGH:
    case ERL_REFERENCE_EXT:
    case ERL_NEW_REFERENCE_EXT:
    case ERL_PORT_EXT:
    case ERL_PID_EXT:
    default:
	r = E_INVALIDARG;
    }
    return r;
}

static HRESULT get_var_pars(int n, erl_com_thread_data* d, int* index, VARIANTARG** var_pars, DISPPARAMS* disp_pars, 
		     VARIANT* refs, int* n_refs, UINT* bad_par)
{
    int i;
    HRESULT r;
    *var_pars = driver_alloc(sizeof(VARIANTARG)*n);
    disp_pars->cArgs = n;
    disp_pars->rgvarg = *var_pars;
    *n_refs = 0;
    for (i = 0; i < n; ++i) {
	*bad_par = n - i - 1; /* dispatch numbers them backwards */
	r = get_1_var_par(d->x.buff, index, &(*var_pars)[n - i - 1], refs, n_refs, d);
	if (FAILED(r))
	    return r;
    }
    *bad_par = -1;
    return S_OK;
}

/* get stdcall parameter from erlang binary format list */
static HRESULT get_stdcall_pars(int n, erl_com_thread_data* d, int* index, 
				char** stdcall_pars, int* pari, 
				VARIANT* refs, int* n_refs, UINT* bad_par)
{
    int i, tmp;
    char atomname[MAXATOMLEN+1];
    HRESULT r;
    char* buff = d->x.buff;
    *stdcall_pars = driver_alloc(sizeof(VARIANT)*n);
    *n_refs = 0;
    *pari = 0;
    for (i = 0; i < n; ++i) {
	int t, sz, j;
	double dbl;
	char* s;
	OLECHAR* oles;

	*bad_par = i;
	ei_get_type(buff, index, &t, &sz);
	switch (t) {
	case ERL_SMALL_INTEGER_EXT:
	case ERL_INTEGER_EXT:
	    ei_decode_long(buff, index, &j);
	    *(long*)&(*stdcall_pars)[*pari] = j;
	    *pari += sizeof(long);
	    break;
	case ERL_FLOAT_EXT:
	    ei_decode_double(buff, index, &dbl);
	    *(double*)&(*stdcall_pars)[*pari] = dbl;
	    *pari += sizeof(double);
	    break;
	case ERL_SMALL_TUPLE_EXT:
	case ERL_LARGE_TUPLE_EXT:
	    r = get_stdcall_tuple_parameter(buff, index, *stdcall_pars,
					    pari, refs, n_refs, d);
	    if (!SUCCEEDED(r)) return r;
	    break;
	case ERL_STRING_EXT:
	    s = driver_alloc(sz);
	    ei_decode_string(buff, index, s);
	    oles = get_ole_str(s);
	    *(BSTR*)&(*stdcall_pars)[*pari] = oles;
	    pari += sizeof(BSTR);
	    break;
	case ERL_ATOM_EXT:
	    ei_decode_atom(buff, index, atomname);
	    if (strcmp(atomname, bools[TRUE]) == 0) {
		j = TRUE;
	    } else if (strcmp(atomname, bools[FALSE]) == 0) {
		j = FALSE;
	    } else {
		return E_INVALIDARG;
	    }
	    *(long*)&(*stdcall_pars)[*pari] = j;
	    pari += sizeof(long);
	    break;
	case ERL_NIL_EXT:
	    ei_decode_list_header(buff, index, &tmp);
	    *(BSTR*)&(*stdcall_pars)[*pari] = NULL;
	    pari += sizeof(long);
	    break;
	case ERL_LIST_EXT:
	case ERL_BINARY_EXT:
	case ERL_SMALL_BIG_EXT:
	case ERL_LARGE_BIG_EXT:
	case ERL_PASS_THROUGH:
	case ERL_REFERENCE_EXT:
	case ERL_NEW_REFERENCE_EXT:
	case ERL_PORT_EXT:
	case ERL_PID_EXT:
	    return E_INVALIDARG;
	}
    }
    *bad_par = -1;
    return S_OK;
}

/* a nice hack to perform stdcall calling from erlang to COM
 * synthesize a stack frame with parameters and return address,
 * and jump right in
 */
static HRESULT stdcall_apply(char* i_p, long m_offset, char* stdcall_pars, int pari)
{
    __asm {
	mov  esi, stdcall_pars	/* get parameters */
	mov  ecx, pari		/* size of pars */
	mov  ebx, i_p		/* interface pointer */
	mov  eax, m_offset	/* method offset */
	push ebp
	mov  ebp, esp
	sub  esp, ecx		/* make room */
	shr  ecx, 2
	mov  edi, esp
   rep  movsd			/* move them to stack */
	push ebx		/* push "this" */
	add  eax, [ebx]
	call [eax]		/* call it */
	mov  esp, ebp		/* clean up my stack */
	pop  ebp
    }				/* return value in eax is untouched */
}

/* return out parameters from stdcall */
static HRESULT return_stdcall_pars(VARIANT* results, int n_results, erl_com_thread_data* d)
{
    int i;
    ei_x_buff* x = &d->x;
    x->index = 0;
    ei_x_encode_version(x);
    ei_x_encode_tuple_header(x, 2);
    ei_x_encode_long(x, d->thread_n);
    ei_x_encode_list_header(x, n_results);
    for (i = 0; i < n_results; ++i)
	encode_variant(d, &results[i]);
    ei_x_encode_empty_list(x);
    return S_OK;
}

/* constant used by get_interface_description */

/* function kinds in COM */
static char* func_kind_names[] = {
    "virtual", "purevirtual", "nonvirtual", "static", "dispatch",
    NULL };

/* invoke kinds in COM */
static char* invoke_kind_names[] = {
    "func", "property_get", "property_put", "property_put_ref",
    NULL };
/*    {	INVOKE_FUNC	= 1,
	INVOKE_PROPERTYGET	= 2,
	INVOKE_PROPERTYPUT	= 4,
	INVOKE_PROPERTYPUTREF	= 8 */

static char* param_flag_names[] = {
    "in", "out", "lcid", "retval", "optional", 
    "has_default", "has_custom_data", NULL };
/*
#define	PARAMFLAG_NONE	( 0 )
#define	PARAMFLAG_FIN	( 0x1 )
#define	PARAMFLAG_FOUT	( 0x2 )
#define	PARAMFLAG_FLCID	( 0x4 )
#define	PARAMFLAG_FRETVAL	( 0x8 )
#define	PARAMFLAG_FOPT	( 0x10 )
#define	PARAMFLAG_FHASDEFAULT	( 0x20 )
#define	PARAMFLAG_FHASCUSTDATA	( 0x40 )
*/

/* type kinds in TypeLibs */
static char* type_kind_names[TKIND_MAX] = {
    "enum", "record", "module", 
    "interface", "dispatch", "coclass",
    "alias", "union" };
/*    {	TKIND_ENUM	= 0,
	TKIND_RECORD	= TKIND_ENUM + 1,
	TKIND_MODULE	= TKIND_RECORD + 1,
	TKIND_INTERFACE	= TKIND_MODULE + 1,
	TKIND_DISPATCH	= TKIND_INTERFACE + 1,
	TKIND_COCLASS	= TKIND_DISPATCH + 1,
	TKIND_ALIAS	= TKIND_COCLASS + 1,
	TKIND_UNION	= TKIND_ALIAS + 1,
	TKIND_MAX	= TKIND_UNION + 1
*/

/* COM type names */
static struct com_type_name {
    const char* name;
    UINT com_type;
} type_names[] = {
    { "vt_i1", VT_I1},
    { "vt_ui1", VT_UI1},
    { "vt_i2", VT_I2},
    { "vt_ui2", VT_UI2},
    { "vt_i4", VT_I4},
    { "vt_ui4", VT_UI4},
    { "vt_r4", VT_R4},
    { "vt_r8", VT_R8},
    { "vt_bool", VT_BOOL},
    { "vt_int", VT_INT},
    { "vt_uint", VT_UINT},
    { "vt_variant", VT_VARIANT},
    { "vt_unknown", VT_UNKNOWN},
    { "vt_decimal", VT_DECIMAL},
    { "vt_date", VT_DATE},
    { "vt_currency", VT_CY},
    { "vt_str", VT_BSTR},
    { "vt_dispatch", VT_DISPATCH},
    { "vt_unknown", VT_UNKNOWN},
    { "vt_ptr", VT_PTR},
    { "vt_array", VT_ARRAY},
    { "vt_safearray", VT_SAFEARRAY},
    { NULL, 0 }
};

/* get type name from com_type */
static const char* get_type_name(UINT com_type)
{
    struct com_type_name* tp;
    for (tp = type_names; tp->name != NULL; ++tp) {
	if (com_type == tp->com_type) return tp->name; 
    }
    return "";
}

/* encode flags as list of atoms */
int x_encode_flags(ei_x_buff* x, int flags, const char* names[], int n)
{
    int i, m;
    for (i = 0, m = 0; i < n; ++i) {
	if ((1 << i) & flags)
	    ++m;
    }
    ei_x_encode_list_header(x, m);
	for (i = 0; i < n; ++i) {
	    if ((1 << i) & flags)
		ei_x_encode_atom(x, names[i]);
	}
    return ei_x_encode_empty_list(x);
}

// encode type of parameters and/or return value
static void encode_type(ei_x_buff* x, TYPEDESC* pt, ITypeInfo* t)
{
    HRESULT r;
    // pointer type
    while (pt->vt == VT_PTR) {
	pt = pt->lptdesc;
	ei_x_encode_tuple_header(x, 2);// A 3 B2?
	ei_x_encode_atom(x, "pointer");//    B 1
    }
    // user defined type
    if (pt->vt == VT_USERDEFINED) {
	ITypeInfo* t2;
	LPTYPEATTR pta2;
	BSTR b;
	HREFTYPE rt = pt->hreftype;
	ei_x_encode_tuple_header(x, 2); // A 3/B 2' D2
	r = ITypeInfo_GetRefTypeInfo(t, rt, &t2);
	r = ITypeInfo_GetDocumentation(t2, MEMBERID_NIL, &b, NULL, NULL, NULL);
	r = ITypeInfo_GetTypeAttr(t2, &pta2);
	if (SUCCEEDED(r)) {
	    if (pta2->typekind == TKIND_ALIAS) {
		ei_x_encode_tuple_header(x, 2);
		ei_x_encode_atom(x, type_kind_names[pta2->typekind]);// D 1'
		encode_type(x, &pta2->tdescAlias, t2);
	    } else
		ei_x_encode_atom(x, type_kind_names[pta2->typekind]);// D 1"
	    ITypeInfo_ReleaseTypeAttr(t, pta2); // really void
	} else
	    ei_x_encode_atom(x, "unknown");   // D 1'''
	ei_x_encode_and_free_bstr(x, &b);	    // D 2
	r = IUnknown_Release(t2);
    } else
	ei_x_encode_atom(x,
	    get_type_name(pt->vt));                     // A 3/B 2"
}

// generate erlang description from TypeInfo
static HRESULT get_interface(ITypeInfo* t, LPTYPEATTR pt, 
			     int dispflag, ei_x_buff* x)
{
    int i, n;
    HRESULT r = S_OK;
    // count functions
    for (i = 0, n = 0; i < pt->cFuncs; ++i) {
	LPFUNCDESC pf;
	r = ITypeInfo_GetFuncDesc(t, i, &pf);
	if (SUCCEEDED(r)) {
	    if ((pf->wFuncFlags & (FUNCFLAG_FRESTRICTED | FUNCFLAG_FHIDDEN)) == 0)
		++n;
	    ITypeInfo_ReleaseFuncDesc(t, pf);
	}
    }
    // get functions
    ei_x_encode_list_header(x, n);
    for (i = 0; SUCCEEDED(r) && i < pt->cFuncs; ++i) {
	LPFUNCDESC pf;
	r = ITypeInfo_GetFuncDesc(t, i, &pf);
	if (SUCCEEDED(r) 
	    && (pf->wFuncFlags & (FUNCFLAG_FRESTRICTED | FUNCFLAG_FHIDDEN)) == 0) {
	    int n_names;
	    BSTR names[100];
	    r = ITypeInfo_GetNames(t, pf->memid, names, 100, &n_names);
	    if (SUCCEEDED(r) && n_names > 0) {
		int j;
		int retval = (dispflag == com_DispatchIntf 
			    && pf->elemdescFunc.tdesc.vt != VT_VOID
			    && pf->elemdescFunc.tdesc.vt != VT_EMPTY);
		// function name and type
		ei_x_encode_tuple_header(x, 6);	// T6
		ei_x_encode_and_free_bstr(x, &names[0]);  //T 1
		x_encode_flags(x, pf->invkind, invoke_kind_names, 6); // T 2
		ei_x_encode_atom(x,  
		    func_kind_names[pf->funckind]);			    // T 3
		ei_x_encode_long(x, 
		    (dispflag == com_DispatchIntf) ? pf->memid : pf->oVft); // T 4
		// list of parameters
		if (pf->cParams > 0) {
		    ei_x_encode_list_header(x, pf->cParams); // T 5 
		    for (j = 0; j < pf->cParams; ++j) {
			USHORT parflags = pf->lprgelemdescParam[j].paramdesc.wParamFlags;
			ei_x_encode_tuple_header(x, 4);	   // A4
			if (j+1 < n_names)
			    ei_x_encode_and_free_bstr(x, &names[j+1]); // A 1'
	    		else
			    ei_x_encode_string(x, "property_"); // A 1"
			x_encode_flags(x, parflags, param_flag_names, 8); // A 2
			encode_type(x, &pf->lprgelemdescParam[j].tdesc, t);// A 3
	    		if (parflags & PARAMFLAG_FHASDEFAULT) {
			    erl_com_thread_data d;
			    memset(&d, 0, sizeof(d));
			    d.x = *x;
			    encode_variant(&d,
				&pf->lprgelemdescParam[j].paramdesc.pparamdescex->varDefaultValue); // A 4'
			    *x = d.x;
			} else
			    ei_x_encode_tuple_header(x, 0);			    // A 4"
		    }
		}
		ei_x_encode_empty_list(x);
		// return value
		if (retval) {
		    encode_type(x, &pf->elemdescFunc.tdesc, t);
		} else
		    ei_x_encode_atom(x, "void");		// T 6"
	    } 
	    ITypeInfo_ReleaseFuncDesc(t, pf);
	}
    }
    ei_x_encode_empty_list(x);
    return r;
}

static HRESULT get_enum(ITypeInfo* t, LPTYPEATTR pt, 
			     int dispflag, ei_x_buff* x)
{
    int i, n;
    HRESULT r = S_OK;
    LPVARDESC pv;
    //DebugBreak();
    for (i = 0, n = 0; i < pt->cVars; ++i) {
	r = ITypeInfo_GetVarDesc(t, i, &pv);
	if (SUCCEEDED(r)) {
	    if ((pv->wVarFlags & (VARFLAG_FRESTRICTED | VARFLAG_FHIDDEN)) == 0)
		++n;
	    ITypeInfo_ReleaseVarDesc(t, pv);
	}
    }
    ei_x_encode_list_header(x, n);
    for (i = 0; SUCCEEDED(r) && i < pt->cVars; ++i) {
	r = ITypeInfo_GetVarDesc(t, i, &pv);
	if (SUCCEEDED(r) 
	    && (pv->wVarFlags & (VARFLAG_FRESTRICTED | VARFLAG_FHIDDEN)) == 0) {
	    int n_names = 1;
	    BSTR name;
	    r = ITypeInfo_GetNames(t, pv->memid, &name, 1, &n_names);
	    if (SUCCEEDED(r) && n_names > 0) {
		erl_com_thread_data d;
		memset(&d, 0, sizeof(d));
		ei_x_encode_tuple_header(x, 2);
		ei_x_encode_and_free_bstr(x, &name);
		d.x = *x;
		encode_variant(&d, pv->lpvarValue); // this will crash mercilessly if value is interface...
		*x = d.x;
	    } else
		ei_x_encode_empty_list(x);
	    ITypeInfo_ReleaseVarDesc(t, pv);
	}
    }
    ei_x_encode_empty_list(x);
    return r;
}

static HRESULT get_typeinfo(ITypeInfo* t, LPTYPEATTR pt, int dispflag, ei_x_buff* x)
{
    char* s;
    BSTR b;
    HRESULT r;
    int i;
    //DebugBreak();
    ei_x_encode_tuple_header(x, 5);
    if ((pt->wTypeFlags & TYPEFLAG_FDUAL)) {
	s = "dual";
	if (dispflag == com_VirtualIntf) {
	    ITypeInfo* t2 = t;
	    HREFTYPE rt;
	    ITypeInfo_GetRefTypeOfImplType(t2, -1, &rt);
	    ITypeInfo_GetRefTypeInfo(t2, rt, &t);
	    ITypeInfo_ReleaseTypeAttr(t2, pt);
	    IUnknown_Release(t2);
	    ITypeInfo_GetTypeAttr(t, &pt);
	}
    } else if (pt->wTypeFlags & TYPEFLAG_FDISPATCHABLE) {
	s = "dispatch";
    } else {
	s = "virtual";
    }
    ei_x_encode_atom(x, type_kind_names[pt->typekind]);
    ei_x_encode_atom(x, s);
    ei_x_encode_tuple_header(x, 2);
    ITypeInfo_GetDocumentation(t, MEMBERID_NIL, &b, NULL, NULL, NULL);
    ei_x_encode_and_free_bstr(x, &b);
    StringFromCLSID(&pt->guid, &b);
    ei_x_encode_and_free_bstr(x, &b);
    if (pt->typekind == TKIND_INTERFACE || pt->typekind == TKIND_DISPATCH) {
	r = get_interface(t, pt, dispflag, x);//ei_x_encode_empty_list(x);
    } else if (pt->typekind == TKIND_ENUM) {
	r = get_enum(t, pt, dispflag, x);
    } else
	ei_x_encode_empty_list(x);
    // implemented types, including inherited type
    for (i = -1; SUCCEEDED(r) && i < pt->cImplTypes; ++i) {
	ITypeInfo* impt;
	HREFTYPE reft;
	LPTYPEATTR imppt;
	r = ITypeInfo_GetRefTypeOfImplType(t, i, &reft);
	if (SUCCEEDED(r)) {
	    if (i == -1)
		ei_x_encode_list_header(x, pt->cImplTypes+1);
	    ei_x_encode_tuple_header(x, 3);
	    ei_x_encode_long(x, i);
    	    ITypeInfo_GetRefTypeInfo(t, reft, &impt);
	    ITypeInfo_GetDocumentation(impt, MEMBERID_NIL, &b, NULL, NULL, NULL);
	    ei_x_encode_and_free_bstr(x, &b);
	    ITypeInfo_GetTypeAttr(impt, &imppt);
	    StringFromCLSID(&imppt->guid, &b);
	    ei_x_encode_and_free_bstr(x, &b);
	    ITypeInfo_ReleaseTypeAttr(impt, imppt);
	    IUnknown_Release(impt);
	}
    }
    if (i == 0) 
	r = S_OK;
    ei_x_encode_empty_list(x);
    return r;
}

static HRESULT decode_typelib_and_type(erl_com_thread_data* d, int* index, ITypeLib** l, ITypeInfo** t)
{
    int ty, sz, i;
    char* buff = d->x.buff;
    HRESULT r;
    BSTR b;

    *t = NULL;
    ei_get_type(buff, index, &ty, &sz);
    if (ty == ERL_SMALL_INTEGER_EXT || ty == ERL_INTEGER_EXT) {
	IDispatch* di;
        ei_decode_long(buff, index, &i);
        r = IUnknown_QueryInterface((LPUNKNOWN)d->com_i_ptrs[i], &IID_IDispatch, &di);
	if (SUCCEEDED(r)) {
	    r = IDispatch_GetTypeInfo((LPDISPATCH)d->com_i_ptrs[i], 0, LOCALE_SYSTEM_DEFAULT, t);
	    if (SUCCEEDED(r) && l != NULL) 
		r = ITypeInfo_GetContainingTypeLib(*t, l, &i);
	    //else
		//*l = NULL;
	}
    } else if (ty == ERL_STRING_EXT) {
	char* s = driver_alloc(sz + 1);
	ei_decode_string(buff, index, s);
	b = get_ole_str(s);
        driver_free(s);
        r = LoadTypeLib(b, l);
	SysFreeString(b);
    } else {
	r = E_INVALIDARG;
    }
    return r;
}

static void get_interface_info(erl_com_thread_data* d, int index)
{
    ITypeInfo* t;
    ITypeLib* l;
    HRESULT r;
    int n, dispflag;
    ei_x_buff* x = &d->x;
    BSTR mem = NULL;
    ei_decode_tuple_header(x->buff, &index, &n);
    if (n != 2 && n != 3) {
        return_err(3, 0, d); 
        return;
    }
    if (n == 3) {
        char memname[256];
        ei_decode_string(x->buff, &index, memname);
        mem = get_ole_str(memname);
    }
    r = decode_typelib_and_type(d, &index, (mem != NULL ? &l : NULL), &t);
    ei_decode_long(x->buff, &index, &dispflag);
    if (SUCCEEDED(r)) {
        LPTYPEATTR pt;
	USHORT us = 1;
	MEMBERID mi;
	if (mem != NULL) {
	    if (t != NULL)
		IUnknown_Release(t);
	    r = ITypeLib_FindName(l, mem, 0, &t, &mi, &us);
	    if (us == 0)    // Shouldn't this give an error in COM???
		r = DISP_E_MEMBERNOTFOUND;
	}	    
	if (SUCCEEDED(r)) {
	    x->index = 0;
	    ei_x_encode_version(x);
	    ei_x_encode_tuple_header(x, 2);
	    ei_x_encode_long(x, d->thread_n);
	    r = ITypeInfo_GetTypeAttr(t, &pt);
	    r = get_typeinfo(t, pt, dispflag, x);
	    ITypeInfo_ReleaseTypeAttr(t, pt);
	    IUnknown_Release(t);
	}
    }
    if (FAILED(r)) 
	return_err(r, 0, d);
}

static char* get_ms_error(DWORD err)
{
    LPVOID	lpMsgBuf;
    char* res;
    FormatMessage( 
	FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
	NULL,
	err,
	MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), 
	(LPTSTR) &lpMsgBuf,
	0,
	NULL);
    res = driver_alloc(strlen(lpMsgBuf)+1);
    strcpy(res, lpMsgBuf);
    LocalFree(lpMsgBuf);
    return res;
}


static void get_typelib_info(erl_com_thread_data* d, int index)
{
    ITypeInfo* t = NULL;
    HRESULT r;
    int n, i;
    BSTR b;
    ei_x_buff* x = &d->x;
    ITypeLib* l;

    r = decode_typelib_and_type(d, &index, &l, &t);
    if (FAILED(r))
	return_err(r, 0, d);
    if (SUCCEEDED(r)) {
	ITypeLib_GetDocumentation(l, -1, &b, NULL, NULL, NULL);
	x->index = 0;
	ei_x_encode_version(x);
	ei_x_encode_tuple_header(x, 2);
	ei_x_encode_long(x, d->thread_n);
	ei_x_encode_tuple_header(x, 2);
	ei_x_encode_and_free_bstr(x, &b);
	n = ITypeLib_GetTypeInfoCount(l);
	//DebugBreak();
	ei_x_encode_list_header(x, n);
	for (i = 0; i < n; ++i) {
	    ITypeInfo* t;
	    LPTYPEATTR pt;
    	    ITypeLib_GetTypeInfo(l, i, &t);
	    r = ITypeInfo_GetTypeAttr(t, &pt);
	    ei_x_encode_tuple_header(x, 3);
	    ei_x_encode_atom(x, type_kind_names[pt->typekind]);
	    ITypeLib_GetDocumentation(l, i, &b, NULL, NULL, NULL);
	    ei_x_encode_and_free_bstr(x, &b);
	    StringFromCLSID(&pt->guid, &b); 
	    ei_x_encode_and_free_bstr(x, &b);
	    ITypeInfo_ReleaseTypeAttr(t, pt);
	    IUnknown_Release(t);
	}
	ei_x_encode_empty_list(x);
	IUnknown_Release(l);
	if (t != NULL)
	    IUnknown_Release(t);
    }
    if (FAILED(r)) 
	return_err(r, 0, d);
}

