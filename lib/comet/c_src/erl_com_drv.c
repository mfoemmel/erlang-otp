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
    char* buff;			/* buffer */
    int buffsz;			/* allocated buffsize */
    int bufflen;		/* length of data in buffer (bufflen <= buffsz) */
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
	    ienv->buffsz = n + 100;
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
    n_threads = 0;
    init_dates();

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
	n -= WAIT_OBJECT_0;
	if (n == 0) {		/* this is the input_thread */
	    char* tmpbuff; int tmpbufflen, tmpbuffsz;
	    /* check for the special operation of creating a new thread */
	    if (in_env.op == com_NewThread) {
		int t;
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
		tmpbuff = erl_data[i]->buff;
		tmpbufflen = erl_data[i]->bufflen;
		tmpbuffsz = erl_data[i]->buffsz;
		erl_data[i]->buff = in_env.buff;
		erl_data[i]->bufflen = in_env.bufflen;
		erl_data[i]->buffsz = in_env.buffsz;
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
	    n = htonl(erl_data[i]->bufflen); /* erlang is big-endian */
	    if (!WriteFile(out_h, &n, sizeof(n), &m, NULL)) {
		fprintf(stderr, "erl_com: couldn't write packet size (res) \n");
		exit(1);
	    }
	    if (!WriteFile(out_h, erl_data[i]->buff, erl_data[i]->bufflen, &n, NULL)) {
		fprintf(stderr, "erl_com: couldn't write packet (res) (wanted %d, got %d) \n", 
			erl_data[i]->bufflen, n);
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
    ret->buff = NULL;
    ret->bufflen = 0;
    ret->buffsz = 0;
    ret->stop = FALSE;
    ret->n_com_i_ptrs = 0;
    ret->com_i_ptrs = driver_alloc(sizeof(void*));
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
    driver_output((ErlDrvPort)handle, data->buff, data->bufflen);
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
    /*DebugBreak();*/
    if (op == com_NewThread) {
	char buf[100]; int index = 0;
	for (i = 0; i < n_threads; ++i) {
	    if (erl_data[i] == NULL)
		break;
	}
	if (i == N_MAX_THREADS) 
	    return;
	data = erl_data[i] = new_erl_com_thread(port);
	ei_encode_version(buf, &index);
	ei_encode_tuple_header(buf, &index, 2);
	ei_encode_long(buf, &index, NEW_THREAD_SPECIAL_N);
	ei_encode_long(buf, &index, i);
        driver_output(port, buf, index);
	if (i == n_threads)
	    ++n_threads;
    } else {
	if (thread >= 0 && thread < n_threads) {
	    data = erl_data[thread];
	    data->buffsz = data->bufflen = bufflen;
	    data->buff = driver_realloc(data->buff, bufflen);
	    memcpy(data->buff, buff, bufflen);
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
    d->com_i_ptrs = driver_realloc(d->com_i_ptrs, (d->n_com_i_ptrs+1)*sizeof(void*));
    d->com_i_ptrs[d->n_com_i_ptrs] = p;
    return d->n_com_i_ptrs++;
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
	if (SUCCEEDED(r)) {
	    *i = add_interface(d, p);
	}
    }
    return r;
}

/* call com release */
static HRESULT release_(erl_com_thread_data* d, int i)
{
    HRESULT r = IUnknown_Release((LPUNKNOWN)d->com_i_ptrs[i]);
    d->com_i_ptrs[i] = NULL;
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

static const char* bools[] = {"false", "true"};
static char com_null[] = "null";

/* convert COM variant to erlang binary format */
static void encode_variant(erl_com_thread_data* d, int* index, VARIANT* v)
{
    long l;
    unsigned long u;
    double dbl;
    char* s, b[50];
    OLECHAR* oles;
    void* p;
    char* buff = d->buff;
    BOOL ref = (v->vt & VT_BYREF) != 0;
    switch (v->vt) {
    case VT_EMPTY:
	ei_encode_empty_list(buff, index);
	break;
    case VT_UI1:
    case VT_UI1 | VT_BYREF:
	u = ref ? *v->pbVal : v->bVal;
	ei_encode_ulong(buff, index, u);
	break;
    case VT_UI2:
    case VT_UI2 | VT_BYREF:
	u = ref ? *v->puiVal : v->uiVal;
	ei_encode_ulong(buff, index, u);
	break;
    case VT_UINT:
    case VT_UI4:
    case VT_UINT | VT_BYREF:
    case VT_UI4 | VT_BYREF:
	u = ref ? *v->pulVal : v->ulVal;
	ei_encode_ulong(buff, index, u);
	break;
    case VT_I1:
    case VT_I1 | VT_BYREF:
	l = ref ? *v->pcVal : v->cVal;
	ei_encode_long(buff, index, l);
	break;
    case VT_I2:
    case VT_I2 | VT_BYREF:
	l = ref ? *v->piVal : v->iVal;
	ei_encode_long(buff, index, l);
	break;
    case VT_INT:
    case VT_I4:
    case VT_INT | VT_BYREF:
    case VT_I4 | VT_BYREF:
	l = ref ? *v->plVal : v->lVal;
	ei_encode_long(buff, index, l);
	break;
    case VT_R4:
    case VT_R4 | VT_BYREF:
	dbl = ref ? *v->pfltVal : v->fltVal;
	ei_encode_double(buff, index, dbl);
	break;
    case VT_R8:
    case VT_R8 | VT_BYREF:
	dbl = ref ? *v->pdblVal : v->dblVal;
	ei_encode_double(buff, index, dbl);
	break;
    case VT_BSTR:
    case VT_BSTR | VT_BYREF:
	oles = ref ? *v->pbstrVal : v->bstrVal;
	s = get_erl_str(oles);
	SysFreeString(oles);
	ei_encode_string(buff, index, s);
	driver_free(s);
	break;
    case VT_BOOL:
    case VT_BOOL | VT_BYREF:
	l = ref ? *v->pboolVal : v->boolVal;
	l &= 1;
	ei_encode_atom(buff, index, bools[l]);
	break;
    case VT_NULL:
	l = ref ? *v->pboolVal : v->boolVal;
	l &= 1;
	ei_encode_atom(buff, index, com_null);
	break;
    case VT_DATE:
    case VT_DATE | VT_BYREF:
	dbl = ref ? *v->pdate : v->date;
	ei_encode_tuple_header(buff, index, 3);
	dbl -= date_1970;
	l = (long)(dbl / 1000000.0 * (24 * 60 * 60));
	dbl -= l * 1000000.0 / (24 * 60 * 60);
	ei_encode_long(buff, index, l);
	l = (long)(dbl * (24 * 60 * 60));
	ei_encode_long(buff, index, l);
	dbl -= l / (24 * 60 * 60);
	l = (long)(dbl / 1000000 / (24 * 60 * 60));
	ei_encode_long(buff, index, l);
	break;
    case VT_DISPATCH:
    case VT_UNKNOWN:
	p = ref ? *v->ppunkVal : v->punkVal;
	ei_encode_long(buff, index, add_interface(d, p));
	break;
    case VT_CY:
    case VT_ERROR:
    case VT_VARIANT:
    case VT_DECIMAL:
    case VT_I8:
    case VT_UI8:
    default:
	sprintf(b, "unknown type %d", v->vt);
	ei_encode_string(buff, index, b);
	break;
    }
}

/* encode and return int to gen_server */
static void return_int(int i, erl_com_thread_data* d)
{
    int index = 0;
    ei_encode_version(d->buff, &index);
    ei_encode_tuple_header(d->buff, &index, 2);
    ei_encode_long(d->buff, &index, d->thread_n);
    ei_encode_long(d->buff, &index, i);
    d->bufflen = index;
}

/* encode and return variant to gen_server */
static void return_variant(VARIANT* res, erl_com_thread_data* d)
{
    int index = 0;
    ei_encode_version(d->buff, &index);
    ei_encode_tuple_header(d->buff, &index, 2);
    ei_encode_long(d->buff, &index, d->thread_n);
    encode_variant(d, &index, res);
    d->bufflen = index;
}

/* encode and return COM error code to gen_server */
static void return_err(HRESULT r, int bad_param_no, erl_com_thread_data* d)
{
    int index = 0;
    char b[200];
    ei_encode_version(d->buff, &index);
    ei_encode_tuple_header(d->buff, &index, 2);
    ei_encode_long(d->buff, &index, d->thread_n);
    ei_encode_tuple_header(d->buff, &index, 2);
    ei_encode_atom(d->buff, &index, "com_error");
    sprintf(b, "%x %d", r, bad_param_no);
    ei_encode_string(d->buff, &index, b);
    d->bufflen = index;
}

/* encode and return multiple out parameters in a list */
static void return_var_pars(DISPPARAMS* disp_pars, 
		     int n_results, erl_com_thread_data* d)
{
    int i, index = 0;
    UINT j;
    ei_encode_version(d->buff, &index);
    ei_encode_tuple_header(d->buff, &index, 2);
    ei_encode_long(d->buff, &index, d->thread_n);
    ei_encode_list_header(d->buff, &index, n_results);
    for (i = 0, j = disp_pars->cArgs-1; i < n_results; ++i) {
	while ((disp_pars->rgvarg[j].vt & VT_BYREF) == 0 && j>= 0) --j;
	if (j >= 0) {
	    encode_variant(d, &index, &disp_pars->rgvarg[j]);
	} else {
	    ei_encode_long(d->buff, &index, -1);
	}
    }
    ei_encode_empty_list(d->buff, &index);
    d->bufflen = index;
}

/* decode double from either a double or a long in erlang binary form */
static int my_decode_double(const char* buff, int* index, double* d)
{
    int r = -1, is, t, s;
    ei_get_type(buff, index, &t, &s);
    switch (t) {
    case ERL_FLOAT_EXT:
	r = ei_decode_double(buff, index, d);
	break;
    case ERL_SMALL_INTEGER_EXT:
    case ERL_INTEGER_EXT:
	r = ei_decode_long(buff, index, &is);
	*d = is;
	break; }
    return r;
}


static HRESULT get_var_pars(erl_com_thread_data* d, int* index, VARIANTARG** var_pars,
		     DISPPARAMS* disp_pars, double* refs, int* n_refs,
		     UINT* bad_par);

static HRESULT get_stdcall_pars(erl_com_thread_data* d, int* index, char** stdcall_pars,
			 int* pari, double* refs, int* results_types,
			 int* n_refs, UINT* bad_par);

static HRESULT return_stdcall_pars(double* results, int* result_types,
			    int n_results, erl_com_thread_data* d);

static HRESULT __cdecl stdcall_apply(char* i_p, long m_offset, char* stdcall_pars, int pari);

/*
 * yet another dynamic-buffer version of ei 
 */
enum { X_EI_EXTRA = 100 };

static int x_fix_buff(char** buff, int* buffsz, int szneeded)
{
    int sz = szneeded + X_EI_EXTRA;
    if (sz > *buffsz) {
	sz += X_EI_EXTRA;	/* to avoid reallocating each and every time */
	*buffsz = sz;
	*buff = driver_realloc(*buff, sz);
    }
    return *buff != NULL;
}

static int x_ei_encode_string(char** buff, int* buffsz, int* index, const char* s)
{
    int i = *index;
    ei_encode_string(NULL, &i, s);
    if (!x_fix_buff(buff, buffsz, i))
	return FALSE;
    return ei_encode_string(*buff, index, s);
}

static int x_ei_encode_and_free_bstr(char** buff, int* buffsz, int* index, BSTR* b)
{    
    int r;
    char* s;
    if (b == NULL)  
	return FALSE;
    s = get_erl_str(*b);
    SysFreeString(*b);
    *b = NULL;
    r = x_ei_encode_string(buff, buffsz, index, s);
    driver_free(s);
    return r;
}

static int x_ei_encode_long(char** buff, int* buffsz, int* index, long n)
{
    int i = *index;
    ei_encode_long(NULL, &i, n);
    if (!x_fix_buff(buff, buffsz, i))
	return FALSE;
    return ei_encode_long(*buff, index, n);
}

static int x_ei_encode_list_header(char** buff, int* buffsz, int* index, long n)
{
    int i = *index;
    ei_encode_list_header(NULL, &i, n);
    if (!x_fix_buff(buff, buffsz, i))
	return FALSE;
    return ei_encode_list_header(*buff, index, n);
}

static int x_ei_encode_empty_list(char** buff, int* buffsz, int* index)
{
    int i = *index;
    ei_encode_empty_list(NULL, &i);
    if (!x_fix_buff(buff, buffsz, i))
	return FALSE;
    return ei_encode_empty_list(*buff, index);
}

static int x_ei_encode_version(char** buff, int* buffsz, int* index)
{
    int i = *index;
    ei_encode_version(NULL, &i);
    if (!x_fix_buff(buff, buffsz, i))
	return FALSE;
    return ei_encode_version(*buff, index);
}

static int x_ei_encode_tuple_header(char** buff, int* buffsz, int* index, long n)
{
    int i = *index;
    ei_encode_tuple_header(NULL, &i, n);
    if (!x_fix_buff(buff, buffsz, i))
	return FALSE;
    return ei_encode_tuple_header(*buff, index, n);
}

static int x_ei_encode_atom(char** buff, int* buffsz, int* index, const char* s)
{
    int i = *index;
    ei_encode_atom(NULL, &i, s);
    if (!x_fix_buff(buff, buffsz, i))
	return FALSE;
    return ei_encode_atom(*buff, index, s);
}

enum { N_RESULTS = 100 };

static void get_interface_info(erl_com_thread_data* d, int* in_index);
static void get_typelib_info(erl_com_thread_data* d, int* in_index);

/* 
 * perform operation requested in gen_server, return result
 */
static void perform_op(erl_com_thread_data* d)
{
    int index = 0;
    int v, n, i, m;
    char s[200];
    double results[N_RESULTS];
    int result_types[N_RESULTS];
    int n_results = 0;
    OLECHAR* oles, * oles2;
    HRESULT r;
    char* buff = d->buff;
    int buffsz = d->buffsz;
    d->op = buff[index++];	/* get operation */
    d->thread_n = buff[index++]; /* get thread (we know it, we should do a check instead) */
    ei_decode_version(buff, &index, &v);
    //DebugBreak();
    switch (d->op) {
    /* create a com object
     */
    case com_CreateObject:
	//DebugBreak();
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
	    //DebugBreak();
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
		n = 0;
	    } else if (t != ERL_LIST_EXT) {
		return_err(3, 0, d);
		break;
	    }
	    flags = (d->op==com_Invoke)? DISPATCH_METHOD : DISPATCH_PROPERTYGET;
	    r = get_var_pars(d, &index, &var_pars, &disp_pars, results,
			     &n_results, &bad_par);
	    if (SUCCEEDED(r)) {
		r = IDispatch_Invoke((LPDISPATCH)d->com_i_ptrs[i],
				     m,
				     &IID_NULL,
				     LOCALE_SYSTEM_DEFAULT,
				     flags,
				     &disp_pars,
				     &var_result,
				     &excep,
				     &bad_par);
		if (SUCCEEDED(r)) {
		    if (n_results > 1 || var_result.vt == VT_EMPTY)
			return_var_pars(&disp_pars, n_results, d);
		    else   
			return_variant(&var_result, d);
		} else {
		    return_err(r, bad_par, d);
		}
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

	    //DebugBreak();
	    ei_decode_tuple_header(buff, &index, &n);
	    if (n != 3) {
		return_err(2, 0, d);
		break; }
	    ei_decode_long(buff, &index, &i);
	    ei_decode_long(buff, &index, &m);
	    ei_get_type(buff, &index, &t, &n);
	    if (t != ERL_LIST_EXT) {
		return_err(3, 0, d);
		break;
	    }
	    r = get_stdcall_pars(d, &index, &stdcall_pars, &pari, results,
				 result_types, &n_results, &bad_par);
	    if (SUCCEEDED(r)) {
		r = stdcall_apply(d->com_i_ptrs[i], m, stdcall_pars, pari);
		if (SUCCEEDED(r)) {
		    return_stdcall_pars(results, result_types, n_results, d);
		} else {
		    return_err(r, bad_par, d);
		}
	    }
	    driver_free(stdcall_pars);
	}
	break;
    /* get property (dispatch)
     */
    case com_PropertyGet+1000:
	{
	    EXCEPINFO excep;
	    VARIANT var_result;
	    DISPPARAMS no_disp_pars = {NULL, NULL, 0, 0};
	    //DebugBreak();
	    ei_decode_tuple_header(buff, &index, &n);
	    if (n != 2) {
		return_err(1, 0, d);
		break; }
	    ei_decode_long(buff, &index, &i);
	    ei_decode_long(buff, &index, &m);
	    r = IDispatch_Invoke((LPDISPATCH)d->com_i_ptrs[i],
				 m, &IID_NULL,
				 LOCALE_SYSTEM_DEFAULT,
				 DISPATCH_PROPERTYGET,
				 &no_disp_pars, &var_result,
				 &excep, NULL);
	    if (FAILED(r)) {
		return_err(r, -1, d);
	    } else {
		return_variant(&var_result, d);
	    }
	}
	break;
    /* put property (dispatch)
     */
    case com_PropertyPut:
	{
	    VARIANTARG* var_pars;
	    DISPPARAMS disp_pars;
	    DISPID dispip_propput = DISPID_PROPERTYPUT;
	    UINT bad_par;
	    ei_decode_tuple_header(buff, &index, &n);
	    //DebugBreak();
	    if (n != 3) {
		return_err(2, 0, d);
		break; }
	    ei_decode_long(buff, &index, &i);
	    ei_decode_long(buff, &index, &m);
	    disp_pars.rgdispidNamedArgs = &dispip_propput; 
	    disp_pars.cNamedArgs = 1;
	    r = get_var_pars(d, &index, &var_pars, &disp_pars,
			     results, &n_results, &bad_par);
	    if (SUCCEEDED(r)) {
		r = IDispatch_Invoke((LPDISPATCH)d->com_i_ptrs[i],
				     m,
				     &IID_NULL,
				     LOCALE_SYSTEM_DEFAULT,
				     DISPATCH_PROPERTYPUT,
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
	    //DebugBreak();
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
	/* meny 27/4: Höger (H), resten finns på sr.se/p1 */
	d->stop = TRUE;
	break;
    /* get type information from com
     * to be used for erlang code generation real soon now
     */
    case com_GetInterfaceInfo:
	get_interface_info(d, &index);
	break;
    case com_GetTypeLibInfo:
	get_typelib_info(d, &index);
	break;
    case com_Test:
	{
	    index = 0;
	    x_ei_encode_version(&buff, &buffsz, &index);
	    x_ei_encode_list_header(&buff, &buffsz, &index, 1);
	    x_ei_encode_long(&buff, &buffsz, &index, 0);
	    x_ei_encode_empty_list(&buff, &buffsz, &index); 
	    d->bufflen = index;
	    d->buffsz = buffsz;
	    d->buff = buff;
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
   {{y, m, d}, {h, m, s}} */
static void my_decode_date(const char* buff, int* index, DATE* date)
{
    long l;
    int arity;
    *date = 0;
    ei_decode_tuple_header(buff, index, &arity);
    if (arity == 2) {		/* universal_time */
	UDATE s;
	ei_decode_tuple_header(buff, index, &arity);
	if (arity != 3) return;
	memset(&s, '\0', sizeof(s));
	ei_decode_long(buff, index, &l);
	s.st.wYear = (USHORT)l;
	ei_decode_long(buff, index, &l);
	s.st.wMonth = (USHORT)l;
	ei_decode_long(buff, index, &l);
	s.st.wDay = (USHORT)l;
	ei_decode_tuple_header(buff, index, &arity);
	if (arity != 3) return;
	ei_decode_long(buff, index, &l);
	s.st.wHour = (USHORT)l;
	ei_decode_long(buff, index, &l);
	s.st.wMinute = (USHORT)l;
	ei_decode_long(buff, index, &l);
	s.st.wSecond = (USHORT)l;
	VarDateFromUdate(&s, 0, date);
    } else if (arity == 3) {	/* now format */
	*date = date_1970;
	ei_decode_long(buff, index, &l);
	*date += l * (1000000.0 / (24 * 60 * 60.0));
	ei_decode_long(buff, index, &l);
	*date += l / (24 * 60 * 60.0);
	ei_decode_long(buff, index, &l);
	*date += l / 1000000.0 / (24 * 60 * 60.0);
    }
}

/* decode string and convert to BSTR */
static void my_decode_string(const char* buff, int* index, BSTR* oles)
{
    char* s;
    int t, sz;
    ei_get_type(buff, index, &t, &sz);
    if (t != ERL_STRING_EXT) {
	*oles = NULL;
	return;
    }
    s = driver_alloc(sz);
    ei_decode_string(buff, index, s);
    *oles = get_ole_str(s);
    driver_free(s);
}

/* hmmm... */
static void my_decode_interface(const char* buff, int* index, int* intf)
{
    char b[MAXATOMLEN];
    int t, sz;
    ei_get_type(buff, index, &t, &sz);
    if (t == ERL_SMALL_INTEGER_EXT || t == ERL_INTEGER_EXT) {
	ei_decode_long(buff, index, intf); 
    } else if (t != ERL_SMALL_TUPLE_EXT || sz != 4) {
	*intf = -1;
	return;
    }
    ei_decode_atom(buff, index, b);
    ei_decode_long(buff, index, &t);
    ei_decode_long(buff, index, &t);
    ei_decode_long(buff, index, intf);
}

/* 
 * we use a table of functions to convert from erlang binary
 * format to com types
 */
typedef void (*erl_decf)(const char* buff, int* index, void* dst);

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
    { "vt_u4",   ei_decode_ulong, VT_UI4,  0,	     4 },
    { "vt_r8",   my_decode_double,VT_R8,   0,       8 },
    { "vt_r4",   my_decode_double,VT_R8,   VT_R4,   4 },
    { "vt_bool", ei_decode_long,  VT_I4,   VT_BOOL, 4 },
    { "vt_date", my_decode_date,  VT_DATE, 0,       8 },
    { "vt_str",  my_decode_string,VT_BSTR, 0,       4 },
    { "vt_unknown", my_decode_interface, 
				  VT_UNKNOWN, 0,    4 },
    { "vt_dispatch", my_decode_interface, 
				  VT_DISPATCH, 0,   4 },
    { NULL, NULL, 0, 0 }
};

/* the two following functions convert a parameter from erlang binary format 
 * to com type, also supports the {vt_type, value} tuple */

/* convert to stdcall stack form */
static HRESULT get_stdcall_tuple_parameter(char* buff, int* index, char* stdcall_pars, int* pari,
					   double* refs, int* result_types, int* n_refs, 
					   erl_com_thread_data* d)
{
    int arity, t, sz;
    char atomname[MAXATOMLEN+1], data[100];
    BOOL ref = FALSE;
    var_erl_conv* conv;
    VARIANT interarg;

    ei_decode_tuple_header(buff, index, &arity);
    if (arity != 2) return E_INVALIDARG;
    ei_get_type(buff, index, &t, &sz);
    if (t != ERL_ATOM_EXT) return E_INVALIDARG;
    ei_decode_atom(buff, index, atomname);
    ei_get_type(buff, index, &t, &sz);
    if (t == ERL_ATOM_EXT) {
	char a2[MAXATOMLEN+1];
	ei_decode_atom(buff, index, a2);
	if (strcmp(a2, "out") != 0) return E_INVALIDARG;
	ref = TRUE;
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
	*(void* *)&stdcall_pars[*pari] = &refs[*n_refs];
	memset(&refs[*n_refs], 255, sizeof(refs[0]));
	result_types[*n_refs] = conv->vt2 != 0 ? conv->vt2 : conv->vt1;
	result_types[*n_refs] |= VT_BYREF;
	++*n_refs;
	*pari += sizeof(void*);
	return S_OK;
    }
    conv->erl_dec(buff, index, data);
    interarg.vt = conv->vt1;
    if (conv->erl_dec == my_decode_interface) {	/* unfortunately, we can't pass d to the decoder */
	interarg.punkVal = (LPUNKNOWN)d->com_i_ptrs[*(int*)data];
    } else {
	memcpy(&interarg.dblVal, data, sizeof(double));
	if (conv->vt2 != 0) {
	    HRESULT r = VariantChangeType(&interarg, &interarg, 0, conv->vt2);
		if (FAILED(r)) return r;
	}
    }
    memcpy(&stdcall_pars[*pari], &interarg.dblVal, conv->sz);
    *pari += conv->sz;
    return S_OK;
}

/* convert to com variant */
static HRESULT get_tuple_parameter(char* buff, int* index, VARIANTARG* curarg,
				   double* refs, int* n_refs, 
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
	ei_decode_atom(buff, index, a2);
	if (strcmp(a2, "out") != 0) return E_INVALIDARG;
	ref = TRUE;
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
	curarg->pdblVal = &refs[*n_refs];
	*n_refs += 1;
	curarg->vt = conv->vt2;
	if (curarg->vt == 0) curarg->vt = conv->vt1;
	curarg->vt |= VT_BYREF;
	return S_OK;
    }
    conv->erl_dec(buff, index, data);
    interarg.vt = conv->vt1;
    if (conv->erl_dec == my_decode_interface) { /* see get_stdcall_tuple_parameter */
	interarg.punkVal = (LPUNKNOWN)d->com_i_ptrs[*(int*)data];
    } else
	memcpy(&interarg.dblVal, data, sizeof(double));
    if (conv->vt2 != 0) {
	return VariantChangeType(curarg, &interarg, 0, conv->vt2);
    } else {
	*curarg = interarg;
    }
    return S_OK;
}

/* get variant parameter from erlang binary format list */
static HRESULT get_var_pars(erl_com_thread_data* d, int* index, VARIANTARG** var_pars, DISPPARAMS* disp_pars, 
		     double* refs, int* n_refs, UINT* bad_par)
{
    int i, n;
    char atomname[MAXATOMLEN+1];
    HRESULT r;
    char* buff = d->buff;
    ei_decode_list_header(buff, index, &n);
    *var_pars = driver_alloc(sizeof(VARIANTARG)*n);
    disp_pars->cArgs = n;
    disp_pars->rgvarg = *var_pars;
    *n_refs = 0;
    for (i = 0; i < n; ++i) {
	int t, sz, j;
	double dbl;
	char* s;
	OLECHAR* oles;
	VARIANTARG* curarg = &(*var_pars)[n - i - 1];
	*bad_par = n - i - 1;
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
	    if (!SUCCEEDED(r)) return r;
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
	    } else {
		return E_INVALIDARG;
	    }
	    break;
	case ERL_NIL_EXT:
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

/* get stdcall parameter from erlang binary format list */
static HRESULT get_stdcall_pars(erl_com_thread_data* d, int* index, char** stdcall_pars,
			 int* pari, double* refs, int* result_types,
			 int* n_refs, UINT* bad_par)
{
    int i, n;
    char atomname[MAXATOMLEN+1];
    HRESULT r;
    char* buff = d->buff;
    ei_decode_list_header(buff, index, &n);
    *stdcall_pars = driver_alloc(sizeof(double)*n);
    *n_refs = 0;
    *pari = 0;
    for (i = 0; i < n; ++i) {
	int t, sz, j;
	double dbl;
	char* s;
	OLECHAR* oles;

	*bad_par = n - i - 1;
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
					    pari, refs, result_types, n_refs, d);
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
static HRESULT return_stdcall_pars(double* results, int* result_types, int n_results, erl_com_thread_data* d)
{
    int i, index = 0;
    x_ei_encode_version(&d->buff, &d->buffsz, &index);
    x_ei_encode_tuple_header(&d->buff, &d->buffsz, &index, 2);
    x_ei_encode_long(&d->buff, &d->buffsz, &index, d->thread_n);
    x_ei_encode_list_header(&d->buff, &d->buffsz, &index, n_results);
    for (i = 0; i < n_results; ++i) {
	VARIANT v;
	v.vt = result_types[i];
	v.pdblVal = &results[i];
	encode_variant(d, &index, &v);
    }
    x_ei_encode_empty_list(&d->buff, &d->buffsz, &index);
    d->bufflen = index;
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

static HRESULT get_interface(ITypeInfo* t, LPTYPEATTR pt, 
			     int dispflag, char** buff, int* buffsz, int* index)
{
    int i, n;
    HRESULT r = S_OK;
    for (i = 0, n = 0; i < pt->cFuncs; ++i) {
	LPFUNCDESC pf;
	r = ITypeInfo_GetFuncDesc(t, i, &pf);
	if (SUCCEEDED(r)) {
	    if ((pf->wFuncFlags & (FUNCFLAG_FRESTRICTED | FUNCFLAG_FHIDDEN)) == 0)
		++n;
	    ITypeInfo_ReleaseFuncDesc(t, pf);
	}
    }
    x_ei_encode_list_header(buff, buffsz, index, n);
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
		x_ei_encode_tuple_header(buff, buffsz, index, 5);
		// 1
		x_ei_encode_and_free_bstr(buff, buffsz, index, &names[0]);		
		for (j = 0, n = 0; j < 6; ++j) {
		    if ((1 << j) & pf->invkind)
			++n;
		}
		// 2
		x_ei_encode_list_header(buff, buffsz, index, n+1);
		    x_ei_encode_atom(buff, buffsz, index, 
			func_kind_names[pf->funckind]);
		    for (j = 0; j < 6; ++j) {
			if ((1 << j) & pf->invkind)
			    x_ei_encode_atom(buff, buffsz, index, invoke_kind_names[j]);
		    }
		x_ei_encode_empty_list(buff, buffsz, index);
		// 3
		x_ei_encode_long(buff, buffsz, index, 
		    (dispflag == com_DispatchIntf) ? pf->memid : pf->oVft);
		// 4
		if (n_names > 1) {
		    x_ei_encode_list_header(buff, buffsz, index, n_names-1);
		    for (j = 1; j < n_names; ++j) {
			TYPEDESC* pt;
			int is_ptr;
			USHORT parflags;
			x_ei_encode_tuple_header(buff, buffsz, index, 2);
			x_ei_encode_and_free_bstr(buff, buffsz, index, &names[j]);
	    		pt = &pf->lprgelemdescParam[j-1].tdesc;
			parflags = pf->lprgelemdescParam[j-1].paramdesc.wParamFlags;
			is_ptr = pt->vt == VT_PTR || (parflags & PARAMFLAG_FRETVAL);
			if (is_ptr) {
			    pt = pt->lptdesc;
			    x_ei_encode_tuple_header(buff, buffsz, index, 2); 
			}
			if (is_ptr) 
			    x_ei_encode_atom(buff, buffsz, index, 
				(parflags & PARAMFLAG_FRETVAL) ? "retval" : "out");
			x_ei_encode_atom(buff, buffsz, index, 
			    get_type_name(pt->vt));
		    }
		}
		x_ei_encode_empty_list(buff, buffsz, index);
		// 5
		if (retval) {
		    x_ei_encode_tuple_header(buff, buffsz, index, 2); 
		    x_ei_encode_string(buff, buffsz, index, "_"); 
		    x_ei_encode_tuple_header(buff, buffsz, index, 2); 
		    x_ei_encode_atom(buff, buffsz, index, 
			get_type_name(pf->elemdescFunc.tdesc.vt));
		    x_ei_encode_atom(buff, buffsz, index, "retval");
		} else
		    x_ei_encode_atom(buff, buffsz, index, "void");
	    } 
	    ITypeInfo_ReleaseFuncDesc(t, pf);
	}
    }
    x_ei_encode_empty_list(buff, buffsz, index);
    return r;
}

static HRESULT get_enum(ITypeInfo* t, LPTYPEATTR pt, 
			     int dispflag, char** buff, int* buffsz, int* index)
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
    x_ei_encode_list_header(buff, buffsz, index, n);
    for (i = 0; SUCCEEDED(r) && i < pt->cVars; ++i) {
	r = ITypeInfo_GetVarDesc(t, i, &pv);
	if (SUCCEEDED(r) 
	    && (pv->wVarFlags & (VARFLAG_FRESTRICTED | VARFLAG_FHIDDEN)) == 0) {
	    int n_names = 1;
	    BSTR name;
	    r = ITypeInfo_GetNames(t, pv->memid, &name, 1, &n_names);
	    if (SUCCEEDED(r) && n_names > 0) {
		erl_com_thread_data d;
		x_ei_encode_tuple_header(buff, buffsz, index, 2);
		x_ei_encode_and_free_bstr(buff, buffsz, index, &name);
		d.buff = *buff;
		d.buffsz = X_EI_EXTRA;
		encode_variant(&d, index, pv->lpvarValue); // this will crash mercilessly if value is interface...
	    } else
		x_ei_encode_empty_list(buff, buffsz, index);
	    ITypeInfo_ReleaseVarDesc(t, pv);
	}
    }
    x_ei_encode_empty_list(buff, buffsz, index);
    return r;
}

static HRESULT get_typeinfo(ITypeInfo* t, LPTYPEATTR pt, int dispflag, char** buff, int* buffsz, int* index)
{
    char* s;
    BSTR b;
    HRESULT r;
    int i;
    //DebugBreak();
    x_ei_encode_tuple_header(buff, buffsz, index, 5);
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
    x_ei_encode_atom(buff, buffsz, index, type_kind_names[pt->typekind]);
    x_ei_encode_atom(buff, buffsz, index, s);
    x_ei_encode_tuple_header(buff, buffsz, index, 2);
    ITypeInfo_GetDocumentation(t, MEMBERID_NIL, &b, NULL, NULL, NULL);
    x_ei_encode_and_free_bstr(buff, buffsz, index, &b);
    StringFromCLSID(&pt->guid, &b);
    x_ei_encode_and_free_bstr(buff, buffsz, index, &b);
    if (pt->typekind == TKIND_INTERFACE || pt->typekind == TKIND_DISPATCH) {
	r = get_interface(t, pt, dispflag, buff, buffsz, index);//x_ei_encode_empty_list(buff, buffsz, index);
    } else if (pt->typekind == TKIND_ENUM) {
	r = get_enum(t, pt, dispflag, buff, buffsz, index);
    } else
	x_ei_encode_empty_list(buff, buffsz, index);
    for (i = -1; SUCCEEDED(r) && i < pt->cImplTypes; ++i) {
	ITypeInfo* impt;
	HREFTYPE reft;
	LPTYPEATTR imppt;
	r = ITypeInfo_GetRefTypeOfImplType(t, i, &reft);
	if (SUCCEEDED(r)) {
	    if (i == -1)
		x_ei_encode_list_header(buff, buffsz, index, pt->cImplTypes+1);
	    x_ei_encode_tuple_header(buff, buffsz, index, 2);
    	    ITypeInfo_GetRefTypeInfo(t, reft, &impt);
	    ITypeInfo_GetDocumentation(impt, MEMBERID_NIL, &b, NULL, NULL, NULL);
	    x_ei_encode_and_free_bstr(buff, buffsz, index, &b);
	    ITypeInfo_GetTypeAttr(impt, &imppt);
	    StringFromCLSID(&imppt->guid, &b);
	    x_ei_encode_and_free_bstr(buff, buffsz, index, &b);
	    ITypeInfo_ReleaseTypeAttr(impt, imppt);
	    IUnknown_Release(impt);
	}
    }
    if (i == 0) 
	r = S_OK;
    x_ei_encode_empty_list(buff, buffsz, index);
    return r;
}

static void get_interface_info(erl_com_thread_data* d, int *in_index)
{
    ITypeInfo* t;
    HRESULT r;
    int n, i, index = *in_index, dispflag;
    char* buff = d->buff;
    int bufflen = d->bufflen, buffsz = d->buffsz;
    BSTR mem = NULL;

    ei_decode_tuple_header(buff, &index, &n);
    if (n != 2 && n != 3) {
	return_err(3, 0, d); 
	return;
    }
    if (n == 3) {
	char memname[256];
	ei_decode_string(buff, &index, memname);
	mem = get_ole_str(memname);
    }
    //DebugBreak();
    ei_decode_long(buff, &index, &i);
    ei_decode_long(buff, &index, &dispflag);
    r = IDispatch_GetTypeInfo((LPDISPATCH)d->com_i_ptrs[i],
	0,
	LOCALE_SYSTEM_DEFAULT,
	&t);
    if (SUCCEEDED(r)) {
	LPTYPEATTR pt;
	r = ITypeInfo_GetTypeAttr(t, &pt);
	index = 0;
	x_ei_encode_version(&buff, &buffsz, &index);
	x_ei_encode_tuple_header(&buff, &buffsz, &index, 2);
	x_ei_encode_long(&buff, &buffsz, &index, d->thread_n);
	if (mem != NULL) {
	    ITypeLib* l;
	    ITypeInfo* t2;
	    MEMBERID mi;
	    UINT n;
	    USHORT us = 1;
	    r = ITypeInfo_GetContainingTypeLib(t, &l, &n);
	    ITypeLib_FindName(l, mem, 0, &t2, &mi, &us);
	    ITypeInfo_ReleaseTypeAttr(t, pt);
	    IUnknown_Release(t);
	    t = t2;
	    ITypeInfo_GetTypeAttr(t, &pt);
	}
	r = get_typeinfo(t, pt, dispflag, &buff, &buffsz, &index);
	ITypeInfo_ReleaseTypeAttr(t, pt);
	IUnknown_Release(t);
	d->buffsz = buffsz;
	d->buff = buff;
	d->bufflen = index;
    }
    if (FAILED(r)) return_err(r, 0, d);
}

static void get_typelib_info(erl_com_thread_data* d, int *in_index)
{
    ITypeInfo* t;
    HRESULT r;
    int n, i, index = *in_index;
    char* buff = d->buff;
    BSTR b;
    int bufflen = d->bufflen, buffsz = d->buffsz;
    ITypeLib* l;

    /*ei_decode_tuple_header(buff, &index, &n);
    if (n != 2) {
	return_err(3, 0, d);
	return;
    }*/
    ei_decode_long(buff, &index, &i);
    //DebugBreak();
    r = IDispatch_GetTypeInfo((LPDISPATCH)d->com_i_ptrs[i],
	0,
	LOCALE_SYSTEM_DEFAULT,
	&t);
    if (SUCCEEDED(r)) {
	ITypeInfo_GetContainingTypeLib(t, &l, &i);
	ITypeLib_GetDocumentation(l, -1, &b, NULL, NULL, NULL);
	index = 0;
	x_ei_encode_version(&buff, &buffsz, &index);
	x_ei_encode_tuple_header(&buff, &buffsz, &index, 2);
	x_ei_encode_long(&buff, &buffsz, &index, d->thread_n);
	x_ei_encode_tuple_header(&buff, &buffsz, &index, 2);
	x_ei_encode_and_free_bstr(&buff, &buffsz, &index, &b);
	n = ITypeLib_GetTypeInfoCount(l);
	//DebugBreak();
	x_ei_encode_list_header(&buff, &buffsz, &index, n);
	for (i = 0; i < n; ++i) {
	    ITypeInfo* t;
	    LPTYPEATTR pt;
    	    ITypeLib_GetTypeInfo(l, i, &t);
	    r = ITypeInfo_GetTypeAttr(t, &pt);
	    x_ei_encode_tuple_header(&buff, &buffsz, &index, 3);
	    x_ei_encode_atom(&buff, &buffsz, &index, type_kind_names[pt->typekind]);
	    ITypeLib_GetDocumentation(l, i, &b, NULL, NULL, NULL);
	    x_ei_encode_and_free_bstr(&buff, &buffsz, &index, &b);
	    StringFromCLSID(&pt->guid, &b); 
	    x_ei_encode_and_free_bstr(&buff, &buffsz, &index, &b);
	    ITypeInfo_ReleaseTypeAttr(t, pt);
	    IUnknown_Release(t);
	}
	x_ei_encode_empty_list(&buff, &buffsz, &index);
	IUnknown_Release(l);
	IUnknown_Release(t);
	d->buffsz = buffsz;
	d->buff = buff;
	d->bufflen = index;
    }
    if (FAILED(r)) return_err(r, 0, d);
}

