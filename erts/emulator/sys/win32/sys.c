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
 * system-dependent functions
 *
 */

#include "sys.h"
#include "erl_sys_driver.h"
#include "global.h"

void erts_sys_init_float(void);
void init_sys_select(void);

void win_check_io(int wait);

#ifdef USE_THREADS
void async_ready(int, int);
int init_async(int);
int exit_async(void);
#endif

void erl_start(int, char**);
void erl_exit(int n, char*, _DOTS_);
void erl_error(char*, va_list);
int send_error_to_logger(Eterm);
int schedule(_VOID_);
void erl_crash_dump(char*, char*);

/*
 * Microsoft-specific function to map a WIN32 error code to a Posix errno.
 */
extern void _dosmaperr(DWORD);

#ifdef ERL_RUN_SHARED_LIB
#ifdef __argc
#undef __argc
#endif
#define __argc e_argc
#ifdef __argv
#undef __argv
#endif
#define __argv e_argv
#endif

static void init_console();
static int get_and_remove_option(int* argc, char** argv, const char* option);
static char *get_and_remove_option2(int *argc, char **argv, 
				    const char *option);
static int init_async_io(struct async_io* aio, int use_threads);
static void release_async_io(struct async_io* aio);
static void async_read_file(struct async_io* aio, LPVOID buf, DWORD numToRead);
static int async_write_file(struct async_io* aio, LPVOID buf, DWORD numToWrite);
static int get_overlapped_result(struct async_io* aio,
				 LPDWORD pBytesRead, BOOL wait);
static FUNCTION(BOOL, CreateChildProcess, (char *, HANDLE, HANDLE,
					   HANDLE, LPHANDLE, BOOL,
					   LPVOID, LPTSTR));
static int create_pipe(LPHANDLE, LPHANDLE, BOOL);
static int ApplicationType(const char* originalName, char fullPath[MAX_PATH]);

HANDLE erts_service_event;

/* Results from ApplicationType is one of */
#define APPL_NONE 0
#define APPL_DOS  1
#define APPL_WIN3X 2
#define APPL_WIN32 3

static FUNCTION(int, driver_write, (long, HANDLE, byte*, int));
static FUNCTION(void*, checked_alloc, (unsigned));
static void common_stop(int);
static int create_file_thread(struct async_io* aio, int mode);
static DWORD WINAPI threaded_reader(LPVOID param);
static DWORD WINAPI threaded_writer(LPVOID param);
static DWORD WINAPI threaded_exiter(LPVOID param);

#ifdef DEBUG
static void debug_console(void);
#endif

BOOL WINAPI ctrl_handler(DWORD dwCtrlType);

#define PORT_BUFSIZ 4096

#define PORT_FREE (-1)
#define PORT_EXITING (-2)

/********************* General functions ****************************/

/*
 * Whether create_pipe() should use a named pipe or an anonymous.
 * (Named pipes are not supported on Windows 95.)
 */

static int max_files = 1024;

static BOOL use_named_pipes;
static BOOL win_console = FALSE;


static OSVERSIONINFO int_os_version;	/* Version information for Win32. */


/* This is the system's main function (which may or may not be called "main")
   - do general system-dependent initialization
   - call erl_start() to parse arguments and do other init
   - arrange for schedule() to be called forever, and i/o to be done
*/

HMODULE beam_module = NULL;

void erl_sys_init();

void erl_sys_args(int* argc, char** argv);

int nohup;

void erl_sys_args(int* argc, char** argv)
{
    char *event_name;
    nohup = get_and_remove_option(argc, argv, "-nohup");

#ifdef DEBUG
    /*
     * Start a debug console if -console option given.
     */

    if (get_and_remove_option(argc, argv, "-console")) {
	debug_console();
    }
#endif

    if (nohup && (event_name = get_and_remove_option2(argc, argv, 
						      "-service_event"))) {
	if ((erts_service_event = 
	     OpenEvent(EVENT_ALL_ACCESS,FALSE,event_name)) == NULL) {
	    sys_printf(CERR,
		       "Warning: could not open service event: %s\r\n", 
		       event_name);
	}	    
    } else {
	erts_service_event = NULL;
    }

#ifdef DEBUG
    /*
     * Given the "-threads" option, always use threads instead of
     * named pipes.
     */

    if (get_and_remove_option(argc, argv, "-threads")) {
	use_named_pipes = FALSE;
    }
#endif
}

static void
init_console()
{
    char* mode = getenv("ERL_CONSOLE_MODE");

    if (mode == NULL) {
	mode = "window";
    }
#ifdef PURIFY
    mode = "window";
#endif

    if (strcmp(mode, "window") == 0) {
	win_console = TRUE;
	ConInit();
	/*nohup = 0;*/
    } else if (strncmp(mode, "tty:", 4) == 0) {
	mode += 4;
	if (mode[1] == 'c') {
	    setvbuf(stdout, NULL, _IONBF, 0);
	}
	if (mode[2] == 'c') {
	    setvbuf(stderr, NULL, _IONBF, 0);
	}
    }
}

int sys_max_files() 
{
    return max_files;
}

/*
 * Looks for the given option in the argv vector.  If it is found,
 * it will be removed from the argv vector.
 *
 * If the return value indicates that the option was found and removed,
 * it is the responsibility of the caller to decrement the value of argc.
 *
 * Returns: 0 if the option wasn't found, 1 if it was found
 */

static int
get_and_remove_option(argc, argv, option)
    int* argc;			/* Number of arguments. */
    char* argv[];		/* The argument vector. */
    const char* option;		/* Option to search for and remove. */
{
    int i;

    for (i = 1; i < *argc; i++) {
	if (strcmp(argv[i], option) == 0) {
	    (*argc)--;
	    while (i < *argc) {
		argv[i] = argv[i+1];
		i++;
	    }
	    argv[i] = NULL;
	    return 1;
	}
    }
    return 0;
}

static char *get_and_remove_option2(int *argc, char **argv, 
				    static char *option)
{
    char *ret;
    int i;

    for (i = 1; i < *argc; i++) {
	if (strcmp(argv[i], option) == 0) {
	    if (i+1 < *argc) {
		ret = argv[i+1];
		(*argc) -= 2;
		while (i < *argc) {
		    argv[i] = argv[i+2];
		    i++;
		}
		argv[i] = NULL;
		return ret;
	    }
	}
    }
    return NULL;
}
    

/************************** OS info *******************************/

/* Used by erlang:info/1. */
/* (This code was formerly in drv.XXX/XXX_os_drv.c) */

char os_type[] = "win32";

void
os_flavor(namebuf, size)
char* namebuf;			/* Where to return the name. */
unsigned size;			/* Size of name buffer. */
{
    switch (int_os_version.dwPlatformId) {
    case VER_PLATFORM_WIN32_WINDOWS:
	strcpy(namebuf, "windows");
	break;
    case VER_PLATFORM_WIN32_NT:
	strcpy(namebuf, "nt");
	break;
    default:			/* Can't happen. */
	strcpy(namebuf, "unknown");
	break;
    }
}

void
os_version(pMajor, pMinor, pBuild)
int* pMajor;			/* Pointer to major version. */
int* pMinor;			/* Pointer to minor version. */
int* pBuild;			/* Pointer to build number. */
{
  *pMajor = int_os_version.dwMajorVersion;
  *pMinor = int_os_version.dwMinorVersion;
  *pBuild = int_os_version.dwBuildNumber;
}

void init_getenv_state(state)
GETENV_STATE *state;
{
   *state = NULL;
}

char *getenv_string(state0)
GETENV_STATE *state0;
{
   char *state = (char *) *state0;
   char *cp;
   int len;

   if (state == NULL)
      state = (char *) GetEnvironmentStrings();

   if (*state == '\0')
      return NULL;

   len = strlen(state);
   cp = state;
   state += len+1;

   *state0 = (GETENV_STATE) state;

   return cp;
}

/************************** Port I/O *******************************/

/* I. Common stuff */

#define TMP_BUF_MAX (tmp_buf_size - 1024)
static byte *tmp_buf;
static int tmp_buf_size;
int cerr_pos;

/* II. The spawn/fd/vanilla drivers */

/*
 * Definitions for driver flags.
 */

#define DF_OVR_READY	1	/* Overlapped result is ready. */
#define DF_EXIT_THREAD	2	/* The thread should exit. */
#define DF_XLAT_CR	4	/* The thread should translate CRs. */

#define OV_BUFFER_PTR(dp) ((LPVOID) ((dp)->ov.Internal))
#define OV_NUM_TO_READ(dp) ((dp)->ov.InternalHigh)

/*
 * This data is used to make overlapped I/O operations work on both
 * Windows NT (using true overlapped I/O) and Windows 95 (using threads).
 */

typedef struct async_io {
  unsigned flags;		/* Driver flags, definitions found above. */
  HANDLE thread;		/* If -1, overlapped I/O is used (Windows NT).
				 * Otherwise, it is the handle of the thread used
				 * for simulating overlapped I/O (Windows 95 and
				 * the console for Windows NT).
				 */
  HANDLE fd;			/* Handle for file or pipe. */
  OVERLAPPED ov;		/* Control structure for overlapped reading.
				 * When overlapped reading is simulated with
				 * a thread, the fields are used as follows:
				 *   ov.Internal - Read buffer.
				 *   ov.InternalHigh - Number of bytes to read.
				 * See macros above.
				 */
  HANDLE ioAllowed;		/* The thread will wait for this event
				 * before starting a new read or write.
				 */
  DWORD pendingError;		/* Used to delay presentating an error to Erlang
				 * until the check_io function is entered.
				 */
  DWORD bytesTransferred;	/* Bytes read or write in the last operation.
				 * Valid only when DF_OVR_READY is set.
				 */
} AsyncIo;


/*
 * Input thread for fd_driver (if fd_driver is running).
 */
static AsyncIo* fd_driver_input = NULL;

/*
 * This data is used by the spawn and vanilla drivers.
 * There will be one entry for each port, even if the input
 * and output HANDLES are different.  Since handles are not
 * guaranteed to be small numbers in Win32, we cannot index
 * with them.  I.e. the index for each entry is not equal to
 * none of the file handles.
 */

typedef struct driver_data {
    int totalNeeded;		/* Total number of bytes needed to fill
				 * up the packet header or packet. */
    int bytesInBuffer;		/* Number of bytes read so far in
				 * the input buffer.
				 */
    int inBufSize;		/* Size of input buffer. */
    byte *inbuf;		/* Buffer to use for overlapped read. */
    byte *outbuf;		/* Buffer to use for overlapped write. */
    ErlDrvPort port_num;		/* The port number. */
    int packet_bytes;		/* 0: continous stream, 1, 2, or 4: the number
				 * of bytes in the packet header.
				 */
    HANDLE port_pid;		/* PID of the port process. */
    AsyncIo in;			/* Control block for overlapped reading. */
    AsyncIo out;		/* Control block for overlapped writing. */
    int report_exit;            /* Do report exit status for the port */
} DriverData;

static DriverData* driver_data;	/* Pointer to array of driver data. */

/* Driver interfaces */
static ErlDrvData spawn_start(ErlDrvPort, char*, SysDriverOpts*);
static ErlDrvData fd_start(ErlDrvPort, char*, SysDriverOpts*);
static ErlDrvData vanilla_start(ErlDrvPort, char*, SysDriverOpts*);
static int spawn_init(void);
static void fd_stop(ErlDrvData);
static void stop(ErlDrvData);
static void output(ErlDrvData, char*, int);
static void ready_input(ErlDrvData, ErlDrvEvent);
static void ready_output(ErlDrvData, ErlDrvEvent);

const struct erl_drv_entry spawn_driver_entry = {
    spawn_init,
    spawn_start,
    stop,
    output,
    ready_input,
    ready_output,
    "spawn",
    NULL, /* finish */
    NULL, /* handle */
    NULL, /* timeout */
    NULL, /* outputv */
    NULL /* ready_async */
};

extern int null_func(void);

const struct erl_drv_entry fd_driver_entry = {
    null_func,
    fd_start,
    fd_stop,
    output,
    ready_input,
    ready_output,
    "fd",
    NULL, /* finish */
    NULL, /* handle */
    NULL, /* timeout */
    NULL, /* outputv */
    NULL /* ready_async */
};

const struct erl_drv_entry vanilla_driver_entry = {
    null_func,
    vanilla_start,
    stop,
    output,
    ready_input,
    ready_output,
    "vanilla",
    NULL, /* finish */
    NULL, /* handle */
    NULL, /* timeout */
    NULL, /* outputv */
    NULL /* ready_async */
};

#ifdef USE_THREADS

static int  async_drv_init(void);
static ErlDrvData async_drv_start(ErlDrvPort, char*, SysDriverOpts*);
static void async_drv_stop(ErlDrvData);
static void async_drv_input(ErlDrvData, ErlDrvEvent);

/* INTERNAL use only */

void null_output(ErlDrvData drv_data, char* buf, int len)
{
}

void null_ready_output(ErlDrvData drv_data, ErlDrvEvent event)
{
}

struct erl_drv_entry async_driver_entry = {
    async_drv_init,
    async_drv_start,
    async_drv_stop,
    null_output,
    async_drv_input,
    null_ready_output,
    "async",
    NULL, /* finish */
    NULL, /* handle */
    NULL, /* timeout */
    NULL, /* outputv */
    NULL /* ready_async */
};

#endif

/*
 * Initialises a DriverData structure.
 *
 * Results: Returns a pointer to a DriverData structure, or NULL
 * if the initialsation failed.
 */

static DriverData*
new_driver_data(port_num, packet_bytes, wait_objs_required, use_threads)
    int port_num;		/* The port number. */
    int packet_bytes;		/* Number of bytes in header. */
    int wait_objs_required;	/* The number objects this port is going
				/* wait for (typically 1 or 2). */
    int use_threads;		/* TRUE if threads are intended to be used. */
{
    DriverData* dp;
    
    DEBUGF(("new_driver_data(port_num %d, pb %d)\n",
	    port_num, packet_bytes));

    /*
     * We used to test first at all that there is enough room in the
     * array used by WaitForMultipleObjects(), but that is not necessary
     * any more, since driver_select() can't fail.
     */

    /*
     * Search for a free slot.
     */

    for (dp = driver_data; dp < driver_data+max_files; dp++) {
	if (dp->port_num == PORT_FREE) {
	    dp->bytesInBuffer = 0;
	    dp->totalNeeded = packet_bytes;
	    dp->inBufSize = PORT_BUFSIZ;
	    dp->inbuf = sys_alloc(dp->inBufSize);
	    if (dp->inbuf == NULL)
		return NULL;
	    dp->outbuf = NULL;
	    dp->port_num = port_num;
	    dp->packet_bytes = packet_bytes;
	    dp->port_pid = INVALID_HANDLE_VALUE;

	    if (init_async_io(&dp->in, use_threads) == -1)
		break;
	    if (init_async_io(&dp->out, use_threads) == -1)
		break;
	    return dp;
	}
    }

    /*
     * Error or no free driver data.
     */

    if (dp < driver_data+max_files) {
	release_async_io(&dp->in);
	release_async_io(&dp->out);
    }
    return NULL;
}

static void
release_driver_data(DriverData* dp)
{
    if (dp->inbuf != NULL)
	sys_free(dp->inbuf);
    dp->inbuf = NULL;

    if (dp->outbuf != NULL)
	sys_free(dp->outbuf);
    dp->outbuf = NULL;

    if (dp->port_pid != INVALID_HANDLE_VALUE) {
	CloseHandle(dp->port_pid);
	dp->port_pid = INVALID_HANDLE_VALUE;
    }

    release_async_io(&dp->in);
    release_async_io(&dp->out);

    /*
     * This must be last, because this function might be executed from
     * the exit thread.
     */

    dp->port_num = PORT_FREE;
}

/*
 * Stores input and output file descriptors in the DriverData structure,
 * and calls driver_select().
 *
 * This function fortunately can't fail!
 */

static ErlDrvData
set_driver_data(dp, ifd, ofd, read_write, report_exit)
    DriverData* dp;
    HANDLE ifd;
    HANDLE ofd;
    int read_write;
    int report_exit;
{
    int index = dp - driver_data;
    int result;

    dp->in.fd = ifd;
    dp->out.fd = ofd;
    dp->report_exit = report_exit;

    if (read_write & DO_READ) {
	result = driver_select(dp->port_num, (ErlDrvEvent)dp->in.ov.hEvent,
			       DO_READ, 1);
	ASSERT(result != -1);
	async_read_file(&dp->in, dp->inbuf, dp->inBufSize);
    }

    if (read_write & DO_WRITE) {
	result = driver_select(dp->port_num, (ErlDrvEvent)dp->out.ov.hEvent,
			       DO_WRITE, 1);
	ASSERT(result != -1);
    }
    return (ErlDrvData)index;
}

/*
 * Initialises an AsyncIo structure.
 */

static int
init_async_io(AsyncIo* aio, int use_threads)
{
    aio->flags = 0;
    aio->thread = (HANDLE) -1;
    aio->fd = INVALID_HANDLE_VALUE;
    aio->ov.hEvent = NULL;
    aio->ov.Offset = 0L;
    aio->ov.OffsetHigh = 0L;
    aio->ioAllowed = NULL;
    aio->pendingError = 0;
    aio->bytesTransferred = 0;

    aio->ov.hEvent = CreateManualEvent(FALSE);
    if (aio->ov.hEvent == NULL)
	return -1;
    if (use_threads) {
	aio->ioAllowed = CreateAutoEvent(FALSE);
	if (aio->ioAllowed == NULL)
	    return -1;
    }
    return 0;
}

/*
 * Releases everything allocated in an AsyncIo structure.
 */  

static void
release_async_io(AsyncIo* aio)
{
    aio->flags = 0;

    if (aio->thread != (HANDLE) -1)
	CloseHandle(aio->thread);
    aio->thread = (HANDLE) -1;

    if (aio->fd != INVALID_HANDLE_VALUE)
	CloseHandle(aio->fd);
    aio->fd = INVALID_HANDLE_VALUE;

    if (aio->ov.hEvent != NULL)
	CloseHandle(aio->ov.hEvent);
    aio->ov.hEvent = NULL;

    if (aio->ioAllowed != NULL)
	CloseHandle(aio->ioAllowed);
    aio->ioAllowed = NULL;
}

/* ----------------------------------------------------------------------
 * async_read_file --
 *	Initiaties an asynchronous file read, or simulates that using
 *	the thread associated with this driver data.  To get the results,
 *	call get_overlapped_result().
 *
 * Results:
 *	None.
 * ----------------------------------------------------------------------
 */

static void
async_read_file(aio, buf, numToRead)
    AsyncIo* aio;		/* Pointer to driver data. */
    LPVOID buf;			/* Pointer to buffer to receive data. */
    DWORD numToRead;		/* Number of bytes to read. */
{
    aio->pendingError = NO_ERROR;
    if (aio->thread != (HANDLE) -1) {
	DEBUGF(("async_read_file: signaling thread 0x%x, event 0x%x\n",
		aio->thread, aio->ioAllowed));
	OV_BUFFER_PTR(aio) = buf;
	OV_NUM_TO_READ(aio) = numToRead;
	ResetEvent(aio->ov.hEvent);
	SetEvent(aio->ioAllowed);
    } else if (ReadFile(aio->fd, buf, numToRead,
			&aio->bytesTransferred, &aio->ov)) {
	DEBUGF(("async_read_file: ReadFile() suceeded: %d bytes\n",
		aio->bytesTransferred));
	aio->flags |= DF_OVR_READY;
	SetEvent(aio->ov.hEvent);
    } else {
	DWORD error = GetLastError();
	if (error != ERROR_IO_PENDING) {
	    aio->pendingError = error;
	    SetEvent(aio->ov.hEvent);
	}
	DEBUGF(("async_read_file: ReadFile() -> %s\n", win32_errorstr(error)));
    }
}

/* ----------------------------------------------------------------------
 * async_write_file --
 *	Initiaties an asynchronous file write, or simulates that using
 *	the output thread associated with this driver data. 
 *	To get the results, call get_overlapped_result().
 *
 * Results:
 *	None.
 * ----------------------------------------------------------------------
 */
static int
async_write_file(aio, buf, numToWrite)
    AsyncIo* aio;		/* Pointer to async control block. */
    LPVOID buf;			/* Pointer to buffer with data to write. */
    DWORD numToWrite;		/* Number of bytes to write. */
{
    aio->pendingError = NO_ERROR;
    if (aio->thread != (HANDLE) -1) {
	DEBUGF(("async_write_file: signaling thread 0x%x, event 0x%x\n",
		aio->thread, aio->ioAllowed));
	OV_BUFFER_PTR(aio) = buf;
	OV_NUM_TO_READ(aio) = numToWrite;
	ResetEvent(aio->ov.hEvent);
	SetEvent(aio->ioAllowed);
    } else if (WriteFile(aio->fd, buf, numToWrite,
			 &aio->bytesTransferred, &aio->ov)) {
	DEBUGF(("async_write_file: WriteFile() suceeded: %d bytes\n",
		aio->bytesTransferred));
	ResetEvent(aio->ov.hEvent);
	return TRUE;
    } else {
	DWORD error = GetLastError();
	if (error != ERROR_IO_PENDING) {
	    aio->pendingError = error;
	    SetEvent(aio->ov.hEvent);
	}
	DEBUGF(("async_write_file: WriteFile() -> %s\n", win32_errorstr(error)));
    }
    return FALSE;
}

/* ----------------------------------------------------------------------
 * get_overlapped_result --
 *
 * Results:
 *	Returns the error code for the overlapped result, or NO_ERROR
 *	if no error.
 * ----------------------------------------------------------------------
 */
static int
get_overlapped_result(aio, pBytesRead, wait)
    AsyncIo* aio;		/* Pointer to async control block. */
    LPDWORD pBytesRead;		/* Where to place the number of bytes
				 * transferred.
				 */
    BOOL wait;			/* If true, wait until result is ready. */
{
    DWORD error = NO_ERROR;	/* Error status from last function. */

    if (aio->thread != (HANDLE) -1) {

	/*
	 * Simulate overlapped io with a thread.
	 */
	DEBUGF(("get_overlapped_result: about to wait for event 0x%x\n",
		aio->ov.hEvent));
	error = WaitForSingleObject(aio->ov.hEvent, wait ? INFINITE : 0);
	switch (error) {
	case WAIT_OBJECT_0:
	    error = aio->pendingError;
	    aio->pendingError = NO_ERROR;
	    *pBytesRead = aio->bytesTransferred;
	    ResetEvent(aio->ov.hEvent);
	    DEBUGF(("get_overlapped_result -> %s\n",
		    win32_errorstr(error)));
	    return error;
	case WAIT_TIMEOUT:
	    DEBUGF(("get_overlapped_result -> %s\n",
		    ERROR_IO_INCOMPLETE));
	    return ERROR_IO_INCOMPLETE;
	case WAIT_FAILED:		/* XXX: Shouldn't happen? */
	    error = GetLastError();
	    DEBUGF(("get_overlapped_result (WAIT_FAILED) -> %s\n",
		    win32_errorstr(error)));
	    return error;
	}
    } else if (aio->pendingError != NO_ERROR) { /* Pending error. */
	error = aio->pendingError;
	aio->pendingError = NO_ERROR;
	ResetEvent(aio->ov.hEvent);
	DEBUGF(("get_overlapped_result: pending error: %s\n",
		win32_errorstr(error)));
	return error;
    } else if (aio->flags & DF_OVR_READY) { /* Operation succeded. */
	aio->flags &= ~DF_OVR_READY;
	*pBytesRead = aio->bytesTransferred;
	ResetEvent(aio->ov.hEvent);
	DEBUGF(("get_overlapped_result: delayed success: %d bytes\n",
		aio->bytesTransferred));
    } else if (!GetOverlappedResult(aio->fd, &aio->ov, pBytesRead, wait)) {
	error = GetLastError();
	ResetEvent(aio->ov.hEvent);
	DEBUGF(("get_overlapped_result: error: %s\n", win32_errorstr(error)));
	return error;
    } else {			/* Success. */
	DEBUGF(("get_overlapped_result: success\n"));
	ResetEvent(aio->ov.hEvent);
    }
    return NO_ERROR;
}
  
static int
spawn_init()
{
    int i;
  
    driver_data = (struct driver_data *)
	checked_alloc(max_files * sizeof(struct driver_data));
    for (i = 0; i < max_files; i++)
	driver_data[i].port_num = PORT_FREE;
    return 0;
}

static ErlDrvData
spawn_start(ErlDrvPort port_num, char* name, SysDriverOpts* opts)
{
    HANDLE hToChild = INVALID_HANDLE_VALUE; /* Write handle to child. */
    HANDLE hFromChild = INVALID_HANDLE_VALUE; /* Read handle from child. */
    HANDLE hChildStdin = INVALID_HANDLE_VALUE;		/* Child's stdin. */
    HANDLE hChildStdout = INVALID_HANDLE_VALUE;	/* Child's stout. */
    HANDLE hChildStderr = INVALID_HANDLE_VALUE;	/* Child's sterr. */
    DriverData* dp;		/* Pointer to driver data. */
    ErlDrvData retval = ERL_DRV_ERROR_GENERAL; /* Return value. */
    int ok;
    int neededSelects = 0;
    SECURITY_ATTRIBUTES sa = {sizeof(SECURITY_ATTRIBUTES), NULL, TRUE};
    
    if (opts->read_write & DO_READ)
	neededSelects++;
    if (opts->read_write & DO_WRITE)
	neededSelects++;

    if ((dp = new_driver_data(port_num, opts->packet_bytes, neededSelects,
			      !use_named_pipes)) == NULL)
	return ERL_DRV_ERROR_GENERAL;

    /*
     * Create two pipes to communicate with the port program.
     */

    if (opts->read_write & DO_READ) {
	if (!create_pipe(&hFromChild, &hChildStdout, FALSE))
	    goto error;
    } else {
	hChildStdout = CreateFile("nul", GENERIC_WRITE, 0,
				  &sa, OPEN_EXISTING,
				  FILE_ATTRIBUTE_NORMAL, NULL);
	DEBUGF(("Created nul file for hChildStdout = %d\n",hChildStdout));
    }
    if (opts->read_write & DO_WRITE) {
	if (!create_pipe(&hChildStdin, &hToChild, TRUE)) {
	    CloseHandle(hFromChild);
	    CloseHandle(hChildStdout);
	    goto error;
	}
    } else {
	hChildStdin = CreateFile("nul", GENERIC_READ, 0,
				 &sa, OPEN_EXISTING,
				 FILE_ATTRIBUTE_NORMAL, NULL);
	DEBUGF(("Created nul file for hChildStdin = %d\n",hChildStdin));
    }	

    /*
     * Make sure that standard error is valid handle, because a Command Prompt
     * window not work properly otherwise.  We leave standard error alone if
     * it is okay and no redirection was specified.
     */
    hChildStderr = GetStdHandle(STD_ERROR_HANDLE);
    if (opts->redir_stderr) {
	hChildStderr = hChildStdout;
    } else if (hChildStderr == INVALID_HANDLE_VALUE || hChildStderr == 0) {
	hChildStderr = CreateFile("nul", GENERIC_WRITE, 0, &sa, OPEN_EXISTING,
				  FILE_ATTRIBUTE_NORMAL, NULL);
    }

    /*
     * Spawn the port program.
     */

    DEBUGF(("Spawning \"%s\"\n", name));
    ok = CreateChildProcess(name, 
			    hChildStdin, 
			    hChildStdout,
			    hChildStderr,
			    &dp->port_pid,
			    opts->hide_window,
			    (LPVOID) opts->envir,
			    (LPTSTR) opts->wd);
    CloseHandle(hChildStdin);
    CloseHandle(hChildStdout);

    if (!ok) {
	dp->port_pid = INVALID_HANDLE_VALUE;
    } else {
	if (!use_named_pipes) {
	    if ((opts->read_write & DO_READ) &&
		!create_file_thread(&dp->in, DO_READ))
		goto error;
	    if ((opts->read_write & DO_WRITE) &&
		!create_file_thread(&dp->out, DO_WRITE)) {
		dp->in.flags = DF_EXIT_THREAD;
		SetEvent(dp->in.ioAllowed);
		WaitForSingleObject(dp->in.thread, INFINITE);
		dp->in.thread = (HANDLE) -1;
		goto error;
	    }
	}
	retval = set_driver_data(dp, hFromChild, hToChild, opts->read_write,
				 opts->exit_status);
    }
    
    if (retval != ERL_DRV_ERROR_GENERAL)
	return retval;
    
 error:
    if (hFromChild != INVALID_HANDLE_VALUE)
	CloseHandle(hFromChild);
    if (hToChild != INVALID_HANDLE_VALUE)
	CloseHandle(hToChild);
    release_driver_data(dp);
    return ERL_DRV_ERROR_GENERAL;
}

static int
create_file_thread(AsyncIo* aio, int mode)
{
    DWORD tid;			/* Id for thread. */

    aio->thread = (HANDLE)
	_beginthreadex(NULL, 0, 
		       (mode & DO_WRITE) ? threaded_writer : threaded_reader,
		       aio, 0, &tid);

    return aio->thread != (HANDLE) -1;
}

/* 
 *  A helper function used by CreateChildProcess().
 *  Parses a command line with arguments and returns the length of the
 *  first part containing the program name.
 *  Example: input = "\"Program Files\"\\erl arg1 arg2"
 *  gives 19 as result.
 *  The length returned is equivalent with length(argv[0]) if the
 *  comman line should have been prepared by _setargv for the main function
*/
int parse_command(char* cmd){
#define NORMAL 2
#define STRING 1
#define STOP 0
    int i =0;
    int state = NORMAL;
    while (cmd[i]) {
	switch (cmd[i]) {
	case '"':
	    if (state == NORMAL) 
		state = STRING;
	    else
		state = NORMAL;
	    break;
	case '\\':
	    if ((state == STRING) && (cmd[i+1]=='"'))
		i++;
	    break;
	case ' ':
	    if (state == NORMAL)
		state = STOP;
	    break;
	default:
	    break;
	}
	if (state == STOP) {
	    return i;
	}
	i++;
    }
    return i;
}

/*
 *----------------------------------------------------------------------
 *
 * CreateChildProcess --
 *
 *	Create a child process that has pipes as its 
 *	standard input, output, and error.  The child process runs
 *	synchronously under Win32s and asynchronously under Windows NT
 *	and Windows 95, and runs with the same environment variables
 *	as the creating process.
 *
 *	The complete Windows search path is searched to find the specified 
 *	executable.  If an executable by the given name is not found, 
 *	automatically tries appending ".com", ".exe", and ".bat" to the 
 *	executable name.
 *
 * Results:
 *	The return value is FALSE if there was a problem creating the child process.  
 *      Otherwise, the return value is 0 and *phPid is
 *	filled with the process id of the child process.
 * 
 * Side effects:
 *	A process is created.
 *	
 *----------------------------------------------------------------------
 */

static BOOL
CreateChildProcess(origcmd, hStdin, hStdout, hStderr, phPid, hide, env, wd)
    char *origcmd;		/* Command line for child process (including
				 * name of executable).
				 */
    HANDLE hStdin;		/* The standard input handle for child. */
    HANDLE hStdout;		/* The standard output handle for child. */
    HANDLE hStderr;		/* The standard error handle for child. */
    LPHANDLE phPid;		/* Pointer to variable to received PID. */
    BOOL hide;			/* Hide the window unconditionally. */
    LPVOID env;			/* Environment for the child */
    LPTSTR wd;			/* Working dir for the child */
{ 
    PROCESS_INFORMATION piProcInfo = {0};
    STARTUPINFO siStartInfo = {0};
    BOOL ok = FALSE;
    int applType;
    /* Not to be changed for different types of executables */
    int staticCreateFlags = GetPriorityClass(GetCurrentProcess()); 
    int createFlags = DETACHED_PROCESS;
    char newcmdline[2048];
    char execPath[MAX_PATH];
    int cmdlength;
    char* thecommand;
    HANDLE hProcess = GetCurrentProcess();
    
    siStartInfo.cb = sizeof(STARTUPINFO); 
    siStartInfo.dwFlags = STARTF_USESTDHANDLES;
    siStartInfo.hStdInput = hStdin;
    siStartInfo.hStdOutput = hStdout;
    siStartInfo.hStdError = hStderr;

    /*
     * Parse out the program name from the command line (it can be quoted and
     * contain spaces).
     */
    cmdlength = parse_command(origcmd);
    thecommand = checked_alloc(cmdlength+1);
    strncpy(thecommand, origcmd, cmdlength);
    thecommand[cmdlength] = '\0';
    DEBUGF(("spawn command: %s\n", thecommand));
    
    applType = ApplicationType(thecommand, execPath);
    DEBUGF(("ApplicationType returned for (%s) is %d\n", thecommand, applType));
    sys_free(thecommand);
    if (applType == APPL_NONE) {
	return FALSE;
    }
    newcmdline[0] = '\0'; 

    if (int_os_version.dwPlatformId == VER_PLATFORM_WIN32_NT) {
	if (applType == APPL_DOS) {
	    /*
	     * Under NT, 16-bit DOS applications will not run unless they
	     * can be attached to a console.  Run the 16-bit program as
	     * a normal process inside of a hidden console application,
	     * and then run that hidden console as a detached process.
	     */
	  
	    siStartInfo.wShowWindow = SW_HIDE;
	    siStartInfo.dwFlags |= STARTF_USESHOWWINDOW;
	    createFlags = CREATE_NEW_CONSOLE;
	    strcat(newcmdline, "cmd.exe /c ");
	} else if (hide) {
	    DEBUGF(("hiding window\n"));
	    siStartInfo.wShowWindow = SW_HIDE;
	    siStartInfo.dwFlags |= STARTF_USESHOWWINDOW;
	    createFlags = 0;
	}
    } else {
	/* Windows 95 */
	if (applType == APPL_DOS) {
	    /*
	     * Under Windows 95, 16-bit DOS applications do not work well 
	     * with pipes:
	     *
	     * 1. EOF on a pipe between a detached 16-bit DOS application 
	     * and another application is not seen at the other
	     * end of the pipe, so the listening process blocks forever on 
	     * reads.  This inablity to detect EOF happens when either a 
	     * 16-bit app or the 32-bit app is the listener.  
	     *
	     * 2. If a 16-bit DOS application (detached or not) blocks when 
	     * writing to a pipe, it will never wake up again, and it
	     * eventually brings the whole system down around it.
	     *
	     * The 16-bit application is run as a normal process inside
	     * of a hidden helper console app, and this helper may be run
	     * as a detached process.  If any of the stdio handles is
	     * a pipe, the helper application accumulates information 
	     * into temp files and forwards it to or from the DOS 
	     * application as appropriate.  This means that DOS apps 
	     * must receive EOF from a stdin pipe before they will actually
	     * begin, and must finish generating stdout or stderr before 
	     * the data will be sent to the next stage of the pipe.
	     */
	  
	    siStartInfo.wShowWindow = SW_HIDE;
	    siStartInfo.dwFlags |= STARTF_USESHOWWINDOW;
	    createFlags = CREATE_NEW_CONSOLE;
	    strcat(newcmdline, "erl_stub16 ");
	} else if (hide) {
	    DEBUGF(("hiding window\n"));
	    siStartInfo.wShowWindow = SW_HIDE;
	    siStartInfo.dwFlags |= STARTF_USESHOWWINDOW;
	    createFlags = 0;
	}
    }

    strcat(newcmdline, execPath);
    strcat(newcmdline, origcmd+cmdlength);

    DEBUGF(("Creating child process: %s, createFlags = %d\n", newcmdline, createFlags));
    ok = CreateProcess(NULL, 
		       newcmdline, 
		       NULL, 
		       NULL, 
		       TRUE, 
		       createFlags | staticCreateFlags, 
		       env, 
		       wd, 
		       &siStartInfo, 
		       &piProcInfo);
    if (!ok) {
	DEBUGF(("CreateProcess failed: %s\n", last_error()));
	return FALSE;
    }
    CloseHandle(piProcInfo.hThread); /* Necessary to avoid resource leak. */
    *phPid = piProcInfo.hProcess;
    
    if (applType == APPL_DOS) {
	WaitForSingleObject(hProcess, 50);
    }
    
    /* 
     * "When an application spawns a process repeatedly, a new thread 
     * instance will be created for each process but the previous 
     * instances may not be cleaned up.  This results in a significant 
     * virtual memory loss each time the process is spawned.  If there 
     * is a WaitForInputIdle() call between CreateProcess() and
     * CloseHandle(), the problem does not occur." PSS ID Number: Q124121
     */
    
    WaitForInputIdle(piProcInfo.hProcess, 5000);
    
    return ok;
}

static int
create_pipe(phRead, phWrite, inheritRead)
     LPHANDLE phRead;		/* Where to store the handle to the read end
				 * of the pipe. */
     LPHANDLE phWrite;		/* Where to store the handle to the write end
				 * of the pipe. */
     BOOL inheritRead;		/* If TRUE, the read handle will be inheritable;
				 * otherwise the write handle will be
				 * inheritable.
				 */
{
    SECURITY_ATTRIBUTES sa = {sizeof(SECURITY_ATTRIBUTES), NULL, TRUE};
    static long calls = 0;	/* Pipe instance pointer. */
    char pipe_name[128];	/* Name of pipe. */

    /*
     * If we should't use named pipes, create anonmous pipes.
     */

    if (!use_named_pipes) {
	int success;
	HANDLE non_inherited;	/* Non-inherited copy of handle. */

	if (!CreatePipe(phRead, phWrite, &sa, 0)) {
	    DEBUGF(("Error creating anonyomous pipe: %s\n", last_error()));
	    return FALSE;
	}

	if (inheritRead) {
	    success = DuplicateHandle(GetCurrentProcess(), *phWrite,
				      GetCurrentProcess(), &non_inherited, 0,
				      FALSE, DUPLICATE_SAME_ACCESS);
	    CloseHandle(*phWrite);
	    *phWrite = non_inherited;
	} else {
	    success = DuplicateHandle(GetCurrentProcess(), *phRead,
				      GetCurrentProcess(), &non_inherited, 0,
				      FALSE, DUPLICATE_SAME_ACCESS);
	    CloseHandle(*phRead);
	    *phRead = non_inherited;
	}
	return success;
    }


    /*
     * Otherwise, create named pipes.
     */
    
    sprintf(pipe_name, "\\\\.\\pipe\\erlang44_%d_%d",
	    getpid(), calls++);

    DEBUGF(("Creating pipe %s\n", pipe_name));
    sa.bInheritHandle = inheritRead;
    if ((*phRead = CreateNamedPipe(pipe_name,
				   PIPE_ACCESS_INBOUND | FILE_FLAG_OVERLAPPED,
				   PIPE_TYPE_BYTE | PIPE_READMODE_BYTE,
				   1,
				   0,
				   0,
				   2000,
				   &sa)) == NULL) {
	DEBUGF(("Error creating pipe: %s\n", last_error()));
	return FALSE;
    }
  
    sa.bInheritHandle = !inheritRead;
    if ((*phWrite = CreateFile(pipe_name,
			       GENERIC_WRITE,
			       0, /* No sharing */
			       &sa,
			       OPEN_EXISTING,
			       FILE_ATTRIBUTE_NORMAL | FILE_FLAG_OVERLAPPED,
			       NULL)) == INVALID_HANDLE_VALUE) {
	CloseHandle(*phRead);
	DEBUGF(("Error opening other end of pipe: %s\n", last_error()));
	return FALSE;
    }
    return TRUE;
}




static int ApplicationType(originalName, fullPath)
    const char *originalName;	/* Name of the application to find. */
    char fullPath[MAX_PATH];	/* Filled with complete path to 
				 * application. */
{
    int applType, i;
    HANDLE hFile;
    char *ext, *rest;
    char buf[2];
    DWORD read;
    IMAGE_DOS_HEADER header;
    static char extensions[][5] = {"", ".com", ".exe", ".bat"};

    /* Look for the program as an external program.  First try the name
     * as it is, then try adding .com, .exe, and .bat, in that order, to
     * the name, looking for an executable.
     * NOTE! that we does not support execution of .com programs on Windows NT
     * 
     *
     * Using the raw SearchPath() procedure doesn't do quite what is 
     * necessary.  If the name of the executable already contains a '.' 
     * character, it will not try appending the specified extension when
     * searching (in other words, SearchPath will not find the program 
     * "a.b.exe" if the arguments specified "a.b" and ".exe").   
     * So, first look for the file as it is named.  Then manually append 
     * the extensions, looking for a match.  
     */

    applType = APPL_NONE;
    for (i = 0; i < (int) (sizeof(extensions) / sizeof(extensions[0])); i++) {
	lstrcpyn(fullPath, originalName, MAX_PATH - 5);
        lstrcat(fullPath, extensions[i]);
	
	SearchPath(NULL, fullPath, NULL, MAX_PATH, fullPath, &rest);

	/*
	 * Ignore matches on directories or data files, return if identified
	 * a known type.
	 */

	if (GetFileAttributes(fullPath) & FILE_ATTRIBUTE_DIRECTORY) {
	    continue;
	}

	ext = strrchr(fullPath, '.');
	if ((ext != NULL) && (strcmpi(ext, ".bat") == 0)) {
	    applType = APPL_DOS;
	    break;
	}

	hFile = CreateFile(fullPath, GENERIC_READ, FILE_SHARE_READ, NULL, 
		OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
	if (hFile == INVALID_HANDLE_VALUE) {
	    continue;
	}

	header.e_magic = 0;
	ReadFile(hFile, (void *) &header, sizeof(header), &read, NULL);
	if (header.e_magic != IMAGE_DOS_SIGNATURE) {
	    /* 
	     * Doesn't have the magic number for relocatable executables.  If 
	     * filename ends with .com, assume it's a DOS application anyhow.
	     * Note that we didn't make this assumption at first, because some
	     * supposed .com files are really 32-bit executables with all the
	     * magic numbers and everything.  
	     */

	    CloseHandle(hFile);
	    if ((ext != NULL) && (strcmpi(ext, ".com") == 0)) {
		applType = APPL_DOS;
		break;
	    }
	    continue;
	}
	if (header.e_lfarlc != sizeof(header)) {
	    /* 
	     * All Windows 3.X and Win32 and some DOS programs have this value
	     * set here.  If it doesn't, assume that since it already had the 
	     * other magic number it was a DOS application.
	     */

	    CloseHandle(hFile);
	    applType = APPL_DOS;
	    break;
	}

	/* 
	 * The DWORD at header.e_lfanew points to yet another magic number.
	 */

	buf[0] = '\0';
	SetFilePointer(hFile, header.e_lfanew, NULL, FILE_BEGIN);
	ReadFile(hFile, (void *) buf, 2, &read, NULL);
	CloseHandle(hFile);

	if ((buf[0] == 'L') && (buf[1] == 'E')) {
	    applType = APPL_DOS;
	} else if ((buf[0] == 'N') && (buf[1] == 'E')) {
	    applType = APPL_WIN3X;
	} else if ((buf[0] == 'P') && (buf[1] == 'E')) {
	    applType = APPL_WIN32;
	} else {
	    continue;
	}
	break;
    }

    if (applType == APPL_NONE) {
	return APPL_NONE;
    }

    if ((applType == APPL_DOS) || (applType == APPL_WIN3X)) {
	/* 
	 * Replace long path name of executable with short path name for 
	 * 16-bit applications.  Otherwise the application may not be able
	 * to correctly parse its own command line to separate off the 
	 * application name from the arguments.
	 */

	GetShortPathName(fullPath, fullPath, MAX_PATH);
    }
    return applType;
}

/*
 * Thread function used to emulate overlapped reading.
 */

DWORD WINAPI
threaded_reader(LPVOID param)
{
    AsyncIo* aio = (AsyncIo *) param;
    HANDLE thread = GetCurrentThread();
    char* buf;
    DWORD numToRead;
  
    for (;;) {
	WaitForSingleObject(aio->ioAllowed, INFINITE);
	if (aio->flags & DF_EXIT_THREAD)
	    break;
	buf = OV_BUFFER_PTR(aio);
	numToRead = OV_NUM_TO_READ(aio);
	aio->pendingError = 0;
	if (!ReadFile(aio->fd, buf, numToRead, &aio->bytesTransferred, NULL))
	    aio->pendingError = GetLastError();
	else if (aio->flags & DF_XLAT_CR) {
	    char *s;
	    int n;
	    
	    n = aio->bytesTransferred;
	    for (s = buf; s < buf+n; s++) {
		if (*s == '\r') {
		    if (s < buf + n - 1 && s[1] == '\n') {
			memmove(s, s+1, (buf+n - s - 1));
			--n;
		    } else {
			*s = '\n';
		    }
		}
	    }
	    aio->bytesTransferred = n;
	}
	SetEvent(aio->ov.hEvent);
	if ((aio->flags & DF_XLAT_CR) == 0 && aio->bytesTransferred == 0) {
	    break;
	}
	if (aio->pendingError != NO_ERROR) {
	    break;
	}
	if (aio->flags & DF_EXIT_THREAD)
	    break;
    }
    return 0;
}

/*
 * Thread function used to emulate overlapped writing
 */

DWORD WINAPI
threaded_writer(LPVOID param)
{
    AsyncIo* aio = (AsyncIo *) param;
    HANDLE thread = GetCurrentThread();
    char* buf;
    DWORD numToWrite;
    int ok;
  
    for (;;) {
	WaitForSingleObject(aio->ioAllowed, INFINITE);
	if (aio->flags & DF_EXIT_THREAD)
	    break;
	buf = OV_BUFFER_PTR(aio);
	numToWrite = OV_NUM_TO_READ(aio);
	aio->pendingError = 0;
	ok = WriteFile(aio->fd, buf, numToWrite, &aio->bytesTransferred, NULL);
	if (!ok)
	    aio->pendingError = GetLastError();
	SetEvent(aio->ov.hEvent);
	if (aio->pendingError != NO_ERROR || aio->bytesTransferred == 0)
	    break;
	if (aio->flags & DF_EXIT_THREAD)
	    break;
    }
    CloseHandle(aio->fd);
    aio->fd = INVALID_HANDLE_VALUE;
    return 0;
}

static HANDLE
translate_fd(int fd)
{
    DWORD access;
    HANDLE handle;

    switch (fd) {
    case 0:
	access = GENERIC_READ;
	handle = GetStdHandle(STD_INPUT_HANDLE);
	break;
    case 1:
	access = GENERIC_WRITE;
	handle = GetStdHandle(STD_OUTPUT_HANDLE);
	break;
    case 2:
	access = GENERIC_WRITE;
	handle = GetStdHandle(STD_ERROR_HANDLE);
	break;
    default:
	return (HANDLE) fd;
    }
    DEBUGF(("translate_fd(%d) -> std(%d)\n", fd, handle));

    if (handle == INVALID_HANDLE_VALUE || handle == 0) {
	handle = CreateFile("nul", access, 0,
			    NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    }
    DEBUGF(("translate_fd(%d) -> %d\n", fd, handle));
    return handle;
}

static ErlDrvData
fd_start(ErlDrvPort port_num, char* name, SysDriverOpts* opts)
{
    DriverData* dp;
    
    opts->ifd = (int) translate_fd(opts->ifd);
    opts->ofd = (int) translate_fd(opts->ofd);
    if ((dp = new_driver_data(port_num, opts->packet_bytes, 2, TRUE)) == NULL)
	return ERL_DRV_ERROR_GENERAL;
    
    if (!create_file_thread(&dp->in, DO_READ)) {
	dp->port_num = PORT_FREE;
	return ERL_DRV_ERROR_GENERAL;
    }

    if (!create_file_thread(&dp->out, DO_WRITE)) {
	dp->port_num = PORT_FREE;
	return ERL_DRV_ERROR_GENERAL;
    }
    
    fd_driver_input = &(dp->in);
    dp->in.flags = DF_XLAT_CR;
    return set_driver_data(dp, opts->ifd, opts->ofd, opts->read_write, 0);
}

static void fd_stop(ErlDrvData d)
{
  int fd = (int)d;
  /*
   * I don't know a clean way to terminate the threads
   * (TerminateThread() doesn't release the stack),
   * so will we'll let the threads live.  Normally, the fd
   * driver is only used to support the -oldshell option,
   * so this shouldn't be a problem in practice.
   *
   * Since we will not attempt to terminate the threads,
   * better not close the input or output files either.
   */

  driver_data[fd].in.thread = (HANDLE) -1;
  driver_data[fd].out.thread = (HANDLE) -1;
  driver_data[fd].in.fd = INVALID_HANDLE_VALUE;
  driver_data[fd].out.fd = INVALID_HANDLE_VALUE;

  /*return */ common_stop(fd);
}

static ErlDrvData
vanilla_start(ErlDrvPort port_num, char* name, SysDriverOpts* opts)
{
    HANDLE fd;
    DriverData* dp;
    DWORD access;		/* Access mode: GENERIC_READ, GENERIC_WRITE. */
    DWORD crFlags;

    access = 0;
    if (opts->read_write == DO_READ)
	access |= GENERIC_READ;
    if (opts->read_write == DO_WRITE)
	access |= GENERIC_WRITE;

    if (opts->read_write == DO_READ)
	crFlags = OPEN_EXISTING;
    else if (opts->read_write == DO_WRITE)
	crFlags = CREATE_ALWAYS;
    else
	crFlags = OPEN_ALWAYS;

    if ((dp = new_driver_data(port_num, opts->packet_bytes, 2, FALSE)) == NULL)
	return ERL_DRV_ERROR_GENERAL;
    fd = CreateFile(name, access, FILE_SHARE_READ | FILE_SHARE_WRITE,
		    NULL, crFlags, FILE_ATTRIBUTE_NORMAL, NULL);
    if (fd == INVALID_HANDLE_VALUE)
	return ERL_DRV_ERROR_GENERAL;
    return set_driver_data(dp, fd, fd, opts->read_write,0);
}

static void
stop(ErlDrvData index)
{
    common_stop((int)index);
}

static void common_stop(int index)
{
    DriverData* dp = driver_data+index;

    DEBUGF(("common_stop(%d)\n", index));

    if (dp->in.ov.hEvent != NULL)
	(void) driver_select(dp->port_num,
			     (ErlDrvEvent)dp->in.ov.hEvent,
			     DO_READ,
			     0);

    if (dp->out.ov.hEvent != NULL)
	(void) driver_select(dp->port_num,
			     (ErlDrvEvent)dp->out.ov.hEvent,
			     DO_WRITE,
			     0);

    if (dp->out.thread == (HANDLE) -1 && dp->in.thread == (HANDLE) -1) {
	release_driver_data(dp);
    } else {
	/*
	 * If there are read or write threads, start a thread which will
	 * wait for them to finish.
	 */
	HANDLE thread;
	DWORD tid;
	dp->port_num = PORT_EXITING;
	thread = (HANDLE *) _beginthreadex(NULL, 0, threaded_exiter, dp, 0, &tid);
	CloseHandle(thread);
    }
}

DWORD WINAPI
threaded_exiter(LPVOID param)
{
    DriverData* dp = (DriverData *) param;
    HANDLE handles[2];
    int i;

    /*
     * Ask the threads to terminated.
     *
     * Note that we can't reliable test the state of the ioAllowed event,
     * because it is an auto reset event.  Therefore, always set the
     * exit flag and signal the event.
     */

    i = 0;
    if (dp->out.thread != (HANDLE) -1) {
	dp->out.flags = DF_EXIT_THREAD;
	SetEvent(dp->out.ioAllowed);
	handles[i++] = dp->out.thread;
    }
    if (dp->in.thread != (HANDLE) -1) {
	dp->in.flags = DF_EXIT_THREAD;
	SetEvent(dp->in.ioAllowed);
	handles[i++] = dp->in.thread;
    }

    /*
     * If we were lucky, the following happened above:
     * 	1) The output thread terminated (and closed the pipe).
     *  2) As a consequence of that, the port program received
     *     EOF on its standard input.
     *  3) Hopefully, because of (2), the port program terminated.
     *  4) Because of (3), the input thread terminated.
     *
     * But this might need some time; therefore, we must wait for
     * both threads to terminate.
     */

    if (i > 0) {
	switch (WaitForMultipleObjects(i, handles, TRUE, 5000)) {
	case WAIT_TIMEOUT:
	    DEBUGF(("Timeout waiting for %d threads failed\n", i));
	    break;
	case WAIT_FAILED:
	    DEBUGF(("Wait for %d threads failed: %s\n",
		    i, win32_errorstr(GetLastError())));
	    break;
	default:
	    break;
	}
    }

    /*
     * Wait for threads to terminate didn't help.  Now use some force.
     * TerminateThread() is *not* a good idea, because it doesn't clean
     * up the thread's stack.
     *
     * Instead we well terminate the port program and wait for the
     * threads to terminate themselves when they receive end of file.
     */

    if (dp->out.thread != (HANDLE) -1) {
	int error;

	if (WaitForSingleObject(dp->out.thread, 0) == WAIT_OBJECT_0) {
	    CloseHandle(dp->out.thread);
	    dp->out.thread = (HANDLE) -1;
	} else if (dp->port_pid != INVALID_HANDLE_VALUE) {
	    DEBUGF(("Killing port process 0x%x (output thread)\n", dp->port_pid));
	    TerminateProcess(dp->port_pid, 0);
	    if (!CloseHandle(dp->port_pid))
	        DEBUGF(("Failed to close output handle!!!\n"));
	    dp->port_pid = INVALID_HANDLE_VALUE;
	    DEBUGF(("Waiting for output thread 0x%x to finish\n", dp->out.thread));
	    error = WaitForSingleObject(dp->out.thread, INFINITE);
	}
    }
    
    if (dp->in.thread != (HANDLE) -1) {
	if (WaitForSingleObject(dp->in.thread, 0) == WAIT_OBJECT_0) {
	    CloseHandle(dp->in.thread);
	    dp->in.thread = (HANDLE) -1;
	} else if (dp->port_pid != INVALID_HANDLE_VALUE) {
	    DEBUGF(("Killing port process 0x%x (input thread)\n", dp->port_pid));
	    TerminateProcess(dp->port_pid, 0);
	    if (!CloseHandle(dp->port_pid))
	        DEBUGF(("Failed to close input handle!!!\n"));
	    dp->port_pid = INVALID_HANDLE_VALUE;

	    DEBUGF(("Waiting for input thread 0x%x to finish\n", dp->in.thread));
	    switch (WaitForSingleObject(dp->in.thread, INFINITE)) {
	    case WAIT_OBJECT_0:
		CloseHandle(dp->in.thread);
		dp->in.thread = (HANDLE) -1;
		break;
	    default:
		DEBUGF(("Wait for input thread to finish failed: %s\n",
			win32_errorstr(GetLastError())));
		break;
	    }
	}
    }

    release_driver_data(dp);
    return 0;
}

/* ----------------------------------------------------------------------
 * output --
 * 	Outputs data from Erlang to the port program.
 *
 * Results:
 *	Returns the actual number of bytes written (including the
 *	packet header) or -1 if an error occurred.
 * ----------------------------------------------------------------------
 */

static void
output(ErlDrvData drv_data, char* buf, int len)
/*     long drv_data;		/* The slot to use in the driver data table.
				 * For Windows NT, this is *NOT* a file handle.
				 * The handle is found in the driver data.
				 */
/*     char *buf;			/* Pointer to data to write to the port program. */
/*     int len;			/* Number of bytes to write. */
{
    DriverData* dp;
    int pb;			/* The header size for this port. */
    int port_num;		/* The actual port number (for diagnostics). */
    char* current;

    dp = driver_data + (int)drv_data;
    if ((port_num = dp->port_num) == -1)
	return ; /*-1;*/

    pb = dp->packet_bytes;

    /*
     * Check that the message can be sent with given header length.
     */

    if ((pb == 2 && len > 65535) || (pb == 1 && len > 255)) {
	driver_failure_posix(port_num, EINVAL);
	return ; /* -1; */
    }

    /*
     * Allocate memory for both the message and the header.
     */

    if ((dp->outbuf = sys_alloc(pb+len)) == NULL) {
	driver_failure_posix(port_num, ENOMEM);
	return ; /* -1; */
    }

    /*
     * Store header bytes (if any).
     */

    current = dp->outbuf;
    switch (pb) {
    case 4:
	*current++ = (len >> 24) & 255;
	*current++ = (len >> 16) & 255;
    case 2:
	*current++ = (len >> 8) & 255;
    case 1:
	*current++ = len & 255;
    }

    /*
     * Start the write.
     */

    if ((pb+len) == 0)
	return ; /* 0; */
    memcpy(current, buf, len);
    
    if (!async_write_file(&dp->out, dp->outbuf, pb+len)) {
	set_busy_port(port_num, 1);
    } else {
	dp->out.ov.Offset += pb+len; /* For vanilla driver. */
	/* XXX OffsetHigh should be changed too. */
	sys_free(dp->outbuf);
	dp->outbuf = NULL;
    }
    /*return 0;*/
}


/* ----------------------------------------------------------------------
 * ready_input --
 *	This function is called (indirectly) from check_io() when an
 *	event object has been signaled, indicating that there is
 *	something to read on the corresponding file handle.
 *
 *	If the port is working in the continous stream mode (packet_bytes == 0),
 *	whatever data read will be sent straight to Erlang.
 *
 * Results:
 *	Always 0.
 * ----------------------------------------------------------------------
 */

static void
ready_input(ErlDrvData drv_data, ErlDrvEvent ready_event)
/*     long drv_data;		/* Driver data. */
/*     HANDLE ready_event;	/* The handle for the ready event. */
{
    int error = 0;		/* The error code (assume initially no errors). */
    DWORD bytesRead;		/* Number of bytes read. */
    DriverData* dp;
    int pb;

    dp = driver_data+(int)drv_data;
    pb = dp->packet_bytes;

    DEBUGF(("ready_input: dp %p, event 0x%x\n", dp, ready_event));

    /*
     * Evaluate the result of the overlapped read.
     */

    error = get_overlapped_result(&dp->in, &bytesRead, TRUE);
    if (error == NO_ERROR) {
	if (pb == 0) { /* Continous stream. */
#ifdef DEBUG
	    DEBUGF(("ready_input: %d: ", bytesRead));
	    erl_bin_write(dp->inbuf, 16, bytesRead);
	    DEBUGF(("\n"));
#endif
	    driver_output(dp->port_num, dp->inbuf, bytesRead);
	} else {			/* Packet mode */
	    dp->bytesInBuffer += bytesRead;

	    /*
	     * Loop until we've exhausted the data in the buffer.
	     */

	    for (;;) {

		/*
		 * Check for completion of a header read.
		 */

		if (dp->bytesInBuffer >= dp->totalNeeded &&
		    dp->totalNeeded == pb) {

		    /*
		     * We have successfully read the packet header
		     * (and perhaps even the packet).  Get the packet size
		     * from the header and update dp->totalNeeded to include
		     * the packet size.
		     */

		    int packet_size = 0;
		    unsigned char *header = (unsigned char *) dp->inbuf;
		    
		    switch (pb) {
		    case 4:
			packet_size = (packet_size << 8) | *header++;
			packet_size = (packet_size << 8) | *header++;
		    case 2:
			packet_size = (packet_size << 8) | *header++;
		    case 1:
			packet_size = (packet_size << 8) | *header++;
		    }
		    
		    dp->totalNeeded += packet_size;
		    
		    /*
		     * Make sure that the receive buffer is big enough.
		     */
		    
		    if (dp->inBufSize < dp->totalNeeded) {
			char* new_buf;
		    
			dp->inBufSize = dp->totalNeeded;
			new_buf = sys_realloc(dp->inbuf, dp->totalNeeded);
			if (new_buf == NULL) {
			    error = ERROR_NOT_ENOUGH_MEMORY;
			    break; /* Break out of loop into error handler. */
			}
			dp->inbuf = new_buf;
		    }
		}
		
		/*
		 * Check for completion of a packet read.
		 */
		
		if (dp->bytesInBuffer < dp->totalNeeded) {
		    /*
		     * Not enough bytes in the buffer.  Break out of
		     * the loop and initiate a new read.
		     */

		    break;
		} else {
		    
		    /*
		     * We have successfully read a complete packet, which
		     * can be passed to Erlang.
		     */
		    
		    driver_output(dp->port_num, dp->inbuf+pb, dp->totalNeeded-pb);
		    
		    /*
		     * Update the number of bytes remaining in the buffer,
		     * and move the data remaining (if any) to the beginning
		     * of the buffer.
		     */
		    
		    dp->bytesInBuffer -= dp->totalNeeded;
		    if (dp->bytesInBuffer > 0) {
			memmove(dp->inbuf, dp->inbuf+dp->totalNeeded,
				dp->bytesInBuffer);
		    }
		    
		    /*
		     * Indicate that we need the size of a header, and
		     * go through the loop once more (to either process
		     * remaining bytes or initiate reading more).
		     */
		    
		    dp->totalNeeded = pb;
		}
	    }
	}
    }

    /*
     * Start a new overlapped read, or report the error.
     */

    if (error == NO_ERROR) {
	async_read_file(&dp->in, dp->inbuf+dp->bytesInBuffer,
			dp->inBufSize - dp->bytesInBuffer);
    } else {
	DEBUGF(("ready_input(): error: %s\n", win32_errorstr(error)));
	if (error == ERROR_BROKEN_PIPE || error == ERROR_HANDLE_EOF) {
	    /* Maybe check exit status */
	    if (dp->report_exit) {
		DWORD exitcode;
		if (GetExitCodeProcess(dp->port_pid, &exitcode) &&
		    exitcode != STILL_ACTIVE) {
		    driver_report_exit(dp->port_num, exitcode);
		}
	    }
	    driver_failure_eof(dp->port_num);
	} else {			/* Report real errors. */
	    int error = GetLastError();
	    (void) driver_select(dp->port_num, ready_event, DO_READ, 0);
	    _dosmaperr(error);
	    driver_failure_posix(dp->port_num, errno);
	}
    }

    /*return 0;*/
}

static void
ready_output(ErlDrvData drv_data, ErlDrvEvent ready_event)
{
    DWORD bytesWritten;
    DriverData* dp = driver_data + (int)drv_data;
    int error;

    DEBUGF(("ready_output(%d, 0x%x)\n", drv_data, ready_event));
    set_busy_port(dp->port_num, 0);
    ASSERT(dp->outbuf != NULL);
    sys_free(dp->outbuf);
    dp->outbuf = NULL;
    error = get_overlapped_result(&dp->out, &bytesWritten, TRUE);
    if (error == NO_ERROR) {
	dp->out.ov.Offset += bytesWritten; /* For vanilla driver. */
	return ; /* 0; */
    }
    (void) driver_select(dp->port_num, ready_event, DO_WRITE, 0);
    _dosmaperr(error);
    driver_failure_posix(dp->port_num, errno);
    /* return 0; */
}
/* Fills in the systems representation of the beam process identifier.
** The Pid is put in STRING representation in the supplied buffer,
** no interpretation of this should be done by the rest of the
** emulator. The buffer should be at least 21 bytes long.
*/
void sys_get_pid(char *buffer){
    DWORD p = GetCurrentProcessId();
    /* The pid is scalar and is an unsigned long. */
    sprintf(buffer,"%lu",(unsigned long) p);
}

int sys_putenv(char *buffer){
    char *env = safe_alloc(strlen(buffer)+1);
    strcpy(env,buffer);
    return(_putenv(env));
}

void sys_init_io(buf, size)
byte *buf;
uint32 size;
{
    tmp_buf = buf;
    tmp_buf_size = size;
    cerr_pos = 0;

#ifdef USE_THREADS
    {
	/* This is speical stuff, starting a driver from the 
	 * system routines, but is a nice way of handling stuff
	 * the erlang way
	 */
	SysDriverOpts dopts;
	int ret;

	sys_memset((void*)&dopts, 0, sizeof(SysDriverOpts));
	add_driver_entry(&async_driver_entry);
	/* FIXME: 7 == NIL */
	ret = open_driver(&async_driver_entry, 7, "async", &dopts);
	DEBUGF(("open_driver = %d\n", ret));
    }
#endif
}

/*void sys_init_io(buf, size)
byte *buf;
uint32 size;
{
    tmp_buf = buf;
    tmp_buf_size = (int)size;
}
*/

#ifdef INSTRUMENT
/* When instrumented sys_alloc and friends are implemented in utils.c */
#define SYS_ALLOC_ELSEWHERE
#endif

/* A = alloc |realloc | free */
#ifdef SYS_ALLOC_ELSEWHERE

/* sys_A implemented elsewhere (calls sys_A2) ... */ 
#ifndef sys_alloc2
/* ... and sys_A2 makros not used; use this implementation as sys_A2 */
#define SYS_ALLOC   sys_alloc2
#define SYS_REALLOC sys_realloc2
#define SYS_FREE    sys_free2
#else  /* ifndef sys_alloc2 */
/* ... and sys_A2 makros used; don't use this implementation as sys_A2 */
#endif /* ifndef sys_alloc2 */

#else /* #ifdef SYS_ALLOC_ELSEWHERE */

/* sys_A not implemented elsewhere ... */
#ifndef sys_alloc2
/* ... and sys_A2 makros not used; skip sys_A2 and use
   this implementation as sys_A */
#define SYS_ALLOC   sys_alloc
#define SYS_REALLOC sys_realloc
#define SYS_FREE    sys_free

#else /* ifndef sys_alloc2 */

/* ... and sys_A2 makros used; need sys_A (symbols) */
void* sys_alloc(Uint size)              { return sys_alloc2(size);        }
void* sys_realloc(void* ptr, Uint size) { return sys_realloc2(ptr, size); }
void* sys_free(void* ptr)               { return sys_free2(ptr);          }
#endif /* ifndef sys_alloc2 */

#endif /* #ifdef SYS_ALLOC_ELSEWHERE */

#ifdef SYS_ALLOC
 
#ifdef DEBUG
int tot_allocated = 0;
int alloc_calls = 0, realloc_calls = 0, free_calls = 0;
#endif

/* Allocate memory */
void* SYS_ALLOC(Uint size)
{
#ifdef DEBUG
    uint32* p = (uint32*) malloc(2*sizeof(uint32)+((size_t)size));
    alloc_calls++;
  
    if (p != NULL) {
	*p = size;
	tot_allocated += size;
	return (void*)(((char*)p) + 2*sizeof(uint32));
    }
    /* memset(p, 0, size); */
    return p;
#else
    return (void*) malloc((size_t)size);
#endif
}

/* Allocate memory */
void* SYS_REALLOC(void* ptr, Uint size)
{
#ifdef DEBUG
    if (ptr == NULL)
	return SYS_ALLOC(size);
    else {
	uint32* p = (uint32*) ((char*)ptr - 2*sizeof(uint32));
	realloc_calls++;
	tot_allocated -= *p;
	p = (uint32*) realloc((char*)p, 2*sizeof(uint32)+((size_t)size));
	if (p != NULL) {
	    *p = size;
	    tot_allocated += size;
	    return (void*)(((char*)p) + 2*sizeof(uint32));
	}
	return p;
    }
#else
    return (void*) realloc((char*)ptr, (size_t)size);
#endif
}

/* Deallocate memory */
void SYS_FREE(void* ptr)
{
#ifdef DEBUG
    uint32* p = (uint32*) ((char*)ptr-2*sizeof(uint32));
    free_calls++;
    tot_allocated -= *p;
    free((char*)p);
#else
    free((char*)ptr);
#endif
}

#endif /* #ifdef SYS_ALLOC */

/* XXX It isn't possible to do this safely without "*nsprintf"
   (i.e. something that puts a limit on the number of chars printed)
   - the below is probably the best we can do...    */
    
/*VARARGS*/

void sys_printf(CIO where, char* format, ...)
{
    va_list va;

    va_start(va, format);
    if (where == CBUF) {
	if (cerr_pos < TMP_BUF_MAX) {
	    vsprintf((char*)&tmp_buf[cerr_pos],format,va);
	    cerr_pos += sys_strlen((char*)&tmp_buf[cerr_pos]);
	    if (cerr_pos >= tmp_buf_size)
		erl_exit(1, "Internal buffer overflow in erl_printf\n");
	    if (cerr_pos >= TMP_BUF_MAX) {
		strcpy((char*)&tmp_buf[TMP_BUF_MAX - 3], "...");
		cerr_pos = TMP_BUF_MAX;
	    }
	}
    }
    else if (win_console && (where == COUT || where == CERR))
	ConVprintf(format, va);
    else if (where == CERR)
	erl_error(format, va);
    else if (where == COUT) {
	vprintf(format, va);
	fflush(stdout);
    } else {
	/* where indicates which fd to write to */
	vsprintf((char*)tmp_buf, format, va);
	write(where, tmp_buf, sys_strlen((char*)tmp_buf));
    }
    va_end(va);
}

void sys_putc(ch, where)
int ch; CIO where;
{
    if (where == CBUF) {
	if (cerr_pos < TMP_BUF_MAX) {
	    tmp_buf[cerr_pos++] = ch;
	    if (cerr_pos == TMP_BUF_MAX) {
		strcpy((char*)&tmp_buf[TMP_BUF_MAX - 3], "...");
		cerr_pos = TMP_BUF_MAX;
	    }
	}
	else if (cerr_pos >= tmp_buf_size)
	    erl_exit(1, "Internal buffer overflow in erl_printf\n");
    } else if (where == COUT && win_console) {
	ConPutChar(ch);
    } else if (where == COUT) {
	putchar(ch);
	fflush(stdout);
    } else
	sys_printf(where, "%c", ch);
}

static Preload* preloaded = NULL;
static unsigned* res_name = NULL;
static int num_preloaded = 0;

/* Return a pointer to a vector of names of preloaded modules */

Preload* sys_preloaded(void)
{
    HRSRC hRes;
    unsigned char* data;

#define GETWORD(p) (0[p] | 1[p] << 8)
#define GETDWORD(p) (GETWORD(p) | GETWORD(p+2) << 16)


    if (preloaded == NULL) {
	int i;

#ifdef ERL_RUN_SHARED_LIB
	if (beam_module == NULL)
#ifdef DEBUG
	    beam_module = GetModuleHandle("beam.debug.dll");
#else
            beam_module = GetModuleHandle("beam.dll");
#endif
#endif
	hRes = FindResource(beam_module, "0", "ERLANG_DICT");

	if (hRes == NULL) {
	    DWORD n = GetLastError();
	    fprintf(stderr, "No ERLANG_DICT resource\n");
	    exit(1);
	}
	data = (unsigned char *) LoadResource(beam_module, hRes);

	num_preloaded = GETWORD(data);
	if (num_preloaded == 0) {
	    fprintf(stderr, "No preloaded modules\n");
	    exit(1);
	}

	data += 2;
	preloaded = sys_alloc((num_preloaded+1)*sizeof(Preload));
	res_name = sys_alloc((num_preloaded+1)*sizeof(unsigned));
	for (i = 0; i < num_preloaded; i++) {
	    int n;

	    preloaded[i].size = GETDWORD(data);
	    data += 4;
	    res_name[i] = GETWORD(data);
	    data += 2;
	    n = GETWORD(data);
	    data += 2;
	    preloaded[i].name = sys_alloc(n+1);
	    sys_memcpy(preloaded[i].name, data, n);
	    preloaded[i].name[n] = '\0';
	    data += n;
	    DEBUGF(("name: %s; size: %d; resource: %p\n",
		    preloaded[i].name, preloaded[i].size, res_name[i]));
	}
	preloaded[i].name = NULL;
    }

#undef GETWORD
#undef GETDWORD
    return preloaded;
}

/* Return a pointer to preloaded code for module "module" */
unsigned char* sys_preload_begin(Preload* pp)
{
    HRSRC hRes;
    unsigned resource;
    
#ifdef ERL_RUN_SHARED_LIB
    /* if beam is a DLL, the resources are there, not in the exe */
    beam_module = GetModuleHandle("ERL_RUN.DLL");
    if (beam_module == NULL)
	beam_module = GetModuleHandle("beam.dll");
    if (beam_module == NULL)
	beam_module = GetModuleHandle("beam.debug.dll");
#endif

    resource = res_name[pp-preloaded];
    DEBUGF(("Loading name: %s; size: %d; resource: %p\n",
	    pp->name, pp->size, resource));
    hRes = FindResource(beam_module, (char *) resource, "ERLANG_CODE");
    return pp->code = LoadResource(beam_module, hRes);
}

/* Clean up if allocated */
void sys_preload_end(Preload* pp)
{
}

/* Read a key from console */

int
sys_get_key(int fd)
{
    ASSERT(fd == 0);

    if (win_console) {
        return ConGetKey();
    }

    /*
     * Black magic follows. (Code stolen from get_overlapped_result())
     */

    if (fd_driver_input != NULL && fd_driver_input->thread != (HANDLE)-1) {
	DWORD error;
	int key;

	error = WaitForSingleObject(fd_driver_input->ov.hEvent, INFINITE);
	if (error == WAIT_OBJECT_0) {
	    if (fd_driver_input->bytesTransferred > 0) {
		int n;
		int i;
		char* buf = OV_BUFFER_PTR(fd_driver_input);

		fd_driver_input->bytesTransferred--;
		n = fd_driver_input->bytesTransferred;
		key = buf[0];
		for (i = n; i > 0; i--) {
		    buf[i-1] = buf[i];
		}
		return key;
	    }
	}
    }
    return '*';		/* Error! */
}

static void*
checked_alloc(size)
     unsigned int size;		/* Amount to allocate. */
{
    void* p;

    if ((p = sys_alloc(size)) == NULL)
	erl_exit(1, "Can't allocate %d bytes of memory\n", size);
    return p;
}

/*
 * Returns a human-readable description of the last error.
 * The returned pointer will be valid only as long as last-error()
 * isn't called again.
 */

char* win32_errorstr(error)
     int error;
{
  static LPTSTR lpBufPtr = NULL;

  if (lpBufPtr)
    LocalFree(lpBufPtr);
  FormatMessage(
		FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM |
		FORMAT_MESSAGE_IGNORE_INSERTS,
		NULL,
		error,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		(LPTSTR) &lpBufPtr,
		0,
		NULL);
  SetLastError(error);
  return lpBufPtr;
}

char* last_error(void)
{
  return win32_errorstr(GetLastError());
}

static void* sys_func_memzero(void* s, size_t n)
{
    return sys_memzero(s, n);
}

#ifdef DEBUG
static HANDLE hDebugWrite = INVALID_HANDLE_VALUE;

void erl_debug(char *fmt,...)
{
  char sbuf[1024];		/* Temporary buffer. */
  DWORD written;		/* Actual number of chars written. */
  va_list va;

  if (hDebugWrite != INVALID_HANDLE_VALUE) {
    va_start(va, fmt);
    vsprintf(sbuf, fmt, va);
    WriteFile(hDebugWrite, sbuf, strlen(sbuf), &written, NULL);
    va_end(va);
  }
}

static void debug_console(void)
{
  HANDLE hRead;			/* Handle to read end of pipe. */
  SECURITY_ATTRIBUTES sa;
  PROCESS_INFORMATION procInfo;
  STARTUPINFO startInfo;
  BOOL ok;

  /*
   * Create a pipe for communicating with the sub process.
   */

  sa.nLength = sizeof(sa);
  sa.lpSecurityDescriptor = NULL;
  sa.bInheritHandle = TRUE;
  if (!CreatePipe(&hRead, &hDebugWrite, &sa, 0)) {
    fprintf(stderr, "Failed to create pipe: %d\n", 
	    GetLastError());
    exit(1);
  }

  startInfo.cb = sizeof(STARTUPINFO);
  startInfo.lpTitle = "Erlang Debug Log";
  startInfo.lpReserved = NULL; 
  startInfo.lpReserved2 = NULL; 
  startInfo.cbReserved2 = 0; 
  startInfo.lpDesktop = NULL;  
  startInfo.dwFlags = STARTF_USESTDHANDLES;
  startInfo.hStdInput = hRead;

  /* The following handles are not intended to be used. */
  startInfo.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
  startInfo.hStdError = GetStdHandle(STD_ERROR_HANDLE);

  ok = CreateProcess(NULL,
		     "erl_log.exe", /* Application */
		     NULL,	/* Process security attributes. */
		     NULL,	/* Thread security attributes. */
		     TRUE,	/* Handle inheritance flag. */
		     CREATE_NEW_CONSOLE, /* Flags. */
		     NULL,	/* Environment. */
		     NULL,	/* Current directory. */
		     &startInfo,/* Startup info. */
		     &procInfo	/* Process information. */
		     );

  CloseHandle(hRead); 

  if (ok) {
    /*
     * Since we don't use these, close them at once to avoid a resource
     * leak.
     */
    CloseHandle(procInfo.hProcess);
    CloseHandle(procInfo.hThread);
  } else {
    fprintf(stderr, "Create process failed: %s\n", last_error());
    exit(1);
  }
}

void
erl_bin_write(buf, sz, max)
     unsigned char* buf;
     int sz;
     int max;
{
  int i, imax;
  char comma[5] = ",";

  if (hDebugWrite == INVALID_HANDLE_VALUE)
    return;

  if (!sz)
    return;
  if (sz > max)
    imax = max;
  else
    imax = sz;
  
  for (i=0; i<imax; i++) {
    if (i == imax-1) {
      if (sz > max)
	strcpy(comma, ",...");
      else
	comma[0] = 0;
    }
    if (isdigit(buf[i]))
      erl_debug("%u%s", (int)(buf[i]), comma);
    else {
      if (isalpha(buf[i])) {
	erl_debug("%c%s", buf[i], comma);
      }
      else
	erl_debug("%u%s", (int)(buf[i]), comma);
    }
  }
}

void
erl_assert_error(char* expr, char* file, int line)
{   
    char message[1024];

    sprintf(message, "File %hs, line %d: %hs", file, line, expr);
    MessageBox(GetActiveWindow(), message, "Assertion failed",
	       MB_OK | MB_ICONERROR);
    erl_crash_dump(NULL, NULL);
    DebugBreak();
}

#endif // DEBUG

/*
 * the last two only used for standalone erlang
 * they should are used by sae_main in beam dll to
 * enable standalone execution via erl_api-routines
 */

void
erl_sys_init()
{
    HANDLE handle;
    /*
     * Firstly, initialise malloc to make it safe for us...
     */

    int_os_version.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
    GetVersionEx(&int_os_version);

    /*
     * Test if we have named pipes or not.
     */

    switch (int_os_version.dwPlatformId) {
    case VER_PLATFORM_WIN32_WINDOWS:
	DEBUGF(("Running on Windows 95"));
	use_named_pipes = FALSE;
	break;
    case VER_PLATFORM_WIN32_NT:
	DEBUGF(("Running on Windows NT"));
	use_named_pipes = TRUE;
	break;
    default:			/* Unsupported platform. */
	exit(1);
    }
    DEBUGF((" %d.%d, build %d, %s\n",
	    int_os_version.dwMajorVersion, int_os_version.dwMinorVersion,
	    int_os_version.dwBuildNumber, int_os_version.szCSDVersion));

#ifdef ERL_RUN_SHARED_LIB
	if (beam_module == NULL)
#ifdef DEBUG
	    beam_module = GetModuleHandle("beam.debug.dll");
#else
            beam_module = GetModuleHandle("beam.dll");
#endif
#endif
    init_console();

    /*
     * The following makes sure that the current directory for the current drive
     * is remembered (in the environment).
     */  

    chdir(".");

    /*
     * Make sure that the standard error handle is valid.
     */
    handle = GetStdHandle(STD_ERROR_HANDLE);
    if (handle == INVALID_HANDLE_VALUE || handle == 0) {
	SetStdHandle(STD_ERROR_HANDLE, GetStdHandle(STD_OUTPUT_HANDLE));
    }
    erts_sys_init_float();
    init_sys_select();
}

void 
erl_sys_schedule_loop(void)
{
    for (;;) {
	while (schedule()) {
	    win_check_io(0);	/* Poll for I/O. */
	    check_async_ready(); /* Check async completions. */
	}
	if (check_async_ready()) {
	    win_check_io(0);
	} else {
	    win_check_io(1);		/* Wait for I/O or a timeout. */
	}
    }
}

#ifdef USE_THREADS
/*
 * Async operation support.
 */

static ErlDrvEvent async_drv_event;

void
sys_async_ready(int fd)
{
    SetEvent((HANDLE)async_drv_event);
}

static int
async_drv_init(void)
{
    async_drv_event = (ErlDrvEvent) NULL;
    return 0;
}

static ErlDrvData
async_drv_start(ErlDrvPort port_num, char* name, SysDriverOpts* opts)
{
    if (async_drv_event != (ErlDrvEvent) NULL) {
	return ERL_DRV_ERROR_GENERAL;
    }
    if ((async_drv_event = (ErlDrvEvent)CreateAutoEvent(FALSE)) == (ErlDrvEvent) NULL) {
	return ERL_DRV_ERROR_GENERAL;
    }

    driver_select(port_num, async_drv_event, DO_READ, 1);
    if (init_async(async_drv_event) < 0) {
	return ERL_DRV_ERROR_GENERAL;
    }
    return (ErlDrvData)port_num;
}

static void
async_drv_stop(ErlDrvData port_num)
{
    exit_async();
    driver_select((ErlDrvPort)port_num, async_drv_event, DO_READ, 0);
    CloseHandle((HANDLE)async_drv_event);
    async_drv_event = (ErlDrvEvent) NULL;
}


static void
async_drv_input(ErlDrvData port_num, ErlDrvEvent e) 
{
    check_async_ready();

    /*
     * Our event is auto-resetting.
     */
}

#endif
