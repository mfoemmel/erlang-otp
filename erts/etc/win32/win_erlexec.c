#include <windows.h>
#include <winuser.h>
#include <wincon.h>
#include <process.h>
#include "sys.h"
#include "driver.h"

extern char* strsave(char* string);

extern int nohup;
extern int keep_window;
void error(char* format, ...);

/*
 * Local functions.
 */

static int start(char* emu, char** argv);
static void start_winsock(void);
static DWORD WINAPI threaded_reader(LPVOID param);
static DWORD WINAPI threaded_writer(LPVOID param);
static char* last_error(void);
static char* last_wsa_error(void);
static char* win32_errorstr(int error);
static BOOL WINAPI signal_handler(DWORD dwCtrlType);
static int has_console(void);

static HANDLE hChildStdInWr = INVALID_HANDLE_VALUE;


/*
 * The following struct is used to pass parameters to a threaded reader.
 */
typedef struct reader_params {
    HANDLE from;		/* Handle to read from. */
    int to;			/* File descriptor to write to. */
} ReaderParams;

int
start_win_emulator(char* emu, char** argv, int start_detached)
{
    int result;

    if (start_detached) {
	close(0);
	close(1);
	close(2);
	putenv("ERL_CONSOLE_MODE=detached");
	result = spawnv(_P_DETACH, emu, argv);
    } else {
	putenv("ERL_CONSOLE_MODE=window");
	result = spawnv(_P_OVERLAY, emu, argv);
    }
    if (result == -1) {
	error("Failed to execute %s: %s", emu, win32_errorstr(_doserrno));
    }
    SetPriorityClass((HANDLE) result, GetPriorityClass(GetCurrentProcess()));
    return 0;
}

int
start_emulator(char* emu, char** argv, int start_detached)
{
    int result;
    SECURITY_ATTRIBUTES saAttr;
    HANDLE hStdIn;
    HANDLE hStdOut;
    HANDLE hStdErr;
    HANDLE temp;
    HANDLE hChildStdOutRd = INVALID_HANDLE_VALUE;
    HANDLE hChildStdOutWr = INVALID_HANDLE_VALUE;
    HANDLE hChildStdErrWr = INVALID_HANDLE_VALUE;
    HANDLE hChildStdInRd;
    HANDLE hChildStdErrRd;
    int hEmulator;
    HANDLE hThreadedReader1 = INVALID_HANDLE_VALUE;
    HANDLE hThreadedReader2 = INVALID_HANDLE_VALUE;
    HANDLE hThreadedWriter = INVALID_HANDLE_VALUE;
    ReaderParams params1;
    ReaderParams params2;
    DWORD rtid;
    DWORD mode;
    static char console_mode[] = "ERL_CONSOLE_MODE=tty:ccc";
    char* fd_type;
    char* title;

    fd_type = strchr(console_mode, ':');
    fd_type++;
    _flushall();
    SetConsoleCtrlHandler(signal_handler, TRUE);
    
    /*
     * If no console, we will spawn the emulator detached.
     */

    if (start_detached) {
	close(0);
	close(1);
	close(2);
	putenv("ERL_CONSOLE_MODE=detached");
	result = spawnv(_P_DETACH, emu, argv);
	if (result == -1) {
	    return 1;
	}
	SetPriorityClass((HANDLE) result, GetPriorityClass(GetCurrentProcess()));
	return 0;
    }

    /*
     * Prepare to setup pipes to the emulator.
     */

    hStdIn = GetStdHandle(STD_INPUT_HANDLE);
    hStdOut = GetStdHandle(STD_OUTPUT_HANDLE);
    hStdErr = GetStdHandle(STD_ERROR_HANDLE);
    saAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
    saAttr.bInheritHandle = TRUE;
    saAttr.lpSecurityDescriptor = NULL;

    
    /* 
     * Create a pipe for the child's stdin.
     */

    if (!GetConsoleMode(hStdIn, &mode)) {
	*fd_type++ = 'o';
    } else {
	*fd_type++ = 'c';
	if (!CreatePipe(&hChildStdInRd, &temp, &saAttr, 0))
	    error("failed to create stdin pipe");
	if (!SetStdHandle(STD_INPUT_HANDLE, hChildStdInRd)) 
	    error("failed to redirect stdin");
	result = DuplicateHandle(GetCurrentProcess(), temp,
				 GetCurrentProcess(), &hChildStdInWr,
				 0, FALSE, DUPLICATE_SAME_ACCESS);
	CloseHandle(temp);
	if (!result) {
	    error("failed to duplicate stdin");
	}
    }

    /* 
     * Create a pipe for the child's stdout.
     */

    if (!GetConsoleMode(hStdOut, &mode)) {
	*fd_type++ = 'o';
    } else {
	*fd_type++ = 'c';
	if (!CreatePipe(&temp, &hChildStdOutWr, &saAttr, 0))
	    error("failed to create stdout pipe");
	if (!SetStdHandle(STD_OUTPUT_HANDLE, hChildStdOutWr))
	    error("failed to redirect stdout");
	result = DuplicateHandle(GetCurrentProcess(), temp,
				 GetCurrentProcess(), &hChildStdOutRd,
				 0, FALSE, DUPLICATE_SAME_ACCESS);
	CloseHandle(temp);
	if (!result) {
	    error("failed to duplicate stdin");
	}
    }

    /* 
     * Create a pipe for the child's stderr.
     */

    if (!GetConsoleMode(hStdErr, &mode)) {
	*fd_type++ = 'o';
    } else {
	*fd_type++ = 'c';
	if (!CreatePipe(&temp, &hChildStdErrWr, &saAttr, 0))
	    error("failed to create stdout pipe");
	if (!SetStdHandle(STD_ERROR_HANDLE, hChildStdErrWr))
	    error("failed to redirect stderr");
	result = DuplicateHandle(GetCurrentProcess(), temp,
				 GetCurrentProcess(), &hChildStdErrRd,
				 0, FALSE, DUPLICATE_SAME_ACCESS);
	CloseHandle(temp);
	if (!result) {
	    error("failed to duplicate stderr");
	}
    }


    /*
     * Start the emulator.
     */

    if ((title = getenv("ERL_WINDOW_TITLE")) != NULL) {
	SetConsoleTitle(title);
    }
    putenv(console_mode);
    hEmulator = spawnv(_P_NOWAIT, emu, argv);
    if (hEmulator == -1) {
	error("failed to spawn %s", emu);
    }
    SetPriorityClass((HANDLE) hEmulator, GetPriorityClass(GetCurrentProcess()));

    SetStdHandle(STD_INPUT_HANDLE, hStdIn);
    SetStdHandle(STD_OUTPUT_HANDLE, hStdOut);
    SetStdHandle(STD_OUTPUT_HANDLE, hStdErr);

    if (hChildStdInWr != INVALID_HANDLE_VALUE) {
	hThreadedWriter = (HANDLE) _beginthreadex(NULL, 0, threaded_writer,
					 (LPVOID) hChildStdInWr, 0, &rtid);
	if (hThreadedWriter == (HANDLE) -1) {
	    error("failed to create write thread");
	}
	CloseHandle(hChildStdOutWr);
    }

    if (hChildStdOutRd != INVALID_HANDLE_VALUE) {
	params1.from = hChildStdOutRd;
	params1.to = 1;
	hThreadedReader1 = (HANDLE) _beginthreadex(NULL, 0, threaded_reader,
						  (LPVOID) &params1,
						  0, &rtid);
	if (hThreadedReader1 == (HANDLE) -1) {
	    error("failed to create read thread");
	}
	CloseHandle(hChildStdOutWr);
    }

    if (hChildStdErrRd != INVALID_HANDLE_VALUE) {
	params2.from = hChildStdErrRd;
	params2.to = 2;
	hThreadedReader2 = (HANDLE) _beginthreadex(NULL, 0, threaded_reader,
						  (LPVOID) &params2,
						  0, &rtid);
	if (hThreadedReader2 == (HANDLE) -1) {
	    error("failed to create read thread");
	}
	CloseHandle(hChildStdErrWr);
    }
    if(getenv("ERLSRV_SERVICE_NAME")){
	/* Running as a service, prepare to handle the WM_USER thread
	   message which means "SIGHUP" == die semi-gracefully... */
	HANDLE emu[1] = {(HANDLE) hEmulator}; /* Yea, I know this is
						 not necessary, but
						 it avoids further casting */
	DWORD res;
	for(;;){
	    res = MsgWaitForMultipleObjects(1,
					    emu, 
					    FALSE, 
					    INFINITE, 
					    QS_POSTMESSAGE);
	    if(res == WAIT_OBJECT_0+1){
		/* We got a WM_USER thread message (probably...) */
		TerminateProcess(emu[0],0);
	    }
	    if(res == WAIT_OBJECT_0 || res == WAIT_OBJECT_0+1){
		GetExitCodeProcess(emu[0],&result);
		break;
	    }
	}
    } else { 
	if (_cwait(&result, hEmulator, 0) == -1) {
	    return 1;
	}
    }
    if (hThreadedReader1 != INVALID_HANDLE_VALUE) {
	WaitForSingleObject(hThreadedReader1, 1000);
    }
    if(keep_window){
#if 0
	DWORD dummy;
	char buff[1];
	if(hThreadedWriter != INVALID_HANDLE_VALUE)
	    TerminateThread(hThreadedWriter,0);
	WriteFile(GetStdHandle(STD_OUTPUT_HANDLE),
		  "Press return to close window: ",
		  30,
		  &dummy,
		  NULL);
	ReadFile(GetStdHandle(STD_INPUT_HANDLE),buff,1,&dummy,NULL);
#else
	_spawnlp(_P_NOWAIT,"cmd.exe","cmd.exe","/c","pause",NULL);
#endif
    }
    return result;
}

static DWORD WINAPI
threaded_reader(LPVOID p)
{
    ReaderParams* params = (ReaderParams *) p;
    HANDLE from = params->from;
    int to = params->to;
    char sbuf[1024];
    DWORD bytes_read;

    for (;;) {
	if (!ReadFile(from, sbuf, sizeof(sbuf), &bytes_read, NULL)) {
	    return 0;
	}
	if (bytes_read == 0) {
	    return 0;
	}
	write(to, sbuf, bytes_read);
    }
}

static DWORD WINAPI
threaded_writer(LPVOID param)
{
    HANDLE to = (HANDLE) param;
    char sbuf[1024];
    DWORD bytes_read;
    DWORD bytes_written;

    for (;;) {
	bytes_read = read(0, sbuf, sizeof(sbuf));
	if (bytes_read <= 0) {
	    continue;
	}
	if (!WriteFile(to, sbuf, bytes_read, &bytes_written, NULL)) {
	    return 0;
	}
    }
}

void
error(char* format, ...)
{
    char sbuf[2048];
    va_list ap;

    va_start(ap, format);
    vsprintf(sbuf, format, ap);
    va_end(ap);

    if (has_console()) {
	fprintf(stderr, "%s\n", sbuf);
    } else {
	MessageBox(NULL, sbuf, "Werl", MB_OK|MB_ICONERROR);
    }
    exit(1);
}

static char*
last_error(void)
{
    return win32_errorstr(GetLastError());
}

/*
 * Returns a human-readable description of the last error.
 * The returned pointer will be valid only as long as last-error()
 * isn't called again.
 */

static char*
win32_errorstr(int error)
{
    static LPTSTR lpBufPtr = NULL;

    if (lpBufPtr)
	LocalFree(lpBufPtr);
    FormatMessage(
		  FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
		  NULL,
		  error,
		  MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		  (LPTSTR) &lpBufPtr,
		  0,
		  NULL);
    SetLastError(error);
    return lpBufPtr;
}

/*
 * Ignore Ctrl-C and break events; exit on any other.
 */

static BOOL WINAPI
signal_handler(DWORD dwCtrlType)
{
    char c = 3;
    DWORD written;

    switch (dwCtrlType) {
    case CTRL_C_EVENT:
    case CTRL_BREAK_EVENT:
	WriteFile(hChildStdInWr, &c, 1, &written, NULL);
	break;
    case CTRL_LOGOFF_EVENT:
	if (nohup)
	    break;
	/* Fall through */
    default:
	exit(0);
    }
    return TRUE;
}

static int
has_console(void)
{
    HANDLE handle = CreateFile("CONOUT$", GENERIC_WRITE, FILE_SHARE_WRITE,
			       NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

    if (handle != INVALID_HANDLE_VALUE) {
        CloseHandle(handle);
	return 1;
    } else {
        return 0;
    }
}
