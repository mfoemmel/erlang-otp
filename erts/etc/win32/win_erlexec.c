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
 * Extra support for running the emulator on Windows.
 * Most of this only used when beam is run as a separate process.
 */

#include <windows.h>
#include <winuser.h>
#include <wincon.h>
#include <process.h>
#include "sys.h"
#include "driver.h"

#include "erl_api.h"

extern int nohup;
extern int keep_window;
void error(char* format, ...);

/*
 * Local functions.
 */

static int start(char* emu, char** argv);
static void start_winsock(void);
static char* last_error(void);
static char* last_wsa_error(void);
static char* win32_errorstr(int error);
static int has_console(void);


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
	int argc = 0;

	putenv("ERL_CONSOLE_MODE=window");
	while (argv[argc] != NULL) {
	    ++argc;
	}
	ErlOtpStart(argc, argv);
	result = 0;
    }
    if (result == -1) {
	error("Failed to execute %s: %s", emu, win32_errorstr(_doserrno));
    }
    return 0;
}

void __cdecl 
do_keep_window(void)
{
    printf("\nPress any key to close window.\n");
    ErlGetConsoleKey();
}

int
start_emulator(char* emu, char** argv, int start_detached)
{
    int result;
    static char console_mode[] = "ERL_CONSOLE_MODE=tty:ccc";
    char* fd_type;
    char* title;

    fd_type = strchr(console_mode, ':');
    fd_type++;
    _flushall();
    
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
     * Start the emulator.
     */

    if ((title = getenv("ERL_WINDOW_TITLE")) != NULL) {
	SetConsoleTitle(title);
    }

    putenv(console_mode);
    {
	int argc = 0;
	while (argv[argc] != NULL) {
	    ++argc;
	}
	if (keep_window) {
	    atexit(do_keep_window);
	}
	ErlOtpStart(argc, argv);
	return 0;
    }

    if (getenv("ERLSRV_SERVICE_NAME")){
	/* Running as a service, prepare to handle the WM_USER thread
	   message which means "SIGHUP" == die semi-gracefully... */
    }
    return result;
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
