/* 
 * tclWinPipe.c --
 *
 *	This file implements the Windows-specific pipeline exec functions.
 *
 * Copyright (c) 1996 by Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tclWinPipe.c 1.32 96/10/11 17:39:22
 */

#include "tclWinInt.h"

#include <dos.h>
#include <fcntl.h>
#include <io.h>
#include <windows.h>
#include <sys/stat.h>

/*
 * The following defines identify the various types of applications that 
 * run under windows.  There is special case code for the various types.
 */

#define APPL_NONE	0
#define APPL_DOS	1
#define APPL_WIN3X	2
#define APPL_WIN32	3

/*
 * This value indicates whether this module has been initialized.
 */

static int initialized = 0;

/*
 * This list is used to map from pids to process handles.
 */

typedef struct ProcInfo {
    HANDLE hProcess;
    DWORD dwProcessId;
    struct ProcInfo *nextPtr;
} ProcInfo;

static ProcInfo *procList = NULL;

/*
 * Declarations for functions used only in this file.
 */

static int		ApplicationType(Tcl_Interp *interp, 
			    const char *fileName, char *fullName);
static void		BuildCommandLine(int argc, char **argv, 
			    Tcl_DString *linePtr);
static char *		MakeTempFile(Tcl_DString *namePtr);
static void		CopyFileByHandles(HANDLE dst, HANDLE src);
static BOOL		HasConsole(void);

/*
 *----------------------------------------------------------------------
 *
 * Tcl_WaitPid --
 *
 *	Emulates the waitpid system call.
 *
 * Results:
 *	Returns 0 if the process is still alive, -1 on an error, or
 *	the pid on a clean close.  
 *
 * Side effects:
 *	Unless WNOHANG is set and the wait times out, the process
 *	information record will be deleted and the process handle
 *	will be closed.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_WaitPid(pid, statPtr, options)
    pid_t pid;
    int *statPtr;
    int options;
{
    ProcInfo *infoPtr, **prevPtrPtr;
    int flags, result;
    DWORD ret;

    if (options & WNOHANG) {
	flags = 0;
    } else {
	flags = INFINITE;
    }
    if (pid == 0) {
	*statPtr = 0;
	return 0;
    }

    /*
     * Find the process on the process list.
     */

    prevPtrPtr = &procList;
    for (infoPtr = procList; infoPtr != NULL;
	 prevPtrPtr = &infoPtr->nextPtr, infoPtr = infoPtr->nextPtr) {
	if (infoPtr->dwProcessId == (DWORD)pid) {
	    break;
	}
    }
    if (infoPtr == NULL) {
	return 0;
    }

    ret = WaitForSingleObject(infoPtr->hProcess, flags);
    if (ret == WAIT_TIMEOUT) {
	*statPtr = 0;
	if (options & WNOHANG) {
	    return 0;
	} else {
	    result = 0;
	}
    } else if (ret != WAIT_FAILED) {
	GetExitCodeProcess(infoPtr->hProcess, (DWORD*)statPtr);
	*statPtr = ((*statPtr << 8) & 0xff00);
	result = pid;
    } else {
	errno = ECHILD;
	result = -1;
    }

    /*
     * Remove the process from the process list and close the process handle.
     */
    CloseHandle(infoPtr->hProcess);
    *prevPtrPtr = infoPtr->nextPtr;
    ckfree((char*)infoPtr);

    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * TclpCreateProcess --
 *
 *	Create a child process that has the specified files as its 
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
 *	The return value is TCL_ERROR and an error message is left in
 *	interp->result if there was a problem creating the child 
 *	process.  Otherwise, the return value is TCL_OK and *pidPtr is
 *	filled with the process id of the child process.
 * 
 * Side effects:
 *	A process is created.
 *	
 *----------------------------------------------------------------------
 */

int
TclpCreateProcess(interp, argc, argv, inputFile, outputFile, errorFile, 
	inputFileName, outputFileName, errorFileName, pidPtr)
    Tcl_Interp *interp;		/* Interpreter in which to leave errors that
				 * occurred when creating the child process.
				 * Error messages from the child process
				 * itself are sent to errorFile. */
    int argc;			/* Number of arguments in following array. */
    char **argv;		/* Array of argument strings.  argv[0]
				 * contains the name of the executable
				 * converted to native format (using the
				 * Tcl_TranslateFileName call).  Additional
				 * arguments have not been converted. */
    Tcl_File inputFile;		/* If non-NULL, gives the file to use as
				 * input for the child process.  If inputFile
				 * file is not readable or is NULL, the child
				 * will receive no standard input. */
    Tcl_File outputFile;	/* If non-NULL, gives the file that
				 * receives output from the child process.  If
				 * outputFile file is not writeable or is
				 * NULL, output from the child will be
				 * discarded. */
    Tcl_File errorFile;		/* If non-NULL, gives the file that
				 * receives errors from the child process.  If
				 * errorFile file is not writeable or is NULL,
				 * errors from the child will be discarded.
				 * errorFile may be the same as outputFile. */
    char *inputFileName;	/* If non-NULL, gives the name of the disk
				 * file that corresponds to inputFile.  If
				 * NULL, then the name was not available
				 * because inputFile corresponds to a channel,
				 * pipe, socket, etc. */
    char *outputFileName;	/* If non-NULL, gives the name of the disk
				 * file that corresponds to outputFile.  If
				 * NULL, then the name was not available
				 * because outputFile corresponds to a
				 * channel, pipe, socket, etc. */
    char *errorFileName;	/* If non-NULL, gives the name of the disk
				 * file that corresponds to errorFile.  If
				 * NULL, then the name was not available
				 * because errorFile corresponds to a channel,
				 * pipe, socket, etc. */
    int *pidPtr;		/* If this procedure is successful, pidPtr
				 * is filled with the process id of the child
				 * process. */
{
    int result, type, applType, createFlags;
    Tcl_DString cmdLine;
    STARTUPINFO startInfo;
    PROCESS_INFORMATION procInfo;
    SECURITY_ATTRIBUTES secAtts;
    HANDLE hProcess, h, inputHandle, outputHandle, errorHandle;
    char execPath[MAX_PATH];
    char *originalName;
    OSVERSIONINFO os;

    os.dwOSVersionInfoSize = sizeof(os);
    GetVersionEx(&os);

    applType = ApplicationType(interp, argv[0], execPath);
    if (applType == APPL_NONE) {
	return TCL_ERROR;
    }
    originalName = argv[0];
    argv[0] = execPath;

    result = TCL_ERROR;
    Tcl_DStringInit(&cmdLine);

    if (os.dwPlatformId == VER_PLATFORM_WIN32s) {
	/*
	 * Under Win32s, there are no pipes.  In order to simulate pipe
	 * behavior, the child processes are run synchronously and their
	 * I/O is redirected from/to temporary files before the next 
	 * stage of the pipeline is started.
	 */

	Tcl_DString inputTempFile, outputTempFile;
	ClientData clientData;
	TclWinPipe *pipePtr;
	DWORD args[4];
	void *trans[5];
	DWORD status;
	MSG msg;

	BuildCommandLine(argc, argv, &cmdLine);

	ZeroMemory(&startInfo, sizeof(startInfo));
	startInfo.cb = sizeof(startInfo);

	Tcl_DStringInit(&inputTempFile);
	Tcl_DStringInit(&outputTempFile);
	outputHandle = INVALID_HANDLE_VALUE;

	if (inputFileName == NULL) {
	    if (inputFile != NULL) {
		clientData = Tcl_GetFileInfo(inputFile, &type);
		if (type == TCL_WIN_FILE) {
		    h = INVALID_HANDLE_VALUE;
		    inputFileName = MakeTempFile(&inputTempFile);
		    if (inputFileName != NULL) {
			h = CreateFile(inputFileName, GENERIC_WRITE, 0, 
				NULL, CREATE_ALWAYS, 0, NULL);
		    }
		    if (h == INVALID_HANDLE_VALUE) {
			Tcl_AppendResult(interp, "couldn't duplicate input handle: ", 
				Tcl_PosixError(interp), (char *) NULL);
			goto end32s;
		    }
		    CopyFileByHandles(h, (HANDLE) clientData);
		    CloseHandle(h);
		} else if (type == TCL_WIN32S_PIPE) {
		    pipePtr = (TclWinPipe *) clientData;
		    inputFileName = pipePtr->fileName;
		}
	    }
	    if (inputFileName == NULL) {
		inputFileName = "nul";
	    }
	}
	if (outputFileName == NULL) {
	    if (outputFile != NULL) {
		clientData = Tcl_GetFileInfo(outputFile, &type);
		if ((type >= TCL_WIN_PIPE) && (type <= TCL_WIN_CONSOLE)) {
		    outputFileName = MakeTempFile(&outputTempFile);
		    if (outputFileName == NULL) {
			Tcl_AppendResult(interp, "couldn't duplicate output handle: ",
				Tcl_PosixError(interp), (char *) NULL);
			goto end32s;
		    }
		    outputHandle = (HANDLE) clientData;
		} else if (type == TCL_WIN32S_PIPE) {
		    pipePtr = (TclWinPipe *) clientData;
		    outputFileName = pipePtr->fileName;
		}
	    }
	    if (outputFileName == NULL) {
		outputFileName = "nul";
	    }
	}

	if (applType == APPL_DOS) {
	    args[0] = (DWORD) Tcl_DStringValue(&cmdLine);
	    args[1] = (DWORD) inputFileName;
	    args[2] = (DWORD) outputFileName;
	    trans[0] = &args[0];
	    trans[1] = &args[1];
	    trans[2] = &args[2];
	    trans[3] = NULL;
	    if (TclSynchSpawn(args, 0, trans, pidPtr) != 0) {
		result = TCL_OK;
	    }
	} else if (applType == APPL_WIN3X) {
	    args[0] = (DWORD) Tcl_DStringValue(&cmdLine);
	    trans[0] = &args[0];
	    trans[1] = NULL;
	    if (TclSynchSpawn(args, 1, trans, pidPtr) != 0) {
		result = TCL_OK;
	    }
	} else {
	    if (CreateProcess(NULL, Tcl_DStringValue(&cmdLine), NULL, NULL, 
		    FALSE, DETACHED_PROCESS, NULL, NULL, &startInfo, 
		    &procInfo) != 0) {
		CloseHandle(procInfo.hThread);
		while (1) {
		    if (GetExitCodeProcess(procInfo.hProcess, &status) == FALSE) {
			break;
		    }
		    if (status != STILL_ACTIVE) {
			break;
		    }
		    if (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE) == TRUE) {
			TranslateMessage(&msg);
			DispatchMessage(&msg);
		    }
		}
		*pidPtr = (int) procInfo.dwProcessId;
		if (*pidPtr != 0) {
		    ProcInfo *procPtr = (ProcInfo *)ckalloc(sizeof(ProcInfo));
		    procPtr->hProcess = procInfo.hProcess;
		    procPtr->dwProcessId = procInfo.dwProcessId;
		    procPtr->nextPtr = procList;
		    procList = procPtr;
		}
		result = TCL_OK;
	    }
	}
	if (result != TCL_OK) {
	    TclWinConvertError(GetLastError());
	    Tcl_AppendResult(interp, "couldn't execute \"", originalName,
		    "\": ", Tcl_PosixError(interp), (char *) NULL);
	}

	end32s:
	if (outputHandle != INVALID_HANDLE_VALUE) {
	    /*
	     * Now copy stuff from temp file to actual output handle. Don't
	     * close outputHandle because it is associated with the output
	     * file owned by the caller.
	     */

	    h = CreateFile(outputFileName, GENERIC_READ, 0, NULL, OPEN_ALWAYS,
		    0, NULL);
	    if (h != INVALID_HANDLE_VALUE) {
		CopyFileByHandles(outputHandle, h);
	    }
	    CloseHandle(h);
	}

	if (inputFileName == Tcl_DStringValue(&inputTempFile)) {
	    DeleteFile(inputFileName);
	}
	if (outputFileName == Tcl_DStringValue(&outputTempFile)) {
	    DeleteFile(outputFileName);
	}

	Tcl_DStringFree(&inputTempFile);
	Tcl_DStringFree(&outputTempFile);
        Tcl_DStringFree(&cmdLine);
	return result;
    }
    hProcess = GetCurrentProcess();

    /*
     * STARTF_USESTDHANDLES must be used to pass handles to child process.
     * Using SetStdHandle() and/or dup2() only works when a console mode 
     * parent process is spawning an attached console mode child process.
     */

    ZeroMemory(&startInfo, sizeof(startInfo));
    startInfo.cb = sizeof(startInfo);
    startInfo.dwFlags   = STARTF_USESTDHANDLES;
    startInfo.hStdInput	= INVALID_HANDLE_VALUE;
    startInfo.hStdOutput= INVALID_HANDLE_VALUE;
    startInfo.hStdError = INVALID_HANDLE_VALUE;

    secAtts.nLength = sizeof(SECURITY_ATTRIBUTES);
    secAtts.lpSecurityDescriptor = NULL;
    secAtts.bInheritHandle = TRUE;

    /*
     * We have to check the type of each file, since we cannot duplicate 
     * some file types.  
     */

    inputHandle = INVALID_HANDLE_VALUE;
    if (inputFile != NULL) {
	h = (HANDLE) Tcl_GetFileInfo(inputFile, &type);
	if ((type >= TCL_WIN_PIPE) && (type <= TCL_WIN_CONSOLE)) {
	    inputHandle = h;
	}
    }
    outputHandle = INVALID_HANDLE_VALUE;
    if (outputFile != NULL) {
	h = (HANDLE) Tcl_GetFileInfo(outputFile, &type);
	if ((type >= TCL_WIN_PIPE) && (type <= TCL_WIN_CONSOLE)) {
	    outputHandle = h;
	}
    }
    errorHandle = INVALID_HANDLE_VALUE;
    if (errorFile != NULL) {
	h = (HANDLE) Tcl_GetFileInfo(errorFile, &type);
	if ((type >= TCL_WIN_PIPE) && (type <= TCL_WIN_CONSOLE)) {
	    errorHandle = h;
	}
    }

    /*
     * Duplicate all the handles which will be passed off as stdin, stdout
     * and stderr of the child process. The duplicate handles are set to
     * be inheritable, so the child process can use them.
     */

    if (inputHandle == INVALID_HANDLE_VALUE) {
	/* 
	 * If handle was not set, stdin should return immediate EOF.
	 * Under Windows95, some applications (both 16 and 32 bit!) 
	 * cannot read from the NUL device; they read from console
	 * instead.  When running tk, this is fatal because the child 
	 * process would hang forever waiting for EOF from the unmapped 
	 * console window used by the helper application.
	 *
	 * Fortunately, the helper application detects a closed pipe 
	 * as an immediate EOF and can pass that information to the 
	 * child process.
	 */

	if (CreatePipe(&startInfo.hStdInput, &h, &secAtts, 0) != FALSE) {
	    CloseHandle(h);
	}
    } else {
	DuplicateHandle(hProcess, inputHandle, hProcess, &startInfo.hStdInput,
		0, TRUE, DUPLICATE_SAME_ACCESS);
    }
    if (startInfo.hStdInput == INVALID_HANDLE_VALUE) {
	TclWinConvertError(GetLastError());
	Tcl_AppendResult(interp, "couldn't duplicate input handle: ",
		Tcl_PosixError(interp), (char *) NULL);
	goto end;
    }

    if (outputHandle == INVALID_HANDLE_VALUE) {
	/*
	 * If handle was not set, output should be sent to an infinitely 
	 * deep sink.  Under Windows 95, some 16 bit applications cannot
	 * have stdout redirected to NUL; they send their output to
	 * the console instead.  Some applications, like "more" or "dir /p", 
	 * when outputting multiple pages to the console, also then try and
	 * read from the console to go the next page.  When running tk, this
	 * is fatal because the child process would hang forever waiting
	 * for input from the unmapped console window used by the helper
	 * application.
	 *
	 * Fortunately, the helper application will detect a closed pipe
	 * as a sink.
	 */

	if ((os.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS) 
		&& (applType == APPL_DOS)) {
	    if (CreatePipe(&h, &startInfo.hStdOutput, &secAtts, 0) != FALSE) {
		CloseHandle(h);
	    }
	} else {
	    startInfo.hStdOutput = CreateFile("NUL:", GENERIC_WRITE, 0,
		    &secAtts, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
	}
    } else {
	DuplicateHandle(hProcess, outputHandle, hProcess, &startInfo.hStdOutput, 
		0, TRUE, DUPLICATE_SAME_ACCESS);
    }
    if (startInfo.hStdOutput == INVALID_HANDLE_VALUE) {
	TclWinConvertError(GetLastError());
	Tcl_AppendResult(interp, "couldn't duplicate output handle: ",
		Tcl_PosixError(interp), (char *) NULL);
	goto end;
    }

    if (errorHandle == INVALID_HANDLE_VALUE) {
	/*
	 * If handle was not set, errors should be sent to an infinitely
	 * deep sink.
	 */

	startInfo.hStdError = CreateFile("NUL:", GENERIC_WRITE, 0,
		&secAtts, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    } else {
	DuplicateHandle(hProcess, errorHandle, hProcess, &startInfo.hStdError, 
		0, TRUE, DUPLICATE_SAME_ACCESS);
    } 
    if (startInfo.hStdError == INVALID_HANDLE_VALUE) {
	TclWinConvertError(GetLastError());
	Tcl_AppendResult(interp, "couldn't duplicate error handle: ",
		Tcl_PosixError(interp), (char *) NULL);
	goto end;
    }
    /* 
     * If we do not have a console window, then we must run DOS and
     * WIN32 console mode applications as detached processes. This tells
     * the loader that the child application should not inherit the
     * console, and that it should not create a new console window for
     * the child application.  The child application should get its stdio 
     * from the redirection handles provided by this application, and run
     * in the background.
     *
     * If we are starting a GUI process, they don't automatically get a 
     * console, so it doesn't matter if they are started as foreground or
     * detached processes.  The GUI window will still pop up to the
     * foreground.
     */

    if (os.dwPlatformId == VER_PLATFORM_WIN32_NT) {
	if (HasConsole()) {
	    createFlags = 0;
	} else if (applType == APPL_DOS) {
	    /*
	     * Under NT, 16-bit DOS applications will not run unless they
	     * can be attached to a console.  If we are running without a
	     * console, run the 16-bit program as an normal process inside
	     * of a hidden console application, and then run that hidden
	     * console as a detached process.
	     */

	    startInfo.wShowWindow = SW_HIDE;
	    startInfo.dwFlags |= STARTF_USESHOWWINDOW;
	    createFlags = CREATE_NEW_CONSOLE;
	    Tcl_DStringAppend(&cmdLine, "cmd.exe /c ", -1);
	} else {
	    createFlags = DETACHED_PROCESS;
	} 
    } else {
	if (HasConsole()) {
	    createFlags = 0;
	} else {
	    createFlags = DETACHED_PROCESS;
	}
	
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
	     *
	     * The helper app should be located in the same directory as
	     * the tcl dll.
	     */

	    if (createFlags != 0) {
		startInfo.wShowWindow = SW_HIDE;
		startInfo.dwFlags |= STARTF_USESHOWWINDOW;
		createFlags = CREATE_NEW_CONSOLE;
	    }
	    Tcl_DStringAppend(&cmdLine, "tclpip" STRINGIFY(TCL_MAJOR_VERSION) 
		    STRINGIFY(TCL_MINOR_VERSION) ".dll ", -1);
	}
    }
    
    /*
     * cmdLine gets the full command line used to invoke the executable,
     * including the name of the executable itself.  The command line
     * arguments in argv[] are stored in cmdLine separated by spaces. 
     * Special characters in individual arguments from argv[] must be 
     * quoted when being stored in cmdLine.
     *
     * When calling any application, bear in mind that arguments that 
     * specify a path name are not converted.  If an argument contains 
     * forward slashes as path separators, it may or may not be 
     * recognized as a path name, depending on the program.  In general,
     * most applications accept forward slashes only as option 
     * delimiters and backslashes only as paths.
     *
     * Additionally, when calling a 16-bit dos or windows application, 
     * all path names must use the short, cryptic, path format (e.g., 
     * using ab~1.def instead of "a b.default").  
     */

    BuildCommandLine(argc, argv, &cmdLine);

    if (!CreateProcess(NULL, Tcl_DStringValue(&cmdLine), NULL, NULL, TRUE, 
	    createFlags, NULL, NULL, &startInfo, &procInfo)) {
	TclWinConvertError(GetLastError());
	Tcl_AppendResult(interp, "couldn't execute \"", originalName,
		"\": ", Tcl_PosixError(interp), (char *) NULL);
	goto end;
    }

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

    WaitForInputIdle(procInfo.hProcess, 5000);
    CloseHandle(procInfo.hThread);

    *pidPtr = (int) procInfo.dwProcessId;
    if (*pidPtr != 0) {
	ProcInfo *procPtr = (ProcInfo *)ckalloc(sizeof(ProcInfo));
	procPtr->hProcess = procInfo.hProcess;
	procPtr->dwProcessId = procInfo.dwProcessId;
	procPtr->nextPtr = procList;
	procList = procPtr;
    }
    result = TCL_OK;

    end:
    Tcl_DStringFree(&cmdLine);
    if (startInfo.hStdInput != INVALID_HANDLE_VALUE) {
        CloseHandle(startInfo.hStdInput);
    }
    if (startInfo.hStdOutput != INVALID_HANDLE_VALUE) {
        CloseHandle(startInfo.hStdOutput);
    }
    if (startInfo.hStdError != INVALID_HANDLE_VALUE) {
	CloseHandle(startInfo.hStdError);
    }
    return result;
}


/*
 *----------------------------------------------------------------------
 *
 * HasConsole --
 *
 *	Determines whether the current application is attached to a
 *	console.
 *
 * Results:
 *	Returns TRUE if this application has a console, else FALSE.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static BOOL
HasConsole()
{
    HANDLE handle = CreateFile("CONOUT$", GENERIC_WRITE, FILE_SHARE_WRITE,
	    NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

    if (handle != INVALID_HANDLE_VALUE) {
        CloseHandle(handle);
	return TRUE;
    } else {
        return FALSE;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * TclCreatePipe --
 *
 *      Creates an anonymous pipe.  Under Win32s, creates a temp file
 *	that is used to simulate a pipe.
 *
 * Results:
 *      Returns 1 on success, 0 on failure. 
 *
 * Side effects:
 *      Creates a pipe.
 *
 *----------------------------------------------------------------------
 */

int
TclCreatePipe(readPipe, writePipe)
    Tcl_File *readPipe;	/* Location to store file handle for
				 * read side of pipe. */
    Tcl_File *writePipe;	/* Location to store file handle for
				 * write side of pipe. */
{
    HANDLE readHandle, writeHandle;
    OSVERSIONINFO os;

    if (CreatePipe(&readHandle, &writeHandle, NULL, 0) != 0) {
	*readPipe = Tcl_GetFile((ClientData) readHandle, TCL_WIN_FILE);
	*writePipe = Tcl_GetFile((ClientData) writeHandle, TCL_WIN_FILE);
	return 1;
    }

    os.dwOSVersionInfoSize = sizeof(os);
    GetVersionEx(&os);

    if (os.dwPlatformId == VER_PLATFORM_WIN32s) {
	TclWinPipe *readPipePtr, *writePipePtr;
	char buf[MAX_PATH];

	if ((GetTempPath(MAX_PATH, buf) != 0)
	        && (GetTempFileName(buf, "TCL", 0, buf) != 0)) {

	    readPipePtr = (TclWinPipe *) ckalloc(sizeof(TclWinPipe));
	    writePipePtr = (TclWinPipe *) ckalloc(sizeof(TclWinPipe));

	    readPipePtr->otherPtr = writePipePtr;
	    readPipePtr->fileName = strcpy(ckalloc(strlen(buf) + 1), buf);
	    readPipePtr->fileHandle = INVALID_HANDLE_VALUE;
	    writePipePtr->otherPtr = readPipePtr;
	    writePipePtr->fileName = readPipePtr->fileName;
	    writePipePtr->fileHandle = INVALID_HANDLE_VALUE;
	    *readPipe = Tcl_GetFile((ClientData) readPipePtr, TCL_WIN32S_PIPE);
	    *writePipe = Tcl_GetFile((ClientData) writePipePtr, TCL_WIN32S_PIPE);
	    return 1;
	}
    }

    TclWinConvertError(GetLastError());
    return 0;
}

/*
 *--------------------------------------------------------------------
 *
 * ApplicationType --
 *
 *	Search for the specified program and identify if it refers to a DOS,
 *	Windows 3.X, or Win32 program.  Used to determine how to invoke 
 *	a program, or if it can even be invoked.
 *
 *	It is possible to almost positively identify DOS and Windows 
 *	applications that contain the appropriate magic numbers.  However, 
 *	DOS .com files do not seem to contain a magic number; if the program 
 *	name ends with .com and could not be identified as a Windows .com
 *	file, it will be assumed to be a DOS application, even if it was
 *	just random data.  If the program name does not end with .com, no 
 *	such assumption is made.
 *
 *	The Win32 procedure GetBinaryType incorrectly identifies any 
 *	junk file that ends with .exe as a dos executable and some 
 *	executables that don't end with .exe as not executable.  Plus it 
 *	doesn't exist under win95, so I won't feel bad about reimplementing
 *	functionality.
 *
 * Results:
 *	The return value is one of APPL_DOS, APPL_WIN3X, or APPL_WIN32
 *	if the filename referred to the corresponding application type.
 *	If the file name could not be found or did not refer to any known 
 *	application type, APPL_NONE is returned and an error message is 
 *	left in interp.  .bat files are identified as APPL_DOS.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static int
ApplicationType(interp, originalName, fullPath)
    Tcl_Interp *interp;		/* Interp, for error message. */
    const char *originalName;	/* Name of the application to find. */
    char fullPath[MAX_PATH];	/* Filled with complete path to 
				 * application. */
{
    int applType, i;
    HANDLE hFile;
    char *ext;
    char buf[2];
    DWORD read;
    IMAGE_DOS_HEADER header;
    static char extensions[][5] = {"", ".com", ".exe", ".bat"};

    /* Look for the program as an external program.  First try the name
     * as it is, then try adding .com, .exe, and .bat, in that order, to
     * the name, looking for an executable.
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
	
	if (SearchPath(NULL, fullPath, NULL, MAX_PATH, fullPath, NULL) == 0) {
	    continue;
	}

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
	TclWinConvertError(GetLastError());
	Tcl_AppendResult(interp, "couldn't execute \"", originalName,
		"\": ", Tcl_PosixError(interp), (char *) NULL);
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
 *----------------------------------------------------------------------
 *
 * BuildCommandLine --
 *
 *	The command line arguments are stored in linePtr separated
 *	by spaces, in a form that CreateProcess() understands.  Special 
 *	characters in individual arguments from argv[] must be quoted 
 *	when being stored in cmdLine.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static void
BuildCommandLine(argc, argv, linePtr)
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
    Tcl_DString *linePtr;	/* Initialized Tcl_DString that receives the
				 * command line. */
{
    char *start, *special;
    int quote, i;

    for (i = 0; i < argc; i++) {
	if (i > 0) {
	    Tcl_DStringAppend(linePtr, " ", 1);	
	}

	quote = 0;
	for (start = argv[i]; *start != '\0'; start++) {
	    if (isspace(*start)) {
		quote = 1;
		Tcl_DStringAppend(linePtr, "\"", 1);
    		break;
	    }
	}

	start = argv[i];	    
	for (special = argv[i]; ; ) {
	    if ((*special == '\\') && 
		    (special[1] == '\\' || special[1] == '"')) {
		Tcl_DStringAppend(linePtr, start, special - start);
		start = special;
		while (1) {
		    special++;
		    if (*special == '"') {
			/* 
			 * N backslashes followed a quote -> insert 
			 * N * 2 + 1 backslashes then a quote.
			 */

			Tcl_DStringAppend(linePtr, start, special - start);
			break;
		    }
		    if (*special != '\\') {
			break;
		    }
		}
		Tcl_DStringAppend(linePtr, start, special - start);
		start = special;
	    }
	    if (*special == '"') {
		Tcl_DStringAppend(linePtr, start, special - start);
		Tcl_DStringAppend(linePtr, "\\\"", 2);
		start = special + 1;
	    }
	    if (*special == '\0') {
		break;
	    }
	    special++;
	}
	Tcl_DStringAppend(linePtr, start, special - start);
	if (quote) {
	    Tcl_DStringAppend(linePtr, "\"", 1);
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * MakeTempFile --
 *
 *	Helper function for TclpCreateProcess under Win32s.  Makes a 
 *	temporary file that _won't_ go away automatically when it's file
 *	handle is closed.  Used for simulated pipes, which are written
 *	in one pass and reopened and read in the next pass.
 *
 * Results:
 *	namePtr is filled with the name of the temporary file.
 *
 * Side effects:
 *	A temporary file with the name specified by namePtr is created.  
 *	The caller is responsible for deleting this temporary file.
 *
 *----------------------------------------------------------------------
 */

static char *
MakeTempFile(namePtr)
    Tcl_DString *namePtr;	/* Initialized Tcl_DString that is filled 
				 * with the name of the temporary file that 
				 * was created. */
{
    char name[MAX_PATH];

    if ((GetTempPath(MAX_PATH, name) == 0)
	    || (GetTempFileName(name, "TCL", 0, name) == 0)) {
	return NULL;
    }

    Tcl_DStringAppend(namePtr, name, -1);
    return Tcl_DStringValue(namePtr);
}

/*
 *----------------------------------------------------------------------
 *
 * CopyFileByHandles --
 *
 *	Helper function used by TclpCreateProcess under Win32s.  Copies
 *	what remains of source file to destination file; source file 
 *	pointer need not be positioned at the beginning of the file if
 *	all of source file is not desired, but data is copied up to end 
 *	of source file.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

static void
CopyFileByHandles(dst, src)
    HANDLE dst;			/* Destination file. */
    HANDLE src;			/* Source file. */
{
    char buf[8192];
    DWORD dwRead, dwWrite;

    while (ReadFile(src, buf, sizeof(buf), &dwRead, NULL) != FALSE) {
	if (dwRead == 0) {
	    break;
	}
	if (WriteFile(dst, buf, dwRead, &dwWrite, NULL) == FALSE) {
	    break;
	}
    }
}
