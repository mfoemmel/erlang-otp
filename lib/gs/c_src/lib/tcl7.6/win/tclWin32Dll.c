/* 
 * tclWin32Dll.c --
 *
 *	This file contains the DLL entry point which sets up the 32-to-16-bit
 *	thunking code for SynchSpawn if the library is running under Win32s.
 *
 * Copyright (c) 1995-1996 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tclWin32Dll.c 1.15 96/09/12 15:10:59
 */

#include <windows.h>
#include "tcl.h"
#include "tclPort.h"
#include "tclWinInt.h"
#include "../compat/dlfcn.h"

typedef DWORD (WINAPI * UT32PROC)(LPVOID lpBuff, DWORD dwUserDefined,
	LPVOID *lpTranslationList);

typedef BOOL (WINAPI * PUTREGISTER)(HANDLE hModule, LPCSTR SixteenBitDLL,
	LPCSTR InitName, LPCSTR ProcName, UT32PROC* ThirtyTwoBitThunk,
	FARPROC UT32Callback, LPVOID Buff);

typedef VOID (WINAPI * PUTUNREGISTER)(HANDLE hModule);

static PUTUNREGISTER UTUnRegister = NULL;
static int           tclProcessesAttached = 0;

/*
 * The following data structure is used to keep track of all of the DLL's
 * opened by Tcl so that they can be freed with the Tcl.dll is unloaded.
 */

typedef struct LibraryList {
    HINSTANCE handle;
    struct LibraryList *nextPtr;
} LibraryList;

static LibraryList *libraryList = NULL;	/* List of currently loaded DLL's.  */

static HINSTANCE tclInstance;		/* Global library instance handle. */

/*
 * Declarations for functions that are only used in this file.
 */

static void 		UnloadLibraries _ANSI_ARGS_((void));

/*
 * The following declaration is for the VC++ DLL entry point.
 */

BOOL APIENTRY		DllMain _ANSI_ARGS_((HINSTANCE hInst,
			    DWORD reason, LPVOID reserved));

/*
 *----------------------------------------------------------------------
 *
 * DllEntryPoint --
 *
 *	This wrapper function is used by Borland to invoke the
 *	initialization code for Tcl.  It simply calls the DllMain
 *	routine.
 *
 * Results:
 *	See DllMain.
 *
 * Side effects:
 *	See DllMain.
 *
 *----------------------------------------------------------------------
 */

BOOL APIENTRY
DllEntryPoint(hInst, reason, reserved)
    HINSTANCE hInst;		/* Library instance handle. */
    DWORD reason;		/* Reason this function is being called. */
    LPVOID reserved;		/* Not used. */
{
    return DllMain(hInst, reason, reserved);
}

/*
 *----------------------------------------------------------------------
 *
 * DllMain --
 *
 *	This routine is called by the VC++ C run time library init
 *	code, or the DllEntryPoint routine.  It is responsible for
 *	initializing various dynamically loaded libraries.
 *
 * Results:
 *	TRUE on sucess, FALSE on failure.
 *
 * Side effects:
 *	Establishes 32-to-16 bit thunk and initializes sockets library.
 *
 *----------------------------------------------------------------------
 */
BOOL APIENTRY
DllMain(hInst, reason, reserved)
    HINSTANCE hInst;		/* Library instance handle. */
    DWORD reason;		/* Reason this function is being called. */
    LPVOID reserved;		/* Not used. */
{
    switch (reason) {
    case DLL_PROCESS_ATTACH:

	/*
	 * Registration of UT need to be done only once for first
	 * attaching process.  At that time set the tclWin32s flag
	 * to indicate if the DLL is executing under Win32s or not.
	 */

	if (tclProcessesAttached++) {
	    return FALSE;         /* Not the first initialization. */
	}

	tclInstance = hInst;
	return TRUE;

    case DLL_PROCESS_DETACH:

	tclProcessesAttached--;
	if (tclProcessesAttached == 0) {

	    /*
	     * Unregister the Tcl thunk.
	     */

	    if (UTUnRegister != NULL) {
		UTUnRegister(hInst);
	    }

	    /*
	     * Cleanup any dynamically loaded libraries.
	     */

	    UnloadLibraries();
	}
	break;
    }

    return TRUE; 
}

/*
 *----------------------------------------------------------------------
 *
 * TclWinLoadLibrary --
 *
 *	This function is a wrapper for the system LoadLibrary.  It is
 *	responsible for adding library handles to the library list so
 *	the libraries can be freed when tcl.dll is unloaded.
 *
 * Results:
 *	Returns the handle of the newly loaded library, or NULL on
 *	failure.
 *
 * Side effects:
 *	Loads the specified library into the process.
 *
 *----------------------------------------------------------------------
 */

HINSTANCE
TclWinLoadLibrary(name)
    char *name;			/* Library file to load. */
{
    HINSTANCE handle;
    LibraryList *ptr;

    handle = LoadLibrary(name);
    if (handle != NULL) {
	ptr = (LibraryList*) ckalloc(sizeof(LibraryList));
	ptr->handle = handle;
	ptr->nextPtr = libraryList;
	libraryList = ptr;
    } else {
	TclWinConvertError(GetLastError());
    }
    return handle;
}

/*
 *----------------------------------------------------------------------
 *
 * UnloadLibraries --
 *
 *	Frees any dynamically allocated libraries loaded by Tcl.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Frees the libraries on the library list as well as the list.
 *
 *----------------------------------------------------------------------
 */

static void
UnloadLibraries()
{
    LibraryList *ptr;

    while (libraryList != NULL) {
	FreeLibrary(libraryList->handle);
	ptr = libraryList->nextPtr;
	ckfree(libraryList);
	libraryList = ptr;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * TclSynchSpawn --
 *
 *	32-bit entry point to the 16-bit SynchSpawn code.
 *
 * Results:
 *	1 on success, 0 on failure.
 *
 * Side effects:
 *	Spawns a command and waits for it to complete.
 *
 *----------------------------------------------------------------------
 */
int 
TclSynchSpawn(void *args, int type, void **trans, int *pidPtr)
{
    static UT32PROC UTProc = NULL;
    static int utErrorCode;

    if (UTUnRegister == NULL) {
	/*
	 * Load the Universal Thunking routines from kernel32.dll.
	 */

	HINSTANCE hKernel;
	PUTREGISTER UTRegister;
	char buffer[] = "TCL16xx.DLL";

	hKernel = TclWinLoadLibrary("Kernel32.Dll");
	if (hKernel == NULL) {
	    return 0;
	}

	UTRegister = (PUTREGISTER) GetProcAddress(hKernel, "UTRegister");
	UTUnRegister = (PUTUNREGISTER) GetProcAddress(hKernel, "UTUnRegister");
	if (!UTRegister || !UTUnRegister) {
	    UnloadLibraries();
	    return 0;
	}

	/*
	 * Construct the complete name of tcl16xx.dll.
	 */

	buffer[5] = '0' + TCL_MAJOR_VERSION;
	buffer[6] = '0' + TCL_MINOR_VERSION;

	/*
	 * Register the Tcl thunk.
	 */

	if (UTRegister(tclInstance, buffer, NULL, "UTProc", &UTProc, NULL,
		NULL) == FALSE) {
	    utErrorCode = GetLastError();
	}
    }

    if (UTProc == NULL) {
	/*
	 * The 16-bit thunking DLL wasn't found.  Return error code that
	 * indicates this problem.
	 */

	SetLastError(utErrorCode);
	return 0;
    }

    UTProc(args, type, trans);
    *pidPtr = 0;
    return 1;
}

/*
 *----------------------------------------------------------------------
 *
 * TclWinGetTclInstance --
 *
 *	Retrieves the global library instance handle.
 *
 * Results:
 *	Returns the global library instance handle.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

HINSTANCE
TclWinGetTclInstance()
{
    return tclInstance;
}


/*
 *----------------------------------------------------------------------
 *
 * dlopen --
 *
 *	This function is an alternative for the functions
 *	TclWinLoadLibrary and TclWinGetTclInstance.  It is
 *	responsible for adding library handles to the library list so
 *	the libraries can be freed when tcl.dll is unloaded.
 *
 * Results:
 *	Returns the handle of the newly loaded library, or NULL on
 *	failure. If path is NULL, the global library instance handle
 *	is returned.
 *
 * Side effects:
 *	Loads the specified library into the process.
 *
 *----------------------------------------------------------------------
 */

VOID *dlopen(path, mode)
    CONST char *path;
    int mode;
{
    VOID *handle;
    LibraryList *ptr;

    if (path == (char *) NULL) {
	return (VOID *) tclInstance;
    }
    handle = (VOID *) LoadLibrary(path);
    if (handle != NULL) {
	    ptr = (LibraryList*) ckalloc(sizeof(LibraryList));
	    ptr->handle = (HINSTANCE) handle;
	    ptr->nextPtr = libraryList;
	    libraryList = ptr;
    }
    return handle;
}


/*
 *----------------------------------------------------------------------
 *
 * dlclose --
 *
 *	This function is an alternative for the function
 *	FreeLibrary.  It is responsible for removing library
 *	handles from the library list and remove the dll
 *	from memory.
 *
 * Results:
 *	-1 on error, 0 on success.
 *
 * Side effects:
 *	Removes the specified library from the process.
 *
 *----------------------------------------------------------------------
 */

int
dlclose(handle)
    VOID *handle;
{
    LibraryList *ptr, *prevPtr;

    ptr = libraryList; prevPtr = NULL;
    while (ptr != NULL) {
	if (ptr->handle == (HINSTANCE) handle) {
	    FreeLibrary((HINSTANCE) handle);
	    if (prevPtr) {
		prevPtr->nextPtr = ptr->nextPtr;
	    } else {
		libraryList = ptr->nextPtr;
	    }
	    ckfree(ptr);
	    return 0;
	}
	prevPtr = ptr;
	ptr = ptr->nextPtr;
    }
    return -1;
}

/*
 *----------------------------------------------------------------------
 *
 * dlsym --
 *
 *	This function is an alternative for the system function
 *	GetProcAddress. It returns the address of a
 *	symbol, give the handle returned by dlopen().
 *
 * Results:
 *	Returns the address of the symbol in the dll.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

VOID *dlsym(handle, symbol)
    VOID *handle;
    CONST char *symbol;
{
    return (VOID *) GetProcAddress((HINSTANCE) handle, symbol);
}


/*
 *----------------------------------------------------------------------
 *
 * dlerror --
 *
 *	This function returns a string describing the error which
 *	occurred in dlopen().
 *
 * Results:
 *	Returns an error message.
 *
 * Side effects:
 *	errno is set.
 *
 *----------------------------------------------------------------------
 */
char *dlerror()
{
    TclWinConvertError(GetLastError());
    return Tcl_ErrnoMsg(errno);
}
