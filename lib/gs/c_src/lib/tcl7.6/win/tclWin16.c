/*
 * tclWin16.c --
 *
 *	This file contains code for a 16-bit DLL to handle 32-to-16 bit
 *      thunking. This is necessary for the Win32s SynchSpawn() call.
 *
 * Copyright (c) 1994-1996 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tclWin16.c 1.13 96/09/12 15:10:06
 */

#define STRICT

#include <windows.h>  
#include <toolhelp.h> 

#include <stdio.h>
#include <string.h>

static int 			WinSpawn(char *command);
static int 			DosSpawn(char *command, char *fromFileName,
				    char *toFileName);						
static int			WaitForExit(int inst);


static char pifData[545] = {
'\000', '\013', '\040', '\040', '\040', '\040', '\040', '\040', 
'\040', '\040', '\040', '\040', '\040', '\040', '\040', '\040', 
'\040', '\040', '\040', '\040', '\040', '\040', '\040', '\040', 
'\040', '\040', '\040', '\040', '\040', '\040', '\040', '\040', 
'\200', '\000', '\200', '\000', '\103', '\117', '\115', '\115', 
'\101', '\116', '\104', '\056', '\103', '\117', '\115', '\000', 
'\040', '\040', '\040', '\040', '\040', '\040', '\040', '\040', 
'\040', '\040', '\040', '\040', '\040', '\040', '\040', '\040', 
'\040', '\040', '\040', '\040', '\040', '\040', '\040', '\040', 
'\040', '\040', '\040', '\040', '\040', '\040', '\040', '\040', 
'\040', '\040', '\040', '\040', '\040', '\040', '\040', '\040', 
'\040', '\040', '\040', '\040', '\040', '\040', '\040', '\040',
'\040', '\040', '\040', '\020', '\000', '\000', '\040', '\040', 
'\040', '\040', '\040', '\040', '\040', '\040', '\040', '\040', 
'\040', '\040', '\040', '\040', '\040', '\040', '\040', '\040', 
'\040', '\040', '\040', '\040', '\040', '\040', '\040', '\040', 
'\040', '\040', '\040', '\040', '\040', '\040', '\040', '\040', 
'\040', '\040', '\040', '\040', '\040', '\040', '\040', '\040', 
'\040', '\040', '\040', '\040', '\040', '\040', '\040', '\040', 
'\040', '\040', '\040', '\040', '\040', '\040', '\040', '\040',
'\040', '\040', '\040', '\040', '\040', '\000', '\000', '\000', 
'\000', '\000', '\000', '\000', '\000', '\000', '\000', '\000', 
'\000', '\000', '\000', '\000', '\000', '\000', '\000', '\000', 
'\000', '\000', '\000', '\000', '\000', '\000', '\000', '\000', 
'\000', '\000', '\000', '\000', '\000', '\000', '\000', '\000', 
'\000', '\000', '\000', '\000', '\000', '\000', '\000', '\000', 
'\000', '\000', '\000', '\000', '\000', '\000', '\000', '\000', 
'\000', '\000', '\000', '\000', '\000', '\000', '\000', '\000', 
'\000', '\000', '\000', '\000', '\000', '\177', '\001', '\000', 
'\377', '\031', '\120', '\000', '\000', '\007', '\000', '\000', 
'\000', '\000', '\000', '\000', '\000', '\000', '\000', '\000', 
'\000', '\000', '\000', '\000', '\000', '\000', '\000', '\000', 
'\000', '\000', '\000', '\000', '\000', '\000', '\000', '\000', 
'\000', '\000', '\000', '\000', '\000', '\000', '\000', '\000', 
'\000', '\000', '\000', '\000', '\000', '\000', '\000', '\000', 
'\000', '\000', '\000', '\000', '\000', '\000', '\000', '\000', 
'\000', '\000', '\000', '\000', '\000', '\000', '\000', '\000', 
'\000', '\000', '\000', '\000', '\000', '\000', '\000', '\000',
'\000', '\000', '\000', '\000', '\000', '\000', '\000', '\000', 
'\000', '\000', '\000', '\000', '\000', '\000', '\000', '\000', 
'\000', '\000', '\000', '\000', '\000', '\000', '\000', '\000', 
'\000', '\000', '\000', '\000', '\000', '\000', '\000', '\000', 
'\000', '\000', '\000', '\000', '\000', '\000', '\000', '\000', 
'\000', '\000', '\000', '\000', '\000', '\000', '\000', '\000', 
'\000', '\000', '\000', '\000', '\000', '\000', '\000', '\000', 
'\000', '\000', '\000', '\000', '\000', '\000', '\000', '\340', 
'\040', '\115', '\111', '\103', '\122', '\117', '\123', '\117', 
'\106', '\124', '\040', '\120', '\111', '\106', '\105', '\130', 
'\000', '\207', '\001', '\000', '\000', '\161', '\001', '\127', 
'\111', '\116', '\104', '\117', '\127', '\123', '\040', '\063',
'\070', '\066', '\040', '\063', '\056', '\060', '\000', '\005', 
'\002', '\235', '\001', '\150', '\000', '\200', '\002', '\200', 
'\000', '\144', '\000', '\062', '\000', '\000', '\004', '\000', 
'\000', '\000', '\004', '\000', '\000', '\002', '\020', '\002', 
'\000', '\037', '\000', '\000', '\000', '\000', '\000', '\000', 
'\000', '\000', '\000', '\000', '\000', '\000', '\000', '\000', 
'\000', '\000', '\000', '\000', '\000', '\057', '\143', '\040', 
'\146', '\157', '\157', '\056', '\142', '\141', '\164', '\000', 
'\040', '\040', '\040', '\040', '\040', '\040', '\040', '\040', 
'\040', '\040', '\040', '\040', '\040', '\040', '\040', '\040', 
'\040', '\040', '\040', '\040', '\040', '\040', '\040', '\040', 
'\040', '\040', '\040', '\040', '\040', '\040', '\040', '\040', 
'\040', '\040', '\040', '\040', '\040', '\040', '\040', '\040', 
'\040', '\040', '\040', '\040', '\040', '\040', '\040', '\040', 
'\040', '\040', '\040', '\040', '\040', '\127', '\111', '\116', 
'\104', '\117', '\127', '\123', '\040', '\062', '\070', '\066', 
'\040', '\063', '\056', '\060', '\000', '\377', '\377', '\033', 
'\002', '\006', '\000', '\000', '\000', '\000', '\000', '\000', 
'\000'
};

BOOL CALLBACK 
LibMain(HINSTANCE hinst, WORD wDS, WORD cbHeap, LPSTR unused)
{
    // Nothing to do.      

    hinst = hinst;
    wDS = wDS;
    cbHeap = cbHeap;
    unused = unused;

    return TRUE;
}

int WINAPI
UTProc(buf, func)
    void *buf;
    DWORD func;
{
    char **args;

    args = (char **) buf;
    if (func == 0) {
	return DosSpawn(args[0], args[1], args[2]);
    } else {
	return WinSpawn(args[0]);
    }
}

static int
WinSpawn(command)
    char *command;
{
    return WaitForExit(WinExec(command, SW_SHOW));
}
/*
 *---------------------------------------------------------------------------
 *
 * Spawn --
 *
 *---------------------------------------------------------------------------
 */
static int
DosSpawn(command, fromFileName, toFileName)
    char *command;		/* The name of the program, plus any
				 * arguments, to be run. */
    char *fromFileName;		/* Standard input for the program is to be
				 * redirected from this file, or NULL for no
				 * standard input. */
    char *toFileName;		/* Standard output for the program is to be
				 * redirected to this file, or NULL to
				 * discard standard output. */
{
    int result;
    HFILE batFile, pifFile;
    char batFileName[144], pifFileName[144];

    GetTempFileName(0, "tcl", 0, batFileName);
    unlink(batFileName);
    strcpy(strrchr(batFileName, '.'), ".bat");
    batFile = _lcreat(batFileName, 0);

    GetTempFileName(0, "tcl", 0, pifFileName);
    unlink(pifFileName);
    strcpy(strrchr(pifFileName, '.'), ".pif");
    pifFile = _lcreat(pifFileName, 0);

    _lwrite(batFile, command, strlen(command));
    if (fromFileName == NULL) {
	_lwrite(batFile, " < nul", 6);
    } else {
	_lwrite(batFile, " < ", 3);
	_lwrite(batFile, fromFileName, strlen(fromFileName));
    }
    if (toFileName == NULL) {
	_lwrite(batFile, " > nul", 6);
    } else {
	_lwrite(batFile, " > ", 3);
	_lwrite(batFile, toFileName, strlen(toFileName));
    }
    _lwrite(batFile, "\r\n\032", 3);
    _lclose(batFile);

    strcpy(pifData + 0x1c8, batFileName);
    _lwrite(pifFile, pifData, sizeof(pifData));
    _lclose(pifFile);

    result = WaitForExit(WinExec(pifFileName, SW_MINIMIZE));

    unlink(pifFileName);
    unlink(batFileName);

    return result;
}

/*
 * Results:
 * 	The return value is 1 if the process exited successfully,
 *	or 0 otherwise.
 *
 * Side effects:
 *	None.
 *
 *---------------------------------------------------------------------------
 */

static int
WaitForExit(inst)
    int inst;			/* Identifies the instance handle of the
				 * process to wait for. */
{
    TASKENTRY te;
    MSG msg;

    if (inst < 32) {
	return 0;
    }

    te.dwSize = sizeof(te);
    te.hInst = 0;
    TaskFirst(&te);
    do {
	if (te.hInst == (HINSTANCE) inst) {
	    break;
	}
    } while (TaskNext(&te) != FALSE);

    if (te.hInst != (HINSTANCE) inst) {
	return 0;
    }
    while (1) {
	if (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE) == TRUE) {
	    TranslateMessage(&msg);
	    DispatchMessage(&msg);
	}
	TaskFirst(&te);
	do {
	    if (te.hInst == (HINSTANCE) inst) {
		break;
	    }
	} while (TaskNext(&te) != FALSE);

	if (te.hInst != (HINSTANCE) inst) {
	    return 1;
	}
    }
}
#if 0




 #ifndef APIENTRY
#define APIENTRY
#endif

#include <windows.h>
#include <malloc.h>
#include <toolhelp.h>
#ifdef _MSC_VER
#include "tclWinInt.h"
#else
#include "tclWinIn.h"
#endif

typedef DWORD (FAR PASCAL  * UT16CBPROC)(LPVOID lpBuff, DWORD dwUserDefined,
	LPVOID FAR *lpTranslationList);

HINSTANCE hInstance;


/*
 *----------------------------------------------------------------------
 *
 * LibMain --
 *
 *	DLL entry point
 *
 * Results:
 *	Returns 1.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */
int FAR PASCAL
LibMain(instance, dataSeg, heapSize, cmdLine)
    HINSTANCE instance;
    WORD dataSeg;
    WORD heapSize;
    LPSTR cmdLine;
{
    hInstance = instance;
    return 1;
}


/*
 *----------------------------------------------------------------------
 *
 * UTInit --
 *
 *	Universal Thunk initialization procedure.
 *
 * Results:
 *	Always returns 1.
 *
 * Side effects:
 *	Sets the universal thunk callback procedure.
 *
 *----------------------------------------------------------------------
 */
DWORD FAR PASCAL _export
UTInit(callback, buf)
    UT16CBPROC callback;
    LPVOID buf;
{
    return 1;   /* Return Success */
} 

/*
 *----------------------------------------------------------------------
 *
 * UTProc --
 *
 *	Universal Thunk dispatch routine.
 *
 * Results:
 *	1 on success, 0 or -1 on failure.
 *
 * Side effects:
 *	Executes 16-bit code.
 *
 *----------------------------------------------------------------------
 */

DWORD FAR PASCAL _export
UTProc(buf, func)
    LPVOID buf;
    DWORD func;
{
    char **argv;

    argv = (char **) buf;

    if (func == 0) {



    switch (func) {

	case TCLSYNCHSPAWN: {
	    HINSTANCE inst;
	    LPCSTR cmdLine;
	    UINT cmdShow;
	    MSG msg;
	    TASKENTRY te;
	    
	    /* Retrieve the command line arguments stored in buffer */

	    cmdLine = (LPSTR) ((LPDWORD)buf)[0];
	    cmdShow = (UINT) ((LPDWORD)buf)[1];
	    
	    /* Start the application with WinExec() */
	    
	    inst = WinExec(cmdLine, cmdShow);
	    if ((int) inst < 32) {
		return 0;
	    }

	    /* Loop until the application is terminated. The Toolhelp API
	     * ModuleFindHandle() returns NULL when the application is
	     * terminated. NOTE: PeekMessage() is used to yield the
	     * processor; otherwise, nothing else could execute on the
	     * system.
	     */

	    te.dwSize = sizeof(TASKENTRY);
	    TaskFirst(&te);
	    do {
	    	if (te.hInst == inst) {
	    	    break;
	    	}
	    } while (TaskNext(&te));
	    
	    if (te.hInst == inst) {
	    	while (1) {
	    	    if (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE) != 0) {
	    	    	TranslateMessage(&msg);
	    	    	DispatchMessage(&msg);
	    	    }
	    	    
	    	    TaskFirst(&te);
	    	    do {
	    	    	if (te.hInst == inst) {
	    	    	    break;
	    	    	}
	    	    } while (TaskNext(&te));
	    	    
	    	    if (te.hInst != inst) {
	    	        break;
	    	    }
	    	}
	    }
	    return 1;
	}
    }

    return (DWORD)-1L; /* We should never get here. */
}

/*
 *----------------------------------------------------------------------
 *
 * _WEP --
 *
 *	Windows exit procedure
 *
 * Results:
 *	Always returns 1.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

int FAR PASCAL
_WEP(dummy)
    int dummy;
{
   return 1;
}
#endif
