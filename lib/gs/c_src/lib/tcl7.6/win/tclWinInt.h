/*
 * tclWinInt.h --
 *
 *	Declarations of Windows-specific shared variables and procedures.
 *
 * Copyright (c) 1994-1996 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tclWinInt.h 1.4 96/10/03 14:58:46
 */

#ifndef _TCLWININT
#define _TCLWININT

#ifndef _TCLINT
#include "tclInt.h"
#endif
#ifndef _TCLPORT
#include "tclPort.h"
#endif

/*
 * Some versions of Borland C have a define for the OSVERSIONINFO for
 * Win32s and for NT, but not for Windows 95.
 */

#ifndef VER_PLATFORM_WIN32_WINDOWS
#define VER_PLATFORM_WIN32_WINDOWS 1
#endif

/*
 * The following structure represents a synchronous pipe under Win32s.
 * It is stored as the clientData for a Tcl_File of type TCL_WIN32S_PIPE.
 */

typedef struct TclWinPipe {
    struct TclWinPipe *otherPtr;/* Pointer to the TclWinPipe structure that
				 * corresponds to the other end of this 
				 * pipe. */
    char *fileName;		/* The name of the staging file that gets 
				 * the data written to this pipe.  Malloc'd.
				 * and shared by both ends of the pipe.  Only
				 * when both ends are freed will fileName be
				 * freed and the file it refers to deleted. */
    HANDLE fileHandle;		/* When Tcl is reading from "pipe", this 
				 * handle will refer to the open fileName.  
				 * Otherwise, it is INVALID_HANDLE_VALUE. */
} TclWinPipe;


EXTERN int		TclSynchSpawn(void *args, int type, void **trans,
				      int *pipePtr);


#endif	/* _TCLWININT */
