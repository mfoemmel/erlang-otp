/* 
 * tclLoadNone.c --
 *
 *	This procedure provides a version of the TclLoadFile for use
 *	in systems that don't support dynamic loading; it just returns
 *	an error.
 *
 * Copyright (c) 1995-1996 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tclLoadNone.c 1.5 96/02/15 11:43:01
 */

#include "tclInt.h"
#include "../compat/dlfcn.h"


/*
 *----------------------------------------------------------------------
 *
 * TclLoadFile --
 *
 *	This procedure is called to carry out dynamic loading of binary
 *	code;  it is intended for use only on systems that don't support
 *	dynamic loading (it returns an error).
 *
 * Results:
 *	The result is TCL_ERROR, and an error message is left in
 *	interp->result.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */
#if 0
int
TclLoadFile(interp, fileName, sym1, sym2, proc1Ptr, proc2Ptr)
    Tcl_Interp *interp;		/* Used for error reporting. */
    CONST char *fileName;	/* Name of the file containing the desired
				 * code. */
    CONST char *sym1, *sym2;	/* Names of two procedures to look up in
				 * the file's symbol table. */
    Tcl_PackageInitProc **proc1Ptr, **proc2Ptr;
				/* Where to return the addresses corresponding
				 * to sym1 and sym2. */
{
    Tcl_AppendResult(interp, "couldn't load file \"", fileName,
	    "\": ", dlerror(), (char *) NULL);
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * TclGuessPackageName --
 *
 *	If the "load" command is invoked without providing a package
 *	name, this procedure is invoked to try to figure it out.
 *
 * Results:
 *	Always returns 0 to indicate that we couldn't figure out a
 *	package name;  generic code will then try to guess the package
 *	from the file name.  A return value of 1 would have meant that
 *	we figured out the package name and put it in bufPtr.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

int
TclGuessPackageName(fileName, bufPtr)
    CONST char *fileName;	/* Name of file containing package (already
				 * translated to local form if needed). */
    Tcl_DString *bufPtr;	/* Initialized empty dstring.  Append
				 * package name to this if possible. */
{
    return 0;
}
#endif


/*
 *----------------------------------------------------------------------
 *
 * dlopen  --
 * dlsym   --
 * dlerror --
 * dlclose --
 *
 *	Dummy functions, in case our system doesn't support
 *	dynamic loading.
 *
 * Results:
 *	NULL for dlopen() and dlsym(). Error for other functions.
 *
 * Side effects:
 *	None
 *
 *----------------------------------------------------------------------
 */
VOID *dlopen(path, mode)
    CONST char *path;
    int mode;
{
    return (VOID *) NULL;
}

VOID *dlsym(handle, symbol)
    VOID *handle;
    CONST char *symbol;
{
    return (VOID *) NULL;
}

char *dlerror()
{
    return "dynamic loading is not currently available on this system";
}

int dlclose(handle)
    VOID *handle;
{
    return -1;
}
