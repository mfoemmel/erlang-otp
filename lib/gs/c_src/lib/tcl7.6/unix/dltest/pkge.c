/* 
 * pkge.c --
 *
 *	This file contains a simple Tcl package "pkge" that is intended
 *	for testing the Tcl dynamic loading facilities.  Its Init
 *	procedure returns an error in order to test how this is handled.
 *
 * Copyright (c) 1995 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) pkge.c 1.5 96/03/07 09:34:27
 */
#include "tcl.h"


/*
 *----------------------------------------------------------------------
 *
 * Pkge_Init --
 *
 *	This is a package initialization procedure, which is called
 *	by Tcl when this package is to be added to an interpreter.
 *
 * Results:
 *	Returns TCL_ERROR and leaves an error message in interp->result.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

int
#ifdef _USING_PROTOTYPES_
Pkge_Init (
    Tcl_Interp *interp)		/* Interpreter in which the package is
				 * to be made available. */
#else
Pkge_Init(interp)
    Tcl_Interp *interp;		/* Interpreter in which the package is
				 * to be made available. */
#endif
{
    return Tcl_Eval(interp, "if 44 {open non_existent}");
}
