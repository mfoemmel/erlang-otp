/* 
 * tkWinInit.c --
 *
 *	This file contains Windows-specific interpreter initialization
 *	functions.
 *
 * Copyright (c) 1995 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tkWinInit.c 1.13 96/03/18 14:22:29
 */

#include "tkWinInt.h"

#ifndef TCL_ACTIVE
#define TCL_ACTIVE (1<<4)
#endif

static void		DisplayCheckProc _ANSI_ARGS_((ClientData clientData,
			    int flags));
static void		DisplaySetupProc _ANSI_ARGS_((ClientData clientData,
			    int flags));
/*
 * Default directory in which to look for libraries:
 */

#ifndef TK_LIBRARY
#define TK_LIBRARY "."
#endif

static char defaultLibraryDir[200] = TK_LIBRARY;

/*
 * The following string is the startup script executed in new
 * interpreters.  It looks on disk in several different directories
 * for a script "tk.tcl" that is compatible with this version
 * of Tk.  The tk.tcl script does all of the real work of
 * initialization.
 */

static char *initScript =
"proc init {} {\n\
    global tk_library tk_version tk_patchLevel env\n\
    rename init {}\n\
    set dirs {}\n\
    if [info exists env(TK_LIBRARY)] {\n\
	lappend dirs $env(TK_LIBRARY)\n\
    }\n\
    lappend dirs $tk_library\n\
    set tk_library {}\n\
    if ![catch {uplevel #0 source -rsrc tk}] {\n\
	uplevel #0 {\n\
	    source -rsrc button\n\
	    source -rsrc entry\n\
	    source -rsrc listbox\n\
	    source -rsrc menu\n\
	    source -rsrc scale\n\
	    source -rsrc scrlbar\n\
	    source -rsrc text\n\
	}\n\
	return\n\
    }\n\
    lappend dirs [file join [file dirname [info library]] tk$tk_version]\n\
    set parentDir [file dirname [file dirname [info nameofexecutable]]]\n\
    lappend dirs [file join $parentDir lib tk$tk_version]\n\
    if {![regexp {.*[ab][12345]} $tk_patchLevel lib]} {\n\
	set lib tk$tk_version\n\
    }\n\
    lappend dirs [file join [file dirname $parentDir] $lib library]\n\
    lappend dirs [file join $parentDir library]\n\
    foreach i $dirs {\n\
	set tk_library $i\n\
	if ![catch {uplevel #0 source [list [file join $i tk.tcl]]}] {\n\
	    return\n\
	}\n\
    }\n\
    set tk_library {}\n\
    set msg \"Can't find a usable tk.tcl in the following directories: \n\"\n\
    append msg \"    $dirs\n\"\n\
    append msg \"This probably means that Tk wasn't installed properly.\n\"\n\
    error $msg\n\
}\n\
init";

/*
 * The following script is used to initialize Tk in a safe interpreter.
 */

static char safeInitScript[] =
"proc tkInit {} {\n\
    global tk_library tk_version tk_patchLevel env\n\
    rename tkInit {}\n\
    set dirs {}\n\
    if [info exists env(TK_LIBRARY)] {\n\
	lappend dirs $env(TK_LIBRARY)\n\
    }\n\
    lappend dirs $tk_library\n\
    set tk_library {}\n\
    if ![catch {uplevel #0 source -rsrc tk}] {\n\
	uplevel #0 {\n\
	    source -rsrc button\n\
	    source -rsrc entry\n\
	    source -rsrc listbox\n\
	    source -rsrc menu\n\
	    source -rsrc scale\n\
	    source -rsrc scrlbar\n\
	    source -rsrc text\n\
	}\n\
	return\n\
    }\n\
    lappend dirs [file join [file dirname [info library]] tk$tk_version]\n\
    set parentDir [file dirname [file dirname [info nameofexecutable]]]\n\
    lappend dirs [file join $parentDir lib tk$tk_version]\n\
    if {![regexp {.*[ab][12345]} $tk_patchLevel lib]} {\n\
	set lib tk$tk_version\n\
    }\n\
    lappend dirs [file join [file dirname [file dirname [info library]]] $lib library]\n\
    foreach i $dirs {\n\
	set tk_library $i\n\
	if ![catch {uplevel #0 source [list [file join $i tk.tcl]]}] {\n\
	    return\n\
	}\n\
    }\n\
    set tk_library {}\n\
    set msg \"Can't find a usable tk.tcl in the following directories: \n\"\n\
    append msg \"    $dirs\n\"\n\
    append msg \"This probably means that Tk wasn't installed properly.\n\"\n\
    error $msg\n\
}\n\
tkInit";


/*
 *----------------------------------------------------------------------
 *
 * DisplaySetupProc --
 *
 *	This procedure is part of the event source for Mac displays.
 *	It is invoked by Tcl_DoOneEvent before it calls select to check
 *	for events on all displays. Because for the Mac this kind of
 *	events is handled already in Tcl, all that needs to be done is
 *	check if there are any windows open.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Tells the notifier whether the Tk event-handler is active or not.
 *
 *----------------------------------------------------------------------
 */

static void
DisplaySetupProc(clientData, flags)
    ClientData clientData;		/* Not used. */
    int flags;				/* Flags passed to Tk_DoOneEvent:
					 * if it doesn't include
					 * TCL_WINDOW_EVENTS then we do
					 * nothing. */
{
    if ((flags & TCL_WINDOW_EVENTS) && (Tk_GetNumMainWindows()>0)) {
	Tcl_WatchFile((Tcl_File) NULL, TCL_ACTIVE);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * DisplayCheckProc --
 *
 *	This procedure is just a dummy function, because Mac events
 *	are handled in Tcl.
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
DisplayCheckProc(clientData, flags)
    ClientData clientData;		/* Not used. */
    int flags;				/* Not used. */
{
    return;
}

/*
 *----------------------------------------------------------------------
 *
 * TkPlatformInit --
 *
 *	Performs Windows-specific interpreter initialization related to the
 *      tk_library variable.
 *
 * Results:
 *	A standard Tcl completion code (TCL_OK or TCL_ERROR).  Also
 *	leaves information in interp->result.
 *
 * Side effects:
 *	Sets "tk_library" Tcl variable, runs "tk.tcl" script.
 *
 *----------------------------------------------------------------------
 */

int
TkPlatformInit(interp)
    Tcl_Interp *interp;
{
    char *libDir;
    static int initialized = 0;

    if (!initialized) {
	Tcl_CreateEventSource(DisplaySetupProc, DisplayCheckProc,
		(ClientData) NULL);
	initialized = 1;
    }

    libDir = Tcl_GetVar(interp, "tk_library", TCL_GLOBAL_ONLY);
    if (libDir == NULL) {
	Tcl_SetVar(interp, "tk_library", defaultLibraryDir, TCL_GLOBAL_ONLY);
    }

    if (Tcl_IsSafe(interp)) {
        return Tcl_Eval(interp, safeInitScript);
    }
    return Tcl_Eval(interp, initScript);
}
