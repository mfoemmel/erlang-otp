/* 
 * tclLoadOSF.c --
 *
 *	This procedure provides a version of the TclLoadFile that works
 *	under OSF/1 1.0/1.1/1.2 and related systems, utilizing the old OSF/1
 *	/sbin/loader and /usr/include/loader.h.  OSF/1 versions from 1.3 and
 *	on use ELF, rtld, and dlopen()[/usr/include/ldfcn.h].
 *
 *	This is useful for:
 *		OSF/1 1.0, 1.1, 1.2 (from OSF)
 *			includes: MK4 and AD1 (from OSF RI)
 *		OSF/1 1.3 (from OSF) using ROSE
 *		HP OSF/1 1.0 ("Acorn") using COFF
 *
 *	This is likely to be useful for:
 *		Paragon OSF/1 (from Intel) 
 *		HI-OSF/1 (from Hitachi) 
 *
 *	This is NOT to be used on:
 *		Digitial Alpha OSF/1 systems
 *		OSF/1 1.3 or later (from OSF) using ELF
 *			includes: MK6, MK7, AD2, AD3 (from OSF RI)
 *
 *	This approach to things was utter @&^#; thankfully,
 * 	OSF/1 eventually supported dlopen().
 *
 *	John Robert LoVerso <loverso@freebsd.osf.org>
 *
 * Copyright (c) 1995 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tclLoadOSF.c 1.2 96/02/15 11:58:40
 */

#include "tclInt.h"
#include "tclPort.h"
#include <loader.h>
#include "../compat/dlfcn.h"

VOID *dlopen(path, mode)
    CONST char *path;
    int mode;
{
    ldr_module_t lm;
    VOID *handle;

    if (!path) {
	return (VOID *) NULL;
    }

    lm = (ldr_module_t) load(fileName, LDR_NOFLAGS);
    if (lm == LDR_NULL_MODULE) {
	return (VOID *) NULL;
    }

    /*
     * My convention is to use a [OSF loader] package name the same as shlib,
     * since the idiots never implemented ldr_lookup() and it is otherwise
     * impossible to get a package name given a module.
     *
     * I build loadable modules with a makefile rule like 
     *		ld ... -export $@: -o $@ $(OBJS)
     */

    if ((handle = (VOID *) strrchr(path, '/')) == (VOID *) NULL)
	handle = (VOID *) path;
    } else {
	((char *) handle)++;
    }
    return handle;
}

VOID *dlsym(handle, symbol)
    VOID *handle;
    CONST char *symbol;
{
    return (VOID *) ldr_lookup_package((char *) handle, symbol);
}

char *dlerror()
{
    return Tcl_ErrnoMsg(errno);
}
