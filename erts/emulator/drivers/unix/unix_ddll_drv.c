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
 * Interface functions to the dynamic linker using dl* functions.
 * (As far as I know it works on SunOS 4, 5, Linux and FreeBSD. /Seb) 
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_ddll.h"
#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#endif


/* some systems do not have RTLD_NOW defined, and require the "mode"
 * argument to dload() always be 1.
 */
#ifndef RTLD_NOW
#  define RTLD_NOW 1
#endif

#define MAX_NAME_LEN 255      /* XXX should we get the system path size? */
#define EXT_LEN      3
#define FILE_EXT     ".so"    /* extension appended to the filename */

/* the ddll driver is single threaded !! */
static int dlopen_error;      /* set if not dl error !! */

#define ERL_DLERR_NAME_TO_LONG  1
#define ERL_DLERR_NOT_AVAILABLE 2     

/* 
 * Open a shared object
 */
void *ddll_open(full_name)
    char *full_name;
{
#if defined(HAVE_DLOPEN)
    char dlname[MAX_NAME_LEN+EXT_LEN+1];
    int len;

    dlopen_error = 0;
    if ((len = sys_strlen(full_name)) >= MAX_NAME_LEN) {
	dlopen_error = ERL_DLERR_NAME_TO_LONG;
	return NULL;
    }
    sys_strcpy(dlname, full_name);
    sys_strcpy(dlname+len, FILE_EXT);
    return dlopen(dlname, RTLD_NOW);
#else
    dlopen_error = ERL_DLERR_NOT_AVAILABLE;
    return NULL;
#endif
}

/* 
 * Find a symbol in the shared object
 */
uint32 *ddll_sym(handle, func_name)
     void *handle;
     char *func_name;
{
#if defined(HAVE_DLOPEN)
    return dlsym(handle, func_name);
#else
    dlopen_error = ERL_DLERR_NOT_AVAILABLE;
    return NULL;
#endif
}

/* 
 * Close a chared object
 */
int ddll_close(handle)
     void *handle;
{
#if defined(HAVE_DLOPEN)
    return dlclose(handle);
#else
    dlopen_error = ERL_DLERR_NOT_AVAILABLE;
    return -1;
#endif
}


/*
 * Return string that describes the (current) error
 */
char *ddll_error()
{
    char* msg = (char *)NULL;

    if (dlopen_error) {
	switch(dlopen_error) {
	case ERL_DLERR_NAME_TO_LONG:
	    return "name to long";
	case ERL_DLERR_NOT_AVAILABLE:
	    return "dynamic loading not implemented on this platform";
	default:
	    return "unknown error";
	}
    }
#ifdef HAVE_DLOPEN
    msg = dlerror();
#endif
    return msg ? msg : "no error";
}
