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
 * Interface functions for the Windows dynamic loader.
 */
#include <windows.h>

#include "sys.h"
#include "erl_ddll.h"

#define EXT_LEN          4
#define FILE_EXT         ".dll"


/* XXX The ddll driver is single threaded */
static int dl_error;
static int dlopen_error;

#define ERL_DLERR_NAME_TO_LONG  1
#define ERL_DLERR_SYM_NOT_FOUND 2
#define ERL_DLERR_BAD_VERSION   3

#define APIINITSYM       "_ERL__DRIVER_INIT"

/* 
 * Open a shared object
 */
void *ddll_open(full_name)
    char *full_name;
{
    HINSTANCE handle;
    char* name;
    int len;
    char dlname[MAXPATHLEN + EXT_LEN + 1];

    dlopen_error = 0;
    name = full_name;

    if ((len = sys_strlen(name)) > MAXPATHLEN-EXT_LEN-1) {
	dlopen_error = ERL_DLERR_NAME_TO_LONG;
	return NULL;
    }
    sys_strcpy(dlname, name);
    sys_strcpy(dlname+len, FILE_EXT);

    if ((handle = LoadLibrary(dlname)) == NULL) {
	dl_error = GetLastError();
	return NULL;
    }

    return handle;
}

/* 
 * Find a symbol in the shared object
 */
void *ddll_sym(void *handle, char *func_name)
{
    return GetProcAddress((HINSTANCE) handle, func_name);
}

/* 
 * Close a chared object
 */
int ddll_close(handle)
    void *handle;
{
    FreeLibrary((HINSTANCE) handle);
    return 0;
}

/*
 * Return string that describes the (current) error
 */
char *ddll_error()
{
    if (dlopen_error) {
	switch(dlopen_error) {
	case ERL_DLERR_NAME_TO_LONG:
	    return "name to long";
	case ERL_DLERR_SYM_NOT_FOUND:
	    return "bad driver (link with erl_dll.lib)";
	case ERL_DLERR_BAD_VERSION:
	    return "bad driver version (recompile)";
	default:
	    return "unknown error";
	}
    }
    return win32_errorstr(dl_error);
}
