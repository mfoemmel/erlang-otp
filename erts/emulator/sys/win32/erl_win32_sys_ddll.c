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

#include <windows.h>

#define GET_ERTS_ALC_TEST
#include "sys.h"
#include "global.h"
#include "erl_alloc.h"

#include "erl_driver.h"
#include "erl_win_dyn_driver.h"

#define EXT_LEN          4
#define FILE_EXT         ".dll"

static DWORD tls_index = 0;
static TWinDynDriverCallbacks wddc;

void erl_sys_ddll_init(void) {
    tls_index = TlsAlloc();
    ERL_INIT_CALLBACK_STRUCTURE(wddc);
    return;
}
/* 
 * Open a shared object
 */
int erts_sys_ddll_open(char *full_name, void **handle)
{
    HINSTANCE hinstance;
    int len;
    int ret = ERL_DE_NO_ERROR;
    char dlname[MAXPATHLEN + EXT_LEN + 1];

    if ((len = sys_strlen(full_name)) > MAXPATHLEN-EXT_LEN-1) {
	ret = ERL_DE_LOAD_ERROR_NAME_TO_LONG;
	goto done;
    }
    sys_strcpy(dlname, full_name);
    sys_strcpy(dlname+len, FILE_EXT);
    if ((hinstance = LoadLibrary(dlname)) == NULL) {
	ret = ERL_DE_DYNAMIC_ERROR_OFFSET - GetLastError();
	goto done;
    } else {
	*handle = (void *) hinstance;
    }
 done:
    return ret;
}

/* 
 * Find a symbol in the shared object
 */
int erts_sys_ddll_sym(void *handle, char *func_name, void **function)
{
    FARPROC proc;
    if ((proc = GetProcAddress( (HINSTANCE) handle, func_name)) == NULL) {
	return  ERL_DE_DYNAMIC_ERROR_OFFSET - GetLastError();
    }
    *function = (void *) proc;
    return ERL_DE_NO_ERROR;
}

/* XXX:PaN These two will be changed with new driver interface! */

/* 
 * Load the driver init function, might appear under different names depending on object arch... 
 */

int erts_sys_ddll_load_driver_init(void *handle, void **function)
{
    void *fn;
    int res;
    if ((res = erts_sys_ddll_sym(handle, "driver_init", &fn)) != ERL_DE_NO_ERROR) {
	return res;
    }
    *function = fn;
    return res;
}

/* 
 * Call the driver_init function, whatever it's really called, simple on unix... 
*/
void *erts_sys_ddll_call_init(void *function) {
    void *(*initfn)(TWinDynDriverCallbacks *) = function;
    return (*initfn)(&wddc);
}


/* 
 * Close a chared object
 */
int erts_sys_ddll_close(void *handle)
{
    if (!FreeLibrary((HINSTANCE) handle)) {
	return  ERL_DE_DYNAMIC_ERROR_OFFSET - GetLastError();
    }
    return  ERL_DE_NO_ERROR;
}

/*
 * Return string that describes the (current) error
 */
#define MAX_ERROR 255
char *erts_sys_ddll_error(int code)
{
    int actual_code;
    char *local_ptr;
    if (code > ERL_DE_DYNAMIC_ERROR_OFFSET) {
	return "Unspecified error";
    }
    actual_code = -1*(code - ERL_DE_DYNAMIC_ERROR_OFFSET);

    local_ptr = TlsGetValue(tls_index);
    if (local_ptr == NULL) {
	local_ptr = erts_alloc(ERTS_ALC_T_DDLL_ERRCODES, MAX_ERROR);
	TlsSetValue(tls_index,local_ptr);
    }
    if (!FormatMessage(
		       FORMAT_MESSAGE_FROM_SYSTEM,
		       NULL,
		       (DWORD) actual_code,
		       MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		       local_ptr,
		       MAX_ERROR, NULL )) {
	return "Unspecified error";
    } else {
	char *ptr = local_ptr + strlen(local_ptr) - 1;
	while (ptr >= local_ptr && (*ptr == '\r' || *ptr == '\n')) {
	    *ptr-- = '\0';
	}
    }
    return  local_ptr;
}
