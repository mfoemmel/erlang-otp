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
 * Dynamic driver loader and linker
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "erl_sys_driver.h"
#include "global.h"
#include "erl_ddll.h"

#define LOAD_DRIVER   'l'
#define UNLOAD_DRIVER 'u'
#define GET_DRIVERS   'g'

/***********************************************************************
 * R e p l i e s
 *
 * The following replies are sent from this driver.
 *
 * Reply		Translated to on Erlang side
 * -----		----------------------------
 * [$o]			ok
 * [$o|List]		{ok, List}
 * [$e|Atom]		{error, Atom}
 * [$E|[Atom, 0, Message]]	{error, {Atom, Message}}
 * [$i|String]		"..."    (This is an item in a list.)
 *
 * List of strings are sent as multiple messages:
 *
 *	[$i|String]
 *	.
 *	.
 *	.
 *	[$i|String]
 *	[$o]
 *
 * This will be represented as {ok, [String1, ..., StringN]} in Erlang.
 ************************************************************************/
#define LOAD_FAILED   2

/*typedef FUNCTION(DriverEntry*, (*DRV_INITFN), (void*));*/

static ErlDrvData dyn_start(ErlDrvPort, char*, SysDriverOpts* opts);
static void dyn_stop(ErlDrvData);
static void handle_command(ErlDrvData, char*, int);
static void unload_all(void);

struct erl_drv_entry ddll_driver_entry = {
    NULL,			/* init */
    dyn_start,			/* start */
    dyn_stop,			/* stop */
    handle_command,		/* output */
    NULL,			/* ready_input */
    NULL,			/* ready_output */
    "ddll",			/* driver_name */
    NULL,			/* finish */
    NULL			/* handle */
};

static long erlang_port = -1;

#ifdef _OSE_
void reset_ddll_drv() {
  erlang_port = -1;
}
#endif

static long reply(long port, int success, char *str)
{
    char tmp[200];
    *tmp = success;
    strncpy(tmp + 1, str, 198);
    tmp[199] = 0; /* in case str was more than 198 bytes */
    driver_output(port, tmp, strlen(tmp));
    return 0;
}

static long
error_reply(char* atom, char* string)
{
    char* reply_str;
    int alen;			/* Length of atom. */
    int slen;			/* Length of string. */
    int rlen;			/* Length of reply. */

    alen = strlen(atom);
    slen = strlen(string);
    rlen = 1+alen+1+slen;
    reply_str = driver_alloc(rlen+1);
    reply_str[0] = 'E';
    strcpy(reply_str+1, atom);
    strcpy(reply_str+alen+2, string);
    driver_output(erlang_port, reply_str, rlen);
    driver_free(reply_str);
    return 0;
}

static ErlDrvData dyn_start(ErlDrvPort port, char *buf, SysDriverOpts* opts)
{
    if (erlang_port != -1) {
	return ERL_DRV_ERROR_GENERAL;	/* Already started! */
    }
    erlang_port = port;
    return (ErlDrvData)port;
}

static void dyn_stop(ErlDrvData d)
{
    erlang_port = -1;
    unload_all();
    return;
}


static int unload(void* arg1, void* arg2, void* dummy1, void* dummy2)
{
    DE_List* de = (DE_List*) arg1;
    int ix = (int) (long) arg2;
    DE_Handle* dh;
    int j;

    DEBUGF(("ddll_drv: unload: (%d) %s\r\n", ix, de->drv->driver_name));
    /*
     * Kill all ports that depend on this driver.
     */
    for (j = 0; j < erts_max_ports; j++) {
	if (erts_port[j].status != FREE &&
	    erts_port[j].drv_ptr == de->drv) {
	    driver_failure(j, -1);
	}
    }

    /* 
     * Let the driver clean up its mess before unloading it.
     * We ignore errors from the finish funtion and
     * ddll_close().
     */

    dh = de->de_hndl;
    
    if (de->drv->finish) {
	(*(de->drv->finish))();
    }
    DEBUGF(("ddll_drv: unload: lib=%08x\r\n", dh->handle));

    ddll_close(dh->handle);
    driver_free((void*)dh);
    remove_driver_entry(de->drv);

    return ix != -1  ?  reply(ix, 'o', "")  :  0;
}

static void unload_all()
{
    DE_List *de;
    DE_Handle* dh;
 
    do {
	for (de = driver_list; de != NULL; de = de->next) {
	    if ( (dh = de->de_hndl)) {
		unload((void *)de, (void *)erlang_port, NULL, NULL);
		break;
	    }
	}
    } while (de != NULL);
}

static int load(char* full_name, char* driver_name)
{
    ErlDrvEntry *dp;
    DE_Handle* dh;
    void *lib;
    ErlDrvEntry* (* initfn)(DE_Handle *);

    DEBUGF(("ddll_drv: load: %s, %s\r\n", full_name, driver_name));

    if ((lib = ddll_open(full_name)) == NULL) {
	return error_reply("open_error", ddll_error());
    }
    DEBUGF(("ddll_drv: load: lib=%08x\r\n", lib));
    /* Some systems still require an underscore at the beginning of the
     * symbol name. If we can't find the name without the underscore, try
     * again with it.
     */
	
    if ((initfn = ddll_sym(lib, "driver_init")) == NULL)
	if ((initfn = ddll_sym(lib, "_driver_init")) == NULL) {
	    ddll_close(lib);
	    return reply(erlang_port, 'e', "no_driver_init");
	}

    /* 
     * Here we go, lets hope the driver write knew what he was doing...
     */
    dh = driver_alloc(sizeof(DE_Handle));
    dh->handle = lib;
    dh->ref_count = 1;
    dh->status = ERL_DE_OK;
    dh->cb = NULL;
    dh->ca[0] = NULL;

#ifdef __WIN32__
    dp = initfn((DE_Handle *) win_get_ddll_init_param());
#else
    dp = initfn(dh);
#endif
    if (dp == NULL) {
	ddll_close(lib);
	driver_free(dh);
	return reply(erlang_port, 'e', "driver_init_failed");
    }
    
    if (strcmp(driver_name, dp->driver_name) != 0) {
	ddll_close(lib);
	driver_free(dh);
	return reply(erlang_port, 'e', "bad_driver_name");
    }

    dp->handle = dh;

    /* insert driver */
    add_driver_entry(dp);

    return reply(erlang_port, 'o', "");
}


/* static reload callback */
static int reload(void* arg1, void* arg2, void* arg3, void* arg4)
{
    char* full_name = (char*) arg1;
    char* driver_name = (char*) arg2;
    int code;

    DEBUGF(("ddll_drv: reload: %s, %s\r\n", full_name, driver_name));

    unload(arg3, arg4, NULL, NULL);
    code = load(full_name, driver_name);
    driver_free(full_name);
    driver_free(driver_name);
    return code;
}


static void handle_command(ErlDrvData inport, char *buf, int count)
{
    char *driver_name;
    
    /*
     * Do some sanity checking on the commands passed in buf. Make sure the
     * last string is null terminated, and that buf isn't empty.
     */
    
    if (count == 0 || (count > 1 && buf[count-1] != 0)) {
	driver_failure(erlang_port, 100);
	return;
    }
  
    switch (*buf++) {
    case LOAD_DRIVER:
	{
	    char* full_name;
	    DE_List *de;
	    DE_Handle* dh;
	    int j;

	    if (count < 5) {
		driver_failure(erlang_port, 110);
		return;
	    }

	    driver_name = buf;
	    j = strlen(driver_name);
	    if (j == 0 || j+3 >= count) {
		driver_failure(erlang_port, 111);
		return;
	    }

	    full_name = buf+j+1;

	    for (de = driver_list; de != NULL; de = de->next) {
		if (strcmp(de->drv->driver_name, driver_name) == 0) {
		    dh = de->de_hndl;
		    if (dh != NULL) {
			if (dh->status == ERL_DE_UNLOAD) {
			    dh->status = ERL_DE_RELOAD;
			    dh->cb = reload;
			    dh->ca[2] = dh->ca[0];
			    dh->ca[3] = dh->ca[1];
			    dh->ca[0] = driver_alloc(strlen(full_name)+1);
			    dh->ca[1] = driver_alloc(strlen(driver_name)+1);
			    strcpy((char*)dh->ca[0], full_name);
			    strcpy((char*)dh->ca[1], driver_name);
			    return;
			}
			else if (dh->status == ERL_DE_RELOAD) {
			    driver_free(dh->ca[0]);
			    driver_free(dh->ca[1]);
			    dh->ca[0] = driver_alloc(strlen(full_name)+1);
			    dh->ca[1] = driver_alloc(strlen(driver_name)+1);
			    strcpy((char*)dh->ca[0], full_name);
			    strcpy((char*)dh->ca[1], driver_name);
			    return;
			}
		    }
		    reply(erlang_port, 'e', "already_loaded");
		    return;
		}
	    }
	    load(full_name, driver_name);	    
	    return;
	}

    case UNLOAD_DRIVER:
	{
	    DE_List *de;
	    DE_Handle* dh;

	    if (count < 3) {
		driver_failure(erlang_port, 200);
		return;
	    }
	    driver_name = buf;

	    DEBUGF(("ddll_drv: UNLOAD: %s\r\n", driver_name));

	    for (de = driver_list; de != NULL; de = de->next) {
		if (strcmp(de->drv->driver_name, driver_name) == 0) {
		    dh = de->de_hndl;
		    if (dh == NULL) {
			reply(erlang_port, 'e', "linked_in_driver");
			return;
		    }
		    dh->ref_count--;
		    if (dh->ref_count > 0) {
			DEBUGF(("ddll_drv: UNLOAD: ref_count =%d\r\n",
				dh->ref_count));
			if (dh->status == ERL_DE_OK) {
			    dh->status = ERL_DE_UNLOAD;
			    dh->cb = unload;
			    dh->ca[0] = (void*) de;
			    dh->ca[1] = (void*) erlang_port;
			    return;  /* delayed op */
			}
			else if (dh->status == ERL_DE_RELOAD) {
			    dh->status = ERL_DE_UNLOAD;
			    dh->cb = unload;
			    driver_free(dh->ca[0]);
			    driver_free(dh->ca[1]);
			    dh->ca[0] = (void*) de;
			    dh->ca[1] = (void*) erlang_port;
			    return;
			}
			else if (dh->status == ERL_DE_UNLOAD)
			    return;
		    }
		    else {
			unload((void*) de, (void*) erlang_port, 
			       NULL, NULL);
			return;
		    }
		}
	    }
	    reply(erlang_port, 'e', "not_loaded");
	    return;
	}
    
    case GET_DRIVERS:
	{
	    DE_List *pos;

	    for (pos = driver_list; pos != NULL; pos = pos->next) {
		reply(erlang_port, 'i', pos->drv->driver_name);
	    }
	    reply(erlang_port, 'o', "");
	    return;
	}
	
    default:
	driver_failure(erlang_port, 999);
    }
    return;
}
