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
/* Purpose: Low level driver for dynamic driver loader/unloader and
 * linker for VxWorks.
 */
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include <vxWorks.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <a_out.h>
#include <symLib.h>
#include <loadLib.h>
#include <unldLib.h>
#include <moduleLib.h>
#include <sysSymTbl.h>
#include "sys.h"
#include "erl_vm.h"
#include "erl_ddll.h"

static char thismodname[] = "vxworks_ddll_drv";
static char *curr_error = "No error";

static MODULE_ID get_mid(char *);
static FUNCPTR lookup(char *);
static void drv_error(char*, ...);

/*
 * ddll_open(full_name)
 *
 * full_name is the path to the file containing the driver.
 * The basename of full_name is the driver name.
 * 
 * Returns the MODULE_ID of the module containing the driver.
 *
 */
void *ddll_open(char *full_name)
{
    char *path;
    char *drvname;
    char *cp;
    MODULE_ID mid;
    int len;

    if ((mid = get_mid(full_name)) == NULL) {
	if((path = malloc(strlen(full_name)+5)) == NULL) {
	    curr_error = "ddll_open: Cannot malloc";
	    return NULL;
	}
	strcpy(path, full_name);
	len = strlen(path);
	strcpy(path + len, ".eld");
	if(((mid = get_mid(path)) == NULL) &&
	   strcpy(path + len, ".o") &&
	   ((mid = get_mid(path)) == NULL)) {
	    curr_error = "ddll_open: Module not found";
	}
	free(path);
    }
    return (void *) mid;
}

static MODULE_ID
get_mid(char* name) 
{ 
    int fd;
    MODULE_ID mid = NULL;
    
    if((fd = open(name, O_RDONLY, 0664)) >= 0) {
	mid = loadModule(fd, GLOBAL_SYMBOLS);
	close(fd);
    }
    return mid;
}

/*
 * Return pointer to function MODULENAME_init.
 *
 * n.b. func_name is ignored.
 *
 * The convention is that the module name of the driver is .eld (e.g. echo_drv.eld).
 * But if it has .o as suffix or no suffix it is also ok (e.g. echo_drv.o and echo_drv)
 *
 */
uint32 *ddll_sym(void *handle, char *func_name)
{
    MODULE_ID mid = (MODULE_ID) handle;
    char *modname;
    char *cp;
    char *fname;
    char *suffix = "_init";
    int len;
    FUNCPTR func;
    
    if((modname = moduleNameGet(mid)) == NULL) {
	curr_error = "ddll_sym: Module not found";
	return 0;
    }
    if((cp = strrchr(modname, '.')) == NULL) {
	len = strlen(modname);
    } else {
	len = cp - modname;
    }
    if((fname = malloc(len + strlen(suffix) + 1)) == NULL) {
	curr_error = "ddll_sym: cannot malloc";
	return 0;
    }
    strncpy(fname, modname, len);
    fname[len] = '\0';
    strcat(fname, suffix);
    if((func = lookup(fname)) == NULL)
	curr_error = "ddll_sym: function NAME_init not found";
    free(fname);
    return (uint32 *) func;
}

int ddll_close(void *handle)
{
    MODULE_ID mid = (MODULE_ID) handle;
    int ret;
    
    if((ret = unld(mid, 0)) < 0)
	curr_error = "ddll_close: Module not found";
    return ret;
}

char *ddll_error(void)
{
    return curr_error;
}

/*
 * Lookup and return global text symbol or NULL on failure
 *
 * ddll_drv.c looks for sym with and without a leading '_'.
 * (this is needed since gcc for PPC doesn't include the
 * leading '_' while gcc for e.g. 68k does so)
 */
static FUNCPTR lookup(char *sym)
{
    FUNCPTR entry;
    SYM_TYPE type;
    char buf[256];
    char *u_score_sym = buf;
    int len;
    
    if (symFindByNameAndType(sysSymTbl, sym, (char **)&entry,
			     &type, N_EXT | N_TEXT, N_EXT | N_TEXT) != OK) {
	len = strlen(sym);
	if (len > 254 && (u_score_sym = malloc(len+2)) == NULL)
	    return(NULL);
	sprintf(u_score_sym, "_%s", sym);
	if (symFindByNameAndType(sysSymTbl, u_score_sym, (char **)&entry,
				 &type, N_EXT | N_TEXT, N_EXT | N_TEXT) != OK)
	    entry = NULL;
	if (u_score_sym != buf)
	    free(u_score_sym);
    }
    return(entry);
}

static void drv_error(char* format, ...)
{
    va_list va;

    fprintf(stdout, "%s: ", thismodname);
    va_start(va, format);
    vfprintf(stdout, format, va);
    va_end(va);
}

