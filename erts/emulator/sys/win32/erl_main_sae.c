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
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "sys.h"
#include "erl_api.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#ifndef __WIN32__
#error "Win32 version only!"
#endif

#include <Windows.h>

enum {
    RES_ENV = 0, RES_RING0, RES_CODE, N_RES
};

int nohup = 0;
int keep_window = 0;

/* We're compiling for a tool, just call the exported API */

static void res_put_env(char* envs)
{
    const char* seps = "\n\r";
    char* token = strtok(envs, seps);
    while (token != NULL) {
	putenv(token);
	token = strtok(NULL, seps);
    }
}

int main(int argc, char** argv)
{
    HMODULE hModule = 0;
    HRSRC hRes;
    const char* res_names[N_RES] = { "ERLANG_ENV", "ERLANG_RING0", "ERLANG_BUNDLE" };
    char* res_data[N_RES];
    long res_size[N_RES];
    int i;

    for (i = 0; i < N_RES; ++i) {	/* load resources */
        hRes = FindResource(hModule, MAKEINTRESOURCE(1), res_names[i]);
	if (i > 0 && hRes == NULL) 
	    exit(98);
	res_data[i] = (char *) LoadResource(hModule, hRes); 
	if (i > 0 && res_data[i] == NULL) 
	    exit(99);
	res_size[i] = SizeofResource(hModule, hRes);
    }
    if (res_data[RES_ENV] != NULL)
	res_put_env(res_data[RES_ENV]);
    ErlInit();
    ErlLoadModule("ring0", res_data[RES_RING0], res_size[RES_RING0]);
    ErlCreateInitialProcess("ring0", res_data[RES_CODE], res_size[RES_CODE], argc, argv);
    ErlScheduleLoop();
}
