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

extern unsigned preloaded_size_ring0;
extern unsigned char preloaded_ring0[];

int
main(int argc, char** argv)
{
    char sbuf[1024];
    FILE* fp;
    char* progname = argv[0];

    argv++, argc--;

    if (argc > 0 && argv[0][0] == '-') {
	argv++, argc--;
    }
    if (argc < 1) {
	abort();
    }
    if ((fp = fopen(argv[0], "r")) == NULL) {
	abort();
    }
    argv++, argc--;
    
    while (fgets(sbuf, sizeof sbuf, fp)) {
	if (sbuf[0] == '#') {
	    continue;		/* Comment */
	} else if (sbuf[0] == 'e' && strncmp("exec ", sbuf, 4)) {
	    continue;		/* Comment ;-) */
	} else if (strchr(sbuf, '=')) {
	    char* p = strchr(sbuf, '\n');
	    if (p) {
		*p = '\0';
	    }
	    sys_putenv(sbuf);
	} else if (sbuf[0] == ':') {
	    int load_size = atoi(sbuf+1);
	    void* bin;
	    char** new_argv;
	    int i;

	    if (load_size == 0) {
		abort();
	    }
	    bin = malloc(load_size);
	    if (fread(bin, 1, load_size, fp) != load_size) {
		abort();
	    }
	    if (fgets(sbuf, sizeof sbuf, fp) == NULL) {
		abort();
	    }
	    if (strcmp(sbuf, "--end--\n") != 0) {
		abort();
	    }
	    fclose(fp);
	    new_argv = (char **) malloc((argc+1)*sizeof(char*));
	    new_argv[0] = progname;
	    for (i = 0; i < argc; i++) {
		new_argv[i+1] = argv[i];
	    }

	    ErlInit();
	    ErlLoadModule("ring0", preloaded_ring0, preloaded_size_ring0);
	    ErlCreateInitialProcess("ring0", bin, load_size, argc+1, new_argv);
	    free(new_argv);
	    free(bin);
	    ErlScheduleLoop();
	}
    }
    abort();
}
