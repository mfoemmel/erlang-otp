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
#include "erl_vm.h"
#include "global.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/*
 * XXX This is a temporary dummy to make sys.c happy until we'll rewrite it.
 */
unsigned preloaded_size_ring0 = 1;
unsigned char preloaded_ring0[1] = {0};

struct {
    char* name;
    int size;
    unsigned char* code;
} pre_loaded[] = {
    {"ring0", 1, preloaded_ring0},
    {0, 0, 0}
};

int
main(int argc, char** argv)
{
    char sbuf[1024];
    struct {
	void* p;
	int sz;
    } bins[2];
    int bin_num = 0;
    FILE* fp;
    char* progname = argv[0];
    char* eq;

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
    
    while (fgets(sbuf, sizeof sbuf, fp)) {
	if (sbuf[0] == '#') {
	    continue;		/* Comment */
	} else if (sbuf[0] == 'e' && strncmp("exec", sbuf, 4) == 0) {
	    continue;		/* Comment ;-) */
	} else if ((eq = strchr(sbuf, '=')) != NULL) {
	    char* p = strchr(sbuf, '\n');
	    if (p) {
		*p = '\0';
	    }
	    *eq = '\0';
	    if (getenv(sbuf) == NULL) {
		*eq = '=';
		sys_putenv(sbuf);
	    }
	} else if (sbuf[0] == ':' && '0' <= sbuf[1] && sbuf[1] <= '9') {
	    int load_size = atoi(sbuf+1);
	    void* bin;
	    
	    bin = malloc(load_size);
	    if (fread(bin, 1, load_size, fp) != load_size) {
		abort();
	    }
	    bins[bin_num].p = bin;
	    bins[bin_num].sz = load_size;
	    bin_num++;
	} else if (strcmp(sbuf, "--end--\n") == 0) {
	    int rval;
	    Eterm mod = NIL;

	    fclose(fp);

	    if (bin_num != 2) {
		abort();
	    }

	    erts_short_init();
 	    if (getenv("ERLBREAKHANDLER")) {
		init_break_handler();
	    }

	    if ((rval = erts_load_module(NIL, &mod, bins[0].p, bins[0].sz)) < 0) {
		fprintf(stderr, "%s: Load of initial module failed: %d\n",
			progname, rval);
		abort();
	    }
	    erts_first_process(mod, bins[1].p, bins[1].sz, argc, argv);
	    free(bins[0].p);
	    free(bins[1].p);
	    process_main();
	    abort();
	} else {
	    fprintf(stderr, "%s: bad line: %s\n", progname, sbuf);
	    abort();
	}
    }
    abort();
}
