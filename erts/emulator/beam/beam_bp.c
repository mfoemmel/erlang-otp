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
#include "erl_process.h"
#include "beam_load.h"
#include "hash.h"
#include "bif.h"
#include "error.h"
#include "beam_bp.h"


/*
** Macros
*/

/*
** Memory allocation macros
*/
/* 220 == Breakpoint data (for instrumentation) */
#define Alloc(SIZ) sys_alloc_from(220, (SIZ))
#define ReAlloc(OLD,SIZ) sys_realloc(OLD,SIZ)
#define Free(OLD) sys_free(OLD)

/*
** Initial size of breakpoint table
*/
#define BP_INITIAL_SIZE 10

/*
** Global variables
*/

/*
** Breakpoint hash table (accessed via macros in beam_bp.h
*/
Hash erts_bp_table;

/*
** Local prototypes
*/

/*
** Helpers
*/
static int do_erts_set_break(Module *m, Eterm mfa[3], int specified,
			     Binary *match_spec, Uint break_op);
static int do_erts_clear_break(Module *m, Eterm mfa[3],
			       int specified, Uint break_op);

/*
** Table access functions
*/
static int erts_set_break(Eterm mfa[3], int specified,
		   Binary *match_spec, Uint break_op);
static int erts_clear_break(Eterm mfa[3], int specified, Uint break_op);
static TraceBpData *erts_get_bp_data(Uint *pc);
static void erts_put_bp_data(TraceBpData *bpd);
static void erts_remove_bp_data(Uint *pc);

/*
** Callbacks for hash.c
*/
static HashValue bp_hash(void *);
static int bp_cmp(void *, void *);
static void *bp_alloc(void *);
static void bp_free(void *);

/*
** Static globals
*/

/*
** Info struct for hash.c
*/
static HashFunctions bp_hf = {
    &bp_hash,
    &bp_cmp,
    &bp_alloc,
    &bp_free
};

/*
** External interfaces
*/

void erts_bp_init(void) 
{
    hash_init(&erts_bp_table, "breakpoint_data", BP_INITIAL_SIZE, bp_hf);
}

int 
erts_set_trace_break(Eterm mfa[3], int specified, Binary *match_spec)
{
    return erts_set_break(mfa, specified, match_spec,
			  (Uint) BeamOp(op_i_trace_breakpoint));
}

int 
erts_set_debug_break(Eterm mfa[3], int specified)
{
    return erts_set_break(mfa, specified, NULL, (Uint) BeamOp(op_i_debug_breakpoint));
}

static int 
erts_set_break(Eterm mfa[3], int specified, Binary *match_spec, Eterm break_op)
{
    Module *modp;
    int num_processed = 0;
    if (!specified) {
	/* Find and process all modules in the system... */
	int current = -1;
	while ((current = index_iter(&module_table,current)) >= 0) {
	    if ((modp = module_code(current)) != NULL) {
		num_processed += do_erts_set_break(modp, mfa, 
						   specified, match_spec, break_op);
	    }
	}
    } else {
	/* Process a single module */
	if ((modp = erts_get_module(mfa[0])) != NULL) {
	    num_processed += do_erts_set_break(modp, mfa, 
					       specified, match_spec, break_op);
	}	
    }
    return num_processed;
}
int
erts_clear_trace_break(Eterm mfa[3], int specified)
{
    return erts_clear_break(mfa, specified, (Uint) BeamOp(op_i_trace_breakpoint));
}

int
erts_clear_debug_break(Eterm mfa[3], int specified)
{
    return erts_clear_break(mfa, specified, (Uint) BeamOp(op_i_debug_breakpoint));
}

static int
erts_clear_break(Eterm mfa[3], int specified, Uint break_op) 
{
    int num_processed = 0;
    Module *modp;
    if (!specified) {
	int current = -1;
	while ((current = index_iter(&module_table,current)) >= 0) {
	    if ((modp = module_code(current)) != NULL) {
		num_processed += do_erts_clear_break(modp, mfa, specified, break_op);
	    }
	}
    } else {
	/* Process a single module */
	if ((modp = erts_get_module(mfa[0])) != NULL) {
	    num_processed += do_erts_clear_break(modp, mfa, specified, break_op);
	}	
    }
    return num_processed;
}

void erts_clear_module_break(Module *modp)
{
    (void) do_erts_clear_break(modp, NULL, 0, 0);
}

Uint erts_process_break(Process *p, Uint *pc, Eterm *mfa, 
			Eterm *args, Uint32 *ret_flags)
{
    TraceBpData *bpd = erts_get_bp_data(pc);
    ASSERT(bpd != NULL);

    *ret_flags = erts_call_trace(p, mfa, bpd->match_spec, args, 1);
    return bpd->orig_instr;
}


Uint *erts_find_local_func(Eterm mfa[3])
{
    Module *modp;
    Uint** code_base;
    Uint* code_ptr;
    int i,n;

    if ((modp = erts_get_module(mfa[0])) == NULL)
	return NULL;
    
    code_base = (Uint **) modp->code;
    n = (int) code_base[MI_NUM_FUNCTIONS];
    for (i = 0; i < n; ++i) {
	code_ptr = code_base[MI_FUNCTIONS+i];
	if (mfa[1] == ((Eterm) code_ptr[3]) &&
	    ((int) mfa[2]) == ((int) code_ptr[4])) {
	    return code_ptr + 5;
	}
    }
    return NULL;
}

int erts_is_local_tracepoint(Uint *pc, Binary **match_spec_ret)
{
    if (*pc != (Uint) BeamOp(op_i_trace_breakpoint)) {
	return 0;
    }
    if (match_spec_ret != NULL) {
	TraceBpData *bpd = erts_get_bp_data(pc);
	ASSERT(bpd != NULL);
	*match_spec_ret = bpd->match_spec;
    }
    return 1;
}

/*
** Local helpers
*/
static int do_erts_set_break(Module *m, Eterm mfa[3], int specified,
			    Binary *match_spec, Uint break_op) 
{
    Uint** code_base;
    Uint* code_ptr;
    int num_processed = 0;
    int i,n;
    TraceBpData bpd;


    bpd.match_spec = match_spec;
    code_base = (Uint **) m->code;
    if (code_base == NULL) {
	return 0;
    }
    n = (int) code_base[MI_NUM_FUNCTIONS];
    for (i = 0; i < n; ++i) {
	code_ptr = code_base[MI_FUNCTIONS+i];
	if ((specified < 2 || mfa[1] == ((Eterm) code_ptr[3])) &&
	    (specified < 3 || ((int) mfa[2]) == ((int) code_ptr[4]))) {
#ifdef HIPE
	    /*
	     * Currently no trace support for native code.
	     */
	    if (code_ptr[1] != 0) {
		continue;
	    }
#endif

	    bpd.position = code_ptr + 5;
	    /* Insert only if it's not already a breakpoint */
	    if (*(bpd.position) != break_op) {
		bpd.orig_instr = *bpd.position;
		*(bpd.position) = break_op;
		erts_put_bp_data(&bpd);
		++(code_base[MI_NUM_BREAKPOINTS]);
	    } else {
		/* Only update the match_spec. */
		TraceBpData *b = erts_get_bp_data(bpd.position);
		MatchSetUnref(b->match_spec);
		MatchSetRef(match_spec);
		b->match_spec = match_spec;
	    }
	    ++num_processed;
	}
    }
    return num_processed;
}

static int 
do_erts_clear_break(Module *m, Eterm mfa[3], int specified, Uint break_op) 
{
    Uint** code_base;
    Uint* code_ptr;
    int num_processed = 0;
    int i,n;
    TraceBpData *bp;
    Uint *pc;


    code_base = (Uint **) m->code;
    if (code_base == NULL) {
	return 0;
    }
    n = (int) code_base[MI_NUM_FUNCTIONS];
    for (i = 0; i < n; ++i) {
	code_ptr = code_base[MI_FUNCTIONS+i];
	if ((specified < 2 || mfa[1] == ((Eterm) code_ptr[3])) &&
	    (specified < 3 || ((int) mfa[2]) == ((int) code_ptr[4]))) {
	    pc = code_ptr + 5;
	    if ((break_op == 0 || *pc == break_op) &&
		(bp = erts_get_bp_data(pc)) != NULL) {
		*pc = bp->orig_instr;
		erts_remove_bp_data(pc);
		ASSERT(code_base[MI_NUM_BREAKPOINTS] > 0);
		--(code_base[MI_NUM_BREAKPOINTS]);
	    }
	    ++num_processed;
	}
    }
    return num_processed;
}

/*
** Access functions for breakpoint table
*/
static TraceBpData *erts_get_bp_data(Uint *pc)
{
    TraceBpBucket *x;
    TraceBpLookupBucket(pc, x);
    return (x) ? &(x->bp_data) : NULL;
}

static void erts_put_bp_data(TraceBpData *bpd)
{
    TraceBpBucket tmpl;
    tmpl.bp_data = *bpd;
    hash_put(&erts_bp_table,&tmpl);
}

static void erts_remove_bp_data(Uint *pc)
{
    TraceBpBucket tmpl;
    tmpl.bp_data.position = pc;
    hash_erase(&erts_bp_table,&tmpl);
}


/*
** Functions for the hashing module
*/
static HashValue bp_hash(void *obj)
{
    return TraceBpHash(((TraceBpBucket *) obj)->bp_data.position);
}

static int bp_cmp(void *a, void *b) 
{
    return !TraceBpBucketEqual(a,b);
}

static void *bp_alloc(void *orig)
{
    TraceBpBucket *n = Alloc(sizeof(TraceBpBucket));
    n->bp_data = ((TraceBpBucket *) orig)->bp_data;
    MatchSetRef(n->bp_data.match_spec);
    return (void *) n;
}

static void bp_free(void *obj)
{
    MatchSetUnref(((TraceBpBucket *)obj)->bp_data.match_spec);
    Free(obj);
}

