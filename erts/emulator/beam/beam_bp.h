#ifndef _BEAM_BP_H
#define _BEAM_BP_H
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

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "hash.h"

/*
** This is a trace breakpoint, no flags or anything is needed.
*/

typedef struct trace_bp_data {
    Uint *position;         /* Pointer into code where the Breakpoint
				Instruction is. (PC) */
    Uint orig_instr;        /* The original instruction to execute */
    Binary *match_spec;
} TraceBpData;

/*
** The bucket used by hash.c
*/
typedef struct {
    HashBucket bucket;
    TraceBpData bp_data;
} TraceBpBucket;

/*
** The breakpoint hash table
*/
extern Hash erts_bp_table;

/* 
** Hash lookup inlined. If ever hash.c is changed, this will break... 
*/
#define TraceBpHash(BpPosition) (((HashValue) (BpPosition)) >> (sizeof(void *)/2))

#define TraceBpBucketEqualToPc(A,PC)				\
    (((Uint) ((TraceBpBucket *) (A))->bp_data.position) == 	\
     ((Uint) (PC)))

#define TraceBpBucketEqual(A,B)                                         \
     TraceBpBucketEqualToPc(A,((TraceBpBucket *) (B))->bp_data.position)

#define TraceBpLookupBucket(PC,BucketResult)				\
do {                                                                    \
    HashValue hval = TraceBpHash((PC));					\
    HashBucket* b = erts_bp_table.bucket[hval % erts_bp_table.size];	\
    while (b != NULL) {							\
	if ((b->hvalue == hval) && TraceBpBucketEqualToPc(b,(PC)))	\
	    break;							\
	b = b->next;							\
    }	                                                                \
    (BucketResult) = (TraceBpBucket *) b;				\
} while (0)

#define TraceBpLookupInstr(PC,InstrResult)	\
do {						\
    TraceBpBucket *x;				\
    TraceBpLookupBucket((PC),x);		\
    ASSERT(x != NULL);				\
    (InstrResult) = (x->bp_data).orig_instr;	\
} while(0)


/*
** Function interface exported from beam_bp.c
*/
void erts_bp_init(void);
int erts_set_break(Eterm mfa[3], int specified, Binary *match_spec);
int erts_clear_break(Eterm mfa[3], int specified);
void erts_clear_module_break(Module *modp);
Uint erts_process_break(Process *p, Uint *pc, Eterm *mfa, 
			Eterm *args, Uint32 *ret_flags);
int erts_is_local_tracepoint(Uint *pc, Binary **match_spec_ret);
Uint *erts_find_local_func(Eterm mfa[3]);

#endif /* _BEAM_BP_H */
