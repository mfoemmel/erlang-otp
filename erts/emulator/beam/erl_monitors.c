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

/**************************************************************************
 * Monitors and links data structure manipulation.
 * Monitors and links are organized as AVL trees with the reference as 
 * key in the monitor case and the pid of the linked process as key in the 
 * link case. Lookups the order of the references is somewhat special. Local 
 * references are strictly smaller than remote references and are sorted 
 * by inlined comparision functionality. Remote references are handled by the
 * usual cmp function.
 * Each Monitor is tagged with  diferent tags depending on which end of the 
 * monitor it is.
 * A monitor is removed either explicitly by reference or all monitors are 
 * removed when the process exits. No need to access the monitor by pid.
 * [...] More comments to follow when I know how I implemented it :-)
 **************************************************************************/ 

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "erl_db.h"
#include "bif.h"
#include "big.h"
#include "erl_monitors.h"

#define STACK_NEED 50
#define MAX_MONITORS 0xFFFFFFFFUL

#define DIR_LEFT 0
#define DIR_RIGHT 1
#define DIR_END 2 
Sint erts_tot_nlink_lh_size;
Sint erts_tot_monitor_lh_size;

/* Implements the sort order in monitor trees, which is different from 
   the ordinary term order.
   No short local ref's should ever exist (the ref is created by the bif's 
   in runtime), therefore:
   All local ref's are less than external ref's
   Local ref's are inline-compared,
   External ref's are compared by cmp */

#if 0   
#define CMP_MON_REF(Ref1,Ref2) \
cmp((Ref1),(Ref2)) /* XXX, the inline comparision yet to be done */
#else
#define CMP_MON_REF(Ref1,Ref2) cmp_mon_ref((Ref1),(Ref2))
#endif

static ERTS_INLINE int cmp_mon_ref(Eterm ref1, Eterm ref2) 
{
    Eterm *b1, *b2;


    b1 = boxed_val(ref1);
    b2 = boxed_val(ref2);
    if (is_ref_thing_header(*b1)) {
	if (is_ref_thing_header(*b2)) {
	    return memcmp(b1+1,b2+1,ERTS_REF_WORDS*sizeof(Uint));
	}
	return -1;
    }
    if (is_ref_thing_header(*b2)) {
	return 1;
    }
    return cmp(ref1,ref2);
}
	    
#define CP_LINK_VAL(To, Hp, From)				\
do {								\
    if (IS_CONST(From))						\
	(To) = (From);						\
    else {							\
	Uint i__;						\
	Uint len__;						\
	ASSERT((Hp));						\
	ASSERT(is_internal_ref((From)) || is_external((From)));	\
	(To) = make_boxed((Hp));				\
	len__ = thing_arityval(*boxed_val((From))) + 1;		\
	for(i__ = 0; i__ < len__; i__++)			\
	    (*((Hp)++)) = boxed_val((From))[i__];		\
	if (is_external((To))) {				\
	    external_thing_ptr((To))->next = NULL;		\
	    external_thing_ptr((To))->node->refc++;		\
	}							\
    }								\
} while (0)

static ErtsMonitor *create_monitor(Uint type, Eterm ref, Eterm pid, Eterm name)
{
     Uint mon_size = ERTS_MONITOR_SIZE;
     ErtsMonitor *n;
     Eterm *hp;

     mon_size += NC_HEAP_SIZE(ref);
     if (!IS_CONST(pid)) {
	 mon_size += NC_HEAP_SIZE(pid);
     }

     if (mon_size <= ERTS_MONITOR_SH_SIZE) {
	 n = (ErtsMonitor *) erts_alloc(ERTS_ALC_T_MONITOR_SH,
					mon_size*sizeof(Uint));
     } else {
	 n = (ErtsMonitor *) erts_alloc(ERTS_ALC_T_MONITOR_LH,
					mon_size*sizeof(Uint));
	 erts_tot_monitor_lh_size += 	mon_size*sizeof(Uint);
     } 
     hp = n->heap;


     n->left = n->right = NULL; /* Always the same initial value*/
     n->type = (Uint16) type;
     n->balance = 0;            /* Always the same initial value */
     n->name = name; /* atom() or [] */
     CP_LINK_VAL(n->ref, hp, ref); /*XXX Unneccesary check, never immediate*/
     CP_LINK_VAL(n->pid, hp, pid);
     
     ERTS_PROC_MORE_MEM(mon_size);

     return n;
}

static ErtsLink *create_link(Uint type, Eterm pid)
{
     Uint lnk_size = ERTS_LINK_SIZE;
     ErtsLink *n;
     Eterm *hp;

     if (!IS_CONST(pid)) {
	 lnk_size += NC_HEAP_SIZE(pid);
     }

     if (lnk_size <= ERTS_LINK_SH_SIZE) {
	 n = (ErtsLink *) erts_alloc(ERTS_ALC_T_NLINK_SH,
					lnk_size*sizeof(Uint));
     } else {
	 n = (ErtsLink *) erts_alloc(ERTS_ALC_T_NLINK_LH,
					lnk_size*sizeof(Uint));
	 erts_tot_nlink_lh_size += 	lnk_size*sizeof(Uint);
     } 
     hp = n->heap;


     n->left = n->right = NULL; /* Always the same initial value*/
     n->type = (Uint16) type;
     n->balance = 0;            /* Always the same initial value */
     if (n->type == LINK_NODE) {
	 ERTS_LINK_ROOT_AS_UINT(n) = 0;
     } else {
	 n->root = NULL; 
     }
     CP_LINK_VAL(n->pid, hp, pid);
     
     ERTS_PROC_MORE_MEM(lnk_size);

     return n;
}

#undef CP_LINK_VAL

void erts_destroy_monitor(ErtsMonitor *mon)
{
    Uint mon_size = ERTS_MONITOR_SIZE;
    ErlNode *node;

    ASSERT(!IS_CONST(mon->ref));
    mon_size +=  NC_HEAP_SIZE(mon->ref);
    if (is_external(mon->ref)) {
	node = external_thing_ptr(mon->ref)->node;
	DEREF_ERL_NODE(node);
    }
    if (!IS_CONST(mon->pid)) {
	mon_size += NC_HEAP_SIZE(mon->pid);
	if (is_external(mon->pid)) {
	    node = external_thing_ptr(mon->pid)->node;
	    DEREF_ERL_NODE(node);
	}
    }
    if (mon_size <= ERTS_MONITOR_SH_SIZE) {
	erts_free(ERTS_ALC_T_MONITOR_SH, (void *) mon);
    } else {
	erts_free(ERTS_ALC_T_MONITOR_LH, (void *) mon);
	erts_tot_monitor_lh_size -= mon_size*sizeof(Uint);

    }
    ERTS_PROC_LESS_MEM(mon_size);
}
    
void erts_destroy_link(ErtsLink *lnk)
{
    Uint lnk_size = ERTS_LINK_SIZE;
    ErlNode *node;

    ASSERT(lnk->type == LINK_NODE || lnk->root == NULL);

    if (!IS_CONST(lnk->pid)) {
	lnk_size += NC_HEAP_SIZE(lnk->pid);
	if (is_external(lnk->pid)) {
	    node = external_thing_ptr(lnk->pid)->node;
	    DEREF_ERL_NODE(node);
	}
    }
    if (lnk_size <= ERTS_LINK_SH_SIZE) {
	erts_free(ERTS_ALC_T_NLINK_SH, (void *) lnk);
    } else {
	erts_free(ERTS_ALC_T_NLINK_LH, (void *) lnk);
	erts_tot_nlink_lh_size -= lnk_size*sizeof(Uint);
    }
    ERTS_PROC_LESS_MEM(lnk_size);
}
    
     
static void insertion_rotation(int dstack[], int dpos, 
			       void *tstack[], int tpos, 
			       int state) {
    
    ErtsMonitorOrLink **this;
    ErtsMonitorOrLink *p1, *p2, *p;
    int dir;

    while (state && ( dir = dstack[--dpos] ) != DIR_END) {
	this = tstack[--tpos];
	p = *this;
	if (dir == DIR_LEFT) {
	    switch (p->balance) {
	    case 1:
		p->balance = 0;
		state = 0;
		break;
	    case 0:
		p->balance = -1;
		break;
	    case -1: /* The icky case */
		p1 = p->left;
		if (p1->balance == -1) { /* Single LL rotation */
		    p->left = p1->right;
		    p1->right = p;
		    p->balance = 0;
		    (*this) = p1;
		} else { /* Double RR rotation */
		    p2 = p1->right;
		    p1->right = p2->left;
		    p2->left = p1;
		    p->left = p2->right;
		    p2->right = p;
		    p->balance = (p2->balance == -1) ? +1 : 0;
		    p1->balance = (p2->balance == 1) ? -1 : 0;
		    (*this) = p2;
		}
		(*this)->balance = 0;
		state = 0;
		break;
	    }
	} else { /* dir == DIR_RIGHT */
	    switch (p->balance) {
	    case -1:
		p->balance = 0;
		state = 0;
		break;
	    case 0:
		p->balance = 1;
		break;
	    case 1:
		p1 = p->right;
		if (p1->balance == 1) { /* Single RR rotation */
		    p->right = p1->left;
		    p1->left = p;
		    p->balance = 0;
		    (*this) = p1;
		} else { /* Double RL rotation */
		    p2 = p1->left;
		    p1->left = p2->right;
		    p2->right = p1;
		    p->right = p2->left;
		    p2->left = p;
		    p->balance = (p2->balance == 1) ? -1 : 0;
		    p1->balance = (p2->balance == -1) ? 1 : 0;
		    (*this) = p2;
		}
		(*this)->balance = 0; 
		state = 0;
		break;
	    }
	}
    }
}

void erts_add_monitor(ErtsMonitor **root, Uint type, Eterm ref, Eterm pid, 
		      Eterm name)
{
    void *tstack[STACK_NEED];
    int tpos = 0;
    int dstack[STACK_NEED+1];
    int dpos = 1;
    int state = 0;
    ErtsMonitor **this = root;
    Sint c;
  
    dstack[0] = DIR_END;
    for (;;) {
	if (!*this) { /* Found our place */
	    state = 1;
	    *this = create_monitor(type,ref,pid,name);
	    break;
	} else if ((c = CMP_MON_REF(ref,(*this)->ref)) < 0) { 
	    /* go left */
	    dstack[dpos++] = DIR_LEFT;
	    tstack[tpos++] = this;
	    this = &((*this)->left);
	} else if (c > 0) { /* go right */
	    dstack[dpos++] = DIR_RIGHT;
	    tstack[tpos++] = this;
	    this = &((*this)->right);
	} else { /* Equal key is an error for monitors */
	    erl_exit(1,"Insertion of already present monitor!");
	    break;
	}
    }
    insertion_rotation(dstack, dpos, tstack, tpos, state);
}


/* Returns 0 if OK, < 0 if already present */
int erts_add_link(ErtsLink **root, Uint type, Eterm pid)
{
    void *tstack[STACK_NEED];
    int tpos = 0;
    int dstack[STACK_NEED+1];
    int dpos = 1;
    int state = 0;
    ErtsLink **this = root;
    Sint c;
  
    dstack[0] = DIR_END;
    for (;;) {
	if (!*this) { /* Found our place */
	    state = 1;
	    *this = create_link(type,pid);
	    break;
	} else if ((c = cmp(pid,(*this)->pid)) < 0) { 
	    /* go left */
	    dstack[dpos++] = DIR_LEFT;
	    tstack[tpos++] = this;
	    this = &((*this)->left);
	} else if (c > 0) { /* go right */
	    dstack[dpos++] = DIR_RIGHT;
	    tstack[tpos++] = this;
	    this = &((*this)->right);
	} else { /* Equal key is an error for monitors */
	    return -1;
	}
    }
    insertion_rotation(dstack, dpos, tstack, tpos, state);
    return 0;
}

/* Returns the new or old link structure */
ErtsLink *erts_add_or_lookup_link(ErtsLink **root, Uint type, Eterm pid)
{
    void *tstack[STACK_NEED];
    int tpos = 0;
    int dstack[STACK_NEED+1];
    int dpos = 1;
    int state = 0;
    ErtsLink **this = root;
    Sint c;
    ErtsLink *ret = NULL;

    dstack[0] = DIR_END;
    for (;;) {
	if (!*this) { /* Found our place */
	    state = 1;
	    *this = create_link(type,pid);
	    ret = *this;
	    break;
	} else if ((c = cmp(pid,(*this)->pid)) < 0) { 
	    /* go left */
	    dstack[dpos++] = DIR_LEFT;
	    tstack[tpos++] = this;
	    this = &((*this)->left);
	} else if (c > 0) { /* go right */
	    dstack[dpos++] = DIR_RIGHT;
	    tstack[tpos++] = this;
	    this = &((*this)->right);
	} else { /* Equal key is an error for monitors */
	    return *this;
	}
    }
    insertion_rotation(dstack, dpos, tstack, tpos, state);
    return ret;
}


/*
 * Deletion helpers
 */
static int balance_left(ErtsMonitorOrLink **this) 
{
    ErtsMonitorOrLink *p, *p1, *p2;
    int b1, b2, h = 1;
    
    p = *this;
    switch (p->balance) {
    case -1:
	p->balance = 0;
	break;
    case 0:
	p->balance = 1;
	h = 0;
	break;
    case 1:
	p1 = p->right;
	b1 = p1->balance;
	if (b1 >= 0) { /* Single RR rotation */
	    p->right = p1->left;
	    p1->left = p;
	    if (b1 == 0) {
		p->balance = 1;
		p1->balance = -1;
		h = 0;
	    } else {
		p->balance = p1->balance = 0;
	    }
	    (*this) = p1;
	} else { /* Double RL rotation */
	    p2 = p1->left;
	    b2 = p2->balance;
	    p1->left = p2->right;
	    p2->right = p1;
	    p->right = p2->left;
	    p2->left = p;
	    p->balance = (b2 == 1) ? -1 : 0;
	    p1->balance = (b2 == -1) ? 1 : 0;
	    p2->balance = 0;
	    (*this) = p2;
	}
	break;
    }
    return h;
}

static int balance_right(ErtsMonitorOrLink **this) 
{
    ErtsMonitorOrLink *p, *p1, *p2;
    int b1, b2, h = 1;
    
    p = *this;
    switch (p->balance) {
    case 1:
	p->balance = 0;
	break;
    case 0:
	p->balance = -1;
	h = 0;
	break;
    case -1:
	p1 = p->left;
	b1 = p1->balance;
	if (b1 <= 0) { /* Single LL rotation */
	    p->left = p1->right;
	    p1->right = p;
	    if (b1 == 0) {
		p->balance = -1;
		p1->balance = 1;
		h = 0;
	    } else {
		p->balance = p1->balance = 0;
	    }
	    (*this) = p1;
	} else { /* Double LR rotation */
	    p2 = p1->right;
	    b2 = p2->balance;
	    p1->right = p2->left;
	    p2->left = p1;
	    p->left = p2->right;
	    p2->right = p;
	    p->balance = (b2 == -1) ? 1 : 0;
	    p1->balance = (b2 == 1) ? -1 : 0;
	    p2->balance = 0;
	    (*this) = p2;
	}
    }
    return h;
}

static int delsub(ErtsMonitorOrLink **this) 
{
    ErtsMonitorOrLink **tstack[STACK_NEED];
    int tpos = 0;
    ErtsMonitorOrLink *q = (*this);
    ErtsMonitorOrLink **r = &(q->left);
    int h;

    /*
     * Walk down the tree to the right and search 
     * for a void right child, pick that child out
     * and return it to be put in the deleted 
     * object's place.
     */
    
    while ((*r)->right != NULL) {
	tstack[tpos++] = r;
	r = &((*r)->right);
    }
    *this = *r;
    *r = (*r)->left;
    (*this)->left = q->left;
    (*this)->right = q->right;
    (*this)->balance = q->balance;
    tstack[0] = &((*this)->left);
    h = 1;
    while (tpos && h) {
	r = tstack[--tpos];
	h = balance_right(r);
    }
    return h;
}

ErtsMonitor *erts_remove_monitor(ErtsMonitor **root, Eterm ref) 
{
    ErtsMonitor **tstack[STACK_NEED];
    int tpos = 0;
    int dstack[STACK_NEED+1];
    int dpos = 1;
    int state = 0;
    ErtsMonitor **this = root;
    Sint c;
    int dir;
    ErtsMonitor *q = NULL;

    dstack[0] = DIR_END;
    for (;;) {
	if (!*this) { /* Failure */
	    return NULL;
	} else if ((c = CMP_MON_REF(ref,(*this)->ref)) < 0) { 
	    dstack[dpos++] = DIR_LEFT;
	    tstack[tpos++] = this;
	    this = &((*this)->left);
	} else if (c > 0) { /* go right */
	    dstack[dpos++] = DIR_RIGHT;
	    tstack[tpos++] = this;
	    this = &((*this)->right);
	} else { /* Equal key, found the one to delete */
	    q = (*this);
	    if (q->right == NULL) {
		(*this) = q->left;
		state = 1;
	    } else if (q->left == NULL) {
		(*this) = q->right;
		state = 1;
	    } else {
		dstack[dpos++] = DIR_LEFT;
		tstack[tpos++] = this;
		state = delsub((ErtsMonitorOrLink **) this);
	    }
	    break;
	}
    }
    while (state && ( dir = dstack[--dpos] ) != DIR_END) {
	this = tstack[--tpos];
	if (dir == DIR_LEFT) {
	    state = balance_left((ErtsMonitorOrLink **) this);
	} else {
	    state = balance_right((ErtsMonitorOrLink **) this);
	}
    }
    return q;
}

ErtsLink *erts_remove_link(ErtsLink **root, Eterm pid) 
{
    ErtsLink **tstack[STACK_NEED];
    int tpos = 0;
    int dstack[STACK_NEED+1];
    int dpos = 1;
    int state = 0;
    ErtsLink **this = root;
    Sint c;
    int dir;
    ErtsLink *q = NULL;

    dstack[0] = DIR_END;
    for (;;) {
	if (!*this) { /* Failure */
	    return NULL;
	} else if ((c = cmp(pid,(*this)->pid)) < 0) { 
	    dstack[dpos++] = DIR_LEFT;
	    tstack[tpos++] = this;
	    this = &((*this)->left);
	} else if (c > 0) { /* go right */
	    dstack[dpos++] = DIR_RIGHT;
	    tstack[tpos++] = this;
	    this = &((*this)->right);
	} else { /* Equal key, found the one to delete */
	    q = (*this);
	    if (q->right == NULL) {
		(*this) = q->left;
		state = 1;
	    } else if (q->left == NULL) {
		(*this) = q->right;
		state = 1;
	    } else {
		dstack[dpos++] = DIR_LEFT;
		tstack[tpos++] = this;
		state = delsub((ErtsMonitorOrLink **) this);
	    }
	    break;
	}
    }
    while (state && ( dir = dstack[--dpos] ) != DIR_END) {
	this = tstack[--tpos];
	if (dir == DIR_LEFT) {
	    state = balance_left((ErtsMonitorOrLink **) this);
	} else {
	    state = balance_right((ErtsMonitorOrLink **) this);
	}
    }
    return q;
}

ErtsMonitor *erts_lookup_monitor(ErtsMonitor *root, Eterm ref) 
{
    Sint c;

    for (;;) {
	if (root == NULL || (c = CMP_MON_REF(ref,root->ref)) == 0) {
	    return root;
	} else if (c < 0) { 
	    root = root->left;
	} else { /* c > 0 */ 
	    root = root->right;
	} 
    }
}

ErtsLink *erts_lookup_link(ErtsLink *root, Eterm pid) 
{
    Sint c;

    for (;;) {
	if (root == NULL || (c = cmp(pid,root->pid)) == 0) {
	    return root;
	} else if (c < 0) { 
	    root = root->left;
	} else { /* c > 0 */ 
	    root = root->right;
	} 
    }
}

void erts_sweep_monitors(ErtsMonitor *root, 
			 void (*doit)(ErtsMonitor *, void *),
			 void *context) 
{
    ErtsMonitor *tstack[STACK_NEED];
    int tpos = 0;
    int dstack[STACK_NEED+1];
    int dpos = 1;
    int dir;
    
    dstack[0] = DIR_END;

    for (;;) {
	if (root == NULL) {
	    if ((dir = dstack[dpos-1]) == DIR_END) {
		return;
	    }
	    if (dir == DIR_LEFT) {
		/* Still has DIR_RIGHT to do */
		dstack[dpos-1] = DIR_RIGHT;
		root = (tstack[tpos-1])->right;
	    } else {
		/* stacktop is an object to be deleted */
		(*doit)(tstack[--tpos],context); /* expeted to do the 
						    deletion */
		--dpos;
		root = NULL;
	    }
	} else {
	    dstack[dpos++] = DIR_LEFT;
	    tstack[tpos++] = root;
	    root = root->left;
	}
    }
}

void erts_sweep_links(ErtsLink *root, 
		      void (*doit)(ErtsLink *, void *),
		      void *context) 
{
    ErtsLink *tstack[STACK_NEED];
    int tpos = 0;
    int dstack[STACK_NEED+1];
    int dpos = 1;
    int dir;
    
    dstack[0] = DIR_END;

    for (;;) {
	if (root == NULL) {
	    if ((dir = dstack[dpos-1]) == DIR_END) {
		return;
	    }
	    if (dir == DIR_LEFT) {
		/* Still has DIR_RIGHT to do */
		dstack[dpos-1] = DIR_RIGHT;
		root = (tstack[tpos-1])->right;
	    } else {
		/* stacktop is an object to be deleted */
		(*doit)(tstack[--tpos],context); /* expeted to do the 
						    deletion */
		--dpos;
		root = NULL;
	    }
	} else {
	    dstack[dpos++] = DIR_LEFT;
	    tstack[tpos++] = root;
	    root = root->left;
	}
    }
}


/* Debug BIF, always present, but undocumented... */
	    
static void erts_dump_monitors(ErtsMonitor *root, int indent)
{
    if (root == NULL)
	return;
    erts_dump_monitors(root->right,indent+2);
    erl_printf(COUT,"%*s[%d:%d:",indent,"",(int) root->balance,
	       (int) root->type);
    display(root->ref,COUT);
    erl_printf(COUT,":");
    display(root->pid,COUT);
    erl_printf(COUT,":");
    display(root->name,COUT);
    erl_printf(COUT,"]\r\n");
    erts_dump_monitors(root->left,indent+2);
}

static void erts_dump_links(ErtsLink *root, int indent)
{
    if (root == NULL)
	return;
    erts_dump_links(root->right,indent+2);
    cerr_pos = 0;
    erl_printf(CBUF,"%*s[%d:%d:",indent,"",(int) root->balance,
	       (int) root->type);
    display(root->pid,CBUF);
    erl_printf(CBUF,":");
    erl_printf(CBUF,"0x%08X]", (Uint) root->root);
    if (root->root != NULL) {
	ErtsLink *sub = root->root;
	int len = strlen(tmp_buf);
	erts_dump_links(sub->right,indent+len+5);
	erl_printf(CBUF,"-> %*s[%d:%d:",indent,"",(int) sub->balance,
		   (int) sub->type);
	display(sub->pid,CBUF);
	erl_printf(CBUF,":");
	erl_printf(CBUF,"0x%08X]", (Uint) sub->root);
	erl_printf(COUT,"%s\r\n",tmp_buf);
	erts_dump_links(sub->left,indent+len+5);
    } else {
	erl_printf(COUT,"%s\r\n",tmp_buf);
    }
    erts_dump_links(root->left,indent+2);
}

Eterm erts_debug_dump_monitors_1(Process *p, Eterm pid)
{
    Process *rp;
    DistEntry *dep;
    if (!is_pid(pid) || (rp = pid2proc(pid)) == NULL) {
	if (is_atom(pid) && is_node_name_atom(pid) &&
	    (dep = erts_find_dist_entry(pid)) != NULL) {
	    erl_printf(COUT,"Dumping dist monitors-------------------\r\n");
	    erts_dump_monitors(dep->monitors,0);
	    erl_printf(COUT,"Monitors dumped-------------------------\r\n");
	    BIF_RET(am_true);
	} else {
	    BIF_ERROR(p,BADARG);
	}
    } else {
	erl_printf(COUT,"Dumping pid monitors--------------------\r\n");
	erts_dump_monitors(rp->monitors,0);
	erl_printf(COUT,"Monitors dumped-------------------------\r\n");
	BIF_RET(am_true);
    }
}

Eterm erts_debug_dump_links_1(Process *p, Eterm pid)
{
    Process *rp;
    DistEntry *dep;
    if (is_internal_port(pid)) {
	Port *rport = erts_port+internal_port_index(pid);
	if (! INVALID_PORT(rport, pid)) {
	    erl_printf(COUT,"Dumping port links----------------------\r\n");
	    erts_dump_links(rport->nlinks,0);
	    erl_printf(COUT,"Links dumped----------------------------\r\n");
	    BIF_RET(am_true);
	} else {
	    BIF_ERROR(p,BADARG);
	}
    } else if (!is_pid(pid) || (rp = pid2proc(pid)) == NULL) {
	if (is_atom(pid) && is_node_name_atom(pid) &&
	    (dep = erts_find_dist_entry(pid)) != NULL) {
	    erl_printf(COUT,"Dumping dist links----------------------\r\n");
	    erts_dump_links(dep->nlinks,0);
	    erl_printf(COUT,"Links dumped----------------------------\r\n");
	    BIF_RET(am_true);
	} else {
	    BIF_ERROR(p,BADARG);
	}
    } else {
	erl_printf(COUT,"Dumping pid links-----------------------\r\n");
	erts_dump_links(rp->nlinks,0);
	erl_printf(COUT,"Links dumped----------------------------\r\n");
	BIF_RET(am_true);
    }
}
