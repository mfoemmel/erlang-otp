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
#include <stdio.h>

typedef struct _Pool {
    int desc;
    void* ptr;
    struct _Pool* next;
} Pool;

static Pool* root = NULL;

void
purify_map_pool(int desc, void (*free_func)(void *))
{
    Pool* p;
    Pool* next;
    Pool** pp = &root;

    while (*pp != NULL) {
	p = *pp;
	next = p->next;
	if (p->desc == desc) {
	    *pp = next;
	    free_func(p->ptr);
	    free(p);
	}
	pp = &(next->next);
    }
}

void
purify_set_pool_id(void* ptr, int desc)
{
    Pool* p = (Pool *) malloc(sizeof(Pool));

    p->desc = desc;
    p->ptr = ptr;
    p->next = root;
    root = p;
    
}

void
purify_clear_pool_id(int desc, void* ptr)
{
    Pool** pp;
    Pool* p;

    pp = &root;
    while (*pp != NULL) {
	p = *pp;
	if (p->ptr == ptr && p->desc == desc) {
	    *pp = p->next;
	    free(p);
	    return;
	}
	pp = &(p->next);
    }
}

