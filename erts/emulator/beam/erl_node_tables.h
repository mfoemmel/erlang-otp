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

#ifndef ERL_NODE_TABLES_H__
#define ERL_NODE_TABLES_H__

/*
 * The "node_tables module" contain two (hash) tables: the node_table
 * and the dist_table.
 *
 * The elements of the node_table represents a specific incarnation of
 * an Erlang node and has {Nodename, Creation} pairs as keys. Elements
 * in the node_table are referred to from node containers (see
 * node_container_utils.h).
 *
 * The elements of the dist_table represents a (potential) connection
 * to an Erlang node and has Nodename as key. Elements in the
 * dist_table are either referred to from elements in the node_table
 * or from the process or port structure of the entity controlling
 * the connection.
 *
 * Both tables are garbage collected by reference counting.
 */

#include "sys.h"
#include "hash.h"
#include "erl_process.h"
#include "erl_monitors.h"
#include "erl_smp.h"

#define ERST_INTERNAL_CHANNEL_NO 0

#define ERTS_DE_SFLG_INITIALIZING			(((Uint32) 1) <<  0)
#define ERTS_DE_SFLG_CONNECTED				(((Uint32) 1) <<  1)
#define ERTS_DE_SFLG_EXITING				(((Uint32) 1) <<  2)

/*
 * Lock order:
 *   1. dist_entry->mtxp
 *   2. erts_node_table_mtx
 *   3. erts_dist_table_mtx
 *
 *   Lock mutexes with lower numbers before mutexes with higher numbers and
 *   unlock mutexes with higher numbers before mutexes with higher numbers.
 */

struct erl_link;
struct process;
struct port;

typedef struct dist_entry_ {
    HashBucket hash_bucket;     /* Hash bucket */
    struct dist_entry_ *next;	/* Next entry in dist_table (not sorted) */
    struct dist_entry_ *prev;	/* Previous entry in dist_table (not sorted) */
    erts_refc_t refc;		/* Reference count */
    Eterm sysname;		/* name@host atom for efficiency */
    Uint32 creation;		/* creation of connected node */
    Eterm cid;			/* connection handler (pid or port), NIL == free */
    ErtsLink *node_links;       /* In a dist entry, node links are kept 
				   in a separate tree, while they are 
				   colocted with the ordinary link tree
				   for processes. It's not due to confusion,
				   it's because the link tree for the dist 
				   entry is in two levels, see erl_monitors.h 
				*/
    ErtsLink *nlinks;           /* Link tree with subtrees */
    ErtsMonitor *monitors;      /* Monitor tree */
    Uint32 status;		/* Slot status, like exiting reserved etc */
    Uint32 flags;		/* Distribution flags, like hidden, 
				   atom cache etc. */
    struct cache* cache;	/* The atom cache */
    unsigned long version;	/* Protocol version */
    struct port *port;
#ifdef ERTS_SMP
    erts_smp_mtx_t *mtxp;
#endif

} DistEntry;

typedef struct erl_node_ {
  HashBucket hash_bucket;	/* Hash bucket */
  erts_refc_t refc;		/* Reference count */
  Eterm	sysname;		/* name@host atom for efficiency */
  Uint32 creation;		/* Creation */
  DistEntry *dist_entry;	/* Corresponding dist entry */
} ErlNode;


extern Hash erts_dist_table;
extern Hash erts_node_table;
extern erts_smp_mtx_t erts_dist_table_mtx;
extern erts_smp_mtx_t erts_node_table_mtx;

extern DistEntry *erts_hidden_dist_entries;
extern DistEntry *erts_visible_dist_entries;
extern DistEntry *erts_not_connected_dist_entries;
extern Sint erts_no_of_hidden_dist_entries;
extern Sint erts_no_of_visible_dist_entries;
extern Sint erts_no_of_not_connected_dist_entries;

extern DistEntry *erts_this_dist_entry;
extern ErlNode *erts_this_node;

#ifdef ERTS_SMP
#define ERTS_SMP_LOCK_NODE_TABLES_AND_ENTRIES \
  erts_lock_node_tables_and_entries()
#define ERTS_SMP_UNLOCK_NODE_TABLES_AND_ENTRIES \
  erts_unlock_node_tables_and_entries()
void erts_lock_node_tables_and_entries(void);
void erts_unlock_node_tables_and_entries(void);
#else
#define ERTS_SMP_LOCK_NODE_TABLES_AND_ENTRIES
#define ERTS_SMP_UNLOCK_NODE_TABLES_AND_ENTRIES
#endif

DistEntry *erts_channel_no_to_dist_entry(Uint);
DistEntry *erts_sysname_to_connected_dist_entry(Eterm);
DistEntry *erts_find_or_insert_dist_entry(Eterm);
DistEntry *erts_find_dist_entry(Eterm);
void erts_delete_dist_entry(DistEntry *);
Uint erts_dist_table_size(void);
void erts_dist_table_info(int, void *);
void erts_set_dist_entry_not_connected(DistEntry *);
void erts_set_dist_entry_connected(DistEntry *, Eterm, Uint);
ErlNode *erts_find_or_insert_node(Eterm, Uint);
void erts_delete_node(ErlNode *);
void erts_set_this_node(Eterm, Uint);
Uint erts_node_table_size(void);
void erts_init_node_tables(void);
void erts_node_table_info(int, void *);
void erts_print_node_info(int, void *, Eterm, int*, int*);
Eterm erts_get_node_and_dist_references(struct process *);
#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
int erts_lc_is_dist_entry_locked(DistEntry *);
#endif

ERTS_GLB_INLINE void erts_deref_dist_entry(DistEntry *dep);
ERTS_GLB_INLINE void erts_deref_node_entry(ErlNode *np);
ERTS_GLB_INLINE void erts_smp_dist_entry_lock(DistEntry *dep);
ERTS_GLB_INLINE void erts_smp_dist_entry_unlock(DistEntry *dep);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_deref_dist_entry(DistEntry *dep)
{
    ASSERT(dep);
    if (erts_refc_dectest(&dep->refc, 0) == 0)
	erts_delete_dist_entry(dep);
}

ERTS_GLB_INLINE void
erts_deref_node_entry(ErlNode *np)
{
    ASSERT(np);
    if (erts_refc_dectest(&np->refc, 0) == 0)
	erts_delete_node(np);
}

ERTS_GLB_INLINE void
erts_smp_dist_entry_lock(DistEntry *dep)
{
#ifdef ERTS_SMP
    erts_smp_mtx_lock(dep->mtxp);
#endif
}

ERTS_GLB_INLINE void
erts_smp_dist_entry_unlock(DistEntry *dep)
{
#ifdef ERTS_SMP
    erts_smp_mtx_unlock(dep->mtxp);
#endif
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */


#endif
