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

#include "hash.h"
#include "erl_process.h"
#include "erl_monitors.h"

#define ERST_INTERNAL_CHANNEL_NO 0

struct erl_link;
struct process;

typedef struct dist_entry_ {
    HashBucket hash_bucket;     /* Hash bucket */
    struct dist_entry_ *next;	/* Next entry in dist_table (not sorted) */
    struct dist_entry_ *prev;	/* Previous entry in dist_table (not sorted) */
    Uint refc;			/* Reference count */
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
} DistEntry;

typedef struct erl_node_ {
  HashBucket	 hash_bucket;	/* Hash bucket */
  Uint		 refc;		/* Reference count */
  Eterm		 sysname;	/* name@host atom for efficiency */
  Uint32	 creation;	/* Creation */
  DistEntry	*dist_entry;	/* Corresponding dist entry */
} ErlNode;


extern Hash erts_dist_table;
extern Hash erts_node_table;


extern DistEntry *erts_hidden_dist_entries;
extern DistEntry *erts_visible_dist_entries;
extern DistEntry *erts_not_connected_dist_entries;
extern Sint erts_no_of_hidden_dist_entries;
extern Sint erts_no_of_visible_dist_entries;
extern Sint erts_no_of_not_connected_dist_entries;

extern DistEntry *erts_this_dist_entry;
extern ErlNode *erts_this_node;

#define DEREF_DIST_ENTRY(DEP)						\
do {									\
    ASSERT((DEP));							\
    ASSERT((DEP)->refc > 0);						\
    (DEP)->refc--;							\
    if ((DEP)->refc == 0)						\
	erts_delete_dist_entry((DEP));					\
} while (0)

#define DEREF_ERL_NODE(ENP)						\
do {									\
    ASSERT((ENP));							\
    ASSERT((ENP)->refc > 0);						\
    (ENP)->refc--;							\
    if ((ENP)->refc == 0)						\
	erts_delete_node((ENP));					\
} while (0)

DistEntry *erts_channel_no_to_dist_entry(Uint);
DistEntry *erts_sysname_to_connected_dist_entry(Eterm);
DistEntry *erts_find_or_insert_dist_entry(Eterm);
DistEntry *erts_find_dist_entry(Eterm);
void erts_delete_dist_entry(DistEntry *);
Uint erts_dist_table_size(void);
void erts_dist_table_info(CIO);
void erts_set_dist_entry_not_connected(DistEntry *);
void erts_set_dist_entry_connected(DistEntry *, Eterm, Uint);
ErlNode *erts_find_or_insert_node(Eterm, Uint);
void erts_delete_node(ErlNode *);
void erts_set_this_node(Eterm, Uint);
Uint erts_node_table_size(void);
void erts_init_node_tables(void);
void erts_node_table_info(CIO);
void erts_print_node_info(CIO, Eterm, int*, int*);
Eterm erts_get_node_and_dist_references(struct process *);
#endif
