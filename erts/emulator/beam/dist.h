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

#ifndef __DIST_H__
#define __DIST_H__

#define DFLAG_PUBLISHED           0x01
#define DFLAG_ATOM_CACHE          0x02
#define DFLAG_EXTENDED_REFERENCES 0x04
#define DFLAG_DIST_MONITOR        0x08
#define DFLAG_FUN_TAGS            0x10
#define DFLAG_DIST_MONITOR_NAME   0x20
#define DFLAG_HIDDEN_ATOM_CACHE   0x40
#define DFLAG_NEW_FUN_TAGS        0x80
#define DFLAG_EXTENDED_PIDS_PORTS 0x100
#define DFLAG_EXPORT_PTR_TAG      0x200
#define DFLAG_BIT_BINARIES        0x400

/* All flags that should be enabled when term_to_binary/1 is used. */
#define TERM_TO_BINARY_DFLAGS (DFLAG_EXTENDED_REFERENCES	\
			       | DFLAG_NEW_FUN_TAGS		\
			       | DFLAG_EXTENDED_PIDS_PORTS	\
			       | DFLAG_EXPORT_PTR_TAG		\
			       | DFLAG_BIT_BINARIES)

/* opcodes used in distribution messages */
#define DOP_LINK		1
#define DOP_SEND		2
#define DOP_EXIT		3
#define DOP_UNLINK		4
#define DOP_NODE_LINK		5
#define DOP_REG_SEND		6
#define DOP_GROUP_LEADER	7
#define DOP_EXIT2		8

#define DOP_SEND_TT		12
#define DOP_EXIT_TT		13
#define DOP_REG_SEND_TT		16
#define DOP_EXIT2_TT		18

#define DOP_MONITOR_P		19
#define DOP_DEMONITOR_P		20
#define DOP_MONITOR_P_EXIT	21

#ifdef __SYS_H__

/* distribution trap functions */
extern Export* dsend2_trap;
extern Export* dsend3_trap;
/*extern Export* dsend_nosuspend_trap;*/
extern Export* dlink_trap;
extern Export* dunlink_trap;
extern Export* dmonitor_node_trap;
extern Export* dgroup_leader_trap;
extern Export* dexit_trap;
extern Export* dmonitor_p_trap;



extern int dist_link(Process*, Uint32, DistEntry*, Eterm, Eterm);
extern int dist_send(Process*, Uint32, DistEntry*, Eterm, Eterm);
extern int dist_exit_tt(Process*, Uint32, DistEntry*, Eterm, Eterm, Eterm, Eterm);
extern int dist_unlink(Process*, Uint32, DistEntry*, Eterm, Eterm);
extern int dist_reg_send(Process*, Uint32, DistEntry*, Eterm, Eterm);
extern int dist_group_leader(Process*, Uint32, DistEntry*, Eterm, Eterm);
extern int dist_exit(Process*, Uint32, DistEntry*, Eterm, Eterm, Eterm);
extern int dist_exit2(Process*, Uint32, DistEntry*, Eterm, Eterm, Eterm);
#endif

extern int dist_demonitor(Process*, Uint32, DistEntry*, Eterm, Eterm, Eterm, int);
extern int dist_monitor(Process*, Uint32, DistEntry*, Eterm, Eterm, Eterm);
extern int dist_m_exit(Process*, Uint32, DistEntry*, Eterm, Eterm, Eterm, Eterm);

extern Uint erts_dist_cache_size(void);
extern int erts_is_alive(void);

#endif
