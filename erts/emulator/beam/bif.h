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
#ifndef __BIF_H__
#define __BIF_H__

#define BIF_RETTYPE Eterm

#define BIF_P A__p

#define BIF_ALIST_0 A__p
#define BIF_ALIST_1 A__p, A_1
#define BIF_ALIST_2 A__p, A_1, A_2
#define BIF_ALIST_3 A__p, A_1, A_2, A_3

#define BIF_ADECL_0 Process* A__p;
#define BIF_ADECL_1 Process* A__p; uint32 A_1;
#define BIF_ADECL_2 Process* A__p; uint32 A_1; uint32 A_2;
#define BIF_ADECL_3 Process* A__p; uint32 A_1; uint32 A_2; uint32 A_3;

#define BIF_ARG_1  A_1
#define BIF_ARG_2  A_2
#define BIF_ARG_3  A_3

#define BUMP_ALL_REDS(p) do {if ((p)->ct == NULL) (p)->fcalls = 0; else (p)->fcalls = -CONTEXT_REDS;} while(0)

#define BIF_RET2(x, gc) do { \
     BIF_P->fcalls -= (gc); \
     if (BIF_P->fcalls < 0) { \
	if (BIF_P->ct == NULL) \
           BIF_P->fcalls = 0; \
	else if (BIF_P->fcalls < -CONTEXT_REDS) \
           BIF_P->fcalls = -CONTEXT_REDS; \
     } \
     return (x); \
} while(0)

#define BIF_RET(x) return (x)

#define BIF_ERROR(p,r) { (p)->freason = r; return 0; }

#define BIF_TRAP(p, Trap_) do { \
      (p)->fvalue = (Eterm) (Trap_); \
      (p)->freason = TRAP; \
      return 0; \
 } while(0)

#define BIF_TRAP0(Trap_,p)          BIF_TRAP(p, Trap_)

#define BIF_TRAP1(Trap_,p,a0)       do { (p)->def_arg_reg[0] = (a0); \
					  BIF_TRAP0(Trap_, p); } while (0)

#define BIF_TRAP2(Trap_,p,a0,a1)    do { (p)->def_arg_reg[1] = (a1); \
					  BIF_TRAP1(Trap_, p, a0); } while (0)

#define BIF_TRAP3(Trap_,p,a0,a1,a2) do { (p)->def_arg_reg[2] = (a2); \
					  BIF_TRAP2(Trap_, p, a0, a1); } while (0)

#include "erl_bif_table.h"

#endif
