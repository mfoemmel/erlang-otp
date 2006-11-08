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

#define BIF_ALIST_0 Process* A__p
#define BIF_ALIST_1 Process* A__p, Eterm A_1
#define BIF_ALIST_2 Process* A__p, Eterm A_1, Eterm A_2
#define BIF_ALIST_3 Process* A__p, Eterm A_1, Eterm A_2, Eterm A_3

#define BIF_ARG_1  A_1
#define BIF_ARG_2  A_2
#define BIF_ARG_3  A_3

#define BUMP_ALL_REDS(p) do {			\
    if ((p)->ct == NULL) 			\
	(p)->fcalls = 0; 			\
    else 					\
	(p)->fcalls = -CONTEXT_REDS;		\
} while(0)

#define BUMP_REDS(p, gc) do {			   \
     (p)->fcalls -= (gc); 			   \
     if ((p)->fcalls < 0) { 			   \
	if ((p)->ct == NULL) 		           \
           (p)->fcalls = 0; 			   \
	else if ((p)->fcalls < -CONTEXT_REDS)      \
           (p)->fcalls = -CONTEXT_REDS; 	   \
     } 						   \
} while(0)
    

#define BIF_RET2(x, gc) do {			\
    BUMP_REDS(BIF_P, (gc));			\
    return (x);					\
} while(0)

#define BIF_RET(x) return (x)

#define ERTS_BIF_PREP_RET(Ret, Val) ((Ret) = (Val))

#define BIF_ERROR(p,r) do { 			\
    (p)->freason = r; 				\
    return THE_NON_VALUE; 			\
} while(0)

#define ERTS_BIF_PREP_ERROR(Ret, Proc, Reason)	\
do {						\
    (Proc)->freason = (Reason);			\
    (Ret) = THE_NON_VALUE;			\
} while (0)


#define ERTS_BIF_PREP_TRAP0(Ret, Trap, Proc)		\
do {							\
    (Proc)->arity = 0;					\
    (Proc)->def_arg_reg[3] = (Eterm) (Trap);		\
    (Proc)->freason = TRAP;				\
    (Ret) = THE_NON_VALUE;				\
} while (0)

#define ERTS_BIF_PREP_TRAP1(Ret, Trap, Proc, A0)	\
do {							\
    (Proc)->arity = 1;					\
    (Proc)->def_arg_reg[0] = (Eterm) (A0);		\
    (Proc)->def_arg_reg[3] = (Eterm) (Trap);		\
    (Proc)->freason = TRAP;				\
    (Ret) = THE_NON_VALUE;				\
} while (0)

#define ERTS_BIF_PREP_TRAP2(Ret, Trap, Proc, A0, A1)	\
do {							\
    (Proc)->arity = 2;					\
    (Proc)->def_arg_reg[0] = (Eterm) (A0);		\
    (Proc)->def_arg_reg[1] = (Eterm) (A1);		\
    (Proc)->def_arg_reg[3] = (Eterm) (Trap);		\
    (Proc)->freason = TRAP;				\
    (Ret) = THE_NON_VALUE;				\
} while (0)

#define ERTS_BIF_PREP_TRAP3(Ret, Trap, Proc, A0, A1, A2)\
do {							\
    (Proc)->arity = 3;					\
    (Proc)->def_arg_reg[0] = (Eterm) (A0);		\
    (Proc)->def_arg_reg[1] = (Eterm) (A1);		\
    (Proc)->def_arg_reg[2] = (Eterm) (A2);		\
    (Proc)->def_arg_reg[3] = (Eterm) (Trap);		\
    (Proc)->freason = TRAP;				\
    (Ret) = THE_NON_VALUE;				\
} while (0)

#define BIF_TRAP0(p, Trap_) do {		\
      (p)->arity = 0;				\
      (p)->def_arg_reg[3] = (Eterm) (Trap_);	\
      (p)->freason = TRAP;			\
      return THE_NON_VALUE;			\
 } while(0)

#define BIF_TRAP1(Trap_, p, A0) do {		\
      (p)->arity = 1;				\
      (p)->def_arg_reg[0] = (A0);		\
      (p)->def_arg_reg[3] = (Eterm) (Trap_);	\
      (p)->freason = TRAP;			\
      return THE_NON_VALUE;			\
 } while(0)

#define BIF_TRAP2(Trap_, p, A0, A1) do {	\
      (p)->arity = 2;				\
      (p)->def_arg_reg[0] = (A0);		\
      (p)->def_arg_reg[1] = (A1);		\
      (p)->def_arg_reg[3] = (Eterm) (Trap_);	\
      (p)->freason = TRAP;			\
      return THE_NON_VALUE;			\
 } while(0)

#define BIF_TRAP3(Trap_, p, A0, A1, A2) do {	\
      (p)->arity = 3;				\
      (p)->def_arg_reg[0] = (A0);		\
      (p)->def_arg_reg[1] = (A1);		\
      (p)->def_arg_reg[2] = (A2);		\
      (p)->def_arg_reg[3] = (Eterm) (Trap_);	\
      (p)->freason = TRAP;			\
      return THE_NON_VALUE;			\
 } while(0)

#define ERTS_BIF_EXITED(PROC)		\
do {					\
    KILL_CATCHES((PROC));		\
    BIF_ERROR((PROC), EXC_EXIT);	\
} while (0)

#define ERTS_BIF_CHK_EXITED(PROC)	\
do {					\
    if (ERTS_PROC_IS_EXITING((PROC)))	\
	ERTS_BIF_EXITED((PROC));	\
} while (0)

#ifdef ERTS_SMP
/* "Call" ERTS_SMP_BIF_CHK_RESCHEDULE(P) when current process might have
   to be rescheduled in order to avoid deadlock */
#define ERTS_SMP_BIF_CHK_RESCHEDULE(P) \
do { if ((P)->freason == RESCHEDULE) return THE_NON_VALUE; } while (0)
#define ERTS_SMP_BIF_CHK_PENDING_EXIT(P, L)				\
do {									\
    ERTS_SMP_LC_ASSERT((L) == erts_proc_lc_my_proc_locks((P)));		\
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN & (L));			\
    if (!((L) & ERTS_PROC_LOCK_STATUS))					\
	erts_smp_proc_lock((P), ERTS_PROC_LOCK_STATUS);			\
    if (ERTS_PROC_PENDING_EXIT((P))) {					\
	erts_handle_pending_exit((P), (L)|ERTS_PROC_LOCK_STATUS);	\
	erts_smp_proc_unlock((P),					\
			     (((L)|ERTS_PROC_LOCK_STATUS)		\
			      & ~ERTS_PROC_LOCK_MAIN));			\
	ERTS_BIF_EXITED((P));						\
    }									\
    if (!((L) & ERTS_PROC_LOCK_STATUS))					\
	erts_smp_proc_unlock((P), ERTS_PROC_LOCK_STATUS);		\
} while (0)
#else
#define ERTS_SMP_BIF_CHK_RESCHEDULE(P)
#define ERTS_SMP_BIF_CHK_PENDING_EXIT(P, L)
#endif

#include "erl_bif_table.h"

#endif
