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

#ifndef ERL_MSEG_H_
#define ERL_MSEG_H_

#include "sys.h"

#ifndef HAVE_MMAP
#  define HAVE_MMAP 0
#endif
#ifndef HAVE_MREMAP
#  define HAVE_MREMAP 0
#endif

#if HAVE_MMAP
#  define HAVE_ERTS_MSEG 1
#else
#  define HAVE_ERTS_MSEG 0
#endif

#if HAVE_ERTS_MSEG

#define ERTS_MSEG_VSN_STR "0.9"

typedef struct {
    Uint amcbf;
    Uint rmcbf;
    Uint mcs;
    Uint cci;
} ErtsMsegInit_t;

#define ERTS_MSEG_INIT_DEFAULT_INITIALIZER				\
{									\
    4*1024*1024,	/* amcbf: Absolute max cache bad fit	*/	\
    20,			/* rmcbf: Relative max cache bad fit	*/	\
    5,			/* mcs:   Max cache size		*/	\
    1000		/* cci:   Cache check interval		*/	\
}

typedef struct {
    int  cache;
    int  preserv;
    Uint abs_shrink_th;
    Uint rel_shrink_th;
} ErtsMsegOpt_t;

#define ERTS_MSEG_DEFAULT_OPT_INITIALIZER				\
{									\
    1,			/* Use cache				*/	\
    1,			/* Preserv data				*/	\
    0,			/* Absolute shrink threshold		*/	\
    0			/* Relative shrink threshold		*/	\
}

void *erts_mseg_alloc(Uint *size_p);
void *erts_mseg_alloc_opt(Uint *size_p, const ErtsMsegOpt_t *opt);
void  erts_mseg_dealloc(void *seg, Uint size);
void  erts_mseg_dealloc_opt(void *seg, Uint size, const ErtsMsegOpt_t *opt);
void *erts_mseg_realloc(void *seg, Uint old_size, Uint *new_size_p);
void *erts_mseg_realloc_opt(void *seg, Uint old_size, Uint *new_size_p,
			    const ErtsMsegOpt_t *opt);
void  erts_mseg_clear_cache(void);
Uint  erts_mseg_no(void);
Uint  erts_mseg_unit_size(void);
void  erts_mseg_init(ErtsMsegInit_t *init);
void  erts_mseg_late_init(void); /* Have to be called after all allocators,
				   threads and timers have been initialized. */
Eterm erts_mseg_info_options(CIO *ciop, Uint **hpp, Uint *szp);
Eterm erts_mseg_info(CIO *ciop, Uint **hpp, Uint *szp);

#endif

#endif




