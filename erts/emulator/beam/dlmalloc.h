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
#ifdef DLMALLOC_IS_CLIB

#ifndef DLMALLOC_H__
#define DLMALLOC_H__
/*
  A slightly modified version of malloc/free/realloc written by Doug Lea.
  Send questions/comments/complaints/performance data to dl@cs.oswego.edu

* VERSION 2.6.6  Sun Mar  5 19:10:03 2000  Doug Lea  (dl at gee)
*/

#define DLMALLOC_MAJOR 2
#define DLMALLOC_MINOR 6
#define DLMALLOC_BUILD 6
#define DLMALLOC_ERTS  1

/* Preliminaries */

#ifndef __STD_C
#ifdef __STDC__
#define __STD_C     1
#else
#if __cplusplus
#define __STD_C     1
#else
#define __STD_C     0
#endif /*__cplusplus*/
#endif /*__STDC__*/
#endif /*__STD_C*/

#ifndef Void_t
#if (__STD_C || defined(__WIN32__))
#define Void_t      void
#else
#define Void_t      char
#endif
#endif /*Void_t*/

#if __STD_C
#include <stddef.h>   /* for size_t */
#else
#include <sys/types.h>
#endif

/* SVID2/XPG mallinfo structure */

struct mallinfo {
  int arena;    /* total space allocated from system */
  int ordblks;  /* number of non-inuse chunks */
  int smblks;   /* unused -- always zero */
  int hblks;    /* number of mmapped regions */
  int hblkhd;   /* total space in mmapped regions */
  int usmblks;  /* unused -- always zero */
  int fsmblks;  /* unused -- always zero */
  int uordblks; /* total allocated space */
  int fordblks; /* total non-inuse space */
  int keepcost; /* top-most, releasable (via malloc_trim) space */
};      

/* SVID2/XPG mallopt options */

#define M_MXFAST                 1    /* UNUSED in this malloc */
#define M_NLBLKS                 2    /* UNUSED in this malloc */
#define M_GRAIN                  3    /* UNUSED in this malloc */
#define M_KEEP                   4    /* UNUSED in this malloc */

/* mallopt options that actually do something */

#define M_TRIM_THRESHOLD         (-1)
#define M_TOP_PAD                (-2)
#define M_MMAP_THRESHOLD         (-3)
#define M_MMAP_MAX               (-4)


#undef  HAVE_MALLINFO
#define HAVE_MALLINFO            1

#undef  HAVE_MALLOPT
#define HAVE_MALLOPT             1

#if __STD_C
void    init_dlmalloc(void);
Void_t* malloc(size_t);
void    free(Void_t*);
Void_t* realloc(Void_t*, size_t);
Void_t* memalign(size_t, size_t);
Void_t* valloc(size_t);
Void_t* pvalloc(size_t);
Void_t* calloc(size_t, size_t);
void    cfree(Void_t*);
int     malloc_trim(size_t);
size_t  malloc_usable_size(Void_t*);
void    malloc_stats();
int     mallopt(int, int);
struct mallinfo mallinfo(void);
#else
void    init_dlmalloc();
Void_t* malloc();
void    free();
Void_t* realloc();
Void_t* memalign();
Void_t* valloc();
Void_t* pvalloc();
Void_t* calloc();
void    cfree();
int     malloc_trim();
size_t  malloc_usable_size();
void    malloc_stats();
int     mallopt();
struct mallinfo mallinfo();
#endif

#endif /* #ifdef DLMALLOC_H__ */

#endif /* #ifdef DLMALLOC_IS_CLIB */



