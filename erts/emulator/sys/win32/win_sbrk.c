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
/*
 * Simulates sbrk() on WIN32.  Will only work up to the 256M border,
 * to avoid giving untaggable pointers to the emulator.
 */
#include <stdio.h>
#include <stdlib.h>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN

/*
 * Aborts the program with an error message.  Note: Using printf() is
 * a bad idea, because it might call malloc().  Therefore, we use
 * WriteFile().
 */
#define ABORT(msg) \
{   static char error[] = msg; \
    DWORD written; \
    WriteFile(GetStdHandle(STD_ERROR_HANDLE), error, sizeof(error)-1, \
	      &written, NULL); \
    abort(); }

#define SEGMENT 0x10000		/* 64K segment */
#define START_UNUSABLE ((char *) 0x10000000) /* Start of unusuable memory. */

/*
 * This is a convenience structure use to hold the start and
 * size of a memory region.
 */

typedef struct {
  char* base;			/* Base address of region. */
  size_t size;			/* Size of region. */
} Region;

static char* current;		/* Current break address.  If NULL,
				 * no virtual memory has been initialized.
				 * If non-null, points to the first byte
				 * of reserverd but uncommitted memory
				 * (actually, pages are committed page-wise,
				 * so some memory above this pointer could
				 * be committed, but this doesn't matter
				 * since it is okay to commit already
				 * committed memory).
				 */


/*
 * Local functions.
 */

static void init(void);
static Region try(char* start);
static int probe(LPVOID address, unsigned size);

/*
 * Works like the Unix sbrk() function.
 *
 * Note: Decrementing the break pointer is *not* allowed.
 *
 * Results: The previous value for the break pointer if the
 * increment operation was successful, (void *) -1 if any error
 * occurred.
 */

void*
sbrk(incr)
int incr;			/* Bytes to increment the break address. */
{
  void* rval;			/* Value to return. */

  if (current == NULL)		/* First-time initalization. */
    init();

  if (incr && VirtualAlloc(current, incr, MEM_COMMIT, PAGE_READWRITE) == NULL)
      return (void *) -1L;

  rval = current;
  current += incr;
  return rval;
}

/*
 * Searches for the largest continous region of address space below
 * the 256 Mb limit.
 */

static void
init(void)
{
#define MBYTES(m) (m*1024*1024)
    static DWORD sizes[] =
    {
	MBYTES(248), MBYTES(224), MBYTES(196), MBYTES(148),
	MBYTES(128), MBYTES(96), MBYTES(80),
	MBYTES(64), MBYTES(48), MBYTES(40), MBYTES(32), MBYTES(24),
    };
    char *address;
    DWORD size;
    int i;

    for (i = 0; i < sizeof(sizes)/sizeof(sizes[0]); i++) {
	size = sizes[i];
	for (address = (char *) 0x40000; address+size < START_UNUSABLE;
	     address += SEGMENT) {
	    if (VirtualAlloc(address, size, MEM_RESERVE, PAGE_READWRITE)) {
		current = address;
		return;
	    }
	}
    }
    ABORT("win_sbrk: Failed to find any free memory below 256 Mb.\n");
}
