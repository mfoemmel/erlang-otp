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
** Description: Reserves all free virtual memory address space above 256Mb so 
**              that we can safely assume that the 4 most significant bits in
**              any address in a malloced block of memory will be zero.
*/

/*
** use this to write a log file with a map of virtul memory space
** also runs a test function with lots of calls to malloc and free
*/
/* #define TEST_MALLOC */

#ifdef TEST_MALLOC
#define LOG(FP, FORMAT)  fprintf(FP, FORMAT)
#define LOG1(FP, TEXT, ARG1)  fprintf(FP, TEXT, ARG1)
#define LOG2(FP, TEXT, ARG1, ARG2)  fprintf(FP, TEXT, ARG1, ARG2)
#define LOG3(FP, TEXT, ARG1, ARG2, ARG3)  fprintf(FP, TEXT, ARG1, ARG2, ARG3)
#else
#define LOG(FP, FORMAT)  
#define LOG1(FP, TEXT, ARG1)
#define LOG2(FP, TEXT, ARG1, ARG2)
#define LOG3(FP, TEXT, ARG1, ARG2, ARG3)
#endif

/*
** test_malloc
*/
#include <windows.h>
#include <stdio.h>
#include <assert.h>
#include "sys.h"
#include "erl_vm.h"
#ifdef DEBUG
#  include "global.h"
#endif

#ifdef TEST_MALLOC
int test_malloc(FILE *fp);
#endif

#ifdef ASSERT
#undef ASSERT
#endif
#define ASSERT assert

void VERIFY(BOOL bTrue);
void MapVirtualMemory(FILE *fp);

int init_malloc()
{

  PVOID   pAddress;
  const PVOID maxErlangAddress = (PVOID)0x40000000;
  DWORD	  size;
  int	  i = -1;
  BOOL    bSuccess = TRUE;
#ifdef TEST_MALLOC
  FILE    *fp;
#endif

#ifdef TEST_MALLOC
  fp = fopen( "C:\\TEMP\\LOG.TXT", "w+");
#endif
  /* SetConsoleScreenBufferSize(GetStdHandle(STD_OUTPUT_HANDLE), crd);*/
  pAddress = maxErlangAddress;

  /* let's compact the heap first...hopefully, it will reduce the number of */
  /* reserved / committed regions that exist above 256Mb*/
  if (!HeapCompact(GetProcessHeap(), 0))
    LOG1(fp, "HeapCompact() failed.  Reason: %s\n", last_error());

  /* just reserve memory address space until we get down to the valid*/
  /* Erlang address space.  We start by reserving biggish blocks and then reserve*/
  /* progressively smaller ones, hopefully plugging any gaps as we go*/
  size = 4 * 1024 * 1024;
  while (size >= 4096) {
	do {
	  pAddress = VirtualAlloc(NULL, size, MEM_TOP_DOWN | MEM_RESERVE, PAGE_NOACCESS);
	} while (pAddress && pAddress > maxErlangAddress);
    if (!pAddress) {
	  LOG1(fp, "***Virtual Alloc failed!! Reason: %s", win32_errorstr(GetLastError()));
	  return 0;
	}
        /* else free up the last reserved chunk if it's below the threshold*/
	else if (((UINT)pAddress) < (UINT)maxErlangAddress)
	  VirtualFree(pAddress, 0, MEM_RELEASE);
	size >>= 1;
  }

#ifdef DEBUG
  {
      char buff[1024];
      if (GetEnvironmentVariable("ERL_DEBUG_RESERVE_MEM",buff,1024)) {
	  unsigned long howmuch = strtoul(buff, NULL, 0);
	  if (howmuch != ULONG_MAX && howmuch != 0UL) {
	      unsigned long where = (howmuch >> 16) << 16;
	      unsigned long to = howmuch;
	      void *waste;
	      if (where == to) 
		  where -= 0x10000;
	      erts_printf("Trying to reserve memory below 0x%08x.\n",
			 howmuch);
	      erts_printf("maxErlangAddress = 0x%08x.\n", (unsigned long) 
			 maxErlangAddress);
	      for (;;) {
		  if (VirtualAlloc((void *) where, to - where, 
				   MEM_RESERVE, PAGE_NOACCESS) == NULL) {
		      erts_printf( "Reservation stopped at 0x%08x.\n",
				 where);
		      break;
		  }
		  to = where;
		  where -= 0x10000;
	      }
	      erts_printf("Mallocing the rest...\n");
	      while ((unsigned long) (waste = malloc(1024)) < howmuch)
		  ;
	      free(waste);
	  }
      }
  }
#endif
	      

#ifdef TEST_MALLOC
  MapVirtualMemory(fp);
  LOG(fp, "testing malloc...\n");
  bSuccess = test_malloc(fp);
  fclose(fp);
#endif  /* TEST_MALLOC*/

  return bSuccess;
}

/* VERIFY differs from assert in that the expression will still be evaluated for non-debug builds*/
void VERIFY(BOOL bTrue)
{
  ASSERT(bTrue);
}

#ifdef TEST_MALLOC
#include <time.h>

/* function to test malloc*/
int test_malloc(FILE *fp) 
{
  MEMORYSTATUS ms;
  const UINT maxblocks = 2000;
  const UINT maxblocksize = 8 * 1024 * 1024; /* 8 Mb max size of a single block*/
  UINT address, size, maxsize, total, blocks;
  void **pptrs, *pTable, *pv;
  int i, j;
  BOOL bFailed = FALSE;

  pptrs = (PVOID *)malloc(maxblocks * sizeof(PVOID));

  ms.dwLength = sizeof(MEMORYSTATUS);
  GlobalMemoryStatus(&ms);
  srand((unsigned)time(NULL));
  /* a bit of leeway...we aren't interested in running the system out of memory!!*/
  /* ms.dwAvailPhys -= 512 * 1024;*/

  LOG(fp, "Allocating blocks.  Sizes: \n");
  for (i = 0; i < 4 && !bFailed; i++) {
	maxsize = 4096 + (maxblocksize - 4096) * i / 3;
	j = 0; blocks = 0; total = 0;
	while (blocks < maxblocks) {
	  size = (rand() * rand()) % maxsize;
	  while (total + size > ms.dwAvailPhys && size > 4096)
		size /= 2;
	  if (total + size > ms.dwAvailPhys)
		break; 
	  if ((pv = malloc(size)) == NULL) {
		LOG1(fp, "malloc failed to alloc %u bytes\n", size);
		break;
	  }
	  total += (size = _msize(pv));
	  /* every so often, free a previously alloced block...*/
	  if ((j++ % 4) == 0 && blocks >= 4) {
		int index;
		index = rand() % blocks;
		total -= _msize(pptrs[index]);
		pptrs[index] = pv;
	  }
	  else
	    pptrs[blocks++] = pv;
	  LOG1(fp, "%7u ", size);
	  if ((blocks % 10) == 0)
		LOG(fp, "\n");
	  /* check the address to make sure that none of the top four bits are set...*/
	  address = (UINT)pv;
	  if (((address + size) & 0xC0000000)) 
	      {
	    LOG2(fp, "\n***Illegal memory block!! Addr = %X, size = %u\n", address, size);
		bFailed = TRUE;
		break;
	  }
	}
	LOG2(fp, "\nTotal of %u bytes alloced in %u separate blocks.\nFreeing...\n", total, blocks);
	while (blocks--)
	  free(pptrs[blocks]);
  }
  free(pptrs);
  return 1;
}

/* creates a map of a snapshot of the virtual memory address space and appends it to the*/
/* specified text file*/
void MapVirtualMemory(FILE *fp)
{
  SYSTEM_INFO  sinfo;
  MEMORY_BASIC_INFORMATION mbi;
  PVOID   pAddress, pEndAddress;
  DWORD	  size;
  int	  j, end, i = -1;
  DWORD   resSpace = 0, resBlocks = 0;
  DWORD   comSpace = 0, comBlocks = 0;
  DWORD   freeSpace = 0, freeBlocks = 0;
  DWORD   biggestFreeMemBlock = 0, addr;
  const int linelen = 80;
  char    line[80 + 1], ch;

  GetSystemInfo(&sinfo);
  i = 0;
  pAddress = sinfo.lpMinimumApplicationAddress;
  sinfo.dwAllocationGranularity = 4096;
  addr = (UINT)pAddress;
  LOG1(fp, "Process Id = 0x%08X\n", _getpid());
  LOG1(fp, "Process heap handle = 0x%8X\n", GetProcessHeap());
  LOG1(fp, "Virtual memory map start address = %X\n", addr);
  LOG1(fp, "Each char = %u bytes (allocation granularity)\n", sinfo.dwAllocationGranularity);
  LOG(fp, "Key: _ = Free, C = Committed, R = Reserved, ? = Unknown\n");
  line[0] = '\0';
  line[linelen] = '\0';
  do {
    size = VirtualQuery(pAddress, &mbi, sizeof(MEMORY_BASIC_INFORMATION));

	switch(mbi.State) {
	case MEM_FREE:
	  freeSpace += mbi.RegionSize;
	  freeBlocks++;
	  if (mbi.RegionSize > biggestFreeMemBlock)
		biggestFreeMemBlock = mbi.RegionSize;
	  ch = '_';
	  break;
	case MEM_COMMIT:
	  comSpace += mbi.RegionSize;
	  comBlocks++;
	  ch = 'C';
	  break;
	case MEM_RESERVE:
	  resSpace += mbi.RegionSize;
	  resBlocks++;
	  ch = 'R';
	  break;
	default:
	  ch = '?';
	}
	/* fill in the right number of characters...*/
	end = i + mbi.RegionSize / sinfo.dwAllocationGranularity;
	for (j = i; j < end; j++) {
	  if (i && (j % linelen) == 0)
		LOG2(fp, "0x%08X  %s\n", addr - linelen * 4096, line);
	  line[j % linelen] = ch;
	  addr += 4096;
	}
	i = j;
	/* ASSERT((mbi.RegionSize % sinfo.dwAllocationGranularity) == 0);*/
	/* LOG(fp, "Type: %sBaseAddress: %8X Size: %8X\n", type, mbi.BaseAddress, mbi.RegionSize);*/
	pAddress = (VOID *)((BYTE *)mbi.BaseAddress + mbi.RegionSize);
  } while (pAddress < sinfo.lpMaximumApplicationAddress);
  /* output final line....*/
  line[i % linelen] = '\0';
  LOG2(fp, "0x%08X  %s\n", addr - (i % linelen) * 4096, line);
  
  LOG1(fp, "\nbiggest free memory block = %u\n", biggestFreeMemBlock / 0x100000);
  LOG2(fp, "%u blocks, total %u Mb reserved \n", resBlocks, resSpace / 0x100000);
  LOG2(fp, "%u blocks, total %u Mb free \n", freeBlocks, freeSpace / 0x100000);
  LOG2(fp, "%u blocks, total %u Mb committed \n", comBlocks, comSpace / 0x100000);
}

#endif  /* TEST_MALLOC*/

