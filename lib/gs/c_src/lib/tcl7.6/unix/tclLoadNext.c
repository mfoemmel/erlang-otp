/* 
 * tclLoadNext.c --
 *
 *	This file implements the dlopen and dlsym APIs under the
 *	Next operating system, to enable the Tcl "load" command to
 *	work.
 *
 * Copyright (c) 1995 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tclLoadNext.c 1.4 96/02/15 11:58:55
 */
#include "tclInt.h"
#include "tclPort.h"
#include "../compat/dlfcn.h"
#include <mach-o/rld.h>
#include <streams/streams.h>

static char *errorMsg = (char *) NULL;

VOID *dlopen(path, mode)
    CONST char *path;
    int mode;
{
  struct mach_header *header;
  int len, maxlen;
  char *data;
  char *files[]={fileName,NULL};
  NXStream *errorStream=NXOpenMemory(0,0,NX_READWRITE);

  if (!errorMsg) {
    ckfree(errorMsg);
    errorMsg = (char *) NULL;
  }

  if(!rld_load(errorStream,&header,files,NULL)) {
    NXGetMemoryBuffer(errorStream,&data,&len,&maxlen);
    errorMsg = ckalloc(strlen(data)+1);
    strcpy(errorMsg, data);
    NXCloseMemory(errorStream,NX_FREEBUFFER);
    return (VOID *) NULL;
  }
  NXCloseMemory(errorStream,NX_FREEBUFFER);
  return (VOID *) 1;
}

VOID *dlsym(handle, symbol)
    VOID *handle;
    CONST char *symbol;
{   VOID *procPtr = (VOID *) NULL;

    rld_lookup(NULL,symbol,(unsigned long *)procPtr);
    return procPtr;
}

char *dlerror()
{
    return errorMsg;
}
