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

/**
 **
   This file is compiled into a lib which must be included when
   linking dynaimc drivers and port programs.
 **
 **/

#include "ose.h"
#include "dbgprintf.h"
#include "stdio.h"
#include "string.h"
#include "efs.h"
#include "outfmt.h"

/*************************** FOR DEBUGGING ***************************/
/* use this function to print both to standard out and to 
   debug printout driver (see Makefile for com port config) */

char dbg_print_buf[4096];

int erl_dbg_fputc(char ch, FILE* stream) {
  dbgprintf("%c", ch);
  return fputc(ch, stdout);
}

int erl_dbg_vfprintf(FILE* stream, char* format, va_list args) {
  int r;
  r = vsnprintf(dbg_print_buf, sizeof(dbg_print_buf), format, args);
  dbgprintf(dbg_print_buf);	/* print to driver */
  dbgprintf("\r");
  fprintf(stream, dbg_print_buf); /* print to stream */
  return r;
}

int erl_dbg_fprintf(FILE* stream, char* format, ...) { 
  int r;
  va_list va;
  va_start(va, format);
  r = erl_dbg_vfprintf(stream, format, va);
  va_end(va);
  return r;
}

