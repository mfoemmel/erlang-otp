/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2008-2009. All Rights Reserved.
 * 
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * %CopyrightEnd% 
 */

#include <stdio.h>

/* Platform specific initialisation stuff */ 
#ifdef _MACOSX

#include "wxe_macosx.h"
#include <Cocoa/Cocoa.h>
#include <objc/objc-runtime.h>

void * wxe_ps_init() 
{
   /* COCOA */
   CPSProcessSerNum PSN;
   OSErr err;
   void *rel_pool;

   rel_pool = [[NSAutoreleasePool alloc] init];
   [NSApplication sharedApplication];

/*    objc_msgSend(objc_msgSend(objc_getClass("NSAutoreleasePool"),  */
/* 			     @selector(alloc)), @selector(init)); */
 
/*    objc_msgSend(objc_getClass("NSApplication"), */
/* 		@selector(sharedApplication)); */

   /* To get a Menu & a dock icon : */
   assert(!(err = CPSGetCurrentProcess(&PSN)));
   assert(!(err = CPSSetProcessName(&PSN,"Erlang")));
   assert(!(err = CPSEnableForegroundOperation(&PSN,0x03,0x3C,0x2C,0x1103)));
   assert(!(err = CPSSetFrontProcess(&PSN)));

   return rel_pool;
}
/* _MACOSX */
#else
void * wxe_ps_init() 
{
   return (void *) 0;
}
#endif 
