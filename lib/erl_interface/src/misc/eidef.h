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

#ifndef _EIDEF_H
#define _EIDEF_H

/* Common definitions used in ei user interface */

#include "config.h"		/* Central include of config.h */

#include <stddef.h>		/* We want to get definition of NULL */

#include "ei.h"			/* Want the API function declarations */

#define EISMALLBUF 2048

typedef unsigned char  uint8;	/* FIXME use configure */
typedef unsigned short uint16;
typedef unsigned int   uint32;
typedef signed   char  int8;
typedef signed   short int16;
typedef signed   int   int32;

#endif /* _EIDEF_H */
