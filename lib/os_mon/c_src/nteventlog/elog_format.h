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
#ifndef _ELOG_FORMAT_H
#define _ELOG_FORMAT_H
/*
 * Module: elog_format
 * Purpouse: Format messages in the eventlog.
 * ToDo: Maximum buffersize is used...
 */

#include "elog_global.h"

char *format_message(MessageFiles mf, DWORD id, 
		     char *strings, int numstrings, 
		     char *buff, int bufflen);
/*
 * Formats an eventlog message with the messagefiles
 * in mf, the ID id, the stringarray strings, 
 * containing numstrings strings into buff.
 * if bufflen is to small or anything else failes, 
 * the return value is NULL.
 */

#endif /* _ELOG_FORMAT_H */
