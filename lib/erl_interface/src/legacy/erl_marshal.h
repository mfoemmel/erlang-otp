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
#ifndef _ERL_MARSHALL_H
#define _ERL_MARSHALL_H

#include "erl_eterm.h"		/* FIXME don't want to include this here */

/* FIXME: not documented, may be internal */
int erl_verify_magic(unsigned char*);
void erl_init_marshal(void);
int erl_encode_it(ETERM *ep, unsigned char **ext, int dist);

#endif /* _ERL_MARSHALL_H */
