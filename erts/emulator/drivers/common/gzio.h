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
 * This file contains the functions added to the gzio driver
 * and that was not in the original zlib distribution
 */

/* XXX: Sounds like an impossible operation to me... /kgb */
extern int gzseekk(gzFile, int, int);
extern ErlDrvBinary* gzinflate_buffer(char*, int);
extern ErlDrvBinary* gzdeflate_buffer(char*, int);


