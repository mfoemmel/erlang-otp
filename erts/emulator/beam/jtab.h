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
** Macros for direct jumps
*/
#ifndef __JTAB_H__
#define __JTAB_H__

#if defined(__GNUC__) && !defined(NO_JUMP_TABLE) && \
   !defined(QUANTIFY) && !defined(PURIFY)

#define DECL_JTABLE(T,size)       static void* JTAB__##T[size]; \
                                  static int JTAB__##T##__init = 0;
#define DECL_JVALUE(T,value)      __label__ JVAL__##T##__##value;
#define JUMP(T,x)                 goto *JTAB__##T[x]
#define DEFINE_LOCATION(T,value)  JTAB__##T[value] = &&JVAL__##T##__##value
#define LOCATION(T,value)         JVAL__##T##__##value##:
#define JUMP_END
#define JTABLE_NEED_INIT(T)       !JTAB__##T##__init
#define DEFINE_JTABLE(T)          JTAB__##T##__init = 1

#else

#define DECL_JVALUE(T,value)
#define DECL_JTABLE(T,size)
#define DEFINE_LOCATION(T,value)
#define JUMP(vec, x)         switch(x) { default:
#define LOCATION(T,value)    case (value):
#define JUMP_END             }
#define JTABLE_NEED_INIT(T)  0
#define DEFINE_JTABLE(T)

#endif

#endif

