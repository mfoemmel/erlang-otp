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

#ifdef __cplusplus
extern "C" {
#endif


#ifdef __STDC__
extern ETERM *erl_decode(unsigned char*);
extern ETERM *erl_decode_buf(unsigned char**);
extern int erl_encode(ETERM*,unsigned char*t);
extern int erl_encode_buf(ETERM*,unsigned char**);
extern int erl_term_len(ETERM*);
extern unsigned char *erl_peek_ext(unsigned char*,int);
extern char erl_ext_type(unsigned char*);

extern int erl_ext_size(unsigned char*);
extern int erl_compare_ext(unsigned char*, unsigned char*);
int erl_verify_magic(unsigned char*);
#else
extern ETERM *erl_decode();
extern ETERM *erl_decode_buf();
extern int erl_encode();
extern int erl_term_len();
extern int erl_encode_buf();
extern unsigned char *erl_peek_ext();
extern char erl_ext_type();
extern int erl_ext_size();
extern int erl_compare_ext();
int erl_verify_magic();
#endif

#ifdef __cplusplus
}
#endif

#endif
