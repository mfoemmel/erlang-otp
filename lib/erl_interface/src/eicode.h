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
#ifndef EICODE_H
#define EICODE_H

#ifdef __cplusplus
extern "C" {
#endif

#ifndef MAXATOMLEN
#define MAXATOMLEN 255
#endif

/* a pid */
typedef struct {
  char node[MAXATOMLEN+1];
  unsigned int num;
  unsigned int serial;
  unsigned int creation;
} erlang_pid;

/* a port */
typedef struct {
  char node[MAXATOMLEN+1];
  unsigned int id;
  unsigned int creation;
} erlang_port;

/* a ref */
typedef struct {
  char node[MAXATOMLEN+1];
  int len;
  unsigned int n[3];
  unsigned int creation;
} erlang_ref;

/* a trace token */
typedef struct {
  long serial;
  long prev;
  erlang_pid from;
  long label;
  long flags;
} erlang_trace;

/* a message */
typedef struct {
  long msgtype;
  erlang_pid from;
  erlang_pid to;
  char toname[MAXATOMLEN+1];
  char cookie[MAXATOMLEN+1];
  erlang_trace token;
} erlang_msg;

/*
 * The following functions are used to encode from c native types directly into
 * Erlang external format. To use them, you need
 *
 * - a destination buffer
 * - an index counter
 * - some data
 * - an idea of how you want to represent the data as an Erlang term.
 *
 * You can encode exactly one (1) term into the buffer if you are
 * going to transmit it to Erlang. Do the following:
 *
 * 1. Set your index to 0
 * 2. Encode the version into the buffer: ei_encode_version(buf,&index);
 *    The function has now advanced index so the next item can be encoded.
 * 3. Encode your term:
 *
 * Encoding non-compound types (i.e. not lists or tuples) is
 * straightforward. Just do it!
 *
 * Encoding tuples is done by first encoding the tuple header (it
 * contains the arity) and then encoding the tuple elements in
 * sequence.
 *
 * Encoding lists is done by first encoding the list header (it
 * contains the arity) and then encoding the list elements in
 * sequence, and finally encoding an empty list. 
 *
 * After all this, the index counter will tell you how much buffer you
 * used. If you really need to know in advance how big the buffer
 * should be, go through the same steps but with a NULL buffer. No
 * attempt will be made to modify the buffer, but index will be
 * updated as though you really did encode something.
 */
 
/* encode the given object into buf[index] as 'type'. 0 is
 * returned and index is updated to the position for the next item. if
 * buf == NULL, no data is actually copied, but index is updated to
 * indicate the number of bytes that would have been necessary.
 */
extern int ei_encode_version(char *buf, int *index);
extern int ei_encode_long(char *buf, int *index, long p);
extern int ei_encode_ulong(char *buf, int *index, unsigned long p);
extern int ei_encode_double(char *buf, int *index, double p);
extern int ei_encode_boolean(char *buf, int *index, int p);
extern int ei_encode_char(char *buf, int *index, char p);
extern int ei_encode_string(char *buf, int *index, const char *p);
extern int ei_encode_atom(char *buf, int *index, const char *p);
extern int ei_encode_binary(char *buf, int *index, const void *p, long len);
extern int ei_encode_pid(char *buf, int *index, const erlang_pid *p);
extern int ei_encode_port(char *buf, int *index, const erlang_port *p);
extern int ei_encode_ref(char *buf, int *index, const erlang_ref *p);
extern int ei_encode_term(char *buf, int *index, void *t); /* ETERM* actually */
extern int ei_encode_trace(char *buf, int *index, const erlang_trace *p);
extern int ei_encode_tuple_header(char *buf, int *index, int arity);
extern int ei_encode_list_header(char *buf, int *index, int arity);
#define ei_encode_empty_list(buf,i) ei_encode_list_header(buf,i,0)

/* Step through buffer, decoding the given type into the buffer
 * provided. On success, 0 is returned and index is updated to point
 * to the start of the next item in the buffer. If the type of item at
 * buf[index] is not the requested type, -1 is returned and index is
 * not updated. The buffer provided by the caller must be sufficiently
 * large to contain the decoded object.
 */
extern int ei_decode_version(const char *buf, int *index, int *version);
extern int ei_decode_long(const char *buf, int *index, long *p);
extern int ei_decode_ulong(const char *buf, int *index, unsigned long *p);
extern int ei_decode_double(const char *buf, int *index, double *p);
extern int ei_decode_boolean(const char *buf, int *index, int *p);
extern int ei_decode_char(const char *buf, int *index, char *p);
extern int ei_decode_string(const char *buf, int *index, char *p);
extern int ei_decode_atom(const char *buf, int *index, char *p);
extern int ei_decode_binary(const char *buf, int *index, void *p, long *len);
extern int ei_decode_pid(const char *buf, int *index, erlang_pid *p);
extern int ei_decode_port(const char *buf, int *index, erlang_port *p);
extern int ei_decode_ref(const char *buf, int *index, erlang_ref *p);
extern int ei_decode_term(const char *buf, int *index, void *t); /* ETERM** actually */
extern int ei_decode_trace(const char *buf, int *index, erlang_trace *p);
extern int ei_decode_tuple_header(const char *buf, int *index, int *arity);
extern int ei_decode_list_header(const char *buf, int *index, int *arity);

/* decode a list of integers into an integer array (i.e. even if it is
 * encoded as a string). count gets number of items in array
 */
extern int ei_decode_intlist(const char *buf, int *index, long *a, int *count);

/* returns the type and "size" of the item at buf[index]. For strings
 * and atoms, size is the number of characters not including the
 * terminating 0. For binaries, size is the number of bytes. For lists
 * and tuples, size is the arity of the object. For other types, size
 * is 0. In all cases, index is left unchanged. */
extern int ei_get_type(const char *buf, const int *index, int *type, int *size);

#ifdef __cplusplus
}
#endif

#endif


