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
#ifndef EIHASH_H
#define EIHASH_H

#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

#define EI_SMALLKEY 32

typedef struct bucket_s {
  int rawhash;
  const char *key;
  char keybuf[EI_SMALLKEY];
  const void *value;
  struct bucket_s *next;
} ei_bucket;

/* users of the package declare variables as pointers to this. */
typedef struct {
  ei_bucket **tab;
  int (*hash)(const char *); /* hash function for this table */
  int size; /* size of table */
  int nelem; /* nr elements */
  int npos;  /* nr occupied positions */
  ei_bucket *freelist; /* reuseable freed buckets */
} ei_hash;

#define ei_hash_size(tab) ((tab)->size)
#define ei_hash_count(tab) ((tab)->count)

#define ALIGN_QUAD 0x7 
#define ei_align(size) while (((unsigned)size) & ALIGN_QUAD) (size)++

extern int ei_isprime(int n);
extern int ei_dohash(const char *key);
extern void *ei_hash_lookup(ei_hash *tab, const char *key);
extern const char *ei_hash_rlookup(ei_hash *tab, const void *value);
extern int ei_hash_foreach(ei_hash *tab, int (*f)(const char *key, const void *value));
extern void *ei_hash_insert(ei_hash *tab, const char *key, const void *value);
extern void *ei_hash_remove(ei_hash *tab, const char *key);
extern ei_hash *ei_hash_newtab(int tabsize);
extern ei_hash *ei_hash_resize(ei_hash *oldtab, int newsize);
extern int ei_hash_freetab(ei_hash *tab, void (*f)(void *));
extern void ei_hash_stats(ei_hash *tab, FILE *out);


#ifdef __cplusplus
}
#endif

#endif
