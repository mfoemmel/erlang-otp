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
#ifndef EIREG_H
#define EIREG_H

#include "eihash.h"

#ifdef __cplusplus
extern "C" {
#endif

#define EI_MNESIA_MODULE  "mnesia_registry"

#define EI_MNESIA_DUMP    "start_dump"
#define EI_MNESIA_WRITE   "write"
#define EI_MNESIA_DELETE  "delete"
#define EI_MNESIA_COMMIT  "commit"

#define EI_MNESIA_RESTORE "start_restore"
#define EI_MNESIA_SEND    "send_records"
#define EI_MNESIA_RECV    "restore"
#define EI_MNESIA_SIZE    "size"

/* object attributes */
#define EI_DIRTY 0x01 /* dirty bit (object value differs from backup) */
#define EI_DELET 0x02 /* object is deleted */
#define EI_INT 0x10 /* object is an integer */
#define EI_FLT 0x20 /* object is a float */
#define EI_STR 0x40 /* object is a string */
#define EI_BIN 0x80 /* object is a binary, i.e. pointer to arbitrary type */

typedef struct ei_reg_inode {
  int attr; 
  int size;
  union {
    long i;   
    double f;
    char *s;
    void *p;
  } val;
  struct ei_reg_inode *next;
} ei_reg_obj;

typedef struct {
  ei_reg_obj *freelist;
  ei_hash *tab;
} ei_reg;

#define EI_REG_TYPEMASK 0xf8 /* all but lowest bits */
#define ei_reg_typeof(r) (r->attr & EI_REG_TYPEMASK) 

/* open / close registry. On open, a descriptor is returned that must
 * be specified in all subsequent calls to registry functions. You can
 * open as many registries as you like.
 */
extern ei_reg *ei_reg_open(int size);
extern int ei_reg_resize(ei_reg *oldreg, int newsize);
extern int ei_reg_close(ei_reg *reg);

/* get information about an object */
struct ei_reg_stat {
  int attr;             /* object attributes (see above) */
  int size;             /* size in bytes (for STR and BIN) 0 for others */
};
extern int ei_reg_stat(ei_reg *reg, const char *key, struct ei_reg_stat *obuf);

struct ei_reg_tabstat {
  int size;   /* size of table */
  int nelem; /* number of stored elements */
  int npos;   /* number of occupied positions */
  int collisions; /* number of positions with more than one element */
};

/* get information about table */
extern int ei_reg_tabstat(ei_reg *reg, struct ei_reg_tabstat *obuf);

/* get value of any type object (must specify) 
 * Retrieve a value from an object. The type of value expected and a
 * pointer to a large enough buffer must be provided. flags must be
 * set to the appropriate type (see type constants above) and the
 * object type must match. If (flags == 0) the pointer is *assumed* to
 * be of the correct type for the object. In any case, the actual
 * object type is always returned on success.
 *
 * The argument following flags must be one of int*, double*, const
 * char** and const void**. 
 *
 * for BIN objects an int* is needed to return the size of the object, i.e.
 * extern int ei_reg_getval(ei_reg *reg, const char *path, int flags, void **p, int *size);
 */
extern int ei_reg_getval(ei_reg *reg, const char *key, int flags, ...);

/* get value of specific type object */
/* warning: it may be difficult to detect errors when using these
 * functions, since the error values are returned "in band"
 */
extern long ei_reg_getival(ei_reg *reg, const char *key);
extern double ei_reg_getfval(ei_reg *reg, const char *key);
extern const char *ei_reg_getsval(ei_reg *reg, const char *key);
extern const void *ei_reg_getpval(ei_reg *reg, const char *key, int *size);

/* set values... these routines assign values to keys. If the key
 * exists, the previous value is discarded and the new one replaces
 * it.
 *
 * BIN objects require an additional argument indicating the size in
 * bytes of the stored object. This will be used when the object is
 * backed up, since it will need to be copied at that time. Remember
 * also that pointers are process-space specific and it is not
 * meaningful to back them up for later recall. If you are storing
 * binary objects for backup, make sure that they are self-contained
 * (without references to other objects).
 *
 * On success the function returns 0, otherwise a value
 * indicating the reason for failure will be returned.
 */
extern int ei_reg_setival(ei_reg *reg, const char *key, long i);
extern int ei_reg_setfval(ei_reg *reg, const char *key, double f);
extern int ei_reg_setsval(ei_reg *reg, const char *key, const char *s);
extern int ei_reg_setpval(ei_reg *reg, const char *key, const void *p, int size);

/* general set function (specifiy type via flags)
 * optional arguments are as for equivalent type-specific function,
 * i.e.:
 * ei_reg_setval(fd, path, EI_INT, int i);
 * ei_reg_setval(fd, path, EI_FLT, float f);
 * ei_reg_setval(fd, path, EI_STR, const char *s);
 * ei_reg_setval(fd, path, EI_BIN, const void *p, int size);
 */
extern int ei_reg_setval(ei_reg *reg, const char *key, int flags, ...);

/* mark the object as dirty. Normally this operation will not be
 * necessary, as it is done automatically by all of the above 'set'
 * functions. However, if you modify the contents of an object pointed
 * to by a STR or BIN object, then the registry will not be aware of
 * the change. As a result, the object may be missed on a subsequent
 * backup operation. Use this function to set the dirty bit on the
 * object.
 */
extern int ei_reg_markdirty(ei_reg *reg, const char *key);

/* remove objects. The value, if any, is discarded. For STR and BIN
 * objects, the object itself is removed using free(). */
extern int ei_reg_delete(ei_reg *reg, const char *key);

/* dump to / restore from backup */
/* fd is open descriptor to Erlang, mntab is Mnesia table name */
/* flags here: */
#define EI_FORCE 0x1 /* dump all records (not just dirty ones) */
#define EI_NOPURGE 0x2 /* don't purge deleted records */
extern int ei_reg_dump(int fd, ei_reg *reg, const char *mntab, int flags);
extern int ei_reg_restore(int fd, ei_reg *reg, const char *mntab);
extern int ei_reg_purge(ei_reg *reg);

#ifdef __cplusplus
}
#endif

#endif
