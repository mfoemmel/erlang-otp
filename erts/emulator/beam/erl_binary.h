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

#ifndef __ERL_BINARY_H
#define __ERL_BINARY_H

/*
 * Maximum number of bytes to place in a heap binary.
 */

#define ERL_ONHEAP_BIN_LIMIT 64

/*
 * This structure represents a SUB_BINARY.
 *
 * Note: The last field (orig) is not counted in arityval in the header.
 * This simplifies garbage collection.
 */

typedef struct erl_sub_bin {
    Eterm thing_word;		/* Subtag SUB_BINARY_SUBTAG. */
    Uint size;			/* Binary size in bytes. */
    Uint offs;			/* Offset into original binary. */
    Eterm orig;			/* Original binary (REFC or HEAP binary). */
} ErlSubBin;

#define ERL_SUB_BIN_SIZE (sizeof(ErlSubBin)/sizeof(Eterm))
#define HEADER_SUB_BIN	_make_header(ERL_SUB_BIN_SIZE-2,_TAG_HEADER_SUB_BIN)

/*
 * This structure represents a HEAP_BINARY.
 */

typedef struct erl_heap_bin {
    Eterm thing_word;		/* Subtag HEAP_BINARY_SUBTAG. */
    Uint size;			/* Binary size in bytes. */
    Eterm data[1];		/* The data in the binary. */
} ErlHeapBin;

#define heap_bin_size(num_bytes)		\
  (sizeof(ErlHeapBin)/sizeof(Eterm) - 1 +	\
   ((num_bytes)+sizeof(Eterm)-1)/sizeof(Eterm))

#define header_heap_bin(num_bytes) \
  _make_header(heap_bin_size(num_bytes)-1,_TAG_HEADER_HEAP_BIN)

/*
 * Get the size in bytes of any type of binary.
 */

#define binary_size(Bin) (binary_val(Bin)[1])

/*
 * Get the pointer to the actual data bytes in a binary.
 * Works for any type of binary.
 *
 * Bin: input variable (Eterm)
 * Bytep: output variable (byte *)
 */

#define GET_BINARY_BYTES(Bin,Bytep)					\
do {									\
    Eterm* _real_bin = binary_val(Bin);					\
    Uint _offs = 0;							\
    if (thing_subtag(*_real_bin) == SUB_BINARY_SUBTAG) {		\
	ErlSubBin* _sb = (ErlSubBin *) _real_bin;			\
	_offs = _sb->offs;						\
	_real_bin = binary_val(_sb->orig);				\
    }									\
    if (thing_subtag(*_real_bin) == REFC_BINARY_SUBTAG) {		\
	Bytep = ((ProcBin *) _real_bin)->bytes + _offs;			\
    } else {								\
	Bytep = (byte *)(&(((ErlHeapBin *) _real_bin)->data)) + _offs;	\
    }									\
} while (0)

/*
 * Get the real binary from any binary type, where "real" means
 * a REFC or HEAP binary. Also get the byte offset into the
 * real binary. Useful if you want to build a SUB binary from
 * any binary.
 *
 * Bin: Input variable (Eterm)
 * RealBin: Output variable (Eterm)
 * Offset: Output variable (Uint)
 */

#define GET_REAL_BIN(Bin, RealBin, Offset)			\
do {								\
    ErlSubBin* _sb = (ErlSubBin *) binary_val(Bin);		\
    Offset = 0;							\
    RealBin = Bin;							\
    if (thing_subtag(_sb->thing_word) == SUB_BINARY_SUBTAG) {	\
	Offset = _sb->offs;					\
	RealBin = _sb->orig;					\
    }								\
} while (0)

/*
 * Returns the pointer to the actual data bytes in a binary.
 * The binary must be a REFC or HEAP binary, not a SUB binary.
 */

#define binary_bytes(Bin)					\
((thing_subtag(*binary_val(Bin)) == REFC_BINARY_SUBTAG) ?	\
 ((ProcBin *) binary_val(Bin))->bytes :				\
 (byte *)(&(((ErlHeapBin *) binary_val(Bin))->data)))

#endif
