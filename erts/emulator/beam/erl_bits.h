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
 * This structure represents a binary to be matched.
 */

typedef struct erl_bin_match_buffer {
    Eterm orig;			/* Original binary term. */
    byte* base;			/* Current position in binary. */
    unsigned offset;		/* Offset in bits. */
    size_t size;		/* Size of binary in bits. */
} ErlBinMatchBuffer;

#define erts_InitMatchBuf(Mb, Src, Fail)			\
do {								\
    Eterm Bin = (Src);						\
    if (!is_binary(Bin)) {					\
	Fail;							\
    } else {							\
	Eterm _orig;						\
	Uint _offs;						\
	GET_REAL_BIN(Bin, _orig, _offs);			\
	(Mb)->orig = _orig;					\
	(Mb)->base = binary_bytes(_orig);			\
	(Mb)->offset = 8 * _offs;				\
	(Mb)->size = binary_size(Bin) * 8 + (Mb)->offset;	\
    }								\
} while (0)

Eterm erts_get_integer(Process *p, ErlBinMatchBuffer* mb, int size, unsigned flags);
Eterm erts_get_binary(Process *p, ErlBinMatchBuffer* mb, int size, unsigned flags);
Eterm erts_get_float(Process *p, ErlBinMatchBuffer* mb, int size, unsigned flags);
Eterm erts_get_binary_all(Process *p, ErlBinMatchBuffer* mb);


/*
 * Binary construction.
 */
extern byte* erts_bin_buf;
extern unsigned erts_bin_buf_len;
extern unsigned erts_bin_offset;

int erts_put_integer(Eterm arg, unsigned size, unsigned flags);
int erts_put_binary(Eterm arg, unsigned size);
int erts_put_binary_all(Eterm arg);
int erts_put_float(Eterm arg, Eterm size, int unit, int flags);
void erts_put_string(byte* iptr, size_t n);
void erts_init_bits(void);
/*
 * Flags for bs_get_* instructions.
 */

#define BSF_ALIGNED 1		/* Field is guaranteed to be byte-aligned. */
#define BSF_LITTLE 2		/* Field is little-endian (otherwise big-endian). */
#define BSF_SIGNED 4		/* Field is signed (otherwise unsigned). */
#define BSF_EXACT 8		/* Size in bs_init is exact. */

