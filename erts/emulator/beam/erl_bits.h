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

#define erts_InitMatchBuf(Src, Fail)				\
do {								\
    Eterm _Bin = (Src);						\
    if (!is_binary(_Bin)) {					\
	Fail;							\
    } else {							\
	Eterm _orig;						\
	Uint _offs;						\
								\
	GET_REAL_BIN(_Bin, _orig, _offs);			\
	erts_mb.orig = _orig;					\
	erts_mb.base = binary_bytes(_orig);			\
	erts_mb.offset = 8 * _offs;				\
	erts_mb.size = binary_size(_Bin) * 8 + erts_mb.offset;	\
    }								\
} while (0)

void erts_init_bits(void);	/* Initialization once. */

/*
 * Binary matching.
 */

extern ErlBinMatchBuffer erts_mb;
extern ErlBinMatchBuffer erts_save_mb[MAX_REG];

int erts_bs_start_match(Eterm Bin);
int erts_bs_skip_bits(Uint num_bits);
int erts_bs_skip_bits_all(void);
int erts_bs_test_tail(Uint num_bits);
void erts_bs_save(int index);
void erts_bs_restore(int index);
Eterm erts_bs_get_integer(Process *p, Uint num_bits, unsigned flags);
Eterm erts_bs_get_binary(Process *p, Uint num_bits, unsigned flags);
Eterm erts_bs_get_float(Process *p, Uint num_bits, unsigned flags);
Eterm erts_bs_get_binary_all(Process *p);

/*
 * Binary construction.
 */

extern byte* erts_bin_buf;
extern unsigned erts_bin_buf_len;
extern unsigned erts_bin_offset;

void erts_bs_init(void);
Eterm erts_bs_final(Process* p);

int erts_bs_put_integer(Eterm Integer, Uint num_bits, unsigned flags);
int erts_bs_put_binary(Eterm Bin, Uint num_bits);
int erts_bs_put_binary_all(Eterm Bin);
int erts_bs_put_float(Eterm Float, Uint num_bits, int flags);
void erts_bs_put_string(byte* iptr, Uint num_bytes);

/*
 * Flags for bs_get_* instructions.
 */

#define BSF_ALIGNED 1		/* Field is guaranteed to be byte-aligned. */
#define BSF_LITTLE 2		/* Field is little-endian (otherwise big-endian). */
#define BSF_SIGNED 4		/* Field is signed (otherwise unsigned). */
#define BSF_EXACT 8		/* Size in bs_init is exact. */
#define BSF_NATIVE 16		/* Native endian. */
