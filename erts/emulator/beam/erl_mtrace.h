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

#ifndef ERL_MTRACE_H__
#define ERL_MTRACE_H__

#define ERTS_MT_MAJOR_VSN		(1)
#define ERTS_MT_MINOR_VSN		(0)

/* Trace flags */

#define ERTS_MT_64_BIT_FLAG		(1 << 0)

/* Entry tags */

#define ERTS_MT_START_TAG		(0)
#define ERTS_MT_ALLOCATOR_TAG		(1)
#define ERTS_MT_BLOCK_TYPE_TAG		(2)
#define ERTS_MT_ALLOC_TAG		(3)
#define ERTS_MT_REALLOC_NPB_TAG		(4)
#define ERTS_MT_REALLOC_MV_TAG		(5)
#define ERTS_MT_REALLOC_NMV_TAG		(6)
#define ERTS_MT_FREE_TAG		(7)
#define ERTS_MT_TIME_INC_TAG		(8)
#define ERTS_MT_STOP_TAG		(9)
#define ERTS_MT_EXIT_TAG		(10)

#define ERTS_MT_START_WORD		(0xfff04711)
/* Entry header fields */

#define ERTS_MT_UI8_MSB_EHDR_FLD_SZ	(0)
#define ERTS_MT_UI16_MSB_EHDR_FLD_SZ	(1)
#define ERTS_MT_UI32_MSB_EHDR_FLD_SZ	(2)
#define ERTS_MT_UI64_MSB_EHDR_FLD_SZ	(3)
#define ERTS_MT_UI_MSB_EHDR_FLD_SZ	ERTS_MT_UI64_MSB_EHDR_FLD_SZ
#define ERTS_MT_TAG_EHDR_FLD_SZ	(4)

#define ERTS_MT_UI8_MSB_EHDR_FLD_MSK	((1 << ERTS_MT_UI8_MSB_EHDR_FLD_SZ)-1)
#define ERTS_MT_UI16_MSB_EHDR_FLD_MSK	((1 << ERTS_MT_UI16_MSB_EHDR_FLD_SZ)-1)
#define ERTS_MT_UI32_MSB_EHDR_FLD_MSK	((1 << ERTS_MT_UI32_MSB_EHDR_FLD_SZ)-1)
#define ERTS_MT_UI64_MSB_EHDR_FLD_MSK	((1 << ERTS_MT_UI64_MSB_EHDR_FLD_SZ)-1)
#define ERTS_MT_UI_MSB_EHDR_FLD_MSK	ERTS_MT_UI64_MSB_EHDR_FLD_MSK
#define ERTS_MT_TAG_EHDR_FLD_MSK	((1 << ERTS_MT_TAG_EHDR_FLD_SZ)-1)

/* Time increment word */
#define ERTS_MT_TIME_INC_SECS_SHIFT	20
#define ERTS_MT_TIME_INC_USECS_SHIFT	0

#define ERTS_MT_TIME_INC_SECS_MASK	((1 << 12) - 1)
#define ERTS_MT_TIME_INC_USECS_MASK	((1 << 20) - 1)


#define ERTS_MT_MAX_HEADER_ENTRY_SIZE (2 + 2 + 1 + 255)
/* Largest header entry is block type entry (ERTS_MT_BLOCK_TYPE_TAG) */
#define ERTS_MT_MAX_BODY_ENTRY_SIZE   (2 + 8 + 8 + 8 + 4)
/* Largest body entry is realloc moved entry (ERTS_MT_REALLOC_MV_TAG) */

/*
 *
 * Entry header:
 *
 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 * |         ... |MSB2|MSB1|  Tag  |
 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *
 * Time inc entry field:
 *
 * 31               23                                            0
 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 * |     Seconds   |   Micro Seconds                               |
 * +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 *
 * alloc entry:
 *
 * N             1        2          3          4
 * MSB          1-0     7|3-0      7|3-0       3-0
 * SZ      2    2-1     8|4-1      8|4-1       4-1
 * UIT    UI16  UI16  UI64|UI32  UI64|UI32    UI32
 *      +-----+...--+...-------+...-------+...-------+
 *      | Hdr | Type| Out ptr  | In size  | Time inc |
 *      +-----+...--+...-------+...-------+...-------+
 *
 *      realloc entry:
 *
 * N             1        2          3          4          5
 * MSB          1-0     7|3-0      7|3-0      7|3-0       3-0
 * SZ      2    2-1     8|4-1      8|4-1      8|4-1       4-1
 * UIT    UI16  UI16  UI64|UI32  UI64|UI32  UI64|UI32    UI32
 *      +-----+...--+...-------+...-------+...-------+...-------+
 *      | Hdr | Type| Out ptr  | In ptr   | In size  | Time inc |
 *      +-----+...--+...-------+...-------+...-------+...-------+
 *
 * free entry:
 *
 * N             1        2          3
 * MSB          1-0     7|3-0       3-0
 * SZ      2    2-1     8|4-1       4-1
 * UIT    UI16  UI16  UI64|UI32    UI32
 *      +-----+...--+...-------+...-------+
 *      | Hdr | Type| In ptr   | Time inc |
 *      +-----+...--+...-------+...-------+
 *
 *
 */

#ifdef ERL_ALLOC_H__
extern int erts_mtrace_enabled;

void erts_mtrace_init(char *receiver);
void erts_mtrace_stop(void);
void erts_mtrace_exit(Uint32 exit_value);

#endif /* #ifdef ERL_ALLOC_H__ */
#endif /* #ifndef ERL_MTRACE_H__ */

