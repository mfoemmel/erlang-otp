/* $Id$
 * hipe_bif0.h
 *
 * Compiler and linker support.
 */
#ifndef HIPE_BIF0_H
#define HIPE_BIF0_H

extern int hipe_patch_address(Uint *address, Eterm patchtype, Uint value);

extern Uint *hipe_bifs_find_pc_from_mfa(Eterm mfa);

/* shared with ggc.c -- NOT an official API */
extern Eterm *hipe_constants_start;
extern Eterm *hipe_constants_next;

/*
 * stack descriptor stuff (XXX: move to a file of its own)
 */

#include <stddef.h>	/* offsetof() */

struct sdesc {
    struct {
	unsigned long hvalue;	/* return address */
	struct sdesc *next;	/* hash collision chain */
    } bucket;
    unsigned int summary; /* frame size, exn handler presence flag, arity */
    unsigned int livebits[1]; /* size depends on arch & data in summary field */
};

struct sdesc_with_exnra {
    unsigned long exnra;
    struct sdesc sdesc;
};

static __inline__ unsigned int sdesc_fsize(const struct sdesc *sdesc)
{
    return sdesc->summary >> 9;
}

static __inline__ unsigned int sdesc_arity(const struct sdesc *sdesc)
{
    return sdesc->summary & 0xFF;
}

static __inline__ unsigned long sdesc_exnra(const struct sdesc *sdesc)
{
    if( (sdesc->summary & (1<<8)) ) {
	const char *tmp;
	tmp = (const char*)sdesc - offsetof(struct sdesc_with_exnra, sdesc);
	return ((const struct sdesc_with_exnra*)tmp)->exnra;
    }
    return 0;
}

struct hipe_sdesc_table {
    unsigned int log2size;
    unsigned int mask;		/* INV: mask == (1 << log2size)-1 */
    unsigned int used;
    struct sdesc **bucket;
};
extern struct hipe_sdesc_table hipe_sdesc_table;

extern void hipe_init_sdesc_table(struct sdesc*);

#if defined(__sparc__)
#define HIPE_RA_LSR_COUNT	2	/* low 2 bits are always zero */
#elif defined(__i386__)
#define HIPE_RA_LSR_COUNT	0	/* all bits are significant */
#endif

#if !defined(__GNUC__) || (__GNUC__ < 2) || (__GNUC__ == 2 && __GNUC_MINOR__ < 96)
#define __builtin_expect(x, expected_value) (x)
#endif
#define likely(x)	__builtin_expect((x),1)
#define unlikely(x)	__builtin_expect((x),0)

/* XXX: use this on sparc too later 
   #ifdef __i386__ */
static __inline__ const struct sdesc *hipe_find_sdesc(unsigned long ra)
{
    unsigned int i = (ra >> HIPE_RA_LSR_COUNT) & hipe_sdesc_table.mask;
    const struct sdesc *sdesc = hipe_sdesc_table.bucket[i];
    if( likely(sdesc->bucket.hvalue == ra) )
	return sdesc;
    do {
	sdesc = sdesc->bucket.next;
    } while( sdesc->bucket.hvalue != ra );
    return sdesc;
}
/* 
 #else
 extern const struct sdesc *hipe_find_sdesc(unsigned long ra);
 #endif
*/

#endif /* HIPE_BIF0_H */
