/*
 * $Id$
 */
#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#if HIPE /* hipe_mkliterals is needed even if HIPE is not enabled */
#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "erl_bits.h"
/* this sucks, but the loaders need data for all platforms */
#include "hipe_x86_asm.h"
#undef P
#undef HP
#undef NSP
#undef TEMP0
#undef TEMP1
#undef ARG0
#undef ARG1
#undef ARG2
#include "hipe_sparc_registers.h"
#undef P
#undef HP
#undef TEMP0
#undef TEMP1
#undef ARG0
#undef ARG1
#undef ARG2
#include "erl_binary.h"
#endif /* HIPE */

static const unsigned long CRCTABLE[256] = {
    0x00000000L, 0x77073096L, 0xEE0E612CL, 0x990951BAL,
    0x076DC419L, 0x706AF48FL, 0xE963A535L, 0x9E6495A3L,
    0x0EDB8832L, 0x79DCB8A4L, 0xE0D5E91EL, 0x97D2D988L,
    0x09B64C2BL, 0x7EB17CBDL, 0xE7B82D07L, 0x90BF1D91L,
    0x1DB71064L, 0x6AB020F2L, 0xF3B97148L, 0x84BE41DEL,
    0x1ADAD47DL, 0x6DDDE4EBL, 0xF4D4B551L, 0x83D385C7L,
    0x136C9856L, 0x646BA8C0L, 0xFD62F97AL, 0x8A65C9ECL,
    0x14015C4FL, 0x63066CD9L, 0xFA0F3D63L, 0x8D080DF5L,
    0x3B6E20C8L, 0x4C69105EL, 0xD56041E4L, 0xA2677172L,
    0x3C03E4D1L, 0x4B04D447L, 0xD20D85FDL, 0xA50AB56BL,
    0x35B5A8FAL, 0x42B2986CL, 0xDBBBC9D6L, 0xACBCF940L,
    0x32D86CE3L, 0x45DF5C75L, 0xDCD60DCFL, 0xABD13D59L,
    0x26D930ACL, 0x51DE003AL, 0xC8D75180L, 0xBFD06116L,
    0x21B4F4B5L, 0x56B3C423L, 0xCFBA9599L, 0xB8BDA50FL,
    0x2802B89EL, 0x5F058808L, 0xC60CD9B2L, 0xB10BE924L,
    0x2F6F7C87L, 0x58684C11L, 0xC1611DABL, 0xB6662D3DL,
    0x76DC4190L, 0x01DB7106L, 0x98D220BCL, 0xEFD5102AL,
    0x71B18589L, 0x06B6B51FL, 0x9FBFE4A5L, 0xE8B8D433L,
    0x7807C9A2L, 0x0F00F934L, 0x9609A88EL, 0xE10E9818L,
    0x7F6A0DBBL, 0x086D3D2DL, 0x91646C97L, 0xE6635C01L,
    0x6B6B51F4L, 0x1C6C6162L, 0x856530D8L, 0xF262004EL,
    0x6C0695EDL, 0x1B01A57BL, 0x8208F4C1L, 0xF50FC457L,
    0x65B0D9C6L, 0x12B7E950L, 0x8BBEB8EAL, 0xFCB9887CL,
    0x62DD1DDFL, 0x15DA2D49L, 0x8CD37CF3L, 0xFBD44C65L,
    0x4DB26158L, 0x3AB551CEL, 0xA3BC0074L, 0xD4BB30E2L,
    0x4ADFA541L, 0x3DD895D7L, 0xA4D1C46DL, 0xD3D6F4FBL,
    0x4369E96AL, 0x346ED9FCL, 0xAD678846L, 0xDA60B8D0L,
    0x44042D73L, 0x33031DE5L, 0xAA0A4C5FL, 0xDD0D7CC9L,
    0x5005713CL, 0x270241AAL, 0xBE0B1010L, 0xC90C2086L,
    0x5768B525L, 0x206F85B3L, 0xB966D409L, 0xCE61E49FL,
    0x5EDEF90EL, 0x29D9C998L, 0xB0D09822L, 0xC7D7A8B4L,
    0x59B33D17L, 0x2EB40D81L, 0xB7BD5C3BL, 0xC0BA6CADL,
    0xEDB88320L, 0x9ABFB3B6L, 0x03B6E20CL, 0x74B1D29AL,
    0xEAD54739L, 0x9DD277AFL, 0x04DB2615L, 0x73DC1683L,
    0xE3630B12L, 0x94643B84L, 0x0D6D6A3EL, 0x7A6A5AA8L,
    0xE40ECF0BL, 0x9309FF9DL, 0x0A00AE27L, 0x7D079EB1L,
    0xF00F9344L, 0x8708A3D2L, 0x1E01F268L, 0x6906C2FEL,
    0xF762575DL, 0x806567CBL, 0x196C3671L, 0x6E6B06E7L,
    0xFED41B76L, 0x89D32BE0L, 0x10DA7A5AL, 0x67DD4ACCL,
    0xF9B9DF6FL, 0x8EBEEFF9L, 0x17B7BE43L, 0x60B08ED5L,
    0xD6D6A3E8L, 0xA1D1937EL, 0x38D8C2C4L, 0x4FDFF252L,
    0xD1BB67F1L, 0xA6BC5767L, 0x3FB506DDL, 0x48B2364BL,
    0xD80D2BDAL, 0xAF0A1B4CL, 0x36034AF6L, 0x41047A60L,
    0xDF60EFC3L, 0xA867DF55L, 0x316E8EEFL, 0x4669BE79L,
    0xCB61B38CL, 0xBC66831AL, 0x256FD2A0L, 0x5268E236L,
    0xCC0C7795L, 0xBB0B4703L, 0x220216B9L, 0x5505262FL,
    0xC5BA3BBEL, 0xB2BD0B28L, 0x2BB45A92L, 0x5CB36A04L,
    0xC2D7FFA7L, 0xB5D0CF31L, 0x2CD99E8BL, 0x5BDEAE1DL,
    0x9B64C2B0L, 0xEC63F226L, 0x756AA39CL, 0x026D930AL,
    0x9C0906A9L, 0xEB0E363FL, 0x72076785L, 0x05005713L,
    0x95BF4A82L, 0xE2B87A14L, 0x7BB12BAEL, 0x0CB61B38L,
    0x92D28E9BL, 0xE5D5BE0DL, 0x7CDCEFB7L, 0x0BDBDF21L,
    0x86D3D2D4L, 0xF1D4E242L, 0x68DDB3F8L, 0x1FDA836EL,
    0x81BE16CDL, 0xF6B9265BL, 0x6FB077E1L, 0x18B74777L,
    0x88085AE6L, 0xFF0F6A70L, 0x66063BCAL, 0x11010B5CL,
    0x8F659EFFL, 0xF862AE69L, 0x616BFFD3L, 0x166CCF45L,
    0xA00AE278L, 0xD70DD2EEL, 0x4E048354L, 0x3903B3C2L,
    0xA7672661L, 0xD06016F7L, 0x4969474DL, 0x3E6E77DBL,
    0xAED16A4AL, 0xD9D65ADCL, 0x40DF0B66L, 0x37D83BF0L,
    0xA9BCAE53L, 0xDEBB9EC5L, 0x47B2CF7FL, 0x30B5FFE9L,
    0xBDBDF21CL, 0xCABAC28AL, 0x53B39330L, 0x24B4A3A6L,
    0xBAD03605L, 0xCDD70693L, 0x54DE5729L, 0x23D967BFL,
    0xB3667A2EL, 0xC4614AB8L, 0x5D681B02L, 0x2A6F2B94L,
    0xB40BBE37L, 0xC30C8EA1L, 0x5A05DF1BL, 0x2D02EF8DL,
};

static const struct literal {
    const char *name;
    unsigned long value;
} literals[] = {
#if HIPE
    /* Field offsets in a process struct */
    { "P_HP", offsetof(struct process, htop) },
#ifdef SHARED_HEAP
    { "P_HP_LIMIT", offsetof(struct process, hend) },
#else
    { "P_HP_LIMIT", offsetof(struct process, stop) },
    { "P_OFF_HEAP_FUNS", offsetof(struct process, off_heap.funs) },
    { "P_OFF_HEAP_MSO", offsetof(struct process, off_heap.mso) },
    { "P_OFF_HEAP_OVERHEAD", offsetof(struct process, off_heap.overhead) },
#endif
    { "P_ID", offsetof(struct process, id) },
    { "P_FLAGS", offsetof(struct process, flags) },
    { "P_FVALUE", offsetof(struct process, fvalue) },
    { "P_FREASON", offsetof(struct process, freason) },
    { "P_FCALLS", offsetof(struct process, fcalls) },
    { "P_BEAM_IP", offsetof(struct process, i) },
    { "P_ARITY", offsetof(struct process, arity) },
    { "P_ARG0", offsetof(struct process, def_arg_reg[0]) },
    { "P_ARG1", offsetof(struct process, def_arg_reg[1]) },
    { "P_ARG2", offsetof(struct process, def_arg_reg[2]) },
    { "P_ARG3", offsetof(struct process, def_arg_reg[3]) },
    { "P_ARG4", offsetof(struct process, def_arg_reg[4]) },
#if defined(__sparc__)
    { "P_ARG5", offsetof(struct process, def_arg_reg[5]) },
    { "P_ARG6", offsetof(struct process, def_arg_reg[6]) },
    { "P_ARG7", offsetof(struct process, def_arg_reg[7]) },
    { "P_ARG8", offsetof(struct process, def_arg_reg[8]) },
    { "P_ARG9", offsetof(struct process, def_arg_reg[9]) },
    { "P_ARG10", offsetof(struct process, def_arg_reg[10]) },
    { "P_ARG11", offsetof(struct process, def_arg_reg[11]) },
    { "P_ARG12", offsetof(struct process, def_arg_reg[12]) },
    { "P_ARG13", offsetof(struct process, def_arg_reg[13]) },
    { "P_ARG14", offsetof(struct process, def_arg_reg[14]) },
    { "P_ARG15", offsetof(struct process, def_arg_reg[15]) },
#endif
    { "P_NSP", offsetof(struct process, hipe.nsp) },
    { "P_NCALLEE", offsetof(struct process, hipe.ncallee) },
    { "P_CLOSURE", offsetof(struct process, hipe.closure) },
#if defined(__sparc__)
    { "P_NSP_LIMIT", offsetof(struct process, hipe.nstend) },
    { "P_NRA", offsetof(struct process, hipe.nra) },
    { "P_CRA", offsetof(struct process, hipe.ncra) },
#elif defined(__i386__)
    { "P_NSP_LIMIT", offsetof(struct process, hipe.nstack) },
    { "P_CSP", offsetof(struct process, hipe.ncsp) },
    { "P_NARITY", offsetof(struct process, hipe.narity) },
#endif

    /* process flags bits */
    {  "F_TIMO", F_TIMO },

    /* freason codes */
    { "FREASON_THROWN", THROWN },
    { "FREASON_USER_EXIT", USER_EXIT },
    { "FREASON_USER_ERROR", USER_ERROR },
    { "FREASON_USER_ERROR2", USER_ERROR2 },
    { "FREASON_TRAP", TRAP },
    { "FREASON_RESCHEDULE", RESCHEDULE },

    /* special Erlang constants */
    { "ERL_NIL", NIL },
    { "THE_NON_VALUE", THE_NON_VALUE },

    /* funs */
    { "EFE_BUCKET", offsetof(struct erl_fun_entry, bucket) },
    { "EFE_OLD_UNIQ", offsetof(struct erl_fun_entry, old_uniq) },
    { "EFE_OLD_INDEX", offsetof(struct erl_fun_entry, old_index) },
    { "EFE_ADDRESS", offsetof(struct erl_fun_entry, address) },
    { "EFE_NATIVE_ADDRESS", offsetof(struct erl_fun_entry, native_address) },
    { "EFE_MODULE", offsetof(struct erl_fun_entry, module) },
    { "EFE_REFC", offsetof(struct erl_fun_entry, refc) },
    { "EFT_THING", offsetof(struct erl_fun_thing, thing_word) },
#ifndef SHARED_HEAP
    { "EFT_NEXT", offsetof(struct erl_fun_thing, next) },
#endif
    { "EFT_CREATOR", offsetof(struct erl_fun_thing, creator) },
    { "EFT_FE", offsetof(struct erl_fun_thing, fe) },
    { "EFT_NATIVE_ADDRESS", offsetof(struct erl_fun_thing, native_address) },
    { "EFT_ARITY", offsetof(struct erl_fun_thing, arity) },
    { "EFT_NUM_FREE", offsetof(struct erl_fun_thing, num_free) },
    { "EFT_ENV", offsetof(struct erl_fun_thing, env[0]) },
    { "ERL_FUN_SIZE", ERL_FUN_SIZE },

    /* bit syntax */
    { "BSF_ALIGNED", BSF_ALIGNED},
    { "BSF_LITTLE", BSF_LITTLE},
    { "BSF_SIGNED", BSF_SIGNED},
    { "BSF_EXACT", BSF_EXACT},
    { "MB_ORIG", offsetof(struct erl_bin_match_buffer, orig) },
    { "MB_BASE", offsetof(struct erl_bin_match_buffer, base) },
    { "MB_OFFSET", offsetof(struct erl_bin_match_buffer, offset) },
    { "MB_SIZE", offsetof(struct erl_bin_match_buffer, size) },
    { "PROC_BIN_THING_WORD", offsetof(struct proc_bin, thing_word) },
    { "PROC_BIN_BINSIZE", offsetof(struct proc_bin, size) },
    { "PROC_BIN_NEXT", offsetof(struct proc_bin, next) },
    { "PROC_BIN_VAL", offsetof(struct proc_bin, val) },
    { "PROC_BIN_BYTES", offsetof(struct proc_bin, bytes) },
    { "PROC_BIN_BYTESIZE", PROC_BIN_SIZE},
    { "BINARY_ORIG_BYTES", offsetof(struct binary, orig_bytes) },
    { "MAX_HEAP_BIN_SIZE", ERL_ONHEAP_BIN_LIMIT},
    { "OVERHEAD_FACTOR", (BINARY_OVERHEAD_FACTOR*sizeof(Eterm))},

    /* x86 */
    { "X86_NR_ARG_REGS", X86_NR_ARG_REGS },
#if X86_HP_IN_ESI
    { "X86_HP_IN_ESI", 1 },
#endif

    /* SPARC */
    { "HIPE_SPARC_LEAF_WORDS", HIPE_SPARC_LEAF_WORDS },
    { "SPARC_ARGS_IN_REGS", HIPE_SPARC_ARGS_IN_REGS},
    { "SPARC_REG_P", P_NR},
    { "SPARC_REG_NSP", NSP_NR},
    { "SPARC_REG_NSP_LIMIT", NSP_LIMIT_NR},
    { "SPARC_REG_HP", HP_NR},
    { "SPARC_REG_HP_LIMIT", HP_LIMIT_NR},
    { "SPARC_REG_FCALLS", FCALLS_NR},
    { "SPARC_REG_RA", RA_NR},
    { "SPARC_REG_TEMP0", TEMP0_NR},
    { "SPARC_REG_TEMP1", TEMP1_NR},
    { "SPARC_REG_TEMP2", TEMP2_NR},
    { "SPARC_REG_TEMP3", TEMP3_NR},
    { "SPARC_REG_ARG0", ARG0_NR},
    { "SPARC_REG_ARG1", ARG1_NR},
    { "SPARC_REG_ARG2", ARG2_NR},
    { "SPARC_REG_ARG3", ARG3_NR},
    { "SPARC_REG_ARG4", ARG4_NR},
    { "SPARC_REG_ARG5", ARG5_NR},
    { "SPARC_REG_ARG6", ARG6_NR},
    { "SPARC_REG_ARG7", ARG7_NR},
    { "SPARC_REG_ARG8", ARG8_NR},
    { "SPARC_REG_ARG9", ARG9_NR},
    { "SPARC_REG_ARG10", ARG10_NR},
    { "SPARC_REG_ARG11", ARG11_NR},
    { "SPARC_REG_ARG12", ARG12_NR},
    { "SPARC_REG_ARG13", ARG13_NR},
    { "SPARC_REG_ARG14", ARG14_NR},
    { "SPARC_REG_ARG15", ARG15_NR},
#else /* !HIPE, fake minimum set to allow compiling the loaders */
    { "SPARC_ARGS_IN_REGS", 0 },
    { "P_BEAM_IP", 0 },
    { "P_ARITY", 0 },
#endif /* HIPE */

#ifdef SHARED_HEAP
    { "HEAP_ARCH_SHARED", 1 },
#else
    { "HEAP_ARCH_PRIVATE", 1 },
#endif
};
#define NLITERALS ((sizeof literals)/sizeof(literals[0]))

/*
 *  The algorithm for calculating the 32 bit CRC checksum is based upon
 *  documentation and algorithms provided by Dr. Ross N. Williams in the
 *  document "A Painless Guide to CRC Error Detection Algorithms."
 *  This document may be downloaded from
 *  ftp://ftp.rocksoft.com/cliens/rocksoft/papers/crc_v3.txt
 *  as of 12/15/1998. Dr. Williams has placed this document and algorithms
 *  in the public domain.
 */
static unsigned long crc_init(void)
{
    return 0xFFFFFFFF;
}

static unsigned long crc_update(unsigned long crc_value, const void *buf, unsigned int length)
{
    const unsigned char *tab;

    tab = (const unsigned char*)buf;
    for(; length > 0; --length) {
	unsigned char t = (crc_value >> 24) & 0xFF;
	crc_value = (crc_value << 8) | *tab++;
	crc_value ^= CRCTABLE[t];
    }
    return crc_value;
}

static unsigned long literals_crc(void)
{
    unsigned long crc_value;
    unsigned int i;

    crc_value = crc_init();
    for(i = 0; i < NLITERALS; ++i)
	crc_value = crc_update(crc_value, &literals[i].value, sizeof(literals[i].value));
    return crc_value & 0x07FFFFFF;
}

static void c_print1(FILE *fp, const struct literal *literal)
{
    fprintf(fp, "#define %s %lu\n", literal->name, literal->value);
}

static void e_print1(FILE *fp, const struct literal *literal)
{
    fprintf(fp, "-define(%s,%lu).\n", literal->name, literal->value);
}

static void printall(FILE *fp, void (*print1)(FILE*,const struct literal*))
{
    unsigned int i;

    for(i = 0; i < NLITERALS; ++i)
	(*print1)(fp, &literals[i]);
}

static int do_c(FILE *fp)
{
    fprintf(fp, "/* File: hipe_literals.h, generated by hipe_mkliterals */\n");
    fprintf(fp, "#ifndef __HIPE_LITERALS_H__\n");
    fprintf(fp, "#define __HIPE_LITERALS_H__\n\n");
    printall(fp, c_print1);
    fprintf(fp, "#define HIPE_SYSTEM_CRC %luL\n", literals_crc());
    fprintf(fp, "\n#endif\n");
    return 0;
}

static int do_e(FILE *fp)
{
    fprintf(fp, "%% File: hipe_literals.hrl, generated by hipe_mkliterals\n\n");
    printall(fp, e_print1);
    fprintf(fp, "-define(HIPE_SYSTEM_CRC,%lu).\n", literals_crc());
    return 0;
}

int main(int argc, const char **argv)
{
    if( argc > 0 ) {
	if( strcmp(argv[1], "-c") == 0 )
	    return do_c(stdout);
	if( strcmp(argv[1], "-e") == 0 )
	    return do_e(stdout);
    }
    fprintf(stderr, "usage: %s [-c | -e] > output-file\n", argv[0]);
    return 1;
}
