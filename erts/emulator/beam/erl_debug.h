#ifndef _ERL_DEBUG_H_
#define _ERL_DEBUG_H_

#ifdef DEBUG

#ifdef HIPE
#include "hipe_debug.h"
#endif

/*
 * Verbose - To print debug messages
 * ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
 * There are six levels of verbose at this moment:
 *
 * 0. VERBOSE_SILENT    - Normal mode. This means a debug compiled system
 *                        started without any -v flag.
 *
 * 1. VERBOSE_CHATTY    - This is the level previously used when verbose
 *                        was turned on, that is messages about configuration
 *                        at startup time etc.
 *
 * 2. VERBOSE_NOISY     - Adds messages about garbage collection traces.
 *
 * 3. VERBOSE_MORE_MEM  - Adds more messages about garbage collection.
 *
 * 4. VERBOSE_SCREAMING - Adds messages about heap sanity checks.
 *
 * 5. VERBOSE_OVERKILL  - Nothing so far...
 *
 * There are two methods to print debug messages, verbose_message and
 * verbose_error, which produces output to stdout and stderr
 * respectively. Both functions share the same syntax:
 *
 * (void) verbose_message( Level, Message, ... );
 *
 * The message is a standard printf-format string and ... is replaced
 * by optional arguments to satisfy the format string.
 */

typedef enum {
    VERBOSE_SILENT,
    VERBOSE_CHATTY,
    VERBOSE_NOISY,
    VERBOSE_MORE_MEM,
    VERBOSE_SCREAMING,
    VERBOSE_OVERKILL
} verbose_level;

extern verbose_level verbose;

#define VERBOSE(x) verbose_error(VERBOSE_SILENT,"The macro VERBOSE() is deprecated! See erl_debug.h for more information.");

static ERTS_INLINE void verbose_message(verbose_level level, char *message, ...)
{
    if (level <= verbose) {
        va_list ap;
        va_start(ap,message);
        vfprintf(stdout,message,ap);
        va_end(ap);
        fprintf(stdout,"\r");
    }
}

static ERTS_INLINE void verbose_error(verbose_level level, char *message, ...)
{
    if (level <= verbose) {
        va_list ap;
        va_start(ap,message);
        vfprintf(stderr,message,ap);
        va_end(ap);
    }
}

void upp(byte*, int);
void pat(Eterm);
void pinfo(void);
void pp(Process*);
void ppi(Eterm);
void pba(Process*, int);
void td(Eterm);
void ps(Process*, Eterm*);

#if 0
# define VERBOSE_MESSAGE(Args) verbose_message Args
# define VERBOSE_ERROR(Args) verbose_error Args
#else
# define VERBOSE_MESSAGE(Args)
# define VERBOSE_ERROR(Args)
#endif
void verbose_message(verbose_level level, char *message, ...);
void verbose_error(verbose_level level, char *message, ...);

#else /* Non-debug mode */

#define CHECK_HEAP(p)
#define CHECK_MEMORY(start, end)
#define VERBOSE_MESSAGE(Args)
#define VERBOSE_ERROR(Args)

#endif /* DEBUG */

/*
 * These functions can be handy when developing, and perhaps useful
 * even outside debugging.
 */
extern void print_tagged_memory(Eterm *start, Eterm *end);
extern void print_untagged_memory(Eterm *start, Eterm *end);
extern void print_memory_info(Process *p);
#ifdef HYBRID
extern void print_ma_info(void);
extern void print_message_area(void);
extern void check_message_area(void);
#endif
#ifdef INCREMENTAL_GC
extern void print_active_procs(void);
#endif

#endif /* _ERL_DEBUG_H_ */
