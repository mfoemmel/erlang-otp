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

#ifndef ERL_PRINTF_H_
#define ERL_PRINTF_H_
#include <stdio.h>
#include <stdarg.h>

extern int (*erts_printf_stdout_func)(char *, va_list);
extern int (*erts_printf_stderr_func)(char *, va_list);
extern int erts_printf_add_cr_to_stdout;
extern int erts_printf_add_cr_to_stderr;

typedef struct erts_dsprintf_buf_t_ erts_dsprintf_buf_t;

struct erts_dsprintf_buf_t_ {
    char *str;
    size_t str_len;
    size_t size;
    erts_dsprintf_buf_t *(*grow)(erts_dsprintf_buf_t *, size_t);
};

#define ERTS_DSPRINTF_BUF_INITER(GFUNC) {NULL, 0, 0, (GFUNC)}

int erts_printf(const char *, ...);
int erts_fprintf(FILE *, const char *, ...);
int erts_fdprintf(int, const char *, ...);
int erts_sprintf(char *, const char *, ...);
int erts_snprintf(char *, size_t, const char *, ...);
int erts_dsprintf(erts_dsprintf_buf_t *, const char *, ...);

int erts_vprintf(const char *, va_list);
int erts_vfprintf(FILE *, const char *, va_list);
int erts_vfdprintf(int, const char *, va_list);
int erts_vsprintf(char *, const char *, va_list);
int erts_vsnprintf(char *, size_t, const char *, va_list);
int erts_vdsprintf(erts_dsprintf_buf_t *, const char *, va_list);

#endif /* #ifndef ERL_PRINTF_H_ */
