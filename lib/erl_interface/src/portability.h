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

#ifndef PORTABILITY_H__
#define PORTABILITY_H__ 1


#ifdef __STDC__
# define __ERL_P(args) args
# define __ERL_DOTS , ...
#else
# define __ERL_P(args) ()
# define __ERL_DOTS
#endif

#ifdef __cplusplus
# define __ERL_BEGIN_DECL  extern "C" {
# define __ERL_END_DECL    }
#else
# define __ERL_BEGIN_DECL
# define __ERL_END_DECL
#endif

#if !defined(__GNUC__) || __GNUC__ < 2

/*
 * GCC's attributes are too useful to not use. Other compilers
 * just lose opportunities to optimize and warn.
 */
# define __attribute__(foo) /* nothing */

#endif

/*
 * Magic for link-time warnings and suchlike.
 */
#if defined(__GNUC__)		/* GNU assembler syntax */

#if 1

/*
 * Weave the spell with GCC extensions and ELF sections.
 *
 * __SYM_WARNING(sym, text) tries to make the linker emit a warning on
 * the form:
 *
 *   frotz.c:17: `frobozzica()' --- le bien choisi.
 *
 * where the function frobozzica() is called in frotz.c, line 17.
 */
# define __SYM_WARNING(sym, text)   \
static const char __sym_warning_ ## sym [] __attribute__((section (".gnu.warning." #sym), unused)) = text

#else

/*
 * Weave the spell with STABS markup.
 */
# define __SYM_WARNING(sym, text)			\
__asm__(".stabs \"" text "\", 0x1e, 0, 0, 0\n\t"	\
	".stabs \"" #sym "\", 0x01, 0, 0, 0\n")
#endif

/*
 * Insert a comment into the object file, retrievable by SCCS `what'
 * (`@(#)') and RCS/CVS `ident' (`$Comment: ...$').
 */
# define __COMMENT(text)   \
static const char __comment_ ## __LINE__ [] __attribute__((unused)) = "@(#) $Comment: " text "$"

#else  /* !__GNUC__ */

/*
 * Does MSVC++ have anything like it?
 */

# define __SYM_WARNING(sym, text)

#endif /* !__GNUC__ */


#endif /* PORTABILITY_H__ */
