/*	Copyright (C) 1995 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of SYSTAS.
 *
 * The exception is that, if you link the SYSTAS library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the SYSTAS library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name SYSTAS.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * SYSTAS, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for SYSTAS, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  
 */



#include <stdio.h>
#include <_scm.h>
#include "inst-rxposix.h"
#include "rxgnucomp.h"
#include "rxanal.h"
#include "rxunfa.h"
#include "rxbasic.h"


SCM_CONST_LONG (scm_REG_EXTENDED, "REG_EXTENDED", REG_EXTENDED);
SCM_CONST_LONG (scm_REG_ICASE, "REG_ICASE", REG_ICASE);
SCM_CONST_LONG (scm_REG_NEWLINE, "REG_NEWLINE", REG_NEWLINE);
SCM_CONST_LONG (scm_REG_NOSUB, "REG_NOSUB", REG_NOSUB);
SCM_CONST_LONG (scm_REG_NOTBOL, "REG_NOTBOL", REG_NOTBOL);
SCM_CONST_LONG (scm_REG_NOTEOL, "REG_NOTEOL", REG_NOTEOL);



long scm_tc16_regex_t;
#define RGX(X)	((regex_t *)SCM_CDR(X))
#define RGXP(X)	(SCM_CAR(X) == (SCM)scm_tc16_regex_t)

#ifdef __STDC__
size_t
free_regex_t (SCM obj)
#else
size_t
free_regex_t (obj)
     SCM obj;
#endif
{
  regex_t *r;
  r = RGX(obj);
  free ((char *)(r->owner_data));
  regfree (r);
  return 0;
}

#ifdef __STDC__
int
print_regex_t (SCM obj, SCM port, int writing)
#else
int
print_regex_t (obj, port, writing)
     SCM obj;
     SCM port;
     int writing;
#endif
{
  regex_t *r;
  r = RGX (obj);
  scm_gen_puts (scm_regular_string, "#<rgx ", port);
  scm_gen_puts (scm_regular_string, (char *)(r->owner_data), port);
  scm_gen_puts (scm_regular_string, ">", port);
  return 1;
}


static scm_smobfuns regex_t_smob =
{ scm_mark0, free_regex_t, print_regex_t, 0 };


SCM_PROC (s_compiled_regexp_p, "compiled-regexp?", 1, 0, 0, scm_compiled_regexp_p);
#ifdef __STDC__
SCM
scm_compiled_regexp_p (SCM obj)
#else
SCM
scm_compiled_regexp_p (obj)
     SCM obj;
#endif
{
  return ((SCM_NIMP (obj) && RGXP (obj))
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}

SCM_PROC(s_regcomp, "regcomp", 1, 1, 0, scm_regcomp);
#ifdef __STDC__
SCM
scm_regcomp (SCM pat, SCM cfl)
#else
SCM
scm_regcomp (pat, cfl)
     SCM pat;
     SCM cfl;
#endif
{
  SCM answer;
  SCM_ASSERT (SCM_NIMP (pat) && SCM_RO_STRINGP (pat), pat, SCM_ARG1, s_regcomp);
  if (cfl == SCM_UNDEFINED)
    cfl = SCM_INUM0;
  SCM_ASSERT (SCM_INUMP (cfl), cfl, SCM_ARG2, s_regcomp);

  SCM_NEWCELL (answer);
  SCM_DEFER_INTS;
  {
    regex_t * it;
    int status;
    it = malloc (sizeof (*it));
    if (!it)
      {
      allocation:
	SCM_ALLOW_INTS;
	SCM_ASSERT (0, pat, "allocation failure", s_regcomp);
      }
    status = regncomp (it, SCM_RO_CHARS (pat), SCM_RO_LENGTH (pat), SCM_INUM (cfl));
    if (status)
      {
	free (it);
	answer = SCM_MAKINUM (status);
      }
    else
      {
	it->owner_data = (void *)malloc (SCM_RO_LENGTH (pat) + 1);
	if (!it->owner_data)
	  {
	    regfree (it);
	    free (it);
	    goto allocation;
	  }
	memcpy (it->owner_data, SCM_RO_CHARS (pat), SCM_RO_LENGTH (pat));
	((char *)it->owner_data)[SCM_RO_LENGTH (pat)] = 0;
	SCM_SETCAR (answer, scm_tc16_regex_t);
	SCM_SETCDR (answer, (SCM)it);
      }
  }
  SCM_ALLOW_INTS;
  return answer;
}


SCM_PROC(s_regexec, "regexec", 2, 2, 0, scm_regexec);
#ifdef __STDC__
SCM
scm_regexec (SCM rgx, SCM str, SCM match_pick, SCM eflags)
#else
SCM
scm_regexec (rgx, str, match_pick, eflags)
     SCM rgx;
     SCM str;
     SCM match_pick;
     SCM eflags;
#endif
{
  SCM answer;
  SCM malloc_protect;
  regmatch_t * pmatch;
  int vector_result;

  SCM_ASSERT (SCM_NIMP (rgx) && RGXP (rgx), rgx, SCM_ARG1, s_regexec);
  SCM_ASSERT (SCM_NIMP (str) && SCM_RO_STRINGP (str), str, SCM_ARG2, s_regexec);
  if (eflags == SCM_UNDEFINED)
    eflags = SCM_INUM0;
  SCM_ASSERT (SCM_INUMP (eflags), eflags, SCM_ARG4, s_regexec);

  vector_result = (SCM_NIMP (match_pick) && SCM_VECTORP (match_pick));

  if (RGX(rgx)->re_nsub)
    {
      SCM starts;
      SCM ends;
      starts = scm_make_vector (SCM_MAKINUM (RGX(rgx)->re_nsub), SCM_BOOL_F, SCM_BOOL_F);
      ends  = scm_make_vector (SCM_MAKINUM (RGX(rgx)->re_nsub), SCM_BOOL_F, SCM_BOOL_F);
      answer = scm_cons (starts, ends);
    }
  
  malloc_protect = scm_malloc_obj (0);
  SCM_DEFER_INTS;
  {
    int status;
    pmatch = 0;
    status = regnexec (RGX(rgx), SCM_RO_CHARS (str), SCM_RO_LENGTH (str),
		       0, &pmatch, SCM_INUM (eflags) | REG_ALLOC_REGS);
    if (status)
      {
	SCM_ALLOW_INTS;
	if (vector_result)
	  return SCM_MAKINUM (status);
	else
	  {
	    if (status == REG_NOMATCH)
	      return SCM_BOOL_F;
	    else
	      scm_throw (SCM_CAR (scm_intern0 ("regexp-error")),
			 scm_listify (SCM_MAKINUM (status),
				      rgx,
				      str,
				      match_pick,
				      eflags,
				      SCM_UNDEFINED));
	  }
      }
    if (match_pick == SCM_BOOL_F)
      free (pmatch);
    else
      SCM_SETMALLOCDATA (malloc_protect, pmatch);
  }
  SCM_ALLOW_INTS;

  if (match_pick == SCM_BOOL_F)
    return SCM_BOOL_T;
  else if ((match_pick == SCM_BOOL_T) || (match_pick == SCM_UNDEFINED))
    {
      answer = (scm_listify
		(scm_make_shared_substring (str, SCM_MAKINUM (0), SCM_MAKINUM (pmatch[0].rm_so)),
		 scm_make_shared_substring (str, SCM_MAKINUM (pmatch[0].rm_so), SCM_MAKINUM (pmatch[0].rm_eo)),
		 scm_make_shared_substring (str, SCM_MAKINUM (pmatch[0].rm_eo), SCM_UNDEFINED),
		 SCM_UNDEFINED));
      return answer;
    }
  else if (vector_result)
    {
      int i;
      size_t bound;
      int vlen;

      bound = RGX(rgx)->re_nsub;
      vlen = SCM_LENGTH (match_pick);
      if (vlen < bound)
	bound = vlen;
      for (i = 0; i < bound; ++i)
	if (pmatch[0].rm_so >= 0)
	  SCM_VELTS (match_pick)[i] = scm_cons (SCM_MAKINUM (pmatch[i].rm_so),
					    SCM_MAKINUM (pmatch[i].rm_eo));
	else
	  SCM_VELTS (match_pick)[i] = SCM_BOOL_F;
      while (i < vlen)
	{
	  SCM_VELTS (match_pick)[i] = SCM_BOOL_F;
	  ++i;
	}
      return match_pick;
    }
  else
    {
      SCM spec;
      size_t bound;
      SCM ans_pos;

      answer = scm_cons (SCM_BOOL_F, SCM_BOOL_F);
      ans_pos = answer;
      bound = RGX(rgx)->re_nsub;

      for (spec = match_pick; spec != SCM_EOL; spec = SCM_CDR (spec))
	{
	  SCM item;
	  SCM frm;
	  SCM to;

	  SCM_ASSERT (SCM_NIMP (spec) && SCM_CONSP (spec), spec, SCM_ARG3, s_regexec);
	  item = SCM_CAR (spec);

	  if (SCM_ICHRP (item))
	    {
	      if (SCM_ICHR (item) == '<')
		{
		  frm = SCM_INUM0;
		  to = SCM_MAKINUM (pmatch[0].rm_so);
		  SCM_SETCDR (ans_pos,
			      scm_cons (scm_make_shared_substring (str, frm, to), SCM_EOL));
		}
	      else if (SCM_ICHR (item) == '>')
		{
		  frm = SCM_MAKINUM (pmatch[0].rm_eo);
		  to = SCM_UNDEFINED;
		  SCM_SETCDR (ans_pos,
			      scm_cons (scm_make_shared_substring (str, frm, to), SCM_EOL));
		}
	      else if (SCM_ICHR (item) == 'c')
		{
		  SCM_SETCDR (ans_pos,
			      scm_cons (SCM_MAKINUM (pmatch[0].final_tag), SCM_EOL));
		}
	      else
		SCM_ASSERT (0, spec, SCM_ARG3, s_regexec);
	      ans_pos = SCM_CDR (ans_pos);
	    }
	  else if (SCM_NIMP (item) && SCM_CONSP (item))
	    {
	      SCM ipos;
	      int solved;

	      solved = 0;
	      for (ipos = item; SCM_NIMP (ipos) && SCM_CONSP (ipos); ipos = SCM_CDR (ipos))
		{
		  SCM iitem;
		  int ival;
		  iitem = SCM_CAR (ipos);
		  SCM_ASSERT (SCM_INUMP (iitem), spec, SCM_ARG3, s_regexec);
		  ival = SCM_INUM (iitem);
		  SCM_ASSERT ((ival >= 0) && (ival < bound), spec, SCM_OUTOFRANGE, s_regexec);
		  if (pmatch[ival].rm_so >= 0)
		    {
 		      SCM_SETCDR (ans_pos, scm_cons (SCM_MAKINUM (ival), SCM_EOL));
		      ans_pos = SCM_CDR (ans_pos);
		      solved = 1;
		      break;
		    }
		}
	      if (!solved)
		{
		  SCM_SETCDR (ans_pos, scm_cons (SCM_BOOL_F, SCM_EOL));
		  ans_pos = SCM_CDR (ans_pos);
		}
	    }
	  else
	    {
	      int n;
	      SCM_ASSERT (SCM_INUMP (item), spec, SCM_ARG3, s_regexec);
	      n = SCM_INUM (item);
	      SCM_ASSERT ((n >= 0) && (n < bound), spec, SCM_OUTOFRANGE, s_regexec);
	      if (pmatch[n].rm_so < 0)
		{
		  SCM_SETCDR (ans_pos, scm_cons (SCM_BOOL_F, SCM_EOL));
		  ans_pos = SCM_CDR (ans_pos);
		  continue;
		}
	      frm = SCM_MAKINUM (pmatch[n].rm_so);
	      to = SCM_MAKINUM (pmatch[n].rm_eo);
	      SCM_SETCDR (ans_pos,
			  scm_cons (scm_make_shared_substring (str, frm, to), SCM_EOL));
	      ans_pos = SCM_CDR (ans_pos);
	    }
	}
      return SCM_CDR (answer);
    }
}




struct rx_dfa_state
{
  struct rx_classical_system frame;
  struct rx_unfa * unfa;
};

long scm_tc16_dfa_t;
#define DFA(X)	((struct rx_dfa_state *)SCM_CDR(X))
#define DFAP(X)	(SCM_CAR(X) == (SCM)scm_tc16_dfa_t)

#ifdef __STDC__
size_t
free_dfa_t (SCM obj)
#else
size_t
free_dfa_t (obj)
     SCM obj;
#endif
{
  struct rx_dfa_state *r;
  r = DFA(obj);
  rx_terminate_system (&r->frame);
  rx_free_unfa (r->unfa);
  scm_must_free ((char *)r);
  return sizeof (struct rx_dfa_state);
}

#ifdef __STDC__
int
print_dfa_t (SCM obj, SCM port, int writing)
#else
int
print_dfa_t (obj, port, writing)
     SCM obj;
     SCM port;
     int writing;
#endif
{
  struct rx_dfa_state *r;
  r = DFA (obj);
  scm_gen_puts (scm_regular_string, "#<dfa ", port);
  scm_intprint (r->frame.rx->rx_id, 10, port);
  scm_gen_puts (scm_regular_string, ">", port);
  return 1;
}

static scm_smobfuns dfa_t_smob =
{ scm_mark0, free_dfa_t, print_dfa_t, 0 };


SCM_PROC (s_regexp_to_dfa, "regexp->dfa", 1, 1, 0, scm_regexp_to_dfa);
#ifdef __STDC__
SCM 
scm_regexp_to_dfa (SCM regexp, SCM cfl)
#else
SCM 
scm_regexp_to_dfa (regexp, cfl)
     SCM regexp;
     SCM cfl;
#endif
{
  int ret;
  unsigned int syntax;
  struct rx_dfa_state *r;
  struct rexp_node * parsed;
  int cflags;
  char * pattern;
  int len;
  SCM answer;

  SCM_ASSERT (SCM_NIMP (regexp) && SCM_RO_STRINGP (regexp),
	      regexp, SCM_ARG1, s_regexp_to_dfa);
  if (cfl == SCM_UNDEFINED)
    cfl = SCM_INUM0;
  SCM_ASSERT (SCM_INUMP (cfl), cfl, SCM_ARG2, s_regexp_to_dfa);

  pattern = SCM_RO_CHARS (regexp);
  len = SCM_RO_LENGTH (regexp);
  cflags = SCM_INUM (cfl);


  SCM_NEWCELL (answer);

  SCM_DEFER_INTS;
  syntax = ((cflags & REG_EXTENDED)
	    ? RE_SYNTAX_POSIX_EXTENDED
	    : RE_SYNTAX_POSIX_BASIC);

  if (cflags & REG_NEWLINE)
    {
      syntax &= ~RE_DOT_NEWLINE;
      syntax |= RE_HAT_LISTS_NOT_NEWLINE;
    }


  ret = rx_parse (&parsed, pattern, len, syntax, 256, 0);

  if (ret)
    {
      SCM_ALLOW_INTS;
      return SCM_MAKINUM (ret);
    }


  r = (struct rx_dfa_state *)scm_must_malloc (sizeof (struct rx_dfa_state), s_regexp_to_dfa);
  r->unfa = rx_unfa (rx_basic_unfaniverse (), parsed, 256);
  rx_free_rexp (parsed);
  if (!r->unfa)
    {
      scm_mallocated -= sizeof (*r);
      scm_must_free ((char *)r);
      SCM_ALLOW_INTS;
      SCM_ASSERT (0, regexp, "internal error constructing rx_unfa", s_regexp_to_dfa);
    }

  rx_init_system (&r->frame, r->unfa->nfa);
  SCM_SETCAR (answer, scm_tc16_dfa_t);
  SCM_SETCDR (answer, (SCM)r);
  SCM_ALLOW_INTS;
  return answer;
}


SCM_PROC (s_dfa_fork, "dfa-fork", 1, 0, 0, scm_dfa_fork);
#ifdef __STDC__
SCM
scm_dfa_fork (SCM dfa)
#else
SCM
scm_dfa_fork (dfa)
     SCM dfa;
#endif
{
  struct rx_dfa_state *r;
  SCM answer;

  SCM_ASSERT (SCM_NIMP (dfa) && DFAP (dfa),
	      dfa, SCM_ARG1, s_dfa_fork);

  SCM_NEWCELL (answer);

  SCM_DEFER_INTS;
  r = (struct rx_dfa_state *)scm_must_malloc (sizeof (struct rx_dfa_state), s_dfa_fork);
  rx_save_unfa (DFA (dfa)->unfa);
  r->unfa = DFA (dfa)->unfa;
  rx_init_system (&r->frame, r->unfa->nfa);
  r->frame.state = DFA(dfa)->frame.state;
  if (r->frame.state)
    rx_lock_superstate (r->frame.rx, r->frame.state);
  SCM_SETCAR (answer, scm_tc16_dfa_t);
  SCM_SETCDR (answer, (SCM)r);
  SCM_ALLOW_INTS;
  return answer;
}


SCM_PROC (s_reset_dfa_x, "reset-dfa!", 1, 0, 0, scm_reset_dfa_x);
#ifdef __STDC__
SCM
scm_reset_dfa_x (SCM dfa)
#else
SCM
scm_reset_dfa_x (dfa)
     SCM dfa;
#endif
{
  SCM_ASSERT (SCM_NIMP (dfa) && DFAP (dfa),
	      dfa, SCM_ARG1, s_reset_dfa_x);
  
  SCM_DEFER_INTS;
  if (rx_yes != rx_start_superstate (&DFA(dfa)->frame))
    {
      SCM_ALLOW_INTS;
      SCM_ASSERT (0, dfa, "internal error constructing rx starting superstate", s_reset_dfa_x);
    }
  SCM_ALLOW_INTS;
  return dfa;
}



SCM_PROC (s_dfa_final_tag, "dfa-final-tag", 1, 0, 0, scm_dfa_final_tag);
#ifdef __STDC__
SCM
scm_dfa_final_tag (SCM dfa)
#else
SCM
scm_dfa_final_tag (dfa)
     SCM dfa;
#endif
{
  SCM_ASSERT (SCM_NIMP (dfa) && DFAP (dfa),
	      dfa, SCM_ARG1, s_dfa_final_tag);

  if (DFA (dfa)->frame.state)
    return scm_long2num ((long)DFA (dfa)->frame.state->contents->is_final);
  else
    return SCM_INUM0;
}



SCM_PROC (s_dfa_continuable_p, "dfa-continuable?", 1, 0, 0, scm_dfa_continuable_p);
#ifdef __STDC__
SCM
scm_dfa_continuable_p (SCM dfa)
#else
SCM
scm_dfa_continuable_p (dfa)
     SCM dfa;
#endif
{
  SCM_ASSERT (SCM_NIMP (dfa) && DFAP (dfa),
	      dfa, SCM_ARG1, s_dfa_continuable_p);

  return ((DFA (dfa)->frame.state && DFA (dfa)->frame.state->contents->has_cset_edges)
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}


SCM_PROC (s_advance_dfa_x, "advance-dfa!", 2, 0, 0, scm_advance_dfa_x);
#ifdef __STDC__
SCM
scm_advance_dfa_x (SCM dfa, SCM s)
#else
SCM
scm_advance_dfa_x (dfa, s)
     SCM dfa;
     SCM s;
#endif
{
  struct rx_dfa_state * d;
  char * str;
  int len;
  int matched;
  
  SCM_ASSERT (SCM_NIMP (dfa) && DFAP (dfa),
	      dfa, SCM_ARG1, s_advance_dfa_x);
  SCM_ASSERT (SCM_NIMP (s) && SCM_RO_STRINGP (s),
	      s, SCM_ARG2, s_advance_dfa_x);

  str = SCM_RO_CHARS (s);
  len = SCM_RO_LENGTH (s);
  d = DFA (dfa);

  SCM_DEFER_INTS;
  matched = rx_advance_to_final (&d->frame, (unsigned char *)str, len);
  SCM_ALLOW_INTS;

  if (matched >= 0)
    scm_return_first (SCM_MAKINUM (matched), dfa, s);
  else
    SCM_ASSERT (0, dfa, "internal error in rx_advance_to_final", s_advance_dfa_x);
}






#ifdef __STDC__
void
scm_init_rgx (void)
#else
void
scm_init_rgx ()
#endif
{
  scm_tc16_regex_t = scm_newsmob (&regex_t_smob);
  scm_tc16_dfa_t = scm_newsmob (&dfa_t_smob);
#include "rgx.x"
}

