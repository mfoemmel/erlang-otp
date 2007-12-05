/*	Copyright (C) 1995, 1996 Tom Lord
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Library General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 * 
 * You should have received a copy of the GNU Library General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 59 Temple Place - Suite 330, 
 * Boston, MA 02111-1307, USA. 
 */



#include <stdio.h>
#include "regex.h"



struct a_test
{
  int expected;
  char * pattern;
  unsigned char * data;
};

struct a_test the_tests[] = 
{
#include "testcases.h"
  {-1, 0, 0}
};




void
run_a_test (id, t)
     int id;
     struct a_test * t;
{
  static char * last_pattern = 0;
  static regex_t r;
  int err;
  char errmsg[100];
  int x;
  regmatch_t regs[10];

  if (!last_pattern || strcmp (last_pattern, t->pattern))
    {
      if (last_pattern)
	{
	  regfree (&r);
	  last_pattern = 0;
	}
      err = regcomp (&r, t->pattern, REG_EXTENDED);
      if (err)
	{
	  if (t->expected)
	    return;
	  regerror (err, &r, errmsg, 100);
	  printf ("test %d\n", id);
	  puts (errmsg);
	  return;
	}
      last_pattern = t->pattern;
    }
      
  err = regexec (&r, t->data, 10, regs, 0);

  if (err != t->expected)
    {
      printf ("### test %d\n", id);
      printf ("### pattern \"%s\" data \"%s\" wanted %d got %d\n",
	      t->pattern, t->data, t->expected, err);
      for (x = 0; x < 10; ++x)
	printf ("### reg %d == (%d, %d) %.*s\n",
		x,
		regs[x].rm_so,
		regs[x].rm_eo,
		regs[x].rm_eo - regs[x].rm_so,
		t->data + regs[x].rm_so);
    }

}



main(argc, argv)
     int argc;
     char * argv[];
{
  int x;
  int lo;
  int hi;
  int reps;

  reps = (getenv ("RXREPS")
	  ? atoi (getenv ("RXREPS"))
	  : 1);

#if 0
  {
    union dbmalloptarg val;
    val.i = 1; dbmallopt (MALLOC_CKCHAIN, &val);
  }
#endif

  lo = 0;
  hi = (sizeof (the_tests) / sizeof (the_tests[0])) - 1;

  if (argc > 1)
    {
      lo = atoi (argv[1]);
      hi = lo + 1;

      if (argc > 2)
	hi = atoi (argv[2]);
    }

  for (x = lo; x < hi; ++x)
    {
      {
	int q;
	printf ("#%d\n", x);
	for (q = 0; q < reps; ++q)
	  run_a_test (x, &the_tests[x]);
      }
    }
  {
    if (exit (0))
      print_rexp ();
  }
}


