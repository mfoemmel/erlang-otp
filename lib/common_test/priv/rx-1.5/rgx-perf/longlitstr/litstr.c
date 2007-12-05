/*	Copyright (C) 1996 Tom Lord
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



extern char data[];

#ifdef __STDC__
int
main(int argc, char * argv[])
#else
int
main(argc, argv)
     int argc;
     char * argv[];
#endif
{
  regex_t r;
  regmatch_t regs[10];

  if (regcomp (&r, "back-tracking oriented stream-of-solution functions", REG_EXTENDED))
    {
      printf ("### unexpected compilation error\n");
      exit (1);
    }

  if (regexec (&r, data, 10, regs, 0))
    {
      printf ("### unexpected regexec error\n");
      exit (1);
    }

  {
    int x;

    printf ("regexp is \"back-tracking oriented stream-of-solution functions\"\n");

    for (x = 0; x < 1; ++x)
      printf ("reg %d == (%d, %d) %.*s\n",
	      x,
	      regs[x].rm_so,
	      regs[x].rm_eo,
	      regs[x].rm_eo - regs[x].rm_so,
	      data + regs[x].rm_so);
  }
  exit (0);
}

