/* Created:  11 May 1996 by tobbe@cslab.ericsson.se
 * Function: Test cases for the erl_interface library.
 */
#include <stdio.h>
#include "erl_interface.h"

#ifdef SUNOS4
extern int fprintf();
#endif

#define NL fprintf(stderr,"\n")

#define TC(i) fprintf(stderr,"\n====== TEST CASE %d ======\n",i)

#define MEM_DUMP() memory_dump()

#define PRINT_CMP(exp,i) \
if ((i = exp) < 0) \
  fprintf(stderr, " < "); \
else if (i > 0) \
  fprintf(stderr, " > "); \
else \
  fprintf(stderr, " == ")

/* forward */
void dump(unsigned char*, int);
void do_memory_dump(const char*);
void memory_dump(int);

void main() 
{
  ETERM *arr[4],*list,*list2,*ep,*ep2,*tuple,*term,*pattern;
  ETERM *tmp1,*tmp2;
  unsigned char buf[BUFSIZ],*bp;
  unsigned char buf2[BUFSIZ];
  int i;

  /* Initialize the library */
  erl_init((Erl_Heap *) NULL, 0);

  /* Build a list, print and deallocate it */
  TC(0);
  arr[0] = erl_mk_int(32);
  arr[1] = erl_mk_atom("tobbe");
  arr[2] = erl_mk_float(3.14);
  arr[3] = erl_mk_string("madonna");
  list = erl_mk_list(arr, 4);
  erl_print_term(stderr, list); NL;
  MEM_DUMP();
  erl_free_term(list);
  MEM_DUMP();
  erl_free_array(arr, 4);
  MEM_DUMP();

  /* Build a list and print it */
  TC(1);
  arr[0] = erl_mk_int(32);
  arr[1] = erl_mk_atom("tobbe");
  arr[2] = erl_mk_float(3.14);
  arr[3] = erl_mk_string("madonna");
  list = erl_mk_list(arr, 4);
  erl_print_term(stderr, list); NL;
  MEM_DUMP();

  /* Use some components of the first list to build a new list */
  TC(2);
  erl_free_term(arr[0]);
  arr[0] = arr[1];
  arr[1] = arr[3];
  erl_free_term(arr[2]);
  arr[2] = erl_mk_list(arr, 2);
  MEM_DUMP();

  /* Deallocate the first list */
  TC(3);
  MEM_DUMP();
  erl_free_term(list);
  MEM_DUMP();
  erl_free_array(arr, 2);
  MEM_DUMP();

  /* The second list shall still be intact */
  TC(4);
  erl_print_term(stderr, arr[2]); NL;

  /* Deallocate the second list */
  TC(5);
  MEM_DUMP();
  erl_free_term(arr[2]);
  MEM_DUMP();

  /* Build a tuple,encode and print it */
  TC(6);
  arr[0] = erl_mk_int(32);
  arr[1] = erl_mk_atom("tobbe");
  arr[2] = erl_mk_float(3.14);
  arr[3] = erl_mk_string("madonna");
  tuple  = erl_mk_tuple(arr, 4);
  erl_print_term(stderr,tuple); NL;
  MEM_DUMP();
  erl_free_term(tuple);
  MEM_DUMP();
  erl_free_array(arr, 4);
  MEM_DUMP();

  /* Check the length of well formed cons-lists */
  TC(7);
  arr[0] = erl_mk_int(32);
  arr[1] = erl_mk_atom("tobbe");
  list2  = erl_cons(arr[1], (ETERM *) NULL);
  list   = erl_cons(arr[0], list2);
  erl_free_term(list2);
  erl_print_term(stderr, list);
  fprintf(stderr," Length == %d\n", erl_length(list));
  MEM_DUMP();
  erl_free_term(list);
  MEM_DUMP();
  erl_free_term(arr[0]);
  erl_free_term(arr[1]);
  MEM_DUMP();

  /* Check the length of non-well formed cons-lists */
  TC(8);
  arr[0] = erl_mk_int(32);
  arr[1] = erl_mk_atom("tobbe");
  list   = erl_cons(arr[0], arr[1]);
  erl_print_term(stderr, list); 
  fprintf(stderr," Length == %d\n", erl_length(list));
  MEM_DUMP();
  erl_free_term(list);
  MEM_DUMP();
  erl_free_term(arr[0]);
  erl_free_term(arr[1]);
  MEM_DUMP();

  /* Check the size of a tuple */
  TC(9);
  arr[0] = erl_mk_int(32);
  arr[1] = erl_mk_atom("tobbe");
  arr[2] = erl_mk_float(3.14);
  arr[3] = erl_mk_string("madonna");
  tuple  = erl_mk_tuple(arr, 4);
  erl_print_term(stderr,tuple); 
  fprintf(stderr," size == %d\n", erl_size(tuple));
  MEM_DUMP();
  erl_free_term(tuple);
  MEM_DUMP();
  erl_free_array(arr, 4);
  MEM_DUMP();

  /* Build a list,encode,decode and print it */
  TC(10);
  arr[0] = erl_mk_int(32);
  arr[1] = erl_mk_atom("tobbe");
  arr[2] = erl_mk_float(3.14);
  arr[3] = erl_mk_string("madonna");
  list   = erl_mk_list(arr, 4);
  MEM_DUMP();
  erl_free_array(arr, 4);
  MEM_DUMP();
  if ((i = erl_encode(list, buf)) != 0) 
    if ((ep = erl_decode(buf)) != NULL) {
      erl_print_term(stderr, ep); 
      fprintf(stderr," encoded into %d bytes, then decoded\n", i);
    }
  MEM_DUMP();
  erl_free_term(list);
  MEM_DUMP();
  erl_free_term(ep);
  MEM_DUMP();

  /* Build a tuple,encode,decode and print it */
  TC(11);
  arr[0] = erl_mk_int(32);
  arr[1] = erl_mk_atom("tobbe");
  arr[2] = erl_mk_float(3.14);
  arr[3] = erl_mk_string("madonna");
  tuple  = erl_mk_tuple(arr, 4);
  erl_free_array(arr, 4);
  if ((i = erl_encode(tuple, buf)) != 0) 
    if ((ep = erl_decode(buf)) != NULL) {
      erl_print_term(stderr, ep); 
      fprintf(stderr," encoded into %d bytes, then decoded\n", i);
    }
  MEM_DUMP();
  erl_free_term(tuple);
  MEM_DUMP();
  erl_free_term(ep);
  MEM_DUMP();

  /* Build a list,encode it, jump forward and then decode it */
  TC(12);
  arr[0] = erl_mk_int(32);
  arr[1] = erl_mk_atom("tobbe");
  arr[2] = erl_mk_float(3.14);
  arr[3] = erl_mk_string("madonna");
  list   = erl_mk_list(arr, 4);
  erl_free_array(arr, 4);
  if ((i = erl_encode(list, buf)) != 0) {
    bp = erl_peek_ext(buf, 2);
    if ((ep = erl_decode(bp)) != NULL) {
      fprintf(stderr,"Original: ");
      erl_print_term(stderr, list); 
      fprintf(stderr," ,after jumped forward 2 elements: ");
      erl_print_term(stderr, ep); NL;
    }
  }
  MEM_DUMP();
  erl_free_term(list);
  MEM_DUMP();
  erl_free_term(ep);
  MEM_DUMP();
  
  /* Build a tuple,encode it, jump forward and then decode it */
  TC(13);
  arr[0] = erl_mk_int(32);
  arr[1] = erl_mk_atom("tobbe");
  arr[2] = erl_mk_float(3.14);
  arr[3] = erl_mk_string("madonna");
  tuple  = erl_mk_tuple(arr, 4);
  erl_free_array(arr, 4);
  if ((i = erl_encode(tuple, buf)) != 0) {
    bp = erl_peek_ext(buf, 3);
    if ((ep = erl_decode(bp)) != NULL) {
      fprintf(stderr,"Original: ");
      erl_print_term(stderr, tuple); 
      fprintf(stderr," ,after jumped forward 3 elements: ");
      erl_print_term(stderr, ep); NL;
    }
  }
  MEM_DUMP();
  erl_free_term(tuple);
  MEM_DUMP();
  erl_free_term(ep);
  MEM_DUMP();

  /* Build two lists the cons-way, print and deallocate them */
  TC(14);
  list = erl_cons(erl_mk_atom("one"), NULL);
  list = erl_cons(erl_mk_atom("two"), list);
  list = erl_cons(erl_mk_atom("three"), list);
  erl_print_term(stderr, list); NL;
  list2 = erl_cons(erl_mk_atom("ett"), NULL);
  list2 = erl_cons(erl_mk_atom("tvaa"), list2);
  list2 = erl_cons(erl_mk_atom("tre"), list2);
  erl_print_term(stderr, list2); NL;
  MEM_DUMP();
  erl_free_compound(list);
  MEM_DUMP();
  erl_free_compound(list2);
  MEM_DUMP();

  /* Build two lists, encode, compare, decode and print them */
  TC(15);
  list = erl_cons(erl_mk_atom("one"), NULL);
  list = erl_cons(erl_mk_atom("two"), list);
  list = erl_cons(erl_mk_atom("three"), list);
  erl_print_term(stderr, list); NL;
  if (!erl_encode(list, buf)) erl_err_quit("<TC 15.1> FAILED !!\n");
  list2 = erl_cons(erl_mk_atom("ett"), NULL);
  list2 = erl_cons(erl_mk_atom("tvaa"), list2);
  list2 = erl_cons(erl_mk_atom("tre"), list2);
  erl_print_term(stderr, list2); NL;
  if (!erl_encode(list2, buf2)) erl_err_quit("<TC 15.2> FAILED !!\n");
  erl_print_term(stderr, (ep = erl_decode(buf)));
  PRINT_CMP(erl_compare_ext(buf,buf2), i); 
  erl_print_term(stderr, (ep2 = erl_decode(buf2))); NL;
  MEM_DUMP();
  erl_free_compound(list);
  MEM_DUMP();
  erl_free_compound(list2);
  MEM_DUMP();
  erl_free_term(ep);
  MEM_DUMP();
  erl_free_term(ep2);
  MEM_DUMP();

  /* Build a term using erl_format, print and deallocate it */
  TC(16);
  ep2 = erl_format("[{adr,~s,~i}]","E-street",42);
  ep  = erl_format("[{name,~a},{age,~i},{data,~w}]",
		  "madonna", 21, ep2);
  erl_print_term(stderr, ep); NL;
  MEM_DUMP();
  erl_free_term(ep);
  MEM_DUMP();
  erl_free_term(ep2);
  MEM_DUMP();

  /* Build a term and a pattern, try to match them, etc... */
  TC(17);
  pattern = erl_format("{madonna,Age,Age}");
  term    = erl_format("{madonna,21,21}");
  erl_print_term(stderr, pattern); NL;
  erl_print_term(stderr, term); NL;
  if (erl_match(pattern, term)) {
    fprintf(stderr, "Yes: %s = ", "Age");
    ep = erl_var_content(pattern, "Age"); 
    erl_print_term(stderr, ep); NL;
    MEM_DUMP();
    erl_free_term(ep);
    MEM_DUMP();
  }
  else 
    fprintf(stderr, "No: <ERROR IN PATTERN MATCH !!>\n");
  MEM_DUMP();
  erl_free_term(term);
  MEM_DUMP();
  erl_free_term(pattern);
  MEM_DUMP();

  /* Build a tuple containing a Pid and deallocate it in parts */
  TC(18);
  arr[0] = erl_mk_int(6);
  arr[1] = erl_mk_pid("noname@nohost",1,2,3);
  arr[2] = erl_mk_atom("rex");
  tuple  = erl_mk_tuple(arr, 3);
  erl_print_term(stderr,tuple); NL;
  MEM_DUMP();
  erl_free_array(arr, 3);
  MEM_DUMP();
  ep  = erl_element(2, tuple);
  ep2 = erl_copy_term(ep);
  MEM_DUMP();
  erl_free_compound(tuple);
  MEM_DUMP();
  erl_free_term(ep2);
  MEM_DUMP();

  /* Build a list, copy and deallocate them */
  TC(19);
  arr[0] = erl_mk_int(6);
  arr[1] = erl_mk_pid("noname@nohost",1,2,3);
  arr[2] = erl_mk_atom("rex");
  list   = erl_mk_list(arr, 3);
  erl_print_term(stderr,list); NL;
  MEM_DUMP();
  erl_free_array(arr, 3);
  MEM_DUMP();
  ep = erl_copy_term(list);
  erl_free_term(list);
  MEM_DUMP();
  erl_free_term(ep);
  MEM_DUMP();

  /* Build a list, deallocate each part by itself */
  TC(20);
  tmp1 = erl_mk_atom("one");
  list = erl_cons(tmp1, NULL);
  erl_free_term(tmp1);
  tmp1 = erl_mk_atom("two");
  tmp2 = erl_cons(tmp1, list);
  erl_free_term(tmp1);
  erl_free_term(list);
  tmp1 = erl_mk_atom("three");
  list = erl_cons(tmp1, tmp2);
  erl_free_term(tmp1);
  erl_free_term(tmp2);
  erl_free_term(list);
  MEM_DUMP();
 
  /* Test the following bug report */
  TC(21);
  ep2 = erl_format("~a","false");
  ep  = erl_format("{file,_}");
  erl_print_term(stderr, ep); NL;
  MEM_DUMP();
  if (erl_match(ep,ep2))
    fprintf(stderr,"  YES, they matched !\n");
  else
    fprintf(stderr,"  NO, they didn't match !\n");
  MEM_DUMP();
  /* according to the bug report we got a core dump here */
  erl_free_term(ep);
  MEM_DUMP();
  erl_free_term(ep2);
  MEM_DUMP();

  /* As TC(17) but now with lists */
  TC(22);
  pattern = erl_format("[madonna,Age,Age]");
  term    = erl_format("[madonna,21,21]");
  erl_print_term(stderr, pattern); NL;
  erl_print_term(stderr, term); NL;
  if (erl_match(pattern, term)) {
    fprintf(stderr, "Yes: %s = ", "Age");
    ep = erl_var_content(pattern, "Age"); 
    erl_print_term(stderr, ep); NL;
    MEM_DUMP();
    erl_free_term(ep);
    MEM_DUMP();
  }
  else 
    fprintf(stderr, "No: <ERROR IN PATTERN MATCH !!>\n");
  MEM_DUMP();
  erl_free_term(term);
  MEM_DUMP();
  erl_free_term(pattern);
  MEM_DUMP();

  /* Bugfix: Test that we can create and print atoms ok */
  TC(23);
  ep = erl_format("'hello world'");
  ep2 = erl_format("{~a}","aaaa^");
  erl_print_term(stderr, ep); NL;
  erl_print_term(stderr, ep2); NL;
  MEM_DUMP();
  erl_free_term(ep);
  erl_free_term(ep2);
  MEM_DUMP();

  /* Bugfix: Test the length of an empty list */
  TC(24);
  ep = erl_format("[]");
  erl_print_term(stderr, ep); 
  fprintf(stderr, " , length is: %d\n", erl_length(ep));
  MEM_DUMP();
  erl_free_term(ep);
  MEM_DUMP();

  /* 
   * Release all memory allocated by the fix-allocator.
   * This will test the fix-allocator when we are running Purify.
   */
  TC(101);
  MEM_DUMP();
  erl_fix_release();
  MEM_DUMP();
  
}

/*
 * Dump an encoded byte array. Typically use is:
 *   dump(bp, erl_ext_size(bp));
 */
void dump(unsigned char *bp, int sz)
{
  int i;

  for (i=0; i<sz; i++)
    fprintf(stderr,"%d ",(int) bp[i]);
  NL;
}

/* 
 * Print out the content of the allocated blocks
 */
void memory_dump(void) 
{
  int ab,fb;

  fprintf(stderr, "Content in allocated blocks: <<"); 
  erl_eterm_statistics(&ab, &fb, do_memory_dump); 
  fprintf(stderr, ">>\n"); 
  fprintf(stderr, "Allocated blocks: %d , Free blocks: %d\n", ab, fb);
}

/* 
 * The actual print routine called from within erl_fix_statistics.
 */
void do_memory_dump(const char *mp)
{
  fprintf(stderr, "(");
  erl_print_term(stderr, (ETERM *) mp);
  fprintf(stderr, " , %d) ", ERL_COUNT(mp));
}

