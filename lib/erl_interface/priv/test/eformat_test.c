#include <stdio.h>
#include "erl_interface.h"
#include "erl_format.h"
#include "erl_error.h"

#ifdef SUN
extern int fprintf();
#endif

#define PRINT(t) \
{ \
  print_term(stderr,t); \
  fprintf(stderr,"\n"); \
}

int main(int argc, char *argv[])
{
  ETERM *res,*p,*t;
  
  /*  res = erl_format("{~w,~w}", mk_int(99), erl_format("{~w,{~w}}", mk_int(123), mk_atom("madonna")));*/
  /*  res = erl_format("{~a}", "mange"); */
  /* res = erl_format("[{~a,~i},~i,~s]", "mange", 28, 42, "madonna"); */
  res = erl_format("{~i,Kalle,[~a,Kalle]}", 88, "madonna"); 
  PRINT(res);

  res = erl_format("{send, hello}");
  PRINT(res);

  res = erl_format("[]");
  PRINT(res);

  res = erl_format("[\"~p\",hello]");
  PRINT(res);

  p = erl_format("{~a,Kalle,Kalle}", "madonna");
  t = erl_format("{~a,~i,~i}", "madonna", 33, 33); 

  PRINT(p);
  PRINT(t);

  if (erl_match(p, t)) {
    erl_err_msg("YES they matched !!");
    fprintf(stderr,"Content of Kalle is: ");
    PRINT(var_content(p, "Kalle"));
  }
  else {
    erl_err_msg("NO they didn't match !!");
    if (!(var_content(p, "Kalle") == (ETERM *) NULL)) {
      fprintf(stderr, "ERROR: Kalle contains the value: ");
      PRINT(var_content(p, "Kalle"));
    }
  }

  p = erl_format("{ok,_,_}");
  t = erl_format("{ok,23,45}"); 

  PRINT(p);
  PRINT(t);

  if (erl_match(p, t)) 
    erl_err_msg("YES they matched !!");
  else
    erl_err_msg("NO they didn't match !!");


  exit(0);
}



