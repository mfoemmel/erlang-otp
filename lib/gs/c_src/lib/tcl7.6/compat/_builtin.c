#include <tcl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>

typedef VOID (*vfp) _ANSI_ARGS_((void));
vfp __new_handler = (vfp) NULL;

extern int write _ANSI_ARGS_((int fd, CONST char *buf, size_t size));
extern vfp set_new_handler _ANSI_ARGS_((vfp handler));
extern void __default_new_handler _ANSI_ARGS_((void));
extern VOID *__builtin_new _ANSI_ARGS_((size_t sz));
extern void __builtin_delete _ANSI_ARGS_((VOID *ptr));
extern VOID *__builtin_vec_new _ANSI_ARGS_((size_t sz));
extern void __builtin_vec_delete _ANSI_ARGS_((VOID *ptr));
extern void __pure_virtual _ANSI_ARGS_((void));

vfp
set_new_handler (vfp handler)
{
  vfp prev_handler;

  prev_handler = __new_handler;
  if (handler == 0) handler = __default_new_handler;
  __new_handler = handler;
  return prev_handler;
}

#define VMEM_EXCEEDED "Virtual memory exceeded in `new'\n"

void
__default_new_handler ()
{
  write (2, VMEM_EXCEEDED, sizeof (VMEM_EXCEEDED));
  _exit (-1);
}

VOID *
__builtin_new (size_t sz)
{
  VOID *p;
  vfp handler = (__new_handler) ? __new_handler : __default_new_handler;

  /* malloc (0) is unpredictable; avoid it.  */
  if (sz == 0)
    sz = 1;
  p = (VOID *) malloc (sz);
  while (p == 0)
    {
      (*handler) ();
      p = (VOID *) malloc (sz);
    }
  
  return p;
}

void
__builtin_delete (VOID *ptr)
{
  if (ptr)
    free (ptr);
}

VOID *
__builtin_vec_new (size_t sz)
{
  return __builtin_new (sz);
}

void
__builtin_vec_delete (VOID *ptr)
{
  __builtin_delete (ptr);
}

#define PURE_VIRT_CALLED "pure virtual method called\n"

void
__pure_virtual ()
{
  write (2, PURE_VIRT_CALLED, sizeof (PURE_VIRT_CALLED));
  _exit (-1);
}

