#include <ic.h>


int oe_ei_encode_wchar(CORBA_Environment *ev, CORBA_wchar p) {
  return oe_ei_encode_ulong(ev, p);
}


