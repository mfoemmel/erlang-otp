#include <ic.h>


int oe_ei_decode_wchar(const char *buf, int *index, CORBA_wchar *p) {
  return ei_decode_ulong(buf, index, p);
}
