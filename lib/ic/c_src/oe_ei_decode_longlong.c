#include <ic.h>


int oe_ei_decode_longlong(const char *buf, int *index, CORBA_long_long *p) {
  return ei_decode_long(buf, index, p);
}
