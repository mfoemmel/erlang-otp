#include <ic.h>


int oe_ei_decode_ulonglong(const char *buf, int *index, CORBA_unsigned_long_long *p) {
  return ei_decode_ulong(buf, index, p);
}
