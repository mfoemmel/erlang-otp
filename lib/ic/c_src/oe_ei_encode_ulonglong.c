#include <ic.h>


int oe_ei_encode_ulonglong(CORBA_Environment *ev, CORBA_unsigned_long_long p) {
  int size = ev->_iout + __OE_ULONGLONGSZ__;

  if (size >= ev->_outbufsz) {
    char *buf = ev->_outbuf;
    int bufsz = ev->_outbufsz + ev->_memchunk;
    
    if ((buf = realloc(buf,bufsz)) != NULL) {
      ev->_outbuf = buf;
      ev->_outbufsz += ev->_memchunk;
    }
    else {
      CORBA_exc_set(ev, CORBA_SYSTEM_EXCEPTION, NO_MEMORY, "End of heap memory while encoding");
      return -1;  /* OUT OF MEMORY */ 
    }
  }
  
  /* CORBA_long_long = long because of erl_interface limitation */
  return ei_encode_ulong(ev->_outbuf, &ev->_iout, p);
}


