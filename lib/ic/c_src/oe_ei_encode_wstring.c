#include <ic.h>


int oe_ei_encode_wstring(CORBA_Environment *ev, CORBA_wchar *p) {

  int len,wchar,size,tmp,error_code;

  len = ic_wstrlen(p);
  size = ev->_iout + __OE_LISTHDRSZ__ +(len * __OE_WCHARSZ__); 

  if (size >= ev->_outbufsz) {
    char *buf = ev->_outbuf;
    int bufsz = ev->_outbufsz + ev->_memchunk;
    
    while (size >= bufsz)
      bufsz += ev->_memchunk;
    
    if ((buf = realloc(buf, bufsz)) == NULL) {
      CORBA_exc_set(ev, CORBA_SYSTEM_EXCEPTION, NO_MEMORY, "End of heap memory while encoding");
      return -1;  /* OUT OF MEMORY */ 
    }

    ev->_outbuf = buf;
    ev->_outbufsz = bufsz;
  }

  /* Encode the wide string */
  error_code = 0;

  if ((error_code = oe_ei_encode_list_header(ev, len)) < 0)
    return error_code;
  
  for(tmp = 0; tmp < len; tmp++) 
    if ((error_code = oe_ei_encode_wchar(ev, p[tmp])) < 0)
      return error_code;

  if ((error_code = oe_ei_encode_empty_list(ev)) < 0)
    return error_code;

  return 0;
}


