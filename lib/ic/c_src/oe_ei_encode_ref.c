/* ``The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved via the world wide web at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */
#include <ic.h>


int oe_ei_encode_ref(CORBA_Environment *ev, const erlang_ref *p) {
  int size = ev->_iout;
  
  (int) ei_encode_ref(NULL, &size, p);
  
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

  return ei_encode_ref(ev->_outbuf, &ev->_iout, p);
}

