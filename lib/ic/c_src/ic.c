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


void CORBA_free(void *storage) {
  if (storage != NULL) 
    free(storage); 
}


CORBA_char *CORBA_string_alloc(CORBA_unsigned_long len) {
  return (CORBA_char *) malloc(len+1);
}


CORBA_wchar *CORBA_wstring_alloc(CORBA_unsigned_long len) {
  return (CORBA_wchar *) malloc(len*(__OE_WCHAR_SIZE_OF__+1));
}


CORBA_Environment *CORBA_Environment_alloc(int inbufsz, int outbufsz) {
  CORBA_Environment *ev;

  ev = malloc(sizeof(CORBA_Environment));

  if (ev != NULL) {
    ev->_major = CORBA_NO_EXCEPTION;
    ev->_fd= -1;
    ev->_iin = 0;
    ev->_iout = 0;
    ev->_inbuf = malloc(inbufsz);
    ev->_outbuf = malloc(outbufsz);
    ev->_inbufsz = inbufsz;
    ev->_outbufsz = outbufsz;
    ev->_memchunk = __OE_MEMCHUNK__;
    ev->_received = 0;
    ev->_to_pid = NULL;
    ev->_from_pid = NULL;
    ev->_exc_id = NULL;
    ev->_exc_value = NULL;
    ev->_ref_counter_1 = 0;
    ev->_ref_counter_2 = 0;
    ev->_ref_counter_3 = 0;
  }

  return ev;
}

/* NOT EXPORTED SO FAR 
void CORBA_Environment_free(CORBA_Environment *ev) {

  CORBA_free(ev->_inbuf);
  CORBA_free(ev->_outbuf);
  CORBA_exception_free(ev);
  CORBA_free(ev);
} */


CORBA_char *CORBA_exception_id(CORBA_Environment *ev) {

  return ev->_exc_id;
}



void *CORBA_exception_value(CORBA_Environment *ev) {

  return ev->_exc_value;
}



void CORBA_exception_free(CORBA_Environment *ev) {
 
  /* Setting major value */
  ev->_major=CORBA_NO_EXCEPTION;

  /* Freeing storage */
  CORBA_free(ev->_exc_id);
  CORBA_free(ev->_exc_value);
}



void CORBA_exc_set(CORBA_Environment *ev, 
	      CORBA_exception_type Major, 
	      CORBA_char *Id, 
	      CORBA_char *Value) {

  int ilen,vlen;

  /* Create exception ONLY if there where
     not allready one on the run */ 
  if (ev->_major == CORBA_NO_EXCEPTION) {
    
    /* Counting lengths */
    ilen=strlen(Id)+1;
    vlen=strlen(Value)+1;
    
    /* Allocating storage */
    ev->_exc_id=(CORBA_char *) malloc(ilen);
    ev->_exc_value=(CORBA_char *) malloc(vlen);
    
    /* Initiating */
    ev->_major=Major;
    strcpy(ev->_exc_id,Id);
    strcpy(ev->_exc_value,Value);
  }
}



/* Initiating message reference */
void ic_init_ref(CORBA_Environment *ev, erlang_ref *ref) {

  strcpy(ref->node,erl_thisnodename());
  
  ref->len = 3;

  if(ref->n[0] < 0x3ffff)
    ev->_ref_counter_1 += 1;
  else {
    if(ref->n[1] < 0xffffffff) {
      ev->_ref_counter_1 = 0;
      ev->_ref_counter_2 += 1;
    }
    else {
      if(ref->n[2] < 0xffffffff) {
	ev->_ref_counter_1 = 0;
	ev->_ref_counter_2 = 0;
	ev->_ref_counter_3 += 1;
      }
      else {
	ev->_ref_counter_1 = 0;
	ev->_ref_counter_2 = 0;
	ev->_ref_counter_3 = 0;
      }
    }
  }
    
  ref->n[0] = ev->_ref_counter_1;
  ref->n[1] = ev->_ref_counter_2;
  ref->n[2] = ev->_ref_counter_3;

  ref->creation = erl_thiscreation();

}




/* Comparing message references */
int ic_compare_refs(erlang_ref *ref1, erlang_ref *ref2) {

  if(strcmp(ref1->node,ref2->node) != 0) 
    return -1;
 
  if (ref1->len != ref2->len) 
    return -1;
 
  if (ref1->len == 1) {
    
    if (ref1->n[0] != ref2->n[0]) 
      return -1;
 
    if (ref1->len == 2) {
      
      if (ref1->n[1] != ref2->n[1]) 
        return -1;
      
      if (ref1->len == 3) {
	
        if (ref1->n[2] != ref2->n[2]) 
          return -1;
	
      }
    }
  }

  return 0; 
}


/* Length counter for wide strings */
int ic_wstrlen(CORBA_wchar * p) {
  int len = 0;

  while(1) {
    if (p[len] == 0)
      return len;

    len+=1;
  }
}


/* Wide string compare function */
int ic_wstrcmp(CORBA_wchar * ws1, CORBA_wchar * ws2) {
  int index = 0;

  while(1) {
    if (ws1[index] == ws2[index]) {

      if (ws1[index] == 0)
	return 0;
      
      index += 1;

    } else 
      return -1;
  }
}

