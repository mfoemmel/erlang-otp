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

/**
 **
   This file is compiled into a lib which must be included when
   linking dynaimc drivers and port programs.
 **
 **/

#include "ose.h"
#include "stdio.h"
#include "string.h"
#include "efs.h"

/****************** Program Reg Decl ********************/
/* Make sure this part is consistent with erl_ose_sys.h */

/* the name of the erts OSE process */
#define ERTS_OSE_PROC_NAME "erts"

#define PGM_SERVER "erl_sys_pgm_server"
#define REG_PGM    1
#define GET_PGM    2
#define DEL_PGM    3
#define ADD_HND    4

typedef struct pgm_entry {
  SIGSELECT sigNo;
  void *hnd;
  void *entrypoint;
  int is_static;
  char name[1];
} pgmEntry;

union pgmSig {
  SIGSELECT sigNo;
  struct pgm_entry pgm;
};

/*********************************************************/

/* 
   search for data given name or entrypoint

   name == NULL:    search on entrypoint, ignore name value
   name[0] == '\0': search on entrypoint, fill name with found program name
   name == <pgm>  : search on name, possibly set entrypoint to found value

   no search on hnd or is_static
*/
static int get_pgm_data(char *name, void **entrypoint, void **hnd, int *is_static) {
  union pgmSig *sig;
  static const SIGSELECT select[] = {1, GET_PGM};
  PROCESS server_;

  if(!hunt(PGM_SERVER, 0, &server_, NULL)) {
    fprintf(stderr, "sys_get_pgm_data: %s not running!\n", PGM_SERVER);
    return 0;
  }

  if((name != NULL) && (name[0] != '\0')) { /* name is key */
    sig = (union pgmSig *)alloc(sizeof(pgmEntry) + strlen(name), GET_PGM);  
    strcpy(sig->pgm.name, name);   
  } else {
    sig = (union pgmSig *)alloc(sizeof(pgmEntry) + 256, GET_PGM);
    sig->pgm.name[0] = '\0';
  }

  if(entrypoint != NULL) 
    sig->pgm.entrypoint = *entrypoint;
  else
    sig->pgm.entrypoint = NULL;

  printf("Sending %s to reg server (%x)\n", sig->pgm.name, server_);
  send((union SIGNAL **)&sig, server_);
  sig = (union pgmSig *)receive((SIGSELECT *) select);

  if(sig->pgm.entrypoint == NULL) { /* not found */
    free_buf((union SIGNAL **)&sig);
    return 0;
  }
  if(name != NULL) strcpy(name, sig->pgm.name);
  if(entrypoint != NULL) *entrypoint = sig->pgm.entrypoint;
  if(hnd != NULL)        *hnd =        sig->pgm.hnd;
  if(is_static != NULL)  *is_static =  sig->pgm.is_static;
  free_buf((union SIGNAL **)&sig);
  return 1;
}

static int reg_pgm(char *name, void *entry, PROCESS from_) {
  union pgmSig *sig;
  PROCESS erts_, server_;

  hunt(ERTS_OSE_PROC_NAME, 0, &erts_, NULL);
  
  if(!hunt(PGM_SERVER, 0, &server_, NULL)) {
    fprintf(stderr, "sys_reg_pgm(%s): %s not running!\n", name, PGM_SERVER);
    kill_proc(current_process());
  }
  sig = (union pgmSig *)alloc(sizeof(pgmEntry) + strlen(name), REG_PGM);
  strcpy(sig->pgm.name, name);
  sig->pgm.hnd = NULL;
  sig->pgm.entrypoint = entry;
  if(from_ == erts_)		/* static programs are reg. by erts_ */
    sig->pgm.is_static = 1;    
  else 
    sig->pgm.is_static = 0;
  send((union SIGNAL **)&sig, server_);
  return 0;
}

static void del_pgm(char *name) {
  union pgmSig *sig;
  PROCESS server_;
  
  if(!hunt(PGM_SERVER, 0, &server_, NULL)) {
    fprintf(stderr, "sys_del_pgm(%s): %s not running!\n", name, PGM_SERVER);
    kill_proc(current_process());
  }
  sig = (union pgmSig *)alloc(sizeof(pgmEntry) + strlen(name), DEL_PGM);
  strcpy(sig->pgm.name, name);
  send((union SIGNAL **)&sig, server_);
}

int unreg_pgm(char *name) {
  int is_static;

  if(get_pgm_data(name, NULL, NULL, &is_static)) {
    if(is_static) {
      fprintf(stderr, "%s is static and cannot be unregistered\n", name);
      return -1;
    } else {
      del_pgm(name);
      return 0;
    }
  }
  fprintf(stderr, "%s is not a registered program\n", name);
  return -1;
}

/* exported functions for registering static user drivers and port programs
   (dynamic programs use a lib) */
int erl_reg_port_prog(char *name, OSENTRYPOINT *entrypoint, PROCESS from_) {
  reg_pgm(name, (void*)entrypoint, from_);
}
int erl_unreg_port_prog(char *name) {
  unreg_pgm(name);
}
int erl_reg_driver(char *name, void *drv_init_func, PROCESS from_) {
  reg_pgm(name, drv_init_func, from_);
}
int erl_unreg_driver(char *name) {
  unreg_pgm(name);
}
