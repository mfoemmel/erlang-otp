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
/* Just include the interface function */
#include "rmod_random.h"


/* Assign your own node name here */
#define SNODE "babbis@balin"
#define SERVER "rmod_random_impl"
#define COOKIE "flash"
#define CLNODE "c47@balin"
#define INBUFSZ 1024
#define OUTBUFSZ 1024

/* Stopping node */
void client_exit(CORBA_Environment *env) {

  /* Free env & buffers */
  CORBA_free(env->_inbuf);
  CORBA_free(env->_outbuf);
  CORBA_free(env);

  erl_close_connection(env->_fd);
  exit(1);
}

int main(){
  
  double result=0;
  int i=0;
  erlang_pid pid;
  CORBA_Environment *env;
  
  /* Create and init CORBA_Environment */
  env = CORBA_Environment_alloc(INBUFSZ,OUTBUFSZ);
  
  /* Initiating the connection */
  erl_init(NULL,0);
  erl_connect_init(47,COOKIE,0);

  /* Initiating pid*/
  strcpy(pid.node,CLNODE);
  pid.num = 99;
  pid.serial = 0;
  pid.creation = 0;

  /* Fixing environment variable */
  env->_fd=erl_connect(SNODE);
  strcpy(env->_regname,SERVER);
  env->_to_pid = NULL;
  env->_from_pid = &pid;
  
  if (env->_fd < 0) {
    fprintf(stderr,"Error : Cannot connect to Server\n");

    /* Free env & buffers */
    CORBA_free(env->_inbuf);
    CORBA_free(env->_outbuf);
    CORBA_free(env);
    exit(1);
  }
  
  /* Calling the init function */
  rmod_random_init(NULL, 1, 2, 3, env);
 
  switch(env->_major) {
  case CORBA_NO_EXCEPTION: /* Success */
    printf("Init complete !\n");
    break;
  case CORBA_SYSTEM_EXCEPTION: /* System exception */
    printf("Init call failure, reason : %s\n",(char *) CORBA_exception_value(env));
    CORBA_exception_free(env);
    client_exit(env);
  default: /* Should not come here */
    client_exit(env);
  }

  /* Calling the produce function */
  for(i=1; i<=10; i++) {
    result = rmod_random_produce(NULL, env);

    switch(env->_major) {
    case CORBA_NO_EXCEPTION: /* Success */
      break;
    case CORBA_SYSTEM_EXCEPTION: /* System exception */
      printf("Init call failure, reason : %s\n",(char *) CORBA_exception_value(env));
      CORBA_exception_free(env);
      client_exit(env);
    default: /* Should not come here */
      client_exit(env);
    }

    printf("the random number nr%d is %f\n",i,result);
  }

  /* Closing the connection */
  erl_close_connection(env->_fd);
  
  /* Free env & buffers */
  CORBA_free(env->_inbuf);
  CORBA_free(env->_outbuf);
  CORBA_free(env);

  return 0;
}

