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
/*
 * CPU supervision
 *
 * Uses kstat library only available on Solaris 2
 * Compile with: gcc -o cpu_sup cpu_sup.c -lkstat
 *
 * Use open_port({spawn,Prog},[stream]) to communicate.
 * 
 */

#include<stdio.h>
#include<kstat.h>

#define FD_IN    0
#define FD_OUT   1
#define FD_ERR   2

#define PING     'p'
#define NPROCS   'n'
#define AVG1     '1'
#define AVG5     '5'
#define AVG15    'f'
#define QUIT     'q'

/*#define DISPLAY_ON_STDERR*/

static unsigned int measure(char* name);
static void send(unsigned int data);
static void error(char* err_msg);

static kstat_ctl_t *kc;
static kstat_t *ks;

int main(int argc, char** argv) {
  kid_t kid;
  char cmd;
  int rc;

  kc = kstat_open();
  if(!kc) error("Can't open header kstat");

  ks = kstat_lookup(kc,"unix",0,"system_misc");
  if(!ks) error("Can't open system_misc kstat");


  while(1) {
    rc = read(FD_IN, &cmd, sizeof(cmd));
    if(rc <= 0)
      error(rc<0?"Error reading from Erlang":"Erlang has closed");
    
    switch(cmd) {
    case PING:   send(4711); break;
    case NPROCS: send(measure("nproc")); break;
    case AVG1:   send(measure("avenrun_1min")); break;
    case AVG5:   send(measure("avenrun_5min")); break;
    case AVG15:  send(measure("avenrun_15min")); break;
    case QUIT:   return 0;
    default:     error("Bad command"); break;
    }
  }
  return 0; /* supress warnings */
}

static unsigned int measure(char* name) {
  kstat_named_t* entry;

  kstat_read(kc,ks,NULL);
  entry = kstat_data_lookup(ks,name);
  if(!entry)
    return -1;
  
  if(entry->data_type != KSTAT_DATA_ULONG)
    return -1;

  return entry->value.ul;
}

static void send(unsigned int data) {
  char buf[4];
  int rc;

  buf[0] = (data >> 24) & 0xff;
  buf[1] = (data >> 16) & 0xff;
  buf[2] = (data >>  8) & 0xff;
  buf[3] = (data      ) & 0xff;

  rc = write(FD_OUT, buf, sizeof(buf));

  if(rc != sizeof(buf))
    error("Error writing to Erlang");

#ifdef DISPLAY_ON_STDERR
  { /* Debugging aid */
    char text[80];
    sprintf(text,"%d (%d,%d,%d,%d)\n",data,buf[0],buf[1],buf[2],buf[3]);
    rc = write(FD_ERR,text,strlen(text));
  }
#endif
}

static void error(char* err_msg) {
  write(FD_ERR, err_msg, strlen(err_msg));
  write(FD_ERR, "\n", 1);
  exit(-1);
}
