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

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <kstat.h>
#include <sys/sysinfo.h>
#include <errno.h>

#define FD_IN				0
#define FD_OUT				1
#define FD_ERR				2

#define PING				'p'
#define NPROCS				'n'
#define AVG1				'1'
#define AVG5				'5'
#define AVG15				'f'
#define UTIL				'u'
#define QUIT				'q'


#define CPU_UTIL_CPU_NO			0
#define CPU_UTIL_USER			1
#define CPU_UTIL_KERNEL			2
#define CPU_UTIL_WAIT			3
#define CPU_UTIL_IDLE			4

#define CPU_UTIL_VALUES			5

static unsigned int misc_measure(char* name);
static void util_measure(unsigned int **result_vec, int *result_sz);
static void send(unsigned int data);
static void sendv(unsigned int data[], int ints);
static void error(char* err_msg);

static kstat_ctl_t *kstat_ctl;

int main(int argc, char** argv) {
  char cmd;
  int rc;
  int sz;
  unsigned int *rv;

  kstat_ctl = kstat_open();
  if(!kstat_ctl)
    error("Can't open header kstat");

  while(1) {

    rc = read(FD_IN, &cmd, 1);
    if (rc < 0) {
      if (errno == EINTR)
	continue;
      error("Error reading from Erlang");
    }

    if(rc == 0)
      error("Erlang has closed");
    
    switch(cmd) {
    case PING:		send(4711);					break;
    case NPROCS:	send(misc_measure("nproc"));			break;
    case AVG1:		send(misc_measure("avenrun_1min"));		break;
    case AVG5:		send(misc_measure("avenrun_5min"));		break;
    case AVG15:		send(misc_measure("avenrun_15min"));		break;
    case UTIL:		util_measure(&rv,&sz);	sendv(rv, sz);		break;
    case QUIT:		return 0;
    default:		error("Bad command");				break;
    }
  }
  return 0; /* supress warnings */
}

static unsigned int misc_measure(char* name) {
  static kstat_t *ksp = NULL;
  kstat_named_t* entry;
  kid_t kcid;

  kcid = kstat_chain_update(kstat_ctl);

  if(kcid == -1)
    error("Error updating kstat chain");

  if (!ksp || kcid != 0) {

    /* The kstat chain changed (or we are initializing);
       find system misc entry in the new chain... */
    
    ksp = kstat_lookup(kstat_ctl,"unix",0,"system_misc");
    if(!ksp)
      error("Can't open system_misc kstat");
  }

  kstat_read(kstat_ctl,ksp,NULL);
  entry = kstat_data_lookup(ksp,name);
  if(!entry)
    return -1;
  
  if(entry->data_type != KSTAT_DATA_ULONG)
    return -1;

  return entry->value.ul;
}

static int cpu_cmp(const void *p1, const void *p2) {
  kstat_t *ksp1 = *((kstat_t **) p1);
  kstat_t *ksp2 = *((kstat_t **) p2);
  
  if (ksp1->ks_instance > ksp2->ks_instance)
    return 1;
  if (ksp1->ks_instance < ksp2->ks_instance)
    return -1;
  return 0;
}

static void util_measure(unsigned int **result_vec, int *result_sz) {
  static int no_of_cpus = 0;
  static kstat_t **cpu_ksps = NULL;
  static unsigned int *resv = NULL;
  unsigned int *resp;
  kstat_t *ksp;
  kid_t kcid;
  int cpu_stats_read;
  int i;

  kcid = kstat_chain_update(kstat_ctl);

  if(kcid == -1)
    error("Error updating kstat chain");

  if (no_of_cpus == 0 || kcid != 0) {

    /* The kstat chain changed (or we are initializing);
       find cpu_stat entries in the new chain... */

    no_of_cpus = 0;

    for(ksp = kstat_ctl->kc_chain; ksp; ksp = ksp->ks_next) {
      if (strcmp(ksp->ks_module, "cpu_stat") == 0
	  && ksp->ks_type == KSTAT_TYPE_RAW) {
	no_of_cpus++;
	/* Assumes that modifications of the cpu_stat_t struct
	   in future releases of Solaris only are additions
	   of fields at the end of the struct. */
	if(ksp->ks_data_size < sizeof(cpu_stat_t))
	  error("Error: unexpected kstat data size");
      }
    }

    free((void *) cpu_ksps);
    if (no_of_cpus > 0) {
      cpu_ksps = (kstat_t **) malloc(no_of_cpus*sizeof(kstat_t *));
      if(!cpu_ksps)
	error("Error allocating memory");
    
      i = 0;
      for(ksp = kstat_ctl->kc_chain;
	  ksp && i < no_of_cpus;
	  ksp = ksp->ks_next) {
	if (strcmp(ksp->ks_module, "cpu_stat") == 0
	    && ksp->ks_type == KSTAT_TYPE_RAW) {
	  cpu_ksps[i++] = ksp;
	}
      }

      if (i != no_of_cpus)
	error("Error: private kstat chain copy unexpectedly changed");

      /* Erlang assumes that cpu information are sent in ascending order;
	 sort them ... */
      qsort((void  *)cpu_ksps,(size_t)no_of_cpus,sizeof(kstat_t *),cpu_cmp);

    }

    free((void *) resv);
    resv = (unsigned int *) malloc(sizeof(unsigned int)
				   *(1 + no_of_cpus*CPU_UTIL_VALUES));
    if(!resv)
      error("Error allocating memory");

  }

  /* Read cpu utilization statistics ... */

  resp = resv;
  resp++;
  cpu_stats_read = 0;

  for(i = 0; i < no_of_cpus; i++) {
    if (kstat_read(kstat_ctl, cpu_ksps[i], NULL) != -1) {
      cpu_stat_t *cpu_stat = (cpu_stat_t *)cpu_ksps[i]->ks_data;
      
      resp[CPU_UTIL_CPU_NO] = cpu_ksps[i]->ks_instance;
      resp[CPU_UTIL_USER]   = cpu_stat->cpu_sysinfo.cpu[CPU_USER];
      resp[CPU_UTIL_KERNEL] = cpu_stat->cpu_sysinfo.cpu[CPU_KERNEL];
      resp[CPU_UTIL_WAIT]   = cpu_stat->cpu_sysinfo.cpu[CPU_WAIT];
      resp[CPU_UTIL_IDLE]   = cpu_stat->cpu_sysinfo.cpu[CPU_IDLE];
      
      resp += CPU_UTIL_VALUES;
      
      cpu_stats_read++;
    }
  }

  resv[0] = cpu_stats_read;

  *result_vec = resv;
  *result_sz = 1 + CPU_UTIL_VALUES * cpu_stats_read;

}

static void send(unsigned int data) { sendv(&data, 1); }

static void sendv(unsigned int data[], int ints) {
  static unsigned char *buf = NULL;
  static int bufsz = 0;
  int rc, di, bi, msgsz;

  /* Assumes 32-bit integers... */

  msgsz = 4*ints;

  if(bufsz < msgsz) {
    free((void *) buf);
    buf = malloc(msgsz);
    if (!buf)
      error("Error allocating memory");
    bufsz = msgsz;
  }

  for(bi = 0, di = 0; di < ints; di++) {
    buf[bi++] = (data[di] >> 24) & 0xff;
    buf[bi++] = (data[di] >> 16) & 0xff;
    buf[bi++] = (data[di] >>  8) & 0xff;
    buf[bi++] = (data[di]      ) & 0xff;
  }

  bi = 0;
  do {
    rc = write(FD_OUT, &buf[bi], msgsz - bi);
    if (rc < 0) {
      if (errno == EINTR)
	continue;
      error("Error writing to Erlang");
    }
    bi += rc;
  } while(msgsz - bi > 0);

}

static void error(char* err_msg) {
  write(FD_ERR, err_msg, strlen(err_msg));
  write(FD_ERR, "\n", 1);
  exit(-1);
}


