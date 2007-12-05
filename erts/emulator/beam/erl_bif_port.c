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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#ifdef _OSE_
#  include "ose.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "erl_sys_driver.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "bif.h"
#include "big.h"
#include "dist.h"
#include "erl_version.h"
#include "erl_binary.h"
#include "erl_db_util.h"
#include "register.h"
#include "external.h"

extern ErlDrvEntry fd_driver_entry;
extern ErlDrvEntry vanilla_driver_entry;
extern ErlDrvEntry spawn_driver_entry;

static int open_port(Process* p, Eterm name, Eterm settings, int *err_nump);
static byte* convert_environment(Process* p, Eterm env);

BIF_RETTYPE open_port_2(BIF_ALIST_2)
{
    int port_num;
    Eterm port_val;
    char *str;
    int err_num;

    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);

    if ((port_num = open_port(BIF_P, BIF_ARG_1, BIF_ARG_2, &err_num)) < 0) {
	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
	if (port_num == -3) {
	    ASSERT(err_num == BADARG || err_num == SYSTEM_LIMIT);
	    BIF_ERROR(BIF_P, err_num);
	} else if (port_num == -2) {
	    str = erl_errno_id(err_num);
	} else {
	    str = "einval";
	}
	BIF_P->fvalue = am_atom_put(str, strlen(str));
	BIF_ERROR(BIF_P, EXC_ERROR);
    }

    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_LINK);

    port_val = erts_port[port_num].id;
    erts_add_link(&(erts_port[port_num].nlinks), LINK_PID, BIF_P->id);
    erts_add_link(&(BIF_P->nlinks), LINK_PID, port_val);

    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_LINK);

    erts_port_release(&erts_port[port_num]);

    BIF_RET(port_val);
}

/****************************************************************************

  PORT BIFS:

           port_command/2   -- replace Port ! {..., {command, Data}}
               port_command(Port, Data) -> true
               when port(Port), io-list(Data)

           port_control/3   -- new port_control(Port, Ctl, Data) -> Reply
	      port_control(Port, Ctl, Data) -> Reply
              where integer(Ctl), io-list(Data), io-list(Reply)

           port_close/1     -- replace Port ! {..., close}
             port_close(Port) -> true
             when port(Port)

           port_connect/2   -- replace Port ! {..., {connect, Pid}}
              port_connect(Port, Pid) 
              when port(Port), pid(Pid)

 ***************************************************************************/

static Port*
id_or_name2port(Process *c_p, Eterm id)
{
    Port *port;
    if (is_not_atom(id))
	port = erts_id2port(id, c_p, ERTS_PROC_LOCK_MAIN);
    else
	erts_whereis_name(c_p, ERTS_PROC_LOCK_MAIN, id, NULL, 0, 0, &port);
    return port;
}

BIF_RETTYPE port_command_2(BIF_ALIST_2)
{
    BIF_RETTYPE res;
    Port *p;

    /* Trace sched out before lock check wait */    
    if (IS_TRACED_FL(BIF_P, F_TRACE_SCHED_PROCS)) {
	trace_virtual_sched(BIF_P, am_out);
    }
    
    if (erts_system_profile_flags.runnable_procs && erts_system_profile_flags.exclusive) {
    	profile_runnable_proc(BIF_P, am_inactive);
    }

    p = id_or_name2port(BIF_P, BIF_ARG_1);
    if (!p) {
    	if (IS_TRACED_FL(BIF_P, F_TRACE_SCHED_PROCS)) {
	    trace_virtual_sched(BIF_P, am_in);
	}
    	if (erts_system_profile_flags.runnable_procs && erts_system_profile_flags.exclusive) {
    	    profile_runnable_proc(BIF_P, am_active);
    	}
	BIF_ERROR(BIF_P, BADARG);
    }
    
    /* Trace port in, id_or_name2port causes wait */

    if (IS_TRACED_FL(p, F_TRACE_SCHED_PORTS)) {
	trace_sched_ports_where(p, am_in, am_command);
    }
    if (erts_system_profile_flags.runnable_ports && !erts_port_is_scheduled(p)) {
    	profile_runnable_port(p, am_active);
    }

    ERTS_BIF_PREP_RET(res, am_true);

    if (p->status & ERTS_PORT_SFLG_PORT_BUSY) {
	erts_suspend(BIF_P, ERTS_PROC_LOCK_MAIN, p);
	if (erts_system_monitor_flags.busy_port) {
	    monitor_generic(BIF_P, am_busy_port, p->id);
	}
	ERTS_BIF_PREP_ERROR(res, BIF_P, RESCHEDULE);
    
    } else {
	int wres;
	erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	ERTS_SMP_CHK_NO_PROC_LOCKS;
	wres = erts_write_to_port(BIF_P->id, p, BIF_ARG_2);
	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
	if (wres != 0) {
	    ERTS_BIF_PREP_ERROR(res, BIF_P, BADARG);
	}
    }
    
    if (IS_TRACED_FL(p, F_TRACE_SCHED_PORTS)) {
	trace_sched_ports_where(p, am_out, am_command);
    }
    if (erts_system_profile_flags.runnable_ports && !erts_port_is_scheduled(p)) {
    	profile_runnable_port(p, am_inactive);
    }

    erts_port_release(p);
    /* Trace sched in after port release */
    if (IS_TRACED_FL(BIF_P, F_TRACE_SCHED_PROCS)) {
	trace_virtual_sched(BIF_P, am_in);
    }
    if (erts_system_profile_flags.runnable_procs && erts_system_profile_flags.exclusive) {
    	profile_runnable_proc(BIF_P, am_active);
    }
    
    if (ERTS_PROC_IS_EXITING(BIF_P)) {
	KILL_CATCHES(BIF_P);	/* Must exit */
	ERTS_BIF_PREP_ERROR(res, BIF_P, EXC_ERROR);
    }
    return res;
}

BIF_RETTYPE port_call_2(BIF_ALIST_2)
{
    return port_call_3(BIF_P,BIF_ARG_1,make_small(0),BIF_ARG_2);
}

BIF_RETTYPE port_call_3(BIF_ALIST_3)
{
    Uint op;
    Port *p;
    Uint size;
    byte *bytes;
    byte *endp;
    size_t real_size;
    ErlDrvEntry *drv;
    byte port_input[256];	/* Default input buffer to encode in */
    byte port_result[256];	/* Buffer for result from port. */
    byte* port_resp;		/* Pointer to result buffer. */
    char *prc;
    int ret;
    Eterm res;
    Sint result_size;
    Eterm *hp;
    Eterm *hp_end;              /* To satisfy hybrid heap architecture */
    unsigned ret_flags = 0U;

    bytes = &port_input[0];
    port_resp = port_result;
    /* trace of port scheduling with virtual process descheduling
     * lock wait 
     */
    if (IS_TRACED_FL(BIF_P, F_TRACE_SCHED_PROCS)) {
    	trace_virtual_sched(BIF_P, am_out);
    }

    if (erts_system_profile_flags.runnable_procs && erts_system_profile_flags.exclusive) {
    	profile_runnable_proc(BIF_P, am_inactive);
    }

    p = id_or_name2port(BIF_P, BIF_ARG_1);
    if (!p) {
    error:
	if (port_resp != port_result && 
	    !(ret_flags & DRIVER_CALL_KEEP_BUFFER)) {
	    driver_free(port_resp);
	}
	if (bytes != &port_input[0])
	    erts_free(ERTS_ALC_T_PORT_CALL_BUF, bytes);
	/* Need to virtual schedule in the process if there
	 * was an error.
	 */
    	if (IS_TRACED_FL(BIF_P, F_TRACE_SCHED_PROCS)) {
    	    trace_virtual_sched(BIF_P, am_in);
    	}

	if (erts_system_profile_flags.runnable_procs && erts_system_profile_flags.exclusive) {
    	    profile_runnable_proc(BIF_P, am_active);
    	}

	if (p)
	    erts_port_release(p);
#ifdef ERTS_SMP
	ERTS_SMP_BIF_CHK_PENDING_EXIT(BIF_P, ERTS_PROC_LOCK_MAIN);
#else
	ERTS_BIF_CHK_EXITED(BIF_P);
#endif
	BIF_ERROR(BIF_P, BADARG);
    }

    if ((drv = p->drv_ptr) == NULL) {
	goto error;
    }
    if (drv->call == NULL) {
	goto error;
    }
    if (!term_to_Uint(BIF_ARG_2, &op)) {
	goto error;
    }
    p->caller = BIF_P->id;
    
    /* Lock taken, virtual schedule of port */
    if (IS_TRACED_FL(p, F_TRACE_SCHED_PORTS)) {
    	trace_sched_ports_where(p, am_in, am_call);
    }
    
    if (erts_system_profile_flags.runnable_ports && !erts_port_is_scheduled(p)) {
    	profile_runnable_port(p, am_active);
    }
    size = encode_size_struct(BIF_ARG_3, TERM_TO_BINARY_DFLAGS);
    if (size > sizeof(port_input))
	bytes = erts_alloc(ERTS_ALC_T_PORT_CALL_BUF, size);

    endp = bytes;
    if (erts_to_external_format(NULL, BIF_ARG_3, &endp, NULL, NULL) || !endp) {
	erl_exit(1, "%s, line %d: bad term: %x\n",
		 __FILE__, __LINE__, BIF_ARG_3);
    }

    real_size = endp - bytes;
    if (real_size > size) {
	erl_exit(1, "%s, line %d: buffer overflow: %d word(s)\n",
		 __FILE__, __LINE__, endp - (bytes + size));
    }
    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    prc  = (char *) port_resp;
    ret = drv->call((ErlDrvData)p->drv_data, 
		    (unsigned) op,
		    (char *) bytes, 
		    (int) real_size,
		    &prc, 
		    (int) sizeof(port_result),
		    &ret_flags);
    
    if (IS_TRACED_FL(p, F_TRACE_SCHED_PORTS)) {
    	trace_sched_ports_where(p, am_out, am_call);
    }
    
    if (erts_system_profile_flags.runnable_ports && !erts_port_is_scheduled(p)) {
    	profile_runnable_port(p, am_inactive);
    }
   
    port_resp = (byte *) prc;
    p->caller = NIL;
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
#ifdef HARDDEBUG
    { 
	int z;
	printf("real_size = %ld,%d, ret = %d\r\n",real_size, 
	       (int) real_size, ret);
	printf("[");
	for(z = 0; z < real_size; ++z) {
	    printf("%d, ",(int) bytes[z]);
	}
	printf("]\r\n");
	printf("[");
	for(z = 0; z < ret; ++z) {
	    printf("%d, ",(int) port_resp[z]);
	}
	printf("]\r\n");
    }
#endif
    if (ret <= 0 || port_resp[0] != VERSION_MAGIC) { 
	/* Error or a binary without magic/ with wrong magic */
	goto error;
    }
    result_size = erts_decoded_size(port_resp, ret, 0);
    if (result_size < 0) {
	goto error;
    }
    hp = HAlloc(BIF_P, result_size);
    hp_end = hp + result_size;
    endp = port_resp;
    if ((res = erts_from_external_format(NULL, &hp, &endp, &MSO(BIF_P)))
	== THE_NON_VALUE) {
	goto error;
    }
    HRelease(BIF_P, hp_end, hp);
    if (port_resp != port_result && !(ret_flags & DRIVER_CALL_KEEP_BUFFER)) {
	driver_free(port_resp);
    }
    if (bytes != &port_input[0])
	erts_free(ERTS_ALC_T_PORT_CALL_BUF, bytes);
    if (p)
	erts_port_release(p);
#ifdef ERTS_SMP
    ERTS_SMP_BIF_CHK_PENDING_EXIT(BIF_P, ERTS_PROC_LOCK_MAIN);
#else
    ERTS_BIF_CHK_EXITED(BIF_P);
#endif
    if (IS_TRACED_FL(BIF_P, F_TRACE_SCHED_PROCS)) {
    	trace_virtual_sched(BIF_P, am_in);
    }

    if (erts_system_profile_flags.runnable_procs && erts_system_profile_flags.exclusive) {
    	profile_runnable_proc(BIF_P, am_active);
    }
  
    return res;
}
    
BIF_RETTYPE port_control_3(BIF_ALIST_3)
{
    Port* p;
    Uint op;
    Eterm res = THE_NON_VALUE;
    
    /* Virtual schedule out calling process before lock wait */
    if (IS_TRACED_FL(BIF_P, F_TRACE_SCHED_PROCS)) {
	trace_virtual_sched(BIF_P, am_out);
    }

    if (erts_system_profile_flags.runnable_procs && erts_system_profile_flags.exclusive) {
    	profile_runnable_proc(BIF_P, am_inactive);
    }

    p = id_or_name2port(BIF_P, BIF_ARG_1);
    if (!p) {
    	/* Schedule the process before exiting */
    	if (IS_TRACED_FL(BIF_P, F_TRACE_SCHED_PROCS)) {
	    trace_virtual_sched(BIF_P, am_in);
    	}
	
	if (erts_system_profile_flags.runnable_procs && erts_system_profile_flags.exclusive) {
    	    profile_runnable_proc(BIF_P, am_active);
	}
	
	BIF_ERROR(BIF_P, BADARG);
    }

    /* Trace the port for scheduling in */
    if (IS_TRACED_FL(p, F_TRACE_SCHED_PORTS)) {
    	trace_sched_ports_where(p, am_in, am_control);
    }
    
    if (erts_system_profile_flags.runnable_ports && !erts_port_is_scheduled(p)) {
    	profile_runnable_port(p, am_active);
    }

    if (term_to_Uint(BIF_ARG_2, &op))
	res = erts_port_control(BIF_P, p, op, BIF_ARG_3);
    
    /* Trace the port for scheduling out */
    if (IS_TRACED_FL(p, F_TRACE_SCHED_PORTS)) {
    	trace_sched_ports_where(p, am_out, am_control);
    }

    if (erts_system_profile_flags.runnable_ports && !erts_port_is_scheduled(p)) {
    	profile_runnable_port(p, am_inactive);
    }

    erts_port_release(p);
#ifdef ERTS_SMP
    ERTS_SMP_BIF_CHK_PENDING_EXIT(BIF_P, ERTS_PROC_LOCK_MAIN);
#else
    ERTS_BIF_CHK_EXITED(BIF_P);
#endif
    
    if (IS_TRACED_FL(BIF_P, F_TRACE_SCHED_PROCS)) {
	trace_virtual_sched(BIF_P, am_in);
    }
    
    if (erts_system_profile_flags.runnable_procs && erts_system_profile_flags.exclusive) {
    	profile_runnable_proc(BIF_P, am_active);
    }
    
    if (is_non_value(res)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(res);
}

BIF_RETTYPE port_close_1(BIF_ALIST_1)
{
    Port* p;
    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    p = id_or_name2port(NULL, BIF_ARG_1);
    if (!p) {
	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
	BIF_ERROR(BIF_P, BADARG);
    }
    erts_do_exit_port(p, p->connected, am_normal);
    /* if !ERTS_SMP: since we terminate port with reason normal 
       we SHOULD never get an exit signal ourselves
       */
    erts_port_release(p);
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
    BIF_RET(am_true);
}

BIF_RETTYPE port_connect_2(BIF_ALIST_2)
{
    Port* prt;
    Process* rp;
    Eterm pid = BIF_ARG_2;

    if (is_not_internal_pid(pid)) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    prt = id_or_name2port(BIF_P, BIF_ARG_1);
    if (!prt) {
	goto error;
    }

    rp = erts_pid2proc(BIF_P, ERTS_PROC_LOCK_MAIN,
		       pid, ERTS_PROC_LOCK_LINK);
    if (!rp) {
	erts_smp_port_unlock(prt);
	ERTS_SMP_ASSERT_IS_NOT_EXITING(BIF_P);
	goto error;
    }

    erts_add_link(&(rp->nlinks), LINK_PID, prt->id);
    erts_add_link(&(prt->nlinks), LINK_PID, pid);

    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);

    prt->connected = pid; /* internal pid */
    erts_smp_port_unlock(prt);
    BIF_RET(am_true);
}

BIF_RETTYPE port_set_data_2(BIF_ALIST_2)
{
    Port* prt;
    Eterm portid = BIF_ARG_1;
    Eterm data   = BIF_ARG_2;

    prt = id_or_name2port(BIF_P, portid);
    if (!prt) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (prt->bp != NULL) {
	free_message_buffer(prt->bp);
	prt->bp = NULL;
    }
    if (IS_CONST(data)) {
	prt->data = data;
    } else {
	Uint size;
	ErlHeapFragment* bp;
	Eterm* hp;

	size = size_object(data);
	prt->bp = bp = new_message_buffer(size);
	hp = bp->mem;
	prt->data = copy_struct(data, size, &hp, &bp->off_heap);
    }
    erts_smp_port_unlock(prt);
    BIF_RET(am_true);
}


BIF_RETTYPE port_get_data_1(BIF_ALIST_1)
{
    BIF_RETTYPE res;
    Port* prt;
    Eterm portid = BIF_ARG_1;

    prt = id_or_name2port(BIF_P, portid);
    if (!prt) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (prt->bp == NULL) {	/* MUST be CONST! */
	res = prt->data;
    } else {
	Eterm* hp = HAlloc(BIF_P, prt->bp->size);
	res = copy_struct(prt->data, prt->bp->size, &hp, &MSO(BIF_P));
    }
    erts_smp_port_unlock(prt);
    BIF_RET(res);
}

/* 
 * Open a port. Most of the work is not done here but rather in
 * the file io.c.
 * Error returns: -1 or -2 returned from open_driver (-2 implies
 * that *err_nump contains the error code; -1 means we don't really know what happened),
 * -3 if argument parsing failed or we are out of ports (*err_nump should contain
 * either BADARG or SYSTEM_LIMIT).
 */
static int
open_port(Process* p, Eterm name, Eterm settings, int *err_nump)
{
#define OPEN_PORT_ERROR(VAL) do { port_num = (VAL); goto do_return; } while (0)
    int i, port_num;
    Eterm option;
    Uint arity;
    Eterm* tp;
    Uint* nargs;
    ErlDrvEntry* driver;
    char* name_buf = NULL;
    SysDriverOpts opts;
    int binary_io;
    int soft_eof;
    Sint linebuf;
    byte dir[MAXPATHLEN];

    /* These are the defaults */
    opts.packet_bytes = 0;
    opts.use_stdio = 1;
    opts.redir_stderr = 0;
    opts.read_write = 0;
    opts.hide_window = 0;
    opts.wd = NULL;
    opts.envir = NULL;
    opts.exit_status = 0;
    binary_io = 0;
    soft_eof = 0;
    linebuf = 0;

    *err_nump = 0;

    if (is_not_list(settings) && is_not_nil(settings)) {
	goto badarg;
    }
    /*
     * Parse the settings.
     */

    if (is_not_nil(settings)) {
	nargs = list_val(settings);
	while (1) {
	    if (is_tuple(*nargs)) {
		tp = tuple_val(*nargs);
		arity = *tp++;
		if (arity != make_arityval(2)) {
		    goto badarg;
		}
		option = *tp++;
		if (option == am_packet) {
		    if (is_not_small(*tp)) {
			goto badarg;
		    }
		    opts.packet_bytes = signed_val(*tp);
		    switch (opts.packet_bytes) {
		    case 1:
		    case 2:
		    case 4:
			break;
		    default:
			goto badarg;
		   }
		} else if (option == am_line) {
		    if (is_not_small(*tp)) {
			goto badarg;
		    }
		    linebuf = signed_val(*tp);
		    if (linebuf <= 0) {
			goto badarg;
		    }
		} else if (option == am_env) {
		    byte* bytes;
		    if ((bytes = convert_environment(p, *tp)) == NULL) {
			goto badarg;
		    }
		    opts.envir = (char *) bytes;
		} else if (option == am_cd) {
		    Eterm iolist;
		    Eterm heap[4];
		    int r;

		    heap[0] = *tp;
		    heap[1] = make_list(heap+2);
		    heap[2] = make_small(0);
		    heap[3] = NIL;
		    iolist = make_list(heap);
		    r = io_list_to_buf(iolist, (char*) dir, MAXPATHLEN);
		    if (r < 0) {
			goto badarg;
		    }
		    opts.wd = (char *) dir;
		} else {
		    goto badarg;
		}
	    } else if (*nargs == am_stream) {
		opts.packet_bytes = 0;
	    } else if (*nargs == am_use_stdio) {
		opts.use_stdio = 1;
	    } else if (*nargs == am_stderr_to_stdout) {
		opts.redir_stderr = 1;
	    } else if (*nargs == am_line) {
		linebuf = 512;
	    } else if (*nargs == am_nouse_stdio) {
		opts.use_stdio = 0;
	    } else if (*nargs == am_binary) {
		binary_io = 1;
	    } else if (*nargs == am_in) {
		opts.read_write |= DO_READ;
	    } else if (*nargs == am_out) {
		opts.read_write |= DO_WRITE;
	    } else if (*nargs == am_eof) {
		soft_eof = 1;
	    } else if (*nargs == am_hide) {
		opts.hide_window = 1;
	    } else if (*nargs == am_exit_status) {
		opts.exit_status = 1;
	    } else {
		goto badarg;
	    }
	    if (is_nil(*++nargs)) 
		break;
	    if (is_not_list(*nargs)) {
		goto badarg;
	    }
	    nargs = list_val(*nargs);
	}
    }
    if (opts.read_write == 0)	/* implement default */
	opts.read_write = DO_READ|DO_WRITE;

    /* Mutually exclusive arguments. */
    if((linebuf && opts.packet_bytes) || 
       (opts.redir_stderr && !opts.use_stdio)) {
	goto badarg;
    }

    /*
     * Parse the first argument and start the appropriate driver.
     */
    
    if (is_atom(name) || (i = is_string(name))) {
	/* a vanilla port */
	if (is_atom(name)) {
	    name_buf = (char *) erts_alloc(ERTS_ALC_T_TMP,
					   atom_tab(atom_val(name))->len+1);
	    sys_memcpy((void *) name_buf,
		       (void *) atom_tab(atom_val(name))->name, 
		       atom_tab(atom_val(name))->len);
	    name_buf[atom_tab(atom_val(name))->len] = '\0';
	} else {
	    name_buf = (char *) erts_alloc(ERTS_ALC_T_TMP, i + 1);
	    if (intlist_to_buf(name, name_buf, i) != i)
		erl_exit(1, "%s:%d: Internal error\n", __FILE__, __LINE__);
	    name_buf[i] = '\0';
	}
	driver = &vanilla_driver_entry;
    } else {   
	if (is_not_tuple(name)) {
	    goto badarg;		/* Not a process or fd port */
	}
	tp = tuple_val(name);
	arity = *tp++;

	if (arity == make_arityval(0)) {
	    goto badarg;
	}

	if (*tp == am_spawn) {	/* A process port */
	    if (arity != make_arityval(2)) {
		goto badarg;
	    }
	    name = tp[1];
	    if (is_atom(name)) {
		name_buf = (char *) erts_alloc(ERTS_ALC_T_TMP,
					       atom_tab(atom_val(name))->len+1);
		sys_memcpy((void *) name_buf,
			   (void *) atom_tab(atom_val(name))->name, 
			   atom_tab(atom_val(name))->len);
		name_buf[atom_tab(atom_val(name))->len] = '\0';
	    } else if ((i = is_string(name))) {
		name_buf = (char *) erts_alloc(ERTS_ALC_T_TMP, i + 1);
		if (intlist_to_buf(name, name_buf, i) != i)
		    erl_exit(1, "%s:%d: Internal error\n", __FILE__, __LINE__);
		name_buf[i] = '\0';
	    } else {
		goto badarg;
	    }
	    driver = &spawn_driver_entry;
	} else if (*tp == am_fd) { /* An fd port */
	    int n;
	    struct Sint_buf sbuf;
	    char* p;

	    if (arity != make_arityval(3)) {
		goto badarg;
	    }
	    if (is_not_small(tp[1]) || is_not_small(tp[2])) {
		goto badarg;
	    }
	    opts.ifd = unsigned_val(tp[1]);
	    opts.ofd = unsigned_val(tp[2]);

	    /* Syntesize name from input and output descriptor. */
	    name_buf = erts_alloc(ERTS_ALC_T_TMP,
				  2*sizeof(struct Sint_buf) + 2); 
	    p = Sint_to_buf(opts.ifd, &sbuf);
	    n = sys_strlen(p);
	    sys_strncpy(name_buf, p, n);
	    name_buf[n] = '/';
	    p = Sint_to_buf(opts.ofd, &sbuf);
	    sys_strcpy(name_buf+n+1, p);

	    driver = &fd_driver_entry;
	} else {
	    goto badarg;
	}
    }

    if (driver != &spawn_driver_entry && opts.exit_status) {
	goto badarg;
    }

    if (IS_TRACED_FL(p, F_TRACE_SCHED_PROCS)) {
        trace_virtual_sched(p, am_out);
    }

    port_num = erts_open_driver(driver, p->id, name_buf, &opts, err_nump);
    if (port_num < 0) {
	DEBUGF(("open_driver returned %d(%d)\n", port_num, *err_nump));
    	if (IS_TRACED_FL(p, F_TRACE_SCHED_PROCS)) {
            trace_virtual_sched(p, am_in);
    	}
	OPEN_PORT_ERROR(port_num);
    }
    
    if (IS_TRACED_FL(p, F_TRACE_SCHED_PROCS)) {
        trace_virtual_sched(p, am_in);
    }

    if (binary_io) {
	erts_port_status_bor_set(&erts_port[port_num],
				 ERTS_PORT_SFLG_BINARY_IO);
    }
    if (soft_eof) {
	erts_port_status_bor_set(&erts_port[port_num],
				 ERTS_PORT_SFLG_SOFT_EOF);
    }
    if (linebuf && erts_port[port_num].linebuf == NULL){
	erts_port[port_num].linebuf = allocate_linebuf(linebuf); 
	erts_port_status_bor_set(&erts_port[port_num],
				 ERTS_PORT_SFLG_LINEBUF_IO);
    }

 do_return:
    if (name_buf)
	erts_free(ERTS_ALC_T_TMP, (void *) name_buf);
    return port_num;

 badarg:
    *err_nump = BADARG;
    OPEN_PORT_ERROR(-3);
    goto do_return;
#undef OPEN_PORT_ERROR
}

static byte* convert_environment(Process* p, Eterm env)
{
    Eterm all;
    Eterm* temp_heap;
    Eterm* hp;
    Uint heap_size;
    int n;
    byte* bytes;

    if ((n = list_length(env)) < 0) {
	return NULL;
    }
    heap_size = 2*(5*n+1);
    temp_heap = hp = (Eterm *) erts_alloc(ERTS_ALC_T_TMP, heap_size*sizeof(Eterm));
    bytes = NULL;		/* Indicating error */

    /*
     * All errors below are handled by jumping to 'done', to ensure that the memory
     * gets deallocated. Do NOT return directly from this function.
     */

    all = CONS(hp, make_small(0), NIL);
    hp += 2;

    while(is_list(env)) {
	Eterm tmp;
	Eterm* tp;

	tmp = CAR(list_val(env));
	if (is_not_tuple(tmp)) {
	    goto done;
	}
	tp = tuple_val(tmp);
	if (tp[0] != make_arityval(2)) {
	    goto done;
	}
	tmp = CONS(hp, make_small(0), NIL);
	hp += 2;
	if (tp[2] != am_false) {
	    tmp = CONS(hp, tp[2], tmp);
	    hp += 2;
	}
	tmp = CONS(hp, make_small('='), tmp);
	hp += 2;
	tmp = CONS(hp, tp[1], tmp);
	hp += 2;
	all = CONS(hp, tmp, all);
	hp += 2;
	env = CDR(list_val(env));
    }
    if (is_not_nil(env)) {
	goto done;
    }
    if ((n = io_list_len(all)) < 0) {
	goto done;
    }

    /*
     * Put the result in a binary (no risk for a memory leak that way).
     */
    (void) erts_new_heap_binary(p, NULL, n, &bytes);
    io_list_to_buf(all, (char*)bytes, n);

 done:
    erts_free(ERTS_ALC_T_TMP, temp_heap);
    return bytes;
}
