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

static int open_port(Eterm pid, Eterm name, Eterm settings);
static Port* id_or_name2port(Eterm id);

BIF_RETTYPE open_port_prim_2(BIF_ALIST_2)
BIF_ADECL_2
{
    int port_num;
    Eterm port_val;
    char *str;

    if ((port_num = open_port(BIF_P->id, BIF_ARG_1, BIF_ARG_2)) < 0) {
       if (port_num == -3) {
	  BIF_ERROR(BIF_P, BADARG);
       }
       if (port_num == -2)
	  str = erl_errno_id(errno);
       else
	  str = "einval";
       BIF_P->fvalue = am_atom_put(str, strlen(str));
       BIF_ERROR(BIF_P, USER_ERROR);
    }

    port_val = erts_port[port_num].id;
    erts_port[port_num].links = new_link(erts_port[port_num].links,LNK_LINK,
					 BIF_P->id, NIL);
    BIF_P->links = new_link(BIF_P->links, LNK_LINK, port_val, NIL);
    BIF_RET(port_val);
}

BIF_RETTYPE port_to_list_1(BIF_ALIST_1)
BIF_ADECL_1
{
   int n;
   char* tmpp;
   Eterm obj = BIF_ARG_1;
   Eterm* hp;

   tmpp = (char*) tmp_buf;

   if (is_port(obj)) {
      int i, ix, node;

      node = port_node(obj);
      ix = port_number(obj);
      i = port_creation(obj);
      sprintf(tmpp, "#Port<%d.%d>", node, ix);
   } else
      BIF_ERROR(BIF_P, BADARG);

   n = strlen(tmp_buf);
   hp = HAlloc(BIF_P, n*2);	/* we need length * 2 heap words */
   BIF_RET(buf_to_intlist(&hp, tmp_buf, n, NIL));
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
Port*
id2port(Eterm id)
{
    int ix;

    if (is_not_port(id) || (port_node(id) != THIS_NODE))
	return NULL;
    ix = port_index(id);
    if ((erts_port[ix].status == FREE) || (erts_port[ix].status & CLOSING))
	return NULL;
    return &erts_port[ix];
}

static Port*
id_or_name2port(Eterm id)
{
    if (is_atom(id)) {
	RegProc r;
	RegProc *rp;

	r.name = id;
	if ((rp = (RegProc*) hash_get(&process_reg, (void*) &r)) == NULL) {
	    return NULL;
	}
	return rp->pt;
    }
    return id2port(id);
}

BIF_RETTYPE port_command_2(BIF_ALIST_2)
BIF_ADECL_2
{
    Port* p;

    if ((p = id_or_name2port(BIF_ARG_1)) == NULL) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }

    if (p->status & PORT_BUSY) {
	erl_suspend(BIF_P, p->id);
	BIF_ERROR(BIF_P, RESCHEDULE);
    }

    if (write_port(BIF_P->id, port_index(p->id), BIF_ARG_2) != 0) {
	goto error;
    }

    if (BIF_P->status == P_EXITING) {
       KILL_CATCHES(BIF_P);	/* Must exit */
       BIF_ERROR(BIF_P, USER_ERROR);
    }
    BIF_RET(am_true);
}

static byte *erts_port_call_buff;
static Uint erts_port_call_buff_size;
/* Reversed logic to make VxWorks happy */
static int erts_port_call_need_init = 1; 

static byte *ensure_buff(Uint size)
{
    if (erts_port_call_need_init) {
	erts_port_call_buff = safe_alloc((size_t) size);
	erts_port_call_buff_size = size;
	erts_port_call_need_init = 0;
    } else if (erts_port_call_buff_size < size) {
	erts_port_call_buff_size = size;
	erts_port_call_buff = safe_realloc(erts_port_call_buff,
					   (size_t) size);
    }
    return erts_port_call_buff;
}
BIF_RETTYPE port_call_2(BIF_ALIST_2)
BIF_ADECL_2
{
    return port_call_3(BIF_P,BIF_ARG_1,make_small(0),BIF_ARG_2);
}

BIF_RETTYPE port_call_3(BIF_ALIST_3)
BIF_ADECL_3
{
    Uint op;
    Port *p;
    Uint size;
    byte *bytes;
    byte *endp;
    size_t real_size;
    ErlDrvEntry *drv;
    byte  port_result[128];	/* Buffer for result from port. */
    byte* port_resp;		/* Pointer to result buffer. */
    int ret;
    Eterm res;
    int result_size;
    Eterm *hp;
    unsigned ret_flags = 0U;

    port_resp = port_result;
    if ((p = id_or_name2port(BIF_ARG_1)) == NULL) {
    error:
	if (port_resp != port_result && 
	    !(ret_flags & DRIVER_CALL_KEEP_BUFFER)) {
	    driver_free(port_resp);
	}
	BIF_ERROR(BIF_P, BADARG);
    }
    if ((drv = p->drv_ptr) == NULL) {
	goto error;
    }
    if (!term_to_Uint(BIF_ARG_2, &op)) {
	goto error;
    }
    size = encode_size_struct(BIF_ARG_3, TERM_TO_BINARY_DFLAGS);
    bytes = ensure_buff(size);
    
    endp = bytes;
    if (to_external(-1, BIF_ARG_3, &endp) || endp == NULL) {
	erl_exit(1, "%s, line %d: bad term: %x\n",
		 __FILE__, __LINE__, BIF_ARG_3);
    }

    real_size = endp - bytes;
    if (real_size > size) {
	erl_exit(1, "%s, line %d: buffer overflow: %d word(s)\n",
		 __FILE__, __LINE__, endp - (bytes + size));
    }
    ret = drv->call((ErlDrvData)p->drv_data, 
		    (unsigned) op,
		    (char *) bytes, 
		    (int) real_size,
		    (char **) &port_resp, 
		    (int) sizeof(port_result),
		    &ret_flags);
    if (ret <= 0 || port_resp[0] != VERSION_MAGIC) { 
	/* Error or a binary without magic/ with wrong magic */
	goto error;
    }
    result_size = decode_size(port_resp, ret);
    if (result_size < 0) {
	goto error;
    }
    hp = HAlloc(BIF_P, result_size);
    endp = port_resp;
    if ((res = from_external(-1, &hp, &endp, &(BIF_P->off_heap))) 
	== THE_NON_VALUE) {
	goto error;
    }
    HRelease(BIF_P, hp);
    if (port_resp != port_result && !(ret_flags & DRIVER_CALL_KEEP_BUFFER)) {
	driver_free(port_resp);
    }
    return res;
}
    
    

BIF_RETTYPE port_control_3(BIF_ALIST_3)
BIF_ADECL_3
{
    Port* p;
    Uint op;
    Eterm res;

    if ((p = id_or_name2port(BIF_ARG_1)) == NULL) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    if (!term_to_Uint(BIF_ARG_2, &op)) {
	goto error;
    }
    if (port_control(BIF_P, p, op, BIF_ARG_3, &res) == 0) {
	goto error;
    }
    BIF_RET(res);
}

BIF_RETTYPE port_close_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Port* p;
    if ((p = id_or_name2port(BIF_ARG_1)) == NULL)
	BIF_ERROR(BIF_P, BADARG);
    do_exit_port(p->id, p->connected, am_normal);
    /* since we terminate port with reason normal 
       we SHOULD never get an exit signal ourselves
       */
    BIF_RET(am_true);
}

BIF_RETTYPE port_connect_2(BIF_ALIST_2)
BIF_ADECL_2
{
    Port* prt;
    Process* rp;
    Eterm pid = BIF_ARG_2;
    int ix;

    if (is_not_pid(pid) || (prt = id_or_name2port(BIF_ARG_1)) == NULL) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    if (pid_node(pid) != THIS_NODE) {
	goto error;
    }
    if ((ix = pid_number(pid)) >= max_process) {
	goto error;
    }
    rp = process_tab[ix];
    if (INVALID_PID(rp, pid)) {
	goto error;
    }
    
    if (find_link(&rp->links,LNK_LINK,prt->id,NIL) == NULL) {
	rp->links = new_link(rp->links, LNK_LINK, prt->id, NIL);
	prt->links = new_link(prt->links, LNK_LINK, pid, NIL);	
    }
    prt->connected = pid;
    BIF_RET(am_true);
}

BIF_RETTYPE port_set_data_2(BIF_ALIST_2)
BIF_ADECL_2
{
    Port* prt;
    Eterm portid = BIF_ARG_1;
    Eterm data   = BIF_ARG_2;

    if ((prt = id_or_name2port(portid)) == NULL) {
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
    BIF_RET(am_true);
}


BIF_RETTYPE port_get_data_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Port* prt;
    Eterm portid = BIF_ARG_1;

    if ((prt = id_or_name2port(portid)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (prt->bp == NULL) {	/* MUST be CONST! */
	BIF_RET(prt->data);
    } else {
	Eterm* hp = HAlloc(BIF_P, prt->bp->size);
	Eterm res = copy_struct(prt->data, prt->bp->size, &hp, &BIF_P->off_heap);
	BIF_RET(res);
    }
}

/* 
 * Open a port. Most of the work is not done here but rather in
 * the file io.c.
 * Error returns: -1 or -2 returned from open_driver (-2 implies
 * 'errno' contains error code; -1 means we don't really know what happened),
 * -3 if argument parsing failed.
 */
static int
open_port(Eterm pid, Eterm name, Eterm settings)
{
    int i, port_num;
    Eterm option;
    Uint arity;
    Eterm* tp;
    Uint* nargs;
    ErlDrvEntry* driver;
    char* name_buf;
    SysDriverOpts opts;
    int binary_io;
    int soft_eof;
    int linebuf;

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

    if (is_not_list(settings) && is_not_nil(settings))
	return -3;

    /*
     * Parse the settings.
     */

    if (is_not_nil(settings)) {
	nargs = list_val(settings);
	while (1) {
	    if (is_tuple(*nargs)) {
		tp = tuple_val(*nargs);
		arity = *tp++;
		if (arity != make_arityval(2))
		    return -3;
		option = *tp++;
		if (option == am_packet) {
		   if (is_not_small(*tp))
		      return -3;
		   opts.packet_bytes = signed_val(*tp);
		   switch (opts.packet_bytes) {
		    case 1:
		    case 2:
		    case 4:
		      break;
		    default:
		      return -3;
		   }
		} else if (option == am_line) {
		    if (is_not_small(*tp))
			return -3;
		    linebuf = signed_val(*tp);
		    if(linebuf <= 0)
			return -3;
		} else if (option == am_env) {
		    byte* bytes;
		    if (is_not_binary(*tp)) {
			return -3;
		    }
		    GET_BINARY_BYTES(*tp, bytes);
		    opts.envir = (char *) bytes;
		} else if (option == am_cd) {
		    byte* bytes;
		    if (is_not_binary(*tp)) {
			return -3;
		    }
		    GET_BINARY_BYTES(*tp, bytes);
		    opts.wd = (char *) bytes;
		} else {
		   return -3;
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
		return -3;
	    }
	    if (is_nil(*++nargs)) 
		break;
	    if (is_not_list(*nargs)) 
		return -3;
	    nargs = list_val(*nargs);
	}
    }
    if (opts.read_write == 0)	/* implement default */
	opts.read_write = DO_READ|DO_WRITE;

    /* Mutually exclusive arguments. */
    if((linebuf && opts.packet_bytes) || 
       (opts.redir_stderr && !opts.use_stdio))
	return -3; 

    /*
     * Parse the first argument and start the appropriate driver.
     */

    if (is_atom(name) || is_string(name)) {
	/* a vanilla port */
	if (is_atom(name)) {
	    if (atom_tab(atom_val(name))->len >= TMP_BUF_SIZE)
		return -3;
	    sys_memcpy(tmp_buf, atom_tab(atom_val(name))->name, 
		       atom_tab(atom_val(name))->len);
	    tmp_buf[atom_tab(atom_val(name))->len] = '\0';
	} else {
	    i = intlist_to_buf(name, tmp_buf, TMP_BUF_SIZE);
	    tmp_buf[i] = '\0';
	}
	name_buf = tmp_buf;
	driver = &vanilla_driver_entry;
    } else {   
	if (is_not_tuple(name))
	    return -3;		/* Not a process or fd port */
	tp = tuple_val(name);
	arity = *tp++;

	if (*tp == am_spawn) {	/* A process port */
	    if (arity != make_arityval(2)) {
		return -3;
	    }
	    name = tp[1];
	    if (is_atom(name)) {
		if (atom_tab(atom_val(name))->len >= TMP_BUF_SIZE)
		    return -3;
		sys_memcpy(tmp_buf, atom_tab(atom_val(name))->name,
			   atom_tab(atom_val(name))->len);
		tmp_buf[atom_tab(atom_val(name))->len] = '\0';
	    } else  if (is_string(name)) {
		 i = intlist_to_buf(name,tmp_buf, TMP_BUF_SIZE);
		 tmp_buf[i] = '\0';
	    } else
		return -3;
	    name_buf = tmp_buf;
	    driver = &spawn_driver_entry;
	} else if (*tp == am_fd) { /* An fd port */
	    int n;
	    char sbuf[16];
	    char* p;

	    if (arity != make_arityval(3)) {
		return -3;
	    }
	    if (is_not_small(tp[1]) || is_not_small(tp[2])) {
		return -3;
	    }
	    opts.ifd = unsigned_val(tp[1]);
	    opts.ofd = unsigned_val(tp[2]);

	    /* Syntesize name from input and output descriptor. */
	    name_buf = tmp_buf;
	    p = int_to_buf(opts.ifd, sbuf);
	    n = sys_strlen(p);
	    sys_strncpy(name_buf, p, n);
	    name_buf[n] = '/';
	    p = int_to_buf(opts.ofd, sbuf);
	    sys_strcpy(name_buf+n+1, p);

	    driver = &fd_driver_entry;
	} else
	    return -3;
    }

    if (driver != &spawn_driver_entry && opts.exit_status)
       return -3;

    if ((port_num = open_driver(driver, pid, name_buf, &opts)) < 0) {
	DEBUGF(("open_driver returned %d\n", port_num));
	return port_num;
    }

    if (binary_io) {
	erts_port[port_num].status |= BINARY_IO;
    }
    if (soft_eof) {
	erts_port[port_num].status |= SOFT_EOF;
    }
    if (linebuf && erts_port[port_num].linebuf == NULL){
	erts_port[port_num].linebuf = allocate_linebuf(linebuf); 
	erts_port[port_num].status |= LINEBUF_IO;
    }

    return port_num;
}
