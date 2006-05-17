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
 * BIFs belonging to the 'os' module.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "erl_driver.h"
#include "bif.h"
#include "big.h"
#include "dist.h"
#include "erl_version.h"

/*
 * Return the pid for the Erlang process in the host OS.
 */

Eterm
os_getpid_0(Process* p)
{
     char pid_string[21]; /* enough for a 64 bit number */
     int n;
     Eterm* hp;
     sys_get_pid(pid_string); /* In sys.c */
     n = sys_strlen(pid_string);
     hp = HAlloc(p, n*2);
     BIF_RET(buf_to_intlist(&hp, pid_string, n, NIL));
}

Eterm
os_getenv_0(Process* p)
{
    GETENV_STATE state;
    char *cp;
    Eterm* hp;
    Eterm ret;
    Eterm str;
    int len;

    init_getenv_state(&state);

    ret = NIL;
    while ((cp = getenv_string(&state)) != NULL) {
	len = strlen(cp);
	hp = HAlloc(p, len*2+2);
	str = buf_to_intlist(&hp, cp, len, NIL);
	ret = CONS(hp, str, ret);
    }
    return ret;
}

Eterm
os_getenv_1(Process* p, Eterm key)
{
    Eterm str;
    int len;
    char *key_str;
    char* val;

    len = is_string(key);
    if (!len) {
	BIF_ERROR(p, BADARG);
    }
    key_str = (char *) erts_alloc(ERTS_ALC_T_TMP, len+1);
    if (intlist_to_buf(key, key_str, len) != len)
	erl_exit(1, "%s:%d: Internal error\n", __FILE__, __LINE__);
    key_str[len] = '\0';
    val = getenv(key_str);
    if (val == NULL) {
	str = am_false;
    } else {
	Eterm* hp;
	len = strlen(val);
	hp = HAlloc(p, len*2);
	str = buf_to_intlist(&hp, val, len, NIL);
    }
    erts_free(ERTS_ALC_T_TMP, (void *) key_str);
    BIF_RET(str);
}

Eterm
os_putenv_2(Process* p, Eterm key, Eterm value)
{
    char *buf = NULL;
    int i, key_len, value_len;
    key_len = is_string(key);
    if (!key_len) {
    error:
	if (buf)
	    erts_free(ERTS_ALC_T_TMP, (void *) buf);
	BIF_ERROR(p, BADARG);
    }
    value_len = is_string(value);
    if (!value_len)
	goto error;
    buf = erts_alloc(ERTS_ALC_T_TMP, key_len + 1 + value_len + 1);
    i = intlist_to_buf(key, buf, key_len);
    if (i != key_len)
	erl_exit(1, "%s:%d: Internal error\n", __FILE__, __LINE__);	
    buf[i++] = '=';
    i += intlist_to_buf(value, &buf[i], value_len);
    if (i != key_len + 1 + value_len)
	erl_exit(1, "%s:%d: Internal error\n", __FILE__, __LINE__);	
    buf[i] = '\0';
    if (sys_putenv(buf)) {
	goto error;
    }
    erts_free(ERTS_ALC_T_TMP, (void *) buf);
    BIF_RET(am_true);
}

