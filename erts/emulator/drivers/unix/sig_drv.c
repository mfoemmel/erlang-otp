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

/* Purpose: demonstrate how to include interupt handlers in erlang */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "driver.h"
#include <signal.h>
#include <stdio.h>

static long sig_start();
static int sig_init(),sig_stop(),doio();

const struct driver_entry sig_driver_entry = {
    sig_init,sig_start,sig_stop,null_func,
    doio,null_func,"sig_test"
    };

static long this_port;

static int sig_init()
{
    this_port = -1;
}


static sigc(ino)
int ino;
{
    driver_interrupt(this_port, ino);
}

static long sig_start(port,buf) 
int port;
char *buf;
{

    if (this_port != -1)
	return(-1);
    this_port = port;
    signal(SIGUSR1, sigc);
    return(port);
}

static int sig_stop()
{
    this_port = -1;
    signal(SIGUSR1, SIG_DFL);
}


doio(port ,ino)
int port, ino;
{
    /* First go get the io, unless we already did that */
    /* In the sighandler */

    /* Then send it to erlang */

    driver_output(this_port,"y",1);
}
