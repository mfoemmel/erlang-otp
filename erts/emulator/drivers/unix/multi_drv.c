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

/* Purpose:    Multidriver interface 
   This is an example of a driver which allows multiple instances of itself.
   I.e have one erlang process execute open_port(multi......) and
   at the same time have an other erlang process open an other port 
   running multi there as well.
*/

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "driver.h"
#include "sys.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#define MAXCHANNEL 20

static char buf[BUFSIZ];

static long multi_start();
static int multi_init(),multi_stop(),multi_erlang_read();

struct driver_entry multi_driver_entry = {
    multi_init,multi_start,multi_stop,multi_erlang_read,
    null_func,null_func,"multi"
    };


struct channel {
    int portno;
    int channel;
};

struct channel channels[MAXCHANNEL]; /* Max MAXCHANNEL instances */




static int multi_init()
{
    memzero(channels,MAXCHANNEL * sizeof(struct channel));
}


static long multi_start(port,buf) 
int port;
char *buf;
{

    int chan;
    chan = get_new_channel();
    channels[port].portno = port;
    channels[port].channel = chan;
    fprintf(stderr,"Opening channel %d port is %d\n",chan,port);
    return(port);
}


static int multi_stop(port)
int port;
{
    fprintf(stderr,"Closing channel %d\n",channels[port].channel);
    remove_channel(channels[port].channel);
}

 
static int multi_erlang_read(port,buf,count)
long port;
char *buf;
int count;

{
    fprintf(stderr,"Writing %d bytes to channel %d\n",
	    count,
	    channels[port].channel);
}



/* These two funs are fake */

get_new_channel() 
{
    static int ch = 1;
    
    return(ch++);
}

remove_channel(ch)
int ch;
{
    return;
}
