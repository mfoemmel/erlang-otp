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

#include "inet.h"
#include "inet.sig"
#include "efs.h"
#include "dbgprintf.h"

#include "sys.h"
#include "erl_driver.h"

#include "erl_inet.sig"
#include "ose_inet_drv.h"

#define LOCALHOST "127.0.0.1"

/* global variables to be used by erts to send dummy data to 
   sock_select, to wake it from the select */
static struct sockaddr_in rel_addr;
static int rel_sock = -1;
static int rel_port = 0;

union SIGNAL {
  SIGSELECT sig_no;
};

union EventSig {
  SIGSELECT sig_no;
  struct PidSig          pid_sig;
  struct SockSelect      sock_select;
  struct InetEventRead   inet_ev_read;
  struct InetEventWrite  inet_ev_write;
  struct InetEventAck    inet_ev_ack;
  struct SockSelectError sock_select_error;
  struct PortInfo        port_info;
};

/* forward declarations */
void send_error(int, int);
void send_write_event(int);
void send_read_event(int);
void select_release();

static PROCESS erts_;
extern PROCESS erl_block;

/* start sock_select process */
PROCESS start_sock_select() {
  PROCESS sock_select_;
  union EventSig *sig;
  static const SIGSELECT recv_port_info[] = {1,PORT_INFO};
  OSENTRYPOINT erl_sock_select;
  char dummy = '\0';
  int zero_value = 0;

  erts_ = current_process();
  
  sock_select_ = create_process(OS_PRI_PROC, /* processtype */
				"erl_sock_select", /* name        */
				erl_sock_select, /* entrypoint  */
				2000,	/* stacksize   */
				15,	/* priority    */
				0,	/* timeslice */
				erl_block,	/* block */
				NULL,0,0); /* not used    */
  efs_clone(sock_select_);
  start(sock_select_); 
  
#ifdef DEBUG
  printf("Spawned sock_select as process %li\n", (unsigned long)sock_select_);
#endif

  /* receive port info from sock_select process (used for sync really) */
  sig = (union EventSig *)receive((SIGSELECT *)recv_port_info);
  rel_port = sig->port_info.port;
  free_buf((union SIGNAL **)&sig);
  
  /* create "release" socket */
  if((rel_sock = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
    fprintf(stderr, "Can't open release socket (erts)! %d\n", errno);
    error(errno);
  }

  /* set blocking */
  ioctl(rel_sock, FIONBIO, (char*)&zero_value);

  /* initiate rel_addr to be used later for sending "wake up" byte */
  memset(&rel_addr, 0, sizeof(rel_addr));
  rel_addr.sin_family = AF_INET;
  rel_addr.sin_addr.s_addr = inet_addr(LOCALHOST);
  rel_addr.sin_port = htons(rel_port);

#ifdef DEBUG
  printf("Release socket %d set up for communication to %s:%d\n", 
	    rel_sock, LOCALHOST, rel_port);
#endif

  return sock_select_;
}

void stop_sock_select(PROCESS erl_sock_select_) {
  /* clean up needed!? */
  kill_proc(erl_sock_select_);
}

/* for debugging */
void print_fd_sets(int max_sock, fd_set *read_socks, fd_set *write_socks)
{
  int i;

  for(i=0; i<=max_sock; i++)
    if(FD_ISSET(i, read_socks))
      printf("READ: %d\n", i);
  for(i=0; i<=max_sock; i++)
    if(FD_ISSET(i, write_socks))
      printf("WRITE: %d\n", i);
  printf("\n");
}

OS_PROCESS(erl_sock_select)
{
  union EventSig *sig;
  static const SIGSELECT recv_init_sig[] = {1,PID_SIG};
  static const SIGSELECT recv_any_sig[] = { 0 };
  int i, sock, s0, mode, on, len;
  int result;
  int max_sock = -1;
  struct sockaddr_in addr;
  char dummy;
  fd_set read_socks, write_socks, err_set;
  fd_set read_socks1, write_socks1;
  struct timeval tv;

  attach(NULL, erts_);

  FD_ZERO(&read_socks);
  FD_ZERO(&write_socks);
  FD_ZERO(&read_socks1);
  FD_ZERO(&write_socks1);

  /* open "release" socket (to be used for releasing the select), bind 
     socket to localhost and send back port info to erts */

  if((s0 = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
    fprintf(stderr, "Can't open release socket (sock_select)! %d\n", errno);
    error(errno);
  }

#ifdef DEBUG
  printf("sock_select uses release socket %d\n", s0);
#endif

  memset(&addr, 0, sizeof (addr));
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = inet_addr(LOCALHOST);
  addr.sin_port = 0;		/* dynamic port assignment */
  
  if(bind(s0, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
    fprintf(stderr, "Can't bind release socket! %d\n", errno);
    close(s0);
    error(errno);
  }

  len = sizeof(addr);
  if(getsockname(s0, (struct sockaddr *)&addr, &len) < 0)
    error(errno);

  sig = (union EventSig *)alloc(sizeof(struct PortInfo), PORT_INFO);
  sig->port_info.port = ntohs(addr.sin_port);
  send((union SIGNAL **)&sig, erts_);
  
  FD_SET(s0, &read_socks);
  max_sock = s0;
  
  while(1) {
    /* FIRST check if there are any select requests waiting */
    while((sig = (union EventSig *)receive_w_tmo(0, (SIGSELECT *)recv_any_sig)) != NULL)
      {
	switch(sig->sig_no) {
	  
	case SOCK_SELECT:
	  sock = sig->sock_select.sock;
	  mode = sig->sock_select.mode;
	  on = sig->sock_select.on;	  
	  
	  free_buf((union SIGNAL **)&sig);
	  if(on) {
	    if(mode & DO_READ) {
	      FD_SET(sock, &read_socks);
	    }
	    if(mode & DO_WRITE) {
	      FD_SET(sock, &write_socks);
	    }
	    if((mode & (DO_READ|DO_WRITE)) && max_sock < sock) max_sock = sock;
	  } else {
	    if(mode & DO_READ) {
	      FD_CLR(sock, &read_socks);
	    }
	    if(mode & DO_WRITE) {
	      FD_CLR(sock, &write_socks);
	    }
	    if((mode & (DO_READ|DO_WRITE)) && max_sock == sock) {
	      while (max_sock >= 0 &&
		     !FD_ISSET(max_sock, &read_socks) &&
		     !FD_ISSET(max_sock, &write_socks))
		max_sock--;
	    }
	  }
	  break;
	  
	case OS_ATTACH_SIG:
#ifdef DEBUG
	  printf("\nose_inet_sock_select: Attach signal received, closing!\n");
#endif
	  kill_proc(current_process());
	  
	default:
	  fprintf(stderr, "Unrecognised signal %d to sock_select\n", sig->sig_no); 
	  free_buf((union SIGNAL **)&sig);
	}
      }

    /* set fd sets for select */
    read_socks1  = read_socks;
    write_socks1 = write_socks;

#ifdef xDEBUG
    printf("selecting... (max %d, set(%d)=%d)\n", max_sock, s0, FD_ISSET(s0, &read_socks1));
    /* print_fd_sets(max_sock, &read_socks1, &write_socks1); */
#endif

    /* THEN detect activity on sockets. Specify select with a timeout value to handle the 
       case when a connect is in progress but hasn't finished before select is called. 
       OSE doesn't handle this case correctly so we need to try again. */
    
    tv.tv_sec = 5; tv.tv_usec = 0;
    result = select(max_sock+1, &read_socks1, &write_socks1, NULL, &tv); 

    if(result > 0) {		/* activity detected */
      /* first check for data on release socket */
      if(FD_ISSET(s0, &read_socks1)) {
	len = sizeof(addr);
	/* remove dummy data */
	if((result = recvfrom(s0, &dummy, 1, 0, 
			      (struct sockaddr *)&addr, &len)) <= 0) {
	  fprintf(stderr, "Fail to read data from release socket: %d\n", errno);
	  error(errno);
	}
      }
      /* next check status of write & read sockets */
      for(i=0; i <= max_sock; i++) 
	if(FD_ISSET(i, &write_socks1)) {
	  send_write_event(i);
	}
      for(i=0; i <= max_sock; i++) 
	if(FD_ISSET(i, &read_socks1)) {
	  if(i != s0) send_read_event(i);
	}
    } else if(result < 0) {
      /* check which sock is bad */
      for(i = 0; i <= max_sock; i++) {
	if(FD_ISSET(i, &read_socks1)) {
	  FD_ZERO(&err_set);
	  FD_SET(i, &err_set);
	      tv.tv_sec = 0; tv.tv_usec = 0;
	  if(select(max_sock + 1, &err_set, NULL, NULL, &tv) == -1){
#ifdef DEBUG
	    fprintf(stderr, "Bad Socket (read): %d\n", sock);
#endif
	    /* bad read FD found */
	    send_error(sock, errno);
	    /* remove the bad sock from orig set */
	    FD_CLR(i, &read_socks);
	  }
	}
	if(FD_ISSET(i, &write_socks1)) {
	  FD_ZERO(&err_set);
	  FD_SET(i, &err_set);
	  tv.tv_sec = 0; tv.tv_usec = 0;
	  if(select(max_sock + 1, NULL, &err_set, NULL, &tv) == -1) {
#ifdef DEBUG
	    fprintf(stderr, "Bad Socket (write): %d\n", sock);
#endif
	    /* bad write FD found */
	    send_error(sock, errno);
	    /* remove the bad sock from orig set*/
	    FD_CLR(i, &write_socks);
	  }
	}
      }
    } /* else select has timed out, go again */
  }
}
      
  
static void recv_event_ack() {
  union EventSig *sig;
  static const SIGSELECT recv_ack_sig[] = {1,INET_EVENT_ACK};
  sig = (union EventSig *)receive((SIGSELECT *)recv_ack_sig);
  free_buf((union SIGNAL **)&sig);
}

void send_error(int sock, int error) {
  union EventSig *sig;
  sig = (union EventSig *)alloc(sizeof(struct SockSelectError), SOCK_SELECT_ERROR);
  sig->sock_select_error.sock = sock;
  sig->sock_select_error.error = error;
  send((union SIGNAL **)&sig, erts_);
  if(erts_ != current_process()) recv_event_ack();
}

void send_read_event(int sock) {
  union EventSig *sig;

  sig = (union EventSig *)alloc(sizeof(struct InetEventRead), INET_EVENT_READ);
  sig->inet_ev_read.sock = sock;
  send((union SIGNAL **)&sig, erts_);
  if(erts_ != current_process()) recv_event_ack();
}

void send_write_event(int sock) {
  union EventSig *sig;
  sig = (union EventSig *)alloc(sizeof(struct InetEventWrite), INET_EVENT_WRITE);
  sig->inet_ev_write.sock = sock;
  send((union SIGNAL **)&sig, erts_);
  if(erts_ != current_process()) recv_event_ack();
}

void select_release() {
  char dummy = '\0';
  int result = 0;

  if((result = sendto(rel_sock, (void*)&dummy, 1, 0, 
		      (struct sockaddr *)&rel_addr, 
		      sizeof(rel_addr)) == -1))
    {
      fprintf(stderr, "Can't send data to wake sock_select! %d\n", errno);
      close(rel_sock);
      error(errno);
    }
  /*! try to explain why 0 is always returned! */
#ifdef xDEBUG
  if(result != 1) 
    fprintf(stderr, "Whoa! %d byte(s) written on %d!\n", result, rel_sock);
#endif

    /*
  while(result == 0) {
    if((result = sendto(rel_sock, (void*)&dummy, 1, 0, 
			(struct sockaddr *)&rel_addr, 
			sizeof(rel_addr)) == -1))
      {
	fprintf(stderr, "Can't send data to wake sock_select! %d\n", errno);
	close(rel_sock);
	error(errno);
      }
  }
  if(result > 0) return;
  fprintf(stderr, "Warning 0 bytes sent!\n");
    */
}

	  



