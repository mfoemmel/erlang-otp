/**
 * ``The contents of this file are subject to the Erlang Public License,
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
 *
 */

/* For some defines and orber_init_agent specific declarations */
#include "obj_init_port.h"
#include "inet.h"

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

static char version[] = "orber_init_agent 0.1";

int main(argc, argv)
     int argc;
     char **argv;    
{
  extern char *progName;  /* Holds the program name */
  extern char *optarg;
  extern int errno;
  extern int optind;
  extern int opterr;
  int fromlen; 
  struct sockaddr_in name, from;
  char *servName = SERVICE;
  char *proto = PROTO;

  int sd;
  int nd;
  int c;
  int verbose;
  /*
   * Strip off heading path to program.
   */
  if ((progName = strrchr(argv[0], '/')) != NULL)
    progName++;
  else
    progName = argv[0];
  
  while ((c = getopt(argc, argv, "p:v")) != -1)
    {
      switch (c)
	{
	case 'p':       /* Optional port number */
	  if (isdigit(*optarg))
	    servName = optarg;
	  break;

	case 'v':
	  verbose = 1;
	  break;
	  
	default:
	  if(verbose)
	    (void) usage();
	  (void) exit(1);
	}
    }
  
  argc -= optind;
  argv += optind;
  
  if (argc != 0)
    {
      if (verbose)
	(void) usage();
      (void) exit(1);
    }

  /*
   * Initiate network stuff.
   */
  if ((sd = net_init(&name, servName, proto)) == -1)
    return 1;


  /* 
   * We shall loop forever and just listening on the socket.
   */
  for (;;)
    {
      fromlen = sizeof(from);

      while ((nd = accept(sd, (struct sockaddr *) &from, &fromlen)) == -1)
	{
	  /*
	   * If we were interrupted by some reason.
	   */
	  if (errno == EINTR)
	    continue;

	  (void) syslog(LOG_ERR, "accept() (%m)");
	  (void) shutdown(sd, 2);
	  return 1;
	}
      (void) syslog(LOG_ERR, "info accepted ()");
      (void) dispatch(nd);
      (void) close(nd);
      continue;
    }
}


/*
 * Reads a CDR coded unsigned long from a buffer.
 */
unsigned int read_uint32(ptr, bo)
     char **ptr;
     int bo;
{
  unsigned int ulong;

  if(bo)
    ulong = (unsigned int) (((*ptr)[3] & 0xFF) << 24) + (((*ptr)[2] & 0xFF) << 16) +
      (((*ptr)[1] & 0xFF) << 8) + ((*ptr)[0] & 0xFF);
  else
    ulong = (unsigned int) (((*ptr)[0] & 0xFF) << 24) + (((*ptr)[1] & 0xFF) << 16) +
      (((*ptr)[2] & 0xFF) << 8) + ((*ptr)[3] & 0xFF);

  *ptr += 4;
  return ulong;
}

/*
 * Read len bytes from socket.
 */
read_from_socket(sd, buf, len)
     int sd;
     char *buf;
     int len;
{
  int nbytes;

  if((nbytes = read(sd, buf, len)) != len)
    {
      if(nbytes < 0)
	{
	  (void) syslog(LOG_ERR, "read() (%m)");
	  return -1;
	}
      else
	{
	  (void) syslog(LOG_ERR, "read() (read %d bytes instead of expected %d)",
			nbytes, len);
	  return -1;
	}
    }

  return 0;
}


int read_request_from_socket(sd, request_bytes)
     int sd;
     char **request_bytes;
{
  char buf[12];
  char *ptr;
  int length;
  int byte_order;
  int read_bytes;

  if(read_from_socket(sd, &buf, 12) == -1)
    return -1;
  else
    {
      ptr =(char*) &buf;
      
      if( memcmp("GIOP", ptr, 4) != 0)
	{
	  (void) syslog(LOG_ERR, "read_from_socket() (Bad magic number)");
	  return -1;
	}
      else
	ptr+=4;
      
      ptr+=2;
      byte_order = *ptr++;
      if (*ptr++ != 0)
	{
	  (void) syslog(LOG_ERR, "read_giop_header() (Not a GIOP request)");
	  return -1;
	}	  
      read_bytes = 8;     
      length = read_uint32(&ptr, byte_order);
      
      *request_bytes = (char*) malloc(length + 12);
      (void) memcpy(*request_bytes, buf, 12);
      ptr = *request_bytes + 12;
      
      if(read_from_socket(sd, ptr, length) == -1)
	{
	  free(request_bytes);
	  return -1;
	}
      
      return 12 + length;
    }
}

void dispatch(sd)
     int sd;
{
  char *request_bytes;
  char *reply_bytes;
  int l;
  int reply_length;

  if ( (l = read_request_from_socket(sd, &request_bytes)) == -1)
    {
      (void) syslog(LOG_ERR, "read_from_socket() (%m)");
      return;
    }
  else
    {
      (void) syslog(LOG_ERR, "info read_request_from_socket ()");

      if((reply_length = request_erlang(request_bytes, l, &reply_bytes)) == -1)
	{
	  (void) syslog(LOG_ERR, "request_erlang() (%m)");
	  (void) exit(1);
	}
  
      if (write(sd, reply_bytes, reply_length) == -1)
	{
	  (void) syslog(LOG_ERR, "write() (%m)");
	}
      free(request_bytes);
      free(reply_bytes);
    }
  return;
}

int request_erlang(request_bytes, request_length, reply_bytes)
     char *request_bytes;
     int request_length;
     char **reply_bytes;
{
  (void) syslog(LOG_ERR, "info request_erlang ");

  if(write_request(request_bytes, request_length) != request_length)
    {
      (void) syslog(LOG_ERR, "info write_request error ");
      return -1;
    }

  return read_reply(reply_bytes);
}

/*
 * Write message to erlang (stdout).
 */
void put_uint32(l, str)
     int l;
     char *str;
{
  str[0] = (l >> 24) & 0xFF;
  str[1] = (l >> 16) & 0xFF;
  str[2] = (l >> 8) & 0xFF;
  str[3] = (l >> 0) & 0xFF;
}

int write_request(request, len)
     char *request;
     int len;
{
  char buf[5];

  put_uint32(len, buf);
  (void) syslog(LOG_ERR, "info write_request (%d)",len);

  /* write packet length, consists of 4 bytes MSB */
  if(write_bytes_to_stdout(buf, 4) != 4)
    return(-1);

  (void) syslog(LOG_ERR, "info write_request2");

  return write_bytes_to_stdout(request, len);
}

int write_bytes_to_stdout(buf, len)
  char* buf;
  int len;
{
  int i;
  int wrote = 0;
  
  do {
    if ((i = write(1, buf + wrote, len - wrote)) <= 0)
	return(i);
    wrote += i;
    (void) syslog(LOG_ERR, "info write_bytes_to_stdout (%d)", wrote);

  } while( wrote < len);

  return(len);
}

/* 
 * Read reply from erlang (stdin).
 */
int read_reply(reply_buf)
     char **reply_buf;
{
  unsigned int len = 0;
  int read_bytes;
  char buf[5];
  char *ptr;
  (void) syslog(LOG_ERR, "info read_reply0 ");

  /* Read packet length, consists of 4 bytes MSB */
  if(read_bytes_from_stdin(&buf, 4) != 4)
    return(-1);
  (void) syslog(LOG_ERR, "info read_reply1 ");

  ptr = (char*) &buf;
  len = read_uint32(&ptr, 0);
  (void) syslog(LOG_ERR, "info read_reply2 ");

  *reply_buf = (char*) malloc(len + 1);
  (void) syslog(LOG_ERR, "info read_reply3 ");

  return read_bytes_from_stdin(*reply_buf, len);
}

int read_bytes_from_stdin(buf, len)
  char* buf;
  int len;
{
  int i;
  int got = 0;
  
  do {
    if ((i = read(0, buf + got, len - got)) <= 0)
	return(i);
    got += i;
    (void) syslog(LOG_ERR, "info read_bytes_from_stdin (%d)", got);

  } while( got < len);

  return(len);
}

/*
 * Useage text.
 */
void usage()
{
  extern char *progName;
  (void) fprintf(stderr, 
		 "Usage: %s [-p ip-portnr] \n", progName);
}


/*
 * Init the socket for for communication.
 */
int net_init(name, servName, proto)
     struct sockaddr_in *name;
     char *servName;
     char *proto;

{
  int sd;
  int sockOpt = 1; /* Enable KEEP_ALIVE, OOBINLINE and REUSEADDR */
  char hostName[MAXHOSTNAMELEN];

  /*
   * Find out our hostname.
   */
  if (gethostname(hostName, sizeof (hostName)) == -1)
    {
      (void) syslog(LOG_ERR, "gethostname() (%m)");
      (void) exit(1);
    }

  /*
   * Fill in sockaddr_in struct with
   * port number, host name, etc.
   */
  (void) setsockaddr_in(name, hostName, servName, proto);

  /*
   * Initialize and set up socket communication.
   */
  if ((sd = socket(AF_INET, SOCK_STREAM, 0)) == -1)
    {
      (void) syslog(LOG_ERR, "socket() (%m)");
      return -1;
    }

  /*
   * Set the SO_KEEPALIVE flag to make read return on 
   * a broken connection from the other side.
   */
  if (setsockopt(sd, SOL_SOCKET, SO_KEEPALIVE, 
		 (char *) &sockOpt, sizeof sockOpt) == -1 || 
      setsockopt(sd, SOL_SOCKET, SO_REUSEADDR, 
		 (char *) &sockOpt, sizeof sockOpt) == -1 || 
      setsockopt(sd, SOL_SOCKET, SO_OOBINLINE, 
		 (char *) &sockOpt, sizeof sockOpt) == -1)
    {
      (void) syslog(LOG_ERR, "setsockopt() (%m)");
      (void) shutdown(sd, 2);
      return -1;
    }

  /*
   * Bind the address to the socket.
   */
  if (bind(sd, (struct sockaddr *) name, sizeof(*name)) == -1)
    {
      (void) syslog(LOG_ERR, "bind() (%m)");
      (void) shutdown(sd, 2);
      return -1;
    }

  /*
   * Tell kernel to listen on socket.
   * Enable maximum allowed (5) entries in queue.
   */
  if (listen(sd, SOMAXCONN) == -1)
    {
      (void) syslog(LOG_ERR, "listen() (%m)");
      (void) shutdown(sd, 2);
      return -1;
    }

  return sd;
}
