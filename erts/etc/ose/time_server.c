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
 * Let this program run as a deamon on host to send date & time to 
 * OSE node when it starts up (see erl_ose_init.c)
 * Compile: gcc -Wall -o time_server time_server.c -lsocket -lnsl
 */

#include <time.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/uio.h>
#include <unistd.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netinet/tcp.h>

#define IP "134.138.177.125"
#define PORT 5555

/*------------------------- HELP FUNCTIONS -------------------------*/

/* function for sending a message */
int send_stuff(int s, char *msg, int nbyte) {
  if(send(s, (void*)msg, nbyte, 0) < 0)
    return -1;
  return 0;
}

/*------------------------------ MAIN ------------------------------*/

int main(int argc, char **argv) {
  int s, s1, len, result;
  struct sockaddr_in addr;
  char buf[20];
  unsigned long secs;

  /* open listen socket */
  if((s = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
    fprintf(stderr, "socket open fails! %d\n", errno);
    exit(EXIT_FAILURE);
  }    

  memset(&addr, 0, sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = inet_addr(IP);
  addr.sin_port = htons(PORT);
  
  /* bind socket to server address and port */
  if(bind(s, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
    fprintf(stderr, "socket bind fails! %d\n", errno);
    close(s);
    exit(EXIT_FAILURE);
  }

  /* listen on socket */
  if(listen(s, 100) < 0) {
    fprintf(stderr, "listen fails! %d\n", errno);
    close(s);
    exit(EXIT_FAILURE);
  } 

  while(1) {
    printf("Waiting to send time...\n");
    len = sizeof(addr);
  
    /* wait to accept connection from client */
    if((s1 = accept(s, (struct sockaddr *)&addr, &len)) < 0) {
      fprintf(stderr, "accept fails! %d\n", errno);
      close(s);
      exit(EXIT_FAILURE);
    }

    secs = (unsigned long)time((time_t *)NULL);
    sprintf(buf, "%lu", secs);

    printf("Sending time %s\n", buf);

    result = send_stuff(s1, buf, strlen(buf));
    close(s1);

    if(result < 0) {
      fprintf(stderr, "send fails! %d\n", errno);
      close(s);
      exit(EXIT_FAILURE);
    }
  }

  close(s);
  return 0;
}

