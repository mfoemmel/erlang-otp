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

#include "ose.h"
#include "dbgprintf.h"
#include "malloc.h"
#include "errno.h"
#include "sys/stat.h"
#include "unistd.h"
#include "shell.h"
#include "efs.h"
#include "efs_err.h"
#include "fm.sig"

#include "inetlink.h"
#include "inet.h"
#include "inetutil.h"
#include "inet.sig"

#include "nameser.h"
#include "resolv.h"
#include "netdb.h"

#include "stdlib.h"
#include "stdio.h"
#include "string.h"

#define IPSHIFT(cp, l)  ((l) = (unsigned long)(((unsigned long)((cp)[3]) << 24) | \
                                               ((unsigned long)((cp)[2]) << 16) | \
                                               ((unsigned long)((cp)[1]) << 8)  | \
                                                (unsigned long)((cp)[0])))

int get_ip_addr(struct in_addr *addr, int *len);
int config_dns(void);
int wait_for_if(const char *ifname);

/*____________________ INET CFG _____________________*/
      
#define MaxIfNum 4
int wait_for_if(const char *ifname);

int get_conf(int argc, char **argv)
{
	InetErr err;
	int i = 0;
	int sock;
	int ifnum = 0;
	struct ifreq *IfReq;
	struct sockaddr_dl *SockAddr_dl;
	struct sockaddr_in *SockAddr;
	char *dump;
	struct ifconf IfConf;

	printf("%s\n",wait_for_if(0) ? "Interface Fail!." : "Interface Ok!");

	if ( (sock = socket(AF_INET,SOCK_STREAM,0) ) == -1 ) 
	  error(errno);
	dump = (char *)malloc(SDL_IFREQSIZE  * MaxIfNum);
	IfConf.ifc_len = ( SDL_IFREQSIZE  * MaxIfNum );
	IfConf.ifc_ifcu.ifcu_buf = ( char * ) dump;
	if ( ioctl(sock,SIOCGIFCONF,(char *) &IfConf) == -1 )
	  error(errno);
	close ( sock );
	ifnum = IfConf.ifc_len / SDL_IFREQSIZE;
	IfReq = (void *)IfConf.ifc_ifcu.ifcu_req;
	free(dump);

	if ( (sock = socket(AF_INET,SOCK_STREAM,0) ) == -1 ) 
	  error(errno);
	for ( i=0; i < ifnum ; i++ )
	{
		printf("----------------------------------------\n");
		SockAddr_dl = (struct sockaddr_dl *) &IfReq->ifr_ifru.ifru_addr;
		SockAddr = (struct sockaddr_in *)SockAddr_dl;
		printf("IFname : %s\n",IfReq->ifr_name);
		printf("Index : %2x\n",SockAddr_dl->sdl_index);
		if ( ioctl(sock,SIOCGIFMTU,(char *) IfReq) == -1 )
			error(errno);
		printf("MTU :%4x\n",IfReq->ifr_ifru.ifru_mtu);
		if ( ioctl(sock,SIOCGIFADDR,(char *) IfReq) == -1 )
			error(errno);
		printf("IP_Address :%s \n",inet_ntoa(SockAddr->sin_addr));
		if ( ioctl(sock,SIOCGIFNETMASK,(char *) IfReq) == -1 )
			error(errno);
		printf("SubNetMask :%s \n",inet_ntoa(SockAddr->sin_addr));
		if ( ioctl(sock,SIOCGIFBRDADDR,(char *) IfReq) == -1 )
			error(errno);
		printf("Broad Cast : %s \n",inet_ntoa(SockAddr->sin_addr));
		IfReq = (struct ifreq *) ((char *)IfReq + SDL_IFREQSIZE);
	}
	printf("----------------------------------------\n");
	close ( sock );
	return 0;
}

union iSIGNAL {
  SIGSELECT sig_no;
  struct InetIfUp      up;
  struct InetRouteAdd  route;
};

int wait_for_if(const char *ifname)
{
  union iSIGNAL   *sig;
  static const    SIGSELECT sel[2] = {1, INET_IF_UP_REPLY};
  PROCESS inet_;

  sig = (union iSIGNAL *)alloc(sizeof(struct InetIfUp), INET_IF_UP_REQUEST);

  /* No interface specified, pick the first one. */
  if(ifname == 0 || *ifname == '\0')
    strcpy(sig->up.ifName, "*");
  else
    strcpy(sig->up.ifName, ifname);
      
  /* Request a notification signal when the interface is up. */
  hunt("ose_inet", 0, &inet_, NULL);
  send((union SIGNAL **)&sig, inet_);
  sig = (union iSIGNAL *)receive((SIGSELECT *)sel);
      
  /* Verify the INET reply status value. */
  if(sig->up.status != 0)
    fprintf(stderr, "<wait_for_if> bad status code for INET_IF_UP_REQUEST");

  /* Don't forget to release the buffer. */
  free_buf((union SIGNAL **)&sig);

  return 0;
}

/***************************************************************************/
/* get ip address (first interface) */

#define IP_LEN(s) (strlen((s)) > 16 ? 16 : 4)

int get_ip_addr(struct in_addr *addr, int *len) {
  int sock;
  int ifnum = 0;
  struct ifconf IfConf;
  struct ifreq *IfReq;
  int bufsize;
  char *IfReqBuf;
  struct sockaddr_in *SockAddr;
  int result;

  wait_for_if(0);

  if((sock = socket(AF_INET, SOCK_STREAM, 0)) == -1)
    return -1;
  bufsize = MaxIfNum * SDL_IFREQSIZE;
  IfReqBuf = (char *)malloc(bufsize * sizeof(char));
  IfConf.ifc_len = bufsize;
  IfConf.ifc_buf = IfReqBuf;
  result = ioctl(sock, SIOCGIFCONF, (char *)&IfConf);
  close(sock);
  if(result == -1) return -1;

  ifnum = IfConf.ifc_len / SDL_IFREQSIZE;
  IfReq = (struct ifreq *)(IfConf.ifc_req);

  /* printf("Found %d interfaces\n", ifnum); */

  if(ifnum < 2) return 0;	/* only loopback configured */
  
  IfReq = (struct ifreq *)((char *)IfReq + SDL_IFREQSIZE);
  if((sock = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
    free(IfReqBuf);
    return -1;
  }
  result = ioctl(sock, SIOCGIFADDR, (char *)IfReq);
  close(sock);
  if(result == -1) {
    free(IfReqBuf);
    return -1;
  }
  SockAddr = (struct sockaddr_in *)&(IfReq->ifr_addr);
  *addr = (SockAddr->sin_addr);
  *len = IP_LEN(inet_ntoa(*addr));
  free(IfReqBuf);
  return 1;
}
  
/***************************************************************************/
/* get hostname (from dns) */

int gethostname(int argc, char **argv) {
  struct in_addr addr;
  int result, len;
  unsigned char *cp;
  struct hostent *host;

  config_dns();
  
  result = get_ip_addr(&addr, &len);
  if(result == -1) {
    fprintf(stderr, "Fail to get IP address. errno = %d\n", errno);
    return 0;
  }
  if(result == 0) {
    fprintf(stderr, "No interface available\n");
    return 0;
  }

  printf("Will try to resolv address: %s (len: %d)\n", inet_ntoa(addr), len); 
  if((host = gethostbyaddr_r((const char *)&addr, len, AF_INET)) == NULL) {
    /* Could be that the OSE resolver doesn't reverse the IP string (compiled 
       for LITTLE ENDIAN!?), change the byte order and try again */
    cp = (unsigned char*)&(addr.s_addr);
    IPSHIFT(cp, addr.s_addr);
    if((host = gethostbyaddr_r((const char *)&addr, len, AF_INET)) == NULL) {
      printf("nohost\n");
      return 0;
    }
  }
  printf("%s\n", host->h_name);
  free_buf((union SIGNAL **)&host);
  return 0;
}


/***************************************************************************/
/* DNS */

#define DNS_ADDR "134.138.176.16"
#define DOMAIN   ".du.uab.ericsson.se"

union rcSIGNAL {
  SIGSELECT sig_no;
  struct resGetConfig rgc;
  struct resSetConfig rsc;
};

/* must never be called twice appearently!!! */
int dns_config(int argc, char **argv) {
  PROCESS resolve_; 
  union rcSIGNAL *sig; 
  ResTag *tag; 
  static const SIGSELECT rec_any_sig[] = { 0 };

  set_env(get_bid(current_process()), "DNS_CONFIG", "true");

  sig = (union rcSIGNAL *)alloc(512, RES_CONF_SET); 
  tag = sig->rsc.conftags; /* Do not throw away truncated replies. */ 
  tag[0] = RES_TAG_IGNORE_TRUNC; 
  tag[1] = 1; 
  tag += 2; 
  /* Request recursive search by DNS server. */ 
  tag[0] = RES_TAG_RECURSE; 
  tag[1] = 1; 
  tag += 2; 
  /* Append the default domain name to queries. */ 
  tag[0] = RES_TAG_USE_DEFNAMES; 
  tag[1] = 1; 
  tag += 2; 
  /* Search up local domain tree. */ 
  tag[0] = RES_TAG_DNSRCH;
  tag[1] = 1;
  tag += 2; 
  /* Add DNS server address. */ 
  tag[0] = RES_TAG_NS;
  tag[1] = 1; /* Size of this tag */
  tag[2] = inet_addr(DNS_ADDR);
  tag += 3; 
  /* Default domain name to be used in queries. */ 
  tag[0] = RES_TAG_DEFDNAME;
  tag[1] = strlen(DOMAIN); /* Size of this tag */ 
  strcpy((char *)&(tag[2]), DOMAIN);

  tag += 10; tag[0] = RES_TAG_END;

  hunt("ose_resolve", 0, &resolve_, NULL);
  send((union SIGNAL **)&sig, resolve_);
  sig = (union rcSIGNAL *)receive((SIGSELECT *)rec_any_sig);
  free_buf((union SIGNAL **)&sig);
  printf("DNS resolver has been configured! (nameserver: %s)\n", DNS_ADDR);
  return 0;
}

int config_dns(void) {
  char *env;
  if((env = get_env(get_bid(current_process()), "DNS_CONFIG")) != NULL) {
    free_buf((union SIGNAL **)&env);
    return 0;
  }
  dns_config(0, NULL);
  return 0;
}

int nslookup(int argc, char **argv) {
  struct hostent *host; 
  struct in_addr addr;
  char buf[50];
  unsigned char *cp;

  config_dns();

  if(argc != 2) {
    printf("Invalid number of arguments\r\n");
    return 0;
  }
  strcpy(buf, argv[1]);
  if((addr.s_addr = inet_addr(buf)) != INADDR_NONE) { /* lookup hostname */
    printf("Finding hostname for %s (%lx)...\n", buf, addr.s_addr);
    if((host = gethostbyaddr_r((const char *)&addr, 4, AF_INET)) == NULL) {
      cp = (unsigned char*)&(addr.s_addr);
      IPSHIFT(cp, addr.s_addr);
      printf("Trying %s (%lx)...\n", inet_ntoa(addr), addr.s_addr);
      if((host = gethostbyaddr_r((const char *)&addr, 4, AF_INET)) == NULL) {      
	fprintf(stderr, "No host (name) found\r\n");
	return 0; 
      }
    }
    printf("%s\n", host->h_name); 
    /* free_buf((union SIGNAL **)&host); */
    return 0;
  }
  else {			/* lookup IP */
    char *ptr;
    printf("Finding address for %s...\n", buf);
    host = gethostbyname_r(buf);
    if (host == NULL) { 
      fprintf(stderr, "No host (addr) found\r\n");
      return 0; 
    }
    ptr = (char *)alloc(24, 0); 
    ipaddr_string((struct in_addr *)(host->h_addr_list[0]), ptr); 
    free_buf((union SIGNAL **)&host); 
    printf("%s\n", ptr); 
    free_buf((union SIGNAL **)&ptr);
    return 0;    
  }
}
