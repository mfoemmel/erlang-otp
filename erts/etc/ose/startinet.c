/*
 ****************************************************************************
 *                            PROGRAM MODULE
 *
 *     $Workfile:   startinet.c
 *     Document no: 511/OSE33-37
 *     $Version:   /main/tb_ri24/18   *     $Author:   integrat   *     $Date:   07/19/01 18:48:01   *
 *     Copyright (C) 1995-1999 by ENEA OSE Systems AB. All rights reserved.          
 *              INET was created by Lennart Bang 1995-98.
 ****************************************************************************
 */

/*
 ****************************************************************************
 * 1  DESCRIPTION.
 *
 * This file is an example of how to initialize INET from a process called
 * 'init_inet'. The example shows how to attach both an Ethernet and a PPP
 * interface as well as configure a default router after the interface has
 * come up. Furthermore, INET is configured to act as a router and forward
 * packets between the two interfaces.
 *
 * The advantage of having a process initializing INET instead of sending
 * the INET initialization and configuration signals from an OSE start hook
 * is that the reply signals can be received and the 'status' value can be
 * checked to see it the operation went well. 
 *
 * If you want to use this process you must either declare it in osemain.con
 * or dynamically create it with the OSE system call create_process().
 *
 * This file may be customized by defining various preprocessor macros. A
 * breif explaination of these macros follows:
 *
 * USE_OSEDEF_H
 *   Compilation macros is taken from a headerfile.
 *
 * ETHNAME
 * ETHUNIT
 * ETH_IP_ADDRESS
 * ETH_SUBNETMASK (optional)
 *   Used when attaching an interface to INET. The name (ETHNAME) and unit (ETHUNIT)
 *   of ethernet device to use for the INET interface with IP address ETH_IP_ADDRESS.
 *
 * PETHNAME
 * PETHUNIT<unit>
 * PETH_IP_ADDRESS<unit>
 * PETH_SUBNETMASK<unit>   (optional)
 *   Used when attaching pseudo-ethernet interfaces to INET for the IXP1200 board.
 *   The name (PETHNAME) and unit (PETHUNIT) of ethernet device to use for the INET
 *   interface with IP address PETH_IP_ADDRESS.
 *
 * PPP_SERNAME
 * PPP_SERUNIT
 * PPP_IP_ADDRESS
 * PPP_SUBNETMASK
 * PPP_REMOTE_IP_ADDR
 * PPP_BAUDRATE
 *   Same as above but for PPP.
 *
 * DEFAULT_GATEWAY
 *   IP address of default gateway.
 *
 * USE_DNS
 *   If we have access to DNS related functions (from INETUTIL).
 *
 * DNS_IP_ADDR
 *   The IP address of your DNS server.
 *
 * DNS_DOMAIN
 *   The DNS domain the host resides in.
 *
 * USE_DHCP_ETH1
 *   If the first interface should be initialized by DHCP (from INETUTIL).
 *
 * USE_DHCP_ETH2
 *   If the second interface should be initialized by DHCP (from INETUTIL).
 *
 * USE_OSPF
 *   If OSPS is to be used in the system.
 *
 * USE_RIP
 *   If RIP is to be used in the system. 
 *
 * USE_PPP_PAP_AUTHENTICATE
 *   Respond to authenticate challanges from peer (currently PAP and CHAP).
 *
 * USE_PPP_PAP_LOGIN
 * USE_PPP_CHAP_LOGIN
 *   Require peer to authenticate itself using PAP/CHAP.
 *
 */
#ifdef USE_OSEDEF_H
#include "osedef.h"
#endif

#include <string.h>
#include "inet.sig"
#include "ose.h"
#include "md5.h"

#ifdef USE_DNS
#include "nameser.h"
#include "resolv.h"
#include "netdb.h"
#endif

#if defined(USE_DHCP_ETH1) || defined(USE_DHCP_ETH2)
#include "dhcp.h"
#endif

#ifdef USE_RIP
#include "nsrip.h"
#include "oserip.h"
#endif


/*
 *===========================================================================
 *                         ERROR CODES
 *===========================================================================
 */
#define INETINIT_ERR_BASE             0x02000000
#define INETINIT_ERR_ATTACH           (INETINIT_ERR_BASE + 0x00000)
#define INETINIT_ERR_CONFIG           (INETINIT_ERR_BASE + 0x10000)
#define INETINIT_ERR_IFUP             (INETINIT_ERR_BASE + 0x20000)
#define INETINIT_ERR_ADDROUTE         (INETINIT_ERR_BASE + 0x30000)
#define INETINIT_ERR_OSPFATTACH       (INETINIT_ERR_BASE + 0x40000)
#define INETINIT_ERR_IFDOWN           (INETINIT_ERR_BASE + 0x50000)


/*
 *===========================================================================
 *                         SIGNAL
 *===========================================================================
 */

union SIGNAL 
{
      SIGSELECT                   sigNo;
      struct InetIfAttach         IF;
      struct InetIfUp             up;
      struct InetIfDown           down;      
      struct InetSetConfig        config;
      struct InetRouteAdd         route;
      struct LinkIfAuthenticate   authreq;
      struct LinkIfLogin          authlogin;
#ifdef USE_DNS
      struct ResSetConfig         dnsconfig;
#endif
};


/*
 *===========================================================================
 *                            CONSTANTS
 *===========================================================================
 */

/******** Ethernet interface name ********/
#ifdef ETHNAME
static const char *Eth0Name = "eth0";
#endif

#ifdef ETHNAME2
static const char *Eth1Name = "eth1";
#endif


/******** Pseudo Ethernet interface names for IXP1200 ********/
#ifdef PETHUNIT1
static const char *PEth1Name = "peth0";
#endif

#ifdef PETHUNIT2
static const char *PEth2Name = "peth1";
#endif

#ifdef PETHUNIT3
static const char *PEth3Name = "peth2";
#endif

#ifdef PETHUNIT4
static const char *PEth4Name = "peth3";
#endif

#ifdef PETHUNIT5
static const char *PEth5Name = "peth4";
#endif

#ifdef PETHUNIT6
static const char *PEth6Name = "peth5";
#endif

#ifdef PETHUNIT7
static const char *PEth7Name = "peth6";
#endif

#ifdef PETHUNIT8
static const char *PEth8Name = "peth7";
#endif


/******** PPP interface name ********/
#ifdef PPP_SERNAME
static const char *Ppp0Name = "ppp0";
#endif


/*
 *===========================================================================
 *			 EXTERNAL PROTOTYPES
 *===========================================================================
 */
extern PROCESS ose_inet_;

#ifdef USE_OSPF
/* Defined in initospf.c from the OSPF example code */
extern void initOSPF(U32 routerid);
extern void attachOSPF(U32 ipAddr, U32 ipMask, U32 ifType, const char *ifname, U32 ifSpeed);
#endif

/*
 *===========================================================================
 *                    ipfilter
 *===========================================================================
 * Description:    Interface specific IP filter hook.
 * Parameters:
 * Returns:        -1 to silently discard packet.
 *                  0 for normal IP input address matching.
 *                  1 to accept packet without comparing addresses.
 *
 * To add this hook for one interface, add to the iftags:
 *  sig->IF.iftags[x]   = INET_IFTAG_IPFILTER;
 *  sig->IF.iftags[x+1] = (InetTag)ipfilter;
 */
int
ipfilter(char *pkt)
{
   /*
    * There are a couple of macros defined in inet.h that helps
    * in extracting the various fields from the protocol headers
    * that one may want to use when performing filtering.
    *
    * IPGET_LEN(pkt)       - IP length field
    * IPGET_PROTOCOL(pkt)  - IP prptpcol field
    * IPGET_SRCADDR(pkt)   - IP source 
    * IPGET_DSTADDR(pkt)   - and destination address
    *
    * UDPGET_SRCPORT(pkt)  - TCP/UDP source port number
    * UDPGET_DSTPORT(pkt)  - TCP/UDP destination port number
    * UDPGET_LEN(pkt)      - UDP length field
    *
    */
   (void)pkt;
   /* Normal packet processing. */
   return 0;
}


/*
 *===========================================================================
 *                    arprequesthook
 *===========================================================================
 * Description:    Interface unique ARP request hook.
 * Parameters:
 * Returns:        -1 to silently discard ARP request without replying.
 *                  0 for normal ARP request parsing.
 *                  1 to send an ARP reply regardless of target IP address.
 *
 * To add this hook for one interface, add to the iftags:
 *  sig->IF.iftags[x]   = INET_IFTAG_ARPREQUEST_HOOK;
 *  sig->IF.iftags[x+1] = (InetTag)arprequesthook;
 */
int
arprequesthook(char *pkt)
{
   /*
    * Macros for extracting ARP protocol header information:
    *
    * senderip = ARPGET_SENDER_ADDR(pkt);
    * targetip = ARPGET_TARGET_ADDR(pkt);
    *
    */
   (void)pkt;
   return 0;
}


/*
 *===========================================================================
 *                    attachEth
 *===========================================================================
 * Description:
 *   Attach ethernet interface to INET.

 * Parameters:
 *   ipAddr - Interface IP address.
 *   ifname - Interface name
 *   ddname - Name of device driver.
 *   ddunit - Device unit number.
 *
 */
#if defined(ETHNAME) || defined(ETHNAME2) || defined(PETHNAME)
static void
attachEth(PROCESS pid, U32 ipAddr, U32 subnetmask, const char *ifname, const char *ddname, U32 ddunit)
{
   union SIGNAL   *sig;
   static const SIGSELECT sel[2] = {1, INET_IF_ATTACH_REPLY};
   int         i = 0;

   /* '+ 64' allocates space for 8 sets of tags. */
   sig = alloc(sizeof(struct InetIfAttach) + 64, INET_IF_ATTACH_REQUEST);

   /* IP address. */
   sig->IF.ipAddr = ipAddr;
      
   /* Interface data. */
   strcpy(sig->IF.ifName, ifname);
   sig->IF.ifType = IFT_ETHER;
      
   /* Device driver data. */
   strcpy(sig->IF.ddName, ddname);
   sig->IF.ddUnit = ddunit;
   
   /* Interface configuration. */
   if (-1 != subnetmask)
   {      
      sig->IF.iftags[i++] = INET_IFTAG_SUBNETMASK;
      sig->IF.iftags[i++] = subnetmask;
   }
   
   /* Metric is equal to 10 for a 10MBit Ethernet, value should be 1 for 100MBit.
    */
   sig->IF.iftags[i++] = INET_IFTAG_METRIC;
   sig->IF.iftags[i++] = (InetTag)10;   
      
      /* Enable translation from IP multicat -> Ethnernet multicast (by default IP multicast
       * addresses are mapped on the Ethernet broadcast address)
       */
   sig->IF.iftags[i++] = INET_IFTAG_HW_MULTICAST_ADDR;
   sig->IF.iftags[i++] = 1;
      
      /* Uncomment the two lines below if you want to use an IP filter hook.
       *
       * sig->IF.iftags[i++] = INET_IFTAG_IPFILTER;
       * sig->IF.iftags[i++] = (InetTag)ipfilter;
       */
      
      /* Uncomment the two lines below if you want to add an ARP request hook.
       *
       * sig->IF.iftags[i++] = INET_IFTAG_ARPREQUEST_HOOK;
       * sig->IF.iftags[i++] = (InetTag)arprequesthook;
       */
      
      /* Uncomment to assign IP address via RARP
       *
       * sig->IF.iftags[i++] = INET_IFTAG_RARP;
       * sig->IF.iftags[i++] = -1;
       */
      
      /* A tag array MUST end with this tag
       */
   sig->IF.iftags[i] = INET_TAG_END;             
      
   /* Send attach signal to INET and wait for reply. */
   send(&sig, pid);
   sig = receive((SIGSELECT *)sel);
      
      /* Verify the INET reply. */
   if(sig->IF.status)
      error(INETINIT_ERR_ATTACH + sig->IF.status);
      
   free_buf(&sig);
      
   /* Request a notification signal when the interface is up. */
   sig = alloc(sizeof(struct InetIfUp), INET_IF_UP_REQUEST);
   strcpy(sig->up.ifName, ifname);
   send(&sig, ose_inet_);
      
   /* Attach OSPF. */
#ifdef USE_OSPF
   attachOSPF(ipAddr, inet_addr(ETH_SUBNETMASK), IFT_ETHER, ifname, 10000000);
#endif
}
#endif


/*
 *===========================================================================
 *                    attachAlias
 *===========================================================================
 * Description:
 *   Attach an IP alias address to specified interface
 *
 * Parameters:
 *   ipAddr   - IP alias address
 *   ifname   - Alias interface name (usually something like "eth0:1")
 *   aliasIdx - Interface infex of corresponding physical interface.
 *
 */
#if 0
void
attachAlias(PROCESS pid, U32 ipAddr, U32 subnetmask, const char *ifname, U32 aliasIdx)
{
   union SIGNAL   *sig;
   static const SIGSELECT sel[2] = {1, INET_IF_ATTACH_REPLY};
   int            i = 0;

   /* '+ 64' allocates space for 8 sets of tags. */
   sig = alloc(sizeof(struct InetIfAttach) + 64, INET_IF_ATTACH_REQUEST);

   /* IP address. */
   sig->IF.ipAddr = ipAddr;

   /* Interface data. */
   strcpy(sig->IF.ifName, ifname);
   sig->IF.ifType = IFT_ETHER;
  
   /* Interface configuration. */
   if (-1 != subnetmask)
   {
      sig->IF.iftags[i++] = INET_IFTAG_SUBNETMASK;
      sig->IF.iftags[i++] = subnetmask;
   }

   sig->IF.iftags[i++] = INET_IFTAG_IP_ALIAS;
   sig->IF.iftags[i++] = (InetTag)aliasIdx;

   /* A tag array MUST end with this tag. */
   sig->IF.iftags[i] = INET_TAG_END;             

   /* Send attach signal to INET and wait for reply. */
   send(&sig, pid);
   sig = receive((SIGSELECT *)sel);

   /* Verify the INET reply. */
   if(sig->IF.status)
      error(INETINIT_ERR_ATTACH + sig->IF.status);

   free_buf(&sig);
}
#endif


#ifdef PPP_SERNAME
/*
 *===========================================================================
 *			   ppp_client_auth
 *===========================================================================
 * Description:
 *   Called when remote side requires authentication. The signal contains
 *   all neccecary information to complete authentication.
 *
 *   For PAP, all that is required is to set the 'id' and 'password'
 *   fields to some appropriate values and return the signal.
 *
 *   CHAP requires us to compute a MD5 checksum based on the CHAP ID,
 *   the secret (our password to the remote system) and the CHAP Challange
 *   (supplied in the signal together with the CHAP ID). The checksum
 *   together with our username is returned in the reply signal.
 *   
 *===========================================================================
 */
void
ppp_client_auth(PROCESS pid, union SIGNAL *sig)
{
   const char *username = "ppp";
   const char *password = "kallekula";

   if ( sig->authreq.authprot == PPP_PAP )
   {
      /* PAP - Fill in ID and password
       */
      strcpy(sig->authreq.authinfo.pap.id, username);
      strcpy(sig->authreq.authinfo.pap.passwd, password);
   }
   else if ( sig->authreq.authprot == PPP_CHAP )
   {
      /* CHAP - Compute MD5 checksum
       */
      MD5_CTX context;

      md5_init(&context);

      /* CHAP Challange ID */
      md5_update(&context, &sig->authreq.authinfo.chap.id, 1);

      /* CHAP Secret (our password) */
      md5_update(&context, password, strlen(password));

      /* CHAP Challange */
      md5_update(&context, &sig->authreq.authinfo.chap.challange,
                 sig->authreq.authinfo.chap.challangeLen);

      md5_final((U8*)&sig->authreq.authinfo.chap.response, &context);

      sig->authreq.authinfo.chap.responseLen = 16;
      strcpy(sig->authreq.authinfo.chap.name, username);
   }
   else
   {
      /* Unkown PPP authentication protocol */
      free_buf(&sig);
   }

   /* Return signal */
   sig->sigNo++;
   send(&sig, pid);
}


/*
 *===========================================================================
 *			   ppp_server_auth
 *===========================================================================
 * Description:
 *   Called to handle/verify a peer authentication.
 *
 *   For PAP we just compare the supplied username and password against
 *   our password database (in this example two const strings!).
 *
 *   With CHAP we compute the MD5 checksum in the same way described above.
 *   We then compare our result with the checksum sent from the peer. If
 *   they match we have a succesful login. The feature here (with CHAP) is
 *   that the secret (password) never appear on the media, only the checksums
 *   are exchanged.
 *
 *===========================================================================
 */
void
ppp_server_auth(PROCESS pid, union SIGNAL *sig)
{
   const char *username = "ppp";
   const char *password = "kallekula";

   if ( sig->authlogin.authprot == PPP_PAP )
   { 
      if (strcmp(sig->authlogin.authinfo.pap.id, (char *)username) == 0 &&
          strcmp(sig->authlogin.authinfo.pap.passwd, (char *)password) == 0)
      {
         sig->authlogin.status = AUTH_STATUS_LOGIN_SUCCESS;
      }
      else
      {
         sig->authlogin.status = AUTH_STATUS_LOGIN_FAILURE;
      }
   }
   else if ( sig->authlogin.authprot == PPP_CHAP )
   {
      MD5_CTX context;
      U8      digest[16];

      md5_init(&context);
      md5_update(&context, &sig->authlogin.authinfo.chap.id, 1);
      md5_update(&context, password, strlen(password));
      md5_update(&context, sig->authlogin.authinfo.chap.challange,
                 sig->authlogin.authinfo.chap.challangeLen);
      md5_final(digest, &context);

      if ( strcmp(username, sig->authlogin.authinfo.chap.name) == 0 &&
           memcmp(digest, sig->authlogin.authinfo.chap.response, 16) == 0 )
      {
         sig->authlogin.status = AUTH_STATUS_LOGIN_SUCCESS;
      }
      else
      {
         sig->authlogin.status = AUTH_STATUS_LOGIN_FAILURE;
      }
   }

   /* Return the signal */
   sig->sigNo++;
   send(&sig, pid);
}
#endif


/*
 *===========================================================================
 *			      attachPPP
 *===========================================================================
 * Description:
 *   Attach a PPP interface to INET.
 */
#ifdef PPP_SERNAME
static void
attachPPP(PROCESS pid)
{
   static const SIGSELECT sel[2] = {1, INET_IF_ATTACH_REPLY};
   union SIGNAL   *sig;
   int             i = 0;
   U32             ipaddr;

   /* '+ 128' allocates space for 16 sets of tags. */
   sig = alloc(sizeof(struct InetIfAttach) + 128, INET_IF_ATTACH_REQUEST);

   /* IP address. */
   ipaddr = inet_addr(PPP_IP_ADDRESS);
   sig->IF.ipAddr = ipaddr;

   /* Interface name. */
   strcpy(sig->IF.ifName, Ppp0Name);
   sig->IF.ifType = IFT_PPP;

   /* Device driver. */
   strcpy(sig->IF.ddName, PPP_SERNAME);
   sig->IF.ddUnit = PPP_SERUNIT;

   /*
    * Additional PPP interface configuration
    */

   /* Baudrate */
   sig->IF.iftags[i++] = INET_IFTAG_BAUDRATE;
   sig->IF.iftags[i++] = (InetTag)PPP_BAUDRATE;

   /* Interface metric */
   sig->IF.iftags[i++] = INET_IFTAG_METRIC;
   sig->IF.iftags[i++] = (InetTag)100000000 / PPP_BAUDRATE;

   /* Disable Asynchronous character map. */
   sig->IF.iftags[i++] = INET_IFTAG_DISABLEACCM;
   sig->IF.iftags[i++] = 1;
 
   /* Remote site IP address (if requested) */
#ifdef PPP_REMOTE_IP_ADDR
   if (inet_addr(PPP_REMOTE_IP_ADDR) != INADDR_NONE)
   {
      sig->IF.iftags[i++] = INET_IFTAG_REMOTEIPADDR;
      sig->IF.iftags[i++] = (InetTag)inet_addr(PPP_REMOTE_IP_ADDR);
   }
#endif

#ifdef PPP_SUBNETMASK
   sig->IF.iftags[i++] = INET_IFTAG_SUBNETMASK;
   sig->IF.iftags[i++] = (InetTag)inet_addr(PPP_SUBNETMASK);
#endif

   /* Uncomment the two lines below if you want to use an IP filter hook
    * sig->IF.iftags[i++] = INET_IFTAG_IPFILTER;
    * sig->IF.iftags[i++] = (InetTag)ipfilter;
    */

   /* Uncomment the two lines below if you want to enable proxy RARP.
    * sig->IF.iftags[i++] = INET_IFTAG_PROXYARP;
    * sig->IF.iftags[i++] = 1;
    */

#ifdef USE_PPP_PAP_AUTHENTICATE
   sig->IF.iftags[i++] = INET_IFTAG_AUTHENTICATE_PID;
   sig->IF.iftags[i++] = (InetTag)current_process();
#endif
   
#ifdef USE_PPP_PAP_LOGIN
   sig->IF.iftags[i++] = INET_IFTAG_PAPLOGIN_PID;
   sig->IF.iftags[i++] = (InetTag)current_process();
#endif

#ifdef USE_PPP_CHAP_LOGIN
   sig->IF.iftags[i++] = INET_IFTAG_CHAPLOGIN_PID;
   sig->IF.iftags[i++] = (InetTag)current_process();
#endif

   /* A tag array MUST end with this tag. */
   sig->IF.iftags[i] = INET_TAG_END;                      
  
   /* Send attach signal to INET and wait for reply. */
   send(&sig, pid);
   sig = receive((SIGSELECT *)sel);

   /* Verify the INET reply. */
   if(sig->IF.status)
      error(INETINIT_ERR_ATTACH + sig->IF.status);

   free_buf(&sig);

   /* Request a notification signal when the interface is up. */
   sig = alloc(sizeof(struct InetIfUp), INET_IF_UP_REQUEST);
   strcpy(sig->up.ifName, Ppp0Name);
   send(&sig, ose_inet_);

   /* Attach OSPF. */
#ifdef USE_OSPF
   attachOSPF(ipaddr, inet_addr(PPP_SUBNETMASK), IFT_PPP, Ppp0Name, PPP_BAUDRATE);
#endif
}
#endif


/*
 *===========================================================================
 *                    configure
 *===========================================================================
 * Description:     Enable IP forwarding in INET.
 *
 */
static void
configure(void)
{
   union SIGNAL   *sig;
   static const SIGSELECT sel[2] = {1, INET_SET_CONFIG_REPLY};
   int i = 0;

   /* Allocate space for 4 options (8 bytes each). */
   sig = alloc(sizeof(struct InetSetConfig) + 32, INET_SET_CONFIG_REQUEST);

   sig->config.conftags[i++] = INET_CONFTAG_IPFORWARDING;   /* Enable IP FORWARDING */
   sig->config.conftags[i++] = 1;

#ifdef INET_CONFTAG_SETFLAGS
   /* A INET 3.x non-backwards compatible huge memory saver for RX ABuf's. 
    * introduced in INET r4.0.0.
    */
   sig->config.conftags[i++] = INET_CONFTAG_SETFLAGS;
   sig->config.conftags[i++] = INET_CONFLAG_MINRXABUF;
#endif

   sig->config.conftags[i] = INET_TAG_END;

   /* Send configuration INET and wait for reply. */
   send(&sig, ose_inet_);
   sig = receive((SIGSELECT *)sel);

   /* Verify the INET reply. */
   if(sig->config.status)
      error(INETINIT_ERR_CONFIG + sig->config.status);

   free_buf(&sig);
}


/*
 *===========================================================================
 *                      init_resolver
 *===========================================================================
 */
#ifdef USE_DNS
static void
init_resolver(const char *dns_addr, const char *dns_domain)
{
   static const SIGSELECT confSig[] = {1, RES_CONF_SET};
   PROCESS       resolve_pid;

   /* Hunt for the ose_resolve process */
   if (hunt("ose_resolve", 0, &resolve_pid, NULL))
   {
      union SIGNAL *sig;
      ResTag       *tag;
      int           len;
      int           i = 0;
      int           extra;

      /* Make extra room in the configuration signal for 10 tag pairs plus
       * the domain name
       */
      extra = sizeof(ResTag)*20 + strlen(dns_domain) + 1;

      sig = (union SIGNAL *)alloc(sizeof(struct resSetConfig)+extra, RES_CONF_SET);

      tag = sig->dnsconfig.conftags;

      /* Set time between retransmissions (sec) */
      tag[i++] = RES_TAG_RETRANS;
      tag[i++] = 1;

      /* Set number of retransmissions. */
      tag[i++] = RES_TAG_RETRY;
      tag[i++] = 3;

      /* Enable debugging. (currently no debugging available) */
      tag[i++] = RES_TAG_DEBUG;
      tag[i++] = False;
    
      /* Do not throw away truncated replies. */
      tag[i++] = RES_TAG_IGNORE_TRUNC;
      tag[i++] = True;

      /* Request recursive search by DNS server. */
      tag[i++] = RES_TAG_RECURSE;
      tag[i++] = True;

      /* Append the default domain name to queries. */
      tag[i++] = RES_TAG_USE_DEFNAMES;
      tag[i++] = True;
  
      /* Search up local domain tree. */
      tag[i++] = RES_TAG_DNSRCH;
      tag[i++] = True;

      /* Add DNS server address. */
      tag[i++] = RES_TAG_NS;
      tag[i++] = 1;			  /* Size of this tag */
      tag[i++] = inet_addr(dns_addr);

      /* Default domain name to be used in queries. */
      len = 1 + (strlen(dns_domain) >> 2);
      tag[i++] = RES_TAG_DEFDNAME;
      tag[i++] = len;			  /* Size of this tag */
      strcpy((char *)&tag[i], dns_domain);
      i += len;

      tag[i] = RES_TAG_END;

      /* Send signal to DNS resolver */
      send(&sig, resolve_pid);
      sig = receive((SIGSELECT *)confSig);
      free_buf(&sig);
   }
}
#endif


/*
 *===========================================================================
 *                         init_inet
 *===========================================================================
 */
OS_PROCESS(init_inet)
{
   PROCESS ppplink_pid = 0;		/* PID of PPP link module process. */
   PROCESS ethlink_pid = 0;		/* PID of Ethernet link module process. */
   int interfaces = 0;			/* Number of interfaces reported UP */
#if defined(USE_DHCP_ETH1) || defined(USE_DHCP_ETH2)
   char              *dhcpIf1 = NULL;
   char              *dhcpIf2 = NULL;
   struct dhcp_reply *dhcpReply;
#endif

   /* Hunt for our link processes */
   hunt("ose_ethlink", 0, &ethlink_pid, NULL);
   hunt("ose_ppplink", 0, &ppplink_pid, NULL);

   /*
    *  Initialize OSPF with first IP address as router ID.
    */

#ifdef USE_OSPF
#ifdef ETH_IP_ADDRESS
   initOSPF(inet_addr(ETH_IP_ADDRESS));
#else
   initOSPF(inet_addr(PPP_IP_ADDRESS));
#endif
#endif

   /*
    *  Attach the Ethernet interface(s)
    */

#ifdef ETHNAME
#ifdef USE_DHCP_ETH1
   attachEth(ethlink_pid, inet_addr("0.0.0.0"), inet_addr(ETH_SUBNETMASK), Eth0Name, ETHNAME, ETHUNIT);
   dhcpIf1 = (char *)alloc(strlen(Eth0Name) + 1, 0);
   strcpy(dhcpIf1, Eth0Name);
#else
   attachEth(ethlink_pid, inet_addr(ETH_IP_ADDRESS), inet_addr(ETH_SUBNETMASK), Eth0Name, ETHNAME, ETHUNIT);
   /*
    * Uncomment to use an IP alias address for this interface. Change 10.0.0.1
    * to your favorite IP alias :-) Alias interface should have a name that
    * reflects the fact that they are aliases, ":1" is used here.
    *
    * attachAlias(inet_addr("10.0.0.1"), inet_addr(ETH_SUBNETMASK), strcat(Eth0Name,":1"), 1);
    */
#endif
#endif

#ifdef ETHNAME2
#ifdef USE_DHCP_ETH2
   attachEth(ethlink_pid, inet_addr("0.0.0.0"), inet_addr(ETH_SUBNETMASK), Eth1Name, ETHNAME2, ETHUNIT2);
   dhcpIf2 = (char *)alloc(strlen(Eth1Name) + 1, 0);
   strcpy(dhcpIf2, Eth1Name);
#else
   attachEth(ethlink_pid, inet_addr(ETH_IP_ADDRESS2), inet_addr(ETH_SUBNETMASK), Eth1Name, ETHNAME2, ETHUNIT2);
#endif
#endif

   /*
    *  Attach the Pseudo-Ethernet interface(s) for IXP1200
    */
#ifdef PETHUNIT1
   attachEth(ethlink_pid, inet_addr(PETH_IP_ADDRESS1), inet_addr(PETH_SUBNETMASK1), PEth1Name, PETHNAME, PETHUNIT1);
#endif

#ifdef PETHUNIT2
   attachEth(ethlink_pid, inet_addr(PETH_IP_ADDRESS2), inet_addr(PETH_SUBNETMASK2), PEth2Name, PETHNAME, PETHUNIT2);
#endif

#ifdef PETHUNIT3
   attachEth(ethlink_pid, inet_addr(PETH_IP_ADDRESS3), inet_addr(PETH_SUBNETMASK3), PEth3Name, PETHNAME, PETHUNIT3);
#endif

#ifdef PETHUNIT4
   attachEth(ethlink_pid, inet_addr(PETH_IP_ADDRESS4), inet_addr(PETH_SUBNETMASK4), PEth4Name, PETHNAME, PETHUNIT4);
#endif

#ifdef PETHUNIT5
   attachEth(ethlink_pid, inet_addr(PETH_IP_ADDRESS5), inet_addr(PETH_SUBNETMASK5), PEth5Name, PETHNAME, PETHUNIT5);
#endif

#ifdef PETHUNIT6
   attachEth(ethlink_pid, inet_addr(PETH_IP_ADDRESS6), inet_addr(PETH_SUBNETMASK6), PEth6Name, PETHNAME, PETHUNIT6);
#endif

#ifdef PETHUNIT7
   attachEth(ethlink_pid, inet_addr(PETH_IP_ADDRESS7), inet_addr(PETH_SUBNETMASK7), PEth7Name, PETHNAME, PETHUNIT7);
#endif

#ifdef PETHUNIT8
   attachEth(ethlink_pid, inet_addr(PETH_IP_ADDRESS8), inet_addr(PETH_SUBNETMASK8), PEth8Name, PETHNAME, PETHUNIT8);
#endif

   /*
    * Attach the PPP interface
    */
#ifdef PPP_SERNAME
   attachPPP(ppplink_pid);
#endif

   /* Global configuration of INET */
   configure();

   /* Initialize DNS client */
#if defined(USE_DNS)
   init_resolver(DNS_IP_ADDR, DNS_DOMAIN);
#endif

   /*
    * Signal loop
    */
   for(;;)
   {
      union SIGNAL           *sig;
      static const SIGSELECT anySig[] = {0};

      sig = receive((SIGSELECT *)anySig);

      switch (sig->sigNo)
      {
	 case INET_IF_UP_REPLY:
	    /*
	     * (1) Verify the INET reply
	     */
	    if (sig->up.status)
	    {
	       error(INETINIT_ERR_IFUP + sig->up.status);
	       free_buf(&sig);
	       break;
	    }

	    /*
	     * (2) Let DHCP configure interface.
	     */
#if defined(USE_DHCP_ETH1) || defined(USE_DHCP_ETH2)
	    if ((dhcpIf1 != NULL && strcmp(dhcpIf1, sig->up.ifName) == 0) ||
		(dhcpIf2 != NULL && strcmp(dhcpIf2, sig->up.ifName) == 0) )
	    {
	       /*
		* Start DHCP for interface
		*/ 
	       dhcpReply = dhcpQuery(sig->up.ifName, (DhcpTag *)NULL);
	       if (dhcpReply != NULL)
	       {
		  free_buf((union SIGNAL **)&dhcpReply);
	       }
	       free_buf(&sig);
	       break;
	    }
#endif

	    /*
	     * (3) Add a default route once the first interface comes up
	     */
#ifdef DEFAULT_GATEWAY
	    if (interfaces++ == 0 && inet_addr(DEFAULT_GATEWAY) != INADDR_NONE)
	    {
	       static const SIGSELECT rtsel[2] = {1, INET_ROUTE_ADD_REPLY};
	       union SIGNAL          *rtsig;

	       /* Add default gateway route.
		*/
	       rtsig = alloc(sizeof(struct InetRouteAdd), INET_ROUTE_ADD_REQUEST);
	       rtsig->route.ipAddr   = 0;
	       rtsig->route.gateAddr = inet_addr(DEFAULT_GATEWAY);
	       rtsig->route.maskAddr = htonl(0xffffffff);
	       rtsig->route.metric   = 1;
	       rtsig->route.routetags[0] = INET_TAG_END;

	       send(&rtsig, ose_inet_);
	       rtsig = receive((SIGSELECT *)rtsel);

	       if (rtsig->route.status)
	       {
		  error(INETINIT_ERR_IFUP + rtsig->route.status);
	       }
	       free_buf(&rtsig);
	    }
#endif

#ifdef USE_RIP
	    /*
	     * (4) Enable RIP on this interface.
	     */
	    if(strncmp(sig->up.ifName, "ppp", 3) == 0)
	    {
	       /* This example lets RIP on PPP use MD5 authentication. */
	       if(oserip_open_interface(sig->up.ifName, NSFALSE, NSRIP_AUTHTYPE_MD5) != 0)
	       {
		  error(OSERIP_ERR_OPEN_IF);
	       }
	    }
	    else
	    {
	       /* This example lets RIP on all non ppp interfaces be unauthenticated. */
	       if(oserip_open_interface(sig->up.ifName, NSFALSE, NSRIP_AUTHTYPE_NONE) != 0)
	       {
		  error(OSERIP_ERR_OPEN_IF);
	       }
	    }

	    /* Request a down signal. */
	    sig->sigNo = INET_IF_DOWN_REQUEST;
	    send(&sig, ose_inet_);
#endif
	    break;


	 case INET_IF_DOWN_REPLY:
	    /* Verify the INET reply. */
	    if(sig->down.status)
	    {
	       error(INETINIT_ERR_IFDOWN + sig->down.status);
	       free_buf(&sig);
	       break;
	    }

	    /*
	     * (1) Disable RIP on this interface.
	     */
#ifdef USE_RIP
	    (void)oserip_close_interface(sig->up.ifName);
#endif

	    /*
	     * (2) Request a up signal.
	     */
	    sig->sigNo = INET_IF_UP_REQUEST;
	    send(&sig, ose_inet_);
	    break;


#ifdef PPP_SERNAME
	 case INETLINK_IF_AUTHENTICATE_REQUEST:
	    /*
	     * Peer authenticate request
	     */
	    ppp_client_auth(ppplink_pid, sig);
	    break;


	 case INETLINK_IF_AUTHENTICATE_STATUS:
	    /*
	     * Authenticate failure or success
	     */
	    free_buf(&sig);
	    break;


	 case INETLINK_IF_LOGIN_REQUEST:
	    /*
	     * Peer login request
	     */
	    ppp_server_auth(ppplink_pid, sig);
	    break;
#endif

	 default:
	    free_buf(&sig);
	    break;

      } /* switch(sigNo) */
   }  /* for(ever) */
}


/*
 *===========================================================================
 *                         END OF FILE
 *===========================================================================
 */
