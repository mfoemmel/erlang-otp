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
package com.ericsson.otp.erlang;

import java.net.InetAddress;
import java.net.UnknownHostException;

/**
 * Represents an OTP node, a communication endpoint. It is not
 * possible to create instances of this class, as it is only intended
 * as a superclass for {@link OtpSelf}, {@link OtpServer} and {@link
 * OtpPeer}.
 * 
 * <p> About nodenames: Erlang nodenames consist of two components, an
 * alivename and a hostname separated by '@'. Additionally, there are
 * two nodename formats: short and long. Short names are of the form
 * "alive@hostname", while long names are of the form
 * "alive@host.fully.qualified.domainname". Erlang has special
 * requirements regarding the use of the short and long formats, in
 * particular they cannot be mixed freely in a network of
 * communicating nodes. See the Erlang documentation for more
 * information about nodenames.
 * 
 * <p> The constructors for the OtpNode classes will create names
 * exactly as you provide them as long as the name contains '@'. If
 * the string you provide contains no '@', it will be treated as an
 * alivename and the name of the local host will be appended,
 * resulting in a shortname. Nodenames longer than 255 characters will
 * be truncated without warning.
 *
 * <p> There are no constructors for this class, use one of the
 * subclasses instead.
 **/
public class OtpNode {
  String node;
  String host;
  String alive;
  String cookie;

  // Node types
  static final int NTYPE_R6 =        110; // 'n' post-r5, all nodes
  static final int NTYPE_R4_ERLANG = 109; // 'm' Only for source compatibility
  static final int NTYPE_R4_HIDDEN = 104; // 'h' Only for source compatibility
  
  // Node capability flags
  static final int dFlagPublished = 1;
  static final int dFlagAtomCache = 2;
  static final int dFlagExtendedReferences = 4;
  static final int dFlagDistMonitor = 8;
  static final int dFlagFunTags = 16;

  int ntype = NTYPE_R6;
  int proto = 0; // tcp/ip
  int distHigh = 5; // Cannot talk to nodes before R6
  int distLow = 5;  // Cannot talk to nodes before R6
  int creation = 0;
  int flags = dFlagExtendedReferences;

  OtpNode() {
  }
  
  OtpNode(String node) {
    this(node,"");
  }
  
  // if you want "longnames" then provide one yourself
  OtpNode(String node, String cookie) {
    int i = node.indexOf('@',0);
 
    // if '@' not found assume @localhost
    if (i < 0) {
      alive = node;
      try {
	host = InetAddress.getLocalHost().getHostName();
      }
      catch (UnknownHostException e) {
	host = "localhost";
      }
    }
    else {
      alive = node.substring(0,i);
      host = node.substring(i+1,node.length());
    }
    if (alive.length() > 0xff) {
      alive = alive.substring(0,0xff);
    }
    this.node = alive + "@" + host;
    this.cookie = cookie;
  }

  /**
   * Get the name of this node.
   *
   * @return the name of the node represented by this object.
   **/
  public String node() {
    return node;
  }

  /**
   * Get the hostname part of the nodename. Nodenames are composed of
   * two parts, an alivename and a hostname, separated by '@'. This
   * method returns the part of the nodename following the '@'.
   *
   * @return the hostname component of the nodename.
   **/
  public String host() {
    return host;
  }

  /**
   * Get the alivename part of the hostname. Nodenames are composed of
   * two parts, an alivename and a hostname, separated by '@'. This
   * method returns the part of the nodename preceding the '@'.
   *
   * @return the alivename component of the nodename.
   **/
  public String alive() {
    return alive;
  }

  /**
   * Get the authorization cookie used by this node.
   *
   * @return the authorization cookie used by this node.
   **/
  public String cookie() {
    return cookie;
  }

  // package scope
  int type() {
    return ntype;
  }
  
  // package scope
  int distHigh() {
    return distHigh;
  }

  // package scope
  int distLow() {
    return distLow;
  }

  // package scope: useless information?
  int proto() {
    return proto;
  }

  // package scope
  int creation() {
    return creation;
  }

  /**
   * Set the authorization cookie used by this node.
   *
   * @return the previous authorization cookie used by this node.
   **/
  public String setCookie(String cookie) {
    String prev = this.cookie;
    this.cookie = cookie;
    return prev;
  }

  public String toString() {
    return node();
  }
}
