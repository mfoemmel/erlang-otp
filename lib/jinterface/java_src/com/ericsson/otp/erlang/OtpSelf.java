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

import java.net.UnknownHostException;
import java.io.IOException;

/**
 * Represents a local OTP client-only node. It is used when it will be
 * neccessary for this node to connect to remote nodes, but when it is
 * not necessary for other nodes to be able to connect to this one.
 * 
 * <p> When the Java node will be connecting to a remote Erlang, Java
 * or C node, it must first identify itself as a node by creating an
 * instance of this class, after which it may connect to the remote
 * node.
 *
 * <p> This class does not bind a socket to a listen port as {@link
 * OtpServer} does, so it cannot be used when you want other nodes to
 * be able to establish connections to this one. However it can make
 * outgoing connections to other nodes as follows:
 *
 * <pre>
 * OtpSelf self = new OtpSelf("client","authcookie");  // identify self
 * OtpPeer other = new OtpPeer("server"); // identify peer
 * 
 * OtpConnection conn = self.connect(other); // connect to peer
 * </pre>
 *
 * @see OtpServer
 **/
public class OtpSelf extends OtpNode {
  private static int refCount = 1;
  private OtpErlangPid pid;

  /**
   * Create a self node.
   *
   * @param node the name of this node.
   *
   * @param cookie the authorization cookie that will be used by this
   * node when it communicates with other nodes.
   **/
  public OtpSelf(String node, String cookie) {
    super(node,cookie);
    this.pid = new OtpErlangPid(this.node(), count(), 0, creation());
  }

  /**
   * Get the Erlang PID that will be used as the sender id in all
   * messages sent by this node.
   *
   * @return the Erlang PID that will be used as the sender id in
   * all messages sent by this node.
   **/
  public OtpErlangPid pid() {
    return pid;
  }
  
  // package scope: used to get ref values
  static synchronized int count() {
    return refCount++;
  }

  /**
   * Open a connection to a remote node.
   *
   * @param other the remote node to which you wish to connect.
   *
   * @return a connection to the remote node.
   *
   * @exception java.net.UnknownHostException if the remote host could
   * not be found.
   *
   * @exception java.io.IOException if it was not possible to connect
   * to the remote node.
   *
   * @exception OtpAuthException if the connection was refused by the
   * remote node.
   **/
  public OtpConnection connect(OtpPeer other) 
    throws IOException, UnknownHostException, OtpAuthException  {
    return new OtpConnection(this, other);
  }
}
