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

import java.io.IOException;
import java.net.Socket;
import java.net.ServerSocket;

/**
 * Represents a local OTP client or server node. It is used when you
 * want other nodes to be able to establish connections to this one.
 *
 * When you create an instance of this class, it will bind a socket to
 * a port so that incoming connections can be accepted. However the
 * port number will not be made available to other nodes wishing to
 * connect until you explicitely register with the port mapper daemon
 * by calling {@link #publishPort()}.
 * 
 * <p> When the Java node will be connecting to a remote Erlang, Java
 * or C node, it must first identify itself as a node by creating an
 * instance of this class, after which it may connect to the remote
 * node.
 * 
 * <p> Setting up a connection may be done as follows:
 *
 * 
 * <pre>
 * OtpServer self = new OtpServer("server","cookie"); // identify self
 * self.publishPort(); // make port information available
 * 
 * OtpConnection conn = self.accept(); // get incoming connection
 * </pre>
 *
 * @see OtpSelf
 **/
public class OtpServer extends OtpSelf {
  private int port;
  private ServerSocket sock; 

  Socket epmd; // epmd publish handle
  
  /** Create an {@link OtpServer} from an existing {@link OtpSelf}.
   *
   * @param self an existing self node.
   *
   * @exception java.io.IOException if a ServerSocket could not be created.
   **/
  public OtpServer(OtpSelf self) 
    throws IOException {
    this(self.node(),self.cookie(),0);
  }

  /**
   * Create an OtpServer, using a vacant port chosen by the operating
   * system. To determine what port was chosen, call the object's
   * {@link #port()} method.
   *
   * @param node the name of the node.
   *
   * @param cookie the authorization cookie that will be used by this
   * node when accepts connections from remote nodes.
   *
   * @exception java.io.IOException if a ServerSocket could not be created.
   *
   **/
  public OtpServer(String node, String cookie) 
    throws IOException {
    this(node,cookie,0);
  }

  /**
   * Create an OtpServer, using the specified port number.
   *
   * @param node a name for this node, as above.
   *
   * @param cookie the authorization cookie that will be used by this
   * node when accepts connections from remote nodes.
   *
   * @param port the port number to bind the socket to.
   *
   * @exception java.io.IOException if a ServerSocket could not be
   * created or if the chosen port number was not available.
   **/
  public OtpServer(String node, String cookie, int port) 
    throws IOException {
    super(node,cookie);

    sock = new ServerSocket(port);

    if (port != 0) this.port = port;
    else this.port = sock.getLocalPort();
  }

  /**
   * Get the port number used by this node.
   *
   * @return the port number this server node is accepting
   * connections on.
   **/
  public int port() {
    return port;
  }


  /**
   * Make public the information needed by remote nodes that may wish
   * to connect to this one. This method establishes a connection to
   * the Erlang port mapper (Epmd) and registers the server node's
   * name and port so that remote nodes are able to connect.
   *
   * <p> This method will fail if an Epmd process is not running on
   * the localhost. See the Erlang documentation for information about
   * starting Epmd.
   *
   * <p> Note that once this method has been called, the node is
   * expected to be available to accept incoming connections. For that
   * reason you should make sure that you call {@link #accept()}
   * shortly after calling {@link #publishPort()}. When you no longer
   * intend to accept connections you should call {@link
   * #unPublishPort()}.
   *
   * @return true if the operation was successful, false if the node
   * was already registered.
   *
   * @exception java.io.IOException if the port mapper could not be contacted.
   **/
  public boolean publishPort() 
    throws IOException {
    if (epmd != null) return false; // already published
    
    OtpEpmd.publishPort(this);
    return (epmd != null);
  }


  /**
   * Unregister the server node's name and port number from the Erlang
   * port mapper, thus preventing any new connections from remote
   * nodes.
   **/
  public void unPublishPort() {
    // unregister with epmd
    OtpEpmd.unPublishPort(this); 
    
    // close the local descriptor (if we have one)
    try {
      if (epmd != null) epmd.close();
    }
    catch (IOException e) {/* ignore close errors */}
    epmd = null;
  }

  /**
   * Accept an incoming connection from a remote node. A call to this
   * method will block until an incoming connection is at least
   * attempted.
   *
   * @return a connection to a remote node.
   *
   * @exception java.io.IOException if a remote node attempted to
   * connect but no common protocol was found.
   *
   * @exception OtpAuthException if a remote node attempted to
   * connect, but was not authorized to connect.
   **/
  public OtpConnection accept()
    throws IOException, OtpAuthException 
  {
    Socket newsock=null;
    
    while (true) {
      try {
	newsock = sock.accept();
	return new OtpConnection(this,newsock);
      }
      catch (IOException e) {
	try {
	  if (newsock != null) newsock.close();
	}
	catch (IOException f) {/* ignore close errors */}
	throw e;
      }
    }
  }
}
