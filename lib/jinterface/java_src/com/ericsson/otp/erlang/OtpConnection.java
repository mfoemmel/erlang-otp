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
import java.io.InputStream;
import java.net.Socket;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Random;

/**
 * Maintains a connection between a Java process and a remote Erlang,
 * Java or C node. The object maintains connection state and allows
 * data to be sent to and received from the peer.
 *
 * <p> Once a connection is established between the local node and a
 * remote node, the connection object can be used to send and receive
 * messages between the nodes.
 *
 * <p> The various receive methods are all blocking and will return
 * only when a valid message has been received or an exception is
 * raised.
 *
 * <p> If an exception occurs in any of the methods in this class, the
 * connection will be closed and must be reopened in order to resume
 * communication with the peer.
 * 
 * <p> If the remote node is an Erlang node, it is important to not
 * let the connection fall idle for more than a few seconds at a time.
 * Erlang monitors connection activity and when no messages have been
 * sent for a short time (normally 15 seconds), it sends a 'tick'
 * message to which it expects a response within a short time. If the
 * response does not arrive in time, the node is assumed to have died
 * and the connection will be closed by Erlang. In order to prevent
 * this, make sure that one of the receive methods is called on a
 * regular basis. These methods will automatically respond to 'tick'
 * messages, returning only when a proper Erlang message has been
 * received.
 *
 * <p> For this reason, if you are doing other work while connected to
 * an Erlang node or have long message processing times, it is
 * advisable to let a separate Thread continuously maintain
 * the connection.
 *
 * <p> It is not possible to create an instance of this class
 * directly. OtpConnection objects are returned by {@link
 * OtpSelf#connect(OtpPeer) OtpSelf.connect()} and {@link
 * OtpServer#accept() OtpServer.accept()}.
 *  
 * <p> Communication can be traced by setting the Java property
 * OtpConnection.trace to some non-zero value before the class is
 * loaded. Setting 0 turns off tracing, and more information is shown
 * at each level up to 3. Currrently it is not possible to trace
 * individual connections, as the trace level is set only once when
 * the class is loaded, and affects all connections.
 **/
public class OtpConnection {
  private static final int headerLen = 2048; // more than enough
  
  private static final byte passThrough = (byte) 0x70;
  private static final byte version =     (byte) 0x83;

  // Erlang message header tags
  private static final int linkTag =        1;
  private static final int sendTag =        2;
  private static final int exitTag =        3;
  private static final int unlinkTag =      4;
  private static final int nodeLinkTag =    5;
  private static final int regSendTag =     6;
  private static final int groupLeaderTag = 7;
  private static final int exit2Tag =       8;

  private static final int sendTTTag =     12;
  private static final int exitTTTag =     13;
  private static final int regSendTTTag =  16;
  private static final int exit2TTTag =    18;

  // MD5 challenge messsage tags
  private static final int ChallengeReply = 'r';
  private static final int ChallengeAck = 'a';
  private static final int ChallengeStatus = 's';
  
  private boolean connected = false; // connection status
  private Socket socket;             // communication channel
  private OtpPeer peer;              // who are we connected to
  private OtpSelf self;              // this nodes id
  private boolean cookieOk = false;  // already checked the cookie for this connection
  private boolean sendCookie = true; // Send cookies in messages?

  // tracelevel constants
  // TODO: make current level PER CONNECTION instead
  private static int traceLevel = 0;
  private static int sendThreshold = 1;
  private static int ctrlThreshold = 2;
  private static int handshakeThreshold = 3;

  private static Random random = null;

  static {
    // trace this connection?
    String trace = System.getProperties().getProperty("OtpConnection.trace");
    try {
      if (trace != null) traceLevel = Integer.valueOf(trace).intValue();
    }
    catch (NumberFormatException e) {
      traceLevel=0;
    }
    random = new Random();
  }
    
  // package scope
  /*
   * Accept an incoming connection from a remote node. Used by {@link
   * OtpServer#accept() OtpServer.accept()} to create a connection
   * based on data received when handshaking with the peer node, when
   * the remote node is the connection intitiator.
   *
   * @exception java.io.IOException if it was not possible to connect to the peer.
   *
   * @exception OtpAuthException if handshake resultet in an authentication error
   */
  OtpConnection(OtpServer self, Socket s)
    throws IOException, OtpAuthException {
    this.self = self;
    this.peer = new OtpPeer(); 
    this.socket = s;

    this.socket.setTcpNoDelay(true);
    
    if (traceLevel >= handshakeThreshold)
      System.out.println("<- ACCEPT FROM " + s.getInetAddress() + ":" + s.getPort());
    
    // get his info
    recvName(this.peer);

    // now find highest common dist value
    if ((peer.proto != self.proto) ||
	(self.distHigh < peer.distLow) ||
	(self.distLow > peer.distHigh)) {
      this.close();
      throw new IOException("No common protocol found - cannot accept connection");
    }
    // highest common version: min(peer.distHigh, self.distHigh)
    peer.distChoose = (peer.distHigh > self.distHigh? self.distHigh : peer.distHigh);
      
    doAccept();
  }

  // package scope
  /*
   * Intiate and open a connection to a remote node.
   *
   * @exception java.io.IOException if it was not possible to connect to the peer.
   *
   * @exception OtpAuthException if handshake resultet in an authentication error.
   */
  OtpConnection(OtpSelf self, OtpPeer other)
    throws IOException , OtpAuthException {
    this.peer = other;
    this.self = self;
    this.socket = null;
    int port;

    // now get a connection between the two...
    port = OtpEpmd.lookupPort(peer);

    // now find highest common dist value
    if ((peer.proto != self.proto) ||
	(self.distHigh < peer.distLow) ||
	(self.distLow > peer.distHigh)) {
      throw new IOException("No common protocol found - cannot connect");
    }
    
    // highest common version: min(peer.distHigh, self.distHigh)
    peer.distChoose = (peer.distHigh > self.distHigh? self.distHigh : peer.distHigh);

    doConnect(port);

    this.connected = true;
  }

  /**
   * Send a message to a named process on a remote node.
   *
   * @param dest the name of the remote process.
   * @param msg the message to send.
   *
   * @exception java.io.IOException if the connection is not active or
   * a communication error occurs.
   *
   * @exception OtpErlangDataException if there are
   * errors encoding the message.
   **/
  public void send(String dest, OtpErlangObject msg)
    throws IOException, OtpErlangDataException {
    // encode and send the message
    sendBuf(dest,new OtpOutputStream(msg));
  }

  /**
   * Send a pre-encoded message to a named process on a remote node.
   *
   * @param dest the name of the remote process.
   * @param payload the encoded message to send.
   *
   * @exception java.io.IOException if the connection is not active or
   * a communication error occurs.
   *
   * @exception OtpErlangDataException if there are
   * errors encoding the message.
   **/
  public void sendBuf(String dest, OtpOutputStream payload) 
    throws IOException, OtpErlangDataException {
    if (! this.connected) {
      throw new IOException("Not connected");
    }
    OtpOutputStream header = new OtpOutputStream(headerLen);
    
    // preamble: 4 byte length + "passthrough" tag + version
    header.write4BE(0); // reserve space for length
    header.write1(passThrough);
    header.write1(version);

    // header info
    header.write_tuple_head(4);
    header.write_long(regSendTag);
    header.write_any(this.self.pid());
    if (sendCookie)
      header.write_atom(this.self.cookie());
    else
      header.write_atom("");
    header.write_atom(dest);

    // version for payload
    header.write1(version);

    // fix up length in preamble
    header.poke4BE(0,header.count() + payload.count() - 4);
    
    do_send(header,payload);
  }
  
  /**
   * Send a message to a process on a remote node.
   *
   * @param dest the Erlang PID of the remote process.
   * @param msg the message to send.
   
   * @exception java.io.IOException if the connection is not active
   * or a communication error occurs.
   *
   * @exception OtpErlangDataException if there are
   * errors encoding the message.
   **/
  public void send(OtpErlangPid dest, OtpErlangObject msg)
    throws IOException, OtpErlangDataException {
    // encode and send the message
    sendBuf(dest,new OtpOutputStream(msg));
  }

  /**
   * Send a pre-encoded message to a process on a remote node.
   *
   * @param dest the Erlang PID of the remote process.
   * @param msg the encoded message to send.
   *
   * @exception java.io.IOException if the connection is not active
   * or a communication error occurs.
   *
   * @exception OtpErlangDataException if there are
   * errors encoding the message.
   **/
  public void sendBuf(OtpErlangPid dest, OtpOutputStream payload) 
    throws IOException, OtpErlangDataException {
    if (! this.connected) {
      throw new IOException("Not connected");
    }
    OtpOutputStream header = new OtpOutputStream(headerLen);
    
    // preamble: 4 byte length + "passthrough" tag + version
    header.write4BE(0); // reserve space for length
    header.write1(passThrough);
    header.write1(version);

    // header info
    header.write_tuple_head(3);
    header.write_long(sendTag);
    if (sendCookie)
      header.write_atom(this.self.cookie());
    else
      header.write_atom("");
    header.write_any(dest);

    // version for payload
    header.write1(version);

    // fix up length in preamble
    header.poke4BE(0,header.count() + payload.count() - 4);
    
    do_send(header,payload);
  }




  /**
   * Send an RPC request to the remote Erlang node. This convenience
   * function creates the following message and sends it to 'rex' on
   * the remote node:
   * 
   * <pre>
   * { self, { call, Mod, Fun, Args, user }}
   * </pre>
   *
   * @param mod the name of the Erlang module containing the function to be called.
   * @param fun the name of the function to call.
   * @param args an array of Erlang terms, to be used as arguments to the function.
   *
   * @exception java.io.IOException if the connection is not active
   * or a communication error occurs.
   *
   * @exception OtpErlangDataException if there are
   * errors encoding the message.
   **/
  public void sendRPC(String mod, String fun, OtpErlangObject[] args) 
    throws IOException, OtpErlangDataException {
    sendRPC(mod,fun,new OtpErlangList(args));
  }

  /**
   * Send an RPC request to the remote Erlang node. This convenience
   * function creates the following message and sends it to 'rex' on
   * the remote node:
   * 
   * <pre>
   * { self, { call, Mod, Fun, Args, user }}
   * </pre>
   *
   * @param mod the name of the Erlang module containing the function to be called.
   * @param fun the name of the function to call.
   * @param args a list of Erlang terms, to be used as arguments to the function.
   *
   * @exception java.io.IOException if the connection is not active
   * or a communication error occurs.
   *
   * @exception OtpErlangDataException if there are
   * errors encoding the message.
   **/
  public void sendRPC(String mod, String fun, OtpErlangList args) 
    throws IOException, OtpErlangDataException {
    OtpErlangObject[] rpc = new OtpErlangObject[2];
    OtpErlangObject[] call = new OtpErlangObject[5];
    
    /* {self, { call, Mod, Fun, Args, user}} */

    call[0] = new OtpErlangAtom("call");
    call[1] = new OtpErlangAtom(mod);
    call[2] = new OtpErlangAtom(fun);
    call[3] = args;
    call[4] = new OtpErlangAtom("user");
        
    rpc[0] = self.pid();
    rpc[1] = new OtpErlangTuple(call);

    send("rex", new OtpErlangTuple(rpc));
  }

  /**
   * Receive an RPC reply from the remote Erlang node. This
   * convenience function receives a message from the remote node, and
   * expects it to have the following format:
   * 
   * <pre>
   * {rex, Term}
   * </pre>
   *
   * @return the second element of the tuple if the received message
   * is a two-tuple, otherwise null. No further error checking is
   * performed.
   *
   * @exception java.io.IOException if the connection is not active or
   * a communication error occurs.
   *
   * @exception OtpErlangExit if an exit signal is
   * received from a process on the peer node.
   *
   * @exception OtpAuthException if the remote node
   * sends a message containing an invalid cookie.
   **/
  public OtpErlangObject receiveRPC() 
    throws IOException, OtpErlangExit, OtpAuthException {

    OtpErlangObject msg = receive();

    if (msg instanceof OtpErlangTuple) {
      OtpErlangTuple t = (OtpErlangTuple)msg;
      if (t.arity() == 2) return t.elementAt(1); // obs: second element
    }

    return null;
  }
  
  /* Send an auth error to peer because he sent a bad cookie.
   * The auth error uses his cookie (not revealing ours).
   * This is just like send_reg otherwise
   */
  private void cookieError(OtpErlangAtom cookie)
    throws OtpAuthException {
    try {
      OtpOutputStream header = new OtpOutputStream(headerLen);

      // preamble: 4 byte length + "passthrough" tag + version
      header.write4BE(0); // reserve space for length
      header.write1(passThrough);
      header.write1(version);

      header.write_tuple_head(4);
      header.write_long(regSendTag);
      header.write_any(this.self.pid());
      header.write_atom(cookie.atomValue()); // important: his cookie, not mine...
      header.write_atom("auth");
      
      // version for payload
      header.write1(version);

      // the payload
      
      // the no_auth message (copied from Erlang) Don't change this (Erlang will crash)
      // {$gen_cast, {print, "~n** Unauthorized cookie ~w **~n", [foo@aule]}}
      OtpErlangObject[] msg = new OtpErlangObject[2];
      OtpErlangObject[] msgbody = new OtpErlangObject[3];

      msgbody[0] = new OtpErlangAtom("print");
      msgbody[1] = new OtpErlangString("~n** Bad cookie sent to " + this.self + " **~n");
      // Erlang will crash and burn if there is no third argument here...
      msgbody[2] = new OtpErlangList(); // empty list
      
      msg[0] = new OtpErlangAtom("$gen_cast");
      msg[1] = new OtpErlangTuple(msgbody);

      OtpOutputStream payload = new OtpOutputStream(new OtpErlangTuple(msg));
      
      // fix up length in preamble
      header.poke4BE(0,header.count() + payload.count() - 4);
    
      try {
	do_send(header,payload);
      }
      catch (IOException e) {} // ignore
    }
    finally {
      this.close();
      throw new OtpAuthException("Remote cookie not authorized: " + cookie.atomValue());
    }
  }

  // link to pid

  /**
   * Create a link between the local node and the specified process on
   * the remote node. If the link is still active when the remote
   * process terminates, an exit signal will be sent to this
   * connection. Use {@link #unlink unlink()} to remove the link.
   *
   * @param dest the Erlang PID of the remote process.
   *
   * @exception java.io.IOException if the connection is not active
   * or a communication error occurs.
   *
   * @exception OtpErlangDataException if there are
   * errors encoding the link message.
   **/
  public void link(OtpErlangPid dest) 
    throws IOException, OtpErlangDataException {
    if (! this.connected) {
      throw new IOException("Not connected");
    }
    OtpOutputStream header = new OtpOutputStream(headerLen);

    // preamble: 4 byte length + "passthrough" tag
    header.write4BE(0); // reserve space for length
    header.write1(passThrough);
    header.write1(version);

    // header
    header.write_tuple_head(3);
    header.write_long(linkTag);
    header.write_any(this.self.pid());
    header.write_any(dest);

    // fix up length in preamble
    header.poke4BE(0,header.count()-4);

    do_send(header);
  }
  
  /**
   * Remove a link between the local node and the specified process on
   * the remote node. This method deactivates links created with
   * {@link #link link()}.
   *
   * @param dest the Erlang PID of the remote process.
   *
   * @exception java.io.IOException if the connection is not active or
   * a communication error occurs.
   *
   * @exception OtpErlangDataException if there are
   * errors encoding the unlink message.
   **/
  public void unlink(OtpErlangPid dest) 
    throws IOException, OtpErlangDataException {
    if (! this.connected) {
      throw new IOException("Not connected");
    }
    OtpOutputStream header = new OtpOutputStream(headerLen);

    // preamble: 4 byte length + "passthrough" tag
    header.write4BE(0); // reserve space for length
    header.write1(passThrough);
    header.write1(version);

    // header
    header.write_tuple_head(3);
    header.write_long(unlinkTag);
    header.write_any(this.self.pid());
    header.write_any(dest);

    // fix up length in preamble
    header.poke4BE(0,header.count()-4);

    do_send(header);
  }

  /**
   * Send an exit signal to a remote process.
   * 
   * @param dest the Erlang PID of the remote process.
   * @param reason a string describing the exit reason.
   * 
   * @exception java.io.IOException if the connection is not active or
   * a communication error occurs.
   *
   * @exception OtpErlangDataException if there are
   * errors encoding the exit message.
   **/
  public void exit(OtpErlangPid dest, String reason) 
    throws IOException, OtpErlangDataException {
    if (! this.connected) {
      throw new IOException("Not connected");
    }
    OtpOutputStream header = new OtpOutputStream(headerLen);

    // preamble: 4 byte length + "passthrough" tag
    header.write4BE(0); // reserve space for length
    header.write1(passThrough);
    header.write1(version);

    // header
    header.write_tuple_head(4);
    header.write_long(exit2Tag);
    header.write_any(this.self.pid());
    header.write_any(dest);
    header.write_string(reason);

    // fix up length in preamble
    header.poke4BE(0,header.count()-4);

    do_send(header);
  }

  /**
   * Receive a message from a remote process. This method blocks
   * until a valid message is received or an exception is raised.
   *
   * <p> If the remote node sends a message that cannot be decoded
   * properly, the connection is closed and the method throws an
   * exception.
   *
   * @return an object containing a single Erlang term.
   *
   * @exception java.io.IOException if the connection is not active or
   * a communication error occurs.
   *
   * @exception OtpErlangExit if an exit signal is
   * received from a process on the peer node.
   *
   * @exception OtpAuthException if the remote node
   * sends a message containing an invalid cookie.
   **/
  public OtpErlangObject receive() 
    throws IOException, OtpErlangExit, OtpAuthException {
    OtpInputStream ibuf = receiveBuf();
    OtpErlangObject o = null;

    try {
      o = ibuf.read_any();
    }
    catch (OtpErlangDecodeException e) {
      this.close();
      throw new IOException(e.getMessage());
    }
    
    return o;
  }

  /**
   * Receive a raw (still encoded) message from a remote process.
   * This message blocks until a valid message is received or an
   * exception is raised.
   *
   * <p> If the remote node sends a message that cannot be decoded
   * properly, the connection is closed and the method throws an
   * exception.
   *
   * @return an object containing a raw (still encoded) Erlang term.
   *
   * @exception java.io.IOException if the connection is not active or
   * a communication error occurs.
   *
   * @exception OtpErlangExit if an exit signal is
   * received from a process on the peer node.
   *
   * @exception OtpAuthException if the remote node
   * sends a message containing an invalid cookie.
   **/
  public OtpInputStream receiveBuf()
    throws IOException, OtpErlangExit, OtpAuthException {
    if (! this.connected) {
      throw new IOException("Not connected");
    }
    int i;
    byte[] lbuf = new byte[4];
    OtpInputStream ibuf;
    OtpErlangObject traceobj;
    int len;
    byte[] tock={0,0,0,0};
    
    try {
    receive_loop:
      // don't return until we get a real message
      // or a failure of some kind (e.g. EXIT)
      while (true) {
	synchronized (this) {
	  // read length and read buffer must be atomic!
	tick_loop:
	  do {
	    // read 4 bytes - get length of incoming packet
	    // socket.getInputStream().read(lbuf);
	    // should check that we actually got 4 here
	    if ((i = ReadSock(socket,lbuf)) != 4) {
	      throw new IOException("Short read, got " + i + " bytes, expected 4");
	    }
	    ibuf = new OtpInputStream(lbuf);
	    len = ibuf.read4BE();

	    //  received tick? send tock!
	    if (len == 0) socket.getOutputStream().write(tock);
	  } while (len == 0);

	  // got a real message (maybe) - read len bytes
	  byte[] tmpbuf = new byte[len];
	  // i = socket.getInputStream().read(tmpbuf);
	  if ((i = ReadSock(socket,tmpbuf)) != len) {
	    throw new IOException("Short read, got " + i + " bytes, expected " + len);
	  }
	  ibuf = new OtpInputStream(tmpbuf);
	}
	
	if (ibuf.read1() != passThrough) break receive_loop;

	// got a real message (really)
	OtpErlangAtom reason = null;
	OtpErlangAtom cookie = null;
	OtpErlangObject tmp = null;
	OtpErlangTuple head = null;
	int tag;

	// decode the header
	tmp = ibuf.read_any();
	if (!(tmp instanceof OtpErlangTuple)) break receive_loop;
	
	head = (OtpErlangTuple)tmp;
	if (!(head.elementAt(0) instanceof OtpErlangLong)) break receive_loop;

	// lets see what kind of message this is
	tag = (int)((OtpErlangLong)(head.elementAt(0))).longValue();
	switch (tag) {
	case sendTag: 	      // { SEND, Cookie, ToPid }
	case sendTTTag:	      // { SEND, Cookie, ToPid, TraceToken }
	  if (!this.cookieOk) {
	    // we only do this once, he can send us bad cookies later if he likes
	    if (!(head.elementAt(1) instanceof OtpErlangAtom)) break receive_loop;
	    cookie = (OtpErlangAtom)head.elementAt(1);
	    if (sendCookie) {
	      if (!cookie.atomValue().equals(this.self.cookie())) {
		cookieError(cookie);
	      }
	    }
	    else {
	      if (!cookie.atomValue().equals("")) {
		cookieError(cookie);
	      }
	    }	      
	    this.cookieOk = true;
	  }

	  if (this.traceLevel >= sendThreshold) {
	    System.out.println("<- " + headerType(head) + " " + head);

	    /* show received payload too */
	    ibuf.mark(0);
	    try {
	      traceobj = ibuf.read_any();
	    }
	    catch (OtpErlangDecodeException e) {
	      this.close();
	      throw new IOException(e.getMessage());
	    }

	    if (traceobj != null) System.out.println("   " + traceobj);
	    else System.out.println("   (null)");
	    ibuf.reset();
	  }
	  
	  return ibuf;
	  
	case regSendTag:      // { REG_SEND, FromPid, Cookie, ToName }
	case regSendTTTag:    // { REG_SEND, FromPid, Cookie, ToName, TraceToken }
	  if (!this.cookieOk) {
	    // we only do this once, he can send us bad cookies later if he likes
	    if (!(head.elementAt(2) instanceof OtpErlangAtom)) break receive_loop;
	    cookie = (OtpErlangAtom)head.elementAt(2);
	    if (sendCookie) {
	      if (!cookie.atomValue().equals(this.self.cookie())) {
		cookieError(cookie);
	      }
	    }
	    else {
	      if (!cookie.atomValue().equals("")) {
		cookieError(cookie);
	      }
	    }
	    this.cookieOk = true;
	  }

	  if (this.traceLevel >= sendThreshold) {
	    System.out.println("<- " + headerType(head) + " " + head);

	    /* show received payload too */
	    ibuf.mark(0);
	    try {
	      traceobj = ibuf.read_any();
	    }
	    catch (OtpErlangDecodeException e) {
	      this.close();
	      throw new IOException(e.getMessage());
	    }

	    if (traceobj != null) System.out.println("   " + traceobj);
	    else System.out.println("   (null)");
	    ibuf.reset();
	  }

	  return ibuf;

	case exitTag:         // { EXIT, FromPid, ToPid, Reason }
	case exit2Tag:        // { EXIT2, FromPid, ToPid, Reason }
	  if (!(head.elementAt(3) instanceof OtpErlangAtom)) break receive_loop;
	  if (this.traceLevel >= ctrlThreshold) {
	    System.out.println("<- " + headerType(head) + " " + head);
	  }
	  reason = (OtpErlangAtom)head.elementAt(3);
	  throw new OtpErlangExit(reason.atomValue());

	case exitTTTag:	      // { EXIT, FromPid, ToPid, TraceToken, Reason }
	case exit2TTTag:      // { EXIT2, FromPid, ToPid, TraceToken, Reason }
	  // as above, but bifferent element number
	  if (!(head.elementAt(4) instanceof OtpErlangAtom)) break receive_loop;
	  if (this.traceLevel >= ctrlThreshold) {
	    System.out.println("<- " + headerType(head) + " " + head);
	  }
	  reason = (OtpErlangAtom)head.elementAt(4);
	  throw new OtpErlangExit(reason.atomValue());

	  // these can be (safely?) ignored
	  // (don't have a clue what to do with them anyway)
	case linkTag:         // { LINK, FromPid, ToPid}
	case unlinkTag:       // { UNLINK, FromPid, ToPid}
	case nodeLinkTag:     // { NODELINK }
	case groupLeaderTag:  // { GROUPLEADER, FromPid, ToPid}
	  if (this.traceLevel >= ctrlThreshold) {
	    System.out.println("<- " + headerType(head) + " " + head);
	  }
	  continue receive_loop;

	  // garbage?
	default:
	  break receive_loop;
	}
      } // end receive_loop
      
      // this section reachable only with break
      // we have received garbage from peer 
      this.close();
      throw new OtpErlangExit("Remote is sending garbage");
    }
    catch (OtpAuthException e) {
      this.close();
      throw e;
    }
    catch (OtpErlangDecodeException e) {
      this.close();
      throw new IOException(e.getMessage());
    }
    catch (IOException e) {
      this.close();
      throw new OtpErlangExit("Remote has closed connection");
    }
  }

  /**
   * Close the connection to the remote node. 
   **/
  public synchronized void close() {
    this.connected = false;
    try {
      if (this.socket != null) this.socket.close();
    }
    catch (IOException e) { /* ignore socket close errors */ }
    this.socket = null;
  }

  /**
   * Determine if the connection is still alive.
   *
   * @return true if the connection is alive.
   **/
  public boolean isConnected() {
    return this.connected;
  }
  
  /**
   * Get information about the node at the peer end of this
   * connection.
   * 
   * @return the {@link OtpNode} representing the peer node.
   **/
  public OtpPeer peer() {
    return peer;
  }

  /**
   * Get information about the node at the local end of this
   * connection.
   * 
   * @return the {@link OtpNode} representing the local node.
   **/
  public OtpSelf self() {
    return self;
  }

  // used by  send and send_reg (message types with payload)
  private synchronized void do_send(OtpOutputStream header, OtpOutputStream payload) 
    throws IOException {
    try {
      header.writeTo(socket.getOutputStream());
      payload.writeTo(socket.getOutputStream());

      if (this.traceLevel >= sendThreshold) {
	// Need to decode header and output buffer to show trace message!
	// First make OtpInputStream, then decode.
	try {
	  OtpErlangObject h = (header.getOtpInputStream(5)).read_any();
	  System.out.println("-> " + headerType(h) + " " + h);
	
	  OtpErlangObject o = (payload.getOtpInputStream(0)).read_any();
	  System.out.println("   " + o);
	  o = null;
	}
	catch (OtpErlangDecodeException e) {
	  System.out.println("   " + "can't decode output buffer:" + e);
	}
      }
    }
    catch (IOException e) {
      this.close();
      throw e;
    }
  }

  // used by the other message types
  private synchronized void do_send(OtpOutputStream header)
    throws IOException {
    try {
      header.writeTo(socket.getOutputStream());

      if (this.traceLevel >= ctrlThreshold) {
	try {
	  OtpErlangObject h = (header.getOtpInputStream(5)).read_any();
	  System.out.println("-> " + headerType(h) + " " + h);
	}
	catch (OtpErlangDecodeException e) {
	  System.out.println("   " + "can't decode output buffer: " + e);
	}
      }
    }
    catch (IOException e) {
      this.close();
      throw e;
    }
  }

  private String headerType(OtpErlangObject h) {
    int tag = -1;
    
    if (h instanceof OtpErlangTuple) {
      tag = (int)(((OtpErlangLong)(((OtpErlangTuple)h).elementAt(0))).longValue());
    }

    switch (tag) {
    case linkTag:
      return "LINK";

    case sendTag:
      return "SEND";

    case exitTag:
      return "EXIT";

    case unlinkTag:
      return "UNLINK";

    case nodeLinkTag:
      return "NODELINK";

    case regSendTag:
      return "REG_SEND";

    case groupLeaderTag:
      return "GROUP_LEADER";

    case exit2Tag:
      return "EXIT2";

    case sendTTTag:
      return "SEND_TT";

    case exitTTTag:
      return "EXIT_TT";

    case regSendTTTag:
      return "REG_SEND_TT";
      
    case exit2TTTag:
      return "EXIT2_TT";
    }

    return "(unknown type)";
  }
  
  static private int ReadSock(Socket s, byte[] b)
    throws IOException {
    int got = 0;
    int len = b.length;
    int i;
    InputStream is = s.getInputStream();

    while (got < len) {
      i = is.read(b,got,len-got);

      if (i < 0) return i;
      else got += i;
    }
    return got;
  }


  private void doAccept()
    throws IOException, OtpAuthException  {
    try {
      sendStatus("ok");
      int our_challenge = genChallenge(); 
      sendChallenge(peer.distChoose, self.flags, our_challenge);
      int her_challenge = recvChallengeReply(our_challenge);
      String our_digest = genDigest(her_challenge, self.cookie());
      sendChallengeAck(our_digest);
      this.connected = true;
      cookieOk = true;
      sendCookie = false;
    }
    catch (IOException ie) {
      this.close();
      throw ie;
    }
    catch (OtpAuthException ae) {
      this.close();
      throw ae;
    }
    catch (Exception e) {
      String nn = peer.node();
      this.close();
      throw new IOException("Error accepting connection from " + nn);
    }
    if (traceLevel >= handshakeThreshold)
      System.out.println("<- MD5 ACCEPTED " + 
			 peer.host());
  }

  private void doConnect(int port) 
    throws IOException, OtpAuthException {
    try {
      this.socket = new Socket(peer.host(),port);
      this.socket.setTcpNoDelay(true);
      
      if (traceLevel >= handshakeThreshold)
	System.out.println("-> MD5 CONNECT TO " + 
			   peer.host() + ":" + port);
      sendName(peer.distChoose, self.flags);
      recvStatus();
      int her_challenge = recvChallenge();
      String our_digest = genDigest(her_challenge, self.cookie());
      int our_challenge = genChallenge();
      sendChallengeReply(our_challenge, our_digest);
      recvChallengeAck(our_challenge);
      cookieOk = true;
      sendCookie = false;
    }
    catch (OtpAuthException ae) {
      this.close();
      throw ae;
    }
    catch (Exception e) {
      this.close();
      throw new IOException("Cannot connect to peer node");
    }
  }

  // This is nooo good as a challenge,
  // XXX fix me.
  static private int genChallenge() {
    return random.nextInt();
  }

  // Used to debug print a message digest
  static String hex0(byte x) {
    char tab[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
		   'a', 'b', 'c', 'd', 'e', 'f' };
    int uint;
    if (x < 0) {
      uint = x & 0x7F;
      uint |= (1 << 7);
    }
    else {
      uint = (int) x;
    }
    return "" + tab[uint >>> 4] + tab[uint & 0xF];
  }

  static String hex(String x) {
    StringBuffer sb = new StringBuffer();
    try {
      byte[] b = x.getBytes();
      int i;
      for(i = 0; i < b.length; ++i) 
	sb.append(hex0(b[i]));
    }
    catch (Exception e) {
      // Debug function, ignore errors.
    }
    return sb.toString();

  }

  private String genDigest(int challenge, String cookie) {
    int i;
    long ch2;

    if (challenge < 0) {
      ch2 = 1L << 31;
      ch2 |= (long) (challenge & 0x7FFFFFFF);
    }
    else {
      ch2 = (long) challenge;
    }
    OtpMD5 context = new OtpMD5();
    context.update(cookie);
    context.update("" + ch2);
    return context.final_string();
  }

  private void sendName(int dist, int flags) 
    throws IOException {
    
    OtpOutputStream obuf = new OtpOutputStream();
    String str = self.node();
    obuf.write2BE(str.length()+7); // 7 bytes + nodename
    obuf.write1(OtpNode.NTYPE_R6);
    obuf.write2BE(dist);
    obuf.write4BE(flags);
    obuf.write(str.getBytes());
    
    obuf.writeTo(socket.getOutputStream());

    if (this.traceLevel >= handshakeThreshold) {
      System.out.println("-> " + "HANDSHAKE sendName" +
			 " flags=" + flags +
			 " dist=" + dist +
			 " local=" + self);
    }
  }

  private void sendChallenge(int dist, int flags, int challenge) 
    throws IOException {
    
    OtpOutputStream obuf = new OtpOutputStream();
    String str = self.node();
    obuf.write2BE(str.length()+11); // 11 bytes + nodename
    obuf.write1(OtpNode.NTYPE_R6);
    obuf.write2BE(dist);
    obuf.write4BE(flags);
    obuf.write4BE(challenge);
    obuf.write(str.getBytes());
    
    obuf.writeTo(socket.getOutputStream());

    if (this.traceLevel >= handshakeThreshold) {
      System.out.println("-> " + "HANDSHAKE sendChallenge" +
			 " flags=" + flags +
			 " dist=" + dist +
			 " challenge=" + challenge +
			 " local=" + self);
    }
  }

  private byte[] read2BytePackage()
    throws IOException, OtpErlangDecodeException {

    byte[] lbuf = new byte[2];
    byte[] tmpbuf;

    synchronized (this) {
      int i;
      if ((i = ReadSock(socket,lbuf)) != 2) {
	throw new IOException("Short read, got " + i + 
			      " bytes, expected 2");
      }
      OtpInputStream ibuf = new OtpInputStream(lbuf);
      int len = ibuf.read2BE();
      tmpbuf = new byte[len];
      if ((i = ReadSock(socket,tmpbuf)) != len) {
	throw new IOException("Short read, got " + i + 
			      " bytes, expected " + len);
      }
    }
    return tmpbuf;
  }


  private void recvName(OtpPeer peer) 
    throws IOException {

    String hisname = "";

    try {
      byte[] tmpbuf = read2BytePackage();
      OtpInputStream ibuf = new OtpInputStream(tmpbuf);
      byte[] tmpname;
      int len = tmpbuf.length;
      peer.ntype = ibuf.read1();
      if (peer.ntype != OtpNode.NTYPE_R6) {
	throw new IOException("Unknown remote node type");
      }	
      peer.distLow = peer.distHigh = ibuf.read2BE();
      if (peer.distLow < 5) {
	throw new IOException("Unknown remote node type");
      }
      peer.flags = ibuf.read4BE();
      tmpname = new byte[len-7];
      ibuf.readN(tmpname);
      hisname = new String(tmpname);
      // Set the old nodetype parameter to indicate hidden/normal status
      // When the old handshake is removed, the ntype should also be.
      if ((peer.flags & OtpNode.dFlagPublished) != 0)
	peer.ntype = OtpNode.NTYPE_R4_ERLANG;
      else
	peer.ntype = OtpNode.NTYPE_R4_HIDDEN;
    }
    catch (OtpErlangDecodeException e) {
      throw new IOException("Handshake failed - not enough data");
    }


    int i = hisname.indexOf('@',0);
    peer.node = hisname;
    peer.alive = hisname.substring(0,i);
    peer.host = hisname.substring(i+1,hisname.length());


    if (this.traceLevel >= handshakeThreshold) {
      System.out.println("<- " + "HANDSHAKE" +
			 " ntype=" + peer.ntype +
			 " dist=" + peer.distHigh +
			 " remote=" + peer);
    }
  }

  private int recvChallenge() 
    throws IOException {

    int challenge;

    try {
      byte[] buf = read2BytePackage();
      OtpInputStream ibuf = new OtpInputStream(buf);
      peer.ntype = ibuf.read1();
      if (peer.ntype != OtpNode.NTYPE_R6) {
	throw new IOException("Unexpected peer type");
      }
      peer.distLow = peer.distHigh = ibuf.read2BE();
      peer.flags = ibuf.read4BE();
      challenge = ibuf.read4BE();
      byte[] tmpname = new byte[buf.length - 11];
      ibuf.readN(tmpname);
      String hisname = new String(tmpname);
      int i = hisname.indexOf('@',0);
      peer.node = hisname;
      peer.alive = hisname.substring(0,i);
      peer.host = hisname.substring(i+1,hisname.length());
    }
    catch (OtpErlangDecodeException e) {
      throw new IOException("Handshake failed - not enough data");
    }

    if (this.traceLevel >= handshakeThreshold) {
      System.out.println("<- " + "HANDSHAKE recvChallenge" +
			 " from=" + peer.node +
			 " challenge=" + challenge +
			 " local=" + self);
    }
    
    return challenge;
  }

  private void sendChallengeReply(int challenge, String digest)
    throws IOException {

    OtpOutputStream obuf = new OtpOutputStream();
    obuf.write2BE(21);
    obuf.write1(ChallengeReply);
    obuf.write4BE(challenge);
    obuf.write(digest.getBytes());
    obuf.writeTo(socket.getOutputStream());

    if (this.traceLevel >= handshakeThreshold) {
      System.out.println("-> " + "HANDSHAKE sendChallengeReply" +
			 " challenge=" + challenge +
			 " digest=" + hex(digest) +
			 " local=" + self);
    }
  }
    
  private int recvChallengeReply(int our_challenge)
    throws IOException, OtpAuthException {

    int challenge;
    String her_digest;

    try {
      byte[] buf = read2BytePackage();
      OtpInputStream ibuf = new OtpInputStream(buf);
      int tag = ibuf.read1();
      if (tag != ChallengeReply) {
	throw new IOException("Handshake protocol error");
      }
      challenge = ibuf.read4BE();
      byte[] tmpbuf = new byte[16];
      ibuf.readN(tmpbuf);
      her_digest = new String(tmpbuf);
      String our_digest = genDigest(our_challenge, self.cookie());
      if (her_digest.compareTo(our_digest) != 0) {
	throw new OtpAuthException("Peer authentication error.");
      }
    }
    catch (OtpErlangDecodeException e) {
      throw new IOException("Handshake failed - not enough data");
    }

    if (this.traceLevel >= handshakeThreshold) {
      System.out.println("<- " + "HANDSHAKE recvChallengeReply" +
			 " from=" + peer.node +
			 " challenge=" + challenge +
			 " digest=" + hex(her_digest) +
			 " local=" + self);
    }

    return challenge;
  }
    
  private void sendChallengeAck(String digest)
    throws IOException {

    OtpOutputStream obuf = new OtpOutputStream();
    obuf.write2BE(17); 
    obuf.write1(ChallengeAck);
    obuf.write(digest.getBytes());
    
    obuf.writeTo(socket.getOutputStream());

    if (this.traceLevel >= handshakeThreshold) {
      System.out.println("-> " + "HANDSHAKE sendChallengeAck" +
			 " digest=" + hex(digest) +
			 " local=" + self);
    }
  }
    
  private void recvChallengeAck(int our_challenge)
    throws IOException, OtpAuthException {

    String her_digest;
    try {
      byte[] buf = read2BytePackage();
      OtpInputStream ibuf = new OtpInputStream(buf);
      int tag = ibuf.read1();
      if (tag != ChallengeAck) {
	throw new IOException("Handshake protocol error");
      }
      byte[] tmpbuf = new byte[16];
      ibuf.readN(tmpbuf);
      her_digest = new String(tmpbuf);
      String our_digest = genDigest(our_challenge, self.cookie());
      if (her_digest.compareTo(our_digest) != 0) {
	throw new OtpAuthException("Peer authentication error.");
      }
    }
    catch (OtpErlangDecodeException e) {
      throw new IOException("Handshake failed - not enough data");
    }
    catch (Exception e) {
      throw new OtpAuthException("Peer authentication error.");
    }

    if (this.traceLevel >= handshakeThreshold) {
      System.out.println("<- " + "HANDSHAKE recvChallengeAck" +
			 " from=" + peer.node +
			 " digest=" + hex(her_digest) +
			 " local=" + self);
    }
  }
    
  private void sendStatus(String status) 
    throws IOException {

    OtpOutputStream obuf = new OtpOutputStream();
    obuf.write2BE(status.length() + 1); 
    obuf.write1(ChallengeStatus);
    obuf.write(status.getBytes());
    
    obuf.writeTo(socket.getOutputStream());

    if (this.traceLevel >= handshakeThreshold) {
      System.out.println("-> " + "HANDSHAKE sendStatus" +
			 " status=" + status +
			 " local=" + self);
    }
  }

  private void recvStatus()
    throws IOException {

    try {
      byte[] buf = read2BytePackage();
      OtpInputStream ibuf = new OtpInputStream(buf);
      int tag = ibuf.read1();
      if (tag != ChallengeStatus) {
	throw new IOException("Handshake protocol error");
      }
      byte[] tmpbuf = new byte[buf.length - 1];
      ibuf.readN(tmpbuf);
      String status = new String(tmpbuf);

      if (status.compareTo("ok") != 0) {
	throw new IOException("Peer replied with status '" + status +
			      "' instead of 'ok'");
      }
    }
    catch (OtpErlangDecodeException e) {
      throw new IOException("Handshake failed - not enough data");
    } 
    if (this.traceLevel >= handshakeThreshold) {
      System.out.println("<- " + "HANDSHAKE recvStatus (ok)" +
			 " local=" + self);
    }
  }
}
