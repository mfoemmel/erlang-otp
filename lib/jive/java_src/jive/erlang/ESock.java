package jive.erlang;

import java.io.*;
import java.net.*;

/**
 * ESock is a class that performs the socket communication between
 * a Java client and the Erlang server.
 * 
 * <p>The class contains methods for performing: <tt>apply</tt>,
 * <tt>spawn</tt>, <tt>send</tt> on the server-side.
 *
 * @version     1.2, 8 Mar 1997
 * @author      Kaj Nygren (kaj@medialab.ericsson.se)
 * @author      Joakim Greben&ouml; (jocke@erix.ericsson.se)
 */
public class ESock {
  public static byte EAPPLY      = 50;
  public static byte ESPAWN      = 51;
  public static byte ESEND       = 52;
  public static byte ENEW_CLIENT = 53;
  
  private String host;
  private int port;
  
  /**
   * Creates an ESock object that communicates with a given host and port.
   */  
  public ESock(String host,int port) {
    this.host = host;
    this.port = port;
  }

  /**
   * Returns the port of the Jive server.
   */
  public int getPort() {
    return port;
  }

  /**
   * Returns the host of the Jive server.
   */
  public String getHost() {
    return host;
  }

  /**
   * Performs an Erlang apply.
   *
   * @exception JiveSecurityException is thrown if a security violation
   * occurred.
   * @exception JiveIOException is thrown if a communication error occurred.
   * @exception JiveException is thrown with an error text given by the server.
   * @return An EVar representing an Erlang variable. 
   */  
  public EVar apply(String module,String name, 
		    EList params) throws JiveSecurityException,JiveIOException,JiveException {
		      return apply(new EAtom(module),new EAtom(name),params);
  }

  /**
   * Performs an Erlang apply.
   * 
   * @exception JiveSecurityException is thrown if a security violation
   * occurred.
   * @exception JiveIOException is thrown if a communication error occurred.
   * @exception JiveException is thrown with an error text given by the server.
   * @return An EVar representing an Erlang variable. 
   */  
  public EVar apply(EAtom module,EAtom name, 
		    EList params) throws JiveSecurityException,JiveIOException,JiveException {
		      byte msg[] = new byte[5];
		      msg[4] = EAPPLY;
		      msg = EVar.concat(msg,module.pack());
		      msg = EVar.concat(msg,name.pack());
		      msg = EVar.concat(msg,params.pack());
		      EVar.intToBytes(msg.length-4,msg,0); // Prepend message length
		      return send_get_message(msg);
  }
  
  /** 
   * Spawns a new Erlang process.
   *
   * @exception JiveSecurityException is thrown if a security violation
   * occurred.
   * @exception JiveIOException is thrown if a communication error occurred.
   * @exception JiveException is thrown with an error text given by the server.
   * @return An EProcess representing the Erlang Pid.
   */
  public EProcess spawn(String module,String name, 
			EList params) throws JiveSecurityException,JiveIOException,JiveException {
			  return spawn(new EAtom(module),new EAtom(name),params);
  }

  /** 
   * Spawns a new Erlang process, result is a EProcess representing
   * the Erlang PID
   *
   * @exception JiveSecurityException is thrown if a security violation
   * occurred.
   * @exception JiveIOException is thrown if a communication error occurred.
   * @exception JiveException is thrown with an error text given by the server.
   * @return An EProcess representing the Erlang Pid.
   */
  public EProcess spawn(EAtom module,EAtom name, 
			EList params) throws JiveSecurityException,JiveIOException,JiveException {
			  byte msg[] = new byte[5];
			  msg[4] = ESPAWN;
			  msg = EVar.concat(msg,module.pack());
			  msg = EVar.concat(msg,name.pack());
			  msg = EVar.concat(msg,params.pack());
			  EVar.intToBytes(msg.length-4,msg,0); // Prepend message length
			  return (EProcess) send_get_message(msg);
  }

  /**
   * Sends a message asynchronously to an Erlang process.
   *
   * @exception JiveIOException is thrown if a communication error occurred.
   */
  public void send(EProcess pid,EVar data) throws JiveIOException {
    byte msg[] = new byte[5];
    msg[4] = ESEND;
    msg = EVar.concat(msg,pid.pack());
    msg = EVar.concat(msg,data.pack());
    EVar.intToBytes(msg.length-4,msg,0); // Prepend message length
    send_message(msg);
  }
  
  /**
   * Just for debugging!
   */
  public void print(byte message[]) {
    System.out.print("msg = ");
    for (int i=0;i<message.length;i++) {
      System.out.print(message[i]+", ");
    }
    System.out.println("");
  }

  /**
   * Asynchronous sending of messages.
   *
   * @exception JiveIOException is thrown if a communication error occurred.
   */
  private void send_message(byte message[]) throws JiveIOException {
    try {
      new Deliverer(message);
    } catch(Exception e) {
      //System.out.println("send_message: Send Error: "+e);
      throw new JiveIOException(e.getMessage());
    }
  }

  /**
   * Synchronous sending of messages.
   *
   * @exception JiveSecurityException is thrown if a security violation
   * occurred.
   * @exception JiveIOException is thrown if a communication error occurred.
   * @exception JiveException is thrown with an error text given by the server.
   * @return An EVar
   */
  private EVar send_get_message(byte message[]) throws JiveSecurityException,JiveIOException,JiveException {
    try {
      Socket s = new Socket(host,port);
      OutputStream output = s.getOutputStream();
      InputStream input = s.getInputStream();
      
      // Send message
      output.write(message);
      output.flush();
      
      // Get the result
      Unpacker unpacker = new Unpacker(input,null);
      EVar ret = unpacker.parseReply();      
      s.close();
      return ret;
    } catch(IOException e) {
      throw new JiveIOException(e.getMessage());
    }
  }
  
  /** 
   * This method opens a socket connection to the port on the server
   * and returns the socket.
   *
   * @exception JiveIOException is thrown if a communication error occurred.
   */
  protected Socket connect() throws JiveIOException {
    try {
      return new Socket(host,port);
    } catch (Exception e) {
      throw new JiveIOException(e.getMessage());
    }
  }

  /** 
   * This method is called when initiating the EReceiver. The method
   * opens a socket connection used by the EReceiver and initiates the
   * self pid.
   *
   * <p><b>Note:</b> This method should probably first init the self pid and
   * then open and return an array of socket connections to allow
   * for multiple back channel!
   *
   * @exception JiveIOException is thrown if a communication error occurred.
   */  
  protected synchronized void initReceiver(EReceiver receiver) throws JiveIOException {
    try {

      if (!receiver.connected()) {
	byte msg[] = new byte[5];
	msg[4] = ENEW_CLIENT;
	EVar.intToBytes(msg.length-4,msg,0); // Prepend message length

	Socket s = connect();
	OutputStream output = s.getOutputStream();
	InputStream input = s.getInputStream();
	
	// Send message
	output.write(msg);
	output.flush();
	
	// Get result
	Unpacker unpacker = new Unpacker(input,receiver);
	EProcess self = (EProcess) unpacker.parseReply();      
	receiver.init(s,unpacker,self);
      }
    } catch(Exception e) {
      throw new JiveIOException(e.getMessage());
    }
  }
}
