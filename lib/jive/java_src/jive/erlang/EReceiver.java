package jive.erlang;
import java.io.*;
import java.net.*;
import java.util.*;

/**
 * EReceiver is a class that reads messages sent from the server to the client.
 * The class will read each message, find out who should receive the message
 * and then deliver the message to the recipient.
 *
 * <p>Each class that wants to receive messages from the Erlang server should
 * implement the EReceive interface and then register itself to the EReceiver
 * object.
 * 
 * @version     1.2, 8 Mar 1997
 * @author      Kaj Nygren (kaj@medialab.ericsson.se)
 * @author      Joakim Greben&ouml; (jocke@erix.ericsson.se)
 */
public class EReceiver implements Runnable {
  Socket socket;
  Thread runner;
  EProcess self;
  Unpacker unpacker;
  ESock eSock;
  boolean connected = false;

  // Should probably check for wraparound etc but since it's an integer
  // it shouldn't really matter.
  // (Who would use 4 billion receivers ???)
  int idNr = 0;

  Hashtable receivers = new Hashtable();
  // For reverse lookup
  Hashtable reverse = new Hashtable();

  /** 
   * Creates a EReceiver object using a specified eSock module.
   * The EReceiver starts a new thread that reads messages from a
   * socket, and deliver those messages to the recipients.
   */
  EReceiver(ESock eSock) {
    this.eSock = eSock;
  }
  
  /** 
   * Connects the client to the server.
   *
   * @exception JiveIOException If a connection error occurred.
   */
  public void connect() throws JiveIOException {
    eSock.initReceiver(this);
  }
  
  /** 
   * Disconnects the client from the server.
   */
  public void disconnect() {
    if (connected) {
      if (runner != null)
	runner.stop();
      if (socket != null)
	try {
	  socket.close();
	} catch (IOException e) {}
      connected = false;
    }
  }
  
  /** 
   * Returns whether or not the client is connected to the server.
   */
  public boolean connected() {
    return connected;
  }

  /**
   * Called from the ESock object when the socket connection has been opened.
   * 
   * <p>The method is inherently synchronized since it's called from 
   * ESock.initReceiver which is synchronized.
   */
  protected void init(Socket s,Unpacker unpacker, EProcess self) {
    socket = s;
    if (this.self == null) 
      this.self = self;  // If we don't have a self process get one
    else 
      this.self.setValue(self.value()); // Otherwise reuse the old one
    if (runner != null)
      runner.stop();
    this.unpacker = unpacker;
    unpacker.s = s;
    runner = new Thread(this);
    connected = true;
    runner.start();
  }
  
  /**
   * The main loop of this thread.
   * It waits until a message has been delivered. Then it will parse the
   * message and deliver it to the recipient.
   */
  public void run() {
    try { 
      while (true) {
	unpacker.parseMessage();
      } 
    } catch(JiveException e) {
      System.out.println("Sorry a reading error occurred: "+e);
      connected = false;
    }
  }
  
  /**
   * Returns the EProcess that represents the Erlang process that sends
   * messages to this client.
   * The self process should be supplied to all Erlang processes that should
   * be able to communicate with an EReceive object on the client.
   *
   * @exception JiveIOException If the client isn't connected.
   */
  protected EProcess self() throws JiveIOException {
    if (self != null && connected) 
      return self;
    else 
      throw new JiveIOException("Connection broken: Self not initialized");
  }  

  /**
   * Registers an EReceive object. The EReceive object can then receive
   * messages that are sent from an Erlang process.
   * The method returns an EInteger which should be sent to the Erlang 
   * process wanting to communicate with the EReceive object.
   */
  public EInteger register(EReceive target) {
    EInteger id = new EInteger(idNr++);
    receivers.put(id,target);
    reverse.put(target,id);
    return id;
  }

  /**
   * Unregisters an EReceive object.
   */
  public void remove(EReceive target) {
    receivers.remove(toID(target));
    reverse.remove(target);
  }

  /**
   * Unregisters an EReceive object given the corresponding EInteger targetID.
   */ 
  public void remove(EInteger targetID) {
    reverse.remove(toTarget(targetID));
    receivers.remove(targetID);
  }
  
  /**
   * Converts an EReceive target to the corresponding targetID. This
   * method assumes that the EReceive target is registered.
   */
  public EInteger toID(EReceive target) {
    return (EInteger)reverse.get(target);
  }
  
  /**
   * Converts an EInteger targetID to the corresponding EReceive target. This
   * method assumes that the EReceive target is registered.
   */
  public EReceive toTarget(EInteger targetID) {
    return (EReceive)receivers.get(targetID);
  }
}
