package jive.erlang;

import java.applet.Applet;

/**
 * EApplet provides an applet stub that automatically connects to an 
 * Erlang Jive server.
 * 
 * <p>The class also provides access to some important Jive classes.
 *
 * <p>All classes subclassing EApplet must first call <tt>super.init()</tt>
 * in their init method. This will initializ the Jive client.
 *
 * <pre>
 * class Example extends EApplet {
 *     public void init() {
 *        super.init();
 *     }
 * } 
 * </pre>
 *
 * <p>The HTML code must also contain a PARAM tag specifying which port to
 * use on the server.
 * <pre>
 * <PARAM NAME=PORT VALUE=4711>
 * </pre>
 *
 * @see         EApplication
 * @version     1.2, 8 Feb 1997
 * @author      Kaj Nygren (kaj@medialab.ericsson.se)
 * @author      Joakim Greben&ouml; (jocke@erix.ericsson.se)
 */
public class EApplet extends Applet {
  private EReceiver receiver;

  /** 
   * This method will initialize the Jive client. It must be called by
   * the subclassing Applet's init method.
   */
  public void init() {
    try {
      int port = Integer.parseInt(getParameter("port"));
      // Init ESock
      ERuntime.getRuntime().setESock(getCodeBase().getHost(),port);      
      
      ESock eSock = ERuntime.getRuntime().getESock();
      receiver = new EReceiver(eSock);  // Initiate receiver
      receiver.connect();  // Connect with server
      
    } catch(Exception e) {
      System.out.println("Sorry! Cannot connect to the Jive server: "+e);
    }
  }
  
  /**
   * This method will be called everytime the Applet is started.
   * It will try to connect the Applet to the Erlang Jive server.
   */
  public void start() {
    try {
      connect();
    } catch (JiveException e) {
      System.out.println("Sorry!, Cannot connect to Jive server: "+e);
    }
  }

  /**
   * This method will be called everytime the Applet is stopped.
   * It will try to disconnect the Applet from the server.
   */  
  public void stop() {
    disconnect();
  }

  /**
   * This method connect the Applet to the Erlang Jive server.
   * If a connection is already in place nothing happens. 
   * If a connection cannot be established a JiveIOException is thrown
   *
   * @exception JiveIOException If a connection error occurred or if the 
   * receiver was uninitialized.
   */
  public void connect() throws JiveIOException {
    if (receiver != null)
      receiver().connect();
    else 
      throw new JiveIOException("No Receiver initialized.");
  }

  /**
   * This method disconnect the Applet from the Erlang Jive server.
   * If the Applet already is disconnected nothing happens.
   */ 
  public void disconnect() {
    if (receiver != null)
      receiver().disconnect();
  }
  
  /**
   * This method checks whether the Applet is connected to the Erlang Jive
   * server or not.
   */
  public boolean connected() {
    if (receiver != null)
      return receiver.connected();
    else 
      return false;
  }

  /**
   * Returns the ESock object used for socket communication with the
   * Erlang Jive server.
   */
  public ESock getESock() {
    return ERuntime.getRuntime().getESock();
  }
  
  /**
   * Returns the EReceiver object used for registering objects which should
   * be called from Erlang.
   */
  public EReceiver receiver() {
    return receiver;
  }
  
  /** 
   * Returns the EProcess representing the client's peer Erlang process.
   * The peer Erlang process mimics the client in the Erlang system
   * and forward all messages sent to it (and vice versa).
   *
   * <p>The self process should be supplied to all Erlang processes wanting 
   * to send information back to the client.
   *
   * @exception JiveIOException If the client isn't connected.
   */
  public EProcess self() throws JiveIOException {
    if (receiver != null)
      return receiver.self();
    else 
      throw new JiveIOException("No Receiver initialized.");
  }
}
