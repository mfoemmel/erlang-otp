package jive.erlang;

/**
 * EApplication provides an application stub that automatically connects to
 * the Erlang Jive server.
 * 
 * <p>The class also provides access to some important Jive classes.
 *
 * @see         EApplet
 * @version     1.2, 15 Mar 1997
 * @author      Kaj Nygren (kaj@medialab.ericsson.se)
 * @author      Joakim Greben&ouml; (jocke@erix.ericsson.se)
 */
public class EApplication {
  private EReceiver receiver;
  
  /** 
   * This method will initialize the Jive client. It must be called by
   * the subclassing Application's init method.
   *
   * @exception JiveIOException If a connection error occurred or the 
   * receiver was uninitialized.
   */
  public EApplication(String host,int port) throws JiveIOException {
    ERuntime.getRuntime().setESock(host,port);
    ESock eSock = ERuntime.getRuntime().getESock();
    receiver = new EReceiver(eSock);
  }
  
  /**
   * This method connect the Application to the Erlang Jive server.
   * If a connection is already in place nothing happens. 
   * If a connection cannot be established a JiveIOException is thrown.
   *
   * @exception JiveIOException If a connection error occurred or the 
   * receiver was uninitialized.
   */
  public void connect() throws JiveIOException {
    if (receiver != null)
      receiver().connect();
    else 
      throw new JiveIOException("No Receiver initialized.");
  }
  
  /**
   * This method disconnect the Application from the Erlang Jive server.
   * If the Application already is disconnected nothing happens.
   */ 
  public void disconnect() {
    if (receiver != null)
      receiver().disconnect();
  }
  
  /**
   * This method checks whether the application is connected to the Erlang
   * Jive server or not.
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
