package jive.erlang;

import java.util.*;

/**
 * ERuntime prvovides the hooks into the runtime system of the client
 * Jive environment.
 * 
 * The class is instanciated only once for each Applet. To retreive the runtime
 * environment for an applet use: <tt>ERuntime.getRuntime();</tt>
 * 
 * The Runtime currently only stores the ESock class used for the socket
 * communication with the Jive server.
 *
 * @version     1.2, 8 Mar 1997
 * @author      Kaj Nygren (kaj@medialab.ericsson.se)
 * @author      Joakim Greben&ouml; (jocke@erix.ericsson.se)
 */
public class ERuntime {
  private static ERuntime currentRuntime = new ERuntime();
  private ESock eSock;
    
  /** 
   * Don't let anyone else instantiate this class!
   */
  private ERuntime() {}
  
  /**
   * Returns the runtime environment.
   */
  public static ERuntime getRuntime() { 
    return currentRuntime;
  }
  
  /** 
   * Create a new ESock class that communicates with a given host and port.
   * This method is only called from EApplet when initiating an applet.
   * 
   * @exception JiveIOException If an Applet is trying to connect to a Jive
   * server on a port and another Applet in the same thread group is 
   * connected to a Jive server on another port.
   */
  protected void setESock(String host, int port) throws JiveIOException {
    if (eSock == null) 
      eSock = new ESock(host,port);
    else if (!(eSock.getHost().equals(host) && eSock.getPort() == port)) {
      throw new JiveIOException("A Jive server is already being used on another port");
    }
  }
  
  /** 
   * Returns the current ESock object used to communicate with the 
   * Jive server.
   */
  public ESock getESock() {
    return eSock;
  }
}
