package jive.erlang;

/**
 * Signals that a Jive security exception has occurred.
 * 
 * @version     1.2, 8 Mar 1997
 * @author      Kaj Nygren (kaj@medialab.ericsson.se)
 * @author      Joakim Greben&ouml; (jocke@erix.ericsson.se)
 */
public class JiveSecurityException extends JiveException {
  
  /**
   * Constructs a JiveSecurityException with no details on why it
   * was constructed.
   */
  public JiveSecurityException() {
    super();
  }
  
  /**
   * Constructs a JiveSecurity Exception with a message specifying why
   * it was created.
   */
  public JiveSecurityException(String s) {
    super(s);
  }
}
