package jive.erlang;

/**
 * Signals that a Jive IO exception has occurred.
 * 
 * @version     1.2, 8 Mar 1997
 * @author      Kaj Nygren (kaj@medialab.ericsson.se)
 * @author      Joakim Greben&ouml; (jocke@erix.ericsson.se)
 */
public class JiveIOException extends JiveException {
  /**
   * Constructs a JiveIOException with no details on why it
   * was constructed.
   */
  public JiveIOException() {
    super();
  }
  
  /**
   * Constructs a JiveIOException with a message specifying why it was
   * created.
   */
  public JiveIOException(String s) {
    super(s);
  }
}
