package jive.erlang;

/**
 * Signals that a Jive exception has occurred.
 * 
 * @version     1.2, 8 Mar 1997
 * @author      Kaj Nygren (kaj@medialab.ericsson.se)
 * @author      Joakim Greben&ouml; (jocke@erix.ericsson.se)
 */
public class JiveException extends Exception {

    /**
     * Constructs a JiveException with no details of why it was constructed.
     */
    public JiveException() {
	super();
    }

    /**
     * Constructs a JiveException with a message specifying why it was
     * created.
     */
    public JiveException(String s) {
	super(s);
    }
}
