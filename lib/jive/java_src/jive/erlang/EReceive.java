package jive.erlang;

/**
 * EReceive is an interface that must be implemented by each class which
 * want to receive messages from the Erlang server.
 *
 * <p>When implemented the receive method will be called with the message
 * as parameter when the Erlang side wants to communicate with the object.
 *
 * <p>Each object implementing the EReceive interface must also register 
 * themselves to the EReceiver object.
 * 
 * @version     1.2, 8 Mar 1997
 * @author      Kaj Nygren (kaj@medialab.ericsson.se)
 * @author      Joakim Greben&ouml; (jocke@erix.ericsson.se)
 */
public interface EReceive {
    public abstract void receive(EVar data);
}
