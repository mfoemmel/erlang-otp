package jive.erlang;
import java.io.*;
import java.net.*;

/**
 * Deliverer will start a thread that delivers a message to the server.
 * The Deliverer class is only used by ESock to perform asynchronous 
 * message passing.
 *
 * @version     1.0, 8 Feb 1997
 * @author      Kaj Nygren (kaj@medialab.ericsson.se)
 * @author      Joakim Greben&ouml; (jocke@erix.ericsson.se)
 */
public class Deliverer implements Runnable {
  OutputStream output;
  Socket socket;
  byte message[];
  Thread sender;
  
  public Deliverer() {
  }
  
  /** 
   * Creates a new Deliverer that will send a message through a socket.
   */
  public Deliverer(byte message[]) {
    this.message = message;
    sender = new Thread(this);
    sender.start();
  }
  
  /** 
   * This method is executed by the sender thread. The method sends a message
   * (given as a byte array) through a socket to the Erlang server.
   */
  public void run() {
    try {
      ESock eSock = ERuntime.getRuntime().getESock();
      socket = eSock.connect();
      output = socket.getOutputStream();
      output.write(message);
      output.flush();
      socket.close();
    } catch(Exception e) {
      System.out.println("Deliverer: Send Error: "+e);
    }
  }
}
