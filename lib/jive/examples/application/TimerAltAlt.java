import java.awt.*;
import jive.erlang.*;

public class TimerAltAlt implements EReceive {
  private EApplication jive;
  private int time=0;

  /*
   * Usage: Timer host port
   */
  public static void main(String args[]) {
    if(args.length != 2) {
      System.err.println("Usage: Timer host port");
    } else {
      try {
	int port=Integer.parseInt(args[1]);
	new TimerAltAlt(args[0],port);
      } catch (JiveException e) {
	System.err.println(e);
	System.exit(0);
      }
    }
  }

  /*
   * Constructor which initialize the Erlang connection and spawns an
   * Erlang clock server.
   */
  public TimerAltAlt(String host,int port) throws JiveException {
    // Initialize the connection 
    jive=new EApplication(host,port);
    // Connect to the remote server
    jive.connect();
    // Register this object so Erlang can send messages to it
    EInteger id=jive.receiver().register(this);
    // Spawn a clock server on the Erlang node
    ESock sender=jive.getESock();
    sender.spawn("clock","start",new EList(id,jive.self()));
  }
  
  /*
   * receive gets called each time an Erlang message is sent to an object
   * instance of this class. This function need to be implemented hence
   * the class implements the EReceive interface.
   */
  public void receive(EVar var) {
    // Print the 10 first received time stamps on stdout
    if(var.type() == EVar.EINTEGER) {
      time=((EInteger)var).value();
      if(time == 10) {
	// Disconnect from the Erlang server
	jive.disconnect();
	System.exit(0);
      } else {
	System.out.println("Timer has been running for "+time+" seconds");
      }
    }
  }
}


