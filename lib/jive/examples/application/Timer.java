import java.awt.*;
import jive.erlang.*;

public class Timer {
  /*
   * Usage: Timer host port
   */
  public static void main(String args[]) {
    if(args.length != 2) {
      System.err.println("Usage: Timer host port");
    } else {
      try {
	int port=Integer.parseInt(args[1]);
	new TimeThread(args[0],port);
      } catch (JiveException e) {
	System.err.println(e);
	System.exit(0);
      }
    }
  }
}

class TimeThread extends EApplication implements EReceive, Runnable {
  private int time=0;

  /*
   * Constructor which initialize the Erlang connection and spawns an
   * Erlang clock server.
   */
  public TimeThread(String host,int port) throws JiveException {
    // Call the EApplication constructor to initialize the connection 
    super(host,port);
    // Connect to the remote server
    connect();
    // Register this object so Erlang can send messages to it
    EInteger id=receiver().register(this);
    // Spawn a clock server on the Erlang node
    ESock sender=getESock();
    sender.spawn("clock","start",new EList(id,self()));
  }
  
  public void run() {
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
	disconnect();
	System.exit(0);
      } else {
	System.out.println("Timer has been running for "+time+" seconds");
      }
    }
  }
}
