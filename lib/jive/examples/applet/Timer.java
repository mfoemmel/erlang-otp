import java.applet.Applet;
import java.awt.*;
import jive.erlang.*;

public class Timer extends EApplet implements EReceive {
  private int time=0;
  
  /*
   * The start method initialize the Erlang connection and spawns an
   * Erlang clock server.
   */  
  public void start() {
    // Call the EApplet constructor to initialize the connection 
    super.start();
    try {
      // Register this Applet so Erlang can send messages to it
      EInteger id=receiver().register(this);
      // Spawn a clock server on the Erlang node
      ESock sender=getESock();
      sender.spawn("clock","start",new EList(id,self()));
    } catch (JiveException e) {
      System.err.println(e);
    }
  }
  
  public void paint(Graphics g) {
    g.drawString("Timer has been running for "+time+" seconds",10,10);
  }
  
  public void receive(EVar var) {
    if(var.type() == EVar.EINTEGER) {
      time=((EInteger)var).value();
      repaint();
    }
  }
}
