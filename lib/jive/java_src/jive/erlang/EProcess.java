package jive.erlang;

import java.util.*;

/**
 * EProcess is a class that encapsulates an Erlang Pid.
 * The class will mediate messages to the real Erlang Pid at the
 * Erlang side.
 *
 * @version     1.2, 8 Mar 1997
 * @author      Kaj Nygren (kaj@medialab.ericsson.se)
 * @author      Joakim Greben&ouml; (jocke@erix.ericsson.se)
 */
public class EProcess extends EVar {
  private int ePid;

  /**
   * Initiates the Process type. It is private to prevent creation
   * of processes without a corresponding Erlang Pid.
   */
  private EProcess() {
    type = EPROCESS;
  }
  
  /**
   * Spawns a new Erlang process and creates a Process class 
   * representing the Erlang Pid.
   *
   * @exception JiveSecurityException is thrown if a security violation
   * occurred.
   * @exception JiveIOException is thrown if a communication error occurred.
   * @exception JiveException is thrown with an error text given by the server.
   */
  public EProcess(String module,String name, 
		  EList params) throws JiveSecurityException,JiveIOException,JiveException {
    this();
    ESock eSock = ERuntime.getRuntime().getESock();
    EProcess tmp = eSock.spawn(new EAtom(module),new EAtom(name),params);
    ePid = tmp.value();
  }
  
  /**
   * Spawns a new Erlang process and creates a Process class
   * representing the Erlang Pid.
   *
   * @exception JiveSecurityException is thrown if a security violation
   * occurred.
   * @exception JiveIOException is thrown if a communication error occurred.
   * @exception JiveException is thrown with a error text given by the server.
   */
  public EProcess(EAtom module,EAtom name, 
		  EList params) throws JiveSecurityException,JiveIOException,JiveException {
    this();
    ESock eSock = ERuntime.getRuntime().getESock();
    EProcess tmp  = eSock.spawn(module,name,params);
    ePid = tmp.value();
  }
  
  /** 
   * Create a EProcess wrapper around a given Pid value.
   * Used only by class ESock to create EProcesses when spawning.
   */
  protected EProcess(int ePid) {
    this();
    this.ePid = ePid;
  }
  
  /** 
   * Returns the Pid value representing the Erlang Pid.
   */
  public int value() {
    return ePid;
  }

  /** 
   * Sends a message to the Erlang Process represented by this EProcess class.
   *
   * @exception JiveIOException if a connection error occurred.
   */
  public void send(EVar data) throws JiveIOException {
    ERuntime eRuntime = ERuntime.getRuntime();
    ESock eSock = eRuntime.getESock();
    eSock.send(this,data);
  }

  /** 
   * Packs the Pid value into a byte array. 
   * Used when sending the Pid to an Erlang server.
   */
  public byte[] pack() {
    byte msg[] = new byte[5];
    msg[0] = (byte) type;
    intToBytes(ePid,msg,1);
    return msg;
  }

  /**
   * Sets the internal pid-id value. Only used by EReceiver to reuse
   * the self-pid object with different internal pid-id values.
   */
  protected void setValue(int ePid) {
    this.ePid = ePid;
  }
}

