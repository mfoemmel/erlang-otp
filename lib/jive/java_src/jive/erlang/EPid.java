package jive.erlang;

import java.util.*;

/**
 * EPid is a class that encapsulates an Erlang Pid.
 *
 * @version     1.2, 8 Mar 1997
 * @author      Kaj Nygren (kaj@medialab.ericsson.se)
 * @author      Kent Boortz (kent@erlang.ericsson.se)
 */
public class EPid extends EVar {
  private byte data[] = null;

  /** 
   * Initiates the pid type. 
   */
  public EPid() {
    type = EPID;
    data = new byte[0];
  }

  /**
   * Creates an Erlang Pid with the a given byte array.
   */
  public EPid(byte data[]) {
    this();
    this.data = data;
  }
  
  /** 
   * Returns the integer value of this Erlang Pid.
   */
  public byte[] value() {
    return data;
  }   

  /** 
   * Returns the length of the Erlang Pid.
   */
  public int length() {
    return data.length;
  }   

  /** 
   * Packs the Erlang Pid into a byte array. 
   * Used when sending the pid to an Erlang server.
   */
  public byte[] pack() {
    if (data != null) {
      // Pack the pid data
      byte bytes[] = new byte[data.length+5];
      bytes[0] = type;
      
      // Pack the length as an int (32 bits)
      intToBytes(data.length,bytes,1);
      
      // Pack the data
      System.arraycopy(data, 0, bytes, 5, data.length);
      return bytes;
      
    } else {
      // data array was null
      // Return an empty pid (Could probably throw an exception instead)
      byte bytes[] = new byte[5];
      bytes[0] = type;
      intToBytes(0,bytes,1);
      return bytes;
    }
  }
}
