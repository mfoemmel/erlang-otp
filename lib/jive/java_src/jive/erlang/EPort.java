package jive.erlang;

import java.util.*;

/**
 * EPort is a class that encapsulates an Erlang Port.
 *
 * @version     1.2, 8 Mar 1997
 * @author      Kaj Nygren (kaj@medialab.ericsson.se)
 * @author      Kent Boortz (kent@erlang.ericsson.se)
 */
public class EPort extends EVar {
  private byte data[] = null;

  /** 
   * Initiates the port type. 
   */
  public EPort() {
    type = EPORT;
    data = new byte[0];
  }

  /**
   * Creates an Erlang Port with the a given byte array.
   */
  public EPort(byte data[]) {
    this();
    this.data = data;
  }
  
  /** 
   * Returns the integer value of this Erlang Port.
   */
  public byte[] value() {
    return data;
  }   

  /** 
   * Returns the length of the Erlang Port.
   */
  public int length() {
    return data.length;
  }   

  /** 
   * Packs the Erlang Port into a byte array. 
   * Used when sending the port to an Erlang server.
   */
  public byte[] pack() {
    if (data != null) {
      // Pack the port data
      byte bytes[] = new byte[data.length+5];
      bytes[0] = type;
      
      // Pack the length as an int (32 bits)
      intToBytes(data.length,bytes,1);
      
      // Pack the data
      System.arraycopy(data, 0, bytes, 5, data.length);
      return bytes;
      
    } else {
      // data array was null
      // Return an empty port (Could probably throw an exception instead)
      byte bytes[] = new byte[5];
      bytes[0] = type;
      intToBytes(0,bytes,1);
      return bytes;
    }
  }
}
