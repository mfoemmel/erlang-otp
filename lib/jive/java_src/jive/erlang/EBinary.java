package jive.erlang;

import java.util.*;

/**
 * EBinary is a class that encapsulates an Erlang Binary.
 *
 * @version     1.2, 8 Mar 1997
 * @author      Kaj Nygren (kaj@medialab.ericsson.se)
 * @author      Joakim Greben&ouml; (jocke@erix.ericsson.se)
 */
public class EBinary extends EVar {
  private byte data[] = null;

  /** 
   * Initiates the binary type. 
   */
  public EBinary() {
    type = EBINARY;
    data = new byte[0];
  }

  /**
   * Creates an Erlang Binary with the a given byte array.
   */
  public EBinary(byte data[]) {
    this();
    this.data = data;
  }
  
  /** 
   * Returns the integer value of this Erlang Binary.
   */
  public byte[] value() {
    return data;
  }   

  /** 
   * Returns the length of the Erlang Binary.
   */
  public int length() {
    return data.length;
  }   

  /** 
   * Packs the Erlang Binary into a byte array. 
   * Used when sending the binary to an Erlang server.
   */
  public byte[] pack() {
    if (data != null) {
      // Pack the binary data
      byte bytes[] = new byte[data.length+5];
      bytes[0] = type;
      
      // Pack the length as an int (32 bits)
      intToBytes(data.length,bytes,1);
      
      // Pack the data
      System.arraycopy(data, 0, bytes, 5, data.length);
      return bytes;
      
    } else {
      // data array was null
      // Return an empty binary (Could probably throw an exception instead)
      byte bytes[] = new byte[5];
      bytes[0] = type;
      intToBytes(0,bytes,1);
      return bytes;
    }
  }
}
