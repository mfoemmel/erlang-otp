package jive.erlang;

import java.util.*;

/**
 * EReference is a class that encapsulates an Erlang Reference.
 *
 * @version     1.2, 8 Mar 1997
 * @author      Kaj Nygren (kaj@medialab.ericsson.se)
 * @author      Kent Boortz (kent@erlang.ericsson.se)
 */
public class EReference extends EVar {
  private byte data[] = null;

  /** 
   * Initiates the reference type. 
   */
  public EReference() {
    type = EREFERENCE;
    data = new byte[0];
  }

  /**
   * Creates an Erlang Reference with the a given byte array.
   */
  public EReference(byte data[]) {
    this();
    this.data = data;
  }
  
  /** 
   * Returns the integer value of this Erlang Reference.
   */
  public byte[] value() {
    return data;
  }   

  /** 
   * Returns the length of the Erlang Reference.
   */
  public int length() {
    return data.length;
  }   

  /** 
   * Packs the Erlang Reference into a byte array. 
   * Used when sending the reference to an Erlang server.
   */
  public byte[] pack() {
    if (data != null) {
      // Pack the reference data
      byte bytes[] = new byte[data.length+5];
      bytes[0] = type;
      
      // Pack the length as an int (32 bits)
      intToBytes(data.length,bytes,1);
      
      // Pack the data
      System.arraycopy(data, 0, bytes, 5, data.length);
      return bytes;
      
    } else {
      // data array was null
      // Return an empty reference (Could probably throw an exception instead)
      byte bytes[] = new byte[5];
      bytes[0] = type;
      intToBytes(0,bytes,1);
      return bytes;
    }
  }
}
