package jive.erlang;

import java.util.*;

/**
 * EInteger is a class that encapsulates an Erlang Integer.
 *
 * @version     1.2, 8 Mar 1997
 * @author      Kaj Nygren (kaj@medialab.ericsson.se)
 * @author      Joakim Greben&ouml; (jocke@erix.ericsson.se)
 */
public class EInteger extends EVar {
  
  private int eInt;

  /** 
   * Initiates the integer type. It is private to prevent creation
   * of Erlang Integers without a value.
   */
  private EInteger() {
    type = EINTEGER;
  }

  /**
   * Creates an Erlang Integer with the given integer value.
   */
  public EInteger(int eInt) {
    this();
    this.eInt = eInt;
  }
  
  /** 
   * Returns the integer value of this Erlang Integer.
   */
  public int value() {
    return eInt;
  }   

  /** 
   * Returns the (unsigned) long value of this Erlang Integer.
   */
  public long unsignedValue() {
    return (long)(((long)Math.pow(2,32)+(long)eInt)%(long)Math.pow(2,32));
  }
  
  /**
   * Returns a hashcode for this EInteger.
   */
  public int hashCode() {
    return eInt;
  }

  /**
   * Compares this object to the specified object.
   * Two EIntegers are the same if they contain the same integer value.
   */
  public boolean equals(Object obj) {
    if ((obj != null) && (obj instanceof EInteger)) {
      return eInt == ((EInteger)obj).value();
    }
    return false;
  }

  /** 
   * Packs the Erlang String into a byte array. 
   * Used when sending the string to an Erlang server.
   */
  public byte[] pack() {
    byte msg[] = new byte[6];
    msg[0] = (byte) type;
    msg[1] = (byte)((eInt >= 0) ? 0 : 1);
    intToBytes(Math.abs(eInt),msg,2);
    return msg;
  }
}
