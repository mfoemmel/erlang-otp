package jive.erlang;

import java.util.*;

/**
 * EFloat is a class that encapsulates an Erlang Float.
 *
 * @version     1.2, 8 Mar 1997
 * @author      Kaj Nygren (kaj@medialab.ericsson.se)
 * @author      Joakim Greben&ouml; (jocke@erix.ericsson.se)
 */
public class EFloat extends EVar {
  private double eFloat;

  /** 
   * Initiates the float type. It is private to prevent creation
   * of Erlang Floats without a value.
   */
  private EFloat() {
    type = EFLOAT;
  }

  /**
   * Creates an Erlang Float with the a given double value.
   */
  public EFloat(double eFloat) {
    this();
    this.eFloat = eFloat;
  }
  
  /** 
   * Returns the double value of this Erlang Float.
   */
  public  double value() {
    return eFloat;
  }   

  /**
   * Returns a hashcode for this EFloat (Code taken from Double class).
   */
  public int hashCode() {
    long bits = Double.doubleToLongBits(eFloat);
    return (int)(bits ^ (bits >> 32));
  }

  /**
   * Compares this object to the specified object.
   * Two EFloats are the same if they contain the same double value (Code
   * adapted from Integer class).
   */
  public boolean equals(Object obj) {
    if ((obj != null) && (obj instanceof EFloat)) {
      return eFloat == ((EFloat)obj).value();
    }
    return false;
  }

  /** 
   * Packs the Erlang Float into a byte array. 
   * Used when sending the string to an Erlang server.
   */
  public byte[] pack() {
    // The double is packed in string form
    // Probably not very effective, but at least it works
    String floatTxt = new Double(eFloat).toString();
    byte msg[] = new byte[5+floatTxt.length()];
    msg[0] = (byte) type;
    intToBytes(floatTxt.length(),msg,1);
    floatTxt.getBytes(0,floatTxt.length(),msg,5);
    return msg;
  }
}
