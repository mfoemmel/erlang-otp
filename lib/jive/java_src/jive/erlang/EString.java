package jive.erlang;

import java.util.*;

/**
 * EString is a class that encapsulates an Erlang String.
 *
 * @version     1.2, 8 Mar 1997
 * @author      Kaj Nygren (kaj@medialab.ericsson.se)
 * @author      Joakim Greben&ouml; (jocke@erix.ericsson.se)
 */
public class EString extends  EVar {
  String eString = "";
  
  /** 
   * Creates an empty Erlang String.
   */
  public EString() {
    type = ESTRING;
  }

  /**
   * Creates an Erlang String with the contents of eString.
   */
  public EString(String eString) {
    this();
    this.eString = eString;
  }
  
  /**
   * Returns the length of the string.
   */
  public int length() {
    return eString.length();
  }

  /**
   * Returns a Java string version of the Erlang String.
   */
  public String value() {
    return eString;
  }
  
  /**
   * Returns a hashcode for this EString. Same as for String.
   */
  public int hashCode() {
    return eString.hashCode();
  }
  
  /**
   * Compares this EString with another EString.
   * Two EStrings are equal if their respective Java strings are equal.
   */
  public boolean equals(Object str1) {
    if (str1 != null && eString != null && str1 instanceof EString)
      return eString.equals(((EString)str1).value());
    else
      return false;
  }

  /** 
   * Packs the Erlang String into a byte array. 
   * Used when sending the string to an Erlang server.
   */
  public byte[] pack() {
    if (eString != null) {
      byte bytes[] = new byte[eString.length()+5];
      bytes[0] = type;

      // Pack the length as an int (32 bits)
      intToBytes(eString.length(),bytes,1);

      // Pack the string (Note, no Unicode support)
      eString.getBytes(0,eString.length(),bytes,5);
      return bytes;
    } else {
      // String was null
      // Return an empty string (Could probably throw an exception instead)
      byte bytes[] = new byte[5];
      bytes[0] = type;
      intToBytes(0,bytes,1);
      return bytes;
    }
  }
}
