package jive.erlang;
import java.util.*;

/**
 * EAtom is a class that represents an Erlang Atom.
 *
 * @version     1.2, 8 Mar 1997
 * @author      Kaj Nygren (kaj@medialab.ericsson.se)
 * @author      Joakim Greben&ouml; (jocke@erix.ericsson.se)
 */
public class EAtom extends  EVar {
  String eString = "";
  
  /** 
   * Initiates the atom type. It is private to prevent creation
   * of Erlang Atoms without a value.
   */
  public EAtom() {
    type = EATOM;
  }

  /** 
   * Creates an Erlang Atom with a value given by eString.
   * <tt>Note:</tt> Should not allow empty Strings since we cannot allow
   * empty Atoms.
   */
  public EAtom(String eString) {
    this();
    this.eString = eString;
  }
  
  /**
   * Returns the length of this atom.
   */
  public int length() {
    return eString.length();
  }
  /** 
   * Returns the Java string representing the Atom.
   */
  public String value() {
    return eString;
  }

  /**
   * Returns a hashcode for this EAtom. Same as for String.
   */
  public int hashCode() {
    return eString.hashCode();
  }
  
  /**
   * Compares this EAtom with another EAtom
   * Two EAtoms are equal if their respective Java strings are equal.
   */
  public boolean equals(Object str1) {
    if (str1 != null && eString != null && str1 instanceof EAtom)
      return eString.equals(((EAtom)str1).value());
    else
      return false;
  }

  /** 
   * Packs the Erlang Atom into a byte array. 
   * Used when sending the string to an Erlang server.
   */
  public byte[] pack() {
    if (eString != null) {
      byte bytes[] = new byte[eString.length()+5];
      bytes[0] = type;

      // Pack the length as an int (32 bits)
      intToBytes(eString.length(),bytes,1);

      // Pack the String (Note: No Unicode support)
      eString.getBytes(0,eString.length(),bytes,5);
      return bytes;
    } else
      // Should probably throw some sort of exception
      return null;
  }
}
