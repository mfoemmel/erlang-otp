package jive.erlang;

import java.util.*;

/**
 * ETuple is a class that encapsulates an Erlang Tuple.
 *
 * @version     1.2, 8 Mar 1997
 * @author      Kaj Nygren (kaj@medialab.ericsson.se)
 * @author      Joakim Greben&ouml; (jocke@erix.ericsson.se)
 */
public class ETuple extends EVar {
  private Vector tuple = new Vector();

  /** 
   * Creates an empty tuple.
   */
  public ETuple() {
    type = ETUPLE;
  }

  /** 
   * Creates a tuple with 1 element.
   */
  public ETuple(EVar el1) {
    this();
    tuple.addElement(el1);
  }

  /** 
   * Creates a tuple with 2 elements.
   */
  public ETuple(EVar el1, EVar el2) {
    this();
    tuple.addElement(el1);
    tuple.addElement(el2);
  }

  /** 
   * Creates a tuple with 3 elements.
   */
  public ETuple(EVar el1, EVar el2, EVar el3) {
    this();
    tuple.addElement(el1);
    tuple.addElement(el2);
    tuple.addElement(el3);
  }

  /** 
   * Creates a tuple with 4 elements.
   */
  public ETuple(EVar el1, EVar el2, EVar el3, EVar el4) {
    this();
    tuple.addElement(el1);
    tuple.addElement(el2);
    tuple.addElement(el3);
    tuple.addElement(el4);
  }

  /** 
   * Creates a tuple with 5 elements.
   */
  public ETuple(EVar el1, EVar el2, EVar el3, EVar el4, EVar el5) {
    this();
    tuple.addElement(el1);
    tuple.addElement(el2);
    tuple.addElement(el3);
    tuple.addElement(el4);
    tuple.addElement(el5);
  }

  /** 
   * Creates a tuple with 6 elements.
   */
  public ETuple(EVar el1, EVar el2, EVar el3, EVar el4, EVar el5, EVar el6) {
    this();
    tuple.addElement(el1);
    tuple.addElement(el2);
    tuple.addElement(el3);
    tuple.addElement(el4);
    tuple.addElement(el5);
    tuple.addElement(el6);
  }

  /** 
   * Creates a tuple from an array of EVar elements.
   */
  public ETuple(EVar el[]) {
    this();
    for (int i=0;i<el.length;i++)
      tuple.addElement(el[i]);
  }
  
  /** 
   * Returns this tuple as an array of EVar elements.
   */
  public EVar[] value() {
    if (tuple.size() > 0) {
      EVar vars[] = new EVar[tuple.size()];
      for (int i=0;i<tuple.size();i++) 
	vars[i] = (EVar)tuple.elementAt(i);
      return vars;
    } else
      return null;
  }
  
  /** 
   * Returns the number of EVars in this tuple.
   */
  public int length() {
    return tuple.size();
  }

  /** 
   * Returns the EVar at the specified index.
   */
  public EVar elementAt(int index) {
    return (EVar)tuple.elementAt(index);
  }

  /** 
   * Returns an enumeration of the EVars in this tuple.
   */
  public Enumeration elements() {
    return tuple.elements();
  }
  
  /** 
   * Packs the Erlang tuple into a byte array. 
   * Used when sending the list to an Erlang server.
   */
  public byte[] pack() {
    byte bytes[] = new byte[5];
    bytes[0] = type;
   
    intToBytes(tuple.size(),bytes,1);
    
    for (Enumeration e=tuple.elements();e.hasMoreElements();) {
      EVar var = (EVar) e.nextElement();
      bytes = concat(bytes,var.pack());
    }
    return bytes;
  }
}
