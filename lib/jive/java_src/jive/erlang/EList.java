package jive.erlang;

import java.util.*;

/**
 * EList is a class that encapsulates an Erlang List.
 *
 * @version     1.2, 8 Mar 1997
 * @author      Kaj Nygren (kaj@medialab.ericsson.se)
 * @author      Joakim Greben&ouml; (jocke@erix.ericsson.se)
 */
public class EList extends EVar {
  private Vector list = new Vector();

  /** 
   * Creates an empty list.
   */
  public EList() {
    type = ELIST;
  }

  /** 
   * Creates a list with 1 element.
   */
  public EList(EVar el1) {
    this();
    list.addElement(el1);
  }

  /** 
   * Creates a list with 2 elements.
   */
  public EList(EVar el1, EVar el2) {
    this();
    list.addElement(el1);
    list.addElement(el2);
  }
  
  /** 
   * Creates a list with 3 elements.
   */
  public EList(EVar el1, EVar el2, EVar el3) {
    this();
    list.addElement(el1);
    list.addElement(el2);
    list.addElement(el3);
  }

  /** 
   * Creates a list with 4 elements.
   */
  public EList(EVar el1, EVar el2, EVar el3, EVar el4) {
    this();
    list.addElement(el1);
    list.addElement(el2);
    list.addElement(el3);
    list.addElement(el4);
  }

  /** 
   * Creates a list with 5 elements.
   */
  public EList(EVar el1, EVar el2, EVar el3, EVar el4, EVar el5) {
    this();
    list.addElement(el1);
    list.addElement(el2);
    list.addElement(el3);
    list.addElement(el4);
    list.addElement(el5);
  }

  /** 
   * Creates a list with 6 elements.
   */
  public EList(EVar el1, EVar el2, EVar el3, EVar el4, EVar el5, EVar el6) {
    this();
    list.addElement(el1);
    list.addElement(el2);
    list.addElement(el3);
    list.addElement(el4);
    list.addElement(el5);
    list.addElement(el6);
  }

  /** 
   * Creates a list from an array of EVar elements.
   */
  public EList(EVar el[]) {
    this();
    for (int i=0;i<el.length;i++)
      list.addElement(el[i]);
  }

  /** 
   * Returns this list as an array of EVar elements.
   */
  public EVar[] value() {
    if (list.size() > 0) {
      EVar vars[] = new EVar[list.size()];
      for (int i=0;i<list.size();i++) 
	vars[i] = (EVar)list.elementAt(i);
      return vars;
    } else
      return null;
  }
  
  /** 
   * Returns the number of EVars in this list.
   */
  public int length() {
    return list.size();
  }

  /** 
   * Returns the EVar at the specified index.
   */
  public EVar elementAt(int index) {
    return (EVar)list.elementAt(index);
  }

  /** 
   * Returns an enumeration of the EVars in this list.
   */
  public Enumeration elements() {
    return list.elements();
  }

  /** 
   * Packs the Erlang List into a byte array. 
   * Used when sending the list to an Erlang server.
   */
  public byte[] pack() {
    byte bytes[] = new byte[5];
    bytes[0] = type;

    intToBytes(list.size(),bytes,1);
    for (Enumeration e=list.elements();e.hasMoreElements();) {
      EVar var = (EVar) e.nextElement();
      bytes = concat(bytes,var.pack());
    }
    return bytes;
  }
}
