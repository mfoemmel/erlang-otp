package jive.erlang;

/**
 * EVar is an abstract superclass for Erlang variables.
 * EInteger, EString, EAtom, EFloat, ETuple, EList, EBinary and EProcess are 
 * subclasses of EVar.
 *
 * @see EInteger
 * @see EString
 * @see EAtom
 * @see EFloat
 * @see ETuple
 * @see EList
 * @see EProcess
 * @see EBinary
 * @see EReference
 * @see EPort
 * @see EPid
 * @version     1.2, 8 Mar 1997
 * @author      Kaj Nygren (kaj@medialab.ericsson.se)
 * @author      Joakim Greben&ouml; (jocke@erix.ericsson.se)
 */
public abstract class EVar {
  public final static byte ESTRING  = 100;
  public final static byte ELIST    = 101;
  public final static byte ETUPLE   = 102;
  public final static byte EATOM    = 103;
  public final static byte EINTEGER = 104;
  public final static byte EPROCESS = 105;
  public final static byte EFLOAT   = 106;
  public final static byte EBINARY  = 107;
  public final static byte EREFERENCE = 108;
  public final static byte EPORT    = 109;
  public final static byte EPID     = 110;

  /**
   * The type of this Variable.<br>
   * Possible types are:
   * <tt>EINTEGER, ESTRING, EATOM, EFLOAT, ELIST, ETUPLE, EPROCESS, EBINARY,
   * EREFERENCE, EPORT, EPID
   * </tt>
   */
  protected byte type = -1;
  
  /** 
   * Called to pack the variable into a byte array.
   * <p><b>Note:</b>Must be overridden in subclasses.
   */
  public abstract byte[] pack();
  
  /** 
   * Returns the type of this variable.<br>
   * Possible types are: 
   * <tt>EINTEGER, ESTRING, EATOM, EFLOAT, ELIST, ETUPLE, EPROCESS, EBINARY,
   * EREFERENCE, EPORT, EPID
   * </tt>
   */
  public byte type() {
    return type;
  }

  /**
   * Concatenates two byte arrays into one large array.
   */
  static public byte[] concat(byte src1[],byte src2[]) {
    byte result[] = new byte[src1.length+src2.length];
    System.arraycopy(src1, 0, result, 0, src1.length);
    System.arraycopy(src2, 0, result, src1.length, src2.length);
    return result;
  }
  
  /**
   * Packs an integer into a byte array.
   */
  static public void intToBytes(int val, byte buffer[], int start) {
    buffer[start] = (byte)((val>>24)&0xFF);
    buffer[start+1] = (byte)((val>>16)&0xFF);
    buffer[start+2] = (byte)((val>>8)&0xFF);
    buffer[start+3] = (byte)(val&0xFF);
  }
}
