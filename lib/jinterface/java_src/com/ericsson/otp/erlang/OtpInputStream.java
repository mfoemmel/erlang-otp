/* ``The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved via the world wide web at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */
package com.ericsson.otp.erlang;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.text.DecimalFormat;
import java.math.BigDecimal;

/**
 * Provides a stream for decoding Erlang terms from external format.
 *
 * <p> Note that this class is not synchronized, if you need
 * synchronization you must provide it yourself.
 **/
public class OtpInputStream extends ByteArrayInputStream {
  /**
   * Create a stream from a buffer containing encoded Erlang terms.
   **/
  public OtpInputStream(byte[] buf) {
    super(buf);
  }

  /**
   * Create a stream from a buffer containing encoded
   * Erlang terms at the given offset and length.
   **/
  public OtpInputStream(byte[] buf, int offset, int length) {
    super(buf,offset,length);
  }

  /**
   * Get the current position in the stream.
   *
   * @return the current position in the stream.
   **/
  public int getPos() {
    return super.pos;
  }
  
  /**
   * Set the current position in the stream.
   *
   * @param pos the position to move to in the stream. If pos
   * indicates a position beyond the end of the stream, the position
   * is move to the end of the stream instead. If pos is negative, the
   * position is moved to the beginning of the stream instead.
   *
   * @return the previous position in the stream.
   **/
  public int setPos(int pos) {
    int oldpos = super.pos;
    
    if (pos > super.count) pos = super.count;
    else if (pos < 0) pos = 0;

    super.pos = pos;

    return oldpos;
  }

  /**
   * Read an array of bytes from the stream. The method reads at most
   * buf.length bytes from the input stream.
   *
   * @return the number of bytes read.
   *
   * @exception OtpErlangDecodeException if the next byte cannot be
   * read.
   **/
  public int readN(byte[] buf) 
    throws OtpErlangDecodeException {
    try {
      return super.read(buf);
    }
    catch (IOException e) {
      throw new OtpErlangDecodeException("Cannot read from input stream");
    }
  }
  
  /**
   * Look ahead one position in the stream without consuming the byte
   * found there.
   *
   * @return the next byte in the stream, as an integer.
   *
   * @exception OtpErlangDecodeException if the next byte cannot be
   * read.
   **/
  public int peek()
    throws OtpErlangDecodeException {
    int i;
    try {
      i = super.buf[super.pos];
      if (i<0) i+= 256;
      
      return i;
    }
    catch (Exception e) {
      throw new OtpErlangDecodeException("Cannot read from input stream");
    }
  }

  /**
   * Read a one byte integer from the stream.
   *
   * @return the byte read, as an integer.
   * 
   * @exception OtpErlangDecodeException if the next byte cannot be
   * read.
   **/
  public int read1()
    throws OtpErlangDecodeException {
    int i;
    i = super.read();

    if (i<0) {
      throw new OtpErlangDecodeException("Cannot read from input stream");
    }

    return i;
  }
  
  /**
   * Read a two byte big endian integer from the stream.
   *
   * @return the bytes read, converted from big endian to an integer.
   * 
   * @exception OtpErlangDecodeException if the next byte cannot be
   * read.
   **/
  public int read2BE() 
    throws OtpErlangDecodeException {
    byte[] b = new byte[2];
    try {
      super.read(b);
    }
    catch (IOException e) {
      throw new OtpErlangDecodeException("Cannot read from input stream");
    }; 
    return (((  (int)b[0] << 8) & 0xff00)
	    + (((int)b[1]     ) & 0xff));
  }

  /**
   * Read a four byte big endian integer from the stream.
   *
   * @return the bytes read, converted from big endian to an integer.
   * 
   * @exception OtpErlangDecodeException if the next byte cannot be
   * read.
   **/
  public int read4BE() 
    throws OtpErlangDecodeException {
    byte[] b = new byte[4];
    try {
      super.read(b);
    }
    catch (IOException e) {
      throw new OtpErlangDecodeException("Cannot read from input stream");
    }; 
    return (((  (int)b[0] << 24) & 0xff000000)
	    + (((int)b[1] << 16) & 0xff0000)
	    + (((int)b[2] <<  8) & 0xff00)
	    + (((int)b[3]      ) & 0xff));
  }

  /**
   * Read a two byte little endian integer from the stream.
   *
   * @return the bytes read, converted from little endian to an
   * integer.
   * 
   * @exception OtpErlangDecodeException if the next byte cannot be
   * read.
   **/
  public int read2LE() 
    throws OtpErlangDecodeException {
    byte[] b = new byte[2];
    try {
      super.read(b);
    }
    catch (IOException e) {
      throw new OtpErlangDecodeException("Cannot read from input stream");
    }; 
    return (((  (int)b[1] << 8) & 0xff00)
	    + (((int)b[0]     ) & 0xff));
  }

  /**
   * Read a four byte little endian integer from the stream.
   *
   * @return the bytes read, converted from little endian to an
   * integer.
   * 
   * @exception OtpErlangDecodeException if the next byte cannot be
   * read.
   **/
  public int read4LE() 
    throws OtpErlangDecodeException {
    byte[] b = new byte[4];
    try {
      super.read(b);
    }
    catch (IOException e) {
      throw new OtpErlangDecodeException("Cannot read from input stream");
    }; 
    return (((  (int)b[3] << 24) & 0xff000000)
	    + (((int)b[2] << 16) & 0xff0000)
	    + (((int)b[1] <<  8) & 0xff00)
	    + (((int)b[0]      ) & 0xff));
  }

  /**
   * Read an Erlang atom from the stream and interpret the value as a
   * boolean.
   *
   * @return true if the atom at the current position in the stream
   * contains the value 'true' (ignoring case), false otherwise.
   * 
   * @exception OtpErlangDecodeException if the next term in the
   * stream is not an atom.
   **/
  public boolean read_boolean() 
    throws OtpErlangDecodeException {
    return Boolean.valueOf(this.read_atom()).booleanValue();
  }

  /**
   * Read an Erlang atom from the stream.
   *
   * @return a String containing the value of the atom.
   * 
   * @exception OtpErlangDecodeException if the next term in the
   * stream is not an atom.
   **/
  public String read_atom() 
    throws OtpErlangDecodeException {
    int tag;
    int len;
    byte[] strbuf;
    String atom;

    tag = this.read1();
    if (tag == OtpExternal.versionTag) {
      tag = this.read1();
    }

    if (tag != OtpExternal.atomTag) {
      throw new OtpErlangDecodeException("wrong tag encountered, expected "
					 + OtpExternal.atomTag + ", got " + tag);
    }

    len = this.read2BE();

    strbuf = new byte[len];
    this.readN(strbuf);
    atom = new String(strbuf);

    if (atom.length() > OtpExternal.maxAtomLength) {
      atom = atom.substring(0,OtpExternal.maxAtomLength);
    }

    return atom;
  }

  /**
   * Read an Erlang binary from the stream.
   *
   * @return a byte array containing the value of the binary.
   * 
   * @exception OtpErlangDecodeException if the next term in the
   * stream is not a binary.
   **/
  public byte[] read_binary() 
    throws OtpErlangDecodeException {
    int tag;
    int len;
    byte[] bin;
    
    tag = this.read1();
    if (tag == OtpExternal.versionTag) {
      tag = this.read1();
    }

    if (tag != OtpExternal.binTag) {
      throw new OtpErlangDecodeException("Wrong tag encountered, expected "
					 + OtpExternal.binTag + ", got " + tag);
    }

    len = this.read4BE();

    bin = new byte[len];
    this.readN(bin);

    return bin;
  }

  /**
   * Read an Erlang float from the stream.
   *
   * @return the float value.
   * 
   * @exception OtpErlangDecodeException if the next term in the
   * stream is not a float.
   **/
  public float read_float() 
    throws OtpErlangDecodeException {
    BigDecimal val = getFloatOrDouble();

    return val.floatValue();

    /*
     *
     * double d = this.read_double();
     * float f = (float) d;
     *
     * if (java.lang.Math.abs(d - f) >= 1.0E-20)
     * throw new OtpErlangDecodeException("Value cannot be represented as float: " + d);
     *
     * return f;
     */
  }

  /**
   * Read an Erlang float from the stream.
   *
   * @return the float value, as a double.
   * 
   * @exception OtpErlangDecodeException if the next term in the
   * stream is not a float.
   **/
  public double read_double() 
    throws OtpErlangDecodeException {
    BigDecimal val = getFloatOrDouble();

    return val.doubleValue();
  }

  private BigDecimal getFloatOrDouble()
    throws OtpErlangDecodeException {
    BigDecimal val;
    int epos;
    int exp;
    byte[] strbuf = new byte[31];
    String str;
    int tag;

    // parse the stream
    tag = this.read1();
    if (tag == OtpExternal.versionTag) {
      tag = this.read1();
    }

    if (tag != OtpExternal.floatTag) {
      throw new OtpErlangDecodeException("Wrong tag encountered, expected "
					 + OtpExternal.floatTag + ", got " + tag);
    }

    // get the string
    this.readN(strbuf);
    str = new String(strbuf);

    // find the exponent prefix 'e' in the string
    epos = str.indexOf('e',0);

    if (epos < 0) {
      throw new OtpErlangDecodeException("Invalid float format: '" + str + "'");
    }
    
    // remove the sign from the exponent, if positive
    String estr = str.substring(epos+1).trim();
    
    if (estr.substring(0,1).equals("+")) {
      estr = estr.substring(1);
    }
    
    // now put the mantissa and exponent together
    exp = Integer.valueOf(estr).intValue();
    val = new BigDecimal(str.substring(0,epos)).movePointRight(exp);
    
    return val;
  }

  /**
   * Read one byte from the stream.
   *
   * @return the byte read.
   * 
   * @exception OtpErlangDecodeException if the next byte cannot be
   * read.
   **/
  public byte read_byte() 
    throws OtpErlangDecodeException {
    long l = this.read_long();
    byte i = (byte) l;

    if (l != i) {
      throw new OtpErlangDecodeException("Value too large for byte: " + l);
    }

    return i;
  }

  /**
   * Read a character from the stream.
   *
   * @return the character value.
   * 
   * @exception OtpErlangDecodeException if the next term in the
   * stream is not an integer that can be represented as a char.
   **/
  public char read_char() 
    throws OtpErlangDecodeException {
    long l = this.read_long();
    char i = (char) l;

    if (l != i) {
      throw new OtpErlangDecodeException("Value too large for byte: " + l);
    }

    return i;
  }

  /**
   * Read an unsigned integer from the stream.
   *
   * @return the integer value.
   * 
   * @exception OtpErlangDecodeException if the next term in the
   * stream can not be represented as a positive integer.
   **/
  public int read_uint() 
    throws OtpErlangDecodeException {
    long l = this.read_long();
    int i = (int) l;

    if (l != i) {
      throw new OtpErlangDecodeException("Value too large for integer: " + l);
    }
    else if (l < 0) {
      throw new OtpErlangDecodeException("Value not unsigned: " + l);
    }

    return i;
  }

  /**
   * Read an integer from the stream.
   *
   * @return the integer value.
   * 
   * @exception OtpErlangDecodeException if the next term in the
   * stream can not be represented as an integer.
   **/
  public int read_int() 
    throws OtpErlangDecodeException {
    long l = this.read_long();
    int i = (int) l;

    if (l != i) {
      throw new OtpErlangDecodeException("Value too large for byte: " + l);
    }

    return i;
  }

  /**
   * Read an unsigned short from the stream.
   *
   * @return the short value.
   * 
   * @exception OtpErlangDecodeException if the next term in the
   * stream can not be represented as a positive short.
   **/
  public short read_ushort() 
    throws OtpErlangDecodeException {
    long l = this.read_long();
    short i = (short) l;

    if (l != i) {
      throw new OtpErlangDecodeException("Value too large for byte: " + l);
    }
    else if (l < 0) {
      throw new OtpErlangDecodeException("Value not unsigned: " + l);
    }

    return i;
  }

  /**
   * Read a short from the stream.
   *
   * @return the short value.
   * 
   * @exception OtpErlangDecodeException if the next term in the
   * stream can not be represented as a short.
   **/
  public short read_short() 
    throws OtpErlangDecodeException {
    long l = this.read_long();
    short i = (short) l;

    if (l != i) {
      throw new OtpErlangDecodeException("Value too large for byte: " + l);
    }

    return i;
  }
  
  /**
   * Read an unsigned long from the stream.
   *
   * @return the long value.
   * 
   * @exception OtpErlangDecodeException if the next term in the
   * stream can not be represented as a positive long.
   **/
  public long read_ulong() 
    throws OtpErlangDecodeException {
    long l = this.read_long();

    if (l < 0) {
      throw new OtpErlangDecodeException("Value not unsigned: " + l);
    }
    
    return l;
  }

  /**
   * Read a long from the stream.
   *
   * @return the long value.
   * 
   * @exception OtpErlangDecodeException if the next term in the
   * stream can not be represented as a long.
   **/
  public long read_long() 
    throws OtpErlangDecodeException {
    int tag;
    int len;
    int sign;
    int arity;
    long val;

    tag = this.read1();
    if (tag == OtpExternal.versionTag) {
      tag = this.read1();
    }

    switch (tag) {
    case OtpExternal.smallIntTag:
      val = this.read1();
      break;
      
    case OtpExternal.intTag:
      val = this.read4BE();
      break;

    case OtpExternal.smallBigTag:
      arity = this.read1(); 

      if (arity != 4) {
	throw new OtpErlangDecodeException("Arity for smallBig must be 4, was " + arity);
      }
      
      sign = this.read1();

      // obs! little endian here
      val = this.read4LE();
      val = (sign == 0 ? val : -val); // should deal with overflow

      break;
      
    case OtpExternal.largeBigTag:
    default:
      throw new OtpErlangDecodeException("Not valid integer tag: " + tag);
    }

    return val;
  }

  /**
   * Read a list header from the stream.
   *
   * @return the arity of the list.
   * 
   * @exception OtpErlangDecodeException if the next term in the
   * stream is not a list.
   **/
  public int read_list_head() 
    throws OtpErlangDecodeException {
    int arity = 0;
    int tag = this.read1();
    
    if (tag == OtpExternal.versionTag) {
      tag = this.read1();
    }

    switch (tag) {
    case OtpExternal.nilTag:
      arity = 0;
      break;

    case OtpExternal.stringTag:
      arity = this.read2BE();
      break;

    case OtpExternal.listTag:
      arity = this.read4BE();
      break;

    default:
      throw new OtpErlangDecodeException("Not valid list tag: " + tag);
    }

    return arity;
  }

  /**
   * Read a tuple header from the stream.
   *
   * @return the arity of the tuple.
   * 
   * @exception OtpErlangDecodeException if the next term in the
   * stream is not a tuple.
   **/
  public int read_tuple_head() 
    throws OtpErlangDecodeException {
    int arity = 0;
    int tag = this.read1();
    
    if (tag == OtpExternal.versionTag) {
      tag = this.read1();
    }

    // decode the tuple header and get arity
    switch (tag) {
    case OtpExternal.smallTupleTag:
      arity = this.read1();
      break;

    case OtpExternal.largeTupleTag:
      arity = this.read4BE();
      break;

    default:
      throw new OtpErlangDecodeException("Not valid tuple tag: " + tag);
    }

    return arity;
  }

  /**
   * Read an empty list from the stream.
   *
   * @return zero (the arity of the list).
   * 
   * @exception OtpErlangDecodeException if the next term in the
   * stream is not an empty list.
   **/
  public int read_nil() 
    throws OtpErlangDecodeException {
    int arity = 0;
    int tag = this.read1();
    
    if (tag == OtpExternal.versionTag) {
      tag = this.read1();
    }

    switch (tag) {
    case OtpExternal.nilTag:
      arity = 0;
      break;

    default:
      throw new OtpErlangDecodeException("Not valid nil tag: " + tag);
    }

    return arity;
  }

  /**
   * Read an Erlang PID from the stream.
   *
   * @return the value of the PID.
   * 
   * @exception OtpErlangDecodeException if the next term in the
   * stream is not an Erlang PID.
   **/
  public OtpErlangPid read_pid() 
    throws OtpErlangDecodeException {
    String node;
    int id;
    int serial;
    int creation;
    int tag;

    tag = this.read1();
    if (tag == OtpExternal.versionTag) {
      tag = this.read1();
    }

    if (tag != OtpExternal.pidTag) {
      throw new OtpErlangDecodeException("Wrong tag encountered, expected "
					 + OtpExternal.pidTag + ", got " + tag);
    }

    node = this.read_atom();
    id = this.read4BE() & 0x7fff; // 15 bits
    serial = this.read4BE() & 0x07; // 3 bits
    creation = this.read1() & 0x03; // 2 bits
    
    return new OtpErlangPid(node, id, serial, creation);
  }

  /**
   * Read an Erlang port from the stream.
   *
   * @return the value of the port.
   * 
   * @exception OtpErlangDecodeException if the next term in the
   * stream is not an Erlang port.
   **/
  public OtpErlangPort read_port() 
    throws OtpErlangDecodeException {
    String node;
    int id;
    int creation;
    int tag;

    tag = this.read1();
    if (tag == OtpExternal.versionTag) {
      tag = this.read1();
    }

    if (tag != OtpExternal.portTag) {
      throw new OtpErlangDecodeException("Wrong tag encountered, expected "
					 + OtpExternal.portTag + ", got " + tag);
    }

    node = this.read_atom();
    id = this.read4BE() & 0x3ffff; // 18 bits
    creation = this.read1() & 0x03; // 2 bits
    
    return new OtpErlangPort(node, id, creation);
  }

  /**
   * Read an Erlang reference from the stream.
   *
   * @return the value of the reference
   * 
   * @exception OtpErlangDecodeException if the next term in the
   * stream is not an Erlang reference.
   **/
  public OtpErlangRef read_ref() 
    throws OtpErlangDecodeException {
    String node;
    int id;
    int creation;
    int tag;

    tag = this.read1();
    if (tag == OtpExternal.versionTag) {
      tag = this.read1();
    }

    switch (tag) {
    case OtpExternal.refTag:
      node = this.read_atom();
      id = this.read4BE() & 0x3ffff; // 18 bits
      creation = this.read1() & 0x03; // 2 bits
      return new OtpErlangRef(node, id, creation);
      
    case OtpExternal.newRefTag:
      int arity = this.read2BE();
      node = this.read_atom();
      creation = this.read1() & 0x03; // 2 bits

      int[] ids = new int[arity];
      for (int i=0; i<arity; i++) {
	ids[i] = this.read4BE();
      }
      ids[0] &= 0x3ffff; // first id gets truncated to 18 bits
      return new OtpErlangRef(node, ids, creation);
      
    default:
      throw new OtpErlangDecodeException("Wrong tag encountered, expected ref, got " + tag);
    }
  }

  /**
   * Read a string from the stream.
   *
   * @return the value of the string.
   * 
   * @exception OtpErlangDecodeException if the next term in the
   * stream is not a string.
   **/
  public String read_string() 
    throws OtpErlangDecodeException {
    int tag;
    int len;
    byte[] strbuf;
    char[] charbuf;
    
    tag = this.read1();
    if (tag == OtpExternal.versionTag) {
      tag = this.read1();
    }

    switch(tag) {
      
    case OtpExternal.stringTag:
      len = this.read2BE();
      strbuf = new byte[len];
      this.readN(strbuf);
      return new String(strbuf); 

    case OtpExternal.nilTag:
      return "";

    case OtpExternal.listTag: // List when unicode +
      len = this.read4BE();
      charbuf = new char[len];
      
      for (int i=0; i<len; i++)
	charbuf[i] = this.read_char();
      
      this.read_nil();
      return new String(charbuf);

    default:
      throw new OtpErlangDecodeException("Wrong tag encountered, expected "+ 
					 OtpExternal.stringTag +
					 " or " + 
					 OtpExternal.listTag +
					 ", got " + tag);
    }
  }

  /**
   * Read an arbitrary Erlang term from the stream.
   *
   * @return the Erlang term.
   * 
   * @exception OtpErlangDecodeException if the stream does not
   * contain a known Erlang type at the next position.
   **/
  public OtpErlangObject read_any() 
    throws OtpErlangDecodeException {
    // calls one of the above functions, depending on o
    int tag = this.peek();
    if (tag == OtpExternal.versionTag) {
      this.read1();
      tag = this.peek();      
    }
    
    switch (tag) {
    case OtpExternal.smallIntTag:
    case OtpExternal.intTag:
    case OtpExternal.smallBigTag:
      return new OtpErlangLong(this);
      
    case OtpExternal.atomTag:
      return new OtpErlangAtom(this);
      
    case OtpExternal.floatTag:
      return new OtpErlangDouble(this);
      
    case OtpExternal.refTag:
    case OtpExternal.newRefTag:
      return  new OtpErlangRef(this);
      
    case OtpExternal.portTag:
      return new OtpErlangPort(this);
      
    case OtpExternal.pidTag:
      return new OtpErlangPid(this);
      
    case OtpExternal.stringTag:
      return new OtpErlangString(this);
      
    case OtpExternal.listTag:
    case OtpExternal.nilTag:
      return new OtpErlangList(this);
      
    case OtpExternal.smallTupleTag:
    case OtpExternal.largeTupleTag:
      return new OtpErlangTuple(this);

    case OtpExternal.binTag:
      return new OtpErlangBinary(this);

    case OtpExternal.largeBigTag:
    default:
      throw new OtpErlangDecodeException("Uknown data type: " + tag);
    }
  }
}

