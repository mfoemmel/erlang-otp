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

/**
 * Provides a Java representation of Erlang binaries. Anything that
 *  can be represented as a sequence of bytes can be made into an
 *  Erlang binary.
 **/
public class OtpErlangBinary extends OtpErlangObject {
  private byte[] bin;
  
  /**
   * Create a binary from a byte array
   *
   * @param bin the array of bytes from which to create the binary.
   **/
  public OtpErlangBinary(byte[] bin) {
    this.bin = new byte[bin.length];

    System.arraycopy(bin,0,this.bin,0,bin.length);
  }

  /**
   * Create a binary from a stream containinf a binary encoded in
   * Erlang external format.
   *
   * @param buf the stream containing the encoded binary.
   *
   * @exception OtpErlangDecodeException if the buffer does not
   * contain a valid external representation of an Erlang binary.
   **/
  public OtpErlangBinary(OtpInputStream buf) 
    throws OtpErlangDecodeException {
    this.bin = buf.read_binary();
  }

  /**
   * Get the byte array from a binary.
   * 
   * @return the byte array containing the bytes for this binary.
   **/
  public byte[] binaryValue() {
    return bin;
  }
  
  /**
   * Get the size of the binary.
   * 
   * @return the number of bytes contained in the binary.
   **/
  public int size() {
    return bin.length;
  }

  /**
   * Get the string representation of this binary object. A binary is
   * printed as #Bin&lt;N&gt;, where N is the number of bytes
   * contained in the object.
   *
   * @return the Erlang string representation of this binary.
   **/
  public String toString() {
    return "#Bin<" + bin.length + ">";
  }

  /**
   * Convert this binary to the equivalent Erlang external representation.
   *
   * @param buf an output stream to which the encoded binary should be
   * written.
   **/
  public void encode(OtpOutputStream buf) {
    buf.write_binary(this.bin);
  }
  
  /**
   * Determine if two binaries are equal. Binaries are equal if they have
   * the same length and the array of bytes is identical.
   *
   * @param o the object to compare to.
   *
   * @return true if o is a binary and the byte arrays contain the
   * same bytes, false otherwise.
   **/
  public boolean equals(Object o) {
    return false;
  }

  /**
   * Determine if two binaries are equal. Binaries are equal if they have
   * the same length and the array of bytes is identical.
   *
   * @param t the binary to compare to.
   *
   * @return true if the byte arrays contain the same bytes, false
   * otherwise.
   **/
  public boolean equals(OtpErlangBinary bin) {
    int size = this.size();

    if (size != bin.size()) return false;

    for (int i=0; i<size; i++) {
      if (this.bin[i] != bin.bin[i]) return false; // early exit
    }

    return true;
  }
}
