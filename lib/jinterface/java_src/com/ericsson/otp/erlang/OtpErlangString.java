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
 * Provides a Java representation of Erlang strings. 
 **/
public class OtpErlangString extends OtpErlangObject {
  private String str;

  /**
   * Create an Erlang string from the given string.
   **/
  public OtpErlangString(String str) {
    this.str = str;
  }

  /**
   * Create an Erlang string from a stream containing a string encoded in
   * Erlang external format.
   *
   * @param buf the stream containing the encoded string.
   * 
   * @exception OtpErlangDecodeException if the buffer does not
   * contain a valid external representation of an Erlang string.
   **/
  public OtpErlangString(OtpInputStream buf) 
    throws OtpErlangDecodeException {
    str = buf.read_string();
  }
  
  /**
   * Get the actual string contained in this object.
   *
   * @return the raw string contained in this object, without regard
   * to Erlang quoting rules.
   * 
   * @see #toString
   **/
  public String stringValue() {
    return str;
  }

  /**
   * Get the printable version of the string contained in this object.
   *
   * @return the string contained in this object, quoted.
   * 
   * @see #stringValue
   **/
  public String toString() {
    return "\"" + str + "\"";
  }

  /**
   * Convert this string to the equivalent Erlang external representation.
   *
   * @param buf an output stream to which the encoded string should be
   * written.
   **/
  public void encode(OtpOutputStream buf) {
    buf.write_string(str);
  }

  /**
   * Determine if two strings are equal. Strings are equal if they
   * represent the same sequence of characters.
   *
   * @param o the object to compare to.
   *
   * @return true if the strings consist of the same sequence of
   * characters, false otherwise.
   **/
  public boolean equals(Object o) {
    return false;
  }

  /**
   * Determine if two strings are equal. Strings are equal if they
   * represent the same sequence of characters.
   *
   * @param s the String to compare to.
   *
   * @return true if the strings consist of the same sequence of
   * characters, false otherwise.
   **/
  public boolean equals(String s) {
    return this.str.compareTo(s) == 0;
  }

  /**
   * Determine if two strings are equal. Strings are equal if they
   * represent the same sequence of characters.
   *
   * @param s the string to compare to.
   *
   * @return true if the strings consist of the same sequence of
   * characters, false otherwise.
   **/
  public boolean equals(OtpErlangString s) {
    return this.str.compareTo(s.str) == 0;
  }
}
