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
package com.ericsson.otp.ic;

/**
  Holder class for Port.
  **/

final public class PortHolder {

  /**
    Port instance variable.
    **/
   public Port value;

   // constructors
   public PortHolder() {}
   public PortHolder(Port initial) {
      value = initial;
   }

   // methods
  /**
    Marshal method for the PortHolder class, encodes the Port object value to the output stream.
    **/
   public void _marshal(com.ericsson.otp.erlang.OtpOutputStream out) 
     throws java.lang.Exception {
      PortHelper.marshal(out, value);
   }

  /**
    Unmarshal method for the PortHolder class, decodes a Port object from the output stream
    and assigns it to the Holder value field.
    **/
   public void _unmarshal(com.ericsson.otp.erlang.OtpInputStream in) 
     throws java.lang.Exception {
      value = PortHelper.unmarshal(in);
   }
}
