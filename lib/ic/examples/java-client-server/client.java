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
public class client {

  private static java.lang.String SNode = "client";
  private static java.lang.String PNode = "babbis";
  private static java.lang.String Cookie = "flash";
  private static java.lang.String Server = "rmod_random_impl";
  
  private static rmod._randomStub stub;

  public static void main(String[] args) {

    try {
     
      stub = new rmod._randomStub(SNode,PNode,Cookie,Server);
      int seed1 = 1;
      int seed2 = 2;
      int seed3 = 3;
      double random = 0;
      
      System.out.print("\nClient initialization....");
      stub.init(seed1,seed2,seed3);
      System.out.println("ok\n");
      

      for (int i = 0; i < 10; i++) {
	random = stub.produce();
	System.out.println("Random" + i + " = " + random);
      }
      System.out.println("\nClient terminated.\n");
            
      stub.__disconnect();
      
    } catch( Exception e) {
      System.out.println("Exception :");
      e.printStackTrace();
    }
    
  }
  
}

