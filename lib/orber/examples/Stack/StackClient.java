/*
 * Stack example.
 */

package StackModule;
import org.omg.CORBA.*;
import org.omg.CORBA.SystemException;
import org.omg.CORBA.ORB.*;

public class StackClient 
{
  public static void main(String args[])
    {
      org.omg.CORBA.Object objRef;
      StackFactory sfRef = null;
      Stack sRef = null;
      // The argument can look like
      // "corbaname::host:4001/#StackFactory"
      String corbaName = new String(args[0]);
      try{
          ORB orb = ORB.init(args, null);

          objRef = orb.string_to_object(corbaName);
          sfRef  = StackFactoryHelper.narrow(objRef);
          sRef   = sfRef.create_stack();

          sRef.push(4);
          sRef.push(7);
          sRef.push(1);
          sRef.push(1);
                   
          try{
              System.out.println(sRef.pop());
              System.out.println(sRef.pop());
              System.out.println(sRef.pop());
              System.out.println(sRef.pop());
              // The following operation shall
              // return an EmptyStack exception
              System.out.println(sRef.pop());
            }
          catch(EmptyStack es) {
              System.out.println("Empty stack");
            };

          sfRef.destroy_stack(sRef);
        }
     catch(SystemException se)
       {
         System.out.println("Unexpected exception: " + se.toString());
         return;
       }
    }
}
