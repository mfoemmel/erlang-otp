/*
 * Stack example.
 */

package StackModule;
import org.omg.CosNaming.*;
import org.omg.CORBA.*;
import org.omg.CORBA.SystemException;
import org.omg.CORBA.ORB.*;

public class StackClient 
{
  public static void main(String args[])
    {
      NamingContext nsContext;
      org.omg.CORBA.Object objRef;
      StackFactory sfRef = null;
      Stack sRef = null;
      org.omg.CORBA.Object nsRef, initRef;
      NameComponent[] name = new NameComponent[1];
      Orber.InitialReference ir = new Orber.InitialReference();
      Orber.InitialReferences init;
      String srvHost = new String(args[0]);
      Integer srvPort = new Integer(args[1]);

      try
	{
	  ORB orb = ORB.init(args, null);

	  // Create Initial reference (objectkey "INIT").
	   String s = ir.stringified_ior(srvHost, srvPort.intValue());
	   initRef = orb.string_to_object(s);
	   init = Orber.InitialReferencesHelper.narrow(initRef);

	   // Fetch name service reference.
	   nsRef = init.get("NameService");
	   nsContext = NamingContextHelper.narrow(nsRef);
	   // Create a name
	   name[0] = new NameComponent("StackFactory", "");

	   try
	     {
	       objRef = nsContext.resolve(name);
	     }
	   catch(Exception n)
	     {
	       System.out.println("Unexpected exception: " + n.toString());
	       return;
	     }	    
	   
	  sfRef = StackFactoryHelper.narrow(objRef);
 	  sRef = sfRef.create_stack();

	  sRef.push(4);
	  sRef.push(7);
	  sRef.push(1);
	  sRef.push(1);
	      
	  try
	    {
	      System.out.println(sRef.pop());
	      System.out.println(sRef.pop());
	      System.out.println(sRef.pop());
	      System.out.println(sRef.pop());
	      // The following operation shall return an EmptyStack exception
	      System.out.println(sRef.pop());
	    }
	  catch(EmptyStack es)
	    {
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
