package StackModule;

import CosNaming._NamingContextRef;
import CosNaming.Name;
import IE.Iona.Orbix2._CORBA;
import IE.Iona.Orbix2.CORBA.SystemException;
import IE.Iona.Orbix2.CORBA._ObjectRef;

public class StackClient 
{
  public static void main(String args[])
    {
      CORBA._InitialReferencesRef init;
      _NamingContextRef nsContext;
      Name name;
      _ObjectRef initRef, nsRef, objRef;
      _StackFactoryRef sfRef = null;
      _StackRef sRef = null;
      Orber.InitialReference ir = new Orber.InitialReference();

      int i;
      String srvHost = new String(args[0]);
      Integer srvPort = new Integer(args[1]);

      try
	{
	  // For an explanation about initial reference handling see 
	  // the "Interoperable Naming Service" specification.

	  // Create Initial reference (objectkey "INIT").
	  String s = ir.stringified_ior(srvHost, srvPort.intValue());

	  initRef = _CORBA.Orbix.string_to_object(s);

	  init = CORBA.InitialReferences._narrow(initRef);
	  // Fetch name service reference.
	  nsRef = init.get("NameService");

          nsContext = CosNaming.NamingContext._narrow(nsRef);

	  // Create a name
	  name = new Name(1);
	  name.buffer[0] = new CosNaming.NameComponent("StackFactory", "");

	  try
	    {
	      objRef = nsContext.resolve(name);
	    }
	  catch(IE.Iona.Orbix2.CORBA.UserException n)
	    {
	      System.out.println("Unexpected exception: " + n.toString());
	      return;
	    }	    
	  sfRef = StackFactory._narrow(objRef);
	  
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
