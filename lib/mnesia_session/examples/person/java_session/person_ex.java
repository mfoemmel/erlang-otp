
///Imports
import com.ericsson.otp.ic.*;
///Imports

public class person_ex
{
    public static void main(String args[])
    {
      mnesia._sessionStub mcsRef = null;
      
      mcsRef = start_session(args);
      if(mcsRef != null)
	{
	  sysinfo(mcsRef);
	  create_person_table(mcsRef);
	  insert_person(mcsRef, "Dan", persons.Sex.male, 
			27, 97185, "none", new String[0]);
	  System.out.println("Wrote person Dan ");
	  persons.person p;
	  
	  p = get_person(mcsRef, "Dan");
	  if(p != null)
	    {
	      System.out.println("Read person Dan from mnesia");
	      System.out.println(" Sex " + p.personData.sex.value() + 
				 " Age " +  p.personData.age +
				 " Phone " + p.personData.phone);
	    }
	}
      else
	System.out.println("Ok this didn't work try something else!");
    }

///create_table
  public static void create_person_table(mnesia._sessionStub mcsRef)
    {
      try {
      
	String name = "persons";
	mnesia.TableDef def = new mnesia.TableDef();
	def.type = mnesia.SetOrBag.bag;
	def.mode = mnesia.AccessMode.read_write;
	def.ram_copies = new String[0];
	def.disc_copies = new String[0];
	def.disc_only_copies = new String[0];
	int[] idxs = new int[1];
	idxs[0] = 4;
	def.index_list = idxs;
	String[] attrs = new String[4];
	attrs[0] = "name";
	attrs[1] = "personData";
	attrs[2] = "married_to";
	attrs[3] = "children";
	def.attributes = attrs;
	def.record_name = "persons_person"; // The used IDL type
	
	StringHolder reason;
	reason = new StringHolder();
	
	if(mnesia.Status.ok != mcsRef.create_table(name, def, reason))
	  System.out.println("Create Table Error " + reason.value); 
      }
      catch(Exception se)
	{
	  System.out.println("Unexpected exception: " + se.toString());
	  return;
	} 
    }
///create_table

///dirty_write
  public static void insert_person(mnesia._sessionStub mcsRef,
				   String name,
				   persons.Sex sex,
				   int age,
				   int phone,
				   String mt,
				   java.lang.String[] children)
    {
      persons.data data;
      data = new persons.data(sex, age, phone);
      persons.person person = new persons.person();
      person.name = name;
      person.personData = data;
      person.married_to = mt;
      person.children = children;
      
      try
	{
	  StringHolder reason = new StringHolder();       
	  Term object = new Term();
	  persons.personHelper.insert(object,person);
	  
	  if(mnesia.Status.ok != mcsRef.dirty_write("persons", object, reason))
	    System.out.println("Insert person Error " + reason.value);
	}
      catch(Exception se)
	{
	  System.out.println("Unexpected exception: " + se.toString());
	  return;
	}
    }
///dirty_write
  
///dirty_read
  public static persons.person 
    get_person(mnesia._sessionStub mcsRef, String name)
    {	
      try
	{
	  StringHolder reason = new StringHolder();       
	  Term key = new Term();
	  mnesia.RecordlistHolder res = new mnesia.RecordlistHolder();
	  key.insert_string(name);

	  if(mnesia.Status.ok == mcsRef.dirty_read("persons", 
						   key, res, reason))
	    {
	      if(res.value.length > 0) 
		{
		  persons.person rec1 = 
		    persons.personHelper.extract(res.value[0]);

		  return rec1;
		}
	      else
		return null;			    
	    }
	  else
	    {
	      System.out.println("Insert person Error " + reason.value);
	      return null;
	    }
	}
	catch(Exception se)
	  {
	    System.out.println("Unexpected exception: " + se.toString());
	    return null;
	  }	
    }
///dirty_read
  
///sysinfo
  public static void sysinfo(mnesia._sessionStub mcsRef)
    { 
      StringHolder reason;
      reason = new StringHolder();
	
      mnesia.SystemInfoHolder info = new mnesia.SystemInfoHolder();
      try
	{	    		
	  if(mnesia.Status.ok == mcsRef.system_info(info, reason))
	    {
	      System.out.println("------------------------------------------------");
	      System.out.println("System Info ");
	      System.out.println("Mnesia is_running? " + info.value.is_running);
	      System.out.println("backup_module: " + info.value.backup_module);
	      System.out.println("Directory: " + info.value.directory);
	      System.out.println("Dump Log time time threshold " + 
				   info.value.dump_log_time_threshold + " ms.");
	      System.out.println("------------------------------------------------");
	    }
	  else
	    System.out.println("Info Error" + reason.value);
	}
      catch(Exception se)
	{
	  System.out.println("Unexpected exception: " + se.toString());
	  return;
	}  
    }
///sysinfo

///Init
  public static mnesia._sessionStub start_session(String args[])
    {
      String SNode = new String(args[0]);
      String PNode = new String(args[1]);
      String Cookie = new String(args[2]);

      mnesia._connectorStub mccRef = null;
      mnesia._sessionStub mcsRef = null;
	       
      try
	{
	  
	  mccRef = new mnesia._connectorStub(SNode+"0",PNode,Cookie,
					     "mnesia_connector");
	  
	  com.ericsson.otp.ic.Pid mccPid = mccRef.connect();
	  
	  mcsRef = new mnesia._sessionStub(SNode+"1",PNode,Cookie,mccPid);

	  return mcsRef;
	}
      catch(Exception se)
	{
	  System.out.println("Unexpected exception: " + se.toString());
	  se.printStackTrace();
	  return null;
	}
    }
///Init

}




