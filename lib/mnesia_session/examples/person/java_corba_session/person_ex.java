//
// Example of how to use the corba interface to access Mnesia
// 
///Imports
import CosNaming._NamingContextRef;
import CosNaming.Name;
import IE.Iona.Orbix2._CORBA;
import IE.Iona.Orbix2.CORBA.*;
///Imports
public class person_ex
{
    public static void main(String args[])
    {
	mnesia._corba_sessionRef mcsRef = null;
	mcsRef = start_corba_session(args);
	if(mcsRef != null)
	{
	    System.out.println("Ok so far so good ");
	    sysinfo(mcsRef);
	    create_person_table(mcsRef);
	    insert_person(mcsRef, "Dan", persons.Sex.male, 
			  27, 97185, "none", new _sequence_String(0));
	    System.out.println("Wrote person Dan ");
	    persons.person p;
	    p = get_person(mcsRef, "Dan");
	    if(p != null)
	    {
		System.out.println("Read person Dan from mnesia");
		System.out.println(" Sex " + p.personData.sex + 
				   " Age " +  p.personData.age +
				   " Phone " + p.personData.phone);
	    }
	}
	else
	    System.out.println("Ok this didn't work try something else!");
    }
///create_table
    public static void create_person_table(mnesia._corba_sessionRef mcsRef)
    {
	String name = "persons";
	mnesia.TableDef def = new mnesia.TableDef();
	def.type = mnesia.SetOrBag.bag;
	def.mode = mnesia.AccessMode.read_write;
	def.ram_copies = new mnesia.NodeList(0);
	def.disc_copies = new mnesia.NodeList(0);
	def.disc_only_copies = new mnesia.NodeList(0);
	mnesia.Indices idxs = new mnesia.Indices(1);
	idxs.buffer[0] = 4;
	def.index_list = idxs;
	mnesia.AttrNames attrs = new mnesia.AttrNames(4);
	attrs.buffer[0] = "name";
	attrs.buffer[1] = "personData";
	attrs.buffer[2] = "married_to";
	attrs.buffer[3] = "children";
	def.attributes = attrs;
	def.record_name = "persons_person"; // The used IDL type

	StringHolder reason;
 	reason = new StringHolder();
	try
	{
	    if(mnesia.Status.ok != mcsRef.create_table(name, def, reason))
		System.out.println("Create Table Error " + reason.value); 
	}
	catch( SystemException se)
	{
	    System.out.println("Unexpected exception: " + se.toString());
	    return;
	} 
    }
///create_table
///dirty_write
    public static void insert_person(mnesia._corba_sessionRef mcsRef,
				     String name,
				     int sex,
				     int age,
				     int phone,
				     String mt,
				     _sequence_String children)
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
	    mnesia.Record object = new mnesia.Record();
	    object.insert(person);

	    if(mnesia.Status.ok != mcsRef.dirty_write("persons", object, reason))
		System.out.println("Insert person Error " + reason.value);
	}
	catch(SystemException se)
	{
	    System.out.println("Unexpected exception: " + se.toString());
	    return;
	}
    }
///dirty_write

///dirty_read
    public static persons.person 
	get_person(mnesia._corba_sessionRef mcsRef, String name)
    {	
	try
	{
	    StringHolder reason = new StringHolder();       
	    mnesia.Key key = new mnesia.Key();
	    mnesia.Recordlist res = new mnesia.Recordlist();
	    key.insertString(name);
		
	    if(mnesia.Status.ok == mcsRef.dirty_read("persons", 
						     key, res, reason))
	    {
		if(res.length > 0) 
		{
		    persons.person rec1 = new persons.person();
		    res.buffer[0].extract(rec1);
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
	catch(SystemException se)
	{
	    System.out.println("Unexpected exception: " + se.toString());
	    return null;
	}	
    }
///dirty_read

///sysinfo
    public static void sysinfo(mnesia._corba_sessionRef mcsRef)
    { 
	StringHolder reason;
	reason = new StringHolder();
	
	mnesia.SystemInfo info = new mnesia.SystemInfo();
	try
	{	    		
	    if(mnesia.Status.ok == mcsRef.system_info(info, reason))
	    {
		System.out.println("------------------------------------------------");
		System.out.println("System Info ");
		System.out.println("Mnesia is_running? " + info.is_running);
		System.out.println("backup_module: " + info.backup_module);
		System.out.println("Directory: " + info.directory);
		System.out.println("Dump Log time time threshold " + 
				   info.dump_log_time_threshold + " ms.");
		System.out.println("------------------------------------------------");
	    }
	    else
		System.out.println("Info Error" + reason.value);
	}
	catch( SystemException se)
	{
	    System.out.println("Unexpected exception: " + se.toString());
	    return;
	}  
    }
///Init
    public static mnesia._corba_sessionRef 
	start_corba_session(String args[])
    {
	mnesia._corba_sessionRef mcsRef = null;
	mnesia._corba_connectorRef mccRef = null;
	CORBA._InitialReferencesRef init;
	_NamingContextRef nsContext;
	Name name;
	_ObjectRef initRef, nsRef, objRef;
	Orber.InitialReference ir = new Orber.InitialReference();
	
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
	    name.buffer[0] = 
		new CosNaming.NameComponent("mnesia_corba_connector", "");
	    try
	    {
		objRef = nsContext.resolve(name);
	    }
	    catch(UserException n)
	    {
		System.out.println("Unexpected exception: " + n.toString());
		return null;
	    }
	    mccRef = mnesia.corba_connector._narrow(objRef);
		
	    // Create and return the session reference
	    mcsRef = mccRef.connect();
	    return mcsRef;
	}
	catch(SystemException se)
	{
	    System.out.println("Unexpected exception: " + se.toString());
	    se.printStackTrace();
	    return null;
	}
    }
///Init
}
