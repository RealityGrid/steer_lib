//package ReG_Steer_Proxy;

import java.io.*;
import java.util.Vector;
import java.util.Date;
import javax.swing.*;

import electric.util.Context;
import electric.registry.Registry;
import electric.registry.RegistryException;

// These interfaces are used for convienence. A tooling engine could generate
// them from the WSDL for the GridServices.

import com.fujitsu.arcon.gridservice.IRegistry;
import com.fujitsu.arcon.gridservice.IHandleMap;
import com.fujitsu.arcon.gridservice.IGridServiceFactory;
import com.fujitsu.arcon.gridservice.ISteeringGridService;
import com.fujitsu.arcon.gridservice.INotificationService;
import com.fujitsu.arcon.gridservice.GridServiceException;

// This object will be replaced by the XML representations so that this
// client does not rely on the Java basis of the GridService, i.e. I want to
// make the language neutral aspect of GridServices more explicit.

import com.fujitsu.arcon.gridservice.GridServiceReference;

public class ReG_Steer_Proxy {

    private static final int REG_SUCCESS = 0;
    private static final int REG_FAILURE = 1;
    private static IRegistry		gs_registry	 = null;
    private static IHandleMap		handlemap	 = null;
    private static IGridServiceFactory	factory		 = null;
    private static Context 		context 	 = null;
    // The application being steered
    private static ISteeringGridService	app		 = null;

    private static String recvd_msg;

    //-------------------------------------------------------
   
    public static void main ( String[] args) throws Exception {

       int    nparam;
       int    fd;
       String s;

       ReG_Steer_Proxy proxy = new ReG_Steer_Proxy();

       try{
	   proxy.init();

	   // Process that launched us waits to make sure we're OK so
	   // let it know...
	   proxy.sendMessage("INIT_OK");

	   proxy.start();

       }
       catch(Exception e){

	   proxy.sendMessage("ERROR");
       }

       System.exit(0);
    }

    //-------------------------------------------------------

    private int init() throws Exception {

	// ** 1.0 Set up authentication framework if required.
        
        // Check if we are running over https and set up a security context
        // to use with the https binding. With an http binding, no authentication
        // is performed.
        
        context = new Context();

	/*
        if (System.getProperty("registry.gsh").substring(0,6).equals("https:")) {

	    UserDetailsInput details_input = new UserDetailsInput( "Grid Service "
								   +"authentication" );
	    UserDetailsInput.UserDetails user_details;

	    user_details = details_input.get_input();
			
	    context.addProperty("authUser", user_details.user_name);
	    context.addProperty("authPassword", user_details.password);
        }
	*/

        // 2.0 Set up Grid Services environment.
        
        // 2.1 Start Registry. This will also start the handleMap.
        
        String registry_gsh = System.getProperty("registry.gsh");

        if( startRegistry(registry_gsh) != REG_SUCCESS){

	    throw new Exception ("problems in startRegistry...");
	}

        String registry_contents = gs_registry.registryQuery();
        System.err.println ("Registry Contents : \n" + registry_contents);
        
        // 2.2 Find and start a Grid Service Factory. There will be only one.
        
        String factory_gsh = "No GSH";
        int i_start = registry_contents.indexOf("GSH");

        while (factory_gsh.indexOf("Factory") < 0) {

            i_start = registry_contents.indexOf("value", i_start)+5;
            i_start = registry_contents.indexOf("'", i_start)+1;
            int i_end   = registry_contents.indexOf("'", i_start);
            factory_gsh = registry_contents.substring(i_start,i_end);
        }
        startFactory (factory_gsh);
	
	return REG_SUCCESS;
    }

    //-------------------------------------------------------

    private String[] parseRegistryContents(String contents) {

	int      index;
	int      index2;
	int      count = 0;
	// Pull GSHs out of string containing the registry contents

	// Count number of GSH's in contents string
	index = contents.indexOf("GSH");

	while(index != -1){

	    index = contents.indexOf("GSH", index+1);
	    count++;
	}

	if(count == 0) return null;

	// Allocate memory to store this no. of GSH's
	String[] gsh_string_array = new String[count];

	// Pull the GSH's out of the contents string
	count = 0;
	index = contents.indexOf("GSH");

	while(index != -1){


	    index  = contents.indexOf("'", index+1);
	    index++;
	    index2 = contents.indexOf("'", index);

	    gsh_string_array[count++] = new String(contents.substring(index, 
								      index2));

	    index = contents.indexOf("GSH", index2);
	}
	
	return gsh_string_array;
    }

    //-------------------------------------------------------

    private int attach() {

	// Get message from steerer giving GSH to attach to
	if(getMessage() == REG_FAILURE){

	    System.err.println("attach: failed to get GSH from steerer");
	    return REG_FAILURE;
	}

	try{
	    // (handlemapper started when registry started)
	    GridServiceReference gsr = handlemap.findByHandle(recvd_msg);
	    String endpoint = gsr.getEndpoint();

            app = (ISteeringGridService) Registry.bind (endpoint, 
						    ISteeringGridService.class,
							context);
	}
	catch(Exception e){

	    return REG_FAILURE;
	}

	return REG_SUCCESS;
    }

    //-------------------------------------------------------

    private int detach() {

	// Not sure if this routine really required yet...

	// Get message from steerer giving GSH to detach from
	if(getMessage() == REG_FAILURE){

	    System.err.println("detach: failed to get GSH from steerer");
	    return REG_FAILURE;
	}

	
	return REG_SUCCESS;
    }

    //-------------------------------------------------------

    private void start() throws IOException {

	int  status;

	System.err.println("Entered Start method...");

	while(true){

	    if((status = getMessage()) == REG_FAILURE) break;

	    if (recvd_msg.trim().compareTo("QUIT") == 0){

		break;
	    }
	    else if (recvd_msg.trim().compareTo("GET_APPS") == 0){

		getApps();		
	    }
	    else if (recvd_msg.trim().compareTo("GET_STATUS") == 0){

		getStatus();
	    }
	    else if (recvd_msg.trim().compareTo("SEND_CTRL") == 0){

		sendControl();
	    }
	    else if (recvd_msg.trim().compareTo("ATTACH") == 0){

		attach();
	    }
	    else if (recvd_msg.trim().compareTo("DETACH") == 0){

		detach();
	    }
	}
    }

    //-------------------------------------------------------

    private int getMessage() {

	StringBuffer buf = new StringBuffer();
	int    c;
	int    len;
	int    i;
	byte[] b;

	try{

	    // Wait for a message telling us how many bytes
	    // to receive
	    while( (c = System.in.read()) == 0){

		Thread.sleep(250);
	    }

	    if(c == -1){
		return 1;
	    }

	    // Allocate memory for message

	    b = new byte[c];

	    // Read the specified no. of bytes

	    System.in.read(b, 0, c);

	    // Convert from bytes to a string

	    if( (len = buf.length()) > 0){

		buf.delete(0, len);
	    }

	    buf.append((char)(b[0]));
	    for(i=1; i<c; i++){

		buf.append((char)(b[i]));
	    }

	    // Set class member variable to msg just received
	    recvd_msg = buf.toString();

	    System.err.println("Got string: " + recvd_msg);

	    return 0;

	}
	catch(IOException e){

	    return 1;
	}
	catch(InterruptedException e){

	    return 1;
	}
    }

    //-------------------------------------------------------

    private int sendMessage(String msg) throws NegativeArraySizeException {

	int     len;
       	byte[]  b;

	try{
	// Send message to process that launched this one using the
        // (redirected) stdout pipe

	// Get no. of characters in string
	len = msg.length();
	System.err.println("Length of msg = " + len);

	// A char is 16 bits, byte is 8 bits so might need twice as many
	// bytes as characters
	b = new byte[2*len];

	b = msg.getBytes();
	len = b.length;

	// Send length - use DataOutputStream in order to attempt
	// to ensure that we send four bytes
	DataOutputStream outstream = new DataOutputStream(System.out);

	outstream.writeInt(len);

	// Send string itself
	System.out.write(b, 0, len);
	}
	catch(Exception e){

	    return 1;
	}

	return 0;
    }

    //-------------------------------------------------------
    /*
     * Start the handle mapper, if necessary.
     */
    private static int startHandleMap () {
    
        if (handlemap == null ) { // First time there is no handle 
	                          // mapper accessable.
            try {
            
                // We can construct the Home Handle Mapper endpoint from the 
		// Registry GSH.
                
                String gsh = System.getProperty("registry.gsh");
                String mapper_endpoint = gsh.substring(0,gsh.lastIndexOf('/'))+
		    "/handleMap.wsdl";

                handlemap = (IHandleMap) Registry.bind (mapper_endpoint, 
							IHandleMap.class, 
							context);
            }
            catch (RegistryException ex) {

                System.err.println ("startHandleMap: problems connecting to "
				    +"handle mapper: "+ex);

		return REG_FAILURE;
            }
        }
	return REG_SUCCESS;
    }


    //-------------------------------------------------------
    /*
     * Start the registry, if necessary.
     */
    private static int startRegistry (String gsh) {
    
        if (gs_registry == null) {
            try {
                if(startHandleMap() != REG_SUCCESS){

		    return REG_FAILURE;
		}

                GridServiceReference gsr = handlemap.findByHandle (gsh);

                String registry_endpoint = gsr.getEndpoint();

                System.err.println ("startRegistry: binding to Registry at " + 
				    registry_endpoint);

                gs_registry = (IRegistry) Registry.bind (registry_endpoint, 
                                                         IRegistry.class, 
							 context);
            }
            catch (RegistryException ex) {

                System.err.println ("startRegistry: problems connecting "
				    +"to registry: "+ex);
                return REG_FAILURE;
            }
            catch (GridServiceException ex) {
                System.err.println ("startRegistry: can't find registry <"+gsh+
				    "> at handle map: "+ex);
                return REG_FAILURE;
            }
        }

	return REG_SUCCESS;
    }
    
    //-------------------------------------------------------
    /**
     * Start the factory, if necessary.
     */
    private static void startFactory (String gsh) {
    
        if (factory == null ) { // First time there is no factory accessable.
            try {
                GridServiceReference gsr = handlemap.findByHandle (gsh);
                String factory_endpoint = gsr.getEndpoint();
                System.err.println ("Binding to Factory....");
                factory = (IGridServiceFactory) Registry.bind(factory_endpoint,
					        IGridServiceFactory.class, 
							      context);
            }
            catch (RegistryException ex) {
                System.err.println ("Problems connecting to factory: "+ex);
                // ARPDBG System.exit(1);
            }
            catch (GridServiceException ex) {
                System.err.println ("Can't find factory <"+gsh+
				    "> at handle map: "+ex);
                // ARPDBG System.exit(1);
            }
        }
    }
   
    //-------------------------------------------------------

    private int getStatus() {

	int status;
	// Called in response to a 'GET_STATUS' instruction from
	// the steerer

	// First get the gsh that the steerer will also have sent
	status = getMessage();

	if(status == REG_FAILURE){

	    System.err.println("getStatus: failed to get gsh");
	    return REG_FAILURE;
	}

	if(app == null){

	    System.err.println("getStatus: no app attached");
	    return REG_FAILURE;
	}

	// At the moment, we only ever steer 'app' so don't 
	// actually use GSH...
	try{

	    String status_xml = app.getStatusFile();
	    sendMessage(status_xml);

	}
	catch(GridServiceException e){

	    System.err.println(e.getMessage());
	    return REG_FAILURE;
	}

	return REG_SUCCESS;
    } 

    //-------------------------------------------------------

    private int sendControl() {

	int status;

	// Get gsh of steered app from steerer
	status = getMessage();
	String gsh = recvd_msg;

	// Get control message to send from steerer
	status = getMessage();

	if(app == null){

	    System.err.println("sendControl: not attached to an app");
	    return REG_FAILURE;
	}

	try{

	    // Send it to the specified application
	    app.sendControlFile(recvd_msg);
	}
	catch(GridServiceException e){

	    System.err.println(e.getMessage());
	    return REG_FAILURE;
	}

	return REG_SUCCESS;
    }

    //-------------------------------------------------------

    private int getApps() {

        // Identify available Steerable Applications
        
	System.err.println ("getApps: entered routine...");

	try{
	    int      i;
	    String   app_list;
	    String[] gsh_array;
	    String   implementation;
	    StringBuffer instance_ids = new StringBuffer();

	    // Get the list of registered grid services from the registry

	    app_list = gs_registry.registryQuery();

	    System.err.println ("\nApplications registered are:\n"+
			        app_list+"\n");

	    if( (gsh_array = parseRegistryContents(app_list)) == null){

		throw new Exception("getApps: Failed to find valid GSH");
	    }

	    for(i=0; i<gsh_array.length; i++){
		System.err.println ("Pulled out gsh = "+gsh_array[i]);
	    }

	    // Use the handlemapper to get from the gsh to a description
	    // of the grid service

	    GridServiceReference[] gsr_array = 
		               new GridServiceReference[gsh_array.length];

	    for(i=0; i<gsh_array.length; i++){

		gsr_array[i] = handlemap.findByHandle (gsh_array[i]);

		implementation = gsr_array[i].getImplementation();

	        // We're only interested in 'SteeringGridServices' as this
	        // framework terms them...
	        if( implementation.indexOf("SteeringGridService") != -1){

		    // Send back id's and corresponding gsh's, separated
		    // by spaces
		    instance_ids.append(" ");
		    instance_ids.append(gsr_array[i].getInstanceID());
		    instance_ids.append(" ");
		    instance_ids.append(gsh_array[i]);
		}
	    }

	    sendMessage(instance_ids.toString());

	}
	catch(Exception e){

	    System.err.println (e.getMessage());
	    return REG_FAILURE;
	}

	return REG_SUCCESS;
    }
}
