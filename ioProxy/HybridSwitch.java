import java.io.*;
import java.net.*;
import java.util.*;

public class HybridSwitch {

protected Hashtable threads_by_id;

public HybridSwitch( int port ) throws InstantiationException {
	try {
	threads_by_id = new Hashtable();

	ServerSocket serv = new ServerSocket( port );

	for(;;) {
		Socket sock = serv.accept();
		new HybridThread( sock, this );
	}
	} catch( Exception ex ) { 
	    throw new InstantiationException( ex.toString() );
	} 
}

protected void send( String from,
		     String to,
		     String id, 
		     int length, 
		     byte[] data ) {

	HybridThread thr = (HybridThread) threads_by_id.get(to);

	if( thr!=null ) {
		System.out.println( "Sending from ["+from+"] to ["+to+"]" );
		thr.send( from, id, data );
	}
	else {
		System.out.println( "Sending from ["+from+"] failed: NO destination" );
	}
}

protected void register_thread( String srcID, HybridThread thr ) {
	System.out.println( "Registering subscriber to  ["+srcID+"]" ); 
	if( srcID!=null ) {
		threads_by_id.put( srcID, thr );
	}
}

protected void deregister_thread( String id ) {
	System.out.println( "Deregistering ["+id+"]" ); 
	if( id!=null ) {
		threads_by_id.remove( id );
	}
}

protected boolean destination_valid( String dest ) {

	HybridThread thr = (HybridThread) threads_by_id.get(dest);
	return (thr != null);
}

public static void main( String args[] ) {
	try {
		new HybridSwitch( Integer.parseInt(args[0]) );
	} catch( Exception ex ) {
		System.out.println( "Syntax: HybridSwitch [portnum]" ); 
	} 

} 

}
