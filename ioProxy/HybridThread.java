import java.io.*;
import java.net.*;

public class HybridThread extends Thread {

HybridSwitch sw;
Socket s;
PrintStream os;
InputStream in;
String this_id;
String src_id;

public HybridThread( Socket s, HybridSwitch sw ) {
	try {

	this.s  = s;
	this.sw = sw;

	os = new PrintStream( s.getOutputStream() );
	in = s.getInputStream();

	byte c = ' ';
	int idx=0;
	byte buffer[] = new byte[1024];
	while( c!='\n' && idx < buffer.length ) {
	    c = (byte) in.read();
	    if( c!='\n' ) {
		buffer[idx++] = c;
	    }
	}		

	// The ID of this connection
	this_id = new String( buffer );
	System.out.println( "New connection --["+this_id+"]" ); 
	this_id = this_id.trim();

        int max = idx;
	idx=0;
        while(idx < max){
	    buffer[idx++]=(byte)0;
	}

	c = ' ';
	idx=0;
	while( c!='\n' && idx < buffer.length ) {
	    c = (byte) in.read();
	    if( c!='\n' ) {
		buffer[idx++] = c;
	    }
	}		

	// The ID of the data source it's subscribing to (or "NO_DATA"
	// if we're output only)
	src_id = new String( buffer );
	System.out.println( "subscribing to --["+src_id+"]" ); 
	src_id = src_id.trim();

	sw.register_thread( src_id, this );

	start();

	} catch( Exception ex ) { ex.printStackTrace( System.err ); } 
}


public void run() {
    try {
	byte buffer[] = new byte[1024];

	for(;;) {

	    byte c = ' ';

	    System.out.println( "Searching for tag..." ); 
	    while( c!='#' ) {
		c = (byte) in.read();
		if( c==-1 ) { 
		    System.out.println( "...read failed");
		    throw new Exception(); 
		}
		System.out.println( "B " ); 
	    }
		
	    System.out.println( "Reading tag..." ); 
	    int idx=0;
	    buffer = new byte[1024];
	    while( c!='\n' && idx < buffer.length ) {
		c = (byte) in.read();
		if( c==-1 ) { throw new Exception(); }
		if( c!='\n' ) {
		    buffer[idx++] = c;
		}
	    }		

	    String to     = new String( buffer );
	    System.out.println( "--Dest. tag: "+to ); 

	    c = ' ';
	    idx=0;
	    buffer = new byte[1024];
	    while( c!='\n' && idx < buffer.length ) {
		c = (byte) in.read();
		if( c==-1 ) { throw new Exception(); }
		if( c!='\n' ) {
		    buffer[idx++] = c;
		}
	    }		


	    String id     = new String( buffer );
	    System.out.println( "--ID: "+id ); 

	    c = ' ';
	    idx=0;
	    buffer = new byte[1024];
	    while( c!='\n' && idx < buffer.length ) {
		c = (byte) in.read();
		if( c==-1 ) { throw new Exception(); }
		if( c!='\n' ) {
		    buffer[idx++] = c;
		}
	    }		

	    String length_s = new String( buffer );
	    length_s = length_s.trim();
	    System.out.println( "--Length of data: "+length_s ); 

	    int length = Integer.parseInt( length_s );
	    int cur = 0;
	    int j;

	    byte[] b = new byte[ length ];
	    
	    while( length-cur > 0 ) {
		j = in.read( b, cur, length-cur );

		if( j==-1 ) {
		    throw new Exception();
		}
		cur+=j;
		System.out.println( "Read "+cur +" of "+ length ); 
	    }

	    to = to.trim();
	    id = id.trim();

	    byte[] ackMsg = new byte[2];

	    if( sw.destination_valid( to ) ){
		sw.send( this_id, to, id, length, b );
		ackMsg[0] = '1';
	    }
	    else{
		ackMsg[0] = '0';
	    }
	    ackMsg[1] = '\n';
	    os.write(ackMsg);
	}
    } catch( Exception ex ) {
	ex.printStackTrace(System.err);
	close();
    } 
}

public synchronized void send( String from, String id, byte[] data ) {
    try {
	    //os.print("\n#"+from+"\n"+id+"\n"+data.length+"\n" );
	System.out.println("send, from: " + from);
	os.write( data );	

    } catch( Exception ex ) { 
	ex.printStackTrace( System.err ); 
	close();
    } 
}

private void close() {
		try {
			s.close();			
			in.close();
		} catch( Exception ex ) { }
		sw.deregister_thread( src_id );
}
}
