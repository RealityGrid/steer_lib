import java.io.*;
import java.net.*;
import java.util.*;
import java.lang.Integer;

public class HybridSwitch {

    protected Hashtable threads_by_id;
    protected Hashtable ackCount;
    protected Hashtable msgCount;

public HybridSwitch( int port ) throws InstantiationException {
    try {
	threads_by_id = new Hashtable();
	ackCount = new Hashtable();
	msgCount = new Hashtable();

	ServerSocket serv = new ServerSocket( port );

	for(;;) {
		Socket sock = serv.accept();
		new HybridThread( sock, this );
	}
    } catch( Exception ex ) { 
	throw new InstantiationException( ex.toString() );
    } 
}

protected boolean send( String from,
			String to,
			String id, 
			int length, 
			byte[] data ) {
    boolean hadDest = false;

    List alist = (List) threads_by_id.get(to);
    if( alist == null ){
	System.out.println( "Sending from ["+from+"] failed: NO destination" );
	return false;
    }
    ListIterator it = alist.listIterator();
    while(it.hasNext()){
	HybridThread thr = (HybridThread) it.next();

	//System.out.println( "Sending from ["+from+"] to ["+to+"]" );
	thr.send( from, id, data );
	hadDest = true;
    }
    // Store how many messages we've sent out so that we know how
    // many acks to expect
    msgCount.put(to, new Integer(alist.size()));

    return hadDest;
}

protected void register_thread( String srcID, HybridThread thr ) 
{
    List alist;

    System.out.println( "Registering subscriber to  ["+srcID+"]" ); 
    if( srcID!=null ) {
	alist = (List) threads_by_id.get(srcID);
	if(alist == null){
	    // Used this synchronizedList because the threads
	    // can unregister themselves and thus modify the
	    // ArrayList
	    alist = Collections.synchronizedList(new ArrayList());
	    alist.add(thr);
	    threads_by_id.put( srcID, alist );
	}
	else{
	    alist.add(thr);
	}
    }
}

protected void deregister_thread( String id, HybridThread thr ) 
{
    List alist;

    System.out.println( "Deregistering subscriber to ["+id+"]..." ); 

    if( id == null ) {return;}

    alist = (List) threads_by_id.get( id );
    if(alist == null){return;}

    if(alist.remove(thr)){
	System.out.println( "...done");
	if(alist.size() == 0){
	    threads_by_id.remove(id);
	    System.out.println( "No remaining subscribers to ["+id+"]");
	}
    }
    else{
	System.out.println( "...FAILED!");
    }
}

protected synchronized boolean forwardAck(String dest)
{
    // Remove the trailing "_REG_ACK" from the destination ID
    int idx = dest.lastIndexOf("_REG_ACK");
    String id = dest.substring(0, idx);

    Integer count = (Integer) ackCount.get(id);
    int num;
    if(count == null){
	num = 1;
    }
    else{
	num = count.intValue();
	num++;
    }

    System.out.println("ARPDBG: num = "+num);

    //List alist = (List) threads_by_id.get( id );
    //if(alist == null) return false;
    Integer sentCount = (Integer) msgCount.get(id);
    int numSent;
    if(sentCount == null){
	numSent = 0;
    }
    else{
	numSent = sentCount.intValue();
    }
    System.out.println("ARPDBG: no. of acks expected = "+numSent);

    if(num < numSent){
	ackCount.put(id, new Integer(num));
	return false;
    }
    else{
	ackCount.put(id, new Integer(0));
	return true;
    }
}


public static void main( String args[] ) {
    try {
	new HybridSwitch( Integer.parseInt(args[0]) );
    } catch( Exception ex ) {
	System.out.println( "Syntax: HybridSwitch [portnum]" ); 
    } 
} 

}
