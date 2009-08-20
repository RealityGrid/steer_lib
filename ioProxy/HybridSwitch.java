import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Hashtable;
import java.util.List;
import java.util.ListIterator;

public class HybridSwitch {

  protected Hashtable<String, List<HybridThread>> threads_by_id;
  protected Hashtable<String, Integer> ackCount;
  protected Hashtable<String, Integer> msgCount;

  public HybridSwitch(int port) throws InstantiationException {
    try {
      threads_by_id = new Hashtable<String, List<HybridThread>>();
      ackCount = new Hashtable<String, Integer>();
      msgCount = new Hashtable<String, Integer>();

      ServerSocket serv = new ServerSocket(port);

      for (;;) {
	Socket sock = serv.accept();
	new HybridThread(sock, this);
      }
    } catch (Exception ex) {
      throw new InstantiationException(ex.toString());
    }
  }

  protected boolean send(String from, String to, String id, int length,
			 byte[] data) {
    boolean hadDest = false;

    List<HybridThread> alist = threads_by_id.get(to);
    if (alist == null) {
      System.out.println("Sending from [" + from
			 + "] failed: NO destination");
      return false;
    }
    ListIterator<HybridThread> it = alist.listIterator();
    while (it.hasNext()) {
      HybridThread thr = it.next();

      thr.send(from, id, data);
      hadDest = true;
    }
    System.out.println("Sent " + alist.size() + "msgs from [" + from
		       + "] to [" + to + "]");
    // Store how many messages we've sent out so that we know how
    // many acks to expect
    msgCount.put(to, alist.size());

    return hadDest;
  }

  protected void register_thread(String srcID, HybridThread thr) {
    List<HybridThread> alist;

    System.out.println("Registering subscriber to  [" + srcID + "]");
    if (srcID != null) {
      alist = threads_by_id.get(srcID);
      if (alist == null) {
	// Used this synchronizedList because the threads
	// can unregister themselves and thus modify the
	// ArrayList
	alist = Collections.synchronizedList(new ArrayList<HybridThread>());
	alist.add(thr);
	threads_by_id.put(srcID, alist);
      } else {
	alist.add(thr);
      }
    }
  }

  protected void deregister_thread(String id, HybridThread thr) {
    List<HybridThread> alist;

    System.out.println("Deregistering subscriber to [" + id + "]...");

    if (id == null) {
      return;
    }

    alist = threads_by_id.get(id);
    if (alist == null) {
      return;
    }

    if (alist.remove(thr)) {
      System.out.println("...done");
      if (alist.size() == 0) {
	threads_by_id.remove(id);
	System.out.println("No remaining subscribers to [" + id + "]");
      }
    } else {
      System.out.println("...FAILED!");
    }
  }

  // Called whenever we have received an acknowledgement bound
  // for 'dest'
  protected synchronized boolean forwardAck(String dest) {
    // Remove the trailing "_REG_ACK" from the destination ID
    int idx = dest.lastIndexOf("_REG_ACK");
    String id = dest.substring(0, idx);

    // Increment the number of acknowledgements that have been
    // received for this data source
    Integer count = ackCount.get(id);
    int num;
    if (count == null) {
      num = 1;
    } else {
      num = count;
      num++;
    }

    System.out.println("ARPDBG: num = " + num);

    // Compare the no. of acks received with the number of
    // messages that were sent
    Integer sentCount = msgCount.get(id);
    int numSent;
    if (sentCount == null) {
      numSent = 0;
    } else {
      numSent = sentCount;
    }
    System.out.println("ARPDBG: no. of acks expected = " + numSent);

    if (num < numSent) {
      ackCount.put(id, num);
      return false;
    } else {
      ackCount.put(id, 0);
      return true;
    }
  }

  public static void main(String args[]) {
    try {
      new HybridSwitch(Integer.parseInt(args[0]));
    } catch (Exception ex) {
      System.out.println("Syntax: HybridSwitch [portnum]");
    }
  }
}
