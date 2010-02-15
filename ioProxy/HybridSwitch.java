/*
  The RealityGrid Steering Library

  Copyright (c) 2002-2010, University of Manchester, United Kingdom.
  All rights reserved.

  This software is produced by Research Computing Services, University
  of Manchester as part of the RealityGrid project and associated
  follow on projects, funded by the EPSRC under grants GR/R67699/01,
  GR/R67699/02, GR/T27488/01, EP/C536452/1, EP/D500028/1,
  EP/F00561X/1.

  LICENCE TERMS

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

    * Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of The University of Manchester nor the names
      of its contributors may be used to endorse or promote products
      derived from this software without specific prior written
      permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
  COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
 */

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
      int port = Integer.parseInt(args[0]);
      System.out.println("Starting proxy. Listening on port " + port + ".");
      new HybridSwitch(port);
    } catch(NumberFormatException nex) {
      System.err.println("The port number should be a valid integer.");
      System.exit(1);
    } catch(Exception ex) {
      System.err.println("Syntax: HybridSwitch [portnum]");
      System.exit(1);
    }
  }
}
