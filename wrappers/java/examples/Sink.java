import org.realitygrid.steering.*;

public class Sink implements Runnable, ReG_SteerConstants {

  /* The Thread */
  Thread t;

  /* The Steering Library */
  ReG_SteerAppside rsa;

  /* global variables */
  int nloops = 1000;
  int sleep_time = 1;
  boolean finished = false;

  /* */
  int iotype_handle;

  /* Monitored parameter */
  ReG_SteerParameter bytesRead;

  public static void main(String argv[]) {
    new Sink();
  }

  public Sink() {

    rsa = ReG_SteerAppside.getInstance();

    /* Enable steering and init the library */
    rsa.steeringEnable(true);

    int[] commands = {REG_STR_STOP};
    try {
      rsa.steeringInitialize("Java Sink v1.0", commands);
    }
    catch(ReG_SteerException e) {
      System.err.println(e.getMessage());
      System.exit(e.getErrorCode());
    }

    /* Register the input IO channel */
    try {
      iotype_handle = rsa.registerIOType("VTK_STRUCTURED_POINTS", REG_IO_IN, 1);
    }
    catch(ReG_SteerException e) {
      System.err.println(e.getMessage());
      System.exit(e.getErrorCode());      
    }

    /* Register monitored parameter */
    bytesRead = new ReG_SteerParameter("Items_read", false, REG_INT, "", "");
    try {
      bytesRead.register();
    }
    catch(ReG_SteerException e) {
      System.err.println(e.getMessage());
      System.exit(e.getErrorCode());
    }

    /* Start the thread running */
    t = new Thread(this);
    t.start();
  }

  public void run() {

    int numParamsChanged;
    int numReceivedCommands;
    int[] recvdCmds;

    int iohandle;

    /* Enter main loop waiting for data to arrive */
    for(int i = 0; i < nloops; i++) {
      
      try {
	Thread.sleep(sleep_time * 1000);
      }
      catch(InterruptedException e) {
	System.err.println("Interrupted!");
	continue;
      }

      System.out.println("\ni = " + i);

      /* Talk to the steering client (if one is connected) */
      try {
	int[] result = rsa.steeringControl(i);
	numParamsChanged = result[0];
	numReceivedCommands = result[1];
	recvdCmds = rsa.getReceivedCommands();
      }
      catch(ReG_SteerException e) {
	System.err.println(e.getMessage());
	continue;
      }

      /* Zero count of bytes read this time around */
      bytesRead.setValue(0);

      if(numReceivedCommands > 0) {
	for(int icmd = 0; icmd < numReceivedCommands; icmd++) {
	  switch(recvdCmds[icmd]) {
	  case REG_STR_STOP:
	    finished = true;
	    break;

	  default:
	    if(recvdCmds[icmd] == iotype_handle) {
	      /* 'Open' the channel to consume data */
	      try {
		iohandle = rsa.consumeStart(iotype_handle);

		while(true) {
		  Object data = rsa.consumeDataSlice(iohandle);
		  if(data == null)
		    break;
		  if(data instanceof String) 
		    System.out.println(data);
		}

		/* Reached the end of this data set; 'close' the channel */
		iohandle = rsa.consumeStop(iohandle);

	      }
	      catch(ReG_SteerException e) {
		System.err.println(e.getMessage());
	      }
	    } // if recvd_cmds[icmd]
	    break;
	  } // switch
	  if(finished) break;
	} // for icmd
      } // if num_recvd_cmds
      if(finished) break;

    } // for i

    /* Clean-up the steering library */
    try {
      rsa.steeringFinalize();
    }
    catch(ReG_SteerException e) { }

    return;
  }
}
