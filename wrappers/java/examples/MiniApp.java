import org.realitygrid.steering.*;

public class MiniApp implements Runnable, ReG_SteerConstants {

  /* The Thread */
  Thread t;

  /* The Steering Library */
  ReG_SteerAppside rsa;

  /* global variables */
  int nloops = 1000;
  boolean finished = false;
  int chunkDim = 4;

  /* IO Types */
  int numIOTypes;
  int[] ioTypeHandle = new int[REG_INITIAL_NUM_IOTYPES];


  /* Monitored and Steered parameters */
  ReG_SteerParameter sleepTime;
  ReG_SteerParameter opacityStepStop;
  ReG_SteerParameter temp;
  ReG_SteerParameter nx;
  ReG_SteerParameter ny;
  ReG_SteerParameter nz;
  ReG_SteerParameter aAxis;
  ReG_SteerParameter bAxis;
  ReG_SteerParameter cAxis;

  public static void main(String[] argv) {
    new MiniApp();
  }

  public MiniApp() {

    rsa = ReG_SteerAppside.getInstance();

    /* Enable steering and init the library */
    rsa.steeringEnable(true);

    int[] commands = {REG_STR_STOP, REG_STR_PAUSE_INTERNAL};
    try {
      rsa.steeringInitialize("Java MiniApp v1.0", commands);
    }
    catch(ReG_SteerException e) {
      System.err.println(e.getMessage());
      System.exit(e.getErrorCode());
    }

    /* Register the input and output IO channels */
    try {
      ioTypeHandle[0] = rsa.registerIOType("SOME_INPUT_DATA", REG_IO_IN, 0);
      ioTypeHandle[1] = rsa.registerIOType("VTK_STRUCTURED_POINTS", REG_IO_OUT, 1);
    }
    catch(ReG_SteerException e) {
      System.err.println(e.getMessage());
      System.exit(e.getErrorCode());      
    }
    numIOTypes = 2;

    /* Register the parameters */
    sleepTime = new ReG_SteerParameter("time_to_sleep", 1, true, REG_INT, "0", "100");
    opacityStepStop = new ReG_SteerParameter("OPACITY_STEP_STOP", 130, true, REG_INT, "0", "256");
    temp = new ReG_SteerParameter("TEMP", 55.6f, false, REG_FLOAT, "", "");
    aAxis = new ReG_SteerParameter("a_axis", 1.5, true, REG_DBL, "0.01", "10.0");
    bAxis = new ReG_SteerParameter("b_axis", 1.5, true, REG_DBL, "0.01", "10.0");
    cAxis = new ReG_SteerParameter("c_axis", 1.5, true, REG_DBL, "0.01", "10.0");
    nx = new ReG_SteerParameter("nx", 16, true, REG_INT, "1", "");
    ny = new ReG_SteerParameter("ny", 16, true, REG_INT, "1", "");
    nz = new ReG_SteerParameter("nz", 16, true, REG_INT, "1", "");
    try {
      sleepTime.register();
      opacityStepStop.register();
      temp.register();
      aAxis.register();
      bAxis.register();
      cAxis.register();
      nx.register();
      ny.register();
      nz.register();
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

    int chunkDim = 4;
    //int dataSize = 4096;;

    /* Enter main loop waiting for data to arrive */
    for(int i = 0; i < nloops; i++) {

      try {
	Thread.sleep(sleepTime.getIntValue() * 1000);
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

      System.out.println("opacityStepStop = " + opacityStepStop.getValue());
      System.out.println("temp            = " + temp.getValue());

      if(numReceivedCommands > 0) {
	for(int icmd = 0; icmd < numReceivedCommands; icmd++) {
	  switch(recvdCmds[icmd]) {
	  case REG_STR_STOP:
	    finished = true;
	    break;

	  default:
	    for(int j = 0; j < numIOTypes; j++) {
	      if(recvdCmds[icmd] == ioTypeHandle[1]) {
		// emit data
		try {
		  iohandle = rsa.emitStart(ioTypeHandle[1], i);
		  String header = "DATA HEADER";
		  rsa.emitDataSlice(iohandle, header);

		  /* nx, ny and nz are steerable so we need to alloc every time */
		  int bufferSize = nx.getIntValue() * ny.getIntValue() * nz.getIntValue();

		  float[] buffer = new float[bufferSize];
		  for(int l = 0; l < bufferSize; l++) 
		    buffer[l] = l;

		  /* emit data in chunks to mimic a parallel program */
		  if(nx.getIntValue() % chunkDim != 0) {
		    System.out.println("nx not a multiple of " + chunkDim);
		    rsa.emitStop(iohandle);
		    continue;
		  }

		  int numChunks = nx.getIntValue() / chunkDim;
		  System.out.println("nx = " + nx.getIntValue() + ", chunkDim = " + chunkDim + " so have " + numChunks + " chunks");

		  for(int iChunk = 0; iChunk < numChunks; iChunk++) {
		    System.out.println("chunk " + iChunk + "...");
		    String chunkHeader = "CHUNK HEADER";
		    rsa.emitDataSlice(iohandle, chunkHeader);

		    int dataCount = chunkDim * ny.getIntValue() * nz.getIntValue();
		    float[] sendBuffer = new float[dataCount];
		    for(int b = 0; b < dataCount; b++) {
		      int item = (iChunk * chunkDim * ny.getIntValue() * nz.getIntValue());
		      sendBuffer[b] = buffer[item + b];
		    }
		    rsa.emitDataSlice(iohandle, sendBuffer);
		  } // iChunk
		  rsa.emitStop(iohandle);
		}
		catch(ReG_SteerException e) {
		  System.err.println(e.getMessage());
		}
	      } // if ioTypeHandle
	    } // j (numIOTypes)
	    break;
	  } // switch recvdCmds
	  if(finished) break;
	} // icmd
	if(finished) break;
      } // numReceivedCommands

      temp.setValue(temp.getFloatValue() + 0.5347672f);

    } // nloops

    /* Clean-up the steering library */
    try {
      rsa.steeringFinalize();
    }
    catch(ReG_SteerException e) { }

    return;
  }

}
