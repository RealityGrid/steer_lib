import org.realitygrid.steering.*;

public class Sink implements Runnable, ReG_SteerConstants {
  static {
    try {
      System.loadLibrary("ReG_SteerJava");
    }
    catch(UnsatisfiedLinkError e) {
      System.err.println("Failed to load libReG_SteerJava.so. " + e);
      System.exit(1);
    }
  }

  /* The Thread */
  Thread t;

  /* global variables */
  int status;
  int nloops = 5000;
  int sleep_time = 1;
  boolean finished = false;

  /* For calling Steering_control */
  Intp num_params_changed = new Intp();
  String[] changed_param_labels = new String[REG_MAX_NUM_STR_PARAMS];
  Intp num_recvd_cmds = new Intp();
  int[] recvd_cmds = new int[REG_MAX_NUM_STR_CMDS];
  String[] recvd_cmd_params = new String[REG_MAX_NUM_STR_CMDS];

  /* */
  Intp iotype_handle = new Intp();
  Intp iohandle = new Intp();
  Intp data_type = new Intp();
  Intp data_count = new Intp();

  /* Monitored parameter */
  Intp bytes_read = new Intp();

  public static void main(String argv[]) {
    new Sink();
  }

  public Sink() {
    /* Enable steering and init the library */
    ReG_Steer.Steering_enable(REG_TRUE);

    int[] commands = {REG_STR_STOP};
    status = ReG_Steer.Steering_initialize("Java Sink v1.0", commands);
    if(status != REG_SUCCESS) {
      System.out.println("Steering library initialization failed");
      System.exit(REG_FAILURE);
    }

    /* Register the input IO channel */
    status = ReG_Steer.Register_IOType("VTK_STRUCTURED_POINTS", REG_IO_IN, 1, iotype_handle.cast());
    if(status != REG_SUCCESS) {
      System.out.println("Failed to register IO type");
      System.exit(REG_FAILURE);
    }

    /* Register monitored parameter */
    status = ReG_Steer.Register_param("Bytes_read", REG_FALSE, bytes_read.cast().getVoidPointer(), REG_INT, "", "");

    t = new Thread(this);
    t.start();
  }

  public void run() {
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
      status = ReG_Steer.Steering_control(i,
					  num_params_changed.cast(),
					  changed_param_labels,
					  num_recvd_cmds.cast(),
					  recvd_cmds,
					  recvd_cmd_params);

      if(status != REG_SUCCESS) {
	System.out.println("Ouch!");
	continue;
      }

      /* Zero count of bytes read this time around */
      bytes_read.assign(0);

      if(num_recvd_cmds.value() > 0) {
	for(int icmd = 0; icmd < num_recvd_cmds.value(); icmd++) {
	  switch(recvd_cmds[icmd]) {
	  case REG_STR_STOP:
	    finished = true;
	    break;

	  default:
	    if(recvd_cmds[icmd] == iotype_handle.value()) {
	      /* 'Open' the channel to consume data */
	      status = ReG_Steer.Consume_start(iotype_handle.value(), iohandle.cast());

	      if(status == REG_SUCCESS) {
		/* Data is available to read...get header describing it */
		status = ReG_Steer.Consume_data_slice_header(iohandle.value(),
							     data_type.cast(),
							     data_count.cast());

		while(status == REG_SUCCESS) {
		  System.out.println("Got data: type = " + data_type.value() +
				     ", count = " + data_count.value());

		  /* Do something with data */
		  switch(data_type.value()) {
		  case REG_INT:
		    int[] idata = new int[data_count.value()];
		    status = ReG_Steer.Consume_data_slice_int(iohandle.value(), idata);
		    bytes_read.assign(bytes_read.value() + (data_count.value()
		                                     * ReG_Steer.Sizeof(REG_INT)));
		    break;

		  case REG_CHAR:
		    byte[] cdata = new byte[data_count.value()];
		    status = ReG_Steer.Consume_data_slice_char(iohandle.value(), cdata);
		    bytes_read.assign(bytes_read.value() + (data_count.value()
		                                     * ReG_Steer.Sizeof(REG_CHAR)));

		    System.out.println(new String(cdata));
		    break;

		  case REG_FLOAT:
		    float[] fdata = new float[data_count.value()];
		    status = ReG_Steer.Consume_data_slice_float(iohandle.value(),
								fdata);
		    bytes_read.assign(bytes_read.value() + (data_count.value()
		                                     * ReG_Steer.Sizeof(REG_FLOAT)));

		    break;

		  case REG_DBL:
		    double[] ddata = new double[data_count.value()];
		    status = ReG_Steer.Consume_data_slice_double(iohandle.value(),
								ddata);
		    bytes_read.assign(bytes_read.value() + (data_count.value()
		                                     * ReG_Steer.Sizeof(REG_DBL)));

		    break;
		  } // switch data_type	  

		  status = ReG_Steer.Consume_data_slice_header(iohandle.value(),
							       data_type.cast(),
							       data_count.cast());
		} // while

		/* Reached the end of this data set; 'close' the channel */
		status = ReG_Steer.Consume_stop(iohandle.cast());

	      } // if status
	    } // if recvd_cmds[icmd]
	    break;
	  } // switch
	  if(finished) break;
	} // for icmd
      } // if num_recvd_cmds
      if(finished) break;

    } // for i

    /* Clean-up the steering library */
    status = ReG_Steer.Steering_finalize();
    return;
  }
}
