/* ----------------------------------------------------------------------------
  This file is part of the RealityGrid Steering Library Java Wrappers.
 
  (C) Copyright 2005, University of Manchester, United Kingdom,
  all rights reserved.
 
  This software was developed by the RealityGrid project
  (http://www.realitygrid.org), funded by the EPSRC under grants
  GR/R67699/01 and GR/R67699/02.
 
  LICENCE TERMS
 
  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:
  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.
 
  THIS MATERIAL IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. THE ENTIRE RISK AS TO THE QUALITY
  AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE PROGRAM PROVE
  DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR
  CORRECTION.
 
  Author........: Robert Haines
---------------------------------------------------------------------------- */

package org.realitygrid.steering;

/**
 * This class provides access to the RealityGrid application side API.
 *
 * @version 1.2b
 * @author Robert Haines
 * @author Andrew Porter (RealityGrid API)
 * @see <a href="http://www.realitygrid.org/">The RealityGrid Website</a>
 * @see <a href="http://www.sve.man.ac.uk/Research/AtoZ/RealityGrid">the RealityGrid pages at Manchester Computing</a>
 * @see <a href="http://www.sve.man.ac.uk/Research/AtoZ/RealityGrid/Steering/ReG_Steering_API_v1.2b.pdf">RealityGrid API Documentation v1.2b (PDF file)</a>
 */
public class ReG_SteerAppside implements ReG_SteerConstants {

  private static ReG_SteerAppside instance = new ReG_SteerAppside();

  private int status;

  private String[] changedParamLabels;
  private int[] receivedCommands;
  private String[] receivedCommandParams;

  /**
   * Private constructor. Use <code>getInstance</code> to create an instance
   * of this class.
   */
  private ReG_SteerAppside() {
    changedParamLabels = new String[REG_MAX_NUM_STR_PARAMS];
    receivedCommands = new int[REG_MAX_NUM_STR_CMDS];
    receivedCommandParams = new String[REG_MAX_NUM_STR_CMDS];
  }

  /**
   * This method is used to obtain an instance of this class. Only one
   * instance of this class is allowed, so there are only private
   * constructors and all users of this instance will be using the
   * same instance of the steering library.
   *
   * @return the single instance of ReG_SteerAppside.
   */
  public static ReG_SteerAppside getInstance() {
    return instance;
  }

  /**
   * Provides access to the parameter labels that changed during the
   * last call to <code>steeringControl</code>.
   *
   * @return the array of changed parameter labels.
   *
   * @see #steeringControl(int)
   */
  public String[] getChangedParamLabels() {
    return changedParamLabels;
  }

  /**
   * Provides access to the commands that were received during the
   * last call to <code>steeringControl</code>.
   *
   * @return the array of commands recieved.
   *
   * @see #steeringControl(int)
   */
  public int[] getReceivedCommands() {
    return receivedCommands;
  }

  /**
   * Provides access to the parameters for each of the commands received
   * during the last call to <code>steeringControl</code>.
   *
   * @return the array of parameters for each command.
   *
   * @see #steeringControl(int)
   */
  public String[] getReceivedCommandParams() {
    return receivedCommandParams;
  }

  /**
   * Enable and disable the steering library. Note that disabling the
   * steering library using this method is not the same as calling
   * steeringFinalize. Steering can be re-enabled after being disabled
   * with this call.<p><strong>Steering must be enabled for any other
   * calls to the steering library to work!</strong>
   *
   * @param toggle whether the steering library should be enabled (true) or
   * disabled (false).
   *
   * @see #steeringFinalize()
   */
  public void steeringEnable(boolean toggle) {
    if(toggle)
      ReG_Steer.Steering_enable(REG_TRUE);
    else
      ReG_Steer.Steering_enable(REG_FALSE);
  }

  /**
   * Initialise the steering library. A short text description of the
   * application should be provided along with an array of commands that
   * the application will respond to. These commands are specified with
   * constants such as <code>REG_STR_STOP</code>, <code>REG_STR_PAUSE</code>,
   * etc.
   *
   * @param app the text description of the steered application.
   * @param commands the array of steering commands that the application will
   * respond to.
   *
   * @throws ReG_SteerException If initialisation of the steering library
   * failed.
   *
   * @see #steeringFinalize()
   * @see ReG_SteerConstants
   */
  public void steeringInitialize(String app, int[] commands) throws ReG_SteerException {
    status = ReG_Steer.Steering_initialize(app, commands);
    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Failed to initialize steering.", status);
    }
  }

  /**
   * Register an IOType with the steering library. The short text description
   * is used as a label in the steering client. The direction should be 
   * specified as <code>REG_IO_IN</code>, <code>REG_IO_OUT</code> or <code>
   * REG_IO_INOUT</code> depending on whether the IOType will be used to emit,
   * consume or emit and consume data.
   *
   * @param name a label for the IOType.
   * @param dir the direction of the IOType.
   * @param freq initial frequency (in time steps) that the IOType will be
   * used for emission/consumption of data.
   * @return the IOType handle assigned to this new IOType.
   *
   * @throws ReG_SteerException If the steering library failed to register
   * the IOType.
   *
   * @see #enableIOTypesOnRegistration(boolean)
   * @see ReG_SteerConstants
   */
  public int registerIOType(String name, int dir, int freq) throws ReG_SteerException {
    Intp iotype_handle = new Intp();

    status = ReG_Steer.Register_IOType(name, dir, freq, iotype_handle.cast());
    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Failed to register IO type: " + name, status);
    }

    return iotype_handle.value();
  }

  /**
   * Toggle the automatic enabling of IOTypes as they are registered by
   * <code>registerIOType</code>. Default behaviour is that they are enabled
   * automatically. This method is intended for use with sockets-based IOTypes
   * where "enabling" corresponds to the creation of the socket.
   *
   * @param toggle true to enable IOTypes when registered, false otherwise.
   *
   * @throws ReG_SteerException If there is an internal steering library error.
   *
   * @see #registerIOType(String, int, int)
   */
  public void enableIOTypesOnRegistration(boolean toggle) throws ReG_SteerException {
    if(toggle)
      status = ReG_Steer.Enable_IOTypes_on_registration(REG_TRUE);
    else
      status = ReG_Steer.Enable_IOTypes_on_registration(REG_FALSE);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("IOTypesOnRegistration failed", status);
    }
  }

  /**
   * Enables a disabled IOType. If using sockets-based IOTypes this will
   * create the socket. Details of the IOType's endpoints are (re-)obtained
   * from the underlying Grid framework during this call.
   *
   * @param iot the IOType handle of the IOType to be enabled.
   *
   * @throws ReG_SteerException If enabling the IOType failed.
   */
  public void enableIOType(int iot) throws ReG_SteerException {
    status = ReG_Steer.Enable_IOType(iot);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Could not enable IO Type", status);
    }
  }

  /**
   * Disables an enabled IOType. If using sockets-based IOTypes this will
   * destroy the socket associated with the IOType.
   *
   * @param iot the IOType handle of the IOType to be disabled.
   *
   * @throws ReG_SteerException If disabling the IOType failed.
   */
  public void disableIOType(int iot) throws ReG_SteerException {
    status = ReG_Steer.Disable_IOType(iot);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Could not disable IO Type", status);
    }
  }

  /**
   * Switches on the use of acknowledgements for the specified IOType. When
   * acknowledgements are enabled, call to <code>emitStart</code> will fail
   * with an exception (error code <code>REG_NOT_READY</code>) until an
   * acknowledgement of the last data set emitted has been received from the
   * consumer.
   *
   * @param iot the IOType to enable acknowledgements for.
   *
   * @throws ReG_SteerException If enabling acknowledgements on the IOType
   * failed.
   *
   * @see #disableIOTypeAcks(int)
   * @see #emitStart(int, int)
   */
  public void enableIOTypeAcks(int iot) throws ReG_SteerException {
    status = ReG_Steer.Enable_IOType_acks(iot);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Could not enable IO Type Acknowledgements", status);
    }
  }

  /**
   * Switches off the use of acknowledgements for the specified IOType
   * (acknowledgements are on by default). When acknowledgements are disabled,
   * the library will attempt to emit a data set, irrespective of whether or
   * not the consumer has acknowledged the previous one. When socket-based IO
   * is used this can result in the emitter blocking if the consumer is unable
   * to keep up.
   *
   * @param iot the IOType to disable acknowledgements for.
   *
   * @throws ReG_SteerException If disabling acknowledgements on the IOType
   * failed.
   *
   * @see #enableIOTypeAcks(int)
   * @see #emitStart(int, int)
   */
  public void disableIOTypeAcks(int iot) throws ReG_SteerException {
    status = ReG_Steer.Disable_IOType_acks(iot);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Could not disable IO Type Acknowledgements", status);
    }
  }

  /**
   * Register a parameter with the steering library.
   *
   * @param param the parameter to be registered.
   *
   * @throws ReG_SteerException If registering the parameter fails.
   *
   * @see ReG_SteerParameter#register()
   */
  public void registerParam(ReG_SteerParameter param) throws ReG_SteerException {
    if(param.isRegistered()) {
      throw new ReG_SteerException("This parameter (" + param.getLabel() + ") is already registered.", REG_FAILURE);
    }

    int steered = REG_FALSE;
    if(param.isSteerable())
      steered = REG_TRUE;

    status = ReG_Steer.Register_param(param.getLabel(),
				      steered,
				      param.getVoidPointer(),
				      param.getType(),
				      param.getMinLabel(),
				      param.getMaxLabel());

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Failed to register parameter: " + param.getLabel(), status);
    }   
  }

  /**
   * This method should be called every time through the main loop of the
   * application.<p>It performs the following actions:
   * <ul>
   * <li>checks for and enables a steering client to connect if the application
   * is not already being steered;
   * <li>retrieves any (new) values for the steerable parameters from an
   * attached steering client and updates the associated simulation variables.
   * <li>retrieves any commands from an attached steering client - those that
   * must be handled by the application itself are returned as a list (encoded
   * as integers) that can be accessed via <code>getReceivedCommands</code>. It
   * is the application's responsibility to deal with these commands in the
   * order in which they occur in the list;
   * <li>reports the current values of all registered parameters (both
   * monitored and steered) to the steering client.
   * </ul>
   *
   * @param step the timestep of the application.
   * @return the number of parameters changed and the number of commands
   * received.
   *
   * @throws ReG_SteerException If the internal call to
   * <code>steeringControl</code> failed.
   *
   * @see #getChangedParamLabels()
   * @see #getReceivedCommands()
   * @see #getReceivedCommandParams()
   * @see <a href="http://www.sve.man.ac.uk/Research/AtoZ/RealityGrid/Steering/ReG_Steering_API_v1.2b.pdf">RealityGrid API Documentation v1.2b (PDF file)</a>
   */
  public int[] steeringControl(int step) throws ReG_SteerException {
    int[] result = new int[2];
    Intp numParamsChanged = new Intp();
    Intp numReceivedCommands = new Intp();

    status = ReG_Steer.Steering_control(step,
					numParamsChanged.cast(),
					changedParamLabels,
					numReceivedCommands.cast(),
					receivedCommands,
					receivedCommandParams);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Steering control call failed.", status);
    }

    result[0] = numParamsChanged.value();
    result[1] = numReceivedCommands.value();
    return result;
  }

  /**
   * This method "opens" an IO channel ready to emit data. The channel to open
   * is identified by passing in an IOType handle, which must have been
   * obtained from a prior call to <code>registerIOType</code> and be of type
   * <code>OUT</code>.<p>It is assumed that <code>emitStart</code> can
   * determine the destination associated with this particular type of data,
   * <em>e.g.</em> by interrogating the Grid framework.
   *
   * @param ioth the IOType handle of the IOType to be used for emitting data.
   * @param seq an indication of the application's progress.
   * @return the handle of the opened IO channel.
   *
   * @throws ReG_SteerException If the IO channel is not connected and the data
   * could not be sent the error code will be <code>REG_FAILURE</code>. This
   * will typically occur when sockets-based IO is being used and the component
   * intended to receive the data has yet to establish a connection. An error
   * code of <code>REG_NOT_READY</code> indicates that no acknowledgement of
   * the last data set being succesfully emitted has been received from the
   * consumer of this IOType. (The use of acknowledgements for a given IOType
   * can be controlled with the use of <code>enableIOTypeAcks</code> and 
   * <code>disableIOTypeAcks</code>.)
   *
   * @see #emitStart(int, int, float)
   * @see #emitStop(int)
   * @see #registerIOType(String, int, int)
   * @see #enableIOTypeAcks(int)
   * @see #disableIOTypeAcks(int)
   */
  public int emitStart(int ioth, int seq) throws ReG_SteerException {
    Intp iohandle = new Intp();

    status = ReG_Steer.Emit_start(ioth, seq, iohandle.cast());

    if(status == REG_FAILURE) {
      throw new ReG_SteerException("Emit start failed. The IO channel is not connected.", status);
    }
    else if(status == REG_NOT_READY) {
      throw new ReG_SteerException("Emit start failed. The IO channel is not ready to emit data.", status);
    }

    return iohandle.value();
  }

  /**
   * This is a blocking version of <code>emitStart</code>. It blocks until
   * the IOType is ready to send data or the specified timeout (seconds) is
   * exceeded.
   *
   * @param ioth the IOType handle of the IOType to be used for emitting data.
   * @param seq an indication of the application's progress.
   * @param timeout the timeout of the call in seconds.
   * @return the handle of the opened IO channel.
   *
   * @throws ReG_SteerException If the call times out, the error code returned
   * will be <code>REG_TIMED_OUT</code>.
   *
   * @see #emitStart(int, int)
   * @see #emitStop(int)
   * @see #registerIOType(String, int, int)
   */
  public int emitStart(int ioth, int seq, float timeout) throws ReG_SteerException {
    Intp iohandle = new Intp();

    status = ReG_Steer.Emit_start_blocking(ioth, seq, iohandle.cast(), timeout);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Emit start failed. Timed out.", status);
    }

    return iohandle.value();
  }

  /**
   * After obtaining an IO channel from a call to <code>emitStart</code>, an
   * application should emit the pieces of a data set with successive calls to
   * <code>emitDataSlice</code>. This method wraps the low-level IO necessary
   * for emitting the sample data. It will <strong>block</strong> until either
   * the requested amount of data has been emitted or an error occurs.<p>The
   * application programmer is responsible for collecting sample data (in
   * portions if necessary) and passing it this method. An example of the
   * proposed usage of this routine (in C) is given in Appendix B of the
   * RealityGrid API Reference Manual.
   *
   * @param ioh the IO channel to use for data emission.
   * @param data the data to be emitted.
   *
   * @throws ReG_SteerException If emitting the data slice failed.
   *
   * @see #emitStart(int, int)
   * @see #emitStart(int, int, float)
   * @see <a href="http://www.sve.man.ac.uk/Research/AtoZ/RealityGrid/Steering/ReG_Steering_API_v1.2b.pdf">RealityGrid API Documentation v1.2b (PDF file)</a>
   */
  public void emitDataSlice(int ioh, Object data) throws ReG_SteerException {
    Object dataSlice;
    if(data instanceof char[]) 
      dataSlice = new String((char[]) data);
    else
      dataSlice = data;

    status = ReG_Steer.Emit_data_slice(ioh, dataSlice);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Emit data slice failed.", status);
    }
  }

  /**
   * Signal the end of the emission of data on the IO channel. This signals to
   * the receiving end that the transmission is complete.
   *
   * @param ioh the handle of the IO channel to close.
   * @return the handle of the IO channel that was closed (now invalid).
   *
   * @throws ReG_SteerException If closing the IO channel failed.
   *
   * @see #emitStart(int, int)
   * @see #emitStart(int, int, float)
   */
  public int emitStop(int ioh) throws ReG_SteerException {
    Intp iohandle = new Intp();
    iohandle.assign(ioh);

    status = ReG_Steer.Emit_stop(iohandle.cast());

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Emit stop failed.", status);
    }

    return iohandle.value();
  }

  /**
   * The data input equivalent of <code>emitStart</code> - a call to open an
   * IO channel ready to receive data. The channel to open is identified by
   * its IOType handle which must have been obtained from a prior call to
   * <code>registerIOType</code> and be of type IN. It is assumed that the
   * source associated with this particular type of sample data can be
   * determined, <em>e.g.</em> by interrogating the Grid framework.<p>Note that
   * in calling this method, the application signals the data source that it is
   * ready for new data - <em>i.e.</em> if <code>consumeStop</code> has been
   * called succesfully for a previous data set then a call to
   * <code>consumeStart</code> results in an acknowledgement of that data set
   * being sent to the emitter (if acknowledgements are turned on for that
   * particular IOType).
   *
   * @param ioth the IOType handle of the consuming IOType.
   * @return the handle of the IO channel to be used for data consumption.
   *
   * @throws ReG_SteerException If there is no data on the channel that is
   * available to be consumed the error code will be <code>REG_FAILURE</code>.
   *
   * @see #consumeStart(int, float)
   * @see #consumeStop(int)
   * @see #registerIOType(String, int, int)
   * @see #enableIOTypeAcks(int)
   * @see #disableIOTypeAcks(int)
   */
  public int consumeStart(int ioth) throws ReG_SteerException {
    Intp iohandle = new Intp();

    status = ReG_Steer.Consume_start(ioth, iohandle.cast());

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("No data to consume.", status);
    }

    return iohandle.value();
  }

  /**
   * This is a blocking version of <code>consumeStart</code>. It blocks until
   * the IOType is ready to read data or the specified timeout (seconds) is
   * exceeded.
   *
   * @param ioth the IOType handle of the consuming IOType.
   * @param timeout the timeout of the call in seconds.
   * @return the handle of the IO channel to be used for data consumption.
   *
   * @throws ReG_SteerException If the call times out, the error code returned
   * will be <code>REG_TIMED_OUT</code>.
   *
   * @see #consumeStart(int)
   * @see #consumeStop(int)
   * @see #registerIOType(String, int, int)
   */
  public int consumeStart(int ioth, float timeout) throws ReG_SteerException {
    Intp iohandle = new Intp();

    status = ReG_Steer.Consume_start_blocking(ioth, iohandle.cast(), timeout);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("No data to consume.", status);      
    }

    return iohandle.value();
  }

  /**
   * Wraps the low-level IO for receiving sample data from another component.
   * The IO channel to use is identified by an IO handle obtained from a prior
   * call to <code>consumeStart</code>.<p>This method is significantly
   * different in the Java API from its C and FORTRAN conterparts. In the C and
   * FORTRAN API separate calls are required to <code>Consume_data_slice</code>
   * and <code>Consume_data_slice_header</code>, whereas in the Java API the
   * functionality of both of these routines is contained within this single
   * method.
   *
   * @param ioh the IO handle of the IO channel to use for data consumption.
   * @return the data slice wrapped in an instance of an Object.
   *
   * @see java.lang.Object
   * @see #consumeStart(int)
   * @see #consumeStart(int, float)
   * @see <a href="http://www.sve.man.ac.uk/Research/AtoZ/RealityGrid/Steering/ReG_Steering_API_v1.2b.pdf">RealityGrid API Documentation v1.2b (PDF file)</a>
   */
  public Object consumeDataSlice(int ioh) {
    Intp dataType = new Intp();
    Intp dataCount = new Intp();
    Object result;

    status = ReG_Steer.Consume_data_slice_header(ioh, dataType.cast(), dataCount.cast());

    if(status == REG_SUCCESS) {
      result = ReG_Steer.Consume_data_slice_j(ioh, dataType.value(), dataCount.value());
    }
    else
      result = null;
    
    return result;
  }

  /**
   * This method should be called after a call to <code>consumeDataSlice</code>
   * has returned <code>null</code> indicating that there are no more slices
   * to read for the current data set. It signals the end of the consumption on
   * the IO channel referred to by a particular IO handle. The value of the
   * IO handle is not valid once this call has completed.
   *
   * @param ioh the handle of the IO channel to close.
   * @return the handle of the IO channel that was closed (now invalid).
   *
   * @throws ReG_SteerException If closing the IO channel failed.
   *
   * @see #consumeStart(int)
   * @see #consumeStart(int, float)
   */
  public int consumeStop(int ioh) throws ReG_SteerException {
    Intp iohandle = new Intp();
    iohandle.assign(ioh);

    status = ReG_Steer.Consume_stop(iohandle.cast());

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Failed to stop consuming!", status);
    }

    return iohandle.value();
  }

  /**
   * Close any existing connection to a steering client and clear the internal
   * library tables of registered parameters and sample-data/checkpoint types.
   * The application cannot be steered following this call unless a further
   * call to <code>steeringInitialize</code> is made.
   *
   * @throws ReG_SteerException If the call fails.
   *
   * @see #steeringInitialize(String, int[])
   */
  public void steeringFinalize() throws ReG_SteerException {
    status = ReG_Steer.Steering_finalize();

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Failed to finalize steering.", status);
    }
  }

}
