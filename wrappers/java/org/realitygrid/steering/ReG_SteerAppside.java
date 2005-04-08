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

public class ReG_SteerAppside implements ReG_SteerConstants {

  private static ReG_SteerAppside instance = new ReG_SteerAppside();

  private int status;

  private String[] changedParamLabels;
  private int[] receivedCommands;
  private String[] receivedCommandParams;

  /**
   * Private constructor.
   */
  private ReG_SteerAppside() {
    changedParamLabels = new String[REG_MAX_NUM_STR_PARAMS];
    receivedCommands = new int[REG_MAX_NUM_STR_CMDS];
    receivedCommandParams = new String[REG_MAX_NUM_STR_CMDS];
  }

  public static ReG_SteerAppside getInstance() {
    return instance;
  }

  public String[] getChangedParamLabels() {
    return changedParamLabels;
  }

  public int[] getReceivedCommands() {
    return receivedCommands;
  }

  public String[] getReceivedCommandParams() {
    return receivedCommandParams;
  }

  public void steeringEnable(boolean b) {
    if(b)
      ReG_Steer.Steering_enable(REG_TRUE);
    else
      ReG_Steer.Steering_enable(REG_FALSE);
  }

  public void steeringInitialize(String s, int[] ia) throws ReG_SteerException {
    status = ReG_Steer.Steering_initialize(s, ia);
    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Failed to initialize steering.", status);
    }
  }

  public int registerIOType(String name, int dir, int freq) throws ReG_SteerException {
    Intp iotype_handle = new Intp();

    status = ReG_Steer.Register_IOType(name, dir, freq, iotype_handle.cast());
    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Failed to register IO type: " + name, status);
    }

    return iotype_handle.value();
  }

  public void enableIOTypesOnRegistration(boolean b) throws ReG_SteerException {
    if(b)
      status = ReG_Steer.Enable_IOTypes_on_registration(REG_TRUE);
    else
      status = ReG_Steer.Enable_IOTypes_on_registration(REG_FALSE);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("IOTypesOnRegistration failed", status);
    }
  }

  public void enableIOType(int iot) throws ReG_SteerException {
    status = ReG_Steer.Enable_IOType(iot);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Could not enable IO Type", status);
    }
  }

  public void disableIOType(int iot) throws ReG_SteerException {
    status = ReG_Steer.Disable_IOType(iot);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Could not disable IO Type", status);
    }
  }

  public void enableIOTypeAcks(int iot) throws ReG_SteerException {
    status = ReG_Steer.Enable_IOType_acks(iot);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Could not enable IO Type Acknowledgements", status);
    }
  }

  public void disableIOTypeAcks(int iot) throws ReG_SteerException {
    status = ReG_Steer.Disable_IOType_acks(iot);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Could not disable IO Type Acknowledgements", status);
    }
  }

  public void registerParam(ReG_SteerParameter param) throws ReG_SteerException {
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

  public int[] steeringControl(int i) throws ReG_SteerException {
    int[] result = new int[2];
    Intp numParamsChanged = new Intp();
    Intp numReceivedCommands = new Intp();

    status = ReG_Steer.Steering_control(i,
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

  public int emitStart(int ioth, int seq) throws ReG_SteerException {
    Intp iohandle = new Intp();

    status = ReG_Steer.Emit_start(ioth, seq, iohandle.cast());

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Emit start failed.", status);
    }

    return iohandle.value();
  }

  public int emitStart(int ioth, int seq, float timeout) throws ReG_SteerException {
    Intp iohandle = new Intp();

    status = ReG_Steer.Emit_start_blocking(ioth, seq, iohandle.cast(), timeout);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Emit start failed.", status);
    }

    return iohandle.value();
  }

  public void emitDataSlice(int ioth, Object data) throws ReG_SteerException {
    Object dataSlice;
    if(data instanceof char[]) 
      dataSlice = new String((char[]) data);
    else
      dataSlice = data;

    status = ReG_Steer.Emit_data_slice(ioth, dataSlice);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Emit data slice failed.", status);
    }
  }

  public int emitStop(int ioh) throws ReG_SteerException {
    Intp iohandle = new Intp();
    iohandle.assign(ioh);

    status = ReG_Steer.Emit_stop(iohandle.cast());

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Emit stop failed.", status);
    }

    return iohandle.value();
  }

  public int consumeStart(int ioth) throws ReG_SteerException {
    Intp iohandle = new Intp();

    status = ReG_Steer.Consume_start(ioth, iohandle.cast());

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("No data to consume.", status);
    }

    return iohandle.value();
  }

  public int consumeStart(int ioth, float timeout) throws ReG_SteerException {
    Intp iohandle = new Intp();

    status = ReG_Steer.Consume_start_blocking(ioth, iohandle.cast(), timeout);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("No data to consume.", status);      
    }

    return iohandle.value();
  }

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

  public int consumeStop(int ioh) throws ReG_SteerException {
    Intp iohandle = new Intp();
    iohandle.assign(ioh);

    status = ReG_Steer.Consume_stop(iohandle.cast());

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Failed to stop consuming!", status);
    }

    return iohandle.value();
  }

  public void steeringFinalize() throws ReG_SteerException {
    status = ReG_Steer.Steering_finalize();

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Failed to finalize steering.", status);
    }
  }

  public static int sizeof(int type) throws ReG_SteerException {
    int size = ReG_Steer.Sizeof(type);
    if(size == 0) {
      throw new ReG_SteerException("Invalid type supplied to sizeof.");
    }

    return size;
  }

}
