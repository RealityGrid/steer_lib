/* ----------------------------------------------------------------------------
  This file is part of the RealityGrid Steering Library Java Wrappers.
 
  (C) Copyright 2006, University of Manchester, United Kingdom,
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

public class ReG_SteerSteerside implements ReG_SteerConstants {
  
  private static ReG_SteerSteerside instance = new ReG_SteerSteerside();

  private int status;

  /**
   * Private constructor. Use <code>getInstance</code> to create an instance
   * of this class.
   */
  private ReG_SteerSteerside() {
  }

  /**
   * This method is used to obtain an instance of this class. Only one
   * instance of this class is allowed, so there are only private
   * constructors and all users of this instance will be using the
   * same instance of the steering library.
   *
   * @return the single instance of ReG_SteerSteerside.
   */
  public static ReG_SteerSteerside getInstance() {
    return instance;
  }

  /**
   * Initialise the internal tables <em>etc.</em> used by the steering library
   * on the steering client side. This must be called before all other steering
   * library methods.
   *
   * @throws ReG_SteerException If initialisation fails,
   * <code>REG_FAILURE</code> is returned as the error code.
   *
   * @see #steererFinalize()
   */
  public void steererInitialize() throws ReG_SteerException {
    status = ReG_Steer.Steerer_initialize();

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Could not initialise the steerer", status);
    }
  }

  /**
   * Cleans up the internal tables, <em>etc.</em> used by the client-side
   * steering library. This should be called after all steering activity is
   * complete.
   *
   * @see #steererInitialize()
   */
  public void steererFinalize() {
    status = ReG_Steer.Steerer_finalize();
  }

  /**
   * This method queries available middleware for a list of steerable
   * applications. The results of this method are used as input to
   * <code>simAttach</code>.
   *
   * @return a two dimensional array of Strings. The first entry in this array
   * is an array of application names and the second is an array of application
   * Grid Service Handles (GSHs). If no steerable applications are found this
   * method returns <code>null</code>.
   *
   * @throws ReG_SteerException If no steerable applications are found the
   * errorCode will be <code>REG_FAILURE</code>.
   *
   * @see #simAttach
   */
  public String[][] getSimList() throws ReG_SteerException {
    Intp nSims = new Intp();
    String[] simNames = new String[REG_MAX_NUM_STEERED_SIM];
    String[] simGSHs = new String[REG_MAX_NUM_STEERED_SIM];
    String[][] simList = null;

    status = ReG_Steer.Get_sim_list(nSims.cast(), simNames, simGSHs);

    if((status != REG_SUCCESS) || (nSims.value() <= 0)) {
      throw new ReG_SteerException("Could not find a list of steerable applications.", status);
    }
    else {
      simList = new String[2][nSims.value()];
      for(int i = 0; i < nSims.value(); i++) {
	simList[0][i] = simNames[i];
	simList[1][i] = simGSHs[i];
      }
    }

    return simList;
  }

  /**
   *
   */
  public ReG_SteerRegistryEntry[] getRegistryEntriesSecure(String regAddress,
						      ReG_SteerSecurity sec) {
    return (ReG_SteerRegistryEntry[]) ReG_Steer.Get_registry_entries_secure_j(regAddress, sec);
  }

  /**
   *
   */
  public ReG_SteerRegistryEntry[] getRegistryEntriesFilteredSecure(String regAddress,
						      ReG_SteerSecurity sec,
						      String pattern) {
    return (ReG_SteerRegistryEntry[]) ReG_Steer.Get_registry_entries_filtered_secure_j(regAddress, sec, pattern);
  }

  /**
   *
   */
  public int simAttach(String simID) throws ReG_SteerException {
    Intp simHandle = new Intp();
    String where = "remote";

    if(simID == "") {
      where = "local";
    }

    status = ReG_Steer.Sim_attach(simID, simHandle.cast());

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Could not attach to " + where + " simulation.", status);
    }

    return simHandle.value();
  }

  /**
   *
   */
  public int simAttachSecure(String simID, ReG_SteerSecurity sec) throws ReG_SteerException {
    Intp simHandle = new Intp();
    String where = "remote";

    if(simID == "") {
      where = "local";
    }

    status = ReG_Steer.Sim_attach_secure(simID, sec, simHandle.cast());

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Could not attach to " + where + " simulation.", status);
    }

    return simHandle.value();
  }

  /**
   *
   */
  public int simDetach(int simHandle) {
    Intp sh = new Intp();
    sh.assign(simHandle);

    status = ReG_Steer.Sim_detach(sh.cast());

    return sh.value();
  }

  /**
   *
   */
  public int deleteSimTableEntry(int simHandle) {
    Intp sh = new Intp();
    sh.assign(simHandle);

    status = ReG_Steer.Delete_sim_table_entry(sh.cast());

    return sh.value();
  }

  /**
   *
   */
  public int getParamNumber(int simHandle, boolean steerable) 
    throws ReG_SteerException {
    
    int steer = REG_TRUE;
    Intp numParams = new Intp();
    String which = "steerable";

    if(!steerable) {
      steer = REG_FALSE;
      which = "monitored";
    }

    status = ReG_Steer.Get_param_number(simHandle, steer, numParams.cast());

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Failed to get number of " + which + " parameters.", status);
    }

    return numParams.value();
  }

  /**
   *
   */
  public ReG_SteerParameter[] getParamValues(int simHandle, boolean steerable, int numParams) {

    int steer = REG_TRUE;
    String which = "steerable";
    ReG_SteerParameter[] values; // = new ReG_SteerParameter[numParams];

    if(!steerable) {
      steer = REG_FALSE;
      which = "monitored";
    }

    values = (ReG_SteerParameter[]) ReG_Steer.Get_param_values_j(simHandle, steer, numParams);

    return values;
  }

  /**
   *
   */
  public void setParamValues(int simHandle, int[] handles, String[] values) throws ReG_SteerException {
    status = ReG_Steer.Set_param_values(simHandle, handles.length, handles, values);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Failed to set new param values.", status);
    }
  }

  /**
   *
   */
  public int getSuppCmdNumber(int simHandle) throws ReG_SteerException {
    Intp numCmds = new Intp();

    status = ReG_Steer.Get_supp_cmd_number(simHandle, numCmds.cast());

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Could not get the number of supported commands from the simulation.", status);
    }

    return numCmds.value();
  }

  /**
   *
   */
  public int[] getSuppCmds(int simHandle, int numCmds) {
    int[] cmdIDs = new int[numCmds];    

    status = ReG_Steer.Get_supp_cmds(simHandle, numCmds, cmdIDs);

    return cmdIDs;
  }

  /**
   * A wrapper for generating a Stop command and sending it to the application.
   *
   * @param simHandle the handle of the simulation to send the command to.
   *
   * @throws ReG_SteerException If Stop command could not be issued.
   */
  public void emitStopCmd(int simHandle) throws ReG_SteerException {
    status = ReG_Steer.Emit_stop_cmd(simHandle);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Could not issue Stop command.", status);
    }
  }

  /**
   * A wrapper for generating a Pause command and sending it to the
   * application.
   *
   * @param simHandle the handle of the simulation to send the command to.
   *
   * @throws ReG_SteerException If Pause command could not be issued.
   */
  public void emitPauseCmd(int simHandle) throws ReG_SteerException {
    status = ReG_Steer.Emit_pause_cmd(simHandle);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Could not issue Pause command.", status);
    }
  }

  /**
   * A wrapper for generating a Resume command and sending it to the
   * application.
   *
   * @param simHandle the handle of the simulation to send the command to.
   *
   * @throws ReG_SteerException If Resume command could not be issued.
   */
  public void emitResumeCmd(int simHandle) throws ReG_SteerException {
    status = ReG_Steer.Emit_resume_cmd(simHandle);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Could not issue Resume command.", status);
    }
  }

  /**
   * Look for the next message from any attached simulations. If one is found
   * then the handle of the originating simulation along with the type of the
   * message is returned.
   *
   * @return an integer array of length two holding the handle of the
   * originating simulation and the type of message, in that order.
   *
   * @throws ReG_SteerException If no messages are found,
   * <code>REG_FAILURE</code> is returned as the error code.
   */
  public int[] getNextMessage() throws ReG_SteerException {
    Intp simHandle = new Intp();
    Intp msgType = new Intp();
    int[] result = new int[2];

    status = ReG_Steer.Get_next_message(simHandle.cast(), msgType.cast());

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("No messages found.", status);
    }

    result[0] = simHandle.value();
    result[1] = msgType.value();

    return result;
  }

  /**
   * Consume the parameter definitions emitted by the steered application.
   * The steering library's internal table of parameters is updated ready for
   * querying.
   *
   * @param simHandle the handle of the simulation to query for parameter
   * definitions.
   *
   * @throws ReG_SteerException If consumption of the parameter definitions
   * fails the error code will be <code>REG_FAILURE</code>.
   */
  public void consumeParamDefs(int simHandle) throws ReG_SteerException {
    
    status = ReG_Steer.Consume_param_defs(simHandle);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Could not consume parameter definitions.", status);
    }
  }

  /**
   * Consume the IOType descriptions emitted by the steered application.
   * These descriptions provide information to be displayed in steering clients
   * in order to allow the user to request sample data to be emitted and
   * consumed.
   *
   * @param simHandle the handle of the simulation to query for IOType
   * descriptions.
   *
   * @throws ReG_SteerException If consumption of the IOType descriptions
   * fails the error code will be <code>REG_FAILURE</code>.
   */
  public void consumeIOTypeDefs(int simHandle) throws ReG_SteerException {
    
    status = ReG_Steer.Consume_IOType_defs(simHandle);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Could not consume IOType descriptions.", status);
    }
  }

  /**
   *
   */
  public void consumeChkTypeDefs(int simHandle) throws ReG_SteerException {
    
    status = ReG_Steer.Consume_ChkType_defs(simHandle);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Could not consume checkpoint type definitions.", status);
    }
  }

  /**
   *
   */
  public void emitControl(int simHandle, int[] sysCmds, String[] sysCmdParams) throws ReG_SteerException {
    int numCmds = 0;
    if(sysCmds != null) {
      numCmds = sysCmds.length;
    }

    status = ReG_Steer.Emit_control(simHandle, numCmds, sysCmds, sysCmdParams);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Emit control failed.", status);
    }
  }

  /**
   *
   */
  public int getIOTypeNumber(int simHandle) throws ReG_SteerException {
    Intp numIOTypes = new Intp();

    status = ReG_Steer.Get_iotype_number(simHandle, numIOTypes.cast());

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Could not get number of IO types.", status);
    }

    return numIOTypes.value();
  }

  /**
   *
   */
  public void getIOTypes(int simHandle, int[] ioHandles, String[] ioLabels, int[] ioTypes, int[] ioFreqs) throws ReG_SteerException {
    int numIOs = ioHandles.length;

    status = ReG_Steer.Get_iotypes(simHandle, numIOs, ioHandles, ioLabels, ioTypes, ioFreqs);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("NO!", status);
    }
  }

  /**
   *
   */
  public int getChkTypeNumber(int simHandle) throws ReG_SteerException {
    Intp numChkTypes = new Intp();

    status = ReG_Steer.Get_chktype_number(simHandle, numChkTypes.cast());

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Could not get number of checkpoint types.", status);
    }

    return numChkTypes.value();
  }

  /**
   *
   */
  public void getChkTypes(int simHandle, int[] chkHandles, String[] chkLabels, int[] chkTypes, int[] chkFreqs) throws ReG_SteerException {
    int numChks = chkHandles.length;

    status = ReG_Steer.Get_chktypes(simHandle, numChks, chkHandles, chkLabels, chkTypes, chkFreqs);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("NO!", status);
    }
  }

  /**
   *
   */
  public int consumeStatus(int simHandle, int[] commands) throws ReG_SteerException {
    Intp seqNum = new Intp();
    Intp numCmds = new Intp();

    status = ReG_Steer.Consume_status(simHandle, seqNum.cast(), numCmds.cast(), commands);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("NO, no, no!", status);
    }

    return seqNum.value();
  }

  /**
   *
   */
  public void emitRetrieveParamLogCmd(int simHandle, int paramHandle) throws ReG_SteerException {
    
    status = ReG_Steer.Emit_retrieve_param_log_cmd(simHandle, paramHandle);

    if(status != REG_SUCCESS) {
      throw new ReG_SteerException("Could not emit param log command.", status);
    }
  }

}
