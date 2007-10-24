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

import org.realitygrid.steering.*;
import org.realitygrid.utilities.IO;

public class MiniSteererSecure implements ReG_SteerConstants {

  /* The Steering Library */
  private ReG_SteerSteerside rss;

  /* global variables */
  private String registryAddress;
  private int simHandle;
  private ReG_SteerSecurity regSec;

  public static void main(String[] argv) {
    MiniSteererSecure ms = new MiniSteererSecure();
    ms.run();
  }

  public MiniSteererSecure() {

    rss = ReG_SteerSteerside.getInstance();

    /* Initialise the library */
    try {
      rss.steererInitialize();
    }
    catch(ReG_SteerException e) {
      System.err.println(e);
      System.exit(e.getErrorCode());
    }

    /* ask the user which sim to attach to */
    System.out.print("Do (l)ocal or (r)emote attach [r]: ");
    char userChar = IO.readChar();

    if(userChar == 'l' || userChar == 'L') {
      try {
	simHandle = rss.simAttach("");
      }
      catch(ReG_SteerException e) {
	System.err.println(e);
	System.exit(e.getErrorCode());
      }
    }
    else {
      // Get registry address...
      System.out.print("Enter address of registry and hit return: ");
      registryAddress = IO.readLine();

      // set up security info - we need this even without SSL
      String userConfFile = System.getProperty("user.home") + "/.realitygrid/security.conf";
      regSec = new ReG_SteerSecurity(userConfFile);

      // Using SSL?
      if(registryAddress.startsWith("https")) {
	// yes
	regSec.setUsingSSL(true);

	System.out.print("Enter passphrase for key: ");
	regSec.setPassphrase(IO.readPassword());
      }
      else {
	// no
	regSec.setUsingSSL(false);
	System.out.print("Enter your username [" + regSec.getUserDN() + "]: ");
	String userName = IO.readLine();
	if(userName.length() > 0) {
	  regSec.setUserDN(userName);
	}

	System.out.print("Enter the password for the registry: ");
	regSec.setPassphrase(IO.readPassword());
      }

      System.out.println("Security:\n" + regSec);
      ReG_SteerRegistryEntry[] list = rss.getRegistryEntriesFilteredSecure(registryAddress, regSec, "SWS");

      if(list.length > 0) {
	for(int i = 0; i < list.length; i++) {
	  System.out.print("\n" + i + ": " + list[i].getApplication());
          System.out.print(", gsh = " + list[i].getGSH());
        }

        System.out.print("\n\nWhich one to attach to (0 - " + (list.length - 1) + "): ");
        int userInt = IO.readInt();

	System.out.print("Enter password for SWS: ");
	regSec.setPassphrase(IO.readPassword());

        try {
	  simHandle = rss.simAttachSecure(list[userInt].getGSH(), regSec);
        }
        catch(ReG_SteerException e) {
	  System.err.println("Attach failed!\n" + e);
	  System.exit(e.getErrorCode());
        }
      }
      else {
	System.out.println("Nothing to attach to!");
	System.exit(0);
      }
    }

    System.out.println("Attached to sim, simHandle = " + simHandle);
  }

  private void run() {

    boolean done = false;
    char userChar = '\0';
    int pHandle = REG_PARAM_HANDLE_NOTSET;

    /* enter steering loop */
    while(!done) {

      try {
	int numParams = rss.getParamNumber(simHandle, false);
	System.out.println("\nHave " + numParams + " monitored parameters:");

	ReG_SteerParameter[] monParams = rss.getParamValues(simHandle, false, numParams);
	for(int i = 0; i < numParams; i++)
	  System.out.println(monParams[i]);

	numParams = rss.getParamNumber(simHandle, true);
	System.out.println("\nHave " + numParams + " steerable parameters:");

	ReG_SteerParameter[] stParams = rss.getParamValues(simHandle, true, numParams);
	for(int i = 0; i < numParams; i++)
	  System.out.println(stParams[i]);

      }
      catch(ReG_SteerException e) {
	System.err.println(e);
	rss.simDetach(simHandle);
	System.exit(e.getErrorCode());
      }

      /* Wait for a command from the user */
      System.out.print("\nCommand: ");
      userChar = IO.readChar();
      if(userChar == '\0') {
	userChar = 'h';
      }

      try {
	switch(userChar) {
	case 'd':
	  int numCmds = rss.getSuppCmdNumber(simHandle);
	  System.out.println("We have " + numCmds + " supported commands:");

	  int[] commands = rss.getSuppCmds(simHandle, numCmds);
	  for(int i = 0; i < numCmds; i++) {
	    System.out.print("Supported command " + i + " = ");
	    System.out.println(ReG_SteerUtilities.lookupCommand(commands[i]));
	  }
	  break;

	case 'e':
	  int[] pHandles = new int[1];
	  pHandles[0] = chooseParam(simHandle, true);
	  String[] newValues = new String[1];
	  if(pHandles[0] != REG_PARAM_HANDLE_NOTSET) {
	    System.out.print("Enter new value for param: ");
	    newValues[0] = IO.readLine();
	    rss.setParamValues(simHandle, pHandles, newValues);

	    // emit control automatically emits the values of any steerable
	    // parameters that have been edited since it was last called
	    rss.emitControl(simHandle, null, null);
	  }
	  break;

	case 'g':
	  int[] message = rss.getNextMessage();

	  switch(message[1]) {
	  case SUPP_CMDS:
	    // supported commands should only be output once as part
	    // of handshaking process!
	    if(REG_DEBUG == 1) {
	      System.out.println("ERROR: Got supported commands message\n");
	    }
	    break;
	  case MSG_NOTSET:
	    if(REG_DEBUG == 1) {
	      System.out.println("No message received");
	    }
	    break;
	  case IO_DEFS:
	    System.out.println("Got IO definitions");
	    rss.consumeIOTypeDefs(message[0]);
	    break;
	  case CHK_DEFS:
	    System.out.println("Got checkpoint definitions");
	    rss.consumeChkTypeDefs(message[0]);
	    break;
	  case PARAM_DEFS:
	    System.out.println("Got parameter definitions");
	    rss.consumeParamDefs(message[0]);
	    break;
	  case STATUS:
	    System.out.println("Got status message");
	    int[] cmds = new int[REG_MAX_NUM_STR_CMDS];
	    int seqNum = rss.consumeStatus(simHandle, cmds);

	    for(int i = 0; i < cmds.length; i++) {
	      switch(cmds[i]) {
	      case REG_STR_STOP:
	      case REG_STR_DETACH:
		simHandle = rss.deleteSimTableEntry(simHandle);
		done = true;
		break;
	      default:
		break;
	      }
	      if(done) break;
	    }
	    System.out.println("Application seqNum: " + seqNum);
	    
	    break;
	  case CONTROL:
	    System.out.println("Got control message");
	    break;
	  case STEER_LOG:
	    System.out.println("Got log message");
	    break;
	  default:
	    System.out.println("Unrecognised msg returned by getNextMessage.");
	    break;
	  }

	  break;

	case 'h':
	  System.out.println("Possible commands are:");
	  System.out.println("  d - Display supported commands");
	  System.out.println("  e - Edit steerable parameter");
	  System.out.println("  g - Get next message from application");
	  System.out.println("  h - Display this help message");
	  System.out.println("  l - Display list of IO and checkpoint types");
	  System.out.println("  o - retrieve param hist. log from application");
	  System.out.println("  p - Send Pause signal to application");
	  System.out.println("  q - Quit steerer - detaches from application");
	  System.out.println("  r - Send Resume signal to application");
	  System.out.println("  s - Send Stop signal to application");
	  System.out.println("  V - View logged parameters - SOON");
	  break;

	case 'l':
	  // get io types
	  int numIOs = rss.getIOTypeNumber(simHandle);

	  if(numIOs > 0) {
	    int[] ioHandles = new int[numIOs];
	    String[] ioLabels = new String[numIOs];
	    int[] ioDirs = new int[numIOs];
	    int[] ioFreqs = new int[numIOs];

	    rss.getIOTypes(simHandle, ioHandles, ioLabels, ioDirs, ioFreqs);

	    System.out.println("\nIO Types: " + numIOs);
	    for(int i = 0; i < numIOs; i++) {
	      System.out.println("IOType# " + i + "\n  freq: " + ioFreqs[i]);
	      System.out.println("  label: " + ioLabels[i]);
	      System.out.println("  direction: " + ReG_SteerUtilities.lookupDirection(ioDirs[i]));
	    }
	  }

	  // get checkpoint types
	  int numChks = rss.getChkTypeNumber(simHandle);

	  if(numChks > 0) {
	    int[] chkHandles = new int[numChks];
	    String[] chkLabels = new String[numChks];
	    int[] chkDirs = new int[numChks];
	    int[] chkFreqs = new int[numChks];

	    rss.getChkTypes(simHandle, chkHandles, chkLabels, chkDirs, chkFreqs);

	    System.out.println("\nCheckpoint Types: " + numChks);
	    for(int i = 0; i < numChks; i++) {
	      System.out.println("ChkType# " + i + "\n  freq: " + chkFreqs[i]);
	      System.out.println("  label: " + chkLabels[i]);
	      System.out.println("  direction: " + ReG_SteerUtilities.lookupDirection(chkDirs[i]));
	    }
	  }
	  break;

	case 'o':
	  pHandle = chooseParam(simHandle, false);
	  if(pHandle != REG_PARAM_HANDLE_NOTSET) 
	    rss.emitRetrieveParamLogCmd(simHandle, pHandle);
	  break;

	case 'p':
	  System.out.println("Pausing application...");
	  rss.emitPauseCmd(simHandle);
	  break;

	case 'q':
	  done = true;
	  break;

	case 'r':
	  System.out.println("Resuming application...");
	  rss.emitResumeCmd(simHandle);
	  break;

	case 's':
	  System.out.println("Sending Stop signal...");
	  rss.emitStopCmd(simHandle);
	  break;

	case 'V':
	  //pHandle = chooseParam(simHandle, false);
	  //if(pHandle == REG_PARAM_HANDLE_NOTSET) break;
	  break;

	default:
	  System.out.println("Unrecognised command...");
	  break;
	} // switch
      } // try
      catch(ReG_SteerException e) {
	System.err.println(e);
	if(e.isFatal()) {
	  rss.simDetach(simHandle);
	  System.exit(e.getErrorCode());
	}
      }

    } // while(!done)

    simHandle = rss.simDetach(simHandle);
    System.out.println("Detached from sim, sim_handle = " + simHandle);

    /* Detach (if not already detached) and clean up the steering lib */
    rss.steererFinalize();
  }

  private int chooseParam(int simHandle, boolean steer) {
    
    int numParams = rss.getParamNumber(simHandle, steer);
    ReG_SteerParameter[] params = rss.getParamValues(simHandle, steer, numParams);

    System.out.println("Which parameter to select?\n");
    for(int i = 0; i < numParams; i++) {
      System.out.println(i + ": " + params[i]);
    }

    System.out.print("Enter choice or 'c' to cancel: ");
    String userInput = IO.readLine();
    int sel;
    try{
      if(userInput.charAt(0) == 'c')
	return REG_PARAM_HANDLE_NOTSET;

      sel = Integer.parseInt(userInput);
    }
    catch(IndexOutOfBoundsException ie) {
      return REG_PARAM_HANDLE_NOTSET;
    }
    catch(NumberFormatException ne) {
      System.out.println("Not a number: " + userInput);
      return REG_PARAM_HANDLE_NOTSET;      
    }

    if(sel < 0 || sel >= numParams) {
      System.out.println("Not a valid selection: " + sel);
      return REG_PARAM_HANDLE_NOTSET;
    }

    return params[sel].getHandle();
  }

}
