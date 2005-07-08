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

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

import org.realitygrid.steering.*;

public class MiniSteerer implements Runnable, ReG_SteerConstants {

  /* The Thread */
  private Thread t;

  /* The Steering Library */
  private ReG_SteerSteerside rss;

  /* global variables */
  private int simHandle;

  public static void main(String[] argv) {
    new MiniSteerer();
  }

  public MiniSteerer() {

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
    char userChar = '\0';
    System.out.print("Do (l)ocal or (r)emote attach [r]: ");
    try {
      userChar = readLine().charAt(0);
    }
    catch(IndexOutOfBoundsException e) {}

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
      try {
	String[][] list = rss.getSimList();

	for(int i = 0; i < list[0].length; i++) {
	  System.out.println(i + ": " + list[0][i] + "\n" + list[1][i]);
	}

	System.out.print("\nWhich one to attach to (0 - " + (list[0].length - 1) + "): ");
	int userInt = readInt();
	  
	simHandle = rss.simAttach(list[1][userInt]);
      }
      catch(ReG_SteerException e) {
	System.err.println("Attach failed!\n" + e);
	System.exit(e.getErrorCode());
      }
    }

    System.out.println("Attached to sim, simHandle = " + simHandle);

    /* Start the thread running */
    t = new Thread(this);
    t.start();
  }

  public void run() {

    boolean done = false;
    char userChar = '\0';

    /* enter steering loop */
    while(!done) {

      try {
	Thread.sleep(1000);
      }
      catch(InterruptedException e) {
	System.err.println("Interrupted!");
	//continue;
      }
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
	System.exit(e.getErrorCode());
      }

      /* Wait for a command from the user */
      System.out.print("\nCommand: ");
      try {
	userChar = readLine().charAt(0);
      }
      catch(IndexOutOfBoundsException e) {
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
	    System.out.println(ReG_SteerUtilities.commandLookup(commands[i]));
	  }
	  break;

	case 'g':
	  int[] message = rss.getNextMessage();

	  switch(message[1]) {
	  case SUPP_CMDS:
	  case MSG_NOTSET:
	    if(REG_DEBUG == 1) {
	      System.out.println("No message received");
	    }
	    break;
	  case IO_DEFS:
	    //rss.consumeIOTypeDefs(message[0]);
	    break;
	  case CHK_DEFS:
	    rss.consumeChkTypeDefs(message[0]);
	    break;
	  case PARAM_DEFS:
	    rss.consumeParamDefs(message[0]);
	    break;
	  case STATUS:
	    break;
	  case CONTROL:
	    System.out.println("Got control message");
	    break;
	  case STEER_LOG:
	    System.out.println("Got log message");
	    break;
	  default:
	    System.out.println("Unrecognised msg returned by getNextMessage");
	    break;
	  }

	  break;

	case 'h':
	  System.out.println("Possible commands are:");
	  System.out.println("  d - Display supported commands");
	  System.out.println("  g - Get next message from application");
	  System.out.println("  h - display this help message");
	  System.out.println("  p - send Pause signal to application");
	  System.out.println("  q - Quit steerer - detaches from application");
	  System.out.println("  r - send Resume signal to application");
	  System.out.println("  s - send Stop signal to application");
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
	default:
	  System.out.println("Unrecognised command...");
	  break;
	} // switch
      } // try
      catch(ReG_SteerException e) {
	System.err.println(e);
	rss.simDetach(simHandle);
	System.exit(e.getErrorCode());
      }

    } // while(!done)

    simHandle = rss.simDetach(simHandle);
    System.out.println("Detached from sim, sim_handle = " + simHandle);

    /* Detach (if not already detached) and clean up the steering lib */
    rss.steererFinalize();
  }

  private String readLine() {
    String input = null;

    try {
      BufferedReader stdin = new BufferedReader(new InputStreamReader(System.in));
      input = stdin.readLine();
    }
    catch(IOException e) {
      System.err.println("IOException: " + e);
      System.exit(1);
    }

    return input;
  }

  private int readInt() {
    int result = Integer.MAX_VALUE;
    String input = null;

    try {
      input = readLine();
      result = Integer.parseInt(input);
    }
    catch(NumberFormatException n) {
      System.err.println("Not a valid number: " + input);
      System.exit(1);
    }

    return result;
  }

}
