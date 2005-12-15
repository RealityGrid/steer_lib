/*----------------------------------------------------------------------------
  This header file contains routines and data structures for
  application-side WSRF, SOAP-based communication.

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

  Authors........: Andrew Porter

---------------------------------------------------------------------------*/
#ifndef __REG_STEER_APPSIDE_WSRF_H__
#define __REG_STEER_APPSIDE_WSRF_H__

/** @file ReG_Steer_Appside_WSRF.h
    @brief Header file for WSRF over SOAP communications for the 
    steered application
  */

/** The username which the application will use within WS-Security
    when talking to the SWS */
#define REG_APPSIDE_WSSE_USERNAME "application"

/*-------------------------------------------------------------------*/

/** Set up the connection to the SWS and publish the supported
    commands to it 
    @internal */
int Initialize_steering_connection_wsrf(int  NumSupportedCmds,
					int *SupportedCmds);
/** Check whether a steering client has connected to the SWS
    @internal */
int Steerer_connected_wsrf ();
/** Clean-up the connection to the SWS
    @internal */
int Finalize_steering_connection_wsrf ();
/** Send the supplied status message to the SWS 
    @internal */
int Send_status_msg_wsrf (char *msg);
/** Get the next control msg that has been sent to the app
    from a steering client 
    @internal */
struct msg_struct *Get_control_msg_wsrf ();
/** Save a log (parameter or checkpoint) to the SWS 
    @internal */
int Save_log_wsrf (char *log_data);

/** Query the SWS for the address to connect to for the
    specified IO channel
    @param index Index (generated from order registered) of channel for which to get address

    @param hostname The full-qualified hostname of machine to connect to
    @param port     The port on that machine to connect to
    @internal */
int Get_data_source_address_wsrf (int index, char *hostname, 
				  unsigned short int *port);

/** Notify the SWS that the application has created a checkpoint
    @internal */
int Record_checkpoint_set_wsrf (char *chk_data, 
				char *node_data);

#endif
