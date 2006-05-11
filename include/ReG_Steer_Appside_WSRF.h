/*----------------------------------------------------------------------------
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
---------------------------------------------------------------------------*/
#ifndef __REG_STEER_APPSIDE_WSRF_H__
#define __REG_STEER_APPSIDE_WSRF_H__

/** @internal
    @file ReG_Steer_Appside_WSRF.h
    @brief Header file for WSRF over SOAP communications 

    This header file contains routines and data structures for
    application-side WSRF, SOAP-based communication between the steered
    application and the Steering Web Service.
    @author Andrew Porter
  */

/** The username which the application will use within WS-Security
    when talking to the SWS */
#define REG_APPSIDE_WSSE_USERNAME "application"

/*-------------------------------------------------------------------*/

/** @internal
    @param NumSupportedCmds No. of commands supported by the application
    @param SupportedCmds Array containing the supported commands

    Set up the connection to the SWS and publish the supported
    commands to it */
int Initialize_steering_connection_wsrf(int  NumSupportedCmds,
					int *SupportedCmds);
/** @internal
    Check whether a steering client has connected to the SWS */
int Steerer_connected_wsrf ();

/** @internal
    Clean-up the connection to the SWS */
int Finalize_steering_connection_wsrf ();

/** @internal
    @param msg Buffer containing the status message to send

    Send the supplied status message to the SWS */
int Send_status_msg_wsrf (char *msg);

/** @internal
    Get the next control msg that has been sent to the app
    from a steering client */
struct msg_struct *Get_control_msg_wsrf ();

/** @internal
    @param log_data Buffer containing logging data (columns if parameter
    data, XML otherwise)

    Save a log (parameter or checkpoint) to the SWS */
int Save_log_wsrf (char *log_data);

/** @internal
    @param index Index of IOType for which to get address
    @param hostname The fully-qualified hostname of machine to connect to
    @param port     The port on that machine to connect to

    Query the SWS for the address to SEND data to for the
    specified IO channel */
int Get_data_sink_address_wsrf(const int           index, 
			       char               *hostname,
			       unsigned short int *port);

/** @internal
    @param index Index of IOType for which to get address
    @param hostname The fully-qualified hostname of machine to connect to
    @param port     The port on that machine to connect to
    @param label    Label of data source (when using IOProxy)

    Query the SWS for the address to connect to for the
    specified IO channel */
int Get_data_source_address_wsrf(const int          index, 
				 char               *hostname,
				 unsigned short int *port,
				 char               *label);

/** @internal
    Notify the SWS that the application has created a checkpoint
    @see Record_checkpoint_set_soap() */
int Record_checkpoint_set_wsrf (char *chk_data, 
				char *node_data);
#endif
