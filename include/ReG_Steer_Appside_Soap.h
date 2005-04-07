/*----------------------------------------------------------------------------
  This header file contains routines and data structures for
  SOAP-based steering communication.

  (C) Copyright 2002, 2004, University of Manchester, United Kingdom,
  all rights reserved.

  This software is produced by the Supercomputing, Visualization and
  e-Science Group, Manchester Computing, University of Manchester
  as part of the RealityGrid project (http://www.realitygrid.org),
  funded by the EPSRC under grants GR/R67699/01 and GR/R67699/02.

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

  Authors........: Andrew Porter, Robert Haines

---------------------------------------------------------------------------*/
#ifndef __REG_STEER_APPSIDE_SOAP_H__
#define __REG_STEER_APPSIDE_SOAP_H__

/** @file ReG_Steer_Appside_Soap.h
    @brief Header file for Soap-related Appside routines
  */

/*------------------------------------------------------------------*/

/** Initialize the connection to a steering client.

    */
int Initialize_steering_connection_soap(int  NumSupportedCmds,
					int *SupportedCmds);

/** Check whether a steering client is connected.

    */
int Steerer_connected_soap();

/** Send a status message to an attached client via Soap

 */
int Send_status_msg_soap(char* msg);

/** Detach from a steering client

*/
int Detach_from_steerer_soap();

/** Retrieve a control message from attached client

*/
struct msg_struct *Get_control_msg_soap();

/** Tidy up and take down connection to steering client

*/
int Finalize_steering_connection_soap();

/** Obtain endpoint for a socket connection from OGSI framework

*/
int Get_data_source_address_soap(int                 index, 
				 char               *hostname,
				 unsigned short int *port);

/** Record that application has taken a checkpoint

*/
int Record_checkpoint_set_soap(char *chk_data,
			       char *node_data);

/** Caches specified log on associated SGS

*/
int Save_log_soap(char *log_data);

#endif
