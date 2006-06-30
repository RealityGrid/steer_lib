/*----------------------------------------------------------------------------
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
---------------------------------------------------------------------------*/
#ifndef __REG_STEER_APPSIDE_SOAP_H__
#define __REG_STEER_APPSIDE_SOAP_H__

/** @internal
    @file ReG_Steer_Appside_Soap.h
    @brief Header file for SOAP-related Appside routines and structures
    @author Andrew Porter
    @author Robert Haines
*/

/** @internal 
    @param NumSupportedCmds No. of commands supported by the application
    @param SupportedCmds Array containing the supported commands
    Initialize the SOAP connection to a steering client. */
int Initialize_steering_connection_soap(int  NumSupportedCmds,
					int *SupportedCmds);

/** @internal Check whether a steering client is connected. */
int Steerer_connected_soap();

/** @internal
    @param msg String containing the status message to send

    Send a status message to an attached client via Soap */
int Send_status_msg_soap(char* msg);

/** @internal Detach from a steering client */
int Detach_from_steerer_soap();

/** @internal Retrieve a control message from attached client */
struct msg_struct *Get_control_msg_soap();

/** @internal Tidy up and take down connection to steering client */
int Finalize_steering_connection_soap();

/** @internal
    @param index Index of IOType to get address for
    @param hostname Fully-qualified hostname of machine hosting the 
    data source
    @param port The port on which the data source is listening

    Obtain endpoint for a socket connection from OGSI/WSRF framework */
int Get_data_source_address_soap(int                 index, 
				 char               *hostname,
				 unsigned short int *port);

/** @internal
    @param chk_data XML document holding information on the location
    of the files making up the checkpoint, @e e.g.: @n
    <tt>
    @verbatim
    <Checkpoint_data>
      <Chk_type>1001</Chk_type>
      <Chk_UID>check*t000076-0553007898</Chk_UID>
      <Files location="bezier.man.ac.uk">
        <file type="gsiftp-URL">
          gsiftp://bezier.man.ac.uk/home/blah/checkparams_sc03_t76-0553007898.xdr
        </file>
        <file type="gsiftp-URL">
          gsiftp://bezier.man.ac.uk/home/blah/checkpoint_sc03_t76-0553007898_p0000.xdr
        </file>
      </Files>
    </Checkpoint_data>
    @endverbatim
    </tt>

    @param node_data XML document holding a snapshot of all the parameter
    values at this checkpoint, @e e.g.: @n
    <tt>
    @verbatim
    <Checkpoint_node_data>
      <Param>
        <Handle>-100</Handle>
        <Label>SEQUENCE_NUM</Label>
        <Value>76</Value>
      </Param>
      <Param>
        <Handle>-99</Handle>
        <Label>CPU_TIME_PER_STEP</Label>
        <Value>0.200</Value>
      </Param>
    </Checkpoint_node_data>
    @endverbatim
    </tt>

    Record with the SGS/SWS that the application has taken a checkpoint.
    If a checkpoint tree is being used then the SGS/SWS creates a new
    node in the tree. */
int Record_checkpoint_set_soap(char *chk_data,
			       char *node_data);

/** @internal 
    @param log_data Buffer containing data to log.  If log
    is for parameter values then this is columnar data (\<handle1\>
    \<value1\> \<handle2\> \<value2\>...) otherwise it is XML
    conforming to the steering schema.
    
    Caches specified log on associated SGS/SWS */
int Save_log_soap(char *log_data);

#endif
