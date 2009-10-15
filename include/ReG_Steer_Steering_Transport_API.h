/*
  The RealityGrid Steering Library

  Copyright (c) 2002-2009, University of Manchester, United Kingdom.
  All rights reserved.

  This software is produced by Research Computing Services, University
  of Manchester as part of the RealityGrid project and associated
  follow on projects, funded by the EPSRC under grants GR/R67699/01,
  GR/R67699/02, GR/T27488/01, EP/C536452/1, EP/D500028/1,
  EP/F00561X/1.

  LICENCE TERMS

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

    * Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of The University of Manchester nor the names
      of its contributors may be used to endorse or promote products
      derived from this software without specific prior written
      permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
  COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.

  Author: Robert Haines
 */

#ifndef __REG_STEER_STEERING_TRANSPORT_API_H__
#define __REG_STEER_STEERING_TRANSPORT_API_H__

/** @file ReG_Steer_Steering_Transport_API.h 
 *  @brief The API specification for the steering transport modules.
 *
 *  @author Robert Haines
 */

#include "ReG_Steer_Config.h"
#include "ReG_Steer_types.h"
#include "ReG_Steer_Browser.h"
#include "ReG_Steer_XML.h"

/*-------- Appside function prototypes --------*/

/** @internal
    Removes files, signalling that client is attached and deletes
    any remaining files containing messages sent by client */
int Detach_from_steerer_impl();

/** @internal
    @return REG_SUCCESS if client is attempting to connect

    Check for a connection from a steering client. */
int Steerer_connected_impl();

/** @internal
    @param buf The status message to send

    Send the contents of @p buf to the steering client by writing to 
    an appropriate file. */
int Send_status_msg_impl(char *buf);

/** @internal
    Check for, and read, the next available message from an attached
    steering client. */
struct msg_struct *Get_control_msg_impl();

/** @internal
    @param NumSupportedCmds No. of commands support by application
    @param SupportedCmds Array containing the supported commands

    Set-up and advertise application as steerable */
int Initialize_steering_connection_impl(int  NumSupportedCmds,
					int *SupportedCmds);

/** @internal Take down any connection to a steering client */
int Finalize_steering_connection_impl();

/** @internal
    @param index Index of IOType to get address for
    @param direction The direction of the data that the IOType is used for,
           e.g. <code>REG_IN</code>, <code>REG_OUT</code> or
	   <code>REG_INOUT</code>.
    @param hostname hostname of machine on which data source is listening
    @param port Port on which data source is listening
    @param label The label of the IOType to be used. This only means anything
           when steering via a SWS otherwise this parameter is ignored.

    Obtain endpoint for a socket connection */
int Get_data_io_address_impl(const int index,
			     const int direction,
			     char* hostname, 
			     unsigned short int* port,
			     char* label);

/*-------- Steerside function prototypes --------*/

int Initialize_steerside_transport();
int Finalize_steerside_transport();

/** @internal
    @param index Index of entry in main Sim_table
    @param SimID Address of the simulation or the SWS representing the
           simulation
    
    Initialise soap-specific structures & attach to simulation via SOAP */
int Sim_attach_impl(int index, char *SimID);

/** @internal
    @param index Index of entry in main Sim_table
    @param sec Security structure to be populated.

    Initialise security information for a secure attach. */
int Sim_attach_security_impl(const int index,
			     const struct reg_security_info* sec);

/** @internal
    @param index Index of entry in main Sim_table
    @param no_block Whether to wait for a message or return immediatly if
           there is no message waiting.

    Gets the next status message from the simulation */
struct msg_struct *Get_status_msg_impl(int index, int no_block);

/** @internal 
    @param index Index of entry in main Sim_table
    @param buf The control message to send

    Send the supplied control message to the simulation */
int Send_control_msg_impl(int index, char* buf);

/** @internal
    @param index Index of entry in main Sim_table

    Send a detach msg to a simulation */
int Send_detach_msg_impl(int index);

/** @internal
    @param index Index of entry in main Sim_table

    Clean-up soap-specific structures */
int Finalize_connection_impl(int index);

/** @internal
    @param index Index of entry in main Sim_table
    @param handle Handle of the parameter to get the log of
    @return REG_SUCCESS or REG_FAILURE.

    Retrieve the full log of the parameter with the specified @p handle.
    If successful then log data is stored in internal buffer.
*/
int Get_param_log_impl(int index, int handle);

/** @internal
    Get the entries from a WSRF-based registry
    @param registryEPR Endpoint of the registry to query
    @param sec Pointer to struct holding authentication information
    @param contents Array of structs holding details on each entry */
int Get_registry_entries_impl(const char* registryEPR,
			      const struct reg_security_info* sec,
			      struct registry_contents* contents);

#endif /* __REG_STEER_STEERING_TRANSPORT_API_H__ */
