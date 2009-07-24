/*----------------------------------------------------------------------------
  (C) Copyright 2009, University of Manchester, United Kingdom,
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

#ifndef __REG_STEER_STEERING_TRANSPORT_API_H__
#define __REG_STEER_STEERING_TRANSPORT_API_H__

/** @file ReG_Steer_Steering_Transport_API.h 
 *  @brief Some file...
 *
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
    @param hostname hostname of machine on which data source is listening
    @param port Port on which data source is listening

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
    @param sim Pointer to entry in main Sim_table
    @param SimID GSH of the SGS representing the simulation
    
    Initialise soap-specific structures & attach to simulation via SOAP */
int Sim_attach_impl(int index, char *SimID);

int Sim_attach_security_impl(const int index,
			     const struct reg_security_info* sec);

/** @internal
    @param sim Pointer to entry in main Sim_table

    Gets the next status message from the simulation */
struct msg_struct *Get_status_msg_impl(int index, int no_block);

/** @internal 
    @param sim Pointer to entry in main Sim_table
    @param buf The control message to send

    Send the supplied control message to the simulation */
int Send_control_msg_impl(int index, char* buf);

/** @internal
    @param index Index of entry in main Sim_table

    Send a detach msg to a simulation */
int Send_detach_msg_impl(int index);

/** @internal
    @param sim Pointer to entry in main Sim_table

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
