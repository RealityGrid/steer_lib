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
#ifndef __REG_STEER_APPSIDE_DIRECT_H__
#define __REG_STEER_APPSIDE_DIRECT_H__

/** @internal
    @file ReG_Steer_Appside_Direct.h
    @brief Header file for direct-related Appside routines and structures
    @author Andrew Porter
    @author Robert Haines
  */

#include "ReG_Steer_types.h"
#include "ReG_Steer_Common.h"
#include "ReG_Steer_Appside_internal.h"

#ifndef REG_SOCKETS_ERROR
#define REG_SOCKETS_ERROR -1
#endif

/*-------- Function prototypes --------*/

int socket_info_init_direct(Direct_info_type*);

/** @internal
    Removes files, signalling that client is attached and deletes
    any remaining files containing messages sent by client */
int Detach_from_steerer_direct();

/** @internal
    @return REG_SUCCESS if client is attempting to connect

    Check for a connection from a steering client. */
int Steerer_connected_direct();

/** @internal
    @param filename Root of filename for status message

    Generate a filename (for a status message) from supplied root and 
    internally-generated index */
/*int Generate_status_filename(char* filename);*/

/** @internal
    @param buf The status message to send

    Send the contents of @p buf to the steering client by writing to 
    an appropriate file. */
int Send_status_msg_direct(Direct_info_type*, char *buf);

/** @internal
    Check for, and read, the next available message from an attached
    steering client. */
struct msg_struct *Get_control_msg_direct(Direct_info_type*);

/** @internal
    @param NumSupportedCmds No. of commands support by application
    @param SupportedCmds Array containing the supported commands

    Set-up and advertise application as steerable - write a lock
    file in the REG_STEER_DIRECTORY directory. */
int Initialize_steering_connection_direct(int  NumSupportedCmds,
					int *SupportedCmds);

/** @internal Take down any connection to a steering client */
int Finalize_steering_connection_direct();

/** @internal
    @param index Index of IOType to get address for
    @param hostname hostname of machine on which data source is listening
    @param port Port on which data source is listening

    Obtain endpoint for a socket connection */
int Get_data_source_address_direct(int index,
				   char* hostname, 
				   unsigned short int* port);

int Create_steerer_listener(Direct_info_type*);
void poll_steerer_socket(Direct_info_type*);
int send_steerer_msg(Direct_info_type*, const size_t, void*);
#endif
