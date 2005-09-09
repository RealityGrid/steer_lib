/*----------------------------------------------------------------------------
  This header file contains routines and data structures for
  communication using Sockets.

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

  Authors........: Andrew Porter, Robert Haines

---------------------------------------------------------------------------*/
#ifndef __REG_STEER_IOPROXY_H__
#define __REG_STEER_IOPROXY_H__

/** @file ReG_Steer_Appside_IOProxy.h
    @brief Header file for proxy-related routines for data IO
  */

/* ARP - added to uniquely identify TRU64 systems.  Necessary on
   Alpha workstations and LeMieux */
#ifndef TRU64
#if (defined (__digital__) && defined (__unix__))
#define TRU64
#endif
#endif

#if REG_PROXY_SAMPLES

#include <errno.h>
#if defined(TRU64)
#include <fcntl.h>
#endif
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>

#include "ReG_Steer_types.h"

#define REG_SOCKETS_ERROR -1

/*-------- Function prototypes --------*/

/** Initialize the socket connection for the IOType with the
    supplied index */
int Initialize_IOType_transport_proxy(const int direction, const int index);

/** Take down the socket connections for all registered IOTypes */
void Finalize_IOType_transport_proxy();

/** @brief Creates a socket for the IOType with the supplied index. 

    Only necessary if the IOType has been disabled or 
    Enable_IOTypes_on_registration has been called such that the 
    associated socket was not created when the IOType was registered.
    @see Enable_IOTypes_on_registration
    */
int Enable_IOType_proxy(const int index);

/** @brief Destroys the socket for the IOType with the supplied index. */
int Disable_IOType_proxy(const int index);

/** @brief Queries the status of the connection of the IOType 
    with the supplied index.

    @return REG_SUCCESS if socket is connected to the proxy. */
int Get_communication_status_proxy(const int index);

/** @brief Writes the specified no. of bytes to the socket for the 
    IOType with the supplied index.*/
int Write_proxy(const int index, const int size, void* buffer);

/** @brief A non-blocking version of Write_sockets.

    Uses a select call to check the status of the socket 
    before attempting to write to it.
    @see Write_sockets */
int Write_non_blocking_proxy(const int index, const int size, void* buffer);

/** @brief Emits a header message on the socket for the 
    IOType with the supplied index. */
int Emit_header_proxy(const int index);

/** @brief Wraps Write_proxy.  Is required?? */
int Emit_data_proxy(const int index, const size_t num_bytes_to_send, 
		      void* pData);

/** @brief Reads a message header from the socket for the 
    IOType with the supplied index. 

    @return The data type for the following slice 
    @return The number of objects in the following slice 
    @return The number of bytes in the following slice
    @return Whether the data is from a Fortran code */
int Consume_msg_header_proxy(int index, int* datatype, int* count, int* num_bytes, int* is_fortran_array);

/** @brief Check to see whether data is available on the socket
    for the IOType with the supplied index. */
int Consume_start_data_check_proxy(const int index);

/** @brief Reads the specified amount of data off the socket associated
    with the IOType with the supplied index.

    Calls Read_proxy to read the data. */
int Consume_data_read_proxy(const int index, const int datatype, const int num_bytes_to_read, void *pData);

/** @brief Acknowledge that a data set has been received 
    on IOType with the supplied index.*/
int Emit_ack_proxy(int index);

/** @brief Attempt to read an acknowledgement from the consumer of the 
    IOType with the supplied index.*/
int Consume_ack_proxy(int index);

/*
 ************************************
 * Internal Methods.
 * Should NOT be called from outside
 * of ReG_Steer_Appside_Proxy.h
 ************************************/

/** @internal
    Read the next message received from the proxy - removes the 
    proxy-specific header and passes back pointer to data */
int Read_proxy(const int index, int *size, void** buffer);

#endif /* REG_PROXY_SAMPLES */
#endif /* __REG_STEER_SOCKETS_IO_H__ */
