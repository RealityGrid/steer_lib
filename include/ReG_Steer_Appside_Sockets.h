/*----------------------------------------------------------------------------
  This header file contains routines and data structures for
  communication using Sockets.

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
#ifndef __REG_STEER_SOCKETS_IO_H__
#define __REG_STEER_SOCKETS_IO_H__

/** @file ReG_Steer_Appside_Sockets.h
    @brief Header file for socket-related routines
  */

/* ARP - added to uniquely identify TRU64 systems.  Necessary on
   Alpha workstations and LeMieux */
#ifndef TRU64
#if (defined (__digital__) && defined (__unix__))
#define TRU64
#endif
#endif

#if REG_SOCKET_SAMPLES

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

/** structure used for socket information */
typedef struct
{
  /** port range we can use */
  int min_port;
  int max_port;

  /** default outward tcp interface */
  char			tcp_interface[REG_MAX_STRING_LENGTH];

  /** info for socket connection ("server" end) */
  int			listener_handle;
  char			listener_hostname[REG_MAX_STRING_LENGTH];
  unsigned short int	listener_port;

  /** info for socket connection ("client" end) */
  int			connector_handle;
  char			connector_hostname[REG_MAX_STRING_LENGTH];
  unsigned short int	connector_port;

  /** status indicators for socket comms*/
  int			listener_status;  
  int			comms_status;

} socket_io_type;

/*-------- Function prototypes --------*/

/** Initialize the socket connection for the IOType with the
    supplied index */
int Initialize_IOType_transport_sockets(const int direction, const int index);

/** Take down the socket connections for all registered IOTypes */
void Finalize_IOType_transport_sockets();

/** @brief Creates a socket for the IOType with the supplied index. 

    Only necessary if the IOType has been disabled or Enable_IOTypes_on_registration
    has been called such that the associated socket was not
    created when the IOType was registered.
    @see Enable_IOTypes_on_registration
    */
int Enable_IOType_sockets(const int index);

/** @brief Destroys the socket for the IOType with the supplied index. */
int Disable_IOType_sockets(const int index);

/** @brief Queries the status of the connection of the IOType 
    with the supplied index.

    @return REG_SUCCESS if socket is connected. */
int Get_communication_status_sockets(const int index);

/** @brief Writes the specified no. of bytes to the socket for the IOType with the 
    supplied index.*/
int Write_sockets(const int index, const int size, void* buffer);

/** @brief A non-blocking version of Write_sockets.

    Uses a select call to check the status of the socket 
    before attempting to write to it.
    @see Write_sockets */
int Write_non_blocking_sockets(const int index, const int size, void* buffer);

/** @brief Emits a header message on the socket for the 
    IOType with the supplied index. */
int Emit_header_sockets(const int index);

/** @brief Wraps Write_sockets.  Is required?? */
int Emit_data_sockets(const int index, const size_t num_bytes_to_send, void* pData);

/** @brief Reads a message header from the socket for the 
    IOType with the supplied index. 

    @return The data type for the following slice 
    @return The number of objects in the following slice 
    @return The number of bytes in the following slice
    @return Whether the data is from a Fortran code */
int Consume_msg_header_sockets(int index, int* datatype, int* count, int* num_bytes, int* is_fortran_array);

/** @brief Check to see whether data is available on the socket
    for the IOType with the supplied index. */
int Consume_start_data_check_sockets(const int index);

/** @brief Reads the specified amount of data off the socket associated
    with the IOType with the supplied index.

    Calls recv to read the data. */
int Consume_data_read_sockets(const int index, const int datatype, const int num_bytes_to_read, void *pData);

#ifndef __linux
/** @brief Handler for SIGPIPE generated when connection goes down. 

    Only defined for Linux systems.  Necessary because otherwise the
    signal takes down the program. */
void signal_handler_sockets(int a_signal);
#endif

/** @brief Acknowledge that a data set has been received 
    on IOType with the supplied index.*/
int Emit_ack_sockets(int index);

/** @brief Attempt to read an acknowledgement from the consumer of the 
    IOType with the supplied index.*/
int Consume_ack_sockets(int index);

/*
 ************************************
 * Internal Methods.
 * Should NOT be called from outside
 * of ReG_Steer_Appside_Sockets.h
 ************************************/

/** @brief Initialise socket_io_type structure */
static int socket_info_init(const int index);

/** @brief Clean up members of socket_io_type structure */
static void socket_info_cleanup(const int index);

/** @brief Create a listener */
static int create_listener(const int index);

/** @brief Create a connector */
static int create_connector(const int index);

/** @brief Sets up and then attempts to connect a connector */
static int connect_connector(const int index);

/** @brief Take down a listener */
static void cleanup_listener_connection(const int index);

/** @brief Take down a connector */
static void cleanup_connector_connection(const int index);

/** @brief Calls close on the listener handle */
static void close_listener_handle(const int index);

/** @brief Calls close on the connector handle */
static void close_connector_handle(const int index);

/** @brief Checks to see if anyone is trying to connect */
static void attempt_listener_connect(const int index);

/** @brief Cleans-up a broken socket and tries to reconnect */
static void retry_accept_connect(const int index);

/** @brief Attempts to reconnect a connector */
static void attempt_connector_connect(const int index);

/** @brief Takes down a failed connector and tries again */
static void retry_connect(const int index);

/** @brief Checks socket connection and tries to establish
    connection if none (whether listener or connector) */
static void poll_socket(const int index);

/** @brief Looks up the IP of the specified hostname */
static int dns_lookup(char* hostname);

/** @brief Does a non-blocking receive.  Mainly used to work
    around the fact that AIX and TRU64 recv's seem to block by default. */
static int recv_non_block(socket_io_type  *sock_info, 
			  char *pbuf, int nbytes);

#endif /* REG_SOCKET_SAMPLES */
#endif /* __REG_STEER_SOCKETS_IO_H__ */
