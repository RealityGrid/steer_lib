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
#ifndef __REG_STEER_SOCKETS_IO_H__
#define __REG_STEER_SOCKETS_IO_H__

/** @internal
    @file ReG_Steer_Appside_Sockets.h
    @brief Header file for socket-related routines
    @author Andrew Porter
    @author Robert Haines
  */

/* ARP - added to uniquely identify TRU64 systems.  Necessary on
   Alpha workstations and LeMieux */
#ifndef TRU64
#if (defined (__digital__) && defined (__unix__))
#define TRU64
#endif
#endif

#include "ReG_Steer_Config.h"

#if defined(REG_SOCKET_SAMPLES) || defined(REG_PROXY_SAMPLES) || defined(DOXYGEN)

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

/** @internal 
    Structure to hold socket information */
typedef struct
{
  /** Minimum port number we can use (zero if any) */
  int                   min_port;
  /** Maximum port number we can use (zero if any) */
  int                   max_port;
  /** Default outbound tcp interface */
  char			tcp_interface[REG_MAX_STRING_LENGTH];
  /** Handle of listener socket - info for socket connection ("server" end) */
  int			listener_handle;
  /** Hostname on which to listen -  info for socket connection ("server" end) */
  char			listener_hostname[REG_MAX_STRING_LENGTH];
  /** Port on which to listen -  info for socket connection ("server" end) */
  unsigned short int	listener_port;
  /** Handle of connecting socket - info for socket connection ("client" end) */
  int			connector_handle;
  /** Hostname to connect to - info for socket connection ("client" end) */
  char			connector_hostname[REG_MAX_STRING_LENGTH];
  /** Port to connect to - info for socket connection ("client" end) */
  unsigned short int	connector_port;
  /** status indicator for listening socket */
  int			listener_status;  
  /** status indicator for connecting socket */
  int			comms_status;

} socket_io_type;

/*-------- Function prototypes --------*/

/** @internal
    @param direction Whether connecting (REG_IO_OUT) or listening (REG_IO_IN)
    @param index Index of the IOType to initialize

    Initialize the socket connection for an IOType */
int Initialize_IOType_transport_sockets(const int direction, 
					const int index);

/** @internal 
    Take down the socket connections for @e all registered IOTypes */
void Finalize_IOType_transport_sockets();

/** @internal
    @param index Index of the IOType to enable

    Creates a socket for an IOType.  Only necessary if the IOType has
    been disabled or Enable_IOTypes_on_registration() has been called
    such that the associated socket was not created when the IOType
    was registered.
    @see Enable_IOTypes_on_registration() */
int Enable_IOType_sockets(const int index);

/** @internal
    @param index Index of the IOType to disable

    Destroys the socket for the IOType with the supplied index. */
int Disable_IOType_sockets(const int index);

/** @internal
    @param index Index of the IOType to query
    @return REG_SUCCESS if socket is connected. 

    Queries the status of the connection of an IOType */
int Get_communication_status_sockets(const int index);

/** @internal
    A non-blocking version of Write_sockets().
    Uses a select call to check the status of the socket 
    before attempting to write to it. 
    @see Emit_data_sockets() */
int Emit_data_non_blocking_sockets(const int index, const int size, 
				   void* buffer);

/** @internal
    @param index Index of IOType to write header to

    Emits a header message on the socket for the 
    IOType with the supplied index. */
int Emit_header_sockets(const int index);

/** @internal
    @param index Index of IOType to use to send data
    @param num_bytes_to_send No. of bytes of data to send
    @param pData Pointer to buffer containing data to send
*/
int Emit_data_sockets(const int index, const size_t num_bytes_to_send, 
		      void* pData);

/** @internal
    @param index Index of IOType from which to get header data
    @param datatype On successful return, the type of the data in 
    the following slice
    @param count On successful return, the no. of data objects in 
    the following slice
    @param num_bytes On succesful return, the no. of bytes of data in 
    the following slice
    @param is_fortran_array On successful return, whether the data in 
    the following slice is from a fortran array (changes its ordering)

    Reads a message header from the socket for the 
    IOType with the supplied index. */
int Consume_msg_header_sockets(int  index, 
			       int* datatype, 
			       int* count, 
			       int* num_bytes, 
			       int* is_fortran_array);

/** @internal
    @param index Index of IOType to check for data

    Check to see whether data is available on the socket
    for the IOType. */
int Consume_start_data_check_sockets(const int index);

/** @internal
    @param index Index of the IOType to get data from
    @param datatype Type of data to read from the IOType
    @param num_bytes_to_read No. of bytes of data to read
    @param pData Pointer to buffer large enough to receive data

    Reads the specified amount of data off the socket associated with
    the IOType with the supplied index.  Calls recv to read the
    data. */
int Consume_data_read_sockets(const int index, 
			      const int datatype, 
			      const int num_bytes_to_read, 
			      void *pData);

#ifndef __linux
/** @internal 
    Handler for SIGPIPE generated when connection goes down. 
    Only defined for Linux systems.  Necessary because otherwise the
    signal takes down the program. */
void signal_handler_sockets(int a_signal);
#endif

/** @internal
    @param index Index of the IOType on which to send acknowledgement

    Acknowledge that a data set has been received 
    and processed successfully on an IOType.*/
int Emit_ack_sockets(int index);

/** @internal
    Attempt to read an acknowledgement from the consumer of the 
    IOType with the supplied index.*/
int Consume_ack_sockets(int index);

/*
 ************************************
 * Internal Methods.
 * Should NOT be called from outside
 * of ReG_Steer_Appside_Sockets.h
 ************************************/

/** @internal
    @param index Index of the IOType to which socket belongs

    Initialise socket_io_type structure */
int socket_info_init(const int index);

/** @internal
    @param index Index of the IOType to which socket belongs

    Clean up members of socket_io_type structure */
void socket_info_cleanup(const int index);

/** @internal
    @param index Index of the IOType to which socket belongs

    Create a listening socket */
int create_listener(const int index);

/** @internal
    @param index Index of the IOType to which socket belongs

    Create a connector socket */
int create_connector(const int index);

/** @internal
    @param index Index of the IOType to which socket belongs

    Sets up and then attempts to connect a connector */
int connect_connector(const int index);

/** @internal
    @param index Index of the IOType to which socket belongs

    Take down a listener */
void cleanup_listener_connection(const int index);

/** @internal
    @param index Index of the IOType to which socket belongs

    Take down a connector */
void cleanup_connector_connection(const int index);

/** @internal
    @param index Index of the IOType to which socket belongs
    
    Calls close on the listener handle */
void close_listener_handle(const int index);

/** @internal
    @param index Index of the IOType to which socket belongs

    Calls close on the connector handle */
void close_connector_handle(const int index);

/** @internal
    @param index Index of the IOType to which socket belongs

    Checks to see if anyone is trying to connect */
void attempt_listener_connect(const int index);

/** @internal
    @param index Index of the IOType to which socket belongs

    Cleans-up a broken socket and tries to reconnect */
void retry_accept_connect(const int index);

/** @internal
    @param index Index of the IOType to which socket belongs

    Attempts to reconnect a connector */
void attempt_connector_connect(const int index);

/** @internal
    @param index Index of the IOType to which socket belongs

    Takes down a failed connector and tries again */
void retry_connect(const int index);

/** @internal
    @param index Index of the IOType to which socket belongs

    Checks socket connection and tries to establish
    connection if none (whether listener or connector) */
void poll_socket(const int index);

/** @internal
    @param hostname Fully qualified name of machine to look-up.  On
    successful return holds the IP address of the machine.
    @return REG_SUCCESS, REG_FAILURE

    Looks up the IP of the specified @p hostname */
int dns_lookup(char* hostname);

/** @internal
    @param sock_info Pointer to socket_io_type structure holding info
    on socket
    @param pbuf Pointer to buffer in which to put received data (must
    be at least @p nbytes in size)
    @param nbytes No. of bytes to read from socket

    Does a non-blocking receive.  Mainly used to work
    around the fact that AIX and TRU64 recv's seem to block by default. */
int recv_non_block(socket_io_type *sock_info, 
		   char           *pbuf, 
		   int             nbytes);

#endif /* REG_SOCKET_SAMPLES */
#endif /* __REG_STEER_SOCKETS_IO_H__ */
