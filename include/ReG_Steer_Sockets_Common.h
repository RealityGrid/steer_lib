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

#ifndef __REG_STEER_SOCKETS_COMMON_H__
#define __REG_STEER_SOCKETS_COMMON_H__

#include "ReG_Steer_Config.h"

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

} socket_info_type;

typedef struct {
  int max_entries;
  int num_used;
  socket_info_type* socket_info;
} socket_info_table_type;

/** @internal

    Initialise socket info table.
 */
int socket_info_table_init();

/** @internal
    @param index Index of the IOType to which socket belongs

    Initialise socket_info_type structure */
int socket_info_init(const int index);

/** @internal
    @param index Index of the IOType to which socket belongs

    Clean up members of socket_info_type structure */
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
    @param sock_info Pointer to socket_info_type structure holding info
    on socket
    @param pbuf Pointer to buffer in which to put received data (must
    be at least @p nbytes in size)
    @param nbytes No. of bytes to read from socket

    Does a non-blocking receive.  Mainly used to work
    around the fact that AIX and TRU64 recv's seem to block by default. */
int recv_non_block(socket_info_type* sock_info, 
		   char*             pbuf, 
		   int               nbytes);

#endif /* __REG_STEER_SOCKETS_COMMON_H__ */
