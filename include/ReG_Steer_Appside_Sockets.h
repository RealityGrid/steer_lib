/*----------------------------------------------------------------------------
    This header file contains routines and data structures for
    communication using Sockets.

    (C)Copyright 2002 The University of Manchester, United Kingdom,
    all rights reserved.

    This software is produced by the Supercomputing, Visualization &
    e-Science Group, Manchester Computing, the Victoria University of
    Manchester as part of the RealityGrid project.

    This software has been tested with care but is not guaranteed for
    any particular purpose. Neither the copyright holder, nor the
    University of Manchester offer any warranties or representations,
    nor do they accept any liabilities with respect to this software.

    This software must not be used for commercial gain without the
    written permission of the authors.
    
    This software must not be redistributed without the written
    permission of the authors.

    Permission is granted to modify this software, provided any
    modifications are made freely available to the original authors.
 
    Supercomputing, Visualization & e-Science Group
    Manchester Computing
    University of Manchester
    Manchester M13 9PL
    
    WWW:    http://www.sve.man.ac.uk  
    email:  sve@man.ac.uk
    Tel:    +44 161 275 6095
    Fax:    +44 161 275 6800    

    Initial version by:  R Haines, 05.02.04       0.1               

---------------------------------------------------------------------------*/
#ifndef __REG_STEER_SOCKETS_IO_H__
#define __REG_STEER_SOCKETS_IO_H__

#if REG_SOCKET_SAMPLES

#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>

#include "ReG_Steer_types.h"

#define REG_SOCKETS_ERROR -1

/* structure used for socket information */
typedef struct
{
  /* port range we can use */
  int min_port;
  int max_port;

  /* default outward tcp interface */
  char			tcp_interface[REG_MAX_STRING_LENGTH];

  /* info for socket connection ("server" end) */
  int			listener_handle;
  char			listener_hostname[REG_MAX_STRING_LENGTH];
  unsigned short int	listener_port;

  /* info for socket connection ("client" end) */
  int			connector_handle;
  char			connector_hostname[REG_MAX_STRING_LENGTH];
  unsigned short int	connector_port;

  /* status indicators for socket comms*/
  int			listener_status;  
  int			comms_status;

} socket_io_type;

/*-------- Function prototypes --------*/

int Initialize_IOType_transport_sockets(const int direction, const int index);

void Finalize_IOType_transport_sockets();

int Enable_IOType_sockets(const int index);

int Disable_IOType_sockets(const int index);

int Get_communication_status_sockets(const int index);

int Write_sockets(const int index, const int size, void* buffer);

int Write_non_blocking_sockets(const int index, const int size, void* buffer);

int Emit_header_sockets(const int index);

int Emit_data_sockets(const int index, const size_t num_bytes_to_send, void* pData);

int Consume_msg_header_sockets(int index, int* datatype, int* count, int* num_bytes, int* is_fortran_array);

int Consume_start_data_check_sockets(const int index);

int Consume_data_read_sockets(const int index, const int datatype, const int num_bytes_to_read, void *pData);

#ifndef __linux
void signal_handler_sockets(int a_signal);
#endif

/*************************************
 * Internal Methods.
 * Should NOT be called from outside
 * of ReG_Steer_Appside_Sockets.h
 ************************************/

/* Initialise socket_io_type structure */
static int socket_info_init(const int index);

/* Clean up members of socket_io_type structure */
static void socket_info_cleanup(const int index);

static int create_listener(const int index);

static int create_connector(const int index);

static int connect_connector(const int index);

static void cleanup_listener_connection(const int index);

static void cleanup_connector_connection(const int index);

static void close_listener_handle(const int index);

static void close_connector_handle(const int index);

static void attempt_listener_connect(const int index);

static void retry_accept_connect(const int index);

static void attempt_connector_connect(const int index);

static void retry_connect(const int index);

static void poll_socket(const int index);

static int dns_lookup(char* hostname);

#endif /* REG_SOCKET_SAMPLES */
#endif /* __REG_STEER_SOCKETS_IO_H__ */
