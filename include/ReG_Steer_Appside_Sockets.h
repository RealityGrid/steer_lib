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

#if REG_GLOBUS_SAMPLES

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "ReG_Steer_types.h"

#define REG_SOCKETS_ERROR -1
/* a default port if no others available */
#define REG_SOCKETS_PORT  50000

/* structure used for socket information */
typedef struct
{
  /* port range we can use */
  int min_port;
  int max_port;

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

/* Initialise socket_io_type structure */
extern int Sockets_socket_info_init(const int index);

/* Clean up members of socket_io_type structure */
extern void Sockets_socket_info_cleanup(const int index);

/* Create a listener socket (server) */
extern int Sockets_create_listener(const int index);

/* Create a connector socket (client) */
extern int Sockets_create_connector(const int index);

/* Clean up listener socket */
extern void Sockets_cleanup_listener_connection(const int index);

/* Clean up connector socket */
extern void Sockets_cleanup_connector_connection(const int index);

/* Close the listener handle */
void Sockets_close_listener_handle(const int index);

/* Close the connector handle */
void Sockets_close_connector_handle(const int index);

/* Initialize the IOType transport for sockets */
extern int Sockets_initialize_IOType_transport(const int direction, const int index);

#endif /* REG_GLOBUS_SAMPLES */
#endif /* __REG_STEER_SOCKETS_IO_H__ */
