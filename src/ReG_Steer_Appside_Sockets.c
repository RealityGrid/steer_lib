/*-----------------------------------------------------------------------
    This file contains routines and data structures for socket
    communication.

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

    Initial version by:  A Porter and R Haines, 29.1.2004      0.1  

-----------------------------------------------------------------------*/

#if REG_SOCKET_SAMPLES

#include "ReG_Steer_Common.h"
#include "ReG_Steer_Appside_internal.h"
#include <string.h>

#ifndef REG_DEBUG
#define REG_DEBUG 0
#endif

/* Need access to these tables which are actually declared in 
   ReG_Steer_Appside_internal.h */
extern IOdef_table_type IOTypes_table;

extern Steerer_connection_table_type Steerer_connection;

/*--------------------------------------------------------------------*/

int Socket_info_init(socket_io_type * const socket_info)
{

  return REG_SUCCESS;
}

/*--------------------------------------------------------------------*/

void Socket_info_cleanup(socket_io_type * const socket_info)
{

}

/*--------------------------------------------------------------------*/

int Create_listener(socket_io_type * const socket_info)
{

  return REG_SUCCESS;
}

/*--------------------------------------------------------------------*/

int Create_connector(socket_io_type * const socket_info)
{

  return REG_SUCCESS;
}

/*--------------------------------------------------------------------*/

void Cleanup_listener_connection(socket_io_type * const socket_info)
{

}

/*--------------------------------------------------------------------*/

void Cleanup_connector_connection(socket_io_type * const socket_info)
{

}

/*--------------------------------------------------------------------*/

void Retry_accept_connect(socket_io_type * const socket_info)
{
  /* if this called a socket read  has failed */

}

/*--------------------------------------------------------------------*/

void Attempt_connector_connect(socket_io_type * const socket_info)
{

}

/*--------------------------------------------------------------------*/

void Retry_connect(socket_io_type * const socket_info)
{

}

#endif
