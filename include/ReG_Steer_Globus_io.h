/*----------------------------------------------------------------------------
    This header file contains routines and data structures for socket
    communication using Globus IO.

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

    Initial version by:  S Ramsden, 18.2.2003       0.1               

---------------------------------------------------------------------------*/

/*----------------------------------------------------------------------------
   Globus IO implementation note:

   The Globus IO has only been tested using NON-threaded flavors of
   Globus.  The socket_io_type structure has globus_mutex_t and
   globus_cond_t members which are required should the code need to be
   used with a threaded flavor of Globus.  However, only the callback
   functions have been coded to use these.  Some effor would be
   required to make the rest of the code threadsafe in order to use
   threaded Globus.

---------------------------------------------------------------------------*/

#ifndef __REG_STEER_GLOBUS_IO_H__
#define __REG_STEER_GLOBUS_IO_H__

#if REG_GLOBUS_STEERING || REG_GLOBUS_SAMPLES

#include <globus_io.h>

#include "ReG_Steer_types.h"

/* structure used for globus_io */
typedef struct
{
  /* port for socket connection */
  unsigned short int		listener_port;
  char			        listener_hostname[REG_MAX_STRING_LENGTH];

  /* socket attributes */
  globus_io_attr_t		attr;

  /* socket connection handles */
  globus_io_handle_t		listener_handle;
  globus_io_handle_t		conn_handle;

  /* status indicators for socket comms*/
  int				listener_status;  
  int				comms_status;

  globus_mutex_t		mutex;
  globus_cond_t			cond;

  char			        connector_hostname[REG_MAX_STRING_LENGTH];
  unsigned short int	        connector_port;

#if REG_GLOBUS_SAMPLES

#endif
} socket_io_type;

/* whether globus_io module has been activated */
static int		globus_module_activated = FALSE;

/*-------- Function prototypes --------*/

/* Activate the globus IO module if it hasn't already been activated */
extern int Globus_io_activate(void);

/* Deactivate the globus module if it was ever activated */
extern void Globus_io_deactivate(void);

/* Initialise socket_io_type structure */
extern int Globus_socket_info_init(socket_io_type * const socket_info);

/* Cleanup members of socket_io_type structure */
extern void Globus_socket_info_cleanup(socket_io_type * const socket_info);

/* Kick the Globus callback mechanism */
extern void Globus_callback_poll(socket_io_type * const socket_info);


/* Register listener callback function Globus_listener_callback on any
   available port (within GLOBUS_TCP_PORT_RANGE environment varaible
   if set) */
extern int Globus_create_listener(socket_io_type * const socket_info);

/* The callback function called when connector connects to socket -
   this function registers an accept callback - callback functions are executed
   when kicked via a call to Globus_callback_poll, thus there could be
   time delay between connection being made and connection being
   accepted */
extern void Globus_listener_callback (void		 *callback_arg,
				      globus_io_handle_t *handle,
				      globus_result_t	  result);

/* The callback function called when connector accepts a connection */
extern void Globus_accept_callback (void	       *callback_arg,
				    globus_io_handle_t *handle,
				    globus_result_t	resultparam);


/* Register callback function Globus_connector_callback to attempt to
   connect using globals connector_hostname and connector_port - use
   of globals will be removed eventually */
extern int Globus_create_connector(socket_io_type * const socket_info);


/* The connector callback function */
extern void Globus_connector_callback (void               *callback_arg,
				       globus_io_handle_t *handle,
				       globus_result_t	   result);

/* Close and clean up a listener connection */
extern void Globus_cleanup_listener_connection(socket_io_type * const socket_info);

/* Close and clean up a connector connection */
extern void Globus_cleanup_connector_connection(socket_io_type * const socket_info);

/* Call globus funtion close connections for the conn_handle */
extern void Globus_close_conn_handle(socket_io_type * const	socket_info);

/* Call globus funtions to close listener handle */
extern void Globus_close_listener_handle(socket_io_type * const socket_info);

/* Callback function for globus_io_register_close in
   Globus_close_conn_handle */
extern void Globus_close_callback (void			*callback_arg,
				   globus_io_handle_t	*handle,
				   globus_result_t	resultparam);

/* Callback function for globus_io_register_close in
   Globus_close_listener_handle */
void Globus_close_listener_callback (void			*callback_arg,
				     globus_io_handle_t	*handle,
				     globus_result_t		resultparam);

/* Call globus functions to accept pending connections */
void Globus_attempt_listener_connect(socket_io_type * const socket_info);


/* Call globus functions to close failed socket and accept any pending connection attempts */
extern void Globus_retry_accept_connect(socket_io_type * const socket_info);

/* Call globus functions to attempt to connect */
extern void Globus_attempt_connector_connect(socket_io_type * const socket_info);

/* Call globus functions to close failed socket and retry to connect */
extern void Globus_retry_connect(socket_io_type * const socket_info);

/* Call globus function to print out detail about error when REG_DEBUG on*/
extern void Globus_error_print(const globus_result_t result);


/* Functions for both appside and steerside steering globus comms */
extern int Consume_msg_header_globus(socket_io_type *sock_info,
				     int *DataType,
				     int *Count);

extern int Emit_msg_header_globus(socket_io_type *sock_info,
				  int DataType,
				  int Count);

/* Does what is says.  Uses uname and gethostbyname - not globus specific. */
extern char *Get_fully_qualified_hostname();

#endif /* REG_GLOBUS_STEERING || REG_GLOBUS_SAMPLES */
#endif /* __REG_STEER_GLOBUS_IO_H__ */
