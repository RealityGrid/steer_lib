/*----------------------------------------------------------------------------
    This file contains routines and data structures for socket
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
#include "ReG_Steer_Globus_io.h"

int Globus_io_activate()
{
  /* Activate the globus module if it hasn't been activated yet */
  if (!globus_module_activated) {

    if (globus_module_activate(GLOBUS_IO_MODULE) != GLOBUS_SUCCESS) {
      fprintf(stderr, "Globus_io_activate: ERROR: failed to activate Globus IO module\n");
      return REG_FAILURE;
    }
    else
    {
#if DEBUG
      fprintf(stderr, "Globus_io_activate: activated Globus IO module OK\n");
#endif
      globus_module_activated = TRUE;
    }

  }
        
  return REG_SUCCESS;
}

/*--------------------------------------------------------------------*/

void Globus_io_deactivate()
{
  /* Deactivate the globus module if it was ever activated */
  if (globus_module_activated) {
    if (globus_module_deactivate(GLOBUS_IO_MODULE) != GLOBUS_SUCCESS)
      fprintf(stderr, "Globus_io_deactivate: Globus IO module deactivation error\n");
    else {
#if DEBUG
      fprintf(stderr, "Globus_io_deactivate: Globus IO module deactivation OK\n");
#endif
    }
  }
}

/*--------------------------------------------------------------------*/

void Globus_socket_info_init(socket_io_type * const socket_info)
{
  globus_mutex_init(&socket_info->mutex, GLOBUS_NULL);
  globus_cond_init(&socket_info->cond, GLOBUS_NULL);
  socket_info->port = 0;
  socket_info->listener_status = REG_COMMS_STATUS_NULL;
  socket_info->comms_status = REG_COMMS_STATUS_NULL;   
}

/*--------------------------------------------------------------------*/

void Globus_callback_poll(socket_io_type * const socket_info)
{
  globus_abstime_t	timeout;
  timeout.tv_sec = 5000;

  /* attempt to kick the callback function */
  globus_mutex_lock(&(socket_info->mutex));

#if DEBUG
  fprintf(stderr, "Globus_callback_poll: call globus_cond_timedwait\n");
#endif
  globus_cond_timedwait(&(socket_info->cond), 
			&(socket_info->mutex), 
			&timeout);

  globus_mutex_unlock(&(socket_info->mutex)); 
  
}

/*--------------------------------------------------------------------*/

int Globus_create_listener(socket_io_type * const socket_info)
{
  globus_io_attr_t	attr; 
  globus_object_t	*err; 
  globus_result_t	result;

  if (Globus_io_activate() != REG_SUCCESS)
    return REG_FAILURE;
 

  /* Initialise the attributes structure */
  if (globus_io_tcpattr_init(&attr) != GLOBUS_SUCCESS) {
    fprintf(stderr, "Globus_create_listener: Error initialising attribute.\n");
    return REG_FAILURE;
  }

  /* SMR XXX - add more attr security stuff here */



  /* create listener socket on free port 
   *  - if environment variable GLOBUS_TCP_PORT_RANGE has been set,
   *  only port numbers within that range will be used 
   */

  result = globus_io_tcp_create_listener(&(socket_info->port), -1, &attr, 
					 &(socket_info->listener_handle));
  /* SMR XXX backlog -1?*/
  if (result != GLOBUS_SUCCESS) {
    fprintf(stderr, "Globus_create_listener: Error creating listening socket:\n");
    err =  globus_error_get(result);

    if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_NULL_PARAMETER,
				 globus_object_get_type(err))) {
     fprintf(stderr, " - globus error type: null parameter\n");
    }
    else if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_HOST_NOT_FOUND,
				      globus_object_get_type(err))) {
      fprintf(stderr, " - globus error type: host not found\n");
    } 
    else if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_SYSTEM_FAILURE,
					 globus_object_get_type(err))) {
      fprintf(stderr, " - globus error type: system failure\n");
	}

        
    return REG_FAILURE;
  }

  /* NOTE: this is the listener_status */
  socket_info->listener_status=REG_COMMS_STATUS_LISTENING;

#if DEBUG
  fprintf(stderr, "DBG: Listener on port %d\n", socket_info->port);
#endif


  /* now register listener so can accept connections when they happen */
  result = globus_io_tcp_register_listen(&(socket_info->listener_handle),
					 Globus_listener_callback,
					 (void *) socket_info);
  if (result != GLOBUS_SUCCESS) {

    fprintf(stderr, "Globus_create_listener: Error registering listen :\n");
    err =  globus_error_get(result);
    if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_NULL_PARAMETER,
				 globus_object_get_type(err))) {
     fprintf(stderr, " - globus error type: null parameter\n");
    }
    else if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_CLOSE_ALREADY_REGISTERED,
				      globus_object_get_type(err))) {
     fprintf(stderr, " - globus error type: close already registered on handle\n");
    }
    else if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_NOT_INITIALIZED,
				      globus_object_get_type(err))) {
     fprintf(stderr, " - globus error type: handle not initialised\n");
    }
    else if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_INVALID_TYPE,
				      globus_object_get_type(err))) {
     fprintf(stderr, " - globus error type: invalid handle type\n");
    }

    /* SMR XXXn - but still listening.... so what is status? close listener?
       socket_info->listener_status=REG_COMMS_STATUS_NULL; */
    return REG_FAILURE;
  }
  else {
    socket_info->comms_status=REG_COMMS_STATUS_LISTENING;
    /* attempt to kick the callback function */
    Globus_callback_poll(socket_info);
  } 
  return REG_SUCCESS;

}
/*--------------------------------------------------------------------*/

void
Globus_listener_callback (void			*callback_arg,
			  globus_io_handle_t	*handle,
			  globus_result_t	resultparam)
{ 
  globus_result_t	   result;
  globus_object_t	  *err; 
  socket_io_type *lsocket_info;

  lsocket_info = ((socket_io_type *) callback_arg);

#if DEBUG
  fprintf(stderr, "DBG: In Globus_listen_callback\n");
#endif

  globus_mutex_lock(&lsocket_info->mutex);

  if (resultparam != GLOBUS_SUCCESS) {
    err = globus_error_get(resultparam);
    lsocket_info->comms_status=REG_COMMS_STATUS_FAILURE;
    fprintf(stderr, "Entered Globus_listener_callback resultparam FALSE, no connection\n");
  }
  else {            
    /*  register callback to accept connection  */
    result = globus_io_tcp_register_accept(handle, 
					   GLOBUS_NULL, 
					   &(lsocket_info->conn_handle),
					   Globus_accept_callback,
					   (void *) lsocket_info);
      
    if (result != GLOBUS_SUCCESS) {
      lsocket_info->comms_status=REG_COMMS_STATUS_FAILURE;
      fprintf(stderr, "Globus_listen_callback: Error registering accept:");
 
      err =  globus_error_get(result);	
      if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_NULL_PARAMETER,
				   globus_object_get_type(err))) {
	fprintf(stderr, " - globus error type: null parameter\n");
      }
      else if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_CLOSE_ALREADY_REGISTERED,
					globus_object_get_type(err))) {
	fprintf(stderr, " - globus error type: close already registered on handle\n"); 
      }
      else if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_NOT_INITIALIZED,
					globus_object_get_type(err))) {
	fprintf(stderr, " - globus error type: handle not initialised\n"); 
      }
      else if (globus_object_type_match( GLOBUS_IO_ERROR_TYPE_INVALID_TYPE,
					 globus_object_get_type(err))) {
	fprintf(stderr, " - globus error type: invalid attribute type\n"); 
      }
      else if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_SYSTEM_FAILURE,
					globus_object_get_type(err))) {
	fprintf(stderr, " - globus error type: system failure\n"); 
      }

    }
    else {
      lsocket_info->comms_status=REG_COMMS_STATUS_WAITING_FOR_ACCEPT;
#if DEBUG
      fprintf(stderr, "DBG: Globus_listen_callback - registered accept \n");
#endif
    }
      
  }
  
  globus_cond_signal(&lsocket_info->cond);
  globus_mutex_unlock(&lsocket_info->mutex);

  /* attempt to kick the callback function - need for accept just registered */
  Globus_callback_poll(lsocket_info);    
  

}

/*--------------------------------------------------------------------*/

void
Globus_accept_callback (void			*callback_arg,
			globus_io_handle_t	*handle,
			globus_result_t		resultparam)
{
  globus_object_t		*err; 
  socket_io_type	*lsocket_info;

  lsocket_info = ((socket_io_type *) callback_arg);

  globus_mutex_lock(&lsocket_info->mutex);

  if (resultparam != GLOBUS_SUCCESS) {
    fprintf(stderr, "DBG: Entered Globus_accept_callback resultparam FALSE, no connection\n");
    err = globus_error_get(resultparam);
    lsocket_info->comms_status=REG_COMMS_STATUS_FAILURE;
  }
  else {
#if DEBUG
    fprintf(stderr, "DBG: Globus_accept_callback has connected\n");
#endif
    lsocket_info->comms_status=REG_COMMS_STATUS_CONNECTED;
  }

  globus_cond_signal(&lsocket_info->cond);
  globus_mutex_unlock(&lsocket_info->mutex);
  
}

/*--------------------------------------------------------------------*/

int Globus_create_connector(socket_io_type * const socket_info)
{
  globus_io_attr_t	attr; 
  globus_object_t	*err; 
  globus_result_t	result;


  /* Activate the globus module */
  if (Globus_io_activate() != REG_SUCCESS)
    return REG_FAILURE;
  
  /* Initialise the attributes structure */
  if (globus_io_tcpattr_init(&attr) != GLOBUS_SUCCESS) {
    fprintf(stderr, "Globus_create_connector: Error initialising attribute.\n");
    return REG_FAILURE;
  }

  /* SMR XXX - add more attr security stuff here */

  
  /* now register connector using port and hostname parameter
   * - will connect and call callback function when port listens
   */

  result = globus_io_tcp_register_connect(connector_hostname,
					  connector_port,
					  &attr,
					  Globus_connector_callback,
					  (void *) socket_info,
					  &socket_info->conn_handle);
  if (result != GLOBUS_SUCCESS) {

    fprintf(stderr, "Globus_create_connector: Error registering connect:\n");
    err =  globus_error_get(result);
    if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_NULL_PARAMETER,
				 globus_object_get_type(err))) {
     fprintf(stderr, " - globus error type: null parameter\n");
    }
    else if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_HOST_NOT_FOUND,
				      globus_object_get_type(err))) {
      fprintf(stderr, " - globus error type: host not found\n");
    }

    socket_info->comms_status=REG_COMMS_STATUS_NULL;
    return REG_FAILURE;
  }
  else {
    socket_info->comms_status=REG_COMMS_STATUS_WAITING_TO_CONNECT;

#if DEBUG
    fprintf(stderr, "Globus_create_connector: registered connector on "
	    "connector_port = %d, connector_hostname = %s\n", connector_port, 
	    connector_hostname);
#endif
  }

  /* attempt to kick the callback function */
  Globus_callback_poll(socket_info);

  return REG_SUCCESS;

}

/*--------------------------------------------------------------------*/

/* NOTE: for globus_io_register_connect, callback just happens immediately - 
 * i.e. a connect attempt is made straight away and callback invoked
 * therefore to attempt another connect need to re-register callback
 */
void
Globus_connector_callback (void			*callback_arg,
			   globus_io_handle_t	*handle,
			   globus_result_t	resultparam)
{ 
  globus_object_t		*err; 
  socket_io_type	*lsocket_info;

  lsocket_info = ((socket_io_type *) callback_arg);

  globus_mutex_lock(&lsocket_info->mutex);

  if (resultparam != GLOBUS_SUCCESS) {
    lsocket_info->comms_status=REG_COMMS_STATUS_NULL;
    fprintf(stderr, "DBG: Entered Globus_connector_callback resultparam FALSE, no connection\n");
    err = globus_error_get(resultparam);
  }
  else {
    lsocket_info->comms_status=REG_COMMS_STATUS_CONNECTED;
#if DEBUG
    fprintf(stderr, "DBG: Globus_connector_callback has connected");
#endif
  }

  globus_cond_signal(&lsocket_info->cond);
  globus_mutex_unlock(&lsocket_info->mutex);
      
}

/*--------------------------------------------------------------------*/

void Globus_cleanup_listener_connection(socket_io_type * const socket_info)
{

  if (socket_info->comms_status == REG_COMMS_STATUS_CONNECTED || 
      socket_info->comms_status == REG_COMMS_STATUS_WAITING_FOR_ACCEPT)
    Globus_close_conn_handle(socket_info);

  if (socket_info->listener_status == REG_COMMS_STATUS_LISTENING)
    Globus_close_listener_handle(socket_info);

  globus_mutex_destroy(&socket_info->mutex);
  globus_cond_destroy(&socket_info->cond); 
  
}

/*--------------------------------------------------------------------*/

void Globus_cleanup_connector_connection(socket_io_type * const socket_info)
{
  if (socket_info->comms_status == REG_COMMS_STATUS_CONNECTED ||
      socket_info->comms_status == REG_COMMS_STATUS_WAITING_TO_CONNECT )
    Globus_close_conn_handle(socket_info);


  globus_mutex_destroy(&socket_info->mutex);
  globus_cond_destroy(&socket_info->cond); 

}

/*--------------------------------------------------------------------*/

  void Globus_close_conn_handle(socket_io_type * const socket_info)
{

  globus_object_t	*err; 
  globus_result_t	result;

  /* Note that globus cancel is done as part of globus close */
  result = globus_io_register_close(&socket_info->conn_handle, 
				    Globus_close_callback, 
				    (void *) socket_info);

  if (result != GLOBUS_SUCCESS) {
    fprintf(stderr, "Globus_close_conn_handle: Error register close connection\n");
    err =  globus_error_get(result);

    if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_NULL_PARAMETER,
				 globus_object_get_type(err))) {
     fprintf(stderr, " - globus error type: null parameter\n");
    }
    else if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_NOT_INITIALIZED,
				 globus_object_get_type(err))) {
     fprintf(stderr, " - globus error type: handle not initialised\n");
    }
    else if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_CLOSE_ALREADY_REGISTERED,
				      globus_object_get_type(err))) {
      fprintf(stderr, " - globus error type: handle close already registered\n");
    }
  }
  else {
    fprintf(stderr, "DBG: Globus_close_conn_handle:  Register close OK\n");
  }

  Globus_callback_poll(socket_info);

}

/*--------------------------------------------------------------------*/

void Globus_close_listener_handle(socket_io_type * const socket_info)
{

  globus_object_t	*err; 
  globus_result_t	result;

  result = globus_io_register_close(&socket_info->listener_handle, 
				    Globus_close_listener_callback, 
				    (void *) socket_info);

  if (result != GLOBUS_SUCCESS) {
    fprintf(stderr, "Globus_close_listener_handle: Error register close connection\n");
    err =  globus_error_get(result);

    if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_NULL_PARAMETER,
				 globus_object_get_type(err))) {
     fprintf(stderr, " - globus error type: null parameter\n");
    }
    else if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_NOT_INITIALIZED,
				 globus_object_get_type(err))) {
     fprintf(stderr, " - globus error type: handle not initialised\n");
    }
    else if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_CLOSE_ALREADY_REGISTERED,
				      globus_object_get_type(err))) {
      fprintf(stderr, " - globus error type: handle close already registered\n");
    }
  }
  else {
    fprintf(stderr, "DBG: Globus_close_listener_handle:  Register close OK\n");
  }

  Globus_callback_poll(socket_info);

}

/*--------------------------------------------------------------------*/

void
Globus_close_callback (void			*callback_arg,
		       globus_io_handle_t	*handle,
		       globus_result_t		resultparam)
{ 

  socket_io_type *lsocket_info;
  lsocket_info = ((socket_io_type *) callback_arg);

#if DEBUG
    fprintf(stderr, "Globus_close_callback: done\n");
#endif
    lsocket_info->comms_status = REG_COMMS_STATUS_NULL;

}

/*--------------------------------------------------------------------*/

void
Globus_close_listener_callback (void			*callback_arg,
				globus_io_handle_t	*handle,
				globus_result_t		resultparam)
{

  socket_io_type *lsocket_info;
  lsocket_info = ((socket_io_type *) callback_arg);

#if DEBUG
    fprintf(stderr, "Globus_close_listener_callback: done\n");
#endif
    lsocket_info->listener_status = REG_COMMS_STATUS_NULL;

}

/*--------------------------------------------------------------------*/

void Globus_attempt_listener_connect(socket_io_type * const socket_info)
{

  if (socket_info->comms_status == REG_COMMS_STATUS_LISTENING) {
    /* we're waiting for connect callback so attempt to kick the callback function */
#if DEBUG    
    fprintf(stderr, "Globus_attempt_listener_connect:kick callback_poll\n");
#endif
    Globus_callback_poll(socket_info);
  }
  else if (socket_info->comms_status == REG_COMMS_STATUS_FAILURE) {
    /* connection has broken - we're still listeneing so see if anything to connect */
#if DEBUG
    fprintf(stderr, "Globus_attempt_listener_connect:retry accept connect\n");
#endif
    Globus_retry_accept_connect(socket_info);
  }

}

/*--------------------------------------------------------------------*/

void Globus_retry_accept_connect(socket_io_type * const socket_info)
{
  /* if this called a globus_io read  has failed */
  globus_result_t    result;
  globus_object_t    *err; 

  /* if we're here and  status is connected  we've lost a connection, so first close socket */
  if (socket_info->comms_status==REG_COMMS_STATUS_CONNECTED) {

    Globus_close_conn_handle(socket_info);

  }

  /* assume list_handle is still listening and see if there is a connection to accept */
  result = globus_io_tcp_register_accept(&socket_info->listener_handle, 
					 GLOBUS_NULL, 
					 &socket_info->conn_handle,
					 Globus_accept_callback,
					 (void *) socket_info);

  if (result != GLOBUS_SUCCESS) {
	 
    fprintf(stderr, "Globus_retry_accept_connect: Error registering accept:"); 
    err =  globus_error_get(result);
      
    if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_NULL_PARAMETER,
				 globus_object_get_type(err))) {
      fprintf(stderr, " - globus error type: null parameter\n");
    }
    else if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_CLOSE_ALREADY_REGISTERED,
				      globus_object_get_type(err))) {
      fprintf(stderr, " - globus error type: close already registered on handle\n"); 
    }
    else if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_NOT_INITIALIZED,
				      globus_object_get_type(err))) {
      fprintf(stderr, " - globus error type: handle not initialised\n"); 
    }
    else if (globus_object_type_match( GLOBUS_IO_ERROR_TYPE_INVALID_TYPE,
				       globus_object_get_type(err))) {
      fprintf(stderr, " - globus error type: invalid attribute type\n"); 
    }
    else if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_SYSTEM_FAILURE,
				      globus_object_get_type(err))) {
      fprintf(stderr, " - globus error type: no connection pending\n"); 
    }

    /* Set to FAILURE so retry is attempted again next time round */
    socket_info->comms_status=REG_COMMS_STATUS_FAILURE;
  }
  else {
#if DEBUG
    fprintf(stderr, "Globus_retry_accept_connect: register accept success\n");
#endif
    socket_info->comms_status=REG_COMMS_STATUS_WAITING_FOR_ACCEPT;

    /* attempt to kick the callback function - need for accept just registered */
    Globus_callback_poll(socket_info);

  }

}

/*--------------------------------------------------------------------*/

void Globus_retry_connect(socket_io_type * const socket_info){

  /* close the failed connector and retry to connect */

  if (socket_info->comms_status==REG_COMMS_STATUS_CONNECTED) {
    Globus_close_conn_handle(socket_info);
  }
  
   if (Globus_create_connector(socket_info) != REG_SUCCESS) {
#if DEBUG
     fprintf(stderr, "Register_IOTypes: failed to register connector for IOType\n");
#endif
      /* Set to NULL so Globus_create_connector is attempted again next time round */
     socket_info->comms_status=REG_COMMS_STATUS_NULL;
   }
}

/*--------------------------------------------------------------------*/
