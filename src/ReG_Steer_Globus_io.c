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

#if REG_GLOBUS_STEERING || REG_GLOBUS_SAMPLES

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

int Globus_socket_info_init(socket_io_type * const socket_info)
{
  globus_result_t	result;

  if (Globus_io_activate() != REG_SUCCESS)
    return REG_FAILURE;

  globus_mutex_init(&socket_info->mutex, GLOBUS_NULL);
  globus_cond_init(&socket_info->cond, GLOBUS_NULL);

  result = globus_io_tcpattr_init(&socket_info->attr);
  if (result != GLOBUS_SUCCESS) {
    fprintf(stderr, "Globus_socket_info_init: Error initialising attribute.\n");
    Globus_error_print(result);
    return REG_FAILURE;
  }

  result = globus_io_attr_set_socket_reuseaddr(&socket_info->attr, GLOBUS_TRUE);
  if (result != GLOBUS_SUCCESS) {
    fprintf(stderr, "Globus_socket_info_init: Error setting resueaddr attribute.\n");
    Globus_error_print(result);
    return REG_FAILURE;
  }

  /* SMR XXX - add more attr security stuff here */

  socket_info->listener_port = 0;
  socket_info->listener_status = REG_COMMS_STATUS_NULL;
  socket_info->comms_status = REG_COMMS_STATUS_NULL;   
  socket_info->connector_port = 0;
  sprintf(socket_info->connector_hostname, " ");

#if DEBUG
  fprintf(stderr, "Globus_socket_info_init: done\n");
#endif

  return REG_SUCCESS;

}

/*--------------------------------------------------------------------*/

void Globus_socket_info_cleanup(socket_io_type * const socket_info)
{
  globus_result_t	result;

  globus_mutex_destroy(&socket_info->mutex);
  globus_cond_destroy(&socket_info->cond);

  result = globus_io_tcpattr_destroy(&socket_info->attr);
  if (result != GLOBUS_SUCCESS) {
    fprintf(stderr, "Globus_socket_info_cleanup: Error destroying attribute.\n");
    Globus_error_print(result);
  }
#if DEBUG
  fprintf(stderr, "Globus_socket_info_cleanup: done\n");
#endif
}

/*--------------------------------------------------------------------*/

void Globus_callback_poll(socket_io_type * const socket_info)
{
  globus_abstime_t	timeout;
  timeout.tv_sec = 5000;

#if DEBUG
  fprintf(stderr, "Globus_callback_poll: call globus_cond_timedwait\n");
#endif

  globus_mutex_lock(&(socket_info->mutex));

  /* attempt to kick the callback function */
  globus_cond_timedwait(&(socket_info->cond), 
			&(socket_info->mutex), 
			&timeout);

  globus_mutex_unlock(&(socket_info->mutex)); 
  
}

/*--------------------------------------------------------------------*/

int Globus_create_listener(socket_io_type * const socket_info)
{
  globus_result_t	result;

  /* create listener socket on free port 
   *  - if environment variable GLOBUS_TCP_PORT_RANGE has been set,
   *  only port numbers within that range will be used 
   */

  result = globus_io_tcp_create_listener(&(socket_info->listener_port), 
					 -1, 
					 &socket_info->attr, 
					 &(socket_info->listener_handle));
  if (result != GLOBUS_SUCCESS) {
    fprintf(stderr, "Globus_create_listener: Error creating listening socket\n");
    Globus_error_print(result);
    return REG_FAILURE;
  }

  /* NOTE: this is the listener_status */
  socket_info->listener_status=REG_COMMS_STATUS_LISTENING;

#if DEBUG
  fprintf(stderr, "DBG: Listener on port %d\n", socket_info->listener_port);
#endif

  /* now register listener so can accept connections when they happen */
  result = globus_io_tcp_register_listen(&(socket_info->listener_handle),
					 Globus_listener_callback,
					 (void *) socket_info);
  if (result != GLOBUS_SUCCESS) {
    fprintf(stderr, "Globus_create_listener: Error registering listen\n");
    Globus_error_print(result);

    /* If this fails, must be something wrong with listener_handle, so
       register a close for it (this will change listener_status
       value) and thus create listener will be reattempted in the next
       Emit_start. Don't ever think this will happen...
    */

    Globus_close_listener_handle(socket_info);
    return REG_FAILURE;
  }
  else {
    socket_info->comms_status=REG_COMMS_STATUS_LISTENING;
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
  socket_io_type *lsocket_info;

  lsocket_info = ((socket_io_type *) callback_arg);

#if DEBUG
  fprintf(stderr, "DBG: In Globus_listen_callback\n");
#endif

  globus_mutex_lock(&lsocket_info->mutex);

  if (resultparam != GLOBUS_SUCCESS) {
    lsocket_info->comms_status=REG_COMMS_STATUS_FAILURE;
    fprintf(stderr, "Entered Globus_listener_callback resultparam FALSE, no connection\n");
    Globus_error_print(resultparam);
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
      fprintf(stderr, "Globus_listen_callback: Error registering accept\n");
      Globus_error_print(result);
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

  /* attempt to kick the callback function - for accept just registered */
  Globus_callback_poll(lsocket_info);    
  

}

/*--------------------------------------------------------------------*/

void
Globus_accept_callback (void			*callback_arg,
			globus_io_handle_t	*handle,
			globus_result_t		resultparam)
{
  socket_io_type	*lsocket_info;

  lsocket_info = ((socket_io_type *) callback_arg);

  globus_mutex_lock(&lsocket_info->mutex);

  if (resultparam != GLOBUS_SUCCESS) {
    fprintf(stderr, "DBG: Entered Globus_accept_callback resultparam FALSE, no connection\n");
    Globus_error_print(resultparam);
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
  globus_result_t	result;


  /* Register connector using port and hostname parameter
   * - will connect and call callback function when port listens
   */

  result = globus_io_tcp_register_connect(socket_info->connector_hostname,
					  socket_info->connector_port,
					  &socket_info->attr,
					  Globus_connector_callback,
					  (void *) socket_info,
					  &socket_info->conn_handle);

  if (result != GLOBUS_SUCCESS) {
    fprintf(stderr, "Globus_create_connector: Error registering connect\n");
    Globus_error_print(result);
    socket_info->comms_status=REG_COMMS_STATUS_FAILURE;
    return REG_FAILURE;
  }
  else {
    socket_info->comms_status=REG_COMMS_STATUS_WAITING_TO_CONNECT;

#if DEBUG
    fprintf(stderr, "Globus_create_connector: registered connector on "
	    "connector_port = %d, connector_hostname = %s\n", 
	    socket_info->connector_port, 
	    socket_info->connector_hostname);
#endif
  }

  /* attempt to kick the callback function */
  Globus_callback_poll(socket_info);

  return REG_SUCCESS;

}

/*--------------------------------------------------------------------*/
void
Globus_connector_callback (void			*callback_arg,
			   globus_io_handle_t	*handle,
			   globus_result_t	resultparam)
{ 
  socket_io_type	*lsocket_info;

  lsocket_info = ((socket_io_type *) callback_arg);

  globus_mutex_lock(&lsocket_info->mutex);

  if (resultparam != GLOBUS_SUCCESS) {
    lsocket_info->comms_status=REG_COMMS_STATUS_FAILURE;
    fprintf(stderr, "DBG: Entered Globus_connector_callback resultparam FALSE, no connection\n");
    Globus_error_print(resultparam);
  }
  else {
    lsocket_info->comms_status=REG_COMMS_STATUS_CONNECTED;
#if DEBUG
    fprintf(stderr, "DBG: Globus_connector_callback has connected\n");
#endif
  }

  globus_cond_signal(&lsocket_info->cond);
  globus_mutex_unlock(&lsocket_info->mutex);
      
}

/*--------------------------------------------------------------------*/

void Globus_cleanup_listener_connection(socket_io_type * const socket_info)
{

  if (socket_info->listener_status == REG_COMMS_STATUS_LISTENING)
    Globus_close_listener_handle(socket_info);

  if (socket_info->comms_status == REG_COMMS_STATUS_CONNECTED || 
      socket_info->comms_status == REG_COMMS_STATUS_WAITING_FOR_ACCEPT)
    Globus_close_conn_handle(socket_info);
 
  Globus_socket_info_cleanup(socket_info);
}

/*--------------------------------------------------------------------*/

void Globus_cleanup_connector_connection(socket_io_type * const socket_info)
{
  if (socket_info->comms_status == REG_COMMS_STATUS_CONNECTED ||
      socket_info->comms_status == REG_COMMS_STATUS_WAITING_TO_CONNECT )
    Globus_close_conn_handle(socket_info);

  Globus_socket_info_cleanup(socket_info);
}

/*--------------------------------------------------------------------*/

void Globus_close_conn_handle(socket_io_type * const	socket_info)
{
  globus_result_t	result;

  /* Note that globus cancel is done as part of globus close */

  result = globus_io_register_close(&socket_info->conn_handle, 
				    Globus_close_callback, 
				    (void *) socket_info);

  if (result != GLOBUS_SUCCESS) {
    fprintf(stderr, "Globus_close_conn_handle: Error register close connection\n");
    Globus_error_print(result);
    socket_info->comms_status = REG_COMMS_STATUS_FAILURE;

  }
  else {
    socket_info->comms_status = REG_COMMS_STATUS_CLOSING;
    fprintf(stderr, "DBG: Globus_close_conn_handle:  Register close OK\n");
  }

  Globus_callback_poll(socket_info);

}

/*--------------------------------------------------------------------*/

void Globus_close_listener_handle(socket_io_type * const socket_info)
{
  globus_result_t	result;

  result = globus_io_register_close(&socket_info->listener_handle, 
				    Globus_close_listener_callback, 
				    (void *) socket_info);

  if (result != GLOBUS_SUCCESS) {
    fprintf(stderr, "Globus_close_listener_handle: Error register close connection\n");
    Globus_error_print(result);
    socket_info->listener_status = REG_COMMS_STATUS_FAILURE;
  }
  else {
    fprintf(stderr, "DBG: Globus_close_listener_handle:  Register close OK\n");
    socket_info->listener_status = REG_COMMS_STATUS_CLOSING;
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

  globus_mutex_lock(&lsocket_info->mutex);

  lsocket_info->comms_status = REG_COMMS_STATUS_NULL;

  globus_cond_signal(&lsocket_info->cond);
  globus_mutex_unlock(&lsocket_info->mutex);

#if DEBUG
  fprintf(stderr, "Globus_close_callback: done\n");
#endif
  
}


/*--------------------------------------------------------------------*/

void
Globus_close_listener_callback (void			*callback_arg,
				globus_io_handle_t	*handle,
				globus_result_t		resultparam)
{

  socket_io_type *lsocket_info;
  lsocket_info = ((socket_io_type *) callback_arg);

  globus_mutex_lock(&lsocket_info->mutex);
#if DEBUG
  fprintf(stderr, "Globus_close_listener_callback: done\n");
#endif
  lsocket_info->listener_status = REG_COMMS_STATUS_NULL;
  
  globus_cond_signal(&lsocket_info->cond);
  globus_mutex_unlock(&lsocket_info->mutex);
 
}

/*--------------------------------------------------------------------*/

void Globus_attempt_listener_connect(socket_io_type * const socket_info)
{

  if (socket_info->listener_status != REG_COMMS_STATUS_LISTENING) {
    /* Problem with listener_handle - can only be from failure within
       previous Globus_create_listener call - doubt this will ever
       happen. 
    */
#if DEBUG    
      fprintf(stderr, "Globus_attempt_listener_connect:dealing with listener_status not LISTENING \n");
#endif
    if (socket_info->listener_status == REG_COMMS_STATUS_CLOSING)
      Globus_callback_poll(socket_info);

    if (socket_info->listener_status == REG_COMMS_STATUS_FAILURE ||
	socket_info->listener_status == REG_COMMS_STATUS_NULL ) {
      Globus_create_listener(socket_info);
    }
  }

  /* only check comms_status if listener_status is now listening */
  if (socket_info->listener_status == REG_COMMS_STATUS_LISTENING) {

    if (socket_info->comms_status == REG_COMMS_STATUS_LISTENING ||
	socket_info->comms_status == REG_COMMS_STATUS_WAITING_FOR_ACCEPT ||
	socket_info->comms_status == REG_COMMS_STATUS_CLOSING) {
      /* we're waiting for listen/accept/close callback so attempt to kick the callback function */
#if DEBUG    
      fprintf(stderr, "Globus_attempt_listener_connect:kick callback_poll\n");
#endif
      Globus_callback_poll(socket_info);
    }
    
    if (socket_info->comms_status == REG_COMMS_STATUS_FAILURE ||
	socket_info->comms_status == REG_COMMS_STATUS_NULL ) {
      /* connection has broken - we're still listening so see if anything to connect */
#if DEBUG
      fprintf(stderr, "Globus_attempt_listener_connect:retry accept connect\n");
#endif
      Globus_retry_accept_connect(socket_info);
    }
    
  }

}

/*--------------------------------------------------------------------*/

void Globus_retry_accept_connect(socket_io_type * const socket_info)
{
  /* if this called a globus_io read  has failed */
  globus_result_t    result;
  globus_object_t    *error;
 
  /* if we're here and  status is connected  we've lost a connection, so first close socket */
  if (socket_info->comms_status==REG_COMMS_STATUS_CONNECTED) {
    Globus_close_conn_handle(socket_info);
  }

  /* do not attempt reconnect if not yet closed */
  if (socket_info->comms_status != REG_COMMS_STATUS_CLOSING) {
 
    /* assume list_handle is still listening and see if there is a connection to accept */
    result = globus_io_tcp_register_accept(&socket_info->listener_handle, 
					   GLOBUS_NULL, 
					   &socket_info->conn_handle,
					   Globus_accept_callback,
					   (void *) socket_info);

    if (result != GLOBUS_SUCCESS) {
      /* Will get system error here nothing to accept - don't call Globus_error_print for this error */	 
      error =  globus_error_get(result);
      if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_SYSTEM_FAILURE,
					globus_object_get_type(error))) {
	fprintf(stderr, "Globus_retry_accept_connect:  no connection pending\n"); 
      }
      else {
	fprintf(stderr, "Globus_retry_accept_connect: Error registering accept:"); 
	Globus_error_print(result);
      }
      /* Set to FAILURE so retry is attempted again next time round */
      socket_info->comms_status=REG_COMMS_STATUS_FAILURE;
    }
    else {
#if DEBUG
      fprintf(stderr, "Globus_retry_accept_connect: register accept success\n");
#endif
      socket_info->comms_status=REG_COMMS_STATUS_WAITING_FOR_ACCEPT;

      /* attempt to kick the callback function - for accept just registered */
      Globus_callback_poll(socket_info);

    }
  }

}




/*--------------------------------------------------------------------*/

void Globus_attempt_connector_connect(socket_io_type * const socket_info)
{

  if (socket_info->comms_status == REG_COMMS_STATUS_WAITING_TO_CONNECT ||
      socket_info->comms_status == REG_COMMS_STATUS_CLOSING) {
    /* we're waiting for connect/close callback so attempt to kick the callback function */
#if DEBUG    
    fprintf(stderr, "Globus_attempt_connector_connect:kick callback_poll\n");
#endif
    Globus_callback_poll(socket_info);
  }
  
  if (socket_info->comms_status == REG_COMMS_STATUS_FAILURE || 
      socket_info->comms_status == REG_COMMS_STATUS_NULL ) {
    /* connection has broken - try to re-connect */
#if DEBUG
    fprintf(stderr, "Globus_attempt_connector_connect:retry accept connect\n");
#endif
    Globus_retry_connect(socket_info);
  }

}


/*--------------------------------------------------------------------*/

void Globus_retry_connect(socket_io_type * const socket_info){

  /* close the failed connector and retry to connect */
  if (socket_info->comms_status == REG_COMMS_STATUS_CONNECTED) {
    Globus_close_conn_handle(socket_info);
  }
  
  /* do not attempt reconnect if not yet closed */
  if (socket_info->comms_status != REG_COMMS_STATUS_CLOSING) {

    if (Globus_create_connector(socket_info) != REG_SUCCESS) {
#if DEBUG
      fprintf(stderr, "Globus_retry_connect: failed to register connector for IOType\n");
#endif
      /* Set to FAILURE so Globus_create_connector is attempted again next time round */
      socket_info->comms_status=REG_COMMS_STATUS_FAILURE;
    }
  }
  
}

/*--------------------------------------------------------------------*/

void Globus_error_print(const globus_result_t result)
{
  globus_object_t		*error;
  char				*error_string = NULL;

#if DEBUG
  if  (result != GLOBUS_SUCCESS) {
    error =  globus_error_get(result); 

    /* check for globus_io error string */
    error_string = (char *) globus_i_io_error_string_func(error);

    /* if NULL try general error strings */
    if (error_string == NULL)
      error_string = (char *) globus_error_generic_string_func(error);

    if (error_string != NULL)
      fprintf(stderr, "Globus_error_print: %s \n", error_string);

  }
#endif

}

/*--------------------------------------------------------------------*/

int Consume_msg_header_globus(socket_io_type *sock_info,
			      int *DataType,
			      int *Count)
{
  globus_result_t  result;
  globus_size_t    nbytes;
  char             buffer[REG_PACKET_SIZE];

  /* check socket connection has been made */
  if (sock_info->comms_status != 
      REG_COMMS_STATUS_CONNECTED) return REG_FAILURE;

  /* Read header */

#if DEBUG
  /* ARPDBG */
  fprintf(stderr, "Consume_msg_header_globus: calling globus_io_read...\n");
#endif

  /* Blocks until REG_PACKET_SIZE bytes received */
  result = globus_io_read(&(sock_info->conn_handle), 
			  (globus_byte_t *)buffer, 
			  REG_PACKET_SIZE, 
			  REG_PACKET_SIZE, 
			  &nbytes);

  if(result != GLOBUS_SUCCESS){

    fprintf(stderr, "Consume_msg_header_globus: globus_io_read failed\n");
    Globus_error_print(result);
    return REG_FAILURE;
  }

#if DEBUG
  fprintf(stderr, "Consume_msg_header_globus: read <%s> from socket\n",
	  buffer);
#endif

  /* Check for end of data */
  if(!strncmp(buffer, REG_DATA_FOOTER, strlen(REG_DATA_FOOTER))){

    return REG_EOD;
  }
  else if(strncmp(buffer, BEGIN_SLICE_HEADER, strlen(BEGIN_SLICE_HEADER))){

    fprintf(stderr, "Consume_msg_header_globus: incorrect header on slice\n");
    return REG_FAILURE;
  }

  result = globus_io_read(&(sock_info->conn_handle), 
			  (globus_byte_t *)buffer, 
			  REG_PACKET_SIZE, 
			  REG_PACKET_SIZE, 
			  &nbytes);

  if(result != GLOBUS_SUCCESS){

    fprintf(stderr, "Consume_msg_header_globus: globus_io_read failed\n");
    Globus_error_print(result);
    return REG_FAILURE;
  }

#if DEBUG
  fprintf(stderr, "Consume_msg_header_globus: read <%s> from socket\n", 
	  buffer);
#endif

  if(!strstr(buffer, "<Data_type>")){

    return REG_FAILURE;
  }

  sscanf(buffer, "<Data_type>%d</Data_type>", DataType);

  result = globus_io_read(&(sock_info->conn_handle), 
			  (globus_byte_t *)buffer, 
			  REG_PACKET_SIZE, 
			  REG_PACKET_SIZE, 
			  &nbytes);

  if(result != GLOBUS_SUCCESS){
    
    Globus_error_print(result);
    return REG_FAILURE;
  }

#if DEBUG
  fprintf(stderr, "Consume_msg_header_globus: read <%s> from socket\n", 
	  buffer);
#endif

  if(!strstr(buffer, "<Num_objects>")){

    return REG_FAILURE;
  }

  if( sscanf(buffer, "<Num_objects>%d</Num_objects>", Count) != 1){

    fprintf(stderr, "Consume_msg_header_globus: failed to read Num_objects\n");
    return REG_FAILURE;
  }

  result = globus_io_read(&(sock_info->conn_handle), 
			  (globus_byte_t *)buffer, 
			  REG_PACKET_SIZE, 
			  REG_PACKET_SIZE, 
			  &nbytes);

  if(result != GLOBUS_SUCCESS){

    fprintf(stderr, "Consume_msg_header_globus: globus_io_read failed\n");
    Globus_error_print(result);
    return REG_FAILURE;
  }

#if DEBUG
  fprintf(stderr, "Consume_msg_header_globus: read <%s> from socket\n", 
	  buffer);
#endif

  if(strncmp(buffer, END_SLICE_HEADER, strlen(END_SLICE_HEADER))){

    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Emit_msg_header_globus(socket_io_type *sock_info,
			   int DataType,
			   int Count)
{
  char  buffer[5*REG_PACKET_SIZE];
  char  tmp_buffer[REG_PACKET_SIZE];
  char *pchar;
  globus_result_t  result;
  globus_size_t    nbytes;

  pchar = buffer;
  pchar += sprintf(pchar, REG_PACKET_FORMAT, "<ReG_data_slice_header>");
  sprintf(tmp_buffer, "<Data_type>%d</Data_type>", DataType);
  pchar += sprintf(pchar, REG_PACKET_FORMAT, tmp_buffer);
  sprintf(tmp_buffer, "<Num_objects>%d</Num_objects>", Count);
  pchar += sprintf(pchar, REG_PACKET_FORMAT, tmp_buffer);
  pchar += sprintf(pchar, REG_PACKET_FORMAT, "</ReG_data_slice_header>");

#if DEBUG
  fprintf(stderr, "Emit_msg_header_globus: sending >>%s<<\n", buffer);
#endif

  result = globus_io_write(&(sock_info->conn_handle), 
			   (globus_byte_t *)buffer, 
			   strlen(buffer), 
			   &nbytes);

  if (result != GLOBUS_SUCCESS ) {

#if DEBUG
    fprintf(stderr, "Emit_msg_header_globus: failed to write slice header\n");
#endif
    Globus_error_print(result);
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}



#endif /* REG_GLOBUS_STEERING || REG_GLOBUS_SAMPLES */
