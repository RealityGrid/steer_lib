/*-----------------------------------------------------------------------
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


    Globus IO implementation note:

    The Globus IO has only been tested using NON-threaded flavors of
    Globus.  The socket_io_type structure has globus_mutex_t and
    globus_cond_t members which are required should the code need to be
    used with a threaded flavor of Globus.  However, only the callback
    functions have been coded to use these.  Some effor would be
    required to make the rest of the code threadsafe in order to use
    threaded Globus.

-----------------------------------------------------------------------*/

#if REG_GLOBUS_SAMPLES

#include "ReG_Steer_Common.h"
#include "ReG_Steer_Appside_internal.h"
#include "ReG_Steer_Appside_Globus.h"
#include "ReG_Steer_Appside_File.h"
#include <string.h>

#ifndef REG_DEBUG
#define REG_DEBUG 0
#endif

/* Need access to these tables which are actually declared in 
   ReG_Steer_Appside_internal.h */
extern IOdef_table_type IOTypes_table;

extern Steerer_connection_table_type Steerer_connection;

/*--------------------------------------------------------------------*/

int Globus_io_activate()
{
  /* Activate the globus module if it hasn't been activated yet */
  if (!globus_module_activated) {

    if (globus_module_activate(GLOBUS_IO_MODULE) != GLOBUS_SUCCESS) {
      fprintf(stderr, "Globus_io_activate: ERROR: failed to activate "
	      "Globus IO module\n");
      return REG_FAILURE;
    }
    else
    {
#if REG_DEBUG
      fprintf(stderr, "Globus_io_activate: activated Globus IO module "
	      "OK\n");
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
      fprintf(stderr, "Globus_io_deactivate: Globus IO module "
	      "deactivation error\n");
    else {
#if REG_DEBUG
      fprintf(stderr, "Globus_io_deactivate: Globus IO module "
	      "deactivation OK\n");
#endif
    }
  }
}

/*--------------------------------------------------------------------*/

int Globus_socket_info_init(const int index)
{
  globus_result_t	result;
  char                 *pchar;
  char                 *ip_addr;

  if (Globus_io_activate() != REG_SUCCESS)
    return REG_FAILURE;

  globus_mutex_init(&(IOTypes_table.io_def[index].socket_info.mutex), 
		    GLOBUS_NULL);
  globus_cond_init(&(IOTypes_table.io_def[index].socket_info.cond), 
		   GLOBUS_NULL);

  result = globus_io_tcpattr_init(&(IOTypes_table.io_def[index].socket_info.attr));
  if (result != GLOBUS_SUCCESS) {
    fprintf(stderr, "Globus_socket_info_init: Error initialising "
	    "attribute.\n");
    Globus_error_print(result);
    return REG_FAILURE;
  }

  result = globus_io_attr_set_socket_reuseaddr(&(IOTypes_table.io_def[index].socket_info.attr), GLOBUS_TRUE);
  if (result != GLOBUS_SUCCESS) {
    fprintf(stderr, "Globus_socket_info_init: Error setting resueaddr "
	    "attribute.\n");
    Globus_error_print(result);
    return REG_FAILURE;
  }

  /* Set local TCP interface - Get_fully_qualified_hostname uses
     REG_TCP_INTERFACE if it is set */
  if(Get_fully_qualified_hostname(&pchar, &ip_addr) == REG_SUCCESS){

    /* If we only got localhost then don't bother */
    if(!strstr(ip_addr, "127.0.0.1")){

      result = globus_io_attr_set_tcp_interface(&(IOTypes_table.io_def[index].socket_info.attr), 
						ip_addr);
      if(result != GLOBUS_SUCCESS){
	fprintf(stderr, "Globus_socket_info_init: Error setting "
		"tcp interface.\n");
	Globus_error_print(result);
	return REG_FAILURE;
      }
    }
#if REG_DEBUG
    else{
      fprintf(stderr, 
	  "Globus_socket_info_init: "
	  "Get_fully_qualified_hostname returned IP of localhost\n"
	  "                         so not setting globus_io "
	  "tcp interface - will use INADDR_ANY\n");
    }
#endif
  }

  /* SMR XXX - add more attr security stuff here */

  IOTypes_table.io_def[index].socket_info.listener_port = 0;
  IOTypes_table.io_def[index].socket_info.listener_status = REG_COMMS_STATUS_NULL;
  IOTypes_table.io_def[index].socket_info.comms_status = REG_COMMS_STATUS_NULL;   
  IOTypes_table.io_def[index].socket_info.connector_port = 0;
  sprintf(IOTypes_table.io_def[index].socket_info.connector_hostname, " ");

  return REG_SUCCESS;

}

/*--------------------------------------------------------------------*/

void Globus_socket_info_cleanup(const int index)
{
  globus_result_t	result;

  globus_mutex_destroy(&(IOTypes_table.io_def[index].socket_info.mutex));
  globus_cond_destroy(&(IOTypes_table.io_def[index].socket_info.cond));

  result = globus_io_tcpattr_destroy(&(IOTypes_table.io_def[index].socket_info.attr));
  if (result != GLOBUS_SUCCESS) {
    fprintf(stderr, "Globus_socket_info_cleanup: Error destroying "
	    "attribute.\n");
    Globus_error_print(result);
  }
#if REG_DEBUG
  fprintf(stderr, "Globus_socket_info_cleanup: done\n");
#endif
}

/*--------------------------------------------------------------------*/

void Globus_callback_poll(const int index)
{
  globus_abstime_t	timeout;

  timeout.tv_sec  = 1;
  timeout.tv_nsec = 0;

  if( globus_mutex_lock(&(IOTypes_table.io_def[index].socket_info.mutex)) ){

    fprintf(stderr, "Globus_callback_poll: failed to acquire mutex\n");
    return;
  }

  /* attempt to kick the callback function */
#if REG_DEBUG
  fprintf(stderr, "Globus_callback_poll: polling callbacks\n");
#endif
  globus_cond_timedwait(&(IOTypes_table.io_def[index].socket_info.cond), 
			&(IOTypes_table.io_def[index].socket_info.mutex), 
			&timeout);

  globus_mutex_unlock(&(IOTypes_table.io_def[index].socket_info.mutex)); 
  
}

/*--------------------------------------------------------------------*/

int Globus_create_listener(const int index)
{
  globus_result_t	result;
  char                 *pchar;
  char                 *ip_addr;

  /* create listener socket on free port 
   *  - if environment variable GLOBUS_TCP_PORT_RANGE has been set,
   *  only port numbers within that range will be used 
   */

  result = globus_io_tcp_create_listener(&(IOTypes_table.io_def[index].socket_info.listener_port), 
					 -1, 
					 &(IOTypes_table.io_def[index].socket_info.attr), 
					 &(IOTypes_table.io_def[index].socket_info.listener_handle));
  if (result != GLOBUS_SUCCESS) {
    fprintf(stderr, "Globus_create_listener: Error creating listening socket\n");
    Globus_error_print(result);
    return REG_FAILURE;
  }

  /* NOTE: this is the listener_status */
  IOTypes_table.io_def[index].socket_info.listener_status=REG_COMMS_STATUS_LISTENING;

  fprintf(stderr, "Globus_create_listener: listener on port %d\n", 
	  IOTypes_table.io_def[index].socket_info.listener_port);

  /* Get the hostname of the machine we're running on (so we can publish
     the endpoint of this connection). If REG_IO_ADDRESS is set then
     that's what we use.  If not, call Get_fully_qualified_hostname which 
     itself uses  REG_TCP_INTERFACE if set. */
  if(pchar = getenv("REG_IO_ADDRESS")){

    strcpy(IOTypes_table.io_def[index].socket_info.listener_hostname, pchar);
  }
  else if(Get_fully_qualified_hostname(&pchar, &ip_addr) == REG_SUCCESS){

    strcpy(IOTypes_table.io_def[index].socket_info.listener_hostname, pchar);
  }
  else{

    fprintf(stderr, "Globus_create_listener: WARNING: failed to get "
	    "hostname\n");
    sprintf(IOTypes_table.io_def[index].socket_info.listener_hostname, 
	    "NOT_SET");
  }
  
  /* Now register listener so can accept connections when they happen */
  result = globus_io_tcp_register_listen(&(IOTypes_table.io_def[index].socket_info.listener_handle),
					 Globus_listener_callback,
					 (void *)&(IOTypes_table.io_def[index].socket_info));
  if (result != GLOBUS_SUCCESS) {
    fprintf(stderr, "Globus_create_listener: Error registering listen\n");
    Globus_error_print(result);

    /* If this fails, must be something wrong with listener_handle, so
       register a close for it (this will change listener_status
       value) and thus create listener will be reattempted in the next
       Emit_start. Don't ever think this will happen...
    */

    Globus_close_listener_handle(index);
    return REG_FAILURE;
  }

  IOTypes_table.io_def[index].socket_info.comms_status=REG_COMMS_STATUS_LISTENING;
  Globus_callback_poll(index);

  return REG_SUCCESS;

}
/*--------------------------------------------------------------------*/

void
Globus_listener_callback (void			*callback_arg,
			  globus_io_handle_t	*handle,
			  globus_result_t	resultparam)
{ 
  globus_result_t  result;
  socket_io_type  *lsocket_info;

  lsocket_info = ((socket_io_type *) callback_arg);

  globus_mutex_lock(&lsocket_info->mutex);

  if (resultparam != GLOBUS_SUCCESS) {

#if REG_DEBUG
    fprintf(stderr, "Entered Globus_listener_callback: resultparam is "
	    "FALSE, no connection\n");
#endif
    lsocket_info->comms_status=REG_COMMS_STATUS_FAILURE;
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
      fprintf(stderr, "Globus_listener_callback: Error registering accept\n");
      Globus_error_print(result);
    }
    else {
      lsocket_info->comms_status=REG_COMMS_STATUS_WAITING_FOR_ACCEPT;
#if REG_DEBUG
      fprintf(stderr, "DBG: Globus_listener_callback - registered accept \n");
#endif
    }
  }

  /* Have to register listener again in order to get future connections */
  if(lsocket_info->listener_status != REG_COMMS_STATUS_CLOSING){

    result = globus_io_tcp_register_listen(&(lsocket_info->listener_handle),
                                         Globus_listener_callback,
                                         (void *) lsocket_info);

    if (result == GLOBUS_SUCCESS) {

#if REG_DEBUG
      fprintf(stderr, "Globus_listener_callback - registered listener \n");
#endif
      lsocket_info->listener_status=REG_COMMS_STATUS_LISTENING;
    }
    else{
      fprintf(stderr, "Globus_listener_callback - ERROR failed to register"
              " listener \n");
      Globus_error_print(result);
      lsocket_info->listener_status=REG_COMMS_STATUS_FAILURE;
    }
  }
  
  globus_cond_signal(&lsocket_info->cond);
  globus_mutex_unlock(&lsocket_info->mutex);
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
#if REG_DEBUG
    fprintf(stderr, "DBG: Entered Globus_accept_callback resultparam "
	    "FALSE, no connection\n");
#endif
    Globus_error_print(resultparam);
    lsocket_info->comms_status=REG_COMMS_STATUS_FAILURE;
  }
  else {
#if REG_DEBUG
    fprintf(stderr, "DBG: Globus_accept_callback has connected\n");
#endif
    lsocket_info->comms_status=REG_COMMS_STATUS_CONNECTED;
  }

  globus_cond_signal(&lsocket_info->cond);
  globus_mutex_unlock(&lsocket_info->mutex);  
}

/*--------------------------------------------------------------------*/

int Globus_create_connector(const int index)
{
  globus_result_t	result;

  /* Register connector using port and hostname parameter
   * - will connect and call callback function when port listens
   */

  if (IOTypes_table.io_def[index].socket_info.connector_port == 0) return REG_FAILURE;

  result = globus_io_tcp_register_connect(IOTypes_table.io_def[index].socket_info.connector_hostname,
					  IOTypes_table.io_def[index].socket_info.connector_port,
					  &IOTypes_table.io_def[index].socket_info.attr,
					  Globus_connector_callback,
					  (void *) &(IOTypes_table.io_def[index].socket_info),
					  &IOTypes_table.io_def[index].socket_info.conn_handle);

  if (result != GLOBUS_SUCCESS) {
    fprintf(stderr, "Globus_create_connector: Error registering connect\n");
    fprintf(stderr, "                         hostname = %s\n", 
	    IOTypes_table.io_def[index].socket_info.connector_hostname);
    fprintf(stderr, "                         port = %d\n", 
	    (int)IOTypes_table.io_def[index].socket_info.connector_port);
    Globus_error_print(result);
    IOTypes_table.io_def[index].socket_info.comms_status=REG_COMMS_STATUS_FAILURE;
    return REG_FAILURE;
  }
  else {
    IOTypes_table.io_def[index].socket_info.comms_status=REG_COMMS_STATUS_WAITING_TO_CONNECT;

    fprintf(stderr, "Globus_create_connector: registered connector on "
	    "connector_port = %d, connector_hostname = %s\n", 
	    IOTypes_table.io_def[index].socket_info.connector_port, 
	    IOTypes_table.io_def[index].socket_info.connector_hostname);
  }

  /* attempt to kick the callback function */
  Globus_callback_poll(index);

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
#if REG_DEBUG
    fprintf(stderr, "DBG: Entered Globus_connector_callback "
	    "resultparam FALSE, no connection\n");
#endif
    Globus_error_print(resultparam);
  }
  else {
    lsocket_info->comms_status=REG_COMMS_STATUS_CONNECTED;
#if REG_DEBUG
    fprintf(stderr, "DBG: Globus_connector_callback has connected\n");
#endif
  }

  globus_cond_signal(&lsocket_info->cond);
  globus_mutex_unlock(&lsocket_info->mutex);
}

/*--------------------------------------------------------------------*/

void Globus_cleanup_listener_connection(const int index)
{
  if (IOTypes_table.io_def[index].socket_info.listener_status == REG_COMMS_STATUS_LISTENING){
    Globus_close_listener_handle(index);
  }

  if (IOTypes_table.io_def[index].socket_info.comms_status == REG_COMMS_STATUS_CONNECTED || 
      IOTypes_table.io_def[index].socket_info.comms_status == REG_COMMS_STATUS_WAITING_FOR_ACCEPT){
    Globus_close_conn_handle(index);
  }

  /* Flag that this listener is dead - used in Emit_IOType_defs */
  sprintf(IOTypes_table.io_def[index].socket_info.listener_hostname, 
	  "NOT_SET");
  IOTypes_table.io_def[index].socket_info.listener_port = 0;
}

/*--------------------------------------------------------------------*/

void Globus_cleanup_connector_connection(const int index)
{
  if (IOTypes_table.io_def[index].socket_info.comms_status == REG_COMMS_STATUS_CONNECTED ||
      IOTypes_table.io_def[index].socket_info.comms_status == REG_COMMS_STATUS_WAITING_TO_CONNECT ){
    Globus_close_conn_handle(index);
  }
}

/*--------------------------------------------------------------------*/

void Globus_close_conn_handle(const int index)
{
  globus_result_t	result;

  /* Note that globus cancel is done as part of globus close */

  result = globus_io_register_close(&IOTypes_table.io_def[index].socket_info.conn_handle, 
				    Globus_close_callback, 
				    (void *) &IOTypes_table.io_def[index].socket_info);

  if (result != GLOBUS_SUCCESS) {
    fprintf(stderr, "Globus_close_conn_handle: Error register close "
	    "connection\n");
    Globus_error_print(result);
    IOTypes_table.io_def[index].socket_info.comms_status = REG_COMMS_STATUS_FAILURE;
  }
  else {
    IOTypes_table.io_def[index].socket_info.comms_status = REG_COMMS_STATUS_CLOSING;
#if REG_DEBUG
    fprintf(stderr, "DBG: Globus_close_conn_handle:  Register close OK\n");
#endif
  }

  Globus_callback_poll(index);
}

/*--------------------------------------------------------------------*/

void Globus_close_listener_handle(const int index)
{
  globus_result_t	result;

  result = globus_io_register_close(&IOTypes_table.io_def[index].socket_info.listener_handle, 
				    Globus_close_listener_callback, 
				    (void *) &IOTypes_table.io_def[index].socket_info);

  if (result != GLOBUS_SUCCESS) {
    fprintf(stderr, "Globus_close_listener_handle: Error register close "
	    "connection\n");
    Globus_error_print(result);
    IOTypes_table.io_def[index].socket_info.listener_status = REG_COMMS_STATUS_FAILURE;
  }
  else {
#if REG_DEBUG
    fprintf(stderr, "DBG: Globus_close_listener_handle:  Register close OK\n");
#endif
    IOTypes_table.io_def[index].socket_info.listener_status = REG_COMMS_STATUS_CLOSING;
  }

  Globus_callback_poll(index);
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

#if REG_DEBUG
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
#if REG_DEBUG
  fprintf(stderr, "Globus_close_listener_callback: done\n");
#endif
  lsocket_info->listener_status = REG_COMMS_STATUS_NULL;
  
  globus_cond_signal(&lsocket_info->cond);
  globus_mutex_unlock(&lsocket_info->mutex);
}

/*--------------------------------------------------------------------*/

void Globus_attempt_listener_connect(const int index)
{
  socket_io_type *socket_info;
  socket_info = &(IOTypes_table.io_def[index].socket_info);

  if (socket_info->listener_status != REG_COMMS_STATUS_LISTENING) {
    /* Problem with listener_handle - can only be from failure within
       previous Globus_create_listener call - doubt this will ever
       happen. 
    */
#if REG_DEBUG
    fprintf(stderr, "Globus_attempt_listener_connect:dealing with "
	      "listener_status not LISTENING \n");
#endif
    if (socket_info->listener_status == REG_COMMS_STATUS_CLOSING){

      Globus_callback_poll(index);
    }

    if (socket_info->listener_status == REG_COMMS_STATUS_FAILURE ||
	socket_info->listener_status == REG_COMMS_STATUS_NULL ) {
      Globus_create_listener(index);
    }
  }

  /* only check comms_status if listener_status is now listening */
  if (socket_info->listener_status == REG_COMMS_STATUS_LISTENING) {

    if (socket_info->comms_status == REG_COMMS_STATUS_LISTENING ||
	socket_info->comms_status == REG_COMMS_STATUS_WAITING_FOR_ACCEPT ||
	socket_info->comms_status == REG_COMMS_STATUS_CLOSING) {
      /* we're waiting for listen/accept/close callback so attempt to 
         kick the callback function */
#if REG_DEBUG
      fprintf(stderr, "Globus_attempt_listener_connect:kick callback_poll\n");
#endif
      Globus_callback_poll(index);
    }
    
    if (socket_info->comms_status == REG_COMMS_STATUS_FAILURE ||
	socket_info->comms_status == REG_COMMS_STATUS_NULL ) {
      /* connection has broken - we're still listening so see if 
         anything to connect */
#if REG_DEBUG
      fprintf(stderr, "Globus_attempt_listener_connect:retry accept "
	      "connect\n");
#endif
      Globus_retry_accept_connect(index);
    }

    /* Because two callbacks have to be made to accept a new connection,
       try kicking callback a second time here if waiting to accept
       connection */
    if (socket_info->comms_status == REG_COMMS_STATUS_WAITING_FOR_ACCEPT){

#if REG_DEBUG
      fprintf(stderr, "Globus_attempt_listener_connect: 2nd kick "
	      "callback_poll\n");
#endif
      Globus_callback_poll(index);

      /* One more try because, by this stage, we've normally almost
	 got an active connection */
      if(socket_info->comms_status != REG_COMMS_STATUS_CONNECTED){

#if REG_DEBUG
	fprintf(stderr, "Globus_attempt_listener_connect: 3rd kick "
	      "callback_poll\n");
#endif
	Globus_callback_poll(index);
      }
    }
  }
}

/*--------------------------------------------------------------------*/

void Globus_retry_accept_connect(const int index)
{
  socket_io_type *socket_info;
  socket_info = &(IOTypes_table.io_def[index].socket_info);

  /* if this called a globus_io write has failed */

  /* if we're here and  status is connected  we've lost a connection, 
     so first close socket */
  if (socket_info->comms_status==REG_COMMS_STATUS_CONNECTED) {
    Globus_close_conn_handle(index);
  }

  /* do not attempt reconnect if not yet closed */
  if (socket_info->comms_status != REG_COMMS_STATUS_CLOSING) {
    Globus_callback_poll(index);
  }
}

/*--------------------------------------------------------------------*/

void Globus_attempt_connector_connect(int index)
{
  socket_io_type *socket_info;

  socket_info = &(IOTypes_table.io_def[index].socket_info);

  if (socket_info->comms_status == REG_COMMS_STATUS_WAITING_TO_CONNECT ||
      socket_info->comms_status == REG_COMMS_STATUS_CLOSING) {
    /* we're waiting for connect/close callback so attempt to kick the 
       callback function */
#if REG_DEBUG    
    fprintf(stderr, "Globus_attempt_connector_connect:kick callback_poll\n");
#endif
    Globus_callback_poll(index);
  }
  
  if (socket_info->comms_status == REG_COMMS_STATUS_FAILURE || 
      socket_info->comms_status == REG_COMMS_STATUS_NULL ) {
    /* connection has broken - try to re-connect */
#if REG_DEBUG
    fprintf(stderr, "Globus_attempt_connector_connect:retry accept connect\n");
#endif
    Globus_retry_connect(index);
  }
}

/*--------------------------------------------------------------------*/

void Globus_retry_connect(int index){

  int             status;
  socket_io_type *socket_info;
  socket_info = &(IOTypes_table.io_def[index].socket_info);

  /* close the failed connector and retry to connect */
  if (socket_info->comms_status == REG_COMMS_STATUS_CONNECTED) {
    /* Reset connector port (to force us to go and look for it
       again in case it has changed) */
    socket_info->connector_port = 0;
    Globus_close_conn_handle(index);
  }
  
  /* do not attempt reconnect if not yet closed */
  if (socket_info->comms_status != REG_COMMS_STATUS_CLOSING) {

    /* Check to see that we know where we're supposed to be connecting to */
    if(socket_info->connector_port == 0){

#if REG_SOAP_STEERING	  
      /* Go out into the world of grid services... */
      status = Get_data_source_address_soap(IOTypes_table.io_def[index].input_index, 
					    socket_info->connector_hostname,
					    &(socket_info->connector_port)) ;
#else
      /* Attempt to get port and hostname from env. variables */
      status = Get_data_source_address_file(IOTypes_table.io_def[index].input_index, 
					    socket_info->connector_hostname,
					    &(socket_info->connector_port));
#endif

      if(status != REG_SUCCESS || socket_info->connector_port == 0){
#if REG_DEBUG
	fprintf(stderr, "Globus_retry_connect: failed to get address "
		"to connect to\n");
#endif
	/* Set to FAILURE so Globus_create_connector is attempted again 
	   next time round */
	socket_info->comms_status=REG_COMMS_STATUS_FAILURE;
	return;
      }      
    }

    if (Globus_create_connector(index) != REG_SUCCESS) {
#if REG_DEBUG
      fprintf(stderr, "Globus_retry_connect: failed to register "
	      "connector for IOType\n");
#endif
      /* Set to FAILURE so Globus_create_connector is attempted again 
         next time round */
      socket_info->comms_status=REG_COMMS_STATUS_FAILURE;
    }
  }
}

/*--------------------------------------------------------------------*/

void Globus_error_print(const globus_result_t result)
{
#if REG_DEBUG
  globus_object_t		*error;
  char				*error_string;

  if  (result != GLOBUS_SUCCESS) {
    error =  globus_error_get(result); 

    if(!error)return;

    if(globus_object_type_match(globus_object_get_type(error),
				GLOBUS_IO_ERROR_TYPE_EOF) ){
      fprintf(stderr, "\nGlobus_error_print: got EOF\n");
    }

    /* Get error string */
    error_string = NULL;
    error_string = globus_object_printable_to_string(error);

    if (error_string){
      fprintf(stderr, "Globus_error_print: %s \n", error_string);
      free(error_string);
    }

    globus_object_free(error);
  }
#endif
}

/*--------------------------------------------------------------------*/

int Consume_msg_header_globus(int  index,
			      int *DataType,
			      int *Count,
			      int *NumBytes,
			      int *IsFortranArray)
{
  globus_result_t  result;
  globus_size_t    nbytes;
  char             buffer[REG_PACKET_SIZE];
  socket_io_type  *sock_info;

  sock_info = &(IOTypes_table.io_def[index].socket_info);

  /* check socket connection has been made */
  if (sock_info->comms_status != 
      REG_COMMS_STATUS_CONNECTED) return REG_FAILURE;

  /* Read header */

#if REG_DEBUG
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

#if REG_DEBUG
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

  /*--- Type of objects in message ---*/

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

#if REG_DEBUG
  fprintf(stderr, "Consume_msg_header_globus: read <%s> from socket\n", 
	  buffer);
#endif

  if(!strstr(buffer, "<Data_type>")){

    return REG_FAILURE;
  }

  sscanf(buffer, "<Data_type>%d</Data_type>", DataType);

  /*--- No. of objects in message ---*/

  result = globus_io_read(&(sock_info->conn_handle), 
			  (globus_byte_t *)buffer, 
			  REG_PACKET_SIZE, 
			  REG_PACKET_SIZE, 
			  &nbytes);

  if(result != GLOBUS_SUCCESS){
    
    Globus_error_print(result);
    return REG_FAILURE;
  }

#if REG_DEBUG
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

  /*--- No. of bytes in message ---*/

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

#if REG_DEBUG
  fprintf(stderr, "Consume_msg_header_globus: read >%s< from socket\n", 
	  buffer);
#endif

  if(!strstr(buffer, "<Num_bytes>")){

    return REG_FAILURE;
  }

  if( sscanf(buffer, "<Num_bytes>%d</Num_bytes>", NumBytes) != 1){

    fprintf(stderr, "Consume_msg_header_globus: failed to read Num_bytes\n");
    return REG_FAILURE;
  }

  /*--- Array ordering in message ---*/

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

#if REG_DEBUG
  fprintf(stderr, "Consume_msg_header_globus: read >%s< from socket\n", 
	  buffer);
#endif

  if(!strstr(buffer, "<Array_order>")){

    return REG_FAILURE;
  }

  if(strstr(buffer, "FORTRAN")){

    /* Array data is from Fortran */
    *IsFortranArray = TRUE;
  }
  else{
    /* Array data is not from Fortran */
    *IsFortranArray = FALSE;
  }

  /*--- End of header ---*/

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

#if REG_DEBUG
  fprintf(stderr, "Consume_msg_header_globus: read <%s> from socket\n", 
	  buffer);
#endif

  if(strncmp(buffer, END_SLICE_HEADER, strlen(END_SLICE_HEADER))){

    fprintf(stderr, "Consume_msg_header_globus: failed to find "
	    "end of header\n");
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Write_globus(const globus_io_handle_t *handle,
		 const int n,
		 void *buffer)
{
  globus_size_t   nbytes, bytes_left;
  globus_result_t result;
  char           *pchar;
  int             count = 0;
  if(n < 0){
    fprintf(stderr, "Write_globus: requested to write < 0 bytes!\n");
    return REG_FAILURE;
  }
  else if(n == 0){
    fprintf(stderr, "Write_globus: ARPDBG: asked to send 0 bytes?!?\n");
    return REG_SUCCESS;
  }

  nbytes = (globus_size_t)n;
  bytes_left = nbytes;
  pchar = (char *)buffer;

  while(bytes_left > 0){

    result = globus_io_try_write((globus_io_handle_t *)handle, 
				 (globus_byte_t *)pchar, 
				 bytes_left,
				 &nbytes);

    if(result != GLOBUS_SUCCESS){
      fprintf(stderr, "Write_globus: call to globus_io_try_write failed\n");
      Globus_error_print(result);
      return REG_FAILURE;
    }
    else{
      bytes_left -= nbytes;
      pchar += nbytes;
    }
    count++;
  }

  if(bytes_left > 0){
#if REG_DEBUG
    fprintf(stderr, "Write_globus: timed-out trying to write data\n");
#endif
    return REG_TIMED_OUT;
  }
  else{
    return REG_SUCCESS;
  }
}

/*----------------------------------------------------------------*/

int Write_globus_non_blocking(const globus_io_handle_t *handle,
			      const int n,
			      void *buffer)
{
  globus_size_t   nbytes, bytes_left;
  globus_result_t result;
  char           *pchar;
  int             count = 0;
  if(n < 0){
    fprintf(stderr, "Write_globus_non_blocking: requested to write < 0 bytes!\n");
    return REG_FAILURE;
  }
  else if(n == 0){
    fprintf(stderr, "Write_globus_non_blocking: ARPDBG: asked to send 0 bytes?!?\n");
    return REG_SUCCESS;
  }

  nbytes = (globus_size_t)n;
  bytes_left = nbytes;
  pchar = (char *)buffer;

  while(bytes_left > 0 && count < 10){

    result = globus_io_try_write((globus_io_handle_t *)handle, 
				 (globus_byte_t *)pchar, 
				 bytes_left,
				 &nbytes);

    if(result != GLOBUS_SUCCESS){
      fprintf(stderr, "Write_globus_non_blocking: call to globus_io_try_write failed\n");
      Globus_error_print(result);
      return REG_FAILURE;
    }
    else{
      bytes_left -= nbytes;
      pchar += nbytes;
    }
    count++;
  }

  if(bytes_left > 0){
#if REG_DEBUG
    fprintf(stderr, "Write_globus_non_blocking: timed-out trying to "
	    "write data\n");
#endif
    return REG_TIMED_OUT;
  }
  else{
    return REG_SUCCESS;
  }
}

/*--------------------------------------------------------------------*/

int Initialize_IOType_transport_globus(const int direction,
				       const int index)
{
  int          return_status = REG_SUCCESS;
  static int   call_count = 1;

  /* set up socket_info for callback */
  if (Globus_socket_info_init(index) != REG_SUCCESS) {
#if REG_DEBUG
    fprintf(stderr, "Initialize_IOType_transport_globus: failed to "
	    "initialise socket info for IOType\n");
#endif
    return_status = REG_FAILURE;
  } 
  else {
      
    if (direction == REG_IO_OUT){

      /* Don't create socket yet if this flag is set */
      if (IOTypes_table.enable_on_registration == FALSE) return REG_SUCCESS;

      /* open socket and register callback function to listen for and
	 accept connections */
      if (Globus_create_listener(index) != REG_SUCCESS){
#if REG_DEBUG
	fprintf(stderr, "Initialize_IOType_transport_globus: failed to "
		"create listener "
		"for IOType\n");
#endif
	return_status = REG_FAILURE;
      }
      else{
	  
#if REG_DEBUG
	fprintf(stderr, "Initialize_IOType_transport_globus: Created "
		"listener on port %d, "
		"index %d, label %s\n", 
		IOTypes_table.io_def[index].socket_info.listener_port, 
		index, IOTypes_table.io_def[index].label );
#endif
      }
    }
    else if (direction == REG_IO_IN){

      IOTypes_table.io_def[index].input_index = call_count;
      call_count++;

#if REG_SOAP_STEERING	  

      /* Go out into the world of grid services... */
      return_status = Get_data_source_address_soap(IOTypes_table.io_def[index].input_index, 
		   IOTypes_table.io_def[index].socket_info.connector_hostname,
	           &(IOTypes_table.io_def[index].socket_info.connector_port));
#else
      /* get hostname and port from environment variables */
      return_status = Get_data_source_address_file(IOTypes_table.io_def[index].input_index, 
		   IOTypes_table.io_def[index].socket_info.connector_hostname,
		   &(IOTypes_table.io_def[index].socket_info.connector_port));

#endif /* !REG_SOAP_STEERING */

      /* register connector against port */
  
      if (return_status == REG_SUCCESS) {
	
	/* Don't create socket yet if this flag is set */
	if (IOTypes_table.enable_on_registration == FALSE) {
	  return REG_SUCCESS;
	}

	/* Check that we did get a valid port to connect to */
	if (IOTypes_table.io_def[index].socket_info.connector_port == 0){
	  return REG_SUCCESS;
	}

	if (Globus_create_connector(index) != REG_SUCCESS) {
#if REG_DEBUG
	  fprintf(stderr, "Initialize_IOType_transport_globus: failed to "
		  "register connector for IOType\n");
#endif
	  return_status = REG_FAILURE;
	}
	else{

#if REG_DEBUG
	  fprintf(stderr, "Initialize_IOType_transport_globus: registered"
		  " connector on port %d, hostname = %s, index %d, "
		  "label %s\n", 
		  IOTypes_table.io_def[index].socket_info.connector_port,
		  IOTypes_table.io_def[index].socket_info.connector_hostname,
		  index, IOTypes_table.io_def[index].label );
#endif
	}
      }
      else{
	fprintf(stderr, "Initialize_IOType_transport_globus: cannot "
		"create connector as port and hostname not set\n");
      }
    }
  }

  return return_status;
}

/*---------------------------------------------------*/

void Finalize_IOType_transport_globus()
{
  int index;

  for (index=0; index<IOTypes_table.num_registered; index++) {
    if (IOTypes_table.io_def[index].direction == REG_IO_OUT) {
      /* close globus sockets */
      Globus_cleanup_listener_connection(index);
      Globus_socket_info_cleanup(index);
    }
    else if (IOTypes_table.io_def[index].direction == REG_IO_IN) {
      /* close globus sockets */
      Globus_cleanup_connector_connection(index);
      Globus_socket_info_cleanup(index);
    }
  }
  /* deactivate globus module */
  Globus_io_deactivate();
}

/*---------------------------------------------------*/

int Disable_IOType_globus(int index)
{
  /* check index is valid */
  if(index < 0 || index >= IOTypes_table.num_registered){

    fprintf(stderr, "Disable_IOType_globus: index out of range\n");
    return REG_FAILURE;
  }

  if (IOTypes_table.io_def[index].direction == REG_IO_OUT) {
    /* close globus sockets */
    Globus_cleanup_listener_connection(index);

    while(IOTypes_table.io_def[index].socket_info.comms_status != REG_COMMS_STATUS_NULL
	  && IOTypes_table.io_def[index].socket_info.comms_status != REG_COMMS_STATUS_FAILURE){
      Globus_callback_poll(index);
    }
  }
  else if (IOTypes_table.io_def[index].direction == REG_IO_IN) {
    /* close globus sockets */
    Globus_cleanup_connector_connection(index);

    while(IOTypes_table.io_def[index].socket_info.comms_status != REG_COMMS_STATUS_NULL
	  && IOTypes_table.io_def[index].socket_info.comms_status != REG_COMMS_STATUS_FAILURE){
      Globus_callback_poll(index);
    }
  }

  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Enable_IOType_globus(int index)
{
  /* check index is valid */
  if(index < 0 || index >= IOTypes_table.num_registered)return REG_FAILURE;

  if(IOTypes_table.io_def[index].direction == REG_IO_OUT){

    /* open socket and register callback function to listen for and
       accept connections */
    if (Globus_create_listener(index) != REG_SUCCESS) {
#if REG_DEBUG
      fprintf(stderr, "Enable_IOType_globus: failed to create listener "
	      "for IOType\n");
#endif
      return REG_FAILURE;
    }
  }
  else if (IOTypes_table.io_def[index].direction == REG_IO_IN){

    if(IOTypes_table.io_def[index].socket_info.connector_port == 0){

#if REG_SOAP_STEERING	  

      /* Go out into the world of grid services... */
      if( Get_data_source_address_soap(IOTypes_table.io_def[index].input_index, 
		   IOTypes_table.io_def[index].socket_info.connector_hostname,
	           &(IOTypes_table.io_def[index].socket_info.connector_port)) 
	  != REG_SUCCESS  || 
	  IOTypes_table.io_def[index].socket_info.connector_port == 0){

	return REG_FAILURE;
      }
#else
      /* Attempt to get port and hostname from env. variables */
      if( Get_data_source_address_file(IOTypes_table.io_def[index].input_index, 
		   IOTypes_table.io_def[index].socket_info.connector_hostname,
	           &(IOTypes_table.io_def[index].socket_info.connector_port)) 
	  != REG_SUCCESS || 
	  IOTypes_table.io_def[index].socket_info.connector_port == 0){

	return REG_FAILURE;
      }      
#endif
    }

    if (Globus_create_connector(index) != REG_SUCCESS) {
#if REG_DEBUG
      fprintf(stderr, "Enable_IOType_globus: failed to "
	      "register connector for IOType\n");
#endif
      return REG_FAILURE;
    }
  }

  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Consume_start_data_check_globus(const int index)
{  
  char            buffer[REG_PACKET_SIZE];
  char           *pstart;
  globus_size_t   nbytes, nbytes1;
  globus_result_t result;
  int             attempt_reconnect;
  int             status;

  /* if not connected attempt to connect now */
  if (IOTypes_table.io_def[index].socket_info.comms_status 
      != REG_COMMS_STATUS_CONNECTED){

    /* Check to see that we know where we're supposed to be connecting to */
    if(IOTypes_table.io_def[index].socket_info.connector_port == 0){

#if REG_SOAP_STEERING	  
      /* Go out into the world of grid services... */
      status = Get_data_source_address_soap(IOTypes_table.io_def[index].input_index, 
		   IOTypes_table.io_def[index].socket_info.connector_hostname,
		   &(IOTypes_table.io_def[index].socket_info.connector_port)) ;
#else
      /* Attempt to get port and hostname from env. variables */
      status = Get_data_source_address_file(IOTypes_table.io_def[index].input_index, 
		   IOTypes_table.io_def[index].socket_info.connector_hostname,
		   &(IOTypes_table.io_def[index].socket_info.connector_port));
#endif

      if(status != REG_SUCCESS || 
	 IOTypes_table.io_def[index].socket_info.connector_port == 0){
	
	return REG_FAILURE;
      }      
    }

    Globus_attempt_connector_connect(index);
  }

  /* check if socket connection has been made */
  if (IOTypes_table.io_def[index].socket_info.comms_status != 
      REG_COMMS_STATUS_CONNECTED) {
#if REG_DEBUG
    fprintf(stderr, "Consume_start_data_check_globus: socket is NOT "
	    "connected, index = %d\n",index );
#endif
    return REG_FAILURE;
  }

#if REG_DEBUG
  fprintf(stderr, "Consume_start_data_check_globus: socket status is connected, index = %d\n",
	  index );
#endif

  /* Drain socket until start tag found */
  attempt_reconnect = 1;
  memset(buffer, '\0', 1);

#if REG_DEBUG
  fprintf(stderr, "Consume_start_data_check_globus: searching for start tag\n");
#endif

  while(!(pstart = strstr(buffer, REG_DATA_HEADER))){

#if REG_DEBUG
    fprintf(stderr, ".");
#endif
    result = globus_io_try_read(&(IOTypes_table.io_def[index].socket_info.conn_handle),
				(globus_byte_t *)buffer,
				REG_PACKET_SIZE,
				&nbytes);

    if(result != GLOBUS_SUCCESS){

      /* We're in the middle of a while loop here so don't keep trying
	 to reconnect ad infinitum */
      if(!attempt_reconnect){
	return REG_FAILURE;
      }

#if REG_DEBUG
      fprintf(stderr, "\nConsume_start_data_check_globus: globus_io_try_read"
	      " failed - try immediate reconnect for index %d\n", index);
      Globus_error_print(result);
#endif

      Globus_retry_connect(index);
     
      /* check if socket reconnection has been made and check for 
	 data if it has */
      if (IOTypes_table.io_def[index].socket_info.comms_status != 
	  REG_COMMS_STATUS_CONNECTED) {

	return REG_FAILURE;
      }

      attempt_reconnect = 0;
      memset(buffer, '\0', 1);
    }
    else if(nbytes == 0){
#if REG_DEBUG
      fprintf(stderr, "\n");
#endif
      /* Call was OK but there's no data to read... */
      return REG_FAILURE;
    }
  }
#if REG_DEBUG
  fprintf(stderr, "\n");
#endif

  if(nbytes > 0){

#if REG_DEBUG
    fprintf(stderr, "Consume_start_data_check_globus: read >>%s<< "
	    "from socket\n", buffer);
#endif

    /* Need to make sure we've read the full packet marking the 
       beginning of the next data slice */
    nbytes1 = (int)(pstart - buffer) + (REG_PACKET_SIZE - nbytes);

    if(nbytes1 > 0){
      result = globus_io_try_read(&(IOTypes_table.io_def[index].socket_info.conn_handle),
				  (globus_byte_t *)buffer,
				  nbytes1,
				  &nbytes);

      if(result != GLOBUS_SUCCESS || nbytes != nbytes1){
	fprintf(stderr, "Consume_start_data_check_globus: failed to read"
		" remaining %d bytes of header\n", (int)nbytes1);
	return REG_FAILURE;
      }
    }
    IOTypes_table.io_def[index].buffer_bytes = REG_IO_BUFSIZE;
    IOTypes_table.io_def[index].buffer = (void *)malloc(REG_IO_BUFSIZE);

    if(!IOTypes_table.io_def[index].buffer){
      IOTypes_table.io_def[index].buffer_bytes = 0;
      fprintf(stderr, "Consume_start_data_check_globus: malloc of IO "
	      "buffer failed\n");
      return REG_FAILURE;
    }

    return REG_SUCCESS;
  }
  return REG_FAILURE;
}

/*--------------------------------------------------------------*/

int Consume_data_read_globus(const int		index,  
			     const int		datatype,
			     const size_t	num_bytes_to_read, 
			     void		*pData)
{
  globus_result_t  result;
  globus_size_t    nbytes;

#if REG_DEBUG

#ifdef USE_REG_TIMING
  double time0, time1;
#endif

  fprintf(stderr, "Consume_data_read_globus: calling globus_io_read "
          "for %d bytes\n", (int)num_bytes_to_read);

#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time0);
#endif

#endif /* REG_DEBUG */

  if(IOTypes_table.io_def[index].use_xdr ||
     IOTypes_table.io_def[index].convert_array_order == TRUE){
    result = globus_io_read(&(IOTypes_table.io_def[index].socket_info.conn_handle), 
			    IOTypes_table.io_def[index].buffer, 
			    (globus_size_t) num_bytes_to_read, 
			    (globus_size_t) num_bytes_to_read, 
			    &nbytes);
  }
  else{
    result = globus_io_read(&(IOTypes_table.io_def[index].socket_info.conn_handle), 
			    pData, 
			    (globus_size_t) num_bytes_to_read, 
			    (globus_size_t) num_bytes_to_read, 
			    &nbytes);
  }

#if REG_DEBUG
  fprintf(stderr, "Consume_data_read_globus: globus_io_read read %d bytes\n",
	  (int) nbytes);

#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time1);
  fprintf(stderr, "                          in %.3f seconds\n", 
	  (float)(time1-time0));
#endif

  if(datatype == REG_CHAR){
    fprintf(stderr, "Consume_data_read_globus: got char data:\n>>%s<<\n", 
	    (char *)pData);
  }
#endif /* REG_DEBUG */

  if(result != GLOBUS_SUCCESS){

    fprintf(stderr, "Consume_data_read_globus: error globus_io_read\n");
    Globus_error_print(result);

    /* Reset use_xdr flag set as only valid on a per-slice basis */
    IOTypes_table.io_def[index].use_xdr = FALSE;

    return REG_FAILURE;
  }

  return REG_SUCCESS;

}

/*---------------------------------------------------*/

int Emit_header_globus(const int index)
{
  char            buffer[REG_PACKET_SIZE];
  int             status;

  /* check if socket connection has been made */
  if (IOTypes_table.io_def[index].socket_info.comms_status 
      != REG_COMMS_STATUS_CONNECTED){
    Globus_attempt_listener_connect(index);
  }

  if (IOTypes_table.io_def[index].socket_info.comms_status 
      == REG_COMMS_STATUS_CONNECTED) {

#if REG_DEBUG
    fprintf(stderr, "Emit_header_globus: socket status is connected, "
	    "index = %d\n", index );
#endif

    /* Send header */
    
    sprintf(buffer, REG_PACKET_FORMAT, REG_DATA_HEADER);
    buffer[REG_PACKET_SIZE-1] = '\0';
#if REG_DEBUG
    fprintf(stderr, "Emit_header_globus: Sending >>%s<<\n", buffer);
#endif
    status = Write_globus_non_blocking(&(IOTypes_table.io_def[index].socket_info.conn_handle),
				       REG_PACKET_SIZE,
				       (void *)buffer);

    if(status == GLOBUS_SUCCESS){

#if REG_DEBUG
      fprintf(stderr, "Emit_header_globus: Sent %d bytes\n", REG_PACKET_SIZE);
#endif
      return REG_SUCCESS;
    }
    else if(status == REG_FAILURE){

#if REG_DEBUG
      fprintf(stderr, "Emit_header_globus: Write_globus failed - "
	      "immediate retry connect\n");
#endif
      Globus_retry_accept_connect(index);

      if (IOTypes_table.io_def[index].socket_info.comms_status 
	  == REG_COMMS_STATUS_CONNECTED) {  

#if REG_DEBUG
	fprintf(stderr, "Emit_header_globus: Sending >>%s<<\n", buffer);
#endif    
	if(Write_globus(&(IOTypes_table.io_def[index].socket_info.conn_handle),
			REG_PACKET_SIZE,
			(void *)buffer) == REG_SUCCESS){

	  return REG_SUCCESS;
	}	
      }
    }
#if REG_DEBUG
    else{
      fprintf(stderr, "Emit_header_globus: attempt to write to socket "
	      "timed out\n");
    }
#endif
  }
#if REG_DEBUG
  else {
    fprintf(stderr, "Emit_header_globus: socket not connected, index = %d\n",
	    index );
  }
#endif

  return REG_FAILURE;
}

/*---------------------------------------------------*/

int Emit_data_globus(const int		index,
		     const size_t	num_bytes_to_send,
		     void		*pData)
{
  if(Write_globus(&(IOTypes_table.io_def[index].socket_info.conn_handle), 
		  num_bytes_to_send,
		  (void *)pData) != REG_SUCCESS){

    fprintf(stderr, "Emit_data_globus: error globus_io_write\n");
    return REG_FAILURE;
  }

#if REG_DEBUG
  fprintf(stderr, "Emit_data_globus: sent %d bytes...\n", 
	  (int)num_bytes_to_send);
#endif
  
  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Get_communication_status_globus(const int index)
{

  if (IOTypes_table.io_def[index].socket_info.comms_status != 
      REG_COMMS_STATUS_CONNECTED)
    return REG_FAILURE;
  
  return REG_SUCCESS;

}

/*---------------------------------------------------*/

int Emit_ack_globus(const int index)
{
  char *ack_msg = "<Ready_to_read/>                                                                                                               ";
  if(index < 0 || index >= IOTypes_table.num_registered){

    return REG_FAILURE;
  }

  if (IOTypes_table.io_def[index].socket_info.comms_status != 
      REG_COMMS_STATUS_CONNECTED)
    return REG_FAILURE;

  return Write_globus(&(IOTypes_table.io_def[index].socket_info.conn_handle),
		      strlen(ack_msg),
		      (void *)ack_msg);

}

/*---------------------------------------------------*/

int Consume_ack_globus(const int index)
{
  char *ack_msg = "<Ready_to_read/>                                                                                                               ";
  char  buf[128];
  globus_size_t   nbytes;
  globus_result_t result;

  if(index < 0 || index >= IOTypes_table.num_registered){

    return REG_FAILURE;
  }

  if (IOTypes_table.io_def[index].socket_info.comms_status != 
      REG_COMMS_STATUS_CONNECTED)
    return REG_FAILURE;


  result = globus_io_try_read(&(IOTypes_table.io_def[index].socket_info.conn_handle),
			      (globus_byte_t *)buf,
			      strlen(ack_msg),
			      &nbytes);

  if(result != GLOBUS_SUCCESS){
    fprintf(stderr, "Consume_ack_globus: failed to get ack\n");
    Globus_error_print(result);
    return REG_FAILURE;
  }
  else if( nbytes != strlen(ack_msg)){
    fprintf(stderr, "Consume_ack_globus: error nbytes = %d\n", 
	    (int)nbytes);
    return REG_FAILURE;
  }

  if(!strstr(buf, ack_msg)){
    fprintf(stderr, "Consume_ack_globus: data not ack msg\n\n");
    return REG_FAILURE;
  }
  return REG_SUCCESS;
}

#endif /* REG_GLOBUS_SAMPLES */
