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
#include "ReG_Steer_Appside_Sockets.h"
#include <string.h>

#ifndef REG_DEBUG
#define REG_DEBUG 0
#endif

/* Need access to these tables which are actually declared in 
   ReG_Steer_Appside_internal.h */
extern IOdef_table_type IOTypes_table;

extern Steerer_connection_table_type Steerer_connection;

/*--------------------------------------------------------------------*/

int Sockets_socket_info_init(const int index) {

  char* pchar;
  int min = 0;
  int max = 0;

  if(pchar = getenv("GLOBUS_TCP_PORT_RANGE")) {
    if(sscanf(pchar, "%d,%d", &(min), &(max)) != 2){
      min = 0;
      max = 0;
    }
  }

  IOTypes_table.io_def[index].socket_info.min_port = min;
  IOTypes_table.io_def[index].socket_info.max_port = max;
  IOTypes_table.io_def[index].socket_info.listener_port = 0;
  IOTypes_table.io_def[index].socket_info.listener_status = REG_COMMS_STATUS_NULL;
  IOTypes_table.io_def[index].socket_info.comms_status = REG_COMMS_STATUS_NULL;   
  IOTypes_table.io_def[index].socket_info.connector_port = 0;
  sprintf(IOTypes_table.io_def[index].socket_info.connector_hostname, " ");

  return REG_SUCCESS;
}

/*--------------------------------------------------------------------*/

void Sockets_socket_info_cleanup(const int index) {

}

/*--------------------------------------------------------------------*/

int Sockets_initialize_IOType_transport(const int direction, const int index) {

  int return_status = REG_SUCCESS;
  static int call_count = 1;

  /* set up socket info stuff */
  if(Sockets_socket_info_init(index) != REG_SUCCESS) {
#if REG_DEBUG
    fprintf(stderr, "Sockets_initialize_IOType_transport: failed to init socket info for IOType\n");
#endif
    return_status = REG_FAILURE;
  }
  else {
    if(direction == REG_IO_OUT) {

      /* Don't create socket yet if this flag is set */
      if (IOTypes_table.enable_on_registration == FALSE) return REG_SUCCESS;

      /* open socket and register callback function to listen for and
	 accept connections */
      if (Sockets_create_listener(index) != REG_SUCCESS){
#if REG_DEBUG
	fprintf(stderr, "Sockets_initialize_IOType_transport: failed to create listener for IOType\n");
#endif
	return_status = REG_FAILURE;
      }
#if REG_DEBUG
      else {
	fprintf(stderr, "Sockets_initialize_IOType_transport: Created listener on port %d, index %d, label %s\n", 
		IOTypes_table.io_def[index].socket_info.listener_port, 
		index, IOTypes_table.io_def[index].label );
      }
#endif
    }
    else if(direction == REG_IO_IN) {

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

	if (Sockets_create_connector(index) != REG_SUCCESS) {
#if REG_DEBUG
	  fprintf(stderr, "Sockets_initialize_IOType_transport: failed to register connector for IOType\n");
#endif
	  return_status = REG_FAILURE;
	}
#if REG_DEBUG
	else {
	  fprintf(stderr, "Sockets_initialize_IOType_transport: registered connector on port %d, hostname = %s, index %d, label %s\n", 
		  IOTypes_table.io_def[index].socket_info.connector_port,
		  IOTypes_table.io_def[index].socket_info.connector_hostname,
		  index, IOTypes_table.io_def[index].label );
	}
#endif
      }
      else {
	fprintf(stderr, "Sockets_initialize_IOType_transport: cannot create connector as port and hostname not set\n");
      }
    }
  }

  return return_status;
}

/*--------------------------------------------------------------------*/

int Sockets_create_listener(const int index) {

  char* pchar;
  char* ip_addr;

  int listener;
  int yes = 1;
  int i;
  struct sockaddr_in myAddr;

  /* create and register listener */
  listener = socket(AF_INET, SOCK_STREAM, 0);
  if(listener == REG_SOCKETS_ERROR) {
    perror("socket");
    return REG_FAILURE;
  }
  IOTypes_table.io_def[index].socket_info.listener_handle = listener;
  IOTypes_table.io_def[index].socket_info.listener_status = REG_COMMS_STATUS_LISTENING;

  /* Get the hostname of the machine we're running on (so we can publish
   * the endpoint of this connection). If REG_IO_ADDRESS is set then
   * that's what we use.  If not, call Get_fully_qualified_hostname which 
   * itself uses  REG_TCP_INTERFACE if set. */
  if(pchar = getenv("REG_IO_ADDRESS")){
    strcpy(IOTypes_table.io_def[index].socket_info.listener_hostname, pchar);
  }
  else if(Get_fully_qualified_hostname(&pchar, &ip_addr) == REG_SUCCESS){
    strcpy(IOTypes_table.io_def[index].socket_info.listener_hostname, pchar);
  }
  else{
    fprintf(stderr, "Sockets_create_listener: WARNING: failed to get hostname\n");
    sprintf(IOTypes_table.io_def[index].socket_info.listener_hostname, "NOT_SET");
  }

  /* Turn off the "Address already in use" error message */
  if(setsockopt(listener, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int)) == REG_SOCKETS_ERROR) {
    perror("setsockopt");
    return REG_FAILURE;
  }

  /* set up server address */
  myAddr.sin_family = AF_INET;
  inet_aton(IOTypes_table.io_def[index].socket_info.listener_hostname, &(myAddr.sin_addr));
  memset(&(myAddr.sin_zero), '\0', 8); /* zero the rest */

  /* Now bind listener so we can accept connections when they happen */
  if(IOTypes_table.io_def[index].socket_info.min_port == 0) {
    /* Just bind to REG_SOCKETS_PORT if the globs port range isn't set */
    i = REG_SOCKETS_PORT;
  } else {
    /* otherwise try the ports in that range */
    i = IOTypes_table.io_def[index].socket_info.min_port;
  }
  myAddr.sin_port = htons((short) i);

  while(bind(listener, (struct sockaddr*) &myAddr, sizeof(struct sockaddr)) == REG_SOCKETS_ERROR) {
    if(++i > IOTypes_table.io_def[index].socket_info.max_port) {
      perror("bind");
      close(listener);
      return REG_FAILURE;
    }
    myAddr.sin_port = htons((short) i);
  }
  /* we're bound, so save the port number we're using */
  IOTypes_table.io_def[index].socket_info.listener_port = i;

  IOTypes_table.io_def[index].socket_info.comms_status=REG_COMMS_STATUS_LISTENING;

  return REG_SUCCESS;
}

/*--------------------------------------------------------------------*/

int Sockets_create_connector(const int index) {

  int connector;
  struct sockaddr_in myAddr;

  if (IOTypes_table.io_def[index].socket_info.connector_port == 0) return REG_FAILURE;

  /* create connector*/
  connector = socket(AF_INET, SOCK_STREAM, 0);
  if(connector == REG_SOCKETS_ERROR) {
    /* problem! */
    fprintf(stderr, "Sockets_create_connector: Error registering connect\n");
    fprintf(stderr, "                          hostname = %s\n", 
	    IOTypes_table.io_def[index].socket_info.connector_hostname);
    fprintf(stderr, "                          port = %d\n", 
	    (int)IOTypes_table.io_def[index].socket_info.connector_port);
    perror("socket");
    IOTypes_table.io_def[index].socket_info.comms_status=REG_COMMS_STATUS_FAILURE;
    return REG_FAILURE;
  }

  /* all okay, so save connector handle... */
  IOTypes_table.io_def[index].socket_info.connector_handle = connector;
  IOTypes_table.io_def[index].socket_info.comms_status=REG_COMMS_STATUS_WAITING_TO_CONNECT;

  /* ...turn off the "Address already in use" error message... */
  if(setsockopt(listener, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int)) == REG_SOCKETS_ERROR) {
    perror("setsockopt");
    return REG_FAILURE;
  }

  /* ...build address struct... */
  myAddr.sin_family = AF_INET;
  myAddr.sin_port = htons(IOTypes_table.io_def[index].socket_info.connector_port);
  inet_aton(IOTypes_table.io_def[index].socket_info.connector_hostname, &(myAddr.sin_addr));
  memset(&(myAddr.sin_zero), '\0', 8); /* zero the rest */

  /* ...and bind */
  if(bind(connector, (struct sockaddr*) &myAddr, sizeof(struct sockaddr)) == REG_SOCKETS_ERROR) {
    perror("bind");
    close(listener);
    return REG_FAILURE;
  }

  fprintf(stderr, "Sockets_create_connector: registered connector on connector_port = %d, connector_hostname = %s\n", 
	  IOTypes_table.io_def[index].socket_info.connector_port, 
	  IOTypes_table.io_def[index].socket_info.connector_hostname);
  
  return REG_SUCCESS;
}

/*--------------------------------------------------------------------*/

void Sockets_cleanup_listener_connection(const int index) {
  if (IOTypes_table.io_def[index].socket_info.listener_status == REG_COMMS_STATUS_LISTENING){
    Sockets_close_listener_handle(index);
  }

  if (IOTypes_table.io_def[index].socket_info.comms_status == REG_COMMS_STATUS_CONNECTED || 
      IOTypes_table.io_def[index].socket_info.comms_status == REG_COMMS_STATUS_WAITING_FOR_ACCEPT){
    Sockets_close_connector_handle(index);
  }

  /* Flag that this listener is dead - used in Emit_IOType_defs */
  sprintf(IOTypes_table.io_def[index].socket_info.listener_hostname, "NOT_SET");
  IOTypes_table.io_def[index].socket_info.listener_port = 0;
}

/*--------------------------------------------------------------------*/

void Sockets_cleanup_connector_connection(const int index) {
  if (IOTypes_table.io_def[index].socket_info.comms_status == REG_COMMS_STATUS_CONNECTED ||
      IOTypes_table.io_def[index].socket_info.comms_status == REG_COMMS_STATUS_WAITING_TO_CONNECT ){
    Sockets_close_connector_handle(index);
  }
}

/*--------------------------------------------------------------------*/

void Sockets_close_listener_handle(const int index) {
  if(close(IOTypes_table.io_def[index].socket_info.listener_handle) == REG_SOCKETS_ERROR) {
    perror("close");
    IOTypes_table.io_def[index].socket_info.listener_status = REG_COMMS_STATUS_FAILURE;
  }
  else {
#if REG_DEBUG
    fprintf(stderr, "DBG: Sockets_close_listener_handle: close OK\n");
#endif
    IOTypes_table.io_def[index].socket_info.listener_status = REG_COMMS_STATUS_CLOSING;
  }

}

/*--------------------------------------------------------------------*/

void Sockets_close_connector_handle(const int index) {
  if(close(IOTypes_table.io_def[index].socket_info.connector_handle) == REG_SOCKETS_ERROR) {
    perror("close");
    IOTypes_table.io_def[index].socket_info.comms_status = REG_COMMS_STATUS_FAILURE;
  }
  else {
#if REG_DEBUG
    fprintf(stderr, "DBG: Sockets_close_connector_handle: close OK\n");
#endif
    IOTypes_table.io_def[index].socket_info.comms_status = REG_COMMS_STATUS_CLOSING;
  }

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

/*--------------------------------------------------------------------*/

int Consume_start_data_check_socks(const int index)
{

}

#endif
