/*----------------------------------------------------------------------------
    This file contains routines and data structures for socket
    communication using Globus IO both for steering communication and
    for IOType communication.  Each functions are #if-ed so it is only
    included in compilation if needed for samples (REG_GLOBUS_SAMPLES=1).  

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

    Initial version by:  S Ramsden, 26.2.2003       0.1               

---------------------------------------------------------------------------*/
#include "ReG_Steer_types.h"
#include "ReG_Steer_Common.h"
#include "ReG_Steer_Appside_internal.h"
#include "ReG_Steer_Globus_io.h"
#include "ReG_Steer_Appside_Globus.h"

#ifndef REG_DEBUG
#define REG_DEBUG 0
#endif

/* Need access to these tables which are actually declared in 
   ReG_Steer_Appside_internal.h */
extern IOdef_table_type IOTypes_table;

extern Steerer_connection_table_type Steerer_connection;

/*--------------------------------------------------------------------*/

#if REG_GLOBUS_SAMPLES
int Initialize_IOType_transport_globus(const int direction,
				       const int index)
{
  int          return_status = REG_SUCCESS;
  int	       hostname_ok = 0;
  int	       port_ok = 0;
#if REG_SOAP_STEERING	  
  static int   call_count = 1;
#else
  char	      *pchar;
  int	       len;
#endif

  IOTypes_table.io_def[index].is_enabled = FALSE;

  /* set up socket_info for callback */
  if (Globus_socket_info_init(&(IOTypes_table.io_def[index].socket_info)) 
      != REG_SUCCESS) {
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
      if (Globus_create_listener(&(IOTypes_table.io_def[index].socket_info)) 
	  != REG_SUCCESS) {

	IOTypes_table.io_def[index].is_enabled = FALSE;
#if REG_DEBUG
	fprintf(stderr, "Initialize_IOType_transport_globus: failed to "
		"create listener "
		"for IOType\n");
#endif
	return_status = REG_FAILURE;
      }
      else{
	  
	IOTypes_table.io_def[index].is_enabled = TRUE;
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

      /* register connector against port */

#if REG_SOAP_STEERING	  

      /* Go out into the world of grid services... */
      if( Get_data_source_address_soap(call_count, 
		   IOTypes_table.io_def[index].socket_info.connector_hostname,
	           &(IOTypes_table.io_def[index].socket_info.connector_port)) 
	  == REG_SUCCESS){

	call_count++;
	hostname_ok = 1;
	port_ok = 1;
      }
#else
      /* get hostname and port from environment variables */

      pchar = getenv("REG_CONNECTOR_HOSTNAME");
      if (pchar) {
	len = strlen(pchar);
	if (len < REG_MAX_STRING_LENGTH) {
	  sprintf(IOTypes_table.io_def[index].socket_info.connector_hostname,
		  pchar);
	  hostname_ok = 1;
	}
	else{
	  fprintf(stderr, "Initialize_IOType_transport_globus: content of "
		  "REG_CONNECTOR_HOSTNAME exceeds max. string length of "
		  "%d chars\n", REG_MAX_STRING_LENGTH);
	}
      }

      pchar = getenv("REG_CONNECTOR_PORT");
      if (pchar) {
	IOTypes_table.io_def[index].socket_info.connector_port = atoi(pchar);
	port_ok = 1;
      }

#endif /* !REG_SOAP_STEERING */
	  
      if (port_ok && hostname_ok) {
	
	/* Don't create socket yet if this flag is set */
	if (IOTypes_table.enable_on_registration == FALSE) {
	  IOTypes_table.io_def[index].is_enabled = FALSE;
	  return REG_SUCCESS;
	}

	if (Globus_create_connector(&(IOTypes_table.io_def[index].socket_info)) 
	    != REG_SUCCESS) {

	  IOTypes_table.io_def[index].is_enabled = FALSE;
#if REG_DEBUG
	  fprintf(stderr, "Initialize_IOType_transport_globus: failed to "
		  "register connector for IOType\n");
#endif
	  return_status = REG_FAILURE;
	}
	else{

	  IOTypes_table.io_def[index].is_enabled = TRUE;
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
#endif

/*---------------------------------------------------*/

#if REG_GLOBUS_SAMPLES
void Finalize_IOType_transport_globus()
{
  int index;

  for (index=0; index<IOTypes_table.num_registered; index++) {
    if (IOTypes_table.io_def[index].direction == REG_IO_OUT) {
      /* close globus sockets */
      Globus_cleanup_listener_connection(&(IOTypes_table.io_def[index].socket_info));
      Globus_socket_info_cleanup(&(IOTypes_table.io_def[index].socket_info));
    }
    else if (IOTypes_table.io_def[index].direction == REG_IO_IN) {
      /* close globus sockets */
      Globus_cleanup_connector_connection(&(IOTypes_table.io_def[index].socket_info));
      Globus_socket_info_cleanup(&(IOTypes_table.io_def[index].socket_info));
    }
  }
  /* deactivate globus module */
  Globus_io_deactivate();
}
#endif

/*---------------------------------------------------*/

#if REG_GLOBUS_SAMPLES
int Disable_IOType_globus(int index)
{
  /* check index is valid */
  if(index < 0 || index >= IOTypes_table.num_registered){

    fprintf(stderr, "Disable_IOType_globus: index out of range\n");
    return REG_FAILURE;
  }

  if (IOTypes_table.io_def[index].direction == REG_IO_OUT) {
    /* close globus sockets */
    Globus_cleanup_listener_connection(&(IOTypes_table.io_def[index].socket_info));

    while(IOTypes_table.io_def[index].socket_info.comms_status != REG_COMMS_STATUS_NULL
	  && IOTypes_table.io_def[index].socket_info.comms_status != REG_COMMS_STATUS_FAILURE){
      Globus_callback_poll(&(IOTypes_table.io_def[index].socket_info));
    }
  }
  else if (IOTypes_table.io_def[index].direction == REG_IO_IN) {
    /* close globus sockets */
    Globus_cleanup_connector_connection(&(IOTypes_table.io_def[index].socket_info));

    while(IOTypes_table.io_def[index].socket_info.comms_status != REG_COMMS_STATUS_NULL
	  && IOTypes_table.io_def[index].socket_info.comms_status != REG_COMMS_STATUS_FAILURE){
      Globus_callback_poll(&(IOTypes_table.io_def[index].socket_info));
    }
  }

  return REG_SUCCESS;
}
#endif

/*---------------------------------------------------*/

#if REG_GLOBUS_SAMPLES
int Enable_IOType_globus(int index)
{
  /* check index is valid */
  if(index < 0 || index >= IOTypes_table.num_registered)return REG_FAILURE;

  if(IOTypes_table.io_def[index].direction == REG_IO_OUT){

    /* open socket and register callback function to listen for and
       accept connections */
    if (Globus_create_listener(&(IOTypes_table.io_def[index].socket_info)) 
	!= REG_SUCCESS) {
#if REG_DEBUG
      fprintf(stderr, "Enable_IOType_globus: failed to create listener "
	      "for IOType\n");
#endif
      return REG_FAILURE;
    }
  }
  else if (IOTypes_table.io_def[index].direction == REG_IO_IN){

    if (Globus_create_connector(&(IOTypes_table.io_def[index].socket_info)) 
	!= REG_SUCCESS) {
#if REG_DEBUG
      fprintf(stderr, "Enable_IOType_globus: failed to "
	      "register connector for IOType\n");
#endif
      return REG_FAILURE;
    }
  }

  return REG_SUCCESS;
}
#endif

/*---------------------------------------------------*/

#if REG_GLOBUS_SAMPLES
int Consume_start_data_check_globus(const int index)
{  
  char            buffer[REG_PACKET_SIZE];
  char           *pstart;
  globus_size_t   nbytes, nbytes1;
  globus_result_t result;
  int             attempt_reconnect;

  /* if not connected attempt to connect now */
  if (IOTypes_table.io_def[index].socket_info.comms_status 
      != REG_COMMS_STATUS_CONNECTED){
    Globus_attempt_connector_connect(&(IOTypes_table.io_def[index].socket_info));
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

      if( globus_object_type_match(globus_object_get_type(globus_error_get(result)),
				   GLOBUS_IO_ERROR_TYPE_EOF) ){
	fprintf(stderr, "\nConsume_start_data_check_globus: hit EOF\n");	
	return REG_FAILURE;
      }

      fprintf(stderr, "\nConsume_start_data_check_globus: try read failed\n");
      if(!attempt_reconnect){
	return REG_FAILURE;
      }

#if REG_DEBUG
      fprintf(stderr, "\nConsume_start_data_check_globus: globus_io_try_read"
	      " failed - try immediate reconnect for index %d\n", index);
#endif
      Globus_error_print(result);

      Globus_retry_connect(&(IOTypes_table.io_def[index].socket_info));
     
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
#endif

/*--------------------------------------------------------------*/

#if REG_GLOBUS_SAMPLES
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
#endif

/*---------------------------------------------------*/

#if REG_GLOBUS_SAMPLES
int Emit_header_globus(const int index)
{
  char            buffer[REG_PACKET_SIZE];
  int             status;

  /* check if socket connection has been made */
  if (IOTypes_table.io_def[index].socket_info.comms_status 
      != REG_COMMS_STATUS_CONNECTED) 
    Globus_attempt_listener_connect(&(IOTypes_table.io_def[index].socket_info));
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
    status = Write_globus(&(IOTypes_table.io_def[index].socket_info.conn_handle),
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
      Globus_retry_accept_connect(&(IOTypes_table.io_def[index].socket_info));

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
#endif

/*---------------------------------------------------*/

#if REG_GLOBUS_SAMPLES
int Emit_footer_globus(const int index,
 		       const char * const buffer)
{
#if REG_DEBUG
  fprintf(stderr, "Emit_footer_globus: Sending >>%s<<\n", buffer);
#endif

  if(Write_globus(&(IOTypes_table.io_def[index].socket_info.conn_handle),
		  strlen(buffer)+1,
		  (void *)buffer) != REG_SUCCESS){

    fprintf(stderr, "Emit_footer_globus: call to globus_io_write failed\n");
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}
#endif

/*---------------------------------------------------*/

#if REG_GLOBUS_SAMPLES
int Emit_data_globus(const int		index,
		     const int		datatype,
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
#endif

/*---------------------------------------------------*/

#if REG_GLOBUS_SAMPLES
int Get_communication_status_globus(const int index)
{

  if (IOTypes_table.io_def[index].socket_info.comms_status != 
      REG_COMMS_STATUS_CONNECTED)
    return REG_FAILURE;
  
  return REG_SUCCESS;

}
#endif

/*---------------------------------------------------*/

