/*----------------------------------------------------------------------------
    This file contains routines and data structures for socket
    communication using Globus IO both for steering communication and
    for IOType communication.  Each functions are #if-ed so it is only
    included in compilation if needed for steering
    (REG_GLOBUS_STEERING=1) or for samples (REG_GLOBUS_SAMPLES=1).  

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

#include "ReG_Steer_Appside_Globus.h"

#if REG_GLOBUS_STEERING
int Send_status_msg_globus(char *buf)
{
  char            hdr_buf[REG_MAX_MSG_SIZE];
  globus_size_t   nbytes;
  globus_result_t result;

  if(Steerer_connection.socket_info.comms_status != 
     REG_COMMS_STATUS_CONNECTED) {
    Globus_attempt_listener_connect(&(Steerer_connection.socket_info));
  }

  if (Steerer_connection.socket_info.comms_status != 
      REG_COMMS_STATUS_CONNECTED) return REG_FAILURE;

  /* Send message header */
  sprintf(hdr_buf, REG_PACKET_FORMAT, REG_DATA_HEADER);

  result = globus_io_write(&(Steerer_connection.socket_info.conn_handle), 
			   (globus_byte_t *)hdr_buf, 
			   strlen(hdr_buf), 
			   &nbytes);

  if(result != GLOBUS_SUCCESS){

    Globus_error_print(result);

    /* Try again in case steerer has dropped connection */

    Globus_retry_accept_connect(&(Steerer_connection.socket_info));

    if (Steerer_connection.socket_info.comms_status 
	  != REG_COMMS_STATUS_CONNECTED) {
      return REG_FAILURE;
    }

    result = globus_io_write(&(Steerer_connection.socket_info.conn_handle), 
			     (globus_byte_t *)hdr_buf, 
			     strlen(hdr_buf), 
			     &nbytes);

    if(result != GLOBUS_SUCCESS){

#if DEBUG
      fprintf(stderr, "Send_status_msg_globus: globus_io_write "
	      "failed\n");
#endif
      Globus_error_print(result);
      return REG_FAILURE;
    }	
  }

  if( Emit_msg_header(&(Steerer_connection.socket_info),
		      REG_CHAR,
		      strlen(buf)) != REG_SUCCESS){

#if DEBUG
    fprintf(stderr, "Send_status_msg_globus: failed to send header\n");
#endif
    return REG_FAILURE;
  }

  /* Send message proper */

  result = globus_io_write(&(Steerer_connection.socket_info.conn_handle), 
			   (globus_byte_t *)buf, 
			   strlen(buf), 
			   &nbytes);

  if(result != GLOBUS_SUCCESS){

#if DEBUG
    fprintf(stderr, "Send_status_msg_globus: failed to send message\n");
#endif
    Globus_error_print(result);

    return REG_FAILURE;
  }


  /* and finally, the footer of the message */

  /* Send message header */
  sprintf(hdr_buf, REG_PACKET_FORMAT, REG_DATA_FOOTER);

  result = globus_io_write(&(Steerer_connection.socket_info.conn_handle), 
			   (globus_byte_t *)hdr_buf, 
			   strlen(hdr_buf), 
			   &nbytes);

  if(result != GLOBUS_SUCCESS){

#if DEBUG
    fprintf(stderr, "Send_status_msg_globus: failed to send footer\n");
#endif
    Globus_error_print(result);

    return REG_FAILURE;
  }

  return REG_SUCCESS;
}
#endif

/*-------------------------------------------------------------------*/

#if REG_GLOBUS_STEERING
struct msg_struct *Get_control_msg_globus()
{
  struct msg_struct *msg = NULL;
  globus_size_t   nbytes;
  globus_result_t result;
  int type;
  int count;

  if(Steerer_connection.socket_info.comms_status != REG_COMMS_STATUS_CONNECTED) {
    Globus_attempt_listener_connect(&(Steerer_connection.socket_info));
  }

  if (Steerer_connection.socket_info.comms_status != REG_COMMS_STATUS_CONNECTED){
    return NULL;
  }

  /* Check for data on socket - non-blocking */

  result = globus_io_try_read(&(Steerer_connection.socket_info.conn_handle),
			      (globus_byte_t *)Steerer_connection.msg_buffer,
			      REG_PACKET_SIZE,
			      &nbytes);

  if (result != GLOBUS_SUCCESS){
#if DEBUG
    fprintf(stderr, "Get_control_msg_globus: globus_io_try_read failed - "
	    "try immediate reconnect\n");
#endif
    Globus_error_print(result);

    Globus_retry_connect(&(Steerer_connection.socket_info));
     
    /* check if socket reconnection has been made and check for 
       data if it has */
    if (Steerer_connection.socket_info.comms_status != REG_COMMS_STATUS_CONNECTED) {

#if DEBUG
      fprintf(stderr, "Get_control_msg_globus: reconnect failed\n");
#endif
      return NULL;
    }

    result = globus_io_try_read(&(Steerer_connection.socket_info.conn_handle),
				(globus_byte_t *)Steerer_connection.msg_buffer,
				REG_PACKET_SIZE,
				&nbytes);

    if (result != GLOBUS_SUCCESS){

#if DEBUG
      fprintf(stderr, "Get_control_msg_globus: reconnect OK but "
	      "globus_io_try_read failed\n");
#endif
      Globus_error_print(result);

      return NULL;
    }
  }

#if DEBUG
  fprintf(stderr, "Get_control_msg_globus: read <%s> from socket\n", 
	  Steerer_connection.msg_buffer);
#endif

  /* ARPDBG - globus_io_try_read always returns 0 bytes if connection
     configugured to use GSSAPI or SSL data wrapping. */
  if(nbytes == 0) return NULL;

  if(strncmp(Steerer_connection.msg_buffer, REG_DATA_HEADER, 
	     strlen(REG_DATA_HEADER))){

#if DEBUG
    fprintf(stderr, "Get_control_msg_globus: unrecognised msg >>%s<<\n", 
	    Steerer_connection.msg_buffer);
#endif    
    return NULL;
  }

  if( Consume_msg_header(&(Steerer_connection.socket_info),
			 &type,
			 &count) != REG_SUCCESS){

    return NULL;
  }

  if(type != REG_CHAR){

#if DEBUG
    fprintf(stderr, "Get_control_msg_globus: error, wrong data type "
	    "returned by Consume_msg_header\n");
#endif
    return NULL;
  }

  if(count > REG_MAX_MSG_SIZE){

#if DEBUG
    fprintf(stderr, "Get_control_msg_globus: error, control msg (%d "
	    "chars) exceeds max. length of %d chars\n", count, 
	    REG_MAX_MSG_SIZE);
#endif
    return NULL;
  }

  /* Blocks until count bytes received */
  result = globus_io_read(&(Steerer_connection.socket_info.conn_handle), 
			  (globus_byte_t *)Steerer_connection.msg_buffer, 
			  count, 
			  count, 
			  &nbytes);

  if(result != GLOBUS_SUCCESS){

#if DEBUG
    fprintf(stderr, "Get_control_msg_globus: globus_io_read failed\n");
#endif
    Globus_error_print(result);
    
    return NULL;
  }

#if DEBUG
    fprintf(stderr, "Get_control_msg_globus: globus_io_read got:\n>>%s<<\n",
	    Steerer_connection.msg_buffer);
#endif

  if(count != nbytes){

#if DEBUG
    fprintf(stderr, "Get_control_msg_globus: globus_io_read failed to read"
	    " requested no. of bytes\n");
#endif
    return NULL;    
  }

  if( Consume_msg_header(&(Steerer_connection.socket_info),
			 &type,
			 &count) != REG_EOD){

#if DEBUG
    fprintf(stderr, "Get_control_msg_globus: failed to get message footer\n");
#endif
    return NULL;
  }

  msg = New_msg_struct();

  if(Parse_xml_buf(Steerer_connection.msg_buffer, 
		   strlen(Steerer_connection.msg_buffer), msg) != REG_SUCCESS){

    fprintf(stderr, "Get_control_msg_globus: failed to parse buffer "
	    ">>%s<<\n", Steerer_connection.msg_buffer);
    Delete_msg_struct(msg);
    msg = NULL;
  }

  return msg;
}
#endif

/*-------------------------------------------------------------------*/

#if REG_GLOBUS_STEERING
int Initialize_steering_connection_globus(int  NumSupportedCmds,
					  int *SupportedCmds)
{
  char *pchar;
  int   return_status = REG_SUCCESS;
  int   tmp_port;

  pchar = getenv("REG_STEER_APP_PORT");

  if(!pchar){

    fprintf(stderr, "Initialize_steering_connection_globus: failed to "
	    "get port to listen on\n");
    return REG_FAILURE;
  }

  if(sscanf(pchar, "%d", &tmp_port) != 1){

    Steerer_connection.socket_info.listener_port = REG_PORT_NOTSET;
    return REG_FAILURE;
  }

  Steerer_connection.socket_info.listener_port = (unsigned short int)tmp_port;

  /* Set-up socket_info for callback */
  if (Globus_socket_info_init(&(Steerer_connection.socket_info)) != REG_SUCCESS){
#if DEBUG
    fprintf(stderr, "Initialize_steering_connection_globus: failed to "
	    "initialise socket info\n");
#endif
    return_status = REG_FAILURE;
  }
  else {

    if(Globus_create_listener(&(Steerer_connection.socket_info)) != REG_SUCCESS){

#if DEBUG
      fprintf(stderr, "Initialize_steering_connection_globus: failed to "
	      "create listener for IOType\n");
#endif
      return_status = REG_FAILURE;
    }
    else{

#if DEBUG
      fprintf(stderr, "Initialize_steering_connection_globus: Created "
	      "listener on port %d\n", 
	      Steerer_connection.socket_info.connector_port);
#endif
      /* attempt to kick the callback function (in case accept callback) */
      Globus_callback_poll(&(Steerer_connection.socket_info));
      
      /* Create & store the msg that we will need to send to steerer when
	 it first connects */
      Make_supp_cmds_msg(NumSupportedCmds, SupportedCmds, 
			 Steerer_connection.supp_cmds);
    }
  }

  return return_status;
}
#endif

/*-------------------------------------------------------------------*/

#if REG_GLOBUS_STEERING
int Steerer_connected_globus()
{
  globus_size_t   nbytes;
  globus_result_t result;

  if(Steerer_connection.socket_info.comms_status != 
     REG_COMMS_STATUS_CONNECTED) {
    Globus_attempt_listener_connect(&(Steerer_connection.socket_info));
  }

  if (Steerer_connection.socket_info.comms_status == 
      REG_COMMS_STATUS_CONNECTED) {

    /* In contrast to file-based approach, this is where we have to tell
       steerer about supported commands */
    result = globus_io_write(&(Steerer_connection.socket_info.conn_handle), 
			     (globus_byte_t *)Steerer_connection.supp_cmds, 
			     strlen(Steerer_connection.supp_cmds), 
			     &nbytes);

    if(result == GLOBUS_SUCCESS){

      return REG_SUCCESS;
    }
    else{
      Globus_error_print(result);

      /* Try again in case steerer has dropped connection */
#if DEBUG
      fprintf(stderr, "Steerer_connected_globus: globus_io_write failed "
	      "- immediate retry connect\n");
#endif

      Globus_retry_accept_connect(&(Steerer_connection.socket_info));

      if (Steerer_connection.socket_info.comms_status 
	  == REG_COMMS_STATUS_CONNECTED) {  

#if DEBUG
	fprintf(stderr, "Steerer_connected_globus: Sending >>%s<<\n", 
		Steerer_connection.supp_cmds);
#endif    
	result = globus_io_write(&(Steerer_connection.socket_info.conn_handle), 
				 (globus_byte_t *)Steerer_connection.supp_cmds,
				 strlen(Steerer_connection.supp_cmds), 
				 &nbytes);
	
	if(result != GLOBUS_SUCCESS){

#if DEBUG
	  fprintf(stderr, "Steerer_connected_globus: globus_io_write "
		  "failed\n");
#endif
	  Globus_error_print(result);
	  return REG_FAILURE;
	}	
      }
    }

    return REG_SUCCESS;
  }
  else{

    return REG_FAILURE;
  }
}
#endif

/*-------------------------------------------------------------------*/

#if REG_GLOBUS_STEERING
int Finalize_steering_connection_globus()
{
  Globus_cleanup_listener_connection(&(Steerer_connection.socket_info));
  return REG_SUCCESS;
}
#endif 

/*--------------------------------------------------------------------*/

#if REG_GLOBUS_SAMPLES
int Initialize_IOType_transport_globus(const int direction,
				       const int index)
{
  int          return_status = REG_SUCCESS;
  int	       hostname_ok = 0;
  int	       port_ok = 0;
  char	      *pchar;
  int	       len;

    /* set up socket_info for callback */
    if (Globus_socket_info_init(&(IOTypes_table.io_def[index].socket_info)) != REG_SUCCESS) {
#if DEBUG
	fprintf(stderr, "Initialize_IOType_transport_globus: failed to initialise socket info for IOType\n");
#endif
	return_status = REG_FAILURE;
    } 
    else {
      
      if (direction == REG_IO_OUT)
      {
	/* open socket and register callback function to listen for and
	   accept connections */
	if (Globus_create_listener(&(IOTypes_table.io_def[index].socket_info)) != REG_SUCCESS) {
#if DEBUG
	  fprintf(stderr, "Initialize_IOType_transport_globus: failed to create listener "
		  "for IOType\n");
#endif
	  return_status = REG_FAILURE;
	}
	else{
	  
#if DEBUG
	  fprintf(stderr, "Initialize_IOType_transport_globus: Created listener on port %d, "
		  "index %d, label %s\n", 
		  IOTypes_table.io_def[index].socket_info.listener_port, 
		  index, IOTypes_table.io_def[index].label );
#endif
	  /* attempt to kick the callback function (in case accept callback) */
	  Globus_callback_poll(&(IOTypes_table.io_def[index].socket_info));
	}
	
      }
      else if (direction == REG_IO_IN)
	{
	  /* register connector against port */
	  /* get hostname and port from environment variables */
	  
	  pchar = getenv("REG_CONNECTOR_HOSTNAME");
	  if (pchar) {
	    len = strlen(pchar);
	    if (len < REG_MAX_STRING_LENGTH) {
	      sprintf(IOTypes_table.io_def[index].socket_info.connector_hostname,
		      pchar);
	      hostname_ok = 1;
	    }
	  }
	  /* SMR XXX add error handling */
	  pchar = getenv("REG_CONNECTOR_PORT");
	  if (pchar) {
	    IOTypes_table.io_def[index].socket_info.connector_port = atoi(pchar);
	    port_ok = 1;
	  }
	  
	  if (port_ok && hostname_ok) {
	    
	    if (Globus_create_connector(&(IOTypes_table.io_def[index].socket_info)) != REG_SUCCESS) {
#if DEBUG
	      fprintf(stderr, "Initialize_IOType_transport_globus: failed to register connector "
		      "for IOType\n");
#endif
	      return_status = REG_FAILURE;
	    }
	    else{

#if DEBUG
	      fprintf(stderr, "Initialize_IOType_transport_globus: registered connector on "
		      "port %d, hostname = %s, index %d, label %s\n", 
		      IOTypes_table.io_def[index].socket_info.connector_port,
		      IOTypes_table.io_def[index].socket_info.connector_hostname,
		      index, IOTypes_table.io_def[index].label );
#endif
	      
	    }
	  }
	  else
	fprintf(stderr, "Initialize_IOType_transport_globus: cannot create connector as "
		"port and hostname not set\n");
	}
    }

    return return_status;

}
#endif

/*---------------------------------------------------*/

#if REG_GLOBUS_SAMPLES
void Finalize_IOType_transport_globus()
{
  int	index;

  for (index=0; index<IOTypes_table.num_registered; index++) {
    if (IOTypes_table.io_def[index].direction == REG_IO_OUT) {
      /* close globus sockets */
      Globus_cleanup_listener_connection(&(IOTypes_table.io_def[index].socket_info));
    }
    else if (IOTypes_table.io_def[index].direction == REG_IO_IN) {
      /* close globus sockets */
      Globus_cleanup_connector_connection(&(IOTypes_table.io_def[index].socket_info));
    }
  }
  /* deactivate globus module */
  Globus_io_deactivate();
  
}
#endif

/*---------------------------------------------------*/

#if REG_GLOBUS_SAMPLES
int Consume_start_data_check_globus(const int index)
{  
  char buffer[REG_PACKET_SIZE];
  globus_size_t   nbytes;
  globus_result_t result;


  /* if not connected attempt to connect now */
  if (IOTypes_table.io_def[index].socket_info.comms_status 
      != REG_COMMS_STATUS_CONNECTED)
    Globus_attempt_connector_connect(&(IOTypes_table.io_def[index].socket_info));


  /* check if socket connection has been made */
  if (IOTypes_table.io_def[index].socket_info.comms_status == 
      REG_COMMS_STATUS_CONNECTED) {

#if DEBUG
    fprintf(stderr, "Consume_start_data_check_globus: socket status is connected, index = %d\n",
	    index );
#endif

    /* Check for data on socket - non-blocking */

    result = globus_io_try_read(&(IOTypes_table.io_def[index].socket_info.conn_handle),
				(globus_byte_t *)buffer,
				REG_PACKET_SIZE,
				&nbytes);

    if (result != GLOBUS_SUCCESS){
#if DEBUG
      fprintf(stderr, "Consume_start_data_check_globus: globus_io_try_read failed - "
	      "try immediate reconnect for index %d\n", index);
#endif
      Globus_error_print(result);

      Globus_retry_connect(&(IOTypes_table.io_def[index].socket_info));
     
      /* check if socket reconnection has been made and check for data if it has */
      if (IOTypes_table.io_def[index].socket_info.comms_status == 
	  REG_COMMS_STATUS_CONNECTED) {
	result = globus_io_try_read(&(IOTypes_table.io_def[index].socket_info.conn_handle),
				    (globus_byte_t *)buffer,
				    REG_PACKET_SIZE,
				    &nbytes);
      }
      else {
#if DEBUG
      fprintf(stderr, "Consume_start_data_check_globus: reconnect attempt failed - "
	      "socket is not connected status id %d\n", 
	      IOTypes_table.io_def[index].socket_info.comms_status);
#endif
      return REG_FAILURE;

      }

    }

    if(result == GLOBUS_SUCCESS){

#if DEBUG
      fprintf(stderr, "Consume_start_data_check_globus: read <%s> from socket\n", buffer);
#endif
      /* ARPDBG - globus_io_try_read always returns 0 bytes if connection
	 configugured to use GSSAPI or SSL data wrapping. */
      if(nbytes > 0){

	if(!strncmp(buffer, REG_DATA_HEADER, strlen(REG_DATA_HEADER))){

	  IOTypes_table.io_def[index].buffer_bytes = REG_IO_BUFSIZE;
	  IOTypes_table.io_def[index].buffer =
	                                     (void *)malloc(REG_IO_BUFSIZE);

	  if(!IOTypes_table.io_def[index].buffer) return REG_FAILURE;

	  return REG_SUCCESS;
	}
      }   
    }
    else {
    
#if DEBUG
      fprintf(stderr, "Consume_start_data_check_globus: reconnect attempt success - but "
	      "globus_io_try_read failed\n");
#endif
      Globus_error_print(result);

    }

    return REG_FAILURE;
  }
  else {
#if DEBUG
    fprintf(stderr, "Consume_start_data_check_globus: socket is NOT connected, index = "
	    "%d\n",index );
#endif
  }
}
#endif

/*---------------------------------------------------*/

#if REG_GLOBUS_SAMPLES
int Consume_data_read_globus(const int		index,  
			     const int		datatype,
			     const size_t	num_bytes_to_read, 
			     void		*pData)
{
  globus_result_t  result;
  globus_size_t    nbytes;

#if DEBUG
  float   read_time;
  clock_t start_time, stop_time;
#endif

#if DEBUG
  fprintf(stderr, "Consume_data_read_globus: calling globus_io_read for %d bytes\n",
	  num_bytes_to_read);

  start_time = clock();
#endif


  if(IOTypes_table.io_def[index].use_xdr){
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

#if DEBUG
  stop_time = clock();
  read_time = (float)(start_time - stop_time)/(float)CLOCKS_PER_SEC;

  fprintf(stderr, "Consume_data_read_globus: globus_io_read read %d bytes\n",
	  nbytes);
  fprintf(stderr, "                    in %.3f seconds\n", read_time);

  if(datatype == REG_CHAR){
    fprintf(stderr, "Consume_data_read_globus: got char data:\n>>%s<<\n", 
	    (char *)pData);
  }
#endif

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
  globus_size_t   nbytes;
  globus_result_t result;

  /* check if socket connection has been made */
  if (IOTypes_table.io_def[index].socket_info.comms_status 
      != REG_COMMS_STATUS_CONNECTED) 
    Globus_attempt_listener_connect(&(IOTypes_table.io_def[index].socket_info));

  if (IOTypes_table.io_def[index].socket_info.comms_status 
      == REG_COMMS_STATUS_CONNECTED) {

#if DEBUG
    fprintf(stderr, "Emit_header_globus: socket status is connected, index = %d\n",
	    index );
#endif

    /* Send header */
    
    sprintf(buffer, REG_PACKET_FORMAT, REG_DATA_HEADER);

#if DEBUG
    fprintf(stderr, "Emit_header_globus: Sending >>%s<<\n", buffer);
#endif

    result = globus_io_write(&(IOTypes_table.io_def[index].socket_info.conn_handle), 
			     (globus_byte_t *)buffer, 
			     strlen(buffer), 
			     &nbytes);

    if(result == GLOBUS_SUCCESS){

      return REG_SUCCESS;
    }
    else{

#if DEBUG
      fprintf(stderr, "Emit_header_globus: globus_io_write failed - immediate retry connect\n");
#endif
      Globus_error_print(result);

      Globus_retry_accept_connect(&(IOTypes_table.io_def[index].socket_info));
      if (IOTypes_table.io_def[index].socket_info.comms_status == REG_COMMS_STATUS_CONNECTED) {  

#if DEBUG
    fprintf(stderr, "Emit_header_globus: Sending >>%s<<\n", buffer);
#endif    
	result = globus_io_write(&(IOTypes_table.io_def[index].socket_info.conn_handle), 
				 (globus_byte_t *)buffer, 
				 strlen(buffer), 
				 &nbytes);
	
	if(result == GLOBUS_SUCCESS){

	  return REG_SUCCESS;
	}
	
#if DEBUG
	fprintf(stderr, "Emit_header_globus: globus_io_write failed\n");
#endif
	Globus_error_print(result);

      }

      /* ARPDBG add check on error code */
      return REG_FAILURE;
    }
  }
  else {
#if DEBUG
    fprintf(stderr, "Emit_header_globus: socket not connected, index = %d\n",
	    index );
#endif
    return REG_FAILURE;
  }

}
#endif

/*---------------------------------------------------*/

#if REG_GLOBUS_SAMPLES
int Emit_footer_globus(const int index,
		       const char * const buffer)
{
  globus_size_t   nbytes;
  globus_result_t result;

#if DEBUG
  fprintf(stderr, "Emit_footer_globus: Sending >>%s<<\n", buffer);
#endif

  result = globus_io_write(&(IOTypes_table.io_def[index].socket_info.conn_handle), 
			   (globus_byte_t *)buffer, 
			   strlen(buffer), 
			   &nbytes);
  
  if(result != GLOBUS_SUCCESS){

    fprintf(stderr, "Emit_footer_globus: call to globus_io_write failed\n");
    Globus_error_print(result);
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
  globus_result_t  result;
  globus_size_t    nbytes;

  if(IOTypes_table.io_def[index].use_xdr && datatype != REG_CHAR){
    /* XDR-encoded data stored in buffer associated with IO channel */
    result = globus_io_write(&(IOTypes_table.io_def[index].socket_info.conn_handle), 
			     IOTypes_table.io_def[index].buffer, 
			     (globus_size_t) num_bytes_to_send, 
			     &nbytes);
  }
  else{
    /* Just send raw data as provided by calling routine */
    result = globus_io_write(&(IOTypes_table.io_def[index].socket_info.conn_handle), 
			     pData, 
			     (globus_size_t) num_bytes_to_send, 
			     &nbytes);
  }

#if DEBUG
  fprintf(stderr, "Emit_data_globus: sent %d bytes...\n", nbytes);
#endif

  if (result != GLOBUS_SUCCESS ) {
    fprintf(stderr, "Emit_data_globus: error globus_io_write\n");
    Globus_error_print(result);
    return REG_FAILURE;
  }


}
#endif

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

