/*-----------------------------------------------------------------------
  (C) Copyright 2009, University of Manchester, United Kingdom,
  all rights reserved.

  This software was developed by the RealityGrid project
  (http://www.realitygrid.org), funded by the EPSRC under grants
  GR/R67699/01 and GR/R67699/02.

  LICENCE TERMS

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:
  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.

  THIS MATERIAL IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. THE ENTIRE RISK AS TO THE QUALITY
  AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE PROGRAM PROVE
  DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR
  CORRECTION.
-----------------------------------------------------------------------*/

/** @internal
    @file ReG_Steer_Appside_Sockets.c
    @brief Source file for socket-related routines and data structures
    @author Robert Haines
  */

#include "ReG_Steer_Config.h"

#include "ReG_Steer_Samples_Transport_API.h"
#include "ReG_Steer_Sockets_Common.h"
#include "ReG_Steer_Common.h"
#include "ReG_Steer_Appside_internal.h"

/* */
extern socket_info_table_type socket_info_table;

/* Need access to these tables which are actually declared in 
   ReG_Steer_Appside_internal.h */
extern IOdef_table_type IOTypes_table;
extern Steerer_connection_table_type Steerer_connection;

/** Global scratch buffer - declared in ReG_Steer_Appside.c */
extern char Global_scratch_buffer[];

/*---------------------------------------------------*/

int Initialize_samples_transport() {

#if !REG_HAS_MSG_NOSIGNAL
  signal(SIGPIPE, signal_handler_sockets);
#endif

  return socket_info_table_init();
}

/*---------------------------------------------------*/

int Finalize_samples_transport() {
  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Initialize_IOType_transport_impl(const int direction, const int index) {

  int return_status = REG_SUCCESS;

  /* set up socket info stuff */
  if(socket_info_init(index) != REG_SUCCESS) {
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: Initialize_IOType_transport: failed to "
	    "init socket info for IOType\n");
#endif
    return_status = REG_FAILURE;
  }
  else {
    if(direction == REG_IO_OUT) {

      /* Don't create socket yet if this flag is set */
      if(IOTypes_table.enable_on_registration == REG_FALSE) return REG_SUCCESS;

      /* open socket and register callback function to listen for and
	 accept connections */
      if(create_listener(index) != REG_SUCCESS) {
#ifdef REG_DEBUG
	fprintf(stderr, "STEER: Initialize_IOType_transport: failed to "
		"create listener for IOType\n");
#endif
	return_status = REG_FAILURE;
      }
      else {
	fprintf(stderr, "STEER: Initialize_IOType_transport: Created "
		"listener on port %d, index %d, label %s\n", 
		socket_info_table.socket_info[index].listener_port, 
		index, IOTypes_table.io_def[index].label );
      }
    }
    else if(direction == REG_IO_IN) {

      /* Keep a count of how many input channels have been registered and
	 where this channel is in that list - this is used to map to the
	 list of data inputs held by our SGS (configured when it was 
	 created) */
      IOTypes_table.io_def[index].input_index = ++(IOTypes_table.num_inputs);

      /* Don't create socket yet if this flag is set */
      if(IOTypes_table.enable_on_registration == REG_FALSE) {
	return REG_SUCCESS;
      }

      if(create_connector(index) != REG_SUCCESS) {
#ifdef REG_DEBUG
	fprintf(stderr, "STEER: Initialize_IOType_transport: failed to "
		"register connector for IOType\n");
#endif
	return_status = REG_FAILURE;
      }
#ifdef REG_DEBUG
      else {
	fprintf(stderr, "STEER: Initialize_IOType_transport: "
		"registered connector on port %d, hostname = %s, "
		"index %d, label %s\n", 
		socket_info_table.socket_info[index].connector_port,
		socket_info_table.socket_info[index].connector_hostname,
		index, IOTypes_table.io_def[index].label );
      }
#endif
      
    }
  }

  return return_status;
}

/*---------------------------------------------------*/

void Finalize_IOType_transport_impl() {
  int index;

  for(index = 0; index < IOTypes_table.num_registered; index++) {
    if(IOTypes_table.io_def[index].direction == REG_IO_OUT) {
      /* close sockets */
      cleanup_listener_connection(index);
      socket_info_cleanup(index);
    }
    else if(IOTypes_table.io_def[index].direction == REG_IO_IN) {
      /* close sockets */
      cleanup_connector_connection(index);
      socket_info_cleanup(index);
    }
  }
}

/*---------------------------------------------------*/

int Disable_IOType_impl(const int index) {
  /* check index is valid */
  if(index < 0 || index >= IOTypes_table.num_registered) {
    fprintf(stderr, "STEER: Disable_IOType: index out of range\n");
    return REG_FAILURE;
  }

  if(IOTypes_table.io_def[index].direction == REG_IO_OUT) {
    /* close sockets */
    cleanup_listener_connection(index);
  }
  else if(IOTypes_table.io_def[index].direction == REG_IO_IN) {
    /* close sockets */
    cleanup_connector_connection(index);
  }

  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Enable_IOType_impl(int index) {
  /* check index is valid */
  if(index < 0 || index >= IOTypes_table.num_registered) return REG_FAILURE;

  if(IOTypes_table.io_def[index].direction == REG_IO_OUT) {
    /* open socket and register callback function to listen for and
       accept connections */
    if(create_listener(index) != REG_SUCCESS) {
#ifdef REG_DEBUG
      fprintf(stderr, "STEER: Enable_IOType: failed to create listener for IOType\n");
#endif
      return REG_FAILURE;
    }
  }
  else if(IOTypes_table.io_def[index].direction == REG_IO_IN) {
    if (create_connector(index) != REG_SUCCESS) {
#ifdef REG_DEBUG
      fprintf(stderr, "STEER: Enable_IOType: failed to register "
	      "connector for IOType\n");
#endif
      return REG_FAILURE;
    }
  }

  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Get_communication_status_impl(const int index) {
  if(socket_info_table.socket_info[index].comms_status != 
     REG_COMMS_STATUS_CONNECTED)
    return REG_FAILURE;
  
  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Emit_data_non_blocking_impl(const int index, const int size, 
				   void* buffer) {

  struct timeval timeout;
  int connector = socket_info_table.socket_info[index].connector_handle;
  fd_set sock;

  timeout.tv_sec  = 0;
  timeout.tv_usec = 0;

  FD_ZERO(&sock);
  FD_SET(connector, &sock);

  if(select(connector + 1, NULL, &sock, NULL, &timeout) == -1) {
    perror("select");
    return REG_FAILURE;
  }

  /* are we free to write? */
  if(FD_ISSET(connector, &sock)) {
    return Emit_data_impl(index, size, buffer);
  }

  return REG_FAILURE;
}

/*---------------------------------------------------*/

int Emit_header_impl(const int index) {

  char buffer[REG_PACKET_SIZE];
  int status;

  /* check if socket connection has been made */
  if(socket_info_table.socket_info[index].comms_status != 
     REG_COMMS_STATUS_CONNECTED) {
    attempt_listener_connect(index);
  }

  /* now are we connected? */
  if(socket_info_table.socket_info[index].comms_status == 
     REG_COMMS_STATUS_CONNECTED) {
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: Emit_header: socket status is connected, index = %d\n", index );
#endif

    /* send header */
    sprintf(buffer, REG_PACKET_FORMAT, REG_DATA_HEADER);
    buffer[REG_PACKET_SIZE - 1] = '\0';
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: Emit_header: Sending >>%s<<\n", buffer);
#endif
    status = Emit_data_non_blocking_impl(index, REG_PACKET_SIZE, 
					 (void*) buffer);

    if(status == REG_SUCCESS) {
#ifdef REG_DEBUG
      fprintf(stderr, "STEER: Emit_header: Sent %d bytes\n", REG_PACKET_SIZE);
#endif
      return REG_SUCCESS;
    }
    else if(status == REG_FAILURE) {
#ifdef REG_DEBUG
      fprintf(stderr, "STEER: Emit_header: Write failed - "
	      "immediate retry connect\n");
#endif
      retry_accept_connect(index);

      if(socket_info_table.socket_info[index].comms_status == 
	 REG_COMMS_STATUS_CONNECTED) {
#ifdef REG_DEBUG
	fprintf(stderr, "STEER: Emit_header: Sending >>%s<<\n", buffer);
#endif    
	if(Emit_data_impl(index, REG_PACKET_SIZE, 
			  (void*) buffer) == REG_SUCCESS) {
	  return REG_SUCCESS;
	}
      }
    }
#ifdef REG_DEBUG
    else{
      fprintf(stderr, "STEER: Emit_header: attempt to write to "
	      "socket timed out\n");
    }
#endif
  }
#ifdef REG_DEBUG
  else {
    fprintf(stderr, "STEER: Emit_header: socket not connected, "
	    "index = %d\n", index );
  }
#endif

  return REG_FAILURE;
}
/*---------------------------------------------------*/

int Emit_data_impl(const int    index, 
		      const size_t num_bytes_to_send, 
		      void*        pData)
{
  int bytes_left;
  int result;
  char* pchar;
  int connector = socket_info_table.socket_info[index].connector_handle;

  if(num_bytes_to_send < 0) {
    fprintf(stderr, "STEER: Emit_data: requested to write < 0 bytes!\n");
    return REG_FAILURE;
  }
  else if(num_bytes_to_send == 0) {
    fprintf(stderr, "STEER: Emit_data: asked to send 0 bytes!\n");
    return REG_SUCCESS;
  }

  bytes_left = num_bytes_to_send;
  pchar = (char*) pData;

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: Emit_data: writing...\n");
#endif
  while(bytes_left > 0) {
    result = send_no_signal(connector, pchar, bytes_left, 0);
    if(result == REG_SOCKETS_ERROR) {
      perror("send");
      return REG_FAILURE;
    }
    else {
      bytes_left -= result;
      pchar += result;
    }
  }

  if(bytes_left > 0) {
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: Emit_data: timed-out trying to write data\n");
#endif
    return REG_TIMED_OUT;
  }

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: Emit_data: sent %d bytes...\n", 
	  (int) num_bytes_to_send);
#endif
  
  return REG_SUCCESS;
}


/*---------------------------------------------------*/

int Consume_msg_header_impl(int index, int* datatype, int* count, 
			       int* num_bytes, int* is_fortran_array) {

  int nbytes;
  char buffer[REG_PACKET_SIZE];
  socket_info_type  *sock_info;
  sock_info = &(socket_info_table.socket_info[index]);

  /* check socket connection has been made */
  if (sock_info->comms_status != REG_COMMS_STATUS_CONNECTED) return REG_FAILURE;

  /* Read header */
#ifdef REG_DEBUG_FULL
  fprintf(stderr, "STEER: Consume_msg_header: calling recv...\n");
#endif

  /* Blocks until REG_PACKET_SIZE bytes received */
  if((nbytes = recv(sock_info->connector_handle, buffer, REG_PACKET_SIZE, 
		    MSG_WAITALL)) <= 0) {
    if(nbytes < 0) {
      /* error */
      perror("recv");
    }
#ifdef REG_DEBUG
    else {
      /* closed connection */
      fprintf(stderr, "STEER: Consume_msg_header: hung up!\n");
    }
#endif

    return REG_FAILURE;
  }

  /* if we're here, we've got data */
#ifdef REG_DEBUG_FULL
  fprintf(stderr, "STEER: Consume_msg_header: read <%s> from socket\n", 
	  buffer);
#endif

  /* Check for end of data */
  if(!strncmp(buffer, REG_DATA_FOOTER, strlen(REG_DATA_FOOTER))) {
    return REG_EOD;
  }
  else if(strncmp(buffer, BEGIN_SLICE_HEADER, strlen(BEGIN_SLICE_HEADER))) {
    fprintf(stderr, "STEER: ERROR: Consume_msg_header: incorrect "
	    "header on slice\n");
    return REG_FAILURE;
  }

  /*--- Type of objects in message ---*/
  if((nbytes = recv(sock_info->connector_handle, buffer, REG_PACKET_SIZE, 
		    MSG_WAITALL)) <= 0) {
    if(nbytes == 0) {
      /* closed connection */
      fprintf(stderr, "STEER: Consume_msg_header: hung up!\n");
    }
    else {
      /* error */
      perror("recv");
    }

    return REG_FAILURE;
  }

#ifdef REG_DEBUG_FULL
  fprintf(stderr, "STEER: Consume_msg_header: read <%s> from socket\n", 
	  buffer);
#endif

  if(!strstr(buffer, "<Data_type>")) {
    return REG_FAILURE;
  }

  sscanf(buffer, "<Data_type>%d</Data_type>", datatype);

  /*--- No. of objects in message ---*/
  if((nbytes = recv(sock_info->connector_handle, buffer, REG_PACKET_SIZE, 
		    MSG_WAITALL)) <= 0) {
    if(nbytes == 0) {
      /* closed connection */
      fprintf(stderr, "STEER: Consume_msg_header: hung up!\n");
    }
    else {
      /* error */
      perror("recv");
    }

    return REG_FAILURE;
  }

#ifdef REG_DEBUG_FULL
  fprintf(stderr, "STEER: Consume_msg_header: read <%s> from socket\n", 
	  buffer);
#endif

  if(!strstr(buffer, "<Num_objects>")) {
    return REG_FAILURE;
  }

  if(sscanf(buffer, "<Num_objects>%d</Num_objects>", count) != 1){
    fprintf(stderr, "STEER: ERROR: Consume_msg_header: failed to "
	    "read Num_objects\n");
    return REG_FAILURE;
  }

  /*--- No. of bytes in message ---*/
  if((nbytes = recv(sock_info->connector_handle, buffer, REG_PACKET_SIZE, 
		    MSG_WAITALL)) <= 0) {
    if(nbytes == 0) {
      /* closed connection */
      fprintf(stderr, "STEER: Consume_msg_header: hung up!\n");
    }
    else {
      /* error */
      perror("recv");
    }

    return REG_FAILURE;
  }

#ifdef REG_DEBUG_FULL
  fprintf(stderr, "STEER: Consume_msg_header: read >%s< from socket\n", 
	  buffer);
#endif

  if(!strstr(buffer, "<Num_bytes>")) {
    return REG_FAILURE;
  }

  if(sscanf(buffer, "<Num_bytes>%d</Num_bytes>", num_bytes) != 1) {
    fprintf(stderr, "STEER: ERROR: Consume_msg_header: failed to read "
	    "Num_bytes\n");
    return REG_FAILURE;
  }

  /*--- Array ordering in message ---*/
  if((nbytes = recv(sock_info->connector_handle, buffer, REG_PACKET_SIZE, 
		    MSG_WAITALL)) <= 0) {
    if(nbytes == 0) {
      /* closed connection */
      fprintf(stderr, "STEER: Consume_msg_header: hung up!\n");
    }
    else {
      /* error */
      perror("recv");
    }

    return REG_FAILURE;
  }

#ifdef REG_DEBUG_FULL
  fprintf(stderr, "STEER: Consume_msg_header_socket: read >%s< from socket\n", 
	  buffer);
#endif

  if(!strstr(buffer, "<Array_order>")) {
    return REG_FAILURE;
  }

  if(strstr(buffer, "FORTRAN")){
    /* Array data is from Fortran */
    *is_fortran_array = REG_TRUE;
  }
  else{
    /* Array data is not from Fortran */
    *is_fortran_array = REG_FALSE;
  }

  /*--- End of header ---*/
  if((nbytes = recv(sock_info->connector_handle, buffer, REG_PACKET_SIZE, 
		    MSG_WAITALL)) <= 0) {
    if(nbytes == 0) {
      /* closed connection */
      fprintf(stderr, "STEER: Consume_msg_header: hung up!\n");
    }
    else {
      /* error */
      perror("recv");
    }

    return REG_FAILURE;
  }

#ifdef REG_DEBUG_FULL
  fprintf(stderr, "STEER: Consume_msg_header: read <%s> from socket\n", 
	  buffer);
#endif

  if(strncmp(buffer, END_SLICE_HEADER, strlen(END_SLICE_HEADER))) {
    fprintf(stderr, "STEER: ERROR: Consume_msg_header: failed to find "
	    "end of header\n");
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Consume_start_data_check_impl(const int index) {  
  char buffer[REG_PACKET_SIZE];
  char* pstart;
  int nbytes = 0;
  int nbytes1 = 0;
  int attempt_reconnect;

  socket_info_type  *sock_info;
  sock_info = &(socket_info_table.socket_info[index]);

  /* if not connected attempt to connect now */
  if(sock_info->comms_status != REG_COMMS_STATUS_CONNECTED) {
    attempt_connector_connect(index);
  }

  /* check if socket connection has been made */
  if(sock_info->comms_status != REG_COMMS_STATUS_CONNECTED) {
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: INFO: Consume_start_data_check_socket: socket is NOT "
	    "connected, index = %d\n", index);
#endif
    return REG_FAILURE;
  }

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: INFO: Consume_start_data_check_socket: socket status "
	  "is connected, index = %d\n", index);
#endif

  /* Drain socket until start tag found */
  attempt_reconnect = 1;
  memset(buffer, '\0', 1);

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: Consume_start_data_check_socket: searching for start tag\n");
#endif

  while(!(pstart = strstr(buffer, REG_DATA_HEADER))) {

    if((nbytes = recv_non_block(sock_info->connector_handle,
				buffer, REG_PACKET_SIZE, 0)) <= 0) {

      if(nbytes < 0) {
	if(errno == EAGAIN) {
	  /* Call would have blocked because no data to read */
#ifdef REG_DEBUG
	  fprintf(stderr, "\n");
#endif
	  /* Call was OK but there's no data to read... */
	  return REG_FAILURE;
	}
	else {
	  /* Some error occurred */
#ifdef REG_DEBUG
	  fprintf(stderr, "\n");
#endif
	  perror("STEER: recv");
	}
      }
#ifdef REG_DEBUG
      else {
	/* recv returned 0 bytes => closed connection */
	fprintf(stderr, "STEER: Consume_start_data_check: hung up!\n");
      }
#endif

      /* We're in the middle of a while loop here so don't keep trying
	 to reconnect ad infinitum */
      if(!attempt_reconnect) {
	return REG_FAILURE;
      }

#ifdef REG_DEBUG
      fprintf(stderr, "\nSTEER: Consume_start_data_check: recv failed - "
	      "try immediate reconnect for index %d\n", index);
#endif

      retry_connect(index);

      /* check if socket reconnection has been made and check for 
	 data if it has */
      if (socket_info_table.socket_info[index].comms_status 
	  != REG_COMMS_STATUS_CONNECTED) {
	return REG_FAILURE;
      }
    
      attempt_reconnect = 0;
      memset(buffer, '\0', 1);
    }

#ifdef REG_DEBUG
    fprintf(stderr, "!");
#endif
  } /* !while */

#ifdef REG_DEBUG
  fprintf(stderr, "\n");
#endif

  if(nbytes > 0) {

#ifdef REG_DEBUG
    fprintf(stderr, "STEER: Consume_start_data_check: read >>%s<< "
	    "from socket\n", buffer);
#endif

    /* Need to make sure we've read the full packet marking the 
       beginning of the next data slice */
    nbytes1 = (int) (pstart - buffer) + (REG_PACKET_SIZE - nbytes);

    if(nbytes1 > 0) {

      if((nbytes = recv(sock_info->connector_handle, buffer, 
			nbytes1, 0)) <= 0) {
	if(nbytes == 0) {
	  /* closed connection */
	  fprintf(stderr, "STEER: Consume_start_data_check: hung up!\n");
	}
	else {
	  /* error */
	  perror("recv");
	}

	if(nbytes != nbytes1) {
	  fprintf(stderr, "STEER: ERROR: Consume_start_data_check: failed "
		  "to read remaining %d bytes of header\n", (int) nbytes1);
	  return REG_FAILURE;
	}
      }
    }

    IOTypes_table.io_def[index].buffer_max_bytes = REG_IO_BUFSIZE;
    IOTypes_table.io_def[index].buffer = (void*) malloc(REG_IO_BUFSIZE);
    if(!IOTypes_table.io_def[index].buffer) {
      IOTypes_table.io_def[index].buffer_max_bytes = 0;
      fprintf(stderr, "STEER: ERROR: Consume_start_data_check: malloc "
	      "of IO buffer failed\n");
      return REG_FAILURE;
    }

    /* added following one line for modularization
       moved from ReG_Steer_Appside.c Consume_start_data_check
     */
    IOTypes_table.io_def[index].consuming = REG_TRUE;
    return REG_SUCCESS;
  }
  return REG_FAILURE;
}

/*--------------------------------------------------------------*/

int Consume_data_read_impl(const int index, 
			      const int datatype, 
			      const int num_bytes_to_read, 
			      void *pData) {
  int nbytes;

  socket_info_type  *sock_info;

#ifdef REG_DEBUG

#ifdef USE_REG_TIMING
  double time0, time1;
#endif

  fprintf(stderr, "STEER: Consume_data_read: calling recv for %d bytes\n", (int) num_bytes_to_read);

#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time0);
#endif

#endif /* REG_DEBUG */

  sock_info = &(socket_info_table.socket_info[index]);

  if(IOTypes_table.io_def[index].use_xdr || IOTypes_table.io_def[index].convert_array_order == REG_TRUE) {
    nbytes = recv(sock_info->connector_handle, IOTypes_table.io_def[index].buffer, num_bytes_to_read, MSG_WAITALL);
  }
  else {
    nbytes = recv(sock_info->connector_handle, pData, num_bytes_to_read, MSG_WAITALL);
  }

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: Consume_data_read: recv read %d bytes\n", (int) nbytes);

#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time1);
  fprintf(stderr, "                                  in %.3f seconds\n", (float) (time1-time0));
#endif
#endif /* REG_DEBUG */

  if(nbytes <= 0) {
      if(nbytes == 0) {
	/* closed connection */
	fprintf(stderr, "STEER: INFO: Consume_data_read: hung up!\n");
      }
      else {
	/* error */
	perror("recv");
      }
      
      /* Reset use_xdr flag set as only valid on a per-slice basis */
      IOTypes_table.io_def[index].use_xdr = REG_FALSE;

      return REG_FAILURE;
    }

  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Emit_ack_impl(int index){

  /* Send a 16-byte acknowledgement message */
  char *ack_msg = "<ACK/>          ";
  return Emit_data_impl(index, strlen(ack_msg), (void*)ack_msg);
}

/*---------------------------------------------------*/

int Consume_ack_impl(int index){

  char *ack_msg = "<ACK/>";
  char  buf[32];
  char *pchar;
  int   nbytes;

  /* If no acknowledgement is currently required (e.g. this is the
     first time Emit_start has been called) then return success */
  if(IOTypes_table.io_def[index].ack_needed == REG_FALSE){
    return REG_SUCCESS;
  }

  /* Buffer is twice as long as ack message to allow us to deal with
     getting a truncated message */
  memset(buf, '\0', 32);

  /* Search for an ACK tag */
  if((nbytes = recv_non_block(socket_info_table.socket_info[index].connector_handle, 
			      (void*)buf, 16, 0)) == 16) {
    pchar = strchr(buf, '<');

    if(pchar){
      if(strstr(pchar, ack_msg)){
	return REG_SUCCESS;
      }
      else{
	if( (&(buf[15])- pchar + 1) > strlen(ack_msg) ){

	  /* We found the opening angle bracket but the rest of the tag
	     is missing so we fail */
	  return REG_FAILURE;
	}
	else{
	  /* Looks like our ack msg drops off end of buffer so get another
	     16 bytes */
	  if(recv_non_block(socket_info_table.socket_info[index].connector_handle, 
			    (void*)&(buf[16]), 16, 0) == 16) {

	    if( strstr(buf, ack_msg) ) return REG_SUCCESS;
	  }
	}
      }
    }
  }

  if(nbytes < 0) {
    if(errno == EAGAIN) {
      /* Call would have blocked because no data to read
       * Call was OK but there's no data to read... */
#ifdef REG_DEBUG_FULL
      fprintf(stderr, "STEER: Consume_ack: no data on socket to "
	      "read for ack\n");
#endif
      return REG_FAILURE;
    }
    else {
      /* Some error occurred */
      IOTypes_table.io_def[index].ack_needed = REG_FALSE;
    }
  }
  else {
    /* recv returned 0 bytes => closed connection */
    IOTypes_table.io_def[index].ack_needed = REG_FALSE;
  }

#ifdef REG_DEBUG_FULL
  fprintf(stderr, "STEER: INFO: Consume_ack: no ack received\n");
#endif
  return REG_FAILURE;
}

/*---------------------------------------------------*/

int Get_IOType_address_impl(int i, char** pbuf, int* bytes_left) {
  int nbytes;

  if(IOTypes_table.io_def[i].direction == REG_IO_OUT){
    if(!strstr(socket_info_table.socket_info[i].listener_hostname, "NOT_SET")) {
      nbytes = snprintf(*pbuf, *bytes_left, "<Address>%s:%d</Address>\n",
			socket_info_table.socket_info[i].listener_hostname,
			(int)(socket_info_table.socket_info[i].listener_port));

#ifdef REG_DEBUG
      /* Check for truncation */
      if((nbytes >= (*bytes_left-1)) || (nbytes < 1)){
	fprintf(stderr, "STEER: Emit_IOType_defs: message exceeds max. "
		"msg. size of %d bytes\n", REG_MAX_MSG_SIZE);
	return REG_FAILURE;
      }
#endif /* REG_DEBUG */
      *pbuf += nbytes;
      *bytes_left -= nbytes;
    }
  }

  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Emit_msg_header_impl(const int    index,
			 const size_t num_bytes_to_send,
			 void*        pData) {
  return Emit_data_impl(index, num_bytes_to_send, pData);
}

/*---------------------------------------------------*/

int Emit_start_impl(int index, int seqnum) {
  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Emit_stop_impl(int index) {
  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Consume_start_impl(int index) {
  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Consume_stop_impl(int index) {
  return REG_SUCCESS;
}

/*---------------------------------------------------*/
