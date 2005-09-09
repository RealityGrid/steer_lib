/*-----------------------------------------------------------------------
  This file contains routines and data structures for socket
  communication.

  (C) Copyright 2005, University of Manchester, United Kingdom,
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

  Authors........: Andrew Porter, Robert Haines

-----------------------------------------------------------------------*/

/** @file ReG_Steer_Appside_IOProxy.c
    @brief Source file for proxy-related routines for data IO
  */

#if REG_PROXY_SAMPLES

#include "ReG_Steer_Common.h"
#include "ReG_Steer_Appside_internal.h"
#include "ReG_Steer_Appside_Sockets.h"
#include "ReG_Steer_Appside_WSRF.h"
#include <string.h>
#include <signal.h>
#include <sys/time.h>

#ifdef _AIX
#include <fcntl.h>
#endif

#ifndef REG_DEBUG
#define REG_DEBUG 0
#endif

/* Need access to these tables which are actually declared in 
   ReG_Steer_Appside_internal.h */
extern IOdef_table_type IOTypes_table;

extern Steerer_connection_table_type Steerer_connection;

/*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*
 *                         EXTERNAL METHODS                           *
 *--------------------------------------------------------------------*/

int Initialize_IOType_transport_proxy(const int direction, const int index) {

  int return_status = REG_SUCCESS;

  /* set up socket info stuff */
  if(socket_info_init(index) != REG_SUCCESS) {
#if REG_DEBUG
    fprintf(stderr, "Initialize_IOType_transport_proxy: failed to init "
	    "socket info for IOType\n");
#endif
    return_status = REG_FAILURE;
  }
  else {
    if(direction == REG_IO_OUT) {

      /* Don't create socket yet if this flag is set */
      if(IOTypes_table.enable_on_registration == REG_FALSE) return REG_SUCCESS;

      /* Connect to the proxy */
      if(create_connector(index) != REG_SUCCESS) {
#if REG_DEBUG
	fprintf(stderr, "Initialize_IOType_transport_sockets: failed to "
		"connect to proxy for IOType\n");
#endif
	return_status = REG_FAILURE;
      }
#if REG_DEBUG
      else {
	fprintf(stderr, "Initialize_IOType_transport_sockets: "
		"registered connector to proxy on port %d, hostname = %s, "
		"index %d, label %s\n", 
		IOTypes_table.io_def[index].socket_info.connector_port,
		IOTypes_table.io_def[index].socket_info.connector_hostname,
		index, IOTypes_table.io_def[index].label );
      }
#endif
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
#if REG_DEBUG
	fprintf(stderr, "Initialize_IOType_transport_sockets: failed to "
		"register connector for IOType\n");
#endif
	return_status = REG_FAILURE;
      }
#if REG_DEBUG
      else {
	fprintf(stderr, "Initialize_IOType_transport_sockets: "
		"registered connector on port %d, hostname = %s, "
		"index %d, label %s\n", 
		IOTypes_table.io_def[index].socket_info.connector_port,
		IOTypes_table.io_def[index].socket_info.connector_hostname,
		index, IOTypes_table.io_def[index].label );
      }
#endif
    }
  }

  return return_status;
}

/*-----------------------------------------------------------*/

void Finalize_IOType_transport_proxy() {
  int index;

  /* For proxy, we only have connectors - no listeners */
  for(index = 0; index < IOTypes_table.num_registered; index++) {
    /* close sockets */
    cleanup_connector_connection(index);
    socket_info_cleanup(index);
  }
}

/*-----------------------------------------------------------*/

int Disable_IOType_proxy(const int index) {
  /* check index is valid */
  if(index < 0 || index >= IOTypes_table.num_registered) {
    fprintf(stderr, "Disable_IOType_sockets: index out of range\n");
    return REG_FAILURE;
  }

  /* close sockets */
  cleanup_connector_connection(index);

  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Enable_IOType_proxy(int index) {
  /* check index is valid */
  if(index < 0 || index >= IOTypes_table.num_registered) return REG_FAILURE;

  if (create_connector(index) != REG_SUCCESS) {
#if REG_DEBUG
    fprintf(stderr, "Enable_IOType_sockets: failed to register "
	    "connector for IOType\n");
#endif
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Get_communication_status_proxy(const int index) {
  if(IOTypes_table.io_def[index].socket_info.comms_status != REG_COMMS_STATUS_CONNECTED)
    return REG_FAILURE;
  
  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Write_proxy(const int index, const int size, void* buffer) {

  int   bytes_left;
  int   result;
  char* pchar;
  int   connector = IOTypes_table.io_def[index].socket_info.connector_handle;
  char *label = IOTypes_table.io_def[index].label;
  char  header[128];

  if(size < 0) {
    fprintf(stderr, "Write_proxy: requested to write < 0 bytes!\n");
    return REG_FAILURE;
  }
  else if(size == 0) {
    fprintf(stderr, "Write_proxy: asked to send 0 bytes!\n");
    return REG_SUCCESS;
  }

#if REG_DEBUG
  fprintf(stderr, "Write_proxy: writing...\n");
#endif
  /*
		fprintf( con->fd, "#%s\n", msg->dest );
		fprintf( con->fd, "%d\n", msg->code );
		fprintf( con->fd, "%d\n", msg->length );
		fwrite( msg->data, msg->length, 1, con->fd );
  */
  sprintf(header, "#%s\n%d\n%d\n", label, 1, size);
  bytes_left = strlen(header);
  pchar = header;

  while(bytes_left > 0) {
#ifndef __linux
    result = send(connector, pchar, bytes_left, 0);
#else
    result = send(connector, pchar, bytes_left, MSG_NOSIGNAL);
#endif
    if(result == REG_SOCKETS_ERROR) {
      perror("send");
      return REG_FAILURE;
    }
    else {
      bytes_left -= result;
      pchar += result;
    }
  }

  bytes_left = size;
  pchar = (char*) buffer;

  while(bytes_left > 0) {
#ifndef __linux
    result = send(connector, pchar, bytes_left, 0);
#else
    result = send(connector, pchar, bytes_left, MSG_NOSIGNAL);
#endif
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
#if REG_DEBUG
    fprintf(stderr, "Write_proxy: timed-out trying to write data\n");
#endif
    return REG_TIMED_OUT;
  }

  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Write_non_blocking_proxy(const int index, const int size, void* buffer) {

  struct timeval timeout;
  int connector = IOTypes_table.io_def[index].socket_info.connector_handle;
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
    return Write_proxy(index, size, buffer);
  }

  return REG_FAILURE;
}

/*---------------------------------------------------*/

int Read_proxy(const int index, int *size, void* buffer){

  socket_io_type  *sock_info;
  sock_info = &(IOTypes_table.io_def[index].socket_info);

  /* Blocks until REG_PACKET_SIZE bytes received */
  if((nbytes = recv(sock_info->connector_handle, buffer, REG_PACKET_SIZE, 
		    MSG_WAITALL)) <= 0) {
    if(nbytes < 0) {
      /* error */
      perror("recv");
    }
#if REG_DEBUG
    else {
      /* closed connection */
      fprintf(stderr, "Read_proxy: hung up!\n");
    }
#endif


}

/*---------------------------------------------------*/

int Emit_header_proxy(const int index) {

  char buffer[REG_PACKET_SIZE];
  int status;

  /* check if socket connection has been made */
  if(IOTypes_table.io_def[index].socket_info.comms_status !=
     REG_COMMS_STATUS_CONNECTED) {
    attempt_connector_connect(index);
  }

  /* now are we connected? */
  if(IOTypes_table.io_def[index].socket_info.comms_status == 
     REG_COMMS_STATUS_CONNECTED) {
#if REG_DEBUG
    fprintf(stderr, "Emit_header_proxy: socket status is "
	    "connected, index = %d\n", index );
#endif

    /* send header */
    sprintf(buffer, REG_PACKET_FORMAT, REG_DATA_HEADER);
    buffer[REG_PACKET_SIZE - 1] = '\0';
#if REG_DEBUG
    fprintf(stderr, "Emit_header_proxy: Sending >>%s<<\n", buffer);
#endif
    status = Write_non_blocking_proxy(index, REG_PACKET_SIZE, (void*) buffer);

    if(status == REG_SUCCESS) {
#if REG_DEBUG
      fprintf(stderr, "Emit_header_proxy: Sent %d bytes\n", REG_PACKET_SIZE);
#endif
      return REG_SUCCESS;
    }
    else if(status == REG_FAILURE) {
#if REG_DEBUG
      fprintf(stderr, "Emit_header_proxy: Write_non_blocking_proxy "
	      "failed - immediate retry connect\n");
#endif
      retry_accept_connect(index);

      if(IOTypes_table.io_def[index].socket_info.comms_status == 
	 REG_COMMS_STATUS_CONNECTED) {
#if REG_DEBUG
	fprintf(stderr, "Emit_header_proxy: Sending >>%s<<\n", buffer);
#endif    
	if(Write_proxy(index, REG_PACKET_SIZE, (void*) buffer) == REG_SUCCESS) {
	  return REG_SUCCESS;
	}
      }
    }
#if REG_DEBUG
    else{
      fprintf(stderr, "Emit_header_sockets: attempt to write to socket timed out\n");
    }
#endif
  }
#if REG_DEBUG
  else {
    fprintf(stderr, "Emit_header_proxy: socket not connected, index = %d\n", 
	    index );
  }
#endif

  return REG_FAILURE;
}
/*---------------------------------------------------*/

int Emit_data_proxy(const int index, const size_t num_bytes_to_send, 
		    void* pData) {
  if(Write_proxy(index, num_bytes_to_send, (void*) pData) != REG_SUCCESS) {
    fprintf(stderr, "Emit_data_proxy: error in send\n");
    return REG_FAILURE;
  }

#if REG_DEBUG
  fprintf(stderr, "Emit_data_proxy: sent %d bytes...\n", 
	  (int) num_bytes_to_send);
#endif
  
  return REG_SUCCESS;
}


/*---------------------------------------------------*/

int Consume_msg_header_proxy(int index, int* datatype, int* count, 
			     int* num_bytes, int* is_fortran_array) {

  int nbytes;
  char buffer[REG_PACKET_SIZE];
  socket_io_type  *sock_info;
  sock_info = &(IOTypes_table.io_def[index].socket_info);

  /* check socket connection has been made */
  if (sock_info->comms_status != REG_COMMS_STATUS_CONNECTED) return REG_FAILURE;

  /* Read header */
#if REG_DEBUG_FULL
  fprintf(stderr, "Consume_msg_header_proxy: calling recv...\n");
#endif

  /* Blocks until REG_PACKET_SIZE bytes received */
  if((nbytes = recv(sock_info->connector_handle, buffer, REG_PACKET_SIZE, 
		    MSG_WAITALL)) <= 0) {
    if(nbytes < 0) {
      /* error */
      perror("recv");
    }
#if REG_DEBUG
    else {
      /* closed connection */
      fprintf(stderr, "Consume_msg_header_proxy: hung up!\n");
    }
#endif

    return REG_FAILURE;
  }

  /* if we're here, we've got data */
#if REG_DEBUG_FULL
  fprintf(stderr, "Consume_msg_header_proxy: read <%s> from socket\n", 
	  buffer);
#endif

  /* Check for end of data */
  if(!strncmp(buffer, REG_DATA_FOOTER, strlen(REG_DATA_FOOTER))) {
    return REG_EOD;
  }
  else if(strncmp(buffer, BEGIN_SLICE_HEADER, strlen(BEGIN_SLICE_HEADER))) {
    fprintf(stderr, "ERROR: Consume_msg_header_proxy: incorrect "
	    "header on slice\n");
    return REG_FAILURE;
  }

  /*--- Type of objects in message ---*/
  if((nbytes = recv(sock_info->connector_handle, buffer, REG_PACKET_SIZE, 
		    MSG_WAITALL)) <= 0) {
    if(nbytes == 0) {
      /* closed connection */
      fprintf(stderr, "Consume_msg_header_proxy: hung up!\n");
    }
    else {
      /* error */
      perror("recv");
    }

    return REG_FAILURE;
  }

#if REG_DEBUG_FULL
  fprintf(stderr, "Consume_msg_header_sockets: read <%s> from socket\n", 
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
      fprintf(stderr, "Consume_msg_header_sockets: hung up!\n");
    }
    else {
      /* error */
      perror("recv");
    }

    return REG_FAILURE;
  }

#if REG_DEBUG_FULL
  fprintf(stderr, "Consume_msg_header_sockets: read <%s> from socket\n", 
	  buffer);
#endif

  if(!strstr(buffer, "<Num_objects>")) {
    return REG_FAILURE;
  }

  if(sscanf(buffer, "<Num_objects>%d</Num_objects>", count) != 1){
    fprintf(stderr, "ERROR: Consume_msg_header_sockets: failed to "
	    "read Num_objects\n");
    return REG_FAILURE;
  }

  /*--- No. of bytes in message ---*/
  if((nbytes = recv(sock_info->connector_handle, buffer, REG_PACKET_SIZE, 
		    MSG_WAITALL)) <= 0) {
    if(nbytes == 0) {
      /* closed connection */
      fprintf(stderr, "Consume_msg_header_sockets: hung up!\n");
    }
    else {
      /* error */
      perror("recv");
    }

    return REG_FAILURE;
  }

#if REG_DEBUG_FULL
  fprintf(stderr, "Consume_msg_header_sockets: read >%s< from socket\n", 
	  buffer);
#endif

  if(!strstr(buffer, "<Num_bytes>")) {
    return REG_FAILURE;
  }

  if(sscanf(buffer, "<Num_bytes>%d</Num_bytes>", num_bytes) != 1) {
    fprintf(stderr, "ERROR: Consume_msg_header_sockets: failed to read "
	    "Num_bytes\n");
    return REG_FAILURE;
  }

  /*--- Array ordering in message ---*/
  if((nbytes = recv(sock_info->connector_handle, buffer, REG_PACKET_SIZE, 
		    MSG_WAITALL)) <= 0) {
    if(nbytes == 0) {
      /* closed connection */
      fprintf(stderr, "Consume_msg_header_sockets: hung up!\n");
    }
    else {
      /* error */
      perror("recv");
    }

    return REG_FAILURE;
  }

#if REG_DEBUG_FULL
  fprintf(stderr, "Consume_msg_header_socket: read >%s< from socket\n", 
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
      fprintf(stderr, "Consume_msg_header_sockets: hung up!\n");
    }
    else {
      /* error */
      perror("recv");
    }

    return REG_FAILURE;
  }

#if REG_DEBUG_FULL
  fprintf(stderr, "Consume_msg_header_sockets: read <%s> from socket\n", 
	  buffer);
#endif

  if(strncmp(buffer, END_SLICE_HEADER, strlen(END_SLICE_HEADER))) {
    fprintf(stderr, "ERROR: Consume_msg_header_sockets: failed to find "
	    "end of header\n");
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Consume_start_data_check_sockets(const int index) {  
  char buffer[REG_PACKET_SIZE];
  char* pstart;
  int nbytes = 0;
  int nbytes1 = 0;
  int attempt_reconnect;

  socket_io_type  *sock_info;
  sock_info = &(IOTypes_table.io_def[index].socket_info);

  /* if not connected attempt to connect now */
  if(sock_info->comms_status != REG_COMMS_STATUS_CONNECTED) {
    attempt_connector_connect(index);
  }

  /* check if socket connection has been made */
  if(sock_info->comms_status != REG_COMMS_STATUS_CONNECTED) {
#if REG_DEBUG
    fprintf(stderr, "INFO: Consume_start_data_check_socket: socket is NOT "
	    "connected, index = %d\n", index);
#endif
    return REG_FAILURE;
  }

#if REG_DEBUG
  fprintf(stderr, "INFO: Consume_start_data_check_socket: socket status "
	  "is connected, index = %d\n", index);
#endif

  /* Drain socket until start tag found */
  attempt_reconnect = 1;
  memset(buffer, '\0', 1);

#if REG_DEBUG
  fprintf(stderr, "Consume_start_data_check_socket: searching for start tag\n");
#endif

  while(!(pstart = strstr(buffer, REG_DATA_HEADER))) {

    if( (nbytes = recv_non_block(sock_info, buffer, REG_PACKET_SIZE)) <= 0){

      if(nbytes < 0) {
	if(errno == EAGAIN) {
	  /* Call would have blocked because no data to read */
#if REG_DEBUG
	  fprintf(stderr, "\n");
#endif
	  /* Call was OK but there's no data to read... */
	  return REG_FAILURE;
	}
	else {
	  /* Some error occurred */
#if REG_DEBUG
	  fprintf(stderr, "\n");
#endif
	  perror("recv");
	}
      }
#if REG_DEBUG
      else {
	/* recv returned 0 bytes => closed connection */
	fprintf(stderr, "Consume_start_data_check_sockets: hung up!\n");
      }
#endif

      /* We're in the middle of a while loop here so don't keep trying
	 to reconnect ad infinitum */
      if(!attempt_reconnect) {
	return REG_FAILURE;
      }

#if REG_DEBUG
      fprintf(stderr, "\nConsume_start_data_check_sockets: recv failed - "
	      "try immediate reconnect for index %d\n", index);
#endif

      retry_connect(index);

      /* check if socket reconnection has been made and check for 
	 data if it has */
      if (IOTypes_table.io_def[index].socket_info.comms_status 
	  != REG_COMMS_STATUS_CONNECTED) {
	return REG_FAILURE;
      }
    
      attempt_reconnect = 0;
      memset(buffer, '\0', 1);
    }

#if REG_DEBUG
    fprintf(stderr, "!");
#endif
  } /* !while */

#if REG_DEBUG
  fprintf(stderr, "\n");
#endif

  if(nbytes > 0) {

#if REG_DEBUG
    fprintf(stderr, "Consume_start_data_check_sockets: read >>%s<< "
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
	  fprintf(stderr, "Consume_start_data_check_sockets: hung up!\n");
	}
	else {
	  /* error */
	  perror("recv");
	}

	if(nbytes != nbytes1) {
	  fprintf(stderr, "ERROR: Consume_start_data_check_sockets: failed "
		  "to read remaining %d bytes of header\n", (int) nbytes1);
	  return REG_FAILURE;
	}
      }
    }

    IOTypes_table.io_def[index].buffer_max_bytes = REG_IO_BUFSIZE;
    IOTypes_table.io_def[index].buffer = (void*) malloc(REG_IO_BUFSIZE);
    if(!IOTypes_table.io_def[index].buffer) {
      IOTypes_table.io_def[index].buffer_max_bytes = 0;
      fprintf(stderr, "ERROR: Consume_start_data_check_sockets: malloc "
	      "of IO buffer failed\n");
      return REG_FAILURE;
    }

    return REG_SUCCESS;
  }
  return REG_FAILURE;
}

/*--------------------------------------------------------------*/

int Consume_data_read_sockets(const int index, const int datatype, const int num_bytes_to_read, void *pData) {
  int nbytes;

  socket_io_type  *sock_info;

#if REG_DEBUG

#ifdef USE_REG_TIMING
  double time0, time1;
#endif

  fprintf(stderr, "Consume_data_read_sockets: calling recv for %d bytes\n", (int) num_bytes_to_read);

#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time0);
#endif

#endif /* REG_DEBUG */

  sock_info = &(IOTypes_table.io_def[index].socket_info);

  if(IOTypes_table.io_def[index].use_xdr || IOTypes_table.io_def[index].convert_array_order == REG_TRUE) {
    nbytes = recv(sock_info->connector_handle, IOTypes_table.io_def[index].buffer, num_bytes_to_read, MSG_WAITALL);
  }
  else {
    nbytes = recv(sock_info->connector_handle, pData, num_bytes_to_read, MSG_WAITALL);
  }

#if REG_DEBUG
  fprintf(stderr, "Consume_data_read_sockets: recv read %d bytes\n", (int) nbytes);

#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time1);
  fprintf(stderr, "                          in %.3f seconds\n", (float) (time1-time0));
#endif
#endif /* REG_DEBUG */

  if(nbytes <= 0) {
      if(nbytes == 0) {
	/* closed connection */
	fprintf(stderr, "INFO: Consume_data_read_sockets: hung up!\n");
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

#ifndef __linux
void signal_handler_sockets(int a_signal) {

#if REG_DEBUG
  fprintf(stderr, "INFO: Caught SIGPIPE!\n");
#endif

  signal(SIGPIPE, signal_handler_sockets);

}
#endif

/*---------------------------------------------------*/

int Emit_ack_sockets(int index){

  /* Send a 16-byte acknowledgement message */
  char *ack_msg = "<ACK/>          ";
  return Write_sockets(index, strlen(ack_msg), (void*)ack_msg);
}

/*---------------------------------------------------*/

int Consume_ack_sockets(int index){

  char *ack_msg = "<ACK/>";
  char  buf[32];
  char *pchar;
  int   nbytes;

  /* If no acknowledgement is currently required (e.g. this is the
     first time Emit_start has been called) then return success */
  if(IOTypes_table.io_def[index].ack_needed == REG_FALSE)return REG_SUCCESS;

  /* Buffer is twice as long as ack message to allow us to deal with
     getting a truncated message */
  memset(buf, '\0', 32);

  /* Search for an ACK tag */
  if((nbytes = recv_non_block(&(IOTypes_table.io_def[index].socket_info), 
			      (void *)buf, 16)) == 16){
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
	  if(recv_non_block(&(IOTypes_table.io_def[index].socket_info), 
			    (void *)&(buf[16]), 16) == 16){

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

#if REG_DEBUG_FULL
  fprintf(stderr, "INFO: Consume_ack_sockets: no ack received\n");
#endif
  return REG_FAILURE;
}

/*---------------------------------------------------*/

#endif /* REG_PROXY_SAMPLES */
