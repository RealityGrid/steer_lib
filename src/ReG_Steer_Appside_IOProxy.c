/*-----------------------------------------------------------------------
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
-----------------------------------------------------------------------*/

/** @internal
    @file ReG_Steer_Appside_IOProxy.c
    @brief Source file for proxy-related routines for data IO
 
    This file contains routines and data structures for socket
    communication with a proxy or 'switch'.
    @author Andrew Porter
 */

#if REG_PROXY_SAMPLES || defined(DOXYGEN)

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
   ReG_Steer_Appside.c */
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
  if(IOTypes_table.io_def[index].socket_info.comms_status != 
     REG_COMMS_STATUS_CONNECTED)
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

int Read_proxy(const int index, int *size, void** buffer){

  char             tmpBuf[16];
  char             readBuf[REG_PACKET_SIZE];
  char            *pchar;
  char            *pnext;
  int              nbytes;
  socket_io_type  *sock_info;
  sock_info = &(IOTypes_table.io_def[index].socket_info);

  /* Blocks until REG_PACKET_SIZE bytes received */
  if((nbytes = recv(sock_info->connector_handle, readBuf, REG_PACKET_SIZE, 
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
    return REG_FAILURE;
  }

  /* if we're here, we've got data */
#if REG_DEBUG_FULL
  fprintf(stderr, "Read_proxy: read <%s> from socket\n", 
	  readBuf);
#endif
  /*
		while(in!='#') {
			in =fgetc( con->fd );
			
		}

		fscanf(con->fd, "%s", msg->dest );
		fscanf(con->fd, "%d", &(msg->code) );
		fscanf(con->fd, "%d", &(msg->length) );

		fgetc( con->fd );
*/
  pchar = strstr(readBuf, "#"); pchar++;
  pchar = strchr(pchar, '\n'); pchar++; /* End of destination label */
  pchar = strchr(pchar, '\n'); pchar++; /* End of msg code */
  pnext = strchr(pchar, '\n'); pchar++;/* End of msg length */
  memcpy(tmpBuf, pchar, (pnext-pchar));
  tmpBuf[(pnext-pchar)] = '\0';
  *size = atoi(tmpBuf);
  pchar++; /* Allow one more character (following hybrid_comms.c) */

  /* ARPDBG - optimise; use scratch instead of malloc?? */
  if( !(*buffer = malloc(*size)) ){
    fprintf(stderr, "Read_proxy: ERROR: malloc of %d bytes failed\n", *size);
  }

  /* Blocks until *size bytes received */
  if((nbytes = recv(sock_info->connector_handle, *buffer, *size, 
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
    return REG_FAILURE;
  }

  return REG_SUCCESS;
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

  char            *inBuffer;
  int              inBytes;
  char            *pchar;
  socket_io_type  *sock_info;
  sock_info = &(IOTypes_table.io_def[index].socket_info);

  /* check socket connection has been made */
  if (sock_info->comms_status != 
      REG_COMMS_STATUS_CONNECTED) return REG_FAILURE;

  /* Read header */
#if REG_DEBUG_FULL
  fprintf(stderr, "Consume_msg_header_proxy: calling recv...\n");
#endif

  if(Read_proxy(index, &inBytes, (void *)&inBuffer) != REG_SUCCESS){
    return REG_FAILURE;
  }

  /* if we're here, we've got data */
#if REG_DEBUG_FULL
  fprintf(stderr, "Consume_msg_header_proxy: read <%s> from socket\n", 
	  inBuffer);
#endif

  /* Check for end of data */
  if(!strncmp(inBuffer, REG_DATA_FOOTER, strlen(REG_DATA_FOOTER))) {
    free(inBuffer);
    return REG_EOD;
  }
  else if(strncmp(inBuffer, BEGIN_SLICE_HEADER, strlen(BEGIN_SLICE_HEADER))) {
    fprintf(stderr, "ERROR: Consume_msg_header_proxy: incorrect "
	    "header on slice\n");
    free(inBuffer);
    return REG_FAILURE;
  }

  if(!(pchar = strstr(inBuffer, "<Data_type>")) ){
    free(inBuffer);
    return REG_FAILURE;
  }
  sscanf(pchar, "<Data_type>%d</Data_type>", datatype);

  if( !(pchar = strstr(inBuffer, "<Num_objects>")) ){
    free(inBuffer);
    return REG_FAILURE;
  }
  if(sscanf(pchar, "<Num_objects>%d</Num_objects>", count) != 1){
    fprintf(stderr, "ERROR: Consume_msg_header_sockets: failed to "
	    "read Num_objects\n");
    free(inBuffer);
    return REG_FAILURE;
  }

  if( !(pchar = strstr(inBuffer, "<Num_bytes>")) ){
    free(inBuffer);
    return REG_FAILURE;
  }
  if(sscanf(pchar, "<Num_bytes>%d</Num_bytes>", num_bytes) != 1) {
    fprintf(stderr, "ERROR: Consume_msg_header_sockets: failed to read "
	    "Num_bytes\n");
    free(inBuffer);
    return REG_FAILURE;
  }

  if( !(pchar = strstr(inBuffer, "<Array_order>")) ){
    free(inBuffer);
    return REG_FAILURE;
  }
  if(strstr(pchar, "FORTRAN")){
    /* Array data is from Fortran */
    *is_fortran_array = REG_TRUE;
  }
  else{
    /* Array data is not from Fortran */
    *is_fortran_array = REG_FALSE;
  }

  /*--- End of header ---*/
  if(strncmp(inBuffer, END_SLICE_HEADER, strlen(END_SLICE_HEADER))) {
    fprintf(stderr, "ERROR: Consume_msg_header_sockets: failed to find "
	    "end of header\n");
    free(inBuffer);
    return REG_FAILURE;
  }

  free(inBuffer);
  return REG_SUCCESS;
}

/*---------------------------------------------------*/

#endif /* REG_PROXY_SAMPLES */
