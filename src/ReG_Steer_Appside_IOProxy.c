/*-----------------------------------------------------------------------
  (C) Copyright 2006, University of Manchester, United Kingdom,
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

int Initialize_IOType_transport_proxy(const int direction, 
				      const int index) {

  int return_status = REG_SUCCESS;

  /* set up socket info stuff */
  if(socket_info_init(index) != REG_SUCCESS) {
#if REG_DEBUG
    fprintf(stderr, "STEER: Initialize_IOType_transport_proxy: failed to init "
	    "socket info for IOType\n");
#endif
    return_status = REG_FAILURE;
  }
  else {
    /* Keep a count of how many channels have been registered and
       where this channel is in that list - this is used to map to the
       list of data inputs held by our SGS (configured when it was 
       created) */
    IOTypes_table.io_def[index].input_index = ++(IOTypes_table.num_inputs);

    if(direction == REG_IO_OUT) {

      /* Don't create socket yet if this flag is set */
      if(IOTypes_table.enable_on_registration == REG_FALSE) return REG_SUCCESS;

      /* Connect to the proxy */
      if(create_connector(index) != REG_SUCCESS) {
#if REG_DEBUG
	fprintf(stderr, "STEER: Initialize_IOType_transport_sockets: failed to "
		"connect to proxy for IOType\n");
#endif
	return_status = REG_FAILURE;
      }
#if REG_DEBUG
      else {
	fprintf(stderr, "STEER: Initialize_IOType_transport_sockets: "
		"registered connector to proxy on port %d, hostname = %s, "
		"index %d, label %s\n", 
		IOTypes_table.io_def[index].socket_info.connector_port,
		IOTypes_table.io_def[index].socket_info.connector_hostname,
		index, IOTypes_table.io_def[index].label );
      }
#endif
    }
    else if(direction == REG_IO_IN) {

      /* Don't create socket yet if this flag is set */
      if(IOTypes_table.enable_on_registration == REG_FALSE) {
	return REG_SUCCESS;
      }

      /* Connect to the proxy */
      if(create_connector(index) != REG_SUCCESS) {
#if REG_DEBUG
	fprintf(stderr, "STEER: Initialize_IOType_transport_sockets: failed to "
		"register connector for IOType\n");
#endif
	return_status = REG_FAILURE;
      }
#if REG_DEBUG
      else {
	fprintf(stderr, "STEER: Initialize_IOType_transport_sockets: "
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
    fprintf(stderr, "STEER: Disable_IOType_sockets: index out of range\n");
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
    fprintf(stderr, "STEER: Enable_IOType_sockets: failed to register "
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

int Emit_data_proxy(const int index, const int size, void* buffer) {

  int   bytes_left;
  int   result;
  char* pchar;
  int   connector = IOTypes_table.io_def[index].socket_info.connector_handle;
  char *label = IOTypes_table.io_def[index].label;
  char  header[128];

  if(size < 0) {
    fprintf(stderr, "STEER: Emit_data_proxy: requested to write < 0 bytes!\n");
    return REG_FAILURE;
  }
  else if(size == 0) {
    fprintf(stderr, "STEER: Emit_data_proxy: asked to send 0 bytes!\n");
    return REG_SUCCESS;
  }

#if REG_DEBUG
  fprintf(stderr, "STEER: Emit_data_proxy: writing...\n");
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
    fprintf(stderr, "STEER: Emit_data_proxy: timed-out trying to write data\n");
#endif
    return REG_TIMED_OUT;
  }

#if REG_DEBUG
  fprintf(stderr, "STEER: Emit_data_proxy: sent %d bytes...\n", (int) size);
#endif

  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Emit_data_non_blocking_proxy(const int index, const int size, 
				 void* buffer) {

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
    return Emit_data_proxy(index, size, buffer);
  }

  return REG_FAILURE;
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
    fprintf(stderr, "STEER: Emit_header_proxy: socket status is "
	    "connected, index = %d\n", index );
#endif

    /* send header */
    sprintf(buffer, REG_PACKET_FORMAT, REG_DATA_HEADER);
    buffer[REG_PACKET_SIZE - 1] = '\0';
#if REG_DEBUG
    fprintf(stderr, "STEER: Emit_header_proxy: Sending >>%s<<\n", buffer);
#endif
    status = Emit_data_non_blocking_proxy(index, REG_PACKET_SIZE, 
					  (void*) buffer);

    if(status == REG_SUCCESS) {
#if REG_DEBUG
      fprintf(stderr, "STEER: Emit_header_proxy: Sent %d bytes\n", REG_PACKET_SIZE);
#endif
      return REG_SUCCESS;
    }
    else if(status == REG_FAILURE) {
#if REG_DEBUG
      fprintf(stderr, "STEER: Emit_header_proxy: Emit_data_non_blocking_proxy "
	      "failed - immediate retry connect\n");
#endif
      retry_accept_connect(index);

      if(IOTypes_table.io_def[index].socket_info.comms_status == 
	 REG_COMMS_STATUS_CONNECTED) {
#if REG_DEBUG
	fprintf(stderr, "STEER: Emit_header_proxy: Sending >>%s<<\n", buffer);
#endif    
	if(Emit_data_proxy(index, REG_PACKET_SIZE, (void*) buffer) == 
	   REG_SUCCESS) {
	  return REG_SUCCESS;
	}
      }
    }
#if REG_DEBUG
    else{
      fprintf(stderr, "STEER: Emit_header_sockets: attempt to write to "
	      "socket timed out\n");
    }
#endif
  }
#if REG_DEBUG
  else {
    fprintf(stderr, "STEER: Emit_header_proxy: socket not connected, index = %d\n", 
	    index );
  }
#endif

  return REG_FAILURE;
}

/*---------------------------------------------------*/

#endif /* REG_PROXY_SAMPLES */
