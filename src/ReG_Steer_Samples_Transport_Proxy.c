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
    @file ReG_Steer_Sample_Transport_Proxy.c
    @brief Source file for proxy-related routines and data structures
    @author Robert Haines
  */

#include "ReG_Steer_Config.h"
#include "ReG_Steer_Samples_Transport_API.h"
#include "ReG_Steer_Samples_Transport_Proxy.h"
#include "ReG_Steer_Samples_Transport_Sockets.h"
#include "ReG_Steer_Common.h"
#include "ReG_Steer_Sockets_Common.h"
#include "ReG_Steer_Appside_internal.h"
#include "ReG_Steer_Steering_Transport_API.h"

/* */
socket_info_table_type socket_info_table;

/* Need access to these tables which are actually declared in 
   ReG_Steer_Appside_internal.h */
extern IOdef_table_type IOTypes_table;
extern Steerer_connection_table_type Steerer_connection;

/** Global scratch buffer - declared in ReG_Steer_Appside.c */
extern char Global_scratch_buffer[];

/*--------------------- API -------------------------*/

int Initialize_samples_transport() {

#if !REG_HAS_MSG_NOSIGNAL
  signal(SIGPIPE, signal_handler_sockets);
#endif

  return socket_info_table_init(&socket_info_table, IOTypes_table.max_entries);
}

/*---------------------------------------------------*/

int Finalize_samples_transport() {
  return REG_SUCCESS;
}

/*--------------------------------------------------------------------*/

int Initialize_IOType_transport_impl(const int direction, 
				      const int index) {

  int return_status = REG_SUCCESS;
  socket_info_type* socket_info = &(socket_info_table.socket_info[index]);

  /* set up socket info stuff */
  if(socket_info_init(socket_info) != REG_SUCCESS) {
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: Initialize_IOType_transport: failed to init "
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
      if(create_connector_samples(index) != REG_SUCCESS) {
#ifdef REG_DEBUG
	fprintf(stderr, "STEER: Initialize_IOType_transport_sockets: failed to "
		"connect to proxy for IOType\n");
#endif
	return_status = REG_FAILURE;
      }
#ifdef REG_DEBUG
      else {
	fprintf(stderr, "STEER: Initialize_IOType_transport_sockets: "
		"registered connector to proxy on port %d, hostname = %s, "
		"index %d, label %s\n", 
		socket_info->connector_port,
		socket_info->connector_hostname,
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
      if(create_connector_samples(index) != REG_SUCCESS) {
#ifdef REG_DEBUG
	fprintf(stderr, "STEER: Initialize_IOType_transport_sockets: failed to "
		"register connector for IOType\n");
#endif
	return_status = REG_FAILURE;
      }
#ifdef REG_DEBUG
      else {
	fprintf(stderr, "STEER: Initialize_IOType_transport_sockets: "
		"registered connector on port %d, hostname = %s, "
		"index %d, label %s\n", 
		socket_info->connector_port,
		socket_info->connector_hostname,
		index, IOTypes_table.io_def[index].label );
      }
#endif
    }
  }

  return return_status;
}

/*-----------------------------------------------------------*/

void Finalize_IOType_transport_impl() {
  int index;
  fprintf(stderr, "ARPDBG: Finalize_IOType_transport...\n");

  /* For proxy, we only have connectors - no listeners */
  for(index = 0; index < IOTypes_table.num_registered; index++) {
    fprintf(stderr, "ARPDBG: Finalize_IOType_transport index %d\n", index);

    if(IOTypes_table.io_def[index].ack_needed == REG_TRUE || 
       IOTypes_table.io_def[index].consuming == REG_TRUE){

      /* Signal that we have read this data and are ready for
	 the next set */
      fprintf(stderr, "ARPDBG: emitting ack for index %d\n", index);
      Emit_ack_impl(index);
      IOTypes_table.io_def[index].ack_needed = REG_FALSE;
    }

    /* close sockets */
    cleanup_connector_connection_samples(index);
    socket_info_cleanup(&(socket_info_table.socket_info[index]));
  }
}

/*-----------------------------------------------------------*/

int Disable_IOType_impl(const int index) {
  /* check index is valid */
  if(index < 0 || index >= IOTypes_table.num_registered) {
    fprintf(stderr, "STEER: Disable_IOType_sockets: index out of range\n");
    return REG_FAILURE;
  }

  /* close sockets */
  cleanup_connector_connection_samples(index);

  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Enable_IOType_impl(int index) {
  /* check index is valid */
  if(index < 0 || index >= IOTypes_table.num_registered) return REG_FAILURE;

  if (create_connector_samples(index) != REG_SUCCESS) {
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: Enable_IOType_sockets: failed to register "
	    "connector for IOType\n");
#endif
    return REG_FAILURE;
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

int Emit_data_impl(const int index, const size_t size, void* buffer) {

  int   bytes_left;
  int   result;
  char* pchar;
  int   connector = socket_info_table.socket_info[index].connector_handle;
  char *label = IOTypes_table.io_def[index].label;
  char  header[128];

  if(size < 0) {
    fprintf(stderr, "STEER: Emit_data: requested to write < 0 bytes!\n");
    return REG_FAILURE;
  }
  else if(size == 0) {
    fprintf(stderr, "STEER: Emit_data: asked to send 0 bytes!\n");
    return REG_SUCCESS;
  }

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: Emit_data: writing...\n");
#endif

  sprintf(header, "#%s\n%d\n%d\n", label, 1, (int) size);
  bytes_left = strlen(header);
  pchar = header;

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

  bytes_left = size;
  pchar = (char*) buffer;

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
    fprintf(stderr, "STEER: Emit_data: timed-out trying to "
	    "write data\n");
#endif
    return REG_TIMED_OUT;
  }

  /* Check that the IOProxy had a destination for the data ARPDBG */
  result = Consume_proxy_destination_ack(index);

#ifdef REG_DEBUG
  if(result == REG_SUCCESS){
    fprintf(stderr, "STEER: Emit_data: sent %d bytes...\n", (int) size);
  }
#endif

  return result;
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

int Consume_proxy_destination_ack(const int index) {

  int  result;
  int  connector = socket_info_table.socket_info[index].connector_handle;
  char buffer[2];

  result = recv(connector, buffer, (size_t) 2, MSG_WAITALL);
  if(result == -1){
    fprintf(stderr, "STEER: Consume_proxy_destination_ack: check for proxy OK failed\n");
  }
  fprintf(stderr, "ARPDBG: proxy OK returned: %c\n", buffer[0]);
  if(buffer[0] == '1'){
    return REG_SUCCESS;
  }
  else{
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: Consume_proxy_destination_ack: proxy had no "
	    "destination address\n");
#endif
    /* The recipient of our data has disappeared so we won't be getting an
       acknowledgement back from them... */
    IOTypes_table.io_def[index].ack_needed = REG_FALSE;
    return REG_NOT_READY;
  }
}

/*---------------------------------------------------*/

int Emit_header_impl(const int index) {

  char buffer[REG_PACKET_SIZE];
  socket_info_type* socket_info = &(socket_info_table.socket_info[index]);
  int status;

  /* check if socket connection has been made */
  if(socket_info->comms_status !=
     REG_COMMS_STATUS_CONNECTED) {
    attempt_connector_connect_samples(index);
  }

  /* now are we connected? */
  if(socket_info->comms_status == 
     REG_COMMS_STATUS_CONNECTED) {
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: Emit_header: socket status is "
	    "connected, index = %d\n", index );
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
      fprintf(stderr, "STEER: Emit_header: Emit_data_non_blocking "
	      "failed - immediate retry connect\n");
#endif
      retry_accept_connect_samples(index);

      if(socket_info->comms_status == 
	 REG_COMMS_STATUS_CONNECTED) {
#ifdef REG_DEBUG
	fprintf(stderr, "STEER: Emit_header: Sending >>%s<<\n", buffer);
#endif    
	return Emit_data_impl(index, REG_PACKET_SIZE, (void*) buffer);
      }
    }
#ifdef REG_DEBUG
    else if(status == REG_NOT_READY){
    }
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

int Emit_ack_impl(int index){

  /* Send a 16-byte acknowledgement message */
  char *ack_msg = "<ACK/>          ";
  const int size = 16;
  int   bytes_left;
  int   result;
  int   connector = socket_info_table.socket_info[index].connector_handle;
  char  header[REG_MAX_STRING_LENGTH];
  char *label = IOTypes_table.io_def[index].proxySourceLabel;
  char* pchar;

  snprintf(header, REG_MAX_STRING_LENGTH, "#%s_REG_ACK\n%d\n%d\n", 
	   label, 1, size);

  printf("ARPDBG: emitting ack: %s\n", header);
  bytes_left = strlen(header);
  pchar = header;

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

  bytes_left = size;
  pchar = ack_msg;

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
    fprintf(stderr, "STEER: Emit_ack: timed-out trying to write data\n");
#endif
    return REG_TIMED_OUT;
  }

  /* Check that the IOProxy had a destination for the data ARPDBG */
  result = Consume_proxy_destination_ack(index);
  if(result == REG_SUCCESS)printf("ARPDBG: emitted ack OK\n");

  return result;
}

/*---------------------------------------------------*/

int Get_IOType_address_impl(int i, char** pbuf, int* bytes_left) {
  /* This is just a stub because we set the address of the proxy
     in Get_data_io_address_impl(). But only for WSRF steering
     for now...
  */
  return REG_SUCCESS;
}

/*--------------------- Others ----------------------*/

int create_connector_samples(const int index) {

  int i;
  int yes = 1;
  int connector;
  struct sockaddr_in myAddr;

  /* create connector*/
  connector = socket(AF_INET, SOCK_STREAM, 0);
  if(connector == REG_SOCKETS_ERROR) {
    /* problem! */
    perror("socket");
    socket_info_table.socket_info[index].comms_status=REG_COMMS_STATUS_FAILURE;
    return REG_FAILURE;
  }
  /* all okay, so save connector handle */
  socket_info_table.socket_info[index].connector_handle = connector;

  /* ...turn off the "Address already in use" error message... */
  if(setsockopt(connector, SOL_SOCKET, SO_REUSEADDR, &yes, 
		sizeof(int)) == REG_SOCKETS_ERROR) {
    perror("setsockopt");
    return REG_FAILURE;
  }

  /* Turn off NAGLE's algorithm if using an ioProxy so that the (small)
     acknowledgement messages get sent immediately instead of being buffered
     - helps to ensure ack is received before socket is shutdown when consumer
     is shutdown */
  yes = 1;
  if(setsockopt(connector, IPPROTO_TCP, TCP_NODELAY, &yes,
		sizeof(int)) == REG_SOCKETS_ERROR) {
    perror("setsockopt");
    return REG_FAILURE;
  }

  /* ...build local address struct... */
  myAddr.sin_family = AF_INET;
  if(strlen(socket_info_table.socket_info[index].tcp_interface) == 1) {
    myAddr.sin_addr.s_addr = INADDR_ANY;
  }
  else {
    if( !inet_aton(socket_info_table.socket_info[index].tcp_interface, 
		   &(myAddr.sin_addr)) ){
      fprintf(stderr, "STEER: create_connector: inet_aton failed "
	      "for interface >>%s<<\n", 
	      socket_info_table.socket_info[index].tcp_interface);
      return REG_FAILURE;
    }
  }
  memset(&(myAddr.sin_zero), '\0', 8); /* zero the rest */

  /* ...and bind connector so we can punch out of firewalls (if necessary)... */
  if((i = socket_info_table.socket_info[index].min_port_out)) {
    myAddr.sin_port = htons((short) i);

    fprintf(stderr, "STEER: create_connector: using range %d -- %d for bind\n",
	    socket_info_table.socket_info[index].min_port_out,
	    socket_info_table.socket_info[index].max_port_out);

    while(bind(connector, (struct sockaddr*) &myAddr, 
	       sizeof(struct sockaddr)) == REG_SOCKETS_ERROR) {
      if(++i > socket_info_table.socket_info[index].max_port_out) {
	fprintf(stderr, "STEER: create_connector: failed to find free local port to "
		"bind to in range %d -- %d\n",
		socket_info_table.socket_info[index].min_port_out,
		socket_info_table.socket_info[index].max_port_out);
	close(connector);
	socket_info_table.socket_info[index].comms_status=REG_COMMS_STATUS_FAILURE;
	return REG_FAILURE;
      }
      myAddr.sin_port = htons((short) i);
    }
  }
  socket_info_table.socket_info[index].comms_status=REG_COMMS_STATUS_WAITING_TO_CONNECT;

  /* might as well try to connect now... */
  connect_connector_samples(index);

  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int connect_connector_samples(const int index) {

  struct sockaddr_in theirAddr;
  int  connector     = socket_info_table.socket_info[index].connector_handle;
  int  return_status = REG_SUCCESS;
  char tmpBuf[REG_MAX_STRING_LENGTH];
  int i;

  /* get a remote address if we need to */
  if(socket_info_table.socket_info[index].connector_port == 0) {
    return_status =
      Get_data_io_address_impl(IOTypes_table.io_def[index].input_index,
			       IOTypes_table.io_def[index].direction,
			       socket_info_table.socket_info[index].connector_hostname,
			       &(socket_info_table.socket_info[index].connector_port),
			       IOTypes_table.io_def[index].proxySourceLabel);

/* #ifdef REG_DIRECT_TCP_STEERING */
/*     return_status = Get_data_source_address_direct(IOTypes_table.io_def[index].input_index,  */
/* 						   socket_info_table.socket_info[index].connector_hostname, */
/* 						   &(socket_info_table.socket_info[index].connector_port)); */
/* #else */
/* #ifdef REG_SOAP_STEERING	   */
/*     /\* Go out into the world of grid/web services... *\/ */
/* #ifdef REG_WSRF /\* use WSRF *\/ */
/*     if(IOTypes_table.io_def[index].direction == REG_IO_IN){ */
/*       return_status = Get_data_source_address_wsrf(IOTypes_table.io_def[index].input_index,  */
/* 						   socket_info_table.socket_info[index].connector_hostname, */
/* 						   &(socket_info_table.socket_info[index].connector_port), */
/* 						   IOTypes_table.io_def[index].proxySourceLabel); */
/*     } */
/*     else{ */
/*       /\* (We'll only be attempting to connect a connector for an IOType of  */
/* 	 direction REG_IO_OUT when using an IOProxy.) *\/ */
/*       return_status = Get_data_sink_address_wsrf(IOTypes_table.io_def[index].input_index,  */
/* 						 socket_info_table.socket_info[index].connector_hostname, */
/* 						 &(socket_info_table.socket_info[index].connector_port)); */
/*     } */
/* #else /\* use OGSI *\/ */
/*     return_status = Get_data_source_address_soap(IOTypes_table.io_def[index].input_index,  */
/* 						 socket_info_table.socket_info[index].connector_hostname, */
/* 						 &(socket_info_table.socket_info[index].connector_port)); */
/* #endif /\* REG_WSRF *\/ */

/* #else /\* File-based steering *\/ */
/*     /\* get hostname and port from environment variables *\/ */
/*     return_status = Get_data_source_address_file(IOTypes_table.io_def[index].input_index,  */
/* 						 socket_info_table.socket_info[index].connector_hostname, */
/* 						 &(socket_info_table.socket_info[index].connector_port)); */
/* #endif /\* !REG_SOAP_STEERING *\/ */
/* #endif */ /* REG_DIRECT_TCP_STEERING */
  }

  if(return_status == REG_SUCCESS && 
     socket_info_table.socket_info[index].connector_port != 0) {

    /* ...look up and then build remote address struct... */
    if(dns_lookup(socket_info_table.socket_info[index].connector_hostname) == REG_FAILURE) {
      fprintf(stderr, "STEER: connect_connector: Could not resolve hostname <%s>\n", 
	      socket_info_table.socket_info[index].connector_hostname);
      return REG_FAILURE;
    }

    theirAddr.sin_family = AF_INET;
    theirAddr.sin_port = 
             htons(socket_info_table.socket_info[index].connector_port);
    if( !inet_aton(socket_info_table.socket_info[index].connector_hostname, 
		   &(theirAddr.sin_addr)) ){
      fprintf(stderr, 
	      "STEER: connect_connector: inet_aton reports address is invalid\n");
      return REG_FAILURE;
    }
    memset(&(theirAddr.sin_zero), '\0', 8); /* zero the rest */
    
    /* ...finally connect to the remote address! */
    if(connect(connector, (struct sockaddr*) &theirAddr, 
	       sizeof(struct sockaddr)) == REG_SOCKETS_ERROR) {
      perror("connect_connector: connect");
      socket_info_table.socket_info[index].connector_port = 0;
      return REG_FAILURE;
    }
    socket_info_table.socket_info[index].comms_status = 
      REG_COMMS_STATUS_CONNECTED;

    if(IOTypes_table.io_def[index].direction == REG_IO_IN){
      sprintf(Global_scratch_buffer, "%s\n%s\n", 
	      IOTypes_table.io_def[index].label,
	      IOTypes_table.io_def[index].proxySourceLabel);
    }
    else{
      /* If this is an output channel then we subscribe to 
	 acknowledgements but first trim off any trailing
         white space. */
      strncpy(tmpBuf, IOTypes_table.io_def[index].label, 
	      REG_MAX_STRING_LENGTH);
      i = strlen(tmpBuf) - 1;
      while(tmpBuf[i] == ' ' && (i > -1) ){
	tmpBuf[i] = '\0';
	i--;
      }
      sprintf(Global_scratch_buffer, "%s\n%s_REG_ACK\n",
	      tmpBuf, tmpBuf);

      /* If this is an output channel then we aren't subscribing to 
	 any data source
      sprintf(Global_scratch_buffer, "%s\nACKS_ONLY\n",
      IOTypes_table.io_def[index].label); */
    }

    if(Emit_data_impl(index, strlen(Global_scratch_buffer), 
		      (void*)(Global_scratch_buffer)) != REG_SUCCESS) {
      close_connector_handle_samples(index);
      fprintf(stderr, 
	      "STEER: connect_connector: failed to send ID to proxy\n");
      socket_info_table.socket_info[index].connector_port = 0;
      return REG_FAILURE;
    }
  }
  else {
    fprintf(stderr, "STEER: connect_connector: cannot get remote address\n");
  }

  return return_status;  
}

/*---------------------------------------------------*/
