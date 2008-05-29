/*----------------------------------------------------------------------------
  This file contains routines and data structures for direct-tcp
  steering communication.

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
---------------------------------------------------------------------------*/

#if REG_DIRECT_TCP_STEERING

#include "ReG_Steer_Appside_Direct.h"
#include "ReG_Steer_types.h"
#include "ReG_Steer_Common.h"
#include "ReG_Steer_Appside_internal.h"

#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>


/* Allow value of 'REG_DEBUG' to propagate down from Reg_steer_types.h if
   it has been set there */
#ifndef REG_DEBUG
#define REG_DEBUG 1
#endif

/** @internal
   The table holding details of our communication channel with the
   steering client - defined in ReG_Steer_Appside.c */
extern Steerer_connection_table_type Steerer_connection;

/** @internal
   The table holding details of the registered IOTypes for this
   application - defined in ReG_Steer_Appside.c */
extern IOdef_table_type IOTypes_table;

/** @internal Global scratch buffer - declared in ReG_Steer_Appside.c */
extern char Global_scratch_buffer[];

/*-----------------------------------------------------------------------*/

int Initialize_steering_connection_direct(int  NumSupportedCmds,
					  int* SupportedCmds) {

  Direct_info_type* socket_info = &(Steerer_connection.socket_info);

  /* set up socket_info struct */
  if(socket_info_init_direct(socket_info) != REG_SUCCESS) {
#if REG_DEBUG
    fprintf(stderr, "**DIRECT: Initialize_steering_connection_direct: "
	    "could not initialize socket information\n");
#endif
    return REG_FAILURE;
  }

  /* Create msg about supported commands */
  Make_supp_cmds_msg(NumSupportedCmds, SupportedCmds, 
		     Steerer_connection.supp_cmds, REG_MAX_MSG_SIZE);

  /* try to create listener */
  if(Create_steerer_listener(socket_info) != REG_SUCCESS) {
#if REG_DEBUG
    fprintf(stderr, "**DIRECT: Initialize_steering_connection_direct: "
	    "failed to listen for steering connections\n");
#endif
    return REG_FAILURE;
  }
  else {
    fprintf(stderr, "**DIRECT: Initialize_steering_connection_direct: "
	    "registered steering connection on port %d\n", 
	    socket_info->listener_port);
  }

  return REG_SUCCESS;
}

int Create_steerer_listener(Direct_info_type* socket_info) {

  int i;
  int listener;
  int yes = 1;
  struct sockaddr_in myAddr;

  /* create and register listener */
  listener = socket(AF_INET, SOCK_STREAM, 0);
  if(listener == REG_SOCKETS_ERROR) {
    perror("socket");
    socket_info->comms_status = REG_COMMS_STATUS_FAILURE;
    return REG_FAILURE;
  }
  socket_info->listener_handle = listener;

  /* Turn off the "Address already in use" error message */
  if(setsockopt(listener, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int)) == 
     REG_SOCKETS_ERROR) {
    perror("setsockopt");
    return REG_FAILURE;
  }

  /* set up server address */
  myAddr.sin_family = AF_INET;
  if(strlen(socket_info->tcp_interface) == 1) {
    myAddr.sin_addr.s_addr = INADDR_ANY;
  }
  else {
    inet_aton(socket_info->tcp_interface, &(myAddr.sin_addr));
  }
  memset(&(myAddr.sin_zero), '\0', 8); /* zero the rest */

  /* Now bind listener so we can accept connections when they happen */
  i = socket_info->min_port_in;
  myAddr.sin_port = htons((short) i);

  while(bind(listener, (struct sockaddr*) &myAddr, sizeof(struct sockaddr)) == 
	REG_SOCKETS_ERROR) {
    if(++i > socket_info->max_port_in) {
      perror("bind");
      close(listener);
      socket_info->comms_status=REG_COMMS_STATUS_FAILURE;
      return REG_FAILURE;
    }
    myAddr.sin_port = htons((short) i);
  }
  /* we're bound, so save the port number we're using */
  socket_info->listener_port = i;

  /* now we need to actually listen */
  if(listen(listener, 10) == REG_SOCKETS_ERROR) {
    perror("listen");
    close(listener);
    socket_info->comms_status = REG_COMMS_STATUS_FAILURE;
    socket_info->listener_status = REG_COMMS_STATUS_FAILURE;
    return REG_FAILURE;
  }

  /* we are listening! */
  socket_info->listener_status = REG_COMMS_STATUS_LISTENING;
  socket_info->comms_status = REG_COMMS_STATUS_LISTENING;

  return REG_SUCCESS;
}

/*-----------------------------------------------------------------------*/

int Steerer_connected_direct() {

  Direct_info_type* socket_info = &(Steerer_connection.socket_info);

  /* If already connected then just return a "yes" */  
  if(socket_info->comms_status == REG_COMMS_STATUS_CONNECTED) {
    return REG_SUCCESS;
  }

  /* If not connected, see if something is trying to connect */
  if(socket_info->listener_status != REG_COMMS_STATUS_LISTENING) {
#if REG_DEBUG
    fprintf(stderr, "**DIRECT: Steerer_connected_direct: "
	    "dealing with listener_status not LISTENING \n");
#endif
    if (socket_info->listener_status == REG_COMMS_STATUS_FAILURE ||
	socket_info->listener_status == REG_COMMS_STATUS_NULL ) {
      Create_steerer_listener(socket_info);
    }
  }

  /* only check comms_status if listener_status is now listening */
  if (socket_info->listener_status == REG_COMMS_STATUS_LISTENING) {
    if (socket_info->comms_status == REG_COMMS_STATUS_LISTENING) {
#if REG_DEBUG
      fprintf(stderr, "**DIRECT: Steerer_connected_direct: poll_socket\n");
#endif
      poll_steerer_socket(socket_info);
    }

    /* if we are now connected, then send first message */
    if(socket_info->comms_status == REG_COMMS_STATUS_CONNECTED) {
      /* send first message here */
      send_steerer_msg(socket_info, (strlen(Steerer_connection.supp_cmds) + 1), Steerer_connection.supp_cmds);
      return REG_SUCCESS;
    }
  }

  /* if we get here, then we're not connected */
#if REG_DEBUG
      fprintf(stderr, "**DIRECT: Steerer_connected_direct: not connected\n");
#endif
  return REG_FAILURE;
}

/*-----------------------------------------------------------------------*/

int Detach_from_steerer_direct() {

  Direct_info_type* socket_info = &(Steerer_connection.socket_info);

  /* steerer will detatch FROM us so need to clean up socket, etc here */

  if(socket_info->listener_status = REG_COMMS_STATUS_LISTENING) {
    close_listener_direct(socket_info);
  }

  if(socket_info->comms_status = REG_COMMS_STATUS_CONNECTED) {
    close_connector_direct(socket_info);
  }

  return REG_SUCCESS;
}

/*--------------------------------------------------------------------*/

int Finalize_steering_connection_direct() {

  Direct_info_type* socket_info = &(Steerer_connection.socket_info);

  if(socket_info->listener_status = REG_COMMS_STATUS_LISTENING) {
    close_listener_direct(socket_info);
  }

  if(socket_info->comms_status = REG_COMMS_STATUS_CONNECTED) {
    close_connector_direct(socket_info);
  }

  return REG_SUCCESS;
}

/*--------------------------------------------------------------------*/

void poll_steerer_socket(Direct_info_type* socket_info) {

  struct timeval timeout;
  fd_set sockets;	/* the set of sockets to check */
  int fd_max;		/* the max socket number */

  int listener = socket_info->listener_handle;
  int connector = socket_info->connector_handle;

  timeout.tv_sec  = 0;
  timeout.tv_usec = 0;

  /* just return if we have no handles */
  if((listener == -1) && (connector == -1)) return;

  /* clear the socket set and add required handles */
  FD_ZERO(&sockets);

  if(socket_info->listener_status == REG_COMMS_STATUS_LISTENING) {
    FD_SET(listener, &sockets);
    fd_max = listener;

    /* poll using select() */
#if REG_DEBUG
    fprintf(stderr, "**DIRECT: poll_socket: polling for accept\n");
#endif
    if(select(fd_max + 1, &sockets, NULL, NULL, &timeout) == -1) {
      perror("select");
      return;
    }

    /* see if anything needs doing */
    if(FD_ISSET(listener, &sockets)) {
      /* new connection */
      struct sockaddr_in theirAddr;
#if defined(__sgi)
      socklen_t addrlen = sizeof(theirAddr);
#elif defined(TRU64)
      int addrlen = sizeof(theirAddr);
#else
      unsigned int addrlen = sizeof(theirAddr);
#endif
      int new_fd = accept(listener, (struct sockaddr*) &theirAddr, &addrlen);
      if(new_fd == REG_SOCKETS_ERROR) {
	perror("accept");
	return;
      }
      socket_info->connector_handle = new_fd;
      socket_info->comms_status=REG_COMMS_STATUS_CONNECTED;
    }
  }
}

/*--------------------------------------------------------------------*/

int send_steerer_msg(Direct_info_type* socket_info,
		     const size_t num_bytes_to_send, 
		     void*        pData) {
  int bytes_left;
  int result;
  char* pchar;
  int connector = socket_info->connector_handle;

  if(num_bytes_to_send < 0) {
    fprintf(stderr, "**DIRECT: send_steerer_msg: requested to write < 0 bytes!\n");
    return REG_FAILURE;
  }
  else if(num_bytes_to_send == 0) {
    fprintf(stderr, "**DIRECT: send_steerer_msg: asked to send 0 bytes!\n");
    return REG_SUCCESS;
  }

  /* cast void pointer */
  pchar = (char*) pData;

  /* send header with just an int saying how much data to come */
#if REG_DEBUG
  fprintf(stderr, "**DIRECT: send_steerer_msg: sending header...\n");
#endif
#ifndef __linux
  result = send(connector, (void*) &num_bytes_to_send, sizeof(int), 0);
#else
  result = send(connector, (void*) &num_bytes_to_send, sizeof(int), MSG_NOSIGNAL);
#endif
  if(result != sizeof(int)) {
    perror("send");
    return REG_FAILURE;
  }

  bytes_left = num_bytes_to_send;

#if REG_DEBUG
  fprintf(stderr, "**DIRECT: send_steerer_msg: writing...\n");
#endif
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
    fprintf(stderr, "**DIRECT: send_steerer_msg: timed-out trying to write data\n");
#endif
    return REG_TIMED_OUT;
  }

#if REG_DEBUG
  fprintf(stderr, "**DIRECT: send_steerer_msg: sent %d bytes...\n", 
	  (int) num_bytes_to_send);
#endif
  
  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Send_status_msg_direct(Direct_info_type* socket_info, char *buf) {

  int buf_len;

  buf_len = strlen(buf) + 1; /* +1 for \0 */
  if(send_steerer_msg(socket_info, buf_len, buf) == REG_FAILURE) {
    fprintf(stderr, "**DIRECT: Send_status_msg_direct: failed to send\n");
    
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*---------------------------------------------------*/

struct msg_struct *Get_control_msg_direct(Direct_info_type* socket_info) {

  struct msg_struct* msg = NULL;
  int return_status;
  char data[REG_MAX_MSG_SIZE];

  /* will this block? we never want it to! */
  if(poll_msg_direct(socket_info->connector_handle) == REG_FAILURE) return NULL;

  if(Consume_steerer_msg(socket_info, data) == REG_SUCCESS) {

    msg = New_msg_struct();

    /* Pass NULL down here as this is app-side and we have no ptr to
       a Sim_entry struct */
    return_status = Parse_xml_buf(data, strlen(data), msg, NULL);

    if(return_status != REG_SUCCESS) {
      Delete_msg_struct(&msg);
    }
  }

  return msg;
}

/*-----------------------------------------------------------------------*/

int Get_data_source_address_direct(int                 dummy,
				   char*               hostname,
				   unsigned short int* port)
{
  char* pchar;
  int   len;

  /* Return port = 0 on failure */
  *port = 0;

  /* Get hostname and port from environment variables */

  pchar = getenv("REG_CONNECTOR_HOSTNAME");
  if (pchar) {
    len = strlen(pchar);
    if (len < REG_MAX_STRING_LENGTH) {
      sprintf(hostname, pchar);
    }
    else{
      fprintf(stderr, "**DIRECT: Get_data_source_address_direct: content of "
	      "REG_CONNECTOR_HOSTNAME exceeds max. string length of "
	      "%d chars\n", REG_MAX_STRING_LENGTH);
      return REG_FAILURE;
    }
  }
  else{
    fprintf(stderr, 
	    "**DIRECT: Get_data_source_address_direct: REG_CONNECTOR_HOSTNAME not set\n");
    return REG_FAILURE;
  }

  pchar = getenv("REG_CONNECTOR_PORT");
  if (pchar) {
    *port =  (unsigned short int)atoi(pchar);
  }
  else{
    fprintf(stderr, 
	    "**DIRECT: Get_data_source_address_direct: REG_CONNECTOR_PORT not set\n");
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int socket_info_init_direct(Direct_info_type* socket_info) {

  char* pchar = NULL;
  char* ip_addr;
  int   min, max;

  /* lazy initial port ranges, but they do for now */
  socket_info->min_port_in = 21370;
  socket_info->max_port_in = 65535;
  socket_info->min_port_out = 21370;
  socket_info->max_port_out = 65535;

  /* get a port range to listen on */
  pchar = getenv("GLOBUS_TCP_PORT_RANGE");

  if( pchar && (sscanf(pchar, "%d,%d", &(min), &(max)) == 2) ) {
    socket_info->min_port_in = min;
    socket_info->max_port_in = max;
  }

  /* get a port range to connect out of */
  pchar = getenv("GLOBUS_TCP_SOURCE_RANGE");

  if( pchar && (sscanf(pchar, "%d,%d", &(min), &(max)) == 2) ) {
    socket_info->min_port_out = min;
    socket_info->max_port_out = max;
  }

  /* set local TCP interface to use */
  if(Get_fully_qualified_hostname(&pchar, &ip_addr) == REG_SUCCESS) {
    strcpy(socket_info->tcp_interface, ip_addr);
  }
  else {
    sprintf(socket_info->tcp_interface, " ");
  }

#if REG_DEBUG
  fprintf(stderr, "**DIRECT: socket_info_init_direct: port range to listen on %d - %d\n", 
	  socket_info->min_port_in,
	  socket_info->max_port_in);
  fprintf(stderr, "**DIRECT: socket_info_init_direct: port range to connect out of %d - %d\n", 
	  socket_info->min_port_out,
	  socket_info->max_port_out);
  fprintf(stderr, "**DIRECT: socket_info_init_direct: local tcp interface: %s\n", 
	  socket_info->tcp_interface);
#endif

  socket_info->listener_port = 0;
  socket_info->listener_handle = -1;
  socket_info->listener_status = REG_COMMS_STATUS_NULL;
  socket_info->comms_status = REG_COMMS_STATUS_NULL;   
  socket_info->connector_port = 0;
  socket_info->connector_handle = -1;
  sprintf(socket_info->connector_hostname, " ");

  return REG_SUCCESS;
}

#endif /* REG_DIRECT_TCP_STEERING */
