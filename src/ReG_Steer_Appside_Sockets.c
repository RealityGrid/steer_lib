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
    @file ReG_Steer_Appside_Sockets.c
    @brief Source file for socket-related routines and data structures
    @author Andrew Porter
    @author Robert Haines
  */

#if REG_SOCKET_SAMPLES || REG_PROXY_SAMPLES || defined(DOXYGEN)

#include "ReG_Steer_Common.h"
#include "ReG_Steer_Appside_internal.h"
#include "ReG_Steer_Appside_Sockets.h"
#include "ReG_Steer_Appside_File.h"
#include "ReG_Steer_Appside_Soap.h"
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

/** Global scratch buffer - declared in ReG_Steer_Appside.c */
extern char Global_scratch_buffer[];

/*--------------------------------------------------------------------*/

int socket_info_init(const int index) {

  char* pchar = NULL;
  char* ip_addr;
  int   min, max;
  IOTypes_table.io_def[index].socket_info.min_port = 0;
  IOTypes_table.io_def[index].socket_info.max_port = 0;

#if REG_PROXY_SAMPLES
  /* We always connect OUT to the IOProxy */
  pchar = getenv("GLOBUS_TCP_SOURCE_RANGE");

#elif REG_SOCKET_SAMPLES

  if(IOTypes_table.io_def[index].direction == REG_IO_OUT){
    pchar = getenv("GLOBUS_TCP_PORT_RANGE");
    /* We want to do a bind if we're going to have to listen so we
       know where which port we're listening on - ARP being lazy */
    IOTypes_table.io_def[index].socket_info.min_port = 21370;
    IOTypes_table.io_def[index].socket_info.max_port = 65535;
  }
  else{
    /* We only connect outwards if the IOType 
       is of REG_IO_IN direction */
    pchar = getenv("GLOBUS_TCP_SOURCE_RANGE");
  }

#endif

  if( pchar && (sscanf(pchar, "%d,%d", &(min), &(max)) == 2) ) {
    IOTypes_table.io_def[index].socket_info.min_port = min;
    IOTypes_table.io_def[index].socket_info.max_port = max;
  }

#if REG_DEBUG
  fprintf(stderr, "STEER: socket_info_init: port range %d - %d\n", 
	  IOTypes_table.io_def[index].socket_info.min_port,
	  IOTypes_table.io_def[index].socket_info.max_port);
#endif

  /* set local TCP interface to use */
  if(Get_fully_qualified_hostname(&pchar, &ip_addr) == REG_SUCCESS) {
    strcpy(IOTypes_table.io_def[index].socket_info.tcp_interface, ip_addr);
  }
  else {
    sprintf(IOTypes_table.io_def[index].socket_info.tcp_interface, " ");
  }

#if REG_DEBUG
  fprintf(stderr, "STEER: socket_info_init: local tcp interface: %s\n", 
	  IOTypes_table.io_def[index].socket_info.tcp_interface);
#endif

  IOTypes_table.io_def[index].socket_info.listener_port = 0;
  IOTypes_table.io_def[index].socket_info.listener_handle = -1;
  IOTypes_table.io_def[index].socket_info.listener_status = REG_COMMS_STATUS_NULL;
  IOTypes_table.io_def[index].socket_info.comms_status = REG_COMMS_STATUS_NULL;   
  IOTypes_table.io_def[index].socket_info.connector_port = 0;
  IOTypes_table.io_def[index].socket_info.connector_handle = -1;
  sprintf(IOTypes_table.io_def[index].socket_info.connector_hostname, " ");

  return REG_SUCCESS;
}

/*--------------------------------------------------------------------*/

void socket_info_cleanup(const int index) {

}

/*--------------------------------------------------------------------*/

int create_listener(const int index) {

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
    IOTypes_table.io_def[index].socket_info.comms_status=REG_COMMS_STATUS_FAILURE;
    return REG_FAILURE;
  }
  IOTypes_table.io_def[index].socket_info.listener_handle = listener;

  /* Turn off the "Address already in use" error message */
  if(setsockopt(listener, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int)) == 
     REG_SOCKETS_ERROR) {
    perror("setsockopt");
    return REG_FAILURE;
  }

  /* Get the hostname of the machine we're running on (so we can publish
   * the endpoint of this connection). If REG_IO_ADDRESS is set then
   * that's what we use.  If not, call Get_fully_qualified_hostname which 
   * itself uses  REG_TCP_INTERFACE if set. */
  if( (pchar = getenv("REG_IO_ADDRESS")) ){
#if REG_DEBUG
    fprintf(stderr, "STEER: create_listener: Taking hostname from "
	    "REG_IO_ADDRESS variable\n");
#endif
    strcpy(IOTypes_table.io_def[index].socket_info.listener_hostname, pchar);
  }
  else if( (pchar = getenv("GLOBUS_HOSTNAME")) ){
#if REG_DEBUG
    fprintf(stderr, "STEER: create_listener: Taking hostname from "
	    "GLOBUS_HOSTNAME variable\n");
#endif
    strcpy(IOTypes_table.io_def[index].socket_info.listener_hostname, pchar);
  }
  else if(Get_fully_qualified_hostname(&pchar, &ip_addr) == REG_SUCCESS){
    strcpy(IOTypes_table.io_def[index].socket_info.listener_hostname, pchar);
  }
  else{
    fprintf(stderr, "STEER: WARNING: Sockets_create_listener: failed to get hostname\n");
    sprintf(IOTypes_table.io_def[index].socket_info.listener_hostname, 
	    "NOT_SET");
  }
  
  /* set up server address */
  myAddr.sin_family = AF_INET;
  if(strlen(IOTypes_table.io_def[index].socket_info.tcp_interface) == 1) {
    myAddr.sin_addr.s_addr = INADDR_ANY;
  }
  else {
    inet_aton(IOTypes_table.io_def[index].socket_info.tcp_interface, 
	      &(myAddr.sin_addr));
  }
  memset(&(myAddr.sin_zero), '\0', 8); /* zero the rest */

  /* Now bind listener so we can accept connections when they happen */
  i = IOTypes_table.io_def[index].socket_info.min_port;
  myAddr.sin_port = htons((short) i);

  while(bind(listener, (struct sockaddr*) &myAddr, sizeof(struct sockaddr)) == 
	REG_SOCKETS_ERROR) {
    if(++i > IOTypes_table.io_def[index].socket_info.max_port) {
      perror("bind");
      close(listener);
      IOTypes_table.io_def[index].socket_info.comms_status=REG_COMMS_STATUS_FAILURE;
      return REG_FAILURE;
    }
    myAddr.sin_port = htons((short) i);
  }
  /* we're bound, so save the port number we're using */
  IOTypes_table.io_def[index].socket_info.listener_port = i;

  /* now we need to actually listen */
  if(listen(listener, 10) == REG_SOCKETS_ERROR) {
    perror("listen");
    close(listener);
    IOTypes_table.io_def[index].socket_info.comms_status = REG_COMMS_STATUS_FAILURE;
    IOTypes_table.io_def[index].socket_info.listener_status = REG_COMMS_STATUS_FAILURE;
    return REG_FAILURE;
  }

  /* we are listening! */
  IOTypes_table.io_def[index].socket_info.listener_status = REG_COMMS_STATUS_LISTENING;
  IOTypes_table.io_def[index].socket_info.comms_status = REG_COMMS_STATUS_LISTENING;

  return REG_SUCCESS;
}

/*--------------------------------------------------------------------*/

int create_connector(const int index) {

  int i;
  int yes = 1;
  int connector;
  struct sockaddr_in myAddr;

  /* create connector*/
  connector = socket(AF_INET, SOCK_STREAM, 0);
  if(connector == REG_SOCKETS_ERROR) {
    /* problem! */
    perror("socket");
    IOTypes_table.io_def[index].socket_info.comms_status=REG_COMMS_STATUS_FAILURE;
    return REG_FAILURE;
  }
  /* all okay, so save connector handle */
  IOTypes_table.io_def[index].socket_info.connector_handle = connector;

  /* ...turn off the "Address already in use" error message... */
  if(setsockopt(connector, SOL_SOCKET, SO_REUSEADDR, &yes, 
		sizeof(int)) == REG_SOCKETS_ERROR) {
    perror("setsockopt");
    return REG_FAILURE;
  }

#if REG_PROXY_SAMPLES
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
#endif /* REG_PROXY_SAMPLES */

  /* ...build local address struct... */
  myAddr.sin_family = AF_INET;
  if(strlen(IOTypes_table.io_def[index].socket_info.tcp_interface) == 1) {
    myAddr.sin_addr.s_addr = INADDR_ANY;
  }
  else {
    if( !inet_aton(IOTypes_table.io_def[index].socket_info.tcp_interface, 
		   &(myAddr.sin_addr)) ){
      fprintf(stderr, "STEER: create_connector: inet_aton failed "
	      "for interface >>%s<<\n", 
	      IOTypes_table.io_def[index].socket_info.tcp_interface);
      return REG_FAILURE;
    }
  }
  memset(&(myAddr.sin_zero), '\0', 8); /* zero the rest */

  /* ...and bind connector so we can punch out of firewalls (if necessary)... */
  if( (i = IOTypes_table.io_def[index].socket_info.min_port) ){
    myAddr.sin_port = htons((short) i);

    fprintf(stderr, "STEER: create_connector: using range %d -- %d for bind\n",
	    IOTypes_table.io_def[index].socket_info.min_port,
	    IOTypes_table.io_def[index].socket_info.max_port);

    while(bind(connector, (struct sockaddr*) &myAddr, 
	       sizeof(struct sockaddr)) == REG_SOCKETS_ERROR) {
      if(++i > IOTypes_table.io_def[index].socket_info.max_port) {
	fprintf(stderr, "STEER: create_connector: failed to find free local port to "
		"bind to in range %d -- %d\n",
		IOTypes_table.io_def[index].socket_info.min_port,
		IOTypes_table.io_def[index].socket_info.max_port);
	close(connector);
	IOTypes_table.io_def[index].socket_info.comms_status=REG_COMMS_STATUS_FAILURE;
	return REG_FAILURE;
      }
      myAddr.sin_port = htons((short) i);
    }
  }
  IOTypes_table.io_def[index].socket_info.comms_status=REG_COMMS_STATUS_WAITING_TO_CONNECT;

  /* might as well try to connect now... */
  connect_connector(index);

  return REG_SUCCESS;
}

/*--------------------------------------------------------------------*/

int connect_connector(const int index) {

  struct sockaddr_in theirAddr;
  int  connector     = IOTypes_table.io_def[index].socket_info.connector_handle;
  int  return_status = REG_SUCCESS;
  char tmpBuf[REG_MAX_STRING_LENGTH];
  int  i;

  /* get a remote address if we need to */
  if(IOTypes_table.io_def[index].socket_info.connector_port == 0) {
#if REG_SOAP_STEERING	  
    /* Go out into the world of grid/web services... */
#ifdef REG_WSRF /* use WSRF */
    if(IOTypes_table.io_def[index].direction == REG_IO_IN){
      return_status = Get_data_source_address_wsrf(IOTypes_table.io_def[index].input_index, 
						   IOTypes_table.io_def[index].socket_info.connector_hostname,
						   &(IOTypes_table.io_def[index].socket_info.connector_port),
						   IOTypes_table.io_def[index].proxySourceLabel);
    }
    else{
      /* (We'll only be attempting to connect a connector for an IOType of 
	 direction REG_IO_OUT when using an IOProxy.) */
      return_status = Get_data_sink_address_wsrf(IOTypes_table.io_def[index].input_index, 
						 IOTypes_table.io_def[index].socket_info.connector_hostname,
						 &(IOTypes_table.io_def[index].socket_info.connector_port));
    }
#else /* use OGSI */
    return_status = Get_data_source_address_soap(IOTypes_table.io_def[index].input_index, 
						 IOTypes_table.io_def[index].socket_info.connector_hostname,
						 &(IOTypes_table.io_def[index].socket_info.connector_port));
#endif /* REG_WSRF */

#else /* File-based steering */
    /* get hostname and port from environment variables */
    return_status = Get_data_source_address_file(IOTypes_table.io_def[index].input_index, 
						 IOTypes_table.io_def[index].socket_info.connector_hostname,
						 &(IOTypes_table.io_def[index].socket_info.connector_port));
#endif /* !REG_SOAP_STEERING */
  }

  if(return_status == REG_SUCCESS && 
     IOTypes_table.io_def[index].socket_info.connector_port != 0) {

    /* ...look up and then build remote address struct... */
    if(dns_lookup(IOTypes_table.io_def[index].socket_info.connector_hostname) == REG_FAILURE) {
      fprintf(stderr, "STEER: connect_connector: Could not resolve hostname <%s>\n", 
	      IOTypes_table.io_def[index].socket_info.connector_hostname);
      return REG_FAILURE;
    }

    theirAddr.sin_family = AF_INET;
    theirAddr.sin_port = 
             htons(IOTypes_table.io_def[index].socket_info.connector_port);
    if( !inet_aton(IOTypes_table.io_def[index].socket_info.connector_hostname, 
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
      IOTypes_table.io_def[index].socket_info.connector_port = 0;
      return REG_FAILURE;
    }
    IOTypes_table.io_def[index].socket_info.comms_status = 
      REG_COMMS_STATUS_CONNECTED;

#if REG_PROXY_SAMPLES
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

    if(Emit_data_sockets(index, 
			 strlen(Global_scratch_buffer), 
			 (void*)(Global_scratch_buffer)) !=
       REG_SUCCESS){

      close_connector_handle(index);
      fprintf(stderr, 
	      "STEER: connect_connector: failed to send ID to proxy\n");
      IOTypes_table.io_def[index].socket_info.connector_port = 0;
      return REG_FAILURE;
    }
#endif
  }
  else {
    fprintf(stderr, "STEER: connect_connector: cannot get remote address\n");
  }

  return return_status;  
}

/*--------------------------------------------------------------------*/

void cleanup_listener_connection(const int index) {
  if(IOTypes_table.io_def[index].socket_info.listener_status == REG_COMMS_STATUS_LISTENING) {
    close_listener_handle(index);
  }

  if(IOTypes_table.io_def[index].socket_info.comms_status == REG_COMMS_STATUS_CONNECTED) { 
    close_connector_handle(index);
  }

  /* Flag that this listener is dead - used in Emit_IOType_defs */
  sprintf(IOTypes_table.io_def[index].socket_info.listener_hostname, "NOT_SET");
  IOTypes_table.io_def[index].socket_info.listener_port = 0;
}

/*--------------------------------------------------------------------*/

void cleanup_connector_connection(const int index) {
  if (IOTypes_table.io_def[index].socket_info.comms_status == REG_COMMS_STATUS_CONNECTED ||
      IOTypes_table.io_def[index].socket_info.comms_status == REG_COMMS_STATUS_WAITING_TO_CONNECT ){
    close_connector_handle(index);
  }
}

/*--------------------------------------------------------------------*/

void close_listener_handle(const int index) {
  if(close(IOTypes_table.io_def[index].socket_info.listener_handle) == REG_SOCKETS_ERROR) {
    perror("close");
    IOTypes_table.io_def[index].socket_info.listener_status = REG_COMMS_STATUS_FAILURE;
  }
  else {
#if REG_DEBUG
    fprintf(stderr, "STEER: close_listener_handle: close OK\n");
#endif
    IOTypes_table.io_def[index].socket_info.listener_status = REG_COMMS_STATUS_NULL;
  }
}

/*--------------------------------------------------------------------*/

void close_connector_handle(const int index) {
  if(close(IOTypes_table.io_def[index].socket_info.connector_handle) == REG_SOCKETS_ERROR) {
    perror("close");
    IOTypes_table.io_def[index].socket_info.comms_status = REG_COMMS_STATUS_FAILURE;
  }
  else {
#if REG_DEBUG
    fprintf(stderr, "STEER: close_connector_handle: close OK\n");
#endif
    IOTypes_table.io_def[index].socket_info.comms_status = REG_COMMS_STATUS_NULL;
  }
}

/*--------------------------------------------------------------------*/

void attempt_listener_connect(const int index) {
  socket_io_type *socket_info;
  socket_info = &(IOTypes_table.io_def[index].socket_info);

  if(socket_info->listener_status != REG_COMMS_STATUS_LISTENING) {
#if REG_DEBUG
    fprintf(stderr, "STEER: attempt_listener_connect:dealing with listener_status not LISTENING \n");
#endif
    if (socket_info->listener_status == REG_COMMS_STATUS_FAILURE ||
	socket_info->listener_status == REG_COMMS_STATUS_NULL ) {
      create_listener(index);
    }
  }

  /* only check comms_status if listener_status is now listening */
  if (socket_info->listener_status == REG_COMMS_STATUS_LISTENING) {
    if (socket_info->comms_status == REG_COMMS_STATUS_LISTENING) {
#if REG_DEBUG
      fprintf(stderr, "STEER: attempt_listener_connect: poll_socket\n");
#endif
      poll_socket(index);
    }

    if (socket_info->comms_status == REG_COMMS_STATUS_FAILURE ||
	socket_info->comms_status == REG_COMMS_STATUS_NULL ) {
      /* connection has broken - we're still listening so see if 
         anything to connect */
#if REG_DEBUG
      fprintf(stderr, "STEER: attempt_listener_connect: retry accept connect\n");
#endif
      retry_accept_connect(index);
    }
  }
}

/*--------------------------------------------------------------------*/

void retry_accept_connect(const int index) {
  socket_io_type *socket_info;
  socket_info = &(IOTypes_table.io_def[index].socket_info);

  /* if this is called, a write has failed */

  /* if we're here and  status is connected  we've lost a connection, 
     so first close socket */
  if (socket_info->comms_status==REG_COMMS_STATUS_CONNECTED) {
    close_connector_handle(index);
  }

  poll_socket(index);
}

/*--------------------------------------------------------------------*/

void attempt_connector_connect(const int index) {
  socket_io_type *socket_info;
  socket_info = &(IOTypes_table.io_def[index].socket_info);

  if(socket_info->comms_status == REG_COMMS_STATUS_WAITING_TO_CONNECT) {
#if REG_DEBUG    
    fprintf(stderr, "STEER: attempt_connector_connect: poll_socket\n");
#endif
    poll_socket(index);
  }

  if(socket_info->comms_status == REG_COMMS_STATUS_FAILURE || 
     socket_info->comms_status == REG_COMMS_STATUS_NULL) {
    /* connection has broken - try to re-connect */
#if REG_DEBUG
    fprintf(stderr, "STEER: attempt_connector_connect: retry connect\n");
#endif
    retry_connect(index);
  }
}

/*--------------------------------------------------------------------*/

void retry_connect(const int index) {
  socket_io_type *socket_info;
  socket_info = &(IOTypes_table.io_def[index].socket_info);

  /* close the failed connector and retry to connect */
  if(socket_info->comms_status == REG_COMMS_STATUS_CONNECTED) {
    /* Reset connector port (to force us to go and look for it
       again in case it has changed) */
    socket_info->connector_port = 0;
    close_connector_handle(index);
  }

  if(create_connector(index) != REG_SUCCESS) {
#if REG_DEBUG
    fprintf(stderr, "STEER: retry_connect: failed to register connector for IOType\n");
#endif
    /* Set to FAILURE so create_connector is attempted again next time round */
    socket_info->comms_status=REG_COMMS_STATUS_FAILURE;
  }
}

/*--------------------------------------------------------------------*/

void poll_socket(const int index) {

  struct timeval timeout;
  fd_set sockets;	/* the set of sockets to check */
  int fd_max;		/* the max socket number */

  int listener = IOTypes_table.io_def[index].socket_info.listener_handle;
  int connector = IOTypes_table.io_def[index].socket_info.connector_handle;
  int direction = IOTypes_table.io_def[index].direction;

  timeout.tv_sec  = 0;
  timeout.tv_usec = 0;

  /* just return if we have no handles */
  if((listener == -1) && (connector == -1)) return;

  /* clear the socket set and add required handles */
  FD_ZERO(&sockets);
  if(direction == REG_IO_OUT) {
    /* SERVER */
    if(IOTypes_table.io_def[index].socket_info.listener_status == REG_COMMS_STATUS_LISTENING) {
      FD_SET(listener, &sockets);
      fd_max = listener;

      /* poll using select() */
#if REG_DEBUG
      fprintf(stderr, "STEER: poll_socket: polling for accept\n");
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
	IOTypes_table.io_def[index].socket_info.connector_handle = new_fd;
	IOTypes_table.io_def[index].socket_info.comms_status=REG_COMMS_STATUS_CONNECTED;
      }
    }
  }
  else if(direction == REG_IO_IN) {
    /* CLIENT */
    if(IOTypes_table.io_def[index].socket_info.comms_status != REG_COMMS_STATUS_CONNECTED) {
      connect_connector(index);
    }
  }
}

/*--------------------------------------------------------------------*/

int dns_lookup(char* hostname) {
  struct hostent* host;

  if((host = gethostbyname(hostname)) == NULL) {
    herror("gethostbyname");
    return REG_FAILURE;
  }

#if REG_DEBUG
  fprintf(stderr, "STEER: DNS lookup: host: %s\n", host->h_name);
  fprintf(stderr, "               IP:   %s\n", 
	  inet_ntoa(*((struct in_addr*) host->h_addr)));
#endif

  /* This next bit must be done with a sprintf for AIX...
   * It can be done with a strcpy on Linux or IRIX...      */
  sprintf(hostname, "%s", 
	  inet_ntoa(*((struct in_addr*) host->h_addr_list[0])));

  return REG_SUCCESS;
}

/*--------------------------------------------------------------------*/

int recv_non_block(socket_io_type  *sock_info,
		   char *pbuf, int nbytes){

  int nbytes_read = 0;

#if defined(_AIX) || defined(TRU64)

  /* So it looks like AIX blocks by default... So make the socket
   * non-blocking as we can't control this with flags to recv() in AIX... */
  fcntl(sock_info->connector_handle, F_SETFL, 
	fcntl(sock_info->connector_handle, F_GETFL)|O_NONBLOCK);

  nbytes_read = recv(sock_info->connector_handle, pbuf, nbytes, 0);

  /* ...And turn off non-blocking again... */
  fcntl(sock_info->connector_handle, F_SETFL, 
	fcntl(sock_info->connector_handle, F_GETFL)&~O_NONBLOCK);

#else

  nbytes_read = recv(sock_info->connector_handle, pbuf, 
		     nbytes, MSG_DONTWAIT);
#endif
  
  return nbytes_read;
}

/*--------------------------------------------------------------------*
 *                         EXTERNAL METHODS                           *
 *--------------------------------------------------------------------*/

int Initialize_IOType_transport_sockets(const int direction, const int index) {

  int return_status = REG_SUCCESS;

  /* set up socket info stuff */
  if(socket_info_init(index) != REG_SUCCESS) {
#if REG_DEBUG
    fprintf(stderr, "STEER: Initialize_IOType_transport_sockets: failed to "
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
#if REG_DEBUG
	fprintf(stderr, "STEER: Initialize_IOType_transport_sockets: failed to "
		"create listener for IOType\n");
#endif
	return_status = REG_FAILURE;
      }
      else {
	fprintf(stderr, "STEER: Initialize_IOType_transport_sockets: Created "
		"listener on port %d, index %d, label %s\n", 
		IOTypes_table.io_def[index].socket_info.listener_port, 
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

/*---------------------------------------------------*/

void Finalize_IOType_transport_sockets() {
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

int Disable_IOType_sockets(const int index) {
  /* check index is valid */
  if(index < 0 || index >= IOTypes_table.num_registered) {
    fprintf(stderr, "STEER: Disable_IOType_sockets: index out of range\n");
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

int Enable_IOType_sockets(int index) {
  /* check index is valid */
  if(index < 0 || index >= IOTypes_table.num_registered) return REG_FAILURE;

  if(IOTypes_table.io_def[index].direction == REG_IO_OUT) {
    /* open socket and register callback function to listen for and
       accept connections */
    if(create_listener(index) != REG_SUCCESS) {
#if REG_DEBUG
      fprintf(stderr, "STEER: Enable_IOType_sockets: failed to create listener for IOType\n");
#endif
      return REG_FAILURE;
    }
  }
  else if(IOTypes_table.io_def[index].direction == REG_IO_IN) {
    if (create_connector(index) != REG_SUCCESS) {
#if REG_DEBUG
      fprintf(stderr, "STEER: Enable_IOType_sockets: failed to register "
	      "connector for IOType\n");
#endif
      return REG_FAILURE;
    }
  }

  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Get_communication_status_sockets(const int index) {
  if(IOTypes_table.io_def[index].socket_info.comms_status != 
     REG_COMMS_STATUS_CONNECTED)
    return REG_FAILURE;
  
  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Emit_data_non_blocking_sockets(const int index, const int size, 
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
    return Emit_data_sockets(index, size, buffer);
  }

  return REG_FAILURE;
}

/*---------------------------------------------------*/

int Emit_header_sockets(const int index) {

  char buffer[REG_PACKET_SIZE];
  int status;

  /* check if socket connection has been made */
  if(IOTypes_table.io_def[index].socket_info.comms_status != 
     REG_COMMS_STATUS_CONNECTED) {
    attempt_listener_connect(index);
  }

  /* now are we connected? */
  if(IOTypes_table.io_def[index].socket_info.comms_status == 
     REG_COMMS_STATUS_CONNECTED) {
#if REG_DEBUG
    fprintf(stderr, "STEER: Emit_header_sockets: socket status is connected, index = %d\n", index );
#endif

    /* send header */
    sprintf(buffer, REG_PACKET_FORMAT, REG_DATA_HEADER);
    buffer[REG_PACKET_SIZE - 1] = '\0';
#if REG_DEBUG
    fprintf(stderr, "STEER: Emit_header_sockets: Sending >>%s<<\n", buffer);
#endif
    status = Emit_data_non_blocking_sockets(index, REG_PACKET_SIZE, 
					    (void*) buffer);

    if(status == REG_SUCCESS) {
#if REG_DEBUG
      fprintf(stderr, "STEER: Emit_header_sockets: Sent %d bytes\n", REG_PACKET_SIZE);
#endif
      return REG_SUCCESS;
    }
    else if(status == REG_FAILURE) {
#if REG_DEBUG
      fprintf(stderr, "STEER: Emit_header_sockets: Write_sockets failed - "
	      "immediate retry connect\n");
#endif
      retry_accept_connect(index);

      if(IOTypes_table.io_def[index].socket_info.comms_status == 
	 REG_COMMS_STATUS_CONNECTED) {
#if REG_DEBUG
	fprintf(stderr, "STEER: Emit_header_sockets: Sending >>%s<<\n", buffer);
#endif    
	if(Emit_data_sockets(index, REG_PACKET_SIZE, 
			     (void*) buffer) == REG_SUCCESS) {
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
    fprintf(stderr, "STEER: Emit_header_sockets: socket not connected, "
	    "index = %d\n", index );
  }
#endif

  return REG_FAILURE;
}
/*---------------------------------------------------*/

int Emit_data_sockets(const int    index, 
		      const size_t num_bytes_to_send, 
		      void*        pData)
{
  int bytes_left;
  int result;
  char* pchar;
  int connector = IOTypes_table.io_def[index].socket_info.connector_handle;

  if(num_bytes_to_send < 0) {
    fprintf(stderr, "STEER: Emit_data_sockets: requested to write < 0 bytes!\n");
    return REG_FAILURE;
  }
  else if(num_bytes_to_send == 0) {
    fprintf(stderr, "STEER: Emit_data_sockets: asked to send 0 bytes!\n");
    return REG_SUCCESS;
  }

  bytes_left = num_bytes_to_send;
  pchar = (char*) pData;

#if REG_DEBUG
  fprintf(stderr, "STEER: Emit_data_sockets: writing...\n");
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
    fprintf(stderr, "STEER: Emit_data_sockets: timed-out trying to write data\n");
#endif
    return REG_TIMED_OUT;
  }

#if REG_DEBUG
  fprintf(stderr, "STEER: Emit_data_sockets: sent %d bytes...\n", 
	  (int) num_bytes_to_send);
#endif
  
  return REG_SUCCESS;
}


/*---------------------------------------------------*/

int Consume_msg_header_sockets(int index, int* datatype, int* count, 
			       int* num_bytes, int* is_fortran_array) {

  int nbytes;
  char buffer[REG_PACKET_SIZE];
  socket_io_type  *sock_info;
  sock_info = &(IOTypes_table.io_def[index].socket_info);

  /* check socket connection has been made */
  if (sock_info->comms_status != REG_COMMS_STATUS_CONNECTED) return REG_FAILURE;

  /* Read header */
#if REG_DEBUG_FULL
  fprintf(stderr, "STEER: Consume_msg_header_sockets: calling recv...\n");
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
      fprintf(stderr, "STEER: Consume_msg_header_sockets: hung up!\n");
    }
#endif

    return REG_FAILURE;
  }

  /* if we're here, we've got data */
#if REG_DEBUG_FULL
  fprintf(stderr, "STEER: Consume_msg_header_sockets: read <%s> from socket\n", 
	  buffer);
#endif

  /* Check for end of data */
  if(!strncmp(buffer, REG_DATA_FOOTER, strlen(REG_DATA_FOOTER))) {
    return REG_EOD;
  }
  else if(strncmp(buffer, BEGIN_SLICE_HEADER, strlen(BEGIN_SLICE_HEADER))) {
    fprintf(stderr, "STEER: ERROR: Consume_msg_header_sockets: incorrect "
	    "header on slice\n");
    return REG_FAILURE;
  }

  /*--- Type of objects in message ---*/
  if((nbytes = recv(sock_info->connector_handle, buffer, REG_PACKET_SIZE, 
		    MSG_WAITALL)) <= 0) {
    if(nbytes == 0) {
      /* closed connection */
      fprintf(stderr, "STEER: Consume_msg_header_sockets: hung up!\n");
    }
    else {
      /* error */
      perror("recv");
    }

    return REG_FAILURE;
  }

#if REG_DEBUG_FULL
  fprintf(stderr, "STEER: Consume_msg_header_sockets: read <%s> from socket\n", 
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
      fprintf(stderr, "STEER: Consume_msg_header_sockets: hung up!\n");
    }
    else {
      /* error */
      perror("recv");
    }

    return REG_FAILURE;
  }

#if REG_DEBUG_FULL
  fprintf(stderr, "STEER: Consume_msg_header_sockets: read <%s> from socket\n", 
	  buffer);
#endif

  if(!strstr(buffer, "<Num_objects>")) {
    return REG_FAILURE;
  }

  if(sscanf(buffer, "<Num_objects>%d</Num_objects>", count) != 1){
    fprintf(stderr, "STEER: ERROR: Consume_msg_header_sockets: failed to "
	    "read Num_objects\n");
    return REG_FAILURE;
  }

  /*--- No. of bytes in message ---*/
  if((nbytes = recv(sock_info->connector_handle, buffer, REG_PACKET_SIZE, 
		    MSG_WAITALL)) <= 0) {
    if(nbytes == 0) {
      /* closed connection */
      fprintf(stderr, "STEER: Consume_msg_header_sockets: hung up!\n");
    }
    else {
      /* error */
      perror("recv");
    }

    return REG_FAILURE;
  }

#if REG_DEBUG_FULL
  fprintf(stderr, "STEER: Consume_msg_header_sockets: read >%s< from socket\n", 
	  buffer);
#endif

  if(!strstr(buffer, "<Num_bytes>")) {
    return REG_FAILURE;
  }

  if(sscanf(buffer, "<Num_bytes>%d</Num_bytes>", num_bytes) != 1) {
    fprintf(stderr, "STEER: ERROR: Consume_msg_header_sockets: failed to read "
	    "Num_bytes\n");
    return REG_FAILURE;
  }

  /*--- Array ordering in message ---*/
  if((nbytes = recv(sock_info->connector_handle, buffer, REG_PACKET_SIZE, 
		    MSG_WAITALL)) <= 0) {
    if(nbytes == 0) {
      /* closed connection */
      fprintf(stderr, "STEER: Consume_msg_header_sockets: hung up!\n");
    }
    else {
      /* error */
      perror("recv");
    }

    return REG_FAILURE;
  }

#if REG_DEBUG_FULL
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
      fprintf(stderr, "STEER: Consume_msg_header_sockets: hung up!\n");
    }
    else {
      /* error */
      perror("recv");
    }

    return REG_FAILURE;
  }

#if REG_DEBUG_FULL
  fprintf(stderr, "STEER: Consume_msg_header_sockets: read <%s> from socket\n", 
	  buffer);
#endif

  if(strncmp(buffer, END_SLICE_HEADER, strlen(END_SLICE_HEADER))) {
    fprintf(stderr, "STEER: ERROR: Consume_msg_header_sockets: failed to find "
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
    fprintf(stderr, "STEER: INFO: Consume_start_data_check_socket: socket is NOT "
	    "connected, index = %d\n", index);
#endif
    return REG_FAILURE;
  }

#if REG_DEBUG
  fprintf(stderr, "STEER: INFO: Consume_start_data_check_socket: socket status "
	  "is connected, index = %d\n", index);
#endif

  /* Drain socket until start tag found */
  attempt_reconnect = 1;
  memset(buffer, '\0', 1);

#if REG_DEBUG
  fprintf(stderr, "STEER: Consume_start_data_check_socket: searching for start tag\n");
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
	  perror("STEER: recv");
	}
      }
#if REG_DEBUG
      else {
	/* recv returned 0 bytes => closed connection */
	fprintf(stderr, "STEER: Consume_start_data_check_sockets: hung up!\n");
      }
#endif

      /* We're in the middle of a while loop here so don't keep trying
	 to reconnect ad infinitum */
      if(!attempt_reconnect) {
	return REG_FAILURE;
      }

#if REG_DEBUG
      fprintf(stderr, "\nSTEER: Consume_start_data_check_sockets: recv failed - "
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
    fprintf(stderr, "STEER: Consume_start_data_check_sockets: read >>%s<< "
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
	  fprintf(stderr, "STEER: Consume_start_data_check_sockets: hung up!\n");
	}
	else {
	  /* error */
	  perror("recv");
	}

	if(nbytes != nbytes1) {
	  fprintf(stderr, "STEER: ERROR: Consume_start_data_check_sockets: failed "
		  "to read remaining %d bytes of header\n", (int) nbytes1);
	  return REG_FAILURE;
	}
      }
    }

    IOTypes_table.io_def[index].buffer_max_bytes = REG_IO_BUFSIZE;
    IOTypes_table.io_def[index].buffer = (void*) malloc(REG_IO_BUFSIZE);
    if(!IOTypes_table.io_def[index].buffer) {
      IOTypes_table.io_def[index].buffer_max_bytes = 0;
      fprintf(stderr, "STEER: ERROR: Consume_start_data_check_sockets: malloc "
	      "of IO buffer failed\n");
      return REG_FAILURE;
    }

    return REG_SUCCESS;
  }
  return REG_FAILURE;
}

/*--------------------------------------------------------------*/

int Consume_data_read_sockets(const int index, 
			      const int datatype, 
			      const int num_bytes_to_read, 
			      void *pData) {
  int nbytes;

  socket_io_type  *sock_info;

#if REG_DEBUG

#ifdef USE_REG_TIMING
  double time0, time1;
#endif

  fprintf(stderr, "STEER: Consume_data_read_sockets: calling recv for %d bytes\n", (int) num_bytes_to_read);

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
  fprintf(stderr, "STEER: Consume_data_read_sockets: recv read %d bytes\n", (int) nbytes);

#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time1);
  fprintf(stderr, "                                  in %.3f seconds\n", (float) (time1-time0));
#endif
#endif /* REG_DEBUG */

  if(nbytes <= 0) {
      if(nbytes == 0) {
	/* closed connection */
	fprintf(stderr, "STEER: INFO: Consume_data_read_sockets: hung up!\n");
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
  fprintf(stderr, "STEER: INFO: Caught SIGPIPE!\n");
#endif

  signal(SIGPIPE, signal_handler_sockets);

}
#endif

/*---------------------------------------------------*/

int Emit_ack_sockets(int index){

  /* Send a 16-byte acknowledgement message */
  char *ack_msg = "<ACK/>          ";
  return Emit_data_sockets(index, strlen(ack_msg), (void*)ack_msg);
}

/*---------------------------------------------------*/

int Consume_ack_sockets(int index){

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
#if REG_DEBUG_FULL
      fprintf(stderr, "STEER: Consume_ack_sockets: no data on socket to "
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

#if REG_DEBUG_FULL
  fprintf(stderr, "STEER: INFO: Consume_ack_sockets: no ack received\n");
#endif
  return REG_FAILURE;
}

/*---------------------------------------------------*/

#endif /* REG_SOCKET_SAMPLES */
