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

#include "ReG_Steer_Config.h"

#include "ReG_Steer_Sockets_Common.h"
#include "ReG_Steer_Common.h"
#include "ReG_Steer_Appside_internal.h"
#include <string.h>
#include <signal.h>
#include <sys/time.h>

#ifdef _AIX
#include <fcntl.h>
#endif

/* */
socket_info_table_type socket_info_table;

/* Need access to these tables which are actually declared in 
   ReG_Steer_Appside_internal.h */
extern IOdef_table_type IOTypes_table;
extern Steerer_connection_table_type Steerer_connection;

/** Global scratch buffer - declared in ReG_Steer_Appside.c */
extern char Global_scratch_buffer[];

/*--------------------------------------------------------------------*/

int socket_info_table_init() {
  int i;

  socket_info_table.max_entries = IOTypes_table.max_entries;
  socket_info_table.num_used = 0;
  socket_info_table.socket_info = (socket_info_type*) 
    malloc(socket_info_table.max_entries * sizeof(socket_info_type));

  if(socket_info_table.socket_info == NULL) {
    fprintf(stderr, "STEER: socket_info_table_init: failed to allocate memory "
	    "for socket info table\n");
    return REG_FAILURE;
  }

  for(i = 0; i < socket_info_table.max_entries; i++) {
    sprintf(socket_info_table.socket_info[i].listener_hostname,
	    "%s", "NOT_SET");
  }

  return REG_SUCCESS;
}

int socket_info_init(const int index) {

  char* pchar = NULL;
  char* ip_addr;
  int   min, max;
  socket_info_table.socket_info[index].min_port = 0;
  socket_info_table.socket_info[index].max_port = 0;

/* #ifdef REG_PROXY_SAMPLES */
/*   /\* We always connect OUT to the IOProxy *\/ */
/*   pchar = getenv("GLOBUS_TCP_SOURCE_RANGE"); */

/* #else */
/* #ifdef REG_SOCKET_SAMPLES */

  if(IOTypes_table.io_def[index].direction == REG_IO_OUT){
    pchar = getenv("GLOBUS_TCP_PORT_RANGE");
    /* We want to do a bind if we're going to have to listen so we
       know where which port we're listening on - ARP being lazy */
    socket_info_table.socket_info[index].min_port = 21370;
    socket_info_table.socket_info[index].max_port = 65535;
  }
  else{
    /* We only connect outwards if the IOType 
       is of REG_IO_IN direction */
    pchar = getenv("GLOBUS_TCP_SOURCE_RANGE");
  }

/* #endif */
/* #endif */

  if( pchar && (sscanf(pchar, "%d,%d", &(min), &(max)) == 2) ) {
    socket_info_table.socket_info[index].min_port = min;
    socket_info_table.socket_info[index].max_port = max;
  }

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: socket_info_init: port range %d - %d\n", 
	  socket_info_table.socket_info[index].min_port,
	  socket_info_table.socket_info[index].max_port);
#endif

  /* set local TCP interface to use */
  if(Get_fully_qualified_hostname(&pchar, &ip_addr) == REG_SUCCESS) {
    strcpy(socket_info_table.socket_info[index].tcp_interface, ip_addr);
  }
  else {
    sprintf(socket_info_table.socket_info[index].tcp_interface, " ");
  }

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: socket_info_init: local tcp interface: %s\n", 
	  socket_info_table.socket_info[index].tcp_interface);
#endif

  socket_info_table.socket_info[index].listener_port = 0;
  socket_info_table.socket_info[index].listener_handle = -1;
  socket_info_table.socket_info[index].listener_status = REG_COMMS_STATUS_NULL;
  socket_info_table.socket_info[index].comms_status = REG_COMMS_STATUS_NULL;   
  socket_info_table.socket_info[index].connector_port = 0;
  socket_info_table.socket_info[index].connector_handle = -1;
  sprintf(socket_info_table.socket_info[index].connector_hostname, " ");

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
    socket_info_table.socket_info[index].comms_status=REG_COMMS_STATUS_FAILURE;
    return REG_FAILURE;
  }
  socket_info_table.socket_info[index].listener_handle = listener;

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
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: create_listener: Taking hostname from "
	    "REG_IO_ADDRESS variable\n");
#endif
    strcpy(socket_info_table.socket_info[index].listener_hostname, pchar);
  }
  else if( (pchar = getenv("GLOBUS_HOSTNAME")) ){
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: create_listener: Taking hostname from "
	    "GLOBUS_HOSTNAME variable\n");
#endif
    strcpy(socket_info_table.socket_info[index].listener_hostname, pchar);
  }
  else if(Get_fully_qualified_hostname(&pchar, &ip_addr) == REG_SUCCESS){
    strcpy(socket_info_table.socket_info[index].listener_hostname, pchar);
  }
  else{
    fprintf(stderr, "STEER: WARNING: Sockets_create_listener: failed to get hostname\n");
    sprintf(socket_info_table.socket_info[index].listener_hostname, 
	    "NOT_SET");
  }
  
  /* set up server address */
  myAddr.sin_family = AF_INET;
  if(strlen(socket_info_table.socket_info[index].tcp_interface) == 1) {
    myAddr.sin_addr.s_addr = INADDR_ANY;
  }
  else {
    inet_aton(socket_info_table.socket_info[index].tcp_interface, 
	      &(myAddr.sin_addr));
  }
  memset(&(myAddr.sin_zero), '\0', 8); /* zero the rest */

  /* Now bind listener so we can accept connections when they happen */
  i = socket_info_table.socket_info[index].min_port;
  myAddr.sin_port = htons((short) i);

  while(bind(listener, (struct sockaddr*) &myAddr, sizeof(struct sockaddr)) == 
	REG_SOCKETS_ERROR) {
    if(++i > socket_info_table.socket_info[index].max_port) {
      perror("bind");
      close(listener);
      socket_info_table.socket_info[index].comms_status=REG_COMMS_STATUS_FAILURE;
      return REG_FAILURE;
    }
    myAddr.sin_port = htons((short) i);
  }
  /* we're bound, so save the port number we're using */
  socket_info_table.socket_info[index].listener_port = i;

  /* now we need to actually listen */
  if(listen(listener, 10) == REG_SOCKETS_ERROR) {
    perror("listen");
    close(listener);
    socket_info_table.socket_info[index].comms_status = REG_COMMS_STATUS_FAILURE;
    socket_info_table.socket_info[index].listener_status = REG_COMMS_STATUS_FAILURE;
    return REG_FAILURE;
  }

  /* we are listening! */
  socket_info_table.socket_info[index].listener_status = REG_COMMS_STATUS_LISTENING;
  socket_info_table.socket_info[index].comms_status = REG_COMMS_STATUS_LISTENING;

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

/* #ifdef REG_PROXY_SAMPLES */
/*   /\* Turn off NAGLE's algorithm if using an ioProxy so that the (small) */
/*      acknowledgement messages get sent immediately instead of being buffered */
/*      - helps to ensure ack is received before socket is shutdown when consumer */
/*      is shutdown *\/ */
/*   yes = 1; */
/*   if(setsockopt(connector, IPPROTO_TCP, TCP_NODELAY, &yes,  */
/* 		sizeof(int)) == REG_SOCKETS_ERROR) { */
/*     perror("setsockopt"); */
/*     return REG_FAILURE; */
/*   } */
/* #endif /\* REG_PROXY_SAMPLES *\/ */

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
  if( (i = socket_info_table.socket_info[index].min_port) ){
    myAddr.sin_port = htons((short) i);

    fprintf(stderr, "STEER: create_connector: using range %d -- %d for bind\n",
	    socket_info_table.socket_info[index].min_port,
	    socket_info_table.socket_info[index].max_port);

    while(bind(connector, (struct sockaddr*) &myAddr, 
	       sizeof(struct sockaddr)) == REG_SOCKETS_ERROR) {
      if(++i > socket_info_table.socket_info[index].max_port) {
	fprintf(stderr, "STEER: create_connector: failed to find free local port to "
		"bind to in range %d -- %d\n",
		socket_info_table.socket_info[index].min_port,
		socket_info_table.socket_info[index].max_port);
	close(connector);
	socket_info_table.socket_info[index].comms_status=REG_COMMS_STATUS_FAILURE;
	return REG_FAILURE;
      }
      myAddr.sin_port = htons((short) i);
    }
  }
  socket_info_table.socket_info[index].comms_status=REG_COMMS_STATUS_WAITING_TO_CONNECT;

  /* might as well try to connect now... */
  connect_connector(index);

  return REG_SUCCESS;
}

/*--------------------------------------------------------------------*/

int connect_connector(const int index) {

  struct sockaddr_in theirAddr;
  int  connector     = socket_info_table.socket_info[index].connector_handle;
  int  return_status = REG_SUCCESS;
  char tmpBuf[REG_MAX_STRING_LENGTH];
  int  i;

  /* get a remote address if we need to */
  if(socket_info_table.socket_info[index].connector_port == 0) {
#ifdef REG_DIRECT_TCP_STEERING
    return_status = Get_data_source_address_direct(IOTypes_table.io_def[index].input_index, 
						   socket_info_table.socket_info[index].connector_hostname,
						   &(socket_info_table.socket_info[index].connector_port));
#else
#ifdef REG_SOAP_STEERING	  
    /* Go out into the world of grid/web services... */
#ifdef REG_WSRF /* use WSRF */
    if(IOTypes_table.io_def[index].direction == REG_IO_IN){
      return_status = Get_data_source_address_wsrf(IOTypes_table.io_def[index].input_index, 
						   socket_info_table.socket_info[index].connector_hostname,
						   &(socket_info_table.socket_info[index].connector_port),
						   IOTypes_table.io_def[index].proxySourceLabel);
    }
    else{
      /* (We'll only be attempting to connect a connector for an IOType of 
	 direction REG_IO_OUT when using an IOProxy.) */
      return_status = Get_data_sink_address_wsrf(IOTypes_table.io_def[index].input_index, 
						 socket_info_table.socket_info[index].connector_hostname,
						 &(socket_info_table.socket_info[index].connector_port));
    }
#else /* use OGSI */
    return_status = Get_data_source_address_soap(IOTypes_table.io_def[index].input_index, 
						 socket_info_table.socket_info[index].connector_hostname,
						 &(socket_info_table.socket_info[index].connector_port));
#endif /* REG_WSRF */

#else /* File-based steering */
    /* get hostname and port from environment variables */
    return_status = Get_data_source_address_file(IOTypes_table.io_def[index].input_index, 
						 socket_info_table.socket_info[index].connector_hostname,
						 &(socket_info_table.socket_info[index].connector_port));
#endif /* !REG_SOAP_STEERING */
#endif /* REG_DIRECT_TCP_STEERING */
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

#ifdef REG_PROXY_SAMPLES
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
      socket_info_table.socket_info[index].connector_port = 0;
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
  if(socket_info_table.socket_info[index].listener_status == REG_COMMS_STATUS_LISTENING) {
    close_listener_handle(index);
  }

  if(socket_info_table.socket_info[index].comms_status == REG_COMMS_STATUS_CONNECTED) { 
    close_connector_handle(index);
  }

  /* Flag that this listener is dead - used in Emit_IOType_defs */
  sprintf(socket_info_table.socket_info[index].listener_hostname, "NOT_SET");
  socket_info_table.socket_info[index].listener_port = 0;
}

/*--------------------------------------------------------------------*/

void cleanup_connector_connection(const int index) {
  if (socket_info_table.socket_info[index].comms_status == REG_COMMS_STATUS_CONNECTED ||
      socket_info_table.socket_info[index].comms_status == REG_COMMS_STATUS_WAITING_TO_CONNECT ){
    close_connector_handle(index);
  }
}

/*--------------------------------------------------------------------*/

void close_listener_handle(const int index) {
  if(close(socket_info_table.socket_info[index].listener_handle) == REG_SOCKETS_ERROR) {
    perror("close");
    socket_info_table.socket_info[index].listener_status = REG_COMMS_STATUS_FAILURE;
  }
  else {
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: close_listener_handle: close OK\n");
#endif
    socket_info_table.socket_info[index].listener_status = REG_COMMS_STATUS_NULL;
  }
}

/*--------------------------------------------------------------------*/

void close_connector_handle(const int index) {
  if(close(socket_info_table.socket_info[index].connector_handle) == REG_SOCKETS_ERROR) {
    perror("close");
    socket_info_table.socket_info[index].comms_status = REG_COMMS_STATUS_FAILURE;
  }
  else {
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: close_connector_handle: close OK\n");
#endif
    socket_info_table.socket_info[index].comms_status = REG_COMMS_STATUS_NULL;
  }
}

/*--------------------------------------------------------------------*/

void attempt_listener_connect(const int index) {
  socket_info_type *socket_info;
  socket_info = &(socket_info_table.socket_info[index]);

  if(socket_info->listener_status != REG_COMMS_STATUS_LISTENING) {
#ifdef REG_DEBUG
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
#ifdef REG_DEBUG
      fprintf(stderr, "STEER: attempt_listener_connect: poll_socket\n");
#endif
      poll_socket(index);
    }

    if (socket_info->comms_status == REG_COMMS_STATUS_FAILURE ||
	socket_info->comms_status == REG_COMMS_STATUS_NULL ) {
      /* connection has broken - we're still listening so see if 
         anything to connect */
#ifdef REG_DEBUG
      fprintf(stderr, "STEER: attempt_listener_connect: retry accept connect\n");
#endif
      retry_accept_connect(index);
    }
  }
}

/*--------------------------------------------------------------------*/

void retry_accept_connect(const int index) {
  socket_info_type *socket_info;
  socket_info = &(socket_info_table.socket_info[index]);

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
  socket_info_type *socket_info;
  socket_info = &(socket_info_table.socket_info[index]);

  if(socket_info->comms_status == REG_COMMS_STATUS_WAITING_TO_CONNECT) {
#ifdef REG_DEBUG    
    fprintf(stderr, "STEER: attempt_connector_connect: poll_socket\n");
#endif
    poll_socket(index);
  }

  if(socket_info->comms_status == REG_COMMS_STATUS_FAILURE || 
     socket_info->comms_status == REG_COMMS_STATUS_NULL) {
    /* connection has broken - try to re-connect */
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: attempt_connector_connect: retry connect\n");
#endif
    retry_connect(index);
  }
}

/*--------------------------------------------------------------------*/

void retry_connect(const int index) {
  socket_info_type *socket_info;
  socket_info = &(socket_info_table.socket_info[index]);

  /* close the failed connector and retry to connect */
  if(socket_info->comms_status == REG_COMMS_STATUS_CONNECTED) {
    /* Reset connector port (to force us to go and look for it
       again in case it has changed) */
    socket_info->connector_port = 0;
    close_connector_handle(index);
  }

  if(create_connector(index) != REG_SUCCESS) {
#ifdef REG_DEBUG
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

  int listener = socket_info_table.socket_info[index].listener_handle;
  int connector = socket_info_table.socket_info[index].connector_handle;
  int direction = IOTypes_table.io_def[index].direction;

  timeout.tv_sec  = 0;
  timeout.tv_usec = 0;

  /* just return if we have no handles */
  if((listener == -1) && (connector == -1)) return;

  /* clear the socket set and add required handles */
  FD_ZERO(&sockets);
  if(direction == REG_IO_OUT) {
    /* SERVER */
    if(socket_info_table.socket_info[index].listener_status == REG_COMMS_STATUS_LISTENING) {
      FD_SET(listener, &sockets);
      fd_max = listener;

      /* poll using select() */
#ifdef REG_DEBUG
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
	socket_info_table.socket_info[index].connector_handle = new_fd;
	socket_info_table.socket_info[index].comms_status=REG_COMMS_STATUS_CONNECTED;
      }
    }
  }
  else if(direction == REG_IO_IN) {
    /* CLIENT */
    if(socket_info_table.socket_info[index].comms_status != REG_COMMS_STATUS_CONNECTED) {
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

#ifdef REG_DEBUG
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

int recv_non_block(socket_info_type  *sock_info,
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
