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

/*--------------------------------------------------------------------*/

int socket_info_table_init(socket_info_table_type* table,
			   const int max_entries) {
  int i;

  table->max_entries = max_entries;
  table->num_used = 0;
  table->socket_info = (socket_info_type*) 
    malloc(max_entries * sizeof(socket_info_type));

  if(table->socket_info == NULL) {
    fprintf(stderr, "STEER: socket_info_table_init: failed to allocate memory "
	    "for socket info table\n");
    return REG_FAILURE;
  }

  for(i = 0; i < max_entries; i++) {
    sprintf(table->socket_info[i].listener_hostname,
	    "%s", "NOT_SET");
  }

  return REG_SUCCESS;
}

/*--------------------------------------------------------------------*/

int socket_info_init(socket_info_type* socket_info) {

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

  if(pchar && (sscanf(pchar, "%d,%d", &(min), &(max)) == 2)) {
    socket_info->min_port_in = min;
    socket_info->max_port_in = max;
  }

  /* get a port range to connect out of */
  pchar = getenv("GLOBUS_TCP_SOURCE_RANGE");

  if(pchar && (sscanf(pchar, "%d,%d", &(min), &(max)) == 2)) {
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

#ifdef REG_DEBUG
  fprintf(stderr, "socket_info_init: port range to listen on %d - %d\n", 
	  socket_info->min_port_in,
	  socket_info->max_port_in);
  fprintf(stderr, "socket_info_init: port range to connect out of %d - %d\n", 
	  socket_info->min_port_out,
	  socket_info->max_port_out);
  fprintf(stderr, "socket_info_init: local tcp interface: %s\n", 
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

/*--------------------------------------------------------------------*/

void socket_info_cleanup(socket_info_type* socket_info) {

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

ssize_t recv_non_block(int s, void *buf, size_t len, int flags) {
#if REG_HAS_MSG_DONTWAIT
  /* non-blocking can be achieved with a flag to recv() */
  return recv(s, buf, len, flags | MSG_DONTWAIT);
#else
  ssize_t result;
  int save_flags = fcntl(s, F_GETFL);

  /* turn off blocking, do the recv, then reset the flags */
  fcntl(s, F_SETFL, save_flags | O_NONBLOCK);
  result = recv(s, buf, len, flags);
  fcntl(s, F_SETFL, save_flags);

  return result;
#endif
}

/*--------------------------------------------------------------------*/

#if !REG_HAS_MSG_NOSIGNAL
void signal_handler_sockets(int a_signal) {

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: INFO: Caught SIGPIPE!\n");
#endif

  signal(SIGPIPE, signal_handler_sockets);

}
#endif

/*--------------------------------------------------------------------*/

ssize_t send_no_signal(int s, const void* buf, size_t len, int flags) {
  int pass_flags;

#if REG_HAS_MSG_NOSIGNAL
  pass_flags = flags | MSG_NOSIGNAL;
#else
  pass_flags = flags;
#endif

  return send(s, buf, len, pass_flags);
}

/*--------------------------------------------------------------------*/
