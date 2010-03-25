/*
  The RealityGrid Steering Library

  Copyright (c) 2002-2010, University of Manchester, United Kingdom.
  All rights reserved.

  This software is produced by Research Computing Services, University
  of Manchester as part of the RealityGrid project and associated
  follow on projects, funded by the EPSRC under grants GR/R67699/01,
  GR/R67699/02, GR/T27488/01, EP/C536452/1, EP/D500028/1,
  EP/F00561X/1.

  LICENCE TERMS

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

    * Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of The University of Manchester nor the names
      of its contributors may be used to endorse or promote products
      derived from this software without specific prior written
      permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
  COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.

  Author: Robert Haines
 */

/** @internal
    @file ReG_Steer_Sockets_Common.c
    @brief Source file for sockets-specific routines.
    @author Robert Haines
  */

#include "ReG_Steer_Config.h"
#include "ReG_Steer_Sockets_Common.h"
#include "ReG_Steer_Common.h"

/*--------------------------------------------------------------------*/

int socket_info_table_init(socket_info_table_type* table,
			   const int max_entries) {

  table->max_entries = max_entries;
  table->num_used = 0;
  table->socket_info = (socket_info_type*)
    malloc(max_entries * sizeof(socket_info_type));

  if(table->socket_info == NULL) {
    fprintf(stderr, "STEER: socket_info_table_init: failed to allocate memory "
	    "for socket info table\n");
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*--------------------------------------------------------------------*/

int socket_info_init(socket_info_type* socket_info) {

  char* pchar = NULL;
  int   min, max;
  char host[REG_MAX_STRING_LENGTH];

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
  socket_info->tcp_interface = (char*) malloc(NI_MAXHOST * sizeof(char));
  if(get_fully_qualified_hostname(host, socket_info->tcp_interface)
     != REG_SUCCESS) {
    free(socket_info->tcp_interface);
    socket_info->tcp_interface = NULL;
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
  socket_info->listener_hostname = (char*) malloc(NI_MAXHOST * sizeof(char));
  sprintf(socket_info->listener_hostname, "NOT_SET");

  socket_info->connector_port = 0;
  socket_info->connector_handle = -1;
  socket_info->connector_hostname = (char*) malloc(NI_MAXHOST * sizeof(char));

  socket_info->comms_status = REG_COMMS_STATUS_NULL;

  return REG_SUCCESS;
}

/*--------------------------------------------------------------------*/

void socket_info_cleanup(socket_info_type* socket_info) {
  if(socket_info->tcp_interface)
    free(socket_info->tcp_interface);

  if(socket_info->listener_hostname)
    free(socket_info->listener_hostname);

  if(socket_info->connector_hostname)
    free(socket_info->connector_hostname);
}

/*--------------------------------------------------------------------*/

int get_fully_qualified_hostname(char* hostname, char* ipaddr) {
  int status;
  char* pchar;

  /* First check to see if we're using an interface other than the default */
  if((pchar = getenv("REG_TCP_INTERFACE"))) {
    strncpy(hostname, pchar, REG_MAX_STRING_LENGTH);
  }
  else {
    status = gethostname(hostname, REG_MAX_STRING_LENGTH);
    if(status != 0) {
      fprintf(stderr, "gethostname failed\n");
      return REG_FAILURE;
    }
  }

  return dns_lookup(hostname, ipaddr, 1);
}

/*--------------------------------------------------------------------*/

int dns_lookup(char* hostname, char* ipaddr, int canon) {
  struct addrinfo hints;
  struct addrinfo* result;
  struct addrinfo* rp;
  int status;

  memset(&hints, 0, sizeof(struct addrinfo));
  hints.ai_family = AF_UNSPEC;
  hints.ai_flags = AI_CANONNAME;

  status = getaddrinfo(hostname, NULL, &hints, &result);
  if(status != 0) {
    fprintf(stderr, "STEER: getaddrinfo: %s\n", gai_strerror(status));
    return REG_FAILURE;
  }

  for(rp = result; rp != NULL; rp = rp->ai_next) {
    if(rp->ai_canonname != NULL) {
      status = getnameinfo(rp->ai_addr, rp->ai_addrlen, ipaddr,
			   REG_MAX_STRING_LENGTH, NULL, 0, NI_NUMERICHOST);
      if(status != 0) {
	fprintf(stderr, "STEER: getnameinfo: %s\n", gai_strerror(status));
	return REG_FAILURE;
      }

      if(canon)
	strncpy(hostname, rp->ai_canonname, REG_MAX_STRING_LENGTH);

#ifdef REG_DEBUG
      fprintf(stderr, "DNS lookup: host: %s\n", hostname);
      fprintf(stderr, "              IP: %s\n", ipaddr);
#endif
    }
  }

  freeaddrinfo(result);

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

ssize_t recv_wait_all(int s, void *buf, size_t len, int flags) {
#if REG_HAS_MSG_WAITALL
  return recv(s, buf, len, flags | MSG_WAITALL);
#else
  ssize_t result;
  ssize_t got = 0;

  while(len > 0 && (result = recv(s, buf + got, len, flags)) > 0) {
    len -= result;
    got += result;
  }

  return result < 0 ? result : got;
#endif
}

/*--------------------------------------------------------------------*/

int set_tcpnodelay(int s) {
#ifdef _MSC_VER
  BOOL yes = TRUE;

  return setsockopt(s, IPPROTO_TCP, TCP_NODELAY, (char*) &yes, sizeof(BOOL));
#else
  int yes = 1;

  return setsockopt(s, IPPROTO_TCP, TCP_NODELAY, &yes, sizeof(int));
#endif
}

/*--------------------------------------------------------------------*/
