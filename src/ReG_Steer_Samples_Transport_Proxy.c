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
    @file ReG_Steer_Samples_Transport_Proxy.c
    @brief Source file for proxy-based samples transport.
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

/** Basic library config - declared in ReG_Steer_Common */
extern Steer_lib_config_type Steer_lib_config;

/* */
socket_info_table_type socket_info_table;

/* Need access to these tables which are actually declared in
   ReG_Steer_Appside_internal.h */
extern IOdef_table_type IOTypes_table;
extern Steerer_connection_table_type Steerer_connection;

/*--------------------- API -------------------------*/

int Initialize_samples_transport() {
  strncpy(Steer_lib_config.Samples_transport_string, "Proxy", 6);

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

  result = recv_wait_all(connector, buffer, (size_t) 2, 0);
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
    snprintf(buffer, REG_PACKET_SIZE, REG_PACKET_FORMAT, REG_DATA_HEADER);

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
  int connector;
  socket_info_type* socket_info = &(socket_info_table.socket_info[index]);
  struct addrinfo hints;
  struct addrinfo* result;
  struct addrinfo* rp;
  char port[8];
  int status;

  memset(&hints, 0, sizeof(struct addrinfo));
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_NUMERICHOST;
  hints.ai_protocol = IPPROTO_TCP;

  /* we need to try and bind even though we are connecting out
     so that we can punch out of firewalls. This means we need
     to try different ports until we find one free to use. */
  for(i = socket_info->min_port_out; i <= socket_info->max_port_out; i++) {
#ifdef REG_DEBUG
    fprintf(stderr, "Trying to connect out from %s:%d\n",
	    socket_info->tcp_interface, i);
#endif

    sprintf(port, "%d", i);
    status = getaddrinfo(socket_info->tcp_interface, port, &hints, &result);
    if(status != 0) {
      fprintf(stderr, "STEER: getaddrinfo: %s\n", gai_strerror(status));
      return REG_FAILURE;
    }

    for(rp = result; rp != NULL; rp = rp->ai_next) {
      connector = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
      if(connector == REG_SOCKETS_ERROR)
	continue;

      /* Turn off NAGLE's algorithm if using an ioProxy so that the
	 (small) acknowledgement messages get sent immediately instead
	 of being buffered - helps to ensure ack is received before
	 socket is shutdown when consumer is shutdown */
      if(set_tcpnodelay(connector) == REG_SOCKETS_ERROR) {
	perror("setsockopt");
	close(connector);
	freeaddrinfo(result);
	return REG_FAILURE;
      }

      if(bind(connector, rp->ai_addr, rp->ai_addrlen) == 0) {
	socket_info->comms_status=REG_COMMS_STATUS_WAITING_TO_CONNECT;
	socket_info->connector_handle = connector;
#ifdef REG_DEBUG
	fprintf(stderr, "bound connector to port %d\n", i);
#endif
	break; /* success */
      }

      /* couldn't bind to that port, close connector and start again */
      close(connector);
    }

    freeaddrinfo(result);

    if(socket_info->comms_status == REG_COMMS_STATUS_WAITING_TO_CONNECT) {
      break;
    }
  }

  if(socket_info->comms_status != REG_COMMS_STATUS_WAITING_TO_CONNECT) {
    /* couldn't create connector */
    socket_info->comms_status=REG_COMMS_STATUS_FAILURE;
    return REG_FAILURE;
  }

  /* might as well try to connect now... */
  connect_connector_samples(index);

  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int connect_connector_samples(const int index) {

  socket_info_type* socket_info = &(socket_info_table.socket_info[index]);
  int  connector     = socket_info->connector_handle;
  int  return_status = REG_SUCCESS;
  char tmpBuf[REG_MAX_STRING_LENGTH];
  struct addrinfo hints;
  struct addrinfo* result;
  struct addrinfo* rp;
  char port[8];
  int status;
  int i;

  /* get a remote address if we need to */
  if(socket_info_table.socket_info[index].connector_port == 0) {
    return_status =
      Get_data_io_address_impl(IOTypes_table.io_def[index].input_index,
			       IOTypes_table.io_def[index].direction,
			       socket_info->connector_hostname,
			       &(socket_info->connector_port),
			       IOTypes_table.io_def[index].proxySourceLabel);
  }

  if(return_status == REG_SUCCESS && socket_info->connector_port != 0) {

    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags = 0;
    hints.ai_protocol = IPPROTO_TCP;

    sprintf(port, "%d", socket_info->connector_port);
    status = getaddrinfo(socket_info->connector_hostname, port,
			 &hints, &result);
    if(status != 0) {
      fprintf(stderr, "STEER: getaddrinfo: %s\n", gai_strerror(status));
      return REG_FAILURE;
    }

    for(rp = result; rp != NULL; rp = rp->ai_next) {
      if(connect(connector, rp->ai_addr, rp->ai_addrlen) != REG_SOCKETS_ERROR)
	break; /* connected - success */
    }

    /* if rp == NULL then we didn't connect above */
    if(rp == NULL) {
    fprintf(stderr, "Could not connect to %s:%d\n",
	    socket_info->connector_hostname, socket_info->connector_port);
      socket_info->connector_port = 0;
      freeaddrinfo(result);
      return REG_FAILURE;
    }

    freeaddrinfo(result);

    socket_info->comms_status = REG_COMMS_STATUS_CONNECTED;

    if(IOTypes_table.io_def[index].direction == REG_IO_IN) {
      sprintf(Steer_lib_config.scratch_buffer, "%s\n%s\n",
	      IOTypes_table.io_def[index].label,
	      IOTypes_table.io_def[index].proxySourceLabel);
    }
    else {
      /* If this is an output channel then we subscribe to
	 acknowledgements but first trim off any trailing
         white space. */
      strncpy(tmpBuf, IOTypes_table.io_def[index].label,
	      REG_MAX_STRING_LENGTH);
      i = strlen(tmpBuf) - 1;
      while(tmpBuf[i] == ' ' && (i > -1)) {
	tmpBuf[i] = '\0';
	i--;
      }
      sprintf(Steer_lib_config.scratch_buffer, "%s\n%s_REG_ACK\n",
	      tmpBuf, tmpBuf);

      /* If this is an output channel then we aren't subscribing to
	 any data source
      sprintf(Steer_lib_config.scratch_buffer, "%s\nACKS_ONLY\n",
      IOTypes_table.io_def[index].label); */
    }

    if(Emit_data_impl(index, strlen(Steer_lib_config.scratch_buffer),
		      (void*)(Steer_lib_config.scratch_buffer)) != REG_SUCCESS) {
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
