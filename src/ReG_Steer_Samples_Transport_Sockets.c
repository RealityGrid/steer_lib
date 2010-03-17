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
    @file ReG_Steer_Samples_Transport_Sockets.c
    @brief Source file for socket-based samples transport.
    @author Robert Haines
  */

#include "ReG_Steer_Config.h"
#include "ReG_Steer_Samples_Transport_API.h"
#include "ReG_Steer_Samples_Transport_Sockets.h"
#include "ReG_Steer_Sockets_Common.h"
#include "ReG_Steer_Common.h"
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
  strncpy(Steer_lib_config.Samples_transport_string, "Sockets", 8);

#if !REG_HAS_MSG_NOSIGNAL
  signal(SIGPIPE, signal_handler_sockets);
#endif

  return socket_info_table_init(&socket_info_table, IOTypes_table.max_entries);
}

/*---------------------------------------------------*/

int Finalize_samples_transport() {
  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Initialize_IOType_transport_impl(const int direction, const int index) {

  int return_status = REG_SUCCESS;
  socket_info_type* socket_info = &(socket_info_table.socket_info[index]);

  /* set up socket info stuff */
  if(socket_info_init(socket_info) != REG_SUCCESS) {
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: Initialize_IOType_transport: failed to "
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
      if(create_listener_samples(index) != REG_SUCCESS) {
#ifdef REG_DEBUG
	fprintf(stderr, "STEER: Initialize_IOType_transport: failed to "
		"create listener for IOType\n");
#endif
	return_status = REG_FAILURE;
      }
      else {
	fprintf(stderr, "STEER: Initialize_IOType_transport: Created "
		"listener on port %d, index %d, label %s\n",
		socket_info->listener_port,
		index, IOTypes_table.io_def[index].label);
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

      if(create_connector_samples(index) != REG_SUCCESS) {
#ifdef REG_DEBUG
	fprintf(stderr, "STEER: Initialize_IOType_transport: failed to "
		"register connector for IOType\n");
#endif
	return_status = REG_FAILURE;
      }
#ifdef REG_DEBUG
      else {
	fprintf(stderr, "STEER: Initialize_IOType_transport: "
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

/*---------------------------------------------------*/

void Finalize_IOType_transport_impl() {
  int index;

  for(index = 0; index < IOTypes_table.num_registered; index++) {
    if(IOTypes_table.io_def[index].direction == REG_IO_OUT) {
      /* close sockets */
      cleanup_listener_connection_samples(index);
      socket_info_cleanup(&(socket_info_table.socket_info[index]));
    }
    else if(IOTypes_table.io_def[index].direction == REG_IO_IN) {
      /* close sockets */
      cleanup_connector_connection_samples(index);
      socket_info_cleanup(&(socket_info_table.socket_info[index]));
    }
  }
}

/*---------------------------------------------------*/

int Disable_IOType_impl(const int index) {
  /* check index is valid */
  if(index < 0 || index >= IOTypes_table.num_registered) {
    fprintf(stderr, "STEER: Disable_IOType: index out of range\n");
    return REG_FAILURE;
  }

  if(IOTypes_table.io_def[index].direction == REG_IO_OUT) {
    /* close sockets */
    cleanup_listener_connection_samples(index);
  }
  else if(IOTypes_table.io_def[index].direction == REG_IO_IN) {
    /* close sockets */
    cleanup_connector_connection_samples(index);
  }

  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Enable_IOType_impl(int index) {
  /* check index is valid */
  if(index < 0 || index >= IOTypes_table.num_registered) return REG_FAILURE;

  if(IOTypes_table.io_def[index].direction == REG_IO_OUT) {
    /* open socket and register callback function to listen for and
       accept connections */
    if(create_listener_samples(index) != REG_SUCCESS) {
#ifdef REG_DEBUG
      fprintf(stderr, "STEER: Enable_IOType: failed to create listener for IOType\n");
#endif
      return REG_FAILURE;
    }
  }
  else if(IOTypes_table.io_def[index].direction == REG_IO_IN) {
    if (create_connector_samples(index) != REG_SUCCESS) {
#ifdef REG_DEBUG
      fprintf(stderr, "STEER: Enable_IOType: failed to register "
	      "connector for IOType\n");
#endif
      return REG_FAILURE;
    }
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

int Emit_header_impl(const int index) {

  char buffer[REG_PACKET_SIZE];
  int status;

  /* check if socket connection has been made */
  if(socket_info_table.socket_info[index].comms_status !=
     REG_COMMS_STATUS_CONNECTED) {
    attempt_listener_connect_samples(index);
  }

  /* now are we connected? */
  if(socket_info_table.socket_info[index].comms_status ==
     REG_COMMS_STATUS_CONNECTED) {
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: Emit_header: socket status is connected, index = %d\n", index );
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
      fprintf(stderr, "STEER: Emit_header: Write failed - "
	      "immediate retry connect\n");
#endif
      retry_accept_connect_samples(index);

      if(socket_info_table.socket_info[index].comms_status ==
	 REG_COMMS_STATUS_CONNECTED) {
#ifdef REG_DEBUG
	fprintf(stderr, "STEER: Emit_header: Sending >>%s<<\n", buffer);
#endif
	if(Emit_data_impl(index, REG_PACKET_SIZE,
			  (void*) buffer) == REG_SUCCESS) {
	  return REG_SUCCESS;
	}
      }
    }
#ifdef REG_DEBUG
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

int Emit_data_impl(const int    index,
		      const size_t num_bytes_to_send,
		      void*        pData)
{
  int bytes_left;
  int result;
  char* pchar;
  int connector = socket_info_table.socket_info[index].connector_handle;

  if(num_bytes_to_send < 0) {
    fprintf(stderr, "STEER: Emit_data: requested to write < 0 bytes!\n");
    return REG_FAILURE;
  }
  else if(num_bytes_to_send == 0) {
    fprintf(stderr, "STEER: Emit_data: asked to send 0 bytes!\n");
    return REG_SUCCESS;
  }

  bytes_left = num_bytes_to_send;
  pchar = (char*) pData;

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: Emit_data: writing...\n");
#endif
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
    fprintf(stderr, "STEER: Emit_data: timed-out trying to write data\n");
#endif
    return REG_TIMED_OUT;
  }

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: Emit_data: sent %d bytes...\n",
	  (int) num_bytes_to_send);
#endif

  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Emit_ack_impl(int index){

  /* Send a 16-byte acknowledgement message */
  char *ack_msg = "<ACK/>          ";
  return Emit_data_impl(index, strlen(ack_msg), (void*)ack_msg);
}

/*---------------------------------------------------*/

int Get_IOType_address_impl(int i, char** pbuf, int* bytes_left) {
  int nbytes;

  if(IOTypes_table.io_def[i].direction == REG_IO_OUT){
    if(!strstr(socket_info_table.socket_info[i].listener_hostname, "NOT_SET")) {
      nbytes = snprintf(*pbuf, *bytes_left, "<Address>%s:%d</Address>\n",
			socket_info_table.socket_info[i].listener_hostname,
			(int)(socket_info_table.socket_info[i].listener_port));

#ifdef REG_DEBUG
      /* Check for truncation */
      if((nbytes >= (*bytes_left-1)) || (nbytes < 1)){
	fprintf(stderr, "STEER: Emit_IOType_defs: message exceeds max. "
		"msg. size of %d bytes\n", REG_MAX_MSG_SIZE);
	return REG_FAILURE;
      }
#endif /* REG_DEBUG */
      *pbuf += nbytes;
      *bytes_left -= nbytes;
    }
  }

  return REG_SUCCESS;
}

/*--------------------- Others ----------------------*/

int create_connector_samples(const int index) {

  int i;
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
  if(set_reuseaddr(connector) == REG_SOCKETS_ERROR) {
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

/*--------------------------------------------------------------------*/

int connect_connector_samples(const int index) {

  struct sockaddr_in theirAddr;
  int  connector     = socket_info_table.socket_info[index].connector_handle;
  int  return_status = REG_SUCCESS;

  /* get a remote address if we need to */
  if(socket_info_table.socket_info[index].connector_port == 0) {
    return_status =
      Get_data_io_address_impl(IOTypes_table.io_def[index].input_index,
			       IOTypes_table.io_def[index].direction,
			       socket_info_table.socket_info[index].connector_hostname,
			       &(socket_info_table.socket_info[index].connector_port),
			       IOTypes_table.io_def[index].proxySourceLabel);
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
  }
  else {
    fprintf(stderr, "STEER: connect_connector: cannot get remote address\n");
  }

  return return_status;
}

/*--------------------------------------------------------------------*/
