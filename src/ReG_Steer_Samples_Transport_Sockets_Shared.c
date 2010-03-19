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
    @file ReG_Steer_Samples_Transport_Sockets_Shared.c
    @brief Source file for routines and data structures shared between
           socket and proxy samples transport implementations.
    @author Robert Haines
  */

#include "ReG_Steer_Config.h"
#include "ReG_Steer_Samples_Transport_API.h"
#include "ReG_Steer_Samples_Transport_Sockets.h"
#include "ReG_Steer_Common.h"
#include "ReG_Steer_Sockets_Common.h"
#include "ReG_Steer_Appside_internal.h"

/* */
extern socket_info_table_type socket_info_table;

/* Need access to these tables which are actually declared in
   ReG_Steer_Appside_internal.h */
extern IOdef_table_type IOTypes_table;

/*---------------------- API ------------------------*/

int Consume_msg_header_impl(int index, int* datatype, int* count,
			       int* num_bytes, int* is_fortran_array) {

  int nbytes;
  char buffer[REG_PACKET_SIZE];
  socket_info_type  *sock_info;
  sock_info = &(socket_info_table.socket_info[index]);

  /* check socket connection has been made */
  if (sock_info->comms_status != REG_COMMS_STATUS_CONNECTED) return REG_FAILURE;

  /* Read header */
#ifdef REG_DEBUG_FULL
  fprintf(stderr, "STEER: Consume_msg_header: calling recv...\n");
#endif

  /* Blocks until REG_PACKET_SIZE bytes received */
  if((nbytes = recv_wait_all(sock_info->connector_handle, buffer,
			     REG_PACKET_SIZE, 0)) <= 0) {
    if(nbytes < 0) {
      /* error */
      perror("recv");
    }
#ifdef REG_DEBUG
    else {
      /* closed connection */
      fprintf(stderr, "STEER: Consume_msg_header: hung up!\n");
    }
#endif

    return REG_FAILURE;
  }

  /* if we're here, we've got data */
#ifdef REG_DEBUG_FULL
  fprintf(stderr, "STEER: Consume_msg_header: read <%s> from socket\n",
	  buffer);
#endif

  /* Check for end of data */
  if(!strncmp(buffer, REG_DATA_FOOTER, strlen(REG_DATA_FOOTER))) {
    return REG_EOD;
  }
  else if(strncmp(buffer, BEGIN_SLICE_HEADER, strlen(BEGIN_SLICE_HEADER))) {
    fprintf(stderr, "STEER: ERROR: Consume_msg_header: incorrect "
	    "header on slice\n");
    return REG_FAILURE;
  }

  /*--- Type of objects in message ---*/
  if((nbytes = recv_wait_all(sock_info->connector_handle, buffer,
			     REG_PACKET_SIZE, 0)) <= 0) {
    if(nbytes == 0) {
      /* closed connection */
      fprintf(stderr, "STEER: Consume_msg_header: hung up!\n");
    }
    else {
      /* error */
      perror("recv");
    }

    return REG_FAILURE;
  }

#ifdef REG_DEBUG_FULL
  fprintf(stderr, "STEER: Consume_msg_header: read <%s> from socket\n",
	  buffer);
#endif

  if(!strstr(buffer, "<Data_type>")) {
    return REG_FAILURE;
  }

  sscanf(buffer, "<Data_type>%d</Data_type>", datatype);

  /*--- No. of objects in message ---*/
  if((nbytes = recv_wait_all(sock_info->connector_handle, buffer,
			     REG_PACKET_SIZE, 0)) <= 0) {
    if(nbytes == 0) {
      /* closed connection */
      fprintf(stderr, "STEER: Consume_msg_header: hung up!\n");
    }
    else {
      /* error */
      perror("recv");
    }

    return REG_FAILURE;
  }

#ifdef REG_DEBUG_FULL
  fprintf(stderr, "STEER: Consume_msg_header: read <%s> from socket\n",
	  buffer);
#endif

  if(!strstr(buffer, "<Num_objects>")) {
    return REG_FAILURE;
  }

  if(sscanf(buffer, "<Num_objects>%d</Num_objects>", count) != 1){
    fprintf(stderr, "STEER: ERROR: Consume_msg_header: failed to "
	    "read Num_objects\n");
    return REG_FAILURE;
  }

  /*--- No. of bytes in message ---*/
  if((nbytes = recv_wait_all(sock_info->connector_handle, buffer,
			     REG_PACKET_SIZE, 0)) <= 0) {
    if(nbytes == 0) {
      /* closed connection */
      fprintf(stderr, "STEER: Consume_msg_header: hung up!\n");
    }
    else {
      /* error */
      perror("recv");
    }

    return REG_FAILURE;
  }

#ifdef REG_DEBUG_FULL
  fprintf(stderr, "STEER: Consume_msg_header: read >%s< from socket\n",
	  buffer);
#endif

  if(!strstr(buffer, "<Num_bytes>")) {
    return REG_FAILURE;
  }

  if(sscanf(buffer, "<Num_bytes>%d</Num_bytes>", num_bytes) != 1) {
    fprintf(stderr, "STEER: ERROR: Consume_msg_header: failed to read "
	    "Num_bytes\n");
    return REG_FAILURE;
  }

  /*--- Array ordering in message ---*/
  if((nbytes = recv_wait_all(sock_info->connector_handle, buffer,
			     REG_PACKET_SIZE, 0)) <= 0) {
    if(nbytes == 0) {
      /* closed connection */
      fprintf(stderr, "STEER: Consume_msg_header: hung up!\n");
    }
    else {
      /* error */
      perror("recv");
    }

    return REG_FAILURE;
  }

#ifdef REG_DEBUG_FULL
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
  if((nbytes = recv_wait_all(sock_info->connector_handle, buffer,
			     REG_PACKET_SIZE, 0)) <= 0) {
    if(nbytes == 0) {
      /* closed connection */
      fprintf(stderr, "STEER: Consume_msg_header: hung up!\n");
    }
    else {
      /* error */
      perror("recv");
    }

    return REG_FAILURE;
  }

#ifdef REG_DEBUG_FULL
  fprintf(stderr, "STEER: Consume_msg_header: read <%s> from socket\n",
	  buffer);
#endif

  if(strncmp(buffer, END_SLICE_HEADER, strlen(END_SLICE_HEADER))) {
    fprintf(stderr, "STEER: ERROR: Consume_msg_header: failed to find "
	    "end of header\n");
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Consume_start_data_check_impl(const int index) {
  char buffer[REG_PACKET_SIZE];
  char* pstart;
  int nbytes = 0;
  int nbytes1 = 0;
  int attempt_reconnect;

  socket_info_type  *sock_info;
  sock_info = &(socket_info_table.socket_info[index]);

  /* if not connected attempt to connect now */
  if(sock_info->comms_status != REG_COMMS_STATUS_CONNECTED) {
    attempt_connector_connect_samples(index);
  }

  /* check if socket connection has been made */
  if(sock_info->comms_status != REG_COMMS_STATUS_CONNECTED) {
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: INFO: Consume_start_data_check_socket: socket is NOT "
	    "connected, index = %d\n", index);
#endif
    return REG_FAILURE;
  }

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: INFO: Consume_start_data_check_socket: socket status "
	  "is connected, index = %d\n", index);
#endif

  /* Drain socket until start tag found */
  attempt_reconnect = 1;
  memset(buffer, '\0', 1);

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: Consume_start_data_check_socket: searching for start tag\n");
#endif

  while(!(pstart = strstr(buffer, REG_DATA_HEADER))) {

    if((nbytes = recv_non_block(sock_info->connector_handle,
				buffer, REG_PACKET_SIZE, 0)) <= 0) {

      if(nbytes < 0) {
	if(errno == EAGAIN) {
	  /* Call would have blocked because no data to read */
#ifdef REG_DEBUG
	  fprintf(stderr, "\n");
#endif
	  /* Call was OK but there's no data to read... */
	  return REG_FAILURE;
	}
	else {
	  /* Some error occurred */
#ifdef REG_DEBUG
	  fprintf(stderr, "\n");
#endif
	  perror("STEER: recv");
	}
      }
#ifdef REG_DEBUG
      else {
	/* recv returned 0 bytes => closed connection */
	fprintf(stderr, "STEER: Consume_start_data_check: hung up!\n");
      }
#endif

      /* We're in the middle of a while loop here so don't keep trying
	 to reconnect ad infinitum */
      if(!attempt_reconnect) {
	return REG_FAILURE;
      }

#ifdef REG_DEBUG
      fprintf(stderr, "\nSTEER: Consume_start_data_check: recv failed - "
	      "try immediate reconnect for index %d\n", index);
#endif

      retry_connect_samples(index);

      /* check if socket reconnection has been made and check for
	 data if it has */
      if (socket_info_table.socket_info[index].comms_status
	  != REG_COMMS_STATUS_CONNECTED) {
	return REG_FAILURE;
      }

      attempt_reconnect = 0;
      memset(buffer, '\0', 1);
    }

#ifdef REG_DEBUG
    fprintf(stderr, "!");
#endif
  } /* !while */

#ifdef REG_DEBUG
  fprintf(stderr, "\n");
#endif

  if(nbytes > 0) {

#ifdef REG_DEBUG
    fprintf(stderr, "STEER: Consume_start_data_check: read >>%s<< "
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
	  fprintf(stderr, "STEER: Consume_start_data_check: hung up!\n");
	}
	else {
	  /* error */
	  perror("recv");
	}

	if(nbytes != nbytes1) {
	  fprintf(stderr, "STEER: ERROR: Consume_start_data_check: failed "
		  "to read remaining %d bytes of header\n", (int) nbytes1);
	  return REG_FAILURE;
	}
      }
    }

    IOTypes_table.io_def[index].buffer_max_bytes = REG_IO_BUFSIZE;
    IOTypes_table.io_def[index].buffer = (void*) malloc(REG_IO_BUFSIZE);
    if(!IOTypes_table.io_def[index].buffer) {
      IOTypes_table.io_def[index].buffer_max_bytes = 0;
      fprintf(stderr, "STEER: ERROR: Consume_start_data_check: malloc "
	      "of IO buffer failed\n");
      return REG_FAILURE;
    }

    /* added following one line for modularization
       moved from ReG_Steer_Appside.c Consume_start_data_check
     */
    IOTypes_table.io_def[index].consuming = REG_TRUE;
    return REG_SUCCESS;
  }
  return REG_FAILURE;
}

/*---------------------------------------------------*/

int Consume_data_read_impl(const int index,
			      const int datatype,
			      const int num_bytes_to_read,
			      void *pData) {
  int nbytes;

  socket_info_type  *sock_info;

#ifdef REG_DEBUG

#if REG_USE_TIMING
  double time0, time1;
#endif

  fprintf(stderr, "STEER: Consume_data_read: calling recv for %d bytes\n", (int) num_bytes_to_read);

#if REG_USE_TIMING
  Get_current_time_seconds(&time0);
#endif

#endif /* REG_DEBUG */

  sock_info = &(socket_info_table.socket_info[index]);

  if(IOTypes_table.io_def[index].use_xdr || IOTypes_table.io_def[index].convert_array_order == REG_TRUE) {
    nbytes = recv_wait_all(sock_info->connector_handle,
			   IOTypes_table.io_def[index].buffer,
			   num_bytes_to_read, 0);
  }
  else {
    nbytes = recv_wait_all(sock_info->connector_handle, pData,
			   num_bytes_to_read, 0);
  }

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: Consume_data_read: recv read %d bytes\n", (int) nbytes);

#if REG_USE_TIMING
  Get_current_time_seconds(&time1);
  fprintf(stderr, "                                  in %.3f seconds\n", (float) (time1-time0));
#endif
#endif /* REG_DEBUG */

  if(nbytes <= 0) {
      if(nbytes == 0) {
	/* closed connection */
	fprintf(stderr, "STEER: INFO: Consume_data_read: hung up!\n");
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

int Consume_ack_impl(int index){

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
  if((nbytes = recv_non_block(socket_info_table.socket_info[index].connector_handle,
			      (void*)buf, 16, 0)) == 16) {
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
	  if(recv_non_block(socket_info_table.socket_info[index].connector_handle,
			    (void*)&(buf[16]), 16, 0) == 16) {

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
#ifdef REG_DEBUG_FULL
      fprintf(stderr, "STEER: Consume_ack: no data on socket to "
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

#ifdef REG_DEBUG_FULL
  fprintf(stderr, "STEER: INFO: Consume_ack: no ack received\n");
#endif
  return REG_FAILURE;
}

/*---------------------------------------------------*/

int Emit_msg_header_impl(const int    index,
			 const size_t num_bytes_to_send,
			 void*        pData) {
  return Emit_data_impl(index, num_bytes_to_send, pData);
}

/*---------------------------------------------------*/

int Emit_start_impl(int index, int seqnum) {
  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Emit_stop_impl(int index) {
  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Consume_stop_impl(int index) {
  return REG_SUCCESS;
}

/*--------------------- Others ----------------------*/

int create_listener_samples(const int index) {

  char* pchar;
  char ip_addr[REG_MAX_STRING_LENGTH];
  socket_info_type* socket_info = &(socket_info_table.socket_info[index]);
  int listener;
  struct addrinfo hints;
  struct addrinfo* result;
  struct addrinfo* rp;
  char port[8];
  int status;
  int i;

  /* Get the hostname of the machine we're running on (so we can publish
   * the endpoint of this connection). If REG_IO_ADDRESS is set then
   * that's what we use.  If not, call get_fully_qualified_hostname which
   * itself uses  REG_TCP_INTERFACE if set. */
  if((pchar = getenv("REG_IO_ADDRESS"))) {
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: create_listener: Taking hostname from "
	    "REG_IO_ADDRESS variable\n");
#endif
    strcpy(socket_info->listener_hostname, pchar);
  }
  else if( (pchar = getenv("GLOBUS_HOSTNAME")) ){
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: create_listener: Taking hostname from "
	    "GLOBUS_HOSTNAME variable\n");
#endif
    strcpy(socket_info->listener_hostname, pchar);
  }
  else if(get_fully_qualified_hostname(socket_info->listener_hostname,
				       ip_addr) != REG_SUCCESS) {
    fprintf(stderr, "STEER: WARNING: Sockets_create_listener: "
	    "failed to get hostname\n");
    sprintf(socket_info->listener_hostname, "NOT_SET");
  }

  memset(&hints, 0, sizeof(struct addrinfo));
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_NUMERICHOST;
  hints.ai_protocol = IPPROTO_TCP;

  for(i = socket_info->min_port_in; i <= socket_info->max_port_in; i++) {
#ifdef REG_DEBUG
    fprintf(stderr, "Trying to bind listner to %s:%d\n",
	    socket_info->tcp_interface, i);
#endif

    sprintf(port, "%d", i);
    status = getaddrinfo(socket_info->tcp_interface, port, &hints, &result);
    if(status != 0) {
      fprintf(stderr, "STEER: getaddrinfo: %s\n", gai_strerror(status));
      return REG_FAILURE;
    }

    for(rp = result; rp != NULL; rp = rp->ai_next) {
      listener = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
      if(listener == REG_SOCKETS_ERROR)
	continue;

      /* Turn off the "Address already in use" error message */
      if(set_reuseaddr(listener) == REG_SOCKETS_ERROR) {
	perror("setsockopt");
	close(listener);
	freeaddrinfo(result);
	return REG_FAILURE;
      }

      if(bind(listener, rp->ai_addr, rp->ai_addrlen) == 0) {
#ifdef REG_DEBUG
	fprintf(stderr, "bound listener to port %d\n", i);
#endif
	socket_info->listener_port = i;

	/* now actually listen */
	if(listen(listener, 10) == REG_SOCKETS_ERROR) {
	  perror("listen");
	  close(listener);
	  socket_info->comms_status = REG_COMMS_STATUS_FAILURE;
	  socket_info->listener_status = REG_COMMS_STATUS_FAILURE;
	  freeaddrinfo(result);
	  return REG_FAILURE;
	}

	/* we are listening! */
	socket_info->listener_handle = listener;
	socket_info->listener_status = REG_COMMS_STATUS_LISTENING;
	socket_info->comms_status = REG_COMMS_STATUS_LISTENING;
	break;
      }

      /* couldn't bind to that port, close connector and start again */
      close(listener);
    }

    freeaddrinfo(result);

    if(socket_info->comms_status == REG_COMMS_STATUS_LISTENING) {
      break;
    }
  }

  if(socket_info->comms_status != REG_COMMS_STATUS_LISTENING) {
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*---------------------------------------------------*/

void cleanup_listener_connection_samples(const int index) {
  if(socket_info_table.socket_info[index].listener_status == REG_COMMS_STATUS_LISTENING) {
    close_listener_handle_samples(index);
  }

  if(socket_info_table.socket_info[index].comms_status == REG_COMMS_STATUS_CONNECTED) {
    close_connector_handle_samples(index);
  }

  /* Flag that this listener is dead - used in Emit_IOType_defs */
  sprintf(socket_info_table.socket_info[index].listener_hostname, "NOT_SET");
  socket_info_table.socket_info[index].listener_port = 0;
}

/*---------------------------------------------------*/

void cleanup_connector_connection_samples(const int index) {
  if (socket_info_table.socket_info[index].comms_status == REG_COMMS_STATUS_CONNECTED ||
      socket_info_table.socket_info[index].comms_status == REG_COMMS_STATUS_WAITING_TO_CONNECT ){
    close_connector_handle_samples(index);
  }
}

/*---------------------------------------------------*/

void close_listener_handle_samples(const int index) {
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

/*---------------------------------------------------*/

void close_connector_handle_samples(const int index) {
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

/*---------------------------------------------------*/

void attempt_listener_connect_samples(const int index) {
  socket_info_type *socket_info;
  socket_info = &(socket_info_table.socket_info[index]);

  if(socket_info->listener_status != REG_COMMS_STATUS_LISTENING) {
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: attempt_listener_connect:dealing with listener_status not LISTENING \n");
#endif
    if (socket_info->listener_status == REG_COMMS_STATUS_FAILURE ||
	socket_info->listener_status == REG_COMMS_STATUS_NULL ) {
      create_listener_samples(index);
    }
  }

  /* only check comms_status if listener_status is now listening */
  if (socket_info->listener_status == REG_COMMS_STATUS_LISTENING) {
    if (socket_info->comms_status == REG_COMMS_STATUS_LISTENING) {
#ifdef REG_DEBUG
      fprintf(stderr, "STEER: attempt_listener_connect: poll_socket\n");
#endif
      poll_socket_samples(index);
    }

    if (socket_info->comms_status == REG_COMMS_STATUS_FAILURE ||
	socket_info->comms_status == REG_COMMS_STATUS_NULL ) {
      /* connection has broken - we're still listening so see if
         anything to connect */
#ifdef REG_DEBUG
      fprintf(stderr, "STEER: attempt_listener_connect: retry accept connect\n");
#endif
      retry_accept_connect_samples(index);
    }
  }
}

/*---------------------------------------------------*/

void retry_accept_connect_samples(const int index) {
  socket_info_type *socket_info;
  socket_info = &(socket_info_table.socket_info[index]);

  /* if this is called, a write has failed */

  /* if we're here and  status is connected  we've lost a connection,
     so first close socket */
  if (socket_info->comms_status==REG_COMMS_STATUS_CONNECTED) {
    close_connector_handle_samples(index);
  }

  poll_socket_samples(index);
}

/*---------------------------------------------------*/

void attempt_connector_connect_samples(const int index) {
  socket_info_type *socket_info;
  socket_info = &(socket_info_table.socket_info[index]);

  if(socket_info->comms_status == REG_COMMS_STATUS_WAITING_TO_CONNECT) {
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: attempt_connector_connect: poll_socket\n");
#endif
    poll_socket_samples(index);
  }

  if(socket_info->comms_status == REG_COMMS_STATUS_FAILURE ||
     socket_info->comms_status == REG_COMMS_STATUS_NULL) {
    /* connection has broken - try to re-connect */
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: attempt_connector_connect: retry connect\n");
#endif
    retry_connect_samples(index);
  }
}

/*---------------------------------------------------*/

void retry_connect_samples(const int index) {
  socket_info_type *socket_info;
  socket_info = &(socket_info_table.socket_info[index]);

  /* close the failed connector and retry to connect */
  if(socket_info->comms_status == REG_COMMS_STATUS_CONNECTED) {
    /* Reset connector port (to force us to go and look for it
       again in case it has changed) */
    socket_info->connector_port = 0;
    close_connector_handle_samples(index);
  }

  if(create_connector_samples(index) != REG_SUCCESS) {
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: retry_connect: failed to register connector for IOType\n");
#endif
    /* Set to FAILURE so create_connector is attempted again next time round */
    socket_info->comms_status=REG_COMMS_STATUS_FAILURE;
  }
}

/*---------------------------------------------------*/

void poll_socket_samples(const int index) {

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
      connect_connector_samples(index);
    }
  }
}

/*---------------------------------------------------*/
