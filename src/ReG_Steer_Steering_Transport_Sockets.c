/*
  The RealityGrid Steering Library

  Copyright (c) 2002-2009, University of Manchester, United Kingdom.
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
    @file ReG_Steer_Steering_Transport_Sockets.c
    @brief Source file for socket-based steering transport.
    @author Robert Haines
  */

#include "ReG_Steer_Config.h"
#include "ReG_Steer_types.h"
#include "ReG_Steer_Steering_Transport_API.h"
#include "ReG_Steer_Steering_Transport_Sockets.h"
#include "ReG_Steer_Sockets_Common.h"
#include "ReG_Steer_Appside.h"
#include "ReG_Steer_Appside_internal.h"
#include "ReG_Steer_Steerside.h"
#include "ReG_Steer_Steerside_internal.h"

/** Basic library config - declared in ReG_Steer_Common */
extern Steer_lib_config_type Steer_lib_config;

/* */
socket_info_type appside_socket_info;

/** @internal
   The table holding details of our communication channel with the
   steering client - defined in ReG_Steer_Appside.c */
extern Steerer_connection_table_type Steerer_connection;

/** Log of values of parameters for which logging has
   been requested */
extern Chk_log_type Param_log;

/*----------------- Appside methods ---------------------*/

int Initialize_steering_connection_impl(int  NumSupportedCmds,
					int* SupportedCmds) {

  strncpy(Steer_lib_config.Steering_transport_string, "Sockets", 8);

  /* set up socket_info struct */
  if(socket_info_init(&appside_socket_info) != REG_SUCCESS) {
#ifdef REG_DEBUG
    fprintf(stderr, "Initialize_steering_connection: "
	    "could not initialize socket information\n");
#endif
    return REG_FAILURE;
  }

  /* Create msg about supported commands */
  Make_supp_cmds_msg(NumSupportedCmds, SupportedCmds, 
		     Steerer_connection.supp_cmds, REG_MAX_MSG_SIZE);

  /* try to create listener */
/*   if(Create_steerer_listener(socket_info) != REG_SUCCESS) { */
  if(create_steering_listener(&appside_socket_info) != REG_SUCCESS) {
#ifdef REG_DEBUG
    fprintf(stderr, "Initialize_steering_connection: "
	    "failed to listen for steering connections\n");
#endif
    return REG_FAILURE;
  }
  else {
    fprintf(stderr, "Initialize_steering_connection: "
	    "registered steering connection on port %d\n", 
	    appside_socket_info.listener_port);
  }

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int Finalize_steering_connection_impl() {

/*   Direct_info_type* socket_info = &(Steerer_connection.socket_info); */

  if(appside_socket_info.listener_status == REG_COMMS_STATUS_LISTENING) {
    close_steering_listener(&appside_socket_info);
  }

  if(appside_socket_info.comms_status == REG_COMMS_STATUS_CONNECTED) {
    close_steering_connector(&appside_socket_info);
  }

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int Steerer_connected_impl() {

/*   Direct_info_type* socket_info = &(Steerer_connection.socket_info); */

  /* If already connected then just return a "yes" */  
  if(appside_socket_info.comms_status == REG_COMMS_STATUS_CONNECTED) {
    return REG_SUCCESS;
  }

  /* If not connected, see if something is trying to connect */
  if(appside_socket_info.listener_status != REG_COMMS_STATUS_LISTENING) {
#ifdef REG_DEBUG
    fprintf(stderr, "Steerer_connected: "
	    "dealing with listener_status not LISTENING \n");
#endif
    if (appside_socket_info.listener_status == REG_COMMS_STATUS_FAILURE ||
	appside_socket_info.listener_status == REG_COMMS_STATUS_NULL ) {
      create_steering_listener(&appside_socket_info);
    }
  }

  /* only check comms_status if listener_status is now listening */
  if (appside_socket_info.listener_status == REG_COMMS_STATUS_LISTENING) {
    if (appside_socket_info.comms_status == REG_COMMS_STATUS_LISTENING) {
#ifdef REG_DEBUG
      fprintf(stderr, "Steerer_connected: poll_socket\n");
#endif
      poll_steering_socket(&appside_socket_info);
    }

    /* if we are now connected, then send first message */
    if(appside_socket_info.comms_status == REG_COMMS_STATUS_CONNECTED) {
      /* send first message here */
      send_steering_msg(&appside_socket_info,
			(strlen(Steerer_connection.supp_cmds) + 1),
			Steerer_connection.supp_cmds);
      return REG_SUCCESS;
    }
  }

  /* if we get here, then we're not connected */
#ifdef REG_DEBUG
      fprintf(stderr, "Steerer_connected: not connected\n");
#endif
  return REG_FAILURE;
}

/*-------------------------------------------------------*/

int Detach_from_steerer_impl() {

/*   Direct_info_type* socket_info = &(Steerer_connection.socket_info); */

  /* steerer will detatch FROM us so need to clean up socket, etc here */

  if(appside_socket_info.listener_status == REG_COMMS_STATUS_LISTENING) {
    close_steering_listener(&appside_socket_info);
  }

  if(appside_socket_info.comms_status == REG_COMMS_STATUS_CONNECTED) {
    close_steering_connector(&appside_socket_info);
  }

  /* send out all the param logs */
  Param_log.send_all         = REG_TRUE;

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int Send_status_msg_impl(char *buf) {

  int buf_len;

  buf_len = strlen(buf) + 1; /* +1 for \0 */
  if(send_steering_msg(&appside_socket_info, buf_len, buf) == REG_FAILURE) {
    fprintf(stderr, "Send_status_msg: failed to send\n");
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

struct msg_struct *Get_control_msg_impl() {

  struct msg_struct* msg = NULL;
  int return_status;
  char data[REG_MAX_MSG_SIZE];

  /* will this block? we never want it to! */
  if(poll_steering_msg(&appside_socket_info,
		       appside_socket_info.connector_handle) == REG_FAILURE)
    return NULL;

  if(consume_steering_msg(&appside_socket_info, data) == REG_SUCCESS) {

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

/*-------------------------------------------------------*/

int Get_data_io_address_impl(const int           dummy,
			     const int           direction,
			     char*               hostname,
			     unsigned short int* port,
			     char*               label) {
  char* pchar;
  int   len;

  /* Return port = 0 on failure */
  *port = 0;

  /* Get hostname and port from environment variables */

  pchar = getenv("REG_CONNECTOR_HOSTNAME");
  if(pchar) {
    len = strlen(pchar);
    if(len < REG_MAX_STRING_LENGTH) {
      sprintf(hostname, pchar);
    }
    else {
      fprintf(stderr, "Get_data_source_address: content of "
	      "REG_CONNECTOR_HOSTNAME exceeds max. string length of "
	      "%d chars\n", REG_MAX_STRING_LENGTH);
      return REG_FAILURE;
    }
  }
  else{
    fprintf(stderr, 
	    "Get_data_source_address: REG_CONNECTOR_HOSTNAME not set\n");
    return REG_FAILURE;
  }

  pchar = getenv("REG_CONNECTOR_PORT");
  if(pchar) {
    *port = (unsigned short int)atoi(pchar);
  }
  else {
    fprintf(stderr, 
	    "Get_data_source_address: REG_CONNECTOR_PORT not set\n");
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int Record_checkpoint_set_impl(int ChkType, char* ChkTag, char* Path) {
  return Record_Chkpt(ChkType, ChkTag);
}

/*--------------------- Steerside methods ---------------------*/

extern Sim_table_type Sim_table;
socket_info_table_type steerer_socket_info_table;

int Initialize_steerside_transport() {
  strncpy(Steer_lib_config.Steering_transport_string, "Sockets", 8);

  return socket_info_table_init(&steerer_socket_info_table,
				REG_MAX_NUM_STEERED_SIM);
}

/*-------------------------------------------------------*/

int Finalize_steerside_transport() {
  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

struct msg_struct *Get_status_msg_impl(int index, int no_block) {

  struct msg_struct* msg = NULL;
  int return_status;
  char data[REG_MAX_MSG_SIZE];
  socket_info_type* socket_info;

  socket_info = &(steerer_socket_info_table.socket_info[index]);

  /* will this block? sometimes we want it to, sometimes not */
  if(no_block == REG_TRUE) {
    if(poll_steering_msg(socket_info, socket_info->connector_handle) == REG_FAILURE)
      return NULL;
  }

  if(consume_steering_msg(socket_info, data) == REG_SUCCESS) {

    msg = New_msg_struct();

    return_status = Parse_xml_buf(data, strlen(data), msg,
				  &(Sim_table.sim[index]));

    if(return_status != REG_SUCCESS) {
      Delete_msg_struct(&msg);
    }
  }

  return msg;
}

/*-------------------------------------------------------*/

int Send_control_msg_impl(int index, char* buf) {

  int buf_len;
  socket_info_type* socket_info;

  socket_info = &(steerer_socket_info_table.socket_info[index]);

  buf_len = strlen(buf) + 1; /* +1 for \0 */
  if(send_steering_msg(socket_info, buf_len, buf) == REG_FAILURE) {
    fprintf(stderr, "**DIRECT: Send_control_msg_direct: failed to send\n");
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int Send_detach_msg_impl(int index) {
  int command[1];
  Sim_entry_type *sim;

  sim = &(Sim_table.sim[index]);
  command[0] = REG_STR_DETACH;

  return Emit_control(sim->handle, 1, command, NULL);
}

/*-------------------------------------------------------*/

int Finalize_connection_impl(int index) {

  socket_info_type* socket_info;
  Sim_entry_type *sim;

  socket_info = &(steerer_socket_info_table.socket_info[index]);
  sim = &(Sim_table.sim[index]);

  if(sim->detached == REG_FALSE) {
    Emit_detach_cmd(sim->handle);
    fprintf(stderr, "sent detach command\n");
  }

  /* kill any sockets, etc */

  if(socket_info->comms_status == REG_COMMS_STATUS_CONNECTED ||
     socket_info->comms_status == REG_COMMS_STATUS_WAITING_TO_CONNECT) {
    close_steering_connector(socket_info);
  }

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int Sim_attach_impl(int index, char* SimID) {

  int return_status;
  char* pchar;
  char* hostname[REG_MAX_STRING_LENGTH];
  int hostport;
  char* colon;
  socket_info_type* socket_info;

  return_status = REG_FAILURE;

  /* init socket info struct */
  socket_info = &(steerer_socket_info_table.socket_info[index]);
  if(socket_info_init(socket_info) != REG_SUCCESS) {
#ifdef REG_DEBUG
    fprintf(stderr, "Sim_attach: could not init socket info\n");
#endif
    return REG_FAILURE;
  }

  /* if SimID has a machine:port in it, use that */
  if(strlen(SimID) != 0) {
    pchar = SimID;
  }
  else {
    /* otherwise use REG_APP_ADDRESS env. variable */
    if(!(pchar = getenv("REG_APP_ADDRESS"))) {
      fprintf(stderr, "Sim_attach: failed to get address of application to steer\n");
      return REG_FAILURE;
    }
  }

  colon = strstr(pchar, ":");
  if(colon == NULL) {
#ifdef REG_DEBUG
    fprintf(stderr, "Sim_attach: failed to parse REG_APP_ADDRESS: %s - %s:%d\n", pchar, hostname, hostport);
#endif

    return REG_FAILURE;
  }
  else {
    int addr_len = ((int) (colon - pchar)) + 1;
    snprintf((char*) hostname, addr_len, pchar);
    hostport = atoi(&colon[1]);

#ifdef REG_DEBUG
    fprintf(stderr, "Sim_attach: Got hostname (%s) and port (%d)\n",
	    hostname, hostport);
#endif

    return_status = REG_SUCCESS;
  }

  if(return_status == REG_SUCCESS) {
    socket_info->connector_port = hostport;
    sprintf(socket_info->connector_hostname, (char*) hostname);
    if(create_steering_connector(socket_info) != REG_SUCCESS) {
#ifdef REG_DEBUG
      fprintf(stderr, "Sim_attach: Could not connect to application\n");
#endif

      return REG_FAILURE;
    }
  }

  /* read the commands that the app supports */
  return_status = consume_supp_cmds(index);

  return return_status;
}

/*-------------------------------------------------------*/

int Sim_attach_security_impl(const int index,
			     const struct reg_security_info* sec) {
  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int Get_param_log_impl(int index, int handle) {
  int           command[1];
  static char** command_params = NULL;
  
  command[0] = REG_STR_EMIT_PARAM_LOG;
  
  /* Which parameter to get the log of */
  if(!command_params) command_params = Alloc_string_array(32, 1);
  sprintf(command_params[0], "%d", handle);
  
  return Emit_control(Sim_table.sim[index].handle,
		      1,
		      command,
		      command_params);
}

/*-------------------------------------------------------*/

int Get_registry_entries_impl(const char* registryEPR,
			      const struct reg_security_info* sec,
			      struct registry_contents* contents) {
  return REG_SUCCESS;
}

/*-------------------- Internal methods -------------------------*/

int consume_supp_cmds(int index) {

  struct msg_struct *msg = NULL;
  struct cmd_struct *cmd;
  socket_info_type* socket_info;
  Sim_entry_type *sim;

  socket_info = &(steerer_socket_info_table.socket_info[index]);
  sim = &(Sim_table.sim[index]);

  /* are we connected */
  if(socket_info->comms_status != REG_COMMS_STATUS_CONNECTED) {
#ifdef REG_DEBUG
    fprintf(stderr, "consume_supp_cmds: Not connected!\n");
#endif
    return REG_FAILURE;
  }

  msg = Get_status_msg_impl(index, REG_FALSE);
  if(msg) {
    cmd = msg->supp_cmd->first_cmd;

    while(cmd) {
      sscanf((char *)(cmd->id), "%d", 
	     &(sim->Cmds_table.cmd[sim->Cmds_table.num_registered].cmd_id));

      /* ARPDBG - may need to add cmd parameters here too */
#ifdef REG_DEBUG
      fprintf(stderr, "cmd->id %s: parsed %d\n", cmd->id, sim->Cmds_table.cmd[sim->Cmds_table.num_registered].cmd_id);
#endif

      Increment_cmd_registered(&(sim->Cmds_table));

      cmd = cmd->next;
    }

    Delete_msg_struct(&msg);
  }
  else {
    fprintf(stderr, "consume_supp_cmds: error parsing cmds\n");
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int create_steering_connector(socket_info_type* socket_info) {

  int i;
  int yes = 1;
  int connector;
  struct sockaddr_in myAddr;
  struct sockaddr_in theirAddr;

  /* create_connector */
  connector = socket(AF_INET, SOCK_STREAM, 0);
  if(connector == REG_SOCKETS_ERROR) {
    perror("socket");
    socket_info->comms_status = REG_COMMS_STATUS_FAILURE;
    return REG_FAILURE;
  }

  /* ok so save connector handle */
  socket_info->connector_handle = connector;

  /* ...turn off the "Address already in use" error message... */
  if(setsockopt(connector, SOL_SOCKET, SO_REUSEADDR, &yes, 
		sizeof(int)) == REG_SOCKETS_ERROR) {
    perror("setsockopt");
    return REG_FAILURE;
  }

  /* ...build local address struct... */
  myAddr.sin_family = AF_INET;
  if(strlen(socket_info->tcp_interface) == 1) {
    myAddr.sin_addr.s_addr = INADDR_ANY;
  }
  else {
    if(!inet_aton(socket_info->tcp_interface, &(myAddr.sin_addr)) ){
      fprintf(stderr, "create_steering_connector: inet_aton failed "
	      "for interface >>%s<<\n", 
	      socket_info->tcp_interface);
      return REG_FAILURE;
    }
  }
  memset(&(myAddr.sin_zero), '\0', 8); /* zero the rest */

  /* ...and bind connector so we can punch out of firewalls (if necessary)... */
  if( (i = socket_info->min_port_out) ) {
    myAddr.sin_port = htons((short) i);

#ifdef REG_DEBUG
    fprintf(stderr, "create_steering_connector: using range %d-%d for bind\n",
	    socket_info->min_port_out, socket_info->max_port_out);
#endif

    while(bind(connector, (struct sockaddr*) &myAddr, 
	       sizeof(struct sockaddr)) == REG_SOCKETS_ERROR) {
      if(++i > socket_info->max_port_out) {
	fprintf(stderr, "create_steering_connector: failed to find free local "
		"port to bind to in range %d-%d\n",
		socket_info->min_port_out, socket_info->max_port_out);
	close(connector);
	socket_info->comms_status=REG_COMMS_STATUS_FAILURE;
	return REG_FAILURE;
      }
      myAddr.sin_port = htons((short) i);
    }
  }
  socket_info->comms_status=REG_COMMS_STATUS_WAITING_TO_CONNECT;

  /* try to connect now */

  /* Get an address to bind to first! */

  if(dns_lookup(socket_info->connector_hostname) == REG_FAILURE) {
    fprintf(stderr, "Could not resolve hostname: %s\n",
	    socket_info->connector_hostname);
    return REG_FAILURE;  
  }

  theirAddr.sin_family = AF_INET;
  theirAddr.sin_port = htons(socket_info->connector_port);
  if(!inet_aton(socket_info->connector_hostname, &(theirAddr.sin_addr))) {
    fprintf(stderr, 
	      "create_steering_connector: inet_aton reports address is invalid\n");
      return REG_FAILURE;
    }
    memset(&(theirAddr.sin_zero), '\0', 8); /* zero the rest */

    /* ...finally connect to the remote address! */
    if(connect(connector, (struct sockaddr*) &theirAddr, 
	       sizeof(struct sockaddr)) == REG_SOCKETS_ERROR) {
      perror("create_steering_connector: connect");
      socket_info->connector_port = 0;
      return REG_FAILURE;
    }
    socket_info->comms_status = REG_COMMS_STATUS_CONNECTED;

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int create_steering_listener(socket_info_type* socket_info) {

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

/*-------------------------------------------------------*/

void poll_steering_socket(socket_info_type* socket_info) {

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
#ifdef REG_DEBUG
    fprintf(stderr, "poll_steering_socket: polling for accept\n");
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

/*-------------------------------------------------------*/

int send_steering_msg(socket_info_type* socket_info,
		      const size_t num_bytes_to_send, 
		      void*        pData) {
  int bytes_left;
  int result;
  char* pchar;
  int connector = socket_info->connector_handle;

  if(num_bytes_to_send < 0) {
    fprintf(stderr, "send_steering_msg: requested to write < 0 bytes!\n");
    return REG_FAILURE;
  }
  else if(num_bytes_to_send == 0) {
    fprintf(stderr, "send_steering_msg: asked to send 0 bytes!\n");
    return REG_SUCCESS;
  }

  /* cast void pointer */
  pchar = (char*) pData;

  /* send header with just an int saying how much data to come */
#ifdef REG_DEBUG
  fprintf(stderr, "send_steering_msg: sending header...\n");
#endif
  result = send_no_signal(connector, (void*)&num_bytes_to_send, sizeof(int), 0);
/* #ifndef __linux */
/*   result = send(connector, (void*) &num_bytes_to_send, sizeof(int), 0); */
/* #else */
/*   result = send(connector, (void*) &num_bytes_to_send, sizeof(int), MSG_NOSIGNAL); */
/* #endif */
  if(result != sizeof(int)) {
    perror("send");
    return REG_FAILURE;
  }

  bytes_left = num_bytes_to_send;

#ifdef REG_DEBUG
  fprintf(stderr, "send_steering_msg: writing...\n");
#endif
  while(bytes_left > 0) {
    result = send_no_signal(connector, pchar, bytes_left, 0);
/* #ifndef __linux */
/*     result = send(connector, pchar, bytes_left, 0); */
/* #else */
/*     result = send(connector, pchar, bytes_left, MSG_NOSIGNAL); */
/* #endif */
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
    fprintf(stderr, "send_steering_msg: timed-out trying to write data\n");
#endif
    return REG_TIMED_OUT;
  }

#ifdef REG_DEBUG
  fprintf(stderr, "send_steering_msg: sent %d bytes...\n", 
	  (int) num_bytes_to_send);
#endif
  
  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int consume_steering_msg(socket_info_type* socket_info, char* data) {

  int nbytes;
  int data_size;
  int connector = socket_info->connector_handle;

  /* get header */
  nbytes = recv_wait_all(connector, (void*) &data_size, sizeof(int), 0);
  if(nbytes != sizeof(int)) {
    fprintf(stderr, "consume_steering_msg: Could not get message header");
    return REG_FAILURE;
  }
#ifdef REG_DEBUG
  else {
    fprintf(stderr, "consume_steering_msg: "
	    "Got header: %d bytes to come\n", data_size);
  }
#endif

  /* get message */
  nbytes = recv_wait_all(connector, data, data_size, 0);

  if(nbytes <= 0) {
    if(nbytes == 0) {
      fprintf(stderr, "consume_steering_msg: Hung up!\n");
    }
    else {
      perror("recv");
    }
    return REG_FAILURE;
  }

  if(data[data_size] != '\0') {
    data[data_size] = '\0';
  }

  return REG_SUCCESS;
}

/*-------------------------------------------------------*/

int poll_steering_msg(socket_info_type* socket_info, int handle) {

  struct timeval timeout;
  fd_set sockets;	/* the set of sockets to check */
  int fd_max;		/* the max socket number */
  
  timeout.tv_sec  = 0;
  timeout.tv_usec = 0;

  if(handle == -1) return REG_FAILURE;

  FD_ZERO(&sockets);
  FD_SET(handle, &sockets);
  fd_max = handle;

  if(select(fd_max + 1, &sockets, NULL, NULL, &timeout) == -1) {
    perror("select");
    return REG_FAILURE;
  }

  if(FD_ISSET(handle, &sockets)) {
#ifdef REG_DEBUG
    fprintf(stderr, "socket ready...\n");
#endif

    return REG_SUCCESS;
  }

#ifdef REG_DEBUG
  fprintf(stderr, "socket not ready...\n");
#endif

  return REG_FAILURE;
}

/*-------------------------------------------------------*/

void close_steering_listener(socket_info_type* socket_info) {
  if(close(socket_info->listener_handle) == REG_SOCKETS_ERROR) {
    perror("close");
    socket_info->listener_status = REG_COMMS_STATUS_FAILURE;
  }
  else {
#ifdef REG_DEBUG
    fprintf(stderr, "**close_steering_listener: close OK\n");
#endif
    socket_info->listener_status = REG_COMMS_STATUS_NULL;
  }
}

/*-------------------------------------------------------*/

void close_steering_connector(socket_info_type* socket_info) {
  if(close(socket_info->connector_handle) == REG_SOCKETS_ERROR) {
    perror("close");
    socket_info->comms_status = REG_COMMS_STATUS_FAILURE;
  }
  else {
#ifdef REG_DEBUG
    fprintf(stderr, "close_steering_connector: close OK\n");
#endif
    socket_info->comms_status = REG_COMMS_STATUS_NULL;
  }
}

/*-------------------------------------------------------*/
