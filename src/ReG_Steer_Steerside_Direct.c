/*----------------------------------------------------------------------------
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

#include "ReG_Steer_Config.h"
#include "ReG_Steer_Steerside.h"
#include "ReG_Steer_Steerside_internal.h"
#include "ReG_Steer_Steerside_Direct.h"

/** @file ReG_Steer_Steerside_Direct.c
    @brief Code for direct tcp communications for the steering client.
    @author Robert Haines
*/

/*-------------------------------------------------------------------------*/

int Sim_attach_direct(Sim_entry_type* sim, char* SimID) {

  int return_status;
  char* pchar;
  char* hostname[REG_MAX_STRING_LENGTH];
  int hostport;
  char* colon;

  return_status = REG_FAILURE;

  /* init socket info struct */
  if(socket_info_init_direct(&(sim->socket_info)) != REG_SUCCESS) {
#ifdef REG_DEBUG
    fprintf(stderr, "**DIRECT: connect_steerer_direct: could not init socket info\n");
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
      fprintf(stderr, "**DIRECT: Sim_attach_direct: failed to get address of application to steer\n");
      return REG_FAILURE;
    }
  }

  colon = strstr(pchar, ":");
  if(colon == NULL) {
#ifdef REG_DEBUG
    fprintf(stderr, "**DIRECT: Sim_attach_direct: failed to parse REG_APP_ADDRESS: %s - %s:%d\n", pchar, hostname, hostport);
#endif

    return REG_FAILURE;
  }
  else {
    int addr_len = ((int) (colon - pchar)) + 1;
    snprintf((char*) hostname, addr_len, pchar);
    hostport = atoi(&colon[1]);

#ifdef REG_DEBUG
    fprintf(stderr, "**DIRECT: Sim_attach_direct: Got hostname (%s) and port (%d)\n", hostname, hostport);
#endif

    return_status = REG_SUCCESS;
  }

  if(return_status == REG_SUCCESS) {
    sim->socket_info.connector_port = hostport;
    sprintf(sim->socket_info.connector_hostname, (char*) hostname);
    if(connect_steerer_direct(&(sim->socket_info)) != REG_SUCCESS) {
#ifdef REG_DEBUG
      fprintf(stderr, "**DIRECT: Sim_attach_direct: Could not connect to application\n");
#endif

      return REG_FAILURE;
    }
  }

  /* read the commands that the app supports */
  return_status = Consume_supp_cmds_direct(sim);

  return return_status;
}

/*-------------------------------------------------------------------------*/

int Finalize_connection_direct(Sim_entry_type *sim) {

  Direct_info_type* socket_info;

  socket_info = &(sim->socket_info);

  /* kill any sockets, etc */

  if(socket_info->comms_status == REG_COMMS_STATUS_CONNECTED ||
     socket_info->comms_status == REG_COMMS_STATUS_WAITING_TO_CONNECT) {
    close_connector_direct(socket_info);
  }

  return REG_SUCCESS;
}

/*-------------------------------------------------------------------------*/

int Send_control_msg_direct(Sim_entry_type *sim, char* buf) {

  int buf_len;

  buf_len = strlen(buf) + 1; /* +1 for \0 */
  if(send_steerer_msg(&(sim->socket_info), buf_len, buf) == REG_FAILURE) {
    fprintf(stderr, "**DIRECT: Send_control_msg_direct: failed to send\n");

    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*-------------------------------------------------------------------------*/

int Consume_supp_cmds_direct(Sim_entry_type *sim) {

  struct msg_struct *msg = NULL;
  struct cmd_struct *cmd;

  /* are we connected */
  if(sim->socket_info.comms_status != REG_COMMS_STATUS_CONNECTED) {
#ifdef REG_DEBUG
    fprintf(stderr, "**DIRECT: Consume_supp_cmds_direct: Not connected!\n");
#endif
    return REG_FAILURE;
  }

  msg = Get_status_msg_direct(sim, REG_FALSE);
  if(msg) {
    cmd = msg->supp_cmd->first_cmd;

    while(cmd) {
      sscanf((char *)(cmd->id), "%d", 
	     &(sim->Cmds_table.cmd[sim->Cmds_table.num_registered].cmd_id));

      /* ARPDBG - may need to add cmd parameters here too */
      fprintf(stderr, "cmd->id %s: parsed %d\n", cmd->id, sim->Cmds_table.cmd[sim->Cmds_table.num_registered].cmd_id);

      Increment_cmd_registered(&(sim->Cmds_table));

      cmd = cmd->next;
    }

    Delete_msg_struct(&msg);
  }
  else {
    fprintf(stderr, "**DIRECT: Consume_supp_cmds_direct: error parsing cmds\n");
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*-------------------------------------------------------------------------*/

struct msg_struct *Get_status_msg_direct(Sim_entry_type *sim, int no_block) {

  struct msg_struct* msg = NULL;
  int return_status;
  char data[REG_MAX_MSG_SIZE];
  Direct_info_type* socket_info;

  socket_info = &(sim->socket_info);

  /* will this block? sometimes we want it to, sometimes not */
  if(no_block == REG_TRUE) {
    if(poll_msg_direct(socket_info->connector_handle) == REG_FAILURE) return NULL;
  }

  if(Consume_steerer_msg(socket_info, data) == REG_SUCCESS) {

    msg = New_msg_struct();

    return_status = Parse_xml_buf(data, strlen(data), msg, sim);

    if(return_status != REG_SUCCESS) {
      Delete_msg_struct(&msg);
    }
  }

  return msg;
}

/*-------------------------------------------------------------------------*/

int connect_steerer_direct(Direct_info_type* socket_info) {

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
      fprintf(stderr, "**DIRECT: connect_steerer_direct: inet_aton failed "
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
    fprintf(stderr, "**DIRECT: connect_steerer_direct: using range %d-%d for bind\n", socket_info->min_port_out, socket_info->max_port_out);
#endif

    while(bind(connector, (struct sockaddr*) &myAddr, 
	       sizeof(struct sockaddr)) == REG_SOCKETS_ERROR) {
      if(++i > socket_info->max_port_out) {
	fprintf(stderr, "**DIRECT: connect_steerer_direct: failed to find free local port to bind to in range %d-%d\n", socket_info->min_port_out, socket_info->max_port_out);
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

  if(dns_lookup_direct(socket_info->connector_hostname) == REG_FAILURE) {
    fprintf(stderr, "**DIRECT: Could not resolve hostname: %s\n",
	    socket_info->connector_hostname);
    return REG_FAILURE;  
  }

  theirAddr.sin_family = AF_INET;
  theirAddr.sin_port = htons(socket_info->connector_port);
  if(!inet_aton(socket_info->connector_hostname, &(theirAddr.sin_addr))) {
    fprintf(stderr, 
	      "**DIRECT: connect_steerer_direct: inet_aton reports address is invalid\n");
      return REG_FAILURE;
    }
    memset(&(theirAddr.sin_zero), '\0', 8); /* zero the rest */

    /* ...finally connect to the remote address! */
    if(connect(connector, (struct sockaddr*) &theirAddr, 
	       sizeof(struct sockaddr)) == REG_SOCKETS_ERROR) {
      perror("connect_steerer_direct: connect");
      socket_info->connector_port = 0;
      return REG_FAILURE;
    }
    socket_info->comms_status = REG_COMMS_STATUS_CONNECTED;

  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Consume_steerer_msg(Direct_info_type* socket_info, char* data) {

  int nbytes;
  int data_size;
  int connector = socket_info->connector_handle;

  /* get header */
  nbytes = recv(connector, (void*) &data_size, sizeof(int), MSG_WAITALL);
  if(nbytes != sizeof(int)) {
    fprintf(stderr, "**DIRECT: Consume_steerer_msg: "
	    "Could not get message header");
    return REG_FAILURE;
  }
#ifdef REG_DEBUG
  else {
    fprintf(stderr, "**DIRECT: Consume_steerer_msg: "
	    "Got header: %d bytes to come\n", data_size);
  }
#endif

  /* get message */
  nbytes = recv(connector, data, data_size, MSG_WAITALL);

  if(nbytes <= 0) {
    if(nbytes == 0) {
      fprintf(stderr, "**DIRECT: Consume_steerer_msg: Hung up!\n");
    }
    else {
      perror("recv");
    }
    return REG_FAILURE;
  }

  if(data[data_size] != '\0') {
    data[data_size] == '\0';
  }

  return REG_SUCCESS;
}

/*--------------------------------------------------------------------*/

int poll_msg_direct(int handle) {

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
    fprintf(stderr, "**DIRECT: socket ready...\n");
#endif

    return REG_SUCCESS;
  }

#ifdef REG_DEBUG
  fprintf(stderr, "**DIRECT: socket not ready...\n");
#endif

  return REG_FAILURE;
}

/*--------------------------------------------------------------------*/

int dns_lookup_direct(char* hostname) {
  struct hostent* host;

  if((host = gethostbyname(hostname)) == NULL) {
    herror("gethostbyname");
    return REG_FAILURE;
  }

#ifdef REG_DEBUG
  fprintf(stderr, "**DIRECT: DNS lookup: host: %s\n", host->h_name);
  fprintf(stderr, "                        IP: %s\n", 
	  inet_ntoa(*((struct in_addr*) host->h_addr)));
#endif

  /* This next bit must be done with a sprintf for AIX...
   * It can be done with a strcpy on Linux or IRIX...      */
  sprintf(hostname, "%s", 
	  inet_ntoa(*((struct in_addr*) host->h_addr_list[0])));

  return REG_SUCCESS;
}

/*--------------------------------------------------------------------*/

void close_listener_direct(Direct_info_type* socket_info) {
  if(close(socket_info->listener_handle) == REG_SOCKETS_ERROR) {
    perror("close");
    socket_info->listener_status = REG_COMMS_STATUS_FAILURE;
  }
  else {
#ifdef REG_DEBUG
    fprintf(stderr, "**DIRECT: close_listener_direct: close OK\n");
#endif
    socket_info->listener_status = REG_COMMS_STATUS_NULL;
  }
}

/*--------------------------------------------------------------------*/

void close_connector_direct(Direct_info_type* socket_info) {
  if(close(socket_info->connector_handle) == REG_SOCKETS_ERROR) {
    perror("close");
    socket_info->comms_status = REG_COMMS_STATUS_FAILURE;
  }
  else {
#ifdef REG_DEBUG
    fprintf(stderr, "**DIRECT: close_connector_direct: close OK\n");
#endif
    socket_info->comms_status = REG_COMMS_STATUS_NULL;
  }
}

/*--------------------------------------------------------------------*/
