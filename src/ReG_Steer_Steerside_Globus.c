/*----------------------------------------------------------------------------
    This file contains routines and data structures for use in a
    steering application that uses socket communication using Globus
    IO. Note that the functions contained in this file are relevant
    only to steering using Globus. Note that, the functions are not
    required for iotype (sample) communication using Globus.

    (C)Copyright 2002 The University of Manchester, United Kingdom,
    all rights reserved.

    This software is produced by the Supercomputing, Visualization &
    e-Science Group, Manchester Computing, the Victoria University of
    Manchester as part of the RealityGrid project.

    This software has been tested with care but is not guaranteed for
    any particular purpose. Neither the copyright holder, nor the
    University of Manchester offer any warranties or representations,
    nor do they accept any liabilities with respect to this software.

    This software must not be used for commercial gain without the
    written permission of the authors.
    
    This software must not be redistributed without the written
    permission of the authors.

    Permission is granted to modify this software, provided any
    modifications are made freely available to the original authors.
 
    Supercomputing, Visualization & e-Science Group
    Manchester Computing
    University of Manchester
    Manchester M13 9PL
    
    WWW:    http://www.sve.man.ac.uk  
    email:  sve@man.ac.uk
    Tel:    +44 161 275 6095
    Fax:    +44 161 275 6800    

    Initial version by:  S Ramsden, 26.2.2003       0.1               

---------------------------------------------------------------------------*/
#if REG_GLOBUS_STEERING

#include "ReG_Steer_Steerside.h"
#include "ReG_Steer_Steerside_internal.h"
#include "ReG_Steer_Steerside_Globus.h"

int Sim_attach_globus(Sim_entry_type *sim, char *SimID)
{
  char *pchar;
  int   port_ok;
  int   hostname_ok;

  pchar = getenv("REG_STEER_APP_HOSTNAME");
  if (pchar) {

    if (strlen(pchar) < REG_MAX_STRING_LENGTH) {
      sprintf(sim->socket_info.connector_hostname, pchar);
      hostname_ok = 1;
    }
    else{

      fprintf(stderr, "Sim_attach_globus: REG_STEER_APP_HOSTNAME exceeds "
	      "%d characters in length\n", REG_MAX_STRING_LENGTH);
    }
  }
  
  /* ARPDBG add error handling */
  pchar = getenv("REG_STEER_APP_PORT");
  if (pchar) {
    sim->socket_info.connector_port = atoi(pchar);
    port_ok = 1;
  }

  if (port_ok && hostname_ok) {
    
    if (Globus_create_connector(&(sim->socket_info)) != REG_SUCCESS) {
#if REG_DEBUG
      fprintf(stderr, "Sim_attach_globus: failed to register connector\n");
#endif
      return REG_FAILURE;
    }

#if REG_DEBUG
    fprintf(stderr, "Sim_attach_globus: registered connector on "
	    "port %d, hostname = %s\n", 
	    sim->socket_info.connector_port, 
	    sim->socket_info.connector_hostname);
#endif

  }
  else{
    fprintf(stderr, "Sim_attach_globus: cannot create connector as "
	    "port and hostname not set\n");

    return REG_FAILURE;
  }

  return Consume_supp_cmds_globus(sim);
}

/*-------------------------------------------------------------------*/

int Consume_supp_cmds_globus(Sim_entry_type *sim)
{
  struct msg_struct *msg;
  struct cmd_struct *cmd;
 
  if(sim->socket_info.comms_status != REG_COMMS_STATUS_CONNECTED){

    fprintf(stderr, "Consume_supp_cmds_globus: socket no connected\n");
    return REG_FAILURE;
  }

  msg = Get_status_msg_globus(sim);

  if(!msg){

    return REG_FAILURE;
  }

  /* Store the commands that the simulation supports */

  cmd = msg->supp_cmd->first_cmd;

  while(cmd){

    sscanf((char *)(cmd->id), "%d", 
	   &(sim->Cmds_table.cmd[sim->Cmds_table.num_registered].cmd_id));

    /* ARPDBG - may need to add cmd parameters here too */

    Increment_cmd_registered(&(sim->Cmds_table));

    cmd = cmd->next;
  }

  return REG_SUCCESS;
}

/*-------------------------------------------------------------------*/

int Send_control_msg_globus(Sim_entry_type *sim, char* buf)
{
  char            hdr_buf[REG_MAX_MSG_SIZE];
  globus_size_t   nbytes;
  globus_result_t result;

  /* This routine won't be called unless we think we do have an active
     connection to the simulation... */

  /* Send message header */
  sprintf(hdr_buf, REG_PACKET_FORMAT, REG_DATA_HEADER);

  result = globus_io_write(&(sim->socket_info.conn_handle), 
			   (globus_byte_t *)hdr_buf, 
			   strlen(hdr_buf), 
			   &nbytes);

  if(result != GLOBUS_SUCCESS){

    Globus_error_print(result);

    /* Try again in case application has dropped connection */
    
    Globus_attempt_connector_connect(&(sim->socket_info));

    if (sim->socket_info.comms_status != REG_COMMS_STATUS_CONNECTED) {
      return REG_FAILURE;
    }

    result = globus_io_write(&(sim->socket_info.conn_handle), 
			     (globus_byte_t *)hdr_buf, 
			     strlen(hdr_buf), 
			     &nbytes);

    if(result != GLOBUS_SUCCESS){

#if REG_DEBUG
      fprintf(stderr, "Send_control_msg_globus: globus_io_write "
	      "failed\n");
#endif
      Globus_error_print(result);
      return REG_FAILURE;
    }	
  }

  if( Emit_msg_header(&(sim->socket_info),
		      REG_CHAR,
		      strlen(buf)) != REG_SUCCESS){

#if REG_DEBUG
    fprintf(stderr, "Send_control_msg_globus: failed to send header\n");
#endif
    return REG_FAILURE;
  }

  /* Send message proper */

  result = globus_io_write(&(sim->socket_info.conn_handle), 
			   (globus_byte_t *)buf, 
			   strlen(buf), 
			   &nbytes);

  if(result != GLOBUS_SUCCESS){

#if REG_DEBUG
    fprintf(stderr, "Send_control_msg_globus: failed to send message\n");
#endif
    Globus_error_print(result);
    return REG_FAILURE;
  }

  /* and finally, the footer of the message */

  /* Send message header */
  sprintf(hdr_buf, REG_PACKET_FORMAT, REG_DATA_FOOTER);

  result = globus_io_write(&(sim->socket_info.conn_handle), 
			   (globus_byte_t *)hdr_buf, 
			   strlen(hdr_buf), 
			   &nbytes);

  if(result != GLOBUS_SUCCESS){

#if REG_DEBUG
    fprintf(stderr, "Send_control_msg_globus: failed to send footer\n");
#endif
    Globus_error_print(result);
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*--------------------------------------------------------------------*/

struct msg_struct *Get_status_msg_globus(Sim_entry_type *sim)
{
  struct msg_struct *msg = NULL;
  globus_size_t      nbytes;
  globus_result_t    result;
  char               buffer[REG_MAX_MSG_SIZE];
  int                type;
  int                count;

  /* if not connected attempt to connect now */
  if (sim->socket_info.comms_status 
      != REG_COMMS_STATUS_CONNECTED){
    Globus_attempt_connector_connect(&(sim->socket_info));
  }

  /* check if socket connection has been made */
  if (sim->socket_info.comms_status != REG_COMMS_STATUS_CONNECTED) {

#if REG_DEBUG
    fprintf(stderr, "Get_status_msg_globus: no socket connection\n");
#endif
    return NULL;
  }

  /* Check for data on socket - non-blocking */
  result = globus_io_try_read(&(sim->socket_info.conn_handle),
			      (globus_byte_t *)buffer,
			      REG_PACKET_SIZE,
			      &nbytes);

  if (result != GLOBUS_SUCCESS){

#if REG_DEBUG
    fprintf(stderr, "Get_status_msg_globus: globus_io_try_read failed\n");
#endif
    Globus_error_print(result);
    return NULL;
  }

#if REG_DEBUG
  fprintf(stderr, "Get_status_msg_globus: read <%s> from socket\n", buffer);
#endif

  /* ARPDBG - globus_io_try_read always returns 0 bytes if connection
     configugured to use GSSAPI or SSL data wrapping. */
  if(nbytes == 0){

    return NULL;
  }

  if(strncmp(buffer, REG_DATA_HEADER, strlen(REG_DATA_HEADER))){

#if REG_DEBUG
    fprintf(stderr, "Get_status_msg_globus: unrecognised header\n");
#endif
    return NULL;
  }

  if(Consume_msg_header(&(sim->socket_info),
			&type,
			&count) != REG_SUCCESS){

#if REG_DEBUG
    fprintf(stderr, "Get_status_msg_globus: failed to read msg header\n");
#endif
    return NULL;
  }

  if(type != REG_CHAR){

#if REG_DEBUG
    fprintf(stderr, "Get_status_msg_globus: message is of wrong "
	    "data type\n");
#endif
    return NULL;
  }

#if REG_DEBUG
  if(count > REG_MAX_MSG_SIZE){

    fprintf(stderr, "Get_status_msg_globus: message exceeds max. "
	    "length of %d chars\n", REG_MAX_MSG_SIZE);
    return NULL;
  }
#endif

  /* Read actual message plus footer (hence '+REG_PACKET_SIZE' below) */
  result = globus_io_read(&(sim->socket_info.conn_handle), 
			  (globus_byte_t *)buffer, 
			  count+REG_PACKET_SIZE, 
			  count+REG_PACKET_SIZE, 
			  &nbytes);

  if(result != GLOBUS_SUCCESS){

#if REG_DEBUG
    fprintf(stderr, "Get_status_msg_globus: error globus_io_read\n");
#endif
    Globus_error_print(result);
    return NULL;
  }

#if REG_DEBUG
  fprintf(stderr, "Get_status_msg_globus: read:\n>>%s<<\n", buffer);
#endif

  if(nbytes != (count+REG_PACKET_SIZE)){

#if REG_DEBUG
    fprintf(stderr, "Get_status_msg_globus: read %d bytes but expected "
	    "to get %d\n", nbytes, (count+REG_PACKET_SIZE));
#endif
    return NULL;
  }

  msg = New_msg_struct();

  if(Parse_xml_buf(buffer, count, msg) != REG_SUCCESS){

#if REG_DEBUG
    fprintf(stderr, "Get_status_msg_globus: failed to parse message\n");
#endif
    Delete_msg_struct(msg);
    msg = NULL;
  }

  return msg;
}

/*--------------------------------------------------------------------*/

int Finalize_connection_globus(Sim_entry_type *sim)
{
  /* close globus sockets */
  Globus_cleanup_connector_connection(&(sim->socket_info));
  Globus_socket_info_cleanup(&(sim->socket_info));

  return REG_SUCCESS;
}


#endif /* REG_GLOBUS_STEERING */
