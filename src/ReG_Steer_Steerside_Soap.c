/*----------------------------------------------------------------------------
  This file contains routines and data structures for SOAP-based 
  steering communication.

  (C) Copyright 2005, University of Manchester, United Kingdom,
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

  Authors........: Andrew Porter, Robert Haines

---------------------------------------------------------------------------*/
#include "ReG_Steer_Steerside.h"
#include "ReG_Steer_Steerside_internal.h"
#include "ReG_Steer_Steerside_Soap.h"

/** @file ReG_Steer_Steerside_Soap.c
    @brief Code for SOAP communications for the steering client.
  */

/*-------------------------------------------------------------------------*/

int Sim_attach_soap(Sim_entry_type *sim, char *SimID)
{
  struct sgs__AttachResponse attach_response;
  struct msg_struct *msg;
  struct cmd_struct *cmd;
  int   return_status;
  char *pchar = NULL;
  char *pchar1= NULL;

  /* malloc memory for soap struct for this connection and then
     initialise it */
  sim->SGS_info.soap = (struct soap*)malloc(sizeof(struct soap));
  if(!(sim->SGS_info.soap)){

    fprintf(stderr, "Sim_attach_soap: failed to malloc memory for "
	    "soap struct\n");
    return REG_FAILURE;
  }
  /* Use this form to turn-on keep-alive for both incoming and outgoing
     http connections */
  soap_init2(sim->SGS_info.soap, SOAP_IO_KEEPALIVE, SOAP_IO_KEEPALIVE);

  /* SimID holds the address of the soap server of the SGS */
  attach_response._AttachReturn = NULL;
  if(soap_call_sgs__Attach(sim->SGS_info.soap, SimID, 
			   "", &attach_response )){

    fprintf(stderr, "Sim_attach_soap: Attach to %s failed with message: \n",
	    SimID);
    soap_print_fault(sim->SGS_info.soap, stderr);

    Finalize_connection_soap(sim);

    return REG_FAILURE;
  }


#if REG_DEBUG
  if(attach_response._AttachReturn){
    fprintf(stderr, "Sim_attach_soap: Attach returned:\n>>%s<<\n",
	    attach_response._AttachReturn);
  }
  else{
    fprintf(stderr, "Sim_attach_soap: Attach returned null\n");
  }
#endif

  if(!attach_response._AttachReturn || strstr(attach_response._AttachReturn, 
					REG_SGS_ERROR)){
    Finalize_connection_soap(sim);
    return REG_FAILURE;
  }

  /* That worked OK so store address of SGS */
  sprintf(sim->SGS_info.address, SimID);

  /* get commands back and parse them... */
  if(strlen(attach_response._AttachReturn)){

    /* Strip-off outer tags (present because this is service data
       from the GS) */
    pchar = strstr(attach_response._AttachReturn, "<ReG_steer_message ");
    if (pchar) pchar1= strstr(attach_response._AttachReturn, 
				 "</ReG_steer_message>");
    if (!pchar1) {
      fprintf(stderr, "Sim_attach_soap: failed to strip root-element tags "
	      "from data:\n>>%s<<\n", attach_response._AttachReturn);
      Finalize_connection_soap(sim);
      return REG_FAILURE;
    }

    if(!(msg = New_msg_struct())){

      fprintf(stderr, "Sim_attach_soap: failed to get new msg struct\n");
      Finalize_connection_soap(sim);
      return REG_FAILURE;
    }

    /* strlen("</ReG_steer_message>") == 20 */
    return_status = Parse_xml_buf(pchar, 20 + (int)(pchar1 - pchar), msg, sim);

    if(return_status == REG_SUCCESS && msg->supp_cmd){

      cmd = msg->supp_cmd->first_cmd;

      while(cmd){

	sscanf((char *)(cmd->id), "%d", 
	       &(sim->Cmds_table.cmd[sim->Cmds_table.num_registered].cmd_id));

	/* ARPDBG - may need to add cmd parameters here too */

	Increment_cmd_registered(&(sim->Cmds_table));

	cmd = cmd->next;
      }
    }
    else{

      fprintf(stderr, "Sim_attach_soap: error parsing supported cmds\n");
    }

    Delete_msg_struct(&msg);

    return return_status;
  }
  else{
    Finalize_connection_soap(sim);
    return REG_FAILURE;
  }

}

/*-------------------------------------------------------------------------*/

int Send_control_msg_soap(Sim_entry_type *sim, char* buf)
{
  struct sgs__PutControlResponse putControl_response;

  /* Send message */
  putControl_response._PutControlReturn = NULL;
  if(soap_call_sgs__PutControl(sim->SGS_info.soap, sim->SGS_info.address, 
			       "", buf, &putControl_response )){

    fprintf(stderr, "Send_control_msg_soap: PutControl failed:\n");
    soap_print_fault(sim->SGS_info.soap, stderr);

    return REG_FAILURE;
  }

  if(putControl_response._PutControlReturn && 
     !strstr(putControl_response._PutControlReturn, REG_SGS_ERROR)) return REG_SUCCESS;

  return REG_FAILURE;
}

/*-------------------------------------------------------------------------*/

struct msg_struct *Get_status_msg_soap(Sim_entry_type *sim)
{
  struct sgs__GetNotificationsResponse getNotifications_response;
  struct sgs__GetStatusResponse        getStatus_response;
  struct msg_struct                   *msg = NULL;
  char                                *ptr;

  /* Check for pending notifications first */
  if(sim->SGS_info.sde_count > 0){

    sim->SGS_info.sde_count--;
    return Get_service_data(sim, 
			    sim->SGS_info.notifications[sim->SGS_info.sde_index++]); 
  }

  /* Check SGS for new notifications */
  getNotifications_response._GetNotificationsReturn = NULL;
  if(soap_call_sgs__GetNotifications(sim->SGS_info.soap, sim->SGS_info.address, 
				     "", &getNotifications_response )){

    fprintf(stderr, "Get_status_msg_soap: GetNotifications failed:\n");
    soap_print_fault(sim->SGS_info.soap, stderr);

    /* Flag that we hit an error as opposed to just failed to get a msg */
    msg = New_msg_struct();
    msg->msg_type = MSG_ERROR;
    return msg;
  }
  
#if REG_DEBUG
  if(getNotifications_response._GetNotificationsReturn){
    fprintf(stderr, "Get_status_msg_soap: GetNotifications returned >>%s<<\n",
	    getNotifications_response._GetNotificationsReturn);
  }
  else{
    fprintf(stderr, "Get_status_msg_soap: GetNotifications returned null\n");
  }
#endif

  /* GetNotifications returns a space-delimited list of the names of
     the SDE's that have changed since we last looked at them */
  if(getNotifications_response._GetNotificationsReturn &&
     !strstr(getNotifications_response._GetNotificationsReturn, REG_SGS_ERROR)){

    if( (ptr = strtok(getNotifications_response._GetNotificationsReturn, 
		      " ")) ){

      sim->SGS_info.sde_count = 0;
      while(ptr){

        if(sim->SGS_info.sde_count >= REG_MAX_NUM_SGS_SDE){

	  fprintf(stderr, "Get_status_msg_soap: ERROR - max. no. of "
		  "service data elements (%d) exceeded\n", 
		  REG_MAX_NUM_SGS_SDE);
	  break;
	}

	sprintf(sim->SGS_info.notifications[sim->SGS_info.sde_count++], "%s", ptr);

	ptr = strtok(NULL, " ");
      }

      /* sde_index holds the index of the notification to use _next time_ 
	 we call Get_service_data - the zeroth notification is always
	 dealt with here. */
      sim->SGS_info.sde_index = 1;
      sim->SGS_info.sde_count--;
      return Get_service_data(sim, sim->SGS_info.notifications[0]);
    }
  }

  /* Only ask for a status msg if had no notifications */
  getStatus_response._GetStatusReturn = NULL;
  if(soap_call_sgs__GetStatus(sim->SGS_info.soap, sim->SGS_info.address, 
			       "", &getStatus_response )){

    fprintf(stderr, "Get_status_msg_soap: GetStatus failed:\n");
    soap_print_fault(sim->SGS_info.soap, stderr);

    /* Flag that we hit an error as opposed to just failed to get a msg */
    msg = New_msg_struct();
    msg->msg_type = MSG_ERROR;
    return msg;
  }

#if REG_DEBUG
  if(getStatus_response._GetStatusReturn){
    fprintf(stderr, "Get_status_msg_soap: response: %s\n", 
	    getStatus_response._GetStatusReturn);
  }
  else{
    fprintf(stderr, "Get_status_msg_soap: null response\n");
  }
#endif

  if(getStatus_response._GetStatusReturn &&
     !strstr(getStatus_response._GetStatusReturn, REG_SGS_ERROR)){

    msg = New_msg_struct();

    if(Parse_xml_buf(getStatus_response._GetStatusReturn, 
		     strlen(getStatus_response._GetStatusReturn), 
		     msg, sim) != REG_SUCCESS){

      Delete_msg_struct(&msg);
    }
  }
  return msg;
}

/*-------------------------------------------------------------------------*/

struct msg_struct *Get_service_data(Sim_entry_type *sim, char *sde_name)
{
  struct sgs__findServiceDataResponse  findServiceData_response;
  struct msg_struct                   *msg = NULL;
  char   query_buf[REG_MAX_STRING_LENGTH];
  char  *pchar = NULL;
  char  *pchar1= NULL;

  findServiceData_response._findServiceDataReturn = NULL;
  sprintf(query_buf, "<ogsi:queryByServiceDataNames names=\"%s\"/>", 
	  sde_name);
  if(soap_call_sgs__findServiceData(sim->SGS_info.soap, sim->SGS_info.address, 
				    "", query_buf, 
				    &findServiceData_response )){

    fprintf(stderr, "Get_service_data: findServiceData failed:\n");
    soap_print_fault(sim->SGS_info.soap, stderr);

    msg = New_msg_struct();
    msg->msg_type = MSG_ERROR;
    return msg;
  }

#if REG_DEBUG
  if(findServiceData_response._findServiceDataReturn){
    fprintf(stderr, "Get_service_data: findServiceData returned: %s\n", 
	    findServiceData_response._findServiceDataReturn);
  }
  else{
    fprintf(stderr, "Get_service_data: findServiceData returned null\n");
  }
#endif

  if(findServiceData_response._findServiceDataReturn &&
     !strstr(findServiceData_response._findServiceDataReturn, REG_SGS_ERROR)){

    /* Not all SDEs will contain XML that we can parse - in particular, 
       those that hold the state of the steerer and application. */
    if(strstr(sde_name, REG_STEER_STATUS_SDE)){

      if(strstr(findServiceData_response._findServiceDataReturn, "DETACHED")){
	/* Convert from a notification that steerer is now detached
	   to a message that fits the library spec. */
	msg = New_msg_struct();
	msg->msg_type = STATUS;
	msg->status = New_status_struct();
	msg->status->first_cmd = New_cmd_struct();
	msg->status->cmd = msg->status->first_cmd;
	msg->status->cmd->name = (xmlChar *)xmlMalloc(REG_MAX_STRING_LENGTH);
	sprintf((char *)msg->status->cmd->name, "DETACH");
      }
    }
    else if(strstr(sde_name, REG_APP_STATUS_SDE)){

      if(strstr(findServiceData_response._findServiceDataReturn, "STOPPED")){
	/* Convert from a notification that application is now detached
	   to a message that fits the library spec. */
	msg = New_msg_struct();
	msg->msg_type = STATUS;
	msg->status = New_status_struct();
	msg->status->first_cmd = New_cmd_struct();
	msg->status->cmd = msg->status->first_cmd;
	msg->status->cmd->name = (xmlChar *)xmlMalloc(REG_MAX_STRING_LENGTH);
	sprintf((char *)msg->status->cmd->name, "STOP");
      }
    }
    else{

      /* Strip-off outer tags (present because this is service data
	 from the GS) */
      pchar = strstr(findServiceData_response._findServiceDataReturn, "<ReG_steer_message ");
      if (pchar) pchar1= strstr(findServiceData_response._findServiceDataReturn, 
				 "</ReG_steer_message>");
      if (!pchar1) {
	fprintf(stderr, "Get_service_data: failed to strip root-element tags "
		"from data:\n>>%s<<\n", findServiceData_response._findServiceDataReturn);
	return NULL;
      }
      msg = New_msg_struct();

      /* strlen("</ReG_steer_message>") == 20 */
      if(Parse_xml_buf(pchar,20+(int)(pchar1 - pchar), msg, sim) 
	 != REG_SUCCESS){

	Delete_msg_struct(&msg);
      }
    }
  }
  return msg;
}

/*-------------------------------------------------------------------------*/

int Send_pause_msg_soap(Sim_entry_type *sim)
{
  struct sgs__PauseResponse  pause_response;

#if REG_DEBUG
  fprintf(stderr, "Send_pause_msg_soap: calling Pause...\n");
#endif
  pause_response._PauseReturn = NULL;
  if(soap_call_sgs__Pause(sim->SGS_info.soap, sim->SGS_info.address, 
			   "", &pause_response )){

    fprintf(stderr, "Send_pause_msg_soap: Pause failed:\n");
    soap_print_fault(sim->SGS_info.soap, stderr);

    return REG_FAILURE;
  }

  if(pause_response._PauseReturn && 
     !strstr(pause_response._PauseReturn, REG_SGS_ERROR)){
     return REG_SUCCESS;
  }

  return REG_FAILURE;
}

/*-------------------------------------------------------------------------*/

int Send_resume_msg_soap(Sim_entry_type *sim)
{
  struct sgs__ResumeResponse  resume_response;

#if REG_DEBUG
  fprintf(stderr, "Send_resume_msg_soap: calling Resume...\n");
#endif
  resume_response._ResumeReturn = NULL;
  if(soap_call_sgs__Resume(sim->SGS_info.soap, sim->SGS_info.address, 
			   "", &resume_response )){

    fprintf(stderr, "Send_resume_msg_soap: Resume failed:\n");
    soap_print_fault(sim->SGS_info.soap, stderr);

    return REG_FAILURE;
  }

  if(resume_response._ResumeReturn && 
     !strstr(resume_response._ResumeReturn, REG_SGS_ERROR)){
     return REG_SUCCESS;
  }

  return REG_FAILURE;
}

/*-------------------------------------------------------------------------*/

int Send_detach_msg_soap(Sim_entry_type *sim)
{
  struct sgs__DetachResponse  detach_response;

#if REG_DEBUG
  fprintf(stderr, "Send_detach_msg_soap: calling Detach...\n");
#endif
  detach_response._DetachReturn = NULL;
  if(soap_call_sgs__Detach(sim->SGS_info.soap, sim->SGS_info.address, 
			   "", &detach_response )){

    fprintf(stderr, "Send_detach_msg_soap: Detach failed:\n");
    soap_print_fault(sim->SGS_info.soap, stderr);

    return REG_FAILURE;
  }

  if(detach_response._DetachReturn && 
     !strstr(detach_response._DetachReturn, REG_SGS_ERROR)){
     return REG_SUCCESS;
  }

  return REG_FAILURE;
}

/*-------------------------------------------------------------------------*/

int Send_stop_msg_soap(Sim_entry_type *sim)
{
  struct sgs__StopResponse  stop_response;

#if REG_DEBUG
  fprintf(stderr, "Send_stop_msg_soap: calling Stop...\n");
#endif
  stop_response._StopReturn = NULL;
  if(soap_call_sgs__Stop(sim->SGS_info.soap, sim->SGS_info.address, 
			   "", &stop_response )){

    fprintf(stderr, "Send_stop_msg_soap: Stop failed:\n");
    soap_print_fault(sim->SGS_info.soap, stderr);

    return REG_FAILURE;
  }

  if(stop_response._StopReturn && 
     !strstr(stop_response._StopReturn, REG_SGS_ERROR)){
     return REG_SUCCESS;
  }

  return REG_FAILURE;
}

/*-------------------------------------------------------------------------*/

int Send_restart_msg_soap(Sim_entry_type *sim, char *chkGSH)
{
  struct sgs__RestartResponse  restart_response;

  restart_response._RestartReturn = NULL;
  if(soap_call_sgs__Restart(sim->SGS_info.soap, sim->SGS_info.address, 
			   "", chkGSH, &restart_response )){

    fprintf(stderr, "Send_restart_msg_soap: Restart failed:\n");
    soap_print_fault(sim->SGS_info.soap, stderr);

    return REG_FAILURE;
  }

  if(restart_response._RestartReturn && 
     !strstr(restart_response._RestartReturn, REG_SGS_ERROR)){
     return REG_SUCCESS;
  }

  return REG_FAILURE;
}

/*-------------------------------------------------------------------------*/
/** Clean up a SOAP-based steering connection */
int Finalize_connection_soap(Sim_entry_type *sim)
{
  /* Release memory */
  soap_end(sim->SGS_info.soap);
  free(sim->SGS_info.soap);
  sim->SGS_info.soap = NULL;

  return REG_SUCCESS;  
}

/*-------------------------------------------------------------------------*/

int Get_param_log_soap(Sim_entry_type *sim, int handle)
{
  struct sgs__GetParamLogResponse response;
  int    index, lindex;
  char  *ptr1;
  int    dum_int;
  float  dum_float;
  response._GetParamLogReturn = NULL;

  if( (index=Param_index_from_handle(&(sim->Params_table),
				     handle)) == -1){

      fprintf(stderr, "Get_param_log_soap: failed to match param handle\n");
      fprintf(stderr, "                    handle = %d\n", handle);

      return REG_FAILURE;
  }

  if(!sim->Params_table.param[index].log){
    
    if(Realloc_param_log(&(sim->Params_table.param[index])) !=
       REG_SUCCESS){

      return REG_FAILURE;
    }
  }

  lindex = sim->Params_table.param[index].log_index;


  if(soap_call_sgs__GetParamLog(sim->SGS_info.soap, sim->SGS_info.address, 
				"", (xsd__int)handle, &response )){

    fprintf(stderr, "Get_param_log_soap: failed for handle %d:\n",
	    handle);
    soap_print_fault(sim->SGS_info.soap, stderr);

    return REG_FAILURE;
  }

  if(!(response._GetParamLogReturn) ||
     strstr(response._GetParamLogReturn, REG_SGS_ERROR)){
     return REG_FAILURE;
  }

  fprintf(stderr, "ARPDBG, got log from SGS>>%s<<", 
	  (char*)response._GetParamLogReturn);
  ptr1 = (char*)response._GetParamLogReturn;

  switch(sim->Params_table.param[index].type){

  case REG_INT:

    while(1){

      /* Parse space-delimited list of parameter values */
      sscanf(ptr1, "%d ", &dum_int);
      sim->Params_table.param[index].log[lindex++] = (double)dum_int;

      if(!(ptr1 = strstr(ptr1, " ")))break;
      if(*(++ptr1) == '\0')break;

      if(lindex >= sim->Params_table.param[index].log_size){
	Realloc_param_log(&(sim->Params_table.param[index]));
      }
    }
    sim->Params_table.param[index].log_index = lindex;
    break;

  case REG_FLOAT:

    while(1){

      sscanf(ptr1, "%f", &dum_float);
      sim->Params_table.param[index].log[lindex++] = (double)dum_float;

      if(!(ptr1 = strstr(ptr1, " ")))break;
      if(*(++ptr1) == '\0')break;

      if(lindex >= sim->Params_table.param[index].log_size){
	Realloc_param_log(&(sim->Params_table.param[index]));
      }
    }
    sim->Params_table.param[index].log_index = lindex;
    break;

  case REG_DBL:

    while(1){

      sscanf(ptr1, "%lg", &(sim->Params_table.param[index].log[lindex++]) );

      if(!(ptr1 = strstr(ptr1, " ")))break;
      if(*(++ptr1) == '\0')break;

      if(lindex >= sim->Params_table.param[index].log_size){
	Realloc_param_log(&(sim->Params_table.param[index]));
      }
    }
    sim->Params_table.param[index].log_index = lindex;
    break;

  case REG_CHAR:
    /* This not implemented yet */
#if REG_DEBUG
    fprintf(stderr, "Get_param_log_soap: logging of char params not "
	    "implemented!\n");
#endif
    break;

  default:
    break;
  }

  return REG_SUCCESS;
}
