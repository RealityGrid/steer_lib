/*----------------------------------------------------------------------------
    This file contains routines and data structures for SOAP-based 
    steering communication.

    (C)Copyright 2003, The University of Manchester, United Kingdom,
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

    Initial version by:  A Porter, 23.4.2003       0.1               

---------------------------------------------------------------------------*/
#include "ReG_Steer_Steerside.h"
#include "ReG_Steer_Steerside_internal.h"
#include "ReG_Steer_Steerside_Soap.h"
#include "soapH.h"
#include "soapSGS.nsmap"

struct soap soap;

/*-------------------------------------------------------------------------*/

int Steerer_initialize_soap()
{
  soap_init(&soap);

  return REG_SUCCESS;
}

/*-------------------------------------------------------------------------*/

int Sim_attach_soap(Sim_entry_type *sim, char *SimID)
{
  struct tns__AttachResponse attach_response;
  struct msg_struct *msg;
  struct cmd_struct *cmd;
  int return_status;
  int nbytes;

  /* SimID holds the address of the soap server of the SGS */
  if(soap_call_tns__Attach(&soap, SimID, 
			   "", &attach_response )){

    fprintf(stderr, "Sim_attach_soap: Attach to %s failed with message: \n",
	    SimID);
    soap_print_fault(&soap, stderr);

    return REG_FAILURE;
  }

  /* That worked OK so store address of SGS */
  sprintf(sim->SGS_info.address, SimID);

#if DEBUG
  fprintf(stderr, "Sim_attach_soap: Attach returned:\n>>%s<<\n",
	  attach_response._result);
#endif

  if(!attach_response._result)return REG_FAILURE;

  /* get commands back and parse them... */
  if(nbytes = strlen(attach_response._result)){

    msg = New_msg_struct();

    return_status = Parse_xml_buf(attach_response._result, nbytes, msg);

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

    Delete_msg_struct(msg);

    return return_status;
  }
  else{
    return REG_FAILURE;
  }

}

/*-------------------------------------------------------------------------*/

int Send_control_msg_soap(Sim_entry_type *sim, char* buf)
{
  struct tns__PutControlResponse putControl_response;

  fprintf(stderr, "Send_control_msg_soap: address = %s\n", sim->SGS_info.address);

  /* Send message */
  if(soap_call_tns__PutControl(&soap, sim->SGS_info.address, 
			       "", buf, &putControl_response )){

    fprintf(stderr, "Send_control_msg_soap: PutControl failed:\n");
    soap_print_fault(&soap, stderr);

    return REG_FAILURE;
  }

  fprintf(stderr, "Send_control_msg_soap: PutControl returned: %s\n", 
	  putControl_response._result);

  return REG_SUCCESS;
}

/*-------------------------------------------------------------------------*/

struct msg_struct *Get_status_msg_soap(Sim_entry_type *sim)
{
  struct tns__GetNotificationsResponse getNotifications_response;
  struct tns__GetStatusResponse        getStatus_response;
  struct msg_struct                   *msg = NULL;
  char                                *ptr;

  fprintf(stderr, "Get_status_msg_soap: sde_count = %d\n", sim->SGS_info.sde_count);

  /* Check for pending notifications first */
  if(sim->SGS_info.sde_count > 0){

    sim->SGS_info.sde_count--;
    return Get_service_data(sim, 
			    sim->SGS_info.notifications[sim->SGS_info.sde_index++]); 
  }
  fprintf(stderr, "Get_status_msg_soap: address = %s\n", sim->SGS_info.address);

  /* Check SGS for new notifications */
  if(soap_call_tns__GetNotifications(&soap, sim->SGS_info.address, 
				     "", &getNotifications_response )){

    fprintf(stderr, "Get_status_msg_soap: GetNotifications failed:\n");
    soap_print_fault(&soap, stderr);

    return NULL;
  }
  
#if DEBUG
  fprintf(stderr, "Get_status_msg_soap: GetNotifications returned >>%s<<\n",
	  getNotifications_response._result);
#endif

  /* GetNotifications returns a space-delimited list of the names of
     the SDE's that have changed since we last looked at them */
  if(getNotifications_response._result){

    if(ptr = strtok(getNotifications_response._result, " ")){

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
  if(soap_call_tns__GetStatus(&soap, sim->SGS_info.address, 
			       "", &getStatus_response )){

    fprintf(stderr, "Get_status_msg_soap: GetStatus failed:\n");
    soap_print_fault(&soap, stderr);

    return NULL;
  }

#if DEBUG
  fprintf(stderr, "Get_status_msg_soap: response: %s\n", 
	  getStatus_response._result);
#endif

  if(getStatus_response._result){
    msg = New_msg_struct();

    if(Parse_xml_buf(getStatus_response._result, 
		     strlen(getStatus_response._result), msg) != REG_SUCCESS){

      Delete_msg_struct(msg);
      msg = NULL;
    }
  }
  return msg;
}

/*-------------------------------------------------------------------------*/

struct msg_struct *Get_service_data(Sim_entry_type *sim, char *sde_name)
{
  struct tns__FindServiceDataResponse  findServiceData_response;
  struct msg_struct                   *msg = NULL;

#if DEBUG
  fprintf(stderr, "Get_service_data: calling FindServiceData for %s\n", 
	  sde_name);
#endif

  if(soap_call_tns__FindServiceData(&soap, sim->SGS_info.address, 
				    "", sde_name, 
				    &findServiceData_response )){

    fprintf(stderr, "Get_service_data: FindServiceData failed:\n");
    soap_print_fault(&soap, stderr);

    return NULL;
  }

#if DEBUG
  fprintf(stderr, "Get_service_data: FindServiceData returned: %s\n", 
	  findServiceData_response._result);
#endif

  if(findServiceData_response._result){

    /* Not all SDEs will contain XML that we can parse - in particular, 
       those that hold the state of the steerer and application. */
    if(strstr(sde_name, REG_STEER_STATUS_SDE)){

      if(strstr(findServiceData_response._result, "DETACHED")){
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

      if(strstr(findServiceData_response._result, "STOPPED")){
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
      msg = New_msg_struct();

      if(Parse_xml_buf(findServiceData_response._result, 
		       strlen(findServiceData_response._result), 
		       msg) != REG_SUCCESS){

	Delete_msg_struct(msg);
	msg = NULL;
      }
    }
  }
  return msg;
}

/*-------------------------------------------------------------------------*/

int Send_pause_msg_soap(Sim_entry_type *sim)
{
  struct tns__PauseResponse  pause_response;

#if DEBUG
  fprintf(stderr, "Send_pause_msg_soap: calling Pause...\n");
#endif

  if(soap_call_tns__Pause(&soap, sim->SGS_info.address, 
			   "", &pause_response )){

    fprintf(stderr, "Send_pause_msg_soap: Pause failed:\n");
    soap_print_fault(&soap, stderr);

    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*-------------------------------------------------------------------------*/

int Send_resume_msg_soap(Sim_entry_type *sim)
{
  struct tns__ResumeResponse  resume_response;

#if DEBUG
  fprintf(stderr, "Send_resume_msg_soap: calling Resume...\n");
#endif

  if(soap_call_tns__Resume(&soap, sim->SGS_info.address, 
			   "", &resume_response )){

    fprintf(stderr, "Send_resume_msg_soap: Resume failed:\n");
    soap_print_fault(&soap, stderr);

    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*-------------------------------------------------------------------------*/

int Send_detach_msg_soap(Sim_entry_type *sim)
{
  struct tns__DetachResponse  detach_response;

#if DEBUG
  fprintf(stderr, "Send_detach_msg_soap: calling Detach...\n");
#endif

  if(soap_call_tns__Detach(&soap, sim->SGS_info.address, 
			   "", &detach_response )){

    fprintf(stderr, "Send_detach_msg_soap: Detach failed:\n");
    soap_print_fault(&soap, stderr);

    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*-------------------------------------------------------------------------*/

int Send_stop_msg_soap(Sim_entry_type *sim)
{
  struct tns__StopResponse  stop_response;

#if DEBUG
  fprintf(stderr, "Send_stop_msg_soap: calling Stop...\n");
#endif

  if(soap_call_tns__Stop(&soap, sim->SGS_info.address, 
			   "", &stop_response )){

    fprintf(stderr, "Send_stop_msg_soap: Stop failed:\n");
    soap_print_fault(&soap, stderr);

    return REG_FAILURE;
  }

  return REG_SUCCESS;
}
