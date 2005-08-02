/*----------------------------------------------------------------------------
  This file contains routines and data structures for SOAP-based 
  steering communication using the WSRF specification.

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

  Authors........: Andrew Porter

---------------------------------------------------------------------------*/
#include "ReG_Steer_Steerside.h"
#include "ReG_Steer_Steerside_internal.h"
#include "ReG_Steer_Steerside_WSRF.h"

/** @file ReG_Steer_Steerside_WSRF.c
    @brief Code for WSRF over SOAP communications for the steering client.
  */

/** The last modified time of the ResourceProperty document of our
    associated SWS when we last looked at it. */
long int ReG_lastModTime = 0;

/* Actual declaration is in ReG_Steer_Appside.c */
extern struct msg_store_struct Msg_store;

/*------------------------------------------------------------------*/
/* Handler for unrecognised tags in gSoap */
int soapMismatchHandler(struct soap *soap, 
			const char *tag)
{ 
  printf("soapMismatchHandler: tag = %s\n", tag);
  /*return SOAP_TAG_MISMATCH;	 every tag must be handled */
  return SOAP_OK;	/* We're just curious */
}

/*------------------------------------------------------------------*/
/** Attach to a simulation */
int Sim_attach_wsrf (Sim_entry_type *sim, char *SimID){

  /*  struct sws__AttachResponse response;*/
  char              *response;
  struct msg_struct *msg;
  struct cmd_struct *cmd;
  int                return_status;
  int                len;

  /* malloc memory for soap struct for this connection and then
     initialise it */
  sim->SGS_info.soap = (struct soap*)malloc(sizeof(struct soap));
  if(!(sim->SGS_info.soap)){

    fprintf(stderr, "Sim_attach_wsrf: failed to malloc memory for "
	    "soap struct\n");
    return REG_FAILURE;
  }
  /* Use this form to turn-on keep-alive for both incoming and outgoing
     http connections */
  soap_init2(sim->SGS_info.soap, SOAP_IO_KEEPALIVE, SOAP_IO_KEEPALIVE);

#if REG_DEBUG
  /* ARPDBG Ptr to a handler so we can see which tags we're ignoring */
  sim->SGS_info.soap->fignore = soapMismatchHandler;
#endif

  printf("Calling Attach...\n");
  if(soap_call_sws__Attach((sim->SGS_info.soap), SimID, "", NULL, 
			   &response) != SOAP_OK){
    soap_print_fault((sim->SGS_info.soap), stderr);
    Finalize_connection_wsrf(sim);
    return REG_FAILURE;
  }
  printf("...done Attach\n");
  /*printf("Attach returned: %s\n", response._AttachReturn);*/
  printf("Attach returned: %s\n", response);

  if(strstr(response, "<faultcode>")){
    fprintf(stderr, "Attach returned fault: %s\n", response);
    Finalize_connection_wsrf(sim);
    return REG_FAILURE;
  }

  /* That worked OK so store address of SWS */
  sprintf(sim->SGS_info.address, SimID);

  /* Parse the supported commands */
  len = strlen(response);
  if(len){
    if(!(msg = New_msg_struct())){

      fprintf(stderr, "Sim_attach_wsrf: failed to get new msg struct\n");
      Finalize_connection_wsrf(sim);
      return REG_FAILURE;
    }

    return_status = Parse_xml_buf(response, len, msg, sim);

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
      fprintf(stderr, "Sim_attach_wsrf: error parsing supported cmds\n");
    }

    Delete_msg_struct(&msg);

    return return_status;
  }
  else{
    Finalize_connection_wsrf(sim);
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*------------------------------------------------------------------*/
/** Send a control msg to a simulation */
int Send_control_msg_wsrf (Sim_entry_type *sim, char *buf){

  char   inputBuf[REG_MAX_MSG_SIZE + 32];
  struct wsrp__SetResourcePropertiesResponse out;

  snprintf(inputBuf, REG_MAX_MSG_SIZE + 32,
	   "<controlMsg>%s</controlMsg>", buf);

  if(soap_call_wsrp__SetResourceProperties((sim->SGS_info.soap), 
					   sim->SGS_info.address, 
					   "", inputBuf, &out) != SOAP_OK){
    soap_print_fault((sim->SGS_info.soap), stderr);
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*------------------------------------------------------------------*/
/** Get the latest status msg from the simulation */
struct msg_struct *Get_status_msg_wsrf(Sim_entry_type *sim)
{
  char *pRPDoc;
  char buf[REG_MAX_MSG_SIZE];
  long int modTime;
  struct msg_struct *msg = NULL;

  /* If we have a backlog of messages then return the next one */
  msg = Get_next_stored_msg(sim);
  if(msg)return msg;

  if( Get_resource_property_doc(&(sim->SGS_info), &pRPDoc) != REG_SUCCESS){

    msg = New_msg_struct();
    msg->msg_type = MSG_ERROR;
    return msg;
  }


  /* Check for changes to RP first (a.k.a. notifications) */
  if(Extract_resource_property(pRPDoc, strlen(pRPDoc),
			       "sws:lastModifiedTime", 
			       buf, REG_MAX_MSG_SIZE) != REG_SUCCESS){
    msg = NULL;
    return msg;
  }

  modTime = atoi(buf);

  /*fprintf(stderr, "Get_status_msg_wsrf: modified time = %d\n", modTime);*/
  if(modTime != ReG_lastModTime){
    ReG_lastModTime=modTime;

    /* Parse the whole doc; messages are stored in the Msg_store struct
       associated with the sim entry */
    if(Parse_xml_buf(pRPDoc, strlen(pRPDoc), NULL, sim) != REG_SUCCESS){

      Delete_msg_struct(&msg);
    }
    return Get_next_stored_msg(sim);
  }
  else{
    /* Just get the latest status message from the app */
    if(Extract_resource_property(pRPDoc, strlen(pRPDoc),
				 "sws:latestStatusMsg", 
				 buf, REG_MAX_MSG_SIZE) == REG_SUCCESS){

      msg = New_msg_struct();
      if(Parse_xml_buf(buf, strlen(buf), msg, sim) != REG_SUCCESS){

	Delete_msg_struct(&msg);
      }
    }
  }

  return msg;
}

/*------------------------------------------------------------------*/
/** Retrieve the next stored message (if any) from the simulation */
struct msg_struct *Get_next_stored_msg(Sim_entry_type *sim)
{
  struct msg_struct *msg = NULL;
  struct msg_store_struct *msgStorePtr = &Msg_store;
  struct msg_store_struct *tmp;

  if(sim){
    msgStorePtr = &(sim->Msg_store);
  }

  if(msgStorePtr->msg){
    fprintf(stderr, "Get_next_stored_msg: ARPDBG: retrieving a stored msg\n");

    /* Take a copy of the pointer to the oldest stored message */
    msg = msgStorePtr->msg;
    /* Then shift pointers to point at the next one, if any */
    if(msgStorePtr->next){
      tmp = msgStorePtr->next;
      msgStorePtr->msg = tmp->msg;
      msgStorePtr->next = tmp->next;
      /* Delete the struct we've just copied ptrs from */
      free(tmp);
    }
    else{
      msgStorePtr->msg = NULL;
      msgStorePtr->next = NULL;
    }
  }
  return msg;
}

/*------------------------------------------------------------------*/
/** Get the value of the specified resource property */
int Get_resource_property (SGS_info_type *sgs_info,
			   char *name,
			   char **pRP)
{
  struct wsrp__GetMultipleResourcePropertiesRequest in;
  char  *out;
#ifdef USE_REG_TIMING
  double time0, time1;
#endif
  *pRP = NULL;
  in.__size = 1;
  in.__ptr = (struct wsrp__ResourcePropertyStruct *)malloc(in.__size*
							  sizeof(struct wsrp__ResourcePropertyStruct));

  
  in.__ptr[0].ResourceProperty = (char *)malloc(128*sizeof(char));
  strcpy(in.__ptr[0].ResourceProperty, name);

#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time0);
#endif

  if(soap_call_wsrp__GetMultipleResourceProperties(sgs_info->soap, 
						   sgs_info->address, 
						   "", in,
						   &out) != SOAP_OK){
    soap_print_fault(sgs_info->soap, stderr);
    free(in.__ptr[0].ResourceProperty);
    return REG_FAILURE;
  }
#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time1);
  fprintf(stderr, "TIMING: soap_call_wsrp__GetMultipleResourceProperties "
	  "took %f seconds\n", (time1-time0));
#endif

#if REG_DEBUG_FULL
  printf("Get_resource_property for %s returned >>%s<<\n", name, out);
#endif
  free(in.__ptr[0].ResourceProperty);
  *pRP = out;
  return REG_SUCCESS;
}

/*------------------------------------------------------------------*/
/** Get the whole resource property document */
int Get_resource_property_doc(SGS_info_type *sgs_info,
				char **pDoc)
{
  char *out;
#ifdef USE_REG_TIMING
  double time0, time1;
#endif

  *pDoc = NULL;

#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time0);
#endif
  if(soap_call_wsrp__GetResourcePropertyDocument((sgs_info->soap), 
						 sgs_info->address, 
						 "", NULL,
						 &out) != SOAP_OK){
    soap_print_fault(sgs_info->soap, stderr);
    return REG_FAILURE;
  }
#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time1);
  fprintf(stderr, "TIMING: soap_call_wsrp__GetResourcePropertyDocument "
	  "took %f seconds\n", (time1-time0));
#endif

#if REG_DEBUG_FULL
  printf("GetResourcePropertyDocument returned: %s\n", out);
#endif
  *pDoc = out;
  return REG_SUCCESS;
}

/*------------------------------------------------------------------*/
/** Send a detach msg to a simulation */
int Send_detach_msg_wsrf (Sim_entry_type *sim){

#if REG_DEBUG
  fprintf(stderr, "Send_detach_msg_wsrf: calling Detach...\n");
#endif

  if(soap_call_sws__Detach(sim->SGS_info.soap, sim->SGS_info.address, 
			   "", NULL, NULL )){

    fprintf(stderr, "Send_detach_msg_wsrf: Detach failed:\n");
    soap_print_fault(sim->SGS_info.soap, stderr);

    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*------------------------------------------------------------------*/
/** Clean up a WSRF-based steering connection */
int Finalize_connection_wsrf (Sim_entry_type *sim)
{
  /* Release memory */
  soap_end(sim->SGS_info.soap);
  free(sim->SGS_info.soap);
  sim->SGS_info.soap = NULL;

  return REG_SUCCESS;  
}

/*------------------------------------------------------------------*/
/** Retrieve the full log of the specified parameter */
int Get_param_log_wsrf(Sim_entry_type *sim,
		       int             handle)
{
  struct sws__GetParamLogResponse response;
  int    index, lindex;
  char  *ptr1;
  int    dum_int;
  float  dum_float;
#ifdef USE_REG_TIMING
  double time0, time1;
#endif
  response._GetParamLogReturn = NULL;

  if( (index=Param_index_from_handle(&(sim->Params_table),
				     handle)) == -1){

      fprintf(stderr, "Get_param_log_wsrf: failed to match param handle\n");
      fprintf(stderr, "                    handle = %d\n", handle);

      return REG_FAILURE;
  }

  if(!sim->Params_table.param[index].log){
    
    if(Realloc_param_log(&(sim->Params_table.param[index])) !=
       REG_SUCCESS){

      return REG_MEM_FAIL;
    }
  }

#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time0);
#endif
  if(soap_call_sws__GetParamLog(sim->SGS_info.soap, sim->SGS_info.address, 
				"", (xsd__int)handle, &response )){
#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time1);
  fprintf(stderr, "TIMING: soap_call_sws__GetParamLog "
	  "took %f seconds\n", (time1-time0));
#endif

    fprintf(stderr, "Get_param_log_wsrf: failed for handle %d:\n",
	    handle);
    soap_print_fault(sim->SGS_info.soap, stderr);

    return REG_FAILURE;
  }

  if( !(response._GetParamLogReturn) ){
    fprintf(stderr, "Get_param_log_wsrf: no log entries returned\n");
    return REG_FAILURE;
  }

  /*fprintf(stderr, "ARPDBG, got log from SWS>>%s<<",
    (char*)response._GetParamLogReturn);*/
  ptr1 = (char*)response._GetParamLogReturn;
  lindex = sim->Params_table.param[index].log_index;

  switch(sim->Params_table.param[index].type){

  case REG_INT:

    while(1){

      /* Parse space-delimited list of parameter values */
      sscanf(ptr1, "%d ", &dum_int);
      sim->Params_table.param[index].log[lindex++] = (double)dum_int;

      if(!(ptr1 = strchr(ptr1, ' ')))break;
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

      if(!(ptr1 = strchr(ptr1, ' ')))break;
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

      if(!(ptr1 = strchr(ptr1, ' ')))break;
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
    fprintf(stderr, "Get_param_log_wsrf: logging of char params not "
	    "implemented!\n");
#endif
    break;

  default:
    break;
  }

  return REG_SUCCESS;
}

/*------------------------------------------------------------------*/
/** Instruct the simulation to restart from the specified node
    of a checkpoint tree */
int Send_restart_msg_wsrf(Sim_entry_type *sim, char *chkGSH)
{
  fprintf(stderr, "Send_restart_msg_wsrf: IMPLEMENT ME\n");
  return REG_FAILURE;
}
