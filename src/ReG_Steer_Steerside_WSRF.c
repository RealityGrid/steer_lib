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
#include "ReG_Steer_Steerside.h"
#include "ReG_Steer_Steerside_internal.h"
#include "ReG_Steer_Steerside_WSRF.h"
#include "ReG_Steer_Utils_WSRF.h"

/** @file ReG_Steer_Steerside_WSRF.c
    @brief Code for WSRF over SOAP communications for the steering client.
    @author Andrew Porter
*/

/* Actual declaration is in ReG_Steer_Appside.c */
extern struct msg_store_struct  Msg_store;
extern struct msg_store_struct *Msg_store_tail;
/** Table holding general configuration info. is declared in 
    ReG_Steer_Steerside.c */
extern Steerer_config_table_type Steer_config;

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
int Sim_attach_wsrf (Sim_entry_type *sim, char *SimID){

  struct sws__AttachResponse response;
  int i;

  sim->SGS_info.lastModTime = 0;

  /* malloc memory for soap struct for this connection and then
     initialise it */
  sim->SGS_info.soap = (struct soap*)malloc(sizeof(struct soap));
  if(!(sim->SGS_info.soap)){

    fprintf(stderr, "STEER: Sim_attach_wsrf: failed to malloc memory for "
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

  /* If address of SWS begins with 'https' then initialize SSL context */
  if(strstr(SimID, "https") == SimID){
    if( REG_Init_ssl_context(sim->SGS_info.soap,
			     REG_TRUE, /* Authenticate SWS */
			     NULL,/*char *certKeyPemFile,*/
			     NULL, /* char *passphrase,*/
			     Steer_config.caCertsPath) == REG_FAILURE){

      fprintf(stderr, "STEER: ERROR: Sim_attach_wsrf: call to initialize "
	      "soap SSL context failed\n");
      return REG_FAILURE;
    }
  }

  if(Create_WSRF_header(sim->SGS_info.soap, 
			SimID,
			sim->SGS_info.username, 
			sim->SGS_info.passwd) != REG_SUCCESS){
    return REG_FAILURE;
  }

  if(soap_call_sws__Attach(sim->SGS_info.soap, SimID, "", NULL, 
			   &response) != SOAP_OK){
    soap_print_fault((sim->SGS_info.soap), stderr);
    Finalize_connection_wsrf(sim);
    return REG_FAILURE;
  }

  /* That worked OK so store address of SWS */
  sprintf(sim->SGS_info.address, SimID);

  for(i=0; i<response.ReG_USCOREsteer_USCOREmessage.Supported_USCOREcommands.__size; i++){
    sim->Cmds_table.cmd[sim->Cmds_table.num_registered].cmd_id = 
      response.ReG_USCOREsteer_USCOREmessage.Supported_USCOREcommands.__ptr[i].Cmd_USCOREid;

    /* ARPDBG - may need to add cmd parameters here too */
    Increment_cmd_registered(&(sim->Cmds_table));
  }

  if( !(response.ReG_USCOREsteer_USCOREmessage.Supported_USCOREcommands.__size) ){
    /* Cannot have no supported commands - detach must be supported so this
       is an error */
    fprintf(stderr, "STEER: ERROR: Attach: no supported commands returned\n");
    Finalize_connection_wsrf(sim);
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*------------------------------------------------------------------*/
int Send_control_msg_wsrf (Sim_entry_type *sim, char *buf){

  char   inputBuf[REG_MAX_MSG_SIZE + 32];

  snprintf(inputBuf, REG_MAX_MSG_SIZE + 32,
	   "<controlMsg>%s</controlMsg>", buf);

  return Set_resource_property (sim->SGS_info.soap,
				sim->SGS_info.address,
				sim->SGS_info.username,
				sim->SGS_info.passwd,
				inputBuf);
}

/*------------------------------------------------------------------*/
struct msg_struct *Get_status_msg_wsrf(Sim_entry_type *sim)
{
  char *pRPDoc;
  char buf[REG_MAX_MSG_SIZE];
  long int modTime;
  struct msg_struct *msg = NULL;

  /* If we have a backlog of messages then return the next one */
  if( (msg = Get_next_stored_msg(sim)) ){
    return msg;
  }

  if( Get_resource_property_doc(sim->SGS_info.soap, 
				sim->SGS_info.address,
				sim->SGS_info.username,
				sim->SGS_info.passwd,
				&pRPDoc) != REG_SUCCESS){

    msg = New_msg_struct();
    msg->msg_type = MSG_ERROR;
    return msg;
  }

  /* Check for changes to RP first (a.k.a. notifications) */
  if(Extract_resource_property(pRPDoc, strlen(pRPDoc),
			       "sws:lastModifiedTime", 
			       buf, REG_MAX_MSG_SIZE) != REG_SUCCESS){
    fprintf(stderr, "STEER: Get_status_msg_wsrf: failed to get "
	    "lastModifiedTime from ResourceProperty document\n");
    return NULL;
  }

  modTime = atoi(buf);

  if(modTime != sim->SGS_info.lastModTime){
    sim->SGS_info.lastModTime=modTime;

    /* Parse the whole doc; messages are stored in the Msg_store struct
       associated with the sim entry */
    Parse_xml_buf(pRPDoc, strlen(pRPDoc), NULL, sim);
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
	return NULL;
      }
    }
  }

  return msg;
}

/*------------------------------------------------------------------*/
struct msg_struct *Get_next_stored_msg(Sim_entry_type *sim)
{
  struct msg_struct *msg = NULL;
  struct msg_store_struct *msgStorePtr = &Msg_store;
  struct msg_store_struct *msgStoreTailPtr = Msg_store_tail;
  struct msg_store_struct *tmp;

  if(sim){
    msgStorePtr = &(sim->Msg_store);
    msgStoreTailPtr = sim->Msg_store_tail;
  }

  if(msgStorePtr->msg){
    
    /* Take a copy of the pointer to the oldest stored message */
    msg = msgStorePtr->msg;

    /* Then shift pointers to point at the next one, if any */
    if( (tmp = msgStorePtr->next) ){
      msgStorePtr->msg = tmp->msg;
      msgStorePtr->next = tmp->next;
      if(tmp == msgStoreTailPtr){
	msgStoreTailPtr = msgStorePtr;
      }
      /* Delete the struct we've just copied ptrs from */
      free(tmp);
      tmp = NULL;
    }
    else{
#if REG_DEBUG
      fprintf(stderr, "STEER: Get_next_stored_msg: hit end of msg store\n");
#endif
      msgStorePtr->msg = NULL;
      msgStorePtr->next = NULL;
      /* Make sure the ptr to the tail of the list is
	 reset too */
      msgStoreTailPtr = msgStorePtr;
    }
  }
#if REG_DEBUG
  else{
    fprintf(stderr, "STEER: Get_next_stored_msg: no msg to retrieve\n");
  }
#endif

  if(sim){
    sim->Msg_store_tail = msgStoreTailPtr;
  }
  else{
    Msg_store_tail = msgStoreTailPtr;
  }
  return msg;
}

/*------------------------------------------------------------------*/
int Get_resource_property (struct soap *soapStruct,
                           const char  *epr,
			   const char  *username,
			   const char  *passwd,
			   const char  *name,
			   char       **pRP)
{
  char  *out;
#ifdef USE_REG_TIMING
  double time0, time1;
#endif

  *pRP = NULL;
  if(Create_WSRF_header(soapStruct, epr, username, passwd) 
     != REG_SUCCESS){
    return REG_FAILURE;
  }

#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time0);
#endif

  if(soap_call___wsrp__GetResourceProperty(soapStruct, epr, 
					 "", (char*)name,
					 &out) != SOAP_OK){
    soap_print_fault(soapStruct, stderr);
    return REG_FAILURE;
  }
#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time1);
#if REG_DEBUG
  fprintf(stderr, "STEER: TIMING: soap_call___wsrp__GetResourceProperty"
	  " took %f seconds\n", (float)(time1-time0));
#endif
#endif
#if REG_DEBUG_FULL
  fprintf(stderr, "STEER: Get_resource_property for %s returned >>%s<<\n", 
	  name, out);
#endif

  *pRP = out;
  return REG_SUCCESS;
}

/*------------------------------------------------------------------*/
int Get_resource_property_doc(struct soap *soapStruct,
			      const char  *epr,
			      const char  *username,
			      const char  *passwd,
			      char       **pDoc)
{
  char *out;
#ifdef USE_REG_TIMING
  double time0, time1;
#endif

 /* Free-up dynamically-allocated memory regularly because the RP
    doc can be big */
  soap_end(soapStruct);

  if(Create_WSRF_header(soapStruct, epr, username, passwd) 
     != REG_SUCCESS) return REG_FAILURE;

  *pDoc = NULL;

#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time0);
#endif

  if(soap_call_wsrp__GetResourcePropertyDocument(soapStruct, 
						 (char *)epr, 
						 "", NULL,
						 &out) != SOAP_OK){
    soap_print_fault(soapStruct, stderr);
    return REG_FAILURE;
  }

#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time1);
#if REG_DEBUG
  fprintf(stderr, "STEER: TIMING: soap_call_wsrp__GetResourcePropertyDocument "
	  "took %f seconds\n", (time1-time0));
#endif
#endif

  if( !(*pDoc = out) ){
    fprintf(stderr, "STEER: ERROR: Get_resource_property_doc: gSoap call "
	    "successful but have NULL pointer to document!\n");
    return REG_FAILURE;
  }
  else{
    return REG_SUCCESS;
  }
}

/*------------------------------------------------------------------*/
int Set_resource_property (struct soap *soapStruct,
                           const char  *epr,
			   const char  *username,
			   const char  *passwd,
			   char        *input)
{
  struct wsrp__SetResourcePropertiesResponse out;

  if(Create_WSRF_header(soapStruct, epr, username, 
			passwd) != REG_SUCCESS)return REG_FAILURE;

  if(soap_call_wsrp__SetResourceProperties(soapStruct, epr,
					   "", input, &out) != SOAP_OK){
    fprintf(stderr, "STEER: Set_resource_property: failed to set RP:\n");
    soap_print_fault(soapStruct, stderr);

    if(soapStruct->fault && soapStruct->fault->detail &&
       soapStruct->fault->detail->__any){
	fprintf(stderr, "STEER: Set_resource_property: Soap error detail"
		" any = %s\n", soapStruct->fault->detail->__any);
	if(strstr(soapStruct->fault->detail->__any, "deadlock")){
	  /* If we timed-out because of a deadlock situation (possible in
	     coupled models) then return appropriate code */
	  return REG_TIMED_OUT;
	}
      }

    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*------------------------------------------------------------------*/
int Send_detach_msg_wsrf (Sim_entry_type *sim){

#if REG_DEBUG
  fprintf(stderr, "STEER: Send_detach_msg_wsrf: calling Detach...\n");
#endif

  if(Create_WSRF_header(sim->SGS_info.soap, sim->SGS_info.address,
			sim->SGS_info.username, 
			sim->SGS_info.passwd) != REG_SUCCESS){
    return REG_FAILURE;
  }

  if(soap_call_sws__Detach(sim->SGS_info.soap, sim->SGS_info.address, 
			   "", NULL, NULL )){

    fprintf(stderr, "STEER: Send_detach_msg_wsrf: Detach failed:\n");
    soap_print_fault(sim->SGS_info.soap, stderr);

    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*------------------------------------------------------------------*/
int Finalize_connection_wsrf (Sim_entry_type *sim)
{
  /* Remove temporary data and deserialized data except
     class instances */
  soap_end(sim->SGS_info.soap);
  /* Reset: close master/slave sockets and remove callbacks */
  soap_done(sim->SGS_info.soap);

  free(sim->SGS_info.soap);
  sim->SGS_info.soap = NULL;

  return REG_SUCCESS;  
}

/*------------------------------------------------------------------*/
int Get_param_log_wsrf(Sim_entry_type *sim,
		       int             handle)
{
  struct sws__GetParamLogResponse response;
  int    index, lindex;
  char  *ptr1;
  int    dum_int;
  long   dum_long;
  float  dum_float;
#ifdef USE_REG_TIMING
  double time0, time1;
#endif
  response.LogValues = NULL;

  if( (index=Param_index_from_handle(&(sim->Params_table),
				     handle)) == -1){

      fprintf(stderr, "STEER: Get_param_log_wsrf: failed to match param handle\n");
      fprintf(stderr, "                           handle = %d\n", handle);

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
  if(sim->SGS_info.passwd[0]){
    if(Create_WSRF_header(sim->SGS_info.soap, 
			  sim->SGS_info.address,
			  sim->SGS_info.username, 
			  sim->SGS_info.passwd) != REG_SUCCESS){
      return REG_FAILURE;
    }
  }
  if(soap_call_sws__GetParamLog(sim->SGS_info.soap, sim->SGS_info.address, 
				"", (xsd__int)handle, &response )){
#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time1);
  fprintf(stderr, "STEER: TIMING: soap_call_sws__GetParamLog "
	  "took %f seconds\n", (time1-time0));
#endif

    fprintf(stderr, "STEER: Get_param_log_wsrf: failed for handle %d:\n",
	    handle);
    soap_print_fault(sim->SGS_info.soap, stderr);

    return REG_FAILURE;
  }

  if( !(response.LogValues) ){
    fprintf(stderr, "STEER: Get_param_log_wsrf: no log entries returned\n");
    return REG_FAILURE;
  }

#if REG_DEBUG_FULL
  fprintf(stderr, "STEER: Get_param_log_wsrf: got log from SWS >>%s<<\n",
	  (char*)response.LogValues);
#endif

  ptr1 = (char*)response.LogValues;
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

  case REG_LONG:

    while(1){

      /* Parse space-delimited list of parameter values */
      sscanf(ptr1, "%ld ", &dum_long);
      sim->Params_table.param[index].log[lindex++] = (double)dum_long;

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
    fprintf(stderr, "STEER: Get_param_log_wsrf: logging of char params not "
	    "implemented!\n");
#endif
    break;

  default:
    break;
  }

  return REG_SUCCESS;
}
