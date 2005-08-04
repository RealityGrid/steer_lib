/*----------------------------------------------------------------------------
  Library routines and associated data structures for use in a 
  steering application.  Provides a communication interface 
  intended to inter-operate with an interface constructed for an
  application using the routines in ReG_Steer_Appside.c.

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

/** @file ReG_Steer_Steerside.c
    @brief Source file for steering-client routines
  */

#include "ReG_Steer_Steerside.h"
#include "ReG_Steer_Steerside_internal.h"
#include "ReG_Steer_Proxy_utils.h"
#include "ReG_Steer_Steerside_Soap.h"
#include "ReG_Steer_Steerside_WSRF.h"
#include "ReG_Steer_Browser.h"
#include "Base64.h"

#ifndef REG_DEBUG
#define REG_DEBUG 0
#endif

#define REG_TOP_LEVEL_REGISTRY "http://yaffel.mvc.mcc.ac.uk:50000/Session/ServiceGroupRegistration/service?3893432997"

/*--------------------- Data structures -------------------*/

/** 
   Main table used to record all simulations currently
   being steered */
Sim_table_type Sim_table;

/**
   Structure holding details of the main (java) proxy
   that is always associated with the steerer (if not steering
   via files or SOAP over http) */
Proxy_table_type Proxy;

/**
   This function's prototype is in ReG_Steer_Appside.c 'cos it's
   a utility that may be used by an application developer too */
extern char** Alloc_string_array(int, int);

/*----- Routines to be used by the steering component ------*/

int Steerer_initialize()
{
  int   i;
  /* int   status; */
  char *pchar;
  
  /* Actually defined in ReG_Steer_Common.c because both steerer
     and steered have a variable of this name */
  extern char ReG_Steer_Schema_Locn[REG_MAX_STRING_LENGTH];

#if REG_DEBUG
  /* Print out version information */
  fprintf(stderr, "**** RealityGrid Computational Steering Library "
	  "v.%s ****\n\n", REG_STEER_LIB_VERSION);
#endif

  /* Set the location of the file containing the schema describing all 
     steering communication */

  pchar = getenv("REG_STEER_HOME");

  if(pchar){
    /* Check that path ends in '/' - if not then add one */

    i = strlen(pchar);
    if( pchar[i-1] != '/' ){

      sprintf(ReG_Steer_Schema_Locn, "%s/xml_schema/reg_steer_comm.xsd",
                                     pchar);
    }
    else{

      sprintf(ReG_Steer_Schema_Locn, "%sxml_schema/reg_steer_comm.xsd",
                                     pchar);
    }
  }
  else{

    /* Instead of failing, set the schema location to some nonsense value
       and continue */
    fprintf(stderr, "Steerer_initialize: WARNING: failed to get schema location\n");
    sprintf(ReG_Steer_Schema_Locn, "/tmp/xml_schema/reg_steer_comm.xsd");
  }

  /* Initialize table of connected simulations */

  Sim_table.sim = (Sim_entry_type *)malloc(REG_MAX_NUM_STEERED_SIM*
					   sizeof(Sim_entry_type));

  if(Sim_table.sim == NULL){
    
    fprintf(stderr, "Steerer_initialize: failed to allocate memory\n");
    return REG_FAILURE;
  }

  Sim_table.max_entries    = REG_MAX_NUM_STEERED_SIM;
  Sim_table.num_registered = 0;

  /* Handle used to identify whether a table entry is valid so
     must initialize them all */

  for(i=0; i<Sim_table.max_entries; i++){

    Sim_table.sim[i].handle    = REG_SIM_HANDLE_NOTSET;
    Sim_table.sim[i].msg       = NULL;
    Sim_table.sim[i].pipe_to_proxy   = REG_PIPE_UNSET;
    Sim_table.sim[i].pipe_from_proxy = REG_PIPE_UNSET;
    Sim_table.sim[i].Msg_store.msg   = NULL;
    Sim_table.sim[i].Msg_store.next  = NULL;
  }

  /* Create the main proxy - we use this one to query the 'grid' about
     what services are available */

  /* Don't bother for now...*/ 
  Proxy.available = REG_FALSE;    

  /*
  status = Create_proxy(&(Proxy.pipe_to_proxy), &(Proxy.pipe_from_proxy));

  if(status != REG_SUCCESS){

    fprintf(stderr, "Steerer_initialize: Create_proxy failed\n");
    Proxy.available = REG_FALSE;    
  }
  else{

    Proxy.available = REG_TRUE;
  }
  */

  return REG_SUCCESS;
}

/*----------------------------------------------------------*/

int Steerer_finalize()
{
  int i;

  for(i=0; i<Sim_table.max_entries; i++){

    Delete_sim_table_entry( &(Sim_table.sim[i].handle) );
  }

  if(Sim_table.sim != NULL){

    free(Sim_table.sim);
    Sim_table.sim = NULL;
  }

  Sim_table.num_registered = 0;
  Sim_table.max_entries    = 0;
 
  /* Finished with the proxy */

  if(Proxy.available == REG_TRUE){

    Destroy_proxy(Proxy.pipe_to_proxy);
    Proxy.pipe_to_proxy   = REG_PIPE_UNSET;
    Proxy.pipe_from_proxy = REG_PIPE_UNSET;
    Proxy.available       = REG_FALSE;
  }

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Get_sim_list(int   *nSims,
		 char **simName,
		 char **simGSH)
{
  char *ptr;
  int   count;
  int   nbytes;
  int   status;
  int   i;
  char  registry_address[256];
  struct registry_entry *entries;

  *nSims = 0;

  /* Routine to get list of available steerable applications.
     Assumes that simName and simGSH are arrays of 
     REG_MAX_NUM_STEERED_SIM pointers to char arrays of length
     REG_MAX_STRING_LENGTH. */

  if(Proxy.available != REG_TRUE){

    /* Get address of top-level registry from env. variable if set */
    if( (ptr = getenv("REG_REGISTRY_ADDRESS")) ){
      strcpy(registry_address, ptr);
    } else{
      sprintf(registry_address, REG_TOP_LEVEL_REGISTRY);
    }

    /* Contact a registry here
       and ask it for the location of any SGSs it knows of */
    status = Get_registry_entries(registry_address, 
				  nSims, &entries);

    if(status == REG_SUCCESS){

      /* Hard limit on no. of sims we can return details on */
      if(*nSims > REG_MAX_NUM_STEERED_SIM)*nSims = REG_MAX_NUM_STEERED_SIM;
      
      count = 0;
      for(i=0; i<*nSims; i++){

	/* ARPDBG should check type of service here
	   if(strstr(entries[i].serviceType, "Registry")){

	   } */

	if(strlen(entries[i].application) > 0){
	  /*strcpy(simName[count], entries[i].application);*/
	  sprintf(simName[count], "%s %s %s", entries[i].user, 
		  entries[i].application, 
		  entries[i].start_date_time);
	  strcpy(simGSH[count], entries[i].gsh);
	  count++;
	}
      }
      *nSims = count;

      free(entries);
      entries = NULL;
    }
    else {
      if( (ptr = getenv("REG_SGS_ADDRESS")) ){

	*nSims = 1;
	sprintf(simName[0], "Simulation");
	strcpy(simGSH[0], ptr);
      }
      else{

	fprintf(stderr, "Get_sim_list: REG_SGS_ADDRESS environment variable "
		"is not set\n");
	*nSims = 0;
	sprintf(simName[0], " ");
      }
    }
    return REG_SUCCESS;
  }

  /* Get (space-delimited) list of steerable apps & associated
     grid-service handles */

  Send_proxy_message(Proxy.pipe_to_proxy, GET_APPS_MSG);

  Get_proxy_message(Proxy.pipe_from_proxy, Proxy.buf, &nbytes);

  if(nbytes == 0){
#if REG_DEBUG
    fprintf(stderr, "Get_sim_list: no steerable apps available\n");
#endif
    return REG_SUCCESS;
  }

  if(Proxy.buf[0] == ' '){

    ptr = (char *)strtok(&(Proxy.buf[1]), " ");
  }
  else{

    ptr = (char *)strtok(Proxy.buf, " ");
  }

  count = 0;

  while(ptr){

    strcpy(simName[count], ptr);
    ptr = (char *)strtok(NULL, " ");

    if(ptr){
      strcpy(simGSH[count], ptr);
      ptr = (char *)strtok(NULL, " ");

      count++;

      if(count == REG_MAX_NUM_STEERED_SIM){

	fprintf(stderr, "Get_sim_list: truncating list of steerable apps\n");
	break;
      }
    }
  }

  *nSims = count;

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Sim_attach(char *SimID,
	       int  *SimHandle)
{
  int                  current_sim;
  int                  i, j;
  int                  return_status = REG_SUCCESS;
  Sim_entry_type      *sim_ptr;

  /* Get next free entry in simulation table (allocates more memory if
     required) */

  if( (current_sim = Next_free_sim_index()) == -1){

    fprintf(stderr, "Sim_attach: failed to find free sim. table entry\n");
    return REG_FAILURE;
  }
  sim_ptr = &(Sim_table.sim[current_sim]);

  /* Initialise table entry for this simulation... */

  sim_ptr->pipe_to_proxy   = REG_PIPE_UNSET;
  sim_ptr->pipe_from_proxy = REG_PIPE_UNSET;
  sim_ptr->msg             = NULL;
  sim_ptr->detached        = REG_TRUE;

  /* Initialise the table for keeping track of msg uid's that we've
     seen before */
  for(i=0; i<REG_UID_HISTORY_BUFFER_SIZE; i++){
    sim_ptr->Msg_uid_store.uidStore[i] = 2*REG_UID_HISTORY_BUFFER_SIZE;
  }
  sim_ptr->Msg_uid_store.uidStorePtr = sim_ptr->Msg_uid_store.uidStore;
  sim_ptr->Msg_uid_store.maxPtr = 
    &(sim_ptr->Msg_uid_store.uidStore[REG_UID_HISTORY_BUFFER_SIZE - 1]);

  /* ...registered parameters */

  sim_ptr->Params_table.param = 
            (param_entry *)malloc(REG_INITIAL_NUM_PARAMS*sizeof(param_entry));

  if(sim_ptr->Params_table.param == NULL){

    fprintf(stderr, "Sim_attach: failed to allocate memory\n");
    return REG_MEM_FAIL;
  }

  /* Initialise this table entry */

  sim_ptr->Params_table.num_registered = 0;
  sim_ptr->Params_table.max_entries    = REG_INITIAL_NUM_PARAMS;

  for(i=0; i<Sim_table.sim[current_sim].Params_table.max_entries; i++){

    Init_param_entry(&(sim_ptr->Params_table.param[i]));
  }

  /* ...supported commands */

  sim_ptr->Cmds_table.cmd = 
    (supp_cmd_entry *)malloc(REG_INITIAL_NUM_CMDS*sizeof(supp_cmd_entry));

  if(sim_ptr->Cmds_table.cmd == NULL){

    fprintf(stderr, "Sim_attach: failed to allocate memory\n");
    free(sim_ptr->Params_table.param);
    sim_ptr->Params_table.param = NULL;
    return REG_MEM_FAIL;
  }

  sim_ptr->Cmds_table.num_registered = 0;
  sim_ptr->Cmds_table.max_entries    = REG_INITIAL_NUM_CMDS;

  /* all simulations must support the 'detach' command */
  sim_ptr->Cmds_table.cmd[0].cmd_id = REG_STR_DETACH;
  Increment_cmd_registered(&(sim_ptr->Cmds_table));
  /* The detach cmd is now automatically generated on the app side so
     that clients binding to the SGS need not worry about it. However,
     we keep the above to maintain backwards-compatibility */

  /* ...IO types */

  sim_ptr->IOdef_table.io_def = 
    (IOdef_entry *)malloc(REG_INITIAL_NUM_IOTYPES*sizeof(IOdef_entry));

  if(sim_ptr->IOdef_table.io_def == NULL){

    fprintf(stderr, "Sim_attach: failed to allocate memory\n");
    free(sim_ptr->Params_table.param);
    sim_ptr->Params_table.param = NULL;
    free(sim_ptr->Cmds_table.cmd);
    sim_ptr->Cmds_table.cmd = NULL;
    return REG_MEM_FAIL;
  }

  sim_ptr->IOdef_table.num_registered = 0;
  sim_ptr->IOdef_table.max_entries    = REG_INITIAL_NUM_IOTYPES;

  for(i=0; i<sim_ptr->IOdef_table.max_entries; i++){

    sim_ptr->IOdef_table.io_def[i].handle = REG_IODEF_HANDLE_NOTSET;
  }

  /* ...Chk types */

  sim_ptr->Chkdef_table.io_def = 
    (IOdef_entry *)malloc(REG_INITIAL_NUM_IOTYPES*sizeof(IOdef_entry));

  if(sim_ptr->Chkdef_table.io_def == NULL){

    fprintf(stderr, "sim_attach: failed to allocate memory for Chk types\n");
    free(sim_ptr->Params_table.param);
    sim_ptr->Params_table.param = NULL;
    free(sim_ptr->Cmds_table.cmd);
    sim_ptr->Cmds_table.cmd = NULL;
    free(sim_ptr->IOdef_table.io_def);
    sim_ptr->IOdef_table.io_def = NULL;
    return REG_MEM_FAIL;
  }

  sim_ptr->Chkdef_table.num_registered = 0;
  sim_ptr->Chkdef_table.max_entries    = REG_INITIAL_NUM_IOTYPES;

  for(i=0; i<sim_ptr->Chkdef_table.max_entries; i++){

    sim_ptr->Chkdef_table.io_def[i].handle = REG_IODEF_HANDLE_NOTSET;
  }

  /* Logging table */

  sim_ptr->Chk_log.num_entries = 0;
  sim_ptr->Chk_log.max_entries = REG_INITIAL_CHK_LOG_SIZE;

  sim_ptr->Chk_log.entry = (Chk_log_entry_type *)malloc(sim_ptr->Chk_log.max_entries
					  *sizeof(Chk_log_entry_type));
  if(sim_ptr->Chk_log.entry == NULL){

    fprintf(stderr, "sim_attach: failed to allocate memory for log table\n");
    free(sim_ptr->Params_table.param);
    sim_ptr->Params_table.param = NULL;
    free(sim_ptr->Cmds_table.cmd);
    sim_ptr->Cmds_table.cmd = NULL;
    free(sim_ptr->IOdef_table.io_def);
    sim_ptr->IOdef_table.io_def = NULL;
    free(sim_ptr->Chkdef_table.io_def);
    sim_ptr->Chkdef_table.io_def = NULL;
    return REG_MEM_FAIL;
  }

  for(i=0; i<sim_ptr->Chk_log.max_entries; i++){

    for(j=0; j<REG_MAX_NUM_STR_PARAMS; j++){
      sim_ptr->Chk_log.entry[i].param[j].handle = REG_PARAM_HANDLE_NOTSET;
    }
  }

  /* Initialise Steering Grid Service-related data */
  sim_ptr->SGS_info.active = REG_FALSE;
  sim_ptr->SGS_info.sde_count = 0;
  sim_ptr->SGS_info.sde_index = 0;

  /* Now we actually connect to the application */

  if(Proxy.available){

    /* Use a proxy to interact with the 'grid' */
#if REG_DEBUG
    fprintf(stderr, "Sim_attach: calling Sim_attach_proxy...\n");
#endif
    return_status = Sim_attach_proxy(&(Sim_table.sim[current_sim]), SimID);
  }
  else{

    return_status = REG_FAILURE;

    if(strstr(SimID, "http")){
      /* Try to use SOAP (and Steering Grid Service) - if this fails
	 then SimID isn't a valid GSH so we revert to file-based steering */
#if REG_DEBUG
      fprintf(stderr, "Sim_attach: calling Sim_attach_soap, "
	      "current_sim = %d\n", current_sim);
#endif

#if REG_OGSI
      return_status = Sim_attach_soap(&(Sim_table.sim[current_sim]), SimID);
#else
      return_status = Sim_attach_wsrf(&(Sim_table.sim[current_sim]), SimID);
#endif
      if(return_status == REG_SUCCESS){
	Sim_table.sim[current_sim].SGS_info.active = REG_TRUE;
      }
      else{
	Sim_table.sim[current_sim].SGS_info.active = REG_FALSE;
      }
    }

    if(return_status == REG_FAILURE){

      /* Use local file system */
#if REG_DEBUG
      fprintf(stderr, "Sim_attach: calling Sim_attach_local...\n");
#endif
      return_status = Sim_attach_local(&(Sim_table.sim[current_sim]), SimID);
    }
  }

  if(return_status == REG_SUCCESS){

    /* Generate handle that is returned */

    sim_ptr->handle = current_sim;
    *SimHandle = current_sim;
    Sim_table.num_registered++;

    /* Flag that we are now attached */
    sim_ptr->detached = REG_FALSE;

    /* If simulation supports the pause command then it must also
       support the resume command so add this to the list.
       Do this here in case application is using older version
       of steering library which doesn't automatically add the
       corresponding resume command to the list */

    for(i=0; i<sim_ptr->Cmds_table.num_registered; i++){

      if(sim_ptr->Cmds_table.cmd[i].cmd_id == REG_STR_PAUSE){
	
	j = sim_ptr->Cmds_table.num_registered;
	sim_ptr->Cmds_table.cmd[j].cmd_id = REG_STR_RESUME;
	Increment_cmd_registered(&(sim_ptr->Cmds_table));
	break;
      }
    }
  }
  else{

    free(sim_ptr->Params_table.param);
    sim_ptr->Params_table.param = NULL;
    free(sim_ptr->Cmds_table.cmd);
    sim_ptr->Cmds_table.cmd = NULL;
    free(sim_ptr->IOdef_table.io_def);
    sim_ptr->IOdef_table.io_def = NULL;
    free(sim_ptr->Chkdef_table.io_def);
    sim_ptr->Chkdef_table.io_def = NULL;
    free(sim_ptr->Chk_log.entry);
    sim_ptr->Chk_log.entry = NULL;
  }

  return return_status;
}

/*----------------------------------------------------------*/

int Sim_detach(int *SimHandle)
{
  /* Check that handle is valid */

  if(*SimHandle == REG_SIM_HANDLE_NOTSET) return REG_SUCCESS;

  /* Signal simulation that we are finished steering */
  Emit_detach_cmd(*SimHandle);

  /* Delete associated table entry */
  Delete_sim_table_entry(SimHandle);

  return REG_SUCCESS;
}

/*----------------------------------------------------------*/

int Get_next_message(int   *SimHandle,
		     int   *msg_type)
{
  int        isim;
  int        count_active;
  int        loop_count;
  int        got_msg;
  static int last_sim = -1;
  int        return_status = REG_SUCCESS;

  /* This routine checks for any messages from connected
     simulations */

  *msg_type  = MSG_NOTSET;

  count_active = 0;
  got_msg = REG_FALSE;
  /* Search for next message from the sim. stored immediately
     following the one we got a message from last time */
  isim = ++last_sim;
  loop_count = 0;

  while(loop_count<Sim_table.max_entries){
    
    /* Wrap simulation index around */
    if(isim >= Sim_table.max_entries)isim = 0;

    if(Sim_table.sim[isim].handle != REG_SIM_HANDLE_NOTSET){
  
      /* Just to make sure we don't leak - wipe any existing msg */
      if(Sim_table.sim[isim].msg){
	Delete_msg_struct(&(Sim_table.sim[isim].msg));
      }

      if(Sim_table.sim[isim].pipe_to_proxy != REG_PIPE_UNSET){

	/* Have a proxy so communicate with sim. using it */
	Sim_table.sim[isim].msg = Get_status_msg_proxy(&(Sim_table.sim[isim]));
      }
      else{

        if(Sim_table.sim[isim].SGS_info.active){

#if REG_OGSI
  	  Sim_table.sim[isim].msg = Get_status_msg_soap(&(Sim_table.sim[isim]));
#else
  	  Sim_table.sim[isim].msg = Get_status_msg_wsrf(&(Sim_table.sim[isim]));
#endif
	}
	else{

	  /* No proxy and no SGS available so using 'local' file system */
	  Sim_table.sim[isim].msg = Get_status_msg_file(&(Sim_table.sim[isim]));
	}
      }

      /* If we got a message & parsed it successfully then we're done */

      if(Sim_table.sim[isim].msg){

	/* Pass back the message type  */
	*msg_type = Sim_table.sim[isim].msg->msg_type;

	*SimHandle = Sim_table.sim[isim].handle;

	/* Keep a record of the last sim we received a msg
	   from and then breakout */
	last_sim = isim;
	got_msg = REG_TRUE;

	break;
      }
      else{
        Delete_msg_struct(&(Sim_table.sim[isim].msg));
      }

      /* Count no. of active sim.'s we've checked so that we can
         break out of loop once Sim_table.num_registered have
         been done - this will only happen when there are no
         messages to retrieve */
      if (++count_active == Sim_table.num_registered) break;
    }
    isim++;
    loop_count++;
  }

  /* Didn't find a message so reset last_sim */
  if(got_msg == REG_FALSE)last_sim=-1;

  return return_status;
}

/*--------------------------------------------------------------------*/

struct msg_struct *Get_status_msg_proxy(Sim_entry_type *sim)
{
  struct msg_struct *msg = NULL;
  char   buf[REG_MAX_MSG_SIZE];
  int    nbytes;

  Send_proxy_message(sim->pipe_to_proxy, GET_STATUS_MSG);

  /* Check the success of the request */
  Get_proxy_message(sim->pipe_from_proxy, buf, &nbytes);

  if(!strncmp(buf, OK_MSG, nbytes)){

    /* Get the message itself */
    Get_proxy_message(sim->pipe_from_proxy, buf, &nbytes);

    msg = New_msg_struct();

    if(Parse_xml_buf(buf, nbytes, msg, sim) != REG_SUCCESS){

      Delete_msg_struct(&msg);
    }
  }

  return msg;
}

/*--------------------------------------------------------------------*/

struct msg_struct *Get_status_msg_file(Sim_entry_type *sim)
{
  struct msg_struct *msg = NULL;
  char  filename[REG_MAX_STRING_LENGTH];
  FILE *fp;
  int   return_status;

  sprintf(filename, "%s%s", sim->file_root, APP_TO_STR_FILENAME);

  if( (fp = Open_next_file(filename)) ){
	
    fclose(fp);

    /* Parse it and store it in structure pointed to by msg */

    msg = New_msg_struct();

    return_status = Parse_xml_file(filename, msg, sim);

    if(return_status != REG_SUCCESS){

      Delete_msg_struct(&msg);
    }

    /* Consume the file now that we've read it */
    Delete_file(filename);
  }

  return msg;
}

/*------------------------------------------------------------------------*/

int Consume_param_defs(int SimHandle)
{
  int                  index;
  /*
  int                  i;
  int                  found;
  */
  int                  j;
  struct param_struct *ptr = NULL;
  int                  handle;
  int                  return_status = REG_SUCCESS;

  /* Read a message containing parameter definitions.  Table of
     stored definitions is then updated to match those just read */

  if( (index = Sim_index_from_handle(SimHandle)) == REG_SIM_HANDLE_NOTSET){

    fprintf(stderr, "Consume_param_defs: failed to find sim table entry\n");
    return REG_FAILURE;
  }

  /* Must now check the param definitions we've received and update the
     parameters part of the Sim_table appropriately */

  if(Sim_table.sim[index].msg->status){
    ptr = Sim_table.sim[index].msg->status->first_param;
  }
  else{
    fprintf(stderr, "Consume_param_defs: ERROR: msg is not of "
	    "param_defs type\n");
    return REG_FAILURE;
  }

  while(ptr){

    sscanf((char *)(ptr->handle), "%d", &handle);

    if( Param_index_from_handle(&(Sim_table.sim[index].Params_table),
				handle) == -1){

      /* Our table doesn't have this parameter in it so add it */

      j = Next_free_param_index(&(Sim_table.sim[index].Params_table));

      if(j == -1){
	fprintf(stderr, "Consume_param_defs: failed to add param to table\n");
	return_status = REG_FAILURE;
	break;
      }

      Sim_table.sim[index].Params_table.param[j].handle = handle;

      if(ptr->label){
	strcpy(Sim_table.sim[index].Params_table.param[j].label,
	       (char *)(ptr->label));
      }

      if(ptr->steerable){
	sscanf((char *)(ptr->steerable), "%d", 
	       &(Sim_table.sim[index].Params_table.param[j].steerable));
      }

      if(ptr->type){
	sscanf((char *)(ptr->type), "%d",
	       &(Sim_table.sim[index].Params_table.param[j].type));
      }

      Sim_table.sim[index].Params_table.param[j].ptr = NULL;

      if(ptr->is_internal){
        if(!xmlStrcmp(ptr->is_internal, (const xmlChar *) "TRUE")){

	  Sim_table.sim[index].Params_table.param[j].is_internal = REG_TRUE;
	}
	else{
	  Sim_table.sim[index].Params_table.param[j].is_internal = REG_FALSE;
	}
      }

      if(ptr->min_val){
	strcpy(Sim_table.sim[index].Params_table.param[j].min_val, 
	       (char *)ptr->min_val);
	Sim_table.sim[index].Params_table.param[j].min_val_valid = REG_TRUE;
      }
      else{
	/* Absence of a bound in the xml message indicates no bound
	   is set */
	sprintf(Sim_table.sim[index].Params_table.param[j].min_val, "--");
	Sim_table.sim[index].Params_table.param[j].min_val_valid = REG_FALSE;
      }

      if(ptr->max_val){
	strcpy(Sim_table.sim[index].Params_table.param[j].max_val, 
	       (char *)ptr->max_val);
	Sim_table.sim[index].Params_table.param[j].max_val_valid = REG_TRUE;
      }
      else{
	/* Absence of a bound in the xml message indicates no bound
	   is set */
	sprintf(Sim_table.sim[index].Params_table.param[j].max_val, "--");
	Sim_table.sim[index].Params_table.param[j].max_val_valid = REG_FALSE;
      }

      /* Special memory buffer for 'raw data' type of monitored parameter */
      if(Sim_table.sim[index].Params_table.param[j].type == REG_BIN){
	 if(Sim_table.sim[index].Params_table.param[j].max_val_valid == REG_TRUE){

	   Sim_table.sim[index].Params_table.param[j].ptr_raw = 
	     malloc(atoi(Sim_table.sim[index].Params_table.param[j].max_val));
	   Sim_table.sim[index].Params_table.param[j].raw_buf_size = 
	     atoi(Sim_table.sim[index].Params_table.param[j].max_val);

	   /* Won't have a traditional value field so blank it */
	   sprintf(Sim_table.sim[index].Params_table.param[j].value, " ");
	 }
      }
      else if(ptr->value){
	strcpy(Sim_table.sim[index].Params_table.param[j].value,
		(char *)(ptr->value));
      }

      Sim_table.sim[index].Params_table.num_registered++;
    }
    ptr = ptr->next;
  }

  /* This section removed because prevents packaging of param_defs
     in multiple messages.  Currently (30/6/2003) unused
     anyway because Unregister_param not implemented. */
  /* Remove any parameters that have been deleted *

  for(i=0; i<Sim_table.sim[index].Params_table.max_entries; i++){

    ptr = Sim_table.sim[index].msg->status->first_param;

    while(ptr){

      sscanf((char *)(ptr->handle), "%d", &handle);

      if(handle == Sim_table.sim[index].Params_table.param[i].handle){

	found = REG_TRUE;
	break;
      }
      ptr = ptr->next;
    }

    if(!found){

      * Only indication that param deleted is change of handle - hence loop
	 over max_entries here *

      Sim_table.sim[index].Params_table.param[i].handle = 
	                                       REG_PARAM_HANDLE_NOTSET;
      Sim_table.sim[index].Params_table.num_registered--;
    }
  }
  */

  /* Clean up */

  Delete_msg_struct(&(Sim_table.sim[index].msg));

  return return_status;
}

/*----------------------------------------------------------*/

int Consume_IOType_defs(int SimHandle)
{
  int               index;
  int               i, j;
  int               return_status = REG_SUCCESS;
  int               handle;
  int               found;
  struct io_struct *ptr = NULL;

  if( (index = Sim_index_from_handle(SimHandle)) == REG_SIM_HANDLE_NOTSET){

    fprintf(stderr, "Consume_IOType_defs: failed to find sim table entry\n");
    return REG_FAILURE;
  }

  /* Compare new IOdefs with those currently stored in table */

  if(Sim_table.sim[index].msg->io_def){
    ptr = Sim_table.sim[index].msg->io_def->first_io;
  }
  else{
    fprintf(stderr, "Consume_IOType_defs: ERROR: msg is not of "
	    "IOType_defs type\n");
    return REG_FAILURE;
  }

  while(ptr){

    sscanf((char *)(ptr->handle), "%d", &handle);

    if( IOdef_index_from_handle(&(Sim_table.sim[index].IOdef_table), 
				handle) == REG_IODEF_HANDLE_NOTSET){

      /* Our table doesn't have this IOdef so add it */
      j = Next_free_iodef_index(&(Sim_table.sim[index].IOdef_table));

      if(j==-1){
	fprintf(stderr, "Consume_IOdefs: failed to add IOdef to table\n");
	return_status = REG_FAILURE;
	break;
      }

      Sim_table.sim[index].IOdef_table.io_def[j].handle = handle;

      if(ptr->label){

	strcpy(Sim_table.sim[index].IOdef_table.io_def[j].label,
	       (char *)(ptr->label));
      }

      if(ptr->direction){

        if(!xmlStrcmp(ptr->direction, (const xmlChar *) "IN")){

	  Sim_table.sim[index].IOdef_table.io_def[j].direction = REG_IO_IN;
	}
	else if(!xmlStrcmp(ptr->direction, (const xmlChar *) "OUT")){

	  Sim_table.sim[index].IOdef_table.io_def[j].direction = REG_IO_OUT;
	}
	else{
	  fprintf(stderr, "Consume_IOType_defs: ERROR: unrecognised "
		  "direction value\n");
	}
      }

      if(ptr->freq_handle){

	sscanf((char *)(ptr->freq_handle), "%d",
	       &(Sim_table.sim[index].IOdef_table.io_def[j].freq_param_handle));
      }

      Sim_table.sim[index].IOdef_table.num_registered++;
    }

    ptr = ptr->next;
  }

  /* Remove any IO defs that have been deleted */

  for(i=0; i<Sim_table.sim[index].IOdef_table.max_entries; i++){

    found = REG_FALSE;
    ptr = Sim_table.sim[index].msg->io_def->first_io;

    while(ptr){

      sscanf((char *)(ptr->handle), "%d", &handle);

      if(handle == Sim_table.sim[index].IOdef_table.io_def[i].handle){

	found = REG_TRUE;
	break;
      }
      ptr = ptr->next;
    }

    if(!found){

      /* Only indication that IOdef deleted is change of handle - hence loop
	 over max_entries here */

      Sim_table.sim[index].IOdef_table.io_def[i].handle = 
	                                 REG_IODEF_HANDLE_NOTSET;
      Sim_table.sim[index].IOdef_table.num_registered--;
    }
  }

  /* Clean up */

  Delete_msg_struct(&(Sim_table.sim[index].msg));

  return return_status;
}

/*----------------------------------------------------------*/

int Consume_ChkType_defs(int SimHandle)
{
  int               index;
  int               i, j;
  int               return_status = REG_SUCCESS;
  int               handle;
  int               found;
  struct io_struct *ptr = NULL;

  if( (index = Sim_index_from_handle(SimHandle)) == REG_SIM_HANDLE_NOTSET){

    fprintf(stderr, "Consume_ChkType_defs: failed to find sim table entry\n");
    return REG_FAILURE;
  }

  if(Sim_table.sim[index].msg->chk_def){
    ptr = Sim_table.sim[index].msg->chk_def;
  }
  else{
    fprintf(stderr, "Consume_ChkType_defs: ERROR: msg is not of "
	    "ChkType_defs type\n");
    return REG_FAILURE;
  }

  /* Compare new Chkdefs with those currently stored in table */

  ptr = Sim_table.sim[index].msg->chk_def->first_io;

  while(ptr){

    sscanf((char *)(ptr->handle), "%d", &handle);

    if( IOdef_index_from_handle(&(Sim_table.sim[index].Chkdef_table), 
				handle) == REG_IODEF_HANDLE_NOTSET){

      /* Our table doesn't have this Chkdef so add it */
      j = Next_free_iodef_index(&(Sim_table.sim[index].Chkdef_table));

      if(j==-1){
	fprintf(stderr, "Consume_Chkdefs: failed to add Chkdef to table\n");
	return_status = REG_FAILURE;
	break;
      }

      Sim_table.sim[index].Chkdef_table.io_def[j].handle = handle;

      if(ptr->label){

	strcpy(Sim_table.sim[index].Chkdef_table.io_def[j].label,
	       (char *)(ptr->label));
      }

      if(ptr->direction){

        if(!xmlStrcmp(ptr->direction, (const xmlChar *) "INOUT")){

	  Sim_table.sim[index].Chkdef_table.io_def[j].direction = REG_IO_INOUT;
	}
	else if(!xmlStrcmp(ptr->direction, (const xmlChar *) "OUT")){

	  Sim_table.sim[index].Chkdef_table.io_def[j].direction = REG_IO_OUT;
	}
	else if(!xmlStrcmp(ptr->direction, (const xmlChar *) "IN")){

	  Sim_table.sim[index].Chkdef_table.io_def[j].direction = REG_IO_IN;
	}
	else{
	  fprintf(stderr, "Consume_ChkType_defs: ERROR: unrecognised "
		  "direction value: %s\n", (char *)(ptr->direction));
	}
      }

      if(ptr->freq_handle){

	sscanf((char *)(ptr->freq_handle), "%d",
	       &(Sim_table.sim[index].Chkdef_table.io_def[j].freq_param_handle));
      }

      Sim_table.sim[index].Chkdef_table.num_registered++;
    }

    ptr = ptr->next;
  }

  /* Remove any Chk defs that have been deleted */

  for(i=0; i<Sim_table.sim[index].Chkdef_table.max_entries; i++){

    found = REG_FALSE;
    ptr = Sim_table.sim[index].msg->chk_def->first_io;

    while(ptr){

      sscanf((char *)(ptr->handle), "%d", &handle);

      if(handle == Sim_table.sim[index].Chkdef_table.io_def[i].handle){

	found = REG_TRUE;
	break;
      }
      ptr = ptr->next;
    }

    if(!found){

      /* Only indication that Chkdef deleted is change of handle - hence loop
	 over max_entries here */

      Sim_table.sim[index].Chkdef_table.io_def[i].handle = 
	                                 REG_IODEF_HANDLE_NOTSET;
      Sim_table.sim[index].Chkdef_table.num_registered--;
    }
  }

  /* Clean up */

  Delete_msg_struct(&(Sim_table.sim[index].msg));

  return return_status;
}

/*----------------------------------------------------------*/

int Consume_log(int SimHandle)
{
  int index;
  Sim_entry_type          *sim;
  struct log_entry_struct *entry_ptr = NULL;

  int return_status = REG_SUCCESS;

  if( (index = Sim_index_from_handle(SimHandle)) == 
                                          REG_SIM_HANDLE_NOTSET){

    fprintf(stderr, "Consume_log: failed to find sim table entry\n");
    return REG_FAILURE;
  }

  sim = &(Sim_table.sim[index]);

  /* Get_next_message has already parsed the (xml) message and stored
     it in the structure pointed to by sim->msg */

  if(!sim->msg || !sim->msg->log){
    fprintf(stderr, "Consume_log: ERROR: message is not of "
	    "log type\n");
    return REG_FAILURE;
  }
  if(!(entry_ptr = sim->msg->log->first_entry)){

    return REG_FAILURE;
  }

  while(entry_ptr){

    if(entry_ptr->key){

      sscanf((char *)entry_ptr->key, "%d", 
	     &(sim->Chk_log.entry[index].key));
    }

    if(entry_ptr->first_param_log){
      Consume_param_log(sim, entry_ptr->first_param_log);
    }
    else if(entry_ptr->first_chk_log){
      Consume_chk_log(sim, entry_ptr->first_chk_log);
    }

    entry_ptr = entry_ptr->next;
  }

  /* Clean up */

  Delete_msg_struct(&(sim->msg));

  return return_status;
}

/*----------------------------------------------------------*/

int Consume_chk_log(Sim_entry_type *sim, 
		    struct chk_log_entry_struct *entry)
{
  int                  index, count;
  int                  return_status = REG_SUCCESS;
  struct param_struct *param_ptr = NULL;

  index = sim->Chk_log.num_entries;

  while(entry){

  if(entry->chk_handle){

    sscanf((char *)entry->chk_handle, "%d", 
	   &(sim->Chk_log.entry[index].chk_handle));
  }
  if(entry->chk_tag){

    strcpy(sim->Chk_log.entry[index].chk_tag, 
	   (char *)entry->chk_tag);
  }

  param_ptr = entry->first_param;
  count = 0;

  while(param_ptr){

    if(param_ptr->handle){
      sscanf((char *)(param_ptr->handle), "%d", 
	     &(sim->Chk_log.entry[index].param[count].handle));
    }
    if(param_ptr->value){
      strcpy(sim->Chk_log.entry[index].param[count].value,
	     (char *)(param_ptr->value));
    }
    param_ptr = param_ptr->next;

    if(++count >= REG_MAX_NUM_STR_PARAMS){

      fprintf(stderr, "Consume_chk_log: WARNING: discarding parameter "
	      "because max. no. of %d exceeded\n", 
	      REG_MAX_NUM_STR_PARAMS);
      break;
    }
  }

    entry = entry->next;
  }

  if(Increment_log_entry(&(sim->Chk_log)) != REG_SUCCESS){

    return_status = REG_FAILURE;
  }

  return return_status;
}

/*----------------------------------------------------------*/

int Consume_param_log(Sim_entry_type *sim, 
		      struct param_struct *param_ptr)
{
  int handle;
  int index;
  int i = -1;
  int   dum_int;
  float dum_float;
  
  while(param_ptr){

    if(!param_ptr->handle){
      printf("Consume_param_log: ptr to param handle is NULL!\n");
      return REG_FAILURE;
    }
    /* Look-up entry for param to update */

    sscanf((char *)(param_ptr->handle), "%d", &handle);

    if( (i=Param_index_from_handle(&(sim->Params_table),
				   handle)) == -1){

      fprintf(stderr, "Consume_param_log: failed to match param handles\n");
      fprintf(stderr, "                   handle = %d\n", handle);

      param_ptr = param_ptr->next;
      continue;
    }

    if(!sim->Params_table.param[i].log){

      if(Realloc_param_log(&(sim->Params_table.param[i])) !=
	 REG_SUCCESS){

	return REG_FAILURE;
      }
    }
    index = sim->Params_table.param[i].log_index;

    /* Update value of this param */
    if(param_ptr->value){
      switch(sim->Params_table.param[i].type){

      case REG_INT:
	sscanf((char *)(param_ptr->value), "%d",
	       &dum_int);
	sim->Params_table.param[i].log[index++] = (double)dum_int;
        break;
      case REG_FLOAT:
	sscanf((char *)(param_ptr->value), "%f",
	       &dum_float);
	sim->Params_table.param[i].log[index++] = (double)dum_float;
	break;
      case REG_DBL:
	sscanf((char *)(param_ptr->value), "%lg",
	       &(sim->Params_table.param[i].log[index++]) );
	break;
      case REG_CHAR:
	/* This not implemented yet */
#if REG_DEBUG
	fprintf(stderr, "Consume_param_log: logging of char params not "
		"implemented!\n");
#endif
	break;
      default:
	break;
      }
    }
    else{
      switch(sim->Params_table.param[i].type){

      case REG_CHAR:
	/* This not implemented yet */
	fprintf(stderr, "Consume_param_log: logging of char params not "
		"implemented!\n");
	break;
      default:
	sim->Params_table.param[i].log[index++] = 0.0;
	break;
      }
    } /* end if(param_ptr->value) */

    if(index >= sim->Params_table.param[i].log_size){
      Realloc_param_log(&(sim->Params_table.param[i]));
    }

    sim->Params_table.param[i].log_index = index;

    param_ptr = param_ptr->next;
  }

  return REG_SUCCESS;
}
/*----------------------------------------------------------*/

int Consume_status(int   SimHandle,
		   int  *SeqNum,
		   int  *NumCmds,
		   int  *Commands)
{
  int                  j;
  int                  index;
  int                  handle;
  int                  count;
  int                  return_status;
  /* int                  k; For dbg output of Base64 */

  /* For XML parser */

  struct param_struct *param_ptr = NULL;
  struct   cmd_struct *cmd_ptr = NULL;

  return_status = REG_SUCCESS;
  *NumCmds = 0;

  /* Find the table entry for simulation */

  if( (index = Sim_index_from_handle(SimHandle)) == REG_SIM_HANDLE_NOTSET){

    fprintf(stderr, "Consume_status: failed to find sim table entry\n");
    return REG_FAILURE;
  }

  /* Get_next_message has already parsed the (xml) message and stored it
     in the structure pointed to by Sim_table.sim[index].msg */

  /* Copy data out of structures - commands first... */
  if(Sim_table.sim[index].msg->status){
    cmd_ptr = Sim_table.sim[index].msg->status->first_cmd;
  }
  else{
    fprintf(stderr, "Consume_status: ERROR: msg is not of status type\n");
    return REG_FAILURE;
  }

  count = 0;
  while(cmd_ptr){

    if(cmd_ptr->id){
      sscanf((char *)(cmd_ptr->id), "%d", &(Commands[count]));
    }
    else if(cmd_ptr->name){

	  if(!xmlStrcmp(cmd_ptr->name, (const xmlChar *)"STOP")){
	    Commands[count] = REG_STR_STOP;
	  }
	  else if(!xmlStrcmp(cmd_ptr->name, (const xmlChar *)"PAUSE")){
	    Commands[count] = REG_STR_PAUSE;
	  }
	  else if(!xmlStrcmp(cmd_ptr->name, (const xmlChar *)"DETACH")){
	    Commands[count] = REG_STR_DETACH;
	  }
	  else if(!xmlStrcmp(cmd_ptr->name, (const xmlChar *)"RESUME")){
	    Commands[count] = REG_STR_RESUME;
	  }
	  else{
	    fprintf(stderr, "Consume_status: unrecognised cmd name: %s\n", 
		    (char *)cmd_ptr->name);
	    cmd_ptr = cmd_ptr->next;
	  }
    }
    else{
      fprintf(stderr, "Consume_status: error - skipping cmd because is missing "
	      "both id and name\n");
      cmd_ptr = cmd_ptr->next;
    }

    count++;

    if(count >= REG_MAX_NUM_STR_CMDS){

      fprintf(stderr, "Consume_status: WARNING: truncating list of cmds\n");
      break;
    }
    cmd_ptr = cmd_ptr->next;
  }

  *NumCmds = count;

#if REG_DEBUG
  fprintf(stderr, "Consume_status: got %d commands\n", (*NumCmds));
#endif

  /* ...and now the parameters... */

  param_ptr = Sim_table.sim[index].msg->status->first_param;

  count = 0;
  while(param_ptr){

    sscanf((char *)(param_ptr->handle), "%d", &handle);

    /* Look-up entry for param to update */

    if( (j=Param_index_from_handle(&(Sim_table.sim[index].Params_table),
				   handle)) == -1){

      fprintf(stderr, "Consume_status: failed to match param handles\n");
      fprintf(stderr, "                handle = %d\n", handle);

      param_ptr = param_ptr->next;
      continue;
    }

    /* Update value of this param */
    if(param_ptr->value){

      if(Sim_table.sim[index].Params_table.param[j].type == REG_BIN){

	if(Base64_decode((char *)(param_ptr->value), 
			 strlen((char *)(param_ptr->value)), 
			 (char **)&(Sim_table.sim[index].Params_table.param[j].ptr_raw),
			 &Sim_table.sim[index].Params_table.param[j].raw_buf_size)
	   != REG_SUCCESS){
	  fprintf(stderr, "Consume_status: Base64 decode failed\n");
	  continue;
	}
	/*
	printf("ARPDBG Decoded buffer (len=%d)>>", len);
	for(k=0; k<len; k++){
	  printf("%d", pchar[k]);
	}
	printf("<<ARPDBG\n");
	*/

	/* Put blank in actual 'value' field */
	sprintf(Sim_table.sim[index].Params_table.param[j].value, " ");
      }
      else{
	strcpy(Sim_table.sim[index].Params_table.param[j].value,
	       (char *)(param_ptr->value));
      }
    }
    else{
      sprintf(Sim_table.sim[index].Params_table.param[j].value, " ");
    }

    count++;
    param_ptr = param_ptr->next;
  }

  /* Return sequence number */

  if( (j=Param_index_from_handle(&(Sim_table.sim[index].Params_table),
			    REG_SEQ_NUM_HANDLE)) == -1){
    fprintf(stderr, "Consume_status: failed to find SeqNum entry\n");
  }
  else{

    sscanf(Sim_table.sim[index].Params_table.param[j].value, "%d", SeqNum);
  }

  /* Clean up */

  Delete_msg_struct(&(Sim_table.sim[index].msg));

  return return_status;
}

/*----------------------------------------------------------*/

int Emit_detach_cmd(int SimHandle)
{
  int index;
  int SysCommands[1];
  int status;

  /* Check that handle is valid */
  if(SimHandle == REG_SIM_HANDLE_NOTSET) return REG_SUCCESS;

  if( (index = Sim_index_from_handle(SimHandle)) == -1){
    return REG_FAILURE;
  }

  /* Don't call detach again if we're already detached */
  if(Sim_table.sim[index].detached == REG_TRUE) return REG_SUCCESS;

  if(Sim_table.sim[index].SGS_info.active){
#if REG_OGSI
    status = Send_detach_msg_soap(&(Sim_table.sim[index]));
#else
    status = Send_detach_msg_wsrf(&(Sim_table.sim[index]));
#endif
  }
  else{
    SysCommands[0] = REG_STR_DETACH;

    status = Emit_control(SimHandle, 1, SysCommands, NULL);
  }

  if(status == REG_SUCCESS){
    /* Flag that we're now detached */
    Sim_table.sim[index].detached = REG_TRUE;
  }
  return status;
}

/*----------------------------------------------------------*/

int Emit_stop_cmd(int SimHandle)
{
  int index;
  int SysCommands[1];

  /* Check that handle is valid */
  if(SimHandle == REG_SIM_HANDLE_NOTSET) return REG_SUCCESS;

  if( (index = Sim_index_from_handle(SimHandle)) == -1){

    return REG_FAILURE;
  }

#if REG_OGSI
  if(Sim_table.sim[index].SGS_info.active){
    return Send_stop_msg_soap(&(Sim_table.sim[index]));
  }
#endif

  SysCommands[0] = REG_STR_STOP;

  return Emit_control(SimHandle,
		      1,
		      SysCommands,
		      NULL);
}

/*----------------------------------------------------------*/

int Emit_pause_cmd(int SimHandle)
{
  int index;
  int SysCommands[1];

  /* Check that handle is valid */
  if(SimHandle == REG_SIM_HANDLE_NOTSET) return REG_SUCCESS;

  if( (index = Sim_index_from_handle(SimHandle)) == -1){

    return REG_FAILURE;
  }

#if REG_OGSI
  if(Sim_table.sim[index].SGS_info.active){
    return Send_pause_msg_soap(&(Sim_table.sim[index]));
  }
#endif

  SysCommands[0] = REG_STR_PAUSE;

  return Emit_control(SimHandle, 1,
		      SysCommands, NULL);
}

/*----------------------------------------------------------*/

int Emit_resume_cmd(int SimHandle)
{
  int index;
  int SysCommands[1];

  /* Check that handle is valid */
  if(SimHandle == REG_SIM_HANDLE_NOTSET) return REG_SUCCESS;

  if( (index = Sim_index_from_handle(SimHandle)) == -1){

    return REG_FAILURE;
  }

#if REG_OGSI
  if(Sim_table.sim[index].SGS_info.active){
    return Send_resume_msg_soap(&(Sim_table.sim[index]));
  }
#endif

  SysCommands[0] = REG_STR_RESUME;

  return Emit_control(SimHandle, 1,
		      SysCommands, NULL);
}

/*----------------------------------------------------------*/

int Emit_retrieve_param_log_cmd(int SimHandle, int ParamHandle)
{
  int           index;
  int           SysCommands[1];
  static char **CommandParams = NULL;

  /* Check that handle is valid */
  if(SimHandle == REG_SIM_HANDLE_NOTSET) return REG_SUCCESS;

  if( (index = Sim_index_from_handle(SimHandle)) == -1){

    return REG_FAILURE;
  }

  if(Sim_table.sim[index].SGS_info.active){

#if REG_OGSI
    /* Get all of the log entries cached on the SGS */
    return Get_param_log_soap(&(Sim_table.sim[index]), ParamHandle);
#else
    return Get_param_log_wsrf(&(Sim_table.sim[index]), ParamHandle);
#endif
  }
  else{
    /* Alternatively, send command to app to emit all of them since
       we have no cache on an SGS */
    SysCommands[0] = REG_STR_EMIT_PARAM_LOG;

    /* Which parameter to get the log of */
    if(!CommandParams) CommandParams = Alloc_string_array(32, 1);
    sprintf(CommandParams[0], "%d", ParamHandle);

    return Emit_control(SimHandle,
			1,
			SysCommands,
			CommandParams);
  }
}

/*----------------------------------------------------------*/

int Emit_restart_cmd(int SimHandle, char *chkGSH)
{
  int index;

  /* Check that handle is valid */
  if(SimHandle == REG_SIM_HANDLE_NOTSET) return REG_SUCCESS;

  if( (index = Sim_index_from_handle(SimHandle)) == -1){

    return REG_FAILURE;
  }

  if(Sim_table.sim[index].SGS_info.active){
#if REG_OGSI
    return Send_restart_msg_soap(&(Sim_table.sim[index]), chkGSH);
#else
    return Send_restart_msg_wsrf(&(Sim_table.sim[index]), chkGSH);
#endif
  }
  else{
    return REG_FAILURE;
  }
}

/*----------------------------------------------------------*/

int Emit_control(int    SimHandle,
		 int    NumCommands,
		 int   *SysCommands,
		 char **SysCmdParams)
{
  int   i;
  int   simid;
  int   count;
  int   num_to_emit;
  char  param_buf[REG_MAX_STRING_LENGTH];
  char *param_ptr;
  char  buf[REG_MAX_MSG_SIZE];
  char *pbuf;

  /* Find the simulation referred to */

  if( (simid = Sim_index_from_handle(SimHandle)) == REG_SIM_HANDLE_NOTSET){

    fprintf(stderr, "Emit_control: failed to find sim table entry\n");
    return REG_FAILURE;
  }

  /* Create control message in buffer */

  pbuf = buf;
  Write_xml_header(&pbuf, REG_FALSE);

  pbuf += sprintf(pbuf, "<Steer_control>\n");

  /* Enforce a hard limit of no more than REG_MAX_NUM_STR_CMDS commands
     per message */

  num_to_emit = NumCommands;
  if(NumCommands >= REG_MAX_NUM_STR_CMDS){

    num_to_emit = REG_MAX_NUM_STR_CMDS;
    fprintf(stderr, "Emit_control: WARNING - no. of emitted commands is "
	    "limited to %d\n", REG_MAX_NUM_STR_CMDS);
  }

  for(i=0; i<num_to_emit; i++){

    /* Check that simulation supports each requested command */
    if(Command_supported(simid, SysCommands[i])==REG_SUCCESS){

      pbuf += sprintf(pbuf, "<Command>\n");
      pbuf += sprintf(pbuf, "  <Cmd_id>%d</Cmd_id>\n", SysCommands[i]);

      if(SysCmdParams){

	strcpy(param_buf, SysCmdParams[i]);

	param_ptr = strtok(param_buf, " ");
	while(param_ptr){

    	  pbuf += sprintf(pbuf, "  <Cmd_param>\n");
	  pbuf += sprintf(pbuf, "    <Value>%s</Value>\n", param_ptr);
	  pbuf += sprintf(pbuf, "  </Cmd_param>\n");

	  param_ptr = strtok(NULL, " ");
	}

      }
      pbuf += sprintf(pbuf, "</Command>\n");
    }
  }

  /* Seach for valid parameters that have 'modified' flag set */

  count = 0;

  for(i=0; i<Sim_table.sim[simid].Params_table.max_entries; i++){

    if( (Sim_table.sim[simid].Params_table.param[i].handle !=
	 REG_PARAM_HANDLE_NOTSET) &&
	Sim_table.sim[simid].Params_table.param[i].modified ){

      /* Enforce a hard limit on the number of parameters that may
	 be transmitted at any one time */

      if(count == REG_MAX_NUM_STR_PARAMS){

	fprintf(stderr, "Emit_control: WARNING - no. of emitted params is "
	        "limited to %d\n", REG_MAX_NUM_STR_PARAMS);
	break;
      }

      pbuf += sprintf(pbuf, "<Param>\n");
      pbuf += sprintf(pbuf, "<Handle>%d</Handle>\n", 
	      Sim_table.sim[simid].Params_table.param[i].handle);
      pbuf += sprintf(pbuf, "<Value>%s</Value>\n", 
	      Sim_table.sim[simid].Params_table.param[i].value);
      pbuf += sprintf(pbuf, "</Param>\n");

      /* Unset 'modified' flag */
      Sim_table.sim[simid].Params_table.param[i].modified = REG_FALSE;

      count++;
    }
  }

  /* No parameters or commands to send so we're done */
  if(count==0 && num_to_emit==0){

#if REG_DEBUG
    fprintf(stderr, "Emit_control: nothing to send\n");
#endif
    return REG_SUCCESS;
  }

  pbuf += sprintf(pbuf, "</Steer_control>\n");
  /* ARPDBG - have to use REG_MAX_MSG_SIZE here because this 
     routine doesn't count how many bytes it's written into
     the buffer */
  Write_xml_footer(&pbuf, REG_MAX_MSG_SIZE);

#if REG_DEBUG
  fprintf(stderr, "Emit_control: sending:\n>>%s<<\n", buf);
#endif

  return Send_control_msg(simid, buf);
}

/*--------------------------------------------------------------------*/

int Send_control_msg(int SimIndex, char* buf)
{
  Sim_entry_type *sim = &(Sim_table.sim[SimIndex]);

  if(sim->pipe_to_proxy != REG_PIPE_UNSET){

    return Send_control_msg_proxy(sim, buf);
  }
  else{

    if(sim->SGS_info.active){

#if REG_OGSI
      return Send_control_msg_soap(sim, buf);
#else
      return Send_control_msg_wsrf(sim, buf);
#endif
    }
    else{

      return Send_control_msg_file(SimIndex, buf);
    }
  }
}

/*--------------------------------------------------------------------*/

int Send_control_msg_proxy(Sim_entry_type *sim, char* buf)
{
  int   nbytes;

  /* Instruct proxy to send control message to application */

  Send_proxy_message(sim->pipe_to_proxy, SEND_CTRL_MSG);

  /* Send buffer to proxy for forwarding to application */

  Send_proxy_message(sim->pipe_to_proxy, buf);

  Get_proxy_message(sim->pipe_from_proxy, buf, &nbytes);

  if(!strncmp(buf, ERR_MSG, nbytes)) return REG_FAILURE;

  return REG_SUCCESS;
}

/*--------------------------------------------------------------------*/

int Send_control_msg_file(int SimIndex, char* buf)
{
  FILE *fp;
  char  filename[REG_MAX_STRING_LENGTH];

  /* Write to a 'local' file */

  if( Generate_control_filename(SimIndex, filename) != REG_SUCCESS){

    fprintf(stderr, "Send_control_msg_file: failed to create filename\n");
    return REG_FAILURE;
  }

  if( (fp = fopen(filename, "w")) == NULL){

    fprintf(stderr, "Send_control_msg_file: failed to open file\n");
    return REG_FAILURE;
  }

  fprintf(fp, "%s", buf);
  fclose(fp);

  /* The application only attempts to read files for which it can find an
     associated lock file */
  return Create_lock_file(filename);
}

/*--------------------------------------------------------------------*/

int Delete_sim_table_entry(int *SimHandle)
{
  int             index;
  int             i;
  Sim_entry_type *sim;

  if(*SimHandle == REG_SIM_HANDLE_NOTSET) return REG_SUCCESS;
  
  if( (index = Sim_index_from_handle(*SimHandle)) == -1){

    return REG_FAILURE;
  }
  sim = &(Sim_table.sim[index]);

  Finalize_connection(sim);

  /* Clean-up the provided entry in the table of connected
     simulations */

  sim->Cmds_table.num_registered = 0;
  sim->Cmds_table.max_entries = 0;
  if (sim->Cmds_table.cmd) free(sim->Cmds_table.cmd);
  sim->Cmds_table.cmd = NULL;

  Delete_param_table(&(sim->Params_table));

  sim->IOdef_table.num_registered = 0;
  sim->IOdef_table.max_entries = 0;
  if (sim->IOdef_table.io_def) free(sim->IOdef_table.io_def);
  sim->IOdef_table.io_def = NULL;

  sim->Chkdef_table.num_registered = 0;
  sim->Chkdef_table.max_entries = 0;
  if (sim->Chkdef_table.io_def) free(sim->Chkdef_table.io_def);
  sim->Chkdef_table.io_def = NULL;

  sim->Chk_log.num_entries = 0;
  sim->Chk_log.max_entries = 0;
  if (sim->Chk_log.entry) free(sim->Chk_log.entry);
  sim->Chk_log.entry = NULL;

  for(i=0; i<sim->Params_table.max_entries; i++){
    if(sim->Params_table.param[i].handle != REG_PARAM_HANDLE_NOTSET){
      if(sim->Params_table.param[i].type == REG_BIN){
	if(sim->Params_table.param[i].ptr_raw){
	  free(sim->Params_table.param[i].ptr_raw);
	  sim->Params_table.param[i].ptr_raw = NULL;
	}
      }
    }
  }

  /* Flag that this entry no longer contains valid data */
  sim->handle = REG_SIM_HANDLE_NOTSET;
  *SimHandle = REG_SIM_HANDLE_NOTSET;

  return REG_SUCCESS;
}

/*----------------------------------------------------------*/

int Delete_param_table(Param_table_type *param_table)
{
  int i;

  for(i=0; i<param_table->max_entries; i++){
    if(param_table->param[i].handle != REG_PARAM_HANDLE_NOTSET){

      if(param_table->param[i].ptr_raw)free(param_table->param[i].ptr_raw);
      param_table->param[i].ptr_raw = NULL;

      if(param_table->param[i].log)free(param_table->param[i].log);
      param_table->param[i].log = NULL;
      param_table->param[i].log_size = 0;
      param_table->param[i].log_index = 0;
    }
  }

  if(param_table->param){
    free(param_table->param);
    param_table->param = NULL;
  }

  param_table->num_registered = 0;
  param_table->max_entries = 0;

  return REG_SUCCESS;
}

/*----------------------------------------------------------*/

int Generate_control_filename(int SimIndex, char* filename)
{
  static int output_file_index = 0;

  /* Generate next filename in sequence for sending data to
     steerer & increment counter */

  sprintf(filename, "%s%s_%d", Sim_table.sim[SimIndex].file_root,
	  STR_TO_APP_FILENAME, output_file_index++);

  if(output_file_index == REG_MAX_NUM_FILES) output_file_index = 0;

  return REG_SUCCESS;
}

/*----------------------------------------------------------*/

int Sim_index_from_handle(int SimHandle)
{
  int i;
  int index = REG_SIM_HANDLE_NOTSET;

  /* Finds entry in Sim_table that has handle == SimHandle */
  /* Returns REG_SIM_HANDLE_NOTSET if no match found */

  for(i=0; i<Sim_table.max_entries; i++){

    if(Sim_table.sim[i].handle == SimHandle){

      index = i;
      break;
    }
  }

  if(index == REG_SIM_HANDLE_NOTSET){
    fprintf(stderr, "Sim_index_from_handle: failed to find matching handle\n");
  }

  return index;
}

/*----------------------------------------------------------*/

int Dump_sim_table()
{
  int   isim;
  int   iparam;
  int   iotype;
  int   icmd;

  FILE *fp;
  Sim_entry_type *simptr;
  param_entry    *paramptr;
  IOdef_entry    *ioptr;
  supp_cmd_entry *cmdptr;

  fp = fopen("sim_table.txt", "a");

  if(fp == NULL) return REG_FAILURE;

  fprintf(fp, "####################################################\n\n");

  simptr = Sim_table.sim;

  if(simptr == NULL) return REG_FAILURE;

  for(isim=0; isim<Sim_table.max_entries; isim++){

    if(simptr->handle != REG_SIM_HANDLE_NOTSET){

      fprintf(fp, "Simulation index %d\n\n", isim);

      fprintf(fp, "###### Parameters ######\n\n");

      paramptr = simptr->Params_table.param;
      if(paramptr != NULL){

      	for(iparam=0; iparam<simptr->Params_table.max_entries; iparam++){
  
 	  if(paramptr->handle == REG_PARAM_HANDLE_NOTSET) continue;
  
 	  fprintf(fp, "Param index %d\n", iparam);
 	  fprintf(fp, "--------------\n");
 	  fprintf(fp, "Label  	 = %s\n", paramptr->label);
 	  fprintf(fp, "strble    = %d\n", paramptr->steerable);
 	  fprintf(fp, "type      = %d\n", paramptr->type);
 	  fprintf(fp, "handle    = %d\n", paramptr->handle);
 	  fprintf(fp, "value     = %s\n\n", paramptr->value);
 	  fprintf(fp, "min       = %s\n\n", paramptr->min_val);
 	  fprintf(fp, "min_valid = %d\n\n", paramptr->min_val_valid);
 	  fprintf(fp, "max       = %s\n\n", paramptr->max_val);
 	  fprintf(fp, "max_valid = %d\n\n", paramptr->max_val_valid);
  
 	  paramptr++;
        }
      }

      fprintf(fp, "###### IO Types  ######\n\n");

      ioptr = simptr->IOdef_table.io_def;
      if(ioptr != NULL){

      	for(iotype=0; iotype<simptr->IOdef_table.max_entries; iotype++){
  
 	  if(ioptr->handle == REG_IODEF_HANDLE_NOTSET) continue;
  
 	  fprintf(fp, "IO Type index %d\n", iotype);
 	  fprintf(fp, "----------------\n");
 	  fprintf(fp, "Label     = %s\n", ioptr->label);
 	  fprintf(fp, "handle    = %d\n", ioptr->handle);
 	  fprintf(fp, "direction = %d\n", ioptr->direction);
 	  fprintf(fp, "freq_param_handle = %d\n", ioptr->freq_param_handle);

          iparam = Param_index_from_handle(&(simptr->Params_table), 
			                   ioptr->freq_param_handle);
          if(iparam != -1){
 	    fprintf(fp, "frequency = %s\n", simptr->Params_table.param[iparam].value);
	  }

 	  fprintf(fp, "\n");

 	  ioptr++;
        }
      }

      fprintf(fp, "###### Chk Types  ######\n\n");

      ioptr = simptr->Chkdef_table.io_def;
      if(ioptr != NULL){

      	for(iotype=0; iotype<simptr->Chkdef_table.max_entries; iotype++){
  
 	  if(ioptr->handle == REG_IODEF_HANDLE_NOTSET) continue;
  
 	  fprintf(fp, "Chk Type index %d\n", iotype);
 	  fprintf(fp, "----------------\n");
 	  fprintf(fp, "Label     = %s\n", ioptr->label);
 	  fprintf(fp, "handle    = %d\n", ioptr->handle);
 	  fprintf(fp, "direction = %d\n", ioptr->direction);
 	  fprintf(fp, "freq_param_handle = %d\n", ioptr->freq_param_handle);

          iparam = Param_index_from_handle(&(simptr->Params_table), 
			                   ioptr->freq_param_handle);
          if(iparam != -1){
 	    fprintf(fp, "frequency = %s\n", simptr->Params_table.param[iparam].value);
	  }

 	  fprintf(fp, "\n");
  
 	  ioptr++;
        }
      }

      fprintf(fp, "###### Supported Cmds   ######\n\n");

      cmdptr = simptr->Cmds_table.cmd;
      if(cmdptr != NULL){

        for(icmd=0; icmd<simptr->Cmds_table.num_registered; icmd++){

	  fprintf(fp, "Supported cmd. index %d\n", icmd);
	  fprintf(fp, "-----------------------\n");
	  fprintf(fp, "Command ID = %d\n\n", cmdptr->cmd_id);

	  cmdptr++;
	}
      }
    }

    simptr++;
  }

  fclose(fp);

  return REG_SUCCESS;
} 

/*----------------------------------------------------------------*/

int Get_param_number(int  sim_handle,
		     int  steerable,
		     int *num_params)
{
  int return_status = REG_SUCCESS;
  int isim;
  int i;
  int count;

  if(num_params == NULL) return REG_FAILURE;

  /* Returns the number of valid <steerable> parameters currently
     associated with the simulation with handle sim_handle */

  if( (isim = Sim_index_from_handle(sim_handle)) != -1){
  
    count = 0;
    for(i=0; i<Sim_table.sim[isim].Params_table.max_entries; i++){
  
      /* Check that entry is valid & is not for a library-generated param */

      if(Sim_table.sim[isim].Params_table.param[i].handle != 
	 REG_PARAM_HANDLE_NOTSET &&
	 !(Sim_table.sim[isim].Params_table.param[i].is_internal)){

	if(Sim_table.sim[isim].Params_table.param[i].steerable == steerable){

	  count++;
	}
      }
    }

    *num_params = count;
  }
  else{
    return_status = REG_FAILURE;
  }

  return return_status;
}

/*----------------------------------------------------------------*/

int Get_param_values(int    sim_handle,
		     int    steerable,
		     int    num_params,
		     Param_details_struct *param_details)

{
  int return_status = REG_SUCCESS;
  int isim;
  int i, j;
  int count;

  /* Return lists of registered parameter handles and associated
     values (as strings), types and limits for the steered simulation 
     with handle sim_handle 

     num_param is the number to return and param_details should
     point to an array (of Param_details_struct's) of at least this length 

     if steerable == REG_TRUE (1) then return steerable params, if REG_FALSE 
     (0) then return monitoring params */

  if(!param_details) {
    fprintf(stderr, "Get_param_values: ptr to param_details is NULL\n");
    return REG_FAILURE;
  }

  isim = Sim_index_from_handle(sim_handle);
  if(isim != -1){
  
    count = 0;

    for(i=0; i<Sim_table.sim[isim].Params_table.max_entries; i++){

      /* Check that entry is valid & is not for a library-generated param */

      if(Sim_table.sim[isim].Params_table.param[i].handle != 
	 REG_PARAM_HANDLE_NOTSET &&
	 !(Sim_table.sim[isim].Params_table.param[i].is_internal)){

	if(Sim_table.sim[isim].Params_table.param[i].steerable == steerable){

	  param_details[count].handle = 
	                     Sim_table.sim[isim].Params_table.param[i].handle;

	  strcpy(param_details[count].label, 
		 Sim_table.sim[isim].Params_table.param[i].label);
	  /* Strip off any trailing space (often an issue with strings
	     supplied from F90) */
	  j = strlen(param_details[count].label);
	  while(param_details[count].label[--j] == ' ');

	  if(param_details[count].label[j] != '\0'){

	    param_details[count].label[j+1] = '\0';
	  }

	  strcpy(param_details[count].value, 
		 Sim_table.sim[isim].Params_table.param[i].value);

          param_details[count].type = Sim_table.sim[isim].Params_table.param[i].type;

	  if(Sim_table.sim[isim].Params_table.param[i].min_val_valid == REG_TRUE){
	    strcpy(param_details[count].min_val, 
		   Sim_table.sim[isim].Params_table.param[i].min_val);
	  }
	  else{
	    sprintf(param_details[count].min_val, "--");
	  }
	  if(Sim_table.sim[isim].Params_table.param[i].max_val_valid == REG_TRUE){
	    strcpy(param_details[count].max_val, 
		   Sim_table.sim[isim].Params_table.param[i].max_val);
	  }
	  else{
	    sprintf(param_details[count].max_val, "--");
	  }

	  /* For monitored parameters that are simply chunks of raw data we pass
	     back a pointer to this data */
	  if(param_details[count].type == REG_BIN){
	    param_details[count].raw_data = 
	      Sim_table.sim[isim].Params_table.param[i].ptr_raw;
	  }

	  count++;

	  if(count == num_params)break;
	}
      }
    }
  }
  else{
    return_status = REG_FAILURE;
  }
  
  return return_status;
}

/*----------------------------------------------------------------*/

int Set_param_values(int    sim_handle,
		     int    num_params,
		     int   *handles,
		     char* *vals)
{
  int return_status = REG_SUCCESS;
  int isim;
  int index;
  int i;
  int outside_range;

  int    ivalue, imin, imax;
  float  fvalue, fmin, fmax;
  double dvalue, dmin, dmax;

  /* Set the 'values' (held as strings) of the listed params */

  isim = Sim_index_from_handle(sim_handle);
  if(isim != REG_SIM_HANDLE_NOTSET){

    for(i=0; i<num_params; i++){

      if( (index = Param_index_from_handle(&(Sim_table.sim[isim].Params_table), 
				 handles[i])) == REG_PARAM_HANDLE_NOTSET ){
	return_status = REG_FAILURE;
	break;
      }
      
      /* Only set the value if parameter is steerable */

      if(Sim_table.sim[isim].Params_table.param[index].steerable){

	outside_range = REG_FALSE;

	/* Enforce limits specified when parameter was registered */
	switch(Sim_table.sim[isim].Params_table.param[index].type){

	case REG_INT:
	  sscanf(vals[i], "%d", &ivalue);
	  if(Sim_table.sim[isim].Params_table.param[index].min_val_valid == REG_TRUE){
	    sscanf(Sim_table.sim[isim].Params_table.param[index].min_val, "%d",
		   &imin);
	    if (ivalue < imin) outside_range = REG_TRUE;
	  }
	  if(Sim_table.sim[isim].Params_table.param[index].max_val_valid == REG_TRUE){
	    sscanf(Sim_table.sim[isim].Params_table.param[index].max_val, "%d",
		   &imax);
	    if (ivalue > imax) outside_range = REG_TRUE;
	  }
	  if(outside_range == REG_TRUE){
	    fprintf(stderr, "Set_param_values: new value (%d) of %s is outside\n"
		    "permitted range (%s,%s) - skipping...\n", 
		    ivalue, 
		    Sim_table.sim[isim].Params_table.param[index].label, 
		    Sim_table.sim[isim].Params_table.param[index].min_val,
		    Sim_table.sim[isim].Params_table.param[index].max_val);
	    continue;
	  }
	  break;

	case REG_FLOAT:
	  sscanf(vals[i], "%f", &fvalue);
	  if(Sim_table.sim[isim].Params_table.param[index].min_val_valid == REG_TRUE){
	    sscanf(Sim_table.sim[isim].Params_table.param[index].min_val, "%f",
		   &fmin);
	    if (fvalue < fmin) outside_range = REG_TRUE;
	  }
	  if(Sim_table.sim[isim].Params_table.param[index].max_val_valid == REG_TRUE){
	    sscanf(Sim_table.sim[isim].Params_table.param[index].max_val, "%f",
		   &fmax);
	    if (fvalue > fmax) outside_range = REG_TRUE;
	  }

	  if(outside_range == REG_TRUE){
	    fprintf(stderr, "Set_param_values: new value (%f) of %s is outside\n"
		    "permitted range (%s,%s) - skipping...\n", 
		    fvalue, 
		    Sim_table.sim[isim].Params_table.param[index].label, 
		    Sim_table.sim[isim].Params_table.param[index].min_val, 
		    Sim_table.sim[isim].Params_table.param[index].max_val);
	    continue;
	  }
	  break;

	case REG_DBL:
	  sscanf(vals[i], "%lg", &dvalue);
	  if(Sim_table.sim[isim].Params_table.param[index].min_val_valid == REG_TRUE){
	    sscanf(Sim_table.sim[isim].Params_table.param[index].min_val, 
		   "%lg", &dmin);
	    if(dvalue < dmin) outside_range = REG_TRUE;
	  }
	  if(Sim_table.sim[isim].Params_table.param[index].max_val_valid == REG_TRUE){
	    sscanf(Sim_table.sim[isim].Params_table.param[index].max_val, 
		   "%lg", &dmax);
	    if(dvalue > dmax) outside_range = REG_TRUE;
	  }

	  if(outside_range == REG_TRUE){
	    fprintf(stderr, "Set_param_values: new value (%f) of %s is outside\n"
		    "permitted range (%s,%s) - skipping...\n", 
		    (float)dvalue, 
		    Sim_table.sim[isim].Params_table.param[index].label, 
		    Sim_table.sim[isim].Params_table.param[index].min_val,
		    Sim_table.sim[isim].Params_table.param[index].max_val);
	    continue;
	  }
	  break;

	case REG_CHAR:
	  /* Max. value taken as maximum possible length of steered 
	     character strings */
	  if(Sim_table.sim[isim].Params_table.param[index].max_val_valid == REG_TRUE){
	    sscanf(Sim_table.sim[isim].Params_table.param[index].max_val, "%d",
		   &imax);
	    if (strlen(vals[i]) > imax) {

	      fprintf(stderr, "Set_param_values: new string (%s) of %s exceeds\n"
		      "maximum length (%s) - skipping...\n", 
		      vals[i], 
		      Sim_table.sim[isim].Params_table.param[index].label, 
		      Sim_table.sim[isim].Params_table.param[index].max_val);
	      continue;
	    }
	  }
	  break;

	default:
	  break;

	}

	sprintf(Sim_table.sim[isim].Params_table.param[index].value,
		"%s", vals[i]);
	Sim_table.sim[isim].Params_table.param[index].modified = REG_TRUE;
      }
      else{
	fprintf(stderr, "Set_param_values: can only edit steerable parameters\n");
      }
    }
  }
  else{
    return_status = REG_FAILURE;
  }

  return return_status;
}

/*----------------------------------------------------------------*/

int Get_param_log(int      sim_handle,
		  int      handle,
		  double **buf, 
		  int     *num_entries)
{
  int isim;
  int index;

  *buf = NULL;
  *num_entries = 0;

  isim = Sim_index_from_handle(sim_handle);
  if(isim == REG_SIM_HANDLE_NOTSET){
    return REG_FAILURE;
  }

  if( (index = Param_index_from_handle(&(Sim_table.sim[isim].Params_table), 
				       handle)) == REG_PARAM_HANDLE_NOTSET ){
    return REG_FAILURE;
  }

  *buf = Sim_table.sim[isim].Params_table.param[index].log;
  *num_entries = Sim_table.sim[isim].Params_table.param[index].log_index;

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Next_free_iodef_index(IOdef_table_type *table)
{
  int   i;
  int   index = -1;
  int   new_size;
  void *dum_ptr;

  /* Look for first free entry in table - i.e. one that has an
     unset handle.  If none found then extends the size of the table. */

  for(i=0; i<table->max_entries; i++){

    if(table->io_def[i].handle == REG_IODEF_HANDLE_NOTSET){

      index = i;
      break;
    }
  }

  if(index == -1){

    /* No free entries - need to allocate more memory */

    new_size = table->max_entries + REG_INITIAL_NUM_IOTYPES;
    
    if( (dum_ptr = (void *)realloc(table->io_def, 
				   new_size*sizeof(param_entry))) ){

      index = table->max_entries;
      table->io_def = (IOdef_entry *)dum_ptr;
      table->max_entries = new_size;
    }
  }

  return index;
}

/*-------------------------------------------------------------------*/

int Next_free_sim_index()
{
  int   i;
  int   index = -1;
  int   new_size;
  void *dum_ptr;

  /* Look for first free entry in table - i.e. one that has an
     unset handle.  If none found then extends the size of the table. */

  for(i=0; i<Sim_table.max_entries; i++){

    if(Sim_table.sim[i].handle == REG_SIM_HANDLE_NOTSET){

      index = i;
      break;
    }
  }

  if(index == -1){

    /* No free table entries - need to allocate more memory */

    new_size = Sim_table.max_entries + REG_MAX_NUM_STEERED_SIM;

    dum_ptr = (void*)realloc(Sim_table.sim, new_size*sizeof(Sim_entry_type));

    if(dum_ptr){

      Sim_table.sim = (Sim_entry_type *)dum_ptr;
      index = Sim_table.max_entries;
      Sim_table.max_entries = new_size;
    }
  }

  return index;
}

/*-------------------------------------------------------------------*/

int Get_iotype_number(int sim_handle,
		      int *num_iotypes)
{
  int i;
  int isim;
  int count;
  int return_status = REG_SUCCESS;

  /* Calculates the number of valid IO types currently registered with the
     simulation with handle sim_handle */

  if(num_iotypes == NULL)return REG_FAILURE;

  if((isim = Sim_index_from_handle(sim_handle)) != -1){

    count = 0;

    for(i=0; i<Sim_table.sim[isim].IOdef_table.max_entries; i++){

      if(Sim_table.sim[isim].IOdef_table.io_def[i].handle != 
	 REG_IODEF_HANDLE_NOTSET) count++;
    }

    *num_iotypes = count;
  }
  else{
    return_status = REG_FAILURE;
  }

  return return_status;
}

/*-------------------------------------------------------------------*/

int Get_iotypes(int    sim_handle,
		int    num_iotypes,
		int   *handles,
		char* *labels,
		int   *types,
		int   *io_freqs)
{
  int isim;
  int i, j;
  int count;
  int iparam;
  int nitem;
  int return_status = REG_SUCCESS;
  char *pchar;

  /* Get the first num_iotype IO defs out of the table.  Assumes
     that Get_iotype_number has been called first to get the number
     of entries in the table. */

  if(labels == NULL || handles == NULL)return REG_FAILURE;

  if((isim = Sim_index_from_handle(sim_handle)) != REG_SIM_HANDLE_NOTSET){

    count = 0;

    for(i=0; i<Sim_table.sim[isim].IOdef_table.max_entries; i++){

      if(Sim_table.sim[isim].IOdef_table.io_def[i].handle != 
	 REG_IODEF_HANDLE_NOTSET){

	handles[count] = Sim_table.sim[isim].IOdef_table.io_def[i].handle;

	strcpy(labels[count], 
	       Sim_table.sim[isim].IOdef_table.io_def[i].label);
	/* Strip off any trailing space (often an issue with strings
	   supplied from F90) */
	pchar = labels[count];
	j = strlen(pchar);
	while(pchar[--j] == ' ');

	if(pchar[j] != '\0'){

	  pchar[j+1] = '\0';
	}

	types[count] = Sim_table.sim[isim].IOdef_table.io_def[i].direction;

	/* Get the current frequency at which this occurs */
	iparam = Param_index_from_handle(&(Sim_table.sim[isim].Params_table),
		                         Sim_table.sim[isim].IOdef_table.io_def[i].freq_param_handle);

	if(iparam != REG_PARAM_HANDLE_NOTSET){

	  nitem = sscanf(Sim_table.sim[isim].Params_table.param[iparam].value, 
			   "%d", &(io_freqs[count]) );
	  if(nitem != 1){

	    fprintf(stderr, "Get_iotypes: failed to retrieve freq value\n");
	    io_freqs[count] = 0;
	    return_status = REG_FAILURE;
	  }
	}
	else{

	  fprintf(stderr, "Get_iotypes: failed to match param handle\n");
	  io_freqs[count] = 0;
	  return_status = REG_FAILURE;
	}
	
	count++;

	if(count == num_iotypes)break;
      }
    }
  }
  else{

    fprintf(stderr, "Get_iotypes: unknown sim handle: %d\n", sim_handle);
    return_status = REG_FAILURE;
  }

  return return_status;
}

/*----------------------------------------------------------------------*/

int Set_iotype_freq(int sim_handle,
		    int num_iotypes,
		    int *iotype_handles,
		    int *freqs)
{
  int  isim;
  int  i;
  int  itype;
  char *val_array;
  char param_val[REG_MAX_STRING_LENGTH];
  int  return_status = REG_SUCCESS;

  /* A utility function that allows the steerer to update the emit/consume
     frequency associated with a given IOtype - the frequency itself is
     stored as a steerable parameter and therefore must be looked-up */

  if((isim = Sim_index_from_handle(sim_handle)) != REG_SIM_HANDLE_NOTSET){

    for(itype=0; itype<num_iotypes; itype++){

      /* Find IOdef with matching handle */
      for(i=0; i<Sim_table.sim[isim].IOdef_table.max_entries; i++){

	if(Sim_table.sim[isim].IOdef_table.io_def[i].handle ==
	   iotype_handles[itype]) break;
      }

      if(i==Sim_table.sim[isim].IOdef_table.max_entries){

#if REG_DEBUG
	fprintf(stderr, "Set_iotype_freq: failed to match iotype handle\n");
#endif
	return_status = REG_FAILURE;
	continue;
      }

      /* Identify which entry in the parameter table corresponds to the
	 emit/consume frequency for this iodef */
      sprintf(param_val, "%d", freqs[itype]);

      val_array = param_val;

      return_status =  Set_param_values(sim_handle,
					1,
					&(Sim_table.sim[isim].IOdef_table.io_def[i].freq_param_handle),
					&val_array);
      
    }
  }
  else{

#if REG_DEBUG
    fprintf(stderr, "Set_iotype_freq: failed to match sim_handle\n");
#endif
    return_status = REG_FAILURE;
  }

  return return_status;
}

/*-------------------------------------------------------------------*/

int Get_chktype_number(int  sim_handle,
		       int *num_chktypes)
{
  int i;
  int isim;
  int count;
  int return_status = REG_SUCCESS;

  /* Calculates the number of valid chk types currently registered with the
     simulation with handle sim_handle */

  if(num_chktypes == NULL)return REG_FAILURE;

  if((isim = Sim_index_from_handle(sim_handle)) != -1){

    count = 0;

    for(i=0; i<Sim_table.sim[isim].Chkdef_table.max_entries; i++){

      if(Sim_table.sim[isim].Chkdef_table.io_def[i].handle != 
	 REG_IODEF_HANDLE_NOTSET) count++;
    }

    *num_chktypes = count;
  }
  else{
    return_status = REG_FAILURE;
  }

  return return_status;
}

/*-------------------------------------------------------------------*/

int Get_chktypes(int    sim_handle,
	 	 int    num_chktypes,
		 int   *handles,
		 char* *labels,
		 int   *types,
		 int   *chk_freqs)
{
  int isim;
  int i, j;
  int count;
  int iparam;
  int nitem;
  int return_status = REG_SUCCESS;
  char *pchar;

  /* Get the first num_chktype Chk defs out of the table.  Assumes
     that Get_chktype_number has been called first to get the number
     of entries in the table. */

  if(labels == NULL || handles == NULL)return REG_FAILURE;

  if((isim = Sim_index_from_handle(sim_handle)) != REG_SIM_HANDLE_NOTSET){

    count = 0;

    for(i=0; i<Sim_table.sim[isim].Chkdef_table.max_entries; i++){

      if(Sim_table.sim[isim].Chkdef_table.io_def[i].handle != 
	 REG_IODEF_HANDLE_NOTSET){

	handles[count] = Sim_table.sim[isim].Chkdef_table.io_def[i].handle;

	strcpy(labels[count], 
	       Sim_table.sim[isim].Chkdef_table.io_def[i].label);

	/* Strip off any trailing space (often an issue with strings
	   supplied from F90) */
	pchar = labels[count];
	j = strlen(pchar);
	while(pchar[--j] == ' ');

	if(pchar[j] != '\0'){

	  pchar[j+1] = '\0';
	}

	types[count] = Sim_table.sim[isim].Chkdef_table.io_def[i].direction;

	/* Get the current frequency at which this occurs */
	if(types[count] != REG_IO_IN){

	  iparam = Param_index_from_handle(&(Sim_table.sim[isim].Params_table),
		                           Sim_table.sim[isim].Chkdef_table.io_def[i].freq_param_handle);

	  if(iparam != REG_PARAM_HANDLE_NOTSET){

	    nitem = sscanf(Sim_table.sim[isim].Params_table.param[iparam].value, 
			   "%d", &(chk_freqs[count]) );
	    if(nitem != 1){

#if REG_DEBUG
	      fprintf(stderr, "Get_chktypes: failed to retrieve freq value\n");
#endif
	      chk_freqs[count] = 0;
	      return_status = REG_FAILURE;
	    }
	  }
	  else{
#if REG_DEBUG
	    fprintf(stderr, "Get_chktypes: failed to match param handle\n");
#endif
	    chk_freqs[count] = 0;
	    return_status = REG_FAILURE;
	  }
	}
	else{

	  /* Frequency not meaningful for checkpoints registered as inputs
	     (since this implies a restart of the simulation) */
	  chk_freqs[count] = 0;
	}

	count++;

	if(count == num_chktypes)break;
      }
    }
  }
  else{
    return_status = REG_FAILURE;
  }

  return return_status;
}

/*----------------------------------------------------------------------*/

int Set_chktype_freq(int  sim_handle,
		     int  num_chktypes,
		     int *chktype_handles,
		     int *freqs)
{
  int  isim;
  int  i;
  int  itype;
  char *val_array;
  char param_val[REG_MAX_STRING_LENGTH];
  int  return_status = REG_SUCCESS;

  /* A utility function that allows the steerer to update the emit/consume
     frequency associated with a given IOtype - the frequency itself is
     stored as a steerable parameter and therefore must be looked-up */

  if((isim = Sim_index_from_handle(sim_handle)) != REG_SIM_HANDLE_NOTSET){

    for(itype=0; itype<num_chktypes; itype++){

      /* Find Chkdef with matching handle */
      for(i=0; i<Sim_table.sim[isim].Chkdef_table.max_entries; i++){

	if(Sim_table.sim[isim].Chkdef_table.io_def[i].handle ==
	   chktype_handles[itype]) break;
      }

      if(i==Sim_table.sim[isim].Chkdef_table.max_entries){

#if REG_DEBUG
	fprintf(stderr, "Set_chktype_freq: failed to match iotype handle\n");
#endif
	return_status = REG_FAILURE;
	continue;
      }

      if(Sim_table.sim[isim].Chkdef_table.io_def[i].direction == REG_IO_IN){

#if REG_DEBUG
	fprintf(stderr, "Set_chktype_freq: frequency ignored for ChkTypes"
		"with direction 'IN'\n");
#endif
	continue;
      }

      /* Identify which entry in the parameter table corresponds to the
	 emission frequency for this chkdef */
      sprintf(param_val, "%d", freqs[itype]);

      val_array = param_val;

      return_status =  Set_param_values(sim_handle,
					1,
					&(Sim_table.sim[isim].Chkdef_table.io_def[i].freq_param_handle),
					&val_array);
      
    }
  }
  else{

#if REG_DEBUG
    fprintf(stderr, "Set_chktype_freq: failed to match sim_handle\n");
#endif
    return_status = REG_FAILURE;
  }

  return return_status;
}

/*----------------------------------------------------------------------*/

int Command_supported(int sim_id,
		      int cmd_id)
{
  int return_status = REG_FAILURE;
  int i;

  /* Check to see whether or not the simulation with index sim_id
     supports command cmd_id - returns REG_SUCCESS if it does
     and REG_FAILURE if it doesn't */

  if(cmd_id < REG_MIN_IOTYPE_HANDLE){

    /* Command is a regular, pre-defined command */

    for(i=0; i<Sim_table.sim[sim_id].Cmds_table.num_registered; i++){

      if(cmd_id == Sim_table.sim[sim_id].Cmds_table.cmd[i].cmd_id){

	return_status = REG_SUCCESS;
	break;
      }
    }
  }
  else{

    /* Command is an IO type or Chk type */

    for(i=0; i<Sim_table.sim[sim_id].IOdef_table.max_entries; i++){

      if(cmd_id == Sim_table.sim[sim_id].IOdef_table.io_def[i].handle){

	return_status = REG_SUCCESS;
	break;
      }
    }
    if(return_status != REG_SUCCESS){

      for(i=0; i<Sim_table.sim[sim_id].Chkdef_table.max_entries; i++){

	if(cmd_id == Sim_table.sim[sim_id].Chkdef_table.io_def[i].handle){

	  return_status = REG_SUCCESS;
	  break;
	}
      }
    }
  }
  
#if REG_DEBUG
  if(return_status != REG_SUCCESS){

    fprintf(stderr, 
	    "Command_supported: command %d is not supported by the sim.\n", 
	    cmd_id);
  }
#endif

  return return_status;
}

/*-------------------------------------------------------------------*/

int Get_supp_cmd_number(int sim_handle,
		        int *num_cmds)
{
  int isim;
  int return_status = REG_SUCCESS;
  
  /* Returns the number of commands supported by the specified sim */

  if (num_cmds == NULL) return REG_FAILURE;

  if((isim = Sim_index_from_handle(sim_handle)) != -1){

    *num_cmds = Sim_table.sim[isim].Cmds_table.num_registered;
  }
  else{
    return_status = REG_FAILURE;
  }

  return return_status;
}

/*-------------------------------------------------------------------*/

int Get_supp_cmds(int  sim_handle,
		  int  num_cmds,
		  int  *cmd_ids)
{
  int i;
  int isim;
  int loop_limit;
  int return_status = REG_SUCCESS;

  /* Returns the first num_cmds commands supported by the specified sim.
     Assumes that Get_supp_cmd_number has been called first to get the
     no. of entries in the table */

  if (cmd_ids == NULL) return REG_FAILURE;

  if((isim = Sim_index_from_handle(sim_handle)) != -1){

    if(num_cmds < Sim_table.sim[isim].Cmds_table.num_registered){

      loop_limit = num_cmds;
    }
    else{
      loop_limit = Sim_table.sim[isim].Cmds_table.num_registered;
    }
    
    /* Copy command id's out of table and into supplied array */

    for(i=0; i<loop_limit; i++){

      cmd_ids[i] = Sim_table.sim[isim].Cmds_table.cmd[i].cmd_id;
    }
  }
  else{
    return_status = REG_FAILURE;
  }

  return return_status;
}
/*-------------------------------------------------------------------*/

int Get_chk_log_number(int   sim_handle,
		       int   chk_handle,
		       int  *num_entries)
{
  int i;
  int isim;
  int return_status = REG_SUCCESS;

  *num_entries = 0;

  if((isim = Sim_index_from_handle(sim_handle)) != -1){

    for(i=0; i<Sim_table.sim[isim].Chk_log.num_entries; i++){

      if(Sim_table.sim[isim].Chk_log.entry[i].chk_handle == chk_handle){

	(*num_entries)++;
      }
    }
  }
  else{

    return_status = REG_FAILURE;
  }

  return return_status;
}

/*-------------------------------------------------------------------*/

int Get_chk_log_entries(int                 sim_handle,
			int                 chk_handle,
			int                 num_entries,
			Output_log_struct  *entries)
{
  int i;
  int isim;
  int count;
  Sim_entry_type *sim;
  int return_status = REG_SUCCESS;

  if(!entries) return REG_FAILURE;

  if((isim = Sim_index_from_handle(sim_handle)) != -1){

    sim = &(Sim_table.sim[isim]);
    count = 0;

    for(i=0; i<sim->Chk_log.num_entries; i++){

      if(sim->Chk_log.entry[i].chk_handle == chk_handle){

	Get_log_entry_details(&(sim->Params_table),
			      &(sim->Chk_log.entry[i]),
			      &(entries[count]));

	count++;
	if (count >= num_entries) break;
      }
    }
  }
  else{

    return_status = REG_FAILURE;
  }

  return return_status;
}

/*-------------------------------------------------------------------*/

int Get_chk_log_entries_reverse(int                 sim_handle,
				int                 chk_handle,
				int                 num_entries,
				Output_log_struct  *entries)
{
  int i;
  int isim;
  int count;
  int return_status = REG_SUCCESS;
  Sim_entry_type *sim;

  if(!entries) return REG_FAILURE;

  if((isim = Sim_index_from_handle(sim_handle)) != -1){

    sim = &(Sim_table.sim[isim]);
    count = 0;

    for(i=(sim->Chk_log.num_entries - 1); i>=0; i--){

      if(sim->Chk_log.entry[i].chk_handle == chk_handle){

	Get_log_entry_details(&(sim->Params_table),
			      &(sim->Chk_log.entry[i]),
			      &(entries[count]));

	count++;
	if (count >= num_entries) break;
      }
    }
  }
  else{

    return_status = REG_FAILURE;
  }

  return return_status;
}

/*-------------------------------------------------------------------*/

int Get_log_entry_details(Param_table_type *param_table,
			  Chk_log_entry_type *in,
			  Output_log_struct  *out)
{
  int   i, j;
  int   index;
  char *pchar;

  strcpy(out->chk_tag, in->chk_tag);

  for(i=0; i<REG_MAX_NUM_STR_PARAMS; i++){

    if(in->param[i].handle == REG_PARAM_HANDLE_NOTSET) break;

    index = Param_index_from_handle(param_table, 
				    in->param[i].handle);

    if(index == -1){
      fprintf(stderr, "Get_log_entry_details: error - failed"
	      " to match param handle: %d\n", 
	      in->param[i].handle);
      continue;
    }

    strcpy(out->param_labels[i], 
	   param_table->param[index].label);

    /* Strip off any trailing space (often an issue with strings
       supplied from F90) */
    pchar = out->param_labels[i];
    j = strlen(pchar);

    while(pchar[--j] == ' ');

    if(pchar[j] != '\0'){

      pchar[j+1] = '\0';
    }

    strcpy(out->param_values[i],
	   in->param[i].value);
  }
  out->num_param = i;

  return REG_SUCCESS;
}

/*-------------------------------------------------------------------*/

int Sim_attach_local(Sim_entry_type *sim, char *SimID)
{
  char *pchar;
  char  file_root[REG_MAX_STRING_LENGTH];
  char  filename[REG_MAX_STRING_LENGTH];
  char  sys_cmd[REG_MAX_STRING_LENGTH];
  int   return_status;

  return_status = REG_FAILURE;

  /* Unless SimID contains a string, get the directory to use for 
     steering messages from the REG_STEER_DIRECTORY env. variable.  */
  if(strlen(SimID) != 0){
    pchar = SimID;

    /* Check that path ends in '/' - if not then add one */
    if( pchar[strlen(pchar)-1] != '/' ){

      sprintf(file_root, "%s/", pchar);
    }
    else{
      strcpy(file_root, pchar);
    }

    return_status = Directory_valid(file_root);
#if REG_DEBUG
    if(return_status != REG_SUCCESS){
      fprintf(stderr, "Sim_attach_local: invalid dir for "
	      "steering messages: %s\n", file_root);
    }
#endif
  }

  /* If supplied ID didn't work then try using REG_STEER_DIRECTORY env.
     variable instead */
  if(return_status != REG_SUCCESS){
    
    if(!(pchar = getenv("REG_STEER_DIRECTORY"))){
      fprintf(stderr, "Sim_attach_local: failed to get scratch directory\n");
      return REG_FAILURE;
    }
    
    /* Check that path ends in '/' - if not then add one */
    if( pchar[strlen(pchar)-1] != '/' ){

      sprintf(file_root, "%s/", pchar);
    }
    else{
      strcpy(file_root, pchar);
    }

    return_status = Directory_valid(file_root);
#if REG_DEBUG
    if(return_status != REG_SUCCESS){
      fprintf(stderr, "Sim_attach_local: invalid dir for "
	      "steering messages: %s\n", file_root);
    }
#endif
  }

  if(return_status != REG_SUCCESS){
    fprintf(stderr, "Sim_attach_local: failed to get scratch directory\n");    
    return REG_FAILURE;
  }
#if REG_DEBUG
  else{
    fprintf(stderr, "Sim_attach_local: using following dir for "
	    "steering messages: %s\n", file_root);
  }
#endif

  /* Delete any old communication files written by an app 
     in this location */

  sprintf(filename, "%s%s", file_root, 
	  APP_TO_STR_FILENAME);

  Remove_files(filename);

  /* Save directory used for communication */
  strcpy(sim->file_root, file_root);
 
  /* Read the commands that the application supports */
  return_status = Consume_supp_cmds_local(sim);

  if(return_status == REG_SUCCESS){

    /* Create lock file to indicate that steerer has connected to sim */
    sprintf(sys_cmd, "touch %s%s", sim->file_root, 
	    STR_CONNECTED_FILENAME);
    system(sys_cmd);
  }

  return return_status;
}

/*-------------------------------------------------------------------*/

int Sim_attach_proxy(Sim_entry_type *sim, char *SimID)
{
  char  buf[REG_MAX_MSG_SIZE];
  int   nbytes;
  int   return_status = REG_SUCCESS;

  struct msg_struct *msg;
  struct cmd_struct *cmd;

  /* Create a proxy for the simulation and use this to attach to it */

  return_status = Create_proxy(&(sim->pipe_to_proxy),
			       &(sim->pipe_from_proxy));

  if(return_status != REG_SUCCESS){

    fprintf(stderr, "Sim_attach_proxy: failed to launch proxy\n");
    
    return REG_FAILURE;
  }

  /* Send 'attach' instruction */
  Send_proxy_message(sim->pipe_to_proxy, ATTACH_MSG);

  /* Send GSH of grid-service to attach to */
  Send_proxy_message(sim->pipe_to_proxy, SimID);

  /* Check success */
  Get_proxy_message(sim->pipe_from_proxy, buf, &nbytes);

  if(strncmp(buf, OK_MSG, nbytes) != 0){

    fprintf(stderr, "Sim_attach_proxy: proxy failed to attach to application\n");

    /* Signal proxy to stop */
    Send_proxy_message(sim->pipe_to_proxy, QUIT_MSG);

    return REG_FAILURE;      
  }

  /* If OK, then get list of supported commands back from proxy */
  return_status = Get_proxy_message(sim->pipe_from_proxy, buf, &nbytes);

  if(return_status != REG_SUCCESS){

    fprintf(stderr, "Sim_attach_proxy: failed to get list of cmds from proxy\n");
    /* Signal proxy to stop */
    Send_proxy_message(sim->pipe_to_proxy, QUIT_MSG);

    return REG_FAILURE;
  }

  /* Get message structure to fill */
  msg = New_msg_struct();

  if(!msg){

    fprintf(stderr, "Sim_attach_proxy: failed to get new message struct\n");

    /* Signal proxy to stop */
    Send_proxy_message(sim->pipe_to_proxy, QUIT_MSG);

    return REG_FAILURE;
  }

  /* Parse the returned string */
  if(Parse_xml_buf(buf, nbytes, msg, sim) == REG_SUCCESS){

    cmd = msg->supp_cmd->first_cmd;

    while(cmd != NULL){

      if(sscanf((char *)cmd->id , "%d", 
		&(sim->Cmds_table.cmd[sim->Cmds_table.num_registered].cmd_id))
		!= 1){

	fprintf(stderr, "Sim_attach_proxy: error reading cmd_id\n");
	return_status = REG_FAILURE;
	break;
      }

      /* ARPDBG - need to do cmd params too? */

      cmd = cmd->next;
      Increment_cmd_registered(&(sim->Cmds_table));
    }
  }

  if(return_status != REG_SUCCESS){
    
    /* Signal proxy to stop */
    Send_proxy_message(sim->pipe_to_proxy, QUIT_MSG);
  }

  /* Clean up */

  Delete_msg_struct(&msg);
  msg = NULL;

  return return_status;
}

/*-------------------------------------------------------------------*/


int Consume_supp_cmds_local(Sim_entry_type *sim)
{
  FILE              *fp1;
  FILE              *fp2;
  char               filename[REG_MAX_STRING_LENGTH];
  struct msg_struct *msg;
  struct cmd_struct *cmd;
  int                return_status = REG_SUCCESS;

  /* Check for absence of lock file indicating that sim is
     already being steered */

  sprintf(filename, "%s%s", sim->file_root, STR_CONNECTED_FILENAME);
  fp2 = fopen(filename, "r");

  /* Check for presence of lock file indicating sim is steerable */

  sprintf(filename, "%s%s", sim->file_root, APP_STEERABLE_FILENAME);
  fp1 = fopen(filename, "r");

  if(fp1 != NULL && fp2 == NULL){

    fclose(fp1);

    msg = New_msg_struct();

    return_status = Parse_xml_file(filename, msg, sim);

    if(return_status == REG_SUCCESS){

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

      fprintf(stderr, "Consume_supp_cmds_local: error parsing <%s>\n",
	      filename);
    }

    Delete_msg_struct(&msg);
  }
  else{

    if(fp2) fclose(fp2);

    return REG_FAILURE;
  }

  return return_status;
}

/*-------------------------------------------------------------------*/

int Finalize_connection(Sim_entry_type *sim)
{

  if(sim->pipe_to_proxy != REG_PIPE_UNSET){

    return Finalize_connection_proxy(sim);
  }
  else{

    if(sim->SGS_info.active){

#if REG_OGSI
      /* Detach from the SGS and then clean up */
      if(sim->detached = REG_FALSE){
	if(Send_detach_msg_soap(sim) == REG_SUCCESS){
	  sim->detached = REG_TRUE;
	}
      }
      return Finalize_connection_soap(sim);
#else
      if(sim->detached = REG_FALSE){
	if(Send_detach_msg_wsrf(sim) == REG_SUCCESS){
	  sim->detached = REG_TRUE;
	}
      }
      return Finalize_connection_wsrf(sim);
#endif
    }

    return Finalize_connection_file(sim);
  }
}

/*-------------------------------------------------------------------*/

int Finalize_connection_proxy(Sim_entry_type *sim)
{
  Destroy_proxy(sim->pipe_to_proxy);
  sim->pipe_to_proxy   = REG_PIPE_UNSET;
  sim->pipe_from_proxy = REG_PIPE_UNSET;

  return REG_SUCCESS;
}

/*-------------------------------------------------------------------*/

int Finalize_connection_file(Sim_entry_type *sim)
{
  char base_name[REG_MAX_STRING_LENGTH];

  /* Delete any files that the app's produced that we won't now be
     consuming */

  sprintf(base_name, "%s%s", sim->file_root, 
	  APP_TO_STR_FILENAME);

  Remove_files(base_name);

  return REG_SUCCESS;
}

/*-------------------------------------------------------------------*/

int Realloc_param_log(param_entry *param)
{
  static int chunk_size = 500;
  void *dum_ptr;

  if(!param)return REG_FAILURE;

  if(param->log){

    param->log_size += chunk_size;
    if( !(dum_ptr = realloc(param->log,
			    param->log_size*sizeof(double))) ){
      free(param->log);
      param->log = NULL;
      param->log_size = 0;
      param->log_index = 0;
      fprintf(stderr, "Realloc_param_log: realloc failed for %d"
	      " bytes\n", (int)(param->log_size*sizeof(double)));
      return REG_FAILURE;
    }
    param->log = (double *)dum_ptr;
  }
  else{
    param->log_index = 0;
    param->log_size = chunk_size;

    if( !(param->log = (double *)malloc(param->log_size*sizeof(double))) ){
      param->log_size = 0;
      fprintf(stderr, "Realloc_param_log: failed to malloc mem for log\n");
      return REG_FAILURE;
    }
  }

  return REG_SUCCESS;
}
