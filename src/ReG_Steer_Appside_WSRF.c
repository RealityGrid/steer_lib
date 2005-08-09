/*----------------------------------------------------------------------------
  This file contains routines and data structures for SOAP-based 
  steering communication from the application side using the 
  WSRF specification.

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

/** @file ReG_Steer_Appside_WSRF.c
    @brief Code for WSRF over SOAP communications for the steered 
    application
  */

#include "ReG_Steer_types.h"
#include "ReG_Steer_Appside_internal.h"
#include "ReG_Steer_Appside_WSRF.h"
#include "ReG_Steer_Logging.h"
#include "soapH.h"

/* These three functions are defined in the steerside_WSRF code */
extern int Get_resource_property (SGS_info_type *sgs_info, 
				  char          *name,
				  char         **pRP);
extern int Get_resource_property_doc(SGS_info_type *sgs_info,
				     char         **pDoc);
extern struct msg_struct *Get_next_stored_msg(Sim_entry_type *sim);

#ifndef WIN32
#else
#define snprintf _snprintf
#endif

/* Need access to these tables which are actually declared in 
   ReG_Steer_Appside_internal.h and ReG_Steer_Appside.c */
extern IOdef_table_type IOTypes_table;

extern Steerer_connection_table_type Steerer_connection;

extern Param_table_type Params_table;

extern Chk_log_type Chk_log;

/* Absolute path of directory we are executing in */
extern char ReG_CurrentDir[REG_MAX_STRING_LENGTH];

/* Hostname of machine we are executing on */
extern char ReG_Hostname[REG_MAX_STRING_LENGTH];

/* Name (and version) of the application that has called us */
extern char ReG_AppName[REG_MAX_STRING_LENGTH];

extern char Global_scratch_buffer[];

/* Soap-specific declarations */

/** The gSoap environment structure */
static struct soap wsSoap;

/** Names of the SWS' ResourceProperties - MUST match those
    used in SWS.pm (as launched by container) */
char *SUPPORTED_CMDS_RP = "sws:supportedCommands";
/** @see SUPPORTED_CMDS_SDE */
char *PARAM_DEFS_RP     = "sws:paramDefinitions";
/** @see SUPPORTED_CMDS_SDE */
char *IOTYPE_DEFS_RP    = "sws:ioTypeDefinitions";
/** @see SUPPORTED_CMDS_SDE */
char *CHKTYPE_DEFS_RP   = "sws:chkTypeDefinitions";
/** @see SUPPORTED_CMDS_SDE */
char *STEER_STATUS_RP   = "sws:steererStatus";
/** @see SUPPORTED_CMDS_SDE */
char *MACHINE_ADDRESS_RP= "sws:machineAddress";
/** @see SUPPORTED_CMDS_SDE */
char *WORKING_DIR_RP    = "sws:workingDirectory";
/** @see SUPPORTED_CMDS_SDE */
char *APP_NAME_RP       = "sws:applicationName";
/** @see SUPPORTED_CMDS_SDE */
char *STATUS_MSG_RP     = "sws:statusMsg";

/*-------------------------------------------------------------------------*/

int Initialize_steering_connection_wsrf(int  NumSupportedCmds,
					int *SupportedCmds)
{
  char                              *pchar;
  char                               query_buf[REG_MAX_MSG_SIZE];
  struct wsrp__SetResourcePropertiesResponse out;

  /* malloc memory for soap struct for this connection and then
     initialise it */
  if(!(Steerer_connection.SGS_info.soap = 
                (struct soap*)malloc(sizeof(struct soap)))){

    fprintf(stderr, "Initialize_steering_connection_wsrf: failed to malloc "
	    "memory for soap struct\n");
    return REG_FAILURE;
  }

  /* Set location of steering scratch directory */
  if(Set_steering_directory() != REG_SUCCESS){

    fprintf(stderr, "Initialize_steering_connection_wsrf: failed to set "
	    "steering scratch directory - checkpoint info. will be "
	    "written to ./\n");
  }

  /* Get the address of the SWS for this application from an environment
     variable */
  pchar = getenv("REG_SGS_ADDRESS");

  if(pchar){

    snprintf(Steerer_connection.SGS_info.address, REG_MAX_STRING_LENGTH, 
	     "%s", pchar);
#if REG_DEBUG
    fprintf(stderr, "Initialize_steering_connection_wsrf: SWS address = %s\n",
	    Steerer_connection.SGS_info.address);
#endif
  }
  else{

    fprintf(stderr, "Initialize_steering_connection_wsrf: REG_SGS_ADDRESS "
	    "environment variable not set\n");
    return REG_FAILURE;
  }

  /* Initialise the soap run-time environment:
     Use this form to turn-on keep-alive for both incoming and outgoing
     http connections */
  soap_init2(Steerer_connection.SGS_info.soap, SOAP_IO_KEEPALIVE, 
	     SOAP_IO_KEEPALIVE);

  /* Since we are using KEEPALIVE, we can also ask gSOAP to bind the 
     socket to a specific port on the local machine - only do this if 
     GLOBUS_TCP_PORT_RANGE is set. */
  if( (pchar = getenv("GLOBUS_TCP_PORT_RANGE")) ){

    if(sscanf(pchar, "%d,%d", &(wsSoap.client_port_min), 
	      &(wsSoap.client_port_max)) != 2){
      wsSoap.client_port_min = 0;
      wsSoap.client_port_max = 0;
    }
  }

  /* Create msg to send to SGS */
  Make_supp_cmds_msg(NumSupportedCmds, SupportedCmds, 
		     Steerer_connection.supp_cmds, REG_MAX_MSG_SIZE);

  /* Strip off any xml version declaration */
  pchar = strstr(Steerer_connection.supp_cmds,"<ReG_steer_message");

  snprintf(query_buf, REG_MAX_MSG_SIZE, "<%s>%s</%s>", 
	  SUPPORTED_CMDS_RP, pchar, SUPPORTED_CMDS_RP);

#if REG_DEBUG_FULL
  fprintf(stderr, "Initialize_steering_connection_wsrf: sending 1st msg:\n"
	  ">>%s<<\n\n",query_buf);
#endif

  if(soap_call_wsrp__SetResourceProperties(Steerer_connection.SGS_info.soap,
					   Steerer_connection.SGS_info.address,
					   "", query_buf, &out) != SOAP_OK){
    soap_print_fault(Steerer_connection.SGS_info.soap, stderr);
    return REG_FAILURE;
  }

  /* Publish our location: machine and working directory - these
     are set in Steering_initialize prior to calling us */
  snprintf(query_buf, REG_MAX_MSG_SIZE, 
	   "<%s>%s</%s><%s>%s</%s><%s>%s</%s>", 
	   WORKING_DIR_RP, ReG_CurrentDir, WORKING_DIR_RP,
	   MACHINE_ADDRESS_RP, ReG_Hostname, MACHINE_ADDRESS_RP,
	   APP_NAME_RP, ReG_AppName, APP_NAME_RP);

#if REG_DEBUG_FULL
  fprintf(stderr, "Initialize_steering_connection_wsrf: sending 2nd msg:\n"
	  ">>%s<<\n\n",query_buf);
#endif
  if(soap_call_wsrp__SetResourceProperties(Steerer_connection.SGS_info.soap, 
					   Steerer_connection.SGS_info.address,
					   "", query_buf, &out) != SOAP_OK){
    fprintf(stderr, "Initialize_steering_connection_wsrf: failure\n");
    soap_print_fault(Steerer_connection.SGS_info.soap, stderr);
    return REG_FAILURE;
  }

  fprintf(stderr, "ARPDBG Initialize_steering_connection_wsrf: done ok\n");
  return REG_SUCCESS;
}

/*-------------------------------------------------------------------------*/

int Steerer_connected_wsrf ()
{
  char *steer_status;
  int   status;
  
  status = Get_resource_property(&(Steerer_connection.SGS_info),
				 STEER_STATUS_RP,
				 &steer_status);
  if(status != REG_SUCCESS){
    fprintf(stderr, "Steerer_connected_wsrf: Get_resource_property failed\n");
    return REG_FAILURE;
  }
#if REG_DEBUG
  else{
    fprintf(stderr, "Steerer_connected_wsrf: Get_resource_property returned: %s\n", 
	    steer_status);
  }
#endif

  if(strstr(steer_status, "ATTACHED")){
    return REG_SUCCESS;
  }
  /* Not used in WSRF implementation - ARPDBG
  else if(strstr(steer_status, "DETACHING")){
    * Steerer has attached and detached without us noticing and thus SGS
       is in 'detaching' state.  We have to 'detach' properly now to reset.*
    Detach_from_steerer_wsrf();
    * But we still return 'failure' because no steerer is attached *
  }
*/

  return REG_FAILURE;
}

/*-------------------------------------------------------------------------*/

int Send_status_msg_wsrf (char *msg)
{
  char  *sde_name;
  char   query_buf[REG_MAX_MSG_SIZE];
  int    nbytes;
  int    new_size;
  struct wsrp__SetResourcePropertiesResponse out;
  char  *pTmpBuf = NULL;
  char  *pbuf = NULL;

  /* Status & log messages are both sent as 'status' messages */
  if(strstr(msg, "<App_status>") || strstr(msg, "<Steer_log>")){

    sde_name = STATUS_MSG_RP;
  }
  else if(strstr(msg, "<Supported_commands>")){
      
    sde_name = SUPPORTED_CMDS_RP;
  }
  else if(strstr(msg, "<Param_defs>")){
    
    sde_name = PARAM_DEFS_RP;
  }
  else if(strstr(msg, "<IOType_defs>")){

    sde_name = IOTYPE_DEFS_RP;
  }
  else if(strstr(msg, "<ChkType_defs>")){

    sde_name = CHKTYPE_DEFS_RP;
  }
  else{
    fprintf(stderr, "Send_status_msg_wsrf: not a status or log msg and"
	    "no matching SDE name found either\n");
    return REG_FAILURE;
  }

  pTmpBuf = query_buf;
  nbytes = snprintf(query_buf, REG_MAX_MSG_SIZE, "<%s>%s</%s>", 
		    sde_name, msg, sde_name);

  /* Check for truncation - if it occurs then malloc a bigger buffer
     and try again */
  if((nbytes >= (REG_MAX_MSG_SIZE-1)) || (nbytes < 1)){

    new_size = strlen(msg) + 512;
    if(!(pbuf = (char *)malloc(new_size)) ){

      fprintf(stderr, "Send_status_msg_wsrf: malloc failed\n");
      return REG_FAILURE;
    }

    nbytes = snprintf(pbuf, new_size, "<%s>%s</%s>", 
		      sde_name, msg, sde_name);

    if((nbytes >= (new_size-1)) || (nbytes < 1)){
      
      free(pbuf);
      pbuf = NULL;
      fprintf(stderr, "Send_status_msg_wsrf: ERROR - msg truncated\n");
      return REG_FAILURE;
    }
    pTmpBuf = pbuf;
  }

  /* We loop until we have a clear-cut success or failure - i.e.
     if we have a deadlock we fall back to here and try again */
  while(1){

    if(soap_call_wsrp__SetResourceProperties(Steerer_connection.SGS_info.soap, 
					     Steerer_connection.SGS_info.address,
					     "", pTmpBuf, &out) != SOAP_OK){
      soap_print_fault(Steerer_connection.SGS_info.soap, stderr);
      return REG_FAILURE;
    }
    /* ARPDBG - maybe need to pass a fault back from SWS...
    if(strstr(out, REG_SGS_ERROR)){
      return REG_FAILURE;
    }
    if(!strstr(out, REG_SGS_TIMEOUT))break;
    */
    break;
  }

  if(pbuf){
    free(pbuf);
    pbuf = NULL;
  }

  return REG_SUCCESS;
}

/*-------------------------------------------------------------------------*/

struct msg_struct *Get_control_msg_wsrf ()
{
  struct msg_struct *msg = NULL;
  char *pBuf, *pLast, *pLastBut1;

  /* If we have a backlog of messages then return the next one 
     - we are only interested in control messages */
  msg = Get_next_stored_msg(NULL);
  while(msg){
    if(msg->control){
      return msg;
    }
    Delete_msg_struct(&msg);
    msg = Get_next_stored_msg(NULL);
  }

  /* Check for 'notifications' first */

  /* Now get any new control messages */
  if(Get_resource_property(&(Steerer_connection.SGS_info),
			   "controlMsg", &pBuf) != REG_SUCCESS){
    msg = New_msg_struct();
    msg->msg_type = MSG_ERROR;
    return msg;
  }

  pLastBut1 = NULL;
  while(pLastBut1 != pBuf){

    if( !(pLast = strstr(pBuf, "<sws:controlMsg")) ){
      /* No control message found */
      return NULL;
    }
    pLastBut1 = pLast;
    while(pLast){
      pLastBut1 = pLast;
      pLast = strstr(pLast+1, "<sws:controlMsg");
    }

    fprintf(stderr, "ARPDBG, parsing >>%s<<\n", pLastBut1);
    /* Parse the doc - pass NULLs in as this is appside so have no
       Sim_entry struct and results will be put in Msg_store struct */
    Parse_xml_buf(pLastBut1, strlen(pLastBut1), NULL, NULL);

    *pLastBut1 = '\0';
  }

  /* The results of parsing the ResourcePropertyDocument are stored
     as a series of messages - go through these until we find a 
     control message (if any) */
  msg = Get_next_stored_msg(NULL);
  while (msg && !(msg->control)){
    Delete_msg_struct(&msg);
    msg = Get_next_stored_msg(NULL);
  }
  return msg;
}

/*-------------------------------------------------------------------------*/

int Save_log_wsrf (char *log_data)
{
  struct sws__PutParamLogResponse out;
  char *pmsg_buf;
#ifdef USE_REG_TIMING
  double time0, time1;
#endif

  if(strlen(log_data) > REG_SCRATCH_BUFFER_SIZE){

    fprintf(stderr, "Save_log_wsrf: log data exceeds scratch buffer "
	    "size of %d btes.  More code needed here...\n", 
	    REG_SCRATCH_BUFFER_SIZE);
    return REG_FAILURE;
  }

  pmsg_buf = Global_scratch_buffer;

  /* We just send the data as it comes and therefore have to wrap it
     with CDATA tag to stop the parser getting upset */
  pmsg_buf += sprintf(pmsg_buf, "<Steer_log><Raw_param_log><![CDATA[");
  strcpy(pmsg_buf, log_data);
  pmsg_buf += strlen(log_data);
  pmsg_buf += sprintf(pmsg_buf, "]]></Raw_param_log></Steer_log>");

#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time0);
#endif
  if(soap_call_sws__PutParamLog(Steerer_connection.SGS_info.soap, 
				Steerer_connection.SGS_info.address, 
				"", Global_scratch_buffer,  
				&out)){
    fprintf(stderr, "Save_log_wsrf: soap call failed:\n");
    soap_print_fault(Steerer_connection.SGS_info.soap, stderr);
    return REG_FAILURE;
  }
#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time1);
  fprintf(stderr, "TIMING: soap_call_sws__PutParamLog "
	  "took %f seconds\n", (time1-time0));
#endif

  return REG_SUCCESS;
}

/*-------------------------------------------------------------------------*/

int Detach_from_steerer_wsrf()
{
  /* ARPDBG - I don't think we need this function anymore
  struct sws__DestroyResponse out;

  if(soap_call_sws__Destroy(Steerer_connection.SGS_info.soap, 
			    Steerer_connection.SGS_info.address, 
			    "", &out)){
    fprintf(stderr, "Detach_from_steerer_wsrf: Destroy call failed:\n");
    soap_print_fault(Steerer_connection.SGS_info.soap, stderr);
    return REG_FAILURE;
  }
  */
  return REG_SUCCESS;
}

/*-------------------------------------------------------------------------*/

int Finalize_steering_connection_wsrf ()
{
  int return_status = REG_SUCCESS;
  struct sws__DestroyResponse out;

  fprintf(stderr, "ARPDBG: Finalize_steering_connection_wsrf: calling "
	  "Destroy\n");
  if(soap_call_sws__Destroy(Steerer_connection.SGS_info.soap, 
			    Steerer_connection.SGS_info.address, 
			    "",  &out)){
    soap_print_fault(Steerer_connection.SGS_info.soap, stderr);
    return_status = REG_FAILURE;
  }

  soap_end(Steerer_connection.SGS_info.soap);
  /* Reset: close master/slave sockets and remove callbacks */
  soap_done(Steerer_connection.SGS_info.soap);

  free(Steerer_connection.SGS_info.soap);
  Steerer_connection.SGS_info.soap = NULL;

  return return_status;
}

/*-------------------------------------------------------------------------*/

int Get_data_source_address_wsrf(int   index, 
				 char *hostname,
				 unsigned short int  *port)
{
  fprintf(stderr, "IMPLEMENT ME - ARPDBG!\n");
  return REG_FAILURE;
}

/*----------------------------------------------------------------------*/

int Record_checkpoint_set_wsrf(char *chk_data,
			       char *node_data)
{
  struct sws__RecordCheckpointResponse response;

  response._RecordCheckpointReturn = NULL;
  if(soap_call_sws__RecordCheckpoint(Steerer_connection.SGS_info.soap, 
				      Steerer_connection.SGS_info.address, 
				      "", chk_data, node_data,  
				      &response)){
    fprintf(stderr, "Record_checkpoint_set_wsrf: soap call failed:\n");
    soap_print_fault(Steerer_connection.SGS_info.soap, stderr);
    return REG_FAILURE;
  }

#if REG_DEBUG
  if(response._RecordCheckpointReturn){
    fprintf(stderr, "Record_checkpoint_set_wsrf: "
	    "RecordCheckpoint returned: >>%s<<\n", 
	    response._RecordCheckpointReturn);
  }
  else{
    fprintf(stderr, "Record_checkpoint_set_wsrf: RecordCheckpoint "
	    "returned null\n");
  }
#endif

  if(response._RecordCheckpointReturn && 
     strstr(response._RecordCheckpointReturn, REG_SGS_SUCCESS)){

    return REG_SUCCESS;
  }

  return REG_FAILURE;
}
