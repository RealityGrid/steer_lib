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
extern int Get_resource_property (struct soap *soapStruct,
				  char        *epr,
				  char        *name,
				  char       **pRP);
extern int Get_resource_property_doc(struct soap *soapStruct,
				     char        *epr,
				     char       **pDoc);
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

    if(sscanf(pchar, "%d,%d", &(Steerer_connection.SGS_info.soap->client_port_min), 
	      &(Steerer_connection.SGS_info.soap->client_port_max)) != 2){
      Steerer_connection.SGS_info.soap->client_port_min = 0;
      Steerer_connection.SGS_info.soap->client_port_max = 0;
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
    fprintf(stderr, "Initialize_steering_connection_wsrf: failed to set "
	    "supportedCommands ResourceProperty:\n");
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
    fprintf(stderr, "Initialize_steering_connection_wsrf: failed to set "
	    "machine address and working directory ResourceProperties:\n");
    soap_print_fault(Steerer_connection.SGS_info.soap, stderr);
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*-------------------------------------------------------------------------*/

int Steerer_connected_wsrf ()
{
  char *steer_status;
  int   status;
  
  status = Get_resource_property(Steerer_connection.SGS_info.soap,
				 Steerer_connection.SGS_info.address,
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

  return REG_FAILURE;
}

/*-------------------------------------------------------------------------*/

int Send_status_msg_wsrf (char *msg)
{
  char  *sde_name;
  char   query_buf[REG_MAX_MSG_SIZE];
  int    nbytes;
  int    new_size;
  int    loopCount;
  struct wsrp__SetResourcePropertiesResponse out;
  char  *pTmpBuf;
  char  *pbuf;

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
    fprintf(stderr, "STEER: Send_status_msg_wsrf: not a status or log msg and"
	    "no matching SDE name found either\n");
    return REG_FAILURE;
  }

  pbuf = NULL;
  pTmpBuf = query_buf;
  nbytes = snprintf(query_buf, REG_MAX_MSG_SIZE, "<%s>%s</%s>", 
		    sde_name, msg, sde_name);

  /* Check for truncation - if it occurs then malloc a bigger buffer
     and try again */
  if((nbytes >= (REG_MAX_MSG_SIZE-1)) || (nbytes < 1)){

    new_size = strlen(msg) + 512;
    if(!(pbuf = (char *)malloc(new_size)) ){

      fprintf(stderr, "STEER: Send_status_msg_wsrf: malloc failed\n");
      return REG_FAILURE;
    }

    nbytes = snprintf(pbuf, new_size, "<%s>%s</%s>", 
		      sde_name, msg, sde_name);

    if((nbytes >= (new_size-1)) || (nbytes < 1)){
      
      free(pbuf);
      pbuf = NULL;
      fprintf(stderr, "STEER: ERROR: Send_status_msg_wsrf: msg truncated\n");
      return REG_FAILURE;
    }
    pTmpBuf = pbuf;
  }

  /* We loop until we have a clear-cut success or failure - i.e.
     if we have a deadlock we fall back to here and try again */
  loopCount = -1;
  while(1 && (loopCount < 10)){

    loopCount++;
    if(soap_call_wsrp__SetResourceProperties(Steerer_connection.SGS_info.soap, 
					     Steerer_connection.SGS_info.address,
					     "", pTmpBuf, &out) != SOAP_OK){
      fprintf(stderr, "STEER: Send_status_msg_wsrf: call to SetResourceProperties "
	      "for >>%s<< failed:\n", pTmpBuf);
      soap_print_fault(Steerer_connection.SGS_info.soap, stderr);

      if(Steerer_connection.SGS_info.soap->fault && 
	 Steerer_connection.SGS_info.soap->fault->detail){
	printf("STEER: Send_status_msg_wsrf: Soap error detail any = %s\n", 
	       Steerer_connection.SGS_info.soap->fault->detail->__any);
	if(strstr(Steerer_connection.SGS_info.soap->fault->detail->__any,
		  "deadlock")){
	  /* If we timed-out because of a deadlock situation (possible in
	     coupled models) then try again */
	  fprintf(stderr, "STEER: Send_status_msg_wsrf: deadlock - RETRYING\n");
	  continue;
	}
      }

      return REG_FAILURE;
    }
    /* ARPDBG - maybe need to pass a fault back from SWS...
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
  char *pBuf, *pLast, *pStart;
  /*
  char *pLastBut1;
  int   i;
  */
  /* If we have a backlog of messages then return the next one 
     - we are only interested in control messages */
  while( (msg = Get_next_stored_msg(NULL)) ){
    if(msg->control){
      return msg;
    }
    Delete_msg_struct(&msg);
  }

  /* Get any new control messages */
  if(Get_resource_property(Steerer_connection.SGS_info.soap,
			   Steerer_connection.SGS_info.address,
			   "controlMsg", &pBuf) != REG_SUCCESS){
    msg = New_msg_struct();
    msg->msg_type = MSG_ERROR;
    return msg;
  }

  /* Parse and store the control messages in the order in which they
     occur in the RPDoc which is the same as the order in which they
     were received. */
  if( !(pStart = strstr(pBuf, "<sws:controlMsg")) ){
    /* No control message found */    
    return NULL;
  }

  while(pStart){

    pLast = strstr(pStart+1, "<sws:controlMsg");
    /* Parse the doc - pass NULLs in as this is appside so have no
       Sim_entry struct and results will be put in Msg_store struct */
    if(pLast){
      Parse_xml_buf(pStart, (int)(pLast-pStart), NULL, NULL);
    }
    else{
      /* Have reached last control message so length of this one
	 is just num. chars 'til end of buffer */
      Parse_xml_buf(pStart, strlen(pStart), NULL, NULL);
    }
    pStart = pLast;
  }

  /* I think this stores the messages in reverse order - I don't know
     why I went to the trouble of doing this!
  pLastBut1 = NULL;
  while(pLastBut1 != pBuf){

    if( !(pLast = strstr(pBuf, "<sws:controlMsg")) ){
      * No control message found *
      return NULL;
    }

    pLastBut1 = pLast;
    while(pLast){
      pLastBut1 = pLast;
      pLast = strstr(pLast+1, "<sws:controlMsg");
    }

#if REG_DEBUG_FULL
    fprintf(stderr, "STEER: Get_control_msg_wsrf parsing >>%s<<\n", pLastBut1);
#endif

    * Parse the doc - pass NULLs in as this is appside so have no
       Sim_entry struct and results will be put in Msg_store struct *
    Parse_xml_buf(pLastBut1, strlen(pLastBut1), NULL, NULL);

    *pLastBut1 = '\0';
  }
*/

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

int Finalize_steering_connection_wsrf ()
{
  int return_status = REG_SUCCESS;
  struct wsrp__DestroyResponse out;
  int  commands[1];

  commands[0] = REG_STR_DETACH;
  Emit_status(0,
	      0,   
	      NULL,
	      1,
	      commands);

  if(soap_call_wsrp__Destroy(Steerer_connection.SGS_info.soap, 
			    Steerer_connection.SGS_info.address, 
			    "",  NULL, &out)){
    fprintf(stderr, "Finalize_steering_connection_wsrf: call to Destroy"
	    " failed:\n");
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
  char  *pBuf;
  char  *pchar;
  char  *pLast;
  char  *pIOType;
  char  *pLabel;
  int    count;
  char   epr[REG_MAX_STRING_LENGTH];
  char   label[REG_MAX_STRING_LENGTH];
  char   address[REG_MAX_STRING_LENGTH];
  struct soap mySoap;

  /* Port returned as zero on failure */
  *port = 0;

  if(Get_resource_property(Steerer_connection.SGS_info.soap,
			   Steerer_connection.SGS_info.address,
			   "dataSource", &pBuf) != REG_SUCCESS){
    return REG_FAILURE;
  }

  epr[0]='\0';
  label[0]='\0';
  count = 1;
  pLast = pBuf;
  if(strstr(pLast, "<sourceEPR>")){
    while( (pLast = strstr(pLast, "<sws:dataSource")) ){
      if(count == index){
	/* Pull out the EPR of the service that will provide
	   our data */
	pchar = strstr(pLast, "<sourceEPR>");
	pLabel = strstr(pLast, "<sourceLabel>");

	pchar += strlen("<sourceEPR>");
	pLast = strchr(pchar, '<');
	count = pLast - pchar;
	strncpy(epr, pchar, count);
	epr[count]='\0';

	/* Pull out the label of the IOType of that SWS that
	   will provide our data */
	pchar = strstr(pLabel, "<sourceLabel>");
	pchar += strlen("<sourceLabel>");
	pLast = strchr(pchar, '<');
	count = pLast - pchar;
	strncpy(label, pchar, count);
	label[count]='\0';
	break;
      }
      pLast++;
    }

    if(strlen(epr)==0 || strlen(label)==0){
      return REG_FAILURE;
    }

#if REG_DEBUG
    fprintf(stderr, "Get_data_source_address_wsrf: Got EPR = %s\n"
	    "                                label = %s\n",
	    epr, label);
#endif

    /* Set-up soap environment for call to a SWS other than our own */
    /* Initialise the soap run-time environment:
       Use this form to turn-on keep-alive for both incoming and outgoing
       http connections */
    soap_init2(&mySoap, SOAP_IO_KEEPALIVE, 
	       SOAP_IO_KEEPALIVE);
    /* Set the endpoint address */
    snprintf(address, REG_MAX_STRING_LENGTH, 
	     "%s", epr);
    /* Since we are using KEEPALIVE, we can also ask gSOAP to bind the 
       socket to a specific port on the local machine - only do this if 
       GLOBUS_TCP_PORT_RANGE is set. */
    mySoap.client_port_min = Steerer_connection.SGS_info.soap->client_port_min;
    mySoap.client_port_max = Steerer_connection.SGS_info.soap->client_port_max;

    if(Get_resource_property(&mySoap, address,
			     "ioTypeDefinitions", &pBuf) != REG_SUCCESS){
      return REG_FAILURE;
    }
    printf("ARPDBG, pBuf contains >>%s<<\n", pBuf);

    /* Parse the IOtypes */

    pIOType = pBuf;
    while( (pIOType = strstr(pIOType, "<IOType>")) ){
      /* According to schema, Label must occur before Address */
      pLast = strstr(pIOType, "<Label>");
      pLast += 7; /* strlen("<Label>") = 7 */
      pchar = strstr(pLast, "</Label>");
      *pchar = '\0';
      if(!strncmp(pLast, label, count)){
	/* This is the one we want */
	*pchar = '<';
	pLast = strstr(pIOType, "<Address>");
	pLast += 9; /* strlen("<Address>") = 9 */
	pchar = strstr(pLast, "</Address>");
	count = pchar - pLast;
	strncpy(label, pLast, count);
	label[count]='\0';
	break;
      }
      *pchar = '<';
      pIOType++;
      printf("ARPDBG, Label didn't match, looping...\n");
    }

    soap_end(&mySoap);
    /* Reset: close master/slave sockets and remove callbacks */
    soap_done(&mySoap);

    /* Parse the Address field to pull out port and host */
    if( (pchar = strchr(label, ':')) ){
      strncpy(hostname, label, (pchar - label));
      hostname[(pchar - label)] = '\0';
      pchar++;
      *port = (unsigned short int)atoi(pchar);
#if REG_DEBUG
      fprintf(stderr, "Get_data_source_address_wsrf: host = %s\n"
	      "                              port = %d\n",
	      hostname, *port);
#endif
      return REG_SUCCESS;
    }

    fprintf(stderr, "Get_data_source_address_wsrf: failed to match "
	    "IOType label\n");  
  }
  else{ /* Using a proxy for IO */

    while( (pLast = strstr(pLast, "<sws:dataSource")) ){
      if(count == index){
	/* Pull out the hostname and port of the proxy that will
	   provide our data */
	pchar = strstr(pLast, "<sourceProxy>");
	pchar = strstr(++pchar, "<address>");
	pchar += 9; /* = strlen("<address>") */
	pLast = strchr(pchar, '<');
	count = pLast - pchar;
	strncpy(hostname, pchar, count);
	hostname[count]='\0';
	pchar = strstr(pLast, "<port>");
	pchar += 6; /* = strlen("<port>") */
	pLast = strchr(pchar, '<');
	count = pLast - pchar;
	/* epr used as temp buffer here */
	strncpy(epr, pchar, count);
	epr[count]='\0';
	*port = (unsigned short int)atoi(epr);
	/* Pull out the label used to identify our data by
	   the proxy */
	pchar = strstr(pLast, "<sourceLabel>");
	pchar += 13; /* = strlen("<sourceLabel>") */
	pLast = strchr(pchar, '<');
	count = pLast - pchar;
	strncpy(label, pchar, count);
	label[count]='\0';
	break;
      }
      pLast++;
    }
#if REG_DEBUG
    fprintf(stderr, "Get_data_source_address_wsrf: proxy host = %s\n"
	            "                              proxy port = %d\n"
 	            "                                   label = %s\n",
	    hostname, *port, label);
#endif /* REG_DEBUG */
  } /* end if(strstr("sourceEPR")) */

  /* So long as soap call did return something we return success - even
     if we didn't actually get a valid address.  This consistent with
     polling a GS for valid address - success is indicated by non-zero
     port no. */
  return REG_SUCCESS;
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
