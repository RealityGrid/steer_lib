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
#include "ReG_Steer_types.h"
#include "ReG_Steer_Appside_internal.h"
#include "ReG_Steer_Appside_Soap.h"
#include "soapSGS.nsmap"

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

/* Soap-specific declarations */
static struct soap soap;

/* Names of the SGS' service data elements - MUST match those
   used in SGS.pm (as launched by container) */
char *SUPPORTED_CMDS_SDE = "SGS:Supp_cmds";
char *PARAM_DEFS_SDE     = "SGS:Param_defs";
char *IOTYPE_DEFS_SDE    = "SGS:IOType_defs";
char *CHKTYPE_DEFS_SDE   = "SGS:ChkType_defs";
char *STEER_STATUS_SDE   = "SGS:Steerer_status";
char *MACHINE_ADDRESS_SDE= "SGS:Machine_address";
char *WORKING_DIR_SDE    = "SGS:Working_directory";
char *APP_NAME_SDE       = "SGS:Application_name";

/*-------------------------------------------------------------------------*/

int Initialize_steering_connection_soap(int  NumSupportedCmds,
					int *SupportedCmds)
{
  struct tns__setServiceDataResponse setSDE_response;
  struct tns__AppStartResponse       appStart_response;
  char                              *pchar;
  char                               query_buf[REG_MAX_MSG_SIZE];

  /* Set location of steering scratch directory */
  if(Set_steering_directory() != REG_SUCCESS){

    fprintf(stderr, "Initialize_steering_connection_soap: failed to set "
	    "steering scratch directory - checkpoint info. will be "
	    "written to ./");;
  }

  /* Get the address of the SGS for this application from an environment
     variable */
  pchar = getenv("REG_SGS_ADDRESS");

  if(pchar){

    snprintf(Steerer_connection.SGS_address, REG_MAX_STRING_LENGTH, 
	     "%s", pchar);
#if REG_DEBUG
    fprintf(stderr, "Initialize_steering_connection_soap: SGS address = %s\n",
	    Steerer_connection.SGS_address);
#endif
  }
  else{

    fprintf(stderr, "Initialize_steering_connection_soap: REG_SGS_ADDRESS "
	    "environment variable not set\n");
    return REG_FAILURE;
  }

  /* Initialise the soap run-time environment */
  /* soap_init(&soap); */
  /* Use this form to turn-on keep-alive for both incoming and outgoing
     http connections */
  soap_init2(&soap, SOAP_IO_KEEPALIVE, SOAP_IO_KEEPALIVE);

  /* Since we are using KEEPALIVE, we can also ask gSOAP to bind the 
     socket to a specific port on the local machine - only do this if 
     GLOBUS_TCP_PORT_RANGE is set. */
  if( pchar = getenv("GLOBUS_TCP_PORT_RANGE") ){

    if(sscanf(pchar, "%d,%d", &(soap.client_port_min), 
	      &(soap.client_port_max)) != 2){
      soap.client_port_min = 0;
      soap.client_port_max = 0;
    }
  }

  appStart_response._result = NULL;
  if (soap_call_tns__AppStart(&soap, Steerer_connection.SGS_address, "", 
			      &appStart_response)){

    fprintf(stderr, "Initialize_steering_connection_soap: failed to attach to SGS\n");
    soap_print_fault(&soap, stderr);
    return REG_FAILURE;
  }

  if(!appStart_response._result || strstr(appStart_response._result, REG_SGS_ERROR)){

    fprintf(stderr, "Initialize_steering_connection_soap: AppStart returned error\n");
    return REG_FAILURE;
  }
  
  /* Create msg to send to SGS */
  Make_supp_cmds_msg(NumSupportedCmds, SupportedCmds, 
		     Steerer_connection.supp_cmds);

  setSDE_response._result = NULL;

  /* Strip off any xml version declaration */
  pchar = strstr(Steerer_connection.supp_cmds,"<ReG_steer_message");

  snprintf(query_buf, REG_MAX_MSG_SIZE, 
	   "<ogsi:setByServiceDataNames><%s>%s</%s></ogsi:setByServiceDataNames>", 
	  SUPPORTED_CMDS_SDE, pchar, SUPPORTED_CMDS_SDE);

  if (soap_call_tns__setServiceData(&soap, 
				    Steerer_connection.SGS_address, "", 
				    query_buf, 
				    &setSDE_response)){

    fprintf(stderr, "Initialize_steering_connection_soap: failure\n");
    soap_print_fault(&soap, stderr);
    return REG_FAILURE;
  }

#if REG_DEBUG
  if(setSDE_response._result){
    fprintf(stderr, "Initialize_steering_connection_soap: setServiceData "
	    "returned: %s\n", setSDE_response._result);
  }
  else{
    fprintf(stderr, "Initialize_steering_connection_soap: setServiceData "
	    "returned null\n");
  }
#endif

  /* Publish our location: machine and working directory - these
     are set in Steering_initialize prior to calling us */
  snprintf(query_buf, REG_MAX_MSG_SIZE, 
	   "<ogsi:setByServiceDataNames><%s>%s</%s>"
	   "<%s>%s</%s><%s>%s</%s></ogsi:setByServiceDataNames>", 
	   WORKING_DIR_SDE, ReG_CurrentDir, WORKING_DIR_SDE,
	   MACHINE_ADDRESS_SDE, ReG_Hostname, MACHINE_ADDRESS_SDE,
	   APP_NAME_SDE, ReG_AppName, APP_NAME_SDE);

  if (soap_call_tns__setServiceData(&soap, 
				    Steerer_connection.SGS_address, "", 
				    query_buf, 
				    &setSDE_response)){

    fprintf(stderr, "Initialize_steering_connection_soap: failure\n");
    soap_print_fault(&soap, stderr);
    return REG_FAILURE;
  }

#if REG_DEBUG
  if(setSDE_response._result){
    fprintf(stderr, "Initialize_steering_connection_soap: setServiceData "
	    "returned: %s\n", setSDE_response._result);
  }
  else{
    fprintf(stderr, "Initialize_steering_connection_soap: setServiceData "
	    "returned null\n");
  }
#endif

  return REG_SUCCESS;
}

/*-------------------------------------------------------------------------*/

int Detach_from_steerer_soap()
{
  struct tns__AppDetachResponse appDetach_response;

  appDetach_response._result = NULL;
  if(soap_call_tns__AppDetach(&soap, Steerer_connection.SGS_address, 
			      "", &appDetach_response )){

    fprintf(stderr, "Detach_from_steerer_soap: AppDetach failed:\n");
    soap_print_fault(&soap, stderr);

    return REG_FAILURE;
  }

  if(appDetach_response._result && 
     strstr(appDetach_response._result, "SGS_SUCCESS")){

    return REG_SUCCESS;
  }

  return REG_FAILURE;
}

/*-------------------------------------------------------------------------*/

int Steerer_connected_soap()
{
  struct tns__findServiceDataResponse  findServiceData_response;
  char                                 query_buf[REG_MAX_STRING_LENGTH];

  findServiceData_response._result = NULL;
  snprintf(query_buf, REG_MAX_STRING_LENGTH, 
	   "<ogsi:queryByServiceDataNames names=\"%s\"/>", 
	   STEER_STATUS_SDE );
  if(soap_call_tns__findServiceData(&soap, Steerer_connection.SGS_address, 
				    "", query_buf, 
				    &findServiceData_response )){

    fprintf(stderr, "Steerer_connected_soap: findServiceData failed:\n");
    soap_print_fault(&soap, stderr);

    return REG_FAILURE;
  }

#if REG_DEBUG
  if(findServiceData_response._result){
    fprintf(stderr, "Steerer_connected_soap: findServiceData returned: %s\n", 
	    findServiceData_response._result);
  }
  else{
    fprintf(stderr, "Steerer_connected_soap: findServiceData returned null\n");
  }
#endif

  if(findServiceData_response._result && 
     strstr(findServiceData_response._result, "ATTACHED")){
    return REG_SUCCESS;
  }
  else if(findServiceData_response._result && 
	  strstr(findServiceData_response._result, "DETACHING")){
    /* Steerer has attached and detached without us noticing and thus SGS
       is in 'detaching' state.  We have to 'detach' properly now to reset.*/
    Detach_from_steerer_soap();

    /* But we still return 'failure' because no steerer is attached */
  }

  return REG_FAILURE;
}

/*-------------------------------------------------------------------------*/

int Send_status_msg_soap(char* msg)
{
  struct tns__PutStatusResponse       putStatus_response;
  struct tns__setServiceDataResponse  setSDE_response;
  char                               *sde_name;
  char                                query_buf[REG_MAX_MSG_SIZE];
  int                                 nbytes;
  int                                 new_size;
  int                                 status;
  char                               *pbuf;

  /* Status & log messages are both sent as 'status' messages */
  if(strstr(msg, "<App_status>") || strstr(msg, "<Steer_log>")){

    putStatus_response._result = NULL;
    if(soap_call_tns__PutStatus(&soap, Steerer_connection.SGS_address, 
				"", msg, &putStatus_response )){
      soap_print_fault(&soap, stderr);

      return REG_FAILURE;
    }

#if REG_DEBUG
    if(putStatus_response._result){
      fprintf(stderr, "Send_status_msg_soap: PutStatus returned: %s\n", 
	      putStatus_response._result);
    }
    else{
      fprintf(stderr, "Send_status_msg_soap: PutStatus returned null\n");
    }
#endif

    if(!putStatus_response._result ||
       strstr(putStatus_response._result, REG_SGS_ERROR)){
      return REG_FAILURE;
    }
  }
  else{

    if(strstr(msg, "<Supported_commands>")){
      
      sde_name = SUPPORTED_CMDS_SDE;
    }
    else if(strstr(msg, "<Param_defs>")){

      sde_name = PARAM_DEFS_SDE;
    }
    else if(strstr(msg, "<IOType_defs>")){

      sde_name = IOTYPE_DEFS_SDE;
    }
    else if(strstr(msg, "<ChkType_defs>")){

      sde_name = CHKTYPE_DEFS_SDE;
    }

    setSDE_response._result = NULL;
    nbytes = snprintf(query_buf, REG_MAX_MSG_SIZE, 
		      "<ogsi:setByServiceDataNames>" 
		      "<%s>%s</%s></ogsi:setByServiceDataNames>", 
		      sde_name, msg, sde_name);

    /* Check for truncation - if it occurs then malloc a bigger buffer
       and try again */
    if((nbytes >= (REG_MAX_MSG_SIZE-1)) || (nbytes < 1)){

      new_size = strlen(msg) + 512;
      if(!(pbuf = (char *)malloc(new_size)) ){

	fprintf(stderr, "Send_status_msg_soap: malloc failed\n");
	return REG_FAILURE;
      }

      nbytes = snprintf(pbuf, new_size, 
			"<ogsi:setByServiceDataNames>" 
			"<%s>%s</%s></ogsi:setByServiceDataNames>", 
			sde_name, msg, sde_name);

      if((nbytes >= (new_size-1)) || (nbytes < 1)){
      
	free(pbuf);
	pbuf = NULL;
	fprintf(stderr, "Send_status_msg_soap: ERROR - msg truncated\n");
	return REG_FAILURE;
      }
      status = soap_call_tns__setServiceData(&soap, 
					     Steerer_connection.SGS_address, 
					     "", pbuf,  
					     &setSDE_response);
      free(pbuf);
      pbuf = NULL;
    }
    else {
      status = soap_call_tns__setServiceData(&soap, 
					     Steerer_connection.SGS_address,
					     "", query_buf,  
					     &setSDE_response);
    }

    if(status){
      fprintf(stderr, "Send_status_msg_soap: setServiceData failed:\n");
      soap_print_fault(&soap,stderr);
      return REG_FAILURE;
    }

    if(!setSDE_response._result || 
       strstr(setSDE_response._result, REG_SGS_ERROR)){
      return REG_FAILURE;
    }
  }

  return REG_SUCCESS;
}

/*-------------------------------------------------------------------------*/

struct msg_struct *Get_control_msg_soap()
{
  struct tns__GetControlResponse getControl_response;
  struct msg_struct *msg = NULL;

#if REG_DEBUG
    fprintf(stderr, "Get_control_msg_soap: address = %s\n", 
	    Steerer_connection.SGS_address);
#endif
    getControl_response._result = NULL;
  if(soap_call_tns__GetControl(&soap, Steerer_connection.SGS_address, 
			       "",  &getControl_response)){
    soap_print_fault(&soap, stderr);
    return NULL;
  }

#if REG_DEBUG
  if(getControl_response._result){
    fprintf(stderr, "Get_control_msg_soap: GetControl returned: %s\n", 
	    getControl_response._result);
  }
  else{
    fprintf(stderr, "Get_control_msg_soap: GetControl returned null\n");
  }
#endif

  if(getControl_response._result && 
     !strstr(getControl_response._result, REG_SGS_ERROR)){
    msg = New_msg_struct();

    if(Parse_xml_buf(getControl_response._result, 
		     strlen(getControl_response._result), msg) != REG_SUCCESS){

      Delete_msg_struct(msg);
      msg = NULL;
    }
#if REG_LOG_STEERING
    else{
      Log_control_msg(getControl_response._result);
    }
#endif
  }

  return msg;
}

/*-------------------------------------------------------------------------*/

int Finalize_steering_connection_soap()
{
  struct tns__AppStopResponse appStop_response;

  /* Tell the SGS to die - could use Destroy here but that doesn't 
     provide any opportunites for clean-up */
  appStop_response._result = NULL;
  if(soap_call_tns__AppStop(&soap, Steerer_connection.SGS_address, 
			    "",  &appStop_response)){
    soap_print_fault(&soap, stderr);
    return REG_FAILURE;
  }

  if(appStop_response._result && 
     !strstr(appStop_response._result, REG_SGS_ERROR)){

    soap_end(&soap);
    return REG_SUCCESS;
  }

  soap_end(&soap);

  return REG_FAILURE;
}

/*-------------------------------------------------------------------------*/

int Get_data_source_address_soap(int   index, 
				 char *hostname,
				 unsigned short int  *port)
{
  char  *pchar;
  char   index_string[10];
  struct tns__GetNthDataSourceResponse getNthDataSource_response;

  /* Port returned as zero on failure */
  *port = 0;

  sprintf(index_string, "%d", index);
  getNthDataSource_response._result = NULL;
  if(soap_call_tns__GetNthDataSource(&soap, Steerer_connection.SGS_address, 
				     "", index_string,  
				     &getNthDataSource_response)){
    fprintf(stderr, "Get_data_source_address_soap: soap call failed:\n");
    soap_print_fault(&soap, stderr);
    return REG_FAILURE;
  }

#if REG_DEBUG
  if(getNthDataSource_response._result){
    fprintf(stderr, "Get_data_source_address_soap: GetNthDataSource "
	    "(for n=%d)\nreturned: >>%s<<\n", 
	    index, getNthDataSource_response._result);
  }
  else{
    fprintf(stderr, "Get_data_source_address_soap: GetNthDataSource "
	    "(for n=%d)\nreturned null\n", index);
  }
#endif

  if(getNthDataSource_response._result && 
     !strstr(getNthDataSource_response._result, REG_SGS_ERROR)){

    if(pchar = strtok(getNthDataSource_response._result, ":")){

      strcpy(hostname, pchar);
      if(pchar = strtok(NULL, ":")){

	*port = (unsigned short int)atoi(pchar);
      }
    }
  }

  /* So long as soap call did return something we return success - even
     if we didn't actually get a valid address.  This consistent with
     polling a GS for valid address - success is indicated by non-zero
     port no. */
  return REG_SUCCESS;
}

/*----------------------------------------------------------------------*/

int Log_control_msg(char *msg_txt)
{
  char      *pbuf, *pstart, *pstop;
  static int seq_num_index   = -1;
  int        len;
  int        nbytes;
  int        bytes_free;
  void      *pdum;

  /*fprintf(stderr, "Log_control_msg: got >>%s<<\n", msg_txt);*/

  /* If this is the first time we've been called then calculate
     the index of the Sequence No. in the table of parameters */
  if(seq_num_index == -1){
    seq_num_index = Param_index_from_handle(&(Params_table), 
					    REG_SEQ_NUM_HANDLE);
    if(seq_num_index == -1){
      fprintf(stderr, "Log_control_msg: failed to find "
	      "index of sequence no.\n");
      return REG_FAILURE;
    }
  }

  pbuf = Chk_log.pSteer_cmds_slot;
  bytes_free = Chk_log.steer_cmds_bytes - (int)(pbuf - Chk_log.pSteer_cmds);

  if(  !(pstart = strstr(msg_txt, "<Steer_control>")) ){

    fprintf(stderr, "Log_control_msg: failed to find <Steer_control>\n");
    pbuf[0] = '\0';
    return REG_FAILURE;
  }

  /* 15 = strlen("<Steer_control>") */
  pstart += 15;
  if(*pstart == '\n')pstart++;

  pstop   = strstr(msg_txt, "</Steer_control>");
  len     = (int)(pstop - pstart);

  nbytes = snprintf(pbuf, bytes_free, "<Log_entry>\n"
		  "<Seq_num>%s</Seq_num>\n"
		  "<Steer_log_entry>\n",
		  Params_table.param[seq_num_index].value);

  if(nbytes >= (REG_MAX_STRING_LENGTH-1) || (nbytes < 1)){

    if(!(pdum = realloc(Chk_log.pSteer_cmds, 2*Chk_log.steer_cmds_bytes))){

      fprintf(stderr, "Log_control_msg: failed to realloc log buffer\n");
      /* Terminate buffer at end of last complete entry */
      *(Chk_log.pSteer_cmds_slot) = '\0';
      return REG_FAILURE;
    }
    else{

      Chk_log.steer_cmds_bytes *= 2;
      bytes_free += Chk_log.steer_cmds_bytes;
      Chk_log.pSteer_cmds = (char *)pdum;
      pbuf = Chk_log.pSteer_cmds_slot;

      nbytes = snprintf(pbuf, bytes_free, "<Log_entry>\n"
			"<Seq_num>%s</Seq_num>\n"
			"<Steer_log_entry>\n",
			Params_table.param[seq_num_index].value);
    }
  }
  pbuf += nbytes;
  bytes_free -= nbytes;

  /* 30 = strlen("</SteerLogEntry>\n</Log_entry>\n") */
  if(bytes_free > (len + 30)){
    strncpy(pbuf, pstart, len);
    pbuf += len;

    pbuf += sprintf(pbuf, "</Steer_log_entry>\n"
		    "</Log_entry>\n");

    /* Point to next free space in buffer */
    Chk_log.pSteer_cmds_slot = pbuf;
    
    return REG_SUCCESS;
  }

  return REG_FAILURE;
}

/*----------------------------------------------------------------------*/

int Record_checkpoint_set_soap(char *chk_data,
			       char *node_data)
{
  struct tns__AppRecordChkpointResponse AppRecordChkpoint_response;

  AppRecordChkpoint_response._result = NULL;
  if(soap_call_tns__AppRecordChkpoint(&soap, 
					Steerer_connection.SGS_address, 
					"", chk_data, node_data,  
					&AppRecordChkpoint_response)){
    fprintf(stderr, "Record_checkpoint_set_soap: soap call failed:\n");
    soap_print_fault(&soap, stderr);
    return REG_FAILURE;
  }

#if REG_DEBUG
  if(AppRecordChkpoint_response._result){
    fprintf(stderr, "Record_checkpoint_set_soap: "
	    "AppRecordChkpoint returned: >>%s<<\n", 
	    AppRecordChkpoint_response._result);
  }
  else{
    fprintf(stderr, "Record_checkpoint_set_soap: AppRecordChkpoint "
	    "returned null\n");
  }
#endif

  if(AppRecordChkpoint_response._result && 
     strstr(AppRecordChkpoint_response._result, REG_SGS_SUCCESS)){

    return REG_SUCCESS;
  }

  return REG_FAILURE;
}
