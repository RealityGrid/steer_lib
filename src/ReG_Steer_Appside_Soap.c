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
#include "soapH.h"
#include "soapSGS.nsmap"

/* Need access to these tables which are actually declared in 
   ReG_Steer_Appside_internal.h and ReG_Steer_Appside.c */
extern IOdef_table_type IOTypes_table;

extern Steerer_connection_table_type Steerer_connection;

/* Soap-specific declarations */
struct soap soap;

/* Names of the SGS' service data elements - MUST match those
   used in SGS.pm (as launched by container) */
char *SUPPORTED_CMDS_SDE = "SGS:Supp_cmds";
char *PARAM_DEFS_SDE     = "SGS:Param_defs";
char *IOTYPE_DEFS_SDE    = "SGS:IOType_defs";
char *CHKTYPE_DEFS_SDE   = "SGS:ChkType_defs";
char *STEER_STATUS_SDE   = "SGS:Steerer_status";
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

  appStart_response._result = NULL;
  if (soap_call_tns__AppStart(&soap, Steerer_connection.SGS_address, "", 
			      &appStart_response)){

    fprintf(stderr, "Initialize_steering_connection_soap: failed to attach to SGS\n");
    soap_print_fault(&soap, stderr);
    return REG_FAILURE;
  }

  if(!appStart_response._result || strstr(appStart_response._result, "SGS_ERROR")){

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
  fprintf(stderr, "Initialize_steering_connection_soap: setServiceData "
	  "returned: %s\n", setSDE_response._result);
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
  fprintf(stderr, "Steerer_connected_soap: findServiceData returned: %s\n", 
	  findServiceData_response._result);
#endif

  if(findServiceData_response._result && 
     strstr(findServiceData_response._result, "ATTACHED")){
    return REG_SUCCESS;
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
    fprintf(stderr, "Send_status_msg_soap: PutStatus returned: %s\n", 
	    putStatus_response._result);
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
  fprintf(stderr, "Get_control_msg_soap: GetControl returned: %s\n", 
	  getControl_response._result);
#endif

  if(getControl_response._result && 
     !strstr(getControl_response._result, REG_SGS_ERROR)){
    msg = New_msg_struct();

    if(Parse_xml_buf(getControl_response._result, 
		     strlen(getControl_response._result), msg) != REG_SUCCESS){

      Delete_msg_struct(msg);
      msg = NULL;
    }
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
  fprintf(stderr, "Get_data_source_address_soap: GetNthDataSource (for n=%d)\n"
	  "returned: >>%s<<\n", index, getNthDataSource_response._result);
#endif

  if(getNthDataSource_response._result && 
     !strstr(getNthDataSource_response._result, "SGS_ERROR")){

    if(pchar = strtok(getNthDataSource_response._result, ":")){

      strcpy(hostname, pchar);
      if(pchar = strtok(NULL, ":")){

	*port = (unsigned short int)atoi(pchar);
	return REG_SUCCESS;
      }
    }
  }

  return REG_FAILURE;
}
