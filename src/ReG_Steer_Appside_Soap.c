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
   ReG_Steer_Appside_internal.h */
extern IOdef_table_type IOTypes_table;

extern Steerer_connection_table_type Steerer_connection;

/* Soap-specific declarations */
struct soap soap;

/* Hardwire address of Steering Grid Service (SGS) for the mo' */
const char *SGS_address = "http://vermont.mvc.mcc.ac.uk:50005/";

/* Names of the SGS' service data elements */
char *SUPPORTED_CMDS_SDE = "Supp_cmds";
char *PARAM_DEFS_SDE     = "Param_defs";
char *IOTYPE_DEFS_SDE    = "IOType_defs";
char *CHKTYPE_DEFS_SDE   = "ChkType_defs";

/*-------------------------------------------------------------------------*/

int Initialize_steering_connection_soap(int  NumSupportedCmds,
					int *SupportedCmds)
{
  struct tns__SetServiceDataResponse setSDE_response;
  struct tns__AppAttachResponse      appAttach_response;

  soap_init(&soap);

  if (soap_call_tns__AppAttach(&soap, SGS_address, "", &appAttach_response)){

    fprintf(stderr, "Initialize_steering_connection_soap: failed to attach to SGS\n");
    soap_print_fault(&soap, stderr);
    return REG_FAILURE;
  }

  if(strstr(appAttach_response._result, "SGS_ERROR")){

    fprintf(stderr, "Initialize_steering_connection_soap: AppAttach returned error\n");
    return REG_FAILURE;
  }
  
  /* Create msg to send to SGS */
  Make_supp_cmds_msg(NumSupportedCmds, SupportedCmds, 
		     Steerer_connection.supp_cmds);

  if (soap_call_tns__SetServiceData(&soap, 
				    SGS_address, "", "Supp_cmds", 
				    Steerer_connection.supp_cmds, 
				    &setSDE_response)){

    fprintf(stderr, "Initialize_steering_connection_soap: failure\n");
    soap_print_fault(&soap, stderr);
    return REG_FAILURE;
  }

  fprintf(stderr, "Initialize_steering_connection_soap: SetServiceData "
	  "returned: %s\n", setSDE_response._result);

  return REG_SUCCESS;
}

/*-------------------------------------------------------------------------*/

int Detach_from_steerer_soap()
{
  struct tns__AppDetachResponse appDetach_response;

  if(soap_call_tns__AppDetach(&soap, SGS_address, 
			      "", &appDetach_response )){

    fprintf(stderr, "Steerer_connected_soap: FindServiceData failed:\n");
    soap_print_fault(&soap, stderr);

    return REG_FAILURE;
  }

  if(strstr(appDetach_response._result, "SGS_SUCCESS")){

    return REG_SUCCESS;
  } else {

    return REG_FAILURE;
  }
}

/*-------------------------------------------------------------------------*/

int Steerer_connected_soap()
{
  struct tns__FindServiceDataResponse  findServiceData_response;

  if(soap_call_tns__FindServiceData(&soap, SGS_address, 
				    "", "Steerer_status", 
				    &findServiceData_response )){

    fprintf(stderr, "Steerer_connected_soap: FindServiceData failed:\n");
    soap_print_fault(&soap, stderr);

    return REG_FAILURE;
  }

#if DEBUG
  fprintf(stderr, "Steerer_connected_soap: FindServiceData returned: %s\n", 
	  findServiceData_response._result);
#endif

  if(strstr(findServiceData_response._result, "ATTACHED")){
    return REG_SUCCESS;
  }
  else{

    return REG_FAILURE;
  }
}

/*-------------------------------------------------------------------------*/

int Send_status_msg_soap(char* msg)
{
  struct tns__PutStatusResponse       putStatus_response;
  struct tns__SetServiceDataResponse  setSDE_response;
  char *sde_name;

  /* Status & log messages are both sent as 'status' messages */
  if(strstr(msg, "<App_status>") || strstr(msg, "<Steer_log>")){

    if(soap_call_tns__PutStatus(&soap, SGS_address, 
				"", msg, &putStatus_response )){
      soap_print_fault(&soap, stderr);

      return REG_FAILURE;
    }

#if DEBUG
    fprintf(stderr, "Send_status_msg_soap: PutStatus returned: %s\n", 
	    putStatus_response._result);
#endif
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

    if(soap_call_tns__SetServiceData (&soap, SGS_address, "",
				      sde_name, msg,  &setSDE_response)){
      soap_print_fault(&soap,stderr);
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

#if DEBUG
    fprintf(stderr, "Get_control_msg_soap: address = %s\n", SGS_address);
#endif

  if(soap_call_tns__GetControl(&soap, SGS_address, 
			       "",  &getControl_response)){
    soap_print_fault(&soap, stderr);
    return NULL;
  }

#if DEBUG
  fprintf(stderr, "Get_control_msg_soap: GetControl returned: %s\n", 
	  getControl_response._result);
#endif

  if(getControl_response._result){
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
  /* Tell the SGS to die */
  if(soap_call_tns__Destroy(&soap, SGS_address, 
			    "",  NULL)){
    soap_print_fault(&soap, stderr);
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}
