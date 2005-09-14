/*----------------------------------------------------------------------------
  This file contains utility routines and data structures related to
  WSRF(SOAP)-based steering.

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

/** @file ReG_Steer_Utils_WSRF.c
    @brief Source file for routines to do registry look-up
  */

#include "ReG_Steer_types.h"
#include "ReG_Steer_Browser.h"
#include "ReG_Steer_Utils.h"
#include "ReG_Steer_Utils_WSRF.h"
#include "ReG_Steer_Common.h"
#include "ReG_Steer_XML.h"
#include "soapH.h"

/** Global scratch buffer - declared in ReG_Steer_Appside.c */
extern char Global_scratch_buffer[];

/*-------------------------------------------------------------------------*/

int Get_registry_entries_wsrf(char *registryEPR, int *num_entries,  
			      struct registry_entry **entries){

  struct wsrp__GetMultipleResourcePropertiesRequest in;
  struct soap soap;
  int    status = REG_SUCCESS;
  char  *out;
  int    i;
#ifdef USE_REG_TIMING
  double time0, time1;
#endif
  *num_entries = 0;

  soap_init(&soap);
  in.__size = 1;
  in.__ptr = (struct wsrp__ResourcePropertyStruct *)malloc(in.__size*
							  sizeof(struct wsrp__ResourcePropertyStruct));
  if(!in.__ptr){
    fprintf(stderr, "STEER: Get_registry_entries_wsrf: ERROR: "
	    "malloc failed\n");
    return REG_MEM_FAIL;
  }

  for(i=0; i<in.__size;i++){
    in.__ptr[i].ResourceProperty = (char *)malloc(16*sizeof(char));
    if(!(in.__ptr[i].ResourceProperty))return REG_MEM_FAIL;
  }
  /* Entries in a ServiceGroup are held in the 'Entry' ResourceProperty */
  sprintf(in.__ptr[0].ResourceProperty, "Entry");

#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time0);
#endif

  if(soap_call_wsrp__GetMultipleResourceProperties(&soap, 
						   registryEPR, 
						   "", in,
						   &out) != SOAP_OK){
    soap_print_fault(&soap, stderr);
    free(in.__ptr[0].ResourceProperty);
    soap_end(&soap);
    soap_done(&soap);
    return REG_FAILURE;
  }

  free(in.__ptr[0].ResourceProperty);
  free(in.__ptr);

#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time1);
  fprintf(stderr, "TIMING: soap_call_wsrp__GetMultipleResourceProperties "
	  "took %f seconds\n", (time1-time0));
#endif

#if REG_DEBUG
  printf("Get_resource_property for Entry: %s\n", out);
#endif
  if(strlen(out) > 0){
    status = Parse_registry_entries(out, 
				    strlen(out),
				    num_entries, entries);
  }

  soap_end(&soap);
  soap_done(&soap);

  return status;
}

/*-----------------------------------------------------------------*/

char *Create_SWS(const int   lifetimeMinutes,
		 const char *containerAddress,
		 const char *registryAddress,
		 const char *userName,
		 const char *group,
		 const char *software,
		 const char *purpose,
		 const char *inputFilename,
		 const char *checkpointAddress){

  struct swsf__createSWSResourceResponse     response;
  struct wsrp__SetResourcePropertiesResponse setRPresponse;
  char                                       factoryAddr[256];
  char                                       jobDescription[1024];
  static char                                epr[256];
  struct soap                                soap;
  char                                      *contents;
  char                                      *pchar;
  int                                        numBytes;

  snprintf(jobDescription, 1024, "<registryEntry>\n"
	   "<serviceType>SWS</serviceType>\n"
	   "<componentContent>\n"
	   "<componentStartDateTime>%s</componentStartDateTime>\n"
	   "<componentCreatorName>%s</componentCreatorName>\n"
	   "<componentCreatorGroup>%s</componentCreatorGroup>\n"
	   "<componentSoftwarePackage>%s</componentSoftwarePackage>\n"
	   "<componentTaskDescription>%s</componentTaskDescription>\n"
	   "</componentContent>"
	   "</registryEntry>",
	   Get_current_time_string(), userName, group, software, purpose);

  sprintf(factoryAddr, "%sSession/SWSFactory/SWSFactory", 
	  containerAddress);

#if REG_DEBUG
  fprintf(stderr, "Create_SWS: using factory >>%s<<\n",
	  factoryAddr);
#endif

  soap_init(&soap);
  /* Something to do with the XML type */
  soap.encodingStyle = NULL;

  /* 1440 = 24hrs in minutes.  Is the default lifetime of the service
     until its associated job starts up and then the TerminationTime
     is reset using the maxRunTime RP */
  if(soap_call_swsf__createSWSResource(&soap, factoryAddr, NULL, 
				       1440, 
				       (char *)registryAddress, 
				       (char *)jobDescription, 
				       (char *)checkpointAddress, 
				       &response) != SOAP_OK){
    if(soap.fault && soap.fault->detail){

      printf("Soap error detail any = %s\n", 
	     soap.fault->detail->__any);
    }
    soap_print_fault(&soap, stderr);
    return NULL;
  }

  strncpy(epr, response.wsa__EndpointReference.wsa__Address, 256);

  /* Finally, set it up with information on its
     maximum run-time */

  snprintf(jobDescription, 1024, "<maxRunTime>%d</maxRunTime>",
	   lifetimeMinutes);

#if REG_DEBUG
  fprintf(stderr, "Calling SetResourceProperties with >>%s<<\n",
	  jobDescription);
#endif
  if(soap_call_wsrp__SetResourceProperties(&soap, epr, "", jobDescription,
					   &setRPresponse) != SOAP_OK){
    soap_print_fault(&soap, stderr);
    return NULL;
  }

  /* If an input deck has been specified, grab it and store on the
     steering service */
  if(strlen(inputFilename) &&
     (Read_file(inputFilename, &contents, &numBytes, REG_TRUE) 
      == REG_SUCCESS) ){
    /* 49 = 12 + 37 = strlen("<![CDATA[]]>") + 
       2*strlen("<inputFileContent>") + 1 */
    if((strlen(contents) + 49)< REG_SCRATCH_BUFFER_SIZE){
      pchar = Global_scratch_buffer;
      /* Protect the file content from any XML parsing by wrapping
	 in CDATA tags */
      pchar += sprintf(pchar, "<inputFileContent><![CDATA[");
      pchar += sprintf(pchar, "%s", contents);
      sprintf(pchar, "]]></inputFileContent>");
    }
    else{
      
    }
#if REG_DEBUG
    fprintf(stderr, "Calling SetResourceProperties with >>%s<<\n",
	    Global_scratch_buffer);
#endif
    if(soap_call_wsrp__SetResourceProperties(&soap, epr, "", 
					     Global_scratch_buffer,
					     &setRPresponse) != SOAP_OK){
      soap_print_fault(&soap, stderr);
      return NULL;
    }
  }

  soap_end(&soap); /* dealloc deserialized data */
  soap_done(&soap); /* cleanup and detach soap struct */

  return epr;
}

/*-----------------------------------------------------------------*/

int Destroy_WSRP(char *epr)
{
  struct wsrp__DestroyResponse out;
  struct soap soap;
  int    return_status = REG_SUCCESS;
  if(epr){
    soap_init(&soap);
    /* Something to do with the XML type */
    soap.encodingStyle = NULL;

    if(soap_call_wsrp__Destroy(&soap, epr, NULL, &out) != SOAP_OK){
      fprintf(stderr, "Destroy_WSRP: call to Destroy on %s failed:\n   ",
	      epr);
      soap_print_fault(&soap, stderr);
      return_status = REG_FAILURE;
    }

    soap_end(&soap); /* dealloc deserialized data */
    soap_done(&soap); /* cleanup and detach soap struct */
  }
  return return_status;
}
