/*----------------------------------------------------------------------------
  This file contains routines and data structures for SOAP-based 
  steering communication.

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

/** @file ReG_Steer_Browser.c
    @brief Source file for routines to do registry look-up
  */

#include "ReG_Steer_types.h"
#include "ReG_Steer_Browser.h"
#include "ReG_Steer_Common.h"
#include "ReG_Steer_XML.h"
#include "ReG_Steer_Utils_WSRF.h"
#include "soapH.h"

#if REG_OGSI
#define REG_TOP_LEVEL_REGISTRY "http://yaffel.mvc.mcc.ac.uk:50000/Session/ServiceGroupRegistration/service?3893432997"
#else
#define REG_TOP_LEVEL_REGISTRY "http://calculon.cs.man.ac.uk:50005/Session/myServiceGroup/myServiceGroup/51449871051219079298"
#endif

/*----------------------------------------------------------------*/

int Get_sim_list(int   *nSims,
		 char **simName,
		 char **simGSH)
{
  char *ptr;
  int   count;
  /*int   nbytes;*/
  int   status;
  int   i;
  char  registry_address[256];
  struct registry_entry *entries;

  *nSims = 0;

  /* Routine to get list of available steerable applications.
     Assumes that simName and simGSH are arrays of 
     REG_MAX_NUM_STEERED_SIM pointers to char arrays of length
     REG_MAX_STRING_LENGTH. */

  /*if(Proxy.available != REG_TRUE){*/

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


	if((strlen(entries[i].application) > 0) && 
	   (!strcmp(entries[i].service_type, "SWS") || 
	   !strcmp(entries[i].service_type, "SGS")) ){

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
  /*} End of if Proxy.available */

  /* Get (space-delimited) list of steerable apps & associated
     grid-service handles *

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
  */
  return REG_SUCCESS;
}

/*-------------------------------------------------------------------------*/

int Get_registry_entries(const char             *registryGSH, 
			 int                    *num_entries,  
			 struct registry_entry **entries){

  return Get_registry_entries_secure(registryGSH, "", "", "", 
				     num_entries, entries);
}

/*-------------------------------------------------------------------------*/

int Get_registry_entries_filtered_secure(const char             *registryGSH, 
					 const char             *userKeyPasswd,
					 const char             *userKeyCertPath,
					 const char             *caCertsPath,
					 int                    *num_entries,  
					 struct registry_entry **entries,
					 char                   *pattern){
  int status;
  int i, j;
  int count;

  if( (status = Get_registry_entries_secure(registryGSH, 
					    userKeyPasswd, 
					    userKeyCertPath,
					    caCertsPath,
					    num_entries,  
					    entries)) != REG_SUCCESS ){
    return status;
  }

  /* Job done if we have no valid string to match against */
  if(!pattern || (strlen(pattern) == 0)){

    return REG_SUCCESS;
  }

  j=0; /* j will index the filtered entries */
  count = *num_entries;

  for(i=0; i<*num_entries; i++){

    /* Does this entry match the supplied pattern? */
    if(strstr((*entries)[i].service_type, pattern) ||
       strstr((*entries)[i].application, pattern) ||
       strstr((*entries)[i].user, pattern) ||
       strstr((*entries)[i].group, pattern) ||
       strstr((*entries)[i].start_date_time, pattern) ||
       strstr((*entries)[i].job_description, pattern)){

      /* It does */
      if(j<i){
	strcpy((*entries)[j].service_type, (*entries)[i].service_type);
	strcpy((*entries)[j].gsh, (*entries)[i].gsh);
	strcpy((*entries)[j].entry_gsh, (*entries)[i].entry_gsh);
	strcpy((*entries)[j].application, (*entries)[i].application);
	strcpy((*entries)[j].user, (*entries)[i].user);
	strcpy((*entries)[j].group, (*entries)[i].group);
	strcpy((*entries)[j].start_date_time, (*entries)[i].start_date_time);
	strcpy((*entries)[j].job_description, (*entries)[i].job_description);
      }

      j++;
    }
    else{
      /* It didn't match the supplied pattern */
      count--;
    }
  }

  *num_entries = count;

#if REG_DEBUG
  fprintf(stderr,
	  "\nGet_registry_entries_filtered, got %d filtered entries...\n", 
	  *num_entries);

  for(i=0; i<*num_entries; i++){
    fprintf(stderr,"Entry %d:\n", i);
    fprintf(stderr,"          GSH: %s\n", (*entries)[i].gsh);
    fprintf(stderr,"          App: %s\n", (*entries)[i].application);
    fprintf(stderr,"         user: %s, %s\n", (*entries)[i].user, 
	    (*entries)[i].group);
    fprintf(stderr,"   Start time: %s\n", (*entries)[i].start_date_time);
    fprintf(stderr,"  Description: %s\n", (*entries)[i].job_description);
  }
#endif

  return REG_SUCCESS;
}

/*-------------------------------------------------------------------------*/

int Get_registry_entries_secure(const char *registryGSH, 
				const char *userKeyPasswd,
				const char *userKeyCertPath,
				const char *caCertsPath,
				int *num_entries,  
				struct registry_entry **entries){
  int status;

#if REG_OGSI

  fprintf(stderr, "Get_registry_entries_secure: WARNING: no secure version "
	  "available for OGSI implementation!\n");

  struct sgr__findServiceDataResponse out;
  struct soap soap;
  char   query_buf[256];

  soap_init(&soap);

  sprintf(query_buf, "<ogsi:queryByServiceDataNames names=\"ogsi:entry\"/>");

  if (soap_call_sgr__findServiceData ( &soap, registryGSH, "", query_buf, 
				       &out)){
    soap_print_fault(&soap,stderr);
    return REG_FAILURE;
  }
  else{
    if(!(out._findServiceDataReturn)){
      fprintf(stderr, "Get_registry_entries: findServiceData returned null\n");
      return REG_FAILURE;
    }
#if REG_DEBUG
    else{
      fprintf(stderr, "Get_registry_entries: findServiceData returned: %s\n", 
	      out._findServiceDataReturn);
    }
#endif
  }

  status = Parse_registry_entries(out._findServiceDataReturn, 
				  strlen(out._findServiceDataReturn),
				  num_entries, entries);
  soap_destroy(&soap);
  soap_end(&soap);
#else

  status = Get_registry_entries_wsrf(registryGSH, userKeyPasswd, 
				     userKeyCertPath, caCertsPath, 
				     num_entries, entries);

#if REG_DEBUG
  if(status == REG_SUCCESS){
    int i;
    fprintf(stderr,"Get_registry_entries_secure, got %d entries...\n", 
	    *num_entries);
    for(i=0; i<*num_entries; i++){
      fprintf(stderr,"Entry %d:\n", i);
      fprintf(stderr,"          GSH: %s\n", (*entries)[i].gsh);
      fprintf(stderr,"          App: %s\n", (*entries)[i].application);
      fprintf(stderr,"         user: %s, %s\n", (*entries)[i].user, 
	      (*entries)[i].group);
      fprintf(stderr,"   Start time: %s\n", (*entries)[i].start_date_time);
      fprintf(stderr,"  Description: %s\n", (*entries)[i].job_description);
    }
  }
#endif

  return status;
#endif /* REG_OGSI */
}

/*-------------------------------------------------------------------------*/

int Get_registry_entries_filtered(const char             *registryGSH, 
				  int                    *num_entries,  
				  struct registry_entry **entries,
				  char                   *pattern){

  return Get_registry_entries_filtered_secure(registryGSH,
					      "","","",
					      num_entries,
					      entries,
					      pattern);
}
