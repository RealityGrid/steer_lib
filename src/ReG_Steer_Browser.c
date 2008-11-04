/*----------------------------------------------------------------------------
  This file contains routines and data structures for SOAP-based 
  steering communication.

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

  Authors........: Andrew Porter

---------------------------------------------------------------------------*/

/** @file ReG_Steer_Browser.c
    @brief Source file for routines to do registry look-up
  */

#include "ReG_Steer_Config.h"
#include "ReG_Steer_types.h"
#include "ReG_Steer_Browser.h"
#include "ReG_Steer_Common.h"
#include "ReG_Steer_XML.h"
#include "ReG_Steer_Utils.h"
#include "soapH.h"

#ifdef REG_WSRF
#define REG_TOP_LEVEL_REGISTRY "http://calculon.cs.man.ac.uk:50005/Session/myServiceGroup/myServiceGroup/51449871051219079298"
#else
#define REG_TOP_LEVEL_REGISTRY "http://yaffel.mvc.mcc.ac.uk:50000/Session/ServiceGroupRegistration/service?3893432997"
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
  struct registry_contents contents;

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
    status = Get_registry_entries(registry_address, &contents);

    if(status == REG_SUCCESS){

      *nSims = contents.numEntries;

      /* Hard limit on no. of sims we can return details on */
      if(*nSims > REG_MAX_NUM_STEERED_SIM)*nSims = REG_MAX_NUM_STEERED_SIM;
      
      count = 0;
      for(i=0; i<*nSims; i++){


	if((strlen(contents.entries[i].application) > 0) && 
	   (!strcmp(contents.entries[i].service_type, "SWS") || 
	   !strcmp(contents.entries[i].service_type, "SGS")) ){

	  sprintf(simName[count], "%s %s %s", contents.entries[i].user, 
		  contents.entries[i].application, 
		  contents.entries[i].start_date_time);
	  strcpy(simGSH[count], contents.entries[i].gsh);
	  count++;
	}
      }
      *nSims = count;

      Delete_registry_table(&contents);
    }
    else {
      if( (ptr = getenv("REG_SGS_ADDRESS")) ){

	*nSims = 1;
	sprintf(simName[0], "Simulation");
	strcpy(simGSH[0], ptr);
      }
      else{

	fprintf(stderr, "STEERUtils: Get_sim_list: REG_SGS_ADDRESS environment variable "
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
#ifdef REG_DEBUG
    fprintf(stderr, "STEERUtils: Get_sim_list: no steerable apps available\n");
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

	fprintf(stderr, "STEERUtils: Get_sim_list: truncating list of steerable apps\n");
	break;
      }
    }
  }

  *nSims = count;
  */
  return REG_SUCCESS;
}

/*-------------------------------------------------------------------------*/

int Get_registry_entries(const char               *registryGSH, 
			 struct registry_contents *contents){

  struct reg_security_info sec;
  Wipe_security_info(&sec);
  return Get_registry_entries_secure(registryGSH, &sec, contents);
}

/*-------------------------------------------------------------------------*/

int Get_registry_entries_filtered_secure(const char             *registryGSH,
					 const struct reg_security_info *sec,
					 struct registry_contents *contents,
					 char                   *pattern){
  int status;
  int i, j;
  int count;
  int len;
  struct registry_entry *entry;
  char *pTmp;

  if( (status = Get_registry_entries_secure(registryGSH, 
					    sec,
					    contents)) != REG_SUCCESS ){
    return status;
  }

  /* Job done if we have no valid string to match against */
  if(!pattern || (strlen(pattern) == 0)){

    return REG_SUCCESS;
  }

  j=0; /* j will index the filtered entries */
  count = contents->numEntries;

  for(i=0; i<contents->numEntries; i++){

    entry = &(contents->entries[i]);

    /* Does this entry match the supplied pattern? */
    if((entry->service_type && strstr(entry->service_type, pattern)) ||
       (entry->application && strstr(entry->application, pattern)) ||
       (entry->user && strstr(entry->user, pattern)) ||
       (entry->group && strstr(entry->group, pattern)) ||
       (entry->start_date_time && strstr(entry->start_date_time, pattern)) ||
       (entry->job_description && strstr(entry->job_description, pattern))){

      /* It does */
      if(j<i){
	pTmp = contents->entries[j].pBuf;
	contents->entries[j].pBuf = entry->pBuf;
	entry->pBuf = pTmp;
	len = contents->entries[j].bufLen;
	contents->entries[j].bufLen = entry->bufLen;
	entry->bufLen = len;
	len = contents->entries[j].bufIndex;
	contents->entries[j].bufIndex = entry->bufIndex;
	entry->bufIndex = len;

        pTmp = contents->entries[j].service_type;
	contents->entries[j].service_type = entry->service_type;
	entry->service_type = pTmp;

        pTmp = contents->entries[j].gsh;
	contents->entries[j].gsh = entry->gsh;
	entry->gsh = pTmp;

        pTmp = contents->entries[j].entry_gsh;
	contents->entries[j].entry_gsh = entry->entry_gsh;
	entry->entry_gsh = pTmp;

        pTmp = contents->entries[j].application;
	contents->entries[j].application = entry->application;
	entry->application = pTmp;

        pTmp = contents->entries[j].user;
	contents->entries[j].user = entry->user;
	entry->user = pTmp;

        pTmp = contents->entries[j].group;
	contents->entries[j].group = entry->group;
	entry->group = pTmp;

        pTmp = contents->entries[j].start_date_time;
	contents->entries[j].start_date_time = entry->start_date_time;
	entry->start_date_time = pTmp;

        pTmp = contents->entries[j].job_description;
	contents->entries[j].job_description = entry->job_description;
	entry->job_description = pTmp;
      }

      j++;
    }
    else{
      /* It didn't match the supplied pattern */
      count--;
    }
  }

  /* Delete the unwanted entries */
  for(i=count; i<contents->numEntries; i++){
    free(contents->entries[i].pBuf);
    contents->entries[i].pBuf = NULL;
    contents->entries[i].bufLen = 0;
  }

  contents->numEntries = count;

#ifdef REG_DEBUG
  fprintf(stderr,
	  "\nSTEERUtils: Get_registry_entries_filtered_secure, got %d filtered "
	  "entries...\n", count);

  for(i=0; i<contents->numEntries; i++){
    fprintf(stderr,"Entry %d:\n", i);
    fprintf(stderr,"          GSH: %s\n", contents->entries[i].gsh);
    fprintf(stderr,"          App: %s\n", contents->entries[i].application);
    fprintf(stderr,"         user: %s, %s\n", contents->entries[i].user, 
	    contents->entries[i].group);
    fprintf(stderr,"   Start time: %s\n", contents->entries[i].start_date_time);
    fprintf(stderr,"  Description: %s\n", contents->entries[i].job_description);
  }
#endif

  return REG_SUCCESS;
}

/*-------------------------------------------------------------------------*/

int Get_registry_entries_secure(const char                     *registryGSH, 
				const struct reg_security_info *sec,
				struct registry_contents       *contents){
  int status;

#ifndef REG_WSRF

  fprintf(stderr, "STEERUtils: Get_registry_entries_secure: WARNING: no secure version "
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
      fprintf(stderr, "STEERUtils: Get_registry_entries_secure: findServiceData "
	      "returned null\n");
      return REG_FAILURE;
    }
#ifdef REG_DEBUG_FULL
    else{
      fprintf(stderr, "STEERUtils: Get_registry_entries_secure: findServiceData "
	      "returned: %s\n", out._findServiceDataReturn);
    }
#endif /* REG_DEBUG_FULL */
  }

  status = Parse_registry_entries(out._findServiceDataReturn, 
				  strlen(out._findServiceDataReturn),
				  contents);
  soap_destroy(&soap);
  soap_end(&soap);

#else /* WSRF, not OGSI */

  status = Get_registry_entries_wsrf(registryGSH, sec, contents);

#endif /* !defined REG_WSRF */

  return status;
}

/*-------------------------------------------------------------------------*/

int Get_registry_entries_filtered(const char               *registryGSH, 
				  struct registry_contents *contents,
				  char                     *pattern){
  struct reg_security_info sec;
  Wipe_security_info(&sec);

  return Get_registry_entries_filtered_secure(registryGSH,
					      &sec,
					      contents,
					      pattern);
}

/*------------------------------------------------------------------*/

int Delete_registry_table(struct registry_contents *contents)
{
  int i;

  if(!contents)return REG_FAILURE;

  for(i=0; i<contents->numEntries; i++){
    free(contents->entries[i].pBuf);
  }
  free(contents->entries);
  contents->entries = NULL;
  contents->numEntries = 0;

  return REG_SUCCESS;
}
