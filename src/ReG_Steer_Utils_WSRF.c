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
#include "ReG_Steer_Utils_WSRF.h"
#include "ReG_Steer_Common.h"
#include "ReG_Steer_XML.h"
#include "soapH.h"

/*-------------------------------------------------------------------------*/

int Get_registry_entries_wsrf(char *registryEPR, int *num_entries,  
			      struct registry_entry **entries){

  struct wsrp__GetMultipleResourcePropertiesRequest in;
  struct soap soap;
  int    status;
  char *out;
  int   i;
#ifdef USE_REG_TIMING
  double time0, time1;
#endif

  soap_init(&soap);
  in.__size = 1;
  in.__ptr = (struct wsrp__ResourcePropertyStruct *)malloc(in.__size*
							  sizeof(struct wsrp__ResourcePropertyStruct));
  if(!in.__ptr)return REG_MEM_FAIL;

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
    return REG_FAILURE;
  }

#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time1);
  fprintf(stderr, "TIMING: soap_call_wsrp__GetMultipleResourceProperties "
	  "took %f seconds\n", (time1-time0));
#endif


  printf("Get_resource_property for Entry: %s\n", out);


  status = Parse_registry_entries(out, 
				  strlen(out),
				  num_entries, entries);

  free(in.__ptr[0].ResourceProperty);

  soap_destroy(&soap);
  soap_end(&soap);

  return status;
}
