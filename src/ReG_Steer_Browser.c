/*----------------------------------------------------------------------------
  This file contains routines and data structures for SOAP-based 
  steering communication.

  (C) Copyright 2002, 2004, University of Manchester, United Kingdom,
  all rights reserved.

  This software is produced by the Supercomputing, Visualization and
  e-Science Group, Manchester Computing, University of Manchester
  as part of the RealityGrid project (http://www.realitygrid.org),
  funded by the EPSRC under grants GR/R67699/01 and GR/R67699/02.

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
#include "ReG_Steer_types.h"
#include "ReG_Steer_Browser.h"
#include "ReG_Steer_XML.h"
#include "soapH.h"

/*-------------------------------------------------------------------------*/

int Get_registry_entries(char *registryGSH, int *num_entries,  
			 struct registry_entry **entries){

  struct sgr__findServiceDataResponse out;
  struct soap soap;
  char   query_buf[256];
  int    status;

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

  soap_end(&soap);

  return status;
}
