/*----------------------------------------------------------------------------
  This file contains utility routines and data structures for the utilities
  library associated with the ReG computational steering framework.

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

/** @file ReG_Steer_Utils.c
    @brief Source file for utilities
  */

#include "ReG_Steer_types.h"
#include "ReG_Steer_Utils.h"
#include "ReG_Steer_Utils_WSRF.h"

#ifndef WIN32
#include <time.h>
#include <sys/time.h>
#endif /* not WIN32 */

#include "soapH.h"

/*----------------------------------------------------------------*/

char* Create_steering_service(const struct job_details *job,
			      const char *containerAddress,
			      const char *registryAddress,
			      const char *keyPassphrase,
			      const char *keyAndCertFile,
			      const char *caCertsPath)
{
#ifdef REG_WSRF
  return Create_SWS(job,
		    containerAddress,
		    registryAddress,
		    keyPassphrase,
		    keyAndCertFile,
		    caCertsPath);
#else
  fprintf(stderr, "Create_steering_service: NOT IMPLEMENTED for OGSI\n");
  return NULL;
#endif /* REG_WSRF */
}

/*----------------------------------------------------------------*/

char *Create_checkpoint_tree(const char *factory, 
			     const char *metadata)
{
  struct rgtf__createNewTreeResponse out;
  char                              *pchar;
  char                              *pend;
  static char                        epr[256];
  struct soap                        soap;

  soap_init(&soap);
  /* Something to do with the XML type */
  soap.encodingStyle = NULL;

  if(soap_call_rgtf__createNewTree(&soap, 
				   factory, 
				   "", /* soap Action */
				   "<ogsi:terminationTime />", "", "", 
				   (char *)metadata, 
				   &out) != SOAP_OK){
    fprintf(stderr, "Create_checkpoint_tree: soap call failed:\n");
    soap_print_fault(&soap, stderr);
    soap_end(&soap); /* dealloc deserialized data */
    soap_done(&soap); /* cleanup and detach soap struct */
    return NULL;
  }

  if( !(pchar = strstr(out._createNewTreeReturn, "<ogsi:handle>")) ){
    fprintf(stderr, "Create_checkpoint_tree: failed to find "
	    "<ogsi:handle> in >>%s<< returned by createNewTree on %s\n", 
	    out._createNewTreeReturn, factory);
    soap_end(&soap); /* dealloc deserialized data */
    soap_done(&soap); /* cleanup and detach soap struct */
    return NULL;
  }

  pchar += 13; /* 13 = strlen("<ogsi:handle>") */
  pend = strchr(pchar, '<');
  strncpy(epr, pchar, (int)(pend-pchar));

  soap_end(&soap); /* dealloc deserialized data */
  soap_done(&soap); /* cleanup and detach soap struct */

  return epr;
}

/*----------------------------------------------------------------*/

int Destroy_steering_service(char *address,
			     char *username,
			     char *passphrase){

#ifdef REG_WSRF
  return Destroy_WSRP(address, username, passphrase);
#else
  fprintf(stderr, "Destroy_steering_service: not implemented for OGSI :-(\n");
  return REG_FAILURE;
#endif

}
