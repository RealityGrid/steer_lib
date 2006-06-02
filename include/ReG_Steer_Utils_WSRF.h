/*----------------------------------------------------------------------------
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

---------------------------------------------------------------------------*/

#ifndef __REG_STEER_UTILS_WSRF_H__
#define __REG_STEER_UTILS_WSRF_H__

#include <ReG_Steer_Browser.h>
#include "soapH.h"

/** @internal
    @file ReG_Steer_Utils_WSRF.h
    @brief Header file for WSRF-specific utility routines

    This file contains prototypes for utility routines and data
    structures related to WSRF(SOAP)-based steering.
    @author Andrew Porter  */

/** @internal
    Get the entries from a WSRF-based registry
    @param registryEPR Endpoint of the registry to query
    @param sec Pointer to struct holding authentication information
    @param num_entries On successful return, holds the number of 
    entries in the registry
    @param entries Array of structs holding details on each entry */
int Get_registry_entries_wsrf(const char             *registryEPR, 
			      const struct reg_security_info *sec,
			      struct registry_contents *contents);

/** @internal
    Create a Steering Web Service and return its address 
    @param job Pointer to struct containing info on job associated with SWS
    @param containerAddress Address of the WSRF-Lite container to use
    @param registryAddress Endpoint of the registery to register SWS with
    @param sec Pointer to struct containing authentication information
    @returns Pointer to static buffer containing the EPR of the new 
    SWS or NULL on failure.  Static buffer will be overwritten on 
    subsequent calls to this routine. */
char *Create_SWS(const struct reg_job_details   *job,
		 const char                     *containerAddress,
		 const char                     *registryAddress,
		 const struct reg_security_info *sec);

/** @internal
    Calls the Destroy method on the service at the supplied Endpoint.
    Note that an SWS is derived from a WSRP so this method applies
    to SWSs.
    @param epr Endpoint reference of the service to destroy
    @param sec Pointer to struct holding WSSE username & password 
    (if any) and necessary details for SSL (if being used) */
int Destroy_WSRP(const char *epr, 
		 const struct reg_security_info *sec);

/** @internal
    Queries the specified SWS and returns a list of its IOTypes
    @param address Endpoint of the SWS to query
    @param sec Pointer to struct holding WSSE username & password
    @param list Pointer to struct holding list of IOType definitions */
int Get_IOTypes_WSRF(const char                     *address,
		     const struct reg_security_info *sec,
		     struct reg_iotype_list         *list);

/** internal
    Calls the createNewTree method on the specified factory service
    to create a new CheckPointTree with the metadata specified in
    metadata. Returned pointer is to a static string which will remain
    valid until this routine is called again.
    @param factory Endpoint of the checkpoint factory to call
    @param metadata Label to give new checkpoint tree
    @returns Pointer to string containing endpoint of new checkpoint 
    tree or NULL on failure */
char *Create_checkpoint_tree_wsrf(const char *factory, 
				  const char *metadata);

#endif /* !defined __REG_STEER_UTILS_WSRF_H__ */
