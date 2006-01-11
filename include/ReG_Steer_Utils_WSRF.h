/*----------------------------------------------------------------------------
  This file contains prototypes for utility routines and data
  structures related to WSRF(SOAP)-based steering.

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
#include <ReG_Steer_Browser.h>
#include <ReG_Steer_Utils.h>
#include "soapH.h"

/** @file ReG_Steer_Utils_WSRF.h
    @brief Header file for routines to do registry look-up
  */

/** Get the entries from a WSRF-based registry
    @param registryEPR Endpoint of the registry to query
    @param userKeyPasswd Passphrase for the user's key
    @param userKeyCertPath Path to the PEM file containing both the user's key and certificate
    @param caCertsPath Path to the directory containing CA certificates
    @param num_entries On successful return, holds the number of entries in the registry
    @param entries Array of structst holding details on each entry */
int Get_registry_entries_wsrf(const char             *registryEPR, 
			      const char             *userKeyPasswd,
			      const char             *userKeyCertPath,
			      const char             *caCertsPath,
			      int                    *num_entries,  
			      struct registry_entry **entries);

/** Create a Steering Web Service and return its address 
    @param lifetimeMinutes Lifetime of the SWS in minutes
    @param containerAddress Address of the WSRF-Lite container to use
    @param registryAddress Endpoint of the registery to register SWS with
    @param userName Identity of user creating SWS - recorded in metadata and used with WS-Security. If using SSL then must be DN of user.
    @param group Group to which the user belongs
    @param software The software being run for this job
    @param purpose The purpose of this job
    @param inputFilename Name of the input deck for this job
    @param checkpointAddress Endpoint of the checkpoint this job is starting from (blank if none)
    @param passphrase Passphrase for use with WSSE or blank if none 
    @param caCertsPath Path to directory containing CA certs (for SSL)
    @returns Pointer to static buffer containing the EPR of the new SWS or NULL on failure.  Static buffer will be overwritten on subsequent calls to this routine. */
char *Create_SWS(const struct job_details *job,
		 const char *containerAddress,
		 const char *registryAddress,
		 const char *keyPassphrase,
		 const char *keyAndCertFile,
		 const char *caCertsPath);

/** Calls the Destroy method on the service at the supplied Endpoint.
    Note that an SWS is derived from a WSRP so this method applies
    to SWSs.
    @param epr Endpoint reference of the service to destroy
    @param username   Username for use with WSSE
    @param passphrase Passphrase to the service (if any) */
int Destroy_WSRP(const char *epr, 
		 const char *username,
		 const char *passphrase);
