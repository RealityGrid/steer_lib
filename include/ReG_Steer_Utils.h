/*--------------------------------------------------------------------------*/

/** @file ReG_Steer_Utils.h
    @brief Header file containing prototypes and datatype definitions for
    entities that are using in the utilities library (provides
    functionality related to the ReG steering framework).

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

  @author Andrew Porter
    
---------------------------------------------------------------------------*/

#ifndef __REG_STEER_UTILS_H__
#define __REG_STEER_UTILS_H__

#ifdef __cplusplus
  #define PREFIX "C"
#else
  #define PREFIX 
#endif

/** Holds details on the job associated with an SWS */
struct reg_job_details {
  int  lifetimeMinutes;
  char userName[REG_MAX_STRING_LENGTH];
  char group[REG_MAX_STRING_LENGTH];
  char software[REG_MAX_STRING_LENGTH];
  char purpose[REG_MAX_STRING_LENGTH];
  char inputFilename[REG_MAX_STRING_LENGTH];
  char checkpointAddress[REG_MAX_STRING_LENGTH];
  char passphrase[REG_MAX_STRING_LENGTH];
};

struct reg_security_info {
  char caCertsPath[REG_MAX_STRING_LENGTH];
  char myKeyCertFile[REG_MAX_STRING_LENGTH];
  char userDN[REG_MAX_STRING_LENGTH];
};

/** Creates either an SGS or SWS
    @param job Ptr to struct holding details on the job
    @param containerAddress Address of the container in which to create SWS
    @param registryAddress Address of registry with which to register SWS
    @param keyPassphrase Passphrase to user's (encrypted) SSL key
    @param keyAndCertFile Location of pem file containing the user's certificate and private key
    @param caCertsPath Location of directory containg CA certs, used when authenticating connection to Containern */
extern PREFIX char* Create_steering_service(const struct reg_job_details *job,
					    const char *containerAddress,
					    const char *registryAddress,
					    const char *keyPassphrase,
					    const char *keyAndCertFile,
					    const char *caCertsPath);

/** Destroy either an SGS or SWS */
extern PREFIX int Destroy_steering_service(char *address,
					   char *username,
					   char *passphrase);

/** Creates a new checkpoint tree and returns its GSH 
    @param factory The address of the factory to use
    @param metadata Text describing the experiment that the tree will record */
extern PREFIX char *Create_checkpoint_tree(const char *factory, 
					   const char *metadata);

/** Reads the specified RealityGrid security configuration file to get
    location of the PEM file containing BOTH the user's key and certificate
    and the path to the directory containing CA certificates.  Parses 
    user's certificate to get their DN.
    @param configFile Location of RealityGrid security config file
    @param sec Pointer to reg_security_info struct to populate */ 
extern PREFIX int Get_security_config(const char               *configFile,
				      struct reg_security_info *sec);

#endif

