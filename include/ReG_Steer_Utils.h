/*--------------------------------------------------------------------------
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
-------------------------------------------------------------------------*/

/** @file ReG_Steer_Utils.h
    @brief Header file for the utilities library

    Contains prototypes and datatype definitions for
    entities that are using in the utilities library (provides
    functionality related to the ReG steering framework).

    @author Andrew Porter
*/

#ifndef __REG_STEER_UTILS_H__
#define __REG_STEER_UTILS_H__

#ifdef __cplusplus
  #define PREFIX "C"
#else
  #define PREFIX 
#endif

#include "ReG_Steer_types.h"
#include "ReG_Steer_Common.h"

/** Holds details on the job associated with an SWS */
struct reg_job_details {
  /** How long the SWS should live for once its associated job
      has contacted it for the first time */
  int  lifetimeMinutes;
  /** Name of the person creating this SWS - recorded in metadata
      and used with WS-Security. If using SSL then this
      must be the DN of the user */
  char userName[REG_MAX_STRING_LENGTH];
  /** The group that the person @p userName belongs to */
  char group[REG_MAX_STRING_LENGTH];
  /** The software that this job is using */
  char software[REG_MAX_STRING_LENGTH];
  /** The purpose of the job */
  char purpose[REG_MAX_STRING_LENGTH];
  /** The name of the main input file of the job (if any) */
  char inputFilename[REG_MAX_STRING_LENGTH];
  /** Address of the checkpoint node being used for a restart (if any) */
  char checkpointAddress[REG_MAX_STRING_LENGTH];
  /** The passphrase used to access the SWS (using WSSE) */
  char passphrase[REG_MAX_STRING_LENGTH];
};

/** Holds details on a single iotype */
struct iotype_detail {
  /** The label given to this IOType at registration */
  char label[REG_MAX_STRING_LENGTH];
  /** The direction of this IOType (REG_IN or REG_OUT) */
  int  direction;
};

/** Holds details on the IOTypes of a steerable app */
struct reg_iotype_list {
  /** Number of entries in this list */
  int numEntries;
  /** Array of entries */
  struct iotype_detail *iotype;
};

#include "ReG_Steer_Utils_WSRF.h"

/** 
    Creates either an SGS or SWS
    @param job Ptr to struct holding details on the job.  @p userName 
    and @p passphrase are used for the SWS, not the registry.
    @param containerAddress Address of the container in which to create SWS
    @param registryAddress Address of registry with which to register SWS
    @param sec Pointer to struct holding ssl authentication details and
    WSSE username and password (WSSE stuff for registry only) */
extern PREFIX char* Create_steering_service(const struct reg_job_details *job,
					    const char *containerAddress,
					    const char *registryAddress,
					    const struct reg_security_info *sec);

/** 
    Destroy either an SGS or SWS 
    @param address The address of the service to destroy 
    @param sec Pointer to struct holding details for 
    authentication to the service (using SSL or WSSE) */
extern PREFIX int Destroy_steering_service(const char                     *address,
					   const struct reg_security_info *sec);

/** 
    Creates a new checkpoint tree and returns its GSH 
    @param factory The address of the factory to use
    @param metadata Text describing the experiment that the tree will record */
extern PREFIX char *Create_checkpoint_tree(const char *factory, 
					   const char *metadata);

/** 
    @param configFile Location of RealityGrid security config file or NULL/emptry string to use default of ~/.realitygrid/security.conf.
    @param sec Pointer to reg_security_info struct to populate

    Reads the specified RealityGrid security configuration file to get
    location of the PEM file containing BOTH the user's key and certificate
    and the path to the directory containing CA certificates. Parses 
    user's certificate to get their DN.
    The security config. file is of the form: @n
    <tt>
    @verbatim
    <?xml version="1.0"?>
    <Security_config>
      <caCertsPath value="/etc/grid-security/certificates"/>
      <privateKeyCertFile value="/home/me/.globus/mycertnkey.pem"/>
    </Security_config>
    @endverbatim
    </tt>
 */ 
extern PREFIX int Get_security_config(const char               *configFile,
				      struct reg_security_info *sec);

/**
   @param address Endpoint of the SWS from which to get IOTypes
   @param sec     Pointer to struct containing data required to authenticate to the SWS
   @param list    Pointer to reg_iotype_list to fill with details on IOTypes.  Must be cleaned up once finished with by calling Delete_iotype_list()

   A utility to get the IOTypes published by an application.
*/
extern PREFIX int Get_IOTypes(const char                     *address,
			      const struct reg_security_info *sec,
			      struct reg_iotype_list         *list);

/**
   @param list Pointer to reg_iotype_list to clean up

   Frees memory associated with the reg_iotype_list and resets
   member variables.
*/
extern PREFIX int Delete_iotype_list(struct reg_iotype_list *list);

/**
   @param EPR EndPointReference (address) of the service to configure
   @param sourceAddress Address of service representing data source OR
address of IOProxy if @p sourcePort != 0
   @param sourcePort Port on which to connect to IOProxy or 0 if using
direct socket connection
   @param label Label of the IOType from which to get data
   @param sec Pointer to struct containing data required to authenticate to the SWS

   Configures the specified service with information on a data source 
   (either direct socket connection between source and sink or a socket 
   connection to an IOProxy).
*/
extern PREFIX int Set_service_data_source(const char *EPR, 
					  const char *sourceAddress, 
					  const int   sourcePort, 
					  const char *label,
					  const struct reg_security_info *sec);

#endif

