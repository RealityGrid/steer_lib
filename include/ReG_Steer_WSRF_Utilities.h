/*
  The RealityGrid Steering Library

  Copyright (c) 2002-2010, University of Manchester, United Kingdom.
  All rights reserved.

  This software is produced by Research Computing Services, University
  of Manchester as part of the RealityGrid project and associated
  follow on projects, funded by the EPSRC under grants GR/R67699/01,
  GR/R67699/02, GR/T27488/01, EP/C536452/1, EP/D500028/1,
  EP/F00561X/1.

  LICENCE TERMS

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

    * Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of The University of Manchester nor the names
      of its contributors may be used to endorse or promote products
      derived from this software without specific prior written
      permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
  COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.

  Author: Andrew Porter
          Robert Haines
 */

#ifndef __REG_STEER_WSRF_UTILITIES_H__
#define __REG_STEER_WSRF_UTILITIES_H__

#include "ReG_Steer_Config.h"
#include "ReG_Steer_Common.h"
#include "ReG_Steer_Browser.h"
#include "soapH.h"

/** @internal
    @file ReG_Steer_WSRF_Utilities.h
    @brief Header file for WSRF-specific utility routines

    This file contains prototypes for utility routines and data
    structures related to WSRF(SOAP)-based steering.
    @author Robert Haines */

/** @internal
    Holds details on a job associated with an SWS */
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

/* ------------------------------------------------------------- */

/** @internal
    Create a Steering Web Service and return its address
    @param job Pointer to struct containing info on job associated with SWS
    @param containerAddress Address of the WSRF-Lite container to use
    @param registryAddress Endpoint of the registery to register SWS with
    @param sec Pointer to struct containing authentication information
    @returns Pointer to static buffer containing the EPR of the new
    SWS or NULL on failure.  Static buffer will be overwritten on
    subsequent calls to this routine. */
char* Create_steering_service(const struct reg_job_details* job,
			      const char* containerAddress,
			      const char* registryAddress,
			      const struct reg_security_info* sec);

/**
    Destroy an SWS.
    @param address The address of the service to destroy
    @param sec Pointer to struct holding details for
    authentication to the service (using SSL or WSSE) */
int Destroy_steering_service(const char                     *address,
                             const struct reg_security_info *sec);

/** internal
    Calls the createNewTree method on the specified factory service
    to create a new CheckPointTree with the metadata specified in
    metadata. Returned pointer is to a static string which will remain
    valid until this routine is called again.
    @param factory Endpoint of the checkpoint factory to call
    @param metadata Label to give new checkpoint tree
    @returns Pointer to string containing endpoint of new checkpoint
    tree or NULL on failure */
char* Create_checkpoint_tree(const char *factory, const char *metadata);

/**
   @param address Endpoint of the SWS from which to get IOTypes
   @param sec     Pointer to struct containing data required to authenticate to the SWS
   @param list    Pointer to reg_iotype_list to fill with details on IOTypes.  Must be cleaned up once finished with by calling Delete_iotype_list()

   A utility to get the IOTypes published by an application.
*/
int Get_IOTypes(const char                     *address,
		const struct reg_security_info *sec,
		struct reg_iotype_list         *list);

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
int Set_service_data_source(const char *EPR,
			    const char *sourceAddress,
			    const int   sourcePort,
			    const char *label,
			    const struct reg_security_info *sec);

#endif /* __REG_STEER_WSRF_UTILITIES_H__ */
