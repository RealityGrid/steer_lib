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
#ifndef __REG_STEER_BROWSER_H__
#define __REG_STEER_BROWSER_H__

/** @file ReG_Steer_Browser.h
    @brief Header file for registry browsing functionality 

    This header file contains routines and data structures for
    steerside SOAP-based communication.

    @author Andrew Porter
    @author Robert Haines
*/

#include "ReG_Steer_Utils.h"

#ifdef __cplusplus
  #define PREFIX "C"
#else
  #define PREFIX 
#endif

/**
  Holds details on an entry in the registry 
*/
struct registry_entry {

  /** Index of the type of service (SWS, SGS, SGSFactory etc.)  in 
      string pointed to by @p pBuf
  char service_type[REG_MAX_STRING_LENGTH];*/
  char *service_type;
  /** Index of the endpoint of the service in string pointed to
      by @p pBuf
  char gsh[REG_MAX_STRING_LENGTH]; */
  char *gsh;
  /** Index of the endpoint of the service modelling the registry entry 
      in string pointed to by @p pBuf
  char entry_gsh[REG_MAX_STRING_LENGTH]; */
  char *entry_gsh;
  /** Index of the Name of the application in string pointed to
      by @p pBuf
  char application[REG_MAX_STRING_LENGTH];*/
  char *application;
  /** Index of the Date and time at which application started in string
      pointed to by @p pBuf
  char start_date_time[REG_MAX_STRING_LENGTH];*/
  char *start_date_time;
  /** Index of the User who lauched the application in string pointed to
      by @p pBuf
  char user[REG_MAX_STRING_LENGTH];*/
  char *user;
  /** Index of the group to which the user belongs in string pointed to
      by @p pBuf
  char group[REG_MAX_STRING_LENGTH];*/
  char *group;
  /** Index of description of purpose of job in string pointed to
      by @p pBuf
  char job_description[REG_MAX_STRING_LENGTH]; */
  char *job_description;
  /** Pointer to buffer containing data */
  char *pBuf;
  /** Length of the buffer pointed to by @p pBuf */
  int   bufLen;
  int   bufIndex;
};

/*-------------------------------------------------------------------*/

/**
   @param nSims No. of steerable applications available
   @param simName Pointer to array of strings, each of length 
   REG_MAX_STRING_LENGTH.  On successful return holds names of each 
   steerable application
   @param simGSH Pointer to array of strings, each of length 
   REG_MAX_STRING_LENGTH. On successful return holds Grid Service 
   Handles of each application
   @return REG_SUCCESS, REG_FAILURE

   @b OBSOLETE - superceded by Get_registry_entries() @n
   Really just wraps Get_registry_entries().  Uses the registry 
   address in REG_REGISTRY_ADDRESS if set, otherwise uses
   default (#define'd at the top of ReG_Steer_Browser.c).
   Returns list of steerable applications (up to 
   REG_MAX_NUM_STEERED_SIM). The Grid Service Handle returned 
   in @p simGSH must be supplied as the SimID to Sim_attach. */
extern PREFIX int Get_sim_list(int   *nSims,
			       char **simName,
			       char **simGSH);

/** Queries specified registry
    The pointer held in *entries must be free'd once the data
    has been used.
    @param registryGSH Address of the ServiceGroup/Registry to query
    @param num_entries Number of entries in the registry
    @param entries Array of structs holding details for each entry
 */
extern PREFIX int Get_registry_entries(const char *registryGSH, 
				       int *num_entries,  
				       struct registry_entry **entries);

/** Queries specified registry and filters results using the
    supplied string.
    An entry is matched if the supplied string appears in one or more
    of its service_type, application, user, group, start_date_time or
    job_description fields.
    @see Get_registry_entries
    @param pattern String holding pattern to be used in filtering
 */
extern PREFIX int Get_registry_entries_filtered(const char *registryGSH, 
						int *num_entries,  
						struct registry_entry **entries,
						char *pattern);

/** Queries the specified, secure registry using SSL
    The pointer held in *entries must be free'd once the data
    has been used.
    @param registryGSH Endpoint of the registry to query
    @param sec Pointer to reg_security_info struct holding information
    needed to authenticate user to the registry (using either ssl or wsse)
    @param num_entries The number of entries found
    @param entries Pointer to array of registry_entry structs containing
    details on the entries found.  Must be free'd by calling code.
*/
extern PREFIX int Get_registry_entries_secure(const char *registryGSH, 
					      const struct reg_security_info *sec,
					      int *num_entries,  
					      struct registry_entry **entries);

/** @see Get_registry_entries_filtered
    @see Get_registry_entries_secure */
extern PREFIX int Get_registry_entries_filtered_secure(const char             *registryGSH, 
						       const struct reg_security_info *sec,
						       int                    *num_entries,  
						       struct registry_entry **entries,
						       char                   *pattern);

#endif
