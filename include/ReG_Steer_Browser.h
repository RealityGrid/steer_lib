/*----------------------------------------------------------------------------
  This header file contains routines and data structures for
  steerside SOAP-based communication.

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

  Authors........: Andrew Porter, Robert Haines

---------------------------------------------------------------------------*/
#ifndef __REG_STEER_BROWSER_H__
#define __REG_STEER_BROWSER_H__

/** @file ReG_Steer_Browser.h
    @brief Header file for registry browsing functionality */

#ifdef __cplusplus
  #define PREFIX "C"
#else
  #define PREFIX
#endif

/**
  Holds details on a entry in the registry 
*/
struct registry_entry {

  char service_type[REG_MAX_STRING_LENGTH];
  char gsh[REG_MAX_STRING_LENGTH];
  char application[REG_MAX_STRING_LENGTH];
  char start_date_time[REG_MAX_STRING_LENGTH];
  char user[REG_MAX_STRING_LENGTH];
  char group[REG_MAX_STRING_LENGTH];
  char job_description[REG_MAX_STRING_LENGTH];

};

/*-------------------------------------------------------------------*/

/** Queries specified registry
    @return The number of entries in the registry
    @return Details for each entry
 */
extern PREFIX int Get_registry_entries(char *registryGSH, 
			               int *num_entries,  
			               struct registry_entry **entries);

#endif
