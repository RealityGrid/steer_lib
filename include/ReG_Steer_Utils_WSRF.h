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

/** @file ReG_Steer_Utils_WSRF.h
    @brief Header file for routines to do registry look-up
  */

/** Get the entries from a WSRF-based registry */
int Get_registry_entries_wsrf(char *registryEPR, int *num_entries,  
			      struct registry_entry **entries);

/** Create a Steering Web Service and return its address */
char *Create_SWS(int lifetimeMinutes,
		 char *containerAddress,
		 char *registryAddress,
		 char *userName,
		 char *group,
		 char *software,
		 char *purpose,
		 char *checkpointAddress);

/** Calls the Destroy method on the service at the supplied Endpoint.
    Note that an SWS is derived from a WSRP so this method applies
    to SWSs.*/
int Destroy_WSRP(char *epr);
