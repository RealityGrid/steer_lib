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

/** Return the current (GMT) date and time as a string in the format
    YYYY-MM-DDTHH:MM:SSZ suitable for inclusion in XML documents */
extern PREFIX char *Get_current_time_string();

/** Creates either an SGS or SWS */
extern PREFIX char* Create_steering_service(const int   lifetimeMinutes,
					    const char *containerAddress,
					    const char *registryAddress,
					    const char *userName,
					    const char *group,
					    const char *software,
					    const char *purpose,
					    const char *inputFilename,
					    const char *checkpointAddress);

/** Destroy either an SGS or SWS */
extern PREFIX int Destroy_steering_service(char *address);

/** Creates a new checkpoint tree and returns its GSH 
    @param factory The address of the factory to use
    @param metadata Text describing the experiment that the tree will record */
extern PREFIX char *Create_checkpoint_tree(const char *factory, 
					   const char *metadata);

#endif

