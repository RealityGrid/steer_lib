/*----------------------------------------------------------------------------
  (C) Copyright 2009, University of Manchester, United Kingdom,
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

#ifndef __REG_STEER_STEERING_TRANSPORT_FILES_H__
#define __REG_STEER_STEERING_TRANSPORT_FILES_H__

#include "ReG_Steer_Config.h"
#include "ReG_Steer_types.h"
#include "ReG_Steer_Steering_Transport_API.h"

/** @internal
    @param filename Root of filename for status message

    Generate a filename (for a status message) from supplied root and 
    internally-generated index */
int generate_status_filename(char* filename);

/** @internal
    @param index Index of attached simulation in main Sim_table
    @param filename On return, filename to use for sending next control
    message to the simulation
    
    Generate a filename for sending steering commands to the application.
*/
int generate_control_filename(int index, char* filename);

/** @internal
    @param index Index of simulation entry in Sim_table
    
    Read an applications supported commands from disk and store them
    in the entry referenced by @p index. */
int consume_supp_cmds(int index);

#endif /* __REG_STEER_STEERING_TRANSPORT_FILES_H__ */
