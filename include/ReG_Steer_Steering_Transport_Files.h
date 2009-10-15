/*
  The RealityGrid Steering Library

  Copyright (c) 2002-2009, University of Manchester, United Kingdom.
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

  Author: Robert Haines
 */

#ifndef __REG_STEER_STEERING_TRANSPORT_FILES_H__
#define __REG_STEER_STEERING_TRANSPORT_FILES_H__

/** @file ReG_Steer_Steering_Transport_Files.h 
 *  @brief File specific routines for the steering transport module.
 *
 *  @author Robert Haines
 */

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
