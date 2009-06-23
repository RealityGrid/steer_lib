/*----------------------------------------------------------------------------
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
---------------------------------------------------------------------------*/
#ifndef __REG_STEER_STEERSIDE_DIRECT_H__
#define __REG_STEER_STEERSIDE_DIRECT_H__

/** @file ReG_Steer_Steerside_Direct.h
    @brief Header file for direct tcp communications for the steering client.
    @author Robert Haines
*/

#ifndef REG_SOCKETS_ERROR
#define REG_SOCKETS_ERROR -1
#endif

/** @internal
    @param sim Pointer to entry in main Sim_table
    @param SimID GSH of the SGS representing the simulation
    
    Initialise soap-specific structures & attach to simulation via SOAP */
int Sim_attach_direct(Sim_entry_type *sim, char *SimID);

/** @internal 
    @param sim Pointer to entry in main Sim_table
    @param buf The control message to send

    Send the supplied control message to the simulation */
int Send_control_msg_direct(Sim_entry_type *sim, char* buf);

/** @internal
    @param sim Pointer to entry in main Sim_table

    Gets the next status message from the simulation */
struct msg_struct *Get_status_msg_direct(Sim_entry_type *sim, int);

/** @internal
    @param sim Pointer to entry in main Sim_table

    Clean-up soap-specific structures */
int Finalize_connection_direct(Sim_entry_type *sim);

/* int connect_steerer_direct(Direct_info_type*); */
/* int steerer_socket_init_direct(Direct_info_type*); */
/* int dns_lookup_direct(char*); */
/* int Consume_steerer_msg(Direct_info_type*, char*); */
/* int poll_msg_direct(int); */
/* void close_listener_direct(Direct_info_type*); */
/* void close_connector_direct(Direct_info_type*); */

#endif /* __REG_STEER_STEERSIDE_DIRECT_H__ */
