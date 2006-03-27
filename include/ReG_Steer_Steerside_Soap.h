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
---------------------------------------------------------------------------*/
#ifndef __REG_STEER_STEERSIDE_SOAP_H__
#define __REG_STEER_STEERSIDE_SOAP_H__

/** @file ReG_Steer_Steerside_Soap.h
    @brief Header file for SOAP communications for the steering client.
    @author Andrew Porter
    @author Robert Haines
*/

/* These NEED TO MATCH the names given to the service data elements in
   SGS.pm */
#define REG_APP_STATUS_SDE "SGS:Application_status"
#define REG_STEER_STATUS_SDE "SGS:Steerer_status"

/*-------------------------------------------------------------------*/

/** @internal
    @param sim Pointer to entry in main Sim_table
    @param SimID GSH of the SGS representing the simulation
    
    Initialise soap-specific structures & attach to simulation via SOAP */
int Sim_attach_soap(Sim_entry_type *sim, char *SimID);

/** @internal 
    @param sim Pointer to entry in main Sim_table
    @param buf The control message to send

    Send the supplied control message to the simulation */
int Send_control_msg_soap(Sim_entry_type *sim, char* buf);

/** @internal
    @param sim Pointer to entry in main Sim_table

    Send a detach message to the simulation */
int Send_detach_msg_soap(Sim_entry_type *sim);

/** @internal
    @param sim Pointer to entry in main Sim_table
    
    Send a stop message to the simulation (wraps 
    Send_control_msg_soap() ) */
int Send_stop_msg_soap(Sim_entry_type *sim);

/** @internal
    @param sim Pointer to entry in main Sim_table

    Send a pause message to the simulation (wraps 
    Send_control_msg_soap()) */
int Send_pause_msg_soap(Sim_entry_type *sim);

/** @internal
    @param sim Pointer to entry in main Sim_table

    Send a resume message to the simulation (wraps 
    Send_control_msg_soap()) */
int Send_resume_msg_soap(Sim_entry_type *sim);

/** @internal
    @param sim Pointer to entry in main Sim_table

    Gets the next status message from the simulation */
struct msg_struct *Get_status_msg_soap(Sim_entry_type *sim);

/** @internal
    @param sim Pointer to entry in main Sim_table
    @param sde_name Name of the ServiceDataEntry to fetch

    Get the specified ServiceDataEntry from the SGS representing 
    the simulation */
struct msg_struct *Get_service_data(Sim_entry_type *sim, char *sde_name);

/** @internal
    @param sim Pointer to entry in main Sim_table
    @param chkGSH The Grid Service Handle of the node in a checkpoint
    tree from which to restart

    Send a restart message to the simulation (wraps Send_control_msg_soap()) */
int Send_restart_msg_soap(Sim_entry_type *sim, char *chkGSH);

/** @internal
    @param sim Pointer to entry in main Sim_table

    Clean-up soap-specific structures */
int Finalize_connection_soap(Sim_entry_type *sim);

/** @internal
    @param sim Pointer to entry in main Sim_table
    @param handle Handle of a parameter registered by the simulation

    Requests the log of values of the parameter with the specified handle */
int Get_param_log_soap(Sim_entry_type *sim, int handle);

#endif
