/*----------------------------------------------------------------------------
  Header file defining the internal (to the library) utility 
  routines used in the construction of a steering interface for 
  a steering component. Also contains the main table that stores
  information about each application being steered.

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
#ifndef __REG_STEER_STEERSIDE_INTERNAL_H__
#define __REG_STEER_STEERSIDE_INTERNAL_H__

/** @file ReG_Steer_Steerside_internal.h
    @brief Header for library-internal steering side routines */

#include "ReG_Steer_Common.h"
#include "ReG_Steer_XML.h"

/*---------------- Data structure definitions -----------------*/

/** Type of main table used to record all simulations currently
    being steered */

typedef struct {

  int             num_registered;
  int             max_entries;
  Sim_entry_type *sim;

} Sim_table_type;

/** Typedef for structure holding details of the main (java) proxy
    that is always associated with the steerer (if not steering
    via files or SOAP over http) */

typedef struct {

  char buf[REG_MAX_MSG_SIZE];
  int  pipe_to_proxy;
  int  pipe_from_proxy;
  int  available;

} Proxy_table_type;


/*--------- Prototypes of internal library functions -------------*/

/** Generate a filename for sending steering commands to the application
    referred to by SimHandle. */
int Generate_control_filename(int SimIndex, char* filename);

/** A look-up function - return the index of the simulation with handle 
    SimHandle in the main table.  Returns REG_SIM_HANDLE_NOTSET if no
    matching handle found. */
int Sim_index_from_handle(int SimHandle);

/** Looks for the next free entry in the table pointed to by *table.  If
    there are no free entries then more memory is allocated. Returns -1
    on failure. */
int Next_free_iodef_index(IOdef_table_type *table);

/** Checks to see that the simulation of index sim_id supports the command
    with handle cmd_id - returns REG_SUCCESS if it does and REG_FAILURE if
    it doesn't */
int Command_supported(int sim_id, int cmd_id);

/** Attach to specified simulation using local file system */
int Sim_attach_local(Sim_entry_type *sim, char *SimID);

/** Attach to specified simulation via (java) proxy.  SimID is GSH
    of sim. to attach to. */
int Sim_attach_proxy(Sim_entry_type *sim, char *SimID);

/** Read an applications supported commands from disk */
int Consume_supp_cmds_local(Sim_entry_type *sim);

/** Send the supplied control message to the simulation with the
    supplied index */
int Send_control_msg(int SimIndex, char* buf);

/** Send the supplied control message to the simulation with the
    supplied index via the java proxy */
int Send_control_msg_proxy(Sim_entry_type *sim, char* buf);

/** Send the supplied control message to the simulation with the
    supplied index via local disk */
int Send_control_msg_file(int SimIndex, char* buf);

/** Get a status message back via the java proxy */
struct msg_struct *Get_status_msg_proxy(Sim_entry_type *sim);

/** Get a status message back by reading from disk */
struct msg_struct *Get_status_msg_file(Sim_entry_type *sim);

/** Take down the steering connection */
int Finalize_connection(Sim_entry_type *sim);

/** Take down the proxy-based steering connection */
int Finalize_connection_proxy(Sim_entry_type *sim);

/** Take down the file-based steering connection */
int Finalize_connection_file(Sim_entry_type *sim);

/** free's any allocated memory held in the parameter table */
int Delete_param_table(Param_table_type *param_table);

/** Parses the last Checkpoint-log message read in for the 
    specified simulation */
int Consume_chk_log(Sim_entry_type *sim, 
		    struct chk_log_entry_struct *entry);

/** Parses the last parameter-log message read in for the 
    specified simulation */
int Consume_param_log(Sim_entry_type *sim, 
		      struct param_struct *param_ptr);

/** Obtain details associated with the supplied Chk log entry (e.g. values
    of steerable parameters at that point) */
int Get_log_entry_details(Param_table_type   *param_table,
			  Chk_log_entry_type *in,
			  Output_log_struct  *out);

/** Returns the index of the next free entry in the table of simulations
    being steered */
int Next_free_sim_index();

/** Mallocs or reallocs the logging buffer associated with the
    supplied parameter.  Updates the log_size member of
    the parameter struct. */
int Realloc_param_log(param_entry *param);

#endif
