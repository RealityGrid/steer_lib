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

#ifndef __REG_STEER_STEERSIDE_INTERNAL_H__
#define __REG_STEER_STEERSIDE_INTERNAL_H__

/** @internal
    @file ReG_Steer_Steerside_internal.h
    @brief Header for library-internal steering side routines

    Header file defining the internal (to the library) utility
    routines used in the construction of a steering interface for
    a steering component. Also contains the main table that stores
    information about each application being steered.

    @author Andrew Porter
    @author Robert Haines
*/

#include "ReG_Steer_Common.h"
#include "ReG_Steer_XML.h"

/*---------------- Data structure definitions -----------------*/

/** Type of main table used to record all simulations currently
    being steered */
typedef struct {

  /** No. of simulations we have registered with us */
  int             num_registered;
  /** Maximum no. of entries we can have in the sim array */
  int             max_entries;
  /** Ptr to array of structs holding info on each attached sim */
  Sim_entry_type *sim;

} Sim_table_type;

/** Typedef for structure holding details of the main (java) proxy
    that is always associated with the steerer (if not steering
    via files or SOAP over http) */
typedef struct {

  /** Buffer to hold messages */
  char buf[REG_MAX_MSG_SIZE];
  /** File descriptor of pipe to proxy */
  int  pipe_to_proxy;
  /** File descriptor of pipe from proxy */
  int  pipe_from_proxy;
  /** Whether the java proxy process is available */
  int  available;

} Proxy_table_type;

/** Typedef for structure holding general configuration details
    for the steerer */
typedef struct {
  /** Whether the OpenSSL pseudo-random no. generator is
      correctly initialized */
  int             ossl_rand_available;
  /** The location of the pem file containing the user's certificate
      and private key concatenated together. */
  char            privateKeyCertFile[REG_MAX_STRING_LENGTH];
  /** Path to the directory containing the CA certificates (for
      authenticating any secure containers we connect to when using
      SOAP over https) */
  char            caCertsPath[REG_MAX_STRING_LENGTH];

} Steerer_config_table_type;

/*--------- Prototypes of internal library functions -------------*/

/** @internal
    @param SimHandle Handle of an attached simulation

    A look-up function - return the index of the simulation with handle
    @p SimHandle in the main table.  Returns REG_SIM_HANDLE_NOTSET if no
    matching handle found. */
int Sim_index_from_handle(int SimHandle);

/** @internal
    @param table Pointer to table to search for a free entry
    @return Index of next free entry in the table or -1 on failure

    Looks for the next free entry in the specified table.  If
    there are no free entries then more memory is allocated. */
int Next_free_iodef_index(IOdef_table_type *table);

/** @internal
    @param sim_id Index of attached simulation in main Sim_table
    @param cmd_id ID of command
    @return REG_SUCCESS if simulation supports the command,
    REG_FAILURE otherwise

    Checks to see that the simulation of index @p sim_id supports the
    command with handle @p cmd_id  */
int Command_supported(int sim_id, int cmd_id);

/** @internal
    @param SimIndex Index of simulation in main Sim_table
    @param buf Buffer containing control message

    Send the supplied control message to the simulation with the
    supplied index */
int Send_control_msg(int SimIndex, char* buf);

/** @internal
    @param index Index of simulation entry in Sim_table

    Take down the steering connection */
int Finalize_connection(int index);

/** @internal
    @param param_table Pointer to parameter table

    free's any allocated memory held in the parameter table */
int Delete_param_table(Param_table_type *param_table);

/** @internal
    @param sim Pointer to simulation entry in Sim_table
    @param entry Pointer to beginning of linked list of chk log
    entries

    Parses the last Checkpoint-log message read in for the
    specified simulation.  Stores results within the Chk_log field
    lf the supplied Sim_entry_type structure. */
int Consume_chk_log(Sim_entry_type *sim,
		    struct chk_log_entry_struct *entry);

/** @internal
    @param sim Pointer to simulation entry in Sim_table
    @param param_ptr Pointer to beginning of linked list of
    parameter details

    Parses the last parameter-log message read in for the
    specified simulation and stores the details in the Params_table
    struct of the Sim_entry_type */
int Consume_param_log(Sim_entry_type *sim,
		      struct param_struct *param_ptr);

/** @internal
    @param param_table Pointer to param table from which to get details
    @param in The type of Checkpoint log to get details for
    @param out On successful return, holds the details associated with
    the specified checkpoint log.

    Obtain details associated with the supplied Chk log entry (@e
    e.g. values of steerable parameters at that point). Necessary
    because Chk_log_entry_type only holds parameter handles and no
    other information. Parameter details are extracted from the table
    pointed to by @p param_table */
int Get_log_entry_details(Param_table_type   *param_table,
			  Chk_log_entry_type *in,
			  Output_log_struct  *out);

/** @internal
    Returns the index of the next free entry in the table of simulations
    being steered */
int Next_free_sim_index();

/** @internal
    Mallocs or reallocs the logging buffer associated with the
    supplied parameter.  Updates the log_size member of
    the parameter struct. */
int Realloc_param_log(param_entry *param);

/** @internal
    Signal handler for steerer-side of library.  Calls Steerer_finalize()
    to attempt to go down gracefully
    @param aSignal The signal that has been caught. */
void Steerside_signal_handler(int aSignal);

#endif
