/*----------------------------------------------------------------------------
    Header file defining the internal (to the library) utility 
    routines used in the construction of a steering interface for 
    a steering component. Also contains the main table that stores
    information about each application being steered.

    (C)Copyright 2002 The University of Manchester, United Kingdom,
    all rights reserved.

    This software is produced by the Supercomputing, Visualization &
    e-Science Group, Manchester Computing, the Victoria University of
    Manchester as part of the RealityGrid project.

    This software has been tested with care but is not guaranteed for
    any particular purpose. Neither the copyright holder, nor the
    University of Manchester offer any warranties or representations,
    nor do they accept any liabilities with respect to this software.

    This software must not be used for commercial gain without the
    written permission of the authors.
    
    This software must not be redistributed without the written
    permission of the authors.

    Permission is granted to modify this software, provided any
    modifications are made freely available to the original authors.
 
    Supercomputing, Visualization & e-Science Group
    Manchester Computing
    University of Manchester
    Manchester M13 9PL
    
    WWW:    http://www.sve.man.ac.uk  
    email:  sve@man.ac.uk
    Tel:    +44 161 275 6095
    Fax:    +44 161 275 6800    

    Initial version by:  A Porter, 23.7.2002

---------------------------------------------------------------------------*/
#ifndef __REG_STEER_STEERSIDE_INTERNAL_H__
#define __REG_STEER_STEERSIDE_INTERNAL_H__


#include "ReG_Steer_Common.h"
#include "ReG_Steer_XML.h"

/*---------------- Data structure definitions -----------------*/


/* Definition of entry in main table holding data for connected simulations.  
   Contains five sub-tables - the first holding the commands that the 
   simulation supports, the second its registered parameters (both 
   steerable and monitored), the third its registered IO types, the fourth
   its registered Chk types and the fifth a log of checkpoints taken. */

typedef struct {

  int                  handle;

  /* For connection to applications using local file system - contains
     the location of the directory used to communicate with the sim. */
  char                 file_root[REG_MAX_STRING_LENGTH];

  /* File descriptors used to talk to (java) proxy for
     this simulation */
  int                  pipe_to_proxy;
  int                  pipe_from_proxy;

  /* For steering via globus_io */
  socket_type_steering socket_info;

  /* For steering via SOAP */
  struct {
    /* whether we're steering via SOAP (1) or not (0) */
    int  active;
    char address[REG_MAX_STRING_LENGTH];
    /* Holds list of names of service data elements on the SGS for which
       notifications are pending */
    char notifications[REG_MAX_NUM_SGS_SDE][REG_MAX_STRING_LENGTH];
    /* Used to keep track of notifications that we've yet to process */
    int  sde_count;
    int  sde_index;
  } SGS_info;

  /* Last status message received from this simulation - filled in
     Get_next_message and used by whichever Consume_... routine
     is called in response to the message type */
  struct msg_struct   *msg;

  /* Table of registered commands for this sim */
  Supp_cmd_table_type  Cmds_table;

  /* Table of registered params for this sim */
  Param_table_type     Params_table;

  /* Table of registered IOTypes for this sim */
  IOdef_table_type     IOdef_table;

  /* Table of registered ChkTypes for this sim */
  IOdef_table_type     Chkdef_table;

  /* Table for logging checkpoint activity */
  Chk_log_type         Chk_log;

} Sim_entry_type;


/* Type of main table used to record all simulations currently
   being steered */

typedef struct {

  int             num_registered;
  int             max_entries;
  Sim_entry_type *sim;

} Sim_table_type;

/* Typedef for structure holding details of the main (java) proxy
   that is always associated with the steerer (if not steering
   via files, Globus or SOAP) */

typedef struct {

  char buf[REG_MAX_MSG_SIZE];
  int  pipe_to_proxy;
  int  pipe_from_proxy;
  int  available;

} Proxy_table_type;


/*--------- Prototypes of internal library functions -------------*/

/* Generate a filename for sending steering commands to the application
   referred to by SimHandle. */
static int Generate_control_filename(int SimIndex, char* filename);

/* A look-up function - return the index of the simulation with handle 
   SimHandle in the main table.  Returns REG_SIM_HANDLE_NOTSET if no
   matching handle found. */
static int Sim_index_from_handle(int SimHandle);

/* Looks for the next free entry in the table pointed to by *table.  If
   there are no free entries then more memory is allocated. Returns -1
   on failure. */
static int Next_free_iodef_index(IOdef_table_type *table);

/* Checks to see that the simulation of index sim_id supports the command
   with handle cmd_id - returns REG_SUCCESS if it does and REG_FAILURE if
   it doesn't */
static int Command_supported(int sim_id, int cmd_id);

/* Attach to specified simulation using local file system */
static int Sim_attach_local(Sim_entry_type *sim, char *SimID);

/* Attach to specified simulation via (java) proxy.  SimID is GSH
   of sim. to attach to. */
static int Sim_attach_proxy(Sim_entry_type *sim, char *SimID);

static int Consume_supp_cmds_local(Sim_entry_type *sim);

static int Send_control_msg(int SimIndex, char* buf);

static int Send_control_msg_proxy(Sim_entry_type *sim, char* buf);

static int Send_control_msg_file(int SimIndex, char* buf);

static struct msg_struct *Get_status_msg_proxy(Sim_entry_type *sim);

static struct msg_struct *Get_status_msg_file(Sim_entry_type *sim);

static int Finalize_connection(Sim_entry_type *sim);

static int Finalize_connection_proxy(Sim_entry_type *sim);

static int Finalize_connection_file(Sim_entry_type *sim);

/* Obtain details associated with the supplied Chk log entry (e.g. values
   of steerable parameters at that point) */
static int Get_log_entry_details(Param_table_type   *param_table,
				 Chk_log_entry_type *in,
				 Output_log_struct  *out);

#endif
