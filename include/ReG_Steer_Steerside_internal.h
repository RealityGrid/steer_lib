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

#include "ReG_Steer_Common.h"
#include "ReG_Steer_XML.h"

/*---------------- Data structure definitions -----------------*/


/* Definition of entry in main table holding data for connected simulations.  
   Contains three sub-tables - the first holding the commands that the 
   simulation supports, the second its registered parameters (both 
   steerable and monitored) and the third its registered IO types.
   'file_root' contains the location of the directory used to communicate
   with the simulation. */

typedef struct {

  int    handle;

  /* For connection to applications using local file system*/
  char   file_root[REG_MAX_STRING_LENGTH];

  /* File descriptors used to talk to (java) proxy for
     this simulation */
  int                pipe_to_proxy;
  int                pipe_from_proxy;
  struct msg_struct *msg;

  /* Table of registered commands for this sim */
  Supp_cmd_table_type Cmds_table;

  /* Table of registered params for this sim */
  Param_table_type    Params_table;

  /* Table of registered IOTypes for this sim */
  IOdef_table_type    IOdef_table;

} Sim_entry_type;

/*--------- Prototypes of internal library functions -------------*/

/* Generate a filename for sending steering commands to the application
   referred to by SimHandle. */
static int Generate_control_filename(int SimHandle, char* filename);

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

/* Attach to specified simulation using globus_io */
static int Sim_attach_globus(Sim_entry_type *sim, char *SimID);
