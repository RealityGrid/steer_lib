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
    any particular purpose. Neither the authors, nor the University of
    Manchester offer any warranties or representations, nor do they
    accept any liabilities with respect to this software.

    This program must not be used for commmercial gain without the
    written permission of the authors.
    
    Supercomputing, Visualization & e-Science Group
    Manchester Computing
    University of Manchester
    Manchester M13 9PL

    email:  csar-advice@cfs.ac.uk.
    Tel:    +44 161 275 6824/5997
    Fax:    +44 161 275 6040    
    
    Date          Version    Updates                            Author
    ----          -------    -------                            ------
    23.7.2002       0.1                                         A Porter

---------------------------------------------------------------------------*/

#include "ReG_Steer_Common.h"

/*---------------- Data structure definitions -----------------*/


/* Definition of entry in main table holding data for connected simulations.  
   Contains three sub-tables - the first holding the commands that the 
   simulation supports, the second its registered parameters (both 
   steerable and monitored) and the third its registered IO types.
   'file_root' contains the location of the directory used to communicate
   with the simulation. */

typedef struct {

  char   file_root[REG_MAX_STRING_LENGTH];
  int    handle;

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

/* A look-up function - return the index of the parameter with handle
   ParamHandle in the table pointed to by *table. Returns 
   REG_PARAM_HANDLE_NOTSET if no matching handle found. */
static int Param_index_from_handle(Param_table_type *table, int ParamHandle);

/* A look-up function - return the index of the IOdef with handle 
   IOdefHandle in the table pointed to by *table.  Returns
   REG_IODEF_HANDLE_NOTSET if no matching handle found. */
static int IOdef_index_from_handle(IOdef_table_type *table, int IOdefHandle);

/* Looks for the next free entry in the table pointed to by *table.  If
   there are no free entries then more memory is allocated. Returns -1
   on failure. */
static int Next_free_iodef_index(IOdef_table_type *table);

/* Checks to see that the simulation of index sim_id supports the command
   with handle cmd_id - returns REG_SUCCESS if it does and REG_FAILURE if
   it doesn't */
static int Command_supported(int sim_id, int cmd_id);
