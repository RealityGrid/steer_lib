/*----------------------------------------------------------------------------
  This file contains routines and data structures for the construction
  of an interface to a steering component (from an application
  component).

  (C) Copyright 2002, 2004, University of Manchester, United Kingdom,
  all rights reserved.

  This software is produced by the Supercomputing, Visualization and
  e-Science Group, Manchester Computing, University of Manchester
  as part of the RealityGrid project (http://www.realitygrid.org),
  funded by the EPSRC under grants GR/R67699/01 and GR/R67699/02.

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

#include "ReG_Steer_Appside.h"
#include "ReG_Steer_Appside_internal.h"
#include "ReG_Steer_Appside_Sockets.h"
#include "ReG_Steer_Appside_Soap.h"
#include "ReG_Steer_Appside_File.h"

#include <signal.h>
#include <unistd.h>
#include <time.h>
#include <rpc/rpc.h>
#include <math.h>
#include <string.h>

/* Allow value of 'REG_DEBUG' to propagate down from Reg_steer_types.h if
   it has been set there */
#ifndef REG_DEBUG
#define REG_DEBUG 1
#endif

/* Buffer used for string handling etc - use 1MB for now */
#define REG_SCRATCH_BUFFER_SIZE 1048576
char Global_scratch_buffer[REG_SCRATCH_BUFFER_SIZE];

/* The table holding details of our communication channel with the
   steering client */
Steerer_connection_table_type Steerer_connection;

/* IOdef_table_type is declared in ReG_Steer_Common.h since it is 
   used in both the steerer-side and app-side libraries */

IOdef_table_type IOTypes_table;

/* Table for registered checkpoint types */

IOdef_table_type ChkTypes_table;

/* Log of checkpoints taken */

Chk_log_type Chk_log;

/* Log of steering commands received */

Steer_log_type Steer_log;

/* Param_table_type is declared in ReG_Steer_Common.h since it is 
   used in both the steerer-side and app-side libraries */

Param_table_type Params_table;

/* Whether steering is enabled (set by user) */
static int ReG_SteeringEnabled = FALSE;
/* Whether the set of registered params has changed */
static int ReG_ParamsChanged   = FALSE;
/* Whether the set of registered IO types has changed */
static int ReG_IOTypesChanged  = FALSE;
/* Whether the set of registered Chk types has changed */
static int ReG_ChkTypesChanged = FALSE;
/* Whether app. is currently being steered */
static int ReG_SteeringActive  = FALSE;
/* Whether steering library has been initialised */
static int ReG_SteeringInit    = FALSE;
/* Whether steering lib is being called from F90 */
static int ReG_CalledFromF90   = FALSE;
/* Absolute path of directory we are executing in */
char ReG_CurrentDir[REG_MAX_STRING_LENGTH];
/* Hostname of machine we are executing on */
char ReG_Hostname[REG_MAX_STRING_LENGTH];
/* Name (and version) of the application that has called us */
char ReG_AppName[REG_MAX_STRING_LENGTH];

/* Global variable used to store next valid handle value for both
   IOTypes and ChkTypes - these MUST have unique handles because
   they are used as command IDs */
static int Next_IO_Chk_handle = REG_MIN_IOTYPE_HANDLE;

/*----------------------------------------------------------------*/

void Steering_enable(const int EnableSteer)
{
  /* Set global flag that controls whether steering is enabled or not */
  ReG_SteeringEnabled = EnableSteer;

  return;
}

/*----------------------------------------------------------------*/

int Steering_initialize(char *AppName,
			int   NumSupportedCmds,
			int  *SupportedCmds)
{
  int   i, j;
  char *pchar, *ip_addr;

  /* Actually defined in ReG_Steer_Common.c because both steerer
     and steered have a variable of this name */
  extern char ReG_Steer_Schema_Locn[REG_MAX_STRING_LENGTH];
  char       *schema_path = "xml_schema/reg_steer_comm.xsd";

#if REG_DEBUG
  /* Print out version information */
  fprintf(stderr, "**** RealityGrid Computational Steering Library "
	  "v.%s ****\n\n", REG_STEER_LIB_VERSION);
#endif

  /* Don't do anything if steering is not enabled */
  if (!ReG_SteeringEnabled){
    fprintf(stderr, "Steering_initialize: WARNING: steering library "
	    "not enabled - no steering will be possible\n");
    return REG_SUCCESS;
  }

  /* Set the location of the file containing the schema describing all 
     steering communication */

  if(pchar = getenv("REG_STEER_HOME")){

    /* Check that path ends in '/' - if not then add one */

    i = strlen(pchar);

#if REG_DEBUG
    /* Check that path of schema location fits in the string we've
       put aside for it */
    if((i + strlen(schema_path) + 1) > REG_MAX_STRING_LENGTH){

      fprintf(stderr, "Steering_initialize: schema path exceeds "
	      "REG_MAX_STRING_LENGTH (%d chars)\n", REG_MAX_STRING_LENGTH);
      Steering_enable(FALSE);
      return REG_FAILURE;
    }
#endif

    if( pchar[i-1] != '/' ){

      sprintf(ReG_Steer_Schema_Locn, "%s/%s", pchar, schema_path);
    }
    else{

      sprintf(ReG_Steer_Schema_Locn, "%s%s", pchar, schema_path);
    }
  }
  else{

    fprintf(stderr, "Steering_initialize: Error - REG_STEER_HOME "
	    "environment variable not set\n");
    Steering_enable(FALSE);
    return REG_FAILURE;
  }

  /* Get our current working directory - ASSUMES 'pwd' is in our path */
  memset(ReG_CurrentDir, '\0', REG_MAX_STRING_LENGTH);
  if( (pchar = getenv("PWD")) ){

    strcpy(ReG_CurrentDir, pchar);
  }
  else{

    fprintf(stderr, "Steering_initialize: failed to get working "
	    "directory\n");
  } 

  /* Store the user-supplied name and version of the calling
     application */
  if(strlen(AppName) > REG_MAX_STRING_LENGTH){

    fprintf(stderr, "Steering_initialize: Error - tag specifying "
	    "application name and version exceeds %d chars\n",
            REG_MAX_STRING_LENGTH);
    Steering_enable(FALSE);
    return REG_FAILURE;
  }
  strcpy(ReG_AppName, AppName);

  /* Get the hostname of this machine - used to tell the outside
     world how to contact us and where the (checkpoint) files we've 
     written live.  On many machines we may be running on a node
     that cannot be seen by the outside world and therefore globus 
     connections for file transfer etc. will need an address of a 
     publicly-accessible node.  This can be passed to us via the
     REG_MACHINE_NAME environment variable. */
  if( (pchar = getenv("REG_MACHINE_NAME")) ){
    
    strcpy(ReG_Hostname, pchar);
  }
  else if(Get_fully_qualified_hostname(&pchar, &ip_addr) == REG_SUCCESS){
    strcpy(ReG_Hostname, pchar);
  }
  else{
    ReG_Hostname[0] = '\0';
    fprintf(stderr, "Steering_initialize: failed to get machine name\n");
  }

#if REG_DEBUG
  fprintf(stderr, "Steering_initialize: machine name = %s\n", 
	  ReG_Hostname);
#endif

  /* Allocate memory and initialise tables of IO types and 
     parameters */

  IOTypes_table.num_registered = 0;
  IOTypes_table.max_entries    = REG_INITIAL_NUM_IOTYPES;
  IOTypes_table.enable_on_registration = TRUE;
  IOTypes_table.io_def         = (IOdef_entry *)
                                 malloc(IOTypes_table.max_entries
					*sizeof(IOdef_entry));
 
  if(IOTypes_table.io_def == NULL){
    
    fprintf(stderr, "Steering_initialize: failed to allocate memory "
	    "for IOType table\n");
    Steering_enable(FALSE);
    return REG_FAILURE;
  }

  for(i=0; i<IOTypes_table.max_entries; i++){

    IOTypes_table.io_def[i].handle = REG_IODEF_HANDLE_NOTSET;
#if REG_SOCKET_SAMPLES
    sprintf(IOTypes_table.io_def[i].socket_info.listener_hostname,
	    "%s", "NOT_SET");
#endif
  }

  /* Initialise table of open IO channels */

  for(i=0; i<REG_INITIAL_NUM_IOTYPES; i++){
    IO_channel[i].buffer = NULL;
  }

  /* Initialise table for registered checkpoint types */

  ChkTypes_table.num_registered = 0;
  ChkTypes_table.max_entries    = REG_INITIAL_NUM_IOTYPES;
  ChkTypes_table.io_def         = (IOdef_entry *)
                                   malloc(IOTypes_table.max_entries
				  	  *sizeof(IOdef_entry));
 
  if(ChkTypes_table.io_def == NULL){
    
    fprintf(stderr, "Steering_initialize: failed to allocate memory "
	    "for ChkType table\n");
    free(IOTypes_table.io_def);
    IOTypes_table.io_def = NULL;
    Steering_enable(FALSE);
    return REG_FAILURE;
  }

  for(i=0; i<ChkTypes_table.max_entries; i++){

    ChkTypes_table.io_def[i].handle = REG_IODEF_HANDLE_NOTSET;
  }

  /* Set up table for registered parameters */

  Params_table.num_registered = 0;
  Params_table.max_entries    = REG_INITIAL_NUM_PARAMS;
  Params_table.next_handle    = REG_MIN_PARAM_HANDLE;
  Params_table.param          = (param_entry *)malloc(Params_table.max_entries
					      *sizeof(param_entry));

  if(Params_table.param == NULL){

    fprintf(stderr, "Steering_initialize: failed to allocate memory "
	    "for param table\n");

    free(IOTypes_table.io_def);
    IOTypes_table.io_def = NULL;
    free(ChkTypes_table.io_def);
    ChkTypes_table.io_def = NULL;
    Steering_enable(FALSE);
    return REG_FAILURE;
  }

  /* Initialise parameter handles */

  for(i=0; i<Params_table.max_entries; i++){

    Params_table.param[i].handle = REG_PARAM_HANDLE_NOTSET;
    Params_table.param[i].min_val_valid = FALSE;
    Params_table.param[i].max_val_valid = FALSE;
  }

  /* 'Sequence number' is treated as a parameter */
  Params_table.param[0].ptr       = NULL;
  Params_table.param[0].type      = REG_INT;
  Params_table.param[0].handle    = REG_SEQ_NUM_HANDLE;
  Params_table.param[0].steerable = FALSE;
  Params_table.param[0].modified  = FALSE;
  Params_table.param[0].is_internal=FALSE;
  sprintf(Params_table.param[0].label, "SEQUENCE_NUM");
  sprintf(Params_table.param[0].value, "-1");
  sprintf(Params_table.param[0].min_val, "-1");
  Params_table.param[0].min_val_valid = TRUE;
  /* Max. value for sequence number is unlimited */
  sprintf(Params_table.param[0].max_val, " ");
  Params_table.param[0].max_val_valid = FALSE;
  Increment_param_registered(&Params_table);

  /* Parameter for monitoring CPU time per step */
  i = Params_table.num_registered;
  Params_table.param[i].ptr       = NULL;
  Params_table.param[i].type      = REG_FLOAT;
  Params_table.param[i].handle    = REG_STEP_TIME_HANDLE;
  Params_table.param[i].steerable = FALSE;
  Params_table.param[i].modified  = FALSE;
  Params_table.param[i].is_internal=FALSE;
  sprintf(Params_table.param[i].label, "CPU_TIME_PER_STEP");
  sprintf(Params_table.param[i].value, "0.0");
  sprintf(Params_table.param[i].min_val, "");
  Params_table.param[i].min_val_valid = FALSE;
  sprintf(Params_table.param[i].max_val, "");
  Params_table.param[i].max_val_valid = FALSE;
  Increment_param_registered(&Params_table);

  /* Parameter for recording time stamp - currently ONLY used
     for checkpoint logging */
  i = Params_table.num_registered;
  Params_table.param[i].ptr       = NULL;
  Params_table.param[i].type      = REG_CHAR;
  Params_table.param[i].handle    = REG_TIMESTAMP_HANDLE;
  Params_table.param[i].steerable = FALSE;
  Params_table.param[i].modified  = FALSE;
  Params_table.param[i].is_internal=TRUE;
  sprintf(Params_table.param[i].label, "TIMESTAMP");
  sprintf(Params_table.param[i].value, "");
  sprintf(Params_table.param[i].min_val, "");
  Params_table.param[i].min_val_valid = FALSE;
  sprintf(Params_table.param[i].max_val, "");
  Params_table.param[i].max_val_valid = FALSE;
  Increment_param_registered(&Params_table);

  /* Set-up a steerable parameter to control how often the steering lib.
     attempts to do steering stuff (a value of 1 means everytime that
     Steering_control is called, 10 means once for every ten calls etc.) */
  Steerer_connection.steer_interval = 1;

  i = Params_table.num_registered;
  Params_table.param[i].ptr       = (void *)(&(Steerer_connection.steer_interval));
  Params_table.param[i].type      = REG_INT;
  Params_table.param[i].handle    = REG_STEER_INTERVAL_HANDLE;
  Params_table.param[i].steerable = TRUE;
  Params_table.param[i].modified  = FALSE;
  Params_table.param[i].is_internal=FALSE;
  sprintf(Params_table.param[i].label, "STEERING_INTERVAL");
  sprintf(Params_table.param[i].value, "1");
  sprintf(Params_table.param[i].min_val, "1");
  Params_table.param[i].min_val_valid = TRUE;
  sprintf(Params_table.param[i].max_val, "");
  Params_table.param[i].max_val_valid = FALSE;
  Increment_param_registered(&Params_table);

  /* Set-up/prepare for connection to steering client */
  if(Initialize_steering_connection(NumSupportedCmds, 
				    SupportedCmds) != REG_SUCCESS){

    free(IOTypes_table.io_def);
    IOTypes_table.io_def = NULL;
    free(ChkTypes_table.io_def);
    ChkTypes_table.io_def = NULL;
    free(Params_table.param);
    Params_table.param = NULL;

    Steering_enable(FALSE);
    return REG_FAILURE;
  }

  /* Initialise log of checkpoints */
  /* Jens, 09.04.03: Moved these lines here because Open_log_file()
   * needs Initialize_steering_connection to be called before. */

  Chk_log.num_entries = 0;
  Chk_log.max_entries = REG_INITIAL_CHK_LOG_SIZE;
  Chk_log.num_unsent  = 0;
  Chk_log.send_all    = TRUE;

  Set_log_primary_key();

  if( Open_log_file() != REG_SUCCESS ){
    fprintf(stderr, "Steering_initialize: failed to create log file, "
	    "log will not be saved to file\n");
  }

  Chk_log.entry = (Chk_log_entry_type *)malloc(Chk_log.max_entries
					  *sizeof(Chk_log_entry_type));

  Chk_log.pSteer_cmds = (char *)malloc(REG_MAX_MSG_SIZE*sizeof(char));
  Chk_log.steer_cmds_bytes = REG_MAX_MSG_SIZE;

  if(!(Chk_log.entry) || !(Chk_log.pSteer_cmds) ){

    fprintf(stderr, "Steering_initialize: failed to allocate memory "
	    "for checkpoint logging\n");
    free(IOTypes_table.io_def);
    IOTypes_table.io_def = NULL;
    free(ChkTypes_table.io_def);
    ChkTypes_table.io_def = NULL;
    free(Params_table.param);
    Params_table.param = NULL;
    Steering_enable(FALSE);
    return REG_FAILURE;
  }

  Chk_log.pSteer_cmds[0] = '\0';
  /* pSteer_cmds_slot points to next free space in the buffer */
  Chk_log.pSteer_cmds_slot = Chk_log.pSteer_cmds;

  for(i=0; i<Chk_log.max_entries; i++){

    Chk_log.entry[i].sent_to_steerer = TRUE;
    Chk_log.entry[i].num_param       = 0;

    for(j=0; j<REG_MAX_NUM_STR_PARAMS; j++){
      Chk_log.entry[i].param[j].handle = REG_PARAM_HANDLE_NOTSET;
    }
  }

  /* Initialise table for logging steering commands */
  
  Steer_log.num_cmds = 0;
  Steer_log.num_params = 0;

  /* Set up signal handler so can clean up if application 
     exits in a hurry */
  /* ctrl-c */
  signal(SIGINT, Steering_signal_handler);
  /* kill (note cannot (and should not) catch kill -9) */
  signal(SIGTERM, Steering_signal_handler);
  signal(SIGSEGV, Steering_signal_handler);
  signal(SIGILL, Steering_signal_handler);
  signal(SIGABRT, Steering_signal_handler);
  signal(SIGFPE, Steering_signal_handler);
#if REG_SOCKET_SAMPLES
#ifndef __linux
  /* Catch the broken pipe signal for sending down a disconnected
     socket. Linux allows us to do this with a flag to send()    */
  signal(SIGPIPE, signal_handler_sockets);
#endif
#endif

  /* Flag that library has been successfully initialised */

  ReG_SteeringInit = TRUE;

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Steering_finalize()
{
  int i;

  /* Can only call this function if steering lib initialised */

  if (!ReG_SteeringInit) return REG_FAILURE;

  /* Tell the steerer that we are done - signal that component 
     no-longer steerable */
  Finalize_steering_connection();

  /* Save remaining log entries to file */
  Save_log();

  /* Close log file */
  Close_log_file();

  /* Clean-up IOTypes table */

  if(IOTypes_table.io_def){
    /* cleanup transport mechanism */
    Finalize_IOType_transport();

    /* Free buffers associated with each iotype */
    for(i=0; i<IOTypes_table.num_registered; i++){

      if(IOTypes_table.io_def[i].buffer){
	free(IOTypes_table.io_def[i].buffer);
	IOTypes_table.io_def[i].buffer = NULL;
	IOTypes_table.io_def[i].buffer_bytes = 0;
      }
    }
    free(IOTypes_table.io_def);
    IOTypes_table.io_def = NULL;
  }

  IOTypes_table.num_registered = 0;
  IOTypes_table.max_entries = REG_INITIAL_NUM_IOTYPES;

  /* Clean-up ChkTypes table */

  if(ChkTypes_table.io_def != NULL){
    free(ChkTypes_table.io_def);
    ChkTypes_table.io_def = NULL;
  }

  ChkTypes_table.num_registered = 0;
  ChkTypes_table.max_entries = REG_INITIAL_NUM_IOTYPES;

  /* Clean-up log of checkpoints */

  if(Chk_log.entry != NULL){

    free(Chk_log.entry);
    Chk_log.entry = NULL;
  }
  Chk_log.num_entries = 0;
  Chk_log.max_entries = REG_INITIAL_CHK_LOG_SIZE;
  /* Buffer for logged commands */
  free(Chk_log.pSteer_cmds);
  Chk_log.pSteer_cmds = NULL;
  Chk_log.steer_cmds_bytes = 0;

  /* Clean-up parameters table */

  if(Params_table.param != NULL){
    free(Params_table.param);
    Params_table.param = NULL;
  }

  Params_table.num_registered = 0;
  Params_table.max_entries = REG_INITIAL_NUM_IOTYPES;

  /* Reset state of library */

  ReG_ParamsChanged  = FALSE;
  ReG_IOTypesChanged = FALSE;
  ReG_ChkTypesChanged = FALSE;
  ReG_SteeringActive = FALSE;

  /* Flag that library no-longer initialised */
  ReG_SteeringInit    = FALSE;

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Register_IOTypes(int    NumTypes,
                     char* *IOLabel,
		     int   *direction,
		     int   *IOFrequency,
                     int   *IOType)
{
  int          i;
  int          current;
  int          new_size;
  IOdef_entry *dum_ptr;
  char        *iofreq_labels[1];
  char        *freq_label = "IO_Frequency";
  int          iofreq_strbl;
  int          iofreq_type;
  int          iparam;
  void        *ptr_array[1];
  char        *min_array[1];
  char        *max_array[1];
  char        *min_val = "0";
  char        *max_val = " ";
  int          return_status = REG_SUCCESS;

  /* Check that steering is enabled */

  if(!ReG_SteeringEnabled){

    for(i=0; i<NumTypes; i++){
      IOType[i] = REG_IODEF_HANDLE_NOTSET;
    }
    return REG_SUCCESS;
  }

  /* Can only call this function if steering lib initialised */

  if (!ReG_SteeringInit) return REG_FAILURE;

  /* IO types cannot be deleted so is safe to use num_registered to 
     get next free entry */
  current = IOTypes_table.num_registered;

  for(i=0; i<NumTypes; i++){

    if(String_contains_xml_chars(IOLabel[i]) == TRUE){

      fprintf(stderr, "Register_IOTypes: ERROR: IO label contains "
	      "reserved xml characters (<,>,&): %s\n"
              "     - skipping this IOType.\n", IOLabel[i]);
      continue;
    }

    strncpy(IOTypes_table.io_def[current].label, IOLabel[i],
	    REG_MAX_STRING_LENGTH);

    /* Will need to check that IOLabel is something that the 
       component framework knows about */
  
    /* Whether input or output (sample data) */

    IOTypes_table.io_def[current].direction = direction[i];

    /* Set variables required for registration of associated io
       frequency as a steerable parameter */

    iofreq_labels[0] = freq_label;
    iofreq_strbl = TRUE;
    iofreq_type  = REG_INT;
    IOTypes_table.io_def[current].frequency = IOFrequency[i];
    ptr_array[0] = (void *)&(IOTypes_table.io_def[current].frequency);
    min_array[0] = min_val;
    max_array[0] = max_val;

    Register_params(1,
		    iofreq_labels,
		    &iofreq_strbl,
		    ptr_array,
		    &iofreq_type,
		    min_array,
		    max_array);

    /* Store the handle given to this parameter - this line must
       immediately succeed the call to Register_params */

    IOTypes_table.io_def[current].freq_param_handle = 
	                               Params_table.next_handle - 1;

    /* Annotate the parameter table entry just created to flag that
       it is a parameter that is internal to the steering library */
    iparam = Param_index_from_handle(&Params_table, 
			      IOTypes_table.io_def[current].freq_param_handle);

    if(iparam != -1){
      Params_table.param[iparam].is_internal = TRUE;
    }
    else{

#if REG_DEBUG
      fprintf(stderr, "Register_IOTypes: failed to get handle for param\n");
#endif
      return_status = REG_FAILURE;
    }
      
    IOTypes_table.io_def[current].buffer = NULL;
    IOTypes_table.io_def[current].buffer_bytes = 0;
    IOTypes_table.io_def[current].use_xdr = FALSE;
    IOTypes_table.io_def[current].num_xdr_bytes = 0;
    IOTypes_table.io_def[current].array.nx = 0;
    IOTypes_table.io_def[current].array.ny = 0;
    IOTypes_table.io_def[current].array.nz = 0;
    IOTypes_table.io_def[current].array.sx = 0;
    IOTypes_table.io_def[current].array.sy = 0;
    IOTypes_table.io_def[current].array.sz = 0;
    IOTypes_table.io_def[current].convert_array_order = FALSE;
    IOTypes_table.io_def[current].is_enabled = IOTypes_table.enable_on_registration;

    /* set up transport for sample data - eg sockets */
    return_status = Initialize_IOType_transport(direction[i], current);

    /* Create, store and return a handle for this IOType */
    IOTypes_table.io_def[current].handle = Next_IO_Chk_handle++;
    IOType[i] = IOTypes_table.io_def[current].handle;

    current++;

    if(current == IOTypes_table.max_entries){

      new_size = IOTypes_table.max_entries + REG_INITIAL_NUM_IOTYPES;

      dum_ptr = (IOdef_entry*)realloc((void *)(IOTypes_table.io_def),
		                      new_size*sizeof(IOdef_entry));

      if(dum_ptr == NULL){

        fprintf(stderr, "Register_IOTypes: failed to allocate memory\n");
	return REG_FAILURE;
      }
      else{

	IOTypes_table.io_def = dum_ptr;
      }

      IOTypes_table.max_entries += REG_INITIAL_NUM_IOTYPES;
    }
  }

  IOTypes_table.num_registered = current;

  /* Flag that the registered IO Types have changed */
  ReG_IOTypesChanged = TRUE;

  return return_status;
}

/*----------------------------------------------------------------*/

int Disable_IOType(int IOType){

  int index;
  int status = REG_FAILURE;

  /* Check that steering is enabled */
  if(!ReG_SteeringEnabled) return REG_SUCCESS;

  /* Can only call this function if steering lib initialised */
  if (!ReG_SteeringInit) return REG_FAILURE;

  /* Find corresponding entry in table of IOtypes */
  index = IOdef_index_from_handle(&IOTypes_table, IOType);
  if(index == REG_IODEF_HANDLE_NOTSET){

    fprintf(stderr, "Disable_IOType: failed to find matching IOType\n");
    return REG_FAILURE;
  }

  if(IOTypes_table.io_def[index].is_enabled == TRUE){

#if REG_SOCKET_SAMPLES
    status = Disable_IOType_sockets(index);
#endif
    IOTypes_table.io_def[index].is_enabled = FALSE;

    /* If this is an output IOType then destroying the socket
       changes the listening port and so we have to reset its IOType
       definition. */
    if(status == REG_SUCCESS && 
       IOTypes_table.io_def[index].direction == REG_IO_OUT){

      Emit_IOType_defs();
    }
  }

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Enable_IOTypes_on_registration(int toggle){

  /* Default operation is for IOTypes to be enabled (socket created
     in the case of sockets) when they are registered.  Can turn
     this off using this function prior to call to Register_IOTypes.*/

  if(toggle == TRUE){
    IOTypes_table.enable_on_registration = TRUE;
  }
  else if(toggle == FALSE){
    IOTypes_table.enable_on_registration = FALSE;
  }
  else{
    return REG_FAILURE;
  }
  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Enable_IOType(int IOType){

  int index;
  int status = REG_FAILURE;

  /* Check that steering is enabled */
  if(!ReG_SteeringEnabled) return REG_SUCCESS;

  /* Can only call this function if steering lib initialised */
  if(!ReG_SteeringInit){
    fprintf(stderr, "Enable_IOType: error: steering library not "
	    "initialised\n");
    return REG_FAILURE;
  }

  /* Find corresponding entry in table of IOtypes */
  index = IOdef_index_from_handle(&IOTypes_table, IOType);
  if(index == REG_IODEF_HANDLE_NOTSET){

    fprintf(stderr, "Enable_IOType: failed to find matching IOType\n");
    return REG_FAILURE;
  }

  if(IOTypes_table.io_def[index].is_enabled == FALSE){

#if REG_SOCKET_SAMPLES
    status = Enable_IOType_sockets(index);
#endif
    IOTypes_table.io_def[index].is_enabled = TRUE;

    /* If this is an output IOType then creating the socket
       changes the listening port and so we have to reset its IOType
       definition. */
    if(status == REG_SUCCESS && 
       IOTypes_table.io_def[index].direction == REG_IO_OUT){

      Emit_IOType_defs();
    }
  }
#if REG_DEBUG
  else{
    fprintf(stderr, "Enable_IOType: IOType %d already enabled\n", 
	    IOType);
  }
#endif

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Set_f90_array_ordering(int IOTypeIndex, int flag){

  /* Check that steering is enabled */
  if(!ReG_SteeringEnabled) return REG_SUCCESS;

  /* Can only call this function if steering lib initialised */
  if (!ReG_SteeringInit) return REG_FAILURE;

  if(IOTypeIndex < 0){
    fprintf(stderr, "Set_f90_ordering: IOTypeIndex is < 0\n");
    return REG_FAILURE;
  }

  IOTypes_table.io_def[IOTypeIndex].array.is_f90 = flag;

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Called_from_f90(int flag){

  if(flag == TRUE){
    ReG_CalledFromF90 = TRUE;
  }
  else if(flag == FALSE){
    ReG_CalledFromF90 = FALSE;
  }
  else{
    fprintf(stderr, "Called_from_f90: flag is neither TRUE or FALSE\n");
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Register_ChkTypes(int    NumTypes,
		      char* *ChkLabel,
		      int   *direction,
		      int   *ChkFrequency,
		      int   *ChkType)
{
  int 	       i;
  int 	       current;
  int 	       iparam;
  int          new_size;
  char        *chkfreq_labels[1];
  char        *freq_label = "Chk_Frequency";
  int          chkfreq_strbl;
  int          chkfreq_type;
  void        *ptr_array[1];
  char        *min_array[1];
  char        *max_array[1];
  char        *min_val = "0";
  char        *max_val = " ";
  IOdef_entry *dum_ptr;
  int          return_status = REG_SUCCESS;

  /* Check that steering is enabled */

  if(!ReG_SteeringEnabled){

    for(i=0; i<NumTypes; i++){
      ChkType[i] = REG_IODEF_HANDLE_NOTSET;
    }
    return REG_SUCCESS;
  }

  /* Can only call this function if steering lib initialised */

  if (!ReG_SteeringInit) return REG_FAILURE;

  /* Chk types cannot be deleted so is safe to use num_registered to 
     get next free entry */
  current = ChkTypes_table.num_registered;

  for(i=0; i<NumTypes; i++){

    if(String_contains_xml_chars(ChkLabel[i]) == TRUE){

      fprintf(stderr, "Register_ChkTypes: ERROR: Chk label contains "
	      "reserved xml characters (<,>,&): %s\n"
	      "     - skipping this ChkType.\n", ChkLabel[i]);
      continue;
    }

    strcpy(ChkTypes_table.io_def[current].label, ChkLabel[i]);

    /* filename not used currently */
    sprintf(ChkTypes_table.io_def[current].filename, "NOT_SET");

    /* Whether input or output */
    ChkTypes_table.io_def[current].direction = direction[i];

    /* Set variables required for registration of associated io
       frequency as a steerable parameter (but only if checkpoint is
       to be emitted) */
    if(direction[i] != REG_IO_IN){

      /* Set variables required for registration of associated io
         frequency as a steerable parameter */

      chkfreq_labels[0] = freq_label;
      chkfreq_strbl = TRUE;
      chkfreq_type  = REG_INT;
      ChkTypes_table.io_def[current].frequency = ChkFrequency[i];
      ptr_array[0]  = (void *)&(ChkTypes_table.io_def[current].frequency);
      min_array[0]  = min_val;
      max_array[0]  = max_val;

      Register_params(1,
		      chkfreq_labels,
		      &chkfreq_strbl,
		      ptr_array,
		      &chkfreq_type,
		      min_array, 
		      max_array);

      /* Store the handle given to this parameter - this line MUST
         immediately succeed the call to Register_params */

      ChkTypes_table.io_def[current].freq_param_handle = 
	                                 Params_table.next_handle - 1;

      /* Annotate the parameter table entry just created to flag that
         it is a parameter that is internal to the steering library */
      iparam = Param_index_from_handle(&Params_table, 
			    ChkTypes_table.io_def[current].freq_param_handle);
      if(iparam != -1){
        Params_table.param[iparam].is_internal = TRUE;
      }
      else{
#if REG_DEBUG
        fprintf(stderr, "Register_ChkTypes: failed to get handle for param\n");
#endif
        return_status = REG_FAILURE;
      }
    }
    else{
      /* Auto consume is senseless for checkpoints so set frequency-related
	 elements to 'null' values */
      ChkTypes_table.io_def[current].freq_param_handle = REG_PARAM_HANDLE_NOTSET;
      ChkTypes_table.io_def[current].frequency = 0;
    }

    /* Create, store and return a handle for this ChkType */
    ChkTypes_table.io_def[current].handle = Next_IO_Chk_handle++;
    ChkType[i] = ChkTypes_table.io_def[current].handle;

    /* Check whether we need to allocate more storage */
    current++;
    if(current == ChkTypes_table.max_entries){

      new_size = ChkTypes_table.max_entries + REG_INITIAL_NUM_IOTYPES;

      dum_ptr = (IOdef_entry*)realloc((void *)(ChkTypes_table.io_def),
		                      new_size*sizeof(IOdef_entry));

      if(dum_ptr == NULL){

        fprintf(stderr, "Register_ChkTypes: failed to allocate memory\n");
	return REG_FAILURE;
      }
      else{

	ChkTypes_table.io_def = dum_ptr;
      }

      ChkTypes_table.max_entries += REG_INITIAL_NUM_IOTYPES;
    }

  } /* End of loop over Chk types to register */

  ChkTypes_table.num_registered = current;

  /* Flag that the registered Chk Types have changed */
  ReG_ChkTypesChanged = TRUE;

  return return_status;
}

/*----------------------------------------------------------------*/

int Record_Chkpt(int   ChkType,
		 char *ChkTag)
{
  int    index;
  int    count;
  time_t time_now;
  char   *pchar;

  /* Can only call this function if steering lib initialised */

  if (!ReG_SteeringInit) return REG_FAILURE;

  if (ChkType == REG_IODEF_HANDLE_NOTSET) return REG_SUCCESS;

  /* Check that we have enough storage space - if not then store
     current entries on disk (rather than continually grab more
     memory) */

  if(Chk_log.num_entries == Chk_log.max_entries){

    /* Save_log also resets Chk_log.num_entries to zero */
    if(Save_log() != REG_SUCCESS){

      fprintf(stderr, "Record_Chkpt: Save_log failed\n");
      return REG_FAILURE;
    }
  }

  Chk_log.entry[Chk_log.num_entries].key = Chk_log.primary_key_value++;
  strcpy(Chk_log.entry[Chk_log.num_entries].chk_tag, ChkTag);
  Chk_log.entry[Chk_log.num_entries].chk_handle      = ChkType;
  Chk_log.entry[Chk_log.num_entries].sent_to_steerer = FALSE;

  /* Store the values of all registered parameters at this point (so
     long as they're not internal to the library) */
  count = 0;
  for(index = 0; index<Params_table.max_entries; index++){

    if(Params_table.param[index].handle == REG_PARAM_HANDLE_NOTSET ||
       Params_table.param[index].is_internal == TRUE){

      /* Time stamp is a special case - is internal but we do want
	 it for checkpoint records */
      if(Params_table.param[index].handle != REG_TIMESTAMP_HANDLE){
	continue;
      }
      else{
	/* Get timestamp */
	if( (int)(time_now = time(NULL)) != -1){
	  pchar = ctime(&time_now);
	  strcpy(Params_table.param[index].value, pchar);
	  /* Remove new-line character */
          Params_table.param[index].value[strlen(pchar)-1] = '\0';
	}
	else{
	  sprintf(Params_table.param[index].value, "");
	}
      }
    }

    /* This is one we want - store its handle and current value */
    Chk_log.entry[Chk_log.num_entries].param[count].handle = 
                                           Params_table.param[index].handle;

    /* Update value associated with pointer */
    Get_ptr_value(&(Params_table.param[index]));

    /* Save this value */
    strcpy(Chk_log.entry[Chk_log.num_entries].param[count].value,
           Params_table.param[index].value);
    
    /* Storage for params associated with log entry is static */
    if(++count >= REG_MAX_NUM_STR_PARAMS)break;  
  }

  /* Store the no. of params this entry has */
  Chk_log.entry[Chk_log.num_entries].num_param = count;

  /* Keep a count of entries that we have yet to send to steerer */
  Chk_log.num_unsent++;

  Chk_log.num_entries++;

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Record_checkpoint_set(int   ChkType,
			  char *ChkTag,
			  char *Path)
{
#if REG_SOAP_STEERING
  int    nfiles;
  int    i, j, status, len;
  int    count = 0;
  int    nbytes, bytes_left;
  char **filenames;
  char  *cp_data;
  char   node_data[REG_MAX_MSG_SIZE];
  char  *pchar;
  char  *pTag;
  time_t time_now;
#endif

  /* Can only call this function if steering lib initialised */
  if (!ReG_SteeringInit) return REG_FAILURE;

  if (ChkType == REG_IODEF_HANDLE_NOTSET) return REG_SUCCESS;

#if !REG_SOAP_STEERING

  return Record_Chkpt(ChkType, ChkTag);

#else /* Steering via SGS so log checkpoints with SGS */

  /* Get list of checkpoint files */

  /* Calc. length of string - 'ls -1' and slashes add 9 chars so
     add a few more for safety.  Ask for 2*strlen(ChkTag) so that
     we can use the end of this buffer to hold the trimmed version
     of the tag. */
  len = strlen(Path) + 2*strlen(ChkTag) + strlen(ReG_CurrentDir) + 20;

  if( !(pchar = (char *)malloc(len)) ){

    fprintf(stderr, "Record_checkpoint_set: malloc of %d bytes failed\n",
	    len);
    return REG_FAILURE;
  }

  /* Set our pointer to the 'spare' bit at the end of the buffer
     we've just malloc'd */
  pTag = &(pchar[len - strlen(ChkTag) - 1]);

  /* Trim off leading space... */
  len = strlen(ChkTag);
  j = -1;
  for(i=0; i<len; i++){

    if(ChkTag[i] != ' '){
      j = i;
      break;
    }
  }

  if(j == -1){
    fprintf(stderr, "Record_checkpoint_set: ChkTag is blank\n");
    return REG_FAILURE;
  }

  /* Copy tag until first blank space - i.e. tag must not contain any
     spaces. */
  for(i=j; i<len; i++){
    if(ChkTag[i] == ' ')break;
    pTag[count++] = ChkTag[i];
  }
  pTag[count] = '\0';

  sprintf(pchar, "%s/%s/*%s*", ReG_CurrentDir, Path, pTag);

  filenames = NULL;
  status = Get_file_list(pchar, &nfiles, &filenames);
  free(pchar);

  if( (status != REG_SUCCESS) || !nfiles){

    fprintf(stderr, "Record_checkpoint_set: failed to find checkpoint "
	    "files with tag >%s<\n", ChkTag);
    return REG_FAILURE;
  }

  /* Construct checkpoint meta-data */
  cp_data = Global_scratch_buffer;
  pchar = cp_data;
  bytes_left = REG_SCRATCH_BUFFER_SIZE;
  nbytes = snprintf(pchar, bytes_left, "<Checkpoint_data application=\"%s\">\n"		    
		    "<Chk_type>%d</Chk_type>\n"
		    "<Chk_UID>%s</Chk_UID>\n"
		    "<Files location=\"%s\">\n", 
		    ReG_AppName, ChkType, ChkTag, ReG_Hostname);
  pchar += nbytes;
  bytes_left -= nbytes;

  for(i=0; i<nfiles; i++){
    
    nbytes = snprintf(pchar, bytes_left, "  <file type=\"gsiftp-URL\">\n"
		      "    gsiftp://%s%s\n    </file>\n",
		      ReG_Hostname, filenames[i]);

    /* Check for truncation */
    if((nbytes >= (bytes_left-1)) || (nbytes < 1)){
      fprintf(stderr, "Record_checkpoint_set: data exceeds %d chars\n", 
	      REG_SCRATCH_BUFFER_SIZE);
      return REG_FAILURE;
    }
    pchar += nbytes;
    bytes_left -= nbytes;
    free(filenames[i]);
  }
  free(filenames);

  nbytes = snprintf(pchar, bytes_left, "</Files>\n</Checkpoint_data>\n");
  pchar += nbytes;
  bytes_left -= nbytes;

  /* Store the values of all registered parameters at this point (so
     long as they're not internal to the library) */
  pchar = node_data;
  bytes_left = REG_MAX_MSG_SIZE;
  nbytes = snprintf(pchar, bytes_left, "<Checkpoint_node_data>\n");

  pchar += nbytes;
  bytes_left -= nbytes;

  for(i = 0; i<Params_table.max_entries; i++){

    if(Params_table.param[i].handle == REG_PARAM_HANDLE_NOTSET ||
       Params_table.param[i].is_internal == TRUE){

      /* Time stamp is a special case - is internal but we do want
	 it for checkpoint records */
      if(Params_table.param[i].handle != REG_TIMESTAMP_HANDLE){
	continue;
      }

      /* Get timestamp */
      if( (int)(time_now = time(NULL)) != -1){
	strcpy(Params_table.param[i].value, ctime(&time_now));
	/* Remove new-line character */
	Params_table.param[i].value[strlen(pchar)-1] = '\0';
      }
      else{
	sprintf(Params_table.param[i].value, "");
      }
    }

    /* Update value associated with pointer */
    Get_ptr_value(&(Params_table.param[i]));
    
    nbytes = snprintf(pchar, bytes_left, 
		      "<Param>\n"
		      "<Handle>%d</Handle>\n"
		      "<Label>%s</Label>\n"
		      "<Value>%s</Value>\n</Param>\n",
		      Params_table.param[i].handle,
		      Params_table.param[i].label,
		      Params_table.param[i].value);

    /* Check for truncation */
    if((nbytes >= (bytes_left-1)) || (nbytes < 1)){
      fprintf(stderr, "Record_checkpoint_set: node metadata "
	      "exceeds %d chars\n", REG_MAX_MSG_SIZE);
      return REG_FAILURE;
    }

    pchar += nbytes;
    bytes_left -= nbytes;
  }

  nbytes = snprintf(pchar, bytes_left, "</Checkpoint_node_data>\n");
  if((nbytes >= (bytes_left-1)) || (nbytes < 1)){
    fprintf(stderr, "Record_checkpoint_set: node metadata "
	    "exceeds %d chars\n", REG_MAX_MSG_SIZE);
    return REG_FAILURE;
  }

#if REG_DEBUG
  fprintf(stderr, "Record_checkpoint_set: node meta data >>%s<<\n",
	  node_data);
  fprintf(stderr, "Record_checkpoint_set: cp_data >>%s<<\n",
	  cp_data);
#endif

  /* Record checkpoint */
  Record_checkpoint_set_soap(cp_data, node_data);

  return REG_SUCCESS;

#endif /* REG_SOAP_STEERING */

}

/*----------------------------------------------------------------*/

int Open_log_file()
{
  char filename[REG_MAX_STRING_LENGTH];

  if(Chk_log.file_ptr){
    fclose(Chk_log.file_ptr);
  }
  sprintf(filename, "%s%s", Steerer_connection.file_root, 
          REG_LOG_FILENAME);
  Chk_log.file_ptr = fopen(filename, "a");

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Close_log_file()
{
  if(Chk_log.file_ptr){
    fclose(Chk_log.file_ptr);
    Chk_log.file_ptr = NULL;
  }

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Save_log()
{
  char *buf;
  int   len;

  if(!Chk_log.file_ptr){

    return REG_FAILURE;
  }

  /* Third argument says we want all entries - not just those that
     haven't already been sent to the steerer */
  if(Log_to_xml(&buf, &len, FALSE) == REG_SUCCESS){

    fprintf(Chk_log.file_ptr, "%s", buf);
  }

  Chk_log.num_entries = 0;

  if(buf){
    free(buf);
    buf = NULL;
  }

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Log_to_xml(char **pchar, int *count, const int not_sent_only)
{
  int   i, j;
  char  entry[BUFSIZ];
  char *pentry;
  char *pbuf;
  void *ptr;
  int   len;
  int   size = BUFSIZ;
  int   nbytes = 0;
  int   bytes_left;

  if( !(*pchar = (char *)malloc(size*sizeof(char))) ){

    fprintf(stderr, "Log_to_xml: malloc failed\n");
    return REG_FAILURE;
  }

  *count = 0;
  pbuf = *pchar;

  for(i=0; i<Chk_log.num_entries; i++){

    /* Check to see whether steerer already has this entry */
    if (not_sent_only && (Chk_log.entry[i].sent_to_steerer == TRUE)) continue;

    bytes_left = size;
    pentry = entry;
    nbytes = snprintf(pentry, bytes_left, "<Log_entry>\n"
		      "<Key>%d</Key>\n"
		      "<Chk_handle>%d</Chk_handle>\n"
		      "<Chk_tag>%s</Chk_tag>\n", 
		      Chk_log.entry[i].key, 
		      Chk_log.entry[i].chk_handle, 
		      Chk_log.entry[i].chk_tag);

#if REG_DEBUG
    /* Check for truncation of message */
    if((nbytes >= (bytes_left-1)) || (nbytes < 1)){
      fprintf(stderr, "Log_to_xml: message size exceeds BUFSIZ (%d)\n", 
	      BUFSIZ);
      free(*pchar);
      *pchar = NULL;
      return REG_FAILURE;
    }
#endif
    bytes_left -= nbytes;
    pentry += nbytes;

    /* Associated parameters are stored contiguously so need only
       loop over the no. of params that this entry has */
    for(j=0; j<Chk_log.entry[i].num_param; j++){

      nbytes = snprintf(pentry, bytes_left, "<Param>\n"
			"<Handle>%d</Handle>\n" 
			"<Value>%s</Value>\n"
			"</Param>\n", 
		        Chk_log.entry[i].param[j].handle,
		        Chk_log.entry[i].param[j].value);

#if REG_DEBUG
      /* Check for truncation of message */
      if((nbytes >= (bytes_left-1)) || (nbytes < 1)){
	fprintf(stderr, "Log_to_xml: message size exceeds BUFSIZ (%d)\n", 
		BUFSIZ);
	free(*pchar);
	*pchar = NULL;
	return REG_FAILURE;
      }
#endif
      bytes_left -= nbytes;
      pentry += nbytes;
    }
    
    nbytes = snprintf(pentry, bytes_left, "</Log_entry>\n");
#if REG_DEBUG
    /* Check for truncation of message */
    if((nbytes >= (bytes_left-1)) || (nbytes < 1)){
      fprintf(stderr, "Log_to_xml: message size exceeds BUFSIZ (%d)\n", 
	      BUFSIZ);
      free(*pchar);
      *pchar = NULL;
      return REG_FAILURE;
    }
#endif
    bytes_left -= nbytes;
    pentry += nbytes;
    
    len = strlen(entry);

    if( (size - *count) < len){

      size += ((len/BUFSIZ) + 1)*BUFSIZ;
      if( !(ptr = realloc(*pchar, size*sizeof(char))) ){

	fprintf(stderr, "Log_to_xml: realloc failed\n");
	free(*pchar);
	*pchar = NULL;
	return REG_FAILURE;
      }
      
      *pchar = (char *)ptr;
      pbuf = &((*pchar)[*count]);
    }

    strncpy(pbuf, entry, len);
    pbuf += len;
    *count += len;

    /* Flag this entry as having been sent to steerer */
    Chk_log.entry[i].sent_to_steerer = TRUE;
  }

  *pbuf = 0;

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Set_log_primary_key()
{
  int   size;
  int   return_status = REG_SUCCESS;
  char *pbuf = NULL;
  char *ptr = NULL;
  char *old_ptr = NULL;
  char  filename[REG_MAX_STRING_LENGTH];

  Close_log_file();

  /* Read the log file and get back contents in buffer pointed
     to by pbuf.  We must free() this once we're done. */

  /* Added by Jens, 09.04.03: */
  sprintf(filename, "%s%s", Steerer_connection.file_root,
		            REG_LOG_FILENAME);

  if(Read_file(filename, &pbuf, &size) != REG_SUCCESS){

    Chk_log.primary_key_value = 0;
    if(pbuf)free(pbuf);
    return REG_SUCCESS;
  }

  if(size == 0){

    /* Log file existed but was empty */
    Chk_log.primary_key_value = 0;
    if(pbuf)free(pbuf);
    return REG_SUCCESS;
  }

  ptr = pbuf;
  while(ptr = strstr((ptr+1), "<Key>")){

    old_ptr = ptr;
  }

#if REG_DEBUG
  fprintf(stderr, "Set_log_primary_key: last chunk = >>%s<<\n", old_ptr);
#endif

  if( 1 != sscanf(old_ptr, "<Key>%d</Key>", &(Chk_log.primary_key_value))){

    Chk_log.primary_key_value = 0;
    return_status = REG_FAILURE;
  }
  else{
    Chk_log.primary_key_value++;
  }

  free(pbuf);
  Open_log_file();

  return return_status;
}

/*----------------------------------------------------------------*/

int Consume_start(int  IOType,
		  int *IOTypeIndex)
{
  /* Check that steering is enabled */
  if(!ReG_SteeringEnabled) return REG_SUCCESS;

  /* Can only call this function if steering lib initialised */
  if (!ReG_SteeringInit) return REG_FAILURE;

  /* Find corresponding entry in table of IOtypes */
  *IOTypeIndex = IOdef_index_from_handle(&IOTypes_table, IOType);
  if(*IOTypeIndex == REG_IODEF_HANDLE_NOTSET){
    fprintf(stderr, "Consume_start: failed to find matching IOType, "
	    "handle = %d\n", IOType);
    return REG_FAILURE;
  }

  /* Check that this IOType is enabled */
  if(IOTypes_table.io_def[*IOTypeIndex].is_enabled == FALSE){
    return REG_FAILURE;
  }

  /* Check that this IOType can be consumed */
  if(IOTypes_table.io_def[*IOTypeIndex].direction == REG_IO_OUT){

    fprintf(stderr, "Consume_start: ERROR - IOType has direction REG_IO_OUT\n");
    return REG_FAILURE;
  }

  /* Initialise array-ordering flags */
  IOTypes_table.io_def[*IOTypeIndex].convert_array_order = FALSE;

  return Consume_start_data_check(*IOTypeIndex);
}

/*----------------------------------------------------------------*/

int Consume_stop(int *IOTypeIndex)
{
  int return_status = REG_SUCCESS;

  /* Check that steering is enabled */
  if(!ReG_SteeringEnabled) return REG_SUCCESS;

  /* Can only call this function if steering lib initialised */
  if (!ReG_SteeringInit) return REG_FAILURE;

  if(*IOTypeIndex < 0 || *IOTypeIndex >= IOTypes_table.num_registered){

    fprintf(stderr, "Consume_stop: IOType index out of range\n");
    return REG_FAILURE;
  }

  /* Check that this IOType is enabled */
  if(IOTypes_table.io_def[*IOTypeIndex].is_enabled == FALSE){
    return REG_FAILURE;
  }

#if !REG_SOCKET_SAMPLES
  /* Close any file associated with this channel */
  if(IOTypes_table.io_def[*IOTypeIndex].fp){
    fclose(IOTypes_table.io_def[*IOTypeIndex].fp);
    IOTypes_table.io_def[*IOTypeIndex].fp = NULL;
    remove(IOTypes_table.io_def[*IOTypeIndex].filename);
  }
#endif

  /* Free memory associated with channel */
  if( IOTypes_table.io_def[*IOTypeIndex].buffer ){
    free(IOTypes_table.io_def[*IOTypeIndex].buffer);
    IOTypes_table.io_def[*IOTypeIndex].buffer = NULL;
    IOTypes_table.io_def[*IOTypeIndex].buffer_bytes = 0;
  }

  /* Reset handle associated with channel */
  *IOTypeIndex = REG_IODEF_HANDLE_NOTSET;

  return return_status;
}

/*----------------------------------------------------------------*/

int Consume_data_slice_header(int  IOTypeIndex,
			      int *DataType,
			      int *Count)
{
  int status;
  int NumBytes;
  int IsFortranArray;

  /* Check that steering is enabled */
  if(!ReG_SteeringEnabled) return REG_SUCCESS;

  /* Check that this IOType is enabled */
  if(IOTypes_table.io_def[IOTypeIndex].is_enabled == FALSE){
    return REG_FAILURE;
  }

  status = Consume_iotype_msg_header(IOTypeIndex,
				     DataType,
				     Count,
				     &NumBytes,
				     &IsFortranArray);

  if(status != REG_SUCCESS) return REG_FAILURE;

  /* Use of XDR is internal to library so make sure user doesn't
     get confused.  use_xdr flag set here for use in subsequent call
     to consume_data_slice */
  switch(*DataType){

  case REG_XDR_INT:
    IOTypes_table.io_def[IOTypeIndex].use_xdr = TRUE;
    IOTypes_table.io_def[IOTypeIndex].num_xdr_bytes = NumBytes;
    *DataType = REG_INT;
    break;

  case REG_XDR_FLOAT:
    IOTypes_table.io_def[IOTypeIndex].use_xdr = TRUE;
    IOTypes_table.io_def[IOTypeIndex].num_xdr_bytes = NumBytes;
    *DataType = REG_FLOAT;
    break;

  case REG_XDR_DOUBLE:
    IOTypes_table.io_def[IOTypeIndex].use_xdr = TRUE;
    IOTypes_table.io_def[IOTypeIndex].num_xdr_bytes = NumBytes;
    *DataType = REG_DBL;
    break;

  default:
    IOTypes_table.io_def[IOTypeIndex].use_xdr = FALSE;
    break;
  }

  /* Check whether or not we'll need to convert the array ordering *
  if(ReG_CalledFromF90 != IsFortranArray){

    * Assume we don't ever want to re-order char data *
    if(*DataType != REG_CHAR){
      IOTypes_table.io_def[IOTypeIndex].convert_array_order = TRUE;
    }
    else{
      IOTypes_table.io_def[IOTypeIndex].convert_array_order = FALSE;
    }
  }
  else{
    IOTypes_table.io_def[IOTypeIndex].convert_array_order = FALSE;
  }
  */

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Consume_data_slice(int    IOTypeIndex,
		       int    DataType,
		       int    Count,
		       void  *pData)
{
  int              return_status = REG_SUCCESS;
  size_t	   num_bytes_to_read;

  /* Check that steering is enabled */
  if(!ReG_SteeringEnabled) return REG_SUCCESS;

  /* Check that this IOType is enabled */
  if(IOTypes_table.io_def[IOTypeIndex].is_enabled == FALSE){
    return REG_FAILURE;
  }

  /* Calculate how many bytes to expect */
  switch(DataType){

  case REG_INT:
    if(IOTypes_table.io_def[IOTypeIndex].use_xdr){
/*       num_bytes_to_read = Count*REG_SIZEOF_XDR_INT; */
      num_bytes_to_read = IOTypes_table.io_def[IOTypeIndex].num_xdr_bytes;
    }
    else{
      num_bytes_to_read = Count*sizeof(int);
    }
    break;

  case REG_FLOAT:
    if(IOTypes_table.io_def[IOTypeIndex].use_xdr){
/*       num_bytes_to_read = Count*REG_SIZEOF_XDR_FLOAT; */
      num_bytes_to_read = IOTypes_table.io_def[IOTypeIndex].num_xdr_bytes;
    }
    else{
      num_bytes_to_read = Count*sizeof(float);
    }
    break;

  case REG_DBL:
    if(IOTypes_table.io_def[IOTypeIndex].use_xdr){
/*       num_bytes_to_read = Count*REG_SIZEOF_XDR_DOUBLE; */
      num_bytes_to_read = IOTypes_table.io_def[IOTypeIndex].num_xdr_bytes;
    }
    else{
      num_bytes_to_read = Count*sizeof(double);
    }
    break;

  case REG_CHAR:
    num_bytes_to_read = Count*sizeof(char);
    break;

  default:
    fprintf(stderr, "Consume_data_slice: Unrecognised data type specified "
	    "in slice header\n");

    /* Reset use_xdr flag set as only valid on a per-slice basis */
    IOTypes_table.io_def[IOTypeIndex].use_xdr = FALSE;
    IOTypes_table.io_def[IOTypeIndex].num_xdr_bytes = 0;

    return REG_FAILURE;
    break;
  }

  /* Check that input buffer is large enough (only an issue if have XDR-
     encoded data or need to reorder it) */
  if(IOTypes_table.io_def[IOTypeIndex].use_xdr ||
     IOTypes_table.io_def[IOTypeIndex].convert_array_order == TRUE){

    if(IOTypes_table.io_def[IOTypeIndex].buffer_bytes < num_bytes_to_read){

      if(Realloc_iotype_buffer(IOTypeIndex, num_bytes_to_read) 
	 != REG_SUCCESS){

	/* Reset use_xdr flag set as only valid on a per-slice basis */
	IOTypes_table.io_def[IOTypeIndex].use_xdr = FALSE;
	IOTypes_table.io_def[IOTypeIndex].num_xdr_bytes = 0;
	return REG_FAILURE;
      }
    }
  }

  /* Read this number of bytes - if xdr is being used or the array needs to
     be reordered then the data is read into
     IOTypes_table.io_def[IOTypeIndex].buffer, else it is stored
     in the buffer pointed to by pData. */
  if (Consume_data_read(IOTypeIndex,
			DataType,
			num_bytes_to_read,
			pData) != REG_SUCCESS)
    return REG_FAILURE;


  /* Re-order and decode (xdr) data as necessary - CURRENTLY
     ONLY DECODES.*/
  Reorder_decode_array(&(IOTypes_table.io_def[IOTypeIndex]), 
		       DataType, Count,  pData);

  /* Reset use_xdr flag set as only valid on a per-slice basis */
  IOTypes_table.io_def[IOTypeIndex].use_xdr = FALSE;
  IOTypes_table.io_def[IOTypeIndex].num_xdr_bytes = 0;

  return return_status;
}

/*----------------------------------------------------------------*/

int Emit_start(int  IOType,
	       int  SeqNum,
	       int *IOTypeIndex)
{
#if !REG_SOCKET_SAMPLES
  char *pchar;
  int   len;
#endif

  /* Check that steering is enabled */
  if(!ReG_SteeringEnabled) return REG_SUCCESS;

  /* Can only call this function if steering lib initialised */
  if (!ReG_SteeringInit) return REG_FAILURE;

  /* Find corresponding entry in table of IOtypes */
  *IOTypeIndex = IOdef_index_from_handle(&IOTypes_table, IOType);
  if(*IOTypeIndex == REG_IODEF_HANDLE_NOTSET){

    fprintf(stderr, "Emit_start: failed to find matching IOType\n");
    return REG_FAILURE;
  }

  /* Check that this IOType is enabled */
  if(IOTypes_table.io_def[*IOTypeIndex].is_enabled == FALSE){
    return REG_FAILURE;
  }

  /* Check that this IOType can be emitted */
  if(IOTypes_table.io_def[*IOTypeIndex].direction == REG_IO_IN){

    fprintf(stderr, "Emit_start: ERROR - IOType has direction REG_IO_IN\n");
    return REG_FAILURE;
  }

  /* Set whether or not to encode as XDR */
  IOTypes_table.io_def[*IOTypeIndex].use_xdr = TRUE;

  /* Initialise array-ordering flags */
  IOTypes_table.io_def[*IOTypeIndex].convert_array_order = FALSE;

#if !REG_SOCKET_SAMPLES

  /* Currently have no way of looking up what filename to use so 
     hardwire... */

  len = strlen(IOTypes_table.io_def[*IOTypeIndex].directory) +
    strlen(IOTypes_table.io_def[*IOTypeIndex].label);

  if(len > REG_MAX_STRING_LENGTH){

    fprintf(stderr, "Emit_start: combination of filename + "
	    "directory path exceeds %d characters: increase "
	    "REG_MAX_STRING_LENGTH\n", REG_MAX_STRING_LENGTH);
    return REG_FAILURE;
  }

  /* In the short term, use the label as the filename */
  sprintf(IOTypes_table.io_def[*IOTypeIndex].filename, 
	  "%s%s", IOTypes_table.io_def[*IOTypeIndex].directory,
	  IOTypes_table.io_def[*IOTypeIndex].label);

  /* Remove trailing white space */
  while(IOTypes_table.io_def[*IOTypeIndex].filename[--len] == ' ');

  /* Terminate string and correct length of string (since final
     character we looked at wasn't actually blank) */
  IOTypes_table.io_def[*IOTypeIndex].filename[++len] = '\0';

  /* Replace any spaces with '_' */
  pchar = strchr(IOTypes_table.io_def[*IOTypeIndex].filename, ' ');
  while( pchar && ((pchar - IOTypes_table.io_def[*IOTypeIndex].filename + 1) < len) ){
    *pchar = '_';
    pchar = strchr(++pchar,' ');
  }

  pchar = IOTypes_table.io_def[*IOTypeIndex].filename;
  pchar += strlen(IOTypes_table.io_def[*IOTypeIndex].filename);

  sprintf(pchar, "_%d", SeqNum);
  if( !(IOTypes_table.io_def[*IOTypeIndex].fp = 
	fopen(IOTypes_table.io_def[*IOTypeIndex].filename, "w")) ){

    fprintf(stderr, "Emit_start: failed to open file %s\n", 
	    IOTypes_table.io_def[*IOTypeIndex].filename);
    return REG_FAILURE;
  }
#endif

  return Emit_header(*IOTypeIndex);
}

/*----------------------------------------------------------------*/

int Emit_stop(int *IOTypeIndex)
{
  int             return_status = REG_SUCCESS;

  /* Check that steering is enabled */
  if(!ReG_SteeringEnabled) return REG_SUCCESS;

  /* Can only call this function if steering lib initialised */
  if (!ReG_SteeringInit) return REG_FAILURE;

  /* Check that this IOType is enabled */
  if(IOTypes_table.io_def[*IOTypeIndex].is_enabled == FALSE){
    return REG_FAILURE;
  }

  /* Send footer */
  sprintf(Global_scratch_buffer, REG_PACKET_FORMAT, REG_DATA_FOOTER);
  /* Include termination char WITHIN the packet */
  Global_scratch_buffer[REG_PACKET_SIZE-1] = '\0';

  return_status = Emit_footer(*IOTypeIndex, Global_scratch_buffer);

#if !REG_SOCKET_SAMPLES
  if(IOTypes_table.io_def[*IOTypeIndex].fp){
    fclose(IOTypes_table.io_def[*IOTypeIndex].fp);
    IOTypes_table.io_def[*IOTypeIndex].fp = NULL;
  }
  /* Create lock file for this data file to prevent race 
     conditions */
  Create_lock_file(IOTypes_table.io_def[*IOTypeIndex].filename);
#endif

  *IOTypeIndex = REG_IODEF_HANDLE_NOTSET;

  return return_status;
}

/*----------------------------------------------------------------*/

int Emit_data_slice(int		      IOTypeIndex,
		    int               DataType,
		    int               Count,
		    const void       *pData)
{
  int              datatype;
  int              actual_count;
  size_t	   num_bytes_to_send;
  XDR              xdrs;
  char            *pChar;
  void            *out_ptr;

  /* Check that steering is enabled */
  if(!ReG_SteeringEnabled) return REG_SUCCESS;

  /* Can only call this function if steering lib initialised */
  if (!ReG_SteeringInit) return REG_FAILURE;

  /* check comms connection has been made */
  if (Get_communication_status(IOTypeIndex) !=  REG_SUCCESS)
    return REG_FAILURE;

  /* Check that this IOType is enabled */
  if(IOTypes_table.io_def[IOTypeIndex].is_enabled == FALSE){
    return REG_FAILURE;
  }

  actual_count = Count;

  /* Check data type, calculate number of bytes to send and convert
     to XDR if required */
  switch(DataType){

  case REG_INT:
    if(IOTypes_table.io_def[IOTypeIndex].use_xdr){
      datatype = REG_XDR_INT;
      num_bytes_to_send = actual_count*REG_SIZEOF_XDR_INT;

      if(num_bytes_to_send > IOTypes_table.io_def[IOTypeIndex].buffer_bytes){

	if(Realloc_iotype_buffer(IOTypeIndex, num_bytes_to_send) 
	   != REG_SUCCESS){
	  return REG_FAILURE;
	}
      }

      xdrmem_create(&xdrs, 
		    IOTypes_table.io_def[IOTypeIndex].buffer,
		    num_bytes_to_send,
		    XDR_ENCODE);
      xdr_vector(&xdrs, (char *)pData, (unsigned int)actual_count, 
		 (unsigned int)sizeof(int), (xdrproc_t)xdr_int);

      num_bytes_to_send = (int)xdr_getpos(&xdrs);
      out_ptr = IOTypes_table.io_def[IOTypeIndex].buffer;

      xdr_destroy(&xdrs);
    }
    else{
      datatype = DataType;
      num_bytes_to_send = actual_count*sizeof(int);
      out_ptr = (void *)pData;
    }
    break;

  case REG_FLOAT:
    if(IOTypes_table.io_def[IOTypeIndex].use_xdr){
      datatype = REG_XDR_FLOAT;
      num_bytes_to_send = actual_count*REG_SIZEOF_XDR_FLOAT;

      if(num_bytes_to_send > IOTypes_table.io_def[IOTypeIndex].buffer_bytes){

	if(Realloc_iotype_buffer(IOTypeIndex, num_bytes_to_send) 
	   != REG_SUCCESS){
	  return REG_FAILURE;
	}
      }

      xdrmem_create(&xdrs, 
		    IOTypes_table.io_def[IOTypeIndex].buffer,
		    num_bytes_to_send,
		    XDR_ENCODE);
      xdr_vector(&xdrs, (char *)pData, (unsigned int)actual_count, 
		 (unsigned int)sizeof(float), (xdrproc_t)xdr_float);

      num_bytes_to_send = (int)xdr_getpos(&xdrs);
      out_ptr = IOTypes_table.io_def[IOTypeIndex].buffer;

      xdr_destroy(&xdrs);
    }
    else{
      datatype = DataType;
      num_bytes_to_send = actual_count*sizeof(float);
      out_ptr = (void *)pData;
    }
    break;

  case REG_DBL:
    if(IOTypes_table.io_def[IOTypeIndex].use_xdr){
      datatype = REG_XDR_DOUBLE;
      num_bytes_to_send = actual_count*REG_SIZEOF_XDR_DOUBLE;

      if(num_bytes_to_send > IOTypes_table.io_def[IOTypeIndex].buffer_bytes){

	/* This function will malloc if buffer not already set */
	if(Realloc_iotype_buffer(IOTypeIndex, num_bytes_to_send) 
	   != REG_SUCCESS){
	  return REG_FAILURE;
	}
      }

      xdrmem_create(&xdrs, 
		    IOTypes_table.io_def[IOTypeIndex].buffer,
		    num_bytes_to_send,
		    XDR_ENCODE);
      xdr_vector(&xdrs, (char *)pData, (unsigned int)actual_count, 
		 (unsigned int)sizeof(double), (xdrproc_t)xdr_double);

      num_bytes_to_send = (int)xdr_getpos(&xdrs);
      out_ptr = IOTypes_table.io_def[IOTypeIndex].buffer;

      xdr_destroy(&xdrs);
    }
    else{
      datatype = DataType;
      num_bytes_to_send = actual_count*sizeof(double);
      out_ptr = (void *)pData;
    }
    break;

  case REG_CHAR:
    datatype = DataType;

    /* Ensure we send termination character */
    pChar = (char *)pData;
    if(pChar[actual_count-1] != '\0'){
      /* This could lead to string truncation - should copy string
         and add termination at end ARPDBG */ 
      pChar[actual_count-1] = '\0';
      /* actual_count++; */
    }
    num_bytes_to_send = actual_count*sizeof(char);
    out_ptr = (void *)pData;
    break;

  default:
    fprintf(stderr, "Emit_data_slice: Unrecognised data type\n");
    return REG_FAILURE;
    break;
  }
 
  /* Send ReG-specific header */

  if( Emit_iotype_msg_header(IOTypeIndex,
			     datatype,
			     actual_count,
			     num_bytes_to_send,
			     ReG_CalledFromF90)!= REG_SUCCESS){
    return REG_FAILURE;
  }

  /* Send data */
  return Emit_data(IOTypeIndex,
		   datatype,
		   num_bytes_to_send,
		   out_ptr);
}

/*----------------------------------------------------------------*/

int Register_params(int    NumParams,
		    char* *ParamLabels,
		    int   *ParamSteerable,
		    void **ParamPtrs,
		    int   *ParamTypes,
		    char* *ParamMinima,
		    char* *ParamMaxima)
{
  int    i;
  int    current;
  int    dum_int;
  float  dum_flt;
  double dum_dbl;

  /* Check that steering is enabled */

  if(!ReG_SteeringEnabled) return REG_SUCCESS;

  /* Can only call this function if steering lib initialised */

  if (!ReG_SteeringInit) return REG_FAILURE;

  for(i=0; i<NumParams; i++){

    /* Find next free entry - allocates more memory if required */
    current = Next_free_param_index(&Params_table);

    if(current == -1){

      fprintf(stderr, "Register_params: failed to find free "
	      "param entry\n");
      return REG_FAILURE;
    }

    if(String_contains_xml_chars(ParamLabels[i]) == TRUE){

      fprintf(stderr, "Register_params: ERROR: Param label "
	      "contains reserved xml characters (<,>,&): %s\n"
              "     - skipping this parameter.\n", ParamLabels[i]);
      continue;
    }

    /* Store label */
    strncpy(Params_table.param[current].label, 
	    ParamLabels[i],
	    REG_MAX_STRING_LENGTH);

    /* Store 'steerable' */
    Params_table.param[current].steerable = ParamSteerable[i];
    
    /* Store pointer */
    Params_table.param[current].ptr = ParamPtrs[i];

    /* Store type */
    Params_table.param[current].type = ParamTypes[i];

    /* This set to TRUE external to this routine if this param.
       has been created by the steering library itself */
    Params_table.param[current].is_internal = FALSE;

    /* Range of validity for this parameter - assume invalid 
       first and check second */
    Params_table.param[current].min_val_valid = FALSE;
    Params_table.param[current].max_val_valid = FALSE;
    switch(ParamTypes[i]){

    case REG_INT:
      if(sscanf(ParamMinima[i], "%d", &dum_int) == 1){
	Params_table.param[current].min_val_valid = TRUE;
      }
      if(sscanf(ParamMaxima[i], "%d", &dum_int) == 1){
	Params_table.param[current].max_val_valid = TRUE;
      }
      break;

    case REG_FLOAT:
      if(sscanf(ParamMinima[i], "%f", &dum_flt) == 1){
	Params_table.param[current].min_val_valid = TRUE;
      }
      if(sscanf(ParamMaxima[i], "%f", &dum_flt) == 1){
	Params_table.param[current].max_val_valid = TRUE;
      }
      break;

    case REG_DBL:
      if(sscanf(ParamMinima[i], "%lf", &dum_dbl) == 1){
	Params_table.param[current].min_val_valid = TRUE;
      }
      if(sscanf(ParamMaxima[i], "%lf", &dum_dbl) == 1){
	Params_table.param[current].max_val_valid = TRUE;
      }
      break;

    case REG_CHAR:
      /* Limits are taken as lengths for a string */
      if(sscanf(ParamMinima[i], "%d", &dum_int) == 1){
	Params_table.param[current].min_val_valid = TRUE;
      }
      if(sscanf(ParamMaxima[i], "%d", &dum_int) == 1){
	Params_table.param[current].max_val_valid = TRUE;
      }
      break;

    default:
      fprintf(stderr, "Register_params: unrecognised parameter "
	      "type - skipping parameter >%s<\n", ParamLabels[i]);
      continue;
    }
    if(Params_table.param[current].min_val_valid == TRUE){
      strncpy(Params_table.param[current].min_val, ParamMinima[i],
	      REG_MAX_STRING_LENGTH);
    }
    else{
      sprintf(Params_table.param[current].min_val, " ");
    }

    if(Params_table.param[current].max_val_valid == TRUE){
      strncpy(Params_table.param[current].max_val, ParamMaxima[i],
	      REG_MAX_STRING_LENGTH);
    }
    else{
      sprintf(Params_table.param[current].max_val, " ");
    }

    /* Create handle for this parameter */
    Params_table.param[current].handle = Params_table.next_handle++;

    Params_table.num_registered++;
  }

  /* Flag that the registered parameters have changed */
  ReG_ParamsChanged = TRUE;

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Steering_control(int     SeqNum,
		     int    *NumSteerParams,
		     char  **SteerParamLabels,
		     int    *NumSteerCommands,
		     int    *SteerCommands,
		     char  **SteerCmdParams)
{
  int    i;
  int    status;
  int    do_steer;
  int    detached;
  int    count         = 0;
  int    return_status = REG_SUCCESS;
  int    num_commands;
  int    commands[REG_MAX_NUM_STR_CMDS];
  int    param_handles[REG_MAX_NUM_STR_PARAMS];
  char*  param_labels[REG_MAX_NUM_STR_PARAMS];

  /* Indices to save having to keep looking-up handles */
  static int     step_time_index = 0;
  static int     seq_num_index   = -1;

  /* Variables for timing */
  float          time_per_step;
  clock_t        new_time;
  static clock_t previous_time   = 0;
  static int     first_time      = TRUE;

  /* For throttling the polling rate */
  static time_t this_wc_time = (time_t)0, last_wc_time = (time_t)0;

#ifdef USE_REG_TIMING
  /* More accurate timing but less portable so only used
     for analysis */
  static float  sim_time, steer_time;
  static double time0 = -1.0, time1 = -1.0;

  Get_current_time_seconds(&time0);

  if(time1 > -1.0){
    sim_time = (float)(time0 - time1);
    fprintf(stderr, "TIMING: Spent %.5f seconds working\n",
	    sim_time);
    if(sim_time > REG_TOL_ZERO){
      fprintf(stderr, "TIMING: Steering overhead = %.3f%%\n", 
	      100.0*(steer_time/sim_time));
    }
  }
#endif

  *NumSteerParams   = 0;
  *NumSteerCommands = 0;

  /* Check that steering is enabled */

  if(!ReG_SteeringEnabled) return REG_SUCCESS;

  /* Can only call this function if steering lib initialised */

  if (!ReG_SteeringInit) return REG_FAILURE;

  /* Check to see if a steerer is trying to get control */

  if(!ReG_SteeringActive){

    /* To minimise steering overhead while no steerer is connected
       we only check for a connection at intervals greater than
       Steerer_connection.polling_interval seconds. */
    this_wc_time = time(NULL);
    if(difftime(this_wc_time, last_wc_time) > 
       Steerer_connection.polling_interval){

      last_wc_time = this_wc_time;
      if(Steerer_connected() == REG_SUCCESS){

	ReG_SteeringActive = TRUE;
	first_time = TRUE;
#if REG_DEBUG
	fprintf(stderr, "Steering_control: steerer has connected\n");
#endif
      }
    }
  }

  /* Throttle how often we perform steering-related activity - for
     use when a simulation step is v. short */
  do_steer = ((SeqNum % Steerer_connection.steer_interval) == 0);

  /* If we're not steering via SOAP (and a Steering Grid Service)
     then we can't emit our parameter definitions etc. until
     a steerer has connected */
#if !REG_SOAP_STEERING
  if(ReG_SteeringActive && do_steer){
#endif /* !REG_SOAP_STEERING */

    /* If registered params have changed since the last time then
       tell the steerer about the current set */
    if(ReG_ParamsChanged){
      
      if(Emit_param_defs() != REG_SUCCESS){

	fprintf(stderr, "Steering_control: Emit_param_defs "
		"failed\n");
      }
#if REG_DEBUG
      fprintf(stderr, "Steering_control: done Emit_param_defs\n");
#endif
      ReG_ParamsChanged  = FALSE;
    }

    /* If the registered IO types have changed since the last time
       then tell the steerer about the current set */
    if(ReG_IOTypesChanged){

      Emit_IOType_defs();

#if REG_DEBUG
      fprintf(stderr, "Steering_control: done Emit_IOType_defs\n");
#endif
      ReG_IOTypesChanged = FALSE;
    }

    /* If the registered Chk types have changed since the last time
       then tell the steerer about the current set */
    if(ReG_ChkTypesChanged){

      Emit_ChkType_defs();

#if REG_DEBUG
      fprintf(stderr, "Steering_control: done Emit_ChkType_defs\n");
#endif
      ReG_ChkTypesChanged = FALSE;
    }

    /* If we are steering via SOAP (and associated Steering Grid
       Service) then we can and should publish our parameter
       definitions etc., irrespective of whether a steerer is
       attached */
#if REG_SOAP_STEERING
  if(ReG_SteeringActive && do_steer){
#endif

    /* If we're being steered (no matter how) then... */

    /* Update any library-controlled monitored variables */
    if(seq_num_index != -1){
      sprintf(Params_table.param[seq_num_index].value, "%d", SeqNum);
      Params_table.param[seq_num_index].modified = TRUE;
    }
    else{
      seq_num_index = Param_index_from_handle(&(Params_table), 
					      REG_SEQ_NUM_HANDLE);
      if(seq_num_index != -1){
	sprintf(Params_table.param[seq_num_index].value, "%d", SeqNum);
	Params_table.param[seq_num_index].modified = TRUE;
      }
    }

    /* Calculate CPU time used since last call */
    if(step_time_index != -1){

      new_time = clock();
      time_per_step = (float)(new_time - previous_time)/(float)(CLOCKS_PER_SEC*Steerer_connection.steer_interval);
      previous_time = new_time;

      /* First value we get will be rubbish because need two passes 
	 through to get a valid difference... */
      if(!first_time){
	sprintf(Params_table.param[step_time_index].value, "%.3f", 
		time_per_step);
        Params_table.param[step_time_index].modified = TRUE;
      }
      else{
	step_time_index = Param_index_from_handle(&(Params_table), 
						  REG_STEP_TIME_HANDLE);
	first_time = FALSE;
      }    
    }

    /* Read anything that the steerer has sent to us */
    if( Consume_control(&num_commands,
			commands,
			SteerCmdParams,
			NumSteerParams,
			param_handles,
			param_labels) != REG_SUCCESS ){

      return_status = REG_FAILURE;

#if REG_DEBUG
      fprintf(stderr, "Steering_control: call to Consume_control failed\n");
#endif
    }

    /* Emit logging info. */
    if( Emit_log() != REG_SUCCESS ){

      fprintf(stderr, "Steering_control: Emit_log failed\n");
    }
#if REG_DEBUG
    else{
      fprintf(stderr, "Steering_control: done Emit_log\n");
    }
#endif

    /* Set array holding labels of changed params - pass back strings 
       rather than pointers to strings */

    for(i=0; i<(*NumSteerParams); i++){

      strcpy(SteerParamLabels[i], param_labels[i]);
    }

#if REG_DEBUG
    fprintf(stderr, "Steering_control: done Consume_control\n");
#endif

    /* Parse list of commands for any that we can handle ourselves */

    i     = 0;
    detached = FALSE;

    while(i<num_commands){

      switch(commands[i]){

      case REG_STR_DETACH:

#if REG_DEBUG
        fprintf(stderr, "Steering_control: got detach command\n");
#endif

	if( Detach_from_steerer() != REG_SUCCESS){

	  return_status = REG_FAILURE;
	}

#if !REG_SOAP_STEERING
	/* Confirm that we have received the detach command */
	commands[0] = REG_STR_DETACH;
	Emit_status(SeqNum,
		    0,   
		    NULL,
		    1,
		    commands);
#endif

        detached = TRUE;
	break;

      default:

#if REG_DEBUG
        fprintf(stderr, "Steering_control: got command %d\n", commands[i]);
#endif

        SteerCommands[count] = commands[i];
	strcpy(SteerCmdParams[count], SteerCmdParams[i]);
	count++;

	/* If we've received a stop command then do just that - don't
	   mess about */

	if(commands[i] == REG_STR_STOP){

	  /* If we are being steered using soap then don't emit
	     a confirmation - Steering_finalize takes care of this */
#if !REG_SOAP_STEERING
	  /* Confirm that we have received the stop command */
	  commands[0] = REG_STR_STOP;
          Emit_status(SeqNum,
		      0,   
		      NULL,
		      1,
		      commands);
#endif

	  detached = TRUE;
	}

	break;
      }

      /* If we get a 'detach' command then don't process anything
         else */
      if(detached)break;

      i++;
    }

    /* Tell the steerer what we've been doing */
    if( !detached ){

      /* Currently don't support returning a copy of the data just 
	 received from the steerer - hence NULL's below */
      status = Emit_status(SeqNum,
			   0,    /* *NumSteerParams, */
			   NULL, /* param_handles,   */
			   *NumSteerCommands,
			   commands);

      if(status != REG_SUCCESS){

	fprintf(stderr, "Steering_control: call to Emit_status failed\n");
	return_status = REG_FAILURE;
      }
    }
  } /* End if steering active */

  /* Deal with automatic emission/consumption of data - this is done
     whether or not a steering client is connected */
  Auto_generate_steer_cmds(SeqNum, &count, SteerCommands, SteerCmdParams);

  /* Record how many commands we're going to pass back to caller */
  *NumSteerCommands = count;

#ifdef USE_REG_TIMING
  Get_current_time_seconds(&time1);
  steer_time = (float)(time1 - time0);

  fprintf(stderr, "TIMING: Spent %.5f seconds in Steering_control\n",
	  steer_time);
#endif

  return return_status;
}

/*----------------------------------------------------------------*/

int Auto_generate_steer_cmds(int    SeqNum,
			     int   *posn, 
			     int   *SteerCommands, 
			     char **SteerCmdParams)
{
  int i;
  int return_status = REG_SUCCESS;

  /* IOTypes cannot be deleted so the num_registered entries that
     we have will be contiguous within the table */
  for(i=0; i<IOTypes_table.num_registered; i++){

    /* A freq. of zero indicates no automatic emit/consume */
    if(IOTypes_table.io_def[i].frequency == 0) continue;

    if((SeqNum % IOTypes_table.io_def[i].frequency) != 0)continue;

    if( *posn >= REG_MAX_NUM_STR_CMDS ){
      fprintf(stderr, "Auto_generate_steer_cmds: WARNING: discarding "
	      "steering cmds as max number (%d) exceeded\n", 
	      REG_MAX_NUM_STR_CMDS);

      return_status = REG_FAILURE;
      break;
    }
 
    /* Add command to list to send back to caller */
    SteerCommands[*posn]  = IOTypes_table.io_def[i].handle;

    switch(IOTypes_table.io_def[i].direction){

    case REG_IO_IN:
      sprintf(SteerCmdParams[*posn], "IN");
      break;

    case REG_IO_OUT:
    case REG_IO_INOUT:
      sprintf(SteerCmdParams[*posn], "OUT");
      break;

    default:
      sprintf(SteerCmdParams[*posn], " ");
      break;
    }
    (*posn)++;
  }

  /* Repeat for Chk types */
  for(i=0; i<ChkTypes_table.num_registered; i++){

    /* A freq. of zero indicates no automatic emit/consume */
    if(ChkTypes_table.io_def[i].frequency == 0) continue;

    if( (SeqNum % ChkTypes_table.io_def[i].frequency) != 0)continue;

    if( *posn >= REG_MAX_NUM_STR_CMDS ){
      fprintf(stderr, "Auto_generate_steer_cmds: WARNING: discarding "
	      "steering cmds as max number (%d) exceeded\n", 
	      REG_MAX_NUM_STR_CMDS);

      return_status = REG_FAILURE;
      break;
    }
 
    /* Add command to list to send back to caller */
    SteerCommands[*posn]  = ChkTypes_table.io_def[i].handle;

    /* We only ever instruct the app. to emit checkpoints since to consume
       a checkpoint implies a restart */
    sprintf(SteerCmdParams[*posn], "OUT");

    (*posn)++;
  }

  return return_status;
}

/*----------------------------------------------------------------*/

int Steering_pause(int   *NumSteerParams,
		   char **SteerParamLabels,
		   int   *NumCommands,
		   int   *SteerCommands,
		   char **SteerCmdParams)
{
  int    paused        = TRUE;
  int    return_status = REG_SUCCESS;
  int    i, j, index;
  int    seqnum;
  int    num_commands;
  int    commands[REG_MAX_NUM_STR_CMDS];
  int    param_handles[REG_MAX_NUM_STR_PARAMS];
  char*  param_labels[REG_MAX_NUM_STR_PARAMS];
  int    tot_num_params = 0;

  /* Can only call this function if steering lib initialised */

  if (!ReG_SteeringInit) return REG_FAILURE;

  /* Pause the application by waiting for a 'resume' or 'detach'
     (failsafe) command from the steerer.  If comms link goes 
     down then could remain paused indefinitely? */

  while(paused){

    sleep(1);

    /* Read anything that the steerer has sent to us */

    if( Consume_control(&num_commands,
			commands,
			SteerCmdParams,
			NumSteerParams,
			param_handles,
			param_labels) != REG_SUCCESS ){

      return_status = REG_FAILURE;
      paused = FALSE;
#if REG_DEBUG
      fprintf(stderr, "Steering_pause: call to Consume_control failed\n");
#endif
    }
    else{

#if REG_DEBUG
      fprintf(stderr,"Steering_pause: got %d cmds and %d params\n", 
	      num_commands,
	      *NumSteerParams);
#endif

      /* Add to array holding labels of changed params - pass back  
	 strings rather than pointers */

      for(j=0; j<(*NumSteerParams); j++){

	if(tot_num_params < REG_MAX_NUM_STR_PARAMS){
	  strcpy(SteerParamLabels[tot_num_params], param_labels[j]);
	  tot_num_params++;
	}
	else{

	  fprintf(stderr, "Steering_pause: no. of parameters edited "
	          "exceeds %d\n", REG_MAX_NUM_STR_PARAMS);
	  fprintf(stderr, "                Only returning the first %d\n",
		  REG_MAX_NUM_STR_PARAMS);
        }
      }

      /* Check for a resume command - any other commands are
	 ignored (although Consume_control will have updated the
         parameter tables & the associated simulation variables) */

      for(i=0; i<num_commands; i++){

	if(commands[i] == REG_STR_RESUME){

	  paused = FALSE;

	  /* Return all commands that follow the resume command */

	  *NumCommands = num_commands - i - 1;
	  for(j=0; j<*NumCommands; j++){
	    SteerCommands[j] = commands[i + 1 + j];
	    strcpy(SteerCmdParams[j], SteerCmdParams[i + 1 + j]);
	  }

	  break;
	}
	else if(commands[i] == REG_STR_DETACH){

	  paused = FALSE;
	  return_status = Detach_from_steerer();

	  /* Confirm that we have received the detach command */

	  index = Param_index_from_handle(&(Params_table), REG_SEQ_NUM_HANDLE);
	  if(index != -1){
	    sscanf(Params_table.param[index].value, "%d", &seqnum);
	  }
	  else{
	    seqnum = -1;
	  }

	  commands[0] = REG_STR_DETACH;
	  Emit_status(seqnum,
		      0,   
		      NULL,
		      1,
		      commands);

	  *NumCommands  = 0;
	  break;
	}
	else if(commands[i] == REG_STR_STOP){

	  paused = FALSE;
	  return_status = Detach_from_steerer();

	  /* Confirm that we have received the stop command */

	  index = Param_index_from_handle(&(Params_table), REG_SEQ_NUM_HANDLE);
	  if(index != -1){
	    sscanf(Params_table.param[index].value, "%d", &seqnum);
	  }
	  else{
	    seqnum = -1;
	  }
	  commands[0] = REG_STR_STOP;
          Emit_status(seqnum,
		      0,   
		      NULL,
		      1,
		      commands);

	  /* Return the stop command so app can act on it */
	  *NumCommands = 1;
	  SteerCommands[0] = REG_STR_STOP;
	  break;
	}
      }
    }
  }

  /* Return the total no. of parameters that have been edited
     while the application was paused */
  *NumSteerParams = tot_num_params;

  return return_status;
}

/*----------------------------------------------------------------
              Low-level steering routines
----------------------------------------------------------------*/

int Emit_param_defs(){

  char *buf;
  char *pbuf;
  char *dum_ptr;
  int   nbytes;
  int   bytes_left;
  int   i = 0;
  int   more_space = 0;
  int   buf_size = REG_MAX_MSG_SIZE;
  char *plast_param;

  /* Check to see that we do actually have something to emit */
  if (Params_table.num_registered == 0) return REG_SUCCESS;

  if( !(buf=(char *)malloc(buf_size)) ){

    fprintf(stderr,"Emit_param_defs: malloc failed\n");
    return REG_FAILURE;
  }

  pbuf = buf;
  bytes_left = buf_size;

  /* Assume header and following line fit comfortably within
     REG_MAX_MSG_SIZE so don't check for truncation yet */
  Write_xml_header(&pbuf);

  pbuf += sprintf(pbuf, "<Param_defs>\n");
  bytes_left -= (int)(pbuf - buf);
  plast_param = pbuf;

  /* Emit all currently registered parameters  */
  while (i < Params_table.max_entries) {

    /* Check handle because if a parameter is deleted then this is
       flagged by unsetting its handle */
  
    if(Params_table.param[i].handle != REG_PARAM_HANDLE_NOTSET){

      /* We ran out of space */
      if(more_space){
	buf_size += REG_MAX_MSG_SIZE;
	/* Adjust amount of remaining space to allow for return to 
	   last complete param entry as well as realloc */
	bytes_left += REG_MAX_MSG_SIZE + (int)(pbuf - plast_param);

	if(!(dum_ptr = (char *)realloc(buf, buf_size)) ){
	  
	  free(buf);
	  buf = NULL;
	  fprintf(stderr, "Emit_param_defs: realloc failed for %d "
		  "bytes\n", buf_size);
	  return REG_FAILURE;
	}
	/* realloc can return a ptr to a different chunk of memory */
	plast_param += (dum_ptr - buf);
	buf = dum_ptr;
	more_space = 0;

	/* Move back to last complete param entry */
	pbuf = plast_param;
      }
  
      /* Update the 'value' part of this parameter's table entry */
      if(Get_ptr_value(&(Params_table.param[i])) == REG_SUCCESS){
    	 
	nbytes = snprintf(pbuf, bytes_left, "<Param>\n"
			  "<Label>%s</Label>\n"
			  "<Steerable>%d</Steerable>\n"
			  "<Type>%d</Type>\n"
			  "<Handle>%d</Handle>\n"
			  "<Value>%s</Value>\n", 
			  Params_table.param[i].label, 
			  Params_table.param[i].steerable,
			  Params_table.param[i].type,
			  Params_table.param[i].handle, 
			  Params_table.param[i].value);


	/* Check for truncation */
	if((nbytes >= (bytes_left-1)) || (nbytes < 1)){
	  more_space = 1;
	  continue;
	}
	bytes_left -= nbytes;
	pbuf += nbytes;

	if(Params_table.param[i].is_internal == TRUE){
	  
	  nbytes = snprintf(pbuf, bytes_left, 
			    "<Is_internal>TRUE</Is_internal>\n");
	}
	else{
	  
	  nbytes = snprintf(pbuf, bytes_left, 
			    "<Is_internal>FALSE</Is_internal>\n");
	}

	/* Check for truncation */
	if((nbytes >= (bytes_left-1)) || (nbytes < 1)){
	  more_space = 1;
	  continue;
	}
	bytes_left -= nbytes;
	pbuf += nbytes;

	if(Params_table.param[i].min_val_valid==TRUE &&
	   Params_table.param[i].max_val_valid==TRUE){
	  
	  nbytes = snprintf(pbuf, bytes_left, "<Min_value>%s</Min_value>"
			    "<Max_value>%s</Max_value>\n"
			    "</Param>\n", 
			    Params_table.param[i].min_val, 
			    Params_table.param[i].max_val);
	}
	else if(Params_table.param[i].min_val_valid==TRUE){
	  nbytes = snprintf(pbuf, bytes_left, 
			    "<Min_value>%s</Min_value>\n" 
			    "</Param>\n", 
			    Params_table.param[i].min_val);
	}
	else if(Params_table.param[i].max_val_valid==TRUE){
	  nbytes = snprintf(pbuf, bytes_left, 
			    "<Max_value>%s</Max_value>\n" 
			    "</Param>\n", 
			    Params_table.param[i].max_val);
	}
	else{
	  nbytes = snprintf(pbuf, bytes_left, "</Param>\n");
	}

	/* Check for truncation */
	if((nbytes >= (bytes_left-1)) || (nbytes < 1)){
	  more_space = 1;
	  continue;
	}
	
	bytes_left -= nbytes;
	pbuf += nbytes;

	/* Try and predict whether we'll hit truncation problems
	   for the next param def - allow 5/4 the length of 
	   last param def. */
	if( bytes_left < (int)(1.25*(pbuf - plast_param))){
	  more_space = 1;
	  continue;
	}
	plast_param = pbuf;
      }
    }

    i++;
  }

  nbytes = snprintf(pbuf, bytes_left, "</Param_defs>\n");

  /* Check for truncation */
  if((nbytes >= (bytes_left-1)) || (nbytes < 1)){

    buf_size +=  REG_MAX_MSG_SIZE;
    bytes_left +=  REG_MAX_MSG_SIZE;

    if(!(dum_ptr = (char *)realloc(buf, buf_size)) ){

      free(buf);
      buf = NULL;
      fprintf(stderr, "Emit_param_defs: realloc failed for %d "
	      "bytes\n", buf_size);
      return REG_FAILURE;
    }
    /* realloc can return a ptr to a different chunk of memory */
    pbuf += (dum_ptr - buf);
    buf = dum_ptr;

    nbytes = snprintf(pbuf, bytes_left, "</Param_defs>\n");
  }

  bytes_left -= nbytes;
  pbuf += nbytes;
  
  if(Write_xml_footer(&pbuf, bytes_left) == REG_SUCCESS){

    /* Physically send the message */
    Send_status_msg(buf);

    free(buf);
    buf = NULL;
    return REG_SUCCESS;
  }
  else{
    fprintf(stderr, "Emit_param_defs: ran out of space for footer\n");
    free(buf);
    buf = NULL;
    return REG_FAILURE;
  }
}

/*----------------------------------------------------------------*/

int Emit_IOType_defs(){

  int   i;
  char  buf[REG_MAX_MSG_SIZE];
  char *pbuf;
  int   nbytes, bytes_left;

  /* Check that we do actually have something to emit */
  if (IOTypes_table.num_registered == 0) return REG_SUCCESS;

  /* Emit all currently registered IOTypes */
  
  bytes_left = REG_MAX_MSG_SIZE;
  pbuf = buf;
  Write_xml_header(&pbuf);

  nbytes = sprintf(pbuf, "<IOType_defs>\n");
  
  pbuf += nbytes;
  bytes_left -= nbytes;

  for(i=0; i<IOTypes_table.max_entries; i++){
  
    if(IOTypes_table.io_def[i].handle != REG_IODEF_HANDLE_NOTSET){
  
      nbytes = snprintf(pbuf, bytes_left, "<IOType>\n"
			"<Label>%s</Label>\n"
			"<Handle>%d</Handle>\n", 
			IOTypes_table.io_def[i].label, 
			IOTypes_table.io_def[i].handle);

#if REG_DEBUG
      /* Check for truncation */
      if((nbytes >= (bytes_left-1)) || (nbytes < 1)){

	fprintf(stderr, "Emit_IOType_defs: message exceeds max. "
		"msg. size of %d bytes\n", REG_MAX_MSG_SIZE);
	return REG_FAILURE;
      }
#endif
      pbuf += nbytes;
      bytes_left -= nbytes;

      switch(IOTypes_table.io_def[i].direction){

      case REG_IO_IN:
        nbytes = snprintf(pbuf, bytes_left, "<Direction>IN</Direction>\n");
	break;

      case REG_IO_OUT:
        nbytes = snprintf(pbuf, bytes_left, "<Direction>OUT</Direction>\n");
	break;

      default:
#if REG_DEBUG
	fprintf(stderr, 
		"Emit_IOType_defs: Unrecognised IOType direction\n");
#endif
	return REG_FAILURE;
      }
      pbuf += nbytes;
      bytes_left -= nbytes;


      nbytes = snprintf(pbuf, bytes_left, "<Freq_handle>%d</Freq_handle>\n",
			IOTypes_table.io_def[i].freq_param_handle);
#if REG_DEBUG
      /* Check for truncation */
      if((nbytes >= (bytes_left-1)) || (nbytes < 1)){

	fprintf(stderr, "Emit_IOType_defs: message exceeds max. "
		"msg. size of %d bytes\n", REG_MAX_MSG_SIZE);
	return REG_FAILURE;
      }
#endif
      pbuf += nbytes;
      bytes_left -= nbytes;


#if REG_SOCKET_SAMPLES
      if(IOTypes_table.io_def[i].direction == REG_IO_OUT){

	if(!strstr(IOTypes_table.io_def[i].socket_info.listener_hostname,
		     "NOT_SET")){
	  nbytes = snprintf(pbuf, bytes_left, "<Address>%s:%d</Address>\n",
		     IOTypes_table.io_def[i].socket_info.listener_hostname,
		     (int)(IOTypes_table.io_def[i].socket_info.listener_port));

#if REG_DEBUG
	  /* Check for truncation */
	  if((nbytes >= (bytes_left-1)) || (nbytes < 1)){

	    fprintf(stderr, "Emit_IOType_defs: message exceeds max. "
		    "msg. size of %d bytes\n", REG_MAX_MSG_SIZE);
	    return REG_FAILURE;
	  }
#endif /* REG_DEBUG */
	  pbuf += nbytes;
	  bytes_left -= nbytes;
	}
      }
#endif /* REG_SOCKET_SAMPLES */
      nbytes = snprintf(pbuf, bytes_left, "</IOType>\n");
      pbuf += nbytes;
      bytes_left -= nbytes;
    }
  }
  
  nbytes = snprintf(pbuf, bytes_left, "</IOType_defs>\n");
  pbuf += nbytes;
  bytes_left -= nbytes;

  if(Write_xml_footer(&pbuf, bytes_left) == REG_SUCCESS){

    /* Physically send message */
    return Send_status_msg(buf);
  }

  fprintf(stderr, "Emit_IOType_defs: ran out of space for footer\n");
  return REG_FAILURE;
}

/*----------------------------------------------------------------*/

int Emit_ChkType_defs(){

  int   i;
  char  buf[REG_MAX_MSG_SIZE];
  char *pbuf;
  int   nbytes, bytes_left;

  /* Check that we do actually have something to emit */
  if (ChkTypes_table.num_registered == 0) return REG_SUCCESS;

  /* Emit all currently registered ChkTypes */
  
  bytes_left = REG_MAX_MSG_SIZE;
  pbuf = buf;
  Write_xml_header(&pbuf);

  nbytes = sprintf(pbuf, "<ChkType_defs>\n");
  
  pbuf += nbytes;
  bytes_left -= nbytes;

  for(i=0; i<ChkTypes_table.max_entries; i++){
  
    if(ChkTypes_table.io_def[i].handle != REG_IODEF_HANDLE_NOTSET){
  
      nbytes = snprintf(pbuf, bytes_left, "<ChkType>\n"
		       "<Label>%s</Label>\n"
		       "<Handle>%d</Handle>\n", 
		       ChkTypes_table.io_def[i].label, 
		       ChkTypes_table.io_def[i].handle);
#if REG_DEBUG
      /* Check for truncation */
      if((nbytes >= (bytes_left-1)) || (nbytes < 1)){

	fprintf(stderr, "Emit_ChkType_defs: message exceeds max. "
		"msg. size of %d bytes\n", REG_MAX_MSG_SIZE);
	return REG_FAILURE;
      }
#endif /* REG_DEBUG */
      pbuf += nbytes;
      bytes_left -= nbytes;


      switch(ChkTypes_table.io_def[i].direction){

      case REG_IO_IN:
        nbytes = snprintf(pbuf,bytes_left,"<Direction>IN</Direction>\n");
	break;

      case REG_IO_OUT:
        nbytes = snprintf(pbuf,bytes_left,"<Direction>OUT</Direction>\n");
	break;

      case REG_IO_INOUT:
	nbytes = snprintf(pbuf,bytes_left,"<Direction>INOUT</Direction>\n");
	break;

      default:
#if REG_DEBUG

	fprintf(stderr, 
		"Emit_ChkType_defs: Unrecognised ChkType direction\n");
#endif /* REG_DEBUG */
	return REG_FAILURE;
      }
#if REG_DEBUG
      /* Check for truncation */
      if((nbytes >= (bytes_left-1)) || (nbytes < 1)){

	fprintf(stderr, "Emit_ChkType_defs: message exceeds max. "
		"msg. size of %d bytes\n", REG_MAX_MSG_SIZE);
	return REG_FAILURE;
      }
#endif /* REG_DEBUG */
      pbuf += nbytes;
      bytes_left -= nbytes;


      nbytes = snprintf(pbuf,bytes_left,"<Freq_handle>%d</Freq_handle>\n"
			"</ChkType>\n",
			ChkTypes_table.io_def[i].freq_param_handle);
#if REG_DEBUG
      /* Check for truncation */
      if((nbytes >= (bytes_left-1)) || (nbytes < 1)){
	fprintf(stderr, "Emit_ChkType_defs: message exceeds max. "
		"msg. size of %d bytes\n", REG_MAX_MSG_SIZE);
	return REG_FAILURE;
      }
#endif /* REG_DEBUG */
      pbuf += nbytes;
      bytes_left -= nbytes;
    }
  }
  
  nbytes = snprintf(pbuf,bytes_left,"</ChkType_defs>\n");
#if REG_DEBUG
  /* Check for truncation */
  if((nbytes >= (bytes_left-1)) || (nbytes < 1)){
    fprintf(stderr, "Emit_ChkType_defs: message exceeds max. "
	    "msg. size of %d bytes\n", REG_MAX_MSG_SIZE);
    return REG_FAILURE;
  }
#endif /* REG_DEBUG */
  pbuf += nbytes;
  bytes_left -= nbytes;

  if(Write_xml_footer(&pbuf, bytes_left) == REG_SUCCESS){

    /* Physically send message */
    return Send_status_msg(buf);
  }
  fprintf(stderr, "Emit_ChkType_defs: ran out of space for footer\n");
  return REG_FAILURE;
}

/*----------------------------------------------------------------*/

int Emit_log()
{
  char *pbuf;
  char  filename[REG_MAX_STRING_LENGTH];
  int   size;
  int   return_status = REG_SUCCESS;

  if(Chk_log.send_all == TRUE){

#if REG_DEBUG
    fprintf(stderr, "Emit_log: sending all saved log entries...\n");
#endif
    /* Then we have to send any entries we've saved to file too... */
    Close_log_file();

    /* Read the log file and get back contents in buffer pointed
       to by pbuf.  We must free() this once we're done. */
    sprintf(filename, "%s%s", Steerer_connection.file_root,
		              REG_LOG_FILENAME);
    if(Read_file(filename, &pbuf, &size) != REG_SUCCESS){

      if(!pbuf)free(pbuf);
      pbuf = NULL;
      return REG_FAILURE;
    }

    if (size > 0) Emit_log_entries(pbuf);

    free(pbuf);
    pbuf = NULL;

    /* Re-open log-file for future buffering */
    if( Open_log_file() != REG_SUCCESS){

#if REG_DEBUG
      fprintf(stderr, "Emit_log: Open_log_file failed\n");
#endif
    }
    /* End of sending buffered entries */

    /* Now send the entries that we have stored in memory - 
       need to send all current log entries to the steerer */
    if(Log_to_xml(&pbuf, &size, FALSE) != REG_SUCCESS){

      return REG_FAILURE;
    }
    Chk_log.send_all = FALSE;
  }
  else{

#if REG_DEBUG
    fprintf(stderr, "Emit_log: sending unsent log entries...\n");
#endif
    /* Third argument specifies that we only want those entries that haven't
       already been sent to the steerer */
    if(Log_to_xml(&pbuf, &size, TRUE) != REG_SUCCESS){

      return REG_FAILURE;
    }
  }

#if REG_DEBUG
  fprintf(stderr, "Emit_log: calling Emit_log_entries...\n");
#endif

  /* Pull the entries out of the buffer returned by Log_to_xml and
     send them to the steerer */
  if(size > 0)return_status = Emit_log_entries(pbuf);
  free(pbuf);
  pbuf = NULL;

#if REG_DEBUG
  fprintf(stderr, "Emit_log: sending logged steering commands...\n");
#endif

  /* Send log of steering commands */
  if(strlen(Chk_log.pSteer_cmds) > 0){

    Emit_log_entries(Chk_log.pSteer_cmds);
    Chk_log.pSteer_cmds[0]='\0';
    Chk_log.pSteer_cmds_slot = Chk_log.pSteer_cmds;
  }

  if(return_status == REG_SUCCESS){

    /* Zero counter since we've just told steerer all about any log
       entries it hadn't got */
    Chk_log.num_unsent = 0;
  }

  return return_status;
}

/*----------------------------------------------------------------*/

int Emit_log_entries(char *buf)
{
  char *msg_buf;
  char *pmsg_buf;
  char *plast = NULL;
  char *pbuf1 = NULL;
  char *pbuf2 = NULL;
  char *pbuf3 = NULL;
  void *pdum;
  int   msg_buf_size;
  int   tot_len = 0;
  int   len;
  int   nbytes;
  int   rewind;
  int   return_status = REG_SUCCESS;

  msg_buf_size = REG_MAX_MSG_SIZE;
  if(!(msg_buf = (char *)malloc(msg_buf_size))){

    fprintf(stderr, "Emit_log_entries: malloc failed\n");
    return REG_FAILURE;
  }

  /* Pull each log entry out of the buffer and pack them into
     messages to the steerer */
  pbuf1 = strstr(buf, "<Log_entry>");
  pbuf2 = pbuf1;
  if(pbuf2){
    if(pbuf3 = strstr(pbuf2, "</Log_entry>")){
      /* Increment ptr so as to include all of the "</Log_entry>" */
      pbuf3 += 12;
    }
  }

  while(pbuf3){

    if(tot_len == 0){
      /* Begin the first message */
      pmsg_buf = msg_buf;
      Write_xml_header(&pmsg_buf);
      pmsg_buf += sprintf(pmsg_buf, "<Steer_log>\n");
      tot_len = (int)(pmsg_buf - msg_buf);
    }
    len = (int)(pbuf3 - pbuf2);

    /* 35 = strlen("</Steer_log>\n</ReG_steer_message>\n"); */
    if((tot_len + len) < (msg_buf_size - 35)){

      /* Buffer has enough free space so add this entry to it */
      plast = pmsg_buf;
      strncpy(pmsg_buf, pbuf2, len);
      pmsg_buf += len;
      tot_len += len;
    }
    else if(!plast){

      /* We've not managed to fit a single entry into the buffer
	 - time for a realloc... */
      msg_buf_size *= 2;
      if( !(pdum = realloc(msg_buf, msg_buf_size)) ){

	free(msg_buf);
	msg_buf = NULL;
	fprintf(stderr, "Emit_log_entries: realloc failed\n");
	return REG_FAILURE;
      }
      /* Allow for fact that realloc can return a ptr to a diff't
	 chunk of memory (with any content intact though) */
      pmsg_buf += (char *)pdum - msg_buf;
      msg_buf = (char *)pdum;
      continue;
    }
    else{
      /* Complete the xml message */
      nbytes = snprintf(pmsg_buf, (msg_buf_size-tot_len), 
			"</Steer_log>\n");

      rewind = (nbytes >= (msg_buf_size-tot_len-1) || (nbytes < 1));

      if(!rewind){
	pmsg_buf += nbytes;
	tot_len += nbytes;

	if(Write_xml_footer(&pmsg_buf, (msg_buf_size-tot_len))
	                                             != REG_SUCCESS){
	  rewind = 1;
	}
      }

      if(rewind){

	/* Can't fit the footer in remaining space in buffer - go
	   back to last complete entry */
	tot_len = (int)(plast - msg_buf);
	pmsg_buf = plast;
	pmsg_buf += sprintf(pmsg_buf, "</Steer_log>\n");
	tot_len = (int)(pmsg_buf - msg_buf);
	pbuf3 = pbuf2;
	pbuf2 = pbuf1;
	len = (int)(pbuf3 - pbuf2);

	if(Write_xml_footer(&pmsg_buf, (msg_buf_size-tot_len)) 
	                                             != REG_SUCCESS){
	  fprintf(stderr, "Emit_log_entries: error writing footer\n");
	  return REG_FAILURE;
	}
      }

      if(Send_status_msg(msg_buf) != REG_SUCCESS){

	return REG_FAILURE;
      }

      /* Begin the next message */
      plast = NULL;
      pmsg_buf = msg_buf;
      Write_xml_header(&pmsg_buf);
      pmsg_buf += sprintf(pmsg_buf, "<Steer_log>\n");

      strncpy(pmsg_buf, pbuf2, len);
      pmsg_buf += len;
      tot_len = (int)(pmsg_buf - msg_buf);
    }

    /* Look for next entry */
    pbuf1 = pbuf2;
    pbuf2 = pbuf3;
    if(pbuf3 = strstr(pbuf2, "</Log_entry>")){
      /* Increment ptr so as to include all of the "</Log_entry>" */
      pbuf3 += 12;
    }
  }

  /* Complete the xml message */
  if(tot_len > 0){
    pmsg_buf += sprintf(pmsg_buf, "</Steer_log>\n");
    tot_len = (int)(pmsg_buf - msg_buf);

    if(Write_xml_footer(&pmsg_buf, (msg_buf_size-tot_len)) 
                                                   == REG_SUCCESS){

      return_status = Send_status_msg(msg_buf);
    }
    else{
      fprintf(stderr, "Emit_log_entries: error writing final footer\n");
      return_status = REG_FAILURE;
    }
  }

  free(msg_buf);
  msg_buf = NULL;
  return return_status;
}

/*----------------------------------------------------------------*/

int Consume_control(int    *NumCommands,
		    int    *Commands,
		    char  **CommandParams,
		    int    *NumSteerParams,
		    int    *SteerParamHandles,
		    char  **SteerParamLabels){

  int                  j;
  int                  count;
  int                  log_count;
  char                *ptr;
  struct msg_struct   *msg;
  struct cmd_struct   *cmd;
  struct param_struct *param;
  int                  handle;
  int                  return_status = REG_SUCCESS;
  
  /* Read any message sent by the steerer - may contain commands and/or
     new parameter values */

  if((msg = Get_control_msg()) != NULL){

    if(msg->control){

      cmd   = msg->control->first_cmd;
      count = 0;

      while(cmd){

	if(cmd->id){
	  sscanf((char *)(cmd->id), "%d", &(Commands[count]));
	}
	else if(cmd->name){

	  if(!xmlStrcmp(cmd->name, (const xmlChar *)"STOP")){
	    Commands[count] = REG_STR_STOP;
	  }
	  else if(!xmlStrcmp(cmd->name, (const xmlChar *)"PAUSE")){
	    Commands[count] = REG_STR_PAUSE;
	  }
	  else if(!xmlStrcmp(cmd->name, (const xmlChar *)"DETACH")){
	    Commands[count] = REG_STR_DETACH;
	  }
	  else if(!xmlStrcmp(cmd->name, (const xmlChar *)"RESUME")){
	    Commands[count] = REG_STR_RESUME;
	  }
	  else{
	    fprintf(stderr, "Consume_control: unrecognised cmd name: %s\n", 
		    (char *)cmd->name);
	    cmd = cmd->next;
	    continue;
	  }

	  /* Log this command */
	  Steer_log.cmd[count].id = Commands[count];
	}
	else{
	  fprintf(stderr, "Consume_control: error - skipping cmd because "
		  "is missing both id and name\n");
	  cmd = cmd->next;
	  continue;
	}

	if(cmd->first_param){

	  param = cmd->first_param;
	  ptr   = CommandParams[count];
	  
	  while(param){

	    if(param->value){
	      sprintf(ptr, "%s ", (char *)(param->value));
	      ptr += strlen((char *)param->value) + 1;
	    }

	    param = param->next;
	  }
	}
	else{

	  sprintf(CommandParams[count], " ");
	}

	/* Log this cmd parameter */
	strcpy(Steer_log.cmd[count].params, CommandParams[count]);

#if REG_DEBUG
	fprintf(stderr, "Consume_control: cmd[%d] = %d\n", count,
		Commands[count]);
	fprintf(stderr, "                 params  = %s\n", 
		CommandParams[count]);
#endif
	count++;

	if(count >= REG_MAX_NUM_STR_CMDS){

	  fprintf(stderr, 
		  "Consume_control: WARNING: truncating list of commands\n");
	  break;
	}

	cmd = cmd->next;
      }

      /* Record how many cmds we've just received */
      *NumCommands = count;
      Steer_log.num_cmds = count;

#if REG_DEBUG
      fprintf(stderr, "Consume_control: received %d commands\n", 
	      *NumCommands);
#endif

      param = msg->control->first_param;
      count = 0;
      log_count = 0;

      while(param){

	sscanf((char *)(param->handle), "%d", &handle);

	for(j=0; j<Params_table.max_entries; j++){
  
	  if(Params_table.param[j].handle == handle){
	  
	    break;
	  }
	}

	if(j == Params_table.max_entries){
  
	  fprintf(stderr, "Consume_control: failed to match param "
		  "handles\n");
	  return_status = REG_FAILURE;
	}
	else{

	  /* Store char representation of new parameter value */
	  if(param->value){

	    strcpy(Params_table.param[j].value, (char *)(param->value));

	    /* Update value associated with pointer */
	    Update_ptr_value(&(Params_table.param[j]));

	    if( !(Params_table.param[j].is_internal) ){

	      SteerParamHandles[count] = handle;
	      SteerParamLabels[count]  = Params_table.param[j].label;
	      count++;
	    }

	    /* Log new parameter value */
	    Steer_log.param[log_count].handle = handle;
	    strcpy(Steer_log.param[log_count].value, 
		   Params_table.param[j].value);
	    log_count++;
	  }
	  else{
	    fprintf(stderr, "Consume_control: empty parameter value "
		    "field\n");
	  }
	}

	param = param->next;
      }

      /* Record no. of param. changes in this log entry */
      Steer_log.num_params = log_count;

      /* Update the number of parameters received to allow for fact that
	 some may be internal and are not passed up to the calling routine */
      *NumSteerParams = count;

#if REG_DEBUG
      fprintf(stderr, "Consume_control: received %d params\n", 
	      *NumSteerParams);
#endif
    }
    else{
      fprintf(stderr, "Consume_control: error, no control data\n");
      *NumSteerParams = 0;
      *NumCommands    = 0;
      return_status   = REG_FAILURE;
    }

    /* free up msg memory */
    Delete_msg_struct(msg);
    msg = NULL;

  }
  else{

#if REG_DEBUG
    fprintf(stderr, "Consume_control: no message from steerer\n");
#endif

    /* No message found */

    *NumSteerParams = 0;
    *NumCommands = 0;
  }


  return return_status;
}

/*----------------------------------------------------------------*/

int Detach_from_steerer()
{

#if REG_SOAP_STEERING

  Detach_from_steerer_soap();

#else /* File-based steering */

  Detach_from_steerer_file();

#endif /* File-based steering */

  /* Flag that all entries in log need to be sent to steerer (in case
     another one attaches later on) */
  Chk_log.send_all   = TRUE;

  ReG_SteeringActive = FALSE;
  ReG_IOTypesChanged = TRUE;
  ReG_ChkTypesChanged= TRUE;
  ReG_ParamsChanged  = TRUE;

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Emit_status(int   SeqNum,
		int   NumParams,
		int  *ParamHandles,
		int   NumCommands,
		int  *Commands)
{
  int   i;
  int   pcount = 0;
  int   tot_pcount = 0;
  int   ccount = 0;
  int   num_param;
  int   cmddone   = FALSE;
  int   paramdone = FALSE;
  char  buf[REG_MAX_MSG_SIZE];
  char *pbuf;
  int   nbytes, bytes_left;

  /* Emit a status report - this is complicated because we must ensure we
     don't write too many params or commands to a single file (self-imposed
     limits to make it easier for user to supply arrays to receive results) */

  /* Count how many monitoring parameters there are */

  for(i=0; i<Params_table.max_entries; i++){
    
    if(Params_table.param[i].handle != REG_PARAM_HANDLE_NOTSET) 
	/* Want to output ALL params now - not just steerable ones */
	/* && (!Params_table.param[i].steerable) ) */  pcount++;
  }
  num_param = pcount;
  pcount = 0;

  /* If we are sending a 'detach' command then don't send any
     parameter values */
  if(NumCommands > 0){

    if(Commands[0] == REG_STR_DETACH){

      paramdone = TRUE;
    }
  }
  else{
    cmddone = TRUE;
  }

  if(num_param == 0) paramdone = TRUE;

  /* Loop until all params and commands have been emitted */

  while(!paramdone || !cmddone){

    pbuf = buf;

    Write_xml_header(&pbuf);
    nbytes = sprintf(pbuf, "<App_status>\n");

    bytes_left = REG_MAX_MSG_SIZE - nbytes -  (pbuf - buf);
    pbuf += nbytes;

    /* Parameter values section */

    if(!paramdone){

      /* Loop over max. no. of params to write to any given file */

      for(i=0; i<REG_MAX_NUM_STR_PARAMS; i++){
  
    	/* Handle value used to indicate whether entry is valid */
    	if(Params_table.param[tot_pcount].handle != REG_PARAM_HANDLE_NOTSET){

 	  /* Update the 'value' part of this parameter's table entry
	     - Get_ptr_value checks to make sure parameter is not library-
	     controlled (& hence has valid ptr to get value from) */
 	  Get_ptr_value(&(Params_table.param[tot_pcount]));

 	  nbytes = snprintf(pbuf, bytes_left, "<Param>\n"
			    "<Handle>%d</Handle>\n"
			    "<Value>%s</Value>\n"
			    "</Param>\n", 
			    Params_table.param[tot_pcount].handle, 
			    Params_table.param[tot_pcount].value);

	  /* Check for truncation */
	  if((nbytes >= (bytes_left-1)) || (nbytes < 1)){

	    fprintf(stderr, "Emit_status: message exceeds max. "
		    "msg. size of %d bytes\n", REG_MAX_MSG_SIZE);
	    return REG_FAILURE;
	  }
	  pbuf += nbytes;
	  bytes_left -= nbytes;

 	  pcount++;
    	}
  
	/* Cumulative counter to move us through param table */
	tot_pcount++;

    	if(pcount >= num_param){
 	  paramdone = TRUE;
 	  break;
    	}
      }
    }

    /* Commands section */

    if(!cmddone){

#if REG_DEBUG
      fprintf(stderr, "Emit_status: NumCommands = %d, ccount = %d\n", 
	      NumCommands, ccount);
#endif

      for(i=0; i<REG_MAX_NUM_STR_CMDS; i++){
  
    	nbytes = snprintf(pbuf, bytes_left, "<Command>\n"
			  "<Cmd_id>%d</Cmd_id>\n"
			  "</Command>\n", 
			  Commands[ccount]);

	/* Check for truncation */
	if((nbytes >= (bytes_left-1)) || (nbytes < 1)){

	  fprintf(stderr, "Emit_status: message exceeds max. "
		  "msg. size of %d bytes\n", REG_MAX_MSG_SIZE);
	  return REG_FAILURE;
	}
	pbuf += nbytes;
	bytes_left -= nbytes;

    	ccount++;
  
    	if(ccount >= NumCommands){
 	  cmddone = TRUE;
 	  break;
    	}
      }
    }

    nbytes = snprintf(pbuf, bytes_left, "</App_status>\n");

    /* Check for truncation */
    if((nbytes >= (bytes_left-1)) || (nbytes < 1)){

      fprintf(stderr, "Emit_status: message exceeds max. "
	      "msg. size of %d bytes\n", REG_MAX_MSG_SIZE);
      return REG_FAILURE;
    }

    pbuf += nbytes;
    bytes_left -= nbytes;

    if(Write_xml_footer(&pbuf, bytes_left) == REG_SUCCESS){

      /* Physically send the status message */
      Send_status_msg(buf);
    }
    else{
      fprintf(stderr, "Emit_status: failed to write footer\n");
    }
  }

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Update_ptr_value(param_entry *param)
{

  if (!param->ptr) return REG_SUCCESS;

  switch(param->type){

  case REG_INT:
    sscanf(param->value, "%d", (int *)(param->ptr));
    break;

  case REG_FLOAT:
    sscanf(param->value, "%f", (float *)(param->ptr));
    break;

  case REG_DBL:
    sscanf(param->value, "%lf", (double *)(param->ptr));
    break;

  case REG_CHAR:
    if(ReG_CalledFromF90 == TRUE){
      /* Avoid terminating with '\0' if calling code 
	 is F90 */
      strncpy((char *)(param->ptr), param->value, 
	      strlen(param->value));
      /* Blank the remainder of the string */
      memset((char *)(param->ptr) + strlen(param->value), ' ',
	     atoi(param->max_val)-strlen(param->value));
    }
    else{
      strcpy((char *)(param->ptr), param->value);
    }
    break;

  default:
    fprintf(stderr, "Update_ptr_value: unrecognised parameter type\n");
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Get_ptr_value(param_entry *param)
{
  int return_status;

  /* Retrieve the value of the variable pointed to by this parameter's
     registered pointer and store it in the 'value' character string
     of the table entry */

  return_status = REG_SUCCESS;
  
  /* If this is a special parameter then its pointer isn't used */
  if(param->handle < REG_MIN_PARAM_HANDLE ){

    return REG_SUCCESS;
  }

  switch(param->type){
  
  case REG_INT:
    sprintf(param->value,"%d", *((int *)(param->ptr)));
    break;
  
  case REG_FLOAT:
    sprintf(param->value,"%.8f", *((float *)(param->ptr)));
    break;
  
  case REG_DBL:
    sprintf(param->value,"%.8lf", *((double *)(param->ptr)));
    break;
  
  case REG_CHAR:
    if(ReG_CalledFromF90 == TRUE && param->max_val_valid){
      /* We've got a ptr to a F90 string here and they aren't 
	 terminated with a '\0'.  We know how long it is though
	 because we save that information when it was registered. */
      strncpy(param->value, (char *)(param->ptr), atoi(param->max_val));
      param->value[atoi(param->max_val)-1] = '\0';
    }
    else{
      strcpy(param->value, (char *)(param->ptr));
    }
    break;
  
  default:
    fprintf(stderr, "Get_ptr_value: unrecognised parameter type\n");
    fprintf(stderr, "Param type   = %d\n", param->type);
    fprintf(stderr, "Param handle = %d\n", param->handle);
    fprintf(stderr, "Param label  = %s\n", param->label);

    return_status = REG_FAILURE;
    break;
  }

  return return_status;
}

/*----------------------------------------------------------------*/

void Steering_signal_handler(int aSignal)
{
  
  /* caught one signal - ignore all others now as going to quit and do not
     want the quit process to be interrupted and restarted... */
  signal(SIGINT, SIG_IGN); 
  signal(SIGTERM, SIG_IGN);
  signal(SIGSEGV, SIG_IGN);
  signal(SIGILL, SIG_IGN);
  signal(SIGABRT, SIG_IGN);
  signal(SIGFPE, SIG_IGN);

  switch(aSignal){

    case SIGINT:
      fprintf(stderr, "Interrupt signal received (signal %d)\n", aSignal);
      break;
      
    case SIGTERM:
      fprintf(stderr, "Kill signal received (signal %d)\n", aSignal);
      break;
      
    case SIGSEGV:
      fprintf(stderr, "Illegal Access caught (signal %d)\n", aSignal);
      break;

    case  SIGILL:
      fprintf(stderr, "Illegal Exception caught (signal %d)\n", aSignal);
      break;

      /* note: abort called if exception not caught (and hence calls 
	 terminate) */
    case SIGABRT:
      fprintf(stderr, "Abort signal caught (signal %d)\n", aSignal);
      break;

    case SIGFPE:
      fprintf(stderr, "Arithmetic Exception caught (signal %d)\n", aSignal);
      break;

    default:
      fprintf(stderr, "Signal caught (signal %d)\n", aSignal);

  }

  fprintf(stderr, "Steering library quitting...\n");

  if (Steering_finalize() != REG_SUCCESS){
    fprintf(stderr, "Steering_signal_handler: Steerer_finalize failed\n");
  }

  exit(0);
}

/*--------------------------------------------------------------------*/

int Make_vtk_buffer(int    nx,
		    int    ny,
		    int    nz,
		    int    veclen,
		    double a,
		    double b,
		    double c,
		    float *array)
{
  int    i, j, k;
  int    count;
  double a2, b2, c2;
  float  mag;
  float  sum;
  float *fptr;

  a2 = a*a;
  b2 = b*b;
  c2 = c*c;

  /* Make an array of data */
  fptr = array;

  sum = 0.0;
  count = 0;

  if(veclen == 1){

    for(i=-nx/2; i<nx/2; i++){
      for(j=-ny/2; j<ny/2; j++){
	for(k=-nz/2; k<nz/2; k++){
          *fptr = (float)sqrt((double)(i*i*a2 + j*j*b2 + k*k*c2));
	  sum += *(fptr++);

	  count++;
	}
      }
    }
  }
  else if(veclen == 2){

    for(i=-nx/2; i<nx/2; i++){
      for(j=-ny/2; j<ny/2; j++){
	for(k=-nz/2; k<nz/2; k++){

	  mag = 2.0/(1.0 + (float)sqrt((double)(i*i*a2 + j*j*b2 + k*k*c2)));
	  *fptr = mag*(float)(i*k);
	  sum += *(fptr++);
	  *fptr = mag*(float)(j*k);
	  sum += *(fptr++);

	  count = count+2;
	}
      }
    }
  }
  else if(veclen == 3){

    mag = 10.0/(float)(nx*ny*nz);

    for(i=-nx/2; i<nx/2; i++){
      for(j=-ny/2; j<ny/2; j++){
	for(k=-nz/2; k<nz/2; k++){
	  /*
	  mag = 2.0/(1.0 + (float)sqrt((double)(i*i*a2 + j*j*b2 + k*k*c2)));
	  *fptr = mag*(float)i;
	  sum += *(fptr++);
	  *fptr = mag*(float)j;
	  sum += *(fptr++);
	  *fptr = mag*(float)k;
	  sum += *(fptr++);
	  */
	  *fptr = mag*(float)i;
	  sum += *(fptr++);
	  *fptr = mag*(float)j;
	  sum += *(fptr++);
	  *fptr = mag*(float)k;
	  sum += *(fptr++);

	  count = count+3;
	}
      }
    }
  }
  else{
    fprintf(stderr, "Make_vtk_buffer: error, only  1 <= veclen <= 3 "
	    "supported\n");
    return REG_FAILURE;
  }

#if REG_DEBUG
  fprintf(stderr, "Make_vtk_buffer: checksum = %f\n", sum/((float) count));
#endif

  return REG_SUCCESS;
}

/*--------------------------------------------------------------------
  Make ASCII header to describe data to vtk
*/
int Make_vtk_header(char  *header,
		    char  *title,
		    int    nx,
		    int    ny,
		    int    nz,
		    int    veclen,
		    int    type)
{
  char *pchar = header;
  char  type_text[8];

  /* Flag to switch between AVS- and vtk-style headers 
     - for testing */
  const int AVS_STYLE = 0;

  if (!pchar) return REG_FAILURE;

  if(veclen != 1 && veclen != 3){

    fprintf(stderr, "Make_vtk_header: only veclen of 1 or 3 supported\n");
    return REG_FAILURE;
  }


  if(AVS_STYLE){

    pchar += sprintf(pchar, "# AVS field file\n");
    pchar += sprintf(pchar, "ndim=3\n");
    pchar += sprintf(pchar, "dim1= %d\n", nx);
    pchar += sprintf(pchar, "dim2= %d\n", ny);
    pchar += sprintf(pchar, "dim3= %d\n", nz);
    pchar += sprintf(pchar, "nspace=3\n");
    pchar += sprintf(pchar, "field=uniform\n");
    pchar += sprintf(pchar, "veclen= %d\n", veclen);

    if(type == REG_DBL){
      sprintf(type_text, "double");
    }
    else if(type == REG_FLOAT){

      sprintf(type_text, "float");
    }
    else if(type == REG_INT){

      sprintf(type_text, "integer");
    }
    else{

      fprintf(stderr, "Make_vtk_header: Unrecognised data type\n");
      return REG_FAILURE;
    }
    pchar += sprintf(pchar, "data=%s\n", type_text);

    /* Use 'filetype=stream' because this is _not_ standard AVS because
       the way we interpret 'skip' at the other end of a socket is
       not standard either - we use it as the number of objects (floats,
       ints etc.) to skip rather than the no. of bytes or lines. */
    if(veclen == 1){
      pchar += sprintf(pchar, "variable 1 filetype=stream "
		       "skip=0000000 stride=1\n");
    }
    else if(veclen == 2){
      pchar += sprintf(pchar, "variable 1 filetype=stream "
		     "skip=0000000 stride=2\n");
      pchar += sprintf(pchar, "variable 2 filetype=stream "
		     "skip=0000001 stride=2\n");

    }
    else if(veclen == 3){
      pchar += sprintf(pchar, "variable 1 filetype=stream "
		       "skip=0000000 stride=3\n");
      pchar += sprintf(pchar, "variable 2 filetype=stream "
		       "skip=0000001 stride=3\n");
      pchar += sprintf(pchar, "variable 3 filetype=stream "
		       "skip=0000002 stride=3\n");
    }
    pchar += sprintf(pchar, "END_OF_HEADER\n");
  }
  else{ /* Make a vtk-style header */

    pchar += sprintf(pchar, "# vtk DataFile Version 2.1\n");
    pchar += sprintf(pchar, "%s\n", title);
    pchar += sprintf(pchar, "BINARY\n");
    pchar += sprintf(pchar, "DATASET STRUCTURED_POINTS\n");
    pchar += sprintf(pchar, "DIMENSIONS %d %d %d\n", nx, ny, nz);
    pchar += sprintf(pchar, "ORIGIN  0.000   0.000   0.000\n");
    pchar += sprintf(pchar, "SPACING  1  1  1\n");
    pchar += sprintf(pchar, "POINT_DATA %d\n", nx*ny*nz);

    if(type == REG_DBL){
      sprintf(type_text, "double");
    }
    else if(type == REG_FLOAT){

      sprintf(type_text, "float");
    }
    else if(type == REG_INT){

      sprintf(type_text, "int");
    }
    else{

      fprintf(stderr, "Make_vtk_header: Unrecognised data type\n");
      return REG_FAILURE;
    }

    if(veclen == 1){

      pchar += sprintf(pchar, "SCALARS scalars %s\n", type_text);
    }
    else if(veclen == 3){

      pchar += sprintf(pchar, "VECTORS vectors %s\n", type_text);
    }
    else{

      fprintf(stderr, "Make_vtk_header: invalid veclen value: %d\n", veclen);
      return REG_FAILURE;
    }

    pchar += sprintf(pchar, "LOOKUP_TABLE default\n");
  }

  return REG_SUCCESS;
}

/*--------------------------------------------------------------------*/

int Make_chunk_header(char *header,
		      int   IOindex,
		      int   totx,int toty, int totz,
                      int   sx,  int sy,   int sz,
                      int   nx,  int ny,   int nz)
{
  char *pchar = header;

  pchar += sprintf(pchar, "CHUNK_HDR\n"
		   "ARRAY  %d %d %d\n"
		   "ORIGIN %d %d %d\n"
		   "EXTENT %d %d %d\n"
		   "FROM_FORTRAN %d\n"
		   "END_CHUNK_HDR\n", 
		   totx, toty, totz, 
		   sx, sy, sz, nx, ny, nz,
		   ReG_CalledFromF90);

  return REG_SUCCESS;
}

/*--------------------------------------------------------------------*/

int Steerer_connected()
{

#if REG_SOAP_STEERING

  return Steerer_connected_soap();

#else

  return Steerer_connected_file();
#endif
}

/*-------------------------------------------------------------------*/

int Send_status_msg(char *buf)
{
#if REG_DEBUG
  fprintf(stderr, "Send_status_msg: sending:\n>>%s<<\n", buf);
#endif

#if REG_SOAP_STEERING

  return Send_status_msg_soap(buf);

#else

  return Send_status_msg_file(buf);
#endif
}

/*-------------------------------------------------------------------*/

struct msg_struct *Get_control_msg()
{

#if REG_SOAP_STEERING

  return Get_control_msg_soap();

#else

  return Get_control_msg_file();

#endif

}

/*-------------------------------------------------------------------*/

int Initialize_steering_connection(int  NumSupportedCmds,
				   int *SupportedCmds)
{
  char *pchar;
  int   interval;

  /* Set-up the minimum interval (in seconds) between checks on
     whether a steerer has connected */
  Steerer_connection.polling_interval = REG_APP_POLL_INTERVAL_DEFAULT;

  if(pchar = getenv("REG_APP_POLL_INTERVAL")){

    if(sscanf(pchar, "%d", &interval) == 1){

      Steerer_connection.polling_interval = (double)interval;
    }
  }

#if REG_DEBUG
  fprintf(stderr, "Initialize_steering_connection: polling "
	  "interval = %d\n", (int)Steerer_connection.polling_interval);
#endif

#if REG_SOAP_STEERING

  return Initialize_steering_connection_soap(NumSupportedCmds,
					     SupportedCmds);

#else

  return Initialize_steering_connection_file(NumSupportedCmds,
					     SupportedCmds);
#endif

}

/*-------------------------------------------------------------------*/

int Set_steering_directory()
{
  char *pchar;
  int   i;
  int   len; 
  int   max_len;

  pchar = getenv("REG_STEER_DIRECTORY");

  if(pchar){

    /* Check that path ends in '/' - if not then add one */

    i = strlen(pchar);

    /* Check that we've got enough memory to hold full path
       for steering-message filenames */
    max_len = strlen(REG_LOG_FILENAME);
    if((len = strlen(APP_STEERABLE_FILENAME)) > max_len){
      max_len = len;
    }
    if((len = strlen(STR_CONNECTED_FILENAME)) > max_len){
      max_len = len;
    }
    if((len = strlen(APP_TO_STR_FILENAME "_1000.lock")) > max_len){
      max_len = len;
    }
    if((len = strlen(STR_TO_APP_FILENAME "_1000.lock")) > max_len){
      max_len = len;
    }

    if((i + max_len + 1) > REG_MAX_STRING_LENGTH){

      fprintf(stderr, "Set_steering_directory: "
	      "REG_MAX_STRING_LENGTH (%d chars) less\nthan predicted "
	      "string length of %d chars\n", REG_MAX_STRING_LENGTH, 
	      (i+max_len+1));
      return REG_FAILURE;
    }

    if( pchar[i-1] != '/' ){

      sprintf(Steerer_connection.file_root, "%s/", pchar);
    }
    else{

      strcpy(Steerer_connection.file_root, pchar);
    }

    if(Directory_valid(Steerer_connection.file_root) != REG_SUCCESS){

      fprintf(stderr, "Set_steering_directory: invalid scratch dir for "
	      "steering: %s\n", Steerer_connection.file_root);
      return REG_FAILURE;
    }
    else{
      fprintf(stderr, "Using following dir for steering scratch: %s\n", 
	     Steerer_connection.file_root);
    }
  }
  else{
    fprintf(stderr, "Set_steering_directory: failed to get "
	    "scratch directory\n");
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*-------------------------------------------------------------------*/

int Finalize_steering_connection()
{

#if REG_SOAP_STEERING

  return Finalize_steering_connection_soap();

#else
  int  commands[1];

  if(ReG_SteeringActive){

    commands[0] = REG_STR_DETACH;
    Emit_status(0,
		0,
		NULL,
		1,
		commands);
  }

  return Finalize_steering_connection_file();
#endif

}
/*---------------------------------------------------*/

int Make_supp_cmds_msg(int   NumSupportedCmds,
		       int  *SupportedCmds, 
                       char *msg)
{
  char *pchar;
  int   i;

  pchar = msg;

  Write_xml_header(&pchar);

  pchar += sprintf(pchar, "<Supported_commands>\n");

  for(i=0; i<NumSupportedCmds; i++){
    pchar += sprintf(pchar, "<Command>\n");
    pchar += sprintf(pchar, "<Cmd_id>%d</Cmd_id>\n", SupportedCmds[i]);
    pchar += sprintf(pchar, "</Command>\n");
  }

  pchar += sprintf(pchar, "</Supported_commands>\n");
  /* ARPDBG - we have no idea how much space msg points to
     so unfortunately have to use some random integer here.
     We're working on the assumption that the supported commands
     message is only ever going to be small... */
  Write_xml_footer(&pchar, 256);

  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Initialize_IOType_transport(const int direction,
				const int index)
{
#if REG_SOCKET_SAMPLES

  return Initialize_IOType_transport_sockets(direction, index);

#else

  return Initialize_IOType_transport_file(direction, index);
#endif
}

/*---------------------------------------------------*/

void Finalize_IOType_transport()
{

#if REG_SOCKET_SAMPLES

  Finalize_IOType_transport_sockets();

#endif

}

/*---------------------------------------------------*/

int Consume_start_data_check(const int index)
{

#if REG_SOCKET_SAMPLES

  return Consume_start_data_check_sockets(index);

#else

  return Consume_start_data_check_file(index);
#endif
}

/*---------------------------------------------------*/

int Consume_data_read(const int		index,  
		      const int		datatype,
		      const size_t	num_bytes_to_read, 
		      void		*pData)
{
  if(index < 0 || index >= IOTypes_table.num_registered){

    fprintf(stderr, "Consume_data_read: ERROR: IOType index out of range\n");
    return REG_FAILURE;
  }

#if REG_SOCKET_SAMPLES

  return Consume_data_read_sockets(index,
				   datatype,
				   num_bytes_to_read,
				   pData);

#else

  return Consume_data_read_file(index,
				datatype,
				num_bytes_to_read,
				pData);
#endif
}

/*---------------------------------------------------*/

int Emit_header(const int index)
{
#if REG_SOCKET_SAMPLES

  return Emit_header_sockets(index);
		   
#else

  sprintf(Global_scratch_buffer, REG_PACKET_FORMAT, REG_DATA_HEADER);
  Global_scratch_buffer[REG_PACKET_SIZE-1] = '\0';
#if REG_DEBUG
  fprintf(stderr, "Emit_header: Sending >>%s<<\n", Global_scratch_buffer);
#endif

  return Emit_data_file(index,
			REG_PACKET_SIZE,
			(void *)Global_scratch_buffer);
#endif
}

/*---------------------------------------------------*/

int Emit_footer(const int index,
		const char * const buffer)
{
  int nbytes_to_send;

  /* strlen + 1 because it doesn't count '\0' */
  nbytes_to_send = strlen(buffer)+1;

#if REG_SOCKET_SAMPLES

  return Emit_data_sockets(index, nbytes_to_send, (void*)buffer);

#else

  return Emit_data_file(index, nbytes_to_send, (void*)buffer);
#endif
}

/*---------------------------------------------------*/

int Emit_data(const int		index,  
	      const int		datatype,
	      const size_t	num_bytes_to_send,
	      void		*pData )
{
#if REG_SOCKET_SAMPLES

  return Emit_data_sockets(index, 
			   num_bytes_to_send,
			   pData);

#else

  return Emit_data_file(index, 
			num_bytes_to_send,
			pData);
#endif

}

/*---------------------------------------------------*/

int Get_communication_status(const int	index)
{

#if REG_SOCKET_SAMPLES

  return Get_communication_status_sockets(index);

#else

  if(IOTypes_table.io_def[index].fp){

    return REG_SUCCESS;
  }
  else{
    return REG_FAILURE;
  }
#endif

}

/*---------------------------------------------------*/

int Consume_iotype_msg_header(int  IOTypeIndex,
			      int *DataType,
			      int *Count,
			      int *NumBytes,
			      int *IsFortranArray)
{
  
  if(IOTypeIndex < 0 || IOTypeIndex >= IOTypes_table.num_registered){
    fprintf(stderr, "Consume_iotype_msg_header: IOType index out of range\n");
    return REG_FAILURE;
  }

#if REG_SOCKET_SAMPLES
  return Consume_msg_header_sockets(IOTypeIndex,
				    DataType,
				    Count,
				    NumBytes,
				    IsFortranArray);
#else
  return Consume_msg_header_file(IOTypeIndex,
				 DataType,
				 Count,
				 NumBytes,
				 IsFortranArray);
#endif
}

/*----------------------------------------------------------------*/

int Emit_iotype_msg_header(int IOTypeIndex,
			   int DataType,
			   int Count,
			   int NumBytes,
			   int IsFortranArray)
{
  char  buffer[7*REG_PACKET_SIZE];
  char  tmp_buffer[REG_PACKET_SIZE];
  char *pchar;

  pchar = buffer;
  pchar += sprintf(pchar, REG_PACKET_FORMAT, "<ReG_data_slice_header>");
  /* Put terminating char within the 128-byte packet */
  *(pchar-1) = '\0';
  sprintf(tmp_buffer, "<Data_type>%d</Data_type>", DataType);
  pchar += sprintf(pchar, REG_PACKET_FORMAT, tmp_buffer);
  *(pchar-1) = '\0';
  sprintf(tmp_buffer, "<Num_objects>%d</Num_objects>", Count);
  pchar += sprintf(pchar, REG_PACKET_FORMAT, tmp_buffer);
  *(pchar-1) = '\0';
  sprintf(tmp_buffer, "<Num_bytes>%d</Num_bytes>", NumBytes);
  pchar += sprintf(pchar, REG_PACKET_FORMAT, tmp_buffer);
  *(pchar-1) = '\0';
  if(IsFortranArray){
    sprintf(tmp_buffer, "<Array_order>FORTRAN</Array_order>");
  }
  else{
    sprintf(tmp_buffer, "<Array_order>C</Array_order>");
  }
  pchar += sprintf(pchar, REG_PACKET_FORMAT, tmp_buffer);
  *(pchar-1) = '\0';
  pchar += sprintf(pchar, REG_PACKET_FORMAT, "</ReG_data_slice_header>");
  *(pchar-1) = '\0';

#if REG_SOCKET_SAMPLES
  return Write_sockets(IOTypeIndex, (int) (pchar-buffer), (void*) buffer);

#else

  fwrite((void *)buffer, sizeof(char), (int)(pchar-buffer), 
	 IOTypes_table.io_def[IOTypeIndex].fp);
  return REG_SUCCESS;
#endif

}

/*----------------------------------------------------------------*/

int Realloc_iotype_buffer(int index,
			  int num_bytes)
{
  void *dum_ptr;

  if(index < 0)return REG_FAILURE;

#if REG_DEBUG
  if(IOTypes_table.io_def[index].buffer){
    fprintf(stderr, "Realloc_iotype_buffer: realloc'ing pointer %p\n", 
	    IOTypes_table.io_def[index].buffer);
  }
  else{
    fprintf(stderr, "Realloc_iotype_buffer: doing malloc for IO buffer\n");
  }
#endif

  if(IOTypes_table.io_def[index].buffer){

    if(!(dum_ptr = realloc(IOTypes_table.io_def[index].buffer, 
			 (size_t)num_bytes))){

      free(IOTypes_table.io_def[index].buffer);
      IOTypes_table.io_def[index].buffer = NULL;
      IOTypes_table.io_def[index].buffer_bytes = 0;
 
      fprintf(stderr, "Realloc_iotype_buffer: realloc failed for %d bytes\n", 
	      num_bytes);
      return REG_FAILURE;
    }

    IOTypes_table.io_def[index].buffer = dum_ptr;
  }
  else{
    if(!(IOTypes_table.io_def[index].buffer = malloc(num_bytes))){

      IOTypes_table.io_def[index].buffer_bytes = 0;
 
      fprintf(stderr, "Realloc_iotype_buffer: malloc failed for %d bytes\n", 
	      num_bytes);
      return REG_FAILURE;
    }
  }

  IOTypes_table.io_def[index].buffer_bytes = num_bytes;

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Reorder_array(int          ndims,
		  int         *tot_extent,
		  int         *sub_extent,
		  int         *origin,
		  int          type,
		  void        *pInData,
		  void        *pOutData,
		  int          to_f90)
{
  int         i, j, k;
  int         ox, oy, oz;
  int         nx, ny, nz;
  int         nslab, nrow;
  int        *pi, *pi_old;
  float      *pf, *pf_old;
  double     *pd, *pd_old;

  if(ndims != 3){

    fprintf(stderr, "Reorder_array: only 3D arrays supported\n");
    return REG_FAILURE;
  }

  ox = origin[0];
  oy = origin[1];
  oz = origin[2];
  nx = sub_extent[0];
  ny = sub_extent[1];
  nz = sub_extent[2];

  switch(type){

  case REG_INT:
    pi = (int *)pOutData;
    pi_old = (int *)pInData;

    if(to_f90 != TRUE){

      /* Convert F90 array to C array */
      nslab = tot_extent[2]*tot_extent[1];
      nrow  = tot_extent[2];

      /* Order loops so i,j,k vary as they should for an F90-style
	 array ordered consecutively in memory */
      for(k=oz; k<(nz+oz); k++){
	for(j=oy; j<(ny+oy); j++){
	  for(i=ox; i<(nx+ox); i++){
	    /* Calculate position of (i,j,k)'th element in a C array 
	       (where k varies most rapidly) and store value */
	    pi[i*nslab + j*nrow + k] = *(pi_old++);
	  }
	}
      }
    }
    else{
      
      /* Convert C array to F90 array */

      nslab = tot_extent[0]*tot_extent[1];
      nrow  = tot_extent[0];
	
      /* Order loops so i,j,k vary as they should for a C-style
	 array ordered consecutively in memory */
      for(i=ox; i<(nx+ox); i++){
	for(j=oy; j<(ny+oy); j++){
	  for(k=oz; k<(nz+oz); k++){
	    /* Calculate position of (i,j,k)'th element in an F90 array 
	       (where i varies most rapidly) and store value */
	    pi[k*nslab + j*nrow + i] = *(pi_old++);
	  }
	}
      }
    }
    break;

  case REG_FLOAT:

    pf = (float *)pOutData;
    pf_old = (float *)pInData;

    if(to_f90 != TRUE){

      /* Convert F90 array to C array */
	
      nslab = tot_extent[2]*tot_extent[1];
      nrow  = tot_extent[2];

      /* Order loops so i,j,k vary as they should for an F90-style
	 array ordered consecutively in memory */
      for(k=oz; k<(nz+oz); k++){
	for(j=oy; j<(ny+oy); j++){
	  for(i=ox; i<(nx+ox); i++){
	    /* Calculate position of (i,j,k)'th element in a C array 
	       (where k varies most rapidly) and store value */
	    pf[i*nslab + j*nrow + k] = *(pf_old++);
	  }
	}
      }
    }
    else{
	
      /* Convert C array to F90 array */
      
      nslab = tot_extent[0]*tot_extent[1];
      nrow  = tot_extent[0];

      /* Order loops so i,j,k vary as they should for a C-style
	 array ordered consecutively in memory */
      for(i=ox; i<(nx+ox); i++){
	for(j=oy; j<(ny+oy); j++){
	  for(k=oz; k<(nz+oz); k++){
	    /* Calculate position of (i,j,k)'th element in an F90 array 
	       (where i varies most rapidly) and store value */
	    pf[k*nslab + j*nrow + i] = *(pf_old++);
	  }
	}
      }
    }
    break;

  case REG_DBL:
    
    pd = (double *)pOutData;
    pd_old = (double *)pInData;

    if(to_f90 != TRUE){
	
      /* Convert F90 array to C array */
	
      nslab = tot_extent[2]*tot_extent[1];
      nrow  = tot_extent[2];

      /* Order loops so i,j,k vary as they should for an F90-style
	 array ordered consecutively in memory */
      for(k=oz; k<(nz+oz); k++){
	for(j=oy; j<(ny+oy); j++){
	  for(i=ox; i<(nx+ox); i++){
	    /* Calculate position of (i,j,k)'th element in a C array 
	       (where k varies most rapidly) and store value */
	    pd[i*nslab + j*nrow + k] = *(pd_old++);
	  }
	}
      }
    }
    else{
      
      /* Convert C array to F90 array */
	
      nslab = tot_extent[0]*tot_extent[1];
      nrow  = tot_extent[0];

      /* Order loops so i,j,k vary as they should for a C-style
	 array ordered consecutively in memory */
      for(i=ox; i<(nx+ox); i++){
	for(j=oy; j<(ny+oy); j++){
	  for(k=oz; k<(nz+oz); k++){
	    /* Calculate position of (i,j,k)'th element in an F90 array 
	       (where i varies most rapidly) and store value */
	    pd[k*nslab + j*nrow + i] = *(pd_old++);
	  }
	}
      }
    }
    break;
    
  default:
    break;
  }

  return REG_SUCCESS;
}
