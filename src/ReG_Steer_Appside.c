/*----------------------------------------------------------------------------
    This file contains routines and data structures for the construction
    of an interface to a steering component (from an application
    component).

    FILE-BASED implementation.

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

#include "ReG_Steer_Appside.h"
#include "ReG_Steer_Appside_internal.h"

/* Allow value of 'DEBUG' to propagate down from Reg_steer_types.h if
   it has been set there */
#ifndef DEBUG
#define DEBUG 0
#endif

/* UNICORE_DEMO must be defined in order to produce a steering lib
   compatible with the UNICORE steering demonstration framework.
   Only consequence is that the status files emitted by the application
   are all called 'steer_status' and not indexed - i.e. some output will 
   be lost if the status files are not consumed sufficiently rapidly */
/* #define UNICORE_DEMO */

/*---------------- Global data structures --------------------*/

/* Whether steering is enabled (set by user) */
static int ReG_SteeringEnabled = FALSE;
/* Whether the set of registered params has changed */
static int ReG_ParamsChanged   = FALSE;
/* Whether the set of registered IO types has changed */
static int ReG_IOTypesChanged  = FALSE;
/* Whether app. is currently being steered */
static int ReG_SteeringActive  = FALSE;
/* Whether steering library has been initialised */
static int ReG_SteeringInit    = FALSE;

static struct {

  char  file_root[REG_MAX_STRING_LENGTH];

} Steerer_connection;

/* IOdef_table_type is declared in ReG_Steer_Common.h since it is 
   used in both the steerer-side and app-side libraries */

IOdef_table_type IOTypes_table;

/* Param_table_type is declared in ReG_Steer_Common.h since it is 
   used in both the steerer-side and app-side libraries */

Param_table_type Params_table;

/*----------------------------------------------------------------*/

void Steering_enable(const int EnableSteer)
{
  /* Set global flag that controls whether steering is enabled or not */
  ReG_SteeringEnabled = EnableSteer;

  return;
}

/*----------------------------------------------------------------*/

int Steering_initialize(int  NumSupportedCmds,
			int *SupportedCmds)
{
  int   i;
  FILE *fp;
  char *pchar;
  char  filename[REG_MAX_STRING_LENGTH];

  /* Actually defined in ReG_Steer_Common.c because both steerer
     and steered have a variable of this name */
  extern char ReG_Steer_Schema_Locn[REG_MAX_STRING_LENGTH];

  /* Don't do anything if steering is not enabled */
  if (!ReG_SteeringEnabled) return REG_SUCCESS;

  /* Set the location of the file containing the schema describing all 
     steering communication */

  pchar = getenv("REG_STEER_HOME");

  if(pchar){

    /* Check that path ends in '/' - if not then add one */

    i = strlen(pchar);
    if( pchar[i-1] != '/' ){

      sprintf(ReG_Steer_Schema_Locn, "%s/xml_schema/reg_steer_comm.xsd",
                                     pchar);
    }
    else{

      sprintf(ReG_Steer_Schema_Locn, "%sxml_schema/reg_steer_comm.xsd",
                                     pchar);
    }
  }
  else{

    printf("Steering_initialize: failed to get schema location\n");
    return REG_FAILURE;
  }

  /* Set location of all comms files */

  pchar = getenv("REG_STEER_DIRECTORY");

  if(pchar){

    /* Check that path ends in '/' - if not then add one */

    i = strlen(pchar);
    if( pchar[i-1] != '/' ){

      sprintf(Steerer_connection.file_root, "%s/", pchar);
    }
    else{

      strcpy(Steerer_connection.file_root, pchar);
    }

    if(Directory_valid(Steerer_connection.file_root) != REG_SUCCESS){

      printf("Steering_initialize: invalid dir for steering messages: %s\n",
	     Steerer_connection.file_root);
      return REG_FAILURE;
    }
    else{
      printf("Using following dir for steering messages: %s\n", 
	     Steerer_connection.file_root);
    }
  }
  else{
    printf("Steering_initialize: failed to get scratch directory\n");
    return REG_FAILURE;
  }

  /* Delete any files that may have been written by a steering
     component previously */

  sprintf(filename, "%s%s", Steerer_connection.file_root, 
	  STR_TO_APP_FILENAME);
  Remove_files(filename);
  
  /* Allocate memory and initialize tables of IO types and 
     parameters */

  IOTypes_table.num_registered = 0;
  IOTypes_table.max_entries    = REG_INITIAL_NUM_IOTYPES;
  IOTypes_table.next_handle    = REG_MIN_IOTYPE_HANDLE;
  IOTypes_table.io_def         = (IOdef_entry *)
                                 malloc(IOTypes_table.max_entries
					*sizeof(IOdef_entry));
 
  if(IOTypes_table.io_def == NULL){
    
    printf("Steering_initialize: failed to allocate memory for IOType table\n");
    return REG_FAILURE;
  }

  for(i=0; i<IOTypes_table.max_entries; i++){

    IOTypes_table.io_def[i].handle = REG_IODEF_HANDLE_NOTSET;
  }

  /* Set up table for registered parameters */

  Params_table.num_registered = 0;
  Params_table.max_entries    = REG_INITIAL_NUM_PARAMS;
  Params_table.next_handle    = 0;
  Params_table.param          = (param_entry *)malloc(Params_table.max_entries
					      *sizeof(param_entry));

  if(Params_table.param == NULL){

    printf("Steering_initialize: failed to allocate memory for param table\n");
    return REG_FAILURE;
  }

  /* Initialise parameter handles */

  for(i=0; i<Params_table.max_entries; i++){

    Params_table.param[i].handle = REG_PARAM_HANDLE_NOTSET;
  }

  /* 'Sequence number' is treated as a parameter */
  Params_table.param[0].ptr       = NULL;
  Params_table.param[0].type      = REG_INT;
  Params_table.param[0].handle    = REG_SEQ_NUM_HANDLE;
  Params_table.param[0].steerable = FALSE;
  sprintf(Params_table.param[0].label, "REG_SEQ_NUM");
  sprintf(Params_table.param[0].value, "-1");
  Increment_param_registered(&Params_table);

  /* Clean up any old files */

  sprintf(filename, "%s%s", Steerer_connection.file_root, 
	  STR_CONNECTED_FILENAME);
  fp = fopen(filename, "w");
  if(fp != NULL){

    fclose(fp);
    if(remove(filename)){

      printf("Steering_initialize: failed to remove %s\n",filename);
    }
  }

  /* Signal that component is available to be steered */

  sprintf(filename, "%s%s", Steerer_connection.file_root, 
	                    APP_STEERABLE_FILENAME);
  fp = fopen(filename,"w");

  if(fp == NULL){

    printf("Steering_initialize: failed to open %s\n",
	   filename);
    return REG_FAILURE;
  }

  fprintf(fp, "<Supported_commands>\n");

  for(i=0; i<NumSupportedCmds; i++){
    fprintf(fp, "<Command>\n");
    fprintf(fp, "<Cmd_id>%d</Cmd_id>\n", SupportedCmds[i]);
    fprintf(fp, "</Command>\n");
  }

  fprintf(fp, "</Supported_commands>\n");

  fclose(fp);

  /* Flag that library has successfully been initialised */

  ReG_SteeringInit = TRUE;

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Steering_finalize()
{
  int  max, max1;
  int  commands[1];
  char sys_command[REG_MAX_STRING_LENGTH];

  /* Can only call this function if steering lib initialised */

  if (!ReG_SteeringInit) return REG_FAILURE;

  /* Tell the steerer that we are done */

  if(ReG_SteeringActive){

    commands[0] = REG_STR_DETACH;
    Emit_status(0,
		NULL,
		NULL,
		1,
		commands);
  }

  /* Clean-up IOTypes table */

  if(IOTypes_table.io_def != NULL){

    free(IOTypes_table.io_def);
    IOTypes_table.io_def = NULL;
  }

  IOTypes_table.num_registered = 0;
  IOTypes_table.max_entries = REG_INITIAL_NUM_IOTYPES;

  /* Clean-up parameters table */

  if(Params_table.param != NULL){
    free(Params_table.param);
    Params_table.param = NULL;
  }

  Params_table.num_registered = 0;
  Params_table.max_entries = REG_INITIAL_NUM_IOTYPES;

  /* Signal that component no-longer steerable */
  
  max = strlen(APP_STEERABLE_FILENAME);
  max1 = strlen(STR_CONNECTED_FILENAME);

  if(max1 > max) max=max1;
  
  max += strlen(Steerer_connection.file_root);
  if(max > REG_MAX_STRING_LENGTH ){

    printf("Steering_finalize: WARNING: truncating filename\n");
  }

  /* Delete the lock file that indicates we are steerable */
  sprintf(sys_command, "%s%s", Steerer_connection.file_root,
	  APP_STEERABLE_FILENAME);
  if(remove(sys_command)){

    printf("Steering_finalize: failed to remove %s\n", sys_command);
  }

  /* Delete the lock file that indicates we are being steered */
  if(ReG_SteeringActive){

    sprintf(sys_command, "%s%s", Steerer_connection.file_root,
	    STR_CONNECTED_FILENAME);
    if(remove(sys_command)){

      printf("Steering_finalize: failed to remove %s\n", sys_command);
    }
  }

  /* Delete any files we'd have consumed if we'd lived longer */
  sprintf(sys_command, "%s%s", Steerer_connection.file_root, 
	  STR_TO_APP_FILENAME);
  Remove_files(sys_command);

  /* Reset state of library */

  ReG_ParamsChanged  = FALSE;
  ReG_IOTypesChanged = FALSE;
  ReG_SteeringActive = FALSE;

  /* Flag that library no-longer initialised */
  ReG_SteeringInit    = FALSE;

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Register_IOTypes(int    NumTypes,
                     char* *IOLabel,
		     int   *type,
		     int   *support_auto_io,
		     int  **IOFrequency,
                     int   *IOType)
{
  int          i;
  int          current;
  int          new_size;
  IOdef_entry *dum_ptr;
  char*        iofreq_label;
  int          iofreq_strbl;
  int          iofreq_type;
  int          iparam;
  int          return_status = REG_SUCCESS;

  /* Can only call this function if steering lib initialised */

  if (!ReG_SteeringInit) return REG_FAILURE;

  /* Set variables required for registration of associated io
     frequency as a steerable parameter */

  iofreq_label = "IO_Frequency";
  iofreq_strbl = TRUE;
  iofreq_type  = REG_INT;

  /* IO types cannot be deleted so is safe to use num_registered to 
     get next free entry */
  current = IOTypes_table.num_registered;

  for(i=0; i<NumTypes; i++){

    strcpy(IOTypes_table.io_def[current].label, IOLabel[i]);

    /* Will need to check that IOLabel is something that the 
       component framework knows about */
  
    /* Currently have no way of looking up what filename to use so 
       hardwire... */

    if(IOLabel[i] == "VTK_STRUCTURED_POINTS_INPUT"){

      sprintf(IOTypes_table.io_def[current].filename, "data.vtk");
    }
    else{
      /* In the short term, use the label as the filename */
      strcpy(IOTypes_table.io_def[current].filename, IOLabel[i]);
    }

    /* Whether input or output (sample data) or a checkpoint */

    IOTypes_table.io_def[current].direction = type[i];

    /* Whether automatic emission/consumption is supported */

    IOTypes_table.io_def[current].auto_io_support = support_auto_io[i];

    /* We only expect a frequency variable to register if the application
       claims to support automatic emission/consumption */
    if(support_auto_io[i] == TRUE){

      Register_params(1,
		      &iofreq_label,
		      &iofreq_strbl,
		      (void **)(&(IOFrequency[i])),
		      &iofreq_type);

      /* Store the handle given to this parameter - this line must
	 immediately succeed the call to Register_params */

      IOTypes_table.io_def[current].freq_param_handle = 
	                                 Params_table.next_handle - 1;

      /* Annotate the parameter table entry just created to flag that
	 it is a parameter that is internal to the steering library */
      iparam = Param_index_from_handle(&Params_table, 
				       IOTypes_table.io_def[current].freq_param_handle);
      if(iparam != REG_PARAM_HANDLE_NOTSET){
	Params_table.param[iparam].is_internal = TRUE;
      }
      else{
#if DEBUG
	fprintf(stderr, "Register_IOTypes: failed to get handle for param\n");
#endif
	return_status = REG_FAILURE;
      }
      
    }
    else{

      IOTypes_table.io_def[current].freq_param_handle = 
	                                 REG_PARAM_HANDLE_NOTSET;
    }

    /* Create, store and return a handle for this IOType */
    IOTypes_table.io_def[current].handle = IOTypes_table.next_handle++;
    IOType[i] = IOTypes_table.io_def[current].handle;

    current++;

    if(current == IOTypes_table.max_entries){

      new_size = IOTypes_table.max_entries + REG_INITIAL_NUM_IOTYPES;

      dum_ptr = (IOdef_entry*)realloc((void *)(IOTypes_table.io_def),
		                      new_size*sizeof(IOdef_entry));

      if(dum_ptr == NULL){

        printf("Register_IOTypes: failed to allocate memory\n");
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

int Consume_start(int               IOType,
		  REG_IOHandleType *IOHandle)
{

  /* Can only call this function if steering lib initialised */

  if (!ReG_SteeringInit) return REG_FAILURE;

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Consume_stop(REG_IOHandleType *IOHandle)
{
  /* Can only call this function if steering lib initialised */

  if (!ReG_SteeringInit) return REG_FAILURE;

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Register_params(int    NumParams,
		    char* *ParamLabels,
		    int   *ParamSteerable,
		    void **ParamPtrs,
		    int   *ParamTypes)
{
  int           i;
  int           current;
  
  /* Can only call this function if steering lib initialised */

  if (!ReG_SteeringInit) return REG_FAILURE;

  for(i=0; i<NumParams; i++){

    /* Find next free entry - allocates more memory if required */
    current = Next_free_param_index(&Params_table);

    if(current == -1){

      printf("Register_params: failed to get find free param entry\n");
      return REG_FAILURE;
    }

    /* Store label */
    strcpy(Params_table.param[current].label, ParamLabels[i]);

    /* Store 'steerable' */
    Params_table.param[current].steerable = ParamSteerable[i];
    
    /* Store pointer */
    Params_table.param[current].ptr = ParamPtrs[i];

    /* Store type */
    Params_table.param[current].type = ParamTypes[i];

    /* This set to TRUE external to this routine if this param.
       has been created by the steering library itself */
    Params_table.param[current].is_internal = FALSE;

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
		     int    *SteerCommands)
{
  FILE  *fp;
  int    i;
  int    status;
  int    count;
  int    detached;
  int    return_status;
  int    num_commands;
  int    commands[REG_MAX_NUM_STR_CMDS];
  int    param_handles[REG_MAX_NUM_STR_PARAMS];
  char*  param_labels[REG_MAX_NUM_STR_PARAMS];
  char   filename[REG_MAX_STRING_LENGTH];

  /* Can only call this function if steering lib initialised */

  if (!ReG_SteeringInit) return REG_FAILURE;

  return_status     = REG_SUCCESS;
  *NumSteerParams   = 0;
  *NumSteerCommands = 0;

  /* Check that steering is enabled */

  if(!ReG_SteeringEnabled) return REG_SUCCESS;

  /* Check to see if a steerer is trying to get control */

  if(!ReG_SteeringActive){

    sprintf(filename, "%s%s", Steerer_connection.file_root, 
	    STR_CONNECTED_FILENAME);

    if( (fp = fopen(filename, "r")) ){

      fclose(fp);
      ReG_SteeringActive = TRUE;
      #if DEBUG
      printf("Steering_control: steerer has connected\n");
      #endif
    }
  }

  /* If we're being steered then... */

  if(ReG_SteeringActive){

    /* If registered params have changed since the last time then
       tell the steerer about the current set */
    if(ReG_ParamsChanged){
      
      Emit_param_defs();

      #if DEBUG
      printf("Steering_control: done Emit_param_defs\n");
      #endif

      ReG_ParamsChanged  = FALSE;
    }

    /* If the registered IO types have changed since the last time
       then tell the steerer about the current set */
    if(ReG_IOTypesChanged){

      Emit_IOType_defs();

      #if DEBUG
      printf("Steering_control: done Emit_IOType_defs\n");
      #endif

      ReG_IOTypesChanged = FALSE;
    }

    /* Read anything that the steerer has sent to us */
    if( Consume_control(&num_commands,
			commands,
			NumSteerParams,
			param_handles,
			param_labels) != REG_SUCCESS ){

      return_status = REG_FAILURE;

#if DEBUG
      printf("Steering_control: call to Consume_control failed\n");
#endif
    }

    /* Set array holding labels of changed params - pass back strings 
       rather than pointers to strings */

    for(i=0; i<(*NumSteerParams); i++){

      strcpy(SteerParamLabels[i], param_labels[i]);
    }

#if DEBUG
    printf("Steering_control: done Consume_control\n");
#endif

    /* Parse list of commands for any that we can handle ourselves */

    count = 0;
    i     = 0;
    detached = FALSE;

    while(i<num_commands){

      switch(commands[i]){

      case REG_STR_DETACH:

#if DEBUG
        printf("Steering_control: got detach command\n");
#endif

	if( Detach_from_steerer() != REG_SUCCESS){

	  return_status = REG_FAILURE;
	}
        detached = TRUE;
	break;

      default:

#if DEBUG
        printf("Steering_control: got command %d\n", commands[i]);
#endif

        SteerCommands[count] = commands[i];
	count++;

	/* If we've received a stop command then do just that - don't
	   mess about */

	if(commands[i] == REG_STR_STOP){

	  if( Detach_from_steerer() != REG_SUCCESS){

	    return_status = REG_FAILURE;
	  }
	  detached = TRUE;
	}

	break;
      }

      /* If we get a 'detach' command then don't process anything
         else */
      if(detached)break;

      i++;
    }

    /* Record how many commands we're going to pass back to caller */
    *NumSteerCommands = count;

    /* Tell the steerer what we've been doing */
    if( !detached ){

      /* Currently don't support returning a copy of the data just 
	 received from the steerer - hence NULL's below */
      status = Emit_status(SeqNum,
			   NULL, /* *NumSteerParams, */
			   NULL, /* param_handles,   */
			   *NumSteerCommands,
			   commands);

      if(status != REG_SUCCESS){

	fprintf(stderr, "Steering_control: call to Emit_status failed\n");
	return_status = REG_FAILURE;
      }
    }

  } /* End if steering active */

  return return_status;
}

/*----------------------------------------------------------------*/

int Steering_pause(int   *NumSteerParams,
		   char **SteerParamLabels,
		   int   *NumCommands,
		   int   *SteerCommands)
{
  int    paused        = TRUE;
  int    return_status = REG_SUCCESS;
  int    i, j;
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

    system("sleep 1");

    /* Read anything that the steerer has sent to us */

    if( Consume_control(&num_commands,
			commands,
			NumSteerParams,
			param_handles,
			param_labels) != REG_SUCCESS ){

      return_status = REG_FAILURE;
      paused = FALSE;
#if DEBUG
      fprintf(stderr, "Steering_pause: call to Consume_control failed\n");
#endif
    }
    else{

#if DEBUG
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
	  }

	  break;
	}
	else if(commands[i] == REG_STR_DETACH){

	  paused = FALSE;
	  return_status = Detach_from_steerer();
	  break;
	}
	else if(commands[i] == REG_STR_STOP){

	  paused = FALSE;

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

  FILE *fp;
  int   i;
  int   status;
  char  filename[REG_MAX_STRING_LENGTH];

  /* Check to see that we do actually have something to emit */
  if (Params_table.num_registered == 0) return REG_SUCCESS;

  /* Emit all currently registered parameters */

  status = Generate_status_filename(filename);
  
  if(status != REG_SUCCESS){
  
    return REG_FAILURE;
  }
  
  fp = fopen(filename, "w");
  
  if(fp == NULL){
    printf("Emit_param_defs: Failed to open file\n");
    return REG_FAILURE;
  }
  
  Write_xml_header(fp);
  fprintf(fp,"<Param_defs>\n");
  
  for(i=0; i<Params_table.max_entries; i++){
  
    /* Check handle because if a parameter is deleted then this is
  	 flagged by unsetting its handle */
  
    if(Params_table.param[i].handle != REG_PARAM_HANDLE_NOTSET){
  
      /* Update the 'value' part of this parameter's table entry */
      if(Get_ptr_value(&(Params_table.param[i])) == REG_SUCCESS){
    	 
	fprintf(fp,"<Param>\n");
	fprintf(fp,"<Label>%s</Label>\n", Params_table.param[i].label);
	fprintf(fp,"<Steerable>%d</Steerable>\n", 
		Params_table.param[i].steerable);
	fprintf(fp,"<Type>%d</Type>\n", Params_table.param[i].type);
	fprintf(fp,"<Handle>%d</Handle>\n",Params_table.param[i].handle);
	fprintf(fp,"<Value>%s</Value>\n", Params_table.param[i].value);

	if(Params_table.param[i].is_internal == TRUE){

	  fprintf(fp,"<Is_internal>TRUE</Is_internal>\n");
	}
	else{

	  fprintf(fp,"<Is_internal>FALSE</Is_internal>\n");
	}

	fprintf(fp,"</Param>\n");
      }
    }
  }
  
  fprintf(fp,"</Param_defs>\n");
  Write_xml_footer(fp);
  fclose(fp);
  Create_lock_file(filename);

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Emit_IOType_defs(){

  int   status;
  int   i;
  FILE *fp;
  char  filename[REG_MAX_STRING_LENGTH];

  /* Check that we do actually have something to emit */
  if (IOTypes_table.num_registered == 0) return REG_SUCCESS;

  /* Emit all currently registered IOTypes */

  status = Generate_status_filename(filename);
  
  if(status != REG_SUCCESS){
  
    return REG_FAILURE;
  }
  
  fp = fopen(filename, "w");
  
  if(fp == NULL){
    fprintf(stderr, "Emit_IOType_defs: Failed to open file\n");
    return REG_FAILURE;
  }
  
  Write_xml_header(fp);
  fprintf(fp,"<IOType_defs>\n");
  
  for(i=0; i<IOTypes_table.max_entries; i++){
  
    if(IOTypes_table.io_def[i].handle != REG_IODEF_HANDLE_NOTSET){
  
      fprintf(fp,"<IOType>\n");
      fprintf(fp,"<Label>%s</Label>\n", IOTypes_table.io_def[i].label);
      fprintf(fp,"<Handle>%d</Handle>\n", IOTypes_table.io_def[i].handle);

      switch(IOTypes_table.io_def[i].direction){

      case REG_IO_IN:
        fprintf(fp,"<Direction>IN</Direction>\n");
	break;

      case REG_IO_OUT:
        fprintf(fp,"<Direction>OUT</Direction>\n");
	break;

      case REG_IO_CHKPT:
        fprintf(fp,"<Direction>CHECKPOINT</Direction>\n");
	break;

      default:
#if DEBUG
	fprintf(stderr, 
		"Emit_IOType_defs: Unrecognised IOType direction\n");
#endif
	fclose(fp);
	remove(filename);
	return REG_FAILURE;
      }

      if(IOTypes_table.io_def[i].auto_io_support){

	fprintf(fp,"<Support_auto_io>TRUE</Support_auto_io>\n");

	fprintf(fp,"<Freq_handle>%d</Freq_handle>\n",
		IOTypes_table.io_def[i].freq_param_handle);
      }
      else{

	fprintf(fp,"<Support_auto_io>FALSE</Support_auto_io>\n");
      }

      fprintf(fp,"</IOType>\n");
    }
  }
  
  fprintf(fp,"</IOType_defs>\n");
  Write_xml_footer(fp);
  fclose(fp);
  
  Create_lock_file(filename);

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Consume_control(int    *NumCommands,
		    int    *Commands,
		    int    *NumSteerParams,
		    int    *SteerParamHandles,
		    char  **SteerParamLabels){

  FILE                *fp;
  int                  i, j;
  int                  count;
  int                  return_status;
  char                 filename[REG_MAX_STRING_LENGTH];

  /* For XML parser */

  size_t               len;
  char                 buf[BUFSIZ];
  XML_Parser           parser = XML_ParserCreate(NULL);
  int                  done;
  user_data_type       User_data;
  param_xml_struct     param_struct;
  supp_cmds_xml_struct cmd_struct;
  control_xml_struct   ctrl_struct;
  Supp_cmd_table_type  recvd_cmds;
  Param_table_type     recvd_params;

  return_status = REG_SUCCESS;

  /* Set up tables and pointers to receive data */

  recvd_params.num_registered = 0;
  recvd_params.max_entries    = REG_MAX_NUM_STR_PARAMS;
  recvd_params.param = (param_entry *)malloc(REG_MAX_NUM_STR_PARAMS*
					     sizeof(param_entry));

  if(recvd_params.param == NULL){

    printf("Consume_control: failed to allocate memory");
    return REG_MEM_FAIL;
  }

  recvd_cmds.num_registered = 0;
  recvd_cmds.max_entries    = REG_MAX_NUM_STR_CMDS;
  recvd_cmds.cmd = (supp_cmd_entry *)malloc(REG_MAX_NUM_STR_CMDS*
					    sizeof(supp_cmd_entry));

  if(recvd_cmds.cmd == NULL){

    printf("Consume_control: failed to allocate memory");
    return REG_MEM_FAIL;
  }

  param_struct.table       = &recvd_params;
  param_struct.field_type  = PARAM_NOTSET;
  cmd_struct.table         = &recvd_cmds;
  cmd_struct.field_type    = CMD_NOTSET;
  ctrl_struct.param_struct = &param_struct;
  ctrl_struct.cmd_struct   = &cmd_struct;
  ctrl_struct.field_type   = CTRL_NOTSET;
  User_data.gen_xml_struct = (void *)(&ctrl_struct);
  User_data.msg_type       = MSG_NOTSET;

  /* Initialise XML parser */

  XML_SetUserData(parser, &User_data);
  XML_SetElementHandler(parser, startElement, endElement);
  XML_SetCharacterDataHandler(parser, dataHandler);

  /* Read the file produced by the steerer - may contain commands and/
     or new parameter values */

  sprintf(filename, "%s%s", Steerer_connection.file_root, STR_TO_APP_FILENAME);

  if( (fp = Open_next_file(filename)) != NULL){

    do {
  
      len  = fread(buf, 1, sizeof(buf), fp);
      done = len < sizeof(buf);
  
      if (!XML_Parse(parser, buf, len, done)) {
  
  	  printf("%s at line %d\n",
  	         XML_ErrorString(XML_GetErrorCode(parser)),
  	         XML_GetCurrentLineNumber(parser));
  	  return REG_FAILURE;
      }
    } while (!done);
  
    XML_ParserFree(parser);
    fclose(fp);
  
    /* Delete the file once we've read it */

    if( Delete_file(filename) != REG_SUCCESS){

      printf("Consume_control: failed to delete %s\n",filename);
    }

    /* Copy data out of structures */

    *NumCommands = recvd_cmds.num_registered;

#if DEBUG
    printf("Consume_control: received %d commands\n", *NumCommands);
#endif

    for(i=0; i<(*NumCommands); i++){

      Commands[i] = recvd_cmds.cmd[i].cmd_id;
#if DEBUG
      printf("Consume_control: cmd[%d] = %d\n", i, Commands[i]);
#endif
    }

    *NumSteerParams = recvd_params.num_registered;
  
#if DEBUG
    printf("Consume_control: received %d params\n", *NumSteerParams);
#endif

    count = 0;
    i = 0;
    while(count < (*NumSteerParams)){
    
      for(j=0; j<Params_table.max_entries; j++){
  
  	if(Params_table.param[j].handle == recvd_params.param[count].handle){
	  
	  break;
  	}
      }

      if(j == Params_table.max_entries){
  
  	printf("Consume_control: failed to match param handles\n");
  	return_status = REG_FAILURE;
      }
      else{

  	/* Store char representation of new parameter value */
	if( strlen(recvd_params.param[count].value) ){

	  strcpy(Params_table.param[j].value, recvd_params.param[count].value);
  
	  /* Update value associated with pointer */
	  Update_ptr_value(&(Params_table.param[j]));

	  /* Return list of handles to inform caller of what's changed 
	     unless parameter is internal to the library */
	  if( !(Params_table.param[j].is_internal) ){

	    SteerParamHandles[i] = recvd_params.param[count].handle;
	    SteerParamLabels[i]  = Params_table.param[j].label;
	    i++;
	  }
	}
	else{
	  printf("Consume_control: empty parameter value field\n");
	}
      }

      count++;
    }

    /* Update the number of parameters received to allow for fact that
       some may be internal and are not passed up to the calling routine */
    *NumSteerParams = i;
  }
  else{

#if DEBUG
    printf("Consume_control: failed to open file %s\n", filename);
#endif

    /* No file found */

    *NumSteerParams = 0;
    *NumCommands = 0;
  }

  /* Clean up */

  free(recvd_params.param);
  recvd_params.param = NULL;
  free(recvd_cmds.cmd);
  recvd_cmds.cmd = NULL;

  return return_status;
}

/*----------------------------------------------------------------*/

int Generate_status_filename(char* filename)
{

#ifdef UNICORE_DEMO

  /* Always just output <path>/steer_status for UNICORE demo */
  sprintf(filename, "%ssteer_status", Steerer_connection.file_root);

#else /* Not UNICORE demo - use full, indexed filenames */

  static int output_file_index = 0;

  /* Generate next filename in sequence for sending data to
     steerer & increment counter */

  sprintf(filename, "%s%s_%d", Steerer_connection.file_root, 
	  APP_TO_STR_FILENAME, output_file_index++);

  /* Wrap counter if no. of distinct files exceeded */

  if(output_file_index == REG_MAX_NUM_FILES) output_file_index = 0;

#endif /* UNICORE_DEMO */

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Detach_from_steerer()
{
  char  filename[REG_MAX_STRING_LENGTH];

  /* Remove lock file that indicates app is being steered */

  sprintf(filename, "%s%s", Steerer_connection.file_root, 
	  STR_CONNECTED_FILENAME);
  remove(filename);

  /* Remove any files that steerer has produced that we won't
     now be consuming */

  sprintf(filename, "%s%s", Steerer_connection.file_root, 
	  STR_TO_APP_FILENAME);
  Remove_files(filename);
  
  ReG_SteeringActive = FALSE;
  ReG_IOTypesChanged = TRUE;
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
  FILE *fp;
  int   i;
  int   pcount = 0;
  int   tot_pcount = 0;
  int   ccount = 0;
  int   num_param;
  int   cmddone   = FALSE;
  int   paramdone = FALSE;
  char  filename[REG_MAX_STRING_LENGTH];

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

  if(NumCommands == 0) cmddone = TRUE;
  if(num_param == 0) paramdone = TRUE;

  /* Loop until all params and commands have been emitted */

  while(!paramdone || !cmddone){

    Generate_status_filename(filename);

    if( (fp = fopen(filename, "w")) == NULL){

      printf("Emit_status: failed to open file\n");
      return REG_FAILURE;
    }

    Write_xml_header(fp);
    fprintf(fp, "<App_status>\n");

    /* Parameter values section */

    if(!paramdone){

      /* Loop over max. no. of params to write to any given file */

      for(i=0; i<REG_MAX_NUM_STR_PARAMS; i++){
  
    	/* Handle value used to indicate whether entry is valid */
    	if(Params_table.param[tot_pcount].handle != REG_PARAM_HANDLE_NOTSET){

	  /* Changed to emit ALL parameters, ARP 19.08.2002 */
	  /*  && (!Params_table.param[tot_pcount].steerable) ){ */
  
 	  /* Update the 'value' part of this parameter's table entry */
 	  if(Params_table.param[tot_pcount].handle != REG_SEQ_NUM_HANDLE){
 	    Get_ptr_value(&(Params_table.param[tot_pcount]));
 	  }
 	  else{
 	    /* Update stored value of sequence number */
 	    sprintf(Params_table.param[tot_pcount].value, "%d", SeqNum);
 	  }
  
 	  fprintf(fp, "<Param>\n");
 	  fprintf(fp, "<Handle>%d</Handle>\n", 
 		  Params_table.param[tot_pcount].handle);
 	  fprintf(fp, "<Value>%s</Value>\n", 
		  Params_table.param[tot_pcount].value);
 	  fprintf(fp, "</Param>\n");
  
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

#if DEBUG
      fprintf(stderr, "Emit_status: NumCommands = %d, ccount = %d\n", 
	      NumCommands, ccount);
#endif

      for(i=0; i<REG_MAX_NUM_STR_CMDS; i++){
  
    	fprintf(fp, "<Command>\n");
	fprintf(fp, "<Cmd_id>%d</Cmd_id>\n", Commands[ccount]);
	fprintf(fp, "</Command>\n");
    	ccount++;
  
    	if(ccount >= NumCommands){
 	  cmddone = TRUE;
 	  break;
    	}
      }
    }

    fprintf(fp, "</App_status>\n");

    Write_xml_footer(fp);
    fclose(fp);

    Create_lock_file(filename);
  }

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Update_ptr_value(param_entry *param)
{

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
    strcpy((char *)(param->ptr), param->value);
    break;

  default:
    printf("Update_ptr_value: unrecognised parameter type\n");
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
  if(param->handle == REG_SEQ_NUM_HANDLE){

    return REG_SUCCESS;
  }

  switch(param->type){
  
  case REG_INT:
    sprintf(param->value,"%d", *((int *)(param->ptr)));
    break;
  
  case REG_FLOAT:
    sprintf(param->value,"%f", *((float *)(param->ptr)));
    break;
  
  case REG_DBL:
    sprintf(param->value,"%lf", *((double *)(param->ptr)));
    break;
  
  case REG_CHAR:
    strcpy(param->value, (char *)(param->ptr));
    break;
  
  default:
    printf("Get_ptr_value: unrecognised parameter type\n");
    printf("Param type   = %d\n", param->type);
    printf("Param handle = %d\n", param->handle);
    printf("Param label  = %s\n", param->label);

    return_status = REG_FAILURE;
    break;
  }

  return return_status;
}





