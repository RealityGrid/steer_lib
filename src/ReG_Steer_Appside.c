/*----------------------------------------------------------------------------
    This file contains routines and data structures for the construction
    of an interface to a steering component (from an application
    component).

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

    Initial version by:   A Porter, 23.7.2002
---------------------------------------------------------------------------*/

#include "ReG_Steer_Appside.h"
#include "ReG_Steer_Appside_internal.h"
#include "ReG_Steer_Globus_io.h"

#include <signal.h>
#include <unistd.h>
#include <time.h>
#include <rpc/rpc.h>
#include <math.h>


/* Allow value of 'DEBUG' to propagate down from Reg_steer_types.h if
   it has been set there */
#ifndef DEBUG
#define DEBUG 0
#endif

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
  char *pchar;

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

    fprintf(stderr, "Steering_initialize: failed to get schema location\n");
    return REG_FAILURE;
  }
  
  /* Allocate memory and initialise tables of IO types and 
     parameters */

  IOTypes_table.num_registered = 0;
  IOTypes_table.max_entries    = REG_INITIAL_NUM_IOTYPES;
  IOTypes_table.next_handle    = REG_MIN_IOTYPE_HANDLE;
  IOTypes_table.io_def         = (IOdef_entry *)
                                 malloc(IOTypes_table.max_entries
					*sizeof(IOdef_entry));
 
  if(IOTypes_table.io_def == NULL){
    
    fprintf(stderr, "Steering_initialize: failed to allocate memory "
	    "for IOType table\n");
    return REG_FAILURE;
  }

  for(i=0; i<IOTypes_table.max_entries; i++){

    IOTypes_table.io_def[i].handle = REG_IODEF_HANDLE_NOTSET;
  }

  /* Initialise table of open IO channels */

  for(i=0; i<REG_INITIAL_NUM_IOTYPES; i++){
    IO_channel[i].buffer = NULL;
  }

  /* Set up table for registered parameters */

  Params_table.num_registered = 0;
  Params_table.max_entries    = REG_INITIAL_NUM_PARAMS;
  Params_table.next_handle    = 0;
  Params_table.param          = (param_entry *)malloc(Params_table.max_entries
					      *sizeof(param_entry));

  if(Params_table.param == NULL){

    fprintf(stderr, "Steering_initialize: failed to allocate memory "
	    "for param table\n");
    free(IOTypes_table.io_def);
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
  Params_table.param[0].modified  = FALSE;
  Params_table.param[0].is_internal=FALSE;
  sprintf(Params_table.param[0].label, "REG_SEQ_NUM");
  sprintf(Params_table.param[0].value, "-1");
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
  sprintf(Params_table.param[i].value, "-1.0");
  Increment_param_registered(&Params_table);

  /* Set-up/prepare for connection to steering client */
  if(Initialize_steering_connection(NumSupportedCmds, 
				    SupportedCmds) != REG_SUCCESS){

    free(IOTypes_table.io_def);
    free(Params_table.param);
    return REG_FAILURE;
  }

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

  /* Flag that library has been successfully initialised */

  ReG_SteeringInit = TRUE;

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Steering_finalize()
{
  int  index;
  int  commands[1];

  /* Can only call this function if steering lib initialised */

  if (!ReG_SteeringInit) return REG_FAILURE;

  /* Tell the steerer that we are done */

  if(ReG_SteeringActive){

    commands[0] = REG_STR_DETACH;
    Emit_status(0,
		0,
		NULL,
		1,
		commands);
  }

  /* Clean-up IOTypes table */

  if(IOTypes_table.io_def != NULL){

    for (index=0; index<IOTypes_table.num_registered; index++) {
      if (IOTypes_table.io_def[index].direction == REG_IO_OUT) {
	/* close globus sockets */
	Globus_cleanup_listener_connection(&(IOTypes_table.io_def[index].socket_info));
      }
      else if (IOTypes_table.io_def[index].direction == REG_IO_IN) {
	/* close globus sockets */
	Globus_cleanup_connector_connection(&(IOTypes_table.io_def[index].socket_info));
      }
    }
    /* deactivate globus module */
    Globus_io_deactivate();

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
  Finalize_steering_connection();

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
		     int   *direction,
		     int   *IOFrequency,
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
  void        *ptr_array[1];
  int          return_status = REG_SUCCESS;
  char	      *pchar;
  int	       len;
  int	       hostname_ok = 0;
  int	       port_ok = 0;


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

    IOTypes_table.io_def[current].direction = direction[i];

    /* Set variables required for registration of associated io
       frequency as a steerable parameter */

    iofreq_label = "IO_Frequency";
    iofreq_strbl = TRUE;
    iofreq_type  = REG_INT;
    IOTypes_table.io_def[current].frequency = IOFrequency[i];
    ptr_array[0] = (void *)&(IOTypes_table.io_def[current].frequency);

    Register_params(1,
		    &iofreq_label,
		    &iofreq_strbl,
		    ptr_array,
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
      
    /* deal with the globus sockets for sample data */
    IOTypes_table.io_def[current].buffer = NULL;
    IOTypes_table.io_def[current].use_xdr = FALSE;

    /* set up socket_info for callback */
    Globus_socket_info_init(&(IOTypes_table.io_def[current].socket_info));

    if (direction[i] == REG_IO_OUT)
    {
      /* open socket and register callback function to listen for and
	 accept connections */
      if (Globus_create_listener(&(IOTypes_table.io_def[current].socket_info)) != REG_SUCCESS) {
#if DEBUG
	fprintf(stderr, "Register_IOTypes: failed to create listener "
		"for IOType\n");
#endif
	return_status = REG_FAILURE;
      }
      else{

#if DEBUG
	fprintf(stderr, "Register_IOTypes: Created listener on port %d, index %d, label %s\n", IOTypes_table.io_def[current].socket_info.port, current, IOLabel[i] );
#endif
	/* attempt to kick the callback function (in case accept callback) */
	Globus_callback_poll(&(IOTypes_table.io_def[current].socket_info));
      }
	
    }
    else if (direction[i] == REG_IO_IN)
    {
      /* register connector against port */
      /* get hostname and port from environment variables */
      
      pchar = getenv("REG_CONNECTOR_HOSTNAME");
      if (pchar) {
	len = strlen(pchar);
	if (len < REG_MAX_STRING_LENGTH) {
	  sprintf(connector_hostname, pchar);
	  connector_hostname[len]='\0';
	  hostname_ok = 1;
	}
      }
      /* SMR XXX add error handling */
      pchar = getenv("REG_CONNECTOR_PORT");
      if (pchar) {
	connector_port = atoi(pchar);
	port_ok = 1;
      }

      if (port_ok && hostname_ok) {

	if (Globus_create_connector(&(IOTypes_table.io_def[current].socket_info)) != REG_SUCCESS) {
#if DEBUG
	fprintf(stderr, "Register_IOTypes: failed to register connector "
		"for IOType\n");
#endif
	  return_status = REG_FAILURE;
	}
	else{

#if DEBUG
	  fprintf(stderr, "Register_IOTypes: registered connector on "
		  "port %d, hostname = %s, index %d, label %s\n", 
		  connector_port, connector_hostname, current, IOLabel[i] );
#endif

	}
      }
      else
	fprintf(stderr, "Register_IOTypes: cannot create connector as "
		"port and hostname not set\n");
    }

    /* Create, store and return a handle for this IOType */
    IOTypes_table.io_def[current].handle = IOTypes_table.next_handle++;
    IOType[i] = IOTypes_table.io_def[current].handle;

    /* ARPDBG */
#if DEBUG
    fprintf(stderr, "Register_IOTypes: handle = %d\n", IOType[i]);
    fprintf(stderr, "Register_IOTypes: index  = %d\n", current);
#endif

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

int Consume_start(int  IOType,
		  int *IOTypeIndex)
{
  char buffer[REG_PACKET_SIZE];
  globus_size_t   nbytes;
  globus_result_t result;


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

  /* if not connected attempt to connect now */
  if (IOTypes_table.io_def[*IOTypeIndex].socket_info.comms_status != REG_COMMS_STATUS_CONNECTED)
    Globus_attempt_connector_connect(&(IOTypes_table.io_def[*IOTypeIndex].socket_info));


  /* check if socket connection has been made */
  if (IOTypes_table.io_def[*IOTypeIndex].socket_info.comms_status == 
      REG_COMMS_STATUS_CONNECTED) {

#if DEBUG
    fprintf(stderr, "Consume_start: socket status is connected, index = %d\n",
	    *IOTypeIndex );
#endif

    /* Check for data on socket - non-blocking */

    result = globus_io_try_read(&(IOTypes_table.io_def[*IOTypeIndex].socket_info.conn_handle),
				(globus_byte_t *)buffer,
				REG_PACKET_SIZE,
				&nbytes);

    if (result != GLOBUS_SUCCESS){
#if DEBUG
      fprintf(stderr, "Consume_start: globus_io_try_read failed - "
	      "try immediate reconnect for index %d\n", *IOTypeIndex);
#endif
    
      Globus_retry_connect(&(IOTypes_table.io_def[*IOTypeIndex].socket_info));
     
      /* check if socket reconnection has been made and check for data if it has */
      if (IOTypes_table.io_def[*IOTypeIndex].socket_info.comms_status == 
	  REG_COMMS_STATUS_CONNECTED) {
	result = globus_io_try_read(&(IOTypes_table.io_def[*IOTypeIndex].socket_info.conn_handle),
				    (globus_byte_t *)buffer,
				    REG_PACKET_SIZE,
				    &nbytes);
      }
      else {
#if DEBUG
      fprintf(stderr, "Consume_start: reconnect attempt failed - "
	      "socket is not connected status id %d\n", 
	      IOTypes_table.io_def[*IOTypeIndex].socket_info.comms_status);
#endif
      return REG_FAILURE;

      }

    }

    if(result == GLOBUS_SUCCESS){

#if DEBUG
      fprintf(stderr, "Consume_start: read <%s> from socket\n", buffer);
#endif
      /* ARPDBG - globus_io_try_read always returns 0 bytes if connection
	 configugured to use GSSAPI or SSL data wrapping. */
      if(nbytes > 0){

	if(!strncmp(buffer, REG_DATA_HEADER, strlen(REG_DATA_HEADER))){

	  IOTypes_table.io_def[*IOTypeIndex].buffer_bytes = REG_IO_BUFSIZE;
	  IOTypes_table.io_def[*IOTypeIndex].buffer =
	                                     (void *)malloc(REG_IO_BUFSIZE);

	  if(!IOTypes_table.io_def[*IOTypeIndex].buffer) return REG_FAILURE;

	  return REG_SUCCESS;
	}
      }
      

    }
    else {
    
#if DEBUG
    fprintf(stderr, "Consume_start: reconnect attempt success - but "
	    "globus_io_try_read failed\n");
#endif
    }

    return REG_FAILURE;
    
  }
  else {
#if DEBUG
    fprintf(stderr, "Consume_start: socket is NOT connected, index = "
	    "%d\n",*IOTypeIndex );
#endif
  }

  return REG_FAILURE;
}

/*----------------------------------------------------------------*/

int Consume_stop(int *IOTypeIndex)
{
  int             return_status = REG_SUCCESS;

  /* Check that steering is enabled */
  if(!ReG_SteeringEnabled) return REG_SUCCESS;

  /* Can only call this function if steering lib initialised */
  if (!ReG_SteeringInit) return REG_FAILURE;

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
  globus_result_t  result;
  globus_size_t    nbytes;
  char             buffer[REG_PACKET_SIZE];

  char *begin_slice_header = "<ReG_data_slice_header>";
  char *end_slice_header   = "</ReG_data_slice_header>";

  /* Check that steering is enabled */
  if(!ReG_SteeringEnabled) return REG_SUCCESS;

  /* Can only call this function if steering lib initialised */
  if (!ReG_SteeringInit) return REG_FAILURE;

  /* check socket connection has been made */
  if (IOTypes_table.io_def[IOTypeIndex].socket_info.comms_status != 
      REG_COMMS_STATUS_CONNECTED) return REG_FAILURE;

  /* Read header */

#if DEBUG
  /* ARPDBG */
  fprintf(stderr, "Consume_data_slice_header: calling globus_io_read...\n");
#endif

  /* Blocks until REG_PACKET_SIZE bytes received */
  result = globus_io_read(&(IOTypes_table.io_def[IOTypeIndex].socket_info.conn_handle), 
			  (globus_byte_t *)buffer, 
			  REG_PACKET_SIZE, 
			  REG_PACKET_SIZE, 
			  &nbytes);

  if(result != GLOBUS_SUCCESS){

    fprintf(stderr, "Consume_data_slice_header: globus_io_read failed\n");
    return REG_FAILURE;
  }

#if DEBUG
  fprintf(stderr, "Consume_data_slice_header: read <%s> from socket\n",
	  buffer);
#endif

  /* Check for end of data */
  if(!strncmp(buffer, REG_DATA_FOOTER, strlen(REG_DATA_FOOTER))){

    return REG_EOD;
  }
  else if(strncmp(buffer, begin_slice_header, strlen(begin_slice_header))){

    fprintf(stderr, "Consume_data_slice_header: incorrect header on slice\n");
    return REG_FAILURE;
  }

  result = globus_io_read(&(IOTypes_table.io_def[IOTypeIndex].socket_info.conn_handle), 
			  (globus_byte_t *)buffer, 
			  REG_PACKET_SIZE, 
			  REG_PACKET_SIZE, 
			  &nbytes);

  if(result != GLOBUS_SUCCESS){

    fprintf(stderr, "Consume_data_slice_header: globus_io_read failed\n");
    return REG_FAILURE;
  }

#if DEBUG
  fprintf(stderr, "Consume_data_slice_header: read <%s> from socket\n", 
	  buffer);
#endif

  if(!strstr(buffer, "<Data_type>")){

    return REG_FAILURE;
  }

  sscanf(buffer, "<Data_type>%d</Data_type>", DataType);

  /* Use of XDR is internal to library so make sure user doesn't
     get confused.  use_xdr flag set here for use in subsequent call
     to consume_data_slice */
  switch(*DataType){

  case REG_XDR_INT:
    IOTypes_table.io_def[IOTypeIndex].use_xdr = TRUE;
    *DataType = REG_INT;
    break;

  case REG_XDR_FLOAT:
    IOTypes_table.io_def[IOTypeIndex].use_xdr = TRUE;
    *DataType = REG_FLOAT;
    break;

  case REG_XDR_DOUBLE:
    IOTypes_table.io_def[IOTypeIndex].use_xdr = TRUE;
    *DataType = REG_DBL;
    break;

  default:
    break;
  }

  result = globus_io_read(&(IOTypes_table.io_def[IOTypeIndex].socket_info.conn_handle), 
			  (globus_byte_t *)buffer, 
			  REG_PACKET_SIZE, 
			  REG_PACKET_SIZE, 
			  &nbytes);

  if(result != GLOBUS_SUCCESS){

    return REG_FAILURE;
  }

#if DEBUG
  fprintf(stderr, "Consume_data_slice_header: read <%s> from socket\n", 
	  buffer);
#endif

  if(!strstr(buffer, "<Num_objects>")){

    return REG_FAILURE;
  }

  if( sscanf(buffer, "<Num_objects>%d</Num_objects>", Count) != 1){

    fprintf(stderr, "Consume_data_slice_header: failed to read Num_objects\n");
    return REG_FAILURE;
  }

  result = globus_io_read(&(IOTypes_table.io_def[IOTypeIndex].socket_info.conn_handle), 
			  (globus_byte_t *)buffer, 
			  REG_PACKET_SIZE, 
			  REG_PACKET_SIZE, 
			  &nbytes);

  if(result != GLOBUS_SUCCESS){

    fprintf(stderr, "Consume_data_slice_header: globus_io_read failed\n");
    return REG_FAILURE;
  }

#if DEBUG
  fprintf(stderr, "Consume_data_slice_header: read <%s> from socket\n", 
	  buffer);
#endif

  if(strncmp(buffer, end_slice_header, strlen(end_slice_header))){

    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Consume_data_slice(int    IOTypeIndex,
		       int    DataType,
		       int    Count,
		       void  *pData)
{
  int              i;
  int              return_status = REG_SUCCESS;
  globus_object_t *err; 
  globus_result_t  result;
  globus_size_t    nbytes;
  globus_size_t    num_bytes_to_read;

  XDR     xdrs;
  int    *pint;
  float  *pfloat;
  double *pdouble;
  void   *tmp_ptr;
#if DEBUG
  float   read_time;
  clock_t start_time, stop_time;
#endif

  /* Calculate how many bytes to expect */
  switch(DataType){

  case REG_INT:
    if(IOTypes_table.io_def[IOTypeIndex].use_xdr){
      num_bytes_to_read = Count*REG_SIZEOF_XDR_INT;
    }
    else{
      num_bytes_to_read = Count*sizeof(int);
    }
    break;

  case REG_FLOAT:
    if(IOTypes_table.io_def[IOTypeIndex].use_xdr){
      num_bytes_to_read = Count*REG_SIZEOF_XDR_FLOAT;
    }
    else{
      num_bytes_to_read = Count*sizeof(float);
    }
    break;

  case REG_DBL:
    if(IOTypes_table.io_def[IOTypeIndex].use_xdr){
      num_bytes_to_read = Count*REG_SIZEOF_XDR_DOUBLE;
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

    return REG_FAILURE;
    break;
  }

  /* Check that input buffer is large enough (only an issue if have XDR-
     encoded data) */
  if(IOTypes_table.io_def[IOTypeIndex].use_xdr){
    if(IOTypes_table.io_def[IOTypeIndex].buffer_bytes < num_bytes_to_read){

      tmp_ptr = realloc(IOTypes_table.io_def[IOTypeIndex].buffer, 
			(size_t)num_bytes_to_read);
            
      if(!tmp_ptr){
	fprintf(stderr, "Consume_data_slice: failed to realloc input "
		"buffer - consume failed\n");

	/* Reset use_xdr flag set as only valid on a per-slice basis */
	IOTypes_table.io_def[IOTypeIndex].use_xdr = FALSE;

	return REG_FAILURE;
      }

      IOTypes_table.io_def[IOTypeIndex].buffer_bytes = num_bytes_to_read;
      IOTypes_table.io_def[IOTypeIndex].buffer = tmp_ptr;
    }
  }

  /* Read this number of bytes */

#if DEBUG
  fprintf(stderr, "Consume_data_slice: calling globus_io_read for %d bytes\n",
	  num_bytes_to_read);

  start_time = clock();
#endif


  if(IOTypes_table.io_def[IOTypeIndex].use_xdr){
    result = globus_io_read(&(IOTypes_table.io_def[IOTypeIndex].socket_info.conn_handle), 
			    IOTypes_table.io_def[IOTypeIndex].buffer, 
			    num_bytes_to_read, 
			    num_bytes_to_read, 
			    &nbytes);
  }
  else{
    result = globus_io_read(&(IOTypes_table.io_def[IOTypeIndex].socket_info.conn_handle), 
			    pData, 
			    num_bytes_to_read, 
			    num_bytes_to_read, 
			    &nbytes);
  }

#if DEBUG
  stop_time = clock();
  read_time = (float)(start_time - stop_time)/(float)CLOCKS_PER_SEC;

  fprintf(stderr, "Consume_data_slice: globus_io_read read %d bytes\n",
	  nbytes);
  fprintf(stderr, "                    in %.3f seconds\n", read_time);

  if(DataType == REG_CHAR){
    fprintf(stderr, "Consume_data_slice: got char data:\n>>%s<<\n", 
	    (char *)pData);
  }
#endif

  if(result != GLOBUS_SUCCESS){

    fprintf(stderr, "Consume_data_slice: error globus_io_read\n");
    err =  globus_error_get(result);

    if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_NULL_PARAMETER,
				 globus_object_get_type(err))) {
     fprintf(stderr, " - globus error type: null parameter\n");
    }
    else if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_NOT_INITIALIZED,
				      globus_object_get_type(err))) {
     fprintf(stderr, " - globus error type: not initialised\n");
    }
    else if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_CLOSE_ALREADY_REGISTERED,
				      globus_object_get_type(err))) {
     fprintf(stderr, " - globus error type: close already registered\n");
    }
    else if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_READ_ALREADY_REGISTERED, 
				      globus_object_get_type(err))) {
     fprintf(stderr, " - globus error type: read already registered\n");
    }
    else if (globus_object_type_match(GLOBUS_ERROR_TYPE_TYPE_MISMATCH,
				      globus_object_get_type(err))) {
     fprintf(stderr, " - globus error type: type mismatch\n");
    }

    /* Reset use_xdr flag set as only valid on a per-slice basis */
    IOTypes_table.io_def[IOTypeIndex].use_xdr = FALSE;

    return REG_FAILURE;
  }

  if(IOTypes_table.io_def[IOTypeIndex].use_xdr){

#if DEBUG
    fprintf(stderr, "Consume_data_slice: doing XDR decode\n");
#endif

    xdrmem_create(&xdrs, 
		  IOTypes_table.io_def[IOTypeIndex].buffer,
		  num_bytes_to_read,
		  XDR_DECODE);

    switch(DataType){

    case REG_INT:
      
      pint = (int *)pData;

      for(i=0; i<Count; i++){

	if (1!=xdr_int(&xdrs, pint++)) {
	  fprintf(stderr, "Consume_data_slice: error decoding datum %d\n",i);
	  return_status = REG_FAILURE;
	  break;
	}
      }
      break;

    case REG_FLOAT:
      
      pfloat = (float *)pData;

      for(i=0; i<Count; i++){

	if (1!=xdr_float(&xdrs, pfloat++)) {
	  fprintf(stderr, "Consume_data_slice: error decoding datum %d\n",i);
	  return_status = REG_FAILURE;
	  break;
	}
      }
      break;

    case REG_DBL:
      
      pdouble = (double *)pData;

      for(i=0; i<Count; i++){

	if (1!=xdr_double(&xdrs, pdouble++)) {
	  fprintf(stderr, "Consume_data_slice: error decoding datum %d\n",i);
	  return_status = REG_FAILURE;
	  break;
	}
      }
      break;

    default:
      fprintf(stderr, "Consume_data_slice: unexpected datatype\n");
      return_status = REG_FAILURE;
      break;
    }

    xdr_destroy(&xdrs);
  }

#if DEBUG
  fprintf(stderr, "Consume_data_slice: done XDR decode\n");
#endif

  /* Reset use_xdr flag set as only valid on a per-slice basis */
  IOTypes_table.io_def[IOTypeIndex].use_xdr = FALSE;

  return return_status;
}


/*----------------------------------------------------------------*/

int Consume_data_slice_old(REG_IOHandleType  IOHandle,
		       int              *DataType,
		       int              *Count,
		       void            **pData)
{
  int return_status = REG_SUCCESS;
  int index;

  /* ARPDBG - for testing */
  static int test_count_call = 0;
  int   i, j, k, ivar;
  int   nx = 16;
  int   ny = 16;
  int   nz = 16;
  char *pdata;
  float value;
  double sum;
  XDR   xdrs;
  /* ...end of test variables */

  index = (int)(IOHandle);

  if( IO_channel[index].buffer == NULL ){

    fprintf(stderr, "Consume_data_slice: io channel not open\n");
    fprintf(stderr, "                    IOHandle = %d\n", index);
    return REG_FAILURE;
  }

  /* ARPDGB For testing purposes ONLY */
  fprintf(stderr, "Consume_data_slice: test_count_call = %d\n", 
	  test_count_call);

  if(test_count_call == 0){

    *DataType = REG_CHAR;

    pdata = (char *)(IO_channel[index].buffer);
    pdata += sprintf(pdata, "%-128s", "# AVS field file\n");
    pdata += sprintf(pdata, "%-128s", "ndim=3\n");
    pdata += sprintf(pdata, "%-128s", "dim1= 16\n");
    pdata += sprintf(pdata, "%-128s", "dim2= 16\n");
    pdata += sprintf(pdata, "%-128s", "dim3= 16\n");
    pdata += sprintf(pdata, "%-128s", "nspace=3\n");
    pdata += sprintf(pdata, "%-128s", "field=uniform\n");
    pdata += sprintf(pdata, "%-128s", "veclen=2\n");
    pdata += sprintf(pdata, "%-128s", "data=xdr_float\n");
    pdata += sprintf(pdata, "%-128s", "variable 1 filetype=binary "
    		   "skip=0000000 stride=2\n");
    pdata += sprintf(pdata, "%-128s", "variable 2 filetype=binary "
    		   "skip=0000008 stride=2\n");
    pdata += sprintf(pdata, "%-128s", "END_OF_HEADER\n");

    *Count = strlen((char *)(IO_channel[index].buffer));

    *pData = IO_channel[index].buffer;
  }
  else if(test_count_call == 1){

    fprintf(stderr, "Consume_data_slice: Creating xdr data buffer...\n");

    xdrmem_create(&xdrs, IO_channel[index].buffer, REG_IO_BUFSIZE, 
		  XDR_ENCODE);

    sum = 0.0;
    for(i=0; i<nx/2; i++){
      for(j=0; j<ny; j++){
	for(k=0; k<nz; k++){
	  for(ivar=0; ivar<2; ivar++){

	    value = (float)sqrt((double)(i*i*(ivar+1)*0.1 + j*j*0.1 +k*k*0.1));

	    sum += value;
	    if( xdr_float(&xdrs, &(value)) != 1){
	      fprintf(stderr, "Failed to write xdr datum no. %d\n", i);
	      break;
	    }
	  }
	}
      }
    }

    xdr_destroy(&xdrs);

    *DataType = REG_FLOAT;
    *Count = nx*ny*nz;
    *pData = IO_channel[index].buffer;
  }
  else if(test_count_call == 2){

    xdrmem_create(&xdrs, IO_channel[index].buffer, REG_IO_BUFSIZE, 
		  XDR_ENCODE);

    for(i=nx/2; i<nx; i++){
      for(j=0; j<ny; j++){
	for(k=0; k<nz; k++){
	  for(ivar=0; ivar<2; ivar++){

	    value = sqrt((float)(i*i*(ivar+1)*0.1 + j*j*0.1 +k*k*0.1));

	    if( xdr_float(&xdrs, &(value)) != 1){
	      fprintf(stderr, "Failed to write xdr datum no. %d\n", i);
	      break;
	    }
	  }
	}
      }

    }

    xdr_destroy(&xdrs);

    *DataType = REG_FLOAT;
    *Count = nx*ny*nz;
    /**pData = &(((double *)IO_channel[index].buffer)[nx*ny*nz]);
    *pData = &(((char *)IO_channel[index].buffer)[nx*ny*nz*8]);*/
    *pData = IO_channel[index].buffer;
  }
  else{

    return_status = REG_EOD;
  }

  /* ARPDBG */
  test_count_call++;

  return return_status;
}

/*----------------------------------------------------------------*/

int Emit_start(int  IOType,
	       int  SeqNum,
	       int  UseXDR,
	       int *IOTypeIndex)
{
  char            buffer[REG_PACKET_SIZE];
  char            fmt[16];
  globus_size_t   nbytes;
  globus_result_t result;

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

  /* Set whether or not to encode as XDR */
  IOTypes_table.io_def[*IOTypeIndex].use_xdr = UseXDR;

  /* We'll need additional memory to perform conversion to XDR */
  if(UseXDR){

    IOTypes_table.io_def[*IOTypeIndex].buffer = (void *)malloc(REG_IO_BUFSIZE);

    if(!IOTypes_table.io_def[*IOTypeIndex].buffer){

      return REG_FAILURE;
    }
  }

  /* check if socket connection has been made */
  if (IOTypes_table.io_def[*IOTypeIndex].socket_info.comms_status != REG_COMMS_STATUS_CONNECTED) 
    Globus_attempt_listener_connect(&(IOTypes_table.io_def[*IOTypeIndex].socket_info));


  if (IOTypes_table.io_def[*IOTypeIndex].socket_info.comms_status == REG_COMMS_STATUS_CONNECTED) {

#if DEBUG
    fprintf(stderr, "Emit_start: socket status is connected, index = %d\n",
	    *IOTypeIndex );
#endif

    /* Send header */
    
    sprintf(fmt, "%s%ds", "%-", REG_PACKET_SIZE);
    sprintf(buffer, fmt, REG_DATA_HEADER);

#if DEBUG
    fprintf(stderr, "Emit_start: Sending >>%s<<\n", buffer);
#endif

    result = globus_io_write(&(IOTypes_table.io_def[*IOTypeIndex].socket_info.conn_handle), 
			     (globus_byte_t *)buffer, 
			     strlen(buffer), 
			     &nbytes);

    if(result == GLOBUS_SUCCESS){

      return REG_SUCCESS;
    }
    else{

#if DEBUG
      fprintf(stderr, "Emit_start: globus_io_write failed - immediate retry connect\n");
#endif

      Globus_retry_accept_connect(&(IOTypes_table.io_def[*IOTypeIndex].socket_info));
      if (IOTypes_table.io_def[*IOTypeIndex].socket_info.comms_status == REG_COMMS_STATUS_CONNECTED) {  

#if DEBUG
    fprintf(stderr, "Emit_start: Sending >>%s<<\n", buffer);
#endif    
	result = globus_io_write(&(IOTypes_table.io_def[*IOTypeIndex].socket_info.conn_handle), 
				 (globus_byte_t *)buffer, 
				 strlen(buffer), 
				 &nbytes);
	
	if(result == GLOBUS_SUCCESS){

	  return REG_SUCCESS;
	}
	
#if DEBUG
      fprintf(stderr, "Emit_start: globus_io_write failed\n");
#endif

      }

      /* ARPDBG add check on error code */
      return REG_FAILURE;
    }
  }
  else {
#if DEBUG
    fprintf(stderr, "Emit_start: socket not connected, index = %d\n",
	    *IOTypeIndex );
#endif
    return REG_FAILURE;
  }

}

/*----------------------------------------------------------------*/

int Emit_stop(int *IOTypeIndex)
{
  char            buffer[REG_PACKET_SIZE];
  char            fmt[16];
  globus_size_t   nbytes;
  globus_result_t result;
  int             return_status = REG_SUCCESS;

  /* Check that steering is enabled */
  if(!ReG_SteeringEnabled) return REG_SUCCESS;

  /* Can only call this function if steering lib initialised */
  if (!ReG_SteeringInit) return REG_FAILURE;

  /* Send footer */
  sprintf(fmt, "%s%ds", "%-", REG_PACKET_SIZE);
  sprintf(buffer, fmt, REG_DATA_FOOTER);

  /* ARPDBG */
  fprintf(stderr, "Emit_stop: Sending >>%s<<\n", buffer);

  result = globus_io_write(&(IOTypes_table.io_def[*IOTypeIndex].socket_info.conn_handle), 
			   (globus_byte_t *)buffer, 
			   strlen(buffer), 
			   &nbytes);
  
  if(result != GLOBUS_SUCCESS){

    fprintf(stderr, "Emit_stop: call to globus_io_write failed\n");
    return_status = REG_FAILURE;
  }

  /* Free associated memory (will have been allocated if conversion to
     XDR performed) */
  if(IOTypes_table.io_def[*IOTypeIndex].buffer){

    free(IOTypes_table.io_def[*IOTypeIndex].buffer);
    IOTypes_table.io_def[*IOTypeIndex].buffer = NULL;
  }

  *IOTypeIndex = REG_IODEF_HANDLE_NOTSET;

  return return_status;
}

/*----------------------------------------------------------------*/

int Emit_data_slice(int		      IOTypeIndex,
		    int               DataType,
		    int               Count,
		    void             *pData)
{
  int              return_status = REG_SUCCESS;
  int              datatype;
  int              i;
  globus_object_t *err; 
  globus_result_t  result;
  globus_size_t    nbytes;
  globus_size_t    num_bytes_to_send;
  char             buffer[5*REG_PACKET_SIZE];
  char             tmp_buffer[REG_PACKET_SIZE];
  char            *pchar;
  char             fmt[16];

  XDR              xdrs;
  int             *iptr;
  float           *fptr;
  double          *dptr;

  /* Check that steering is enabled */
  if(!ReG_SteeringEnabled) return REG_SUCCESS;

  /* Can only call this function if steering lib initialised */
  if (!ReG_SteeringInit) return REG_FAILURE;

  /* check socket connection has been made */
  if (IOTypes_table.io_def[IOTypeIndex].socket_info.comms_status != 
      REG_COMMS_STATUS_CONNECTED) return REG_FAILURE;

  /* Check data type, calculate number of bytes to send and convert
     to XDR if required */
  switch(DataType){

  case REG_INT:
    if(IOTypes_table.io_def[IOTypeIndex].use_xdr){
      datatype = REG_XDR_INT;
      num_bytes_to_send = Count*REG_SIZEOF_XDR_INT;

      xdrmem_create(&xdrs, 
		    IOTypes_table.io_def[IOTypeIndex].buffer,
		    num_bytes_to_send,
		    XDR_ENCODE);

      iptr = (int *)pData;

      for(i=0; i<Count; i++){

	xdr_int(&xdrs, iptr++);
      }

      xdr_destroy(&xdrs);
    }
    else{
      datatype = DataType;
      num_bytes_to_send = Count*sizeof(int);
    }
    break;

  case REG_FLOAT:
    if(IOTypes_table.io_def[IOTypeIndex].use_xdr){
      datatype = REG_XDR_FLOAT;
      num_bytes_to_send = Count*REG_SIZEOF_XDR_FLOAT;

      xdrmem_create(&xdrs, 
		    IOTypes_table.io_def[IOTypeIndex].buffer,
		    num_bytes_to_send,
		    XDR_ENCODE);

      fptr = (float *)pData;

      for(i=0; i<Count; i++){

	xdr_float(&xdrs, fptr++);
      }

      xdr_destroy(&xdrs);
    }
    else{
      datatype = DataType;
      num_bytes_to_send = Count*sizeof(float);
    }
    break;

  case REG_DBL:
    if(IOTypes_table.io_def[IOTypeIndex].use_xdr){
      datatype = REG_XDR_DOUBLE;
      num_bytes_to_send = Count*REG_SIZEOF_XDR_DOUBLE;

      xdrmem_create(&xdrs, 
		    IOTypes_table.io_def[IOTypeIndex].buffer,
		    num_bytes_to_send,
		    XDR_ENCODE);

      dptr = (double *)pData;

      for(i=0; i<Count; i++){

	xdr_double(&xdrs, dptr++);
      }

      xdr_destroy(&xdrs);
    }
    else{
      datatype = DataType;
      num_bytes_to_send = Count*sizeof(double);
    }
    break;

  case REG_CHAR:
    datatype = DataType;
    num_bytes_to_send = Count*sizeof(char);
    break;

  default:
    fprintf(stderr, "Emit_data_slice: Unrecognised data type\n");
    return REG_FAILURE;
    break;
  }
 
  /* Send ReG-specific header */

  sprintf(fmt, "%s%ds", "%-", REG_PACKET_SIZE);

  pchar = (char *)(&buffer);
  pchar += sprintf(pchar, fmt, "<ReG_data_slice_header>");
  sprintf(tmp_buffer, "<Data_type>%d</Data_type>", datatype);
  pchar += sprintf(pchar, fmt, tmp_buffer);
  sprintf(tmp_buffer, "<Num_objects>%d</Num_objects>", Count);
  pchar += sprintf(pchar, fmt, tmp_buffer);
  pchar += sprintf(pchar, fmt, "</ReG_data_slice_header>");

#if DEBUG
  fprintf(stderr, "Emit_data_slice: sending >>%s<<\n", buffer);
#endif

  result = globus_io_write(&(IOTypes_table.io_def[IOTypeIndex].socket_info.conn_handle), 
			   (globus_byte_t *)buffer, 
			   strlen(buffer), 
			   &nbytes);

  if (result != GLOBUS_SUCCESS ) {

#if DEBUG
    fprintf(stderr, "Emit_data_slice: failed to write slice header\n");
#endif

    /* ARPDBG - some stuff here */
    return REG_FAILURE;
  }

  /* Send data */

  if(IOTypes_table.io_def[IOTypeIndex].use_xdr && DataType != REG_CHAR){
    /* XDR-encoded data stored in buffer associated with IO channel */
    result = globus_io_write(&(IOTypes_table.io_def[IOTypeIndex].socket_info.conn_handle), 
			     IOTypes_table.io_def[IOTypeIndex].buffer, 
			     num_bytes_to_send, 
			     &nbytes);
  }
  else{
    /* Just send raw data as provided by calling routine */
    result = globus_io_write(&(IOTypes_table.io_def[IOTypeIndex].socket_info.conn_handle), 
			     pData, 
			     num_bytes_to_send, 
			     &nbytes);
  }

#if DEBUG
  fprintf(stderr, "Emit_data_slice: sent %d bytes...\n", nbytes);
#endif

  if (result != GLOBUS_SUCCESS ) {
    fprintf(stderr, "Emit_data_slice: error globus_io_write\n");
    err =  globus_error_get(result);

    if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_NULL_PARAMETER,
				 globus_object_get_type(err))) {
     fprintf(stderr, " - globus error type: null parameter\n");
    }
    else if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_NOT_INITIALIZED,
				      globus_object_get_type(err))) {
     fprintf(stderr, " - globus error type: not initialised\n");
    }
    else if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_CLOSE_ALREADY_REGISTERED,
				      globus_object_get_type(err))) {
     fprintf(stderr, " - globus error type: close already registered\n");
    }
    else if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_WRITE_ALREADY_REGISTERED, 
				      globus_object_get_type(err))) {
     fprintf(stderr, " - globus error type: write already registered\n");
    }
    else if (globus_object_type_match(GLOBUS_ERROR_TYPE_TYPE_MISMATCH,
				      globus_object_get_type(err))) {
     fprintf(stderr, " - globus error type: type mismatch\n");
    }
    else if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_BAD_PROTECTION,
				      globus_object_get_type(err))) {
     fprintf(stderr, " - globus error type: bad protection\n");
    }
    else if (globus_object_type_match(GLOBUS_IO_ERROR_TYPE_SYSTEM_FAILURE,
				      globus_object_get_type(err))) {
     fprintf(stderr, " - globus error type: system failure\n");
    }

    return REG_FAILURE;
  }

  return return_status;
}

/*----------------------------------------------------------------*/

int Register_params(int    NumParams,
		    char* *ParamLabels,
		    int   *ParamSteerable,
		    void **ParamPtrs,
		    int   *ParamTypes)
{
  int i;
  int current;
  
  /* Check that steering is enabled */

  if(!ReG_SteeringEnabled) return REG_SUCCESS;

  /* Can only call this function if steering lib initialised */

  if (!ReG_SteeringInit) return REG_FAILURE;

  for(i=0; i<NumParams; i++){

    /* Find next free entry - allocates more memory if required */
    current = Next_free_param_index(&Params_table);

    if(current == -1){

      fprintf(stderr, "Register_params: failed to get find free "
	      "param entry\n");
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
		     int    *SteerCommands,
		     char  **SteerCmdParams)
{
  int    i;
  int    status;
  int    count;
  int    detached;
  int    return_status;
  int    num_commands;
  int    commands[REG_MAX_NUM_STR_CMDS];
  int    param_handles[REG_MAX_NUM_STR_PARAMS];
  char*  param_labels[REG_MAX_NUM_STR_PARAMS];

  /* Variables for timing */
  float          time_per_step;
  clock_t        new_time;
  static clock_t previous_time = 0;
  static int     first_time    = TRUE;

  return_status     = REG_SUCCESS;
  *NumSteerParams   = 0;
  *NumSteerCommands = 0;

  /* Check that steering is enabled */

  if(!ReG_SteeringEnabled) return REG_SUCCESS;

  /* Can only call this function if steering lib initialised */

  if (!ReG_SteeringInit) return REG_FAILURE;

  /* Check to see if a steerer is trying to get control */

  if(!ReG_SteeringActive){

    if(Steerer_connected() == REG_SUCCESS){

      ReG_SteeringActive = TRUE;
      first_time = TRUE;
#if DEBUG
      fprintf(stderr, "Steering_control: steerer has connected\n");
#endif
    }
  }

  /* If we're being steered then... */

  if(ReG_SteeringActive){

    /* Update any library-controlled monitored variables */
    i = Param_index_from_handle(&(Params_table), REG_SEQ_NUM_HANDLE);
    if(i != -1){
      sprintf(Params_table.param[i].value, "%d", SeqNum);
      Params_table.param[i].modified = TRUE;
    }

    i = Param_index_from_handle(&(Params_table), REG_STEP_TIME_HANDLE);
    if(i != -1){

      new_time = clock();
      time_per_step = (float)(new_time - previous_time)/(float)CLOCKS_PER_SEC;
      previous_time = new_time;

      /* First value we get will be rubbish because need two passes 
	 through to get a valid difference... */
      if(!first_time){
	sprintf(Params_table.param[i].value, "%.3f", time_per_step);
        Params_table.param[i].modified = TRUE;
      }
      else{
	first_time = FALSE;
      }    
    }

    /* If registered params have changed since the last time then
       tell the steerer about the current set */
    if(ReG_ParamsChanged){
      
      Emit_param_defs();

#if DEBUG
      fprintf(stderr, "Steering_control: done Emit_param_defs\n");
#endif

      ReG_ParamsChanged  = FALSE;
    }

    /* If the registered IO types have changed since the last time
       then tell the steerer about the current set */
    if(ReG_IOTypesChanged){

      Emit_IOType_defs();

#if DEBUG
      fprintf(stderr, "Steering_control: done Emit_IOType_defs\n");
#endif

      ReG_IOTypesChanged = FALSE;
    }

    /* Read anything that the steerer has sent to us */
    if( Consume_control(&num_commands,
			commands,
			SteerCmdParams,
			NumSteerParams,
			param_handles,
			param_labels) != REG_SUCCESS ){

      return_status = REG_FAILURE;

#if DEBUG
      fprintf(stderr, "Steering_control: call to Consume_control failed\n");
#endif
    }

    /* Set array holding labels of changed params - pass back strings 
       rather than pointers to strings */

    for(i=0; i<(*NumSteerParams); i++){

      strcpy(SteerParamLabels[i], param_labels[i]);
    }

#if DEBUG
    fprintf(stderr, "Steering_control: done Consume_control\n");
#endif

    /* Parse list of commands for any that we can handle ourselves */

    count = 0;
    i     = 0;
    detached = FALSE;

    while(i<num_commands){

      switch(commands[i]){

      case REG_STR_DETACH:

#if DEBUG
        fprintf(stderr, "Steering_control: got detach command\n");
#endif

	if( Detach_from_steerer() != REG_SUCCESS){

	  return_status = REG_FAILURE;
	}

	/* Confirm that we have received the detach command */
	commands[0] = REG_STR_DETACH;
	Emit_status(SeqNum,
		    0,   
		    NULL,
		    1,
		    commands);

        detached = TRUE;
	break;

      default:

#if DEBUG
        fprintf(stderr, "Steering_control: got command %d\n", commands[i]);
#endif

        SteerCommands[count] = commands[i];
	strcpy(SteerCmdParams[count], SteerCmdParams[i]);
	count++;

	/* If we've received a stop command then do just that - don't
	   mess about */

	if(commands[i] == REG_STR_STOP){

	  if( Detach_from_steerer() != REG_SUCCESS){

	    return_status = REG_FAILURE;
	  }

	  /* Confirm that we have received the stop command */
	  commands[0] = REG_STR_STOP;
          Emit_status(SeqNum,
		      0,   
		      NULL,
		      1,
		      commands);

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

      /* Deal with automatic emission/consumption of data */

      /* IOTypes cannot be deleted so the num_registered entries that
	 we have will be contiguous within the table */
      for(i=0; i<IOTypes_table.num_registered; i++){

	/* A freq. of zero indicates no automatic emit/consume */
	if(IOTypes_table.io_def[i].frequency == 0) continue;

	if(fmod(SeqNum, IOTypes_table.io_def[i].frequency) < REG_TOL_ZERO){

	  if( count >= REG_MAX_NUM_STR_CMDS ){
	    fprintf(stderr, "Steering_control: WARNING: discarding "
		    "steering cmds as max number (%d) exceeded\n", 
		    REG_MAX_NUM_STR_CMDS);

	    return_status = REG_FAILURE;
	    break;
	  }
 
	  /* Add command to list to send back to caller */
	  /* ARPDBG - this isn't going to work for checkpoints as things
	     stand 'cos we need to specify IN or OUT in the associated
	     param entry */
	  SteerCommands[count]  = IOTypes_table.io_def[i].handle;
	  SteerCmdParams[count] = " ";
	  count++;
	}
      }

    }

    /* Record how many commands we're going to pass back to caller */
    *NumSteerCommands = count;

  } /* End if steering active */

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

  int   i;
  char  buf[REG_MAX_MSG_SIZE];
  char *pbuf;

  /* Check to see that we do actually have something to emit */
  if (Params_table.num_registered == 0) return REG_SUCCESS;

  /* Emit all currently registered parameters */
  
  pbuf = buf;
  Write_xml_header(&pbuf);

  pbuf += sprintf(pbuf,"<Param_defs>\n");
  
  for(i=0; i<Params_table.max_entries; i++){
  
    /* Check handle because if a parameter is deleted then this is
  	 flagged by unsetting its handle */
  
    if(Params_table.param[i].handle != REG_PARAM_HANDLE_NOTSET){
  
      /* Update the 'value' part of this parameter's table entry */
      if(Get_ptr_value(&(Params_table.param[i])) == REG_SUCCESS){
    	 
	pbuf += sprintf(pbuf,"<Param>\n");
	pbuf += sprintf(pbuf,"<Label>%s</Label>\n", 
			Params_table.param[i].label);
	pbuf += sprintf(pbuf,"<Steerable>%d</Steerable>\n", 
			Params_table.param[i].steerable);
	pbuf += sprintf(pbuf,"<Type>%d</Type>\n", 
			Params_table.param[i].type);
	pbuf += sprintf(pbuf,"<Handle>%d</Handle>\n",
			Params_table.param[i].handle);
	pbuf += sprintf(pbuf,"<Value>%s</Value>\n", 
			Params_table.param[i].value);

	if(Params_table.param[i].is_internal == TRUE){

	  pbuf += sprintf(pbuf,"<Is_internal>TRUE</Is_internal>\n");
	}
	else{

	  pbuf += sprintf(pbuf,"<Is_internal>FALSE</Is_internal>\n");
	}

	pbuf += sprintf(pbuf,"</Param>\n");
      }
    }
  }
  
  pbuf += sprintf(pbuf,"</Param_defs>\n");
  Write_xml_footer(&pbuf);

  /* Physically send the message */
  return Send_status_msg(buf);
}

/*----------------------------------------------------------------*/

int Emit_IOType_defs(){

  int   i;
  char  buf[REG_MAX_MSG_SIZE];
  char *pbuf;

  /* Check that we do actually have something to emit */
  if (IOTypes_table.num_registered == 0) return REG_SUCCESS;

  /* Emit all currently registered IOTypes */
  
  pbuf = buf;
  Write_xml_header(&pbuf);

  pbuf += sprintf(pbuf, "<IOType_defs>\n");
  
  for(i=0; i<IOTypes_table.max_entries; i++){
  
    if(IOTypes_table.io_def[i].handle != REG_IODEF_HANDLE_NOTSET){
  
      pbuf += sprintf(pbuf,"<IOType>\n");
      pbuf += sprintf(pbuf,"<Label>%s</Label>\n", 
		      IOTypes_table.io_def[i].label);
      pbuf += sprintf(pbuf,"<Handle>%d</Handle>\n", 
		      IOTypes_table.io_def[i].handle);

      switch(IOTypes_table.io_def[i].direction){

      case REG_IO_IN:
        pbuf += sprintf(pbuf,"<Direction>IN</Direction>\n");
	break;

      case REG_IO_OUT:
        pbuf += sprintf(pbuf,"<Direction>OUT</Direction>\n");
	break;

      case REG_IO_CHKPT:
        pbuf += sprintf(pbuf,"<Direction>CHECKPOINT</Direction>\n");
	break;

      default:
#if DEBUG
	fprintf(stderr, 
		"Emit_IOType_defs: Unrecognised IOType direction\n");
#endif
	return REG_FAILURE;
      }

      pbuf += sprintf(pbuf,"<Freq_handle>%d</Freq_handle>\n",
	      IOTypes_table.io_def[i].freq_param_handle);

      pbuf += sprintf(pbuf,"</IOType>\n");
    }
  }
  
  pbuf += sprintf(pbuf,"</IOType_defs>\n");
  Write_xml_footer(&pbuf);

  /* Physically send message */
  return Send_status_msg(buf);
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
  char                *ptr;
  struct msg_struct   *msg;
  struct cmd_struct   *cmd;
  struct param_struct *param;
  int                  handle;
  int                  return_status = REG_SUCCESS;

  /* Read the file produced by the steerer - may contain commands and/or
     new parameter values */

  if((msg = Get_control_msg()) != NULL){

    if(msg->control){

      cmd   = msg->control->first_cmd;
      count = 0;

      while(cmd){

	sscanf((char *)(cmd->id), "%d", &(Commands[count]));

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
#if DEBUG
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

      *NumCommands = count;

#if DEBUG
      fprintf(stderr, "Consume_control: received %d commands\n", 
	      *NumCommands);
#endif

      param = msg->control->first_param;
      count = 0;

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
	  }
	  else{
	    fprintf(stderr, "Consume_control: empty parameter value "
		    "field\n");
	  }
	}

	param = param->next;
      }

      /* Update the number of parameters received to allow for fact that
	 some may be internal and are not passed up to the calling routine */
      *NumSteerParams = count;

#if DEBUG
	fprintf(stderr, "Consume_control: received %d params\n", *NumSteerParams);
#endif
    }
    else{
      fprintf(stderr, "Consume_control: error, no control data\n");
      *NumSteerParams = 0;
      *NumCommands    = 0;
      return_status   = REG_FAILURE;
    }
  }
  else{

#if DEBUG
    fprintf(stderr, "Consume_control: no message from steerer\n");
#endif

    /* No message found */

    *NumSteerParams = 0;
    *NumCommands = 0;
  }

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
  int   i;
  int   pcount = 0;
  int   tot_pcount = 0;
  int   ccount = 0;
  int   num_param;
  int   cmddone   = FALSE;
  int   paramdone = FALSE;
  char  buf[REG_MAX_MSG_SIZE];
  char *pbuf;

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

  if(NumCommands == 0) cmddone = TRUE;
  if(num_param == 0) paramdone = TRUE;

  /* Loop until all params and commands have been emitted */

  while(!paramdone || !cmddone){

    pbuf = buf;

    Write_xml_header(&pbuf);
    pbuf += sprintf(pbuf, "<App_status>\n");

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

 	  pbuf += sprintf(pbuf, "<Param>\n");
 	  pbuf += sprintf(pbuf, "<Handle>%d</Handle>\n", 
 		  Params_table.param[tot_pcount].handle);
 	  pbuf += sprintf(pbuf, "<Value>%s</Value>\n", 
		  Params_table.param[tot_pcount].value);
 	  pbuf += sprintf(pbuf, "</Param>\n");
  
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
  
    	pbuf += sprintf(pbuf, "<Command>\n");
	pbuf += sprintf(pbuf, "<Cmd_id>%d</Cmd_id>\n", Commands[ccount]);
	pbuf += sprintf(pbuf, "</Command>\n");
    	ccount++;
  
    	if(ccount >= NumCommands){
 	  cmddone = TRUE;
 	  break;
    	}
      }
    }

    pbuf += sprintf(pbuf, "</App_status>\n");

    Write_xml_footer(&pbuf);

    /* Physically send the status message */
    Send_status_msg(buf);
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
  if(param->handle == REG_SEQ_NUM_HANDLE || 
     param->handle == REG_STEP_TIME_HANDLE){

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
    fprintf(stderr, "Steering_signal_handler: Steerer_finalize failed");
  }

  exit(0);
}

/*--------------------------------------------------------------------*/

int Make_vtk_buffer(char  *header,
		    int    nx,
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
  char  *pchar;
  char   text[64];

  a2 = a*a;
  b2 = b*b;
  c2 = c*c;

  /* Make ASCII header to describe data to vtk */

  pchar = header;
  pchar += sprintf(pchar, "%-128s", "# AVS field file\n");
  pchar += sprintf(pchar, "%-128s", "ndim=3\n");
  sprintf(text, "dim1= %d\n", nx);
  pchar += sprintf(pchar, "%-128s", text);
  sprintf(text, "dim2= %d\n", ny);
  pchar += sprintf(pchar, "%-128s", text);
  sprintf(text, "dim3= %d\n", nz);
  pchar += sprintf(pchar, "%-128s", text);
  pchar += sprintf(pchar, "%-128s", "nspace=3\n");
  pchar += sprintf(pchar, "%-128s", "field=uniform\n");
  sprintf(text, "veclen= %d\n", veclen);
  pchar += sprintf(pchar, "%-128s", text);
  pchar += sprintf(pchar, "%-128s", "data=float\n");

  /* Use 'filetype=stream' because this is _not_ standard AVS because
     the way we interpret 'skip' at the other end of a socket is
     not standard either - we use it as the number of objects (floats,
     ints etc.) to skip rather than the no. of bytes or lines. */
  if(veclen == 1){
    pchar += sprintf(pchar, "%-128s", "variable 1 filetype=stream "
		     "skip=0000000 stride=1\n");
  }
  else if(veclen == 2){
    pchar += sprintf(pchar, "%-128s", "variable 1 filetype=stream "
		     "skip=0000000 stride=2\n");
    pchar += sprintf(pchar, "%-128s", "variable 2 filetype=stream "
		     "skip=0000001 stride=2\n");

  }
  else if(veclen == 3){
    pchar += sprintf(pchar, "%-128s", "variable 1 filetype=stream "
		     "skip=0000000 stride=3\n");
    pchar += sprintf(pchar, "%-128s", "variable 2 filetype=stream "
		     "skip=0000001 stride=3\n");
    pchar += sprintf(pchar, "%-128s", "variable 3 filetype=stream "
		     "skip=0000002 stride=3\n");
  }
  pchar += sprintf(pchar, "%-128s", "END_OF_HEADER\n");

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
	  mag = 1.0;
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

  fprintf(stderr, "Make_vtk_buffer: checksum = %f\n", sum/((float) count));

  /*
  fptr = array;
  fprintf(stderr, "Array is: \n");
  for(i=0; i<nx; i++){

    fprintf(stderr, "%.3f %.3f %.3f\n", *(fptr++), *(fptr++), *(fptr++));
  }
  */
  return REG_SUCCESS;
}

/*--------------------------------------------------------------------*/

int Steerer_connected()
{
  char   filename[REG_MAX_STRING_LENGTH];
  FILE  *fp;

  sprintf(filename, "%s%s", Steerer_connection.file_root, 
	  STR_CONNECTED_FILENAME);

  if( (fp = fopen(filename, "r")) ){

      fclose(fp);
      return REG_SUCCESS;
  }

  return REG_FAILURE;
}

/*-------------------------------------------------------------------*/

int Send_status_msg(char *buf)
{
  FILE *fp;
  char  filename[REG_MAX_STRING_LENGTH];

  Generate_status_filename(filename);

  if( (fp = fopen(filename, "w")) == NULL){

    fprintf(stderr, "Send_status_msg: failed to open file\n");
    return REG_FAILURE;
  }

  fprintf(fp, "%s", buf);
  fclose(fp);

  Create_lock_file(filename);

  return REG_SUCCESS;
}

/*-------------------------------------------------------------------*/

struct msg_struct *Get_control_msg()
{
  struct msg_struct   *msg = NULL;
  FILE                *fp;
  char                 filename[REG_MAX_STRING_LENGTH];

  sprintf(filename, "%s%s", Steerer_connection.file_root, STR_TO_APP_FILENAME);

  if( (fp = Open_next_file(filename)) != NULL){

    fclose(fp);

    msg = New_msg_struct();

    if(Parse_xml_file(filename, msg) != REG_SUCCESS){

      fprintf(stderr, "Read_control: failed to parse <%s>\n", filename);
      Delete_msg_struct(msg);
      msg = NULL;
    }

    /* Delete the file once we've read it */
    if( Delete_file(filename) != REG_SUCCESS){

      fprintf(stderr, "Read_control: failed to delete %s\n",filename);
    }
  }

  return msg;
}

/*-------------------------------------------------------------------*/

int Initialize_steering_connection(int  NumSupportedCmds,
				   int *SupportedCmds)
{
  FILE *fp;
  char *pchar;
  char  buf[REG_MAX_MSG_SIZE];
  char  filename[REG_MAX_STRING_LENGTH];
  int   i;

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

      fprintf(stderr, "Initialize_steering_connection: invalid dir for "
	      "steering messages: %s\n", Steerer_connection.file_root);
      return REG_FAILURE;
    }
    else{
      fprintf(stderr, "Using following dir for steering messages: %s\n", 
	     Steerer_connection.file_root);
    }
  }
  else{
    fprintf(stderr, "Initialize_steering_connection: failed to get "
	    "scratch directory\n");
    return REG_FAILURE;
  }

  /* Clean up any old files... */

  /* ...file indicating a steerer is connected (which it can't be since we've
     only just begun) */ 
  sprintf(filename, "%s%s", Steerer_connection.file_root, 
	  STR_CONNECTED_FILENAME);
  fp = fopen(filename, "w");
  if(fp != NULL){

    fclose(fp);
    if(remove(filename)){

      fprintf(stderr, "Initialize_steering_connection: failed to "
	      "remove %s\n",filename);
    }
#if DEBUG
    else{
      fprintf(stderr, "Initialize_steering_connection: removed "
	      "%s\n", filename);
    }
#endif
  }

  /* ...files containing messages from a steerer */
  sprintf(filename, "%s%s", Steerer_connection.file_root, 
	  STR_TO_APP_FILENAME);

  Remove_files(filename);

  /* Signal that component is available to be steered */

  sprintf(filename, "%s%s", Steerer_connection.file_root, 
	                    APP_STEERABLE_FILENAME);
  fp = fopen(filename,"w");

  if(fp == NULL){

    fprintf(stderr, "Initialize_steering_connection: failed to open %s\n",
	    filename);
    return REG_FAILURE;
  }

#if DEBUG
  fprintf(stderr, "Initialize_steering_connection: writing file: %s\n", 
	  filename);
#endif

  pchar = buf;

  Write_xml_header(&pchar);

  pchar += sprintf(pchar, "<Supported_commands>\n");

  for(i=0; i<NumSupportedCmds; i++){
    pchar += sprintf(pchar, "<Command>\n");
    pchar += sprintf(pchar, "<Cmd_id>%d</Cmd_id>\n", SupportedCmds[i]);
    pchar += sprintf(pchar, "</Command>\n");
  }

  pchar += sprintf(pchar, "</Supported_commands>\n");

  Write_xml_footer(&pchar);

  fprintf(fp, "%s", buf);
  fclose(fp);

  return REG_SUCCESS;
}

/*-------------------------------------------------------------------*/

int Finalize_steering_connection()
{
  int  max, max1;
  char sys_command[REG_MAX_STRING_LENGTH];

  max = strlen(APP_STEERABLE_FILENAME);
  max1 = strlen(STR_CONNECTED_FILENAME);

  if(max1 > max) max=max1;
  
  max += strlen(Steerer_connection.file_root);
  if(max > REG_MAX_STRING_LENGTH ){

    fprintf(stderr, "Finalize_steering_connection: WARNING: truncating "
	    "filename\n");
  }

  /* Delete the lock file that indicates we are steerable */
  sprintf(sys_command, "%s%s", Steerer_connection.file_root,
	  APP_STEERABLE_FILENAME);
  if(remove(sys_command)){

    fprintf(stderr, "Finalize_steering_connection: failed to remove "
	    "%s\n", sys_command);
  }

  /* Delete the lock file that indicates we are being steered */
  if(ReG_SteeringActive){

    sprintf(sys_command, "%s%s", Steerer_connection.file_root,
	    STR_CONNECTED_FILENAME);
    if(remove(sys_command)){

      fprintf(stderr, "Finalize_steering_connection: failed to remove "
	      "%s\n", sys_command);
    }
  }

  /* Delete any files we'd have consumed if we'd lived longer */
  sprintf(sys_command, "%s%s", Steerer_connection.file_root, 
	  STR_TO_APP_FILENAME);
  Remove_files(sys_command);

  return REG_SUCCESS;
}
