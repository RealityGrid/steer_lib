/*----------------------------------------------------------------------------
    Library routines and associated data structures for use in a 
    steering application.  Provides a communication interface 
    intended to inter-operate with an interface constructed for an
    application using the routines in ReG_Steer_Appside.c.

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

#include "ReG_Steer_Steerside.h"
#include "ReG_Steer_Steerside_internal.h"

#ifndef DEBUG
#define DEBUG 0
#endif

/*----------- Data structures ---------------*/

/* Main table used to record all simulations currently
   being steered */

static struct {

  int             num_registered;
  int             max_entries;
  Sim_entry_type *sim;

} Sim_table;

/*----- Routines to be used by the steering component ------*/

int Steerer_initialize()
{
  int   i;
  char *pchar;

  /* Actually defined in ReG_Steer_Common.c because both steerer
     and steered have a variable of this name */
  extern char ReG_Steer_Schema_Locn[REG_MAX_STRING_LENGTH];

  /* Set the location of the file containing the schema describing all 
     steering communication */

  pchar = getenv("REG_STEER_HOME");

  if(pchar){
    sprintf(ReG_Steer_Schema_Locn, "%s/xml_schema/reg_steer_comm.xsd",
	                           pchar);
  }
  else{

    printf("Steerer_initialize: failed to get schema location\n");
    return REG_FAILURE;
  }

  /* Initialize table of connected simulations */

  Sim_table.sim = (Sim_entry_type *)malloc(REG_MAX_NUM_STEERED_SIM*
					   sizeof(Sim_entry_type));

  if(Sim_table.sim == NULL){
    
    printf("Steerer_initialize: failed to allocate memory\n");
    return REG_FAILURE;
  }

  Sim_table.max_entries    = REG_MAX_NUM_STEERED_SIM;
  Sim_table.num_registered = 0;

  /* Handle used to identify whether a table entry is valid so
     must initialize them all */

  for(i=0; i<Sim_table.max_entries; i++){

    Sim_table.sim[i].handle = REG_SIM_HANDLE_NOTSET;
  }

  return REG_SUCCESS;
}

/*----------------------------------------------------------*/

int Steerer_finalize()
{
  int i;

  for(i=0; i<Sim_table.max_entries; i++){

    Delete_sim_table_entry( &(Sim_table.sim[i].handle) );
  }

  if(Sim_table.sim != NULL){

    free(Sim_table.sim);
    Sim_table.sim = NULL;
  }

  Sim_table.num_registered = 0;
  Sim_table.max_entries    = 0;
  
  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Sim_attach(REG_SimIDType SimID,
	       int          *SimHandle)
{
  int                  current_sim;
  int                  i;
  int                  j;
  int                  new_size;
  void                *dum_ptr;
  FILE                *fp1;
  FILE                *fp2;
  char                *pchar;
  char                 file_root[REG_MAX_STRING_LENGTH];
  char                 filename[REG_MAX_STRING_LENGTH];
  char                 sys_cmd[REG_MAX_STRING_LENGTH];
  char                 buf[BUFSIZ];
  XML_Parser           parser = XML_ParserCreate(NULL);
  int                  done;
  user_data_type       User_data;
  supp_cmds_xml_struct cmds_xml_struct;
  int                  return_status = REG_SUCCESS;

  /* Get next free entry in simulation table (allocates more memory if
     required ) */

  if( (current_sim = Next_free_sim_index()) == -1){

    printf("Sim_attach: failed to find free sim. table entry\n");
    return REG_FAILURE;
  }

  /* Initialise table entry for this simulation... */

  /* ...registered parameters */

  Sim_table.sim[current_sim].Params_table.param = 
            (param_entry *)malloc(REG_INITIAL_NUM_PARAMS*sizeof(param_entry));

  if(Sim_table.sim[current_sim].Params_table.param == NULL){

    printf("Sim_attach: failed to allocate memory\n");
    return REG_MEM_FAIL;
  }

  /* Initialise this table entry */

  Sim_table.sim[current_sim].Params_table.num_registered = 0;
  Sim_table.sim[current_sim].Params_table.max_entries    = 
                                                    REG_INITIAL_NUM_PARAMS;

  for(i=0; i<Sim_table.sim[current_sim].Params_table.max_entries; i++){

    Sim_table.sim[current_sim].Params_table.param[i].handle = 
                                             REG_PARAM_HANDLE_NOTSET;
    Sim_table.sim[current_sim].Params_table.param[i].modified = FALSE;
  }

  /* ...supported commands */

  Sim_table.sim[current_sim].Cmds_table.cmd = 
    (supp_cmd_entry *)malloc(REG_INITIAL_NUM_CMDS*sizeof(supp_cmd_entry));

  if(Sim_table.sim[current_sim].Cmds_table.cmd == NULL){

    printf("Sim_attach: failed to allocate memory\n");
    free(Sim_table.sim[current_sim].Params_table.param);
    return REG_MEM_FAIL;
  }

  Sim_table.sim[current_sim].Cmds_table.num_registered = 0;
  Sim_table.sim[current_sim].Cmds_table.max_entries    = REG_INITIAL_NUM_CMDS;

  /* all simulations must support the 'detach' command */

  Sim_table.sim[current_sim].Cmds_table.cmd[0].cmd_id = REG_STR_DETACH;
  Increment_cmd_registered(&(Sim_table.sim[current_sim].Cmds_table));

  /* ...IO types */

  Sim_table.sim[current_sim].IOdef_table.io_def = 
    (IOdef_entry *)malloc(REG_INITIAL_NUM_IOTYPES*sizeof(IOdef_entry));

  if(Sim_table.sim[current_sim].IOdef_table.io_def == NULL){

    printf("Sim_attach: failed to allocate memory\n");
    free(Sim_table.sim[current_sim].Params_table.param);
    free(Sim_table.sim[current_sim].Cmds_table.cmd);
    return REG_MEM_FAIL;
  }

  Sim_table.sim[current_sim].IOdef_table.num_registered = 0;
  Sim_table.sim[current_sim].IOdef_table.max_entries    = 
                                                  REG_INITIAL_NUM_IOTYPES;

  for(i=0; i<Sim_table.sim[current_sim].IOdef_table.max_entries; i++){

    Sim_table.sim[current_sim].IOdef_table.io_def[i].handle = 
      REG_IODEF_HANDLE_NOTSET;
  }

  /* Long-term - will need access to info on where sim. is
     running in order to setup info about how to access it */

  pchar = getenv("REG_STEER_DIRECTORY");

  if(pchar){
    strcpy(file_root, pchar);

    if(Directory_valid(file_root) != REG_SUCCESS){

      printf("Steerer_initialize: invalid dir for steering messages: %s\n",
	     file_root);

      free(Sim_table.sim[current_sim].Params_table.param);
      free(Sim_table.sim[current_sim].Cmds_table.cmd);
      free(Sim_table.sim[current_sim].IOdef_table.io_def);

      return REG_FAILURE;
    }
    else{

      printf("Using following dir for steering messages: %s\n", 
	   file_root);
    }
  }
  else{
    printf("Sim_attach: failed to get scratch directory\n");
    
    free(Sim_table.sim[current_sim].Params_table.param);
    free(Sim_table.sim[current_sim].Cmds_table.cmd);
    free(Sim_table.sim[current_sim].IOdef_table.io_def);
    return REG_FAILURE;
  }

  /* Delete any old communication files written by an app 
     in this location */

  sprintf(filename, "%s%s", file_root, 
	  APP_TO_STR_FILENAME);
  Remove_files(filename);

  /* Initialise XML parser */

  XML_SetUserData(parser, &User_data);
  XML_SetElementHandler(parser, startElement, endElement);
  XML_SetCharacterDataHandler(parser, dataHandler);

  /* Set ptr to the table to fill */

  cmds_xml_struct.field_type = CMD_NOTSET;
  cmds_xml_struct.table      = &(Sim_table.sim[current_sim].Cmds_table);
  User_data.gen_xml_struct = (void *) (&(cmds_xml_struct));

  /* Check for presence of lock file indicating sim is steerable */

  sprintf(filename, "%s%s", file_root, APP_STEERABLE_FILENAME);
  fp1 = fopen(filename, "r");

  /* Check for absence of lock file indicating that sim is
     already being steered */

  sprintf(filename, "%s%s", file_root, STR_CONNECTED_FILENAME);
  fp2 = fopen(filename, "r");

  if(fp1 != NULL && fp2 == NULL){

    /* Read supported commands */

    do {

      size_t len = fread(buf, 1, sizeof(buf), fp1);
      done = len < sizeof(buf);

      if (!XML_Parse(parser, buf, len, done)) {

	printf("%s at line %d\n",
	       XML_ErrorString(XML_GetErrorCode(parser)),
	       XML_GetCurrentLineNumber(parser));
	return_status = REG_FAILURE;
	break;
      }
    } while (!done);

    fclose(fp1);

    if(return_status == REG_FAILURE){

      free(Sim_table.sim[current_sim].Params_table.param);
      free(Sim_table.sim[current_sim].Cmds_table.cmd);
      free(Sim_table.sim[current_sim].IOdef_table.io_def);
      return return_status;
    }

    Sim_table.sim[current_sim].handle = current_sim;
    *SimHandle = current_sim;
    strcpy(Sim_table.sim[current_sim].file_root, file_root);
    Sim_table.num_registered++;

    XML_ParserFree(parser);

    /* If simulation supports the pause command then it must also
       support the resume command so add this to the list */

    for(i=0; i<Sim_table.sim[current_sim].Cmds_table.num_registered; i++){

      printf("Sim_attach: cmd[%d] = %d\n", i, 
	     Sim_table.sim[current_sim].Cmds_table.cmd[i].cmd_id);

      if(Sim_table.sim[current_sim].Cmds_table.cmd[i].cmd_id == REG_STR_PAUSE){
	
	j = Sim_table.sim[current_sim].Cmds_table.num_registered;

	/* Check that we aren't about to exceed allocated storage */

	if(j == Sim_table.sim[current_sim].Cmds_table.max_entries){

	  new_size = Sim_table.sim[current_sim].Cmds_table.max_entries +
	             REG_INITIAL_NUM_CMDS;

	  dum_ptr = (void *)realloc(Sim_table.sim[current_sim].Cmds_table.cmd,
				    new_size*sizeof(supp_cmd_entry));

	  if(dum_ptr){
	    Sim_table.sim[current_sim].Cmds_table.cmd = 
	      (supp_cmd_entry *)dum_ptr;
	    Sim_table.sim[current_sim].Cmds_table.max_entries = new_size;
	  }
	  else{

	    printf("Sim_attach: failed to realloc memory for supp commands\n");
	    return REG_FAILURE;
	  }
	}
	Sim_table.sim[current_sim].Cmds_table.cmd[j].cmd_id = REG_STR_RESUME;
	Sim_table.sim[current_sim].Cmds_table.num_registered++;
	break;
      }
    }

    /* Create lock file to indicate that steerer has connected to sim */

    sprintf(sys_cmd, "touch %s%s", file_root, STR_CONNECTED_FILENAME);
    system(sys_cmd);
  }
  else{

    if( fp2 != NULL){
      fclose(fp2);
    }

    XML_ParserFree(parser);

    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*----------------------------------------------------------*/

int Sim_detach(int *SimHandle)
{
  int  SysCommands[1];

  /* Check that handle is valid */

  if(*SimHandle == REG_SIM_HANDLE_NOTSET) return REG_SUCCESS;

  /* Signal simulation that we are finished steering */

  SysCommands[0] = REG_STR_DETACH;

  Emit_control(*SimHandle,
	       1,
	       SysCommands);

  /* Delete associated table entry */

  Delete_sim_table_entry(SimHandle);

  return REG_SUCCESS;
}

/*----------------------------------------------------------*/

int Get_next_message(int         *SimHandle,
		     REG_MsgType *msg_type)
{
  int        return_status = REG_SUCCESS;
  int        isim;
  FILE      *fp;

  XML_Parser parser = XML_ParserCreate(NULL);
  size_t     len;
  char       buf[BUFSIZ];
  int        done;
  int        count_active;
  static int last_sim = 0;

  /* This routine checks for any messages from connected
     simulations */

  *msg_type = MSG_NOTSET;

  /* Initialise XML parser */

  XML_SetUserData(parser, (void *)msg_type);
  XML_SetElementHandler(parser, scan_startElement, scan_endElement);

  count_active = 0;

  for(isim=last_sim; isim<Sim_table.max_entries; isim++){
  
    if(Sim_table.sim[isim].handle != REG_SIM_HANDLE_NOTSET){
  

      sprintf(buf, "%s%s", Sim_table.sim[isim].file_root,
  		           APP_TO_STR_FILENAME);
  	
      if(fp = Open_next_file(buf)){
  
	do {
  
	  len  = fread(buf, 1, sizeof(buf), fp);
	  done = len < sizeof(buf);
  
	  if (!XML_Parse(parser, buf, len, done)) {
  
	    printf("%s at line %d\n",
		   XML_ErrorString(XML_GetErrorCode(parser)),
		   XML_GetCurrentLineNumber(parser));
	    break;
	  }

	  if (*msg_type != MSG_NOTSET) break;

	} while (!done);
  
  	fclose(fp);

	if(*msg_type != MSG_NOTSET){

	  *SimHandle = Sim_table.sim[isim].handle;

	  /* Keep a record of the last sim we received a msg
	     from and then breakout */
	  last_sim = isim;
	  break;
	}
      }

      /* Count no. of active sim.'s we've checked so that we can
	 break out of loop once Sim_table.num_registered have
	 been done - this will only happen when there are no
         messages to retrieve */
      if (++count_active == Sim_table.num_registered) break;
    }
  }

  XML_ParserFree(parser);

  return return_status;
}

/*----------------------------------------------------------*/

int Consume_param_defs(int SimHandle)
{
  char             base_name[REG_MAX_STRING_LENGTH];
  int              index;
  int              i;
  int              j;
  int              done;
  FILE            *fp;
  XML_Parser       parser = XML_ParserCreate(NULL);
  user_data_type   User_data;
  size_t           len;
  char             buf[BUFSIZ];
  param_xml_struct param_struct;
  Param_table_type param_table;
  int              return_status = REG_SUCCESS;

  /* Read a message containing parameter definitions.  Table of
     stored definitions is then updated to match those just read */

  if( (index = Sim_index_from_handle(SimHandle)) == REG_SIM_HANDLE_NOTSET){

    printf("Consume_param_defs: failed to find sim table entry\n");
    return REG_FAILURE;
  }

  /* Initialise temporary param table to fill - can't put stuff straight into
     the main table until we've checked handles */

  param_table.param = (param_entry *)malloc(REG_INITIAL_NUM_PARAMS*
					    sizeof(param_entry));

  if(param_table.param == NULL) return REG_FAILURE;

  param_table.num_registered = 0;
  param_table.max_entries    = REG_INITIAL_NUM_PARAMS;

  for(i=0; i<REG_INITIAL_NUM_PARAMS; i++){

    param_table.param[i].handle = REG_PARAM_HANDLE_NOTSET;
  }

  /* Initialise XML parser */

  XML_SetUserData(parser, &User_data);
  XML_SetElementHandler(parser, startElement, endElement);
  XML_SetCharacterDataHandler(parser, dataHandler);


  /* Set ptr to the table to fill */

  param_struct.field_type  = PARAM_NOTSET;
  param_struct.table       = &(param_table);
  User_data.msg_type       = MSG_NOTSET;
  User_data.gen_xml_struct = &param_struct;

  /* Get name of file to read */

  sprintf(base_name,"%s%s",Sim_table.sim[SimHandle].file_root,
	  APP_TO_STR_FILENAME);

  fp = Open_next_file(base_name);

  if(fp == NULL){
    printf("Consume_param_defs: failed to find file to read\n");
    XML_ParserFree(parser);
    free(param_table.param);
    return REG_FAILURE;
  }

  /* Parse the file */

  do {
  
    len = fread(buf, 1, sizeof(buf), fp);
    done = len < sizeof(buf);
  
    if (!XML_Parse(parser, buf, len, done)) {
  
  	printf("%s at line %d\n",
  	       XML_ErrorString(XML_GetErrorCode(parser)),
  	       XML_GetCurrentLineNumber(parser));
  	return REG_FAILURE;
    }
  } while (!done);

  fclose(fp);
  Delete_file(base_name);

  /* Must now check the param definitions we've received and update the
     parameters part of the Sim_table appropriately */

  for(i=0; i<param_table.num_registered; i++){

    if( Param_index_from_handle(&(Sim_table.sim[index].Params_table), 
		  param_table.param[i].handle) == REG_PARAM_HANDLE_NOTSET){

      /* Our table doesn't have this parameter in it so add it */
      j = Next_free_param_index(&(Sim_table.sim[index].Params_table));

      if(j == -1){
	printf("Consume_param_defs: failed to add param to table\n");
	return_status = REG_FAILURE;
	break;
      }
      Sim_table.sim[index].Params_table.param[j] = param_table.param[i];
      Sim_table.sim[index].Params_table.num_registered++;
    }
  }

  /* Remove any parameters that have been deleted */

  for(i=0; i<Sim_table.sim[index].Params_table.max_entries; i++){

    if( Param_index_from_handle(&(param_table), 
			 Sim_table.sim[index].Params_table.param[i].handle)
	                                          == REG_PARAM_HANDLE_NOTSET){

      /* Only indication that param deleted is change of handle - hence loop
	 over max_entries here */

      Sim_table.sim[index].Params_table.param[i].handle = 
	                                       REG_PARAM_HANDLE_NOTSET;
      Sim_table.sim[index].Params_table.num_registered--;
    }    
  }

  /* Clean up */

  free(param_table.param);

  return return_status;
}


/*----------------------------------------------------------*/

int Consume_IOType_defs(int     SimHandle)
{
  char             base_name[REG_MAX_STRING_LENGTH];
  int              index;
  int              done;
  int              i;
  int              j;
  FILE            *fp;
  XML_Parser       parser = XML_ParserCreate(NULL);
  user_data_type   User_data;
  size_t           len;
  char             buf[BUFSIZ];
  iodef_xml_struct iodef_struct;
  IOdef_table_type iodef_table;
  int              return_status = REG_SUCCESS;

  if( (index = Sim_index_from_handle(SimHandle)) == REG_SIM_HANDLE_NOTSET){

    printf("Consume_IOType_defs: failed to find sim table entry\n");
    return REG_FAILURE;
  }

  /* Initialise table to fill initially - can't fill actual table 
     during read as need to allow for IOtypes being added and removed */

  iodef_table.io_def = (IOdef_entry *)malloc(REG_INITIAL_NUM_IOTYPES*
					     sizeof(IOdef_entry));

  if(iodef_table.io_def == NULL){

    printf("Consume_IOType_defs: failed to allocate memory\n");
    return REG_FAILURE;
  }

  iodef_table.num_registered = 0;
  iodef_table.max_entries    = REG_INITIAL_NUM_IOTYPES;

  for(i=0; i<REG_INITIAL_NUM_IOTYPES; i++){

    iodef_table.io_def[i].handle = REG_IODEF_HANDLE_NOTSET;
  }

  /* Initialise XML parser */

  XML_SetUserData(parser, &User_data);
  XML_SetElementHandler(parser, startElement, endElement);
  XML_SetCharacterDataHandler(parser, dataHandler);

  /* Set ptr to the table to fill */

  iodef_struct.field_type  = IODEF_NOTSET;
  iodef_struct.table       = &(iodef_table);
  User_data.msg_type       = MSG_NOTSET;
  User_data.gen_xml_struct = &iodef_struct;

  /* Get name of file to read */

  sprintf(base_name,"%s%s",Sim_table.sim[index].file_root,
	  APP_TO_STR_FILENAME);

  fp = Open_next_file(base_name);

  if(fp == NULL){
    printf("Consume_IOType_defs: failed to find file to read\n");
    XML_ParserFree(parser);
    return REG_FAILURE;
  }

  /* Parse the file */

  do {
  
    len = fread(buf, 1, sizeof(buf), fp);
    done = len < sizeof(buf);
  
    if (!XML_Parse(parser, buf, len, done)) {
  
  	printf("%s at line %d\n",
  	       XML_ErrorString(XML_GetErrorCode(parser)),
  	       XML_GetCurrentLineNumber(parser));
  	return REG_FAILURE;
    }
  } while (!done);

  fclose(fp);
  Delete_file(base_name);

  /* Compare new IOdefs with those currently stored in table */

  for(i=0; i<iodef_table.num_registered; i++){

    if( IOdef_index_from_handle(&(Sim_table.sim[index].IOdef_table), 
		  iodef_table.io_def[i].handle) == REG_IODEF_HANDLE_NOTSET){

      /* Our table doesn't have this IOdef so add it */
      j = Next_free_iodef_index(&(Sim_table.sim[index].IOdef_table));

      if(j==-1){
	printf("Consume_IOdefs: failed to add IOdef to table\n");
	return_status = REG_FAILURE;
	break;
      }

      Sim_table.sim[index].IOdef_table.io_def[j] = iodef_table.io_def[i];
      Sim_table.sim[index].IOdef_table.num_registered++;
    }
  }

  /* Remove any IO defs that have been deleted */

  for(i=0; i<Sim_table.sim[index].IOdef_table.max_entries; i++){

    if( IOdef_index_from_handle(&iodef_table, 
			   Sim_table.sim[index].IOdef_table.io_def[i].handle) 
	                                          == REG_IODEF_HANDLE_NOTSET){

      /* Only indication that IOdef deleted is change of handle - hence loop
	 over max_entries here */

      Sim_table.sim[index].IOdef_table.io_def[i].handle = 
	                                 REG_IODEF_HANDLE_NOTSET;
      Sim_table.sim[index].IOdef_table.num_registered--;
    }
  }

  /* Clean up */

  free(iodef_table.io_def);

  return return_status;
}

/*----------------------------------------------------------*/

int Consume_status(int   SimHandle,
		   int  *SeqNum,
		   int  *NumCmds,
		   int  *Commands)
{
  FILE                *fp;
  int                  i, j;
  int                  index;
  int                  return_status;
  int                  NumParams;
  char                 filename[REG_MAX_STRING_LENGTH];

  /* For XML parser */

  size_t               len;
  char                 buf[BUFSIZ];
  XML_Parser           parser = XML_ParserCreate(NULL);
  int                  done;
  user_data_type       User_data;
  param_xml_struct     param_struct;
  status_xml_struct    status_struct;
  supp_cmds_xml_struct cmd_struct;
  Param_table_type     recvd_params;
  Supp_cmd_table_type  recvd_cmds;

  return_status = REG_SUCCESS;

  /* Find the table entry for simulation */

  if( (index = Sim_index_from_handle(SimHandle)) == REG_SIM_HANDLE_NOTSET){

    printf("Consume_status: failed to find sim table entry\n");
    return REG_FAILURE;
  }

  /* Set up tables and pointers to receive data... */

  /* ...for parameters */

  recvd_params.num_registered = 0;
  recvd_params.max_entries    = REG_MAX_NUM_STR_PARAMS;
  recvd_params.param = (param_entry *)malloc(REG_MAX_NUM_STR_PARAMS*
					     sizeof(param_entry));

  if(recvd_params.param == NULL){

    printf("Consume_control: failed to allocate memory");
    return REG_MEM_FAIL;
  }

  for(i=0; i<recvd_params.max_entries; i++){

    recvd_params.param[i].handle = REG_PARAM_HANDLE_NOTSET;
  }

  /* ...for commands */

  recvd_cmds.num_registered = 0;
  recvd_cmds.max_entries    = REG_MAX_NUM_STR_CMDS;
  recvd_cmds.cmd = (supp_cmd_entry *)malloc(REG_MAX_NUM_STR_CMDS*
					    sizeof(supp_cmd_entry));

  if(recvd_cmds.cmd == NULL){

    printf("Consume_status: failed to allocate memory");
    free(recvd_params.param);
    return REG_MEM_FAIL;
  }

  /* Each command may have parameters associated with it so prepare
     appropriate structures */

  for(i=0; i<recvd_cmds.max_entries; i++){

    recvd_cmds.cmd[i].cmd_params.num_registered = 0;
    recvd_cmds.cmd[i].cmd_params.max_entries    = REG_INITIAL_NUM_PARAMS;

    recvd_cmds.cmd[i].cmd_params.param = 
      (param_entry *)malloc(REG_INITIAL_NUM_PARAMS*sizeof(param_entry));

    if(recvd_cmds.cmd[i].cmd_params.param == NULL){

      for(j=i-1; j>-1; j--){

	free(recvd_cmds.cmd[j].cmd_params.param);
      }
      free(recvd_cmds.cmd);
      free(recvd_params.param);

      return REG_MEM_FAIL;
    }
  }

  /* Set pointers to structures to fill */

  param_struct.table         = &recvd_params;
  param_struct.field_type    = PARAM_NOTSET;
  cmd_struct.table           = &recvd_cmds;
  cmd_struct.field_type      = CMD_NOTSET;
  status_struct.param_struct = &param_struct;
  status_struct.cmd_struct   = &cmd_struct;
  status_struct.field_type   = STAT_NOTSET;
  User_data.gen_xml_struct   = (void *)(&status_struct);
  User_data.msg_type         = MSG_NOTSET;

  /* Initialise XML parser */

  XML_SetUserData(parser, &User_data);
  XML_SetElementHandler(parser, startElement, endElement);
  XML_SetCharacterDataHandler(parser, dataHandler);

  sprintf(filename, "%s%s", Sim_table.sim[index].file_root,
	  APP_TO_STR_FILENAME);

  if( (fp = Open_next_file(filename)) != NULL){

    do {
  
      len  = fread(buf, 1, sizeof(buf), fp);
      done = len < sizeof(buf);
  
      if (!XML_Parse(parser, buf, len, done)) {
  
  	  printf("%s at line %d\n",
  	         XML_ErrorString(XML_GetErrorCode(parser)),
  	         XML_GetCurrentLineNumber(parser));
  	  return_status = REG_FAILURE;
	  break;
      }
    } while (!done);
  
    XML_ParserFree(parser);
    fclose(fp);
    Delete_file(filename);

    /* Copy data out of structures */

    *NumCmds = recvd_cmds.num_registered;

#if DEBUG
    printf("Consume_status: got %d commands\n", (*NumCmds));
#endif
    for(i=0; i<(*NumCmds); i++){

      Commands[i] = recvd_cmds.cmd[i].cmd_id;
    }

    NumParams = recvd_params.num_registered;

    for(i=0; i<NumParams; i++){

      /* Look-up entry for param to update */

      if( (j=Param_index_from_handle(&(Sim_table.sim[index].Params_table),
	           recvd_params.param[i].handle)) == REG_PARAM_HANDLE_NOTSET){

	printf("Consume_status: failed to match param handles\n");
	printf("                handle = %d\n", recvd_params.param[i].handle);
	continue;
      }

      /* Update value of this param */
      strcpy(Sim_table.sim[index].Params_table.param[j].value, 
	     recvd_params.param[i].value);
    }

  }
  else{

    /* Failed to open file */
    *NumCmds = 0;
  }

  /* Return sequence number */

  if( (j=Param_index_from_handle(&(Sim_table.sim[index].Params_table),
			    REG_SEQ_NUM_HANDLE)) == REG_PARAM_HANDLE_NOTSET){
    printf("Consume_status: failed to find SeqNum entry\n");
  }
  else{

    sscanf(Sim_table.sim[index].Params_table.param[j].value, "%d", SeqNum);
  }

  /* Clean up */

  free(recvd_params.param);
  recvd_params.param = NULL;
  free(recvd_cmds.cmd);
  recvd_cmds.cmd = NULL;

  return return_status;
}

/*----------------------------------------------------------*/

int Emit_control(int    SimHandle,
		 int    NumCommands,
		 int   *SysCommands)
{
  FILE *fp;
  int   i;
  int   simid;
  int   count;
  int   num_to_emit;
  char  filename[REG_MAX_STRING_LENGTH];

  /* Find the simulation referred to */

  if( (simid = Sim_index_from_handle(SimHandle)) == REG_SIM_HANDLE_NOTSET){

    printf("Emit_control: failed to find sim table entry\n");
    return REG_FAILURE;
  }

  if( Generate_control_filename(SimHandle, filename) != REG_SUCCESS){

    printf("Emit_control: failed to create filename\n");
    return REG_FAILURE;
  }

  if( (fp = fopen(filename, "w")) == NULL){

    printf("Emit_control: failed to open file\n");
    return REG_FAILURE;
  }

  Write_xml_header(fp);
  fprintf(fp, "<Steer_control>\n");

  /* Enforce a hard limit of no more than REG_MAX_NUM_STR_CMDS commands
     per message */

  num_to_emit = NumCommands;
  if(NumCommands >= REG_MAX_NUM_STR_CMDS){

    num_to_emit = REG_MAX_NUM_STR_CMDS;
    printf("Emit_control: WARNING - no. of emitted commands is "
	   "limited to %d\n", REG_MAX_NUM_STR_CMDS);
  }

  for(i=0; i<num_to_emit; i++){

    /* Check that simulation supports each requested command */
    if(Command_supported(simid, SysCommands[i])==REG_SUCCESS){

      fprintf(fp, "<Command>\n");
      fprintf(fp, "<Cmd_id>%d</Cmd_id>\n", SysCommands[i]);
      fprintf(fp, "</Command>\n");
    }
  }

  /* Seach for valid parameters that have 'modified' flag set */

  count = 0;

  for(i=0; i<Sim_table.sim[simid].Params_table.max_entries; i++){

    if( (Sim_table.sim[simid].Params_table.param[i].handle !=
	 REG_PARAM_HANDLE_NOTSET) &&
	Sim_table.sim[simid].Params_table.param[i].modified ){

      /* Enforce a hard limit on the number of parameters that may
	 be transmitted at any one time */

      if(count == REG_MAX_NUM_STR_PARAMS){

	printf("Emit_control: WARNING - no. of emitted params is "
	       "limited to %d\n", REG_MAX_NUM_STR_PARAMS);
	break;
      }

      fprintf(fp, "<Param>\n");
      fprintf(fp, "<Handle>%d</Handle>\n", 
	      Sim_table.sim[simid].Params_table.param[i].handle);
      fprintf(fp, "<Value>%s</Value>\n", 
	      Sim_table.sim[simid].Params_table.param[i].value);
      fprintf(fp, "</Param>\n");

      /* Unset 'modified' flag */
      Sim_table.sim[simid].Params_table.param[i].modified = FALSE;

      count++;
    }
  }

  fprintf(fp, "</Steer_control>\n");
  Write_xml_footer(fp);
  fclose(fp);

  /* The application only attempts to read files for which it can find an
     associated lock file */
  Create_lock_file(filename);
  
  return REG_SUCCESS;
}

/*----------------------------------------------------------*/

int Delete_sim_table_entry(int *SimHandle)
{
  int             index;
  Sim_entry_type *entry;
  char            base_name[REG_MAX_STRING_LENGTH];

  if(*SimHandle == REG_SIM_HANDLE_NOTSET) return REG_SUCCESS;
  
  /* Find associated entry to delete */

  if( (index = Sim_index_from_handle(*SimHandle)) == REG_SIM_HANDLE_NOTSET){

    printf("Delete_sim_table_entry: failed to match handles\n");
    return REG_FAILURE;
  }

  entry = &(Sim_table.sim[index]);

  /* Delete any files that the app's produced that we won't now be
     consuming */

  sprintf(base_name, "%s%s", entry->file_root, 
	  APP_TO_STR_FILENAME);

  Remove_files(base_name);

  /* Clean-up the provided entry in the table of connected
     simulations */

  entry->Cmds_table.num_registered = 0;
  entry->Cmds_table.max_entries = 0;
  if (entry->Cmds_table.cmd) free(entry->Cmds_table.cmd);
  entry->Cmds_table.cmd = NULL;

  entry->Params_table.num_registered = 0;
  entry->Params_table.max_entries = 0;
  if (entry->Params_table.param) free(entry->Params_table.param);
  entry->Params_table.param = NULL;

  entry->IOdef_table.num_registered = 0;
  entry->IOdef_table.max_entries = 0;
  if (entry->IOdef_table.io_def) free(entry->IOdef_table.io_def);
  entry->IOdef_table.io_def = NULL;

  /* Flag that this entry no longer contains valid data */

  entry->handle = REG_SIM_HANDLE_NOTSET;
  *SimHandle    = REG_SIM_HANDLE_NOTSET;

  return REG_SUCCESS;
}

/*----------------------------------------------------------*/

int Generate_control_filename(int SimHandle, char* filename)
{
  static int output_file_index = 0;
  int        index;

  if( (index = Sim_index_from_handle(SimHandle)) == REG_SIM_HANDLE_NOTSET){

    return REG_FAILURE;
  }

  /* Generate next filename in sequence for sending data to
     steerer & increment counter */

  sprintf(filename, "%s%s_%d", Sim_table.sim[index].file_root,
	  STR_TO_APP_FILENAME, output_file_index++);

  if(output_file_index == REG_MAX_NUM_FILES) output_file_index = 0;

  return REG_SUCCESS;
}

/*----------------------------------------------------------*/

int Sim_index_from_handle(int SimHandle)
{
  int i;
  int index = REG_SIM_HANDLE_NOTSET;

  /* Finds entry in Sim_table that has handle == SimHandle */
  /* Returns REG_SIM_HANDLE_NOTSET if no match found */

  for(i=0; i<Sim_table.max_entries; i++){

    if(Sim_table.sim[i].handle == SimHandle){

      index = i;
      break;
    }
  }

  if(index == REG_SIM_HANDLE_NOTSET){
    printf("Sim_index_from_handle: failed to find matching handle\n");
  }

  return index;
}

/*----------------------------------------------------------*/

int IOdef_index_from_handle(IOdef_table_type *table, int IOdefHandle)
{
  int i;
  int index = REG_IODEF_HANDLE_NOTSET;

  /* Finds entry in a table of IOdefs that has handle == IOdefHandle
     Returns REG_IODEF_HANDLE_NOTSET if no match found */

  for(i=0; i<table->max_entries; i++){

    if(table->io_def[i].handle == IOdefHandle){

      index = i;
      break;
    }
  }

  return index;
}

/*----------------------------------------------------------*/

int Dump_sim_table()
{
  int   isim;
  int   iparam;
  int   iotype;
  int   icmd;

  FILE *fp;
  Sim_entry_type *simptr;
  param_entry    *paramptr;
  IOdef_entry    *ioptr;
  supp_cmd_entry *cmdptr;

  fp = fopen("sim_table.txt", "a");

  if(fp == NULL) return REG_FAILURE;

  fprintf(fp, "####################################################\n\n");

  simptr = Sim_table.sim;

  if(simptr == NULL) return REG_FAILURE;

  for(isim=0; isim<Sim_table.max_entries; isim++){

    if(simptr->handle != REG_SIM_HANDLE_NOTSET){

      fprintf(fp, "Simulation index %d\n\n", isim);

      fprintf(fp, "###### Parameters ######\n\n");

      paramptr = simptr->Params_table.param;
      if(paramptr != NULL){

      	for(iparam=0; iparam<simptr->Params_table.max_entries; iparam++){
  
 	  if(paramptr->handle == REG_PARAM_HANDLE_NOTSET) continue;
  
 	  fprintf(fp, "Param index %d\n", iparam);
 	  fprintf(fp, "--------------\n");
 	  fprintf(fp, "Label  = %s\n", paramptr->label);
 	  fprintf(fp, "strble = %d\n", paramptr->steerable);
 	  fprintf(fp, "type   = %d\n", paramptr->type);
 	  fprintf(fp, "handle = %d\n", paramptr->handle);
 	  fprintf(fp, "value  = %s\n\n", paramptr->value);
  
 	  paramptr++;
        }
      }

      fprintf(fp, "###### IO Types  ######\n\n");

      ioptr = simptr->IOdef_table.io_def;
      if(ioptr != NULL){

      	for(iotype=0; iotype<simptr->IOdef_table.max_entries; iotype++){
  
 	  if(ioptr->handle == REG_IODEF_HANDLE_NOTSET) continue;
  
 	  fprintf(fp, "IO Type index %d\n", iotype);
 	  fprintf(fp, "----------------\n");
 	  fprintf(fp, "Label  = %s\n", ioptr->label);
 	  fprintf(fp, "handle = %d\n\n", ioptr->handle);
  
 	  ioptr++;
        }
      }

      fprintf(fp, "###### Supported Cmds   ######\n\n");

      cmdptr = simptr->Cmds_table.cmd;
      if(cmdptr != NULL){

        for(icmd=0; icmd<simptr->Cmds_table.num_registered; icmd++){

	  fprintf(fp, "Supported cmd. index %d\n", icmd);
	  fprintf(fp, "-----------------------\n");
	  fprintf(fp, "Command ID = %d\n\n", cmdptr->cmd_id);

	  cmdptr++;
	}
      }
    }

    simptr++;
  }

  fclose(fp);

  return REG_SUCCESS;
} 

/*----------------------------------------------------------------*/

int Get_param_number(int  sim_handle,
		     int  steerable,
		     int *num_params)
{
  int return_status = REG_SUCCESS;
  int isim;
  int i;
  int count;

  if(num_params == NULL) return REG_FAILURE;

  /* Returns the number of valid <steerable> parameters currently
     associated with the simulation with handle sim_handle */

  if( (isim = Sim_index_from_handle(sim_handle)) != -1){
  
    count = 0;
    for(i=0; i<Sim_table.sim[isim].Params_table.max_entries; i++){
  
      /* Check that entry is valid & is not for a library-generated param */

      if(Sim_table.sim[isim].Params_table.param[i].handle != 
	 REG_PARAM_HANDLE_NOTSET &&
	 !(Sim_table.sim[isim].Params_table.param[i].is_internal)){

	if(Sim_table.sim[isim].Params_table.param[i].steerable == steerable){

	  count++;
	}
      }
    }

    *num_params = count;
  }
  else{
    return_status = REG_FAILURE;
  }

  return return_status;
}

/*----------------------------------------------------------------*/

int Get_param_values(int    sim_handle,
		     int    steerable,
		     int    num_params,
		     int   *handles,
		     char* *labels,
		     char* *vals,
		     int   *types)
{
  int return_status = REG_SUCCESS;
  int isim;
  int i;
  int count;

  /* Return lists of registered parameter handles and associated
     values (as strings) for the steered simulation with handle
     sim_handle 

     num_param is the number to return and handles and vals should
     each point to arrays (of arrays of char) of at least this length 

     if steerable == TRUE (1) then return steerable params, if FALSE 
     (0) then return monitoring params */

  if(handles == NULL || labels == NULL || vals == NULL) return REG_FAILURE;

  isim = Sim_index_from_handle(sim_handle);
  if(isim != -1){
  
    count = 0;

    for(i=0; i<Sim_table.sim[isim].Params_table.max_entries; i++){

      /* Check that entry is valid & is not for a library-generated param */

      if(Sim_table.sim[isim].Params_table.param[i].handle != 
	 REG_PARAM_HANDLE_NOTSET &&
	 !(Sim_table.sim[isim].Params_table.param[i].is_internal)){

	if(Sim_table.sim[isim].Params_table.param[i].steerable == steerable){

	  handles[count] = Sim_table.sim[isim].Params_table.param[i].handle;
	  strcpy(labels[count], 
		 Sim_table.sim[isim].Params_table.param[i].label);
	  strcpy(vals[count], Sim_table.sim[isim].Params_table.param[i].value);
          types[count] = Sim_table.sim[isim].Params_table.param[i].type;

	  count++;

	  if(count == num_params)break;
	}
      }
    }
  }
  else{
    return_status = REG_FAILURE;
  }
  
  return return_status;
}

/*----------------------------------------------------------------*/

int Set_param_values(int    sim_handle,
		     int    num_params,
		     int   *handles,
		     char* *vals)
{
  int return_status = REG_SUCCESS;
  int isim;
  int index;
  int i;

  /* Set the 'values' (held as strings) of the listed params */

  isim = Sim_index_from_handle(sim_handle);
  if(isim != REG_SIM_HANDLE_NOTSET){

    for(i=0; i<num_params; i++){

      if( (index = Param_index_from_handle(&(Sim_table.sim[isim].Params_table), 
				 handles[i])) == REG_PARAM_HANDLE_NOTSET ){
	return_status = REG_FAILURE;
	break;
      }
      
      /* Only set the value if parameter is steerable */

      if(Sim_table.sim[isim].Params_table.param[index].steerable){

	sprintf(Sim_table.sim[isim].Params_table.param[index].value,
		"%s", vals[i]);
	Sim_table.sim[isim].Params_table.param[index].modified = TRUE;
      }
      else{
	printf("Set_param_values: can only edit steerable parameters\n");
      }
    }
  }
  else{
    return_status = REG_FAILURE;
  }

  return return_status;
}

/*----------------------------------------------------------------*/

static int Next_free_iodef_index(IOdef_table_type *table)
{
  int   i;
  int   index = -1;
  int   new_size;
  void *dum_ptr;

  /* Look for first free entry in table - i.e. one that has an
     unset handle.  If none found then extends the size of the table. */

  for(i=0; i<table->max_entries; i++){

    if(table->io_def[i].handle == REG_IODEF_HANDLE_NOTSET){

      index = i;
      break;
    }
  }

  if(index == -1){

    /* No free entries - need to allocate more memory */

    new_size = table->max_entries + REG_INITIAL_NUM_IOTYPES;
    
    if(dum_ptr = (void *)realloc(table->io_def, new_size*sizeof(param_entry))){

      index = table->max_entries;
      table->io_def = (IOdef_entry *)dum_ptr;
      table->max_entries = new_size;
    }
  }

  return index;
}

/*-------------------------------------------------------------------*/

int Next_free_sim_index()
{
  int   i;
  int   index = -1;
  int   new_size;
  void *dum_ptr;

  /* Look for first free entry in table - i.e. one that has an
     unset handle.  If none found then extends the size of the table. */

  for(i=0; i<Sim_table.max_entries; i++){

    if(Sim_table.sim[i].handle == REG_SIM_HANDLE_NOTSET){

      index = i;
      break;
    }
  }

  if(index == -1){

    /* No free table entries - need to allocate more memory */

    new_size = Sim_table.max_entries + REG_MAX_NUM_STEERED_SIM;

    dum_ptr = (void*)realloc(Sim_table.sim, new_size*sizeof(Sim_entry_type));

    if(dum_ptr){

      Sim_table.sim = (Sim_entry_type *)dum_ptr;
      index = Sim_table.max_entries;
      Sim_table.max_entries = new_size;
    }
  }

  return index;
}

/*-------------------------------------------------------------------*/

int Get_iotype_number(int sim_handle,
		      int *num_iotypes)
{
  int i;
  int isim;
  int count;
  int return_status = REG_SUCCESS;

  /* Calculates the number of valid IO types currently registered with the
     simulation with handle sim_handle */

  if(num_iotypes == NULL)return REG_FAILURE;

  if((isim = Sim_index_from_handle(sim_handle)) != -1){

    count = 0;

    for(i=0; i<Sim_table.sim[isim].IOdef_table.max_entries; i++){

      if(Sim_table.sim[isim].IOdef_table.io_def[i].handle != 
	 REG_IODEF_HANDLE_NOTSET) count++;
    }

    *num_iotypes = count;
  }
  else{
    return_status = REG_FAILURE;
  }

  return return_status;
}

/*-------------------------------------------------------------------*/

int Get_iotypes(int    sim_handle,
		int    num_iotypes,
		int   *handles,
		char* *labels,
		int   *types,
		int   *auto_io_supported,
		int   *io_freqs)
{
  int isim;
  int i;
  int count;
  int iparam;
  int nitem;
  int return_status = REG_SUCCESS;

  /* Get the first num_iotype IO defs out of the table.  Assumes
     that Get_iotype_number has been called first to get the number
     of entries in the table. */

  if(labels == NULL || handles == NULL)return REG_FAILURE;

  if((isim = Sim_index_from_handle(sim_handle)) != -1){

    count = 0;

    for(i=0; i<Sim_table.sim[isim].IOdef_table.max_entries; i++){

      if(Sim_table.sim[isim].IOdef_table.io_def[i].handle != 
	 REG_IODEF_HANDLE_NOTSET){

	handles[count] = Sim_table.sim[isim].IOdef_table.io_def[i].handle;
	strcpy(labels[count], Sim_table.sim[isim].IOdef_table.io_def[i].label);

	types[count] = Sim_table.sim[isim].IOdef_table.io_def[i].direction;

	auto_io_supported[count] = 
	             Sim_table.sim[isim].IOdef_table.io_def[i].auto_io_support;

	/* If this IO type supports automatic emission/consumption then get
	   the current frequency at which this occurs */
	if(auto_io_supported[count]){

	  iparam = Param_index_from_handle(&(Sim_table.sim[isim].Params_table),
		                           Sim_table.sim[isim].IOdef_table.io_def[i].freq_param_handle);

	  if(iparam != REG_PARAM_HANDLE_NOTSET){

	    nitem = sscanf(Sim_table.sim[isim].Params_table.param[iparam].value, 
			   "%d", &(io_freqs[count]) );
	    if(nitem != 1){

#if DEBUG
	      fprintf(stderr, "Get_iotypes: failed to retrieve freq value\n");
#endif
	      io_freqs[count] = 0;
	      return_status = REG_FAILURE;
	    }
	  }
	  else{
#if DEBUG
	    fprintf(stderr, "Get_iotypes: failed to match param handle\n");
#endif
	    io_freqs[count] = 0;
	    return_status = REG_FAILURE;
	  }
	}
	count++;

	if(count == num_iotypes)break;
      }
    }
  }
  else{
    return_status = REG_FAILURE;
  }

  return return_status;
}

/*----------------------------------------------------------------------*/

int Set_iotype_freq(int sim_handle,
		    int num_iotypes,
		    int *iotype_handles,
		    int *freqs)
{
  int  isim;
  int  i;
  int  itype;
  char **val_array;
  char param_val[REG_MAX_STRING_LENGTH];
  int  return_status = REG_SUCCESS;

  /* A utility function that allows the steerer to update the emit/consume
     frequency associated with a given IOtype - the frequency itself is
     stored as a steerable parameter and therefore must be looked-up */

  if((isim = Sim_index_from_handle(sim_handle)) != REG_SIM_HANDLE_NOTSET){

    for(itype=0; itype<num_iotypes; itype++){

      /* Find IOdef with matching handle */
      for(i=0; i<Sim_table.sim[isim].IOdef_table.max_entries; i++){

	if(Sim_table.sim[isim].IOdef_table.io_def[i].handle ==
	   iotype_handles[itype]) break;
      }

      if(i==Sim_table.sim[isim].IOdef_table.max_entries){

#if DEBUG
	fprintf(stderr, "Set_iotype_freq: failed to match iotype handle\n");
#endif
	return_status = REG_FAILURE;
	continue;
      }

      /* Identify which entry in the parameter table corresponds to the
	 emit/consume frequency for this iodef */
      sprintf(param_val, "%d", freqs[itype]);

      *val_array = param_val;
      return_status =  Set_param_values(sim_handle,
					1,
					&(Sim_table.sim[isim].IOdef_table.io_def[i].freq_param_handle),
					val_array);
      
    }
  }
  else{

#if DEBUG
    printf("Set_iotype_freq: failed to match sim_handle\n");
#endif
    return_status = REG_FAILURE;
  }

  return return_status;
}

/*----------------------------------------------------------------------*/

int Command_supported(int sim_id,
		      int cmd_id)
{
  int return_status = REG_FAILURE;
  int i;

  /* Check to see whether or not the simulation with index sim_id
     supports command cmd_id - returns REG_SUCCESS if it does
     and REG_FAILURE if it doesn't */

  if(cmd_id < REG_MIN_IOTYPE_HANDLE){

    /* Command is a regular, pre-defined command */

    for(i=0; i<Sim_table.sim[sim_id].Cmds_table.num_registered; i++){

      if(cmd_id == Sim_table.sim[sim_id].Cmds_table.cmd[i].cmd_id){

	return_status = REG_SUCCESS;
	break;
      }
    }
  }
  else{

    /* Command is an IO type */

    for(i=0; i<Sim_table.sim[sim_id].IOdef_table.max_entries; i++){

      if(cmd_id == Sim_table.sim[sim_id].IOdef_table.io_def[i].handle){

	return_status = REG_SUCCESS;
	break;
      }
    }
  }
  
#if DEBUG
  if(return_status != REG_SUCCESS){

    printf("Command_supported: command %d is not supported by the sim.\n", 
	   cmd_id);
  }
#endif

  return return_status;
}

/*-------------------------------------------------------------------*/

int Get_supp_cmd_number(int sim_handle,
		        int *num_cmds)
{
  int isim;
  int return_status = REG_SUCCESS;
  
  /* Returns the number of commands supported by the specified sim */

  if (num_cmds == NULL) return REG_FAILURE;

  if((isim = Sim_index_from_handle(sim_handle)) != -1){

    *num_cmds = Sim_table.sim[isim].Cmds_table.num_registered;
  }
  else{
    return_status = REG_FAILURE;
  }

  return return_status;
}

/*-------------------------------------------------------------------*/

int Get_supp_cmds(int  sim_handle,
		  int  num_cmds,
		  int  *cmd_ids)
{
  int i;
  int isim;
  int loop_limit;
  int return_status = REG_SUCCESS;

  /* Returns the first num_cmds commands supported by the specified sim.
     Assumes that Get_supp_cmd_number has been called first to get the
     no. of entries in the table */

  if (cmd_ids == NULL) return REG_FAILURE;

  if((isim = Sim_index_from_handle(sim_handle)) != -1){

    if(num_cmds < Sim_table.sim[isim].Cmds_table.num_registered){

      loop_limit = num_cmds;
    }
    else{
      loop_limit = Sim_table.sim[isim].Cmds_table.num_registered;
    }
    
    /* Copy command id's out of table and into supplied array */

    for(i=0; i<loop_limit; i++){

      cmd_ids[i] = Sim_table.sim[isim].Cmds_table.cmd[i].cmd_id;
    }
  }
  else{
    return_status = REG_FAILURE;
  }

  return return_status;
}
