/*----------------------------------------------------------------------------
    This file contains routines and data structures that are common to
    the construction of a steering interface for both a steering
    component and a steered application.

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

#include "ReG_Steer_types.h"
#include "ReG_Steer_Common.h"

#define DEBUG 0

/* Location of file specifying the schema/namespace to which all
   steering messages should belong.  Set in Steerer_initialize or
   Steering_initialize (depending on whether we are steerer or
   application side) using the REG_STEER_HOME environment variable */

char ReG_Steer_Schema_Locn[REG_MAX_STRING_LENGTH];

/*----------------------------------------------------------*/

FILE *Open_next_file(char* base_name)
{
  FILE *fp;
  char  filename[REG_MAX_STRING_LENGTH+9];
  char  filename1[REG_MAX_STRING_LENGTH+9];
  char  filename2[REG_MAX_STRING_LENGTH+9];
  int   i;

  i = 0;
  /*
  while(i<REG_MAX_NUM_FILES){
    
     Look for presence of lock file 
    sprintf(filename1,"%s_%d.lock", base_name, i);
    fp = fopen(filename1, "r");

    if (fp != NULL) {
     
       Found one - open associated data file 
      fclose(fp);

      sprintf(filename,"%s_%d", base_name, i);
      
      if(fp = fopen(filename, "r")){

#if DEBUG
	printf("Open_next_file: opening %s\n", filename);
#endif
	 Return the name of the file actually opened 
	strcpy(base_name, filename);
      }
      break;
    }

    i++;
  }
  */

  while(i<REG_MAX_NUM_FILES){
    
    /* Look for presence of lock file */
    sprintf(filename,"%s_%d.lock", base_name, i);
    fp = fopen(filename, "r");

    if (fp != NULL) {
     
      /* Found one - open associated data file */
      fclose(fp);

      sprintf(filename,"%s_%d", base_name, i);
      
      if(fp = fopen(filename, "r")){

#if DEBUG
	printf("Open_next_file: opening %s\n", filename);
#endif
	/* Return the name of the file actually opened */
	strcpy(base_name, filename);
      }
      break;
    }

    i++;
  }

  return fp;
}

/*-----------------------------------------------------------------*/

int Create_lock_file(char *filename)
{
  char lock_file[REG_MAX_STRING_LENGTH+11];

  /* Create lock file to flag that a file of same root is ready to
     be read */

  sprintf(lock_file, "touch %s.lock", filename);

  if(system(lock_file)){

    return REG_FAILURE;
  }
  
  return REG_SUCCESS;
}

/*-----------------------------------------------------------------*/

int Delete_file(char *filename)
{
  int  return_status = REG_SUCCESS;
  char long_filename[REG_MAX_STRING_LENGTH+5];

  /* Remove lock file first (because this is what is searched for) */

  sprintf(long_filename, "%s.lock", filename);

#if DEBUG
  printf("Delete_file: removing %s\n", long_filename);
#endif

  if(remove(long_filename)){
    perror("Delete_file, deleting lock file");
    return_status = REG_FAILURE;
  }

#if NO_FILE_DELETE
  /* If this debugging flag is set then don't remove the data file */
  return REG_SUCCESS;
#endif

  /* Remove the data file */

#if DEBUG
  printf("Delete_file: removing %s\n", filename);
#endif

  if(remove(filename)){
    perror("Delete_file, deleting data file");
    return_status = REG_FAILURE;
  }
  return return_status;
}
/*----------------------------------------------------------------*/

int Remove_files(char* base_name)
{
  char  filename[REG_MAX_STRING_LENGTH];
  char  lock_name[REG_MAX_STRING_LENGTH];
  FILE *fp;

  /* Remove any files that we would have normally consumed */

  strcpy(filename, base_name);

#if DEBUG
  printf("Remove_files: looking for files beginning: %s\n", filename);
#endif

  while(fp = Open_next_file(filename)){

    fclose(fp);

    /* Remove lock file */
    sprintf(lock_name, "%s.lock", filename);
#if DEBUG
    printf("Remove_files: deleting %s\n", lock_name);
#endif
    remove(lock_name);

#if NO_FILE_DELETE
    /* Don't delete actual data files if this debugging flag set */
    continue;
#endif

    /* Remove associated data file */
#if DEBUG
    printf("Remove_files: deleting %s\n", filename);
#endif
    remove(filename);

    /* Reset filename ready to look for next one */
    strcpy(filename, base_name);
  }

  return REG_SUCCESS;
}

/*-----------------------------------------------------------------*/

void startElement(void *userData, const char *name, const char **atts)
{
  user_data_type *user_data = userData;

#if DEBUG
  printf("startElement: name = %s\n", name);
#endif /* DEBUG */

  /* This is the start of a field within a message */
  
  switch(user_data->msg_type){
  
  case MSG_NOTSET:
  
    user_data->msg_type = Get_message_type(name);

    if(user_data->msg_type == MSG_NOTSET){

#if DEBUG
      printf("startElement: unrecognised message type: %s\n", name);
#endif /* DEBUG */
    }
    break;
  
  case PARAM_DEFS:
    /* Already within a param def message so identify which field 
       this is */
    Set_param_field_type((void *)(user_data->gen_xml_struct), name);
    break;
  
  case IO_DEFS:
    Set_iodef_field_type((void *)(user_data->gen_xml_struct), name);
    break;
  
  case SUPP_CMDS:
#if DEBUG
    printf("Start of supp cmds element\n");
#endif /* DEBUG */
    Set_supp_cmd_field_type((void *)(user_data->gen_xml_struct), name);
    break;

  case STATUS:
    Set_status_field_type((void *)(user_data->gen_xml_struct), name);
    break;

  case CONTROL:
#if DEBUG
    printf("Start of steering-control element\n");
#endif /* DEBUG */
    Set_ctrl_field_type((void *)(user_data->gen_xml_struct), name);
    break;

  default:
#if DEBUG
    printf("startElement: unrecognised message type\n");
#endif /* DEBUG */
    break;
  }

}

/*----------------------------------------------------------*/

REG_MsgType Get_message_type(const char *name)
{

  if(strcmp(name, "Param_defs") == 0){
  
    return PARAM_DEFS;
  }
  else if(strcmp(name, "IOType_defs") == 0){
  
    return IO_DEFS;
  }
  else if(strcmp(name, "Supported_commands") == 0){
    
    return SUPP_CMDS;
  }
  else if(strcmp(name, "Steer_control") == 0){
  
    return CONTROL;
  }
  else if(strcmp(name, "App_status") == 0){
  
    return STATUS;
  }
  else{

#if DEBUG  
    printf("Get_message_type: unrecognised message type: %s\n", name);
#endif /* DEBUG */

    return MSG_NOTSET;
  }

}

/*-----------------------------------------------------------------*/

void Set_param_field_type(void *ptr, const char* name)
{
  param_xml_struct *param_struct;

  param_struct = (param_xml_struct *)(ptr);

  if(strcmp(name, "Label") == 0){
    param_struct->field_type = PARAM_LABEL;
  }
  else if(strcmp(name, "Steerable") == 0){
    param_struct->field_type = STRABLE;
  }
  else if(strcmp(name, "Type") == 0){
    param_struct->field_type = TYPE;
  }
  else if(strcmp(name, "Handle") == 0){
    param_struct->field_type = HANDLE;
  }
  else if(strcmp(name, "Value") == 0){
    param_struct->field_type = VALUE;
  }

}

/*-----------------------------------------------------------------*/

void Set_iodef_field_type(void *ptr, const char* name)
{
  iodef_xml_struct *iodef_struct;

  iodef_struct = (iodef_xml_struct *)(ptr);

  if(strcmp(name, "Label") == 0){
    iodef_struct->field_type = IODEF_LABEL;
  }
  else if(strcmp(name, "Handle") == 0){
    iodef_struct->field_type = IODEF_HANDLE;
  }
  /* 'direction' field not currently used
  else if(strcmp(name, "Direction") == 0){
    iodef_struct->field_type = IODEF_DIRN;
  }
  */
  else{
    printf("Set_iodef_field_type: unrecognised field type\n");
  }

}

/*-----------------------------------------------------------------*/

void Set_supp_cmd_field_type(void *ptr, const char* name)
{
  supp_cmds_xml_struct *supp_cmds_struct;

  supp_cmds_struct = (supp_cmds_xml_struct *)(ptr);

#if DEBUG
  printf("Set_supp_cmd_field_type: name = %s\n", name);
#endif /* DEBUG */

  if(strcmp(name, "Cmd_id") == 0){
    supp_cmds_struct->field_type = CMD_ID;
  }
  else if(strcmp(name, "Cmd_param") == 0){
    supp_cmds_struct->field_type = CMD_PARAM;
  }
  else{
#if DEBUG
    printf("Set_supp_cmd_field_type: unrecognised field type\n");
#endif
  }

}

/*-----------------------------------------------------------------*/

void Set_ctrl_field_type(void *ptr, const char* name)
{
  control_xml_struct *ctrl_struct;

  ctrl_struct = (control_xml_struct *)(ptr);

  switch(ctrl_struct->field_type){

  case CTRL_NOTSET:

    if(strcmp(name, "Command") == 0){

      ctrl_struct->field_type = CTRL_CMD;
    }
    else if(strcmp(name, "Param") == 0){

      ctrl_struct->field_type = CTRL_PARAM;
    }
    break;

  case CTRL_PARAM:

    Set_param_field_type((void *)ctrl_struct->param_struct, name);
    break;

  case CTRL_CMD:
    Set_supp_cmd_field_type((void *)ctrl_struct->cmd_struct, name);
    break;

  default:
    break;
  }
}

/*-----------------------------------------------------------------*/

void Set_status_field_type(void *ptr, const char* name)
{
  status_xml_struct *status_struct;

  status_struct = (status_xml_struct *)(ptr);

  switch(status_struct->field_type){

  case STAT_NOTSET:

    if(strcmp(name, "Command") == 0){

      status_struct->field_type = STAT_CMD;
    }
    else if(strcmp(name, "Param") == 0){

      status_struct->field_type = STAT_PARAM;
    }
    break;
  
  case STAT_PARAM:

    Set_param_field_type((void *)status_struct->param_struct, name);
    break;

  case STAT_CMD:

    Set_supp_cmd_field_type((void *)status_struct->cmd_struct, name);
    break;

  default:
    break;
  }
}

/*-----------------------------------------------------------------*/

void endElement(void *userData, const char *name)
{
  param_xml_struct     *param_struct;
  supp_cmds_xml_struct *supp_cmds_struct;
  iodef_xml_struct     *iodef_struct;
  control_xml_struct   *ctrl_struct;
  status_xml_struct    *status_struct;

  user_data_type *data = userData;

#if DEBUG
  printf("endElement: name = %s\n", name);
#endif

  switch(data->msg_type){

  case MSG_NOTSET:
#if DEBUG
    printf("endElement: Warning - no element flagged as started\n");
#endif
    break;

  case SUPP_CMDS:
    supp_cmds_struct = (supp_cmds_xml_struct *)(data->gen_xml_struct);

    if(supp_cmds_struct->field_type != CMD_NOTSET){

      if(supp_cmds_struct->field_type == CMD_PARAM){

	if(supp_cmds_struct->param_struct->field_type != PARAM_NOTSET){

	  /* End of a parameter sub-element */
	  supp_cmds_struct->param_struct->field_type = PARAM_NOTSET;
	}
	else{

	  /* Have reached end of a parameter element */
	  Increment_param_registered(supp_cmds_struct->param_struct->table);
	  supp_cmds_struct->field_type = CMD_NOTSET;
	}
      }
      else{

	/* Have reached end of a command sub-element */
	supp_cmds_struct->field_type = CMD_NOTSET;
      }
    }
    else{

      /* If name is a recognised message type then we've come to
	 the end of the message.  If it isn't then we've only
	 come to the end of a sub-element of the message */
      if( Get_message_type(name) != MSG_NOTSET ){

        data->msg_type = MSG_NOTSET;
      }
      else{
	/* Increment counter & allocate more memory if required */
	Increment_cmd_registered(supp_cmds_struct->table);
      }
    }
    break;

  case IO_DEFS:
    iodef_struct = (iodef_xml_struct *)(data->gen_xml_struct);

    if(iodef_struct->field_type != IODEF_NOTSET){

      iodef_struct->field_type = IODEF_NOTSET;
    }
    else{

      /* If name is a recognised message type then we've come to
	 the end of the message.  If it isn't then we've only
	 come to the end of a sub-element of the message */
      if( Get_message_type(name) != MSG_NOTSET ){

        data->msg_type = MSG_NOTSET;
      }
      else{
	Increment_iodef_registered(iodef_struct->table);
      }
    }
    break;

  case PARAM_DEFS:
    param_struct = (param_xml_struct *)(data->gen_xml_struct);

    if(param_struct->field_type != PARAM_NOTSET){

      param_struct->field_type = PARAM_NOTSET;
    }
    else{

      /* If name is a recognised message type then we've come to
	 the end of the message.  If it isn't then we've only
	 come to the end of a sub-element of the message */
      if( Get_message_type(name) != MSG_NOTSET ){

	data->msg_type = MSG_NOTSET;
      }
      else{
	Increment_param_registered(param_struct->table);
      }
    }
    break;

  case STATUS:
    status_struct = (status_xml_struct *)(data->gen_xml_struct);

    if(status_struct->field_type != STAT_NOTSET){

      if(status_struct->field_type == STAT_PARAM){

	if(status_struct->param_struct->field_type != PARAM_NOTSET){

	  /* Have reached the end of a param sub-element */
	  status_struct->param_struct->field_type = PARAM_NOTSET;
	}
	else{

	  /* Have reached end of param element */
	  Increment_param_registered(status_struct->param_struct->table);
	  status_struct->field_type = STAT_NOTSET;
	}
      }
      else{

	if(status_struct->cmd_struct->field_type != CMD_NOTSET){

	  /* Have reached the end of a cmd sub-element */
	  status_struct->cmd_struct->field_type = CMD_NOTSET;
	}
	else{

	  /* Have reached end of command element */
	  Increment_cmd_registered(status_struct->cmd_struct->table);
	  status_struct->field_type = STAT_NOTSET;
	}
      }
    }
    else{

      /* If name is a recognised message type then we've come to
	 the end of the message.  If it isn't then we've only
	 come to the end of a sub-element of the message */
      if( Get_message_type(name) != MSG_NOTSET ){

        data->msg_type = MSG_NOTSET;
      }
    }
    break;

  case CONTROL:
    ctrl_struct = (control_xml_struct *)(data->gen_xml_struct);

    if(ctrl_struct->field_type != CTRL_NOTSET){

      if(ctrl_struct->field_type == CTRL_PARAM){

	if(ctrl_struct->param_struct->field_type != PARAM_NOTSET){

	  /* Have reached end of some element of param */
	  ctrl_struct->param_struct->field_type = PARAM_NOTSET;
	}
	else{

	  /* Have reached end of an actual param element */
	  Increment_param_registered(ctrl_struct->param_struct->table);
	  ctrl_struct->field_type = CTRL_NOTSET;
	}
      }
      else{

	if(ctrl_struct->cmd_struct->field_type != CMD_NOTSET){

	  /* Have reached end of command sub-element */
	  ctrl_struct->cmd_struct->field_type = CMD_NOTSET;
	}
	else{

	  /* Have reached end of command element */
	  Increment_cmd_registered(ctrl_struct->cmd_struct->table);
	  ctrl_struct->field_type = CTRL_NOTSET;
	}
      }
    }
    else{

      /* If name is a recognised message type then we've come to
	 the end of the message.  If it isn't then we've only
	 come to the end of a sub-element of the message */
      if( Get_message_type(name) != MSG_NOTSET ){

       data->msg_type = MSG_NOTSET;
      }
    }
    break;
  }

}

/*-----------------------------------------------------------------*/

void dataHandler(void *userData, const XML_Char *s, int len)
{
  char            buf[REG_MAX_STRING_LENGTH];
  user_data_type *data = userData;
  
  /* Create null-terminated string from input array of XML_Char */
  strncpy(buf, s, len);
  buf[len] = 0;

  /* This routine gets called if we have sub-elements beginning
     on new lines so check for this */
  if(buf[0] == '\n'){
    return;
  }

  switch(data->msg_type){

  case MSG_NOTSET:
#if DEBUG
    printf("dataHandler: Warning - no element flagged as started\n");
#endif
    break;

  case SUPP_CMDS:
#if DEBUG
    printf("dataHandler: calling Store_supp_cmds_field_value\n");
    printf("dataHandler: buf = %s\n", buf);
#endif
    Store_supp_cmds_field_value(data->gen_xml_struct, buf);
    break;

  case IO_DEFS:
#if DEBUG
    printf("dataHandler: calling Store_iodef_field_value\n");
    printf("dataHandler: buf = %s\n", buf);
#endif
    Store_iodef_field_value(data->gen_xml_struct, buf);
    break;

  case PARAM_DEFS:
#if DEBUG
    printf("dataHandler: calling Store_param_field_value\n");
    printf("dataHandler: buf = %s\n", buf);
#endif
    Store_param_field_value(data->gen_xml_struct, buf);
    break;

  case STATUS:
#if DEBUG
    printf("dataHandler: calling Store_status_field_value\n");
    printf("dataHandler: buf = %s\n", buf);
#endif
    Store_status_field_value(data->gen_xml_struct, buf);
    break;

  case CONTROL:
#if DEBUG
    printf("dataHandler: calling Store_control_field_value\n");
    printf("dataHandler: buf = %s\n", buf);
#endif
    Store_control_field_value(data->gen_xml_struct, buf);
    break;

  default:
#if DEBUG
    printf("dataHandler: unrecognised message type\n");
#endif
    break;
  }

}

/*----------------------------------------------------------*/

void Store_control_field_value(void *ptr, char *buf)
{
  control_xml_struct *ctrl_struct;

  ctrl_struct = (control_xml_struct *)(ptr);

  switch(ctrl_struct->field_type){

  case CTRL_NOTSET:
    break;

  case CTRL_CMD:
    
    Store_supp_cmds_field_value((void*)ctrl_struct->cmd_struct, buf);
    break;

  case CTRL_PARAM:

    Store_param_field_value((void*)ctrl_struct->param_struct, buf);
    break;

  default:
#if DEBUG
    printf("Store_control_field_value: unrecognised field_type\n");
#endif
    break;
  }

}

/*----------------------------------------------------------*/

void Store_status_field_value(void *ptr, char *buf)
{
  status_xml_struct *status_struct;

  status_struct = (status_xml_struct *)(ptr);

  switch(status_struct->field_type){

  case STAT_NOTSET:
    break;

  case STAT_CMD:

    Store_supp_cmds_field_value((void*)status_struct->cmd_struct, buf);
    break;

  case STAT_PARAM:

    Store_param_field_value((void*)status_struct->param_struct, buf);
    break;

  default:
#if DEBUG
    printf("Store_status_field_value: unrecognised status field_type\n");
#endif
    break;
  }

}

/*----------------------------------------------------------*/

void Store_param_field_value(void *ptr, char *buf)
{
  int               index;
  param_xml_struct *param_struct;

  param_struct = (param_xml_struct *)(ptr);

  index = param_struct->table->num_registered;

#if DEBUG
  printf("Store_param_field_value: index = %d\n", index);
#endif

  switch(param_struct->field_type){

  case PARAM_NOTSET:
    break;

  case PARAM_LABEL:
    strcpy(param_struct->table->param[index].label, buf);
    break;

  case STRABLE:
    sscanf(buf , "%d", &(param_struct->table->param[index].steerable) );
    break;

  case TYPE:
    sscanf(buf , "%d", &(param_struct->table->param[index].type) );
    break;

  case HANDLE:
    sscanf(buf , "%d", &(param_struct->table->param[index].handle) );
    break;

  case VALUE:

    strcpy(param_struct->table->param[index].value, buf);
    break;

  default:
#if DEBUG
    printf("Store_param_field_value: no element ID set\n");
#endif
    break;
  }  
}

/*----------------------------------------------------------*/

void Store_supp_cmds_field_value(void *ptr, char *buf)
{
  int                   index;
  supp_cmds_xml_struct *cmd_struct;

  cmd_struct = (supp_cmds_xml_struct *)(ptr);

  index = cmd_struct->table->num_registered;

#if DEBUG
  printf("Store_supp_cmds_field_value: index = %d\n", index);
  printf("                               buf = %s\n", buf);
  printf("                             table = %p\n", cmd_struct->table);
#endif

  switch(cmd_struct->field_type){

  case CMD_NOTSET:
    break;

  case CMD_ID:
    sscanf(buf , "%d", &(cmd_struct->table->cmd[index].cmd_id) );    
#if DEBUG
    printf("Store_supp_cmds_field_value: stored id = %d\n",
	   cmd_struct->table->cmd[index].cmd_id);
#endif
    break;

  case CMD_PARAM:
    Store_param_field_value((void*)cmd_struct->param_struct, buf);
    break;

  default:
#if DEBUG
    printf("Store_supp_cmds_field_value: unrecognised field type\n");
    printf("                             field type = %d\n", 
	   cmd_struct->field_type);
#endif
    break;
  }
}

/*----------------------------------------------------------*/

void Store_iodef_field_value(void *ptr, char *buf)
{
  int               index;
  iodef_xml_struct *iodef_struct;

  iodef_struct = (iodef_xml_struct *)(ptr);

  index = iodef_struct->table->num_registered;

  switch(iodef_struct->field_type){

  case IODEF_NOTSET:
    printf("Store_IOdef_field_value: field type not set\n");
    break;

  case IODEF_LABEL:
    strcpy(iodef_struct->table->io_def[index].label, buf);
    break;

  case IODEF_HANDLE:
    sscanf(buf, "%d", &(iodef_struct->table->io_def[index].handle) );
    break;

  /* 'direction' field not currently used
  case IODEF_DIRN:
    sscanf(buf, "%d", &(iodef_struct->table->io_def[index].direction) );
    break;
  */

  default:
#if DEBUG
    printf("Store_IOdef_field_value: unrecognised field type\n");
#endif
    break;
  }
}

/*----------------------------------------------------------------*/

int Next_free_param_index(Param_table_type *table)
{
  int   i;
  int   index = -1;
  int   new_size;
  void *dum_ptr;

  /* Look for first free entry in table - i.e. one that has an
     unset handle.  If none found then extends the size of the table. */

  for(i=0; i<table->max_entries; i++){

    if(table->param[i].handle == REG_PARAM_HANDLE_NOTSET){

      index = i;
      break;
    }
  }

  if(index == -1){

    /* No free entries - need to allocate more memory */

    new_size = table->max_entries + REG_INITIAL_NUM_PARAMS;
    
    if(dum_ptr = (void *)realloc(table->param, new_size*sizeof(param_entry))){

      index = table->max_entries;
      table->param = (param_entry *)dum_ptr;
      table->max_entries = new_size;
    }
  }

  return index;
}

/*--------------------------------------------------------------------*/

int Increment_param_registered(Param_table_type *table)
{
  int   new_size;
  void *dum_ptr;
  int   i;
  int   return_status = REG_SUCCESS;

  table->num_registered++;

  /* Allocate more memory if required */

  if(table->num_registered == table->max_entries){

    new_size = table->max_entries + REG_INITIAL_NUM_PARAMS;

    if(dum_ptr = (void*)realloc(table->param, new_size*sizeof(param_entry))){

      table->param = (param_entry*)dum_ptr;
      table->max_entries = new_size;

      for(i=table->num_registered; i<table->max_entries; i++){

	table->param[i].handle = REG_PARAM_HANDLE_NOTSET;
      }
    }
    else{
      printf("endElement: failed to allocate more param memory\n");
      table->num_registered--;
      return_status = REG_FAILURE;
    }
  }

  return return_status;
}

/*--------------------------------------------------------------------*/

int Increment_cmd_registered(Supp_cmd_table_type *table)
{
  int   new_size;
  void *dum_ptr;
  int   return_status = REG_SUCCESS;

  table->num_registered++;

  /* Allocate more memory if required */

  if(table->num_registered == table->max_entries){

    new_size = table->max_entries + REG_INITIAL_NUM_CMDS;

    if(dum_ptr = (void*)realloc(table->cmd, new_size*sizeof(supp_cmd_entry))){

      table->cmd = (supp_cmd_entry*)dum_ptr;
      table->max_entries = new_size;
    }
    else{
      printf("endElement: failed to allocate more cmd memory\n");
      table->num_registered--;
      return_status = REG_FAILURE;
    }
  }

  return return_status;
}

/*--------------------------------------------------------------------*/

int Increment_iodef_registered(IOdef_table_type *table)
{
  int   new_size;
  void *dum_ptr;
  int   i;
  int   return_status = REG_SUCCESS;

  table->num_registered++;

  /* Allocate more memory if required */

  if(table->num_registered == table->max_entries){

    new_size = table->max_entries + REG_INITIAL_NUM_IOTYPES;

    if(dum_ptr = (void*)realloc(table->io_def, new_size*sizeof(IOdef_entry))){

      table->io_def = (IOdef_entry*)dum_ptr;
      table->max_entries = new_size;

      for(i=table->num_registered; i<table->max_entries; i++){

	table->io_def[i].handle = REG_IODEF_HANDLE_NOTSET;
      }
    }
    else{
      printf("endElement: failed to allocate more IOdef memory\n");
      table->num_registered--;
      return_status = REG_FAILURE;
    }
  }

  return return_status;
}

/*--------------------------------------------------------------------*/

int Write_xml_header(FILE *fp)
{
  int return_status = REG_SUCCESS;

  /* Write header for a ReG steering message */

  if(fp){

    fprintf(fp, "<?xml version=\"1.0\"?>\n");

    fprintf(fp, "<ReG_steer_message xmlns:xsi=\""
	        "http://www.w3.org/2001/XMLSchema-instance\"\n");
    fprintf(fp, "xmlns=\"%s\"\n", REG_STEER_NAMESPACE);
    fprintf(fp, "       xsi:SchemaLocation=\"%s %s\">\n", 
	        REG_STEER_NAMESPACE,
	        ReG_Steer_Schema_Locn);
  }
  else{
    return_status = REG_FAILURE;
  }

  return return_status;
}

/*--------------------------------------------------------------------*/

int Write_xml_footer(FILE *fp)
{
  int return_status = REG_SUCCESS;

  /* Write end of ReG steering message */

  if(fp){
    fprintf(fp, "</ReG_steer_message>\n");
  }
  else{
    return_status = REG_FAILURE;
  }

  return return_status;
}

/*-----------------------------------------------------------------*/

void scan_startElement(void *userData, const char *name, const char **atts)
{
  REG_MsgType *msg_type = (REG_MsgType *)userData;

  /* Used when scanning a message to see what sort it is - we just keep
     scanning until we find an element that is a recognised message
     type */

  if(*msg_type != MSG_NOTSET) return;

  *msg_type = Get_message_type(name);

  return;
}

/*-----------------------------------------------------------------*/

void scan_endElement(void *userData, const char *name)
{
  /* Used when scanning a message to see what sort it is - this routine
     is not actually needed in this case but have to pass something
     to the XML parser */

  return;
}

/*-----------------------------------------------------------------*/

int Directory_valid(char *directory)
{
  char  system_str[REG_MAX_STRING_LENGTH];
  FILE *fp;

  /* Test to see that we can write a file to the specified
     directory */

  sprintf(system_str, "%s%s", directory, "test_file");
  fp = fopen(system_str, "w");

  if(fp){

    fclose(fp);
    remove(system_str);
    return REG_SUCCESS;
  }
  else{

    return REG_FAILURE;
  }
}
