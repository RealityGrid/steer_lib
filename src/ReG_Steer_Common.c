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

#ifndef DEBUG
#define DEBUG 0
#endif

/* Location of file specifying the schema/namespace to which all
   steering messages should belong.  Set in Steerer_initialize or
   Steering_initialize (depending on whether we are steerer or
   application side) using the REG_STEER_HOME environment variable */

char ReG_Steer_Schema_Locn[REG_MAX_STRING_LENGTH];

/*----------------------------------------------------------*/

FILE *Open_next_file(char* base_name)
{
  FILE *fp;
  char  tmp_filename[REG_MAX_STRING_LENGTH+9];
  char  filename1[REG_MAX_STRING_LENGTH+9];
  char  filename2[REG_MAX_STRING_LENGTH+9];
  struct stat stbuf;
  long  time1;
  long  time2;
  int   i;

  fp = NULL;

  i = 0;
  time1 = -1;
  while(i<REG_MAX_NUM_FILES){
    
    /* Look for presence of lock file */
    sprintf(tmp_filename,"%s_%d.lock", base_name, i);
    fp = fopen(tmp_filename, "r");

    if (fp != NULL) {
     
      /* Found one - check its last-modified time */
      fclose(fp);
      if(stat(tmp_filename, &stbuf) != -1){

        /* timespec_t     st_mtim;      Time of last data modification
           Times measured in seconds and nanoseconds
           since 00:00:00 UTC, Jan. 1, 1970 */
        sprintf(filename1,"%s_%d", base_name, i);
	time1 = (long)stbuf.st_mtim.tv_sec;
        break;
      }
      else{

	fprintf(stderr, "Open_next_file: failed to stat %s\n", tmp_filename);
      }
    }

    i++;
  }

  /* Now search in the opposite direction (in case consumption lags
     creation and we've wrapped around the REG_MAX_NUM_FILES counter) */

  i = REG_MAX_NUM_FILES - 1;
  time2 = -1;
  while(i > -1){
    
    /* Look for presence of lock file */
    sprintf(tmp_filename,"%s_%d.lock", base_name, i);
    fp = fopen(tmp_filename, "r");

    if (fp != NULL) {
     
      /* Found one - check its last-modified time */
      fclose(fp);
      if(stat(tmp_filename, &stbuf) != -1){

        /* timespec_t     st_mtim;      Time of last data modification
           Times measured in seconds and nanoseconds
           since 00:00:00 UTC, Jan. 1, 1970 */
        sprintf(filename2,"%s_%d", base_name, i);
	time2 = (long)stbuf.st_mtim.tv_sec;
        break;
      }
      else{

	fprintf(stderr, "Open_next_file: failed to stat %s\n", tmp_filename);
      }
    }

    i--;
  }

  /* We want to open the oldest file that we've found... */

  if(time1 != -1 && time2 != -1){

    if(time2 < time1) strcpy(filename1, filename2);

    if(fp = fopen(filename1, "r")){

#if DEBUG
      fprintf(stderr, "Open_next_file: opening %s\n", filename1);
#endif
      /* Return the name of the file actually opened */
      strcpy(base_name, filename1);
    }
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
  fprintf(stderr, "Delete_file: removing %s\n", long_filename);
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
  fprintf(stderr, "Delete_file: removing %s\n", filename);
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
  fprintf(stderr, "Remove_files: looking for files beginning: %s\n", filename);
#endif

  while(fp = Open_next_file(filename)){

    fclose(fp);

    /* Remove lock file */
    sprintf(lock_name, "%s.lock", filename);
#if DEBUG
    fprintf(stderr, "Remove_files: deleting %s\n", lock_name);
#endif
    remove(lock_name);

#if NO_FILE_DELETE
    /* Don't delete actual data files if this debugging flag set */
    continue;
#endif

    /* Remove associated data file */
#if DEBUG
    fprintf(stderr, "Remove_files: deleting %s\n", filename);
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
  fprintf(stderr, "startElement: name = %s\n", name);
#endif /* DEBUG */

  /* This is the start of a field within a message */
  
  switch(user_data->msg_type){
  
  case MSG_NOTSET:
  
    user_data->msg_type = Get_message_type(name);

    if(user_data->msg_type == MSG_NOTSET){

#if DEBUG
      fprintf(stderr, "startElement: unrecognised message type: %s\n", name);
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
    fprintf(stderr, "Start of supp cmds element\n");
#endif /* DEBUG */
    Set_supp_cmd_field_type((void *)(user_data->gen_xml_struct), name);
    break;

  case STATUS:
    Set_status_field_type((void *)(user_data->gen_xml_struct), name);
    break;

  case CONTROL:
#if DEBUG
    fprintf(stderr, "Start of steering-control element\n");
#endif /* DEBUG */
    Set_ctrl_field_type((void *)(user_data->gen_xml_struct), name);
    break;

  default:
#if DEBUG
    fprintf(stderr, "startElement: unrecognised message type\n");
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
    fprintf(stderr, "Get_message_type: unrecognised message type: %s\n", name);
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
  else if(strcmp(name, "Is_internal") == 0){
    param_struct->field_type = IS_INTERNAL;
  }
  else{
#if DEBUG
    fprintf(stderr, "Set_param_field_type: unrecognised field type\n");
#endif
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
  else if(strcmp(name, "Direction") == 0){
    iodef_struct->field_type = IODEF_DIRN;
  }
  else if(strcmp(name, "Support_auto_io") == 0){
    iodef_struct->field_type = IODEF_AUTO_SUPP;
  }
  else if(strcmp(name, "Freq_handle") == 0){
    iodef_struct->field_type = IODEF_FREQ_HANDLE;
  }
  else{
    fprintf(stderr, "Set_iodef_field_type: unrecognised field type\n");
  }

}

/*-----------------------------------------------------------------*/

void Set_supp_cmd_field_type(void *ptr, const char* name)
{
  supp_cmds_xml_struct *supp_cmds_struct;

  supp_cmds_struct = (supp_cmds_xml_struct *)(ptr);

#if DEBUG
  fprintf(stderr, "Set_supp_cmd_field_type: name = %s\n", name);
#endif /* DEBUG */

  if(strcmp(name, "Cmd_id") == 0){
    supp_cmds_struct->field_type = CMD_ID;
  }
  else if(strcmp(name, "Cmd_param") == 0){
    supp_cmds_struct->field_type = CMD_PARAM;
  }
  else{
#if DEBUG
    fprintf(stderr, "Set_supp_cmd_field_type: unrecognised field type\n");
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
  fprintf(stderr, "endElement: name = %s\n", name);
#endif

  switch(data->msg_type){

  case MSG_NOTSET:
#if DEBUG
    fprintf(stderr, "endElement: Warning - no element flagged as started\n");
#endif
    break;

  case SUPP_CMDS:
    supp_cmds_struct = (supp_cmds_xml_struct *)(data->gen_xml_struct);

    if(supp_cmds_struct->field_type != CMD_NOTSET){

      if(supp_cmds_struct->field_type == CMD_PARAM){

	/* Command parameters not currently implemented..

	if(supp_cmds_struct->param_struct->field_type != PARAM_NOTSET){

	  / End of a parameter sub-element /
	  supp_cmds_struct->param_struct->field_type = PARAM_NOTSET;
	}
	else{

	  / Have reached end of a parameter element /
	  Increment_param_registered(supp_cmds_struct->param_struct->table);
 	  supp_cmds_struct->field_type = CMD_NOTSET;
	}

	...just flag section as ended... */

	supp_cmds_struct->field_type = CMD_NOTSET;
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
	if(supp_cmds_struct->read_status != REG_FAILURE){
	  Increment_cmd_registered(supp_cmds_struct->table);
	}
	else{
	  /* Unset the 'failed' flag as we've reached end of element */
	  supp_cmds_struct->read_status = REG_SUCCESS;
	}
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
	if(iodef_struct->read_status != REG_FAILURE){
  	  Increment_iodef_registered(iodef_struct->table);
	}
	else{
	  /* Unset the 'failed' flag as we've reached end of element */
	  iodef_struct->read_status = REG_SUCCESS;
	}
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
	if(param_struct->read_status != REG_FAILURE){
	  Increment_param_registered(param_struct->table);
	}
	else{
	  /* Unset the 'failed' flag as we've reached end of element */
	  param_struct->read_status = REG_SUCCESS;
	}
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
	  if(status_struct->param_struct->read_status != REG_FAILURE){
	    Increment_param_registered(status_struct->param_struct->table);
	  }
	  else{
	    /* Unset the 'failed' flag as we've reached end of element */
	    status_struct->param_struct->read_status = REG_SUCCESS;
	  }
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
	  if(status_struct->cmd_struct->read_status != REG_FAILURE){
	    Increment_cmd_registered(status_struct->cmd_struct->table);
	  }
	  else{
	    /* Unset the 'failed' flag as we've reached end of element */
	    status_struct->cmd_struct->read_status = REG_SUCCESS;
	  }
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
	  if(ctrl_struct->param_struct->read_status != REG_FAILURE){
	    Increment_param_registered(ctrl_struct->param_struct->table);
	  }
	  else{
	    /* Unset the 'failed' flag as we've reached end of element */
	    ctrl_struct->param_struct->read_status = REG_SUCCESS;
	  }
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
	  if(ctrl_struct->cmd_struct->read_status != REG_FAILURE){
	    Increment_cmd_registered(ctrl_struct->cmd_struct->table);
	  }
	  else{
	    /* Unset the 'failed' flag as we've reached end of element */
	    ctrl_struct->cmd_struct->read_status = REG_SUCCESS;
	  }
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
    fprintf(stderr, "dataHandler: Warning - no element flagged as started\n");
#endif
    break;

  case SUPP_CMDS:
#if DEBUG
    fprintf(stderr, "dataHandler: calling Store_supp_cmds_field_value\n");
    fprintf(stderr, "dataHandler: buf = %s\n", buf);
#endif
    Store_supp_cmds_field_value(data->gen_xml_struct, buf);
    break;

  case IO_DEFS:
#if DEBUG
    fprintf(stderr, "dataHandler: calling Store_iodef_field_value\n");
    fprintf(stderr, "dataHandler: buf = %s\n", buf);
#endif
    Store_iodef_field_value(data->gen_xml_struct, buf);
    break;

  case PARAM_DEFS:
#if DEBUG
    fprintf(stderr, "dataHandler: calling Store_param_field_value\n");
    fprintf(stderr, "dataHandler: buf = %s\n", buf);
#endif
    Store_param_field_value(data->gen_xml_struct, buf);
    break;

  case STATUS:
#if DEBUG
    fprintf(stderr, "dataHandler: calling Store_status_field_value\n");
    fprintf(stderr, "dataHandler: buf = %s\n", buf);
#endif
    Store_status_field_value(data->gen_xml_struct, buf);
    break;

  case CONTROL:
#if DEBUG
    fprintf(stderr, "dataHandler: calling Store_control_field_value\n");
    fprintf(stderr, "dataHandler: buf = %s\n", buf);
#endif
    Store_control_field_value(data->gen_xml_struct, buf);
    break;

  default:
#if DEBUG
    fprintf(stderr, "dataHandler: unrecognised message type\n");
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
    fprintf(stderr, "Store_control_field_value: unrecognised field_type\n");
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
    fprintf(stderr, 
	    "Store_status_field_value: unrecognised status field_type\n");
#endif
    break;
  }

}

/*----------------------------------------------------------*/

void Store_param_field_value(void *ptr, char *buf)
{
  int               index;
  int               nitem;
  param_xml_struct *param_struct;

  param_struct = (param_xml_struct *)(ptr);

  index = param_struct->table->num_registered;

#if DEBUG
  fprintf(stderr, "Store_param_field_value: index = %d\n", index);
#endif

  switch(param_struct->field_type){

  case PARAM_NOTSET:
    break;

  case PARAM_LABEL:
    if(strlen(buf)){

      strcpy(param_struct->table->param[index].label, buf);
    }
    else{
      param_struct->read_status = REG_FAILURE;
    }
    break;

  case STRABLE:
    nitem = sscanf(buf , "%d", &(param_struct->table->param[index].steerable));

    if(nitem != 1) param_struct->read_status = REG_FAILURE;
    break;

  case TYPE:
    nitem = sscanf(buf , "%d", &(param_struct->table->param[index].type) );

    if(nitem != 1) param_struct->read_status = REG_FAILURE;
    break;

  case HANDLE:
    nitem = sscanf(buf , "%d", &(param_struct->table->param[index].handle) );

    if(nitem != 1) param_struct->read_status = REG_FAILURE;
    break;

  case VALUE:

    if(strlen(buf)){

      strcpy(param_struct->table->param[index].value, buf);
    }
    else{
      param_struct->read_status = REG_FAILURE;
    }
    break;

  case IS_INTERNAL:

    if(strcmp(buf, "TRUE") == 0){

      param_struct->table->param[index].is_internal = TRUE;
    }
    else{
      param_struct->table->param[index].is_internal = FALSE;
    }
    break;

  default:
#if DEBUG
    fprintf(stderr, "Store_param_field_value: no element ID set\n");
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
  fprintf(stderr, "Store_supp_cmds_field_value: index = %d\n", index);
  fprintf(stderr, "                               buf = %s\n", buf);
  fprintf(stderr, "                             table = %p\n", 
	  cmd_struct->table);
#endif

  switch(cmd_struct->field_type){

  case CMD_NOTSET:
    break;

  case CMD_ID:
    sscanf(buf , "%d", &(cmd_struct->table->cmd[index].cmd_id) );    
#if DEBUG
    fprintf(stderr, "Store_supp_cmds_field_value: stored id = %d\n",
	   cmd_struct->table->cmd[index].cmd_id);
#endif
    break;

  case CMD_PARAM:
    /* Command parameters not currently implemented... 
    Store_param_field_value((void*)cmd_struct->param_struct, buf);
    */
    break;

  default:
#if DEBUG
    fprintf(stderr, "Store_supp_cmds_field_value: unrecognised field type\n");
    fprintf(stderr, "                             field type = %d\n", 
	    cmd_struct->field_type);
#endif
    break;
  }
}

/*----------------------------------------------------------*/

void Store_iodef_field_value(void *ptr, char *buf)
{
  int               index;
  char              input[REG_MAX_STRING_LENGTH];
  iodef_xml_struct *iodef_struct;

  iodef_struct = (iodef_xml_struct *)(ptr);

  index = iodef_struct->table->num_registered;

  switch(iodef_struct->field_type){

  case IODEF_NOTSET:
    fprintf(stderr, "Store_IOdef_field_value: field type not set\n");
    break;

  case IODEF_LABEL:
    strcpy(iodef_struct->table->io_def[index].label, buf);
    break;

  case IODEF_HANDLE:
    sscanf(buf, "%d", &(iodef_struct->table->io_def[index].handle) );
    break;

  case IODEF_DIRN:
    sscanf(buf, "%s", input);

    if(strcmp(input, "IN") == 0){
      iodef_struct->table->io_def[index].direction = REG_IO_IN;
    }
    else if(strcmp(input, "OUT") == 0){
      iodef_struct->table->io_def[index].direction = REG_IO_OUT;
    }
    else if(strcmp(input, "CHECKPOINT") == 0){
      iodef_struct->table->io_def[index].direction = REG_IO_CHKPT;
    }
    else{

#if DEBUG
      fprintf(stderr, "Store_IOdef_field_value: unrecognised direction type\n");
#endif
      /* Set it to something to be on the safe side... */
      iodef_struct->table->io_def[index].direction = REG_IO_IN;
    }
    break;

  case IODEF_AUTO_SUPP:
    sscanf(buf, "%s", input);

    if(strcmp(input, "TRUE") == 0){

      iodef_struct->table->io_def[index].auto_io_support = TRUE;
    }
    else if(strcmp(input, "FALSE") == 0){

      iodef_struct->table->io_def[index].auto_io_support = FALSE;
    }
    else{

#if DEBUG
      fprintf(stderr, 
	      "Store_IOdef_field_value: unrecognised auto_supp value\n");
#endif
      /* Set to false to be on the safe side */
      iodef_struct->table->io_def[index].auto_io_support = FALSE;
    }
    break;

  case IODEF_FREQ_HANDLE:
    sscanf(buf, "%d", &(iodef_struct->table->io_def[index].freq_param_handle) );
    break;

  default:
#if DEBUG
    fprintf(stderr, "Store_IOdef_field_value: unrecognised field type\n");
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

int Param_index_from_handle(Param_table_type *table, int ParamHandle)
{
  int i;
  int index = REG_PARAM_HANDLE_NOTSET;

  /* Finds entry in a table of parameters that has handle == ParamHandle
     Returns REG_PARAM_HANDLE_NOTSET if no match found */

  for(i=0; i<table->max_entries; i++){

    if(table->param[i].handle == ParamHandle){

      index = i;
      break;
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
      fprintf(stderr, "endElement: failed to allocate more param memory\n");
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
      fprintf(stderr, "endElement: failed to allocate more cmd memory\n");
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
      fprintf(stderr, "endElement: failed to allocate more IOdef memory\n");
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
