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
#ifdef UNICORE_DEMO
    sprintf(tmp_filename,"%s.lock", base_name);
#else
    sprintf(tmp_filename,"%s_%d.lock", base_name, i);
#endif

    fp = fopen(tmp_filename, "r");

    if (fp != NULL) {
     
      /* Found one - check its last-modified time */
      fclose(fp);
      if(stat(tmp_filename, &stbuf) != -1){

        /* timespec_t     st_mtim;      Time of last data modification
           Times measured in seconds and nanoseconds
           since 00:00:00 UTC, Jan. 1, 1970 */
#ifdef UNICORE_DEMO
        sprintf(filename1,"%s", base_name);
#else
        sprintf(filename1,"%s_%d", base_name, i);
#endif
	time1 = (long)stbuf.st_mtime;
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
#ifdef UNICORE_DEMO
    sprintf(tmp_filename,"%s.lock", base_name);
#else
    sprintf(tmp_filename,"%s_%d.lock", base_name, i);
#endif

    fp = fopen(tmp_filename, "r");

    if (fp != NULL) {
     
      /* Found one - check its last-modified time */
      fclose(fp);
      if(stat(tmp_filename, &stbuf) != -1){

        /* timespec_t     st_mtim;      Time of last data modification
           Times measured in seconds and nanoseconds
           since 00:00:00 UTC, Jan. 1, 1970 */
#ifdef UNICORE_DEMO
        sprintf(filename2,"%s", base_name);
#else
        sprintf(filename2,"%s_%d", base_name, i);
#endif
	time2 = (long)stbuf.st_mtime;/*.tv_sec;*/
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
      fprintf(stderr, "expat_endElement: failed to allocate more param memory\n");
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
      fprintf(stderr, "expat_endElement: failed to allocate more cmd memory\n");
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
      fprintf(stderr, "expat_endElement: failed to allocate more IOdef memory\n");
      table->num_registered--;
      return_status = REG_FAILURE;
    }
  }

  return return_status;
}

/*--------------------------------------------------------------------*/

int Write_xml_header(char **buf)
{
  int return_status = REG_SUCCESS;

  /* Write header for a ReG steering message */

  if(buf){

    *buf += sprintf(*buf, "<?xml version=\"1.0\"?>\n");

    *buf += sprintf(*buf, "<ReG_steer_message xmlns:xsi=\""
	        "http://www.w3.org/2001/XMLSchema-instance\"\n");
    *buf += sprintf(*buf, "xmlns=\"%s\"\n", REG_STEER_NAMESPACE);
    *buf += sprintf(*buf, "       xsi:SchemaLocation=\"%s %s\">\n", 
		    REG_STEER_NAMESPACE,
		    ReG_Steer_Schema_Locn);
  }
  else{
    return_status = REG_FAILURE;
  }

  return return_status;
}

/*--------------------------------------------------------------------*/

int Write_xml_footer(char **buf)
{
  int return_status = REG_SUCCESS;

  /* Write end of ReG steering message */

  if(*buf){
    *buf += sprintf(*buf, "</ReG_steer_message>\n");
  }
  else{
    return_status = REG_FAILURE;
  }

  return return_status;
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
