/*----------------------------------------------------------------------------
  This file contains routines and data structures that are common to
  the construction of a steering interface for both a steering
  component and a steered application.

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

#include "ReG_Steer_types.h"
#include "ReG_Steer_Common.h"
#include <sys/time.h>
#include <rpc/rpc.h>
#include <string.h>
/* These are for uname and gethostbyname */
#include <sys/utsname.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

#ifndef REG_DEBUG
#define REG_DEBUG 0
#endif

/* Location of file specifying the schema/namespace to which all
   steering messages should belong.  Set in Steerer_initialize or
   Steering_initialize (depending on whether we are steerer or
   application side) using the REG_STEER_HOME environment variable.
   If that is not set then we use a fake path (this info was only
   intended for validating xml messages which we do not normally do). */

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
      fp = NULL;
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
      fp = NULL;
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

#if REG_DEBUG
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

#if REG_DEBUG
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

#if REG_DEBUG
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

#if REG_DEBUG
  fprintf(stderr, "Remove_files: looking for files beginning: %s\n", filename);
#endif

  while(fp = Open_next_file(filename)){

    fclose(fp);

    /* Remove lock file */
    sprintf(lock_name, "%s.lock", filename);
#if REG_DEBUG
    fprintf(stderr, "Remove_files: deleting %s\n", lock_name);
#endif
    remove(lock_name);

#if NO_FILE_DELETE
    /* Don't delete actual data files if this debugging flag set */
    continue;
#endif

    /* Remove associated data file */
#if REG_DEBUG
    fprintf(stderr, "Remove_files: deleting %s\n", filename);
#endif
    remove(filename);

    /* Reset filename ready to look for next one */
    strcpy(filename, base_name);
  }

  return REG_SUCCESS;
}

/*----------------------------------------------------------*/

int Get_message_type(const char *name)
{

  if(strcmp(name, "Param_defs") == 0){
  
    return PARAM_DEFS;
  }
  else if(strcmp(name, "IOType_defs") == 0){
  
    return IO_DEFS;
  }
  else if(strcmp(name, "ChkType_defs") == 0){
  
    return CHK_DEFS;
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
  else if(strcmp(name, "Steer_log") == 0){

    return STEER_LOG;
  }
  else{

#if REG_DEBUG  
    fprintf(stderr, "Get_message_type: unrecognised message type: %s\n", name);
#endif /* REG_DEBUG */

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

      /* Initialise new entries */
      for(i=index; i<table->max_entries; i++){
	table->param[i].handle = REG_PARAM_HANDLE_NOTSET;
 	table->param[i].min_val_valid = REG_FALSE;
	table->param[i].max_val_valid = REG_FALSE;
     }
    }
  }

  return index;
}

/*--------------------------------------------------------------------*/

int Param_index_from_handle(Param_table_type *table, int ParamHandle)
{
  int i;
  int index = -1;

  if(ParamHandle == REG_PARAM_HANDLE_NOTSET) return -1;

  /* Finds entry in a table of parameters that has handle == ParamHandle
     Returns -1 if no match found */

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
	table->param[i].min_val_valid = REG_FALSE;
	table->param[i].max_val_valid = REG_FALSE;
      }
    }
    else{
      fprintf(stderr, "Increment_param_registered: failed to "
	      "allocate more param memory\n");
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
      fprintf(stderr, "Increment_cmd_registered: failed to allocate "
	      "more cmd memory\n");
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
      fprintf(stderr, "Increment_iodef_registered: failed to allocate "
	      "more IOdef memory\n");
      table->num_registered--;
      return_status = REG_FAILURE;
    }
  }

  return return_status;
}

/*--------------------------------------------------------------------*/

int Increment_log_entry(Chk_log_type *log)
{
  int   i, j;
  int   new_size;
  void *dum_ptr;
  int   return_status = REG_SUCCESS;
  
  if (!log) return REG_FAILURE;

  /* Increment count of how many entries table has and allocate
     more memory if required */
  if(++log->num_entries >= log->max_entries){

    new_size = log->max_entries + REG_INITIAL_CHK_LOG_SIZE;

    if(dum_ptr = realloc(log->entry, new_size*sizeof(Chk_log_entry_type))){

      log->entry = (Chk_log_entry_type *)dum_ptr;
      log->max_entries = new_size;

      /* Initialise the new storage space */
      for(i=log->num_entries; i<log->max_entries; i++){

	for(j=0; j<REG_MAX_NUM_STR_PARAMS; j++){
	  log->entry[i].param[j].handle = REG_PARAM_HANDLE_NOTSET;
	}
      }
    }
    else{
      fprintf(stderr, "Increment_log_entry: failed to allocate more "
	      "memory\n");
      log->num_entries--;
      return_status = REG_FAILURE;
    }
  }

  return return_status;
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


/*--------------------------------------------------------------------*/

int Write_xml_header(char **buf)
{
  int  return_status = REG_SUCCESS;

  /* Write header for a ReG steering message */

  if(buf){
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

int Write_xml_footer(char **buf, int bytes_free)
{
  int return_status = REG_SUCCESS;
  int n;
  /* Write end of ReG steering message */

  if(*buf){
    n = snprintf(*buf, bytes_free, "</ReG_steer_message>\n");

    if(n >= (bytes_free-1) || (n < 1)){

      return_status = REG_FAILURE;
    }
    else{
      *buf += n;
    }
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

/*----------------------------------------------------------------*/

int Read_file(char *filename, char **buf, int *size, int retain_newlines)
{ 
  FILE *fp;
  const int maxlen = 80;
  char  bufline[80];
  void *ptr;
  int   len;
  int   bufsize = BUFSIZ;
  int   newline_adj = 1;

  if( !(fp = fopen(filename, "r")) ){

    fprintf(stderr, "Read_file: failed to open file: %s\n", filename);
    return REG_FAILURE;
  }

  if(!(*buf = (char *)malloc(bufsize*sizeof(char)))){
    fprintf(stderr, "Read_file: malloc failed\n");
    return REG_FAILURE;
  }

  *size = 0;

  if(retain_newlines == REG_TRUE){
    newline_adj = 0;
  }

  while(fgets(bufline, maxlen, fp)){

    /* '- 1' to allow for '\n' or '- 0' to keep them */
    len = (int)strlen(bufline) - newline_adj;
    memcpy(&((*buf)[*size]), bufline, len);
    *size += len;

    if( (bufsize - *size) < maxlen){

      bufsize += BUFSIZ;
      ptr = realloc(*buf, (size_t)bufsize);
      if(!ptr){

	fprintf(stderr, "Read_file: realloc failed, size = %d\n", bufsize);
	free(*buf);
	*buf = NULL;
	fclose(fp);
	return REG_FAILURE;
      }
      *buf = (char *)ptr;
    }
  }

  (*buf)[*size] = 0;
  fclose(fp);

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

#ifdef USE_REG_TIMING

int Get_current_time_seconds(double *now)
{
  struct timeval tv;
  struct timezone tz;

  if(!now || gettimeofday(&tv, &tz)){
    return REG_FAILURE;
  }

  *now = (double)(tv.tv_sec) + 1.0e-6*(double)(tv.tv_usec);

  return REG_SUCCESS;
}

#endif /* defined USE_REG_TIMING */

/*----------------------------------------------------------------*/

int Reorder_decode_array(IOdef_entry *io,
			 int          type, 
			 int          count,
			 void        *pData)
{
  int         i, j, k;
  int         nslab, nrow;
  int        *pi, *pi_old;
  float      *pf, *pf_old;
  double     *pd, *pd_old;
  int         return_status = REG_SUCCESS;
  XDR         xdrs;
  Array_type *array;

  array = &(io->array);
  /*
  if( (io->convert_array_order == REG_TRUE) && (array->nx == 0) ){

    fprintf(stderr, "Reorder_decode_array: array has zero dimension\n");
    return REG_FAILURE;
  }
  */
  if(io->use_xdr){

#if REG_DEBUG
    fprintf(stderr, "Reorder_decode_array: doing XDR decode for type = %d\n",
	    type);
#endif

    xdrmem_create(&xdrs, 
		  io->buffer,
		  io->num_xdr_bytes,
		  XDR_DECODE);
  }

  if(io->use_xdr && io->convert_array_order != REG_TRUE){

    /* Straight xdr decode with no re-ordering */
    switch(type){

    case REG_INT:
      
      if(1 != xdr_vector(&xdrs, (char *)pData, (unsigned int)count, 
			 (unsigned int)sizeof(int), (xdrproc_t)xdr_int)){
	fprintf(stderr, "Reorder_decode_array: xdr_vector decode failed\n");
	return_status = REG_FAILURE;
      }
      break;

    case REG_FLOAT:
      
      if(1 != xdr_vector(&xdrs, (char *)pData, (unsigned int)count, 
			 (unsigned int)sizeof(float), (xdrproc_t)xdr_float)){
	fprintf(stderr, "Reorder_decode_array: xdr_vector decode failed\n");
	return_status = REG_FAILURE;
      }
      break;

    case REG_DBL:
      
      if(1 != xdr_vector(&xdrs, (char *)pData, (unsigned int)count, 
			 (unsigned int)sizeof(double), (xdrproc_t)xdr_double)){
	fprintf(stderr, "Reorder_decode_array: xdr_vector decode failed\n");
	return_status = REG_FAILURE;
      }
      break;

    default:
      fprintf(stderr, "Reorder_decode_array: unexpected datatype\n");
      return_status = REG_FAILURE;
      break;
    }
  }
  else if(io->convert_array_order == REG_TRUE){

    switch(type){

    case REG_INT:

      pi = (int *)pData;

      /* In this context, array->is_f90 flags whether we want to
	 convert _to_ an F90-style array */
      if(array->is_f90 != REG_TRUE){

	/* Convert F90 array to C array */

	nslab = (array->totz)*(array->toty);
	nrow  = array->totz;

	if(io->use_xdr){

	  /* Order loops so i,j,k vary as they should for an F90-style
	     array ordered consecutively in memory */
	  for(k=array->sz; k<(array->nz+array->sz); k++){
	    for(j=array->sy; j<(array->ny+array->sy); j++){
	      for(i=array->sx; i<(array->nx+array->sx); i++){
		/* Calculate position of (i,j,k)'th element in a C array 
		   (where k varies most rapidly) and store value. This
		   statement decodes the next value from the array we
		   specified when we opened 'xdrs'.*/
		xdr_int(&xdrs, &(pi[i*nslab + j*nrow + k]));
	      }
	    }
	  }
	}
	else{ /* No xdr-decode required */

	  /* Data we've read in is stored in io->buffer */
	  pi_old = (int *)io->buffer;

	  /* Order loops so i,j,k vary as they should for an F90-style
	     array ordered consecutively in memory */
	  for(k=array->sz; k<(array->nz+array->sz); k++){
	    for(j=array->sy; j<(array->ny+array->sy); j++){
	      for(i=array->sx; i<(array->nx+array->sx); i++){
		/* Calculate position of (i,j,k)'th element in a C array 
		   (where k varies most rapidly) and store value */
		pi[i*nslab + j*nrow + k] = *(pi_old++);
	      }
	    }
	  }
	}
      }
      else{
      
	/* Convert C array to F90 array */

	nslab = array->totx*array->toty;
	nrow  = array->totx;

	if(io->use_xdr){

	  /* Order loops so i,j,k vary as they should for a C-style
	     array ordered consecutively in memory */
	  for(i=array->sx; i<(array->nx+array->sx); i++){
	    for(j=array->sy; j<(array->ny+array->sy); j++){
	      for(k=array->sz; k<(array->nz+array->sz); k++){
		/* Calculate position of (i,j,k)'th element in an F90 array 
		   (where i varies most rapidly) and store value */
		xdr_int(&xdrs, &(pi[k*nslab + j*nrow + i]));
	      }
	    }
	  }
	}
	else{ /* No xdr-decode required */

	  /* Data we've read in is stored in io->buffer */
	  pi_old = (int *)io->buffer;

	  /* Order loops so i,j,k vary as they should for a C-style
	     array ordered consecutively in memory */
	  for(i=array->sx; i<(array->nx+array->sx); i++){
	    for(j=array->sy; j<(array->ny+array->sy); j++){
	      for(k=array->sz; k<(array->nz+array->sz); k++){
		/* Calculate position of (i,j,k)'th element in an F90 array 
		   (where i varies most rapidly) and store value */
		pi[k*nslab + j*nrow + i] = *(pi_old++);
	      }
	    }
	  }
	}
      }
      break;

    case REG_FLOAT:

      pf = (float *)pData;

      /* In this context, array->is_f90 flags whether we want to
	 convert _to_ an F90-style array */
      if(array->is_f90 != REG_TRUE){

	/* Convert F90 array to C array */
	
	nslab = (array->totz)*(array->toty);
	nrow  = array->totz;

	if(io->use_xdr){

	  /* Order loops so i,j,k vary as they should for an F90-style
	     array ordered consecutively in memory */
	  for(k=array->sz; k<(array->nz+array->sz); k++){
	    for(j=array->sy; j<(array->ny+array->sy); j++){
	      for(i=array->sx; i<(array->nx+array->sx); i++){
		/* Calculate position of (i,j,k)'th element in a C array 
		   (where k varies most rapidly) and store value */
		xdr_float(&xdrs, &(pf[i*nslab + j*nrow + k]));
	      }
	    }
	  }
	}
	else{ /* No xdr-decode required */

	  /* Data we've read in is stored in io->buffer */
	  pf_old = (float *)io->buffer;

	  /* Order loops so i,j,k vary as they should for an F90-style
	     array ordered consecutively in memory */
	  for(k=array->sz; k<(array->nz+array->sz); k++){
	    for(j=array->sy; j<(array->ny+array->sy); j++){
	      for(i=array->sx; i<(array->nx+array->sx); i++){
		/* Calculate position of (i,j,k)'th element in a C array 
		   (where k varies most rapidly) and store value */
		pf[i*nslab + j*nrow + k] = *(pf_old++);
	      }
	    }
	  }
	}
      }
      else{
	
	/* Convert C array to F90 array */
	
	nslab = (array->totx)*(array->toty);
	nrow  = array->totx;

	if(io->use_xdr){

	  /* Order loops so i,j,k vary as they should for a C-style
	     array ordered consecutively in memory */
	  for(i=array->sx; i<(array->nx+array->sx); i++){
	    for(j=array->sy; j<(array->ny+array->sy); j++){
	      for(k=array->sz; k<(array->nz+array->sz); k++){
		/* Calculate position of (i,j,k)'th element in an F90 array 
		   (where i varies most rapidly) and store value */
		xdr_float(&xdrs, &(pf[k*nslab + j*nrow + i]));
	      }
	    }
	  }
	}
	else{ /* No xdr-decode required */

	  /* Data we've read in is stored in io->buffer */
	  pf_old = (float *)pData;

	  /* Order loops so i,j,k vary as they should for a C-style
	     array ordered consecutively in memory */
	  for(i=array->sx; i<(array->nx+array->sx); i++){
	    for(j=array->sy; j<(array->ny+array->sy); j++){
	      for(k=array->sz; k<(array->nz+array->sz); k++){
		/* Calculate position of (i,j,k)'th element in an F90 array 
		   (where i varies most rapidly) and store value */
		pf[k*nslab + j*nrow + i] = *(pf_old++);
	      }
	    }
	  }
	}
      }
      break;

    case REG_DBL:
    
      pd = (double *)pData;

      /* In this context, array->is_f90 flags whether we want to
	 convert _to_ an F90-style array */
      if(array->is_f90 != REG_TRUE){
	
	/* Convert F90 array to C array */
	
	nslab = (array->totz)*(array->toty);
	nrow  = array->totz;

	if(io->use_xdr){

	  /* Order loops so i,j,k vary as they should for an F90-style
	   array ordered consecutively in memory */
	  for(k=array->sz; k<(array->nz+array->sz); k++){
	    for(j=array->sy; j<(array->ny+array->sy); j++){
	      for(i=array->sx; i<(array->nx+array->sx); i++){
		/* Calculate position of (i,j,k)'th element in a C array 
		   (where k varies most rapidly) and store value */
		xdr_double(&xdrs, &(pd[i*nslab + j*nrow + k]));
	      }
	    }
	  }
	}
	else{ /* No xdr-decode required */

	  /* Data we've read in is stored in io->buffer */
	  pd_old = (double *)io->buffer;

	  /* Order loops so i,j,k vary as they should for an F90-style
	   array ordered consecutively in memory */
	  for(k=array->sz; k<(array->nz+array->sz); k++){
	    for(j=array->sy; j<(array->ny+array->sy); j++){
	      for(i=array->sx; i<(array->nx+array->sx); i++){
		/* Calculate position of (i,j,k)'th element in a C array 
		   (where k varies most rapidly) and store value */
		pd[i*nslab + j*nrow + k] = *(pd_old++);
	      }
	    }
	  }
	}
      }
      else{
      
	/* Convert C array to F90 array */
	
	nslab = (array->totx)*(array->toty);
	nrow  = array->totx;

	if(io->use_xdr){

	  /* Order loops so i,j,k vary as they should for a C-style
	     array ordered consecutively in memory */
	  for(i=array->sx; i<(array->nx+array->sx); i++){
	    for(j=array->sy; j<(array->ny+array->sy); j++){
	      for(k=array->sz; k<(array->nz+array->sz); k++){
		/* Calculate position of (i,j,k)'th element in an F90 array 
		   (where i varies most rapidly) and store value */
		xdr_double(&xdrs, &(pd[k*nslab + j*nrow + i]));
	      }
	    }
	  }
	}
	else{ /* No xdr-decode required */

	  /* Data we've read in is stored in io->buffer */
	  pd_old = (double *)io->buffer;

	  /* Order loops so i,j,k vary as they should for a C-style
	     array ordered consecutively in memory */
	  for(i=array->sx; i<(array->nx+array->sx); i++){
	    for(j=array->sy; j<(array->ny+array->sy); j++){
	      for(k=array->sz; k<(array->nz+array->sz); k++){
		/* Calculate position of (i,j,k)'th element in an F90 array 
		   (where i varies most rapidly) and store value */
		pd[k*nslab + j*nrow + i] = *(pd_old++);
	      }
	    }
	  }
	}
      }
      break;
    
    default:
      break;
    }
  }

  if(io->use_xdr){
    xdr_destroy(&xdrs);
  }

  return return_status;
}

/*------------------------------------------------------------------*/

int Get_fully_qualified_hostname(char **hostname, char **ip_addr_ptr)
{
  struct utsname  name;
  struct hostent *host;
  char           *pchar;
  static char     ip_addr[16];

  /* First check to see if we're using an interface other than the default */
  if(pchar = getenv("REG_TCP_INTERFACE")){

    host = gethostbyname(pchar);
  }
  else{

    if(uname(&name) < 0){

      fprintf(stderr, "Get_fully_qualified_hostname: uname failed\n");
      return REG_FAILURE;
    }

    host = gethostbyname(name.nodename);
  }

  if(!host){

    fprintf(stderr, "Get_fully_qualified_hostname: gethostbyname failed\n");
    return REG_FAILURE;
  }

  if(host->h_length == 4){
    sprintf(ip_addr, "%d.%d.%d.%d", 
	    (unsigned char)(host->h_addr_list[0][0]),
	    (unsigned char)(host->h_addr_list[0][1]),
	    (unsigned char)(host->h_addr_list[0][2]),
	    (unsigned char)(host->h_addr_list[0][3]));
  }
  else{
    fprintf(stderr, "Get_fully_qualified_hostname: address not four "
	    "bytes long\n");
    return REG_FAILURE;
  }

  /* ARPDBG - lets break it to test *
  sprintf(host->h_name, "node12.beowulf.cluster");
  sprintf(ip_addr, " ");
  * end of ARPDBG */

#if REG_DEBUG
  fprintf(stderr, "Get_fully_qualified_hostname: hostname = %s\n", 
	  host->h_name);
  fprintf(stderr, "                              IP       = %s\n",
	  ip_addr);
#endif

  *hostname = host->h_name;
  *ip_addr_ptr = ip_addr;

  return REG_SUCCESS;
}
