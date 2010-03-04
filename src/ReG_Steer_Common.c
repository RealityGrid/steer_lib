/*
  The RealityGrid Steering Library

  Copyright (c) 2002-2010, University of Manchester, United Kingdom.
  All rights reserved.

  This software is produced by Research Computing Services, University
  of Manchester as part of the RealityGrid project and associated
  follow on projects, funded by the EPSRC under grants GR/R67699/01,
  GR/R67699/02, GR/T27488/01, EP/C536452/1, EP/D500028/1,
  EP/F00561X/1.

  LICENCE TERMS

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

    * Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of The University of Manchester nor the names
      of its contributors may be used to endorse or promote products
      derived from this software without specific prior written
      permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
  COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.

  Author: Andrew Porter
          Robert Haines
 */

/** @internal
    @file ReG_Steer_Common.c
    @brief Source file for utility routines used in both Appside and Steerside

    This file contains routines and data structures that are common to
    the construction of a steering interface for both a steering
    component and a steered application.

    @author Andrew Porter
    @author Robert Haines
 */

#include "ReG_Steer_Config.h"
#include "ReG_Steer_types.h"
#include "ReG_Steer_Common.h"

/** Basic library config. Declared here as used by all. */
Steer_lib_config_type Steer_lib_config;

/*----------------------------------------------------------*/

int Get_message_type(const char *name)
{

  if(strcmp(name, "Param_defs") == 0){

#ifdef REG_DEBUG_FULL
    fprintf(stderr, "STEER: Setting msg type to PARAM_DEFS\n");
#endif
    return PARAM_DEFS;
  }
  else if(strcmp(name, "IOType_defs") == 0){

#ifdef REG_DEBUG_FULL
    fprintf(stderr, "STEER: Setting msg type to IO_DEFS\n");
#endif
    return IO_DEFS;
  }
  else if(strcmp(name, "ChkType_defs") == 0){

#ifdef REG_DEBUG_FULL
    fprintf(stderr, "STEER: Setting msg type to CHK_DEFS\n");
#endif
    return CHK_DEFS;
  }
  else if(strcmp(name, "Supported_commands") == 0){

#ifdef REG_DEBUG_FULL
    fprintf(stderr, "STEER: Setting msg type to SUPP_CMDS\n");
#endif
    return SUPP_CMDS;
  }
  else if(strcmp(name, "Steer_control") == 0){

#ifdef REG_DEBUG_FULL
    fprintf(stderr, "STEER: Setting msg type to CONTROL\n");
#endif
    return CONTROL;
  }
  else if(strcmp(name, "App_status") == 0){

#ifdef REG_DEBUG_FULL
    fprintf(stderr, "STEER: Setting msg type to STATUS\n");
#endif
    return STATUS;
  }
  else if(strcmp(name, "Steer_log") == 0){

#ifdef REG_DEBUG_FULL
    fprintf(stderr, "STEER: Setting msg type to STEER_LOG\n");
#endif
    return STEER_LOG;
  }
  else{

#ifdef REG_DEBUG
    fprintf(stderr, "STEER: Get_message_type: unrecognised message type: %s\n", name);
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

    if( (dum_ptr = (void *)realloc(table->param,
				   new_size*sizeof(param_entry))) ){

      index = table->max_entries;
      table->param = (param_entry *)dum_ptr;
      table->max_entries = new_size;

      /* Initialise new entries */
      for(i=index; i<table->max_entries; i++){
	Init_param_entry(&(table->param[i]));
     }
    }
  }

  return index;
}

/*--------------------------------------------------------------------*/

void Init_param_entry(param_entry *param)
{
    param->handle   = REG_PARAM_HANDLE_NOTSET;
    param->modified = REG_FALSE;
    param->log      = NULL;
    param->log_index= 0;
    param->log_size = 0;
    param->ptr_raw  = NULL;
    param->raw_buf_size = 0;
    return;
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

    if( (dum_ptr = (void*)realloc(table->param,
				  new_size*sizeof(param_entry))) ){

      table->param = (param_entry*)dum_ptr;
      table->max_entries = new_size;

      for(i=table->num_registered; i<table->max_entries; i++){

	table->param[i].handle = REG_PARAM_HANDLE_NOTSET;
	table->param[i].min_val_valid = REG_FALSE;
	table->param[i].max_val_valid = REG_FALSE;
      }
    }
    else{
      fprintf(stderr, "STEER: Increment_param_registered: failed to "
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

    if( (dum_ptr = (void*)realloc(table->cmd,
				  new_size*sizeof(supp_cmd_entry))) ){

      table->cmd = (supp_cmd_entry*)dum_ptr;
      table->max_entries = new_size;
    }
    else{
      fprintf(stderr, "STEER: Increment_cmd_registered: failed to allocate "
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

    if( (dum_ptr = (void*)realloc(table->io_def,
				  new_size*sizeof(IOdef_entry))) ){

      table->io_def = (IOdef_entry*)dum_ptr;
      table->max_entries = new_size;

      for(i=table->num_registered; i<table->max_entries; i++){

	table->io_def[i].handle = REG_IODEF_HANDLE_NOTSET;
      }
    }
    else{
      fprintf(stderr, "STEER: Increment_iodef_registered: failed to allocate "
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

    if( (dum_ptr = realloc(log->entry,
			   new_size*sizeof(Chk_log_entry_type))) ){

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
      fprintf(stderr, "STEER: Increment_log_entry: failed to allocate more "
	      "memory\n");
      log->num_entries--;
      return_status = REG_FAILURE;
    }
  }

  return return_status;
}

/*------------------------------------------------------------------*/

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

  if(buf){
    *buf += sprintf(*buf, "<ReG_steer_message xmlns=\"%s\">\n",
		    REG_STEER_NAMESPACE);
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

int Get_scratch_directory() {
  char* pchar;
  int   i;
  int   len;
  int   max_len;

  pchar = getenv("REG_STEER_DIRECTORY");

  if(pchar) {

    /* Check that path ends in '/' - if not then add one */

    i = strlen(pchar);

    /* Check that we've got enough memory to hold full path
       for steering-message filenames */
    max_len = strlen(REG_LOG_FILENAME);
    if((len = strlen(APP_STEERABLE_FILENAME)) > max_len) {
      max_len = len;
    }
    if((len = strlen(STR_CONNECTED_FILENAME)) > max_len) {
      max_len = len;
    }
    if((len = strlen(APP_TO_STR_FILENAME "_1000.lock")) > max_len) {
      max_len = len;
    }
    if((len = strlen(STR_TO_APP_FILENAME "_1000.lock")) > max_len) {
      max_len = len;
    }

    if((i + max_len + 1) > REG_MAX_STRING_LENGTH) {

      fprintf(stderr, "STEER: Get_scratch_directory: "
	      "REG_MAX_STRING_LENGTH (%d chars) less\nthan predicted "
	      "string length of %d chars\n", REG_MAX_STRING_LENGTH,
	      (i+max_len+1));
      return REG_FAILURE;
    }

    if(pchar[i-1] != '/') {
      sprintf(Steer_lib_config.scratch_dir, "%s/", pchar);
    }
    else{
      strcpy(Steer_lib_config.scratch_dir, pchar);
    }

    if(Directory_valid(Steer_lib_config.scratch_dir) != REG_SUCCESS) {
      fprintf(stderr, "STEER: Get_scratch_directory: invalid scratch dir: %s\n",
	      Steer_lib_config.scratch_dir);
      return REG_FAILURE;
    }
    else {
      fprintf(stderr, "STEER: Using following dir for scratch: %s\n",
	     Steer_lib_config.scratch_dir);
    }
  }
  else {
    fprintf(stderr, "STEER: Get_scratch_directory: failed to get "
	    "scratch directory from REG_STEER_DIRECTORY. Using default (%s).\n",
	    REG_SCRATCH_DEFAULT);
    sprintf(Steer_lib_config.scratch_dir, REG_SCRATCH_DEFAULT);
  }

  return REG_SUCCESS;
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

int Read_file(const char *filename,
	      char **buf, int *size,
	      const int retain_newlines)
{
  FILE *fp;
  const int maxlen = 80;
  char  bufline[80];
  void *ptr;
  int   len;
  int   bufsize = BUFSIZ;
  int   newline_adj = 1;

  if( !(fp = fopen(filename, "r")) ){

    fprintf(stderr, "STEER: Read_file: failed to open file: %s\n",
	    filename);
    return REG_FAILURE;
  }

  if(!(*buf = (char *)malloc(bufsize*sizeof(char)))){
    fprintf(stderr, "STEER: Read_file: malloc failed\n");
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

	fprintf(stderr, "STEER: Read_file: realloc failed, size = %d\n",
		bufsize);
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


int Get_current_time_seconds(double *now)
{
#if REG_USE_TIMING
  struct timeval tv;
  struct timezone tz;

  if(!now || gettimeofday(&tv, &tz)){
    return REG_FAILURE;
  }

  *now = (double)(tv.tv_sec) + 1.0e-6*(double)(tv.tv_usec);
#else
  *now = 0.0;
#endif /* REG_USE_TIMING */

  return REG_SUCCESS;
}

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
  long       *pl, *pl_old;
  int         return_status = REG_SUCCESS;
  XDR         xdrs;
  Array_type *array;

  array = &(io->array);
  /*
  if( (io->convert_array_order == REG_TRUE) && (array->nx == 0) ){

    fprintf(stderr, "STEER: Reorder_decode_array: array has zero dimension\n");
    return REG_FAILURE;
  }
  */
  if(io->use_xdr){

#ifdef REG_DEBUG
    fprintf(stderr, "STEER: Reorder_decode_array: doing XDR decode for type = %d\n",
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
	fprintf(stderr, "STEER: Reorder_decode_array: xdr_vector decode "
		"failed for REG_INT\n");
	return_status = REG_FAILURE;
      }
      break;

    case REG_LONG:

      if(1 != xdr_vector(&xdrs, (char *)pData, (unsigned int)count,
			 (unsigned int)sizeof(int), (xdrproc_t)xdr_long)){
	fprintf(stderr, "STEER: Reorder_decode_array: xdr_vector decode "
		"failed for REG_LONG\n");
	return_status = REG_FAILURE;
      }
      break;

    case REG_FLOAT:

      if(1 != xdr_vector(&xdrs, (char *)pData, (unsigned int)count,
			 (unsigned int)sizeof(float), (xdrproc_t)xdr_float)){
	fprintf(stderr, "STEER: Reorder_decode_array: xdr_vector decode "
		"failed for REG_FLOAT\n");
	return_status = REG_FAILURE;
      }
      break;

    case REG_DBL:

      if(1 != xdr_vector(&xdrs, (char *)pData, (unsigned int)count,
			 (unsigned int)sizeof(double), (xdrproc_t)xdr_double)){
	fprintf(stderr, "STEER: Reorder_decode_array: xdr_vector decode "
		"failed for REG_DBL\n");
	return_status = REG_FAILURE;
      }
      break;

    default:
      fprintf(stderr, "STEER: Reorder_decode_array: unexpected datatype\n");
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


    case REG_LONG:

      pl = (long *)pData;

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
		xdr_long(&xdrs, &(pl[i*nslab + j*nrow + k]));
	      }
	    }
	  }
	}
	else{ /* No xdr-decode required */

	  /* Data we've read in is stored in io->buffer */
	  pl_old = (long *)io->buffer;

	  /* Order loops so i,j,k vary as they should for an F90-style
	     array ordered consecutively in memory */
	  for(k=array->sz; k<(array->nz+array->sz); k++){
	    for(j=array->sy; j<(array->ny+array->sy); j++){
	      for(i=array->sx; i<(array->nx+array->sx); i++){
		/* Calculate position of (i,j,k)'th element in a C array
		   (where k varies most rapidly) and store value */
		pl[i*nslab + j*nrow + k] = *(pl_old++);
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
		xdr_long(&xdrs, &(pl[k*nslab + j*nrow + i]));
	      }
	    }
	  }
	}
	else{ /* No xdr-decode required */

	  /* Data we've read in is stored in io->buffer */
	  pl_old = (long *)io->buffer;

	  /* Order loops so i,j,k vary as they should for a C-style
	     array ordered consecutively in memory */
	  for(i=array->sx; i<(array->nx+array->sx); i++){
	    for(j=array->sy; j<(array->ny+array->sy); j++){
	      for(k=array->sz; k<(array->nz+array->sz); k++){
		/* Calculate position of (i,j,k)'th element in an F90 array
		   (where i varies most rapidly) and store value */
		pl[k*nslab + j*nrow + i] = *(pl_old++);
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
#ifndef WIN32
  struct utsname  name;
  struct hostent *host;
  char           *pchar;
  static char     ip_addr[16];

  /* First check to see if we're using an interface other than the default */
  if( (pchar = getenv("REG_TCP_INTERFACE")) ){

    host = gethostbyname(pchar);
  }
  else{

    if(uname(&name) < 0){

      fprintf(stderr, "STEER: Get_fully_qualified_hostname: uname failed\n");
      return REG_FAILURE;
    }

    host = gethostbyname(name.nodename);
  }

  if(!host){

    fprintf(stderr, "STEER: Get_fully_qualified_hostname: gethostbyname failed\n");
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
    fprintf(stderr, "STEER: Get_fully_qualified_hostname: address not four "
	    "bytes long\n");
    return REG_FAILURE;
  }

  /* ARPDBG - lets break it to test *
  sprintf(host->h_name, "node12.beowulf.cluster");
  sprintf(ip_addr, " ");
  * end of ARPDBG */

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: Get_fully_qualified_hostname: hostname = %s\n",
	  host->h_name);
  fprintf(stderr, "                                     IP       = %s\n",
	  ip_addr);
#endif

  *hostname = host->h_name;
  *ip_addr_ptr = ip_addr;

  return REG_SUCCESS;

#else
  HOSTENT *hostDetailsStruct;
  char FAR hostNameBuffer[64];

  gethostname (&hostNameBuffer[0], 64);
  hostDetailsStruct = gethostbyname(hostNameBuffer);

  *hostname = hostNameBuffer;
  *ip_addr_ptr = hostDetailsStruct->h_addr_list[0];

  return REG_SUCCESS;
#endif
}

/*----------------------------------------------------------------*/

char *Get_current_time_string()
{
  struct timeval tv;
  struct timezone tz;
  struct tm *now_details;
  static char date_string[128];

  gettimeofday(&tv, &tz);

  now_details = gmtime(&(tv.tv_sec));
  /* 2005-08-31T14:31:51Z */
  sprintf(date_string,"%d-%02d-%02dT%02d:%02d:%02dZ",
	  (now_details->tm_year) + 1900,
	  (now_details->tm_mon) + 1,
	  now_details->tm_mday,
	  now_details->tm_hour,
	  now_details->tm_min,
	  now_details->tm_sec);

  return date_string;
}

/*-------------------------------------------------------------------*/

void Wipe_security_info(struct reg_security_info *sec){

  sec->use_ssl = 0;
  sec->caCertsPath[0] = '\0';
  sec->myKeyCertFile[0] = '\0';
  sec->userDN[0] = '\0';
  sec->passphrase[0] = '\0';
}

/*----------------------------------------------------------------*/

int Get_security_config(const char               *configFile,
			struct reg_security_info *sec){
  char      *pChar;
  int        len;
  xmlDocPtr  doc;
  xmlNodePtr cur;
  xmlChar   *attrValue;
  FILE      *fp;
  char       bufline[512];

  sec->myKeyCertFile[0] = '\0';
  sec->caCertsPath[0] = '\0';

  /* Default to using ~/.realitygrid/security.conf unless we're told
     otherwise */
  if(!configFile || (strlen(configFile) == 0)){
    pChar = getenv("HOME");
    if(!pChar){
      fprintf(stderr, "STEERUtils: Get_security_config: cannot get HOME environment "
	      "variable and no alternative config. file specified\n");
      return REG_FAILURE;
    }
    snprintf(bufline, 512, "%s/.realitygrid/security.conf", pChar);
  }
  else if(strlen(configFile) > 2) { /* minimum length will be ~/a ie 3 */
    int replace = 0;
    if(strncmp(configFile, "$HOME", 5) == 0)
      replace = 5;
    else if(strncmp(configFile, "${HOME}", 7) == 0)
      replace = 5;
    else if(strncmp(configFile, "~", 1) == 0)
      replace = 1;

    if(replace != 0) {
      pChar = getenv("HOME");
      if(!pChar) {
	fprintf(stderr, "STEERUtils: Get_security_config: cannot get HOME environment "
		"variable for ~ or $HOME substitution\n");
	return REG_FAILURE;
      }
      snprintf(bufline, 512, "%s%s", pChar, &configFile[replace]);
    }
    else{
      strncpy(bufline, configFile, 512);
    }
  }

  /* Set the username to the value of the USER environment variable
     in case we fail to get/parse the certificate for the DN */
  if( (pChar = getenv("USER")) ){
    snprintf(sec->userDN, REG_MAX_STRING_LENGTH, "%s", pChar);
  }

  /* Parse the security.conf file */

  doc = xmlParseFile(bufline);
  if( !(cur = xmlDocGetRootElement(doc)) ){
    printf("Error parsing xml from security.conf: empty document\n");
    xmlFreeDoc(doc);
    xmlCleanupParser();
    return REG_FAILURE;
  }
  if (xmlStrcmp(cur->name, (const xmlChar *) "Security_config")){
    printf("Error parsing xml from security.conf: root element "
           "is not 'Security_config'\n");
    xmlFreeDoc(doc);
    xmlCleanupParser();
    return REG_FAILURE;
  }
  cur = cur->xmlChildrenNode;
  /* Walk the tree - search for first non-blank node */
  while ( cur ){
    if(xmlIsBlankNode ( cur ) ){
      cur = cur -> next;
      continue;
    }
    if( !xmlStrcmp(cur->name, (const xmlChar *)"caCertsPath") ){
      attrValue = xmlGetProp(cur, (const xmlChar*) "value");
      if(attrValue){
        len = xmlStrlen(attrValue);
        strncpy(sec->caCertsPath, (char *)attrValue, len);
        sec->caCertsPath[len] = '\0';
#ifdef REG_DEBUG
        printf("Get_security_config: caCertsPath >>%s<<\n", sec->caCertsPath);
#endif
        xmlFree(attrValue);
      }
    }
    else if( !xmlStrcmp(cur->name, (const xmlChar *)"privateKeyCertFile") ){
      attrValue = xmlGetProp(cur, (const xmlChar*) "value");
      if(attrValue){
        len = xmlStrlen(attrValue);
        strncpy(sec->myKeyCertFile, (char *)attrValue, len);
        sec->myKeyCertFile[len] = '\0';
#ifdef REG_DEBUG
        printf("Get_security_config: myKeyCertFile >>%s<<\n",
	       sec->myKeyCertFile);
#endif
        xmlFree(attrValue);
      }
    }
    cur = cur->next;
  }

  /* Config file exists but does not specify any path to CA certs
     or key+cert file */
  if(!(sec->myKeyCertFile[0]) || !(sec->caCertsPath[0])){
    xmlFreeDoc(doc);
    xmlCleanupParser();
    return REG_FAILURE;
  }

  /* Extract user's DN from their certificate */
  if( !(fp = fopen(sec->myKeyCertFile, "r")) ){

    fprintf(stderr, "STEERUtils: Failed to open key and cert file >>%s<<\n",
            sec->myKeyCertFile);
    xmlFreeDoc(doc);
    xmlCleanupParser();
    return REG_FAILURE;
  }

  sec->userDN[0] = '\0';
  while( fgets(bufline, 512, fp) ){
    if(strstr(bufline, "subject=")){
      /* Remove trailing new-line character */
      bufline[strlen(bufline)-1] = '\0';
      pChar = strchr(bufline, '=');
      snprintf(sec->userDN, REG_MAX_STRING_LENGTH, "%s", pChar+1);
      break;
    }
  }
  fclose(fp);
#ifdef REG_DEBUG
  printf("Get_security_config: User's DN >>%s<<\n\n", sec->userDN);
#endif

  xmlFreeDoc(doc);
  xmlCleanupParser();

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

void Common_signal_handler(int aSignal){

  /* caught one signal - ignore all others now as going to quit and do not
     want the quit process to be interrupted and restarted... */
  signal(SIGINT, SIG_IGN);
  signal(SIGTERM, SIG_IGN);
  signal(SIGSEGV, SIG_IGN);
  signal(SIGILL, SIG_IGN);
  signal(SIGABRT, SIG_IGN);
  signal(SIGFPE, SIG_IGN);
#ifndef WIN32
  signal(SIGXCPU, SIG_IGN);
  signal(SIGUSR2, SIG_IGN);
#endif

  switch(aSignal){

    case SIGINT:
      fprintf(stderr, "STEER: Steering_signal_handler: Interrupt "
	      "signal received (signal %d)\n", aSignal);
      break;

    case SIGTERM:
      fprintf(stderr, "STEER: Steering_signal_handler: Kill signal "
	      "received (signal %d)\n", aSignal);
      break;

    case SIGSEGV:
      fprintf(stderr, "STEER: Steering_signal_handler: Illegal "
	      "Access caught (signal %d)\n", aSignal);
      break;

    case  SIGILL:
      fprintf(stderr, "STEER: Steering_signal_handler: Illegal "
	      "Exception caught (signal %d)\n", aSignal);
      break;

      /* note: abort called if exception not caught (and hence calls
	 terminate) */
    case SIGABRT:
      fprintf(stderr, "STEER: Steering_signal_handler: Abort "
	      "signal caught (signal %d)\n", aSignal);
      break;

    case SIGFPE:
      fprintf(stderr, "STEER: Steering_signal_handler: Arithmetic "
	      "Exception caught (signal %d)\n", aSignal);
      break;

#ifndef WIN32
    case SIGXCPU:
      fprintf(stderr, "STEER: Steering_signal_handler: CPU usuage "
	      "exceeded (signal %d)\n", aSignal);
      break;

    case SIGUSR2:
      /* This is for the benefit of LSF - it sends us this when we hit
         our wall-clock limit */
      fprintf(stderr, "STEER: Steering_signal_handler: USR2 signal "
	      "caught (signal %d)\n", aSignal);
      break;
#endif

    default:
      fprintf(stderr, "STEER: Steering_signal_handler: Signal caught (signal %d)\n",
              aSignal);
  }

  return;
}


/*----------------------------------------------------------------*/

char *trimWhiteSpace(char *pChar){

  int i;

  if (!pChar) return NULL;

  /* Remove any trailing white space from the supplied character
     array */
  i = strlen(pChar)-1;
  while( (i > -1) && (pChar[i] == ' ') ){
    pChar[i--] = '\0';
  }

  return pChar;
}

/*----------------------------------------------------------------*/

int Delete_iotype_list(struct reg_iotype_list *list) {
  if(!list)
    return REG_FAILURE;

  if(list->iotype)
    free(list->iotype);

  list->iotype = NULL;
  list->numEntries = 0;

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Get_file_list(const char* dirname, int num_tags, char** tags,
		  int* num_files, char*** filenames) {

  DIR* dir;
  char** tmp;
  struct dirent* entry;
  int array_len;
  int name_len;
  int i, j;
  int tag_not_found;

  if((dir = opendir(dirname)) == NULL)
    return REG_FAILURE;

  /* allocate an array of 50 filenames */
  array_len = 50;
  *filenames = (char**) malloc(array_len * sizeof(char*));
  if(*filenames == NULL) {
    closedir(dir);
    return REG_FAILURE;
  }

  i = 0;
  while((entry = readdir(dir)) != NULL) {
    /* search for the tags - they must all be present */
    tag_not_found = 0;
    for(j = 0; j < num_tags; j++) {
      if((strstr(entry->d_name, tags[j])) == NULL) {
	tag_not_found = 1;
	break;
      }
    }
    if(tag_not_found)
      continue;

    /* do we need more space in the filenames array? */
    if(i == array_len) {
      array_len += 10;
      tmp = (char**) realloc((void*)(*filenames),
			     array_len * sizeof(char*));
      if(tmp == NULL) {
	for(j = 0; j < i; j++) {
	  free((*filenames)[j]);
	}
	free(*filenames);
	closedir(dir);
	return REG_FAILURE;
      }

      *filenames = tmp;
    }

    /* allocate the memory required to store the filename */
    name_len = strlen(entry->d_name) + 1;
    (*filenames)[i] = (char*) malloc(name_len * sizeof(char));
    if((*filenames)[i] == NULL) {
      for(j = 0; j < i; j++) {
	free((*filenames)[j]);
      }
      free(*filenames);
      closedir(dir);
      return REG_FAILURE;
    }

    strncpy((*filenames)[i], entry->d_name, name_len);
    i++;
  }

  closedir(dir);

  if(i > 0)
    qsort(*filenames, i, sizeof(**filenames), cmpstrs);

  *num_files = i;

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int cmpstrs(const void* str1, const void* str2) {
  return strcmp(*(char* const*) str1, *(char* const*) str2);
}

/*----------------------------------------------------------------*/
