/*----------------------------------------------------------------------------
  This file contains routines and data structures for file-based
  steering communication and sample transfer.

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
#include <string.h>

#include "ReG_Steer_Appside_File.h"
#include "ReG_Steer_types.h"
#include "ReG_Steer_Appside_internal.h"

/* Allow value of 'REG_DEBUG' to propagate down from Reg_steer_types.h if
   it has been set there */
#ifndef REG_DEBUG
#define REG_DEBUG 1
#endif

/* The table holding details of our communication channel with the
   steering client - defined in ReG_Steer_Appside.c */
extern Steerer_connection_table_type Steerer_connection;

/* The table holding details of the registered IOTypes for this
   application - defined in ReG_Steer_Appside.c */
extern IOdef_table_type IOTypes_table;

/* Global scratch buffer - declared in ReG_Steer_Appside.c */
extern char Global_scratch_buffer[];

/*-----------------------------------------------------------------------*/

int Detach_from_steerer_file(){

  int   nbytes;
  char  filename[REG_MAX_STRING_LENGTH];

  /* Remove lock file that indicates app is being steered */

  nbytes = snprintf(filename, REG_MAX_STRING_LENGTH, "%s%s", 
		    Steerer_connection.file_root, 
		    STR_CONNECTED_FILENAME);

  if(nbytes >= (REG_MAX_STRING_LENGTH-1) || (nbytes < 1)){

    fprintf(stderr, "Detach_from_steerer_file: name of lock-file exceeds %d"
	    " characters - increase REG_MAX_STRING_LENGTH\n", 
	    REG_MAX_STRING_LENGTH);
    return REG_FAILURE;
  }

  remove(filename);

  /* Remove any files that steerer has produced that we won't
     now be consuming */

  nbytes = snprintf(filename, REG_MAX_STRING_LENGTH, "%s%s", 
		    Steerer_connection.file_root, 
		    STR_TO_APP_FILENAME);

  if( nbytes >= (REG_MAX_STRING_LENGTH-1) || (nbytes < 1)){

    fprintf(stderr, "Detach_from_steerer_file: name of steerer ctrl files "
	    "exceeds %d characters - increase REG_MAX_STRING_LENGTH\n", 
	    REG_MAX_STRING_LENGTH);
    return REG_FAILURE;
  }

  Remove_files(filename);
  return REG_SUCCESS;
}

/*--------------------------------------------------------------------*/

int Steerer_connected_file()
{
  char   filename[REG_MAX_STRING_LENGTH];
  FILE  *fp;
  int    nbytes;

  nbytes = snprintf(filename, REG_MAX_STRING_LENGTH, "%s%s", 
		    Steerer_connection.file_root, 
		    STR_CONNECTED_FILENAME);

  if( nbytes >= (REG_MAX_STRING_LENGTH-1) || (nbytes < 1)){

    fprintf(stderr, "Steerer_connected_file: full path name of lockfile "
	    "indicating steerrer connected exceeds %d chars - increase "
	    "REG_MAX_STRING_LENGTH\n", REG_MAX_STRING_LENGTH);
    return REG_FAILURE;
  }

  if( (fp = fopen(filename, "r")) ){

      fclose(fp);
      return REG_SUCCESS;
  }

  return REG_FAILURE;
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

/*-------------------------------------------------------------------*/

int Send_status_msg_file(char *buf)
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

struct msg_struct *Get_control_msg_file()
{
  struct msg_struct   *msg = NULL;
  FILE                *fp;
  char                 filename[REG_MAX_STRING_LENGTH];
  int                  nbytes;

  nbytes = snprintf(filename, REG_MAX_STRING_LENGTH, "%s%s", 
		    Steerer_connection.file_root, 
		    STR_TO_APP_FILENAME);

  if( nbytes >= (REG_MAX_STRING_LENGTH-1) || (nbytes < 1)){

    fprintf(stderr, "Get_control_msg_file: length of ctrl msg filename "
	    "exceeds %d chars - increase REG_MAX_STRING_LENGTH\n", 
	    REG_MAX_STRING_LENGTH);
  }

  if( (fp = Open_next_file(filename)) != NULL){

    fclose(fp);

    msg = New_msg_struct();

    if(Parse_xml_file(filename, msg) != REG_SUCCESS){

      fprintf(stderr, "Get_control_msg_file: failed to parse <%s>\n", filename);
      Delete_msg_struct(msg);
      msg = NULL;
    }

    /* Delete the file once we've read it */
    if( Delete_file(filename) != REG_SUCCESS){

      fprintf(stderr, "Get_control_msg_file: failed to delete %s\n",filename);
    }
  }

  return msg;
}

/*-------------------------------------------------------------------*/

int Finalize_steering_connection_file()
{
  char sys_command[REG_MAX_STRING_LENGTH];

#if REG_DEBUG
  int  max, max1;

  max = strlen(APP_STEERABLE_FILENAME);
  max1 = strlen(STR_CONNECTED_FILENAME);

  if(max1 > max) max=max1;
  
  max += strlen(Steerer_connection.file_root);
  if(max > REG_MAX_STRING_LENGTH ){

    fprintf(stderr, "Finalize_steering_connection_file: WARNING: truncating "
	    "filename\n");
  }
#endif

  /* Delete the lock file that indicates we are steerable */
  sprintf(sys_command, "%s%s", Steerer_connection.file_root,
	  APP_STEERABLE_FILENAME);
  if(remove(sys_command)){

    fprintf(stderr, "Finalize_steering_connection_file: failed to remove "
	    "%s\n", sys_command);
  }

  /* Delete the lock file that indicates we are being steered */
  sprintf(sys_command, "%s%s", Steerer_connection.file_root,
	  STR_CONNECTED_FILENAME);
  if(remove(sys_command)){
#if REG_DEBUG    
    fprintf(stderr, "Finalize_steering_connection_file: failed to remove "
	    "%s\n", sys_command);
#endif
  }

  /* Delete any files we'd have consumed if we'd lived longer */
  sprintf(sys_command, "%s%s", Steerer_connection.file_root, 
	  STR_TO_APP_FILENAME);
  Remove_files(sys_command);

  return REG_SUCCESS;
}

/*-------------------------------------------------------------------*/

int Initialize_steering_connection_file(int  NumSupportedCmds,
					int *SupportedCmds)
{
  FILE *fp;
  char  buf[REG_MAX_MSG_SIZE];
  char  filename[REG_MAX_STRING_LENGTH];

  /* Set location of all comms files */
  if(Set_steering_directory() != REG_SUCCESS){

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

      fprintf(stderr, "Initialize_steering_connection_file: failed to "
	      "remove %s\n",filename);
    }
#if REG_DEBUG
    else{
      fprintf(stderr, "Initialize_steering_connection_file: removed "
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

    fprintf(stderr, "Initialize_steering_connection_file: failed to open %s\n",
	    filename);
    return REG_FAILURE;
  }

#if REG_DEBUG
  fprintf(stderr, "Initialize_steering_connection_file: writing file: %s\n", 
	  filename);
#endif

  Make_supp_cmds_msg(NumSupportedCmds, SupportedCmds, buf);

  fprintf(fp, "%s", buf);
  fclose(fp);

  return REG_SUCCESS;
}

/*-----------------------------------------------------------------------*/

int Get_data_source_address_file(int                 dummy,
				 char               *hostname,
				 unsigned short int *port)
{
  char *pchar;
  int   len;

  /* Return port = 0 on failure */
  *port = 0;

  /* Get hostname and port from environment variables */

  pchar = getenv("REG_CONNECTOR_HOSTNAME");
  if (pchar) {
    len = strlen(pchar);
    if (len < REG_MAX_STRING_LENGTH) {
      sprintf(hostname, pchar);
    }
    else{
      fprintf(stderr, "Get_data_source_address_file: content of "
	      "REG_CONNECTOR_HOSTNAME exceeds max. string length of "
	      "%d chars\n", REG_MAX_STRING_LENGTH);
      return REG_FAILURE;
    }
  }
  else{
    fprintf(stderr, 
	    "Get_data_source_address_file: REG_CONNECTOR_HOSTNAME not set\n");
    return REG_FAILURE;
  }

  pchar = getenv("REG_CONNECTOR_PORT");
  if (pchar) {
    *port =  (unsigned short int)atoi(pchar);
  }
  else{
    fprintf(stderr, 
	    "Get_data_source_address_file: REG_CONNECTOR_PORT not set\n");
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*-----------------------------------------------------------------------*/

int Consume_start_data_check_file(int index){

  int    i;
  int    nfiles;
  char  *pchar;
  char** filenames;
  char   fileroot[REG_MAX_STRING_LENGTH];

  /* In the short term, use the label (with spaces replaced by
     '_'s) as the filename */
  sprintf(fileroot, "%s%s", IOTypes_table.io_def[index].directory,
	  IOTypes_table.io_def[index].label);

  /* Remove trailing white space */
  i = strlen(fileroot);
  while(fileroot[--i] == ' ');

  /* Terminate string and correct length of string (since final
     character we looked at wasn't actually blank) */
  fileroot[++i] = '\0';

  /* Replace any spaces with '_' */
  pchar = (char*) strchr(fileroot, ' ');
  while( pchar && ((pchar - fileroot + 1) < i) ){
    *pchar = '_';
    pchar = (char*) strchr(++pchar,' ');
  }

  strcat(fileroot, "_*.lock");

  filenames = NULL;
  if(Get_file_list(fileroot, &nfiles, &filenames) != REG_SUCCESS ||
     nfiles == 0){
    return REG_FAILURE;
  }

  strcpy(IOTypes_table.io_def[index].filename, filenames[0]);

  for(i=0; i<nfiles; i++){
    free(filenames[i]);
  }
  free(filenames);

  /* Remove the lock file to take ownership of the data file */
  remove(IOTypes_table.io_def[index].filename);

  /* Remove the '.lock' from the filename */
  pchar = (char*) strstr(IOTypes_table.io_def[index].filename, ".lock");

  if(!pchar){
    fprintf(stderr, "Consume_start_data_check_file: failed to strip .lock!\n");
    return REG_FAILURE;
  }

  *pchar = '\0';
  if( !(IOTypes_table.io_def[index].fp = 
	fopen(IOTypes_table.io_def[index].filename, "r")) ){

    fprintf(stderr, "Consume_start_data_check_file: failed to open file: %s\n",
	    IOTypes_table.io_def[index].filename);
    return REG_FAILURE;
  }

  /* Use fileroot buffer as is plenty big enough for the small header
     we want to read here */
  if(fread((void *)fileroot, 
	   (size_t)1, 
	   REG_PACKET_SIZE, 
	   IOTypes_table.io_def[index].fp) != (size_t)REG_PACKET_SIZE){

    fprintf(stderr, "Consume_start_data_check_file: failed to read "
	    "header from file: %s\n",
	    IOTypes_table.io_def[index].filename);
    remove(IOTypes_table.io_def[index].filename);
    return REG_FAILURE;
  }

  if(!strstr(fileroot, REG_DATA_HEADER)){

    fprintf(stderr, "Consume_start_data_check_file: wrong "
	    "header from file: %s\n",
	    IOTypes_table.io_def[index].filename);
    remove(IOTypes_table.io_def[index].filename);
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Get_file_list(char *fileroot,
		  int  *num,
		  char ***names)
{
  char *redirection = " > ReG_files.tmp";
  char *pchar;
  char  bufline[REG_MAX_STRING_LENGTH];
  int   len;
  int   i, j;
  FILE *fp;

  *num = 0;

  /* Calc. length of string - 'ls -1' and slashes add 9 chars so
     add a few more for safety.  Ask for 2*strlen(ChkTag) so that
     we can use the end of this buffer to hold the trimmed version
     of the tag. */
  len = strlen(fileroot) + strlen(redirection) + 20;

  if( !(pchar = (char *)malloc(len)) ){

    fprintf(stderr, "Get_file_list: malloc of %d bytes failed\n",
	    len);
    return REG_FAILURE;
  }

  sprintf(pchar, "ls -1 %s %s", fileroot, redirection);
  system(pchar);

  free(pchar);
  pchar = NULL;

  if( (fp = fopen("ReG_files.tmp", "r")) ){

    while(fgets(bufline, REG_MAX_STRING_LENGTH, fp)){
      (*num)++;
    }

    if(*num == 0){
      remove("ReG_files.tmp");
      return REG_FAILURE;
    }

    *names = (char **)malloc(*num * sizeof(char*));
    rewind(fp);

    for(i=0; i<*num; i++){

      fgets(bufline, REG_MAX_STRING_LENGTH, fp);
      /* fgets includes '\n' in the returned buffer */
      len = (int)strlen(bufline);
      if(!((*names)[i] = (char *)malloc(len))){

	fprintf(stderr, "Get_checkpoint_files: malloc failed\n");
	for(j=i; j>=0; j--){
	  free((*names)[i]);
	  (*names)[i] = NULL;
	}
	free(*names);
	*names = NULL;
	remove("ReG_files.tmp");
	return REG_FAILURE;
      }
      memcpy((*names)[i], bufline, len);
      /* Terminate string - overwrite '\n' */
      (*names)[i][len-1] = '\0';
    }

    fclose(fp);
    remove("ReG_files.tmp");
  }

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Consume_data_read_file(const int	 index,  
			   const int	 datatype,
			   const size_t	 num_bytes_to_read, 
			   void		*pData)
{  
  size_t nbytes;

  if(!IOTypes_table.io_def[index].fp){

    fprintf(stderr, "Consume_data_read_file: ERROR: null file pointer\n");
    return REG_FAILURE;
  }

  if(IOTypes_table.io_def[index].use_xdr ||
     IOTypes_table.io_def[index].convert_array_order == TRUE){

    nbytes = fread((void *)IOTypes_table.io_def[index].buffer,
		   1,
		   (size_t)num_bytes_to_read, 
		   IOTypes_table.io_def[index].fp);
  }
  else{
    nbytes = fread(pData,
		   1,
		   (size_t)num_bytes_to_read, 
		   IOTypes_table.io_def[index].fp);
  }
#if REG_DEBUG
  fprintf(stderr, "Consume_data_read_file: read %d bytes\n",
	  (int) nbytes);

  if(datatype == REG_CHAR){
    fprintf(stderr, "Consume_data_read_file: got char data:\n>>%s<<\n", 
	    (char *)pData);
  }
#endif /* REG_DEBUG */

  if((int)nbytes != num_bytes_to_read){

    fprintf(stderr, "Consume_data_read_file: failed to read expected "
	    "quantity of data\n");
    /* Reset use_xdr flag set as only valid on a per-slice basis */
    IOTypes_table.io_def[index].use_xdr = FALSE;

    fclose(IOTypes_table.io_def[index].fp);
    IOTypes_table.io_def[index].fp = NULL;
    remove(IOTypes_table.io_def[index].filename);
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Emit_data_file(const int	 index,
		   const size_t	 num_bytes_to_send,
		   void		*pData)
{
  int n_written;

  if(IOTypes_table.io_def[index].fp){
    n_written = (int)fwrite( pData, (int)num_bytes_to_send, 1, 
			     IOTypes_table.io_def[index].fp);

    if(n_written == 1)return REG_SUCCESS;
  }
  return REG_FAILURE;
}

/*----------------------------------------------------------------*/

int Consume_msg_header_file(int  index,
			    int *DataType,
			    int *Count,
			    int *NumBytes,
			    int *IsFortranArray)
{
  char   buffer[REG_PACKET_SIZE];

  if(!IOTypes_table.io_def[index].fp){

    fprintf(stderr, "Consume_iotype_msg_header: file pointer is null\n");
    return REG_FAILURE;
  }

  if(fread(buffer, 1, REG_PACKET_SIZE, IOTypes_table.io_def[index].fp) 
     != (size_t)REG_PACKET_SIZE){

    fprintf(stderr, "Consume_iotype_msg_header: fread failed for header\n");
    fclose(IOTypes_table.io_def[index].fp);
    IOTypes_table.io_def[index].fp = NULL;
    remove(IOTypes_table.io_def[index].filename);
    return REG_FAILURE;
  }

#if REG_DEBUG
  fprintf(stderr, "Consume_iotype_msg_header: read >%s< from file\n",
	  buffer);
#endif

  /* Check for end of data */
  if(!strncmp(buffer, REG_DATA_FOOTER, strlen(REG_DATA_FOOTER))){

    return REG_EOD;
  }
  else if(strncmp(buffer, BEGIN_SLICE_HEADER, strlen(BEGIN_SLICE_HEADER))){

    fprintf(stderr, "Consume_iotype_msg_header: incorrect header on slice\n");
    fclose(IOTypes_table.io_def[index].fp);
    IOTypes_table.io_def[index].fp = NULL;
    remove(IOTypes_table.io_def[index].filename);
    return REG_FAILURE;
  }

  /*--- Type of objects in message ---*/

  if(fread(buffer, 1, REG_PACKET_SIZE, IOTypes_table.io_def[index].fp) 
     != (size_t)REG_PACKET_SIZE){

    fprintf(stderr, "Consume_iotype_msg_header: fread failed for object type\n");
    fclose(IOTypes_table.io_def[index].fp);
    IOTypes_table.io_def[index].fp = NULL;
    remove(IOTypes_table.io_def[index].filename);
    return REG_FAILURE;
  }

#if REG_DEBUG
  fprintf(stderr, "Consume_iotype_msg_header: read >%s< from file\n", 
	  buffer);
#endif

  if(!strstr(buffer, "<Data_type>")){
    fclose(IOTypes_table.io_def[index].fp);
    IOTypes_table.io_def[index].fp = NULL;
    remove(IOTypes_table.io_def[index].filename);
    return REG_FAILURE;
  }

  sscanf(buffer, "<Data_type>%d</Data_type>", DataType);

  /*--- No. of objects in message ---*/

  if(fread(buffer, 1, REG_PACKET_SIZE, IOTypes_table.io_def[index].fp) 
     != (size_t)REG_PACKET_SIZE){

    fclose(IOTypes_table.io_def[index].fp);
    IOTypes_table.io_def[index].fp = NULL;
    remove(IOTypes_table.io_def[index].filename);
    return REG_FAILURE;
  }

#if REG_DEBUG
  fprintf(stderr, "Consume_iotype_msg_header: read >%s< from file\n", 
	  buffer);
#endif

  if(!strstr(buffer, "<Num_objects>")){

    fclose(IOTypes_table.io_def[index].fp);
    IOTypes_table.io_def[index].fp = NULL;
    remove(IOTypes_table.io_def[index].filename);
    return REG_FAILURE;
  }

  if( sscanf(buffer, "<Num_objects>%d</Num_objects>", Count) != 1){

    fprintf(stderr, "Consume_iotype_msg_header: failed to read Num_objects\n");
    fclose(IOTypes_table.io_def[index].fp);
    IOTypes_table.io_def[index].fp = NULL;
    remove(IOTypes_table.io_def[index].filename);
    return REG_FAILURE;
  }

  /*--- No. of bytes in message ---*/

  if(fread(buffer, 1, REG_PACKET_SIZE, IOTypes_table.io_def[index].fp) 
     != (size_t)REG_PACKET_SIZE){

    fprintf(stderr, "Consume_iotype_msg_header: fread failed for num bytes\n");
    fclose(IOTypes_table.io_def[index].fp);
    IOTypes_table.io_def[index].fp = NULL;
    remove(IOTypes_table.io_def[index].filename);
    return REG_FAILURE;
  }

#if REG_DEBUG
  fprintf(stderr, "Consume_iotype_msg_header: read >%s< from file\n", 
	  buffer);
#endif

  if(!strstr(buffer, "<Num_bytes>")){

    fclose(IOTypes_table.io_def[index].fp);
    IOTypes_table.io_def[index].fp = NULL;
    remove(IOTypes_table.io_def[index].filename);
    return REG_FAILURE;
  }

  if( sscanf(buffer, "<Num_bytes>%d</Num_bytes>", NumBytes) != 1){

    fprintf(stderr, "Consume_iotype_msg_header: failed to read Num_bytes\n");
    fclose(IOTypes_table.io_def[index].fp);
    IOTypes_table.io_def[index].fp = NULL;
    remove(IOTypes_table.io_def[index].filename);
    return REG_FAILURE;
  }

  /*--- Array ordering in message ---*/

  if(fread(buffer, 1, REG_PACKET_SIZE, IOTypes_table.io_def[index].fp) 
     != (size_t)REG_PACKET_SIZE){

    fprintf(stderr, "Consume_iotype_msg_header: fread failed for array ordering\n");
    fclose(IOTypes_table.io_def[index].fp);
    IOTypes_table.io_def[index].fp = NULL;
    remove(IOTypes_table.io_def[index].filename);
    return REG_FAILURE;
  }

#if REG_DEBUG
  fprintf(stderr, "Consume_iotype_msg_header: read >%s< from file\n", 
	  buffer);
#endif

  if(!strstr(buffer, "<Array_order>")){

    fclose(IOTypes_table.io_def[index].fp);
    IOTypes_table.io_def[index].fp = NULL;
    remove(IOTypes_table.io_def[index].filename);
    return REG_FAILURE;
  }

  if(strstr(buffer, "FORTRAN")){

    /* Array data is from Fortran */
    *IsFortranArray = TRUE;
  }
  else{
    /* Array data is not from Fortran */
    *IsFortranArray = FALSE;
  }

  /*--- End of header ---*/

  if(fread(buffer, 1, REG_PACKET_SIZE, IOTypes_table.io_def[index].fp) 
     != (size_t)REG_PACKET_SIZE){

    fprintf(stderr, "Consume_iotype_msg_header: fread failed for header end\n");
    fclose(IOTypes_table.io_def[index].fp);
    IOTypes_table.io_def[index].fp = NULL;
    remove(IOTypes_table.io_def[index].filename);
    return REG_FAILURE;
  }

#if REG_DEBUG
  fprintf(stderr, "Consume_msg_header: read >%s< from file\n", 
	  buffer);
#endif

  if(strncmp(buffer, END_SLICE_HEADER, strlen(END_SLICE_HEADER))){

    fprintf(stderr, "Consume_msg_header: failed to find "
	    "end of header\n");
    fclose(IOTypes_table.io_def[index].fp);
    IOTypes_table.io_def[index].fp = NULL;
    remove(IOTypes_table.io_def[index].filename);
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Initialize_IOType_transport_file(int direction, 
				     int index)
{
  char *pchar;
  int   len;

  /* This is only simple - we use the same directory for every
     IOType, irrespective of label or direction (input/output). */
  if(pchar = getenv("REG_DATA_DIRECTORY")){

    len = strlen(pchar);
    if(len > REG_MAX_STRING_LENGTH){

      fprintf(stderr, "Initialize_IOType_transport_file: content of "
	      "REG_DATA_DIRECTORY env. variable exceeds %d\n"
	      "characters - increase REG_MAX_STRING_LENGTH\n", 
	      REG_MAX_STRING_LENGTH);
      return REG_FAILURE;
    }

   /* Check that path ends in '/' - if not then add one */
    if(pchar[len-1] != '/'){

      sprintf(IOTypes_table.io_def[index].directory, "%s/", pchar);
    }
    else{
      strcpy(IOTypes_table.io_def[index].directory, pchar);
    }
#if REG_DEBUG
    fprintf(stderr, "Initialize_IOType_transport_file: will use following"
	    " directory for data files: %s\n", 
	    IOTypes_table.io_def[index].directory);
#endif    
  }
  else{
    IOTypes_table.io_def[index].directory[0] = '\0';
#if REG_DEBUG
    fprintf(stderr, "Initialize_IOType_transport_file: will use current"
	    " working directory for data files\n");
#endif    
  }

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Consume_ack_file(const int index)
{
  FILE*  fp;

  if(IOTypes_table.io_def[index].ack_needed == FALSE)return REG_SUCCESS;

  /* In the short term, use the label (with spaces replaced by
     '_'s) as the filename.  This routine is called before filename
     for next sample is generated so 'filename' contains the index
     of the last one we emitted. i.e. we look for an acknowledgement
     of a specific sample rather than just any sample. */
  sprintf(Global_scratch_buffer, "%s_ACK", 
	  IOTypes_table.io_def[index].filename);

  if(fp = fopen(Global_scratch_buffer, "r")){

    fclose(fp);
    remove(Global_scratch_buffer);

    return REG_SUCCESS;
  }

  return REG_FAILURE;
}

/*----------------------------------------------------------------*/

int Emit_ack_file(const int index)
{
  FILE*  fp;

  /* In the short term, use the label (with spaces replaced by
     '_'s) as the filename.  'filename' is set in 
     Consume_start_data_check and contains an index so this ack
     is unique to the data set we've just read. */
  sprintf(Global_scratch_buffer, "%s_ACK", 
	  IOTypes_table.io_def[index].filename);
  
  if(fp = fopen(Global_scratch_buffer, "w")){

    fclose(fp);
    return REG_SUCCESS;
  }

  return REG_FAILURE;
}
