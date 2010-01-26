/*
  The RealityGrid Steering Library

  Copyright (c) 2002-2009, University of Manchester, United Kingdom.
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

  Author: Robert Haines
 */

/** @internal
    @file ReG_Steer_Samples_Transport_Files.c
    @brief Source file for file-based samples transport.
    @author Robert Haines
  */

#include "ReG_Steer_Config.h"
#include "ReG_Steer_types.h"
#include "ReG_Steer_Samples_Transport_API.h"
#include "ReG_Steer_Files_Common.h"
#include "ReG_Steer_Common.h"
#include "ReG_Steer_Appside_internal.h"

/** Basic library config - declared in ReG_Steer_Common */
extern Steer_lib_config_type Steer_lib_config;

/* */
extern file_info_table_type file_info_table;

/* Need access to these tables which are actually declared in 
   ReG_Steer_Appside_internal.h */
extern IOdef_table_type IOTypes_table;

/** Global scratch buffer - declared in ReG_Steer_Appside.c */
extern char Global_scratch_buffer[];

/*---------------------------------------------------*/

int Initialize_samples_transport() {
  strncpy(Steer_lib_config.Samples_transport_string, "Files", 6);

  return file_info_table_init(IOTypes_table.max_entries);
}

/*---------------------------------------------------*/

int Finalize_samples_transport() {
  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Emit_start_impl(int index, int seqnum) {
  char *pchar;
  int   len;

  /* Currently have no way of looking up what filename to use so 
     hardwire... */

  len = strlen(file_info_table.file_info[index].directory) +
    strlen(IOTypes_table.io_def[index].label);

  if(len > REG_MAX_STRING_LENGTH){

    fprintf(stderr, "STEER: Emit_start: combination of filename + "
	    "directory path exceeds %d characters: increase "
	    "REG_MAX_STRING_LENGTH\n", REG_MAX_STRING_LENGTH);
    return REG_FAILURE;
  }

  /* In the short term, use the label as the filename */
  sprintf(file_info_table.file_info[index].filename, 
	  "%s%s", file_info_table.file_info[index].directory,
	  IOTypes_table.io_def[index].label);

  /* Remove trailing white space */
  trimWhiteSpace(file_info_table.file_info[index].filename);
  len = strlen(file_info_table.file_info[index].filename);

  /* Replace any spaces with '_' */
  pchar = strchr(file_info_table.file_info[index].filename, ' ');
  while( pchar && ((pchar - file_info_table.file_info[index].filename + 1) < len) ){
    *pchar = '_';
    pchar = strchr(++pchar,' ');
  }

  pchar = file_info_table.file_info[index].filename;
  pchar += strlen(file_info_table.file_info[index].filename);

  sprintf(pchar, "_%d", seqnum);
  if( !(file_info_table.file_info[index].fp = 
	fopen(file_info_table.file_info[index].filename, "w")) ){

    fprintf(stderr, "STEER: Emit_start: failed to open file %s\n", 
	    file_info_table.file_info[index].filename);
    return REG_FAILURE;
  }
  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Emit_stop_impl(int index) {
  if(file_info_table.file_info[index].fp){
    fclose(file_info_table.file_info[index].fp);
    file_info_table.file_info[index].fp = NULL;
  }
  /* Create lock file for this data file to prevent race 
     conditions */
  create_lock_file(file_info_table.file_info[index].filename);

  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Consume_stop_impl(int index) {
  /* Close any file associated with this channel */
  if(file_info_table.file_info[index].fp){
    fclose(file_info_table.file_info[index].fp);
    file_info_table.file_info[index].fp = NULL;
    remove(file_info_table.file_info[index].filename);
  }

  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Initialize_IOType_transport_impl(int direction, int index) {
  char *pchar;
  int   len;

  /* This is only simple - we use the same directory for every
     IOType, irrespective of label or direction (input/output). */
  if((pchar = getenv("REG_DATA_DIRECTORY"))) {

    len = strlen(pchar);
    if(len > REG_MAX_STRING_LENGTH) {
      fprintf(stderr, "STEER: Initialize_IOType_transport_file: content of "
	      "REG_DATA_DIRECTORY env. variable exceeds %d\n"
	      "characters - increase REG_MAX_STRING_LENGTH\n", 
	      REG_MAX_STRING_LENGTH);
      return REG_FAILURE;
    }

   /* Check that path ends in '/' - if not then add one */
    if(pchar[len-1] != '/') {

      sprintf(file_info_table.file_info[index].directory, "%s/", pchar);
    }
    else {
      strcpy(file_info_table.file_info[index].directory, pchar);
    }
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: Initialize_IOType_transport_file: will use following"
	    " directory for data files: %s\n", 
	    file_info_table.file_info[index].directory);
#endif    
  }
  else {
    file_info_table.file_info[index].directory[0] = '\0';
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: Initialize_IOType_transport_file: will use current"
	    " working directory for data files\n");
#endif    
  }

  return REG_SUCCESS;
}

/*---------------------------------------------------*/

void Finalize_IOType_transport_impl() {}

/*----------------------------------------------------------------*/

int Consume_data_read_impl(const int index,  
			   const int datatype,
			   const int num_bytes_to_read, 
			   void*     pData) {  
  size_t nbytes;

  if(!file_info_table.file_info[index].fp) {

    fprintf(stderr, "STEER: ERROR: Consume_data_read_file: null file pointer\n");
    return REG_FAILURE;
  }

  if(IOTypes_table.io_def[index].use_xdr ||
     IOTypes_table.io_def[index].convert_array_order == REG_TRUE) {

    nbytes = fread((void *)IOTypes_table.io_def[index].buffer,
		   1,
		   (size_t)num_bytes_to_read, 
		   file_info_table.file_info[index].fp);
  }
  else {
    nbytes = fread(pData,
		   1,
		   (size_t)num_bytes_to_read, 
		   file_info_table.file_info[index].fp);
  }
#ifdef REG_DEBUG
  fprintf(stderr, "STEER: Consume_data_read_file: read %d bytes\n",
	  (int) nbytes);
#endif /* REG_DEBUG */

  if((int)nbytes != num_bytes_to_read) {

    fprintf(stderr, "STEER: Consume_data_read_file: failed to read expected "
	    "quantity of data\n");
    /* Reset use_xdr flag set as only valid on a per-slice basis */
    IOTypes_table.io_def[index].use_xdr = REG_FALSE;

    fclose(file_info_table.file_info[index].fp);
    file_info_table.file_info[index].fp = NULL;
    remove(file_info_table.file_info[index].filename);
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Emit_ack_impl(const int index) {
  FILE*  fp;

  /* In the short term, use the label (with spaces replaced by
     '_'s) as the filename.  'filename' is set in 
     Consume_start_data_check and contains an index so this ack
     is unique to the data set we've just read. */
  sprintf(Global_scratch_buffer, "%s_ACK", 
	  file_info_table.file_info[index].filename);
  
  if((fp = fopen(Global_scratch_buffer, "w"))) {
    fclose(fp);
    return REG_SUCCESS;
  }

  return REG_FAILURE;
}

/*----------------------------------------------------------------*/

int Consume_ack_impl(const int index) {
  FILE*  fp;

  if(IOTypes_table.io_def[index].ack_needed == REG_FALSE)
    return REG_SUCCESS;

  /* In the short term, use the label (with spaces replaced by
     '_'s) as the filename.  This routine is called before filename
     for next sample is generated so 'filename' contains the index
     of the last one we emitted. i.e. we look for an acknowledgement
     of a specific sample rather than just any sample. */
  sprintf(Global_scratch_buffer, "%s_ACK", 
	  file_info_table.file_info[index].filename);

  if((fp = fopen(Global_scratch_buffer, "r"))) {
    fclose(fp);
    remove(Global_scratch_buffer);

    return REG_SUCCESS;
  }

  return REG_FAILURE;
}

/*---------------------------------------------------*/

int Emit_header_impl(const int index) {
  sprintf(Global_scratch_buffer, REG_PACKET_FORMAT, REG_DATA_HEADER);
  Global_scratch_buffer[REG_PACKET_SIZE-1] = '\0';
#ifdef REG_DEBUG
  fprintf(stderr, "STEER: Emit_header: Sending >>%s<<\n", Global_scratch_buffer);
#endif

  return Emit_data_impl(index, REG_PACKET_SIZE,
			(void*)Global_scratch_buffer);

}

/*---------------------------------------------------*/

int Emit_data_impl(const int	index,
		   const size_t	num_bytes_to_send,
		   void*        pData) {
  int n_written;

  if(file_info_table.file_info[index].fp) {
    n_written = (int) fwrite(pData, (int)num_bytes_to_send, 1, 
			     file_info_table.file_info[index].fp);

    if(n_written == 1)
      return REG_SUCCESS;
  }
  return REG_FAILURE;
}

/*---------------------------------------------------*/

int Get_communication_status_impl(const int index) {
  if(file_info_table.file_info[index].fp) {
    return REG_SUCCESS;
  }
  else {
    return REG_FAILURE;
  }
}

/*----------------------------------------------------------------*/

int Emit_msg_header_impl(const int    index,
			 const size_t num_bytes_to_send,
			 void*        pData) {

  if(fwrite(pData, sizeof(char), num_bytes_to_send,
	    file_info_table.file_info[index].fp) == num_bytes_to_send) {
    return REG_SUCCESS;
  }
  return REG_FAILURE;
}

/*----------------------------------------------------------------*/

int Consume_msg_header_impl(int  index,
			    int* DataType,
			    int* Count,
			    int* NumBytes,
			    int* IsFortranArray) {
  char buffer[REG_PACKET_SIZE];

  if(!file_info_table.file_info[index].fp) {
    fprintf(stderr, "STEER: Consume_iotype_msg_header: file pointer is null\n");
    return REG_FAILURE;
  }

  if(fread(buffer, 1, REG_PACKET_SIZE, file_info_table.file_info[index].fp) 
     != (size_t)REG_PACKET_SIZE) {

    fprintf(stderr, "STEER: Consume_iotype_msg_header: fread failed for header\n");
    fclose(file_info_table.file_info[index].fp);
    file_info_table.file_info[index].fp = NULL;
    remove(file_info_table.file_info[index].filename);
    return REG_FAILURE;
  }

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: Consume_iotype_msg_header: read >%s< from file\n",
	  buffer);
#endif

  /* Check for end of data */
  if(!strncmp(buffer, REG_DATA_FOOTER, strlen(REG_DATA_FOOTER))) {

    return REG_EOD;
  }
  else if(strncmp(buffer, BEGIN_SLICE_HEADER, strlen(BEGIN_SLICE_HEADER))) {

    fprintf(stderr, "STEER: Consume_iotype_msg_header: incorrect header on slice\n");
    fclose(file_info_table.file_info[index].fp);
    file_info_table.file_info[index].fp = NULL;
    remove(file_info_table.file_info[index].filename);
    return REG_FAILURE;
  }

  /*--- Type of objects in message ---*/

  if(fread(buffer, 1, REG_PACKET_SIZE, file_info_table.file_info[index].fp) 
     != (size_t)REG_PACKET_SIZE) {

    fprintf(stderr, "STEER: Consume_iotype_msg_header: fread failed for object type\n");
    fclose(file_info_table.file_info[index].fp);
    file_info_table.file_info[index].fp = NULL;
    remove(file_info_table.file_info[index].filename);
    return REG_FAILURE;
  }

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: Consume_iotype_msg_header: read >%s< from file\n", 
	  buffer);
#endif

  if(!strstr(buffer, "<Data_type>")) {
    fclose(file_info_table.file_info[index].fp);
    file_info_table.file_info[index].fp = NULL;
    remove(file_info_table.file_info[index].filename);
    return REG_FAILURE;
  }

  sscanf(buffer, "<Data_type>%d</Data_type>", DataType);

  /*--- No. of objects in message ---*/

  if(fread(buffer, 1, REG_PACKET_SIZE, file_info_table.file_info[index].fp) 
     != (size_t)REG_PACKET_SIZE) {

    fclose(file_info_table.file_info[index].fp);
    file_info_table.file_info[index].fp = NULL;
    remove(file_info_table.file_info[index].filename);
    return REG_FAILURE;
  }

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: Consume_iotype_msg_header: read >%s< from file\n", 
	  buffer);
#endif

  if(!strstr(buffer, "<Num_objects>")) {
    fclose(file_info_table.file_info[index].fp);
    file_info_table.file_info[index].fp = NULL;
    remove(file_info_table.file_info[index].filename);
    return REG_FAILURE;
  }

  if(sscanf(buffer, "<Num_objects>%d</Num_objects>", Count) != 1) {
    fprintf(stderr, "STEER: Consume_iotype_msg_header: failed to read Num_objects\n");
    fclose(file_info_table.file_info[index].fp);
    file_info_table.file_info[index].fp = NULL;
    remove(file_info_table.file_info[index].filename);
    return REG_FAILURE;
  }

  /*--- No. of bytes in message ---*/

  if(fread(buffer, 1, REG_PACKET_SIZE, file_info_table.file_info[index].fp) 
     != (size_t)REG_PACKET_SIZE) {

    fprintf(stderr, "STEER: Consume_iotype_msg_header: fread failed for num bytes\n");
    fclose(file_info_table.file_info[index].fp);
    file_info_table.file_info[index].fp = NULL;
    remove(file_info_table.file_info[index].filename);
    return REG_FAILURE;
  }

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: Consume_iotype_msg_header: read >%s< from file\n", 
	  buffer);
#endif

  if(!strstr(buffer, "<Num_bytes>")) {
    fclose(file_info_table.file_info[index].fp);
    file_info_table.file_info[index].fp = NULL;
    remove(file_info_table.file_info[index].filename);
    return REG_FAILURE;
  }

  if(sscanf(buffer, "<Num_bytes>%d</Num_bytes>", NumBytes) != 1) {
    fprintf(stderr, "STEER: Consume_iotype_msg_header: failed to read Num_bytes\n");
    fclose(file_info_table.file_info[index].fp);
    file_info_table.file_info[index].fp = NULL;
    remove(file_info_table.file_info[index].filename);
    return REG_FAILURE;
  }

  /*--- Array ordering in message ---*/

  if(fread(buffer, 1, REG_PACKET_SIZE, file_info_table.file_info[index].fp) 
     != (size_t)REG_PACKET_SIZE) {

    fprintf(stderr, "STEER: Consume_iotype_msg_header: fread failed for array ordering\n");
    fclose(file_info_table.file_info[index].fp);
    file_info_table.file_info[index].fp = NULL;
    remove(file_info_table.file_info[index].filename);
    return REG_FAILURE;
  }

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: Consume_iotype_msg_header: read >%s< from file\n", 
	  buffer);
#endif

  if(!strstr(buffer, "<Array_order>")) {
    fclose(file_info_table.file_info[index].fp);
    file_info_table.file_info[index].fp = NULL;
    remove(file_info_table.file_info[index].filename);
    return REG_FAILURE;
  }

  if(strstr(buffer, "FORTRAN")) {
    /* Array data is from Fortran */
    *IsFortranArray = REG_TRUE;
  }
  else {
    /* Array data is not from Fortran */
    *IsFortranArray = REG_FALSE;
  }

  /*--- End of header ---*/

  if(fread(buffer, 1, REG_PACKET_SIZE, file_info_table.file_info[index].fp) 
     != (size_t)REG_PACKET_SIZE) {

    fprintf(stderr, "STEER: Consume_iotype_msg_header: fread failed for header end\n");
    fclose(file_info_table.file_info[index].fp);
    file_info_table.file_info[index].fp = NULL;
    remove(file_info_table.file_info[index].filename);
    return REG_FAILURE;
  }

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: Consume_msg_header: read >%s< from file\n", 
	  buffer);
#endif

  if(strncmp(buffer, END_SLICE_HEADER, strlen(END_SLICE_HEADER))) {
    fprintf(stderr, "STEER: Consume_msg_header: failed to find "
	    "end of header\n");
    fclose(file_info_table.file_info[index].fp);
    file_info_table.file_info[index].fp = NULL;
    remove(file_info_table.file_info[index].filename);
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Enable_IOType_impl(const int index) {
  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Disable_IOType_impl(const int index) {
  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Get_IOType_address_impl(int index, char** pbuf, int* bytes_left) {
  return REG_SUCCESS;
}

/*---------------------------------------------------*/

int Consume_start_data_check_impl(int index) {

  int    i;
  int    nfiles;
  char  *pchar;
  char** filenames;
  char buffer[REG_MAX_STRING_LENGTH];
  char* tags[2];

  /* In the short term, use the label (with spaces replaced by
     '_'s) as the filename */
  tags[0] = (char*) malloc(strlen(IOTypes_table.io_def[index].label) * sizeof(char));
  strcpy(tags[0], IOTypes_table.io_def[index].label);

  /* Remove trailing white space */
  i = strlen(tags[0]);
  while(tags[0][--i] == ' ');

  /* Terminate string and correct length of string (since final
     character we looked at wasn't actually blank) */
  tags[0][++i] = '\0';

  /* Replace any spaces with '_' */
  pchar = (char*) strchr(tags[0], ' ');
  while( pchar && ((pchar - tags[0] + 1) < i) ) {
    *pchar = '_';
    pchar = (char*) strchr(++pchar,' ');
  }

  tags[1] = ".lock";

  filenames = NULL;
  if(Get_file_list(file_info_table.file_info[index].directory, 2, tags,
		   &nfiles, &filenames) != REG_SUCCESS || nfiles == 0) {
    return REG_FAILURE;
  }

  sprintf(file_info_table.file_info[index].filename, "%s%s",
	  file_info_table.file_info[index].directory, filenames[0]);

  for(i=0; i<nfiles; i++){
    free(filenames[i]);
  }
  free(filenames);
  free(tags[0]);

  /* Remove the lock file to take ownership of the data file */
  remove(file_info_table.file_info[index].filename);

  /* Remove the '.lock' from the filename */
  pchar = (char*) strstr(file_info_table.file_info[index].filename, ".lock");

  if(!pchar) {
    fprintf(stderr, "STEER: Consume_start_data_check_file: failed to strip .lock!\n");
    return REG_FAILURE;
  }

  *pchar = '\0';
  if(!(file_info_table.file_info[index].fp = 
	fopen(file_info_table.file_info[index].filename, "r"))) {

    fprintf(stderr, "STEER: Consume_start_data_check_file: failed to open file: %s\n",
	    file_info_table.file_info[index].filename);
    return REG_FAILURE;
  }

  /* Read header */
  if(fread((void *)buffer, 
	   (size_t)1, 
	   REG_PACKET_SIZE, 
	   file_info_table.file_info[index].fp) != (size_t)REG_PACKET_SIZE) {
    fprintf(stderr, "STEER: Consume_start_data_check_file: failed to read "
	    "header from file: %s\n",
	    file_info_table.file_info[index].filename);
    remove(file_info_table.file_info[index].filename);
    return REG_FAILURE;
  }

  if(!strstr(buffer, REG_DATA_HEADER)) {
    fprintf(stderr, "STEER: Consume_start_data_check_file: wrong "
	    "header from file: %s\n",
	    file_info_table.file_info[index].filename);
    remove(file_info_table.file_info[index].filename);
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}

/*---------------------------------------------------*/
