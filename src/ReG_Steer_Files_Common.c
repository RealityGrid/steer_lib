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

  Author: Robert Haines
 */

/** @internal
    @file ReG_Steer_Files_Common.c
    @brief Source file for file-specific routines.
    @author Robert Haines
  */

#include "ReG_Steer_Config.h"

#include "ReG_Steer_Files_Common.h"
#include "ReG_Steer_Common.h"
#include "ReG_Steer_Appside_internal.h"

file_info_table_type file_info_table;

/*--------------------------------------------------------------------*/

int file_info_table_init(const int max_entries) {
  int i;

  file_info_table.max_entries = max_entries;
  file_info_table.num_used = 0;
  file_info_table.file_info = (file_info_type*)
    malloc(max_entries * sizeof(file_info_type));

  if(file_info_table.file_info == NULL) {
    fprintf(stderr, "STEER: file_info_table_init: failed to allocate memory "
	    "for file info table\n");
    return REG_FAILURE;
  }

  for(i = 0; i < max_entries; i++) {
    file_info_table.file_info[i].fp = NULL;
  }

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

FILE* open_next_file(char* base_name) {
  FILE* fp;
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
      fp = NULL;
      if(stat(tmp_filename, &stbuf) != -1){

        /* timespec_t     st_mtim;      Time of last data modification
           Times measured in seconds and nanoseconds
           since 00:00:00 UTC, Jan. 1, 1970 */
        sprintf(filename1,"%s_%d", base_name, i);
	time1 = (long)stbuf.st_mtime;
        break;
      }
      else{

	fprintf(stderr, "STEER: Open_next_file: failed to stat %s\n", tmp_filename);
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
      fp = NULL;
      if(stat(tmp_filename, &stbuf) != -1){

        /* timespec_t     st_mtim;      Time of last data modification
           Times measured in seconds and nanoseconds
           since 00:00:00 UTC, Jan. 1, 1970 */
        sprintf(filename2,"%s_%d", base_name, i);
	time2 = (long)stbuf.st_mtime;/*.tv_sec;*/
        break;
      }
      else{

	fprintf(stderr, "STEER: Open_next_file: failed to stat %s\n", tmp_filename);
      }
    }

    i--;
  }

  /* We want to open the oldest file that we've found... */

  if(time1 != -1 && time2 != -1){

    if(time2 < time1) strcpy(filename1, filename2);

    if( (fp = fopen(filename1, "r")) ){

#ifdef REG_DEBUG
      fprintf(stderr, "STEER: Open_next_file: opening %s\n", filename1);
#endif
      /* Return the name of the file actually opened */
      strcpy(base_name, filename1);
    }
  }

  return fp;
}

/*----------------------------------------------------------------*/

int create_lock_file(char* filename) {
  int fd;
  char lock_file[REG_MAX_STRING_LENGTH + 5];

  /* Create lock file to flag that a file of same root is ready to
     be read */

  sprintf(lock_file, "%s.lock", filename);

  if((fd = creat(lock_file,
		 (S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH))) < 0) {

    return REG_FAILURE;
  }

  close(fd);
  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int delete_file(char* filename) {
  int  return_status = REG_SUCCESS;
  char long_filename[REG_MAX_STRING_LENGTH+5];

  /* Remove lock file first (because this is what is searched for) */

  sprintf(long_filename, "%s.lock", filename);

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: Delete_file: removing %s\n", long_filename);
#endif

  if(remove(long_filename)){
    perror("STEER: Delete_file, deleting lock file");
    return_status = REG_FAILURE;
  }

#if REG_KEEP_XML_MESSAGES
  /* If this debugging flag is set then don't remove the data file */
  return REG_SUCCESS;
#endif

  /* Remove the data file */

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: Delete_file: removing %s\n", filename);
#endif

  if(remove(filename)){
    perror("STEER: Delete_file, deleting data file");
    return_status = REG_FAILURE;
  }
  return return_status;
}

/*----------------------------------------------------------------*/

int remove_files(char* base_name) {
  char  filename[REG_MAX_STRING_LENGTH];
  char  lock_name[REG_MAX_STRING_LENGTH];
  FILE *fp;

  /* Remove any files that we would have normally consumed */

  strcpy(filename, base_name);

#ifdef REG_DEBUG
  fprintf(stderr, "STEER: Remove_files: looking for files beginning: %s\n", filename);
#endif

  while((fp = open_next_file(filename))) {

    fclose(fp);

    /* Remove lock file */
    sprintf(lock_name, "%s.lock", filename);
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: Remove_files: deleting %s\n", lock_name);
#endif
    remove(lock_name);

#if REG_KEEP_XML_MESSAGES
    /* Don't delete actual data files if this debugging flag set */
    continue;
#endif

    /* Remove associated data file */
#ifdef REG_DEBUG
    fprintf(stderr, "STEER: Remove_files: deleting %s\n", filename);
#endif
    remove(filename);

    /* Reset filename ready to look for next one */
    strcpy(filename, base_name);
  }

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/


/*----------------------------------------------------------------*/


/*----------------------------------------------------------------*/
