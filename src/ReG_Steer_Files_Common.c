/*----------------------------------------------------------------------------
  (C) Copyright 2009, University of Manchester, United Kingdom,
  all rights reserved.

  This software was developed by the RealityGrid project
  (http://www.realitygrid.org), funded by the EPSRC under grants
  GR/R67699/01 and GR/R67699/02.

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
---------------------------------------------------------------------------*/

#include "ReG_Steer_Config.h"

#include "ReG_Steer_Files_Common.h"
#include "ReG_Steer_Common.h"
#include "ReG_Steer_Appside_internal.h"

file_info_table_type file_info_table;

/* Need access to these tables which are actually declared in 
   ReG_Steer_Appside_internal.h */
extern IOdef_table_type IOTypes_table;
extern Steerer_connection_table_type Steerer_connection;

/** Global scratch buffer - declared in ReG_Steer_Appside.c */
extern char Global_scratch_buffer[];

/*--------------------------------------------------------------------*/

int file_info_table_init() {
  int i;

  file_info_table.max_entries = IOTypes_table.max_entries;
  file_info_table.num_used = 0;
  file_info_table.file_info = (file_info_type*) 
    malloc(file_info_table.max_entries * sizeof(file_info_type));

  if(file_info_table.file_info == NULL) {
    fprintf(stderr, "STEER: file_info_table_init: failed to allocate memory "
	    "for file info table\n");
    return REG_FAILURE;
  }

  for(i = 0; i < file_info_table.max_entries; i++) {
    file_info_table.file_info[i].fp = NULL;
  }

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/

int Get_file_list(char *fileroot,
		  int  *num,
		  char ***names)
{
  char  redirection[REG_MAX_STRING_LENGTH];
  char *pchar;
  char  bufline[REG_MAX_STRING_LENGTH];
  int   len;
  int   i, j;
  FILE *fp;

  *num = 0;

/*   snprintf(redirection, REG_MAX_STRING_LENGTH, " > %s/ReG_files.tmp", */
/* 	   Steerer_connection.file_root); */
  snprintf(redirection, REG_MAX_STRING_LENGTH, " > ./ReG_files.tmp");

  /* Calc. length of string - 'ls -1' and slashes add 9 chars so
     add a few more for safety.  Ask for 2*strlen(ChkTag) so that
     we can use the end of this buffer to hold the trimmed version
     of the tag. */
  len = strlen(fileroot) + strlen(redirection) + 20;

  if(!(pchar = (char *)malloc(len))) {

    fprintf(stderr, "STEER: Get_file_list: malloc of %d bytes failed\n",
	    len);
    return REG_FAILURE;
  }

  sprintf(pchar, "ls -1 %s %s", fileroot, redirection);
  system(pchar);

  free(pchar);
  pchar = NULL;

/*   snprintf(redirection, REG_MAX_STRING_LENGTH, "%s/ReG_files.tmp", */
/* 	   Steerer_connection.file_root); */
  snprintf(redirection, REG_MAX_STRING_LENGTH, "./ReG_files.tmp");

  if((fp = fopen(redirection, "r"))) {

    while(fgets(bufline, REG_MAX_STRING_LENGTH, fp)) {
      (*num)++;
    }

    if(*num == 0){
      remove(redirection);
      fclose(fp);
      return REG_FAILURE;
    }

    *names = (char **)malloc(*num * sizeof(char*));
    rewind(fp);

    for(i=0; i<*num; i++) {

      fgets(bufline, REG_MAX_STRING_LENGTH, fp);
      /* fgets includes '\n' in the returned buffer */
      len = (int)strlen(bufline);
      if(!((*names)[i] = (char *)malloc(len))) {

	fprintf(stderr, "STEER: Get_file_list: malloc failed\n");
	for(j=i; j>=0; j--) {
	  free((*names)[i]);
	  (*names)[i] = NULL;
	}
	free(*names);
	*names = NULL;
	remove(redirection);
	fclose(fp);
	return REG_FAILURE;
      }
      memcpy((*names)[i], bufline, len);
      /* Terminate string - overwrite '\n' */
      (*names)[i][len-1] = '\0';
    }

    fclose(fp);
    remove(redirection);
  }

  return REG_SUCCESS;
}

/*----------------------------------------------------------------*/
