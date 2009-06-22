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
