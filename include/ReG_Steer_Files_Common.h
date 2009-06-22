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

#ifndef __REG_STEER_FILES_COMMON_H__
#define __REG_STEER_FILES_COMMON_H__

#include "ReG_Steer_Config.h"

#include "ReG_Steer_types.h"

typedef struct {
  /** Base filename - for file-based IO */
  char	filename[REG_MAX_STRING_LENGTH];
  /** Directory to write to - for file-based IO */
  char  directory[REG_MAX_STRING_LENGTH];
  /** Pointer to open file - for file-based IO */
  FILE* fp;
} file_info_type;

typedef struct {
  int max_entries;
  int num_used;
  file_info_type* file_info;
} file_info_table_type;

/* Function Prototypes */

int file_info_table_init();

/** @internal
    @param dir The directory to search
    @param num_tags Number of tags to search for
    @param tags Elements of the filenames that we are searching for
    @param num_files Number of filenames being returned
    @param filenames Array of char* holding names of files found

    Searches for files in directory @p dir matching the tags specified.
    If any are found, @p filenames is malloc'd to point to an array of
    char* and each entry in this array is malloc'd and set to the
    relevant filename. These ptrs MUST be free'd. @p num_files can be
    zero even if the routine returns REG_SUCCESS. */
int Get_file_list(const char* dir, int num_tags, char** tags,
		  int* num_files, char*** filenames);

/** @internal
    @param str1 The first dtring to be compared
    @param str2 The second string to be compared

    A string comparison function for use with qsort
    to sort the filenames returned by Get_file_list */
static int cmpstrs(const void* str1, const void* str2) {
  return strcmp(*(char* const*) str1, *(char* const*) str2);
}

#endif /* __REG_STEER_FILES_COMMON_H__ */
