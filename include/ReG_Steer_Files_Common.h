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

int file_info_table_init(const int max_entries);

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

/** @internal
    @param base_name Root of the filename to search for

    Searches for and opens the next file in a numbered
    sequence with the specified root name */
FILE* open_next_file(char* base_name);

/** @internal
    @param filename Base name of lock file

    Creates a lock file with name consisting of ".lock"
    appended to supplied name */
int create_lock_file(char* filename);

/** @internal
    @param filename Full path to file to delete

    Delete the specified file (must have full path) */
int delete_file(char* filename);

/** @internal
    @param base_name Base of the name of the messaging files to look for

    Called when steering finished - cleans up any files that either the app
    or steerer hasn't got around to consuming 
 */
int remove_files(char* base_name);

#endif /* __REG_STEER_FILES_COMMON_H__ */
