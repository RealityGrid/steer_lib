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
int Get_file_list(char* fileroot, int* num, char*** names);

#endif /* __REG_STEER_FILES_COMMON_H__ */
