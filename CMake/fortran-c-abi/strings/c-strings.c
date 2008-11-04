/*----------------------------------------------------------------------------
  The Fortran/C ABI wrangler - This file contains functions and wrappers to
  test string passing between C and Fortran.

  (C) Copyright 2006, University of Manchester, United Kingdom,
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

  Author.........: Robert Haines

---------------------------------------------------------------------------*/

#include <stdio.h>

void SF
char* str_ptr; int str_len;
int* val;
{
  int i;
  char str_tmp[10];

  if(str_len != 8) {
    printf("%d\n", 0);
  }
  else {
    for(i = 0; i < str_len; i++) {
      str_tmp[i] = str_ptr[i];
    }
    i++;
    str_tmp[i] = '\0';

    printf("%d\n", 1);
  }
}
