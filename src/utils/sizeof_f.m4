/*----------------------------------------------------------------------------
  Simple c-code designed to provide 'sizeof' functionality to an
  F90 calling routine.

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

void FUNCTION(sizeof_f) ARGS(`STRING_ARG(string), size')
STRING_ARG_DECL(string);
int *size;
{
  char *ptr;

  ptr = STRING_PTR(string);

  if(strcmp(ptr, "short") == 0){

    *size = sizeof(short);
  }
  else if(strcmp(ptr, "int") == 0){

    *size = sizeof(int);
  }
  else if(strcmp(ptr, "long") == 0){
  
    *size = sizeof(long);
  }
  else if(strcmp(ptr, "float") == 0){
  
    *size = sizeof(float);
  }
  else if(strcmp(ptr, "double") == 0){
  
    *size = sizeof(double);
  }
  else{

    printf("sizeof_f: unrecognised c type: %s\n", STRING_PTR(string));
    *size = 0;
  }

  return;
}
