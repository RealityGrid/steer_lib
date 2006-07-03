/*----------------------------------------------------------------------------
  Simply code to report sizeof c types on current architecture.

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

  Authors........: Andrew Porter, Robert Haines
    
---------------------------------------------------------------------------*/
#include <stdlib.h>
#include <stdio.h>

void main()
{
  printf("sizeof(short)  = %d bytes\n", sizeof(short));
  printf("sizeof(uint)   = %d bytes\n", sizeof(unsigned int));
  printf("sizeof(char)   = %d bytes\n", sizeof(char));
  printf("sizeof(int)    = %d bytes\n", sizeof(int));
  printf("sizeof(long)   = %d bytes\n", sizeof(long));
  printf("sizeof(float)  = %d bytes\n", sizeof(float));
  printf("sizeof(double) = %d bytes\n", sizeof(double));
}
