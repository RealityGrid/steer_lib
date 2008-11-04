/*----------------------------------------------------------------------------
  The Fortran/C ABI wrangler - This file contains functions and wrappers to
  test name mangling between C and Fortran.

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

/* Test functions. One with an underscore in it */

int testfunc(int ret) {
  return ret;
}

int test_func(int ret) {
  return ret;
}

/*
 * Test wrappers.
 */

/* lower case */

void tf (Status)
int *Status;
{
  *Status = (int)(testfunc(0));
  return;
}

void t_f (Status)
int *Status;
{
  *Status = (int)(test_func(0));
  return;
}

void tf_ (Status)
int *Status;
{
  *Status = (int)(testfunc(1));
  return;
}

void t_f_ (Status)
int *Status;
{
  *Status = (int)(test_func(1));
  return;
}

void t_f__ (Status)
int *Status;
{
  *Status = (int)(test_func(2));
  return;
}

/* upper case */

void TF (Status)
int *Status;
{
  *Status = (int)(testfunc(10));
  return;
}

void T_F (Status)
int *Status;
{
  *Status = (int)(test_func(10));
  return;
}

void TF_ (Status)
int *Status;
{
  *Status = (int)(testfunc(11));
  return;
}

void T_F_ (Status)
int *Status;
{
  *Status = (int)(test_func(11));
  return;
}

void T_F__ (Status)
int *Status;
{
  *Status = (int)(test_func(12));
  return;
}
