/*----------------------------------------------------------------------------
  Header file defining the routines used to create and communicate
  with a steering-proxy process.

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

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>

#include "ReG_Steer_types.h"

/*-----------------------------------------------------------------*/

/* Messages used to communicate with proxy steerer - these MUST
   MATCH the definitions in ReG_Steer_Proxy.java */

static char *END_OF_MSG     = "END_OF_MSG\n";
static char *OK_MSG         = "STATUS_OK\n";
static char *ERR_MSG        = "ERROR\n";
static char *ATTACH_MSG     = "ATTACH\n";
static char *DETACH_MSG     = "DETACH\n";
static char *QUIT_MSG       = "QUIT\n";
static char *GET_APPS_MSG   = "GET_APPS\n";
static char *GET_STATUS_MSG = "GET_STATUS\n";
static char *SEND_CTRL_MSG  = "SEND_CTRL\n";

/*-----------------------------------------------------------------*/

int Create_proxy(int *to_proxy, int *from_proxy);

int Destroy_proxy(int pipe_to_proxy);

int Send_proxy_message(int pipe_to_proxy, const char *buf);

int Get_proxy_message(int pipe_from_proxy, char *buf, int *nbytes);

/* Function to check that the specified java executable (assumed to
   need ".class" appending to it) is on the specified class path */
int Proxy_is_in_path(const char *class_path, const char *exec);

/* Utility function to read next line from stream attached to fd */
int getline(char s[], int lim, int fd);

