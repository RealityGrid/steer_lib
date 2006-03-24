/*----------------------------------------------------------------------------
  Header file defining the routines used to create and communicate
  with a steering-proxy process.

  (C) Copyright 2005, University of Manchester, United Kingdom,
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

/** @internal
    @file ReG_Steer_Proxy_utils.h
    @brief Header defining routines for creating & communicating with steering-proxy
    @deprecated
    @author Andrew Porter
*/

#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#ifndef WIN32
#include <unistd.h>
#endif
#include "ReG_Steer_types.h"

/*-----------------------------------------------------------------*/

/** @internal
    Messages used to communicate with proxy steerer - these MUST
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

/** @internal @deprecated */
int Create_proxy(int *to_proxy, int *from_proxy);

/** @internal @deprecated */
int Destroy_proxy(int pipe_to_proxy);

/** @internal @deprecated */
int Send_proxy_message(int pipe_to_proxy, const char *buf);

/** @internal @deprecated */
int Get_proxy_message(int pipe_from_proxy, char *buf, int *nbytes);

/** @internal @deprecated
    Function to check that the specified java executable (assumed to
    need ".class" appending to it) is on the specified @p class_path */
int Proxy_is_in_path(const char *class_path, const char *exec);

/** @internal @deprecated
    Utility function to read next line from stream attached to @p fd */
int getline(char s[], int lim, int fd);

