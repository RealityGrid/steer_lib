/*----------------------------------------------------------------------------
  This file contains utility routines and data structures for the utilities
  library associated with the ReG computational steering framework.

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

  Authors........: Andrew Porter

---------------------------------------------------------------------------*/

/** @file ReG_Steer_Utils.c
    @brief Source file for utilities
  */

#include "ReG_Steer_types.h"

#ifndef WIN32
#include <time.h>
#include <sys/time.h>
#endif /* not WIN32 */

#include "soapH.h"

/*----------------------------------------------------------------*/

char *Get_current_time_string()
{
  struct timeval tv;
  struct timezone tz;
  struct tm *now_details;
  static char date_string[128];

  gettimeofday(&tv, &tz);

  now_details = gmtime(&(tv.tv_sec));
  /* 2005-08-31T14:31:51Z */
  sprintf(date_string,"%d-%02d-%02dT%02d:%02d:%02dZ",
	  (now_details->tm_year) + 1900,
	  (now_details->tm_mon) + 1, 
	  now_details->tm_mday,
	  now_details->tm_hour,
	  now_details->tm_min,
	  now_details->tm_sec);

  return date_string;
}
