/*----------------------------------------------------------------------------
  Base header file for the steering library - contains definitions
  of return values, message types and array sizes.

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

#ifndef __REG_TYPES_INCLUDED

#define __REG_TYPES_INCLUDED

/* Whether or not to use timing routines - these are not very portable 
#define USE_REG_TIMING */

#include <stdio.h>
#include <stdlib.h>

/* Flag for debugging - set to one to retain all xml messages */
#define NO_FILE_DELETE 0

/* UNICORE_DEMO must be defined in order to produce a steering lib
   compatible with the UNICORE steering demonstration framework.
   Only consequence is that the status files emitted by the application
   are all called 'steer_status' and not indexed - i.e. some output will 
   be lost if the status files are not consumed sufficiently rapidly */
/*#define UNICORE_DEMO*/

/* The namespace used for steering messages (which are in xml) */
#define REG_STEER_NAMESPACE "http://www.realitygrid.org/xml/steering"

/* Filename of lockfile indicating sim is steerable */
#define APP_STEERABLE_FILENAME "app_steerable"

/* Filename of lockfile to signify steerer has connected */
#define STR_CONNECTED_FILENAME  "steering_active"

/* Root of filename used by application to send data to steerer
   Actual communication consists of two files: of the form 
   APP_TO_STR_FILENAME_<n> and APP_TO_STR_FILENAME_<n>.lock.  The 
   library looks for the presence of the (empty) .lock file before 
   attempting to open the associated data file.  <n> is some integer, 
   incremented each time a file is written and limited 
   to 0 <= n <= REG_MAX_NUM_FILES-1 */
#define APP_TO_STR_FILENAME "status_info"

/* Root of filename used by steerer to send data to application.
   Actual name will be of form STR_TO_APP_FILENAME_<n> and
   STR_TO_APP_FILENAME_<n>.lock.  The library looks for the 
   presence of the (empty) .lock file before attempting to open
   the associated data file.  <n> is some integer, incremented each
   time a file is written and limited to 0 <= n <= REG_MAX_NUM_FILES-1 */
#define STR_TO_APP_FILENAME "control_info"

/* Return values */

#define REG_SUCCESS   0
#define REG_FAILURE   1
#define REG_EOD       2
#define REG_MEM_FAIL  3
#define REG_TIMED_OUT 4

/* Limit on number of commands steerer can send at any one time */
#define REG_MAX_NUM_STR_CMDS 20

/* Limit on number of parameters steerer can modify at any one time */
#define REG_MAX_NUM_STR_PARAMS 40

/* Initial sizes for internal tables of registered IO types 
   and parameters */

#define REG_INITIAL_NUM_IOTYPES 20
#define REG_INITIAL_NUM_PARAMS  40

/* Initial limit on no. of registered supported commands */

#define REG_INITIAL_NUM_CMDS 20

/* Initial limit on no. of sims being steered at any one time */

#define REG_MAX_NUM_STEERED_SIM 5

/* Limit on number of files used in communication - filenames
   wrap back on themselves once this limit reached */
#define REG_MAX_NUM_FILES 500

/* Maximum length for any string */

#define REG_MAX_STRING_LENGTH 256

/* Steering commands */

#define REG_STR_STOP             1
#define REG_STR_PAUSE            2
#define REG_STR_RESUME           3
#define REG_STR_DETACH           4

/* All generated IOtype handles must be >= this value because they
   will be interpreted as commands - this value must therefore be >
   than all of the commands #define'd above */

#define REG_MIN_IOTYPE_HANDLE 1000

/* The three different types that an IOtype can have - used only
   by the steerer in the construction of a more intuitive gui */

#define REG_IO_IN    0
#define REG_IO_OUT   1
#define REG_IO_INOUT 2

/* Size (in bytes) of input buffer for each active IO channel */

#define REG_IO_BUFSIZE  1048576

/* Max length of each ASCII packet to be sent down a socket.  This enables
   headers and footers to be sent. */

#define REG_PACKET_SIZE 128
#define REG_PACKET_FORMAT "%-128s"

#define REG_DATA_HEADER "<ReG_data>"
#define REG_DATA_FOOTER "</ReG_data>"

#define BEGIN_SLICE_HEADER "<ReG_data_slice_header>"
#define END_SLICE_HEADER   "</ReG_data_slice_header>"


/* Coding scheme for data types */

#define REG_INT        0
#define REG_FLOAT      1
#define REG_DBL        2
#define REG_CHAR       3
#define REG_XDR_INT    4
#define REG_XDR_FLOAT  5
#define REG_XDR_DOUBLE 6

#define REG_SIZEOF_XDR_INT    4
#define REG_SIZEOF_XDR_FLOAT  8
#define REG_SIZEOF_XDR_DOUBLE 8

/* Type definitions */

#define FALSE 0
#define TRUE  1
#define false 0
#define true  1

/* Reserved handle values */

#define REG_SIM_HANDLE_NOTSET   -1
#define REG_IODEF_HANDLE_NOTSET -1
#define REG_PARAM_HANDLE_NOTSET -1
/* Handle for the sequence number parameter */
#define REG_SEQ_NUM_HANDLE      -100
/* Handle for the time-per-step parameter */
#define REG_STEP_TIME_HANDLE    -99
/* Handle for parameter holding time-stamp */
#define REG_TIMESTAMP_HANDLE    -98
/* Handle for the variable holding the interval between 
   steering activity */
#define REG_STEER_INTERVAL_HANDLE -97
/* Min value for an automatically generated parameter handle
   - must be > than all special handles defined above */
#define REG_MIN_PARAM_HANDLE     0

/* Message tags */

#define MSG_NOTSET  0
#define SUPP_CMDS   1
#define IO_DEFS     2
#define PARAM_DEFS  3
#define STATUS      4
#define CONTROL     5
#define CHK_DEFS    6
#define STEER_LOG   7

/* Some type definitions - handles are simply integers for
   the moment */

typedef int REG_IOHandleType;

/* Definitions used in communicating with the java proxy */

#define REG_MAX_LINE_LEN 256
#define REG_MAX_MSG_SIZE 4096
#define REG_PIPE_UNSET   -1

/* Tolerance used to determine whether a floating point no. should
   be interpreted as zero */

#define REG_TOL_ZERO 1.0e-6

/* Parameters used to configure table for logging checkpoints */

#define REG_INITIAL_CHK_LOG_SIZE 50
#define REG_LOG_FILENAME         "ReG_checkpoint_log.xml"

#define REG_LOG_STEERING 1

/* Values for IOdef_entry.comms_status */

#define REG_COMMS_STATUS_NULL			0
#define REG_COMMS_STATUS_LISTENING		1
#define REG_COMMS_STATUS_WAITING_FOR_ACCEPT	2
#define REG_COMMS_STATUS_WAITING_TO_CONNECT	3
#define REG_COMMS_STATUS_CONNECTED		4
#define REG_COMMS_STATUS_FAILURE		5
#define REG_COMMS_STATUS_CLOSING		6

#define REG_COMMS_NOT_READY   0
#define REG_COMMS_READY_READ  1
#define REG_COMMS_READY_WRITE 2

#define REG_PORT_NOTSET  -1

/* Max no. of service-data elements on SGS */
#define REG_MAX_NUM_SGS_SDE 15

/* Standard return values from the SGS */
#define REG_SGS_ERROR "SGS_ERROR"
#define REG_SGS_SUCCESS "SGS_SUCCESS"

/* Default minimum interval (integer no. of seconds) at which 
   to poll for connection from steerer - overridden by 
   REG_APP_POLL_INTERVAL environment variable if set */ 
#define REG_APP_POLL_INTERVAL_DEFAULT 5

#endif /* __REG_TYPES_INCLUDED defined */
