/*----------------------------------------------------------------------------
    Base header file for the steering library - contains definitions
    of return values, message types and array sizes.

    (C)Copyright 2002 The University of Manchester, United Kingdom,
    all rights reserved.

    This software is produced by the Supercomputing, Visualization &
    e-Science Group, Manchester Computing, the Victoria University of
    Manchester as part of the RealityGrid project.

    This software has been tested with care but is not guaranteed for
    any particular purpose. Neither the authors, nor the University of
    Manchester offer any warranties or representations, nor do they
    accept any liabilities with respect to this software.

    This program must not be used for commmercial gain without the
    written permission of the authors.
    
    Supercomputing, Visualization & e-Science Group
    Manchester Computing
    University of Manchester
    Manchester M13 9PL

    email:  csar-advice@cfs.ac.uk.
    Tel:    +44 161 275 6824/5997
    Fax:    +44 161 275 6040    
    
    Date          Version    Updates                            Author
    ----          -------    -------                            ------
    23.7.2002       0.1                                         A Porter

---------------------------------------------------------------------------*/

#ifndef __REG_TYPES_INCLUDED

#define __REG_TYPES_INCLUDED

#define DEBUG 1

#include <stdio.h>
#include <stdlib.h>

/* Flag for debugging - set to one to retain all xml messages */
#define NO_FILE_DELETE 0

/* UNICORE_DEMO must be defined in order to produce a steering lib
   compatible with the UNICORE steering demonstration framework.
   Only consequence is that the status files emitted by the application
   are all called 'steer_status' and not indexed - i.e. some output will 
   be lost if the status files are not consumed sufficiently rapidly */
#define UNICORE_DEMO

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

#define REG_SUCCESS  0
#define REG_FAILURE  1
#define REG_EOD      2
#define REG_MEM_FAIL 3

/* Limit on number of commands steerer can send at any one time */
#define REG_MAX_NUM_STR_CMDS 50

/* Limit on number of parameters steerer can modify at any one time */
#define REG_MAX_NUM_STR_PARAMS 100

/* Initial sizes for internal tables of registered IO types 
   and parameters */

#define REG_INITIAL_NUM_IOTYPES 20
#define REG_INITIAL_NUM_PARAMS  20

/* Initial limit on no. of registered supported commands */

#define REG_INITIAL_NUM_CMDS 20

/* Initial limit on no. of sims being steered at any one time */

#define REG_MAX_NUM_STEERED_SIM 10

/* Limit on number of files used in communication - filenames
   wrap back on themselves once this limit reached */
#ifdef UNICORE_DEMO
#define REG_MAX_NUM_FILES 50
#else
#define REG_MAX_NUM_FILES 6
#endif

/* Maximum length for any string */

#define REG_MAX_STRING_LENGTH 80

/* Steering commands */

#define REG_STR_STOP             1
#define REG_STR_PAUSE            2
#define REG_STR_RESUME           3
#define REG_STR_DETACH           4
#define REG_STR_SET_EMIT_FREQ    5
#define REG_STR_SET_CONSUME_FREQ 6

/* All generated IOtype handles must be >= this value because they
   will be interpreted as commands - this value must therefore be >
   than all of the commands #define'd above */

#define REG_MIN_IOTYPE_HANDLE 1000

/* The three different types that an IOtype can have - used only
   by the steerer in the construction of a more intuitive gui */

#define REG_IO_IN    0
#define REG_IO_OUT   1
#define REG_IO_CHKPT 2

/* Coding scheme for data types */

#define REG_INT   0
#define REG_FLOAT 1
#define REG_DBL   2
#define REG_CHAR  3

/* Type definitions */

#define FALSE 0
#define TRUE  1
#define false 0
#define true  1

/* Reserved handle values */

#define REG_SIM_HANDLE_NOTSET   -1
#define REG_IODEF_HANDLE_NOTSET -1
#define REG_PARAM_HANDLE_NOTSET -1
#define REG_SEQ_NUM_HANDLE      -100

/* Message tags */

#define MSG_NOTSET  0
#define SUPP_CMDS   1
#define IO_DEFS     2
#define PARAM_DEFS  3
#define STATUS      4
#define CONTROL     5

/* Some type definitions - handles are simply integers for
   the moment */

typedef int REG_MsgType;
typedef int REG_IOHandleType;
/*typedef int REG_SimIDType;*/

/* Definitions used in communicating with the java proxy */

#define REG_MAX_LINE_LEN 256
#define REG_MAX_MSG_SIZE BUFSIZ
/*#define REG_HEADER_BYTES 4*/
#define REG_PIPE_UNSET   -1

#endif /* __REG_TYPES_INCLUDED defined */
