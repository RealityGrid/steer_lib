/*----------------------------------------------------------------------------
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

#ifndef __REG_TYPES_INCLUDED
#define __REG_TYPES_INCLUDED

/** @file ReG_Steer_types.h
    @brief Top-level header file

    Base header file for the steering library - contains definitions
    of return values, message types and array sizes.

    @author Andrew Porter
    @author Robert Haines

    @mainpage

    @section intro_sec Introduction 

    A number of people contributed to the initial design of the
    RealityGrid steering API and thus were authors of the document
    that this Doxygen-generated documentation replaces.  They are:
    Stephen Pickles, Robin Pinning, Andrew Porter, Graham Riley,
    Rupert Ford, Ken Mayes, David Snelling, Jim Stanton, Steven Kenny
    and Shantenu Jha.

    @latexonly
    \begin{figure}
    \centerline{\includegraphics{../steer_arch_symm.png}}
    \caption{Basic architecture for RealityGrid steering.  Each ellipse
    represents a distinct application, the location of which is not
    constrained.  The client can dynamically attach to the running
    application, perform some steering activity and then detach again. The
    application may also use the steering library to send data sets to
    some other component, {\it e.g}. a visualization package.}
    \end{figure}
    @endlatexonly

    <IMG SRC="../steer_arch_symm.png" alt="Illustrative architecture diagram"
         title="Illustrative architecture of a steering scenario"/>

    The figure shows an example use case for computational steering.
    The simulation component has been instrumented using the steering
    API.  This enables it to receive control messages from the
    steering client and return status messages to it. In addition to
    interacting with the steering client, the simulation in the figure
    also uses the API to transfer (potentially large) data sets to a
    second component.  Although this second component is labelled 
    'visualization,' it could in fact be any other type of component
    including a second form of simulation (as in a coupled-model
    scenario).

    The API has been designed with the assumption that the control and
    status links between a steering client (labelled 'Steering
    connection' in the figure) and simulation will involve the
    exchange of small quantities of data.  For cases where the
    steerable 'parameter' is a large array of data, it will best be
    treated as a form of 'sample data' with the steering client
    issuing instructions for it to be emitted and consumed as
    required.

    All routines will provide integer return values with a return
    value of REG_SUCCESS (zero) indicating success and a return value
    of REG_FAILURE (unity) indicating failure.  (These quantities are
    defined in ReG_Steer_types.h.)  Some routines use this return
    value to specify more detailed information on the nature of the
    success/failure - see individual interface specifications for
    details.

    @section sec_background Background Assumptions

    Application scientists have pre-existing codes (hereafter referred
    to as simulations), which are to be made steerable. These codes
    are written in Fortran, C, C++ or a mixture of these, and can be
    compiled using standards-compliant Fortran90 and C/C++
    compilers. These codes will in general be parallel, but we avoid
    assuming or prescribing any particular paradigm (e.g. message
    passing or shared memory) or harness (e.g. MPI, PVM, SHMEM).  As a
    consequence of this, the API presented here is designed to be
    called on a single thread only.  In a parallel code therefore, the
    application programmer assumes the responsibility of communicating
    any changes resulting from steering activity to the other
    threads/processes.

    We assume that the logical structure within the simulation is such
    that there exists a point (breakpoint) within a larger control
    loop at which it is feasible to insert new functionality intended
    to:
    @li accept a change to one or more of the parameters of the
    simulation (steerable parameters); 
    @li emit a consistent representation of the current state of 
    both the steerable parameters and other variables (monitored 
    quantities); 
    @li emit a consistent representation (provisionally called 
    outsample) of part of the system being simulated that may be 
    required by a downstream component (@e e.g. a visualisation system or 
    another simulation).  

    We also assume that it is feasible, at the same point in the control
    loop, to:
    @li output a consistent representation of the system (checkpoint) 
    containing sufficient information to enable a subsequent restart of 
    the simulation from its current state;
    @li (in the case that the steered component is itself downstream 
    of another component), to accept a sample emitted by an upstream 
    component (provisionally called insample).

    @section sec_f90 F90 bindings

    The API itself also provides F90 bindings for the simulation-side
    routines in ReG_Steer_Appside_f.c.  All of the routines making up
    the F90 bindings are implemented as F90 subroutines and hence
    their return value is passed back to the caller by an additional
    @p status argument (c.f. MPI).  Where an argument to an interface
    in the API is an array, the first element of the array should be
    supplied by the calling routine. The KIND parameters used here,
    REG_INT_KIND, REG_SP_KIND and REG_DP_KIND, are defined in
    reg_steer_f90.inc and correspond to KIND(1), KIND(1.0) and
    KIND(1.0D0), respectively.

    @section cmds_sec Pre-defined steering commands

    The API defines and uses four pre-defined commands: 'stop', 
    'pause', 'resume' and 'detach'.  An application that is
    steering enabled using the API supports `detach' by default.  It
    is the responsibility of the application programmer to decide
    whether or not their application will support 'stop' and 'pause' 
    and, if so, to implement this functionality.  An application that
    supports 'pause' is automatically assumed to support `resume.' 
    The relevant constants are defined in ReG_Steer_types.h

    @section sec_example Example useage

    Included with the RealityGrid steering library are a number of
    example applications:

    @li simple.c provides a very simple example of the basic 
    instrumentation required in order to make a simulation
    steerable.  
    @li mini_steerer.c provides a fairly simple example of a command-line 
    steering client.
    @li mini_app.c illustrates more of the library's functionality
    @li mini_app2.f90 similar to mini_app.c but using the F90 bindings
    @li mini_app_para.f90 An example of using the steering library in a
    parallel (MPI), F90 code
    @li sink.c Example of using steering library to receive data (@e e.g.
    for visualization)

    @section sec_IO IO Control

    The API contains a number of routines  intended to allow an
    application to emit and consume potentially large quantities of
    data ('Sample data').  An application may emit or consume various
    sorts of data set and we term each of these an 'IOType.'  The
    library is designed to cope with parallel applications that are
    unable to collect the complete data set to be emitted on a single
    processor.  Such a data set must therefore be emitted piece by
    piece - this corresponds to several calls to Emit_data_slice() in
    the steering library.  Multiple calls to this routine must also be
    made if emitting data of different types.
    @see Emit_start(), Emit_data_slice(), Emit_stop() @n Consume_start(),
    Consume_data_slice_header, Consume_data_slice(), Consume_stop()

    @section sec_ChkPt Checkpointing Control

    When requesting that the simulation perform a restart,
    Steering_control() returns the ChkType from which to restart and a
    ChkTag identifying which checkpoint set.  Currently, no information
    is supplied on where to find the checkpoint or on how the ChkTag
    should be used to construct the necessary filenames; this is all
    assumed to be carried out by the simulation.  This situation is not
    satisfactory but is difficult to resolve since it impinges on the
    more general area of checkpoint management.
    @see Register_ChkTypes(), Add_checkpoint_file(), Record_checkpoint_set()

    @section sec_EnvVars Environment Variables

    Some of the behaviour of the steering library, particularly on the 
    application/simulation side can be configured using certain 
    environment variables.  These can be particularly important when
    using socket-based IOTypes on machines with complex network
    configurations. Each of these environment variables
    is described below:

    @verbinclude Environment_variables.txt

    @section sec_Acks Acknowledgements

    This work was funded by the Engineering and Physical Sciences
    Research Council (http://www.epsrc.ac.uk) as part of the
    RealityGrid project (http://www.realitygrid.org).  More
    documentation and the library itself are available from
    http://www.sve.man.ac.uk/Research/AtoZ/RealityGrid/.
*/

/** If REG_WSRF is not defined then the code builds with OGSI stubs
    instead */
#define REG_WSRF

/** Comment out the below if yours is an old version of libxml2 that
    doesn't have XML_PARSE_NOERROR defined (to go with the xmlReadMemory
    in Parse_xml) */
#define REG_HAVE_XMLREADMEMORY

/** Whether or not to use timing routines - these are not very portable
#define USE_REG_TIMING */

/** This is here to allow use of SSL to be switched on and off by editing
    Makefile.include (because WITH_OPENSSL is a gSoap #define) */
#if REG_WITH_OPENSSL
#  define WITH_OPENSSL
#endif

#include <stdio.h>
#include <stdlib.h>

/** Flag for debugging - set to one to retain all xml messages */
#define NO_FILE_DELETE 0

/** UNICORE_DEMO must be defined in order to produce a steering lib
    compatible with the UNICORE steering demonstration framework.
    Only consequence is that the status files emitted by the application
    are all called 'steer_status' and not indexed - @e i.e. some output will 
    be lost if the status files are not consumed sufficiently rapidly */
/*#define UNICORE_DEMO*/

/** The namespace used for steering messages (which are in xml) */
#define REG_STEER_NAMESPACE "http://www.realitygrid.org/xml/steering"

/** Filename of lockfile indicating sim is steerable */
#define APP_STEERABLE_FILENAME "app_steerable"

/** Filename of lockfile to signify steerer has connected */
#define STR_CONNECTED_FILENAME  "steering_active"

/** Root of filename used by application to send data to steerer
    Actual communication consists of two files: of the form 
    APP_TO_STR_FILENAME_@p n and APP_TO_STR_FILENAME_@p n.lock.  The 
    library looks for the presence of the (empty) .lock file before 
    attempting to open the associated data file.  @p n is some integer, 
    incremented each time a file is written and limited 
    to 0 \<= n \<= REG_MAX_NUM_FILES-1 */
#define APP_TO_STR_FILENAME "status_info"

/** Root of filename used by steerer to send data to application.
    Actual name will be of form STR_TO_APP_FILENAME_@p n and
    STR_TO_APP_FILENAME_@p n.lock.  The library looks for the 
    presence of the (empty) .lock file before attempting to open
    the associated data file.  @p n is some integer, incremented each
    time a file is written and limited to 0 \<= n \<= REG_MAX_NUM_FILES-1 */
#define STR_TO_APP_FILENAME "control_info"

/** Return value upon complete success */
#define REG_SUCCESS    0
/** Return value upon failure */
#define REG_FAILURE    1
/** Return value upon reaching End Of Data */
#define REG_EOD        2
/** Return value upon running out of memory */
#define REG_MEM_FAIL   3
/** Return value when call has timed out */
#define REG_TIMED_OUT  4
/** Return value when library not in state to complete task */
#define REG_NOT_READY  5
/** Return value indicating more data needs to be sent - NOT USED? */
#define REG_UNFINISHED 6

/** Limit on number of commands steerer can send at any one time */
#define REG_MAX_NUM_STR_CMDS 20

/** Limit on number of parameters steerer can modify at any one time */
#define REG_MAX_NUM_STR_PARAMS 40

/** Limit on number of log messages we can send in one go */
#define REG_MAX_NUM_LOG_MSG 30

/** Initial size for internal table of registered IO types */
#define REG_INITIAL_NUM_IOTYPES 20
/** Initial size for internal table of registered parameters */
#define REG_INITIAL_NUM_PARAMS  40

/** Initial limit on no. of registered supported commands */
#define REG_INITIAL_NUM_CMDS 20

/** Initial limit on no. of sims being steered at any one time */
#define REG_MAX_NUM_STEERED_SIM 20

/** Limit on number of files used in communication - filenames
    wrap back on themselves once this limit reached */
#define REG_MAX_NUM_FILES 500

/** Maximum length for any string */
#define REG_MAX_STRING_LENGTH 256

/** Encoding for STOP command */
#define REG_STR_STOP             1
/** Encoding for PAUSE command - indicates that application supports
    pause but it is up to the application programmer to actually
    implement it. */
#define REG_STR_PAUSE            2
/** Encoding for RESUME command */
#define REG_STR_RESUME           3
/** Encoding for DETACH command */
#define REG_STR_DETACH           4
/** Encoding for EMIT PARAM LOG command */
#define REG_STR_EMIT_PARAM_LOG   5
/** Encoding for INTERNAL PAUSE command (used when the pause command
    is to be handled internally by the library rather than passed up
    to the application - @e i.e. the call to Steering_control will block
    until a 'resume' or 'stop' command is received) */
#define REG_STR_PAUSE_INTERNAL   6

/** All generated IOtype handles must be >= this value because they
    will be interpreted as commands - this value must therefore be >
    than all of the commands #define'd above */
#define REG_MIN_IOTYPE_HANDLE 1000

/** The three different types that an IOtype/CHKtype can have */
/** Type for an IOtype that is input only */
#define REG_IO_IN    0
/** Type for an IOtype that is output only */
#define REG_IO_OUT   1
/** Type for an IOtype that is for input and output */
#define REG_IO_INOUT 2

/** Size (in bytes) of input buffer for each active IO channel */
#define REG_IO_BUFSIZE  1048576

/** Max length of each ASCII packet to be sent down a socket.  This enables
   headers and footers to be sent. */
#define REG_PACKET_SIZE 128
/** The format to be used with printf to create a header/footer */
#define REG_PACKET_FORMAT "%-128s"
/** The header to use when sending data down a socket */
#define REG_DATA_HEADER "<ReG_data>"
/** The footer to use when sending data down a socket */
#define REG_DATA_FOOTER "</ReG_data>"
/** Marks the start of a header for an individual 'slice' of data 
    being sent down a socket */
#define BEGIN_SLICE_HEADER "<ReG_data_slice_header>"
/** Marks the end of a header for an individual 'slice' of data 
    being sent down a socket */
#define END_SLICE_HEADER   "</ReG_data_slice_header>"


/* Coding scheme for data types */
/** Encoding for an int type - equivalent to KIND(REG_INT_KIND) in F90 */
#define REG_INT        0
/** Encoding for a float type - equivalent to KIND(REG_SP_KIND) in F90 */
#define REG_FLOAT      1
/** Encoding for a double type - equivalent to KIND(REG_DP_KIND) in F90 */
#define REG_DBL        2
/** Encoding for a char type - equivalent to CHARACTER in F90 */
#define REG_CHAR       3
/** Encoding for an XDR int */
#define REG_XDR_INT    4
/** Encoding for an XDR float */
#define REG_XDR_FLOAT  5
/** Encoding for an XDR double */
#define REG_XDR_DOUBLE 6
/** Encoding for raw binary data type */
#define REG_BIN        7
/** Encoding for a long type - mainly for 64-bit F90 interface */
#define REG_LONG       8
/** Encoding for an XDR long */
#define REG_XDR_LONG   9

/** Stores the size in bytes of an XDR-encoded int - NOT USED? */
#define REG_SIZEOF_XDR_INT    4
/** Stores the size in bytes of an XDR-encoded float - NOT USED? */
#define REG_SIZEOF_XDR_FLOAT  8
/** Stores the size in bytes of an XDR-encoded double - NOT USED? */
#define REG_SIZEOF_XDR_DOUBLE 8
/** The maximum number of bytes an XDR-encoded type might use - only
    used for malloc'ing a sufficiently large buffer */
#define REG_MAX_SIZEOF_XDR_TYPE 8

/* Type definitions */
#define REG_FALSE 0
#define REG_TRUE  1
#define reg_false 0
#define reg_true  1

/* Reserved handle values */
/** Special value indicating a simulation handle is not in use */
#define REG_SIM_HANDLE_NOTSET   -1
/** Special value indicating an IOtype handle is not in use */
#define REG_IODEF_HANDLE_NOTSET -1
/** Special value indicating a parameter handle is not in use */
#define REG_PARAM_HANDLE_NOTSET -1

/** Handle for the sequence number parameter */
#define REG_SEQ_NUM_HANDLE      -100
/** Handle for the time-per-step parameter */
#define REG_STEP_TIME_HANDLE    -99
/** Handle for parameter holding time-stamp */
#define REG_TIMESTAMP_HANDLE    -98
/** Handle for the variable holding the interval between 
   steering activity */
#define REG_STEER_INTERVAL_HANDLE -97
/** Handle for variable holding current simulated time of
   simulation */
#define REG_TOT_SIM_TIME_HANDLE -96
/** Min value for an automatically generated parameter handle
   - must be > than all special handles defined above */
#define REG_MIN_PARAM_HANDLE     0
/** Label to use for the Time Step monitored parameter that
    the simulation library creates itself */
#define REG_TIMESTEP_LABEL "REG_TIME_STEP"

/* Message tags */
/** Indicates an error with the latest message */
#define MSG_ERROR   -1
/** Indicates no message received */
#define MSG_NOTSET  0
/** Indicates a supported commands message has been received */
#define SUPP_CMDS   1
/** Indicates an IOType definitions message has been received */
#define IO_DEFS     2
/** Indicates a parameter definitions message has been received */
#define PARAM_DEFS  3
/** Indicates a status message has been received */
#define STATUS      4
/** Indicates a control message has been received */
#define CONTROL     5
/** Indicates a ChkTypes definitions message has been received */
#define CHK_DEFS    6
/** Indicates that a log message has been received */
#define STEER_LOG   7

/** Typedef for handles - are simply integers for
   the moment */
typedef int REG_IOHandleType;

/** Max. line length to used in communicating with java steering 
    proxy (obsolete - UNICORE) */
#define REG_MAX_LINE_LEN 256
/** Used to indicate whether pipe connecting us to java steering
    proxy is valid (obsolete - UNICORE) */
#define REG_PIPE_UNSET   -1

/** Maximum possible size of a steering msg in bytes */
#define REG_MAX_MSG_SIZE 131072 /* 262144 524288 1048576 16384 8192 */

/** Tolerance used to determine whether a floating point no. should
   be interpreted as zero */
#define REG_TOL_ZERO 1.0e-6

/* Parameters used to configure table for logging checkpoints */
/** The initial size of the table for logging checkpoints */
#define REG_INITIAL_CHK_LOG_SIZE 50
/** The filename to use for the checkpoint log */
#define REG_LOG_FILENAME         "ReG_checkpoint_log.xml"
/** The filename to use for the parameter log */
#define REG_PARAM_LOG_FILENAME   "ReG_params_log.dat"
/** Whether or not to log steering commands */
#define REG_LOG_STEERING 1

/** Values for IOdef_entry.comms_status */
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

/** Max no. of service-data elements on SGS */
#define REG_MAX_NUM_SGS_SDE 15

/** Standard return values from the SGS */
#define REG_SGS_ERROR "SGS_ERROR"
#define REG_SGS_SUCCESS "SGS_SUCCESS"
#define REG_SGS_TIMEOUT "SGS_TIMEOUT"

/** Default minimum interval (integer no. of seconds) at which 
   to poll for connection from steerer - overridden by 
   REG_APP_POLL_INTERVAL environment variable if set */ 
#define REG_APP_POLL_INTERVAL_DEFAULT 5

/** Size of buffer used for string handling etc - use 1MB for now */
#define REG_SCRATCH_BUFFER_SIZE 1048576

/** Size of the buffer in which to store previous msg UIDs - used
    to check whether we've seen a message before */
#define REG_UID_HISTORY_BUFFER_SIZE 64

#endif /* __REG_TYPES_INCLUDED defined */
