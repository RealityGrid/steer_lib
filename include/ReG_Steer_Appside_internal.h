/*----------------------------------------------------------------------------
    Header file defining internal (to the steering library) utility
    routines for use in constructing a steering interface to an 
    application.

    (C)Copyright 2002 The University of Manchester, United Kingdom,
    all rights reserved.

    This software is produced by the Supercomputing, Visualization &
    e-Science Group, Manchester Computing, the Victoria University of
    Manchester as part of the RealityGrid project.

    This software has been tested with care but is not guaranteed for
    any particular purpose. Neither the copyright holder, nor the
    University of Manchester offer any warranties or representations,
    nor do they accept any liabilities with respect to this software.

    This software must not be used for commercial gain without the
    written permission of the authors.
    
    This software must not be redistributed without the written
    permission of the authors.

    Permission is granted to modify this software, provided any
    modifications are made freely available to the original authors.
 
    Supercomputing, Visualization & e-Science Group
    Manchester Computing
    University of Manchester
    Manchester M13 9PL
    
    WWW:    http://www.sve.man.ac.uk  
    email:  sve@man.ac.uk
    Tel:    +44 161 275 6095
    Fax:    +44 161 275 6800    

    Initial version by:  A Porter, 23.7.2002

---------------------------------------------------------------------------*/

#include "ReG_Steer_Common.h"
#include "ReG_Steer_XML.h"

/*----------------------- Data structures -----------------------*/

/* Table of open IO channels */
typedef struct {

  /* Handle of associated IOType */
  int   iotype_handle;

  /* Pointer to buffer to hold data */
  void *buffer;

}IO_channel_table_type;

static IO_channel_table_type IO_channel[REG_INITIAL_NUM_IOTYPES];

/*--------- Prototypes of internal library functions -------------*/

/* Emit all of the parameters that have previously been registered 
   (if any). */
static int Emit_param_defs();

/* Emit all of the IO types that have previously been registered
   (if any). */
static int Emit_IOType_defs();

/* Emit all of the Chk types that have previously been registered
   (if any). */
static int Emit_ChkType_defs();

/* Consume a control message (if any present) from the steerer. Returns
   any commands and associated parameters (the latter as a space-separated 
   list in a string) that the application must deal with.  CommandParams is an
   array of pointers to strings that must have already had memory allocated
   for them. Also returns 
   the handles and labels of any (steerable) parameters whose values have
   changed. (The values themselves are updated from within this routine.) */
static int Consume_control(int    *NumCommands,
			   int    *Commands,
			   char  **CommandParams,
			   int    *NumSteerParams,
			   int    *SteerParamHandles,
			   char  **SteerParamLabels);

/* Emit a status report to be consumed by the steerer.  <NumParams>
   and <ParamHandles> are intended to allow us to tell the steerer
   what we just received from it but this is not currently 
   implemented.  <NumCommands> and <Commands> are intended for the
   same purpose but are also used to send commands such as 'finish'
   (when sim. has finished) back to the steerer. */
static int Emit_status(int   SeqNum,
		       int   NumParams,
		       int  *ParamHandles,
		       int   NumCommands,
		       int  *Commands);

/* Generate a filename (for a status message) from supplied root and 
   internally-generated index */
static int Generate_status_filename(char* filename);

/* Take down connection with steerer and clean-up table entries */
static int Detach_from_steerer();

/* Update the value of the variable pointed to by the pointer
   associated with the supplied param_entry */
static int Update_ptr_value(param_entry *param);

/* Update the 'value' field of the param_entry by retrieving the
   value of the variable pointed to by the pointer associated
   with the entry */
static int Get_ptr_value(param_entry *param);

/* Catch any signals and thus allow the library to clean up if the
   application crashes or is stopped abruptly */
static void Steering_signal_handler(int aSignal);
