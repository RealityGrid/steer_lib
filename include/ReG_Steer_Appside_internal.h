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

#include "ReG_Steer_Common.h"
#include "ReG_Steer_XML.h"

/*--------- Prototypes of internal library functions -------------*/

/* Emit all of the parameters that have previously been registered 
   (if any). */
static int Emit_param_defs();

/* Emit all of the IO types that have previously been registered
   (if any). */
static int Emit_IOType_defs();

/* Consume a control message (if any present) from the steerer. Returns
   any commands that the application must deal with as well as the handles
   and labels of any (steerable) parameters whose values have changed.
   (The values themselves are updated from within this routine.) */
static int Consume_control(int    *NumCommands,
			   int    *Commands,
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
