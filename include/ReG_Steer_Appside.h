/*----------------------------------------------------------------------------
    Header file defining public routines used in the construction
    of a steering interface for an application.

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

#include "ReG_Steer_types.h"

#ifdef __cplusplus
  #define PREFIX "C"
#else
  #define PREFIX 
#endif


/*-------------- Application-side function prototypes -------------*/

/* Set global flag to enable/disable steering.  This flag is checked
   at start of Steering_control and causes routine to simply return
   if not enabled. */
extern PREFIX void Steering_enable(const int EnableSteer);

/* Initialise the internal library tables and advertise application
   as being steerable. */
extern PREFIX int Steering_initialize(int  NumSupportedCmds,
				      int *SupportedCmds);

/* Register the specified IO types.  This routine performs some sort of
   mapping between the supplied label and a physical destination for
   data (e.g. a filename and location), possibly using the component
   framework. Returns an integer handle for each IO type. */
extern PREFIX int Register_IOTypes(int    NumTypes,
				   char* *IOLabel,
				   int   *direction,
				   int   *IOFrequency,
				   int   *IOType);

/* Register the specified parameters. Since labels are used to 
   identify parameters, they must be unique and not contain
   any white space. */
extern PREFIX int Register_params(int    NumParams,
				  char* *ParamLabels,
				  int   *ParamSteerable,
				  void **ParamPtrs,
				  int   *ParamTypes);

/* Un-register the parameters identified by the given labels. */
extern PREFIX int Unregister_params(int    NumParams,
				    char* *ParamLabels);

/* Main steering routine to be called at application breakpoint.
   Takes <SeqNum> (an indication of application progress) as input
   and returns the labels of any parameters changed by the steerer
   as well as any commands that the app. must handle. This routine
   does not block - if there are no waiting messages from the steerer
   then it returns immediately. */
extern PREFIX int Steering_control(int     SeqNum,
				   int    *NumSteerParams,
				   char*  *SteerParamLabels,
				   int    *NumSteerCommands,
				   int    *SteerCommands,
				   char*  *SteerCmdParams);

/* Open the specified IOType (as returned by a call to Register_IOTypes)
   ready for output. <SeqNum> provides a measure of the application's
   progress at this point. */
extern PREFIX int Emit_start(int  IOType,
			     int  SeqNum,
			     int  UseXDR,
			     int *IOTypeIndex);

/* Must be called following a call to Emit_start.  Emits <Count> items
   of type <DataType> as pointed to by <pData>. */
extern PREFIX int Emit_data_slice(int	            IOTypeIndex,
				  int               DataType,
				  int               Count,
				  void             *pData);

/* Close the specified IOType and complete the emission process. */
extern PREFIX int Emit_stop(int	       *IOTypeIndex);

/* Open the specified IOType (as returned by a call to Register_IOTypes)
   ready for input. */
extern PREFIX int Consume_start(int               IOType,
				int		  *IOTypeIndex);

/* Must be called following a call to Consume_start.  Use to get the type
   and number of data objects in the next 'slice' - allows user to allocate
   sufficient memory for call to Consume_data_slice */
extern PREFIX int Consume_data_slice_header(int  IOTypeIndex,
			                    int *DataType,
			                    int *Count);

/* Must be called following a call to Consume_data_slice_header. <pData> 
   should point to a block of memory large enough to hold <Count> items
   of type <DataType>. */
extern PREFIX int Consume_data_slice(int     IOTypeIndex,
		                     int     DataType,
		                     int     Count,
		                     void   *pData);

/* Close the specified IOType and complete the consumption process. 
   Frees any memory used during the consumption. */
extern PREFIX int Consume_stop(int	       *IOTypeIndex);

/* Called once all steering activity is complete.  Disconnects from
   steerer (if any), removes the 'I am steerable' advertisement and 
   cleans up internal tables. */
extern PREFIX int Steering_finalize();

/* Intended to be called by the application in response to a 'pause'
   command from the steerer.  Blocks until a 'resume' or 'stop' 
   command received.  Returns all commands that immediately followed
   a 'resume' command as well as the labels of any parameters edited
   in that particular message.  (Parameters can be edited while the
   application is paused because this routine continually calls
   Consume_control until it receives 'resume' or 'stop'.) */
extern PREFIX int Steering_pause(int   *NumSteerParams,
				 char **SteerParamLabels,
				 int   *NumCommands,
				 int   *SteerCommands,
				 char **SteerCmdParams);

/* For testing only, generates a buffer containing vtk data plus a 
   suitable ASCII header.  Both header and array should point to 
   big enough chunks of memory (BUFSIZ for header and nx*ny*nz floats
   for array) */
extern PREFIX int Make_vtk_buffer(char  *header,
				  int    nx,
				  int    ny,
				  int    nz,
				  int    veclen,
				  double a,
				  double b,
				  double c,
				  float *array);

