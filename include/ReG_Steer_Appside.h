/*----------------------------------------------------------------------------
  Header file defining public routines used in the construction
  of a steering interface for an application.

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
extern PREFIX int Steering_initialize(char *AppName,
				      int   NumSupportedCmds,
				      int  *SupportedCmds);

/* Register the specified IO types.  This routine performs some sort of
   mapping between the supplied label and a physical destination for
   data (e.g. a filename and location), possibly using the component
   framework. Returns an integer handle for each IO type. */
extern PREFIX int Register_IOTypes(int    NumTypes,
				   char* *IOLabel,
				   int   *direction,
				   int   *IOFrequency,
				   int   *IOType);

extern PREFIX int Enable_IOTypes_on_registration(int toggle);

extern PREFIX int Disable_IOType(int IOType);

extern PREFIX int Enable_IOType(int IOType);

/* Register the listed Checkpoint types. This is for control and
   monitoring purposes - no reference is made to the actual location
   of the checkpoint data */
extern PREFIX int Register_ChkTypes(int    NumTypes,
				    char* *ChkLabel,
				    int   *direction,
				    int   *ChkFrequency,
				    int   *ChkType);

/* Record that the application has just read/written a checkpoint
   of type ChkType - the handle returned by Register_ChkTypes.  Whether
   it has been read or written is given by the 'direction' of this 
   registered type.  The caller must supply a string in ChkTag which
   will allow them to retrieve the data associated with this checkpoint.
   If the same tag is supplied in multiple calls then the checkpoint is 
   assumed to be have been overwritten (in the case where the 
   'direction' of the associated ChkType is OUT). */
extern PREFIX int Record_Chkpt(int   ChkType,
			       char *ChkTag);

/* Record that a file is a member of the checkpoint of
   type ChkType (as returned by Register_ChkTypes).  Should
   be called for each file making up a checkpoint prior to a 
   final call to Record_checkpoint_set to actually record 
   the checkpoint. */
extern PREFIX int Record_checkpoint_file(int   ChkType,
					 char *filename);

/* Replacement for Record_Chkpt - to be called by the application once
   it has successfully taken a checkpoint of type ChkType - the handle 
   returned by Register_ChkTypes.  ChkTag should hold a unique
   identifier for the checkpoint set.  If Record_checkpoint_file has
   not been called one or more times prior to this call then ChkTag is 
   used by the library to identify all of the files belonging to the 
   checkpoint.  If one or more calls to Record_checkpoint_file have 
   been made since the last call of this routine then the checkpoint 
   set is recorded as consisting of the file(s) specified in those 
   calls.  That list of files is reset following this call.  Path 
   should hold the location of the checkpoint set, relative to 
   the current working directory. */
extern PREFIX int Record_checkpoint_set(int   ChkType,
					char *ChkTag,
					char *Path);

/* Register the specified parameters. Since labels are used to 
   identify parameters, they must be unique and not contain
   any white space. */
extern PREFIX int Register_params(int    NumParams,
				  char* *ParamLabels,
				  int   *ParamSteerable,
				  void **ParamPtrs,
				  int   *ParamTypes,
				  char* *ParamMinima,
				  char* *ParamMaxima);

/* As above but for a single parameter to remove the need for
   arrays of parameter labels etc. */
extern PREFIX int Register_param(char* ParamLabel,
                                 int   ParamSteerable,
                                 void *ParamPtr,
                                 int   ParamType,
                                 char* ParamMinimum,
                                 char* ParamMaximum);

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
			     int *IOTypeIndex);

/* Must be called following a call to Emit_start.  Emits <Count> items
   of type <DataType> as pointed to by <pData>. */
extern PREFIX int Emit_data_slice(int	            IOTypeIndex,
				  int               DataType,
				  int               Count,
				  const void       *pData);

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
extern PREFIX int Make_vtk_buffer(int    nx,
				  int    ny,
				  int    nz,
				  int    veclen,
				  double a,
				  double b,
				  double c,
				  float *array);

/* Create a vtk header for a structured points data set of nx*ny*nz
   points of type <type> (coded as REG_INT etc.) */
extern PREFIX int Make_vtk_header(char  *header,
				  char  *title,
				  int    nx,
				  int    ny,
				  int    nz,
				  int    veclen,
				  int    type);

/* Create a simple header for a data chunk - simply gives origin and
   extent of a 3D chunk of a larger data set */
extern PREFIX int Make_chunk_header(char *header,
				    int   IOindex,
				    int   totx, int toty, int totz,
				    int   sx,   int sy,   int sz,
				    int   nx,   int ny,   int nz);

/* Toggle whether or not (flag = 1 or 0) the data to be emitted on the 
   IOType with the given index is from Fortran */
extern PREFIX int Set_f90_array_ordering(int IOTypeIndex,
				         int flag);

/* Flag that library (flag = TRUE) is being called from F90 code. */
extern PREFIX int Called_from_f90(int flag);

/* Reorder array pointed to by pInData into array pointed to by
   pOutData (must be of dimension tot_extent[0]*tot_extent[1]*tot_extent[2])
   If to_f90 == 1 then reorders from C to F90, otherwise, F90 to C */
extern PREFIX int Reorder_array(int          ndims,
				int         *tot_extent,
				int         *sub_extent,
				int         *origin,
				int          type,
				void        *pInData,
				void        *pOutData,
				int          to_f90);
