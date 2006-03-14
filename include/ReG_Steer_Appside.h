/*----------------------------------------------------------------------------
  Header file defining public routines used in the construction
  of a steering interface for an application.

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

/** @file ReG_Steer_Appside.h 
    @brief Header file for inclusion in code to be steered 
    @author Andrew Porter
    @author Robert Haines
  */

#include "ReG_Steer_types.h"

#ifdef __cplusplus
  #define PREFIX "C"
#else
  #define PREFIX 
#endif


/*-------------- Application-side function prototypes -------------*/

/**
   Set global flag to enable/disable steering.  This flag is checked
   at start of Steering_control and causes routine to simply return
   if not enabled. By default, the steering library is disabled
   and therefore this routine must be called with @p EnableSteer
   set to REG_TRUE in order to make an application steerable.
 */
extern PREFIX void Steering_enable(const int EnableSteer);

/**
   Initialise the internal library tables and advertise application
   as being steerable. 
   @param AppName Name and version of the user application.  This 
   information is included in the log of checkpoints created and 
   used to check consistency when restarts are requested.
   @param NumSupportedCmds No. of steering commands supported
   @param SupportedCmds Array holding which of the pre-defined 
   supported commands the application supports (e.g. REG_STR_STOP).
   @return REG_SUCCESS or REG_FAILURE
 */
extern PREFIX int Steering_initialize(char *AppName,
				      int   NumSupportedCmds,
				      int  *SupportedCmds);

/**
   Register the specified IO types.  This routine performs some sort of
   mapping between the supplied label and a physical destination for
   data (e.g. a filename and location), possibly using the component
   framework. Returns an integer handle for each IO type. */
extern PREFIX int Register_IOTypes(int    NumTypes,
				   char* *IOLabel,
				   int   *direction,
				   int   *IOFrequency,
				   int   *IOType);

/**
   Register one IO type.  This routine performs some sort of
   mapping between the supplied label and a physical destination for
   data (e.g. a filename and location or IP and port), possibly using 
   the Grid service framework. Returns an integer handle for the IO type. */
extern PREFIX int Register_IOType(char* IOLabel,
				  int   direction,
				  int   IOFrequency,
				  int   *IOType);

extern PREFIX int Enable_IOTypes_on_registration(int toggle);

extern PREFIX int Disable_IOType(int IOType);

extern PREFIX int Enable_IOType(int IOType);

/**
   Turn on use of acknowledgements for this IOType - this means
   that an emitter won't attempt to send a sample until the consumer
   has acknowledged the previous one. 
   Acknowledgements are ON by default.
   @see Disable_IOType_acks
   @ see Enable_IOTypes_on_registration
 */
extern PREFIX int Enable_IOType_acks(int IOType);

/**
   Turn off use of acknowledgements for this IOType - this means
   that an emitter will keep (attempting to) send data samples,
   irrespective of whether the consumer has processed previous ones. 
   Acknowledgements are ON by default. 
   @ see Enable_IOType_acks
   @ see Enable_IOTypes_on_registration
 */
extern PREFIX int Disable_IOType_acks(int IOType);

/**
   Register the listed Checkpoint types. This is for control and
   monitoring purposes - no reference is made to the actual location
   of the checkpoint data 
 */
extern PREFIX int Register_ChkTypes(int    NumTypes,
				    char* *ChkLabel,
				    int   *direction,
				    int   *ChkFrequency,
				    int   *ChkType);

/**
   Register a Checkpoint type. This is for control and
   monitoring purposes - no reference is made to the actual location
   of the checkpoint data */
extern PREFIX int Register_ChkType(char* ChkLabel,
				   int   direction,
				   int   ChkFrequency,
				   int   *ChkType);


/**
   Record that the application has just read/written a checkpoint
   of type ChkType - the handle returned by Register_ChkTypes.  Whether
   it has been read or written is given by the 'direction' of this 
   registered type.  The caller must supply a string in ChkTag which
   will allow them to retrieve the data associated with this checkpoint.
   If the same tag is supplied in multiple calls then the checkpoint is 
   assumed to be have been overwritten (in the case where the 
   'direction' of the associated ChkType is OUT). */
extern PREFIX int Record_Chkpt(int   ChkType,
			       char *ChkTag);

/**
   Record that a file is a member of the checkpoint of
   type ChkType (as returned by Register_ChkTypes).  Should
   be called for each file making up a checkpoint prior to a 
   final call to Record_checkpoint_set to actually record 
   the checkpoint. */
extern PREFIX int Add_checkpoint_file(int   ChkType,
				      char *filename);

/**
   Replacement for Record_Chkpt - to be called by the application once
   it has successfully taken a checkpoint of type ChkType - the handle 
   returned by Register_ChkTypes.  ChkTag should hold a unique
   identifier for the checkpoint set.  If Add_checkpoint_file has
   not been called one or more times prior to this call then ChkTag is 
   used by the library to identify all of the files belonging to the 
   checkpoint.  If one or more calls to Add_checkpoint_file have 
   been made since the last call of this routine then the checkpoint 
   set is recorded as consisting of the file(s) specified in those 
   calls.  That list of files is reset following this call.  Path 
   should hold the location of the checkpoint set, relative to 
   the current working directory. */
extern PREFIX int Record_checkpoint_set(int   ChkType,
					char *ChkTag,
					char *Path);

/** 
    Register the provided list of parameters with the library - this 
    allows a steering client to monitor or, if they are flagged as 
    steerable, to modify them.  The labels must be unique as they are 
    used to identify the parameters.  Also, they must not match the 
    following reserved strings:
    @li `CPU_TIME_PER_STEP';
    @li `SEQUENCE_NUM';
    @li `STEERING_INTERVAL'.

    The first two of these identify monitored parameters automatically 
    generated by the steering library.  The first exposes the amount of 
    CPU time (in seconds) being used between calls of Steering_control 
    (e.g. per simulation time step) and the second, the value of the 
    `sequence number' supplied in the call to Steering_control. The 
    third reserved label identifies a steerable parameter that allows the 
    frequency with which Steering_control actually contacts the steering 
    client to be adjusted.  (e.g. A value of ten corresponds to the steering 
    client being contacted once in every ten calls.)

    This routine may be called more than once and at any time (whether or 
    not Steering_initialize has successfully initiated a connection with 
    a steering client).  The steering library's internal tables of 
    parameters remains unchanged by the (dis)connection of a steering client.

    Multiple calls to this routine have a cumulative effect.  That is, 
    every successful call to this routine results in the addition of the 
    specified parameters to the table of registered parameters within the 
    library.
    
    The pointers in @p ParamPtrs may be to variables of different types (since 
    this routine is only available in C and C++).  The type of each 
    parameter is specified by the @p ParamTypes array using the coding scheme 
     defined within the ReG_Steer_types.h header file, c.f. MPI.)

    The user may specify a range of validity for steerable parameters
    consisting of one or both of a maximum and minimum value.  These
    values are specified by the strings pointed to by the elements of
    the @p ParamMinima and @p ParamMaxima arrays.  If any limit (minimum,
    maximum or both) is inapplicable, the user should supply a blank
    or empty string (` ' or `').  If the parameter being
    registered is a (steerable) string then the corresponding
    ParamMaxima gives the maximum length that the string can take.
    This may then be enforced by a steering client.  The upper bound on
    the length of a string registered with the library is
    REG_MAX_STRING_LENGTH characters.

    Although this routine is flexible, in practice it requires that a 
    user set up arrays for the various quantities being passed to it.  It 
    is also not possible to provide a binding for this routine in F90.  
    As a consequence of these issues, the API also provides a singular 
    form of the parameter registration routine, Register_param.
    @param NumParams Number of params to register
    @param ParamLabels Label for each parameter
    @param ParamSteerable REG_TRUE if parameter steerable, REG_FALSE 
    otherwise. Since the quantities being registered with the steering library
    are pointers, it is a requirement that the variables to which they refer 
    remain in scope through multiple calls to Steering_control (i.e. 
    until Steering_finalize is called).
    @param ParamPtrs Pointer to parameter value
    @param ParamTypes Data type of parameter
    @param ParamMinima Min. parameter value
    @param ParamMaxima Max. parameter value
    @return REG_SUCCESS or REG_FAILURE
    @see Register_param
*/
extern PREFIX int Register_params(int    NumParams,
				  char* *ParamLabels,
				  int   *ParamSteerable,
				  void **ParamPtrs,
				  int   *ParamTypes,
				  char* *ParamMinima,
				  char* *ParamMaxima);

/**
   Singular form of Register_params. Removes the need for
   arrays of parameter labels etc. 
   @see Register_params */
extern PREFIX int Register_param(char* ParamLabel,
                                 int   ParamSteerable,
                                 void *ParamPtr,
                                 int   ParamType,
                                 char* ParamMinimum,
                                 char* ParamMaximum);

/**
   A wrapper for Register_param for variables of type
   REG_BIN.  Calculates number of bytes that ParamPtr
   points at.  Mandates that REG_BIN variables are
   monitored only. */
extern PREFIX int Register_bin_param(char *ParamLabel, 
				     void *ParamPtr,
				     int ParamType, 
				     int NumObjects);

/**
   Un-register the parameters identified by the given labels. */
extern PREFIX int Unregister_params(int    NumParams,
				    char* *ParamLabels);
/**
   Toggle whether (toggle=REG_TRUE) or not (toggle=REG_FALSE) to 
   log values of all registered  parameters. Logging is on by
   default. */
extern PREFIX int Enable_all_param_logging(int toggle);

/** 
   Enable logging of values of the parameter identified by the
   provided label. Logging is on by default. */
extern PREFIX int Enable_param_logging(char *ParamLabel);

/** 
   Disable logging of values of the parameter identified by the
   provided label. Logging is on by default. */
extern PREFIX int Disable_param_logging(char *ParamLabel);

/**
   Main steering routine to be called at application breakpoint.
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

/**
   Open the specified IOType (as returned by a call to Register_IOTypes)
   ready for output. <SeqNum> provides a measure of the application's
   progress at this point. 
   @see Register_IOTypes
 */
extern PREFIX int Emit_start(int  IOType,
			     int  SeqNum,
			     int *IOTypeIndex);

/**
   Blocking version of Emit_start.  Blocks until IOType is ready to
   send data OR the specified TimeOut (seconds) is exceeded. */
extern PREFIX int Emit_start_blocking(int    IOType,
				      int    SeqNum,
				      int   *IOTypeIndex,
				      float  TimeOut);

/** 
   Must be called following a call to Emit_start.  Emits <Count> items
   of type <DataType> as pointed to by <pData>. */
extern PREFIX int Emit_data_slice(int	            IOTypeIndex,
				  int               DataType,
				  int               Count,
				  const void       *pData);

/** 
   Close the specified IOType and complete the emission process. */
extern PREFIX int Emit_stop(int	       *IOTypeIndex);

/**
   Open the specified IOType (as returned by a call to Register_IOTypes)
   ready for input. 
   @see Register_IOTypes */
extern PREFIX int Consume_start(int               IOType,
				int		  *IOTypeIndex);

/**
   Blocking version of above routine.  Blocks until data is available to
   read OR TimeOut (seconds) is exceeded */
extern PREFIX int Consume_start_blocking(int   IOType,
					 int  *IOTypeIndex,
					 float TimeOut);

/**
   Must be called following a call to Consume_start.  Use to get the type
   and number of data objects in the next 'slice' - allows user to allocate
   sufficient memory for call to Consume_data_slice */
extern PREFIX int Consume_data_slice_header(int  IOTypeIndex,
			                    int *DataType,
			                    int *Count);

/**
   Must be called following a call to Consume_data_slice_header. <pData> 
   should point to a block of memory large enough to hold <Count> items
   of type <DataType>. */
extern PREFIX int Consume_data_slice(int     IOTypeIndex,
		                     int     DataType,
		                     int     Count,
		                     void   *pData);

/**
   Close the specified IOType and complete the consumption process. 
   Frees any memory used during the consumption. */
extern PREFIX int Consume_stop(int	       *IOTypeIndex);

/**
   Called once all steering activity is complete. Close any existing 
   connection to a steering client and clear the internal library 
   tables of registered parameters and sample-data/checkpoint types.  
   The steering library reverts to the state it had prior to the original 
   call to Steering_initialize. Removes the 'I am steerable' advertisement
   and thus the application cannot be steered following this call (unless 
   a further call to Steering_initialize is made).
   @return REG_SUCCESS or REG_FAILURE */
extern PREFIX int Steering_finalize();

/** 
   Intended to be called by the application in response to a 'pause'
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

/**
   Two utility routines to allow the user to not have to worry about
   allocating arrays of strings to be passed to Steering_control.  This
   routine allocates an array containing @p Array_len strings, each of
   length @p String_len.  Intended to aid in the creation of the arrays 
   needed by e.g. Steering_control.  The memory allocated by a call to 
   this routine is managed and ultimately free'd by the steering library 
   (during the call to Steering_finalize).
   @return NULL if mallocs fail, otherwise pointer to array of char* */
extern PREFIX char **Alloc_string_array(int String_len,
					int Array_len);

/**
   This frees ALL of the string arrays allocated by (possibly several)
   calls to Alloc_string_array.  Returns REG_SUCCESS. */
extern PREFIX int Free_string_arrays();

/**
   For testing only, generates a buffer containing vtk data plus a 
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

/**
   Create a vtk header for a structured points data set of nx*ny*nz
   points of type <type> (coded as REG_INT etc.) */
extern PREFIX int Make_vtk_header(char  *header,
				  char  *title,
				  int    nx,
				  int    ny,
				  int    nz,
				  int    veclen,
				  int    type);

/**
   Create a simple header for a data chunk - simply gives origin and
   extent of a 3D chunk of a larger data set */
extern PREFIX int Make_chunk_header(char *header,
				    int   IOindex,
				    int   totx, int toty, int totz,
				    int   sx,   int sy,   int sz,
				    int   nx,   int ny,   int nz);

/**
   Toggle whether or not (flag = 1 or 0) the data to be emitted on the 
   IOType with the given index is from Fortran */
extern PREFIX int Set_f90_array_ordering(int IOTypeIndex,
				         int flag);

/**
   Flag that library (flag = REG_TRUE) is being called from F90 code. */
extern PREFIX int Called_from_f90(int flag);

/**
   Reorder array pointed to by pInData into array pointed to by
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
