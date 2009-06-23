/*----------------------------------------------------------------------------
  (C) Copyright 2006, University of Manchester, United Kingdom,
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

    Header file defining public routines used in the construction
    of a steering interface for an application.

    @author Andrew Porter
    @author Robert Haines
  */

#include "ReG_Steer_Config.h"
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
   In order to enable the generic steering client to control the
   emission and consumption of different types of sample data, the
   steered application must register the different sample-data types
   that it supports. Frequencies are interpreted as the number of
   steps (calls of Steering_control()) between IO activity.  A
   frequency of zero for a given type thus disables the automatic
   consumption/emission of data for that IO type.

   This routine is cumulative in its operation in that the IOTypes
   passed to it are appended to the internal list of any IOTypes
   registered in previous calls.

   It is assumed that the mapping from IOLabel to a source/sink is
   performed by the steering library using some Web Service framework.
   However, in the RealityGrid implementation, if file-based IOTypes
   are being used then the label is taken as the basis for the
   filenames to be produced: any spaces in the label are replaced by
   underscores and '_@p SeqNum' is appended (where @p SeqNum is
   supplied in the call to Emit_start()).

   Register the specified IO types.  This routine performs some sort of
   mapping between the supplied label and a physical destination for
   data (@e e.g. a filename and location), possibly using the component
   framework. Returns an integer handle for each IO type. 
   @param NumTypes The number of IOTypes to register
   @param IOLabel Array of labels for the IOTypes
   @param direction Array giving direction of each IOType (@c IN, 
   @c OUT or @c INOUT)
   @param IOFrequency Array giving the interval between 
   automatically-generated commands to emit/consume data using each IOType
   @param IOType On success - array of handles for registered IOTypes
   @return REG_SUCCESS, REG_FAILURE
   @see Register_IOType()
*/
extern PREFIX int Register_IOTypes(int    NumTypes,
				   char* *IOLabel,
				   int   *direction,
				   int   *IOFrequency,
				   int   *IOType);

/**
   Register a single IOType. Behaves exactly as Register_IOTypes() but
   does not require arrays of variables as arguments. 
 */
extern PREFIX int Register_IOType(char* IOLabel,
				  int   direction,
				  int   IOFrequency,
				  int   *IOType);

/**
   Toggle whether (@p toggle = @c REG_TRUE) or not (@p toggle = @c
   REG_FALSE) to enable IOTypes during calls to the family of
   Register_IOType() routines (the default behaviour is for them to be
   enabled).  Intended for use with sockets-based IOTypes in which
   case 'enabling' corresponds to the creation of the socket.
*/
extern PREFIX int Enable_IOTypes_on_registration(int toggle);

/**
   Disable a previously registered (and enabled) IOType.  If the
   steering library is built to use sockets-based communications for
   IOTypes then this corresponds to shutting-down the socket
   associated with the IOType.
   @param IOType The handle of the IOType to disable
   @return REG_SUCCESS, REG_FAILURE
*/
extern PREFIX int Disable_IOType(int IOType);

/** 
    Enables a previously disabled IOType (such as one produced by
    Register_IOTypes() after an earlier call of
    Enable_IOTypes_on_registration(toggle) with @p toggle= @c
    REG_FALSE).  For a socket-based IOType this corresponds to
    creation of the socket.  Details of the IOType's endpoints are
    (re-)obtained from the underlying Web-Service framework during
    this call to allow for component creation and destruction.
    @param IOType The handle of the IOType to disable
    @return REG_SUCCESS, REG_FAILURE
*/
extern PREFIX int Enable_IOType(int IOType);

/**
   Turn on use of acknowledgements for the specified IOType. When
   acknowledgements are enabled, calls to Emit_start() will return
   REG_NOT_READY until an acknowledgement of the last data set emitted
   has been received from the consumer
.  @e N.B. acknowledgements are ON by default.  
   @see Disable_IOType_acks(), Enable_IOTypes_on_registration()
 */
extern PREFIX int Enable_IOType_acks(int IOType);

/**
   Turn off use of acknowledgements for the specified IOType.  When
   acknowledgements are disabled, the library will attempt to emit a
   data set, irrespective of whether or not the consumer has
   acknowledged the previous one. Note that when socket-based IO is
   used this can result in the emitter blocking if the consumer is
   unable to keep up (@e e.g. if the consumer's processing of a data set
   is taking longer than the interval between the generation of data
   sets by the emitter).
   @e N.B. acknowledgements are ON by default.  
   @see Enable_IOType_acks(), Enable_IOTypes_on_registration()
 */
extern PREFIX int Disable_IOType_acks(int IOType);

/**
   @param NumTypes No. of checkpoint types to register
   @param ChkLabel Unique label for each Chk type
   @param direction @c IN, @c OUT or @c INOUT for each Chk type
   @param ChkFrequency Initial value of frequency of checkpoint 
   creation (ignored for ChkTypes with direction @c IN)
   @param ChkType On successful return, holds identifier(s) of the
   registered Chk type(s)

   Register the listed Checkpoint types for control and monitoring
   purposes - no reference is made to the actual location of the
   checkpoint data. The direction parameter specifies whether the
   application can create (@c OUT), restart from (@c IN) or both
   create and restart from (@c INOUT) the given checkpoint type.
   ChkFrequency is interpreted in the same way as for
   Register_IOTypes() and is only applied to the creation of
   checkpoints (@e i.e. those with a direction of @c OUT or @c INOUT).
   This routine is cumulative in its operation in that the ChkTypes
   passed to it are appended to the internal list of any ChkTypes
   registered in previous calls.
 */
extern PREFIX int Register_ChkTypes(int    NumTypes,
				    char* *ChkLabel,
				    int   *direction,
				    int   *ChkFrequency,
				    int   *ChkType);

/**
   Identical in operation to Register_ChkTypes() with the obvious
   exception that they only register one ChkType at a time and
   therefore do not require arrays of variables as arguments. 
*/
extern PREFIX int Register_ChkType(char* ChkLabel,
				   int   direction,
				   int   ChkFrequency,
				   int   *ChkType);


/**
   OBSOLETE - replaced by Record_checkpoint_set() @n
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
   @param ChkType Which checkpoint type to add this file to
   @param filename The name of the file to add (excluding path)

   Intended to tell the steering library about each file making up a
   checkpoint of type @p ChkType (the handle created by a call to
   Register_ChkTypes()). The filename argument should give the name of
   a file to be added to the list of files making up the current
   checkpoint; it should not contain any path information. Each call
   to this routine adds the named file to an internal list of files
   for the current checkpoint.  Once this list is complete, the
   application should call Record_checkpoint_set() (where a path to the
   files may be specified if required). Calling Record_checkpoint_set()
   wipes the internal list of files for the specified @p ChkType, ready
   for the creation of the next checkpoint.
*/
extern PREFIX int Add_checkpoint_file(int   ChkType,
				      char *filename);

/**
   @param ChkType The type of checkpoint that is being recorder (as 
   returned by Register_ChkTypes()).
   @param ChkTag Specifies a unique string identifying this checkpoint instance
   @param Path Specifies the location of the files making up the 
   checkpoint set, relative to the current working directory
   @return REG_SUCCESS, REG_FAILURE

   Replacement for Record_Chkpt() - to be called by the application
   once it has successfully taken a checkpoint of type @p ChkType.  If
   Add_checkpoint_file() has not been called one or more times prior
   to this call then @p ChkTag is used by the library to identify all
   of the files belonging to the checkpoint (@e i.e. searching ./@p Path
   for files with @p ChkTag in their names will return a list of all the
   files constituting the checkpoint).  If one or more calls to
   Add_checkpoint_file() have been made since the last call of this
   routine then the checkpoint set is recorded as consisting of the
   file(s) specified in those calls.  That list of files is reset
   following this call.
*/
extern PREFIX int Record_checkpoint_set(int   ChkType,
					char *ChkTag,
					char *Path);

/**
    @param NumParams Number of params to register
    @param ParamLabels Label for each parameter
    @param ParamSteerable @c REG_TRUE if parameter steerable, @c REG_FALSE 
    otherwise. Since the quantities being registered with the steering library
    are pointers, it is a requirement that the variables to which they refer 
    remain in scope through multiple calls to Steering_control (@e i.e. 
    until Steering_finalize() is called).
    @param ParamPtrs Pointer to parameter value
    @param ParamTypes Data type of parameter
    @param ParamMinima Min. parameter value
    @param ParamMaxima Max. parameter value
    @return REG_SUCCESS or REG_FAILURE

    Register the provided list of parameters with the library - this 
    allows a steering client to monitor or, if they are flagged as 
    steerable, to modify them.  The labels must be unique as they are 
    used to identify the parameters.  Also, they must not match the 
    following reserved strings:
    @li 'CPU_TIME_PER_STEP';
    @li 'SEQUENCE_NUM';
    @li 'STEERING_INTERVAL'.

    The first two of these identify monitored parameters automatically
    generated by the steering library.  The first exposes the amount
    of CPU time (in seconds) being used between calls of
    Steering_control() (@e e.g. per simulation time step) and the
    second, the value of the 'sequence number' supplied in the call to
    Steering_control. The third reserved label identifies a steerable
    parameter that allows the frequency with which Steering_control
    actually contacts the steering client to be adjusted.  (@e e.g. A
    value of ten corresponds to the steering client being contacted
    once in every ten calls.)

    This routine may be called more than once and at any time (whether or 
    not Steering_initialize() has successfully initiated a connection with 
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
    or empty string (' ' or '').  If the parameter being
    registered is a (steerable) string then the corresponding
    @p ParamMaxima gives the maximum length that the string can take.
    This may then be enforced by a steering client.  The upper bound on
    the length of a string registered with the library is
    @c REG_MAX_STRING_LENGTH characters.

    Although this routine is flexible, in practice it requires that a 
    user set up arrays for the various quantities being passed to it.  It 
    is also not possible to provide a binding for this routine in F90.  
    As a consequence of these issues, the API also provides a singular 
    form of the parameter registration routine, Register_param().
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
   Singular form of Register_params(). Removes the need for
   arrays of parameter labels @e etc. 
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
   Un-register the parameters identified by the given labels.
   NOT CURRENTLY IMPLEMENTED. 
   @param NumParams Number of parameters to unregister
   @param ParamLabels Array of parameter labels */
extern PREFIX int Unregister_params(int    NumParams,
				    char* *ParamLabels);
/**
   Toggle whether (@p toggle = @c REG_TRUE) or not 
   (@p toggle = @c REG_FALSE) to 
   log values of all registered  parameters. Logging is on by
   default for all parameters except those of
   type @c REG_BIN. */
extern PREFIX int Enable_all_param_logging(int toggle);

/** 
   Enable logging of values of a parameter (which must have been
   previously registered with the steering library).  When logging of
   a parameter is enabled, all values of that parameter (as captured
   when Steering_control() is called) are stored and can be accessed
   by a steering client.  When using file-based steering, these values
   are cached to disk and are stored in
   \$REG_STEER_DIRECTORY/ReG_params_log.dat.  This file is moved to
   one side and stored as ReG_params_log.dat.backup by any subsequent
   steering-enabled program that is using the same
   \$REG_STEER_DIRECTORY.

   When using WS-based steering, the parameter values are cached on
   the WS itself and are retrieved from it by the library (when requested)
   using the @c GetParamLog method of the SWS.
   @e N.B. logging is on by default for all parameters except those of
   type @c REG_BIN. 
   @param ParamLabel The label of the parameter for which to enable logging 
   @see Disable_param_logging() */
extern PREFIX int Enable_param_logging(char *ParamLabel);

/** 
   Disable logging of values of a parameter.
   @e N.B. Logging is on by default for all parameters except those of
   type @c REG_BIN. 
   @param ParamLabel The label of the parameter for which to disable logging 
   @see Enable_param_logging() */
extern PREFIX int Disable_param_logging(char *ParamLabel);

/**
   @param SeqNum An indication of the calling application's progress
   @param NumSteerParams No. of parameters that have been steered by client
   @param SteerParamLabels The labels of parameters that have been steered
   @param NumSteerCommands No. of steering commands received from client
   @param SteerCommands List of commands received from client
   @param SteerCmdParams List of params associated with each command 
   from client
   @return REG_SUCCESS, REG_FAILURE

   Main steering routine to be called at the application breakpoint (as 
   described in @ref sec_background). It performs the following actions:

   @li checks for and enables a steering client to connect if the 
   application is not already being steered;
   @li retrieves any (new) values for the steerable parameters from an 
   attached steering client and updates the associated simulation 
   variables via the pointers supplied at their registration;
   @li retrieves any commands from an attached steering client - those that 
   must be handled by the application itself are returned as a list (encoded 
   as integers) in @p SteerCommands.  It is the application's responsibility 
   to deal with these commands in the order in which they occur in the list;
   @li reports the current values of all registered parameters (both 
   monitored and steered) to the steering client.

   This routine (internally) handles attach and detach requests from
   the steering client.  Since there may be performance implications
   associated with checking whether a client is attempting to attach,
   the frequency with which this is done is configurable by means of
   the REG_APP_POLL_INTERVAL environment variable.  This may be used
   to override the default minimum time interval (in seconds) between
   checks on whether a steering client has connected.  If it is set to
   zero then a check is performed on every call to Steering_control().
   If the environment variable is not set then a default value (set in
   ReG_Steer_types.h) is used.

   Steering_control() returns a list of labels identifying which of
   the steered parameters have been changed by the steering
   client. (The values themselves will have been updated using the
   pointers previously registered with the library.)

   The commands returned by this routine may include pre-defined
   commands (such as 'stop') as well as commands to emit/consume a
   sample (of type @p IOType) or create/restart from a checkpoint (of
   type @p ChkType).  The latter are indicated by returning the value
   of an IOType or Chktype (as returned by a previous call to
   Register_IOTypes() or Register_ChkTypes(), respectively) in the @p
   SteerCommands array.

   Certain commands may require the supply of additional information
   in order to enable the application to execute them.  This
   information is supplied in the form of a character string for each
   command.  The only command that currently makes use of this is the
   instruction to checkpoint - the associated parameter string then
   contains either 'IN' plus the tag of the checkpoint (see
   Record_checkpoint_set()) or 'OUT' according to whether a checkpoint
   is to be created or a restart performed.

   Unless the application has specified (by giving
   REG_STR_PAUSE_INTERNAL as one of its supported commands in its call
   to Steering_initialize()) that it wants the steering library to
   implement the pause command for it, this routine does not block -
   if there are no waiting messages from the steerer then it returns
   immediately. 
*/
extern PREFIX int Steering_control(int     SeqNum,
				   int    *NumSteerParams,
				   char*  *SteerParamLabels,
				   int    *NumSteerCommands,
				   int    *SteerCommands,
				   char*  *SteerCmdParams);

/**
   A call to 'open' an IOType ready to emit a data set.  The channel
   to open is identified by IOType, which must have been obtained from
   a prior call to Register_IOTypes() and be of type @c OUT.  It is assumed
   that Emit_start() can determine the destination associated with this
   particular type of sample data, @e e.g. by interrogating a Web-Services
   framework.  If a call to this routine returns @c REG_FAILURE then this
   signifies that the IO channel is not connected and therefore no
   data can be emitted.  This will typically occur when sockets-based
   IO is being used and the component intended to receive this data
   has yet to establish a connection. A return value of @c REG_NOT_READY
   indicates that no acknowledgement of the last data set that was
   (successfully) emitted has been received from the consumer of this
   IOType. (The use of acknowledgements for a given IOType may be
   disabled by calling Disable_IOType_acks().)
   @param IOType The IOType to use to emit data
   @param SeqNum Provides a measure of the application's
   progress at this point. 
   @param IOTypeIndex On success, holds an index to the open IOType
   @return REG_SUCCESS, REG_NOT_READY, REG_FAILURE
   @see Emit_start_blocking(), Emit_data_slice(), Register_IOTypes
 */
extern PREFIX int Emit_start(int  IOType,
			     int  SeqNum,
			     int *IOTypeIndex);

/**
   Blocking version of Emit_start().  Blocks until IOType is ready to
   send data OR the specified @p TimeOut (seconds) is exceeded. */
extern PREFIX int Emit_start_blocking(int    IOType,
				      int    SeqNum,
				      int   *IOTypeIndex,
				      float  TimeOut);

/** 
   @param IOTypeIndex is the handle returned by a prior call to Emit_start()
   @param DataType should be specified using the coding scheme given in 
   ReG_Steer_types.h (@e e.g. @c REG_INT)
   @param Count The number of objects of type @p DataType to emit
   @param pData Pointer to the data to emit
   @return REG_SUCCESS, REG_FAILURE

   Having called Emit_start(), an application should emit the various 
   pieces of a data set by successive calls to this routine which wraps 
   the low-level IO necessary for emitting the sample data.  It will 
   BLOCK until either the requested amount of data has been emitted or 
   an error occurs. 

   The application programmer is responsible for collecting sample
   data (in portions if necessary) and passing it to this routine.
   They are also responsible for reconstructing/identifying it in the
   downstream component.  An example of the structure of a routine to
   emit sample data as might be written by the application scientist
   is given below.  This routine is called by the application in
   response to instructions returned by Steering_control. It is
   assumed that the MPI process with rank 0 is the process that calls
   Steering_initialize(), Steering_control(), @e etc.

   @code
   int Emit_sample(int IOType, int SeqNum
                   \<, + any other required arguments \>){

     int irank;
     int count;
     int nprocs;
     int *data_ptr;
     REG_SampleHandleType SampleHandle;

     MPI_Comm_rank(MPI_COMM_WORLD, &irank);
     MPI_Comm_size(MPI_COMM_WORLD, &nprocs);

     if(irank == 0){

       // IOType corresponds to a previously registered sample
       // type
       Emit_start(IOType, SeqNum, &SampleHandle);
     }

     for(iproc=0; iproc<nprocs; iproc++){

       MPI_Barrier(MPI_COMM_WORLD);

       if(irank != 0 && irank == iproc){

         // Some code to get data_ptr and the quantity of data
         // (count) goes here...

	 MPI_Send(&count, 1, MPI_INT, 0, Some_Tag, MPI_COMM_WORLD);
	 MPI_Send(data_ptr, count, MPI_DOUBLE, 0, Some_Tag,
                  MPI_COMM_WORLD);
       }
       else if(irank == 0){

	 // Collect data on master process and then emit it...

	 if(iproc != 0){

	   MPI_Recv(&count, 1, MPI_INT, iproc, MPI_ANY_TAG, 
                    MPI_COMM_WORLD, &status);
	    
	   // Allocate memory if required 

	   MPI_Recv(pdata, count, MPI_DOUBLE, iproc, MPI_ANY_TAG, 
 		    MPI_COMM_WORLD, &status);
	 }
	 else{

           // Some code to get pdata and quantity of data (count)
	   // on process 0 goes here
	 }

	 Emit_data_slice(sampleHandle, REG_DBL, count, pdata);
       }

     }

     if(irank == 0){
       Emit_stop(sampleHandle);
     }

     return 0;
   }
   @endcode
*/
extern PREFIX int Emit_data_slice(int	            IOTypeIndex,
				  int               DataType,
				  int               Count,
				  const void       *pData);

/** 
   Signal the end of the emission of the sample/data set referred to by
   IOHandle.   This signals the receiving end that the
   transmission is complete - @e i.e. that they have all
   of the 'slices' constituting the data set.  Close the specified
   IOType and complete the emission process.
   @param IOTypeIndex Index of the open IOType as returned by 
   Emit_start().  @p IOTypeIndex is not a valid handle once
   this call has completed.
   @return REG_SUCCESS, REG_FAILURE
*/
extern PREFIX int Emit_stop(int	       *IOTypeIndex);

/**
   The equivalent of Emit_start() for input - a call to 'open' an IO
   channel ready to receive a data set.  The channel to open is
   identified by @p IOType, which must have been obtained from a prior
   call to Register_IO_types() and be of type @c IN.  It is assumed that
   this routine can determine the source associated with this
   particular type of sample data, @e e.g. by interrogating the Web Service
   framework.  If this call returns @c REG_FAILURE then there is
   currently no data available on the channel (which might be because
   the channel is not connected to a data source). Note that in
   calling this routine, the application signals the data source that
   it is ready for new data - @e i.e. if Consume_stop() has been called
   successfully for a previous data set then a call to Consume_start
   results in an acknowledgement of that data set being sent to the
   emitter.
   @param IOType The IOType channel to open
   @param IOTypeIndex On successful return, holds index of open channel
   @return REG_SUCCESS, REG_FAILURE
   @see Register_IOTypes()
*/
extern PREFIX int Consume_start(int               IOType,
				int		  *IOTypeIndex);

/**
   @return REG_SUCCESS, REG_TIMED_OUT
   Blocking version of Consume_start().  Blocks until data is available to
   read OR @p TimeOut (seconds) is exceeded 
*/
extern PREFIX int Consume_start_blocking(int   IOType,
					 int  *IOTypeIndex,
					 float TimeOut);

/**
   @param IOTypeIndex The index returned from call to Consume_start() 
   - identifies the IO channel to be read.
   @param DataType On success, the type of the data that is available 
   to read, as encoded in ReG_Steer_types.h.
   @param Count On success, the number of data elements (e.g. integers) 
   forming the next slice - it is NOT the number of bytes.
   @return REG_SUCCESS, REG_FAILURE

   Must only be called once a successful call to Consume_start() has
   indicated that data is available for consumption.  Use to get the
   type and number of data objects in the next 'slice' - allows user
   to allocate sufficient memory for call to Consume_data_slice. This
   routine blocks until it receives a header describing the next data
   slice to be read or end-of-message.  If end-of-message is received
   then this routine returns @c REG_FAILURE. 
*/
extern PREFIX int Consume_data_slice_header(int  IOTypeIndex,
			                    int *DataType,
			                    int *Count);

/**
   @param IOTypeIndex The index returned from call to 
   Consume_start() - identifies the IO channel to be read.
   @param DataType The type of the data to read
   @param Count The number of objects of type @p DataType to read
   @param pData Pointer to array large enough to hold incoming data
   @return REG_SUCCESS, REG_FAILURE

   Wraps the low-level IO for receiving sample data from another
   component. This routine BLOCKS until the requested amount of data
   has been consumed. The type of data to consume is specified using
   the coding scheme given in ReG_Steer_types.h.  The amount and type
   of the data to consume is obtained from a prior call to
   Consume_data_slice_header().
*/
extern PREFIX int Consume_data_slice(int     IOTypeIndex,
		                     int     DataType,
		                     int     Count,
		                     void   *pData);

/**
   @param IOTypeIndex Index of the open IOType channel to close.  Not
   valid once this call has completed.  
   @return REG_SUCCESS, REG_FAILURE

   Should be called after a call to Consume_data_slice_header() has
   returned @c REG_FAILURE indicating that there are no more 'slices'
   to read for the current data set.  It signals the end of the
   consumption of the sample/data set referred to by @p
   IOTypeIndex. Frees any memory used during the consumption. This
   routine should be called by the same thread that made the
   corresponding call to Consume_start.
*/
extern PREFIX int Consume_stop(int	       *IOTypeIndex);

/**
   Called once all steering activity is complete. Close any existing 
   connection to a steering client and clear the internal library 
   tables of registered parameters and sample-data/checkpoint types.  
   The steering library reverts to the state it had prior to the original 
   call to Steering_initialize(). Removes the 'I am steerable' advertisement
   and thus the application cannot be steered following this call (unless 
   a further call to Steering_initialize is made).
   @return REG_SUCCESS or REG_FAILURE */
extern PREFIX int Steering_finalize();

/** 
   @return REG_SUCCESS, REG_FAILURE
   Intended to be called by the application in response to a 'pause'
   command from the steerer.  Blocks until a 'resume' or 'stop' 
   command received.  Returns all commands that immediately followed
   a 'resume' command as well as the labels of any parameters edited
   in that particular message.  (Parameters can be edited while the
   application is paused because this routine continually calls
   Consume_control() until it receives 'resume' or 'stop'.) 

   The application programmer is free to provide their own version of
   this routine, should they need to take actions while the simulation
   is paused (as may be the case with an experiment). Any 'pause' 
   routine must periodically call Steering_control() in order to check
   for a 'resume' command.
   @see Steering_control()
*/
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
   needed by @e e.g. Steering_control().  The memory allocated by a call to 
   this routine is managed and ultimately free'd by the steering library 
   (during the call to Steering_finalize()).
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
   points of type &lt;type&gt; (coded as REG_INT etc.) */
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
