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
#ifndef __REG_STEER_APPSIDE_INTERNAL_H__
#define __REG_STEER_APPSIDE_INTERNAL_H__

#include "ReG_Steer_Common.h"
#include "ReG_Steer_XML.h"
#include "soapH.h"

/** @internal
    @file ReG_Steer_Appside_internal.h
    @brief Source file for steering-client routines
    @author Andrew Porter
    @author Robert Haines

    Header file defining internal (to the steering library) utility
    routines for use in constructing a steering interface to an 
    application.
  */

/*----------------------- Data structures -----------------------*/

/** @internal
    Structure to hold details of open IO channels */
typedef struct {

  /** Handle of associated IOType */
  int   iotype_handle;
  /** Pointer to buffer to hold data received data */
  void *buffer;

}IO_channel_table_type;

/** @internal 
    Type definition for table used to hold details on the communication
    channel between the application and the steerer */
typedef struct {

  /** Interval in seconds between checks on whether steerer is
      attached */
  double                polling_interval;

  /** Steerable interval (in increments of Seq. no.) controlling how 
      often we perform steering activity while steerer is connected */
  int                   steer_interval;

  /** Where to write files for file-based steering */
  char			file_root[REG_MAX_STRING_LENGTH];

  /** Struct holding info on Steering Grid Service (for SOAP-based steering) */
  SGS_info_type         SGS_info;

  /** String to hold 'supported commands' message 'cos we can't 
      actually send it until a steerer has connected in the case
      where we're using SOAP */
  char			supp_cmds[REG_MAX_MSG_SIZE];

  /** Buffer to hold received messages */
  char			msg_buffer[REG_MAX_MSG_SIZE];

  /** Flag indicating whether or not lib should handle pause cmd
     internally (REG_TRUE) or pass it up to the app (REG_FALSE) */
  int                   handle_pause_cmd;

} Steerer_connection_table_type;

/*--------- Prototypes of internal library functions -------------*/

/** @internal 
    Emit all of the parameters that have previously been registered 
    (if any). */
int Emit_param_defs();

/** @internal
    Emit all of the IO types that have previously been registered
    (if any). */
int Emit_IOType_defs();

/** @internal 
    Emit all of the Chk types that have previously been registered
    (if any). */
int Emit_ChkType_defs();

/** @internal
   @param NumCommands The number of commands obtained from the steerer
   @param Commands Array holding the commands obtained from the steerer
   @param CommandParams Array holding parameters associated with 
   each command
   @param NumSteerParams No. of params that have been steered
   @param SteerParamHandles Array holding handles of steered params
   @param SteerParamLabels Array holding labels of steered params

   Consume a control message (if any present) from the steerer. Returns
   any commands and associated parameters (the latter as a space-separated 
   list in a string) that the application must deal with.  @p CommandParams 
   is an array of pointers to strings that must have already had memory 
   allocated for them. Also returns 
   the handles and labels of any (steerable) parameters whose values have
   changed. (The values themselves are updated from within this routine.) 
*/
int Consume_control(int    *NumCommands,
		    int    *Commands,
		    char  **CommandParams,
		    int    *NumSteerParams,
		    int    *SteerParamHandles,
		    char  **SteerParamLabels);

/** @internal
   @param SeqNum The current sequence number of the simulation
   @param NumParams No. of parameter changes made at this breakpoint
   @param ParamHandles Array of the handles of changed parameters
   @param NumCommands No. of commands executed at this breakpoint
   @param Commands Array of the commands executed

   Emit a status report to be consumed by the steerer.  @p NumParams
   and @p ParamHandles are intended to allow us to tell the steerer
   what we just received from it but this is not currently 
   implemented.  @p NumCommands and @p Commands are intended for the
   same purpose but are also used to send commands such as 'finish'
   (when sim. has finished) back to the steerer. */
int Emit_status(int   SeqNum,
		int   NumParams,
		int  *ParamHandles,
		int   NumCommands,
		int  *Commands);

/** @internal
   @param SeqNum The current sequence number of the simulation
   @param posn The point at which to start adding commands and 
   associated parameters to the SteerCommands and SteerCmdParams arrays.
   @param SteerCommands The array of commands to which to append any
   automatically-generated commands
   @param SteerParamCommands The array of command parameters to append to
   @param paramPosn The point at which to start adding parameter handles
   and labels to the SteerParamHandles and SteerParamLabels arrays
   @param SteerParamHandles The array to which to add the handles of 
   parameters that have been steered
   @param SteerParamLabels The array to which to add the labels of 
   parameters that have been steered

   Generate steering commands independently of the steerer. @e e.g. this
   implements the automatic emission/consumption of those IOTypes and
   ChkTypes with a non-zero auto. emit/consume frequency.  Routine also 
   checks for any messages received previously that might now be valid.
 */
int Auto_generate_steer_cmds(int    SeqNum,
			     int   *posn, 
			     int   *SteerCommands, 
			     char **SteerCmdParams,
			     int   *paramPosn,
			     int   *SteerParamHandles,
			     char **SteerParamLabels);

/** @internal
   Take down connection with steerer and clean-up table entries */
int Detach_from_steerer();

/** @internal 
   @param param Pointer to struct holding information on parameter

   Update the value of the variable pointed to by the pointer
   associated with @p param */
int Update_ptr_value(param_entry *param);

/** @internal
   @param param Pointer to struct holding information on parameter

   Update the 'value' field of the param_entry by retrieving the
   value of the variable pointed to by the pointer associated
   with the entry */
int Get_ptr_value(param_entry *param);

/** @internal
    Catch any signals and thus allow the library to clean up if the
    application crashes or is stopped abruptly */
void Steering_signal_handler(int aSignal);

/** @internal
    @param buf Pointer to message (xml document) to send

    Send a status message to attached steering client */
int Send_status_msg(char *buf);

/** @internal
    @param NumSupportedCmds The number of commands supported
    @param SupportedCmds Array of commands that are supported
    @param msg Pointer to existing buffer in which to create message
    @param max_msg_size Size of buffer (bytes) pointed to by @p msg

    Create the xml message to tell steerer what standard commands
    the application supports. */
int Make_supp_cmds_msg(int   NumSupportedCmds,
		       int  *SupportedCmds, 
		       char *msg,
		       int   max_msg_size);

/** @internal
    @return NULL if no message else pointer to a valid msg_struct.

    Read the next control message from the steerer, if any. */
struct msg_struct *Get_control_msg();

/** @internal
    @param NumSupportedCmds No. of commands we support
    @param SupportedCmds Array holding the commands we support

    Set-up stuff to receive connection from steering client */
int Initialize_steering_connection(int  NumSupportedCmds,
				   int *SupportedCmds);

/** @internal
    @return REG_SUCCESS if a steering client is connected, 
    REG_FAILURE otherwise

    Check for a connection from a steering client */
int Steerer_connected();

/** @internal 
    @return REG_SUCCESS, REG_FAILURE

    Take down any connection to a steering client */
int Finalize_steering_connection();

/** @internal
    @param direction The direction/type of IOType (REG_IN or REG_OUT)
    @param index The index of this IOType in the table of IOTypes

    Initialize the transport mechanism for an IOtype */
int Initialize_IOType_transport(const int direction,
				const int index);

/** @internal Finalize the transport mechanism for ALL IOtypes */
void Finalize_IOType_transport();

/** @internal 
    @param index The index of the IOType being used
    @return REG_SUCCESS if ack has been received, REG_FAILURE otherwise

    Check for an acknowledgement from the consumer for the last
    data set emitted */
int Consume_ack(const int index);

/** @internal 
    @param index The index of the IOType being used
    @return REG_SUCCESS, REG_FAILURE

    Acknowledge the last data set sent to us and indicate we are
    ready for the next */
int Emit_ack(const int index);

/** @internal
    @param index The index of the IOType being used
    @return REG_SUCCESS if data available, REG_FAILURE otherwise

    Check if any sample data needs to be consumed */
int Consume_start_data_check(const int index);


/** @internal
    @param index The index of the IOType being used
    @param datatype The type of data to read
    @param num_bytes_to_read No. of bytes to read
    @param pData Pointer to buffer of at least @p num_bytes_to_read bytes.
    This buffer is used to store received data unless it is XDR encoded
    in which case the data is stored in an internal buffer associated with
    the IOType.
    @return REG_SUCCESS, REG_FAILURE

    Read the sample data */
int Consume_data_read(const int		index,  
		      const int		datatype,
		      const size_t	num_bytes_to_read, 
		      void		*pData);

/** @internal 
    @param index The index of the IOType being used

    Emit header for sample data */
int Emit_header(const int index);


/** @internal 
    @param index The index of the IOType being used
    @param buffer Ptr to buffer holding footer to emit

    Emit footer for sample data */
int Emit_footer(const int index,
		const char * const buffer);

/** @internal 
  @param index Index of IOType being used
  @param datatype Type of the data to emit
  @param num_bytes_to_send The no. of bytes to emit
  @param pData Pointer to buffer containing the data
  @return REG_SUCCESS, REG_FAILURE

  Emit the raw sample data
*/
int Emit_data(const int		index,
	      const int		datatype,
	      const size_t	num_bytes_to_send,
	      void	       *pData);

/** @internal 
    @param index Index of IOType being used
    @return REG_SUCCESS if connection up, REG_FAILURE otherwise

    Is the communication connection up? */
int Get_communication_status(const int index);

/** @internal 
    @param IOTypeIndex Index of IOType being used
    @param DataType The type of the data specified in the header
    @param Count The no. of data elements specified in the header
    @param NumBytes The no. of bytes of data specified in the header
    @param IsFortranArray Whether the header is for data from a FORTRAN
    array (has consequences for the way in which it is ordered)
    @return REG_SUCCESS if header read successfully, REG_FAILURE otherwise

    Read ReG-specific header for iotype */
int Consume_iotype_msg_header(int IOTypeIndex,
			      int *DataType,
			      int *Count,
			      int *NumBytes,
			      int *IsFortranArray);

/** @internal
    @param IOTypeIndex Index of IOType being used
    @param DataType Type of data to specify
    @param Count No. of data elements to specify
    @param NumBytes No. of bytes to specify
    @param IsFortranArray Whether this header is for data from a FORTRAN
    array (REG_TRUE or REG_FALSE)
    @return REG_SUCCESS, REG_FAILURE

    Construct and send ReG-specific header for iotype*/
int Emit_iotype_msg_header(int IOTypeIndex,
			   int DataType,
			   int Count,
			   int NumBytes,
			   int IsFortranArray);

/** @internal
    @param index Index of IOType
    @param num_bytes No. of bytes to specify in realloc
    
    Wrapper for call to Realloc_IOdef_entry_buffer() */
int Realloc_iotype_buffer(int index,
			  int num_bytes);

/** @internal
    @param index Index of IOType
    @param num_bytes No. of bytes to specify in realloc

    Wrapper for call to Realloc_IOdef_entry_buffer() */
int Realloc_chktype_buffer(int index,
			   int num_bytes);

/** @internal
    @param iodef Pointer to an IOdef_entry
    @param num_bytes No. of bytes to specify in realloc

    Attempt to realloc the buffer associated with the IOdef_entry to
    @p num_bytes.  If this fails, the buffer is free'd and iodef's @p
    buffer_max_bytes is zeroed, otherwise @p buffer_max_bytes is set
    to @p num_bytes. */
int Realloc_IOdef_entry_buffer(IOdef_entry *iodef, 
			       int num_bytes);

/** @internal
    @param ParamLabel Label identifying parameter to edit
    @param toggle REG_TRUE to enable logging, REG_FALSE to disable it.

    Toggle whether or not to log values of the parameter identified by
    the provided label. Logging is on by default. */
int Toggle_param_logging(char *ParamLabel,
			 int   toggle);

/** @internal 
    @param ctrl Pointer to structure holding a control message
    @param NumCommands No. of commands contained in the message
    @param Commands Array holding the commands contained in the message
    @param CommandParams Array holding the command parameters 
    @param NumSteerParams No. of parameters that are given new values in 
    the message
    @param SteerParamHandles Array holding the handles of parameters given
    new values in the message
    @param SteerParamLabels Array holding list of labels of the parameters
    given new values in the message

    Takes the supplied structure holding a control message
    and returns its constituents */
int Unpack_control_msg(struct control_struct *ctrl,
		       int    *NumCommands,
		       int    *Commands,
		       char  **CommandParams,
		       int    *NumSteerParams,
		       int    *SteerParamHandles,
		       char  **SteerParamLabels);

/** @internal
    Extracts the valid_time (if any) from the message and compares
    it with the current simulation time.  Message is valid if its
    valid_time \< (Current_time + 0.1*time_step).
    @returns 1 if msg valid, 0 otherwise */
int Control_msg_now_valid(struct msg_struct *msg);

#endif
