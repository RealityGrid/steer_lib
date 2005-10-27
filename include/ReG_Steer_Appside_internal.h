/*----------------------------------------------------------------------------
  Header file defining internal (to the steering library) utility
  routines for use in constructing a steering interface to an 
  application.

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

  Authors........: Andrew Porter, Robert Haines

---------------------------------------------------------------------------*/
#ifndef __REG_STEER_APPSIDE_INTERNAL_H__
#define __REG_STEER_APPSIDE_INTERNAL_H__

#include "ReG_Steer_Common.h"
#include "ReG_Steer_XML.h"
#include "soapH.h"

/** @file ReG_Steer_Appside_internal.h
    @brief Source file for steering-client routines
  */

/*----------------------- Data structures -----------------------*/

/** Structure to hold details of open IO channels */
typedef struct {

  /* Handle of associated IOType */
  int   iotype_handle;

  /* Pointer to buffer to hold data */
  void *buffer;

}IO_channel_table_type;

/** Type definition for table used to hold details on the communication
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

  /** Address of the Steering Grid Service (for SOAP-based steering)
  char                  SGS_address[REG_MAX_STRING_LENGTH];*/
  SGS_info_type SGS_info;

  /** String to hold 'supported commands' message 'cos we can't 
      actually send it until a steerer has connected in the case
      where we're using SOAP */
  char			supp_cmds[REG_MAX_MSG_SIZE];

  /** Buffer to hold received messages */
  char			msg_buffer[REG_MAX_MSG_SIZE];

  /** Flag indicating whether or not lib should handle pause cmd
     internally (REG_TRUE) or pass it up to the app (REG_FALSE); */
  int                   handle_pause_cmd;

} Steerer_connection_table_type;

/*--------- Prototypes of internal library functions -------------*/

/** Emit all of the parameters that have previously been registered 
    (if any). */
int Emit_param_defs();

/** Emit all of the IO types that have previously been registered
    (if any). */
int Emit_IOType_defs();

/** Emit all of the Chk types that have previously been registered
    (if any). */
int Emit_ChkType_defs();

/** 
   Consume a control message (if any present) from the steerer. Returns
   any commands and associated parameters (the latter as a space-separated 
   list in a string) that the application must deal with.  CommandParams is an
   array of pointers to strings that must have already had memory allocated
   for them. Also returns 
   the handles and labels of any (steerable) parameters whose values have
   changed. (The values themselves are updated from within this routine.) */
int Consume_control(int    *NumCommands,
		    int    *Commands,
		    char  **CommandParams,
		    int    *NumSteerParams,
		    int    *SteerParamHandles,
		    char  **SteerParamLabels);

/** 
   Emit a status report to be consumed by the steerer.  <NumParams>
   and <ParamHandles> are intended to allow us to tell the steerer
   what we just received from it but this is not currently 
   implemented.  <NumCommands> and <Commands> are intended for the
   same purpose but are also used to send commands such as 'finish'
   (when sim. has finished) back to the steerer. */
int Emit_status(int   SeqNum,
		int   NumParams,
		int  *ParamHandles,
		int   NumCommands,
		int  *Commands);

/** 
   Generate steering commands independently of the steerer. e.g. this
   implements the automatic emission/consumption of those IOTypes and
   ChkTypes with a non-zero auto. emit/consume frequency.  Routine also 
   checks for any messages received previously that might now be valid.
   \param *posn the point at which to start adding commands and 
   associated parameters to the SteerCommands and SteerCmdParams arrays. */
int Auto_generate_steer_cmds(int    SeqNum,
			     int   *posn, 
			     int   *SteerCommands, 
			     char **SteerCmdParams,
			     int   *paramPosn,
			     int   *SteerParamHandles,
			     char **SteerParamLabels);

/** Take down connection with steerer and clean-up table entries */
int Detach_from_steerer();

/** Update the value of the variable pointed to by the pointer
    associated with the supplied param_entry */
int Update_ptr_value(param_entry *param);

/** Update the 'value' field of the param_entry by retrieving the
    value of the variable pointed to by the pointer associated
    with the entry */
int Get_ptr_value(param_entry *param);

/** Catch any signals and thus allow the library to clean up if the
    application crashes or is stopped abruptly */
void Steering_signal_handler(int aSignal);

/** Send a status message to attached steering client */
int Send_status_msg(char *buf);

/** Create the xml message to tell steerer what standard commands
    the application supports (supplied in array SupportedCmds). */
int Make_supp_cmds_msg(int   NumSupportedCmds,
		       int  *SupportedCmds, 
		       char *msg,
		       int   max_msg_size);

/** Read the next control message from the steerer, if any.  Results
    passed back in structure.  Returns NULL if no message. */
struct msg_struct *Get_control_msg();

/** Set-up stuff to receive connection from steering client */
int Initialize_steering_connection(int  NumSupportedCmds,
				   int *SupportedCmds);

/** Check for a connection from a steering client - return REG_SUCCESS
    if a client is attempting to connect */
int Steerer_connected();

/** Take down any connection to a steering client */
int Finalize_steering_connection();


/** Initialize the transport mechanism for IOtypes */
int Initialize_IOType_transport(const int direction,
				const int index);


/** Finalize the transport mechanism for IOtypes */
void Finalize_IOType_transport();

/** Check for an acknowledgement from the consumer for the last
    data set emitted */
int Consume_ack(const int index);

/** Acknowledge the last data set sent to us and indicate we are
    ready for the next */
int Emit_ack(const int index);

/** Check if any sample data needs to be consumed */
int Consume_start_data_check(const int index);


/** Read the sample data */
int Consume_data_read(const int		index,  
		      const int		datatype,
		      const size_t	num_bytes_to_send, 
		      void		*pData);

/** Emit header for sample data */
int Emit_header(const int index);


/** Emit footer for sample data */
int Emit_footer(const int index,
		const char * const buffer);

/** Emit the raw sample data
  @param index Index of IOType
*/
int Emit_data(const int		index,
	      const int		datatype,
	      const size_t	num_bytes_to_send,
	      void	       *pData);

/** Is the communication connection up? */
int Get_communication_status(const int index);

/** Read ReG-specific header for iotype */
int Consume_iotype_msg_header(int IOTypeIndex,
			      int *DataType,
			      int *Count,
			      int *NumBytes,
			      int *IsFortranArray);

/** Construct and send ReG-specific header for iotype*/
int Emit_iotype_msg_header(int IOTypeIndex,
			   int DataType,
			   int Count,
			   int NumBytes,
			   int IsFortranArray);

/** Wrapper for call to Realloc_IOdef_entry_buffer */
int Realloc_iotype_buffer(int index,
			  int num_bytes);

/** Wrapper for call to Realloc_IOdef_entry_buffer */
int Realloc_chktype_buffer(int index,
			   int num_bytes);

/** Attempt to realloc the buffer associated with the IOdef_entry to 
    num_bytes.  If this fails, the buffer is 'freed' and
    buffer_max_bytes is zeroed, otherwise buffer_max_bytes is set to
    num_bytes. */
int Realloc_IOdef_entry_buffer(IOdef_entry *iodef, 
			       int num_bytes);

/** Toggle whether (toggle=REG_TRUE) or not (toggle=REG_FALSE) to log
    values of the parameter identified by the provided label. Logging
    is on by default. */
int Toggle_param_logging(char *ParamLabel,
			 int   toggle);

/** Takes the supplied structure holding a control message
    and returns its constituents */
int Unpack_control_msg(struct control_struct *ctrl,
		       int    *NumCommands,
		       int    *Commands,
		       char  **CommandParams,
		       int    *NumSteerParams,
		       int    *SteerParamHandles,
		       char  **SteerParamLabels);

/** Extracts the valid_time (if any) from the message and compares
    it with the current simulation time.  Message is valid if its
    valid_time < (Current_time + 0.1*time_step).
    @returns 1 if msg valid, 0 otherwise */
int Control_msg_now_valid(struct msg_struct *msg);

#endif
