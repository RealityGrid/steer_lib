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
#ifndef __REG_STEER_APPSIDE_INTERNAL_H__
#define __REG_STEER_APPSIDE_INTERNAL_H__

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

/* Type definition for table used to hold details on the communication
   channel between the application and the steerer */
typedef struct {

  /* Where to write files for file-based steering */
  char			file_root[REG_MAX_STRING_LENGTH];

  /* For steering via globus_io - port to listen on */
  int			port_num;

  /* structure used to hold all globus_io socket information */
  socket_type_steering	socket_info;

  /* Address of the Steering Grid Service (for SOAP-based steering) */
  char                  SGS_address[REG_MAX_STRING_LENGTH];

  /* String to hold 'supported commands' message 'cos we can't 
     actually send it until a steerer has connected in the case
     where we're using globus_io */
  char			supp_cmds[REG_MAX_MSG_SIZE];

  /* Buffer to hold received messages */
  char			msg_buffer[REG_MAX_MSG_SIZE];

} Steerer_connection_table_type;


/* Whether steering is enabled (set by user) */
static int ReG_SteeringEnabled = FALSE;
/* Whether the set of registered params has changed */
static int ReG_ParamsChanged   = FALSE;
/* Whether the set of registered IO types has changed */
static int ReG_IOTypesChanged  = FALSE;
/* Whether the set of registered Chk types has changed */
static int ReG_ChkTypesChanged  = FALSE;
/* Whether app. is currently being steered */
static int ReG_SteeringActive  = FALSE;
/* Whether steering library has been initialised */
static int ReG_SteeringInit    = FALSE;

/* Global variable used to store next valid handle value for both
   IOTypes and ChkTypes - these MUST have unique handles because
   they are used as command IDs */
static int Next_IO_Chk_handle = REG_MIN_IOTYPE_HANDLE;

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

/* Emit any log entries that steerer doesn't know about */
static int Emit_log();

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

/* Generate steering commands independently of the steerer. e.g. this
   implements the automatic emission/consumption of those IOTypes and
   ChkTypes with a non-zero auto. emit/consume frequency. *posn is 
   the point at which to start adding commands and associated parameters
   to the SteerCommands and SteerCmdParams arrays. */
static int Auto_generate_steer_cmds(int    SeqNum,
				    int   *posn, 
				    int   *SteerCommands, 
				    char **SteerCmdParams);

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

/* Send a status message to the steerer */
static int Send_status_msg(char *buf);

static int Send_status_msg_file(char *buf);

/* Read the next control message from the steerer, if any.  Results
   passed back in structure.  Returns NULL if no message. */
static struct msg_struct *Get_control_msg();

static struct msg_struct *Get_control_msg_file();


/* Set-up stuff to receive connection from steering client */
static int Initialize_steering_connection(int  NumSupportedCmds,
					  int *SupportedCmds);

static int Initialize_steering_connection_file(int  NumSupportedCmds,
					       int *SupportedCmds);


/* Check for a connection from a steering client - return REG_SUCCESS
   if a client is attempting to connect */
static int Steerer_connected();

static int Steerer_connected_file();


/* Take down any connection to a steering client */
static int Finalize_steering_connection();


/* Initialize the transport mechanism for IOtypes */
static int Initialize_IOType_transport(const int direction,
				const int index);


/* Finalize the transport mechanism for IOtypes */
static void Finalize_IOType_transport();


/* Check if any sample data needs to be consumed */
static int Consume_start_data_check(const int index);


/* Read the sample data */
static int Consume_data_read(const int		index,  
			     const int		datatype,
			     const size_t	num_bytes_to_send, 
			     void		*pData);

/* Emit header for sample data */
static int Emit_header(const int index);


/* Emit footer for sample data */
static int Emit_footer(const int index,
		       const char * const buffer);


static int Emit_data(const int		index,
		     const int		datatype,
		     const size_t	num_bytes_to_send,
		     void		*pData);

/* Is the communication connection up */
static int Get_communication_status(const int		index);

/* Read ReG-specific header for iotype */
static int Consume_iotype_msg_header(int IOTypeIndex,
				     int *DataType,
				     int *Count);

/* Construct and send ReG-specific header for iotype*/
static int Emit_iotype_msg_header(int IOTypeIndex,
				  int DataType,
				  int Count);

/* Sets the value of the next primary key to be used in generating
   log entries.  If a log file exists then it pulls the last value out
   of it and increments it by one, otherwise it sets it to zero. */
int Set_log_primary_key();

/* Pulls log entries out of the supplied buffer, packs them into
   messages and sends them to the steerer. Log entries are assumed
   to be contiguous in the buffer. */
static int Emit_log_entries(char *buf);

/* Open log file (in append mode) */
static int Open_log_file();

/* Close the log file */
static int Close_log_file();

/* Save current contents of log to file */
static int Save_log();

/* Convert current log to xml and store in buffer pointed to by pchar.
   Length of buffer is returned in count. The memory pointed to by
   *pchar must be free()'d by the caller once finished.  If
   not_sent_only == TRUE then only those entries not already returned
   to the steering client are retrieved */
static int Log_to_xml(char **pchar, int *count, const int not_sent_only);

#endif
