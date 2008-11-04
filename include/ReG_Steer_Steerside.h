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

/** @file ReG_Steer_Steerside.h 
    @brief Header file for inclusion in steering-client code 

    Header file defining public routines used in the construction of
    the steering interface of a steering component.

    @author Andrew Porter
    @author Robert Haines
  */

#ifndef __REG_STEER_STEERSIDE_H__
#define __REG_STEER_STEERSIDE_H__

#include "ReG_Steer_Config.h"
#include "ReG_Steer_Utils.h"

#ifdef __cplusplus
  #define PREFIX "C"
#else
  #define PREFIX 
#endif

/**
   Data structure used to return details of checkpoint log
   entries to steerer */
typedef struct {
  /** Tag associated with this checkpoing */
  char   chk_tag[REG_MAX_STRING_LENGTH];
  /** No. of parameters for which we have details at this chkpt */
  int    num_param;
  /** Associated parameter labels at this chkpt */
  char   param_labels[REG_MAX_NUM_STR_PARAMS][REG_MAX_STRING_LENGTH];
  /** Associated parameter values at this chkpt */
  char   param_values[REG_MAX_NUM_STR_PARAMS][REG_MAX_STRING_LENGTH];

} Output_log_struct;

/**
   Data structure used to return details of steerable/monitored
   parameters */
typedef struct {
  /** Label given to this parameter at its registration */
  char  label[REG_MAX_STRING_LENGTH];
  /** The type of this parameter */
  int   type;
  /** The handle generated for this parameter at its registration */
  int   handle;
  /** The current value of this parameter */
  char  value[REG_MAX_STRING_LENGTH];
  /** The minimum permitted value for this parameter (if steerable) */
  char  min_val[REG_MAX_STRING_LENGTH];
  /** The maximum permitted value for this parameter (if steerable).
      If the parameter is of type REG_CHAR then this holds the maximum
      permitted length fo the string. */
  char  max_val[REG_MAX_STRING_LENGTH];
  /** If this parameter is of type REG_BIN, then this points to the
      raw data */
  void *raw_data;

} Param_details_struct;

/*-------------- Steerer-side function prototypes -------------*/

/**
   @param SimID A string (of no more than REG_MAX_STRING_LENGTH 
   characters) identifying the simulation to attach to
   (Web Service endpoint or directory on hard disk)
   @param SimHandle On successful return, holds the 
   library-generated handle of the simulation
   @return REG_SUCCESS, REG_FAILURE, REG_MEM_FAIL

   In the RealityGrid implementation, this routine first attempts to
   contact the Steering Web/Grid Service with the handle given by @p
   SimID.  If this fails then it attempts to revert to steering via
   the local file system.  In this case, the library first tries
   interpreting @p SimID as a directory on the local filesystem (@e
   i.e. it assumes that @p SimID gives the directory that was passed
   to the running simulation in the REG_STEER_DIRECTORY environment
   variable).  If this too fails then the library attempts to get a
   valid steering directory from the REG_STEER_DIRECTORY environment
   variable.  If all of these attempts fail then the routine returns
   REG_FAILURE.
*/
extern PREFIX int Sim_attach(char *SimID,
			     int  *SimHandle);

/**
   @param SimID Address of service or directory to attach to 
   @param sec Pointer to struct holding userDN, passphrase and,
   potentially, path to CA certs 
   @param SimHandle On successful return holds the library-generated 
   handle of the simulation 
   @return REG_SUCCESS, REG_FAILURE, REG_MEM_FAIL

   As Sim_attach() except takes a pointer to a reg_security_info struct.
   This holds a username and passphrase for use 
   with WS-Security (only supported in the WSRF implementation).  If this
   struct also holds a valid path to the directory holding CA certs then 
   this turns on authentication of the SWS via SSL (WSRF only).
*/
extern PREFIX int Sim_attach_secure(const char *SimID,
                                    const struct reg_security_info *sec,
				    int  *SimHandle);

/**
   @param SimHandle Handle of the simulation from which to detach, not
   valid once call has returned
   @return REG_SUCCESS

   Detach from the specfied simulation.  Signals the simulation
   that steerer has detached and then cleans-up associated
   files and table entries. */
extern PREFIX int Sim_detach(int *SimHandle);

/**
   @param SimHandle Handle of the simulation that emitted the message
   @param msg_type The type of the message received
   @return REG_SUCCESS, REG_FAILURE

   Looks for the next message from any attached simulations. If it
   finds one then it returns the handle of the originating simulation
   and the type of message.  If it fails to find a message then it
   returns REG_FAILURE.
*/
extern PREFIX int Get_next_message(int   *SimHandle,
				   int   *msg_type);

/**
   @param SimHandle Handle of simulation from which to receive
   definitions of steerable and monitored parameters
   @return REG_SUCCESS, REG_FAILURE

   Consume the parameter definitions emitted by the steered
   application.  The internal table of parameters is updated ready for
   querying (@e e.g. in order to update a GUI).
*/
extern PREFIX int Consume_param_defs(int SimHandle);

/**
   @param SimHandle Handle of simulation from which to receive
   definitions of IOTypes.
   @return REG_SUCCESS, REG_FAILURE

   Consume and store the IOType definitions that the simulation has
   emitted. These definitions provide information to be displayed by a
   steering client in order to allow the user to request sample data
   to be emitted and consumed.
*/
extern PREFIX int Consume_IOType_defs(int SimHandle);

/** 
   Consume and store the Chk type definitions that the 
   simulation referred to by SimHandle has emitted. */
extern PREFIX int Consume_ChkType_defs(int SimHandle);

/**
   @param SimHandle Handle of simulation from which to receive status msg
   @param SeqNum Measure of progress of attached simulation
   @param NumCmds No. of commands received from simulation
   @param Commands List of commands received from simulation
   @return REG_SUCCESS, REG_FAILURE, REG_MEM_FAIL

   Consume a status message emitted by the simulation associated with
   @p SimHandle.  Returns that simulation's current sequence no.  and
   a list of any commands received (@e e.g. notification that it has
   finished). Any parameter values received by this routine are
   automatically used to update the internal library table of
   parameters.
*/
extern PREFIX int Consume_status(int   SimHandle,
				 int  *SeqNum,
				 int  *NumCmds,
				 int  *Commands);

/**
   @param SimHandle Handle of simulation from which to receive log msg
   @return REG_SUCCESS, REG_FAILURE

   Consumes any logging messages (containing details on checkpoints
   taken) emitted by an attached simulation.  The contents of the
   message are stored in the log for that simulation.  This log may be
   accessed by the Get_chk_log_number() and Get_chk_log_entries()
   functions.  This functionality is only used in local (file-based)
   steering - in remote steering using Grid Services the checkpoint
   logging is carried out using a Checkpoint Tree.  See the
   ReG_Steering_Grid_Service.doc document for more details.
 */
extern PREFIX int Consume_log(int   SimHandle);

/**
   @param SimHandle Handle of the simulation to send control msg to
   @param NumCommands No. of commands to send (cannot exceed 
   REG_MAX_NUM_STR_CMDS, defined in ReG_Steer_types.h)
   @param SysCommands List of commands to send
   @param SysCmdParams Parameters (if any) to go with each command
   @return REG_SUCCESS, REG_FAILURE

   Emit a steering-control message to a simulation. Emits the
   specified commands (if any) and automatically sends any (steerable)
   parameter values that have been edited since the last call to this
   routine. 
*/
extern PREFIX int Emit_control(int    SimHandle,
			       int    NumCommands,
			       int   *SysCommands,
			       char **SysCmdParams);

/** Wrapper for generating common steering command */
extern PREFIX int Emit_detach_cmd(int SimHandle);

/** Wrapper for generating common steering command */
extern PREFIX int Emit_stop_cmd(int SimHandle);

/**
   @param SimHandle Handle of the simulation to send msg to
   @return REG_SUCCESS, REG_FAILURE

   Wrapper for generating pause command and sending to attached 
   simulation.
 */
extern PREFIX int Emit_pause_cmd(int SimHandle);

/**
   @param SimHandle Handle of the simulation to send msg to
   @return REG_SUCCESS, REG_FAILURE

   Wrapper for generating resume command and sending it to the attached 
   simulation.
*/
extern PREFIX int Emit_resume_cmd(int SimHandle);

/**
   @param SimHandle Handle of the simulation to send msg to
   @param ParamHandle Handle of parameter for which to retrieve log
   @return REG_SUCCESS, REG_FAILURE

   Emit a command to instruct the steered application to emit all of
   the logged values of the specified parameter.  The log itself is
   stored internally and is accessed via Get_param_log().
 */
extern PREFIX int Emit_retrieve_param_log_cmd(int SimHandle, 
					      int ParamHandle);

/** 
   @param SimHandle Handle of the simulation to send msg to
   @param chkGSH Grid Service Handle of checkpoint to restart from
   @return REG_SUCCESS, REG_FAILURE

   OGSI/WSRF-specific for generating restart command and sending it to
   the attached application.  Note that the steering library does not
   concern itself with the physical location of checkpoint files and
   therefore this must be handled externally if the specified
   checkpoint does not exist on the machine running the steered
   application.
*/
extern PREFIX int Emit_restart_cmd(int   SimHandle, 
				   char *chkGSH);

/** 
   @return REG_SUCCESS, REF_FAILURE
   Initialize the internal tables @e etc. used by the steering library
   on the steering application side.  Must be called before all other
   steering-library routines. */
extern PREFIX int Steerer_initialize();

/** 
   @return REG_SUCCESS
   Cleans up the internal tables @e etc. Must be called after all steering
   activity is complete. */
extern PREFIX int Steerer_finalize();

/** 
   @param SimHandle Handle of simulation for which to delete tables
   @return REG_SUCCESS, REG_FAILURE

   Deletes all data associated with the simulation with handle @p
   SimHandle.  Used when a simulation detaches.  Supplied as a
   separate interface because also required when the simulation
   initiates the detach (@e e.g. when it has completed its run).
*/
extern PREFIX int Delete_sim_table_entry(int *SimHandle);

/**
   A debugging routine - writes the complete contents of the internal
   table holding information on all connected simulations to 
   ./sim_table.txt 
   @internal
*/
extern PREFIX int Dump_sim_table();

/** 
   @param sim_handle Handle of simulation for which to get no. of params
   @param steerable Whether to get steerable (@c REG_TRUE) or monitored 
   (@c REG_FALSE) parameters
   @param num_params On success, the no. of parameters currently registered
   by the attached simulation
   @return REG_SUCCESS, REG_FAILURE

   Gets the number of @p steerable parameters associated with the simulation 
   with handle @p sim_handle.  */
extern PREFIX int Get_param_number(int  sim_handle,
				   int  steerable,
				   int *num_params);

/**
   @param sim_handle Handle of simulation for which to get param values
   @param steerable Whether to get steerable (@c REG_TRUE) or monitored 
   (@c REG_FALSE) parameters
   @param num_params Number of parameter values to retrieve
   @param param_details Pointer to array of Param_details_struct structs
   holding details of the parameters registered with the library
   @return REG_SUCCESS, REG_FAILURE

   Get_param_number() should be called before this routine in order
   to establish the number of parameters available and @p param_details
   should point to a sufficiently long array
   @see Get_param_number(), Param_details_struct
*/
extern PREFIX int Get_param_values(int                   sim_handle,
				   int                   steerable,
				   int                   num_params,
				   Param_details_struct *param_details);

/**
   @param sim_handle Handle of simulation for which to set param values
   @param num_params No. of parameters for which to set new values
   @param handles Array of handles of the parameters to change
   @param vals Array of new values (encoded as strings) to give the parameters
   @return REG_SUCCESS, REG_FAILURE

   Sets the values of the parameters with the specified handles for the
   simulation with handle @p sim_handle. Causes internal flags to be set
   to indicate that these parameter values have changed. The new values are 
   sent to the simulation on the next call of Emit_control().
*/
extern PREFIX int Set_param_values(int    sim_handle,
				   int    num_params,
				   int   *handles,
				   char* *vals);

/**
   @param sim_handle Handle of simulation for which to get param log
   @param handle Handle of parameter for which to get log
   @param buf On successful return, holds pointer to internal library buffer
   holding logged values as array of doubles
   @param num_entries On successful return, holds no. of entries in log

   Retrieve pointer to internal library buffer holding previous values of
   the parameter.
*/
extern PREFIX int Get_param_log(int      sim_handle,
				int      handle,
				double **buf, 
				int     *num_entries);

/**
   @param sim_handle Handle of simulation for which to get no. of IOTypes
   @param num_iotypes On successful return, holds no. of IOTypes registered
   by the simulation

   Gets the number of IO types associated by the simulation.
   @see Get_iotypes()
*/
extern PREFIX int Get_iotype_number(int sim_handle,
				    int *num_iotypes);

/**
   @param sim_handle Handle of simulation for which to get details of IOTypes
   @param num_iotypes Number of IOTypes to retrieve details on
   @param handles List of handles of retrieved IOTypes
   @param labels List of labels of retrieved IOTypes
   @param types List of the types (REG_IO_IN or REG_IO_OUT) 
   of the retrieved IOTypes
   @param io_freqs Frequency associated with each IOType (zero if no 
   automatic emission/consumption)
   @return REG_SUCCESS, REG_FAILURE

   Gets the first @p num_iotypes IOTypes associated with the simulation
   with handle @p sim_handle. 
 */
extern PREFIX int Get_iotypes(int    sim_handle,
			      int    num_iotypes,
			      int   *handles,
			      char* *labels,
			      int   *types,
			      int   *io_freqs);

/**
   @param sim_handle Handle of simulation for which to set IOType freqs
   @param num_iotypes The number of IOTypes to change
   @param iotype_handles Array of handles of the IOTypes to change
   @param freqs Array of the new frequency values

   A utility function that allows the steerer to update the emit/consume
   frequency associated with a given IOtype. The new values are sent to 
   the simulation on the next call of Emit_control(). */
extern PREFIX int Set_iotype_freq(int  sim_handle,
				  int  num_iotypes,
				  int *iotype_handles,
				  int *freqs);

/**
   @param sim_handle Handle of simulation to get details for
   @param num_chktypes On successful return, the no of ChkTypes registered 
   by the simulation

   Gets the number of Chk types associated with a simulation.
   @see Get_chktypes(), Get_iotype_number()
 */
extern PREFIX int Get_chktype_number(int  sim_handle,
				     int *num_chktypes);

/** 
   As for Get_iotypes() with the only exception that a checkpoint type
   can be of type REG_IO_IN, REG_IO_OUT or REG_IO_INOUT.
   @see Get_chktype_number(), Get_iotypes() 
*/
extern PREFIX int Get_chktypes(int    sim_handle,
			       int    num_chktypes,
			       int   *handles,
			       char* *labels,
			       int   *types,
			       int   *chk_freqs);

/**
   A utility function that allows the steerer to update the emit
   frequency associated with a given Chktype. 
   @see Set_iotype_freq()
*/
extern PREFIX int Set_chktype_freq(int  sim_handle,
				   int  num_chktypes,
				   int *chktype_handles,
				   int *freqs);

/**
   @param sim_handle Handle of the simulation to get info. on
   @param num_cmds On successful return, the no. of supported commands
   that the simulation has.

   Gets the number of supported commands registered by a simulation.
*/
extern PREFIX int Get_supp_cmd_number(int  sim_handle,
				      int *num_cmds);

/**
   @param sim_handle Handle of the simulation to get info. on
   @param num_cmds No. of supported commands to get details on
   @param cmd_ids On successful return, list of supported commands.  Must
   point to an array large enough to hold @p num_cmds integers.

   Get the first @p num_cmds supported commands registered by a
   simulation @e i.e. those that the application has undertaken to 
   support in its call to Steering_initialize() (plus those that are 
   supported by default such as REG_STR_DETACH).
*/
extern PREFIX int Get_supp_cmds(int  sim_handle,
				int  num_cmds,
				int *cmd_ids);

/**
   @param sim_handle Handle of the simulation to get info. on
   @param chk_handle Handle of ChkType to get info. on
   @param num_entries On successful return, holds the number of entries
   in the log for checkpoints of this type
   @return REG_SUCCESS, REG_FAILURE

   Query function for getting checkpoint information out of logging
   structure. Note that in the RealityGrid implementation this routine
   is only used for local (file-based) steering.  When the
   grid/web-service framework is used, checkpoint logging is performed
   using a tree of grid services and querying this tree is outside the
   scope of this API.
*/
extern PREFIX int Get_chk_log_number(int  sim_handle,
				     int  chk_handle,
				     int *num_entries);

/**
   @param sim_handle Handle of the simulation to get info. on
   @param chk_handle Handle of ChkType to get info. on (as 
   returned in the @p handles array by a call to Get_chktypes())
   @param num_entries No. of entries to retrieve from logs
   @param entries On successful return, holds array of @p num_entries
   structs giving information on each log entry.
   @return REG_SUCCESS, REG_FAILURE

   Returns the FIRST @p num_entries checkpoint entries from the logs */
extern PREFIX int Get_chk_log_entries(int                sim_handle,
				      int                chk_handle,
			              int                num_entries,
				      Output_log_struct *entries);

/**
   Returns the LAST @p num_entries checkpoint entries in reverse
   chronological order 
   @see Get_chk_log_entries */
extern PREFIX int Get_chk_log_entries_reverse(int                sim_handle,
					      int                chk_handle,
					      int                num_entries,
					      Output_log_struct *entries);


#endif
