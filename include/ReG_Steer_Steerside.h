/*----------------------------------------------------------------------------
    Header file defining public routines used in the construction of
    the steering interface of a steering component.

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
#include "ReG_Steer_Globus_io.h"


#ifdef __cplusplus
  #define PREFIX "C"
#else
  #define PREFIX 
#endif

/* Data structure used to return details of checkpoint log
   entries to steerer */

typedef struct {

  /* Tag associated with this checkpoing */
  char   chk_tag[REG_MAX_STRING_LENGTH];
  /* No. of parameters for which we have details at this chkpt */
  int    num_param;
  /* Associated parameter labels and values at this chkpt */
  char   param_labels[REG_MAX_STRING_LENGTH][REG_MAX_NUM_STR_PARAMS];
  char   param_values[REG_MAX_STRING_LENGTH][REG_MAX_NUM_STR_PARAMS];

} Output_log_struct;

/* Data structure used to return details of steerable/monitored
   parameters */

typedef struct {

  char  label[REG_MAX_STRING_LENGTH];
  int   type;
  int   handle;
  char  value[REG_MAX_STRING_LENGTH];
  char  min_val[REG_MAX_STRING_LENGTH];
  char  max_val[REG_MAX_STRING_LENGTH];

} Param_details_struct;

/*-------------- Steerer-side function prototypes -------------*/

/* Returns list of steerable applications returned by UNICORE 
   registry (contacted via GridService on the port given in the
   REGISTRY_GSH environment variable. If the steering proxy is not
   available this routine returns REG_FAILURE and nsims=0. The Grid
   Service Handle returned in simGSH must be supplied as the SimID
   to Sim_attach. */
extern PREFIX int Get_sim_list(int   *nSims,
			       char **simName,
			       char **simGSH);

/* Attempt to attach to the specified simulation - returns a 
   handle for this simulation if successful. */
extern PREFIX int Sim_attach(char *SimID,
			     int  *SimHandle);

/* Detach from the specfied simulation.  Signals the simulation
   that steerer has detached and then cleans-up associated
   files and table entries. */
extern PREFIX int Sim_detach(int *SimHandle);

/* Looks for the next message from any attached simulations. If
   it finds one then it returns the handle of the originating
   simulation and the type of message. */
extern PREFIX int Get_next_message(int         *SimHandle,
				   REG_MsgType *msg_type);

/* Consume and store the parameter definitions that the 
   simulation referred to by SimHandle has emitted. */
extern PREFIX int Consume_param_defs(int SimHandle);

/* Consume and store the IO-type definitions that the 
   simulation referred to by SimHandle has emitted. */
extern PREFIX int Consume_IOType_defs(int SimHandle);

/* Consume and store the Chk type definitions that the 
   simulation referred to by SimHandle has emitted. */
extern PREFIX int Consume_ChkType_defs(int SimHandle);

/* Consume a status message emitted by the simulation associated
   with SimHandle.  Returns that simulations current sequence no.
   and a list of any commands received (e.g. finish). */
extern PREFIX int Consume_status(int   SimHandle,
				 int  *SeqNum,
				 int  *NumCmds,
				 int  *Commands);

/* Consume a logging message emitted by the simulatin associated
   with SimHandle.  Contents of the message are stored in the log
   for that simulation. */
extern PREFIX int Consume_log(int   SimHandle);

/* Emit a steering-control message to the simulation associated
   with SimHandle.  Emits the specified commands (if any) and
   automatically sends any (steerable) parameter values that have
   been edited since the last call to this routine. */
extern PREFIX int Emit_control(int    SimHandle,
			       int    NumCommands,
			       int   *SysCommands,
			       char **SysCmdParams);

/* Wrappers for generating common steering commands */
extern PREFIX int Emit_detach_cmd(int SimHandle);
extern PREFIX int Emit_stop_cmd(int SimHandle);
extern PREFIX int Emit_pause_cmd(int SimHandle);
extern PREFIX int Emit_resume_cmd(int SimHandle);

/* Initialise the internal tables etc. used by the steering library
   on the steering application sied.  Must be called before all other
   steering-library routines. */
extern PREFIX int Steerer_initialize();

/* Cleans up the internal tables etc. Must be called after all steering
   activity is complete. */
extern PREFIX int Steerer_finalize();

/* Deletes all data associated with the simulation with handle SimHandle.
   Used when a simulation detaches. */
extern PREFIX int Delete_sim_table_entry(int *SimHandle);

/* A debugging routine - writes the complete contents of the internal
   table holding information on all connected simulations to 
   ./sim_table.txt */
extern PREFIX int Dump_sim_table();

/* Gets the number of <steerable> parameters associated with the simulation 
   with handle sim_handle.  i.e. if steerable==TRUE then this returns the
   number of steerable parameters that the simulation has. */
extern PREFIX int Get_param_number(int  sim_handle,
				   int  steerable,
				   int *num_params);

/* Gets the first <num_params> <steerable> parameters assocaited with the
   simulation with handle sim_handle.  Returns the handles, labels and
   values (as strings) of these parameters. handles, labels, vals, min_vals
   and max_vals must all point to chunks of memory large enough to receive
   num_params entries. */
/*extern PREFIX int Get_param_values(int    sim_handle,
				   int    steerable,
				   int    num_params,
				   int   *handles,
				   char* *labels,
				   char* *vals,
				   int   *types,
				   char* *min_vals,
				   char* *max_vals);
*/
extern PREFIX int Get_param_values(int                   sim_handle,
				   int                   steerable,
				   int                   num_params,
				   Param_details_struct *param_details);

/* Sets the values of the parameters with the specified handles for the
   simulation with handle sim_handle. Causes internal flags to be set
   to indicate that these parameter values have changed. */
extern PREFIX int Set_param_values(int    sim_handle,
				   int    num_params,
				   int   *handles,
				   char* *vals);

/* Gets the number of IO types associated with the simulation with
   handle <sim_handle>. */
extern PREFIX int Get_iotype_number(int sim_handle,
				    int *num_iotypes);

/* Gets the first <num_iotypes> IO types associated with the simulation
   with hand <sim_handle>. Returns the handle and label associated with
   each IO type. Also returns whether IO type is IN,OUT or CHKPT, whether
   it supports automatic emission/consumption and, if so, with what
   frequency (in no. of steps between emission/consumption) it is 
   currently doing so. */
extern PREFIX int Get_iotypes(int    sim_handle,
			      int    num_iotypes,
			      int   *handles,
			      char* *labels,
			      int   *types,
			      int   *io_freqs);

/* A utility function that allows the steerer to update the emit/consume
   frequency associated with a given IOtype - the frequency itself is
   stored as a steerable parameter and therefore must be looked-up. */
extern PREFIX int Set_iotype_freq(int  sim_handle,
				  int  num_iotypes,
				  int *iotype_handles,
				  int *freqs);

/* Gets the number of Chk types associated with the simulation with
   handle <sim_handle>. */
extern PREFIX int Get_chktype_number(int  sim_handle,
				     int *num_chktypes);

/* Gets the first <num_iotypes> Chk types associated with the simulation
   with hand <sim_handle>. Returns the handle and label associated with
   each Chk type. Also returns whether Chk type is IN or OUT and
   frequency (in no. of steps between each emission) of
   automatic emission. */
extern PREFIX int Get_chktypes(int    sim_handle,
			       int    num_chktypes,
			       int   *handles,
			       char* *labels,
			       int   *types,
			       int   *chk_freqs);

/* A utility function that allows the steerer to update the emit
   frequency associated with a given Chktype - the frequency itself is
   stored as a steerable parameter and therefore must be looked-up. */
extern PREFIX int Set_chktype_freq(int  sim_handle,
				   int  num_chktypes,
				   int *chktype_handles,
				   int *freqs);

/* Gets the number of supported commands registered by the simulation
   with handle <sim_handle>. */
extern PREFIX int Get_supp_cmd_number(int  sim_handle,
				      int *num_cmds);

/* Get the first <num_cmds> supported commands registered for the
   simulation with handle <sim_handle>.  Returns the id of each
   command in the array pointed to by cmd_ids.  This array must be
   large enough to hold <num_cmds> integers */
extern PREFIX int Get_supp_cmds(int  sim_handle,
				int  num_cmds,
				int *cmd_ids);

/* Query functions for getting checkpoint information out of logging 
   structure */
extern PREFIX int Get_chk_log_number(int  sim_handle,
				     int  chk_handle,
				     int *num_entries);

/* Returns the FIRST num_entries checkpoint entries from the logs */
extern PREFIX int Get_chk_log_entries(int                sim_handle,
				      int                chk_handle,
			              int                num_entries,
				      Output_log_struct *entries);

/* Returns the LAST num_entries checkpoint entries in reverse
   chronological order */
extern PREFIX int Get_chk_log_entries_reverse(int                sim_handle,
					      int                chk_handle,
					      int                num_entries,
					      Output_log_struct *entries);
