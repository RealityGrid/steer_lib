/* ----------------------------------------------------------------------------
  This file is part of the RealityGrid Steering Library Script Wrappers.
 
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
 
  Author........: Robert Haines
---------------------------------------------------------------------------- */

/* This ensures REG_DEBUG is passed through to the wrappers */
#if (REG_DEBUG == 0)
#define REG_DEBUG 0
#else
#define REG_DEBUG 1
#endif // REG_DEBUG

/*-------------- Application-side function prototypes -------------*/

extern void Steering_enable(const int EnableSteer);

extern int Steering_initialize(char *AppName,
			       int   NumSupportedCmds,
			       int  *SupportedCmds);

extern int Register_IOType(char* IOLabel,
			   int   direction,
			   int   IOFrequency,
			   int   *iotypehandle);

extern int Enable_IOTypes_on_registration(int toggle);

extern int Disable_IOType(int IOType);

extern int Enable_IOType(int IOType);

extern int Enable_IOType_acks(int IOType);

extern int Disable_IOType_acks(int IOType);

#if 0
extern int Register_ChkType(char* ChkLabel,
			    int   direction,
			    int   ChkFrequency,
			    int   *ChkType);

extern int Record_Chkpt(int   ChkType,
			char *ChkTag);

extern int Add_checkpoint_file(int   ChkType,
			       char *filename);

extern int Record_checkpoint_set(int   ChkType,
				 char *ChkTag,
				 char *Path);
#endif

extern int Register_param(char* ParamLabel,
			  int   ParamSteerable,
			  void *ParamPtr,
			  int   ParamType,
			  char* ParamMinimum,
			  char* ParamMaximum);

extern int Steering_control(int     SeqNum,
			    int    *NumSteerParams,
			    char*  *SteerParamLabels,
			    int    *NumSteerCommands,
			    int    *SteerCommands,
			    char*  *SteerCmdParams);

#ifdef SWIGJAVA
extern int Emit_start(int  IOType,
		      int  SeqNum,
		      int *IOHandle);

extern int Emit_start_blocking(int    IOType,
			       int    SeqNum,
			       int   *IOHandle,
			       float  TimeOut);

extern int Emit_data_slice(int	            IOHandle,
			   int             DataTypeIN,
			   int             CountIN,
			   const void     *pDataIN);

extern int Emit_stop(int *IOHandleINOUT);
#endif

extern int Consume_start(int  IOType,
			 int *IOHandle);

extern int Consume_start_blocking(int   IOType,
				  int  *IOHandle,
				  float TimeOut);

extern int Consume_data_slice_header(int  IOTypeIndex,
				     int *DataType,
				     int *DataCount);

#ifndef SWIGJAVA
extern int Consume_data_slice(int     IOTypeIndex,
			      int     DataType,
			      int     Count,
			      void   *pDataOUT);
#endif

extern int Consume_stop(int     *IOHandleINOUT);

extern int Steering_finalize();

#if 0
extern int Steering_pause(int   *NumSteerParams,
			  char **SteerParamLabels,
			  int   *NumCommands,
			  int   *SteerCommands,
			  char **SteerCmdParams);

extern char **Alloc_string_array(int String_len,
				 int Array_len);

extern int Free_string_arrays();

extern int Make_vtk_buffer(int    nx,
			   int    ny,
			   int    nz,
			   int    veclen,
			   double a,
			   double b,
			   double c,
			   float *array);

extern int Make_vtk_header(char  *header,
			   char  *title,
			   int    nx,
			   int    ny,
			   int    nz,
			   int    veclen,
			   int    type);

extern int Make_chunk_header(char *header,
			     int   IOindex,
			     int   totx, int toty, int totz,
			     int   sx,   int sy,   int sz,
			     int   nx,   int ny,   int nz);

extern int Set_f90_array_ordering(int IOTypeIndex,
				  int flag);

extern int Called_from_f90(int flag);

extern int Reorder_array(int          ndims,
			 int         *tot_extent,
			 int         *sub_extent,
			 int         *origin,
			 int          type,
			 void        *pInData,
			 void        *pOutData,
			 int          to_f90);
#endif

/* It's useful to have a sizeof type thing... */
%inline %{
  int Sizeof(int type) {
    int result;
    switch(type) {
    case REG_INT:
      result = sizeof(int);
      break;
    case REG_CHAR:
      result = sizeof(char);
      break;
    case REG_FLOAT:
      result = sizeof(float);
      break;
    case REG_DBL:
      result = sizeof(double);
      break;
    case REG_XDR_INT:
      result = REG_SIZEOF_XDR_INT;
      break;
    case REG_XDR_FLOAT:
      result = REG_SIZEOF_XDR_FLOAT;
      break;
    case REG_XDR_DOUBLE:
      result = REG_SIZEOF_XDR_DOUBLE;
      break;
    default:
      result = 0;
      break;
    }

    return result;
  }
%}
int Sizeof(int type);

/*-------------- Steering-side function prototypes -------------*/

#ifdef SWIGJAVA

extern int Steerer_initialize();

extern int Steerer_finalize();

extern int Get_sim_list(int* nSims,
			char** simName,
			char** simGSH);

extern int Sim_attach(char *SimID,
		      int  *SimHandle);

extern int Sim_detach(int *SimHandle);

extern int Delete_sim_table_entry(int *SimHandle);

extern int Consume_param_defs(int SimHandle);

extern int Get_param_number(int  sim_handle,
			    int  steerable,
			    int *num_params);

extern int Set_param_values(int    sim_handle,
			    int    num_params,
			    int   *handles,
			    char* *vals);

//extern int Get_param_log(int      sim_handle,
//			 int      handle,
//			 double **buf, 
//			 int     *num_entries);

extern int Consume_IOType_defs(int SimHandle);

extern int Get_iotype_number(int sim_handle,
			     int *num_iotypes);

extern int Get_iotypes(int    sim_handle,
		       int    num_iotypes,
		       int   *io_handles,
		       char* *io_labels,
		       int   *io_types,
		       int   *io_freqs);

extern int Consume_ChkType_defs(int SimHandle);

extern int Get_chktype_number(int  sim_handle,
			      int *num_chktypes);

extern int Get_chktypes(int    sim_handle,
			int    num_chktypes,
			int   *chk_handles,
			char* *chk_labels,
			int   *chk_types,
			int   *chk_freqs);

extern int Get_supp_cmd_number(int  sim_handle,
			       int *num_cmds);

extern int Get_supp_cmds(int  sim_handle,
			 int  num_cmds,
			 int *cmd_ids);

extern int Emit_stop_cmd(int SimHandle);

extern int Emit_pause_cmd(int SimHandle);

extern int Emit_resume_cmd(int SimHandle);

extern int Emit_retrieve_param_log_cmd(int SimHandle, 
				       int ParamHandle);

extern int Get_next_message(int   *SimHandle,
			    int   *msg_type);

extern int Emit_control(int    SimHandle,
			int    NumCommands,
			int   *SysCommands,
			char **SysCmdParams);

extern int Consume_status(int   SimHandle,
			  int  *SeqNum,
			  int  *NumCmds,
			  int  *Commands);

#endif // SWIGJAVA
