/*----------------------------------------------------------------------------
  This file contains wrapper routines allowing the routines for the 
  construction of an interface to a steering component (from an 
  application component) to be called from F90.  The m4 macros used
  to create the platform-specific C functions are taken from the
  PVM distribution.  See the readme file in the 'conf' directory.

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

#include "ReG_Steer_Appside.h"
#include <unistd.h>
#include <string.h>

#ifndef REG_DEBUG
#define REG_DEBUG 1
#endif

/* Some global variables for this interface layer - used in 
   steering_control_f but initialised in steering_initialize_f */

static char string_buf[(REG_MAX_NUM_STR_PARAMS+REG_MAX_NUM_STR_CMDS)
                       *REG_MAX_STRING_LENGTH];
static char *str_array[REG_MAX_NUM_STR_PARAMS];
static char *str_array_params[REG_MAX_NUM_STR_CMDS];

/* Array to store size in bytes of each of ReG type */
static int sizeof_type[8] = {0,0,0,0,0,0,0,0};

/*----------------------------------------------------------------

SUBROUTINE steering_enable_f(EnableSteer)

  INTEGER (KIND=REG_SP_KIND), INTENT(in) :: EnableSteer
----------------------------------------------------------------*/

void FUNCTION(steering_enable_f) ARGS(`EnableSteer')
INT_KIND_1_DECL(EnableSteer);
{
  Steering_enable((int)(*EnableSteer));
  return;
}

/*----------------------------------------------------------------

SUBROUTINE steering_initialize_wrapper(AppName, NumSupportedCmds, &
                                       SupportedCmds, Status)

  CHARACTER (LEN=REG_MAX_STRING_LEN), INTENT(in) :: AppName
  INTEGER (KIND=REG_SP_KIND), INTENT(in)  :: NumSupportedCmds
  INTEGER (KIND=REG_SP_KIND), INTENT(in)  :: SupportedCmds
  INTEGER (KIND=REG_SP_KIND), INTENT(out) :: Status
----------------------------------------------------------------*/

void FUNCTION(steering_initialize_wrapper) ARGS(`STRING_ARG(AppName), 
                                                 NumSupportedCmds,
			                         SupportedCmds, Status')
STRING_ARG_DECL(AppName);
INT_KIND_1_DECL(NumSupportedCmds);
INT_KIND_1_DECL(SupportedCmds);
INT_KIND_1_DECL(Status);
{
  int   i;
  int   j = 0;
  int   len;
  char *pchar;
  int   found = 0;

  /* Perform some initialisation of ptrs to memory used by 
     steering_control_f */
  for(i=0; i<REG_MAX_NUM_STR_PARAMS; i++){

    str_array[i] = &(string_buf[j*REG_MAX_STRING_LENGTH]);
    j++;
  }
  for(i=0; i<REG_MAX_NUM_STR_CMDS; i++){

    str_array_params[i] = &(string_buf[j*REG_MAX_STRING_LENGTH]);
    j++;
  }

  /* Set flag to indicate library is being called from a F90 code */
  *Status = INT_KIND_1_CAST( Called_from_f90(REG_TRUE) );
  if(*Status != REG_SUCCESS){
    return;
  }

  len = STRING_LEN(AppName);

  if(len > REG_MAX_STRING_LENGTH){

    fprintf(stderr, "steering_initialize_f: ERROR - length of tag "
            "exceeds REG_MAX_STRING_LENGTH (%d) chars\n", 
            REG_MAX_STRING_LENGTH);

    *Status = INT_KIND_1_CAST(REG_FAILURE);
    return;
  }

  memcpy(string_buf, STRING_PTR(AppName), len);

  if(len == REG_MAX_STRING_LENGTH){

    found = 0;
    pchar = STRING_PTR(AppName);
    for(i = (REG_MAX_STRING_LENGTH-1); i == 0; i--){
      if(pchar[i] == '\0'){
        found = 1;
	break;
      }
    }
    if(!found){

      fprintf(stderr, "steering_initialize_f: ERROR - length of tag "
                "is REG_MAX_STRING_LENGTH (%d) chars long\nbut contains "
                "no termination character - shorten label (or its len "
                "declaration)\n", 
                REG_MAX_STRING_LENGTH);
      *Status = INT_KIND_1_CAST(REG_FAILURE);
      return;
    }
  }
  else{

    /* Terminate string */
    string_buf[len] = '\0';
  }

  *Status = INT_KIND_1_CAST( Steering_initialize(string_buf,
                                                 (int)(*NumSupportedCmds), 
	 	                                 (int *)(SupportedCmds)) );
  return;
}

/*----------------------------------------------------------------

SUBROUTINE steering_finalize_f(Status)

  INTEGER (KIND=REG_SP_KIND), INTENT(out) :: Status
----------------------------------------------------------------*/

void FUNCTION(steering_finalize_f) ARGS(`Status')
INT_KIND_1_DECL(Status);
{

  *Status = INT_KIND_1_CAST( Steering_finalize() );
  return;
}

/*----------------------------------------------------------------

SUBROUTINE enable_iotypes_on_registrn_f(Toggle, Status)

  INTEGER (KIND=REG_SP_KIND), INTENT(in)  :: Toggle
  INTEGER (KIND=REG_SP_KIND), INTENT(out) :: Status
----------------------------------------------------------------*/

void FUNCTION(enable_iotypes_on_registrn_f) ARGS(`Toggle,
                                                      Status')
INT_KIND_1_DECL(Toggle);
INT_KIND_1_DECL(Status);
{
  *Status = INT_KIND_1_CAST( Enable_IOTypes_on_registration((int)(*Toggle)) );
  return;
}

/*----------------------------------------------------------------

SUBROUTINE enable_iotype_f(IOType, Status)

  INTEGER (KIND=REG_SP_KIND), INTENT(in)  :: IOType
  INTEGER (KIND=REG_SP_KIND), INTENT(out) :: Status
----------------------------------------------------------------*/

void FUNCTION(enable_iotype_f) ARGS(`IOType,
                                     Status')
INT_KIND_1_DECL(IOType);
INT_KIND_1_DECL(Status);
{
  *Status = INT_KIND_1_CAST( Enable_IOType((int)(*IOType)) );
  return;
}

/*----------------------------------------------------------------

SUBROUTINE disable_iotype_f(IOType, Status)

  INTEGER (KIND=REG_SP_KIND), INTENT(in)  :: IOType
  INTEGER (KIND=REG_SP_KIND), INTENT(out) :: Status
----------------------------------------------------------------*/

void FUNCTION(disable_iotype_f) ARGS(`IOType,
                                      Status')
INT_KIND_1_DECL(IOType);
INT_KIND_1_DECL(Status);
{
  *Status = INT_KIND_1_CAST( Disable_IOType((int)(*IOType)) );
  return;
}

/*----------------------------------------------------------------

SUBROUTINE enable_iotype_acks_f(IOType, Status)

  INTEGER (KIND=REG_SP_KIND), INTENT(in)  :: IOType
  INTEGER (KIND=REG_SP_KIND), INTENT(out) :: Status
----------------------------------------------------------------*/

void FUNCTION(enable_iotype_acks_f) ARGS(`IOType,
                                          Status')
INT_KIND_1_DECL(IOType);
INT_KIND_1_DECL(Status);
{
  *Status = INT_KIND_1_CAST( Enable_IOType_acks((int)(*IOType)) );

  return;
}

/*----------------------------------------------------------------

SUBROUTINE disable_iotype_acks_f(IOType, Status)

  INTEGER (KIND=REG_SP_KIND), INTENT(in)  :: IOType
  INTEGER (KIND=REG_SP_KIND), INTENT(out) :: Status
----------------------------------------------------------------*/

void FUNCTION(disable_iotype_acks_f) ARGS(`IOType,
                                           Status')
INT_KIND_1_DECL(IOType);
INT_KIND_1_DECL(Status);
{
  *Status = INT_KIND_1_CAST( Disable_IOType_acks((int)(*IOType)) );

  return;
}

/*----------------------------------------------------------------

SUBROUTINE register_iotypes_f(NumTypes, IOLabel, IODirn, IOFrequency, 
                              IOType, Status)

  INTEGER (KIND=REG_SP_KIND), INTENT(in)                      :: NumTypes
  CHARACTER (LEN=*), DIMENSION(NumTypes), INTENT(in)          :: IOLabel
  INTEGER (KIND=REG_SP_KIND), DIMENSION(NumTypes), INTENT(in) :: IODirn
  INTEGER (KIND=REG_SP_KIND), DIMENSION(NumTypes), INTENT(in) :: IOFrequency
  INTEGER (KIND=REG_SP_KIND), DIMENSION(NumTypes), INTENT(out):: IOType
  INTEGER (KIND=REG_SP_KIND), INTENT(out)                     :: Status
----------------------------------------------------------------*/

void FUNCTION(register_iotypes_f) ARGS(`NumTypes, 
				        STRING_ARG(IOLabel),
					IODirn,
					IOFrequency,
				        IOType,
				        Status')
INT_KIND_1_DECL(NumTypes);
STRING_ARG_DECL(IOLabel);
INT_KIND_1_DECL(IODirn);
INT_KIND_1_DECL(IOFrequency);
INT_KIND_1_DECL(IOType);
INT_KIND_1_DECL(Status);
{
  int    i;
  int    len;
  char **str_array;
/*  char  *buf;*/

  len = STRING_LEN(IOLabel);
  if(len >= REG_MAX_STRING_LENGTH){
    fprintf(stderr, 
            "register_iotypes_f: WARNING: truncating label\n");
    /* Allow space for terminating '/0' */
    len = REG_MAX_STRING_LENGTH - 1;
  }

  str_array = (char**)malloc((*NumTypes)*sizeof(char*));
 /* buf       = (char*)malloc((*NumTypes)*(len+1));*/

  for(i=0; i<(*NumTypes); i++){

    str_array[i] = (char *)malloc(REG_MAX_STRING_LENGTH);
  }

/*  if(!str_array || !buf){*/
  if(!str_array){

    fprintf(stderr, "Register_IOTypes_f: malloc failed\n");
    *Status = INT_KIND_1_CAST( REG_FAILURE );
    return;
  }

  /* Convert from a single array of char to an array of char* */

  for(i=0; i<(int)(*NumTypes); i++){

    /*str_array[i] = &(buf[i*(len + 1)]);*/
    memcpy(str_array[i], 
           &(STRING_PTR(IOLabel)[i*STRING_LEN(IOLabel)]),
           len);
    str_array[i][len] = '\0';
  }

  *Status = INT_KIND_1_CAST( Register_IOTypes((int)*NumTypes,
			                             str_array,
                                              (int *)IODirn,
                                              (int *)IOFrequency,
			                      (int *)IOType) );
  for(i=0; i<(*NumTypes); i++){

    free(str_array[i]);
  }

  free(str_array);
  str_array = NULL;
/*  free(buf);
  buf = NULL;
*/
  return;
}

/*----------------------------------------------------------------

SUBROUTINE register_iotype_f(IOLabel, IODirn, IOFrequency, 
                             IOType, Status)

  CHARACTER (LEN=*), INTENT(in)          :: IOLabel
  INTEGER (KIND=REG_SP_KIND), INTENT(in) :: IODirn
  INTEGER (KIND=REG_SP_KIND), INTENT(in) :: IOFrequency
  INTEGER (KIND=REG_SP_KIND), INTENT(out):: IOType
  INTEGER (KIND=REG_SP_KIND), INTENT(out):: Status
----------------------------------------------------------------*/

void FUNCTION(register_iotype_f) ARGS(`STRING_ARG(IOLabel),
				       IODirn,
				       IOFrequency,
				       IOType,
				       Status')
STRING_ARG_DECL(IOLabel);
INT_KIND_1_DECL(IODirn);
INT_KIND_1_DECL(IOFrequency);
INT_KIND_1_DECL(IOType);
INT_KIND_1_DECL(Status);
{
  int   len;
  char *buf;

  len = STRING_LEN(IOLabel);
  if(len >= REG_MAX_STRING_LENGTH){
    fprintf(stderr, 
            "register_iotype_f: WARNING: truncating label\n");
    /* Allow space for terminating '/0' */
    len = REG_MAX_STRING_LENGTH - 1;
  }

  buf = (char *)malloc(REG_MAX_STRING_LENGTH);
  if(!buf){

    fprintf(stderr, "Register_IOType_f: malloc failed\n");
    *Status = INT_KIND_1_CAST( REG_FAILURE );
    return;
  }

  /* Convert to null-terminated C-style string */
  memcpy(buf, STRING_PTR(IOLabel), len);
  buf[len] = '\0';

  *Status = INT_KIND_1_CAST( Register_IOType(buf,
                                             (int)*IODirn,
                                             (int)*IOFrequency,
			                     (int *)IOType) );
  free(buf);
  buf = NULL;

  return;
}

/*----------------------------------------------------------------

SUBROUTINE register_chktypes_f(NumTypes, ChkLabel, ChkDirn,
                               ChkFrequency, ChkType, Status)

  INTEGER (KIND=REG_SP_KIND), INTENT(in)                       :: NumTypes
  CHARACTER (LEN=*), DIMENSION(NumTypes), INTENT(in)           :: ChkLabel 
  INTEGER (KIND=REG_SP_KIND), DIMENSION(NumTypes), INTENT(in)  :: ChkDirn
  INTEGER (KIND=REG_SP_KIND), DIMENSION(NumTypes), INTENT(in)  :: ChkFrequency
  INTEGER (KIND=REG_SP_KIND), DIMENSION(NumTypes), INTENT(out) :: ChkType
  INTEGER (KIND=REG_SP_KIND), INTENT(out)                      :: Status

----------------------------------------------------------------*/

void FUNCTION(register_chktypes_f) ARGS(`NumTypes, 
                                        STRING_ARG(ChkLabel), 
                                        ChkDirn,
                                        ChkFrequency,
                                        ChkType,
                                        Status')
INT_KIND_1_DECL(NumTypes);
STRING_ARG_DECL(ChkLabel);
INT_KIND_1_DECL(ChkDirn);
INT_KIND_1_DECL(ChkFrequency);
INT_KIND_1_DECL(ChkType);
INT_KIND_1_DECL(Status);
{
  int    i;
  int    len;
  char **str_array;
  char  *buf;

  len = STRING_LEN(ChkLabel);
  if(len >= REG_MAX_STRING_LENGTH){
    fprintf(stderr, 
            "register_chktypes_f: WARNING: truncating label\n");
    /* Allow space for terminating '/0' */
    len = REG_MAX_STRING_LENGTH - 1;
  }

  str_array = (char**)malloc((*NumTypes)*sizeof(char*));
  buf       = (char*)malloc((*NumTypes)*(len+1));

  if(!str_array || !buf){

    fprintf(stderr, "Register_ChkTypes_f: malloc failed\n");
    *Status = INT_KIND_1_CAST( REG_FAILURE );
    return;
  }

  /* Convert from a single array of char to an array of char* */

  for(i=0; i<(int)(*NumTypes); i++){

    str_array[i] = &(buf[i*(len + 1)]);
    memcpy(str_array[i], 
           &(STRING_PTR(ChkLabel)[i*STRING_LEN(ChkLabel)]),
           len);
    str_array[i][len] = '\0';
  }

  *Status = INT_KIND_1_CAST( Register_ChkTypes((int) *NumTypes,
                                                      str_array,
                                               (int *)ChkDirn,
                                               (int *)ChkFrequency,
                                               (int *)ChkType) );

  free(str_array);
  str_array = NULL;
  free(buf);
  buf = NULL;
  return;
}

/*----------------------------------------------------------------

SUBROUTINE register_chktype_f(ChkLabel, ChkDirn,
                              ChkFrequency, ChkType, Status)

  CHARACTER (LEN=*), DIMENSION(NumTypes), INTENT(in)           :: ChkLabel 
  INTEGER (KIND=REG_SP_KIND), DIMENSION(NumTypes), INTENT(in)  :: ChkDirn
  INTEGER (KIND=REG_SP_KIND), DIMENSION(NumTypes), INTENT(in)  :: ChkFrequency
  INTEGER (KIND=REG_SP_KIND), DIMENSION(NumTypes), INTENT(out) :: ChkType
  INTEGER (KIND=REG_SP_KIND), INTENT(out)                      :: Status

----------------------------------------------------------------*/

void FUNCTION(register_chktype_f) ARGS(`STRING_ARG(ChkLabel), 
                                        ChkDirn,
                                        ChkFrequency,
                                        ChkType,
                                        Status')
STRING_ARG_DECL(ChkLabel);
INT_KIND_1_DECL(ChkDirn);
INT_KIND_1_DECL(ChkFrequency);
INT_KIND_1_DECL(ChkType);
INT_KIND_1_DECL(Status);
{
  int   len;
  char *buf;

  len = STRING_LEN(ChkLabel);
  if(len >= REG_MAX_STRING_LENGTH){
    fprintf(stderr, 
            "register_chktypes_f: WARNING: truncating label\n");
    /* Allow space for terminating '/0' */
    len = REG_MAX_STRING_LENGTH - 1;
  }

  buf = (char*)malloc(len + 1);
  if(!buf){

    fprintf(stderr, "Register_ChkType_f: malloc failed\n");
    *Status = INT_KIND_1_CAST( REG_FAILURE );
    return;
  }

  /* Convert string to a null-terminated C-style string */
  memcpy(buf, STRING_PTR(ChkLabel), len);
  buf[len] = '\0';
 
  *Status = INT_KIND_1_CAST( Register_ChkType(buf,
                                              (int)*ChkDirn,
                                              (int)*ChkFrequency,
                                              (int *)ChkType) );
  free(buf);
  buf = NULL;
  return;
}

/*----------------------------------------------------------------

SUBROUTINE record_chkpt_f(ChkType, ChkTag, Status)

  INTEGER  (KIND=REG_SP_KIND), INTENT(in)           :: ChkType
  CHARACTER (LEN=REG_MAX_STRING_LENGTH), INTENT(in) :: ChkTag 
  INTEGER (KIND=REG_SP_KIND), INTENT(out)           :: Status

----------------------------------------------------------------*/


void FUNCTION(record_chkpt_f) ARGS(`ChkType,
                                    STRING_ARG(ChkTag),
                                    Status')
INT_KIND_1_DECL(ChkType);
STRING_ARG_DECL(ChkTag);
INT_KIND_1_DECL(Status);
{
  char *pchar;
  char  buf[REG_MAX_STRING_LENGTH];
  int   len;
  int   i;
  int   found = 1;

  len = STRING_LEN(ChkTag);

  if(len > REG_MAX_STRING_LENGTH){

    fprintf(stderr, "record_chkpt_f: ERROR - length of tag "
            "exceeds REG_MAX_STRING_LENGTH (%d) chars\n", 
            REG_MAX_STRING_LENGTH);

    *Status = INT_KIND_1_CAST(REG_FAILURE);
    return;
  }

  memcpy(buf, STRING_PTR(ChkTag), len);

  if(len == REG_MAX_STRING_LENGTH){

    found = 0;
    pchar = STRING_PTR(ChkTag);
    for(i = (REG_MAX_STRING_LENGTH-1); i == 0; i--){
      if(pchar[i] == '\0'){
        found = 1;
	break;
      }
    }
    if(!found){

      fprintf(stderr, "record_chkpt_f: ERROR - length of label "
                "is REG_MAX_STRING_LENGTH (%d) chars long\nbut contains "
                "no termination character - shorten label (or its len "
                "declaration)\n", 
                REG_MAX_STRING_LENGTH);
      *Status = INT_KIND_1_CAST(REG_FAILURE);
      return;
    }
  }
  else{

    /* Terminate string */
    buf[len] = '\0';
  }

  *Status = INT_KIND_1_CAST(Record_Chkpt((int)*ChkType, 
                                         buf));

  return;
}

/*----------------------------------------------------------------

SUBROUTINE add_checkpoint_file_f(ChkType, ChkTag, Status)

  INTEGER  (KIND=REG_SP_KIND), INTENT(in)           :: ChkType
  CHARACTER (LEN=REG_MAX_STRING_LENGTH), INTENT(in) :: ChkTag 
  INTEGER (KIND=REG_SP_KIND), INTENT(out)           :: Status

----------------------------------------------------------------*/


void FUNCTION(add_checkpoint_file_f) ARGS(`ChkType,
                                           STRING_ARG(Filename),
                                           Status')
INT_KIND_1_DECL(ChkType);
STRING_ARG_DECL(Filename);
INT_KIND_1_DECL(Status);
{
  int   len;
  char  buf[REG_MAX_STRING_LENGTH];
  char *pchar;
  int   found;
  int   i;

  len = STRING_LEN(Filename);

  if(len > REG_MAX_STRING_LENGTH){

    fprintf(stderr, "add_checkpoint_file_f: ERROR - length of filename "
            "exceeds REG_MAX_STRING_LENGTH (%d) chars\n", 
            REG_MAX_STRING_LENGTH);

    *Status = INT_KIND_1_CAST(REG_FAILURE);
    return;
  }

  memcpy(buf, STRING_PTR(Filename), len);

  if(len == REG_MAX_STRING_LENGTH){

    found = 0;
    pchar = STRING_PTR(Filename);
    for(i = (REG_MAX_STRING_LENGTH-1); i == 0; i--){
      if(pchar[i] == '\0'){
        found = 1;
	break;
      }
    }
    if(!found){

      fprintf(stderr, "add_checkpoint_file_f: ERROR - length of filename "
                "is REG_MAX_STRING_LENGTH (%d) chars long\nbut contains "
                "no termination character - shorten label (or its len "
                "declaration)\n", 
                REG_MAX_STRING_LENGTH);
      *Status = INT_KIND_1_CAST(REG_FAILURE);
      return;
    }
  }
  else{

    /* Terminate string */
    buf[len] = '\0';
  }

  *Status = INT_KIND_1_CAST(Add_checkpoint_file((int)*ChkType, 
                                         	buf) );

  return;
}

/*----------------------------------------------------------------

SUBROUTINE record_checkpoint_set_f(ChkType, ChkTag, Status)

  INTEGER  (KIND=REG_SP_KIND), INTENT(in)           :: ChkType
  CHARACTER (LEN=REG_MAX_STRING_LENGTH), INTENT(in) :: ChkTag 
  INTEGER (KIND=REG_SP_KIND), INTENT(out)           :: Status

----------------------------------------------------------------*/


void FUNCTION(record_checkpoint_set_f) ARGS(`ChkType,
                                             STRING_ARG(ChkTag),
                                             STRING_ARG(Path),
                                             Status')
INT_KIND_1_DECL(ChkType);
STRING_ARG_DECL(ChkTag);
STRING_ARG_DECL(Path);
INT_KIND_1_DECL(Status);
{
  char *pchar;
  char  buf[REG_MAX_STRING_LENGTH];
  char  path_buf[REG_MAX_STRING_LENGTH];
  int   len;
  int   i;
  int   found = 1;

  len = STRING_LEN(ChkTag);

  if(len > REG_MAX_STRING_LENGTH){

    fprintf(stderr, "record_checkpoint_set_f: ERROR - length of tag "
            "exceeds REG_MAX_STRING_LENGTH (%d) chars\n", 
            REG_MAX_STRING_LENGTH);

    *Status = INT_KIND_1_CAST(REG_FAILURE);
    return;
  }

  memcpy(buf, STRING_PTR(ChkTag), len);

  if(len == REG_MAX_STRING_LENGTH){

    found = 0;
    pchar = STRING_PTR(ChkTag);
    for(i = (REG_MAX_STRING_LENGTH-1); i == 0; i--){
      if(pchar[i] == '\0'){
        found = 1;
	break;
      }
    }
    if(!found){

      fprintf(stderr, "record_checkpoint_set_f: ERROR - length of label "
                "is REG_MAX_STRING_LENGTH (%d) chars long\nbut contains "
                "no termination character - shorten label (or its len "
                "declaration)\n", 
                REG_MAX_STRING_LENGTH);
      *Status = INT_KIND_1_CAST(REG_FAILURE);
      return;
    }
  }
  else{

    /* Terminate string */
    buf[len] = '\0';
  }

  len = STRING_LEN(Path);

  if(len > REG_MAX_STRING_LENGTH){

    fprintf(stderr, "record_checkpoint_set_f: ERROR - length of path "
            "exceeds REG_MAX_STRING_LENGTH (%d) chars\n", 
            REG_MAX_STRING_LENGTH);

    *Status = INT_KIND_1_CAST(REG_FAILURE);
    return;
  }

  memcpy(path_buf, STRING_PTR(Path), len);

  if(len == REG_MAX_STRING_LENGTH){

    found = 0;
    pchar = STRING_PTR(Path);
    for(i = (REG_MAX_STRING_LENGTH-1); i == 0; i--){
      if(pchar[i] == '\0'){
        found = 1;
	break;
      }
    }
    if(!found){

      fprintf(stderr, "record_checkpoint_set_f: ERROR - length of path "
                "is REG_MAX_STRING_LENGTH (%d) chars long\nbut contains "
                "no termination character - shorten label (or its len "
                "declaration)\n", 
                REG_MAX_STRING_LENGTH);
      *Status = INT_KIND_1_CAST(REG_FAILURE);
      return;
    }
  }
  else{

    /* Terminate string */
    path_buf[len] = '\0';
  }

  *Status = INT_KIND_1_CAST(Record_checkpoint_set((int)*ChkType, 
                                         	  buf,
                                                  path_buf) );

  return;
}

/*----------------------------------------------------------------

SUBROUTINE register_param_f(ParamLabel, ParamSteerable, ParamPtr, &
                             ParamType, ParamMin, ParamMax, Status)

  CHARACTER (LEN=REG_MAX_STRING_LENGTH), INTENT(in) :: ParamLabel
  INTEGER (KIND=REG_SP_KIND), INTENT(in)            :: ParamSteerable
  XXXXXXX (KIND=REG_DP_KIND), INTENT(in)            :: ParamPtr
  INTEGER (KIND=REG_SP_KIND), INTENT(in)            :: ParamType
  CHARACTER (LEN=REG_MAX_STRING_LENGTH), INTENT(in) :: ParamMin
  CHARACTER (LEN=REG_MAX_STRING_LENGTH), INTENT(in) :: ParamMax
  INTEGER (KIND=REG_SP_KIND), INTENT(out)           :: Status

where XXXXXXX can be INTEGER or REAL of a KIND to give storage no
larger than required by KIND=REG_DP_KIND.  The routine
register_string_param_f should be used if the monitored/steerable
parameter is a CHARACTER string.
----------------------------------------------------------------*/

void FUNCTION(register_param_f) ARGS(`STRING_ARG(ParamLabel),
		                      ParamSteerable,
		                      ParamPtr,
		                      ParamType,
                                      STRING_ARG(ParamMin),
                                      STRING_ARG(ParamMax),
	                              Status')
STRING_ARG_DECL(ParamLabel);
INT_KIND_1_DECL(ParamSteerable);
void   *ParamPtr;
INT_KIND_1_DECL(ParamType);
STRING_ARG_DECL(ParamMin);
STRING_ARG_DECL(ParamMax);
INT_KIND_1_DECL(Status);
{
  char *pbuf[3];
  char *pchar;
  int   i;
  int   found = 0;

  if(STRING_LEN(ParamLabel) > REG_MAX_STRING_LENGTH){

    fprintf(stderr, "register_param_f: ERROR - length of label "
            "exceeds REG_MAX_STRING_LENGTH (%d) chars\n", 
            REG_MAX_STRING_LENGTH);

    *Status = INT_KIND_1_CAST(REG_FAILURE);
    return;
  }

  if(STRING_LEN(ParamLabel) == REG_MAX_STRING_LENGTH){

    pchar = STRING_PTR(ParamLabel);
    for(i = (REG_MAX_STRING_LENGTH-1); i == 0; i--){
      if(pchar[i] == '\0'){
        found = 1;
	break;
      }
    }
    if(!found){

      fprintf(stderr, "register_param_f: ERROR - length of label "
              "is REG_MAX_STRING_LENGTH (%d) chars long\nbut contains "
              "no termination character - shorten label (or its len "
              "declaration)\n", 
              REG_MAX_STRING_LENGTH);

      *Status = INT_KIND_1_CAST(REG_FAILURE);
      return;
    }
  }

  /* Use >= here because I don't want to have to check for '\0'
     if these strings are REG_MAX_STRING_LENGTH long */
  if( (STRING_LEN(ParamMin) >= REG_MAX_STRING_LENGTH) ||
      (STRING_LEN(ParamMax) >= REG_MAX_STRING_LENGTH) ){
    fprintf(stderr, "register_param_f: ERROR - string specifying "
            "max. and/or min. param. value exceeds "
            "REG_MAX_STRING_LENGTH (%d) chars in length\n", 
            REG_MAX_STRING_LENGTH);

    *Status = INT_KIND_1_CAST(REG_FAILURE);
    return;
  }

  if(!(pbuf[0] = (char*)malloc(3*REG_MAX_STRING_LENGTH)) ){

    fprintf(stderr, "register_param_f: ERROR - malloc failed\n");
    *Status = INT_KIND_1_CAST(REG_FAILURE);
    return;
  }
  pbuf[1] = pbuf[0] + REG_MAX_STRING_LENGTH;
  pbuf[2] = pbuf[1] + REG_MAX_STRING_LENGTH;

  /* Terminate strings just in case */
  memcpy(pbuf[0], STRING_PTR(ParamLabel), STRING_LEN(ParamLabel));
  if(!found){
    pbuf[0][STRING_LEN(ParamLabel)] = '\0';
  }

  memcpy(pbuf[1], STRING_PTR(ParamMin), STRING_LEN(ParamMin));
  pbuf[1][STRING_LEN(ParamMin)] = '\0';
  memcpy(pbuf[2], STRING_PTR(ParamMax), STRING_LEN(ParamMax));
  pbuf[2][STRING_LEN(ParamMax)] = '\0';

  *Status = INT_KIND_1_CAST( Register_params(1,
 	                      		     pbuf,
			      		     (int *)ParamSteerable,
			      		     &ParamPtr,
			      		     (int *)ParamType,
                                             &(pbuf[1]),
                                             &(pbuf[2])) );

  free(pbuf[0]);
  pbuf[0] = NULL;
  pbuf[1] = NULL;
  pbuf[2] = NULL;
  return;
}

/*----------------------------------------------------------------

SUBROUTINE register_string_param_f(ParamLabel, ParamSteerable, &
                                   StringParam, Status)

  CHARACTER (LEN=REG_MAX_STRING_LENGTH), INTENT(in) :: ParamLabel
  INTEGER (KIND=REG_SP_KIND), INTENT(in)            :: ParamSteerable
  CHARACTER (LEN=REG_MAX_STRING_LENGTH), INTENT(in) :: StringParam
  INTEGER (KIND=REG_SP_KIND), INTENT(out)           :: Status

----------------------------------------------------------------*/

void FUNCTION(register_string_param_f) ARGS(`STRING_ARG(ParamLabel),
		                       ParamSteerable,
		                       STRING_ARG(StringParam),
	                               Status')
STRING_ARG_DECL(ParamLabel);
INT_KIND_1_DECL(ParamSteerable);
STRING_ARG_DECL(StringParam);
INT_KIND_1_DECL(Status);
{
  char *min  = " ";
  char *pbuf[2];
  char *pchar;
  int   type = REG_CHAR;
  int   i;
  int   found = 0;

  if(STRING_LEN(ParamLabel) > REG_MAX_STRING_LENGTH){

    fprintf(stderr, "register_string_param_f: ERROR - length of label "
            "exceeds REG_MAX_STRING_LENGTH (%d) chars\n", 
            REG_MAX_STRING_LENGTH);

    *Status = INT_KIND_1_CAST(REG_FAILURE);
    return;
  }

  if(STRING_LEN(ParamLabel) == REG_MAX_STRING_LENGTH){

    pchar = STRING_PTR(ParamLabel);
    for(i = (REG_MAX_STRING_LENGTH-1); i == 0; i--){
      if(pchar[i] == '\0'){
        found = 1;
	break;
      }
    }
    if(!found){

      fprintf(stderr, "register_string_param_f: ERROR - length of label "
              "is REG_MAX_STRING_LENGTH (%d) chars long\nbut contains "
              "no termination character - shorten label (or its len "
              "declaration)\n", 
              REG_MAX_STRING_LENGTH);

      *Status = INT_KIND_1_CAST(REG_FAILURE);
      return;
    }
  }

  if(!(pbuf[0] = (char*)malloc(2*REG_MAX_STRING_LENGTH)) ){

    fprintf(stderr, "register_string_param_f: ERROR - malloc failed\n");
    *Status = INT_KIND_1_CAST(REG_FAILURE);
    return;
  }
  pbuf[1] = pbuf[0] + REG_MAX_STRING_LENGTH;

  /* Terminate string just in case */
  memcpy(pbuf[0], STRING_PTR(ParamLabel), STRING_LEN(ParamLabel));
  if(!found){
    pbuf[0][STRING_LEN(ParamLabel)] = '\0';
  }

  /* Max. string length */
  sprintf(pbuf[1], "%d", STRING_LEN(StringParam));

  *Status = INT_KIND_1_CAST( Register_params(1,
 	                      		     pbuf,
			      		     (int *)ParamSteerable,
			      		     (void **)&STRING_PTR(StringParam),
			      		     &type,
                                             &min,
                                             &(pbuf[1])) );

  free(pbuf[0]);
  pbuf[0] = NULL;
  pbuf[1] = NULL;
}

/*----------------------------------------------------------------

SUBROUTINE register_bin_param_f(ParamLabel, ParamPtr, &
                                ParamType, NumObjects, Status)

  CHARACTER (LEN=REG_MAX_STRING_LENGTH), INTENT(in) :: ParamLabel
  XXXXXXX (KIND=REG_DP_KIND), INTENT(in)            :: ParamPtr
  INTEGER (KIND=REG_SP_KIND), INTENT(in)            :: ParamType
  INTEGER (KIND=REG_SP_KIND), INTENT(in)            :: NumObjects
  INTEGER (KIND=REG_SP_KIND), INTENT(out)           :: Status

----------------------------------------------------------------*/
void FUNCTION(register_bin_param_f) ARGS(`STRING_ARG(ParamLabel),
		                          ParamPtr,
		                          ParamType,
                                          NumObjects,
	                                  Status')
STRING_ARG_DECL(ParamLabel);
void   *ParamPtr;
INT_KIND_1_DECL(ParamType);
INT_KIND_1_DECL(NumObjects);
INT_KIND_1_DECL(Status);
{
  int   i, found = 0;
  char  len_buf[16];
  char *pchar, *pbuf;

  if(STRING_LEN(ParamLabel) > REG_MAX_STRING_LENGTH){

    fprintf(stderr, "register_bin_param_f: ERROR - length of label "
            "exceeds REG_MAX_STRING_LENGTH (%d) chars\n", 
            REG_MAX_STRING_LENGTH);

    *Status = INT_KIND_1_CAST(REG_FAILURE);
    return;
  }

  if(STRING_LEN(ParamLabel) == REG_MAX_STRING_LENGTH){

    pchar = STRING_PTR(ParamLabel);
    for(i = (REG_MAX_STRING_LENGTH-1); i == 0; i--){
      if(pchar[i] == '\0'){
        found = 1;
	break;
      }
    }
    if(!found){
      fprintf(stderr, "register_bin_param_f: ERROR - length of label "
              "is REG_MAX_STRING_LENGTH (%d) chars long\nbut contains "
              "no termination character - shorten label (or its len "
              "declaration)\n", 
              REG_MAX_STRING_LENGTH);

      *Status = INT_KIND_1_CAST(REG_FAILURE);
      return;
    }
  }

  if(!(pbuf = (char*)malloc(REG_MAX_STRING_LENGTH)) ){

    fprintf(stderr, "register_bin_param_f: ERROR - malloc failed\n");
    *Status = INT_KIND_1_CAST(REG_FAILURE);
    return;
  }

  /* Terminate label just in case */
  memcpy(pbuf, STRING_PTR(ParamLabel), STRING_LEN(ParamLabel));
  if(!found){
    pbuf[STRING_LEN(ParamLabel)] = '\0';
  }

  if(sizeof_type[*ParamType]){
    /* Calculate how many bytes we're being passed a ptr to */
    sprintf(len_buf, "%d", *NumObjects*sizeof_type[*ParamType]);

    *Status = INT_KIND_1_CAST( Register_param( pbuf,
	  		      		       REG_FALSE,
			      		       ParamPtr,
			      		       REG_BIN,
                                               "",
                                               len_buf ) );
  }
  else{
    fprintf(stderr, "Register_bin_param_f: sizeof type %d is unknown\n", *ParamType);
    *Status = INT_KIND_1_CAST( REG_FAILURE );
  }

  free(pbuf);
  pbuf = NULL;
  return;
}

/*----------------------------------------------------------------

SUBROUTINE enable_all_param_logging_f(Toggle, Status)

  INTEGER (KIND=REG_SP_KIND), INTENT(in)  :: Toggle
  INTEGER (KIND=REG_SP_KIND), INTENT(out) :: Status
----------------------------------------------------------------*/

void FUNCTION(enable_all_param_logging_f) ARGS(`Toggle,
		        	                Status')
INT_KIND_1_DECL(Toggle);
INT_KIND_1_DECL(Status);
{
  *Status = INT_KIND_1_CAST( Enable_all_param_logging((int)*Toggle) );
  return;
}

/*----------------------------------------------------------------

SUBROUTINE enable_param_logging_f(ParamLabel, Toggle, Status)

  CHARACTER (LEN=REG_MAX_STRING_LENGTH), INTENT(in) :: ParamLabel
  INTEGER (KIND=REG_SP_KIND), INTENT(in)            :: Toggle
  INTEGER (KIND=REG_SP_KIND), INTENT(out)           :: Status
----------------------------------------------------------------*/

void FUNCTION(enable_param_logging_f) ARGS(`STRING_ARG(ParamLabel),
                                            Toggle,
		        	            Status')
STRING_ARG_DECL(ParamLabel);
INT_KIND_1_DECL(Toggle);
INT_KIND_1_DECL(Status);
{
  char  buf[REG_MAX_STRING_LENGTH];
  char *pchar;
  int   len, i, found;

  len = STRING_LEN(ParamLabel);
  if(len > REG_MAX_STRING_LENGTH){

    fprintf(stderr, "enable_param_logging_f: ERROR - length of param "
            "label exceeds REG_MAX_STRING_LENGTH (%d) chars\n", 
            REG_MAX_STRING_LENGTH);

    *Status = INT_KIND_1_CAST(REG_FAILURE);
    return;
  }

  memcpy(buf, STRING_PTR(ParamLabel), len);

  if(len == REG_MAX_STRING_LENGTH){

    found = 0;
    pchar = STRING_PTR(ParamLabel);
    for(i = (REG_MAX_STRING_LENGTH-1); i == 0; i--){
      if(pchar[i] == '\0'){
        found = 1;
	break;
      }
    }
    if(!found){

      fprintf(stderr, "Enable_param_logging_f: ERROR - length of label "
                "is REG_MAX_STRING_LENGTH (%d) chars long\nbut contains "
                "no termination character - shorten label (or its len "
                "declaration)\n", 
                REG_MAX_STRING_LENGTH);
      *Status = INT_KIND_1_CAST(REG_FAILURE);
      return;
    }
  }
  else{

    /* Terminate string */
    buf[len] = '\0';
  }

  *Status = INT_KIND_1_CAST( Enable_param_logging(buf, (int)*Toggle) );
  return;
}

/*----------------------------------------------------------------

SUBROUTINE consume_start_f(IOType, IOHandle, Status)

  INTEGER (KIND=REG_SP_KIND), INTENT(in)  :: IOType
  INTEGER (KIND=REG_SP_KIND), INTENT(out) :: IOHandle
  INTEGER (KIND=REG_SP_KIND), INTENT(out) :: Status
----------------------------------------------------------------*/

void FUNCTION(consume_start_f) ARGS(`IOType,
		                     IOHandle,
			             Status')
INT_KIND_1_DECL(IOType);
INT_KIND_1_DECL(IOHandle);
INT_KIND_1_DECL(Status);
{

  *Status = INT_KIND_1_CAST( Consume_start((int)*IOType,
	                                   (int *)IOHandle) );

  if(*Status == REG_SUCCESS){
    Set_f90_array_ordering((int)*IOHandle, REG_TRUE);
  }

  return;
}

/*----------------------------------------------------------------

SUBROUTINE consume_stop_f(IOHandle, Status)

  INTEGER(KIND=REG_SP_KIND), INTENT(inout) :: IOHandle
  INTEGER(KIND=REG_SP_KIND), INTENT(out)   :: Status
----------------------------------------------------------------*/

void FUNCTION(consume_stop_f) ARGS(`IOHandle,
                                    Status')
INT_KIND_1_DECL(IOHandle);
INT_KIND_1_DECL(Status);
{

  *Status = INT_KIND_1_CAST( Consume_stop((int *)IOHandle) );
  return;
}

/*----------------------------------------------------------------

SUBROUTINE consume_data_slice_header_f(IOHandle, DataType, &
                                       Count, Status)

  INTEGER(KIND=REG_SP_KIND), INTENT(in)  :: IOHandle
  INTEGER(KIND=REG_SP_KIND), INTENT(out) :: DataType
  INTEGER(KIND=REG_SP_KIND), INTENT(out) :: Count
  INTEGER(KIND=REG_SP_KIND), INTENT(out) :: Status
----------------------------------------------------------------*/

void FUNCTION(consume_data_slice_header_f) ARGS(`IOHandle,
                                                 DataType,
                                                 Count,
                                                 Status')
INT_KIND_1_DECL(IOHandle);
INT_KIND_1_DECL(DataType);
INT_KIND_1_DECL(Count);
INT_KIND_1_DECL(Status);
{
  *Status = INT_KIND_1_CAST( Consume_data_slice_header((int)*IOHandle,
                                                       (int *)DataType, 
                                                       (int *)Count));
  return;
}

/*----------------------------------------------------------------

SUBROUTINE consume_data_slice_f(IOHandle, pData, Status)

  INTEGER(KIND=REG_SP_KIND), INTENT(in)  :: IOHandle
  INTEGER(KIND=REG_SP_KIND), INTENT(in)  :: DataType
  INTEGER(KIND=REG_SP_KIND), INTENT(in)  :: Count
  XXXXXXX(KIND=REG_DP_KIND), DIMENSION(),INTENT(out) :: pData
  INTEGER(KIND=REG_SP_KIND), INTENT(out) :: Status

----------------------------------------------------------------*/

void FUNCTION(consume_data_slice_f) ARGS(`IOHandle,
                                          DataType,
                                          Count,
                                          pData, 
                                          Status')
INT_KIND_1_DECL(IOHandle);
INT_KIND_1_DECL(DataType);
INT_KIND_1_DECL(Count);
void *pData;
INT_KIND_1_DECL(Status);
{
  *Status = INT_KIND_1_CAST( Consume_data_slice((int)*IOHandle,
                                                (int)*DataType, 
                                                (int)*Count, 
                                                pData) );
  return;
}

/*----------------------------------------------------------------

SUBROUTINE emit_start_f(IOType, SeqNum, IOHandle, Status)

  INTEGER(KIND=REG_SP_KIND), INTENT(in)  :: IOType
  INTEGER(KIND=REG_SP_KIND), INTENT(in)  :: SeqNum
  INTEGER(KIND=REG_SP_KIND), INTENT(out) :: IOHandle
  INTEGER(KIND=REG_SP_KIND), INTENT(out) :: Status

----------------------------------------------------------------*/

void FUNCTION(emit_start_f) ARGS(`IOType, 
                                SeqNum, 
                                IOHandle,
                                Status')
INT_KIND_1_DECL(IOType);
INT_KIND_1_DECL(SeqNum);
INT_KIND_1_DECL(IOHandle);
INT_KIND_1_DECL(Status);
{
  *Status = INT_KIND_1_CAST( Emit_start((int)*IOType, 
                                        (int)*SeqNum, 
                                        (int *)IOHandle) );

  if(*Status == REG_SUCCESS){
    Set_f90_array_ordering((int)*IOHandle, REG_TRUE);
  }

  return;
}

/*----------------------------------------------------------------

SUBROUTINE emit_stop_f(IOHandle, Status)

  INTEGER(KIND=REG_SP_KIND), INTENT(inout) :: IOHandle
  INTEGER(KIND=REG_SP_KIND), INTENT(out)   :: Status

----------------------------------------------------------------*/

void FUNCTION(emit_stop_f) ARGS(`IOHandle,
                                 Status')
INT_KIND_1_DECL(IOHandle);
INT_KIND_1_DECL(Status);
{
  *Status = INT_KIND_1_CAST( Emit_stop(IOHandle) );

  return;
}

/*----------------------------------------------------------------

SUBROUTINE emit_data_slice_f(IOHandle, DataType, Count, pData, Status)

  INTEGER(KIND=REG_SP_KIND), INTENT(in)  :: IOHandle
  INTEGER(KIND=REG_SP_KIND), INTENT(in)  :: DataType
  INTEGER(KIND=REG_SP_KIND), INTENT(in)  :: Count
  XXXXXXX(KIND=REG_DP_KIND), DIMENSION(),INTENT(in) :: pData
  INTEGER(KIND=REG_SP_KIND), INTENT(out) :: Status

----------------------------------------------------------------*/

void FUNCTION(emit_data_slice_f) ARGS(`IOHandle,
                                       DataType,
                                       Count,
                                       pData,
                                       Status')
INT_KIND_1_DECL(IOHandle);
INT_KIND_1_DECL(DataType);
INT_KIND_1_DECL(Count);
void *pData;
INT_KIND_1_DECL(Status);
{
  *Status = INT_KIND_1_CAST( Emit_data_slice(*IOHandle, *DataType, *Count, 
                                             pData) );

  return;
}

/*----------------------------------------------------------------

SUBROUTINE make_vtk_buffer_f(nx, ny, nz, veclen, a, b, c, 
                             array, status)

  INTEGER (KIND=REG_SP_KIND), INTENT(in)  :: nx, ny, nz
  INTEGER (KIND=REG_SP_KIND), INTENT(in)  :: veclen
  REAL (KIND=REG_DP_KIND),    INTENT(in)  :: a, b, c
  REAL (KIND=REG_SP_KIND), DIMENSION(nx*ny*nz), INTENT(out) :: array
  INTEGER (KIND=REG_SP_KIND), INTENT(out) :: status

----------------------------------------------------------------*/

void FUNCTION(make_vtk_buffer_f) ARGS(`nx, ny, nz,
                                       veclen, 
                                       a, b, c,
                                       array,
                                       Status')
INT_KIND_1_DECL(nx);
INT_KIND_1_DECL(ny);
INT_KIND_1_DECL(nz);
INT_KIND_1_DECL(veclen);
/* Really need an M4 macro for this but since this is just a test
   routine, leave for now ARPDBG */
double *a;
double *b;
double *c;
float *array;
INT_KIND_1_DECL(Status);
{
  *Status = INT_KIND_1_CAST( Make_vtk_buffer((int)*nx, 
                                             (int)*ny, 
                                             (int)*nz,
                                             (int)*veclen,
                                             *a, *b, *c,
                                             array) );
  return;
}

/*----------------------------------------------------------------

SUBROUTINE make_vtk_header_f(header, title, nx, ny, nz, veclen, 
                             type, status)

  CHARACTER (LEN=BUFSIZ), INTENT(out)               :: header
  CHARACTER (LEN=REG_MAX_STRING_LENGTH), INTENT(in) :: title
  INTEGER (KIND=REG_SP_KIND), INTENT(in)            :: nx, ny, nz
  INTEGER (KIND=REG_SP_KIND), INTENT(in)            :: veclen
  INTEGER (KIND=REG_SP_KIND), INTENT(out)           :: status

----------------------------------------------------------------*/

void FUNCTION(make_vtk_header_f) ARGS(`STRING_ARG(header),
                                       STRING_ARG(title),
                                       nx, ny, nz,
                                       veclen,
                                       type,
                                       Status')
STRING_ARG_DECL(header);
STRING_ARG_DECL(title);
INT_KIND_1_DECL(nx);
INT_KIND_1_DECL(ny);
INT_KIND_1_DECL(nz);
INT_KIND_1_DECL(veclen);
INT_KIND_1_DECL(type);
INT_KIND_1_DECL(Status);
{
  *Status = INT_KIND_1_CAST( Make_vtk_header(STRING_PTR(header),
                                             STRING_PTR(title),
                                             (int)*nx, 
                                             (int)*ny, 
                                             (int)*nz,
                                             (int)*veclen,
                                             (int)*type) );
  return;
}

/*----------------------------------------------------------------

SUBROUTINE make_chunk_header_f(header, IOindex, sx, sy, sz, nx, ny, nz, 
                               status)

  CHARACTER (LEN=BUFSIZ), INTENT(out)               :: header
  INTEGER (KIND=REG_SP_KIND), INTENT(in)            :: IOindex
  INTEGER (KIND=REG_SP_KIND), INTENT(in)            :: nx, ny, nz
  INTEGER (KIND=REG_SP_KIND), INTENT(in)            :: sz, sy, sz
  INTEGER (KIND=REG_SP_KIND), INTENT(out)           :: status

----------------------------------------------------------------*/

void FUNCTION(make_chunk_header_f) ARGS(`STRING_ARG(header),
                                         IOindex,
                                         totx, toty, totz,
                                         sx, sy, sz,
                                         nx, ny, nz,
                                         Status')
STRING_ARG_DECL(header);
INT_KIND_1_DECL(IOindex);
INT_KIND_1_DECL(totx);
INT_KIND_1_DECL(toty);
INT_KIND_1_DECL(totz);
INT_KIND_1_DECL(sx);
INT_KIND_1_DECL(sy);
INT_KIND_1_DECL(sz);
INT_KIND_1_DECL(nx);
INT_KIND_1_DECL(ny);
INT_KIND_1_DECL(nz);
INT_KIND_1_DECL(Status);
{

  *Status = INT_KIND_1_CAST( Make_chunk_header(STRING_PTR(header),
                                               (int)*IOindex, 
                                               (int)*totx, 
                                               (int)*toty, 
                                               (int)*totz,
                                               (int)*sx, 
                                               (int)*sy, 
                                               (int)*sz,
                                               (int)*nx, 
                                               (int)*ny, 
                                               (int)*nz) );
  return;
}

/*----------------------------------------------------------------

SUBROUTINE steering_control_f(SeqNum, NumSteerParams, SteerParamLabels, &
                              NumSteerCommands, SteerCommands, &
                              SteerCommandParams, Status)

  INTEGER (KIND=REG_SP_KIND), INTENT(in)           :: SeqNum
  INTEGER (KIND=REG_SP_KIND), INTENT(out)          :: NumSteerParams
  CHARACTER (LEN=REG_MAX_STRING_LENGTH)  &
    DIMENSION(REG_MAX_NUM_STR_PARAMS), INTENT(out) :: SteerParamLabels
  INTEGER (KIND=REG_SP_KIND), INTENT(out)          :: NumSteerCommands
  INTEGER (KIND=REG_SP_KIND), DIMENSION(REG_MAX_NUM_STR_CMDS), &
	                              INTENT(out)  :: SteerCommands
  CHARACTER (LEN=REG_MAX_STRING_LENGTH)  &
      DIMENSION(REG_MAX_NUM_STR_CMDS), INTENT(out) :: SteerCommandParams
  INTEGER (KIND=REG_SP_KIND), INTENT(out)          :: Status  
----------------------------------------------------------------*/

void FUNCTION(steering_control_f) ARGS(`SeqNum,
		     	                NumSteerParams,
		     			STRING_ARG(SteerParamLabels),
		     			NumSteerCommands,
		     			SteerCommands,
                                        STRING_ARG(SteerCommandParams),
					Status')
INT_KIND_1_DECL(SeqNum);
INT_KIND_1_DECL(NumSteerParams);
STRING_ARG_DECL(SteerParamLabels);
INT_KIND_1_DECL(NumSteerCommands);
INT_KIND_1_DECL(SteerCommands);
STRING_ARG_DECL(SteerCommandParams);
INT_KIND_1_DECL(Status);
{
  int   i, len, pos;

#if REG_DEBUG
  fprintf(stderr, "steering_control_f: Calling Steering_control...\n");
#endif

  *Status = INT_KIND_1_CAST( Steering_control((int)*SeqNum,
			     		      (int *)NumSteerParams,
			     		      str_array,
			     		      (int *)NumSteerCommands,
			     		      (int *)SteerCommands,
                                              str_array_params) );
#if REG_DEBUG
  fprintf(stderr, 
          "steering_control_f: got %d params and %d cmds\n", 
          *NumSteerParams, *NumSteerCommands);
#endif

  if(*Status == INT_KIND_1_CAST(REG_SUCCESS) ){

    /* ARPDBG Copy each returned string back into single array of char
       to return to caller. This may well fail on Crays. */

    for(i=0; i<(int)(*NumSteerParams); i++){

      strcpy(&(STRING_PTR(SteerParamLabels)[i*STRING_LEN(SteerParamLabels)]),
             str_array[i]);

      /* Blank the remainder of the string being returned to the F90 
         routine */
      len = (int)strlen(str_array[i]);
      pos = i*STRING_LEN(SteerParamLabels) + len;
      
      if( (len = (STRING_LEN(SteerParamLabels) - len)) > 0){

        memset(&(STRING_PTR(SteerParamLabels)[pos]), ' ', len);
      }
    }

    for(i=0; i<(int)(*NumSteerCommands); i++){

      strcpy(&(STRING_PTR(SteerCommandParams)[i*STRING_LEN(SteerCommandParams)]),
             str_array_params[i]);

      /* Blank the remainder of the string being returned to the F90 
         routine */
      len = (int)strlen(str_array_params[i]);
      pos = i*STRING_LEN(SteerCommandParams) + len;
      
      if( (len = (STRING_LEN(SteerCommandParams) - len)) > 0){

        memset(&(STRING_PTR(SteerCommandParams)[pos]), ' ', len);
      }
    }
  }

  return;
}

/*----------------------------------------------------------------

SUBROUTINE steering_pause_f(NumSteerParams, SteerParamLabels, &
                            NumCommands, SteerCommands, &
                            SteerCommandParams, Status)

  INTEGER (KIND=REG_SP_KIND), INTENT(out)          :: NumSteerParams
  CHARACTER (LEN=REG_MAX_STRING_LENGTH)  &
        DIMENSION(MAX_NUM_STR_PARAMS), INTENT(out) :: SteerParamLabels
  INTEGER (KIND=REG_SP_KIND), INTENT(out)          :: NumCommands
  INTEGER (KIND=REG_SP_KIND), DIMENSION(REG_MAX_NUM_STR_CMDS), &
	                              INTENT(out)  :: SteerCommands
  CHARACTER (LEN=REG_MAX_STRING_LENGTH)  &
      DIMENSION(REG_MAX_NUM_STR_CMDS), INTENT(out) :: SteerCommandParams
  INTEGER (KIND=REG_SP_KIND), INTENT(out)          :: Status  
----------------------------------------------------------------*/

void FUNCTION(steering_pause_f) ARGS(`NumSteerParams,
		                      STRING_ARG(SteerParamLabels),
		                      NumCommands,
		                      SteerCommands,
                                      STRING_ARG(SteerCommandParams),
				      Status')
INT_KIND_1_DECL(NumSteerParams);
STRING_ARG_DECL(SteerParamLabels);
INT_KIND_1_DECL(NumCommands);
INT_KIND_1_DECL(SteerCommands);
STRING_ARG_DECL(SteerCommandParams);
INT_KIND_1_DECL(Status);
{
  int   i, j, len, pos;
  char  buf[(REG_MAX_NUM_STR_PARAMS+REG_MAX_NUM_STR_CMDS)*REG_MAX_STRING_LENGTH];
  char *str_array[REG_MAX_NUM_STR_PARAMS];
  char *str_array_params[REG_MAX_NUM_STR_CMDS];

  j = 0;
  for(i=0; i<REG_MAX_NUM_STR_PARAMS; i++){

    str_array[i] = &(buf[j*REG_MAX_STRING_LENGTH]);
    j++;
  }
  for(i=0; i<REG_MAX_NUM_STR_CMDS; i++){

    str_array_params[i] = &(buf[j*REG_MAX_STRING_LENGTH]);
    j++;
  }

  *Status = INT_KIND_1_CAST( Steering_pause((int *)NumSteerParams,
                           		    str_array,
			   		    (int *)NumCommands,
			   		    (int *)SteerCommands,
                                            str_array_params) );

  if(*Status == INT_KIND_1_CAST(REG_SUCCESS) ){

    /* Copy each returned string back into single array of char
       to return to caller. This may well fail on Crays. */

    for(i=0; i<(int)(*NumSteerParams); i++){

      strcpy(&(STRING_PTR(SteerParamLabels)[i*STRING_LEN(SteerParamLabels)]),
             str_array[i]);

      /* Blank the remainder of the string being returned to the F90 
         routine */
      len = (int)strlen(str_array[i]);
      pos = i*STRING_LEN(SteerParamLabels) + len;
      
      if( (len = (STRING_LEN(SteerParamLabels) - len)) > 0){

        memset(&(STRING_PTR(SteerParamLabels)[pos]), ' ', len);
      }
    }

    for(i=0; i<(int)(*NumCommands); i++){

      strcpy(&(STRING_PTR(SteerCommandParams)[i*STRING_LEN(SteerCommandParams)]),
             str_array_params[i]);

      /* Blank the remainder of the string being returned to the F90 
         routine */
      len = (int)strlen(str_array_params[i]);
      pos = i*STRING_LEN(SteerCommandParams) + len;
      
      if( (len = (STRING_LEN(SteerCommandParams) - len)) > 0){

        memset(&(STRING_PTR(SteerCommandParams)[pos]), ' ', len);
      }
    }
  }

  return;
}

/*----------------------------------------------------------------

SUBROUTINE steering_sleep_f()
----------------------------------------------------------------*/

void FUNCTION(steering_sleep_f) ARGS(`')
{
  sleep(2);
  return;
}

/*----------------------------------------------------------------

SUBROUTINE steering_char_to_ptr_f(string, ptr)

  CHARACTER (LEN=REG_MAX_STRING_LENGTH), INTENT(in) :: string
  INTEGER (KIND=REG_DP_KIND), INTENT(out)           :: ptr
----------------------------------------------------------------*/

void FUNCTION(steering_char_to_ptr_f) ARGS(`STRING_ARG(string),
                                            ptr')
STRING_ARG_DECL(string);
INT_KIND_1D0_DECL(ptr);
{
  /* Obtain a pointer to the supplied character string.  This is intended
     to be passed to register_params_f as type void*. */

#if REG_DEBUG
  fprintf(stderr, "steering_char_to_ptr_f: Entered routine, "
	  "string = %s\n", STRING_PTR(string));
#endif

  *ptr = INT_KIND_1D0_CAST(STRING_PTR(string));

#if REG_DEBUG
  fprintf(stderr, "steering_char_to_ptr_f: Leaving routine\n");
#endif

}

/*----------------------------------------------------------------

SUBROUTINE set_type_size(Type, Ptr1, Ptr2, Status)

  INTEGER (KIND=REG_SP_KIND), INTENT(in)  :: Type
  XXXXXXX, INTENT(in)                     :: Ptr1, Ptr2
  INTEGER (KIND=REG_SP_KIND), INTENT(out) :: Status
----------------------------------------------------------------*/

void FUNCTION(set_type_size) ARGS(`Type, Ptr1, Ptr2, Status')
INT_KIND_1_DECL(Type);
void *Ptr1;
void *Ptr2;
INT_KIND_1_DECL(Status);
{
  int size;

  if(Ptr2 < Ptr1){
    fprintf(stderr, "set_type_size: ERROR: Ptr2 < Ptr1; arguments"
            " ordered incorrectly?\n");
    *Status = INT_KIND_1_CAST(REG_FAILURE);
    return;
  }

  size = (int)(Ptr2 - Ptr1);

  switch(*Type){

    case REG_CHAR:
      sizeof_type[REG_CHAR] = size;
      break;

    case REG_INT:
      sizeof_type[REG_INT] = size;
      break;

    case REG_FLOAT:
      sizeof_type[REG_FLOAT] = size;
      break;

    case REG_DBL:
      sizeof_type[REG_DBL] = size;
      break;

    default:
      fprintf(stderr, "set_type_size: ERROR: unrecognised type\n");
      *Status = INT_KIND_1_CAST(REG_FAILURE);
      break;
  }

  *Status = INT_KIND_1_CAST(REG_SUCCESS);
  return;
}
