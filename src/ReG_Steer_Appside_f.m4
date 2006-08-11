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

#include "ReG_Steer_Appside.h"
#include <unistd.h>
#include <string.h>

#ifndef REG_DEBUG
#define REG_DEBUG 1
#endif

/** @file ReG_Steer_Appside_f.c
    @brief Generated from ReG_Steer_Appside_f.m4 
    @author Andrew Porter
    @author Robert Haines

    This file contains wrapper routines allowing the routines for the 
    construction of an interface to a steering component (from an 
    application component) to be called from F90.  The m4 macros used
    to create the platform-specific C functions are based on those from the
    PVM distribution.  See the readme file in the 'conf' directory.
*/

/** Large global string buffer.  str_array and str_array_params are
    initialized to point into this array. */
static char gStringBuf[(REG_MAX_NUM_STR_PARAMS+REG_MAX_NUM_STR_CMDS)
                       *REG_MAX_STRING_LENGTH];
/** Global array of char* pointers - used for passing parameter
    labels into the steering library.  Initialised in 
    steering_initialize_f. */
static char *str_array[REG_MAX_NUM_STR_PARAMS];
/** Global array of char* pointers - used for passing parameter info
    associated with commands.  Initialised in steering_initialize_f. */
static char *str_array_params[REG_MAX_NUM_STR_CMDS];
/** Global array to store steering commands being sent or received */
static int   gSteerCommands[REG_MAX_NUM_STR_CMDS];
/** Global array to store size in bytes of each of ReG type */
static int sizeof_type[10] = {0,0,0,0,0,0,0,0,0,0};
/** Global array to store mapping of F90 type to equivalent C type */
static int f90_to_c_type[10] = {0,0,0,0,0,0,0,0,0,0};

/*----------------------------------------------------------------

SUBROUTINE steering_enable_f(EnableSteer)

  INTEGER (KIND=REG_SP_KIND), INTENT(in) :: EnableSteer
----------------------------------------------------------------*/

/** Wrapper for Steering_enable(), for use from within F90 */
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

/** Wrapper for Steering_initialize(), for use from within F90 
    @param Status Return status of the call */
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

    str_array[i] = &(gStringBuf[j*REG_MAX_STRING_LENGTH]);
    j++;
  }
  for(i=0; i<REG_MAX_NUM_STR_CMDS; i++){

    str_array_params[i] = &(gStringBuf[j*REG_MAX_STRING_LENGTH]);
    j++;
  }

  /* Set flag to indicate library is being called from a F90 code */
  *Status = INT_KIND_1_CAST( Called_from_f90(REG_TRUE) );
  if(*Status != REG_SUCCESS){
    return;
  }

  len = STRING_LEN(AppName);

  if(len > REG_MAX_STRING_LENGTH){

    fprintf(stderr, "STEER: ERROR: steering_initialize_f: length of tag "
            "exceeds REG_MAX_STRING_LENGTH (%d) chars\n", 
            REG_MAX_STRING_LENGTH);

    *Status = INT_KIND_1_CAST(REG_FAILURE);
    return;
  }

  memcpy(gStringBuf, STRING_PTR(AppName), len);

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

      fprintf(stderr, "STEER: ERROR: steering_initialize_f: length of tag "
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
    gStringBuf[len] = '\0';
  }

  /* We have to be careful about casting (esp. on 64-bit systems) */
  for(i=0; i<(int)(*NumSupportedCmds); i++){
    /*printf("ARPDBG, supp. cmd %d: %ld\n", i, SupportedCmds[i]);*/
    gSteerCommands[i] = (int)(SupportedCmds[i]);
  }

  *Status = INT_KIND_1_CAST( Steering_initialize(gStringBuf,
                                                 (int)(*NumSupportedCmds), 
	 	                                 gSteerCommands) );
  return;
}

/*----------------------------------------------------------------

SUBROUTINE steering_finalize_f(Status)

  INTEGER (KIND=REG_SP_KIND), INTENT(out) :: Status
----------------------------------------------------------------*/

/** Wrapper for Steering_finalize(), for use from within F90
    @param Status Return status of the call, REG_SUCCESS or 
    REG_FAILURE */
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

/** Wrapper for Enable_IOTypes_on_registration(), for use from 
    within F90
    @param Status Return status of the call, REG_SUCCESS or 
    REG_FAILURE */
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

/** Wrapper for Enable_IOType(), for use from within F90
    @param Status Return status of the call, REG_SUCCESS or 
    REG_FAILURE */
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

/** Wrapper for Disable_IOType(), for use from within F90
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
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

/** Wrapper for Enable_IOType_acks(), for use from within F90
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
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

/** Wrapper for Disable_IOType_acks(), for use from within F90
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
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

/** Wrapper for Register_IOTypes(), for use from within F90
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
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
  int    i, lNumTypes;
  int    len;
  char **str_array;
  int    *typeArray;
  int    *dirnArray;
  int    *freqArray;

  len = STRING_LEN(IOLabel);
  if(len >= REG_MAX_STRING_LENGTH){
    fprintf(stderr, 
            "STEER: WARNING: register_iotypes_f: truncating label\n");
    /* Allow space for terminating '/0' */
    len = REG_MAX_STRING_LENGTH - 1;
  }

  lNumTypes = (int)(*NumTypes);

  str_array = (char**)malloc(lNumTypes*sizeof(char*));
  typeArray = (int *)malloc(lNumTypes*sizeof(int));
  dirnArray = (int *)malloc(lNumTypes*sizeof(int));
  freqArray = (int *)malloc(lNumTypes*sizeof(int));

  if(!str_array  || !typeArray || !dirnArray || !freqArray){

    fprintf(stderr, "STEER: Register_IOTypes_f: malloc failed\n");
    *Status = INT_KIND_1_CAST( REG_FAILURE );
    return;
  }

  for(i=0; i<lNumTypes; i++){

    str_array[i] = (char *)malloc(REG_MAX_STRING_LENGTH);
  }

  /* Convert from a single array of char to an array of char* */

  for(i=0; i<lNumTypes; i++){

    memcpy(str_array[i], 
           &(STRING_PTR(IOLabel)[i*STRING_LEN(IOLabel)]),
           len);
    str_array[i][len] = '\0';

    dirnArray[i] = (int)(IODirn[i]);
    freqArray[i] = (int)(IOFrequency[i]);
  }

  *Status = INT_KIND_1_CAST( Register_IOTypes(lNumTypes,
			                      str_array,
                                              dirnArray,
                                              freqArray,
			                      typeArray) );
  for(i=0; i<lNumTypes; i++){
    IOType[i] = INT_KIND_1_CAST(typeArray[i]);
    free(str_array[i]);
  }

  free(str_array);
  str_array = NULL;

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

/** Wrapper for Register_IOType(), for use from within F90 
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
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
  int   len, dirn, freq, type;

  len = STRING_LEN(IOLabel);
  if(len >= REG_MAX_STRING_LENGTH){
    fprintf(stderr, 
            "STEER: WARNING: register_iotype_f: truncating label\n");
    /* Allow space for terminating '/0' */
    len = REG_MAX_STRING_LENGTH - 1;
  }

  /* Convert to null-terminated C-style string */
  memcpy(gStringBuf, STRING_PTR(IOLabel), len);
  gStringBuf[len] = '\0';

  dirn = (int)*IODirn;
  freq = (int)*IOFrequency;
  
  *Status = INT_KIND_1_CAST( Register_IOType(gStringBuf, dirn,
                                             freq, &type) );
  *IOType = INT_KIND_1_CAST(type);

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

/** Wrapper for Register_ChkTypes(), for use from F90
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
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
  int    lNumTypes;
  char **str_array;
  char  *buf;

  len = STRING_LEN(ChkLabel);
  if(len >= REG_MAX_STRING_LENGTH){
    fprintf(stderr, 
            "STEER: WARNING: register_chktypes_f: truncating label\n");
    /* Allow space for terminating '/0' */
    len = REG_MAX_STRING_LENGTH - 1;
  }

  lNumTypes = (int)(*NumTypes);
  str_array = (char**)malloc(lNumTypes*sizeof(char*));
  buf       = (char*)malloc(lNumTypes*(len+1));

  if(!str_array || !buf){

    fprintf(stderr, "STEER: Register_ChkTypes_f: malloc failed\n");
    *Status = INT_KIND_1_CAST( REG_FAILURE );
    return;
  }

  /* Convert from a single array of char to an array of char* */

  for(i=0; i<lNumTypes; i++){

    str_array[i] = &(buf[i*(len + 1)]);
    memcpy(str_array[i], 
           &(STRING_PTR(ChkLabel)[i*STRING_LEN(ChkLabel)]),
           len);
    str_array[i][len] = '\0';
  }

  *Status = INT_KIND_1_CAST( Register_ChkTypes(lNumTypes,
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

/** Wrapper for Register_ChkType(), for use from F90
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
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
  int len;

  len = STRING_LEN(ChkLabel);
  if(len >= REG_MAX_STRING_LENGTH){
    fprintf(stderr, 
            "STEER: WARNING: register_chktypes_f: truncating label\n");
    /* Allow space for terminating '/0' */
    len = REG_MAX_STRING_LENGTH - 1;
  }

  /* Convert string to a null-terminated C-style string */
  memcpy(gStringBuf, STRING_PTR(ChkLabel), len);
  gStringBuf[len] = '\0';
 
  *Status = INT_KIND_1_CAST( Register_ChkType(gStringBuf,
                                              (int)*ChkDirn,
                                              (int)*ChkFrequency,
                                              (int *)ChkType) );
  return;
}

/*----------------------------------------------------------------

SUBROUTINE record_chkpt_f(ChkType, ChkTag, Status)

  INTEGER  (KIND=REG_SP_KIND), INTENT(in)           :: ChkType
  CHARACTER (LEN=REG_MAX_STRING_LENGTH), INTENT(in) :: ChkTag 
  INTEGER (KIND=REG_SP_KIND), INTENT(out)           :: Status

----------------------------------------------------------------*/

/** Wrapper for Record_Chkpt(), for use from F90
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
void FUNCTION(record_chkpt_f) ARGS(`ChkType,
                                    STRING_ARG(ChkTag),
                                    Status')
INT_KIND_1_DECL(ChkType);
STRING_ARG_DECL(ChkTag);
INT_KIND_1_DECL(Status);
{
  char *pchar;
  int   len;
  int   i;
  int   found = 1;

  len = STRING_LEN(ChkTag);

  if(len > REG_MAX_STRING_LENGTH){

    fprintf(stderr, "STEER: ERROR: record_chkpt_f: length of tag "
            "exceeds REG_MAX_STRING_LENGTH (%d) chars\n", 
            REG_MAX_STRING_LENGTH);

    *Status = INT_KIND_1_CAST(REG_FAILURE);
    return;
  }

  memcpy(gStringBuf, STRING_PTR(ChkTag), len);

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

      fprintf(stderr, "STEER: ERROR: record_chkpt_f: length of label "
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
    gStringBuf[len] = '\0';
  }

  *Status = INT_KIND_1_CAST(Record_Chkpt((int)*ChkType, 
                                         gStringBuf));

  return;
}

/*----------------------------------------------------------------

SUBROUTINE add_checkpoint_file_f(ChkType, ChkTag, Status)

  INTEGER  (KIND=REG_SP_KIND), INTENT(in)           :: ChkType
  CHARACTER (LEN=REG_MAX_STRING_LENGTH), INTENT(in) :: ChkTag 
  INTEGER (KIND=REG_SP_KIND), INTENT(out)           :: Status

----------------------------------------------------------------*/

/** Wrapper for Add_checkpoint_file(), for use from F90
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
void FUNCTION(add_checkpoint_file_f) ARGS(`ChkType,
                                           STRING_ARG(Filename),
                                           Status')
INT_KIND_1_DECL(ChkType);
STRING_ARG_DECL(Filename);
INT_KIND_1_DECL(Status);
{
  int   len;
  char *pchar;
  int   found;
  int   i;

  len = STRING_LEN(Filename);

  if(len > REG_MAX_STRING_LENGTH){

    fprintf(stderr, "STEER: ERROR: add_checkpoint_file_f: length of filename "
            "exceeds REG_MAX_STRING_LENGTH (%d) chars\n", 
            REG_MAX_STRING_LENGTH);

    *Status = INT_KIND_1_CAST(REG_FAILURE);
    return;
  }

  memcpy(gStringBuf, STRING_PTR(Filename), len);

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

      fprintf(stderr, "STEER: ERROR: add_checkpoint_file_f: length of filename "
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
    gStringBuf[len] = '\0';
  }

  *Status = INT_KIND_1_CAST(Add_checkpoint_file((int)*ChkType, 
                                         	gStringBuf) );

  return;
}

/*----------------------------------------------------------------

SUBROUTINE record_checkpoint_set_f(ChkType, ChkTag, Status)

  INTEGER  (KIND=REG_SP_KIND), INTENT(in)           :: ChkType
  CHARACTER (LEN=REG_MAX_STRING_LENGTH), INTENT(in) :: ChkTag 
  INTEGER (KIND=REG_SP_KIND), INTENT(out)           :: Status

----------------------------------------------------------------*/

/** Wrapper for Record_checkpoint_set(), for use from F90
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
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
  char  path_buf[REG_MAX_STRING_LENGTH];
  int   len;
  int   i;
  int   found = 1;

  len = STRING_LEN(ChkTag);

  if(len > REG_MAX_STRING_LENGTH){

    fprintf(stderr, "STEER: ERROR: record_checkpoint_set_f: length of tag "
            "exceeds REG_MAX_STRING_LENGTH (%d) chars\n", 
            REG_MAX_STRING_LENGTH);

    *Status = INT_KIND_1_CAST(REG_FAILURE);
    return;
  }

  memcpy(gStringBuf, STRING_PTR(ChkTag), len);

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

      fprintf(stderr, "STEER: ERROR: record_checkpoint_set_f: length of label "
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
    gStringBuf[len] = '\0';
  }

  len = STRING_LEN(Path);

  if(len > REG_MAX_STRING_LENGTH){

    fprintf(stderr, "STEER: ERROR: record_checkpoint_set_f: length of path "
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

      fprintf(stderr, "STEER: ERROR: record_checkpoint_set_f: length of path "
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
                                         	  gStringBuf,
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

/** Wrapper for Register_param(), for use from F90
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
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

    fprintf(stderr, "STEER: ERROR: register_param_f: length of label "
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

      fprintf(stderr, "STEER: ERROR: register_param_f: length of label "
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
    fprintf(stderr, "STEER: ERROR: register_param_f: string specifying "
            "max. and/or min. param. value exceeds "
            "REG_MAX_STRING_LENGTH (%d) chars in length\n", 
            REG_MAX_STRING_LENGTH);

    *Status = INT_KIND_1_CAST(REG_FAILURE);
    return;
  }

  /* Following code assumes 
     (REG_MAX_NUM_STR_PARAMS+REG_MAX_NUM_STR_CMDS) >= 3 */
  pbuf[0] = &(gStringBuf[0]);
  pbuf[1] = pbuf[0] + REG_MAX_STRING_LENGTH;
  pbuf[2] = pbuf[1] + REG_MAX_STRING_LENGTH;
/*
  if(!(pbuf[0] = (char*)malloc(3*REG_MAX_STRING_LENGTH)) ){

    fprintf(stderr, "STEER: ERROR: register_param_f: malloc failed\n");
    *Status = INT_KIND_1_CAST(REG_FAILURE);
    return;
  }
  pbuf[1] = pbuf[0] + REG_MAX_STRING_LENGTH;
  pbuf[2] = pbuf[1] + REG_MAX_STRING_LENGTH;
*/
  /* Terminate strings just in case */
  memcpy(pbuf[0], STRING_PTR(ParamLabel), STRING_LEN(ParamLabel));
  if(!found){
    pbuf[0][STRING_LEN(ParamLabel)] = '\0';
  }

  memcpy(pbuf[1], STRING_PTR(ParamMin), STRING_LEN(ParamMin));
  pbuf[1][STRING_LEN(ParamMin)] = '\0';
  memcpy(pbuf[2], STRING_PTR(ParamMax), STRING_LEN(ParamMax));
  pbuf[2][STRING_LEN(ParamMax)] = '\0';

  *Status = INT_KIND_1_CAST( Register_param(pbuf[0],
			      		    (int)*ParamSteerable,
			      		    ParamPtr,
			      		    (int)*ParamType,
                                            pbuf[1],
                                            pbuf[2]) );
/*
  free(pbuf[0]);
  pbuf[0] = NULL;
  pbuf[1] = NULL;
  pbuf[2] = NULL;
*/
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

/** F90-only.  For registering a monitored/steerable parameter that
    is of type CHARACTER. The maximum length of the string is 
    automatically obtained and stored along with the registered parameter.
    @param ParamLabel A string containing the label of this parameter 
    @param ParamSteerable Whether (REG_TRUE) or not (REG_FALSE) this
           parameter is steerable
    @param StringParam The string variable to register
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE
    @see Register_param() */
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

    fprintf(stderr, "STEER: ERROR: register_string_param_f: length of label "
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

      fprintf(stderr, "STEER: ERROR: register_string_param_f: length of label "
              "is REG_MAX_STRING_LENGTH (%d) chars long\nbut contains "
              "no termination character - shorten label (or its len "
              "declaration)\n", 
              REG_MAX_STRING_LENGTH);

      *Status = INT_KIND_1_CAST(REG_FAILURE);
      return;
    }
  }

  /* User global buffer rather than malloc - assumes that
       (REG_MAX_NUM_STR_PARAMS+REG_MAX_NUM_STR_CMDS) >= 2 */
  pbuf[0] = &(gStringBuf[0]);
  pbuf[1] = pbuf[0] + REG_MAX_STRING_LENGTH;
/*
  if(!(pbuf[0] = (char*)malloc(2*REG_MAX_STRING_LENGTH)) ){

    fprintf(stderr, "STEER: ERROR: register_string_param_f: malloc failed\n");
    *Status = INT_KIND_1_CAST(REG_FAILURE);
    return;
  }
  pbuf[1] = pbuf[0] + REG_MAX_STRING_LENGTH;
*/

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
/*
  free(pbuf[0]);
  pbuf[0] = NULL;
  pbuf[1] = NULL; */
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

/** Wrapper for Register_bin_param(), for use from F90.
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
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

    fprintf(stderr, "STEER: ERROR: register_bin_param_f: length of label "
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
      fprintf(stderr, "STEER: ERROR: register_bin_param_f: length of label "
              "is REG_MAX_STRING_LENGTH (%d) chars long\nbut contains "
              "no termination character - shorten label (or its len "
              "declaration)\n", 
              REG_MAX_STRING_LENGTH);

      *Status = INT_KIND_1_CAST(REG_FAILURE);
      return;
    }
  }

  if(!(pbuf = (char*)malloc(REG_MAX_STRING_LENGTH)) ){

    fprintf(stderr, "STEER: ERROR: register_bin_param_f: malloc failed\n");
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
    fprintf(stderr, "STEER: Register_bin_param_f: sizeof type %d is unknown\n", 
            *ParamType);
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

/** Wrapper for Enable_all_param_logging(), for use from F90.
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
void FUNCTION(enable_all_param_logging_f) ARGS(`Toggle,
		        	                Status')
INT_KIND_1_DECL(Toggle);
INT_KIND_1_DECL(Status);
{
  *Status = INT_KIND_1_CAST( Enable_all_param_logging((int)*Toggle) );
  return;
}

/*----------------------------------------------------------------
SUBROUTINE enable_param_logging_f(ParamLabel, Status)

  CHARACTER (LEN=REG_MAX_STRING_LENGTH), INTENT(in) :: ParamLabel
  INTEGER (KIND=REG_SP_KIND), INTENT(out)           :: Status
----------------------------------------------------------------*/

/** Wrapper for Enable_param_logging(), for use from F90.
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
void FUNCTION(enable_param_logging_f) ARGS(`STRING_ARG(ParamLabel),
		        	            Status')
STRING_ARG_DECL(ParamLabel);
INT_KIND_1_DECL(Status);
{
  char *pchar;
  int   len, i, found;

  len = STRING_LEN(ParamLabel);
  if(len > REG_MAX_STRING_LENGTH){

    fprintf(stderr, "STEER: ERROR: enable_param_logging_f: length of param "
            "label exceeds REG_MAX_STRING_LENGTH (%d) chars\n", 
            REG_MAX_STRING_LENGTH);

    *Status = INT_KIND_1_CAST(REG_FAILURE);
    return;
  }

  memcpy(gStringBuf, STRING_PTR(ParamLabel), len);

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

      fprintf(stderr, "STEER: ERROR: enable_param_logging_f: length of label "
              "is REG_MAX_STRING_LENGTH (%d) chars long\nbut contains "
              "no termination character - shorten label (or its len "
              "declaration)\n", REG_MAX_STRING_LENGTH);
      *Status = INT_KIND_1_CAST(REG_FAILURE);
      return;
    }
  }
  else{
    /* Terminate string */
    gStringBuf[len] = '\0';
  }

  *Status = INT_KIND_1_CAST( Enable_param_logging(gStringBuf) );
  return;
}

/*----------------------------------------------------------------
SUBROUTINE disable_param_logging_f(ParamLabel, Status)

  CHARACTER (LEN=REG_MAX_STRING_LENGTH), INTENT(in) :: ParamLabel
  INTEGER (KIND=REG_SP_KIND), INTENT(out)           :: Status
----------------------------------------------------------------*/

/** Wrapper for Disable_param_logging(), for use from F90.
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
void FUNCTION(disable_param_logging_f) ARGS(`STRING_ARG(ParamLabel),
		        	             Status')
STRING_ARG_DECL(ParamLabel);
INT_KIND_1_DECL(Status);
{
  char *pchar;
  int   len, i, found;

  len = STRING_LEN(ParamLabel);
  if(len > REG_MAX_STRING_LENGTH){

    fprintf(stderr, "STEER: ERROR: disable_param_logging_f: length of param "
            "label exceeds REG_MAX_STRING_LENGTH (%d) chars\n", 
            REG_MAX_STRING_LENGTH);

    *Status = INT_KIND_1_CAST(REG_FAILURE);
    return;
  }

  memcpy(gStringBuf, STRING_PTR(ParamLabel), len);

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

      fprintf(stderr, "STEER: ERROR: disable_param_logging_f: length of label "
              "is REG_MAX_STRING_LENGTH (%d) chars long\nbut contains "
              "no termination character - shorten label (or its len "
              "declaration)\n", REG_MAX_STRING_LENGTH);
      *Status = INT_KIND_1_CAST(REG_FAILURE);
      return;
    }
  }
  else{
    /* Terminate string */
    gStringBuf[len] = '\0';
  }

  *Status = INT_KIND_1_CAST( Disable_param_logging(gStringBuf) );
  return;
}

/*----------------------------------------------------------------
SUBROUTINE consume_start_f(IOType, IOHandle, Status)

  INTEGER (KIND=REG_SP_KIND), INTENT(in)  :: IOType
  INTEGER (KIND=REG_SP_KIND), INTENT(out) :: IOHandle
  INTEGER (KIND=REG_SP_KIND), INTENT(out) :: Status
----------------------------------------------------------------*/
/** Wrapper for Consume_start(), for use from within F90.
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
void FUNCTION(consume_start_f) ARGS(`IOType,
		                     IOHandle,
			             Status')
INT_KIND_1_DECL(IOType);
INT_KIND_1_DECL(IOHandle);
INT_KIND_1_DECL(Status);
{
  int handle;

  *Status = INT_KIND_1_CAST( Consume_start((int)*IOType,
	                                   &handle) );
  *IOHandle = INT_KIND_1_CAST(handle);

  if(*Status == REG_SUCCESS){
    Set_f90_array_ordering((int)*IOHandle, REG_TRUE);
  }

  return;
}

/*----------------------------------------------------------------
SUBROUTINE consume_start_blocking_f(IOType, IOHandle, TimeOut, &
                                    Status)

  INTEGER (KIND=REG_SP_KIND), INTENT(in)  :: IOType
  INTEGER (KIND=REG_SP_KIND), INTENT(out) :: IOHandle
  REAL    (KIND=REG_SP_KIND), INTENT(in)  :: TimeOut
  INTEGER (KIND=REG_SP_KIND), INTENT(out) :: Status
----------------------------------------------------------------*/
/** Wrapper for Consume_start_blocking(), for use from within F90.
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
void FUNCTION(consume_start_blocking_f) ARGS(`IOType,
		                              IOHandle,
                                              TimeOut,
			                      Status')
INT_KIND_1_DECL(IOType);
INT_KIND_1_DECL(IOHandle);
float *TimeOut;
INT_KIND_1_DECL(Status);
{
  int handle;
  *Status = INT_KIND_1_CAST( Consume_start_blocking((int)*IOType,
	                                            &handle,
                                                    *TimeOut) );
  *IOHandle = INT_KIND_1_CAST(handle);

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
/** Wrapper for Consume_stop(), for use from within F90.
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
void FUNCTION(consume_stop_f) ARGS(`IOHandle,
                                    Status')
INT_KIND_1_DECL(IOHandle);
INT_KIND_1_DECL(Status);
{
  int handle = (int)*IOHandle;
  *Status = INT_KIND_1_CAST( Consume_stop(&handle) );
  *IOHandle = INT_KIND_1_CAST(handle);
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
/** Wrapper for Consume_data_slice_header(), for use from within F90.
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
void FUNCTION(consume_data_slice_header_f) ARGS(`IOHandle,
                                                 DataType,
                                                 Count,
                                                 Status')
INT_KIND_1_DECL(IOHandle);
INT_KIND_1_DECL(DataType);
INT_KIND_1_DECL(Count);
INT_KIND_1_DECL(Status);
{
  int lCount, lType;
  *Status = INT_KIND_1_CAST( Consume_data_slice_header((int)*IOHandle,
                                                       &lType, 
                                                       &lCount));
  *DataType = INT_KIND_1_CAST(lType);
  *Count = INT_KIND_1_CAST(lCount);
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
/** Wrapper for Consume_data_slice(), for use from within F90.
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
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
/** Wrapper for Emit_start(), for use from within F90.
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
void FUNCTION(emit_start_f) ARGS(`IOType, 
                                SeqNum, 
                                IOHandle,
                                Status')
INT_KIND_1_DECL(IOType);
INT_KIND_1_DECL(SeqNum);
INT_KIND_1_DECL(IOHandle);
INT_KIND_1_DECL(Status);
{
  int handle;
  *Status = INT_KIND_1_CAST( Emit_start((int)*IOType, 
                                        (int)*SeqNum, 
                                        &handle) );
  *IOHandle = INT_KIND_1_CAST(handle);

  if(*Status == REG_SUCCESS){
    Set_f90_array_ordering((int)*IOHandle, REG_TRUE);
  }

  return;
}

/*----------------------------------------------------------------
SUBROUTINE emit_start_blocking_f(IOType, SeqNum, IOHandle, &
                                 TimeOut, Status)

  INTEGER(KIND=REG_SP_KIND), INTENT(in)  :: IOType
  INTEGER(KIND=REG_SP_KIND), INTENT(in)  :: SeqNum
  INTEGER(KIND=REG_SP_KIND), INTENT(out) :: IOHandle
  REAL(KIND=REG_SP_KIND),    INTENT(in)  :: TimeOut
  INTEGER(KIND=REG_SP_KIND), INTENT(out) :: Status
----------------------------------------------------------------*/
/** Wrapper for Emit_start_blocking(), for use from within F90.
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
void FUNCTION(emit_start_blocking_f) ARGS(`IOType, 
                                           SeqNum, 
                                           IOHandle,
                                           TimeOut,
                                           Status')
INT_KIND_1_DECL(IOType);
INT_KIND_1_DECL(SeqNum);
INT_KIND_1_DECL(IOHandle);
float *TimeOut;
INT_KIND_1_DECL(Status);
{
  int handle;
  *Status = INT_KIND_1_CAST( Emit_start_blocking((int)*IOType, 
                                                 (int)*SeqNum, 
                                                 &handle,
                                                 *TimeOut) );
  *IOHandle = INT_KIND_1_CAST(handle);

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
/** Wrapper for Emit_stop(), for use from within F90.
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
void FUNCTION(emit_stop_f) ARGS(`IOHandle,
                                 Status')
INT_KIND_1_DECL(IOHandle);
INT_KIND_1_DECL(Status);
{
  int handle = (int)*IOHandle;
  *Status = INT_KIND_1_CAST( Emit_stop(&handle) );
  *IOHandle = INT_KIND_1_CAST(handle);

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
/** Wrapper for Emit_data_slice(), for use from within F90.
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
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
  *Status = INT_KIND_1_CAST( Emit_data_slice((int)*IOHandle, 
                                             f90_to_c_type[(int)*DataType],/*(int)*DataType,*/
                                             (int)*Count, 
                                             pData) );
  return;
}

/*----------------------------------------------------------------
SUBROUTINE emit_char_data_slice_f(IOHandle, pData, Status)

  INTEGER(KIND=REG_SP_KIND), INTENT(in)  :: IOHandle
  CHARACTER, DIMENSION(),    INTENT(in)  :: pData
  INTEGER(KIND=REG_SP_KIND), INTENT(out) :: Status
----------------------------------------------------------------*/
/** F90 only. Routine to emit a data slice consisting of CHARACTER data.
    @param IOHandle The handle of the IOType to use to emit data
    @param pData The CHARACTER string to emit
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE 
    @see emit_data_slice() */
void FUNCTION(emit_char_data_slice_f) ARGS(`IOHandle,
                                            STRING_ARG(pData),
                                            Status')
INT_KIND_1_DECL(IOHandle);
STRING_ARG_DECL(pData);
INT_KIND_1_DECL(Status);
{
  *Status = INT_KIND_1_CAST( Emit_data_slice(*IOHandle, REG_CHAR, 
                                             STRING_LEN(pData), 
                                             STRING_PTR(pData)) );
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
/** Wrapper for Make_vtk_buffer(), for use from within F90.
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
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
/** Wrapper for Make_vtk_header(), for use from within F90.
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
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
  int len = STRING_LEN(title);
  memset(STRING_PTR(header), '\0', STRING_LEN(header));
  /* Ensure that the string that we pass in to the library is 
  correctly null-terminated */
  strncpy(gStringBuf, STRING_PTR(title), len);
  gStringBuf[len] = '\0';

  *Status = INT_KIND_1_CAST( Make_vtk_header(STRING_PTR(header),
                                             gStringBuf,
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
/** Wrapper for Make_chunk_header(), for use from within F90.
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
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
  memset(STRING_PTR(header), '\0', STRING_LEN(header));
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
/** Wrapper for Steering_control(), for use from within F90.
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
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
  int num_commands, num_params;
  int i, len, pos;

#if REG_DEBUG
  fprintf(stderr, "STEER: steering_control_f: Calling Steering_control...\n");
#endif

  *Status = INT_KIND_1_CAST( Steering_control((int)*SeqNum,
			     		      &num_params,
			     		      str_array,
			     		      &num_commands,
			     		      gSteerCommands,
                                              str_array_params) );
#if REG_DEBUG
  fprintf(stderr, 
          "STEER: steering_control_f: got %d params and %d cmds\n", 
          num_params, num_commands);
#endif

  if(*Status == INT_KIND_1_CAST(REG_SUCCESS) ){

    *NumSteerParams = INT_KIND_1_CAST(num_params);

    /* ARPDBG Copy each returned string back into single array of char
       to return to caller. This may well fail on Crays. */

    for(i=0; i<num_params; i++){

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

    *NumSteerCommands =  INT_KIND_1_CAST(num_commands);

    for(i=0; i<num_commands; i++){

      SteerCommands[i] = INT_KIND_1_CAST(gSteerCommands[i]);

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
/** Wrapper for Steering_pause(), for use from within F90.
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE */
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
  int   lNumParams, lNumCommands;
  char *str_array[REG_MAX_NUM_STR_PARAMS];
  char *str_array_params[REG_MAX_NUM_STR_CMDS];

  j = 0;
  for(i=0; i<REG_MAX_NUM_STR_PARAMS; i++){

    str_array[i] = &(gStringBuf[j*REG_MAX_STRING_LENGTH]);
    j++;
  }
  for(i=0; i<REG_MAX_NUM_STR_CMDS; i++){

    str_array_params[i] = &(gStringBuf[j*REG_MAX_STRING_LENGTH]);
    j++;
  }

  *Status = INT_KIND_1_CAST( Steering_pause(&lNumParams,
                           		    str_array,
			   		    &lNumCommands,
			   		    gSteerCommands,
                                            str_array_params) );

  if(*Status == INT_KIND_1_CAST(REG_SUCCESS) ){

    *NumSteerParams = INT_KIND_1_CAST(lNumParams);

    /* Copy each returned string back into single array of char
       to return to caller. This may well fail on Crays. */

    for(i=0; i<lNumParams; i++){

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

    *NumCommands = INT_KIND_1_CAST(lNumCommands);

    for(i=0; i<lNumCommands; i++){

      SteerCommands[i] = INT_KIND_1_CAST(gSteerCommands[i]);

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
/** Wrapper for C call to sleep(2), for use from F90. 
    @internal */
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
/** F90 only. Routine to obtain a pointer to the supplied string.
    Originally intended to be passed to register_params_f as type void*. 
    Now obsolete since introduction of FUNCTION(register_string_param_f)()? */
void FUNCTION(steering_char_to_ptr_f) ARGS(`STRING_ARG(string),
                                            ptr')
STRING_ARG_DECL(string);
INT_KIND_1D0_DECL(ptr);
{
  /* Obtain a pointer to the supplied character string.  This is intended
     to be passed to register_params_f as type void*. */

#if REG_DEBUG
  fprintf(stderr, "STEER: steering_char_to_ptr_f: Entered routine, "
	  "string = %s\n", STRING_PTR(string));
#endif

  *ptr = INT_KIND_1D0_CAST(STRING_PTR(string));

#if REG_DEBUG
  fprintf(stderr, "STEER: steering_char_to_ptr_f: Leaving routine\n");
#endif
}

/*----------------------------------------------------------------
SUBROUTINE set_type_size(Type, Ptr1, Ptr2, Status)

  INTEGER (KIND=REG_SP_KIND), INTENT(in)  :: Type
  XXXXXXX, INTENT(in)                     :: Ptr1, Ptr2
  INTEGER (KIND=REG_SP_KIND), INTENT(out) :: Status
----------------------------------------------------------------*/
/** Routine to allow the storage size of different variable types
    to be determined at run time.
    @param Type The type of the variable being examined, as encoded 
           in ReG_Steer_types.h
    @param Ptr1 Pointer to first element in array of type @p Type
    @param Ptr2 Pointer to second element in array of type @p Type
    @param Status Return status of the call, REG_SUCCESS or
    REG_FAILURE 
    @internal */    
void FUNCTION(set_type_size) ARGS(`Type, Ptr1, Ptr2, Status')
INT_KIND_1_DECL(Type);
char *Ptr1;
char *Ptr2;
INT_KIND_1_DECL(Status);
{
  int size;
  int type;

  if(Ptr2 < Ptr1){
    fprintf(stderr, "STEER: set_type_size: ERROR: Ptr2 < Ptr1; arguments"
            " ordered incorrectly?\n");
    *Status = INT_KIND_1_CAST(REG_FAILURE);
    return;
  }

  size = (int)(Ptr2 - Ptr1);
  type = (int)*Type;

  switch(type){

    case REG_CHAR:
#if REG_DEBUG
      fprintf(stderr, "STEER: Sizeof REG_CHAR = %d, ", size);
#endif
      sizeof_type[REG_CHAR] = size;
      break;

    case REG_INT:
#if REG_DEBUG
      fprintf(stderr, "STEER: Sizeof REG_INT = %d\n, ", size);
#endif
      sizeof_type[REG_INT] = size;
      break;

    case REG_LONG:
#if REG_DEBUG
      fprintf(stderr, "STEER: Sizeof REG_LONG = %d\n, ", size);
#endif
      sizeof_type[REG_LONG] = size;
      break;

    case REG_FLOAT:
#if REG_DEBUG
      fprintf(stderr, "STEER: Sizeof REG_FLOAT = %d, ", size);
#endif
      sizeof_type[REG_FLOAT] = size;
      break;

    case REG_DBL:
#if REG_DEBUG
      fprintf(stderr, "STEER: Sizeof REG_DBL = %d, ", size);
#endif
      sizeof_type[REG_DBL] = size;
      break;

    default:
      fprintf(stderr, "STEER: set_type_size: ERROR: unrecognised type: %d\n",
              type);
      *Status = INT_KIND_1_CAST(REG_FAILURE);
      break;
  }

  if(size == sizeof(char)){
#if REG_DEBUG
    fprintf(stderr, "STEER: equivalent to a C char\n");
#endif
    f90_to_c_type[type] = REG_CHAR;
  }

  if((type == REG_INT) || (type == REG_LONG)){
    if(size == sizeof(int)){
#if REG_DEBUG
      fprintf(stderr, "STEER: equivalent to a C int\n");
#endif
      f90_to_c_type[type] = REG_INT;
    }
    else if(size == sizeof(long)){
#if REG_DEBUG
      fprintf(stderr, "STEER: equivalent to a C long\n");
#endif
      f90_to_c_type[type] = REG_LONG;
    }
  }

  if((type == REG_FLOAT) || (type == REG_DBL)){
    if(size == sizeof(float)){
#if REG_DEBUG
      fprintf(stderr, "STEER: equivalent to a C float\n");
#endif
      f90_to_c_type[(int)*Type] = REG_FLOAT;
    }
    else if(size == sizeof(double)){
#if REG_DEBUG
      fprintf(stderr, "STEER: equivalent to a C double\n");
#endif
      f90_to_c_type[(int)*Type] = REG_DBL;
    }
  }

  *Status = INT_KIND_1_CAST(REG_SUCCESS);
  return;
}
