/*----------------------------------------------------------------------------
    This file contains wrapper routines allowing the routines for the 
    construction of an interface to a steering component (from an 
    application component) to be called from F90.  The m4 macros used
    to create the platform-specific C functions are taken from the
    PVM distribution.  See the readme file in the 'conf' directory.

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

    Initial version by:  A Porter, 2.8.2002
---------------------------------------------------------------------------*/

#include "ReG_Steer_Appside.h"
#include <unistd.h>

#ifndef DEBUG
#define DEBUG 1
#endif

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

SUBROUTINE steering_initialize_f(NumSupportedCmds, SupportedCmds, Status)

  INTEGER (KIND=REG_SP_KIND), INTENT(in)  :: NumSupportedCmds
  INTEGER (KIND=REG_SP_KIND), INTENT(in)  :: SupportedCmds
  INTEGER (KIND=REG_SP_KIND), INTENT(out) :: Status
----------------------------------------------------------------*/

void FUNCTION(steering_initialize_f) ARGS(`NumSupportedCmds,
			SupportedCmds, Status')
INT_KIND_1_DECL(NumSupportedCmds);
INT_KIND_1_DECL(SupportedCmds);
INT_KIND_1_DECL(Status);
{
  *Status = INT_KIND_1_CAST( Steering_initialize((int)(*NumSupportedCmds), 
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
  char **str_array;

  str_array = (char**)malloc((*NumTypes)*sizeof(char));

  if(*str_array == NULL){

    *Status = INT_KIND_1_CAST( REG_FAILURE );
    return;
  }

  /* Convert from a single array of char to an array of char* */

  for(i=0; i<(int)(*NumTypes); i++){

    str_array[i] = &(STRING_PTR(IOLabel)[i*STRING_LEN(IOLabel)]);
  }

  *Status = INT_KIND_1_CAST( Register_IOTypes((int)*NumTypes,
			                             str_array,
                                              (int *)IODirn,
                                              (int *)IOFrequency,
			                      (int *)IOType) );

  free(str_array);
  return;
}

/*----------------------------------------------------------------

SUBROUTINE register_params_f(NumParams, ParamLabels, ParamSteerable, &
                             ParamPtrs, ParamTypes, Status)

  INTEGER (KIND=REG_SP_KIND), INTENT(in)                     :: NumParams
  CHARACTER (LEN=REG_MAX_STRING_LENGTH) DIMENSION(NumParams), &
                                              INTENT(in)     :: IOLabel
  INTEGER (KIND=REG_SP_KIND), DIMENSION(NumTypes),INTENT(in) :: ParamSteerable
  XXXXXXX (KIND=REG_DP_KIND), DIMENSION(NumTypes),INTENT(in) :: ParamPtrs
  INTEGER (KIND=REG_SP_KIND), DIMENSION(NumTypes),INTENT(in) :: ParamTypes
  INTEGER (KIND=REG_SP_KIND), INTENT(out)                    :: Status

where XXXXXXX can be INTEGER or REAL of a KIND to give storage no
larger than required by KIND=REG_DP_KIND.  If it is desired to 
register a CHARACTER string then the routine steering_char_to_ptr_f
(see later) must be used to get an INTEGER to be passed as
ParamPtr to this routine.
----------------------------------------------------------------*/

void FUNCTION(register_params_f) ARGS(`NumParams,
		                       STRING_ARG(ParamLabels),
		                       ParamSteerable,
		                       ParamPtrs,
		                       ParamTypes,
	                               Status')
INT_KIND_1_DECL(NumParams);
STRING_ARG_DECL(ParamLabels);
INT_KIND_1_DECL(ParamSteerable);
void   *ParamPtrs;
INT_KIND_1_DECL(ParamTypes);
INT_KIND_1_DECL(Status);
{
  int    i;
  char **str_array;
  void **ptr_array;

  *Status = INT_KIND_1_CAST( REG_SUCCESS );

  str_array = (char**)malloc((*NumParams)*sizeof(char*));
  ptr_array = (void**)malloc((*NumParams)*sizeof(void*));

  if(str_array == NULL || ptr_array == NULL){

    *Status = INT_KIND_1_CAST( REG_FAILURE );
    return;
  }

  /* Convert from a single array of char to an array of char* */

  for(i=0; i<(*NumParams); i++){

    str_array[i] = &(STRING_PTR(ParamLabels)[i*STRING_LEN(ParamLabels)]);
  }

  /* Convert pointers */

  for(i=0; i<(*NumParams); i++){

    switch(ParamTypes[i]){

    case REG_INT:

      ptr_array[i] = (void *)( &((int *)ParamPtrs)[i] );
      break;

    case REG_FLOAT:

      ptr_array[i] = (void *)( &((float *)ParamPtrs)[i] );
      break;

    case REG_CHAR:

      /* IMPORTANT - this assumes that ParamPtrs has been obtained from a
         call to steering_char_to_ptr_f */
      ptr_array[i] = ((void **)ParamPtrs)[i];

#if DEBUG
      fprintf(stderr, 
	      "register_params_f: got string %s\n", (char *)(ptr_array[i]));
#endif
      break;

    case REG_DBL:

      ptr_array[i] = (void *)( &((double *)ParamPtrs)[i] );
      break;

    default:
      fprintf(stderr, "register_params_f: unrecognised data type\n");
      *Status = REG_FAILURE;
      break;
    }
  }

  if(*Status != INT_KIND_1_CAST(REG_FAILURE)){

    *Status = INT_KIND_1_CAST( Register_params((int)*NumParams,
 	                      		       str_array,
			      		       (int *)ParamSteerable,
			      		       ptr_array,
			      		       (int *)ParamTypes) );
  }
  if(str_array != NULL)free(str_array);
  if(ptr_array != NULL)free(ptr_array);

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

SUBROUTINE steering_control_f(SeqNum, NumSteerParams, SteerParamLabels &
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
  int   i, j, len, pos;
  char  buf[(REG_MAX_NUM_STR_PARAMS+REG_MAX_NUM_STR_CMDS)
            *REG_MAX_STRING_LENGTH];
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

#if DEBUG
  fprintf(stderr, "steering_control_f: Calling Steering_control...\n");
#endif

  *Status = INT_KIND_1_CAST( Steering_control((int)*SeqNum,
			     		      (int *)NumSteerParams,
			     		      str_array,
			     		      (int *)NumSteerCommands,
			     		      (int *)SteerCommands,
                                              str_array_params) );
#if DEBUG
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
      len = strlen(str_array[i]);
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
      len = strlen(str_array_params[i]);
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
      len = strlen(str_array[i]);
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
      len = strlen(str_array_params[i]);
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

#if DEBUG
  fprintf(stderr, "steering_char_to_ptr_f: Entered routine, "
	  "string = %s\n", STRING_PTR(string));
#endif

  *ptr = INT_KIND_1D0_CAST(STRING_PTR(string));

#if DEBUG
  fprintf(stderr, "steering_char_to_ptr_f: Leaving routine\n");
#endif

}


