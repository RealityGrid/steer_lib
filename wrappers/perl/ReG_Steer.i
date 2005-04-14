/* ----------------------------------------------------------------------------
  This file is part of the RealityGrid Steering Library Perl Wrappers.
 
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
 
  Author........: Robert Haines
---------------------------------------------------------------------------- */

%module ReG_Steer
%{
#include "../../include/ReG_Steer_Appside.h"
%}

%include "../../include/ReG_Steer_types.h"

/* Set up pointers */
%include "cpointer.i"

%pointer_class(int, intp);
%pointer_class(float, floatp);
%pointer_class(double, doublep);

/* Set up typemaps */
%include "typemaps.i"

/* A set of typemaps to convert a perl list of ints and a length
 * variable to an array of ints */
%typemap(perl5, in) (int length, int *array) {
  int i;
  I32 len;
  AV* tempav;
  SV** tempiv;
  if (!SvROK($input))
    croak("Argument $argnum is not a reference");
  if (SvTYPE(SvRV($input)) != SVt_PVAV)
    croak("Argument $argnum is not an array");
  tempav = (AV*) SvRV($input);
  len = av_len(tempav);
  if(len == -1)
    croak("Array is empty");
  $1 = len + 1;
  $2 = (int*) malloc($1 * sizeof(int));
  for(i = 0; i < $1; i++) {
    tempiv = av_fetch(tempav, i, 0);
    if(SvIOK(*tempiv))
      $2[i] = (int) SvIV(*tempiv);
    else
      croak("Element %d in argument $argnum array is not an int", i); 
  }
}
%typemap(freearg) (int length, int *array) {
  if($2) free($2);
}

/* A set of typemaps to map a returned string array and length
 * variable into two python lists of strings and ints */
%typemap(perl5, in, numinputs=0) (int *length, char **outstrs) {
  int i;
  $1 = (int*) malloc(sizeof(int));
  $2 = (char**) malloc(REG_MAX_NUM_STR_PARAMS * sizeof(char*));
  for(i = 0; i < REG_MAX_NUM_STR_PARAMS; i++)
    $2[i] = (char*) malloc(REG_MAX_STRING_LENGTH * sizeof(char));
}
%typemap(perl5, argout) (int *length, char **outstrs) {
  int i;
  AV* outArray;
  SV** svList;
  svList = (SV**) malloc(*$1 * sizeof(SV*));
  for(i = 0; i < *$1; i++) {
    svList[i] = sv_newmortal();
    sv_setpv((SV*) svList[i], (const char*) $2[i]);
  }
  outArray = av_make(*$1, svList);
  free(svList);
  if(argvi >= items) {            
    EXTEND(sp,1);
  }
  $result = newRV((SV*) outArray);
  sv_2mortal($result);
  argvi++;
}
%typemap(freearg) (int *length, char **outstrs) {
  int i;
  if($1) free($1);
  for(i = 0; i < REG_MAX_NUM_STR_PARAMS; i++)
    if($2[i]) free($2[i]);
  if($2) free($2);
}

/* A set of typemaps to map a returned int array, string array and
 * length variable into two python lists of strings and ints */
%typemap(perl5, in, numinputs=0) (int *length, int *outints, char **outstrs) {
  int i;
  $1 = (int*) malloc(sizeof(int));
  $2 = (int*) malloc(REG_MAX_NUM_STR_CMDS * sizeof(int));
  $3 = (char**) malloc(REG_MAX_NUM_STR_CMDS * sizeof(char*));
  for(i = 0; i < REG_MAX_NUM_STR_CMDS; i++)
    $3[i] = (char*) malloc(REG_MAX_STRING_LENGTH * sizeof(char));
}
%typemap(perl5, argout) (int *length, int *outints, char **outstrs) {
  int i;
  AV* outArrayInts;
  AV* outArrayStrs;
  SV** svListInts;
  SV** svListStrs;
  svListInts = (SV**) malloc(*$1 * sizeof(SV*));
  svListStrs = (SV**) malloc(*$1 * sizeof(SV*));
  for(i = 0; i < *$1; i++) {
    svListInts[i] = sv_newmortal();
    svListStrs[i] = sv_newmortal();
    sv_setiv((SV*) svListInts[i], (int) $2[i]);
    sv_setpv((SV*) svListStrs[i], (const char*) $3[i]);
  }
  outArrayInts = av_make(*$1, svListInts);
  outArrayStrs = av_make(*$1, svListStrs);
  free(svListInts);
  free(svListStrs);
  if(argvi >= items) {            
    EXTEND(sp,1);
  }
  $result = newRV((SV*) outArrayInts);
  sv_2mortal($result);
  argvi++;
  if(argvi >= items) {            
    EXTEND(sp,1);
  }
  $result = newRV((SV*) outArrayStrs);
  sv_2mortal($result);
  argvi++;
}
%typemap(freearg) (int *length, int *outints, char **outstrs) {
  int i;
  if($1) free($1);
  if($2) free($2);
  for(i = 0; i < REG_MAX_NUM_STR_CMDS; i++)
    if($3[i]) free($3[i]);
  if($3) free($3);
}

/* A set of typemaps to map a returned block of data into a
 * python list of the correct type */
%typemap(perl5, in, numinputs=0) void *outdata {
  /* Just throw outdata away from the inputs! */
}
%typemap(perl5, check) (int type, int count, void *outdata) {
  if($1 < 0 || $1 > 3) {
    printf("Type out of valid range!\n");
  }
  if($2 <= 0) {
    printf("Data count is zero!\n");
  }
  if(!$3) {
    switch($1) {
    case REG_INT:
      $3 = (void*) malloc($2 * sizeof(int));
      break;
    case REG_CHAR:
      $3 = (void*) malloc($2 * sizeof(char));
      break;
    case REG_FLOAT:
      $3 = (void*) malloc($2 * sizeof(float));
      break;
    case REG_DBL:
      $3 = (void*) malloc($2 * sizeof(double));
      break;
    }
  }
}
%typemap(perl5, argout) (int type, int count, void *outdata) {
  int i;
  AV* outArray;
  SV* sv;
  SV** svList;

  switch($1) {
  case REG_INT:
    svList = (SV**) malloc($2 * sizeof(SV*));
    for(i = 0; i < $2; i++) {
      svList[i] = sv_newmortal();
      sv_setiv((SV*) svList[i], ((int*) $3)[i]);
    }
    outArray = av_make($2, svList);
    free(svList);
    $result = newRV((SV*) outArray);
    break;
  case REG_FLOAT:
    svList = (SV**) malloc($2 * sizeof(SV*));
    for(i = 0; i < $2; i++) {
      svList[i] = sv_newmortal();
      sv_setnv((SV*) svList[i], (double) ((float*) $3)[i]);
    }
    outArray = av_make($2, svList);
    free(svList);
    $result = newRV((SV*) outArray);    
    break;
  case REG_DBL:
    svList = (SV**) malloc($2 * sizeof(SV*));
    for(i = 0; i < $2; i++) {
      svList[i] = sv_newmortal();
      sv_setnv((SV*) svList[i], ((double*) $3)[i]);
    }
    outArray = av_make($2, svList);
    free(svList);
    $result = newRV((SV*) outArray);    
    break;
  case REG_CHAR:
    sv = sv_newmortal();
    sv_setpvn(sv, (const char*) $3, $2);
    $result = newRV(sv);
    break;
  }
  sv_2mortal($result);
  argvi++;
}
%typemap(freearg) void *outdata {
  if($1) free($1);
}

/* Apply typemaps to the required variables */
%apply (int length, int *array) { (int NumSupportedCmds, int *SupportedCmds) }

%apply (int *length, char **outstrs) {
  (int *NumSteerParams, char **SteerParamLabels)
}

%apply (int *length, int *outints, char **outstrs) {
  (int *NumSteerCommands, int *SteerCommands, char **SteerCmdParams)
}

%apply void *outdata { void *pDataOUT }
%apply (int type, int count, void *outdata) {
  (int DataType, int Count, void *pDataOUT)
}

%apply int *OUTPUT {
  int *iotypehandle,
  int *ChkType,
  int *IOTypeIndex,
  int *IOHandle,
  int *DataType,
  int *DataCount
}

%apply int *INOUT {
  int *IOTypeIndexINOUT,
  int *IOHandleINOUT
}

%include "../ReG_Steer_API.i"
