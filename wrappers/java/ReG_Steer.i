/* ----------------------------------------------------------------------------
  This file is part of the RealityGrid Steering Library Java Wrappers.
 
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

%module ReG_Steer
%{
#include "../../include/ReG_Steer_Appside.h"
#include "../../include/ReG_Steer_Steerside.h"
%}

/* Ensure that all #defined constants are wrapped in a sensible
 * mannner and then pull in the ReG_Steer_types header.  */
%javaconst(1);
%include "../../include/ReG_Steer_types.h"

/* Ensure that the initial module wrapping has package level
 * access so that the general public use the ReG_SteerAppside
 * wrapper class for access to the steering library instead. */
%pragma(java) moduleclassmodifiers="class"

/* Let the JNI bit of the wrapper code load in the native library
 * seeing as we'll need it every time anyway. */
%pragma(java) jniclasscode=%{
  static {
    try {
      System.loadLibrary("ReG_SteerJava");
    } catch(UnsatisfiedLinkError e) {
      System.err.println("Failed to load native code library:\n" + e);
      System.exit(1);
    }
  }
%}

/* Set up pointers */
%include "cpointer.i"
%pointer_class(int, Intp);
%pointer_class(char, Charp);
%pointer_class(float, Floatp);
%pointer_class(double, Doublep);
//%pointer_class(byte, Bytep);

/*
 * The following two typemaps add the ReG_SWIG baseclass and
 * getPointer to the SWIGTYPE classes that are auto-generated
 */
%typemap(javabase) SWIGTYPE, SWIGTYPE *, SWIGTYPE &, SWIGTYPE [], 
                                                    SWIGTYPE (CLASS::*) "ReG_SWIG"

%typemap(javacode) SWIGTYPE, SWIGTYPE *, SWIGTYPE &, SWIGTYPE [], 
                                                    SWIGTYPE (CLASS::*) %{
  protected long getPointer() {
    return swigCPtr;
  }
%}

/* 
 * A set of typemaps to convert a java array of ints into a
 * C array of ints and a length variable
 */
%typemap(java, in) (int length, int *array) {
  $1 = (int) (*jenv)->GetArrayLength(jenv, $input);
  $2 = (int*) (*jenv)->GetIntArrayElements(jenv, $input, 0);
}
%typemap(freearg) (int length, int *array) {
  if($2) (*jenv)->ReleaseIntArrayElements(jenv, $input, $2, JNI_ABORT);
}
%typemap(jni) (int length, int *array) "jintArray"
%typemap(jtype) (int length, int *array) "int[]"
%typemap(jstype) (int length, int *array) "int[]"
%typemap(javain) (int length, int *array) "$javainput"

/*
 * A set of typemaps to update the contents of arrays of strings
 */
%typemap(java, in) char **update (jint len) {
  int i;
  if(!$input) {
    SWIG_JavaThrowException(jenv, SWIG_JavaNullPointerException, "array null");
    return $null;
  }
  
  len = (*jenv)->GetArrayLength(jenv, $input);
  $1 = (char**) malloc(len * sizeof(char*));
  for(i = 0; i < len; i++) {
    $1[i] = (char*) malloc(REG_MAX_STRING_LENGTH * sizeof(char));
  }
}
%typemap(java, argout) char **update {
  int i;
  jstring temp_string;
  for(i = 0; i < len$argnum; i++) {
    temp_string = (*jenv)->NewStringUTF(jenv, $1[i]);
    (*jenv)->SetObjectArrayElement(jenv, $input, i, temp_string);
    (*jenv)->DeleteLocalRef(jenv, temp_string);
  }
}
%typemap(freearg) char **update {
  int i;
  for(i = 0; i < len$argnum; i++) 
    if($1[i]) free($1[i]);
  if($1) free($1);
}
%typemap(jni) char **update "jobjectArray"
%typemap(jtype) char **update "String[]"
%typemap(jstype) char **update "String[]"
%typemap(javain) char **update "$javainput"

/*
 * A set of typemaps for the data emitter method
 */
%typemap(java, in, numinputs=0) (int type, int count) {
  /* Just throw these away as we can work them out! */
}
%typemap(java, check) (int type, int count, void* indata) {
  if((*jenv)->IsInstanceOf(jenv, jarg4, (*jenv)->FindClass(jenv, "[I")) == JNI_TRUE) {
    $1 = REG_INT;
    $2 = (*jenv)->GetArrayLength(jenv, jarg4);
    $3 = (int*) malloc($2 * sizeof(int));
    (*jenv)->GetIntArrayRegion(jenv, jarg4, 0, $2, $3);
  }
  else if((*jenv)->IsInstanceOf(jenv, jarg4, (*jenv)->FindClass(jenv, "[F")) == JNI_TRUE) {
    $1 = REG_FLOAT;
    $2 = (*jenv)->GetArrayLength(jenv, jarg4);
    $3 = (float*) malloc($2 * sizeof(float));
    (*jenv)->GetFloatArrayRegion(jenv, jarg4, 0, $2, $3);
  }
  else if((*jenv)->IsInstanceOf(jenv, jarg4, (*jenv)->FindClass(jenv, "[D")) == JNI_TRUE) {
    $1 = REG_DBL;
    $2 = (*jenv)->GetArrayLength(jenv, jarg4);
    $3 = (double*) malloc($2 * sizeof(double));
    (*jenv)->GetDoubleArrayRegion(jenv, jarg4, 0, $2, $3);
  }
  else if((*jenv)->IsInstanceOf(jenv, jarg4, (*jenv)->FindClass(jenv, "[B")) == JNI_TRUE) {
    $1 = REG_CHAR;
    $2 = (*jenv)->GetArrayLength(jenv, jarg4);
    $3 = (char*) malloc($2 * sizeof(char));
    (*jenv)->GetByteArrayRegion(jenv, jarg4, 0, $2, $3);
  }
  else if((*jenv)->IsInstanceOf(jenv, jarg4, (*jenv)->FindClass(jenv, "java/lang/String")) == JNI_TRUE) {
    jboolean isCopy;

    $1 = REG_CHAR;
    $2 = (*jenv)->GetStringUTFLength(jenv, jarg4) + 1;
    $3 = (char*) malloc($2 * sizeof(char));
    const char* cArray = (*jenv)->GetStringUTFChars(jenv, jarg4, &isCopy);
    strncpy($3, cArray, $2);
    if(isCopy == JNI_TRUE)
      (*jenv)->ReleaseStringUTFChars(jenv, jarg4, cArray);
  }
}
%typemap(freearg) void* indata {
  if($1) free($1);
}
%typemap(jni) void* indata "jobject"
%typemap(jtype) void* indata "Object"
%typemap(jstype) void* indata "Object"
%typemap(javain) void* indata "$javainput"

/*
 * A set of typemaps for the data consumer method
 */
%typemap(java, in, numinputs=0) void* outdata {
  /* Just throw outdata away from the inputs! */
}
%typemap(java, check) (int type, int count, void *outdata) {
  switch($1) {
  case REG_INT:
    $3 = (int*) malloc($2 * sizeof(int));
    break;
  case REG_CHAR:
    $3 = (char*) malloc($2 * sizeof(char));
    break;
  case REG_FLOAT:
    $3 = (float*) malloc($2 * sizeof(float));
    break;
  case REG_DBL:
    $3 = (double*) malloc($2 * sizeof(double));
    break;
  }
}
%typemap(java, argout) (int type, int count, void* outdata) {
  jintArray iArray;
  jbyteArray bArray;
  jstring cString;
  jfloatArray fArray;
  jdoubleArray dArray;

  switch($1) {
  case REG_INT:
    iArray = (*jenv)->NewIntArray(jenv, $2);
    (*jenv)->SetIntArrayRegion(jenv, iArray, 0, $2, $3);
    $result = iArray;
    break;
  case REG_CHAR:
    if(((char*) $3)[$2 - 1] == '\0') {
      cString = (*jenv)->NewStringUTF(jenv, $3);
      $result = cString;
    }
    else {
      bArray = (*jenv)->NewByteArray(jenv, $2);
      (*jenv)->SetByteArrayRegion(jenv, bArray, 0, $2, $3);
      $result = bArray;
    }
    break;
  case REG_FLOAT:
    fArray = (*jenv)->NewFloatArray(jenv, $2);
    (*jenv)->SetFloatArrayRegion(jenv, fArray, 0, $2, $3);
    $result = fArray;
    break;
  case REG_DBL:
    dArray = (*jenv)->NewDoubleArray(jenv, $2);
    (*jenv)->SetDoubleArrayRegion(jenv, dArray, 0, $2, $3);
    $result = dArray;
    break;
  default:
    $result = $null;
    break;
  }
  
}
%typemap(freearg) void* outdata {
  if($1) free($1);
}
%typemap(jni) void* outdata "jobject"
%typemap(jtype) void* outdata "Object"
%typemap(jstype) void* outdata "Object"
%typemap(javain) void* outdata "$javainput"
%typemap(javaout) void* outdata {
  return $jnicall;
}

/*
 * A set of typemaps for the get_param_details method
 */
%typemap(java, in, numinputs=0) Param_details_struct* outdetails {
  /* Just throw outdata away from the inputs! */
}
%typemap(java, check) (int count, Param_details_struct* outdetails) {
  $2 = (Param_details_struct*) malloc($1 * sizeof(Param_details_struct));
}
%typemap(java, argout) (int steer, int count, Param_details_struct* outdetails) {

  if($2 > 0) {
    int i;
    jclass regParam;
    jobjectArray paramArray;
    jmethodID regParamInit;
    jvalue args[7];
    jobject newObject;

    /* Set up access to the java classes and methods */
    regParam = (*jenv)->FindClass(jenv, "org/realitygrid/steering/ReG_SteerParameter");
    paramArray = (*jenv)->NewObjectArray(jenv, $2, regParam, NULL);
    regParamInit = (*jenv)->GetMethodID(jenv, regParam, "<init>", "(Ljava/lang/String;Ljava/lang/String;ZILjava/lang/String;Ljava/lang/String;I)V");

    for(i = 0; i < $2; i++) {
      args[0].l = (*jenv)->NewStringUTF(jenv, $3[i].label);
      args[1].l = (*jenv)->NewStringUTF(jenv, $3[i].value);
      args[2].z = ($1 == REG_TRUE) ? JNI_TRUE : JNI_FALSE;
      args[3].i = $3[i].type;
      args[4].l = (*jenv)->NewStringUTF(jenv, $3[i].min_val);
      args[5].l = (*jenv)->NewStringUTF(jenv, $3[i].max_val);
      args[6].i = $3[i].handle;

      newObject = (*jenv)->NewObjectA(jenv, regParam, regParamInit, args);
      (*jenv)->SetObjectArrayElement(jenv, paramArray, i, newObject);
    }

    $result = paramArray;
  }
  else {
    $result = NULL;
  }
}
%typemap(freearg) Param_details_struct* outdetails {
  if($1) free($1);
}
%typemap(jni) Param_details_struct* outdetails "jobjectArray"
%typemap(jtype) Param_details_struct* outdetails "Object[]"
%typemap(jstype) Param_details_struct* outdetails "Object[]"
%typemap(javain) Param_details_struct* outdetails "$javainput"
%typemap(javaout) Param_details_struct* outdetails {
  return $jnicall;
}

/*
 * A set of typemaps to accept arrays of Strings as arrays of char*
 */
%typemap(java, in) char **inStrings(jint len) {
  int i;
  if(!$input) {
    $1 = NULL;
    len = 0;
  }
  else {
    len = (*jenv)->GetArrayLength(jenv, $input);
    $1 = (char**) malloc((len + 1) * sizeof(char*));
    /* make a copy of each string */
    for(i = 0; i < len; i++) {
      jstring jString = (jstring)(*jenv)->GetObjectArrayElement(jenv, $input, i);
      const char* cString = (*jenv)->GetStringUTFChars(jenv, jString, 0);
      $1[i] = malloc(strlen((cString) + 1) * sizeof(const char*));
      strcpy($1[i], cString);
      (*jenv)->ReleaseStringUTFChars(jenv, jString, cString);
      (*jenv)->DeleteLocalRef(jenv, jString);
    }
    $1[i] = 0;
  }
}
%typemap(freearg) char **inStrings {
  int i;
  for(i = 0; i < len$argnum-1; i++)
    if($1[i]) free($1[i]);
  if($1) free($1);
}
%typemap(jni) char **inStrings "jobjectArray"
%typemap(jtype) char **inStrings "String[]"
%typemap(jstype) char **inStrings "String[]"
%typemap(javain) char **inStrings "$javainput"

/*
 * A set of typemaps to accept arrays of ints as int* (and handle NULL!)
 */
%typemap(java, in) int *inInts(jint len) {
  if(!$input) {
    $1 = NULL;
    len = 0;
  }
  else {
    len = (*jenv)->GetArrayLength(jenv, $input);
    $1 = (int*) malloc(len * sizeof(int));
    (*jenv)->GetIntArrayRegion(jenv, $input, 0, (len - 1), $1);
  }
}
%typemap(freearg) int *inInts {
  if($1) free($1);
}
%typemap(jni) int *inInts "jobjectArray"
%typemap(jtype) int *inInts "int[]"
%typemap(jstype) int *inInts "int[]"
%typemap(javain) int *inInts "$javainput"


/* Strings and arrays stuff */
%include "arrays_java.i"

/* Apply typemaps to the required variables */
%apply (int length, int *array) { (int NumSupportedCmds, int *SupportedCmds) }

%apply char **update {
  char** SteerParamLabels,
  char** SteerCmdParams,
  char** simName,
  char** simGSH,
  char** chk_labels,
  char** io_labels
}

%apply int[] {
  int *SteerCommands,
  int *cmd_ids,
  int *handles,
  int *chk_handles,
  int *chk_types,
  int *chk_freqs,
  int *io_handles,
  int *io_types,
  int *io_freqs,
  int *Commands
};

%apply (int type, int count, void* outdata) {
  (int DataType, int Count, void* pDataOUT)
}
%apply void* outdata { void* pDataOUT }

%apply (int type, int count, void* indata) {
  (int DataTypeIN, int CountIN, void* pDataIN)
}
%apply (int type, int count) { (int DataTypeIN, int CountIN) }
%apply void* indata { void* pDataIN }

%apply (int steer, int count, Param_details_struct* outdetails) {
  (int steerable, int num_params, Param_details_struct* param_details)
}
%apply (int count, Param_details_struct* outdetails) {
  (int num_params, Param_details_struct* param_details)
}
%apply Param_details_struct* outdetails { Param_details_struct* param_details }

%apply char **inStrings {
  char **SysCmdParams,
  char* *vals
}
%apply int *inInts { int *SysCommands }

/* Pull in the common API definition file */
%include "../ReG_Steer_API.i"

/* Re-define the Consume_data_slice method to return an object
 * this is done by creating a new version of the method and
 * calling that instead.: Consume_data_slice_j */
%inline %{
  jobject Consume_data_slice_j(int IOTypeIndex, int DataType, int Count, void* pDataOUT) {
    Consume_data_slice(IOTypeIndex, DataType, Count, pDataOUT);
    return 0;
  }
%}
jobject Consume_data_slice_j(int IOTypeIndex, int DataType, int Count, void* pDataOUT);

/* Re-define the Get_param_values method to return an objectArray
 * this is done by creating a new version of the method and
 * calling that instead.: Get_param_values_j */
%inline %{
  jobjectArray Get_param_values_j(int sim_handle, int steerable, int num_params, Param_details_struct *param_details) {
    Get_param_values(sim_handle, steerable, num_params, param_details);
    return 0;
  }
%}
jobjectArray Get_param_values_j(int sim_handle, int steerable, int num_params, Param_details_struct *param_details);
