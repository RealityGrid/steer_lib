// ReG_Steer.i : SWIG file for ReG
%module ReG_Steer
%{
#include "../../include/ReG_Steer_Appside.h"
%}

%javaconst(1);

%include "../../include/ReG_Steer_types.h"

/* Set up pointers */
%include "cpointer.i"
%pointer_class(int, Intp);
%pointer_class(float, Floatp);
%pointer_class(char, Charp);
%pointer_class(double, Doublep);

/* Set up typemaps */
%include "typemaps.i"

/* The following two typemaps add the SWIG baseclass
 * to the SWIGTYPE classes that are auto-generated */
%typemap(javabase) SWIGTYPE, SWIGTYPE *, SWIGTYPE &, SWIGTYPE [], 
                                                    SWIGTYPE (CLASS::*) "ReG_SWIG"

%typemap(javacode) SWIGTYPE, SWIGTYPE *, SWIGTYPE &, SWIGTYPE [], 
                                                    SWIGTYPE (CLASS::*) %{
  protected long getPointer() {
    return swigCPtr;
  }
%}

/* A set of typemaps to convert a java array of ints into a
 * C array of ints and a length variable */
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

/* A set of typemaps to update the contents of arrays of strings */
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

/* Typemaps for the data consumer methods */
/* int */
%typemap(java, in) (int count, int* data) {
  $1 = (*jenv)->GetArrayLength(jenv, $input);
  $2 = (int*) malloc($1 * sizeof(int));
}
%typemap(java, argout) (int count, int* data) {
  (*jenv)->SetIntArrayRegion(jenv, $input, 0, $1, $2);
}
%typemap(freearg) (int count, int* data) {
  if($2) free($2);
}
%typemap(jni) (int count, int* data) "jintArray"
%typemap(jtype) (int count, int* data) "int[]"
%typemap(jstype) (int count, int* data) "int[]"
%typemap(javain) (int count, int* data) "$javainput"

/* char */
%typemap(java, in) (int count, char* data) {
  $1 = (*jenv)->GetArrayLength(jenv, $input);
  $2 = (char*) malloc($1 * sizeof(char));
}
%typemap(java, argout) (int count, char* data) {
  (*jenv)->SetByteArrayRegion(jenv, $input, 0, $1, $2);
}
%typemap(freearg) (int count, char* data) {
  if($2) free($2);
}
%typemap(jni) (int count, char* data) "jbyteArray"
%typemap(jtype) (int count, char* data) "byte[]"
%typemap(jstype) (int count, char* data) "byte[]"
%typemap(javain) (int count, char* data) "$javainput"

/* float */
%typemap(java, in) (int count, float* data) {
  $1 = (*jenv)->GetArrayLength(jenv, $input);
  $2 = (float*) malloc($1 * sizeof(float));
}
%typemap(java, argout) (int count, float* data) {
  (*jenv)->SetFloatArrayRegion(jenv, $input, 0, $1, $2);
}
%typemap(freearg) (int count, float* data) {
  if($2) free($2);
}
%typemap(jni) (int count, float* data) "jfloatArray"
%typemap(jtype) (int count, float* data) "float[]"
%typemap(jstype) (int count, float* data) "float[]"
%typemap(javain) (int count, float* data) "$javainput"

/* double */
%typemap(java, in) (int count, double* data) {
  $1 = (*jenv)->GetArrayLength(jenv, $input);
  $2 = (double*) malloc($1 * sizeof(double));
}
%typemap(java, argout) (int count, double* data) {
  (*jenv)->SetDoubleArrayRegion(jenv, $input, 0, $1, $2);
}
%typemap(freearg) (int count, double* data) {
  if($2) free($2);
}
%typemap(jni) (int count, double* data) "jdoubleArray"
%typemap(jtype) (int count, double* data) "double[]"
%typemap(jstype) (int count, double* data) "double[]"
%typemap(javain) (int count, double* data) "$javainput"




/* Strings and arrays stuff */
//%include "various.i"
%include "arrays_java.i"

/* Apply typemaps to the required variables */
%apply (int length, int *array) { (int NumSupportedCmds, int *SupportedCmds) }

%apply char **update {
  char** SteerParamLabels,
  char** SteerCmdParams
}

%apply int[] {int *SteerCommands};

%apply (int count, int* data) { (int Count, int* data) }
%apply (int count, char* data) { (int Count, char* data) }
%apply (int count, float* data) { (int Count, float* data) }
%apply (int count, double* data) { (int Count, double* data) }

/* %apply int *OUTPUT { */
/*   int *ChkType, */
/*   int *IOTypeIndex, */
/*   int *IOHandle, */
/*   int *DataType, */
/*   int *DataCount */
/* } */

/* %apply int *INOUT { */
/*   int *IOTypeIndexINOUT, */
/*   int *IOHandleINOUT */
/* } */

%include "../ReG_Steer_API.i"

/* These prototypes "overload" the data consumer methods */
%inline %{
  int Consume_data_slice_int(int IOTypeIndex, int Count, int* data) {
    return Consume_data_slice(IOTypeIndex, REG_INT, Count, data);
  }
%}
int Consume_data_slice_int(int IOTypeIndex, int Count, int* data);

%inline %{
  int Consume_data_slice_char(int IOTypeIndex, int Count, char* data) {
    return Consume_data_slice(IOTypeIndex, REG_CHAR, Count, data);
  }
%}
int Consume_data_slice_char(int IOTypeIndex, int Count, char* data);

%inline %{
  int Consume_data_slice_float(int IOTypeIndex, int Count, float* data) {
    return Consume_data_slice(IOTypeIndex, REG_FLOAT, Count, data);
  }
%}
int Consume_data_slice_float(int IOTypeIndex, int Count, float* data);

%inline %{
  int Consume_data_slice_double(int IOTypeIndex, int Count, double* data) {
    return Consume_data_slice(IOTypeIndex, REG_DBL, Count, data);
  }
%}
int Consume_data_slice_double(int IOTypeIndex, int Count, double* data);
