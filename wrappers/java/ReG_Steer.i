// ReG_Steer.i : SWIG file for ReG
%module ReG_Steer
%{
#include "../../include/ReG_Steer_Appside.h"
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

/* Typemaps for the data consumer method */
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
  int i;
  jintArray iArray;
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
    cString = (*jenv)->NewStringUTF(jenv, $3);
    $result = cString;
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

/* Strings and arrays stuff */
%include "arrays_java.i"

/* Apply typemaps to the required variables */
%apply (int length, int *array) { (int NumSupportedCmds, int *SupportedCmds) }

%apply char **update {
  char** SteerParamLabels,
  char** SteerCmdParams
}

%apply int[] {int *SteerCommands};

%apply (int type, int count, void* outdata) { (int DataType, int Count, void* pDataOUT) }
%apply void* outdata { void* pDataOUT }

/* Pull in the common API definition file */
%include "../ReG_Steer_API.i"

/* Re-define the Consume_data_slice method to return an object
 * this is done by creating a new version of the method an
 * calling that instead.: Consume_data_slice_j */
%inline %{
  jobject Consume_data_slice_j(int IOTypeIndex, int DataType, int Count, void* pDataOUT) {
    Consume_data_slice(IOTypeIndex, DataType, Count, pDataOUT);
    return 0;
  }
%}
jobject Consume_data_slice_j(int IOTypeIndex, int DataType, int Count, void* pDataOUT);

