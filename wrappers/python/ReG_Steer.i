// ReG_Steer.i : SWIG file for ReG
%module ReG_Steer
%{
#include "../../include/ReG_Steer_Appside.h"
%}

%include "../../include/ReG_Steer_types.h"

/* Set up pointers */
%include "cpointer.i"

%pointer_class(int, intp);

/* Set up typemaps */
%include "typemaps.i"

/* A set of typemaps to convert a python list of ints and a length
 * variable to an array of ints */
%typemap(python, in) (int length, int *array) {
  int i;
  if(!PyList_Check($input)) {
    PyErr_SetString(PyExc_ValueError, "Expected a list");
    return NULL;
  }
  $1 = PyList_Size($input);
  int* temp = (int*) malloc($1 * sizeof(int));
  for(i = 0; i < $1; i++) {
    PyObject *o = PyList_GetItem($input, i);
    if(PyInt_Check(o)) {
      temp[i] = (int) PyInt_AsLong(o);
    }
    else {
      PyErr_SetString(PyExc_ValueError, "List elements must be integers");
      return NULL;
    }
  }
  $2 = temp;
}
%typemap(freearg) (int length, int *array) {
  if($2) free($2);
}

/* A set of typemaps to map a returned string array and length
 * variable into two python lists of strings and ints */
%typemap(python, in, numinputs=0) (int *length, char **outstrs) {
  int i;
  $1 = (int*) malloc(sizeof(int));
  $2 = (char**) malloc(REG_MAX_NUM_STR_PARAMS * sizeof(char*));
  for(i = 0; i < REG_MAX_NUM_STR_PARAMS; i++)
    $2[i] = (char*) malloc(REG_MAX_STRING_LENGTH * sizeof(char));
}
%typemap(python, argout) (int *length, char **outstrs) {
  int i;
  PyObject* outliststrs = PyList_New(*$1);
  for(i = 0; i < *$1; i++) {
    PyList_SetItem(outliststrs, i, PyString_FromString($2[i]));
  }
  $result = t_output_helper($result, outliststrs);
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
%typemap(python, in, numinputs=0) (int *length, int *outints, char **outstrs) {
  int i;
  $1 = (int*) malloc(sizeof(int));
  $2 = (int*) malloc(REG_MAX_NUM_STR_CMDS * sizeof(int));
  $3 = (char**) malloc(REG_MAX_NUM_STR_CMDS * sizeof(char*));
  for(i = 0; i < REG_MAX_NUM_STR_CMDS; i++)
    $3[i] = (char*) malloc(REG_MAX_STRING_LENGTH * sizeof(char));
}
%typemap(python, argout) (int *length, int *outints, char **outstrs) {
  int i;
  PyObject* outlistints = PyList_New(*$1);
  PyObject* outliststrs = PyList_New(*$1);
  for(i = 0; i < *$1; i++) {
    PyList_SetItem(outlistints, i, PyInt_FromLong($2[i]));
    PyList_SetItem(outliststrs, i, PyString_FromString($3[i]));
  }
  $result = t_output_helper($result, outlistints);
  $result = t_output_helper($result, outliststrs);
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
%typemap(python, in, numinputs=0) void *outdata {
  /* Just throw outdata away from the inputs! */
}
%typemap(python, check) (int type, int count, void *outdata) {
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
%typemap(python, argout) (int type, int count, void *outdata) {
  int i;

  PyObject* outlist;

  switch($1) {
  case REG_CHAR:
    outlist = PyList_New(1);
    PyList_SetItem(outlist, 0, PyString_FromString((char*) $3));
    break;
  case REG_INT:
    outlist = PyList_New($2);
    for(i = 0; i < $2; i++)
      PyList_SetItem(outlist, i, PyInt_FromLong(((int*) $3)[i]));
    break;
  case REG_FLOAT:
    outlist = PyList_New($2);
    for(i = 0; i < $2; i++)
      PyList_SetItem(outlist, i, PyFloat_FromDouble(((double*) $3)[i]));
    break;
  case REG_DBL:
    outlist = PyList_New($2);
    for(i = 0; i < $2; i++)
      PyList_SetItem(outlist, i, PyFloat_FromDouble(((double*) $3)[i]));
    break;
  }
  $result = t_output_helper($result, outlist);
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
