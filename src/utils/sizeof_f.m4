/*----------------------------------------------------------------------------
    Simple c-code designed to provide 'sizeof' functionality to an
    F90 calling routine.

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

    Initial version by: A Porter.
    
---------------------------------------------------------------------------*/
#include <string.h>

void FUNCTION(sizeof_f) ARGS(`STRING_ARG(string), size')
STRING_ARG_DECL(string);
int *size;
{
  char *ptr;

  ptr = STRING_PTR(string);

  if(strcmp(ptr, "short") == 0){

    *size = sizeof(short);
  }
  else if(strcmp(ptr, "int") == 0){

    *size = sizeof(int);
  }
  else if(strcmp(ptr, "long") == 0){
  
    *size = sizeof(long);
  }
  else if(strcmp(ptr, "float") == 0){
  
    *size = sizeof(float);
  }
  else if(strcmp(ptr, "double") == 0){
  
    *size = sizeof(double);
  }
  else{

    printf("sizeof_f: unrecognised c type: %s\n", STRING_PTR(string));
    *size = 0;
  }

  return;
}
