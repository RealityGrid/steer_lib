/*----------------------------------------------------------------------------
    Simply code to report sizeof c types on current architecture.

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

    Initial version by: A Porter
    
---------------------------------------------------------------------------*/
#include <stdlib.h>
#include <stdio.h>

void main()
{
  printf("sizeof(short)  = %d\n", sizeof(short));
  printf("sizeof(int)    = %d\n", sizeof(int));
  printf("sizeof(long)   = %d\n", sizeof(long));
  printf("sizeof(float)  = %d\n", sizeof(float));
  printf("sizeof(double) = %d\n", sizeof(double));
}
