/*----------------------------------------------------------------------------
    This file contains routines and data structures for file-based
    steering communication and sample transfer.

    (C)Copyright 2003, The University of Manchester, United Kingdom,
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

    Initial version by:  A Porter, 30.1.2004       0.1

---------------------------------------------------------------------------*/
#include "ReG_Steer_types.h"

int Get_data_source_address_file(int dummy,
				 char *hostname,
				 int  *port)
{
  char *pchar;
  int   len;

  /* Return port = 0 on failure */
  *port = 0;

  /* Get hostname and port from environment variables */

  pchar = getenv("REG_CONNECTOR_HOSTNAME");
  if (pchar) {
    len = strlen(pchar);
    if (len < REG_MAX_STRING_LENGTH) {
      sprintf(hostname, pchar);
    }
    else{
      fprintf(stderr, "Get_data_source_address_file: content of "
	      "REG_CONNECTOR_HOSTNAME exceeds max. string length of "
	      "%d chars\n", REG_MAX_STRING_LENGTH);
      return REG_FAILURE;
    }
  }
  else{
    fprintf(stderr, 
	    "Get_data_source_address_file: REG_CONNECTOR_HOSTNAME not set\n");
    return REG_FAILURE;
  }

  pchar = getenv("REG_CONNECTOR_PORT");
  if (pchar) {
    *port = atoi(pchar);
  }
  else{
    fprintf(stderr, 
	    "Get_data_source_address_file: REG_CONNECTOR_PORT not set\n");
    return REG_FAILURE;
  }

  return REG_SUCCESS;
}
